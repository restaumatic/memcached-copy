{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Prelude hiding (log)
import System.Environment (getArgs)
import Network.Socket (getAddrInfo, defaultHints, AddrInfo (..), SocketType (..))
import qualified Network.Socket  as Socket
import Control.Monad (forM_, when, void, forever, forM)
import Control.Exception
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import GHC.IO.Handle (hSetBuffering, BufferMode (..), hFlush)
import Data.Function (fix)
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import qualified Database.Memcache.Client as MC
import Data.List (intercalate, sortOn)
import Data.Text (Text)
import qualified Network.URI.Encode as URIEncode
import System.Random (randomIO)
import Data.Maybe (isJust, catMaybes)
import Control.Concurrent.Async (forConcurrently, concurrently_, mapConcurrently_)
import Data.Bits
import Data.Word
import qualified Database.Memcache.Cluster as MC
import Control.Concurrent (newMVar, threadDelay, forkIO)
import Control.Concurrent.MVar (swapMVar)
import qualified Data.Vector as V
import qualified Database.Memcache.Server as MC
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Text.Read (readMaybe)
import Database.Memcache.Types (Expiration)
import qualified Database.Memcache.Types as MC

pattern FLAG_REPLICATED :: Word32
pattern FLAG_REPLICATED = 1

data CopyMode = Dump | Watch deriving (Show, Eq)

main :: IO ()
main = do
  [hostname, self] <- getArgs
  let hints = defaultHints { addrSocketType = Stream }

  selfClient <- MC.newClient [MC.def { MC.ssHost = self }] MC.def

  let getServers = do
        addrs <- sortOn addrAddress <$> getAddrInfo (Just hints) (Just hostname) (Just "11211")
        catMaybes <$> forConcurrently addrs \addr -> do
          isSelf <- bracket (MC.newClient [toServerSpec addr] MC.def) MC.quit (isSameAs selfClient)
          if isSelf then do
            pure Nothing
          else
            pure $ Just addr

      toServerSpec addr = MC.def { MC.ssHost = showIP (addrAddress addr) }

  serversCache <- newIORef mempty
  serversMVar <- newMVar mempty

  let updateServers = squashException "updateServers" do
        oldServers <- readIORef serversCache
        addrs' <- getServers
        servers <- forM addrs' \addr -> do
          let key = showIP (addrAddress addr)
          server <- case Map.lookup key oldServers of
            Just s -> pure s
            Nothing -> do
              log $ "Discovered new server " <> key
              MC.newServerDefault $ toServerSpec addr
          pure (key, server)
        writeIORef serversCache (Map.fromList servers)
        void $ swapMVar serversMVar (V.fromList $ fmap snd servers)

  updateServers
  void $ forkIO $ forever do
    threadDelay 5000000
    squashException "updateServers" updateServers

  allClient <- flip MC.setServers (Left serversMVar) <$> MC.newClient [] MC.def

  let watchSelf = squashException "watching changes from self" do
        [addr] <- getAddrInfo (Just hints) (Just self) (Just "11211")
        bracket (Socket.openSocket addr) Socket.close \s1 -> do
          Socket.connect s1 (addrAddress addr)
          h <- Socket.socketToHandle s1 ReadWriteMode
          hSetBuffering h (BlockBuffering Nothing)

          Text.hPutStrLn h "watch mutations"
          hFlush h
          do
            line <- Text.strip <$> Text.hGetLine h
            when (line /= "OK") do
              putStrLn $ "'watch mutations' failed: " <> show line

          fix \loop -> do
            line <- Text.strip <$> Text.hGetLine h
            forM_ (parseWatchMutationsLine line) \(key, ttl) -> do
              let key' = Text.encodeUtf8 key
              m_value <- MC.get selfClient key'
              forM_ m_value \(value, flags, _) -> do
                when (flags .&. FLAG_REPLICATED == 0) $
                  squashException "replicating key" $
                    void $ MC.allOp allClient $ MC.emptyReq { MC.reqOp = MC.ReqSet MC.Loud key' value (MC.SESet (flags .|. FLAG_REPLICATED) ttl) }

            when (line /= "" && line /= "END") loop

      watchSelfForever = forever do
        watchSelf
        threadDelay 1000000

      copyFromServer addr = do
        let ip = showIP (addrAddress addr)
        client <- MC.newClient [toServerSpec addr] MC.def

        squashException ("copying data from " <> ip) do
          log $ "copying data from " <> ip

          bracket (Socket.openSocket addr) Socket.close \s1 -> do
            Socket.connect s1 (addrAddress addr)
            h <- Socket.socketToHandle s1 ReadWriteMode
            hSetBuffering h (BlockBuffering Nothing)

            Text.hPutStrLn h "lru_crawler metadump all"
            hFlush h

            fix \loop -> do
              line <- Text.strip <$> Text.hGetLine h
              forM_ (parseMetadumpLine line) \(key, expirationTime) -> do
                let key' = Text.encodeUtf8 key
                m_value <- MC.get client key'
                forM_ m_value \(value, flags, _) -> do
                  void $ MC.set selfClient key' value (flags .|. FLAG_REPLICATED) expirationTime

              when (line /= "" && line /= "END") loop

      copyFromOthers = do
        addrs <- retryForever "getServers" getServers
        mapConcurrently_ copyFromServer addrs

  concurrently_ watchSelfForever copyFromOthers
      
isSameAs :: MC.Client -> MC.Client -> IO Bool
isSameAs selfClient client = do
  randomKey <- Text.encodeUtf8 . Text.pack . show <$> randomIO @Int
  _ <- MC.set selfClient randomKey "" FLAG_REPLICATED 3600
  result <- MC.get client randomKey
  _ <- MC.delete selfClient randomKey 0
  pure $ isJust result

parseMetadumpLine :: Text -> Maybe (Text, Expiration)
parseMetadumpLine t = do
  a:b:_ <- pure $ Text.split (== ' ') t
  key <- Text.stripPrefix "key=" a
  expirationTime <- Text.stripPrefix "exp=" b >>= readMaybe . Text.unpack
  pure (URIEncode.decodeText key, expirationTime)

parseWatchMutationsLine :: Text -> Maybe (Text, Expiration)
parseWatchMutationsLine t = do
  _ts:_gid:_type:a:_status:_cmd:b:_ <- pure $ Text.split (== ' ') t
  key <- Text.stripPrefix "key=" a
  expirationTime <- Text.stripPrefix "ttl=" b >>= readMaybe . Text.unpack
  pure (URIEncode.decodeText key, expirationTime)

showIP :: Socket.SockAddr -> String
showIP = \case
  Socket.SockAddrInet _ addr ->
    let (a,b,c,d) = Socket.hostAddressToTuple addr
    in intercalate "." $ map show [a,b,c,d]
  x -> error $ "Expected SockAddrInet, got " <> show x

squashException :: String -> IO () -> IO ()
squashException operationName =
  handle \(e :: SomeException) -> do
    rethrowIfAsync e
    log $ operationName <> " failed with exception: " <> show e

retryForever :: String -> IO a -> IO a
retryForever operationName block = fix \loop -> do
  r <- try block
  case r of
    Left (e :: SomeException) -> do
      rethrowIfAsync e
      log $ operationName <> " failed with exception: " <> show e <> ", will retry"
      threadDelay 1000000
      loop
    Right x ->
      pure x

rethrowIfAsync :: SomeException -> IO ()
rethrowIfAsync e =
  forM_ (fromException e) \(ae :: SomeAsyncException) ->
    throwIO ae

log :: String -> IO ()
log s = putStrLn $ "memcached-replicator: " <> s
