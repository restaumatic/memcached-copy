{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

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

  addrs <- getServers

  serversCache <- newIORef mempty
  serversMVar <- newMVar mempty

  let updateServers = do
        oldServers <- readIORef serversCache
        addrs' <- getServers
        servers <- forM addrs' \addr -> do
          let key = showIP (addrAddress addr)
          server <- case Map.lookup key oldServers of
            Just s -> pure s
            Nothing -> do
              putStrLn $ "Discovered new server " <> key
              MC.newServerDefault $ toServerSpec addr
          pure (key, server)
        writeIORef serversCache (Map.fromList servers)
        void $ swapMVar serversMVar (V.fromList $ fmap snd servers)

  updateServers
  void $ forkIO $ forever do
    threadDelay 5000000
    updateServers

  allClient <- flip MC.setServers (Left serversMVar) <$> MC.newClient [] MC.def

  let watchSelf = do
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
                if flags .&. FLAG_REPLICATED /= 0 then
                  Text.putStrLn $ "got replicated change of " <> key <> ", ignoring"
                else do
                  Text.putStrLn $ "replicating change of " <> key
                  handle (\(e :: SomeException) -> putStrLn (show e)) $
                    void $ MC.allOp allClient $ MC.emptyReq { MC.reqOp = MC.ReqSet MC.Loud key' value (MC.SESet (flags .|. FLAG_REPLICATED) ttl) }

            when (line /= "" && line /= "END") loop

      copyFromServer addr = do
        let ip = showIP (addrAddress addr)
        client <- MC.newClient [toServerSpec addr] MC.def

        putStrLn $ "copying from " <> ip

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

  concurrently_ watchSelf (mapConcurrently_ copyFromServer addrs)
      
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
