{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Prelude hiding (log)
import System.Environment (getArgs)
import Network.Socket (SockAddr)
import Network.DNS (lookupA, makeResolvSeed, withResolver, defaultResolvConf)
import Data.IP (fromIPv4)
import Data.Either (fromRight)
import qualified Data.ByteString.Char8 as BS
import qualified Network.Socket  as Socket
import Control.Monad (forM_, when, void)
import Control.Exception
import GHC.IO.IOMode (IOMode(ReadWriteMode))
import GHC.IO.Handle (hSetBuffering, BufferMode (..), hFlush)
import Data.Function (fix)
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import qualified Database.Memcache.Client as MC
import Data.List (intercalate)
import Data.Text (Text)
import qualified Network.URI.Encode as URIEncode
import System.Random (randomIO)
import Data.Maybe (isJust, catMaybes)
import Control.Concurrent.Async (forConcurrently, mapConcurrently_)
import Data.Bits
import Data.Word
import Control.Concurrent (threadDelay)
import Text.Read (readMaybe)
import Database.Memcache.Types (Expiration)

pattern FLAG_REPLICATED :: Word32
pattern FLAG_REPLICATED = 1

main :: IO ()
main = do
  [hostname, self] <- getArgs
  selfClient <- MC.newClient [MC.def { MC.ssHost = self }] MC.def

  let getServers = do
        rs <- makeResolvSeed defaultResolvConf
        ips <- withResolver rs $ \resolver -> lookupA resolver (BS.pack hostname)
        case ips of
          Left err -> throwIO $ userError $ "DNS lookup failed: " <> show err
          Right ipList -> do
            let addrs = map (Socket.SockAddrInet 11211 . Socket.tupleToHostAddress . toTuple) ipList
            toTuple ip = case fromIPv4 ip of
              [a, b, c, d] -> (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d)
              _ -> error "Invalid IPv4 address format"
        catMaybes <$> forConcurrently addrs \addr -> do
          isSelf <- bracket (MC.newClient [toServerSpec addr] MC.def) MC.quit (isSameAs selfClient)
          if isSelf then do
            pure Nothing
          else
            pure $ Just addr

      toServerSpec :: SockAddr -> MC.ServerSpec
      toServerSpec addr = MC.def { MC.ssHost = showIP addr }

  let copyFromServer addr = do
        let ip = showIP addr

        squashException ("copying data from " <> ip) do
          log $ "copying data from " <> ip

          client <- MC.newClient [toServerSpec addr] MC.def

          bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close \s1 -> do
            Socket.connect s1 addr
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

  copyFromOthers

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
log s = putStrLn $ "memcached-copy: " <> s
