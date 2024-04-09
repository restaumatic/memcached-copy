{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import System.Environment (getArgs)
import Network.Socket (getAddrInfo, defaultHints, AddrInfo (..), SocketType (..))
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
import Data.List (intercalate, sortOn)
import Data.Text (Text)
import qualified Network.URI.Encode as URIEncode
import System.Random (randomIO)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Control.Concurrent.Async (forConcurrently_)
import Data.Bits
import Data.Word

pattern FLAG_REPLICATED :: Word32
pattern FLAG_REPLICATED = 1

data CopyMode = Dump | Watch deriving (Show, Eq)

main :: IO ()
main = do
  [hostname, self] <- getArgs
  let hints = defaultHints { addrSocketType = Stream }
  addrs <- sortOn addrAddress <$> getAddrInfo (Just hints) (Just hostname) (Just "11211")

  selfClient <- MC.newClient [MC.def { MC.ssHost = self }] MC.def

  forConcurrently_ addrs \addr -> do
    bracket (Socket.openSocket addr) Socket.close \s1 -> do
      let ip = showIP (addrAddress addr)
      client <- MC.newClient [MC.def { MC.ssHost = ip }] MC.def

      r <- Text.encodeUtf8 . Text.pack . show <$> randomIO @Int
      _ <- MC.set selfClient r "" FLAG_REPLICATED 3600
      result <- MC.get client r

      if isJust result then
        putStrLn $ ip <> " is self, skipping"
      else do
        putStrLn $ ip <> " is not self, copying"
        Socket.connect s1 (addrAddress addr)
        h <- Socket.socketToHandle s1 ReadWriteMode
        hSetBuffering h (BlockBuffering Nothing)

        let copy mode = do
              fix \loop -> do
                line <- Text.strip <$> Text.hGetLine h
                forM_ (parseLine line) \key -> do
                  let key' = Text.encodeUtf8 key
                  m_value <- MC.get client key'
                  forM_ m_value \(value, flags, _) -> do
                    if mode == Watch && flags .&. FLAG_REPLICATED /= 0 then
                      Text.putStrLn $ "got replicated change of " <> key <> ", ignoring"
                    else do
                      Text.putStrLn line
                      when (mode == Watch) do
                        Text.putStrLn $ "replicating change of " <> key <> " from " <> Text.pack ip
                      void $ MC.set selfClient key' value (flags .|. FLAG_REPLICATED) 0

                when (line /= "" && line /= "END") loop

        Text.hPutStrLn h "lru_crawler metadump all"
        hFlush h
        copy Dump

        putStrLn $ "watching " <> ip
        Text.hPutStrLn h "watch mutations"
        hFlush h
        line <- Text.strip <$> Text.hGetLine h
        if line == "OK" then
          copy Watch
        else
          putStrLn $ "'watch mutations' failed: " <> show line
      
parseLine :: Text -> Maybe Text
parseLine t =
  fmap URIEncode.decodeText $
  listToMaybe $
  mapMaybe (Text.stripPrefix "key=") $ Text.split (== ' ') t

showIP :: Socket.SockAddr -> String
showIP = \case
  Socket.SockAddrInet _ addr ->
    let (a,b,c,d) = Socket.hostAddressToTuple addr
    in intercalate "." $ map show [a,b,c,d]
  x -> error $ "Expected SockAddrInet, got " <> show x
