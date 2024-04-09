{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import System.Environment (getArgs)
import Network.Socket (getAddrInfo, defaultHints, AddrInfo (..), SocketType (..))
import qualified Network.Socket  as Socket
import Control.Monad (forM_, when)
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
import Data.Maybe (isJust)

main :: IO ()
main = do
  [hostname, self] <- getArgs
  let hints = defaultHints { addrSocketType = Stream }
  addrs <- getAddrInfo (Just hints) (Just hostname) (Just "11211")

  selfClient <- MC.newClient [MC.def { MC.ssHost = self }] MC.def

  forM_ addrs \addr -> do
    bracket (Socket.openSocket addr) Socket.close \s1 -> do
      let ip = showIP (addrAddress addr)
      client <- MC.newClient [MC.def { MC.ssHost = ip }] MC.def

      r <- Text.encodeUtf8 . Text.pack . show <$> randomIO @Int
      _ <- MC.set selfClient r "" 0 0
      result <- MC.get client r

      if isJust result then
        putStrLn $ ip <> " is self, skipping"
      else do
        putStrLn $ ip <> " is not self, copying"
        Socket.connect s1 (addrAddress addr)
        h <- Socket.socketToHandle s1 ReadWriteMode
        hSetBuffering h (BlockBuffering Nothing)
        Text.hPutStrLn h "lru_crawler metadump all\n"
        hFlush h

        fix \loop -> do
          line <- Text.strip <$> Text.hGetLine h
          forM_ (parseMetadumpLine line) \key -> do
            let key' = Text.encodeUtf8 key
            m_value <- MC.get client key'
            forM_ m_value \(value, flags, _) ->
              MC.set selfClient key' value flags 0

          when (line /= "" && line /= "END") loop
      
parseMetadumpLine :: Text -> Maybe Text
parseMetadumpLine t =
  fmap URIEncode.decodeText $
  Text.stripPrefix "key=" $ fst $ Text.break (== ' ') t

showIP :: Socket.SockAddr -> String
showIP = \case
  Socket.SockAddrInet _ addr ->
    let (a,b,c,d) = Socket.hostAddressToTuple addr
    in intercalate "." $ map show [a,b,c,d]
  x -> error $ "Expected SockAddrInet, got " <> show x
