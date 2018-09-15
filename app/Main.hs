{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import Bitcoin.Protocol (withBitcoinConnection, recvMessageHeader, recvMessage)
import Control.Monad (forever)

main :: IO ()
main = withBitcoinConnection $ \(sock, _) -> do
  forever $ dispatch sock =<< recvMessageHeader sock
    where
      dispatch sock (name, size) =
        if size > 0
          then recvMessage sock size :: IO ByteString
          else pure ""
