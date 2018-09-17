{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Protocol.Merkleblock where

import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Data.Binary (Binary(..), encode)
import Data.Binary.Put (putInt32le, putWord32le, putByteString)
import Data.Binary.Get (getInt32le, getWord32le, getByteString)
import Data.Bits (shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import GHC.Generics (Generic)

import Bitcoin.Protocol.VarList (VarList)
import qualified Bitcoin.Protocol.VarList as VarList
import Bitcoin.Hash (hash256)
import Bitcoin.Types (Message(..), Chars, toChars, toByteString, Int32le, Word32le)

-- | マークルブロック
data Merkleblock = Merkleblock
  { version           :: Int32le
  , prevBlock         :: Chars 32
  , merkleRoot        :: Chars 32
  , timestamp         :: Word32le
  , bits              :: Word32le
  , nonce             :: Word32le
  , totalTransactions :: Word32le
  , hashes            :: VarList (Chars 32)
  , flags             :: VarList Word8
  } deriving (Show, Generic)

instance Binary Merkleblock

instance Message Merkleblock where
  commandName = "merkleblock"

-- | ブロックハッシュを計算する
blockHash :: Merkleblock -> Chars 32
blockHash = toChars . hash256 . BS.take 80 . BL.toStrict . encode
