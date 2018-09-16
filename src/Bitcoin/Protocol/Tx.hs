{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Protocol.Tx where

import Prelude hiding (sequence)
import Data.List (find)
import GHC.Generics (Generic)

import Data.Binary (Binary(..), encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Bitcoin.Hash (hash256)
import Bitcoin.Types (Message(..), Chars, toChars, Word32le, Int32le, Int64le)
import Bitcoin.Protocol.VarStr (VarStr)
import qualified Bitcoin.Protocol.VarStr as VarStr
import Bitcoin.Protocol.VarList (VarList)
import qualified Bitcoin.Protocol.VarList as VarList
-- import Bitcoin.Script (op_dup, op_hash160)

-- | トランザクションID
type TxId = Chars 32

-- | TxOutへの参照
data OutPoint = OutPoint
  { hash :: TxId
  , index :: Word32le
  } deriving (Show, Generic)

instance Binary OutPoint

-- | トランザクションで使用するUTXO
data TxIn = TxIn
  { previousOutput  :: OutPoint
  , signatureScript :: VarStr
  , sequence        :: Word32le
  } deriving (Show, Generic)

instance Binary TxIn

-- | 送金情報
data TxOut = TxOut
  { value    :: Int64le
  , pkScript :: VarStr
  } deriving (Show, Generic)

instance Binary TxOut

-- | トランザクション
data Tx = Tx
  { version  :: Int32le
  , txIn     :: VarList TxIn
  , txOut    :: VarList TxOut
  , lockTime :: Word32le
  } deriving (Show, Generic)

instance Binary Tx

instance Message Tx where
  commandName = "tx"

txId :: Tx -> TxId
txId = toChars . hash256 . BL.toStrict . encode
