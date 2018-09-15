{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Bitcoin.Protocol.NetAddr where

import Data.Word (Word16)

import Data.Binary (Binary(..))
import GHC.Generics (Generic)

import Bitcoin.Types (Chars, Word64le)

-- | ネットワーク・アドレス
data NetAddr = NetAddr
  { services :: Word64le -- versionのserviceと同様
  , ip       :: Chars 16 -- IPアドレス
  , port     :: Word16   -- ポート番号
  } deriving (Show, Generic)

instance Binary NetAddr
