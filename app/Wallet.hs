module Wallet where

import Crypto.Secp256k1
import qualified Data.ByteString.Char8 as BS
import Data.Function (fix)
import System.Directory (doesFileExist)

import Bitcoin.Wallet (genSecKey, encodeWIF, decodeWIF, encodeBitcoinAddress)

-- | 秘密鍵を保存するファイルパス
secKeyFilePath :: FilePath
secKeyFilePath = "secret-key.der"

-- | ファイルから秘密鍵を取得する
getWalletSecretKey :: IO (Maybe SecKey)
getWalletSecretKey = do
  isExists <- doesFileExist secKeyFilePath
  if isExists
    then decodeWIF <$> BS.readFile secKeyFilePath
    else pure Nothing

-- | ウォレットのBitcoinアドレスを表示する
-- | もし秘密鍵が保存されて無ければ生成する
showWallet :: IO ()
showWallet = do
  msecKey <- getWalletSecretKey
  secKey <- case msecKey of
    Nothing -> do
      sk <- genSecKey
      BS.writeFile secKeyFilePath $ encodeWIF sk
      pure sk
    Just secKey -> pure secKey
  BS.putStrLn . encodeBitcoinAddress $ derivePubKey secKey
