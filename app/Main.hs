{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)

import Wallet (showWallet)

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then putStrLn "Please give subcommand wallet/send/balance."
    else case head args of
      "wallet" -> showWallet
