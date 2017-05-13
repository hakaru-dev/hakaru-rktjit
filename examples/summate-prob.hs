{-# LANGUAGE DataKinds, NegativeLiterals #-}
module Main where

import           Prelude hiding (product)
import           Language.Hakaru.Runtime.Prelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad
import           Data.Number.LogFloat hiding (product)

prog = 
  lam $ \ a0 ->
  lam $ \ b1 ->
  summate (nat_ 0) (size a0) (\ i2 -> a0 ! i2 + nat2prob b1)

main :: IO ()
main = do
  g <- MWC.createSystemRandom
  print prog
