{-# LANGUAGE DataKinds, NegativeLiterals #-}
module Main where

import           Prelude hiding (product)
import           Language.Hakaru.Runtime.Prelude

import           Language.Hakaru.Runtime.CmdLine
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad
import           System.Environment (getArgs)

prog :: ((Double, Double) -> (Measure (Prob, Prob)))
prog =
  lam $ \ x90 ->
  case_ x90
        [branch (ppair PVar PVar)
                (\ r31 r12 ->
                 (pose (recip pi * prob_ (1/2)) $
                       (dirac 5 >>= \ noiseT_d3 ->
                        uniform (real_ 1) (real_ 4) >>= \ noiseE_b4 ->
                        (pose (exp (recip (noiseE_b4 ^ nat_ 4 +
                                           noiseE_b4 ^ nat_ 2 * noiseT_d3 ^ nat_ 2 * real_ 3 +
                                           noiseT_d3 ^ nat_ 4) *
                                    r12 ^ nat_ 2 *
                                    noiseE_b4 ^ nat_ 2 *
                                    real_ (-1/2)) *
                               exp (recip (noiseE_b4 ^ nat_ 4 +
                                           noiseE_b4 ^ nat_ 2 * noiseT_d3 ^ nat_ 2 * real_ 3 +
                                           noiseT_d3 ^ nat_ 4) *
                                    r12 ^ nat_ 2 *
                                    noiseT_d3 ^ nat_ 2 *
                                    real_ (-1/2)) *
                               exp (recip (noiseE_b4 ^ nat_ 4 +
                                           noiseE_b4 ^ nat_ 2 * noiseT_d3 ^ nat_ 2 * real_ 3 +
                                           noiseT_d3 ^ nat_ 4) *
                                    r12 *
                                    r31 * recip 0 *
                                    noiseT_d3 ^ nat_ 2) *
                               exp (recip (noiseE_b4 ^ nat_ 4 +
                                           noiseE_b4 ^ nat_ 2 * noiseT_d3 ^ nat_ 2 * real_ 3 +
                                           noiseT_d3 ^ nat_ 4) *
                                    r31 ^ nat_ 2 *
                                    noiseE_b4 ^ nat_ 2 *
                                    real_ (-1/2)) *
                               recip (exp (recip (noiseE_b4 ^ nat_ 4 +
                                                  noiseE_b4 ^ nat_ 2 *
                                                  noiseT_d3 ^ nat_ 2 *
                                                  real_ 3 +
                                                  noiseT_d3 ^ nat_ 4) *
                                           r31 ^ nat_ 2 *
                                           noiseT_d3 ^ nat_ 2)) *
                               recip (nat_ 2
                                      `thRootOf` (unsafeProb (noiseE_b4 ^ nat_ 4 +
                                                              noiseE_b4 ^ nat_ 2 *
                                                              noiseT_d3 ^ nat_ 2 *
                                                              real_ 3 +
                                                              noiseT_d3 ^ nat_ 4)))) $
                              (dirac (ann_ (SData (STyApp (STyApp (STyCon (SingSymbol :: Sing "Pair")) SProb) SProb) (SPlus (SEt (SKonst SProb) (SEt (SKonst SProb) SDone)) SVoid))
                                           ((pair (unsafeProb noiseT_d3) 
                                                  (unsafeProb noiseE_b4)))))))))]

main :: IO ()
main = makeMain (prog (-12.165,2.5)) =<< getArgs
