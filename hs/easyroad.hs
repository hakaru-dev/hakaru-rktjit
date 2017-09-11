{-# LANGUAGE DataKinds, NegativeLiterals #-}
module Main where

import           Prelude hiding (product)
import           Language.Hakaru.Runtime.Prelude

import           Language.Hakaru.Runtime.CmdLine
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad
import           System.Environment (getArgs)

prog :: (Measure ((Double, Double), (Prob, Prob)))
prog =
  let_ (uniform (nat2real (nat_ 3))
                (nat2real (nat_ 8)) >>= \ noiseT_1 ->
        uniform (nat2real (nat_ 1)) (nat2real (nat_ 4)) >>= \ noiseE_2 ->
        let_ (unsafeProb noiseT_1) $ \ noiseT3 ->
        let_ (unsafeProb noiseE_2) $ \ noiseE4 ->
        normal (nat2real (nat_ 0)) noiseT3 >>= \ x15 ->
        normal x15 noiseE4 >>= \ m16 ->
        normal x15 noiseT3 >>= \ x27 ->
        normal x27 noiseE4 >>= \ m28 ->
        dirac (ann_ (SData (STyApp (STyApp (STyCon (SingSymbol :: Sing "Pair")) (SData (STyApp (STyApp (STyCon (SingSymbol :: Sing "Pair")) SReal) SReal) (SPlus (SEt (SKonst SReal) (SEt (SKonst SReal) SDone)) SVoid))) (SData (STyApp (STyApp (STyCon (SingSymbol :: Sing "Pair")) SProb) SProb) (SPlus (SEt (SKonst SProb) (SEt (SKonst SProb) SDone)) SVoid))) (SPlus (SEt (SKonst (SData (STyApp (STyApp (STyCon (SingSymbol :: Sing "Pair")) SReal) SReal) (SPlus (SEt (SKonst SReal) (SEt (SKonst SReal) SDone)) SVoid))) (SEt (SKonst (SData (STyApp (STyApp (STyCon (SingSymbol :: Sing "Pair")) SProb) SProb) (SPlus (SEt (SKonst SProb) (SEt (SKonst SProb) SDone)) SVoid))) SDone)) SVoid))
                    ((pair (ann_ (SData (STyApp (STyApp (STyCon (SingSymbol :: Sing "Pair")) SReal) SReal) (SPlus (SEt (SKonst SReal) (SEt (SKonst SReal) SDone)) SVoid))
                                 ((pair m16 m28)))
                           (ann_ (SData (STyApp (STyApp (STyCon (SingSymbol :: Sing "Pair")) SProb) SProb) (SPlus (SEt (SKonst SProb) (SEt (SKonst SProb) SDone)) SVoid))
                                 ((pair noiseT3 noiseE4))))))) $ \ easyroad0 ->
  easyroad0

main :: IO ()
main = makeMain prog =<< getArgs
