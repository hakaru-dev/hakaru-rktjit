{-# LANGUAGE DataKinds, NegativeLiterals #-}
module Main where

import           Data.Number.LogFloat (LogFloat)
import           Prelude hiding (product, exp, log, (**))
import           Language.Hakaru.Runtime.LogFloatPrelude

import           Language.Hakaru.Runtime.CmdLine
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad
import           System.Environment (getArgs)

import qualified Data.Time.Clock as C
import qualified System.Environment as SE
import qualified Data.Number.LogFloat as LF
import qualified Data.Vector.Unboxed as UV
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Text.Read as TR

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
main = do
  twds <- SE.getArgs
  print twds
  let [tps, wps, vf, wf, df, du, ouf] = twds
  v_st <- TIO.readFile vf
  words_st <- TIO.readFile wf
  docs_st <- TIO.readFile df
  let topic_prior = UV.map LF.logFloat $ UV.replicate (read tps) 1.0
  let word_prior = UV.map LF.logFloat  $ UV.replicate (read wps) 1.0
  let v = UV.fromList $ (Prelude.map (read . T.unpack) (T.lines v_st) :: [Int])
  let words = UV.fromList $ ((Prelude.map (read . T.unpack) (T.lines words_st)) :: [Int])
  let docs = UV.fromList $ ((Prelude.map (read . T.unpack) (T.lines docs_st)) :: [Int])
  let docUpdate = (read du :: Int)
  start_time <- C.getCurrentTime
  result <- return $! (prog topic_prior word_prior v words docs docUpdate)
  end_time <- C.getCurrentTime
  print "result:"
  print (UV.map LF.logFromLogFloat result)
  TIO.writeFile ouf $ T.unlines $ Prelude.map T.pack $ Prelude.map show $ UV.toList $ UV.map LF.logFromLogFloat result 
  print "time:"
  print $ C.diffUTCTime end_time start_time
