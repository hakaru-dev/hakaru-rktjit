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

prog :: ((Double, Double) -> (Measure (Prob, Prob)))
prog =
  lam $ \ x90 ->
  case_ x90
        [branch (ppair PVar PVar)
                (\ r31 r12 ->
                 (pose (recip pi * prob_ (1/2)) $
                       (uniform (real_ 3) (real_ 8) >>= \ noiseT_d3 ->
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
                                    r31 *
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
