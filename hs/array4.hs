{-# LANGUAGE DataKinds, NegativeLiterals #-}
module Main where

import           Data.Number.LogFloat hiding (product)
import           Prelude              hiding (product, exp, log, (**))

import           Language.Hakaru.Runtime.LogFloatPrelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad
import           Data.Number.LogFloat hiding (product)

import qualified Data.Time.Clock as C
import qualified System.Environment as SE
import qualified Data.Number.LogFloat as LF
import qualified Data.Vector.Unboxed as UV
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Text.Read as TR
prog = 
  lam $ \ topic_prior0 ->
  lam $ \ word_prior1 ->
  lam $ \ z2 ->
  lam $ \ w3 ->
  lam $ \ doc4 ->
  lam $ \ docUpdate5 ->
  (array (size topic_prior0) $
         \ zNew6 ->
         product (nat_ 0)
                 (size topic_prior0)
                 (\ i7 ->
                  product (nat_ 0)
                          (summate (nat_ 0)
                                   (size w3)
                                   (\ i_b9 ->
                                    case_ (docUpdate5 == doc4 ! i_b9)
                                          [branch ptrue
                                                  (case_ (not (w3 ! i_b9 < nat_ 0) && i7 == zNew6)
                                                         [branch ptrue (nat_ 1),
                                                          branch pfalse (nat_ 0)]),
                                           branch pfalse (nat_ 0)]))
                          (\ i_a8 ->
                           nat2prob (summate (nat_ 0)
                                             (size w3)
                                             (\ i_b10 ->
                                              case_ (doc4 ! i_b10 == docUpdate5)
                                                    [branch ptrue (nat_ 0),
                                                     branch pfalse
                                                            (case_ (not (w3 ! i_b10 < nat_ 0) &&
                                                                    i7 == z2 ! (doc4 ! i_b10))
                                                                   [branch ptrue (nat_ 1),
                                                                    branch pfalse (nat_ 0)])])) +
                           nat2prob i_a8 +
                           summate (nat_ 0)
                                   (size word_prior1)
                                   (\ i_b11 -> word_prior1 ! i_b11))))
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
