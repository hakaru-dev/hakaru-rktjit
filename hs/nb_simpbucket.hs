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

prog ::
  ((MayBoxVec Prob Prob) ->
   ((MayBoxVec Prob Prob) ->
    ((MayBoxVec Int Int) ->
     ((MayBoxVec Int Int) ->
      ((MayBoxVec Int Int) -> (Int -> (MayBoxVec Prob Prob)))))))
prog =
  lam $ \ topic_prior21 ->
  lam $ \ word_prior22 ->
  lam $ \ z23 ->
  lam $ \ w24 ->
  lam $ \ doc25 ->
  lam $ \ docUpdate26 ->
  case_ (docUpdate26 < size z23)
        [branch ptrue
                ((array (size topic_prior21) $
                        \ zNew27 ->
                        product (nat_ 0)
                                (size topic_prior21)
                                (\ i28 ->
                                 product (nat_ 0)
                                         (size word_prior22)
                                         (\ i_a29 ->
                                          product (nat_ 0)
                                                  (let_ (bucket (nat_ 0)
                                                                (size w24)
                                                                ((r_fanout (r_index (\ () ->
                                                                                     size z23)
                                                                                    (\ (i_b32,()) ->
                                                                                     doc25
                                                                                     ! i_b32)
                                                                                    (r_index (\ (docUpdate33,()) ->
                                                                                              size word_prior22)
                                                                                             (\ (i_b32,(docUpdate33,())) ->
                                                                                              w24
                                                                                              ! i_b32)
                                                                                             (r_add (\ (i_b32,(i_a34,(docUpdate33,()))) ->
                                                                                                     nat_ 1))))
                                                                           r_nop))) $ \ summary31 ->
                                                   case_ (i28 == zNew27)
                                                         [branch ptrue
                                                                 (case_ summary31
                                                                        [branch (ppair PVar PVar)
                                                                                (\ y35 z36 -> y35)]
                                                                  ! docUpdate26
                                                                  ! i_a29),
                                                          branch pfalse (nat_ 0)])
                                                  (\ j30 ->
                                                   nat2prob (let_ (bucket (nat_ 0)
                                                                          (size w24)
                                                                          ((r_split (\ (i_b38,()) ->
                                                                                     doc25 ! i_b38
                                                                                     == docUpdate26)
                                                                                    r_nop
                                                                                    (r_index (\ () ->
                                                                                              size word_prior22)
                                                                                             (\ (i_b38,()) ->
                                                                                              w24
                                                                                              ! i_b38)
                                                                                             (r_index (\ (i_a39,()) ->
                                                                                                       size topic_prior21)
                                                                                                      (\ (i_b38,(i_a39,())) ->
                                                                                                       z23
                                                                                                       ! (doc25
                                                                                                          ! i_b38))
                                                                                                      (r_add (\ (i_b38,(i40,(i_a39,()))) ->
                                                                                                              nat_ 1))))))) $ \ summary37 ->
                                                             case_ summary37
                                                                   [branch (ppair PVar PVar)
                                                                           (\ y41 z42 -> z42)]
                                                             ! i_a29
                                                             ! i28) +
                                                   nat2prob j30 +
                                                   word_prior22 ! i_a29))) *
                        ((let_ (bucket (nat_ 0)
                                       (size z23)
                                       ((r_split (\ (i_b44,()) -> i_b44 == docUpdate26)
                                                 r_nop
                                                 (r_index (\ () -> size topic_prior21)
                                                          (\ (i_b44,()) -> z23 ! i_b44)
                                                          (r_add (\ (i_b44,(zNew45,())) ->
                                                                  nat_ 1)))))) $ \ summary43 ->
                          nat2prob (case_ summary43
                                          [branch (ppair PVar PVar) (\ y46 z47 -> z47)]
                                    ! zNew27)) +
                         topic_prior21 ! zNew27) *
                        recip (nat2prob (summate (nat_ 0)
                                                 (size z23)
                                                 (\ i_b48 ->
                                                  case_ (i_b48 == docUpdate26)
                                                        [branch ptrue (nat_ 0),
                                                         branch pfalse
                                                                (case_ (z23 ! i_b48 < nat_ 0)
                                                                       [branch ptrue (nat_ 0),
                                                                        branch pfalse
                                                                               (nat_ 1)])])) +
                               summate (nat_ 0)
                                       (size topic_prior21)
                                       (\ i_b49 -> topic_prior21 ! i_b49)) *
                        recip (product (nat_ 0)
                                       (size topic_prior21)
                                       (\ i50 ->
                                        product (nat_ 0)
                                                (let_ (bucket (nat_ 0)
                                                              (size w24)
                                                              ((r_fanout (r_split (\ (i_b53,()) ->
                                                                                   w24 ! i_b53
                                                                                   < nat_ 0)
                                                                                  r_nop
                                                                                  (r_index (\ () ->
                                                                                            size z23)
                                                                                           (\ (i_b53,()) ->
                                                                                            doc25
                                                                                            ! i_b53)
                                                                                           (r_add (\ (i_b53,(docUpdate54,())) ->
                                                                                                   nat_ 1))))
                                                                         r_nop))) $ \ summary52 ->
                                                 case_ (i50 == zNew27)
                                                       [branch ptrue
                                                               (case_ (case_ summary52
                                                                             [branch (ppair PVar
                                                                                            PVar)
                                                                                     (\ y55 z56 ->
                                                                                      y55)])
                                                                      [branch (ppair PVar PVar)
                                                                              (\ y57 z58 -> z58)]
                                                                ! docUpdate26),
                                                        branch pfalse (nat_ 0)])
                                                (\ i_a51 ->
                                                 nat2prob (let_ (bucket (nat_ 0)
                                                                        (size w24)
                                                                        ((r_split (\ (i_b60,()) ->
                                                                                   w24 ! i_b60
                                                                                   < nat_ 0)
                                                                                  r_nop
                                                                                  (r_split (\ (i_b60,()) ->
                                                                                            doc25
                                                                                            ! i_b60
                                                                                            == docUpdate26)
                                                                                           r_nop
                                                                                           (r_index (\ () ->
                                                                                                     size topic_prior21)
                                                                                                    (\ (i_b60,()) ->
                                                                                                     z23
                                                                                                     ! (doc25
                                                                                                        ! i_b60))
                                                                                                    (r_add (\ (i_b60,(i61,())) ->
                                                                                                            nat_ 1))))))) $ \ summary59 ->
                                                           case_ (case_ summary59
                                                                        [branch (ppair PVar PVar)
                                                                                (\ y62 z63 -> z63)])
                                                                 [branch (ppair PVar PVar)
                                                                         (\ y64 z65 -> z65)]
                                                           ! i50) +
                                                 nat2prob i_a51 +
                                                 summate (nat_ 0)
                                                         (size word_prior22)
                                                         (\ i_b66 -> word_prior22 ! i_b66)))))),
         branch pfalse (UV.empty)]
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
