{-# LANGUAGE DataKinds, NegativeLiterals #-}
module Main where

import           Data.Number.LogFloat hiding (product)
import           Prelude              hiding (product, exp, log, (**))

import           Language.Hakaru.Runtime.LogFloatPrelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad
import qualified Data.Time.Clock as C
import qualified System.Environment as SE
import qualified Data.Number.LogFloat as LF
import qualified Data.Vector.Unboxed as UV
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           System.Environment (getArgs)


prog = 
  lam $ \ topic_prior0 ->
  lam $ \ word_prior1 ->
  lam $ \ z2 ->
  lam $ \ w3 ->
  lam $ \ doc4 ->
  lam $ \ docUpdate5 ->
  case_ (docUpdate5 < size z2)
        [branch ptrue
                ((array (size topic_prior0) $
                        \ zNew丏6 ->
                        product (nat_ 0)
                                (size topic_prior0)
                                (\ i7 ->
                                 product (nat_ 0)
                                         (size word_prior1)
                                         (\ i丣8 ->
                                          product (nat_ 0)
                                                  (let_ (bucket (nat_ 0)
                                                                (size w3)
                                                                ((r_fanout (r_index (\ () ->
                                                                                     size z2)
                                                                                    (\ (i丙11,()) ->
                                                                                     doc4
                                                                                     ! i丙11)
                                                                                    (r_index (\ (docUpdate12,()) ->
                                                                                              size word_prior1)
                                                                                             (\ (i丙11,(docUpdate12,())) ->
                                                                                              w3
                                                                                              ! i丙11)
                                                                                             (r_add (\ (i丙11,(i丣13,(docUpdate12,()))) ->
                                                                                                     nat_ 1))))
                                                                           r_nop))) $ \ summary10 ->
                                                   case_ (i7 == zNew丏6)
                                                         [branch ptrue
                                                                 (case_ summary10
                                                                        [branch (ppair PVar PVar)
                                                                                (\ y14 z15 -> y14)]
                                                                  ! docUpdate5
                                                                  ! i丣8),
                                                          branch pfalse (nat_ 0)])
                                                  (\ j9 ->
                                                   nat2prob (let_ (bucket (nat_ 0)
                                                                          (size w3)
                                                                          ((r_split (\ (i丙17,()) ->
                                                                                     doc4 ! i丙17
                                                                                     == docUpdate5)
                                                                                    r_nop
                                                                                    (r_index (\ () ->
                                                                                              size word_prior1)
                                                                                             (\ (i丙17,()) ->
                                                                                              w3
                                                                                              ! i丙17)
                                                                                             (r_index (\ (i丣18,()) ->
                                                                                                       size topic_prior0)
                                                                                                      (\ (i丙17,(i丣18,())) ->
                                                                                                       z2
                                                                                                       ! (doc4
                                                                                                          ! i丙17))
                                                                                                      (r_add (\ (i丙17,(i19,(i丣18,()))) ->
                                                                                                              nat_ 1))))))) $ \ summary16 ->
                                                             case_ summary16
                                                                   [branch (ppair PVar PVar)
                                                                           (\ y20 z21 -> z21)]
                                                             ! i丣8
                                                             ! i7) +
                                                   nat2prob j9 +
                                                   word_prior1 ! i丣8))) *
                        ((let_ (bucket (nat_ 0)
                                       (size z2)
                                       ((r_split (\ (i丙23,()) -> i丙23 == docUpdate5)
                                                 r_nop
                                                 (r_index (\ () -> size topic_prior0)
                                                          (\ (i丙23,()) -> z2 ! i丙23)
                                                          (r_add (\ (i丙23,(zNew丏24,())) ->
                                                                  nat_ 1)))))) $ \ summary22 ->
                          nat2prob (case_ summary22
                                          [branch (ppair PVar PVar) (\ y25 z26 -> z26)]
                                    ! zNew丏6)) +
                         topic_prior0 ! zNew丏6) *
                        recip (nat2prob (summate (nat_ 0)
                                                 (size z2)
                                                 (\ i丙27 ->
                                                  case_ (i丙27 == docUpdate5)
                                                        [branch ptrue (nat_ 0),
                                                         branch pfalse
                                                                (case_ (z2 ! i丙27 < nat_ 0)
                                                                       [branch ptrue (nat_ 0),
                                                                        branch pfalse
                                                                               (nat_ 1)])])) +
                               summate (nat_ 0)
                                       (size topic_prior0)
                                       (\ i丙28 -> topic_prior0 ! i丙28)) *
                        recip (product (nat_ 0)
                                       (size topic_prior0)
                                       (\ i29 ->
                                        product (nat_ 0)
                                                (let_ (bucket (nat_ 0)
                                                              (size w3)
                                                              ((r_fanout (r_split (\ (i丙32,()) ->
                                                                                   w3 ! i丙32
                                                                                   < nat_ 0)
                                                                                  r_nop
                                                                                  (r_index (\ () ->
                                                                                            size z2)
                                                                                           (\ (i丙32,()) ->
                                                                                            doc4
                                                                                            ! i丙32)
                                                                                           (r_add (\ (i丙32,(docUpdate33,())) ->
                                                                                                   nat_ 1))))
                                                                         r_nop))) $ \ summary31 ->
                                                 case_ (i29 == zNew丏6)
                                                       [branch ptrue
                                                               (case_ (case_ summary31
                                                                             [branch (ppair PVar
                                                                                            PVar)
                                                                                     (\ y34 z35 ->
                                                                                      y34)])
                                                                      [branch (ppair PVar PVar)
                                                                              (\ y36 z37 -> z37)]
                                                                ! docUpdate5),
                                                        branch pfalse (nat_ 0)])
                                                (\ i丣30 ->
                                                 nat2prob (let_ (bucket (nat_ 0)
                                                                        (size w3)
                                                                        ((r_split (\ (i丙39,()) ->
                                                                                   w3 ! i丙39
                                                                                   < nat_ 0)
                                                                                  r_nop
                                                                                  (r_split (\ (i丙39,()) ->
                                                                                            doc4
                                                                                            ! i丙39
                                                                                            == docUpdate5)
                                                                                           r_nop
                                                                                           (r_index (\ () ->
                                                                                                     size topic_prior0)
                                                                                                    (\ (i丙39,()) ->
                                                                                                     z2
                                                                                                     ! (doc4
                                                                                                        ! i丙39))
                                                                                                    (r_add (\ (i丙39,(i40,())) ->
                                                                                                            nat_ 1))))))) $ \ summary38 ->
                                                           case_ (case_ summary38
                                                                        [branch (ppair PVar PVar)
                                                                                (\ y41 z42 -> z42)])
                                                                 [branch (ppair PVar PVar)
                                                                         (\ y43 z44 -> z44)]
                                                           ! i29) +
                                                 nat2prob i丣30 +
                                                 summate (nat_ 0)
                                                         (size word_prior1)
                                                         (\ i丙45 -> word_prior1 ! i丙45)))))),
         branch pfalse ((arrayLit []))]

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
