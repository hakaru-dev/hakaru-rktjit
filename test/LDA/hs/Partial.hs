{-# LANGUAGE DataKinds, NegativeLiterals #-}
module Main where

import           Data.Number.LogFloat (LogFloat)
import           Prelude hiding (product, exp, log, (**), pi)
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
    (Int ->
     ((MayBoxVec Int Int) ->
      ((MayBoxVec Int Int) ->
       ((MayBoxVec Int Int) -> (Int -> (MayBoxVec Prob Prob))))))))
prog =
  lam $ \ topic_prior59 ->
  lam $ \ word_prior60 ->
  lam $ \ numDocs61 ->
  lam $ \ w62 ->
  lam $ \ doc63 ->
  lam $ \ z64 ->
  lam $ \ wordUpdate65 ->
                 (array (size topic_prior59) $
                                           \ zNewh94 ->
                                           unsafeProb ((fromInt (let_ (bucket (nat_ 0)
                                                                              (size w62)
                                                                              ((r_index (\ () ->
                                                                                         size topic_prior59)
                                                                                        (\ (iB96,()) ->
                                                                                         z64
                                                                                         ! iB96)
                                                                                        (r_split (\ (iB96,(zNewh97,())) ->
                                                                                                  doc63
                                                                                                  ! wordUpdate65
                                                                                                  == doc63
                                                                                                     ! iB96)
                                                                                                 (r_add (\ (iB96,(zNewh97,())) ->
                                                                                                         nat_ 1))
                                                                                                 r_nop)))) $ \ summary95 ->
                                                                 nat2int (case_ (not (nat2int (size topic_prior59) +
                                                                                      int_ -1
                                                                                      < nat2int (z64
                                                                                                 ! wordUpdate65)) &&
                                                                                 zNewh94
                                                                                 == z64
                                                                                    ! wordUpdate65)
                                                                                [branch ptrue
                                                                                        (nat_ 1),
                                                                                 branch pfalse
                                                                                        (nat_ 0)]) *
                                                                 int_ -1 +
                                                                 nat2int (case_ (summary95
                                                                                 ! zNewh94)
                                                                                [branch (ppair PVar
                                                                                               PVar)
                                                                                        (\ y98
                                                                                           z99 ->
                                                                                         y98)])) +
                                                        fromProb (topic_prior59 ! zNewh94)) *
                                                       (fromInt (let_ (bucket (nat_ 0)
                                                                              (size w62)
                                                                              ((r_index (\ () ->
                                                                                         size topic_prior59)
                                                                                        (\ (iB101,()) ->
                                                                                         z64
                                                                                         ! iB101)
                                                                                        (r_split (\ (iB101,(zNewh102,())) ->
                                                                                                  w62
                                                                                                  ! wordUpdate65
                                                                                                  == w62
                                                                                                     ! iB101)
                                                                                                 (r_add (\ (iB101,(zNewh102,())) ->
                                                                                                         nat_ 1))
                                                                                                 r_nop)))) $ \ summary100 ->
                                                                 nat2int (case_ (not (nat2int (size topic_prior59) +
                                                                                      int_ -1
                                                                                      < nat2int (z64
                                                                                                 ! wordUpdate65)) &&
                                                                                 zNewh94
                                                                                 == z64
                                                                                    ! wordUpdate65)
                                                                                [branch ptrue
                                                                                        (nat_ 1),
                                                                                 branch pfalse
                                                                                        (nat_ 0)]) *
                                                                 int_ -1 +
                                                                 nat2int (case_ (summary100
                                                                                 ! zNewh94)
                                                                                [branch (ppair PVar
                                                                                               PVar)
                                                                                        (\ y103
                                                                                           z104 ->
                                                                                         y103)])) +
                                                        fromProb (word_prior60
                                                                  ! (w62 ! wordUpdate65))) *
                                                       recip (fromInt (let_ (bucket (nat_ 0)
                                                                                    (size w62)
                                                                                    ((r_index (\ () ->
                                                                                               size topic_prior59)
                                                                                              (\ (iB106,()) ->
                                                                                               z64
                                                                                               ! iB106)
                                                                                              (r_add (\ (iB106,(zNewh107,())) ->
                                                                                                      nat_ 1))))) $ \ summary105 ->
                                                                       nat2int (case_ (not (nat2int (size topic_prior59) +
                                                                                            int_ -1
                                                                                            < nat2int (z64
                                                                                                       ! wordUpdate65)) &&
                                                                                       zNewh94
                                                                                       == z64
                                                                                          ! wordUpdate65)
                                                                                      [branch ptrue
                                                                                              (nat_ 1),
                                                                                       branch pfalse
                                                                                              (nat_ 0)]) *
                                                                       int_ -1 +
                                                                       nat2int (summary105
                                                                                ! zNewh94)) +
                                                              fromProb (summate (nat_ 0)
                                                                                (size word_prior60)
                                                                                (\ iB108 ->
                                                                                 word_prior60
                                                                                 ! iB108)))))

main :: IO ()
main = do
  words_st <- TIO.readFile "../../../news/words"
  docs_st <- TIO.readFile "../../../news/docs"
  topics_st <- TIO.readFile "../../../news/topics"
  let topics =  UV.fromList $ ((Prelude.map (read . T.unpack) (T.lines topics_st)) :: [Int])
  let words = UV.fromList $ ((Prelude.map (read . T.unpack) (T.lines words_st)) :: [Int])
  let docs = UV.fromList $ ((Prelude.map (read . T.unpack) (T.lines docs_st)) :: [Int])

  let numDocs = UV.last docs + 1
  let numTopics = UV.maximum topics + 1
  let numWords = UV.maximum words + 1

  let topic_prior = UV.map LF.logFloat $ UV.replicate numTopics 1.0
  let word_prior = UV.map LF.logFloat  $ UV.replicate numWords 1.0
  let wordUpdate = 42
  g <- MWC.createSystemRandom
  zs <- UV.replicateM (UV.length words) (return 0)

  start_time <- C.getCurrentTime
  result <- return $! (prog topic_prior word_prior numDocs words docs zs wordUpdate)
  end_time <- C.getCurrentTime

  print "result:"
  print result
  print (UV.map LF.logFromLogFloat result)
  print "time:"

  print $ C.diffUTCTime end_time start_time
