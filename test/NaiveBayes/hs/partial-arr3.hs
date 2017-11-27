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
     ((MayBoxVec Int Int) -> ((MayBoxVec Int Int) -> (Int -> (MayBoxVec Prob Prob)))))))

prog =
  lam $ \ topic_prior60 ->
  lam $ \ word_prior61 ->
  lam $ \ z62 ->
  lam $ \ w63 ->
  lam $ \ doc64 ->
  lam $ \ docUpdate65 ->
  case_ (docUpdate65 < size z62)
        [branch ptrue
                ((array (size topic_prior60) $
                                           \ zNewf93 ->
                                             (nat2prob (nat_ 0)-- (let_ (bucket (nat_ 0)
                                                       --                    (size w63)
                                                       --                    ((r_split (\ (iF104,()) ->
                                                       --                               doc64
                                                       --                               ! iF104
                                                       --                               == docUpdate65)
                                                       --                              r_nop
                                                       --                              (r_index (\ () ->
                                                       --                                        size word_prior61)
                                                       --                                       (\ (iF104,()) ->
                                                       --                                        w63
                                                       --                                        ! iF104)
                                                       --                                       (r_index (\ (iB105,()) ->
                                                       --                                                 size topic_prior60)
                                                       --                                                (\ (iF104,(iB105,())) ->
                                                       --                                                 z62
                                                       --                                                 ! (doc64
                                                       --                                                    ! iF104))
                                                       --                                                (r_add (\ (iF104,(i106,(iB105,()))) ->
                                                       --                                                        nat_ 1))))))) $ \ summary103 ->
                                                       --       case_ summary103
                                                       --             [branch (ppair PVar
                                                       --                            PVar)
                                                       --                     (\ y107
                                                       --                        z108 ->
                                                       --                      z108)]
                                                       --       ! (nat_ 0)
                                                       --       ! (nat_ 0))
                                              +
                                                   -- nat2prob (nat_ 0) +
                                                   word_prior61
                                                   ! (nat_ 0)
                                             ))),
         branch pfalse (UV.empty)]

main :: IO ()
main = do
  let topics =  UV.fromList [2, 1, 0, 2, 1, 0, 0]
  let words = UV.fromList [0, 3, 3, 2, 1, 2, 0, 1, 2, 3, 3, 0, 0, 3, 2, 1, 0]
  let docs = UV.fromList [0, 0, 1, 1, 1, 2, 2, 2, 3, 4, 4, 5, 5, 5, 6, 6, 6]

  let numDocs = UV.last docs + 1
  let numTopics = UV.maximum topics + 1
  let numWords = UV.maximum words + 1

  let topic_prior = array numTopics (const 1.0) -- UV.map LF.logFloat $ UV.replicate numTopics 1.0
  let word_prior = array numWords (const 1.0) -- UV.map LF.logFloat  $ UV.replicate numWords 1.0

  let docUpdate = 4
  let zs = topics

  let result = (prog topic_prior word_prior zs words docs docUpdate)
  print "result"
  print result
  -- print "result in logdomain:"
  -- print (UV.map LF.logFromLogFloat result)
