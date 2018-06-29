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



import qualified System.Environment as SE
import qualified System.Random.MWC                as MWC
import qualified Data.Number.LogFloat as LF
import qualified Data.Time.Clock as C
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector.Unboxed as UV

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
                                           (summate (nat_ 0)
                                                                                (size word_prior60)
                                                                                (\ iB108 ->
                                                                                 word_prior60
                                                                                 ! iB108)))
main :: IO ()
main = do
  [input_path] <- getArgs
  words_st <- TIO.readFile $ concat [input_path, "words"]
  docs_st <- TIO.readFile $ concat [input_path,  "docs"]
  topics_st <- TIO.readFile $ concat [input_path, "topics"]
  let topics =  UV.fromList $ ((Prelude.map (read . T.unpack) (T.lines topics_st)) :: [Int])
      words = UV.fromList $ ((Prelude.map (read . T.unpack) (T.lines words_st)) :: [Int])
      docs = UV.fromList $ ((Prelude.map (read . T.unpack) (T.lines docs_st)) :: [Int])

  let numDocs = UV.last docs + 1
      numTopics = UV.maximum topics + 1
      numWords = UV.maximum words + 1
      wordLen = UV.length words
      topic_prior = UV.map LF.logFloat $ UV.replicate numTopics 1.0
      word_prior = UV.map LF.logFloat  $ UV.replicate wordLen 1.0
      wordUpdate = 1
      zs = UV.generate wordLen $ const 1
  print [numDocs, numTopics, numWords, wordLen]
  -- g <- MWC.createSystemRandom
  let result0 = prog topic_prior word_prior numDocs words docs zs wordUpdate
  print $ UV.map log result0
