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
  lam $ \ w ->
  (bucket (nat_ 0)
    (size w)
    ((r_fanout (r_index (\ () ->
                           size w)
                 (\ (i,()) ->
                     w
                   ! i)
                 (r_add (\ (i, (docUpdate, ())) -> nat_ 1))
                 -- (r_index (\ (docUpdate12,()) ->
                 --              size word_prior1)
                 --   (\ (i丙11,(docUpdate12,())) ->
                 --       w3
                 --       ! i丙11)
                 --   (r_add (\ (i丙11,(i丣13,(docUpdate12,()))) ->
                 --             nat_ 1)))
               )
       r_nop)))

main :: IO ()
main = do
  let result = prog (UV.replicate 100 (1 :: Int))
  print "result:"
  print result
