{-# LANGUAGE CPP, DataKinds, NegativeLiterals #-}
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
  (Prob ->
   ((MayBoxVec Prob Prob) ->
    ((MayBoxVec Int Int) ->
     ((MayBoxVec Double Double) -> (Int -> (MayBoxVec Prob Prob))))))

prog =
  lam $ \ s44 ->
  lam $ \ as45 ->
  lam $ \ z46 ->
  lam $ \ t47 ->
  lam $ \ docUpdate48 ->
          (array (size as45) $
                                           \ zNewd60 ->
                                           exp (summate (nat_ 0)
                                                        (size as45)
                                                        (\ _a64 ->
                                                         (let_ (bucket (nat_ 0)
                                                                       (size t47)
                                                                       ((r_index (\ () -> size as45)
                                                                                 (\ (i66,()) ->
                                                                                  z46
                                                                                  ! i66)
                                                                                 (r_add (\ (i66,(_a67,())) ->
                                                                                         t47
                                                                                         ! i66))))) $ \ summary65 ->
                                                          case_ (_a64 == zNewd60)
                                                                [branch ptrue (t47 ! docUpdate48),
                                                                 branch pfalse (real_ 0)] +
                                                          case_ (_a64 == z46 ! docUpdate48)
                                                                [branch ptrue (t47 ! docUpdate48),
                                                                 branch pfalse (real_ 0)] *
                                                          real_ (-1) +
                                                          summary65 ! _a64)
                                                         ^ nat_ 2 *
                                                         recip (fromInt (let_ (bucket (nat_ 0)
                                                                                      (size t47)
                                                                                      ((r_index (\ () ->
                                                                                                 size as45)
                                                                                                (\ (i69,()) ->
                                                                                                 z46
                                                                                                 ! i69)
                                                                                                (r_add (\ (i69,(_a70,())) ->
                                                                                                        nat_ 1))))) $ \ summary68 ->
                                                                         nat2int (case_ (_a64
                                                                                         == zNewd60)
                                                                                        [branch ptrue
                                                                                                (nat_ 1),
                                                                                         branch pfalse
                                                                                                (nat_ 0)]) +
                                                                         nat2int (case_ (_a64
                                                                                         == z46
                                                                                            ! docUpdate48)
                                                                                        [branch ptrue
                                                                                                (nat_ 1),
                                                                                         branch pfalse
                                                                                                (nat_ 0)]) *
                                                                         int_ -1 +
                                                                         nat2int (summary68
                                                                                  ! _a64)) *
                                                                fromProb (s44 ^ nat_ 2) +
                                                                real_ 1)) *
                                                fromProb (s44 ^ nat_ 2) *
                                                real_ (1/2)))


main :: IO ()
main = do
  let points = 10
      classes = 9
      stddev = 14
      as = array classes (const 1)
      ts = UV.fromList ([-3.8907259534842202,-6.618193122202014,-4.941794905899988,-7.119531519149578,-5.9345748166212,4.825733087265147,-8.331891801596736,-5.597647903988242,-9.082321592213654,14.219895056304555] :: [Double])
      zs = UV.fromList [4,4,4,4,4,7,0,4,0,6]
      doc = 0
  let result = (prog stddev as zs ts doc)
  print result
  print $ UV.map LF.logFromLogFloat result