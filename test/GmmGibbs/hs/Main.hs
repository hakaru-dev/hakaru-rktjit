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
                                           unsafeProb ((fromInt (let_ (bucket (nat_ 0)
                                                                              (size t47)
                                                                              ((r_index (\ () ->
                                                                                         size as45)
                                                                                        (\ (_a62,()) ->
                                                                                         z46
                                                                                         ! _a62)
                                                                                        (r_add (\ (_a62,(zNewd63,())) ->
                                                                                                nat_ 1))))) $ \ summary61 ->
                                                                 nat2int (case_ (zNewd60
                                                                                 == z46
                                                                                    ! docUpdate48)
                                                                                [branch ptrue
                                                                                        (nat_ 1),
                                                                                 branch pfalse
                                                                                        (nat_ 0)]) *
                                                                 int_ -1 +
                                                                 nat2int (summary61 ! zNewd60)) +
                                                        fromProb (as45 ! zNewd60)) *
                                                       fromProb (recip (nat_ 2
                                                                        `thRootOf` (unsafeProb (product (nat_ 0)
                                                                                                        (size as45)
                                                                                                        (\ _b71 ->
                                                                                                         fromInt (let_ (bucket (nat_ 0)
                                                                                                                               (size t47)
                                                                                                                               ((r_index (\ () ->
                                                                                                                                          size as45)
                                                                                                                                         (\ (_a73,()) ->
                                                                                                                                          z46
                                                                                                                                          ! _a73)
                                                                                                                                         (r_add (\ (_a73,(_b74,())) ->
                                                                                                                                                 nat_ 1))))) $ \ summary72 ->
                                                                                                                  nat2int (case_ (_b71
                                                                                                                                  == zNewd60)
                                                                                                                                 [branch ptrue
                                                                                                                                         (nat_ 1),
                                                                                                                                  branch pfalse
                                                                                                                                         (nat_ 0)]) +
                                                                                                                  nat2int (case_ (_b71
                                                                                                                                  == z46
                                                                                                                                     ! docUpdate48)
                                                                                                                                 [branch ptrue
                                                                                                                                         (nat_ 1),
                                                                                                                                  branch pfalse
                                                                                                                                         (nat_ 0)]) *
                                                                                                                  int_ -1 +
                                                                                                                  nat2int (summary72
                                                                                                                           ! _b71)) *
                                                                                                         fromProb (s44
                                                                                                                   ^ nat_ 2) +
                                                                                                         real_ 1)))))) *
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
      classes = 3
      stddev = 14
      as = array classes (const 1)
      ts = UV.fromList ([3.8728103253204136,1.1452918218810444,-0.37443733246614497,2.2524280674567634,0.1088871787126991,2.2484645323958334,0.19013878436498044,1.4032911741452248,2.1930977191694936,1.7312282946567383] :: [Double])
      zs = UV.fromList [2,2,2,2,1,2,2,2,2,2]
      doc = 0
  print (prog stddev as zs ts doc)
