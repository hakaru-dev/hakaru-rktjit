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

prog ::
  ((MayBoxVec Prob Prob) ->
   ((MayBoxVec Prob Prob) ->
    ((MayBoxVec Int Int) ->
     ((MayBoxVec Int Int) ->
      ((MayBoxVec Int Int) -> (Int -> (MayBoxVec Prob Prob)))))))
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
                        \ zNew6 ->
                        product (nat_ 0)
                                (size topic_prior0)
                                (\ i7 ->
                                 product (nat_ 0)
                                         (size word_prior1)
                                         (\ i_a8 ->
                                          product (nat_ 0)
                                                  (summate (nat_ 0)
                                                           (size w3)
                                                           (\ i_b10 ->
                                                            case_ (docUpdate5 == doc4 ! i_b10)
                                                                  [branch ptrue
                                                                          (case_ (i7 == zNew6 &&
                                                                                  i_a8
                                                                                  == w3 ! i_b10)
                                                                                 [branch ptrue
                                                                                         (nat_ 1),
                                                                                  branch pfalse
                                                                                         (nat_ 0)]),
                                                                   branch pfalse (nat_ 0)]))
                                                  (\ j9 ->
                                                   nat2prob (summate (nat_ 0)
                                                                     (size w3)
                                                                     (\ i_b11 ->
                                                                      case_ (doc4 ! i_b11
                                                                             == docUpdate5)
                                                                            [branch ptrue (nat_ 0),
                                                                             branch pfalse
                                                                                    (case_ (i7
                                                                                            == z2
                                                                                               ! (doc4
                                                                                                  ! i_b11) &&
                                                                                            i_a8
                                                                                            == w3
                                                                                               ! i_b11)
                                                                                           [branch ptrue
                                                                                                   (nat_ 1),
                                                                                            branch pfalse
                                                                                                   (nat_ 0)])])) +
                                                   nat2prob j9 +
                                                   word_prior1 ! i_a8))) *
                        (nat2prob (summate (nat_ 0)
                                           (size z2)
                                           (\ i_b12 ->
                                            case_ (i_b12 == docUpdate5)
                                                  [branch ptrue (nat_ 0),
                                                   branch pfalse
                                                          (case_ (zNew6 == z2 ! i_b12)
                                                                 [branch ptrue (nat_ 1),
                                                                  branch pfalse (nat_ 0)])])) +
                         topic_prior0 ! zNew6) *
                        recip (nat2prob (summate (nat_ 0)
                                                 (size z2)
                                                 (\ i_b13 ->
                                                  case_ (i_b13 == docUpdate5)
                                                        [branch ptrue (nat_ 0),
                                                         branch pfalse
                                                                (case_ (z2 ! i_b13 < nat_ 0)
                                                                       [branch ptrue (nat_ 0),
                                                                        branch pfalse
                                                                               (nat_ 1)])])) +
                               summate (nat_ 0)
                                       (size topic_prior0)
                                       (\ i_b14 -> topic_prior0 ! i_b14)) *
                        recip (product (nat_ 0)
                                       (size topic_prior0)
                                       (\ i15 ->
                                        product (nat_ 0)
                                                (summate (nat_ 0)
                                                         (size w3)
                                                         (\ i_b17 ->
                                                          case_ (docUpdate5 == doc4 ! i_b17)
                                                                [branch ptrue
                                                                        (case_ (not (w3 ! i_b17
                                                                                     < nat_ 0) &&
                                                                                i15 == zNew6)
                                                                               [branch ptrue
                                                                                       (nat_ 1),
                                                                                branch pfalse
                                                                                       (nat_ 0)]),
                                                                 branch pfalse (nat_ 0)]))
                                                (\ i_a16 ->
                                                 nat2prob (summate (nat_ 0)
                                                                   (size w3)
                                                                   (\ i_b18 ->
                                                                    case_ (doc4 ! i_b18
                                                                           == docUpdate5)
                                                                          [branch ptrue (nat_ 0),
                                                                           branch pfalse
                                                                                  (case_ (not (w3
                                                                                               ! i_b18
                                                                                               < nat_ 0) &&
                                                                                          i15
                                                                                          == z2
                                                                                             ! (doc4
                                                                                                ! i_b18))
                                                                                         [branch ptrue
                                                                                                 (nat_ 1),
                                                                                          branch pfalse
                                                                                                 (nat_ 0)])])) +
                                                 nat2prob i_a16 +
                                                 summate (nat_ 0)
                                                         (size word_prior1)
                                                         (\ i_b19 -> word_prior1 ! i_b19)))))),
         branch pfalse ((array (nat_ 0) $ \ i20 -> prob_ 0))]
