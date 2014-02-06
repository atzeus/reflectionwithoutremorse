{-# LANGUAGE GADTs,FlexibleInstances,UndecidableInstances,NoMonomorphismRestriction #-}

module ExplicitExpr.PMonadPlus(module Data.Interface.Sequence, MExp,(><),val,expr, PMonadPlus(..))  where

import Control.Monad
import Data.LowerTSequence
import Data.Interface.Sequence
import qualified Data.CTQueue as C
import qualified Data.RTQueue as R

(><) = (.><)

type MExp s a = MSeq (C.CTQueue R.RTQueue) (s a)
 
class PMonadPlus s where
  mplus' :: s a -> MExp s a -> s a
  mzero' :: s a

val :: PMonadPlus s => MExp s a -> s a
val q = case viewl q of
  EmptyL -> mzero'
  h :< t ->   mplus' h t
  
expr =  singleton

instance (PMonadPlus s, Monad s) => MonadPlus s where
  mzero =  mzero'
  mplus a b = mplus' a (expr b)
  
