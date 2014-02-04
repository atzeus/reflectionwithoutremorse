{-# LANGUAGE GADTs,FlexibleInstances,UndecidableInstances #-}

module ExplicitExpr.PMonadPlus where

import Control.Monad
import Data.CQueue

type MExp s a = CQueue (s a)
 
class PMonadPlus s where
  mplus' :: s a -> MExp s a -> s a
  mzero'    :: s a

val :: PMonadPlus s => MExp s a -> s a
val q = case viewl q of
  EmptyL -> mzero'
  h :< t ->   mplus' h t
  
expr =  singleton

instance (PMonadPlus s, Monad s) => MonadPlus s where
  mzero =  mzero'
  mplus a b = mplus' a (expr b)
  
