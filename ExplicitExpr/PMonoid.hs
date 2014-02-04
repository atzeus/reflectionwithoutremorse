{-# LANGUAGE GADTs, ViewPatterns, FlexibleInstances, UndecidableInstances, NoMonomorphismRestriction #-}

module ExplicitExpr.PMonoid where

import Data.Monoid
import Data.CQueue

type MExp a = CQueue a

class PMonoid a where
  (.++)   :: a -> MExp a -> a
  mnaught :: a

val :: PMonoid a => MExp a -> a
val q = case viewl q of
  EmptyL -> mnaught
  h :< t -> h .++ t
  
expr = singleton

instance PMonoid a => Monoid a where
  a `mappend` b = a .++ expr b
  mempty = mnaught
  
