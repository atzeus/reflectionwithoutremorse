{-# LANGUAGE GADTs, ViewPatterns, FlexibleInstances, UndecidableInstances, NoMonomorphismRestriction #-}

module PMonoid (MExp, PMonoid(..), val, expr) where

import Data.Monoid
import CQueue

newtype MExp a = MExp (CQueue a)
 -- constructor not exported!
 
class PMonoid a where
  (.++) :: a -> MExp a -> a
  mnaught    :: a
  -- laws:
  -- (a .++ b) .++ c === a .++ exp (b .++ c)
  -- a .++ exp mzero === a
  -- mzero .++ exp b = b

val :: PMonoid a => MExp a -> a
val (MExp q) = case viewl q of
  EmptyL -> mnaught
  h :< t -> h .++ MExp t
  
expr = MExp . singleton

instance PMonoid a => Monoid a where
  a `mappend` b = a .++ expr b
  mempty = mnaught
  
instance Monoid (MExp a) where
  (MExp a) `mappend` (MExp b) = MExp (a .>< b)
  mempty = MExp empty
