{-# LANGUAGE GADTs,FlexibleInstances,UndecidableInstances #-}

module PMonadPlus where


import Control.Monad
import CQueue
import Data.Monoid

newtype MExp s a = MExp (CQueue (s a))
 -- constructor not exported!
 
class PMonadPlus s where
  (.++) :: s a -> MExp s a -> s a
  mnaught    :: s a
  -- laws:
  -- (a .++ b) .++ c === a .++ exp (b .++ c)
  -- a .++ exp mzero === a
  -- mzero .++ exp b = b

val :: PMonadPlus s => MExp s a -> s a
val (MExp q) = case viewl q of
  EmptyL -> mnaught
  h :< t -> h .++ MExp t
  
expr = MExp . singleton

instance (PMonadPlus s, Monad s) => MonadPlus s where
  mzero = mnaught
  mplus a b = a .++ expr b
  
instance Monoid (MExp s a) where
  (MExp a) `mappend` (MExp b) = MExp (a .>< b)
  mempty = MExp empty
