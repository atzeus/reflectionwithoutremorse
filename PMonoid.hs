module PMonoid (MExp, PMonoid(..), val, exp) where

import Data.Monoid

newtype MExp a = MExp (CQueue a)
 -- constructor not exported!
 
class PMonoid a where
  (.++) :: a -> MExp a -> a
  mzero :: a
  -- laws:
  -- (a .++ b) .++ c === a .++ exp (b .++ c)
  -- a .++ exp mzero === a
  -- mzero .++ exp b = b

val :: PMonoid a => MExp a -> a
val (MExp q) = case viewl q of
  EmptyL -> mzero
  h :< t -> h .++ MExp t
  
exp = MExp . singleton

instance PMonoid a => Monoid a where
  a `mappend` b = a .++ exp b
  mempty = mzero
  
instance Monoid (MExp a) where
  (MExp a) `mappend` (MExp b) = MExp (a .>< b)
  mempty = MExp empty
