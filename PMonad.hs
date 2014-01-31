{-# LANGUAGE GADTs, ViewPatterns, FlexibleInstances, UndecidableInstances, NoMonomorphismRestriction #-}
module PMonad (MExp, PMonad(..), (>>>), rid, val, expr, valm) where

import Prelude hiding (id,(.))
import Control.Category
import Queue


newtype MCont m a b = MCont { runMC :: a -> m b }

newtype MExp m a b = MExp (CTQueue (MCont m) a b)
-- constructor not exported!

instance PMonad m => Category (MExp m) where
  id = expr return'
  (MExp r) . (MExp l) = MExp (l >< r)

rid = id

class PMonad m where
  return' :: a -> m a
  (>>>=) :: m a -> MExp m a b -> m b


val :: PMonad m => MExp m a b -> (a -> m b)
val (MExp q) = case tviewl q of
  TEmptyL -> return'
  h :| t -> \x -> runMC h x >>>= MExp t


expr :: PMonad m => (a -> m b) -> MExp m a b
expr = MExp . tsingleton . MCont


valm :: PMonad m => MExp m () a -> m a
valm m = val m ()  

instance PMonad m => Monad m where
  m >>= f = m >>>= expr f
  return = return'
