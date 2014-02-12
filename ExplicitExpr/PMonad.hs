{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances, NoMonomorphismRestriction #-}
module ExplicitExpr.PMonad(module Data.FastTCQueue, MExp, MCExp, PMonad(..), expr,val, exprm,valm)  where

import Data.FastTCQueue

newtype MCont m a b = MCont { runMC :: a -> m b }

type MCExp m a b = FastTCQueue (MCont m) a b
type MExp m a = MCExp m () a

class PMonad m where
  return' :: a -> m a
  (>>>=) :: m a -> MCExp m a b -> m b


val :: PMonad m => MCExp m a b -> (a -> m b)
val q = case tviewl q of
  TEmptyL -> return'
  h :| t -> \x -> runMC h x >>>= t


expr :: PMonad m => (a -> m b) -> MCExp m a b
expr =  tsingleton . MCont

exprm :: PMonad m => m a -> MExp m a 
exprm m = expr (\() -> m)
valm :: PMonad m => MExp m  a -> m a
valm m = val m ()  

instance PMonad m => Monad m where
  m >>= f = m >>>= expr f
  return = return'
