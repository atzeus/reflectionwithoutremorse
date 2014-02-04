{-# LANGUAGE GADTs, FlexibleInstances, UndecidableInstances, NoMonomorphismRestriction #-}
module ExplicitExpr.PMonad where

import Data.CTQueue

newtype MCont m a b = MCont { runMC :: a -> m b }

type MExp m a b = CTQueue (MCont m) a b

class PMonad m where
  return' :: a -> m a
  (>>>=) :: m a -> MExp m a b -> m b


val :: PMonad m => MExp m a b -> (a -> m b)
val q = case tviewl q of
  TEmptyL -> return'
  h :| t -> \x -> runMC h x >>>= t


expr :: PMonad m => (a -> m b) -> MExp m a b
expr =  tsingleton . MCont

exprm :: PMonad m => m a -> MExp m () a 
exprm m = expr (\() -> m)
valm :: PMonad m => MExp m () a -> m a
valm m = val m ()  

instance PMonad m => Monad m where
  m >>= f = m >>>= expr f
  return = return'
