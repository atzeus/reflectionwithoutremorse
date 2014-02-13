module FreeMonad(module ExplicitExpr.PMonad, FreeMonad(..)) where


import ExplicitExpr.PMonad


data FreeMonad f a = Pure a | Impure (f (MExp (FreeMonad f) a))

instance Functor f => PMonad (FreeMonad f) where
  return' = Pure
  (Pure x)    >>>= f = val f x
  (Impure t)  >>>= f = Impure (fmap (>< f) t)




