{-# LANGUAGE GADTs #-}
module BeforeFix.TermMonad where


import ExplicitExpr.PMonad


data TermM r a where
  Bind :: r w -> MCExp (TermM r) w a -> TermM r a
  Return :: a -> TermM r a

instance PMonad (TermM r) where
  return' = Return
  (Return x)  >>>= f = val f x
  (Bind r c)  >>>= f = Bind r (c >< f)

