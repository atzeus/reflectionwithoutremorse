{-# LANGUAGE GADTs #-}
module BeforeFix.TermMonad where


-- An innefficient term monad, as can be found for example in the paper the Constrained Monad Problem
-- Unimo and the operational hackage package also use a term monad,
-- but a smarter construction, in which the sequence is presented as a tree instead

data TermM r a where
  Bind :: r w -> (w -> TermM r a) -> TermM r a
  Return :: a -> TermM r a

instance Monad (TermM r) where
  return = Return
  (Return a) >>= f = f a
  (Bind r c) >>= f = Bind r ((>>= f) . c)

