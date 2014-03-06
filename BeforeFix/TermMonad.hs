{-# LANGUAGE GADTs #-}
module BeforeFix.TermMonad where


-- An innefficient term monad, as can be found for example in the paper the "Constrained Monad Problem" ICFP 2013
-- and also in the paper "Handlers in Action" ICFP 2013, where they do note that it is inefficient and use 
-- the codensity monad to solve this
-- 
-- Unimo and the operational hackage package also use a term monad,
-- but a smarter construction, in which the sequence is presented as a tree instead

data TermM r a where
  Bind :: r w -> (w -> TermM r a) -> TermM r a
  Return :: a -> TermM r a

instance Monad (TermM r) where
  return = Return
  (Return a) >>= f = f a
  (Bind r c) >>= f = Bind r ((>>= f) . c)

