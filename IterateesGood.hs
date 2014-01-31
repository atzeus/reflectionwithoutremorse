module EfficientIteratees where

import PMonad

data It i a 
  = Get (MExp (It i) i a)
  | Done a

instance PMonad (It i) where
  return' = Done
  (Done x) >>>= g = val g x
  (Get f)  >>>= g = Get (f >>> g)

get = Get rid

race :: It i a -> It i b -> It i (It i a, It i b)
race l r 
  | Done _ <- l = Done (l,r)
  | Done _ <- r = Done (l,r)
  | Get f <- l, Get g <- r =
    do x <- get
       race (val f x) (val g x)
