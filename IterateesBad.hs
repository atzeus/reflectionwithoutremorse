module IterateesBad where

data It i a 
  = Get (i -> It i a)
  | Done a

instance Monad (It i) where
  return = Done
  (Done x)  >>= g = g x
  (Get f)   >>= g = Get ((>>= g) . f)

get = Get return

race :: It i a -> It i b -> It i (It i a, It i b)
race l r 
  | Done _ <- l = Done (l,r)
  | Done _ <- r = Done (l,r)
  | Get f <- l, Get g <- r =
    do x <- get
       race (f x) (g x)
