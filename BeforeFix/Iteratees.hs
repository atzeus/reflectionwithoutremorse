module IterateesBeforeFix where
import Control.Monad

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



feedAll :: It a b -> [a] -> Maybe b
feedAll (Done a) _  = Just a
feedAll _        [] = Nothing
feedAll (Get f)  (h : t) = feedAll (f h) t



{-
Benchmark results, quadratic performance

*Benchmarks.IterateesBench> testquadratic 1000
Just 500500
(0.28 secs, 125736056 bytes)
*Benchmarks.IterateesBench> testquadratic 2000
Just 2001000
(1.08 secs, 533956304 bytes)
*Benchmarks.IterateesBench> testquadratic 4000
Just 8002000
(5.06 secs, 2131837520 bytes)
*Benchmarks.IterateesBench> testquadratic 5000
Just 12502500
(8.40 secs, 3323089264 bytes)
*Benchmarks.IterateesBench> testquadratic 6000
Just 18003000
(13.68 secs, 4795386080 bytes)
-}
