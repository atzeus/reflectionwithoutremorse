module Fixed.Iteratees where
import Control.Monad 

import ExplicitExpr.PMonad

data It i s a 
  = Get (MCExp s (It i) i a)
  | Done a

instance PMonad (It i) where
  return' = Done
  (Done x) >>>= g = val g x
  (Get f)  >>>= g = Get (f >< g)

get :: TSequence s => It i s i
get = Get tempty

race :: TSequence s => It i s a -> It i s b -> It i s (It i s a, It i s b)
race l r 
  | Done _ <- l = Done (l,r)
  | Done _ <- r = Done (l,r)
  | Get f <- l, Get g <- r =
    do x <- get
       race (val f x) (val g x)


feedAll :: TSequence s => It  a s b -> [a] -> Maybe b
feedAll (Done a) _  = Just a
feedAll _        [] = Nothing
feedAll (Get f)  (h : t) = feedAll (val f h) t


{-

Benchmark results, linear performance

*Benchmarks.IterateesBench> testlinear  1000
Just 500500
(0.02 secs, 5182024 bytes)
*Benchmarks.IterateesBench> testlinear  2000
Just 2001000
(0.03 secs, 8357936 bytes)
*Benchmarks.IterateesBench> testlinear  3000
Just 4501500
(0.04 secs, 12465048 bytes)
*Benchmarks.IterateesBench> testlinear  4000
Just 8002000
(0.04 secs, 16730936 bytes)
*Benchmarks.IterateesBench> testlinear  5000
Just 12502500
(0.07 secs, 20965584 bytes)
*Benchmarks.IterateesBench> testlinear  6000
Just 18003000
(0.07 secs, 24235448 bytes)
*Benchmarks.IterateesBench> testlinear  7000
Just 24503500
-}

