-- A micro-benchmark for Iteratees

-- choose Iteratees implementation:

--import BeforeFix.Iteratees
import Fixed.Iteratees
import Control.Monad 





(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = (>>= g) . f

-- get n numbers and return their sum
addNbad :: Int -> It Int Int
addNbad n = foldl (>>=) get (replicate (n - 1) addGet) 
  where addGet x = liftM (+ x) get

testquadratic n = feedAll (addNbad n) [1..n]

{- Benchmark results with IterateesBeforeFix : 

Performance is quadratic:

*Main> testquadratic 1000
Just 500500
(0.28 secs, 126413048 bytes)
*Main> testquadratic 2000
Just 2001000
(1.07 secs, 534004000 bytes)
*Main> testquadratic 3000
Just 4501500
(2.48 secs, 1191993288 bytes)
*Main> testquadratic 4000
Just 8002000
(5.08 secs, 2131577200 bytes)
*Main> testquadratic 5000
Just 12502500
(8.72 secs, 3322301584 bytes)
*Main> testquadratic 6000
Just 18003000
(13.15 secs, 4795560208 bytes)
*Main> testquadratic 7000
Just 24503500
(19.02 secs, 6519277032 bytes)

-}



{- Benchmark results with Iteratees (with fix) : 

Performance is linear

*Main> testquadratic 1000
Just 500500
(0.02 secs, 4778552 bytes)
*Main> testquadratic 2000
Just 2001000
(0.02 secs, 8271360 bytes)
*Main> testquadratic 3000
Just 4501500
(0.03 secs, 12414640 bytes)
*Main> testquadratic 4000
Just 8002000
(0.04 secs, 16615640 bytes)
*Main> testquadratic 5000
Just 12502500
(0.06 secs, 20631368 bytes)
*Main> testquadratic 6000
Just 18003000
(0.06 secs, 24381552 bytes)
*Main> testquadratic 7000
Just 24503500
(0.09 secs, 28647048 bytes)


-}

