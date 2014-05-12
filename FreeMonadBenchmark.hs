{-# LANGUAGE ViewPatterns,DeriveFunctor #-}

{- A micro benchmark of Freemonads -}


-- choose and implementation of Free monads
--import BeforeFix.FreeMonad
import Fixed.FreeMonad

import Control.Monad

data Get i a = Get (i -> a) deriving Functor

type It i a = FreeMonad (Get i) a

get :: It i i
get = fromView $ Impure (Get (\x -> return x))

-- This is the same benchmark as for iteratees, but Iteratees are now
-- defined as free monads

addGet :: Int -> It Int Int
addGet x = liftM (+ x) get

(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = (>>= g) . f


-- get n numbers and return their sum
addNbad :: Int -> It Int Int
addNbad n = foldl (>>>) return (replicate n addGet) 0

feedAll :: FreeMonad (Get a) b -> [a] -> Maybe b
feedAll (toView -> Pure a) _  = Just a
feedAll _        [] = Nothing
feedAll (toView -> Impure (Get f))  (h : t) = feedAll (f h) t


testquadratic n = feedAll (addNbad n) [1..n]

{- Benchmark results for FreeMonads without fix:

Performance is quadratic.

*Main> :set +s
*Main> testquadratic 1000
Just 500500
(0.31 secs, 188267392 bytes)
*Main> testquadratic 2000
Just 2001000
(1.20 secs, 810305400 bytes)
*Main> testquadratic 3000
Just 4501500
(2.95 secs, 1807155480 bytes)
*Main> testquadratic 4000
Just 8002000
(5.49 secs, 3242480608 bytes)
*Main> testquadratic 5000
Just 12502500
(10.01 secs, 5052190528 bytes)
*Main> testquadratic 6000
Just 18003000
(15.71 secs, 7297093416 bytes)
-}

{- Benchmark results for FreeMonads with fix:

Performance is linear.

*Main> :set +s
*Main> testquadratic 1000
Just 500500
(0.03 secs, 7693216 bytes)
*Main> testquadratic 2000
Just 2001000
(0.03 secs, 13389752 bytes)
*Main> testquadratic 3000
Just 4501500
(0.04 secs, 20046768 bytes)
*Main> testquadratic 4000
Just 8002000
(0.06 secs, 26376864 bytes)
*Main> testquadratic 5000
Just 12502500
(0.07 secs, 32256552 bytes)
*Main> testquadratic 6000
Just 18003000
(0.09 secs, 38864560 bytes)

-}


