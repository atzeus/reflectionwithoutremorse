{-# LANGUAGE GADTs,ViewPatterns,ExistentialQuantification,GADTs #-}
module Fixed.Iteratees where
import Data.Interface.TSequence
import Data.FastTCQueue

type TCQueue = FastTCQueue

newtype IC i a b = IC (a -> It i b)
type IExp i a b = TCQueue (IC i) a b
data It i a = forall x. It (ItView i x) (IExp i x a)
data ItView i a 	= Done a 
                        | Get (i -> It i a)
fromView x = It x tempty

toView :: It i a -> ItView i a
toView (It h t) = case h of
   Done x -> 
    case tviewl t of
       TEmptyL -> Done x
       IC hc :| tc -> toView (hc x >>>= tc)
   Get f -> Get ((>>>= t) . f) 
 where (>>>=) :: It i a -> IExp i a b -> It i b 
       (It h t) >>>= r = It h (t >< r)

instance Monad (It i) where
  return = fromView . Done
  (It m r) >>= f = It m (r >< tsingleton (IC f))

get :: It i i
get = fromView (Get return) 

race :: It i a -> It i b -> It i (It i a, It i b)
race (toView -> l) (toView -> r) 
  | Done _ <- l = return (fromView l, fromView r)
  | Done _ <- r = return (fromView l, fromView r)
  | Get f <- l, Get g <- r =
    do x <- get
       race (f x) (g x)


feedAll :: It  a b -> [a] -> Maybe b
feedAll (toView ->Done a) _  = Just a
feedAll _        [] = Nothing
feedAll (toView -> Get f)  (h : t) = feedAll (f h) t


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

