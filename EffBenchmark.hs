{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction, IncoherentInstances #-}
-- choose implementation
--import Fixed.Eff
import BeforeFix.Eff

{- A benchmark for extensible effects -}

import System.Environment 
import System.IO
import Control.Monad
import Data.OpenUnion1
import Data.Typeable


cutEm n m | n == 0 = m
          | otherwise =  ifte (cutEm (n-1) m) return (return 1)

test
  :: (Member Choose r,
      Member (State Int) r) =>
     Int -> Eff r Int
test  n = cutEm n (return 1 `mplus'` (buildStack n >> get))
runTest n =  snd $ run $ runState (makeChoice  (test n)) (0 :: Int)

modify :: Member (State Int) r => (Int -> Int) -> Eff r ()
modify f = get >>= put . f

buildStack :: Member (State Int) r => Int -> Eff r ()
buildStack n | n == 0 = return ()
             | otherwise = buildStack (n - 1) >> modify (+ 1) 

{- With BeforeFix.Eff (original)

performance is quadratic:

*Main> :set +s
*Main> runTest 500
500
(0.64 secs, 336601864 bytes)
*Main> runTest 1000
1000
(2.51 secs, 1335003648 bytes)
*Main> runTest 1500
1500
(6.40 secs, 2996091784 bytes)
*Main> runTest 2000
2000
(12.82 secs, 5318262672 bytes)
*Main> runTest 2500
2500
(21.99 secs, 8303964304 bytes)
-}

{- With Fixed.Eff 

performance is linear:

*Main> runTest 500
500
(0.05 secs, 15492192 bytes)
*Main> runTest 1000
1000
(0.06 secs, 28215896 bytes)
*Main> runTest 1500
1500
(0.08 secs, 41812808 bytes)
*Main> runTest 2000
2000
(0.13 secs, 55871168 bytes)
*Main> runTest 2500
2500
(0.15 secs, 69804592 bytes)


-}


