-- A micro benchmark of delimited continuations

-- choose delimited continuations implementation:

--import Control.Monad.CC -- from Hackage package delim-cc
import Fixed.CCT -- our fixed implementation of the above

import Control.Monad.Identity



test n = 
  do p <- newPrompt
     q <- newPrompt
     pushPrompt p $ 
      buildStack p q
  where  
  buildStack p q = loop n where
     loop z  | z == 0 = withSubCont p (inner q)
             | otherwise = loop (z - 1) >>= return
                         
  inner q sk = loop n where
     loop z | z == 0 = return n
     loop z | otherwise = noop >> loop (z - 1)
     noop = pushPrompt q $ pushSubCont sk $ abort q (return 0)


doTest n = runIdentity $ runCCT $ test n

{- Benchmark results for Control.Monad.CC:

Performance is quadratic

*Main> :set +s
*Main> doTest 1000
1000
(0.07 secs, 120225240 bytes)
*Main> doTest 2000
2000
(0.09 secs, 389453600 bytes)
*Main> doTest 3000
3000
(0.22 secs, 921198520 bytes)
*Main> doTest 4000
4000
(0.39 secs, 1548533512 bytes)
*Main> doTest 5000
5000
(0.64 secs, 2498885656 bytes)
*Main> doTest 6000
6000
(1.02 secs, 3674215808 bytes)
*Main> doTest 7000
7000
(1.44 secs, 4846641832 bytes)
*Main> doTest 8000
8000
(1.92 secs, 6441650728 bytes)
*Main> doTest 9000
9000
(3.54 secs, 7966412200 bytes)
*Main> doTest 10000
10000
(5.76 secs, 9978747376 bytes)
-}

{-
Benchmark results with  Fixed.CCT:

Performance is quadratic

*Main> :set +s
*Main> doTest 1000
1000
(0.05 secs, 12501936 bytes)
*Main> doTest 2000
2000
(0.05 secs, 11983000 bytes)
*Main> doTest 3000
3000
(0.07 secs, 17924656 bytes)
*Main> doTest 4000
4000
(0.08 secs, 23409200 bytes)
*Main> doTest 5000
5000
(0.10 secs, 28851880 bytes)
*Main> doTest 6000
6000
(0.10 secs, 34309112 bytes)
*Main> doTest 7000
7000
(0.12 secs, 39732464 bytes)
*Main> doTest 8000
8000
(0.14 secs, 44878928 bytes)
*Main> doTest 9000
9000
(0.18 secs, 50686240 bytes)
*Main> doTest 10000
10000
(0.16 secs, 56221408 bytes)

-}
