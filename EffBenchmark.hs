import EffBeforeFix
import System.Environment 
import System.IO

-- primes (very inefficiently -- but a good example of ifte)
test_ifte n = do
  n <- gen
  guard' $ n > 1
  ifte (do
     d <- gen
     guard' $ d < n && d > 1 && n `mod` d == 0
     -- _ <- trace ("d: " ++ show d) (return ())
     return d)
    (\_->mzero')
    (return n)
 where gen = choose [1..n]

test_ifte_run = run . makeChoice . test_ifte 

main :: IO ()
main = do args <- getArgs 
          let n = read (head args)
          let x = test_ifte_run n
          print (show $ length x)
