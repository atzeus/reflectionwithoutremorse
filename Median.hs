{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Control.Monad.Logic.Class
import Control.Monad.Trans
import System.Environment 
import System.IO
import Data.List
-- compute the median of the arguments 

-- code stolen from hstats

-- |Numerically stable mean
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

-- |Same as 'mean' 
average :: Floating a => [a] -> a
average = mean

-- |Harmonic mean
harmean :: (Floating a) => [a] -> a
harmean xs = fromIntegral (length xs) / (sum $ map (1/) xs)

-- |Geometric mean
geomean :: (Floating a) => [a] -> a
geomean xs = (foldr1 (*) xs)**(1 / fromIntegral (length xs))

-- |Median
median :: (Floating a, Ord a) => [a] -> a
median x | odd n  = head  $ drop (n `div` 2) x'
         | even n = mean $ take 2 $ drop i x'
                  where i = (length x' `div` 2) - 1
                        x' = sort x
                        n  = length x

main = do args <- getArgs 
          let n = map read args :: [Double]
          putStrLn $ show $ median n
