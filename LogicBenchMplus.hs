

import Control.Monad
import Control.Monad.Logic.Class
import Control.Monad.Trans
import System.Environment 
import System.IO

-- A micro-benchmark of LogicT measuring left-skewed mplus

-- the three implementations of Logic:

-- import BeforeFix.Logic -- direct style implementation
-- import Control.Monad.Logic -- two continuation implementation
import Fixed.Logic -- direct style implementation with our solution applied to it

leftskewed n = foldl mplus mzero $ replicate n (return 1)

runseq :: Int -> IO ()
runseq n = do l <- observeAllT $ leftskewed n
              putStrLn $ show $ length l

main = do args <- getArgs 
          let n = read (head args)
          runseq n
