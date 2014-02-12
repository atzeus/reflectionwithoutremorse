

import Control.Monad
import Control.Monad.Logic.Class
import Control.Monad.Trans
import System.Environment 
import System.IO

-- A micro-benchmark of LogicT

-- the three implementations of Logic:

import Logic -- our new implementation
-- import Control.Monad.Logic -- two continuation implementation
-- import BenchMarks.LogicCC -- delimited continuations implementation


natsFrom :: MonadPlus m => Integer -> m Integer
natsFrom n = return n `mplus` natsFrom (n + 1)
nats :: MonadPlus m => m Integer
nats = natsFrom 0

runseq :: Int -> IO ()
runseq n = do l <- observeT $ seqN n nats
              putStrLn $ show $ length l

seqN :: MonadLogic m => Int -> m a -> m [a]
seqN n m 
  | n == 0     = return []
  | otherwise  = msplit m >>= \x -> case x of
     Nothing    -> return []
     Just (a,m) -> liftM (a:) $ seqN (n-1) m


main = do args <- getArgs 
          let n = read (head args)
          runseq n
