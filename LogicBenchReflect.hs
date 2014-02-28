

import Control.Monad
import Control.Monad.Logic.Class
import Control.Monad.Trans
import System.Environment 
import System.IO

-- A micro-benchmark of LogicT measuring repeated reflection

-- the three implementations of Logic:

--import BeforeFix.Logic -- direct style implementation
--import Control.Monad.Logic -- two continuation implementation
import Fixed.Logic -- direct style implementation with our solution applied to it




seqN :: MonadLogic m => Int -> m a -> m [a]
seqN n m | n == 0     = return []
         | otherwise  = msplit m >>= \x -> case x of
                          Nothing    -> return []
                          Just (a,m) -> liftM (a:) $ seqN (n-1) m

nats = natsFrom 0 where
  natsFrom n = return n `mplus` natsFrom (n + 1)

bench n = observeT $ seqN n nats



main = do args <- getArgs 
          let n = read (head args)
          bench n
