-- A micro-benchmark of LogicT

module LogicTMBench where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Logic.Class
import LogicTBad



-- A stream of natural numbers
nats :: MonadPlus m => m Int
nats = go 0
 where go n = return n `mplus` (go $! n+1)

-- Obtain the sequence of at mots n answers from a non-deterministic
-- computation using msplit

seqN 0 m = return []
seqN n m = msplit m >>= \x -> case x of
  Nothing    -> return []
  Just (a,m) -> liftM (a:) $ seqN (n-1) m


runseq :: Int -> [Int]
runseq n = runIdentity $ observe (seqN n nats) 

observe m = do x <- getOne m 
               case x of
                  Just (a,_) -> return a
                  Nothing -> error "fail"


