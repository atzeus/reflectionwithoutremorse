{-# LANGUAGE Rank2Types #-}
module LogicBenchmarks.LogicCC where

import Control.Monad
import Control.Monad.Logic.Class
import Control.Monad.CC
import Control.Monad.Reader


promptP :: Monad m => (Prompt r a -> CCT r m a) -> CCT r m a
promptP = reset
abortP :: Monad m => Prompt r a -> CCT r m a -> CCT r m b
abortP = abort
shiftP :: Monad m => Prompt r b -> ((CCT r m a -> CCT r m b) -> CCT r m b) -> CCT r m a
shiftP = shift

newtype SR r m a = SR { unSR :: forall ans. ReaderT (Prompt r (Tree r m ans)) (CCT r m) a }

data Tree r m a = HZero
                | HOne a
                | HChoice a (CCT r m (Tree r m a))

instance Monad m => Monad (SR r m) where
  return e = SR (return e)
  (SR m) >>= f = SR (m >>= (unSR . f ))

instance Monad m => MonadPlus (SR r m) where
  mzero = SR (ask >>= \pr -> lift (abortP pr (return HZero)))
  m1 `mplus` m2 = SR (ask >>= \pr -> 
      lift $ shiftP pr $ \sk -> 
        do f1 <- sk (runReaderT (unSR m1) pr)
           let f2 = sk (runReaderT (unSR m2) pr)
           compose_trees f1 f2)

compose_trees :: Monad m => Tree r m a -> CCT r m (Tree r m a) -> CCT r m (Tree r m a)
compose_trees HZero r = r
compose_trees (HOne a) r = return $ HChoice a r
compose_trees (HChoice a r') r =
   return $ HChoice a $ r' >>= (\v -> compose_trees v r)

reify :: Monad m => SR r m a -> CCT r m (Tree r m a)
reify m = promptP (\pr -> runReaderT (unSR m) pr >>= (return . HOne))

instance MonadTrans (SR r) where
  lift m = SR (lift (lift m))

instance Monad m => MonadLogic (SR r m) where
  msplit m = SR (lift (reify m >>= (return . reflect_sr))) 
    where reflect_sr HZero = Nothing
          reflect_sr (HOne a) = Just (a,mzero)
          reflect_sr (HChoice a r1) = Just (a,SR (lift r1) >>=
                                          (return . reflect_sr) >>= 
                                          reflect)


-- this fixes a bug in standard impl..
  once m = msplit m >>= \x -> case x of
            Just (a,_) -> return a
            Nothing    -> mzero


observeAllT :: Monad m => (forall ans . SR ans m a) -> m [a]
observeAllT m = runCCT  (reify m >>= flatten)
  where 
  flatten HZero = return []
  flatten (HOne a) = return [a]
  flatten (HChoice a r) = r >>= flatten >>= (return . (a:))
