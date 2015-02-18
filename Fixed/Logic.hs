{-# LANGUAGE ViewPatterns, LambdaCase #-}
module Fixed.Logic where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logic.Class
import Control.Monad.IO.Class
import Data.FastCQueue

type CQueue = FastCQueue

newtype ML m a = ML ( CQueue (m (Maybe (a, ML m a))))
fromView = ML . singleton

toView :: Monad m => ML m a -> m (Maybe (a, ML m a)) 
toView (ML s) = case viewl s of
   EmptyL -> return Nothing
   h :< t -> h >>= \x -> case x of
     Nothing    -> toView (ML t)
     Just (hi,ML ti) -> return (Just (hi,ML $ ti .>< t))
single a = return (Just (a,mzero))

instance Monad m => Monad (ML m) where
  return = fromView . single
  (toView -> m) >>= f = fromView $ m >>= \x -> case x of
       Nothing    -> return Nothing
       Just (h,t) -> toView (f h `mplus` (t >>= f))
  fail _ = mzero

instance Monad m => MonadPlus (ML m) where
  mzero = ML empty
  mplus (toView -> m) n = fromView $ m >>= return . \case
       Nothing    -> Nothing
       Just (h,t) -> Just (h, cat t n) 
    where cat (ML l) (ML r) = ML $ l .>< r 

instance MonadTrans ML where
  lift m = fromView (m >>= single)
instance Monad m => MonadLogic (ML m) where
  msplit (toView -> m) = lift m

observeAllT :: Monad m => ML m a -> m [a]
observeAllT (toView -> m) = m >>= get where
      get (Just (a,t)) = liftM (a :) (observeAllT t)
      get _            = return []

observeT :: Monad m => ML m a -> m a
observeT (toView -> m) = m >>= get where
      get (Just (a,t)) = return a
      get _            = fail "No results"


instance (MonadIO m) => MonadIO (ML m) where
    liftIO = lift . liftIO

