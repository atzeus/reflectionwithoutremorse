{-# LANGUAGE ViewPatterns #-}
module BeforeFix.Logic where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logic.Class
import Control.Monad.IO.Class

newtype ML m a = ML { toView :: m (Maybe (a, ML m a)) } 
fromView = ML
single a = return (Just (a,mzero))

instance Monad m => Monad (ML m) where
  return = fromView . single
  (toView -> m) >>= f = fromView $ m >>= \x -> case x of
       Nothing    -> return Nothing
       Just (h,t) -> toView (f h `mplus` (t >>= f))
  fail _ = mzero

instance Monad m => MonadPlus (ML m) where
  mzero = fromView (return Nothing)
  mplus (toView -> a) b = fromView $ a >>= \x -> case x of
     Nothing    -> toView b
     Just (h,t) -> return (Just (h,t `mplus` b))

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

