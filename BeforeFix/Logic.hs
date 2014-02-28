module BeforeFix.Logic where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logic.Class
import Control.Monad.IO.Class

newtype ML m a = ML { getML :: m (Maybe (a, ML m a)) } 

single a = return (Just (a,mzero))

instance Monad m => Monad (ML m) where
  return = ML . single
  (ML m) >>= f = ML $ m >>= \x -> case x of
       Nothing    -> return Nothing
       Just (h,t) -> getML (f h `mplus` (t >>= f))
  fail _ = mzero

instance Monad m => MonadPlus (ML m) where
  mzero      = ML (return Nothing)
  mplus (ML a) b = ML $ a >>= \x -> case x of
     Nothing    -> getML b
     Just (h,t) -> return (Just (h,t `mplus` b))

instance MonadTrans ML where
  lift m     = ML (m >>= single)

instance Monad m => MonadLogic (ML m) where
  msplit (ML m) = lift m



observeAllT :: Monad m => ML m a -> m [a]
observeAllT (ML m) = m >>= get where
      get (Just (a,t)) = liftM (a :) (observeAllT t)
      get _            = return []

observeT :: Monad m => ML m a -> m a
observeT (ML m) = m >>= get where
      get (Just (a,t)) = return a
      get _            = fail "No results"


instance (MonadIO m) => MonadIO (ML m) where
    liftIO = lift . liftIO

