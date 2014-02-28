{-# LANGUAGE TypeSynonymInstances,NamedFieldPuns #-}

module Fixed.Logic where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logic.Class
import Control.Monad.IO.Class
import ExplicitExpr.PMonadPlus

import Prelude

newtype ML m a = ML { getML :: m (Maybe (a,MExp (ML m) a)) } 

single a = return (Just (a,empty))

instance Monad m => Monad (ML m) where
  return = ML . single
  (ML m) >>= f = ML $ m >>= \x -> case x of
       Nothing    -> return Nothing
       Just (h,t) -> getML (f h `mplus` (val t >>= f))
  fail _ = mzero

instance Monad m => PMonadPlus (ML m) where
  mzero'      = ML (return Nothing)
  mplus' (ML a) b = ML $ a >>= \x -> case x of
     Nothing    -> getML (val b)
     Just (h,t) -> return (Just (h,t .>< b))

instance MonadTrans ML where
  lift m     = ML (m >>= single)

valTup :: Monad m => m (Maybe (a, MExp (ML m) a)) -> m (Maybe (a,ML m a))
valTup m = liftM (fmap (\(a,b) -> (a,val b))) m

instance Monad m => MonadLogic (ML m) where
  msplit (ML m) = lift (valTup m)

observeAllT :: Monad m => ML m a -> m [a]
observeAllT (ML m) = m >>= get where
      get (Just (a,t)) = liftM (a :) (observeAllT (val t))
      get _            = return []

observeT :: Monad m => ML m a -> m a
observeT (ML m) = m >>= get where
      get (Just (a,t)) = return a
      get _            = fail "No results"

instance (MonadIO m) => MonadIO (ML m) where
    liftIO = lift . liftIO

