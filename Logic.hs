{-# LANGUAGE TypeSynonymInstances,NamedFieldPuns #-}

module Logic where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logic.Class
import ExplicitExpr.PMonadPlus
import Data.CQueue
import Prelude

{-
newtype ML m a = ML { getML :: m (Maybe (a, ML m a)) } 

single a = return (Just (a,mzero))

instance Monad m => Monad (ML m) where
  return = ML . single
  (ML m) >>= f = ML $ m >>= \x -> case x of
       Nothing    -> return Nothing
       Just (h,t) -> getML (f h `mplus` (t >>= f))

instance Monad m => MonadPlus (ML m) where
  mzero      = ML (return Nothing)
  mplus (ML a) b = ML $ a >>= \x -> case x of
     Nothing    -> getML b
     Just (h,t) -> return (Just (h,t `mplus` b))

instance MonadTrans ML where
  lift m     = ML (m >>= single)

instance Monad m => MonadLogic (ML m) where
  msplit (ML m) = lift m
-}

newtype ML m a = ML { getML :: m (Maybe (a, CQueue (ML m a))) } 

single a = return (Just (a,empty))

instance Monad m => Monad (ML m) where
  return = ML . single
  (ML m) >>= f = ML $ m >>= \x -> case x of
       Nothing    -> return Nothing
       Just (h,t) -> getML (f h `mplus` (val t >>= f))

instance Monad m => PMonadPlus (ML m) where
  mzero'      = ML (return Nothing)
  mplus' (ML a) b = ML $ a >>= \x -> case x of
     Nothing    -> getML (val b)
     Just (h,t) -> return (Just (h,t .>< b))

instance MonadTrans ML where
  lift m     = ML (m >>= single)

valTup :: Monad m => m (Maybe (a, CQueue (ML m a))) -> m (Maybe (a,ML m a))
valTup m = liftM (fmap (\(a,b) -> (a,val b))) m

instance Monad m => MonadLogic (ML m) where
  msplit (ML m) = lift (valTup m)

observeT :: Monad m => ML m a -> m a
observeT (ML m) = liftM get m where
  get (Just (a,_)) = a
  get _            = error "No answers"

