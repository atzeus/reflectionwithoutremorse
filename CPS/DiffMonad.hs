{-# LANGUAGE TypeSynonymInstances,Rank2Types,FlexibleInstances,UndecidableInstances #-}
module DiffMonad where


newtype DiffMonad m a = DiffMonad {getDM :: forall b. (a -> m b) -> m b} 

abs :: Monad m => DiffMonad m a -> m a
abs (DiffMonad m) = m return

rep :: Monad m => m a -> DiffMonad m a
rep m = DiffMonad (m >>=)

instance Monad m => Monad (DiffMonad m) where
  return x = rep (return x)
  (DiffMonad m) >>= f  = DiffMonad $ \k -> m (\a -> getDM (f a) k)
