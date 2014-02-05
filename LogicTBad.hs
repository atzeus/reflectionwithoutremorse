{-# LANGUAGE TypeSynonymInstances,FlexibleInstances, NoMonomorphismRestriction #-}

module LogicTBad where

import CQueue
import Data.Monoid
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logic.Class
import Prelude hiding (foldl,concatMap)








data Logic m a = Pick a
               | Lift (m (Logic m a))
               | Choose (CQueue (Logic m a))

instance Monad m => Monad (Logic m) where
  return = Pick 
  (Pick x) >>= f = f x
  (Lift m) >>= f = Lift (liftM (>>= f) m)
  (Choose q) >>= f = Choose (fmap (>>= f) q)

instance Monad m => MonadPlus (Logic m) where
  mzero       = Choose empty
  l `mplus` r = Choose (singleton l .>< singleton r)

instance MonadTrans Logic where
  lift m = Lift (liftM Pick m)

instance Monad m => MonadLogic (Logic m) where
  msplit = lift . getOne

  -- this fixes a bug in the standard 
  -- impl....
  once m = do x <- msplit m
              case x of
                Just (a,_) -> return a
                Nothing    -> mzero


getOne (Pick x)   = return (Just (x,mzero))
getOne (Lift m)   = m >>= getOne
getOne (Choose q) = case viewl q of
  EmptyL -> return Nothing
  h :< t -> do f <- getOne h 
               case f of
                Nothing -> getOne (Choose t)
                Just (f,ft) -> return (Just (f, ft `mplus` Choose t))
{-


data List a = Nil | a :~~ List a

Nil       +++ r = r
(h :~~ t) +++ r = r

concatMap f Nil = Nil
concatMap f (h :~~ t) = f h +++ concatMap f t

single a = a :~~ Nil

instance Monad List where
  return = single
  (>>=)  = flip concatMap

instance MonadPlus List where
  mzero = Nil
  mplus = (+++)

instance MonadLogic List where
  msplit Nil = single Nothing
  msplit (h :~~ t) = single (Just (h,t))

newtype MList  m a = MList { getML :: m (MListR m a) }
data    MListR m a = a :~ MList m a
                   | End

mconc :: Monad m => MList m a -> MList m a -> MList m a
(MList l) `mconc` r = MList (l >>= (`mconcr` r))
End      `mconcr` r = getML r
(h :~ t) `mconcr` r = return $ h :~ (t `mconc` r)


concatMapMr :: Monad m =>(a -> MList m b) -> MList m a -> MList m b
concatMapMr f (MList m) = MList (m >>= concatMapM f)
concatMapM f End       = return End
concatMapM f (h :~ t)  = getML $ f h `mconc` concatMapMr f t

singleMR a = return (a :~ mzero)
singleM = MList . singleMR

instance Monad m => MonadPlus (MList m) where
  mzero = MList (return End)
  mplus = mconc

instance Monad m => Monad (MList m) where
  return = singleM
  (>>=)  = flip concatMapMr

instance MonadTrans MList where
  lift m = MList $ m >>= singleMR


instance Monad m => MonadLogic (MList m) where
  msplit m = lift $ getOne m

  -- this fixes a bug in the standard 
  -- impl....
  once m = do x <- msplit m
              case x of
                Just (a,_) -> return a
                Nothing    -> mzero




getOne m = getML m >>= \x -> case x of 
   End    -> return Nothing
   h :~ t -> return (Just (h,t))
-}
