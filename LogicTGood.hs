{-# LANGUAGE TypeSynonymInstances,FlexibleInstances, NoMonomorphismRestriction, FlexibleContexts, OverlappingInstances #-}

module LogicTGood where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logic.Class
import PMonad
import Data.Monoid
import CQueue
import Prelude hiding (foldl,concatMap)

{-
data Logic m a = Pick a
               | Lift (m (MExp (Logic m) () a))
               | Choose (CQueue (Logic m a))

instance Monad m =>PMonad (Logic m) where
  return' = Pick 
  (Pick x)   >>>= f = val f x
  (Lift m)   >>>= f = Lift ( liftM (>>> f) m )
  (Choose q) >>>= f = Choose (fmap (>>>= f) q)

instance Monad m => MonadPlus (Logic m) where
  mzero       = Choose empty
  l `mplus` r = Choose (singleton l .>< singleton r)

instance MonadTrans Logic where
  lift m = Lift (liftM (exprm . Pick) m)

instance Monad m => MonadLogic (Logic m) where
  msplit = lift . getOne

  -- this fixes a bug in the standard 
  -- impl....
  once m = do x <- msplit m
              case x of
                Just (a,_) -> return a
                Nothing    -> mzero


getOne (Pick x)   = return (Just (x,mzero))
getOne (Lift m)   = m >>= getOne . valm
getOne (Choose q) = case viewl q of
  EmptyL -> return Nothing
  h :< t -> do f <- getOne h 
               case f of
                Nothing -> getOne (Choose t)
                Just (f,ft) -> return (Just (f, ft `mplus` Choose t))
-}

type  MList  m a = m (MListR m a)
data  MListR m a = a :~ MList m a
                 | End

End      +++ r = r
(h :~ t) +++ r = return $ h :~ (t >>= (+++ r))

concatMap f End       = return End
concatMap f (h :~ t)  = f h +++ (t >>= (concatMap f))

singleton a = return (a :~ mzero)

instance Monad m => MonadPlus (MList m) where
  mzero      = return End
  mplus a b  = a >>= (+++ b)
instance Monad m => Monad (MList m) where
  return     = singleton
  m >>= f    = m >>= concatMap f
instance MonadLogic (MList m) where
  msplit m   = liftM transform m where
   transform End       = Nothing
   transform (h :~ t)  = Just (h, val t)
instance MonadTrans MList where
  lift m     = m >>= singleton
-}

{-
newtype MList  m a = MList { getML :: m (MListR m a) }
data    MListR m a = a :~ MExp (MList m) a
                   | End

mconc :: Monad m => MList m a -> MExp (MList m) a -> MList m a
(MList l) `mconc` r = MList (l >>= (`mconcr` r))
End      `mconcr` r = getML (val r)
(h :~ t) `mconcr` r = return $ h :~ (t `mappend` r)

concatMapMr :: Monad m => (a -> MList m b) -> MList m a -> MList m b
concatMapMr f (MList m) = MList (m >>= concatMapM f)
concatMapM f End       = return End
concatMapM f (h :~ t)  = getML $ f h `mplus` concatMapMr f (val t)


singleMR a = return (a :~ expr mzero)
singleM = MList . singleMR

instance Monad m => PMonadPlus (MList m) where
  mnaught = MList (return End)
  (.++) = mconc

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




getOne m = 

-}

