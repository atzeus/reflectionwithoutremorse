{-# LANGUAGE GADTs,FlexibleInstances,UndecidableInstances #-}


module Data.LowerTSequence where

import Data.Sequence
import Data.TSequence
import Data.Foldable
import Data.Traversable
import Control.Applicative hiding (empty)
import Data.Monoid

data MonoidMorphism a b c where 
   MArr :: !a -> MonoidMorphism a () ()

newtype MSeq s a = MSeq { getMS :: s (MonoidMorphism a) () () }

instance TSequence s => Sequence (MSeq s) where
  empty      = MSeq tempty
  singleton  = MSeq . tsingleton . MArr
  l .>< r    = MSeq $ getMS l >< getMS r
  l .|> x    = MSeq $ getMS l |> MArr x
  x .<| r    = MSeq $ MArr x <| getMS r
  viewl  s   = case tviewl (getMS s) of
     TEmptyL      -> EmptyL
     MArr h :| t -> h :< MSeq t
  viewr  s   = case tviewr (getMS s) of
     TEmptyR      -> EmptyR
     p :|< MArr l -> MSeq p :> l


instance TSequence s => Functor (MSeq s) where
  fmap f = map where
   map q = case viewl q of
     EmptyL -> empty
     h :< t -> f h .<| map t 

instance TSequence s => Foldable (MSeq s) where
  foldMap f = fm where
   fm q = case viewl q of
     EmptyL -> mempty
     h :< t -> f h `mappend` fm t 


instance TSequence s => Traversable (MSeq s) where
  sequenceA q = case viewl q of
     EmptyL -> pure empty
     h :< t -> pure (.<|) <*> h <*> sequenceA t


