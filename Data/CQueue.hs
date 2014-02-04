{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module CQueue(module Sequence,CQueue) where

import TSequence
import Sequence
import CTQueue

import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Applicative hiding (empty)
import Prelude hiding (foldr,foldl)

type CQueue = MSeq CTQueue


instance Functor CQueue where
  fmap f = map where
   map q = case viewl q of
     EmptyL -> empty
     h :< t -> f h `cons` map t 

instance Foldable CQueue where
  foldMap f = fm where
   fm q = case viewl q of
     EmptyL -> mempty
     h :< t -> f h `mappend` fm t 

instance Traversable CQueue where
  sequenceA q = case viewl q of
     EmptyL -> pure empty
     h :< t -> pure cons <*> h <*> sequenceA t
