{-# LANGUAGE GADTs,FlexibleInstances,UndecidableInstances #-}

module Data.Sequence where

import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Applicative hiding (empty)
import Prelude hiding (foldr,foldl)

class Sequence s where
  empty      :: s a
  singleton  :: a -> s a
  (.><)      :: s a -> s a -> s a
  viewl      :: s a -> ViewL s a
  viewr      :: s a -> ViewR s a
  (.|>)      :: s a -> a -> s a
  (.<|)      :: a -> s a -> s a
  
  l .|> r = l .>< singleton r
  l .<| r = singleton l .>< r
  l .>< r = case viewl l of
    EmptyL -> r
    h :< t  -> h .<| (t .>< r)

  viewl q = case viewr q of 
    EmptyR -> EmptyL
    p :> l -> case viewl p of
        EmptyL -> l :< empty
        h :< t ->  h :< (t .|> l)

  viewr q = case viewl q of 
    EmptyL -> EmptyR
    h :< t -> case viewr t of
        EmptyR -> empty :> h
        p :> l ->  (h .<| p) :> l

data ViewL s a where
  EmptyL :: ViewL s a
  (:<)   :: a -> s a -> ViewL s a

data ViewR s a where
  EmptyR :: ViewR s a
  (:>)   :: s a -> a -> ViewR s a







