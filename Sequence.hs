{-# LANGUAGE GADTs,FlexibleInstances #-}

module Sequence where

import Data.Monoid

class Sequence s where
  empty :: s a
  singleton :: a -> s a
  (.><)      :: s a -> s a -> s a
  viewl     :: s a -> ViewL s a

data ViewL s a where
  EmptyL :: ViewL s a
  (:<)   :: a -> s a -> ViewL s a

instance Sequence s => Monoid (s a) where
  mempty = empty
  mappend = (.><) -- or flip (.><)
