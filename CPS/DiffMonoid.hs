{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
module DiffMonoid where

import Data.Monoid

type DiffMonoid a = a -> a

abs :: Monoid a => DiffMonoid a -> a
abs a = a mempty

rep :: Monoid a => a -> DiffMonoid a
rep = mappend

instance Monoid a => Monoid (DiffMonoid a) where
  mempty = rep mempty
  mappend  = (.)
