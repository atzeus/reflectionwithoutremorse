{-# LANGUAGE KindSignatures, TypeOperators, TypeSynonymInstances,FlexibleInstances, NoMonomorphismRestriction #-}

module LogicTBad where

import Data.Monoid
-- import CQueue
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logic.Class
import Prelude hiding (foldl,concatMap)


data ListF a s = Nil | a :~ s
newtype Fix f = Fix (f (Fix f))

(+++) :: Wrap f => f (ListF a) -> f (ListF a) -> f (ListF a)
l +++ r = case unwrap l of
  Nil ->  r
  h :~ t -> wrap (h :~ (t +++ r))

concatMap :: Wrap f => (a -> f (ListF b)) -> f (ListF a) -> f (ListF b) 
concatMap f m = case unwrap m of
  Nil -> wrap Nil
  (h :~ t) -> f h +++ concatMap f t

class Wrap f where
  wrap :: g (f g) -> f (g f)
  unwrap :: f (g f) -> g (f g) 

instance Wrap Fix where
  wrap = Fix
  unwrap (Fix f) = f

newtype (f :. g) x = Comp (f (g x))


