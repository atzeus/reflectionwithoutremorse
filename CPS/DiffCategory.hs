{-# LANGUAGE TypeSynonymInstances,Rank2Types,FlexibleInstances,UndecidableInstances #-}
module DiffCategory where
import Control.Category
import Prelude hiding ((.),id)

newtype DiffCategory c a b = DiffCategory {getDC :: forall x. c x a -> c x b} 

abs :: Category c => DiffCategory c a b -> c a b
abs (DiffCategory f) = f id

rep :: Category c => c a b -> DiffCategory c a b
rep f = DiffCategory (f .)

instance Category m => Category (DiffCategory m) where
  id = rep id
  (DiffCategory f) . (DiffCategory g)  = DiffCategory (f . g)
