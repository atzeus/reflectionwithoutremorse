{-# LANGUAGE OverlappingInstances,GADTs, FlexibleInstances, UndecidableInstances, NoMonomorphismRestriction #-}
module ExplicitExpr.PCategory(module Data.FastTCQueue, PCategory(..), CExp, val,expr) where
import Data.FastTCQueue
import Data.CTQueue
import Control.Category
import Prelude hiding ((.),id)

type CExp c a b = FastTCQueue c a b

class PCategory c where
  id'  :: c x x 
  (.>>>) :: c x y -> CExp c y z -> c x z


val :: PCategory c => CExp c a b -> c a b
val q = case tviewl q of
  TEmptyL -> id'
  h :| t -> h .>>> t


expr :: c a b -> CExp c a b
expr =  tsingleton 


instance PCategory c => Category c where
  id = id'
  f . g = g .>>> expr f
