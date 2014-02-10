{-# LANGUAGE GADTs #-}
module GenericTree where

import Data.Interface.TSequence
import qualified Data.CTQueue as C
import qualified Data.RTQueue as R
type TCQueue = C.CTQueue R.RTQueue

newtype TreeCont a b = TC (a -> Tree b)
type TreeCExp a b = TCQueue TreeCont a b


toCont :: Tree a -> TreeCont () a
toCont m = TC $ \() -> m

type TreeExp b = TCQueue TreeCont () b

data Tree a  = Node (TreeExp a) (TreeExp a)
             | Leaf a
          
(<.|) :: Tree a -> TreeCExp a b -> Tree b
Leaf a      <.| f  = val f a
(Node l r)  <.| f  = Node (l >< f) (r >< f) 

val :: TreeCExp a b-> (a -> Tree b)
val s = case tviewl s of
  TEmptyL -> Leaf
  TC h :| t -> \x -> h x <.| t

expr = tsingleton . TC

(<.||) :: Tree a -> (a -> Tree b) -> Tree b
l <.|| r = l <.| expr r
