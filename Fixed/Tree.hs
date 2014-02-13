module Fixed.Tree where

import Data.FastCQueue


type TreeExp = FastCQueue Tree

data Tree    = Node TreeExp TreeExp
             | Leaf
          
(<.|) :: Tree -> TreeExp -> Tree
Leaf        <.| y  = val y
(Node l r)  <.| y  = Node (l .>< y) (r .>< y) 

val :: TreeExp -> Tree
val s = case viewl s of
  EmptyL -> Leaf
  h :< t -> h <.| t

expr :: Tree -> TreeExp
expr = singleton

(<.||) :: Tree -> Tree -> Tree
l <.|| r = l <.| expr r
