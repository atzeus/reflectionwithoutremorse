
module Data.BinaryTree where


import Data.Interface.Sequence

data BinTree where
  Empty :: BinTree a
  Leaf  :: a-> BinTree a
  Node  :: BinTree a -> BinTree a -> BinTree a

instance Sequence BinTree where
  empty = Empty
  singleton c = Leaf c 
  (.><) = Node
  viewl Empty = TEmptyL
  viewl (Leaf c) = c :< Empty
  viewl (Node (Node l m) r) = viewl (Node l (Node m r))
  viewl (Node (Leaf c) r)   = c :< r
  viewl (Node Empty r)      = tviewl r
                        
