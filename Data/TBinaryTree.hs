{-# LANGUAGE GADTs #-}

module Data.TBinaryTree where


import Data.Interface.TSequence

data TBinTree c x y where
  Empty :: TBinTree c x x
  Leaf  :: c x y -> TBinTree c x y
  Node  :: TBinTree c x y -> TBinTree c y z -> TBinTree c x z

instance TSequence TBinTree where
  tempty = Empty
  tsingleton c = Leaf c 
  (><) = Node
  tviewl Empty = TEmptyL
  tviewl (Leaf c) = c :| Empty
  tviewl (Node (Node l m) r) = tviewl (Node l (Node m r))
  tviewl (Node (Leaf c) r)   = c :| r
  tviewl (Node Empty r)      = tviewl r
                        
