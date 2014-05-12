{-# LANGUAGE ExistentialQuantification,GADTs #-}
module Fixed.GenericTree(TreeView(..), TreeD, fromView, (<.|), toView) where

import Data.Interface.TSequence
import Data.FastTCQueue
type TCQueue = FastTCQueue

newtype TreeCont a b = TC (a -> TreeD b)
type TreeCExp a b = TCQueue TreeCont a b


data TreeView a  = Node (TreeD a) (TreeD a)
                 | Leaf a
          
data TreeD a = forall x. TreeD (TreeView x) (TreeCExp x a)

fromView :: TreeView a -> TreeD a
fromView x = TreeD x tempty

(<.|) :: TreeD a -> (a -> TreeD b) -> TreeD b
(TreeD x s) <.| f = TreeD x (s >< tsingleton (TC f))

toView :: TreeD a -> TreeView a
toView (TreeD x s) = case x of
  Leaf a -> case tviewl s of 
             TEmptyL -> Leaf a
             TC h :| t  -> toView $ (h a) <.|| t
  Node l r -> Node (l <.|| s) (r <.|| s)
  where (<.||) :: TreeD a -> TreeCExp a b -> TreeD b
        (TreeD x l) <.|| r = TreeD x (l >< r)

