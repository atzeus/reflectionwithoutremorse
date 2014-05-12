module Fixed.Tree(TreeView(..),Tree, (<.|), fromView,toView) where

import Data.FastCQueue

type CQueue = FastCQueue

data TreeView = Node TreeD TreeD
              | Leaf

newtype TreeD = TreeD (CQueue TreeView)

fromView :: TreeView -> TreeD
fromView x = TreeD $ singleton x

(<.|) :: TreeD -> TreeD -> TreeD
(TreeD l) <.| (TreeD r) = TreeD $ l .>< r

toView :: TreeD -> TreeView
toView (TreeD s) = case viewl s of
  EmptyL -> Leaf
  h :< t -> case h of
       Leaf     -> toView (TreeD t)
       Node l r -> Node (l <.|| t) (r <.|| t)
  where (<.||) :: TreeD -> CQueue TreeView -> TreeD
        (TreeD l) <.|| r = TreeD (l .>< r)

