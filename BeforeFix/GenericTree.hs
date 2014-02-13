module BeforeFix.GenericTree where

data Tree a  = Node (Tree a) (Tree a)
             | Leaf a
          
(<.|) :: Tree a -> (a -> Tree b) -> Tree b
Leaf a      <.| f  = f a
(Node l r)  <.| f  = Node (l <.|f) (r <.| f) 
