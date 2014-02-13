module BeforeFix.Tree where



data Tree    = Node Tree Tree
             | Leaf
          
(<.|) :: Tree -> Tree -> Tree
Leaf        <.| y  = y
(Node l r)  <.| y  = Node (l <.| y) (r <.| y) 
