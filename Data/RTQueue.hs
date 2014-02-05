{-# LANGUAGE GADTs, ViewPatterns, TypeOperators #-}

data RTQueue where

{- Queue with worst case O(1) operations!
   Based on Okasaki Simple and Efficient Purely Functional Queues and Deques
   Journal of Functional Programming
-}


data ConsList tc a b where
  CNil  :: ConsList tc a a
  (:>) :: tc a b -> ConsList tc b c -> ConsList tc a c

data SnocList tc a b where
  SNil  :: SnocList tc a a
  (:<) :: SnocList tc a b -> tc b c -> SnocList tc a c

revAppend l r = rotate l r CNil
-- precondtion : |a| = |f| - (|r| - 1)
-- postcondition: |a| = |f| - |r|
rotate :: ConsList tc a b -> SnocList tc b c -> ConsList tc c d -> ConsList tc a d
rotate CNil  (SNil :< y) r = y :> r
rotate (x :> f) (r :< y) a = x :> rotate f r (y :> a)
rotate f        a     r  = error "Invariant |a| = |f| - (|r| - 1) broken"

data Queue tc a b where
  Queue :: !(ConsList tc a b) -> !(SnocList tc b c) -> !(ConsList tc x b) -> Queue tc a c

queue :: ConsList tc a b -> SnocList tc b c -> ConsList tc x b -> Queue tc a c
queue f r CNil = let f' = revAppend f r 
                 in Queue f' SNil f'
queue f r (h :> t) = Queue f r t

empty = Queue CNil SNil CNil

snoc (Queue f r a) x = queue f (r :< x) a

view (Queue CNil SNil CNil) = Empty
view (Queue (h :> t) f a) = h ::> queue t f a


data ViewL tc a b where
  Empty :: ViewL tc a a
  (::>) :: tc a b -> Queue tc b c -> ViewL tc a c

