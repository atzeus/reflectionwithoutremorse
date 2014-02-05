{-# LANGUAGE GADTs, ViewPatterns, TypeOperators #-}

module Data.RTQueue where

{- Queue with worst case O(1) operations!
   Based on Okasaki Simple and Efficient Purely Functional Queues and Deques
   Journal of Functional Programming
-}

import Data.TConsList
import Data.TSnocList
import Data.Interface.TSequence

revAppend l r = rotate l r CNil
-- precondtion : |a| = |f| - (|r| - 1)
-- postcondition: |a| = |f| - |r|
rotate :: TConsList tc a b -> TSnocList tc b c -> TConsList tc c d -> TConsList tc a d
rotate CNil  (SNil `Snoc` y) r = y `Cons` r
rotate (x `Cons` f) (r `Snoc` y) a = x `Cons` rotate f r (y `Cons` a)
rotate f        a     r  = error "Invariant |a| = |f| - (|r| - 1) broken"

data RTQueue tc a b where
  RQ :: !(TConsList tc a b) -> !(TSnocList tc b c) -> !(TConsList tc x b) -> RTQueue tc a c

queue :: TConsList tc a b -> TSnocList tc b c -> TConsList tc x b -> RTQueue tc a c
queue f r CNil = let f' = revAppend f r 
                 in RQ f' SNil f'
queue f r (h `Cons` t) = RQ f r t

instance TSequence RTQueue where
 tempty = RQ CNil SNil CNil
 tsingleton x = let c = tsingleton x in queue c SNil c
 (RQ f r a) |> x = queue f (r `Snoc` x) a

 tviewl (RQ CNil SNil CNil) = TEmptyL
 tviewl (RQ (h `Cons` t) f a) = h :| queue t f a

