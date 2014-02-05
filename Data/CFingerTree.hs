{-# LANGUAGE GADTs, CPP #-}

{- Type safe, Type aligned sequences by Atze van der Ploeg
   Based on:

    "Finger trees: a simple general-purpose data structure"
     Ralf Hinze and Ross Paterson. in Journal of Functional Programming16:2 (2006), pages 197-217.
   
-}


module Data.ZSeq (ZSeq, singleton,(<|), (|>), ViewL(..), ViewR(..), empty, viewl, viewr, (><)) where


data ZSeq r a b where
  Empty  :: ZSeq r a a
  Single :: r a b -> ZSeq r a b
  Deep   :: !(Digit r a b) -> ZSeq (Node r) b c -> !(Digit r c d) -> ZSeq r a d


data Node r a b where
  Node2 :: r a b -> r b c -> Node r a c
  Node3 :: r a b -> r b c -> r c d -> Node r a d

data Digit r a b where
  One   :: r a b -> Digit r a b
  Two   :: r a b -> r b c -> Digit r a c
  Three :: r a b -> r b c -> r c d -> Digit r a d
  Four  :: r a b -> r b c -> r c d -> r d e -> Digit r a e


toTree :: Digit r a b -> ZSeq r a b
toTree (One a)         = Single a
toTree (Two a b)       = Deep (One a) Empty (One b)
toTree (Three a b c)   = Deep (Two a b) Empty (One c)
toTree (Four a b c d)  = Deep (Two a b) Empty (Two c d)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE toTree #-}
#endif

appendd :: Digit r a b -> Digit r b c -> Digit r a c
appendd (One a)        (One b)        = Two a b
appendd (One a)        (Two b c)      = Three a b c
appendd (Two a b)      (One c)        = Three a b c
appendd (One a)        (Three b c d)  = Four a b c d
appendd (Two a b)      (Two c d)      = Four a b c d
appendd (Three a b c)  (One d)        = Four a b c d

#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE appendd #-}
#endif
infixr 5 <|

(<|) :: r a b -> ZSeq r b c -> ZSeq r a c
a <| Empty                     = Single a
a <| Single b                  = Deep (One a) Empty (One b) 
a <| Deep (Four b c d e) m sf  = Deep (Two a b) (Node3 c d e <| m) sf
a <| Deep pr m sf              = Deep (appendd (One a) pr) m sf


infixr 5 |>

(|>) :: ZSeq r a b -> r b c -> ZSeq r a c
Empty                     |> a = Single a
Single b                  |> a = Deep (One b) Empty (One a)
Deep pr m (Four e d c b)  |> a = Deep pr (m |> Node3 e d c) (Two b a)
Deep pr m sf              |> a = Deep pr m (appendd sf (One a))
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE (|>) #-}
#endif

empty :: ZSeq r a a
empty = Empty
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE empty #-}
#endif

singleton :: r a b -> ZSeq r a b
singleton a = Single a
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE singleton #-}
#endif


infixr 5 ::: 


data ZList r a b where
  ZNil :: ZList r a a
  (:::) :: r a b -> ZList r b c -> ZList r a c

toList (One a) = a ::: ZNil 
toList (Two a b) = a ::: b ::: ZNil
toList (Three a b c) = a ::: b ::: c ::: ZNil
toList (Four a b c d) = a ::: b ::: c ::: d ::: ZNil
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE toList #-}
#endif



fromList :: ZList r a b -> Digit r a b
fromList (a ::: ZNil) = One a
fromList (a ::: b ::: ZNil) = Two a b
fromList (a ::: b ::: c ::: ZNil) = Three a b c
fromList (a ::: b ::: c ::: d ::: ZNil) = Four a b c d
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromList #-}
#endif

append :: ZList r a b -> ZList r b c -> ZList r a c
append ZNil t = t
append (h ::: t) r = h ::: append t r

data ViewL r a b where
  EmptyL :: ViewL r a a
  ConsL  :: r a b -> ZSeq r b c -> ViewL r a c

viewl :: ZSeq r a c -> ViewL r a c
viewl Empty = EmptyL
viewl (Single a) = ConsL a Empty
viewl (Deep pr m sf) = case toList pr of
            h ::: t -> ConsL h (deepl t m sf)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE viewl #-}
#endif

deepl :: ZList r a b -> ZSeq (Node r) b c -> Digit r c d -> ZSeq r a d
deepl ZNil m sf = case viewl m of
           EmptyL -> toTree sf
           ConsL a m' -> Deep (nodeToDigit a) m' sf 
deepl pr m sf = Deep (fromList pr) m sf

infixr 5 :::< 

data ZListR r a b where
  ZNilR :: ZListR r a a
  (:::<) :: r b c -> ZListR r a b -> ZListR r a c

toListR :: Digit r a b -> ZListR r a b
toListR (One a) = a :::< ZNilR
toListR (Two a b) = b :::< a :::< ZNilR
toListR (Three a b c) = c :::< b :::< a :::< ZNilR
toListR (Four a b c d) = d:::< c :::< b :::< a :::< ZNilR
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE toListR #-}
#endif


fromListR :: ZListR r a b -> Digit r a b
fromListR (a :::< ZNilR) = One a
fromListR (b :::< a :::< ZNilR) = Two a b
fromListR (c :::< b :::< a :::< ZNilR) = Three a b c
fromListR (d :::< c :::< b :::< a :::< ZNilR) = Four a b c d
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE fromListR #-}
#endif

data ViewR r a b where
  EmptyR :: ViewR r a a
  ConsR  ::  ZSeq r a b -> r b c -> ViewR r a c

rev = toList . fromListR 
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE rev #-}
#endif

viewr :: ZSeq r a c -> ViewR r a c
viewr Empty = EmptyR
viewr (Single a) = ConsR Empty a 
viewr (Deep pr m sf) = case (toListR sf) of
            h :::< t -> ConsR (deepr pr m t) h
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE viewr #-}
#endif

deepr :: Digit r a b -> ZSeq (Node r) b c -> ZListR r c d -> ZSeq r a d
deepr pr m ZNilR = case viewr m of
           EmptyR -> toTree pr
           ConsR m' a -> Deep pr m' (nodeToDigit a)
deepr pr m sf = Deep pr m (fromListR sf)
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE deepr #-}
#endif

nodeToDigit :: Node r a b -> Digit r a b
nodeToDigit (Node2 a b) = Two a b
nodeToDigit (Node3 a b c) = Three a b c
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE nodeToDigit #-}
#endif


addAlll :: ZList r a b -> ZSeq r b c -> ZSeq r a c
addAlll ZNil m = m
addAlll (h ::: t) m = h <| addAlll t m

addAllr :: ZSeq r a b -> ZList r b c  -> ZSeq r a c
addAllr m ZNil  = m
addAllr m (h ::: t) = addAllr (m |> h) t

  

app3 :: ZSeq r a b -> ZList r b c -> ZSeq r c d -> ZSeq r a d
app3 Empty ts xs = addAlll ts xs
app3 xs ts Empty = addAllr xs ts
app3 (Single x) ts xs = x <| (addAlll ts xs)
app3 xs ts (Single x) = (addAllr xs ts) |> x
app3 (Deep pr1 m1 sf1) ts (Deep pr2 m2 sf2) =
    Deep pr1 
        (app3 m1 (nodes (append (toList sf1) (append ts (toList pr2)))) m2) sf2
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE app3 #-}
#endif

nodes :: ZList r a b -> ZList (Node r) a b
nodes (a ::: b ::: ZNil) = Node2 a b ::: ZNil
nodes (a ::: b ::: c ::: ZNil) = Node3 a b c ::: ZNil
nodes (a ::: b ::: c ::: d ::: ZNil) = Node2 a b ::: Node2 c d ::: ZNil
nodes (a ::: b ::: c ::: xs) = Node3 a b c ::: nodes xs

#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE nodes #-}
#endif

(><) :: ZSeq r a b -> ZSeq r b c -> ZSeq r a c
xs >< ys = app3 xs ZNil ys

#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE (><) #-}
#endif
