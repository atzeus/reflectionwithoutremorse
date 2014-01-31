{-# LANGUAGE GADTs, DataKinds, TypeOperators #-}

module CTQueue(CTQueue,  where
-- Author : Atze van der Ploeg
-- a simplified version of Okasaki's implicit recursive
-- slowdown queues. 
-- See purely functional data structures by Chris Okasaki 
-- section 8.4: Queues based on implicit recursive slowdown

-- A very simple type aligned, purely functional deque with 
-- amortized O(1) operations (viewl, <|, viewr, |>)

data TViewl s c x y where
   TEmptyL  :: TViewl s c x x
   (:|)     :: c x y -> s c y z -> TViewl s c x z

data P c a b where
  (:*) :: c a w -> c w b -> P c a b

data B c a b where
  B1 :: c a b    -> B c a b
  B2 :: P c a b  -> B c a b

data Q c a b where
  Q0 :: Q c a a
  Q1 :: c a b -> Q c a b
  QN :: B c a x -> Q (P c) x y -> B c y b -> Q c a b

(|>) :: Q c a w -> c w b -> Q c a b
q |> b = case q of
  Q0             -> Q1 b
  Q1 a           -> QN (B1 a) Q0 (B1 b)
  QN l m (B1 a)  -> QN l m (B2 (a :* b)) 
  QN l m (B2 r)  -> QN l (m |> r) (B1 b)

viewl :: Q c a b -> TViewl Q c a b
viewl q = case q of
  Q0                    -> TEmptyL
  Q1 a                  -> a :| Q0
  QN (B2 (a :* b)) m r  -> a :| QN (B1 b) m r
  QN (B1 a) m r         -> a :| shiftLeft m r
  where  shiftLeft :: Q (P c) a w -> B c w b -> Q c a b
         shiftLeft q r = case viewl q of
             TEmptyL -> buf2queue r
             l :| m -> QN (B2 l) m r
         buf2queue (B1 a)        = Q1 a
         buf2queue(B2 (a :* b))  = QN (B1 a) Q0 (B1 b)

-- Author : Atze van der Ploeg
-- A purely functional queue representation with
-- O(1) ++/snoc/cons, and O(1) viewl.
-- Based on Purely functional data structures by Chris Okasaki 
-- section 7.2: Catenable lists

data CTQueue c a b where
  C0 :: CTQueue c a a
  CN :: NECQueue c a b -> CTQueue c a b

data NECQueue c a b where
  NE :: c a w -> Q (NECQueue c) w b -> NECQueue c a b

instance TSequence CTQueue where
 tempty       = C0
 tsingleton a = CN (NE a Q0)

 C0             >< ys       = ys
 xs             >< C0       = xs
 (CN (NE x q))  >< (CN ys)  = CN (NE x (q |> ys))

 tviewl C0             = TEmptyL
 tviewl (CN (NE h t))  = h :| linkAll t
   where 
    linkAll :: Q (NECQueue c) a b -> CTQueue c a b
    linkAll v = case viewl v of
     TEmptyL      -> C0
     NE x q :| t  -> CN (NE x (q `snoc` linkAll t)) 
    snoc q C0      = q
    snoc q (CN e)  = q |> e

