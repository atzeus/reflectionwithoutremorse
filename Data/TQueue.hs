{-# LANGUAGE GADTs, DataKinds, TypeOperators #-}

module Data.CTQueue(module Data.TSequence,Q)  where
-- Author : Atze van der Ploeg
-- a simplified version of Okasaki's implicit recursive
-- slowdown queues. 
-- See purely functional data structures by Chris Okasaki 
-- section 8.4: Queues based on implicit recursive slowdown

-- A very simple type aligned, purely functional queue with 
-- amortized O(1) operations

import Data.TSequence

data P c a b where
  (:*) :: c a w -> c w b -> P c a b

data B c a b where
  B1 :: c a b    -> B c a b
  B2 :: !(P c a b)  -> B c a b

data Q c a b where
  Q0 :: Q c a a
  Q1 :: c a b -> Q c a b
  QN :: !(B c a x) -> Q (P c) x y -> !(B c y b) -> Q c a b

instance TSequence Q where
  tempty = Q0
  tsingleton = Q1 
  q |> b = case q of
    Q0             -> Q1 b
    Q1 a           -> QN (B1 a) Q0 (B1 b)
    QN l m (B1 a)  -> QN l m (B2 (a :* b)) 
    QN l m (B2 r)  -> QN l (m |> r) (B1 b)

  tviewl q = case q of
    Q0                    -> TEmptyL
    Q1 a                  -> a :| Q0
    QN (B2 (a :* b)) m r  -> a :| QN (B1 b) m r
    QN (B1 a) m r         -> a :| shiftLeft m r
    where  shiftLeft :: Q (P c) a w -> B c w b -> Q c a b
           shiftLeft q r = case tviewl q of
               TEmptyL -> buf2queue r
               l :| m -> QN (B2 l) m r
           buf2queue (B1 a)        = Q1 a
           buf2queue(B2 (a :* b))  = QN (B1 a) Q0 (B1 b)
  
             
