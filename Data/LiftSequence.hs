{-# LANGUAGE ScopedTypeVariables,KindSignatures,GADTs, CPP #-}
module LiftSequence(module Data.TSequence, LiftSeq) where

-- Author : Atze van der Ploeg
-- Lifts any sequence to a type-aligned sequence by coercion
-- Only safe iff the sequence does what we expect and doesn't 
-- reorganize the ordering (for example due to a bug..)
-- Use at your own risk!

import Data.TSequence 
import Data.Sequence 
import Unsafe.Coerce

data Any c where Any :: c a b -> Any c

newtype LiftSeq s c a b = LiftSeq (s (Any c))

instance Sequence s => TSequence (LiftSeq s) where
  tempty = LiftSeq empty

  tsingleton x = LiftSeq (singleton (Any x))

  (LiftSeq l) |> x = LiftSeq (l .|> Any x)

  x <| (LiftSeq r) = LiftSeq (Any x .<| r)

  (LiftSeq l) >< (LiftSeq r) = LiftSeq (l .>< r)

  tviewl (LiftSeq l) = case viewl l of
    EmptyL -> unsafeCoerce TEmptyL
    h :< t -> unsafeCoerce h :| LiftSeq t

  tviewr (LiftSeq l) = case viewr l of
    EmptyR -> unsafeCoerce TEmptyR
    p :> l -> LiftSeq p :|< unsafeCoerce l
  
  


