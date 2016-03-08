{-# LANGUAGE ScopedTypeVariables, MagicHash,KindSignatures,GADTs, PolyKinds, CPP #-}
module Data.LiftSequence(module Data.Interface.TSequence, LiftSeq) where

-- Author : Atze van der Ploeg
-- Lifts any sequence to a type-aligned sequence by coercion
-- Only safe iff the sequence does what we expect and doesn't
-- reorganize the ordering (for example due to a bug..)
-- Use at your own risk!

import Data.Interface.TSequence
import Data.Interface.Sequence
import GHC.Prim

newtype LiftSeq s (c :: k -> k -> *) (a :: k) (b :: k) = LiftSeq (s (Any c))

instance Sequence s => TSequence (LiftSeq s) where
  tempty = LiftSeq empty

  tsingleton x = LiftSeq (singleton (unsafeCoerce# x))

  (LiftSeq l) |> x = LiftSeq (l .|> unsafeCoerce# x)

  x <| (LiftSeq r) = LiftSeq (unsafeCoerce# x .<| r)

  (LiftSeq l) >< (LiftSeq r) = LiftSeq (l .>< r)

  tviewl (LiftSeq l) = case viewl l of
    EmptyL -> unsafeCoerce# TEmptyL
    h :< t -> unsafeCoerce# h :| LiftSeq t

  tviewr (LiftSeq l) = case viewr l of
    EmptyR -> unsafeCoerce# TEmptyR
    p :> l -> LiftSeq p :|< unsafeCoerce# l
