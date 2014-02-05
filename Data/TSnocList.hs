{-# LANGUAGE GADTs #-}

module Data.TSnocList where


import Data.Interface.TSequence

data TSnocList c x y where
  SNil :: TSnocList c x x
  Snoc :: TSnocList c x y -> c y z -> TSnocList c x z

instance TSequence TSnocList where
  tempty = SNil
  tsingleton c = Snoc SNil c 
  (|>) = Snoc
  tviewr SNil = TEmptyR
  tviewr (Snoc p l) = p :|< l
