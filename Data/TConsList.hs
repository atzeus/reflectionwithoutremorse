{-# LANGUAGE GADTs #-}

module Data.TConsList where

import Data.TSequence

data TConsList c x y where
  CNil :: TConsList c x x
  Cons :: c x y -> TConsList c y z -> TConsList c x z

instance TSequence TConsList where
  tempty = CNil
  tsingleton c = Cons c CNil
  (<|) = Cons
  tviewl CNil = TEmptyL
  tviewl (Cons h t) = h :| t
