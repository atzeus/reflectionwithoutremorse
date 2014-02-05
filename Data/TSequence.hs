{-# LANGUAGE GADTs,TypeSynonymInstances,FlexibleInstances #-}

module Data.TSequence.Class(TSequence(..), TViewL(..), TViewR(..)) where

import Data.Sequence.Class
import Control.Category
import Prelude hiding ((.),id)
infixr 5 <|
infixl 5 |>
infix 5 ><
class TSequence s where
  -- minimal complete def: tempty, tsingleton, (tviewl or tviewr) and (><) or (|>) or (<|)
  tempty     :: s c x x
  tsingleton :: c x y -> s c x y
  (><)       :: s c x y -> s c y z  -> s c x z
  tviewl     :: s c x y -> TViewL s c x y
  tviewr     :: s c x y -> TViewR s c x y
  (|>)       :: s c x y -> c y z -> s c x z
  (<|)       :: c x y -> s c y z -> s c x z
  
  l |> r = l >< tsingleton r
  l <| r = tsingleton l >< r
  l >< r = case tviewl l of
    TEmptyL -> r
    h :| t  -> h <| (t >< r)

  tviewl q = case tviewr q of 
    TEmptyR -> TEmptyL
    p :|< l -> case tviewl p of
        TEmptyL -> l :| tempty
        h :| t ->  h :| (t |> l)

  tviewr q = case tviewl q of 
    TEmptyL -> TEmptyR
    h :| t -> case tviewr t of
        TEmptyR -> tempty :|< h
        p :|< l ->  (h <| p) :|< l


data TViewL s c x y where
   TEmptyL  :: TViewL s c x x
   (:|)     :: c x y -> s c y z -> TViewL s c x z

data TViewR s c x y where
   TEmptyR  :: TViewR s c x x
   (:|<)     :: s c x y -> c y z -> TViewR s c x z



instance TSequence s => Category (s c) where
  id = tempty
  (.) = flip (><) -- not (><): type error
