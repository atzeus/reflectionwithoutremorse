{-# LANGUAGE GADTs,TypeSynonymInstances,FlexibleInstances #-}

module TSequenceClass(TSequence(..), TViewL(..), MSeq) where

import Sequence
import Control.Category
import Prelude hiding ((.),id)

class TSequence s where
  tempty     :: s c x x
  tsingleton :: c x y -> s c x y
  (><)       :: s c x y -> s c y z  -> s c x z
  tviewl     :: s c x y -> TViewL s c x y

data TViewL s c x y where
   TEmptyL  :: TViewL s c x x
   (:|)     :: c x y -> s c y z -> TViewL s c x z

data MonoidMorphism a b c where 
   MArr :: a -> MonoidMorphism a () ()

newtype MSeq s a = MSeq { getMS :: s (MonoidMorphism a) () () }

instance TSequence s => Sequence (MSeq s) where
  empty      = MSeq tempty
  singleton  = MSeq . tsingleton . MArr
  l .>< r    = MSeq $ getMS l >< getMS r
  viewl  s   = case tviewl (getMS s) of
     TEmptyL      -> EmptyL
     MArr h :| t -> h :< MSeq t

instance TSequence s => Category (s c) where
  id = tempty
  (.) = flip (><) -- not (><): type error
