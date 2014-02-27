module Data.SeqList where

import Data.Interface.Sequence


instance Sequence [] where
  empty = []
  singleton a = a:[]
  (.<|) = (:)
  (.><) = (++)
  viewl []    = EmptyL
  viewl (h:t) =  h :< t

