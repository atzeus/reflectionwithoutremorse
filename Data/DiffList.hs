module Data.DiffList where

import Data.Interface.Sequence

newtype DL a = DL { unDL :: [a] -> [a]}

instance Sequence DL where
  empty = DL id
  singleton a = DL $ \t -> a:t
  (DL l) .>< (DL r) = DL (l . r)
  viewl (DL l) = case l [] of
     [] -> EmptyL
     (h:t) -> h :< reflect t

reflect l = DL $ \x -> 
  let ref [] = x
      ref (h:t) = h : ref t
  in ref l
