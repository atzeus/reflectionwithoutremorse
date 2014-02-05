{-# LANGUAGE GADTs #-}

module Data.CTQueue where


import Data.TSequence

-- Author : Atze van der Ploeg
-- A purely functional catenable queue representation with
-- that turns takes a purely functional queue and turns in it into
-- a catenable queue, i.e. with the same complexity for (><) as for (|>)
-- Based on Purely functional data structures by Chris Okasaki 
-- section 7.2: Catenable lists

data CTQueue q c x y where
  C0 :: CTQueue q c x x
  CN :: c x y -> q (CTQueue q c) y z -> CTQueue q c x z

instance TSequence q => TSequence (CTQueue q) where
 tempty       = C0
 tsingleton a = CN a tempty
 C0        >< ys  = ys
 xs        >< C0  = xs
 (CN x q)  >< ys   = CN x (q |> ys)

 tviewl C0             = TEmptyL
 tviewl (CN h t)  = h :| linkAll t
   where 
    linkAll :: TSequence q =>  q (CTQueue q c) a b -> CTQueue q c a b
    linkAll v = case tviewl v of
     TEmptyL      -> C0
     CN x q :| t  -> CN x (q `snoc` linkAll t)
    snoc q C0  = q
    snoc q r   = q |> r
