module CPS.DiffList where

type DiffList a = [a] -> [a]

abs :: DiffList a -> [a]
abs a = a []

rep :: [a] -> DiffList a
rep = (++)

(+++) :: DiffList a -> DiffList a -> DiffList a
(+++) = (.)

