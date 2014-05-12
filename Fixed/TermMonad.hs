{-# LANGUAGE ExistentialQuantification,GADTs #-}
module Fixed.TermMonad(TermM,TermMView(..), fromView, toView) where

import Data.Interface.TSequence
import Data.FastTCQueue
type TCQueue = FastTCQueue

newtype TermMCont r a b = TC (a -> TermM r b)
type TermCExp r a b = TCQueue (TermMCont r) a b

data TermMView r a where
  Bind   :: r w -> (w -> TermM r a) -> TermMView r a
  Return :: a -> TermMView r a

data TermM r a = forall x. TermM (TermMView r x) (TermCExp r x a)

fromView :: TermMView r a -> TermM r a
fromView r = TermM r tempty

toView :: TermM r a -> TermMView r a
toView (TermM x s) = case x of
  Return a -> case tviewl s of 
             TEmptyL -> Return a
             TC h :| t  -> toView $ (h a) <.|| t
  Bind t f -> Bind t (\x -> f x <.|| s) 
  where (<.||) :: TermM r a -> TermCExp r a b -> TermM r b
        (TermM x l) <.|| r = TermM x (l >< r)

instance Monad (TermM r) where
  return = fromView . Return
  (TermM t s) >>= f = TermM t (s |> TC f)
