{-# LANGUAGE ExistentialQuantification,GADTs #-}

module Fixed.FreeMonad(FreeMonadView(..),FreeMonad, fromView,toView) where

import Data.Interface.TSequence
import Data.FastTCQueue
type TCQueue = FastTCQueue

newtype FC f a b = FC (a -> FreeMonad f b)
type FMExp f a b = TCQueue (FC f) a b
data FreeMonad f a = 
   forall x. FM (FreeMonadView f x) (FMExp f x a)
data FreeMonadView f a 	= Pure a 
                        | Impure (f (FreeMonad f a))
fromView x = FM x tempty

toView :: Functor f => FreeMonad f a -> FreeMonadView f a
toView (FM h t) = case h of
   Pure x -> 
    case tviewl t of
       TEmptyL -> Pure x
       FC hc :| tc -> toView (hc x >>>= tc)
   Impure f -> Impure (fmap (>>>= t) f) 
 where (>>>=) :: FreeMonad f a -> FMExp f a b -> FreeMonad f b 
       (FM h t) >>>= r = FM h (t >< r)

instance Monad (FreeMonad f) where
  return = fromView . Pure
  (FM m r) >>= f = FM m (r >< tsingleton (FC f))


