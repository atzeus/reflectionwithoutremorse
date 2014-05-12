module BeforeFix.FreeMonad where


data FreeMonad f a = Pure a | Impure (f (FreeMonad f a))

instance Functor f => Monad (FreeMonad f) where
  return = Pure
  (Pure x)    >>= f = f x
  (Impure t)  >>= f = Impure (fmap (>>= f) t)

fromView = id
toView = id


