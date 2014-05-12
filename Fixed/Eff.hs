{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns,NoMonomorphismRestriction,TypeSynonymInstances #-}

module Fixed.Eff where

import Control.Monad
import Data.Typeable
import Data.OpenUnion1
import Fixed.FreeMonad

-- Fixed version of the extensible effects
-- framework at http://okmij.org/ftp/Haskell/extensible/Eff.hs


type Eff r a = FreeMonad (Union r) a

instance Functor f => Functor (FreeMonad f) where
  fmap = liftM 



-- send a request and wait for a reply
send :: Union r a -> Eff r a
send u = fromView $ Impure (fmap (fromView . Pure) u)



-- ------------------------------------------------------------------------
-- The initial case, no effects

data Void -- no constructors

-- The type of run ensures that all effects must be handled:
-- only pure computations may be run.
run :: Eff Void w -> w
run (toView -> Pure a) = a

-- A convenient pattern: given a request (open union), either
-- handle it or relay it.
handle_relay :: Typeable1 t =>
     Union (t :> r) v -> (v -> Eff r a) -> (t v -> Eff r a) -> Eff r a
handle_relay u loop h = case decomp u of
  Right x -> h x
  Left u  -> send u >>= loop

-- Add something like Control.Exception.catches? It could be useful
-- for control with cut.

interpose :: (Typeable1 t, Functor t, Member t r) =>
     Union r v -> (v -> Eff r a) -> (t v -> Eff r a) -> Eff r a
interpose u loop h = case prj u of
  Just x -> h x
  _       -> send u >>= loop

--- ------------------------------------------------------------------------
-- State, strict

data State s w = State (s->s) (s -> w)
  deriving (Typeable, Functor) 

-- The signature is inferred
put :: (Typeable s, Member (State s) r) => s -> Eff r ()
put s = send (inj (State (const s) (const ())))

-- The signature is inferred
get :: (Typeable s, Member (State s) r) => Eff r s
get =  send (inj (State id id))

runState :: Typeable s => Eff (State s :> r) w -> s -> Eff r (w,s)
runState m s = loop s m where
 loop s (toView -> Pure x) = return (x,s)
 loop s (toView -> Impure u) = handle_relay u (loop s) $
                       \(State t k) -> let s' = t s in s' `seq` loop s' (k s')

-- ------------------------------------------------------------------------
-- Non-determinism (choice)

-- choose lst non-deterministically chooses one value from the lst
-- choose [] thus corresponds to failure
data Choose v = forall a. Choose [a] (a -> v)
              deriving (Typeable)

instance Functor Choose where
    fmap f (Choose lst k) = Choose lst (f . k)

choose :: Member Choose r => [a] -> Eff r a
choose lst = send (inj $ Choose lst id)

-- MonadPlus-like operators are expressible via choose

mzero' :: Member Choose r => Eff r a
mzero' = choose []
mplus' m1 m2 = choose [m1,m2] >>= id


-- The interpreter
makeChoice :: forall a r. Eff (Choose :> r) a -> Eff r [a]
makeChoice = loop 
 where
 loop (toView -> Pure x)   = return [x]
 loop (toView -> Impure u) = handle_relay u loop (\(Choose lst k) -> handle lst k)
 -- Need the signature since local bindings aren't polymorphic any more
 handle :: [t] -> (t -> Eff (Choose :> r) a) -> Eff r [a]
 handle [] _  = return []
 handle [x] k = loop (k x)
 handle lst k = fmap concat $ mapM (loop . k) lst
 


-- ------------------------------------------------------------------------
-- Soft-cut: non-deterministic if-then-else, aka Prolog's *->
-- Declaratively,
--    ifte t th el = (t >>= th) `mplus` ((not t) >> el)
-- However, t is evaluated only once. In other words, ifte t th el
-- is equivalent to t >>= th if t has at least one solution.
-- If t fails, ifte t th el is the same as el.

ifte :: forall r a b.
        Member Choose r => Eff r a -> (a -> Eff r b) -> Eff r b -> Eff r b
ifte t th el = loop [] t
 where 
 loop [] (toView -> Pure x)  = th x
 -- add all other latent choices of t to th x
 -- this is like reflection of t
 loop jq (toView -> Pure x)  = choose ((th x) : map (\t -> t >>= th) jq) >>= id 
 loop jq (toView -> Impure u)    = interpose u (loop jq) (\(Choose lst k) -> handle jq lst k)
 -- Need the signature since local bindings aren't polymorphic any more
 handle :: [Eff r a] -> [t] -> (t -> Eff r a) -> Eff r b
 handle [] [] _     = el                    -- no more choices left
 handle (j:jq) [] _ = loop jq j
 handle jq [x] k    = loop jq (k x)
 handle jq (x:rest) k = loop (map k rest ++ jq) (k x) -- DFS

guard' :: Member Choose r => Bool -> Eff r ()
guard' True  = return ()
guard' False = mzero'


-- ------------------------------------------------------------------------
-- Co-routines
-- The interface is intentionally chosen to be the same as in transf.hs

-- The yield request: reporting the value of type e and suspending 
-- the coroutine
data Yield inn out res = Yield out (inn -> res)
    deriving (Typeable, Functor)

-- The signature is inferred
yield :: (Typeable inn, Typeable out, Member (Yield inn out) r) => out -> Eff r inn
yield x = send (inj $ Yield x id)

-- Status of a thread: done or reporting the value of the type a
-- (For simplicity, a co-routine reports a value but accepts unit)
data Y inn out r a = Done a | Y out (inn -> Eff (Yield inn out :> r) a)

-- Launch a thread and report its status
runC :: (Typeable inn, Typeable out) => Eff (Yield inn out :> r) a -> Eff r (Y inn out r a)
runC m = loop m where
 loop (toView -> Pure x) = return (Done x)
 loop (toView -> Impure u)   = case decomp u of
   Right (Yield x c)  -> return (Y x c)
   Left u             -> send u >>= loop

