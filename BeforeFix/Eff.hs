{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module BeforeFix.Eff where

import Control.Monad
import Data.Typeable
import Data.OpenUnion1

-- This is a part of the code of the extensible effects
-- framework at http://okmij.org/ftp/Haskell/extensible/Eff.hs

-- Status of a coroutine (client): done with the value of type w,
-- or sending a request of type Union r
data VE w r = Val w | E !(Union r (VE w r))

-- The Eff monad (not a transformer!)
-- It is actually
--     type Eff r = forall w. Cont (VE w r)
-- We inline it into Cont to put forall under newtype;
-- it is awkward otherwise in Haskell.
-- Also, in MTL, Cont is defined via transformers. We want to
-- avoid transformers!
newtype Eff r a = Eff{runEff :: forall w. (a -> VE w r) -> VE w r}

-- standard instances for a continuation monad
instance Functor (Eff r) where
    fmap f m = Eff $ \k -> runEff m (k . f)

instance Monad (Eff r) where
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    return x = Eff $ \k -> k x
    m >>= f  = Eff $ \k -> runEff m (\v -> runEff (f v) k)

-- send a request and wait for a reply
send :: (forall w. (a -> VE w r) -> Union r (VE w r)) -> Eff r a
send f = Eff (E . f)

-- administer a client: launch a coroutine and wait for it
-- to send a request or terminate with a value
admin :: Eff r w -> VE w r
admin (Eff m) = m Val

-- The opposite of admin, occasionally useful
-- See the soft-cut for an example
-- It is currently quite inefficient. There are better ways
reflect :: VE a r -> Eff r a
reflect (Val x) = return x
reflect (E u) = Eff (\k -> E $ fmap (loop k) u)
 where
 loop :: (a -> VE w r) -> VE a r -> VE w r
 loop k (Val x) = k x
 loop k (E u)   = E $ fmap (loop k) u


-- ------------------------------------------------------------------------
-- The initial case, no effects

data Void -- no constructors

-- The type of run ensures that all effects must be handled:
-- only pure computations may be run.
run :: Eff Void w -> w
run m = case admin m of Val x -> x
-- the other case is unreachable since Void has no constructors
-- Therefore, run is a total function if m Val terminates.

-- A convenient pattern: given a request (open union), either
-- handle it or relay it.
handle_relay :: Typeable1 t =>
     Union (t :> r) v -> (v -> Eff r a) -> (t v -> Eff r a) -> Eff r a
handle_relay u loop h = case decomp u of
  Right x -> h x
  Left u  -> send (\k -> fmap k u) >>= loop
  -- perhaps more efficient:
  -- Left u  -> send (\k -> fmap (\w -> runEff (loop w) k) u)

-- Add something like Control.Exception.catches? It could be useful
-- for control with cut.

interpose :: (Typeable1 t, Functor t, Member t r) =>
     Union r v -> (v -> Eff r a) -> (t v -> Eff r a) -> Eff r a
interpose u loop h = case prj u of
  Just x -> h x
  _       -> send (\k -> fmap k u) >>= loop

--- ------------------------------------------------------------------------
-- State, strict

data State s w = State (s->s) (s -> w)
  deriving (Typeable, Functor) 

-- The signature is inferred
put :: (Typeable s, Member (State s) r) => s -> Eff r ()
put s = send (\k -> inj (State (const s) (\_ -> k ())))

-- The signature is inferred
get :: (Typeable s, Member (State s) r) => Eff r s
get = send (\k -> inj (State id k))

runState :: Typeable s => Eff (State s :> r) w -> s -> Eff r (w,s)
runState m s = loop s (admin m) where
 loop s (Val x) = return (x,s)
 loop s (E u)   = handle_relay u (loop s) $
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
choose lst = send (\k -> inj $ Choose lst k)

-- MonadPlus-like operators are expressible via choose

mzero' :: Member Choose r => Eff r a
mzero' = choose []
mplus' m1 m2 = choose [m1,m2] >>= id


-- The interpreter
makeChoice :: forall a r. Eff (Choose :> r) a -> Eff r [a]
makeChoice m = loop (admin m)
 where
 loop (Val x)  = return [x]
 loop (E u)    = handle_relay u loop (\(Choose lst k) -> handle lst k)
 -- Need the signature since local bindings aren't polymorphic any more
 handle :: [t] -> (t -> VE a (Choose :> r)) -> Eff r [a]
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
ifte t th el = loop [] (admin t)
 where 
 loop [] (Val x)  = th x
 -- add all other latent choices of t to th x
 -- this is like reflection of t
 loop jq (Val x)  = choose ((th x) : map (\t -> reflect t >>= th) jq) >>= id 
 loop jq (E u)    = interpose u (loop jq) (\(Choose lst k) -> handle jq lst k)
 -- Need the signature since local bindings aren't polymorphic any more
 handle :: [VE a r] -> [t] -> (t -> VE a r) -> Eff r b
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
-- (For simplicity, a co-routine reports a value but accepts unit)
data Yield inn out res = Yield out (inn -> res)
    deriving (Typeable, Functor)


yield :: (Typeable inn, Typeable out, Member (Yield inn out) r) => out -> Eff r inn
yield x = send (inj . Yield x)

-- Status of a thread: now can be composed again 
data Y inn out r a = Done a | Y out (inn -> Eff (Yield inn out :> r) a)

-- Launch a thread and report its status
runC :: (Typeable inn, Typeable out) => Eff (Yield inn out :> r) a -> Eff r (Y inn out r a)
runC m = loop (admin m) where
 loop (Val x) = return (Done x)
 loop (E u)   = case decomp u of
   Right (Yield x c) -> return (Y x (reflect . c))
   Left u -> send (\k -> fmap k u) >>= loop
