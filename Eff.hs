{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction,TypeSynonymInstances #-}

module Eff where

import Control.Monad
import Data.Typeable
import OpenUnion1
import FreeMonad

-- Fixed version of the extensible effects
-- framework at http://okmij.org/ftp/Haskell/extensible/Eff.hs


type Eff r a = FreeMonad (Union r) a

instance Functor f => Functor (FreeMonad f) where
  fmap = liftM 



-- send a request and wait for a reply
send :: Union r a -> Eff r a
send u = Impure (fmap (exprm . Pure) u)



-- ------------------------------------------------------------------------
-- The initial case, no effects

data Void -- no constructors

-- The type of run ensures that all effects must be handled:
-- only pure computations may be run.
run :: Eff Void w -> w
run (Pure a) = a

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
 loop (Pure x)  = return [x]
 loop (Impure u)    = handle_relay (fmap valm u) loop (\(Choose lst k) -> handle lst k)
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
 loop [] (Pure x)  = th x
 -- add all other latent choices of t to th x
 -- this is like reflection of t
 loop jq (Pure x)  = choose ((th x) : map (\t -> t >>= th) jq) >>= id 
 loop jq (Impure u)    = interpose (fmap valm u) (loop jq) (\(Choose lst k) -> handle jq lst k)
 -- Need the signature since local bindings aren't polymorphic any more
 handle :: [Eff r a] -> [t] -> (t -> Eff r a) -> Eff r b
 handle [] [] _     = el                    -- no more choices left
 handle (j:jq) [] _ = loop jq j
 handle jq [x] k    = loop jq (k x)
 handle jq (x:rest) k = loop (map k rest ++ jq) (k x) -- DFS

guard' :: Member Choose r => Bool -> Eff r ()
guard' True  = return ()
guard' False = mzero'



