
{-# LANGUAGE ScopedTypeVariables,RankNTypes, NoMonomorphismRestriction #-}
module Transform2Cont where
import Prelude hiding (concat, map,concatMap)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logic.Class hiding (reflect)

-- This is the list encoding as functions, encoding a list as a right fold
-- see http://en.wikipedia.org/wiki/Church_encoding#List_encodings

{-
I've been thinking about if we can transform the 2-continuation passing implementation
of LogicT to use our solution. I've been staring at it for quite a while, and found
out what is going on. Essentially the 2-continuation passing implementation corresponds
a function representation of a list using the right fold encoding:

newtype FoldRList a = FoldRList { unfl :: forall b. (a -> b -> b) -> b -> b }

The 2 continuation passing implementation is the same, except that now the result (b) must be m b:

newtype LogicT m a = LogicT { unLogicT :: forall b. (a -> m b -> m b) -> m b -> m b }

What's going on? Essentially, if we think of lists as the fixpoint of
data ListF a s = End | HT a s
then this is a _function encoding_ of fix Logic(a) = fix v. m (ListF a v)

Hence, the 2-continuation passing implementation of LogicT is a function encoding of the following datatype:

type LogicD m a = m (Lst m a)
data Lst m a = End
             | HT a (LogicD m a)

This is isomorphic to the code in the paper, since the data type ML (below)
is isomorphic to LogicD 

newtype ML m a = ML (m (Maybe (a, ML m a)))

End corresponds to Nothing, Just (h,t) to HT

I've added the complete code to code/Transform2Cont.hs 

Hence, what I've actually done with the code in the paper 
is reverse the function encoding to a regular datatype and then apply our solution.

Do you think we should mention this in the paper? Our method does not apply to function encodings
as far as I can see (this is related to your thought on the Boehm-Berarducci).
-}

newtype FoldRList a = FoldRList { unfl :: forall b. (a -> b -> b) -> b -> b }

empty = FoldRList $ \f z -> z

concat :: FoldRList a -> FoldRList a -> FoldRList a
concat l r = FoldRList $ \f z -> unfl l f (unfl r f z)

single :: a -> FoldRList a
single a = FoldRList $ \f z -> f a z

cons :: a -> FoldRList a -> FoldRList a
cons h t = FoldRList $ \f z -> f h (unfl t f z)

concatMap :: (a -> FoldRList b) -> FoldRList a -> FoldRList b
concatMap g l = FoldRList $ \f z -> unfl l (\a -> unfl (g a) f) z

headtail :: FoldRList a -> Maybe (a, FoldRList a)
headtail l = unfl l fsth Nothing
  where fsth h t = Just (h,reflect t)

reflect :: Maybe (a, FoldRList a) -> FoldRList a
reflect (Just (h,t)) = cons h t
reflect Nothing = empty

{- Now we go to LogicT with 2 continuations -}

newtype LogicT m a = LogicT { unLogicT :: forall b. (a -> m b -> m b) -> m b -> m b }

{-
  The r code is then essentially the same, except that we now
  need monadic operations in some places
-}
   
emptyl :: LogicT m a
emptyl = LogicT $ \f z -> z

concatl :: LogicT m a -> LogicT m a -> LogicT m a
concatl l r = LogicT $ \f z -> unLogicT l f (unLogicT r f z)


singlelm :: Monad m => m a -> LogicT m a
singlelm m = LogicT $ \f z -> m >>= \a -> f a z

singlel :: a -> LogicT m a
singlel a = LogicT $ \f z -> f a z

concatMapl :: forall a m b. (a -> LogicT m b) -> LogicT m a -> LogicT m b
concatMapl g l = LogicT $ \f z -> unLogicT l (\a -> unLogicT (g a) f) z

headtaill :: Monad m => LogicT m a -> m (Maybe (a, LogicT m a))
headtaill l = unLogicT l fsth (return Nothing)
  where fsth h t = return $ Just (h,lift t >>= reflectl)

reflectl :: Maybe (a, LogicT m a) -> LogicT m a
reflectl (Just (h,t)) = singlel h `concatl` t
reflectl Nothing = emptyl

instance Monad (LogicT m) where
  return = singlel
  (>>=) = flip concatMapl

instance MonadPlus (LogicT m) where
  mzero = emptyl
  mplus = concatl

instance MonadTrans LogicT where
  lift = singlelm

instance Monad m => MonadLogic (LogicT m) where
  msplit = lift . headtaill



type LogicD m a = m (Lst m a)
data Lst m a = End
             | HT a (LogicD m a)

emptyd = return End

concatd :: Monad m => LogicD m a -> LogicD m a -> LogicD m a
concatd l r = l >>= loop where
  loop End = r
  loop (HT h t) = return (HT h (t >>= loop))

concatMapd :: Monad m => (a -> LogicD m b) -> LogicD m a -> LogicD m b
concatMapd f l = l >>= loop where
  loop End = return End
  loop (HT h t) = concatd (f h) (t >>= loop)

-- Etc..

