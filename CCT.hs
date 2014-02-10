
{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables,ExistentialQuantification,GADTs,MultiParamTypeClasses,Rank2Types #-}
module DCC(MonadDelimitedCont(..),reset,shift,control,shift0,control0,abort, Prompt, SubCont,CCT) where

import Data.Interface.TSequence
import Data.RTQueue
import Data.CTQueue
import Control.Monad.CC hiding (Prompt,SubCont, CC, CCT, runCCT,SubCont)
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans
import Unsafe.Coerce -- for Prompt equality

newtype MCont m a b = MCont { runMC :: a -> m b }
type MCExp m a b = CTQueue RTQueue (MCont m) a b

data DelimCont r m a b = Delim (Prompt r b) (MCExp (CCT r m) a b)
type DelimConts r m a b = CTQueue RTQueue (DelimCont r m) a b
data SubCont r m a b = forall w. SubCont {
   delimited :: DelimConts r m a w,
   top       :: MCExp (CCT r m) w b
  }

emptySC = SubCont tempty tempty

singleSC f = SubCont tempty (tsingleton (MCont f))

concatSC :: SubCont r m a b -> SubCont r m b c -> SubCont r m a c
concatSC (SubCont l lt) (SubCont r rt) = case tviewl r of
  TEmptyL -> SubCont l (lt >< rt)
  Delim p h :| t -> let r = Delim p (lt >< h) <| t
                    in SubCont (l >< r) rt

data Action r m a where
  WithSubCont :: Prompt r w -> (SubCont r m a w -> CCT r m w) -> Action r m a
  PAct :: P r m a -> Action r m a

data CCT r m a = forall w. CCT { 
   cur :: Action r m w,
   rest :: SubCont r m w a 
  }

(!>>=) :: CCT r m a -> SubCont r m a b -> CCT r m b
(CCT c r) !>>= d = CCT c (r `concatSC` d)

instance Monad m => Monad (CCT r m) where
  return a = CCT (PAct (return a)) emptySC
  m >>= f = m !>>= singleSC f

instance Monad m => MonadDelimitedCont (Prompt ans) (SubCont ans m) (CCT ans m) where
  newPrompt                          = CCT (PAct newPromptName) emptySC
  pushPrompt p (CCT c (SubCont d t)) = CCT c (SubCont (d |> Delim p t) tempty)
  withSubCont p f                    = CCT (WithSubCont p f) emptySC
  pushSubCont s (CCT c r)            = CCT c (r `concatSC` s)


runCCT :: Monad m => (forall r. CCT r m a) -> m a
runCCT m = runP (runCCTP m)

runCCTP :: forall m r a .Monad m => CCT r m a -> P r m a
runCCTP (CCT c r) = case c of
  PAct m          -> m >>= runSC r
  WithSubCont p f | SubCont d l <- r -> 
     let (sc,r) = splitSeq p d
     in runCCTP $ f sc !>>= SubCont r l
 where
  runSC :: SubCont r m w a -> w -> P r m a
  runSC (SubCont d l) x = case tviewl d of
    TEmptyL -> case tviewl l of
      TEmptyL       -> return x
      MCont f :| t  -> runCCTP $ f x !>>= SubCont tempty t
    Delim p h :| t -> case tviewl h of
      TEmptyL       -> runSC (SubCont t l) x
      MCont f :| ti -> runCCTP $ f x !>>= SubCont (Delim p ti <| t) l

splitSeq :: Prompt r w -> DelimConts r m a b -> (SubCont r m a w, DelimConts r m w b)
splitSeq p q = case tviewl q of
   TEmptyL ->   error "Prompt was not found on the stack."
   Delim p' sk :| t -> 
    case eqPrompt p' p of
         EQU -> (SubCont tempty sk, t)
         NEQ -> case splitSeq p t of
                     (SubCont dl tl, sk') -> (SubCont ((Delim p' sk) <| dl) tl, sk')



{- Adapted from LogicT package Control.Monad.CC.Prompt -}

newtype Prompt r a = Prompt Int

newtype P r m a = P { unP :: StateT Int m a }
    deriving (Functor, Monad, MonadTrans, MonadState Int, MonadReader r)


runP :: (Monad m) => (forall r. P r m a) -> m a
runP p = evalStateT (unP p) 0

newPromptName :: (Monad m) => P r m (Prompt r a)
newPromptName = do i <- get ; put (succ i) ; return (Prompt i)

data Equal a b where
    EQU :: Equal a a
    NEQ :: Equal a b


eqPrompt :: Prompt ans a -> Prompt ans b -> Equal a b
eqPrompt (Prompt p1) (Prompt p2)
    | p1 == p2  = unsafeCoerce EQU
    | otherwise = NEQ

