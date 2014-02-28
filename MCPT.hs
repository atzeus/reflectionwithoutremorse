

{- A benchmark for LogicT: Find n solutions to the missionaries
   and cannibals problem 
Adapted from the LogicT code at http://okmij.org/ftp/Haskell/LogicT.tar.gz
-}


import Control.Monad
import Control.Monad.Trans
import System.Environment 
import System.IO

import Control.Monad.Logic.Class
import Control.Monad.Identity
-- the three implementations of Logic:

import Fixed.Logic -- our new implementation
--import Control.Monad.Logic -- two continuation implementation
--import LogicBenchmarks.LogicCC  -- delimited continuations implementation


-------------------------------------------------------------------------------
-- Given M missionaries, C cannibals, and B boats where each boat 
--   holds at most two people
-- Goal: move M+C from one side to the other
-- Constraint: cannibals never outnumber missionaries in any place
-- For more details on the problem, see:
-- http://www.cs.berkeley.edu/~russell/code/search/domains/cannibals.lisp


bagofN :: MonadLogic m => Maybe Int -> m a -> m [a]
bagofN (Just n) _ | n <= 0  = return []
bagofN n m = msplit m >>= bagofN'
    where bagofN' Nothing = return []
	  bagofN' (Just (a,m')) = liftM (a:) (bagofN (fmap (-1 +) n) m')

-- (M,C,B) on each side of the river
type Left  = (Int,Int,Int)
type Right = (Int,Int,Int)
type State = (Left,Right)

-- A final state
final :: MonadPlus m => State -> m State
final s@((0,0,_),_) = return s
final _ = mzero

-- Moves

data MoveDir = FWD | BWD		-- From left-to-right or vice versa
	     deriving Show

-- An action: a change in the State
type Action = (Int,Int,MoveDir)

-- Permissible actions: at most two people can move in a boat
legalActions :: [Action]
legalActions = (map (add_dir FWD) changes) ++ (map (add_dir BWD) changes)
    where 
    changes = [(1,0),(0,1),(2,0),(0,2),(1,1)]
    add_dir dir (a,b) = (a,b,dir)

-- The transmission function...
-- Apply an action to a state. Fail if a bad state is reached
move :: MonadPlus m => State -> Action -> m State
move ((m1,c1,b1),(m2,c2,b2)) (mm,cm,FWD) | b1 > 0 = 
  check ((m1-mm, c1-cm, b1-1),(m2+mm, c2+cm, b2+1))
move ((m1,c1,b1),(m2,c2,b2)) (mm,cm,BWD) | b2 > 0 = 
  check ((m1+mm, c1+cm, b1+1),(m2-mm, c2-cm, b2-1))
move _ _ = mzero

-- Check the newly reached state. Fail if it is bad
check :: MonadPlus m => State -> m State
check s@((m1,c1,b1),(m2,c2,b2)) = 
    if and [m1 >= 0, m2 >= 0, c1 >= 0, c2 >= 0, 
	    (m1 == 0 || c1 <= m1),	-- If there are missionaries, there
					-- should be at least as many 
	    (m2 == 0 || c2 <= m2)]	-- of them as there are cannibals
       then return s
       else mzero


-- non-deterministically, choose an element from a list
-- Obviously, we fail if the list is empty.
-- This function is obviously a manifestation of the Axiom of Choice
choose:: MonadPlus m => [a] -> m a
choose = msum . map return

occurs e lst = do { e' <- choose lst; if e == e' then return e else mzero }

-- The first solution: Depth-first-search

data SearchS = SearchS State     -- Current state
	               [State]   -- Seen states; includes current
		       [Action]  -- Actions that lead to the current state

instance Show SearchS where
    show (SearchS _ _ actions) = show actions

solve_dfs (SearchS current seen actions) = 
    do 
    a     <- choose legalActions
    s     <- move current a
    --liftIO $ putStrLn $ "Tentative move: " ++ (show current) ++ " -" ++
	--                (show a) ++ "-> " ++ (show s)
    let news = SearchS s (s:seen) (a:actions)
    ifte (final s) 
         (const $ return news)
	 (ifte (once (occurs s seen))
	       (const $ mzero)
	       (solve_dfs news))


do'solve nr left = result >>= (print . show . length) 
      where s = (left, (0,0,0))
	    result = observeAllT$ solve_dfs (SearchS s [s] []) 


main = do args <- getArgs 
          let n = read (head args)
          let m = read (head (tail args))
          let p = read (head (tail $ tail args))
          let nr = read (head (tail $ tail $ tail $ args)) :: Int 
          do'solve nr (n,m,p)
