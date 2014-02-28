module TreeShow where

data Tree = Leaf Int | Node Tree Tree

show1 :: Tree -> String
show1 (Leaf x) = show x
show1 (Node x y) = paren $ (show1 x) ++ " " ++ (show1 y)

paren :: String -> String
paren x = "(" ++ x ++ ")"

t1 = (Leaf 1 `Node` Leaf 2) `Node` Leaf 3
t1s1 = show1 t1

-- build some trees
tskewed :: Int -> Tree
tskewed 0 = Leaf 0
tskewed n = tskewed (n-1) `Node` Leaf n

tfull :: Int -> Tree
tfull n = go n 1
 where
   go 0 x = Leaf x
   go n x = Node (go (n-1) (2*x)) (go (n-1) (2*x+1))

tf5s1 = show1 (tfull 3)
-- "(((8 9) (10 11)) ((12 13) (14 15)))"

tsemifull :: Int -> Tree
tsemifull n = go n 1
 where
   go 0 x = Leaf x
   go 1 x = Leaf x
   go 2 x = Leaf x
   go n x = Node (go (n-1) (2*x)) (go (n-3) (2*x+1))

tg5s1 = show1 (tsemifull 7)
-- "(((((32 33) 17) 9) (10 11)) ((12 13) 7))"

-- performance problem

tsprobs1 = length $ show1 (tskewed 100)
{-
493
(0.01 secs, 1732868 bytes)
-}

tsprobs2 = length $ show1 (tskewed 1000)
{-
5894
(0.60 secs, 162124732 bytes)
-}
tsprobs3 = length $ show1 (tskewed 10000)
-- takes too long

tfprobs1 = length $ show1 (tsemifull 25)
-- 69028
-- (0.44 secs, 55053456 bytes)

tfprobs2 = length $ show1 (tsemifull 27)
-- 155126
-- (1.08 secs, 130748248 bytes)

tfprobs3 = length $ show1 (tsemifull 29)
-- 347272
-- (2.55 secs, 310292384 bytes)

tfprobs4 = length $ show1 (tsemifull 31)
-- 775554
-- (5.86 secs, 734798260 bytes)


-- Speading up
shows2 :: Tree -> ShowS
shows2 (Leaf x) = shows x
shows2 (Node x y) = parens $ (shows2 x) . lit " " . (shows2 y)


lit :: String -> ShowS
lit x = \l -> x ++ l

parens :: ShowS -> ShowS
parens x = lit "(" . x . lit ")"

show2 :: Tree -> String
show2 x = shows2 x ""

tg5s2 = show2 (tsemifull 5)
-- "((((16 17) 9) (10 11)) ((12 13) 7))"

ts2probs2 = length $ show2 (tskewed 1000)
{-
5894
(0.03 secs, 2118276 bytes)
-}
ts2probs3 = length $ show2 (tskewed 10000)
{-
68895
(0.25 secs, 9317740 bytes)
-}

tf2probs2 = length $ show2 (tsemifull 27)
-- 155126
-- (0.81 secs, 26437680 bytes)

tf2probs3 = length $ show2 (tsemifull 29)
-- 347272
-- (1.88 secs, 55337732 bytes)

tf2probs4 = length $ show2 (tsemifull 31)
-- 775554
-- (4.02 secs, 120562296 bytes)
-- Better by a whole second

--  But we don't want to print a space between parens

shows3 :: Tree -> ShowS
shows3 (Leaf x) = shows x
shows3 (Node x y) = parens $ if is_paren xs && is_paren ys then xs . ys
                               else xs . lit " " . ys
 where
  xs = shows3 x
  ys = shows3 y


is_paren :: ShowS -> Bool
is_paren x | '(':_ <- x "" = True
is_paren _ = False

show3 :: Tree -> String
show3 x = shows3 x ""

tg5s3 = show3 (tsemifull 7)
-- "(((((32 33) 17) 9)(10 11))((12 13) 7))"

ts3probs2 = length $ show3 (tskewed 1000)
-- 5894
-- (0.04 secs, 2067384 bytes)
ts3probs3 = length $ show3 (tskewed 10000)
-- 68895
-- (0.34 secs, 11869540 bytes)

tf3probs2 = length $ show3 (tsemifull 27)
-- 149231
-- (0.94 secs, 29482740 bytes)

tf3probs3 = length $ show3 (tsemifull 29)
-- 334609
-- (2.10 secs, 64129572 bytes)

tf3probs4 = length $ show3 (tsemifull 31)
-- 748354
-- (4.66 secs, 139182136 bytes)

{-
*TreeShow> length $ show1 (tfull 16)
555357
(3.54 secs, 413707604 bytes)
*TreeShow> length $ show2 (tfull 16)
555357
(2.71 secs, 83352628 bytes)
*TreeShow> length $ show3 (tfull 16)
522590
(3.03 secs, 93693336 bytes)
-}