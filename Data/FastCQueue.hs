module Data.FastCQueue(module Data.Interface.Sequence, FastCQueue) where

import Data.LowerTSequence
import Data.Interface.Sequence
import Data.RTQueue
import Data.CTQueue

-- A worst case O(1) catenable queue, combining two RTQueue with CTQueue

type FastCQueue =  MSeq (CTQueue RTQueue)
