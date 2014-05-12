module Data.FastCQueue(module Data.Interface.Sequence, FastCQueue) where

import Data.LowerTSequence
import Data.Interface.Sequence
import Data.RTQueue
import Data.CTQueue

type FastCQueue =  MSeq (CTQueue RTQueue)
