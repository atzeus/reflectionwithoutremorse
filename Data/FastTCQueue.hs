module Data.FastTCQueue(module Data.Interface.TSequence, FastTCQueue) where

import Data.Interface.TSequence
import Data.RTQueue
import Data.CTQueue

type FastTCQueue =  CTQueue RTQueue
