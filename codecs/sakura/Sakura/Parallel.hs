{-# LANGUAGE PackageImports #-}

module Sakura.Serial (HShape, p) where

import Data.Word
import Control.Parallel.Strategies

type BStr = [Word8]
type HashF = BStr -> BStr

data HShape =
    InnerHash HShape
    |Concat [HShape]
    |Interleaving [HShape]
    |Slice Int Int
    |Pad BStr deriving Show

my_slice :: Int -> Int -> BStr -> BStr
my_slice from to = (drop from).(take to)

p :: HashF -> HShape -> BStr -> BStr
-- Parallel hash computation
-- Only place where the hash function is called
p h (InnerHash aShape) bStr = h $ p h aShape bStr
-- Concatenate results of subtree computations
p h (Concat l) bStr = concat (parMap rpar (\mu -> p h mu bStr) l)
-- Only way to directly consume input string
p _ (Slice from to) bStr = my_slice from to bStr
-- Only way to directly insert padding bits
p _ (Pad x) _ = x