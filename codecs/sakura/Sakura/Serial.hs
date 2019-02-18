{-# LANGUAGE PackageImports #-}

module Sakura.Serial (HShape, s) where

import Data.Word

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

s :: HashF -> HShape -> BStr -> BStr
-- Serial hash computation
-- only place where hash function is called
s h (InnerHash aShape) bStr = h (s h aShape bStr)
-- Concatenate results of subtree computations
s h (Concat l) bStr = concat (map (\x-> s h x bStr) l)
-- Only way to directly consume input string
s _ (Slice from to) bStr = my_slice from to bStr
-- Only way to directly insert padding bits
s _ (Pad x) _ = x