{-# LANGUAGE PackageImports #-}


module Sakura (HShape) where

import Data.Word
import Control.Parallel.Strategies
import Data.Digest.SHA384
import System.Environment
import qualified Codec.Binary.UTF8.String as CodStr

type BStr = [Word8]
type HashF = [Word8] -> [Word8]

data HShape =
  InnerHash HShape
  |Concat [HShape]
  |Interleaving [HShape]
  |Slice Int Int
  |Pad  BStr  deriving Show

-- my_slice :: Int -> Int -> BStr -> BStr
my_slice from to = (drop from).(take to)

-- s :: HashF -> HShape -> BStr -> BStr
-- Serial hash computation

-- only place where hash function is called
s h (InnerHash aShape) bStr  =  h (s h aShape bStr)
-- Concatenate results of subtree computations
s h (Concat l) bStr = concat  (map (\x-> s h x bStr)  l)        -- 
-- Only way to directly consume input string
s _ (Slice from to) bStr = my_slice from to bStr
-- Only way to directly insert padding bits
s _ (Pad x) _ = x

-- p :: HashF -> HShape -> BStr -> BStr
-- Parallel hash computation

-- Only place where the hash function is called
p h (InnerHash aShape) bStr = h $ p h aShape  bStr
-- Concatenate results of subtree computations
p h (Concat l) bStr = concat (parMap rpar (\mu -> p h mu bStr) l)
-- Only way to directly consume input string
p _ (Slice from to) bStr = my_slice from to bStr
-- Only way to directly insert padding bits
p _ (Pad x) _ = x

fb_ith i size = Slice (i * size) ((i + 1) * size)
fb_last i size n = Slice (i*size) n
fb_pad chunk innerpad = Concat [chunk, (Pad innerpad)]

-- chunker :: Int -> Int -> BStr -> BStr -> HShape
chunker n size innerpad rootpad =
    let b = quot n size
        make_node i = InnerHash (fb_pad (fb_ith i size) innerpad)
        ranges = map make_node [0 .. b]
        all_ranges = 
            if rem n size == 0 then
                ranges
            else
                ranges ++ [InnerHash(fb_pad (fb_last b size n) innerpad)]
    in
           InnerHash (Concat (all_ranges ++ [(Pad rootpad)]))

-- c2w8 :: String -> [Word8]
c2w8 = CodStr.encode

a_block_mode x block_size  =
    chunker (length x) block_size  (c2w8 "IIII") (c2w8 "RRRR")
