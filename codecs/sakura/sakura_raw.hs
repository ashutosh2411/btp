{-# LANGUAGE PackageImports #-}


import Data.Word
import qualified Codec.Binary.UTF8.String as CodStr
import Control.Parallel.Strategies
import Data.Time.Clock
--import Crypto.Hash.SHA256
--import Data.Digest.SHA256
import Data.Digest.SHA384
import System.Environment

i_padding = "I"
l_padding = "L"
r_padding = "R"

type BStr = [Word8]
type HashF = [Word8] -> [Word8]

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


fb_ith i size = Slice (i * size) ((i + 1) * size)
-- ith slice
fb_last i size n = Slice (i*size) n
-- last slice in case required
fb_pad chunk innerpad = Concat [chunk, (Pad innerpad)]
-- concatenating padding bits

chunker :: Int -> Int -> BStr -> BStr -> HShape
chunker n size innerpad rootpad =
    let
        b = quot n size
        make_node i = InnerHash (fb_pad (fb_ith i size) innerpad)
        -- Function that returns InnerHash of ith block with padding
        ranges = map make_node [0 .. b]
        -- list of padded hash slices
        all_ranges =
            if rem n size == 0 then
                ranges
            else
                ranges ++ [InnerHash (fb_pad (fb_last b size n) innerpad)]
                -- Add last block
    in
        InnerHash (Concat (all_ranges ++ [(Pad rootpad)]))
        -- In the end, pad it with rootpad

c2w8 :: String -> [Word8]
c2w8 = CodStr.encode

-- Fixed Block length mode with inner padding and root padding
a_block_mode x block_size = chunker (length x) block_size (c2w8 i_padding) (c2w8 r_padding)


-- For a Binary Tree with values only at leaves.
data BTree a =
    Inner (BTree a) (BTree a)
    |Leaf a
    deriving Show

    -- From a list to a Binary tree with leaves consisting of the values
to_tree :: [a] -> BTree a
to_tree = fixpt_treefy . (map Leaf)

-- Club 2 children and give the parent node at one level
one_level_treefy :: [BTree a] -> [BTree a]
one_level_treefy (h0:h1:tl) = (Inner h0 h1):(one_level_treefy tl)
one_level_treefy x = x

-- Recursively treefy at one level until one point is left
fixpt_treefy :: [BTree a] -> BTree a
fixpt_treefy [x] = x
fixpt_treefy x_l = fixpt_treefy ( one_level_treefy x_l)

-- Pads appropriate paddings to the tree
pad_and_hash (Leaf slice) leaf_pad inner_pad root_pad = InnerHash (Concat[slice]) -- Concat [slice, (Pad leaf_pad)]
pad_and_hash (Inner t1 t2) leaf_pad inner_pad root_pad = InnerHash (Concat [
             pad_and_hash t1 leaf_pad inner_pad inner_pad,  -- t1
             Pad root_pad,                                  -- root_pad
             pad_and_hash t2 leaf_pad inner_pad inner_pad]) -- t2

-- Wrapper function: returns hashed string
bin_tree_hash :: (BStr -> BStr) -> Int -> BStr -> BStr
bin_tree_hash inner_hash block_size a_str =
    let
        len = length a_str
        ranges = size_to_pairs len block_size
        shape = to_shape ranges (c2w8 l_padding) (c2w8 i_padding) (c2w8 r_padding)
    in
        s inner_hash shape a_str

-- generates a list of parameters for Slice
size_to_pairs :: Integral a => a -> a -> [(a, a)]
size_to_pairs n block_size =
    let
        b = quot n block_size
        ranges = map (\a -> (a * block_size, (a + 1) * block_size)) [0 .. b]
    in
        if rem n block_size == 0 then
            ranges
        else
            ranges ++ [(b * block_size, n)]

-- generates a Padded Tree which is supposed to be similar to the HShape generated by chunker
to_shape :: [(Int,Int)] -> BStr -> BStr -> BStr -> HShape
to_shape ranges leaf_pad inner_pad root_pad =
    let
        leaves = map (\(x,y) -> (Slice x y)) ranges
        slice_tree = to_tree leaves
        -- Tree with leaves as Slices
    in
        pad_and_hash slice_tree leaf_pad inner_pad root_pad

-- Validating the correctness of Sakura implementation
id_hash x = x
ipad = Pad (c2w8 i_padding)
rpad = Pad (c2w8 r_padding)

-- Some shapes :: HShape
shape0 = (Slice 0 4)
shape1 = (Concat [(InnerHash (Slice 0 4)), (InnerHash (Slice 4 8))])
shape2 = (Concat [Concat [(InnerHash (Slice 0 4)),ipad], Concat [(InnerHash (Slice 4 8)),ipad],rpad])

b1 = my_slice 0 4 (c2w8 "abcdefgh")
b1' = s id_hash shape0 (c2w8 "abcdefgh")
-- essentially slice of first 4 characters
hash1 = s id_hash shape1 (c2w8 "abcdefgh")
hash2 = s id_hash shape2 (c2w8 "abcdefgh")

alongstr = "abcdefghijklm"
alongstr' = c2w8 alongstr

lshape = chunker (length alongstr') 1 (c2w8 i_padding) (c2w8 r_padding)

alonghash = s id_hash lshape alongstr'
-- Using the chunker shape
alonghash' = bin_tree_hash id_hash 1 (alongstr')
-- generating the shape from tree

-- Interface to hash the contents of a file
do_hash func = do
    args <- getArgs
    putStr "Args:"
    putStrLn (show args)
    contents <- readFile (head args)
    putStrLn $ "Hash:" ++ (show (func hash (a_block_mode contents (read (args !! 1)::Int)) (c2w8 contents)))

-- Main function
main = do
    putStrLn $ "b1: " ++ show b1
    putStrLn $ "b1': " ++ show b1'
    putStrLn $ "hash1: " ++ show hash1
    putStrLn $ "hash2: " ++ show hash2
    putStrLn $ "chunker:   " ++ show alonghash
    putStrLn $ "from tree: " ++ show alonghash'
    putStrLn $ ""
    do_hash s
