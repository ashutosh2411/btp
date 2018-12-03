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


chunker :: Int -> Int -> BStr -> BStr -> HShape
chunker n size innerpad rootpad = 
	let b = quot n size
		make_node i = InnerHash (Concat [(Slice (i*size) ((i+1)*size)), (Pad innerpad)])
		ranges = map make_node [0 .. b]
		all_ranges = 
			if rem n size == 0 then
				ranges
			else
				ranges ++ [InnerHash(Concat [(Slice (b*size) n), (Pad innerpad)])]
	in 
		InnerHash (Concat (all_ranges ++ [(Pad rootpad)]))

c2w8 :: String -> [Word8]
c2w8 = CodStr.encode


-- Fixed Block length mode with inner padding and root padding
a_block_mode x block_size = chunker (length x) block_size (c2w8 i_padding) (c2w8 r_padding)


data BTree a = 
	Inner (BTree a) (BTree a)
	|Leaf a
	deriving Show

one_level_treefy :: [BTree a] -> [BTree a]
one_level_treefy (h0:h1:tl) = (Inner h0 h1):(one_level_treefy tl)
one_level_treefy x = x


fixpt_treefy :: [BTree a] -> BTree a
fixpt_treefy [x] = x
fixpt_treefy x_l = fixpt_treefy ( one_level_treefy x_l)

to_tree :: [a] -> BTree a
to_tree = fixpt_treefy . (map Leaf)

pad_and_hash (Leaf slice) leaf_pad inner_pad root_pad = InnerHash (Concat[slice])
pad_and_hash (Inner t1 t2) leaf_pad inner_pad root_pad = InnerHash (Concat [
			 pad_and_hash t1 leaf_pad inner_pad inner_pad,
			 Pad root_pad,
			 pad_and_hash t2 leaf_pad inner_pad inner_pad])


bin_tree_hash :: (BStr -> BStr) -> Int -> BStr -> BStr
bin_tree_hash inner_hash block_size a_str =
	let 
		len = length a_str
		ranges = size_to_pairs len block_size
		shape = to_shape ranges (c2w8 l_padding) (c2w8 i_padding) (c2w8 r_padding)
	in
		s inner_hash shape a_str


p_bin_tree_hash :: (BStr -> BStr) -> Int -> BStr -> BStr
p_bin_tree_hash inner_hash block_size a_str = 
	let 
		len = length a_str
		ranges = size_to_pairs len block_size
		shape = to_shape ranges (c2w8 l_padding) (c2w8 i_padding) (c2w8 r_padding)
	in
		p inner_hash shape a_str

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

to_shape :: [(Int,Int)] -> BStr -> BStr -> BStr -> HShape
to_shape ranges leaf_pad inner_pad root_pad = 
	let
		leaves = map (\(x,y) -> (Slice x y)) ranges
		slice_tree = to_tree leaves
	in
		pad_and_hash slice_tree leaf_pad inner_pad root_pad

id_hash x = x
ipad = Pad (c2w8 i_padding)
rpad = Pad (c2w8 r_padding)

shape0 = (Slice 0 4)
shape1 = (Concat [(InnerHash (Slice 0 4)), (InnerHash (Slice 4 8))])
shape2 = (Concat [Concat [(InnerHash (Slice 0 4)),ipad], Concat [(InnerHash (Slice 4 8)),ipad],rpad])

b1 = my_slice 0 4 (c2w8 "abcdefgh")
b1' = s id_hash shape0 (c2w8 "abcdefgh")

hash1 = s id_hash shape1 (c2w8 "abcdefgh")
hash2 = s id_hash shape2 (c2w8 "abcdefgh")

alongstr = "abcdefgh"--ijklm"--opqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789"
alongstr' = c2w8 alongstr

lshape = chunker (length alongstr') 1 (c2w8 i_padding) (c2w8 r_padding)

alonghash = s id_hash lshape alongstr'
alonghash' = bin_tree_hash id_hash 1 (alongstr')

parHash1 shape1 = p id_hash shape1 alongstr'

test1 = parHash1 lshape == alonghash'
test2 = p_bin_tree_hash id_hash 8 (c2w8 alongstr) == alonghash

do_hash func = do
	args <- getArgs
	putStr "Args:"
	putStrLn (show args)
	contents <- readFile (head args)
	putStrLn $ "Hash:" ++ (show (func hash (a_block_mode contents (read (args !! 1)::Int)) (c2w8 contents)))

main = do 
	putStrLn $ "b1: " ++ show b1
	putStrLn $ "b1': " ++ show b1'
	putStrLn $ "hash1: " ++ show hash1
	putStrLn $ "hash2: " ++ show hash2
	putStrLn $ "chunker:   " ++ show alonghash
	putStrLn $ "from tree: " ++ show alonghash'
	putStrLn $ ""
	do_hash s

-- c for serial p for parallel