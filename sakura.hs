import Data.Word
import Control.Parallel.Strategies

type BStr = [Word8]
type HashF = [Word8] -> [Word8]

data HShape =
  InnerHash HShape
  |Concat [HShape]  
  |Interleaving [HShape]  
  |Slice Int Int  
  |Pad  BStr  deriving Show

my_slice :: Int -> Int -> BStr -> BStr
my_slice from to = (drop from).(take to)

s :: HashF -> HShape -> BStr -> BStr
s h (InnerHash aShape) bStr  =  h (s h aShape bStr)
s h (Concat l) bStr = concat  (map (\x-> s h x bStr)  l)
s _ (Slice from to) bStr = my_slice from to bStr
s _ (Pad x) _ = x

p :: HashF -> HShape -> BStr -> BStr
p h (InnerHash aShape) bStr = h $ p h aShape  bStr
p h (Concat l) bStr = concat (parMap rpar (\mu -> p h mu bStr) l)
p _ (Slice from to) bStr = my_slice from to bStr
p _ (Pad x) _ = x
