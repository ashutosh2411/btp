{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Word
import Control.Parallel.Strategies
import Data.Digest.SHA384
import System.Environment

type BStr = [Word8]
type HashF = [Word8] -> [Word8]

data HShape =
  InnerHash HShape
  |Concat [HShape]
  |Interleaving [HShape]
  |Slice Int Int
  |Pad  BStr  deriving Show

class Hashable a where
	data ID a
	type Node a
	hash :: Node a -> ID a
	toSakura :: a -> HShape
	fromSakura :: HShape -> a

instance Hash [a] where
	hash = 