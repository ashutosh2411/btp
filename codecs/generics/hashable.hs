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

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Generic, Show)


class Hashable a where
	data ID a
	type Node a
	hash :: Node a -> ID a
	toSakura :: a -> HShape

instance Hashable [a] where
	hash = 
	toSakura [] = InnerHash "Nil"
	toSakura x:xs = InnerHash $ Concat [InnerHash $ Pad "Cons", InnerHash $ show x, InnerHash ]

instance Hashable Tree a where
	toSakura EmptyTree = InnerHash $ Pad "EmptyTree"
	toSakura Node a tl tr = Concat [InnerHash $ Pad "Node", InnerHash $ Concat [toSakura tl, toSakura tr]]

