{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE UndecidableInstances #-}


module DThash where
import Data.Word
import qualified Codec.Binary.UTF8.String as CodStr
import Data.Time.Clock
--import Crypto.Hash.SHA256
import Data.Digest.SHA256
--import Data.Digest.SHA384
import System.Environment
import GHC.Generics

type BStr = [Word8]
type HashF = [Word8] -> [Word8]


id :: a -> a
id x = x

toWord8 :: String -> [Word8]
toWord8 = CodStr.encode

class GHashable f where 
    gcomputeHash :: HashF -> f a -> BStr

class G'Hashable a where 
	g'computeHash :: HashF -> a -> BStr

instance (Show a) => G'Hashable a where
	g'computeHash hashf a = hashf $ toWord8 (show a)

instance GHashable U1 where
    gcomputeHash hashf U1 = hashf (toWord8 "U1")

instance (GHashable a, GHashable b) => GHashable (a :*: b) where
    gcomputeHash hashf (a :*: b) = 
        concat [gcomputeHash hashf a, 
                gcomputeHash hashf b, 
                toWord8 "Pdt1"]

instance (GHashable a, GHashable b) => GHashable (a :+: b) where
    gcomputeHash hashf (L1 x) = 
        concat [gcomputeHash hashf x, 
                toWord8 "Sum1L1"]
    gcomputeHash hashf (R1 x) = 
        concat [gcomputeHash hashf x, 
                toWord8 "Sum1R1"]

instance (GHashable a) => GHashable (M1 i c a) where
    gcomputeHash hashf (M1 x) = gcomputeHash hashf (x)

instance (Show a) => GHashable (K1 i a) where
    gcomputeHash hashf (K1 x) =  concat [hashf (toWord8 $ show x)]

class Hashable a where
    computeHash :: HashF -> a -> BStr
    default computeHash :: (Generic a, GHashable (Rep a)) => HashF -> a -> BStr
    computeHash hashf x = gcomputeHash hashf (from x) 