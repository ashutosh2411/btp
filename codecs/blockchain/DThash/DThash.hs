{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module DThash where
import Data.Data
import Data.Word
import qualified Codec.Binary.UTF8.String as CodStr
import Data.Time.Clock
import Data.Digest.SHA256
import System.Environment
import GHC.Generics

type BStr = [Word8]
type HashF = [Word8] -> [Word8]
--type BStr = String
--type HashF = String -> String


toWord8 :: String -> [Word8]
toWord8 = CodStr.encode
--toWord8 :: String -> String
--toWord8 x = x

class GHashable f where 
    gcomputeHash :: HashF -> f a -> BStr

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

instance (GHashable a) => GHashable (D1 c a) where
    gcomputeHash hashf (M1 x) = concat[gcomputeHash hashf x, toWord8 "M1D"]

instance (GHashable a, Constructor c) => GHashable (C1 c a) where
    gcomputeHash hashf m1x@(M1 x) = concat[hashf $ toWord8 $ conName (m1x), toWord8 "M1D", gcomputeHash hashf x]

instance (GHashable a) => GHashable (S1 c a) where
    gcomputeHash hashf (M1 x) = concat[gcomputeHash hashf x, toWord8 "M1S"]

instance (Show a) => GHashable (K1 i a) where
    gcomputeHash hashf (K1 x) =  concat [hashf (toWord8 $ show x), toWord8 "K1"]

class Hashable a where
    computeHash :: HashF -> a -> BStr
    default computeHash :: (Generic a, GHashable (Rep a)) => HashF -> a -> BStr
    computeHash hashf x = gcomputeHash hashf (from x) 
