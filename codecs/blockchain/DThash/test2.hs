{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}



import GHC.Generics

constrName :: (HasConstructor (Rep a), Generic a)=> a -> String
constrName = genericConstrName . from 

class HasConstructor (f :: * -> *) where
  genericConstrName :: f x -> String

instance HasConstructor f => HasConstructor (M1 D c f) where
  genericConstrName (M1 x) = genericConstrName x

instance HasConstructor f => HasConstructor (M1 S c f) where
  genericConstrName (M1 x) = genericConstrName x

instance (HasConstructor x, HasConstructor y) => HasConstructor (x :+: y) where
  genericConstrName (L1 l) = genericConstrName l
  genericConstrName (R1 r) = genericConstrName r

instance HasConstructor (K1 i a) where
	genericConstrName x = ""

instance Constructor c => HasConstructor (C1 c f) where
  genericConstrName x = conName x

--------------

--type BStr = [Word8]
--type HashF = [Word8] -> [Word8]
type BStr = String
type HashF = String -> String


--toWord8 :: String -> [Word8]
--toWord8 = CodStr.encode
toWord8 :: String -> String
toWord8 x = x

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

instance (GHashable a, HasConstructor a) => GHashable (C1 c a) where
    gcomputeHash hashf (M1 x) = concat[hashf $ toWord8 $ genericConstrName (x), toWord8 "M1D"]

instance (GHashable a) => GHashable (S1 c a) where
    gcomputeHash hashf (M1 x) = concat[gcomputeHash hashf x, toWord8 "M1S"]

instance (Show a) => GHashable (K1 i a) where
    gcomputeHash hashf (K1 x) =  concat [hashf (toWord8 $ show x), toWord8 "K1"]

class Hashable a where
    computeHash :: HashF -> a -> BStr
    default computeHash :: (Generic a, GHashable (Rep a)) => HashF -> a -> BStr
    computeHash hashf x = gcomputeHash hashf (from x) 

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Generic, Show)

instance (Hashable a,  Show a) => Hashable (Tree a) 
instance Hashable Int 

main = putStrLn $ show $ computeHash Prelude.id (Node 5 EmptyTree EmptyTree :: Tree Int)


--------------
data Foo = Bar Int | Baz Float deriving Generic
newtype X = X Char deriving Generic
data Y = Y deriving Generic

--main = print $ constrName (Bar 1)