{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

import GHC.Generics

data Tree a = EmptyTree 
    | Node a (Tree a) (Tree a) 
    deriving (Generic, Show)

instance (Hashable a, Show a) => Hashable (Tree a)  

data HShape =
    InnerHash HShape
    |Concat [HShape]
    |Interleaving [HShape]
    |Slice Int Int
    |Pad String 
    deriving Show

class Hashable a where
    toHShape :: a -> HShape
    default toHShape :: (Generic a, GHashable (Rep a)) => a -> HShape
    toHShape = gtoHShape . from

class GHashable f where
    gtoHShape :: f a -> HShape 

instance GHashable U1 where
    gtoHShape U1 = Concat []

instance (GHashable a, GHashable b) => GHashable (a :*: b) where
    gtoHShape (a :*: b) = Concat [InnerHash (gtoHShape a), InnerHash (gtoHShape b)]

instance (GHashable a, GHashable b) => GHashable (a :+: b) where
    gtoHShape (L1 x) = InnerHash (gtoHShape x)
    gtoHShape (R1 x) = InnerHash (gtoHShape x)

instance (GHashable a) => GHashable (M1 i c a) where
    gtoHShape (M1 x) = Concat [gtoHShape x]

instance (Show a) => GHashable (K1 i a) where
    gtoHShape (K1 x) = Pad (show x)

-- instance GHashable (Tree) where
--     gtoHShape ta = gtoHShape (from ta)

a = M1 {unM1 = R1 (M1 {unM1 = M1 {unM1 = K1 {unK1 = 5}} :*: (M1 {unM1 = K1 {unK1 = EmptyTree}} :*: M1 {unM1 = K1 {unK1 = EmptyTree}})})}

main = print $ show $ from (Node 5 EmptyTree EmptyTree)