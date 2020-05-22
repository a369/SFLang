{-# LANGUAGE DeriveGeneric,
             TypeOperators,
             ScopedTypeVariables,
             FlexibleInstances,
             FlexibleContexts,
             DefaultSignatures #-}
module Lang.Helper.ProgramGeneric (bindObject, declareEnum, instanceEnum, ToBind(..), GBind(..)) where

import GHC.Generics
import Data.Char

import Lang.Objects.Syntax

bindObject :: (ToBind a) => String -> a -> Bind
bindObject s a = let (GBind e t) = bind a in Bind s e t 

declareEnum :: String -> [String] -> Bind
declareEnum t v = Enum (fType t) (map fVal v)

instanceEnum :: String -> String -> GBind
instanceEnum v t = GBind (Val (VLit (fVal v))) (TyRigid (fType t))

fType :: String -> String
fType (x:xs) = (toUpper x) : xs
fType []     = []

fVal  :: String -> String
fVal  (x:xs) = (toLower x) : xs
fVal  []     = []

data GBind = GBind Exp Type
    deriving (Show)

class ToBind a where
    bind :: a -> GBind
    default bind :: (Generic a, GToBind (Rep a)) => a -> GBind
    bind a = gbind (from a)

instance ToBind Int where
    bind i = GBind (Val (VNum i)) TyNum

instance ToBind Bool where
    bind b = GBind (Val (VBool b)) TyBool

instance (GToBind fa, GToBind fb) => ToBind (fa a, fb b) where
    bind (a, b) = GBind (Prod ea eb) (TyProd ta tb)
        where 
            (GBind ea ta) = gbind a
            (GBind eb tb) = gbind b 

instance (ToBind a, ToBind b) => ToBind (a, b) where
    bind (a, b) = GBind (Prod ea eb) (TyProd ta tb)
        where 
            (GBind ea ta) = bind a
            (GBind eb tb) = bind b 

class GToBind f where
    gbind :: f a -> GBind

instance GToBind U1 where
    gbind _ = GBind (Val (VNum 0)) (TyNum)

instance (GToBind a, GToBind b) => GToBind (a :*: b) where
    gbind (a :*: b) = GBind (Prod ea eb) (TyProd ta tb)
        where 
            (GBind ea ta) = gbind a
            (GBind eb tb) = gbind b

instance (GToBind a, GToBind b) => GToBind (a :+: b) where
    gbind (L1 a) = GBind (SumL ea) (TySum ta (TyVar "r"))
        where (GBind ea ta) = gbind a
    gbind (R1 b) = GBind (SumR eb) (TySum (TyVar "l") tb)
        where (GBind eb tb) = gbind b

instance (GToBind a) => GToBind (M1 i c a) where
    gbind (M1 a) = gbind a

instance (ToBind a) => GToBind (K1 i a) where
    gbind (K1 a) = bind a

data TestType = Num Int | Something (Bool, Int)
    deriving (Generic, Show)
