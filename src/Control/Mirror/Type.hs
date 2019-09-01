{-# LANGUAGE MultiParamTypeClasses
            , TemplateHaskell
            , ScopedTypeVariables
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , NamedFieldPuns
            , GADTs
            , PatternSynonyms
            , TypeOperators
            , DeriveFunctor
            , GeneralizedNewtypeDeriving
            , DataKinds
            , RankNTypes
            , KindSignatures
            , DeriveGeneric
            , StandaloneDeriving
            , RebindableSyntax
            , InstanceSigs
   #-}

module Control.Mirror.Type where

import Unbound.Generics.LocallyNameless
import Data.Either (Either(..), either)
import GHC.Generics
import Data.List
import Data.String
import Numeric.Natural hiding ((+), (*), negate, (-), (/))
import Prelude hiding ((+), (*), negate, (-), (/))
import Data.Maybe (Maybe(..))
import Control.Arrow (second)

data Sign = Pos | Neg
  deriving (Read, Show, Ord, Eq, Enum, Bounded, Generic)
instance Alpha Sign

-- like Sign but for if a factor is > or < 1
-- A Sigil is either Gro[w] or Shr[ink]
data Sigil = Gro | Shr
  deriving (Read, Show, Ord, Eq, Enum, Bounded, Generic)
instance Alpha Sigil

type VarSet = [Name TypeExpr]

data Product where
  ProductVar :: Name TypeExpr -> Product
  ProductExpr :: [(Sigil, Sum)] -> Product
  deriving (Eq, Show, Generic)

data Sum where
  SumVar :: Name TypeExpr -> Sum
  SumExpr :: [(Sign, Product)] -> Sum
  deriving (Eq, Show, Generic)

type TypeExpr = Either Sum Product

newtype Poly = Poly (Bind VarSet TypeExpr)
  deriving (Show, Generic)

newtype Full = Full (Bind VarSet (TypeExpr, TypeExpr))
  deriving (Show, Generic)

instance Alpha Product
instance Alpha Sum
instance Alpha Poly
instance Alpha Full

instance forall t. Subst t Sign where
  isvar _ = Nothing

instance forall t. Subst t Sigil where
  isvar _ = Nothing

toProduct :: Sum -> Product
toProduct sum = ProductExpr [(Gro, sum)]
toSum :: Product -> Sum
toSum product = SumExpr [(Pos, product)]

normalizeSums :: Sum -> Sum
normalizeSums (SumExpr outer) =
  SumExpr . go $ second normalizeProducts <$> outer
  where
    go [] = []
    go ((Pos, ProductExpr [(Gro, SumExpr inner)]) : xs)
      = go (inner ++ xs)
    go (x:xs) = x : go xs

normalizeProducts :: Product -> Product
normalizeProducts (ProductExpr outer) =
  ProductExpr . go $ second normalizeSums <$> outer
  where
    go [] = []
    go ((Gro, SumExpr [(Pos, ProductExpr inner)]) : xs)
      = go (inner ++ xs)
    go (x:xs) = x : go xs

instance Subst TypeExpr Product where
  isCoerceVar (ProductVar v) =
      pure (SubstCoerce v (Just . either toProduct id))
  isCoerceVar _ = Nothing

instance Subst TypeExpr Sum where
  isCoerceVar (SumVar v) =
    pure (SubstCoerce v (Just . either id toSum))
  isCoerceVar _ = Nothing

class TypeBody v where
  var :: String -> v

instance TypeBody Sum where
  var :: String -> Sum
  var = SumVar . string2Name

instance TypeBody Product where
  var :: String -> Product
  var = ProductVar . string2Name

toVarSet :: [String] -> VarSet
toVarSet s = map string2Name s

full bindSet l r =
  Full $ bind (toVarSet bindSet) (l,r)
poly bindSet t =
  Poly $ bind (toVarSet bindSet) t

oneP :: Product
oneP = ProductExpr []

natToSum :: Natural -> Sum
natToSum n = SumExpr $ replicate (fromIntegral n) (Pos, oneP)

zeroS :: Sum
zeroS = SumExpr [] -- = natToSum 0