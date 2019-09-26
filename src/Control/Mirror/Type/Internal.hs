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
            , MultiWayIf
            , TypeFamilies
            , ScopedTypeVariables
   #-}

module Control.Mirror.Type.Internal where

import Unbound.Generics.LocallyNameless
import GHC.Generics
import Data.List
import Data.String
import Prelude hiding ((+), (*), negate, (-), (/))
import Data.Maybe (Maybe(..))
import Control.Arrow (second)
import Control.Monad (forM)

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

data TypeExpr = SumTypeExpr Sum | ProductTypeExpr Product
  deriving (Eq, Show, Generic)

onTypeExpr onSum onProduct expr =
  case expr of
    SumTypeExpr sum -> onSum sum
    ProductTypeExpr product -> onProduct product

newtype Poly = Poly (Bind VarSet TypeExpr)
  deriving (Show, Generic)

newtype Full = Full (Bind VarSet (TypeExpr, TypeExpr))
  deriving (Show, Generic)

instance Alpha Product
instance Alpha Sum
instance Alpha Poly
instance Alpha Full
instance Alpha TypeExpr

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
      pure (SubstCoerce v (Just . onTypeExpr toProduct id))
  isCoerceVar _ = Nothing

instance Subst TypeExpr Sum where
  isCoerceVar (SumVar v) =
    pure (SubstCoerce v (Just . onTypeExpr id toSum))
  isCoerceVar _ = Nothing

newtype Identifier =
  Identifier {unIdent :: String}
  deriving (Show, Eq, Generic)
identToName = string2Name . unIdent

instance Alpha Identifier where

class TypeBody v where
  varCons :: Name TypeExpr -> v
  type Piece v
  exprCons :: [Piece v] -> v
  toTypeExpr :: v -> TypeExpr
  foldNames :: Monad m => (Name TypeExpr -> m (Name TypeExpr)) -> v -> m v

var :: TypeBody v => Identifier -> v
var = varCons . identToName

foldNames'
  :: forall m v. (Monad m, TypeBody v)
  => (Name TypeExpr -> m (Name TypeExpr))
  -> Either [Piece v] (Name TypeExpr)
foldNames' f (Right v) = varCons <$> f v
-- (the recursion needs to be foldNames, not foldNames')
foldNames' f (Left l) = exprCons <$> forM l (second foldNames f)

instance TypeBody Sum where
  varCons :: Name TypeExpr -> Sum
  varCons = SumVar
  type Piece Sum = (Sign, Product)
  exprCons = SumExpr
  toTypeExpr = SumTypeExpr
  foldNames f (SumVar v) = foldNames' f (Right v)
  foldNames f (SumExpr l) = foldNames' f (Left l)

instance TypeBody Product where
  varCons :: Name TypeExpr -> Product
  varCons = ProductVar
  type Piece Product = (Sigil, Sum)
  exprCons = ProductExpr
  toTypeExpr = ProductTypeExpr
  foldNames f (ProductVar v) = foldNames' f (Right v)
  foldNames f (ProductExpr l) = foldNames' f (Left l)

toVarSet :: [Identifier] -> VarSet
toVarSet s = map identToName s

full bindSet l r =
  Full $ bind bindSet (l,r)
poly bindSet t =
  Poly $ bind bindSet t

oneP :: Product
oneP = ProductExpr []

natToSum :: Int -> Sum
natToSum n = SumExpr $ replicate (fromIntegral n') (sign, oneP)
  where
    n' = abs n
    sign =
      if | n > 0 -> Pos
         | n < 0 -> Neg
         | otherwise -> error "zero has no sign" -- should never get triggered

zeroS :: Sum
zeroS = SumExpr [] -- == natToSum 0