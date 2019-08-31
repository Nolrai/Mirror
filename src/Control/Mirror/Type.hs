{-# LANGUAGE MultiParamTypeClasses
            , TemplateHaskell
            , ScopedTypeVariables
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , NamedFieldPuns
            , GADTSyntax
            , PatternSynonym
   #-}

module Control.Mirror.Type (module Parse) where

import Unbound.LocallyNameless
import Data.Either (Either(..))
import Data.Fix
import Control.Mirror.Type.Parse as Parse

data Sign = Pos | Neg
-- like Sign but for if a factor is > or < 1
data Sigil = Gro | Shr

newtype Sum a = Sum [(Sign, a)]
  deriving (Functor, Traversable, Read, Show, Ord, Eq)
newtype Prod a = Prod [(Sigil, a)] deriving functor
  deriving (Functor, Traversable, Read, Show, Ord, Eq)

newtype O g f where
  MkO :: a -> f (g a)

newtype Layer a = Layer (Prod (Sum a)) deriving functor
  deriving (Functor, Traversable, Read, Show, Ord, Eq)
newtype TYPE a = Layers (Fix (Either a `O` Layer))
  deriving (Functor, Traversable, Read, Show, Ord, Eq)

newtype Var = Var (Name TypeExp)
  deriving (Functor, Traversable, Read, Show, Ord, Eq, Alpha)

newtype TypeExp = TypeExp (TYPE Var)
newtype PolyType = PolyType (Bind [Var] TypeExp)
newtype FullType = FullType (Bind [Var] (TypeExp, TypeExp))
data Node a = Node a FullType

$(derive [''Var, ''TypeExp, ''PolyType, ''FullType, ''Node])

instance Alpha TypeExp
instance Alpha PolyType
instance Alpha FullType
instance Alpha Node

instance Subst TypeExp TypeExp where
  isvar = f . unfix
    where
    f (Left (Var v)) = Just (SubstName v)
    f _ = Nothing

var = Var . string2Name
fullType bindList l r =
  FullType $ bind (fmap string2Name bindlist) (l,r)
polyType bindList l r =
  PolyType $ bind (fmap string2Name bindlist) (l,r)

