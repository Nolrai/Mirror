{-# LANGUAGE MultiParamTypeClasses
            , TemplateHaskell
            , ScopedTypeVariables
            , FlexibleInstances
            , FlexibleContexts
            , UndecidableInstances
            , NamedFieldPuns
            , GADTSyntax
            , PatternSynonyms
            , TypeOperators
            , DeriveFunctor
            , GeneralizedNewtypeDeriving
            , DeriveTraversable
   #-}

module Control.Mirror.Type (module Parse) where

import Unbound.LocallyNameless
import Data.Either (Either(..))
import Control.Mirror.Type.Parse as Parse

data Sign = Pos | Neg
-- like Sign but for if a factor is > or < 1
data Sigil = Gro | Shr

newtype Sum a = Sum [(Sign, a)]
  deriving (Functor, Traversable, Read, Show, Ord, Eq)
newtype Prod a = Prod [(Sigil, a)]
  deriving (Functor, Traversable, Read, Show, Ord, Eq)


newtype TYPE a = Atom a | PofS Prod (Sum (TYPE a))
  deriving (Functor, Traversable, Read, Show, Ord, Eq)

newtype Var = Var (Name TypeExp)
  deriving (Show, Ord, Eq, Alpha)

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
  isvar (Atom (Var v)) = Just $ SubstName v
  isvar _ = Nothing

var = Var . string2Name
fullType bindList l r =
  FullType $ bind (fmap string2Name bindlist) (l,r)
polyType bindList l r =
  PolyType $ bind (fmap string2Name bindlist) (l,r)

