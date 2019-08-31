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
  deriving (Read, Show, Ord, Eq, Enum, Bounded)
instance Alpha Sign

-- like Sign but for if a factor is > or < 1
-- A Sigil is either Gro[w] or Shr[ink]
data Sigil = Gro | Shr
  deriving (Read, Show, Ord, Eq, Enum, Bounded)
instance Alpha Sigil

type Sum a = [(Sign, a)]

type Prod a = [(Sigil, a)]

type Var = Name Exp

data Exp where
  Atom :: Var -> Exp
  PofS :: Prod (Sum Exp) -> Exp
  deriving (Show, Ord, Eq)

newtype Poly = Poly (Bind [Var] Exp)

newtype Full = Full (Bind [Var] (Exp, Exp))

data Node a = Node a Full

$(derive [''Sigil, ''Sign, ''Poly, ''Full, ''Node, ''Exp])

instance Alpha Exp
instance Alpha Poly
instance Alpha Full
instance Alpha a => Alpha (Node a)

instance Subst Exp Sign where
  isvar _ = Nothing

instance Subst Exp Sigil where
  isvar _ = Nothing

instance Subst Exp Exp where
  isvar (Atom v) = Just $ SubstName v
  isvar _ = Nothing


var :: String -> Exp
var = Atom . string2Name
full bindList l r =
  Full $ bind (fmap string2Name bindList) (l,r)
poly bindList t =
  Poly $ bind (fmap string2Name bindList) t

