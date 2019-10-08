{-# LANGUAGE TypeSynonymInstances
           , FlexibleInstances
           , OverloadedStrings
           , IncoherentInstances
#-}

module Control.Mirror.Type.PrettyPrint where

import Control.Mirror.Type.Internal

import Unbound.Generics.LocallyNameless
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (Doc, (<+>), punctuate, hcat)
import Data.String(IsString(..))
import Control.Applicative

import qualified Data.Foldable as F

runFreshPrint :: Pretty p => p -> Doc
runFreshPrint = runLFreshM . pprint

instance (IsString s, Applicative m) => IsString (m s) where
  fromString = pure . fromString

-- Pretty printing of type expresions
class Pretty p where
  pprint :: (Applicative m, LFresh m) => p -> m Doc

instance Pretty Sum where
  pprint (SumVar v) = pprint v
  pprint (SumExpr []) = "0"
  pprint (SumExpr [x]) = pprint x
  pprint (SumExpr l) =
    PP.parens . F.foldr1 (<+>) <$> sequenceA (pprint <$> l)

instance Pretty Product where
  pprint (ProductVar v) = pprint v
  pprint (ProductExpr []) = "1"
  pprint (ProductExpr [x]) = pprint x
  pprint (ProductExpr l) =
    PP.parens . F.foldr1 (<+>) <$> sequenceA (pprint <$> l)

instance Pretty (Sign, Product) where
  pprint (sign, product) = (<+>) <$> pprint sign <*> pprint product

instance Pretty (Sigil, Sum) where
  pprint (sigil, sum) = (<+>) <$> pprint sigil <*> pprint sum

instance Pretty Sign where
  pprint Pos = "+"
  pprint Neg = "-"

instance Pretty Sigil where
  pprint Gro = "*"
  pprint Shr = "%"

instance Pretty TypeExpr where
  pprint = onTypeExpr pprint pprint

instance Pretty Poly where
  pprint (Poly p) =
    lunbind p $
    \ (bindings, typeExpr) ->
      abSep
        "=>"
        (pprint bindings)
        (pprint typeExpr)

instance Pretty Full where
  pprint (Full f) =
    lunbind f $
    \ (bindings, (typeExprIn, typeExprOut)) ->
      abSep
        (PP.text suchThat)
        (pprint bindings)
        $ abSep (PP.text to)
          (pprint typeExprIn)
          (pprint typeExprOut)

instance Pretty VarSet where
  pprint l = PP.brackets <$> sepP "," (pprint <$> l)

sepP :: (LFresh m) => Doc -> [m Doc] -> m Doc
sepP p actionList = (hcat . punctuate p) <$> sequence actionList

abSep :: Applicative m => Doc -> m Doc -> m Doc -> m Doc
abSep p = liftA2 (\ a b -> a <+> p <+> b)

instance Pretty (Name a) where
  pprint n = pure
    $ (PP.text $ name2String n)
    <> "_"
    <> (PP.integer $ name2Integer n)