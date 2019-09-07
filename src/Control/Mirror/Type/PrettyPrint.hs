{-# LANGUAGE TypeSynonymInstances
           , FlexibleInstances
#-}

module Control.Mirror.Type.PrettyPrint where

import Control.Mirror.Type.Internal

import Unbound.Generics.LocallyNameless
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (Doc, (<+>))

import qualified Data.Foldable as F

runFreshPrint :: Pretty p => p -> Doc
runFreshPrint = runLFreshM . pprint

-- Pretty printing of type expresions
class Pretty p where
  pprint :: (Applicative m, LFresh m) => p -> m Doc

instance Pretty Identifier where
  pprint = pure . PP.text . unIdent

instance Pretty Sum where
  pprint (SumVar v) = pure . PP.text . show $ v
  pprint (SumExpr []) = pure $ PP.text "0"
  pprint (SumExpr [x]) = pprint x
  pprint (SumExpr l) =
    PP.parens . F.foldr1 (<+>) <$> sequenceA (pprint <$> l)

instance Pretty Product where
  pprint (ProductVar v) = pure . PP.text . show $ v
  pprint (ProductExpr []) = pure $ PP.text "1"
  pprint (ProductExpr [x]) = pprint x
  pprint (ProductExpr l) =
    PP.parens . F.foldr1 (<+>) <$> sequenceA (pprint <$> l)

instance Pretty (Sign, Product) where
  pprint (sign, product) = (<+>) <$> pprint sign <*> pprint product

instance Pretty (Sigil, Sum) where
  pprint (sigil, sum) = (<+>) <$> pprint sigil <*> pprint sum

instance Pretty Sign where
  pprint Pos = pure $ PP.text "+"
  pprint Neg = pure $ PP.text "-"

instance Pretty Sigil where
  pprint Gro = pure $ PP.text "*"
  pprint Shr = pure $ PP.text "%"

instance Pretty TypeExpr where
  pprint = onTypeExpr pprint pprint

instance Pretty Poly where
  pprint (Poly p) =
    lunbind p $
    \ (bindings, typeExpr) ->
      abSep
        (PP.text "=>")
        (pprint bindings)
        (pprint typeExpr)

instance Pretty Full where
  pprint (Full f) =
    lunbind f $
    \ (bindings, (typeExprIn, typeExprOut)) ->
      abSep
        (PP.text "=>")
        (pprint bindings)
        $ abSep (PP.text "->")
          (pprint typeExprIn)
          (pprint typeExprOut)

instance Pretty VarSet where
  pprint l = pprintListSep (PP.text ",") (pprint <$> l)

pprintListSep :: (LFresh m) => Doc -> [m Doc] -> m Doc
pprintListSep sep l = foldr (abSep sep) (pure PP.empty) l

abSep :: Applicative m => Doc -> m Doc -> m Doc -> m Doc
abSep sep a b = (\x y -> x <+> sep <+> y) <$> a <*> b

instance Pretty (Name a) where
  pprint = pure . PP.text . show