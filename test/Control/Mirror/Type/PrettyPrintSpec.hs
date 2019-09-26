{-# LANGUAGE ScopedTypeVariables
           , KindSignatures
           , NoStarIsType
           , GADTs
           , FlexibleInstances
           , FlexibleContexts
           , LambdaCase
#-}
module Control.Mirror.Type.PrettyPrintSpec where

import Control.Mirror.Type.Parse as Pa
import Control.Mirror.Type.PrettyPrint as Pr
import Control.Mirror.Type.Internal as T
import Control.Mirror.Type.ParseSpec ()
import Control.Lens.Fold as F

import Text.Megaparsec
import Unbound.Generics.LocallyNameless

import Test.Hspec
import Test.QuickCheck
import Data.Maybe (Maybe(Just), isJust)
import Numeric.Natural (Natural)

data SomeParse where
  SomeParse :: forall a
    . (Pretty a, Alpha a, Arbitrary a)
    => Parser a -> String -> String -> SomeParse

allTheFunctions :: [ SomeParse ]
allTheFunctions =
  [ SomeParse Pa.sign "sign" "Sign"
  , SomeParse Pa.sigil "sigil" "Sigil"
  , SomeParse Pa.term "term" "(Sign, Product)"
  , SomeParse Pa.factor "factor" "(Sigil, Sum)"
  , SomeParse Pa.identifier "identifier" "String"
  , SomeParse Pa.sum "sum" "Sum"
  , SomeParse Pa.product "product" "Product"
  , SomeParse Pa.typeExpr "typeExpr" "TypeExpr"
  , SomeParse Pa.polyParser "polyParser" "Poly"
  , SomeParse Pa.fullParser "fullParser" "Full"
  ]

instance Arbitrary Sign where
  arbitrary = oneof $ pure <$> [Pos, Neg]
  shrink Neg = [Pos]
  shrink Pos = []

instance Arbitrary Sigil where
  arbitrary = oneof $ pure <$> [Gro, Shr]
  shrink Shr = [Gro]
  shrink Gro = []

-- This is ment to generate the same few values
-- when in a small expresion by using growingElements
instance Arbitrary (Name a) where
  arbitrary = oneof $ pure `map` [makeName [c] n | c <- "abcxyz", n <- [0]]
  shrink = genericShrink

arbitrary_body :: (Arbitrary (Piece b), TypeBody b) => Gen b
arbitrary_body =
  scale (max 0) . sized $
    \case
    0 -> pure $ exprCons []
    1 -> oneof [varCons <$> arbitrary, terms]
    _ -> terms
    where
    terms :: (Arbitrary (Piece b), TypeBody b) => Gen b
    terms = exprCons <$> sized sizedTerms
    sizedTerms size =
      do
      p <- partition size
      mapM (\s -> resize (s-1) arbitrary) p
    partition :: Int -> Gen [Int]
    partition n
      | n <= 0 = pure []
      | otherwise =
        do
        m <- oneof $ pure `map` [1..n]
        (m:) <$> partition (n - m)

instance Arbitrary Sum where
  arbitrary = arbitrary_body
  shrink = genericShrink

instance Arbitrary Product where
  arbitrary = arbitrary_body
  shrink = genericShrink

instance Arbitrary TypeExpr where
  arbitrary = oneof
    [ SumTypeExpr <$> (regenNames <$> arbitrary)
    , ProductTypeExpr <$> (regenNames <$> arbitrary)
    ]
  shrink = genericShrink

regenNames 

instance Arbitrary Full where
  arbitrary = Full <$> arbitrary
  shrink = genericShrink

instance (Arbitrary expr, Alpha expr) => Arbitrary (Bind VarSet expr) where
  arbitrary =
    do
    expr <- arbitrary
    v <- shuffle =<< sublistOf (toListOf fv expr)
    pure $ bind v expr

spec :: Spec
spec = describe "The Pretty Printer" $ do
  mapM_ roundTrip allTheFunctions

roundTrip :: SomeParse -> Spec
roundTrip (SomeParse parser parserName typeName) =
  it message . property $
      \ x ->
        case parse parser "Pretty Print" (show . runFreshPrint $ x) of
          Left bundle -> error $ show bundle
          Right y -> y `shouldSatisfy` (aeq x)
  where
  message = firstPart ++ secondPart
  firstPart = "of " ++ typeName ++ " produces"
  secondPart = " a string " ++ parserName ++ " can parse."