{-# LANGUAGE ScopedTypeVariables
           , KindSignatures
           , NoStarIsType
           , GADTs
#-}
module Control.Mirror.Type.PrettyPrintSpec where

import Control.Mirror.Type.Parse as Pa
import Control.Mirror.Type.PrettyPrint as Pr
import Control.Mirror.Type.Internal as T
import Control.Mirror.Type.ParseSpec ()

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
  , SomeParse Pa.factor "term" "(Sigil, Sum)"
  , SomeParse Pa.identifier "identifier" "String"
  , SomeParse Pa.sum "sum" "Sum"
  , SomeParse Pa.product "product" "Product"
  , SomeParse Pa.typeExpr "typeExpr" "TypeExpr"
  , SomeParse Pa.polyParser "polyParser" "Poly"
  , SomeParse Pa.fullParser "fullParser" "Full"
  ]

instance Arbitrary Sign where
  arbitrary = pure Pos <|> pure Neg
  shrink Neg = [Pos]
  shrink Pos = []

instance Arbitrary Sigil where
  arbitrary = pure Gro <|> pure Shr
  shrink Shr = [Gro]
  shrink Gro = []

instance Arbitrary Sum where
  shrink = genericShrink

instance Arbitrary Product where
  shrink = genericShrink

instance Arbitrary TypeExpr where
  shrink = genericShrink

instance Arbitrary Poly where
  shrink = genericShrink

instance Arbitrary Full where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance (Arbitrary v, Arbitrary expr) => Arbitrary (Bind v expr) where
  arbitrary = (\ expr -> bind (fvAny (\x->[x]) expr) expr) <$> arbitrary

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