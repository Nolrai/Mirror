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
import Control.Lens.Operators ((??))

import Text.Megaparsec
import Unbound.Generics.LocallyNameless

import Test.Hspec
import Test.QuickCheck

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

arbitrary_body :: (Arbitrary (Piece b), TypeBody b) => Gen b
arbitrary_body =
  scale (max 0) . sized $
    \case
    0 -> pure $ exprCons []
    1 -> oneof [var <$> arbitrary, terms]
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
  arbitrary = normalizeTypeExpr <$> oneof
    [ SumTypeExpr <$> (regenNames =<< arbitrary)
    , ProductTypeExpr <$> (regenNames =<< arbitrary)
    ]
  shrink x = normalizeTypeExpr <$> genericShrink x

regenNames :: TypeBody b => b -> Gen b
regenNames = foldNames go
  where
  go n = makeName (name2String n) <$> sized (\s -> choose (0, fromIntegral s `div` 5))

instance Arbitrary Poly where
  arbitrary = Poly <$> arbitrary
  shrink = genericShrink

instance Arbitrary Full where
  arbitrary = Full <$> arbitrary
  shrink = genericShrink

instance (Arbitrary expr, Alpha expr) => Arbitrary (Bind VarSet expr) where
  arbitrary =
    do
    expr <- arbitrary
    v <- sublistOf (toListOf fv expr)
    bind <$> shuffle v ?? expr -- from lens: (??) :: f (a -> b) -> a -> f b

spec :: Spec
spec = do
  describe "normalizeTypeExpr" $ do
    idempotent normalizeTypeExpr
  describe "The TypeExpr arbitrary" $ do
    it "should be normalized" . property $
      \ x ->
        do
        let y = normalizeTypeExpr x
        x `shouldBe` y

  describe "The Pretty Printer" $ do
    roundTripTypeExpr
  describe "The Pretty Printer" $ do
    mapM_ roundTrip allTheFunctions

idempotent f = it "should be idempotent" . property
  $ \x -> f (f x) `shouldBe` f x

roundTrip :: SomeParse -> Spec
roundTrip (SomeParse parser parserName typeName) =
  it message . property $
      \ x ->
        let showFresh = show . runFreshPrint in
        case parse parser "Pretty Print" (showFresh x) of
          Left bundle -> error $ errorBundlePretty bundle
          Right y -> y `shouldSatisfy` (aeq x)
  where
  message = firstPart ++ secondPart
  firstPart = "of " ++ typeName ++ " produces"
  secondPart = " a string " ++ parserName ++ " can parse."

roundTripTypeExpr =
  it message . property $
      \ x ->
        let showFresh = show . runFreshPrint in
        case parse typeExpr "Pretty Print" (showFresh x) of
          Left bundle -> error $ errorBundlePretty bundle
          Right y -> showFresh y `shouldBe` showFresh x
  where
  message = firstPart ++ secondPart
  firstPart = "of TypeExpr produces"
  secondPart = " a string typeExpr can parse."

instance ShowErrorComponent () where
  showErrorComponent = const ""
  errorComponentLen = const 0