{-# LANGUAGE ScopedTypeVariables #-}
module Control.Mirror.Type.ParseSpec where

import Control.Mirror.Type.Parse as P
import Control.Mirror.Type.Internal as T
import Unbound.Generics.LocallyNameless
  (Name, makeName, name2String, name2Integer)

import Text.Megaparsec (parseMaybe, parse)

import Test.Hspec
import Test.QuickCheck
import Data.Maybe (Maybe(Just))

spec = parallel $ do
  describe "The TypeExpr parser" $ do
    it "Parses integers" . property $
        \(x' :: Int) -> let x = abs x' in
          parseMaybe typeExpr (show x)
          `shouldBe` Just (normalizeTypeExpr (SumTypeExpr (natToSum x)))

    describe "Parses variables" $
      it "parses a lone variable" . property $
        \ s ->
          (parse typeExpr "test" (show s))
            `shouldBe` Right (SumTypeExpr . var $ s)

    describe "Names" $
      it "should have a letter first" . property $
          \ (s :: Name TypeExpr) -> (head $ show s) `shouldNotBe` '_'

instance Arbitrary (Name a) where
  arbitrary = oneof $ pure `map` [makeName [c] 0 | c <- abcxyz]
  shrink n = makeName (name2String n) <$> init [0..(name2Integer n)]

abcxyz = "a"