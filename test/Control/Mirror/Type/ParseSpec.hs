module Control.Mirror.Type.ParseSpec where

import Control.Mirror.Type.Parse
import Control.Mirror.Type

import Text.Megaparsec (parseMaybe)

import Test.Hspec
import Data.Maybe (Maybe(Just), isJust)

spec = parallel $ do
  describe "The TypeExpr parser" $ do
    describe "Parses integers" $ do
      it "parses 0" $
        (parseMaybe typeExpr "0" `shouldBe` Just (SumTypeExpr zeroS))
      it "or 1" $
        (parseMaybe typeExpr "1" `shouldBe` Just (ProductTypeExpr oneP))
      xit "and others" $
        (parseMaybe typeExpr "2" `shouldBe` Just (SumTypeExpr (natToSum 2)))

    describe "Parses variables" $
      it "parses a lone variable" $
        (parseMaybe typeExpr "a") `shouldSatisfy` isJust
