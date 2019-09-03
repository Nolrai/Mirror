module Control.Mirror.Type.Parse where

spec :: Spec
spec = do
  describe "The TypeExpr parser" $ do
    it "Parses integers" $ do
      describe "Like 0" $
        parseMaybe typeExpr "0" `shouldBe` Just (SumTypeExpr zeroS)
      describe "or 1" $
        parseMaybe typeExpr "1" `shouldBe` Just (ProductTypeExpr oneP)
      xdesribe "and others" $
        parseMaybe typeExpr "2" `shouldBe` Just (SumTypeExpr (natToSum 2))

    it "Parses variables"
      (parseMaybe typeExpr "a") `shouldSatisfy` isJust