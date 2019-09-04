{-# LANGUAGE ScopedTypeVariables #-}
module Control.Mirror.Type.ParseSpec where

import Control.Mirror.Type.Parse as P
import Control.Mirror.Type

import Text.Megaparsec (parseMaybe, parse)

import Test.Hspec
import Test.QuickCheck
import Data.Maybe (Maybe(Just), isJust)
import Numeric.Natural (Natural)

spec = parallel $ do
  describe "The TypeExpr parser" $ do
    it "Parses integers" . property $
        \(x' :: Int) -> let x = abs x' in
          parseMaybe typeExpr (show x)
          `shouldBe` Just (SumTypeExpr (natToSum x))

    describe "Parses variables" $
      it "parses a lone variable" . property $
        \ s ->
          (parse typeExpr "test" (unIdent s))
            `shouldBe` Right (SumTypeExpr . var $ unIdent s)

newtype Identifier = Identifier {unIdent :: String}
  deriving (Eq, Ord, Read, Show)

genLetter = oneof [choose ('a', 'z') <|>  choose ('A', 'Z')]
genDigit = choose ('0','9')

instance Arbitrary Identifier where
  arbitrary =
    (\ x xs -> Identifier (x : xs))
      <$> genLetter
      <*> listOf (genLetter <|> genDigit)

-- don't use nested.
-- (because a <|> b <|> c will produce strongly biased results,
-- with a twice as likely as b or c)
a <|> b = oneof [a,b]