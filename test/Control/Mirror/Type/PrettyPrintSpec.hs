{-# LANGUAGE ScopedTypeVariables #-}
module Control.Mirror.Type.PrettyPrintSpec where

import Control.Mirror.Type.Parse as Pa
import Control.Mirror.Type.Pretty as Pr
import Control.Mirror.Type.Internal as T
import Control.Mirror.Type.ParseSpec ()

import Text.Megaparsec (parseMaybe, parse)

import Test.Hspec
import Test.QuickCheck
import Data.Maybe (Maybe(Just), isJust)
import Numeric.Natural (Natural)

spec :: Spec
spec = do $
describe "The Pretty Printer" $ do
  roundTrip 