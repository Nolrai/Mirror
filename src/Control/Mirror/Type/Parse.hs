{-# LANGUAGE MultiParamTypeClasses
           , TemplateHaskell
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , NoImplicitPrelude
           , OverloadedStrings
           , GeneralizedNewtypeDeriving
  #-}

module Control.Mirror.Type.Parse where
import Control.Mirror.Type.Internal hiding (poly, full)
import qualified Control.Mirror.Type.Internal as Type

import Prelude hiding (sum, product)

import Data.Text as T

import Data.Functor (($>))
import Control.Applicative hiding (many, some)
import Control.Arrow ((+++))
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import qualified Data.Foldable as F
import Numeric.Natural (Natural)

import Text.Megaparsec as M
import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Unbound.Generics.LocallyNameless.Fresh as Fr
import Unbound.Generics.LocallyNameless.LFresh as LFr

-- These are just the defaults for now.
-- The () is where custom error info when we have some.
type Parser = Parsec () String
type PError = ParseErrorBundle String ()

sign :: Parser Sign
sigil :: Parser Sigil
sign  = (Pos <$ symbol "+") <|> (Neg <$ symbol "-")
sigil = (Gro <$ symbol "*") <|> (Shr <$ symbol "%")

-- A sum is composed of terms (or is a variable)
term :: Parser (Sign, Product)
term = (,) <$> sign <*> product

-- A product is composed of factors (or is a variable)
factor :: Parser (Sigil, Sum)
factor = (,) <$> sigil <*> sum

identifier :: Parser Identifier
identifier = Identifier <$> lexeme ( (:) <$> letterChar <*> many alphaNumChar)

product :: Parser Product
sum :: Parser Sum
product = expr' "1" factor ProductExpr
sum     = (natToSum <$> natural) <|> expr' "0" term SumExpr
  where
  natural = fromIntegral <$> lexeme L.decimal

typeExpr :: Parser TypeExpr
typeExpr = fmap SumTypeExpr sum <|> fmap ProductTypeExpr product

varSet :: Parser VarSet
varSet = toVarSet <$> squares (identifier `sepBy` optional (char ','))

polyParser :: Parser Poly
polyParser = Type.poly <$> varSet  <*> typeExpr

fullParser :: Parser Full
fullParser =
  Type.full
    <$> varSet
    <*> typeExpr
    <* symbol "~>"
    <*> typeExpr

--- helpers --

expr' :: TypeBody b
  => String -> Parser (c,d) -> ([(c, d)] -> b) -> Parser b
-- A type expresion is either:
expr' empty item constr =
  (var <$> identifier) -- a variable
  <|> (constr <$> -- Or a body
        ( (symbol empty $> []) -- which in turn is either empty
        <|> ((\x -> [x]) <$> item) -- one bare item
        <|> parens (some item) -- or is parentheses around at least 1 items.
        )
      )

lexeme = L.lexeme whiteSpace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

squares = between (symbol "[") (symbol "]")

symbol :: String -> Parser String
symbol = L.symbol whiteSpace

whiteSpace :: Parser ()
whiteSpace =
  L.space
    space1
    (L.skipLineComment "##")
    (L.skipBlockCommentNested "(#" "#)")