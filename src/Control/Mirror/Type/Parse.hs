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

module Control.Mirror.Type.Parse
(sign, sigil, term, factor, name, product, typeExpr, varSet, polyParser, fullParser) where
import Control.Mirror.Type.Internal hiding (poly, full)
import qualified Control.Mirror.Type.Internal as Type

import Unbound.Generics.LocallyNameless (Name, makeName)

import Prelude hiding (sum, product)

import Data.Functor (($>))
import Control.Applicative hiding (many, some)

import Text.Megaparsec as M
import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

-- These are just the defaults for now.
-- The () is where custom error info when we have some.
type Parser = Parsec () String
type PError = ParseErrorBundle String ()

sign :: Parser Sign
sigil :: Parser Sigil
sign  = (Pos <$ symbol "+") <|> (Neg <$ symbol "-") <?> "Sign"
sigil = (Gro <$ symbol "*") <|> (Shr <$ symbol "%") <?> "Sigil"

-- A sum is composed of terms (or is a variable)
term :: Parser (Sign, Product)
term = (,) <$> sign <*> product <?> "term"

-- A product is composed of factors (or is a variable)
factor :: Parser (Sigil, Sum)
factor = (,) <$> sigil <*> sum <?> "factor"

name :: forall a. Parser (Name a)
name = lexeme $ makeName <$> prefix <*> option 0 sufix
  where
    prefix :: Parser String
    prefix = some letterChar
    sufix :: Parser Integer
    sufix = char '_' *> L.decimal

product :: Parser Product
sum :: Parser Sum
product = expr' "1" factor ProductExpr
sum     = (natToSum <$> natural) <|> expr' "0" term SumExpr
  where
  natural = fromIntegral <$> lexeme L.decimal

typeExpr :: Parser TypeExpr
typeExpr =
  normalizeTypeExpr
  <$> (fmap SumTypeExpr (try sum) <|> fmap ProductTypeExpr (try product))
    <?> "Type Expresion"

varSet :: Parser VarSet
varSet = toVarSet <$> squares (name `sepBy` optional (char ','))

polyParser :: Parser Poly
polyParser =
  Type.poly
    <$> varSet
    <* symbol suchThat
    <*> typeExpr

fullParser :: Parser Full
fullParser =
  Type.full
    <$> varSet
    <* symbol suchThat
    <*> typeExpr
    <* symbol to
    <*> typeExpr

--- helpers --

expr' :: TypeBody b
  => String -> Parser (c,d) -> ([(c, d)] -> b) -> Parser b
-- A type expresion is either:
expr' empty item constr =
  (var <$> name) -- a variable
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