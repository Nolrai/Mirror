{-# LANGUAGE MultiParamTypeClasses
           , TemplateHaskell
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , NoImplicitPrelude
           , OverloadedStrings
  #-}

module Control.Mirror.Type.Parse where
import Control.Mirror.Type

import Prelude hiding (sum, product)

import Data.Text as T

import Data.Functor (($>))
import Control.Applicative hiding (many, some)
import Control.Arrow ((+++))
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import qualified Data.Foldable as F

import Text.Megaparsec
import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (Doc, (<+>))

import Unbound.Generics.LocallyNameless.Fresh as Fr
import Unbound.Generics.LocallyNameless.LFresh as LFr

-- These are just the defaults for now. The () is where to put custom error info
type Parser = Parsec () String
type PError = ParseErrorBundle String ()

-- A sum is composed of terms (or is a variable)
term :: Parser (Sign, Product)
term = (,) <$> sign <*> product

-- A product is composed of factors (or is a variable)
factor :: Parser (Sigil, Sum)
factor = (,) <$> sigil <*> sum

sign :: Parser Sign
sigil :: Parser Sigil
sign  = (Pos <$ symbol "+") <|> (Neg <$ symbol "-")
sigil = (Gro <$ symbol "*") <|> (Shr <$ symbol "%")

identifier :: Parser String
identifier = lexeme ( (:) <$> letterChar <*> many (char ';'))

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

product :: Parser Product
sum :: Parser Sum
product = expr' "1" factor ProductExpr
sum     = expr' "0" term SumExpr

typeExpr :: Parser TypeExpr
typeExpr = fmap SumTypeExpr sum <|> fmap ProductTypeExpr product

--- helpers --

lexeme = L.lexeme whiteSpace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

symbol :: String -> Parser String
symbol = L.symbol whiteSpace

whiteSpace :: Parser ()
whiteSpace =
  L.space
    space1
    (L.skipLineComment "##")
    (L.skipBlockCommentNested "(#" "#)")

-- Pretty printing. I put this in the same module because they really can't be tested seperately.

class Pretty p where
  pprint :: (Applicative m, Fresh m) => p -> m Doc

instance Pretty Sum where
  pprint (SumVar v) = pure . PP.text . show $ v
  pprint (SumExpr []) = pure $ PP.text "0"
  pprint (SumExpr [x]) = pprint x
  pprint (SumExpr l) =
    PP.parens . F.foldr1 (<+>) <$> sequenceA (pprint <$> l)

instance Pretty Product where
  pprint (ProductVar v) = pure . PP.text . show $ v
  pprint (ProductExpr []) = pure $ PP.text "1"
  pprint (ProductExpr [x]) = pprint x
  pprint (ProductExpr l) =
    PP.parens . F.foldr1 (<+>) <$> sequenceA (pprint <$> l)

instance Pretty (Sign, Product) where
  pprint (sign, product) = (<+>) <$> pprint sign <*> pprint product

instance Pretty (Sigil, Sum) where
  pprint (sigil, sum) = (<+>) <$> pprint sigil <*> pprint sum

instance Pretty Sign where
  pprint Pos = pure $ PP.text "+"
  pprint Neg = pure $ PP.text "-"

instance Pretty Sigil where
  pprint Gro = pure $ PP.text "*"
  pprint Shr = pure $ PP.text "%"