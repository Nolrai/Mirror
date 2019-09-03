{-# LANGUAGE MultiParamTypeClasses
           , TemplateHaskell
           , ScopedTypeVariables
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
  #-}

module Control.Mirror.Type.Parse where
import Control.Mirror.Type

import Data.Text

import Control.Applicative hiding (many, some)
import Control.Arrow ((+++))
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (Doc, (<+>))

type Parser = Parsec () Text
type PError = ParseErrorBundle Text ()

-- A sum is composed of terms (or is a variable)
term :: Parser (Sign, Product)
term = (,) <$> sign <*> product

-- A product is composed of factors (or is a variable)
factor :: Parser (Sigil, Sum)
factor = (,) <$> sigil <*> sum

sign :: Parser Sign
sigil :: Parser Sigil
sign  = (Pos <$ symbol "+") <|> (Neg <$ symbol "-")
sigil = (Grow <$ symbol "*") <|> (Neg <$ symbol "%")

identifier :: Parser Str
identifier = lexeme ( (:) <$> letterChar <*> many alphaNumChar)

expr' :: TypeBody b => String -> Parser a -> ([(c, d)] -> b) -> Parser b
expr' empty item constr = var identifier <|>
  (constr <$> ((symbol empty $> []) <|> ((\x -> [x]) <$> item) <|> parens (some item)))

product :: Parser Product
product = expr' "1" factor ProductExpr

sum :: Parser Sum
sum     = expr' "0" term SumExpr

typeExpr :: Parser TypeExpr
typeExpr = fmap SumTypeExpr sum <|> fmap ProductTypeExpr product

--- helpers --

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parens :: String -> Parser String
symbol = L.symbol whiteSpace

whiteSpace :: Parser String
whiteSpace =
  space1
    (skipLineComment (string "##"))
    (skipBlockCommentNested (string "(#") (string "#)"))

class Pretty p where
  print :: (Applicative m, Fresh m) => p -> m Doc

instance Pretty' Sum where
  print (SumVar v) = pure . PP.text . show $ x
  print (SumExpr []) = pure $ PP.text "0"
  print (SumExpr [x]) = print x
  print (SumExpr l) = PP.parens <$> (foldr1 (<+>) (map print l))

instance Pretty' Product where
  print (ProductVar v) = pure . PP.text . show $ x
  print (ProductExpr []) = pure $ PP.text "1"
  print (ProductExpr [x]) = print x
  print (ProductExpr l) = PP.parens <$> (foldr1 (<+>) (map print l))

instance Pretty' (Sign, Product) where
  print (sign, product) = (<+>) <$> print sign <*> print product

instance Pretty' (Sigil, Sum) where
  print (sigil, sum) = (<+>) <$> print sigil <*> print sum

instance Pretty' Sign where
  print Pos = PP.text "+"
  print Neg = PP.text "-"

instance Pretty' Sigil where
  print Gro = PP.text "*"
  print Shr = PP.text "%"

