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

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (Doc, (<+>))

type Parser = Parsec () Text
type PError = ParseErrorBundle Text ()

sumExpr :: Parser Sum
sumExpr = error "stub"