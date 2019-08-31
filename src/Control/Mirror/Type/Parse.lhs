> {-# LANGUAGE MultiParamTypeClasses
>            , TemplateHaskell
>            , ScopedTypeVariables
>            , FlexibleInstances
>            , FlexibleContexts
>            , UndecidableInstances
>   #-}

> module Control.Mirror.Type.Parse where

> import Control.Applicative
> import Control.Arrow ((+++))
> import Control.Monad
> import Control.Monad.Trans.Maybe
> import Control.Monad.Trans.Except
>
> import Text.Parsec hiding ((<|>), Empty)
> import qualified Text.Parsec.Token as P
> import Text.Parsec.Language (haskellDef)
>
> import qualified Text.PrettyPrint as PP
> import Text.PrettyPrint (Doc, (<+>))

zeroS :: Sum a
zeroS = Sum []

oneP :: Prod a
oneP = Prod []

intToSum :: Int -> TYPE a
intToSum n =
  Fix (Right (Prod [(Gro, Sum (f n))]))
  where
  f n | n >= 0 = replicate n (Pos, oneP)
      | otherwise = replicate n (Neg, oneP)

fromRational :: Rational -> TYPE a
fromRational (a :% b) = Prod [(Gro,
