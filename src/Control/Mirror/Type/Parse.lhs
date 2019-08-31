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
> import Control.Monad.Trans.Error
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

from

fromRational :: Rational -> TYPE
fromRational (a :% b) = Prod [(Gro,
