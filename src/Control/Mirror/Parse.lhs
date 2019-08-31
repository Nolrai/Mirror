> {-# LANGUAGE MultiParamTypeClasses
>            , TemplateHaskell
>            , ScopedTypeVariables
>            , FlexibleInstances
>            , FlexibleContexts
>            , UndecidableInstances
>   #-}

> module Control.Mirror.Parse where

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