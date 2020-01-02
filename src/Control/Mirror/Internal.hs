{-# LANGUAGE DataKinds
           , GADTs
           , NoStarIsType
           , PatternSynonyms
           , TupleSections
           , ScopedTypeVariables
           , FlexibleContexts
  #-}
module Control.Mirror.Internal where
import Control.Mirror.Type.Internal as Type
-- import Control.Mirror.Type.PrettyPrint as Type.PrettyPrint
-- import Control.Mirror.Type.Parse as Type.Parse
import Unbound.Generics.LocallyNameless
import Control.Monad.Except

data Flavor = Add | Mul deriving (Show, Read, Eq, Ord)
data Hand = L | R deriving (Show, Eq, Read, Ord)
data Dir = SpreadLeft | SpreadRight deriving (Show, Eq, Read, Ord)

pattern CollectRight = SpreadLeft
pattern CollectLeft = SpreadRight

data BaseShape = Cup Poly | Split Poly Poly deriving (Show)
data Shape = BaseShape BaseShape Dir | Swap Poly Poly deriving (Show)

data Atom = Shape Shape Flavor | Dist Hand Dir deriving (Show)

data Circut where
  Atom :: Atom -> Circut
  Deeper :: Bind [(Name TypeExpr, Circut)] (TypeExpr, [(Name TypeExpr, Circut)]) -> Circut

data TypeError = InvalidSplit Flavor TypeExpr TypeExpr deriving (Show)

-- assign an atom's type
atomToFull :: (LFresh m, MonadError TypeError m) => Atom -> m Type.Full

atomToFull (Shape (BaseShape (Cup (Poly poly')) dir) flavor) =
  lunbind poly' (cupToFull dir flavor)

atomToFull (Shape (BaseShape (Split (Poly top) (Poly bot)) dir) flavor) =
  lunbind top
    (\(topPat, topBody) ->
      lunbind bot
        (\(botPat, botBody) ->
          splitToFull dir flavor topPat botPat topBody botBody
        )
    )

atomToFull (Shape (Swap (Poly aPoly) (Poly bPoly)) flavor) =
  lunbind aPoly
  (\(aPat, aBody) ->
    lunbind bPoly
      (\(bPat, bBody) ->
        swapToFull flavor aPat bPat aBody bBody
      )
  )

cupToFull :: (LFresh m)
  => Dir -> Flavor -> (VarSet, TypeExpr) -> m Type.Full
cupToFull dir flavor (pat, body) =
  pure $ uncurry (full pat) (a,b)
  where
  (a, b) = swapToDir dir big small
  big = cupType flavor body
  cupType Add (ProductTypeExpr p) = SumTypeExpr $ SumExpr [(Pos, p), (Neg, p)]
  cupType Mul (SumTypeExpr s) = ProductTypeExpr $ ProductExpr [(Gro, s), (Shr, s)]
  cupType Add (SumTypeExpr s) = cupType Add . ProductTypeExpr $ toProduct s
  cupType Mul (ProductTypeExpr p) = cupType Mul . SumTypeExpr $ toSum p
  small = idtype flavor

splitToFull :: forall m. (LFresh m, MonadError TypeError m)
  => Dir -> Flavor -> VarSet -> VarSet -> TypeExpr -> TypeExpr -> m Type.Full
splitToFull dir flavor topPat botPat topBody botBody =
  do
  lfreshen botPat
    $ \ botPat' renameing ->
      do
      let botBody' = swaps renameing botBody
      big <- bigSplitType flavor topBody botBody'
      small <- smallSplitType flavor topBody botBody'
      let disjointUnionPat = topPat ++ botPat'
      pure . uncurry (full disjointUnionPat)
        $ swapToDir dir big small
  where
  bigSplitType ::
    Flavor -> TypeExpr -> TypeExpr -> m TypeExpr
  smallSplitType ::
    Flavor -> TypeExpr -> TypeExpr -> m TypeExpr

  bigSplitType Add (SumTypeExpr top) (SumTypeExpr bot) =
    pure . SumTypeExpr . SumExpr $ map ((Pos,) . toProduct) [top, bot]
  bigSplitType Mul (ProductTypeExpr top) (ProductTypeExpr bot) =
    pure . ProductTypeExpr . ProductExpr
      $ map ((Gro,) . toSum) [top, bot]
      -- See comment on smallSplitType
  bigSplitType flavor topBody botBody' =
    throwError $ InvalidSplit flavor topBody botBody'

  smallSplitType Add (SumTypeExpr sTop) (SumTypeExpr sBot) =
    pure . SumTypeExpr $ (sTop <> sBot)
  smallSplitType Mul (ProductTypeExpr pTop) (ProductTypeExpr pBot) =
    pure . ProductTypeExpr $ (pTop <> pBot)
  -- this is why we fail with bigSplitType:
  --  if the flavor doesn't match the bodies then we can't make a small type.
  -- Perhaps restructure so that splits are patterns + expresions of the right flavor?
  smallSplitType flavor topBody botBody' =
    throwError $ InvalidSplit flavor topBody botBody'

swapToFull :: forall m. (LFresh m, MonadError TypeError m)
  => Flavor -> VarSet -> VarSet -> TypeExpr -> TypeExpr
  -> m Type.Full
swapToFull flavor aPat bPat aBody bBody =
  pure $ full (aPat ++ bPat) leftBody rightBody
  where
  leftBody = smoosh flavor aBody bBody
  rightBody = smoosh flavor bBody aBody

smoosh Add (SumTypeExpr a) (SumTypeExpr b) = SumTypeExpr (a <> b)
smoosh Add (ProductTypeExpr a) (ProductTypeExpr b) =
  normalizeTypeExpr . SumTypeExpr $ SumExpr [(Pos, a),(Pos, b)]
smoosh Mul (ProductTypeExpr a) (ProductTypeExpr b) = ProductTypeExpr (a <> b)
smoosh Mul (SumTypeExpr a) (SumTypeExpr b) =
  normalizeTypeExpr . ProductTypeExpr $ ProductExpr [(Gro, a),(Gro, b)]


idtype :: Flavor -> TypeExpr
idtype Add = SumTypeExpr mempty
idtype Mul = ProductTypeExpr mempty

swapToDir :: Dir -> a -> a -> (a, a)
swapToDir SpreadLeft big small = (big, small)
swapToDir SpreadRight big small = (small, big)