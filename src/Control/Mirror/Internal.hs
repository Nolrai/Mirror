{-# LANGUAGE DataKinds
           , GADTs
           , NoStarIsType
           , PatternSynonyms
  #-}
module Control.Mirror.Internal where
import Control.Mirror.Type.Internal as Type
import Control.Mirror.Type.PrettyPrint as Type.PrettyPrint
import Control.Mirror.Type.Parse as Type.Parse
import Unbound.Generics.LocallyNameless

data Flavor = Add | Mul
data Hand = L | R
data Dir = SpreadLeft | SpreadRight

pattern CollectRight = SpreadLeft
pattern CollectLeft = SpreadRight

data BaseShape = Cup Poly | Split Poly Poly
data Shape = BaseShape BaseShape Dir | Swap Poly Poly

data Atom = Shape Shape Flavor | Dist Hand Dir

data Circut where
  Atom :: Atom -> Circut
  Deeper :: Bind [(Name TypeExpr, Circut)] (TypeExpr, [(Name TypeExpr, Circut)]) -> Circut

data TypeError = InvalidSplit gi

atomToFull :: Atom -> m Type.Full
atomToFull (Shape (BaseShape (Cup (Poly poly')) dir) flavor) =
  lunbind poly' (cupToFull dir flavor)
atomToFull (Shape (BaseShape (Split top bot) dir) flavor) =
  lunbind top
    (\(topPat, topBody) ->
      lunbind bot
        (\(botPat, botBody) ->
          splitToFull dir flavor topPat botPat topBody botBody
        )
    )

cupToFull dir flavor (pat, body) = pure $ full pat (a,b)
  where
  (a, b) = swapToDir dir big small
  big = cupType flavor body
  cupType Add (ProductTypeExpr p) = SumTypeExpr $ SumExpr [(Pos, p), (Neg, p)]
  cupType Mul (SumTypeExpr s) = ProductTypeExpr $ ProductExpr [(Gro, s), (Shr, s)]
  cupType Add (SumTypeExpr s) = cupType Add . ProductTypeExpr $ toProduct s
  cupType Mul (ProductTypeExpr p) = cupType Mul . SumTypeExpr $ toSum p
  small = idtype flavor

splitToFull dir flavor topPat botPat topBody botBody =
  do
  lfreshen botPat
  \ botPat' renameing ->
    do
    let botBody' = swaps renaming botBody
    big <- bigSplitType flavor topBody botBody'
    small <- smallSplitType flavor topBody botBody'
    let disjointUnionPat = topPat ++ botPat'
    pure $ full disjointUnionPat $ swapToDir dir big small
  where
  bigSplitType Add (SumTypeExpr top) (SumTypeExpr bot) =
    pure . SumTypeExpr . SumExpr $ map ((Pos,) . toProduct) [top, bot]
  bigSplitType Mul (ProductTypeExpr top) (ProductTypeExpr bot) =
    pure . ProductTypeExpr . ProductExpr
      $ map ((Grow,) . toSum) [top, bot]
  bigSplitType flavor topBody botBody' =
    fail $ "splitType " ++ show (flavor,topBody,botBody') ++ " invalid combination"

  smallSplitType Add (SumTypeExpr sTop) (SumTypeExpr sBot) =
    pure . SumTypeExpr . SumExpr (sTop ++ sBot)
  smallSplitType Mul (ProductTypeExpr pTop) (ProductTypeExpr pBot) =
    pure . ProductTypeExpr . ProductExpr (pTop ++ pBot)
  smallSplitType flavor topBody botBody' =
    fail $ "splitType " ++ show (flavor,topBody,botBody') ++ " invalid combination"

idtype :: Flavor -> TypeExpr
idtype Add = SumTypeExpr zeroS
idtype Mul = ProductTypeExpr oneP

swapToDir SpreadLeft big small = (big, small)
swapToDir SpreadRight big small = (small, big)