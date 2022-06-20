module VectorExtras.Generic
  ( module Basics,
    fromFoldable,
  )
where

import Data.Vector.Generic hiding (foldl')
import qualified VectorExtras.Accumulator as VAcc
import VectorExtras.Basics.Generic as Basics
import VectorExtras.Prelude

fromFoldable :: (Foldable f, Vector v a) => f a -> v a
fromFoldable =
  VAcc.toVector . foldl' (flip VAcc.add) VAcc.init
