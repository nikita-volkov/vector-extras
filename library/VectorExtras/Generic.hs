module VectorExtras.Generic
  ( module Basics,
    fromFoldable,
    fromFoldableNarrowing,
  )
where

import Data.Vector.Generic hiding (foldl')
import qualified VectorExtras.Accumulator as VAcc
import VectorExtras.Basics.Generic as Basics
import VectorExtras.Prelude

{-# INLINE fromFoldable #-}
fromFoldable :: (Foldable f, Vector v a) => f a -> v a
fromFoldable =
  VAcc.toVector . foldl' (flip VAcc.add) VAcc.init

{-# INLINE fromFoldableNarrowing #-}
fromFoldableNarrowing :: (Foldable f, Vector v b) => (a -> Maybe b) -> f a -> v b
fromFoldableNarrowing narrow =
  VAcc.toVector . foldl' step VAcc.init
  where
    step acc el = case narrow el of
      Nothing -> acc
      Just narrowed -> VAcc.add narrowed acc
