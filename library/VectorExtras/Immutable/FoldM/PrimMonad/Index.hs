module VectorExtras.Immutable.FoldM.PrimMonad.Index where

import VectorExtras.Prelude hiding (length)
import Data.Vector.Generic
import Control.Foldl
import qualified Data.Vector.Generic.Mutable as Mutable


type IndexPrimMonadFoldM result = forall m. PrimMonad m => FoldM m Int result

counts :: (Vector vector count, Enum count) => Int -> IndexPrimMonadFoldM (vector count)
counts amount = FoldM step init extract where
  init = Mutable.new amount
  step mv index = Mutable.modify mv succ index $> mv
  extract = unsafeFreeze
