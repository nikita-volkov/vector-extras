module VectorExtras.Immutable.FoldM.PrimMonad.Index where

import Control.Foldl
import Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable as Mutable
import VectorExtras.Prelude hiding (length)

-- |
-- Fold on indices in PrimMonad.
type IndexPrimMonadFoldM result = forall m. PrimMonad m => FoldM m Int result

-- |
-- Given the size of the vector, construct a fold, which produces a vector of
-- frequencies of each index. I.e., the counts of how often it appeared.
--
-- It is your responsibility to ensure that the indices are within the size of the vector produced.
frequency :: (Vector vector count, Enum count) => Int -> IndexPrimMonadFoldM (vector count)
frequency amount = FoldM step init extract
  where
    init = Mutable.new amount
    step mv index = Mutable.modify mv succ index $> mv
    extract = unsafeFreeze
