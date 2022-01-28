-- |
-- Vector-specialised combinators often used for parsing.
module VectorExtras.Combinators where

import Data.Vector.Generic (Vector)
import VectorExtras.Accumulator (Accumulator)
import qualified VectorExtras.Accumulator as Acc
import VectorExtras.Combinators.Helpers
import VectorExtras.Prelude

many :: (MonadPlus m, Vector v a) => m a -> m (v a)
many getElement =
  accMany Acc.init getElement <&> Acc.toVector

sepBy :: (MonadPlus m, Vector v a) => m a -> m b -> m (v a)
sepBy getElement getSeparator =
  do
    element <- getElement
    Acc.toVector <$> accMany (Acc.add element Acc.init) (getSeparator *> getElement)
