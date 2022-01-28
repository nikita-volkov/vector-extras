-- |
-- Vector-specialised combinators often used for parsing.
module VectorExtras.Combinators where

import Data.Vector.Generic (Vector)
import qualified VectorExtras.Accumulator as Acc
import VectorExtras.Combinators.Helpers
import VectorExtras.Prelude

many :: (MonadPlus m, Vector v a) => m a -> m (v a)
many f =
  collect Acc.init <&> Acc.toVector
  where
    collect !acc =
      orPure acc $ f >>= collect . flip Acc.add acc
