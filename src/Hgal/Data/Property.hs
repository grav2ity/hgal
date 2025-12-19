module Hgal.Data.Property where

import Control.Lens
import Data.Kind (Type)
import Data.Proxy
import qualified Data.Vector as V

import Hgal.Graph.Class

type family Vp g :: Type
type family Hp g :: Type
type family Ep g :: Type
type family Fp g :: Type


class PropertyGraph g where
  vertexProperty :: V g -> Lens' g (Maybe (Vp g))

-- class Property s k p | s k -> p, s p -> k where
class Property s k p | s k -> p where
  property :: k -> Lens' s (Maybe p)
  -- properties :: s -> Proxy k -> V.Vector (Maybe p)

  find :: s -> p -> Maybe k
  findKeys :: s -> (p -> Bool) -> [k]
