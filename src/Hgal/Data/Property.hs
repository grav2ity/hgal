module Hgal.Data.Property where

import Control.Lens


class Property s k p | s k -> p where
  property :: k -> Lens' s (Maybe p)
  -- properties :: s -> Proxy k -> V.Vector (Maybe p)

  find :: s -> p -> Maybe k
  findKeys :: s -> (p -> Bool) -> [k]
