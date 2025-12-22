module Hgal.Data.PropertyM where

import Data.Default.Class
import Data.Maybe
-- import Data.Proxy
-- import qualified Data.Vector as V


class Property m s k p | s k -> p where
  getProperty :: s -> k -> m (Maybe p)

  getProperty' :: (Functor m, Default p) => s -> k -> m p
  getProperty' s k = fromMaybe def <$> getProperty s k

  adjustProperty :: s -> (p -> p) -> k -> m ()

  replaceProperty :: s -> k -> p -> m ()
  replaceProperty s k v = adjustProperty s (const v) k

  -- properties :: s -> Proxy k -> m (V.Vector (Maybe p))
