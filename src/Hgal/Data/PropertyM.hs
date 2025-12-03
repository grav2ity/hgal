module Hgal.Data.PropertyM where


import Data.Default.Class
import Data.Maybe
import Data.Proxy
import qualified Data.Vector as V

import Hgal.Data.Property
import Hgal.Graph.Class

class PropertyGraph m g where
  getVertexProperty :: g -> V g -> m (Maybe (Vp g))

  -- getVertexProperty' :: (Functor m, Default p) => g -> V g -> m p
  -- getVertexProperty' s k = fromMaybe def <$> getVertexProperty s k

  adjustVertexProperty :: g -> (Vp g -> Vp g) -> V g -> m ()
  replaceVertexProperty :: g -> V g -> Vp g -> m ()
  replaceVertexProperty s k v = adjustVertexProperty s (const v) k

  -- vertexProperty :: V g -> Lens' g (Maybe (Vp g))

-- class Property m s k p | s k -> p, s p -> k where
class Property m s k p | s k -> p where
  getProperty :: s -> k -> m (Maybe p)

  getProperty' :: (Functor m, Default p) => s -> k -> m p
  getProperty' s k = fromMaybe def <$> getProperty s k
    -- mp <- getProperty s k
    -- where
    --   f (Just p) = p
    --   f Nothing = def
    -- case mp of
    --   Just p -> return p
    --   Nothing -> return def

  adjustProperty :: s -> (p -> p) -> k  -> m ()
  replaceProperty :: s -> k -> p -> m ()
  replaceProperty s k v = adjustProperty s (const v) k
  properties :: s -> Proxy k -> m (V.Vector (Maybe p))

  -- default getProperty' :: Default p => s -> k -> m p
  -- isBorder a = gets (`Pure.isBorder` a)
