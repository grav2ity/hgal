module Hgal.Data.PropertyM where


import Data.Default.Class
import Data.Maybe
import Data.Proxy
import qualified Data.Vector as V

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
