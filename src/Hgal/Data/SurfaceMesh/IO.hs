module Hgal.Data.SurfaceMesh.IO where

import Data.Either
import Data.Traversable
import HOff.Parser
import Text.Parsec
import Linear


import Hgal.Data.SurfaceMesh (SurfaceMesh(), Vertex(..), empty, newFace, newVertex)

fromOFF :: (Num a, Read a) => String -> IO (SurfaceMesh (V3 a) () () () )
fromOFF fname = do
  input <- readFile fname
  result <- runParserT offP defParStat fname input
  let off = fromRight (OFF [] []) result
      vertices = (\(Vertice (x, y, z)) -> V3 x y z) <$> offVertice off
      faces = (\(Face is) -> Vertex <$> is) <$> offFace off
      (sm, _) = mapAccumL newVertex empty vertices
      (sm', _) = mapAccumL newFace sm faces
  return sm'
