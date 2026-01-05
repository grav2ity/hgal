module Hgal.Data.SurfaceMesh.Import where

import Data.Either
import Data.Traversable
import HOff.Parser
import Text.Parsec
import Linear


import Hgal.Data.SurfaceMesh (SurfaceMesh(), Vertex(..), empty, newFace, newVertex)

fromOFF :: (Num a, Read a) => String -> SurfaceMesh (V3 a) b c d
fromOFF input =
  let result = runParser offP defParStat "" input
      off = fromRight (OFF [] []) result
      vertices = (\(Vertice (x, y, z)) -> V3 x y z) <$> offVertice off
      faces = (\(Face is) -> Vertex <$> is) <$> offFace off
      (sm, _) = mapAccumL newVertex empty vertices
      (sm', _) = mapAccumL newFace sm faces
  in sm'
