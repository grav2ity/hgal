module Hgal.Graph.Helpers where

import Hgal.Graph.Class

next2 :: HalfedgeGraph g
      => g -> H g -> H g
next2 g = next g . next g

next3 :: HalfedgeGraph g
      => g -> H g -> H g
next3 g = next g . next g . next g

exactNumVertices :: HalfedgeGraph g
                 => g -> Int
exactNumVertices = length . vertices

exactNumHalfedges :: HalfedgeGraph g
                  => g -> Int
exactNumHalfedges = length . halfedges

exactNumEdges :: HalfedgeGraph g
              => g -> Int
exactNumEdges = length . edges

exactNumFaces :: FaceGraph g
              => g -> Int
exactNumFaces = length . faces

