module Hgal.Graph.Class where

import Data.Kind (Type)


type family Vertex g :: Type
type family Halfedge g :: Type
type family Edge g :: Type
type family Face g :: Type

type V g = Vertex g
type H g = Halfedge g
type E g = Edge g
type F g = Face g


class Element g a where
  isBorder :: g -> a -> Bool
  isValid :: g -> a -> Either String Bool
  degree :: g -> a -> Int

class Element g a => RemovableElement g a where
  remove :: g -> a -> g

class GetHalfedge g a where
  halfedge :: g -> a -> Halfedge g

class SetHalfedge g a where
  setHalfedge :: g -> a -> Halfedge g -> g

class GetFace g a where
  face :: g -> a -> Face g

class SetFace g a where
  setFace :: g -> a -> Face g -> g


class
  (
    Element g (Vertex g),
    Element g (Halfedge g),
    Element g (Edge g),
    GetHalfedge g (Vertex g),
    GetHalfedge g (Edge g)
  ) => HalfedgeGraph g where
  edge :: g -> Halfedge g -> Edge g
  opposite :: g -> Halfedge g -> Halfedge g
  source :: g -> Halfedge g -> Vertex g
  target :: g -> Halfedge g -> Vertex g
  next :: g -> Halfedge g -> Halfedge g
  prev :: g -> Halfedge g -> Halfedge g

  halfedgeVV :: g -> Vertex g -> Vertex g -> Maybe (Halfedge g)

  vertices :: g -> [Vertex g]
  halfedges :: g -> [Halfedge g]
  edges :: g -> [Edge g]

class
  ( HalfedgeGraph g,
    RemovableElement g (Vertex g),
    RemovableElement g (Edge g),
    SetHalfedge g (Vertex g)
  ) => MutableHalfedgeGraph g where
  setTarget :: g -> Halfedge g -> Vertex g -> g
  setNext :: g -> Halfedge g -> Halfedge g -> g

  addVertex :: g -> (Vertex g, g)
  addEdge :: g -> (Edge g, g)

class
  ( HalfedgeGraph g ,
    Element g (Face g),
    GetHalfedge g (Face g),
    GetFace g (Halfedge g)
  ) => FaceGraph g where

  faces :: g -> [Face g]

  outerFace :: g -> Face g

class
  ( FaceGraph g,
    MutableHalfedgeGraph g,
    RemovableElement g (Face g),
    SetHalfedge g (Face g),
    SetFace g (Halfedge g)
  ) => MutableFaceGraph g where

  addFace :: g -> (Face g, g)
