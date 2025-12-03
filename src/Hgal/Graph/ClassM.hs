module Hgal.Graph.ClassM where

import Control.Lens(Iso'())
import Control.Monad.State

import qualified Hgal.Graph.Class as Pure
import Hgal.Graph.Class (Vertex, Halfedge, Edge, Face)
import Hgal.Data.PropertyM

type V g = Vertex g
type H g = Halfedge g
type E g = Edge g
type F g = Face g


class Element m a where
  isBorder :: a -> m Bool
  isValid :: a -> m (Either String Bool)
  degree :: a -> m Int

  default isBorder :: Pure.Element g a
                   => MonadState g m
                   => a -> m Bool
  isBorder a = gets (`Pure.isBorder` a)

  default isValid :: Pure.Element g a
                  => MonadState g m
                  => a -> m (Either String Bool)
  isValid a = gets (`Pure.isValid` a)

  default degree :: Pure.Element g a
                 => MonadState g m
                 => a -> m Int
  degree a = gets (`Pure.degree` a)


class Element m a => RemovableElement m a where
  remove :: a -> m ()

  default remove :: Pure.RemovableElement g a
                 => MonadState g m
                 => a -> m ()
  remove a = modify (`Pure.remove` a)


class GetHalfedge m g a | m -> g where
  halfedge :: a -> m (Halfedge g)

  default halfedge :: Pure.GetHalfedge g a
                   => MonadState g m
                   => a -> m (Halfedge g)
  halfedge a = gets (`Pure.halfedge` a)


class SetHalfedge m g a | m -> g where
  setHalfedge :: a -> Halfedge g -> m ()

  default setHalfedge :: Pure.SetHalfedge g a
                      => MonadState g m
                      => a -> Halfedge g -> m ()
  setHalfedge a h = modify (\s -> Pure.setHalfedge s a h)


class GetFace m g a | m -> g where
  face :: a -> m (Face g)

  default face :: Pure.GetFace g a
               => MonadState g m
               => a -> m (Face g)
  face a = gets (`Pure.face` a)


class SetFace m g a | m -> g where
  setFace :: a -> Face g -> m ()

  default setFace :: Pure.SetFace g a
                  => MonadState g m
                  => a -> Face g -> m ()
  setFace a f = modify (\s -> Pure.setFace s a f)






-- -- either Monad or Elements (v, h, e, f) must hold or reference Graph
-- -- but since Monad is not required to do so (ST s)
-- -- functions that do not take any Element must take Graph

class
  ( Monad m,
    Element m (Vertex g),
    Element m (Halfedge g),
    Element m (Edge g),
    GetHalfedge m g (Vertex g),
    GetHalfedge m g (Edge g)
  ) => HalfedgeGraph m g where
  edge :: Halfedge g -> m (Edge g)
  opposite :: Halfedge g -> m (Halfedge g)
  source :: Halfedge g -> m (Vertex g)
  target :: Halfedge g -> m (Vertex g)
  next :: Halfedge g -> m (Halfedge g)
  prev :: Halfedge g -> m (Halfedge g)

  halfedgeVV :: Vertex g -> Vertex g -> m (Maybe (Halfedge g))

  vertices :: g -> m [Vertex g]
  halfedges :: g -> m [Halfedge g]
  edges :: g -> m [Edge g]

  nullVertex :: g -> m (Vertex g)
  nullHalfedge :: g -> m (Halfedge g)
  nullEdge :: g -> m (Edge g)

  default edge :: Pure.HalfedgeGraph g
               => MonadState g m
               => Halfedge g -> m (Edge g)
  edge h = gets (`Pure.edge` h)

  default opposite :: Pure.HalfedgeGraph g
                   => MonadState g m
                   => Halfedge g -> m (Halfedge g)
  opposite h = gets (`Pure.opposite` h)

  default source :: Pure.HalfedgeGraph g
                 => MonadState g m
                 => Halfedge g -> m (Vertex g)
  source h = gets (`Pure.source` h)

  default target :: Pure.HalfedgeGraph g
                 => MonadState g m
                 => Halfedge g -> m (Vertex g)
  target h = gets (`Pure.target` h)

  default next :: Pure.HalfedgeGraph g
               => MonadState g m
               => Halfedge g -> m (Halfedge g)
  next h = gets (`Pure.next` h)

  default prev :: Pure.HalfedgeGraph g
               => MonadState g m
               => Halfedge g -> m (Halfedge g)
  prev h = gets (`Pure.prev` h)

  default halfedgeVV :: Pure.HalfedgeGraph g
                     => MonadState g m
                     => Vertex g -> Vertex g -> m (Maybe (Halfedge g))
  halfedgeVV v1 v2 = gets (\s -> Pure.halfedgeVV s v1 v2)


  default vertices :: Pure.HalfedgeGraph g
                   => MonadState g m
                   => g -> m [Vertex g]
  vertices _ = gets Pure.vertices

  default halfedges :: Pure.HalfedgeGraph g
                    => MonadState g m
                    => g -> m [Halfedge g]
  halfedges _ = gets Pure.halfedges

  default edges :: Pure.HalfedgeGraph g
                => MonadState g m
                => g -> m [Edge g]
  edges _ = gets Pure.edges

  default nullVertex :: Pure.HalfedgeGraph g
                     => MonadState g m
                     => g -> m (Vertex g)
  nullVertex _ = gets Pure.nullVertex

  default nullHalfedge :: Pure.HalfedgeGraph g
                       => MonadState g m
                       => g -> m (Halfedge g)
  nullHalfedge _ = gets Pure.nullHalfedge

  default nullEdge :: Pure.HalfedgeGraph g
                   => MonadState g m
                   => g -> m (Edge g)
  nullEdge _ = gets Pure.nullEdge


class
  ( HalfedgeGraph m g,
    RemovableElement m (Vertex g),
    RemovableElement m (Edge g),
    SetHalfedge m g (Vertex g)
  ) => MutableHalfedgeGraph m g where
  setTarget :: Halfedge g -> Vertex g -> m ()
  setNext :: Halfedge g -> Halfedge g -> m ()

  addVertex :: g -> m (Vertex g)
  addEdge :: g -> m (Edge g)

  default setTarget :: Pure.MutableHalfedgeGraph g
                    => MonadState g m
                    => Halfedge g -> Vertex g -> m ()
  setTarget h v = modify (\s -> Pure.setTarget s h v)

  default setNext :: Pure.MutableHalfedgeGraph g
                  => MonadState g m
                  => Halfedge g -> Halfedge g -> m ()
  setNext h1 h2 = modify (\s -> Pure.setNext s h1 h2)

  default addVertex :: Pure.MutableHalfedgeGraph g
                    => MonadState g m
                    => g -> m (Vertex g)
  addVertex _ = state Pure.addVertex

  default addEdge :: Pure.MutableHalfedgeGraph g
                  => MonadState g m
                  => g -> m (Edge g)
  addEdge _ = state Pure.addEdge


class
  ( HalfedgeGraph m g,
    Element m (Face g),
    GetHalfedge m g (Face g),
    GetFace m g (Halfedge g)
  ) => FaceGraph m g where

  faces :: g -> m [Face g]

  nullFace :: g -> m (Face g)

  default faces :: Pure.FaceGraph g
                => MonadState g m
                => g -> m [Face g]
  faces _ = gets Pure.faces

  default nullFace :: Pure.FaceGraph g
                   => MonadState g m
                   => g -> m (Face g)
  nullFace _ = gets Pure.nullFace


class
  ( FaceGraph m g,
    MutableHalfedgeGraph m g,
    RemovableElement m (Face g),
    SetHalfedge m g (Face g),
    SetFace m g (Halfedge g)
  ) => MutableFaceGraph m g where

  addFace :: g -> m (Face g)

  default addFace :: Pure.MutableFaceGraph g
                  => MonadState g m
                  => g -> m (Face g)
  addFace _ = state Pure.addFace


class Property m g (Pure.Point v) p => PointGraph m g v p where

