module Hgal.Graph.LoopsM where

import Control.Monad
import Control.Lens ((??))
import Data.Vector.Circular (CircularVector)
import qualified Data.Vector.Circular as CV

import Hgal.Graph.ClassM


nextAroundTarget :: HalfedgeGraph m g
                 => H g -> m (H g)
nextAroundTarget = opposite <=< next

prevAroundTarget :: HalfedgeGraph m g
                 => H g -> m (H g)
prevAroundTarget = prev <=< opposite

nextAroundSource :: HalfedgeGraph m g
                 => H g -> m (H g)
nextAroundSource = next <=< opposite

prevAroundSource :: HalfedgeGraph m g
                 => H g -> m (H g)
prevAroundSource = opposite <=< prev

-- -------------------------------------------------------------------------------
-- -- perform action on each element

halfedgeAroundTarget :: HalfedgeGraph m g
                     => Eq (H g)
                     => g -> (g -> H g -> m ()) -> H g -> m ()
halfedgeAroundTarget = loop nextAroundTarget

halfedgeAroundSource :: HalfedgeGraph m g
                     => Eq (H g)
                     => g -> (g -> H g -> m ()) -> H g -> m ()
halfedgeAroundSource = loop nextAroundSource

halfedgeAroundFace :: HalfedgeGraph m g
                   => Eq (H g)
                   => g -> (g -> H g -> m ()) -> H g -> m ()
halfedgeAroundFace = loop next

faceAroundTarget :: FaceGraph m g
                 => Eq (H g)
                 => g -> (g -> F g -> m ()) -> H g -> m ()
faceAroundTarget g f = halfedgeAroundTarget g (\g' hx -> f g' =<< face hx)

faceAroundFace :: FaceGraph m g
               => Eq (H g)
               => g -> (g -> F g -> m ()) -> H g -> m ()
faceAroundFace g f = halfedgeAroundFace g (\g' hx -> f g' =<< (face <=< opposite) hx)

vertexAroundTarget :: HalfedgeGraph m g
                   => Eq (H g)
                   => g -> (g -> V g -> m ()) -> H g -> m ()
vertexAroundTarget g f =
  halfedgeAroundTarget g (\g' hx -> f g' =<< source hx)

vertexAroundFace :: FaceGraph m g
                 => Eq (H g)
                 => g -> (g -> V g -> m ()) -> H g -> m ()
vertexAroundFace g f =
  halfedgeAroundFace g (\g' hx -> f g' =<< target hx)

edgeAroundFace :: HalfedgeGraph m g
               => Eq (H g)
               => g -> (g -> E g -> m ()) -> H g -> m ()
edgeAroundFace g f =
  halfedgeAroundFace g (\g' hx -> f g' =<< edge hx)

-- -------------------------------------------------------------------------------
-- -- gets circular vector of all elements

halfedgesAroundTarget :: HalfedgeGraph m g
                      => Eq (H g)
                      => H g -> m (CircularVector (H g))
halfedgesAroundTarget h =
  CV.unfoldr1M (loopC nextAroundTarget return h) h h

halfedgesAroundSource :: HalfedgeGraph m g
                      => Eq (H g)
                      => H g -> m (CircularVector (H g))
halfedgesAroundSource h =
  CV.unfoldr1M (loopC nextAroundSource return h) h h

halfedgesAroundFace :: HalfedgeGraph m g
                    => Eq (H g)
                    => H g -> m (CircularVector (H g))
halfedgesAroundFace h = CV.unfoldr1M (loopC next return h) h h

facesAroundTarget :: FaceGraph m g
                  => Eq (H g)
                  => H g -> m (CircularVector (F g))
facesAroundTarget h =
  (CV.unfoldr1M (loopC nextAroundTarget face h) ?? h) =<< face h

facesAroundFace :: FaceGraph m g
                => Eq (H g)
                => H g -> m (CircularVector (F g))
facesAroundFace h =
  (CV.unfoldr1M (loopC next face h) ?? h) =<< face h

verticesAroundTarget :: HalfedgeGraph m g
                     => Eq (H g)
                     => H g -> m (CircularVector (V g))
verticesAroundTarget h =
  (CV.unfoldr1M (loopC nextAroundTarget source h) ?? h) =<< source h

verticesAroundFace :: HalfedgeGraph m g
                   => Eq (H g)
                   => H g -> m (CircularVector (V g))
verticesAroundFace h =
  (CV.unfoldr1M (loopC next target h) ?? h) =<< target h

edgesAroundFace :: HalfedgeGraph m g
                => Eq (H g)
                => H g -> m (CircularVector (E g))
edgesAroundFace h =
  (CV.unfoldr1M (loopC next edge h) ?? h) =<< edge h

-- -------------------------------------------------------------------------------
-- -- internal helpers

loop :: Monad m
     => Eq (H g)
     => (H g -> m (H g))
     -> g
     -> (g -> H g -> m ())
     -> H g
     -> m ()
loop m g f h = worker h
  where
    worker hx = do
      f g hx
      n <- m hx
      when (n /= h) (worker n)

-- loopC :: Monad m
--       => Eq (H g)
--       => (H g -> m (H g))
--       -> (H g -> m a)
--       -> H g
--       -> H g
--       -> m (Maybe (a, H g))
-- loopC m f h hx = do
--   n <- m hx
--   a <- f n
--   if n /= h then return (Just (a, n)) else return Nothing

loopC :: Monad m
      => Eq a
      => (a -> m a)
      -> (a -> m b)
      -> a
      -> a
      -> m (Maybe (b, a))
loopC m f h hx = do
  n <- m hx
  a <- f n
  if n /= h then return (Just (a, n)) else return Nothing
