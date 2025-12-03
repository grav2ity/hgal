module Hgal.Graph.HelpersM where

import Control.Monad
import Control.Lens ((??))

import Hgal.Graph.ClassM


setBorder :: MutableFaceGraph m g
          => g -> H g -> m ()
setBorder g h = setFace h =<< nullFace g

copy :: MutableFaceGraph m g
     => g -> H g -> m (H g)
copy g h = do
  e <- addEdge g
  res <- halfedge e
  ropp <- opposite res
  hopp <- opposite h
  setTarget res =<< target h
  setTarget hopp =<< target hopp -- ??
  setFace res =<< face h
  setFace ropp =<< face hopp
  -- note that we cannot call set_next as it then would call set_prev on the  original
  return res

setVertexHalfedge :: MutableHalfedgeGraph m g
                  => H g -> m ()
setVertexHalfedge h = do
  (setHalfedge ?? h) =<< target h

closeTip :: MutableHalfedgeGraph m g
         => H g -> V g -> m ()
closeTip h v = do
  setNext h =<< opposite h
  setTarget h v
  setHalfedge v h

insertTip :: MutableHalfedgeGraph m g
          => H g -> H g -> m ()
insertTip h h2 = do
  setNext h =<< next h2
  setNext h2 =<< opposite h
  setTarget h =<< target h2

removeTip :: MutableHalfedgeGraph m g
          => H g -> m ()
removeTip h = do
  setNext h =<< (next <=< opposite <=< next) h

setFaceInFaceLoop :: MutableFaceGraph m g
                  => Eq (H g)
                  => H g -> F g -> m ()
setFaceInFaceLoop h f = worker h
  where
    worker hx = do
      setFace hx f
      n <- next hx
      when (n /= h) (worker n)

insertHalfedge :: MutableFaceGraph m g
               => H g -> H g -> m ()
insertHalfedge h f = do
  setNext h =<< next f
  setNext f h
  setFace h =<< face f

exactNumVertices :: HalfedgeGraph m g
                 => g -> m Int
exactNumVertices = return . length <=< vertices

exactNumHalfedges :: HalfedgeGraph m g
                  => g -> m Int
exactNumHalfedges = return . length <=< halfedges

exactNumEdges :: HalfedgeGraph m g
              => g -> m Int
exactNumEdges = return . length <=< edges

exactNumFaces :: FaceGraph m g
              => g -> m Int
exactNumFaces = return . length <=< faces

isIsolated :: HalfedgeGraph m g
           => Eq (H g)
           => g -> V g -> m Bool
isIsolated g v = liftM2 (==) (halfedge v) (nullHalfedge g)

adjustIncomingHalfedge :: MutableHalfedgeGraph m g
                       => FaceGraph m g
                       => (Eq (V g), Eq (H g), Eq (F g))
                       => g -> V g -> m ()
adjustIncomingHalfedge g v = do
  h <- halfedge v
  nullH <- nullHalfedge g
  nullF <- nullFace g
  if h == nullH then return ()
    else do
      tar <- target h
      h' <- if tar == v then return h
              else opposite h >>= \o -> setHalfedge v o >> return o
      let worker hx = do
            f <- face h
            if f == nullF then setHalfedge v h
              else do
                n <- (opposite <=< next) hx
                when (hx /= h') (worker n)
      worker h'
