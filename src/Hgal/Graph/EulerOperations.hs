module Hgal.Graph.EulerOperations where

import Control.Monad.State

import Hgal.Graph.ClassM
import Hgal.Graph.EulerOperationsM as EulerM

-- note that ClassM.(*)Graph (State g) g
-- can be derived for free once Class.(*)Graph g is available


addFace :: Foldable t
        => MutableFaceGraph (State g) g
        => (Ord (V g), Eq (H g), Eq (F g))
        => g -> t (V g) -> (F g, g)
addFace g vs = runState (EulerM.addFace g vs) g

removeFace :: MutableFaceGraph (State g) g
           => Eq (H g)
           => g -> H g -> g
removeFace g h = execState (EulerM.removeFace g h) g

addEdge :: MutableHalfedgeGraph (State g) g
        => g -> V g -> V g -> (E g, g)
addEdge g v1 v2 = runState (EulerM.addEdge g v1 v2) g

splitEdge :: MutableFaceGraph (State g) g
          => Eq (H g)
          => g -> H g -> (H g, g)
splitEdge g h = runState (EulerM.splitEdge g h) g

joinLoop :: MutableFaceGraph (State g) g
         => (Eq (H g), Eq (F g))
         => g -> H g -> H g -> (H g, g)
joinLoop g h1 h2 = runState (EulerM.joinLoop g h1 h2) g

splitLoop :: MutableFaceGraph (State g) g
          => (Eq (V g), Eq (H g))
          => g -> H g -> H g -> H g -> (H g, g)
splitLoop g h i j = runState (EulerM.splitLoop g h i j) g

splitVertex :: MutableFaceGraph (State g) g
            => Eq (H g)
            => g -> H g -> H g -> (H g, g)
splitVertex g h1 h2 = runState (EulerM.splitVertex g h1 h2) g

joinVertex :: MutableFaceGraph (State g) g
           => (Eq (V g), Eq (H g))
           => g -> H g -> (H g, g)
joinVertex g h = runState (EulerM.joinVertex g h) g

makeHole :: MutableFaceGraph (State g) g
         => Eq (H g)
         => g -> H g -> g
makeHole g h = execState (EulerM.makeHole g h) g

fillHole :: MutableFaceGraph (State g) g
         => Eq (H g)
         => g -> H g -> g
fillHole g h = execState (EulerM.fillHole g h) g

addCenterVertex :: MutableFaceGraph (State g) g
                => Eq (H g)
                => g -> H g -> (H g, g)
addCenterVertex g h = runState (EulerM.addCenterVertex g h) g

removeCenterVertex :: MutableFaceGraph (State g) g
                   => Eq (H g)
                   => g -> H g -> (H g, g)
removeCenterVertex g h = runState (EulerM.removeCenterVertex g h) g

addVertexAndFaceToBorder :: MutableFaceGraph (State g) g
                         => Eq (H g)
                         => g -> H g -> H g -> (H g, g)
addVertexAndFaceToBorder g h1 h2 = runState (EulerM.addVertexAndFaceToBorder g h1 h2) g

addFaceToBorder :: MutableFaceGraph (State g) g
                => Eq (H g)
                => g -> H g -> H g -> (H g, g)
addFaceToBorder g h1 h2 = runState (EulerM.addFaceToBorder g h1 h2) g

joinFace :: MutableFaceGraph (State g) g
         => Eq (H g)
         => g -> H g -> (H g, g)
joinFace g h = runState (EulerM.joinFace g h) g

splitFace :: MutableFaceGraph (State g) g
          => Eq (H g)
          => g -> H g -> H g -> (H g, g)
splitFace g h1 h2 = runState (EulerM.splitFace g h1 h2) g
