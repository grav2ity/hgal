module Hgal.Graph.Generators where

import Control.Monad.State
import Linear(R3())

import Hgal.Data.PropertyM
import Hgal.Graph.ClassM
import Hgal.Graph.GeneratorsM as M


makeTriangle :: MutableFaceGraph (State g) g
             => Property (State g) g (V g) p
             => g -> p -> p -> p -> (H g, g)
makeTriangle g p0 p1 p2 = runState (M.makeTriangle g p0 p1 p2) g

makeQuad :: MutableFaceGraph (State g) g
         => Property (State g) g (V g) p
         => g -> p -> p -> p -> p -> (H g, g)
makeQuad g p0 p1 p2 p3 = runState (M.makeQuad g p0 p1 p2 p3) g

makeTetrahedron :: MutableFaceGraph (State g) g
                => Property (State g) g (V g) p
                => g -> p -> p -> p -> p -> (H g, g)
makeTetrahedron g p0 p1 p2 p3 = runState (M.makeTetrahedron g p0 p1 p2 p3) g

makeHexahedron :: MutableFaceGraph (State g) g
               => Property (State g) g (V g) p
               => Eq (H g)
               => g -> p -> p -> p -> p -> p -> p -> p -> p -> (H g, g)
makeHexahedron g p0 p1 p2 p3 p4 p5 p6 p7 =
  runState (M.makeHexahedron g p0 p1 p2 p3 p4 p5 p6 p7) g

makeRegularPrism :: MutableFaceGraph (State g) g
                 => Property (State g) g (V g) (p a)
                 => (Ord (V g), Eq (H g), Eq (F g))
                 => Floating a
                 => R3 p
                 => g -> Int -> p a -> a -> a -> Bool -> (H g, g)
makeRegularPrism g n center height radius isClosed =
  runState (M.makeRegularPrism g n center height radius isClosed) g

makePyramid :: MutableFaceGraph (State g) g
            => Property (State g) g (V g) (p a)
            => (Ord (V g), Eq (H g), Eq (F g))
            => Floating a
            => R3 p
            => g -> Int -> p a -> a -> a -> Bool -> (H g, g)
makePyramid g n center height radius isClosed =
  runState (M.makePyramid g n center height radius isClosed) g

makeIcosahedron :: MutableFaceGraph (State g) g
                => Property (State g) g (V g) (p a)
                => (Ord (V g), Eq (H g), Eq (F g))
                => Floating a
                => R3 p
                => g -> p a -> a -> (H g, g)
makeIcosahedron g center radius = runState (M.makeIcosahedron g center radius) g

makeGrid :: MutableFaceGraph (State g) g
         => Property (State g) g (V g) p
         => (Ord (V g), Eq (H g), Eq (F g))
         => g -> Int -> Int -> (Int -> Int -> p) -> Bool -> (H g, g)
makeGrid g i j coordF triangulated = runState (M.makeGrid g i j coordF triangulated) g
