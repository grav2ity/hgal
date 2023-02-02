module Hgal.Graph.Generators where

import Control.Monad.State
import Linear(R3())

import Hgal.Data.PropertyM
import Hgal.Graph.ClassM
import Hgal.Graph.GeneratorsM as M


makeTriangle :: MutableFaceGraph (State g) g v h e f
             => Property (State g) g v p
             => g -> p -> p -> p -> (h, g)
makeTriangle g p0 p1 p2 = runState (M.makeTriangle g p0 p1 p2) g

makeQuad :: MutableFaceGraph (State g) g v h e f
         => Property (State g) g v p
         => g -> p -> p -> p -> p -> (h, g)
makeQuad g p0 p1 p2 p3 = runState (M.makeQuad g p0 p1 p2 p3) g

makeTetrahedron :: MutableFaceGraph (State g) g v h e f
                => Property (State g) g v p
                => g -> p -> p -> p -> p -> (h, g)
makeTetrahedron g p0 p1 p2 p3 = runState (M.makeTetrahedron g p0 p1 p2 p3) g

makeHexahedron :: MutableFaceGraph (State g) g v h e f
               => Property (State g) g v p
               => Eq h
               => g -> p -> p -> p -> p -> p -> p -> p -> p -> (h, g)
makeHexahedron g p0 p1 p2 p3 p4 p5 p6 p7 =
  runState (M.makeHexahedron g p0 p1 p2 p3 p4 p5 p6 p7) g

makeRegularPrism :: MutableFaceGraph (State g) g v h e f
                 => Property (State g) g v (p a)
                 => (Ord v, Eq h, Eq f)
                 => Floating a
                 => R3 p
                 => g -> Int -> p a -> a -> a -> Bool -> (h, g)
makeRegularPrism g n center height radius isClosed =
  runState (M.makeRegularPrism g n center height radius isClosed) g

makePyramid :: MutableFaceGraph (State g) g v h e f
            => Property (State g) g v (p a)
            => (Ord v, Eq h, Eq f)
            => Floating a
            => R3 p
            => g -> Int -> p a -> a -> a -> Bool -> (h, g)
makePyramid g n center height radius isClosed =
  runState (M.makePyramid g n center height radius isClosed) g

makeIcosahedron :: MutableFaceGraph (State g) g v h e f
                => Property (State g) g v (p a)
                => (Ord v, Eq h, Eq f)
                => Floating a
                => R3 p
                => g -> p a -> a -> (h, g)
makeIcosahedron g center radius = runState (M.makeIcosahedron g center radius) g

makeGrid :: MutableFaceGraph (State g) g v h e f
         => Property (State g) g v p
         => (Ord v, Eq h, Eq f)
         => g -> Int -> Int -> (Int -> Int -> p) -> Bool -> (h, g)
makeGrid g i j coordF triangulated = runState (M.makeGrid g i j coordF triangulated) g
