{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hgal.Graph.Fixtures where

import Control.Lens
import Control.Monad.State
import Data.Maybe
import Linear (V3(..))

import Hgal.Data.Property
import qualified Hgal.Data.PropertyM as M
import Hgal.Data.SurfaceMesh (SurfaceMesh)
import qualified Hgal.Data.SurfaceMesh as SurfaceMesh
import qualified Hgal.Data.SurfaceMesh.IO as SurfaceMesh.IO
import Hgal.Graph.Class
import qualified Hgal.Graph.ClassM as M
import Hgal.Graph.Loops

class FromOFF a where
  fromOFF :: String -> IO a

instance (Num a, Read a) => FromOFF (SurfaceMesh (V3 a) () () ()) where
  fromOFF = SurfaceMesh.IO.fromOFF

data FaceFixture g v h e f = FaceFixture
  { faceFixture :: g
  , u, v, w, x, y, z :: v
  , f1, f2, f3, f4 :: f
  }

data HalfedgeFixture g v h e f = HalfedgeFixture
  { halfedgeFixture :: g
  , h1, h2, h3 :: h
  }

class
  ( FromOFF g,
    MutableFaceGraph g v h e f,
    M.MutableFaceGraph (State g) g v h e f,
    Property g v (V3 a),
    M.Property (State g) g v (V3 a),
    Eq a, Num a,
    Eq v, Eq h, Eq f, Show v, Show h, Show f
  ) => SurfaceFixtureC a g v h e f p | g -> v, g -> h, g -> e, g -> f, g -> p

instance (Eq a, Num a, Read a) => SurfaceFixtureC a
  (SurfaceMesh (V3 a) () () ())
  SurfaceMesh.Vertex
  SurfaceMesh.Halfedge
  SurfaceMesh.Edge
  SurfaceMesh.Face
  (Point SurfaceMesh.Vertex)

surfaceFixture1 :: SurfaceFixtureC a g v h e f p
                => IO (FaceFixture g v h e f)
surfaceFixture1 = do
  g <- fromOFF "test/Hgal/Meshes/fixture1.off"
  let [u, v, w, x, y] = fromJust . find g <$>
        [V3 0 0 0, V3 1 0 0, V3 0 1 0, V3 1 1 0, V3 2 0 0]
      f1 = let h = halfedge g u
           in if isBorder g h then face g (opposite g h) else face g h
      f2 = fromJust $ face g . opposite g <$>
           findOf traversed (not . isBorder g . opposite g)
           (halfedgesAroundFace g (halfedge g f1))
      f3 = fromJust $ findOf traversed (\x -> x /= f1 && x /= f2) (faces g)
  return $ FaceFixture g u v w x y y f1 f2 f3 f3

surfaceFixture2 :: forall a g v h e f p. SurfaceFixtureC a g v h e f p
                => IO (FaceFixture g v h e f)
surfaceFixture2 = do
  g <- fromOFF "test/Hgal/Meshes/fixture2.off"
  let [u, v, w, x, y] = fromJust . find g <$>
        [V3 0 2 0, V3 2 2 0, V3 0 0 0, V3 2 0 0, V3 1 1 0]
      hs = fromJust . uncurry (halfedgeVV @g @v @h g) <$>
        [(x, v), (v, u), (u, w), (w, x)]
      [f1, f2, f3, f4] = face g <$> hs
  return $ FaceFixture g u v w x y y f1 f2 f3 f4

surfaceFixture3 :: SurfaceFixtureC a g v h e f p
                => IO (FaceFixture g v h e f)
surfaceFixture3 = do
  g <- fromOFF "test/Hgal/Meshes/fixture3.off"
  let [u, v, w, x, y, z] = fromJust . find g <$>
        [V3 0 1 0, V3 0 0 0, V3 1 0 0, V3 1 1 0, V3 2 0 0, V3 2 1 0]
      f1 = let h = halfedge g u
           in if isBorder g h then face g (opposite g h) else face g h
      f2 = let h = halfedge g z
           in if isBorder g h then face g (opposite g h) else face g h
  return $ FaceFixture g u v w x y z f1 f2 f2 f2

surfaceFixture4 :: SurfaceFixtureC a g v h e f p
                => IO (HalfedgeFixture g v h e f)
surfaceFixture4 = do
  g <- fromOFF "test/Hgal/Meshes/fixture4.off"
  let [v1, v2] = findKeys g (== V3 0 0 0)
      h1 = fromJust $
           findOf traversed (\x -> isBorder g x && target g x == v1 )
           (halfedges g)
      h2 = fromJust $
           findOf traversed (\x -> isBorder g x && target g x == v2 )
           (halfedges g)
  return $ HalfedgeFixture g h1 h2 h2

surfaceFixture5 :: SurfaceFixtureC a g v h e f p
                => IO (HalfedgeFixture g v h e f)
surfaceFixture5 = do
  g <- fromOFF "test/Hgal/Meshes/add_face_to_border.off"
  let v1 = fromJust $ find g (V3 2 1 0)
      v2 = fromJust $ find g (V3 2 (-1) 0)
      h1 = fromJust $
           findOf traversed (\x -> isBorder g x && target g x == v1 )
           (halfedges g)
      h2 = fromJust $
           findOf traversed (\x -> isBorder g x && target g x == v2 )
           (halfedges g)
  return $ HalfedgeFixture g h1 h2 h2

surfaceFixture6 :: SurfaceFixtureC a g v h e f p
                => IO (HalfedgeFixture g v h e f)
surfaceFixture6 = do
  g <- fromOFF "test/Hgal/Meshes/quad.off"
  let h1 = halfedge g . head . faces $ g
      h2 = next g (next g h1)
  return $ HalfedgeFixture g h1 h2 h2

surfaceFixture7 :: SurfaceFixtureC a g v h e f p
                => IO (HalfedgeFixture g v h e f)
surfaceFixture7 = do
  g <- fromOFF "test/Hgal/Meshes/cube.off"
  let h1 = head . halfedges $ g
  return $ HalfedgeFixture g h1 h1 h1

surfaceFixture8 :: SurfaceFixtureC a g v h e f p
                => IO (HalfedgeFixture g v h e f)
surfaceFixture8 = do
  g <- fromOFF "test/Hgal/Meshes/fixture5.off"
  let v1 = fromJust $ find g (V3 0 0 0)
      v2 = fromJust $ find g (V3 1 0 0)
      v3 = fromJust $ find g (V3 0 1 0)
      h1 = fromJust $
           findOf traversed (\x -> source g x == v1 && target g x == v2)
           (halfedges g)
      h2 = fromJust $
           findOf traversed (\x -> source g x == v2 && target g x == v3)
           (halfedges g)
      h3 = fromJust $
           findOf traversed (\x -> source g x == v3 && target g x == v1)
           (halfedges g)
  return $ HalfedgeFixture g h1 h2 h3
