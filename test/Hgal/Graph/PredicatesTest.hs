{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hgal.Graph.PredicatesTest where

import Control.Monad.State
import Linear (V3(..))
import Test.Tasty
import Test.Tasty.Hspec

import Hgal.Graph.Class
import qualified Hgal.Graph.ClassM as M
import Hgal.Graph.Fixtures
import Hgal.Graph.Predicates
import Hgal.Data.SurfaceMesh (SurfaceMesh)
import qualified Hgal.Data.SurfaceMesh as SurfaceMesh


test_Predicates :: IO TestTree
test_Predicates = do
  specs <- concat <$> mapM testSpecs
           [ testSimple @Double @(SurfaceMesh (V3 Double) () () ())
           , testValidity @Double @(SurfaceMesh (V3 Double) () () ()) (SurfaceMesh.empty)
           , testValidity2 @Double @(SurfaceMesh (V3 Double) () () ()) (SurfaceMesh.empty)
           ]
  return $ testGroup "Predicates" [testGroup "Static fixture tests" specs]


isWhatItIs :: forall a g v h e f p. SurfaceFixtureC a g v h e f p
           => g -> String -> Bool -> Bool -> Bool -> Bool -> Spec
isWhatItIs g name triangle quad tetrahedron hexahedron = do
  let h = head $ halfedges g
  it (name ++ " test") $ do
    isIsolatedTriangle g h `shouldBe` triangle
    isIsolatedQuad g h `shouldBe` quad
    isTetrahedron g h `shouldBe` tetrahedron
    isHexahedron g h `shouldBe` hexahedron

testSimple :: forall a g v h e f p. SurfaceFixtureC a g v h e f p => Spec
testSimple = do
  triangle <- runIO $ fromOFF @g "test/Hgal/Meshes/triangle.off"
  quad <- runIO $ fromOFF @g "test/Hgal/Meshes/quad.off"
  tetrahedron <- runIO $ fromOFF @g "test/Hgal/Meshes/tetrahedron.off"
  cube <- runIO $ fromOFF @g "test/Hgal/Meshes/cube.off"
  cubeQuads <- runIO $ fromOFF @g "test/Hgal/Meshes/cube-quads.off"
  isWhatItIs triangle "triangle" True False False False
  isWhatItIs quad "quad" False True False False
  isWhatItIs tetrahedron "tetrahedron" False False True False
  isWhatItIs cube "cube" False False False False
  isWhatItIs cubeQuads "hexahedron" False False False True

testValidity :: forall a g v h e f p. SurfaceFixtureC a g v h e f p
             => g -> Spec
testValidity g = do
  describe "validity functions test 1" $ do
    it "seem to work" $ do
      let (vs, g1) = runState (replicateM 4 (M.addVertex g)) g
          (es, g2) = runState (replicateM 4 (M.addEdge g1)) g1
      either id show (isValidHalfedgeGraph g2) `shouldNotBe` "True"
      let (_, g3) = runState f g2
          vsc = cycle vs
          esc = cycle es
          f = do
            zipWithM_ f1 vs es
            zipWithM_ f2 (take 4 . drop 1 $ vsc) es
            zipWithM f3 es (take 4 . drop 1 $ esc)
          f1 v e = do
            h <- M.halfedge e
            M.setTarget h v
            M.setHalfedge v h
          f2 v e = do
            h <- (M.opposite <=< M.halfedge) e
            M.setTarget h v
          f3 e1 e2 = do
            h1 <- M.halfedge e1
            h2 <- M.halfedge e2
            h1o <- M.opposite h1
            h2o <- M.opposite h2
            M.setNext h2 h1
            M.setNext h1o h2o
      either id show (isValidHalfedgeGraph g3) `shouldBe` "True"
      let (fac, g4) = addFace g3
      either id show (isValidFaceGraph g4) `shouldNotBe` "True"
      let (_, g5) = runState f4 g4
          f4 = do
            mapM_ f5 es
            h <- (M.opposite <=< M.halfedge) (head es)
            M.setHalfedge fac h
          f5 e = do
            h <- (M.opposite <=< M.halfedge) e
            M.setFace h fac
      either id show (isValidFaceGraph g5) `shouldBe` "True"
      either id show (isValidPolygonMesh g5) `shouldBe` "True"

testValidity2 :: forall a g v h e f p. SurfaceFixtureC a g v h e f p
             => g -> Spec
testValidity2 g = do
  describe "validity functions test 2" $ do
    it "seem to work" $ do
      let ([v0, v1], g1) = runState (replicateM 2 (M.addVertex g)) g
          (e, g2) = runState (M.addEdge g1) g1
          (fac, g3) = runState (M.addFace g2) g2
          h = halfedge g3 e
          ho = opposite g3 h
          f = do
            M.setTarget h v0
            M.setHalfedge v0 h
            M.setTarget ho v1
            M.setHalfedge v1 ho
            M.setNext h ho
            M.setNext ho h
            M.setHalfedge fac ho
            M.setFace h fac
            M.setFace ho fac
          g4 = execState f g3
      either id show (isValidFaceGraph g4) `shouldBe` "True"
      either id show (isValidPolygonMesh g4) `shouldNotBe` "True"
