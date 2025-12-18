{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hgal.Graph.EulerOperationsTest where

import Control.Exception
import Control.Lens
import Control.Monad.State
import Data.Either
import Data.Maybe
import Linear (V3(..))
import Test.Tasty
import Test.Tasty.Hspec

import Hgal.Graph.Class
import qualified Hgal.Graph.ClassM as M
import Hgal.Graph.EulerOperations as Euler
import Hgal.Graph.Fixtures
import Hgal.Graph.Helpers
import Hgal.Graph.Loops
import Hgal.Graph.Predicates
import Hgal.Data.SurfaceMesh (SurfaceMesh)


test_eulerOpertations :: IO TestTree
test_eulerOpertations = do
  specs <- concat <$> mapM testSpecs
           [ joinFaceTest @Double @(SurfaceMesh (V3 Double) () () ())
           , removeFaceTest1 @Double @(SurfaceMesh (V3 Double) () () ())
           , removeFaceTest2 @Double @(SurfaceMesh (V3 Double) () () ())
           , addFaceToBorderTest @Double @(SurfaceMesh (V3 Double) () () ())
           , addVertexAndFaceToBorderTest @Double @(SurfaceMesh (V3 Double) () () ())
           , joinVertexInteriorTest @Double @(SurfaceMesh (V3 Double) () () ())
           , joinVertexExteriorTest1 @Double @(SurfaceMesh (V3 Double) () () ())
           , joinVertexExteriorTest2 @Double @(SurfaceMesh (V3 Double) () () ())
           , splitVertexTest @Double @(SurfaceMesh (V3 Double) () () ())
           , splitJoinVertexInverseTest @Double @(SurfaceMesh (V3 Double) () () ())
           , joinLoopTest @Double @(SurfaceMesh (V3 Double) () () ())
           , splitLoopTest @Double @(SurfaceMesh (V3 Double) () () ())
           , splitFaceTest @Double @(SurfaceMesh (V3 Double) () () ())
           , makeHoleTest @Double @(SurfaceMesh (V3 Double) () () ())
           ]
  return $ testGroup "EulerOperations" [testGroup "SurfaceMesh static test" specs]


testFaceFixture :: FaceGraph g
                => M.FaceGraph (State g) g
                => (Eq (V g), Eq (H g), Eq (F g), Show (V g), Show (H g), Show (F g))
                => FaceFixture g -> Spec
testFaceFixture f = do
  let g = faceFixture f
  it "valid fixture" $ do
    either id show (isValidPolygonMesh g) `shouldBe` "True"
    all (\x -> degree g x /= 0) (($ f) <$> [u, v, w, x, y, z]) `shouldBe` True

testHalfedgeFixture :: FaceGraph g
                    => M.FaceGraph (State g) g
                    => (Eq (V g), Eq (H g), Eq (F g), Show (V g), Show (H g), Show (F g))
                    => HalfedgeFixture g -> Spec
testHalfedgeFixture f = do
  let g = halfedgeFixture f
  it "valid fixture" $ do
    either id show (isValidPolygonMesh g) `shouldBe` "True"
    all (\x -> degree g x /= 0) (($ f) <$> [h1, h2, h3]) `shouldBe` True

joinFaceTest :: forall a g. SurfaceFixtureC a g => Spec
joinFaceTest = do
  f <- runIO (surfaceFixture1 @a @g)
  describe "joinFace" $ do
      testFaceFixture f
      let g = faceFixture f
          e = fromJust $ halfedgeVV g (w f) (v f)
          (e', g') = Euler.joinFace (setHalfedge g (f1 f) e) e
      it "edges count" $ do
        exactNumEdges g' `shouldBe` 6
      it "faces count" $ do
        exactNumFaces g' `shouldBe` 2
      context "halfedges check" $ do
        let haf = halfedgesAroundFace g' (halfedge g' (f1 f))
        it "halfedgesAroundFace count" $ do
          length haf `shouldBe` 4
        it "halfedges have correct face" $ do
          all ((== f1 f) . face g') haf `shouldBe` True
      it "deleted face is no more" $ do
        all (\x -> x == f1 f || x == f3 f) (faces g') `shouldBe` True
      it "vertices degree" $ do
        degree g' (w f) `shouldBe` 2
        degree g' (v f) `shouldBe` 3
      it "valid poylygon mesh" $ do
        either id show (isValidPolygonMesh g') `shouldBe` "True"


removeFaceTest1 :: forall a g. SurfaceFixtureC a g => Spec
removeFaceTest1 = do
  f <- runIO (surfaceFixture1 @a @g)
  describe "removeFaceTest1" $ do
    testFaceFixture f
    let g = faceFixture f
        e = fromJust $ halfedgeVV g (x f) (y f)
        g' = Euler.removeFace g e
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` 4
    it "edges count" $ do
      exactNumEdges g' `shouldBe` 5
    it "border halfedges count" $ do
      (length . filter (isBorder g') $ halfedges g') `shouldBe` 4
    it "faces count" $ do
      exactNumFaces g' `shouldBe` 2
    it "vertices degree" $ do
      degree g' (v f) `shouldBe` 3
      degree g' (x f) `shouldBe` 2
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

removeFaceTest2 :: forall a g. SurfaceFixtureC a g => Spec
removeFaceTest2 = do
  f <- runIO (surfaceFixture2 @a @g)
  describe "removeFaceTest2" $ do
    testFaceFixture f
    let g = faceFixture f
        e = fromJust $ halfedgeVV g (x f) (v f)
        g' = Euler.removeFace g e
        e' = fromJust $ halfedgeVV g' (x f) (w f)
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` 5
    it "edges count" $ do
      exactNumEdges g' `shouldBe` 7
    it "halfedges have border face" $ do
      all ((== outerFace g') . face g') (halfedgesAroundFace g' e') `shouldBe` True
    it "faces count" $ do
      exactNumFaces g' `shouldBe` 3
    it "face removed" $ do
      face g e' == outerFace g `shouldBe` True
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

addFaceToBorderTest :: forall a g. SurfaceFixtureC a g => Spec
addFaceToBorderTest = do
  f <- runIO (surfaceFixture5 @a @g)
  describe "addFaceToBorderTest" $ do
    testHalfedgeFixture f
    let g = halfedgeFixture f
        (_, g') = Euler.addFaceToBorder g (h1 f) (h2 f)
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

addVertexAndFaceToBorderTest :: forall a g. SurfaceFixtureC a g => Spec
addVertexAndFaceToBorderTest = do
  f <- runIO (surfaceFixture5 @a @g)
  describe "addVertexAndFaceToBordetTest" $ do
    testHalfedgeFixture f
    let g = halfedgeFixture f
        bl = length $ halfedgesAroundFace g (h1 f)
    it "halfedge distance" $ do
      distance g next (h1 f) (h2 f) `shouldBe` Just 2
    let (e', g') = Euler.addVertexAndFaceToBorder g (h1 f) (h2 f)
    it "correct border" $ do
      isBorder g' e' `shouldBe` False
      isBorder g' (opposite g' e') `shouldBe` True
    it "halfege count" $ do
      length (halfedgesAroundFace g' (opposite g' e')) `shouldBe` bl
    it "is valid polygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

joinVertexInteriorTest :: forall a g. SurfaceFixtureC a g => Spec
joinVertexInteriorTest = do
  f <- runIO (surfaceFixture3 @a @g)
  describe "joinVertexInteriorTest" $ do
    testFaceFixture f
    let g = faceFixture f
        e = fromJust $ halfedgeVV g (w f) (x f)
        (e', g') = Euler.joinVertex g e
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` 5
    it "edges count" $ do
      exactNumEdges g' `shouldBe` 6
    it "halfedges count" $ do
      length (halfedgesAroundFace g' (halfedge g' (f1 f))) `shouldBe` 3
      length (halfedgesAroundFace g' (halfedge g' (f2 f))) `shouldBe` 3
    it "faces count" $ do
      exactNumFaces g' `shouldBe` 2
    it "vertices degree" $ do
      degree g' (x f) `shouldBe` 4
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

joinVertexExteriorTest1 :: forall a g. SurfaceFixtureC a g => Spec
joinVertexExteriorTest1 = do
  f <- runIO (surfaceFixture3 @a @g)
  describe "joinVertexExteriorTest1" $ do
    testFaceFixture f
    let g = faceFixture f
        e = fromJust $ halfedgeVV g (w f) (y f)
    it "fixture validity" $ do
      source g e `shouldBe` w f
      target g e `shouldBe` y f
    let (e', g') = Euler.joinVertex g e
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` 5
    it "halfedges count" $ do
      length (halfedgesAroundFace g' (halfedge g' (f1 f))) `shouldBe` 4
      length (halfedgesAroundFace g' (halfedge g' (f2 f))) `shouldBe` 3
    it "edges count" $ do
      exactNumEdges g' `shouldBe` 6
    it "faces count" $ do
      exactNumFaces g' `shouldBe` 2
    it "vertices degree" $ do
      degree g' (y f) `shouldBe` 3
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

joinVertexExteriorTest2 :: forall a g. SurfaceFixtureC a g => Spec
joinVertexExteriorTest2 = do
  f <- runIO (surfaceFixture3 @a @g)
  describe "joinVertexExteriorTest2" $ do
    testFaceFixture f
    let g = faceFixture f
        e = fromJust $ halfedgeVV g (y f) (w f)
        (e', g') = Euler.joinVertex g e
    it "fixture validity" $ do
      source g e `shouldBe` y f
      target g e `shouldBe` w f
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` 5
    it "halfedges count" $ do
      length (halfedgesAroundFace g' (halfedge g' (f1 f))) `shouldBe` 4
      length (halfedgesAroundFace g' (halfedge g' (f2 f))) `shouldBe` 3
    it "edges count" $ do
      exactNumEdges g' `shouldBe` 6
    it "faces count" $ do
      exactNumFaces g' `shouldBe` 2
    it "vertices degree" $ do
      degree g' (w f) `shouldBe` 3
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

splitVertexTest :: forall a g. SurfaceFixtureC a g => Spec
splitVertexTest = do
  f <- runIO (surfaceFixture3 @a @g)
  describe "splitVertexTest" $ do
    testFaceFixture f
    let g = faceFixture f
        h1 = fromJust $ halfedgeVV g (w f) (y f)
        h2 = fromJust $ halfedgeVV g (z f) (y f)
    it "fixture validity" $ do
      face g h2 `shouldBe` outerFace g
    let (e', g') = Euler.splitVertex g h1 h2
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` 7
    it "halfedges count" $ do
      length (halfedgesAroundFace g' h1) `shouldBe` 5
      length (halfedgesAroundFace g' h2) `shouldBe` 7
    it "edges count" $ do
      exactNumEdges g' `shouldBe` 8
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

splitJoinVertexInverseTest :: forall a g. SurfaceFixtureC a g => Spec
splitJoinVertexInverseTest = do
  f <- runIO (surfaceFixture3 @a @g)
  describe "splitJoinVertexInverseTest" $ do
    testFaceFixture f
    let g = faceFixture f
        h = fromJust $ halfedgeVV g (w f) (x f)
        (_, g1) = Euler.joinVertex g h
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g1) `shouldBe` "True"
    let h1 = fromJust $ halfedgeVV g1 (z f) (x f)
        h2 = fromJust $ halfedgeVV g1 (v f) (x f)
        (e, g2) = Euler.splitVertex g1 h1 h2
        (_, g3) = Euler.joinVertex g2 e
    it "vertex count" $ do
      exactNumVertices g3 `shouldBe` 5
    it "halfedges count" $ do
      exactNumHalfedges g3 `shouldBe` 12
      length (halfedgesAroundFace g3 h1) `shouldBe` 3
      length (halfedgesAroundFace g3 h2) `shouldBe` 3
    it "edges count" $ do
      exactNumEdges g3 `shouldBe` 6
    it "faces count" $ do
      exactNumFaces g3 `shouldBe` 2
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g3) `shouldBe` "True"

joinLoopTest :: forall a g. SurfaceFixtureC a g => Spec
joinLoopTest = do
  f <- runIO (surfaceFixture4 @a @g)
  describe "joinLoopTest" $ do
    testHalfedgeFixture f
    let g = halfedgeFixture f
        (e', g') = Euler.joinLoop g (h1 f) (h2 f)
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

splitLoopTest :: forall a g. SurfaceFixtureC a g => Spec
splitLoopTest = do
  f <- runIO (surfaceFixture8 @a @g)
  describe "splitLoopTest" $ do
    testHalfedgeFixture f
    let g = halfedgeFixture f
        (e', g') = Euler.splitLoop g (h1 f) (h2 f) (h3 f)
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` 8
    it "halfedge count" $ do
      exactNumHalfedges g' `shouldBe` 24
    it "face count" $ do
      exactNumFaces g' `shouldBe` 8
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

splitFaceTest :: forall a g. SurfaceFixtureC a g => Spec
splitFaceTest = do
  f <- runIO (surfaceFixture6 @a @g)
  describe "splitFaceTest" $ do
    testHalfedgeFixture f
    let g = halfedgeFixture f
        (e', g') = Euler.splitFace g (h1 f) (h2 f)
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` 4
    it "halfedge count" $ do
      exactNumHalfedges g' `shouldBe` 10
    it "face count" $ do
      exactNumFaces g' `shouldBe` 2
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"

makeHoleTest :: forall a g. SurfaceFixtureC a g => Spec
makeHoleTest = do
  f <- runIO (surfaceFixture7 @a @g)
  describe "makeHoleTest" $ do
    testHalfedgeFixture f
    let g = halfedgeFixture f
        nv = exactNumVertices g
        nf = exactNumFaces g
        nh = exactNumHalfedges g
        g' = Euler.makeHole g (h1 f)
    it "vertex count" $ do
      exactNumVertices g' `shouldBe` nv
    it "halfedge count" $ do
      exactNumHalfedges g' `shouldBe` nh
    it "face count" $ do
      exactNumFaces g' `shouldBe` (nf - 1)
    it "valid poylygon mesh" $ do
      either id show (isValidPolygonMesh g') `shouldBe` "True"
