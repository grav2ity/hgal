module Hgal.Data.SurfaceMeshTest where

import Data.Maybe
import Data.Traversable
import Linear
import Test.Tasty
import Test.Tasty.Hspec

import Hgal.Data.SurfaceMesh
import Hgal.Graph.Loops

test_surfaceMesh :: IO TestTree
test_surfaceMesh = do
  specs <- concat <$> mapM testSpecs
           [ loops
           , validateFixtures
           , removeEdgeSpec
           , unconnectedVertex
           , embeddedVertex
           , borderVertex
           ]
  return $ testGroup "SurfaceMesh" [testGroup "Static fixture tests" specs]

u, v, w, x, y, z :: Vertex
[u, v, w, x, y, z] = Vertex <$> [0..5]

loops :: Spec
loops =
  describe "loops check" $ do
    let f = surfaceFixture
    it "correct number of elements" $ do
      length (verticesAroundTarget f (halfedge f u)) `shouldBe` 2
      length (verticesAroundTarget f (halfedge f v)) `shouldBe` 4
      length (verticesAroundTarget f (halfedge f w)) `shouldBe` 3
      length (verticesAroundTarget f (halfedge f x)) `shouldBe` 3

      length (halfedgesAroundTarget f (halfedge f v)) `shouldBe` 4
      length (facesAroundTarget f (halfedge f v)) `shouldBe` 4
      length (verticesAroundFace f (halfedge f (Face 0))) `shouldBe` 3
      length (halfedgesAroundFace f (halfedge f (Face 0))) `shouldBe` 3

validateFixtures :: Spec
validateFixtures =
  describe "fixture validation" $ do
    it "surfaceFixture" $ do
      checkIntegrity surfaceFixture `shouldBe` Right True
    it "surfaceFixture2" $ do
      checkIntegrity surfaceFixture `shouldBe` Right True
    it "surfaceFixture3" $ do
      checkIntegrity surfaceFixture `shouldBe` Right True

removeEdgeSpec :: Spec
removeEdgeSpec =
  describe "remove edge" $ do
    let f = surfaceFixture
        hs = uncurry (halfedgeVV f) <$> [(w, v), (u, w), (w, x), (x, v), (v, u)]
    it "got halfedges from pairs of vertices" $ do
        all isJust hs `shouldBe` True
    let hs'@[wv, uw, wx, xv, vu] = fromJust <$> hs
    it "and they are valid" $ do
        all ((== Right True) . isValid f) hs' `shouldBe` True
    context "ater removing edge wv" $ do
      let f' = removeEdge (setNext (setNext f xv vu) uw wx) (edge f wv)
      it "w and v are in a correct state" $ do
        length (halfedgesAroundTarget f' (halfedge f' w)) `shouldBe` 2
        length (halfedgesAroundTarget f' (halfedge f' v)) `shouldBe` 3
        degree f' w `shouldBe` 2
        degree f' v `shouldBe` 3

unconnectedVertex :: Spec
unconnectedVertex =
  describe "unconnected vertex" $ do
    let (f, p) = newVertex surfaceFixture (V3 10 10 10)
    it "is isolated" $ do
      isIsolated f p `shouldBe` True
    it "is on border" $ do
      isBorder f p `shouldBe` True
    it "its incident halfedge does not exist" $ do
      halfedge f p `shouldBe` nullE
    it "its degree equals 0" $ do
      degree f p `shouldBe` 0

embeddedVertex :: Spec
embeddedVertex =
  describe "embedded vertex" $ do
    let f = surfaceFixture2
    it "is not isolated" $ do
      isIsolated f y `shouldBe` False
    it "is not on border" $ do
      isBorder f y `shouldBe` False
    it "its incident halfedge does exist" $ do
      halfedge f y /= nullE `shouldBe` True
    it "has a correct degree" $ do
      degree f y `shouldBe` 4

borderVertex :: Spec
borderVertex =
  describe "border vertex" $ do
    let f = surfaceFixture
    it "is not isolated" $ do
      isIsolated f y `shouldBe` False
    it "is on border" $ do
      isBorder f y `shouldBe` True
    it "its incident halfedge does exist" $ do
      halfedge f y /= nullE `shouldBe` True
    it "has a correct degree" $ do
      degree f y `shouldBe` 2
    context "after setting its halfedge to interior" $ do
      let h = halfedge f y
          h' = opposite (next f h)
          f' = setHalfedge f y h'
      it "is still on border" $ do
        isBorder f' y `shouldBe` True
      it "but its incident halfedge is not" $ do
        isBorder f' (halfedge f' y) `shouldBe` False


surfaceFixture :: Num a => SurfaceMesh (V3 a) () () ()
surfaceFixture = sm'
  where vs = [V3 0 0 0, V3 1 0 0, V3 0 1 0, V3 1 1 0, V3 2 0 0]
        (sm, [u, v, w, x, y]) = mapAccumL newVertex (empty) vs
        (sm', _) = mapAccumL newFace sm [[u, w, v], [v, w, x], [v, x, y]]

surfaceFixture2 :: Num a => SurfaceMesh (V3 a) () () ()
surfaceFixture2 = sm'
  where vs = [V3 0 2 0, V3 2 2 0, V3 0 0 0, V3 2 0 0, V3 1 1 0]
        (sm, [u, v, w, x, y]) = mapAccumL newVertex (empty) vs
        (sm', _) = mapAccumL newFace sm [[x, v, y], [u, y, v], [u, w, y], [w, x, y]]

surfaceFixture3 :: Num a => SurfaceMesh (V3 a) () () ()
surfaceFixture3 = sm'
  where vs = [V3 0 1 0, V3 0 0 0, V3 1 0 0, V3 1 1 0, V3 2 0 0, V3 2 1 0]
        (sm, [u, v, w, x, y, z]) = mapAccumL newVertex (empty) vs
        (sm', _) = mapAccumL newFace sm [[u, v, w, x], [x, w, y, z]]
