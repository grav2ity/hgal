{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Hgal.Graph.GeneratorsTest where

import Linear (V3(..), zero)
import Test.Tasty
import Test.Tasty.Hspec

import Hgal.Graph.Class
import Hgal.Graph.Fixtures
import Hgal.Graph.Generators
import Hgal.Graph.Helpers
import Hgal.Graph.Predicates
import Hgal.Data.SurfaceMesh (SurfaceMesh)
import qualified Hgal.Data.SurfaceMesh as SurfaceMesh


test_Generators :: IO TestTree
test_Generators = do
  specs <- concat <$> mapM testSpecs
           [ testGenerators @Double @(SurfaceMesh (V3 Double) () () ()) (SurfaceMesh.empty)
           ]
  return $ testGroup "Generators" [testGroup "Surface Mesh" specs]

testGenerators :: forall a g v h e f p. SurfaceFixtureC a g v h e f p
               => Ord v
               => Floating a
               => g -> Spec
testGenerators g = do
  let [a, b, c, d] = [V3 0 0 0, V3 1 0 0, V3 1 1 0, V3 0 1 0]
  let [aa, bb, cc, dd] = [V3 0 0 1, V3 1 0 1, V3 1 1 1, V3 0 1 1]
  it "makeTriangle test" $ do
    let (h, tri) = makeTriangle g a b c
    isIsolatedTriangle tri h `shouldBe` True
    either id show (isValidPolygonMesh tri) `shouldBe` "True"
  it "makeQuad test" $ do
    let (h, quad) = makeQuad g a b c d
    isIsolatedQuad quad h `shouldBe` True
    isQuadMesh quad `shouldBe` True
    either id show (isValidPolygonMesh quad) `shouldBe` "True"
  it "makeTetrahedron test" $ do
    let (h, tet) = makeTetrahedron g a b c d
    isTetrahedron tet h `shouldBe` True
    isTriangleMesh tet `shouldBe` True
    either id show (isValidPolygonMesh tet) `shouldBe` "True"
  it "makeHexahedron test" $ do
    let (h, hex) = makeHexahedron g a b c d dd aa bb cc
    isHexahedron hex h `shouldBe` True
    isQuadMesh hex `shouldBe` True
    either id show (isValidPolygonMesh hex) `shouldBe` "True"
  it "makeIcosahedron test" $ do
    let (h, ico) = makeIcosahedron g zero 1
    exactNumFaces ico `shouldBe` 20
    isTriangleMesh ico `shouldBe` True
    either id show (isValidPolygonMesh ico) `shouldBe` "True"
  it "makePyramid test" $ do
    let (h, pyr) = makePyramid g 3 zero 1 1 True
    exactNumFaces pyr `shouldBe` 6
    isTriangleMesh pyr `shouldBe` True
    either id show (isValidPolygonMesh pyr) `shouldBe` "True"
  it "makeRegularPrism test" $ do
    let (h, pri) = makeRegularPrism g 4 zero 1 1 True
    exactNumFaces pri `shouldBe` 16
    isTriangleMesh pri `shouldBe` True
    either id show (isValidPolygonMesh pri) `shouldBe` "True"
  it "makeGrid test" $ do
    let (h, grid) = makeGrid g 3 3 (\x y -> fromIntegral <$> V3 x y 0) False
    exactNumFaces grid `shouldBe` 9
    isQuadMesh grid `shouldBe` True
    either id show (isValidPolygonMesh grid) `shouldBe` "True"
