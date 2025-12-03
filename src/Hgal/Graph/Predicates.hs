module Hgal.Graph.Predicates where

import Control.Lens
import Control.Monad.State
import Data.Bifunctor
import Data.Either

import Hgal.Graph.Class
import Hgal.Graph.Helpers
import qualified Hgal.Graph.ClassM as M
import Hgal.Graph.Loops


isTriangle :: HalfedgeGraph g => Eq (H g) => g -> H g -> Bool
isTriangle g h = h == next3 g h

isIsolatedTriangle :: HalfedgeGraph g
                   => M.HalfedgeGraph (State g) g
                   => Eq (H g)
                   => g -> H g -> Bool
isIsolatedTriangle g h =
  isTriangle g h && allOf traversed (isBorder g . opposite g) (halfedgesAroundFace g h)

isTriangleMesh :: FaceGraph g => Eq (H g) => g -> Bool
isTriangleMesh g = allOf traversed (isTriangle g . halfedge g) (faces g)

isQuad :: HalfedgeGraph g => Eq (H g) => g -> H g -> Bool
isQuad g h = h == (next g . next3 g $ h)

isIsolatedQuad :: HalfedgeGraph g
               => M.HalfedgeGraph (State g) g
               => Eq (H g)
               => g -> H g -> Bool
isIsolatedQuad g h =
  isQuad g h && allOf traversed (isBorder g . opposite g) (halfedgesAroundFace g h)

isQuadMesh :: FaceGraph g => Eq (H g) => g -> Bool
isQuadMesh g = allOf traversed (isQuad g . halfedge g) (faces g)

isTetrahedron :: HalfedgeGraph g => Eq (H g) => g -> H g -> Bool
isTetrahedron g h1 =
  let h2 = next g h1
      h3 = next g h2
      h4 = next g (opposite g h1)
      h5 = next g (opposite g h2)
      h6 = next g (opposite g h3)
  in not $ isBorder g h1
        || h4 == opposite g h3
        || h5 == opposite g h1
        || h6 == opposite g h2
        || next g (opposite g h4) /= opposite g h3
        || next g (opposite g h5) /= opposite g h1
        || next g (opposite g h6) /= opposite g h2
        || opposite g (next g h4) /= h5
        || opposite g (next g h5) /= h6
        || opposite g (next g h6) /= h4
        || any not (isTriangle g <$> [h1, h4, h5, h6])
        || or (isBorder g <$> [h1, h4, h5, h6])

isHexahedron :: HalfedgeGraph g => Eq (H g) => g -> H g -> Bool
isHexahedron g h1 =
  let h2 = next g h1
      h3 = next g h2
      h4 = next g h3
      h1o = opposite g h1
      h2o = opposite g h2
      h3o = opposite g h3
      h4o = opposite g h4
      h1o' = opposite g (next2 g h1o)
      h2o' = opposite g (next2 g h2o)
      h3o' = opposite g (next2 g h3o)
      h4o' = opposite g (next2 g h4o)
  in not $ isBorder g h1
        || opposite g (next g h2o) /= prev g h1o
        || opposite g (next g h3o) /= prev g h2o
        || opposite g (next g h4o) /= prev g h3o
        || opposite g (next g h1o) /= prev g h4o
        || any not (isQuad g <$> [h1, h1o, h2o, h3o, h4o])
        || next g h2o' /= h1o'
        || next g h3o' /= h2o'
        || next g h4o' /= h3o'
        || next g h1o' /= h4o'
        || not (isQuad g h4o')

isValidHalfedgeGraph :: HalfedgeGraph g
                     => M.HalfedgeGraph (State g) g
                     => (Eq (V g), Eq (H g), Show (V g), Show (H g))
                     => g -> Either String Bool
isValidHalfedgeGraph g =
   first (<> "Halfedge Graph Structure is NOT VALID.\n") $
   firstError [oddCheck, hsI, vsI, countHalfedges, geometryI]
  where
    hsI = second and (mapM (halfedgeIntegrity g) $ halfedges g)
    vsI = second and (mapM (vertexIntegrity g) $ vertices g)
    oddCheck
      | (odd . exactNumHalfedges $ g) &&
        (2 * exactNumEdges g == exactNumHalfedges g) = Left "Number of halfedges is odd.\n"
      | otherwise = Right True
    countHalfedges
      | vhCount /= exactNumHalfedges g = Left "Counting halfedges via vertices failed.\n"
      | otherwise = Right True
      where
        vhCount = lengthOf (folded.to (halfedgesAroundTarget g . halfedge g).folded) (vertices g)
    geometryI = second and (mapM (halfedgeG g) $ halfedges g)
      where
        halfedgeG g h
          | (next g h == h) || (target g h == target g (opposite g h)) = Left "Pointer validity corrupted.\n"
          | otherwise = Right True

isValidFaceGraph :: FaceGraph g
                 => M.FaceGraph (State g) g
                 => (Eq (V g), Eq (H g), Eq (F g), Show (V g), Show (H g), Show (F g))
                 => g -> Either String Bool
isValidFaceGraph g = firstError [isValidHalfedgeGraph g, fCheck]
  where
    fCheck = first (<> "Face Graph Structure is NOT VALID.\n") $
             firstError [fsI, countCheck, halfedgesI]
    fsI = second and (mapM (faceIntegrity g) $ faces g)
    countHalfedgesAroundFaces =
      lengthOf (folded.to (halfedgesAroundFace g . halfedge g).folded) (faces g)
    countBorderHalfedges = length . filter (isBorder g) . halfedges $ g
    countCheck
      | countHalfedgesAroundFaces + countBorderHalfedges == exactNumHalfedges g = Right True
      | otherwise = Left "Counting halfedges via faces failed.\n"
    halfedgesI = second and (mapM (halfedgeF g) $ halfedges g)
      where
        halfedgeF g h
          | face g h /= face g (next g h) = Left $ "Halfedge " ++ show h ++ " and its next have a differet face.\n"
          | otherwise = Right True

isValidPolygonMesh :: FaceGraph g
                   => M.FaceGraph (State g) g
                   => (Eq (V g), Eq (H g), Eq (F g), Show (V g), Show (H g), Show (F g))
                   => g -> Either String Bool
isValidPolygonMesh g = firstError [isValidFaceGraph g, geometryI]
  where
    geometryI = bimap (<> "Polygon Mesh Structure is NOT VALID.\n") and
                (mapM (halfedgeG g) $ halfedges g)
    halfedgeG g h
      | face g h == face g (opposite g h) = Left "Both incident facets are equal.\n"
      | next g nexth == h || target g h == target g nexth || target g h == target g (next g nexth) =
        Left "Incident facet is not at least a triangle.\n"
      | otherwise = Right True
      where nexth = next g h


vertexIntegrity :: HalfedgeGraph g
                => (Eq (V g), Show (V g))
                => g -> V g -> Either String Bool
vertexIntegrity g v
  | fromRight False (isValid g h) && target g (halfedge g v) == v = Right True
  | otherwise = Left $ "Halfedge of vertex " ++ show v ++ " is not an incoming halfedge.\n"
  where h = halfedge g v

halfedgeIntegrity :: HalfedgeGraph g
                  => (Eq (V g), Eq (H g), Show (H g))
                  => g -> H g -> Either String Bool
halfedgeIntegrity g h = firstError [halfedgeI, edgeI, oppositeI, previousI, nextI, vertexI, nextOppositeI]
  where
    halfedgeI =
      first (\s -> "Integrity of halfedge " ++ show h ++ " corrupted.\n" ++ s) $
      collectErrors [isValid g (next g h), isValid g (opposite g h)]
    edgeI
      | halfedge g (edge g h) == h = Right True
      | otherwise = Left $ "Integrity of edge of " ++ show h ++ " corrupted.\n"
    oppositeI
      | opposite g h /= h && opposite g (opposite g h) == h = Right True
      | otherwise = Left $ "Integrity of opposite halfedge of " ++ show h ++ " corrupted.\n"
    previousI
      | next g (prev g h) == h = Right True
      | otherwise = Left $ "Integrity of previous halfedge of " ++ show h ++ " corrupted.\n"
    nextI
      | prev g (next g h) == h = Right True
      | otherwise = Left $ "Integrity of next halfedge of " ++ show h ++ " corrupted.\n"
    vertexI =
      first (\s -> "Integrity of vertex of halfedge " ++ show h ++ " corrupted.\n" ++ s) $
      collectErrors [isValid g (target g h)]
    nextOppositeI
      | target g (opposite g (next g h)) == target g h = Right True
      | otherwise = Left $ "Halfedge vertex of next opposite is not the same for " ++ show h ++ ".\n"

faceIntegrity :: FaceGraph g
              => (Eq (F g), Show (F g))
              => g -> F g -> Either String Bool
faceIntegrity g f = faceI
  where
    faceI
      | fromRight False (isValid g h) && face g h == f = Right True
      | otherwise = Left $ "Halfedge of face " ++ show f ++ " points to a different face.\n"
      where h = halfedge g f


collectErrors :: [Either String Bool] -> Either String Bool
collectErrors es =
  let right = Right (and (rights es))
      left = Left (unlines (lefts es))
  in if Prelude.null (lefts es) then right else left

firstError :: [Either String Bool] -> Either String Bool
firstError = second and . sequence
