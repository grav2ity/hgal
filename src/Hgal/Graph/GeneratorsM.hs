module Hgal.Graph.GeneratorsM where

import Control.Exception
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.Vector ((!))
import qualified Data.Vector as V
import Linear

import Hgal.Graph.ClassM
import Hgal.Data.PropertyM
import qualified Hgal.Graph.EulerOperationsM as Euler


{-|
  Create an isolated triangle
  with its vertices initialized to `p0`, `p1` and `p2`, and adds it to the graph `g`.
-}
makeTriangle :: MutableFaceGraph m g
             => Property m g (V g) p
             => g
             -> p   -- ^ p0
             -> p   -- ^ p1
             -> p   -- ^ p2
             -> m (H g) -- ^ the non-border halfedge that has the target vertex
                    -- associated with `p0`
makeTriangle g p0 p1 p2 = do
  v0 <- addVertex g
  v1 <- addVertex g
  v2 <- addVertex g
  h0 <- halfedge =<< addEdge g
  h1 <- halfedge =<< addEdge g
  h2 <- halfedge =<< addEdge g
  mapM_ (uncurry setNext) [(h0, h1), (h1, h2), (h2, h0)]
  mapM_ (uncurry setTarget) [(h0, v1), (h1, v2), (h2, v0)]
  mapM_ (uncurry setHalfedge) [(v1, h0), (v2, h1), (v0, h2)]
  f <- addFace g
  mapM_ (setFace ?? f) [h0, h1, h2]
  setHalfedge f h0
  h0' <- opposite h0
  h1' <- opposite h1
  h2' <- opposite h2
  mapM_ (uncurry setNext) [(h0', h2'), (h2', h1'), (h1', h0')]
  mapM_ (uncurry setTarget) [(h0', v0), (h1', v1), (h2', v2)]
  outerF <- outerFace g
  mapM_ (setFace ?? outerF) [h0', h1', h2']

  mapM_ (uncurry (replaceProperty g)) [(v0, p0), (v1, p1), (v2, p2)]

  opposite h2'

{-|
  Create an isolated quad
  with its vertices initialized to `p0`, `p1`, `p2` and `p3`, and adds it to the graph `g`.
-}
makeQuad :: MutableFaceGraph m g
         => Property m g (V g) p
         => g
         -> p   -- ^ p0
         -> p   -- ^ p1
         -> p   -- ^ p2
         -> p   -- ^ p3
         -> m (H g) -- ^ the non-border halfedge that has the target vertex associated with `p0`
makeQuad g p0 p1 p2 p3 = do
  vs <- replicateM 4 (addVertex g)
  let [v0, v1, v2, v3] = vs
  mapM_ (uncurry (replaceProperty g)) [(v0, p0), (v1, p1), (v2, p2), (v3, p3)]
  formQuad g v0 v1 v2 v3

formQuad :: MutableFaceGraph m g
         => g -> V g -> V g -> V g -> V g -> m (H g)
formQuad g v0 v1 v2 v3 = do
  hs <- replicateM 4 (halfedge =<< addEdge g)
  let [h0, h1, h2, h3] = hs
  mapM_ (uncurry setNext) [(h0, h1), (h1, h2), (h2, h3), (h3, h0)]
  mapM_ (uncurry setTarget) [(h0, v1), (h1, v2), (h2, v3), (h3, v0)]
  mapM_ (uncurry setHalfedge) [(v1, h0), (v2, h1), (v3, h2), (v0, h3)]
  f <- addFace g
  mapM_ (setFace ?? f) [h0, h1, h2, h3]
  setHalfedge f h0
  hs' <- mapM opposite hs
  let [h0', h1', h2', h3'] = hs'
  mapM_ (uncurry setNext) [(h0', h3'), (h3', h2'), (h2', h1'), (h1', h0')]
  mapM_ (uncurry setTarget) [(h0', v0), (h1', v1), (h2', v2), (h3', v3)]
  outerF <- outerFace g
  mapM_ (setFace ?? outerF) [h0', h1', h2', h3']
  opposite h3'


{-|
  Create an isolated tetrahedron
  with its vertices initialized to `p0`, `p1`, `p2`, and `p3`, and adds it to the graph `g`.
-}
makeTetrahedron :: MutableFaceGraph m g
                => Property m g (V g) p
                => g
                -> p   -- ^ p0
                -> p   -- ^ p1
                -> p   -- ^ p2
                -> p   -- ^ p3
                -> m (H g) -- ^ the halfedge that has the target vertex associated
                       -- with `p0`, in the face with the vertices with the
                       -- points `p0`, `p1`, and `p2`
makeTetrahedron g p0 p1 p2 p3 = do
  vs <- replicateM 4 (addVertex g)
  let [v0, v2, v1, v3] = vs -- v2 and v1 are switched to keep points in order
  hs <- replicateM 6 (halfedge =<< addEdge g)
  let [h0, h1, h2, h3, h4, h5] = hs
  mapM_ (uncurry setNext) [(h0, h1), (h1, h2), (h2, h0)]
  mapM_ (uncurry setTarget) [(h0, v1), (h1, v2), (h2, v0)]
  mapM_ (uncurry setHalfedge) [(v1, h0), (v2, h1), (v0, h2)]
  f <- addFace g
  mapM_ (setFace ?? f) [h0, h1, h2]
  setHalfedge f h0

  mapM_ (uncurry setTarget) [(h3, v3), (h4, v3), (h5, v3)]
  setHalfedge v3 h3

  hs' <- mapM opposite [h0, h1, h2, h3, h4, h5]
  let [h0', h1', h2', h3', h4', h5'] = hs'

  mapM_ (uncurry setNext) [(h0', h2'), (h2', h1'), (h1', h0')]
  mapM_ (uncurry setTarget) [(h0', v0), (h1', v1), (h2', v2)]
  mapM_ (uncurry setNext) [(h0', h3), (h1', h4), (h2', h5)]
  mapM_ (uncurry setNext) [(h3, h4'), (h4, h5'), (h5, h3'), (h4', h0'), (h5', h1'), (h3', h2')]
  mapM_ (uncurry setTarget) [(h3', v0), (h4', v1), (h5', v2)]

  f2 <- addFace g
  setHalfedge f2 h0'
  setFace h0' f2
  setFace h3 f2
  (setFace ?? f2) =<< opposite h4
  f3 <- addFace g
  setHalfedge f3 h1'
  setFace h1' f3
  setFace h4 f3
  (setFace ?? f3) =<< opposite h5
  f4 <- addFace g
  setHalfedge f4 h2'
  setFace h2' f4
  setFace h5 f4
  (setFace ?? f4) =<< opposite h3

  -- p1 and p2 are switched to reorient the surface
  mapM_ (uncurry (replaceProperty g)) [(v0, p0), (v1, p2), (v2, p1), (v3, p3)]

  opposite h2'

{-|
  Create an isolated hexahedron with its vertices initialized to
  `p0`, `p1`, ... , and `p7`, and adds it to the graph `g`.
-}
makeHexahedron :: MutableFaceGraph m g
               => Property m g (V g) p
               => Eq (H g)
               => g
               -> p   -- ^ p0
               -> p   -- ^ p1
               -> p   -- ^ p2
               -> p   -- ^ p3
               -> p
               -> p
               -> p
               -> p   -- ^ p7
               -> m (H g) -- ^ the halfedge that has the target vertex associated
                      -- with `p0`, in the face with the vertices with the
                      -- points `p0`, `p1`, `p2`, and `p3`
makeHexahedron g p0 p1 p2 p3 p4 p5 p6 p7 = do
  vs <- replicateM 8 (addVertex g)
  let [v0, v1, v2, v3, v4, v5, v6, v7] = vs
  ht <- formQuad g v4 v5 v6 v7
  hb <- prev =<< formQuad g v0 v3 v2 v1
  let
    worker (ht', hb') _ = do
      h <- halfedge =<< addEdge g
      setTarget h =<< target hb'
      setNext h =<< opposite hb'
      (setNext ?? h) =<< (opposite <=< prev) ht'
      h' <- opposite h
      setTarget h' =<< (source <=< prev) ht'
      setNext h' =<< (opposite <=< next <=< next) ht'
      (setNext ?? h') =<< (opposite <=< next) hb'
      liftM2 (,) (prev ht') (next hb')
    worker2 hb' _ = do
      Euler.fillHole g =<< opposite hb'
      next hb'
  (_, hb') <- foldM worker (ht, hb) [0..3]
  hb'' <- foldM worker2 hb' [0..3]

  mapM_ (uncurry $ replaceProperty g) (zip vs [p0, p1, p2, p3, p4, p5, p6, p7])

  (next <=< next) hb''


{-|
  Create a triangulated regular prism, outward oriented, and adds it to the graph `g`.

  If `center` is zero, then _xyz of the first point of the prism is (V3 radius height 0).
-}
makeRegularPrism :: MutableFaceGraph m g
                 => Property m g (V g) (p a)
                 => (Ord (V g), Eq (H g), Eq (F g))
                 => (Floating a, R3 p)
                 => g
                 -> Int  -- ^ number of vertices per base >= 3
                 -> p a  -- ^ center (of the circle in which the lower base is inscribed)
                 -> a    -- ^ height (distance between the two bases)
                 -> a    -- ^ radius (of the circles in which the bases are inscribed)
                 -> Bool -- ^ whether to create bases
                 -> m (H g)  -- ^ the halfedge that has the target vertex associated with the first point in the first face
makeRegularPrism g n center height radius isClosed = do
  let step = assert (n >= 3) $
             2 * pi / fromIntegral n

  vs <- V.replicateM (2*n) (addVertex g)

  forM_ [0..n-1] $ \i -> do
    let i' = fromIntegral i
        p1 = center & _x +~ (radius * cos (i' * step))
                    & _z -~ (radius * sin (i' * step))
        p2 = p1 & _y +~ height
    replaceProperty g (vs ! (i + n)) p1
    replaceProperty g (vs ! i) p2

  forM_ [0..n-1] $ \i -> do
    let ii = mod (i+1) n
    Euler.addFace g [vs ! ii, vs ! i, vs ! (ii + n)]
    Euler.addFace g [vs ! (ii + n), vs ! i, vs ! (i + n)]

  when isClosed $ do
    top <- addVertex g
    bot <- addVertex g
    replaceProperty g top (_y +~ height $ center)
    replaceProperty g bot center

    forM_ [0..n-1] $ \i -> do
      let ii = mod (i+1) n
      Euler.addFace g [vs ! i, vs ! ii, top]
      Euler.addFace g [bot, vs ! (ii + n), vs ! (i + n)]

  fromJust <$> halfedgeVV (vs ! 0) (vs ! 1)

{-|
  Create a pyramid, outward oriented, and adds it to the graph `g`.

  If `center` is zero, then _xyz of the first point of the base is (V3 radius 0 0).
-}
makePyramid :: MutableFaceGraph m g
            => Property m g (V g) (p a)
            => (Ord (V g), Eq (H g), Eq (F g))
            => (Floating a, R3 p)
            => g
            -> Int   -- ^ the nummber of vertices in the base >= 3
            -> p a   -- ^ the center of the circle in which the base is inscribed
            -> a     -- ^ height (distance between the base and the apex)
            -> a     -- ^ radius (of the cirlce in which the base is inscribed)
            -> Bool  -- ^ whether to create the base
            -> m (H g)   -- ^ the halfedge that has the target vertex associated
                     -- with the apex point in the first face
makePyramid g n center height radius isClosed = do
  let step = assert (n >= 3) $
             2 * pi / fromIntegral n

  apex <- addVertex g
  vs <- V.replicateM n (addVertex g)

  replaceProperty g apex (center & _y +~ height)

  forM_ [0..n-1] $ \i -> do
    let i' = fromIntegral i
        p = center & _x +~ (radius * cos (i' * step))
                   & _z -~ (radius * sin (i' * step))
    replaceProperty g (vs ! i) p

  forM_ [0..n-1] $ \i -> do
    let ii = mod (i+1) n
    Euler.addFace g [apex, vs ! i, vs ! ii]

  when isClosed $ do
    bot <- addVertex g
    replaceProperty g bot center

    forM_ [0..n-1] $ \i -> do
      let ii = mod (i+1) n
      Euler.addFace g [bot, vs ! ii, vs ! i]

  fromJust <$> halfedgeVV (vs ! 0) apex


-- | Create an icosahedron, outward oriented, and adds it to the graph `g`.
makeIcosahedron :: MutableFaceGraph m g
                => Property m g (V g) (p a)
                => (Ord (V g), Eq (H g), Eq (F g))
                => (Floating a, R3 p)
                => g
                -> p a -- ^ center (of the sphere in which the icosahedron is inscribed)
                -> a   -- ^ radius (of the sphere)
                -> m (H g) -- ^ the halfedge that has the target vertex associated
                       -- with the first point in the first face
makeIcosahedron g center radius = do

  vs <- V.replicateM 12 (addVertex g)

  let phi = (1 + sqrt 5) * 0.5
      t = radius / sqrt (1 + phi*phi)
      tphi = t * phi

      ps = V.fromList $ ($ center) <$>
        [ _yz +~ V2 t tphi
        , _yz +~ V2 t (-tphi)
        , _yz +~ V2 (-t) tphi
        , _yz +~ V2 (-t) (-tphi)
        , _xy +~ V2 t tphi
        , _xy +~ V2 t (-tphi)
        , _xy +~ V2 (-t) tphi
        , _xy +~ V2 (-t) (-tphi)
        , _xz +~ V2 tphi t
        , _xz +~ V2 tphi (-t)
        , _xz +~ V2 (-tphi) t
        , _xz +~ V2 (-tphi) (-t)
        ]

  V.zipWithM_ (replaceProperty g) vs ps

  let faceI =
        [ (0, 2, 8)
        , (0, 8, 4)
        , (0, 4, 6)
        , (0, 6, 10)
        , (0, 10, 2)
        , (1, 9, 3)
        , (1, 3, 11)
        , (1, 11, 6)
        , (1, 6, 4)
        , (1, 4, 9)
        , (5, 8, 2)
        , (5, 2, 7)
        , (5, 7, 3)
        , (5, 3, 9)
        , (5, 9, 8)
        , (8, 9, 4)
        , (3, 7, 11)
        , (11, 7, 10)
        , (10, 7, 2)
        , (6, 11, 10)
        ]

  mapM_ (\(i, j, k) -> Euler.addFace g [vs ! i, vs ! j, vs ! k] ) faceI

  fromJust <$> halfedgeVV (vs ! 5) (vs ! 0)

{-|
  Creates a row major ordered grid with `i` cells along the width and `j` cells
  along the height and adds it to the graph `g`.
-}
makeGrid :: MutableFaceGraph m g
         => Property m g (V g) p
         => (Ord (V g), Eq (H g), Eq (F g))
         => g
         -> Int               -- ^ i (number of cells along the width)
         -> Int               -- ^ j (number of cells along the height)
         -> (Int -> Int -> p) -- ^ function that assign coordinates to the grid vertices
         -> Bool              -- ^ whether each cell is composed of one quad or two triangles
                              -- if true, the diagonal of each cell is oriented from (0,0) to (1,1)
         -> m (H g)               -- ^ the non-border non-diagonal halfedge that has
                              -- the target vertex associated with the first point of the grid

makeGrid g i j coordF triangulated = do
  vs <- V.fromList <$> sequence
    [ addVertex g >>= \v -> replaceProperty g v (coordF x y) >> return v
    | x <- [0..i], y <- [0..j]
    ]
  sequence_
    [ if triangulated then
        Euler.addFace g [v0, v1, v3] >>
        Euler.addFace g [v1, v2, v3]
      else
        Euler.addFace g fvs
    | x <- [0..(i - 1)], y <- [0..(j - 1)]
    , let i0 = i2 - (i + 1) - 1
          i1 = i0 + 1
          i2 = (i + 1) * (y + 1) + x + 1
          i3 = i2 - 1
          fvs@[v0, v1, v2, v3] = (vs !) <$> [i0, i1, i2, i3]
    ]

  fromJust <$> halfedgeVV (vs ! 1) (vs ! 0)
