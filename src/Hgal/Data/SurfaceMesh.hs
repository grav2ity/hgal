{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hgal.Data.SurfaceMesh
  ( -- * Types
    SurfaceMesh
  , Element(..)
  , Vertex(..)
  , Halfedge(..)
  , Edge(..)
  , Face(..)
    -- * Accessors
  , numVertices
  , numHalfedges
  , numEdges
  , numFaces
  , vertices
  , halfedges
  , edges
  , faces
  , points
  , pointProperties
    -- * Construction
    -- $construction
  , empty
  , addVertex
  , addEdge
  , addFace
  , newFace
  , newVertex
  ) where


import Control.Exception
import Control.Lens hiding (point)
import Control.Monad.State
import Data.Bifunctor
import Data.Bits
import Data.Either
import Data.Foldable (length, toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Sequence as Seq hiding (empty, length)
import qualified Data.Sequence as Seq (empty)
import Data.Tuple
import qualified Data.Vector as V
import GHC.Generics (Generic)

import qualified Hgal.Graph.Class as Graph
import qualified Hgal.Graph.ClassM as GraphM
import Hgal.Graph.Loops
import Hgal.Data.Property
import qualified Hgal.Data.PropertyM as M

import Hgal.Graph.Predicates (isValidPolygonMesh)
import Debug.Trace
import qualified Hgal.Graph.EulerOperations as Euler
import qualified Hgal.Graph.EulerOperationsM as EulerM
import qualified Hgal.Graph.GeneratorsM as GM
import Linear
import Data.Map (Map)


data family Connectivity a :: *

{- | Convenience class for SurfaceMesh's graph elements.

Each element is associated with a halfedge.


-}
class Element a where
  name :: a -> String

  new :: SurfaceMesh v d -> a

  nullE :: a
  default nullE :: Bounded a => a
  nullE = maxBound

  conn :: a -> Lens' (SurfaceMesh v d) (Connectivity a)

  propertyOf :: Property s a p => a -> Lens' (SurfaceMesh v s) (Maybe p)

  hasValidIndex :: SurfaceMesh v d -> a -> Bool
  hasValidConnectivity :: SurfaceMesh v d -> a -> Either String Bool

  isValid :: SurfaceMesh v d -> a -> Either String Bool
  isValid sm v
    | hasValidIndex sm v = hasValidConnectivity sm v
    | otherwise = Right False

  isBorder :: SurfaceMesh v d -> a -> Bool

  isIsolated :: SurfaceMesh v d -> a -> Bool
  isIsolated sm = (== nullE) . halfedge sm

  isRemoved :: SurfaceMesh v d -> a -> Bool

  degree :: SurfaceMesh v d -> a -> Int
  degree sm = degree sm . vertex sm . halfedge sm

  vertex :: SurfaceMesh v d -> a -> Vertex
  vertex sm = vertex sm . halfedge sm

  halfedge :: SurfaceMesh v d -> a -> Halfedge

  setHalfedge :: SurfaceMesh v d -> a -> Halfedge -> SurfaceMesh v d

  edge :: SurfaceMesh v d -> a -> Edge
  edge sm = (\(Halfedge i) -> Edge i) . halfedge sm

  face :: SurfaceMesh v d -> a -> Face
  face sm = face sm . halfedge sm

  {- | Next vertex in a loop.

  Next halfegde in a loop.

  Next edge in a loop.

  Next face in a loop. (same if graph is correct)
  -}
  next :: SurfaceMesh v d -> a -> a
  prev :: SurfaceMesh v d -> a -> a

newtype Vertex = Vertex Int
    deriving (Bounded, Enum, Eq, Generic, Ord )
    deriving newtype (Bits, Integral, Num, Real)
instance Wrapped Vertex
newtype Halfedge = Halfedge Int
    deriving (Bounded, Enum, Eq, Generic, Ord)
    deriving newtype (Bits, Integral, Num, Real)
instance Wrapped Halfedge
newtype Edge = Edge Int
    deriving (Bounded, Enum, Generic, Ord)
    deriving newtype (Bits, Integral, Num, Real)
instance Eq Edge where
  (==) a b = div a 2 == div b 2
newtype Face = Face Int
    deriving (Bounded, Enum, Eq, Generic, Ord)
    deriving newtype (Bits, Integral, Num, Real)
instance Wrapped Face

{- | Halfedge graph based Mesh data structure

holding point data v assocaited with Vertex

and arbitrary data sore d with additional

properties for Vertices, Halfedges, Edges and Faces

-}
data SurfaceMesh v d = SurfaceMesh
  { _vconn :: Seq (Connectivity Vertex)
  , _hconn :: Seq (Connectivity Halfedge)
  , _fconn :: Seq (Connectivity Face)
  , _vpoint :: Seq (Maybe v)
  , _vremoved :: IntMap Bool
  , _eremoved :: IntMap Bool
  , _fremoved :: IntMap Bool
  , _props :: d
  }

makeLenses ''SurfaceMesh


newtype instance Connectivity Vertex = VertexConnectivity
  { _vH :: Halfedge }

data instance Connectivity Halfedge = HalfedgeConnectivity
  { _hF :: Face
  , _hV :: Vertex
  , _hN :: Halfedge
  , _hP :: Halfedge
  }

newtype instance Connectivity Face = FaceConnectivity
  { _fH :: Halfedge }


instance Element Vertex where

  name _ = "Vertex"

  new = Vertex . numVertices

  conn (Vertex i) = vconn.singular (ix i)
  propertyOf e = props.property e

  hasValidIndex sm (Vertex i) = i < numVertices sm
  hasValidConnectivity sm v
    | not (hasValidIndex sm h) || isRemoved sm h = Left $ conError sm v "" h
    | otherwise = Right True
    where h = halfedge sm v

  isBorder sm v
    | h == nullE = True
    | otherwise = any (isBorder sm) (halfedgesAroundTarget sm h)
    where h = halfedge sm v

  isRemoved sm (Vertex i) = fromMaybe False $ IntMap.lookup i (_vremoved sm)

  degree sm v
    | h == nullE = 0
    | otherwise = length $ verticesAroundTarget sm h
    where h = halfedge sm v

  vertex _ v = v
  halfedge sm v = view (conn v.vH) sm
  setHalfedge sm v h = set (conn v.vH) h sm
  next sm = vertex sm . next sm . halfedge sm
  prev sm = vertex sm . prev sm . halfedge sm

instance Element Halfedge where

  name _ = "Halfedge"

  new = Halfedge . numHalfedges

  conn (Halfedge i) = hconn.singular (ix i)
  propertyOf e = props.property e

  hasValidIndex sm (Halfedge i) = i < numHalfedges sm

  hasValidConnectivity sm h = collectErrors [faceC, vertexC, nextC, prevC]
    where
      faceC
        | isBorder sm h = Right True
        | not (hasValidIndex sm f) || isRemoved sm f = Left $ conError sm h "" f
        | otherwise = Right True
      vertexC
        | not (hasValidIndex sm v) || isRemoved sm v = Left $ conError sm h "" v
        | otherwise = Right True
      nextC
        | not (hasValidIndex sm hn) || isRemoved sm hn = Left $ conError sm h "Next" hn
        | otherwise = Right True
      prevC
        | not (hasValidIndex sm hp) || isRemoved sm hp = Left $ conError sm h "Prev" hp
        | otherwise = Right True
      f = face sm h
      v = target sm h
      hn = next sm h
      hp = prev sm h

  isBorder sm = (== nullE) . face sm

  isRemoved sm = isRemoved sm . edge sm

  halfedge _ h = h
  vertex sm h = view (conn h.hV) sm
  face sm h = view (conn h.hF) sm
  next sm h = view (conn h.hN) sm
  prev sm h = view (conn h.hP) sm

instance Element Edge where

  name _ = "Edge"

  new = Edge . numHalfedges

  propertyOf e = props.property (div e 2)

  hasValidIndex sm (Edge i) = i < numEdges sm

  isValid sm e
    | hasValidIndex sm e = collectErrors [isValid sm h, isValid sm (opposite h)]
    | otherwise = Right False
    where h = halfedge sm e

  isBorder sm e = isBorder sm h || isBorder sm (opposite h)
    where h = halfedge sm e

  isRemoved sm (Edge i) = fromMaybe False $ IntMap.lookup (div i 2) (_eremoved sm)

  halfedge _ (Edge i) = Halfedge i
  next sm = edge sm . next sm . halfedge sm
  prev sm = edge sm . prev sm . halfedge sm

instance Element Face where

  name _ = "Face"

  new = Face . numFaces

  conn (Face i) = fconn.singular (ix i)
  propertyOf e = props.property e

  hasValidIndex sm (Face i) = i < numFaces sm

  hasValidConnectivity sm f
    | not (hasValidIndex sm f) || isRemoved sm f = Left $ conError sm f "" h
    | otherwise = Right True
    where h = halfedge sm f

  isBorder sm v
    | h == nullE = True
    | otherwise = any (isBorder sm) (halfedgesAroundFace sm h)
    where h = halfedge sm v

  isRemoved sm (Face i) = fromMaybe False $ IntMap.lookup i (_fremoved sm)

  degree sm v
    | h == nullE = 0
    | otherwise = length $ verticesAroundFace sm h
    where h = halfedge sm v

  halfedge sm f = view (conn f.fH) sm
  setHalfedge sm f h = set (conn f.fH) h sm
  face _ f = f


empty :: d -> SurfaceMesh v d
empty = SurfaceMesh
   Seq.empty Seq.empty Seq.empty
   Seq.empty
   IntMap.empty IntMap.empty IntMap.empty


-------------------------------------------------------------------------------
-- Elements

numVertices :: SurfaceMesh v d -> Int
numVertices = length . _vconn

numHalfedges :: SurfaceMesh v d -> Int
numHalfedges = length . _hconn

numEdges :: SurfaceMesh v d -> Int
numEdges = (`div` 2) . length . _hconn

numFaces :: SurfaceMesh v d -> Int
numFaces = length . _fconn

vertices :: SurfaceMesh v d -> [Vertex]
vertices sm = Prelude.filter (not . isRemoved sm) $
              Vertex <$> [0..numVertices sm - 1]

halfedges :: SurfaceMesh v d -> [Halfedge]
halfedges sm = Prelude.filter (not . isRemoved sm) $
               Halfedge <$> [0..numHalfedges sm - 1]

edges :: SurfaceMesh v d -> [Edge]
edges sm = fmap (`div` 2) <$> Prelude.filter (not . isRemoved sm) $
           Edge . (*2) <$> [0..numEdges sm - 1]

faces :: SurfaceMesh v d -> [Face]
faces sm = Prelude.filter (not . isRemoved sm) $
           Face <$> [0..numFaces sm - 1]

points :: Eq v => SurfaceMesh v d -> [Maybe v]
points = toList . pointProperties

-------------------------------------------------------------------------------
-- Adding elements

{- | Allocate and return a Vertex.
-}
addVertex :: SurfaceMesh v d -> (Vertex, SurfaceMesh v d)
addVertex sm =
  let sm' = vconn %~ (snoc ?? VertexConnectivity nullE) $ sm
      sm'' = vpoint %~ (snoc ?? Nothing) $ sm'
  in (new sm, sm'')

{- | Allocate and return an Edge.
-}
addEdge :: SurfaceMesh v d -> (Edge, SurfaceMesh v d)
addEdge sm =
  let addh = (snoc ?? HalfedgeConnectivity nullE nullE nullE nullE)
      sm' = hconn %~ (addh . addh) $ sm
  in (new sm, sm')

{- | Allocate and return a Face.

This does not update graph connectivity.
-}
addFace :: SurfaceMesh v d -> (Face, SurfaceMesh v d)
addFace sm =
  let sm' = fconn %~ (snoc ?? FaceConnectivity nullE) $ sm
  in (new sm, sm')

{- | Allocate and return a Vertex with point properites v.
-}
newVertex :: SurfaceMesh v d -> v -> (SurfaceMesh v d, Vertex)
newVertex sm v =
  --that's not exactly right
  let (n, sm') = addVertex sm
  in (vpoint %~ (\(vs :|> _) -> vs :|> Just v) $ sm', n)

{- | Create Face from Vertices already present in the Mesh.

This does update graph connectivity.
-}
newFace :: Foldable t => SurfaceMesh v d -> t Vertex -> (SurfaceMesh v d, Face)
newFace sm vs = swap $ Euler.addFace sm vs

-------------------------------------------------------------------------------
-- Removing elements

removeVertex :: SurfaceMesh v d -> Vertex -> SurfaceMesh v d
removeVertex sm (Vertex i) = vremoved.at i ?~ True $ sm

removeEdge :: SurfaceMesh v d -> Edge -> SurfaceMesh v d
removeEdge sm (Edge i) = eremoved.at (div i 2) ?~ True $ sm

removeFace :: SurfaceMesh v d -> Face -> SurfaceMesh v d
removeFace sm (Face i) = fremoved.at i ?~ True $ sm

-------------------------------------------------------------------------------
-- Connectivity

target :: SurfaceMesh v d -> Halfedge -> Vertex
target sm h = view (conn h.hV) sm

setTarget :: SurfaceMesh v d -> Halfedge -> Vertex -> SurfaceMesh v d
setTarget sm h v = set (conn h.hV) v sm

setFace :: SurfaceMesh v d -> Halfedge -> Face -> SurfaceMesh v d
setFace sm h f = set (conn h.hF) f sm

setNextOnly :: SurfaceMesh v d -> Halfedge -> Halfedge -> SurfaceMesh v d
setNextOnly sm h nh = set (conn h.hN) nh sm

setPrevOnly :: SurfaceMesh v d -> Halfedge -> Halfedge -> SurfaceMesh v d
setPrevOnly sm h ph = set (conn h.hP) ph sm

setNext :: SurfaceMesh v d -> Halfedge -> Halfedge -> SurfaceMesh v d
setNext sm h nh = setPrevOnly (setNextOnly sm h nh) nh h

opposite :: Halfedge -> Halfedge
opposite h = xor h 1

source :: SurfaceMesh v d -> Halfedge -> Vertex
source sm = target sm . opposite


halfedgeVV :: SurfaceMesh v d -> Vertex -> Vertex -> Maybe Halfedge
halfedgeVV sm sour tar =
  assert (hasValidIndex sm sour && hasValidIndex sm tar) result
  where
    h = halfedge sm tar
    worker hx =
      let n = opposite (next sm hx)
      in
        if source sm hx == sour then Just hx
          else if n /= h then worker n
             else Nothing
    result = if h == nullE then Nothing
      else worker h

-------------------------------------------------------------------------------
-- Merge

merge :: (d1 -> d2 -> d3) -> SurfaceMesh v d1 -> SurfaceMesh v d2 -> SurfaceMesh v d3
merge merged sm sm2 =
  let vconn2 = sm2 ^. vconn
               & traversed.vH.filtered (/= nullE)._Wrapped' %~ (+ numHalfedges sm)
      hconn2 = sm2 ^. hconn
               & traversed.hF.filtered (/= nullE)._Wrapped' %~ (+ numFaces sm)
               & traversed.hV.filtered (/= nullE)._Wrapped' %~ (+ numVertices sm)
               & traversed.hN.filtered (/= nullE)._Wrapped' %~ (+ numHalfedges sm)
               & traversed.hP.filtered (/= nullE)._Wrapped' %~ (+ numHalfedges sm)
      fconn2 = sm2 ^. fconn
               & traversed.fH.filtered (/= nullE)._Wrapped' %~ (+ numHalfedges sm)
      vremoved2 = IntMap.mapKeys (+ numVertices sm) (sm2 ^. vremoved)
      eremoved2 = IntMap.mapKeys (+ numEdges sm) (sm2 ^. eremoved)
      fremoved2 = IntMap.mapKeys (+ numFaces sm) (sm2 ^. fremoved)
  in SurfaceMesh
     (_vconn sm <> vconn2)
     (_hconn sm <> hconn2)
     (_fconn sm <> fconn2)
     (_vpoint sm <> _vpoint sm2)
     (_vremoved sm <> vremoved2)
     (_eremoved sm <> eremoved2)
     (_fremoved sm <> fremoved2)
     (merged (_props sm) (_props sm2))

instance Semigroup d => Semigroup (SurfaceMesh v d) where
  (<>) = merge (<>)

instance Monoid d => Monoid (SurfaceMesh v d) where
  mempty = empty mempty

-------------------------------------------------------------------------------
-- Properties

instance Eq v => Property (SurfaceMesh v d) (Graph.Point Vertex) v where
  property (Graph.Point (Vertex i)) = vpoint.lens (join . Seq.lookup i) (flip $ Seq.update i)
  properties sm = V.fromList . toList $ _vpoint sm

  find sm v = Graph.Point . Vertex <$> Seq.elemIndexL (Just v) (_vpoint sm)
  findKeys sm f = Graph.Point . Vertex <$> Seq.findIndicesL (maybe False f) (_vpoint sm)

instance Eq v => M.Property (St v d) (SurfaceMesh v d) (Graph.Point Vertex) v where
  getProperty _ k = use (property k)
  adjustProperty _ f k = modifying (property k) (fmap f)
  replaceProperty _ k v = assign (property k) (Just v)
  properties sm = return $ properties sm

vertexProperties :: Property d Vertex p => SurfaceMesh v d -> V.Vector (Maybe p)
vertexProperties sm = properties (_props sm)

halfedgeProperties :: Property d Halfedge p => SurfaceMesh v d -> V.Vector (Maybe p)
halfedgeProperties sm = properties (_props sm)

edgeProperties :: Property d Edge p => SurfaceMesh v d -> V.Vector (Maybe p)
edgeProperties sm = properties (_props sm)

faceProperties :: Property d Face p => SurfaceMesh v d -> V.Vector (Maybe p)
faceProperties sm = properties (_props sm)

pointProperties :: Eq v => SurfaceMesh v d -> V.Vector (Maybe v)
pointProperties = properties

-------------------------------------------------------------------------------
-- Full integrity check

checkIntegrity :: SurfaceMesh v d -> Either String Bool
checkIntegrity sm = collectErrors [hsI, vsI]
  where
    hsI = second and (mapM (halfedgeIntegrity sm) $ halfedges sm)
    vsI = second and (mapM (vertexIntegrity sm) $ vertices sm)

halfedgeIntegrity :: SurfaceMesh v d -> Halfedge -> Either String Bool
halfedgeIntegrity sm h = halfedgeI >> oppositeI >> previousI >> nextI >> vertexI >> nextOppositeI
  where
    halfedgeI =
      first (\s -> "Integrity of halfedge " ++ show h ++ " corrupted.\n" ++ s) $
      collectErrors [isValid sm (next sm h), isValid sm (opposite h)]
    oppositeI
      | opposite h /= h && opposite (opposite h) == h = Right True
      | otherwise = Left $ "Integrity of opposite halfedge of " ++ show h ++ " corrupted.\n"
    previousI
      | next sm (prev sm h) == h = Right True
      | otherwise = Left $ "Integrity of previous halfedge of " ++ show h ++ " corrupted.\n"
    nextI
      | prev sm (next sm h) == h = Right True
      | otherwise = Left $ "Integrity of next halfedge of " ++ show h ++ " corrupted.\n"
    vertexI =
      first (\s -> "Integrity of vertex of halfedge " ++ show h ++ " corrupted.\n" ++ s) $
      collectErrors [isValid sm (target sm h)]
    nextOppositeI
      | target sm (opposite (next sm h)) == target sm h = Right True
      | otherwise = Left $ "Halfedge vertex of next opposite is not the same for " ++ show h ++ ".\n"

vertexIntegrity :: SurfaceMesh v d -> Vertex -> Either String Bool
vertexIntegrity sm v
  | fromRight False (isValid sm h) && target sm (halfedge sm v) == v = Right True
  | otherwise = Left $ "Halfedge of vertex " ++ show v ++ " is not an incoming halfedge.\n"
  where h = halfedge sm v

-------------------------------------------------------------------------------
-- string & error helpers

showConnectivity :: (Show a, Foldable t) => String -> t a -> String
showConnectivity t s =
  t ++ " :: " ++ ifoldMapOf folded
  (\i a -> "\n(" ++ show i ++ ", " ++ show a ++ ")") s

conError :: (Element a, Element b, Show a, Show b)
         => SurfaceMesh v d -> a -> String -> b -> String
conError sm els s elt =
  name els ++ " " ++ show els ++ " connectivity error. " ++
  s ++ " " ++ name elt ++ " " ++ show elt ++ " is " ++
  if hasValidIndex sm elt then "removed." else "invalid."

collectErrors :: [Either String Bool] -> Either String Bool
collectErrors es =
  let right = Right (and (rights es))
      left = Left (unlines (lefts es))
  in if Prelude.null (lefts es) then right else left

putIntegrity :: SurfaceMesh v d -> IO ()
putIntegrity = putStr . fromLeft "All correct.\n" . checkIntegrity

-------------------------------------------------------------------------------
-- Show instances

instance Show (Connectivity Vertex) where
  show s = "H " ++ show (_vH s)

instance Show (Connectivity Halfedge) where
  show s = "F " ++ show (_hF s)
         ++ ", V " ++ show (_hV s)
         ++ ", Next " ++ show (_hN s)
         ++ ", Prev " ++ show (_hP s)

instance Show (Connectivity Face) where
  show s = "H " ++ show (_fH s)

instance Show (SurfaceMesh v d) where
  show s = unlines [ showConnectivity "Vertices" (_vconn s)
                   , showConnectivity "Halfedges" (_hconn s)
                   , showConnectivity "Faces" (_fconn s)
                   ]

instance Show Vertex where
  show ii@(Vertex i)
    | ii == nullE = "N"
    | otherwise = show i

instance Show Halfedge where
  show ii@(Halfedge i)
    | ii == nullE = "N"
    | otherwise = show i

instance Show Edge where
  show ii@(Edge i)
    | ii == nullE = "N"
    | otherwise = show i

instance Show Face where
  show ii@(Face i)
    | ii == nullE = "N"
    | otherwise = show i

-------------------------------------------------------------------------------
-- Internal Connectivity Lenses

vH :: Lens' (Connectivity Vertex) Halfedge
vH g (VertexConnectivity v) = VertexConnectivity <$> g v

hF :: Lens' (Connectivity Halfedge) Face
hF g (HalfedgeConnectivity f v n p) = (\x -> HalfedgeConnectivity x v n p) <$> g f

hV :: Lens' (Connectivity Halfedge) Vertex
hV g (HalfedgeConnectivity f v n p) = (\x -> HalfedgeConnectivity f x n p) <$> g v

hN :: Lens' (Connectivity Halfedge) Halfedge
hN g (HalfedgeConnectivity f v n p) = (\x -> HalfedgeConnectivity f v x p) <$> g n
hP :: Lens' (Connectivity Halfedge) Halfedge
hP g (HalfedgeConnectivity f v n p) = (\x -> HalfedgeConnectivity f v n x) <$> g p

fH :: Lens' (Connectivity Face) Halfedge
fH g (FaceConnectivity v) = FaceConnectivity <$> g v


-------------------------------------------------------------------------------
-- Graph isntances


instance Graph.Element (SurfaceMesh v d) Vertex where
  isBorder = isBorder
  isValid g = isValid g
  degree = degree
instance Graph.Element (SurfaceMesh v d) Halfedge where
  isBorder = isBorder
  isValid g = isValid g
  degree = degree
instance Graph.Element (SurfaceMesh v d) Edge where
  isBorder = isBorder
  isValid g = isValid g
  degree = degree
instance Graph.Element (SurfaceMesh v d) Face where
  isBorder = isBorder
  isValid g = isValid g
  degree = degree

instance Graph.RemovableElement (SurfaceMesh v d) Vertex where
  remove = removeVertex
instance Graph.RemovableElement (SurfaceMesh v d) Edge where
  remove = removeEdge
instance Graph.RemovableElement (SurfaceMesh v d) Face where
  remove = removeFace

instance Graph.GetHalfedge (SurfaceMesh v d) Vertex Halfedge where
  halfedge = halfedge
instance Graph.SetHalfedge (SurfaceMesh v d) Vertex Halfedge where
  setHalfedge = setHalfedge
instance Graph.GetHalfedge (SurfaceMesh v d) Edge Halfedge where
  halfedge = halfedge
instance Graph.SetHalfedge (SurfaceMesh v d) Edge Halfedge where
  setHalfedge = setHalfedge
instance Graph.GetHalfedge (SurfaceMesh v d) Face Halfedge where
  halfedge = halfedge
instance Graph.SetHalfedge (SurfaceMesh v d) Face Halfedge where
  setHalfedge = setHalfedge
instance Graph.GetFace (SurfaceMesh v d) Halfedge Face where
  face = face
instance Graph.SetFace (SurfaceMesh v d) Halfedge Face where
  setFace = setFace

instance Graph.HalfedgeC (SurfaceMesh v d) Vertex Halfedge Edge where
  edge = edge
  opposite _ = opposite

  source = source
  target = target

  next = next
  prev = prev

  halfedgeVV = halfedgeVV

instance Graph.MutableHalfedgeC (SurfaceMesh v d) Vertex Halfedge where
  setTarget = setTarget
  setNext = setNext

instance Graph.HalfedgeGraph (SurfaceMesh v d) Vertex Halfedge Edge where
  vertices = vertices
  halfedges = halfedges
  edges = edges

  nullVertex _ = nullE
  nullHalfedge _ = nullE
  nullEdge _ = nullE

instance Graph.MutableHalfedgeGraph  (SurfaceMesh v d) Vertex Halfedge Edge where
  addVertex = addVertex
  addEdge = addEdge

instance Graph.FaceGraph (SurfaceMesh v d) Vertex Halfedge Edge Face where
  faces = faces

  nullFace _ = nullE

instance Graph.MutableFaceGraph (SurfaceMesh v d) Vertex Halfedge Edge Face where
  addFace = addFace


type St v d = State (SurfaceMesh v d)

instance GraphM.Element (St v d) Vertex
instance GraphM.Element (St v d) Halfedge
instance GraphM.Element (St v d) Edge
instance GraphM.Element (St v d) Face
instance GraphM.RemovableElement (St v d) Vertex
instance GraphM.RemovableElement (St v d) Edge
instance GraphM.RemovableElement (St v d) Face

instance GraphM.GetHalfedge (St v d) Vertex Halfedge
instance GraphM.SetHalfedge (St v d) Vertex Halfedge
instance GraphM.GetHalfedge (St v d) Edge Halfedge
instance GraphM.SetHalfedge (St v d) Edge Halfedge
instance GraphM.GetHalfedge (St v d) Face Halfedge
instance GraphM.SetHalfedge (St v d) Face Halfedge
instance GraphM.GetFace (St v d) Halfedge Face
instance GraphM.SetFace (St v d) Halfedge Face

instance GraphM.HalfedgeC (St v d) Vertex Halfedge Edge
instance GraphM.MutableHalfedgeC (St v d) Vertex Halfedge

instance GraphM.HalfedgeGraph (St v d) (SurfaceMesh v d) Vertex Halfedge Edge
instance GraphM.MutableHalfedgeGraph (St v d) (SurfaceMesh v d) Vertex Halfedge Edge
instance GraphM.FaceGraph (St v d) (SurfaceMesh v d) Vertex Halfedge Edge Face
instance GraphM.MutableFaceGraph (St v d) (SurfaceMesh v d) Vertex Halfedge Edge Face

instance Eq v => Graph.PointGraph (SurfaceMesh v d) Vertex v
instance Eq v => GraphM.PointGraph (St v d) (SurfaceMesh v d) Vertex v

-------------------------------------------------------------------------------
-- Garbage temp tests

foo :: SurfaceMesh (V3 Double) ()
foo =
  let sm = empty ()
      f = do
            v1 <- GraphM.addVertex sm
            v2 <- GraphM.addVertex sm
            v3 <- GraphM.addVertex sm
            EulerM.addFace sm [v1, v2, v3]
            -- return ()
  in execState f sm

foo1 :: SurfaceMesh () ()
foo1 =
  let sm = empty ()
      (v1, sm0) = Graph.addVertex sm
      (v2, sm1) = Graph.addVertex sm0
      (v3, sm2) = Graph.addVertex sm1
      (_, sm3)  = Euler.addFace sm2 [v1, v2, v3]
  in sm3

newtype MyProps2 = MyProps2 (IntMap Int, IntMap (Map String String))

instance Property MyProps2 Vertex Int where
  property (Vertex i) g (MyProps2 m) = MyProps2 <$> (_1.at i) g m

instance Property MyProps2 Edge (Map String String) where
  property (Edge i) g (MyProps2 m) = MyProps2 <$> (_2.at i) g m

foo2 :: SurfaceMesh (V3 Double) MyProps2
foo2 =
  let sm = empty (MyProps2 (IntMap.empty, IntMap.empty)) :: SurfaceMesh (V3 Double) MyProps2
      f = do
           -- GM.makeRegularPrism sm 3 (V3 0 0 0) 7 13 True
            -- GM.makeTriangle sm (V3 2 2 2) (V3 0 0 0) (V3 1 1 1)
            GM.makeGrid sm 3 3 (\x y -> fromIntegral <$> (V3 x y 0)) True
            -- v1 <- GraphM.addVertex sm
            -- GraphM.addEdge sm
            -- propertyOf (Vertex 0) ?= 7
            -- propertyOf (Edge 0) ?= Map.fromList [("foo", "bar")]
            -- (propertyOf (Edge 0)._Just.at "foo") ?= "alice"
  in execState f sm

{- $construction

These are fairly low-level functions.

Most of the time you will want to use "Hgal.Graph.ClassM" and "Hgal.Graph.EulerOperationsM" instead.

==== __Examples__

>>> vs = [V3 0 0 0, V3 1 0 0, V3 0 1 0, V3 1 1 0, V3 2 0 0]
>>> (sm, [u, v, w, x, y]) = mapAccumL newVertex (empty ()) vs
>>> (sm', f) = mapAccumL newFace sm [[u, w, v], [v, w, x], [v, x, y]]



-}
