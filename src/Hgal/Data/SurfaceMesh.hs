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
    -- * Construction
  , empty
  , addVertex
  , addEdge
  , addFace
  , newFace
  , newVertex

  , checkIntegrity
  , halfedgeVV
  , removeEdge
  , opposite
  , setNext
  ) where


import Control.Exception
import Control.Lens hiding (point)
import Control.Monad (join)
import Control.Monad.State
import Data.Bifunctor
import Data.Bits
import Data.Either
import Data.Foldable (length, toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Kind (Type)
import Data.Sequence as Seq hiding (empty, length)
import qualified Data.Sequence as Seq (empty)
import Data.Tuple
import qualified Data.Vector as V
import GHC.Generics (Generic)

import qualified Hgal.Graph.Class as Graph
import qualified Hgal.Graph.ClassM as GraphM
import qualified Hgal.Graph.EulerOperations as Euler
import Hgal.Graph.Loops
import Hgal.Data.Property
import qualified Hgal.Data.PropertyM as M

-- import qualified Hgal.Graph.EulerOperationsM as EulerM
-- import qualified Hgal.Graph.GeneratorsM as GM
-- import Linear
-- import Data.Map (Map)


data family Connectivity a :: Type

{- | Convenience class for SurfaceMesh's graph elements.

Each element is associated with a halfedge.


-}
class Element a where
  name :: a -> String

  new :: SurfaceMesh v h e f -> a

  nullE :: a
  default nullE :: Bounded a => a
  nullE = maxBound

  conn :: a -> Lens' (SurfaceMesh v h e f) (Connectivity a)

  hasValidIndex :: SurfaceMesh v h e f -> a -> Bool
  hasValidConnectivity :: SurfaceMesh v h e f -> a -> Either String Bool

  isValid :: SurfaceMesh v h e f -> a -> Either String Bool
  isValid sm v
    | hasValidIndex sm v = hasValidConnectivity sm v
    | otherwise = Right False

  isBorder :: SurfaceMesh v h e f -> a -> Bool

  isIsolated :: SurfaceMesh v h e f -> a -> Bool
  default isIsolated :: Element Halfedge =>
                        SurfaceMesh v h e f -> a -> Bool
  isIsolated sm = (== nullE) . halfedge sm

  isRemoved :: SurfaceMesh v h e f -> a -> Bool

  degree :: SurfaceMesh v h e f -> a -> Int
  default degree :: Element Halfedge =>
                    Element Vertex =>
                    SurfaceMesh v h e f -> a -> Int
  degree sm = degree sm . vertex sm . halfedge sm

  vertex :: SurfaceMesh v h e f -> a -> Vertex
  default vertex :: Element Halfedge =>
                    SurfaceMesh v h e f -> a -> Vertex
  vertex sm = vertex sm . halfedge sm

  halfedge :: SurfaceMesh v h e f -> a -> Halfedge

  setHalfedge :: SurfaceMesh v h e f -> a -> Halfedge -> SurfaceMesh v h e f

  edge :: SurfaceMesh v h e f -> a -> Edge
  edge sm = (\(Halfedge i) -> Edge i) . halfedge sm

  face :: SurfaceMesh v h e f -> a -> Face
  default face :: Element Halfedge =>
                  SurfaceMesh v h e f -> a -> Face
  face sm = face sm . halfedge sm

  {- | Next vertex in a loop.

  Next halfegde in a loop.

  Next edge in a loop.

  Next face in a loop. (same if graph is correct)
  -}
  next :: SurfaceMesh v h e f -> a -> a
  prev :: SurfaceMesh v h e f -> a -> a

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

type instance Graph.Vertex (SurfaceMesh v h e f) = Vertex
type instance Graph.Halfedge (SurfaceMesh v h e f) = Halfedge
type instance Graph.Edge (SurfaceMesh v h e f) = Edge
type instance Graph.Face (SurfaceMesh v h e f) = Face

{- | Halfedge graph based Mesh data structure

holding point data v assocaited with Vertex

and arbitrary data sore d with additional

properties for Vertices, Halfedges, Edges and Faces

-}
data SurfaceMesh v h e f = SurfaceMesh
  { _vconn :: Seq (Connectivity Vertex)
  , _hconn :: Seq (Connectivity Halfedge)
  , _fconn :: Seq (Connectivity Face)
  , _vprops :: Seq (Maybe v)
  , _hprops :: Seq (Maybe h)
  , _eprops :: Seq (Maybe e)
  , _fprops :: Seq (Maybe f)
  , _vremoved :: IntMap Bool
  , _eremoved :: IntMap Bool
  , _fremoved :: IntMap Bool
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

  vertex sm h = view (conn h.hV) sm
  halfedge _ h = h
  setHalfedge sm _ _ = sm
  face sm h = view (conn h.hF) sm
  next sm h = view (conn h.hN) sm
  prev sm h = view (conn h.hP) sm

instance Element Edge where

  name _ = "Edge"

  new = Edge . numHalfedges

  hasValidConnectivity sm (Edge i) = collectErrors [hasValidConnectivity sm (Halfedge i),
                                                    hasValidConnectivity sm (Halfedge (i + 1))]

  hasValidIndex sm (Edge i) = i < numEdges sm

  isValid sm e
    | hasValidIndex sm e = collectErrors [isValid sm h, isValid sm (opposite h)]
    | otherwise = Right False
    where h = halfedge sm e

  isBorder sm e = isBorder sm h || isBorder sm (opposite h)
    where h = halfedge sm e

  isRemoved sm (Edge i) = fromMaybe False $ IntMap.lookup (div i 2) (_eremoved sm)

  halfedge _ (Edge i) = Halfedge i
  setHalfedge sm _ _ = sm
  next sm = edge sm . next sm . halfedge sm
  prev sm = edge sm . prev sm . halfedge sm

instance Element Face where

  name _ = "Face"

  new = Face . numFaces

  conn (Face i) = fconn.singular (ix i)

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
  next sm = face sm . next sm . halfedge sm
  prev sm = face sm . prev sm . halfedge sm


empty :: SurfaceMesh v h e f
empty = SurfaceMesh
   Seq.empty Seq.empty Seq.empty
   Seq.empty Seq.empty Seq.empty Seq.empty
   IntMap.empty IntMap.empty IntMap.empty


-------------------------------------------------------------------------------
-- Elements

numVertices :: SurfaceMesh v h e f -> Int
numVertices = length . _vconn

numHalfedges :: SurfaceMesh v h e f -> Int
numHalfedges = length . _hconn

numEdges :: SurfaceMesh v h e f -> Int
numEdges = (`div` 2) . length . _hconn

numFaces :: SurfaceMesh v h e f -> Int
numFaces = length . _fconn

vertices :: SurfaceMesh v h e f -> [Vertex]
vertices sm = Prelude.filter (not . isRemoved sm) $
              Vertex <$> [0..numVertices sm - 1]

halfedges :: SurfaceMesh v h e f -> [Halfedge]
halfedges sm = Prelude.filter (not . isRemoved sm) $
               Halfedge <$> [0..numHalfedges sm - 1]

edges :: SurfaceMesh v h e f -> [Edge]
edges sm = fmap (`div` 2) <$> Prelude.filter (not . isRemoved sm) $
           Edge . (*2) <$> [0..numEdges sm - 1]

faces :: SurfaceMesh v h e f -> [Face]
faces sm = Prelude.filter (not . isRemoved sm) $
           Face <$> [0..numFaces sm - 1]

-------------------------------------------------------------------------------
-- Adding elements

{- | Allocate and return a Vertex.
-}
addVertex :: SurfaceMesh v h e f -> (Vertex, SurfaceMesh v h e f)
addVertex sm =
  let sm' = vconn %~ (snoc ?? VertexConnectivity nullE) $ sm
      sm'' = vprops %~ (snoc ?? Nothing) $ sm'
  in (new sm, sm'')

{- | Allocate and return an Edge.
-}
addEdge :: SurfaceMesh v h e f -> (Edge, SurfaceMesh v h e f)
addEdge sm =
  let sm' = hconn %~ (addh . addh) $ sm
        where addh = (snoc ?? HalfedgeConnectivity nullE nullE nullE nullE)
      sm'' = (hprops %~ (addp . addp)) $ (eprops %~ addp') $ sm'
        where addp = (snoc ?? Nothing)
              addp' = (snoc ?? Nothing)
  in (new sm, sm'')

{- | Allocate and return a Face.

This does not update graph connectivity.
-}
addFace :: SurfaceMesh v h e f -> (Face, SurfaceMesh v h e f)
addFace sm =
  let sm' = fconn %~ (snoc ?? FaceConnectivity nullE) $ sm
      sm'' = eprops %~ (snoc ?? Nothing) $ sm'
  in (new sm, sm'')

{- | Allocate and return a Vertex with point properites v.
-}
newVertex :: SurfaceMesh v h e f -> v -> (SurfaceMesh v h e f, Vertex)
newVertex sm v =
  --that's not exactly right
  let (n, sm') = addVertex sm
  in (vprops %~ (\(vs :|> _) -> vs :|> Just v) $ sm', n)
  -- in (sm', n)

{- | Create Face from Vertices already present in the Mesh.

This does update graph connectivity.
-}
newFace :: Foldable t => SurfaceMesh v h e f -> t Vertex -> (SurfaceMesh v h e f, Face)
newFace sm vs = swap $ Euler.addFace sm vs

-------------------------------------------------------------------------------
-- Removing elements

removeVertex :: SurfaceMesh v h e f -> Vertex -> SurfaceMesh v h e f
removeVertex sm (Vertex i) = vremoved.at i ?~ True $ sm

removeEdge :: SurfaceMesh v h e f -> Edge -> SurfaceMesh v h e f
removeEdge sm (Edge i) = eremoved.at (div i 2) ?~ True $ sm

removeFace :: SurfaceMesh v h e f -> Face -> SurfaceMesh v h e f
removeFace sm (Face i) = fremoved.at i ?~ True $ sm

-------------------------------------------------------------------------------
-- Connectivity

target :: SurfaceMesh v h e f -> Halfedge -> Vertex
target sm h = view (conn h.hV) sm

setTarget :: SurfaceMesh v h e f -> Halfedge -> Vertex -> SurfaceMesh v h e f
setTarget sm h v = set (conn h.hV) v sm

setFace :: SurfaceMesh v h e f -> Halfedge -> Face -> SurfaceMesh v h e f
setFace sm h f = set (conn h.hF) f sm

setNextOnly :: SurfaceMesh v h e f -> Halfedge -> Halfedge -> SurfaceMesh v h e f
setNextOnly sm h nh = set (conn h.hN) nh sm

setPrevOnly :: SurfaceMesh v h e f -> Halfedge -> Halfedge -> SurfaceMesh v h e f
setPrevOnly sm h ph = set (conn h.hP) ph sm

setNext :: SurfaceMesh v h e f -> Halfedge -> Halfedge -> SurfaceMesh v h e f
setNext sm h nh = setPrevOnly (setNextOnly sm h nh) nh h

opposite :: Halfedge -> Halfedge
opposite h = xor h 1

source :: SurfaceMesh v h e f -> Halfedge -> Vertex
source sm = target sm . opposite


halfedgeVV :: SurfaceMesh v h e f -> Vertex -> Vertex -> Maybe Halfedge
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

merge ::  SurfaceMesh v h e f -> SurfaceMesh v h e f -> SurfaceMesh v h e f
merge sm sm2 =
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
     (_vprops sm <> _vprops sm2)
     (_hprops sm <> _hprops sm2)
     (_eprops sm <> _eprops sm2)
     (_fprops sm <> _fprops sm2)
     (_vremoved sm <> vremoved2)
     (_eremoved sm <> eremoved2)
     (_fremoved sm <> fremoved2)

instance Semigroup (SurfaceMesh v h e f) where
  (<>) = merge

instance Monoid (SurfaceMesh v h e f) where
  mempty = empty

-------------------------------------------------------------------------------
-- Properties

instance Eq v => Property (SurfaceMesh v h e f) Vertex v where
  property (Vertex i) = vprops.lens (join . Seq.lookup i) (flip $ Seq.update i)
  -- properties sm = V.fromList . toList $ _vprops sm

  find sm v = Vertex <$> Seq.elemIndexL (Just v) (_vprops sm)
  findKeys sm f = Vertex <$> Seq.findIndicesL (maybe False f) (_vprops sm)

instance Eq v => M.Property (St v h e f) (SurfaceMesh v h e f) Vertex v where
  getProperty _ k = use (property k)
  adjustProperty _ f k = modifying (property k) (fmap f)
  replaceProperty _ k v = assign (property k) (Just v)
  -- properties sm = return $ properties sm

instance Eq h => Property (SurfaceMesh v h e f) Halfedge h where
  property (Halfedge i) = hprops.lens (join . Seq.lookup i) (flip $ Seq.update i)

  find sm v = Halfedge <$> Seq.elemIndexL (Just v) (_hprops sm)
  findKeys sm f = Halfedge <$> Seq.findIndicesL (maybe False f) (_hprops sm)

instance Eq h => M.Property (St v h e f) (SurfaceMesh v h e f) Halfedge h where
  getProperty _ k = use (property k)
  adjustProperty _ f k = modifying (property k) (fmap f)
  replaceProperty _ k v = assign (property k) (Just v)

instance Eq e => Property (SurfaceMesh v h e f) Edge e where
  property (Edge i) = eprops.lens (join . Seq.lookup i) (flip $ Seq.update i)

  find sm v = Edge <$> Seq.elemIndexL (Just v) (_eprops sm)
  findKeys sm f = Edge <$> Seq.findIndicesL (maybe False f) (_eprops sm)

instance Eq e => M.Property (St v h e f) (SurfaceMesh v h e f) Edge e where
  getProperty _ k = use (property k)
  adjustProperty _ f k = modifying (property k) (fmap f)
  replaceProperty _ k v = assign (property k) (Just v)

instance Eq f => Property (SurfaceMesh v h e f) Face f where
  property (Face i) = fprops.lens (join . Seq.lookup i) (flip $ Seq.update i)

  find sm v = Face <$> Seq.elemIndexL (Just v) (_fprops sm)
  findKeys sm f = Face <$> Seq.findIndicesL (maybe False f) (_fprops sm)

instance Eq f => M.Property (St v h e f) (SurfaceMesh v h e f) Face f where
  getProperty _ k = use (property k)
  adjustProperty _ f k = modifying (property k) (fmap f)
  replaceProperty _ k v = assign (property k) (Just v)

vertexProperties :: SurfaceMesh v h e f -> V.Vector (Maybe v)
vertexProperties sm = V.fromList . toList $ _vprops sm

halfedgeProperties :: SurfaceMesh v h e f -> V.Vector (Maybe h)
halfedgeProperties sm = V.fromList . toList $ _hprops sm

edgeProperties :: SurfaceMesh v h e f -> V.Vector (Maybe e)
edgeProperties sm = V.fromList . toList $ _eprops sm

faceProperties :: SurfaceMesh v h e f -> V.Vector (Maybe f)
faceProperties sm = V.fromList . toList $ _fprops sm

-------------------------------------------------------------------------------
-- Full integrity check

checkIntegrity :: SurfaceMesh v h e f -> Either String Bool
checkIntegrity sm = collectErrors [hsI, vsI]
  where
    hsI = second and (mapM (halfedgeIntegrity sm) $ halfedges sm)
    vsI = second and (mapM (vertexIntegrity sm) $ vertices sm)

halfedgeIntegrity :: SurfaceMesh v h e f -> Halfedge -> Either String Bool
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

vertexIntegrity :: SurfaceMesh v h e f -> Vertex -> Either String Bool
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
         => SurfaceMesh v h e f -> a -> String -> b -> String
conError sm els s elt =
  name els ++ " " ++ show els ++ " connectivity error. " ++
  s ++ " " ++ name elt ++ " " ++ show elt ++ " is " ++
  if hasValidIndex sm elt then "removed." else "invalid."

collectErrors :: [Either String Bool] -> Either String Bool
collectErrors es =
  let right = Right (and (rights es))
      left = Left (unlines (lefts es))
  in if Prelude.null (lefts es) then right else left

putIntegrity :: SurfaceMesh v h e f -> IO ()
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

instance Show (SurfaceMesh v h e f) where
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


instance Graph.Element (SurfaceMesh v h e f) Vertex where
  isBorder = isBorder
  isValid = isValid
  degree = degree
instance Graph.Element (SurfaceMesh v h e f) Halfedge where
  isBorder = isBorder
  isValid = isValid
  degree = degree
instance Graph.Element (SurfaceMesh v h e f) Edge where
  isBorder = isBorder
  isValid = isValid
  degree = degree
instance Graph.Element (SurfaceMesh v h e f) Face where
  isBorder = isBorder
  isValid = isValid
  degree = degree

instance Graph.RemovableElement (SurfaceMesh v h e f) Vertex where
  remove = removeVertex
instance Graph.RemovableElement (SurfaceMesh v h e f) Edge where
  remove = removeEdge
instance Graph.RemovableElement (SurfaceMesh v h e f) Face where
  remove = removeFace

instance Graph.GetHalfedge (SurfaceMesh v h e f) Vertex where
  halfedge = halfedge
instance Graph.SetHalfedge (SurfaceMesh v h e f) Vertex where
  setHalfedge = setHalfedge
instance Graph.GetHalfedge (SurfaceMesh v h e f) Edge where
  halfedge = halfedge
instance Graph.SetHalfedge (SurfaceMesh v h e f) Edge where
  setHalfedge = setHalfedge
instance Graph.GetHalfedge (SurfaceMesh v h e f) Face where
  halfedge = halfedge
instance Graph.SetHalfedge (SurfaceMesh v h e f) Face where
  setHalfedge = setHalfedge
instance Graph.GetFace (SurfaceMesh v h e f) Halfedge where
  face = face
instance Graph.SetFace (SurfaceMesh v h e f) Halfedge where
  setFace = setFace


instance Graph.HalfedgeGraph (SurfaceMesh v h e f) where
  edge = edge
  opposite _ = opposite

  source = source
  target = target

  next = next
  prev = prev

  halfedgeVV = halfedgeVV
  vertices = vertices
  halfedges = halfedges
  edges = edges

instance Graph.MutableHalfedgeGraph (SurfaceMesh v h e f) where
  setTarget = setTarget
  setNext = setNext
  addVertex = addVertex
  addEdge = addEdge

instance Graph.FaceGraph (SurfaceMesh v h e f) where
  faces = faces

  outerFace _ = nullE

instance Graph.MutableFaceGraph (SurfaceMesh v h e f) where
  addFace = addFace


type St v h e f = State (SurfaceMesh v h e f)

instance GraphM.Element (St v h e f) Vertex
instance GraphM.Element (St v h e f) Halfedge
instance GraphM.Element (St v h e f) Edge
instance GraphM.Element (St v h e f) Face
instance GraphM.RemovableElement (St v h e f) Vertex
instance GraphM.RemovableElement (St v h e f) Edge
instance GraphM.RemovableElement (St v h e f) Face

instance GraphM.GetHalfedge (St v h e f) (SurfaceMesh v h e f) Vertex
instance GraphM.SetHalfedge (St v h e f) (SurfaceMesh v h e f) Vertex
instance GraphM.GetHalfedge (St v h e f) (SurfaceMesh v h e f) Edge
instance GraphM.SetHalfedge (St v h e f) (SurfaceMesh v h e f) Edge
instance GraphM.GetHalfedge (St v h e f) (SurfaceMesh v h e f) Face
instance GraphM.SetHalfedge (St v h e f) (SurfaceMesh v h e f) Face
instance GraphM.GetFace (St v h e f) (SurfaceMesh v h e f) Halfedge
instance GraphM.SetFace (St v h e f) (SurfaceMesh v h e f) Halfedge

instance GraphM.HalfedgeGraph (St v h e f) (SurfaceMesh v h e f)
instance GraphM.MutableHalfedgeGraph (St v h e f) (SurfaceMesh v h e f)
instance GraphM.FaceGraph (St v h e f) (SurfaceMesh v h e f)
instance GraphM.MutableFaceGraph (St v h e f) (SurfaceMesh v h e f)

-------------------------------------------------------------------------------
-- Garbage temp tests

-- foo :: SurfaceMesh () () () ()
-- foo =
--   let sm = empty
--       f = do
--             v1 <- GraphM.addVertex sm
--             v2 <- GraphM.addVertex sm
--             v3 <- GraphM.addVertex sm
--             EulerM.addFace sm [v1, v2, v3]
--   in execState f sm

-- foo1 :: SurfaceMesh () () () ()
-- foo1 =
--   let sm = empty
--       (v1, sm0) = Graph.addVertex sm
--       (v2, sm1) = Graph.addVertex sm0
--       (v3, sm2) = Graph.addVertex sm1
--       (_, sm3)  = Euler.addFace sm2 [v1, v2, v3]
--   in sm3

-- foo2 :: SurfaceMesh (V3 Double) () (Map String String) ()
-- foo2 =
--   let sm = empty
--       f = do
--             -- GM.makeRegularPrism sm 3 (V3 0 0 0) 7 13 True
--             -- GM.makeTriangle sm (V3 2 2 2) (V3 0 0 0) (V3 1 1 1)
--             GM.makeGrid sm 3 3 (\x y -> fromIntegral <$> V3 x y 0) True
--             v1 <- GraphM.addVertex sm
--             GraphM.addEdge sm
--             property (Vertex 0) ?= 7
--             property (Edge 0) ?= Data.Map.fromList [("foo", "bar")]
--             (property (Edge 0)._Just.at "foo") ?= "alice"
--   in execState f sm

{- $construction

These are fairly low-level functions.

Most of the time you will want to use "Hgal2.Graph.ClassM" and "Hgal2.Graph.EulerOperationsM" instead.

==== __Examples__

>>> vs = [V3 0 0 0, V3 1 0 0, V3 0 1 0, V3 1 1 0, V3 2 0 0]
>>> (sm, [u, v, w, x, y]) = mapAccumL newVertex (empty ()) vs
>>> (sm', f) = mapAccumL newFace sm [[u, w, v], [v, w, x], [v, x, y]]



-}
