{-# LANGUAGE MultiWayIf #-}

module Hgal.Graph.EulerOperationsM where

import Control.Applicative ((<|>))
import Control.Exception
import Control.Lens (both, findMOf, forMOf_, traversed, (??))
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Containers.ListUtils
import Data.Foldable
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Hgal.Graph.ClassM hiding (addEdge, addFace)
import qualified Hgal.Graph.ClassM as Graph (addEdge, addFace)
import Hgal.Graph.HelpersM
import Hgal.Graph.LoopsM

-- import Debug.Trace


addFace :: Foldable t
        => MutableFaceGraph m g
        => (Ord (V g), Eq (H g), Eq (F g))
        => g -> t (V g) -> m (F g)
addFace g vs = do
  let
    n = length vs
    indices = [0..n-1]
    indicesTuple = zip ic (drop 1 ic)
      where ic = take (n+1) . cycle $ indices
    vList = toList vs
    vertices = V.fromList vList

  uvs <- forM indicesTuple
           (\(i, ii) -> halfedgeVV (vertices V.! i) (vertices V.! ii))
  outerF <- outerFace g

  let
    halfedges = V.fromList uvs
    halfedgesTuple = [(halfedges V.! i, halfedges V.! ii) | (i, ii) <- indicesTuple ]
    isNew i = (isNothing <$> halfedges) V.! i

    checkVertexCount = guard (n > 2)
    checkVerticesU = guard (length (nubOrd vList) == n)
    checkVertex v = (lift (isIsolated g v) >>= guard) <|> (lift (isBorder v) >>= guard)
    checkHalfedge i =
      case halfedges V.! i of
        Nothing -> return ()
        Just h -> lift (isBorder h) >>= guard

    allCheck = checkVertexCount >> checkVerticesU >>
               forM vertices checkVertex >>
               forM indices checkHalfedge

    bothOld (i, ii) = not $ isNew i || isNew ii

    -- matches on (Just, Just) when both halfedges were already in the graph
    reLink = forM [(p, n) | (Just p, Just n) <- halfedgesTuple]
      $ \(innerPrev, innerNext) -> do
          p <- lift $ next innerPrev
          if p == innerNext then return []
            else do
              outerPrev <- lift $ opposite innerNext
              outerNext <- lift $ opposite innerPrev
              let
                worker hx = do
                  n <- lift $ (opposite <=< next) hx
                  isBorder <- lift $ isBorder n
                  if not isBorder || (n == innerPrev) then worker n
                    else return $ assert isBorder n
              borderPrev <- worker outerPrev
              borderNext <- lift $ do
                p <- next borderPrev
                isB <- isBorder p
                return $ assert isB p
              if borderNext == innerNext then mzero
                else do
                  patchStart <- lift $ next innerPrev
                  patchEnd <- lift $ prev innerNext
                  return [ (borderPrev, patchStart), (patchEnd, borderNext)
                         , (innerPrev, innerNext) ]

    createMissingEdges = forM indicesTuple $ \(i, ii) -> do
      when (isNew i) $ do
        ne <- lift $ addEdge g (vertices V.! i) (vertices V.! ii)
        he <- lift $ do
          h <- halfedge ne
          s <- source h
          -- return $ assert (h /= nullH && s == vertices V.! i) h
          return $ assert (s == vertices V.! i) h
        modify (V.modify (\vec -> VM.write vec i (Just he)))
        lift $ (setFace ?? outerF) =<< opposite he

    setupHalfedgesF halfedges' (i, ii) = do
      let
        v = vertices V.! ii
        innerPrev = halfedges' V.! i
        innerNext = halfedges' V.! ii

      if bothOld (i, ii) then return []
        else do
          outerPrev <- opposite innerNext
          outerNext <- opposite innerPrev
          let innerLink = (innerPrev, innerNext)
          if
            | isNew i && isNew ii -> do
              hv <- runMaybeT $ do
                vDeg <- lift $ degree v
                guard (vDeg /= 0)
                h <- lift $ halfedge v
                bh <- lift $ isBorder v
                if not bh then do
                  hAroundV <- lift $ halfedgesAroundTarget h
                  MaybeT $ findMOf traversed isBorder hAroundV
                else return h
              case hv of
                Nothing -> do
                  setHalfedge v outerPrev
                  return [innerLink, (outerPrev, outerNext)]
                Just hv' -> do
                  let borderPrev' = hv'
                  borderNext' <- next borderPrev'
                  return [innerLink, (borderPrev', outerNext), (outerPrev, borderNext')]
            | (isNew i) -> do
              borderPrev <- prev innerNext
              setHalfedge v borderPrev
              return [innerLink, (borderPrev, outerNext)]
            | (isNew ii) -> do
              borderNext <- next innerPrev
              setHalfedge v outerPrev
              return [innerLink, (outerPrev, borderNext)]


  r <- runMaybeT $ allCheck >> reLink
  case r of
    Nothing -> return outerF
    Just nextCache -> execStateT createMissingEdges halfedges >>=
      (\x -> assert (V.all isJust x) (return $ V.catMaybes x)) >>= \halfedges' ->
      do
        f <- Graph.addFace g
        setHalfedge f (halfedges' V.! (n - 1))
        nextCache2 <- forM indicesTuple (setupHalfedgesF halfedges')
        forM_ indicesTuple (\(i, _) -> setFace (halfedges' V.! i) f)
        forMOf_ (both.traversed.traversed) (nextCache, nextCache2) (uncurry setNext)
        forM_ vertices (adjustIncomingHalfedge g)
        return f

removeFace :: MutableFaceGraph m g
           => Eq (H g)
           => g -> H g -> m ()
removeFace g h = do
  isB <- isBorder h
  f <- assert (not isB) (face h)
  let
    worker hx = do
      setBorder g hx
      nh <- next hx
      hBorder <- isBorder =<< opposite hx
      nhBorder <- isBorder =<< opposite nh
      hxo <- opposite hx
      nhon <- (next <=< opposite) nh
      if hBorder && nhBorder && (hxo == nhon)
        then do
          remove =<< target hx
          when (hx /= h) (remove =<< edge hx)
        else do
          when nhBorder $ do
            setVertexHalfedge =<< (opposite <=< next <=< opposite) nh
            removeTip hx
          when hBorder $ do
            setVertexHalfedge =<< (opposite <=< next) hx
            removeTip =<< (prev <=< opposite) hx
            when (hx /= h) (remove =<< edge hx)
      when (nh /= h) (worker nh)
  worker h

  remove f
  isB' <- isBorder =<< opposite h
  when isB' (remove =<< edge h)

addEdge :: MutableHalfedgeGraph m g
        => g -> V g -> V g -> m (E g)
addEdge g s t = do
  e <- Graph.addEdge g
  (setTarget ?? t) =<< halfedge e
  (setTarget ?? s) =<< (opposite <=< halfedge) e
  return e

splitEdge :: MutableFaceGraph m g
          => Eq (H g)
          => g -> H g -> m (H g)
splitEdge g h = do
  p <- prev h
  opposite =<< splitVertex g p =<< opposite h


joinLoop :: MutableFaceGraph m g
         => (Eq (H g), Eq (F g))
         => g -> H g -> H g -> m (H g)
joinLoop g h1 h2 = do
  isB1 <- isBorder h1
  isB2 <- isBorder h1
  f1 <- face h1
  f2 <- face h2
  assert (isB1 || f1 /= f2) $ return ()
  unless isB1 (remove f1)
  unless isB2 (remove f2)

  let
    worker hx gx = do
      hn <- next hx
      setFace hx =<< (face <=< opposite) gx
      (setHalfedge ?? hx) =<< face hx
      remove =<< (target <=< opposite) gx
      onon <- (next <=< opposite <=< next <=< opposite) gx
      gn <- if onon == gx then (opposite <=< next <=< opposite) gx
        else do
          setNext hx =<< (next <=< opposite) gx
          gx' <- (opposite <=< next <=< opposite) gx
          setTarget gx' =<< target hx
          let
            worker2 hx2 = do
              t <- (next <=< opposite <=< next) hx2
              if t == gx then return hx2
                else do
                  n <- (opposite <=< next) hx2
                  setTarget n =<< target hx
                  worker2 n
          gx'' <- worker2 gx'
          (opposite <=< next) gx'' <* setNext gx'' hn

      if hn /= h1 then worker hn gn else return gn
  h2' <- worker h1 h2
  assert (h2' == h2) $ return ()
  let
    worker3 hx = do
      n <- next hx
      remove =<< edge hx
      when (n /= h2) (worker3 n)
  worker3 h2
  return h1

splitLoop :: MutableFaceGraph m g
          => (Eq (V g), Eq (H g))
          => g -> H g -> H g -> H g -> m (H g)
splitLoop g h i j = do
  th <- target h
  ti <- target i
  tj <- target j
  toh <- (target <=< opposite) h
  toi <- (target <=< opposite) i
  toj <- (target <=< opposite) j
  assert (h /= i && h /= j && i /= j) $ return ()
  assert (th /= toi && ti /= toj && tj /= toh) $ return ()

  hnew <- copy g h
  inew <- copy g i
  jnew <- copy g j
  closeTip hnew =<< addVertex g
  closeTip inew =<< addVertex g
  closeTip jnew =<< addVertex g
  (insertTip ?? hnew) =<< opposite inew
  (insertTip ?? inew) =<< opposite jnew
  (insertTip ?? jnew) =<< opposite hnew

  nh <- next h
  when (nh /= i) $ do
    setNext h i
    setNext hnew nh
    onh <- opposite nh
    let
      worker hx = do
        t <- next hx
        if t == i then return hx
          else do
            setTarget hx =<< target hnew
            worker =<< (opposite <=< next) hx
    nh' <- worker onh
    setTarget nh' =<< target hnew
    setNext nh' inew

  ni <- next i
  when (ni /= j) $ do
    setNext i j
    setNext inew ni
    oni <- opposite ni
    let
      worker hx = do
        t <- next hx
        if t == j then return hx
          else do
            setTarget hx =<< target inew
            worker =<< (opposite <=< next) hx
    ni' <- worker oni
    setTarget ni' =<< target inew
    setNext ni' jnew

  nj <- next j
  when (nj /= h) $ do
    setNext j h
    setNext jnew nj
    onj <- opposite nj
    let
      worker hx = do
        t <- next hx
        if t == h then return hx
          else do
            setTarget hx =<< target jnew
            worker =<< (opposite <=< next) hx
    nj' <- worker onj
    setTarget nj' =<< target jnew
    setNext nj' hnew

  f <- Graph.addFace g
  setFace h f
  setFace i f
  setFace j f
  (setHalfedge ?? h) =<< face h
  f2 <- Graph.addFace g
  (setFace ?? f2) =<< opposite hnew
  (setFace ?? f2) =<< opposite inew
  (setFace ?? f2) =<< opposite jnew
  hf <- (face <=< opposite) hnew
  setHalfedge hf =<< opposite hnew

  (setHalfedge ?? hnew) =<< face hnew
  (setHalfedge ?? inew) =<< face inew
  (setHalfedge ?? jnew) =<< face jnew
  (setHalfedge ?? hnew) =<< target hnew
  (setHalfedge ?? inew) =<< target inew
  (setHalfedge ?? jnew) =<< target jnew

  opposite hnew

splitVertex :: MutableFaceGraph m g
            => Eq (H g)
            => g -> H g -> H g -> m (H g)
splitVertex g h1 h2 = do
  -- add missing assert here

  hnew <- halfedge =<< Graph.addEdge g
  hnewopp <- opposite hnew
  vnew <- addVertex g
  insertHalfedge hnew h2
  insertHalfedge hnewopp h1
  setTarget hnew =<< target h1

  let
    worker hx = do
      setTarget hx vnew
      n <- (opposite <=< next) hx
      if n /= hnewopp
        then worker n
        else return n
  hnewopp' <- worker hnewopp

  setVertexHalfedge hnew
  setVertexHalfedge hnewopp'
  return hnew

joinVertex :: MutableFaceGraph m g
           => (Eq (V g), Eq (H g))
           => g -> H g -> m (H g)
joinVertex g h = do
  hop <- opposite h
  hprev <- prev hop
  gprev <- prev h
  hnext <- next hop
  gnext <- next h
  v_to_remove <- target hop
  v <- target h

  -- add missing assert here

  halfedgeAroundTarget g (\_ hx -> do
                             t <- target hx
                             assert (t == v_to_remove) $
                               setTarget hx v
                         ) hop

  setNext hprev hnext
  setNext gprev gnext
  setHalfedge v gprev

  bg <- isBorder gprev
  unless bg $ do
    (setHalfedge ?? gprev) =<< face gprev

  bh <- isBorder hprev
  unless bh $ do
    (setHalfedge ?? hprev) =<< face hprev

  remove =<< edge h
  remove v_to_remove

  return hprev

makeHole :: MutableFaceGraph m g
         => Eq (H g)
         => g -> H g -> m ()
makeHole g h = do
  isB <- isBorder h
  fd <- assert (not isB) (face h)
  halfedgeAroundFace g (\g' h' -> do
                           isB' <- isBorder =<< opposite  h'
                           assert (not isB') $ setBorder g' h'
                       ) h
  remove fd

fillHole :: MutableFaceGraph m g
         => Eq (H g)
         => g -> H g -> m ()
fillHole g h = do
  f <- Graph.addFace g
  halfedgeAroundFace g (\_ h' -> setFace h' f) h
  setHalfedge f h

addCenterVertex :: MutableFaceGraph m g
                => Eq (H g)
                => g -> H g -> m (H g)
addCenterVertex g h = do
  hnew <- halfedge =<< Graph.addEdge g
  vnew <- addVertex g
  closeTip hnew vnew

  (insertTip ?? h) =<< opposite hnew
  setFace hnew =<< face h
  (setHalfedge ?? h) =<< face h

  h2 <- next =<< opposite hnew
  let
    worker hx = do
      n <- next hx
      when (n /= hnew) $ do
        gnew <- halfedge =<< Graph.addEdge g
        insertTip gnew hnew
        (insertTip ?? hx) =<< opposite gnew
        fnew <- Graph.addFace g
        setFace hx fnew
        setFace gnew fnew
        (setFace ?? fnew) =<< next gnew
        (setHalfedge ?? hx) =<< face hx
        worker =<< (next <=< opposite) gnew
  worker h2

  nhnew <- next hnew
  setFace nhnew =<< face hnew
  setVertexHalfedge hnew
  return hnew

removeCenterVertex :: MutableFaceGraph m g
                   => Eq (H g)
                   => g -> H g -> m (H g)
removeCenterVertex g h = do
  h2 <- (opposite <=< next) h
  hret <- prev h
  let
    worker hx = when (hx /= h) $ do
      gprev <- prev hx
      setVertexHalfedge gprev
      removeTip gprev

      remove =<< face hx

      gnext <- (opposite <=< next) hx
      remove =<< edge hx
      worker gnext
  worker h2
  setVertexHalfedge hret
  removeTip hret
  remove =<< target h
  remove =<< edge h
  setFaceInFaceLoop hret =<< face hret
  (setHalfedge ?? hret) =<< face hret

  return hret

addVertexAndFaceToBorder :: MutableFaceGraph m g
                         => Eq (H g)
                         => g -> H g -> H g -> m (H g)
addVertexAndFaceToBorder g h1 h2 = do
  v <- addVertex g
  f <- Graph.addFace g
  e1 <- Graph.addEdge g
  e2 <- Graph.addEdge g
  he1 <- halfedge e1
  he2 <- halfedge e2
  ohe1 <- opposite he1
  ohe2 <- opposite he2

  setNext ohe1 =<< next h1
  setNext h1 he1
  setTarget ohe1 =<< target h1
  setTarget he1 v
  setNext ohe2 ohe1
  setTarget ohe2 v
  setNext he1 he2
  setTarget he1 v
  setHalfedge v he1
  setNext he2 =<< next h2
  setTarget he2 =<< target h2
  setNext h2 ohe2
  setBorder g he1
  setBorder g he2

  halfedgeAroundFace g (\_ h' -> setFace h' f) ohe1
  setHalfedge f ohe1
  return ohe2

addFaceToBorder :: MutableFaceGraph m g
                => Eq (H g)
                => g -> H g -> H g -> m (H g)
addFaceToBorder g h1 h2 = do
  isB1 <- isBorder h1
  isB2 <- isBorder h2
  nh1 <- next h1
  assert (isB1 && isB2 && h1 /= h2 && nh1 /= h2) (return ())
  f <- Graph.addFace g
  e <- Graph.addEdge g
  newh <- halfedge e
  newhop <- opposite newh

  setNext newhop =<< next h2
  setNext h2 newh
  setNext newh =<< next h1
  setNext h1 newhop
  setTarget newh =<< target h1
  setTarget newhop =<< target h2

  (setHalfedge ?? newhop) =<< target h2
  setBorder g newhop

  halfedgeAroundFace g (\_ h' -> setFace h' f) newh
  setHalfedge f newh
  return newh

joinFace :: MutableFaceGraph m g
         => Eq (H g)
         => g -> H g -> m (H g)
joinFace g h = do
  hop <- opposite h
  hprev <- prev h
  gprev <- prev hop
  f <- face h
  f2 <- face hop

  removeTip hprev
  removeTip gprev

  (unless ?? remove f2) =<< isBorder hop
  isB <- isBorder h

  let
    worker hx = when (hx /= gprev) $ do
      n <- next hx
      setFace n f
      worker n
  worker hprev

  unless isB (setHalfedge f hprev)
  (setHalfedge ?? hprev) =<< target hprev
  (setHalfedge ?? gprev) =<< target gprev

  remove =<< edge h
  return hprev

splitFace :: MutableFaceGraph m g
          => Eq (H g)
          => g -> H g -> H g -> m (H g)
splitFace g h1 h2 = do
  hnew <- halfedge =<< Graph.addEdge g
  fnew <- Graph.addFace g
  insertTip hnew h2
  (insertTip ?? h1) =<< opposite hnew
  setFace hnew =<< face h1
  (setFaceInFaceLoop ?? fnew) =<< opposite hnew
  (setHalfedge ?? hnew) =<< face hnew
  hnew' <- opposite hnew
  (setHalfedge ?? hnew') =<< face hnew'
  return hnew
