-- this module contains the definition of the Editor State, a structure
-- containing all the informations needed to draw a graph in the canvas
-- it also contain functions to modify the editor state

module Editor.EditorState
( EditorState
, emptyES
, editorGetGraph
, editorSetGraph
, editorGetGI
, editorSetGI
, editorGetSelected
, editorSetSelected
, editorGetZoom
, editorSetZoom
, editorGetPan
, editorSetPan
, selectNodeInPosition
, selectEdgeInPosition
, createNode
, createEdges
, deleteSelected
, moveNodes
, moveEdges
, adjustEdges
, changeNodeShape
, changeEdgeStyle
) where

import qualified Data.Map as M
import Data.Graphs hiding (null, empty)
import qualified Data.Graphs as G
import Editor.GraphicalInfo
import Editor.Helper
import Data.List

-- | Graph Editor State
-- A tuple containing all the informations needed to draw the graph in the canvas
-- (graph, GraphicalInfo, elected nodes and edges, zoom, pan)
type EditorState = (Graph String String, GraphicalInfo, ([NodeId],[EdgeId]) , Double, (Double,Double))

-- constructor
emptyES :: EditorState
emptyES = (G.empty, (M.empty, M.empty), ([], []), 1.0, (0.0,0.0))

-- getters and setters
editorGetGraph :: EditorState -> Graph String String
editorGetGraph (g,_,_,_,_) = g

editorSetGraph :: Graph String String-> EditorState -> EditorState
editorSetGraph g (_,gi,s,z,p) = (g,gi,s,z,p)

editorGetGI :: EditorState -> GraphicalInfo
editorGetGI (_,gi,_,_,_) = gi

editorSetGI :: GraphicalInfo -> EditorState -> EditorState
editorSetGI gi (g,_,s,z,p) = (g,gi,s,z,p)

editorGetSelected :: EditorState -> ([NodeId], [EdgeId])
editorGetSelected (_,_,s,_,_) = s

editorSetSelected :: ([NodeId], [EdgeId]) -> EditorState -> EditorState
editorSetSelected s (g,gi,_,z,p) = (g,gi,s,z,p)

editorGetZoom :: EditorState -> Double
editorGetZoom (_,_,_,z,_) = z

editorSetZoom :: Double -> EditorState -> EditorState
editorSetZoom z (g,gi,s,_,p) = (g,gi,s,z,p)

editorGetPan :: EditorState -> (Double,Double)
editorGetPan (_,_,_,_,p) = p

editorSetPan :: (Double,Double) -> EditorState -> EditorState
editorSetPan p (g,gi,s,z,_) = (g,gi,s,z,p)


-- node/edge intersection functions
-- check if a given point is inside a node
selectNodeInPosition:: GraphicalInfo -> (Double,Double) -> Maybe NodeId
selectNodeInPosition (nodesG,_) (x,y) =
  case find (\n -> isSelected (snd n)) $ (M.toList nodesG) of
    Nothing -> Nothing
    Just (k,a) -> Just $ NodeId k
  where isSelected = (\n -> let (nx,ny) = position  n
                                (w,h) = dims n
                                l = max w h
                            in case shape n of
                              NCircle -> pointDistance (x,y) (nx,ny) < l/2
                              NRect -> pointInsideRectangle (x,y) (nx,ny,w,h)
                              NQuad -> pointInsideRectangle (x,y) (nx,ny,l,l) )

-- check if a given point is close of an edge control point
selectEdgeInPosition:: GraphicalInfo -> (Double,Double) -> Maybe EdgeId
selectEdgeInPosition (_,edgesG) (x,y) =
  case find (\e -> isSelected (snd e)) $ (M.toList edgesG) of
    Nothing -> Nothing
    Just (k,a) -> Just $ EdgeId k
  where isSelected = (\e -> pointDistance (x,y) (cPosition e) < 5)

-- create/delete operations ----------------------------------------------------
-- create a new node with it's default Info and GraphicalInfo
createNode :: EditorState -> GIPos -> GIDim -> String -> NodeShape -> GIColor -> GIColor -> EditorState
createNode es pos dim content nshape color lcolor = editorSetGraph newGraph . editorSetGI newGI . editorSetSelected ([nid], []) $ es
  where
    graph = editorGetGraph es
    nid = head $ newNodes graph
    newGraph = insertNodeWithPayload nid content graph
    newNgi = NodeGI {position = pos, fillColor = color, lineColor = lcolor, dims = dim, shape = nshape}
    newGI = (M.insert (fromEnum nid) newNgi $ fst (editorGetGI es) , snd (editorGetGI es))


-- create edges between the selected nodes and a target node
createEdges:: EditorState -> NodeId -> EdgeStyle -> (Double,Double,Double) -> EditorState
createEdges es dstNode estyle ecolor = editorSetGraph newGraph . editorSetGI (ngiM, newegiM) . editorSetSelected ([],createdEdges) $ es
  where selectedNodes = fst $ editorGetSelected es
        graph = editorGetGraph es
        (ngiM,egiM) = editorGetGI es
        (newGraph, newegiM, createdEdges) = foldl create (graph, egiM, []) selectedNodes
        create = (\(g,giM,eids) nid -> let
                                    eid = head $ newEdges g
                                    ng = insertEdgeWithPayload eid nid dstNode "" g
                                    (newPos,center) = if (dstNode == nid) then (newLoopPos nid (g,(ngiM,egiM)),False) else newEdgePos nid dstNode (g,(ngiM,egiM))
                                    negi = EdgeGI {cPosition = newPos, color = ecolor, centered = center, style = estyle}
                                  in (ng, M.insert (fromEnum eid) negi giM, eid:eids))

-- delete the selection
deleteSelected:: EditorState -> EditorState
deleteSelected es = editorSetSelected ([],[]) . editorSetGI (newngiM, newegiM) . editorSetGraph newGraph $ es
  where graph = editorGetGraph es
        (nids,eids) = editorGetSelected es
        (ngiM, egiM) = editorGetGI es
        newngiM = foldl (\giM n -> M.delete n giM) ngiM (map fromEnum nids)
        newegiM = foldl (\giM n -> M.delete n giM) egiM (map fromEnum eids)
        graph' = foldl (\g e -> removeEdge e g) graph eids
        newGraph = foldl (\g n -> removeNodeAndIncidentEdges n g) graph' nids

-- auxiliar functions to createEdges
-- return a list of numbers
edgesFromTo :: NodeInContext n e -> NodeInContext n e -> [Edge e]
edgesFromTo (n, context) (n', _) = foldl edgesTo [] econtexts
  where
    econtexts  = outgoingEdges context
    nid' = nodeId n'
    edgesTo = \l (_ , e, (tgt,_)) -> case nodeId tgt == nid' of
      True -> e:l
      False -> l


-- calculate a position for the new edge
newEdgePos :: NodeId -> NodeId -> (Graph a b, GraphicalInfo) -> ((Double,Double),Bool)
newEdgePos nid nid' (g, giM)= (pos,isMid)
  where getPos = \n -> position . getNodeGI (fromEnum n) $ fst giM
        (srcPos,dstPos) = applyPair getPos (nid,nid')
        mContextSrc = lookupNodeInContext nid g
        mContextTgt = lookupNodeInContext nid' g
        k = case (mContextSrc,mContextTgt) of
          (Just csrc, Just ctgt) -> let thisLength = genericLength (edgesFromTo csrc ctgt)
                                        otherLength = length (edgesFromTo ctgt csrc)
                                    in thisLength + if otherLength > 0 then 1 else 0
          _ -> 0
        mid = midPoint srcPos dstPos
        (pos,isMid) = if k == 0
          then (mid,True)
          else let a = angle srcPos dstPos
               in (pointAt (a + pi/2) (20*k) mid, False)

-- calculate a position fot the new loop
newLoopPos :: NodeId -> (Graph a b, GraphicalInfo) -> (Double,Double)
newLoopPos nid (g, giM)= pos
 where getPos = \n -> position . getNodeGI (fromEnum n) $ fst giM
       nodePos =  getPos nid
       mContext = lookupNodeInContext nid g
       k = case mContext of
         Just c -> genericLength $ edgesFromTo c c
         _ -> 0
       mid = addPoint nodePos (0,-50)
       pos = if k == 0
         then mid
         else addPoint mid (0, (-20) * k)

moveNodes:: EditorState -> (Double,Double) -> (Double,Double) -> EditorState
moveNodes es (xold,yold) (xnew,ynew) = editorSetGI (movedNGIs,movedEGIs)  es
  where
      (sNodes, sEdges) = editorGetSelected es
      graph = editorGetGraph es
      (ngiM,egiM) = editorGetGI es
      (deltaX, deltaY) = (xnew-xold, ynew-yold)
      -- move os nodos
      moveN = \giMap (NodeId nid) -> let gi = getNodeGI nid giMap
                                         (ox, oy) = position gi
                                     in M.insert nid (nodeGiSetPosition (ox+deltaX,oy+deltaY) gi) giMap
      movedNGIs = foldl moveN ngiM sNodes
      -- move as arestas que estão entre os nodos movidos
      moveE = \giMap edge -> let
                                a = sourceId edge
                                b = targetId edge
                                getPos = \(NodeId nid) -> position . getNodeGI nid $ movedNGIs
                                aPos = getPos a
                                bPos = getPos b
                                eid = fromEnum $ edgeId edge
                                gi = getEdgeGI eid egiM
                                (xd,yd) = addPoint (cPosition gi) (deltaX,deltaY)
                             in case (edgeId edge `elem` sEdges, a == b, any (`elem` sNodes) [a,b], centered gi) of
                                (False, True, True, _) -> M.insert eid (edgeGiSetPosition (xd,yd) gi) giMap
                                (False, False, True, True) -> M.insert eid (edgeGiSetPosition (midPoint aPos bPos) gi) giMap
                                _ -> giMap
      movedEGIs = foldl moveE egiM (edges graph)

moveEdges:: EditorState -> (Double,Double) -> (Double,Double) -> EditorState
moveEdges es (xold,yold) (xnew,ynew) = editorSetGI (ngi,newegi) es
  where graph = editorGetGraph es
        (sNodes,sEdges) = editorGetSelected es
        (deltaX, deltaY) = (xnew-xold,ynew-yold)
        (ngi,egi) = editorGetGI es
        moveE = (\egiM eid -> case lookupEdge eid graph of
          Just edge -> let  gi = getEdgeGI (fromEnum $ eid) egi
                            (xe, ye) = cPosition gi
                            newPos = (xe+deltaX, ye+deltaY)
                            srcPos = position . getNodeGI (fromEnum $ sourceId edge) $ ngi
                            dstPos = position . getNodeGI (fromEnum $ targetId edge) $ ngi
                            mustCenter = pointLineDistance newPos srcPos dstPos < 10
                       in M.insert (fromEnum eid) (edgeGiSetCentered mustCenter . edgeGiSetPosition newPos $ gi) egiM
          Nothing -> egiM)
        newegi = foldl moveE egi sEdges

-- adjust the selected edges positions if the propriety centered is True
adjustEdges:: EditorState -> EditorState
adjustEdges es = editorSetGI (ngiM,newEgiM) es
  where graph = editorGetGraph es
        (ngiM,egiM) = editorGetGI es
        adjust = (\giM eid -> case lookupEdge eid graph of
          Nothing -> giM
          Just edge -> let
                          srcPos = position $ getNodeGI (fromEnum $ sourceId edge) ngiM
                          dstPos = position $ getNodeGI (fromEnum $ targetId edge) ngiM
                          gi = getEdgeGI (fromEnum eid) egiM
                        in if centered gi
                          then M.insert (fromEnum eid) (edgeGiSetPosition (midPoint srcPos dstPos) gi) giM
                          else giM)
        newEgiM = foldl adjust egiM (snd . editorGetSelected $ es)

-- change the selected nodes shape
changeNodeShape :: EditorState -> NodeShape -> EditorState
changeNodeShape es s = editorSetGI (newNgiM, egiM) es
  where
      nids = fst $ editorGetSelected es
      (ngiM, egiM) = editorGetGI es
      newNgiM = M.mapWithKey (\k gi -> if NodeId k `elem` nids then nodeGiSetShape s gi else gi) ngiM

-- change the selected edges style
changeEdgeStyle :: EditorState -> EdgeStyle -> EditorState
changeEdgeStyle es s = editorSetGI (ngiM, newEgiM) es
  where
    eids = snd $ editorGetSelected es
    (ngiM, egiM) = editorGetGI es
    newEgiM = M.mapWithKey (\k gi -> if EdgeId k `elem` eids then edgeGiSetStyle s gi else gi) egiM
