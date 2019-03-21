module Editor.DiaGraph
( DiaGraph
, isDiaGraphEqual
, diagrUnion
, empty
)where

import qualified Data.Map as M
import Data.Graphs hiding (null, empty)
import qualified Data.Graphs as G
import Editor.GraphicalInfo

-- |DiaGraph
-- A pair containing a graph and it's graphical information
type DiaGraph = (Graph String String ,GraphicalInfo)

isDiaGraphEqual :: DiaGraph -> DiaGraph -> Bool
isDiaGraphEqual (g1,gi1) (g2,gi2) = g1 == g2 && nodesGiEq && edgesGiEq
  where
    nodesGiEq = sameLength (M.elems $ fst gi1) (M.elems $ fst gi2) && all (\(x,y) -> x == y) (zip (M.elems $ fst gi1) (M.elems $ fst gi2))
    edgesGiEq = sameLength (M.elems $ snd gi1) (M.elems $ snd gi2) && all (\(x,y) -> x == y) (zip (M.elems $ snd gi1) (M.elems $ snd gi2))
    sameLength l1 l2 = length l1 == length l2

-- union between two DiaGraphs
diagrUnion :: DiaGraph -> DiaGraph -> DiaGraph
diagrUnion (g1,(ngiM1,egiM1)) (g2,(ngiM2,egiM2)) = (g3,(ngiM3,egiM3))
  where
    ns2 = nodes g2
    es2 = edges g2
    newNids = take (length ns2) $ newNodes g1
    newEids = take (length es2) $ newEdges g1
    maxNidG1 = if G.null g1 then 0 else maximum $ nodeIds g1
    maxEidG1 = if null $ edgeIds g1 then 0 else maximum $ edgeIds g1
    ns2' = map (\n -> Node (nodeId n + maxNidG1) (nodeInfo n)) ns2
    es2' = map (\e -> Edge (edgeId e + maxEidG1) (sourceId e + maxNidG1) (targetId e + maxNidG1) (edgeInfo e)) es2
    ns3 = concat [nodes g1, ns2']
    es3 = concat [edges g1, es2']
    g3 = fromNodesAndEdges ns3 es3
    ngiM2' = M.fromList $ zipWith (\(NodeId nid) gi -> (nid, gi)) newNids (M.elems ngiM2)
    egiM2' = M.fromList $ zipWith (\(EdgeId eid) gi -> (eid, gi)) newEids (M.elems egiM2)
    ngiM3 = M.union ngiM1 ngiM2'
    egiM3 = M.union egiM1 egiM2'

empty :: DiaGraph
empty = (G.empty, (M.empty, M.empty))
