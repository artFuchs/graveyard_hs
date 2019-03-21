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
) where

import qualified Data.Map as M
import Data.Graphs hiding (null, empty)
import qualified Data.Graphs as G
import Editor.GraphicalInfo

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
