-- essa é uma ideia de interface para a expansão do projeto graph-editor para um editor de gramática de grafo.
-- meramente um protótipo, sem incluir as funcionalidades do editor

module Main where

import Graphics.UI.Gtk hiding (rectangle)
import Control.Monad.IO.Class
import Control.Monad
import Data.IORef
import Data.Tree
import qualified Data.Text as T
import Editor.UIBuilders
import Editor.GraphicalInfo
import Editor.Render

type Info = ([(Int,String,NodeGI)],[(Int,Int,Int,String,EdgeGI)])
data GraphType = MenuGraph | HostGraph Info | TypeGraph Info | RuleGraph Info deriving (Show)
type Name = String
data GraphStore = Store GraphType Name

main = do
  initGUI

  -- IORefs
  pathRef <- newIORef [0]

  -- Definição da interface
  (frameR, entryNameR, comboBoxNodeTypeR, comboBoxEdgeTypeR, comboBoxOperationR, (hBoxNodeType, hBoxEdgeType)) <- buildRuleMenu
  (frameH, entryNameH, comboBoxNodeType, comboBoxEdgeType, (hBoxNodeType, hBoxEdgeType)) <- buildHostMenu
  (frameT, entryNameT, colorBtn, lineColorBtn, radioShapes, radioStyles, (hBoxColor, frameShape, frameStyle)) <- buildTypeMenu
  let propMenuFrames = (frameH, frameT, frameR)
  (maybeMenubar, new, opn, svn, sva, opg, svg, udo, rdo, cpy, pst, cut, sla, sle, sln, hlp') <- buildMaybeMenubar
  (treePanel, treeview, renderer, btnNew, btnRemove) <- buildTreePanel
  (window,canvas,hPaneMain) <- buildMainWindow maybeMenubar frameH treePanel

  -- criar os modelos para as comboboxes do buildHostMenu
  sequence $ map (comboBoxAppendText comboBoxNodeType . T.pack) ["None", "A", "B", "C"]
  comboBoxSetActive comboBoxNodeType 0

  sequence $ map (comboBoxAppendText comboBoxEdgeType . T.pack) ["None", "AA", "AB", "AC", "BB", "BC", "CC"]
  comboBoxSetActive comboBoxEdgeType 0

  sequence $ map (comboBoxAppendText comboBoxNodeTypeR . T.pack) ["None", "A", "B", "C"]
  comboBoxSetActive comboBoxNodeTypeR 0

  sequence $ map (comboBoxAppendText comboBoxEdgeTypeR . T.pack) ["None", "AA", "AB", "AC", "BB", "BC", "CC"]
  comboBoxSetActive comboBoxEdgeTypeR 0

  sequence $ map (comboBoxAppendText comboBoxOperationR . T.pack) ["None", "Create", "Delete"]
  comboBoxSetActive comboBoxOperationR 0

  -- criar uma estrutura para armazenar na treeStore
  let typeGraphTree = Node (Store MenuGraph "TypeGraphs") [Node (Store (TypeGraph testTypeGraph) "TypeGraph") []]
      graphTree = Node (Store MenuGraph "Graphs") [Node (Store (HostGraph testHostGraph) "Graph") []]
      ruleGraphTree = Node (Store MenuGraph "Rules") [Node (Store (RuleGraph ([],[])) "Rule0") []]
  store <- treeStoreNew [Node (Store MenuGraph "Project") [typeGraphTree, graphTree, ruleGraphTree]]
  projectCol <- treeViewGetColumn treeview 0
  case projectCol of
    Nothing -> return ()
    Just col -> do
      treeViewSetModel treeview (Just store)
      cellLayoutSetAttributes col renderer store $ \(Store t n) -> [cellText:=n]
      treeViewExpandAll treeview
      treeViewSetCursor treeview [0,1,0] Nothing
      writeIORef pathRef [0,1,0]

  --binding de eventos
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  treeview `on` cursorChanged $ do
    selection <- treeViewGetSelection treeview
    maybeIt <- treeSelectionGetSelected selection
    case maybeIt of
      Nothing -> return ()
      Just it -> do
        path <- treeModelGetPath store it
        (Store t n) <- treeStoreGetValue store path
        changePropMenu t hPaneMain propMenuFrames
        writeIORef pathRef path
        widgetShowAll window

  canvas `on` draw $ do
    path <- liftIO $ readIORef pathRef
    currentStore <- liftIO $ treeStoreGetValue store path
    case currentStore of
      Store MenuGraph n -> drawGraph canvas ([],[])
      Store (TypeGraph g) n -> drawGraph canvas g
      Store (HostGraph g) n -> drawGraph canvas g
      Store (RuleGraph g) n -> drawGraph canvas g
  widgetShowAll window
  mainGUI


changePropMenu :: GraphType -> HPaned -> (Frame,Frame,Frame) -> IO ()
changePropMenu MenuGraph _ _ = return ()

changePropMenu graphT hpane (hFrame,tFrame,rFrame) = do
  panedchild <- panedGetChild2 hpane
  let theFrame = case graphT of
                  HostGraph i -> hFrame
                  TypeGraph i -> tFrame
                  RuleGraph i -> rFrame
  case panedchild of
    Nothing -> panedPack2 hpane hFrame False True
    Just frame -> if toWidget theFrame == frame
      then return ()
      else do
        containerRemove hpane frame
        panedPack2 hpane theFrame False True

drawGraph canvas (nodes, edges) = do
  context <- liftIO $ widgetGetPangoContext canvas
  forM edges (\(eid,nid1,nid2,content,gi) ->
    let
      (_,_,gi1) = nodes!!(nid1-1)
      (_,_,gi2) = nodes!!(nid2-1)
    in renderEdge gi content False gi1 gi2 context )
  forM nodes (\(nid,content,gi) -> renderNode gi content False context)
  return ()

-- typeNode = (id, name of the type, GI)
testTypeNodes = [ (1,"A",NodeGI {position = (100,100), fillColor = (1,0.5,0.5), lineColor = (0,0,0), dims = (30,20), shape = NQuad})
                , (2,"B",NodeGI {position = (200,100), fillColor = (0.2,0.5,0.5), lineColor = (0,0,0), dims = (30,20), shape = NCircle})
                , (3,"C",NodeGI {position = (200,200), fillColor = (0.2,0.7,0.9), lineColor = (0,0,0), dims = (30,20), shape = NRect}) ]
-- typeEdge = (id, src id, tgt id, name of the type, GI)
testTypeEdges = [ (1,1,1,"AA",EdgeGI {cPosition = (100,50), color = (0.8,0.6,0), style = ENormal, centered = True})
                , (2,1,2,"AB",EdgeGI {cPosition = (150,100), color = (0,0,0), style = ESlashed, centered = True})
                , (3,1,3,"AC",EdgeGI {cPosition = (150,150), color = (0,0,0), style = EPointed, centered = True})
                , (4,2,2,"BB",EdgeGI {cPosition = (200,50), color = (0.8,0.6,0), style = ENormal, centered = True})
                , (5,2,3,"BC",EdgeGI {cPosition = (200,150), color = (0,0,0), style = ESlashed, centered = True})
                , (6,3,3,"CC",EdgeGI {cPosition = (200,250), color = (0.8,0.6,0), style = ENormal, centered = True}) ]
testTypeGraph = (testTypeNodes, testTypeEdges)

typesNGI = map (\(nid,nt,gi) -> (nt,gi)) testTypeNodes
typesEGI = map (\(eid,src,tgt,et,gi) -> (et,gi)) testTypeEdges

-- hostNode = (id, type, name, position)
testHostNodes = [ (1,"A","node 1", (100,100), (50,20))
                , (2,"B","node 2", (300,100), (50,20))
                , (3,"C","node 3", (300,300), (50,20))
                , (4,"A","node 4", (100,300), (50,20)) ]
testHostNodes' = foldl (\ns (nid,t,n,pos,dim) -> case lookup t typesNGI of
                                                Just gi -> (nid, n, nodeGiSetPosition pos . nodeGiSetDims dim $ gi) : ns
                                                Nothing -> ns)
                       []
                       (reverse testHostNodes)
-- hostEdge = (id, src id, tgt id, type, name, position)
testHostEdges = [ (1,1,2,"AB","",(200,100))
                , (2,2,3,"BC","",(300,200))
                , (3,1,4,"AA","",(100,200))
                , (4,4,3,"AC","",(200,300)) ]
testHostEdges' = foldl (\es (eid, src, tgt, t, n, pos) -> case lookup t typesEGI of
                                                            Just gi -> (eid, src, tgt, n, edgeGiSetPosition pos gi) : es
                                                            Nothing -> es)
                       []
                       (reverse testHostEdges)
testHostGraph = (testHostNodes',testHostEdges')
