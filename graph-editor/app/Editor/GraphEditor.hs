module Editor.GraphEditor
( startGUI
)where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Layout
import Data.List
import qualified Data.Text as T
import qualified Control.Exception as E
import qualified Data.Map as M
import Data.Graphs hiding (null)
import qualified Data.Graphs as G
import Editor.GraphicalInfo
import Editor.Render
import Editor.Helper
import Editor.UIBuilders

-- | Graph Editor State
-- A tuple containing all the informations needed to draw the graph in the canvas
-- (graph, GraphicalInfo, elected nodes and edges, zoom, pan)
type EditorState = (Graph String String, GraphicalInfo, ([NodeId],[EdgeId]) , Double, (Double,Double))

-- basic contructor
emptyES :: EditorState
emptyES = (empty, (M.empty, M.empty), ([], []), 1.0, (0.0,0.0))

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



--------------------------------------------------------------------------------
-- MODULE FUNCTIONS ------------------------------------------------------------
--------------------------------------------------------------------------------

-- startGUI
-- creates the Graphical User Interface using the UIBuilders module and do the bindings
startGUI :: IO()
startGUI = do
  -- init GTK
  initGUI

  -- GUI definition ------------------------------------------------------------
  -- help window ---------------------------------------------------------------
  helpWindow <- buildHelpWindow

  -- main window ---------------------------------------------------------------
  -- creates the menu bar
  (maybeMenubar,new,opn,svn,sva,opg,svg,udo,rdo,hlp) <- buildMaybeMenubar
  -- creates the inspector panel on the right
  (frameProps, entryNodeID, entryName, colorBtn, lineColorBtn, radioShapes, radioStyles, propBoxes) <- buildPropMenu
  let
    propWidgets = (entryNodeID, entryName, colorBtn, lineColorBtn, radioShapes, radioStyles)
    [radioCircle, radioRect, radioQuad] = radioShapes
    [radioNormal, radioPointed, radioSlashed] = radioStyles
  -- creates the tree panel on the left
  (treePanel, treeview, treeRenderer, btnNew, btnRmv) <- buildTreePanel
  -- creates the main window, containing the canvas and the built panels
  (window, canvas) <- buildMainWindow maybeMenubar frameProps treePanel
  -- shows the main window
  widgetShowAll window

  -- init the editor variables  ------------------------------------------------
  st              <- newIORef emptyES -- estado do editor: todas as informações necessárias para desenhar o grafo
  oldPoint        <- newIORef (0.0,0.0) -- ultimo ponto em que o botão do mouse foi pressionado
  squareSelection <- newIORef Nothing -- estado da caixa de seleção - Maybe (x,y,w,h)
  undoStack       <- newIORef ([] :: [(Graph String String, GraphicalInfo)]) -- pilha de undo
  redoStack       <- newIORef ([] :: [(Graph String String, GraphicalInfo)]) -- pilha de redo
  movingGI        <- newIORef False -- se o usuario começou a mover algum objeto
  currentShape    <- newIORef NCircle
  currentStyle    <- newIORef ENormal
  currentC        <- newIORef (1,1,1)
  currentLC       <- newIORef (0,0,0)
  clipboard       <- newIORef (empty, (M.empty, M.empty)) -- clipboard - (Graph, GraphicalInfo)
  fileName        <- newIORef (Nothing :: Maybe String) -- arquivo aberto
  currentGraph    <- newIORef [0]

  -- init an model to display in the tree panel --------------------------------
  store <- listStoreNew [("new", emptyES, [], [])]
  projectCol <- treeViewGetColumn treeview 0
  case projectCol of
    Nothing -> return ()
    Just col -> do
      treeViewSetModel treeview (Just store)
      cellLayoutSetAttributes col treeRenderer store $ \(name,_,_,_) -> [cellText := name]



  -- EVENT BINDINGS ------------------------------------------------------------
  -- event bindings for the canvas ---------------------------------------------
  -- drawing event
  canvas `on` draw $ do
    es <- liftIO $ readIORef st
    sq <- liftIO $ readIORef squareSelection
    drawGraph es sq canvas

  -- mouse button pressed on canvas
  canvas `on` buttonPressEvent $ do
    b <- eventButton
    (x,y) <- eventCoordinates
    ms <- eventModifierAll
    es <- liftIO $ readIORef st
    click <- eventClick
    let z = editorGetZoom es
        (px,py) = editorGetPan es
        (x',y') = (x/z - px, y/z - py)
    liftIO $ do
      writeIORef oldPoint (x',y')
      widgetGrabFocus canvas
    case (b, click == DoubleClick) of
      (LeftButton, True) -> do
        let (n,e) = editorGetSelected es
        if null n && null e
          then return ()
          else liftIO $ widgetGrabFocus entryName
      -- left button: select nodes and edges
      (LeftButton, False)  -> liftIO $ do
        let (oldSN,oldSE) = editorGetSelected es
            graph = editorGetGraph es
            gi = editorGetGI es
            sNode = case checkSelectNode gi (x',y') of
              Nothing -> []
              Just nid -> [nid]
            sEdge = case checkSelectEdge gi (x',y') of
              Nothing -> []
              Just eid -> [eid]
        -- add/remove elements of selection
        case (sNode, sEdge, Shift `elem` ms, Control `elem` ms) of
          -- clicked in blank space with Shift not pressed
          ([], [], False, _) -> do
            modifyIORef st (editorSetSelected ([],[]))
            writeIORef squareSelection $ Just (x',y',0,0)
          -- selected nodes or edges with shift pressed:
          (n, e, False, _) -> do
            let nS = if null n then False else n!!0 `elem` oldSN
                eS = if null e then False else e!!0 `elem` oldSE
            if nS || eS
              then return ()
              else modifyIORef st (editorSetSelected (n, e))
          -- selected nodes or edges with Shift pressed -> add to selection
          (n, e, True, False) -> do
            let jointSN = foldl (\ns n -> if n `notElem` ns then n:ns else ns) [] $ sNode ++ oldSN
                jointSE = foldl (\ns n -> if n `notElem` ns then n:ns else ns) [] $ sEdge ++ oldSE
            modifyIORef st (editorSetGraph graph . editorSetSelected (jointSN,jointSE))
          -- selected nodes or edges with Shift + Ctrl pressed -> remove from selection
          (n, e, True, True) -> do
            let jointSN = if null n then oldSN else delete (n!!0) oldSN
                jointSE = if null e then oldSE else delete (e!!0) oldSE
            modifyIORef st (editorSetGraph graph . editorSetSelected (jointSN,jointSE))
          _ -> return ()
        widgetQueueDraw canvas
        updatePropMenu st currentC currentLC propWidgets propBoxes
      -- clique com o botão direito: cria nodos e insere edges entre nodos
      (RightButton, _) -> liftIO $ do
        let g = editorGetGraph es
            gi = editorGetGI es
            dstNode = checkSelectNode gi (x',y')
        context <- widgetGetPangoContext canvas
        stackUndo undoStack redoStack es
        case (Control `elem` ms, dstNode) of
          -- nenhum nodo foi selecionado, criar nodo
          (False, Nothing) -> do
            shape <- readIORef currentShape
            c <- readIORef currentC
            lc <- readIORef currentLC
            createNode st (x',y') context shape c lc
          -- um nodo foi selecionado, criar edges levando a esse nodo
          (False, Just nid) -> do
            estyle <- readIORef currentStyle
            color <- readIORef currentLC
            modifyIORef st (\es -> createEdges es nid estyle color)
          -- ctrl pressionado, emulação do botão do meio do mouse
          (True,_) -> return ()
        widgetQueueDraw canvas
        updatePropMenu st currentC currentLC propWidgets propBoxes
      _           -> return ()

    return True

  -- movimento do mouse
  canvas `on` motionNotifyEvent $ do
    ms <- eventModifierAll
    (x,y) <- eventCoordinates
    (ox,oy) <- liftIO $ readIORef oldPoint
    es <- liftIO $ readIORef st
    let leftButton = Button1 `elem` ms
        middleButton = Button2 `elem` ms || Button3 `elem` ms && Control `elem` ms
        (sNodes, sEdges) = editorGetSelected es
        z = editorGetZoom es
        (px,py) = editorGetPan es
        (x',y') = (x/z - px, y/z - py)
    case (leftButton, middleButton, sNodes, sEdges) of
      (True, False, [], []) -> liftIO $ do
        modifyIORef squareSelection $ liftM $ (\(a,b,c,d) -> (a,b,x'-a,y'-b))
        sq <- readIORef squareSelection
        widgetQueueDraw canvas
      (True, False, n, e) -> liftIO $ do
        modifyIORef st (\es -> moveNodes es (ox,oy) (x',y'))
        modifyIORef st (\es -> moveEdges es (ox,oy) (x',y'))
        writeIORef oldPoint (x',y')
        mv <- readIORef movingGI
        if not mv
          then do
            writeIORef movingGI True
            stackUndo undoStack redoStack es
          else return ()
        widgetQueueDraw canvas
      (False ,True, _, _) -> liftIO $ do
        let (dx,dy) = (x'-ox,y'-oy)
        modifyIORef st (editorSetPan (px+dx, py+dy))
        widgetQueueDraw canvas
      (_,_,_,_) -> return ()
    return True

  -- soltar botão do mouse
  canvas `on` buttonReleaseEvent $ do
    b <- eventButton
    case b of
      LeftButton -> liftIO $ do
        writeIORef movingGI False
        es <- readIORef st
        sq <- readIORef squareSelection
        let (n,e) = editorGetSelected es
        case (editorGetSelected es,sq) of
          (([],[]), Just (x,y,w,h)) -> do
            let graph = editorGetGraph es
                (ngiM, egiM) = editorGetGI es
                sNodes = map NodeId $ M.keys $
                                      M.filter (\ngi -> let pos = position ngi
                                                        in pointInsideRectangle pos (x + (w/2), y + (h/2), abs w, abs h)) ngiM
                sEdges = map EdgeId $ M.keys $
                                      M.filter (\egi -> let pos = cPosition egi
                                                        in pointInsideRectangle pos (x + (w/2), y + (h/2), abs w, abs h)) egiM
                newEs = editorSetSelected (sNodes, sEdges) $ es
            writeIORef st newEs
            updatePropMenu st currentC currentLC propWidgets propBoxes
          ((n,e), Nothing) -> modifyIORef st (adjustEdges)
          (_,_) -> return ()
      _ -> return ()
    liftIO $ do
      writeIORef squareSelection Nothing
      widgetQueueDraw canvas
    return True

  -- roda do mouse
  canvas `on` scrollEvent $ do
    d <- eventScrollDirection
    ms <- eventModifierAll
    case (Control `elem` ms, d) of
      (True, ScrollUp)  -> liftIO $ do
        modifyIORef st (\es -> editorSetZoom (editorGetZoom es * 1.1) es )
        widgetQueueDraw canvas
      (True, ScrollDown) -> liftIO $ do
        modifyIORef st (\es -> if (editorGetZoom es * 0.9) > 0.6 then editorSetZoom (editorGetZoom es * 0.9) es else es)
        widgetQueueDraw canvas
      _ -> return ()
    return True

  -- teclado
  canvas `on` keyPressEvent $ do
    k <- eventKeyName
    ms <- eventModifierAll
    liftIO $ do
      pos <- readIORef oldPoint
      context <- widgetGetPangoContext canvas
      case (Control `elem` ms, Shift `elem` ms, T.unpack $ T.toLower k) of
        -- <delete> : delete selection
        (False,False,"delete") -> do
          es <- readIORef st
          modifyIORef st (\es -> deleteSelected es)
          stackUndo undoStack redoStack es
          widgetQueueDraw canvas
        -- CTRL + <+>/<->/<=> : zoom controls
        (True,_,"plus") -> do
          modifyIORef st (\es -> editorSetZoom (editorGetZoom es * 1.1) es )
          widgetQueueDraw canvas
        (True,_,"minus") -> do
          modifyIORef st (\es -> if (editorGetZoom es * 0.9) > 0.6 then editorSetZoom (editorGetZoom es * 0.9) es else es)
          widgetQueueDraw canvas
        (True,_,"equal") -> do
          modifyIORef st (\es -> editorSetZoom 1.0 es )
          widgetQueueDraw canvas
        -- CTRL + <0> : reset pan & zoom
        (True,_,"0") -> do
          modifyIORef st (\es -> editorSetZoom 1 $ editorSetPan (0,0) es )
          widgetQueueDraw canvas
        -- CTRL + [SHIFT] + A : select/desselect all
        (True, True, "a") -> do
          modifyIORef st $ editorSetSelected ([],[])
          widgetQueueDraw canvas
        (True, False, "a") -> do
          modifyIORef st (\es -> let g = editorGetGraph es
                                 in editorSetSelected (nodeIds g, edgeIds g) es)
          widgetQueueDraw canvas

        -- CTRL + N : create a new file
        (True, False, "n") -> do
          modifyIORef st (\es -> (empty, (M.empty, M.empty),([],[]),1.0,(0.0,0.0)))
          listStoreClear store
          listStoreAppend store ("new",emptyES,[], [])
          writeIORef fileName Nothing
          writeIORef undoStack []
          writeIORef redoStack []
          set window [windowTitle := "Graph Editor"]
          widgetQueueDraw canvas
        -- CTRL + SHIFT + S : save file as
        (True, True, "s") -> actionActivate sva
        -- CTRL + S : save file
        (True, False, "s") -> actionActivate svn
        -- CTRL + O : open file
        (True, False, "o") -> actionActivate opn
        -- CTRL + Z/R : undo/redo
        (True, False, "z") -> do
          applyUndo undoStack redoStack st
          widgetQueueDraw canvas
        (True, False, "r") -> do
          applyRedo undoStack redoStack st
          widgetQueueDraw canvas
        -- CTRL + C/V/X : copy/paste/cut
        (True, False, "c") -> do
          es <- readIORef st
          writeIORef clipboard $ copySelected es
        (True, False, "v") -> do
          es <- readIORef st
          clip <- readIORef clipboard
          stackUndo undoStack redoStack es
          modifyIORef st (pasteClipBoard clip)
          widgetQueueDraw canvas
        (True, False, "x") -> do
          es <- readIORef st
          writeIORef clipboard $ copySelected es
          modifyIORef st (\es -> deleteSelected es)
          stackUndo undoStack redoStack es
          widgetQueueDraw canvas
        (False,False,"f2") -> widgetGrabFocus entryName
        _       -> return ()

    return True

  -- tratamento de eventos -- menu toolbar -------------------------------------
  new `on` actionActivated $ do
    writeIORef fileName Nothing
    treeViewSetCursor treeview [0] Nothing
    listStoreClear store
    listStoreAppend store ("new", emptyES, [], [])
    writeIORef st emptyES
    writeIORef undoStack []
    writeIORef redoStack []
    set window [windowTitle := "Graph Editor"]
    widgetQueueDraw canvas

  opn `on` actionActivated $ do
    mg <- loadFile window loadProject
    case mg of
      Just (list,fn) -> do
        if length list > 0
          then do
            listStoreClear store
            let plist = map (\(n,e) -> (n,e,[],[])) list
            forM plist (listStoreAppend store)
            let (name,es) = list!!0
            writeIORef st es
            writeIORef fileName $ Just fn
            writeIORef undoStack []
            writeIORef redoStack []
            set window [windowTitle := "Graph Editor - " ++ fn]
            widgetQueueDraw canvas
          else return ()
      Nothing -> return ()

  svn `on` actionActivated $ do
    currentES <- readIORef st
    undo <- readIORef undoStack
    redo <- readIORef redoStack
    [path] <- readIORef currentGraph
    (name, _, _, _)<- listStoreGetValue store path
    listStoreSetValue store path (name, currentES, undo, redo)
    saveFile store saveProject fileName window True

  sva `on` actionActivated $ do
    currentES <- readIORef st
    undo <- readIORef undoStack
    redo <- readIORef redoStack
    [path] <- readIORef currentGraph
    (name, _, _, _)<- listStoreGetValue store path
    listStoreSetValue store path (name, currentES, undo, redo)
    saveFileAs store saveProject fileName window True

  opg `on`actionActivated $ do
    mg <- loadFile window loadGraph
    case mg of
      Just ((g,gi),_) -> do
        writeIORef st (g,gi,([],[]),1.0,(0.0,0.0))
        widgetQueueDraw canvas
      _      -> return ()

  svg `on` actionActivated $ do
    es <- readIORef st
    let g  = editorGetGraph es
        gi = editorGetGI es
    saveFileAs (g,gi) saveGraph' fileName window False

  udo `on` actionActivated $ do
    applyUndo undoStack redoStack st
    widgetQueueDraw canvas

  rdo `on` actionActivated $ do
    applyRedo undoStack redoStack st
    widgetQueueDraw canvas

  hlp `on` actionActivated $ do
    widgetShowAll helpWindow

    return ()


  -- tratamento de eventos -- menu de propriedades -----------------------------
  entryName `on` keyPressEvent $ do
    k <- eventKeyName
    liftIO $ do
      case T.unpack k of
        "Return" -> do
          es <- readIORef st
          stackUndo undoStack redoStack es
          name <- entryGetText entryName :: IO String
          context <- widgetGetPangoContext canvas
          renameSelected st name context
          widgetQueueDraw canvas
        _       -> return ()
    return False

  onColorSet colorBtn $ do
    Color r g b <- colorButtonGetColor colorBtn
    es <- readIORef st
    let col = ((fromIntegral r)/65535, (fromIntegral g)/65535, (fromIntegral b)/65535)
        graph = editorGetGraph es
        (ns,edgs) = editorGetSelected es
        (ngiM, egiM) = editorGetGI es
        changeColor = (\giMap (NodeId nid) -> let gi = nodeGiSetColor col $ getNodeGI nid ngiM
                                              in M.insert nid gi giMap)
        newngiM = foldl changeColor ngiM ns
    modifyIORef st (\es -> editorSetGI (newngiM, egiM) es)
    if null ns
      then writeIORef currentC col
      else return ()
    widgetQueueDraw canvas

  onColorSet lineColorBtn $ do
    Color r g b <- colorButtonGetColor lineColorBtn
    es <- readIORef st
    let color = ((fromIntegral r)/65535, (fromIntegral g)/65535, (fromIntegral b)/65535)
        graph = editorGetGraph es
        (ns,edgs) = editorGetSelected es
        (ngiM, egiM) = editorGetGI es
        changeNLC = (\giMap (NodeId nid) -> let gi = nodeGiSetLineColor color $ getNodeGI nid ngiM
                                            in M.insert nid gi giMap)
        newngiM = foldl changeNLC ngiM ns
        changeELC = (\giMap (EdgeId eid) -> let gi = edgeGiSetColor color $ getEdgeGI eid egiM
                                            in M.insert eid gi giMap)
        newegiM = foldl changeELC egiM edgs
    modifyIORef st (\es -> editorSetGI (newngiM, newegiM) es)
    if null ns && null edgs
      then writeIORef currentLC color
      else return ()
    widgetQueueDraw canvas

  radioCircle `on` toggled $ do
    modifyIORef st (\es -> changeNodeShape es NCircle)
    writeIORef currentShape NCircle
    widgetQueueDraw canvas

  radioRect `on` toggled $ do
    modifyIORef st (\es -> changeNodeShape es NRect)
    writeIORef currentShape NRect
    widgetQueueDraw canvas

  radioQuad `on` toggled $ do
    modifyIORef st (\es -> changeNodeShape es NQuad)
    writeIORef currentShape NQuad
    widgetQueueDraw canvas

  radioNormal `on` toggled $ do
    modifyIORef st (\es -> changeEdgeStyle es ENormal)
    writeIORef currentStyle ENormal
    widgetQueueDraw canvas

  radioPointed `on` toggled $ do
    modifyIORef st (\es -> changeEdgeStyle es EPointed)
    writeIORef currentStyle EPointed
    widgetQueueDraw canvas

  radioSlashed `on` toggled $ do
    modifyIORef st (\es -> changeEdgeStyle es ESlashed)
    writeIORef currentStyle ESlashed
    widgetQueueDraw canvas

  -- Tratamento de eventos - arvore de grafos ----------------------------------
  treeview `on` cursorChanged $ do
    selection <- treeViewGetSelection treeview
    sel <- treeSelectionGetSelected selection
    case sel of
      Nothing -> return ()
      Just it -> do
        [path] <- treeModelGetPath store it
        [currentPath] <- readIORef currentGraph
        if currentPath == path
          then return ()
          else do
            -- update the current graph in the tree
            currentES <- readIORef st
            u <- readIORef undoStack
            r <- readIORef redoStack
            (name, _, _, _) <- listStoreGetValue store currentPath
            listStoreSetValue store currentPath (name,currentES, u, r)
            -- load the selected graph from the tree
            (_,newEs, newU, newR) <- listStoreGetValue store path
            writeIORef st newEs
            writeIORef undoStack newU
            writeIORef redoStack newR
            writeIORef currentGraph [path]
            -- update canvas
            widgetQueueDraw canvas

  btnNew `on` buttonActivated $ do
    listStoreAppend store ("new",emptyES,[],[])
    return ()

  btnRmv `on` buttonActivated $ do
    selection <- treeViewGetSelection treeview
    sel <- treeSelectionGetSelected selection
    case sel of
      Nothing -> return ()
      Just it -> do
        size <- listStoreGetSize store
        [path] <- treeModelGetPath store it
        case (size>1, path==size-1) of
          (True, True) -> do
            treeViewSetCursor treeview [path-1] Nothing
            listStoreRemove store path
          (True, False) -> do
            listStoreRemove store path
            treeViewSetCursor treeview [path] Nothing
          (False, True) -> do
            listStoreSetValue store 0 ("new",emptyES,[],[])
            writeIORef st emptyES
          _ -> return ()

        widgetQueueDraw canvas

  treeRenderer `on` edited $ \[path] newName -> do
    (oldName, val, u, r) <- listStoreGetValue store path
    listStoreSetValue store path (newName, val, u, r)

  -- tratamento de eventos - janela principal ----------------------------------
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  mainGUI



-- Callbacks -------------------------------------------------------------------
-- atualização do menu de propriedades -----------------------------------------
updatePropMenu :: IORef EditorState -> IORef (Double,Double,Double) -> IORef (Double,Double,Double) -> (Entry, Entry, ColorButton, ColorButton, [RadioButton], [RadioButton]) -> (HBox, Frame, Frame)-> IO ()
updatePropMenu st currentC currentLC (entryID, entryName, colorBtn, lcolorBtn, radioShapes, radioStyles) (hBoxColor, frameShape, frameStyle) = do
  est <- readIORef st
  let g = editorGetGraph est
      ns = filter (\n -> elem (nodeId n) $ fst $ editorGetSelected est) $ nodes g
      es = filter (\e -> elem (edgeId e) $ snd $ editorGetSelected est) $ edges g
      (ngiM,egiM) = editorGetGI est
      unifyNames (x:xs) = if all (==x) xs then x else "----"
  case (length ns, length es) of
    (0,0) -> do
      (r, g, b)    <- readIORef currentC
      (r', g', b') <- readIORef currentLC
      entrySetText entryID ""
      entrySetText entryName ""
      colorButtonSetColor colorBtn $ Color (round (r*65535)) (round (g*65535)) (round (b*65535))
      colorButtonSetColor lcolorBtn $ Color (round (r'*65535)) (round (g'*65535)) (round (b'*65535))
      set hBoxColor [widgetVisible := True]
      set frameShape [widgetVisible := True]
      set frameStyle [widgetVisible := True]
    (n,0) -> do
      let nid = nodeId (ns!!0)
          name = if n == 1 then nodeInfo $ (ns!!0) else unifyNames (map nodeInfo ns)
          gi = getNodeGI (fromEnum nid) ngiM
          (r,g,b) = fillColor gi
          (r',g',b') = lineColor gi
          nodeColor = Color (round (r*65535)) (round (g*65535)) (round (b*65535))
          nodeLineC = Color (round (r'*65535)) (round (g'*65535)) (round (b'*65535))
          nodeShape = shape gi
      entrySetText entryID $ if n==1 then (show nid) else "----"
      entrySetText entryName name
      colorButtonSetColor colorBtn $ if n==1 then nodeColor else Color 49151 49151 49151
      colorButtonSetColor lcolorBtn $ if n==1 then nodeLineC else Color 49151 49151 49151
      case (n,nodeShape) of
        (1,NCircle) -> toggleButtonSetActive (radioShapes!!0) True
        (1,NRect) -> toggleButtonSetActive (radioShapes!!1) True
        (1,NQuad) -> toggleButtonSetActive (radioShapes!!2) True
        _ -> return ()

      set hBoxColor [widgetVisible := True]
      set frameShape [widgetVisible := True]
      set frameStyle [widgetVisible := False]
    (0,n) -> do
      let eid = edgeId (es!!0)
          name = if n == 1 then edgeInfo (es!!0) else unifyNames (map edgeInfo es)
          gi = getEdgeGI (fromEnum eid) egiM
          (r,g,b) = color gi
          edgeColor = Color (round (r*65535)) (round (g*65535)) (round (b*65535))
          edgeStyle = style gi
      entrySetText entryID $ if n == 1 then (show eid) else "----"
      entrySetText entryName name
      colorButtonSetColor lcolorBtn $ if n == 1 then edgeColor else Color 49151 49151 49151
      case (n,edgeStyle) of
        (1,ENormal) -> toggleButtonSetActive (radioStyles!!0) True
        (1,EPointed) -> toggleButtonSetActive (radioStyles!!1) True
        (1,ESlashed) -> toggleButtonSetActive (radioStyles!!2) True
        _ -> return ()

      set hBoxColor [widgetVisible := False]
      set frameShape [widgetVisible := False]
      set frameStyle [widgetVisible := True]
    _ -> do
      entrySetText entryID "--"
      entrySetText entryName "----"
      colorButtonSetColor colorBtn $ Color 49151 49151 49151
      colorButtonSetColor lcolorBtn $ Color 49151 49151 49151
      set hBoxColor [widgetVisible := True]
      set frameShape [widgetVisible := True]
      set frameStyle [widgetVisible := True]

-- save function ---------------------------------------------------------------
saveFile :: a -> (a -> String -> IO Bool) -> IORef (Maybe String) -> Window -> Bool -> IO ()
saveFile x saveF fileName window changeFN = do
  fn <- readIORef fileName
  case fn of
    Just path -> do
      tentativa <- saveF x path
      case tentativa of
        True -> return ()
        False -> showError (Just window) "Não foi possivel salvar o arquivo"
    Nothing -> saveFileAs x saveF fileName window changeFN


saveFileAs :: a -> (a -> String -> IO Bool) -> IORef (Maybe String) -> Window -> Bool -> IO ()
saveFileAs x saveF fileName window changeFN = do
  saveD <- createSaveDialog window
  response <- dialogRun saveD
  fn <- case response of
    ResponseAccept -> do
      filename <- fileChooserGetFilename saveD
      case filename of
        Nothing -> do
          widgetDestroy saveD
          return Nothing
        Just path -> do
          tentativa <- saveF x path
          case tentativa of
            True -> do
              widgetDestroy saveD
              return $ Just path
            False -> do
              widgetDestroy saveD
              showError (Just window) "Não foi possível escrever no arquivo"
              return Nothing
    _  -> do
      widgetDestroy saveD
      return Nothing
  case (changeFN, fn) of
    (True, Just path) -> writeIORef fileName (Just path)
    _ -> return ()



-- salvar projeto --------------------------------------------------------------
saveProject :: ListStore (String, EditorState, [(Graph String String ,GraphicalInfo)], [(Graph String String ,GraphicalInfo)]) -> String -> IO Bool
saveProject model path = do
  editorList <- listStoreToList model
  let getWhatMatters = (\(name, es, _, _) -> (name, editorGetGraph es, editorGetGI es))
      whatMatters = map getWhatMatters editorList
      contents = map (\(name, g, gi) -> ( name
                                        , map (\n -> (nodeId n, nodeInfo n) ) $ nodes g
                                        , map (\e -> (edgeId e, sourceId e, targetId e, edgeInfo e)) $ edges g
                                        , gi )) whatMatters
      writeProject = writeFile path $ show contents
  tentativa <- E.try (writeProject)  :: IO (Either E.IOException ())
  case tentativa of
    Left _ -> return False
    Right _ -> return True

-- load function ---------------------------------------------------------------
loadFile :: Window -> (String -> a) -> IO (Maybe (a,String))
loadFile window loadF = do
  loadD <- fileChooserDialogNew
           (Just "Abrir Arquivo")
           (Just window)
           FileChooserActionOpen
           [("Cancela", ResponseCancel), ("Abre",ResponseAccept)]
  fileChooserSetDoOverwriteConfirmation loadD True
  widgetShow loadD
  response <- dialogRun loadD
  case response of
    ResponseAccept -> do
      filename <- fileChooserGetFilename loadD
      widgetDestroy loadD
      case filename of
        Nothing -> do
          return Nothing
        Just path -> do
          tentativa <- E.try (readFile path) :: IO (Either E.IOException String)
          case tentativa of
            Left _ -> do
              showError (Just window) "Não foi possivel ler o arquivo"
              return Nothing
            Right content -> return $ Just (loadF content, path)
    _             -> do
      widgetDestroy loadD
      return Nothing


-- carregar projeto ------------------------------------------------------------
loadProject :: String -> [(String, EditorState)]
loadProject content = editorList
  where
    contentList = read content :: [(String, [(Int, String)], [(Int,Int,Int,String)], GraphicalInfo)]
    genNodes = map (\(nid, info) -> Node (NodeId nid) info)
    genEdges = map (\(eid, src, dst, info) -> Edge (EdgeId eid) (NodeId src) (NodeId dst) info)
    editorList = map (\(name,readNodes,readEdges,gi) -> let nds = genNodes readNodes
                                                            eds = genEdges readEdges
                                                            g = fromNodesAndEdges nds eds
                                                        in (name, editorSetGI gi . editorSetGraph g $ emptyES) ) contentList


-- salvar grafo ----------------------------------------------------------------


saveGraph' :: (Graph String String ,GraphicalInfo) -> String -> IO Bool
saveGraph' (g,gi) path = do
    let writeGraph = writeFile path $ show ( map (\n -> (nodeId n, nodeInfo n) ) $ nodes g
                                           , map (\e -> (edgeId e, sourceId e, targetId e, edgeInfo e)) $ edges g
                                           , gi)
    tentativa <- E.try (writeGraph)  :: IO (Either E.IOException ())
    case tentativa of
      Left _ -> return False
      Right _ -> return True

-- abrir grafo -----------------------------------------------------------------
loadGraph :: String -> (Graph String String,GraphicalInfo)
loadGraph contents = (g,gi)
  where
    (rns,res,gi) = read contents :: ([(Int, String)], [(Int,Int,Int,String)], GraphicalInfo)
    ns = map (\(nid, info) -> Node (NodeId nid) info) rns
    es = map (\(eid, src, dst, info) -> Edge (EdgeId eid) (NodeId src) (NodeId dst) info) res
    g = fromNodesAndEdges ns es

-- desenhar grafo no canvas ----------------------------------------------------
drawGraph :: EditorState -> Maybe (Double,Double,Double,Double) -> DrawingArea -> Render ()
drawGraph (g, (nGI,eGI), (sNodes, sEdges), z, (px,py)) sq canvas = do
  context <- liftIO $ widgetGetPangoContext canvas
  scale z z
  translate px py

  -- desenha as arestas
  forM (edges g) (\e -> do
    let dstN = M.lookup (fromEnum . targetId $ e) nGI
        srcN = M.lookup (fromEnum . sourceId $ e) nGI
        egi  = M.lookup (fromEnum . edgeId   $ e) eGI
        selected = (edgeId e) `elem` sEdges
    case (egi, srcN, dstN) of
      (Just gi, Just src, Just dst) -> renderEdge gi (edgeInfo e) selected src dst context
      _ -> return ())

  -- desenha os nodos
  forM (nodes g) (\n -> do
    let ngi = M.lookup (fromEnum . nodeId $ n) nGI
        selected = (nodeId n) `elem` (sNodes)
        info = nodeInfo n
    case (ngi) of
      Just gi -> renderNode gi info selected context
      Nothing -> return ())

  -- desenha a selectionBox
  case sq of
    Just (x,y,w,h) -> do
      rectangle x y w h
      setSourceRGBA 0 0 1 0.5
      fill
      rectangle x y w h
      setSourceRGBA 0 0 1 1
      stroke
    Nothing -> return ()
  return ()


-- operações de interação ------------------------------------------------------
-- verifica se o usuario selecionou algum nodo
checkSelectNode:: GraphicalInfo -> (Double,Double) -> Maybe NodeId
checkSelectNode (nodesG,_) (x,y) = case find (\n -> isSelected (snd n)) $ (M.toList nodesG) of
                                    Nothing -> Nothing
                                    Just (k,a) -> Just $ NodeId k
  where isSelected = (\n -> let (nx,ny) = position  n
                                (w,h) = dims n
                                l = max w h
                            in case shape n of
                              NCircle -> pointDistance (x,y) (nx,ny) < l/2
                              NRect -> pointInsideRectangle (x,y) (nx,ny,w,h)
                              NQuad -> pointInsideRectangle (x,y) (nx,ny,l,l) )

-- verifica se o usuario selecionou alguma aresta
checkSelectEdge:: GraphicalInfo -> (Double,Double) -> Maybe EdgeId
checkSelectEdge (_,edgesG) (x,y) = case find (\e -> isSelected (snd e)) $ (M.toList edgesG) of
                            Nothing -> Nothing
                            Just (k,a) -> Just $ EdgeId k
  where isSelected = (\e -> pointDistance (x,y) (cPosition e) < 5)

-- move os nodos selecionados
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

-- move as arestas selecionadas
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

-- ajusta a posição das edges selecionadas caso a propriedade centered seja True
adjustEdges:: EditorState -> EditorState
adjustEdges es = editorSetGI (ngiM,newEgiM) es
  where graph = editorGetGraph es
        (ngiM,egiM) = editorGetGI es
        adjust = (\giM eid -> case lookupEdge eid graph of
          Just edge ->  let
                          srcPos = position $ getNodeGI (fromEnum $ sourceId edge) ngiM
                          dstPos = position $ getNodeGI (fromEnum $ targetId edge) ngiM
                          gi = getEdgeGI (fromEnum eid) egiM
                        in if centered gi
                          then M.insert (fromEnum eid) (edgeGiSetPosition (midPoint srcPos dstPos) gi) giM
                          else giM)
        newEgiM = foldl adjust egiM (snd . editorGetSelected $ es)



-- create operations -----------------------------------------------------------
-- create a new node with it's default GraphicalInfo
createNode:: IORef EditorState -> (Double,Double) -> PangoContext -> NodeShape -> (Double,Double,Double) -> (Double,Double,Double) -> IO ()
createNode st pos context nshape color lColor = do
  es <- readIORef st
  let graph = editorGetGraph es
      nid = head $ newNodes graph
      content = "node " ++ show nid
      newGraph = insertNodeWithPayload nid content graph
  dim <- getStringDims content context
  let newNgi = NodeGI {position = pos, fillColor = color, lineColor = lColor, dims = dim, shape = nshape}
      newGIM = (M.insert (fromEnum nid) newNgi $ fst (editorGetGI es), snd (editorGetGI es))
  writeIORef st $ editorSetGI newGIM . editorSetGraph newGraph . editorSetSelected ([nid], []) $ es

-- create edges between the selected nodes and a target node
createEdges:: EditorState -> NodeId -> EdgeStyle -> (Double,Double,Double) -> EditorState
createEdges es dstNode estyle ecolor = editorSetGraph newGraph . editorSetGI (ngiM, newegiM) . editorSetSelected ([dstNode],[]) $ es
  where selectedNodes = fst $ editorGetSelected es
        graph = editorGetGraph es
        (ngiM,egiM) = editorGetGI es
        (newGraph, newegiM) = foldl create (graph, egiM) selectedNodes
        create = (\(g,giM) nid -> let
                                  eid = head $ newEdges g
                                  ng = insertEdgeWithPayload eid nid dstNode "" g
                                  getPos = (\n -> position . getNodeGI (fromEnum n) $ ngiM)
                                  (srcPos,dstPos) = applyPair getPos (nid,dstNode)
                                  negi = if (dstNode == nid)
                                          then EdgeGI {cPosition = (fst srcPos, snd srcPos - 50), color = ecolor, centered = False, style = estyle}
                                          else EdgeGI {cPosition = (midPoint srcPos dstPos), color = ecolor, centered = True, style = estyle}
                                in (ng, M.insert (fromEnum eid) negi giM))




-- deleta os nodos e arestas selecionados no grafo
deleteSelected:: EditorState -> EditorState
deleteSelected es = editorSetSelected ([],[]) . editorSetGI (newngiM, newegiM) . editorSetGraph newGraph $ es
  where graph = editorGetGraph es
        (nids,eids) = editorGetSelected es
        (ngiM, egiM) = editorGetGI es
        newngiM = foldl (\giM n -> M.delete n giM) ngiM (map fromEnum nids)
        newegiM = foldl (\giM n -> M.delete n giM) egiM (map fromEnum eids)
        graph' = foldl (\g n -> removeNode n g) graph nids
        newGraph = foldl (\g e -> removeEdge e g) graph' eids

-- renomeia os itens selecionados
renameSelected:: IORef EditorState -> String -> PangoContext -> IO()
renameSelected state name context = do
  es <- readIORef state
  dim <- getStringDims name context
  let graph = editorGetGraph es
      (nids,eids) = editorGetSelected es
      (ngiM,egiM) = editorGetGI es
  let graph' = foldl (\g nid -> updateNodePayload nid g (\_ -> name)) graph nids
      newGraph  = foldl (\g eid -> updateEdgePayload eid g (\_ -> name)) graph' eids
      newNgiM = M.mapWithKey (\k gi -> if NodeId k `elem` nids then nodeGiSetDims dim gi else gi) ngiM
      newEs   = editorSetGI (newNgiM,egiM) . editorSetGraph newGraph $ es
  writeIORef state newEs

-- muda a forma de um nodo
changeNodeShape :: EditorState -> NodeShape -> EditorState
changeNodeShape es s = editorSetGI (newNgiM, egiM) es
  where
      nids = fst $ editorGetSelected es
      (ngiM, egiM) = editorGetGI es
      newNgiM = M.mapWithKey (\k gi -> if NodeId k `elem` nids then nodeGiSetShape s gi else gi) ngiM

-- muda o estilo das edges selecionadas
changeEdgeStyle :: EditorState -> EdgeStyle -> EditorState
changeEdgeStyle es s = editorSetGI (ngiM, newEgiM) es
  where
    eids = snd $ editorGetSelected es
    (ngiM, egiM) = editorGetGI es
    newEgiM = M.mapWithKey (\k gi -> if EdgeId k `elem` eids then edgeGiSetStyle s gi else gi) egiM


-- função auxiliar para createNode e renameSelected
-- dado um texto, adquire o tamanho da bounding box do texto para renderiza-lo
-- utiliza a biblioteca pango para isso
getStringDims :: String -> PangoContext -> IO (Double, Double)
getStringDims str context = do
  pL <- layoutText context str
  (_, PangoRectangle _ _ w h) <- layoutGetExtents pL
  return (w+4, h+4)

-- Undo / Redo -----------------------------------------------------------------
stackUndo :: IORef [(Graph String String, GraphicalInfo)] -> IORef [(Graph String String, GraphicalInfo)] -> EditorState -> IO ()
stackUndo undo redo es = do
  let g = editorGetGraph es
      gi = editorGetGI es
  modifyIORef undo (\u -> (g,gi):u )
  modifyIORef redo (\_ -> [])

applyUndo :: IORef [(Graph String String, GraphicalInfo)] -> IORef [(Graph String String, GraphicalInfo)] -> IORef EditorState -> IO ()
applyUndo undoStack redoStack st = do
  es <- readIORef st
  undo <- readIORef undoStack
  redo <- readIORef redoStack
  let apply [] r es = ([],r, es)
      apply ((g,gi):u) r es = (u, (eg,egi):r, editorSetGI gi . editorSetGraph g $ es)
                            where
                              eg = editorGetGraph es
                              egi = editorGetGI es
      (nu, nr, nes) = apply undo redo es
  writeIORef undoStack nu
  writeIORef redoStack nr
  writeIORef st nes

applyRedo :: IORef [(Graph String String, GraphicalInfo)] -> IORef [(Graph String String, GraphicalInfo)] -> IORef EditorState -> IO ()
applyRedo undoStack redoStack st = do
  undo <- readIORef undoStack
  redo <- readIORef redoStack
  es <- readIORef st
  let apply u [] es = (u, [], es)
      apply u ((g,gi):r) es = ((eg,egi):u, r, editorSetGI gi . editorSetGraph g $ es)
                            where
                              eg = editorGetGraph es
                              egi = editorGetGI es
      (nu, nr, nes) = apply undo redo es
  writeIORef undoStack nu
  writeIORef redoStack nr
  writeIORef st nes

-- Copy / Paste / Cut ----------------------------------------------------------
copySelected :: EditorState -> (Graph String String, GraphicalInfo)
copySelected  es = (cg,(ngiM',egiM'))
  where
    (nids,eids) = editorGetSelected es
    g = editorGetGraph es
    (ngiM, egiM) = editorGetGI es
    cnodes = filter (\n -> nodeId n `elem` nids) $ nodes g
    cedges = filter (\e -> edgeId e `elem` eids) $ edges g
    cg = fromNodesAndEdges cnodes cedges
    ngiM' = M.filterWithKey (\k _ -> NodeId k `elem` nids) ngiM
    egiM' = M.filterWithKey (\k _ -> EdgeId k `elem` eids) egiM

pasteClipBoard :: (Graph String String, GraphicalInfo) -> EditorState -> EditorState
pasteClipBoard (cGraph, (cNgiM, cEgiM)) es = editorSetGI (newngiM,newegiM) . editorSetGraph newGraph . editorSetSelected ([], [])$ es
  where
    graph = editorGetGraph es
    (ngiM, egiM) = editorGetGI es
    minX = minimum $ map (fst . position) (M.elems cNgiM)
    minY = minimum $ map (snd . position) (M.elems cNgiM)
    upd (a,b) = (20+a-minX, 20+b-minY)
    cNgiM' = M.map (\gi -> nodeGiSetPosition (upd $ position gi) gi) cNgiM
    cEgiM' = M.map (\gi -> edgeGiSetPosition (upd $ cPosition gi) gi) cEgiM
    (newGraph, (newngiM,newegiM)) = diagrUnion (graph,(ngiM,egiM)) (cGraph,(cNgiM', cEgiM'))

diagrUnion :: (Graph String String, GraphicalInfo) -> (Graph String String, GraphicalInfo) -> (Graph String String, GraphicalInfo)
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





-- Tarefas ---------------------------------------------------------------------
-- *Espaçar edges quando houver mais de uma aresta entre dois nodos e ela estiver centralizada
-- *TypeGraph

-- Progresso -------------------------------------------------------------------
-- *Criar uma janela de ajuda



-- Feito -----------------------------------------------------------------------
-- *Melhorar menu de Propriedades
--  *3 aparencias diferentes para nodos, edges e nodos+edges
-- *Corrigir Zoom para ajustar o Pan quando ele for modificado
-- *Copy/Paste/Cut
-- *Corrigir arestas não sendo coladas com Cut/Paste
-- *Corrigir movimento das arestas quando mover um nodo
-- *corrigir bug no copiar/colar que ocorre quando a seleção é movida antes de copiar
-- *Novo Arquivo
-- *Separar a estrutura do grafo das estruturas gráficas
-- *Estilos diferentes para as arestas
-- *Criar uma janela de mensagens de erros para substituir prints
-- *Mudar para que quando o usuario clique em um nodo, ele não invalide toda a seleção se o nodo for parte da seleção
-- *Fazer com que duplo-clique em um nodo ou aresta ou pressionando F2 com nodos/arestas selecionados, o dialogo nome seja focado
-- *Mudar estrutura do grafo para estrutura usada no verigraph
-- *Editar multiplos grafos no mesmo projeto
--   *Criar uma arvore de grafos
--   *Consertar Undo/Redo
