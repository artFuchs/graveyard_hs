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
import Data.Maybe
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

-- constructor
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


type DiaGraph = (Graph String String ,GraphicalInfo)
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
  (maybeMenubar,new,opn,svn,sva,opg,svg,udo,rdo,cpy,pst,cut,sla,sle,sln,hlp) <- buildMaybeMenubar
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
  st              <- newIORef emptyES -- editor state: all the necessary info to draw the graph
  oldPoint        <- newIORef (0.0,0.0) -- last point where a mouse button was pressed
  squareSelection <- newIORef Nothing -- selection box : Maybe (x,y,w,h)
  undoStack       <- newIORef ([] :: [(Graph String String, GraphicalInfo)])
  redoStack       <- newIORef ([] :: [(Graph String String, GraphicalInfo)])
  movingGI        <- newIORef False -- if the user started moving some object - necessary to add a position to the undoStack
  currentShape    <- newIORef NCircle -- the shape that all new nodes must have
  currentStyle    <- newIORef ENormal -- the style that all new edges must have
  currentC        <- newIORef (1,1,1) -- the color to init new nodes
  currentLC       <- newIORef (0,0,0) -- the color to init new edges and the line and text of new nodes
  clipboard       <- newIORef (empty, (M.empty, M.empty)) -- clipboard - DiaGraph
  fileName        <- newIORef (Nothing :: Maybe String) -- name of the opened file
  currentGraph    <- newIORef [0] -- current graph being edited
  changedProject  <- newIORef False -- set this flag as True when the graph is changed somehow
  changedGraph    <- newIORef [False] -- when modify a graph, set the flag in the path specified by 'currentGraph' to True
  lastSavedState  <- newIORef ([(empty, (M.empty, M.empty))] :: [DiaGraph])

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
      -- double click with left button : rename selection
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
        widgetQueueDraw canvas
        updatePropMenu st currentC currentLC propWidgets propBoxes
      -- right button click: create nodes and insert edges
      (RightButton, _) -> liftIO $ do
        let g = editorGetGraph es
            gi = editorGetGI es
            dstNode = checkSelectNode gi (x',y')
        context <- widgetGetPangoContext canvas
        stackUndo undoStack redoStack es
        case (Control `elem` ms, dstNode) of
          -- no selected node: create node
          (False, Nothing) -> do
            shape <- readIORef currentShape
            c <- readIORef currentC
            lc <- readIORef currentLC
            createNode st (x',y') context shape c lc
            setChangeFlags window changedProject changedGraph currentGraph True
          -- one node selected: create edges targeting this node
          (False, Just nid) -> do
            estyle <- readIORef currentStyle
            color <- readIORef currentLC
            modifyIORef st (\es -> createEdges es nid estyle color)
            setChangeFlags window changedProject changedGraph currentGraph True
          -- ctrl pressed: middle mouse button emulation
          (True,_) -> return ()
        widgetQueueDraw canvas
        updatePropMenu st currentC currentLC propWidgets propBoxes
      _           -> return ()

    return True

  -- mouse motion on canvas
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
      -- if left button is pressed and no node is selected, update square selection
      (True, False, [], []) -> liftIO $ do
        modifyIORef squareSelection $ liftM $ (\(a,b,c,d) -> (a,b,x'-a,y'-b))
        sq <- readIORef squareSelection
        widgetQueueDraw canvas
      -- if left button is pressed with some elements selected, then move them
      (True, False, n, e) -> liftIO $ do
        modifyIORef st (\es -> moveNodes es (ox,oy) (x',y'))
        modifyIORef st (\es -> moveEdges es (ox,oy) (x',y'))
        writeIORef oldPoint (x',y')
        setChangeFlags window changedProject changedGraph currentGraph True
        mv <- readIORef movingGI
        if not mv
          then do
            writeIORef movingGI True
            stackUndo undoStack redoStack es
          else return ()
        widgetQueueDraw canvas
      -- if middle button is pressed, then move the view
      (False ,True, _, _) -> liftIO $ do
        let (dx,dy) = (x'-ox,y'-oy)
        modifyIORef st (editorSetPan (px+dx, py+dy))
        widgetQueueDraw canvas
      (_,_,_,_) -> return ()
    return True

  -- mouse button release on canvas
  canvas `on` buttonReleaseEvent $ do
    b <- eventButton
    case b of
      LeftButton -> liftIO $ do
        writeIORef movingGI False
        es <- readIORef st
        sq <- readIORef squareSelection
        let (n,e) = editorGetSelected es
        case (editorGetSelected es,sq) of
          -- if release the left button when there's a square selection,
          -- select the elements that are inside the selection
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

  -- mouse wheel scroll on canvas
  canvas `on` scrollEvent $ do
    d <- eventScrollDirection
    ms <- eventModifierAll
    case (Control `elem` ms, d) of
      -- when control is pressed,
      -- if the direction is up, then zoom in
      (True, ScrollUp)  -> liftIO $ do
        modifyIORef st (\es -> editorSetZoom (editorGetZoom es * 1.1) es )
        widgetQueueDraw canvas
      -- if the direction is down, then zoom out
      (True, ScrollDown) -> liftIO $ do
        modifyIORef st (\es -> if (editorGetZoom es * 0.9) > 0.6 then editorSetZoom (editorGetZoom es * 0.9) es else es)
        widgetQueueDraw canvas
      _ -> return ()
    return True

  -- keyboard
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
          setChangeFlags window changedProject changedGraph currentGraph True
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
          writeIORef changedProject False
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
        (True, False, "z") -> actionActivate udo
        (True, False, "r") -> actionActivate rdo
        -- CTRL + C/V/X : copy/paste/cut
        (True, False, "c") -> actionActivate cpy
        (True, False, "v") -> actionActivate pst
        (True, False, "x") -> actionActivate cut
        -- F2 - rename selection
        (False,False,"f2") -> widgetGrabFocus entryName
        _       -> return ()

    return True

  -- event bindings for the menu toolbar ---------------------------------------
  -- new project action activated
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

  -- open project
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

  -- save project
  svn `on` actionActivated $ do
    currentES <- readIORef st
    undo <- readIORef undoStack
    redo <- readIORef redoStack
    [path] <- readIORef currentGraph
    (name, _, _, _)<- listStoreGetValue store path
    listStoreSetValue store path (name, currentES, undo, redo)
    saved <- saveFile store saveProject fileName window True
    if saved
      then do
        size <- listStoreGetSize store
        writeIORef changedProject False
        writeIORef changedGraph (take size (repeat False))
        indicateChanges window False
        updateSavedState lastSavedState store
      else return ()


  -- save project as
  sva `on` actionActivated $ do
    currentES <- readIORef st
    undo <- readIORef undoStack
    redo <- readIORef redoStack
    [path] <- readIORef currentGraph
    (name, _, _, _) <- listStoreGetValue store path
    listStoreSetValue store path (name, currentES, undo, redo)
    saved <- saveFileAs store saveProject fileName window True
    if saved
      then do
        size <- listStoreGetSize store
        writeIORef changedProject False
        writeIORef changedGraph (take size (repeat False))
        indicateChanges window False
        updateSavedState lastSavedState store
      else return ()

  -- open graph
  opg `on`actionActivated $ do
    mg <- loadFile window loadGraph
    case mg of
      Just ((g,gi),path) -> do
        let splitAtToken str tkn = splitAt (1 + (fromMaybe (-1) $ findIndex (==tkn) str)) str
            getLastPart str = let splited = (splitAtToken str '/') in if fst splited == "" then str else getLastPart (snd splited)
            getName str = if (tails str)!!(length str - 3) == ".gr" then take (length str - 3) str else str
        listStoreAppend store (getName . getLastPart $ path, editorSetGI gi . editorSetGraph g $ emptyES, [],[])
        size <- listStoreGetSize store
        treeViewSetCursor treeview [size-1] Nothing
        modifyIORef changedGraph (\xs -> xs ++ [True])
        writeIORef changedProject True
        indicateChanges window True
        widgetQueueDraw canvas
      _      -> return ()

  -- save graph
  svg `on` actionActivated $ do
    es <- readIORef st
    let g  = editorGetGraph es
        gi = editorGetGI es
    saveFileAs (g,gi) saveGraph fileName window False
    return ()

  -- undo
  udo `on` actionActivated $ do
    applyUndo undoStack redoStack st
    -- indicate changes
    sst <- readIORef lastSavedState
    [path] <- readIORef currentGraph
    es <- readIORef st
    let (g,gi) = (editorGetGraph es, editorGetGI es)
        x = if length sst > path then sst!!path else (empty,(M.empty,M.empty))
    setChangeFlags window changedProject changedGraph currentGraph $ not (isDiaGraphEqual (g,gi) x)
    widgetQueueDraw canvas

  -- redo
  rdo `on` actionActivated $ do
    applyRedo undoStack redoStack st
    -- indicate changes
    sst <- readIORef lastSavedState
    [path] <- readIORef currentGraph
    es <- readIORef st
    let (g,gi) = (editorGetGraph es, editorGetGI es)
        x = if length sst > path then sst!!path else (empty,(M.empty,M.empty))
    setChangeFlags window changedProject changedGraph currentGraph $ not (isDiaGraphEqual (g,gi) x)
    widgetQueueDraw canvas

  -- copy
  cpy `on` actionActivated $ do
    es <- readIORef st
    writeIORef clipboard $ copySelected es

  -- paste
  pst `on` actionActivated $ do
    es <- readIORef st
    clip <- readIORef clipboard
    stackUndo undoStack redoStack es
    setChangeFlags window changedProject changedGraph currentGraph True
    modifyIORef st (pasteClipBoard clip)
    widgetQueueDraw canvas

  -- cut
  cut `on` actionActivated $ do
    es <- readIORef st
    writeIORef clipboard $ copySelected es
    modifyIORef st (\es -> deleteSelected es)
    stackUndo undoStack redoStack es
    setChangeFlags window changedProject changedGraph currentGraph True
    widgetQueueDraw canvas

  -- select all
  sla `on` actionActivated $ do
    modifyIORef st (\es -> let g = editorGetGraph es
                           in editorSetSelected (nodeIds g, edgeIds g) es)
    widgetQueueDraw canvas

  -- select edges
  sle `on` actionActivated $ do
    es <- readIORef st
    let selected = editorGetSelected es
        g = editorGetGraph es
    case selected of
      ([],[]) -> writeIORef st $ editorSetSelected ([], edgeIds g) es
      ([], e) -> return ()
      (n,e) -> writeIORef st $ editorSetSelected ([],e) es
    widgetQueueDraw canvas

  -- select nodes
  sln `on` actionActivated $ do
    es <- readIORef st
    let selected = editorGetSelected es
        g = editorGetGraph es
    case selected of
      ([],[]) -> writeIORef st $ editorSetSelected (nodeIds g, []) es
      (n, []) -> return ()
      (n,e) -> writeIORef st $ editorSetSelected (n,[]) es
    widgetQueueDraw canvas

  -- help
  hlp `on` actionActivated $ do
    widgetShowAll helpWindow


  -- event bindings -- inspector panel -----------------------------------------
  -- pressed a key when editing the entryName
  entryName `on` keyPressEvent $ do
    k <- eventKeyName
    liftIO $ do
      -- if it's Return, then change the name of the selected elements
      case T.unpack k of
        "Return" -> do
          es <- readIORef st
          stackUndo undoStack redoStack es
          setChangeFlags window changedProject changedGraph currentGraph True
          name <- entryGetText entryName :: IO String
          context <- widgetGetPangoContext canvas
          renameSelected st name context
          widgetQueueDraw canvas
        _       -> return ()
    return False

  -- select a fill color or line color
  -- change the selection fill color or line color and
  -- set the current fill or line color as the selected color
  onColorSet colorBtn $ do
    Color r g b <- colorButtonGetColor colorBtn
    es <- readIORef st
    let color = ((fromIntegral r)/65535, (fromIntegral g)/65535, (fromIntegral b)/65535)
        (nds,edgs) = editorGetSelected es
    writeIORef currentC color
    if null nds
      then return ()
      else do
        let (ngiM, egiM) = editorGetGI es
            newngiM = M.mapWithKey (\k ngi -> if NodeId k `elem` nds then nodeGiSetColor color ngi else ngi) ngiM
        stackUndo undoStack redoStack es
        setChangeFlags window changedProject changedGraph currentGraph True
        modifyIORef st (\es -> editorSetGI (newngiM, egiM) es)
        widgetQueueDraw canvas

  onColorSet lineColorBtn $ do
    Color r g b <- colorButtonGetColor lineColorBtn
    es <- readIORef st
    let color = ((fromIntegral r)/65535, (fromIntegral g)/65535, (fromIntegral b)/65535)
        (nds,edgs) = editorGetSelected es
    writeIORef currentLC color
    if null nds && null edgs
      then return ()
      else do
        let (ngiM, egiM) = editorGetGI es
            newngiM = M.mapWithKey (\k ngi -> if NodeId k `elem` nds then nodeGiSetLineColor color ngi else ngi) ngiM
            newegiM = M.mapWithKey (\k egi -> if EdgeId k `elem` edgs then edgeGiSetColor color egi else egi) egiM
        stackUndo undoStack redoStack es
        setChangeFlags window changedProject changedGraph currentGraph True
        modifyIORef st (\es -> editorSetGI (newngiM, newegiM) es)
        widgetQueueDraw canvas

  -- toogle the radio buttons for node shapes
  -- change the shape of the selected nodes and set the current shape for new nodes
  radioCircle `on` toggled $ do
    writeIORef currentShape NCircle
    es <- readIORef st
    active <- toggleButtonGetActive radioCircle
    let nds = fst $ editorGetSelected es
        giM = fst $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> NodeId k `elem` nds && shape gi /= NCircle) giM)
      then return ()
      else do
        stackUndo undoStack redoStack es
        setChangeFlags window changedProject changedGraph currentGraph True
        modifyIORef st (\es -> changeNodeShape es NCircle)
        widgetQueueDraw canvas

  radioRect `on` toggled $ do
    writeIORef currentShape NRect
    es <- readIORef st
    active <- toggleButtonGetActive radioRect
    let nds = fst $ editorGetSelected es
        giM = fst $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> NodeId k `elem` nds && shape gi /= NRect) giM)
      then return ()
      else do
        stackUndo undoStack redoStack es
        setChangeFlags window changedProject changedGraph currentGraph True
        modifyIORef st (\es -> changeNodeShape es NRect)
        widgetQueueDraw canvas

  radioQuad `on` toggled $ do
    writeIORef currentShape NQuad
    es <- readIORef st
    active <- toggleButtonGetActive radioQuad
    let nds = fst $ editorGetSelected es
        giM = fst $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> NodeId k `elem` nds && shape gi /= NQuad) giM)
      then return ()
      else do
        stackUndo undoStack redoStack es
        setChangeFlags window changedProject changedGraph currentGraph True
        modifyIORef st (\es -> changeNodeShape es NQuad)
        widgetQueueDraw canvas

  -- toogle the radio buttons for edge styles
  -- change the style of the selected edges and set the current style for new edges
  radioNormal `on` toggled $ do
    writeIORef currentStyle ENormal
    es <- readIORef st
    active <- toggleButtonGetActive radioNormal
    let edgs = snd $ editorGetSelected es
        giM = snd $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> EdgeId k `elem` edgs && style gi /= ENormal) giM)
      then return ()
      else do
        stackUndo undoStack redoStack es
        setChangeFlags window changedProject changedGraph currentGraph True
        modifyIORef st (\es -> changeEdgeStyle es ENormal)
        widgetQueueDraw canvas

  radioPointed `on` toggled $ do
    writeIORef currentStyle EPointed
    es <- readIORef st
    active <- toggleButtonGetActive radioPointed
    let edgs = snd $ editorGetSelected es
        giM = snd $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> EdgeId k `elem` edgs && style gi /= EPointed) giM)
      then return ()
      else do
        stackUndo undoStack redoStack es
        setChangeFlags window changedProject changedGraph currentGraph True
        modifyIORef st (\es -> changeEdgeStyle es EPointed)
        widgetQueueDraw canvas

  radioSlashed `on` toggled $ do
    writeIORef currentStyle ESlashed
    es <- readIORef st
    active <- toggleButtonGetActive radioSlashed
    let edgs = snd $ editorGetSelected es
        giM = snd $ editorGetGI es
    if not active || M.null (M.filterWithKey (\k gi -> EdgeId k `elem` edgs && style gi /= ESlashed) giM)
      then return ()
      else do
        stackUndo undoStack redoStack es
        setChangeFlags window changedProject changedGraph currentGraph True
        modifyIORef st (\es -> changeEdgeStyle es ESlashed)
        widgetQueueDraw canvas

  -- event bindings for the graphs' tree ---------------------------------------
  -- changed the selected graph
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

  -- pressed the 'new' button
  btnNew `on` buttonActivated $ do
    listStoreAppend store ("new",emptyES,[],[])
    modifyIORef lastSavedState (\sst -> sst ++ [(empty, (M.empty, M.empty))])
    setChangeFlags window changedProject changedGraph currentGraph True
    return ()

  -- pressed the 'remove' button
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

  -- edited a graph name
  treeRenderer `on` edited $ \[path] newName -> do
    (oldName, val, u, r) <- listStoreGetValue store path
    listStoreSetValue store path (newName, val, u, r)

  -- remove menuItem "insert Emoji" cause it causes the program to crash
  treeRenderer `on` editingStarted $ \widget path -> do
    let entry = castToEntry widget
    entry `on` entryPopulatePopup $ \menu -> do
      items <- containerGetChildren menu
      containerRemove menu (items!!(length items -1))
      widgetShowAll menu
    return ()


  -- event bindings for the main window ----------------------------------------
  -- when click in the close button, the application must close
  window `on` deleteEvent $ do
    changes <- liftIO $ readIORef changedProject
    response <- liftIO $ if changes
      then createCloseDialog (Just window) "O projeto foi modificado, deseja salvar?"
      else return ResponseNo
    case response of
      ResponseNo -> do
        liftIO mainQuit
        return False
      ResponseYes -> liftIO $ do
        currentES <- readIORef st
        [path] <- readIORef currentGraph
        (name, _, _, _)<- listStoreGetValue store path
        listStoreSetValue store path (name, currentES, [], [])
        saved <- saveFile store saveProject fileName window True
        if saved
          then do
            mainQuit
            return False
          else return True
      ResponseCancel -> return True

  -- run the preogram ----------------------------------------------------------
  mainGUI


--------------------------------------------------------------------------------
-- Callbacks -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- update the inspector --------------------------------------------------------
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

-- general save function -------------------------------------------------------
saveFile :: a -> (a -> String -> IO Bool) -> IORef (Maybe String) -> Window -> Bool -> IO Bool
saveFile x saveF fileName window changeFN = do
  fn <- readIORef fileName
  case fn of
    Just path -> do
      tentativa <- saveF x path
      case tentativa of
        True -> return True
        False -> do
          showError (Just window) $ "Não foi possível escrever no arquivo " ++ path
          return False
    Nothing -> saveFileAs x saveF fileName window changeFN


saveFileAs :: a -> (a -> String -> IO Bool) -> IORef (Maybe String) -> Window -> Bool -> IO Bool
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
              showError (Just window) $ "Não foi possível escrever no arquivo " ++ path
              return Nothing
    _  -> do
      widgetDestroy saveD
      return Nothing
  case (changeFN, fn) of
    (True, Just path) -> do
      writeIORef fileName (Just path)
      return True
    _ -> return False

-- auxiliar save functions -----------------------------------------------------
-- save project
saveProject :: ListStore (String, EditorState, [DiaGraph], [DiaGraph]) -> String -> IO Bool
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

-- save graph
saveGraph :: (Graph String String ,GraphicalInfo) -> String -> IO Bool
saveGraph (g,gi) path = do
    let path' = if (tails path)!!(length path-3) == ".gr" then path else path ++ ".gr"
        writeGraph = writeFile path' $ show ( map (\n -> (nodeId n, nodeInfo n) ) $ nodes g
                                           , map (\e -> (edgeId e, sourceId e, targetId e, edgeInfo e)) $ edges g
                                           , gi)

    tentativa <- E.try (writeGraph)  :: IO (Either E.IOException ())
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
            Right content -> return $ Just (loadF content,path)
    _             -> do
      widgetDestroy loadD
      return Nothing

-- auxiliar load functions -----------------------------------------------------
-- load project
loadProject :: String -> [(String, EditorState)]
loadProject content = editorList
  where
    contentList = read content :: [(String, [(Int, String)], [(Int,Int,Int,String)], GraphicalInfo)]
    genNodes = map (\(nid, info) -> Node (NodeId nid) info)
    genEdges = map (\(eid, src, dst, info) -> Edge (EdgeId eid) (NodeId src) (NodeId dst) info)
    genProj = map (\(name,readNodes,readEdges,gi) ->
                      let nds = genNodes readNodes
                          eds = genEdges readEdges
                          g = fromNodesAndEdges nds eds
                      in (name, editorSetGI gi . editorSetGraph g $ emptyES) )
    editorList = genProj contentList


-- load graph
loadGraph :: String -> (Graph String String,GraphicalInfo)
loadGraph contents = (g,gi)
  where
    (rns,res,gi) = read contents :: ([(Int, String)], [(Int,Int,Int,String)], GraphicalInfo)
    ns = map (\(nid, info) -> Node (NodeId nid) info) rns
    es = map (\(eid, src, dst, info) -> Edge (EdgeId eid) (NodeId src) (NodeId dst) info) res
    g = fromNodesAndEdges ns es


-- draw a graph in the canvas --------------------------------------------------
drawGraph :: EditorState -> Maybe (Double,Double,Double,Double) -> DrawingArea -> Render ()
drawGraph (g, (nGI,eGI), (sNodes, sEdges), z, (px,py)) sq canvas = do
  context <- liftIO $ widgetGetPangoContext canvas
  scale z z
  translate px py

  -- draw the edges
  forM (edges g) (\e -> do
    let dstN = M.lookup (fromEnum . targetId $ e) nGI
        srcN = M.lookup (fromEnum . sourceId $ e) nGI
        egi  = M.lookup (fromEnum . edgeId   $ e) eGI
        selected = (edgeId e) `elem` sEdges
    case (egi, srcN, dstN) of
      (Just gi, Just src, Just dst) -> renderEdge gi (edgeInfo e) selected src dst context
      _ -> return ())

  -- draw the nodes
  forM (nodes g) (\n -> do
    let ngi = M.lookup (fromEnum . nodeId $ n) nGI
        selected = (nodeId n) `elem` (sNodes)
        info = nodeInfo n
    case (ngi) of
      Just gi -> renderNode gi info selected context
      Nothing -> return ())

  -- draw the selectionBox
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


-- interaction ------------------------------------------------------
-- check if the user selected a node
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

-- check if the user selected a edge
checkSelectEdge:: GraphicalInfo -> (Double,Double) -> Maybe EdgeId
checkSelectEdge (_,edgesG) (x,y) = case find (\e -> isSelected (snd e)) $ (M.toList edgesG) of
                            Nothing -> Nothing
                            Just (k,a) -> Just $ EdgeId k
  where isSelected = (\e -> pointDistance (x,y) (cPosition e) < 5)





-- create/delete operations ----------------------------------------------------
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


-- Graphical Info manipulation -------------------------------------------------
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
          Just edge ->  let
                          srcPos = position $ getNodeGI (fromEnum $ sourceId edge) ngiM
                          dstPos = position $ getNodeGI (fromEnum $ targetId edge) ngiM
                          gi = getEdgeGI (fromEnum eid) egiM
                        in if centered gi
                          then M.insert (fromEnum eid) (edgeGiSetPosition (midPoint srcPos dstPos) gi) giM
                          else giM)
        newEgiM = foldl adjust egiM (snd . editorGetSelected $ es)

-- rename the selected itens
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


-- auxiliar function used by createNode and renameSelected
-- given a text, compute the size of it's bounding box
-- uses the pango lib
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
copySelected :: EditorState -> DiaGraph
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

pasteClipBoard :: DiaGraph -> EditorState -> EditorState
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

-- change window name to indicate if the project was modified
indicateChanges :: Window -> Bool -> IO ()
indicateChanges window True = do
  title <- get window windowTitle
  if title!!0 == '*'
    then return ()
    else set window [windowTitle := '*':title]

indicateChanges window False = do
  i:title <- get window windowTitle
  if i == '*'
    then set window [windowTitle := title]
    else return ()

setChangeFlags :: Window -> IORef Bool -> IORef [Bool] -> IORef [Int] -> Bool -> IO ()
setChangeFlags window changedProject changedGraph currentPath True = do
  [path] <- readIORef currentPath
  modifyIORef changedGraph (\xs -> take path xs ++ [True] ++ drop (path+1) xs)
  writeIORef changedProject True
  indicateChanges window True

setChangeFlags window changedProject changedGraph currentPath False = do
  [path] <- readIORef currentPath
  cg <- readIORef changedGraph
  let cg' = take path cg ++ [False] ++ drop (path+1) cg
      projRestored = and cg'
  writeIORef changedGraph cg'
  writeIORef changedProject projRestored
  indicateChanges window projRestored

-- change updatedState
updateSavedState :: IORef [DiaGraph] -> ListStore (String, EditorState, [DiaGraph], [DiaGraph]) -> IO ()
updateSavedState sst store = do
  list <- listStoreToList store
  let newSavedState = map (\(_,es,_,_) -> (editorGetGraph es, editorGetGI es)) list
  writeIORef sst newSavedState

isDiaGraphEqual :: DiaGraph -> DiaGraph -> Bool
isDiaGraphEqual (g1,gi1) (g2,gi2) = nodesEq && edgesEq && nodesGiEq && edgesGiEq
  where
    nodesEq = sameLength (nodes g1) (nodes g2) && all (\(x,y) -> nodeId x == nodeId y && nodeInfo x == nodeInfo y) (zip (nodes g1) (nodes g2))
    edgesEq = sameLength (edges g1) (edges g2) && all (\(x,y) -> edgeId x == edgeId y && sourceId x == sourceId y && targetId x == targetId y && edgeInfo x == edgeInfo y) (zip (edges g1) (edges g2))
    nodesGiEq = sameLength (M.elems $ fst gi1) (M.elems $ fst gi2) && all (\(x,y) -> x == y) (zip (M.elems $ fst gi1) (M.elems $ fst gi2))
    edgesGiEq = sameLength (M.elems $ snd gi1) (M.elems $ snd gi2) && all (\(x,y) -> x == y) (zip (M.elems $ snd gi1) (M.elems $ snd gi2))
    sameLength l1 l2 = length l1 == length l2

-- Tarefas ---------------------------------------------------------------------
-- *TypeGraph

-- Progresso -------------------------------------------------------------------
-- *Criar uma janela de ajuda
-- *Indicar em qual grafo está a mudança do projeto
-- *Usar

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
-- *Espaçar edges quando houver mais de uma aresta entre dois nodos e ela estiver centralizada
-- *Removida a opção "Insert Emoji" do menu da treeView, porque a ativação estava fazendo o programa encerrar.
