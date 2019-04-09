module Editor.GraphEditor
( startGUI
)where

import Graphics.UI.Gtk hiding (rectangle)
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Layout
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Control.Exception as E
import qualified Data.Map as M
import Data.Graphs hiding (null, empty)
import qualified Data.Graphs as G
import Editor.GraphicalInfo
import Editor.Render
import Editor.Helper
import Editor.UIBuilders
import Editor.DiaGraph hiding (empty)
import qualified Editor.DiaGraph as DG
import Editor.EditorState
--------------------------------------------------------------------------------
-- MODULE STRUCTURES -----------------------------------------------------------
--------------------------------------------------------------------------------
-- |GraphStore
-- A tuple representing what is stored in each node of the tree in the treeview
-- It contains the informations: name, editor state, undo stack, redo stack and color to draw the cell
type GraphStore = (String, EditorState, [DiaGraph], [DiaGraph], String)
graphStoreName (n,es,u,r,col) = n
graphStoreEditor (n,es,u,r,col) = es
graphStoreUndo (n,es,u,r,col) = u
graphStoreRedo (n,es,u,r,col) = r
graphStoreColor (n,es,u,r,col) = col
--------------------------------------------------------------------------------
-- MODULE FUNCTIONS ------------------------------------------------------------
--------------------------------------------------------------------------------

-- startGUI
-- creates the Graphical User Interface using the UIBuilders module and do the bindings
startGUI :: IO()
startGUI = do
  -- init GTK
  initGUI

  ------------------------------------------------------------------------------
  -- GUI definition ------------------------------------------------------------
  -- help window ---------------------------------------------------------------
  helpWindow <- buildHelpWindow

  -- main window ---------------------------------------------------------------
  -- creates the menu bar
  (menubar,(new,opn,svn,sva,opg,svg),(udo,rdo,cpy,pst,cut,sla,sln,sle),(zin,zut,z50,zdf,z150,z200,vdf),hlp) <- buildMenubar
  -- creates the inspector panel on the right
  (frameProps, entryName, colorBtn, lineColorBtn, radioShapes, radioStyles, propBoxes) <- buildTypeMenu
  let
    propWidgets = (entryName, colorBtn, lineColorBtn, radioShapes, radioStyles)
    [radioCircle, radioRect, radioQuad] = radioShapes
    [radioNormal, radioPointed, radioSlashed] = radioStyles
  -- creates the tree panel on the left
  (treePanel, treeview, treeRenderer, btnNew, btnRmv) <- buildTreePanel
  -- creates the main window, containing the canvas and the built panels
  (window, canvas, _) <- buildMainWindow (Just menubar) frameProps treePanel
  -- shows the main window
  widgetShowAll window

  -- init an model to display in the tree panel --------------------------------
  store <- listStoreNew [("new", emptyES, [], [], "white")]
  projectCol <- treeViewGetColumn treeview 0
  case projectCol of
    Nothing -> return ()
    Just col -> do
      treeViewSetModel treeview (Just store)
      cellLayoutSetAttributes col treeRenderer store $ \(name,_,_,_,color) -> [cellText := name, cellTextBackground := color]
      treeViewSetCursor treeview [0] Nothing

  ------------------------------------------------------------------------------
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
  clipboard       <- newIORef DG.empty -- clipboard - DiaGraph
  fileName        <- newIORef (Nothing :: Maybe String) -- name of the opened file
  currentGraph    <- newIORef [0] -- current graph being edited
  changedProject  <- newIORef False -- set this flahttps://www.youtube.com/g as True when the graph is changed somehow
  changedGraph    <- newIORef [False] -- when modify a graph, set the flag in the 'currentGraph' to True
  lastSavedState  <- newIORef ([] :: [DiaGraph])

  ------------------------------------------------------------------------------
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
            sNode = case selectNodeInPosition gi (x',y') of
              Nothing -> []
              Just nid -> [nid]
            sEdge = case selectEdgeInPosition gi (x',y') of
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
            dstNode = selectNodeInPosition gi (x',y')
        context <- widgetGetPangoContext canvas
        stackUndo undoStack redoStack es
        case (Control `elem` ms, dstNode) of
          -- no selected node: create node
          (False, Nothing) -> do
            shape <- readIORef currentShape
            c <- readIORef currentC
            lc <- readIORef currentLC
            createNode' st (x',y') context shape c lc
            setChangeFlags window store changedProject changedGraph currentGraph True
          -- one node selected: create edges targeting this node
          (False, Just nid) -> do
            estyle <- readIORef currentStyle
            color <- readIORef currentLC
            modifyIORef st (\es -> createEdges es nid estyle color)
            setChangeFlags window store changedProject changedGraph currentGraph True
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
        -- in case of the editor being used in a notebook or with a mouse with just two buttons, ctrl + right button can be used instead of the middle button.
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
        setChangeFlags window store changedProject changedGraph currentGraph True
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
          _ -> return ()
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
      (True, ScrollUp)  -> liftIO $ menuItemEmitActivate zin
      -- if the direction is down, then zoom out
      (True, ScrollDown) -> liftIO $ menuItemEmitActivate zut
      _ -> return ()
    return True

  -- keyboard
  canvas `on` keyPressEvent $ do
    k <- eventKeyName
    ms <- eventModifierAll
    liftIO $ do
      context <- widgetGetPangoContext canvas
      case (Control `elem` ms, Shift `elem` ms, T.unpack $ T.toLower k) of
        -- <delete> : delete selection
        (False,False,"delete") -> do
          es <- readIORef st
          modifyIORef st (\es -> deleteSelected es)
          stackUndo undoStack redoStack es
          setChangeFlags window store changedProject changedGraph currentGraph True
          widgetQueueDraw canvas
        -- CTRL + [SHIFT] + A : [de]select all
        (True, True, "a") -> do
          modifyIORef st $ editorSetSelected ([],[])
          widgetQueueDraw canvas
        (True, False, "a") -> do
          modifyIORef st (\es -> let g = editorGetGraph es
                                 in editorSetSelected (nodeIds g, edgeIds g) es)
          widgetQueueDraw canvas
        -- F2 - rename selection
        (False,False,"f2") -> widgetGrabFocus entryName
        -- CTRL + C/V/X : copy/paste/cut
        (True, False, "c") -> menuItemEmitActivate cpy
        (True, False, "v") -> menuItemEmitActivate pst
        (True, False, "x") -> menuItemEmitActivate cut
        _       -> return ()
    return True

  window `on` keyPressEvent $ do
    k <- eventKeyName
    ms <- eventModifierAll
    liftIO $ do
      context <- widgetGetPangoContext canvas
      case (Control `elem` ms, Shift `elem` ms, T.unpack $ T.toLower k) of
        -- CTRL + <+>/<->/<=> : zoom controls
        (True,_,"plus") -> menuItemEmitActivate zin
        (True,_,"minus") -> menuItemEmitActivate zut
        (True,_,"equal") -> menuItemEmitActivate zdf
        -- CTRL + <0> : reset pan & zoom
        (True,_,"0") -> menuItemEmitActivate vdf

        -- CTRL + N : create a new graph in the treeView
        (True, False, "n") -> buttonClicked btnNew
        -- CTRL + W : remove a graph from the treeView
        (True, False, "w") -> buttonClicked btnRmv

        -- CTRL + SHIFT + N : create a new file
        (True, True, "n") -> do
          modifyIORef st (\es -> (G.empty, (M.empty, M.empty),([],[]),1.0,(0.0,0.0)))
          listStoreClear store
          listStoreAppend store ("new",emptyES,[], [], "white")
          writeIORef changedProject False
          writeIORef fileName Nothing
          writeIORef undoStack []
          writeIORef redoStack []
          set window [windowTitle := "Graph Editor"]
          widgetQueueDraw canvas
        -- CTRL + SHIFT + S : save file as
        (True, True, "s") -> menuItemEmitActivate sva
        -- CTRL + S : save file
        (True, False, "s") -> menuItemEmitActivate svn
        -- CTRL + O : open file
        (True, False, "o") -> menuItemEmitActivate opn
        -- CTRL + Z/R : undo/redo
        (True, False, "z") -> menuItemEmitActivate udo
        (True, False, "r") -> menuItemEmitActivate rdo
        _ -> return ()
    return False

  -- event bindings for the menu toolbar ---------------------------------------

  -- auxiliar functions to create/open/save the project
  -- auxiliar function to prepare the treeStore to save
  let prepToSave = do currentES <- readIORef st
                      undo <- readIORef undoStack
                      redo <- readIORef redoStack
                      [path] <- readIORef currentGraph
                      gs <- listStoreGetValue store path
                      let (name, c) = (graphStoreName gs, graphStoreColor gs)
                      listStoreSetValue store path (name, currentES, undo, redo, c)

  -- auxiliar function to clean the flags after saving
  let afterSave = do  size <- listStoreGetSize store
                      writeIORef changedProject False
                      writeIORef changedGraph (take size (repeat False))
                      list <- listStoreToList store
                      writeIORef lastSavedState (map (\gs -> let es = graphStoreEditor gs in (editorGetGraph es, editorGetGI es)) list)
                      forM [0..(size-1)] $ \i -> let (n,e,u,r,_) = list!!i in listStoreSetValue store i (n,e,u,r,"white")
                      indicateProjChanged window False
                      updateSavedState lastSavedState store
                      filename <- readIORef fileName
                      case filename of
                        Nothing -> set window [windowTitle := "Graph Editor"]
                        Just fn -> set window [windowTitle := "Graph Editor - " ++ fn]


  -- auxiliar function to check if the project was changed
  -- it does the checking and if no, ask the user if them want to save.
  -- returns True if there's no changes, if the user don't wanted to save or if he wanted and the save operation was successfull
  -- returns False if the user wanted to save and the save operation failed or opted to cancel.
  let confirmOperation = do changed <- readIORef changedProject
                            response <- if changed
                              then createCloseDialog (Just window) "The project was changed, wants to save?"
                              else return ResponseNo
                            case response of
                              ResponseCancel -> return False
                              r -> case r of
                                ResponseNo -> return True
                                ResponseYes -> do
                                  prepToSave
                                  saveFile store saveProject fileName window True -- returns True if saved the file

  -- new project action activated
  new `on` menuItemActivated $ do
    continue <- confirmOperation
    if continue
      then do
        writeIORef fileName Nothing
        treeViewSetCursor treeview [0] Nothing
        listStoreClear store
        listStoreAppend store ("new", emptyES, [], [], "white")
        writeIORef st emptyES
        writeIORef undoStack []
        writeIORef redoStack []
        writeIORef lastSavedState []
        writeIORef changedProject False
        writeIORef changedGraph [False]
        set window [windowTitle := "Graph Editor"]
        widgetQueueDraw canvas
      else return ()

  -- open project
  opn `on` menuItemActivated $ do
    continue <- confirmOperation
    if continue
      then do
        mg <- loadFile window loadProject
        case mg of
          Just (list,fn) -> do
            if length list > 0
              then do
                listStoreClear store
                let plist = map (\(n,e) -> (n,e,[],[],"white")) list
                forM plist (listStoreAppend store)
                let (name,es) = list!!0
                writeIORef st es
                writeIORef fileName $ Just fn
                writeIORef undoStack []
                writeIORef redoStack []
                writeIORef changedProject False
                writeIORef changedGraph [False]
                set window [windowTitle := "Graph Editor - " ++ fn]
                widgetQueueDraw canvas
              else return ()
          Nothing -> return ()
        else return ()

  -- save project
  svn `on` menuItemActivated $ do
    prepToSave
    saved <- saveFile store saveProject fileName window True
    if saved
      then do afterSave
      else return ()

  -- save project as
  sva `on` menuItemActivated $ do
    prepToSave
    saved <- saveFileAs store saveProject fileName window True
    if saved
      then afterSave
      else return ()

  -- open graph
  opg `on`menuItemActivated $ do
    mg <- loadFile window loadGraph
    case mg of
      Just ((g,gi),path) -> do
        let splitAtToken str tkn = splitAt (1 + (fromMaybe (-1) $ findIndex (==tkn) str)) str
            getLastPart str = let splited = (splitAtToken str '/') in if fst splited == "" then str else getLastPart (snd splited)
            getName str = if (tails str)!!(length str - 3) == ".gr" then take (length str - 3) str else str
        listStoreAppend store (getName . getLastPart $ path, editorSetGI gi . editorSetGraph g $ emptyES, [],[],"green")
        size <- listStoreGetSize store
        treeViewSetCursor treeview [size-1] Nothing
        modifyIORef changedGraph (\xs -> xs ++ [True])
        writeIORef changedProject True
        indicateProjChanged window True
        widgetQueueDraw canvas
      _      -> return ()

  -- save graph
  svg `on` menuItemActivated $ do
    es <- readIORef st
    let g  = editorGetGraph es
        gi = editorGetGI es
    saveFileAs (g,gi) saveGraph fileName window False
    return ()

  -- undo
  udo `on` menuItemActivated $ do
    applyUndo undoStack redoStack st
    -- indicate changes
    sst <- readIORef lastSavedState
    [path] <- readIORef currentGraph
    es <- readIORef st
    let (g,gi) = (editorGetGraph es, editorGetGI es)
        x = if length sst > path then sst!!path else (G.empty,(M.empty,M.empty))
    setChangeFlags window store changedProject changedGraph currentGraph $ not (isDiaGraphEqual (g,gi) x)
    widgetQueueDraw canvas

  -- redo
  rdo `on` menuItemActivated $ do
    applyRedo undoStack redoStack st
    -- indicate changes
    sst <- readIORef lastSavedState
    [path] <- readIORef currentGraph
    es <- readIORef st
    let (g,gi) = (editorGetGraph es, editorGetGI es)
        x = if length sst > path then sst!!path else (G.empty,(M.empty,M.empty))
    setChangeFlags window store changedProject changedGraph currentGraph $ not (isDiaGraphEqual (g,gi) x)
    widgetQueueDraw canvas

  -- copy
  cpy `on` menuItemActivated $ do
    es <- readIORef st
    writeIORef clipboard $ copySelected es

  -- paste
  pst `on` menuItemActivated $ do
    es <- readIORef st
    clip <- readIORef clipboard
    stackUndo undoStack redoStack es
    setChangeFlags window store changedProject changedGraph currentGraph True
    modifyIORef st (pasteClipBoard clip)
    widgetQueueDraw canvas

  -- cut
  cut `on` menuItemActivated $ do
    es <- readIORef st
    writeIORef clipboard $ copySelected es
    modifyIORef st (\es -> deleteSelected es)
    stackUndo undoStack redoStack es
    setChangeFlags window store changedProject changedGraph currentGraph True
    widgetQueueDraw canvas

  -- select all
  sla `on` menuItemActivated $ do
    modifyIORef st (\es -> let g = editorGetGraph es
                           in editorSetSelected (nodeIds g, edgeIds g) es)
    widgetQueueDraw canvas

  -- select edges
  sle `on` menuItemActivated $ do
    es <- readIORef st
    let selected = editorGetSelected es
        g = editorGetGraph es
    case selected of
      ([],[]) -> writeIORef st $ editorSetSelected ([], edgeIds g) es
      ([], e) -> return ()
      (n,e) -> writeIORef st $ editorSetSelected ([],e) es
    widgetQueueDraw canvas

  -- select nodes
  sln `on` menuItemActivated $ do
    es <- readIORef st
    let selected = editorGetSelected es
        g = editorGetGraph es
    case selected of
      ([],[]) -> writeIORef st $ editorSetSelected (nodeIds g, []) es
      (n, []) -> return ()
      (n,e) -> writeIORef st $ editorSetSelected (n,[]) es
    widgetQueueDraw canvas

  -- zoom in
  zin `on` menuItemActivated $ do
    modifyIORef st (\es -> editorSetZoom (editorGetZoom es * 1.1) es )
    widgetQueueDraw canvas

  -- zoom out
  zut `on` menuItemActivated $ do
    modifyIORef st (\es -> let z = editorGetZoom es * 0.9 in if z >= 0.5 then editorSetZoom z es else es)
    widgetQueueDraw canvas

  z50 `on` menuItemActivated $ do
    modifyIORef st (\es -> editorSetZoom 0.5 es )
    widgetQueueDraw canvas

  -- reset zoom to defaults
  zdf `on` menuItemActivated $ do
    modifyIORef st (\es -> editorSetZoom 1.0 es )
    widgetQueueDraw canvas

  z150 `on` menuItemActivated $ do
    modifyIORef st (\es -> editorSetZoom 1.5 es )
    widgetQueueDraw canvas

  z200 `on` menuItemActivated $ do
    modifyIORef st (\es -> editorSetZoom 2.0 es )
    widgetQueueDraw canvas

  -- reset view to defaults (reset zoom and pan)
  vdf `on` menuItemActivated $ do
    modifyIORef st (\es -> editorSetZoom 1 $ editorSetPan (0,0) es )
    widgetQueueDraw canvas

  -- help
  hlp `on` menuItemActivated $ do
    widgetShowAll helpWindow

  -- event bindings -- inspector panel -----------------------------------------
  -- pressed a key when editing the entryName
  entryName `on` keyPressEvent $ do
    k <- eventKeyName
    let setName = liftIO $ do
          es <- readIORef st
          stackUndo undoStack redoStack es
          setChangeFlags window store changedProject changedGraph currentGraph True
          name <- entryGetText entryName :: IO String
          context <- widgetGetPangoContext canvas
          renameSelected st name context
          widgetQueueDraw canvas
    -- if it's Return, then change the name of the selected elements
    case T.unpack k of
      "Return" -> setName
      "KP_Enter" -> setName
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
        setChangeFlags window store changedProject changedGraph currentGraph True
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
        setChangeFlags window store changedProject changedGraph currentGraph True
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
        setChangeFlags window store changedProject changedGraph currentGraph True
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
        setChangeFlags window store changedProject changedGraph currentGraph True
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
        setChangeFlags window store changedProject changedGraph currentGraph True
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
        setChangeFlags window store changedProject changedGraph currentGraph True
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
        setChangeFlags window store changedProject changedGraph currentGraph True
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
        setChangeFlags window store changedProject changedGraph currentGraph True
        modifyIORef st (\es -> changeEdgeStyle es ESlashed)
        widgetQueueDraw canvas

  -- event bindings for the graphs' tree ---------------------------------------

  -- auxiliar
  let loadFromStore path = do
                      (_,newEs, newU, newR, _) <- listStoreGetValue store path
                      writeIORef st newEs
                      writeIORef undoStack newU
                      writeIORef redoStack newR
                      writeIORef currentGraph [path]


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
            gs <- listStoreGetValue store currentPath
            let (name, color) = (graphStoreName gs, graphStoreColor gs)
            listStoreSetValue store currentPath (name,currentES, u, r, color)
            -- load the selected graph from the tree
            loadFromStore path
            -- update canvas
            widgetQueueDraw canvas

  -- pressed the 'new' button
  btnNew `on` buttonActivated $ do
    listStoreAppend store ("new",emptyES,[],[],"green")
    modifyIORef lastSavedState (\sst -> sst ++ [DG.empty])
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
            modifyIORef changedGraph (\cg -> take path cg)
          (True, False) -> do
            listStoreRemove store path
            loadFromStore path -- load the graph of the next entry
            modifyIORef changedGraph (\cg -> take path cg ++ drop (path+1) cg)
          (False, True) -> do
            listStoreSetValue store 0 ("new",emptyES,[],[],"green")
            writeIORef changedGraph [False]
            writeIORef st emptyES
          _ -> return ()

        widgetQueueDraw canvas

  -- edited a graph name
  treeRenderer `on` edited $ \[path] newName -> do
    (oldName, val, u, r, color) <- listStoreGetValue store path
    listStoreSetValue store path (newName, val, u, r, color)
    writeIORef changedProject True
    indicateProjChanged window True

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
    continue <- liftIO $ confirmOperation
    if continue
      then do
        liftIO mainQuit
        return False
      else return True

  -- run the preogram ----------------------------------------------------------
  mainGUI


--------------------------------------------------------------------------------
-- Callbacks -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- update the inspector --------------------------------------------------------
updatePropMenu :: IORef EditorState -> IORef (Double,Double,Double) -> IORef (Double,Double,Double) -> (Entry, ColorButton, ColorButton, [RadioButton], [RadioButton]) -> (HBox, Frame, Frame)-> IO ()
updatePropMenu st currentC currentLC (entryName, colorBtn, lcolorBtn, radioShapes, radioStyles) (hBoxColor, frameShape, frameStyle) = do
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
      entrySetText entryName "----"
      colorButtonSetColor colorBtn $ Color 49151 49151 49151
      colorButtonSetColor lcolorBtn $ Color 49151 49151 49151
      set hBoxColor [widgetVisible := True]
      set frameShape [widgetVisible := True]
      set frameStyle [widgetVisible := True]

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
      setSourceRGBA 0.29 0.56 0.85 0.5
      fill
      rectangle x y w h
      setSourceRGBA 0.29 0.56 0.85 1
      stroke
    Nothing -> return ()
  return ()

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
          showError (Just window) $ "Couldn't write to file." ++ path
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
              showError (Just window) $ "Couldn't write to file." ++ path
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
saveProject :: ListStore GraphStore -> String -> IO Bool
saveProject model path = do
  editorList <- listStoreToList model
  let getWhatMatters = (\(name, es, _, _, _) -> (name, editorGetGraph es, editorGetGI es))
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

-- graph interaction
-- create a new node, auto-generating it's name and dimensions
createNode' :: IORef EditorState -> GIPos -> PangoContext -> NodeShape -> GIColor -> GIColor -> IO ()
createNode' st pos context nshape color lcolor = do
  es <- readIORef st
  let nid = head $ newNodes (editorGetGraph es)
      --content = "node " ++ show nid
      content = ""
  dim <- getStringDims content context
  writeIORef st $ createNode es pos dim content nshape color lcolor

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

-- auxiliar function used by createNode' and renameSelected
-- given a text, compute the size of it's bounding box
-- uses the pango lib
getStringDims :: String -> PangoContext -> IO (Double, Double)
getStringDims str context = do
  pL <- layoutText context str
  (_, PangoRectangle _ _ w h) <- layoutGetExtents pL
  return (w+4, h+4)


-- Undo / Redo -----------------------------------------------------------------
stackUndo :: IORef [DiaGraph] -> IORef [DiaGraph] -> EditorState -> IO ()
stackUndo undo redo es = do
  let g = editorGetGraph es
      gi = editorGetGI es
  modifyIORef undo (\u -> (g,gi):u )
  modifyIORef redo (\_ -> [])

applyUndo :: IORef [DiaGraph] -> IORef [DiaGraph] -> IORef EditorState -> IO ()
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

applyRedo :: IORef [DiaGraph] -> IORef [DiaGraph] -> IORef EditorState -> IO ()
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

-- change window name to indicate if the project was modified
indicateProjChanged :: Window -> Bool -> IO ()
indicateProjChanged window True = do
  title <- get window windowTitle
  if title!!0 == '*'
    then return ()
    else set window [windowTitle := '*':title]

indicateProjChanged window False = do
  i:title <- get window windowTitle
  if i == '*'
    then set window [windowTitle := title]
    else return ()

indicateGraphChanged :: ListStore GraphStore -> Int -> Bool -> IO ()
indicateGraphChanged store path True = do
  (n,e,u,r,_) <- listStoreGetValue store path
  listStoreSetValue store path (n,e,u,r,"yellow")

indicateGraphChanged store path False = do
  (n,e,u,r,_) <- listStoreGetValue store path
  listStoreSetValue store path (n,e,u,r,"white")


-- change the flags that inform if the graphs and project were changed and inform the graphs
setChangeFlags :: Window -> ListStore GraphStore -> IORef Bool -> IORef [Bool] -> IORef [Int] -> Bool -> IO ()
setChangeFlags window store changedProject changedGraph currentPath True = do
  [path] <- readIORef currentPath
  modifyIORef changedGraph (\xs -> take path xs ++ [True] ++ drop (path+1) xs)
  writeIORef changedProject True
  indicateProjChanged window True
  indicateGraphChanged store path True

setChangeFlags window store changedProject changedGraph currentPath False = do
  [path] <- readIORef currentPath
  cg <- readIORef changedGraph
  let cg' = take path cg ++ [False] ++ drop (path+1) cg
      projRestored = and cg'
  writeIORef changedGraph cg'
  writeIORef changedProject projRestored
  indicateProjChanged window projRestored
  indicateGraphChanged store path False

-- change updatedState
updateSavedState :: IORef [DiaGraph] -> ListStore GraphStore -> IO ()
updateSavedState sst store = do
  list <- listStoreToList store
  let newSavedState = map (\(_,es,_,_,_) -> (editorGetGraph es, editorGetGI es)) list
  writeIORef sst newSavedState

-- Tarefas ---------------------------------------------------------------------

-- Progresso -------------------------------------------------------------------
-- *Criar uma janela de ajuda
-- *Mudar a linguagem da interface toda para inglês

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
-- *Arrumado bug que fazia o programa encerrar ao salvar com algum grafo que não o primeiro selecionado.
-- *Indicar em qual grafo está a mudança do projeto
-- *Mudar a linguagem dos comentários para inglês
-- *Perguntar se o usuario quer salvar o grafo no caso de ativar a ação 'new'
