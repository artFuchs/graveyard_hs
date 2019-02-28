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

nullNode = Node {nodeId = 0, nodeInfo = ""}
nullEdge = Edge {edgeId = 0, sourceId = 0, targetId = 0, edgeInfo = ""}

-- | estado do editor de grafos
--   contém todas as informações necssárias para desenhar o grafo
-- (grafo, nodos selecionados, arestas selecionadas, zoom, Pan)
type EditorState = (Graph String String, GraphicalInfo, ([NodeId],[EdgeId]) , Double, (Double,Double))

emptyES :: EditorState
emptyES = (empty, (M.empty, M.empty), ([], []), 1.0, (0.0,0.0))

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


getNodeGI nid giM = fromMaybe newNodeGI $ M.lookup nid giM
getEdgeGI eid giM = fromMaybe newEdgeGI $ M.lookup eid giM

startGUI :: IO()
startGUI = do
  -- inicializa a biblioteca GTK
  initGUI

  -- definição da GUI -----------------------------------------------------------
  -- janela de ajuda
  helpWindow <- buildHelpWindow
  -- cria o menu
  (maybeMenubar,new,opn,svn,sva,udo,rdo,hlp) <- buildMaybeMenubar
  -- cria o menu de propriedades
  (frameProps, entryNodeID, entryName, colorBtn, lineColorBtn, radioShapes, radioStyles, propBoxes) <- buildPropMenu
  let
    propWidgets = (entryNodeID, entryName, colorBtn, lineColorBtn, radioShapes, radioStyles)
    [radioCircle, radioRect, radioQuad] = radioShapes
    [radioNormal, radioPointed, radioSlashed] = radioStyles
  -- cria o painel da arvore de grafos
  (treePanel, treeview, btnNew, btnRmv) <- buildTreePanel
  -- cria a janela principal, contendo o canvas
  (window, canvas) <- buildMainWindow maybeMenubar frameProps treePanel

  -- mostra a GUI
  widgetShowAll window

  -- inicializa estado do editor -----------------------------------------------
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

  -- inicializa um modelo para adicionar à arvore ------------------------------
  projectCol <- treeViewGetColumn treeview 0
  store <- listStoreNew [("new", emptyES), ("new1", emptyES)]
  case projectCol of
    Nothing -> return ()
    Just col -> do
      [renderer] <- cellLayoutGetCells col
      treeViewSetModel treeview store
      cellLayoutSetAttributes col (castToCellRendererText renderer) store $ \ind -> [cellText := fst ind]



  -- TRATAMENTO DE EVENTOS -----------------------------------------------------
  -- tratamento de eventos - canvas --------------------------------------------
  -- evento de desenho
  canvas `on` draw $ do
    es <- liftIO $ readIORef st
    sq <- liftIO $ readIORef squareSelection
    drawGraph es sq canvas

  -- clique do mouse
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
      (LeftButton, True) -> liftIO $ widgetGrabFocus entryName
      -- clique com o botão esquerdo: seleciona nodos e edges
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
        -- adicionar/remover elementos da seleção
        (sNodes,sEdges) <- case (sNode, sEdge, Shift `elem` ms, Control `elem` ms) of
          -- clicou no espaço em branco, Shift não pressionado
          ([],[], False, _) -> do
            modifyIORef st (editorSetSelected ([],[]))
            writeIORef squareSelection $ Just (x',y',0,0)
            return ([],[])
          -- selecionou nodos ou edges com shift não pressionado -> se não fizer parte da seleção, torna-los a seleção
          (n, [], False, _) -> if n!!0 `elem` oldSN
                                then return (oldSN,oldSE)
                                else do modifyIORef st (editorSetSelected (n, []))
                                        return (n,[])
          ([],e,False, _) -> if e!!0 `elem` oldSE
                                then return (oldSN,oldSE)
                                else do modifyIORef st (editorSetSelected ([], e))
                                        return ([],e)
          -- selecionou nodos ou edges com shift pressionado -> adicionar para seleção
          (n,e,True, False) -> do
            let jointSN = foldl (\ns n -> if n `notElem` ns then n:ns else ns) [] $ sNode ++ oldSN
                jointSE = foldl (\ns n -> if n `notElem` ns then n:ns else ns) [] $ sEdge ++ oldSE
            modifyIORef st (editorSetGraph graph . editorSetSelected (jointSN,jointSE))
            return (jointSN, jointSE)
          -- selecionou nodos ou edges com shift e ctrl pressionados -> remover da seleção
          (n,e,True, True) -> do
            let jointSN = delete (sNode!!0) oldSN
                jointSE = delete (sEdge!!0) oldSE
            modifyIORef st (editorSetGraph graph . editorSetSelected (jointSN,jointSE))
            return (jointSN, jointSE)
          _ -> return (oldSN, oldSE)
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
          writeIORef fileName Nothing
          set window [windowTitle := "Graph Editor"]
          widgetQueueDraw canvas
        -- CTRL + SHIFT + S : save file as
        (True, True, "s") -> saveGraphAs st fileName window
        -- CTRL + S : save file
        (True, False, "s") -> saveGraph st fileName window
        -- CTRL + O : open file
        (True, False, "o") -> do
          mg <- loadGraph window
          case mg of
            Just (g,gi, fn) -> do
              writeIORef st (g,gi,([],[]),1.0,(0.0,0.0))
              writeIORef fileName $ Just fn
              set window [windowTitle := "Graph Editor - " ++ fn]
              widgetQueueDraw canvas
            _      -> return ()

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
    modifyIORef st (\es -> (empty, (M.empty, M.empty), ([],[]),1.0,(0.0,0.0)))
    writeIORef fileName Nothing
    set window [windowTitle := "Graph Editor"]
    widgetQueueDraw canvas

  opn `on` actionActivated $ do
    mg <- loadProject window
    case mg of
      Just (list,fn) -> do
        if length list > 0
          then do
            listStoreClear store
            forM list (listStoreAppend store)
            let (name,es) = list!!0
            writeIORef st es
            writeIORef fileName $ Just fn
            set window [windowTitle := "Graph Editor - " ++ fn]
            widgetQueueDraw canvas
          else return ()
      Nothing -> return ()

  svn `on` actionActivated $ do
    -- update the current graph to save it alonge the others graphs on the project
    currentES <- readIORef st
    [path] <- readIORef currentGraph
    (name, _)<- listStoreGetValue store path
    listStoreSetValue store path (name,currentES)
    -- save the project
    saveProject store fileName window

  sva `on` actionActivated $ saveGraphAs st fileName window

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
    [currentPath] <- readIORef currentGraph
    selection <- treeViewGetSelection treeview
    sel <- treeSelectionGetSelected selection
    case sel of
      Nothing -> return ()
      Just it -> do
        -- update the current graph in the tree
        currentES <- readIORef st
        (name, _)<- listStoreGetValue store currentPath
        listStoreSetValue store currentPath (name,currentES)
        -- load the selected graph from the tree
        [path] <- treeModelGetPath store it
        writeIORef currentGraph [path]
        (_,newEs) <- listStoreGetValue store path
        writeIORef st newEs
        widgetQueueDraw canvas

  btnNew `on` buttonActivated $ do
    listStoreAppend store ("new",emptyES)
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
            listStoreSetValue store 0 ("new",emptyES)
            writeIORef st emptyES
          _ -> return ()

        widgetQueueDraw canvas









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


-- salvar projeto --------------------------------------------------------------
saveProject :: ListStore (String, EditorState)-> IORef (Maybe String) -> Window -> IO ()
saveProject model fileName window = do
  fn <- readIORef fileName
  case fn of
    Just path -> do
      tentativa <- saveProject' model path
      case tentativa of
        True -> return ()
        False -> do
          showError (Just window) "Não foi possível salvar o projeto"
          return ()
    Nothing -> do
      tentativa <- saveProject' model "dummy"
      case tentativa of
        True -> return ()
        False -> do
          showError (Just window) "Não foi possível salvar o dummy"
          return ()


saveProject' :: ListStore (String, EditorState) -> String -> IO Bool
saveProject' model path = do
  editorList <- listStoreToList model
  let getWhatMatters = (\(name, es) -> (name, editorGetGraph es, editorGetGI es))
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

-- carregar projeto ------------------------------------------------------------
loadProject :: Window -> IO (Maybe ([(String, EditorState)], String))
loadProject window = do
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
            Right content -> do
              let contentList = read content :: [(String, [(Int, String)], [(Int,Int,Int,String)], GraphicalInfo)]
                  genNodes = map (\(nid, info) -> Node (NodeId nid) info)
                  genEdges = map (\(eid, src, dst, info) -> Edge (EdgeId eid) (NodeId src) (NodeId dst) info)
                  editorList = map (\(name,readNodes,readEdges,gi) -> let nds = genNodes readNodes
                                                                          eds = genEdges readEdges
                                                                          g = fromNodesAndEdges nds eds
                                                                      in (name, editorSetGI gi . editorSetGraph g $ emptyES) ) contentList
              return $ Just (editorList, path)
    _             -> do
      widgetDestroy loadD
      return Nothing


-- salvar grafo ----------------------------------------------------------------
saveGraph :: IORef EditorState -> IORef (Maybe String) -> Window -> IO ()
saveGraph st fileName window = do
  es <- readIORef st
  let g = editorGetGraph es
      gi = editorGetGI es
  fn <- readIORef fileName
  case fn of
    Just path -> do
      tentativa <- saveGraph' (g,gi) path
      case tentativa of
        True -> return ()
        False -> do
          showError (Just window) "Não foi possível escrever no arquivo"
          return ()
    Nothing -> do
      saveGraphAs st fileName window

saveGraphAs :: IORef EditorState -> IORef (Maybe String) -> Window -> IO ()
saveGraphAs st fileName window = do
  es <- readIORef st
  let g = editorGetGraph es
      gi = editorGetGI es
  fn <- saveGraphAs' (g,gi) window
  case fn of
    Nothing -> return ()
    Just path -> do
      writeIORef fileName fn
      set window [windowTitle := "Graph Editor - " ++ path]


saveGraphAs' :: (Graph String String ,GraphicalInfo) -> Window -> IO (Maybe String)
saveGraphAs' (g,gi) window = do
  saveD <- fileChooserDialogNew
           (Just "Salvar arquivo")
           (Just window)
           FileChooserActionSave
           [("Cancela",ResponseCancel),("Salva",ResponseAccept)]
  fileChooserSetDoOverwriteConfirmation saveD True
  widgetShow saveD
  response <- dialogRun saveD
  case response of
    ResponseAccept -> do
      filename <- fileChooserGetFilename saveD
      case filename of
        Nothing -> do
          widgetDestroy saveD
          return Nothing
        Just path -> do
          tentativa <- saveGraph' (g,gi) path
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
loadGraph :: Window -> IO (Maybe (Graph String String, GraphicalInfo, String))
loadGraph window = do
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
            Right content -> do
              let (rns,res,gi) = read content :: ([(Int, String)], [(Int,Int,Int,String)], GraphicalInfo)
                  ns = map (\(nid, info) -> Node (NodeId nid) info) rns
                  es = map (\(eid, src, dst, info) -> Edge (EdgeId eid) (NodeId src) (NodeId dst) info) res
                  g = fromNodesAndEdges ns es
              return $ Just $ (g,gi,path)
    _             -> do
      widgetDestroy loadD
      return Nothing

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



-- operações básicas sobre o grafo no estado -----------------------------------
-- cria um novo nodo e insere no grafo
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

-- cria e insere uma nova edge no grafo
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
-- *Editar multiplos grafos no mesmo projeto
--   *Criar uma arvore de grafos


-- Feito (Acho melhor parar de deletar da lista de Tarefas) --------------------
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
