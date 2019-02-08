import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Layout
import Data.List
import Data.Maybe
--import qualified Data.Char as C
import qualified Data.Text as T
import qualified Control.Exception as E
import qualified Data.Map as M
import Graph
import GraphicalInfo
import Render
import Helper
import UIConstructors

nullEdge = Edge 0 ""

-- | estado do editor de grafos
--   contém todas as informações necssárias para desenhar o grafo
-- (grafo, nodos selecionados, arestas selecionadas, zoom, Pan)
type EditorState = (Graph, GraphicalInfo, ([Node],[Edge]) , Double, (Double,Double))

editorGetGraph :: EditorState -> Graph
editorGetGraph (g,_,_,_,_) = g

editorSetGraph :: Graph -> EditorState -> EditorState
editorSetGraph g (_,gi,s,z,p) = (g,gi,s,z,p)

editorGetGI :: EditorState -> GraphicalInfo
editorGetGI (_,gi,_,_,_) = gi

editorSetGI :: GraphicalInfo -> EditorState -> EditorState
editorSetGI gi (g,_,s,z,p) = (g,gi,s,z,p)

editorGetSelected :: EditorState -> ([Node], [Edge])
editorGetSelected (_,_,s,_,_) = s

editorSetSelected :: ([Node], [Edge]) -> EditorState -> EditorState
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

main :: IO()
main = do
  -- inicializa a biblioteca GTK
  initGUI

  -- definição da UI -----------------------------------------------------------
  -- cria a janela principal
  window <- windowNew
  set window  [ windowTitle         := "Graph Editor"
              , windowDefaultWidth  := 640
              , windowDefaultHeight := 480]

  -- cria uma VBox para separar o editor do menu
  vBoxMain <- vBoxNew False 0
  containerAdd window vBoxMain

  -- cria o menu
  (maybeMenubar,new,opn,svn,udo,rdo) <- buildMaybeMenubar
  case maybeMenubar of
    Just x -> boxPackStart vBoxMain x PackNatural 0
    Nothing -> return ()

  -- cria um HPane para dividir o canvas e o menu de propriedade
  hPaneAction <- hPanedNew
  boxPackStart vBoxMain hPaneAction PackGrow 0

  -- cria o menu de propriedades
  (frameProps, entryNodeID, entryNodeName, colorBtn, lineColorBtn, radioShapes, propBoxes) <- buildPropMenu
  let propWidgets = (entryNodeID, entryNodeName, colorBtn, lineColorBtn, radioShapes)
      [radioCircle,radioRect,radioQuad] = radioShapes
  panedPack2 hPaneAction frameProps False True

  -- cria um frame para englobar o canvas
  frameCanvas <- frameNew
  set frameCanvas [ frameShadowType := ShadowIn ]
  panedPack1 hPaneAction frameCanvas True True
  -- cria um canvas em branco
  canvas <- drawingAreaNew
  widgetSetCanFocus canvas True
  widgetAddEvents canvas [AllEventsMask]
  widgetDelEvents canvas [SmoothScrollMask]
  widgetGrabFocus canvas
  widgetModifyBg canvas StateNormal (Color 65535 65535 65535) -- parece que não funciona
  containerAdd frameCanvas canvas

  -- mostra a GUI
  widgetShowAll window

  -- inicializa estado do editor -----------------------------------------------
  st <- newIORef (emptyGraph "New", (M.empty, M.empty), ([], []), 1.0, (0.0,0.0)) -- estado do editor: todas as informações necessárias para desenhar o grafo
  oldPoint <- newIORef (0.0,0.0) -- ultimo ponto em que o botão do mouse foi pressionado
  squareSelection <- newIORef Nothing -- estado da caixa de seleção - Maybe (x,y,w,h)
  changes <- newIORef ([],[]) -- pilhas de undo/redo - ([Graph],[Graph])
  movingGI <- newIORef False -- se o usuario começou a mover algum objeto
  actualShape <- newIORef NCircle
  clipboard <- newIORef (emptyGraph "", (M.empty, M.empty)) -- clipboard - (Graph, GraphicalInfo)

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
    let z = editorGetZoom es
        (px,py) = editorGetPan es
        (x',y') = (x/z - px, y/z - py)
    liftIO $ do
      writeIORef oldPoint (x',y')
      widgetGrabFocus canvas
    case b of
      -- clique com o botão esquerdo: seleciona nodos e edges
      LeftButton  -> liftIO $ do
        let (oldSN,oldSE) = editorGetSelected es
            graph = editorGetGraph es
            gi = editorGetGI es
            sNode = case checkSelectNode gi (x',y') of
              Nothing -> []
              Just nid -> [(fromMaybe nullNode (getNodeByID graph nid))]
            sEdge = case checkSelectEdge gi (x',y') of
              Nothing -> []
              Just eid -> [fromMaybe nullEdge (find (\e -> edgeGetID e == eid) (graphGetEdges graph))]
        -- Shift: seleção de multiplos elementos
        (sNodes,sEdges) <- case (sNode, sEdge, Shift `elem` ms, Control `elem` ms) of
          -- clicou no espaço em branco, Shift não pressionado
          ([],[], False, _) -> do
            modifyIORef st (editorSetSelected ([],[]))
            writeIORef squareSelection $ Just (x',y',0,0)
            return ([],[])
          ([],[], _, _) -> return (oldSN, oldSE)

          (n,e,False, _) -> do
            modifyIORef st (editorSetSelected (sNode, sEdge))
            return (sNode, sEdge)
          (n,e,True, False) -> do
            let jointSN = foldl (\ns n -> if n `notElem` ns then n:ns else ns) [] $ sNode ++ oldSN
                jointSE = foldl (\ns n -> if n `notElem` ns then n:ns else ns) [] $ sEdge ++ oldSE
            modifyIORef st (editorSetGraph graph . editorSetSelected (jointSN,jointSE))
            return (jointSN, jointSE)
          (n,e,True, True) -> do
            let jointSN = delete (sNode!!0) oldSN
                jointSE = delete (sEdge!!0) oldSE
            modifyIORef st (editorSetGraph graph . editorSetSelected (jointSN,jointSE))
            return (jointSN, jointSE)
        widgetQueueDraw canvas
        es <- readIORef st
        updatePropMenu es propWidgets propBoxes
      -- clique com o botão direito: cria nodos e insere edges entre nodos
      RightButton -> liftIO $ do
        let g = editorGetGraph es
            gi = editorGetGI es
            dstNode = case checkSelectNode gi (x',y') of
              Nothing -> []
              Just nid -> (fromMaybe nullNode (getNodeByID g nid)) : []
        context <- widgetGetPangoContext canvas
        stackUndo changes es
        sNode <- if null dstNode
                  then do
                    shape <- readIORef actualShape
                    createNode st (x',y') context shape
                    es <- readIORef st
                    return $ fst (editorGetSelected es)
                  else do
                    modifyIORef st (\es -> createEdges es dstNode)
                    return dstNode
        widgetQueueDraw canvas
        es <- readIORef st
        updatePropMenu es propWidgets propBoxes
      _           -> return ()

    return True

  -- movimento do mouse
  canvas `on` motionNotifyEvent $ do
    ms <- eventModifierAll
    (x,y) <- eventCoordinates
    (ox,oy) <- liftIO $ readIORef oldPoint
    es <- liftIO $ readIORef st
    let leftButton = Button1 `elem` ms
        middleButton = Button2 `elem` ms
        (sNodes, sEdges) = editorGetSelected es
        z = editorGetZoom es
        (px,py) = editorGetPan es
        (x',y') = (x/z - px, y/z - py)
    case (leftButton, middleButton, sNodes, sEdges) of
      (True, False, [], []) -> liftIO $ do
        modifyIORef squareSelection $ liftM $ (\(a,b,c,d) -> (a,b,x'-a,y'-b))
        sq <- readIORef squareSelection
        widgetQueueDraw canvas
        --print $ "squareSelection: " ++ show sq
      (True, False, n, e) -> liftIO $ do
        modifyIORef st (\es -> moveNodes es (ox,oy) (x',y'))
        modifyIORef st (\es -> moveEdges es (ox,oy) (x',y'))
        writeIORef oldPoint (x',y')
        mv <- readIORef movingGI
        if not mv
          then do
            writeIORef movingGI True
            stackUndo changes es
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
                (ngi, egi) = editorGetGI es
                sNodes = filter (\n -> let pos = position $ getNodeGI (nodeGetID n) ngi
                                       in pointInsideRectangle pos (x + (w/2), y + (h/2), abs w, abs h)) $ graphGetNodes graph
                sEdges = filter (\e -> let pos = cPosition $ getEdgeGI (edgeGetID e) egi
                                       in pointInsideRectangle pos (x + (w/2), y + (h/2), abs w, abs h)) $ graphGetEdges graph
                newEs = editorSetSelected (sNodes, sEdges) $ es
            writeIORef st newEs
            updatePropMenu newEs propWidgets propBoxes
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
          stackUndo changes es
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
                                     nodes = graphGetNodes g
                                     edges = graphGetEdges g
                                 in editorSetSelected (nodes,edges) es)
          widgetQueueDraw canvas
        -- CTRL + N : create a new file
        (True, False, "n") -> do
          modifyIORef st (\es -> (emptyGraph "", (M.empty, M.empty),([],[]),1.0,(0.0,0.0)))
          widgetQueueDraw canvas
        -- CTRL + S : save file
        (True, False, "s") -> do
          es <- readIORef st
          let g = editorGetGraph es
              gi = editorGetGI es
          saveGraph (g,gi) window
        -- CTRL + O : open file
        (True, False, "o") -> do
          mg <- loadGraph window
          case mg of
            Just (g,gi) -> do
              writeIORef st (g,gi,([],[]),1.0,(0.0,0.0))
              widgetQueueDraw canvas
            _      -> return ()

        -- CTRL + Z/R : undo/redo
        (True, False, "z") -> do
          applyUndo changes st
          widgetQueueDraw canvas
        (True, False, "r") -> do
          applyRedo changes st
          widgetQueueDraw canvas
        -- CTRL + C/V/X : copy/paste/cut
        (True, False, "c") -> do
          es <- readIORef st
          writeIORef clipboard $ copySelected es
        (True, False, "v") -> do
          es <- readIORef st
          clip <- readIORef clipboard
          stackUndo changes es
          modifyIORef st (pasteClipBoard clip)
          widgetQueueDraw canvas
        (True, False, "x") -> do
          es <- readIORef st
          writeIORef clipboard $ copySelected es
          modifyIORef st (\es -> deleteSelected es)
          stackUndo changes es
          widgetQueueDraw canvas
        _       -> return ()

    return True

  -- tratamento de eventos -- menu toolbar -------------------------------------
  new `on` actionActivated $ do
    modifyIORef st (\es -> (emptyGraph "", (M.empty, M.empty), ([],[]),1.0,(0.0,0.0)))
    widgetQueueDraw canvas

  opn `on` actionActivated $ do
    mg <- loadGraph window
    case mg of
      Just (g,gi) -> do
        writeIORef st (g,gi,([],[]),1.0,(0.0,0.0))
        widgetQueueDraw canvas
      _      -> return ()

  svn `on` actionActivated $ do
    es <- readIORef st
    let g = editorGetGraph es
        gi = editorGetGI es
    saveGraph (g,gi) window

  udo `on` actionActivated $ do
    applyUndo changes st
    widgetQueueDraw canvas

  rdo `on` actionActivated $ do
    applyRedo changes st
    widgetQueueDraw canvas


  -- tratamento de eventos -- menu de propriedades -----------------------------
  entryNodeName `on` keyPressEvent $ do
    k <- eventKeyName
    liftIO $ do
      case T.unpack k of
        "Return" -> do
          es <- readIORef st
          stackUndo changes es
          name <- entryGetText entryNodeName :: IO String
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
        (nodes,edges) = editorGetSelected es
        (ngiM, egiM) = editorGetGI es
        changeColor = (\giMap n -> let nid = nodeGetID n
                                       gi = nodeGiSetColor col $ getNodeGI nid ngiM
                                   in M.insert nid gi giMap)
        newngiM = foldl changeColor ngiM nodes
    modifyIORef st (\es -> editorSetGI (newngiM, egiM) es)
    widgetQueueDraw canvas

  onColorSet lineColorBtn $ do
    Color r g b <- colorButtonGetColor lineColorBtn
    es <- readIORef st
    let color = ((fromIntegral r)/65535, (fromIntegral g)/65535, (fromIntegral b)/65535)
        graph = editorGetGraph es
        (nodes,edges) = editorGetSelected es
        (ngiM, egiM) = editorGetGI es
        changeNLC = (\giMap n -> let nid = nodeGetID n
                                     gi = nodeGiSetLineColor color $ getNodeGI nid ngiM
                                 in M.insert nid gi giMap)
        newngiM = foldl changeNLC ngiM nodes
        changeELC = (\giMap e -> let eid = edgeGetID e
                                     gi = edgeGiSetColor color $ getEdgeGI eid egiM
                                 in M.insert eid gi giMap)
        newegiM = foldl changeELC egiM edges
    modifyIORef st (\es -> editorSetGI (newngiM, newegiM) es)
    widgetQueueDraw canvas

  radioCircle `on` toggled $ do
    modifyIORef st (\es -> changeNodeShape es NCircle)
    writeIORef actualShape NCircle
    widgetQueueDraw canvas

  radioRect `on` toggled $ do
    modifyIORef st (\es -> changeNodeShape es NRect)
    writeIORef actualShape NRect
    widgetQueueDraw canvas

  radioQuad `on` toggled $ do
    modifyIORef st (\es -> changeNodeShape es NQuad)
    writeIORef actualShape NQuad
    widgetQueueDraw canvas


  -- tratamento de eventos - janela principal ---------------------------------
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  mainGUI



-- Callbacks -------------------------------------------------------------------
-- atualização do menu de propriedades -----------------------------------------
updatePropMenu :: EditorState -> (Entry, Entry, ColorButton, ColorButton, [RadioButton]) -> (HBox, Frame)-> IO ()
updatePropMenu es (entryID, entryName, colorBtn, lcolorBtn, radioShapes) (hBoxColor, frameShape) = do
  let (nodes,edges) = editorGetSelected es
      (ngiM,egiM) = editorGetGI es
  case (length nodes, length edges) of
    (0,0) -> do
      entrySetText entryID ""
      entrySetText entryName ""
      colorButtonSetColor colorBtn $ Color 49151 49151 49151
      colorButtonSetColor lcolorBtn $ Color 49151 49151 49151
      set hBoxColor [widgetVisible := True]
      set frameShape [widgetVisible := True]
    (1,0) -> do
      let iD = nodeGetID $ (nodes!!0)
          name = nodeGetInfo $ (nodes!!0)
          gi = getNodeGI iD ngiM
          (r,g,b) = fillColor gi
          (r',g',b') = lineColor gi
          nodeColor = Color (round (r*65535)) (round (g*65535)) (round (b*65535))
          nodeLineC = Color (round (r'*65535)) (round (g'*65535)) (round (b'*65535))
          nodeShape = shape gi
      entrySetText entryID (show iD)
      entrySetText entryName name
      colorButtonSetColor colorBtn nodeColor
      colorButtonSetColor lcolorBtn nodeLineC
      case nodeShape of
        NCircle -> toggleButtonSetActive (radioShapes!!0) True
        NRect -> toggleButtonSetActive (radioShapes!!1) True
        NQuad -> toggleButtonSetActive (radioShapes!!2) True

      set hBoxColor [widgetVisible := True]
      set frameShape [widgetVisible := True]
    (0,1) -> do
      let iD = edgeGetID $ (edges!!0)
          name = edgeGetInfo (edges!!0)
          gi = getEdgeGI iD egiM
          (r,g,b) = color gi
          edgeColor = Color (round (r*65535)) (round (g*65535)) (round (b*65535))
      entrySetText entryID (show iD)
      entrySetText entryName name
      colorButtonSetColor lcolorBtn edgeColor

      set hBoxColor [widgetVisible := False]
      set frameShape [widgetVisible := False]
    (0,n) -> do
      entrySetText entryID "----"
      entrySetText entryName $ (\l -> let x:xs = l in if all (==x) xs then x else "----") (map edgeGetInfo edges)
      colorButtonSetColor colorBtn $ Color 49151 49151 49151
      colorButtonSetColor lcolorBtn $ Color 49151 49151 49151
      set hBoxColor [widgetVisible := False]
      set frameShape [widgetVisible := False]
    (n,0) -> do
      entrySetText entryID "----"
      entrySetText entryName $ (\l -> let x:xs = l in if all (==x) xs then x else "----") (map nodeGetInfo nodes)
      colorButtonSetColor colorBtn $ Color 49151 49151 49151
      colorButtonSetColor lcolorBtn $ Color 49151 49151 49151
      set hBoxColor [widgetVisible := True]
      set frameShape [widgetVisible := True]
    _ -> do
      entrySetText entryID "--"
      entrySetText entryName "----"
      colorButtonSetColor colorBtn $ Color 49151 49151 49151
      colorButtonSetColor lcolorBtn $ Color 49151 49151 49151
      set hBoxColor [widgetVisible := True]
      set frameShape [widgetVisible := True]

-- salvar grafo ----------------------------------------------------------------
saveGraph :: (Graph,GraphicalInfo) -> Window -> IO ()
saveGraph (g,gi) window = do
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
        Nothing -> widgetDestroy saveD
        Just path -> do
          let writeGraph = do
                            writeFile path $ show g
                            appendFile path $ "\n" ++ show gi
          tentativa <- E.try (writeGraph)  :: IO (Either E.IOException ())
          case tentativa of
            Left _ -> print "Não foi possível escrever no arquivo"
            Right _ -> return ()
          widgetDestroy saveD
    _  -> widgetDestroy saveD

-- abrir grafo -----------------------------------------------------------------
loadGraph :: Window -> IO (Maybe (Graph, GraphicalInfo))
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
      case filename of
        Nothing -> do
          widgetDestroy loadD
          return Nothing
        Just path -> do
          tentativa <- E.try (readFile path) :: IO (Either E.IOException String)
          case tentativa of
            Left _ -> do
              putStrLn "Não foi possivel ler o arquivo"
              return Nothing
            Right content -> do
              widgetDestroy loadD
              let (g, str) = string2graph content
                  gi = read str :: GraphicalInfo
              return $ Just $ (g,gi)
    _               -> do
      widgetDestroy loadD
      return Nothing

-- atualização do desenho do grafo ---------------------------------------------
drawGraph :: EditorState -> Maybe (Double,Double,Double,Double) -> DrawingArea -> Render ()
drawGraph (g, (nGI,eGI), (sNodes, sEdges), z, (px,py)) sq canvas = do
  context <- liftIO $ widgetGetPangoContext canvas
  scale z z
  translate px py

  -- desenha as arestas
  forM (graphGetEdges g) (\e -> do
    let dstN = M.lookup (graphGetDstFunc g e) nGI
        srcN = M.lookup (graphGetSrcFunc g e) nGI
        egi = M.lookup (edgeGetID e) eGI
        selected = e `elem` sEdges
    case (egi, srcN, dstN) of
      (Just gi, Just src, Just dst) -> renderEdge gi (edgeGetInfo e) selected src dst context
      _ -> return ())

  -- desenha os nodos
  forM (M.toList nGI) (\(k,ngi) -> let info = nodeGetInfo . fromMaybe nullNode $ getNodeByID g k
                                       selected = (k `elem` (map nodeGetID sNodes))
                                   in renderNode ngi info selected context)
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
checkSelectNode:: GraphicalInfo -> (Double,Double) -> Maybe Int
checkSelectNode (nodesG,_) (x,y) = case find (\n -> isSelected (snd n)) $ (M.toList nodesG) of
                                    Nothing -> Nothing
                                    Just (k,a) -> Just k
  where isSelected = (\n -> let (nx,ny) = position  n
                                (w,h) = dims n
                                l = max w h
                            in case shape n of
                              NCircle -> pointDistance (x,y) (nx,ny) < l/2
                              NRect -> pointInsideRectangle (x,y) (nx,ny,w,h)
                              NQuad -> pointInsideRectangle (x,y) (nx,ny,l,l) )

-- verifica se o usuario selecionou alguma aresta
checkSelectEdge:: GraphicalInfo -> (Double,Double) -> Maybe Int
checkSelectEdge (_,edgesG) (x,y) = case find (\e -> isSelected (snd e)) $ (M.toList edgesG) of
                            Nothing -> Nothing
                            Just (k,a) -> Just k
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
      moveN = (\giMap node -> let nid = nodeGetID node
                                  gi = getNodeGI nid giMap
                                  (ox, oy) = position gi
                              in M.insert nid (nodeGiSetPosition (ox+deltaX,oy+deltaY) gi) giMap )
      movedNGIs = foldl moveN ngiM sNodes
      -- move as arestas que estão no ponto entre os nodos
      moveE = (\giMap edge -> let a = graphGetSrcFunc graph edge
                                  b = graphGetDstFunc graph edge
                                  getPos = \nid -> position . getNodeGI nid $ movedNGIs
                                  aPos = getPos a
                                  bPos = getPos b
                                  gi = getEdgeGI (edgeGetID edge) egiM
                                  (xd,yd) = addPoint (cPosition gi) (deltaX,deltaY)
                              in case (edgeGetID edge `elem` (map edgeGetID sEdges), a == b, any (`elem` (map nodeGetID sNodes)) [a,b], centered gi) of
                                  (False, True, True, _) -> M.insert (edgeGetID edge) (edgeGiSetPosition (xd,yd) gi) giMap
                                  (False, False, True, True) -> M.insert (edgeGetID edge) (edgeGiSetPosition (midPoint aPos bPos) gi) giMap
                                  _ -> giMap
                                  )
      movedEGIs = foldl moveE egiM (graphGetEdges graph)

-- move as arestas selecionadas
moveEdges:: EditorState -> (Double,Double) -> (Double,Double) -> EditorState
moveEdges es (xold,yold) (xnew,ynew) = editorSetGI (ngi,newegi) es
  where graph = editorGetGraph es
        (sNodes,sEdges) = editorGetSelected es
        (deltaX, deltaY) = (xnew-xold,ynew-yold)
        (ngi,egi) = editorGetGI es
        moveE = (\egiM edge -> let gi = getEdgeGI (edgeGetID edge) egi
                                   (xe, ye) = cPosition gi
                                   newPos = (xe+deltaX, ye+deltaY)
                                   srcPos = position . getNodeGI (graphGetSrcFunc graph edge) $ ngi
                                   dstPos = position . getNodeGI (graphGetDstFunc graph edge) $ ngi
                                   mustCenter = pointLineDistance newPos srcPos dstPos < 10
                               in M.insert (edgeGetID edge) (edgeGiSetCentered mustCenter . edgeGiSetPosition newPos $ gi) egiM)
        newegi = foldl moveE egi sEdges

-- ajusta a posição das edges selecionadas caso a propriedade centered seja True
adjustEdges:: EditorState -> EditorState
adjustEdges es = editorSetGI (ngiM,newEgiM) es
  where graph = editorGetGraph es
        (ngiM,egiM) = editorGetGI es
        adjust = (\giM e -> let srcPos = position $ getNodeGI (graphGetSrcFunc graph e) ngiM
                                dstPos = position $ getNodeGI (graphGetDstFunc graph e) ngiM
                                gi = getEdgeGI (edgeGetID e) egiM
                        in if centered gi
                          then M.insert (edgeGetID e) (edgeGiSetPosition (midPoint srcPos dstPos) gi) giM
                          else giM)
        newEgiM = foldl adjust egiM (snd . editorGetSelected $ es)



-- operações básicas sobre o grafo no estado -----------------------------------
-- cria um novo nodo e insere no grafo
createNode:: IORef EditorState -> (Double,Double) -> PangoContext -> NodeShape -> IO ()
createNode st pos context shape = do
  es <- readIORef st
  let graph = editorGetGraph es
      (nodes, edges) = editorGetSelected es
      maxNID = if length (graphGetNodes graph) > 0
                  then nodeGetID $ maximum (graphGetNodes graph)
                  else 0
      nID = 1 + maxNID
      newNode = Node nID content
      newGraph = insertNode graph newNode
      content = ("node " ++ show nID)
  dim <- getStringDims content context
  let newNgi = nodeGiSetShape shape . nodeGiSetDims dim . nodeGiSetPosition pos $ newNodeGI
      newGIM = (M.insert nID newNgi $ fst (editorGetGI es), snd (editorGetGI es))
  writeIORef st $ editorSetGI newGIM . editorSetGraph newGraph . editorSetSelected ([newNode], []) $ es

-- cria e insere uma nova edge no grafo
createEdges:: EditorState -> [Node] -> EditorState
createEdges es dstNodes = editorSetGraph newGraph . editorSetGI (ngiM, newegiM) . editorSetSelected (dstNodes,[]) $ es
  where selectedNodes = fst $ editorGetSelected es
        graph = editorGetGraph es
        (ngiM,egiM) = editorGetGI es
        (newGraph, newegiM) = if length dstNodes == 1
          then foldl (\(g,giM) n -> let ng = insertEdge g n (dstNodes!!0)
                                        e = (graphGetEdges ng)!!0
                                        eid = edgeGetID e
                                        getPos = (\n -> position . getNodeGI (nodeGetID n) $ ngiM)
                                        (srcPos,dstPos) = applyPair getPos (n,dstNodes!!0)
                                        negi = edgeGiSetPosition (midPoint srcPos dstPos) newEdgeGI
                                    in (ng, M.insert eid negi giM)) (graph, egiM) selectedNodes
          else (graph,egiM)




-- deleta os nodos e arestas selecionados no grafo
deleteSelected:: EditorState -> EditorState
deleteSelected es = editorSetSelected ([],[]) . editorSetGI (newngiM, newegiM) . editorSetGraph newGraph $ es
  where graph = editorGetGraph es
        (nodes,edges) = editorGetSelected es
        (ngiM, egiM) = editorGetGI es
        newngiM = foldl (\giM n -> M.delete n giM) ngiM (map nodeGetID nodes)
        newegiM = foldl (\giM n -> M.delete n giM) egiM (map edgeGetID edges)
        graph' = foldl (\g n -> removeNode g n) graph nodes
        newGraph = foldl (\g e -> removeEdge g e) graph' edges

-- renomeia os selecionados
renameSelected:: IORef EditorState -> String -> PangoContext -> IO()
renameSelected state name context = do
  es <- readIORef state
  dim <- getStringDims name context
  let graph = editorGetGraph es
      (nodes,edges) = editorGetSelected es
      (ngiM,egiM) = editorGetGI es
  let renamedNodes = map (\n -> Node (nodeGetID n) name) nodes
      newNgiM = M.mapWithKey (\k gi -> if k `elem` (map nodeGetID nodes) then nodeGiSetDims dim gi else gi) ngiM
      renamedEdges = map (\e -> Edge (edgeGetID e) name) edges
      newGraph = foldl (\g n -> changeNode g n) graph renamedNodes
      newGraph' = foldl (\g e -> changeEdge g e) newGraph renamedEdges
      newEs = editorSetGI (newNgiM,egiM) . editorSetGraph newGraph' . editorSetSelected (renamedNodes, renamedEdges) $ es
  writeIORef state newEs

-- muda a forma de um nodo
changeNodeShape :: EditorState -> NodeShape -> EditorState
changeNodeShape es s = editorSetGI (newNgiM, egiM) $ es
  where
      nodes = fst $ editorGetSelected es
      (ngiM, egiM) = editorGetGI es
      newNgiM = M.mapWithKey (\k gi -> if k `elem` (map nodeGetID nodes) then nodeGiSetShape s gi else gi) ngiM



-- função auxiliar para createNode e renameSelected
-- dado um texto, adquire o tamanho da bounding box do texto para renderiza-lo
-- utiliza a biblioteca pango para isso
getStringDims :: String -> PangoContext -> IO (Double, Double)
getStringDims str context = do
  pL <- layoutText context str
  (_, PangoRectangle _ _ w h) <- layoutGetExtents pL
  return (w+4, h+4)

-- Undo / Redo -----------------------------------------------------------------
stackUndo :: IORef ([Graph],[Graph]) -> EditorState -> IO ()
stackUndo changes es = do
  let g = editorGetGraph es
  modifyIORef changes (\(u,r) -> (g:u,[]))

applyUndo :: IORef ([Graph],[Graph]) -> IORef EditorState -> IO ()
applyUndo changes st = do
  ur <- readIORef changes
  es <- readIORef st
  let apply ([],r) es = (([],r), es)
      apply (g:u,r) es = ((u, editorGetGraph es : r), editorSetGraph g es)
      (nur, nes) = apply ur es
  writeIORef changes nur
  writeIORef st nes

applyRedo :: IORef ([Graph],[Graph]) -> IORef EditorState -> IO ()
applyRedo changes st = do
  ur <- readIORef changes
  es <- readIORef st
  let apply (u,[]) es = ((u,[]), es)
      apply (u,g:r) es = ((editorGetGraph es : u, r), editorSetGraph g es)
      (nur, nes) = apply ur es
  writeIORef changes nur
  writeIORef st nes

-- Copy / Paste / Cut ----------------------------------------------------------
copySelected :: EditorState -> (Graph, GraphicalInfo)
copySelected  es = (cg',(ngiM',egiM'))
  where
    (ns,eds) = editorGetSelected es
    g = editorGetGraph es
    (ngiM, egiM) = editorGetGI es
    cg = foldl (\g n -> insertNode g n) (emptyGraph "") (sort ns)
    connPairs = map (\e -> applyPair (fromMaybe nullNode . getNodeByID g) (graphGetSrcFunc g e, graphGetDstFunc g e)) (sort eds)
    cg' = foldl (\g (src,dst) -> insertEdge g src dst) cg connPairs
    ngiM' = M.filterWithKey (\k _ -> k `elem` (map nodeGetID ns)) ngiM
    egiM' = M.filterWithKey (\k _ -> k `elem` (map edgeGetID eds)) egiM

pasteClipBoard :: (Graph, GraphicalInfo) -> EditorState -> EditorState
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

diagrUnion :: (Graph, GraphicalInfo) -> (Graph, GraphicalInfo) -> (Graph, GraphicalInfo)
diagrUnion (g1,(ngiM1,egiM1)) (g2,(ngiM2,egiM2)) = (g3,(ngiM3,egiM3))
  where
    g3 = graphUnion g1 g2
    maxNid = let ns = graphGetNodes g1 in if null ns then 0 else nodeGetID $ maximum ns
    maxEid = let es = graphGetEdges g1 in if null es then 0 else edgeGetID $ maximum es
    nsG2 = graphGetNodes g2
    esG2 = graphGetEdges g2
    newNids = map (+maxNid) (let l = length nsG2 in [1..l])
    ngiM2' = M.fromList $ zip newNids (M.elems ngiM2)
    ngiM3 = M.union ngiM1 ngiM2'
    newEids = map (+maxEid) (let l = length esG2 in [1..l])
    egiM2' = M.fromList $ zip newEids (M.elems egiM2)
    egiM3 = M.union egiM1 egiM2'



-- Tarefas ---------------------------------------------------------------------
-- *Estilos diferentes para as arestas
-- *Espaçar edges quando entre dois nodos ouver mais de uma aresta e ela estiver centralizada
-- *Criar um editor de subgrafos

-- Progresso -------------------------------------------------------------------


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
