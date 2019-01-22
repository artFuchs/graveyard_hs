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
import Graph
import GraphicalInfo
import Render
import Helper

-- | estado do editor de grafos
--   contém todas as informações necssárias para desenhar o grafo
-- (grafo, nodos selecionados, arestas selecionadas, zoom, Pan)
type EditorState = (Graph, [Node], [Edge], Double, (Double,Double))

editorGetGraph :: EditorState -> Graph
editorGetGraph (g,_,_,_,_) = g

editorSetGraph :: Graph -> EditorState -> EditorState
editorSetGraph g (_,n,e,z,p) = (g,n,e,z,p)

editorGetSelected :: EditorState -> ([Node], [Edge])
editorGetSelected (_,n,e,_,_) = (n,e)

editorSetSelected :: ([Node], [Edge]) -> EditorState -> EditorState
editorSetSelected (n,e) (g,_,_,z,p) = (g,n,e,z,p)

editorGetSelectedNodes :: EditorState -> [Node]
editorGetSelectedNodes (_,n,_,_,_) = n

editorSetSelectedNodes :: [Node] -> EditorState -> EditorState
editorSetSelectedNodes n (g,_,e,z,p) = (g,n,e,z,p)

editorGetSelectedEdges :: EditorState -> [Edge]
editorGetSelectedEdges (_,_,e,_,_) = e

editorGetZoom :: EditorState -> Double
editorGetZoom (_,_,_,z,_) = z

editorSetZoom :: Double -> EditorState -> EditorState
editorSetZoom z (g,n,e,_,p) = (g,n,e,z,p)

editorGetPan :: EditorState -> (Double,Double)
editorGetPan (_,_,_,_,p) = p

editorSetPan :: (Double,Double) -> EditorState -> EditorState
editorSetPan p (g,n,e,z,_) = (g,n,e,z,p)


buildMaybeMenubar = do
    fma <- actionNew "FMA" "File" Nothing Nothing
    opn <- actionNew "OPN" "Open File" (Just "Just a stub") (Just stockOpen)
    svn <- actionNew "SVN" "Save File" (Just "Just a stub") (Just stockSave)
    edt <- actionNew "EDT" "Edit" Nothing Nothing
    udo <- actionNew "UDO" "Undo" (Just "Just a stub") (Just stockUndo)
    rdo <- actionNew "RDO" "Redo" (Just "Just a stub") (Just stockRedo)
    agr <- actionGroupNew "AGR"
    mapM_ (actionGroupAddAction agr) [fma,edt]
    mapM_ (\act -> actionGroupAddActionWithAccel agr act (Nothing :: Maybe String)) [opn,svn,udo,rdo]

    ui <- uiManagerNew
    uiManagerAddUiFromString ui uiStr
    uiManagerInsertActionGroup ui agr 0
    maybeMenubar <- uiManagerGetWidget ui "/ui/menubar"
    return (maybeMenubar, opn, svn, udo, rdo)

  where uiStr = "<ui>\
\                 <menubar>\
\                   <menu action=\"FMA\">\
\                     <menuitem action=\"OPN\"/>\
\                     <menuitem action=\"SVN\"/>\
\                   </menu> \
\                   <menu action=\"EDT\">\
\                     <menuitem action=\"UDO\"/>\
\                     <menuitem action=\"RDO\"/>\
\                   </menu>\
\                 </menubar>\
\                </ui>"




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
  vBoxMain <- vBoxNew False 8
  containerAdd window vBoxMain

  -- cria o menu
  (maybeMenubar,opn,svn,udo,rdo) <- buildMaybeMenubar
  case maybeMenubar of
    Just x -> boxPackStart vBoxMain x PackNatural 0
    Nothing -> return ()

  -- cria um HPane para dividir o canvas e o menu de propriedade
  hPaneAction <- hPanedNew
  boxPackStart vBoxMain hPaneAction PackGrow 0

  -- cria uma VBox para adicionar o menu de propriedades
  vBoxProps <- vBoxNew False 8
  panedPack2 hPaneAction vBoxProps False True
  -- cria a label de titulo
  titleLabel <- labelNew $ Just "Propriedades"
  boxPackStart vBoxProps titleLabel PackNatural 0
  -- cria uma HBox para a propriedade ID
  hBoxID <- hBoxNew False 8
  boxPackStart vBoxProps hBoxID PackNatural 0
  labelID <- labelNew $ Just "ID: "
  boxPackStart hBoxID labelID PackNatural 0
  entryNodeID <- entryNew
  boxPackStart hBoxID entryNodeID PackGrow 0
  widgetSetCanFocus entryNodeID False
  set entryNodeID [ entryEditable := False ]
  -- cria uma HBox para a propriedade nome
  hBoxName <- hBoxNew False 8
  boxPackStart vBoxProps hBoxName PackNatural 0
  labelName <- labelNew $ Just "Nome: "
  boxPackStart hBoxName labelName PackNatural 0
  entryNodeName <- entryNew
  boxPackStart hBoxName entryNodeName PackGrow 0
  widgetSetCanFocus entryNodeName True
  -- cria uma HBox para a propriedade cor
  hBoxColor <- hBoxNew False 8
  boxPackStart vBoxProps hBoxColor PackNatural 0
  labelColor <- labelNew $ Just "Cor: "
  boxPackStart hBoxColor labelColor PackNatural 0
  colorBtn <- colorButtonNew
  boxPackStart hBoxColor colorBtn PackNatural 0
  -- cria uma HBox para a propriedade cor da linha
  hBoxLineColor <- hBoxNew False 8
  boxPackStart vBoxProps hBoxLineColor PackNatural 0
  labelLineColor <- labelNew $ Just "Cor da linha: "
  boxPackStart hBoxLineColor labelLineColor PackNatural 0
  lineColorBtn <- colorButtonNew
  boxPackStart hBoxLineColor lineColorBtn PackNatural 0
  -- cria um frame contendo uma VBox para a propriedade forma do nodo
  frameShape <- frameNew
  boxPackStart vBoxProps frameShape PackNatural 0
  vBoxShape <- vBoxNew False 8
  containerAdd frameShape vBoxShape
  radioCircle <- radioButtonNewWithLabel "Circulo"
  boxPackStart vBoxShape radioCircle PackGrow 0
  radioRect <- radioButtonNewWithLabelFromWidget radioCircle "Retangulo"
  boxPackStart vBoxShape radioRect PackGrow 0
  radioQuad <- radioButtonNewWithLabelFromWidget radioCircle "Quadrado"
  boxPackStart vBoxShape radioQuad PackGrow 0
  let radioShapes = [radioCircle, radioRect, radioQuad]
  -- cria uma VBox contendo labels para depurar
  vBoxDebug <- vBoxNew False 8
  boxPackStart vBoxProps vBoxDebug PackNatural 0
  labelPosition <- labelNew $ Just "Posicao: (--,--)"
  labelDims <- labelNew $ Just "Dimensoes: (--,--)"
  boxPackStart vBoxDebug labelPosition PackGrow 0
  boxPackStart vBoxDebug labelDims PackGrow 0

  -- cria um canvas em branco
  canvas <- drawingAreaNew
  panedPack1 hPaneAction canvas True True
  widgetSetCanFocus canvas True
  widgetAddEvents canvas [AllEventsMask]
  --widgetModifyBg canvas StateNormal (Color 65535 65535 65535) -- parece que não funciona
  widgetGrabFocus canvas

  -- mostra a GUI
  widgetShowAll window

  -- inicializa estado do editor -----------------------------------------------
  st <- newIORef (emptyGraph "Vazio", [], [], 1.0, (0.0,0.0)) -- estado do editor: todas as informações necessárias para desenhar o grafo
  oldPoint <- newIORef (0.0,0.0) -- ultimo ponto em que o botão do mouse foi pressionado
  squareSelection <- newIORef Nothing -- estado da caixa de seleção - Maybe (x,y,w,h)
  changes <- newIORef ([],[]) -- pilhas de undo/redo - ([Graph],[Graph])
  movingGI <- newIORef False -- se o usuario começou a mover algum objeto



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
        (x',y') = ((x-px)/z, (y-py)/z)
    liftIO $ do
      writeIORef oldPoint (x',y')
      widgetGrabFocus canvas
    case b of
      -- clique com o botão esquerdo: seleciona nodos e edges
      LeftButton  -> liftIO $ do

        let (oldSN,oldSE) = editorGetSelected es
            graph = editorGetGraph es
            sNode = checkSelectNode graph (x',y')
            sEdge = checkSelectEdge graph (x',y')
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
        updatePropMenu (sNode, sEdges) entryNodeID entryNodeName colorBtn lineColorBtn radioShapes (labelPosition, labelDims)
      -- clique com o botão direito: cria nodos e insere edges entre nodos
      RightButton -> liftIO $ do
        let g = editorGetGraph es
            dstNode = checkSelectNode g (x',y')
        context <- widgetGetPangoContext canvas
        stackUndo changes es
        if length dstNode == 0
          then do
            createNode st (x',y') context
          else do
            modifyIORef st (\es -> createEdges es dstNode)
        widgetQueueDraw canvas
        updatePropMenu (dstNode,[]) entryNodeID entryNodeName colorBtn lineColorBtn radioShapes (labelPosition, labelDims)
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
        (x',y') = ((x-px)/z, (y-py)/z)
    case (leftButton, middleButton, sNodes, sEdges) of
      (True, False, [], []) -> liftIO $ do
        modifyIORef squareSelection $ liftM $ (\(a,b,c,d) -> (a,b,x'-a,y'-b))
        sq <- readIORef squareSelection
        widgetQueueDraw canvas
        --print $ "squareSelection: " ++ show sq
      (True, False, n, e) -> liftIO $ do
        modifyIORef st (\es -> moveNode es (ox,oy) (x',y'))
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
        modifyIORef st (editorSetPan (px+x'-ox, py+y'-oy))
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
        case (editorGetSelected es,sq) of
          (([],[]), Just (x,y,w,h)) -> do
            let graph = editorGetGraph es
                sNodes = filter (\n -> let pos = position . nodeGetGI $ n
                                   in pointInsideRectangle pos (x + (w/2), y + (h/2) , abs w, abs h)) $ graphGetNodes graph
                sEdges = filter (\e -> let pos = cPosition . edgeGetGI $ e
                                   in pointInsideRectangle pos (x + (w/2), y + (h/2), abs w, abs h)) $ graphGetEdges graph
            modifyIORef st (editorSetGraph graph . editorSetSelected (sNodes, sEdges))
          ((n,e), Nothing) -> modifyIORef st (adjustEdges)
          (_,_) -> return ()
      _ -> return ()
    liftIO $ do
      writeIORef squareSelection Nothing
      widgetQueueDraw canvas

    return True

  -- teclado
  canvas `on` keyPressEvent $ do
    k <- eventKeyName
    ms <- eventModifierAll
    liftIO $ do
      pos <- readIORef oldPoint
      context <- widgetGetPangoContext canvas
      case (Control `elem` ms, Shift `elem` ms, T.unpack $ T.toLower k) of
        (False,False,"delete") -> do
          es <- readIORef st
          modifyIORef st (\es -> deleteSelected es)
          stackUndo changes es
          widgetQueueDraw canvas
        (True,_,"plus") -> do
          modifyIORef st (\es -> editorSetZoom (editorGetZoom es * 1.1) es )
          widgetQueueDraw canvas
        (True,_,"minus") -> do
          modifyIORef st (\es -> editorSetZoom (editorGetZoom es * 0.9) es )
          widgetQueueDraw canvas
        (True,_,"equal") -> do
          modifyIORef st (\es -> editorSetZoom 1.0 es )
          widgetQueueDraw canvas
        (True, True, "a") -> do
          modifyIORef st $ editorSetSelected ([],[])
          widgetQueueDraw canvas
        (True, False, "a") -> do
          modifyIORef st (\es -> let g = editorGetGraph es
                                     nodes = graphGetNodes g
                                     edges = graphGetEdges g
                                 in editorSetSelected (nodes,edges) es)
          widgetQueueDraw canvas
        (True, False, "s") -> do
          es <- readIORef st
          let g = editorGetGraph es
          saveGraph g window
        (True, False, "o") -> do
          mg <- loadGraph window
          case mg of
            Just g -> do
              writeIORef st (g,[],[],1.0,(0.0,0.0))
              widgetQueueDraw canvas
            _      -> return ()
        (True, False, "z") -> do
          applyUndo changes st
          widgetQueueDraw canvas
        (True, False, "r") -> do
          applyRedo changes st
          widgetQueueDraw canvas
        _       -> return ()

    return True

  -- tratamento de eventos -- menu toolbar -------------------------------------
  opn `on` actionActivated $ do
    mg <- loadGraph window
    case mg of
      Just g -> do
        writeIORef st (g,[],[],1.0,(0.0,0.0))
        widgetQueueDraw canvas
      _      -> return ()

  svn `on` actionActivated $ do
    es <- readIORef st
    let g = editorGetGraph es
    saveGraph g window

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
        changeColor = (\n -> let nid = nodeGetID n
                                 c = nodeGetInfo n
                                 gi = nodeGiSetColor col $ nodeGetGI n
                             in Node nid c gi)
        newNodes = map changeColor nodes
        newGraph = foldl (\g n -> changeNode g n) graph newNodes
    modifyIORef st (\es -> editorSetGraph newGraph . editorSetSelectedNodes newNodes $ es)
    widgetQueueDraw canvas

  onColorSet lineColorBtn $ do
    Color r g b <- colorButtonGetColor lineColorBtn
    es <- readIORef st
    let color = ((fromIntegral r)/65535, (fromIntegral g)/65535, (fromIntegral b)/65535)
        graph = editorGetGraph es
        (nodes,edges) = editorGetSelected es
        changeNLC = (\n -> let nid = nodeGetID n
                               c = nodeGetInfo n
                               gi = nodeGiSetLineColor color $ nodeGetGI n
                           in Node nid c gi )
        newNodes = map changeNLC nodes
        changeELC = (\e -> let eid = edgeGetID e
                               c = edgeGetInfo e
                               gi = edgeGiSetColor color $ edgeGetGI e
                           in Edge eid c gi )
        newEdges = map changeELC edges
        newGraph = foldl (\g n -> changeNode g n) graph newNodes
        newGraph' = foldl (\g e -> changeEdge g e) newGraph newEdges
    modifyIORef st (\es -> editorSetGraph newGraph . editorSetSelected (newNodes,newEdges) $ es)
    widgetQueueDraw canvas

  radioCircle `on` toggled $ do
    modifyIORef st (\es -> changeNodeShape es NCircle)
    widgetQueueDraw canvas

  radioRect `on` toggled $ do
    modifyIORef st (\es -> changeNodeShape es NRect)
    widgetQueueDraw canvas

  radioQuad `on` toggled $ do
    modifyIORef st (\es -> changeNodeShape es NQuad)
    widgetQueueDraw canvas


  -- tratamento de eventos - janela principal ---------------------------------
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  mainGUI



-- Callbacks -------------------------------------------------------------------
-- atualização do menu de propriedades -----------------------------------------
updatePropMenu :: ([Node],[Edge]) -> Entry -> Entry -> ColorButton -> ColorButton -> [RadioButton] -> (Label,Label) -> IO()
updatePropMenu (nodes,edges) entryID entryName colorBtn lcolorBtn radioShapes (lPos, lDims) = do
  case (length nodes, length edges) of
    (0, 0) -> do
      entrySetText entryID ""
      entrySetText entryName ""
      colorButtonSetColor colorBtn $ Color 49151 49151 49151
      colorButtonSetColor lcolorBtn $ Color 49151 49151 49151
      labelSetText lPos "Posiçao: --"
      labelSetText lDims "Dimensoes: --"
    (1, 0) -> do
      let iD = show . nodeGetID $ (nodes!!0)
          name = nodeGetInfo $ (nodes!!0)
          (r,g,b) = fillColor . nodeGetGI $ (nodes!!0)
          (r',g',b') = lineColor . nodeGetGI $ (nodes!!0)
          nodeColor = Color (round (r*65535)) (round (g*65535)) (round (b*65535))
          nodeLineC = Color (round (r'*65535)) (round (g'*65535)) (round (b'*65535))
          nodeShape = shape . nodeGetGI $ (nodes!!0)
      entrySetText entryID iD
      entrySetText entryName name
      colorButtonSetColor colorBtn nodeColor
      colorButtonSetColor lcolorBtn nodeLineC
      case nodeShape of
        NCircle -> toggleButtonSetActive (radioShapes!!0) True
        NRect -> toggleButtonSetActive (radioShapes!!1) True
        NQuad -> toggleButtonSetActive (radioShapes!!2) True
      let nodePos = position . nodeGetGI $ (nodes!!0)
          nodeDims = dims . nodeGetGI $ (nodes!!0)
      labelSetText lPos ("Posiçao: " ++ (show nodePos))
      labelSetText lDims ("Dimensoes: " ++ (show nodeDims))

    (0, 1) -> do
      let iD = show . edgeGetID $ (edges!!0)
          name = edgeGetInfo $ (edges!!0)
          (r,g,b) = color . edgeGetGI $ (edges!!0)
          edgeColor = Color (round (r*65535)) (round (g*65535)) (round (b*65535))
      entrySetText entryID iD
      entrySetText entryName name
      colorButtonSetColor lcolorBtn edgeColor
    _ -> do
      entrySetText entryID "--"
      entrySetText entryName "----"
      colorButtonSetColor colorBtn $ Color 49151 49151 49151
      colorButtonSetColor lcolorBtn $ Color 49151 49151 49151
      labelSetText lPos "Posiçao: --"
      labelSetText lDims "Dimensoes: --"

-- salvar grafo ----------------------------------------------------------------
saveGraph :: Graph -> Window -> IO ()
saveGraph g window = do
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
           tentativa <- E.try (writeFile path $ show g)  :: IO (Either E.IOException ())
           case tentativa of
             Left _ -> print "Não foi possível escrever no arquivo"
             Right _ -> return ()
           widgetDestroy saveD
    _  -> widgetDestroy saveD

-- abrir grafo -----------------------------------------------------------------
loadGraph :: Window -> IO (Maybe Graph)
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
              print "Não foi possivel ler o arquivo"
              return Nothing
            Right content -> do
              widgetDestroy loadD
              --mapM (print . words) (lines content)
              --return Nothing
              return $ Just $ string2graph content
    _               -> do
      widgetDestroy loadD
      return Nothing

-- atualização do desenho do grafo ---------------------------------------------
drawGraph :: EditorState -> Maybe (Double,Double,Double,Double) -> DrawingArea -> Render ()
drawGraph (g, sNodes, sEdges, z, (px,py)) sq canvas = do
  context <- liftIO $ widgetGetPangoContext canvas
  translate px py
  scale z z
  forM (graphGetEdges g) (\e -> do
    let dstN = getDstNode g e
        srcN = getSrcNode g e
        selected = e `elem` sEdges
    case (srcN, dstN) of
      (Just src, Just dst) -> renderEdge e selected src dst context
      (_, _) -> return ())
  forM (graphGetNodes g) (\n -> renderNode n (n `elem`sNodes) context)
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
checkSelectNode:: Graph -> (Double,Double) -> [Node]
checkSelectNode g (x,y) = case find (\n -> isSelected n) $ graphGetNodes g of
                            Just a -> [a]
                            Nothing -> []
  where isSelected = (\n -> let (nx,ny) = position . nodeGetGI $ n
                                (w,h) = dims . nodeGetGI $ n
                                l = max w h
                            in case shape . nodeGetGI $ n of
                              NCircle -> pointDistance (x,y) (nx,ny) < l/2
                              NRect -> pointInsideRectangle (x,y) (nx,ny,w,h)
                              NQuad -> pointInsideRectangle (x,y) (nx,ny,l,l) )

-- verifica se o usuario selecionou alguma aresta
checkSelectEdge:: Graph -> (Double,Double) -> [Edge]
checkSelectEdge g (x,y) = case find (\e -> isSelected e) $ graphGetEdges g of
                            Just a -> [a]
                            Nothing -> []
  where isSelected = (\e -> pointDistance (x,y) (cPosition . edgeGetGI $ e) < 5)

-- move os nodos selecionados
moveNode:: EditorState -> (Double,Double) -> (Double,Double) -> EditorState
moveNode es (xold,yold) (xnew,ynew) = editorSetGraph newGraph' . editorSetSelected (movedNodes, sEdges) $ es
  where
      (sNodes, sEdges) = editorGetSelected es
      graph = editorGetGraph es
      (deltaX, deltaY) = (xnew-xold, ynew-yold)
      -- move nodes
      moveN = (\node -> let gi = nodeGetGI node
                            (nox, noy)  = position gi
                            newPos  = (nox+deltaX, noy+deltaY)
                         in Node (nodeGetID node) (nodeGetInfo node) (nodeGiSetPosition newPos gi) )
      movedNodes = map moveN sNodes
      -- move as arestas que estão no ponto entre os nodos
      moveE = (\edge -> let nullNode = Node 0 "" newNodeGI
                            a = fromMaybe nullNode $ getSrcNode graph edge
                            b = fromMaybe nullNode $ getDstNode graph edge
                            getMovedNode = (\node -> fromMaybe node $ find (\n -> n == node) movedNodes)
                            aPos = position. nodeGetGI $ a
                            bPos = position. nodeGetGI $ b
                            newAPos = position. nodeGetGI $ getMovedNode a
                            newBPos = position. nodeGetGI $ getMovedNode b
                            (xe,ye) = cPosition . edgeGetGI $ edge
                            (xe',ye') = (xe+deltaX, ye+deltaY)
                            gi = edgeGetGI edge
                        in case (a == b, any (\n -> n `elem` sNodes) [a,b], centered gi) of
                            (True, True, _) -> Edge (edgeGetID edge) (edgeGetInfo edge) $ edgeGiSetPosition (xe',ye') gi
                            (False, True, True) -> Edge (edgeGetID edge) (edgeGetInfo edge) $ edgeGiSetPosition (midPoint newAPos newBPos) gi
                            _ -> edge
                          )
      movedEdges = map moveE (graphGetEdges graph)
      -- atualiza o grafo
      mvng = (\g n -> changeNode g n)
      mveg = (\g e -> changeEdge g e)
      newSEdges = map (\edge -> fromMaybe edge $ find (==edge) movedEdges ) sEdges
      newGraph = foldl mvng graph movedNodes
      newGraph' = foldl mveg newGraph movedEdges

-- move as arestas selecionadas
moveEdges:: EditorState -> (Double,Double) -> (Double,Double) -> EditorState
moveEdges es (xold,yold) (xnew,ynew) = editorSetGraph newGraph . editorSetSelected (sNodes, movedEdges) $ es
  where graph = editorGetGraph es
        (sNodes,sEdges) = editorGetSelected es
        (deltaX, deltaY) = (xnew-xold,ynew-yold)
        moveE = (\edge-> let gi = edgeGetGI edge
                             (xe, ye) = cPosition gi
                             newPos = (xe+deltaX, ye+deltaY)
                             nullNode = Node 0 "" newNodeGI
                             srcPos = position . nodeGetGI . fromMaybe nullNode $ getSrcNode graph edge
                             dstPos = position . nodeGetGI . fromMaybe nullNode $ getDstNode graph edge
                             mustCenter = pointLineDistance newPos srcPos dstPos < 10
                         in Edge (edgeGetID edge) (edgeGetInfo edge) (edgeGiSetCentered mustCenter . edgeGiSetPosition newPos $ gi))
        movedEdges = map moveE sEdges
        mvg = (\g e -> changeEdge g e)
        newGraph = foldl mvg graph movedEdges

-- ajusta a posição das edges selecionadas caso a propriedade centered seja True
adjustEdges:: EditorState -> EditorState
adjustEdges es = editorSetGraph newGraph es
  where graph = editorGetGraph es
        adjust = (\e -> let nullNode = Node 0 "" newNodeGI
                            srcPos = position . nodeGetGI . fromMaybe nullNode $ getSrcNode graph e
                            dstPos = position . nodeGetGI . fromMaybe nullNode $ getDstNode graph e
                        in if centered (edgeGetGI e)
                          then Edge (edgeGetID e) (edgeGetInfo e) (edgeGiSetCentered True . edgeGiSetPosition (midPoint srcPos dstPos) $ edgeGetGI e)
                          else e)
        newEdges = map adjust (editorGetSelectedEdges es)
        newGraph = foldl (\g e -> changeEdge g e) graph newEdges



-- operações básicas sobre o grafo no estado -----------------------------------
-- cria um novo nodo e insere no grafo
createNode:: IORef EditorState -> (Double,Double) -> PangoContext -> IO ()
createNode st pos context = do
  es <- readIORef st
  let graph = editorGetGraph es
      (nodes, edges) = editorGetSelected es
      maxnode = if length (graphGetNodes graph) > 0
                  then maximum (graphGetNodes graph)
                  else Node 0 "" newNodeGI
      nID = 1 + nodeGetID maxnode
      content = ("node " ++ show nID)
  dim <- getStringDims content context
  let newNode = Node nID content $ nodeGiSetDims dim . nodeGiSetPosition pos $ newNodeGI
      newGraph = insertNode graph newNode
  writeIORef st $ editorSetGraph newGraph . editorSetSelected ([newNode], []) $ es

-- cria e insere uma nova edge no grafo
createEdges:: EditorState -> [Node] -> EditorState
createEdges es dstNodes = editorSetGraph newGraph . editorSetSelected (dstNodes,[]) $ es
  where selectedNodes = editorGetSelectedNodes es
        graph = editorGetGraph es
        newGraph = if length dstNodes == 1
                    then foldl (\g n -> insertEdge g n (dstNodes!!0)) graph selectedNodes
                    else graph

-- deleta os nodos e arestas selecionados no grafo
deleteSelected:: EditorState -> EditorState
deleteSelected es = editorSetSelected ([],[]) . editorSetGraph newGraph $ es
  where graph = editorGetGraph es
        (nodes,edges) = editorGetSelected es
        graph' = foldl (\g n -> removeNode g n) graph nodes
        newGraph = foldl (\g e -> removeEdge g e) graph' edges

-- renomeia os selecionados
renameSelected:: IORef EditorState -> String -> PangoContext -> IO()
renameSelected state name context = do
  es <- readIORef state
  dim <- getStringDims name context
  let graph = editorGetGraph es
      (nodes,edges) = editorGetSelected es
  let renamedNodes = map (\n -> Node (nodeGetID n) name (nodeGiSetDims dim . nodeGetGI $ n)) nodes
      renamedEdges = map (\e -> Edge (edgeGetID e) name $ edgeGetGI e) edges
      newGraph = foldl (\g n -> changeNode g n) graph renamedNodes
      newGraph' = foldl (\g e -> changeEdge g e) newGraph renamedEdges
      newEs = editorSetGraph newGraph' . editorSetSelected (renamedNodes, renamedEdges) $ es
  writeIORef state newEs

-- muda a forma de um nodo
changeNodeShape :: EditorState -> NodeShape -> EditorState
changeNodeShape es s = editorSetGraph newGraph . editorSetSelectedNodes newNodes $ es
  where
      nodes = editorGetSelectedNodes es
      newNodes = map (\n -> Node (nodeGetID n) (nodeGetInfo n) (nodeGiSetShape s $ nodeGetGI n)) nodes
      newGraph = foldl (\g n -> changeNode g n) (editorGetGraph es) newNodes

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
  putStrLn "undo"

applyRedo :: IORef ([Graph],[Graph]) -> IORef EditorState -> IO ()
applyRedo changes st = do
  ur <- readIORef changes
  es <- readIORef st
  let apply (u,[]) es = ((u,[]), es)
      apply (u,g:r) es = ((editorGetGraph es : u, r), editorSetGraph g es)
      (nur, nes) = apply ur es
  writeIORef changes nur
  writeIORef st nes





-- To Do List ------------------------------------------------------------------
-- *Estilos diferentes para as Edges
-- *Melhorar menu de Propriedades
--  *3 aparencias diferentes para nodos, edges e nodos+edges
--  *Fazer update do menu quando um nodo for criado
-- *Copy/Paste/Cut
-- *New File
-- *Espaçar edges quando entre dois nodos ouver mais de uma edge e ela estiver centralizada
-- *Criar nodos com base na escolha do menu
-- *Separar a estrutura do grafo das estruturas gráficas
