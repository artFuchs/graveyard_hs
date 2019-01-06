import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Layout
import qualified Data.Text as T
import Data.List
import Graph
import GraphicalInfo
import Helper

-- | estado do editor de grafos
type EditorState = (Graph, [Node], [Edge])

editorGetGraph :: EditorState -> Graph
editorGetGraph (g,_,_) = g

editorGetSelected :: EditorState -> ([Node], [Edge])
editorGetSelected (_,n,e) = (n,e)

editorGetSelectedNodes :: EditorState -> [Node]
editorGetSelectedNodes (_,n,_) = n

editorGetSelectedEdges :: EditorState -> [Edge]
editorGetSelectedEdges (_,_,e) = e






main :: IO()
main = do
  -- inicializa a biblioteca GTK
  initGUI

  -- cria a janela principal
  window <- windowNew
  set window  [ windowTitle         := "Graph Editor"
              , windowDefaultWidth  := 640
              , windowDefaultHeight := 480]

  -- cria um HPane para dividir o canvas e o menu de propriedades
  hPaneAction <- hPanedNew
  containerAdd window hPaneAction

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
  -- -- cria uma HBox para a propriedade edges
  -- hBoxEdges <- hBoxNew False 8
  -- boxPackStart vBoxProps hBoxEdges PackNatural 0
  -- labelEdges <- labelNew $ Just "Edges: "
  -- boxPackStart hBoxEdges labelEdges PackNatural 0
  -- labelEdges' <- labelNew $ Just " "
  -- boxPackStart hBoxEdges labelEdges' PackNatural 0




  -- cria um canvas em branco
  canvas <- drawingAreaNew
  panedPack1 hPaneAction canvas True True
  widgetSetCanFocus canvas True
  widgetAddEvents canvas [AllEventsMask]
  widgetModifyBg canvas StateNormal (Color 65535 65535 65535) -- parece que não funciona
  widgetGrabFocus canvas

  -- mostra a GUI
  widgetShowAll window

  -- inicializa estado
  -- (Graph, nodos selecionados, arestas selecionadas)
  st <- newIORef (graph1, [], [])
  oldPoint <- newIORef (0.0,0.0)

  -- TRATAMENTO DE EVENTOS -----------------------------------------------------
  -- tratamento de eventos - canvas --------------------------------------------
  -- evento de desenho
  canvas `on` draw $ do
    es <- liftIO $ readIORef st
    drawGraph es canvas

  -- clique do mouse
  canvas `on` buttonPressEvent $ do
    b <- eventButton
    (x,y) <- eventCoordinates
    ms <- eventModifierAll
    liftIO $ do
      writeIORef oldPoint (x,y)
      widgetGrabFocus canvas
    case b of
      -- clique com o botão esquerdo: seleciona nodos
      LeftButton  -> liftIO $ do
        es <- readIORef st
        let oldSN = editorGetSelectedNodes es
        checkSelect st (x,y) canvas
        -- Shift: seleção de multiplos elementos
        if Shift `elem` ms
          then do
            es <- readIORef st
            let graph = editorGetGraph es
                sN = editorGetSelectedNodes es
                desselect = filter (\n -> n `elem` oldSN) sN
                jointSN = filter (\n -> n `notElem` desselect) $ oldSN ++ sN
            writeIORef st (graph, jointSN, [])
          else return ()
        widgetQueueDraw canvas
        es' <- liftIO $ readIORef st
        let graph = editorGetGraph es'
            sN = editorGetSelectedNodes es'
        updatePropMenu sN entryNodeID entryNodeName colorBtn
        --labelSetText labelEdges' $ if (length selected == 1) then foldl (\s e -> s ++ (show e) ++ ", ") "" (getConnectors graph (selected!!0)) else ""
      -- clique com o botão direito: insere edges entre nodos
      RightButton -> liftIO $ do
        es <- liftIO $ readIORef st
        checkSelect st (x,y) canvas
        es' <- liftIO $ readIORef st
        let selectedNodes = editorGetSelectedNodes es
            graph = editorGetGraph es'
            newSelectedNode = editorGetSelectedNodes es'
        if Shift `elem` ms
          then do
            let nodeConnectors = if length newSelectedNode == 1
                                    then getConnectors graph (newSelectedNode!!0)
                                    else []
                otherConnectors = concat . map (\n -> getConnectors graph n) $ selectedNodes
                targetConnectors = filter (\e -> e `elem` nodeConnectors) otherConnectors
                newGraph = foldl (\g e -> removeEdge g e) graph targetConnectors
            writeIORef st (newGraph, newSelectedNode, [])
          else do
            let newGraph = if length newSelectedNode == 1
                             then foldl (\g n -> insertEdge g n (newSelectedNode!!0)) graph selectedNodes
                             else graph
            writeIORef st (newGraph, newSelectedNode, [])

        widgetQueueDraw canvas
        updatePropMenu newSelectedNode entryNodeID entryNodeName colorBtn
      _           -> return ()

    return True

  -- movimento do mouse
  canvas `on` motionNotifyEvent $ do
    ms <- eventModifierAll
    (x,y) <- eventCoordinates
    (ox,oy) <- liftIO $ readIORef oldPoint
    let leftButton = Button1 `elem` ms
        rightButton = Button2 `elem` ms
    if leftButton
      then liftIO $ do
        moveNode st (ox,oy) (x,y)
        writeIORef oldPoint (x,y)
        widgetQueueDraw canvas
        else return ()
    return True

  -- teclado
  canvas `on` keyPressEvent $ do
    k <- eventKeyName
    liftIO $ do
      pos <- readIORef oldPoint
      case T.unpack k of
        "Insert" -> do
          createNode st pos
          widgetQueueDraw canvas
        "Delete" -> do
          deleteNode st
          widgetQueueDraw canvas
        _       -> return ()

    return True

  -- tratamento de eventos -- menu de propriedades -----------------------------
  entryNodeName `on` keyPressEvent $ do
    k <- eventKeyName
    liftIO $ do
      case T.unpack k of
        "Return" -> do
          name <- entryGetText entryNodeName :: IO String
          renameNode st name
          widgetQueueDraw canvas
        _       -> return ()
    return False

  -- tratamento de eventos - janela principal ---------------------------------
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  mainGUI



-- Callbacks -------------------------------------------------------------------
-- atualização do menu de propriedades -----------------------------------------
updatePropMenu :: [Node] -> Entry -> Entry -> ColorButton-> IO()
updatePropMenu selected entryID entryName colorBtn = do
  case length selected of
    0 -> do
      entrySetText entryID ""
      entrySetText entryName ""
      colorButtonSetColor colorBtn $ Color 49151 49151 49151
    1 -> do
      let iD = show . nodeGetID $ (selected!!0)
          name = infoGetContent . nodeGetInfo $ (selected!!0)
          (r,g,b) = color . infoGetGraphicalInfo . nodeGetInfo $ (selected!!0)
          nodeColor = Color (round (r*65535)) (round (g*65535)) (round (b*65535))
      entrySetText entryID iD
      entrySetText entryName name
      colorButtonSetColor colorBtn nodeColor

    _ -> do
      entrySetText entryID "--"
      entrySetText entryName "----"

-- atualização do desenho do grafo --------------------------------------------
drawGraph :: EditorState -> DrawingArea -> Render ()
drawGraph state canvas = do
  let (g, sNodes, _) = state
  context <- liftIO $ widgetGetPangoContext canvas
  forM (graphGetNodes g) (\n -> renderNode n (n `elem`sNodes) context)
  forM (graphGetEdges g) (\e -> do
    let dstN = getDstNode g e
        srcN = getSrcNode g e
    case (srcN, dstN) of
      (Just src, Just dst) -> renderEdge e False src dst context
      (Nothing, _) -> return ()
      (_, Nothing) -> return ())
  return ()


-- desenha um nodo, com seu texto
renderNode :: Node -> Bool -> PangoContext -> Render ()
renderNode node selected context = do
  let (x,y) = position . infoGetGraphicalInfo . nodeGetInfo $ node
      (r,g,b) = if selected
                  then (0,0,1)
                  else color . infoGetGraphicalInfo . nodeGetInfo $ node
      (rl,gl,bl) = if selected
                    then (1,1,1)
                    else lineColor . infoGetGraphicalInfo . nodeGetInfo $ node
      content = infoGetContent . nodeGetInfo $ node
      offset = 3

  pL <- liftIO $ layoutText context content
  (_,PangoRectangle px py pw ph) <- liftIO $ layoutGetExtents pL

  setSourceRGB r g b
  rectangle (x-(offset/2 + pw/2)) (y-(offset/2 + ph/2)) (offset+pw) (offset+ph)
  fill

  setSourceRGB rl gl bl
  rectangle (x-(offset/2 + pw/2)) (y-(offset/2 + ph/2)) (offset+pw) (offset+ph)
  stroke

  setSourceRGB rl gl bl
  moveTo (x-(pw/2)) (y-(ph/2))
  showLayout pL

-- desenha uma aresta
renderEdge :: Edge -> Bool -> Node -> Node -> PangoContext -> Render ()
renderEdge edge selected nodeSrc nodeDst context = do
  setSourceRGB 0 0 0

  -- utiliza a biblioteca Pango para calcular o tamanho da bounding box do texto
  let content = infoGetContent . nodeGetInfo $ nodeSrc
      content2 = infoGetContent . nodeGetInfo $ nodeDst
  pL <- liftIO $ layoutText context content
  pL2 <- liftIO $ layoutText context content2
  (_,PangoRectangle px py pw ph) <- liftIO $ layoutGetExtents pL
  (_,PangoRectangle px2 py2 pw2 ph2) <- liftIO $ layoutGetExtents pL2

  -- calculo dos pontos de origem e destino da aresta
  let (x1,y1) = position . infoGetGraphicalInfo . nodeGetInfo $ nodeSrc
      (x2,y2) = position . infoGetGraphicalInfo . nodeGetInfo $ nodeDst
      (xe, ye) = position . infoGetGraphicalInfo . edgeGetInfo $ edge
      d1 = pointDistance (x1,y1) (xe,ye)
      d2 = pointDistance (xe,ye) (x2,y2)
      (vx1,vy1) = ((xe-x1)/d1 , (ye-y1)/d1)
      (vx2,vy2) = ((x2-xe)/d2 , (y2-ye)/d2)
      n1 = 3 + (max pw ph)/2
      n2 = 5 + (max pw2 ph2)/2
      (x1', y1') = (x1 + vx1*n1, y1 + vy1*n1)
      (x2', y2') = (x2 - vx2*n2, y2 - vy2*n2)
      -- configurações de cor
      (r,g,b) = if selected then (1,1,1) else lineColor . infoGetGraphicalInfo . edgeGetInfo $ edge

  -- desenha uma linha representando a aresta
  setSourceRGB r g b
  moveTo x1' y1'
  lineTo xe  ye
  lineTo x2' y2'
  stroke
  -- desenha um circulo para indicar qual é o nó de destino
  arc x2' y2' 3 0 (2*pi)
  -- desenha um circulo para mostar o ponto de controle
  arc xe  ye 2 0 (2*pi)
  fill

-- verifica se o usuario selecionou algum nodo ---------------------------------
checkSelect:: IORef EditorState -> (Double,Double) -> DrawingArea -> IO ()
checkSelect state (x,y) canvas = do
  es <- readIORef state
  let g = editorGetGraph es
  context <- widgetGetPangoContext canvas
  maybeSelectedNodes <- forM (graphGetNodes g) $ (\n -> do
    inside <- pointInsideNode n (x,y) context
    if inside
      then return (Just n)
      else return Nothing
    )
  let maybeSelected = find (\n -> case n of
                                Nothing -> False
                                _ -> True) $ reverse maybeSelectedNodes
  case maybeSelected of
    Just (Just a) -> writeIORef state (g,[a],[])
    _ -> writeIORef state (g,[],[])
  return ()

-- move um nodo do grafo -------------------------------------------------------
moveNode:: IORef EditorState -> (Double,Double) -> (Double,Double) -> IO ()
moveNode state (xold,yold) (xnew,ynew) = do
  (graph,nodes, edges) <- readIORef state
  let (deltaX, deltaY) = (xnew-xold, ynew-yold)
  maybeNodes <- forM (graphGetNodes graph) (\node -> do
    if node `elem` nodes
      then let info = nodeGetInfo node
               gi = infoGetGraphicalInfo info
               (nox, noy)  = position gi
               newNodePos  = (nox+deltaX, noy+deltaY)
           in return $ Just (Node (nodeGetID node) (Info (infoGetContent info) (giSetPosition gi newNodePos)))
      else return Nothing)
  let mvg = (\g node -> case node of
                          Just n -> changeNode g n
                          Nothing -> g)
      rmMaybe = (\acc node -> case node of
                                Just n -> n:acc
                                Nothing -> acc)
      newGraph = foldl mvg graph maybeNodes
      newNodes = foldl rmMaybe [] maybeNodes
  writeIORef state (newGraph, newNodes, edges)

-- operações básicas sobre o grafo ---------------------------------------------
-- cria um novo nodo e insere no grafo
createNode:: IORef EditorState -> (Double,Double) -> IO()
createNode state pos = do
  (graph, nodes, _) <- readIORef state
  let maxnode = if length (graphGetNodes graph) > 0
                  then maximum (graphGetNodes graph)
                  else Node 0 $ Info "" newGraphicalInfo
      newID = 1 + nodeGetID maxnode
      newNode = Node newID $ Info ("node " ++ show newID) $ giSetPosition newGraphicalInfo pos
      newGraph = insertNode graph newNode
  writeIORef state (newGraph, [newNode], [])

-- deleta os nodos selecionados no grafo
deleteNode:: IORef EditorState -> IO()
deleteNode state = do
  (graph, nodes, _) <- readIORef state
  let newGraph = foldl (\g n -> removeNode g n) graph nodes
  writeIORef state (newGraph, [], [])

-- renomeia os nodos selecionados
renameNode:: IORef EditorState -> String -> IO()
renameNode state name = do
  (graph, nodes, _) <- readIORef state
  let renamedNodes = map (\n -> Node (nodeGetID n) $ Info name (infoGetGraphicalInfo . nodeGetInfo $ n)) nodes
      newGraph = foldl (\g n -> changeNode g n) graph renamedNodes
  writeIORef state (newGraph,renamedNodes, [])

-- ↓↓↓↓↓ estruturas para teste ↓↓↓↓↓ -------------------------------------------

graph1 :: Graph
graph1 = Graph "1" src1 dst1 edges1 nodes1

nodes1 :: [Node]
nodes1 =  [(Node 1 $ Info "hello" $ giSetPosition newGraphicalInfo (100, 40))
          , (Node 2 $ Info "my" $ giSetPosition newGraphicalInfo (40, 100))
          , (Node 3 $ Info "name" $ giSetPosition newGraphicalInfo (160, 100))
          , (Node 4 $ Info "is" $ giSetPosition newGraphicalInfo (40, 160))
          , (Node 5 $ Info "Mr. Fear" $ giSetPosition newGraphicalInfo (160, 160))
          ]

edges1 :: [Edge]
edges1 =  [(Edge 1 $ Info "" $ giSetPosition newGraphicalInfo ( 70, 70))
          ,(Edge 2 $ Info "" $ giSetPosition newGraphicalInfo (130, 70))
          ,(Edge 3 $ Info "" $ giSetPosition newGraphicalInfo ( 40,130))
          ,(Edge 4 $ Info "" $ giSetPosition newGraphicalInfo (100,130))
          ,(Edge 5 $ Info "" $ giSetPosition newGraphicalInfo (100,160))
          ]

-- 1 -1-> 2
-- 1 -2-> 3
-- 2 -3-> 4
-- 3 -4-> 4
-- 4 -5-> 5
src1 :: Edge -> Int
src1 edge
    | edgeGetID edge == 1 = 1
    | edgeGetID edge == 2 = 1
    | edgeGetID edge == 3 = 2
    | edgeGetID edge == 4 = 3
    | edgeGetID edge == 5 = 4
    | otherwise = 0

dst1 :: Edge -> Int
dst1 edge
    | edgeGetID edge == 1 = 2
    | edgeGetID edge == 2 = 3
    | edgeGetID edge == 3 = 4
    | edgeGetID edge == 4 = 4
    | edgeGetID edge == 5 = 5
    | otherwise = 0
