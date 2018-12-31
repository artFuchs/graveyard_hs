import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Layout
import Data.Text hiding (map)
import Data.List as L
import Graph
import GraphicalInfo

main :: IO()
main = do
  -- inicializa a biblioteca GTK
  initGUI

  -- cria a janela principal
  window <- windowNew
  set window  [ windowTitle         := "Graph Viewer"
              , windowDefaultWidth  := 640
              , windowDefaultHeight := 480]

  -- cria um HPane para dividir o canvas e o menu de propriedades
  hPaneAction <- hPanedNew
  containerAdd window hPaneAction

  -- cria uma VBox para adicionar o menu de propriedades
  vBoxProps <- vBoxNew False 8
  panedPack2 hPaneAction vBoxProps False True
  -- cria uma HBox para a propriedade nome
  hBoxName <- hBoxNew False 8
  boxPackStart vBoxProps hBoxName PackNatural 0
  labelName <- labelNew $ Just "Nome do Nodo: "
  boxPackStart hBoxName labelName PackNatural 0
  entryNodeName <- entryNew
  boxPackStart hBoxName entryNodeName PackGrow 0
  widgetSetCanFocus entryNodeName True

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
  -- (Graph, nodos selecionados)
  st <- newIORef (graph1, [])
  oldPoint <- newIORef (0.0,0.0)

  -- TRATAMENTO DE EVENTOS -----------------------------------------------------
  -- tratamento de eventos - canvas --------------------------------------------
  -- evento de desenho
  canvas `on` draw $ do
    (graph, selected) <- liftIO $ readIORef st
    drawGraph graph selected canvas

  -- clique do mouse
  canvas `on` buttonPressEvent $ do
    b <- eventButton
    (x,y) <- eventCoordinates
    liftIO $ do
      writeIORef oldPoint (x,y)
      widgetGrabFocus canvas
    case b of
      LeftButton  -> liftIO $ do
        checkSelect st (x,y) canvas
        widgetQueueDraw canvas
        (_,selected) <- liftIO $ readIORef st
        updatePropMenu selected entryNodeName
      RightButton -> liftIO $ do
        (_,selectedNodes) <- liftIO $ readIORef st
        checkSelect st (x,y) canvas
        (graph,newSelectedNode) <- liftIO $ readIORef st
        let newGraph = if L.length newSelectedNode == 1
                        then L.foldl (\g n -> insertEdge g n (newSelectedNode!!0)) graph selectedNodes
                        else graph
        writeIORef st (newGraph, newSelectedNode)
        widgetQueueDraw canvas
        updatePropMenu newSelectedNode entryNodeName
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
      case unpack k of
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
      case unpack k of
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

updatePropMenu :: [Node] -> Entry -> IO()
updatePropMenu selected entryName = do
  case L.length selected of
    0 -> do
      entrySetText entryName ""
    1 -> do
      let name = infoGetContent . nodeGetInfo $ (selected!!0)
      entrySetText entryName name
    _ -> do
      entrySetText entryName "----"


drawGraph :: Graph -> [Node] -> DrawingArea -> Render ()
drawGraph g nodes canvas = do
  context <- liftIO $ widgetGetPangoContext canvas
  forM (graphGetNodes g) (\n -> renderNode n nodes context)
  forM (graphGetEdges g) (\e -> do
    let dstN = getDstNode g e
        srcN = getSrcNode g e
    case (srcN, dstN) of
      (Just src, Just dst) -> renderEdge src dst context
      (Nothing, _) -> return ()
      (_, Nothing) -> return ())
  return ()


-- desenha um nodo, com seu texto
renderNode :: Node -> [Node] -> PangoContext -> Render ()
renderNode node selected_nodes context = do
  let selected = L.find (\n -> n == node) selected_nodes  /= Nothing
      (x,y) = position . infoGetGraphicalInfo . nodeGetInfo $ node
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

renderEdge :: Node -> Node -> PangoContext -> Render ()
renderEdge nodeSrc nodeDst context = do
  setSourceRGB 0 0 0

  -- utiliza a biblioteca Pango para calcular o tamanho da bounding box do texto
  let content = infoGetContent . nodeGetInfo $ nodeSrc
      content2 = infoGetContent . nodeGetInfo $ nodeDst
  pL <- liftIO $ layoutText context content
  pL2 <- liftIO $ layoutText context content2
  (_,PangoRectangle px py pw ph) <- liftIO $ layoutGetExtents pL
  (_,PangoRectangle px2 py2 pw2 ph2) <- liftIO $ layoutGetExtents pL2

  -- calcula os pontos de origem e destino da aresta
  let (x1,y1) = position . infoGetGraphicalInfo . nodeGetInfo $ nodeSrc
      (x2,y2) = position . infoGetGraphicalInfo . nodeGetInfo $ nodeDst
      d = sqrt $ (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)
      (vx,vy) = ((x2-x1)/d , (y2-y1)/d)
      n1 = 3 + (max pw ph)/2
      n2 = 5 + (max pw2 ph2)/2
      (x1', y1') = (x1 + vx*n1, y1 + vy*n1)
      (x2', y2') = (x2 - vx*n2, y2 - vy*n2)

  -- desenha uma linha representando a aresta
  moveTo x1' y1'
  lineTo x2' y2'
  stroke
  -- desenha um circulo para indicar qual é o nó de destino
  arc x2' y2' 3 0 (2*pi)
  fill

-- verifica se o usuario selecionou algum nodo
checkSelect:: IORef (Graph, [Node]) -> (Double,Double) -> DrawingArea -> IO ()
checkSelect state (x,y) canvas = do
  (g,s) <- readIORef state
  context <- widgetGetPangoContext canvas
  maybeSelectedNodes <- forM (graphGetNodes g) $ (\n -> do
    inside <- pointInsideNode n (x,y) context
    if inside
      then return (Just n)
      else return Nothing
    )
  let maybeSelected = L.find (\n -> case n of
                                Nothing -> False
                                _ -> True) $ L.reverse maybeSelectedNodes
  case maybeSelected of
    Just (Just a) -> writeIORef state (g,[a])
    _ -> writeIORef state (g,[])
  return ()

--verifica se um ponto está dentro da bounding box de um nodo
--utiliza métodos da biblioteca Pango para isso
pointInsideNode:: Node -> (Double,Double) -> PangoContext -> IO Bool
pointInsideNode node (x,y) context = do
  pL <- layoutText context $ infoGetContent . nodeGetInfo $ node
  (_, PangoRectangle px py pw ph) <- layoutGetExtents pL
  let (nx,ny) = position . infoGetGraphicalInfo . nodeGetInfo $ node
  return $ (x >= nx - pw/2) && (x <= nx + pw/2) && (y >= ny - ph/2) && (y <= ny + ph/2)

-- move um nodo do grafo
moveNode:: IORef (Graph, [Node]) -> (Double,Double) -> (Double,Double) -> IO ()
moveNode state (xold,yold) (xnew,ynew) = do
  (graph,nodes) <- readIORef state
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
      newGraph = L.foldl mvg graph maybeNodes
      newNodes = L.foldl rmMaybe [] maybeNodes
  writeIORef state (newGraph, newNodes)

-- cria um novo nodo e insere no grafo
createNode:: IORef (Graph, [Node]) -> (Double,Double) -> IO()
createNode state pos = do
  (graph, nodes) <- readIORef state
  let maxnode = if L.length (graphGetNodes graph) > 0
                  then L.maximum (graphGetNodes graph)
                  else Node 0 $ Info "" newGraphicalInfo
      newID = 1 + nodeGetID maxnode
      newNode = Node newID $ Info ("node " ++ show newID) $ giSetPosition newGraphicalInfo pos
      newGraph = insertNode graph newNode
  writeIORef state (newGraph, [newNode])

-- deleta os nodos selecionados do grafo
deleteNode:: IORef (Graph, [Node]) -> IO()
deleteNode state = do
  (graph, nodes) <- readIORef state
  let newGRaph = L.foldl (\g n -> removeNode g n) graph nodes
  writeIORef state (newGRaph, [])

renameNode state name = do
  (graph, nodes) <- readIORef state
  let renamedNodes = map (\n -> Node (nodeGetID n) $ Info name (infoGetGraphicalInfo . nodeGetInfo $ n)) nodes
      newGraph = L.foldl (\g n -> changeNode g n) graph renamedNodes
  writeIORef state (newGraph,renamedNodes)

-- ↓↓↓↓↓ estruturas para teste ↓↓↓↓↓ -------------------------------------------

nodes1 :: [Node]
nodes1 =  [(Node 1 $ Info "hello" $ giSetPosition newGraphicalInfo (100, 40))
          , (Node 2 $ Info "my" $ giSetPosition newGraphicalInfo (40, 100))
          , (Node 3 $ Info "name" $ giSetPosition newGraphicalInfo (160, 100))
          , (Node 4 $ Info "is" $ giSetPosition newGraphicalInfo (40, 160))
          , (Node 5 $ Info "Mr. Fear" $ giSetPosition newGraphicalInfo (160, 160))
          ]

graph1 :: Graph
graph1 = Graph "1" src1 dst1 [1,2,3,4,5] nodes1


-- ↓↓↓↓↓ src / dst funções para testar o grafo ↓↓↓↓↓ ---------------------------

-- 1 -1-> 2
-- 1 -2-> 3
-- 2 -3-> 4
-- 3 -4-> 4
-- 4 -5-> 5
src1 :: Int -> Int
src1 edge
    | edge == 1 = 1
    | edge == 2 = 1
    | edge == 3 = 2
    | edge == 4 = 3
    | edge == 5 = 4
    | otherwise = 0

dst1 :: Int -> Int
dst1 edge
    | edge == 1 = 2
    | edge == 2 = 3
    | edge == 3 = 4
    | edge == 4 = 4
    | edge == 5 = 5
    | otherwise = 0
