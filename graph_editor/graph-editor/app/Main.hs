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
import Render
import Helper

-- | estado do editor de grafos
--   (grafo, nodos selecionados, arestas selecionadas)
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

  -- definição da UI -----------------------------------------------------------
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



  -- cria um canvas em branco
  canvas <- drawingAreaNew
  panedPack1 hPaneAction canvas True True
  widgetSetCanFocus canvas True
  widgetAddEvents canvas [AllEventsMask]
  --widgetModifyBg canvas StateNormal (Color 65535 65535 65535) -- parece que não funciona
  widgetGrabFocus canvas

  -- mostra a GUI
  widgetShowAll window

  -- inicializa estado
  st <- newIORef (graph1, [], []) -- estado do editor
  oldPoint <- newIORef (0.0,0.0) -- ultimo ponto em que o mouse esteve com algum botão pressionado

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
      -- clique com o botão esquerdo: seleciona nodos e edges
      LeftButton  -> liftIO $ do
        es <- readIORef st
        let (oldSN,oldSE) = editorGetSelected es
            graph = editorGetGraph es
            sNode = checkSelectNode graph (x,y)
            sEdge = checkSelectEdge graph (x,y)
        -- Shift: seleção de multiplos elementos
        let (sNodes,sEdges) = if Shift `elem` ms
                                then let jointSN = foldl (\ns n -> if n `notElem` ns then n:ns else ns) [] (oldSN ++ sNode)
                                         jointSE = foldl (\es e -> if e `notElem` es then e:es else es) [] (oldSE ++ sEdge)
                                     in (jointSN, jointSE)
                                else (sNode, sEdge)
        writeIORef st (editorGetGraph es, sNodes, sEdges)
        widgetQueueDraw canvas
        updatePropMenu sNodes entryNodeID entryNodeName colorBtn
      -- clique com o botão direito: insere edges entre nodos
      RightButton -> liftIO $ do
        es <- liftIO $ readIORef st
        let g = editorGetGraph es
            newSelectedNode = checkSelectNode g (x,y)
        context <- liftIO $ widgetGetPangoContext canvas
        if length newSelectedNode == 0
        then createNode st (x,y) context
        else let  selectedNodes = editorGetSelectedNodes es
                  graph = editorGetGraph es
                  newGraph = if length newSelectedNode == 1
                               then foldl (\g n -> insertEdge g n (newSelectedNode!!0)) graph selectedNodes
                               else graph
             in writeIORef st (newGraph, newSelectedNode, [])
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
        moveEdge st (ox,oy) (x,y)
        writeIORef oldPoint (x,y)
        widgetQueueDraw canvas
        else return ()
    return True

  -- teclado
  canvas `on` keyPressEvent $ do
    k <- eventKeyName
    liftIO $ do
      pos <- readIORef oldPoint
      context <- widgetGetPangoContext canvas
      case T.unpack k of
        "Insert" -> do
          createNode st pos context
          widgetQueueDraw canvas
        "Delete" -> do
          deleteSelected st
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
          context <- widgetGetPangoContext canvas
          renameNode st name context
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
  let (g, sNodes, sEdges) = state
  context <- liftIO $ widgetGetPangoContext canvas
  forM (graphGetNodes g) (\n -> renderNode n (n `elem`sNodes) context)
  forM (graphGetEdges g) (\e -> do
    let dstN = getDstNode g e
        srcN = getSrcNode g e
        selected = e `elem` sEdges
    case (srcN, dstN) of
      (Just src, Just dst) -> renderEdge e selected src dst context
      (Nothing, _) -> return ()
      (_, Nothing) -> return ())
  return ()

-- operações de interação ------------------------------------------------------
-- verifica se o usuario selecionou algum nodo
checkSelectNode:: Graph -> (Double,Double) -> [Node]
checkSelectNode g (x,y) = filter (isSelected) $ graphGetNodes g
  where isSelected = (\n -> let (nx,ny) = position . infoGetGraphicalInfo . nodeGetInfo $ n
                                (w,h) = dims . infoGetGraphicalInfo . nodeGetInfo $ n
                            in pointInsideRectangle (x,y) (nx,ny,w,h) )

-- verifica se o usuario selecionou alguma aresta
checkSelectEdge:: Graph -> (Double,Double) -> [Edge]
checkSelectEdge g (x,y) = filter (isSelected) $ graphGetEdges g
  where isSelected = (\e -> pointDistance (x,y) (position . infoGetGraphicalInfo . edgeGetInfo $ e) < 5)

--verifica se um ponto está dentro da bounding box de um nodo
--utiliza a biblioteca Pango para isso
pointInsideNode:: Node -> (Double,Double) -> PangoContext -> IO Bool
pointInsideNode node (x,y) context = do
  pL <- layoutText context $ infoGetContent . nodeGetInfo $ node
  (_, PangoRectangle px py pw ph) <- layoutGetExtents pL
  let (nx,ny) = position . infoGetGraphicalInfo . nodeGetInfo $ node
  return $ pointInsideRectangle (x,y) (nx,ny,pw,ph)

-- move os nodos selecionados
moveNode:: IORef EditorState -> (Double,Double) -> (Double,Double) -> IO ()
moveNode state (xold,yold) (xnew,ynew) = do
  (graph,sNodes, sEdges) <- readIORef state
  let (deltaX, deltaY) = (xnew-xold, ynew-yold)
  -- move nodes
  movedNodes <- forM (sNodes) (\node -> let info = nodeGetInfo node
                                            gi = infoGetGraphicalInfo info
                                            (nox, noy)  = position gi
                                            newPos  = (nox+deltaX, noy+deltaY)
                                        in return $ Node (nodeGetID node) (Info (infoGetContent info) (giSetPosition newPos gi)) )
  -- move as arestas que estão no ponto entre os nodos
  movedEdges <- forM (graphGetEdges graph) (\edge -> let src = getSrcNode graph edge
                                                         dst = getDstNode graph edge
                                                         getMovedNode = (\node -> case find (\n -> n == node) movedNodes of
                                                                                  Just n -> n
                                                                                  Nothing -> node)
                                                      in case (src,dst) of
                                                         (Just a, Just b) -> let aPos = position. infoGetGraphicalInfo . nodeGetInfo $ a
                                                                                 bPos = position. infoGetGraphicalInfo . nodeGetInfo $ b
                                                                                 newAPos = position. infoGetGraphicalInfo . nodeGetInfo $ getMovedNode a
                                                                                 newBPos = position. infoGetGraphicalInfo . nodeGetInfo $ getMovedNode b
                                                                                 edgePos = position . infoGetGraphicalInfo . edgeGetInfo $ edge
                                                                                 info = edgeGetInfo edge
                                                                                 gi = infoGetGraphicalInfo info
                                                                              in if pointDistance edgePos (midPoint aPos bPos) < 10
                                                                                 then return $ Edge (edgeGetID edge) $ Info (infoGetContent info) (giSetPosition (midPoint newAPos newBPos) gi)
                                                                                 else return edge
                                                         _ -> return edge
                                                        )
  -- atualiza o grafo
  let mvng = (\g n -> changeNode g n)
      mveg = (\g e -> changeEdge g e)
      newSEdges = map (\edge -> case find (\e -> edgeGetID e == edgeGetID edge) movedEdges of
                                Just a -> a
                                Nothing -> edge) sEdges
      newGraph = foldl mvng graph movedNodes
      newGraph' = foldl mveg newGraph movedEdges
  writeIORef state (newGraph', movedNodes, sEdges)

-- move as arestas selecionadas
moveEdge:: IORef EditorState -> (Double,Double) -> (Double,Double) -> IO ()
moveEdge state (xold,yold) (xnew,ynew) = do
  (graph,sNodes,sEdges) <- readIORef state
  let (deltaX, deltaY) = (xnew-xold,ynew-yold)
  movedEdges <- forM (sEdges) (\edge-> let info = edgeGetInfo edge
                                           gi = infoGetGraphicalInfo info
                                           (eox, eoy) = position gi
                                           newPos = (eox+deltaX, eoy+deltaY)
                                        in return $ Edge (edgeGetID edge) (Info (infoGetContent info) (giSetPosition newPos gi)) )
  let mvg = (\g e -> changeEdge g e)
      newGraph = foldl mvg graph movedEdges
  writeIORef state (newGraph, sNodes, movedEdges)

-- operações básicas sobre o grafo ---------------------------------------------
-- cria um novo nodo e insere no grafo
createNode:: IORef EditorState -> (Double,Double) -> PangoContext -> IO ()
createNode state pos context = do
  (graph, nodes, edges) <- readIORef state
  let maxnode = if length (graphGetNodes graph) > 0
                  then maximum (graphGetNodes graph)
                  else Node 0 $ Info "" newGraphicalInfo
      nID = 1 + nodeGetID maxnode
      content = ("node " ++ show nID)
  dim <- getStringDims content context
  let newNode = Node nID $ Info content ( giSetDims dim . giSetPosition pos $ newGraphicalInfo )
      newGraph = insertNode graph newNode
  writeIORef state (newGraph, [newNode], [])

-- deleta os nodos e arestas selecionados no grafo
deleteSelected:: IORef EditorState -> IO()
deleteSelected state = do
  (graph, nodes, edges) <- readIORef state
  let graph' = foldl (\g n -> removeNode g n) graph nodes
  let newGraph = foldl (\g e -> removeEdge g e) graph' edges
  writeIORef state (newGraph, [], [])

-- renomeia os nodos selecionados
renameNode:: IORef EditorState -> String -> PangoContext -> IO()
renameNode state name context = do
  (graph, nodes, _) <- readIORef state
  dim <- getStringDims name context
  let renamedNodes = map (\n -> Node (nodeGetID n) $ Info name (giSetDims dim . infoGetGraphicalInfo . nodeGetInfo $ n)) nodes
      newGraph = foldl (\g n -> changeNode g n) graph renamedNodes
  writeIORef state (newGraph,renamedNodes, [])

getStringDims :: String -> PangoContext -> IO (Double, Double)
getStringDims str context = do
  pL <- layoutText context str
  (_, PangoRectangle _ _ w h) <- layoutGetExtents pL
  return (w+4, h+4)


-- ↓↓↓↓↓ estruturas para teste ↓↓↓↓↓ -------------------------------------------

graph1 :: Graph
graph1 = Graph "1" src1 dst1 edges1 nodes1

nodes1 :: [Node]
nodes1 =  [(Node 1 $ Info "hello" $ giSetPosition (100, 40) newGraphicalInfo)
          , (Node 2 $ Info "my" $ giSetPosition (40, 100) newGraphicalInfo)
          , (Node 3 $ Info "name" $ giSetPosition (160, 100) newGraphicalInfo)
          , (Node 4 $ Info "is" $ giSetPosition (40, 160) newGraphicalInfo)
          , (Node 5 $ Info "Mr. Fear" $ giSetPosition (160, 160) newGraphicalInfo)
          ]

edges1 :: [Edge]
edges1 =  [(Edge 1 $ Info "" $ giSetPosition ( 70, 70) newGraphicalInfo)
          ,(Edge 2 $ Info "" $ giSetPosition (130, 70) newGraphicalInfo)
          ,(Edge 3 $ Info "" $ giSetPosition ( 40,130) newGraphicalInfo)
          ,(Edge 4 $ Info "" $ giSetPosition (100,130) newGraphicalInfo)
          ,(Edge 5 $ Info "" $ giSetPosition (100,160) newGraphicalInfo)
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
