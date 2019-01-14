import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Layout
import qualified Data.Text as T
import Data.List
import Data.Maybe
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
  oldPoint <- newIORef (0.0,0.0) -- ultimo ponto em que o botão do mouse foi pressionado
  squareSelection <- newIORef Nothing


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
        case (sNode, sEdge, Shift `elem` ms) of
          -- clico no espaço em branco, Shift não pressionado
          ([],[], False) -> do
            writeIORef st (graph, [], [])
            writeIORef squareSelection $ Just (x,y,0,0)
            updatePropMenu ([],[]) entryNodeID entryNodeName colorBtn lineColorBtn
          (n,e,False) -> do
            writeIORef st (editorGetGraph es, sNode, sEdge)
            updatePropMenu (sNode,sEdge) entryNodeID entryNodeName colorBtn lineColorBtn
          (n,e,True) -> do
            let jointSN = foldl (\ns n -> if n `notElem` ns then n:ns else ns) [] $ sNode ++ oldSN
                jointSE = foldl (\ns n -> if n `notElem` ns then n:ns else ns) [] $ sEdge ++ oldSE
            writeIORef st (graph, jointSN, jointSE)
            updatePropMenu (jointSN, jointSE) entryNodeID entryNodeName colorBtn lineColorBtn
        widgetQueueDraw canvas
      -- clique com o botão direito: insere edges entre nodos
      RightButton -> liftIO $ do
        es <- liftIO $ readIORef st
        let g = editorGetGraph es
            dstNode = checkSelectNode g (x,y)
        context <- liftIO $ widgetGetPangoContext canvas
        if length dstNode == 0
          then createNode st (x,y) context
          else createEdges st dstNode

        widgetQueueDraw canvas
        updatePropMenu (dstNode,[]) entryNodeID entryNodeName colorBtn lineColorBtn
      _           -> return ()
    return True

  -- movimento do mouse
  canvas `on` motionNotifyEvent $ do
    ms <- eventModifierAll
    (x,y) <- eventCoordinates
    (ox,oy) <- liftIO $ readIORef oldPoint
    es <- liftIO $ readIORef st
    let leftButton = Button1 `elem` ms
        (sNodes, sEdges) = editorGetSelected es
    case (leftButton, sNodes, sEdges) of
      (True, [], []) -> liftIO $ do
        modifyIORef squareSelection $ liftM $ (\(a,b,c,d) -> (a,b,x-a,y-b))
        sq <- readIORef squareSelection
        widgetQueueDraw canvas
        --print $ "squareSelection: " ++ show sq
      (True, n, e) -> liftIO $ do
        moveNode st (ox,oy) (x,y)
        moveEdge st (ox,oy) (x,y)
        writeIORef oldPoint (x,y)
        widgetQueueDraw canvas
      (_,_,_) -> return ()
    return True

  -- soltar botão do mouse
  canvas `on` buttonReleaseEvent $ do
    b <- eventButton
    case b of
      LeftButton -> liftIO $ do
        es <- readIORef st
        sq <- readIORef squareSelection
        case (es,sq) of
          ((_,[],[]), Just (x,y,w,h)) -> do
            let graph = editorGetGraph es
                sNodes = filter (\n -> let pos = position . nodeGetGI $ n
                                   in pointInsideRectangle pos (x + (w/2), y + (h/2) , abs w, abs h)) $ graphGetNodes graph
                sEdges = filter (\e -> let pos = cPosition . edgeGetGI $ e
                                   in pointInsideRectangle pos (x + (w/2), y + (h/2), abs w, abs h)) $ graphGetEdges graph
            writeIORef st (graph, sNodes, sEdges)
          ((_,n,e), Nothing) -> adjustEdges st
          (_,_) -> return ()
      _ -> return ()
    liftIO $ do
      writeIORef squareSelection Nothing
      widgetQueueDraw canvas

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
          renameSelected st name context
          widgetQueueDraw canvas
        _       -> return ()
    return False

  onColorSet colorBtn $ do
    Color r g b <- colorButtonGetColor colorBtn
    let col = ((fromIntegral r)/65535, (fromIntegral g)/65535, (fromIntegral b)/65535)
    (graph, nodes, edges) <- readIORef st
    newNodes <- forM (nodes) (\n -> let nid = nodeGetID n
                                        c = nodeGetInfo n
                                        gi = nodeGiSetColor col $ nodeGetGI n
                                    in return $ Node nid c gi)
    let newGraph = foldl (\g n -> changeNode g n) graph newNodes
    writeIORef st (newGraph, newNodes, edges)
    widgetQueueDraw canvas

  onColorSet lineColorBtn $ do
    Color r g b <- colorButtonGetColor lineColorBtn
    let color = ((fromIntegral r)/65535, (fromIntegral g)/65535, (fromIntegral b)/65535)
    (graph, nodes, edges) <- readIORef st
    newNodes <- forM (nodes) (\n -> let nid = nodeGetID n
                                        c = nodeGetInfo n
                                        gi = nodeGiSetLineColor color $ nodeGetGI n
                                    in return $ Node nid c gi )
    newEdges <- forM (edges) (\e -> let eid = edgeGetID e
                                        c = edgeGetInfo e
                                        gi = edgeGiSetColor color $ edgeGetGI e
                                    in return $ Edge eid c gi )
    let newGraph = foldl (\g n -> changeNode g n) graph newNodes
    let newGraph' = foldl (\g e -> changeEdge g e) newGraph newEdges
    writeIORef st (newGraph', newNodes, edges)
    widgetQueueDraw canvas

  radioCircle `on` toggled $ do
    changeNodeShape st NCircle
    widgetQueueDraw canvas

  radioRect `on` toggled $ do
    changeNodeShape st NRect
    widgetQueueDraw canvas

  radioQuad `on` toggled $ do
    changeNodeShape st NQuad
    widgetQueueDraw canvas


  -- tratamento de eventos - janela principal ---------------------------------
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  mainGUI



-- Callbacks -------------------------------------------------------------------
-- atualização do menu de propriedades -----------------------------------------
updatePropMenu :: ([Node],[Edge]) -> Entry -> Entry -> ColorButton -> ColorButton -> IO()
updatePropMenu (nodes,edges) entryID entryName colorBtn lcolorBtn = do
  case (length nodes, length edges) of
    (0, 0) -> do
      entrySetText entryID ""
      entrySetText entryName ""
      colorButtonSetColor colorBtn $ Color 49151 49151 49151
      colorButtonSetColor lcolorBtn $ Color 49151 49151 49151
    (1, 0) -> do
      let iD = show . nodeGetID $ (nodes!!0)
          name = nodeGetInfo $ (nodes!!0)
          (r,g,b) = fillColor . nodeGetGI $ (nodes!!0)
          (r',g',b') = lineColor . nodeGetGI $ (nodes!!0)
          nodeColor = Color (round (r*65535)) (round (g*65535)) (round (b*65535))
          nodeLineC = Color (round (r'*65535)) (round (g'*65535)) (round (b'*65535))
      entrySetText entryID iD
      entrySetText entryName name
      colorButtonSetColor colorBtn nodeColor
      colorButtonSetColor lcolorBtn nodeLineC
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

-- atualização do desenho do grafo --------------------------------------------renameSelected
drawGraph :: EditorState -> Maybe (Double,Double,Double,Double) -> DrawingArea -> Render ()
drawGraph state sq canvas = do
  let (g, sNodes, sEdges) = state
  context <- liftIO $ widgetGetPangoContext canvas
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
    Just (x,y,w,h) -> do rectangle x y w h
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
                            in case shape . nodeGetGI $ n of
                              NCircle -> pointDistance (x,y) (nx,ny) < (w/2)
                              NRect -> pointInsideRectangle (x,y) (nx,ny,w,h)
                              NQuad -> pointInsideRectangle (x,y) (nx,ny,w,w) )

-- verifica se o usuario selecionou alguma aresta
checkSelectEdge:: Graph -> (Double,Double) -> [Edge]
checkSelectEdge g (x,y) = case find (\e -> isSelected e) $ graphGetEdges g of
                            Just a -> [a]
                            Nothing -> []
  where isSelected = (\e -> pointDistance (x,y) (cPosition . edgeGetGI $ e) < 5)

-- move os nodos selecionados
moveNode:: IORef EditorState -> (Double,Double) -> (Double,Double) -> IO ()
moveNode state (xold,yold) (xnew,ynew) = do
  (graph,sNodes, sEdges) <- readIORef state
  let (deltaX, deltaY) = (xnew-xold, ynew-yold)
  -- move nodes
  movedNodes <- forM (sNodes) (\node -> let gi = nodeGetGI node
                                            (nox, noy)  = position gi
                                            newPos  = (nox+deltaX, noy+deltaY)
                                        in return $ Node (nodeGetID node) (nodeGetInfo node) (nodeGiSetPosition newPos gi) )
  -- move as arestas que estão no ponto entre os nodos
  movedEdges <- forM (graphGetEdges graph) (\edge -> let nullNode = Node 0 "" newNodeGI
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
                                                      in if a == b && a `elem` sNodes
                                                          then return $ Edge (edgeGetID edge) (edgeGetInfo edge) $ edgeGiSetPosition (xe',ye') gi
                                                          else if centered gi
                                                            then return $ Edge (edgeGetID edge) (edgeGetInfo edge) $ edgeGiSetPosition (midPoint newAPos newBPos) gi
                                                            else return edge
                                                        )
  -- atualiza o grafo
  let mvng = (\g n -> changeNode g n)
      mveg = (\g e -> changeEdge g e)
      newSEdges = map (\edge -> fromMaybe edge $ find (==edge) movedEdges ) sEdges
      newGraph = foldl mvng graph movedNodes
      newGraph' = foldl mveg newGraph movedEdges
  writeIORef state (newGraph', movedNodes, sEdges)

-- move as arestas selecionadas
moveEdge:: IORef EditorState -> (Double,Double) -> (Double,Double) -> IO ()
moveEdge state (xold,yold) (xnew,ynew) = do
  (graph,sNodes,sEdges) <- readIORef state
  let (deltaX, deltaY) = (xnew-xold,ynew-yold)
  movedEdges <- forM (sEdges) (\edge-> let gi = edgeGetGI edge
                                           (xe, ye) = cPosition gi
                                           newPos = (xe+deltaX, ye+deltaY)
                                           nullNode = Node 0 "" newNodeGI
                                           srcPos = position . nodeGetGI . fromMaybe nullNode $ getSrcNode graph edge
                                           dstPos = position . nodeGetGI . fromMaybe nullNode $ getDstNode graph edge
                                           mustCenter = pointLineDistance newPos srcPos dstPos < 10
                                        in return $ Edge (edgeGetID edge) (edgeGetInfo edge) (edgeGiSetCentered mustCenter . edgeGiSetPosition newPos $ gi))
  let mvg = (\g e -> changeEdge g e)
      newGraph = foldl mvg graph movedEdges
  writeIORef state (newGraph, sNodes, movedEdges)

-- ajusta a posição das edges selecionadas caso a propriedade centered seja True
adjustEdges:: IORef EditorState -> IO ()
adjustEdges state = do
  (graph,sNodes,sEdges) <- readIORef state
  let adjust = (\e -> let nullNode = Node 0 "" newNodeGI
                          srcPos = position . nodeGetGI . fromMaybe nullNode $ getSrcNode graph e
                          dstPos = position . nodeGetGI . fromMaybe nullNode $ getDstNode graph e
                      in if centered (edgeGetGI e)
                        then Edge (edgeGetID e) (edgeGetInfo e) (edgeGiSetCentered True . edgeGiSetPosition (midPoint srcPos dstPos) $ edgeGetGI e)
                        else e)
      newEdges = map adjust sEdges
      newGraph = foldl (\g e -> changeEdge g e) graph newEdges
  writeIORef state (newGraph, sNodes, sEdges)



-- operações básicas sobre o grafo no estado -----------------------------------
-- cria um novo nodo e insere no grafo
createNode:: IORef EditorState -> (Double,Double) -> PangoContext -> IO ()
createNode state pos context = do
  (graph, nodes, edges) <- readIORef state
  let maxnode = if length (graphGetNodes graph) > 0
                  then maximum (graphGetNodes graph)
                  else Node 0 "" newNodeGI
      nID = 1 + nodeGetID maxnode
      content = ("node " ++ show nID)
  dim <- getStringDims content context
  let newNode = Node nID content $ nodeGiSetDims dim . nodeGiSetPosition pos $ newNodeGI
      newGraph = insertNode graph newNode
  writeIORef state (newGraph, [newNode], [])

-- cria e insere uma nova edge no grafo
createEdges:: IORef EditorState -> [Node] -> IO ()
createEdges state dstNodes = do
  es <- readIORef state
  let selectedNodes = editorGetSelectedNodes es
      graph = editorGetGraph es
      newGraph = if length dstNodes == 1
                   then foldl (\g n -> insertEdge g n (dstNodes!!0)) graph selectedNodes
                   else graph
  writeIORef state (newGraph, dstNodes, [])

-- deleta os nodos e arestas selecionados no grafo
deleteSelected:: IORef EditorState -> IO()
deleteSelected state = do
  (graph, nodes, edges) <- readIORef state
  let graph' = foldl (\g n -> removeNode g n) graph nodes
  let newGraph = foldl (\g e -> removeEdge g e) graph' edges
  writeIORef state (newGraph, [], [])

-- renomeia os selecionados
renameSelected:: IORef EditorState -> String -> PangoContext -> IO()
renameSelected state name context = do
  (graph, nodes, edges) <- readIORef state
  dim <- getStringDims name context
  let renamedNodes = map (\n -> Node (nodeGetID n) name (nodeGiSetDims dim . nodeGetGI $ n)) nodes
      renamedEdges = map (\e -> Edge (edgeGetID e) name $ edgeGetGI e) edges
      newGraph = foldl (\g n -> changeNode g n) graph renamedNodes
      newGraph' = foldl (\g e -> changeEdge g e) newGraph renamedEdges
  writeIORef state (newGraph',renamedNodes, renamedEdges)

-- muda a forma de um nodo
changeNodeShape :: IORef EditorState -> NodeShape -> IO ()
changeNodeShape state s = do
  es <- readIORef state
  let nodes = editorGetSelectedNodes es
      newNodes = map (\n -> Node (nodeGetID n) (nodeGetInfo n) (nodeGiSetShape s $ nodeGetGI n)) nodes
      newGraph = foldl (\g n -> changeNode g n) (editorGetGraph es) newNodes
  writeIORef state (newGraph, newNodes, editorGetSelectedEdges es)

-- ↓↓↓↓↓ estruturas para teste ↓↓↓↓↓ -------------------------------------------

graph1 :: Graph
graph1 = Graph "1" src1 dst1 edges1 nodes1

nodes1 :: [Node]
nodes1 =  [ (Node 1 "hello" $ nodeGiSetPosition (100, 40) newNodeGI)
          , (Node 2 "my" $ nodeGiSetPosition (40, 100) newNodeGI)
          , (Node 3 "name" $ nodeGiSetPosition (160, 100) newNodeGI)
          , (Node 4 "is" $ nodeGiSetPosition (40, 160) newNodeGI)
          , (Node 5 "Mr. Fear" $ nodeGiSetPosition (160, 160) newNodeGI)
          ]

edges1 :: [Edge]
edges1 =  [ (Edge 1 "" $ edgeGiSetPosition ( 70, 70) newEdgeGI)
          , (Edge 2 "" $ edgeGiSetPosition (130, 70) newEdgeGI)
          , (Edge 3 "" $ edgeGiSetPosition ( 40,130) newEdgeGI)
          , (Edge 4 "" $ edgeGiSetPosition (100,130) newEdgeGI)
          , (Edge 5 "" $ edgeGiSetPosition (100,160) newEdgeGI)
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


-- To Do List ------------------------------------------------------------------
-- *Definir formas diferentes para os nodos
-- *Estilos diferentes para as Edges
-- *Refatorar código
-- *Separar a estrutura do grafo das estruturas gráficas
-- *Definir a intersecção da linha da aresta com o retangulo do nodo
-- *Salvar / carregar grafo
-- *Undo / Redo
