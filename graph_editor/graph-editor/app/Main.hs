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
                sNodes = filter (\n -> let pos = position . infoGetGraphicalInfo . nodeGetInfo $ n
                                   in pointInsideRectangle pos (x + (w/2), y + (h/2) , abs w, abs h)) $ graphGetNodes graph
                sEdges = filter (\e -> let pos = position . infoGetGraphicalInfo . edgeGetInfo $ e
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
          renameSelectedNodes st name context
          widgetQueueDraw canvas
        _       -> return ()
    return False

  onColorSet colorBtn $ do
    Color r g b <- colorButtonGetColor colorBtn
    let color = ((fromIntegral r)/65535, (fromIntegral g)/65535, (fromIntegral b)/65535)
    (graph, nodes, edges) <- readIORef st
    newNodes <- forM (nodes) (\n -> let nid = nodeGetID n
                                        info = nodeGetInfo n
                                        c = infoGetContent info
                                        gi = infoGetGraphicalInfo info
                                        gi' = giSetColor color gi
                                    in return $ Node nid (Info c gi') )
    let newGraph = foldl (\g n -> changeNode g n) graph newNodes
    writeIORef st (newGraph, newNodes, edges)
    widgetQueueDraw canvas

  onColorSet lineColorBtn $ do
    Color r g b <- colorButtonGetColor lineColorBtn
    let color = ((fromIntegral r)/65535, (fromIntegral g)/65535, (fromIntegral b)/65535)
    (graph, nodes, edges) <- readIORef st
    newNodes <- forM (nodes) (\n -> let nid = nodeGetID n
                                        info = nodeGetInfo n
                                        c = infoGetContent info
                                        gi = infoGetGraphicalInfo info
                                        gi' = giSetLineColor color gi
                                    in return $ Node nid (Info c gi') )
    newEdges <- forM (edges) (\e -> let eid = edgeGetID e
                                        info = edgeGetInfo e
                                        c = infoGetContent info
                                        gi = infoGetGraphicalInfo info
                                        gi' = giSetLineColor color gi
                                    in return $ Edge eid (Info c gi') )
    let newGraph = foldl (\g n -> changeNode g n) graph newNodes
    let newGraph' = foldl (\g e -> changeEdge g e) newGraph newEdges
    writeIORef st (newGraph', newNodes, edges)
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
          name = infoGetContent . nodeGetInfo $ (nodes!!0)
          (r,g,b) = color . infoGetGraphicalInfo . nodeGetInfo $ (nodes!!0)
          (r',g',b') = lineColor . infoGetGraphicalInfo . nodeGetInfo $ (nodes!!0)
          nodeColor = Color (round (r*65535)) (round (g*65535)) (round (b*65535))
          nodeLineC = Color (round (r'*65535)) (round (g'*65535)) (round (b'*65535))
      entrySetText entryID iD
      entrySetText entryName name
      colorButtonSetColor colorBtn nodeColor
      colorButtonSetColor lcolorBtn nodeLineC
    (0, 1) -> do
      let iD = show . edgeGetID $ (edges!!0)
          name = infoGetContent . edgeGetInfo $ (edges!!0)
          (r,g,b) = lineColor . infoGetGraphicalInfo . edgeGetInfo $ (edges!!0)
          edgeColor = Color (round (r*65535)) (round (g*65535)) (round (b*65535))
      entrySetText entryID iD
      entrySetText entryName name
      colorButtonSetColor lcolorBtn edgeColor
    _ -> do
      entrySetText entryID "--"
      entrySetText entryName "----"
      colorButtonSetColor colorBtn $ Color 49151 49151 49151
      colorButtonSetColor lcolorBtn $ Color 49151 49151 49151

-- atualização do desenho do grafo --------------------------------------------
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
  where isSelected = (\n -> let (nx,ny) = position . infoGetGraphicalInfo . nodeGetInfo $ n
                                (w,h) = dims . infoGetGraphicalInfo . nodeGetInfo $ n
                            in pointInsideRectangle (x,y) (nx,ny,w,h) )

-- verifica se o usuario selecionou alguma aresta
checkSelectEdge:: Graph -> (Double,Double) -> [Edge]
checkSelectEdge g (x,y) = case find (\e -> isSelected e) $ graphGetEdges g of
                            Just a -> [a]
                            Nothing -> []
  where isSelected = (\e -> pointDistance (x,y) (position . infoGetGraphicalInfo . edgeGetInfo $ e) < 5)

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
  movedEdges <- forM (graphGetEdges graph) (\edge -> let nullNode = Node 0 (Info "" newGraphicalInfo)
                                                         a = fromMaybe nullNode $ getSrcNode graph edge
                                                         b = fromMaybe nullNode $ getDstNode graph edge
                                                         getMovedNode = (\node -> fromMaybe node $ find (\n -> n == node) movedNodes)
                                                         aPos = position. infoGetGraphicalInfo . nodeGetInfo $ a
                                                         bPos = position. infoGetGraphicalInfo . nodeGetInfo $ b
                                                         newAPos = position. infoGetGraphicalInfo . nodeGetInfo $ getMovedNode a
                                                         newBPos = position. infoGetGraphicalInfo . nodeGetInfo $ getMovedNode b
                                                         (xe,ye) = position . infoGetGraphicalInfo . edgeGetInfo $ edge
                                                         (xe',ye') = (xe+deltaX, ye+deltaY)
                                                         info = edgeGetInfo edge
                                                         gi = infoGetGraphicalInfo info
                                                      in if a == b && a `elem` sNodes
                                                          then return $ Edge (edgeGetID edge) $ Info (infoGetContent info) (giSetPosition (xe',ye') gi)
                                                          else if centered gi
                                                            then return $ Edge (edgeGetID edge) $ Info (infoGetContent info) (giSetPosition (midPoint newAPos newBPos) gi)
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
  movedEdges <- forM (sEdges) (\edge-> let info = edgeGetInfo edge
                                           gi = infoGetGraphicalInfo info
                                           (xe, ye) = position gi
                                           newPos = (xe+deltaX, ye+deltaY)
                                           nullNode = Node 0 (Info "" newGraphicalInfo)
                                           srcPos = position . infoGetGraphicalInfo . nodeGetInfo . fromMaybe nullNode $ getSrcNode graph edge
                                           dstPos = position . infoGetGraphicalInfo . nodeGetInfo . fromMaybe nullNode $ getDstNode graph edge
                                           mustCenter = pointLineDistance newPos srcPos dstPos < 10
                                        in return $ Edge (edgeGetID edge) (Info (infoGetContent info) (giSetCentered mustCenter . giSetPosition newPos $ gi)) )
  let mvg = (\g e -> changeEdge g e)
      newGraph = foldl mvg graph movedEdges
  writeIORef state (newGraph, sNodes, movedEdges)

-- ajusta a posição das edges selecionadas caso a propriedade centered seja True
adjustEdges:: IORef EditorState -> IO ()
adjustEdges state = do
  (graph,sNodes,sEdges) <- readIORef state
  let adjust = (\e -> let nullNode = Node 0 (Info "" newGraphicalInfo)
                          srcPos = position . infoGetGraphicalInfo . nodeGetInfo . fromMaybe nullNode $ getSrcNode graph e
                          dstPos = position . infoGetGraphicalInfo . nodeGetInfo . fromMaybe nullNode $ getDstNode graph e
                          info = edgeGetInfo e
                          gi = infoGetGraphicalInfo info
                      in if centered gi
                        then Edge (edgeGetID e) (Info (infoGetContent info) (giSetCentered True . giSetPosition (midPoint srcPos dstPos) $ gi))
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
                  else Node 0 $ Info "" newGraphicalInfo
      nID = 1 + nodeGetID maxnode
      content = ("node " ++ show nID)
  dim <- getStringDims content context
  let newNode = Node nID $ Info content ( giSetDims dim . giSetPosition pos $ newGraphicalInfo )
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

-- renomeia os nodos selecionados
renameSelectedNodes:: IORef EditorState -> String -> PangoContext -> IO()
renameSelectedNodes state name context = do
  (graph, nodes, _) <- readIORef state
  dim <- getStringDims name context
  let renamedNodes = map (\n -> Node (nodeGetID n) $ Info name (giSetDims dim . infoGetGraphicalInfo . nodeGetInfo $ n)) nodes
      newGraph = foldl (\g n -> changeNode g n) graph renamedNodes
  writeIORef state (newGraph,renamedNodes, [])





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


-- To Do List ------------------------------------------------------------------

-- *alterar cor dos nodos e das arestas
-- *Label para as Edges
-- *Definir a intersecção da linha da edge com o retangulo do nodo
-- *Definir formas diferentes para os nodos
-- *Estilos diferentes para as Edges
-- *Salvar / carregar grafo
-- *Undo / Redo