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
  hBoxEdges <- hBoxNew False 8
  boxPackStart vBoxProps hBoxEdges PackNatural 0
  labelEdges <- labelNew $ Just "Edges: "
  boxPackStart hBoxEdges labelEdges PackNatural 0
  labelEdges' <- labelNew $ Just " "
  boxPackStart hBoxEdges labelEdges' PackNatural 0




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
      -- clique com o botão esquerdo: seleciona nodos e edges
      LeftButton  -> liftIO $ do
        es <- readIORef st
        let (oldSN,oldSE) = editorGetSelected es
            graph = editorGetGraph es
        sNode <- liftIO $ checkSelectNodes st (x,y) canvas
        sEdge <- liftIO $ checkSelectEdges st (x,y) canvas
        -- Shift: seleção de multiplos elementos
        let (sNodes,sEdges) = if Shift `elem` ms
                                then let jointSN = foldl (\ns n -> if n `notElem` ns then n:ns else ns) [] (oldSN ++ sNode)
                                         jointSE = foldl (\es e -> if e `notElem` es then e:es else es) [] (oldSE ++ sEdge)
                                     in (jointSN, jointSE)
                                else (sNode, sEdge)
        writeIORef st (editorGetGraph es, sNodes, sEdges)
        widgetQueueDraw canvas
        updatePropMenu sNodes entryNodeID entryNodeName colorBtn
        labelSetText labelEdges' $ if (length sNodes == 1) then foldl (\s e -> s ++ (show (edgeGetID e)) ++ ", ") "" (getConnectors graph (sNodes!!0)) else ""
      -- clique com o botão direito: insere edges entre nodos
      RightButton -> liftIO $ do
        es <- liftIO $ readIORef st
        newSelectedNode <- liftIO $ checkSelectNodes st (x,y) canvas
        let selectedNodes = editorGetSelectedNodes es
            graph = editorGetGraph es
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
checkSelectNodes:: IORef EditorState -> (Double,Double) -> DrawingArea -> IO [Node]
checkSelectNodes state (x,y) canvas = do
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

  let ns = case maybeSelected of
            Just (Just a) -> [a]
            _ -> []
  return ns

-- verifica se o usuario selecionou alguma aresta
checkSelectEdges:: IORef EditorState -> (Double,Double) -> DrawingArea -> IO [Edge]
checkSelectEdges state (x,y) canvas = do
  es <- readIORef state
  let g = editorGetGraph es
  maybeSelectedEdges <- forM (graphGetEdges g) $ (\e ->
    let inside = pointDistance (x,y) (position . infoGetGraphicalInfo . edgeGetInfo $ e) < 5
    in if inside
        then return (Just e)
        else return Nothing
    )
  let maybeSelected = find (\e -> case e of
                                Nothing -> False
                                _ -> True) $ reverse maybeSelectedEdges
  let es = case maybeSelected of
            Just (Just a) -> [a]
            _ -> []
  return es

--verifica se um ponto está dentro da bounding box de um nodo
--utiliza métodos da biblioteca Pango para isso
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
                                        in return $ Node (nodeGetID node) (Info (infoGetContent info) (giSetPosition gi newPos)) )
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
                                                                                 then return $ Edge (edgeGetID edge) $ Info (infoGetContent info) (giSetPosition gi $ midPoint newAPos newBPos)
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
                                        in return $ Edge (edgeGetID edge) (Info (infoGetContent info) (giSetPosition gi newPos)) )
  let mvg = (\g e -> changeEdge g e)
      newGraph = foldl mvg graph movedEdges
  writeIORef state (newGraph, sNodes, movedEdges)

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
