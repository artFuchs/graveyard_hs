import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Layout
import Data.Text hiding (map)
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

  -- cria um canvas em branco
  canvas <- drawingAreaNew
  widgetSetCanFocus canvas True
  widgetAddEvents canvas [AllEventsMask]
  containerAdd window canvas
  widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

  -- mostra a GUI
  widgetShowAll window

  -- TRATAMENTOS DE EVENTOS
  -- tratamento de eventos - canvas
  canvas `on` draw $ do
    drawGraph graph1 canvas

  -- tratamento de eventos - janela principal
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  mainGUI


drawGraph :: Graph -> DrawingArea -> Render ()
drawGraph g canvas = do
  context <- liftIO $ widgetGetPangoContext canvas
  forM (graphGetNodes g) (\n -> renderNode n context)
  forM (graphGetEdges g) (\e -> do
    let dstN = getDstNode g e
        srcN = getSrcNode g e
    case (srcN, dstN) of
      (Just src, Just dst) -> renderEdge src dst context
      (Nothing, _) -> return ()
      (_, Nothing) -> return ())
  return ()


-- desenha um nodo, com seu texto
renderNode :: Node -> PangoContext -> Render ()
renderNode node context = do
  let (x,y) = position . infoGetGraphicalInfo . nodeGetInfo $ node
      (r,g,b) = color . infoGetGraphicalInfo . nodeGetInfo $ node
      (rl,gl,bl) = lineColor . infoGetGraphicalInfo . nodeGetInfo $ node
      content = infoGetContent . nodeGetInfo $ node

  pL <- liftIO $ layoutText context content
  (_,PangoRectangle px py pw ph) <- liftIO $ layoutGetExtents pL

  setSourceRGB r g b
  rectangle (x-(pw/2)) (y-(ph/2)) (pw) (ph)
  fill

  setSourceRGB rl gl bl
  rectangle (x-(pw/2)) (y-(ph/2)) (pw) (ph)
  stroke

  setSourceRGB 0 0 0
  moveTo (x-(pw/2)) (y-(ph/2))
  showLayout pL

  -- setSourceRGB 0 0 0
  -- setLineWidth 1
  -- moveTo x y
  -- textPath (pack content)
  -- stroke

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
      n2 = 3 + (max pw2 ph2)/2
      (x1', y1') = (x1 + vx*n1, y1 + vy*n1)
      (x2', y2') = (x2 - vx*n2, y2 - vy*n2)

  -- desenha uma linha representando
  moveTo x1' y1'
  lineTo x2' y2'
  stroke

  -- desenha um circulo para indicar qual é o nó de destino
  arc x2' y2' 3 0 (2*pi)
  fill



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
