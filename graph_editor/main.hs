import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Data.Text hiding (map)
import Graph

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
    drawGraph graph1

  -- tratamento de eventos - janela principal
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False

  mainGUI


drawGraph :: Graph -> Render ()
drawGraph g = do
  forM (graphGetNodes g) renderNode
  forM (graphGetEdges g) (\e -> do
    let dstN = getDstNode g e
        srcN = getSrcNode g e
    case (srcN, dstN) of
      (Just src, Just dst) -> renderEdge src dst
      (Nothing, _) -> return ()
      (_, Nothing) -> return ())
  return ()

renderNode :: Node -> Render ()
renderNode node = do
  let (x,y) = position . infoGetGraphicalInfo . nodeGetInfo $ node
      (r,g,b) = color . infoGetGraphicalInfo . nodeGetInfo $ node
      content = infoGetContent . nodeGetInfo $ node
  setSourceRGB r g b
  moveTo x y
  lineTo (x+10) (y+10)
  lineTo (x-10) (y+10)
  closePath
  stroke

  setSourceRGB 0 0 0
  setLineWidth 1
  moveTo x y
  textPath (pack content)
  stroke

renderEdge :: Node -> Node -> Render ()
renderEdge nodeSrc nodeDst = do
  setSourceRGB 0 0 0
  let (x1,y1) = position . infoGetGraphicalInfo . nodeGetInfo $ nodeSrc
      (x2,y2) = position . infoGetGraphicalInfo . nodeGetInfo $ nodeDst
  moveTo x1 (y1+10)
  lineTo x2 y2
  stroke


-- ↓↓↓↓↓ estruturas para teste ↓↓↓↓↓ -------------------------------------------

nodes1 :: [Node]
nodes1 =  [(Node 1 $ Info "hello" $ GraphicalInfo {color = (1,0,0), position = (100.0, 40.0)})
          , (Node 2 $ Info "my" $ GraphicalInfo {color = (0,1,0), position = (40, 100)})
          , (Node 3 $ Info "name" $ GraphicalInfo {color = (0,0,1), position = (160, 100)})
          , (Node 4 $ Info "is" $ GraphicalInfo {color = (1,1,0), position = (40, 160)})
          , (Node 5 $ Info "Mr. Fear" $ GraphicalInfo {color = (0,1,1), position = (160, 160)})
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
