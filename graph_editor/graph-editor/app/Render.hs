module Render
( renderNode
, renderEdge
)where

import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Layout
import qualified Data.Text as T
import Data.List
import Graph
import GraphicalInfo
import Helper

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
