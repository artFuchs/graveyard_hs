module Helper
( pointDistance
, midPoint
, pointInsideNode
)where

import Graphics.UI.Gtk
import Graphics.Rendering.Pango.Layout
import GraphicalInfo
import Graph

-- Funções auxiliares
pointDistance :: (Double,Double) -> (Double,Double) -> Double
pointDistance (x1,y1) (x2,y2) = sqrt $ (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)

midPoint :: (Double,Double) -> (Double,Double) -> (Double,Double)
midPoint (x1,y1) (x2,y2) = (x1 + (x2-x1)/2, y1 + (y2-y1)/2)

--verifica se um ponto está dentro da bounding box de um nodo
--utiliza métodos da biblioteca Pango para isso
pointInsideNode:: Node -> (Double,Double) -> PangoContext -> IO Bool
pointInsideNode node (x,y) context = do
  pL <- layoutText context $ infoGetContent . nodeGetInfo $ node
  (_, PangoRectangle px py pw ph) <- layoutGetExtents pL
  let (nx,ny) = position . infoGetGraphicalInfo . nodeGetInfo $ node
  return $ (x >= nx - pw/2) && (x <= nx + pw/2) && (y >= ny - ph/2) && (y <= ny + ph/2)
