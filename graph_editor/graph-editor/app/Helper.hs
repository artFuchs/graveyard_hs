module Helper
( pointDistance
, midPoint
, pointInsideRectangle
, getStringDims
)where

import Graphics.UI.Gtk
import Graphics.Rendering.Pango.Layout

-- | Módulo contendo funções auxiliares para uso na seleção

-- | calcula a distancia entre dois pontos
pointDistance :: (Double,Double) -> (Double,Double) -> Double
pointDistance (x1,y1) (x2,y2) = sqrt $ (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)

-- | calcula o ponto médio entre dois pontos
midPoint :: (Double,Double) -> (Double,Double) -> (Double,Double)
midPoint (x1,y1) (x2,y2) = (x1 + (x2-x1)/2, y1 + (y2-y1)/2)

-- | verifica se um ponto está dentro de um retangulo
-- faz o calculo considerando a posição do retangulo como sendo seu centro
pointInsideRectangle :: (Double,Double) -> (Double,Double,Double,Double) -> Bool
pointInsideRectangle (x,y) (rx,ry,rw,rh) = (x >= rx - rw/2) && (x <= rx + rw/2) && (y >= ry - rh/2) && (y <= ry + rh/2)

-- | dado um texto, adquire o tamanho da bounding box do texto para renderiza-lo
-- utiliza a biblioteca pango para isso
getStringDims :: String -> PangoContext -> IO (Double, Double)
getStringDims str context = do
  pL <- layoutText context str
  (_, PangoRectangle _ _ w h) <- layoutGetExtents pL
  return (w+4, h+4)
