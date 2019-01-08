module Helper
( pointDistance
, midPoint
, pointInsideRectangle
, getStringDims
, pointAt
, angle
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

-- | calcula um novo ponto dado um angulo, disntancia e um ponto inicial
pointAt :: Double -> Double -> (Double,Double) -> (Double,Double)
pointAt ang dist (x,y) = (x + dist*cos(ang), y + dist*sin(ang))

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

-- | angulo entre dois pontos
angle :: (Double,Double) -> (Double,Double) -> Double
angle (a,b) (c,d) =
  case (dx `compare` 0,dy `compare` 0) of
       (LT,LT) -> pi + atan(dy/dx)
       (LT,EQ) -> pi
       (LT,GT) -> pi - atan(-dy/dx)
       (EQ,LT) -> 3*pi/2
       (EQ,EQ) -> 0
       (EQ,GT) -> pi/2
       (GT,LT) -> 2*pi - atan(-dy/dx)
       (GT,EQ) -> 0
       (GT,GT) -> atan(dy/dx)
   where  dy = d-b
          dx = c-a
