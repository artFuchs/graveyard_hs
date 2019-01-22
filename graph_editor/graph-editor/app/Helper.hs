module Helper
( pointDistance
, pointLineDistance
, multPoint
, midPoint
, pointInsideRectangle
, intersectLineRect
, getStringDims
, pointAt
, angle
, quadrant
)where

import Graphics.UI.Gtk
import Graphics.Rendering.Pango.Layout
import Data.Fixed

-- | Módulo contendo funções auxiliares para uso na seleção

-- | calcula a distancia entre dois pontos
pointDistance :: (Double,Double) -> (Double,Double) -> Double
pointDistance (x1,y1) (x2,y2) = sqrt $ (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)

-- | calcula a distancia de um ponto para uma reta
pointLineDistance :: (Double,Double) -> (Double,Double) -> (Double,Double) -> Double
pointLineDistance (x0,y0) (x1,y1) (x2,y2) = ( abs $ (y2-y1)*x0 - (x2-x1)*y0 + x2*y1 - y2*x1 ) / (pointDistance (x1,y1) (x2,y2))

-- | multiplicação de dois pontos
multPoint :: (Double,Double) -> (Double,Double) -> (Double,Double)
multPoint (a,b) (c,d) = (a*c,b*d)

-- | calcula o ponto médio entre dois pontos
midPoint :: (Double,Double) -> (Double,Double) -> (Double,Double)
midPoint (x1,y1) (x2,y2) = (x1 + (x2-x1)/2, y1 + (y2-y1)/2)

-- | calcula um novo ponto dado um angulo, disntancia e um ponto inicial
pointAt :: Double -> Double -> (Double,Double) -> (Double,Double)
pointAt ang dist (x,y) = (x + dist*cos(ang), y + dist*sin(ang))

-- | verifica se um ponto está dentro de um retangulo
-- faz o calculo considerando a posição do retangulo como sendo seu centro
pointInsideRectangle :: (Double,Double) -> (Double,Double,Double,Double) -> Bool
pointInsideRectangle (x,y) (rx,ry,rw,rh) = (abs (x - rx) <= rw/2) && (abs (y - ry) <= rh/2)

-- | Retorna o ponto de intersecção de uma linha com um retangulo
-- recebe o ponto inicial da linha e a representação do retangulo (x,y,w,h)
intersectLineRect :: (Double,Double) -> (Double,Double,Double,Double) -> (Double,Double)
intersectLineRect (lx,ly) (rx,ry,rw,rh) = (rx + t*(lx - rx), ry + t*(ly - ry))
  where t = min tx ty
        tx = (rw/2) / abs (lx - rx)
        ty = (rh/2) / abs (ly - ry)



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

-- | quadrante do angulo
quadrant :: Double -> (Double,Double)
quadrant ang = (c,d)
  where a = ang `mod'` pi
        b = if a < 0 then a + 2*pi else a
        c = if (abs a) <= pi/2 then 1 else -1
        d = if b < pi then 1 else -1
