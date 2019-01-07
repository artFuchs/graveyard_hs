module Helper
( pointDistance
, midPoint
, pointInsideRectangle
)where

-- Funções auxiliares

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
