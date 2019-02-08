-- | módulo contendo funções relacionadas com o rendering do grafo
module Render
( renderNode
, renderEdge
)where

import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Layout
import qualified Data.Text as T
import Data.List
import GraphicalInfo
import Helper
import Control.Monad


-- desenha um nodo, com seu texto
renderNode :: NodeGI -> String -> Bool -> PangoContext -> Render ()
renderNode node content selected context = do
  let (x,y) = position node
      (r,g,b) = if selected
                  then (0,0,1)
                  else fillColor node
      (rl,gl,bl) = if selected
                    then (0,1,0)
                    else lineColor node
      (pw,ph) = dims node

  setSourceRGB r g b
  case shape node of
    NCircle -> let radius = (max pw ph)/2 in renderCircle (x,y) radius (r,g,b) (rl,gl,bl)
    NRect -> renderRectangle (x, y, pw, ph) (r,g,b) (rl,gl,bl)
    NQuad -> renderRectangle (x,y, (max pw ph), (max pw ph)) (r,g,b) (rl,gl,bl)

  setSourceRGB rl gl bl
  moveTo (x-(pw/2-2)) (y-(ph/2-2))
  pL <- liftIO $ layoutText context content
  showLayout pL

renderCircle :: (Double,Double) -> Double -> (Double,Double,Double) -> (Double,Double,Double) ->  Render ()
renderCircle (x,y) radius (r,g,b) (lr,lg,lb) = do
  setSourceRGB r g b
  arc x y radius 0 (2*pi)
  fill
  setSourceRGB lr lg lb
  arc x y radius 0 (2*pi)
  stroke

renderRectangle :: (Double,Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double) -> Render ()
renderRectangle (x,y,w,h) (r,g,b) (lr,lg,lb) = do
  setSourceRGB r g b
  rectangle (x-(w/2)) (y-(h/2)) w h
  fill
  setSourceRGB lr lg lb
  rectangle (x-(w/2)) (y-(h/2)) w h
  stroke


-- desenha uma aresta
renderEdge :: EdgeGI -> String -> Bool -> NodeGI -> NodeGI -> PangoContext -> Render ()
renderEdge edge content selected nodeSrc nodeDst context = do
  if nodeSrc == nodeDst
    then renderLoop edge selected nodeSrc
    else renderNormalEdge edge selected nodeSrc nodeDst
  -- draw Label
  if null content
    then  return ()
    else  do
      pL <- liftIO $ layoutText context content
      (_, PangoRectangle px py pw ph) <- liftIO $ layoutGetExtents pL
      let (x,y) = position nodeSrc
          (u,v) = cPosition edge
          a = angle (x,y) (u,v)
          minD = max pw ph
          labelPos = pointAt (a - pi/2) ( minD+2 ) (u, v)
          (r,g,b) = color edge
      setSourceRGB r g b
      moveTo (fst labelPos - pw/2) (snd labelPos - ph/2)
      showLayout pL

renderNormalEdge :: EdgeGI -> Bool -> NodeGI -> NodeGI -> Render ()
renderNormalEdge edge selected nodeSrc nodeDst = do
  setSourceRGB 0 0 0

  -- calculo dos pontos de origem e destino da aresta
  let (x1, y1) = position nodeSrc
      (pw, ph) = dims nodeSrc
      (x2, y2) = position nodeDst
      (pw2, ph2) = dims nodeDst
      (xe, ye) = cPosition edge
      (x1', y1') = case shape nodeSrc of
        NCircle ->
          let d1 = pointDistance (x1,y1) (xe,ye)
              (vx1,vy1) = ((xe-x1)/d1 , (ye-y1)/d1)
              n1 = (max pw ph + 1)/2
          in (x1 + vx1*n1, y1 + vy1*n1)
        NRect -> intersectLineRect (xe,ye) (x1,y1,pw+3,ph+3)
        NQuad -> let l = max pw ph in intersectLineRect (xe,ye) (x1,y1,l+3,l+3)
      (x2', y2') = case shape nodeSrc of
        NCircle ->
          let d2 = pointDistance (xe,ye) (x2,y2)
              (vx2,vy2) = ((x2-xe)/d2 , (y2-ye)/d2)
              n2 = (max pw2 ph2 + 1)/2
          in (x2 - vx2*n2, y2 - vy2*n2)
        NRect-> intersectLineRect (xe,ye) (x2,y2,pw2+3,ph2+3)
        NQuad -> let l = max pw2 ph2 in intersectLineRect (xe,ye) (x2,y2,l+3,l+3)


      -- configurações de cor
      (r,g,b) = if selected then (0,1,0) else color edge

  -- desenha uma linha representando a aresta
  setSourceRGB r g b
  case style edge of
    ENormal -> do
      moveTo x1' y1'
      lineTo xe  ye
      lineTo x2' y2'
    EPointed -> do
      drawPointedLine (x1',y1') (xe,ye)
      drawPointedLine (xe,ye) (x2',y2')
      
  -- desenha uma seta para indicar qual é o nó de destino
  let a = (angle (xe,ye) (x2,y2))
      d = pointDistance (xe,ye) (x2,y2)
      (xa1,ya1) = (x2',y2')
      (xa2,ya2) = pointAt (a+7*pi/8) 10 (x2',y2')
      (xa3,ya3) = pointAt (a-7*pi/8) 10 (x2',y2')
  moveTo xa1 ya1
  lineTo xa2 ya2
  lineTo xa3 ya3
  lineTo xa1 ya1
  fill
  -- desenha um circulo para mostar o ponto de controle
  arc xe  ye 2 0 (2*pi)
  fill

renderLoop:: EdgeGI -> Bool -> NodeGI -> Render ()
renderLoop edge selected node = do
  let (xe, ye) = cPosition edge
      (x,y) = position node
      a = angle (x,y) (xe, ye)
      d = pointDistance (x,y) (xe,ye)
      (f,g) = pointAt a d (x,y)
      p1 = pointAt (a+pi/8) (d/8) (x,y)
      p2 = pointAt (a+pi/2) (d/1.5) (f,g)
      p1' = pointAt (a-pi/8) (d/8) (x,y)
      p2' = pointAt (a-pi/2) (d/1.5) (f,g)
      (rl,gl,bl) = if selected then (0,1,0) else color edge
  -- desenha uma curva levando ao próprio nodo
  moveTo x y
  curveTo (fst p1) (snd p1) (fst p2) (snd p2) xe ye
  moveTo x y
  curveTo (fst p1') (snd p1') (fst p2') (snd p2') xe ye
  setSourceRGB rl gl bl
  stroke
  -- desenha um arco para mostrar o ponto de controle
  arc f g 2 0 (2*pi)
  fill

-- Retorna o ponto de intersecção de uma linha com um retangulo
-- recebe o ponto inicial da linha e a representação do retangulo (x,y,w,h)
intersectLineRect :: (Double,Double) -> (Double,Double,Double,Double) -> (Double,Double)
intersectLineRect (lx,ly) (rx,ry,rw,rh) = (rx + t*(lx - rx), ry + t*(ly - ry))
  where t = min tx ty
        tx = (rw/2) / abs (lx - rx)
        ty = (rh/2) / abs (ly - ry)


drawPointedLine :: (Double,Double) -> (Double,Double) -> Render ()
drawPointedLine p1@(x1,y1) p2@(x2,y2)= do
  let dist = pointDistance p1 p2
      pointsN = dist/4 :: Double
      linePoints = [0,(1/pointsN)..1]
  forM linePoints (\t -> drawPointInLine p1 p2 t)
  return ()

drawPointInLine :: (Double,Double) -> (Double,Double) -> Double -> Render ()
drawPointInLine p0@(x0,y0) p1@(x1,y1) t = do
  let x = x0 + t * (x1 - x0)
      y = y0 + t * (y1 - y0)
  arc x y 1 0 (2*pi)
  fill
