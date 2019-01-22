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
  let (x,y) = position . nodeGetGI $ node
      (r,g,b) = if selected
                  then (0,0,1)
                  else fillColor . nodeGetGI $ node
      (rl,gl,bl) = if selected
                    then (0,1,0)
                    else lineColor . nodeGetGI $ node
      content = nodeGetInfo $ node
      (pw,ph) = dims . nodeGetGI $ node
  --(_,PangoRectangle px py pw ph) <- liftIO $ layoutGetExtents pL

  setSourceRGB r g b
  case shape . nodeGetGI $ node of
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
renderEdge :: Edge -> Bool -> Node -> Node -> PangoContext -> Render ()
renderEdge edge selected nodeSrc nodeDst context = do
  if nodeSrc == nodeDst
    then renderLoop edge selected nodeSrc
    else renderNormalEdge edge selected nodeSrc nodeDst
  -- draw Label
  let content = edgeGetInfo $ edge
  if null content
    then  return ()
    else  do
      pL <- liftIO $ layoutText context content
      (_, PangoRectangle px py pw ph) <- liftIO $ layoutGetExtents pL
      let (x,y) = position . nodeGetGI $ nodeSrc
          (u,v) = cPosition . edgeGetGI $ edge
          a = angle (x,y) (u,v)
          minD = max pw ph
          labelPos = pointAt (a - pi/2) ( minD+2 ) (u, v)
          (r,g,b) = color . edgeGetGI $ edge
      setSourceRGB r g b
      moveTo (fst labelPos - pw/2) (snd labelPos - ph/2)
      showLayout pL

renderNormalEdge :: Edge -> Bool -> Node -> Node -> Render ()
renderNormalEdge edge selected nodeSrc nodeDst = do
  setSourceRGB 0 0 0

  -- calculo dos pontos de origem e destino da aresta
  let (x1, y1) = position . nodeGetGI $ nodeSrc
      (pw, ph) = dims . nodeGetGI $ nodeSrc
      (x2, y2) = position . nodeGetGI $ nodeDst
      (pw2, ph2) = dims . nodeGetGI $ nodeDst
      (xe, ye) = cPosition . edgeGetGI $ edge
      (x1', y1') = case shape . nodeGetGI $ nodeSrc of
        NCircle ->
          let d1 = pointDistance (x1,y1) (xe,ye)
              (vx1,vy1) = ((xe-x1)/d1 , (ye-y1)/d1)
              n1 = 3 + (max pw ph)/2
          in (x1 + vx1*n1, y1 + vy1*n1)
        NRect -> intersectLineRect (xe,ye) (x1,y1,pw+3,ph+3)
        NQuad -> let l = max pw ph in intersectLineRect (xe,ye) (x1,y1,l+3,l+3)
      (x2', y2') = case shape . nodeGetGI $ nodeSrc of
        NCircle ->
          let d2 = pointDistance (xe,ye) (x2,y2)
              (vx2,vy2) = ((x2-xe)/d2 , (y2-ye)/d2)
              n2 = 5 + (max pw2 ph2)/2
          in (x2 - vx2*n2, y2 - vy2*n2)
        NRect-> intersectLineRect (xe,ye) (x2,y2,pw2+3,ph2+3)
        NQuad -> let l = max pw2 ph2 in intersectLineRect (xe,ye) (x2,y2,l+3,l+3)


      -- configurações de cor
      (r,g,b) = if selected then (0,1,0) else color . edgeGetGI $ edge

  -- desenha uma linha representando a aresta
  setSourceRGB r g b
  moveTo x1' y1'
  lineTo xe  ye
  lineTo x2' y2'
  stroke
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
  --arc x2' y2' 3 0 (2*pi)
  -- desenha um circulo para mostar o ponto de controle
  arc xe  ye 2 0 (2*pi)
  fill

renderLoop:: Edge -> Bool -> Node -> Render ()
renderLoop edge selected node = do
  let (xe, ye) = cPosition . edgeGetGI $ edge
      (x,y) = position . nodeGetGI $ node
      a = angle (x,y) (xe, ye)
      d = pointDistance (x,y) (xe,ye)
      (f,g) = pointAt a d (x,y)
      p1 = pointAt (a+pi/8) (d/8) (x,y)
      p2 = pointAt (a+pi/2) (d/1.5) (f,g)
      p1' = pointAt (a-pi/8) (d/8) (x,y)
      p2' = pointAt (a-pi/2) (d/1.5) (f,g)
      (rl,gl,bl) = if selected then (0,1,0) else color . edgeGetGI $ edge
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
