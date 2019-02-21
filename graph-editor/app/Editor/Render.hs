-- | módulo contendo funções relacionadas com o rendering do grafo
module Editor.Render
( renderNode
, renderEdge
)where

import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Layout
import qualified Data.Text as T
import Data.List
import Editor.GraphicalInfo
import Editor.Helper
import Control.Monad


-- desenha um nodo, com seu texto
renderNode :: NodeGI -> String -> Bool -> PangoContext -> Render ()
renderNode node content selected context = do
  let (x,y) = position node
      (r,g,b) = fillColor node
      (rl,gl,bl) = lineColor node
      (pw,ph) = dims node

  setSourceRGB r g b
  case shape node of
    NCircle -> let radius = (max pw ph)/2 in renderCircle (x,y) radius (r,g,b) (rl,gl,bl) selected
    NRect -> renderRectangle (x, y, pw, ph) (r,g,b) (rl,gl,bl) selected
    NQuad -> renderRectangle (x,y, (max pw ph), (max pw ph)) (r,g,b) (rl,gl,bl) selected

  setSourceRGB rl gl bl
  moveTo (x-(pw/2-2)) (y-(ph/2-2))
  pL <- liftIO $ layoutText context content
  showLayout pL

renderCircle :: (Double,Double) -> Double -> (Double,Double,Double) -> (Double,Double,Double) -> Bool ->  Render ()
renderCircle (x,y) radius (r,g,b) (lr,lg,lb) selected = do
  if selected
    then do
      setSourceRGB 0 1 0
      arc x y (radius+3) 0 (2*pi)
      fill
    else
      return ()
  setSourceRGB r g b
  arc x y radius 0 (2*pi)
  fill
  setSourceRGB lr lg lb
  arc x y radius 0 (2*pi)
  stroke

renderRectangle :: (Double,Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double) -> Bool ->  Render ()
renderRectangle (x,y,w,h) (r,g,b) (lr,lg,lb) selected = do
  if selected
    then do
      setSourceRGB 0 1 0
      rectangle (x-(w/2+3)) (y-(h/2+3)) (w+3) (h+3)
      fill
    else
      return ()
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
  -- calculo dos pontos de intersecção da aresta com os nodos de origem e destino
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
      (x2', y2') = case shape nodeDst of
        NCircle ->
          let d2 = pointDistance (xe,ye) (x2,y2)
              (vx2,vy2) = ((x2-xe)/d2 , (y2-ye)/d2)
              n2 = (max pw2 ph2 + 1)/2
          in (x2 - vx2*n2, y2 - vy2*n2)
        NRect-> intersectLineRect (xe,ye) (x2,y2,pw2+3,ph2+3)
        NQuad -> let l = max pw2 ph2 in intersectLineRect (xe,ye) (x2,y2,l+3,l+3)
      -- cor da aresta
      (r,g,b) = color edge

  if selected
    then do
      setLineWidth 4
      setSourceRGB 0 1 0
      moveTo x1' y1'
      lineTo xe ye
      lineTo x2' y2'
      stroke
    else return ()

  -- desenha uma linha representando a aresta
  setLineWidth 2
  setSourceRGB r g b
  case style edge of
    ENormal -> do
      moveTo x1' y1'
      lineTo xe  ye
      lineTo x2' y2'
      stroke
    EPointed -> do
      drawPointedLine (x1',y1') (xe,ye)
      drawPointedLine (xe,ye) (x2',y2')
    ESlashed -> do
      drawSlashedLine (x1',y1') (xe,ye)
      drawSlashedLine (xe,ye) (x2',y2')

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
      (rl,gl,bl) = color edge

  if selected
    then do
      setSourceRGB 0 1 0
      setLineWidth 4
      moveTo x y
      curveTo (fst p1) (snd p1) (fst p2) (snd p2) xe ye
      moveTo x y
      curveTo (fst p1') (snd p1') (fst p2') (snd p2') xe ye
      stroke
    else return ()

  -- desenha uma curva levando ao próprio nodo
  setSourceRGB rl gl bl
  setLineWidth 2
  case style edge of
    ENormal -> do
      moveTo x y
      curveTo (fst p1) (snd p1) (fst p2) (snd p2) xe ye
      moveTo x y
      curveTo (fst p1') (snd p1') (fst p2') (snd p2') xe ye
      stroke
    EPointed -> do
      drawPointedCurve (x,y) p1 p2 (xe,ye)
      drawPointedCurve (x,y) p1' p2' (xe,ye)
    ESlashed -> do
      drawSlashedCurve (x,y) p1 p2 (xe,ye)
      drawSlashedCurve (x,y) p1' p2' (xe,ye)
  -- desenha um arco para mostrar o ponto de controle
  arc f g 2 0 (2*pi)
  fill
  -- desenha dois arcos para mostrar
  setSourceRGB 1 0 0
  arc (fst p1) (snd p1) 2 0 (2*pi)
  fill
  arc (fst p2) (snd p2) 2 0 (2*pi)
  fill
  setSourceRGB 0 0 1
  arc (fst p1') (snd p1') 2 0 (2*pi)
  fill
  arc (fst p2') (snd p2') 2 0 (2*pi)
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
  let (x,y) = interpolate p0 p1 t
  arc x y 1 0 (2*pi)
  fill

drawPointedCurve :: (Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double) -> Render ()
drawPointedCurve p0@(x0,y0) p1@(x1,y1) p2@(x2,y2) p3@(x3,y3) = do
  let mid = midPoint p1 p2
      dist = pointDistance p0 mid + pointDistance p3 mid
      pointsN = dist/4 :: Double
      linePoints = [0,(1/pointsN)..1]
  forM linePoints (\t -> drawPointInCurve p0 p1 p2 p3 t)
  return ()

drawPointInCurve :: (Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double) -> Double -> Render ()
drawPointInCurve p0@(x0,y0) p1@(x1,y1) p2@(x2,y2) p3@(x3,y3) t = do
  let b03 = (1-t)**3
      b13 = 3 * t * (1-t)**2
      b23 = 3 * t**2 * (1-t)
      b33 = t**3
      x = b03*x0 + b13*x1 + b23*x2 + b33*x3
      y = b03*y0 + b13*y1 + b23*y2 + b33*y3
  arc x y 1 0 (2*pi)
  fill

drawSlashedLine :: (Double,Double) -> (Double,Double) -> Render ()
drawSlashedLine p0@(x0,y0) p1@(x1,y1) = do
    let dist = pointDistance p0 p1
        pointsN = dist/4 :: Double
        linePoints = genPairs [0,(1/pointsN)..1]
    forM linePoints (\(t0,t1) -> drawSlashInLine p0 p1 t0 t1)
    return ()

genPairs :: [a] -> [(a,a)]
genPairs [] = []
genPairs (x:[]) = []
genPairs (x:y:ls) = (x,y):(genPairs ls)

drawSlashInLine :: (Double,Double) -> (Double,Double) -> Double -> Double ->  Render ()
drawSlashInLine p0@(x0,y0) p1@(x1,y1) t0 t1 = do
  let
    (x,y)   = interpolate p0 p1 t0
    (x',y') = interpolate p0 p1 t1
  moveTo x y
  lineTo x' y'
  stroke

drawSlashedCurve :: (Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double) -> Render ()
drawSlashedCurve p0@(x0,y0) p1@(x1,y1) p2@(x2,y2) p3@(x3,y3) = do
    let mid = midPoint p1 p2
        dist = pointDistance p0 mid + pointDistance p3 mid
        pointsN = dist/4 :: Double
        curvePoints = genPairs [0,(1/pointsN)..1]

    forM curvePoints (\(t0,t1) -> drawSlashInCurve p0 p1 p2 p3 t0 t1)
    return ()

drawSlashInCurve :: (Double,Double) -> (Double,Double) -> (Double,Double) -> (Double,Double) -> Double -> Double -> Render ()
drawSlashInCurve p0@(x0,y0) p1@(x1,y1) p2@(x2,y2) p3@(x3,y3) t1 t2 = do
  let b03 t = (1-t)**3
      b13 t = 3 * t * (1-t)**2
      b23 t = 3 * t**2 * (1-t)
      b33 t = t**3
      x = (b03 t1)*x0 + (b13 t1)*x1 + (b23 t1)*x2 + (b33 t1)*x3
      y = (b03 t1)*y0 + (b13 t1)*y1 + (b23 t1)*y2 + (b33 t1)*y3
      x' = (b03 t2)*x0 + (b13 t2)*x1 + (b23 t2)*x2 + (b33 t2)*x3
      y' = (b03 t2)*y0 + (b13 t2)*y1 + (b23 t2)*y2 + (b33 t2)*y3
  moveTo x y
  lineTo x' y'
  stroke
