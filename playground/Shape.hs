module Shape
(  Shape(..)
,  translate
,  baseRect
,  baseCircle
,  collides
)where

data Point = Point Float Float deriving (Show)

distance_to :: Point -> Point -> Float
distance_to (Point x1 y1) (Point x2 y2) = sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

translate :: Shape -> Float -> Float -> Shape
translate (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))
translate (Circle (Point x1 y1) r) a b = Circle (Point (x1 + a) (y1 + b)) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r


collides :: Shape -> Shape -> Bool
collides (Circle p1 r1) (Circle p2 r2) = (distance_to p1 p2) < (r1 + r2)
collides (Rectangle (Point x1 y1) (Point x2 y2)) (Rectangle (Point x3 y3) (Point x4 y4)) =
      x1 < x4 && x2 > x3 && y1 < y4 && y2 > y3
