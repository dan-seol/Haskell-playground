data NaiveShape = NaiveCircle Float Float Float | NaiveRectangle Float Float Float Float
    deriving (Show)

naiveArea :: NaiveShape -> Float
naiveArea (NaiveCircle _ _ r) = (r^2) * pi
naiveArea (NaiveRectangle x1 y1 x2 y2)  = (abs $ y2 - y1) * (abs $ x2 - x1)

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = (*pi) . (^2) $ r
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ y2 - y1) * (abs $ x2 - x1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect w h = Rectangle (Point 0 0) (Point w h)
