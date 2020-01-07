module Point
    ( Point
    , pattern Point
    , xVal
    , yVal
    , fromFloat
    , pointMap
    , asTuple
    , (|+|)
    , (|*|)
    , (|/|)
    , cross
    , dot 
    , angleBetween
    , mag
    , magSquared
    , mirror
    , rotate
    ) where

import ApproxEq

data Point = Point !Float !Float deriving (Eq, Show)

instance Num Point where
    Point x1 y1 + Point x2 y2 = Point (x1 + x2) (y1 + y2)
    Point x1 y1 * Point x2 y2 = Point (x1 * x2) (y1 * y2)
    Point x1 y1 - Point x2 y2 = Point (x1 - x2) (y1 - y2)
    abs (Point x y) = Point (abs x) (abs y)
    signum (Point x y) = Point (signum x) (signum y)
    negate (Point x y) = Point (x * (-1)) (y * (-1))
    fromInteger i = Point (fromInteger i) (fromInteger i)

instance Fractional Point where
    Point x1 y1 / Point x2 y2 = Point (x1 / x2) (y1 / y2)
    recip (Point x y) = Point y x
    fromRational r = Point (fromRational r) (fromRational r)    

instance ApproxEq Point where
    approxEqual a b epsilon = let Point dx dy = abs (a - b) in
        dx < epsilon && dy < epsilon

instance Ord Point where
    compare a b = asTuple a `compare` asTuple b
    (<=) a b = asTuple a <= asTuple b

-- | Extract the x axis value from a point
xVal :: Point -> Float
xVal (Point coord _) = coord

-- | Extract the y axis value from a point
yVal :: Point -> Float
yVal (Point _ coord) = coord

-- | Construct a point from a scalar value.
fromFloat :: Float -> Point
fromFloat f = Point f f

-- | Map a float -> float function over a point.
pointMap :: (Float -> Float) -> Point -> Point
pointMap f (Point x y) = Point (f x) (f y)

-- | Convert a point to a 2-tuple representation.
asTuple :: Point -> (Float, Float)
asTuple (Point x y) = (x, y)

-- Operators for doing arithmetic on a point with a scalar float value.
(|+|) :: Point -> Float -> Point
(|+|) p v = p + fromFloat v

(|*|) :: Point -> Float -> Point
(|*|) p v = p * fromFloat v

(|/|) :: Point -> Float -> Point
(|/|) p v = p / fromFloat v



-- | Geometric point manipulation functions.


cross :: Point -> Point -> Float
cross (Point x1 y1) (Point x2 y2) = x1 * y2 - y1 * x2

dot :: Point -> Point -> Float
dot (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

angleBetween :: Point -> Point -> Float
angleBetween p1 p2 = atan2 (cross p1 p2) (dot p1 p2)

mag :: Point -> Float
mag p = sqrt (dot p p)

magSquared :: Point -> Float
magSquared p = dot p p

-- Mirror point a about a line through point p along vector v
mirror :: Point -> Point -> Point -> Point
mirror a p v = (-a) + 2 * p + 2 * w |*| dot (a - p) w 
    where w = v |/| mag v

-- Rotate point a about a line through point p along vector t
rotate :: Point -> Point -> Float -> Point
rotate a p t = p + Point (ax * cos t - ay * sin t) (ax * sin t + ay * cos t)
    where
        Point ax ay = a - p




