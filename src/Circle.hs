{-| A type representing a circle. -}
module Circle where

import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import ApproxEq
import Helpers (fta)
import qualified Point as P
import Shape


data Circle = Circle { center :: P.Point
                     , radius :: !Float
                     } deriving (Eq, Show)

instance SvgShape Circle where
    toSvg (Circle (P.Point cx cy) rad) = 
        S.circle ! A.cx (fta cx) ! A.cy (fta cy) ! A.r (fta rad)


instance ApproxEq Circle where
    approxEqual a b epsilon = raddiiEqual && centersEqual
      where 
        raddiiEqual = abs (radius a - radius b) < epsilon
        centersEqual = approxEqual (center a) (center b) epsilon


instance Transformable Circle where

    translate p c = Circle{center=center c + p, radius=radius c}

    rotate p t c = Circle{center=P.rotateP (center c) p t, radius=radius c}

    mirror p v c = Circle{center=P.mirrorP (center c) p v, radius=radius c}

    offset d leftSide c = Circle{center=center c, radius=radius c + e}
        where e = if leftSide then P.xVal d * (-1) else P.xVal d


instance Mergable Circle where
    merge a b epsilon = if approxEqual a b epsilon then Just a else Nothing


instance Ord Circle where
    compare a b = circleAsTuple a `compare` circleAsTuple b
    (<=) a b = circleAsTuple a <= circleAsTuple b

-- Short-form constructor for a circle.
mkCircle :: Float -> Float -> Float -> Circle
mkCircle a b = Circle (P.Point a b)

-- | Convert a circle to a 2-tuple representation.
circleAsTuple :: Circle -> ((Float, Float), Float)
circleAsTuple (Circle c r) = (P.pointAsTuple c, r)

-- | Caclulate the circumfrence of a circle.
circumference :: Circle -> Float
circumference c = 2 * pi * radius c

-- | Get coordinate on the circumfrance of a circle at the given angle (radians)
circleCoords :: Circle -> Float -> P.Point
circleCoords (Circle (P.Point cx cy) r) angle = P.Point x y
  where 
    x = cx + r * cos angle
    y = cy + r * sin angle



