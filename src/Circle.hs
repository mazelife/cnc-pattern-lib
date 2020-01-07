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

    translate c v = Circle{center=center c P.|+| v, radius=radius c}

    translateP c p = Circle{center=center c + p, radius=radius c}

    rotate c p t = Circle{center=P.rotate (center c) p t, radius=radius c}

    mirror c p v = Circle{center=P.mirror (center c) p v, radius=radius c}

    offset c d leftSide = Circle{center=center c, radius=radius c + e}
        where e = if leftSide then P.xVal d * (-1) else P.xVal d


instance Mergable Circle where
    merge a b epsilon = if approxEqual a b epsilon then Just a else Nothing


instance Ord Circle where
    compare a b = asTuple a `compare` asTuple b
    (<=) a b = asTuple a <= asTuple b

-- Short-form constructor for a circle.
mkCircle :: Float -> Float -> Float -> Circle
mkCircle a b = Circle (P.Point a b)

-- | Convert a circle to a 2-tuple representation.
asTuple :: Circle -> ((Float, Float), Float)
asTuple (Circle c r) = (P.asTuple c, r)

circumference :: Circle -> Float
circumference c = 2 * pi * radius c
