{-| A type representing a hexagon. -}
module Hexagon where

import Text.Blaze.Svg11 (g)

import ApproxEq
import Helpers (degToRad)
import Line
import Point
import Shape


data Hexagon = Hexagon { center :: Point
                       , radius :: !Float
                       } deriving (Eq, Show)
 
instance ApproxEq Hexagon where
    approxEqual a b epsilon = raddiiEqual && centersEqual
      where 
        raddiiEqual = abs (radius a - radius b) < epsilon
        centersEqual = approxEqual (center a) (center b) epsilon

instance Mergable Hexagon where
    merge a b epsilon = if approxEqual a b epsilon then Just a else Nothing

instance Ord Hexagon where
    compare a b = hexagonAsTuple a `compare` hexagonAsTuple b
    (<=) a b = hexagonAsTuple a <= hexagonAsTuple b

instance SvgShape Hexagon where
    toSvg h = g $ mapM_ toSvg $ asLines h

instance Transformable Hexagon where
    translate p h       = Hexagon { center=center h + p, radius=radius h}
    rotate p v h        = Hexagon { center=rotateP (center h) p v, radius=radius h}
    mirror p v h        = Hexagon { center=mirrorP (center h) p v, radius=radius h}
    offset d leftSide h = Hexagon { center=center h, radius=radius h + e}
        where e = if leftSide then xVal d * (-1) else xVal d


asLines :: Hexagon -> [Line]
asLines (Hexagon cntr r) = [ Line a b
                           , Line b c
                           , Line c d
                           , Line d e
                           , Line e f
                           , Line f a ]
  where 
      p = (Point r 0)
      ln = Line (cntr + p) (cntr - p)
      (Line b e) = ln
      (Line a d) = rotate cntr (degToRad 60) ln
      (Line f c) = rotate cntr (degToRad 120) ln

-- | Convert a circle to a 2-tuple representation.
hexagonAsTuple :: Hexagon -> ((Float, Float), Float)
hexagonAsTuple (Hexagon c r) = (pointAsTuple c, r)
