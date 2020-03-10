module Line
    ( Line
    , pattern Line
    , start
    , end
    , mkLine
    , asTuple
    , lineLength
    , slope
    , areParallel
    , containsPoint
    , overlappingSegments ) where

import Control.Monad (liftM2)

import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import ApproxEq
import Point ((|/|))
import qualified Point as P
import Helpers (fta)
import Shape


data Line  = Line { start :: P.Point
                  , end   :: P.Point
                  } deriving (Eq, Show)


instance SvgShape Line where
    toSvg l = let Line (P.Point x1 y1) (P.Point x2 y2) = l in 
        S.line ! A.x1 (fta x1) ! A.y1 (fta y1) ! A.x2 (fta x2) ! A.y2 (fta y2)

instance ApproxEq Line where
    approxEqual a b epsilon = sameStart && sameEnd
        where sameStart = approxEqual (start a) (start b) epsilon
              sameEnd = approxEqual (end a) (end b) epsilon    


instance Transformable Line where
    translate p l       = Line (start l + p) (end l + p)
    rotate p t l        = Line (P.rotate (start l) p t) (P.rotate (end l) p t)
    mirror p v l        = Line (P.mirror (start l) p v) (P.mirror (end l) p v)
    offset d leftSide l = Line (start l + d * r) (end l + d * r)
        where 
            n = end l - start l
            angle = if leftSide then pi / 2 else (-pi) / 2
            q :: P.Point
            q = P.rotate n (P.Point 0 0) angle
            r = q |/| P.mag q


instance Mergable Line where

    -- | Two line segments can be merged if their points are collinear and if one line segment
    -- contains at least one of the points in the other. The merged segment is the minimum
    -- and maximum (sorted by lexicographical order) of the set of all four endpoints,
    merge a b tol 
        | approxEqual a b tol = Just a
        | overlappingSegments a b = 
            let ps = [start a, end a, start b, end b] in Just (Line (minimum ps) (maximum ps))
        | otherwise = Nothing


instance Ord Line where
    compare a b = asTuple a `compare` asTuple b
    (<=) a b = asTuple a <= asTuple b


-- | Short-form constructor for a line.
mkLine :: Float -> Float -> Float -> Float -> Line
mkLine a b c d = Line (P.Point a b) (P.Point c d)


-- | Represent a line as a 4-tuple.
asTuple :: Line -> (Float, Float, Float, Float)
asTuple (Line a b) = (P.xVal a, P.yVal a, P.xVal b, P.yVal b)


-- | Calculate the length of a line.
lineLength :: Line -> Float
lineLength = P.mag . liftM2 (-) start end


-- | Calculate the slope of a line.
slope :: Line -> Either String Float
slope l
    | x == 0 && y == 0 = Left "NaN"
    | x == 0 && y /= 0 = Left "Inf"
    | otherwise = Right (y / x)
    where P.Point x y = start l - end l


-- | Are two lines parallel?
areParallel :: Line -> Line -> Float -> Bool
areParallel a b tolerance = sharedSlope (slope a) (slope b)
    where 
        sharedSlope (Right aSlope) (Right bSlope) = abs (aSlope - bSlope) < tolerance
        sharedSlope _ _ = False


-- | Does the given point lie along the given line ?
onLine :: P.Point -> Line -> Bool
onLine (P.Point px py) (Line (P.Point ax ay) (P.Point bx by))  = 
    (px - ax) * dy == (py - ay) * dx
  where
    dx = bx - ax
    dy = by - ay


-- | Does the given point lie inside the given line segment?
containsPoint :: P.Point -> Line -> Bool
containsPoint p (Line s e) = isOnLine && p <= lineMax && p >= lineMin
  where
    isOnLine = onLine p (Line s e)
    lineMin  = minimum [s, e]
    lineMax  = maximum [s, e]


-- | Are two line (segments) overlapping? (i.e. are collinear and 
-- the enpoint of one is contained within length of the other)
overlappingSegments :: Line -> Line -> Bool
overlappingSegments a b =  sameSlope && (overlappingEnpoint || completelyContained)
  where
    sameSlope           = slope a == slope b
    overlappingEnpoint  = containsPoint (start b) a || containsPoint (end b) a
    completelyContained = containsPoint (start a) b




