module Arc 
    ( Arc
    , pattern Arc
    , svgPathDefinition
    , Arc.asTuple
    , midpoint    
    , arcLength
    , invert ) where

import Text.Blaze.Svg11 ((!))
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Printf

import ApproxEq
import Point ((|+|))
import Point as P
import Shape


-- | Represents an arc with center, radius, ccw from start to end.
-- note that angles must be in (-pi, pi)
data Arc = Arc { center :: P.Point
               , radius :: !Float
               , start  :: !Float
               , end    :: !Float
                } deriving (Eq, Show)


-- | Calculate the SVG path defition for an Arc
svgPathDefinition :: Arc -> String
svgPathDefinition (Arc (Point cx cy) r th1 th2)  =
    printf "m%.4f,%.4f A%.4f,%.4f 0 %d,1 %.4f,%.4f" startX startY r r lgFlag endX endY
  where 
    startX = cx + r * cos th1
    startY = cy + r * sin th1
    endX   = cx + r * cos th2
    endY   = cy + r * sin th2
    lgFlag = if (th2 - th1 > (-pi)) || (th1 - th2 > (-pi)) then 0 else 1 :: Int


instance SvgShape Arc where
    toSvg a = S.path ! A.d  (S.stringValue $ svgPathDefinition a)


instance ApproxEq Arc where
    approxEqual a b epsilon = raddiiEq && centerEq && startEq && endEq
      where
        eqAttr f = abs (f a - f b) < epsilon 
        raddiiEq = eqAttr radius
        centerEq = approxEqual (center a) (center b) epsilon
        startEq  = eqAttr start
        endEq    = eqAttr end

instance Transformable Arc where
    
    translate (Arc c r th1 th2) v = let newC = c |+| v in 
        Arc newC r th1 th2

    translateP (Arc c r th1 th2) v = let newC = c + v in 
        Arc newC r th1 th2

    rotate (Arc c r th1 th2) p t = let newC = P.rotate c p t in 
        Arc newC r (th1 + t) (th2 + t)

    mirror (Arc c r th1 th2) p v = Arc newC r (2 * vth - th2) (2 * vth - th1)
      where
        newC = P.mirror c p v
        vth  = P.angleBetween (Point 1 0) v

    offset (Arc c r th1 th2) (Point x _) leftSide = Arc c (r + e) th1 th2
      where e = if leftSide then x * (-1) else x

instance Mergable Arc where

    merge a b epsilon = if approxEqual a b epsilon then Just a else Nothing



instance Ord Arc where
    compare a b = Arc.asTuple a `compare` Arc.asTuple b
    (<=) a b = Arc.asTuple a <= Arc.asTuple b


-- | Convert a circle to a 2-tuple representation.
asTuple :: Arc -> (Float, Float, Float, Float, Float)
asTuple (Arc (Point x y) r th1 th2) = (x, y, r, th1, th2)


midpoint :: Arc -> Float -> Point
midpoint (Arc c r _ _) t = c |+| r * Point (cos t) (sin t)


arcLength :: Arc -> Float
arcLength (Arc _ r th1 th2) = r * (th2 - th1)


-- | Reflect a point p about the arc at point b.
invert :: Arc -> Point -> Point -> Point
invert (Arc c _ _ _) p b = P.mirror p b t
  where
    Point rx ry = b - c
    t            = Point (-ry) rx




