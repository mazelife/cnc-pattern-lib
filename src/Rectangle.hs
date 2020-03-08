{-| A type representing a parallelogram (doesn't just have to be a rectangle). -}
module Rectangle where

import Control.Monad (mapM_)
import Data.List (all)

import Text.Blaze.Svg11 (g)

import ApproxEq
import Point (xVal, yVal, (|+|))
import qualified Line as L
import qualified Point as P
import Shape


data Rectangle = Rectangle { topLeft     :: P.Point
                           , topRight    :: P.Point   
                           , bottomRight :: P.Point
                           , bottomLeft  :: P.Point
                           } deriving (Eq, Show)

-- | Constructor for a rectangle; supply one point for the top left
-- and one point for the bottom right. 
mkRectangle :: P.Point -> P.Point -> Rectangle
mkRectangle tl br = 
    Rectangle
        { topLeft=tl
        , topRight=P.Point (xVal br) (yVal tl)
        , bottomRight=br
        , bottomLeft=P.Point (xVal tl) (yVal br)
        }

-- | Map a function over each point in the rectangle.
mapPoints :: (P.Point -> P.Point) -> Rectangle -> Rectangle
mapPoints f (Rectangle tl tr br bl) =
    Rectangle (f tl) (f tr) (f br) (f bl)

instance SvgShape Rectangle where
    toSvg r = g $ mapM_ toSvg $ asLines r

instance ApproxEq Rectangle where
    approxEqual a b epsilon = let fs = [topLeft, topRight, bottomRight, bottomLeft] in
        all (\f -> approxEqual (f a) (f b) epsilon) fs

instance Transformable Rectangle where

    translate rect v = mapPoints (\r -> r |+| v) rect 

    translateP rect p = mapPoints (\r -> r + p) rect

    -- | Rotate by ``t`` radians around point p. 
    rotate rect (P.Point px py) t = mapPoints (untransform . rotate_ . transform) rect
      where 
        transform (P.Point a b) = P.Point (a - px) (b - py)
        rotate_ p               = P.rotate p (P.Point 0 0) t
        untransform (P.Point a b)     = P.Point (a + px) (b + py)

    mirror rect p v = mapPoints (\r -> P.mirror r p v) rect

    offset (Rectangle tl tr br bl) p leftSide = Rectangle a b c d
      where  
        L.Line a b = offset (L.Line tl tr) p leftSide
        L.Line d c = offset (L.Line bl br) p leftSide

instance Mergable Rectangle where
    merge a b tol = if approxEqual a b tol then Just a else Nothing


-- | Midpoint of a rectangle.
origin :: Rectangle -> P.Point
origin (Rectangle (P.Point x1 y1) _ (P.Point x2 y2) _) = 
    P.Point ((x2 + x1) / 2) ((y1 + y2) / 2)

-- | Convert a rectangle to a list of lines 
asLines :: Rectangle -> [L.Line]
asLines r = [ L.Line (topLeft r) (topRight r)
            , L.Line (bottomLeft r) (bottomRight r)
            , L.Line (topLeft r) (bottomLeft r)
            , L.Line (topRight r) (bottomRight r) ]
