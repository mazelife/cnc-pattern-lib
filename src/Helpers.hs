module Helpers 
    ( roundToStr
    , fta
    , applyAttrs
    , degToRad 
    , radToDeg
    , vectorToFloats ) where

import Numeric.LinearAlgebra.Data (Vector, R, toList)
import qualified Text.Blaze.Svg11 as S
import Text.Printf


roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"


-- Convert a float to an SVG attribute, preserving up to 6 decimal places.
fta :: Float -> S.AttributeValue
fta f = S.stringValue (roundToStr 6 f)

-- Apply a list of attributes to an SVG element.
applyAttrs :: [S.Attribute] -> S.Svg ->  S.Svg
applyAttrs attributes element = foldr (flip (S.!)) element attributes

-- | Convert from degrees to radians.
radToDeg :: Float -> Float
radToDeg radians = radians * 180 / pi

-- | Convert from radians to degrees.
degToRad :: Float -> Float
degToRad degrees = degrees * pi / 180

-- | [Hmatrix vectors](https://hackage.haskell.org/package/hmatrix) use doubles, not floats  
vectorToFloats :: Vector R -> [Float]
vectorToFloats = map realToFrac . toList