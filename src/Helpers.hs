module Helpers 
    ( roundToStr
    , fta
    , applyAttrs ) where

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