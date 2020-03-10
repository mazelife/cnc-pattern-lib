{-# LANGUAGE PatternSynonyms #-}

module Style 
    ( StyleAttrs
    , pattern StyleAttrs
    , strokeColor
    , strokeWidth
    , fillColor
    , withStrokeColor
    , defaultStyleAttrs 
    , getAttrs
    , applyStyle
    , maybeApplyStyle) where 

import Data.Maybe (fromMaybe, mapMaybe)

import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Helpers


data StyleAttrs = StyleAttrs { strokeColor :: Maybe String
                             , strokeWidth :: Maybe Float
                             , fillColor   :: Maybe String
                             } deriving (Eq, Show)


-- | Shortcut constructor for a StyleAttrs instance wherre just stroke color is set.
withStrokeColor :: String -> StyleAttrs
withStrokeColor color = StyleAttrs {strokeColor=Just color, strokeWidth=Nothing, fillColor=Nothing}

-- | Shortcut constructor for an "empty" StyleAttrs instance (i.e. no styles are specified)
defaultStyleAttrs :: StyleAttrs
defaultStyleAttrs = StyleAttrs Nothing Nothing Nothing

defaultFill :: StyleAttrs -> Maybe String
defaultFill = Just . fromMaybe "none" . fillColor


strokeWidthStr :: StyleAttrs -> Maybe String
strokeWidthStr = (roundToStr 4 <$>) . strokeWidth

-- Return a list of Blaze.SVG attributes from a given style.
getAttrs :: StyleAttrs -> [S.Attribute]
getAttrs style = mapMaybe genAttribute attributeHandlers
    where 
        attributeHandlers = [(strokeColor, A.stroke), (strokeWidthStr, A.strokeWidth), (defaultFill, A.fill)]
        genAttribute (acessor, attr) = attr . S.stringValue <$> acessor style


-- Apply the given style to an SVG element.
applyStyle :: StyleAttrs -> S.Svg -> S.Svg
applyStyle = applyAttrs . getAttrs

-- Apply the given style to an SVG element, if provided.
maybeApplyStyle :: Maybe StyleAttrs -> S.Svg -> S.Svg
maybeApplyStyle (Just style) = applyStyle style 
maybeApplyStyle Nothing      = id