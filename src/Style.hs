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


withStrokeColor :: String -> StyleAttrs
withStrokeColor color = StyleAttrs {strokeColor=Just color, strokeWidth=Nothing, fillColor=Nothing}


defaultFill :: StyleAttrs -> Maybe String
defaultFill = Just . fromMaybe "none" . fillColor


strokeWidthStr :: StyleAttrs -> Maybe String
strokeWidthStr = (roundToStr 4 <$>) . strokeWidth


defaultStyleAttrs :: StyleAttrs
defaultStyleAttrs = StyleAttrs Nothing Nothing Nothing


getAttrs :: StyleAttrs -> [S.Attribute]
getAttrs style = mapMaybe genAttribute attributeHandlers
    where 
        attributeHandlers = [(strokeColor, A.stroke), (strokeWidthStr, A.strokeWidth), (defaultFill, A.fill)]
        genAttribute (acessor, attr) = attr . S.stringValue <$> acessor style


applyStyle :: StyleAttrs -> S.Svg -> S.Svg
applyStyle = applyAttrs . getAttrs

maybeApplyStyle :: Maybe StyleAttrs -> S.Svg -> S.Svg
maybeApplyStyle (Just style) = applyStyle style 
maybeApplyStyle Nothing      = id