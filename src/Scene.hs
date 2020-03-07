{-# LANGUAGE ExistentialQuantification, GADTs #-}

-----------------------------------------------------------------------------
-- A Scene encapsualtes all SVG elements that will be rendered to a file.
-----------------------------------------------------------------------------


module Scene 
  ( Scene
  , pattern Scene
  , width
  , height
  , style
  , elements
  , mkScene
  , mkSceneWithStyle
  , svgDoc
  , emptyScene
  , addElement
  , renderScene ) where 

import Control.Monad (sequence_)

import Text.Blaze.Svg11 ((!))
import Text.Blaze.Svg.Renderer.String (renderSvg)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Printf

import Helpers (applyAttrs, roundToStr)
import Layer (Layer)
import Style
import Shape


data Scene = Scene { width       :: Float
                   , height      :: Float
                   , style       :: StyleAttrs
                   , elements    :: [S.Svg]
                   } 


instance Show Scene where
  show scene = "Scene{width=" ++ show (width scene) ++ " height=" ++ show (height scene) ++ "}"

mkScene :: Float -> Float -> [Layer] -> Scene
mkScene w h layers = let svgElements = map toSvg layers in
  Scene { width=w, height=h, style=defaultStyleAttrs, elements=svgElements}

mkSceneWithStyle :: Float -> Float -> StyleAttrs -> [Layer] -> Scene
mkSceneWithStyle w h st layers = let svgElements = map toSvg layers in
  Scene { width=w, height=h, style=st, elements=svgElements}


instance SvgShape Scene where
  toSvg scene = 
    S.docTypeSvg ! A.version "1.1" 
                 ! A.width (widthAttr scene) 
                 ! A.height (heightAttr scene)  
                 ! A.viewbox (viewBox scene) $
      applyAttrs parentGroupAttrs (S.g $ sequence_ (elements scene))
    where
      parentGroupAttrs = A.transform (transform scene) : getAttrs (style scene)


widthAttr :: Scene -> S.AttributeValue
widthAttr s = S.stringValue $ roundToStr 2 (width s) ++ "in"

heightAttr :: Scene -> S.AttributeValue
heightAttr s = S.stringValue $ roundToStr 2 (height s) ++ "in"

viewBox :: Scene -> S.AttributeValue
viewBox scene = S.stringValue $ "0 0 " ++ roundToStr 4 (width scene) ++ " " ++ roundToStr 4 (height scene)

transform :: Scene -> S.AttributeValue
transform scene = S.stringValue $ printf "translate(%s,%s) scale(1, -1)" (roundToStr 4 $ width scene * 0.5) (roundToStr 4 $ height scene * 0.5)


svgDoc :: Scene -> S.Svg ->  S.Svg
svgDoc scene core = S.docTypeSvg ! A.version "1.1" ! A.width (widthAttr scene) ! A.height (heightAttr scene)  ! A.viewbox (viewBox scene) $
    S.g ! A.transform (transform scene) ! A.fill (S.stringValue "#ffffff") $ core


emptyScene :: Float -> Float -> StyleAttrs -> Scene
emptyScene w h s = Scene w h s []


addElement :: (SvgShape s) => Scene -> s -> Scene
addElement scene s = scene { elements=els }
  where els = toSvg s : elements scene


renderScene :: IO Scene -> IO String
renderScene sceneM = renderSvg . toSvg <$> sceneM
