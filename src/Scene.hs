{-# LANGUAGE ExistentialQuantification, GADTs #-}

-----------------------------------------------------------------------------
-- A Scene encapsualtes all SVG elements that will be rendered to a file.
-----------------------------------------------------------------------------


module Scene 
  ( Scene
  , pattern Scene
  , name
  , width
  , height
  , style
  , elements
  , svgDoc
  , createScene
  , addElement
  , renderScene ) where 

import Control.Monad (sequence_)

import Text.Blaze.Svg11 ((!))
import Text.Blaze.Svg.Renderer.String (renderSvg)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Printf

import Helpers (applyAttrs, roundToStr)
import Style
import Shape


data Scene = Scene { name        :: String
                   , width       :: Float
                   , height      :: Float
                   , style       :: StyleAttrs
                   , elements    :: [S.Svg]
                   } 


instance Show Scene where
  show scene = "Scene{name=" ++ (name scene) ++ "}"


instance SvgShape Scene where
  toSvg scene = 
    S.docTypeSvg ! A.version "1.1" 
                 ! A.width (widthAttr scene) 
                 ! A.height (heightAttr scene)  
                 ! A.viewbox (viewBox scene) $ do
      title scene
      applyAttrs parentGroupAttrs (S.g $ do sequence_ (elements scene))
    where
      parentGroupAttrs = (A.transform $ transform scene) : (getAttrs $ style scene)


title :: Scene -> S.Svg
title = S.title . S.string . name


widthAttr :: Scene -> S.AttributeValue
widthAttr s = S.stringValue $ (roundToStr 2 (width s)) ++ "in"

heightAttr :: Scene -> S.AttributeValue
heightAttr s = S.stringValue $ (roundToStr 2 (height s)) ++ "in"

viewBox :: Scene -> S.AttributeValue
viewBox scene = S.stringValue $ "0 0 " ++ (roundToStr 4 $ width scene) ++ " " ++ (roundToStr 4 $ height scene)

transform :: Scene -> S.AttributeValue
transform scene = S.stringValue $ printf "translate(%s,%s) scale(1, -1)" (roundToStr 4 $ (width scene) * 0.5) (roundToStr 4 $ (height scene) * 0.5)


svgDoc :: Scene -> S.Svg ->  S.Svg
svgDoc scene core = S.docTypeSvg ! A.version "1.1" ! A.width (widthAttr scene) ! A.height (heightAttr scene)  ! A.viewbox (viewBox scene) $ do
    S.g ! A.transform (transform scene) ! A.fill (S.stringValue "#ffffff") $ do core


createScene :: String -> Float -> Float -> StyleAttrs -> Scene
createScene n w h s = Scene n w h s []


addElement :: (SvgShape s) => Scene -> s -> Scene
addElement scene s = scene { elements=els }
  where els = toSvg s : (elements scene)


renderScene :: IO Scene -> IO String
renderScene sceneM = do 
  scene <- sceneM 
  return (renderSvg $ toSvg scene)



