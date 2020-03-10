module Scenes.Simple (getScene) where

import Circle (pattern Circle)
import Layer (mkLayerWithStyle)
import Group (pattern Group, optimizeGroup, toLayer)
import Point (pattern Point)
import Rectangle (mkRectangle)
import Scene (Scene, mkSceneWithStyle)
import Style (pattern StyleAttrs, strokeColor, strokeWidth, fillColor, withStrokeColor)
import Shape (rotate, translate)


-----------------------------------------------------------------------------
-- The simple scene documented in the README.
-- Provides a worked example of all the basic concepts in this library.
-----------------------------------------------------------------------------

getScene :: IO Scene
getScene = pure $ mkSceneWithStyle 5 5 sceneStyle [layer1, layer2]
  where
    circle      = Circle (Point 0 0) 1
    circles     = Group [ circle
                        , translate (Point 1 0) circle
                        , translate (Point (-1) 0) circle] 
    moreCircles = rotate (Point 0 0) (-1.55) circles
    allCircles  = optimizeGroup (circles <> moreCircles) 0.01
    layer1      = toLayer "circles" allCircles
    square      = mkRectangle (Point 1.5 1.5) (Point (-1.5) (-1.5))
    layer2      = mkLayerWithStyle "square" [square] (withStrokeColor "#8c1212")
    sceneStyle  = StyleAttrs { strokeColor=Just "#121c5b"
                             , strokeWidth=Just 0.05
                             , fillColor=Nothing }