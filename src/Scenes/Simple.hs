module Scenes.Simple (getScene) where

import Circle
import Layer ((+:))
import qualified Group as G
import Point
import Rectangle
import Scene
import Style
import Shape


-----------------------------------------------------------------------------
-- The simple scene documented in the README.
-- Provides a nice worked example of all the basic concepts in this library.
-----------------------------------------------------------------------------

getScene :: IO Scene
getScene = pure $ createScene "simple" 5 5 layerStyle `addElement` layer
  where
    circle      = Circle (Point 0 0) 1
    circleList  = [circle, translateP circle (Point 1 0), translateP circle (Point (-1) 0)]
    circles     = G.Group "my three circles" circleList
    moreCircles = G.rotate circles (Point 0 0) (-1.55)
    allCircles  = G.optimizeGroup (circles <> moreCircles) 0.01
    square      = mkRectangle (Point 1.5 1.5) (Point (-1.5) (-1.5))
    layer       = square +: G.toLayer allCircles
    layerStyle  = StyleAttrs { strokeColor=Just "#03161d"
                             , strokeWidth=Just 0.05
                             , fillColor=Nothing }