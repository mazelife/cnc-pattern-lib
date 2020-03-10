module Scenes.HuffmanTower (getScene) where

import Arc (pattern Arc)
import Layer ((+:))
import qualified Group as G
import Line (pattern Line)
import Point (pattern Point)
import Rectangle (mkRectangle)
import Scene
import Shape
import Style


getScene :: IO Scene
getScene = do
    optLines <- G.optimizeGroupAndLog allLines 0.1
    let final = framingRect +: G.toLayer "" allArcs <> G.toLayer "" optLines
    pure $ Scene 8 3 layerStyle [toSvg final]
  where
    layerStyle = StyleAttrs { strokeColor=Just "#03161d"
                            , strokeWidth=Just 0.05
                            , fillColor=Nothing }   
    len = 1.0
    hgt = len * sqrt 3 / 2
    segments = 7.0

    topLeft = Point ((-segments) * len / 2) (1.5 * hgt)
    bottomRight = Point (segments * len / 2) (1.5 * (-hgt))
    framingRect = mkRectangle topLeft bottomRight

    c1 = Point len 0
    a1 = Arc c1 len (2 * pi / 3) pi
    c2 = c1 * 0.5
    a2 = Arc c2 (0.25 * len) (pi / 2) pi
    ag0 = G.Group [a1, a2]
    ag1 = rotate (c2 * 0.5) pi ag0
    ag2 = mirror c2 (Point 0 1) (ag0 <> ag1)
    baseArcs = ag0 <> ag1 <> ag2  

    l1 = Line (Point 0 hgt) (Point (0.52 * len) hgt)
    l2 = Line (Point (0.5 * len) (hgt * 0.97)) (Point (0.5 * len) (1.5 * hgt))
    lg0 = G.Group [l1, l2]  
    lg1 = rotate (c2 * 0.5) pi lg0
    lg2 = mirror c2 (Point 0 1) (lg0 <> lg1)
    baseLines = lg0 <> lg1 <> lg2
   
    start = (-segments) / 2.0
    ts = [start, (start + 1.0) .. start + segments - 1]
    ps = map (\x -> Point x 0) ts
    allArcs = G.translateGroupOverPoints ps baseArcs
    allLines = G.translateGroupOverPoints ps baseLines




