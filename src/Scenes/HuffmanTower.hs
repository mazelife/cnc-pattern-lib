module Scenes.HuffmanTower (getScene) where

import Arc (pattern Arc)
import Layer ((+:), (<+>), rotate, mirror, translateP)
import Line (pattern Line)
import Point (pattern Point)
import Rectangle (mkRectangle)
import Scene
import Style

{--
orange = "#ef6101"
lemon = "#fdac07"
green = "#47802b"
teal = "#045658"
navy = "#03161d"
white = "#ffffff"
--}



getScene :: IO Scene
getScene = return ((createScene "huffman_tower" 8 3 layerStyle) `addElement` final)
  where
    len = 1.0
    hgt = len * (sqrt 3) / 2
    segments = 7.0

    topLeft = Point ((-segments) * len / 2) (1.5 * hgt)
    bottomRight = Point (segments * len / 2) (1.5 * (-hgt))
    framingRect = mkRectangle topLeft bottomRight

    c1 = Point len 0
    a1 = Arc c1 len (2 * pi / 3) pi
    c2 = c1 * 0.5
    a2 = Arc c2 (0.25 * len) (pi / 2) pi
    l1 = Line (Point 0 hgt) (Point (0.52 * len) hgt)
    l2 = Line (Point (0.5 * len) (hgt * 0.97)) (Point (0.5 * len) (1.5 * hgt))
    
    v0 = l1 +: l2 +: a2 <+> a1
    v1 = rotate v0 (c2 * 0.5) pi
    v2 = mirror (v0 ++ v1) c2 (Point 0 1)
    baseForm = v1 ++ v2 ++ v0
    
    start = (-segments) / 2.0
    ts = [start, (start + 1.0) .. start + segments - 1]
    ps = map (\x -> Point x 0) ts
    final = framingRect +: (concat $ map (\p -> translateP baseForm p) ps)
    layerStyle = StyleAttrs { strokeColor=(Just "#03161d")
                            , strokeWidth=(Just 0.05) 
                            , fillColor=Nothing }



