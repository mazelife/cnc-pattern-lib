module Scenes.HuffmanTowerOptimized (getScene) where

import System.IO (hPutStrLn, stderr)

import Arc (pattern Arc)
import Layer ((+:))
import qualified Group as G
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
getScene = do
    let allArcs = mconcat $ map (\p -> G.translateP baseArcs p) ps
    let allLines = mconcat $ map (\p -> G.translateP baseLines p) ps
    hPutStrLn stderr $ "Original Lines :" ++ (show . length $ G.shapes allLines)
    let optLines = G.optimizeGroup allLines 0.1
    hPutStrLn stderr $ "Optimized Lines :" ++ (show . length $ G.shapes optLines)
    let final = framingRect +: (G.toLayer allArcs) ++ (G.toLayer optLines)
    return (createScene "huffman_tower" 8 3 layerStyle `addElement` final)
  where
    layerStyle = StyleAttrs { strokeColor=(Just "#03161d")
                            , strokeWidth=(Just 0.05) 
                            , fillColor=Nothing }   
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
    ag0 = G.Group "arcs" [a1, a2]
    ag1 = G.rotate ag0 (c2 * 0.5) pi
    ag2 = G.mirror (ag0 <> ag1) c2 (Point 0 1)
    baseArcs = ag0 <> ag1 <> ag2

    l1 = Line (Point 0 hgt) (Point (0.52 * len) hgt)
    l2 = Line (Point (0.5 * len) (hgt * 0.97)) (Point (0.5 * len) (1.5 * hgt))
    lg0 = G.Group "lines" [l1, l2]  
    lg1 = G.rotate lg0 (c2 * 0.5) pi
    lg2 = G.mirror (lg0 <> lg1) c2 (Point 0 1)
    baseLines = lg0 <> lg1 <> lg2
   
    start = (-segments) / 2.0
    ts = [start, (start + 1.0) .. start + segments - 1]
    ps = map (\x -> Point x 0) ts




