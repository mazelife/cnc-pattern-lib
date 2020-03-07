module Scenes.HobermanCylinder (getScene) where

import Group (pattern Group, optimizeGroupAndLog, toLayer)
import Line
import Point (pattern Point, cartesianProduct, fromFloat, xVal)
import Scene
import Shape
import Style

getScene :: IO Scene
getScene = do
    let layerStyle = StyleAttrs { strokeColor=Just "#03161d"
                                , strokeWidth=Just 0.03
                                , fillColor=Nothing }
    ls <- optimizeGroupAndLog final 0.01
    pure $ mkSceneWithStyle 12 7 layerStyle [toLayer "layer-1" ls]
  where        
    th1 = 60 * pi / 180
    th2 = 75 * pi / 180
    w = fromFloat 0.275
    h = fromFloat 0.5
    gap = 1.5
    nx = 4
    ny = 6

    x = Point 1 0
    y = Point 0 1
    o = Point 0 0

    p1 = x * w
    p2 = x * h / tan th1 + h * y
    p3 = p1 + x * h / tan th2 + y * h

    l1 = Line o p1
    l2 = Line o p2
    l3 = Line p2 p3
    l4 = Line p1 p3
    l5 = Line p3 (p3 + p1)
    l6 = Line p1 (p3 - p2 + p1)
    l7 = Line (p3 - p2 + p1) (p3 + p1)
    l8 = Line (p3 - p2 + p1) (p3 - p2 + p1 + x * gap)
    l9 = Line (p3 + p1) (p3 + p1 + x * gap)
    l10 = Line (p3 - p2 + p1 + x * gap) (p3 + p1 + x * gap)
    l11 = Line (p3 - p2 + p1) (p3 + p1) 

    base = Group [l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11]
    t0 = base <> mirror base p2 (Point 1 0)
    
    tot_w = 1.5 + xVal (p3 - p2 + p1)
    xs = map (\n -> n * tot_w - 0.5 * nx * tot_w) [0..3]
    ys = map (\n -> n * 2 * 0.5 - ny * 0.5) [0..5]
    final = mconcat $ map (\p -> translateP t0 p) (cartesianProduct xs ys)