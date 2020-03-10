module Scenes.Resch (getScene, vectorToFloats) where

import Numeric.LinearAlgebra (vector)
import Numeric.LinearAlgebra.Data (Vector, R, toList)

import qualified Group as G
import Group (pattern Group, optimizeGroup)
import Layer (mkLayer)
import Line (pattern Line)
import Point (pattern Point, cartesianProduct, fromFloat)
import Rectangle (mkRectangle)
import Scene (Scene, mkSceneWithStyle)
import Shape (mirror, rotate)
import Style 


vectorToFloats :: Vector R -> [Float]
vectorToFloats = map realToFrac . toList

getScene :: IO Scene
getScene = do
    let width      = 3 * (m + 0.5) * l
    let height     = sqrt 3 * (n + 1) * l
    let layerStyle = StyleAttrs { strokeColor=Just "#03161d"
                                , strokeWidth=Just 0.01
                                , fillColor=Nothing }    
    pure $ mkSceneWithStyle width height layerStyle [triangles, frame]
  where
    l = 0.5  -- triangle circumradius
    n = 6    -- twice number in height
    m = 6    -- twice number in width    
   
    lp = fromFloat l
    p0 = Point (-1) 0 * lp
    p1 = Point 0 (sqrt 3  / 3) * lp
    p2 = Point ((-1) / 2) (-sqrt 3 / 6) * lp
    
    -- Create a triangle from our three points.
    baseTriangle = Group [ Line p0 p1, Line p1 p2, Line p0 p2]
    -- Create a group from the base triangle rotated three times.
    rotationalVerticies = map (\r -> 2 * pi * r / 3 ) [0..2]
    t0 = mconcat $ map (\t -> rotate (Point 0 0) t baseTriangle) rotationalVerticies
    -- Mirror this group twice.
    t1 = t0 <> mirror p0 (p1 - p0) t0
    t2 = t1 <> mirror (Point (0.5 * l) 0) (Point 0 1) t1

    -- Clone the above group across a series of positions on our convas.
    xs = vectorToFloats $ (vector [0..5] - 6 / 2 + 0.25) * 3 * 0.5
    ys = vectorToFloats $ (vector [0..5] - 6 / 2 + 0.25) * sqrt 3 * 0.5
    t3 = optimizeGroup (G.translateGroupOverPoints (cartesianProduct xs ys) t2) 0.01
    triangles = G.toLayer "triangles" t3

  -- Create a framing rectangle around the group.
    topLeft = Point ((-1.5 * m - 0.25) * l) (0.5 * sqrt 3 * (n + 0.5) * l)
    bottomRight = Point ((1.5 * m - 0.25) * l) (-0.5 * sqrt 3 * (n + 0.5) * l)
    frame = mkLayer "frame" [mkRectangle topLeft bottomRight]