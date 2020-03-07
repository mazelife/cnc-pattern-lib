module Scenes.Triangles (getScene) where


import qualified Group as G
import Layer (mkLayer, style)
import Line (pattern Line)
import Point (pattern Point, fromFloat)
import Rectangle (mkRectangle)
import Scene (Scene, pattern Scene)
import Shape (toSvg, mirror, rotate, translateP)
import Style 



getScene :: IO Scene
getScene = do
    optGrp <- G.optimizeGroupAndLog final 0.1
    let triangles = (G.toLayer "layer1" optGrp) { style = Just(layerStyle) }
    pure $ Scene (3 * (m + 0.5) * l) (sqrt 3 * (n + 1) * l) layerStyle [toSvg $ frame <> triangles]
  where
    l = 0.5  -- triangle circumradius
    n = 6    -- twice number in height
    m = 6    -- twice number in width    
    layerStyle = StyleAttrs { strokeColor=Just "#03161d"
                            , strokeWidth=Just 0.01
                            , fillColor=Nothing }       
    lp = fromFloat l
    p0 = Point (-1) 0 * lp
    p1 = Point 0 (sqrt 3  / 3) * lp
    p2 = Point ((-1) / 2) (-sqrt 3 / 6) * lp
    lin0 = Line p0 p1
    lin1 = Line p1 p2
    lin2 = Line p0 p2
    g0 = G.Group [lin0, lin1, lin2]
    ts = map (\r -> 2 * pi * r / 3 ) [0..2]
    g1 = mconcat $ map (\t -> rotate g0 (Point 0 0) t) ts
    g2 = mirror g1 p0 (p1 - p2)
    g3 = mirror (g1 <> g2) (Point 0.5 0) (Point 0 1)
    th = map (\x -> (x - n / 2 + 0.25) * l * sqrt 3) [0..n - 1]
    tm = map (\x -> (x - n / 2 + 0.25) * l * 3) [0..m - 1]
    pts = Point <$> tm <*> th
    final = mconcat $ map (\p -> translateP g3 p) pts
    tl = Point ((-1.5) * m - 0.25 * l) (0.5 * sqrt 3 * (n + 0.5) * l)
    br = Point (1.5 * m - 0.25 * l) ((-0.5) * sqrt 3 * (n + 0.5) * l)
    frame = mkLayer "rectangles" [mkRectangle tl br]

