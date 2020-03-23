module Scenes.Hexagons (getScene) where

import Numeric.LinearAlgebra (vector)

import Arc
import Group
import Helpers (degToRad, vectorToFloats)
import Hexagon
import Layer (style)
import Line
import Point
import Scene (Scene, mkSceneWithStyle)
import Shape
import Style


r :: Float
r = 0.35



--rotateArc :: Arc -> Arc
--rotateArc (Arc c ra s e) = Arc c ra (s - (degToRad 90)) (e - (degToRad 90))

connectEndpoints :: (Arc, Arc) -> Line
connectEndpoints (a, b) = Line (snd $ arcCoords a) (snd $ arcCoords b)

duplicate :: (Show a, SvgShape a, Transformable a, Mergable a) => a -> Group a
duplicate = let pts = map (\x -> Point x 0) (vectorToFloats $ vector [0..5] - 6 / 2 + 0.25) in 
  Group . translatePoints pts

offsetGroup :: (Show a, SvgShape a, Transformable a, Mergable a) => Group a -> Group a
offsetGroup = translateGroupOverPoints [Point 0.5 (r * 2.1), Point 0.5 (-r * 2.1)]

alignedGroup :: (Show a, SvgShape a, Transformable a, Mergable a) => Group a -> Group a
alignedGroup = translateGroupOverPoints [Point 0 (r * 4.1), Point 0 (-r * 4.1)]


getScene :: IO Scene
getScene = pure $ mkSceneWithStyle 7 5 sceneStyle [hexLayer, arcLayer, lineLayer]
  where 
      orange = "#ef6101"
      teal = "#045658"

      hexes = duplicate (Hexagon (Point 0 0) r)
      offsetHexes = offsetGroup hexes
      alignedHexes = alignedGroup hexes
      hexLayer = (toLayer "hexagon" (hexes <> offsetHexes <> alignedHexes)) {style=Just $ withStrokeColor orange}

      leftArcs = duplicate $ Arc (Point 0 0) (r * 1.15) (degToRad 90) (degToRad 180) -- <> 
      leftOffsetArcs = offsetGroup leftArcs
      leftAlignedArcs = alignedGroup leftArcs
      
      rightArcs = duplicate $  Arc (Point 0 0) (r * 1.2) (degToRad 270) (degToRad 0)
      rightOffsetArcs = offsetGroup rightArcs
      rightAlignedArcs = alignedGroup rightArcs

      arcLayer = (toLayer "arcs" 
        (leftArcs <> leftOffsetArcs <> leftAlignedArcs <> rightArcs <> rightOffsetArcs <> rightAlignedArcs)) {style=Just $ withStrokeColor teal}

      connectingLines = Group $ zipWith (curry connectEndpoints) (toList rightArcs) (tail (toList leftArcs))
      offsetLines = translate (Point (-0.01) 0) $ offsetGroup connectingLines
      alignedLines = alignedGroup connectingLines
      lineLayer = (toLayer "lines" (connectingLines <> offsetLines <> alignedLines)) {style=Just $ withStrokeColor teal}

      sceneStyle = defaultStyleAttrs { strokeColor=Just "#000000", strokeWidth=Just 0.015 }