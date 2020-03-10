{-# LANGUAGE ImplicitParams #-}

module ArcTest where

import Test.Tasty
import Test.Tasty.HUnit

import ApproxEq ((@?~), approxEqual)
import Arc
import qualified Point as P
import Shape


arcRenderingTest :: TestTree
arcRenderingTest = testGroup "Test arc-to-SVG path rendering is correct" 
  [ testCase "Arcs have a correct SVG path definition" $
      svgPathDefinition (Arc (P.Point 1 0) 2 4 5) @?= "m-0.3073,-1.5136 A2.0000,2.0000 0 0,1 1.5673,-1.9178"
  ]

arcGeometryTest :: TestTree
arcGeometryTest = let ?epsilon = (0.001 :: Double) in testGroup "Test geometric calculations on arcs"
  [ testCase "Arcs are approx. equal function" $
      (approxEqual a1 a1 0.01 @?= True) >> (approxEqual a1 a2 0.01 @?= False)
  , testCase "Translate function" $
      (translate 1.5 a1) @?~ Arc (P.Point 2.5 1.5) 2 4 5
  , testCase "TranslateP function" $
      (translateP (P.Point 1.5 2) a1) @?~ Arc (P.Point 2.5 2) 2 4 5
  , testCase "Rotate function" $
      (rotate (P.Point 2 3) 2.5 a1) @?~ Arc (P.Point 4.596560 4.804959) 2 6.5 7.5
  , testCase "Mirror function" $
      (mirror (P.Point 2 3) (P.Point (-1) 1) a1) @?~ Arc (P.Point 5 4) 2 (-0.287611) 0.712389
  , testCase "Offset function" $
      (offset (P.Point 1 0) True a1) @?~ Arc (P.Point 1 0) 1 4 5
  ]
  where
    a1 = Arc (P.Point 1 0) 2 4 5
    a2 = Arc (P.Point 1 0) 2.5 4 5
