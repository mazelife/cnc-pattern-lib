{-# LANGUAGE ImplicitParams #-}

module CircleTest where

import Test.Tasty
import Test.Tasty.HUnit

import ApproxEq ((@?~), approxEqual)
import Circle
import qualified Point as P
import Shape


circleGeometryTest :: TestTree
circleGeometryTest = let ?epsilon = (0.001 :: Double) in testGroup "Test geometric calculations on circles"
  [ testCase "Circles are close function" $
      (approxEqual a semiClose  0.01 @?= True) >> (approxEqual a semiClose  0.0001 @?= False)
  , testCase "Translate function" $
      translate a 2.5 @?= mkCircle 2.5 2.5 1.5
  , testCase "Rotate function" $
      rotate a (P.Point 2 3) 2.5 @?~ mkCircle 5.397 4.206 1.5
  , testCase "Mirror function" $
      mirror a (P.Point 2 3) (P.Point 1 1) @?~ mkCircle (-1) 1 1.5
  , testCase "Offset function" $
      offset a (P.Point 1 0) False @?= mkCircle 0 0 2.5
  , testGroup "Test circle merging capability" [
      testCase "Circles are the same" $ merge a semiClose 0.01 @?= Just a
    , testCase "Circles are not the same" $ merge a (mkCircle 1 2 1) 0.01 @?= Nothing 
    ]
  ]
  where a               = mkCircle 0 0 1.5           -- "Fixture" data for tests.
        semiClose       = mkCircle 0.001 0 1.501                   
