{-# LANGUAGE ImplicitParams #-}

module LineTest where

import Test.HUnit.Approx ((@~?))
import Test.Tasty
import Test.Tasty.HUnit

import ApproxEq ((@?~), approxEqual)
import Line
import qualified Point as P
import Shape


lineOpsTest :: TestTree
lineOpsTest = testGroup "Test basic operations on lines" 
  [ testCase "Lines are orderable" $
    (compare a b @?= LT) >> (compare a a @?= EQ) >> (compare b a @?= GT)
  ]
  where a = mkLine 0 0 2 3  -- "Fixture" data for tests.
        b = mkLine 0 0 2 4



lineGeometryTest :: TestTree
lineGeometryTest = let ?epsilon = 0.001 in testGroup "Test geometric calculations on lines"
  [ testCase "Lines are close function" $
      (approxEqual a semiClose  0.01 @?= True) >> (approxEqual a semiClose  0.0001 @?= False)
  , testCase "Translate function" $
      translate (P.Point 0.5 0.5) a @?= expTranslateVal
  , testCase "Rotate function" $
      rotate (P.Point 1 1) (0.5 * pi) a @?~ expRotateVal
  , testCase "Line offset function" $
      offset (P.Point 1 1) True a @?~ expOffsetVal
  , testCase "Line length function" $
      lineLength a @~? 3.60555
  , testCase "Line slope function" $
      (slope a @?= Right 1.5) >> (slope (mkLine 1 1 1 2) @?= Left "Inf")
  , testCase "Lines are parallel function" $
      (areParallel a parallelA 0.001 @?= True) >> (areParallel a notParallelA 0.001 @?= False)
  , testGroup "Test line merging capability" [
      testCase "Lines are the same" $ merge a a 0.01 @?= Just a
    , testCase "Lines share endpoint, slope" $ merge a mergableToA 0.01 @?= Just expMergedLine
    , testCase "Lines have different slope" $ merge a notMergableToA 0.01 @?= Nothing 
    , testCase "Line contains point" $ 
        (containsPoint (P.Point 1 1.5) a @?= True) >>
        (containsPoint (P.Point 2 3) a @?= True) >>
        (containsPoint (P.Point 1 1) a @?= False)
    , testCase "Segments are collinear and overlap" $
      (overlappingSegments a (mkLine 0.5 0.75 1 1.5) @?= True) >>
      (overlappingSegments a (mkLine (-1) (-1.5) 0 0) @?= True) >>
      (overlappingSegments a (mkLine 2 3 5 7.5) @?= True)
  

    ]    
  ]
  where a               = mkLine 0 0 2 3                     -- "Fixture" data for tests.
        semiClose       = mkLine 0.001 0.002 2.001 2.999
        expTranslateVal = mkLine 0.5 0.5 2.5 3.5
        expRotateVal    = mkLine 2 0(-1) 2
        expOffsetVal    = mkLine (-0.832) 0.554 1.167 3.554
        parallelA       = mkLine 0 1 2 4
        notParallelA    = mkLine 1 1 3 0
        mergableToA     = mkLine 2 3 4 6
        expMergedLine   = mkLine 0 0 4 6
        notMergableToA  = mkLine 2 3 5 6



