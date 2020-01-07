{-# LANGUAGE ImplicitParams #-}

module PointTest where

import Test.HUnit.Approx
import Test.Tasty
import Test.Tasty.HUnit

import Point

import ApproxEq (approxEqual)


pointArithmeticTest :: TestTree
pointArithmeticTest = testGroup "Test basic arithmetic on points"
  [ testCase "Add points" $
      a + b @?= Point 3 4
  , testCase "Multiply points" $
      a * b @?= Point 2 3  
  , testCase "Subtract points" $
      a - b @?= Point (-1) (-2)
  , testCase "Negate point" $
     (-a) @?= Point (-1) (-1)
  , testCase "Absolute value of point" $
     abs (Point (-4) (-5)) @?= Point 4 5
  , testCase "Points are orderable" $
      (compare a b @?= LT) >> (compare a a @?= EQ) >> (compare b a @?= GT)
  ]    
  where a = Point 1 1  -- "Fixture" data for tests.
        b = Point 2 3


pointGeometryTest :: TestTree
pointGeometryTest = let ?epsilon = 0.001 in testGroup "Test geometric calculations on points"
  [ testCase "Cross function" $
      cross a b @?= 1.0
  , testCase "Dot function" $
      dot a b @?= 5.0
  , testCase "Angle between function" $
      angleBetween a b @~? 0.197395
  , testCase "Mag function" $
      mag a @~? 1.4142135
  , testCase "Mag-squared function" $
      magSquared b @?= 13.0
  , testCase "Mirror point function" $
      mirror a b c @?= Point 1 1
  , testCase "Rotate point function" $
      let Point rx ry = rotate a b 3 in (rx @~? 3.272232) >> (ry @~? 4.838864)
  , testCase "Point approximate equality" $
      (approxEqual a b 0.1 @?= False) >>
      (approxEqual a a 0.1 @?= True) >>
      (approxEqual (Point 1.345 0.123) (Point 1.344 0.122) 0.01 @?= True) >>
      (approxEqual (Point 1.345 0.123) (Point 1.344 0.122) 0.001 @?= False)
  ]
  where a = Point 1 1
        b = Point 2 3
        c = Point 1 2
