{-# LANGUAGE ImplicitParams #-}

module MergeTest where

import Data.List ((\\))


import Test.Tasty
import Test.Tasty.HUnit

import Line (pattern Line)
import Point (pattern Point)
import Shape



listsContainSameElements :: (Eq a) => [a] -> [a] -> Bool
listsContainSameElements a b = null (a \\ b) && null (b \\ a)


mergableTest :: TestTree
mergableTest = testGroup "Test operations in Mergable typeclass" 
  [ testCase "Line segments merge correctly" $
        (merge l1 l2 0.01 @?= Just (Line a c)) >>
        (merge l2 l3 0.01 @?= Nothing) >>
        (merge l3 l4 0.01 @?= Just (Line d f))
  ,  testCase "A list of mergable object can be optimized" $
        listsContainSameElements (optimize [l1, l2, l3, l4] 0.001) [Line a c, Line d f] @?= True
  ]
  -- | Our test data is a series of 6 colinear points that will be used to construct 
  -- overlapping (and thus mergable) line segments. 
  where
    a  = Point (-2) (-2)
    b  = Point 0 0
    c  = Point 1.5 1.5
    d  = Point 3 3
    e  = Point 4.5 4.5
    f  = Point 5 5
    l1 = Line a c
    l2 = Line b c -- ^ l1 and l2 are mergable
    l3 = Line d e
    l4 = Line e f -- ^ l3 and l4 are mergable
