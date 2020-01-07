import Test.Tasty

import ArcTest
import CircleTest
import LineTest
import MergeTest
import PointTest


main :: IO ()
main = defaultMain $ tests

tests :: TestTree
tests = testGroup "Tests" [ pointArithmeticTest
                          , pointGeometryTest
                          , lineOpsTest
                          , lineGeometryTest
                          , circleGeometryTest
                          , arcRenderingTest
                          , arcGeometryTest
                          , mergableTest]

