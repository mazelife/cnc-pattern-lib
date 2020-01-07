module ApproxEq 
    (
      ApproxEq
    , approxEqual
    , (=~)
    , assertApproxEqual
    , (@?~)
    ) where

import Control.Monad  ( unless )
import GHC.Stack (HasCallStack)
import qualified Test.HUnit as H


-- A typeclass for comparing shapes that are approxmately the same size
-- and have the same position in 2-d space.
class Show s => ApproxEq s where
    
    -- Are two shapes identical to within a given tolerance?
    approxEqual :: s -> s -> Float -> Bool

    -- Are two shapes identical to within a given tolerance?
    -- Infix version where epsilon is fixed at 0.0001.
    (=~) :: s -> s -> Bool
    (=~) a b = approxEqual a b 0.0001

    -- Helpful assertion for testing. 
    assertApproxEqual :: (HasCallStack) => s -> s -> Float -> H.Assertion
    assertApproxEqual expected actual epsilon = let eq = approxEqual expected actual epsilon in
        unless eq (H.assertFailure msg)
        where  msg = "expected: " ++ show expected ++ "\n but got: " ++ show actual

    -- Helpful assertion for testing. 
    -- Infix version where epsilon is fixed at 0.01.
    (@?~) :: s -> s -> H.Assertion
    (@?~) expected actual = assertApproxEqual expected actual 0.01  

