module Shape
    ( SvgShape
    , toSvg
    , Transformable 
    , translate
    , translateP
    , rotate
    , mirror
    , offset
    , Mergable
    , merge
    , optimize
    ) where


import qualified Text.Blaze.Svg11 as S

import Point (Point)


-- Typeclass for shapes that can be converted to SVG elements 
class SvgShape s where
    toSvg :: s -> S.Svg


-- Typeclass for shapes that have transformation operations in cartesian space
class Transformable s where
    -- Move a shape in space adding the given scalar to the X and Y axes.
    translate  :: s -> Float -> s
    
    -- Move a shape in space adding the given point to the X and Y axes.
    translateP :: s -> Point -> s
    
    -- Rotate a shape about a line through point p along vector t
    rotate     :: s -> Point -> Float -> s
    
    -- Mirror a shape about a line through point p along vector v    
    mirror     :: s -> Point -> Point -> s
    
    offset     :: s -> Point -> Bool -> s


class Mergable s where    
    -- If two shapes can be merged into one, returen the merged shape
    -- Makes the most sense for lines and arcs.
    merge    :: s -> s -> Float -> Maybe s
    
    -- Merge all shapes in a given list that can be merged
    optimize :: [s] -> Float -> [s]
    optimize ms = optimize' ms []


-- | Recursively merge all items in a list--if possible--returning the
-- unique set of merged and unmergable items.
optimize' :: (Mergable m) => [m] -> [m] -> Float -> [m]
optimize' [] acc _           = acc
optimize' (m:ms) acc epsilon = optimize' remainder (mn : acc) epsilon
    where (mn, remainder) = mergeCandidate m ms [] epsilon


-- | Given one item and a list or remaining items, merge that item with 
-- everything possible in the remainder. Return the merged item and anything
-- from the remainder that was not merged.
mergeCandidate :: (Mergable m) => m -> [m] -> [m] -> Float -> (m, [m])
mergeCandidate m [] acc _           = (m, acc)
mergeCandidate m (n:ns) acc epsilon = case merge m n epsilon of
    Just mn -> mergeCandidate mn ns acc epsilon
    Nothing -> mergeCandidate m ns (n : acc) epsilon
    