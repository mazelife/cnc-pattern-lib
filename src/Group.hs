module Group where 

import Control.Monad (mapM_)
import Data.List (groupBy, sort)
import System.IO (hPutStrLn, stderr)
import Text.Blaze.Svg11 (Svg, g)
import Text.Printf (printf)

import ApproxEq
import Layer (Layer, mkLayer)
import qualified Point as P
import qualified Shape as S
import Style


-----------------------------------------------------------------------------
-- A module for creating collections of homogenous shape types, optimizing
-- shapes (combining paths when possible or removing duplicates), applying
-- transformations to all of them, and rendering them as SVG elements.
-----------------------------------------------------------------------------


-- | A  type represenitng a named group of some type of shape.
newtype Group shape = Group [shape]

instance (Show a) => Show (Group a) where
    show (Group as) = show as

-- | Groups can be combined.
instance (Show a, S.SvgShape a, S.Transformable a, S.Mergable a) => Semigroup (Group a) where
    (<>) (Group as) (Group bs) = Group (as ++ bs)

--- | There exists an identify element for any group.
instance (Show a, S.SvgShape a, S.Transformable a, S.Mergable a) => Monoid (Group a) where
    mempty = Group []

instance (S.Transformable a) => S.Transformable (Group a) where

    translate (Group ts) v = Group $ map (`S.translate` v) ts
    
    translateP (Group ts) p = Group $ map (`S.translateP` p ) ts
    
    rotate (Group ts) p v = Group $ map (\t -> S.rotate t p v) ts

    mirror (Group ts) p v = Group $ map (\t -> S.mirror t p v) ts

    offset (Group ts) p leftSide = Group $ map (\t -> S.offset t p leftSide) ts

toLayer :: (Show a, S.SvgShape a, S.Transformable a) => String -> Group a -> Layer
toLayer name (Group shapes) = mkLayer name shapes

size :: Group a -> Int
size (Group as) = length as

-- | Remove any duplicate shapes from this group
deduplicate :: (Ord t, ApproxEq t) => Group t -> Group t
deduplicate (Group grp) = Group (nub grp)
    where nub = map head . groupBy (=~) . sort


transformAndAppend :: (Show a, S.SvgShape a, S.Transformable a, S.Mergable a) => (a -> a) -> Group a -> Group a
transformAndAppend fn (Group grp) = Group (grp <> newShapes)
    where newShapes = map fn grp

optimizeGroup :: (S.Mergable a) => Group a -> Float -> Group a
optimizeGroup (Group grp) epsilon = Group (S.optimize grp epsilon)


optimizeGroupAndLog :: (S.Mergable a) => Group a -> Float -> IO (Group a)
optimizeGroupAndLog group epsilon = hPutStrLn stderr mssg >> return optGrp
  where 
    _start = size group
    optGrp = optimizeGroup group epsilon
    _end   = size optGrp
    mssg   = printf "Optimized group: %d shapes to %d" _start _end

-- | Wrap a group of shapes into an SVG <g> element, including a comment
-- with the name of the group.
toSvgN :: (S.SvgShape s) => Group s -> Svg
toSvgN (Group grp)  = g $ mapM_ S.toSvg grp


-- | Wrap a group of shapes into an SVG <g> element, including a comment
-- with the name of the group. Apply the given styles to the group.
toSvgWithStyle :: (S.SvgShape s) => Group s -> StyleAttrs -> Svg
toSvgWithStyle grp s = applyStyle s $ toSvgN grp
