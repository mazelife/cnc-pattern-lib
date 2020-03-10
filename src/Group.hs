{-|
A module for creating collections of homogenous shape types, optimizing
shapes (combining paths when possible or removing duplicates), applying
transformations to all of them, and rendering them as SVG elements.
-} 

module Group where 

import Control.Monad (mapM_)
import Data.List (groupBy, sort)
import System.IO (hPutStrLn, stderr)
import Text.Blaze.Svg11 (Svg, g)
import Text.Printf (printf)

import ApproxEq
import Layer (Layer, mkLayer)
import qualified Shape as S
import Style

-- | A type reprresenting a named group of some type of shape.
newtype Group shape = Group [shape]

instance (Show a) => Show (Group a) where
    show (Group as) = show as

-- | Groups can be combined.
instance (Show a, S.SvgShape a, S.Transformable a, S.Mergable a) => Semigroup (Group a) where
    (<>) (Group as) (Group bs) = Group (as ++ bs)

-- | There exists an identity element for any group.
instance (Show a, S.SvgShape a, S.Transformable a, S.Mergable a) => Monoid (Group a) where
    mempty = Group []

-- | A group can be mapped over just like any other container.
instance Functor Group where 
    fmap fn (Group as) = Group (fmap fn as)

instance Applicative Group where
    pure a = Group [a]
    (<*>) (Group fn) (Group as) = Group ([f a | (f, a) <- (zip fn as)])


-- | Groups can be transformed in the same way single shapes can.
instance (S.Transformable a) => S.Transformable (Group a) where
    translate  = fmap . S.translate
    translateP = fmap . S.translateP
    rotate     = (fmap .) . S.rotate
    mirror     = (fmap .) . S.mirror
    offset     = (fmap .) . S.offset

-- | Convert a group to a layer.
toLayer :: (Show a, S.SvgShape a, S.Transformable a) => String -> Group a -> Layer
toLayer name (Group shapes) = mkLayer name shapes

-- | Number of shapes in the group.
size :: Group a -> Int
size (Group as) = length as

-- | Remove any duplicate shapes from this group
deduplicate :: (Ord t, ApproxEq t) => Group t -> Group t
deduplicate (Group grp) = Group (nub grp)
    where nub = map head . groupBy (=~) . sort


transformAndAppend :: (Show a, S.SvgShape a, S.Transformable a, S.Mergable a) => (a -> a) -> Group a -> Group a
transformAndAppend fn (Group grp) = Group (grp <> newShapes)
    where newShapes = map fn grp

-- | Produce a new group where all shapes that can be merged have been, plus all shapes that couldn't be.
optimizeGroup :: (S.Mergable a) => Group a -> Float -> Group a
optimizeGroup (Group grp) epsilon = Group (S.optimize grp epsilon)

-- | Produce a new group where all shapes that can be merged have been, 
-- plus all shapes that couldn't be, logging the result at the end.
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
