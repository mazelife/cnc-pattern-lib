module Group where 

import Control.Monad (sequence_)
import Data.List (groupBy, sort)
import Text.Blaze  (stringComment)
import Text.Blaze.Svg11 (Svg, g)

import ApproxEq
import Layer (Layer, pack)
import qualified Point as P
import qualified Shape as S
import Style


-----------------------------------------------------------------------------
-- A module for creating collections of homogenous shape types, optimizing
-- shapes (combining paths when possible or removing duplicates), applying
-- transformations to all of them, and rendering them as SVG elements.
-----------------------------------------------------------------------------


-- | A  type represenitng a named group of some type of shape.
data Group shape = Group { name   :: !String    -- ^ Name of the group
                         , shapes :: [shape]    -- ^ Shapes in the group 
                         } deriving (Show, Eq)



-- | Groups can be combined.
instance (Show a, S.SvgShape a, S.Transformable a, S.Mergable a) => Semigroup (Group a) where
    (<>) (Group an as) (Group bn bs) = Group (an ++ bn) (as ++ bs)

-- | There exists an identify element for any group.
instance (Show a, S.SvgShape a, S.Transformable a, S.Mergable a) => Monoid (Group a) where
    mempty = Group "" []


-- | Groups can be rendered as a collection of elements under a single svg <g> element.
-- A comment with the group name is included in the group.
instance (S.SvgShape s) => S.SvgShape (Group s) where
    toSvg (Group n ts) = g $ sequence_ elements
      where elements = stringComment ("group: " ++ n) : map S.toSvg ts


-- | Functions that mirror those in the Transformable typeclass, but act on a
-- whole group of shapes, rather than a single shape.
toLayer :: (Show a, S.SvgShape a, S.Transformable a) => Group a -> Layer
toLayer = map pack . shapes


translate :: (S.Transformable t) => Group t -> Float -> Group t
translate (Group n ts) v = Group n (map (`S.translate` v) ts)


translateP :: (S.Transformable t) => Group t -> P.Point -> Group t
translateP (Group n ts) p = Group n (map (`S.translateP` p) ts)


rotate :: (S.Transformable t) => Group t -> P.Point -> Float -> Group t
rotate (Group n ts) p v = Group n (map (\t -> S.rotate t p v) ts)


mirror :: (S.Transformable t) => Group t -> P.Point -> P.Point -> Group t
mirror (Group n ts) p v = Group n (map (\t -> S.mirror t p v) ts)


offset :: (S.Transformable t) => Group t -> P.Point -> Bool -> Group t
offset (Group n ts) p leftSide = Group n (map (\t -> S.offset t p leftSide) ts)


-- | Remove any duplicate shapes from this group
deduplicate :: (Ord t, ApproxEq t) => Group t -> Group t
deduplicate grp = Group (name grp) (nub grp)
    where nub = map head . groupBy (=~) . sort . shapes


transformAndAppend :: (Show a, S.SvgShape a, S.Transformable a, S.Mergable a) => (a -> a) -> Group a -> Group a
transformAndAppend fn grp = grp <> (Group "" newShapes)
    where newShapes = map fn (shapes grp)

optimizeGroup :: (S.Mergable a) => Group a -> Float -> Group a
optimizeGroup (Group n s) epsilon = Group n (S.optimize s epsilon)





-- | Wrap a group of shapes into an SVG <g> element, including a comment
-- with the name of the group.
toSvgN :: (S.SvgShape s) => Group s -> Svg
toSvgN (Group n ts) = g $ sequence_ elements
    where elements = stringComment ("group: " ++ n) : map S.toSvg ts

-- | Wrap a group of shapes into an SVG <g> element, including a comment
-- with the name of the group. Apply the given styles to the group.
toSvgWithStyle :: (S.SvgShape s) => Group s -> StyleAttrs -> Svg
toSvgWithStyle grp s = applyStyle s $ toSvgN grp
