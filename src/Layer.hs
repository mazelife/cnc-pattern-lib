{-# LANGUAGE ExistentialQuantification, GADTs, FlexibleInstances #-}

-----------------------------------------------------------------------------
-- A module for creating collections of heterogenous shape types, applying
-- transformations to all of them, and rendering them as SVG elements.
-----------------------------------------------------------------------------

module Layer
    ( ShapeLike
    , pattern MkShape
    , Layer
    , pack
    , (<+>)
    , (+:)
    , translate
    , translateP
    , rotate
    , mirror
    , offset
    , layerToSvg
    , layerToSvgWithStyle ) where

import Control.Monad (sequence_)

import Text.Blaze.Svg11 (Svg, g)

import qualified Point as P
import qualified Shape as S
import Style

-- | We use an existential type to wrap anything that meets our defintition of a shape.
-- That is to say it can be shown, can be transformed according to methods in the
-- Transformable typeclass, and can be converted to an SVG element. This includes
-- arcs, circles, lines, and rectangles. Heterogenous shapes can all be placed in a single
-- list and transformed via this type.
data ShapeLike where
    MkShape :: (Show a, S.SvgShape a, S.Transformable a) => a -> ShapeLike

-- | Any shape-like thing can be shown using the underlying shape's show method. 
instance Show ShapeLike where
    show (MkShape a) = show a

-- | A constructor to create a shape-like object from any shape. 
pack :: (Show a, S.SvgShape a, S.Transformable a) => a -> ShapeLike
pack = MkShape


instance S.SvgShape ShapeLike where
    toSvg (MkShape a) = S.toSvg a


instance S.Transformable ShapeLike where
    translate (MkShape a) v               = pack $ S.translate a v
    translateP (MkShape a) p              = pack $ S.translateP a p
    rotate (MkShape a) p v                = pack $ S.rotate a p v
    mirror (MkShape a) p v                = pack $ S.mirror a p v
    offset (MkShape a) d leftSide         = pack $ S.offset a d leftSide


-- | A layer is just a sequence of shape-like things
type Layer = [ShapeLike]


instance S.SvgShape [ShapeLike] where
    toSvg layer = g $ sequence_ (map S.toSvg layer) 


-- | Layer constructor:
infixr 6 <+>
(<+>) :: (Show a, S.SvgShape a, S.Transformable a) => a -> a -> Layer
(<+>) a b = [pack a, pack b]


-- | Cons operation on a layer:
infixr 5 +:
(+:) :: (Show a, S.SvgShape a, S.Transformable a) => a -> Layer -> Layer
(+:) = (:) . pack


-- | Functions that mirror those in the Transformable typeclass, but act on a
-- whole layer of shapes, rather than a single shape.

translate :: Layer -> Float -> Layer
translate layer v = map (\s -> S.translate s v) layer


translateP :: Layer -> P.Point -> Layer
translateP layer p = map (\s -> S.translateP s p) layer


rotate :: Layer -> P.Point -> Float -> Layer
rotate layer p v = map (\s -> S.rotate s p v) layer


mirror :: Layer -> P.Point -> P.Point -> Layer
mirror layer p v = map (\s -> S.mirror s p v) layer


offset :: Layer -> P.Point -> Bool -> Layer
offset layer p leftSide = map (\s -> S.offset s p leftSide) layer


-- | Wrap shapes in a layer into an SVG <g> element.
layerToSvg :: Layer -> Svg
layerToSvg grp = let elements = map S.toSvg grp in g $ sequence_ elements


-- | Wrap shapes in a layer into an SVG <g> element. Apply the given styles to the group.
layerToSvgWithStyle :: Layer -> StyleAttrs -> Svg
layerToSvgWithStyle layer style = let elements = map S.toSvg layer in 
    applyStyle style (g $ sequence_ elements)