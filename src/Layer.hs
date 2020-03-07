{-# LANGUAGE ExistentialQuantification, GADTs, FlexibleInstances #-}

-----------------------------------------------------------------------------
-- A module for creating collections of heterogenous shape types, applying
-- transformations to all of them, and rendering them as SVG elements.
-----------------------------------------------------------------------------

module Layer
    ( ShapeLike
    , pattern MkShape
    , Layer
    , shapes
    , style
    , mkLayer
    , mkLayerWithStyle    
    , pack
    , (+:)
    , translate
    , translateP
    , rotate
    , mirror
    , offset ) where

import Control.Monad (sequence_)

import Text.Blaze (stringComment)
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Text.Blaze.Svg11 (g)


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

-- | A constructor to create a shape-like object from any shape. 
pack :: (Show a, S.SvgShape a, S.Transformable a) => a -> ShapeLike
pack = MkShape

-- | Any shape-like thing can be shown using the underlying shape's show method. 
instance Show ShapeLike where
    show (MkShape a) = show a

instance Eq ShapeLike where
    (==) a b = renderSvg  (S.toSvg a) == renderSvg (S.toSvg b)

instance S.SvgShape ShapeLike where
    toSvg (MkShape a) = S.toSvg a

instance S.Transformable ShapeLike where
    translate (MkShape a) v               = pack $ S.translate a v
    translateP (MkShape a) p              = pack $ S.translateP a p
    rotate (MkShape a) p v                = pack $ S.rotate a p v
    mirror (MkShape a) p v                = pack $ S.mirror a p v
    offset (MkShape a) d leftSide         = pack $ S.offset a d leftSide

instance S.SvgShape [ShapeLike] where
    toSvg layer = g $ sequence_ (map S.toSvg layer)

-- | A layer is just a sequence of shape-like things with a name and style
data Layer = Layer { name   :: !String
                   , shapes :: [ShapeLike]
                   , style  :: Maybe StyleAttrs 
                   } deriving (Show, Eq)

-- | Layer constructors
mkLayer :: (Show a, S.SvgShape a, S.Transformable a) => String -> [a] -> Layer
mkLayer layerName ts = Layer layerName (map pack ts) Nothing 

mkLayerWithStyle :: (Show a, S.SvgShape a, S.Transformable a) => String -> [a] -> StyleAttrs -> Layer
mkLayerWithStyle layerName ts st = Layer layerName (map pack ts) (Just st)


instance S.SvgShape Layer where
    toSvg layer = maybeApplyStyle (style layer) group
        where 
            comment = stringComment ("layer: " ++ name layer)
            group   = g $ sequence_ (comment : [S.toSvg $ shapes layer])

-- | Layers can be combined.
instance Semigroup Layer where
    (<>) (Layer leftName leftShapes leftStyle) (Layer rightName rightShapes _) 
        | leftName == rightName  = Layer leftName (leftShapes ++ rightShapes) leftStyle
        | otherwise = Layer (leftName ++ rightName) (leftShapes ++ rightShapes) leftStyle
    
-- | There exists an identity element for any layer.
instance Monoid Layer where
    mempty = Layer "" [] Nothing

-- | Cons operation on a layer:
infixr 5 +:
(+:) :: (Show a, S.SvgShape a, S.Transformable a) => a -> Layer -> Layer
(+:) shape layer = layer { shapes = pack shape : shapes layer}

-- | Functions that mirror those in the Transformable typeclass, but act on a
-- whole layer of shapes, rather than a single shape.

translate :: Layer -> Float -> Layer
translate layer v = layer { shapes = map (\s -> S.translate s v) (shapes layer) }


translateP :: Layer -> P.Point -> Layer
translateP layer p = layer { shapes = map (\s -> S.translateP s p) (shapes layer) }


rotate :: Layer -> P.Point -> Float -> Layer
rotate layer p v = layer { shapes = map (\s -> S.rotate s p v) (shapes layer) }


mirror :: Layer -> P.Point -> P.Point -> Layer
mirror layer p v = layer { shapes = map (\s -> S.mirror s p v) (shapes layer) }


offset :: Layer -> P.Point -> Bool -> Layer
offset layer p leftSide = layer { shapes = map (\s -> S.offset s p leftSide) (shapes layer) }