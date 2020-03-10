{-# LANGUAGE ExistentialQuantification, GADTs, FlexibleInstances #-}

{-|
A module for creating collections of heterogenous shape types with uniform styling
and rendering them as SVG elements.
-}

module Layer
    ( ShapeLike
    , pattern MkShape
    , Layer
    , shapes
    , style
    , mkLayer
    , mkLayerWithStyle    
    , pack
    , (+:) ) where

import Control.Monad (mapM_)

import Text.Blaze (stringComment)
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Text.Blaze.Svg11 (g)

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

instance Show ShapeLike where
    show (MkShape a) = show a

instance Eq ShapeLike where
    (==) a b = renderSvg  (S.toSvg a) == renderSvg (S.toSvg b)

instance S.SvgShape ShapeLike where
    toSvg (MkShape a) = S.toSvg a

instance S.Transformable ShapeLike where
    translate v (MkShape a)               = pack $ S.translate v a
    translateP p (MkShape a)              = pack $ S.translateP p a
    rotate p v (MkShape a)                = pack $ S.rotate p v a
    mirror p v (MkShape a)                = pack $ S.mirror p v a
    offset d leftSide (MkShape a)         = pack $ S.offset d leftSide a

instance S.SvgShape [ShapeLike] where
    toSvg layer = g $ mapM_ S.toSvg layer

-- | A layer is just a sequence of shape-like things with a name and style
data Layer = Layer { name   :: !String
                   , shapes :: [ShapeLike]
                   , style  :: Maybe StyleAttrs 
                   } deriving (Show, Eq)

-- | Layer constructor from a list of shapes: without style
mkLayer :: (Show a, S.SvgShape a, S.Transformable a) => String -> [a] -> Layer
mkLayer layerName ts = Layer layerName (map pack ts) Nothing 

-- | Layer constructor from a list of shapes: with style
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
--
--      @layer = mkLayer "layer-1" [shape1, shape3, shape3]@
--
--      @newlayer = shape+: layer@
--
infixr 5 +:
(+:) :: (Show a, S.SvgShape a, S.Transformable a) => a -> Layer -> Layer
(+:) shape layer = layer { shapes = pack shape : shapes layer}
