module CLIOpts (RenderOpts, renderOpts, scene, preview) where

import Options.Applicative
import Data.Semigroup ((<>))

import Scenes.Registry (scenesLabel)



data RenderOpts = RenderOpts
  { scene      :: String
  , preview    :: Bool }



renderOpts :: Parser RenderOpts
renderOpts = RenderOpts
        <$> argument str
            ( metavar "SCENE"
           <> help ("Name of scene to render. One of: " ++ scenesLabel))
        <*> switch
            ( long "preview"
           <> help  "Show SVG in preview window.")

