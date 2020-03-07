module Lib (render) where

import System.IO (hPutStrLn, stderr)

import Options.Applicative ((<**>), execParser, fullDesc, header, helper, info, progDesc)

import CLIOpts (RenderOpts, renderOpts, scene)
import Scene (renderScene)
import Scenes.Registry (getSceneOrExit)



render :: IO ()
render = execParser opts >>= outputScene
  where
    opts = info (renderOpts <**> helper)
        ( fullDesc
       <> progDesc "Render a scene as an SVG file."
       <> header "cnc-pattern-lib - a program to render SVG files for CNC routers")



outputScene :: RenderOpts -> IO ()
outputScene opts = do
    hPutStrLn stderr $ "Rendering scene \"" ++ scene opts ++ "\"..."
    svgStr <- renderScene $ getSceneOrExit (scene opts)
    putStrLn svgStr

