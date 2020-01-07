module Scenes.Registry
    ( getSceneOrExit
    , scenes
    , scenesLabel ) where


import Data.List (intercalate)
import qualified Data.Map.Lazy as Map
import System.Exit (die)

import Scene (Scene)
import qualified Scenes.HuffmanTowerOptimized as HTO (getScene)
import qualified Scenes.HuffmanTower as HT (getScene)


scenes :: Map.Map String (IO Scene)
scenes = Map.fromList 
    [ ("huffman-tower", HT.getScene)
    , ("huffman-tower-optimized", HTO.getScene)]


getSceneOrExit :: String -> IO Scene
getSceneOrExit sceneName = case Map.lookup sceneName scenes of
    Just s  -> s
    Nothing -> die ("Scene with the name \"" ++ sceneName ++ "\" does not exist.")

scenesLabel :: String
scenesLabel = intercalate ", " (Map.keys scenes)