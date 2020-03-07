module Scenes.Registry
    ( getSceneOrExit
    , scenes
    , scenesLabel ) where


import Data.List (intercalate)
import qualified Data.Map.Lazy as Map
import System.Exit (die)

import Scene (Scene)

import qualified Scenes.HobermanCylinder as H (getScene)
import qualified Scenes.HuffmanTower as HT (getScene)
import qualified Scenes.Resch as R (getScene)
import qualified Scenes.Simple as S (getScene)
import qualified Scenes.Triangles as TR (getScene)

scenes :: Map.Map String (IO Scene)
scenes = Map.fromList 
    [ ("hoberman-cylinder", H.getScene)
    , ("huffman-tower", HT.getScene)
    , ("resch", R.getScene)
    , ("simple", S.getScene)
    , ("triangles", TR.getScene)]


getSceneOrExit :: String -> IO Scene
getSceneOrExit sceneName = case Map.lookup sceneName scenes of
    Just s  -> s
    Nothing -> die ("Scene with the name \"" ++ sceneName ++ "\" does not exist.")

scenesLabel :: String
scenesLabel = intercalate ", " (Map.keys scenes)