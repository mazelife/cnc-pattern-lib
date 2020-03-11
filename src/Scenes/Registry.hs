{-|
A module for handling registration of scenes so they can be rendered by the 
CLI executable.
-} 

module Scenes.Registry
    ( getSceneOrExit
    , scenes
    , scenesLabel ) where


import Data.List (intercalate)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe)
import System.Exit (die)

import Scene (Scene)

import qualified Scenes.Hexagons as HX (getScene)
import qualified Scenes.HobermanCylinder as H (getScene)
import qualified Scenes.HuffmanTower as HT (getScene)
import qualified Scenes.Resch as R (getScene)
import qualified Scenes.Simple as S (getScene)
import qualified Scenes.Triangles as TR (getScene)

-- | Register scenes and their CLI lable here.
scenes :: Map.Map String (IO Scene)
scenes = Map.fromList 
    [ ("hexagons", HX.getScene)
    , ("hoberman-cylinder", H.getScene)
    , ("huffman-tower", HT.getScene)
    , ("resch", R.getScene)
    , ("simple", S.getScene)
    , ("triangles", TR.getScene)]

-- | Return the named scene or exit the program with an error if it doesn't exist.
getSceneOrExit :: String -> IO Scene
getSceneOrExit sceneName =  fromMaybe
    (die
       ("Scene with the name \"" ++ sceneName ++ "\" does not exist."))
    (Map.lookup sceneName scenes)

-- | Comma-separated list of all scene labels registered in thei module.
scenesLabel :: String
scenesLabel = intercalate ", " (Map.keys scenes)