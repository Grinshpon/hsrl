{-# LANGUAGE OverloadedStrings #-}

module Main where

import Grid

import Pine
import qualified SDL
import Foreign.C.Types (CInt)

import Data.Vector (Vector)
import qualified Data.Vector as V

data MyGrid = MyGrid [[Int]]

instance Grid MyGrid where
  getWeight (MyGrid ls) x y = if (ls !! y) !! x == 0 then 1 else 0
  getHeight (MyGrid ls) x y = (ls !! y) !! x
  isOpaque (MyGrid ls) x y = (ls !! y) !! x > 0
  isKnown _ _ _ = True
  width (MyGrid (l:_)) = length l
  height (MyGrid ls) = length ls

myGrid = MyGrid
  [[0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[1,1,0,0,1,1,1,1,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[1,1,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]
  ,[0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0]]

data RLState = RLState
  { logo :: Image
  , tileset :: Vector Image
  , consoleResolution :: (Int, Int) --should be CInt?
  , tileResolution :: (Int, Int)
  }

defaultInitial = RLState
  (newImage "src/Media/logo.png" Nothing (Just $ rect 0 0 400 400))
  (tilesFromImg "src/Media/ascii.png" 10 10 10)
  (100, 75)
  (10, 10)

tilesFromImg :: FilePath -> CInt -> CInt -> CInt -> Vector Image
tilesFromImg fp w h scale =
  V.fromList
    [newImage fp (Just $ rect (x*w) (y*h) w h) dim | y <- [0..15], x <- [0..15]]
  where
    dim = Just $ rect 0 0 (w*scale) (h*scale)

instance Stateful RLState where
  update _ Load                      = contLog "Hello, Pine!"
  update _ WindowClose               = quit
  update _ (KeyPressed SDL.KeycodeQ) = quitLog "Goodbye, Pine!"
  update _ _                         = cont

instance Drawable RLState where
  draw rl = fromImage $ (tileset rl) V.! 1 --translated 200 200 $ fromImage $ logo rl

myConfig = withDefaultConfig
  { SDL.windowHighDPI = True
  , SDL.windowInitialSize = SDL.V2 1000 750
  }

-- brogue is about 100x34 "tiles" (manually counted)
-- I'm thinking of doing 100x75 square tiles (4:3)

main :: IO ()
main = do
  --print $ pointsOnLine 0 0 5 7
  --print $ pointsOnLine 0 0 5 5
  --print $ pointsOnLine 5 5 0 0
  --print $ isOpaque myGrid 10 10
  --print $ getHeight myGrid 10 10
  --print $ isOccluded myGrid 0 0 5 5
  --print $ isOccluded myGrid 0 0 10 10
  --print $ perim (0,0,15,15)
  --print $ calcFov myGrid (0,0,24,17) (5,4)

  --printFov myGrid $ calcFov myGrid (0,0,24,17) (5,4)
  --printPath myGrid (1,1) (5,5)
  --printPath myGrid (0,0) (12,3)
  --printPath myGrid (5,5) (12,3)
  --printPath myGrid (0,0) (1,15)
  pine "Hello Pine" myConfig defaultInitial
