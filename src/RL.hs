{-# LANGUAGE OverloadedStrings #-}
module RL where

import Pine
import qualified SDL
import Foreign.C.Types (CInt)

import Data.Vector (Vector)
import qualified Data.Vector as V

data RLState = RLState
  { logo :: Image
  , tileset :: Vector Image
  , consoleResolution :: (Int, Int) --should be CInt?
  , tileResolution :: (Int, Int)
  }

defaultInitial = RLState
  (newImage "src/Media/logo.png" Nothing (Just $ rect 0 0 400 400))
  (tilesFromImg "src/Media/ascii.png" 10 10 1)
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

runRL :: IO ()
runRL = pine "Hello Pine" myConfig defaultInitial
