{-# LANGUAGE OverloadedStrings #-}
module RL where

import Pine
import qualified SDL
import Foreign.C.Types (CInt (..))

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Char (ord)

data RLState = RLState
  { asciiSet :: Vector Image
  , consoleResolution :: (CInt, CInt) --should be CInt?
  , tileResolution :: (CInt, CInt)
  }

defaultInitial :: (CInt, CInt) -> (CInt, CInt, CInt) -> RLState
defaultInitial res (tWidth, tHeight, tScale) = RLState
  (tilesFromImg "src/Media/ascii.png" tWidth tHeight tScale)
  res
  (tWidth*tScale, tWidth*tScale)

tilesFromImg :: FilePath -> CInt -> CInt -> CInt -> Vector Image
tilesFromImg fp w h scale =
  V.fromList
    [newImage fp (Just $ rect (x*w) (y*h) w h) | y <- [0..15], x <- [0..15]]

instance Stateful RLState where
  update _ Load                      = contLog "Hello, Pine!"
  update _ WindowClose               = quit
  update _ (KeyPressed SDL.KeycodeQ) = quitLog "Goodbye, Pine!"
  update _ _                         = cont

instance Drawable RLState where
  draw rl = fromImages $ V.imap (\i c -> (aset V.! c, Just $ rect (xCoord i) (yCoord i) sx sy)) (toAscii "Hello, World!")
    where
      aset = asciiSet rl
      (w,_h) = consoleResolution rl
      (sx,sy) = tileResolution rl
      xCoord i = sx * (fromIntegral i `mod` w)
      yCoord i = sy * (fromIntegral i `div` w)

toAscii :: String -> Vector Int
toAscii = V.fromList . (fmap ord)

myConfig w h = withDefaultConfig
  { SDL.windowHighDPI = True
  , SDL.windowInitialSize = SDL.V2 w h
  }

-- brogue is about 100x34 rectangular tiles (manually counted)
-- I'm thinking of doing 100x75 square tiles (4:3)

runRL :: IO ()
runRL = pine "Hello Pine" config initState
  where
    --initState = defaultInitial (100, 75) (10, 10, 2)
    initState = defaultInitial (60, 30) (10, 10, 4)
    (w,h) = consoleResolution initState
    (sw,sh) = tileResolution initState
    config = myConfig (w*sw) (h*sh)
