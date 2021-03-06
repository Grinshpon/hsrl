module Main where

import Grid

import RL

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
  runRL
