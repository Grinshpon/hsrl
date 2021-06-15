{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Grid where

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as VM

--import Data.Map (Map)
--import qualified Data.Map as M

import Data.Set (Set)
import qualified Data.Set as S

import Data.List (sort, sortOn)

class Grid a where
  getWeight :: a -> Int -> Int -> Int -- weight of 0 or negative is impassible
  getHeight :: a -> Int -> Int -> Int -- for z axis
  isOpaque  :: a -> Int -> Int -> Bool -- for fov/line of sight
  isKnown   :: a -> Int -> Int -> Bool -- for drawing
  width     :: a -> Int
  height    :: a -> Int

type BoundingBox = (Int, Int, Int, Int) -- (x, y, width, height)
type Point = (Int, Int)
type Path = [Point]


-- | Visibility map
data VisMap = VisMap
  { visOrigin :: Point
  , visBB  :: BoundingBox
  , visMap :: Vector Bool
  }

instance Show VisMap where
  show vm@VisMap{..} = visString
    where
      toChar x y = if (x,y) == visOrigin then '@' else if getVisibility vm x y then '!' else '.'
      (ox,oy) = visOrigin
      (sx,sy,w,h) = visBB
      visList = [[toChar x y | x <- [0..w-1]] | y <- [0..h-1]]
      --generate h $ \y -> generate w $ \x -> getVisibility vm x y
      visString = concat $ fmap (\ls -> ls ++ "\n") visList

getVisibility :: VisMap -> Int -> Int -> Bool
getVisibility VisMap{..} x y = visMap V.! (y*w+x)
  where
    (_, _, w, _) = visBB

pointsOnLine :: Int -> Int -> Int -> Int -> Vector Point
pointsOnLine x1 y1 x2 y2 =
  if x1 == x2 && y1 == y2
    then V.singleton (x1,y1)
    else
      let
        steep = abs (y2 - y1) > abs (x2 - x1)
        maySwitch = if steep then (\(x,y) -> (y,x)) else id
        [(x1',y1'), (x2',y2')] = sort [maySwitch (x1,y1), maySwitch (x2,y2)]
        dx = x2' - x1'
        dy = abs $ y2' - y1'
        yStep = if y1' < y2' then 1 else -1
        go (xTemp, yTemp, err)
          | xTemp > x2' = Nothing
          | otherwise   = Just ((xTemp, yTemp), (xTemp + 1, newY, newErr))
          where
            tempErr = err + dy
            (newY, newErr) = if 2*tempErr >= dx then (yTemp + yStep, tempErr - dx) else (yTemp, tempErr)
      in fmap maySwitch . V.unfoldr go $ (x1', y1', 0)


isLineOccluded :: Grid g => g -> Vector Point -> Bool
isLineOccluded grid = or . fmap (uncurry (isOpaque grid))

isOccluded :: Grid g => g -> Int -> Int -> Int -> Int -> Bool
isOccluded grid x1 y1 x2 y2 = isLineOccluded grid $ pointsOnLine x1 y1 x2 y2

-- | how far into this line of points does the first opaque tile appear (-1 if visible)
lengthVisible :: Grid g => g -> Vector Point -> Int
lengthVisible g v = len + 1
  where
    visVec = fmap (uncurry (isOpaque g)) v
    len = case V.elemIndex True visVec of
      Just l  -> l
      Nothing -> length v

getVisible :: Grid g => g -> Vector Point -> Vector Point
getVisible g v = V.take (lengthVisible g v) v

{- fov algorithm:

  Have a bounding box, and use bresenham's line algorithm from origin to each point on perimeter of box to calculate visible tiles.
  Store visible tiles in a set.

  When drawing, if tile position is unknown, then don't draw. Else, if it is visible, draw lit, else draw dark.
-}

-- perim :: BoundingBox -> [Point]
-- perim (sx,sy,w,h) = concat
--   [ toLine (sx+1) sy (sx+w-2) sy
--   , toLine (sx+1) (sy+h-1) (sx+w-2) (sy+h-1)
--   , toLine sx (sy+1) sx (sy+h-2)
--   , toLine (sx+w-1) (sy+1) (sx+w-1) (sy+h-2)
--   , [(sx,sy)]
--   , [(sx+w-1,sy)]
--   , [(sx,sy+h-1)]
--   , [(sx+w-1,sy+h-1)]
--   ]
--     where toLine x1 y1 x2 y2 = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

calcFov :: Grid g => g -> BoundingBox -> Point -> VisMap
calcFov g bb@(sx,sy,w,h) origin@(ox, oy) =
  let
    p2i (x,y) = y*w+x
    toLine x1 y1 x2 y2 = [(x,y) | x <- [x1..x2], y <- [y1..y2]] 
    startingMap = V.replicate (w*h) False
    -- get set of visible points
    perimeter :: [Point]
    perimeter = concat
      [ toLine (sx+1) sy (sx+w-2) sy
      , toLine (sx+1) (sy+h-1) (sx+w-2) (sy+h-1)
      , toLine sx (sy+1) sx (sy+h-2)
      , toLine (sx+w-1) (sy+1) (sx+w-1) (sy+h-2)
      , [(sx,sy)]
      , [(sx+w-1,sy)]
      , [(sx,sy+h-1)]
      , [(sx+w-1,sy+h-1)]
      ]
    ptSet = (S.fromList . V.toList) $ V.concat $ fmap (\(px,py) -> getVisible g (pointsOnLine ox oy px py)) perimeter
    ptVec = (V.fromList . S.toList) ptSet

    finalMap = V.update startingMap $ fmap (\pt -> (p2i pt, True)) ptVec
    --finalMap = V.modify (\v -> VM.write v 0 True) startingMap
  in VisMap origin bb finalMap


-- pathfinding inspired by blog: https://blog.sulami.xyz/posts/a-star-pathfinding-in-functional-languages/

type DistFn = Point -> Point -> Int
chebyshev :: DistFn
chebyshev (x1, y1) (x2,y2) = max (abs (x2-x1)) (abs (y2-y1))

manhattan :: DistFn
manhattan (x1,y1) (x2,y2) = abs (x2-x1) + abs (y2-y1)

pythagorean :: DistFn
pythagorean (x1,y1) (x2,y2) = (round . sqrt . fromIntegral) $ (abs (x2-x1))^2 + (abs (y2-y1))^2

cost :: Grid g => g -> Point -> DistFn -> Path -> Int
cost g target dist path = pathWeight + dist (last path) target `div` 2 -- | dividing the heuristic by 2 makes it work better
  where
    pathWeight = (sum . tail) $ fmap (uncurry (getWeight g)) path

-- given a point on the grid, which tiles around it can it move to?
possibleTiles :: Grid g => g -> Point -> [Point]
possibleTiles g (sx,sy) = [(x,y) |  x <- [sx-1..sx+1], x >= 0, x < width g,
                                    y <- [sy-1..sy+1], y >= 0, y < height g,
                                    --sx-x == 0 || sy-y == 0,
                                    x /= 0 || y /= 0,
                                    getWeight g x y > 0]

findPath :: Grid g => g -> Point -> Point -> Maybe Path
findPath grid starting target =
  case flood [[starting]] of
    [] -> Nothing
    [path] -> Just path
    paths -> error "just checking" --Just $ head $ sortOn (cost grid target distFn) paths
  where
    --distFn = manhattan
    distFn = chebyshev
    --distFn = pythagorean
    flood :: [Path] -> [Path]
    flood paths
      | any (\p -> last p == target) paths = filter (\p -> last p == target) paths
      | otherwise =
        let
          npaths = zip (fmap (cost grid target distFn) paths) paths
        in if null npaths then [] else
          let
            best = snd $ minimum npaths
            pb = addRoutes paths best
          in flood $ filter (/= best) paths ++ pb

    addRoutes :: [Path] -> Path -> [Path]
    addRoutes paths path = let cps = concat paths in [ path ++ [p] | p <- filter (`notElem` cps) $ possibleTiles grid $ last path ]


printPath :: Grid g => g -> Point -> Point -> IO ()
printPath g p1 p2 = putStrLn $ showPath g p1 p2

showPath :: Grid g => g -> Point -> Point -> String
showPath g start target =
  case findPath g start target of
    Nothing -> "No Path Found"
    Just path ->
      let
        toChar x y = if (x,y) `elem` path then '*' else if getWeight g x y < 1 then '#' else '.'
        mp = [[ toChar x y | x <- [0..width g - 1]] | y <- [0..height g - 1]]
      in concat $ fmap (++ "\n") mp

printFov :: Grid g => g -> VisMap -> IO ()
printFov g vm = putStrLn $ showFov g vm

showFov :: Grid g => g -> VisMap -> String
showFov g vm@VisMap{..} = visString
  where
    toChar x y = if (x,y) == visOrigin then '@' else if getVisibility vm x y then if getWeight g x y > 0 then '.' else '#' else ' '
    (ox,oy) = visOrigin
    (sx,sy,w,h) = visBB
    visList = [[toChar x y | x <- [0..w-1]] | y <- [0..h-1]]
    visString = concat $ fmap (\ls -> ls ++ "\n") visList
