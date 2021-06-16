module Pine.Internal.Types where

import qualified SDL -- (Event, Rectangle(..), WindowConfig, Keycode(..))
import qualified SDL.Vect as SDLV
import Foreign.C.Types (CInt)
import Data.Int (Int32)
import Linear (V2)

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Semigroup
import Control.Monad.State -- see below

type Rectangle = SDL.Rectangle CInt

data Event
  = Load -- ^ very first event to be called
  | Step -- ^ Other events may be called multiple times per frame, but the Step event occurs once per frame
  | KeyPressed  SDL.Keycode
  | KeyReleased SDL.Keycode
  | KeyState --(Key -> Bool)
  | MousePosition (V2 Int32)
  | MouseMoved (V2 Int32)
  | MouseClick MouseButton
  | MouseScroll -- WIP
  | WindowPosition (Double, Double)
  | WindowResized (Double,Double)
  | WindowMinimized
--  | AudioState AudioState
  | WindowClose
  | SDLEvent SDL.Event--(raw SDL data, shouldn't be used typically)
  deriving (Eq, Show)

data MouseButton = MouseLeft | MouseRight | MouseMiddle deriving (Eq, Show)

data Return = Cont | Log String | Quit | QuitWithLog String -- change config settings (like resize window), show/hide mouse

-- | Most apps or games do not run forever, and sometimes people like to log things while building up a project.
-- In order for the user of this framework to do that, they must be able to send values back to the main control loop.
type PineState s = State s Return

cont,quit :: PineState s
cont = pure Cont
quit = pure Quit

contLog,quitLog :: String -> PineState s
contLog s = pure $ Log s
quitLog s = pure $ QuitWithLog s


-- | Drawable class contains the draw function, which takes a type and converts it into a `Scene`
class Drawable d where
  draw :: d -> Scene

type DeltaTime = Double

-- | Stateful class contains initial and update functions. Any objects in your game, including the overrall world, will update according to events that occur
class Stateful s where
  update :: DeltaTime -> Event -> PineState s

-- | An Image which is converted into a `Texture`
data Image = Image
  { imageSrc  :: FilePath -- ^ source file
  , imageQuad :: Maybe Rectangle -- ^ quad, or Nothing for entire image
  --, imageRect :: Maybe (Rectangle) -- ^ location and dimensions, or Nothing to fit entire window
  } deriving (Eq, Show) -- put in other info later (like dimensions, quads, etc)

-- | Construct a rectangle
rect :: ()
     => CInt -- ^ x
     -> CInt -- ^ y
     -> CInt -- ^ w
     -> CInt -- ^ h
     -> Rectangle
rect x y w h = SDL.Rectangle (SDLV.P $ SDLV.V2 x y) (SDLV.V2 w h)


-- | Create an image from a file
newImage :: ()
         => FilePath -- ^ The source of the image file
         -> Maybe Rectangle -- ^ A quad or Nothing for the whole image
         -- -> Maybe Rectangle  -- ^ The rendering target: The location of the image on the window and its size, or Nothing to take up the whole window.
         -> Image
newImage = Image

newtype Audio = Audio -- WIP
  { audiosrc :: FilePath
  } deriving (Eq, Show)

data AudioState = Playing Audio | Stopped Audio deriving (Eq, Show) --WIP

data Playback = Playback Audio | Continue Audio | Stop Audio deriving (Eq, Show) -- WIP

data Media = MImage Image (Maybe Rectangle) | MAudio | MText deriving (Eq, Show) -- WIP (TODO: replace instances of Image in Scene with Media

-- | A Scene can be empty, a single `Image`, or a group of `Image`s. (WIP: Later text and other stuff will be added)
data Scene = EmptyScene | SingleScene Media | MultiScene (Vector Media) deriving (Eq, Show)

instance Semigroup Scene where
  (<>) (SingleScene img1) (SingleScene img2) = MultiScene $ V.fromList [img1,img2]
  (<>) (MultiScene imgs1) (MultiScene imgs2) = MultiScene $ imgs1 <> imgs2
  (<>) (SingleScene img) (MultiScene imgs) = MultiScene $ V.cons img imgs
  (<>) (MultiScene imgs) (SingleScene img) = MultiScene $ V.snoc imgs img
  (<>) EmptyScene c = c
  (<>) c EmptyScene = c

instance Monoid Scene where
  mempty = EmptyScene
  mappend = (<>)

-- | Convert a single `Image` into a `Scene`
fromImage :: Image -> Maybe Rectangle -> Scene
fromImage = SingleScene ... MImage
  where (...) = ((.) . (.))

fromImages :: Vector (Image, Maybe Rectangle) -> Scene
fromImages = MultiScene . fmap (uncurry MImage)

--translateImg :: Image -> CInt -> CInt -> Image
--translateImg img x y =
--  case imageRect img of
--    Nothing -> img
--    Just (SDL.Rectangle pt dim) ->
--      img { imageRect = Just $ SDL.Rectangle (pt + (SDLV.P $ SDLV.V2 x y)) dim }

translated :: CInt -> CInt -> Scene -> Scene
translated x y (SingleScene (MImage img (Just (SDL.Rectangle pt dim)))) = SingleScene $ MImage img $ Just $ SDL.Rectangle (pt + (SDLV.P $ SDLV.V2 x y)) dim
translated x y (MultiScene ms) = MultiScene $ fmap trans ms
  where
    trans m = case m of
                MImage img (Just (SDL.Rectangle pt dim))-> MImage img $ Just $ SDL.Rectangle (pt + (SDLV.P $ SDLV.V2 x y)) dim
                _ -> m
translated _ _ s = s

-- todo: withColor or colored functions
