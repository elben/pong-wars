{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Monoid
import Foreign.C.Types
import Prelude hiding (any, mapM_)
import SDL.Vect
import qualified SDL
import qualified SDL.Image

import Paths_pong_wars (getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

loadImage :: FilePath -> IO SDL.Surface
loadImage fp = getDataFileName fp >>= SDL.Image.load

loadBMP :: FilePath -> IO SDL.Surface
loadBMP path = getDataFileName path >>= SDL.loadBMP

-- | Load image as Surface on the given Surface. Optimized by converting new
-- Surface to the given Surface.
loadSurface :: SDL.Surface -> FilePath -> IO SDL.Surface
loadSurface screenSurface path = do
  loadedSurface <- loadImage path
  desiredFormat <- SDL.surfaceFormat screenSurface
  SDL.convertSurface loadedSurface desiredFormat <* SDL.freeSurface loadedSurface

loadSurfaceBMP :: SDL.Surface -> FilePath -> IO SDL.Surface
loadSurfaceBMP screenSurface path = do
  loadedSurface <- loadBMP path
  desiredFormat <- SDL.surfaceFormat screenSurface
  SDL.convertSurface loadedSurface desiredFormat <* SDL.freeSurface loadedSurface

data GameState = GameState
  { getBall :: Ball
  -- , posA :: Point V2 CInt-- ^ (x, y) coordinates of paddle A
  -- , posB :: Point V2 CInt -- ^ (x, y) coordinates of paddle B
  }

data Ball = Ball
  {
    getBallPos :: (Float, Float) -- ^ (x, y) coordinates of ball
  , getBallVelocity :: Float      -- ^ Velocity, in pixels/tick.
  , getBallHeading :: Float     -- ^ Degree of direction, where 0 is to the right, counter-clockwise.
  , getBallSpinVelocity :: Float  -- ^ Amount of velocity pushing perpendicular of heading, in pixels/tick.
  }

posUp1 :: Point V2 CInt -> Point V2 CInt
posUp1 (P (V2 x y)) = (P (V2 x (y - 10)))

posDown1 :: Point V2 CInt -> Point V2 CInt
posDown1 (P (V2 x y)) = (P (V2 x (y + 10)))

up1 :: (Float, Float) -> (Float, Float)
up1 (x, y) = (x, y - 1)

down1 :: (Float, Float) -> (Float, Float)
down1 (x, y) = (x, y + 1)

ballUp1 :: Ball -> Ball
ballUp1 ball = ball { getBallPos = up1 (getBallPos ball) }

ballDown1 :: Ball -> Ball
ballDown1 ball = ball { getBallPos = down1 (getBallPos ball) }

ballUpdate :: Ball -> Ball
ballUpdate ball =
  let (x, y) = getBallPos ball
      v = getBallVelocity ball
      a = getBallHeading ball
      s = getBallSpinVelocity ball
      x' = x + (v * sin (toRadian a))
      y' = y + (v * cos (toRadian a))
      a' = a + s
  in ball { getBallPos = (x', y')
          , getBallHeading = a'
          }

toRadian :: Float -> Float
toRadian a = 2 * pi * a

toPV2 :: (Float, Float) -> Point V2 CInt
toPV2 (x, y) = P $ V2 (round x) (round y)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window

  surfaceBackground <- loadSurface screenSurface "resources/images/background.png"
  surfaceBall <- loadSurface screenSurface "resources/images/ball.png"
  surfacePaddle <- loadSurface screenSurface "resources/images/paddle.png"

  surfaceDefault <- loadSurfaceBMP screenSurface "resources/images/press.bmp"
  surfaceUp <- loadSurfaceBMP screenSurface "resources/images/up.bmp"
  surfaceDown <- loadSurfaceBMP screenSurface "resources/images/down.bmp"
  surfaceLeft <- loadSurfaceBMP screenSurface "resources/images/left.bmp"
  surfaceRight <- loadSurfaceBMP screenSurface "resources/images/right.bmp"

  let
    startingGameState = GameState {
                  getBall = Ball {
                    getBallPos = (100, 100)
                  , getBallVelocity = 0.2
                  , getBallHeading = 0.2

                  -- This is not a good model for a curve-ball or spinning
                  -- ball. Just creates a perfect circle. Need some kind of polynomial shape.
                  , getBallSpinVelocity = 0.0001
                  }
                }
    loop oldGameState oldSurface = do
      events <- map SDL.eventPayload <$> SDL.pollEvents
      let quit = SDL.QuitEvent `elem` events

      -- Possibly modify game state, or use the last one.
      let gameState' =
            fromMaybe oldGameState $ getLast $
            foldMap (\eventPayload ->
                      case eventPayload of
                          SDL.KeyboardEvent e | SDL.keyboardEventKeyMotion e == SDL.Pressed ->
                            case SDL.keysymKeycode (SDL.keyboardEventKeysym e) of
                                 SDL.KeycodeUp    -> Last (Just (oldGameState { getBall = ballUp1 (getBall oldGameState) }))
                                 SDL.KeycodeDown  -> Last (Just (oldGameState { getBall = ballDown1 (getBall oldGameState) }))
                                 _  -> Last (Nothing)
                          _ -> Last Nothing)
                    events
      let gameState'' = gameState' { getBall = ballUpdate (getBall gameState') }

      -- SDL.surfaceBlit surfaceBackground Nothing screenSurface Nothing
      SDL.surfaceBlit surfaceBall Nothing screenSurface (Just (toPV2 (getBallPos (getBall gameState''))))
      SDL.updateWindowSurface window

      unless quit (loop gameState'' screenSurface)

  -- Start the main loop.
  loop startingGameState surfaceBackground

  mapM_ SDL.freeSurface [ surfaceBackground, surfaceDefault, surfaceUp, surfaceDown, surfaceRight, surfaceLeft ]
  SDL.destroyWindow window
  SDL.quit


main2 :: IO ()
main2 = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "SDL Tutorial" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window
  screenSurface <- SDL.getWindowSurface window

  helloWorld <- loadImage "resources/images/background.png"

  SDL.surfaceBlit helloWorld Nothing screenSurface Nothing
  SDL.updateWindowSurface window

  threadDelay 2000000

  SDL.destroyWindow window
  SDL.freeSurface helloWorld
  SDL.quit
