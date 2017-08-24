{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad hiding (mapM_)
import Data.Foldable hiding (elem)
import Data.Maybe
import Data.Monoid
import Foreign.C.Types
import Prelude hiding (any, mapM_)
import SDL.Vect
import SDL.Video.Renderer
import qualified SDL
import qualified SDL.TTF
import qualified SDL.Image

import Paths_pong_wars (getDataFileName)

maxWidth = 800
maxHeight = 600

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 600)

-- https://hackage.haskell.org/package/sdl2-ttf-1.0.0/src/examples/font_test.hs
-- red :: SDL.Font.Color
-- red = SDL.V4 255 0 0 0

loadImage :: FilePath -> IO SDL.Surface
loadImage fp = getDataFileName fp >>= SDL.Image.load

loadBMPSurface :: FilePath -> IO SDL.Surface
loadBMPSurface path = getDataFileName path >>= SDL.loadBMP

-- | Load image as Surface on the given Surface. Optimized by converting new
-- Surface to the given Surface.
loadSurface :: SDL.Surface -> FilePath -> IO SDL.Surface
loadSurface screenSurface path = do
  loadedSurface <- loadImage path
  desiredFormat <- SDL.surfaceFormat screenSurface
  SDL.convertSurface loadedSurface desiredFormat <* SDL.freeSurface loadedSurface

data GameState = GameState
  { getBall :: Ball
  , getPaddle1 :: Paddle
  , getPaddle2 :: Paddle
  -- , posB :: Point V2 CInt -- ^ (x, y) coordinates of paddle B
  }

-- A Paddle state consist of its position, which represents the center of the
-- paddle, and the half-width/height dimensions.
data Paddle = Paddle
  { getPaddlePos :: (Float, Float) -- ^ (x, y) coordinates of paddle
  , getPaddleHalfWidth :: Float
  , getPaddleHalfHeight :: Float
  }

data Ball = Ball
  { getBallPos :: (Float, Float) -- ^ (x, y) coordinates of center of ball
  , getBallRadius :: Float      -- ^ Radius, in pixels
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

paddleUp1 :: Paddle -> Paddle
paddleUp1 p = p { getPaddlePos = up1 (getPaddlePos p) }

paddleDown1 :: Paddle -> Paddle
paddleDown1 p = p { getPaddlePos = down1 (getPaddlePos p) }

updateBall :: Ball -> Ball
updateBall ball =
  let (x, y) = getBallPos ball
      v = getBallVelocity ball
      a = getBallHeading ball
      x' = x + (v * cos (toRadian a))
      y' = y + (v * sin (toRadian a))

      -- Check for wall bounces
      a' = if x' <= 0 || x' >= maxWidth
             then 0.5 - a
             else a
      a'' = if y' <= 0
             then 1 - a'
             else if y' >= maxHeight
                  then -a'
                  else a'
  in ball { getBallPos = (x', y')
          , getBallHeading = snd (properFraction a'')
          }

normalizeAngle :: Float -> Float
normalizeAngle a = snd (properFraction a)

toRadian :: Float -> Float
toRadian a = 2 * pi * a

toPV2 :: (Float, Float) -> Point V2 CInt
toPV2 (x, y) = P $ V2 (round x) (round y)

toRectBall :: Ball -> Rectangle CInt
toRectBall b =
  let (x, y) = getBallPos b
      r = getBallRadius b
  in Rectangle (toPV2 (x - r, y - r)) (V2 20 20)

toRectPaddle :: Paddle -> Rectangle CInt
toRectPaddle p =
  let (x, y) = getPaddlePos p
      (halfW) = getPaddleHalfWidth p
      (halfH) = getPaddleHalfHeight p
  -- A rectangle's position is defined by the top-left corner. To find that,
  -- take the position and move it by its half-widths/heights.
  in Rectangle (toPV2 (x - halfW, y - halfH)) (V2 20 80)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  -- Hint to SDL that we prefer to scale using linear filtering. Warn if not
  -- available.
  SDL.HintRenderScaleQuality SDL.$= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <- SDL.createWindow "SDL Tutorial"
    SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window

  renderer <- SDL.createRenderer window (-1)
                SDL.RendererConfig
                  { SDL.rendererType = SDL.AcceleratedRenderer
                  , SDL.rendererTargetTexture = False
                  }

  surfaceBackground <- loadBMPSurface "resources/images/background.bmp"
  surfaceBall <- loadBMPSurface "resources/images/ball.bmp"
  surfacePaddle <- loadBMPSurface "resources/images/paddle.bmp"

  textureBackground <- SDL.createTextureFromSurface renderer surfaceBackground
  textureBall <- SDL.createTextureFromSurface renderer surfaceBall
  texturePaddle <- SDL.createTextureFromSurface renderer surfacePaddle

  mapM_ SDL.freeSurface [ surfaceBackground, surfaceBall, surfacePaddle ]

  let
    startingGameState =
      GameState
        { getBall =
            Ball
            { getBallPos = (100, 100)
            , getBallRadius = 10
            , getBallVelocity = 0.5

            -- Heading, number from 0 to 1. 0 should be the vector
            -- pointing to the right, 0.25 points down (clockwise, because
            -- y increases downwards), and 0.5 the vector pointing to the
            -- left.
            , getBallHeading = 0.2

            -- This is not a good model for a curve-ball or spinning
            -- ball. Just creates a perfect circle. Need some kind of polynomial shape.
            , getBallSpinVelocity = 0.0001
            }
        , getPaddle1 =
            Paddle
            { getPaddlePos = (20, 300)
            , getPaddleHalfWidth = 10
            , getPaddleHalfHeight = 40
            }
        , getPaddle2 =
            Paddle
            { getPaddlePos = (780, 300)
            , getPaddleHalfWidth = 10
            , getPaddleHalfHeight = 40
            }
       }
    loop oldGameState = do
      -- Get all buffered keyboard events
      events <- map SDL.eventPayload <$> SDL.pollEvents
      let quit = SDL.QuitEvent `elem` events

      -- A map of keys that are currently pressed down.
      keyMap <- SDL.getKeyboardState

      -- Possibly modify game state, or use the last one. Check if a key is
      -- pressed down, and do the state modification.
      let gameState' =
            if | keyMap SDL.ScancodeUp   -> oldGameState { getPaddle2 = paddleUp1 (getPaddle2 oldGameState) }
               | keyMap SDL.ScancodeDown -> oldGameState { getPaddle2 = paddleDown1 (getPaddle2 oldGameState) }
               | keyMap SDL.ScancodeW    -> oldGameState { getPaddle1 = paddleUp1 (getPaddle1 oldGameState) }
               | keyMap SDL.ScancodeS    -> oldGameState { getPaddle1 = paddleDown1 (getPaddle1 oldGameState) }
               | otherwise -> oldGameState

      -- Update ball position.
      let gameState'' = gameState' { getBall = updateBall (getBall gameState') }

      -- Initialize the backbuffer
      SDL.clear renderer

      -- Draw stuff into buffer
      SDL.copy renderer textureBackground Nothing Nothing
      SDL.copy renderer textureBall Nothing (Just (toRectBall (getBall gameState'')))
      SDL.copy renderer texturePaddle Nothing (Just (toRectPaddle (getPaddle1 gameState'')))
      SDL.copy renderer texturePaddle Nothing (Just (toRectPaddle (getPaddle2 gameState'')))

      -- Flip the buffer and render!
      SDL.present renderer

      unless quit (loop gameState'')

  -- Start the main loop.
  loop startingGameState

  SDL.destroyWindow window
  SDL.quit

