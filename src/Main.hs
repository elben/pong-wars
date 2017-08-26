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
import SDL (($=))
import SDL.Vect
import SDL.Video.Renderer
import qualified SDL
import qualified SDL.Image
import qualified SDL.TTF

import Debug.Trace

import PongWars.Collision

import Paths_pong_wars (getDataFileName)

maxWidth :: Float
maxWidth = 800
maxHeight :: Float
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

loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture r filePath = do
  surface <- getDataFileName filePath >>= SDL.loadBMP
  let key = V4 0 0 0 0
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return t


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

paddleToObject :: Paddle -> Object
paddleToObject p = AABB Wall (getPaddlePos p) (getPaddleHalfWidth p) (getPaddleHalfHeight p)

ballToObject :: Ball -> Object
ballToObject b = AABB Movable (getBallPos b) (getBallRadius b) (getBallRadius b)

wallTop :: Object
wallTop = AABB Wall (maxWidth / 2, -5) (maxWidth / 2 + 1) 5

wallBottom :: Object
wallBottom = AABB Wall (maxWidth / 2, maxHeight + 5) (maxWidth / 2 + 1) 5

wallLeft :: Object
wallLeft = AABB Wall (-5, maxHeight / 2) 5 (maxHeight / 2 + 1)

wallRight :: Object
wallRight = AABB Wall (maxWidth + 5, maxHeight / 2) 5 (maxHeight / 2 + 1)

posUp1 :: Point V2 CInt -> Point V2 CInt
posUp1 (P (V2 x y)) = (P (V2 x (y - 10)))

posDown1 :: Point V2 CInt -> Point V2 CInt
posDown1 (P (V2 x y)) = (P (V2 x (y + 10)))

up1 :: (Float, Float) -> (Float, Float)
up1 (x, y) = (x, y - 1)

down1 :: (Float, Float) -> (Float, Float)
down1 (x, y) = (x, y + 1)

right1 :: (Float, Float) -> (Float, Float)
right1 (x, y) = (x + 1, y)

left1 :: (Float, Float) -> (Float, Float)
left1 (x, y) = (x - 1, y)

paddleUp1 :: Paddle -> Paddle
paddleUp1 p = p { getPaddlePos = up1 (getPaddlePos p) }

paddleDown1 :: Paddle -> Paddle
paddleDown1 p = p { getPaddlePos = down1 (getPaddlePos p) }

paddleRight1 :: Paddle -> Paddle
paddleRight1 p = p { getPaddlePos = right1 (getPaddlePos p) }

paddleLeft1 :: Paddle -> Paddle
paddleLeft1 p = p { getPaddlePos = left1 (getPaddlePos p) }

-- | Normalize heading into the range of [0, 1)
normalizeHeading :: Float -> Float
normalizeHeading a =
  let a' = snd $ properFraction a
  in if a' < 0
     then a' + 1
     else a'

updateBall :: Ball -> Ball
updateBall ball =
  let (x, y) = getBallPos ball
      v = getBallVelocity ball
      a = getBallHeading ball
      x' = x + (v * cos (toRadian a))
      y' = y + (v * sin (toRadian a))

      -- Check for wall bounces
      -- a' = if x' <= 0 || x' >= maxWidth
      --        then 0.5 - a
      --        else a
      -- a'' = if y' <= 0
      --        then 1 - a'
      --        else if y' >= maxHeight
      --             then -a'
      --             else a'
  in ball { getBallPos = (x', y')
          -- , getBallHeading = normalizeHeading a''
          }

-- Flip x axis direction without affecting y axis
bounceXAxis :: Float -> Float
bounceXAxis a = normalizeHeading (0.5 - a)

-- Flip y axis direction without affecting x axis
bounceYAxis :: Float -> Float
bounceYAxis a =
  if a > 0 && a <= 0.5
  then normalizeHeading (1 - a)
  else normalizeHeading (-a)

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
      halfW = getPaddleHalfWidth p
      halfH = getPaddleHalfHeight p
  -- A rectangle's position is defined by the top-left corner. To find that,
  -- take the position and move it by its half-widths/heights.
  in Rectangle (toPV2 (x - halfW, y - halfH)) (V2 20 80)

ballWallCollision :: GameState -> GameState
ballWallCollision gameState =
  let topReport = checkCollision wallTop (ballToObject (getBall gameState))
      bottomReport = checkCollision wallBottom (ballToObject (getBall gameState))
      rightReport = checkCollision wallRight (ballToObject (getBall gameState))
      leftReport = checkCollision wallLeft (ballToObject (getBall gameState))
      b1 = updateBallStateInCollision topReport (getBall gameState)
      b2 = updateBallStateInCollision bottomReport b1
      b3 = updateBallStateInCollision rightReport b2
      b4 = updateBallStateInCollision leftReport b3
  in gameState { getBall = b4 }

paddleWallCollision :: GameState -> GameState
paddleWallCollision gameState =
  let topReport = checkCollision wallTop (paddleToObject (getPaddle1 gameState))
      bottomReport = checkCollision wallBottom (paddleToObject (getPaddle1 gameState))
      rightReport = checkCollision wallRight (paddleToObject (getPaddle1 gameState))
      leftReport = checkCollision wallLeft (paddleToObject (getPaddle1 gameState))
      p11 = updatePaddleStateInCollision topReport (getPaddle1 gameState)
      p12 = updatePaddleStateInCollision bottomReport p11
      p13 = updatePaddleStateInCollision rightReport p12
      p14 = updatePaddleStateInCollision leftReport p13
  in gameState { getPaddle1 = p14 }

updatePaddleStateInCollision :: Report -> Paddle -> Paddle
updatePaddleStateInCollision report paddle =
  case report of
    NotCollided -> paddle
    Collided a d ->
       -- Update the ball's position to get it out of collision.
       let (x, y) = getPaddlePos paddle
       in paddle { getPaddlePos = (x + (d * cos (toRadian a)), y + (d * sin (toRadian a))) }

-- | Change direction if needed, given the projection vector heading.
changeDirection :: Float -- Current heading
                -> Float -- Projection vector heading
                -> Float -- New heading
changeDirection a pvh =
  let a' = if | pvh == 0.0 -> if a <= 0.25 || a >= 0.75 then a else bounceXAxis a
              | pvh == 0.5 -> if a >= 0.25 && a <= 0.75 then a else bounceXAxis a
              | pvh == 0.25 -> if a >= 0 && a <= 0.5 then a else bounceYAxis a
              | pvh == 0.75 -> if a >= 0.5 && a < 1 then a else bounceYAxis a
              | otherwise -> a
  in a'

updateBallStateInCollision :: Report -> Ball -> Ball
updateBallStateInCollision report ball =
  case report of
    NotCollided -> ball
    Collided a d ->
       -- Update the ball's position to get it out of collision (we
       -- don't want to trigger another collision the next check).
       -- Also modify the ball's heading.
       let (x, y) = getBallPos ball
       in ball { getBallHeading = changeDirection (getBallHeading ball) a
               , getBallPos = (x + (d * cos (toRadian a)), y + (d * sin (toRadian a)))
               -- ^ Project ball out of collision.
               }

ballPaddleCollision :: GameState -> GameState
ballPaddleCollision gameState =
  let paddle1Collision = checkCollision (paddleToObject (getPaddle1 gameState)) (ballToObject (getBall gameState))
      paddle2Collision = checkCollision (paddleToObject (getPaddle2 gameState)) (ballToObject (getBall gameState))
      gameState' = gameState { getBall = updateBallStateInCollision paddle1Collision (getBall gameState) }
      gameState'' = gameState { getBall = updateBallStateInCollision paddle2Collision (getBall gameState') }
  in gameState''

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  -- Hint to SDL that we prefer to scale using linear filtering. Warn if not
  -- available.
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
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

  textureBackground <- loadTexture renderer "resources/images/background.bmp"
  textureBall <- loadTexture renderer "resources/images/ball.bmp"
  texturePaddle <- loadTexture renderer "resources/images/paddle.bmp"

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
            , getBallHeading = 0.15

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
      let gameState1 =
            if | keyMap SDL.ScancodeUp   -> oldGameState { getPaddle2 = paddleUp1 (getPaddle2 oldGameState) }
               | keyMap SDL.ScancodeDown -> oldGameState { getPaddle2 = paddleDown1 (getPaddle2 oldGameState) }
               | keyMap SDL.ScancodeRight   -> oldGameState { getPaddle2 = paddleRight1 (getPaddle2 oldGameState) }
               | keyMap SDL.ScancodeLeft -> oldGameState { getPaddle2 = paddleLeft1 (getPaddle2 oldGameState) }
               | otherwise -> oldGameState

      let gameState2 =
            if | keyMap SDL.ScancodeW    -> gameState1 { getPaddle1 = paddleUp1 (getPaddle1 gameState1) }
               | keyMap SDL.ScancodeS    -> gameState1 { getPaddle1 = paddleDown1 (getPaddle1 gameState1) }
               | keyMap SDL.ScancodeD    -> gameState1 { getPaddle1 = paddleRight1 (getPaddle1 gameState1) }
               | keyMap SDL.ScancodeA    -> gameState1 { getPaddle1 = paddleLeft1 (getPaddle1 gameState1) }
               | otherwise -> gameState1

      -- Update ball position.
      let gameState3 = gameState2 { getBall = updateBall (getBall gameState2) }

      -- Check for paddle-ball collisions.
      let gameState4 = ballWallCollision gameState3
      let gameState5 = paddleWallCollision gameState4
      let gameState6 = ballPaddleCollision gameState5

      -- Initialize the backbuffer
      SDL.clear renderer

      -- Draw stuff into buffer
      SDL.copy renderer textureBackground Nothing Nothing
      SDL.copy renderer textureBall Nothing (Just (toRectBall (getBall gameState6)))
      SDL.copy renderer texturePaddle Nothing (Just (toRectPaddle (getPaddle1 gameState6)))
      SDL.copy renderer texturePaddle Nothing (Just (toRectPaddle (getPaddle2 gameState6)))

      -- Flip the buffer and render!
      SDL.present renderer

      unless quit (loop gameState6)

  -- Start the main loop.
  loop startingGameState

  SDL.destroyWindow window
  SDL.quit

