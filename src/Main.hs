{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Control.Monad hiding (mapM_)
import Data.Maybe
import qualified Data.Text as T
import Foreign.C.Types
import Prelude hiding (any, mapM_)
import SDL (($=))
import qualified SDL.Font as Font
import SDL.Vect
import SDL.Video.Renderer
import qualified SDL

import Debug.Trace

import PongWars.Collision

import Paths_pong_wars (getDataFileName)

maxWidth :: Float
maxWidth = 800
maxHeight :: Float
maxHeight = 600

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (800, 600)

fontColorWhite :: Font.Color
fontColorWhite = SDL.V4 255 255 255 0

-- | Load a Texture, which is a hardware-stored pixel blobs (images, fonts).
-- Comparatively, a Surface is software-rendered.
-- https://stackoverflow.com/questions/21392755/difference-between-surface-and-texture-sdl-general
-- https://stackoverflow.com/questions/21007329/what-is-a-sdl-renderer/21007477#21007477
loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture r filePath = do
  surface <- getDataFileName filePath >>= SDL.loadBMP
  let key = V4 0 0 0 0
  SDL.surfaceColorKey surface $= Just key
  t <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  return t

data Screen = Menu | Play | Quit
  deriving (Show, Eq)

data GameState = GameState
  { getScreen :: Screen
  , getBall :: Ball
  , getPaddle1 :: Paddle
  , getPaddle2 :: Paddle
  , getScore1 :: Int
  , getScore2 :: Int
  }
  deriving (Show)

-- A Paddle state consist of its position, which represents the center of the
-- paddle, and the half-width/height dimensions.
data Paddle = Paddle
  { getPaddlePos :: (Float, Float) -- ^ (x, y) coordinates of paddle
  , getPaddleHalfWidth :: Float
  , getPaddleHalfHeight :: Float
  }
  deriving Show

data Ball = Ball
  { getBallPos :: (Float, Float) -- ^ (x, y) coordinates of center of ball
  , getBallRadius :: Float      -- ^ Radius, in pixels
  , getBallVelocity :: Float      -- ^ Velocity, in pixels/tick.
  , getBallHeading :: Float     -- ^ Degree of direction, where 0 is to the right, counter-clockwise.
  , getBallSpinVelocity :: Float  -- ^ Amount of velocity pushing perpendicular of heading, in pixels/tick.
  }
  deriving Show

massWall :: Mass
massWall = 100

massPaddle :: Mass
massPaddle = 50

massBall :: Mass
massBall = 10

paddleToObject :: Paddle -> Object
paddleToObject p = AABB massPaddle (getPaddlePos p) (getPaddleHalfWidth p) (getPaddleHalfHeight p)

ballToObject :: Ball -> Object
ballToObject b = AABB massBall (getBallPos b) (getBallRadius b) (getBallRadius b)

wallTop :: Object
wallTop = AABB massWall (maxWidth / 2, -50) (maxWidth / 2 + 1) 50

wallBottom :: Object
wallBottom = AABB massWall (maxWidth / 2, maxHeight + 50) (maxWidth / 2 + 1) 50

wallLeft :: Object
wallLeft = AABB massWall (-50, maxHeight / 2) 50 (maxHeight / 2 + 1)

wallRight :: Object
wallRight = AABB massWall (maxWidth + 50, maxHeight / 2) 50 (maxHeight / 2 + 1)

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
  let a' = snd (properFraction a :: (Integer, Float))
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
      score1Incr = if isCollided rightReport then 1 else 0
      score2Incr = if isCollided leftReport then 1 else 0
      gameState' = gameState { getBall = b4
                             , getScore1 = getScore1 gameState + score1Incr
                             , getScore2 = getScore2 gameState + score2Incr
                             }
  in if score1Incr > 0 || score2Incr > 0
     then traceShowId gameState' -- Force a trace
     else gameState'

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

toCInt :: Int -> CInt
toCInt = fromIntegral

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  Font.initialize

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

  textureMenu <- loadTexture renderer "resources/images/menu.bmp"
  textureBackground <- loadTexture renderer "resources/images/background.bmp"
  textureBall <- loadTexture renderer "resources/images/ball.bmp"
  texturePaddle <- loadTexture renderer "resources/images/paddle.bmp"

  scoreFont <- do
    fp <- getDataFileName "resources/fonts/overpass/overpass-bold.otf"
    Font.load fp 40

  let
    startingGameState =
      GameState
        { getScreen = Menu
        , getBall =
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
        , getScore1 = 0
        , getScore2 = 0
       }

    loop oldGameState = do
      -- Get all buffered keyboard events
      events <- map SDL.eventPayload <$> SDL.pollEvents
      let gameState = oldGameState { getScreen = if SDL.QuitEvent `elem` events then Quit else getScreen oldGameState }

      -- A map of keys that are currently pressed down.
      keyMap <- SDL.getKeyboardState

      gameState'' <-
        case getScreen gameState of
          Quit -> return gameState
          Menu -> do
            let gameState1 =
                  if | keyMap SDL.ScancodeSpace -> gameState { getScreen = Play }
                     | keyMap SDL.ScancodeQ -> gameState { getScreen = Quit }
                     | otherwise -> gameState

            -- Initialize the backbuffer
            SDL.clear renderer

            -- Draw stuff into buffer
            SDL.copy renderer textureMenu Nothing Nothing

            -- Flip the buffer and render!
            SDL.present renderer

            return gameState1

          Play -> do
            -- Possibly modify game state, or use the last one. Check if a key is
            -- pressed down, and do the state modification.
            let gameState1 =
                  if | keyMap SDL.ScancodeUp   -> gameState { getPaddle2 = paddleUp1 (getPaddle2 gameState) }
                     | keyMap SDL.ScancodeDown -> gameState { getPaddle2 = paddleDown1 (getPaddle2 gameState) }
                     | keyMap SDL.ScancodeRight   -> gameState { getPaddle2 = paddleRight1 (getPaddle2 gameState) }
                     | keyMap SDL.ScancodeLeft -> gameState { getPaddle2 = paddleLeft1 (getPaddle2 gameState) }
                     | otherwise -> gameState

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

            let score1 = T.pack $ show $ getScore1 gameState6
            score1Text <- Font.blended scoreFont fontColorWhite score1
            (fontW, fontH) <- Font.size scoreFont score1
            textureScore1 <- SDL.createTextureFromSurface renderer score1Text
            SDL.copy renderer textureScore1 Nothing (Just (Rectangle (P (V2 0 0)) (V2 (toCInt fontW) (toCInt fontH))))

            -- Flip the buffer and render!
            SDL.present renderer

            -- Free temporary data
            SDL.destroyTexture textureScore1

            return gameState6

      unless (getScreen gameState'' == Quit) (loop gameState'')

  -- Start the main loop.
  loop startingGameState

  SDL.destroyTexture textureBackground
  SDL.destroyTexture textureBall
  SDL.destroyTexture texturePaddle
  SDL.destroyTexture textureMenu

  SDL.destroyWindow window
  Font.quit
  SDL.quit

