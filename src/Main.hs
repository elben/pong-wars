{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Control.Monad hiding (mapM_)
import Data.Maybe
import qualified Data.Text as T
import Foreign.C.Types
import Prelude hiding (any, mapM_)
import qualified Data.List as L
import SDL (($=))
import qualified SDL.Font as Font
import SDL.Vect
import SDL.Video.Renderer
import qualified SDL
import qualified System.Clock as Clock
import System.Posix.Unistd (nanosleep)

import Debug.Trace

import PongWars.Collision

-- Cabal auto-generates this module. getDataFileName is so that we can access
-- data files that we're bundling with the cabal build.
--
-- https://www.haskell.org/cabal/release/cabal-1.10.1.0/doc/users-guide/
import Paths_pong_wars (getDataFileName)

maxWidth :: Double
maxWidth = 800
maxHeight :: Double
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
  , getTimeRemainingSecs :: Double
  , getFps :: Integer
  }
  deriving (Show)

-- A Paddle state consist of its position, which represents the center of the
-- paddle, and the half-width/height dimensions.
data Paddle = Paddle
  { getPaddlePos :: (Double, Double) -- ^ (x, y) coordinates of paddle
  , getPaddleHalfWidth :: Double
  , getPaddleHalfHeight :: Double
  , getPaddleVelocity :: Double
  , getPaddleHeading :: Double
  }
  deriving Show

data Ball = Ball
  { getBallPos :: (Double, Double) -- ^ (x, y) coordinates of center of ball
  , getBallRadius :: Double      -- ^ Radius, in pixels
  , getBallVelocity :: Double      -- ^ Velocity, in pixels/tick.
  , getBallHeading :: Double     -- ^ Degree of direction, where 0 is to the right, counter-clockwise.
  , getBallSpinVelocity :: Double  -- ^ Amount of velocity pushing perpendicular of heading, in pixels/tick.
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

up1 :: (Double, Double) -> (Double, Double)
up1 (x, y) = (x, y - 10)

down1 :: (Double, Double) -> (Double, Double)
down1 (x, y) = (x, y + 10)

right1 :: (Double, Double) -> (Double, Double)
right1 (x, y) = (x + 10, y)

left1 :: (Double, Double) -> (Double, Double)
left1 (x, y) = (x - 10, y)

paddleUp1 :: Paddle -> Paddle
paddleUp1 p = p { getPaddlePos = up1 (getPaddlePos p) }

paddleDown1 :: Paddle -> Paddle
paddleDown1 p = p { getPaddlePos = down1 (getPaddlePos p) }

paddleRight1 :: Paddle -> Paddle
paddleRight1 p = p { getPaddlePos = right1 (getPaddlePos p) }

paddleLeft1 :: Paddle -> Paddle
paddleLeft1 p = p { getPaddlePos = left1 (getPaddlePos p) }

paddleVelocity :: Double
paddleVelocity = 10.0

-- | Normalize heading into the range of [0, 1)
normalizeHeading :: Double -> Double
normalizeHeading a =
  let a' = snd (properFraction a :: (Integer, Double))
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
  in ball { getBallPos = (x', y') }

updatePaddle :: Paddle -> Paddle
updatePaddle paddle =
  let (x, y) = getPaddlePos paddle
      v = getPaddleVelocity paddle
      a = getPaddleHeading paddle
      x' = x + (v * cos (toRadian a))
      y' = y + (v * sin (toRadian a))
  in paddle { getPaddlePos = (x', y') }

paddleMove :: Double -> Paddle -> Paddle
paddleMove a p = p { getPaddleVelocity = paddleVelocity, getPaddleHeading = a }

paddleStop :: Paddle -> Paddle
paddleStop p = p { getPaddleVelocity = 0, getPaddleHeading = 0.0 }

-- Flip x axis direction without affecting y axis
bounceXAxis :: Double -> Double
bounceXAxis a = normalizeHeading (0.5 - a)

-- Flip y axis direction without affecting x axis
bounceYAxis :: Double -> Double
bounceYAxis a =
  if a > 0 && a <= 0.5
  then normalizeHeading (1 - a)
  else normalizeHeading (-a)

toRadian :: Double -> Double
toRadian a = 2 * pi * a

toPV2 :: (Double, Double) -> Point V2 CInt
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
  in gameState'

paddleWallCollision :: GameState -> GameState
paddleWallCollision gameState =
  let topReport1 = checkCollision wallTop (paddleToObject (getPaddle1 gameState))
      bottomReport1 = checkCollision wallBottom (paddleToObject (getPaddle1 gameState))
      rightReport1 = checkCollision wallRight (paddleToObject (getPaddle1 gameState))
      leftReport1 = checkCollision wallLeft (paddleToObject (getPaddle1 gameState))
      p11 = updatePaddleStateInCollision topReport1 (getPaddle1 gameState)
      p12 = updatePaddleStateInCollision bottomReport1 p11
      p13 = updatePaddleStateInCollision rightReport1 p12
      p14 = updatePaddleStateInCollision leftReport1 p13

      topReport2 = checkCollision wallTop (paddleToObject (getPaddle2 gameState))
      bottomReport2 = checkCollision wallBottom (paddleToObject (getPaddle2 gameState))
      rightReport2 = checkCollision wallRight (paddleToObject (getPaddle2 gameState))
      leftReport2 = checkCollision wallLeft (paddleToObject (getPaddle2 gameState))
      p21 = updatePaddleStateInCollision topReport2 (getPaddle2 gameState)
      p22 = updatePaddleStateInCollision bottomReport2 p21
      p23 = updatePaddleStateInCollision rightReport2 p22
      p24 = updatePaddleStateInCollision leftReport2 p23
  in gameState { getPaddle1 = p14, getPaddle2 = p24 }

updatePaddleStateInCollision :: Report -> Paddle -> Paddle
updatePaddleStateInCollision report paddle =
  case report of
    NotCollided -> paddle
    Collided a d ->
       -- Update the ball's position to get it out of collision.
       let (x, y) = getPaddlePos paddle
       in paddle { getPaddlePos = (x + (d * cos (toRadian a)), y + (d * sin (toRadian a))) }

-- | Change direction if needed, given the projection vector heading.
changeDirection :: Double -- Current heading
                -> Double -- Projection vector heading
                -> Double -- New heading
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

renderText :: Renderer -> Font.Font -> Font.Color -> (Int, Int) -> T.Text -> IO ()
renderText renderer font color pos text = do
  blendedText <- Font.blended font color text
  (w, h) <- Font.size font text
  texture <- SDL.createTextureFromSurface renderer blendedText
  SDL.copy renderer texture Nothing (Just (Rectangle (P (V2 (toCInt $ fst pos) (toCInt $ snd pos))) (V2 (toCInt w) (toCInt h))))
  SDL.destroyTexture texture

type PaddleKeyMap = [([SDL.Scancode], Paddle -> Paddle)]

keyMapPlayer1 :: PaddleKeyMap
keyMapPlayer1 = [
    -- ([SDL.ScancodeD, SDL.ScancodeS], paddleMove 0.125)
  -- , ([SDL.ScancodeS, SDL.ScancodeA], paddleMove 0.375)
  -- , ([SDL.ScancodeA, SDL.ScancodeW], paddleMove 0.625)
  -- , ([SDL.ScancodeW, SDL.ScancodeD], paddleMove 0.875)
  -- , ([SDL.ScancodeD], paddleMove 0.0)
    ([SDL.ScancodeS], paddleMove 0.25)
  -- , ([SDL.ScancodeA], paddleMove 0.50)
  , ([SDL.ScancodeW], paddleMove 0.75)
  ]

keyMapPlayer2 :: PaddleKeyMap
keyMapPlayer2 = [
    -- ([SDL.ScancodeRight, SDL.ScancodeDown], paddleMove 0.125)
  -- , ([SDL.ScancodeDown, SDL.ScancodeLeft], paddleMove 0.375)
  -- , ([SDL.ScancodeLeft, SDL.ScancodeUp], paddleMove 0.625)
  -- , ([SDL.ScancodeUp, SDL.ScancodeRight], paddleMove 0.875)
  -- , ([SDL.ScancodeRight], paddleMove 0.0)
    ([SDL.ScancodeDown], paddleMove 0.25)
  -- , ([SDL.ScancodeLeft], paddleMove 0.50)
  , ([SDL.ScancodeUp], paddleMove 0.75)
  ]

-- Possibly modify game state, or use the last one. Check if a key is
-- pressed down, and do the state modification.
movePaddleState ::
  PaddleKeyMap ->
  (SDL.Scancode -> Bool) -> -- ^ From SDL.getKeyboardState
  Paddle -> -- ^ Current paddle state
  Paddle    -- ^ Returns new paddle state
movePaddleState paddleKeyMap keyMap paddle =
  -- Find the first element where all the required keys are hit; use that as the
  -- movement for this paddle.
  case L.find (\(keys, _) -> L.all keyMap keys) paddleKeyMap of
    Just (_, movement) -> movement paddle
    Nothing -> paddleStop paddle

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
            , getBallVelocity = 7

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
            , getPaddleVelocity = 0
            , getPaddleHeading = 0
            }
        , getPaddle2 =
            Paddle
            { getPaddlePos = (780, 300)
            , getPaddleHalfWidth = 10
            , getPaddleHalfHeight = 40
            , getPaddleVelocity = 0
            , getPaddleHeading = 0
            }
        , getScore1 = 0
        , getScore2 = 0
        , getTimeRemainingSecs = 60
        , getFps = 0
       }

    gameStartTime = Clock.getTime Clock.Monotonic

    loop oldGameState = do
      loopStartTime <- Clock.getTime Clock.Monotonic

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
            let gameState1 = gameState { getPaddle1 = movePaddleState keyMapPlayer1 keyMap (getPaddle1 gameState)}
            let gameState2 = gameState1 { getPaddle2 = movePaddleState keyMapPlayer2 keyMap (getPaddle2 gameState1)}

            -- Update ball position.
            let gameState3 = gameState2 { getBall = updateBall (getBall gameState2) }

            -- Update paddle positions.
            let gameState4 = gameState3 { getPaddle1 = updatePaddle (getPaddle1 gameState3) }
            let gameState5 = gameState4 { getPaddle2 = updatePaddle (getPaddle2 gameState4) }

            -- Check for paddle-ball collisions.
            let gameState6 = ballWallCollision gameState5
            let gameState7 = paddleWallCollision gameState6
            let gameState8 = ballPaddleCollision gameState7

            -- Initialize the backbuffer
            SDL.clear renderer

            -- Draw stuff into buffer
            SDL.copy renderer textureBackground Nothing Nothing
            SDL.copy renderer textureBall Nothing (Just (toRectBall (getBall gameState8)))
            SDL.copy renderer texturePaddle Nothing (Just (toRectPaddle (getPaddle1 gameState8)))
            SDL.copy renderer texturePaddle Nothing (Just (toRectPaddle (getPaddle2 gameState8)))

            renderText renderer scoreFont fontColorWhite (200, 0) (T.pack $ show (floor (getTimeRemainingSecs gameState8)) ++ " seconds")
            renderText renderer scoreFont fontColorWhite (400, 0) (T.pack $ show (getFps gameState8) ++ " fps")
            renderText renderer scoreFont fontColorWhite (0, 0) (T.pack $ show (getScore1 gameState8))
            renderText renderer scoreFont fontColorWhite (750, 0) (T.pack $ show (getScore2 gameState8))

            -- Flip the buffer and render!
            SDL.present renderer

            -- Attempt to tick at 60 FPS
            loopEndTime <- Clock.getTime Clock.Monotonic
            let diffNano = Clock.toNanoSecs $ Clock.diffTimeSpec loopStartTime loopEndTime

            -- Sleep when above 60 FPS
            when (diffNano < 16666666) (nanosleep (16666666 - diffNano))

            -- Shot clock ticks down
            let gameDurationSeconds = getTimeRemainingSecs gameState8 - (fromIntegral diffNano / 100000000)

            -- Time *after* a potential sleep.
            tickEndTime <- Clock.getTime Clock.Monotonic
            let tickDiffNano = Clock.toNanoSecs $ Clock.diffTimeSpec loopStartTime tickEndTime

            return $ gameState8 { getFps = 1000000000 `div` tickDiffNano, getTimeRemainingSecs = gameDurationSeconds }

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

