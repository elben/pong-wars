{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad hiding (mapM_)
import Data.Maybe
import Foreign.C.Types
import Prelude hiding (any, mapM_)
import SDL (($=))
import SDL.Vect
import SDL.Video.Renderer
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as T
import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Mixer as Mix
import qualified System.Clock as Clock

import PongWars.Collision

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

data Screen = Menu | Help | Play | Winner | Pause | Quit
  deriving (Show, Eq)

type DeltaTime = Double

-- In terms of seconds
dt :: DeltaTime
dt = 0.001

data GameState = GameState
  { getScreen :: Screen
  , getBall :: Ball
  , getPaddle1 :: Paddle
  , getPaddle2 :: Paddle
  , getPlayer1 :: PlayerData
  , getPlayer2 :: PlayerData
  , getPower1 :: Power
  , getPower2 :: Power
  , getPowerActive1 :: Power
  , getPowerActive2 :: Power
  , getPowerActiveRemSecs1 :: Double -- Seconds remaining for player 1's power
  , getPowerActiveRemSecs2 :: Double -- Seconds remaining for player 2's power
  , getConsecutiveSaves1 :: Int
  , getConsecutiveSaves2 :: Int
  , getTimeRemainingSecs :: Double
  , getFps :: Integer
  , getAccumulatedTimeSecs :: Double
  }
  deriving Show

data PlayerData = PlayerData
  { getScore :: Int
  , getPower :: Power
  , getPowerActive :: Power
  , getPowerActiveRemSecs :: Double  -- Seconds remaining for player's power
  , getConsecutiveSaves :: Int
  }
  deriving Show

incrScore :: Int -> PlayerData -> PlayerData
incrScore incr pd = pd { getScore = getScore pd + incr }

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

data Power = NoPower
           | Speed    -- Increase ball speed
           | Fortress -- Increase paddle size
           | Quagmire -- Slow opponent down
           -- | Hyper -- Speed opponent down
  deriving (Show, Eq)

showPower :: Power -> T.Text
showPower NoPower = ""
showPower Speed = "Speed"
showPower Fortress = "Fortress"
showPower Quagmire = "Quagmire"

-- Number of seconds a power gets
secondsInPower :: Power -> Double
secondsInPower NoPower = 0.0
secondsInPower Speed = 10.0
secondsInPower Fortress = 10.0
secondsInPower Quagmire = 5.0

ballVelocityNormal :: Double
ballVelocityNormal = 500

ballVelocitySpeed :: Double
ballVelocitySpeed = 700

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

paddleVelocity :: Double
paddleVelocity = 600.0

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
      x' = x + (v * cos (toRadian a)) * dt
      y' = y + (v * sin (toRadian a)) * dt
  in ball { getBallPos = (x', y') }

updatePaddle :: Paddle -> Paddle
updatePaddle paddle =
  let (x, y) = getPaddlePos paddle
      v = getPaddleVelocity paddle
      a = getPaddleHeading paddle
      x' = x + (v * cos (toRadian a)) * dt
      y' = y + (v * sin (toRadian a)) * dt
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
      consecutiveSaves1 = if isCollided leftReport then 0 else getConsecutiveSaves1 gameState
      consecutiveSaves2 = if isCollided rightReport then 0 else getConsecutiveSaves2 gameState
      gameState' = gameState { getBall = b4
                             , getPlayer1 = incrScore score1Incr (getPlayer1 gameState)
                             , getPlayer2 = incrScore score2Incr (getPlayer2 gameState)
                             , getConsecutiveSaves1 = consecutiveSaves1
                             , getConsecutiveSaves2 = consecutiveSaves2
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
      gameState1 = gameState { getBall = updateBallStateInCollision paddle1Collision (getBall gameState)
                             , getConsecutiveSaves1 = if isCollided paddle1Collision then getConsecutiveSaves1 gameState + 1 else getConsecutiveSaves1 gameState }
      gameState2 = gameState1 { getBall = updateBallStateInCollision paddle2Collision (getBall gameState1)
                              , getConsecutiveSaves2 = if isCollided paddle2Collision then getConsecutiveSaves2 gameState1 + 1 else getConsecutiveSaves2 gameState1 }
  in gameState2

toCInt :: Int -> CInt
toCInt = fromIntegral

data HAlign = AlignLeft | AlignRight | AlignCenter
data VAlign = AlignTop | AlignBottom | AlignMiddle

renderTextAlign :: Renderer -> Font.Font -> Font.Color -> (Int, Int) -> HAlign -> VAlign -> T.Text -> IO ()
renderTextAlign renderer font color pos halign valign text = do
  blendedText <- Font.blended font color text
  (w, h) <- Font.size font text
  texture <- SDL.createTextureFromSurface renderer blendedText
  let x = case halign of
            AlignLeft -> fst pos
            AlignCenter -> fst pos - w `div` 2
            AlignRight -> fst pos - w
  let y = case valign of
            AlignTop -> snd pos
            AlignMiddle -> snd pos - h `div` 2
            AlignBottom -> snd pos - h
  SDL.copy renderer texture Nothing (Just (Rectangle (P (V2 (toCInt x) (toCInt y))) (V2 (toCInt w) (toCInt h))))
  SDL.destroyTexture texture

renderText :: Renderer -> Font.Font -> Font.Color -> (Int, Int) -> T.Text -> IO ()
renderText renderer font color pos =
  renderTextAlign renderer font color pos AlignLeft AlignTop

type PaddleKeyMap = [([SDL.Scancode], Paddle -> Paddle)]

keyMapPlayer1 :: PaddleKeyMap
keyMapPlayer1 = [
    ([SDL.ScancodeS], paddleMove 0.25)
  , ([SDL.ScancodeW], paddleMove 0.75)
  ]

keyMapPlayer2 :: PaddleKeyMap
keyMapPlayer2 = [
    ([SDL.ScancodeDown], paddleMove 0.25)
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

suddenDeath :: GameState -> Bool
suddenDeath gs = getTimeRemainingSecs gs <= 0 && getScore (getPlayer1 gs) == getScore (getPlayer2 gs)

determinePowers :: GameState -> GameState
determinePowers gameState =
  let gameState1 = if getConsecutiveSaves1 gameState >= 4 && getPower1 gameState == NoPower
                     then gameState { getConsecutiveSaves1 = 0, getPower1 = Speed }
                     else gameState
      gameState2 = if getConsecutiveSaves2 gameState1 >= 4 && getPower2 gameState1 == NoPower
                     then gameState1 { getConsecutiveSaves2 = 0, getPower2 = Speed }
                     else gameState1
  in gameState2

-- TODO refactor this ugly mess
activatePower :: (SDL.Scancode -> Bool) -> GameState -> GameState
activatePower keyMap gs =
  let gs1 = if getPower1 gs /= NoPower && keyMap SDL.ScancodeD
              then gs { getPower1 = NoPower
                      , getPowerActive1 = getPower1 gs
                      , getPowerActiveRemSecs1 = secondsInPower (getPower1 gs) }
              else gs
      gs2 = if getPower2 gs1 /= NoPower && keyMap SDL.ScancodeLeft
              then gs1 { getPower2 = NoPower
                      , getPowerActive2 = getPower2 gs1
                      , getPowerActiveRemSecs2 = secondsInPower (getPower2 gs1) }
              else gs1

      -- Activate Speed if needed
      gs3 = if (getPowerActive1 gs /= Speed && getPowerActive1 gs2 == Speed) ||
               (getPowerActive2 gs /= Speed && getPowerActive2 gs2 == Speed)
            then
              let ball = getBall gs2
              in gs2 { getBall = ball { getBallVelocity = ballVelocitySpeed } }
            else gs2

      -- De-activate Speed if needed (player 1)
      gs4 = if getPowerActive1 gs3 == Speed && getPowerActiveRemSecs1 gs3 <= 0
            then
              let ball = getBall gs3
              in gs3 { getBall = ball { getBallVelocity = ballVelocityNormal }
                     , getPowerActive1 = NoPower
                     }
            else gs3

      -- De-activate Speed if needed (player 2)
      gs5 = if getPowerActive2 gs4 == Speed && getPowerActiveRemSecs2 gs4 <= 0
            then
              let ball = getBall gs4
              in gs4 { getBall = ball { getBallVelocity = ballVelocityNormal }
                     , getPowerActive2 = NoPower
                     }
            else gs4
  in gs5

-- The render (below) "produces" time, and the simulation "consumes"
-- time. Keep on looping until the simulation has consumed all (with
    -- a remainder) of the accumulated time between renders.
--
-- So if the FPS is HIGHER than simulation time dt, then we only
-- simulation once for a couple of frames. But if FPS is lower than
-- simulation time dt, we simulation multiple times per frame.
--
-- https://gafferongames.com/post/fix_your_timestep/
simulationLoop :: (SDL.Scancode -> Bool) -- ^ From SDL.getKeyboardState
               -> GameState
               -> GameState
simulationLoop keyMap gameState =
  if getAccumulatedTimeSecs gameState >= dt
  then
    -- Possibly modify game state, or use the last one. Check if a key is
    -- pressed down, and do the state modification.
    let gameState1 = gameState { getPaddle1 = movePaddleState keyMapPlayer1 keyMap (getPaddle1 gameState)}
        gameState2 = gameState1 { getPaddle2 = movePaddleState keyMapPlayer2 keyMap (getPaddle2 gameState1)}

    -- Update ball position.
        gameState3 = gameState2 { getBall = updateBall (getBall gameState2) }

    -- Update paddle positions.
        gameState4 = gameState3 { getPaddle1 = updatePaddle (getPaddle1 gameState3) }
        gameState5 = gameState4 { getPaddle2 = updatePaddle (getPaddle2 gameState4) }

    -- Check for paddle-ball collisions.
        gameState6 = ballWallCollision gameState5
        gameState7 = paddleWallCollision gameState6
        gameState8 = ballPaddleCollision gameState7

        gameState9 = (determinePowers . activatePower keyMap) gameState8

        gameState10 = gameState9 { getAccumulatedTimeSecs = getAccumulatedTimeSecs gameState9 - dt }

    in simulationLoop keyMap gameState10
  else gameState

-- Make game end when game clock hits 0.
-- If tied, sudden death mode.

renderAndFlip :: Renderer -> IO () -> IO ()
renderAndFlip renderer f = do
  -- Initialize the backbuffer
  SDL.clear renderer

  -- Draw stuff into buffer
  f

  -- Flip the buffer and render!
  SDL.present renderer


main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo, SDL.InitAudio]
  Font.initialize

  -- Hint to SDL that we prefer to scale using linear filtering. Warn if not
  -- available.
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <- SDL.createWindow "Pong Wars"
    SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight
                      -- , SDL.windowMode = SDL.Fullscreen
                      }
  SDL.showWindow window

  renderer <- SDL.createRenderer window (-1)
                SDL.RendererConfig
                  { SDL.rendererType = SDL.AcceleratedRenderer
                  , SDL.rendererTargetTexture = False
                  }

  textureMenu <- loadTexture renderer "resources/images/menu.bmp"
  textureHelp <- loadTexture renderer "resources/images/help.bmp"
  texturePause <- loadTexture renderer "resources/images/pause.bmp"
  textureBackground <- loadTexture renderer "resources/images/background.bmp"
  textureBall <- loadTexture renderer "resources/images/ball_green.bmp"
  texturePaddle1 <- loadTexture renderer "resources/images/paddle_blue.bmp"
  texturePaddle2 <- loadTexture renderer "resources/images/paddle_pink.bmp"
  textureWinnerBlue <- loadTexture renderer "resources/images/winner_blue.bmp"
  textureWinnerPink <- loadTexture renderer "resources/images/winner_pink.bmp"

  mediumFont <- do
    fp <- getDataFileName "resources/fonts/NeonTubes2.otf"
    Font.load fp 40

  smallFont <- do
    fp <- getDataFileName "resources/fonts/NeonTubes2.otf"
    Font.load fp 20

  Mix.openAudio Mix.defaultAudio 512

  musicMenuF <- BS.readFile "resources/audio/purple-planet/Slipstream.ogg"
  decoded <- Mix.decode musicMenuF
  Mix.playMusic Mix.Forever decoded

  let
    startingGameState =
      GameState
        { getScreen = Menu
        , getBall =
            Ball
            { getBallPos = (100, 100)
            , getBallRadius = 10
            , getBallVelocity = ballVelocityNormal

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
        , getPlayer1 =
            PlayerData
            { getScore = 0
            , getPower = NoPower
            , getPowerActive = NoPower
            , getPowerActiveRemSecs = 0.0
            , getConsecutiveSaves = 0
            }
        , getPlayer2 =
            PlayerData
            { getScore = 0
            , getPower = NoPower
            , getPowerActive = NoPower
            , getPowerActiveRemSecs = 0.0
            , getConsecutiveSaves = 0
            }
        , getPower1 = Speed
        , getPower2 = Fortress
        , getPowerActive1 = NoPower
        , getPowerActive2 = NoPower
        , getPowerActiveRemSecs1 = 0.0
        , getPowerActiveRemSecs2 = 0.0
        , getConsecutiveSaves1 = 0
        , getConsecutiveSaves2 = 0
        , getTimeRemainingSecs = 90
        , getFps = 0
        , getAccumulatedTimeSecs = 0.0
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
                  if | keyMap SDL.ScancodeSpace -> startingGameState { getScreen = Play }
                     | keyMap SDL.ScancodeQ -> gameState { getScreen = Quit }
                     | keyMap SDL.ScancodeH -> gameState { getScreen = Help }
                     | otherwise -> gameState

            renderAndFlip renderer $
              SDL.copy renderer textureMenu Nothing Nothing

            return gameState1
          Help -> do
            let gameState1 =
                  if | keyMap SDL.ScancodeSpace -> startingGameState { getScreen = Menu }
                     | keyMap SDL.ScancodeQ -> gameState { getScreen = Quit }
                     | otherwise -> gameState

            renderAndFlip renderer $
              SDL.copy renderer textureHelp Nothing Nothing

            -- If going back to the main menu, delay for 0.5 seconds so that we
            -- don't register the [space] being down twice, so that we don't
            -- start a new game right away. Give the menu a chance to render.
            when (getScreen gameState1 == Menu) (threadDelay 500000)

            return gameState1

          Pause -> do
            let gameState1 =
                  if | keyMap SDL.ScancodeSpace -> gameState { getScreen = Play }
                     | keyMap SDL.ScancodeQ -> gameState { getScreen = Quit }
                     | otherwise -> gameState

            renderAndFlip renderer $
              SDL.copy renderer texturePause Nothing Nothing

            -- If going back to the main menu, delay for 0.5 seconds so that we
            -- don't register the [space] being down twice, so that we don't
            -- start a new game right away. Give the menu a chance to render.
            when (getScreen gameState1 == Menu) (threadDelay 500000)

            return gameState1

          Winner -> do
            let texture = if getScore (getPlayer1 gameState) > getScore (getPlayer2 gameState)
                          then textureWinnerBlue
                          else textureWinnerPink

            renderAndFlip renderer $
              SDL.copy renderer texture Nothing Nothing

            let gameState1 =
                  if | keyMap SDL.ScancodeSpace -> startingGameState { getScreen = Menu }
                     | keyMap SDL.ScancodeQ -> startingGameState { getScreen = Quit }
                     | otherwise -> gameState


            -- If going back to the main menu, delay for 0.5 seconds so that we
            -- don't register the [space] being down twice, so that we don't
            -- start a new game right away. Give the menu a chance to render.
            when (getScreen gameState1 == Menu) (threadDelay 500000)

            return gameState1

          Play ->
            if keyMap SDL.ScancodeEscape
            then return $ gameState { getScreen = Pause }
            else do
              loopStartTime <- Clock.getTime Clock.Monotonic

              let simulatedGameState = simulationLoop keyMap gameState

              -- Press "P" to spit out debugging info.
              when (keyMap SDL.ScancodeP) (print simulatedGameState)

              renderAndFlip renderer $ do
                -- Draw stuff into buffer
                SDL.copy renderer textureBackground Nothing Nothing
                SDL.copy renderer textureBall Nothing (Just (toRectBall (getBall simulatedGameState)))
                SDL.copy renderer texturePaddle1 Nothing (Just (toRectPaddle (getPaddle1 simulatedGameState)))
                SDL.copy renderer texturePaddle2 Nothing (Just (toRectPaddle (getPaddle2 simulatedGameState)))

                if suddenDeath simulatedGameState
                  then renderTextAlign renderer mediumFont fontColorWhite (400, 20) AlignCenter AlignTop "Sudden Death!"
                  else renderTextAlign renderer mediumFont fontColorWhite (400, 20) AlignCenter AlignTop (T.pack $ show (floor (getTimeRemainingSecs simulatedGameState)))

                renderText renderer mediumFont fontColorWhite (20, 20) (T.pack $ show (getScore (getPlayer1 simulatedGameState)))
                renderTextAlign renderer mediumFont fontColorWhite (780, 20) AlignRight AlignTop (T.pack $ show (getScore (getPlayer2 simulatedGameState)))

                when (getPower1 simulatedGameState /= NoPower) $
                  renderTextAlign renderer smallFont fontColorWhite (30, 570) AlignLeft AlignBottom (showPower (getPower1 simulatedGameState))

                when (getPower2 simulatedGameState /= NoPower) $
                  renderTextAlign renderer smallFont fontColorWhite (770, 570) AlignRight AlignBottom (showPower (getPower2 simulatedGameState))

              loopEndTime <- Clock.getTime Clock.Monotonic
              let diffSecs = fromIntegral (Clock.toNanoSecs $ Clock.diffTimeSpec loopStartTime loopEndTime) / 1000000000

              -- Shot clock ticks down
              let gameDurationSeconds = getTimeRemainingSecs simulatedGameState - diffSecs

              tickEndTime <- Clock.getTime Clock.Monotonic
              let tickDiffNano = Clock.toNanoSecs $ Clock.diffTimeSpec loopStartTime tickEndTime

              let finalGameState =
                    simulatedGameState { getFps = 1000000000 `div` tickDiffNano
                                       , getTimeRemainingSecs = gameDurationSeconds
                                       , getAccumulatedTimeSecs = getAccumulatedTimeSecs simulatedGameState + diffSecs
                                       , getPowerActiveRemSecs1 =
                                           if getPowerActive1 simulatedGameState == NoPower
                                           then 0.0
                                           else getPowerActiveRemSecs1 simulatedGameState - diffSecs
                                       , getPowerActiveRemSecs2 =
                                           if getPowerActive2 simulatedGameState == NoPower
                                           then 0.0
                                           else getPowerActiveRemSecs2 simulatedGameState - diffSecs
                                       }

              if gameDurationSeconds >= 0 || suddenDeath finalGameState
                then return finalGameState
                else return $ finalGameState { getScreen = Winner }

      unless (getScreen gameState'' == Quit) (loop gameState'')

  -- Start the main loop.
  loop startingGameState

  SDL.destroyTexture textureBackground
  SDL.destroyTexture textureBall
  SDL.destroyTexture texturePaddle1
  SDL.destroyTexture texturePaddle2
  SDL.destroyTexture textureMenu

  SDL.destroyWindow window
  Font.quit
  Mix.free decoded
  Mix.closeAudio
  SDL.quit

