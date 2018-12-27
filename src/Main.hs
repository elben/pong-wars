{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main
  ( main
  )
where

import           Control.Concurrent             ( threadDelay )
import           Control.Monad           hiding ( mapM_ )
import           Data.Maybe
import           Foreign.C.Types
import           Prelude                 hiding ( any
                                                , mapM_
                                                )
import           SDL                            ( ($=) )
import           SDL.Vect
import           SDL.Video.Renderer
import qualified Data.Text                     as T
import qualified SDL
import qualified SDL.Font                      as Font
import qualified SDL.Mixer                     as Mix
import qualified System.Clock                  as Clock
import qualified System.Random                 as Rand

import           PongWars.Collision

import           Paths_pong_wars                ( getDataFileName )

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
  , getPlayer1 :: PlayerState
  , getPlayer2 :: PlayerState
  , getTimeRemainingSecs :: Double
  , getFps :: Int
  , getAccumulatedTimeSecs :: Double
  }
  deriving Show

data PlayerState = PlayerState
  { getScore :: Int
  , getPaddle :: Paddle
  , getPower :: Power
  , getPowerActive :: Power
  , getPowerActiveRemSecs :: Double  -- Seconds remaining for player's power
  , getNextRandomPower :: Power
  , getConsecutiveSaves :: Int

  -- Keys being pressed
  , getUpKeyPressed :: Bool
  , getDownKeyPressed :: Bool
  , getPowerKeyPressed :: Bool
  }
  deriving Show

data Player = P1 | P2

opponent :: Player -> Player
opponent p = case p of
  P1 -> P2
  P2 -> P1

getPlayer :: Player -> GameState -> PlayerState
getPlayer p gs = case p of
  P1 -> getPlayer1 gs
  P2 -> getPlayer2 gs

setPlayer :: Player -> PlayerState -> GameState -> GameState
setPlayer p ps gs = case p of
  P1 -> gs { getPlayer1 = ps }
  P2 -> gs { getPlayer2 = ps }

-- Modify the given Player's PlayerState, given a mapping function and GameState.
mapPlayer :: Player -> (PlayerState -> PlayerState) -> GameState -> GameState
mapPlayer p f gs = setPlayer p (f (getPlayer p gs)) gs

-- Modify each player's PlayerState, given a mapping function and GameState.
foldPlayers :: (PlayerState -> PlayerState) -> GameState -> GameState
foldPlayers f gs = (mapPlayer P1 f . mapPlayer P2 f) gs

-- Run the given function, which modifies the GameState, for each player.
foldPlayersOver :: (Player -> GameState -> GameState) -> GameState -> GameState
foldPlayersOver f gs = (f P1 . f P2) gs

-- Returns true if the predicate is true for all players.
allPlayers :: (PlayerState -> Bool) -> GameState -> Bool
allPlayers f gs = f (getPlayer P1 gs) && f (getPlayer P2 gs)

-- Do the given action for each player.
forEachPlayer :: (Player -> IO ()) -> IO ()
forEachPlayer f = do
  f P1
  f P2

setPaddle :: Player -> Paddle -> GameState -> GameState
setPaddle p paddle gs = case p of
  P1 -> gs { getPlayer1 = (getPlayer1 gs) { getPaddle = paddle } }
  P2 -> gs { getPlayer2 = (getPlayer2 gs) { getPaddle = paddle } }

incrScore :: Int -> PlayerState -> PlayerState
incrScore incr ps = ps { getScore = getScore ps + incr }

setConsecutiveSaves :: Int -> PlayerState -> PlayerState
setConsecutiveSaves n ps = ps { getConsecutiveSaves = n }

-- A Paddle state consist of its position, which represents the center of the
-- paddle, and the half-width/height dimensions.
data Paddle = Paddle
  { getPaddlePos :: (Double, Double) -- ^ (x, y) coordinates of paddle
  , getPaddleHalfWidth :: Double
  , getPaddleHalfHeight :: Double
  , getPaddleVelocity :: Double
  , getPaddleMaxVelocity :: Double
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
showPower NoPower  = ""
showPower Speed    = "Speed"
showPower Fortress = "Fortress"
showPower Quagmire = "Quagmire"

-- Number of seconds a power gets
secondsInPower :: Power -> Double
secondsInPower NoPower  = 0.0
secondsInPower Speed    = 10.0
secondsInPower Fortress = 10.0
secondsInPower Quagmire = 5.0

ballVelocityNormal :: Double
ballVelocityNormal = 550

ballVelocitySpeed :: Double
ballVelocitySpeed = 750

paddleVelocityNormal :: Double
paddleVelocityNormal = 600.0

paddleVelocitySlow :: Double
paddleVelocitySlow = 300.0

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
wallLeft = AABB massWall (19, maxHeight / 2) 10 (maxHeight / 2 + 1)

wallRight :: Object
wallRight = AABB massWall (maxWidth - 19, maxHeight / 2) 10 (maxHeight / 2 + 1)

-- | Normalize heading into the range of [0, 1)
normalizeHeading :: Double -> Double
normalizeHeading a = let a' = snd (properFraction a :: (Integer, Double)) in if a' < 0 then a' + 1 else a'

updateBall :: Ball -> Ball
updateBall ball =
  let (x, y) = getBallPos ball
      v      = getBallVelocity ball
      a      = getBallHeading ball
      x'     = x + (v * cos (toRadian a)) * dt
      y'     = y + (v * sin (toRadian a)) * dt
  in  ball { getBallPos = (x', y') }

updatePaddle :: Paddle -> Paddle
updatePaddle paddle =
  let (x, y) = getPaddlePos paddle
      v      = getPaddleVelocity paddle
      a      = getPaddleHeading paddle
      x'     = x + (v * cos (toRadian a)) * dt
      y'     = y + (v * sin (toRadian a)) * dt
  in  paddle { getPaddlePos = (x', y') }

paddleMove :: Double -> Paddle -> Paddle
paddleMove a p = p { getPaddleVelocity = getPaddleMaxVelocity p, getPaddleHeading = a }

paddleStop :: Paddle -> Paddle
paddleStop p = p { getPaddleVelocity = 0, getPaddleHeading = 0.0 }

-- Flip x axis direction without affecting y axis
bounceXAxis :: Double -> Double
bounceXAxis a = normalizeHeading (0.5 - a)

-- Flip y axis direction without affecting x axis
bounceYAxis :: Double -> Double
bounceYAxis a = if a > 0 && a <= 0.5 then normalizeHeading (1 - a) else normalizeHeading (-a)

toRadian :: Double -> Double
toRadian a = 2 * pi * a

toPV2 :: (Double, Double) -> Point V2 CInt
toPV2 (x, y) = P $ V2 (round x) (round y)

toRect :: Object -> Rectangle CInt
toRect (AABB _ (x, y) hw hh) =
  Rectangle (toPV2 (x - hw, y - hh)) (V2 (ceiling (hw * 2)) (ceiling (hh * 2)))

toRectBall :: Ball -> Rectangle CInt
toRectBall b =
  let (x, y) = getBallPos b
      r      = getBallRadius b
  in  Rectangle (toPV2 (x - r, y - r)) (V2 20 20)

toRectPaddle :: Paddle -> Rectangle CInt
toRectPaddle p =
  let (x, y) = getPaddlePos p
      halfW  = getPaddleHalfWidth p
      halfH  = getPaddleHalfHeight p
  -- A rectangle's position is defined by the top-left corner. To find that,
  -- take the position and move it by its half-widths/heights.
  in  Rectangle (toPV2 (x - halfW, y - halfH)) (V2 20 80)

ballWallCollision :: GameState -> GameState
ballWallCollision gameState =
  let topReport    = checkCollision wallTop (ballToObject (getBall gameState))
      bottomReport = checkCollision wallBottom (ballToObject (getBall gameState))
      rightReport  = checkCollision wallRight (ballToObject (getBall gameState))
      leftReport   = checkCollision wallLeft (ballToObject (getBall gameState))
      b1           = updateBallCollision topReport (getBall gameState)
      b2           = updateBallCollision bottomReport b1
      b3           = updateBallCollision rightReport b2
      b4           = updateBallCollision leftReport b3
      score1Incr   = if isCollided rightReport then 1 else 0
      score2Incr   = if isCollided leftReport then 1 else 0
      saves1       = if isCollided leftReport then 0 else getConsecutiveSaves (getPlayer1 gameState)
      saves2       = if isCollided rightReport then 0 else getConsecutiveSaves (getPlayer2 gameState)
  in gameState { getBall    = b4
               , getPlayer1 = (incrScore score1Incr . setConsecutiveSaves saves1) (getPlayer1 gameState)
               , getPlayer2 = (incrScore score2Incr . setConsecutiveSaves saves2) (getPlayer2 gameState)
               }

paddleWallCollision :: GameState -> GameState
paddleWallCollision gameState =
  let topReport1    = checkCollision wallTop (paddleToObject (getPaddle (getPlayer P1 gameState)))
      bottomReport1 = checkCollision wallBottom (paddleToObject (getPaddle (getPlayer P1 gameState)))
      p11           = updatePaddleCollision topReport1 (getPaddle (getPlayer P1 gameState))
      p12           = updatePaddleCollision bottomReport1 p11

      topReport2    = checkCollision wallTop (paddleToObject (getPaddle (getPlayer P2 gameState)))
      bottomReport2 = checkCollision wallBottom (paddleToObject (getPaddle (getPlayer P2 gameState)))
      p21           = updatePaddleCollision topReport2 (getPaddle (getPlayer P2 gameState))
      p22           = updatePaddleCollision bottomReport2 p21
  in  (setPaddle P1 p12 . setPaddle P2 p22) gameState

updatePaddleCollision :: Report -> Paddle -> Paddle
updatePaddleCollision report paddle = case report of
  NotCollided -> paddle
  Collided a d ->
     -- Update the paddle's position to get it out of collision.
    let (x, y) = getPaddlePos paddle
    in  paddle { getPaddlePos = (x + (d * cos (toRadian a)), y + (d * sin (toRadian a))) }

-- | Change direction if needed, given the projection vector heading.
changeDirection
  :: Double -- Current heading
  -> Double -- Projection vector heading
  -> Double -- New heading
changeDirection a pvh =
  let a' = if
        | pvh == 0.0  -> if a <= 0.25 || a >= 0.75 then a else bounceXAxis a
        | pvh == 0.5  -> if a >= 0.25 && a <= 0.75 then a else bounceXAxis a
        | pvh == 0.25 -> if a >= 0 && a <= 0.5 then a else bounceYAxis a
        | pvh == 0.75 -> if a >= 0.5 && a < 1 then a else bounceYAxis a
        | otherwise   -> a
  in  a'

updateBallCollision :: Report -> Ball -> Ball
updateBallCollision report ball = case report of
  NotCollided -> ball
  Collided a d ->
     -- Update the ball's position to get it out of collision (we
     -- don't want to trigger another collision the next check).
     -- Also modify the ball's heading.
    let (x, y) = getBallPos ball
    in  ball { getBallHeading = changeDirection (getBallHeading ball) a
             , getBallPos     = (x + (d * cos (toRadian a)), y + (d * sin (toRadian a)))
                                -- ^ Project ball out of collision.
             }

ballPaddleCollision :: GameState -> GameState
ballPaddleCollision gameState =
  let paddle1Collision =
        checkCollision (paddleToObject (getPaddle (getPlayer P1 gameState))) (ballToObject (getBall gameState))
      paddle2Collision =
        checkCollision (paddleToObject (getPaddle (getPlayer P2 gameState))) (ballToObject (getBall gameState))
      saves1 = if isCollided paddle1Collision
        then getConsecutiveSaves (getPlayer1 gameState) + 1
        else getConsecutiveSaves (getPlayer1 gameState)
      saves2 = if isCollided paddle2Collision
        then getConsecutiveSaves (getPlayer2 gameState) + 1
        else getConsecutiveSaves (getPlayer2 gameState)
      gameState1 = gameState { getBall    = updateBallCollision paddle1Collision (getBall gameState)
                             , getPlayer1 = setConsecutiveSaves saves1 (getPlayer1 gameState)
                             }
      gameState2 = gameState1 { getBall    = updateBallCollision paddle2Collision (getBall gameState1)
                              , getPlayer2 = setConsecutiveSaves saves2 (getPlayer2 gameState1)
                              }
  in  gameState2

toCInt :: Int -> CInt
toCInt = fromIntegral

data HAlign = AlignLeft | AlignRight | AlignCenter
data VAlign = AlignTop | AlignBottom | AlignMiddle

renderTextAlign :: Renderer -> Font.Font -> Font.Color -> (Int, Int) -> HAlign -> VAlign -> T.Text -> IO ()
renderTextAlign renderer font color pos halign valign text = do
  blendedText <- Font.blended font color text
  (w, h)      <- Font.size font text
  texture     <- SDL.createTextureFromSurface renderer blendedText
  let x = case halign of
        AlignLeft   -> fst pos
        AlignCenter -> fst pos - w `div` 2
        AlignRight  -> fst pos - w
  let y = case valign of
        AlignTop    -> snd pos
        AlignMiddle -> snd pos - h `div` 2
        AlignBottom -> snd pos - h
  SDL.copy renderer texture Nothing (Just (Rectangle (P (V2 (toCInt x) (toCInt y))) (V2 (toCInt w) (toCInt h))))
  SDL.destroyTexture texture

-- Possibly modify game state, or use the last one. Check if a key is
-- pressed down, and do the state modification.
movePaddle
  :: PlayerState -- ^ The player whose paddle is being moved
  -> Paddle      -- ^ Current paddle state
  -> Paddle      -- ^ Returns new paddle state
movePaddle ps paddle = if
  | getUpKeyPressed ps   -> paddleMove 0.75 paddle
  | getDownKeyPressed ps -> paddleMove 0.25 paddle
  | otherwise            -> paddleStop paddle

suddenDeath :: GameState -> Bool
suddenDeath gs = getTimeRemainingSecs gs <= 0 && getScore (getPlayer P1 gs) == getScore (getPlayer P2 gs)

-- Determine if the players have earned a new power. If so, choose a random
-- power. Don't set a new power if there's a currently active power.
determinePowers :: GameState -> GameState
determinePowers gs =
  let speedCheck p = if getConsecutiveSaves p >= 4 && getPower p == NoPower
        then p { getConsecutiveSaves = 0, getPower = (getNextRandomPower p) }
        else p
  in  foldPlayers speedCheck gs

decrPowerActiveRemSecs :: Double -> PlayerState -> PlayerState
decrPowerActiveRemSecs diffSecs ps =
  ps { getPowerActiveRemSecs = if getPowerActive ps == NoPower then 0.0 else getPowerActiveRemSecs ps - diffSecs }

-- Activate the Speed power for the given player. Speed increases the ball's
-- speed.
applySpeed :: GameState -> GameState
applySpeed gs =
  let ballVel = if allPlayers (\p -> getPowerActive p /= Speed) gs then ballVelocityNormal else ballVelocitySpeed
  in  gs { getBall = (getBall gs) { getBallVelocity = ballVel } }

-- Activate the Quagmire power for the given player. Quagmire decreases the
-- opponent's paddle speed.
applyQuagmire :: Player -> GameState -> GameState
applyQuagmire p gs = if getPowerActive (getPlayer p gs) == Quagmire && getPowerActiveRemSecs (getPlayer p gs) > 0
  then
    let ps  = getPlayer (opponent p) gs
        ps' = ps { getPaddle = (getPaddle ps) { getPaddleMaxVelocity = paddleVelocitySlow } }
    in  setPlayer (opponent p) ps' gs
  else
    let ps  = getPlayer (opponent p) gs
        ps' = ps { getPaddle = (getPaddle ps) { getPaddleMaxVelocity = paddleVelocityNormal } }
        gs' = deactivatePower p gs
    in  setPlayer (opponent p) ps' gs'

activatePower :: PlayerState -> PlayerState
activatePower ps = if getPowerKeyPressed ps && getPower ps /= NoPower
  then ps { getPower = NoPower, getPowerActive = getPower ps, getPowerActiveRemSecs = secondsInPower (getPower ps) }
  else ps

-- Set the Player's active power to NoPower.
deactivatePower :: Player -> GameState -> GameState
deactivatePower p gs =
  let ps = getPlayer p gs
      ps' =
        if getPowerActive ps /= NoPower && getPowerActiveRemSecs ps <= 0 then ps { getPowerActive = NoPower } else ps
  in  setPlayer p ps' gs

-- | Apply the available powers.
--
-- Start a power if the user activates it. Modifies the game state for any
-- active power that's running. Turn off the power if the time is up. Deactivate
-- the user's power.
applyPowers :: GameState -> GameState
applyPowers gs =
  (foldPlayers activatePower . applySpeed . foldPlayersOver applyQuagmire . foldPlayersOver deactivatePower) gs


-- The render (below) "produces" time, and the simulation "consumes" time. Keep
-- on looping until the simulation has consumed all (with a remainder) of the
-- accumulated time between renders.
--
-- So if the FPS is HIGHER than simulation time dt, then we only simulation once
-- for a couple of frames. But if FPS is lower than simulation time dt, we
-- simulation multiple times per frame.
--
-- Because the simulation loop can run 0 or more times, the main game loop
-- should not depend on anything in GameState that may last only one simulation.
-- If the main game loop requires comparing some state before and after a call
-- to this function, it should store its own state.
--
-- https://gafferongames.com/post/fix_your_timestep/
--
simulationLoop :: GameState -> GameState
simulationLoop gameState = if getAccumulatedTimeSecs gameState >= dt
  then
-- Possibly modify game state, or use the last one. Check if a key is
-- pressed down, and do the state modification.
    let gameState1  = setPaddle P1 (movePaddle (getPlayer1 gameState) (getPaddle (getPlayer P1 gameState))) gameState
        gameState2  = setPaddle P2 (movePaddle (getPlayer2 gameState) (getPaddle (getPlayer P2 gameState1))) gameState1

    -- Update ball position.
        gameState3  = gameState2 { getBall = updateBall (getBall gameState2) }

    -- Update paddle positions.
        gameState4  = setPaddle P1 (updatePaddle (getPaddle (getPlayer P1 gameState3))) gameState3
        gameState5  = setPaddle P2 (updatePaddle (getPaddle (getPlayer P2 gameState4))) gameState4

    -- Check for paddle-ball collisions.
        gameState6  = ballWallCollision gameState5
        gameState7  = paddleWallCollision gameState6
        gameState8  = ballPaddleCollision gameState7

        gameState9  = (determinePowers . applyPowers) gameState8

        gameState10 = gameState9 { getAccumulatedTimeSecs = getAccumulatedTimeSecs gameState9 - dt }
    in  simulationLoop gameState10
  else gameState

registerKeyPresses :: (SDL.Scancode -> Bool) -> GameState -> GameState
registerKeyPresses keyMap gs =
  let p1 = (getPlayer P1 gs) { getUpKeyPressed    = keyMap SDL.ScancodeW
                             , getDownKeyPressed  = keyMap SDL.ScancodeS
                             , getPowerKeyPressed = keyMap SDL.ScancodeD
                             }
      p2 = (getPlayer P2 gs) { getUpKeyPressed    = keyMap SDL.ScancodeUp
                             , getDownKeyPressed  = keyMap SDL.ScancodeDown
                             , getPowerKeyPressed = keyMap SDL.ScancodeLeft
                             }
  in  gs { getPlayer1 = p1, getPlayer2 = p2 }

-- Apply various state changes that we need a loop time difference (in seconds).
applyTimeDiffSecs :: Double -> GameState -> GameState
applyTimeDiffSecs diffSecs gs =
  let
      gameDurationSeconds = getTimeRemainingSecs gs - diffSecs
      -- Decrease each player's active power remaining time.
      gs1 = foldPlayers (decrPowerActiveRemSecs diffSecs) gs
      gs2 = gs1
        { getFps                 = floor (1.0 / diffSecs)
          -- Shot clock ticks down
        , getTimeRemainingSecs   = getTimeRemainingSecs gs1 - diffSecs
        , getAccumulatedTimeSecs = getAccumulatedTimeSecs gs1 + diffSecs
        , getScreen = if gameDurationSeconds >= 0 || suddenDeath gs1 then (getScreen gs1) else Winner
        }
  in gs2

-- Converts an integer to a specific Power. Assumes the int > 0, and < the
-- number of powers available.
numToPower :: Int -> Power
numToPower i =
  case i of
    1 -> Speed
    2 -> Quagmire
    _ -> Speed

-- Set the getNextRandomPower attributes in GameState.
registerNextRandomPowers :: GameState -> IO GameState
registerNextRandomPowers gs = do
  power1 <- Rand.getStdRandom (Rand.randomR (1,2)) >>= \i -> return $ numToPower i
  power2 <- Rand.getStdRandom (Rand.randomR (1,2)) >>= \i -> return $ numToPower i

  let gs1 = mapPlayer P1 (\ps -> ps { getNextRandomPower = power1 } ) gs
  let gs2 = mapPlayer P2 (\ps -> ps { getNextRandomPower = power2 } ) gs1

  return gs2

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
  do
    renderQuality <- SDL.get SDL.HintRenderScaleQuality
    when (renderQuality /= SDL.ScaleLinear) $ putStrLn "Warning: Linear texture filtering not enabled!"

  window <- SDL.createWindow "Pong Wars" SDL.defaultWindow {
      SDL.windowInitialSize = V2 screenWidth screenHeight
    , SDL.windowMode = SDL.Fullscreen
    }
  SDL.showWindow window

  renderer <- SDL.createRenderer
    window
    (-1)
    SDL.RendererConfig {SDL.rendererType = SDL.AcceleratedRenderer, SDL.rendererTargetTexture = False}

  textureMenu       <- loadTexture renderer "resources/images/menu.bmp"
  textureHelp       <- loadTexture renderer "resources/images/help.bmp"
  texturePause      <- loadTexture renderer "resources/images/pause.bmp"
  textureBackground <- loadTexture renderer "resources/images/background.bmp"
  textureBall       <- loadTexture renderer "resources/images/ball_green.bmp"
  texturePaddle1    <- loadTexture renderer "resources/images/paddle_blue.bmp"
  texturePaddle2    <- loadTexture renderer "resources/images/paddle_pink.bmp"
  textureWinnerBlue <- loadTexture renderer "resources/images/winner_blue.bmp"
  textureWinnerPink <- loadTexture renderer "resources/images/winner_pink.bmp"
  texturePixel      <- loadTexture renderer "resources/images/pixel.bmp"

  mediumFont        <- do
    fp <- getDataFileName "resources/fonts/NeonTubes2.otf"
    Font.load fp 40

  smallFont <- do
    fp <- getDataFileName "resources/fonts/NeonTubes2.otf"
    Font.load fp 20

  Mix.openAudio Mix.defaultAudio 512

  -- Load sound effects.
  speedSfx <- do
    fp <- getDataFileName "resources/audio/sfx/351409__newagesoup__fat-pulse-short.wav"
    Mix.load fp
  quagmireSfx <- do
    fp <- getDataFileName "resources/audio/sfx/368512__josepharaoh99__engine-dying.mp3"
    Mix.load fp


  -- Load mainMusic as a Chunk, because trying to play a Music was causing
  -- problems where the music would abruptly end.
  mainMusic <- do
    fp <- getDataFileName "resources/audio/purple-planet/Slipstream.ogg"
    Mix.load fp

  -- Play on Channel 0.
  _ <- Mix.playOn 0 Mix.Forever mainMusic

  let
    startingGameState = GameState
      { getScreen              = Menu
      , getBall                = Ball
        { getBallPos          = (100, 100)
        , getBallRadius       = 10
        , getBallVelocity     = ballVelocityNormal

            -- Heading, number from 0 to 1. 0 should be the vector
            -- pointing to the right, 0.25 points down (clockwise, because
            -- y increases downwards), and 0.5 the vector pointing to the
            -- left.
        , getBallHeading      = 0.15

            -- This is not a good model for a curve-ball or spinning
            -- ball. Just creates a perfect circle. Need some kind of polynomial shape.
        , getBallSpinVelocity = 0.0001
        }
      , getPlayer1             = PlayerState
        { getScore              = 0
        , getPaddle             = Paddle
          { getPaddlePos         = (20, 300)
          , getPaddleHalfWidth   = 10
          , getPaddleHalfHeight  = 40
          , getPaddleVelocity    = 0
          , getPaddleMaxVelocity = paddleVelocityNormal
          , getPaddleHeading     = 0
          }
        , getPower              = Quagmire
        , getPowerActive        = NoPower
        , getPowerActiveRemSecs = 0.0
        , getNextRandomPower    = Speed
        , getConsecutiveSaves   = 0
        , getUpKeyPressed       = False
        , getDownKeyPressed     = False
        , getPowerKeyPressed    = False
        }
      , getPlayer2             = PlayerState
        { getScore              = 0
        , getPaddle             = Paddle
          { getPaddlePos         = (780, 300)
          , getPaddleHalfWidth   = 10
          , getPaddleHalfHeight  = 40
          , getPaddleVelocity    = 0
          , getPaddleMaxVelocity = paddleVelocityNormal
          , getPaddleHeading     = 0
          }
        , getPower              = Speed
        , getPowerActive        = NoPower
        , getPowerActiveRemSecs = 0.0
        , getNextRandomPower    = Speed
        , getConsecutiveSaves   = 0
        , getUpKeyPressed       = False
        , getDownKeyPressed     = False
        , getPowerKeyPressed    = False
        }
      , getTimeRemainingSecs   = 90
      , getFps                 = 0
      , getAccumulatedTimeSecs = 0.0
      }

    ----------------------
    -- Helper functions --
    ----------------------

    -- Play power sound effects on channel 2, so that it doesn't collide with
    -- music.
    playPowerSfx :: Power -> IO ()
    playPowerSfx power = do
      let sfx = case power of
                  Speed -> speedSfx
                  Quagmire -> quagmireSfx
                  _ -> speedSfx

      playingChannel2 <- Mix.playing 2
      when playingChannel2 (Mix.halt 2)
      _ <- Mix.playOn 2 Mix.Once sfx
      return ()

    renderBackground :: IO ()
    renderBackground =
      SDL.copy renderer textureBackground Nothing Nothing

    renderBall :: GameState -> IO ()
    renderBall gs =
      SDL.copy renderer textureBall Nothing (Just (toRectBall (getBall gs)))

    renderWalls :: IO ()
    renderWalls = do
      SDL.copy renderer texturePixel Nothing (Just (toRect wallLeft))
      SDL.copy renderer texturePixel Nothing (Just (toRect wallRight))
      SDL.copy renderer texturePixel Nothing (Just (toRect wallTop))
      SDL.copy renderer texturePixel Nothing (Just (toRect wallBottom))

    renderPaddle :: GameState -> Player -> IO ()
    renderPaddle gs player = do
      let tex = case player of
                  P1 -> texturePaddle1
                  P2 -> texturePaddle2
      SDL.copy renderer tex Nothing (Just (toRectPaddle (getPaddle (getPlayer player gs))))

    renderTimeRemaining :: GameState -> IO ()
    renderTimeRemaining gs = do
      let r = renderTextAlign renderer mediumFont fontColorWhite (400, 20) AlignCenter AlignTop
      if suddenDeath gs
        then r "Sudden Death!"
        else r (T.pack $ show (floor (getTimeRemainingSecs gs) :: Int))

    renderScore :: GameState -> Player -> IO ()
    renderScore gs player = do
      let halign = case player of
                     P1 -> AlignLeft
                     P2 -> AlignRight
      let x = case player of
                P1 -> 20
                P2 -> 780
      renderTextAlign renderer
                      mediumFont
                      fontColorWhite
                      (x, 20)
                      halign
                      AlignTop
                      (T.pack $ show (getScore (getPlayer player gs)))

    renderPower :: GameState -> Player -> IO ()
    renderPower gs player = do
      let halign = case player of
                     P1 -> AlignLeft
                     P2 -> AlignRight
      let x = case player of
                P1 -> 30
                P2 -> 770
      when (getPower (getPlayer player gs) /= NoPower) $ renderTextAlign
        renderer
        smallFont
        fontColorWhite
        (x, 570)
        halign
        AlignBottom
        (showPower (getPower (getPlayer player gs)))

    loop :: GameState -> IO ()
    loop oldGameState = do
      -- Get all buffered keyboard events
      events <- map SDL.eventPayload <$> SDL.pollEvents
      let gameState = oldGameState { getScreen = if SDL.QuitEvent `elem` events then Quit else getScreen oldGameState }

      -- A map of keys that are currently pressed down.
      keyMap      <- SDL.getKeyboardState

      gameState'' <- case getScreen gameState of
        Quit -> return gameState
        Menu -> do
          let gameState1 = if
                | keyMap SDL.ScancodeSpace -> startingGameState { getScreen = Play }
                | keyMap SDL.ScancodeQ     -> gameState { getScreen = Quit }
                | keyMap SDL.ScancodeH     -> gameState { getScreen = Help }
                | otherwise                -> gameState

          renderAndFlip renderer $ SDL.copy renderer textureMenu Nothing Nothing

          return gameState1
        Help -> do
          let gameState1 = if
                | keyMap SDL.ScancodeSpace -> startingGameState { getScreen = Menu }
                | keyMap SDL.ScancodeQ     -> gameState { getScreen = Quit }
                | otherwise                -> gameState

          renderAndFlip renderer $ SDL.copy renderer textureHelp Nothing Nothing

          -- If going back to the main menu, delay for 0.5 seconds so that we
          -- don't register the [space] being down twice, so that we don't
          -- start a new game right away. Give the menu a chance to render.
          when (getScreen gameState1 == Menu) (threadDelay 500000)

          return gameState1

        Pause -> do
          let gameState1 = if
                | keyMap SDL.ScancodeSpace -> gameState { getScreen = Play }
                | keyMap SDL.ScancodeQ     -> gameState { getScreen = Quit }
                | otherwise                -> gameState

          renderAndFlip renderer $ SDL.copy renderer texturePause Nothing Nothing

          -- If going back to the main menu, delay for 0.5 seconds so that we
          -- don't register the [space] being down twice, so that we don't
          -- start a new game right away. Give the menu a chance to render.
          when (getScreen gameState1 == Menu) (threadDelay 500000)

          return gameState1

        Winner -> do
          let texture = if getScore (getPlayer P1 gameState) > getScore (getPlayer P2 gameState)
                then textureWinnerBlue
                else textureWinnerPink

          renderAndFlip renderer $ SDL.copy renderer texture Nothing Nothing

          let gameState1 = if
                | keyMap SDL.ScancodeSpace -> startingGameState { getScreen = Menu }
                | keyMap SDL.ScancodeQ     -> startingGameState { getScreen = Quit }
                | otherwise                -> gameState


          -- If going back to the main menu, delay for 0.5 seconds so that we
          -- don't register the [space] being down twice, so that we don't
          -- start a new game right away. Give the menu a chance to render.
          when (getScreen gameState1 == Menu) (threadDelay 500000)

          return gameState1

        Play -> if keyMap SDL.ScancodeEscape
          then return $ gameState { getScreen = Pause }
          else do
            loopStartTime <- Clock.getTime Clock.Monotonic

            -------------------
            -- Prepare state --
            -------------------

            -- Random generators
            gameStateRandom <- registerNextRandomPowers gameState

            -- Save states that we may want to compare after simulation loops.
            let powerActive1 = getPowerActive (getPlayer P1 gameStateRandom)
            let powerActive2 = getPowerActive (getPlayer P2 gameStateRandom)

            ---------------------
            -- Simulation loop --
            ---------------------

            let simulatedGameState = simulationLoop (registerKeyPresses keyMap gameStateRandom)

            let powerActive1' = getPowerActive (getPlayer P1 simulatedGameState)
            let powerActive2' = getPowerActive (getPlayer P2 simulatedGameState)

            ------------
            -- Render --
            ------------

            renderAndFlip renderer $ do
              -- Draw stuff into buffer
              renderBackground
              renderBall simulatedGameState
              forEachPlayer $ renderPaddle simulatedGameState

              renderTimeRemaining simulatedGameState
              forEachPlayer $ renderScore simulatedGameState
              forEachPlayer $ renderPower simulatedGameState

              -- Uncomment this to debug wall positions
              -- renderWalls

            ------------------------
            -- Play sound effects --
            ------------------------

            when (powerActive1 == NoPower && powerActive1' /= NoPower) $ playPowerSfx powerActive1'
            when (powerActive2 == NoPower && powerActive2' /= NoPower) $ playPowerSfx powerActive2'

            -- Press "P" to spit out debugging info.
            when (keyMap SDL.ScancodeP) (print simulatedGameState)

            --------------------
            -- Finalize state --
            --------------------

            loopEndTime <- Clock.getTime Clock.Monotonic
            let diffSecs = fromIntegral (Clock.toNanoSecs (Clock.diffTimeSpec loopStartTime loopEndTime)) / 1000000000

            let finalGameState = applyTimeDiffSecs diffSecs simulatedGameState

            return finalGameState

      unless (getScreen gameState'' == Quit) (loop gameState'')

  -- Start the main loop.
  loop startingGameState

  SDL.destroyTexture textureBackground
  SDL.destroyTexture textureBall
  SDL.destroyTexture texturePaddle1
  SDL.destroyTexture texturePaddle2
  SDL.destroyTexture textureMenu
  SDL.destroyTexture texturePixel

  SDL.destroyWindow window
  Font.quit
  Mix.free mainMusic
  Mix.free speedSfx
  Mix.free quagmireSfx
  Mix.closeAudio
  SDL.quit
