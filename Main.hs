module Main where

import qualified Data.Set                             as S
import           Graphics.Gloss.Geometry.Angle        (degToRad, normalizeAngle)
import           Graphics.Gloss.Interface.Environment (getScreenSize)
import           Graphics.Gloss.Interface.IO.Game     (Display (InWindow),
                                                       Event (EventKey),
                                                       Key (SpecialKey),
                                                       KeyState (Down, Up),
                                                       Picture (Blank, Pictures),
                                                       SpecialKey (KeyLeft, KeyRight, KeyUp),
                                                       black, line, playIO,
                                                       rotate, scale, translate,
                                                       white)


width = 800
height = 600

data Object = Object
    { x      :: Float
    , y      :: Float
    , dx     :: Float
    , dy     :: Float
    , speed  :: Float
    , size   :: Float
    , angle  :: Float
    , points :: [(Float, Float)]
    }

data State = State
    { player  :: Object
    , pressed :: S.Set Key
    }

initState :: State
initState = State
    { player  = Object
                    { x      = 0
                    , y      = 0
                    , dx     = 0
                    , dy     = 0
                    , speed  = 5
                    , size   = 5
                    , angle  = 0
                    , points = [(-2.5, -2.5), (0, 5), (2.5, -2.5), (-2.5, -2.5)]
                    }
    , pressed = S.empty
    }

main :: IO ()
main = playIO (InWindow "Asteroids" (800, 600) (0, 0))
              white
              60
              initState
              render
              update
              step

update :: Event -> State -> IO State
update event state = return $ case event of
    (EventKey (SpecialKey KeyLeft) Down _ _)  -> keyPressed KeyLeft
    (EventKey (SpecialKey KeyLeft) Up _ _)    -> keyReleased KeyLeft
    (EventKey (SpecialKey KeyRight) Down _ _) -> keyPressed KeyRight
    (EventKey (SpecialKey KeyRight) Up _ _)   -> keyReleased KeyRight
    (EventKey (SpecialKey KeyUp) Down _ _)    -> keyPressed KeyUp
    (EventKey (SpecialKey KeyUp) Up _ _)      -> keyReleased KeyUp
    _                                         -> state
  where
    pressedSt = pressed state

    keyPressed key = state { pressed = S.insert (SpecialKey key) pressedSt }

    keyReleased key = state { pressed = S.delete (SpecialKey key) pressedSt }

step :: Float -> State -> IO State
step time state
    | S.member (SpecialKey KeyLeft) (pressed state) = move state
        { player = (player state) { angle = (angle . player) state - 5 }
        }
    | S.member (SpecialKey KeyRight) (pressed state) = move state
        { player = (player state) { angle = (angle . player) state + 5 }
        }
    | S.member (SpecialKey KeyUp) (pressed state) = move state
        { player = (player state) { speed = (speed . player) state + 1 }
        }
    | otherwise = move state
  where
    move state = getScreenSize >>= \(_, _) -> return state
        { player = (player state)
                       { x = let x' =
                                     (x . player) state
                                         + sin
                                               ((degToRad . angle . player)
                                                   state
                                               )
                                         * (speed . player) state
                             in  wrapAround x' $ fromIntegral width
                       , y = let y' =
                                     (y . player) state
                                         + cos
                                               ((degToRad . angle . player)
                                                   state
                                               )
                                         * (speed . player) state
                             in  wrapAround y' $ fromIntegral height
                       }
        }

    wrapAround n dim | n > dim / 2  = -dim / 2
                        | n < -dim / 2 = dim / 2
                        | otherwise       = n

render :: State -> IO Picture
render state = return
  $ translate (x playerSt) (y playerSt)
  $ rotate (angle playerSt)
  $ scale (size playerSt) (size playerSt)
  $ line
  $ points playerSt
  where
    playerSt = player state
