{-# LANGUAGE BangPatterns #-}
module Engine (start) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (void)
import Engine.Entity
import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Engine.Game as Game
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear

data CloseException = CloseException deriving Show
instance Exception CloseException

data WindowParams = WindowParams
  { windowKeyCallback   :: !GLFW.KeyCallback
  , windowMouseCallback :: !GLFW.MouseButtonCallback
  }

keyPressed :: GLFW.KeyCallback
keyPressed _ GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = throwIO CloseException
keyPressed _ _ _ _ _ = return ()

mousePressed :: GLFW.MouseButtonCallback
mousePressed _ _ _ _ = return ()

mkWindow :: WindowParams -> IO GLFW.Window
mkWindow params = do
  void GLFW.init
  GLFW.defaultWindowHints
  Just win <- GLFW.createWindow 640 480 "GLFW Demo" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  GLFW.setKeyCallback win (Just (windowKeyCallback params))
  GLFW.setMouseButtonCallback win (Just (windowMouseCallback params))
  GLFW.setWindowCloseCallback win (Just (const $ throwIO CloseException))
  return win

freeWindow :: GLFW.Window -> IO ()
freeWindow window = GLFW.destroyWindow window >> GLFW.terminate

gameLoop :: GLFW.Window -> IO ()
gameLoop window = do
  game0 <- Game.init
    [ Entity
      (Linear.V3 0 0 0)
      (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) (pi / 2))
      0.5
      0
    , Entity
      (Linear.V3 0.5 0 0)
      (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) (pi / 2))
      0.2
      0
    ]

  let loop !game = do
        GLFW.pollEvents
        glClearColor 0.0 0.0 1.0 1.0
        glClear GL_COLOR_BUFFER_BIT

        game' <- Game.update game
        Game.draw game'

        GLFW.swapBuffers window
        loop game'
  loop game0

start :: IO ()
start =
  bracket (mkWindow (WindowParams keyPressed mousePressed)) freeWindow gameLoop
