{-# LANGUAGE BangPatterns #-}
module Engine (start) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (void)
import Data.Bits ((.|.))
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
  (x, y) <- GLFW.getFramebufferSize win
  glViewport 0 0 (fromIntegral x) (fromIntegral y)
  GLFW.setKeyCallback win (Just (windowKeyCallback params))
  GLFW.setMouseButtonCallback win (Just (windowMouseCallback params))
  GLFW.setWindowCloseCallback win (Just (const $ throwIO CloseException))
  return win

freeWindow :: GLFW.Window -> IO ()
freeWindow window = GLFW.destroyWindow window >> GLFW.terminate

gameLoop :: GLFW.Window -> IO ()
gameLoop window = do
  (w, h) <- GLFW.getFramebufferSize window
  game0 <- Game.init w h
    [ Entity
      (Linear.V3 0 0 0)
      (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) 0)
      0.5
      0
    , Entity
      (Linear.V3 0.5 0 0)
      (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) 0)
      0.2
      0
    ]
  glEnable GL_DEPTH_TEST

  let loop !game = do
        GLFW.pollEvents
        Just time <- GLFW.getTime
        game' <- Game.update (realToFrac time) game

        glClearColor 0.0 0.0 1.0 1.0
        glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
        Game.draw game'

        GLFW.swapBuffers window
        loop game'
  loop game0

start :: IO ()
start =
  bracket (mkWindow (WindowParams keyPressed mousePressed)) freeWindow gameLoop
