{-# LANGUAGE BangPatterns #-}
module Engine (start) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (void)
import Data.Bits ((.|.))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Graphics.GL.Core45
import qualified Data.Set as S
import qualified Engine.Game as Game
import qualified Graphics.UI.GLFW as GLFW

data CloseException = CloseException deriving Show
instance Exception CloseException

data WindowParams = WindowParams
  { windowKeyCallback   :: !GLFW.KeyCallback
  , windowMouseCallback :: !GLFW.MouseButtonCallback
  }

keyPressed :: IORef (S.Set GLFW.Key) -> GLFW.KeyCallback
keyPressed _ _ GLFW.Key'Escape _ GLFW.KeyState'Pressed _ =
  throwIO CloseException
keyPressed ref _ key _ keyState _ = case keyState of
  GLFW.KeyState'Pressed  -> modifyIORef ref (S.insert key)
  GLFW.KeyState'Released -> modifyIORef ref (S.delete key)
  _                      -> return ()

mousePressed :: GLFW.MouseButtonCallback
mousePressed _ _ _ _ = return ()

mkWindow :: WindowParams -> IO GLFW.Window
mkWindow params = do
  void GLFW.init
  GLFW.defaultWindowHints
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
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

gameLoop :: IORef (S.Set GLFW.Key) -> GLFW.Window -> IO ()
gameLoop keysRef window = do
  (w, h) <- GLFW.getFramebufferSize window
  game0 <- Game.init w h
  glEnable GL_DEPTH_TEST

  let loop !game !lastTime = do
        GLFW.pollEvents
        Just time <- fmap realToFrac <$> GLFW.getTime
        let dt = time - lastTime
        keys <- readIORef keysRef
        game' <- Game.update keys dt game

        glClearColor 0.0 0.0 1.0 1.0
        glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
        Game.draw game'

        GLFW.swapBuffers window
        loop game' time
  loop game0 0.0

start :: IO ()
start = do
  ref <- newIORef S.empty
  let createWindow = mkWindow $ WindowParams (keyPressed ref) mousePressed
  bracket createWindow freeWindow (gameLoop ref)
