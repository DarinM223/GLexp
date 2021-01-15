{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
module Engine (start) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (void, when)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Engine.Types
  (Coords (..), MouseInfo (..), mkCoords, noCoord, updateMouseInfo)
import Graphics.GL.Core45
import qualified Data.Set as S
import qualified Engine.Game as Game
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear

data CloseException = CloseException deriving Show
instance Exception CloseException

data WindowParams = WindowParams
  { windowKeyCallback    :: !GLFW.KeyCallback
  , windowMouseCallback  :: !GLFW.MouseButtonCallback
  , windowCursorCallback :: !GLFW.CursorPosCallback
  }

onKeyPressed :: IORef (S.Set GLFW.Key) -> GLFW.KeyCallback
onKeyPressed _ _ GLFW.Key'Escape _ GLFW.KeyState'Pressed _ =
  throwIO CloseException
onKeyPressed ref _ key _ keyState _ = case keyState of
  GLFW.KeyState'Pressed  -> modifyIORef ref (S.insert key)
  GLFW.KeyState'Released -> modifyIORef ref (S.delete key)
  _                      -> return ()

onCursorMoved :: IORef MouseInfo -> GLFW.CursorPosCallback
onCursorMoved mouseInfoRef _ x y = do
  pressed <- mouseRightPressed <$> readIORef mouseInfoRef
  when (pressed == 0) $ modifyIORef mouseInfoRef $ updateMouseInfo x y

onMousePressed :: IORef MouseInfo -> GLFW.MouseButtonCallback
onMousePressed mouseInfoRef win GLFW.MouseButton'2 state _ = do
  modifyIORef mouseInfoRef $ \info -> info { mouseRightPressed = pressed }
  if pressed /= 0
    then GLFW.setCursorInputMode win GLFW.CursorInputMode'Normal
    else do
      GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled
      lastPos <- mouseLastPos <$> readIORef mouseInfoRef
      case lastPos of
        Coords (# (# x, y #) | #) -> GLFW.setCursorPos win x y
        _                         -> return ()
 where
  pressed = case state of
    GLFW.MouseButtonState'Pressed  -> 1
    GLFW.MouseButtonState'Released -> 0
onMousePressed ref win GLFW.MouseButton'1 GLFW.MouseButtonState'Pressed _ = do
  rightButtonClicked <- mouseRightPressed <$> readIORef ref
  when (rightButtonClicked /= 0) $ do
    (x, y) <- GLFW.getCursorPos win
    modifyIORef ref $ \info -> info { mouseLeftCoords = mkCoords x y }
onMousePressed _ _ _ _ _ = return ()

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
  GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled
  GLFW.setKeyCallback win (Just (windowKeyCallback params))
  GLFW.setMouseButtonCallback win (Just (windowMouseCallback params))
  GLFW.setCursorPosCallback win (Just (windowCursorCallback params))
  GLFW.setWindowCloseCallback win (Just (const $ throwIO CloseException))
  return win

freeWindow :: GLFW.Window -> IO ()
freeWindow window = GLFW.destroyWindow window >> GLFW.terminate

gameLoop :: IORef (S.Set GLFW.Key) -> IORef MouseInfo -> GLFW.Window -> IO ()
gameLoop keysRef mouseInfoRef window = do
  (w, h) <- GLFW.getFramebufferSize window
  game0 <- Game.init w h
  glEnable GL_DEPTH_TEST

  let loop !game !lastTime = do
        GLFW.pollEvents
        Just time <- fmap realToFrac <$> GLFW.getTime
        let dt = time - lastTime

        keys <- readIORef keysRef
        mouseInfo <- readIORef mouseInfoRef
        game' <- Game.update keys mouseInfo dt game

        -- Handle left button event only once.
        case mouseLeftCoords mouseInfo of
          Coords (# _ | #) -> modifyIORef mouseInfoRef $
            \info -> info { mouseLeftCoords = noCoord }
          _ -> return ()

        Game.draw game'

        GLFW.swapBuffers window
        loop game' time
  loop game0 0.0

start :: IO ()
start = do
  keysRef <- newIORef S.empty
  mouseInfoRef <- newIORef $
    MouseInfo noCoord (0, -90) (Linear.V3 0 0 (-1)) 0 noCoord
  let
    createWindow = mkWindow $ WindowParams
      (onKeyPressed keysRef)
      (onMousePressed mouseInfoRef)
      (onCursorMoved mouseInfoRef)
  bracket createWindow freeWindow (gameLoop keysRef mouseInfoRef)
