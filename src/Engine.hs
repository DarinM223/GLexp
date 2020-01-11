{-# LANGUAGE QuasiQuotes #-}
module Engine (someFunc) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Graphics.GL.Core45
import Graphics.GL.Types
import NeatInterpolation
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Storable as V
import qualified Engine.Loader as Loader
import qualified Graphics.UI.GLFW as GLFW

data CloseException = CloseException deriving Show
instance Exception CloseException

vertexShaderSource :: ByteString
vertexShaderSource = T.encodeUtf8
  [text|
    #version 130
    in vec3 position;
    void main() {
      gl_Position = vec4(position, 1.0);
    }
  |]

fragmentShaderSource :: ByteString
fragmentShaderSource = T.encodeUtf8
  [text|
    #version 130
    out vec4 color;
    void main() {
      color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
    }
  |]

vertices :: V.Vector GLfloat
vertices = V.fromList
  [ -0.5, -0.5, 0.0
  , 0.5, -0.5, 0.0
  , 0.0, 0.5, 0.0
  ]
  --[ -- Left bottom triangle
  --  -0.5, 0.5, 0.0
  --, -0.5, -0.5, 0.0
  --, 0.5, -0.5, 0.0
  --  -- Right top triangle
  --, 0.5, -0.5, 0.0
  --, 0.5, 0.5, 0.0
  --, -0.5, 0.5, 0.0
  --]

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

gameLoop :: Loader.RawModel -> GLFW.Window -> IO ()
gameLoop model window = forever $ do
  glClearColor 0.0 0.0 1.0 1.0
  glClear GL_COLOR_BUFFER_BIT
  Loader.render model
  GLFW.swapBuffers window
  GLFW.pollEvents

someFunc :: IO ()
someFunc = do
  let params = WindowParams keyPressed mousePressed
  bracket (mkWindow params) freeWindow $ \window -> do
    vertexShader <- Loader.loadShader GL_VERTEX_SHADER vertexShaderSource
    fragmentShader <- Loader.loadShader GL_FRAGMENT_SHADER fragmentShaderSource
    program <- Loader.linkShaders [vertexShader, fragmentShader]
    glDeleteShader vertexShader
    glDeleteShader fragmentShader

    glUseProgram program

    model <- Loader.loadToVAO vertices
    gameLoop model window
