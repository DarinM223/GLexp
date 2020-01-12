{-# LANGUAGE QuasiQuotes #-}
module Engine (start) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Graphics.GL.Core45
import Graphics.GL.Types
import NeatInterpolation (text)
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Storable as V
import qualified Engine.Utils as Utils
import qualified Graphics.UI.GLFW as GLFW

data CloseException = CloseException deriving Show
instance Exception CloseException

vertexShaderSrc :: ByteString
vertexShaderSrc = T.encodeUtf8
  [text|
    #version 130
    in vec3 position;
    void main() {
      gl_Position = vec4(position, 1.0);
    }
  |]

fragmentShaderSrc :: ByteString
fragmentShaderSrc = T.encodeUtf8
  [text|
    #version 130
    out vec4 color;
    void main() {
      color = vec4(1.0f, 0.5f, 0.2f, 1.0f);
    }
  |]

vertices :: V.Vector GLfloat
vertices = V.fromList
  [ -0.5, 0.5, 0.0
  , -0.5, -0.5, 0.0
  , 0.5, -0.5, 0.0
  , 0.5, 0.5, 0.0
  ]

indices :: V.Vector GLuint
indices = V.fromList
  [ 0, 1, 3 -- Top left triangle
  , 3, 1, 2 -- Bottom right triangle
  ]

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

gameLoop :: Utils.RawModel -> GLFW.Window -> IO ()
gameLoop model window = forever $ do
  glClearColor 0.0 0.0 1.0 1.0
  glClear GL_COLOR_BUFFER_BIT
  Utils.render model
  GLFW.swapBuffers window
  GLFW.pollEvents

start :: IO ()
start = do
  let params = WindowParams keyPressed mousePressed
  bracket (mkWindow params) freeWindow $ \window -> do
    program <-
      bracket loadVertexShader glDeleteShader $ \vertexShader ->
      bracket loadFragmentShader glDeleteShader $ \fragmentShader ->
        Utils.linkShaders [vertexShader, fragmentShader]
    glUseProgram program
    model <- Utils.loadVAO vertices indices
    gameLoop model window
 where
  loadVertexShader = Utils.loadShader GL_VERTEX_SHADER vertexShaderSrc
  loadFragmentShader = Utils.loadShader GL_FRAGMENT_SHADER fragmentShaderSrc
