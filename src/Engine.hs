{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Engine (start) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Foreign.C.String (withCString)
import Foreign.Ptr (nullPtr)
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
    in vec2 texCoord;
    out vec2 v_texCoord;
    void main() {
      gl_Position = vec4(position, 1.0);
      v_texCoord = texCoord;
    }
  |]

fragmentShaderSrc :: ByteString
fragmentShaderSrc = T.encodeUtf8
  [text|
    #version 130
    in vec2 v_texCoord;
    uniform sampler2D texture0;
    uniform sampler2D texture1;
    out vec4 color;

    void main() {
      color = mix(texture(texture0, v_texCoord), texture(texture1, v_texCoord), 0.2);
    }
  |]

vertices :: V.Vector GLfloat
vertices = V.fromList
  [ -0.5, 0.5, 0.0   , 0.0, 0.0
  , -0.5, -0.5, 0.0  , 0.0, 1.0
  , 0.5, -0.5, 0.0   , 1.0, 1.0
  , 0.5, 0.5, 0.0    , 1.0, 0.0
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

gameLoop :: GLFW.Window -> IO ()
gameLoop window = do
  Utils.RawModel{..} <- Utils.loadVAO vertices indices
  texture0 <- Utils.loadTexture "res/container.jpg"
  texture1 <- Utils.loadTexture "res/awesomeface.png"
  program <-
    bracket loadVertexShader glDeleteShader $ \vertexShader ->
    bracket loadFragmentShader glDeleteShader $ \fragmentShader ->
      Utils.linkShaders [vertexShader, fragmentShader]
  glUseProgram program

  uniform0Location <- withCString "texture0" $ \name ->
    glGetUniformLocation program name
  uniform1Location <- withCString "texture1" $ \name ->
    glGetUniformLocation program name

  forever $ do
    glClearColor 0.0 0.0 1.0 1.0
    glClear GL_COLOR_BUFFER_BIT

    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D texture0
    glUniform1i uniform0Location 0

    glActiveTexture GL_TEXTURE1
    glBindTexture GL_TEXTURE_2D texture1
    glUniform1i uniform1Location 1

    glBindVertexArray modelVao
    glDrawElements GL_TRIANGLES modelVertexCount GL_UNSIGNED_INT nullPtr
    glBindVertexArray 0

    GLFW.swapBuffers window
    GLFW.pollEvents
 where
  loadVertexShader = Utils.loadShader GL_VERTEX_SHADER vertexShaderSrc
  loadFragmentShader = Utils.loadShader GL_FRAGMENT_SHADER fragmentShaderSrc

start :: IO ()
start =
  bracket (mkWindow (WindowParams keyPressed mousePressed)) freeWindow gameLoop
