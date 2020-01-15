{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Engine (start) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (forever, forM_, void)
import Control.Monad.Primitive (PrimState)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Engine.Entity
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullPtr)
import Graphics.GL.Core45
import Graphics.GL.Types
import Linear ((!!*))
import NeatInterpolation (text)
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import qualified Engine.Utils as Utils
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear

data CloseException = CloseException deriving Show
instance Exception CloseException

vertexShaderSrc :: ByteString
vertexShaderSrc = T.encodeUtf8
  [text|
    #version 130
    in vec3 position;
    in vec2 texCoord;
    out vec2 v_texCoord;
    uniform mat4 transform;
    void main() {
      gl_Position = transform * vec4(position, 1.0);
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
  [ -1.0, 0.5, 0.0   , 0.0, 0.0
  , -1.0, -0.5, 0.0  , 0.0, 1.0
  , 1.0, -0.5, 0.0   , 1.0, 1.0
  , 1.0, 0.5, 0.0    , 1.0, 0.0
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

type IOVec a = V.MVector (PrimState IO) a

-- | Program with a transformation matrix and two blended textures.
data TwoTexProgram = TwoTexProgram
  { pProgram           :: {-# UNPACK #-} !GLuint
  , pTex0Location      :: {-# UNPACK #-} !GLint
  , pTex1Location      :: {-# UNPACK #-} !GLint
  , pTransformLocation :: {-# UNPACK #-} !GLint
  }

mkProgram :: ByteString -> ByteString -> IO TwoTexProgram
mkProgram vertexShaderSrc0 fragmentShaderSrc0 = do
  program <-
    bracket loadVertexShader glDeleteShader $ \vertexShader ->
    bracket loadFragmentShader glDeleteShader $ \fragmentShader ->
      Utils.linkShaders [vertexShader, fragmentShader]
  tex0Location <- withCString "texture0" $ \name ->
    glGetUniformLocation program name
  tex1Location <- withCString "texture1" $ \name ->
    glGetUniformLocation program name
  transformLocation <- withCString "transform" $ \name ->
    glGetUniformLocation program name
  return TwoTexProgram { pProgram           = program
                       , pTex0Location      = tex0Location
                       , pTex1Location      = tex1Location
                       , pTransformLocation = transformLocation
                       }
 where
  loadVertexShader = Utils.loadShader GL_VERTEX_SHADER vertexShaderSrc0
  loadFragmentShader = Utils.loadShader GL_FRAGMENT_SHADER fragmentShaderSrc0

programSetUniforms
  :: TwoTexProgram -> GLuint -> GLuint -> Linear.M44 GLfloat -> IO ()
programSetUniforms p tex0 tex1 matrix = do
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D tex0
  glUniform1i (pTex0Location p) 0

  glActiveTexture GL_TEXTURE1
  glBindTexture GL_TEXTURE_2D tex1
  glUniform1i (pTex1Location p) 1

  with matrix $ \matrixPtr ->
    glUniformMatrix4fv (pTransformLocation p) 1 GL_TRUE (castPtr matrixPtr)

data Game = Game
  { gameEntities :: {-# UNPACK #-} !(IOVec Entity)
  , gameProgram  :: {-# UNPACK #-} !TwoTexProgram
  , gameTexture0 :: {-# UNPACK #-} !GLuint
  , gameTexture1 :: {-# UNPACK #-} !GLuint
  , gameRawModel :: {-# UNPACK #-} !Utils.RawModel
  }

initGame :: [Entity] -> IO Game
initGame es = Game
  <$> V.unsafeThaw (V.fromList es)
  <*> mkProgram vertexShaderSrc fragmentShaderSrc
  <*> Utils.loadTexture "res/container.jpg"
  <*> Utils.loadTexture "res/awesomeface.png"
  <*> Utils.loadVAO vertices indices

updateGame :: Game -> IO ()
updateGame g = traverse_ update [0..VM.length (gameEntities g) - 1]
 where
  update :: Int -> IO ()
  update i = do
    let
      updateEntity :: Entity -> Entity
      updateEntity !e = e { entityPos = entityPos e + Linear.V3 0.001 0.001 0.0
                          , entityScale = entityScale e - 0.001
                          }
    VM.modify (gameEntities g) updateEntity i

drawGame :: Game -> IO ()
drawGame g = do
  glUseProgram $ pProgram $ gameProgram g
  forM_ [0..VM.length (gameEntities g) - 1] $ \i -> do
    e <- VM.read (gameEntities g) i
    let rotM33 = Linear.fromQuaternion (entityRot e) !!* entityScale e
        matrix = Linear.mkTransformationMat rotM33 (entityPos e)
    programSetUniforms (gameProgram g) (gameTexture0 g) (gameTexture1 g) matrix

    let model = gameRawModel g
    glBindVertexArray $ Utils.modelVao model
    glDrawElements
      GL_TRIANGLES (Utils.modelVertexCount model) GL_UNSIGNED_INT nullPtr
    glBindVertexArray 0

gameLoop :: GLFW.Window -> IO ()
gameLoop window = do
  game <- initGame
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

  forever $ do
    glClearColor 0.0 0.0 1.0 1.0
    glClear GL_COLOR_BUFFER_BIT

    updateGame game
    drawGame game

    GLFW.swapBuffers window
    GLFW.pollEvents

start :: IO ()
start =
  bracket (mkWindow (WindowParams keyPressed mousePressed)) freeWindow gameLoop
