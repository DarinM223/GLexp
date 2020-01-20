{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Engine.Game where

import Control.Exception (bracket)
import Control.Lens ((&), (%~))
import Control.Monad (forM_)
import Control.Monad.Primitive (PrimState)
import Data.ByteString (ByteString)
import Data.Foldable (foldlM)
import Engine.Entity
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Graphics.GL.Core45
import Graphics.GL.Types
import Linear ((!!*))
import Linear.Quaternion (_i)
import NeatInterpolation (text)
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import qualified Engine.Utils as Utils
import qualified Linear

vertexShaderSrc :: ByteString
vertexShaderSrc = T.encodeUtf8
  [text|
    #version 130
    in vec3 position;
    in vec2 texCoord;
    in vec3 normal;
    out vec2 v_texCoord;
    uniform mat4 model;      // Transformation of the model
    uniform mat4 view;       // Transformation of the camera
    uniform mat4 projection; // Clipping coordinates outside FOV
    void main() {
      gl_Position = projection * view * model * vec4(position, 1.0);
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

perspectiveMat :: Int -> Int -> Linear.M44 GLfloat
perspectiveMat width height =
  Linear.perspective fov aspectRatio nearPlane farPlane
 where
  fov = 45 * (pi / 180)
  aspectRatio = fromIntegral width / fromIntegral height
  nearPlane = 0.1
  farPlane = 1000

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

type IOVec a = V.MVector (PrimState IO) a

-- | Program with a transformation matrix and two blended textures.
data TwoTexProgram = TwoTexProgram
  { pProgram       :: {-# UNPACK #-} !GLuint
  , pTex0Location  :: {-# UNPACK #-} !GLint
  , pTex1Location  :: {-# UNPACK #-} !GLint
  , pModelLocation :: {-# UNPACK #-} !GLint
  , pViewLocation  :: {-# UNPACK #-} !GLint
  , pProjLocation  :: {-# UNPACK #-} !GLint
  , pWidth         :: {-# UNPACK #-} !Int
  , pHeight        :: {-# UNPACK #-} !Int
  }

mkProgram :: Int -> Int -> ByteString -> ByteString -> IO TwoTexProgram
mkProgram width height vertexShaderSrc0 fragmentShaderSrc0 = do
  program <-
    bracket loadVertexShader glDeleteShader $ \vertexShader ->
    bracket loadFragmentShader glDeleteShader $ \fragmentShader ->
      Utils.linkShaders [vertexShader, fragmentShader]
  tex0Location <- withCString "texture0" $ \name ->
    glGetUniformLocation program name
  tex1Location <- withCString "texture1" $ \name ->
    glGetUniformLocation program name
  modelLocation <- withCString "model" $ \name ->
    glGetUniformLocation program name
  viewLocation <- withCString "view" $ \name ->
    glGetUniformLocation program name
  projLocation <- withCString "projection" $ \name ->
    glGetUniformLocation program name
  return TwoTexProgram { pProgram       = program
                       , pTex0Location  = tex0Location
                       , pTex1Location  = tex1Location
                       , pModelLocation = modelLocation
                       , pViewLocation  = viewLocation
                       , pProjLocation  = projLocation
                       , pWidth         = width
                       , pHeight        = height
                       }
 where
  loadVertexShader = Utils.loadShader GL_VERTEX_SHADER vertexShaderSrc0
  loadFragmentShader = Utils.loadShader GL_FRAGMENT_SHADER fragmentShaderSrc0

programSetUniforms
  :: TwoTexProgram -> GLuint -> GLuint -> Linear.M44 GLfloat -> IO ()
programSetUniforms p tex0 tex1 view = do
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D tex0
  glUniform1i (pTex0Location p) 0

  glActiveTexture GL_TEXTURE1
  glBindTexture GL_TEXTURE_2D tex1
  glUniform1i (pTex1Location p) 1

  with view $ \matrixPtr ->
    glUniformMatrix4fv (pViewLocation p) 1 GL_TRUE (castPtr matrixPtr)
  with (perspectiveMat (pWidth p) (pHeight p)) $ \matrixPtr ->
    glUniformMatrix4fv (pProjLocation p) 1 GL_TRUE (castPtr matrixPtr)

programSetModel :: TwoTexProgram -> Linear.M44 GLfloat -> IO ()
programSetModel p model = with model $ \matrixPtr ->
  glUniformMatrix4fv (pModelLocation p) 1 GL_TRUE (castPtr matrixPtr)

data Game = Game
  { gameEntities :: {-# UNPACK #-} !(IOVec Entity)
  , gameProgram  :: {-# UNPACK #-} !TwoTexProgram
  , gameView     :: {-# UNPACK #-} !(Linear.M44 GLfloat)
  , gameTexture0 :: {-# UNPACK #-} !GLuint
  , gameTexture1 :: {-# UNPACK #-} !GLuint
  , gameRawModel :: {-# UNPACK #-} !Utils.RawModel
  }

init :: Int -> Int -> [Entity] -> IO Game
init w h es = Game
  <$> V.unsafeThaw (V.fromList es)
  <*> mkProgram w h vertexShaderSrc fragmentShaderSrc
  <*> pure (Linear.lookAt (Linear.V3 0 0 15) (Linear.V3 0 0 0) (Linear.V3 0 1 0))
  <*> Utils.loadTexture "res/container.jpg"
  <*> Utils.loadTexture "res/awesomeface.png"
  <*> Utils.loadObj "res/stall.obj"

update :: GLfloat -> Game -> IO Game
update _ g0 = foldlM update' g0 [0..VM.length (gameEntities g0) - 1]
 where
  update' :: Game -> Int -> IO Game
  update' !g i = do
    let
      updateEntity :: Entity -> Entity
      updateEntity !e = e { entityRot = entityRot e & _i %~ (+ 0.01) }
    VM.modify (gameEntities g) updateEntity i
    return g

draw :: Game -> IO ()
draw g = do
  glUseProgram $ pProgram $ gameProgram g
  programSetUniforms
    (gameProgram g) (gameTexture0 g) (gameTexture1 g) (gameView g)

  forM_ [0..VM.length (gameEntities g) - 1] $ \i -> do
    e <- VM.read (gameEntities g) i
    let rotM33 = Linear.fromQuaternion (entityRot e) !!* entityScale e
        matrix = Linear.mkTransformationMat rotM33 (entityPos e)
    programSetModel (gameProgram g) matrix

    let model = gameRawModel g
    glBindVertexArray $ Utils.modelVao model
    glDrawArrays GL_TRIANGLES 0 (Utils.modelVertexCount model)
    -- TODO(DarinM223): Use this when drawing with index buffer.
    --glDrawElements
    --  GL_TRIANGLES (Utils.modelVertexCount model) GL_UNSIGNED_INT nullPtr
    glBindVertexArray 0
