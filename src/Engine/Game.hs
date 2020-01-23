{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Engine.Game where

import Control.Exception (bracket)
import Control.Lens ((&), (%~))
import Control.Monad (forM_)
import Control.Monad.Primitive (PrimState)
import Data.ByteString (ByteString)
import Data.Foldable (foldlM)
import Engine.Entity
import Engine.Utils
  ( RawModel (..)
  , Texture (..)
  , linkShaders
  , loadObj
  , loadShader
  , loadTexture
  , textureID
  )
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
import qualified Linear

vertexShaderSrc :: ByteString
vertexShaderSrc = T.encodeUtf8
  [text|
    #version 130
    in vec3 position;
    in vec2 texCoord;
    in vec3 normal;

    out vec2 v_texCoord;
    out vec3 surfaceNormal;
    out vec3 lightVec;

    uniform mat4 model;      // Transformation of the model
    uniform mat4 view;       // Transformation of the camera
    uniform mat4 projection; // Clipping coordinates outside FOV
    uniform vec3 lightPosition;

    void main() {
      vec4 worldPosition = model * vec4(position, 1.0);
      gl_Position = projection * view * worldPosition;
      v_texCoord = texCoord;

      surfaceNormal = (model * vec4(normal, 0.0)).xyz;
      lightVec = lightPosition - worldPosition.xyz;
    }
  |]

fragmentShaderSrc :: ByteString
fragmentShaderSrc = T.encodeUtf8
  [text|
    #version 130
    in vec2 v_texCoord;
    in vec3 surfaceNormal;
    in vec3 lightVec;

    uniform sampler2D texture0;
    uniform sampler2D texture1;
    uniform vec3 lightColor;

    out vec4 color;

    void main() {
      vec3 unitNormal = normalize(surfaceNormal);
      vec3 unitLightVec = normalize(lightVec);
      float brightness = max(dot(unitNormal, unitLightVec), 0.2);
      vec3 diffuse = brightness * lightColor;

      color = vec4(diffuse, 1.0) * vec4(1.0, 0.0, 0.0, 0.0);
      // color = vec4(diffuse, 1.0) * mix(texture(texture0, v_texCoord), texture(texture1, v_texCoord), 0.2);
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

data Light = Light
  { lightPos   :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , lightColor :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  }

-- | Program with a transformation matrix and two blended textures.
data TwoTexProgram = TwoTexProgram
  { pProgram       :: {-# UNPACK #-} !GLuint
  , pTex0Loc       :: {-# UNPACK #-} !GLint
  , pTex1Loc       :: {-# UNPACK #-} !GLint
  , pModelLoc      :: {-# UNPACK #-} !GLint
  , pViewLoc       :: {-# UNPACK #-} !GLint
  , pProjLoc       :: {-# UNPACK #-} !GLint
  , pLightPosLoc   :: {-# UNPACK #-} !GLint
  , pLightColorLoc :: {-# UNPACK #-} !GLint
  , pWidth         :: {-# UNPACK #-} !Int
  , pHeight        :: {-# UNPACK #-} !Int
  }

mkProgram :: Int -> Int -> ByteString -> ByteString -> IO TwoTexProgram
mkProgram pWidth pHeight vertexShaderSrc0 fragmentShaderSrc0 = do
  pProgram <-
    bracket loadVertexShader glDeleteShader $ \vertexShader ->
    bracket loadFragmentShader glDeleteShader $ \fragmentShader ->
      linkShaders [vertexShader, fragmentShader]
  pTex0Loc <- withCString "texture0" $ \name ->
    glGetUniformLocation pProgram name
  pTex1Loc <- withCString "texture1" $ \name ->
    glGetUniformLocation pProgram name
  pModelLoc <- withCString "model" $ \name ->
    glGetUniformLocation pProgram name
  pViewLoc <- withCString "view" $ \name ->
    glGetUniformLocation pProgram name
  pProjLoc <- withCString "projection" $ \name ->
    glGetUniformLocation pProgram name
  pLightPosLoc <- withCString "lightPosition" $ \name ->
    glGetUniformLocation pProgram name
  pLightColorLoc <- withCString "lightColor" $ \name ->
    glGetUniformLocation pProgram name
  return TwoTexProgram{..}
 where
  loadVertexShader = loadShader GL_VERTEX_SHADER vertexShaderSrc0
  loadFragmentShader = loadShader GL_FRAGMENT_SHADER fragmentShaderSrc0

programSetUniforms
  :: TwoTexProgram -> Light -> Texture -> Texture -> Linear.M44 GLfloat -> IO ()
programSetUniforms p light tex0 tex1 view = do
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D $ textureID tex0
  glUniform1i (pTex0Loc p) 0

  glActiveTexture GL_TEXTURE1
  glBindTexture GL_TEXTURE_2D $ textureID tex1
  glUniform1i (pTex1Loc p) 1

  with view $ \matrixPtr ->
    glUniformMatrix4fv (pViewLoc p) 1 GL_TRUE (castPtr matrixPtr)
  with (perspectiveMat (pWidth p) (pHeight p)) $ \matrixPtr ->
    glUniformMatrix4fv (pProjLoc p) 1 GL_TRUE (castPtr matrixPtr)
  glUniform3f (pLightPosLoc p) posX posY posZ
  glUniform3f (pLightColorLoc p) cX cY cZ
 where
  Light { lightPos = Linear.V3 posX posY posZ
        , lightColor = Linear.V3 cX cY cZ } = light

programSetModel :: TwoTexProgram -> Linear.M44 GLfloat -> IO ()
programSetModel p model = with model $ \matrixPtr ->
  glUniformMatrix4fv (pModelLoc p) 1 GL_TRUE (castPtr matrixPtr)

data Game = Game
  { gameEntities :: {-# UNPACK #-} !(IOVec Entity)
  , gameProgram  :: {-# UNPACK #-} !TwoTexProgram
  , gameView     :: {-# UNPACK #-} !(Linear.M44 GLfloat)
  , gameLight    :: {-# UNPACK #-} !Light
  , gameTexture0 :: {-# UNPACK #-} !Texture
  , gameTexture1 :: {-# UNPACK #-} !Texture
  , gameRawModel :: {-# UNPACK #-} !RawModel
  }

init :: Int -> Int -> [Entity] -> IO Game
init w h es = Game
  <$> V.unsafeThaw (V.fromList es)
  <*> mkProgram w h vertexShaderSrc fragmentShaderSrc
  <*> pure (Linear.lookAt (Linear.V3 0 0 15) (Linear.V3 0 0 0) (Linear.V3 0 1 0))
  <*> pure (Light (Linear.V3 0 0 10) (Linear.V3 1 1 1))
  <*> loadTexture "res/container.jpg"
  <*> loadTexture "res/awesomeface.png"
  <*> loadObj "res/dragon.obj"

update :: GLfloat -> Game -> IO Game
update _ g0 = foldlM update' g0 [0..VM.length (gameEntities g0) - 1]
 where
  update' :: Game -> Int -> IO Game
  update' !g i = do
    let
      updateEntity :: Entity -> Entity
      updateEntity !e = e
        { entityRot = entityRot e * Linear.axisAngle (Linear.V3 0 1 0) 0.01 }
    VM.modify (gameEntities g) updateEntity i
    return g

draw :: Game -> IO ()
draw g = do
  glUseProgram $ pProgram $ gameProgram g
  programSetUniforms
    (gameProgram g) (gameLight g)(gameTexture0 g) (gameTexture1 g) (gameView g)

  forM_ [0..VM.length (gameEntities g) - 1] $ \i -> do
    e <- VM.read (gameEntities g) i
    let rotM33 = Linear.fromQuaternion (entityRot e) !!* entityScale e
        matrix = Linear.mkTransformationMat rotM33 (entityPos e)
    programSetModel (gameProgram g) matrix

    glBindVertexArray $ modelVao $ gameRawModel g
    glDrawArrays GL_TRIANGLES 0 $ modelVertexCount $ gameRawModel g
    -- TODO(DarinM223): Use this when drawing with index buffer.
    --glDrawElements
    --  GL_TRIANGLES (Utils.modelVertexCount model) GL_UNSIGNED_INT nullPtr
    glBindVertexArray 0
