{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Engine.Terrain.Terrain
  ( Terrain (..)
  , TerrainProgram (..)
  , mkTerrain
  , mkProgram
  , setUniforms
  , draw
  ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Engine.Types (Light, RawModel (..), Texture (..), setLightUniforms)
import Engine.Utils (linkShaders, loadShader)
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), peek, sizeOf)
import Graphics.GL.Core45
import Graphics.GL.Types
import NeatInterpolation (text)
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Storable as V
import qualified Linear

vertexShaderSrc :: ByteString
vertexShaderSrc = T.encodeUtf8
  [text|
    #version 330 core
    in vec3 position;
    in vec2 texCoord;
    in vec3 normal;

    out vec2 v_texCoord;
    out vec3 surfaceNormal;
    out vec3 lightVec;
    out vec3 cameraVec;
    out float visibility;

    uniform mat4 model;      // Transformation of the model
    uniform mat4 view;       // Transformation of the camera
    uniform mat4 projection; // Clipping coordinates outside FOV
    uniform vec3 lightPosition;

    const float density = 0.007;
    const float gradient = 1.5;

    void main() {
      vec4 worldPosition = model * vec4(position, 1.0);
      vec4 positionRelativeToCam = view * worldPosition;
      gl_Position = projection * positionRelativeToCam;
      v_texCoord = texCoord * 40.0;

      surfaceNormal = (model * vec4(normal, 0.0)).xyz;
      lightVec = lightPosition - worldPosition.xyz;
      cameraVec = (inverse(view) * vec4(0.0, 0.0, 0.0, 1.0)).xyz - worldPosition.xyz;

      float distance = length(positionRelativeToCam.xyz);
      visibility = clamp(exp(-pow(distance * density, gradient)), 0.0, 1.0);
    }
  |]

fragmentShaderSrc :: ByteString
fragmentShaderSrc = T.encodeUtf8
  [text|
    #version 330 core
    in vec2 v_texCoord;
    in vec3 surfaceNormal;
    in vec3 lightVec;
    in vec3 cameraVec;
    in float visibility;

    uniform sampler2D myTexture;
    uniform vec3 lightColor;
    uniform float shineDamper;
    uniform float reflectivity;
    uniform vec3 skyColor;

    out vec4 color;

    void main() {
      vec3 unitNormal = normalize(surfaceNormal);
      vec3 unitLightVec = normalize(lightVec);
      float brightness = max(dot(unitNormal, unitLightVec), 0.2);
      vec3 diffuse = brightness * lightColor;

      vec3 unitCameraVec = normalize(cameraVec);
      vec3 reflectedLightVec = reflect(-unitLightVec, unitNormal);
      float specularFactor = max(dot(reflectedLightVec, unitCameraVec), 0.0);
      float dampedFactor = pow(specularFactor, shineDamper);
      vec3 finalSpecular = dampedFactor * reflectivity * lightColor;

      color = vec4(diffuse, 1.0) * texture(myTexture, v_texCoord) + vec4(finalSpecular, 1.0);
      color = mix(vec4(skyColor, 1.0), color, visibility);
    }
  |]

terrainSize :: GLfloat
terrainSize = 800

terrainVertexCount :: GLint
terrainVertexCount = 128

data Terrain = Terrain
  { terrainX        :: {-# UNPACK #-} !GLfloat
  , terrainZ        :: {-# UNPACK #-} !GLfloat
  , terrainRawModel :: {-# UNPACK #-} !RawModel
  }

mkTerrain :: GLfloat -> GLfloat -> IO Terrain
mkTerrain x z = Terrain (x * terrainSize) (z * terrainSize) <$> generateTerrain

generateTerrain :: IO RawModel
generateTerrain = V.unsafeWith buffer $ \vPtr ->
                  V.unsafeWith indices $ \ePtr -> do
  vao <- alloca $ \vaoPtr -> do
    glGenVertexArrays 1 vaoPtr
    peek vaoPtr
  glBindVertexArray vao

  vbo <- alloca $ \vboPtr -> do
    glGenBuffers 1 vboPtr
    peek vboPtr
  glBindBuffer GL_ARRAY_BUFFER vbo
  glBufferData GL_ARRAY_BUFFER vSize (castPtr vPtr) GL_STATIC_DRAW

  ebo <- alloca $ \eboPtr -> do
    glGenBuffers 1 eboPtr
    peek eboPtr
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
  glBufferData GL_ELEMENT_ARRAY_BUFFER eSize (castPtr ePtr) GL_STATIC_DRAW

  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE stride nullPtr
  glEnableVertexAttribArray 0

  glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE stride
    (nullPtr `plusPtr` (3 * sizeOf (undefined :: GLfloat)))
  glEnableVertexAttribArray 1

  glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE stride
    (nullPtr `plusPtr` (5 * sizeOf (undefined :: GLfloat)))
  glEnableVertexAttribArray 2

  glBindBuffer GL_ARRAY_BUFFER 0
  glBindVertexArray 0
  return RawModel { modelVao         = vao
                  , modelVertexCount = fromIntegral $ V.length indices
                  }
 where
  vSize = fromIntegral $ sizeOf (undefined :: GLfloat) * V.length buffer
  eSize = fromIntegral $ sizeOf (undefined :: GLuint) * V.length indices
  stride = fromIntegral $ sizeOf (undefined :: GLfloat) * 8

  buffer :: V.Vector GLfloat
  buffer = V.fromList $ do
    i <- [0..terrainVertexCount - 1]
    j <- [0..terrainVertexCount - 1]
    let texCoordX = fromIntegral j / fromIntegral (terrainVertexCount - 1)
        texCoordZ = fromIntegral i / fromIntegral (terrainVertexCount - 1)
        vertX     = texCoordX * terrainSize
        vertZ     = texCoordZ * terrainSize
    [vertX, 0, vertZ, texCoordX, texCoordZ, 0, 1, 0]

  indices :: V.Vector GLuint
  indices = V.fromList . fmap fromIntegral $ do
    z <- [0..terrainVertexCount - 2]
    x <- [0..terrainVertexCount - 2]
    let topLeft     = z * terrainVertexCount + x
        topRight    = topLeft + 1
        bottomLeft  = (z + 1) * terrainVertexCount + x
        bottomRight = bottomLeft + 1
    [topLeft, bottomLeft, topRight, topRight, bottomLeft, bottomRight]

data TerrainProgram = TerrainProgram
  { tProgram         :: {-# UNPACK #-} !GLuint
  , tTextureLoc      :: {-# UNPACK #-} !GLint
  , tModelLoc        :: {-# UNPACK #-} !GLint
  , tViewLoc         :: {-# UNPACK #-} !GLint
  , tProjLoc         :: {-# UNPACK #-} !GLint
  , tLightPosLoc     :: {-# UNPACK #-} !GLint
  , tLightColorLoc   :: {-# UNPACK #-} !GLint
  , tShineDamperLoc  :: {-# UNPACK #-} !GLint
  , tReflectivityLoc :: {-# UNPACK #-} !GLint
  , tSkyColorLoc     :: {-# UNPACK #-} !GLint
  }

mkProgram :: IO TerrainProgram
mkProgram = do
  tProgram <-
    bracket loadVertexShader glDeleteShader $ \vertexShader ->
    bracket loadFragmentShader glDeleteShader $ \fragmentShader ->
      linkShaders [vertexShader, fragmentShader]
  tTextureLoc <- withCString "texture" $ \name ->
    glGetUniformLocation tProgram name
  tModelLoc <- withCString "model" $ \name ->
    glGetUniformLocation tProgram name
  tViewLoc <- withCString "view" $ \name ->
    glGetUniformLocation tProgram name
  tProjLoc <- withCString "projection" $ \name ->
    glGetUniformLocation tProgram name
  tLightPosLoc <- withCString "lightPosition" $ \name ->
    glGetUniformLocation tProgram name
  tLightColorLoc <- withCString "lightColor" $ \name ->
    glGetUniformLocation tProgram name
  tShineDamperLoc <- withCString "shineDamper" $ \name ->
    glGetUniformLocation tProgram name
  tReflectivityLoc <- withCString "reflectivity" $ \name ->
    glGetUniformLocation tProgram name
  tSkyColorLoc <- withCString "skyColor" $ \name ->
    glGetUniformLocation tProgram name
  return TerrainProgram{..}
 where
  loadVertexShader = loadShader GL_VERTEX_SHADER vertexShaderSrc
  loadFragmentShader = loadShader GL_FRAGMENT_SHADER fragmentShaderSrc

setUniforms
  :: TerrainProgram
  -> Texture
  -> Light
  -> Linear.V3 GLfloat
  -> Linear.M44 GLfloat
  -> Linear.M44 GLfloat
  -> IO ()
setUniforms p tex light skyColor view proj = do
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D $ textureID tex
  glUniform1i (tTextureLoc p) 0
  glUniform1f (tShineDamperLoc p) (textureShineDamper tex)
  glUniform1f (tReflectivityLoc p) (textureReflectivity tex)

  with view $ \matrixPtr ->
    glUniformMatrix4fv (tViewLoc p) 1 GL_TRUE (castPtr matrixPtr)
  with proj $ \matrixPtr ->
    glUniformMatrix4fv (tProjLoc p) 1 GL_TRUE (castPtr matrixPtr)
  setLightUniforms light (tLightPosLoc p) (tLightColorLoc p)
  glUniform3f (tSkyColorLoc p) r g b
 where Linear.V3 r g b = skyColor

draw :: Terrain -> TerrainProgram -> IO ()
draw t p = do
  with model $ \matrixPtr ->
    glUniformMatrix4fv (tModelLoc p) 1 GL_TRUE (castPtr matrixPtr)

  glBindVertexArray $ modelVao $ terrainRawModel t
  glDrawElements
    GL_TRIANGLES (modelVertexCount $ terrainRawModel t) GL_UNSIGNED_INT nullPtr
  glBindVertexArray 0
 where
  model = Linear.mkTransformationMat
    Linear.identity (Linear.V3 (terrainX t) 0 (terrainZ t))
