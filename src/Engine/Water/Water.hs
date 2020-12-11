{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Engine.Water.Water
  ( WaterProgram
  , Water (dudvMap)
  , mkProgram
  , mkWater
  , use
  , setUniforms
  , setTextures
  , update
  , drawTile
  ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.Fixed (mod')
import Data.IORef
import Engine.Types
import Engine.Utils (linkShaders, loadShader, loadTexture, loadVAO)
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Graphics.GL.Core45
import Graphics.GL.Types
import Linear ((!!*))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as V
import qualified Linear
import qualified Text.RawString.QQ as QQ

waterTileSize :: GLfloat
waterTileSize = 20

waveSpeed :: GLfloat
waveSpeed = 0.03

vertexShaderSrc :: ByteString
vertexShaderSrc = BS.pack
  [QQ.r|
    #version 330 core
    in vec2 position;
    out vec4 clipSpace;
    out vec2 texCoord;
    out vec3 toCameraVector;

    uniform mat4 model;
    uniform mat4 view;
    uniform mat4 projection;
    uniform vec3 cameraPosition;

    const float tiling = 6.0;

    void main() {
      vec4 worldPosition = model * vec4(position.x, 0.0, position.y, 1.0);
      clipSpace = projection * view * worldPosition;
      gl_Position = clipSpace;
      texCoord = vec2(position.x / 2.0 + 0.5, position.y / 2.0 + 0.5) * tiling;
      toCameraVector = cameraPosition - worldPosition.xyz;
    }
  |]

fragmentShaderSrc :: ByteString
fragmentShaderSrc = BS.pack
  [QQ.r|
    #version 330 core
    in vec4 clipSpace;
    in vec2 texCoord;
    in vec3 toCameraVector;
    out vec4 color;

    uniform sampler2D reflectionTexture;
    uniform sampler2D refractionTexture;
    uniform sampler2D dudvMap;

    uniform float moveFactor;

    const float waveStrength = 0.02;

    void main() {
      vec2 ndc = (clipSpace.xy / clipSpace.w) / 2.0 + 0.5;
      vec2 refractTexCoords = vec2(ndc.x, ndc.y);
      vec2 reflectTexCoords = vec2(ndc.x, -ndc.y);

      vec2 distortion1 = (texture(dudvMap, vec2(texCoord.x + moveFactor, texCoord.y)).rg * 2.0 - 1.0) * waveStrength;
      vec2 distortion2 = (texture(dudvMap, vec2(-texCoord.x + moveFactor, texCoord.y + moveFactor)).rg * 2.0 - 1.0) * waveStrength;
      vec2 totalDistortion = distortion1 + distortion2;

      refractTexCoords += totalDistortion;
      refractTexCoords = clamp(refractTexCoords, 0.001, 0.999);

      reflectTexCoords += totalDistortion;
      reflectTexCoords.x = clamp(reflectTexCoords.x, 0.001, 0.999);
      reflectTexCoords.y = clamp(reflectTexCoords.y, -0.999, -0.001);

      vec4 reflectColor = texture(reflectionTexture, reflectTexCoords);
      vec4 refractColor = texture(refractionTexture, refractTexCoords);

      float refractiveFactor = dot(normalize(toCameraVector), vec3(0.0, 1.0, 0.0));

      color = mix(reflectColor, refractColor, refractiveFactor);
      color = mix(color, vec4(0.0, 0.3, 0.5, 1.0), 0.2);
    }
  |]

data WaterProgram = WaterProgram
  { wProgram              :: {-# UNPACK #-} !GLuint
  , wModelLoc             :: {-# UNPACK #-} !GLint
  , wViewLoc              :: {-# UNPACK #-} !GLint
  , wProjLoc              :: {-# UNPACK #-} !GLint
  , wCameraPositionLoc    :: {-# UNPACK #-} !GLint
  , wReflectionTextureLoc :: {-# UNPACK #-} !GLint
  , wRefractionTextureLoc :: {-# UNPACK #-} !GLint
  , wDudvMapLoc           :: {-# UNPACK #-} !GLint
  , wMoveFactorLoc        :: {-# UNPACK #-} !GLint
  }

mkProgram :: IO WaterProgram
mkProgram = do
  wProgram <-
    bracket loadVertexShader glDeleteShader $ \vertexShader ->
    bracket loadFragmentShader glDeleteShader $ \fragmentShader ->
      linkShaders [vertexShader, fragmentShader]
  wModelLoc <- withCString "model" $ \name ->
    glGetUniformLocation wProgram name
  wViewLoc <- withCString "view" $ \name ->
    glGetUniformLocation wProgram name
  wProjLoc <- withCString "projection" $ \name ->
    glGetUniformLocation wProgram name
  wCameraPositionLoc <- withCString "cameraPosition" $ \name ->
    glGetUniformLocation wProgram name
  wReflectionTextureLoc <- withCString "reflectionTexture" $ \name ->
    glGetUniformLocation wProgram name
  wRefractionTextureLoc <- withCString "refractionTexture" $ \name ->
    glGetUniformLocation wProgram name
  wDudvMapLoc <- withCString "dudvMap" $ \name ->
    glGetUniformLocation wProgram name
  wMoveFactorLoc <- withCString "moveFactor" $ \name ->
    glGetUniformLocation wProgram name
  return WaterProgram{..}
 where
  loadVertexShader = loadShader GL_VERTEX_SHADER vertexShaderSrc
  loadFragmentShader = loadShader GL_FRAGMENT_SHADER fragmentShaderSrc

use :: WaterProgram -> IO ()
use = glUseProgram . wProgram

setUniforms :: WaterProgram
            -> Linear.M44 GLfloat
            -> Linear.M44 GLfloat
            -> Linear.V3 GLfloat
            -> GLfloat
            -> IO ()
setUniforms p view proj cameraPosition moveFactor = do
  glUniform3f (wCameraPositionLoc p) posx posy posz
  with view $ \matrixPtr ->
    glUniformMatrix4fv (wViewLoc p) 1 GL_TRUE (castPtr matrixPtr)
  with proj $ \matrixPtr ->
    glUniformMatrix4fv (wProjLoc p) 1 GL_TRUE (castPtr matrixPtr)
  glUniform1f (wMoveFactorLoc p) moveFactor
 where Linear.V3 posx posy posz = cameraPosition

setTextures :: WaterProgram -> GLuint -> GLuint -> Texture -> IO ()
setTextures p reflectTex refractTex dudvTex = do
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D reflectTex
  glUniform1i (wReflectionTextureLoc p) 0

  glActiveTexture GL_TEXTURE1
  glBindTexture GL_TEXTURE_2D refractTex
  glUniform1i (wRefractionTextureLoc p) 1

  glActiveTexture GL_TEXTURE2
  glBindTexture GL_TEXTURE_2D $ textureID dudvTex
  glUniform1i (wDudvMapLoc p) 2

-- | Only the x and z coordinates because y is fixed to 0 in the shader.
waterVertices :: V.Vector GLfloat
waterVertices = V.fromList [-1, -1, -1, 1, 1, -1, 1, -1, -1, 1, 1, 1]

data Water = Water
  { waterRawModel   :: {-# UNPACK #-} !RawModel
  , dudvMap         :: {-# UNPACK #-} !Texture
  , waterMoveFactor :: {-# UNPACK #-} !(IORef GLfloat)
  }

mkWater :: IO Water
mkWater = Water
  <$> loadVAO waterVertices 2
  <*> loadTexture "res/waterDUDV.png"
  <*> newIORef 0

update :: Water -> GLfloat -> IO GLfloat
update w secs = do
  modifyIORef' (waterMoveFactor w) ((`mod'` 1) . (+ (waveSpeed * secs)))
  readIORef $ waterMoveFactor w

drawTile :: Water -> WaterTile -> WaterProgram -> IO ()
drawTile w tile p = do
  with matrix $ \matrixPtr ->
    glUniformMatrix4fv (wModelLoc p) 1 GL_TRUE (castPtr matrixPtr)

  glBindVertexArray $ modelVao $ waterRawModel w
  glDrawArrays GL_TRIANGLES 0 $ modelVertexCount $ waterRawModel w
  glBindVertexArray 0
 where
  tileVec = Linear.V3 (tileX tile) (tileHeight tile) (tileZ tile)
  matrix =
    Linear.mkTransformationMat (Linear.identity !!* waterTileSize) tileVec
