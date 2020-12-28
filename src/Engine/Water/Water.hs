{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Engine.Water.Water
  ( Program
  , Water (dudvMap, normalMap)
  , mkProgram
  , mkWater
  , use
  , setUniforms
  , setLights
  , setTextures
  , unbind
  , update
  , drawTile
  ) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Fixed (mod')
import Data.IORef
import Engine.Types
import Engine.Utils
  (linkShaders, loadShader, loadTexture, loadVAO, shaderHeader)
import Engine.Water.FrameBuffers (FrameBuffers (..))
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

vertexShaderSrc :: Int -> ByteString
vertexShaderSrc maxLights = BS.pack (shaderHeader maxLights) <> BS.pack
  [QQ.r|
    in vec2 position;
    out vec4 clipSpace;
    out vec2 texCoord;
    out vec3 lightVec[NUM_LIGHTS];
    out vec3 cameraVec;

    uniform mat4 model;
    uniform mat4 view;
    uniform mat4 projection;
    uniform vec3 lightPosition[NUM_LIGHTS];
    uniform vec3 cameraPosition;

    const float tiling = 6.0;

    void main() {
      vec4 worldPosition = model * vec4(position.x, 0.0, position.y, 1.0);
      clipSpace = projection * view * worldPosition;
      gl_Position = clipSpace;
      texCoord = vec2(position.x / 2.0 + 0.5, position.y / 2.0 + 0.5) * tiling;
      for (int i = 0; i < NUM_LIGHTS; i++) {
        lightVec[i] = worldPosition.xyz - lightPosition[i];
      }
      cameraVec = cameraPosition - worldPosition.xyz;
    }
  |]

fragmentShaderSrc :: Int -> ByteString
fragmentShaderSrc maxLights = BS.pack (shaderHeader maxLights) <> BS.pack
  [QQ.r|
    in vec4 clipSpace;
    in vec2 texCoord;
    in vec3 lightVec[NUM_LIGHTS];
    in vec3 cameraVec;
    out vec4 color;

    uniform sampler2D reflectionTexture;
    uniform sampler2D refractionTexture;
    uniform sampler2D dudvMap;
    uniform sampler2D normalMap;
    uniform sampler2D depthMap;
    uniform vec3 lightColor[NUM_LIGHTS];
    uniform vec3 attenuation[NUM_LIGHTS];

    uniform float moveFactor;

    const float waveStrength = 0.02;
    const float shineDamper = 20.0;
    const float reflectivity = 0.6;

    const float near = 0.1;
    const float far = 1000.0;

    void main() {
      vec2 ndc = (clipSpace.xy / clipSpace.w) / 2.0 + 0.5;
      vec2 refractTexCoords = vec2(ndc.x, ndc.y);
      vec2 reflectTexCoords = vec2(ndc.x, -ndc.y);

      float depth = texture(depthMap, refractTexCoords).r;
      float floorDist = 2.0 * near * far / (far + near - (2.0 * depth - 1.0) * (far - near));
      depth = gl_FragCoord.z;
      float waterDist = 2.0 * near * far / (far + near - (2.0 * depth - 1.0) * (far - near));

      float waterDepth = floorDist - waterDist;

      vec2 distortedCoords = texture(dudvMap, vec2(texCoord.x + moveFactor, texCoord.y)).rg * 0.1;
      distortedCoords = texCoord + vec2(distortedCoords.x, distortedCoords.y + moveFactor);
      vec2 totalDistortion = (texture(dudvMap, distortedCoords).rg * 2.0 - 1.0) * waveStrength * clamp(waterDepth / 20.0, 0.0, 1.0);

      refractTexCoords += totalDistortion;
      refractTexCoords = clamp(refractTexCoords, 0.001, 0.999);

      reflectTexCoords += totalDistortion;
      reflectTexCoords.x = clamp(reflectTexCoords.x, 0.001, 0.999);
      reflectTexCoords.y = clamp(reflectTexCoords.y, -0.999, -0.001);

      vec4 reflectColor = texture(reflectionTexture, reflectTexCoords);
      vec4 refractColor = texture(refractionTexture, refractTexCoords);

      vec4 normalMapColor = texture(normalMap, distortedCoords);
      vec3 normal = vec3(normalMapColor.r * 2.0 - 1.0, normalMapColor.b * 3.0, normalMapColor.g * 2.0 - 1.0);
      vec3 unitNormal = normalize(normal);

      vec3 unitCameraVec = normalize(cameraVec);
      float refractiveFactor = dot(unitCameraVec, normal);

      vec3 totalSpecular = vec3(0.0);
      for (int i = 0; i < NUM_LIGHTS; i++) {
        float distance = length(lightVec[i]);
        float attenuationFactor = attenuation[i].x +
                                  attenuation[i].y * distance +
                                  attenuation[i].z * distance * distance;
        vec3 reflectedLightVec = reflect(normalize(lightVec[i]), unitNormal);
        float specularFactor = max(dot(reflectedLightVec, unitCameraVec), 0.0);
        float dampedFactor = pow(specularFactor, shineDamper);
        totalSpecular += dampedFactor * reflectivity * lightColor[i] * clamp(waterDepth / 5.0, 0.0, 1.0) / attenuationFactor;
      }

      color = mix(reflectColor, refractColor, refractiveFactor);
      color = mix(color, vec4(0.0, 0.3, 0.5, 1.0), 0.2) + vec4(totalSpecular, 0.0);
      color.a = clamp(waterDepth / 3.0, 0.0, 1.0);
    }
  |]

data Program = Program
  { program              :: {-# UNPACK #-} !GLuint
  , modelLoc             :: {-# UNPACK #-} !GLint
  , viewLoc              :: {-# UNPACK #-} !GLint
  , projLoc              :: {-# UNPACK #-} !GLint
  , lightPositionLoc     :: {-# UNPACK #-} !(V.Vector GLint)
  , cameraPositionLoc    :: {-# UNPACK #-} !GLint
  , reflectionTextureLoc :: {-# UNPACK #-} !GLint
  , refractionTextureLoc :: {-# UNPACK #-} !GLint
  , dudvMapLoc           :: {-# UNPACK #-} !GLint
  , normalMapLoc         :: {-# UNPACK #-} !GLint
  , depthMapLoc          :: {-# UNPACK #-} !GLint
  , lightColorLoc        :: {-# UNPACK #-} !(V.Vector GLint)
  , lightAttenuationLoc  :: {-# UNPACK #-} !(V.Vector GLint)
  , moveFactorLoc        :: {-# UNPACK #-} !GLint
  }

mkProgram :: Int -> IO Program
mkProgram maxLights = do
  program <-
    bracket loadVertexShader glDeleteShader $ \vertexShader ->
    bracket loadFragmentShader glDeleteShader $ \fragmentShader ->
      linkShaders [vertexShader, fragmentShader]
  modelLoc <- withCString "model" $ glGetUniformLocation program
  viewLoc <- withCString "view" $ glGetUniformLocation program
  projLoc <- withCString "projection" $ glGetUniformLocation program
  lightPositionLoc <- V.forM lightIdxs $ \i ->
    withCString ("lightPosition[" ++ show i ++ "]") $
      glGetUniformLocation program
  cameraPositionLoc <- withCString "cameraPosition" $
    glGetUniformLocation program
  reflectionTextureLoc <- withCString "reflectionTexture" $
    glGetUniformLocation program
  refractionTextureLoc <- withCString "refractionTexture" $
    glGetUniformLocation program
  dudvMapLoc <- withCString "dudvMap" $ glGetUniformLocation program
  normalMapLoc <- withCString "normalMap" $ glGetUniformLocation program
  depthMapLoc <- withCString "depthMap" $ glGetUniformLocation program
  lightColorLoc <- V.forM lightIdxs $ \i ->
    withCString ("lightColor[" ++ show i ++ "]") $
      glGetUniformLocation program
  lightAttenuationLoc <- V.forM lightIdxs $ \i ->
    withCString ("attenuation[" ++ show i ++ "]") $
      glGetUniformLocation program
  moveFactorLoc <- withCString "moveFactor" $ glGetUniformLocation program
  return Program{..}
 where
  lightIdxs = V.enumFromN 0 maxLights :: V.Vector Int
  loadVertexShader = loadShader GL_VERTEX_SHADER (vertexShaderSrc maxLights)
  loadFragmentShader =
    loadShader GL_FRAGMENT_SHADER (fragmentShaderSrc maxLights)

use :: Program -> IO ()
use = glUseProgram . program

setUniforms :: Program
            -> Linear.M44 GLfloat
            -> Linear.M44 GLfloat
            -> Linear.V3 GLfloat
            -> GLfloat
            -> IO ()
setUniforms p view proj cameraPosition moveFactor = do
  glUniform3f (cameraPositionLoc p) posx posy posz
  with view $ glUniformMatrix4fv (viewLoc p) 1 GL_TRUE . castPtr
  with proj $ glUniformMatrix4fv (projLoc p) 1 GL_TRUE . castPtr
  glUniform1f (moveFactorLoc p) moveFactor
 where Linear.V3 posx posy posz = cameraPosition

setLights :: Program -> V.Vector Light -> IO ()
setLights p lights = forM_ [0..V.length lights - 1] $ \i -> setLightUniforms
  (lights V.! i)
  (lightPositionLoc p V.! i)
  (lightColorLoc p V.! i)
  (lightAttenuationLoc p V.! i)

setTextures :: Program -> FrameBuffers -> Water -> IO ()
setTextures p bufs w = do
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D $ reflectionTexture bufs
  glUniform1i (reflectionTextureLoc p) 0

  glActiveTexture GL_TEXTURE1
  glBindTexture GL_TEXTURE_2D $ refractionTexture bufs
  glUniform1i (refractionTextureLoc p) 1

  glActiveTexture GL_TEXTURE2
  glBindTexture GL_TEXTURE_2D $ textureID $ dudvMap w
  glUniform1i (dudvMapLoc p) 2

  glActiveTexture GL_TEXTURE3
  glBindTexture GL_TEXTURE_2D $ textureID $ normalMap w
  glUniform1i (normalMapLoc p) 3

  glActiveTexture GL_TEXTURE4
  glBindTexture GL_TEXTURE_2D $ refractionDepthTexture bufs
  glUniform1i (depthMapLoc p) 4

  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

unbind :: IO ()
unbind = do
  glDisable GL_BLEND
  glDisableVertexAttribArray 0
  glBindVertexArray 0

-- | Only the x and z coordinates because y is fixed to 0 in the shader.
waterVertices :: V.Vector GLfloat
waterVertices = V.fromList [-1, -1, -1, 1, 1, -1, 1, -1, -1, 1, 1, 1]

data Water = Water
  { waterRawModel   :: {-# UNPACK #-} !RawModel
  , dudvMap         :: {-# UNPACK #-} !Texture
  , normalMap       :: {-# UNPACK #-} !Texture
  , waterMoveFactor :: {-# UNPACK #-} !(IORef GLfloat)
  }

mkWater :: IO Water
mkWater = Water
  <$> loadVAO waterVertices 2
  <*> loadTexture "res/waterDUDV.png"
  <*> loadTexture "res/normalMap.png"
  <*> newIORef 0

update :: Water -> GLfloat -> IO GLfloat
update w secs = do
  modifyIORef' (waterMoveFactor w) ((`mod'` 1) . (+ (waveSpeed * secs)))
  readIORef $ waterMoveFactor w

drawTile :: Water -> WaterTile -> Program -> IO ()
drawTile w tile p = do
  with matrix $ glUniformMatrix4fv (modelLoc p) 1 GL_TRUE . castPtr

  glBindVertexArray $ modelVao $ waterRawModel w
  glDrawArrays GL_TRIANGLES 0 $ modelVertexCount $ waterRawModel w
  glBindVertexArray 0
 where
  tileVec = Linear.V3 (tileX tile) (tileHeight tile) (tileZ tile)
  matrix =
    Linear.mkTransformationMat (Linear.identity !!* waterTileSize) tileVec
