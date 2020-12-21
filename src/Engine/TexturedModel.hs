{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Engine.TexturedModel
  ( Program
  , mkProgram
  , use
  , setUniforms
  , setTexture
  , setModel
  , setOffset
  ) where

import Prelude hiding (init)
import Control.Exception (bracket)
import Control.Monad (forM, forM_)
import Data.ByteString (ByteString)
import Data.List (zip4)
import Engine.Types (Light, Texture (..), padLights, setLightUniforms)
import Engine.Utils (linkShaders, loadShader, shaderHeader)
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Data.ByteString.Char8 as BS
import qualified Linear
import qualified Text.RawString.QQ as QQ

vertexShaderSrc :: Int -> ByteString
vertexShaderSrc maxLights = BS.pack (shaderHeader maxLights) <> BS.pack
  [QQ.r|
    in vec3 position;
    in vec2 texCoord;
    in vec3 normal;

    out vec2 v_texCoord;
    out vec3 surfaceNormal;
    out vec3 lightVec[NUM_LIGHTS];
    out vec3 cameraVec;
    out float visibility;

    uniform mat4 model;      // Transformation of the model
    uniform mat4 view;       // Transformation of the camera
    uniform mat4 projection; // Clipping coordinates outside FOV
    uniform vec3 lightPosition[NUM_LIGHTS];
    uniform float useFakeLighting;
    uniform float numberOfRows;
    uniform vec2 offset;
    uniform vec4 clipPlane;

    const float density = 0.007;
    const float gradient = 1.5;

    void main() {
      vec4 worldPosition = model * vec4(position, 1.0);
      gl_ClipDistance[0] = dot(worldPosition, clipPlane);
      vec4 positionRelativeToCam = view * worldPosition;
      gl_Position = projection * positionRelativeToCam;
      v_texCoord = texCoord / numberOfRows + offset;

      vec3 actualNormal = normal;
      if (useFakeLighting > 0.5) {
        actualNormal = vec3(0.0, 1.0, 0.0);
      }

      surfaceNormal = (model * vec4(actualNormal, 0.0)).xyz;
      for (int i = 0; i < NUM_LIGHTS; i++) {
        lightVec[i] = lightPosition[i] - worldPosition.xyz;
      }
      cameraVec = (inverse(view) * vec4(0.0, 0.0, 0.0, 1.0)).xyz - worldPosition.xyz;

      float distance = length(positionRelativeToCam.xyz);
      visibility = clamp(exp(-pow(distance * density, gradient)), 0.0, 1.0);
    }
  |]

fragmentShaderSrc :: Int -> ByteString
fragmentShaderSrc maxLights = BS.pack (shaderHeader maxLights) <> BS.pack
  [QQ.r|
    in vec2 v_texCoord;
    in vec3 surfaceNormal;
    in vec3 lightVec[NUM_LIGHTS];
    in vec3 cameraVec;
    in float visibility;

    uniform sampler2D myTexture;
    uniform vec3 lightColor[NUM_LIGHTS];
    uniform vec3 attenuation[NUM_LIGHTS];
    uniform float shineDamper;
    uniform float reflectivity;
    uniform vec3 skyColor;

    out vec4 color;

    void main() {
      vec3 unitNormal = normalize(surfaceNormal);
      vec3 unitCameraVec = normalize(cameraVec);

      vec3 totalDiffuse = vec3(0.0);
      vec3 totalSpecular = vec3(0.0);
      for (int i = 0; i < NUM_LIGHTS; i++) {
        float distance = length(lightVec[i]);
        float attenuationFactor = attenuation[i].x +
                                  attenuation[i].y * distance +
                                  attenuation[i].z * distance * distance;
        vec3 unitLightVec = normalize(lightVec[i]);
        float brightness = max(dot(unitNormal, unitLightVec), 0.0);
        vec3 reflectedLightVec = reflect(-unitLightVec, unitNormal);
        float specularFactor = max(dot(reflectedLightVec, unitCameraVec), 0.0);
        float dampedFactor = pow(specularFactor, shineDamper);

        totalDiffuse += brightness * lightColor[i] / attenuationFactor;
        totalSpecular += dampedFactor * reflectivity * lightColor[i] / attenuationFactor;
      }
      totalDiffuse = max(totalDiffuse, 0.2);

      vec4 textureColor = texture(myTexture, v_texCoord);
      if (textureColor.a < 0.5) {
        discard;
      }

      color = vec4(totalDiffuse, 1.0) * textureColor + vec4(totalSpecular, 1.0);
      color = mix(vec4(skyColor, 1.0), color, visibility);
    }
  |]

data Program = Program
  { pProgram             :: {-# UNPACK #-} !GLuint
  , pTextureLoc          :: {-# UNPACK #-} !GLint
  , pModelLoc            :: {-# UNPACK #-} !GLint
  , pViewLoc             :: {-# UNPACK #-} !GLint
  , pProjLoc             :: {-# UNPACK #-} !GLint
  , pLightPosLoc         :: ![GLint]
  , pLightColorLoc       :: ![GLint]
  , pLightAttenuationLoc :: ![GLint]
  , pShineDamperLoc      :: {-# UNPACK #-} !GLint
  , pReflectivityLoc     :: {-# UNPACK #-} !GLint
  , pFakeLightingLoc     :: {-# UNPACK #-} !GLint
  , pSkyColorLoc         :: {-# UNPACK #-} !GLint
  , pNumberOfRows        :: {-# UNPACK #-} !GLint
  , pOffset              :: {-# UNPACK #-} !GLint
  , pClipPlane           :: {-# UNPACK #-} !GLint
  }

mkProgram :: Int -> IO Program
mkProgram maxLights = do
  pProgram <-
    bracket loadVertexShader glDeleteShader $ \vertexShader ->
    bracket loadFragmentShader glDeleteShader $ \fragmentShader ->
      linkShaders [vertexShader, fragmentShader]
  pTextureLoc <- withCString "texture" $ \name ->
    glGetUniformLocation pProgram name
  pModelLoc <- withCString "model" $ \name ->
    glGetUniformLocation pProgram name
  pViewLoc <- withCString "view" $ \name ->
    glGetUniformLocation pProgram name
  pProjLoc <- withCString "projection" $ \name ->
    glGetUniformLocation pProgram name
  pLightPosLoc <- forM [0..maxLights - 1] $ \i ->
    withCString ("lightPosition[" ++ show i ++ "]") $ \name ->
      glGetUniformLocation pProgram name
  pLightColorLoc <- forM [0..maxLights - 1] $ \i ->
    withCString ("lightColor[" ++ show i ++ "]") $ \name ->
      glGetUniformLocation pProgram name
  pLightAttenuationLoc <- forM [0..maxLights - 1] $ \i ->
    withCString ("attenuation[" ++ show i ++ "]") $ \name ->
      glGetUniformLocation pProgram name
  pShineDamperLoc <- withCString "shineDamper" $ \name ->
    glGetUniformLocation pProgram name
  pReflectivityLoc <- withCString "reflectivity" $ \name ->
    glGetUniformLocation pProgram name
  pFakeLightingLoc <- withCString "useFakeLighting" $ \name ->
    glGetUniformLocation pProgram name
  pSkyColorLoc <- withCString "skyColor" $ \name ->
    glGetUniformLocation pProgram name
  pNumberOfRows <- withCString "numberOfRows" $ \name ->
    glGetUniformLocation pProgram name
  pOffset <- withCString "offset" $ \name ->
    glGetUniformLocation pProgram name
  pClipPlane <- withCString "clipPlane" $ \name ->
    glGetUniformLocation pProgram name
  return Program{..}
 where
  loadVertexShader = loadShader GL_VERTEX_SHADER $ vertexShaderSrc maxLights
  loadFragmentShader =
    loadShader GL_FRAGMENT_SHADER $ fragmentShaderSrc maxLights

use :: Program -> IO ()
use = glUseProgram . pProgram

setTexture :: Program -> Texture -> IO ()
setTexture p tex = do
  if textureTransparent tex /= 0
    then glDisable GL_CULL_FACE
    else glEnable GL_CULL_FACE >> glCullFace GL_BACK
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D $ textureID tex
  glUniform1i (pTextureLoc p) 0
  glUniform1f (pShineDamperLoc p) (textureShineDamper tex)
  glUniform1f (pReflectivityLoc p) (textureReflectivity tex)
  glUniform1f (pFakeLightingLoc p) (textureUseFakeLighting tex)
  glUniform1f (pNumberOfRows p) $ fromIntegral $ textureNumRows tex

setUniforms
  :: Program
  -> [Light]
  -> Linear.V3 GLfloat
  -> Linear.M44 GLfloat
  -> Linear.M44 GLfloat
  -> Linear.V4 GLfloat
  -> IO ()
setUniforms p lights skyColor view proj clipPlane = do
  with view $ \matrixPtr ->
    glUniformMatrix4fv (pViewLoc p) 1 GL_TRUE (castPtr matrixPtr)
  with proj $ \matrixPtr ->
    glUniformMatrix4fv (pProjLoc p) 1 GL_TRUE (castPtr matrixPtr)
  forM_ lightsWithLocs $ \(l, posLoc, colLoc, attLoc) ->
    setLightUniforms l posLoc colLoc attLoc
  glUniform3f (pSkyColorLoc p) r g b
  glUniform4f (pClipPlane p) px py pz pw
 where
  Linear.V3 r g b = skyColor
  Linear.V4 px py pz pw = clipPlane
  padded = padLights lights (pLightPosLoc p) (pLightColorLoc p)
  lightsWithLocs =
    zip4 padded (pLightPosLoc p) (pLightColorLoc p) (pLightAttenuationLoc p)

setOffset :: Program -> GLfloat -> GLfloat -> IO ()
setOffset p = glUniform2f (pOffset p)

setModel :: Program -> Linear.M44 GLfloat -> IO ()
setModel p model = with model $ \matrixPtr ->
  glUniformMatrix4fv (pModelLoc p) 1 GL_TRUE (castPtr matrixPtr)
