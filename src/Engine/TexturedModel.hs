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
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Engine.Types (Light, Texture (..), setLightUniforms)
import Engine.Utils (linkShaders, loadShader, shaderHeader)
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as V
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
  { program             :: {-# UNPACK #-} !GLuint
  , textureLoc          :: {-# UNPACK #-} !GLint
  , modelLoc            :: {-# UNPACK #-} !GLint
  , viewLoc             :: {-# UNPACK #-} !GLint
  , projLoc             :: {-# UNPACK #-} !GLint
  , lightPosLoc         :: {-# UNPACK #-} !(V.Vector GLint)
  , lightColorLoc       :: {-# UNPACK #-} !(V.Vector GLint)
  , lightAttenuationLoc :: {-# UNPACK #-} !(V.Vector GLint)
  , shineDamperLoc      :: {-# UNPACK #-} !GLint
  , reflectivityLoc     :: {-# UNPACK #-} !GLint
  , fakeLightingLoc     :: {-# UNPACK #-} !GLint
  , skyColorLoc         :: {-# UNPACK #-} !GLint
  , numberOfRowsLoc     :: {-# UNPACK #-} !GLint
  , offsetLoc           :: {-# UNPACK #-} !GLint
  , clipPlaneLoc        :: {-# UNPACK #-} !GLint
  }

mkProgram :: Int -> IO Program
mkProgram maxLights = do
  program <-
    bracket loadVertexShader glDeleteShader $ \vertexShader ->
    bracket loadFragmentShader glDeleteShader $ \fragmentShader ->
      linkShaders [vertexShader, fragmentShader]
  textureLoc <- withCString "texture" $ glGetUniformLocation program
  modelLoc <- withCString "model" $ glGetUniformLocation program
  viewLoc <- withCString "view" $ glGetUniformLocation program
  projLoc <- withCString "projection" $ glGetUniformLocation program
  lightPosLoc <- V.forM lightIdxs $ \i ->
    withCString ("lightPosition[" ++ show i ++ "]") $
      glGetUniformLocation program
  lightColorLoc <- V.forM lightIdxs $ \i ->
    withCString ("lightColor[" ++ show i ++ "]") $
      glGetUniformLocation program
  lightAttenuationLoc <- V.forM lightIdxs $ \i ->
    withCString ("attenuation[" ++ show i ++ "]") $
      glGetUniformLocation program
  shineDamperLoc <- withCString "shineDamper" $ glGetUniformLocation program
  reflectivityLoc <- withCString "reflectivity" $ glGetUniformLocation program
  fakeLightingLoc <- withCString "useFakeLighting" $
    glGetUniformLocation program
  skyColorLoc <- withCString "skyColor" $ glGetUniformLocation program
  numberOfRowsLoc <- withCString "numberOfRows" $ glGetUniformLocation program
  offsetLoc <- withCString "offset" $ glGetUniformLocation program
  clipPlaneLoc <- withCString "clipPlane" $ glGetUniformLocation program
  return Program{..}
 where
  lightIdxs = V.enumFromN 0 maxLights :: V.Vector Int
  loadVertexShader = loadShader GL_VERTEX_SHADER $ vertexShaderSrc maxLights
  loadFragmentShader =
    loadShader GL_FRAGMENT_SHADER $ fragmentShaderSrc maxLights

use :: Program -> IO ()
use = glUseProgram . program

setTexture :: Program -> Texture -> IO ()
setTexture p tex = do
  if textureTransparent tex /= 0
    then glDisable GL_CULL_FACE
    else glEnable GL_CULL_FACE >> glCullFace GL_BACK
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D $ textureID tex
  glUniform1i (textureLoc p) 0
  glUniform1f (shineDamperLoc p) (textureShineDamper tex)
  glUniform1f (reflectivityLoc p) (textureReflectivity tex)
  glUniform1f (fakeLightingLoc p) (textureUseFakeLighting tex)
  glUniform1f (numberOfRowsLoc p) $ fromIntegral $ textureNumRows tex

setUniforms
  :: Program
  -> V.Vector Light
  -> Linear.V3 GLfloat
  -> Linear.M44 GLfloat
  -> Linear.M44 GLfloat
  -> Linear.V4 GLfloat
  -> IO ()
setUniforms p lights skyColor view proj clipPlane = do
  with view $ glUniformMatrix4fv (viewLoc p) 1 GL_TRUE . castPtr
  with proj $ glUniformMatrix4fv (projLoc p) 1 GL_TRUE . castPtr
  for_ [0..V.length lights - 1] $ \i -> setLightUniforms
    (lights V.! i)
    (lightPosLoc p V.! i)
    (lightColorLoc p V.! i)
    (lightAttenuationLoc p V.! i)
  glUniform3f (skyColorLoc p) r g b
  glUniform4f (clipPlaneLoc p) px py pz pw
 where
  Linear.V3 r g b = skyColor
  Linear.V4 px py pz pw = clipPlane

setOffset :: Program -> GLfloat -> GLfloat -> IO ()
setOffset p = glUniform2f (offsetLoc p)

setModel :: Program -> Linear.M44 GLfloat -> IO ()
setModel p model = with model $
  glUniformMatrix4fv (modelLoc p) 1 GL_TRUE . castPtr
