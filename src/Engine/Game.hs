{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Engine.Game where

import Control.Exception (bracket)
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
import NeatInterpolation (text)
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
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
      cameraVec = (inverse(view) * vec4(0.0, 0.0, 0.0, 1.0)).xyz - worldPosition.xyz;
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

    uniform sampler2D myTexture;
    uniform vec3 lightColor;
    uniform float shineDamper;
    uniform float reflectivity;

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

data TexProgram = TexProgram
  { pProgram         :: {-# UNPACK #-} !GLuint
  , pTextureLoc      :: {-# UNPACK #-} !GLint
  , pModelLoc        :: {-# UNPACK #-} !GLint
  , pViewLoc         :: {-# UNPACK #-} !GLint
  , pProjLoc         :: {-# UNPACK #-} !GLint
  , pLightPosLoc     :: {-# UNPACK #-} !GLint
  , pLightColorLoc   :: {-# UNPACK #-} !GLint
  , pShineDamperLoc  :: {-# UNPACK #-} !GLint
  , pReflectivityLoc :: {-# UNPACK #-} !GLint
  , pWidth           :: {-# UNPACK #-} !Int
  , pHeight          :: {-# UNPACK #-} !Int
  }

mkProgram :: Int -> Int -> ByteString -> ByteString -> IO TexProgram
mkProgram pWidth pHeight vertexShaderSrc0 fragmentShaderSrc0 = do
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
  pLightPosLoc <- withCString "lightPosition" $ \name ->
    glGetUniformLocation pProgram name
  pLightColorLoc <- withCString "lightColor" $ \name ->
    glGetUniformLocation pProgram name
  pShineDamperLoc <- withCString "shineDamper" $ \name ->
    glGetUniformLocation pProgram name
  pReflectivityLoc <- withCString "reflectivity" $ \name ->
    glGetUniformLocation pProgram name
  return TexProgram{..}
 where
  loadVertexShader = loadShader GL_VERTEX_SHADER vertexShaderSrc0
  loadFragmentShader = loadShader GL_FRAGMENT_SHADER fragmentShaderSrc0

programSetTexture :: TexProgram -> Texture -> IO ()
programSetTexture p tex = do
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D $ textureID tex
  glUniform1i (pTextureLoc p) 0
  glUniform1f (pShineDamperLoc p) (textureShineDamper tex)
  glUniform1f (pReflectivityLoc p) (textureReflectivity tex)

programSetUniforms :: TexProgram -> Light -> Linear.M44 GLfloat -> IO ()
programSetUniforms p light view = do
  with view $ \matrixPtr ->
    glUniformMatrix4fv (pViewLoc p) 1 GL_TRUE (castPtr matrixPtr)
  with (perspectiveMat (pWidth p) (pHeight p)) $ \matrixPtr ->
    glUniformMatrix4fv (pProjLoc p) 1 GL_TRUE (castPtr matrixPtr)
  glUniform3f (pLightPosLoc p) posX posY posZ
  glUniform3f (pLightColorLoc p) cX cY cZ
 where
  Light { lightPos = Linear.V3 posX posY posZ
        , lightColor = Linear.V3 cX cY cZ } = light

programSetModel :: TexProgram -> Linear.M44 GLfloat -> IO ()
programSetModel p model = with model $ \matrixPtr ->
  glUniformMatrix4fv (pModelLoc p) 1 GL_TRUE (castPtr matrixPtr)

data Game = Game
  { gameEntities :: {-# UNPACK #-} !(IOVec Entity)
  , gameProgram  :: {-# UNPACK #-} !TexProgram
  , gameView     :: {-# UNPACK #-} !(Linear.M44 GLfloat)
  , gameLight    :: {-# UNPACK #-} !Light
  , gameTexture  :: {-# UNPACK #-} !Texture
  , gameRawModel :: {-# UNPACK #-} !RawModel
  }

init :: Int -> Int -> IO Game
init w h = do
  texture <- (\t -> t { textureReflectivity = 1 })
         <$> loadTexture "res/stallTexture.png"
  let
    initEntities =
      [ Entity
        (Linear.V3 0 0 0)
        (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) 0)
        0.5
        texture
      , Entity
        (Linear.V3 5 0 0)
        (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) 0)
        0.2
        texture
      ]
  game <- Game
    <$> V.unsafeThaw (V.fromList initEntities)
    <*> mkProgram w h vertexShaderSrc fragmentShaderSrc
    <*> pure camera
    <*> pure light
    <*> pure texture
    <*> loadObj "res/dragon.obj"
  V.freeze (gameEntities game) >>= print
  return game
 where
  camera = Linear.lookAt (Linear.V3 0 0 15) (Linear.V3 0 0 0) (Linear.V3 0 1 0)
  light = Light (Linear.V3 0 0 10) (Linear.V3 1 1 1)

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
  programSetUniforms (gameProgram g) (gameLight g)(gameView g)
  programSetTexture (gameProgram g) (gameTexture g)

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
