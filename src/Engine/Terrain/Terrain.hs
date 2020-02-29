{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Engine.Terrain.Terrain
  ( Terrain (..)
  , TerrainProgram (..)
  , mkTerrain
  , mkProgram
  , heightAt
  , setUniforms
  , draw
  ) where

import Codec.Picture
import Control.Exception (bracket)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.Fixed (mod')
import Engine.Types
  (Light, RawModel (..), Texture (..), TexturePack (..), setLightUniforms)
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
      v_texCoord = texCoord;

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

    uniform sampler2D backgroundTexture;
    uniform sampler2D rTexture;
    uniform sampler2D gTexture;
    uniform sampler2D bTexture;
    uniform sampler2D blendMap;

    uniform vec3 lightColor;
    uniform float shineDamper;
    uniform float reflectivity;
    uniform vec3 skyColor;

    out vec4 color;

    void main() {
      vec4 blendMapColor = texture(blendMap, v_texCoord);
      float backTextureAmount = 1 - (blendMapColor.r + blendMapColor.g + blendMapColor.b);
      vec2 tiledCoords = v_texCoord * 40.0;
      vec4 backgroundTextureColor = texture(backgroundTexture, tiledCoords) * backTextureAmount;
      vec4 rTextureColor = texture(rTexture, tiledCoords) * blendMapColor.r;
      vec4 gTextureColor = texture(gTexture, tiledCoords) * blendMapColor.g;
      vec4 bTextureColor = texture(bTexture, tiledCoords) * blendMapColor.b;
      vec4 totalColor = backgroundTextureColor + rTextureColor + gTextureColor + bTextureColor;

      vec3 unitNormal = normalize(surfaceNormal);
      vec3 unitLightVec = normalize(lightVec);
      float brightness = max(dot(unitNormal, unitLightVec), 0.2);
      vec3 diffuse = brightness * lightColor;

      vec3 unitCameraVec = normalize(cameraVec);
      vec3 reflectedLightVec = reflect(-unitLightVec, unitNormal);
      float specularFactor = max(dot(reflectedLightVec, unitCameraVec), 0.0);
      float dampedFactor = pow(specularFactor, shineDamper);
      vec3 finalSpecular = dampedFactor * reflectivity * lightColor;

      color = vec4(diffuse, 1.0) * totalColor + vec4(finalSpecular, 1.0);
      color = mix(vec4(skyColor, 1.0), color, visibility);
    }
  |]

terrainSize :: GLfloat
terrainSize = 800

terrainMaxHeight :: GLfloat
terrainMaxHeight = 40

terrainMaxPixelColor :: GLfloat
terrainMaxPixelColor = 256 * 256 * 256

data TerrainHeights = TerrainHeights
  { heightsData        :: {-# UNPACK #-} !(V.Vector GLfloat)
  , heightsVertexCount :: {-# UNPACK #-} !Int
  }

data Terrain = Terrain
  { terrainX        :: {-# UNPACK #-} !GLfloat
  , terrainZ        :: {-# UNPACK #-} !GLfloat
  , terrainPack     :: {-# UNPACK #-} !TexturePack
  , terrainBlendMap :: {-# UNPACK #-} !Texture
  , terrainHeights  :: {-# UNPACK #-} !TerrainHeights
  , terrainRawModel :: {-# UNPACK #-} !RawModel
  }

mkTerrain
  :: GLfloat -> GLfloat -> TexturePack -> Texture -> FilePath -> IO Terrain
mkTerrain x z p t heightMapPath = do
  Right heightMap <- fmap convertRGB8 <$> readImage heightMapPath
  let vertexCount = imageHeight heightMap
      heights     = V.fromList $ do
        j <- [0..vertexCount - 1]
        i <- [0..vertexCount - 1]
        return $ calcHeight j i heightMap
  Terrain (x * terrainSize)(z * terrainSize) p t
    (TerrainHeights heights vertexCount) <$> generateTerrain heightMap

generateTerrain :: Image PixelRGB8 -> IO RawModel
generateTerrain heightMap = V.unsafeWith buffer $ \vPtr ->
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
  terrainVertexCount = imageHeight heightMap

  buffer :: V.Vector GLfloat
  buffer = V.fromList $ do
    i <- [0..terrainVertexCount - 1]
    j <- [0..terrainVertexCount - 1]
    let texCoordX = fromIntegral j / fromIntegral (terrainVertexCount - 1)
        texCoordZ = fromIntegral i / fromIntegral (terrainVertexCount - 1)
        vertX     = texCoordX * terrainSize
        vertZ     = texCoordZ * terrainSize
    let Linear.V3 nx ny nz = calcNormal j i heightMap in
      [ vertX, calcHeight j i heightMap, vertZ
      , texCoordX, texCoordZ
      , nx, ny, nz
      ]

  indices :: V.Vector GLuint
  indices = V.fromList . fmap fromIntegral $ do
    z <- [0..terrainVertexCount - 2]
    x <- [0..terrainVertexCount - 2]
    let topLeft     = z * terrainVertexCount + x
        topRight    = topLeft + 1
        bottomLeft  = (z + 1) * terrainVertexCount + x
        bottomRight = bottomLeft + 1
    [topLeft, bottomLeft, topRight, topRight, bottomLeft, bottomRight]

calcHeight :: Int -> Int -> Image PixelRGB8 -> GLfloat
calcHeight x z image
  | x < 0 || x >= h || z < 0 || z >= h = 0
  | otherwise                          =
    ((pixelToFloat (pixelAt image x z) + v) / v) * terrainMaxHeight - 80
 where
  h = imageHeight image
  v = terrainMaxPixelColor / 2

calcNormal :: Int -> Int -> Image PixelRGB8 -> Linear.V3 GLfloat
calcNormal x z image =
  Linear.normalize $ Linear.V3 (heightL - heightR) 2 (heightD - heightU)
 where
  heightL = calcHeight (x - 1) z image
  heightR = calcHeight (x + 1) z image
  heightD = calcHeight x (z - 1) image
  heightU = calcHeight x (z + 1) image

pixelToFloat :: PixelRGB8 -> GLfloat
pixelToFloat = fromIntegral . pixelToInt

pixelToInt :: PixelRGB8 -> Int
pixelToInt (PixelRGB8 r g b)
  =   shiftL (fromIntegral b) 0
  .|. shiftL (fromIntegral g) 8
  .|. shiftL (fromIntegral r) 16

heightAt :: GLfloat -> GLfloat -> Terrain -> GLfloat
heightAt x z t
  | gridX < 0 || gridX >= lastIdx ||
    gridZ < 0 || gridZ >= lastIdx = 0
  | xCoord < 1 - zCoord = barycentric
    (Linear.V3 0 (heightAtIdx gridX gridZ t) 0)
    (Linear.V3 1 (heightAtIdx (gridX + 1) gridZ t) 0)
    (Linear.V3 0 (heightAtIdx gridX (gridZ + 1) t) 1)
    (Linear.V2 xCoord zCoord)
  | otherwise = barycentric
    (Linear.V3 1 (heightAtIdx (gridX + 1) gridZ t) 0)
    (Linear.V3 1 (heightAtIdx (gridX + 1) (gridZ + 1) t) 1)
    (Linear.V3 0 (heightAtIdx gridX (gridZ + 1) t) 1)
    (Linear.V2 xCoord zCoord)
 where
  tx = x - terrainX t
  tz = z - terrainZ t
  lastIdx = heightsVertexCount (terrainHeights t) - 1
  gridSquareSize = terrainSize / fromIntegral lastIdx
  gridX = floor (tx / gridSquareSize)
  gridZ = floor (tz / gridSquareSize)
  xCoord = (tx `mod'` gridSquareSize) / gridSquareSize
  zCoord = (tz `mod'` gridSquareSize) / gridSquareSize

heightAtIdx :: Int -> Int -> Terrain -> GLfloat
heightAtIdx x z t = heightsData h V.! ((x * heightsVertexCount h) + z)
 where h = terrainHeights t

barycentric
  :: Linear.V3 GLfloat
  -> Linear.V3 GLfloat
  -> Linear.V3 GLfloat
  -> Linear.V2 GLfloat
  -> GLfloat
barycentric (Linear.V3 p1x p1y p1z)
            (Linear.V3 p2x p2y p2z)
            (Linear.V3 p3x p3y p3z)
            (Linear.V2 posx posy) = l1 * p1y + l2 * p2y + l3 * p3y
 where
  det = (p2z - p3z) * (p1x - p3x) + (p3x - p2x) * (p1z - p3z)
  l1 = ((p2z - p3z) * (posx - p3x) + (p3x - p2x) * (posy - p3z)) / det
  l2 = ((p3z - p1z) * (posx - p3x) + (p1x - p3x) * (posy - p3z)) / det
  l3 = 1.0 - l1 - l2

data TerrainProgram = TerrainProgram
  { tProgram         :: {-# UNPACK #-} !GLuint
  , tBackTextureLoc  :: {-# UNPACK #-} !GLint
  , tRTextureLoc     :: {-# UNPACK #-} !GLint
  , tGTextureLoc     :: {-# UNPACK #-} !GLint
  , tBTextureLoc     :: {-# UNPACK #-} !GLint
  , tBlendMapLoc     :: {-# UNPACK #-} !GLint
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

  tBackTextureLoc <- withCString "backgroundTexture" $ \name ->
    glGetUniformLocation tProgram name
  tRTextureLoc <- withCString "rTexture" $ \name ->
    glGetUniformLocation tProgram name
  tGTextureLoc <- withCString "gTexture" $ \name ->
    glGetUniformLocation tProgram name
  tBTextureLoc <- withCString "bTexture" $ \name ->
    glGetUniformLocation tProgram name
  tBlendMapLoc <- withCString "blendMap" $ \name ->
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
  :: Terrain
  -> TerrainProgram
  -> Light
  -> Linear.V3 GLfloat
  -> Linear.M44 GLfloat
  -> Linear.M44 GLfloat
  -> IO ()
setUniforms t p light skyColor view proj = do
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D $ textureID packBackground
  glUniform1i (tBackTextureLoc p) 0

  glActiveTexture GL_TEXTURE1
  glBindTexture GL_TEXTURE_2D $ textureID packR
  glUniform1i (tRTextureLoc p) 1

  glActiveTexture GL_TEXTURE2
  glBindTexture GL_TEXTURE_2D $ textureID packG
  glUniform1i (tGTextureLoc p) 2

  glActiveTexture GL_TEXTURE3
  glBindTexture GL_TEXTURE_2D $ textureID packB
  glUniform1i (tBTextureLoc p) 3

  glActiveTexture GL_TEXTURE4
  glBindTexture GL_TEXTURE_2D $ textureID $ terrainBlendMap t
  glUniform1i (tBlendMapLoc p) 4

  glUniform1f (tShineDamperLoc p) $ textureShineDamper packBackground
  glUniform1f (tReflectivityLoc p) $ textureReflectivity packBackground

  with view $ \matrixPtr ->
    glUniformMatrix4fv (tViewLoc p) 1 GL_TRUE (castPtr matrixPtr)
  with proj $ \matrixPtr ->
    glUniformMatrix4fv (tProjLoc p) 1 GL_TRUE (castPtr matrixPtr)
  setLightUniforms light (tLightPosLoc p) (tLightColorLoc p)
  glUniform3f (tSkyColorLoc p) r g b
 where
  TexturePack{..} = terrainPack t
  Linear.V3 r g b = skyColor

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
