{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Engine.Game where

import Control.Exception (bracket)
import Control.Lens ((^.))
import Control.Monad (forM, forM_)
import Control.Monad.Primitive (PrimState)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.Foldable (foldlM, traverse_)
import Data.List (zip4)
import Engine.MousePicker (calculateMouseRay)
import Engine.Types
import Engine.Utils
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Graphics.GL.Compatibility45
import Graphics.GL.Types
import Linear ((!!*))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import qualified Engine.Skybox as Skybox
import qualified Engine.Terrain.Terrain as Terrain
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear
import qualified Text.RawString.QQ as QQ

vertexShaderSrc :: ByteString
vertexShaderSrc = BS.pack
  [QQ.r|
    #version 330 core
    #define NUM_LIGHTS 4
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

    const float density = 0.007;
    const float gradient = 1.5;

    void main() {
      vec4 worldPosition = model * vec4(position, 1.0);
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

fragmentShaderSrc :: ByteString
fragmentShaderSrc = BS.pack
  [QQ.r|
    #version 330 core
    #define NUM_LIGHTS 4
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

maxLights :: Int
maxLights = 4

type IOVec a = V.MVector (PrimState IO) a

data TexProgram = TexProgram
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
  }

mkProgram :: ByteString -> ByteString -> IO TexProgram
mkProgram vertexShaderSrc0 fragmentShaderSrc0 = do
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
  return TexProgram{..}
 where
  loadVertexShader = loadShader GL_VERTEX_SHADER vertexShaderSrc0
  loadFragmentShader = loadShader GL_FRAGMENT_SHADER fragmentShaderSrc0

programSetTexture :: TexProgram -> Texture -> IO ()
programSetTexture p tex = do
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

programSetUniforms
  :: TexProgram
  -> [Light]
  -> Linear.V3 GLfloat
  -> Linear.M44 GLfloat
  -> Linear.M44 GLfloat
  -> IO ()
programSetUniforms p lights skyColor view proj = do
  with view $ \matrixPtr ->
    glUniformMatrix4fv (pViewLoc p) 1 GL_TRUE (castPtr matrixPtr)
  with proj $ \matrixPtr ->
    glUniformMatrix4fv (pProjLoc p) 1 GL_TRUE (castPtr matrixPtr)
  forM_ lightsWithLocs $ \(l, posLoc, colLoc, attLoc) ->
    setLightUniforms l posLoc colLoc attLoc
  glUniform3f (pSkyColorLoc p) r g b
 where
  Linear.V3 r g b = skyColor
  padded = padLights lights (pLightPosLoc p) (pLightColorLoc p)
  lightsWithLocs =
    zip4 padded (pLightPosLoc p) (pLightColorLoc p) (pLightAttenuationLoc p)

programSetOffset :: TexProgram -> GLfloat -> GLfloat -> IO ()
programSetOffset p = glUniform2f (pOffset p)

programSetModel :: TexProgram -> Linear.M44 GLfloat -> IO ()
programSetModel p model = with model $ \matrixPtr ->
  glUniformMatrix4fv (pModelLoc p) 1 GL_TRUE (castPtr matrixPtr)

data Game = Game
  { gameWidth          :: {-# UNPACK #-} !GLfloat
  , gameHeight         :: {-# UNPACK #-} !GLfloat
  , gameEntities       :: {-# UNPACK #-} !(IOVec Entity)
  , gameGrasses        :: {-# UNPACK #-} !(IOVec Entity)
  , gameFerns          :: {-# UNPACK #-} !(IOVec Entity)
  , gameLamps          :: {-# UNPACK #-} !(IOVec Entity)
  , gameProgram        :: {-# UNPACK #-} !TexProgram
  , gameCamera         :: {-# UNPACK #-} !Camera
  , gameProj           :: {-# UNPACK #-} !(Linear.M44 GLfloat)
  , gameLights         :: ![Light]
  , gameLines          :: ![(Linear.V3 GLfloat, Linear.V3 GLfloat)]
  , gameTexture        :: {-# UNPACK #-} !Texture
  , gameRawModel       :: {-# UNPACK #-} !RawModel
  , gameTerrainProgram :: {-# UNPACK #-} !Terrain.TerrainProgram
  , gameTerrain1       :: {-# UNPACK #-} !Terrain.Terrain
  , gameTerrain2       :: {-# UNPACK #-} !Terrain.Terrain
  , gameSkyboxProgram  :: {-# UNPACK #-} !Skybox.SkyboxProgram
  , gameSkybox         :: {-# UNPACK #-} !Skybox.Skybox
  , gameSkyColor       :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  }

init :: Int -> Int -> IO Game
init w h = do
  texture <- (\t -> t { textureReflectivity = 1 })
         <$> loadTexture "res/stallTexture.png"
  model <- loadObj "res/dragon.obj"
  grassTexture <- (\t -> t { textureTransparent     = 1
                           , textureUseFakeLighting = 1 })
              <$> loadTexture "res/grassTexture.png"
  grassModel <- loadObj "res/grassModel.obj"
  fernTexture <- (\t -> t { textureNumRows         = 2
                          , textureTransparent     = 1
                          , textureUseFakeLighting = 1 })
             <$> loadTexture "res/fern.png"
  fernModel <- loadObj "res/fern.obj"
  lampTexture <- (\t -> t { textureUseFakeLighting = 1 })
             <$> loadTexture "res/lamp.png"
  lampModel <- loadObj "res/lamp.obj"
  let
    initEntities =
      [ Entity
        (Linear.V3 10 0 10)
        (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) 0)
        0.5
        texture
        model
        0
      , Entity
        (Linear.V3 15 0 10)
        (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) 0)
        0.2
        texture
        model
        0
      ]
    grassEntities =
      [ Entity
        (Linear.V3 20 0 10)
        (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) 0)
        1.0
        grassTexture
        grassModel
        0
      ]
    fernEntities =
      [ Entity
        (Linear.V3 20 0 10)
        (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) 0)
        1.0
        fernTexture
        fernModel
        0
      , Entity
        (Linear.V3 30 0 10)
        (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) 0)
        1.0
        fernTexture
        fernModel
        2
      , Entity
        (Linear.V3 40 0 10)
        (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) 0)
        1.0
        fernTexture
        fernModel
        1
      ]
    lampEntities =
      [ Entity
        (Linear.V3 20 0 20)
        (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) 0)
        1.0
        lampTexture
        lampModel
        0
      , Entity
        (Linear.V3 20 0 100)
        (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) 0)
        1.0
        lampTexture
        lampModel
        0
      , Entity
        (Linear.V3 100 0 20)
        (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) 0)
        1.0
        lampTexture
        lampModel
        0
      ]
  pack <- loadTexturePack
    "res/grass.png" "res/mud.png" "res/grassFlowers.png" "res/path.png"
  blendMap <- loadTexture "res/blendMap.png"
  Game
    <$> pure (fromIntegral w)
    <*> pure (fromIntegral h)
    <*> V.unsafeThaw (V.fromList initEntities)
    <*> V.unsafeThaw (V.fromList grassEntities)
    <*> V.unsafeThaw (V.fromList fernEntities)
    <*> V.unsafeThaw (V.fromList lampEntities)
    <*> mkProgram vertexShaderSrc fragmentShaderSrc
    <*> pure camera
    <*> pure proj
    <*> pure [light1, light2, light3, light4]
    <*> pure []
    <*> pure texture
    <*> pure model
    <*> Terrain.mkProgram maxLights
    <*> Terrain.load 0 0 pack blendMap "res/heightmap.png"
    <*> Terrain.load 1 0 pack blendMap "res/heightmap.png"
    <*> Skybox.mkProgram
    <*> Skybox.load
      [ "res/skybox/right.jpg"
      , "res/skybox/left.jpg"
      , "res/skybox/top.jpg"
      , "res/skybox/bottom.jpg"
      , "res/skybox/front.jpg"
      , "res/skybox/back.jpg"
      ]
    <*> pure (Linear.V3 0.5 0.5 0.5)
 where
  camera = Camera (Linear.V3 10 2 30) (Linear.V3 0 0 (-1)) (Linear.V3 0 1 0)
  proj = perspectiveMat w h
  light1 =
    Light (Linear.V3 0 1000 (-7000)) (Linear.V3 0.4 0.4 0.4) (Linear.V3 1 0 0)
  light2 =
    Light (Linear.V3 20 17 20) (Linear.V3 2 0 0) (Linear.V3 1 0.01 0.002)
  light3 =
    Light (Linear.V3 20 17 100) (Linear.V3 0 2 2) (Linear.V3 1 0.01 0.002)
  light4 =
    Light (Linear.V3 100 17 20) (Linear.V3 2 2 0) (Linear.V3 1 0.01 0.002)

update :: S.Set GLFW.Key -> MouseInfo -> GLfloat -> Game -> IO Game
update keys mouseInfo dt g0 =
  foldlM update' g0' [0..VM.length (gameEntities g0') - 1]
 where
  camera' = (gameCamera g0) { cameraFront = mouseFront mouseInfo }
  camera'' = updateCamera keys (30 * dt) camera'

  -- Clamps camera y value to the terrain height.
  Linear.V3 cx _ cz = cameraPos camera''
  terrainHeight = Terrain.heightAt cx cz (gameTerrain1 g0) + 1
  camera''' = camera'' { cameraPos = Linear.V3 cx terrainHeight cz }

  g0' = handleLeftClick mouseInfo g0 { gameCamera = camera''' }

  update' :: Game -> Int -> IO Game
  update' !g i = do
    let
      updateEntity :: Entity -> Entity
      updateEntity !e = e
        { entityRot = entityRot e * Linear.axisAngle (Linear.V3 0 1 0) 0.01 }
    VM.modify (gameEntities g) updateEntity i
    return g

handleLeftClick :: MouseInfo -> Game -> Game
handleLeftClick info g = case mouseLeftCoords info of
  Just (x, y) ->
    let ray = calculateMouseRay (realToFrac x) (realToFrac y) w h proj view
    in g { gameLines = (cameraPos $ gameCamera g, ray):gameLines g }
  _ -> g
 where
  w = gameWidth g
  h = gameHeight g
  proj = gameProj g
  view = toViewMatrix $ gameCamera g

draw :: Game -> IO ()
draw g = do
  let view = toViewMatrix $ gameCamera g

  case gameSkyColor g of
    Linear.V3 red green blue -> do
      glClearColor red green blue 1.0
      glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  Terrain.use $ gameTerrainProgram g
  Terrain.setUniforms
    (gameTerrain1 g)
    (gameTerrainProgram g)
    (gameLights g)
    (gameSkyColor g)
    view
    (gameProj g)
  Terrain.draw (gameTerrain1 g) (gameTerrainProgram g)
  Terrain.draw (gameTerrain2 g) (gameTerrainProgram g)

  glUseProgram $ pProgram $ gameProgram g
  programSetUniforms
    (gameProgram g) (gameLights g) (gameSkyColor g) view (gameProj g)
  programSetTexture (gameProgram g) (gameTexture g)

  forM_ [0..VM.length (gameEntities g) - 1] $ \i -> do
    e <- VM.read (gameEntities g) i

    let rotM33 = Linear.fromQuaternion (entityRot e) !!* entityScale e
        matrix = Linear.mkTransformationMat rotM33 (entityPos e)
    programSetModel (gameProgram g) matrix
    programSetOffset (gameProgram g) (textureXOffset e) (textureYOffset e)

    glBindVertexArray $ modelVao $ gameRawModel g
    glDrawArrays GL_TRIANGLES 0 $ modelVertexCount $ gameRawModel g
    -- TODO(DarinM223): Use this when drawing with index buffer.
    --glDrawElements
    --  GL_TRIANGLES (Utils.modelVertexCount model) GL_UNSIGNED_INT nullPtr
    glBindVertexArray 0
  drawEntities (gameProgram g) (gameGrasses g)
  drawEntities (gameProgram g) (gameFerns g)
  drawEntities (gameProgram g) (gameLamps g)
  drawLines $ gameLines g

  glDepthFunc GL_LEQUAL
  Skybox.use $ gameSkyboxProgram g
  Skybox.setUniforms
    (gameSkyboxProgram g)
    (Linear.m33_to_m44 $ view ^. Linear._m33)
    (gameProj g)
    (gameSkyColor g)
  Skybox.draw $ gameSkybox g
  glDepthFunc GL_LESS

drawEntities :: TexProgram -> IOVec Entity -> IO ()
drawEntities p v = do
  VM.read v 0 >>= programSetTexture p . entityTex
  forM_ [0..VM.length v - 1] $ \i -> do
    e <- VM.read v i
    programSetModel p (Linear.mkTransformationMat Linear.identity (entityPos e))
    programSetOffset p (textureXOffset e) (textureYOffset e)
    glBindVertexArray $ modelVao $ entityModel e
    glDrawArrays GL_TRIANGLES 0 $ modelVertexCount $ entityModel e
    glBindVertexArray 0

-- TODO(DarinM223): Doesn't properly draw lines,
-- so fix this or remove this eventually.
drawLines :: [(Linear.V3 GLfloat, Linear.V3 GLfloat)] -> IO ()
drawLines ls = do
  glLineWidth 100
  glColor3f 1.0 1.0 0.0
  glBegin GL_LINES
  traverse_ (uncurry drawLine) ls
  glEnd
 where
  size = 1000000
  drawLine (Linear.V3 ox oy oz) (Linear.V3 dx dy dz) = do
    glVertex3f ox oy oz
    glVertex3f (ox + dx * size) (oy + dy * size) (oz + dz * size)
