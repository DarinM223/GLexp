{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
module Engine.Game
  ( init
  , update
  , draw
  ) where

import Prelude hiding (init)
import Control.Lens ((^.), (&), (%~))
import Control.Monad ((>=>))
import Data.Bits ((.|.))
import Data.Fixed (mod')
import Data.Foldable (foldlM, for_)
import Engine.MousePicker (calculateMouseRay)
import Engine.Types
import Engine.Unboxed
import Engine.Utils
import Graphics.GL.Core45
import Graphics.GL.Types
import Linear ((!!*))
import System.Random (Random(random), StdGen, mkStdGen)
import qualified Data.Set as S
import qualified Data.Vector.Storable as V
import qualified Engine.FixedArray as FixedArray
import qualified Engine.Particle as Particle
import qualified Engine.Skybox as Skybox
import qualified Engine.Terrain.Terrain as Terrain
import qualified Engine.TexturedModel as TexModel
import qualified Engine.Vec as Vec
import qualified Engine.Water.FrameBuffers as FrameBuffers
import qualified Engine.Water.Water as Water
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear

maxLights :: Int
maxLights = 4

data Game = Game
  { gameWidth           :: {-# UNPACK #-} !GLfloat
  , gameHeight          :: {-# UNPACK #-} !GLfloat
  , gameStdGen          :: {-# UNPACK #-} !StdGen
  , gameEntities        :: {-# UNPACK #-} !(Vec.Vec Entity)
  , gameGrasses         :: {-# UNPACK #-} !(Vec.Vec Entity)
  , gameFerns           :: {-# UNPACK #-} !(Vec.Vec Entity)
  , gameLamps           :: {-# UNPACK #-} !(Vec.Vec Entity)
  , gameWaterTiles      :: {-# UNPACK #-} !(Vec.Vec WaterTile)
  , gameProgram         :: {-# UNPACK #-} !TexModel.Program
  , gameCamera          :: {-# UNPACK #-} !Camera
  , gameProj            :: {-# UNPACK #-} !(Linear.M44 GLfloat)
  , gameLights          :: {-# UNPACK #-} !(V.Vector Light)
  , gameProjectiles     :: {-# UNPACK #-} !(FixedArray.Array Projectile)
  , gameTexture         :: {-# UNPACK #-} !Texture
  , gameRawModel        :: {-# UNPACK #-} !RawModel
  , gameTerrainProgram  :: {-# UNPACK #-} !Terrain.Program
  , gameTerrain1        :: {-# UNPACK #-} !Terrain.Terrain
  , gameTerrain2        :: {-# UNPACK #-} !Terrain.Terrain
  , gameSkyboxProgram   :: {-# UNPACK #-} !Skybox.Program
  , gameSkybox          :: {-# UNPACK #-} !Skybox.Skybox
  , gameWaterProgram    :: {-# UNPACK #-} !Water.Program
  , gameWater           :: {-# UNPACK #-} !Water.Water
  , gameWaterBuffers    :: {-# UNPACK #-} !FrameBuffers.FrameBuffers
  , gameParticleModel   :: {-# UNPACK #-} !RawModel
  , gameParticles       :: {-# UNPACK #-} !(FixedArray.Array Particle)
  , gameParticleProgram :: {-# UNPACK #-} !Particle.Program
  , gameSkyColor        :: {-# UNPACK #-} !(Linear.V3 GLfloat)
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
    waterTiles = [WaterTile 60 60 (-8)]
  pack <- loadTexturePack
    "res/grass.png" "res/mud.png" "res/grassFlowers.png" "res/path.png"
  blendMap <- loadTexture "res/blendMap.png"
  Game (fromIntegral w) (fromIntegral h) (mkStdGen 2)
    <$> Vec.fromList initEntities
    <*> Vec.fromList grassEntities
    <*> Vec.fromList fernEntities
    <*> Vec.fromList lampEntities
    <*> Vec.fromList waterTiles
    <*> TexModel.mkProgram maxLights
    <*> pure camera
    <*> pure proj
    <*> pure (V.fromList [light1, light2, light3, light4])
    <*> FixedArray.new 100
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
    <*> Water.mkProgram maxLights
    <*> Water.mkWater
    <*> FrameBuffers.init (fromIntegral w) (fromIntegral h)
    <*> Particle.mkParticleModel
    <*> FixedArray.new 1000
    <*> Particle.mkProgram
    <*> pure (Linear.V3 0.5 0.5 0.5)
 where
  camera = Camera (Linear.V3 10 2 30) (Linear.V3 0 0 (-1)) (Linear.V3 0 1 0) 0 0
  proj = perspectiveMat w h
  light1 =
    Light (Linear.V3 0 1000 (-7000)) (Linear.V3 0.4 0.4 0.4) (Linear.V3 1 0 0)
  light2 =
    Light (Linear.V3 20 17 20) (Linear.V3 2 0 0) (Linear.V3 1 0.01 0.002)
  light3 =
    Light (Linear.V3 20 17 100) (Linear.V3 0 2 2) (Linear.V3 1 0.01 0.002)
  light4 =
    Light (Linear.V3 100 17 20) (Linear.V3 2 2 0) (Linear.V3 1 0.01 0.002)

update :: S.Set GLFW.Key -> MouseInfo -> GLfloat -> Game -> Vec.UpdateM Game
update keys mouseInfo dt g0 =
  foldlM update' g0' [0..Vec.length (gameEntities g0') - 1] >>=
    (handleLeftClick mouseInfo >=> updateProjectiles dt >=> updateParticles dt)
 where
  (pitch, yaw) = mouseOldPitchYaw mouseInfo
  camera' = (gameCamera g0)
    { cameraFront = mouseFront mouseInfo
    , cameraPitch = pitch
    , cameraYaw   = yaw
    }
  camera'' = updateCamera keys (30 * dt) camera'

  -- Clamps camera y value to the terrain height.
  Linear.V3 cx _ cz = cameraPos camera''
  terrainHeight = Terrain.heightAt cx cz (gameTerrain1 g0) + 1
  camera''' = camera'' { cameraPos = Linear.V3 cx terrainHeight cz }

  g0' = updateTime dt g0 { gameCamera = camera''' }

  update' :: Game -> Int -> Vec.UpdateM Game
  update' !g i = do
    let
      updateEntity :: Entity -> Entity
      updateEntity !e = e
        { entityRot = entityRot e * Linear.axisAngle (Linear.V3 0 1 0) 0.01 }
    Vec.modify (gameEntities g) updateEntity i
    return g

updateParticles :: GLfloat -> Game -> Vec.UpdateM Game
updateParticles elapsed g = do
  ps <- FixedArray.foldlM updateParticle ps0 ps0
  (addedWhole, stdGen) <- foldlM
    (\t _ -> emitParticle t) (ps, gameStdGen g) [0..count - 1]
  let (r, stdGen') = random stdGen
  (addedPartial, stdGen'') <- if r < partialParticle
    then emitParticle (addedWhole, stdGen')
    else pure (addedWhole, stdGen')
  return g { gameParticles = addedPartial, gameStdGen = stdGen'' }
 where
  ps0 = gameParticles g

  pps = 50
  speed = 25
  gravityEffect = 0.3
  lifeLength = 4
  center = Linear.V3 20 0 20

  particlesToCreate = elapsed * pps
  count = floor particlesToCreate :: Int
  partialParticle = particlesToCreate `mod'` 1

  emitParticle (!ps, !stdGen) = (, stdGen'') <$> FixedArray.add ps p
   where
    (xrand, stdGen') = random stdGen
    (zrand, stdGen'') = random stdGen'
    dirX = xrand * 2 - 1
    dirZ = zrand * 2 - 1

    v = Linear.normalize (Linear.V3 dirX 1 dirZ) Linear.^* speed
    p = Particle center v gravityEffect lifeLength 0 1 0
  updateParticle ps i p
    | Particle.alive p' = ps <$ FixedArray.write ps i p'
    | otherwise         = FixedArray.delete ps i
   where p' = Particle.update elapsed p

updateProjectiles :: GLfloat -> Game -> Vec.UpdateM Game
updateProjectiles elapsed g
  =   (\ps' -> g { gameProjectiles = ps' })
  <$> FixedArray.foldlM updateProjectile ps ps
 where
  ps = gameProjectiles g
  updateProjectile ps' i p = do
    let e  = projectileEntity p
        e' = e { entityPos = entityPos e + projectileRay p }
    FixedArray.write ps' i p
      { projectileEntity = e', projectileLife = projectileLife p - elapsed }
    if projectileLife p - elapsed < 0
      then FixedArray.delete ps' i
      else pure ps'

updateTime :: GLfloat -> Game -> Game
updateTime elapsed g = g { gameWater = Water.update elapsed (gameWater g) }

handleLeftClick :: MouseInfo -> Game -> Vec.UpdateM Game
handleLeftClick info g = case mouseLeftCoords info of
  Coords x y ->
    let
      ray    = calculateMouseRay (realToFrac x) (realToFrac y) w h proj view
      bullet = Projectile 1 ray $ Entity
        (cameraPos $ gameCamera g)
        (Linear.axisAngle (Linear.V3 (0.0 :: GLfloat) 0.0 1.0) 0)
        0.1
        (gameTexture g)
        (gameRawModel g)
        0
    in setProjectiles g <$> FixedArray.add (gameProjectiles g) bullet
  _ -> pure g
 where
  setProjectiles g' p = g' { gameProjectiles = p }
  w = gameWidth g
  h = gameHeight g
  proj = gameProj g
  view = toViewMatrix $ gameCamera g

draw :: Game -> IO ()
draw g = do
  let view = toViewMatrix $ gameCamera g
  waterTile <- Vec.read (gameWaterTiles g) 0

  glEnable GL_CLIP_DISTANCE0
  FrameBuffers.bindReflectionFrameBuffer $ gameWaterBuffers g
  let
    distance = 2 * cameraPos (gameCamera g) ^. Linear._y - tileHeight waterTile
    camera'  = (gameCamera g) { cameraPos = cameraPos (gameCamera g)
                                          & Linear._y %~ subtract distance
                              }
    view' = toViewMatrix $ invertPitch camera'
  drawScene g view' (Linear.V4 0 1 0 (-tileHeight waterTile + 1))

  FrameBuffers.bindRefractionFrameBuffer $ gameWaterBuffers g
  drawScene g view (Linear.V4 0 (-1) 0 (tileHeight waterTile + 1))
  glDisable GL_CLIP_DISTANCE0

  FrameBuffers.unbindFrameBuffer $ gameWaterBuffers g
  drawScene g view (Linear.V4 0 0 0 0)

  Water.use $ gameWaterProgram g
  Water.setUniforms
    (gameWaterProgram g)
    view
    (gameProj g)
    (cameraPos (gameCamera g))
    (Water.moveFactor (gameWater g))
  Water.setLights (gameWaterProgram g) (gameLights g)
  Water.setTextures (gameWaterProgram g) (gameWaterBuffers g) (gameWater g)
  for_ [0..Vec.length (gameWaterTiles g) - 1] $ \i -> do
    tile <- Vec.read (gameWaterTiles g) i
    Water.drawTile (gameWater g) tile (gameWaterProgram g)
  Water.unbind

  Particle.prepare (gameParticleProgram g) (gameParticleModel g)
  Particle.setProj (gameParticleProgram g) (gameProj g)
  FixedArray.forM_ (gameParticles g) $ \_ p -> do
    Particle.setModelView (gameParticleProgram g) p view
    Particle.draw $ gameParticleModel g
  Particle.unbind

drawScene :: Game -> Linear.M44 GLfloat -> Linear.V4 GLfloat -> IO ()
drawScene g view clipPlane = do
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
    clipPlane
  Terrain.draw (gameTerrain1 g) (gameTerrainProgram g)
  Terrain.draw (gameTerrain2 g) (gameTerrainProgram g)

  TexModel.use $ gameProgram g
  TexModel.setUniforms
    (gameProgram g) (gameLights g) (gameSkyColor g) view (gameProj g) clipPlane
  TexModel.setTexture (gameProgram g) (gameTexture g)

  glBindVertexArray $ modelVao $ gameRawModel g
  for_ [0..Vec.length (gameEntities g) - 1] $ \i -> do
    e <- Vec.read (gameEntities g) i

    let rotM33 = Linear.fromQuaternion (entityRot e) !!* entityScale e
        matrix = Linear.mkTransformationMat rotM33 (entityPos e)
    TexModel.setModel (gameProgram g) matrix
    TexModel.setOffset (gameProgram g) (textureXOffset e) (textureYOffset e)

    glDrawArrays GL_TRIANGLES 0 $ modelVertexCount $ gameRawModel g
    -- TODO(DarinM223): Use this when drawing with index buffer.
    --glDrawElements
    --  GL_TRIANGLES (Utils.modelVertexCount model) GL_UNSIGNED_INT nullPtr
  FixedArray.forM_ (gameProjectiles g) $ \_ p -> do
    let e      = projectileEntity p
        rotM33 = Linear.fromQuaternion (entityRot e) !!* entityScale e
        matrix = Linear.mkTransformationMat rotM33 (entityPos e)
    TexModel.setModel (gameProgram g) matrix
    TexModel.setOffset (gameProgram g) (textureXOffset e) (textureYOffset e)

    glDrawArrays GL_TRIANGLES 0 $ modelVertexCount $ gameRawModel g
  glBindVertexArray 0

  drawEntities (gameProgram g) (gameGrasses g)
  drawEntities (gameProgram g) (gameFerns g)
  drawEntities (gameProgram g) (gameLamps g)

  glDepthFunc GL_LEQUAL
  Skybox.use $ gameSkyboxProgram g
  Skybox.setUniforms
    (gameSkyboxProgram g)
    (Linear.m33_to_m44 $ view ^. Linear._m33)
    (gameProj g)
    (gameSkyColor g)
  Skybox.draw $ gameSkybox g
  glDepthFunc GL_LESS

drawEntities :: TexModel.Program -> Vec.Vec Entity -> IO ()
drawEntities p v = do
  e0 <- Vec.read v 0
  TexModel.setTexture p $ entityTex e0
  glBindVertexArray $ modelVao $ entityModel e0
  for_ [0..Vec.length v - 1] $ \i -> do
    e <- Vec.read v i
    TexModel.setModel p
      (Linear.mkTransformationMat Linear.identity (entityPos e))
    TexModel.setOffset p (textureXOffset e) (textureYOffset e)
    glDrawArrays GL_TRIANGLES 0 $ modelVertexCount $ entityModel e
  glBindVertexArray 0
