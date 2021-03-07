{-# LANGUAGE TemplateHaskell #-}
module Engine.Types where

import Control.Exception (Exception)
import Data.Fixed (mod')
import Data.Word (Word8)
import Engine.Unboxed
import Foreign.Storable.TH (deriveStorable)
import Graphics.GL.Core45
import Graphics.GL.Types
import Linear ((^+^), (^-^), (*^))
import qualified Data.Set as S
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear

data MouseInfo = MouseInfo
  { mouseLastPos      :: {-# UNPACK #-} !CoordsMaybe
  , mouseOldPitchYaw  :: {-# UNPACK #-} !(Double, Double)
  , mouseFront        :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , mouseRightPressed :: {-# UNPACK #-} !Word8
  , mouseLeftCoords   :: {-# UNPACK #-} !CoordsMaybe
  }

calcFront :: Double -> Double -> Linear.V3 GLfloat
calcFront pitch yaw = Linear.normalize $ Linear.V3
  (cos yawR * cos pitchR) (sin pitchR) (sin yawR * cos pitchR)
 where
  toRadians = realToFrac . (* (pi / 180))
  (yawR, pitchR) = (toRadians yaw, toRadians pitch)

updateMouseInfo :: Double -> Double -> MouseInfo -> MouseInfo
updateMouseInfo x y info = info
  { mouseLastPos     = mkCoords x y
  , mouseOldPitchYaw = (pitch, yaw)
  , mouseFront       = calcFront pitch yaw
  }
 where
  (lastX, lastY) = case mouseLastPos info of
    Coords x' y' -> (x', y')
    _            -> (x, y)
  sensitivity = 0.10
  dx = (x - lastX) * sensitivity
  dy = (lastY - y) * sensitivity
  (oldPitch, oldYaw) = mouseOldPitchYaw info
  yaw = (oldYaw + dx) `mod'` 360
  pitch = oldPitch + dy

data Camera = Camera
  { cameraPos   :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , cameraFront :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , cameraUp    :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , cameraPitch :: {-# UNPACK #-} !Double
  , cameraYaw   :: {-# UNPACK #-} !Double
  }

invertPitch :: Camera -> Camera
invertPitch c = c
  { cameraPitch = pitch'
  , cameraFront = calcFront pitch' (cameraYaw c)
  }
 where pitch' = -cameraPitch c

updateCamera :: S.Set GLFW.Key -> GLfloat -> Camera -> Camera
updateCamera keys speed cam@(Camera pos front up _ _) =
  cam { cameraPos = pos ^+^ (speed *^ Linear.normalize vec) }
 where
  vec = S.foldl' buildVec (Linear.V3 0 0 0) keys
  buildVec v GLFW.Key'W = v ^+^ front
  buildVec v GLFW.Key'S = v ^-^ front
  buildVec v GLFW.Key'A = v ^-^ Linear.normalize (Linear.cross front up)
  buildVec v GLFW.Key'D = v ^+^ Linear.normalize (Linear.cross front up)
  buildVec v _          = v

toViewMatrix :: Camera -> Linear.M44 GLfloat
toViewMatrix (Camera pos front up _ _) = Linear.lookAt pos (pos ^+^ front) up

data Light = Light
  { lightPos         :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , lightColor       :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , lightAttenuation :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  } deriving Show
$(deriveStorable ''Light)

padLights :: [Light] -> Int -> [Light]
padLights (l:ls) lights | lights > 0 = l:padLights ls (lights - 1)
padLights [] lights | lights > 0     = l:padLights [] (lights - 1)
 where l = Light (Linear.V3 0 0 0) (Linear.V3 0 0 0) (Linear.V3 1 0 0)
padLights _ _  = []

setLightUniforms :: Light -> GLint -> GLint -> GLint -> IO ()
setLightUniforms light posLoc colorLoc attLoc = do
  glUniform3f posLoc posX posY posZ
  glUniform3f colorLoc cX cY cZ
  glUniform3f attLoc attX attY attZ
 where
  Linear.V3 posX posY posZ = lightPos light
  Linear.V3 cX cY cZ = lightColor light
  Linear.V3 attX attY attZ = lightAttenuation light

data RawModel = RawModel
  { modelVao         :: {-# UNPACK #-} !GLuint
  , modelVertexCount :: {-# UNPACK #-} !GLsizei
  } deriving Show
$(deriveStorable ''RawModel)

data Texture = Texture
  { textureID              :: {-# UNPACK #-} !GLuint
  , textureShineDamper     :: {-# UNPACK #-} !GLfloat
  , textureReflectivity    :: {-# UNPACK #-} !GLfloat
  , textureTransparent     :: {-# UNPACK #-} !GLboolean
  , textureUseFakeLighting :: {-# UNPACK #-} !GLfloat
  , textureNumRows         :: {-# UNPACK #-} !Int
  } deriving Show
$(deriveStorable ''Texture)

data Character = Character
  { charId       :: {-# UNPACK #-} !Int
  , charX        :: {-# UNPACK #-} !GLfloat
  , charY        :: {-# UNPACK #-} !GLfloat
  , charWidth    :: {-# UNPACK #-} !GLfloat
  , charHeight   :: {-# UNPACK #-} !GLfloat
  , charXOffset  :: {-# UNPACK #-} !GLfloat
  , charYOffset  :: {-# UNPACK #-} !GLfloat
  , charXAdvance :: {-# UNPACK #-} !GLfloat
  } deriving Show
$(deriveStorable ''Character)

data TwoDPoint = TwoDPoint
  { twoDX :: {-# UNPACK #-} !GLfloat
  , twoDY :: {-# UNPACK #-} !GLfloat
  } deriving Show
$(deriveStorable ''TwoDPoint)

data ThreeDPoint = ThreeDPoint
  { threeDX :: {-# UNPACK #-} !GLfloat
  , threeDY :: {-# UNPACK #-} !GLfloat
  , threeDZ :: {-# UNPACK #-} !GLfloat
  } deriving Show
$(deriveStorable ''ThreeDPoint)

data ThreeTuple = ThreeTuple
  { t1 :: {-# UNPACK #-} !Int
  , t2 :: {-# UNPACK #-} !Int
  , t3 :: {-# UNPACK #-} !Int
  } deriving Show
$(deriveStorable ''ThreeTuple)

data FData = FData
  { fA :: {-# UNPACK #-} !ThreeTuple
  , fB :: {-# UNPACK #-} !ThreeTuple
  , fC :: {-# UNPACK #-} !ThreeTuple
  } deriving Show
$(deriveStorable ''FData)

data Line = Line
  { lineY       :: {-# UNPACK #-} !GLfloat
  , lineCursorX :: {-# UNPACK #-} !GLfloat
  } deriving Show

data Particle = Particle
  { particlePosition      :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , particleVelocity      :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , particleGravityEffect :: {-# UNPACK #-} !GLfloat
  , particleLife          :: {-# UNPACK #-} !GLfloat
  , particleRotation      :: {-# UNPACK #-} !GLfloat
  , particleScale         :: {-# UNPACK #-} !GLfloat
  , particleElapsed       :: {-# UNPACK #-} !GLfloat
  , particleTexOffset1    :: {-# UNPACK #-} !(Linear.V2 GLfloat)
  , particleTexOffset2    :: {-# UNPACK #-} !(Linear.V2 GLfloat)
  , particleBlend         :: {-# UNPACK #-} !GLfloat
  } deriving Show
$(deriveStorable ''Particle)

data Entity = Entity
  { entityPos    :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , entityRot    :: {-# UNPACK #-} !(Linear.Quaternion GLfloat)
  , entityScale  :: {-# UNPACK #-} !GLfloat
  , entityTex    :: {-# UNPACK #-} !Texture
  , entityModel  :: {-# UNPACK #-} !RawModel
  , entityTexIdx :: {-# UNPACK #-} !Int
  } deriving Show
$(deriveStorable ''Entity)

textureXOffset :: Entity -> GLfloat
textureXOffset e = column / fromIntegral (textureNumRows (entityTex e))
 where column = fromIntegral $ entityTexIdx e `rem` textureNumRows (entityTex e)

textureYOffset :: Entity -> GLfloat
textureYOffset e = row / fromIntegral (textureNumRows (entityTex e))
 where row = fromIntegral $ entityTexIdx e `quot` textureNumRows (entityTex e)

data Projectile = Projectile
  { projectileLife   :: {-# UNPACK #-} !GLfloat
  , projectileRay    :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , projectileEntity :: {-# UNPACK #-} !Entity
  }
$(deriveStorable ''Projectile)

data WaterTile = WaterTile
  { tileX      :: {-# UNPACK #-} !GLfloat
  , tileZ      :: {-# UNPACK #-} !GLfloat
  , tileHeight :: {-# UNPACK #-} !GLfloat
  }
$(deriveStorable ''WaterTile)

data TexturePack = TexturePack
  { packBackground :: {-# UNPACK #-} !Texture
  , packR          :: {-# UNPACK #-} !Texture
  , packG          :: {-# UNPACK #-} !Texture
  , packB          :: {-# UNPACK #-} !Texture
  }

newtype ShaderException = ShaderException String deriving Show
instance Exception ShaderException

newtype LinkException = LinkException String deriving Show
instance Exception LinkException
