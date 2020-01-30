module Engine.Types where

import Control.Exception (Exception)
import Data.Fixed (mod')
import Foreign.Storable (Storable (..), peek, sizeOf)
import Graphics.GL.Core45
import Graphics.GL.Types
import Linear ((^+^), (^-^), (*^))
import qualified Data.Set as S
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear

data MouseInfo = MouseInfo
  { mouseLastPos     :: !(Maybe (Double, Double))
  , mouseOldPitchYaw :: {-# UNPACK #-} !(Double, Double)
  , mouseFront       :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  }

updateMouseInfo :: Double -> Double -> MouseInfo -> MouseInfo
updateMouseInfo x y info = MouseInfo (Just (x, y)) (pitch, yaw) front
 where
  toRadians = realToFrac . (* (pi / 180))

  (lastX, lastY) = case mouseLastPos info of
    Just (x', y') -> (x', y')
    Nothing       -> (x, y)
  sensitivity = 0.10
  dx = (x - lastX) * sensitivity
  dy = (lastY - y) * sensitivity
  (oldPitch, oldYaw) = mouseOldPitchYaw info
  yaw = (oldYaw + dx) `mod'` 360
  pitch = oldPitch + dy
  (yawR, pitchR) = (toRadians yaw, toRadians pitch)
  front = Linear.normalize $ Linear.V3
    (cos yawR * cos pitchR) (sin pitchR) (sin yawR * cos pitchR)

data Camera = Camera
  { cameraPos   :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , cameraFront :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , cameraUp    :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  }

updateCamera :: S.Set GLFW.Key -> GLfloat -> Camera -> Camera
updateCamera keys speed cam@(Camera pos front up) =
  cam { cameraPos = pos ^+^ (speed *^ Linear.normalize vec) }
 where
  vec = S.foldl' buildVec (Linear.V3 0 0 0) keys
  buildVec v GLFW.Key'W = v ^+^ front
  buildVec v GLFW.Key'S = v ^-^ front
  buildVec v GLFW.Key'A = v ^-^ Linear.normalize (Linear.cross front up)
  buildVec v GLFW.Key'D = v ^+^ Linear.normalize (Linear.cross front up)
  buildVec v _          = v

toViewMatrix :: Camera -> Linear.M44 GLfloat
toViewMatrix (Camera pos front up) = Linear.lookAt pos (pos ^+^ front) up

data Light = Light
  { lightPos   :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , lightColor :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  }

setLightUniforms :: Light -> GLint -> GLint -> IO ()
setLightUniforms light posLoc colorLoc = do
  glUniform3f posLoc posX posY posZ
  glUniform3f colorLoc cX cY cZ
 where
  Linear.V3 posX posY posZ = lightPos light
  Linear.V3 cX cY cZ = lightColor light

data RawModel = RawModel
  { modelVao         :: {-# UNPACK #-} !GLuint
  , modelVertexCount :: {-# UNPACK #-} !GLsizei
  } deriving Show

instance Storable RawModel where
  sizeOf _ = sizeOf (undefined :: GLuint) + sizeOf (undefined :: GLsizei)
  alignment _ = alignment (undefined :: GLuint)
  peek ptr = RawModel
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr (sizeOf (undefined :: GLuint))
  poke ptr m = do
    pokeByteOff ptr 0 $ modelVao m
    pokeByteOff ptr (sizeOf (undefined :: GLuint)) $ modelVertexCount m

data Texture = Texture
  { textureID           :: {-# UNPACK #-} !GLuint
  , textureShineDamper  :: {-# UNPACK #-} !GLfloat
  , textureReflectivity :: {-# UNPACK #-} !GLfloat
  } deriving Show

textureShineDamperOffset :: Int
textureShineDamperOffset = sizeOf (undefined :: GLuint)

textureReflectivityOffset :: Int
textureReflectivityOffset =
  textureShineDamperOffset + sizeOf (undefined :: GLfloat)

instance Storable Texture where
  sizeOf _ = sizeOf (undefined :: GLuint)
           + sizeOf (undefined :: GLfloat)
           + sizeOf (undefined :: GLfloat)
  alignment _ = alignment (undefined :: GLfloat)
  peek ptr = Texture
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr textureShineDamperOffset
    <*> peekByteOff ptr textureReflectivityOffset
  poke ptr t = do
    pokeByteOff ptr 0 $ textureID t
    pokeByteOff ptr textureShineDamperOffset $ textureShineDamper t
    pokeByteOff ptr textureReflectivityOffset $ textureReflectivity t

newtype ShaderException = ShaderException String deriving Show
instance Exception ShaderException

newtype LinkException = LinkException String deriving Show
instance Exception LinkException
