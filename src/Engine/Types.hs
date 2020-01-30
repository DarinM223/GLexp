module Engine.Types where

import Control.Exception (Exception)
import Foreign.Storable (Storable (..), peek, sizeOf)
import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Linear

data Camera = Camera
  { cameraPos   :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , cameraFront :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  , cameraUp    :: {-# UNPACK #-} !(Linear.V3 GLfloat)
  }

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
