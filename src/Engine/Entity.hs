module Engine.Entity (Entity (..)) where

import Engine.Types (RawModel, Texture)
import Foreign.Storable (Storable (..))
import Graphics.GL.Types (GLfloat)
import Linear (Quaternion, V3)

data Entity = Entity
  { entityPos   :: {-# UNPACK #-} !(V3 GLfloat)
  , entityRot   :: {-# UNPACK #-} !(Quaternion GLfloat)
  , entityScale :: {-# UNPACK #-} !GLfloat
  , entityTex   :: {-# UNPACK #-} !Texture
  , entityModel :: {-# UNPACK #-} !RawModel
  } deriving Show

entityRotOffset :: Int
entityRotOffset = sizeOf (undefined :: V3 GLfloat)

entityScaleOffset :: Int
entityScaleOffset = entityRotOffset + sizeOf (undefined :: Quaternion GLfloat)

entityTexOffset :: Int
entityTexOffset = entityScaleOffset + sizeOf (undefined :: GLfloat)

entityModelOffset :: Int
entityModelOffset = entityTexOffset + sizeOf (undefined :: Texture)

instance Storable Entity where
  sizeOf _ = sizeOf (undefined :: V3 GLfloat)
           + sizeOf (undefined :: Quaternion GLfloat)
           + sizeOf (undefined :: GLfloat)
           + sizeOf (undefined :: Texture)
           + sizeOf (undefined :: RawModel)
  alignment _ = alignment (undefined :: V3 GLfloat)
  peek ptr = Entity
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr entityRotOffset
    <*> peekByteOff ptr entityScaleOffset
    <*> peekByteOff ptr entityTexOffset
    <*> peekByteOff ptr entityModelOffset
  poke ptr e = do
    pokeByteOff ptr 0 $ entityPos e
    pokeByteOff ptr entityRotOffset $ entityRot e
    pokeByteOff ptr entityScaleOffset $ entityScale e
    pokeByteOff ptr entityTexOffset $ entityTex e
    pokeByteOff ptr entityModelOffset $ entityModel e
