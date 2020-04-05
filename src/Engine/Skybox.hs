{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Engine.Skybox
  ( Skybox
  , SkyboxProgram
  , load
  , mkProgram
  , setUniforms
  , use
  , draw
  ) where

import Codec.Picture
import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Engine.Types (RawModel (..))
import Engine.Utils (linkShaders, loadShader)
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (peek, sizeOf)
import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as V
import qualified Linear
import qualified Text.RawString.QQ as QQ

vertexShaderSrc :: ByteString
vertexShaderSrc = BS.pack
  [QQ.r|
    #version 330 core
    layout (location = 0) in vec3 position;
    out vec3 v_texCoord;
    uniform mat4 projection;
    uniform mat4 view;

    void main() {
      v_texCoord = position;
      vec4 pos = projection * view * vec4(position, 1.0);
      gl_Position = pos.xyww;
    }
  |]

fragmentShaderSrc :: ByteString
fragmentShaderSrc = BS.pack
  [QQ.r|
    #version 330 core
    in vec3 v_texCoord;
    out vec4 color;
    uniform samplerCube skybox;

    void main() {
      color = texture(skybox, v_texCoord);
    }
  |]

vertexBuffer :: V.Vector GLfloat
vertexBuffer = V.fromList
  [ -1.0,  1.0, -1.0
  , -1.0, -1.0, -1.0
  ,  1.0, -1.0, -1.0
  ,  1.0, -1.0, -1.0
  ,  1.0,  1.0, -1.0
  , -1.0,  1.0, -1.0

  , -1.0, -1.0,  1.0
  , -1.0, -1.0, -1.0
  , -1.0,  1.0, -1.0
  , -1.0,  1.0, -1.0
  , -1.0,  1.0,  1.0
  , -1.0, -1.0,  1.0

  ,  1.0, -1.0, -1.0
  ,  1.0, -1.0,  1.0
  ,  1.0,  1.0,  1.0
  ,  1.0,  1.0,  1.0
  ,  1.0,  1.0, -1.0
  ,  1.0, -1.0, -1.0

  , -1.0, -1.0,  1.0
  , -1.0,  1.0,  1.0
  ,  1.0,  1.0,  1.0
  ,  1.0,  1.0,  1.0
  ,  1.0, -1.0,  1.0
  , -1.0, -1.0,  1.0

  , -1.0,  1.0, -1.0
  ,  1.0,  1.0, -1.0
  ,  1.0,  1.0,  1.0
  ,  1.0,  1.0,  1.0
  , -1.0,  1.0,  1.0
  , -1.0,  1.0, -1.0

  , -1.0, -1.0, -1.0
  , -1.0, -1.0,  1.0
  ,  1.0, -1.0, -1.0
  ,  1.0, -1.0, -1.0
  , -1.0, -1.0,  1.0
  ,  1.0, -1.0,  1.0
  ]

data Skybox = Skybox
  { skyboxTexture  :: {-# UNPACK #-} !GLuint
  , skyboxRawModel :: {-# UNPACK #-} !RawModel
  }

load :: [FilePath] -> IO Skybox
load paths = Skybox <$> loadTexture paths <*> generateSkyboxModel

loadTexture :: [FilePath] -> IO GLuint
loadTexture paths = do
  texture <- alloca $ \texturePtr -> do
    glGenTextures 1 texturePtr
    peek texturePtr
  glBindTexture GL_TEXTURE_CUBE_MAP texture
  forM_ (zip [0..] paths) $ \(i, path) -> do
    Right image <- readImage path
    let ipixelrgb8 = convertRGB8 image
        iWidth     = fromIntegral $ imageWidth ipixelrgb8
        iHeight    = fromIntegral $ imageHeight ipixelrgb8
        iData      = imageData ipixelrgb8
    V.unsafeWith iData $ \dataPtr ->
      glTexImage2D
        (GL_TEXTURE_CUBE_MAP_POSITIVE_X + i)
        0
        GL_RGB
        iWidth
        iHeight
        0
        GL_RGB
        GL_UNSIGNED_BYTE
        (castPtr dataPtr)
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_MIN_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_MAG_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
  glTexParameteri GL_TEXTURE_CUBE_MAP GL_TEXTURE_WRAP_R GL_CLAMP_TO_EDGE
  return texture

generateSkyboxModel :: IO RawModel
generateSkyboxModel = V.unsafeWith vertexBuffer $ \vPtr -> do
  vao <- alloca $ \vaoPtr -> do
    glGenVertexArrays 1 vaoPtr
    peek vaoPtr
  glBindVertexArray vao

  vbo <- alloca $ \vboPtr -> do
    glGenBuffers 1 vboPtr
    peek vboPtr
  glBindBuffer GL_ARRAY_BUFFER vbo
  glBufferData GL_ARRAY_BUFFER vSize (castPtr vPtr) GL_STATIC_DRAW

  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE stride nullPtr
  glEnableVertexAttribArray 0

  glBindBuffer GL_ARRAY_BUFFER 0
  glBindVertexArray 0
  return RawModel
    { modelVao         = vao
    , modelVertexCount = fromIntegral $ V.length vertexBuffer `quot` 3
    }
 where
  vSize = fromIntegral $ sizeOf (undefined :: GLfloat) * V.length vertexBuffer
  stride = fromIntegral $ sizeOf (undefined :: GLfloat) * 3

data SkyboxProgram = SkyboxProgram
  { sProgram :: {-# UNPACK #-} !GLuint
  , sViewLoc :: {-# UNPACK #-} !GLint
  , sProjLoc :: {-# UNPACK #-} !GLint
  }

mkProgram :: IO SkyboxProgram
mkProgram =
  bracket loadVertexShader glDeleteShader $ \vertexShader ->
  bracket loadFragmentShader glDeleteShader $ \fragmentShader -> do
    sProgram <- linkShaders [vertexShader, fragmentShader]
    sViewLoc <- withCString "view" $ \name ->
      glGetUniformLocation sProgram name
    sProjLoc <- withCString "projection" $ \name ->
      glGetUniformLocation sProgram name
    return SkyboxProgram{..}
 where
  loadVertexShader = loadShader GL_VERTEX_SHADER vertexShaderSrc
  loadFragmentShader = loadShader GL_FRAGMENT_SHADER fragmentShaderSrc

setUniforms
  :: SkyboxProgram -> Linear.M44 GLfloat -> Linear.M44 GLfloat -> IO ()
setUniforms p view proj = do
  with view $ \matrixPtr ->
    glUniformMatrix4fv (sViewLoc p) 1 GL_TRUE (castPtr matrixPtr)
  with proj $ \matrixPtr ->
    glUniformMatrix4fv (sProjLoc p) 1 GL_TRUE (castPtr matrixPtr)

use :: SkyboxProgram -> IO ()
use p = glUseProgram $ sProgram p

draw :: Skybox -> IO ()
draw s = do
  glBindVertexArray $ modelVao $ skyboxRawModel s
  glBindTexture GL_TEXTURE_CUBE_MAP $ skyboxTexture s
  glDrawArrays GL_TRIANGLES 0 36
  glBindVertexArray 0
