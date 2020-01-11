module Engine.Loader
  ( RawModel (..)
  , errorString
  , loadShader
  , linkShaders
  , loadToVAO
  , render
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (peek, sizeOf)
import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector.Storable as V

data RawModel = RawModel
  { modelVaoId       :: {-# UNPACK #-} !GLuint
  , modelVertexCount :: {-# UNPACK #-} !GLsizei
  } deriving Show

newtype ShaderException = ShaderException String deriving Show
instance Exception ShaderException
newtype LinkException = LinkException String deriving Show
instance Exception LinkException

loadToVAO :: V.Vector GLfloat -> IO RawModel
loadToVAO v = V.unsafeWith v $ \vPtr -> do
  vao <- alloca $ \vaoPtr -> do
    glGenVertexArrays 1 vaoPtr
    peek vaoPtr
  glBindVertexArray vao

  vbo <- alloca $ \vboPtr -> do
    glGenBuffers 1 vboPtr
    peek vboPtr
  glBindBuffer GL_ARRAY_BUFFER vbo

  glBufferData GL_ARRAY_BUFFER vSize (castPtr vPtr) GL_STATIC_DRAW
  glVertexAttribPointer
    0        -- Attribute number to set
    3        -- Size of each vertex
    GL_FLOAT -- Data is type float
    GL_FALSE -- Not normalized (False)
    stride   -- Distance between each vertex
    nullPtr  -- Offset for first vertex
  glEnableVertexAttribArray 0
  glBindBuffer GL_ARRAY_BUFFER 0
  glBindVertexArray 0
  return RawModel { modelVaoId       = vao
                  , modelVertexCount = fromIntegral $ V.length v `quot` 3
                  }
 where
  vSize = fromIntegral $ sizeOf (undefined :: GLfloat) * V.length v
  stride = fromIntegral $ sizeOf (undefined :: GLfloat) * 3

infoLength :: Int
infoLength = 512

loadShader :: GLenum -> BS.ByteString -> IO GLuint
loadShader shaderType bs = do
  shader <- glCreateShader shaderType
  BS.unsafeUseAsCStringLen bs $ \(bsPtr, len) ->
    with bsPtr $ \ptrPtr ->
    with (fromIntegral len) $ \lenPtr ->
      glShaderSource shader 1 ptrPtr lenPtr >>
      glCompileShader shader
  vertexSuccess <- alloca $ \vertexSuccessPtr -> do
    glGetShaderiv shader GL_COMPILE_STATUS vertexSuccessPtr
    peek vertexSuccessPtr
  when (vertexSuccess == GL_FALSE) $
    alloca $ \resultPtr ->
    allocaArray infoLength $ \infoLog -> do
      glGetShaderInfoLog shader (fromIntegral infoLength) resultPtr infoLog
      result <- peek resultPtr
      logBytes <- peekArray (fromIntegral result) infoLog
      throwIO $ ShaderException $ fmap (toEnum . fromEnum) logBytes
  return shader

linkShaders :: [GLuint] -> IO GLuint
linkShaders shaders = do
  program <- glCreateProgram
  traverse_ (glAttachShader program) shaders
  glLinkProgram program
  linkSuccess <- alloca $ \linkSuccessPtr -> do
    glGetProgramiv program GL_LINK_STATUS linkSuccessPtr
    peek linkSuccessPtr
  when (linkSuccess == GL_FALSE) $
    alloca $ \resultPtr ->
    allocaArray infoLength $ \infoLog -> do
      glGetProgramInfoLog program (fromIntegral infoLength) resultPtr infoLog
      result <- peek resultPtr
      logBytes <- peekArray (fromIntegral result) infoLog
      throwIO $ LinkException $ fmap (toEnum . fromEnum) logBytes
  return program

render :: RawModel -> IO ()
render RawModel{ modelVaoId = vao, modelVertexCount = vertexCount } = do
  glBindVertexArray vao
  glDrawArrays GL_TRIANGLES 0 vertexCount
  glBindVertexArray 0

errorString :: GLenum -> String
errorString GL_NO_ERROR                      = "No error"
errorString GL_INVALID_ENUM                  = "Invalid enum"
errorString GL_INVALID_VALUE                 = "Invalid value"
errorString GL_INVALID_OPERATION             = "Invalid operation"
errorString GL_STACK_OVERFLOW                = "Stack overflow"
errorString GL_STACK_UNDERFLOW               = "Stack underflow"
errorString GL_OUT_OF_MEMORY                 = "Out of memory"
errorString GL_INVALID_FRAMEBUFFER_OPERATION = "Invalid framebuffer operation"
errorString _                                = "Unknown error"
