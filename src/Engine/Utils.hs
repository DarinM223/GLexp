{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Engine.Utils
  ( errorString
  , perspectiveMat
  , loadShader
  , loadTexture
  , loadTexturePack
  , linkShaders
  , loadVAO
  , loadObj
  ) where

import Codec.Picture
import Control.Exception (throwIO)
import Control.Monad (join, when)
import Data.Foldable (traverse_)
import Engine.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), peek, sizeOf)
import Graphics.GL.Core45
import Graphics.GL.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as Vec
import qualified Data.Vector.Storable as V
import qualified Linear

perspectiveMat :: Int -> Int -> Linear.M44 GLfloat
perspectiveMat width height =
  Linear.perspective fov aspectRatio nearPlane farPlane
 where
  fov = 45 * (pi / 180)
  aspectRatio = fromIntegral width / fromIntegral height
  nearPlane = 0.1
  farPlane = 1000

loadVAO :: V.Vector GLfloat -- ^ Positions
        -> V.Vector GLuint  -- ^ Indices
        -> IO RawModel
loadVAO v e = V.unsafeWith v $ \vPtr ->
              V.unsafeWith e $ \ePtr -> do
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

  glVertexAttribPointer
    0        -- Attribute number to set
    3        -- Size of each vertex
    GL_FLOAT -- Data is type float
    GL_FALSE -- Not normalized (False)
    stride   -- Distance between each vertex
    nullPtr  -- Offset for first vertex
  glEnableVertexAttribArray 0

  glVertexAttribPointer
    1
    2
    GL_FLOAT
    GL_FALSE
    stride
    (nullPtr `plusPtr` (3 * sizeOf (undefined :: GLfloat)))
  glEnableVertexAttribArray 1

  glBindBuffer GL_ARRAY_BUFFER 0
  glBindVertexArray 0

  return RawModel { modelVao         = vao
                  , modelVertexCount = fromIntegral $ V.length e
                  }
 where
  vSize = fromIntegral $ sizeOf (undefined :: GLfloat) * V.length v
  eSize = fromIntegral $ sizeOf (undefined :: GLuint) * V.length e
  stride = fromIntegral $ sizeOf (undefined :: GLfloat) * 5

loadObj :: FilePath -> IO RawModel
loadObj path = fmap
  (V.fromList . convertVec . foldr build ([], [], [], []) . T.lines)
  (T.readFile path) >>= toRawModel
 where
  build line (!vs, !vts, !vns, !fs) = case headWord of
    Just "v"  -> (parse3:vs, vts, vns, fs)
    Just "vt" -> (vs, parse2:vts, vns, fs)
    Just "vn" -> (vs, vts, parse3:vns, fs)
    Just "f"  -> (vs, vts, vns, parseF:fs)
    _         -> (vs, vts, vns, fs)
   where
    parse2 :: [GLfloat]
    parse2 = [read $ T.unpack $ words0 !! 1, read $ T.unpack $ words0 !! 2]

    parse3 :: [GLfloat]
    parse3 = [ read $ T.unpack $ words0 !! 1
             , read $ T.unpack $ words0 !! 2
             , read $ T.unpack $ words0 !! 3 ]

    parseF = (words0 !! 1, words0 !! 2, words0 !! 3)

    words0 = filter (/= "") $ T.splitOn " " line
    headWord = case words0 of
      []  -> Nothing
      h:_ -> Just h
  convertVec (vs, vts, vns, fs) = fs >>= combineParams
   where
    vs' = Vec.fromList vs
    vts' = Vec.fromList vts
    vns' = Vec.fromList vns
    combineParams (a, b, c) =
      combineVertex a <> combineVertex b <> combineVertex c
    combineVertex v = join [vs' Vec.! p0, vts' Vec.! p1, vns' Vec.! p2]
     where
      params = T.splitOn "/" v
      p0 = read (T.unpack $ head params) - 1
      p1 = read (T.unpack $ params !! 1) - 1
      p2 = read (T.unpack $ params !! 2) - 1
  toRawModel v = V.unsafeWith v $ \vPtr -> do
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

    glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE stride
      (nullPtr `plusPtr` (3 * sizeOf (undefined :: GLfloat)))
    glEnableVertexAttribArray 1

    glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE stride
      (nullPtr `plusPtr` (5 * sizeOf (undefined :: GLfloat)))
    glEnableVertexAttribArray 2

    glBindBuffer GL_ARRAY_BUFFER 0
    glBindVertexArray 0
    return RawModel { modelVao         = vao
                    , modelVertexCount = fromIntegral $ V.length v `quot` 8
                    }
   where
    vSize = fromIntegral $ sizeOf (undefined :: GLfloat) * V.length v
    stride = fromIntegral $ sizeOf (undefined :: GLfloat) * 8

loadTexture :: FilePath -> IO Texture
loadTexture path = do
  Right file <- readImage path
  let ipixelrgb8 = convertRGBA8 file
      iWidth     = fromIntegral $ imageWidth ipixelrgb8
      iHeight    = fromIntegral $ imageHeight ipixelrgb8
      iData      = imageData ipixelrgb8
  texture <- alloca $ \texturePtr -> do
    glGenTextures 1 texturePtr
    peek texturePtr
  glBindTexture GL_TEXTURE_2D texture

  glGenerateMipmap GL_TEXTURE_2D
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  glTexParameterf GL_TEXTURE_2D GL_TEXTURE_LOD_BIAS (-0.4)

  V.unsafeWith iData $ \dataPtr ->
    glTexImage2D
      GL_TEXTURE_2D
      0
      GL_RGBA
      iWidth
      iHeight
      0
      GL_RGBA
      GL_UNSIGNED_BYTE
      (castPtr dataPtr)
  glGenerateMipmap GL_TEXTURE_2D
  glBindTexture GL_TEXTURE_2D 0
  return $ Texture texture 1.0 0.0 0 0 1

loadTexturePack
  :: FilePath -> FilePath -> FilePath -> FilePath -> IO TexturePack
loadTexturePack back r g b = TexturePack
  <$> loadTexture back <*> loadTexture r <*> loadTexture g <*> loadTexture b

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
      logLength <- peek resultPtr
      logBytes <- peekArray (fromIntegral logLength) infoLog
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
      logLength <- peek resultPtr
      logBytes <- peekArray (fromIntegral logLength) infoLog
      throwIO $ LinkException $ fmap (toEnum . fromEnum) logBytes
  return program

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
