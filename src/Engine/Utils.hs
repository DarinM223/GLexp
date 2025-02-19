{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Engine.Utils
  ( errorString
  , perspectiveMat
  , loadShader
  , loadTexture
  , loadTexturePack
  , loadFont
  , linkShaders
  , loadInstancedVBO
  , updateVBO
  , loadVAO
  , loadVAOWithIndices
  , loadObj
  , shaderHeader
  ) where

import Codec.Picture
import Control.Exception (throwIO)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Attoparsec.ByteString.Char8
import Data.Foldable (foldlM, for_, traverse_)
import Engine.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), peek, sizeOf)
import Graphics.GL.Core45
import Graphics.GL.Types
import System.IO (IOMode (ReadMode), hIsEOF, withFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import qualified Linear

infixr 5 :<

pattern (:<) :: Char -> BSC.ByteString -> BSC.ByteString
pattern b :< bs <- (BSC.uncons -> Just (b, bs))

perspectiveMat :: Int -> Int -> Linear.M44 GLfloat
perspectiveMat width height =
  Linear.perspective fov aspectRatio nearPlane farPlane
 where
  fov = 45 * (pi / 180)
  aspectRatio = fromIntegral width / fromIntegral height
  nearPlane = 0.1
  farPlane = 1000

loadInstancedVBO :: GLsizei -> Int -> IO GLuint
loadInstancedVBO instancedDataLen maxInstances = do
  vbo <- alloca $ \ptr -> glGenBuffers 1 ptr >> peek ptr
  glBindBuffer GL_ARRAY_BUFFER vbo
  glBufferData GL_ARRAY_BUFFER vSize nullPtr GL_STREAM_DRAW

  glVertexAttribPointer 1 4 GL_FLOAT GL_FALSE stride (offset 0)
  glVertexAttribDivisor 1 1
  glEnableVertexAttribArray 1
  glVertexAttribPointer 2 4 GL_FLOAT GL_FALSE stride (offset 4)
  glVertexAttribDivisor 2 1
  glEnableVertexAttribArray 2
  glVertexAttribPointer 3 4 GL_FLOAT GL_FALSE stride (offset 8)
  glVertexAttribDivisor 3 1
  glEnableVertexAttribArray 3
  glVertexAttribPointer 4 4 GL_FLOAT GL_FALSE stride (offset 12)
  glVertexAttribDivisor 4 1
  glEnableVertexAttribArray 4
  glVertexAttribPointer 5 4 GL_FLOAT GL_FALSE stride (offset 16)
  glVertexAttribDivisor 5 1
  glEnableVertexAttribArray 5
  glVertexAttribPointer 6 1 GL_FLOAT GL_FALSE stride (offset 20)
  glVertexAttribDivisor 6 1
  glEnableVertexAttribArray 6

  glBindBuffer GL_ARRAY_BUFFER 0
  return vbo
 where
  vSize = fromIntegral stride * fromIntegral maxInstances
  stride = instancedDataLen * fromIntegral (sizeOf (undefined :: GLfloat))
  offset o = nullPtr `plusPtr`
    (o * fromIntegral (sizeOf (undefined :: GLfloat)))

updateVBO :: GLuint -> VM.IOVector GLfloat -> IO ()
updateVBO vbo v = VM.unsafeWith v $ \vPtr -> do
  glBindBuffer GL_ARRAY_BUFFER vbo
  glBufferData GL_ARRAY_BUFFER vSize (castPtr vPtr) GL_STREAM_DRAW
  glBufferSubData GL_ARRAY_BUFFER 0 vSize (castPtr vPtr)
  glBindBuffer GL_ARRAY_BUFFER 0
 where vSize = fromIntegral $ sizeOf (undefined :: GLfloat) * VM.length v

loadVAO :: V.Vector GLfloat -> Int -> IO RawModel
loadVAO v n = V.unsafeWith v $ \vPtr -> do
  vao <- alloca $ \vaoPtr -> do
    glGenVertexArrays 1 vaoPtr
    peek vaoPtr
  glBindVertexArray vao
  vbo <- alloca $ \vboPtr -> do
    glGenBuffers 1 vboPtr
    peek vboPtr
  glBindBuffer GL_ARRAY_BUFFER vbo
  glBufferData GL_ARRAY_BUFFER vSize (castPtr vPtr) GL_STATIC_DRAW

  glVertexAttribPointer 0 (fromIntegral n) GL_FLOAT GL_FALSE stride nullPtr
  glEnableVertexAttribArray 0

  glBindBuffer GL_ARRAY_BUFFER 0
  glBindVertexArray 0
  return RawModel { modelVao         = vao
                  , modelVertexCount = fromIntegral $ V.length v `quot` n
                  }
 where
  vSize = fromIntegral $ sizeOf (undefined :: GLfloat) * V.length v
  stride = fromIntegral $ sizeOf (undefined :: GLfloat) * n

loadVAOWithIndices :: V.Vector GLfloat -- ^ Positions
                   -> V.Vector GLuint  -- ^ Indices
                   -> IO RawModel
loadVAOWithIndices v e = V.unsafeWith v $ \vPtr ->
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
loadObj path = do
  (vs, vts, vns, fs) <- (\(vs', vts', vns', fs') -> (V.fromList vs', V.fromList vts', V.fromList vns', fs'))
                      . foldr build ([], [], [], [])
                      . BSC.lines
                    <$> BS.readFile path
  vec <- VM.new (length fs * 24)
  void $ foldlM (writeVec vs vts vns vec) (0 :: Int) fs
  toRawModel vec
 where
  build line (!vs, !vts, !vns, !fs) = case line of
    'v' :< 't' :< _ -> (vs, run (parse2d "vt") line:vts, vns, fs)
    'v' :< 'n' :< _ -> (vs, vts, run (parse3d "vn") line:vns, fs)
    'v' :< _        -> (run (parse3d "v") line:vs, vts, vns, fs)
    'f' :< _        -> (vs, vts, vns, run parseFragment line:fs)
    _               -> (vs, vts, vns, fs)
  writeVec :: V.Vector ThreeDPoint -> V.Vector TwoDPoint -> V.Vector ThreeDPoint -> VM.IOVector GLfloat -> Int -> FData -> IO Int
  writeVec vs vts vns vec i f = liftIO $ do
    writeVertex i (fA f) vec vs vts vns
    writeVertex (i + 8) (fB f) vec vs vts vns
    writeVertex (i + 16) (fC f) vec vs vts vns
    return (i + 24)
  writeVertex :: Int -> ThreeTuple -> VM.IOVector GLfloat -> V.Vector ThreeDPoint -> V.Vector TwoDPoint -> V.Vector ThreeDPoint -> IO ()
  writeVertex i (ThreeTuple a b c) vec vs vts vns = do
    for_ (vs V.!? (a - 1)) $ \v -> do
      VM.write vec i (threeDX v)
      VM.write vec (i + 1) (threeDY v)
      VM.write vec (i + 2) (threeDZ v)
    for_ (vts V.!? (b - 1)) $ \vt -> do
      VM.write vec (i + 3) (twoDX vt)
      VM.write vec (i + 4) (twoDY vt)
    for_ (vns V.!? (c - 1)) $ \vn -> do
      VM.write vec (i + 5) (threeDX vn)
      VM.write vec (i + 6) (threeDY vn)
      VM.write vec (i + 7) (threeDZ vn)

  run p = either error id . parseOnly p

  parse2d s = TwoDPoint
          <$> (string s *> skipSpace *> signed rational <* skipSpace)
          <*> signed rational
  parse3d s = ThreeDPoint
          <$> (string s *> skipSpace *> signed rational <* skipSpace)
          <*> (signed rational <* skipSpace)
          <*> signed rational
  parseFragment = FData
              <$> (char 'f' *> skipSpace *> parseSlashes)
              <*> (skipSpace *> parseSlashes <* skipSpace)
              <*> parseSlashes
  parseSlashes = ThreeTuple
    <$> (decimal <* char '/') <*> (decimal <* char '/') <*> decimal

  toRawModel v = VM.unsafeWith v $ \vPtr -> do
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
                    , modelVertexCount = fromIntegral $ VM.length v `quot` 8
                    }
   where
    vSize = fromIntegral $ sizeOf (undefined :: GLfloat) * VM.length v
    stride = fromIntegral $ sizeOf (undefined :: GLfloat) * 8

parseCharacter :: BS.ByteString -> Maybe Character
parseCharacter bs = case parseOnly parse' bs of
  Left _ -> Nothing
  Right r -> Just r
 where
  parse' = Character
       <$> (string "char id=" *> signed decimal <* skipSpace)
       <*> (string "x=" *> signed rational <* skipSpace)
       <*> (string "y=" *> signed rational <* skipSpace)
       <*> (string "width=" *> signed rational <* skipSpace)
       <*> (string "height=" *> signed rational <* skipSpace)
       <*> (string "xoffset=" *> signed rational <* skipSpace)
       <*> (string "yoffset=" *> signed rational <* skipSpace)
       <*> (string "xadvance=" *> signed rational <* skipSpace)

loadFont :: FilePath -> IO (VM.IOVector Character)
loadFont path = do
  chars <- VM.new 256
  withFile path ReadMode $ go chars
  return chars
 where
  go chars handle = do
    isFileEnd <- hIsEOF handle
    unless isFileEnd $ do
      line <- BSC.hGetLine handle
      case parseCharacter line of
        Just ch -> VM.write chars (charId ch) ch
        Nothing -> pure ()
      go chars handle

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

linkShaders :: [GLuint] -> (GLuint -> IO ()) -> IO GLuint
linkShaders shaders bindAttributes = do
  program <- glCreateProgram
  traverse_ (glAttachShader program) shaders
  bindAttributes program
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

shaderHeader :: Int -> String
shaderHeader maxLights =
  "#version 330 core\n" ++ "#define NUM_LIGHTS " ++ show maxLights ++ "\n"

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
