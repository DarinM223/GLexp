module Engine.Water.FrameBuffers
  ( FrameBuffers
    ( reflectionTexture
    , reflectionDepthBuffer
    , refractionTexture
    , refractionDepthTexture
    )
  , init
  , delete
  , bindReflectionFrameBuffer
  , bindRefractionFrameBuffer
  , unbindFrameBuffer
  ) where

import Prelude hiding (init)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import Graphics.GL.Core45
import Graphics.GL.Types

reflectionWidth :: GLsizei
reflectionWidth = 320

reflectionHeight :: GLsizei
reflectionHeight = 180

refractionWidth :: GLsizei
refractionWidth = 1280

refractionHeight :: GLsizei
refractionHeight = 720

data FrameBuffers = FrameBuffers
  { width                  :: {-# UNPACK #-} !GLsizei
  , height                 :: {-# UNPACK #-} !GLsizei
  , reflectionFrameBuffer  :: {-# UNPACK #-} !GLuint
  , reflectionTexture      :: {-# UNPACK #-} !GLuint
  , reflectionDepthBuffer  :: {-# UNPACK #-} !GLuint
  , refractionFrameBuffer  :: {-# UNPACK #-} !GLuint
  , refractionTexture      :: {-# UNPACK #-} !GLuint
  , refractionDepthTexture :: {-# UNPACK #-} !GLuint
  }

init :: GLsizei -> GLsizei -> IO FrameBuffers
init w h = FrameBuffers w h
  <$> mkFrameBuffer
  <*> mkTextureAttachment reflectionWidth reflectionHeight
  <*> mkDepthBufferAttachment reflectionWidth reflectionHeight
  <*  unbindFrameBuffer' w h
  <*> mkFrameBuffer
  <*> mkTextureAttachment refractionWidth refractionHeight
  <*> mkDepthTextureAttachment refractionWidth refractionHeight
  <*  unbindFrameBuffer' w h

delete :: FrameBuffers -> IO ()
delete bufs = do
  with (reflectionFrameBuffer bufs)  $ glDeleteFramebuffers 1
  with (reflectionTexture bufs)      $ glDeleteTextures 1
  with (reflectionDepthBuffer bufs)  $ glDeleteRenderbuffers 1
  with (refractionFrameBuffer bufs)  $ glDeleteFramebuffers 1
  with (refractionTexture bufs)      $ glDeleteTextures 1
  with (refractionDepthTexture bufs) $ glDeleteTextures 1

bindFrameBuffer :: GLuint -> GLsizei -> GLsizei -> IO ()
bindFrameBuffer frameBuffer w h = do
  glBindTexture GL_TEXTURE_2D 0
  glBindFramebuffer GL_FRAMEBUFFER frameBuffer
  glViewport 0 0 w h

bindReflectionFrameBuffer :: FrameBuffers -> IO ()
bindReflectionFrameBuffer bufs = bindFrameBuffer
  (reflectionFrameBuffer bufs) reflectionWidth reflectionHeight

bindRefractionFrameBuffer :: FrameBuffers -> IO ()
bindRefractionFrameBuffer bufs = bindFrameBuffer
  (refractionFrameBuffer bufs) refractionWidth refractionHeight

unbindFrameBuffer' :: GLsizei -> GLsizei -> IO ()
unbindFrameBuffer' w h = do
  glBindFramebuffer GL_FRAMEBUFFER 0
  glViewport 0 0 w h

unbindFrameBuffer :: FrameBuffers -> IO ()
unbindFrameBuffer bufs = unbindFrameBuffer' (width bufs) (height bufs)

mkFrameBuffer :: IO GLuint
mkFrameBuffer = do
  frameBuffer <- alloca $ \frameBufferPtr ->
    glGenFramebuffers 1 frameBufferPtr >> peek frameBufferPtr
  glBindFramebuffer GL_FRAMEBUFFER frameBuffer
  glDrawBuffer GL_COLOR_ATTACHMENT0
  return frameBuffer

mkTextureAttachment :: GLsizei -> GLsizei -> IO GLuint
mkTextureAttachment w h = do
  texture <- alloca $ \texturePtr ->
    glGenTextures 1 texturePtr >> peek texturePtr
  glBindTexture GL_TEXTURE_2D texture
  glTexImage2D GL_TEXTURE_2D 0 GL_RGB w h 0 GL_RGB GL_UNSIGNED_BYTE nullPtr
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  glFramebufferTexture GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 texture 0
  return texture

mkDepthTextureAttachment :: GLsizei -> GLsizei -> IO GLuint
mkDepthTextureAttachment w h = do
  texture <- alloca $ \texturePtr ->
    glGenTextures 1 texturePtr >> peek texturePtr
  glBindTexture GL_TEXTURE_2D texture
  glTexImage2D GL_TEXTURE_2D
    0 GL_DEPTH_COMPONENT32 w h 0 GL_DEPTH_COMPONENT GL_FLOAT nullPtr
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  glFramebufferTexture GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT texture 0
  return texture

mkDepthBufferAttachment :: GLsizei -> GLsizei -> IO GLuint
mkDepthBufferAttachment w h = do
  depthBuffer <- alloca $ \depthBufferPtr ->
    glGenRenderbuffers 1 depthBufferPtr >> peek depthBufferPtr
  glBindRenderbuffer GL_RENDERBUFFER depthBuffer
  glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT w h
  glFramebufferRenderbuffer
    GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER depthBuffer
  return depthBuffer
