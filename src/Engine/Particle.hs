{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Engine.Particle
  ( Program
  , Particles (particlesTexture)
  , gravity
  , update
  , alive
  , mkParticles
  , mkProgram
  , prepare
  , setUniforms
  , fillBuffer
  , updateVBO
  , draw
  , unbind
  ) where

import Control.Exception (bracket)
import Control.Lens ((%~), (&), (^.), (.~))
import Data.Fixed (mod')
import Data.Foldable (foldlM)
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Graphics.GL.Core45
import Graphics.GL.Types
import Engine.Types
import Linear ((!*!), (^+^), (^*), _m33, _y)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import qualified Engine.Utils as Utils
import qualified Linear
import qualified Text.RawString.QQ as QQ

vertexShaderSrc :: BS.ByteString
vertexShaderSrc = BS.pack
  [QQ.r|
    #version 330 core

    in vec2 position;
    in mat4 modelView;
    in vec4 texOffsets;
    in float blendFactor;

    out vec2 texCoord1;
    out vec2 texCoord2;
    out float blend;

    uniform mat4 projection;
    uniform float numberOfRows;

    void main() {
      vec2 texCoord = position + vec2(0.5, 0.5);
      texCoord.y = 1.0 - texCoord.y;
      texCoord /= numberOfRows;
      texCoord1 = texCoord + texOffsets.xy;
      texCoord2 = texCoord + texOffsets.zw;
      blend = blendFactor;

      gl_Position = projection * modelView * vec4(position, 0.0, 1.0);
    }
  |]

fragmentShaderSrc :: BS.ByteString
fragmentShaderSrc = BS.pack
  [QQ.r|
    #version 330 core
    in vec2 texCoord1;
    in vec2 texCoord2;
    in float blend;
    out vec4 color;

    uniform sampler2D tex;

    void main() {
      vec4 color1 = texture(tex, texCoord1);
      vec4 color2 = texture(tex, texCoord2);
      color = mix(color1, color2, blend);
    }
  |]

gravity :: GLfloat
gravity = -50

update :: GLfloat -> Texture -> Particle -> Particle
update elapsed tex p = (updateTexCoordInfo tex p)
  { particleElapsed  = particleElapsed p + elapsed
  , particlePosition = particlePosition p ^+^ change
  , particleVelocity = particleVelocity p & _y %~ updateY
  }
 where
  change = particleVelocity p ^* elapsed
  updateY = (+ gravity * particleGravityEffect p * elapsed)

updateTexCoordInfo :: Texture -> Particle -> Particle
updateTexCoordInfo tex p = p
  { particleBlend      = atlasProgression `mod'` 1
  , particleTexOffset1 = texOffset index1
  , particleTexOffset2 = texOffset index2
  }
 where
  lifeFactor = particleElapsed p / particleLife p
  stageCount = textureNumRows tex * textureNumRows tex
  atlasProgression = lifeFactor * fromIntegral stageCount
  index1 = floor atlasProgression
  index2 = if index1 < stageCount - 1 then index1 + 1 else index1

  texOffset i = Linear.V2
    (fromIntegral column / fromIntegral (textureNumRows tex))
    (fromIntegral row / fromIntegral (textureNumRows tex))
   where
    column = i `rem` textureNumRows tex
    row = i `div` textureNumRows tex

alive :: Particle -> Bool
alive p = particleElapsed p < particleLife p

vertices :: V.Vector GLfloat
vertices = V.fromList [-0.5, 0.5, -0.5, -0.5, 0.5, 0.5, 0.5, -0.5]

maxInstances :: Int
maxInstances = 10000

instanceDataLength :: GLsizei
instanceDataLength = 21

data Particles = Particles
  { particlesModel   :: {-# UNPACK #-} !RawModel
  , particlesVBO     :: {-# UNPACK #-} !GLuint
  , particlesTexture :: {-# UNPACK #-} !Texture
  , particlesBuffer  :: {-# UNPACK #-} !(VM.IOVector GLfloat)
  }

mkParticles :: FilePath -> Int -> IO Particles
mkParticles path numRows = do
  texture <- (\t -> t { textureNumRows = numRows }) <$> Utils.loadTexture path
  rawModel <- Utils.loadVAO vertices 2
  glBindVertexArray $ modelVao rawModel
  vbo <- Utils.loadInstancedVBO instanceDataLength maxInstances
  glBindVertexArray 0
  buf <- VM.new $ fromIntegral instanceDataLength * fromIntegral maxInstances
  return $ Particles rawModel vbo texture buf

data Program = Program
  { program         :: {-# UNPACK #-} !GLuint
  , projLoc         :: {-# UNPACK #-} !GLint
  , numRowsLoc      :: {-# UNPACK #-} !GLint
  }

mkProgram :: IO Program
mkProgram = do
  program <-
    bracket loadVertexShader glDeleteShader $ \vertexShader ->
    bracket loadFragmentShader glDeleteShader $ \fragmentShader ->
      Utils.linkShaders [vertexShader, fragmentShader] bindAttributes

  projLoc <- withCString "projection" $ glGetUniformLocation program
  numRowsLoc <- withCString "numberOfRows" $ glGetUniformLocation program
  return Program{..}
 where
  loadVertexShader = Utils.loadShader GL_VERTEX_SHADER vertexShaderSrc
  loadFragmentShader = Utils.loadShader GL_FRAGMENT_SHADER fragmentShaderSrc
  bindAttributes program = do
    withCString "position" $ glBindAttribLocation program 0
    withCString "modelView" $ glBindAttribLocation program 1
    withCString "texOffsets" $ glBindAttribLocation program 5
    withCString "blendFactor" $ glBindAttribLocation program 6

prepare :: Program -> Particles -> IO ()
prepare p particles = do
  glUseProgram $ program p
  glBindVertexArray $ modelVao $ particlesModel particles
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D $ textureID $ particlesTexture particles
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE
  glDepthMask GL_FALSE

setUniforms :: Program -> Linear.M44 GLfloat -> GLfloat -> IO ()
setUniforms p proj numRows = do
  glUniform1f (numRowsLoc p) numRows
  with proj $ glUniformMatrix4fv (projLoc p) 1 GL_TRUE . castPtr

modelViewMatrix :: Particle -> Linear.M44 GLfloat -> Linear.M44 GLfloat
modelViewMatrix p view = view !*! model''
 where
  rot = Linear.fromQuaternion $
    Linear.axisAngle (Linear.V3 0 0 1) (particleRotation p)
  scale = Linear.V3 (particleScale p) (particleScale p) (particleScale p)
  model = Linear.mkTransformationMat Linear.identity (particlePosition p)
  -- Set model rotation matrix to transpose of view matrix to "cancel" it out.
  model' = model & _m33 .~ Linear.transpose (view ^. _m33)
  -- Apply particle rotation and scale.
  model'' = model' & _m33 %~ ((^* scale) . (!*! rot))

fillBuffer
  :: Particles -> Particle -> Linear.M44 GLfloat -> Int -> IO Int
fillBuffer p particle view size0 = do
  size <- foldlM fillRow size0 modelView
  size' <- fillOffset size (particleTexOffset1 particle)
  size'' <- fillOffset size' (particleTexOffset2 particle)
  VM.write (particlesBuffer p) size'' (particleBlend particle)
  return $ size'' + 1
 where
  modelView = Linear.transpose $ modelViewMatrix particle view

  fillRow :: Int -> Linear.V4 GLfloat -> IO Int
  fillRow size (Linear.V4 x y z w) = do
    VM.write (particlesBuffer p) size x
    VM.write (particlesBuffer p) (size + 1) y
    VM.write (particlesBuffer p) (size + 2) z
    VM.write (particlesBuffer p) (size + 3) w
    return $ size + 4

  fillOffset :: Int -> Linear.V2 GLfloat -> IO Int
  fillOffset size (Linear.V2 x y) = do
    VM.write (particlesBuffer p) size x
    VM.write (particlesBuffer p) (size + 1) y
    return $ size + 2

updateVBO :: Particles -> IO ()
updateVBO = Utils.updateVBO <$> particlesVBO <*> particlesBuffer

draw :: Particles -> GLsizei -> IO ()
draw p = glDrawArraysInstanced GL_TRIANGLE_STRIP 0 vertexCount
       . (`quot` instanceDataLength)
 where vertexCount = modelVertexCount $ particlesModel p

unbind :: IO ()
unbind = do
  glDepthMask GL_TRUE
  glDisable GL_BLEND
  glBindTexture GL_TEXTURE_2D 0
  glBindVertexArray 0
