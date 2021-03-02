{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Engine.Particle
  ( Program
  , Particles (..)
  , gravity
  , update
  , alive
  , mkParticles
  , mkProgram
  , prepare
  , setProj
  , setTexCoordInfo
  , setModelView
  , draw
  , unbind
  ) where

import Control.Exception (bracket)
import Control.Lens ((%~), (&), (^.), (.~))
import Data.Fixed (mod')
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Graphics.GL.Core45
import Graphics.GL.Types
import Engine.Types
import Engine.Utils (linkShaders, loadShader, loadTexture, loadVAO)
import Linear ((!*!), (^+^), (^*), _m33, _y)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as V
import qualified Linear
import qualified Text.RawString.QQ as QQ

vertexShaderSrc :: BS.ByteString
vertexShaderSrc = BS.pack
  [QQ.r|
    #version 330 core

    in vec2 position;
    out vec2 texCoord1;
    out vec2 texCoord2;
    out float blend;

    uniform mat4 projection;
    uniform mat4 modelView;

    uniform vec2 texOffset1;
    uniform vec2 texOffset2;
    uniform vec2 texCoordInfo;

    void main() {
      vec2 texCoord = position + vec2(0.5, 0.5);
      texCoord.y = 1.0 - texCoord.y;
      texCoord /= texCoordInfo.x;
      texCoord1 = texCoord + texOffset1;
      texCoord2 = texCoord + texOffset2;
      blend = texCoordInfo.y;

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

data Particles = Particles
  { particlesModel   :: {-# UNPACK #-} !RawModel
  , particlesTexture :: {-# UNPACK #-} !Texture
  }

mkParticles :: FilePath -> Int -> IO Particles
mkParticles path numRows = Particles
  <$> loadVAO vertices 2
  <*> ((\t -> t { textureNumRows = numRows }) <$> loadTexture path)

data Program = Program
  { program         :: {-# UNPACK #-} !GLuint
  , viewLoc         :: {-# UNPACK #-} !GLint
  , projLoc         :: {-# UNPACK #-} !GLint
  , texOffset1Loc   :: {-# UNPACK #-} !GLint
  , texOffset2Loc   :: {-# UNPACK #-} !GLint
  , texCoordInfoLoc :: {-# UNPACK #-} !GLint
  }

mkProgram :: IO Program
mkProgram = do
  program <-
    bracket loadVertexShader glDeleteShader $ \vertexShader ->
    bracket loadFragmentShader glDeleteShader $ \fragmentShader ->
      linkShaders [vertexShader, fragmentShader]

  viewLoc <- withCString "modelView" $ glGetUniformLocation program
  projLoc <- withCString "projection" $ glGetUniformLocation program
  texOffset1Loc <- withCString "texOffset1" $ glGetUniformLocation program
  texOffset2Loc <- withCString "texOffset2" $ glGetUniformLocation program
  texCoordInfoLoc <- withCString "texCoordInfo" $ glGetUniformLocation program
  return Program{..}
 where
  loadVertexShader = loadShader GL_VERTEX_SHADER vertexShaderSrc
  loadFragmentShader = loadShader GL_FRAGMENT_SHADER fragmentShaderSrc

prepare :: Program -> Particles -> IO ()
prepare p particles = do
  glUseProgram $ program p
  glBindVertexArray $ modelVao $ particlesModel particles
  glEnableVertexAttribArray 0
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D $ textureID $ particlesTexture particles
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE
  glDepthMask GL_FALSE

setProj :: Program -> Linear.M44 GLfloat -> IO ()
setProj p proj =
  with proj $ glUniformMatrix4fv (projLoc p) 1 GL_TRUE . castPtr

setTexCoordInfo :: Program -> Particles -> Particle -> IO ()
setTexCoordInfo p
    Particles { particlesTexture = Texture { textureNumRows = numRows } }
    Particle { particleTexOffset1 = Linear.V2 to1x to1y
             , particleTexOffset2 = Linear.V2 to2x to2y
             , particleBlend = blend } = do
  glUniform2f (texOffset1Loc p) to1x to1y
  glUniform2f (texOffset2Loc p) to2x to2y
  glUniform2f (texCoordInfoLoc p) (fromIntegral numRows) blend

setModelView :: Program -> Particle -> Linear.M44 GLfloat -> IO ()
setModelView p particle view =
  with modelView $ glUniformMatrix4fv (viewLoc p) 1 GL_TRUE . castPtr
 where modelView = modelViewMatrix particle view

modelViewMatrix :: Particle -> Linear.M44 GLfloat -> Linear.M44 GLfloat
modelViewMatrix p view = view Linear.!*! model''
 where
  rot = Linear.fromQuaternion $
    Linear.axisAngle (Linear.V3 0 0 1) (particleRotation p)
  scale = Linear.V3 (particleScale p) (particleScale p) (particleScale p)
  model = Linear.mkTransformationMat Linear.identity (particlePosition p)
  -- Set model rotation matrix to transpose of view matrix to "cancel" it out.
  model' = model & _m33 .~ Linear.transpose (view ^. _m33)
  -- Apply particle rotation and scale.
  model'' = model' & _m33 %~ ((^* scale) . (!*! rot))

draw :: Particles -> IO ()
draw = glDrawArrays GL_TRIANGLE_STRIP 0 . modelVertexCount . particlesModel

unbind :: IO ()
unbind = do
  glDepthMask GL_TRUE
  glDisable GL_BLEND
  glBindTexture GL_TEXTURE_2D 0
  glDisableVertexAttribArray 0
  glBindVertexArray 0
