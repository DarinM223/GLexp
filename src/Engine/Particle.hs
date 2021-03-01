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
  , setModelView
  , draw
  , unbind
  ) where

import Control.Exception (bracket)
import Control.Lens ((%~), (&), (^.), (.~))
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Graphics.GL.Core45
import Graphics.GL.Types
import Engine.Types
import Engine.Utils (linkShaders, loadShader, loadTexture, loadVAO)
import Linear
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as V
import qualified Text.RawString.QQ as QQ

vertexShaderSrc :: BS.ByteString
vertexShaderSrc = BS.pack
  [QQ.r|
    #version 330 core

    in vec2 position;
    out vec2 texCoord;

    uniform mat4 projection;
    uniform mat4 modelView;

    void main() {
      texCoord = position + vec2(0.5, 0.5);
      texCoord.y = 1.0 - texCoord.y;
      gl_Position = projection * modelView * vec4(position, 0.0, 1.0);
    }
  |]

fragmentShaderSrc :: BS.ByteString
fragmentShaderSrc = BS.pack
  [QQ.r|
    #version 330 core
    in vec2 texCoord;
    out vec4 color;

    uniform sampler2D tex;

    void main () {
      color = texture(tex, texCoord);
    }
  |]

gravity :: GLfloat
gravity = -50

update :: GLfloat -> Particle -> Particle
update elapsed p = p
  { particleElapsed  = particleElapsed p + elapsed
  , particlePosition = particlePosition p ^+^ change
  , particleVelocity = particleVelocity p & _y %~ updateY
  }
 where
  change = particleVelocity p ^* elapsed
  updateY = (+ gravity * particleGravityEffect p * elapsed)

alive :: Particle -> Bool
alive p = particleElapsed p < particleLife p

vertices :: V.Vector GLfloat
vertices = V.fromList [-0.5, 0.5, -0.5, -0.5, 0.5, 0.5, 0.5, -0.5]

data Particles = Particles
  { particlesModel   :: {-# UNPACK #-} !RawModel
  , particlesTexture :: {-# UNPACK #-} !Texture
  }

mkParticles :: FilePath -> IO Particles
mkParticles path = Particles <$> loadVAO vertices 2 <*> loadTexture path

data Program = Program
  { program :: {-# UNPACK #-} !GLuint
  , viewLoc :: {-# UNPACK #-} !GLint
  , projLoc :: {-# UNPACK #-} !GLint
  }

mkProgram :: IO Program
mkProgram = do
  program <-
    bracket loadVertexShader glDeleteShader $ \vertexShader ->
    bracket loadFragmentShader glDeleteShader $ \fragmentShader ->
      linkShaders [vertexShader, fragmentShader]

  viewLoc <- withCString "modelView" $ glGetUniformLocation program
  projLoc <- withCString "projection" $ glGetUniformLocation program
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

setProj :: Program -> M44 GLfloat -> IO ()
setProj p proj =
  with proj $ glUniformMatrix4fv (projLoc p) 1 GL_TRUE . castPtr

setModelView :: Program -> Particle -> M44 GLfloat -> IO ()
setModelView p particle view =
  with modelView $ glUniformMatrix4fv (viewLoc p) 1 GL_TRUE . castPtr
 where modelView = modelViewMatrix particle view

modelViewMatrix :: Particle -> M44 GLfloat -> M44 GLfloat
modelViewMatrix p view = view !*! model''
 where
  rot = fromQuaternion $ axisAngle (V3 0 0 1) (particleRotation p)
  scale = V3 (particleScale p) (particleScale p) (particleScale p)
  model = mkTransformationMat identity (particlePosition p)
  -- Set model rotation matrix to transpose of view matrix to "cancel" it out.
  model' = model & _m33 .~ transpose (view ^. _m33)
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
