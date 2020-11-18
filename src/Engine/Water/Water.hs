{-# LANGUAGE QuasiQuotes #-}
module Engine.Water.Water
  ( WaterProgram
  , Water
  , mkProgram
  , mkWater
  , use
  , setUniforms
  , drawTile
  ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Engine.Types
import Engine.Utils (linkShaders, loadShader, loadVAO)
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Graphics.GL.Core45
import Graphics.GL.Types
import Linear ((!!*))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as V
import qualified Linear
import qualified Text.RawString.QQ as QQ

waterTileSize :: GLfloat
waterTileSize = 20

vertexShaderSrc :: ByteString
vertexShaderSrc = BS.pack
  [QQ.r|
    #version 330 core
    in vec2 position;
    out vec2 texCoord;

    uniform mat4 model;
    uniform mat4 view;
    uniform mat4 projection;

    void main() {
      gl_Position = projection * view * model * vec4(position.x, 0.0, position.y, 1.0);
      texCoord = vec2(position.x / 2.0 + 0.5, position.y / 2.0 + 0.5);
    }
  |]

fragmentShaderSrc :: ByteString
fragmentShaderSrc = BS.pack
  [QQ.r|
    #version 330 core
    in vec2 texCoord;
    out vec4 color;

    void main() {
      color = vec4(0.0, 0.0, 1.0, 1.0);
    }
  |]

data WaterProgram = WaterProgram
  { wProgram  :: {-# UNPACK #-} !GLuint
  , wModelLoc :: {-# UNPACK #-} !GLint
  , wViewLoc  :: {-# UNPACK #-} !GLint
  , wProjLoc  :: {-# UNPACK #-} !GLint
  }

mkProgram :: IO WaterProgram
mkProgram = do
  program <-
    bracket loadVertexShader glDeleteShader $ \vertexShader ->
    bracket loadFragmentShader glDeleteShader $ \fragmentShader ->
      linkShaders [vertexShader, fragmentShader]
  modelLoc <- withCString "model" $ \name ->
    glGetUniformLocation program name
  viewLoc <- withCString "view" $ \name ->
    glGetUniformLocation program name
  projLoc <- withCString "projection" $ \name ->
    glGetUniformLocation program name
  return $ WaterProgram program modelLoc viewLoc projLoc
 where
  loadVertexShader = loadShader GL_VERTEX_SHADER vertexShaderSrc
  loadFragmentShader = loadShader GL_FRAGMENT_SHADER fragmentShaderSrc

use :: WaterProgram -> IO ()
use = glUseProgram . wProgram

setUniforms :: WaterProgram -> Linear.M44 GLfloat -> Linear.M44 GLfloat -> IO ()
setUniforms p view proj = do
  with view $ \matrixPtr ->
    glUniformMatrix4fv (wViewLoc p) 1 GL_TRUE (castPtr matrixPtr)
  with proj $ \matrixPtr ->
    glUniformMatrix4fv (wProjLoc p) 1 GL_TRUE (castPtr matrixPtr)

-- | Only the x and z coordinates because y is fixed to 0 in the shader.
waterVertices :: V.Vector GLfloat
waterVertices = V.fromList [-1, -1, -1, 1, 1, -1, 1, -1, -1, 1, 1, 1]

data Water = Water
  { waterRawModel :: {-# UNPACK #-} !RawModel
  }

mkWater :: IO Water
mkWater = Water <$> loadVAO waterVertices 2

drawTile :: Water -> WaterTile -> WaterProgram -> IO ()
drawTile w tile p = do
  with matrix $ \matrixPtr ->
    glUniformMatrix4fv (wModelLoc p) 1 GL_TRUE (castPtr matrixPtr)

  glBindVertexArray $ modelVao $ waterRawModel w
  glDrawArrays GL_TRIANGLES 0 $ modelVertexCount $ waterRawModel w
  glBindVertexArray 0
 where
  tileVec = Linear.V3 (tileX tile) (tileHeight tile) (tileZ tile)
  matrix =
    Linear.mkTransformationMat (Linear.identity !!* waterTileSize) tileVec
