module Engine.MousePicker (calculateMouseRay) where

import Control.Lens ((^.))
import Graphics.GL.Types
import Linear ((!*))
import qualified Linear

calculateMouseRay
  :: GLfloat            -- ^ Mouse x
  -> GLfloat            -- ^ Mouse y
  -> GLfloat            -- ^ Display width
  -> GLfloat            -- ^ Display height
  -> Linear.M44 GLfloat -- ^ Projection matrix
  -> Linear.M44 GLfloat -- ^ View matrix
  -> Linear.V3 GLfloat
calculateMouseRay mouseX mouseY displayWidth displayHeight proj view =
  Linear.normalize rayWor
 where
  rayNDC = normalizedDeviceCoords mouseX mouseY displayWidth displayHeight
  rayClip = Linear.V4 (rayNDC ^. Linear._x) (rayNDC ^. Linear._y) (-1) 1
  rayEye = Linear.inv44 proj !* rayClip
  rayEye' = Linear.V4 (rayEye ^. Linear._x) (rayEye ^. Linear._y) (-1) 0
  rayWor = (Linear.inv44 view !* rayEye') ^. Linear._xyz

normalizedDeviceCoords
  :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Linear.V2 GLfloat
normalizedDeviceCoords mouseX mouseY displayWidth displayHeight = Linear.V2
  ((2 * mouseX) / displayWidth - 1)
  (1 - ((2 * mouseY) / displayHeight))
