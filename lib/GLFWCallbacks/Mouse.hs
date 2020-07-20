module Mouse where
-- https://lokathor.gitbooks.io/using-haskell/content/opengl/camera.html
import Linear
import Graphics.GL.Types
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Data.IORef
import Data.Fixed (mod')

data MouseInfo = MouseInfo {
    lastXY :: Maybe (Double,Double),
    oldPitchYaw :: (Double, Double),
    frontVec :: V3 GLfloat
} deriving Show

cursorPosCallback :: IORef MouseInfo -> GLFW.CursorPosCallback
cursorPosCallback ref window xpos ypos = do
    modifyIORef ref $ \oldInfo -> let
        (lastX, lastY) = case lastXY oldInfo of
            Nothing -> (xpos,ypos)
            (Just (lastX,lastY)) -> (lastX,lastY)
        sensitivity = 0.05
        xoffset = (xpos - lastX) * sensitivity
        yoffset = (lastY - ypos) * sensitivity
        lastX' = xpos
        lastY' = ypos
        (oldPitch,oldYaw) = oldPitchYaw oldInfo
        newYaw = (oldYaw + xoffset) `mod'` 360.0
        newPitch = min (max (oldPitch + yoffset) (-89)) 89
        toRadians = realToFrac . (*(pi/180)) :: Double -> GLfloat
        pitchR = toRadians newPitch
        yawR = toRadians newYaw
        front = normalize $ V3 (cos yawR * cos pitchR) (sin pitchR) (sin yawR * cos pitchR)
        in MouseInfo (Just (lastX',lastY')) (newPitch, newYaw) front
