module Camera where

import Graphics.GL.Types
import Data.Set (Set)
import Linear
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.Set as S

data Camera = Camera {
    cameraPos :: V3 GLfloat,
    cameraFront :: V3 GLfloat,
    cameraUp :: V3 GLfloat
} deriving Show

-- uses normalized movement
updateCamera :: Set GLFW.Key -> GLfloat -> Camera -> Camera
updateCamera keySet speed cam@(Camera pos front up) = let
    moveVector = S.foldr (\key vec -> case key of
            GLFW.Key'W -> vec ^+^ front
            GLFW.Key'S -> vec ^-^ front
            GLFW.Key'A -> vec ^-^ normalize (cross front up)
            GLFW.Key'D -> vec ^+^ normalize (cross front up)
            _ -> vec
            ) (V3 0 0 0) keySet
    in cam {cameraPos = pos ^+^ (speed *^ normalize moveVector)}

getCameraPos :: Camera -> V3 GLfloat
getCameraPos (Camera pos _ _) = pos