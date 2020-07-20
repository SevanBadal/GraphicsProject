module Keys where
-- https://lokathor.gitbooks.io/using-haskell/content/opengl/camera.html
import qualified Graphics.UI.GLFW as GLFW
import Data.Set (Set)
import qualified Data.Set as S
import Data.IORef
import Control.Monad (when, forM_)
import qualified Data.Vector.Storable as VS

-- type KeyCallback = Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyCallback :: IORef (Set GLFW.Key) -> GLFW.KeyCallback
keyCallback ref window key scanCode keyState modKeys = do
    --putStrLn $ show keyState ++ " " ++ show key
    case keyState of
        GLFW.KeyState'Pressed -> modifyIORef ref (S.insert key)
        GLFW.KeyState'Released -> modifyIORef ref (S.delete  key)
        _ -> return ()
    when (key == GLFW.Key'Escape && keyState == GLFW.KeyState'Pressed)
        (GLFW.setWindowShouldClose window True)