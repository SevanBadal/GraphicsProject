module Main where
-------------------
-- Important References:
-- https://dpwright.com/posts/2015/03/25/the-haskell-gl-package/
-- https://lokathor.gitbooks.io/using-haskell/content/opengl/transformations.html
-- https://whatthefunctional.wordpress.com/2019/03/27/writing-a-ray-tracer-in-haskell-part-1/
-- https://books.google.com/books?id=xPu3mN2FPl4C&pg=PT195&lpg=PT195&dq=opengl+Icosahedron+vertices&source=bl&ots=mQdXanaaqq&sig=ACfU3U1kO7KQzRM3Gm1pzKr3klLsV5h8gg&hl=en&sa=X&ved=2ahUKEwiRobej1srqAhXbGc0KHc4kAaUQ6AEwDHoECAkQAQ#v=onepage&q=opengl%20Icosahedron%20vertices&f=false
-------------------
import ShaderLink
import IcoShader
import Mouse
import Camera
import Keys
import qualified Icosahedron as Ico
import Graphics.GL.Core33 -- ensure support for Version 330
import qualified Graphics.UI.GLFW as GLFW
import Foreign -- C lang bindings
import Foreign.C.String (withCAStringLen, newCString)
import Control.Monad (when, forM_)
import Control.Exception (bracket)
import Data.Bits
import Data.IORef
import Graphics.GL.Types
-- JuicyPixels for textures
import Codec.Picture (readImage, generateImage, convertRGB8, DynamicImage(..), Image(..), PixelRGB8(..))
import qualified Data.Vector.Storable as VS
import Linear -- Transformation and Projection support
import qualified Data.Set as S


windowWidth = 1000
windowHeight = 800


-- Control.Exception
-- run 'act' after init success
bracketGLFW :: IO () -> IO ()
bracketGLFW act = bracket GLFW.init (const GLFW.terminate) $ \initWorked ->
    when initWorked act

toViewMatrix :: Camera -> M44 GLfloat
toViewMatrix (Camera pos front up) = lookAt pos (pos ^+^ front) up

main :: IO ()
main = bracketGLFW $ do
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
    GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True); -- required on MacOS
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    maybeWindow <- GLFW.createWindow windowWidth windowHeight "Assignment 4" Nothing Nothing
    case maybeWindow of
        Nothing -> putStrLn "Failed to create a GLFW window!"
        Just window -> do
            -- enable keys
            keyRef <- newIORef S.empty
            GLFW.setKeyCallback window (Just $ keyCallback keyRef)
            -- configure the mouse
            mouseRef <- newIORef $ MouseInfo Nothing (0,(-90)) (V3 0 0 (-1))
            GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
            GLFW.setCursorPosCallback window (Just $ cursorPosCallback mouseRef)

            -- calibrate the viewport
            GLFW.makeContextCurrent (Just window)
            (x,y) <- GLFW.getFramebufferSize window
            glViewport 0 0 (fromIntegral x) (fromIntegral y)

            -- enable depth testing in our display
            glEnable GL_DEPTH_TEST

            -- ready Icosahedron Shader Program
            ico_eErrP <- programFromSources icoVertexShaderSource icoFragmentShaderSource
            ico_shaderProgram <- case ico_eErrP of
                Left e -> putStrLn e >> return 0
                Right p -> return p


            -- Load texture 1
            texture0P <- malloc
            glGenTextures 1 texture0P
            texture0 <- peek texture0P
            glBindTexture GL_TEXTURE_2D texture0
            -- wrapping and filtering params would go here.
            eErrDI0 <- readImage "app/numbers.png"
            dyImage0 <- case eErrDI0 of
                Left e -> do
                    putStrLn e
                    return $ ImageRGB8 $ generateImage (\x y ->
                        let x' = fromIntegral x in PixelRGB8 x' x' x') 500 400
                Right di -> return di
            let ipixelrgb80 = convertRGB8 dyImage0
                iWidth0 = fromIntegral $ imageWidth ipixelrgb80
                iHeight0 = fromIntegral $ imageHeight ipixelrgb80
                iData0 = imageData ipixelrgb80
            VS.unsafeWith iData0 $ \dataP ->
                glTexImage2D GL_TEXTURE_2D 0 GL_RGB iWidth0 iHeight0 0 GL_RGB GL_UNSIGNED_BYTE (castPtr dataP)
            glGenerateMipmap GL_TEXTURE_2D
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
            glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT

            glBindTexture GL_TEXTURE_2D 0

            -- setup Ico verticies
            let icoVerticiesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length Ico.verticies)
            icoVerticesP <- newArray Ico.verticies
            -- setup Ico indicies
            -- let ico_indicesSize = fromIntegral $ sizeOf (0::GLuint) * (length Ico.indicies)
            -- ico_indicesP <- newArray Ico.indicies

            -- Ico: setup vao
            ico_vaoP <- malloc
            glGenVertexArrays 1 ico_vaoP
            ico_vao <- peek ico_vaoP
            glBindVertexArray ico_vao
            -- Ico: vbo
            ico_vboP <- malloc
            glGenBuffers 1 ico_vboP
            ico_vbo <- peek ico_vboP
            glBindBuffer GL_ARRAY_BUFFER ico_vbo
            glBufferData GL_ARRAY_BUFFER icoVerticiesSize (castPtr icoVerticesP) GL_STATIC_DRAW
            -- Ico: element buffer object
            -- ico_eboP <- malloc
            -- glGenBuffers 1 ico_eboP
            -- ico_ebo <- peek ico_eboP
            -- glBindBuffer GL_ELEMENT_ARRAY_BUFFER ico_ebo
            -- glBufferData GL_ELEMENT_ARRAY_BUFFER ico_indicesSize (castPtr ico_indicesP) GL_STATIC_DRAW
            -- Ico: attrib pointer info
            let floatSize = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
            glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (5*floatSize) nullPtr
            glEnableVertexAttribArray 0
            -- Texture
            let threeFloatOffset = castPtr $ plusPtr nullPtr (fromIntegral $ 3*floatSize)
            glVertexAttribPointer 2 2 GL_FLOAT GL_FALSE (5*floatSize) threeFloatOffset
            glEnableVertexAttribArray 2
            glBindVertexArray 0
            -- Ico: color attribute
            -- let floatSize = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
            -- let ico_color_stride = castPtr $ plusPtr nullPtr (fromIntegral $ 3 * floatSize )
            -- glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE (6 * floatSize) ico_color_stride -- offset by 3 floats
            -- glEnableVertexAttribArray 1
            -- glBindVertexArray 0
            -- done with Ico

            -- Uniforms: init names
            ourColor <- newCString "ourColor"
            ourTexture0 <- newCString "ourTexture0"
            ourTexture1 <- newCString "ourTexture1"
            
            res <- newCString "res"
            model <- newCString "model"
            view <- newCString "view"
            projection <- newCString "projection"

            -- Uniforms: pointers to uniforms
            modelP <- malloc
            viewP <- malloc
            projP <- malloc

            -- DEBUG VERTS
            Ico.printVerticies Ico.int_indicies

            -- Main loop
            let loop lastFrame oldCamera = do
                    shouldContinue <- not <$> GLFW.windowShouldClose window
                    when shouldContinue $ do
                        -- event poll
                        GLFW.pollEvents
                        -- check time
                        timeValue <- maybe 0 realToFrac <$> GLFW.getTime
                        let deltaTime = timeValue - lastFrame
                        let cameraSpeed = 5 * deltaTime
                        -- read and use keys
                        keysDown <- readIORef keyRef
                        let cameraTemp = updateCamera keysDown cameraSpeed oldCamera
                        -- read and use mouse
                        mouseInfo <- readIORef mouseRef
                        let camera = cameraTemp{cameraFront = frontVec mouseInfo}
                        -- clear the screen
                        glClearColor 0.85 0.85 0.90 1.0
                        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
                        -- ICO
                        glUseProgram ico_shaderProgram
                        glBindVertexArray ico_vao
                        -- bind textures using texture units
                        glActiveTexture GL_TEXTURE0
                        glBindTexture GL_TEXTURE_2D texture0
                        our0Loc <- glGetUniformLocation ico_shaderProgram ourTexture0
                        glUniform1i our0Loc 0
                        -- UNIFORMS
                        resLoc <- glGetUniformLocation ico_shaderProgram res
                        modelLoc <- glGetUniformLocation ico_shaderProgram model
                        viewLoc <- glGetUniformLocation ico_shaderProgram view
                        projectionLoc <- glGetUniformLocation ico_shaderProgram projection

                        let viewMat = toViewMatrix camera
                        let screenWidthF = fromIntegral x :: GLfloat
                        let screenHeightF = fromIntegral y :: GLfloat
                        let projMat = perspective 45 (screenWidthF / screenHeightF) 0.1 100.0

                        poke viewP (transpose viewMat)
                        poke projP (transpose projMat)

                        glUniformMatrix4fv viewLoc 1 GL_FALSE (castPtr viewP)
                        glUniformMatrix4fv projectionLoc 1 GL_FALSE (castPtr projP)
                        forM_ (zip Ico.dice [0..]) $ \(die, i) -> do
                            let modelMat = mkTransformationMat identity die
                            poke modelP (transpose modelMat)
                            glUniformMatrix4fv modelLoc 1 GL_FALSE (castPtr modelP)
                            glUniform2f resLoc (screenWidthF) (screenHeightF)
                            glDrawArrays GL_TRIANGLES 0 60
                            -- glDrawElements GL_TRIANGLES 60 GL_UNSIGNED_INT nullPtr
                        glBindVertexArray 0
                        -- swap buffers and go again
                        GLFW.swapBuffers window
                        loop timeValue camera
            loop 0.0 (Camera (V3 0 0 3) (V3 0 0 (-1)) (V3 0 1 0))