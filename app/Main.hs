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
import Texture
import Control.Lens
import qualified Light as Light
import qualified Card as Card
import LightShader
import CardShader
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
import Codec.Picture (readImage, generateImage, convertRGBA8, convertRGB8, DynamicImage(..), Image(..), PixelRGB8(..), PixelRGBA8(..))
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

getCameraPos :: Camera -> V3 GLfloat
getCameraPos (Camera pos _ _) = pos

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
            glEnable GL_DEPTH_TEST

            -- ready Icosahedron Shader Program
            ico_eErrP <- programFromSources icoVertexShaderSource icoFragmentShaderSource
            ico_shaderProgram <- case ico_eErrP of
                Left e -> putStrLn e >> return 0
                Right p -> return p
            
            -- ready Light Shader Program
            light_eErrP <- programFromSources lightVertexShaderSource lightFragmentShaderSource
            light_shaderProgram <- case light_eErrP of
                Left e -> putStrLn e >> return 0
                Right p -> return p

            -- ready card Shader Program
            card_eErrP <- programFromSources cardVertexShaderSource cardFragmentShaderSource
            card_shaderProgram <- case card_eErrP of
                Left e -> putStrLn e >> return 0
                Right p -> return p
------------------------------------------------------------------------------------------------------
--          Setup Textures
------------------------------------------------------------------------------------------------------
--          ICO texture
            dice_textureP <- malloc
            glGenTextures 1 dice_textureP
            diceText <- peek dice_textureP
            glBindTexture GL_TEXTURE_2D diceText
            numberTextureImage <- loadImageTexture "app/numbers.png"
            VS.unsafeWith (imageData numberTextureImage) $ \dataP ->
                glTexImage2D GL_TEXTURE_2D 0 GL_RGBA (fromIntegral $ imageWidth numberTextureImage) (fromIntegral $ imageHeight numberTextureImage) 0 GL_RGBA GL_UNSIGNED_BYTE (castPtr dataP)
            glGenerateMipmap GL_TEXTURE_2D
            glBindTexture GL_TEXTURE_2D 0
------------------------------------------------------------------------------------------------------
----------- Card One texture
            card_oneP <- malloc
            glGenTextures 1 card_oneP
            card_oneTexture <- peek card_oneP
            glBindTexture GL_TEXTURE_2D card_oneTexture
            cardOneTextureImage <- loadImageTexture "app/card_gloss.png"
            VS.unsafeWith (imageData cardOneTextureImage) $ \dataP ->
                glTexImage2D GL_TEXTURE_2D 0 GL_RGBA (fromIntegral $ imageWidth cardOneTextureImage) (fromIntegral $ imageHeight cardOneTextureImage) 0 GL_RGBA GL_UNSIGNED_BYTE (castPtr dataP)
            glGenerateMipmap GL_TEXTURE_2D
            glBindTexture GL_TEXTURE_2D 0
----------- Card Two Texture
            card_twoP <- malloc
            glGenTextures 1 card_twoP
            card_twoTexture <- peek card_twoP
            glBindTexture GL_TEXTURE_2D card_twoTexture
            cardTwoTextureImage <- loadImageTexture "app/card_image.png"
            VS.unsafeWith (imageData cardTwoTextureImage) $ \dataP ->
                glTexImage2D GL_TEXTURE_2D 0 GL_RGBA (fromIntegral $ imageWidth cardTwoTextureImage) (fromIntegral $ imageHeight cardTwoTextureImage) 0 GL_RGBA GL_UNSIGNED_BYTE (castPtr dataP)
            glGenerateMipmap GL_TEXTURE_2D
            glBindTexture GL_TEXTURE_2D 0
------------------------------------------------------------------------------------------------------
--          End Texture Setup
------------------------------------------------------------------------------------------------------
--          Setup Object Verts
------------------------------------------------------------------------------------------------------
            let floatSize = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
            let threeFloatOffset = castPtr $ plusPtr nullPtr (fromIntegral $ 3*floatSize)

            -- setup Ico verticies
            let icoVerticiesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length Ico.verticies)
            icoVerticesP <- newArray Ico.verticies
            -- setup Ico indicies
            -- let ico_indicesSize = fromIntegral $ sizeOf (0::GLuint) * (length Ico.indicies)
            -- ico_indicesP <- newArray Ico.indicies

            -- Setup Card verticies
            let cardVerticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length Card.verticies)
            cardVerticesP <- newArray Card.verticies

            -- Setup Light verticies
            let lightVerticesSize = fromIntegral $ sizeOf (0.0 :: GLfloat) * (length Light.verticies)
            lightVerticesP <- newArray Light.verticies
------------------------------------------------------------------------------------------------------
--          Setup Object VAOs and VBOs
------------------------------------------------------------------------------------------------------
--          Dice: VAO
            ico_vaoP <- malloc
            glGenVertexArrays 1 ico_vaoP
            ico_vao <- peek ico_vaoP
            glBindVertexArray ico_vao
--          Dice: VBO
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

--          Dice: Attributes
            glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (8*floatSize) nullPtr
            glEnableVertexAttribArray 0
--          Dice: Normals
            let fiveFloatOffset = castPtr $ plusPtr nullPtr (fromIntegral $ 5*floatSize)
            glVertexAttribPointer 1 3 GL_FLOAT GL_TRUE (8*floatSize) fiveFloatOffset
            glEnableVertexAttribArray 1
--          Dice: Texture
            glVertexAttribPointer 2 2 GL_FLOAT GL_FALSE (8*floatSize) threeFloatOffset
            glEnableVertexAttribArray 2
            glBindVertexArray 0

--          Light: VAO
            light_vaoP <- malloc
            glGenVertexArrays 1 light_vaoP
            light_vao <- peek light_vaoP
            glBindVertexArray light_vao
--          Light: VBO
            light_vboP <- malloc
            glGenBuffers 1 light_vboP
            light_vbo <- peek light_vboP
            glBindBuffer GL_ARRAY_BUFFER light_vbo
            glBufferData GL_ARRAY_BUFFER lightVerticesSize (castPtr lightVerticesP) GL_STATIC_DRAW
--          Light: Attributes
            glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (3*floatSize) nullPtr 
            glEnableVertexAttribArray 0
            glBindVertexArray 0

--          Card: VAO
            card_vaoP <- malloc
            glGenVertexArrays 1 card_vaoP
            card_vao <- peek card_vaoP
            glBindVertexArray card_vao            
--          Card: VBO
            card_vboP <- malloc
            glGenBuffers 1 card_vboP
            card_vbo <- peek card_vboP
            glBindBuffer GL_ARRAY_BUFFER card_vbo
            glBufferData GL_ARRAY_BUFFER cardVerticesSize (castPtr cardVerticesP) GL_STATIC_DRAW
--          Card: Attributes
            glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (8*floatSize) nullPtr 
            glEnableVertexAttribArray 0
            let threeFloatOffset = castPtr $ plusPtr nullPtr (fromIntegral $ 3*floatSize)
            glVertexAttribPointer 1 2 GL_FLOAT GL_FALSE (8*floatSize) threeFloatOffset
            glEnableVertexAttribArray 1
--          Card: Normals
            let fiveFloatOffset = castPtr $ plusPtr nullPtr (fromIntegral $ 5*floatSize)
            glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE (8*floatSize) fiveFloatOffset
            glEnableVertexAttribArray 2        
            glBindVertexArray 0
------------------------------------------------------------------------------------------------------
--          Declare Uniforms
------------------------------------------------------------------------------------------------------
            -- Uniforms: init names
            diceTexture <- newCString "diceTexture"
            cardTexture <- newCString "cardTexture"

            objectColor <- newCString "objectColor"
            lightColor <- newCString "lightColor"
            
            lightPos <- newCString "lightPos"
            viewPos <- newCString "viewPos"

            res <- newCString "res"
            model <- newCString "model"
            view <- newCString "view"
            projection <- newCString "projection"

            -- Uniforms: pointers to uniforms
            modelP <- malloc
            viewP <- malloc
            projP <- malloc
       
            -- DEBUG VERTS
            -- Ico.printVerticies Ico.int_indicies
------------------------------------------------------------------------------------------------------
--          Enter Render Loop
------------------------------------------------------------------------------------------------------
            -- Main loop
            let loop lastFrame oldCamera = do
                    shouldContinue <- not <$> GLFW.windowShouldClose window
                    when shouldContinue $ do
                        GLFW.pollEvents
                        timeValue <- maybe 0 realToFrac <$> GLFW.getTime
                        let deltaTime = timeValue - lastFrame
                        let cameraSpeed = 5 * deltaTime
                        keysDown <- readIORef keyRef
                        let cameraTemp = updateCamera keysDown cameraSpeed oldCamera
                        mouseInfo <- readIORef mouseRef
                        let camera = cameraTemp{cameraFront = frontVec mouseInfo}
                        -- clear the screen
                        glClearColor 0.85 0.85 0.90 1.0
                        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
                        let lp = Light.light_position
                        let viewMat = toViewMatrix camera
                        let screenWidthF = fromIntegral x :: GLfloat
                        let screenHeightF = fromIntegral y :: GLfloat
                        let projMat = perspective 45 (screenWidthF / screenHeightF) 0.1 100.0
                        let lp = Light.light_position  
------------------------------------------------------------------------------------------------------
--                      Draw Dice
------------------------------------------------------------------------------------------------------
                        -- ICO
                        glUseProgram ico_shaderProgram
                        glBindVertexArray ico_vao
                        
                        -- bind textures using texture units
                        glActiveTexture GL_TEXTURE0
                        glBindTexture GL_TEXTURE_2D diceText
                        diceTextureLocation <- glGetUniformLocation ico_shaderProgram diceTexture
                        glUniform1i diceTextureLocation 0
                        
                        -- UNIFORMS
                        resLoc <- glGetUniformLocation ico_shaderProgram res
                        modelLoc <- glGetUniformLocation ico_shaderProgram model
                        viewLoc <- glGetUniformLocation ico_shaderProgram view
                        projectionLoc <- glGetUniformLocation ico_shaderProgram projection

                        objectColorLoc <- glGetUniformLocation ico_shaderProgram objectColor
                        lightColorLoc <- glGetUniformLocation ico_shaderProgram lightColor
                        lightPosLoc <- glGetUniformLocation ico_shaderProgram lightPos
                        viewPosLoc <- glGetUniformLocation ico_shaderProgram viewPos

                        poke viewP (transpose viewMat)
                        poke projP (transpose projMat)
                        glUniformMatrix4fv viewLoc 1 GL_FALSE (castPtr viewP)
                        glUniformMatrix4fv projectionLoc 1 GL_FALSE (castPtr projP)
                        forM_ (zip Ico.dice [0..]) $ \(die, i) -> do
                            let angle = 100 * i
                            let modelMat = mkTransformation (axisAngle (V3 (1.0::GLfloat) 0.5 0.5) ((sin timeValue))) die
                            -- let modelMat = mkTransformationMat identity die
                            poke modelP (transpose modelMat)
                            glUniform3f objectColorLoc (0.35::GLfloat) (0.35::GLfloat) (0.35::GLfloat)
                            glUniform3f lightColorLoc (1.0::GLfloat) (1.0::GLfloat) (1.0::GLfloat)
                            glUniform3f lightPosLoc (lp^._x) (lp^._y) (lp^._z)
                            glUniform3f viewPosLoc ((getCameraPos camera)^._x) ((getCameraPos camera)^._y) ((getCameraPos camera)^._z)
                            glUniformMatrix4fv modelLoc 1 GL_FALSE (castPtr modelP)
                            glUniform2f resLoc (screenWidthF) (screenHeightF)
                            glDrawArrays GL_TRIANGLES 0 60
                        glBindVertexArray 0
------------------------------------------------------------------------------------------------------
--                      Draw Light Source Object
------------------------------------------------------------------------------------------------------
                        -- Light
                        glUseProgram light_shaderProgram
                        glBindVertexArray light_vao
                        
                         -- UNIFORMS
                        resLoc <- glGetUniformLocation light_shaderProgram res
                        modelLoc <- glGetUniformLocation light_shaderProgram model
                        viewLoc <- glGetUniformLocation light_shaderProgram view
                        projectionLoc <- glGetUniformLocation light_shaderProgram projection
                        
                        poke viewP (transpose viewMat)
                        poke projP (transpose projMat)
                        
                        glUniformMatrix4fv viewLoc 1 GL_FALSE (castPtr viewP)
                        glUniformMatrix4fv projectionLoc 1 GL_FALSE (castPtr projP)
                        
                        forM_ (zip Light.lights [0..]) $ \(light, i) -> do
                            let modelMat = mkTransformationMat identity light
                            poke modelP (transpose modelMat)
                            glUniformMatrix4fv modelLoc 1 GL_FALSE (castPtr modelP)
                            glDrawArrays GL_TRIANGLES 0 36
                        glBindVertexArray 0
                        glBindTexture GL_TEXTURE_2D 0
------------------------------------------------------------------------------------------------------
--                      Draw Cards
------------------------------------------------------------------------------------------------------
                        -- Card
                        glUseProgram card_shaderProgram
                        glBindVertexArray card_vao
                        -- Bind Card Texture
                        glBindTexture GL_TEXTURE_2D card_oneTexture
                        glActiveTexture GL_TEXTURE0
                        cardTextureLocation <- glGetUniformLocation card_shaderProgram cardTexture
                        glUniform1i cardTextureLocation 0

                        resLoc <- glGetUniformLocation card_shaderProgram res
                        modelLoc <- glGetUniformLocation card_shaderProgram model
                        viewLoc <- glGetUniformLocation card_shaderProgram view
                        projectionLoc <- glGetUniformLocation card_shaderProgram projection
                        
                        objectColorLoc <- glGetUniformLocation card_shaderProgram objectColor
                        lightColorLoc <- glGetUniformLocation card_shaderProgram lightColor
                        lightPosLoc <- glGetUniformLocation card_shaderProgram lightPos
                        viewPosLoc <- glGetUniformLocation card_shaderProgram viewPos

                        poke viewP (transpose viewMat)
                        poke projP (transpose projMat)
                        
                        glUniformMatrix4fv viewLoc 1 GL_FALSE (castPtr viewP)
                        glUniformMatrix4fv projectionLoc 1 GL_FALSE (castPtr projP)
 
                        let card = Card.cards!!0
                        let modelMat = mkTransformation (axisAngle (V3 (0::GLfloat) 1 0) (timeValue * 0.4)) card
                        -- let modelMat = mkTransformationMat identity card
                        glUniform3f objectColorLoc (0.0::GLfloat) (0.0::GLfloat) (0.0::GLfloat)
                        glUniform3f lightColorLoc (1.0::GLfloat) (1.0::GLfloat) (1.0::GLfloat)
                        glUniform3f lightPosLoc (lp^._x) (lp^._y) (lp^._z)
                        glUniform3f viewPosLoc ((getCameraPos camera)^._x) ((getCameraPos camera)^._y) ((getCameraPos camera)^._z)
                        poke modelP (transpose modelMat)
                        glUniformMatrix4fv modelLoc 1 GL_FALSE (castPtr modelP)
                        glDrawArrays GL_TRIANGLES 0 36
                        glBindTexture GL_TEXTURE_2D 0 
                        -- draw card two
                        glBindTexture GL_TEXTURE_2D card_twoTexture
                        glActiveTexture GL_TEXTURE0
                        cardTextureLocation <- glGetUniformLocation card_shaderProgram cardTexture
                        glUniform1i cardTextureLocation 0
                        let card = Card.cards!!1
                        let modelMat = mkTransformation (axisAngle (V3 (0::GLfloat) (-1) (0)) (timeValue * 0.35)) card
                        -- let modelMat = mkTransformationMat identity card
                        glUniform3f objectColorLoc (1.0::GLfloat) (1.0::GLfloat) (1.0::GLfloat)
                        glUniform3f lightColorLoc (1.0::GLfloat) (1.0::GLfloat) (1.0::GLfloat)
                        glUniform3f lightPosLoc (lp^._x) (lp^._y) (lp^._z)
                        glUniform3f viewPosLoc ((getCameraPos camera)^._x) ((getCameraPos camera)^._y) ((getCameraPos camera)^._z)
                        poke modelP (transpose modelMat)
                        glUniformMatrix4fv modelLoc 1 GL_FALSE (castPtr modelP)
                        glDrawArrays GL_TRIANGLES 0 36
                        
                        glBindVertexArray 0  
                        glBindTexture GL_TEXTURE_2D 0 
------------------------------------------------------------------------------------------------------
--                      Swap Buffers and Repeat
------------------------------------------------------------------------------------------------------
                        GLFW.swapBuffers window
                        loop timeValue camera
            loop 0.0 (Camera (V3 0 0 3) (V3 0 0 (-1)) (V3 0 1 0))