module ShaderLink where
    
import Graphics.GL.Core33
import Graphics.GL.Types
import Foreign -- c lang bindings
import Foreign.C.String (withCAStringLen, newCString)

-- | Given a shader type and a shader source, it gives you (Right id) of the
-- successfully compiled shader, or (Left err) with the error message. In the
-- error case, the shader id is deleted before the function returns to avoid
-- accidentally leaking shader objects.
loadShader :: GLenum -> String -> IO (Either String GLuint)
loadShader shaderType source = do
    -- new shader object
    shaderID <- glCreateShader shaderType
    -- assign the source to the shader object
    withCAStringLen source $ \(strP, strLen) ->
        withArray [strP] $ \linesPtrsPtr ->
            withArray [fromIntegral strLen] $ \lengthsPtr ->
                glShaderSource shaderID 1 linesPtrsPtr lengthsPtr
    -- compile and check success
    glCompileShader shaderID
    success <- alloca $ \successP -> do
        glGetShaderiv shaderID GL_COMPILE_STATUS successP
        peek successP
    if success == GL_TRUE
        -- success: we're done
        then return (Right shaderID)
        -- failure: we get the log, delete the shader, and return the log.
        else do
            -- how many bytes the info log should be (including the '\0')
            logLen <- alloca $ \logLenP -> do
                glGetShaderiv shaderID GL_INFO_LOG_LENGTH logLenP
                peek logLenP
            -- space for the info log
            logBytes <- allocaBytes (fromIntegral logLen) $ \logP -> do
                -- space for the log reading result
                alloca $ \resultP -> do
                    -- Try to obtain the log bytes
                    glGetShaderInfoLog shaderID logLen resultP logP
                    -- this is how many bytes we actually got
                    result <- fromIntegral <$> peek resultP
                    peekArray result logP
            -- delete the shader object and return the log
            glDeleteShader shaderID
            let prefix = case shaderType of
                    GL_VERTEX_SHADER -> "Vertex"
                    GL_GEOMETRY_SHADER -> "Geometry"
                    GL_FRAGMENT_SHADER -> "Fragment"
                    _ -> "Unknown Type"
            return $ Left $
                prefix ++ " Shader Error:" ++
                    (map (toEnum.fromEnum) logBytes)

-- | Given a vertex shader object and a fragment shader object, this will link
-- them into a new program, giving you (Right id). If there's a linking error
-- the error log is retrieved, the program deleted, and (Left err) is returned.
linkProgram :: GLuint -> GLuint -> IO (Either String GLuint)
linkProgram vertexID fragmentID = do
    programID <- glCreateProgram
    glAttachShader programID vertexID
    glAttachShader programID fragmentID
    glLinkProgram programID
    success <- alloca $ \successP -> do
        glGetProgramiv programID GL_LINK_STATUS successP
        peek successP
    if success == GL_TRUE
        -- success: we're done
        then return (Right programID)
        -- failure: we get the log, delete the shader, and return the log.
        else do
            -- how many bytes the info log should be (including the '\0')
            logLen <- alloca $ \logLenP -> do
                glGetProgramiv programID GL_INFO_LOG_LENGTH logLenP
                peek logLenP
            -- space for the info log
            logBytes <- allocaBytes (fromIntegral logLen) $ \logP -> do
                -- space for the log reading result
                alloca $ \resultP -> do
                    -- Try to obtain the log bytes
                    glGetProgramInfoLog programID logLen resultP logP
                    -- this is how many bytes we actually got
                    result <- fromIntegral <$> peek resultP
                    peekArray result logP
            -- delete the program object and return the log
            glDeleteProgram programID
            return $ Left $ "Program Link Error: " ++
                (map (toEnum.fromEnum) logBytes)


-- | Given the source for the vertex shader and the fragment shader, compiles
-- both and links them into a single program. If all of that is successful, the
-- intermediate shaders are deleted before the final value is returned.
programFromSources :: String -> String -> IO (Either String GLuint)
programFromSources vertexSource fragmentSource = do
    eitherVertShader <- loadShader GL_VERTEX_SHADER vertexSource
    case eitherVertShader of
        Left e -> return $ Left e
        Right vertShader -> do
            eitherFragShader <- loadShader GL_FRAGMENT_SHADER fragmentSource
            case eitherFragShader of
                Left e -> do
                    glDeleteShader vertShader
                    return $ Left e
                Right fragShader -> do
                    eitherProgram <- linkProgram vertShader fragShader
                    glDeleteShader vertShader
                    glDeleteShader fragShader
                    return $ eitherProgram