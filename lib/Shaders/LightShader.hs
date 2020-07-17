module LightShader where

-- Vertex Shader
lightVertexShaderSource :: String
lightVertexShaderSource = unlines [
    "#version 330 core"
    ,"layout (location = 0) in vec3 position;"
    ,"uniform mat4 model;"
    ,"uniform mat4 view;"
    ,"uniform mat4 projection;"
    ,"void main()"
    ,"{"
    ,"    gl_Position =  projection * view * model * vec4(position, 1.0);"
    ,"}"]

-- Fragment Shader
lightFragmentShaderSource :: String
lightFragmentShaderSource = unlines [
    "#version 330 core"
    ,"out vec4 color;"
    ,"void main()"
    ,"{"
    ,"    color = vec4(1.0f);"
    ,"}"]