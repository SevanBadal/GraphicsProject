-- This module defines the shaders needed to render the roof object
module IcoShader where

-- Vertex Shader
icoVertexShaderSource :: String
icoVertexShaderSource = unlines [
    "#version 330 core"
    ,"layout (location = 0) in vec3 position;"
    ,"layout (location = 1) in vec3 color;"
    ,"layout (location = 2) in vec2 texCoord;"
    ,"uniform vec2 res;"
    ,"uniform mat4 model;"
    ,"uniform mat4 view;"
    ,"uniform mat4 projection;"
    ,"out vec2 u_resolution;"
    ,"out vec3 ourColor;"
    ,"out vec4 ourTri;"
    ,"out vec2 TexCoord;"
    ,"void main()"
    ,"{"
    ,"    gl_Position =  projection * view * model * vec4(position, 1.0);"
    ,"    u_resolution = res;"
    ,"    ourTri = gl_Position;"
    ,"    TexCoord = vec2(texCoord.x, 1.0f - texCoord.y);"
    ,"    ourColor = color; // Set ourColor to the input color we got from the vertex data"
    ,"}"]

-- Fragment Shader
icoFragmentShaderSource :: String
icoFragmentShaderSource = unlines [
    "#version 330 core"
    ,"uniform sampler2D ourTexture1;"
    ,"in vec3 ourColor;"
    ,"in vec2 TexCoord;"
    ,"out vec4 color;"
    ,"in vec2 u_resolution;"
    ,"in vec4 ourTri;"
    ,"void main()"
    ,"{"
    ,"    float ambientStrength = 0.4;"
    ,"    vec3 ambient = vec3(1.0, 0.5, 0.5) * ambientStrength;"
    ,"    vec4 ambientColor = vec4(ambient, 1.0);"
    ,"    vec2 st = gl_FragCoord.xy/u_resolution.xy;"
    ,"    color = texture(ourTexture1,TexCoord);"
    ,"}"]