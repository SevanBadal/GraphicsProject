-- This module defines the shaders needed to render the roof object
module IcoShader where

-- Vertex Shader
icoVertexShaderSource :: String
icoVertexShaderSource = unlines [
    "#version 330 core"
    ,"layout (location = 0) in vec3 position;"
    ,"layout (location = 1) in vec3 color;"
    ,"uniform vec2 res;"
    ,"uniform mat4 model;"
    ,"uniform mat4 view;"
    ,"uniform mat4 projection;"
    ,"out vec2 u_resolution;"
    ,"out vec3 ourColor;"
    ,"out vec4 ourTri;"
    ,"void main()"
    ,"{"
    ,"    gl_Position =  projection * view * model * vec4(position, 1.0);"
    ,"    u_resolution = res;"
    ,"    ourTri = gl_Position;"
    ,"    ourColor = color; // Set ourColor to the input color we got from the vertex data"
    ,"}"]

-- Fragment Shader
icoFragmentShaderSource :: String
icoFragmentShaderSource = unlines [
    "#version 330 core"
    ,"in vec3 ourColor;"
    ,"out vec4 color;"
    ,"in vec2 u_resolution;"
    ,"in vec4 ourTri;"
    ,"void main()"
    ,"{"
    ,"    vec2 st = gl_FragCoord.xy/u_resolution.xy;"
    ,"    color = vec4(ourColor, 1.0f);"
    ,"}"]