-- This module defines the shaders needed to render the base of the house
module CardShader where

cardVertexShaderSource :: String
cardVertexShaderSource = unlines [
    "#version 330 core"
    ,"layout (location = 0) in vec3 position;"
    ,"layout (location = 1) in vec2 texCoord;"

    ,"out vec2 TexCoord;"

    ,"uniform mat4 model;"
    ,"uniform mat4 view;"
    ,"uniform mat4 projection;"

    ,"void main()"
    ,"{"
    ,"    gl_Position = projection * view * model * vec4(position, 1.0f);"
    ,"    TexCoord = vec2(texCoord.x, 1.0f - texCoord.y);"
    ,"}"]

cardFragmentShaderSource :: String
cardFragmentShaderSource = unlines [
    "#version 330 core"
    ,"in vec2 TexCoord;"

    ,"out vec4 color;"

    ,"uniform sampler2D cardTexture;"

    ,"void main()"
    ,"{"
    ,"    color = texture(cardTexture, TexCoord);"
    ,"}"]