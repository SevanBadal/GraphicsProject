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
    ,"    TexCoord = vec2(texCoord.x, 1.0 - texCoord.y);"
    ,"}"]

cardFragmentShaderSource :: String
cardFragmentShaderSource = unlines [
    "#version 330 core"
    ,"in vec2 TexCoord;"

    ,"out vec4 color;"

    ,"uniform sampler2D cardTexture;"

    ,"void main()"
    ,"{"
    ,"    float left = step(0.1,TexCoord.x);   // Similar to ( X greater than 0.1 )"
    ,"    float bottom = step(0.1,TexCoord.y); // Similar to ( Y greater than 0.1 );"
    ,"    // color = vec4(vec3(bottom * left),1.0);"
    ,"    color = texture(cardTexture, TexCoord);"
    ,"}"]