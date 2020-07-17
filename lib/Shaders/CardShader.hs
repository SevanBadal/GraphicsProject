-- This module defines the shaders needed to render the base of the house
module CardShader where

cardVertexShaderSource :: String
cardVertexShaderSource = unlines [
    "#version 330 core"
    ,"layout (location = 0) in vec3 position;"
    ,"layout (location = 1) in vec2 texCoord;"
    ,"layout (location = 2) in vec3 normal;"

    ,"out vec2 TexCoord;"
    ,"out vec3 Normal;"
    ,"out vec3 FragPos;"
    ,"uniform mat4 model;"
    ,"uniform mat4 view;"
    ,"uniform mat4 projection;"

    ,"void main()"
    ,"{"
    ,"    gl_Position = projection * view * model * vec4(position, 1.0f);"
    ,"    TexCoord = vec2(texCoord.x, 1.0 - texCoord.y);"
    ,"    FragPos = vec3(model * vec4(position,1.0f));"
    ,"    Normal = mat3(transpose(inverse(model))) * normal;"
    ,"}"]

cardFragmentShaderSource :: String
cardFragmentShaderSource = unlines [
    "#version 330 core"
    ,"in vec2 TexCoord;"
    ,"in vec3 Normal;"
    ,"in vec3 FragPos;"
    ,"out vec4 color;"

    ,"uniform vec3 lightPos;"
    ,"uniform vec3 viewPos;"
    ,"uniform vec3 objectColor;"
    ,"uniform vec3 lightColor;"

    ,"uniform sampler2D cardTexture;"

    ,"void main()"
    ,"{"
    ,"    float ambientStrength = 0.4f;"
    ,"    vec3 ambient = lightColor * ambientStrength;"
    ,"    // diffuse"
    ,"    vec3 lightDir = normalize(lightPos - FragPos);"
    ,"    float diff = max(dot(Normal, lightDir),0.0);"
    ,"    vec3 specColor= objectColor + lightColor * lightDir * diff;"
    ,"    vec3 diffuse = diff * lightColor;"
    ,"    // specular"
    ,"    float specularStrength = 3.0f;"
    ,"    vec3 viewDir = normalize(viewPos - FragPos);"
    ,"    vec3 reflectDir = reflect(-lightDir, Normal);"
    ,"    float spec = pow(max(dot(viewDir, reflectDir),0.0),25);"
    ,"    vec3 specular = specularStrength * spec * specColor;"  
    ,"    vec3 result = (ambient + diffuse + specular);"
    ,"    color = vec4(result, 1.0f) * texture(cardTexture, TexCoord) + vec4((0.1f * specColor),1.0f);"
    ,"}"]