#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassInput; 

layout(set = 0, binding = 1) uniform sampler2DArray uTextureDebugData;

void main(){
  vec4 c = subpassLoad(uSubpassInput);
  int slices = textureSize(uTextureDebugData, 0).z;
  float sliceWidthSum = 0.1 * float(slices);
  if(all(lessThanEqual(inTexCoord, vec2(sliceWidthSum, 0.1)))){
    vec2 texCoord = inTexCoord * vec2(float(slices) / sliceWidthSum, 1.0 / 0.1);
    int index = int(texCoord.x);
    texCoord.x -= float(index);
    c = texture(uTextureDebugData, vec3(texCoord, index), 0.0);
  }  
  outFragColor = c;
}
