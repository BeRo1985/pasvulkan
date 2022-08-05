#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(set = 0, binding = 0) uniform sampler2DArray uTexture;

void main() {
  vec2 fragCoordInvScale = vec2(1.0) / vec2(textureSize(uTexture, 0).xy),  //
      w = fragCoordInvScale * 1.75;
  vec4 f = vec4(0.299, 0.587, 0.114, 0.0),                                                           // vec4(0.2126, 0.7152, 0.0722, 0.0),
      d = vec3(-1.0, 0.0, 1.0).xyzy,                                                                 //
      t = vec4(dot(textureLod(uTexture, vec3(inTexCoord + (d.yx * w), float(gl_ViewIndex)), 0), f),  //
               dot(textureLod(uTexture, vec3(inTexCoord + (d.xy * w), float(gl_ViewIndex)), 0), f),  //
               dot(textureLod(uTexture, vec3(inTexCoord + (d.zy * w), float(gl_ViewIndex)), 0), f),  //
               dot(textureLod(uTexture, vec3(inTexCoord + (d.yz * w), float(gl_ViewIndex)), 0), f));
  vec2 n = vec2(-(t.x - t.w), t.z - t.y);
  float nl = length(n);
  vec4 outColor = textureLod(uTexture, vec3(inTexCoord, float(gl_ViewIndex)), 0);
  if (nl >= 0.0625) {
    n *= fragCoordInvScale / nl;                                                                       //
    outColor = (outColor +                                                                             //
                ((textureLod(uTexture, vec3(inTexCoord + (n * 0.5), float(gl_ViewIndex)), 0) * 0.9) +  //
                 (textureLod(uTexture, vec3(inTexCoord - (n * 0.5), float(gl_ViewIndex)), 0) * 0.9) +  //
                 (textureLod(uTexture, vec3(inTexCoord + n, float(gl_ViewIndex)), 0) * 0.75) +         //
                 (textureLod(uTexture, vec3(inTexCoord - n, float(gl_ViewIndex)), 0) * 0.75))          //
                ) /
               4.3;  //
  }
  outFragColor = vec4(mix(pow((outColor.xyz + vec3(5.5e-2)) / vec3(1.055), vec3(2.4)), outColor.xyz / vec3(12.92), lessThan(outColor.xyz, vec3(4.045e-2))), outColor.w);
}
