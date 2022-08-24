#version 450 core

#extension GL_EXT_multiview : enable
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec2 inTexCoord;

layout(location = 0) out vec4 outFragColor;

layout(push_constant) uniform PushConstants {
  float factor;
  float bloomFactor;
  float lensflaresFactor;
  float bloomLensflaresFactor;
  int countGhosts;
  float lensStarRotationAngle;
  float aspectRatio;
  float inverseAspectRatio;
  float dispersal;
  float haloWidth;
  float distortion;
} pushConstants;

layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput uSubpassScene;

layout(set = 0, binding = 1) uniform sampler2DArray uTextureBloom;

float noise(vec2 p){
  vec2 f = fract(p);
  f = (f * f) * (3.0 - (2.0 * f));    
	float n = dot(floor(p), vec2(1.0, 157.0));
  vec4 a = fract(sin(vec4(n + 0.0, n + 1.0, n + 157.0, n + 158.0)) * 43758.5453123);
  return mix(mix(a.x, a.y, f.x), mix(a.z, a.w, f.x), f.y);
} 

float fbm(vec2 p){
  const mat2 m = mat2(0.80, -0.60, 0.60, 0.80);
  float f = 0.0;
  f += 0.5000*noise(p); p = m*p*2.02;
  f += 0.2500*noise(p); p = m*p*2.03;
  f += 0.1250*noise(p); p = m*p*2.01;
  f += 0.0625*noise(p);
  return f/0.9375;
} 

vec4 getLensColor(float x){
  // color gradient values from http://vserver.rosseaux.net/stuff/lenscolor.png
  // you can try to curve-fitting it, my own tries weren't optically better (and smaller) than the multiple mix+smoothstep solution 
  return vec4(vec3(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(mix(vec3(1.0, 1.0, 1.0),
                                                                               vec3(0.914, 0.871, 0.914), smoothstep(0.0, 0.063, x)),
                                                                           vec3(0.714, 0.588, 0.773), smoothstep(0.063, 0.125, x)),
                                                                       vec3(0.384, 0.545, 0.631), smoothstep(0.125, 0.188, x)),
                                                                   vec3(0.588, 0.431, 0.616), smoothstep(0.188, 0.227, x)),
                                                               vec3(0.31, 0.204, 0.537), smoothstep(0.227, 0.251, x)),
                                                           vec3(0.192, 0.106, 0.286), smoothstep(0.251, 0.314, x)),
                                                       vec3(0.102, 0.008, 0.341), smoothstep(0.314, 0.392, x)),
                                                   vec3(0.086, 0.0, 0.141), smoothstep(0.392, 0.502, x)),
                                               vec3(1.0, 0.31, 0.0), smoothstep(0.502, 0.604, x)),
                                           vec3(1.0, 0.49, 0.0), smoothstep(0.604, 0.643, x)),
                                       vec3(1.0, 0.929, 0.0), smoothstep(0.643, 0.761, x)),
                                   vec3(1.0, 0.086, 0.424), smoothstep(0.761, 0.847, x)),
                               vec3(1.0, 0.49, 0.0), smoothstep(0.847, 0.89, x)),
                           vec3(0.945, 0.275, 0.475), smoothstep(0.89, 0.941, x)),
                       vec3(0.251, 0.275, 0.796), smoothstep(0.941, 1.0, x))),
                    1.0);
}

vec4 getLensStar(vec2 p){
  // just be creative to create your own procedural lens star textures :)
  vec2 pp = (p - vec2(0.5)) * 2.0;
  float a = atan(pp.y, pp.x);
  vec4 cp = vec4(sin(a * 1.0), length(pp), sin(a * 13.0), sin(a * 53.0));
  float d = sin(clamp(pow(length(vec2(0.5) - p) * 2.0, 5.0), 0.0, 1.0) * 3.14159);
  vec3 c = vec3(d) * vec3(fbm(cp.xy * 16.0) * fbm(cp.zw * 9.0) * max(max(max(max(0.5, sin(a * 1.0)), sin(a * 3.0) * 0.8), sin(a * 7.0) * 0.8), sin(a * 9.0) * 0.6));
  c *= vec3(mix(1.0, (sin(length(pp.xy) * 256.0) * 0.5) + 0.5, sin((clamp((length(pp.xy) - 0.875) / 0.1, 0.0, 1.0) + 0.0) * 2.0 * 3.14159) * 0.5) + 0.5) * 0.3275;
  return vec4(vec3(c * 4.0), d);	
}

vec4 getLensDirt(vec2 p){
  // just be creative to create your own procedural lens dirt textures :)
  p.xy += vec2(fbm(p.yx * 3.0), fbm(p.yx * 6.0)) * 0.0625;
  vec3 o = vec3(mix(0.125, 0.25, max(max(smoothstep(0.4, 0.0, length(p - vec2(0.25))),
                                         smoothstep(0.4, 0.0, length(p - vec2(0.75)))),
                                         smoothstep(0.8, 0.0, length(p - vec2(0.875, 0.25))))));
  o += vec3(max(fbm(p * 1.0) - 0.5, 0.0)) * 0.5;
  o += vec3(max(fbm(p * 2.0) - 0.5, 0.0)) * 0.5;
  o += vec3(max(fbm(p * 4.0) - 0.5, 0.0)) * 0.25;
  o += vec3(max(fbm(p * 8.0) - 0.75, 0.0)) * 1.0;
  o += vec3(max(fbm(p * 16.0) - 0.75, 0.0)) * 0.75;
  o += vec3(max(fbm(p * 64.0) - 0.75, 0.0)) * 0.5;
  return vec4(clamp(o, vec3(0.0), vec3(1.0)), 1.0);	
}

vec4 textureLimited(const in vec2 texCoord){
	if(((texCoord.x < 0.0) || (texCoord.y < 0.0)) || ((texCoord.x > 1.0) || (texCoord.y > 1.0))){
	 	return vec4(0.0);
	}else{
	 	return textureLod(uTextureBloom, vec3(texCoord, gl_ViewIndex), 0.0);// * pow(1.0 - (length(texCoord.y - vec2(0.5)) * 2.0), 4.0);
	}
}

vec4 textureDistorted(const in vec2 texCoord, const in vec2 direction, const in vec3 distortion) {
  return clamp(
           vec4(textureLimited((texCoord + (direction * distortion.r))).r,
                textureLimited((texCoord + (direction * distortion.g))).g,
							  textureLimited((texCoord + (direction * distortion.b))).b,
                1.0),
           vec4(0.0),
           vec4(32768.0)
          );
}

vec4 getLensFlare(){
  vec2 aspectTexCoord = vec2(1.0) - (((inTexCoord - vec2(0.5)) * vec2(1.0, pushConstants.inverseAspectRatio)) + vec2(0.5)); 
  vec2 texCoord = vec2(1.0) - inTexCoord; 
  vec2 ghostVec = (vec2(0.5) - texCoord) * pushConstants.dispersal;
  vec2 ghostVecAspectNormalized = normalize(ghostVec * vec2(1.0, pushConstants.inverseAspectRatio)) * vec2(1.0, pushConstants.aspectRatio);
  vec2 haloVec = normalize(ghostVec) * pushConstants.haloWidth;
  vec2 haloVecAspectNormalized = ghostVecAspectNormalized * pushConstants.haloWidth;
  vec2 texelSize = vec2(1.0) / vec2(textureSize(uTextureBloom, 0).xy);
  vec3 distortion = vec3(-(texelSize.x * pushConstants.distortion), 0.0, texelSize.x * pushConstants.distortion);
  vec4 c = vec4(0.0);
  for (int i = 0, j = pushConstants.countGhosts; i < j; i++) {
    vec2 offset = texCoord + (ghostVec * float(i));
    c += textureDistorted(offset, ghostVecAspectNormalized, distortion) * clamp(pow(max(0.0, 1.0 - (length(vec2(0.5) - offset) / length(vec2(0.5)))), 10.0), 0.0, 16.0);
  }                       
  vec2 haloOffset = texCoord + haloVecAspectNormalized; 
  return (c * getLensColor((length(vec2(0.5) - aspectTexCoord) / length(vec2(0.5))))) + 
         (textureDistorted(haloOffset, ghostVecAspectNormalized, distortion) * clamp(pow(max(0.0, 1.0 - (length(vec2(0.5) - haloOffset) / length(vec2(0.5)))), 10.0), 0.0, 16.0));
} 


void main(){
  vec4 bloom = clamp(textureLod(uTextureBloom, vec3(inTexCoord, gl_ViewIndex), 0.0), vec4(0.0), vec4(32768.0));  
  vec4 lensflares = vec4(0.0);
  vec2 texCoord = ((inTexCoord - vec2(0.5)) * vec2(pushConstants.aspectRatio, 1.0) * 0.5) + vec2(0.5);
  if(pushConstants.lensflaresFactor > 1e-7){
    vec2 lensStarTexCoord = (mat2(cos(pushConstants.lensStarRotationAngle), -sin(pushConstants.lensStarRotationAngle), sin(pushConstants.lensStarRotationAngle), cos(pushConstants.lensStarRotationAngle)) * (texCoord - vec2(0.5))) + vec2(0.5);
    lensflares = getLensFlare() * mix(vec4(2.0), getLensStar(lensStarTexCoord), 0.5) * pushConstants.lensflaresFactor;
  }
  vec4 lensDirt = getLensDirt(inTexCoord);
  outFragColor = mix(clamp(subpassLoad(uSubpassScene), vec4(0.0), vec4(32768.0)), 
                     mix(bloom * pushConstants.bloomFactor, lensflares, pushConstants.bloomLensflaresFactor) * lensDirt,
                     pushConstants.factor);
}
