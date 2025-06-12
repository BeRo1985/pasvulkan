#ifndef PLANET_WETNESS_GLSL
#define PLANET_WETNESS_GLSL

#include "octahedral.glsl"
#include "octahedralmap.glsl"
 
vec4 getWetness(vec3 position, vec3 planetCenter){

  vec3 normalizedPosition = normalize(position - planetCenter);
  
  float rain = texturePlanetOctahedralMap(uPlanetTextures[PLANET_TEXTURE_RAINMAP], normalizedPosition).x;
  float atmosphere = texturePlanetOctahedralMap(uPlanetTextures[PLANET_TEXTURE_ATMOSPHEREMAP], normalizedPosition).x; 

  return vec4(rain * atmosphere, normalizedPosition); 

}

#endif