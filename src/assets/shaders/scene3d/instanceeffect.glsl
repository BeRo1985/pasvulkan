#ifndef INSTANCEEFFECT_GLSL
#define INSTANCEEFFECT_GLSL

#include "bayer.glsl"

bool applyInstanceEffect(const in uint instanceEffectIndex, inout vec4 color, const in vec3 worldSpacePosition, const in uvec2 fragCoord, const bool forRaytracing){
  if(instanceEffectIndex == 0u){
    return true; 
  }else{
    
    bool visible = true;
    
    InstanceEffectData instanceEffect = instanceEffectData[instanceEffectIndex];

    // First, apply selection highlight effect
    color.xyz = mix(colorx.xyz, instanceEffect.SelectedColorIntensity.xyz * instanceEffect.SelectedColorIntensity.w, instanceEffect.SelectedDissolveDitheredTransparencyUnused.w);
    
    // Then, apply dissolve effect
    const float dissolve = instanceEffect.SelectedDissolveDitheredTransparencyUnused.y;
    if(dissolve > 0.0){
      const vec4 color0Scale = instanceEffect.DissolveColor0Scale;
      const vec4 color1Width = instanceEffect.DissolveColor1Width;
      
    }

    // Finally, apply dithered transparency effect
    const float ditheredTransparency = instanceEffect.SelectedDissolveDitheredTransparencyUnused.z;
    if(ditheredTransparency > 0.0){
      if(forRaytracing){
        color.w *= clamp(1.0 - ditheredTransparency, 0.0, 1.0); // Invert
        if(color.w < 1e-5){
          visible = false;
        }
      }else{
        // Dithered transparency
        if(bayerDither(fragCoord, 256u) < ditheredTransparency){
          color.w = 0.0;
          visible = false;
        }
      } 
    }

    return visible;
  }
}

#endif