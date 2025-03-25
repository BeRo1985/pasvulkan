#ifndef INSTANCEDATAEFFECT_GLSL
#define INSTANCEDATAEFFECT_GLSL

#include "pcg.glsl"

bool applyInstanceDataEffect(const in uint instanceDataIndex, inout vec4 color, const in vec2 uv, const in uvec2 fragCoord, const bool forRaytracing){
  
  if(instanceDataIndex == 0u){

    // No effect

    return true; 

  }else{
    
    bool visible = true;
    
    InstanceData instanceDataItem = instanceDataItems[instanceDataIndex];

    // First, apply selection highlight effect
    color.xyz = mix(color.xyz, instanceDataItem.SelectedColorIntensity.xyz * instanceDataItem.SelectedColorIntensity.w, instanceDataItem.SelectedDissolveDitheredTransparencyUnused.x);
    
    // Then, apply dissolve effect
    const float dissolve = instanceDataItem.SelectedDissolveDitheredTransparencyUnused.y;
    if(dissolve > 0.0){
      const vec4 color0Scale = instanceDataItem.DissolveColor0Scale;
      const vec4 color1Width = instanceDataItem.DissolveColor1Width;
      float edgeWidth = mix(1.0, 0.25, smoothstep(0.0, 1.0, dissolve)) * color1Width.w;
      float threshold = (pcgTileableNoise(uv.xy, color0Scale.w) + (pcgTileableNoise(uv.yx * 2.0, color0Scale.w) * 0.5)) / 1.5;
      float process = mix(0.0 - edgeWidth, 1.0, dissolve);
      float edge = smoothstep(threshold - edgeWidth, threshold, process);
      vec3 edgeColor = mix(color0Scale.xyz, color1Width.xyz, smoothstep(0.5, 0.75, edge));
      color = vec4(mix(color.xyz, edgeColor.xyz, smoothstep(0.0, 0.5, edge)), 1.0 - step(threshold, process));      
      if(color.w < 1e-5){
        visible = false;
      }
    }

    // Finally, apply dithered transparency effect
    if(visible){
      const float ditheredTransparency = instanceDataItem.SelectedDissolveDitheredTransparencyUnused.z;
      if(ditheredTransparency > 0.0){
        if(forRaytracing){
          color.w *= clamp(1.0 - ditheredTransparency, 0.0, 1.0); // Invert
          if(color.w < 1e-5){
            visible = false;
          }
        }else{
          // Dithered transparency with R-sequence ( https://extremelearning.com.au/unreasonable-effectiveness-of-quasirandom-sequences/ )
          if(abs(fract(dot(vec2(fragCoord), vec2(0.75487767, 0.56984029)))) < ditheredTransparency){
            color.w = 0.0;
            visible = false;
          }
        } 
      }
    }

    return visible;
  }
}

#endif