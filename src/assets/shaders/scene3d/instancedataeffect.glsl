#ifndef INSTANCEDATAEFFECT_GLSL
#define INSTANCEDATAEFFECT_GLSL

#include "pcg.glsl"

bool applyMaterialInstanceDataEffect(const in uint instanceDataIndex, 
                                     inout vec4 color, 
                                     const in vec2 uv, 
                                     const in uvec2 fragCoord, 
                                     const bool forRaytracing){

  if(instanceDataIndex == 0u){

    // No effect

    return true; 

  }else{
    
    bool visible = true;
    
    InstanceData instanceDataItem = instanceDataItems[instanceDataIndex];

    uint flags = instanceDataItem.SelectedDissolveDitheredTransparencyFlags.w;

    if((flags & (1u << 0)) != 0u){

      const uvec4 colorKeyRG = instanceDataItem.colorKeysRG;
      const uvec4 colorKeyBA = instanceDataItem.colorKeysBA;

      // Color keys
      const vec4 colorKeys[4] = vec4[4](
        vec4(unpackHalf2x16(colorKeyRG.x), unpackHalf2x16(colorKeyRG.y)),
        vec4(unpackHalf2x16(colorKeyRG.z), unpackHalf2x16(colorKeyRG.w)),
        vec4(unpackHalf2x16(colorKeyBA.x), unpackHalf2x16(colorKeyBA.y)),
        vec4(unpackHalf2x16(colorKeyBA.z), unpackHalf2x16(colorKeyBA.w))
      );

      color = (colorKeys[0] * color.x) + (colorKeys[1] * color.y) + (colorKeys[2] * color.z) + (colorKeys[3] * color.w);

    }

    return visible;

  }

}

// The per instance material colour as the main shading applies it to the base colour: first the color
// key matrix for materials flagged for it (material flags bit 25), then the tint of the material's
// color key slot (material flags bits 17..19). For every other pass that reads a material's base colour
// or alpha - depth, shadows, voxelization, ray queries - so that they see the same surface, including
// an alpha tested surface thinned out by the alpha of a tint.
void applyMaterialInstanceColor(const in uint instanceDataIndex, const in uint materialFlags, inout vec4 color){
  if(instanceDataIndex > 0u){
    if((materialFlags & (1u << 25u)) != 0u){
      applyMaterialInstanceDataEffect(instanceDataIndex, color, vec2(0.0), uvec2(0u), false);
    }
    uint materialColorKeySlot = (materialFlags >> 17u) & 7u;
    if((materialColorKeySlot > 0u) && (materialColorKeySlot <= 4u)){
      color *= unpackUnorm4x8(instanceDataItems[instanceDataIndex].materialColorKeys[materialColorKeySlot - 1u]);
    }
  }
}
  

bool applyInstanceDataEffect(const in uint instanceDataIndex, inout vec4 color, const in vec2 uv, const in uvec2 fragCoord, const bool forRaytracing){
  
  if(instanceDataIndex == 0u){

    // No effect

    return true; 

  }else{
    
    bool visible = true;
    
    InstanceData instanceDataItem = instanceDataItems[instanceDataIndex];

    vec3 SelectedDissolveDitheredTransparency = uintBitsToFloat(instanceDataItem.SelectedDissolveDitheredTransparencyFlags.xyz);

    // First, apply selection highlight effect (replaced by outline now)
    // color.xyz = mix(color.xyz, instanceDataItem.SelectedColorIntensity.xyz * instanceDataItem.SelectedColorIntensity.w, SelectedDissolveDitheredTransparency.x);
    
    // Then, apply dissolve effect
    const float dissolve = SelectedDissolveDitheredTransparency.y;
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
      const float ditheredTransparency = SelectedDissolveDitheredTransparency.z;
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