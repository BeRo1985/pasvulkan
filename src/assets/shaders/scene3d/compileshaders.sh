#!/bin/bash

################################################################################################################### 
#### This script compiles all necessary shaders for the Scene3D sub-framework-part of the PasVulkan framework. ####
###################################################################################################################

#############################################
#          Predefined shader list           #
#############################################

compileshaderarguments=(
  
  '-V downsample.comp -DR11G11B10F -DMIPMAPLEVEL=0 -o downsample_r11g11b10f_level0_comp.spv'
  '-V downsample.comp -DR11G11B10F -DMIPMAPLEVEL=1 -o downsample_r11g11b10f_level1_comp.spv'
  '-V downsample.comp -DR11G11B10F -DMIPMAPLEVEL=2 -o downsample_r11g11b10f_level2_comp.spv'
  '-V downsample.comp -DR11G11B10F -DMIPMAPLEVEL=0 -DMULTIVIEW -o downsample_r11g11b10f_multiview_level0_comp.spv'
  '-V downsample.comp -DR11G11B10F -DMIPMAPLEVEL=1 -DMULTIVIEW -o downsample_r11g11b10f_multiview_level1_comp.spv'
  '-V downsample.comp -DR11G11B10F -DMIPMAPLEVEL=2 -DMULTIVIEW -o downsample_r11g11b10f_multiview_level2_comp.spv'

  '-V downsample.comp -DRGBA16F -DMIPMAPLEVEL=0 -o downsample_rgba16f_level0_comp.spv'
  '-V downsample.comp -DRGBA16F -DMIPMAPLEVEL=1 -o downsample_rgba16f_level1_comp.spv'
  '-V downsample.comp -DRGBA16F -DMIPMAPLEVEL=2 -o downsample_rgba16f_level2_comp.spv'
  '-V downsample.comp -DRGBA16F -DMIPMAPLEVEL=0 -DMULTIVIEW -o downsample_rgba16f_multiview_level0_comp.spv'
  '-V downsample.comp -DRGBA16F -DMIPMAPLEVEL=1 -DMULTIVIEW -o downsample_rgba16f_multiview_level1_comp.spv'
  '-V downsample.comp -DRGBA16F -DMIPMAPLEVEL=2 -DMULTIVIEW -o downsample_rgba16f_multiview_level2_comp.spv'

  '-V downsample_depth.comp -DMIPMAPLEVEL=0 -o downsample_depth_level0_comp.spv'
  '-V downsample_depth.comp -DMIPMAPLEVEL=0 -DREVERSEDZ -o downsample_depth_reversedz_level0_comp.spv'
  '-V downsample_depth.comp -DMIPMAPLEVEL=0 -DMSAA -o downsample_depth_msaa_level0_comp.spv'
  '-V downsample_depth.comp -DMIPMAPLEVEL=0 -DMSAA -DREVERSEDZ -o downsample_depth_msaa_reversedz_level0_comp.spv'
  '-V downsample_depth.comp -DMIPMAPLEVEL=0 -DMULTIVIEW -o downsample_depth_multiview_level0_comp.spv'
  '-V downsample_depth.comp -DMIPMAPLEVEL=0 -DMULTIVIEW -DREVERSEDZ -o downsample_depth_multiview_reversedz_level0_comp.spv'
  '-V downsample_depth.comp -DMIPMAPLEVEL=0 -DMULTIVIEW -DMSAA -o downsample_depth_multiview_msaa_level0_comp.spv'
  '-V downsample_depth.comp -DMIPMAPLEVEL=0 -DMULTIVIEW -DMSAA -DREVERSEDZ -o downsample_depth_multiview_msaa_reversedz_level0_comp.spv'
  '-V downsample_depth.comp -DMIPMAPLEVEL=1 -o downsample_depth_level1_comp.spv'
  '-V downsample_depth.comp -DMIPMAPLEVEL=1 -DMULTIVIEW -o downsample_depth_multiview_level1_comp.spv'
  '-V downsample_depth.comp -DMIPMAPLEVEL=1 -DREVERSEDZ -o downsample_depth_reversedz_level1_comp.spv'
  '-V downsample_depth.comp -DMIPMAPLEVEL=1 -DMULTIVIEW -DREVERSEDZ -o downsample_depth_multiview_reversedz_level1_comp.spv'

  '-V dof_autofocus.comp -o dof_autofocus_comp.spv'
  '-V dof_bokeh.comp -o dof_bokeh_comp.spv'
  '-V dof_prepare.frag -o dof_prepare_frag.spv'
  '-V dof_prefilter.frag -o dof_prefilter_frag.spv'
  '-V dof_blur.frag -o dof_blur_frag.spv'
  '-V dof_bruteforce.frag -o dof_bruteforce_frag.spv'
  '-V dof_postblur.frag -o dof_postblur_frag.spv'
  '-V dof_combine.frag -o dof_combine_frag.spv'
  '-V dof_gather.frag -DPASS1 -o dof_gather_pass1_frag.spv'
  '-V dof_gather.frag -DPASS2 -o dof_gather_pass2_frag.spv'
  '-V dof_resolve.frag -o dof_resolve_frag.spv'

  '-V luminance_histogram.comp -o luminance_histogram_comp.spv'
  '-V luminance_histogram.comp -DMULTIVIEW -o luminance_histogram_multiview_comp.spv'

  '-V luminance_average.comp -o luminance_average_comp.spv'

  '-V frustumclustergridbuild.comp -o frustumclustergridbuild_comp.spv'
  '-V frustumclustergridbuild.comp -DREVERSEDZ -o frustumclustergridbuild_reversedz_comp.spv'

  '-V frustumclustergridassign.comp -o frustumclustergridassign_comp.spv'
  
  '-V lens_upsample.comp -DR11G11B10F -o lens_upsample_r11g11b10f_comp.spv'
  '-V lens_upsample.comp -DRGBA16F -o lens_upsample_rgba16f_comp.spv'
  '-V lens_upsample.comp -DR11G11B10F -DMULTIVIEW -o lens_upsample_r11g11b10f_multiview_comp.spv'
  '-V lens_upsample.comp -DRGBA16F -DMULTIVIEW -o lens_upsample_rgba16f_multiview_comp.spv'
  '-V lens_resolve.frag -o lens_resolve_frag.spv'

  '-V lens_color.frag -o lens_color_frag.spv'
  '-V lens_dirt.frag -o lens_dirt_frag.spv'
  '-V lens_star.frag -o lens_star_frag.spv'
  
  '-V mesh.comp -o mesh_comp.spv'

  '-V mesh.vert -o mesh_vert.spv'

  '-V mesh.vert -DVELOCITY -o mesh_velocity_vert.spv'

  '-V mboit_resolve.frag -o mboit_resolve_frag.spv'
  '-V mboit_resolve.frag -DMSAA -o mboit_resolve_msaa_frag.spv'
  '-V wboit_resolve.frag -o wboit_resolve_frag.spv'
  '-V wboit_resolve.frag -DMSAA -o wboit_resolve_msaa_frag.spv'
  
  '-V lockoit_resolve.frag -o lockoit_resolve_frag.spv'
  '-V lockoit_resolve.frag -DREVERSEDZ -o lockoit_resolve_reversedz_frag.spv'
  '-V lockoit_resolve.frag -DMSAA -o lockoit_resolve_msaa_frag.spv'
  '-V lockoit_resolve.frag -DMSAA -DREVERSEDZ -o lockoit_resolve_reversedz_msaa_frag.spv'

  '-V loopoit_resolve.frag -o loopoit_resolve_frag.spv'
  '-V loopoit_resolve.frag -DREVERSEDZ -o loopoit_resolve_reversedz_frag.spv'
  '-V loopoit_resolve.frag -DMSAA -o loopoit_resolve_msaa_frag.spv'
  '-V loopoit_resolve.frag -DMSAA -DREVERSEDZ -o loopoit_resolve_reversedz_msaa_frag.spv'

  '-V dfaoit_resolve.frag -o dfaoit_resolve_frag.spv'
  '-V dfaoit_resolve.frag -DREVERSEDZ -o dfaoit_resolve_reversedz_frag.spv'
  '-V dfaoit_resolve.frag -DMSAA -o dfaoit_resolve_msaa_frag.spv'
  '-V dfaoit_resolve.frag -DMSAA -DREVERSEDZ -o dfaoit_resolve_reversedz_msaa_frag.spv'

  '-V blend_resolve.frag -o blend_resolve_frag.spv'
  '-V blend_resolve.frag -DMSAA -o blend_resolve_msaa_frag.spv'  

  '-V brdf_charlie.frag -o brdf_charlie_frag.spv'
  '-V brdf_ggx.frag -o brdf_ggx_frag.spv'
  
  '-V brdf_sheen_e.frag -o brdf_sheen_e_frag.spv'
  '-V brdf_sheen_e.frag -DFAST -o brdf_sheen_e_fast_frag.spv'
    
  '-V fullscreen.vert -o fullscreen_vert.spv'
  
  '-V cubemap.vert -o cubemap_vert.spv'
  '-V cubemap_sky.comp -o cubemap_sky_comp.spv'
  '-V cubemap_sky.comp -DFAST -o cubemap_sky_fast_comp.spv'
  '-V cubemap_sky.frag -o cubemap_sky_frag.spv'
  '-V cubemap_sky.frag -DFAST -o cubemap_sky_fast_frag.spv'
  '-V cubemap_filter.comp -o cubemap_filter_comp.spv'
  
  '-V passthrough.vert -o passthrough_vert.spv'
  
  '-V dummy.frag -o dummy_frag.spv'

  '-V dithering.frag -o dithering_frag.spv'

  '-V debug_blit.frag -o debug_blit_frag.spv'
  
  '-V skybox.vert -o skybox_vert.spv'
  '-V skybox.frag -o skybox_frag.spv'
  
  '-V skybox_realtime.frag -o skybox_realtime_frag.spv'
  
  '-V tonemapping.frag -o tonemapping_frag.spv'
  
  '-V antialiasing_dsaa.frag -o antialiasing_dsaa_frag.spv'
  '-V antialiasing_fxaa.frag -o antialiasing_fxaa_frag.spv'
  '-V antialiasing_taa.frag -o antialiasing_taa_frag.spv'
  '-V antialiasing_none.frag -o antialiasing_none_frag.spv'
  
  '-V antialiasing_smaa_blend.vert -o antialiasing_smaa_blend_vert.spv'
  '-V antialiasing_smaa_blend.frag -o antialiasing_smaa_blend_frag.spv'
  
  '-V antialiasing_smaa_edges.vert -o antialiasing_smaa_edges_vert.spv'
  '-V antialiasing_smaa_edges.frag -o antialiasing_smaa_edges_color_frag.spv'
  '-V antialiasing_smaa_edges.frag -DLUMA -o antialiasing_smaa_edges_luma_frag.spv'
  
  '-V antialiasing_smaa_weights.vert -o antialiasing_smaa_weights_vert.spv'
  '-V antialiasing_smaa_weights.frag -o antialiasing_smaa_weights_frag.spv'
  
  '-V blit.frag -o blit_frag.spv'

  '-V msaa_resolve.frag -o msaa_resolve_frag.spv'
  
  '-V msm_blur.frag -o msm_blur_frag.spv'
  '-V msm_blur.vert -o msm_blur_vert.spv'
  
  '-V msm_resolve.frag -o msm_resolve_frag.spv'
  '-V msm_resolve.frag -DMSAA -o msm_resolve_msaa_frag.spv'
  '-V msm_resolve.vert -o msm_resolve_vert.spv'

  '-V ssao.frag -o ssao_frag.spv'
  '-V ssao.frag -DMULTIVIEW -o ssao_multiview_frag.spv'

  '-V ssao_blur.frag -o ssao_blur_frag.spv'

  '-V contentprojection.frag -o contentprojection_frag.spv'
  '-V contentprojection.frag -DREVERSEDZ -o contentprojection_reversedz_frag.spv'

  '-V mipmap.comp -DLEVEL0 -o mipmap_level0_comp.spv'
  '-V mipmap.comp -DLEVEL1 -o mipmap_level1_comp.spv'

  '-V debug_primitive.vert -o debug_primitive_vert.spv'
  '-V debug_primitive.frag -o debug_primitive_frag.spv'

  '-V particle.vert -o particle_vert.spv'

)

#############################################
#               Helper functions            #
#############################################

addShader(){
  compileshaderarguments+=("$1")
}

#############################################
#               Particle shaders            #
#############################################

addParticleFragmentShader(){
  addShader "-V particle.frag ${2} -o ${1}_frag.spv"
}

# Add particle fragment shader variants with different transparency techniques (if any)
addParticleFragmentShadingTransparencyVariants(){

  # Standard alpha blending
  addParticleFragmentShader "${1}_blend" "$2 -DBLEND"

  # WBOIT (Weighted-Blended Order Independent Transparency)
  addParticleFragmentShader "${1}_wboit" "$2 -DWBOIT"

  # MBOIT (Moment-Based order independent transparency)
  addParticleFragmentShader "${1}_mboit_pass1" "$2 -DMBOIT -DMBOITPASS1"
  addParticleFragmentShader "${1}_mboit_pass2" "$2 -DMBOIT -DMBOITPASS2"

  # LoopOIT (Multi-pass order independent transparency)
  addParticleFragmentShader "${1}_loopoit_pass1" "$2 -DLOOPOIT -DLOOPOIT_PASS1"
  addParticleFragmentShader "${1}_loopoit_pass2" "$2 -DLOOPOIT -DLOOPOIT_PASS2"

  # LockOIT (Order independent transparency with spinlock/interlock, depending on the GPU capabilities)
  addParticleFragmentShader "${1}_spinlock_lockoit" "$2 -DLOCKOIT -DSPINLOCK"
  addParticleFragmentShader "${1}_interlock_lockoit" "$2 -DLOCKOIT -DINTERLOCK"

  # DFAOIT (Neural network based order independent transparency)
  addParticleFragmentShader "${1}_spinlock_dfaoit" "$2 -DDFAOIT -DSPINLOCK"
  addParticleFragmentShader "${1}_interlock_dfaoit" "$2 -DDFAOIT -DINTERLOCK"

}

addParticleFragmentShadingAntialiasingVariants(){
  
  # No antialiasing or temporal antialiasing
  addParticleFragmentShadingTransparencyVariants "${1}" "$2"

  # MSAA (Multi-sample anti-aliasing)
  addParticleFragmentShadingTransparencyVariants "${1}_msaa" "$2 -DMSAA"  

}

# Add particle fragment shader variants with different Z direction
addParticleFragmentZVariants(){
  
  # Normal Z direction
  addParticleFragmentShadingAntialiasingVariants "$1" "$2"
  
  # Reversed Z direction
  addParticleFragmentShadingAntialiasingVariants "${1}_reversedz" "$2 -DREVERSEDZ"
  
}

addParticleFragmentZVariants "particle" ""

#############################################
#               Mesh shaders                #
#############################################

addMeshFragmentShader(){
  addShader "-V mesh.frag ${2} -o ${1}_frag.spv"
}

# Add mesh fragment shader variants with different alpha test techniques (if any)
addMeshFragmentShadingAlphaTestVariants(){
  addMeshFragmentShader "$1" "$2"
  addMeshFragmentShader "${1}_alphatest" "$2 -DALPHATEST"
  addMeshFragmentShader "${1}_alphatest_demote" "$2 -DALPHATEST -DUSEDEMOTE"
  addMeshFragmentShader "${1}_alphatest_nodiscard" "$2 -DALPHATEST -DNODISCARD"
}

# Add mesh fragment shader variants with different alpha test techniques (if any)
addMeshFragmentShadingOITAlphaTestVariants(){
  addMeshFragmentShader "$1" "$2"
  addMeshFragmentShader "${1}_alphatest" "$2 -DALPHATEST"
}

# Add mesh fragment shader variants with different transparency techniques (if any)
addMeshFragmentShadingTransparencyVariants(){

  # No blending   
  addMeshFragmentShadingAlphaTestVariants "$1" "$2"

  if [[ $2 != *"ENVMAP"* ]]; then

    # Standard alpha blending
    addMeshFragmentShadingAlphaTestVariants "${1}_blend" "$2 -DBLEND"

    # WBOIT (Weighted-Blended Order Independent Transparency)
    addMeshFragmentShadingOITAlphaTestVariants "${1}_wboit" "$2 -DWBOIT"

    # MBOIT (Moment-Based order independent transparency)
    addMeshFragmentShadingOITAlphaTestVariants "${1}_mboit_pass1" "$2 -DMBOIT -DMBOITPASS1"
    addMeshFragmentShadingOITAlphaTestVariants "${1}_mboit_pass2" "$2 -DMBOIT -DMBOITPASS2" 

    # LoopOIT (Multi-pass order independent transparency)
    addMeshFragmentShadingOITAlphaTestVariants "${1}_loopoit_pass1" "$2 -DLOOPOIT -DLOOPOIT_PASS1"
    addMeshFragmentShadingOITAlphaTestVariants "${1}_loopoit_pass2" "$2 -DLOOPOIT -DLOOPOIT_PASS2"

    # LockOIT (Order independent transparency with spinlock/interlock, depending on the GPU capabilities)  
    addMeshFragmentShadingOITAlphaTestVariants "${1}_spinlock_lockoit" "$2 -DLOCKOIT -DSPINLOCK"
    addMeshFragmentShadingOITAlphaTestVariants "${1}_interlock_lockoit" "$2 -DLOCKOIT -DINTERLOCK"

    # DFAOIT (Neural network based order independent transparency)
    addMeshFragmentShadingOITAlphaTestVariants "${1}_spinlock_dfaoit" "$2 -DDFAOIT -DSPINLOCK"
    addMeshFragmentShadingOITAlphaTestVariants "${1}_interlock_dfaoit" "$2 -DDFAOIT -DINTERLOCK"

  fi  

}

# Add mesh fragment shader variants with different shadow techniques (if any)
addMeshFragmentShadingShadowVariants(){

  # No shadows
  #addMeshFragmentShadingTransparencyVariants "${1}" "$2"

  # MSM (Moment shadow mapping)
  addMeshFragmentShadingTransparencyVariants "${1}_msm" "$2 -DMSM"

  # PCF (Percentage closer filtering) / PCSS (Percentage closer soft shadows) / DPCF (a PCF variant)
  addMeshFragmentShadingTransparencyVariants "${1}_pcfpcss" "$2 -DPCFPCSS"
}

# Add mesh fragment shader variants with different antialiasing techniques (if any)
addMeshFragmentShadingAntialiasingVariants(){
  
  # No antialiasing or temporal antialiasing
  addMeshFragmentShadingShadowVariants "${1}" "$2"

  if [[ $2 != *"ENVMAP"* ]]; then

    # MSAA (Multi-sample anti-aliasing)
    addMeshFragmentShadingShadowVariants "${1}_msaa" "$2 -DMSAA"  

  fi
  
}

# Add mesh fragment shader depth only with different alphatest variants 
addMeshFragmentDepthOnlyAlphaTestVariants(){
  
  # No alpha test
  addMeshFragmentShader "${1}" "$2"

  # Alpha test
  addMeshFragmentShader "${1}_alphatest" "$2 -DALPHATEST"
  
  # Alpha test with demote
  addMeshFragmentShader "${1}_alphatest_demote" "$2 -DALPHATEST -DUSEDEMOTE"

  # Alpha test without discard
  addMeshFragmentShader "${1}_alphatest_nodiscard" "$2 -DALPHATEST -DNODISCARD"

}

# Add mesh fragment shader depth only variants
addMeshFragmentDepthOnlyVariants(){

  # Depth only
  addMeshFragmentDepthOnlyAlphaTestVariants "${1}" "$2"

  # Depth and velocity
  addMeshFragmentDepthOnlyAlphaTestVariants "${1}_velocity" "$2 -DVELOCITY"

}

# Add mesh fragment shader variants with different pass targets
addMeshFragmentPassTargetVariants(){
  
  # Depth only stuff
  addMeshFragmentDepthOnlyVariants "${1}_depth" "$2 -DDEPTHONLY"  

  # The actual shading stuff
  addMeshFragmentShadingAntialiasingVariants "${1}_shading" "$2 -DFRUSTUMCLUSTERGRID -DDECALS -DLIGHTS -DSHADOWS"  

  # The environment map stuff
  #addMeshFragmentShadingAntialiasingVariants "${1}_envmap" "$2 -DFRUSTUMCLUSTERGRID -DDECALS -DLIGHTS -DSHADOWS -DENVMAP"

}

# Add mesh fragment shader variants with different Z direction
addMeshFragmentZVariants(){

  # Normal Z direction
  addMeshFragmentPassTargetVariants "${1}" "$2"

  # Reversed Z direction
  addMeshFragmentPassTargetVariants "${1}_reversedz" "$2 -DREVERSEDZ"

}

# Add mesh fragment shader variants with different material source
addMeshFragmentMaterialSourceVariants(){
  
  # Material access per SSBO => the old-school way for older GPUs
  addMeshFragmentZVariants "${1}_matssbo" "$2 -DUSE_MATERIAL_SSBO"

  # Material access per buffer references (pointer-like raw access inside shaders) => the more modern way for newer GPUs
  addMeshFragmentZVariants "${1}_matbufref" "$2 -DUSE_MATERIAL_BUFFER_REFERENCE"

} 

addMeshFragmentMaterialSourceVariants "mesh" ""

#############################################
#   Deduplication code for shader binaries  # 
#############################################

deduplicate_spv_files() {

  # Initialize an associative array to store checksums
  declare -A checksums

  # Create a new virtualsymlinks.json
  echo -n "{" > virtualsymlinks.json

  is_first_entry=1

  # Iterate over each *.spv file
  for file in *.spv; do

    # Calculate the SHA256 checksum for the file
    checksum=$(sha256sum "$file" | awk '{print $1}')

    # Check if this checksum is already in the array
    if [[ -z "${checksums[$checksum]}" ]]; then

      # If not, store the filename with this checksum
      checksums[$checksum]="$file"

    else

      # If the checksum matches, do a byte-for-byte comparison using cmp, for safety in case of hash collisions
      if cmp -s "$file" "${checksums[$checksum]}"; then

        # Add "," to the virtualsymlinks.json if not the first entry
        if [ $is_first_entry -eq 0 ]; then
          echo -n "," >> virtualsymlinks.json
        else
          is_first_entry=0 
        fi
        
        # Escape special JSON characters in filenames (basic version, might need enhancement based on filename specifics)
        json_key=$(echo "$file" | sed 's/"/\\"/g')
        json_value=$(echo "${checksums[$checksum]}" | sed 's/"/\\"/g')

        # Files are identical, so we consider $file as a duplicate and add a virtual symlink to the original file for the PasVulkan Scene3D asset manager
        echo -n "\"$json_key\":\"$json_value\"" >> virtualsymlinks.json

        # Delete the duplicate file
        rm "$file"
      
      fi
    fi
  done

  # Finish the virtualsymlinks.json
  echo -n "}" >> virtualsymlinks.json

}

#############################################
#                Main code                  #
#############################################

glslangValidatorPath=$(which glslangValidator)
spirvOptPath=$(which spirv-opt)

# Compile all shaders

echo "Compiling . . ."

for index in ${!compileshaderarguments[@]}; do
   parameters=${compileshaderarguments[$index]}
   # echo "Processing $parameters . . ."
   (     
     ${glslangValidatorPath} $parameters
     #--target-env spirv1.5 
     #>/dev/null
   ) & 
done

wait 

# Optimize all shaders

#echo "Optimizing . . ."

#for index in ${!compileshaderarguments[@]}; do
#   (
#     ${spirvOptPath} -O ${compileshaderarguments[$index]} -o ${compileshaderarguments[$index]}.opt
     #>/dev/null
#   ) & 
#done

# Deduplicate possible duplicate shader binaries

deduplicate_spv_files

# Pack all shaders into a zip file

echo "Packing . . ."

#cp -f *.spv ../../../assets/shaders/

rm -f scene3dshaders.zip
zip -m9 scene3dshaders.zip *.spv virtualsymlinks.json

# Delete all shader binaries and virtualsymlinks.json

rm -f *.spv
rm -f virtualsymlinks.json

# Done!

echo "Done!"

# And exit!

exit 0

# That's all!