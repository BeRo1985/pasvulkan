#!/bin/bash

################################################################################################################### 
#### This script compiles all necessary shaders for the Scene3D sub-framework-part of the PasVulkan framework. ####
###################################################################################################################

#############################################
#            Initialization code            #
#############################################

# Get the number of logical CPU cores
countCPUCores=$( ls -d /sys/devices/system/cpu/cpu[[:digit:]]* | wc -w )

# Check if bash version is equal or greater then 4.1 for `wait -n` support
if [ "${BASH_VERSINFO[0]}" -gt 4 ]; then
  bashVersionEqualOrGreaterThan4_1=1
elif [ "${BASH_VERSINFO[0]}" -eq 4 ] && [ "${BASH_VERSINFO[1]}" -ge 1 ]; then
  bashVersionEqualOrGreaterThan4_1=1
else
  bashVersionEqualOrGreaterThan4_1=0  
fi

# Get our current directory
originalDirectory="$(pwd)"
if [ $? -ne 0 ]; then
  echo "Failed to get current directory"
  exit 1
fi

# Get and create a temporary directory
tempPath="$(mktemp -d)"
#tempPath="$(mktemp -d -u -p ${HOME}/.temp/)"
if [ $? -ne 0 ]; then
  echo "Failed to create temporary directory"
  exit 1
fi

#mkdir -p "${tempPath}"
#if [ $? -ne 0 ]; then
#  echo "Failed to create temporary directory"
#  exit 1
#fi

#############################################
#          Predefined shader list           #
#############################################

compileshaderarguments=(
  
  "-V downsample.comp -DR11G11B10F -DMIPMAPLEVEL=0 -o ${tempPath}/downsample_r11g11b10f_level0_comp.spv"
  "-V downsample.comp -DR11G11B10F -DMIPMAPLEVEL=1 -o ${tempPath}/downsample_r11g11b10f_level1_comp.spv"
  "-V downsample.comp -DR11G11B10F -DMIPMAPLEVEL=2 -o ${tempPath}/downsample_r11g11b10f_level2_comp.spv"
  "-V downsample.comp -DR11G11B10F -DMIPMAPLEVEL=0 -DMULTIVIEW -o ${tempPath}/downsample_r11g11b10f_multiview_level0_comp.spv"
  "-V downsample.comp -DR11G11B10F -DMIPMAPLEVEL=1 -DMULTIVIEW -o ${tempPath}/downsample_r11g11b10f_multiview_level1_comp.spv"
  "-V downsample.comp -DR11G11B10F -DMIPMAPLEVEL=2 -DMULTIVIEW -o ${tempPath}/downsample_r11g11b10f_multiview_level2_comp.spv"

  "-V downsample.comp -DRGBA16F -DMIPMAPLEVEL=0 -o ${tempPath}/downsample_rgba16f_level0_comp.spv"
  "-V downsample.comp -DRGBA16F -DMIPMAPLEVEL=1 -o ${tempPath}/downsample_rgba16f_level1_comp.spv"
  "-V downsample.comp -DRGBA16F -DMIPMAPLEVEL=2 -o ${tempPath}/downsample_rgba16f_level2_comp.spv"
  "-V downsample.comp -DRGBA16F -DMIPMAPLEVEL=0 -DMULTIVIEW -o ${tempPath}/downsample_rgba16f_multiview_level0_comp.spv"
  "-V downsample.comp -DRGBA16F -DMIPMAPLEVEL=1 -DMULTIVIEW -o ${tempPath}/downsample_rgba16f_multiview_level1_comp.spv"
  "-V downsample.comp -DRGBA16F -DMIPMAPLEVEL=2 -DMULTIVIEW -o ${tempPath}/downsample_rgba16f_multiview_level2_comp.spv"

  "-V cull_depth_resolve.comp -o ${tempPath}/cull_depth_resolve_comp.spv"
  "-V cull_depth_resolve.comp -DREVERSEDZ -o ${tempPath}/cull_depth_resolve_reversedz_comp.spv"
  "-V cull_depth_resolve.comp -DMSAA -o ${tempPath}/cull_depth_resolve_depth_msaa_comp.spv"
  "-V cull_depth_resolve.comp -DMSAA -DREVERSEDZ -o ${tempPath}/cull_depth_resolve_msaa_reversedz_comp.spv"
  "-V cull_depth_resolve.comp -DMULTIVIEW -o ${tempPath}/cull_depth_resolve_multiview_comp.spv"
  "-V cull_depth_resolve.comp -DMULTIVIEW -DREVERSEDZ -o ${tempPath}/cull_depth_resolve_multiview_reversedz_comp.spv"
  "-V cull_depth_resolve.comp -DMULTIVIEW -DMSAA -o ${tempPath}/cull_depth_resolve_multiview_msaa_comp.spv"
  "-V cull_depth_resolve.comp -DMULTIVIEW -DMSAA -DREVERSEDZ -o ${tempPath}/cull_depth_resolve_multiview_msaa_reversedz_comp.spv"

  "-V downsample_culldepthpyramid.comp -DMIPMAPLEVEL=0 -o ${tempPath}/downsample_culldepthpyramid_level0_comp.spv"
  "-V downsample_culldepthpyramid.comp -DMIPMAPLEVEL=0 -DREVERSEDZ -o ${tempPath}/downsample_culldepthpyramid_reversedz_level0_comp.spv"
  "-V downsample_culldepthpyramid.comp -DMIPMAPLEVEL=0 -DMULTIVIEW -o ${tempPath}/downsample_culldepthpyramid_multiview_level0_comp.spv"
  "-V downsample_culldepthpyramid.comp -DMIPMAPLEVEL=0 -DMULTIVIEW -DREVERSEDZ -o ${tempPath}/downsample_culldepthpyramid_multiview_reversedz_level0_comp.spv"
  "-V downsample_culldepthpyramid.comp -DMIPMAPLEVEL=1 -o ${tempPath}/downsample_culldepthpyramid_level1_comp.spv"
  "-V downsample_culldepthpyramid.comp -DMIPMAPLEVEL=1 -DMULTIVIEW -o ${tempPath}/downsample_culldepthpyramid_multiview_level1_comp.spv"
  "-V downsample_culldepthpyramid.comp -DMIPMAPLEVEL=1 -DREVERSEDZ -o ${tempPath}/downsample_culldepthpyramid_reversedz_level1_comp.spv"
  "-V downsample_culldepthpyramid.comp -DMIPMAPLEVEL=1 -DMULTIVIEW -DREVERSEDZ -o ${tempPath}/downsample_culldepthpyramid_multiview_reversedz_level1_comp.spv"
  "-V downsample_culldepthpyramid.comp -DMULTIPASS -o ${tempPath}/downsample_culldepthpyramid_multipass_comp.spv"
  "-V downsample_culldepthpyramid.comp -DMULTIPASS -DMULTIVIEW -o ${tempPath}/downsample_culldepthpyramid_multiview_multipass_comp.spv"
  "-V downsample_culldepthpyramid.comp -DMULTIPASS -DREVERSEDZ -o ${tempPath}/downsample_culldepthpyramid_reversedz_multipass_comp.spv"
  "-V downsample_culldepthpyramid.comp -DMULTIPASS -DMULTIVIEW -DREVERSEDZ -o ${tempPath}/downsample_culldepthpyramid_multiview_reversedz_multipass_comp.spv"

  "-V downsample_depth.comp -DMIPMAPLEVEL=0 -o ${tempPath}/downsample_depth_level0_comp.spv"
  "-V downsample_depth.comp -DMIPMAPLEVEL=0 -DREVERSEDZ -o ${tempPath}/downsample_depth_reversedz_level0_comp.spv"
  "-V downsample_depth.comp -DMIPMAPLEVEL=0 -DMSAA -o ${tempPath}/downsample_depth_msaa_level0_comp.spv"
  "-V downsample_depth.comp -DMIPMAPLEVEL=0 -DMSAA -DREVERSEDZ -o ${tempPath}/downsample_depth_msaa_reversedz_level0_comp.spv"
  "-V downsample_depth.comp -DMIPMAPLEVEL=0 -DMULTIVIEW -o ${tempPath}/downsample_depth_multiview_level0_comp.spv"
  "-V downsample_depth.comp -DMIPMAPLEVEL=0 -DMULTIVIEW -DREVERSEDZ -o ${tempPath}/downsample_depth_multiview_reversedz_level0_comp.spv"
  "-V downsample_depth.comp -DMIPMAPLEVEL=0 -DMULTIVIEW -DMSAA -o ${tempPath}/downsample_depth_multiview_msaa_level0_comp.spv"
  "-V downsample_depth.comp -DMIPMAPLEVEL=0 -DMULTIVIEW -DMSAA -DREVERSEDZ -o ${tempPath}/downsample_depth_multiview_msaa_reversedz_level0_comp.spv"
  "-V downsample_depth.comp -DMIPMAPLEVEL=1 -o ${tempPath}/downsample_depth_level1_comp.spv"
  "-V downsample_depth.comp -DMIPMAPLEVEL=1 -DMULTIVIEW -o ${tempPath}/downsample_depth_multiview_level1_comp.spv"
  "-V downsample_depth.comp -DMIPMAPLEVEL=1 -DREVERSEDZ -o ${tempPath}/downsample_depth_reversedz_level1_comp.spv"
  "-V downsample_depth.comp -DMIPMAPLEVEL=1 -DMULTIVIEW -DREVERSEDZ -o ${tempPath}/downsample_depth_multiview_reversedz_level1_comp.spv"
  
  "-V dof_autofocus.comp -o ${tempPath}/dof_autofocus_comp.spv"
  "-V dof_bokeh.comp -o ${tempPath}/dof_bokeh_comp.spv"
  "-V dof_prepare.frag -o ${tempPath}/dof_prepare_frag.spv"
  "-V dof_prefilter.frag -o ${tempPath}/dof_prefilter_frag.spv"
  "-V dof_blur.frag -o ${tempPath}/dof_blur_frag.spv"
  "-V dof_bruteforce.frag -o ${tempPath}/dof_bruteforce_frag.spv"
  "-V dof_postblur.frag -o ${tempPath}/dof_postblur_frag.spv"
  "-V dof_combine.frag -o ${tempPath}/dof_combine_frag.spv"
  "-V dof_gather.frag -DPASS1 -o ${tempPath}/dof_gather_pass1_frag.spv"
  "-V dof_gather.frag -DPASS2 -o ${tempPath}/dof_gather_pass2_frag.spv"
  "-V dof_resolve.frag -o ${tempPath}/dof_resolve_frag.spv"

  "-V luminance_histogram.comp -o ${tempPath}/luminance_histogram_comp.spv"
  "-V luminance_histogram.comp -DMULTIVIEW -o ${tempPath}/luminance_histogram_multiview_comp.spv"

  "-V luminance_average.comp -o ${tempPath}/luminance_average_comp.spv"

  "-V luminance_adaptation.frag -o ${tempPath}/luminance_adaptation_frag.spv" 

  "-V frustumclustergridbuild.comp -o ${tempPath}/frustumclustergridbuild_comp.spv"
  "-V frustumclustergridbuild.comp -DREVERSEDZ -o ${tempPath}/frustumclustergridbuild_reversedz_comp.spv"

  "-V frustumclustergridassign.comp -o ${tempPath}/frustumclustergridassign_comp.spv"
  
  "-V lens_upsample.comp -DR11G11B10F -o ${tempPath}/lens_upsample_r11g11b10f_comp.spv"
  "-V lens_upsample.comp -DRGBA16F -o ${tempPath}/lens_upsample_rgba16f_comp.spv"
  "-V lens_upsample.comp -DR11G11B10F -DMULTIVIEW -o ${tempPath}/lens_upsample_r11g11b10f_multiview_comp.spv"
  "-V lens_upsample.comp -DRGBA16F -DMULTIVIEW -o ${tempPath}/lens_upsample_rgba16f_multiview_comp.spv"
  "-V lens_resolve.frag -o ${tempPath}/lens_resolve_frag.spv"

  "-V lens_color.frag -o ${tempPath}/lens_color_frag.spv"
  "-V lens_dirt.frag -o ${tempPath}/lens_dirt_frag.spv"
  "-V lens_star.frag -o ${tempPath}/lens_star_frag.spv"
  
  "-V mesh.comp -o ${tempPath}/mesh_comp.spv"
  "-V mesh.comp -DRAYTRACING -o ${tempPath}/mesh_raytracing_comp.spv"

  "-V mesh_cull.comp -DPASS=0 -o ${tempPath}/mesh_cull_pass0_comp.spv"
  "-V mesh_cull.comp -DPASS=1 -o ${tempPath}/mesh_cull_pass1_comp.spv"

  "-V mesh.vert -o ${tempPath}/mesh_vert.spv"
  "-V mesh.vert -DVELOCITY -o ${tempPath}/mesh_velocity_vert.spv"
  "-V mesh.vert -DVOXELIZATION -o ${tempPath}/mesh_voxelization_vert.spv"

  "-V gi_voxel_occlusion_transfer.comp -o ${tempPath}/gi_voxel_occlusion_transfer_comp.spv"
  "-V gi_voxel_occlusion_mipmap.comp -o ${tempPath}/gi_voxel_occlusion_mipmap_comp.spv"

  "-V gi_voxel_radiance_transfer.comp -o ${tempPath}/gi_voxel_radiance_transfer_comp.spv"
  "-V gi_voxel_radiance_transfer.comp -DUSESHADERBUFFERFLOAT32ATOMICADD -o ${tempPath}/gi_voxel_radiance_transfer_float_comp.spv"

  "-V gi_voxel_radiance_mipmap.comp -o ${tempPath}/gi_voxel_radiance_mipmap_comp.spv"
  
  "-V mesh_voxelization.geom -DCOUNT_CLIPMAPS=1 -o ${tempPath}/mesh_voxelization_1_geom.spv"
  "-V mesh_voxelization.geom -DCOUNT_CLIPMAPS=2 -o ${tempPath}/mesh_voxelization_2_geom.spv"
  "-V mesh_voxelization.geom -DCOUNT_CLIPMAPS=3 -o ${tempPath}/mesh_voxelization_3_geom.spv"
  "-V mesh_voxelization.geom -DCOUNT_CLIPMAPS=4 -o ${tempPath}/mesh_voxelization_4_geom.spv"
  "-V mesh_voxelization.geom -DCOUNT_CLIPMAPS=5 -o ${tempPath}/mesh_voxelization_5_geom.spv"
  "-V mesh_voxelization.geom -DCOUNT_CLIPMAPS=6 -o ${tempPath}/mesh_voxelization_6_geom.spv"
  "-V mesh_voxelization.geom -DCOUNT_CLIPMAPS=7 -o ${tempPath}/mesh_voxelization_7_geom.spv"
  "-V mesh_voxelization.geom -DCOUNT_CLIPMAPS=8 -o ${tempPath}/mesh_voxelization_8_geom.spv"
   
  "-V mesh_voxelization.comp -o ${tempPath}/mesh_voxelization_comp.spv"

  "-V particle_voxelization.geom -DCOUNT_CLIPMAPS=1 -o ${tempPath}/particle_voxelization_1_geom.spv"
  "-V particle_voxelization.geom -DCOUNT_CLIPMAPS=2 -o ${tempPath}/particle_voxelization_2_geom.spv"
  "-V particle_voxelization.geom -DCOUNT_CLIPMAPS=3 -o ${tempPath}/particle_voxelization_3_geom.spv"
  "-V particle_voxelization.geom -DCOUNT_CLIPMAPS=4 -o ${tempPath}/particle_voxelization_4_geom.spv"
  "-V particle_voxelization.geom -DCOUNT_CLIPMAPS=5 -o ${tempPath}/particle_voxelization_5_geom.spv"
  "-V particle_voxelization.geom -DCOUNT_CLIPMAPS=6 -o ${tempPath}/particle_voxelization_6_geom.spv"
  "-V particle_voxelization.geom -DCOUNT_CLIPMAPS=7 -o ${tempPath}/particle_voxelization_7_geom.spv"
  "-V particle_voxelization.geom -DCOUNT_CLIPMAPS=8 -o ${tempPath}/particle_voxelization_8_geom.spv"
  
  "-V mboit_resolve.frag -o ${tempPath}/mboit_resolve_frag.spv"
  "-V mboit_resolve.frag -DMSAA -o ${tempPath}/mboit_resolve_msaa_frag.spv"
  "-V wboit_resolve.frag -o ${tempPath}/wboit_resolve_frag.spv"
  "-V wboit_resolve.frag -DMSAA -o ${tempPath}/wboit_resolve_msaa_frag.spv"
  
  "-V lockoit_resolve.frag -o ${tempPath}/lockoit_resolve_frag.spv"
  "-V lockoit_resolve.frag -DREVERSEDZ -o ${tempPath}/lockoit_resolve_reversedz_frag.spv"
  "-V lockoit_resolve.frag -DMSAA -o ${tempPath}/lockoit_resolve_msaa_frag.spv"
  "-V lockoit_resolve.frag -DMSAA -DREVERSEDZ -o ${tempPath}/lockoit_resolve_reversedz_msaa_frag.spv"

  "-V loopoit_resolve.frag -o ${tempPath}/loopoit_resolve_frag.spv"
  "-V loopoit_resolve.frag -DREVERSEDZ -o ${tempPath}/loopoit_resolve_reversedz_frag.spv"
  "-V loopoit_resolve.frag -DMSAA -o ${tempPath}/loopoit_resolve_msaa_frag.spv"
  "-V loopoit_resolve.frag -DMSAA -DREVERSEDZ -o ${tempPath}/loopoit_resolve_reversedz_msaa_frag.spv"

  "-V dfaoit_resolve.frag -o ${tempPath}/dfaoit_resolve_frag.spv"
  "-V dfaoit_resolve.frag -DREVERSEDZ -o ${tempPath}/dfaoit_resolve_reversedz_frag.spv"
  "-V dfaoit_resolve.frag -DMSAA -o ${tempPath}/dfaoit_resolve_msaa_frag.spv"
  "-V dfaoit_resolve.frag -DMSAA -DREVERSEDZ -o ${tempPath}/dfaoit_resolve_reversedz_msaa_frag.spv"

  "-V blend_resolve.frag -o ${tempPath}/blend_resolve_frag.spv"
  "-V blend_resolve.frag -DMSAA -o ${tempPath}/blend_resolve_msaa_frag.spv"  

  "-V brdf_charlie.frag -o ${tempPath}/brdf_charlie_frag.spv"
  "-V brdf_ggx.frag -o ${tempPath}/brdf_ggx_frag.spv"
  
  "-V brdf_sheen_e.frag -o ${tempPath}/brdf_sheen_e_frag.spv"
  "-V brdf_sheen_e.frag -DFAST -o ${tempPath}/brdf_sheen_e_fast_frag.spv"
    
  "-V fullscreen.vert -o ${tempPath}/fullscreen_vert.spv"
  
  "-V cubemap.vert -o ${tempPath}/cubemap_vert.spv"
  "-V cubemap_sky.comp -o ${tempPath}/cubemap_sky_comp.spv"
  "-V cubemap_sky.comp -DFAST -o ${tempPath}/cubemap_sky_fast_comp.spv"
  "-V cubemap_sky.frag -o ${tempPath}/cubemap_sky_frag.spv"
  "-V cubemap_sky.frag -DFAST -o ${tempPath}/cubemap_sky_fast_frag.spv"
  "-V cubemap_filter.comp -o ${tempPath}/cubemap_filter_comp.spv"
  
  "-V passthrough.vert -o ${tempPath}/passthrough_vert.spv"
  
  "-V dummy.frag -o ${tempPath}/dummy_frag.spv"

  "-V dithering.frag -o ${tempPath}/dithering_frag.spv"

  "-V debug_blit.frag -o ${tempPath}/debug_blit_frag.spv"
  
  "-V skybox.vert -o ${tempPath}/skybox_vert.spv"
  "-V skybox.frag -o ${tempPath}/skybox_frag.spv"
  
  "-V skybox_realtime.frag -o ${tempPath}/skybox_realtime_frag.spv"
  
  "-V tonemapping.frag -o ${tempPath}/tonemapping_frag.spv"
  
  "-V antialiasing_dsaa.frag -o ${tempPath}/antialiasing_dsaa_frag.spv"
  "-V antialiasing_fxaa.frag -o ${tempPath}/antialiasing_fxaa_frag.spv"
  "-V antialiasing_taa.frag -o ${tempPath}/antialiasing_taa_frag.spv"
  "-V antialiasing_none.frag -o ${tempPath}/antialiasing_none_frag.spv"
  
  "-V antialiasing_smaa_blend.vert -o ${tempPath}/antialiasing_smaa_blend_vert.spv"
  "-V antialiasing_smaa_blend.frag -o ${tempPath}/antialiasing_smaa_blend_frag.spv"
  
  "-V antialiasing_smaa_edges.vert -o ${tempPath}/antialiasing_smaa_edges_vert.spv"
  "-V antialiasing_smaa_edges.frag -o ${tempPath}/antialiasing_smaa_edges_color_frag.spv"
  "-V antialiasing_smaa_edges.frag -DLUMA -o ${tempPath}/antialiasing_smaa_edges_luma_frag.spv"
  
  "-V antialiasing_smaa_weights.vert -o ${tempPath}/antialiasing_smaa_weights_vert.spv"
  "-V antialiasing_smaa_weights.frag -o ${tempPath}/antialiasing_smaa_weights_frag.spv"
  
  "-V blit.frag -o ${tempPath}/blit_frag.spv"

  "-V msaa_resolve.frag -o ${tempPath}/msaa_resolve_frag.spv"
  
  "-V msm_blur.frag -o ${tempPath}/msm_blur_frag.spv"
  "-V msm_blur.vert -o ${tempPath}/msm_blur_vert.spv"
  
  "-V msm_resolve.frag -o ${tempPath}/msm_resolve_frag.spv"
  "-V msm_resolve.frag -DMSAA -o ${tempPath}/msm_resolve_msaa_frag.spv"
  "-V msm_resolve.vert -o ${tempPath}/msm_resolve_vert.spv"

  "-V ssao.frag -o ${tempPath}/ssao_frag.spv"
  "-V ssao.frag -DMULTIVIEW -o ${tempPath}/ssao_multiview_frag.spv"

  "-V ssao_blur.frag -o ${tempPath}/ssao_blur_frag.spv"

  "-V contentprojection.frag -o ${tempPath}/contentprojection_frag.spv"
  "-V contentprojection.frag -DREVERSEDZ -o ${tempPath}/contentprojection_reversedz_frag.spv"

  "-V mipmap.comp -DLEVEL0 -o ${tempPath}/mipmap_level0_comp.spv"
  "-V mipmap.comp -DLEVEL1 -o ${tempPath}/mipmap_level1_comp.spv"

  "-V debug_primitive.vert -o ${tempPath}/debug_primitive_vert.spv"
  "-V debug_primitive.frag -o ${tempPath}/debug_primitive_frag.spv"
  "-V debug_primitive.frag -DVELOCITY -o ${tempPath}/debug_primitive_velocity_frag.spv"

  "-V particle.vert -o ${tempPath}/particle_vert.spv"
  "-V particle.vert -DVOXELIZATION -o ${tempPath}/particle_voxelization_vert.spv"

  "-V resampling.frag -o ${tempPath}/resampling_frag.spv"
   
  "-V cubemap_sphericalharmonics.comp -o ${tempPath}/cubemap_sphericalharmonics_comp.spv"
  
  "-V cubemap_sphericalharmonics_accumulation.comp -o ${tempPath}/cubemap_sphericalharmonics_accumulation_comp.spv" 
  "-V cubemap_sphericalharmonics_accumulation.comp -DUSE_ATOMIC_FLOATS -o ${tempPath}/cubemap_sphericalharmonics_accumulation_atomicfloats_comp.spv"
  
  "-V cubemap_sphericalharmonics_normalization.comp -o ${tempPath}/cubemap_sphericalharmonics_normalization_comp.spv"

  "-V topdownskyocclusionmap_resolve.frag -o ${tempPath}/topdownskyocclusionmap_resolve_frag.spv"

  "-V topdownskyocclusionmap_blur.frag -o ${tempPath}/topdownskyocclusionmap_blur_frag.spv"

  "-V gi_cascaded_radiance_hints_inject_cached.comp -o ${tempPath}/gi_cascaded_radiance_hints_inject_cached_comp.spv"

  "-V gi_cascaded_radiance_hints_inject_sky.comp -o ${tempPath}/gi_cascaded_radiance_hints_inject_sky_comp.spv"
  
  "-V gi_cascaded_radiance_hints_inject_rsm.comp -o ${tempPath}/gi_cascaded_radiance_hints_inject_rsm_comp.spv"

  "-V gi_cascaded_radiance_hints_bounce.comp -o ${tempPath}/gi_cascaded_radiance_hints_bounce_comp.spv"

  "-V voxel_visualization.vert -o ${tempPath}/voxel_visualization_vert.spv"
  "-V voxel_visualization.frag -o ${tempPath}/voxel_visualization_frag.spv"
  "-V voxel_visualization.frag -DUSEDEMOTE -o ${tempPath}/voxel_visualization_demote_frag.spv"

  "-V voxel_mesh_visualization.vert -o ${tempPath}/voxel_mesh_visualization_vert.spv"
  "-V voxel_mesh_visualization.vert -DUSEGEOMETRYSHADER -o ${tempPath}/voxel_mesh_visualization_geometry_vert.spv"
  "-V voxel_mesh_visualization.geom -o ${tempPath}/voxel_mesh_visualization_geom.spv"
  "-V voxel_mesh_visualization.frag -o ${tempPath}/voxel_mesh_visualization_frag.spv"

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
  addShader "-V particle.frag ${2} -o ${tempPath}/${1}_frag.spv"
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

# Add particle fragment shader variants with different voxelization modes 
addParticleFragmentVoxelizationVariants(){
    
  # Voxelization
  addParticleFragmentShader "${1}_voxelization" "$2 -DVOXELIZATION"
    
}

# Add particle fragment shader variants with different techniques (if any)
addParticleFragmentVariants(){
  
  addParticleFragmentZVariants "${1}" "$2"

  addParticleFragmentVoxelizationVariants "${1}" "$2"
  
}

addParticleFragmentVariants "particle" ""

#############################################
#               Mesh shaders                #
#############################################

addMeshFragmentShader(){
  addShader "-V mesh.frag ${2} -o ${tempPath}/${1}_frag.spv"
}

# Add mesh fragment shader variants with different alpha test techniques (if any)
addMeshFragmentShadingAlphaTestVariants(){
  addMeshFragmentShader "$1" "$2"
  addMeshFragmentShader "${1}_alphatest" "$2 -DALPHATEST"
  addMeshFragmentShader "${1}_alphatest_demote" "$2 -DALPHATEST -DUSEDEMOTE"
  addMeshFragmentShader "${1}_alphatest_nodiscard" "$2 -DALPHATEST -DNODISCARD"
}

# Add mesh fragment shader variants with or without velocity output (if any)
addMeshFragmentShadingVelocityVariants(){
  addMeshFragmentShadingAlphaTestVariants "$1" "$2"
  addMeshFragmentShadingAlphaTestVariants "${1}_velocity" "$2 -DVELOCITY"
}

# Add mesh fragment shader variants with different alpha test techniques (if any)
addMeshFragmentShadingOITAlphaTestVariants(){
  addMeshFragmentShader "$1" "$2"
  addMeshFragmentShader "${1}_alphatest" "$2 -DALPHATEST"
}

# Add mesh fragment shader variants with different transparency techniques (if any)
addMeshFragmentShadingTransparencyVariants(){

  # No blending   
  addMeshFragmentShadingVelocityVariants "$1" "$2"

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

# Add mesh fragment shader variants with different global illumination techniques (if any)
addMeshFragmentShadingGlobalIlluminationVariants(){
  
  # No global illumination
  addMeshFragmentShadingAntialiasingVariants "${1}" "$2"

  # Cascaded radiance hints
  addMeshFragmentShadingAntialiasingVariants "${1}_globalillumination_cascaded_radiance_hints" "$2 -DGLOBAL_ILLUMINATION_CASCADED_RADIANCE_HINTS"
  
  # Cascaded voxel cone tracing
  addMeshFragmentShadingAntialiasingVariants "${1}_globalillumination_cascaded_voxel_cone_tracing" "$2 -DGLOBAL_ILLUMINATION_CASCADED_VOXEL_CONE_TRACING"
  
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

}

# Add mesh fragment shader variants with different alpha test techniques (if any)
addMeshFragmentReflectiveShadowMapVariants(){
  addMeshFragmentShader "$1" "$2"
  addMeshFragmentShader "${1}_alphatest" "$2 -DALPHATEST"
  addMeshFragmentShader "${1}_alphatest_demote" "$2 -DALPHATEST -DUSEDEMOTE"
  addMeshFragmentShader "${1}_alphatest_nodiscard" "$2 -DALPHATEST -DNODISCARD"
}

# Add mesh fragment shader variants with different alpha test techniques (if any)
addMeshFragmentVoxelizationAlphaVariants(){
  addMeshFragmentShader "$1" "$2"
  addMeshFragmentShader "${1}_alphatest" "$2 -DALPHATEST"
  addMeshFragmentShader "${1}_alphatest_demote" "$2 -DALPHATEST -DUSEDEMOTE"
  addMeshFragmentShader "${1}_alphatest_nodiscard" "$2 -DALPHATEST -DNODISCARD"
}

# Add mesh fragment shader variants with different temporary voxel storage techniques 
addMeshFragmentVoxelizationVariants(){  
  addMeshFragmentVoxelizationAlphaVariants "$1" "$2" 
}  

# Add mesh fragment shader variants with different pass targets
addMeshFragmentPassTargetVariants(){
  
  # Depth only stuff
  addMeshFragmentDepthOnlyVariants "${1}_depth" "$2 -DDEPTHONLY"  

  # The reflective shadow map stuff
  addMeshFragmentReflectiveShadowMapVariants "${1}_rsm" "$2 -DFRUSTUMCLUSTERGRID -DDECALS -DLIGHTS -DLIGHTCLUSTERS -DSHADOWS -DREFLECTIVESHADOWMAPOUTPUT"  

  # The voxelization stuff
  addMeshFragmentVoxelizationVariants "${1}_voxelization" "$2 -DVOXELIZATION"  

  # The actual shading stuff
  addMeshFragmentShadingGlobalIlluminationVariants "${1}_shading" "$2 -DFRUSTUMCLUSTERGRID -DDECALS -DLIGHTS -DLIGHTCLUSTERS -DSHADOWS"  
    
  # The environment map stuff
  #addMeshFragmentShadingGlobalIlluminationVariants "${1}_envmap" "$2 -DFRUSTUMCLUSTERGRID -DDECALS -DLIGHTS -DLIGHTCLUSTERS -DSHADOWS -DENVMAP"

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

  # Go to the temporary directory
  cd "${tempPath}"

  # Initialize an associative array to store checksums
  declare -A checksums

  # Create a new virtualsymlinks.json
  echo -n "{" > "${tempPath}/virtualsymlinks.json"

  # Flag to check if this is the first entry in the virtualsymlinks.json
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
          echo -n "," >> "${tempPath}/virtualsymlinks.json"
        else
          is_first_entry=0 
        fi
        
        # Escape special JSON characters in filenames (basic version, might need enhancement based on filename specifics)
        json_key=$(echo "$file" | sed 's/"/\\"/g')
        json_value=$(echo "${checksums[$checksum]}" | sed 's/"/\\"/g')

        # Files are identical, so we consider $file as a duplicate and add a virtual symlink to the original file for the PasVulkan Scene3D asset manager
        echo -n "\"$json_key\":\"$json_value\"" >> "${tempPath}/virtualsymlinks.json"

        # Delete the duplicate file
        rm "$file"
      
      fi
    fi
  done

  # Finish the virtualsymlinks.json
  echo -n "}" >> "${tempPath}/virtualsymlinks.json"

  # Go back to the original directory
  cd "${originalDirectory}"

}

# Wait until there are less than $1 jobs running in parallel
function pwait() {
  if [ ${bashVersionEqualOrGreaterThan4_1} -eq 1 ]; then
    if [ $(jobs -p -r | wc -l) -ge $1 ]; then
      wait -n # Wait for any job to finish
    fi
  else  
    while [ $(jobs -p -r | wc -l) -ge $1 ]; do
      sleep 0.01s
    done
  fi
}

# Wait until there are less than the number of logical CPU cores jobs running in parallel
function throttleWait() {
  # A bit less than the number of logical CPU cores to leave some room for other processes
  pwait $((${countCPUCores}-1)) 
}

#############################################
#                Main code                  #
#############################################

glslangValidatorPath=$(which glslangValidator)
spirvOptPath=$(which spirv-opt)

# Use a trap to kill all child processes when an error occurs
trap 'kill 0' ERR

# Compile all shaders

echo "Compiling . . ."

#pids=()

for index in ${!compileshaderarguments[@]}; do
  parameters=${compileshaderarguments[$index]}
  # echo "Processing $parameters . . ."
  (     
    ${glslangValidatorPath} $parameters #--target-env spirv1.5 >/dev/null
    if [ $? -ne 0 ]; then
      echo "Error encountered. Stopping compilation."
      kill -s TERM 0
      exit 1
    fi     
  ) & 
  #pids+=("$!")
  throttleWait
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

# Go to the temporary directory
cd "${tempPath}"

# Get a sorted list of .spv files and virtualsymlinks.json without their full paths
toCompressFiles=( $((ls *.spv; echo virtualsymlinks.json) | sort) )

# Check if zipmerge is installed
if command -v zipmerge &> /dev/null; then

  # Create another temporary directory for the intermediate zip files
  zip_temp_dir=$(mktemp -d)
  if [ $? -ne 0 ]; then
    echo "Error creating temporary directory. Stopping compilation."
    exit 1
  fi

  # Parallel compression of each file in toCompressFiles array
  for file in "${toCompressFiles[@]}"; do
    ( 
      zip -9 "${zip_temp_dir}/${file}.zip" "${file}"
    ) &
    throttleWait
  done

  # Wait for all background jobs to complete
  wait

  # Get a sorted list of .zip files in zip_temp_dir with their full paths
  zip_files=( $(find "${zip_temp_dir}" -type f -name "*.zip" | sort) )

   # Create the zip archive using the zip files from zip_temp_dir
  zipmerge "${tempPath}/scene3dshaders.zip" "${zip_files[@]}"

  # Delete the temporary ZIP directory
  rm -rf "${zip_temp_dir}"

else

  # Create the zip archive with virtualsymlinks.json as the first entry
  zip -m9 scene3dshaders.zip "${toCompressFiles[@]}"

fi

cd "${originalDirectory}"

# Delete the old zip archive if it exists
if [ -f "${originalDirectory}/scene3dshaders.zip" ]; then
  rm -f "${originalDirectory}/scene3dshaders.zip"
fi

# Copy the zip archive to the current directory
cp -f "${tempPath}/scene3dshaders.zip" "${originalDirectory}/scene3dshaders.zip"

# Compile bin2c

clang ./bin2c.c -o "${tempPath}/bin2c"

# Convert the zip archive to a C header file
"$tempPath/bin2c" scene3dshaders.zip pasvulkan_scene3dshaders_zip "${tempPath}/scene3dshaders_zip.c"

# Compile the C header file for all platforms in parallel

# Compile for x86-32 Linux
clang -c -target i386-linux -Wno-c++2b-extensions -Wno-return-type -Wno-deprecated -O0 "${tempPath}/scene3dshaders_zip.c" -o scene3dshaders_zip_x86_32_linux.o &
throttleWait

# Compile for x86-64 Linux
clang -c -target x86_64-linux -Wno-c++2b-extensions -Wno-return-type -Wno-deprecated -O0 "${tempPath}/scene3dshaders_zip.c" -o scene3dshaders_zip_x86_64_linux.o & 
throttleWait

# Compile for x86-32 Windows
clang -c -target i386-windows -Wno-c++2b-extensions -Wno-return-type -Wno-deprecated -O0 "${tempPath}/scene3dshaders_zip.c" -o scene3dshaders_zip_x86_32_windows.o &
throttleWait

# Compile for x86-64 Windows
clang -c -target x86_64-windows -Wno-c++2b-extensions -Wno-return-type -Wno-deprecated -O0 "${tempPath}/scene3dshaders_zip.c" -o scene3dshaders_zip_x86_64_windows.o &
throttleWait

# Compile for AArch64 Windows
clang -c -target aarch64-windows -Wno-c++2b-extensions -Wno-return-type -Wno-deprecated -O0 "${tempPath}/scene3dshaders_zip.c" -o scene3dshaders_zip_aarch64_windows.o &
throttleWait

# Compile for ARM32 Linux
clang -c -target armv7-linux -mfloat-abi=hard -Wno-c++2b-extensions -Wno-return-type -Wno-deprecated -O0 "${tempPath}/scene3dshaders_zip.c" -o scene3dshaders_zip_arm32_linux.o &
throttleWait

# Compile for AArch64 Linux
clang -c -target aarch64-linux -Wno-c++2b-extensions -Wno-return-type -Wno-deprecated -O0 "${tempPath}/scene3dshaders_zip.c" -o scene3dshaders_zip_aarch64_linux.o &
throttleWait

# Compile for x86-32 Android
clang -c -target i386-linux-android -Wno-c++2b-extensions -Wno-return-type -Wno-deprecated -O0 "${tempPath}/scene3dshaders_zip.c" -o scene3dshaders_zip_x86_32_android.o &
throttleWait

# Compile for x86-64 Android
clang -c -target x86_64-linux-android -Wno-c++2b-extensions -Wno-return-type -Wno-deprecated -O0 "${tempPath}/scene3dshaders_zip.c" -o scene3dshaders_zip_x86_64_android.o &
throttleWait

# Compile for ARM32 Android
clang -c -target armv7-linux-android -Wno-c++2b-extensions -Wno-return-type -Wno-deprecated -O0 "${tempPath}/scene3dshaders_zip.c" -o scene3dshaders_zip_arm32_android.o &
throttleWait

# Compile for AArch64 Android
clang -c -target aarch64-linux-android -Wno-c++2b-extensions -Wno-return-type -Wno-deprecated -O0 "${tempPath}/scene3dshaders_zip.c" -o scene3dshaders_zip_aarch64_android.o &
throttleWait

# Wait for all compilation jobs to finish
wait

# Delete the temporary directory
rm -rf "${tempPath}" # what actually deletes also the files in it

# Done!

echo "Done!"

# And exit!

exit 0

# That's all!