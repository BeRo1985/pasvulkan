#!/bin/bash

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

  '-V lightclustergridbuild.comp -o lightclustergridbuild_comp.spv'
  '-V lightclustergridbuild.comp -DREVERSEDZ -o lightclustergridbuild_reversedz_comp.spv'
  '-V lightclusterassign.comp -o lightclusterassign_comp.spv'
  
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

  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -o mesh_matbufref_msm_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -o mesh_matbufref_msm_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -DMSAA -o mesh_matbufref_msm_masked_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -DUSEDEMOTE -o mesh_matbufref_msm_masked_demote_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -DUSEDEMOTE -DMSAA -o mesh_matbufref_msm_masked_demote_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -DNODISCARD -o mesh_matbufref_msm_masked_nodiscard_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -DNODISCARD -DMSAA -o mesh_matbufref_msm_masked_nodiscard_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -DNODISCARD -DREVERSEDZ -o mesh_matbufref_msm_masked_nodiscard_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -DNODISCARD -DREVERSEDZ -DMSAA -o mesh_matbufref_msm_masked_nodiscard_reversedz_msaa_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DBLEND -o mesh_matbufref_msm_blend_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -o mesh_matbufref_msm_blend_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -DMSAA -o mesh_matbufref_msm_blend_masked_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -DUSEDEMOTE -o mesh_matbufref_msm_blend_masked_demote_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -DUSEDEMOTE -DMSAA -o mesh_matbufref_msm_blend_masked_demote_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -DNODISCARD -o mesh_matbufref_msm_blend_masked_nodiscard_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -DNODISCARD -DMSAA -o mesh_matbufref_msm_blend_masked_nodiscard_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -DNODISCARD -DREVERSEDZ -o mesh_matbufref_msm_blend_masked_nodiscard_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -DNODISCARD -DREVERSEDZ -DMSAA -o mesh_matbufref_msm_blend_masked_nodiscard_reversedz_msaa_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DWBOIT -o mesh_matbufref_msm_wboit_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DWBOIT -DALPHATEST -o mesh_matbufref_msm_wboit_masked_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DMBOIT -DMBOITPASS1 -DDEPTHONLY -o mesh_matbufref_msm_mboit_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DMBOIT -DALPHATEST -DMBOITPASS1 -DDEPTHONLY -o mesh_matbufref_msm_mboit_masked_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DMBOIT -DMBOITPASS2 -o mesh_matbufref_msm_mboit_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DMBOIT -DALPHATEST -DMBOITPASS2 -o mesh_matbufref_msm_mboit_masked_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DMBOIT -DMBOITPASS2 -DMSAA -o mesh_matbufref_msm_mboit_msaa_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DMBOIT -DALPHATEST -DMBOITPASS2 -DMSAA -o mesh_matbufref_msm_mboit_masked_msaa_pass2_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DDEPTHONLY -o mesh_matbufref_msm_loopoit_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DALPHATEST -DDEPTHONLY -o mesh_matbufref_msm_loopoit_masked_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DREVERSEDZ -DDEPTHONLY -o mesh_matbufref_msm_loopoit_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DREVERSEDZ -DALPHATEST -DDEPTHONLY -o mesh_matbufref_msm_loopoit_masked_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DDEPTHONLY -o mesh_matbufref_msm_loopoit_msaa_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DALPHATEST -DDEPTHONLY -o mesh_matbufref_msm_loopoit_msaa_masked_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DREVERSEDZ -DDEPTHONLY -o mesh_matbufref_msm_loopoit_msaa_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DREVERSEDZ -DALPHATEST -DDEPTHONLY -o mesh_matbufref_msm_loopoit_msaa_masked_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -o mesh_matbufref_msm_loopoit_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -DALPHATEST -o mesh_matbufref_msm_loopoit_masked_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -DREVERSEDZ -o mesh_matbufref_msm_loopoit_reversedz_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -DREVERSEDZ -DALPHATEST -o mesh_matbufref_msm_loopoit_masked_reversedz_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -o mesh_matbufref_msm_loopoit_msaa_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -DALPHATEST -o mesh_matbufref_msm_loopoit_msaa_masked_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -DREVERSEDZ -o mesh_matbufref_msm_loopoit_msaa_reversedz_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -DREVERSEDZ -DALPHATEST -o mesh_matbufref_msm_loopoit_msaa_masked_reversedz_pass2_frag.spv'

  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -o mesh_matbufref_msm_lockoit_spinlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -DALPHATEST -o mesh_matbufref_msm_lockoit_spinlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -DREVERSEDZ -o mesh_matbufref_msm_lockoit_spinlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -o mesh_matbufref_msm_lockoit_spinlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -o mesh_matbufref_msm_lockoit_interlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -DALPHATEST -o mesh_matbufref_msm_lockoit_interlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -DREVERSEDZ -o mesh_matbufref_msm_lockoit_interlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -o mesh_matbufref_msm_lockoit_interlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -DMSAA -o mesh_matbufref_msm_lockoit_spinlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -DALPHATEST -DMSAA -o mesh_matbufref_msm_lockoit_spinlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -DREVERSEDZ -DMSAA -o mesh_matbufref_msm_lockoit_spinlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matbufref_msm_lockoit_spinlock_reversedz_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -DMSAA -o mesh_matbufref_msm_lockoit_interlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -DALPHATEST -DMSAA -o mesh_matbufref_msm_lockoit_interlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -DREVERSEDZ -DMSAA -o mesh_matbufref_msm_lockoit_interlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matbufref_msm_lockoit_interlock_reversedz_masked_msaa_frag.spv'

  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -o mesh_matbufref_msm_dfaoit_spinlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -DALPHATEST -o mesh_matbufref_msm_dfaoit_spinlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -DREVERSEDZ -o mesh_matbufref_msm_dfaoit_spinlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -o mesh_matbufref_msm_dfaoit_spinlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -o mesh_matbufref_msm_dfaoit_interlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -DALPHATEST -o mesh_matbufref_msm_dfaoit_interlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -DREVERSEDZ -o mesh_matbufref_msm_dfaoit_interlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -o mesh_matbufref_msm_dfaoit_interlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -DMSAA -o mesh_matbufref_msm_dfaoit_spinlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -DALPHATEST -DMSAA -o mesh_matbufref_msm_dfaoit_spinlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -DREVERSEDZ -DMSAA -o mesh_matbufref_msm_dfaoit_spinlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matbufref_msm_dfaoit_spinlock_reversedz_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -DMSAA -o mesh_matbufref_msm_dfaoit_interlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -DALPHATEST -DMSAA -o mesh_matbufref_msm_dfaoit_interlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -DREVERSEDZ -DMSAA -o mesh_matbufref_msm_dfaoit_interlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matbufref_msm_dfaoit_interlock_reversedz_masked_msaa_frag.spv'

  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -o mesh_matbufref_pcfpcss_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -o mesh_matbufref_pcfpcss_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -DMSAA -o mesh_matbufref_pcfpcss_masked_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -DUSEDEMOTE -o mesh_matbufref_pcfpcss_masked_demote_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -DUSEDEMOTE -DMSAA -o mesh_matbufref_pcfpcss_masked_demote_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -DNODISCARD -o mesh_matbufref_pcfpcss_masked_nodiscard_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -DNODISCARD -DMSAA -o mesh_matbufref_pcfpcss_masked_nodiscard_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -DNODISCARD -DREVERSEDZ -o mesh_matbufref_pcfpcss_masked_nodiscard_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -DNODISCARD -DREVERSEDZ -DMSAA -o mesh_matbufref_pcfpcss_masked_nodiscard_reversedz_msaa_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -o mesh_matbufref_pcfpcss_blend_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -o mesh_matbufref_pcfpcss_blend_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -DMSAA -o mesh_matbufref_pcfpcss_blend_masked_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -DUSEDEMOTE -o mesh_matbufref_pcfpcss_blend_masked_demote_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -DUSEDEMOTE -DMSAA -o mesh_matbufref_pcfpcss_blend_masked_demote_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -DNODISCARD -o mesh_matbufref_pcfpcss_blend_masked_nodiscard_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -DNODISCARD -DMSAA -o mesh_matbufref_pcfpcss_blend_masked_nodiscard_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -DNODISCARD -DREVERSEDZ -o mesh_matbufref_pcfpcss_blend_masked_nodiscard_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -DNODISCARD -DREVERSEDZ -DMSAA -o mesh_matbufref_pcfpcss_blend_masked_nodiscard_reversedz_msaa_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DWBOIT -o mesh_matbufref_pcfpcss_wboit_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DWBOIT -DALPHATEST -o mesh_matbufref_pcfpcss_wboit_masked_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DMBOIT -DMBOITPASS1 -DDEPTHONLY -o mesh_matbufref_pcfpcss_mboit_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DMBOIT -DALPHATEST -DMBOITPASS1 -DDEPTHONLY -o mesh_matbufref_pcfpcss_mboit_masked_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DMBOIT -DMBOITPASS2 -o mesh_matbufref_pcfpcss_mboit_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DMBOIT -DALPHATEST -DMBOITPASS2 -o mesh_matbufref_pcfpcss_mboit_masked_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DMBOIT -DMBOITPASS2 -DMSAA -o mesh_matbufref_pcfpcss_mboit_msaa_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DMBOIT -DALPHATEST -DMBOITPASS2 -DMSAA -o mesh_matbufref_pcfpcss_mboit_masked_msaa_pass2_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DDEPTHONLY -o mesh_matbufref_pcfpcss_loopoit_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DALPHATEST -DDEPTHONLY -o mesh_matbufref_pcfpcss_loopoit_masked_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DREVERSEDZ -DDEPTHONLY -o mesh_matbufref_pcfpcss_loopoit_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DREVERSEDZ -DALPHATEST -DDEPTHONLY -o mesh_matbufref_pcfpcss_loopoit_masked_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DDEPTHONLY -o mesh_matbufref_pcfpcss_loopoit_msaa_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DALPHATEST -DDEPTHONLY -o mesh_matbufref_pcfpcss_loopoit_msaa_masked_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DREVERSEDZ -DDEPTHONLY -o mesh_matbufref_pcfpcss_loopoit_msaa_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DREVERSEDZ -DALPHATEST -DDEPTHONLY -o mesh_matbufref_pcfpcss_loopoit_msaa_masked_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -o mesh_matbufref_pcfpcss_loopoit_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -DALPHATEST -o mesh_matbufref_pcfpcss_loopoit_masked_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -DREVERSEDZ -o mesh_matbufref_pcfpcss_loopoit_reversedz_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -DREVERSEDZ -DALPHATEST -o mesh_matbufref_pcfpcss_loopoit_masked_reversedz_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -o mesh_matbufref_pcfpcss_loopoit_msaa_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -DALPHATEST -o mesh_matbufref_pcfpcss_loopoit_msaa_masked_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -DREVERSEDZ -o mesh_matbufref_pcfpcss_loopoit_msaa_reversedz_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -DREVERSEDZ -DALPHATEST -o mesh_matbufref_pcfpcss_loopoit_msaa_masked_reversedz_pass2_frag.spv'

  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -o mesh_matbufref_pcfpcss_lockoit_spinlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -DALPHATEST -o mesh_matbufref_pcfpcss_lockoit_spinlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -DREVERSEDZ -o mesh_matbufref_pcfpcss_lockoit_spinlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -o mesh_matbufref_pcfpcss_lockoit_spinlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -o mesh_matbufref_pcfpcss_lockoit_interlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -DALPHATEST -o mesh_matbufref_pcfpcss_lockoit_interlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -DREVERSEDZ -o mesh_matbufref_pcfpcss_lockoit_interlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -o mesh_matbufref_pcfpcss_lockoit_interlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -DMSAA -o mesh_matbufref_pcfpcss_lockoit_spinlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -DALPHATEST -DMSAA -o mesh_matbufref_pcfpcss_lockoit_spinlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -DREVERSEDZ -DMSAA -o mesh_matbufref_pcfpcss_lockoit_spinlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matbufref_pcfpcss_lockoit_spinlock_reversedz_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -DMSAA -o mesh_matbufref_pcfpcss_lockoit_interlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -DALPHATEST -DMSAA -o mesh_matbufref_pcfpcss_lockoit_interlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -DREVERSEDZ -DMSAA -o mesh_matbufref_pcfpcss_lockoit_interlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matbufref_pcfpcss_lockoit_interlock_reversedz_masked_msaa_frag.spv'

  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -o mesh_matbufref_pcfpcss_dfaoit_spinlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -DALPHATEST -o mesh_matbufref_pcfpcss_dfaoit_spinlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -DREVERSEDZ -o mesh_matbufref_pcfpcss_dfaoit_spinlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -o mesh_matbufref_pcfpcss_dfaoit_spinlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -o mesh_matbufref_pcfpcss_dfaoit_interlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -DALPHATEST -o mesh_matbufref_pcfpcss_dfaoit_interlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -DREVERSEDZ -o mesh_matbufref_pcfpcss_dfaoit_interlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -o mesh_matbufref_pcfpcss_dfaoit_interlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -DMSAA -o mesh_matbufref_pcfpcss_dfaoit_spinlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -DALPHATEST -DMSAA -o mesh_matbufref_pcfpcss_dfaoit_spinlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -DREVERSEDZ -DMSAA -o mesh_matbufref_pcfpcss_dfaoit_spinlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matbufref_pcfpcss_dfaoit_spinlock_reversedz_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -DMSAA -o mesh_matbufref_pcfpcss_dfaoit_interlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -DALPHATEST -DMSAA -o mesh_matbufref_pcfpcss_dfaoit_interlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -DREVERSEDZ -DMSAA -o mesh_matbufref_pcfpcss_dfaoit_interlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matbufref_pcfpcss_dfaoit_interlock_reversedz_masked_msaa_frag.spv'

  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DDEPTHONLY -o mesh_matbufref_depth_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DDEPTHONLY -DALPHATEST -o mesh_matbufref_depth_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DDEPTHONLY -DALPHATEST -DUSEDEMOTE -o mesh_matbufref_depth_masked_demote_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DDEPTHONLY -DALPHATEST -DNODISCARD -o mesh_matbufref_depth_masked_nodiscard_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DDEPTHONLY -DALPHATEST -DNODISCARD -DREVERSEDZ -o mesh_matbufref_depth_masked_nodiscard_reversedz_frag.spv'  

  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DDEPTHONLY -DVELOCITY -o mesh_matbufref_depth_velocity_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DDEPTHONLY -DVELOCITY -DALPHATEST -o mesh_matbufref_depth_velocity_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DDEPTHONLY -DVELOCITY -DALPHATEST -DUSEDEMOTE -o mesh_matbufref_depth_velocity_masked_demote_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DDEPTHONLY -DVELOCITY -DALPHATEST -DNODISCARD -o mesh_matbufref_depth_velocity_masked_nodiscard_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_BUFFER_REFERENCE -DDEPTHONLY -DVELOCITY -DALPHATEST -DNODISCARD -DREVERSEDZ -o mesh_matbufref_depth_velocity_masked_nodiscard_reversedz_frag.spv'  
  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -o mesh_matssbo_msm_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -o mesh_matssbo_msm_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -DMSAA -o mesh_matssbo_msm_masked_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -DUSEDEMOTE -o mesh_matssbo_msm_masked_demote_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -DUSEDEMOTE -DMSAA -o mesh_matssbo_msm_masked_demote_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -DNODISCARD -o mesh_matssbo_msm_masked_nodiscard_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -DNODISCARD -DMSAA -o mesh_matssbo_msm_masked_nodiscard_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -DNODISCARD -DREVERSEDZ -o mesh_matssbo_msm_masked_nodiscard_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DALPHATEST -DNODISCARD -DREVERSEDZ -DMSAA -o mesh_matssbo_msm_masked_nodiscard_reversedz_msaa_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DBLEND -o mesh_matssbo_msm_blend_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -o mesh_matssbo_msm_blend_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -DMSAA -o mesh_matssbo_msm_blend_masked_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -DUSEDEMOTE -o mesh_matssbo_msm_blend_masked_demote_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -DUSEDEMOTE -DMSAA -o mesh_matssbo_msm_blend_masked_demote_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -DNODISCARD -o mesh_matssbo_msm_blend_masked_nodiscard_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -DNODISCARD -DMSAA -o mesh_matssbo_msm_blend_masked_nodiscard_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -DNODISCARD -DREVERSEDZ -o mesh_matssbo_msm_blend_masked_nodiscard_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DBLEND -DALPHATEST -DNODISCARD -DREVERSEDZ -DMSAA -o mesh_matssbo_msm_blend_masked_nodiscard_reversedz_msaa_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DWBOIT -o mesh_matssbo_msm_wboit_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DWBOIT -DALPHATEST -o mesh_matssbo_msm_wboit_masked_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DMBOIT -DMBOITPASS1 -DDEPTHONLY -o mesh_matssbo_msm_mboit_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DMBOIT -DALPHATEST -DMBOITPASS1 -DDEPTHONLY -o mesh_matssbo_msm_mboit_masked_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DMBOIT -DMBOITPASS2 -o mesh_matssbo_msm_mboit_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DMBOIT -DALPHATEST -DMBOITPASS2 -o mesh_matssbo_msm_mboit_masked_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DMBOIT -DMBOITPASS2 -DMSAA -o mesh_matssbo_msm_mboit_msaa_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DMBOIT -DALPHATEST -DMBOITPASS2 -DMSAA -o mesh_matssbo_msm_mboit_masked_msaa_pass2_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DDEPTHONLY -o mesh_matssbo_msm_loopoit_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DALPHATEST -DDEPTHONLY -o mesh_matssbo_msm_loopoit_masked_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DREVERSEDZ -DDEPTHONLY -o mesh_matssbo_msm_loopoit_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DREVERSEDZ -DALPHATEST -DDEPTHONLY -o mesh_matssbo_msm_loopoit_masked_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DDEPTHONLY -o mesh_matssbo_msm_loopoit_msaa_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DALPHATEST -DDEPTHONLY -o mesh_matssbo_msm_loopoit_msaa_masked_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DREVERSEDZ -DDEPTHONLY -o mesh_matssbo_msm_loopoit_msaa_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DREVERSEDZ -DALPHATEST -DDEPTHONLY -o mesh_matssbo_msm_loopoit_msaa_masked_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -o mesh_matssbo_msm_loopoit_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -DALPHATEST -o mesh_matssbo_msm_loopoit_masked_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -DREVERSEDZ -o mesh_matssbo_msm_loopoit_reversedz_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -DREVERSEDZ -DALPHATEST -o mesh_matssbo_msm_loopoit_masked_reversedz_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -o mesh_matssbo_msm_loopoit_msaa_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -DALPHATEST -o mesh_matssbo_msm_loopoit_msaa_masked_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -DREVERSEDZ -o mesh_matssbo_msm_loopoit_msaa_reversedz_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -DREVERSEDZ -DALPHATEST -o mesh_matssbo_msm_loopoit_msaa_masked_reversedz_pass2_frag.spv'

  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -o mesh_matssbo_msm_lockoit_spinlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -DALPHATEST -o mesh_matssbo_msm_lockoit_spinlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -DREVERSEDZ -o mesh_matssbo_msm_lockoit_spinlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -o mesh_matssbo_msm_lockoit_spinlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -o mesh_matssbo_msm_lockoit_interlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -DALPHATEST -o mesh_matssbo_msm_lockoit_interlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -DREVERSEDZ -o mesh_matssbo_msm_lockoit_interlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -o mesh_matssbo_msm_lockoit_interlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -DMSAA -o mesh_matssbo_msm_lockoit_spinlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -DALPHATEST -DMSAA -o mesh_matssbo_msm_lockoit_spinlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -DREVERSEDZ -DMSAA -o mesh_matssbo_msm_lockoit_spinlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matssbo_msm_lockoit_spinlock_reversedz_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -DMSAA -o mesh_matssbo_msm_lockoit_interlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -DALPHATEST -DMSAA -o mesh_matssbo_msm_lockoit_interlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -DREVERSEDZ -DMSAA -o mesh_matssbo_msm_lockoit_interlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DLOCKOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matssbo_msm_lockoit_interlock_reversedz_masked_msaa_frag.spv'

  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -o mesh_matssbo_msm_dfaoit_spinlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -DALPHATEST -o mesh_matssbo_msm_dfaoit_spinlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -DREVERSEDZ -o mesh_matssbo_msm_dfaoit_spinlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -o mesh_matssbo_msm_dfaoit_spinlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -o mesh_matssbo_msm_dfaoit_interlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -DALPHATEST -o mesh_matssbo_msm_dfaoit_interlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -DREVERSEDZ -o mesh_matssbo_msm_dfaoit_interlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -o mesh_matssbo_msm_dfaoit_interlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -DMSAA -o mesh_matssbo_msm_dfaoit_spinlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -DALPHATEST -DMSAA -o mesh_matssbo_msm_dfaoit_spinlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -DREVERSEDZ -DMSAA -o mesh_matssbo_msm_dfaoit_spinlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matssbo_msm_dfaoit_spinlock_reversedz_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -DMSAA -o mesh_matssbo_msm_dfaoit_interlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -DALPHATEST -DMSAA -o mesh_matssbo_msm_dfaoit_interlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -DREVERSEDZ -DMSAA -o mesh_matssbo_msm_dfaoit_interlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DMSM -DDFAOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matssbo_msm_dfaoit_interlock_reversedz_masked_msaa_frag.spv'

  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -o mesh_matssbo_pcfpcss_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -o mesh_matssbo_pcfpcss_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -DMSAA -o mesh_matssbo_pcfpcss_masked_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -DUSEDEMOTE -o mesh_matssbo_pcfpcss_masked_demote_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -DUSEDEMOTE -DMSAA -o mesh_matssbo_pcfpcss_masked_demote_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -DNODISCARD -o mesh_matssbo_pcfpcss_masked_nodiscard_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -DNODISCARD -DMSAA -o mesh_matssbo_pcfpcss_masked_nodiscard_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -DNODISCARD -DREVERSEDZ -o mesh_matssbo_pcfpcss_masked_nodiscard_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DALPHATEST -DNODISCARD -DREVERSEDZ -DMSAA -o mesh_matssbo_pcfpcss_masked_nodiscard_reversedz_msaa_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -o mesh_matssbo_pcfpcss_blend_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -o mesh_matssbo_pcfpcss_blend_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -DMSAA -o mesh_matssbo_pcfpcss_blend_masked_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -DUSEDEMOTE -o mesh_matssbo_pcfpcss_blend_masked_demote_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -DUSEDEMOTE -DMSAA -o mesh_matssbo_pcfpcss_blend_masked_demote_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -DNODISCARD -o mesh_matssbo_pcfpcss_blend_masked_nodiscard_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -DNODISCARD -DMSAA -o mesh_matssbo_pcfpcss_blend_masked_nodiscard_msaa_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -DNODISCARD -DREVERSEDZ -o mesh_matssbo_pcfpcss_blend_masked_nodiscard_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DBLEND -DALPHATEST -DNODISCARD -DREVERSEDZ -DMSAA -o mesh_matssbo_pcfpcss_blend_masked_nodiscard_reversedz_msaa_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DWBOIT -o mesh_matssbo_pcfpcss_wboit_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DWBOIT -DALPHATEST -o mesh_matssbo_pcfpcss_wboit_masked_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DMBOIT -DMBOITPASS1 -DDEPTHONLY -o mesh_matssbo_pcfpcss_mboit_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DMBOIT -DALPHATEST -DMBOITPASS1 -DDEPTHONLY -o mesh_matssbo_pcfpcss_mboit_masked_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DMBOIT -DMBOITPASS2 -o mesh_matssbo_pcfpcss_mboit_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DMBOIT -DALPHATEST -DMBOITPASS2 -o mesh_matssbo_pcfpcss_mboit_masked_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DMBOIT -DMBOITPASS2 -DMSAA -o mesh_matssbo_pcfpcss_mboit_msaa_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DMBOIT -DALPHATEST -DMBOITPASS2 -DMSAA -o mesh_matssbo_pcfpcss_mboit_masked_msaa_pass2_frag.spv'
  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DDEPTHONLY -o mesh_matssbo_pcfpcss_loopoit_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DALPHATEST -DDEPTHONLY -o mesh_matssbo_pcfpcss_loopoit_masked_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DREVERSEDZ -DDEPTHONLY -o mesh_matssbo_pcfpcss_loopoit_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DREVERSEDZ -DALPHATEST -DDEPTHONLY -o mesh_matssbo_pcfpcss_loopoit_masked_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DDEPTHONLY -o mesh_matssbo_pcfpcss_loopoit_msaa_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DALPHATEST -DDEPTHONLY -o mesh_matssbo_pcfpcss_loopoit_msaa_masked_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DREVERSEDZ -DDEPTHONLY -o mesh_matssbo_pcfpcss_loopoit_msaa_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS1 -DMSAA -DREVERSEDZ -DALPHATEST -DDEPTHONLY -o mesh_matssbo_pcfpcss_loopoit_msaa_masked_reversedz_pass1_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -o mesh_matssbo_pcfpcss_loopoit_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -DALPHATEST -o mesh_matssbo_pcfpcss_loopoit_masked_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -DREVERSEDZ -o mesh_matssbo_pcfpcss_loopoit_reversedz_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -DREVERSEDZ -DALPHATEST -o mesh_matssbo_pcfpcss_loopoit_masked_reversedz_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -o mesh_matssbo_pcfpcss_loopoit_msaa_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -DALPHATEST -o mesh_matssbo_pcfpcss_loopoit_msaa_masked_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -DREVERSEDZ -o mesh_matssbo_pcfpcss_loopoit_msaa_reversedz_pass2_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOOPOIT -DLOOPOIT_PASS2 -DMSAA -DREVERSEDZ -DALPHATEST -o mesh_matssbo_pcfpcss_loopoit_msaa_masked_reversedz_pass2_frag.spv'

  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -o mesh_matssbo_pcfpcss_lockoit_spinlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -DALPHATEST -o mesh_matssbo_pcfpcss_lockoit_spinlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -DREVERSEDZ -o mesh_matssbo_pcfpcss_lockoit_spinlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -o mesh_matssbo_pcfpcss_lockoit_spinlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -o mesh_matssbo_pcfpcss_lockoit_interlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -DALPHATEST -o mesh_matssbo_pcfpcss_lockoit_interlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -DREVERSEDZ -o mesh_matssbo_pcfpcss_lockoit_interlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -o mesh_matssbo_pcfpcss_lockoit_interlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -DMSAA -o mesh_matssbo_pcfpcss_lockoit_spinlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -DALPHATEST -DMSAA -o mesh_matssbo_pcfpcss_lockoit_spinlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -DREVERSEDZ -DMSAA -o mesh_matssbo_pcfpcss_lockoit_spinlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matssbo_pcfpcss_lockoit_spinlock_reversedz_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -DMSAA -o mesh_matssbo_pcfpcss_lockoit_interlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -DALPHATEST -DMSAA -o mesh_matssbo_pcfpcss_lockoit_interlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -DREVERSEDZ -DMSAA -o mesh_matssbo_pcfpcss_lockoit_interlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DLOCKOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matssbo_pcfpcss_lockoit_interlock_reversedz_masked_msaa_frag.spv'

  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -o mesh_matssbo_pcfpcss_dfaoit_spinlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -DALPHATEST -o mesh_matssbo_pcfpcss_dfaoit_spinlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -DREVERSEDZ -o mesh_matssbo_pcfpcss_dfaoit_spinlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -o mesh_matssbo_pcfpcss_dfaoit_spinlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -o mesh_matssbo_pcfpcss_dfaoit_interlock_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -DALPHATEST -o mesh_matssbo_pcfpcss_dfaoit_interlock_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -DREVERSEDZ -o mesh_matssbo_pcfpcss_dfaoit_interlock_reversedz_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -o mesh_matssbo_pcfpcss_dfaoit_interlock_reversedz_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -DMSAA -o mesh_matssbo_pcfpcss_dfaoit_spinlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -DALPHATEST -DMSAA -o mesh_matssbo_pcfpcss_dfaoit_spinlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -DREVERSEDZ -DMSAA -o mesh_matssbo_pcfpcss_dfaoit_spinlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DSPINLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matssbo_pcfpcss_dfaoit_spinlock_reversedz_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -DMSAA -o mesh_matssbo_pcfpcss_dfaoit_interlock_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -DALPHATEST -DMSAA -o mesh_matssbo_pcfpcss_dfaoit_interlock_masked_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -DREVERSEDZ -DMSAA -o mesh_matssbo_pcfpcss_dfaoit_interlock_reversedz_msaa_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DLIGHTS -DSHADOWS -DPCFPCSS -DDFAOIT -DINTERLOCK -DREVERSEDZ -DALPHATEST -DMSAA -o mesh_matssbo_pcfpcss_dfaoit_interlock_reversedz_masked_msaa_frag.spv'

  '-V mesh.frag -DUSE_MATERIAL_SSBO -DDEPTHONLY -o mesh_matssbo_depth_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DDEPTHONLY -DALPHATEST -o mesh_matssbo_depth_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DDEPTHONLY -DALPHATEST -DUSEDEMOTE -o mesh_matssbo_depth_masked_demote_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DDEPTHONLY -DALPHATEST -DNODISCARD -o mesh_matssbo_depth_masked_nodiscard_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DDEPTHONLY -DALPHATEST -DNODISCARD -DREVERSEDZ -o mesh_matssbo_depth_masked_nodiscard_reversedz_frag.spv'  

  '-V mesh.frag -DUSE_MATERIAL_SSBO -DDEPTHONLY -DVELOCITY -o mesh_matssbo_depth_velocity_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DDEPTHONLY -DVELOCITY -DALPHATEST -o mesh_matssbo_depth_velocity_masked_frag.spv'
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DDEPTHONLY -DVELOCITY -DALPHATEST -DUSEDEMOTE -o mesh_matssbo_depth_velocity_masked_demote_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DDEPTHONLY -DVELOCITY -DALPHATEST -DNODISCARD -o mesh_matssbo_depth_velocity_masked_nodiscard_frag.spv'  
  '-V mesh.frag -DUSE_MATERIAL_SSBO -DDEPTHONLY -DVELOCITY -DALPHATEST -DNODISCARD -DREVERSEDZ -o mesh_matssbo_depth_velocity_masked_nodiscard_reversedz_frag.spv'  

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

  '-V debug_primitive.vert -o debug_primitive_vert.spv'
  '-V debug_primitive.frag -o debug_primitive_frag.spv'

)

glslangValidatorPath=$(which glslangValidator)
spirvOptPath=$(which spirv-opt)

echo "Compiling . . ."

for index in ${!compileshaderarguments[@]}; do
   (
     ${glslangValidatorPath} ${compileshaderarguments[$index]} 
     #--target-env spirv1.5 
     #>/dev/null
   ) & 
done

wait 

#echo "Optimizing . . ."

#for f in *.spv; do
#  echo $f
# ${spirvOptPath} --strip-debug --unify-const --flatten-decorations --eliminate-dead-const $f -o $f
# ${spirvOptPath} --strip-debug --unify-const --flatten-decorations --eliminate-dead-const --strength-reduction --simplify-instructions --remove-duplicates -O $f -o $f
#done

echo "Packing . . ."

#cp -f *.spv ../../../assets/shaders/

rm -f scene3dshaders.zip
zip -m9 scene3dshaders.zip *.spv
rm -f *.spv

echo "Done!"
