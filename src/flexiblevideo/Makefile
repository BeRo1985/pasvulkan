# Flexible Video (FVD) — GPU DCT video codec (DCT-based fork of the FWV wavelet codec)
CC     ?= cc
# -ftrivial-auto-var-init=zero: zero-init all auto/stack locals. Standard hardening (Linux kernel / Chrome / Android)
# that makes an otherwise build-layout-dependent uninitialized-stack-local read deterministic + correct here (it was
# intermittently corrupting the encode output in ~a few %% of builds; invisible to ASan/UBSan/Valgrind/gdb). ~free.
CFLAGS ?= -O2 -Wall -Wextra -Wno-unused-function -std=c11 -ftrivial-auto-var-init=zero
GLSLC  ?= glslc

# VMAF in fvdplay's --eval / --eval-with-reference: build with 'make fvdplay VMAF=1'.
# libvmaf is loaded at runtime via dlopen (never statically linked) — this only adds the
# headers (for the structs) + -ldl, so fvdplay still builds and runs (PSNR only) without
# the libvmaf development package.
ifdef VMAF
EVAL_CFLAGS = -DFVD_VMAF $(shell pkg-config --cflags libvmaf 2>/dev/null)
EVAL_LIBS   = -ldl
endif

SPV = shaders/bitplane_unpack.spv shaders/dequant97.spv shaders/transpose_f.spv \
      shaders/pthresh.spv \
      shaders/idwt97row.spv shaders/round97.spv shaders/color.spv \
      shaders/rgb2yco.spv shaders/fwd97row.spv shaders/quant97fwd.spv \
      shaders/bitplane_size.spv shaders/bitplane_pack.spv \
      shaders/idwt53row.spv shaders/fwd53row.spv shaders/rgb2ycor.spv \
      shaders/coeff_diff.spv shaders/coeff_add.spv shaders/ycocg_diff.spv shaders/frame_energy.spv \
      shaders/mc.spv shaders/motion_add.spv shaders/motion_estimate.spv shaders/motion_refine.spv shaders/motion_refine_full.spv \
      shaders/apply_pcrd.spv shaders/chroma_downsample.spv \
      shaders/rgb2yco16.spv shaders/rgb2ycor16.spv shaders/color_hdr.spv shaders/color_hdr_scrgb.spv \
      shaders/tdwt_int.spv shaders/tdwt_float.spv shaders/nv12rgb.spv shaders/bidi_blend.spv shaders/motion_estimate_bidi.spv \
      shaders/mode_decide.spv shaders/blend_mode.spv \
      shaders/bitplane_pack_coop.spv shaders/bitplane_size_coop.spv shaders/motion_merge.spv \
      shaders/bidi_mode_sad.spv shaders/motion_merge_bidi.spv shaders/mode_decide_root.spv \
      shaders/int2float.spv shaders/extract_alpha.spv shaders/extract_alpha16.spv shaders/color_alpha.spv \
      shaders/color_hdr_alpha.spv shaders/color_hdr_scrgb_alpha.spv shaders/fvd_composite_present.spv \
      shaders/dering.spv shaders/apply_tile_aq.spv \
      shaders/cdef.spv shaders/cdef_sse.spv \
      shaders/dct_fwd.spv shaders/dct_inv.spv shaders/dct_inv_int.spv shaders/dct_fwd_int.spv shaders/rans_unpack.spv \
      shaders/rans_hist.spv shaders/rans_size.spv shaders/rans_pack.spv \
      shaders/rans_unpack_qt.spv shaders/dct_inv_qt.spv shaders/deblock.spv \
      shaders/dct_fwd_qt.spv shaders/rans_hist_qt.spv shaders/rans_size_qt.spv

all: fvdref fvddec fvdenc fvdplay fwa fvdtag

fvdref: fvdref.c
	$(CC) $(CFLAGS) -o $@ $< -lm

# standalone container retag tool: set the motion_mode (interp + MV precision) byte in place, no re-encode
fvdtag: fvdtag.c
	$(CC) $(CFLAGS) -o $@ $<

shaders/%.spv: shaders/%.comp
	$(GLSLC) -O $< -o $@

shaders/color_alpha.spv: shaders/color.comp
	$(GLSLC) -O -DHAS_ALPHA $< -o $@

shaders/color_hdr_alpha.spv: shaders/color_hdr.comp
	$(GLSLC) -O -DHAS_ALPHA $< -o $@

shaders/color_hdr_scrgb_alpha.spv: shaders/color_hdr_scrgb.comp
	$(GLSLC) -O -DHAS_ALPHA $< -o $@

shaders/motion_refine_full.spv: shaders/motion_refine.comp
	$(GLSLC) -O -DSPATIAL_FULL $< -o $@

fvddec: fvddec.c fvdref.c $(SPV)
	$(CC) $(CFLAGS) -Wno-missing-field-initializers -o $@ fvddec.c -lvulkan -lm

fvdenc: fvdenc.c fvdref.c fwa_audio.c fwa_audio.h $(SPV)
	$(CC) $(CFLAGS) -DFWA_NO_MAIN -Wno-missing-field-initializers -o $@ fvdenc.c fwa_audio.c -lvulkan -lm

fvdplay: fvdplay.c fvdref.c fwa_audio.c fwa_audio.h fvd_h264.c fvd_h264.h fvd_eval.c fvd_eval.h avi_writer.c avi_writer.h stb_vorbis.c $(SPV)
	$(CC) $(CFLAGS) -DFWA_NO_MAIN -Wno-missing-field-initializers $(EVAL_CFLAGS) $(shell pkg-config --cflags sdl2) -o $@ fvdplay.c fvd_h264.c avi_writer.c fwa_audio.c fvd_eval.c $(shell pkg-config --libs sdl2) -lvulkan -lm $(EVAL_LIBS)

# standalone FWA audio CLI (test/enc/dec) — built WITHOUT -DFWA_NO_MAIN so fwa_audio.c's own main() is active
fwa: fwa_audio.c fwa_audio.h
	$(CC) $(CFLAGS) -Wno-missing-field-initializers -o $@ fwa_audio.c -lm

clean:
	rm -f fvdref fvddec fvdenc fvdplay fwa shaders/*.spv

.PHONY: all clean
