// fvd_eval.h — quality measurement for fvdplay's --eval / --eval-with-reference modes.
//
// Compares the decoded RGBA8 frames fvdplay produces against a reference video,
// frame by frame, with no AVI detour. The reference is decoded on the fly through
// ffmpeg (used as a command-line tool via popen — never linked).
//
// PSNR, MSE and SSIM (per Y/U/V plane, BT.709) are always computed. VMAF is computed only when this TU is
// built with -DFVD_VMAF, in which case libvmaf is loaded dynamically at runtime via
// dlopen (so fvdplay never statically links libvmaf, and still builds and runs —
// PSNR only — on systems without the libvmaf headers or shared library).

#ifndef FVD_EVAL_H
#define FVD_EVAL_H

#include <stdint.h>

// one metric value, held per luma/chroma plane (BT.709 Y, U, V)
typedef struct FVDEvalValue {
  double y, u, v;
} FVDEvalValue;

typedef struct FVDEval FVDEval;

// Open an evaluation run.
//   reference_path : a video file ffmpeg can read (the external --eval-with-reference
//                    file, or the temp .h264 extracted from the container for --eval).
//   width, height  : the comparison canvas; both distorted and reference frames are
//                    scaled to this (the codec's native resolution).
//   dist_label     : short label for the distorted stream (e.g. "fvd (dct/wavelet)").
//   ref_label      : short label for the reference (printed in the summary).
// Returns NULL on failure (e.g. ffmpeg cannot open the reference); a message is
// printed to stderr in that case.
FVDEval *fvd_eval_open(const char *reference_path, int width, int height,
                       const char *dist_label, const char *ref_label);

// Push one decoded distorted frame (width*height RGBA8, R,G,B,A bytes — the same
// buffer fvdplay hands to avi_video_frame). Reads the matching reference frame and
// accumulates the metrics. Frames past the end of the reference are ignored.
void fvd_eval_push_rgba(FVDEval *eval, const uint8_t *rgba);

// Finish: print the pooled metrics and free everything. Safe on NULL.
void fvd_eval_finish(FVDEval *eval);

#endif // FVD_EVAL_H
