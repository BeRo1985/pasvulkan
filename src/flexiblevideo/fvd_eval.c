// fvd_eval.c — see fvd_eval.h.
//
// PSNR (luma) is always built. VMAF is built only with -DFVD_VMAF, and even then
// libvmaf is resolved at runtime via dlopen("libvmaf.so.3"); if the library is
// absent the run still reports PSNR.

#include "fvd_eval.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#ifdef FVD_VMAF
#include <dlfcn.h>
#include <libvmaf/libvmaf.h>
#endif

// ---- BT.709 full-range RGB -> luma / chroma (both distorted and reference use the
//      same conversion, so the comparison is consistent regardless of the choice). ----
static inline int rgb_to_y(int r, int g, int b) {
  int y = (int)lrintf(0.2126f * r + 0.7152f * g + 0.0722f * b);
  return y < 0 ? 0 : (y > 255 ? 255 : y);
}

#ifdef FVD_VMAF
static inline void rgb_to_yuv(int r, int g, int b, int *yy, int *uu, int *vv) {
  float y = 0.2126f * r + 0.7152f * g + 0.0722f * b;
  int u = (int)lrintf((b - y) / 1.8556f + 128.0f);
  int v = (int)lrintf((r - y) / 1.5748f + 128.0f);
  int yi = (int)lrintf(y);
  *yy = yi < 0 ? 0 : (yi > 255 ? 255 : yi);
  *uu = u < 0 ? 0 : (u > 255 ? 255 : u);
  *vv = v < 0 ? 0 : (v > 255 ? 255 : v);
}
#endif

#ifdef FVD_VMAF
// libvmaf entry points resolved via dlsym (no static link).
typedef int  (*pfn_vmaf_init)(VmafContext **, VmafConfiguration);
typedef int  (*pfn_vmaf_model_load)(VmafModel **, VmafModelConfig *, const char *);
typedef int  (*pfn_vmaf_use_features_from_model)(VmafContext *, VmafModel *);
typedef int  (*pfn_vmaf_picture_alloc)(VmafPicture *, enum VmafPixelFormat, unsigned, unsigned, unsigned);
typedef int  (*pfn_vmaf_picture_unref)(VmafPicture *);
typedef int  (*pfn_vmaf_read_pictures)(VmafContext *, VmafPicture *, VmafPicture *, unsigned);
typedef int  (*pfn_vmaf_score_pooled)(VmafContext *, VmafModel *, enum VmafPoolingMethod, double *, unsigned, unsigned);
typedef int  (*pfn_vmaf_close)(VmafContext *);
typedef void (*pfn_vmaf_model_destroy)(VmafModel *);

typedef struct {
  void *lib;
  pfn_vmaf_init init;
  pfn_vmaf_model_load model_load;
  pfn_vmaf_use_features_from_model use_features_from_model;
  pfn_vmaf_picture_alloc picture_alloc;
  pfn_vmaf_picture_unref picture_unref;
  pfn_vmaf_read_pictures read_pictures;
  pfn_vmaf_score_pooled score_pooled;
  pfn_vmaf_close close;
  pfn_vmaf_model_destroy model_destroy;
  VmafContext *ctx;
  VmafModel *model;
  int active;   // 1 once init + model + features all succeeded
} VmafState;
#endif

struct FvdEval {
  FILE *pipe;          // ffmpeg rgb24 reference stream
  int width, height;
  uint8_t *ref_rgb;    // one reference frame, width*height*3
  int ref_eof;
  char dist_label[64];
  char ref_label[160];
  uint8_t *dist_y, *ref_y;   // per-frame luma planes (width*height), for MSE / PSNR / SSIM
  // per-frame metrics, averaged over the compared frames
  double mse_sum;
  double psnr_sum;
  double psnr_min;
  double ssim_sum;
  double ssim_min;
  unsigned count;      // frames actually compared
#ifdef FVD_VMAF
  VmafState vmaf;
#endif
};

// ---------------------------------------------------------------------- VMAF setup
#ifdef FVD_VMAF
static void vmaf_setup(FvdEval *e) {
  VmafState *v = &e->vmaf;
  memset(v, 0, sizeof *v);
  v->lib = dlopen("libvmaf.so.3", RTLD_NOW | RTLD_LOCAL);
  if (!v->lib) v->lib = dlopen("libvmaf.so", RTLD_NOW | RTLD_LOCAL);
  if (!v->lib) {
    fprintf(stderr, "  (VMAF: libvmaf.so not found at runtime -> PSNR only)\n");
    return;
  }
  v->init                    = (pfn_vmaf_init)dlsym(v->lib, "vmaf_init");
  v->model_load              = (pfn_vmaf_model_load)dlsym(v->lib, "vmaf_model_load");
  v->use_features_from_model = (pfn_vmaf_use_features_from_model)dlsym(v->lib, "vmaf_use_features_from_model");
  v->picture_alloc           = (pfn_vmaf_picture_alloc)dlsym(v->lib, "vmaf_picture_alloc");
  v->picture_unref           = (pfn_vmaf_picture_unref)dlsym(v->lib, "vmaf_picture_unref");
  v->read_pictures           = (pfn_vmaf_read_pictures)dlsym(v->lib, "vmaf_read_pictures");
  v->score_pooled            = (pfn_vmaf_score_pooled)dlsym(v->lib, "vmaf_score_pooled");
  v->close                   = (pfn_vmaf_close)dlsym(v->lib, "vmaf_close");
  v->model_destroy           = (pfn_vmaf_model_destroy)dlsym(v->lib, "vmaf_model_destroy");
  if (!v->init || !v->model_load || !v->use_features_from_model || !v->picture_alloc ||
      !v->picture_unref || !v->read_pictures || !v->score_pooled || !v->close || !v->model_destroy) {
    fprintf(stderr, "  (VMAF: libvmaf symbols missing -> PSNR only)\n");
    return;
  }
  VmafConfiguration cfg; memset(&cfg, 0, sizeof cfg);
  cfg.log_level = VMAF_LOG_LEVEL_NONE;
  cfg.n_threads = 4;
  if (v->init(&v->ctx, cfg) != 0) { fprintf(stderr, "  (VMAF: vmaf_init failed -> PSNR only)\n"); return; }
  VmafModelConfig mcfg; memset(&mcfg, 0, sizeof mcfg);
  mcfg.name = "vmaf";
  if (v->model_load(&v->model, &mcfg, "vmaf_v0.6.1") != 0) {
    fprintf(stderr, "  (VMAF: model vmaf_v0.6.1 load failed -> PSNR only)\n");
    v->close(v->ctx); v->ctx = NULL; return;
  }
  if (v->use_features_from_model(v->ctx, v->model) != 0) {
    fprintf(stderr, "  (VMAF: use_features_from_model failed -> PSNR only)\n");
    v->model_destroy(v->model); v->model = NULL; v->close(v->ctx); v->ctx = NULL; return;
  }
  v->active = 1;
}

// fill one allocated YUV444P picture from a packed buffer (stride = bytes/pixel: 4 = RGBA, 3 = RGB)
static int vmaf_fill(FvdEval *e, VmafPicture *pic, const uint8_t *src, int stride) {
  if (e->vmaf.picture_alloc(pic, VMAF_PIX_FMT_YUV444P, 8, e->width, e->height) != 0) return -1;
  for (int y = 0; y < e->height; y++) {
    uint8_t *yr = (uint8_t *)pic->data[0] + (size_t)y * pic->stride[0];
    uint8_t *ur = (uint8_t *)pic->data[1] + (size_t)y * pic->stride[1];
    uint8_t *vr = (uint8_t *)pic->data[2] + (size_t)y * pic->stride[2];
    const uint8_t *p = src + (size_t)y * e->width * stride;
    for (int x = 0; x < e->width; x++, p += stride) {
      int yy, uu, vv; rgb_to_yuv(p[0], p[1], p[2], &yy, &uu, &vv);
      yr[x] = (uint8_t)yy; ur[x] = (uint8_t)uu; vr[x] = (uint8_t)vv;
    }
  }
  return 0;
}

static void vmaf_push(FvdEval *e, const uint8_t *dist_rgba) {
  VmafState *v = &e->vmaf;
  if (!v->active) return;
  VmafPicture ref_pic, dist_pic;
  if (vmaf_fill(e, &ref_pic, e->ref_rgb, 3) != 0) return;
  if (vmaf_fill(e, &dist_pic, dist_rgba, 4) != 0) { v->picture_unref(&ref_pic); return; }
  // index = count (already incremented for this frame by the caller, so use count-1)
  if (v->read_pictures(v->ctx, &ref_pic, &dist_pic, e->count - 1) != 0) {
    v->picture_unref(&ref_pic); v->picture_unref(&dist_pic);
    fprintf(stderr, "  (VMAF: read_pictures failed -> VMAF disabled)\n");
    v->active = 0;
  }
  // on success libvmaf takes ownership of both pictures and unrefs them.
}
#endif // FVD_VMAF

// ---- mean SSIM over non-overlapping 8x8 luma blocks (Wang et al. 2004; the standard
//      8-bit constants C1=(0.01*255)^2, C2=(0.03*255)^2). Edge pixels past the last full
//      8x8 block are ignored (negligible at typical resolutions). ----
static double ssim_plane(const uint8_t *a, const uint8_t *b, int w, int h) {
  const double C1 = 6.5025, C2 = 58.5225;
  double acc = 0.0;
  long blocks = 0;
  for (int by = 0; by + 8 <= h; by += 8) {
    for (int bx = 0; bx + 8 <= w; bx += 8) {
      double sa = 0, sb = 0, saa = 0, sbb = 0, sab = 0;
      for (int y = 0; y < 8; y++) {
        const uint8_t *ra = a + (size_t)(by + y) * w + bx;
        const uint8_t *rb = b + (size_t)(by + y) * w + bx;
        for (int x = 0; x < 8; x++) {
          double va = ra[x], vb = rb[x];
          sa += va; sb += vb; saa += va * va; sbb += vb * vb; sab += va * vb;
        }
      }
      double n = 64.0;
      double mua = sa / n, mub = sb / n;
      double va = saa / n - mua * mua;
      double vb = sbb / n - mub * mub;
      double cab = sab / n - mua * mub;
      acc += ((2 * mua * mub + C1) * (2 * cab + C2)) /
             ((mua * mua + mub * mub + C1) * (va + vb + C2));
      blocks++;
    }
  }
  return blocks ? acc / (double)blocks : 1.0;
}

// ---------------------------------------------------------------------- public API
FvdEval *fvd_eval_open(const char *reference_path, int width, int height,
                       const char *dist_label, const char *ref_label) {
  char cmd[2048];
  // ffmpeg as a CLI tool only (never linked): decode + scale the reference to rgb24.
  snprintf(cmd, sizeof cmd,
           "ffmpeg -nostdin -v error -i \"%s\" -vf scale=%d:%d:flags=bicubic -f rawvideo -pix_fmt rgb24 -",
           reference_path, width, height);
  FILE *pipe = popen(cmd, "r");
  if (!pipe) { fprintf(stderr, "fvd_eval: cannot start ffmpeg for reference '%s'\n", reference_path); return NULL; }

  FvdEval *e = (FvdEval *)calloc(1, sizeof *e);
  e->pipe = pipe;
  e->width = width;
  e->height = height;
  e->ref_rgb = (uint8_t *)malloc((size_t)width * height * 3);
  e->dist_y = (uint8_t *)malloc((size_t)width * height);
  e->ref_y = (uint8_t *)malloc((size_t)width * height);
  e->psnr_min = 1e30;
  e->ssim_min = 1e30;
  snprintf(e->dist_label, sizeof e->dist_label, "%s", dist_label ? dist_label : "stream");
  snprintf(e->ref_label, sizeof e->ref_label, "%s", ref_label ? ref_label : reference_path);
#ifdef FVD_VMAF
  vmaf_setup(e);
#endif
  return e;
}

void fvd_eval_push_rgba(FvdEval *e, const uint8_t *rgba) {
  if (!e || e->ref_eof) return;
  size_t need = (size_t)e->width * e->height * 3;
  if (fread(e->ref_rgb, 1, need, e->pipe) != need) { e->ref_eof = 1; return; }

  // build the luma planes once, then derive MSE / PSNR / SSIM from them
  size_t n = (size_t)e->width * e->height;
  const uint8_t *d = rgba, *r = e->ref_rgb;
  for (size_t i = 0; i < n; i++, d += 4, r += 3) {
    e->dist_y[i] = (uint8_t)rgb_to_y(d[0], d[1], d[2]);
    e->ref_y[i]  = (uint8_t)rgb_to_y(r[0], r[1], r[2]);
  }
  double mse = 0.0;
  for (size_t i = 0; i < n; i++) {
    int e0 = (int)e->dist_y[i] - (int)e->ref_y[i];
    mse += (double)e0 * e0;
  }
  mse /= (double)n;
  double psnr = (mse <= 0.0) ? 99.0 : 10.0 * log10((255.0 * 255.0) / mse);
  if (psnr > 99.0) psnr = 99.0;
  double ssim = ssim_plane(e->dist_y, e->ref_y, e->width, e->height);
  e->mse_sum += mse;
  e->psnr_sum += psnr;
  if (psnr < e->psnr_min) e->psnr_min = psnr;
  e->ssim_sum += ssim;
  if (ssim < e->ssim_min) e->ssim_min = ssim;
  e->count++;

#ifdef FVD_VMAF
  vmaf_push(e, rgba);
#endif
}

void fvd_eval_finish(FvdEval *e) {
  if (!e) return;
#ifdef FVD_VMAF
  double vmaf_score = -1.0;
  if (e->vmaf.active && e->count > 0) {
    e->vmaf.read_pictures(e->vmaf.ctx, NULL, NULL, 0);  // flush
    if (e->vmaf.score_pooled(e->vmaf.ctx, e->vmaf.model, VMAF_POOL_METHOD_MEAN,
                             &vmaf_score, 0, e->count - 1) != 0) {
      vmaf_score = -1.0;
    }
  }
#endif

  printf("\n");
  printf("eval: %s  vs  %s\n", e->dist_label, e->ref_label);
  if (e->count == 0) {
    printf("  no frames compared (reference shorter than the stream, or read error)\n");
  } else {
    printf("  frames compared : %u\n", e->count);
    printf("  MSE-Y  (mean)   : %8.4f\n", e->mse_sum / e->count);
    printf("  PSNR-Y (mean)   : %6.3f dB\n", e->psnr_sum / e->count);
    printf("  PSNR-Y (min)    : %6.3f dB\n", e->psnr_min);
    printf("  SSIM-Y (mean)   : %7.5f\n", e->ssim_sum / e->count);
    printf("  SSIM-Y (min)    : %7.5f\n", e->ssim_min);
#ifdef FVD_VMAF
    if (vmaf_score >= 0.0)
      printf("  VMAF (mean)     : %6.3f\n", vmaf_score);
    else
      printf("  VMAF            : n/a\n");
#else
    printf("  VMAF            : not built (compile fvdplay with -DFVD_VMAF for VMAF)\n");
#endif
  }
  printf("\n");

#ifdef FVD_VMAF
  if (e->vmaf.lib) {
    if (e->vmaf.model && e->vmaf.model_destroy) e->vmaf.model_destroy(e->vmaf.model);
    if (e->vmaf.ctx && e->vmaf.close) e->vmaf.close(e->vmaf.ctx);
    dlclose(e->vmaf.lib);
  }
#endif
  if (e->pipe) pclose(e->pipe);
  free(e->ref_rgb);
  free(e->dist_y);
  free(e->ref_y);
  free(e);
}
