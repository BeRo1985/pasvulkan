// fwvtag.c — retag an existing .fwv container's motion_mode in place, WITHOUT re-encoding.
//
// The sub-pel mode (interpolation filter + MV precision) lives in the container header's mv_codec byte:
//   bit0 = entropy coder (0 = Exp-Golomb, 1 = range)   <- left untouched
//   bit1 = interpolation (0 = bilinear, 1 = 6-tap)
//   bit2 = MV precision  (0 = half-pel, 1 = quarter-pel)
// Files written before the multi-mode encoder have these bits = 0 (bilinear + half-pel), which is exactly how
// they were encoded — so they need no patch. Files encoded by an unconditional 6-tap / quarter build (their
// header still says 0) DO need patching to declare their true mode. This tool writes that one byte.
//
// usage:
//   ./fwvtag [--interp=6tap|bilinear] [--mv-precision=quarter|half] file.fwv [more.fwv ...]
//   ./fwvtag --show file.fwv                 (print the current mode, change nothing)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <stdint.h>

// The leading part of ContainerHeader, field-for-field identical to fwvenc.c up to mv_codec, so
// offsetof(ContainerHeader, mv_codec) resolves to the same byte (padding included).
typedef struct {
  uint8_t  magic[4];
  uint16_t version;
  uint16_t header_size;
  uint32_t width, height, fps_num, fps_den, levels, quality, frame_count;
  uint8_t  bit_depth;
  uint8_t  color_primaries;
  uint8_t  transfer_function;
  uint8_t  matrix;
  uint8_t  full_range;
  uint8_t  color_flags;
  uint16_t gop;
  uint16_t mastering_primaries_x[3], mastering_primaries_y[3];
  uint16_t mastering_white_x, mastering_white_y;
  uint32_t mastering_max_luminance, mastering_min_luminance;
  uint16_t max_content_light_level;
  uint16_t max_frame_avg_light_level;
  uint64_t audio_offset, audio_size, index_offset;
  uint8_t  prediction_method;
  uint8_t  chroma_quant_x16;
  uint8_t  chroma_format;
  uint8_t  reserved2[6];
  uint8_t  audio_codec[4];
  uint8_t  mv_codec;
} ContainerHeaderPrefix;

static const char *mode_name(int mv_codec) {
  int interp = (mv_codec >> 1) & 1, precision = (mv_codec >> 2) & 1;
  if (!interp && !precision) { return "bilinear + half-pel (legacy)"; }
  if ( interp && !precision) { return "6-tap + half-pel"; }
  if (!interp &&  precision) { return "bilinear + quarter-pel"; }
  return "6-tap + quarter-pel";
}

int main(int argc, char **argv) {
  int set_interp = -1, set_precision = -1, show_only = 0;
  const char *files[256];
  int file_count = 0;
  for (int i = 1; i < argc; i++) {
    if (!strncmp(argv[i], "--interp=", 9)) {
      set_interp = strstr(argv[i] + 9, "bilinear") ? 0 : 1;
    } else if (!strncmp(argv[i], "--mv-precision=", 15)) {
      set_precision = strstr(argv[i] + 15, "half") ? 0 : 1;
    } else if (!strcmp(argv[i], "--show")) {
      show_only = 1;
    } else if (file_count < 256) {
      files[file_count++] = argv[i];
    }
  }
  if (file_count == 0) {
    fprintf(stderr, "usage: %s [--interp=6tap|bilinear] [--mv-precision=quarter|half] file.fwv [...]\n"
                    "       %s --show file.fwv\n", argv[0], argv[0]);
    return 2;
  }

  size_t offset = offsetof(ContainerHeaderPrefix, mv_codec);
  int failures = 0;
  for (int f = 0; f < file_count; f++) {
    FILE *file = fopen(files[f], "r+b");
    if (!file) {
      fprintf(stderr, "%s: cannot open\n", files[f]);
      failures++;
      continue;
    }
    uint8_t magic[4] = { 0, 0, 0, 0 };
    if ((fread(magic, 1, 4, file) != 4) || memcmp(magic, "FWVC", 4)) {
      fprintf(stderr, "%s: not an FWVC container\n", files[f]);
      fclose(file);
      failures++;
      continue;
    }
    if (fseek(file, (long)offset, SEEK_SET) != 0) {
      fprintf(stderr, "%s: seek failed\n", files[f]);
      fclose(file);
      failures++;
      continue;
    }
    int old_byte = fgetc(file);
    if (old_byte < 0) {
      fprintf(stderr, "%s: header too short\n", files[f]);
      fclose(file);
      failures++;
      continue;
    }
    if (show_only) {
      printf("%-40s mv_codec=0x%02x  coder=%s  %s\n", files[f], old_byte,
             (old_byte & 1) ? "range" : "golomb", mode_name(old_byte));
      fclose(file);
      continue;
    }
    int new_byte = old_byte;
    if (set_interp >= 0) {
      new_byte = (new_byte & ~2) | (set_interp << 1);
    }
    if (set_precision >= 0) {
      new_byte = (new_byte & ~4) | (set_precision << 2);
    }
    if (new_byte != old_byte) {
      if (fseek(file, (long)offset, SEEK_SET) != 0) {
        fprintf(stderr, "%s: re-seek failed\n", files[f]);
        fclose(file);
        failures++;
        continue;
      }
      fputc(new_byte, file);
    }
    printf("%-40s %s -> %s\n", files[f], mode_name(old_byte), mode_name(new_byte));
    fclose(file);
  }
  return failures ? 1 : 0;
}
