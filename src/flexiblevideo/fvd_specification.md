# FlexibleVideo (FVD) — Container and Bitstream Specification

FVD ("FlexibleVideo", FourCC `FVDC`, file extension `.fvd`) is a GPU-friendly video
codec. A single container can carry one of three *spatial* coding back-ends, selected
per stream:

* **DCT** — JPEG-style 8×8 (or adaptive 8/16/32) block DCT with rANS entropy coding
  (the default and quality leader),
* **Wavelet** — CDF 9/7 (lossy) / LeGall 5/3 (lossless) discrete wavelet transform with
  bit-plane coding (the FWV-compatible mode), or
* **H.264** — a parallel Annex-B elementary stream decoded by Vulkan Video hardware,
  with a wavelet/DCT stream as a software fallback.

On top of the spatial coder, FVD adds inter prediction (I/P plus hierarchical
bidirectional B-frames), two 3D temporal-wavelet GOP modes, optional adaptive
quantization, in-loop deblocking and CDEF, an optional full-resolution alpha plane,
SDR/HDR (BT.709 / BT.2020-PQ / HLG) color, 4:4:4 / 4:2:2 / 4:2:0 chroma, and an embedded
audio blob (Vorbis / FWA / QOA / raw PCM).

The reference implementation lives next to this document: `fvdenc.c` (encoder),
`fvdref.c` (shared CPU codec core), `fvdplay.c` (reference GPU player), `fvddec.c`
(decode-to-file), `fvd_h264.c` (H.264 hardware path), and the GPU compute shaders under
`shaders/`. A Pascal port exists in the PasVulkan engine
(`PasVulkan.Video.FlexibleVideo*.pas`).

---

## 1. Conventions

* All multi-byte integer fields are **little-endian** unless explicitly stated otherwise.
* Types: `u8`/`u16`/`u32`/`u64` = unsigned integers; `i32` = signed 32-bit.
* `>>` on signed values denotes an **arithmetic** right shift (floor division), which is
  significant for the reversible (lossless) transforms.
* All structures are **packed** (no implicit alignment padding).
* "Exp-Golomb" means the standard unsigned exponential-Golomb code: for value `v`, emit
  `floor(log2(v+1))` zero bits, then the `floor(log2(v+1))+1` bits of `v+1`, MSB first.
* "Plane" indices: 0 = Y (luma), 1 = Co, 2 = Cg, 3 = Alpha (when present).

---

## 2. File Layout

The file is written sequentially; the header is back-filled last so its offset fields are
final. Overall byte map:

| Region | Location | Presence |
|---|---|---|
| File header (228 bytes) | offset 0 | always |
| Frame payloads (coding order) | at each `FrameEntry.offset` | always |
| Audio blob | `audio_offset`, length `audio_size` | if `audio_size > 0` |
| H.264 Annex-B blob | `h264_offset`, length `h264_size` | if `h264_size > 0` |
| AQ QP-map | `qpmap_offset`, length `qpmap_size` | if `qpmap_size > 0` |
| Key-value store | `keyvalue_offset`, length `keyvalue_size` | if `keyvalue_size > 0` (reserved; not yet emitted) |
| Frame index (`FrameEntry[frame_count]`) | `index_offset` | always |
| CDEF strength table | `index_offset + frame_count*28` | if `color_flags` bit 4 |

---

## 3. File Header (228 bytes)

The header is `#pragma pack(1)` (byte-exact, no alignment padding); the layout below is
sequential. `header_size` guards reads of any future appended fixed fields.

| Off | Size | Type | Field | Meaning |
|----:|----:|------|-------|---------|
| 0   | 4 | u8[4] | `magic` | ASCII `"FVDC"` (`46 56 44 43`) |
| 4   | 2 | u16 | `version` | Format version = **1** |
| 6   | 2 | u16 | `header_size` | `sizeof(header)` = 228 (truncation guard) |
| 8   | 4 | u32 | `width` | Frame width in pixels |
| 12  | 4 | u32 | `height` | Frame height in pixels |
| 16  | 4 | u32 | `fps_num` | Frame-rate numerator |
| 20  | 4 | u32 | `fps_den` | Frame-rate denominator |
| 24  | 4 | u32 | `levels` | Spatial wavelet decomposition levels (wavelet mode) |
| 28  | 4 | u32 | `quality` | Global quality: 0 = lossless (5/3 or reversible DCT); ≥1 = lossy step |
| 32  | 4 | u32 | `frame_count` | Total frames in **coding** order |
| 36  | 1 | u8 | `bit_depth` | Source RGB bit depth: 8 = SDR, 12 = HDR |
| 37  | 1 | u8 | `color_primaries` | CICP: 1 = BT.709, 9 = BT.2020, 12 = Display-P3 |
| 38  | 1 | u8 | `transfer_function` | CICP: 13 = sRGB, 1 = BT.709, 16 = PQ, 18 = HLG, 8 = linear |
| 39  | 1 | u8 | `matrix` | CICP: always 8 (YCgCo / YCoCg-R) |
| 40  | 1 | u8 | `full_range` | Always 1 |
| 41  | 2 | u16 | `gop` | Max keyframe interval (seek hint) |
| 43  | 4 | u32 | `color_flags` | Feature / HDR bitfield — see §3.1 |
| 47  | 6 | u16[3] | `mastering_primaries_x[3]` | ST 2086 R/G/B chromaticity X ×50000 |
| 53  | 6 | u16[3] | `mastering_primaries_y[3]` | ST 2086 R/G/B chromaticity Y ×50000 |
| 59  | 2 | u16 | `mastering_white_x` | White point X ×50000 |
| 61  | 2 | u16 | `mastering_white_y` | White point Y ×50000 |
| 63  | 4 | u32 | `mastering_max_luminance` | MaxMDL cd/m² ×10000 |
| 67  | 4 | u32 | `mastering_min_luminance` | MinMDL cd/m² ×10000 |
| 71  | 2 | u16 | `max_content_light_level` | MaxCLL cd/m² |
| 73  | 2 | u16 | `max_frame_avg_light_level` | MaxFALL cd/m² |
| 75  | 1 | u8 | `prediction_method` | Inter-prediction mode — see §3.2 |
| 76  | 1 | u8 | `chroma_quant_x16` | Chroma quant multiplier ×16 (16 = 1.0 = off; treat 0 as 16) |
| 77  | 1 | u8 | `chroma_format` | Chroma subsampling — see §3.3 |
| 78  | 1 | u8 | `temporal_levels` | 3D-DWT/MCTF temporal decomposition levels (else 0) — see §3.4 |
| 79  | 1 | u8 | `temporal_wavelet` | Temporal wavelet: 0 = Haar, 1 = LeGall 5/3, 2 = CDF 9/7 — see §3.4 |
| 80  | 1 | u8 | `bframe_period` | Hierarchical B-frames per anchor pair (0 = no B-frames) — see §3.4 |
| 81  | 1 | u8 | `per_block_mode` | Non-zero ⇒ B-frame MV blobs carry an L0/L1/BI mode array — see §3.4 |
| 82  | 1 | u8 | `coding_block_size` | Bit-plane / rANS tile size in luma pixels: 32 / 64 / 128 (0 ⇒ 32) — see §3.4 |
| 83  | 1 | u8 | `motion_block_size` | Motion grid: 8 / 16 / 32 fixed, or 1 = variable quadtree (0 ⇒ 8) — see §3.4 |
| 84  | 4 | u32 | `mv_codec` | MV entropy + motion bitfield — see §3.5 |
| 88  | 4 | u8[4] | `audio_codec[4]` | Audio sub-FourCC — see §13 |
| 92  | 8 | u64 | `audio_offset` | Byte offset of audio blob (0 = none) |
| 100 | 8 | u64 | `audio_size` | Byte size of audio blob (0 = none) |
| 108 | 8 | u64 | `h264_offset` | Byte offset of H.264 Annex-B stream (0 = wavelet/DCT only) |
| 116 | 8 | u64 | `h264_size` | Byte size of H.264 stream (0 = none) |
| 124 | 8 | u64 | `qpmap_offset` | Byte offset of per-frame AQ QP-map (0 = no AQ) |
| 132 | 8 | u64 | `qpmap_size` | `frame_count × tile_cols × tile_rows` bytes (0 = no AQ) |
| 140 | 8 | u64 | `index_offset` | Byte offset of frame index |
| 148 | 8 | u64 | `keyvalue_offset` | Optional extensible key-value store (0 = none; reserved, not yet emitted) |
| 156 | 8 | u64 | `keyvalue_size` | Byte size of the key-value store |
| 164 | 64 | u32[16] | `reserved[16]` | Zero-filled; future fixed fields claim slots here within version 1 |

The mastering-display fields (offsets 47–74) are meaningful only when `color_flags`
bit 1 is set; otherwise they are zero-filled.

### 3.1 `color_flags` (u32 at offset 43)

| Bit | Mask | Name | Meaning |
|----:|------|------|---------|
| 0 | 0x01 | HDR | HDR stream (typically 12-bit BT.2020 PQ/HLG; `bit_depth=12`) |
| 1 | 0x02 | HDR10_METADATA | Mastering-display metadata (offsets 47–74) is valid |
| 2 | 0x04 | HAS_ALPHA | Optional full-resolution 8-bit alpha plane present (§14) |
| 3 | 0x08 | QUADTREE | Adaptive DCT transform-size partitioning active; partition section present (§6.4) |
| 4 | 0x10 | HAS_CDEF | CDEF in-loop deringing enabled; per-frame strength table follows the index (§6.7) |
| 5 | 0x20 | DEBLOCK | In-loop deblocking filter active (§6.6) |
| 6 | 0x40 | BFRAME_DCT | B-frames use DCT+rANS (else wavelet+bit-plane); requires `prediction_method=1` |
| 7 | 0x80 | SPATIAL_DCT | Intra/spatial coding is DCT+rANS; **clear = wavelet+bit-plane** (primary FVD-vs-FWV bit) |

Bits 8–31 are reserved (zero). Note: premultiplied-alpha signaling is **out of band** — the
encoder writes no premultiply flag bit (the PasVulkan compositor treats alpha as straight).

### 3.2 `prediction_method` (u8 at offset 75)

| Value | Name | Meaning |
|------:|------|---------|
| 0 | CoefDiff | Wavelet-domain coefficient-difference I/P (no motion; MV blob always empty) |
| 1 | ColorDiff | Pixel-domain (YCoCg-R) motion-compensated I/P, plus B-frames when `bframe_period > 0` |
| 2 | OpenLoop3DDWT | Open-loop 3D temporal-wavelet GOP (no motion) |
| 3 | MCTF3DDWT | Motion-compensated temporal-filtering 3D-DWT GOP (predict-only MC-Haar) |

### 3.3 `chroma_format` (u8 at offset 77)

| Value | Name | Chroma plane size |
|------:|------|-------------------|
| 0 | 4:4:4 | `width × height` (default) |
| 1 | 4:2:2 | `ceil(width/2) × height` |
| 2 | 4:2:0 | `ceil(width/2) × ceil(height/2)` |

Luma (plane 0) and alpha (plane 3) are always full resolution. Chroma is box-averaged on
encode and bilinearly upsampled (center-sited) on decode.

### 3.4 Codec configuration fields (offsets 78–83)

These eight named u8 fields configure the coder; each is `0` when not applicable.

| Off | Field | Meaning |
|----:|-------|---------|
| 78 | `temporal_levels` | Temporal decomposition levels (3D-DWT/MCTF modes; else 0) |
| 79 | `temporal_wavelet` | Temporal wavelet: 0 = Haar, 1 = LeGall 5/3, 2 = CDF 9/7 (3D-DWT modes) |
| 80 | `bframe_period` | B-frame period (B-frames per anchor pair; 0 = no B-frames) |
| 81 | `per_block_mode` | Non-zero ⇒ B-frame MV blobs carry an L0/L1/BI mode array (§8.3) |
| 82 | `coding_block_size` | Bit-plane / rANS tile size in luma pixels: 32 / 64 / 128 (0 ⇒ 32) |
| 83 | `motion_block_size` | Motion block size: 8 / 16 / 32 fixed grid, or 1 = variable quadtree (root 32, leaf 8); 0 ⇒ 8 |

### 3.5 `mv_codec` (u32 at offset 84)

| Bits | Meaning |
|------|---------|
| 0 | MV entropy coder: 0 = signed Exp-Golomb (default), 1 = adaptive binary range coder |
| 1–2 | Interpolation filter: 0 = bilinear (legacy), 1 = 6-tap half-pel (default), 2 = 8-tap HEVC DCTIF |
| 3–4 | MV precision: 0 = half-pel, 1 = quarter-pel (default) |

Bits 5–31 are reserved (zero).

---

## 4. Frame Index and CDEF Table

The index is an array of `frame_count` `FrameEntry` records (28 bytes each) at
`index_offset`, in **coding order**. Display order is recovered from `poc`.

### `FrameEntry` (28 bytes)

| Off | Size | Type | Field | Meaning |
|----:|----:|------|-------|---------|
| 0  | 8 | u64 | `offset` | Byte offset of this frame's payload |
| 8  | 4 | u32 | `size` | On-disk payload byte size (includes the 5-byte frame framing of §6.1) |
| 12 | 4 | u32 | `poc` | Display position (Presentation/Picture Order Count) |
| 16 | 4 | i32 | `ref0` | Coding-order index of the L0 (forward) reference; -1 = none |
| 20 | 4 | i32 | `ref1` | Coding-order index of the L1 (backward) reference; -1 = none |
| 24 | 1 | u8 | `type` | 0 = I, 1 = P, 2 = B |
| 25 | 1 | u8 | `quality` | Per-frame quality (the QP-cascaded value actually used; §8.5) |
| 26 | 1 | u8 | `temporal_id` | Temporal hierarchy level (0 = anchor) |
| 27 | 1 | u8 | `alpha_mv_mode` | Per-frame alpha motion mode: 0 = alpha shares the luma MVs, 1 = alpha carries its own MV field (in the alpha section, §14). 0 when no alpha. |

If `color_flags` bit 4 (CDEF) is set, a per-frame strength table immediately follows the
index at `index_offset + frame_count*28`: `frame_count` records of 4 bytes each
`{luma_primary, luma_secondary, chroma_primary, chroma_secondary}` (u8), in coding order.

B-frame blend weights are **not** stored; the decoder derives them from POC (§8.3).

---

## 5. Per-Frame Coded Payload

### 5.1 On-disk frame framing

Each frame at `FrameEntry.offset` begins with a 5-byte envelope, then the (possibly
compressed) payload:

```
[u8  method][u32 raw_len][compressed_bytes...]
```

`FrameEntry.size` counts these 5 bytes plus `compressed_bytes`.

| `method` | Decompressor |
|---:|---|
| 0 | Raw — copy `raw_len` bytes verbatim |
| 1 | LZSS — 16-bit window LZ, 32-bit control words, LE offsets |
| 2 | LZBRRC — range-coded LZ (PasVulkan `PasVulkan.Compression.LZBRRC`) |

### 5.2 Decompressed payload structure

After decompression the payload always begins with:

```
[u32 leading_count]        // luma block count (sanity)
[u32 size_blob_length]
[size_blob ...]            // per-block byte sizes, unsigned Exp-Golomb, planes Y,Co,Cg in order
[u32 mv_length]            // 0 for I-frames and CoefDiff
[mv_data ...]              // motion vectors (§8.4), present only when mv_length > 0
[u32 data_length]
[block_data ...]           // packed bit-plane (wavelet) or rANS (DCT) bytes for all planes
```

The decoder prefix-sums `size_blob` to obtain each block's byte offset into `block_data`,
enabling fully parallel per-block GPU decode.

Then, depending on the coding mode, the following optional sections are appended in this
exact order:

1. **Entropy section** — present iff `color_flags` bit 7 (SPATIAL_DCT) or, for B-frames,
   bit 6 (BFRAME_DCT). See §5.3.
2. **Partition section** — present iff DCT mode **and** `color_flags` bit 3 (QUADTREE)
   **and** lossy. See §5.4 and §6.4.
3. **Alpha section** — present iff `color_flags` bit 2 (HAS_ALPHA). See §14.
   (Exception: on the GPU bidirectional B path the alpha section is re-read by a separate
   CPU pass rather than appended inline.)

### 5.3 Entropy section (DCT)

Appended immediately after `block_data`. For each plane (Y, Co, Cg in order):

```
[u32 table_length][table_bytes...]    // the plane's rANS frequency-table blob
```

There is no outer length prefix; the plane count is implicit.

### 5.4 Partition section (quadtree)

Appended after the entropy section. For each plane: `regions_per_plane` raw u8 region
codes (no length prefix). `regions_per_plane = qt_region_count(plane_w) * qt_region_count(plane_h)`,
`qt_region_count(extent) = ceil(extent / 32)`. The region code is defined in §6.4.

---

## 6. Spatial Coding — DCT Path (`color_flags` bit 7 set)

### 6.1 Color transform and chroma

RGB is converted to **YCoCg-R**, a reversible integer lifting (`matrix = 8`):

```
forward:  Co = R - B;  t = B + (Co >> 1);  Cg = G - t;  Y = t + (Cg >> 1)
inverse:  t = Y - (Cg >> 1);  G = Cg + t;  B = t - (Co >> 1);  R = B + Co
```

Chroma (Co, Cg) is box-averaged to the `chroma_format` resolution after the color
transform, and bilinearly upsampled with center siting
(`src = (out + 0.5) / 2^shift - 0.5`) on decode.

### 6.2 Block DCT

The base transform unit is **8×8** (`DCT_BLOCK = 8`); the quadtree mode also uses 16×16
and 32×32 leaves (`DCT_MAX_SIZE = 32`). Two transforms exist:

* **Lossy (quality ≥ 1):** orthonormal separable DCT-II, rows then columns. Normalization
  `alpha(0) = sqrt(1/N)`, `alpha(k≥1) = sqrt(2/N)`.
* **Lossless (quality = 0):** a **reversible integer DCT** built from lifting. Fixed-point
  fractional bits `INTDCT_F = 12`; each 8-point transform splits into even/odd halves,
  each factored into 6 Givens rotations, each rotation realized as 3 lifting steps
  (`a += round(p*b); b += round(u*a); a += round(p*b)`, with
  `round(k*x) = (k*x + (1<<(F-1))) >> F`). The integer transform is unnormalized
  (coefficients ≈ 2× the orthonormal magnitude). The forward applies rotations in reverse
  table order; the inverse in forward order with each lifting step subtracted. The exact
  per-rotation `(p, u)` constants are in `fvdref.c` (`even`/`odd` rotation tables) and
  mirrored in `shaders/dct_inv_int.comp`.

### 6.3 Quantization

Quantization uses the standard JPEG (ITU-T T.81 Annex K) 8×8 base tables, scaled by
`quality`:

```
Luma base (row-major):                    Chroma base (row-major):
16 11 10 16 24 40 51 61                    17 18 24 47 99 99 99 99
12 12 14 19 26 58 60 55                    18 21 26 66 99 99 99 99
14 13 16 24 40 57 69 56                    24 26 56 99 99 99 99 99
14 17 22 29 51 87 80 62                    47 66 99 99 99 99 99 99
18 22 37 56 68 109 103 77                  99 99 99 99 99 99 99 99
24 35 55 64 81 104 113 92                  99 99 99 99 99 99 99 99
49 64 78 87 103 121 120 101                99 99 99 99 99 99 99 99
72 92 95 98 112 100 103 99                 99 99 99 99 99 99 99 99
```

Per-coefficient step at position `(u, v)` of an N×N block:

```
q = max(1, round(base_quality * sample_white_scale * table_N[v*N+u] / 16))
```

where `sample_white_scale = sample_white / 256` (SDR = 1, HDR-12 = 16), `table_N` is the
8×8 table resampled to N×N via `sv = (n==8) ? v : (v*7)/(n-1)`, and (quadtree only) the
result is additionally multiplied by `qt_size_scale(N) = (N/8)^QTEXP` before the `/16`.
The chroma planes additionally multiply by `chroma_quant` (= `chroma_quant_x16/16`). The
per-pixel step buffer is built for `padded_height = ceil(height/N)*N`.

Quantize is a dead-zone: `level = floor(|coeff| / q)` (sign preserved), so the zero bin is
2× wide. Dequantize reconstructs at the midpoint: `out = (level + 0.5) * q`, or 0 if
`level == 0`.

### 6.4 Quadtree adaptive transform sizes (`color_flags` bit 3)

A region is `QT_REGION = 32` pixels. One byte per region encodes the partition:

* bit 0 = split the 32×32 region into four 16×16 quadrants;
* bits 1–4 = (when bit 0 set) for each 16×16 quadrant in raster order
  (TL, TR, BL, BR), 1 = split that quadrant further into four 8×8 leaves.

So bit0=0 ⇒ one 32×32 leaf; bit0=1, quadrant bit=0 ⇒ one 16×16 leaf; both set ⇒ four 8×8.
A 32 or 16 leaf is placed only if it fully fits the plane; otherwise it is force-split.
`qt_region_count(extent) = ceil(extent/32)`. The size→step exponent
`QTEXP` (`g_qt_size_exp`) defaults to 0.5 and is overridable by the `QTEXP` environment
variable. DC prediction (§6.5) runs across leaves in region-raster then leaf order.

### 6.5 rANS Entropy Coding

The DCT coefficients of each coding tile (`coding_block_size` luma pixels, default 128) are
entropy-coded with a 32-bit, byte-renormalizing **rANS** coder (ryg-style; encode in
reverse, decode forward).

* `RANS_M = 4096` — total normalized frequency.
* `RANS_L = 2^23` — lower renormalization bound; state stays in `[L, (L<<8)-1]`.
* `RANS_DC_SYMBOLS = 17` — DC-difference size categories 0..16.
* `RANS_AC_SYMBOLS = 256` — AC tokens `sym = (run << 4) | size`; `0x00` = EOB, `0xF0` = ZRL.

State init reads 4 bytes LE; encode flush writes the state as 4 bytes LE. Decode renorm:
`state = freq*(state >> 12) + slot - cum; while (state < L) state = (state << 8) | next_byte`.

**DC** coefficients are DPCM-predicted from the previous sub-block's DC in tile-scan
order; the difference's size category is rANS-coded, then `size-1` magnitude bits + 1 sign
bit are written raw (bypass). **AC** coefficients use a JPEG zigzag scan (8×8 table below;
16×16 and 32×32 use the same anti-diagonal generation) and run/size tokens; the magnitude
and sign bits are raw bypass.

```
8×8 zigzag:
 0  1  8 16  9  2  3 10
17 24 32 25 18 11  4  5
12 19 26 33 40 48 41 34
27 20 13  6  7 14 21 28
35 42 49 56 57 50 43 36
29 22 15 23 30 37 44 51
58 59 52 45 38 31 39 46
53 60 61 54 47 55 62 63
```

The per-plane **frequency table** in the entropy section (§5.3) stores all 17 DC + 256 AC
normalized frequencies as unsigned Exp-Golomb codewords. The decoder expands these into
the GPU lookup table layout (per-plane): DC norm[17], DC cum[17], AC norm[256], AC
cum[256], DC slot→symbol[4096], AC slot→symbol[4096] (8738 u32 total). Each per-tile blob
is `[LEB128 rans_length][rans_length rANS bytes][raw bypass bytes]`.

### 6.6 Deblocking filter (`color_flags` bit 5)

An H.264-style 4-tap in-loop filter, applied on the reconstructed integer YCoCg plane
after the inverse DCT round and before CDEF, on the 8-pixel grid, only where adjacent
8×8 cells belong to different transform leaves. Vertical boundaries are filtered first,
then horizontal. Parameters derive from the bit-depth-scaled QP `q`:

```
alpha = q;  beta = (q >> 1) + 1;  tc = (q >> 2) + 1
```

Filter (samples `p1 p0 | q0 q1`), applied only when
`|q0-p0| < alpha && |p1-p0| < beta && |q1-q0| < beta`:

```
delta = clamp(((4*(q0-p0) + (p1-q1)) + 4) >> 3, -tc, +tc);  p0 += delta;  q0 -= delta
```

The same filter runs on luma and both chroma planes.

### 6.7 CDEF (`color_flags` bit 4)

AV1-faithful integer CDEF, per 8×8 block: one dominant direction is found, then all 64
pixels are filtered with primary + secondary taps along that direction. Per-frame
strengths come from the 4-byte CDEF table record (§4). Damping derives from quality:
`damping = min(6, 3 + quality/4)` (0 = disabled at Q0). Primary taps are `[4,2]` when
`pri_strength` is even, `[3,3]` when odd; secondary taps `[2,1]`. The 8 direction tap
offsets, the constraint function, and the `div_table[9] = {0,840,420,280,210,168,140,120,105}`
match AV1 and are in `fvdref.c` / `shaders/cdef.comp`. The encoder searches strengths per
frame by minimizing SSE (luma 6×4 candidates, chroma 4×3).

### 6.8 Adaptive Quantization (AQ)

When the QP-map section is present (`qpmap_size > 0`), each `AQ_TILE = 64`-pixel tile
carries one u8 code. A 256-entry weight LUT maps codes to step multipliers:
`weight = AQ_WMIN * (AQ_WMAX/AQ_WMIN)^(code/255)`, `AQ_WMIN = 0.5`, `AQ_WMAX = 2.0`. The
per-pixel quant step is modulated as `q = max(1, round(base_step * weight))`. The QP-map is
stored once per stream (`frame_count × tile_cols × tile_rows` bytes, coding order,
`tile_cols = ceil(width/64)`, `tile_rows = ceil(height/64)`).

---

## 7. Spatial Coding — Wavelet Path (`color_flags` bit 7 clear)

This is the FWV-compatible spatial mode.

### 7.1 Discrete Wavelet Transform

Separable (rows then columns, via a transpose), `levels` decomposition levels, with
whole-sample symmetric reflection at the borders.

* **Lossy (quality ≥ 1): CDF 9/7** lifting. Constants:
  `ALPHA = -1.586134342`, `BETA = -0.052980118`, `GAMMA = 0.882911076`,
  `DELTA = 0.443506852`, `SCALE = 1.230174105`. Forward order: predict (odd += ALPHA·…),
  update (even += BETA·…), predict (odd += GAMMA·…), update (even += DELTA·…), scale
  (even ×= 1/SCALE, odd ×= SCALE), then deinterleave evens→low, odds→high. Inverse reverses
  these steps.
* **Lossless (quality = 0): LeGall 5/3** integer lifting.
  `predict: d[k] -= (s[k-1]+s[k+1]) >> 1`, `update: s[k] += ((d[k-1]+d[k+1]) + 2) >> 2`,
  then deinterleave. Exactly reversible (int32, arithmetic shifts).

### 7.2 Mallat subband layout

All levels are packed into one `width × height` buffer (row stride = `width`). At each
level the current `cw × ch` rectangle splits at `half_w = (cw+1)/2`, `half_h = (ch+1)/2`
into LL (top-left, iterated next level), HL (right), LH (bottom), HH (bottom-right). The
coarsest LL ends in the top-left corner.

### 7.3 Quantization steps

Per-subband steps come from the measured CDF 9/7 synthesis L2 gain per subband (cached by
`levels`):

```
csf  = (orientation==HH ? 1.4 : 1.0) * (level==0 ? 1.25 : 1.0)
step = max(1, round(base_quality * 2.0 * csf / (gain[level][orientation] / gain_ref)))
```

`base_quality` is pre-scaled by `sample_white/256` (HDR ×16). The final LL band uses
`step = round(base_quality * 2.0 / ll_relative_gain)`. Quantize is a dead-zone
(`floor(|coeff|/step)`), dequant reconstructs at the midpoint. Chroma additionally
multiplies by `chroma_quant`.

### 7.4 Bit-plane coding

Each `BS × BS` block (`BS` = `coding_block_size`, 32/64/128) is byte-aligned and independently
decodable. Format, MSB-first within bytes, bytes into 32-bit LE words:

1. **5-bit `bit_plane_count`** = number of planes for the largest magnitude in the block.
2. For each plane from MSB down to 0: a **2-bit method** flag:
   * `00` raw — one bit per coefficient in raster order;
   * `01` RLE — Exp-Golomb popcount, then Exp-Golomb gaps between set positions;
   * `10` quadtree — recursive any-set flags down the BS→1 quadtree, leaf carries the bit.
   The encoder picks the smallest. The sign bit is written inline at a coefficient's first
   set bit (1 = negative).

---

## 8. Inter Prediction (`prediction_method` = ColorDiff)

### 8.1 Frame types, GOP, B-frames

* **I** (`type=0`): no reference (`ref0=ref1=-1`).
* **P** (`type=1`): L0 only (`ref1=-1`, `temporal_id=0`).
* **B** (`type=2`): both references; hierarchical (dyadic midpoint) when `bframe_period > 0`.

B-frame streams (`prediction_method=1` and `bframe_period > 0`) have period
`bframe_period + 1`. Within an anchor pair `[lo, hi]`, the hi anchor is coded first (as I or
P), then the B-frames in dyadic midpoint order; coding order ≠ display order. The midpoint
B at display `mid` has `ref0=lo`, `ref1=hi`, `temporal_id = recursion depth`. The decoder
reads in coding order, places frames in a DPB indexed by coding index, and recovers
display order from `poc`. A reconstructed frame is evicted once no later coding index
references it.

### 8.2 Motion

* Motion block grid: `motion_block_size` — 8/16/32 fixed (default 16), or 1 = variable quadtree
  (root 32, leaf 8). Minimum motion block 8 (OBMC ramps).
* Interpolation (`mv_codec` bits 1–2): 6-tap H.264 luma half-pel `(1,-5,20,20,-5,1)`
  (default), or 8-tap HEVC DCTIF, or legacy bilinear. Quarter-pel (default precision,
  `mv_codec` bits 3–4 = 1) averages neighboring half-pel grid points (6-tap) or uses the
  HEVC quarter phases (8-tap). MVs are stored in half-pel (×2) or quarter-pel (×4) integer
  units.
* **OBMC** (overlapped block MC) is applied on every MC output: each pixel blends its own
  block prediction with the nearest vertical and horizontal neighbor blocks using an edge
  ramp window; `own_weight = 8 - vweight - hweight`, blend `= (sum + 4) >> 3`.
* Prediction: I ⇒ 0; P ⇒ `ref0`; B ⇒ `((w0*ref0 + w1*ref1) + 128) >> 8`.

### 8.3 Per-block modes (B-frames)

When `per_block_mode != 0`, each motion block carries a 2-bit mode: 0 = L0, 1 = L1, 2 = BI.
The mode array is coded with its own merge-if-equal quadtree (split flags RLE-coded, leaf
mode written raw 2 bits). Blend weights are derived from POC, not stored:

```
w0 = (256 * (poc_ref1 - poc_self)) / (poc_ref1 - poc_ref0);  w1 = 256 - w0
```

(256-based 8-bit fixed point).

### 8.4 MV entropy coding

`mv_codec` bit 0 selects:

* **Signed Exp-Golomb (default):** zigzag-map `m = (v<<1) ^ (v>>31)`, then unsigned
  Exp-Golomb of `m`.
* **Adaptive binary range coder:** LZMA-style 16-bit-probability range coder; residuals use
  truncated-unary magnitude class (cap 20) + bypass mantissa; context = left/up neighbor
  magnitudes bucketed into 6 buckets (CABAC-like). Split flags and per-block modes are
  coded inline in the same stream. One stream per MV blob (per-frame random access).

The MV predictor is the median of causal neighbors (left, up, up-right for the fixed grid;
left, up, up-left for the quadtree fine grid).

### 8.5 QP cascading

When enabled (default), per-frame quality is scaled by temporal level:
`frame_quality = round(base_quality * 2^(0.6 * temporal_id^1.14))`. Anchors
(`temporal_id=0`) are unchanged; deeper B-frames are quantized more coarsely. The result is
stored directly in `FrameEntry.quality`; the decoder uses that value.

---

## 9. CoefDiff Mode (`prediction_method` = 0)

A pure wavelet, motion-free I/P mode: the wavelet coefficients of a P-frame are coded as a
difference against the previous frame's coefficients. The MV blob is always empty; no
entropy/partition section (wavelet only). The alpha section may still be present.

---

## 10. Temporal 3D-DWT Modes

### 10.1 Open-loop 3D-DWT (`prediction_method` = 2)

A GOP of frames (default 16, max 64) is transformed along the temporal axis with the
wavelet in `temporal_wavelet` for `temporal_levels` levels:

* **Lossless (Q0):** integer Haar S-transform (`high = a-b; low = b + (high>>1)`) or 5/3
  (a 9/7 request falls back to 5/3 at Q0).
* **Lossy:** float Haar / 5/3 / 9/7.

Per pixel column the GOP samples are temporally transformed (deinterleaved low|high), then
each resulting temporal-subband frame is spatially transformed and bit-plane coded like an
intra frame. Per-temporal-level quant scaling is
`scale = 2^(0.6 * level^1.14)` (level 0 = deepest low-pass, finest). Coding order equals
display order; GOP boundaries are detected from frame `type` (0 = GOP start, 2 = continuation).

### 10.2 MCTF (`prediction_method` = 3)

A motion-compensated, predict-only MC-Haar temporal filter (valid only inside the GOP
mode). Forward: `low[k] = even; high[k] = odd - OBMC(even)`. Inverse:
`even = low; odd = high + OBMC(even)`. Exactly invertible (the same forward MV field is
used). The luma MV field is shared across all planes (scaled per plane); the alpha plane may
instead carry its own per-high-pass-frame MV field (`alpha_mv_mode = 1`, §14). High-pass frames
(temporal level > 0) carry an MV blob; the base level-0 frame carries none. MV entropy
follows `mv_codec` (§8.4).

---

## 11. H.264 Dual-Stream Path

When `h264_size > 0`, an Annex-B H.264 elementary stream (start codes `00 00 01`; SPS/PPS,
IDR and non-IDR slices; per-frame POC and sliding-window/MMCO reference management) lives at
`h264_offset`. The reference player decodes it with Vulkan Video hardware
(`VK_VIDEO_CODEC_OPERATION_DECODE_H264_BIT_KHR`, High profile, progressive,
`VK_FORMAT_G8_B8R8_2PLANE_420_UNORM` NV12, ≤8 DPB slots), then converts NV12→RGB on the
graphics queue with the BT.709 limited-range matrix:

```
y  = (luma - 16) * 1.164;  cb = chroma.x - 128;  cr = chroma.y - 128
R = y + 1.793*cr;  G = y - 0.213*cb - 0.533*cr;  B = y + 2.112*cb
```

Chroma is upsampled nearest-neighbor (to match the ffmpeg yuv420p→rgb reference). The
decoder auto-selects: if `h264_size != 0` **and** the GPU supports H.264 decode it uses the
hardware path; otherwise it falls back to the wavelet/DCT stream. The two paths do not
switch mid-stream.

---

## 12. HDR and Color

The codec transports PQ/HLG code values as generic 12-bit integers; the color space is pure
container metadata (CICP at offsets 37–40). HDR internal format uses 16-bit samples,
`sample_max = 4095`, `sample_white = 4096` (so quality scales ×16 vs SDR).

PQ (SMPTE ST 2084) constants: `m1 = 0.1593017578125`, `m2 = 78.84375`, `c1 = 0.8359375`,
`c2 = 18.8515625`, `c3 = 18.6875`. PQ decode:
`vp = code^(1/m2); out = (max(vp-c1,0) / (c2 - c3*vp))^(1/m1)`. HLG uses the BT.2100 inverse
OETF (`a=0.17883277, b=0.28466892, c=0.55991073`) plus the OOTF. The BT.2020→Rec.709
3×3 matrix and a reference white of 80 nits / peak 10000 nits are applied on presentation.
An optional scRGB output (`rgba16f`, 1.0 = 80 nits, no tonemap) is opt-in via the player
(`--output=scrgb`); autodetect defaults to safe SDR tonemapping.

---

## 13. Audio

Audio is a self-contained blob at `audio_offset` (length `audio_size`), identified by the
`audio_codec[4]` FourCC:

| Tag | Codec | Notes |
|-----|-------|-------|
| `OGGV` | Ogg/Vorbis | **Default**; a complete Ogg/Vorbis file (decoded via stb_vorbis). Also the fallback for any unrecognized tag. |
| `FWAC` | Flexible Wavelet Audio | The sub-codec specified in `fwa_specification.md`. |
| `QOAL` | QOA (little-endian) | Custom LE QOA; blob starts with `"qoal"` + u32 total samples. |
| `RPCM` | Raw PCM | 12-byte header `[u32 rate][u32 channels][i32 bits]` (bits>0 int, bits<0 float), then samples. |

---

## 14. Alpha Channel (`color_flags` bit 2)

An optional 8-bit, full-resolution alpha plane (plane 3), always full size regardless of
chroma subsampling. Alpha is **inter-predicted exactly like the color planes** and coded as a
per-frame residual: in ColorDiff P-frames it is motion-compensated against the previous
reconstructed alpha; in B-frames it is bidirectionally motion-compensated (the same L0/L1
weights / per-block mode as color); in the temporal modes (open-loop 3D-DWT and MCTF) it
**joins the temporal transform** (the same per-pixel-column wavelet / motion-compensated MC-Haar
over the GOP as the color planes). It is **intra** only in I-frames and in the CoefDiff
(`prediction_method` 0) mode (whose color is itself a coefficient-domain diff without motion).
The alpha keeps its **own** spatial decision (`alpha_qp == 0` ⇒ reversible 5/3, else float 9/7
in wavelet mode, or rANS+DCT in DCT mode; `table_length > 0` distinguishes the two on decode)
**and its own temporal domain** in the GOP modes (a lossless alpha matte stays bit-exact even on
a lossy open-loop color GOP), independent of the color quality. Alpha is never AQ-modulated
(plain `alpha_qp` steps). The transform is otherwise luma-like.

### Alpha motion vectors (`FrameEntry.alpha_mv_mode`)

By default the alpha rides the **shared luma motion vectors** (`alpha_mv_mode = 0`, no extra MV
cost). But when the matte moves independently of the color content (transparency edges that
translate differently from the picture), the encoder may give the alpha its **own** motion
field for that frame (`alpha_mv_mode = 1`); the decoder uses whichever the per-frame flag
selects. The own MVs are always coded **full-grid** (never quad-tree), with the same entropy
coder and predictor as the luma MVs (`mv_codec`, §8.4):

* **ColorDiff P / I:** one MV field (against the single previous reference).
* **B-frames:** a dual blob — L0 then (when `ref1 ≥ 0`) L1 in one stream — using the same POC
  blend weights as color; the per-block L0/L1/BI mode stays the shared luma mode.
* **MCTF:** one MV field per high-pass frame (`temporal_id`/temporal level > 0); low-pass
  frames carry none. (Open-loop 3D-DWT has no motion, so always `alpha_mv_mode = 0`.)

The encoder chooses shared vs own per frame via the `--alpha-mv=<luma|own|cpu-rd|sad>` setting
(`luma` = always shared, the default → `alpha_mv_mode` always 0; `own` = always own; `cpu-rd` =
whichever codes to fewer actual bytes; `sad` = a residual-SAD heuristic incl. the MV-blob cost).
This is purely an encode-time decision — the bitstream only records the resulting per-frame
`alpha_mv_mode` plus, when 1, the own MV blob.

The alpha section (appended per frame, §5.2) is:

```
[u8  alpha_qp]
[u32 mv_blob_length][mv_blob ...]        // own alpha MVs — present ONLY when FrameEntry.alpha_mv_mode = 1
[u32 size_blob_length][size_blob ...]    // Exp-Golomb block sizes, luma block count
[u32 table_length][table ...]            // rANS table (DCT mode); table_length = 0 ⇒ wavelet
[u32 data_length][alpha_data ...]
```

The `mv_blob` line is omitted entirely when `alpha_mv_mode = 0` (shared luma MVs). `alpha_qp`
is per-frame (may differ from `FrameEntry.quality`). Decoders without alpha support stop after
the three color planes and ignore this section.

---

## 15. Decoder Notes

* Validate `magic == "FVDC"` and `version == 1`; check `header_size == 228`. `header_size`
  also guards reads of any future appended fixed fields.
* The frame index is the authoritative random-access structure; seek by `poc`, then decode
  forward in coding order resolving `ref0`/`ref1` against already-decoded frames.
* The spatial back-end is chosen by `color_flags` bit 7 (and bit 6 for B-frames); the
  inter model by `prediction_method`; both are orthogonal to chroma, HDR, alpha, deblock,
  CDEF and AQ, which are independent feature flags.
* The reference GPU decoder requires a compute device able to host the rANS/DCT pipelines;
  see the engine's capability gating for the minimum invocation/shared-memory floor.
