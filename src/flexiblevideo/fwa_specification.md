# FlexibleWaveletAudio (FWA) — Container and Bitstream Specification

FWA ("Flexible Wavelet Audio", FourCC `FWAC`) is a wavelet-based audio codec with a
lossless mode (reversible LeGall 5/3 transform or an adaptive LMS predictor) and a lossy
mode (CDF 9/7 transform with optional adaptive wavelet packets, a psychoacoustic
quantization model, intensity joint-stereo, and block cross-fade overlap). It supports up
to 16 channels with pairwise mid/side decorrelation. Both modes share a single
magnitude-class range-coder entropy stage.

FWA is used standalone and as the `FWAC` audio sub-codec inside an FVD video container (see
`fvd_specification.md`). The reference implementation is `fwa_audio.c` / `fwa_audio.h`
beside this document; a Pascal port lives in the PasVulkan engine
(`PasVulkan.Audio.FlexibleWavelet*.pas`).

---

## 1. Conventions

* Multi-byte integer fields are **little-endian** unless stated otherwise.
* Types: `u8`/`u16`/`u32`/`u64` unsigned; `i16`/`i32` signed.
* `>>` on signed values is an **arithmetic** right shift (floor division) — required for
  the reversible (lossless) transforms.
* PCM samples are 16-bit signed (`i16`), processed internally as `i32` per channel
  (planar).
* "ZigZag map" maps a signed integer to unsigned: `u = (x << 1) ^ (x >> 31)`.

---

## 2. Container Layout

```
[ 24-byte header ]
[ pairing plan ]        // only if flags bit 4 (Pairing)
[ u16 overlap_samples ] // only if flags bit 5 (Overlap)
[ block 0 ][ block 1 ] ... [ block N-1 ]
```

There is **no inline block index / offset table**. A decoder builds an offset table at open
time by walking the blocks once (each channel payload is length-prefixed, §4).

### 2.1 Header (24 bytes)

| Off | Size | Type | Field | Meaning |
|----:|----:|------|-------|---------|
| 0  | 4 | u32 | `magic` | `0x43415746` = ASCII `"FWAC"` (bytes `46 57 41 43`) |
| 4  | 4 | u32 | `sample_rate` | PCM sample rate in Hz |
| 8  | 2 | u16 | `channels` | Channel count, 1–16 |
| 10 | 2 | u16 | `block_samples` | Samples per block per channel; must equal `BLOCK_SAMPLES` = 8192 |
| 12 | 2 | u16 | `quality` | 0 = lossless; ≥1 = lossy quant step |
| 14 | 2 | u16 | `flags` | Mode flags — see §2.2 |
| 16 | 8 | u64 | `frame_count` | Total PCM frames (samples per channel) |

There is no integer version field; format identity is the magic plus the `flags` bits and
the fixed `block_samples = 8192`.

### 2.2 `flags` (u16 at offset 14)

| Bit(s) | Mask | Name | Meaning |
|-------:|------|------|---------|
| 0 | 0x0001 | Perceptual | Psychoacoustic ATH-shaped per-band quant step (lossy only) |
| 1 | 0x0002 | Packet | Adaptive wavelet-packet best-basis (lossy only) |
| 2 | 0x0004 | JointStereo | Intensity joint stereo: Side high bands zeroed (lossy, stereo only) |
| 3 | 0x0008 | LMS | Lossless LMS predictor instead of 5/3 wavelet (Q0 only) |
| 4 | 0x0010 | Pairing | Multichannel pairwise M/S plan present after the header |
| 5 | 0x0020 | Overlap | Block cross-fade overlap; a `u16 overlap_samples` follows the pairing plan |
| 8–15 | 0xFF00 | (LMS taps) | When LMS is set, the LMS tap count = `(flags >> 8) & 0xFF` |

### 2.3 Pairing plan (only if bit 4 set)

```
[u8 pair_count]
  for each pair: [u8 channel_a][u8 channel_b][u8 mode]
```

`mode`: 1 = mid/side applied to this pair, 0 = the pair is coded independently. The decision
is made once at encode time (it is not switched per block). Up to `channels/2` ≤ 8 pairs.

### 2.4 Overlap field (only if bit 5 set)

A single `u16 overlap_samples`, immediately after the pairing plan (or directly after the
header if there is no pairing plan).

---

## 3. Block Structure

* Block size: `BLOCK_SAMPLES = 8192` samples per channel.
* Block stride: 8192 normally; with overlap, `block_stride = BLOCK_SAMPLES - overlap_samples`
  (so adjacent blocks share `overlap_samples`).
* Block count: `ceil(frame_count / block_stride)`. The last block may be shorter:
  `min(BLOCK_SAMPLES, frame_count - block_start)`.

Within a block, all channels are stored consecutively. For each channel:

```
[u32 payload_length][payload_length bytes]
```

The payload is the entropy-coded coefficients. In **packet** mode (flags bit 1) the payload
is prefixed by the packet tree:

```
[u16 tree_byte_len][tree_byte_len tree bytes][coefficient range-coder bytes]
```

There is no per-block header beyond these per-channel length prefixes.

---

## 4. Channel Decorrelation (Mid/Side)

For stereo (`channels == 2`), mid/side is applied unconditionally. For ≥3 channels it is
applied to the pairs listed in the pairing plan whose `mode == 1`. The lifting is exactly
reversible:

```
forward:  S = a - b;  a = b + (S >> 1);  b = S      // 'a' becomes Mid, 'b' becomes Side
inverse:  R = a - (S >> 1);  a = R + S;  b = R       // with S = stored side
```

(For stereo, `a = L`, `b = R`.) On decode the reconstructed samples are clamped to
`[-32768, 32767]` (a no-op at Q0). The optional adaptive decision (which pairs get M/S) uses
a sum-of-magnitudes proxy at encode time and is recorded in the pairing plan's `mode` byte.

---

## 5. Lossless Path (`quality == 0`)

Two mutually exclusive coders, selected by `flags` bit 3 (LMS).

### 5.1 Reversible LeGall 5/3 wavelet (bit 3 clear)

Integer lifting on the per-channel samples, with whole-sample symmetric reflection at the
borders. One level:

```
predict (odd k):  data[i] -= (data[i-1] + data[i+1]) >> 1
update  (even k): data[i] += ((data[i-1] + data[i+1]) + 2) >> 2
deinterleave:     evens (smooth) -> [0, half),  odds (detail) -> [half, length)
```

`half = (length + 1) / 2`. The inverse interleaves, undoes the update, then undoes the
predict. The transform is applied for `dwt_level_count(length)` levels: keep halving the
low band while it stays ≥ `MIN_BAND*2 = 8` (so a full 8192-sample block yields 10 levels).
All arithmetic is `int32` with arithmetic shifts — bit-exactly reversible.

### 5.2 LMS predictor (bit 3 set)

A QOA-style N-tap **sign-sign LMS** predictor replaces the wavelet entirely. The tap count
(4–32, `LMS_MAX_TAPS = 32`) is stored in `flags[15:8]`. State: `history[taps]` and
`weights[taps]`, all zero except (for taps ≥ 2) `weights[taps-2] = -8192` and
`weights[taps-1] = 16384` (a 2nd-order extrapolation prior).

```
predict():  acc = sum_i(weights[i] * history[i])   // i64; return (i32)(acc >> 13)
adapt(sample, residual):
    delta = residual >> adapt_shift
    for i: weights[i] += (history[i] < 0) ? -delta : +delta
    shift history left by one; history[taps-1] = sample   // reconstructed sample
forward:  residual = sample - predict();  store residual;  adapt(sample, residual)
inverse:  residual = stored;  sample = residual + predict();  adapt(sample, residual)
```

`adapt_shift = 4 + log2(taps/4)` (4 at 4 taps, 5 at 8, 6 at 16, 7 at 32). The predictor runs
sequentially over the whole channel; a decoder seeking backward must replay from block 0
(state is carried across blocks).

---

## 6. Lossy Path (`quality >= 1`)

### 6.1 CDF 9/7 wavelet

Float lifting, replacing 5/3, with whole-sample symmetric reflection. Constants:

```
ALPHA = -1.586134342059924   BETA = -0.052980118572961
GAMMA =  0.882911075530934   DELTA = 0.443506852043971
SCALE =  1.230174104914001
```

Per level: predict (odd += ALPHA·neighbors), update (even += BETA·…), predict (odd +=
GAMMA·…), update (even += DELTA·…), scale (odd ×= SCALE, even ×= 1/SCALE), then deinterleave
low|high. Level count is the same dyadic rule as §5.1 (~10 levels for 8192 samples). 9/7 is
biorthogonal (non-unit synthesis energy) — relevant to the packet R-D cost (§6.3).

### 6.2 Quantization

The base step is the raw `quality` value. Per-coefficient step is `step_per_coeff[i]`,
which equals `base_step` uniformly, or `base_step * band_weight[band(i)]` in perceptual
mode (§6.4). Dead-zone rounding:

```
quantize:    index = trunc(coeff / step[i] + (coeff >= 0 ? +0.5 : -0.5))
dequantize:  coeff = index * step[i]                 // midpoint reconstruction; 0 -> 0
```

The coefficient-to-band map uses `band_starts`, the ascending start indices of the dyadic
subbands `[0, LL_size, next_band, ...]`.

### 6.3 Wavelet packets / best-basis (flags bit 1)

Instead of splitting only the low band, the encoder may recursively split **any** band. The
chosen binary tree is transmitted as a preorder bit-stream per channel:

* Each node is **1 bit**, MSB-first: `1` = split (children follow), `0` = leaf.
* The decoder reads bits in preorder, recursing on the low half then the high half when it
  sees a `1`, and calling the inverse 9/7 level at each split on the way back up. The number
  of tree bits is not stored explicitly — the recursion terminates naturally at leaves.
* The tree is byte-packed and prefixed by `u16 tree_byte_len` (§3).

The encoder's split decision is rate-distortion: split iff
`cost_low + cost_high < cost_leaf`, with `cost = rate + lambda*distortion`,
`rate = sum_i log2(|coeff_i| + 1)`,
`distortion = len * (step^2/12) * synthesis_gain_squared[depth]`, and
`lambda = 0.0012` (overridable via the `FWA_LAMBDA` environment variable). The
`synthesis_gain_squared[]` table is measured once per encode (unit-impulse synthesis through
`depth` inverse 9/7 levels) to account for the biorthogonal filter's energy. Maximum depth
is the dyadic level count; splits below 8 samples are rejected. Packet mode is forced off at
quality 0.

### 6.4 Psychoacoustic model (flags bit 0)

Per-band quant weighting from the absolute threshold of hearing (Terhardt):

```
f_kHz   = max(frequency_Hz / 1000, 0.02)
ATH(f)  = 3.64*f_kHz^-0.8 - 6.5*exp(-0.6*(f_kHz - 3.3)^2) + 0.001*f_kHz^4   (dB SPL)
```

Band center frequencies are sample-rate dependent (`nyquist = sample_rate/2`,
`levels = dwt_level_count(length)`): band 0 (LL) center `= nyquist / 2^(levels+1)`; detail
band `b ≥ 1` center `= nyquist * 0.75 / 2^max(levels-b, 0)`. The per-band weight is

```
weight[b] = clamp(10^((ATH[b] - min_ATH)/20), 1.0, 8.0)
```

(`PERCEPTUAL_MAX_WEIGHT = 8.0`) and `step_per_coeff[i] = base_step * weight[band(i)]`. The
decoder reproduces the model from `(length, base_step, sample_rate)` — no per-band metadata
is transmitted.

### 6.5 Joint stereo (flags bit 2)

Active when lossy **and** `joint` **and** not packet **and** `channels == 2`. After the
forward 9/7 of the Side channel (channel 1 post-M/S) and before quantization, the top
`JOINT_TOP_BANDS = 2` frequency bands of the Side coefficient array are zeroed
(`zero_start = band_starts[band_count - 2]`, set `[zero_start, length)` to 0; skipped if
`band_count ≤ 2`). The decoder needs no special branch — the zeroed coefficients simply
decode as zeros (Side high frequencies become 0).

### 6.6 Block overlap cross-fade (flags bit 5)

Lossy only (forced off at Q0). Default 1024 overlap samples (clamped to ≤ `BLOCK_SAMPLES/2`).
Each block is still coded independently and in full; the cross-fade is entirely a
decoder-side operation on the first `overlap` samples of every block after block 0, in the
int32 sample domain (before M/S inverse). The window is the rising half of a raised-cosine
(Hann), **not** a linear ramp:

```
weight = 0.5 - 0.5*cos(pi * (i + 0.5) / overlap)         for i in [0, overlap)
out[i] = (1 - weight)*prev_tail[i] + weight*cur[i]
```

`weight + (1-weight) = 1` preserves amplitude. The decoder retains
`block[block_stride .. block_stride+overlap)` as `prev_tail` for the next block.

---

## 7. Entropy Coding (both modes)

All coefficients (5/3 residuals, LMS residuals, or quantized 9/7 indices) are coded with the
same scheme: a magnitude-class symbol model driven by a **Subbotin carryless range coder**.
One independent range-coder stream per channel per block (contexts reset each block).

### 7.1 Coefficient symbol mapping

1. ZigZag map the signed value to unsigned `v`.
2. Magnitude class `klass = (v == 0) ? 0 : floor(log2(v)) + 1` (number of bits to represent
   `v`), range 0..32 (`CLASS_COUNT = 33`).
3. For `klass ≥ 2`, the mantissa is the low `klass-1` bits of `v` (i.e.
   `v - (1 << (klass-1))`). `klass = 0` ⇒ value 0; `klass = 1` ⇒ value 1 (no mantissa).

### 7.2 Coding

* The class symbol is range-coded adaptively with a per-subband context.
* Mantissa bits (for `klass ≥ 2`) are written as `klass-1` equiprobable (1/2) bypass bits
  through the range coder.

### 7.3 Range coder

Subbotin carryless range coder: `low` (u32), `range` (u32, init `0xFFFFFFFF`), and (decoder)
`code` (u32). Thresholds `RANGE_TOP = 1<<24`, `RANGE_BOTTOM = 1<<16`. Encode renormalize
while `((low ^ (low+range)) < RANGE_TOP) || (range < RANGE_BOTTOM)` (the bottom branch sets
`range = (-low) & (RANGE_BOTTOM-1)`), emitting the top byte of `low` and shifting both left
8. Flush emits 4 bytes of `low` MSB-first; the decoder initializes `code` from 4 bytes
MSB-first and renormalizes symmetrically.

### 7.4 Context model

`BAND_CONTEXTS = 8` separate class models: one per each of the first 8 subbands, with all
further subbands sharing context 7. Each class context holds per-symbol counts (33 symbols,
all initialized to 1, total 33) and is rescaled (`count = (count+1) >> 1`) when the total
reaches `CLASS_CAP = 16384`.

The codec uses **no** Rice/Golomb or Exp-Golomb coding for audio coefficients — only the
magnitude-class + range-coder scheme above. (An unused adaptive `BinaryContext` type exists
in the source but is not part of the coefficient path.)

---

## 8. Decoder Notes

* Validate `magic == "FWAC"` and `block_samples == 8192`; reject otherwise.
* Read the optional pairing plan and overlap field before the first block; build a block
  offset table by scanning the length-prefixed per-channel payloads.
* Decode each block per channel: range-decode coefficients (replaying the packet tree first
  in packet mode), dequantize (lossy) or take residuals directly (lossless), invert the
  wavelet (5/3 or 9/7, or run the LMS inverse), apply the cross-fade overlap (lossy), then
  invert M/S, and clamp to `i16`.
* LMS and overlap never coexist (LMS is lossless-only, overlap lossy-only). For LMS,
  decoding is inherently sequential; a backward seek must replay from block 0.
* The same entropy model is shared with the FVD video container's `FWAC` audio sub-codec;
  the blob embedded there is byte-for-byte a standalone FWA stream.
