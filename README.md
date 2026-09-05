> [!IMPORTANT]
> The primary repository has moved to [git.rosseaux.net/BeRo1985/pasvulkan](https://git.rosseaux.net/BeRo1985/pasvulkan).
> This GitHub repository is kept up-to-date via push mirroring.

# PasVulkan

**A Vulkan-based game engine and application framework for Object Pascal.**

PasVulkan started in 2016 as an auto-generated Vulkan header binding for Object Pascal. It has since grown into a complete, self-contained engine: a GPU-driven Vulkan renderer with clustered forward+ shading, several global illumination techniques, hardware ray tracing, a procedural planet renderer, a 2D canvas with signed-distance-field text rendering, a widget toolkit, a software audio mixer with HRTF spatialization, an asset pipeline, a scripting language, a physics engine, a networking library — and even an in-process RISC-V emulator and a local LLM inference engine.

Almost everything is written in Object Pascal from the ground up. There is no C/C++ middleware in the core: image loaders, font rasterization, audio mixing, video decoding, compression, physics and networking are all native Pascal implementations. The only mandatory runtime dependency is the Vulkan loader. On Windows the framework talks to the native Win32 APIs directly and needs nothing else, while SDL 2.x is used as the OS abstraction layer on Linux.

## Contents

- [Status and scope](#status-and-scope)
- [Feature overview](#feature-overview)
  - [Vulkan bindings and framework](#vulkan-bindings-and-framework)
  - [Application framework](#application-framework)
  - [Scene3D: the GPU-driven 3D renderer](#scene3d-the-gpu-driven-3d-renderer)
  - [Planet and atmosphere rendering](#planet-and-atmosphere-rendering)
  - [2D rendering, text and UI](#2d-rendering-text-and-ui)
  - [Audio](#audio)
  - [Video](#video)
  - [Assets, file formats and I/O](#assets-file-formats-and-io)
  - [Scripting, physics, networking and more](#scripting-physics-networking-and-more)
  - [Diagnostics and platform integration](#diagnostics-and-platform-integration)
- [Supported platforms](#supported-platforms)
- [Repository layout](#repository-layout)
- [Requirements](#requirements)
- [Getting started](#getting-started)
- [Projects in this repository](#projects-in-this-repository)
- [Sibling libraries](#sibling-libraries)
- [Documentation](#documentation)
- [Support me](#support-me)
- [About me / My contact details](#about-me--my-contact-details)
- [License (zlib)](#license-zlib)
- [General guidelines for code contributors](#general-guidelines-for-code-contributors)
- [Showcase videos](#showcase-videos)

## Status and scope

PasVulkan is an open source engine under continuous, active development, and free to use under the zlib license. It is not a packaged product though: there is no stable API promise, no versioned release cycle, and documentation is spread across the source code and the `docs/` folder. Expect to read Pascal sources.

Development is driven by the games built with it — the ones in `projects/` are the engine's own test bed, and features land when those games need them, not on a roadmap.

The `Vulkan.pas` binding header (and its `vkxml2pas.dpr` generator) is usable stand-alone and stays compatible even with the old Delphi 7 compiler. The `PasVulkan.*.pas` framework units are not — they use generics, operator overloading, advanced records and other modern Object Pascal syntax.

## Feature overview

### Vulkan bindings and framework

- **`Vulkan.pas`** — a complete, C-API-style Vulkan header for Object Pascal, auto-generated from `vk.xml` by `vkxml2pas.dpr`, so it is always up to date. Currently tracks Vulkan 1.4 (`VK_HEADER_VERSION` 346), including all registered extensions.
- **`PasVulkan.Framework.pas`** — an object-oriented abstraction over the raw API: instances, devices, queues, command buffers, swap chains, render passes, pipelines, descriptor sets, buffers, images, samplers, synchronization primitives and so on.
- **Vulkan memory management** — a best-fit red-black-tree-based sub-allocator that packs allocations into few large device memory blocks, keeping the live allocation count well below `maxMemoryAllocationCount`, with allocation groups for budget tracking and a dedicated visualizer tool.
- **Texture loading** with native Pascal decoders — KTX, KTX2, DDS, HDR (Radiance), PNG (all types including 16-bit channels), JPEG, TGA, QOI and BMP — plus block-compressed formats (BC, ETC2, ASTC) passed straight through to the GPU, optional automatic GPU mipmap generation, and sRGB / additional-sRGB view handling.
- **Screenshot and capture API** for swap chain and framebuffer contents, with native PNG, JPEG and QOI writers.
- **Transfer queue** with asynchronous, batched uploads, and a **buffer range allocator** for suballocating within large shared buffers.
- **Ray tracing** (`PasVulkan.Raytracing.pas`) — BLAS/TLAS construction and management, compaction, build queues, instance and geometry info management, and a cull-mask system.
- **`PasVulkan.FrameGraph.pas`** — a render graph that resolves pass dependencies, allocates and aliases transient resources, and inserts the required barriers and layout transitions automatically, across multiple queues.

### Application framework

- **`PasVulkan.Application.pas`** — a Vulkan-optimized application/screen framework whose overall design mixes VCL/LCL/FCL and libGDX concepts. It handles the whole lifecycle: device selection, swap chain creation and recreation, frame pacing, input, and screen/state management.
- The OS backend is chosen per platform behind the framework's own abstraction. **Windows uses the native Win32 APIs directly** — own window class and message loop, Win32 GameInput for controllers, MMSYSTEM WaveOut (a thin WASAPI wrapper on Windows 10 and newer) for audio output — so no SDL is involved there. **Linux uses SDL 2.x**, Android has its own path, and a headless mode exists for tools and tests.
- **Automatic recovery** from `VK_ERROR_SURFACE_LOST_KHR`, `VK_ERROR_OUT_OF_DATE_KHR` and `VK_SUBOPTIMAL_KHR`. Critical situations such as `VK_ERROR_DEVICE_LOST` remain the application's responsibility.
- Single-window by design, for maximum cross-platform friendliness.
- **`PasVulkan.Win32.GameInput.pas`** for Windows GameInput controller support, and **`PasVulkan.VirtualReality.pas`** with an OpenVR backend plus a VR-aware screen class.

### Scene3D: the GPU-driven 3D renderer

`PasVulkan.Scene3D*.pas` implements a strict **GPU-driven, clustered forward+ renderer** built around an "everything-in-one-single-buffer" design: all vertex, index and material data lives in one buffer per data type, accessed bindlessly, which is what makes GPU culling, single-draw-call batching and hardware ray tracing over the same data possible. See [`docs/renderer_design.md`](docs/renderer_design.md) for the full rationale.

**Geometry and culling**

- Compute-shader mesh preprocessing (skinning, morph targets, normals/tangents) into a double-buffered vertex buffer, which also yields motion vectors for free.
- **Hi-Z two-pass occlusion culling** with temporal-coherence visibility, supporting both standard-Z and reversed-Z depth conventions.
- **Meshlets** with optional mesh/task shader pipelines, plus meshlet bounds and cone culling.
- A **GPU-driven LOD system** with temporal and immediate modes.
- **Virtual instances** — automatic, temporally stable sharing of GPU resources between similar instances ([`docs/scene3d_virtualinstances.md`](docs/scene3d_virtualinstances.md)).
- Frustum cluster grid build and light assignment compute passes for forward+ light culling.

**Materials and lighting**

- Full **glTF 2.0** material support: metallic-roughness, specular-glossiness and unlit shading models, with `KHR_materials_clearcoat`, `_sheen`, `_transmission`, `_diffuse_transmission`, `_volume`, `_ior`, `_specular`, `_iridescence`, `_anisotropy`, `_dispersion`, `_emissive_strength`, `KHR_texture_transform`, `KHR_texture_basisu`, `KHR_lights_punctual`, `KHR_animation_pointer` and `KHR_node_visibility`.
- Precomputed GGX, Charlie and sheen-E BRDF lookup tables, plus image-based lighting with cubemap IBL filtering, spherical harmonics, reflection probes and gradient environments.
- **Shadows**: cascaded shadow maps with PCF, DPCF, PCSS and MSM filtering, plus blur and resolve passes, reflective shadow maps and a top-down sky occlusion map.
- **Global illumination**, selectable at runtime:
  - `EnvironmentMap` — static IBL environment.
  - `CameraReflectionProbe` — a per-frame updated cubemap around the camera.
  - `CascadedRadianceHints` — RSM-injected cascaded radiance hints with bounce passes.
  - `CascadedVoxelConeTracing` — cascaded voxelization with cone tracing over voxel mipmaps.
  - `DynamicUnifiedGlobalIllumination` (DUGI) — a cascaded probe grid traced against the ray tracing TLAS, storing irradiance as spherical harmonics or an octahedral atlas plus an octahedral mean/mean-squared distance term for Chebyshev visibility, which avoids the light leaking of radiance hints. Requires hardware ray tracing.
- **Decals** — order-stable projected decals with group masks ([`docs/scene3d_decals.md`](docs/scene3d_decals.md)).
- A GPU particle system with its own BVH build pass.

**Transparency**

Eight selectable order-independent transparency modes: `Direct`, spinlock and interlock per-pixel linked-list OIT, `LOOPOIT`, weighted-blended OIT (`WBOIT`), moment-based OIT (`MBOIT`), and deep & fast approximate OIT in spinlock and interlock variants.

**Antialiasing, upscaling and post-processing**

- Antialiasing: `None`, `DSAA`, `FXAA`, `SMAA`, `SMAA T2x`, `MSAA`, `MSAA+SMAA` and `TAA`.
- Resampling with Lanczos or **FSR EASU/RCAS**, plus a CNN-based upscaler (2× / 4×, three quality levels).
- Ground-truth ambient occlusion (GTAO) with depth mip-chain and blur passes.
- Depth of field in five variants: half-res separate near/far, half-res bruteforce, full-res hexagon bokeh and full-res bruteforce, with an auto-focus compute pass.
- Tile-based motion blur, radial blur, volumetric scattering (raymarch, blur, compose), fog, lens flares/dirt/star, lens rain, screen rain and wetness maps.
- Luminance histogram, average and adaptation passes for auto-exposure.
- 18 tone mapping operators including ACES, Uncharted 2, Uchimura, Lottes, AMD, AgX (Rec.709 and Rec.2020, in base, golden and punchy looks) and Khronos PBR Neutral, plus HDR display output with faithful and BT.2390-style display mapping, and a parametric color grading stage (exposure, night adaptation, white balance, channel mixer, shadows/midtones/highlights, ASC CDL, curves, contrast, vibrance, saturation, hue) that loads and saves its settings as JSON.
- GPU picking, selection masks and selection outlines.

### Planet and atmosphere rendering

`PasVulkan.Scene3D.Planet.pas` is a full procedural planet system: GPU heightmap terrain with brush-based editing, blend map layers, tiled LOD, grass rendering with age maps, and a pipe-model water simulation (with FP16/FP32 shader variants selected per GPU) covering flow, settling, shore foam, whitecaps, Gerstner wave displacement, rain splashes and caustics.

`PasVulkan.Scene3D.Atmosphere.pas` provides a physically based atmosphere with Rayleigh/Mie scattering and absorption, volumetric clouds with coverage/type/wetness maps and cloud shadows, precipitation, and support for multiple atmospheres (double-precision transforms, so several planets can coexist).

### 2D rendering, text and UI

- **`PasVulkan.Canvas.pas`** — a batched 2D canvas with paths, strokes, fills, gradients, clipping, blend modes and transformation stacks, usable stand-alone or composited into the 3D pipeline.
- **`PasVulkan.TrueTypeFont.pas` / `PasVulkan.Font.pas`** — a native TrueType/OpenType loader with experimental PostScript-flavoured (CFF Type 2) support and a TrueType hinting bytecode interpreter, plus on-the-fly parallelized high-quality **2D signed distance field** generation ([Practical Analytic 2D SDF Generation](https://web.archive.org/web/20160909051854/http://malideveloper.arm.com/downloads/Presentations/Siggraph16/Practical_Analytic_2D_Signed_Distance_Field_Generation.pdf), parallelized with [PasMP](https://git.rosseaux.net/BeRo1985/pasmp)).
- **`PasVulkan.VectorPath.pas`** and **`PasVulkan.SignedDistanceField2D.pas`** — vector path handling and SDF generation for shapes and glyphs.
- **Sprites and sprite atlases** built on the fly into array textures, with automatic trimming, padding, trimmed hull vectors, and sprites generated directly from vector paths or SVG path data as signed distance fields — so no external atlas packing tool is needed.
- **`PasVulkan.GUI.pas`** — a retained-mode widget toolkit with a vector-based skin: windows, dialogs, popups, menus, buttons, checkboxes, single- and multi-line text edits, scrollbars, sliders, progress bars, panels, tabs, list boxes, combo boxes, splitters, color wheel/picker, list views, tree views and file dialogs, with fill/box/group/grid/advanced-grid/flow layouts.
- **`PasVulkan.Console.pas`**, **`PasVulkan.TextEditor.pas`** (a rope-based text editor core with undo/redo and UTF-8 line cache) and PasTerm-based terminal emulation.
- **`PasVulkan.PasHTMLDownCanvasRenderer.pas`** — Markdown rendering onto the canvas.

### Audio

`PasVulkan.Audio.pas` is a complete software mixer running on its own thread with a lock-free command queue:

- 3D spatialization with **HRTF** convolution, speaker layout handling, distance models, Doppler shift, per-voice low-pass filtering and panning.
- Reverb (all-pass/comb network), a pitch shifter, a compressor/limiter, and cubic-spline and windowed-SINC resamplers.
- Streaming music, sample voices with global voice management, and audio for video playback.
- Codecs: WAV, Ogg Vorbis (native and Tremor), **QOAL**, **RPCM** and the **FlexibleWavelet** codec (encoder and decoder).

### Video

- **FlexibleVideo** — an own video container and codec with encoder-side tooling and a player integrated with the audio system. The spatial transform is selectable per stream, and separately for B-frames: either an 8×8 DCT with JPEG-style quantization matrices and rANS entropy coding, optionally with adaptive quad-tree transform sizes and a reversible integer DCT for lossless output, or a wavelet with bit-plane coding. Prediction is coefficient difference, colour difference, open-loop 3D wavelet or MCTF 3D wavelet.
- Further codec features: I, P and B frames with motion compensation and exp-Golomb or range-coded motion vectors, 4:4:4 / 4:2:2 / 4:2:0 chroma, adaptive per-tile quantization, an optional alpha plane that can carry its own motion field and its own AQ map, in-loop deblocking and AV1-style CDEF deringing, per-frame payload compression, HDR mastering metadata, an embedded audio track and an extensible key-value header.
- **Decoding runs on the GPU** — compute pipelines for entropy unpack, inverse transform, deblocking and deringing, fed from ring buffers.
- **GPU H.264 decoding through `VK_KHR_video_decode_h264`** — a FlexibleVideo container can carry an H.264 Annex-B elementary stream alongside its native stream, and where the GPU exposes Vulkan video decode the player decodes it on the GPU, falling back to the container's own decoder otherwise. The Pascal side is the bitstream front-end (Annex-B NAL splitting, SPS/PPS/slice header and POC parsing) driving the Vulkan video session, DPB and NV12 output.
- An AVI writer for capturing.

### Assets, file formats and I/O

- **`PasVulkan.VirtualFileSystem.pas`** — a layered virtual file system that transparently overlays loose files and archives.
- Archives: **SPK** (own "Simple Package" format) and ZIP.
- Compression: Deflate, LZMA and the own **LZBRS / LZBRSF / LZBRSX / LZBRRC** family, behind a unified `PasVulkan.Compression.pas` facade.
- 3D file formats: **glTF 2.0** (via PasGLTF), OBJ, FBX, DAE/Collada, IES photometric light profiles, and **SAM** ("Simple Animated Model File", a compact vertex-animated format produced by the `gltf2sam` tool).
- Data formats: JSON (via PasJSON), XML, Base64, and a Unicode layer via PUCU.
- Hashes and PRNGs: xxHash64, FastHash, RapidHash, SHA-3, and a fast random generator.
- Containers and structures: extensive generic collections, sparse sets, hierarchical generation arrays, circular doubly linked lists, timed priority queues, dynamic and static AABB trees, dynamic rect trees, triangle BVHs, CSG/BSP, 2D convex hull generation, and Fibonacci/ico/UV sphere generators.
- **`PasVulkan.Math.pas`** — vectors, matrices, quaternions, AABBs, spheres, frustums, with swizzling and a full double-precision counterpart in `PasVulkan.Math.Double.pas`.
- **`PasVulkan.EntityComponentSystem.pas`** — an ECS with registered component types, system dependency resolution and serialization.

### Scripting, physics, networking and more

- **[POCA](https://git.rosseaux.net/BeRo1985/poca)** — a JavaScript/ECMAScript-like scripting language with a register-based VM, bound to the engine via `PasVulkan.POCA.pas` and `PasVulkan.POCA.Scene3D.pas`. It is used for real game logic: the HUD, context menus and much of the gameplay in `supercubi` are written in POCA. Even some shader-side data (sample kernels and lookup tables) is generated by POCA scripts at build time.
- **[Kraft](https://git.rosseaux.net/BeRo1985/kraft)** — the 3D physics engine: rigid bodies, convex hulls, meshes, constraints, raycasting, continuous collision detection.
- **[RNL](https://git.rosseaux.net/BeRo1985/rnl)** — a realtime UDP networking library with encryption, channels, congestion control and reliable/unreliable delivery.
- **[PasLLM](https://git.rosseaux.net/BeRo1985/pasllm)** + **[Pinja](https://git.rosseaux.net/BeRo1985/pinja)** — local LLM inference in pure Object Pascal with 4-bit quantization formats, and a Jinja-subset template engine for prompt templates.
- **[PasRISCV](https://git.rosseaux.net/BeRo1985/pasriscv)** — an RV64GCV/RVA23 emulator, wrapped by `PasVulkan.PasRISCVEmulator.pas`, so a full Linux system can run inside an application.
- **[PasMP](https://git.rosseaux.net/BeRo1985/pasmp)** — the job system behind parallel asset loading, SDF generation, mesh processing and simulation, with a simple parallel job executor wrapper.
- **[FLRE](https://git.rosseaux.net/BeRo1985/flre)** — regular expressions, and **[PasHTMLDown](https://git.rosseaux.net/BeRo1985/pashtmldown)** for Markdown.

### Diagnostics and platform integration

- **`PasVulkan.Profiler.pas`**, **`PasVulkan.FrameTrace.pas`**, **`PasVulkan.TimerQuery.pas`** and a PasMP profiler history view for CPU and GPU timing.
- Vulkan debug utils integration (object labels, command buffer regions), validation layer toggles, and **NVIDIA Aftermath** support for GPU crash dumps.
- **Crash reporting** with minidump writing and Steam integration, a **hang watchdog** thread, and a symbol mapper tool that injects DWARF/PDB symbol data for readable stack traces.
- **Steamworks** bindings (`PasVulkan.Steamworks.pas` + a framework layer) for achievements, cloud saves, lobbies and overlay handling, loaded dynamically so builds run without Steam.
- Tools under `src/tools/`: the project manager, BRDF lookup texture generator, glTF→SAM converter, PNG→raw converter, `bin2pas`, a Vulkan memory visualizer, a Scene3D dump analyzer, swizzle code generation, Steamworks layout/vtable/dispatch verifiers, and training tools for the DFAOIT and upscaler networks.

## Supported platforms

| Platform | Architectures | Notes |
| --- | --- | --- |
| Windows | x86-32, x86-64 | native Win32 backend, no SDL, FPC and Delphi, primary development and shipping target |
| Linux | x86-32, x86-64, ARM32, AArch64 | X11 and Wayland via SDL 2, primary development target |
| Android | x86-32, x86-64, ARM32, AArch64 | via the project manager and Android Studio toolchain |
| macOS / iOS | x86-64, AArch64 | through the MoltenVK wrapper — experimental and largely untested |

## Repository layout

```
pasvulkan/
  src/                    Engine sources (Vulkan.pas, PasVulkan.*.pas)
    assets/shaders/       GLSL shaders (scene3d, canvas, virtualreality)
    tools/                Engine tools (project manager, converters, verifiers, ...)
  externals/              Git submodules (poca, kraft, pasmp, rnl, pasllm, pasriscv, ...)
  projects/               Games, examples and test projects
  docs/                   Design and subsystem documentation
  tests/                  Engine-level tests
```

## Requirements

- **Free Pascal** — a current trunk build (>= 3.3.1) is recommended, and [fpcupdeluxe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe) is the easiest way to get one — together with **Lazarus**. Alternatively a recent **Delphi** version.
- The **Vulkan SDK** (for `glslangValidator` when recompiling shaders) and a Vulkan-capable GPU with up-to-date drivers.
- **SDL 2.x** development libraries and runtime.
- For the Android target additionally: Android Studio, the Java SDK, the Android SDK and NDK, all installed at Google's prescribed default locations, plus FPC cross compilers for *all* Android CPU targets (i386, x86-64, ARM32, AArch64).

## Getting started

### Cloning

The repository has a long history and large assets. If a plain clone is too slow or too large:

```bash
git clone --single-branch --depth 1 --recursive https://github.com/BeRo1985/pasvulkan.git pasvulkan
```

or via SSH:

```bash
git clone --single-branch --depth 1 --recursive git@github.com:BeRo1985/pasvulkan.git pasvulkan
```

If the transfer still fails, disabling git's compression can help (note that this is a global setting):

```bash
git config --global core.compression 0
```

Afterwards, fetch and update the submodules:

```bash
./initsubmodules      # initsubmodules.bat on Windows
./updatesubmodules    # updatesubmodules.bat on Windows
```

### Building with the project manager

The project manager handles asset compilation, cross-target builds and project creation. Build it once:

```bash
./compileprojectmanager      # compileprojectmanager.bat on Windows
```

Then, for the example project:

| Step | Windows | *nix | Description |
| --- | --- | --- | --- |
| 1. | `projectmanager compileassets examples` | `./projectmanager compileassets examples` | Compiles the asset files |
| 2. | `projectmanager build examples` | `./projectmanager build examples` | Compiles the code |
| 3. | `projectmanager run examples` | `./projectmanager run examples` | Starts the binary |

Creating a new project:

| Step | Windows | *nix | Description |
| --- | --- | --- | --- |
| 1. | `projectmanager create [yourprojectname]` | `./projectmanager create [yourprojectname]` | Creates a new project from the template |

*Important:* the project name must be a valid lowercase Pascal identifier *and* a valid Java identifier *and* a valid file name, all at the same time.

Run `projectmanager -h` for the full option and target list. `build` accepts explicit targets such as `fpc-x86_64-linux`, `fpc-x86_64-windows`, `fpc-aarch64-android` or `delphi-x86_64-windows`.

### Building directly with Lazarus / lazbuild / Delphi

Every project also carries a plain `.lpi` (Lazarus) and `.dpr`/`.dproj` (Delphi) in its `src/` directory and can be built without the project manager:

```bash
cd projects/gltfviewer/src
lazbuild -B gltfviewer.lpi
```

`-B` (full rebuild) is recommended — incremental builds of the larger projects can trip FPC internal errors.

### Recompiling shaders

```bash
cd src/assets/shaders/scene3d
./compileshaders.sh          # compileshaders.bat on Windows
```

## Projects in this repository

| Project | Description |
| --- | --- |
| `supercubi` | A 2D jump'n'run whose gameplay, physics and rendering are almost entirely written in POCA — the scripting and 2D testbed. |
| `gltfviewer` | A glTF 2.0 / OBJ / FBX model viewer used to test and debug loading, PBR materials, animation and the renderer in isolation. |
| `examples` | The example/demo application: triangle, cube, canvas, GUI, model and dragon screens. |
| `videoexample`, `pocaexample`, `consoleexample`, `markdownviewertest` | Focused feature demos. |
| `physics2dtest`, `sdfmeshgen` | Testbeds for physics and signed distance field mesh generation. |
| `template` | The skeleton used by `projectmanager create`. |

## Sibling libraries

All of these are separate projects of mine, included here as submodules under `externals/`:

| Library | Purpose |
| --- | --- |
| [POCA](https://git.rosseaux.net/BeRo1985/poca) | ECMAScript-like scripting language |
| [Kraft](https://git.rosseaux.net/BeRo1985/kraft) | 3D physics engine |
| [PasMP](https://git.rosseaux.net/BeRo1985/pasmp) | Parallel processing / job system |
| [RNL](https://git.rosseaux.net/BeRo1985/rnl) | Realtime UDP networking library |
| [PasGLTF](https://git.rosseaux.net/BeRo1985/pasgltf) | glTF 2.0 loader and writer |
| [PasJSON](https://git.rosseaux.net/BeRo1985/pasjson) | JSON library |
| [PasDblStrUtils](https://git.rosseaux.net/BeRo1985/pasdblstrutils) | Exact float ↔ string conversion |
| [PUCU](https://git.rosseaux.net/BeRo1985/pucu) | Unicode utilities |
| [FLRE](https://git.rosseaux.net/BeRo1985/flre) | Regular expressions |
| [PasHTMLDown](https://git.rosseaux.net/BeRo1985/pashtmldown) | Markdown library |
| [PasTerm](https://git.rosseaux.net/BeRo1985/pasterm) | Terminal emulator core |
| [PasRISCV](https://git.rosseaux.net/BeRo1985/pasriscv) | RV64GCV/RVA23 emulator |
| [PasLLM](https://git.rosseaux.net/BeRo1985/pasllm) | LLM inference engine |
| [Pinja](https://git.rosseaux.net/BeRo1985/pinja) | Jinja-subset template engine |

## Documentation

Design documents live in [`docs/`](docs/):

- [`renderer_design.md`](docs/renderer_design.md) — renderer architecture, why forward+, the single-buffer design, Hi-Z two-pass occlusion culling
- [`scene3d_virtualinstances.md`](docs/scene3d_virtualinstances.md), [`scene3d_decals.md`](docs/scene3d_decals.md)
- [`memory_management_in_pasvulkan.md`](docs/memory_management_in_pasvulkan.md), [`framepacing.md`](docs/framepacing.md)
- [`canvas_transparency_rendering.md`](docs/canvas_transparency_rendering.md), [`vectorpath.md`](docs/vectorpath.md)
- [`pocascriptapi.adoc`](docs/pocascriptapi.adoc) — the PasVulkan POCA scripting API

## Support me

Creating is my passion, and with your support, I can keep it alive. Support my work and help me continue innovating. Every contribution makes a difference: [You can donate and support me here.](https://donate.rosseaux.com/) Thank you!

## About me / My contact details

- [My website](https://www.rosseaux.net)
- [My blog](https://blog.rosseaux.net)
- [My Twitter account](https://twitter.com/coder)
- [My Facebook account](https://www.facebook.com/benjamin.rosseaux)

## License (zlib)

    Copyright (C) 2016-2026, Benjamin Rosseaux (benjamin@rosseaux.de)

    This software is provided 'as-is', without any express or implied
    warranty. In no event will the authors be held liable for any damages
    arising from the use of this software.

    Permission is granted to anyone to use this software for any purpose,
    including commercial applications, and to alter it and redistribute it
    freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
       claim that you wrote the original software. If you use this software
       in a product, an acknowledgement in the product documentation would be
       appreciated but is not required.
    2. Altered source versions must be plainly marked as such, and must not be
       misrepresented as being the original software.
    3. This notice may not be removed or altered from any source distribution.

The engine itself is zlib licensed. Some submodules under `externals/` use different licenses — FLRE is LGPL v2.1 with static-linking exception, PasLLM is AGPL v3 — so check each submodule's own license file before shipping.

## General guidelines for code contributors

 1. Make sure you are legally allowed to make a contribution under the zlib license.
 2. The zlib license header goes at the top of each source file, with appropriate copyright notice.
 3. This PasVulkan wrapper may be used only with the PasVulkan-own Vulkan Pascal header.
 4. After a pull request, check the status of your pull request on https://git.rosseaux.net/BeRo1985/pasvulkan
 5. Write code which's compatible with Delphi >= 2009 and FreePascal >= 3.1.1
 6. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, but if needed, make it out-ifdef-able.
 7. No use of third-party libraries/units as possible, but if needed, make it out-ifdef-able.
 8. Try to use const when possible.
 9. Make sure to comment out writeln, used while debugging.
 10. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32, x86-64, ARM, ARM64, etc.).
 11. Make sure the code runs on all platforms with Vulkan support

## Showcase videos

- For more recent showcase videos see [Youtube playlist](https://www.youtube.com/playlist?list=PLoqdQblnX8vTx3menwS15yIMAldRZzPa7)

- PasVulkan on Android 7.0 on a NVIDIA Shield K1 Tablet
  [![PasVulkan on Android 7.0 on a NVIDIA Shield K1 Tablet](https://img.youtube.com/vi/aXIaW7-rHGI/0.jpg)](https://www.youtube.com/watch?v=aXIaW7-rHGI)

- PasVulkan on a NVIDIA Geforce GTX 970 under Windows 10 Pro
  [![PasVulkan on a NVIDIA Geforce GTX 970 under Windows 10 Pro](https://img.youtube.com/vi/6nWdgry84vM/0.jpg)](https://www.youtube.com/watch?v=6nWdgry84vM)
