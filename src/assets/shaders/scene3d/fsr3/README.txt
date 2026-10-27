AMD FidelityFX Super Resolution 3.1 upscaler, GPU side only.

Taken from the FidelityFX SDK v1.1.4 (FSR 3.1.4), MIT license, see LICENSE.txt:
https://github.com/GPUOpen-LibrariesAndSDKs/FidelityFX-SDK/tree/v1.1.4

Unchanged but for one line: in fsr3upscaler/ffx_fsr3upscaler_callbacks_glsl.h, rw_luma_history is declared rgba16f
instead of rgba8. The SDK creates the luma history as R16G16B16A16_FLOAT itself, and a storage image whose format
qualifier does not match its view's format is undefined in Vulkan (the validation layers report it as
Undefined-Value-StorageImage-FormatMismatch-ImageView).

- *_pass.glsl                       sdk/src/backends/vk/shaders/fsr3upscaler/
- ffx_core*.h, ffx_common_types.h   sdk/include/FidelityFX/gpu/
- fsr3upscaler/*.h                  sdk/include/FidelityFX/gpu/fsr3upscaler/ (without the HLSL callbacks)
- spd/ffx_spd.h                     sdk/include/FidelityFX/gpu/spd/
- fsr1/ffx_fsr1.h                   sdk/include/FidelityFX/gpu/fsr1/

The host side, which the SDK has in sdk/src/components/fsr3upscaler/ffx_fsr3upscaler.cpp, is ported to Pascal in
PasVulkan.Scene3D.Renderer.TemporalUpscalerBackend.FSR3.pas. When updating to a newer FSR 3.1.x, replace these files
with the ones of the same places in the new SDK version and compare the host side against that file again.
