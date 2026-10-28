AMD FidelityFX Super Resolution 3.1 upscaler and frame generation (optical flow and frame interpolation), GPU side
only.

Taken from the FidelityFX SDK v1.1.4 (FSR 3.1.4), MIT license, see LICENSE.txt:
https://github.com/GPUOpen-LibrariesAndSDKs/FidelityFX-SDK/tree/v1.1.4

Unchanged but for one line: in fsr3upscaler/ffx_fsr3upscaler_callbacks_glsl.h, rw_luma_history is declared rgba16f
instead of rgba8. The SDK creates the luma history as R16G16B16A16_FLOAT itself, and a storage image whose format
qualifier does not match its view's format is undefined in Vulkan (the validation layers report it as
Undefined-Value-StorageImage-FormatMismatch-ImageView).

- ffx_fsr3upscaler_*_pass.glsl          sdk/src/backends/vk/shaders/fsr3upscaler/
- ffx_opticalflow_*_pass*.glsl          sdk/src/backends/vk/shaders/opticalflow/
- ffx_frameinterpolation_*pass.glsl     sdk/src/backends/vk/shaders/frameinterpolation/
- ffx_core*.h, ffx_common_types.h       sdk/include/FidelityFX/gpu/
- fsr3upscaler/*.h                      sdk/include/FidelityFX/gpu/fsr3upscaler/ (without the HLSL callbacks)
- opticalflow/*.h                       sdk/include/FidelityFX/gpu/opticalflow/ (without the HLSL callbacks)
- frameinterpolation/*.h                sdk/include/FidelityFX/gpu/frameinterpolation/ (without the HLSL callbacks)
- spd/ffx_spd.h                         sdk/include/FidelityFX/gpu/spd/
- fsr1/ffx_fsr1.h                       sdk/include/FidelityFX/gpu/fsr1/

The host side, which the SDK has in sdk/src/components/fsr3upscaler/ffx_fsr3upscaler.cpp, is ported to Pascal in
PasVulkan.Scene3D.Renderer.TemporalUpscalerBackend.FSR3.pas, and the one of sdk/src/components/opticalflow/
ffx_opticalflow.cpp and sdk/src/components/frameinterpolation/ffx_frameinterpolation.cpp, as sdk/src/components/fsr3/
ffx_fsr3.cpp ties them together, in PasVulkan.Scene3D.Renderer.FrameGenerationBackend.FSR3.pas. When updating to a
newer FSR 3.1.x, replace these files with the ones of the same places in the new SDK version and compare the host
side against those files again.
