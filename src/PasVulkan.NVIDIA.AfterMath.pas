(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2020, Benjamin Rosseaux (benjamin@rosseaux.de)          *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. This PasVulkan wrapper may be used only with the PasVulkan-own Vulkan   *
 *    Pascal header.                                                          *
 * 4. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/pasvulkan                                    *
 * 5. Write code which's compatible with Delphi >= 2009 and FreePascal >=     *
 *    3.1.1                                                                   *
 * 6. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 7. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 8. Try to use const when possible.                                         *
 * 9. Make sure to comment out writeln, used while debugging.                 *
 * 10. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,    *
 *     x86-64, ARM, ARM64, etc.).                                             *
 * 11. Make sure the code runs on all platforms with Vulkan support           *
 *                                                                            *
 ******************************************************************************)
unit PasVulkan.NVIDIA.AfterMath;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$m+}

interface

uses {$if defined(Windows)}
      Windows,
     {$elseif defined(Unix)}
      BaseUnix,UnixType,dl,
     {$ifend}
     SysUtils,
     Classes,
     PasVulkan.Types;

const GFSDK_Aftermath_Version_API=$000020b;  // Version 2.11

      // Default setting
      GFSDK_Aftermath_GpuCrashDumpWatchedApiFlags_None=$0;

      // Enable GPU crash dump tracking for the DX API
      GFSDK_Aftermath_GpuCrashDumpWatchedApiFlags_DX=$1;

      // Enable GPU crash dump tracking for the Vulkan API
      GFSDK_Aftermath_GpuCrashDumpWatchedApiFlags_Vulkan=$2;

      // Default settings
      GFSDK_Aftermath_GpuCrashDumpFeatureFlags_Default=$0;

      // Defer shader debug information callbacks until an actual GPU crash
      // dump is generated and also provide shader debug information
      // for the shaders related to the crash dump only.
      // Note: using this option will increase the memory footprint of the
      // application.
      GFSDK_Aftermath_GpuCrashDumpFeatureFlags_DeferDebugInfoCallbacks=$1;

      // Predefined key for application name
      GFSDK_Aftermath_GpuCrashDumpDescriptionKey_ApplicationName=$00000001;

      // Predefined key for application version
      GFSDK_Aftermath_GpuCrashDumpDescriptionKey_ApplicationVersion=$00000002;

      // Base key for creating user-defined key-value pairs.
      // Any value >= GFSDK_Aftermath_GpuCrashDumpDescriptionKey_UserDefined
      // will create a user-defined key-value pair.
      GFSDK_Aftermath_GpuCrashDumpDescriptionKey_UserDefined=$00010000;

      GFSDK_Aftermath_Result_Success=$1;

      GFSDK_Aftermath_Result_NotAvailable=$2;

      GFSDK_Aftermath_Result_Fail=$BAD00000;

      // The callee tries to use a library version
      //  which does not match the built binary.
      GFSDK_Aftermath_Result_FAIL_VersionMismatch=GFSDK_Aftermath_Result_Fail or 1;

      // The library hasn't been initialized, see;
      //  'GFSDK_Aftermath_Initialize'.
      GFSDK_Aftermath_Result_FAIL_NotInitialized=GFSDK_Aftermath_Result_Fail or 2;

      // The callee tries to use the library with
      //  a non-supported GPU. Currently, only
      //  NVIDIA GPUs are supported.
      GFSDK_Aftermath_Result_FAIL_InvalidAdapter=GFSDK_Aftermath_Result_Fail or 3;

      // The callee passed an invalid parameter to the
      //  library, likely a null pointer or bad handle.
      GFSDK_Aftermath_Result_FAIL_InvalidParameter=GFSDK_Aftermath_Result_Fail or 4;

      // Something weird happened that caused the
      //  library to fail for some reason.
      GFSDK_Aftermath_Result_FAIL_Unknown=GFSDK_Aftermath_Result_Fail or 5;

      // Got a fail error code from the graphics API.
      GFSDK_Aftermath_Result_FAIL_ApiError=GFSDK_Aftermath_Result_Fail or 6;

      // Make sure that the NvAPI DLL is up to date.
      GFSDK_Aftermath_Result_FAIL_NvApiIncompatible=GFSDK_Aftermath_Result_Fail or 7;

      // It would appear as though a call has been
      //  made to fetch the Aftermath data for a
      //  context that hasn't been used with
      //  the EventMarker API yet.
      GFSDK_Aftermath_Result_FAIL_GettingContextDataWithNewCommandList=GFSDK_Aftermath_Result_Fail or 8;

      // Looks like the library has already been initialized.
      GFSDK_Aftermath_Result_FAIL_AlreadyInitialized=GFSDK_Aftermath_Result_Fail or 9;

      // Debug layer not compatible with Aftermath.
      GFSDK_Aftermath_Result_FAIL_D3DDebugLayerNotCompatible=GFSDK_Aftermath_Result_Fail or 10;

      // Aftermath failed to initialize in the driver.
      GFSDK_Aftermath_Result_FAIL_DriverInitFailed=GFSDK_Aftermath_Result_Fail or 11;

      // Aftermath v2.x requires driver version 387.xx and beyond
      GFSDK_Aftermath_Result_FAIL_DriverVersionNotSupported=GFSDK_Aftermath_Result_Fail or 12;

      // The system ran out of memory for allocations
      GFSDK_Aftermath_Result_FAIL_OutOfMemory=GFSDK_Aftermath_Result_Fail or 13;

      // No need to get data on bundles, as markers
      //  execute on the command list.
      GFSDK_Aftermath_Result_FAIL_GetDataOnBundle=GFSDK_Aftermath_Result_Fail or 14;

      // No need to get data on deferred contexts, as markers
      //  execute on the immediate context.
      GFSDK_Aftermath_Result_FAIL_GetDataOnDeferredContext=GFSDK_Aftermath_Result_Fail or 15;

      // This feature hasn't been enabled at initialization - see GFSDK_Aftermath_FeatureFlags.
      GFSDK_Aftermath_Result_FAIL_FeatureNotEnabled=GFSDK_Aftermath_Result_Fail or 16;

      // No resources have ever been registered.
      GFSDK_Aftermath_Result_FAIL_NoResourcesRegistered=GFSDK_Aftermath_Result_Fail or 17;

      // This resource has never been registered.
      GFSDK_Aftermath_Result_FAIL_ThisResourceNeverRegistered=GFSDK_Aftermath_Result_Fail or 18;

      // The functionality is not supported for UWP applications
      GFSDK_Aftermath_Result_FAIL_NotSupportedInUWP=GFSDK_Aftermath_Result_Fail or 19;

      // D3D DLL not compatible with Aftermath.
      GFSDK_Aftermath_Result_FAIL_D3dDllNotSupported=GFSDK_Aftermath_Result_Fail or 20;

      // D3D DLL interception is not compatible with Aftermath.
      GFSDK_Aftermath_Result_FAIL_D3dDllInterceptionNotSupported=GFSDK_Aftermath_Result_Fail or 21;

      // Aftermath is disabled on the system by the current user.
      //  On Windows, this is controlled by a Windows registry key:
      //    KeyPath   : HKEY_CURRENT_USER\Software\NVIDIA Corporation\Nsight Aftermath
      //    KeyValue  : ForceOff
      //    ValueType : REG_DWORD
      //    ValueData : Any value != 0 will force the functionality of the Aftermath
      //                SDK off on the system.
      //
      //  On Linux, this is controlled by an environment variable:
      //    Name: NV_AFTERMATH_FORCE_OFF
      //    Value: Any value != '0' will force the functionality of the Aftermath
      //                SDK off.
      //
      GFSDK_Aftermath_Result_FAIL_Disabled=GFSDK_Aftermath_Result_Fail or 22;

      GFSDK_Aftermath_Context_Status_NotStarted=0;

      // This command list has begun execution on the GPU.
      GFSDK_Aftermath_Context_Status_Executing=1;

      // This command list has finished execution on the GPU.
      GFSDK_Aftermath_Context_Status_Finished=2;

      // This context has an invalid state, which could be
      //  caused by an error.
      //
      //  NOTE: See, 'GFSDK_Aftermath_ContextData::getErrorCode()'
      //  for more information.
      GFSDK_Aftermath_Context_Status_Invalid=3;

      // The GPU is still active, and hasn't gone down.
      GFSDK_Aftermath_Device_Status_Active=0;

      // A long running shader/operation has caused a
      //  GPU timeout. Reconfiguring the timeout length
      //  might help tease out the problem.
      GFSDK_Aftermath_Device_Status_Timeout=1;

      // Run out of memory to complete operations.
      GFSDK_Aftermath_Device_Status_OutOfMemory=2;

      // An invalid VA access has caused a fault.
      GFSDK_Aftermath_Device_Status_PageFault=3;

      // The GPU has stopped executing
      GFSDK_Aftermath_Device_Status_Stopped=4;

      // The device has been reset
      GFSDK_Aftermath_Device_Status_Reset=5;

      // Unknown problem - likely using an older driver
      //  incompatible with this Aftermath feature.
      GFSDK_Aftermath_Device_Status_Unknown=6;

      // An invalid rendering call has percolated through the driver
      GFSDK_Aftermath_Device_Status_DmaFault=7;

type EGFSDK_Aftermath=class(Exception);

     TGFSDK_Aftermath_Version=UInt32;

     TGFSDK_Aftermath_GpuCrashDumpWatchedApiFlags=UInt32;

     TGFSDK_Aftermath_GpuCrashDumpFeatureFlags=UInt32;

     TGFSDK_Aftermath_GpuCrashDumpDescriptionKey=UInt32;

     TGFSDK_Aftermath_Result=UInt32;

     TGFSDK_Aftermath_Context_Status=UInt32;

     TGFSDK_Aftermath_Device_Status=UInt32;

     TPFN_GFSDK_Aftermath_AddGpuCrashDumpDescription=procedure(Key:UInt32;Value:PAnsiChar); cdecl;

     TPFN_GFSDK_Aftermath_GpuCrashDumpCb=procedure(pGpuCrashDump:Pointer;gpuCrashDumpSize:UInt32;pUserData:Pointer); cdecl;

     TPFN_GFSDK_Aftermath_ShaderDebugInfoCb=procedure(pShaderDebugInfo:Pointer;shaderDebugInfoSize:UInt32;pUserData:Pointer); cdecl;

     TPFN_GFSDK_Aftermath_GpuCrashDumpDescriptionCb=procedure(addValue:TPFN_GFSDK_Aftermath_AddGpuCrashDumpDescription;pUserData:Pointer); cdecl;

     TGFSDK_Aftermath_EnableGpuCrashDumps=function(apiVersion:TGFSDK_Aftermath_Version;
                                                   watchedApis:UInt32;
                                                   flags:UInt32;
                                                   gpuCrashDumpCb:TPFN_GFSDK_Aftermath_GpuCrashDumpCb;
                                                   shaderDebugInfoCb:TPFN_GFSDK_Aftermath_ShaderDebugInfoCb;
                                                   descriptionCb:TPFN_GFSDK_Aftermath_GpuCrashDumpDescriptionCb;
                                                   pUserData:pointer):TGFSDK_Aftermath_Result; cdecl;

     TGFSDK_Aftermath_DisableGpuCrashDumps=function:TGFSDK_Aftermath_Result; cdecl;


var GFSDK_Aftermath_EnableGpuCrashDumps:TGFSDK_Aftermath_EnableGpuCrashDumps=nil;

    GFSDK_Aftermath_DisableGpuCrashDumps:TGFSDK_Aftermath_DisableGpuCrashDumps=nil;

    GFSDK_Aftermath_LibHandle:Pointer=nil;

    GFSDK_Aftermath_Active:boolean=false;

procedure LoadNVIDIAAfterMath;
procedure FreeNVIDIAAfterMath;

procedure InitializeNVIDIAAfterMath;
procedure FinalizeNVIDIAAfterMath;

implementation

uses PasVulkan.Application;

function _LoadLibrary(const LibraryName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}
begin
{$ifdef Windows}
 result:={%H-}pointer(LoadLibrary(PChar(LibraryName)));
{$else}
{$ifdef Unix}
 result:=dlopen(PChar(LibraryName),RTLD_NOW or RTLD_LAZY);
{$else}
 result:=nil;
{$endif}
{$endif}
end;

function _FreeLibrary(LibraryHandle:pointer):boolean; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=assigned(LibraryHandle);
 if result then begin
{$ifdef Windows}
  result:=FreeLibrary({%H-}HMODULE(LibraryHandle));
{$else}
{$ifdef Unix}
  result:=dlclose(LibraryHandle)=0;
{$else}
  result:=false;
{$endif}
{$endif}
 end;
end;

function _GetProcAddress(LibraryHandle:pointer;const ProcName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}
begin
{$ifdef Windows}
 result:=GetProcAddress({%H-}HMODULE(LibraryHandle),PChar(ProcName));
{$else}
{$ifdef Unix}
 result:=dlsym(LibraryHandle,PChar(ProcName));
{$else}
 result:=nil;
{$endif}
{$endif}
end;

type TPFN_VoidFunction=procedure(); cdecl;

function _VoidFunctionToPointer(const VoidFunction:TPFN_VoidFunction):pointer; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=addr(VoidFunction);
end;

procedure LoadNVIDIAAfterMath;
begin
 if not assigned(GFSDK_Aftermath_LibHandle) then begin
  GFSDK_Aftermath_LibHandle:=_LoadLibrary(
{$if defined(Windows) and (defined(cpuamd64) or defined(cpux64) or defined(cpux86_64))}
                              'GFSDK_Aftermath_Lib.x64.dll'
{$elseif defined(Windows) and defined(cpux86)}
                              'GFSDK_Aftermath_Lib.x86.dll'
{$else}
                              'libGFSDK_Aftermath.so'
{$ifend}
                             );
  if assigned(GFSDK_Aftermath_LibHandle) then begin
   @GFSDK_Aftermath_EnableGpuCrashDumps:=_GetProcAddress(GFSDK_Aftermath_LibHandle,'GFSDK_Aftermath_EnableGpuCrashDumps');
   @GFSDK_Aftermath_DisableGpuCrashDumps:=_GetProcAddress(GFSDK_Aftermath_LibHandle,'GFSDK_Aftermath_DisableGpuCrashDumps');
  end;
 end;
end;

procedure FreeNVIDIAAfterMath;
begin
 if assigned(GFSDK_Aftermath_LibHandle) then begin
  try
   _FreeLibrary(GFSDK_Aftermath_LibHandle);
  finally
   GFSDK_Aftermath_LibHandle:=nil;
  end;
 end;
end;

procedure GPUCrashDumpCallback(pGpuCrashDump:Pointer;gpuCrashDumpSize:UInt32;pUserData:Pointer); cdecl;
begin
end;

procedure ShaderDebugInfoCallback(pShaderDebugInfo:Pointer;shaderDebugInfoSize:UInt32;pUserData:Pointer); cdecl;
begin
end;

procedure CrashDumpDescriptionCallback(addValue:TPFN_GFSDK_Aftermath_AddGpuCrashDumpDescription;pUserData:Pointer); cdecl;
begin
 if assigned(addValue) then begin
  addValue(GFSDK_Aftermath_GpuCrashDumpDescriptionKey_ApplicationName,pAnsiChar(pvApplication.Title));
  addValue(GFSDK_Aftermath_GpuCrashDumpDescriptionKey_ApplicationVersion,'1.0');
  addValue(GFSDK_Aftermath_GpuCrashDumpDescriptionKey_UserDefined,'GPU crash dump');
 end;
end;

procedure AFTERMATH_CHECK_ERROR(const aResult:TGFSDK_Aftermath_Result);
begin
 if (aResult and UInt32($fff00000))=GFSDK_Aftermath_Result_Fail then begin
  case aResult of
   GFSDK_Aftermath_Result_FAIL_VersionMismatch:begin
    raise EGFSDK_Aftermath.Create('Version match');
   end;
   GFSDK_Aftermath_Result_FAIL_NotInitialized:begin
    raise EGFSDK_Aftermath.Create('Not initialized');
   end;
   GFSDK_Aftermath_Result_FAIL_InvalidAdapter:begin
    raise EGFSDK_Aftermath.Create('Invalid adapter');
   end;
   GFSDK_Aftermath_Result_FAIL_InvalidParameter:begin
    raise EGFSDK_Aftermath.Create('Invalid parameter');
   end;
   GFSDK_Aftermath_Result_FAIL_Unknown:begin
    raise EGFSDK_Aftermath.Create('Unknown');
   end;
   GFSDK_Aftermath_Result_FAIL_ApiError:begin
    raise EGFSDK_Aftermath.Create('API error');
   end;
   GFSDK_Aftermath_Result_FAIL_NvApiIncompatible:begin
    raise EGFSDK_Aftermath.Create('NvAPI incompstible');
   end;
   GFSDK_Aftermath_Result_FAIL_GettingContextDataWithNewCommandList:begin
    raise EGFSDK_Aftermath.Create('Getting context data with new command list');
   end;
   GFSDK_Aftermath_Result_FAIL_AlreadyInitialized:begin
    raise EGFSDK_Aftermath.Create('Already initialized');
   end;
   GFSDK_Aftermath_Result_FAIL_D3DDebugLayerNotCompatible:begin
    raise EGFSDK_Aftermath.Create('D3D debug layer not compatible');
   end;
   GFSDK_Aftermath_Result_FAIL_DriverInitFailed:begin
    raise EGFSDK_Aftermath.Create('Driver init failed');
   end;
   GFSDK_Aftermath_Result_FAIL_DriverVersionNotSupported:begin
    raise EGFSDK_Aftermath.Create('Driver version not supported');
   end;
   GFSDK_Aftermath_Result_FAIL_OutOfMemory:begin
    raise EGFSDK_Aftermath.Create('Out of memory');
   end;
   GFSDK_Aftermath_Result_FAIL_GetDataOnBundle:begin
    raise EGFSDK_Aftermath.Create('Get data on bundle');
   end;
   GFSDK_Aftermath_Result_FAIL_GetDataOnDeferredContext:begin
    raise EGFSDK_Aftermath.Create('Get data on deferred context');
   end;
   GFSDK_Aftermath_Result_FAIL_FeatureNotEnabled:begin
    raise EGFSDK_Aftermath.Create('Feature not enabled');
   end;
   GFSDK_Aftermath_Result_FAIL_NoResourcesRegistered:begin
    raise EGFSDK_Aftermath.Create('No resources registered');
   end;
   GFSDK_Aftermath_Result_FAIL_ThisResourceNeverRegistered:begin
    raise EGFSDK_Aftermath.Create('This resource never registered');
   end;
   GFSDK_Aftermath_Result_FAIL_NotSupportedInUWP:begin
    raise EGFSDK_Aftermath.Create('Not supported in UWP');
   end;
   GFSDK_Aftermath_Result_FAIL_D3dDllNotSupported:begin
    raise EGFSDK_Aftermath.Create('D3D DLL not supported');
   end;
   GFSDK_Aftermath_Result_FAIL_D3dDllInterceptionNotSupported:begin
    raise EGFSDK_Aftermath.Create('D3D DLL interception not supported');
   end;
   GFSDK_Aftermath_Result_FAIL_Disabled:begin
    raise EGFSDK_Aftermath.Create('Disabled');
   end;
  end;
 end;
end;

procedure InitializeNVIDIAAfterMath;
begin
 if not GFSDK_Aftermath_Active then begin
  LoadNVIDIAAfterMath;
  if assigned(@GFSDK_Aftermath_EnableGpuCrashDumps) then begin
   AFTERMATH_CHECK_ERROR(
     GFSDK_Aftermath_EnableGpuCrashDumps(
      GFSDK_Aftermath_Version_API,
      GFSDK_Aftermath_GpuCrashDumpWatchedApiFlags_Vulkan,
      GFSDK_Aftermath_GpuCrashDumpFeatureFlags_DeferDebugInfoCallbacks, // Let the Nsight Aftermath library cache shader debug information.
      GPUCrashDumpCallback,                                             // Register callback for GPU crash dumps.
      ShaderDebugInfoCallback,                                          // Register callback for shader debug information.
      CrashDumpDescriptionCallback,                                     // Register callback for GPU crash dump description.
      nil
     )
   );
   GFSDK_Aftermath_Active:=true;
  end;
 end;
end;

procedure FinalizeNVIDIAAfterMath;
begin
 if GFSDK_Aftermath_Active and assigned(@GFSDK_Aftermath_DisableGpuCrashDumps) then begin
  AFTERMATH_CHECK_ERROR(GFSDK_Aftermath_DisableGpuCrashDumps);
  GFSDK_Aftermath_Active:=false;
 end;
end;

initialization
finalization
 FreeNVIDIAAfterMath;
end.

