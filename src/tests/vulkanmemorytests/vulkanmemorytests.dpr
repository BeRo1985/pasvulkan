program vulkanmemorytests;
{$i ../../PasVulkan.inc}
{$if defined(Win32) or defined(Win64) or defined(Windows)}
 {$apptype console}
{$ifend}

uses
{$ifdef Unix}
  cthreads,
{$endif}
  SysUtils,
  Vulkan in '..\..\Vulkan.pas',
  PasVulkan.Types in '..\..\PasVulkan.Types.pas',
  PasVulkan.Framework in '..\..\PasVulkan.Framework.pas',
  UnitVulkanMemoryTestUtils in 'UnitVulkanMemoryTestUtils.pas',
  UnitVulkanMemoryCPUTests in 'UnitVulkanMemoryCPUTests.pas',
  UnitVulkanMemoryValidationTests in 'UnitVulkanMemoryValidationTests.pas';

var RunCPU,RunValidation:boolean;
begin
 try

  RunCPU:=not VulkanMemoryTestHasParameter('--validation-only');
  RunValidation:=not VulkanMemoryTestHasParameter('--cpu-only');

  if RunCPU then begin
   RunVulkanMemoryCPUTests;
  end;

  if RunValidation then begin
   RunVulkanMemoryValidationTests;
  end;

  WriteLn('[PASS] Vulkan memory test run completed');

 except
  on E:Exception do begin
   WriteLn('[FAIL] ',E.ClassName,': ',E.Message);
   Halt(1);
  end;
 end;
end.
