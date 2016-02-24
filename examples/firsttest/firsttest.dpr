program firsttest;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef fpc_little_endian}
  {$define little_endian}
 {$else}
  {$ifdef fpc_big_endian}
   {$define big_endian}
  {$endif}
 {$endif}
 {$ifdef fpc_has_internal_sar}
  {$define HasSAR}
 {$endif}
 {-$pic off}
 {$define CAN_INLINE}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define little_endian}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define delphi} 
 {$undef HasSAR}
 {$define UseDIV}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
{$endif}
{$ifdef cpu386}
 {$define cpux86}
{$endif}
{$ifdef cpuamd64}
 {$define cpux86}
{$endif}
{$ifdef Win32}
 {$define Windows}
{$endif}
{$ifdef Win64}
 {$define Windows}
{$endif}
{$ifdef WinCE}
 {$define Windows}
{$endif}
{$ifdef Windows}
 {$define Win}
{$endif}
{$ifdef sdl20}
 {$define sdl}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}
{$ifndef HAS_TYPE_DOUBLE}
 {$error No double floating point precision}
{$endif}
{$ifdef fpc}
 {$define CAN_INLINE}
{$else}
 {$undef CAN_INLINE}
 {$ifdef ver180}
  {$define CAN_INLINE}
 {$else}
  {$ifdef conditionalexpressions}
   {$if compilerversion>=18}
    {$define CAN_INLINE}
   {$ifend}
  {$endif}
 {$endif}
{$endif}
{$ifdef windows}
 {$apptype console}
{$endif}

uses
  vulkan in '..\..\src\vulkan.pas';

const extensionNames:array[0..1] of PVkChar=('VK_KHR_surface'{$ifdef windows},'VK_KHR_win32_surface'{$else},'VK_KHR_xlib_surface'{$endif});

var instanceCreateInfo:TVkInstanceCreateInfo;
    inst:TVkInstance;
    phys:array[0..3] of TVkPhysicalDevice;
    physCount:TVKUInt32;
    VulkanInstanceCommands:TVulkanCommands;
    VulkanInstance:TVulkan;
begin
 if LoadVulkanLibrary then begin
  writeln('LoadVulkanLibrary was successfully . . .');

  if LoadVulkanGlobalCommands then begin

   FillChar(instanceCreateInfo,SizeOf(TVkInstanceCreateInfo),#0);
   instanceCreateInfo.sType:=VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
   instanceCreateInfo.enabledExtensionCount:=2;
   instanceCreateInfo.ppEnabledExtensionNames:=PPVkChar(pointer(@extensionNames));

   if vk.CreateInstance(@instanceCreateInfo,nil,@inst)=VK_SUCCESS then begin
    writeln('vk.CreateInstance was successfully . . .');

    if (inst<>VK_NULL_INSTANCE) and LoadVulkanInstanceCommands(vk.Commands.GetInstanceProcAddr,inst,VulkanInstanceCommands) then begin
     writeln('LoadVulkanInstanceCommands was successfully . . .');

     VulkanInstance:=TVulkan.Create(VulkanInstanceCommands);
     try

      physCount:=4;
      if VulkanInstance.EnumeratePhysicalDevices(inst,@physCount,pointer(@phys))=VK_SUCCESS then begin
       writeln('VulkanInstance.EnumeratePhysicalDevices was successfully . . .');

       writeln(physCount,' physical device(s) found . . .');
       if physCount>0 then begin

       end;

      end else begin
       writeln('VulkanInstance.EnumeratePhysicalDevices had failed . . .');
      end;

      VulkanInstance.DestroyInstance(inst,nil);
      writeln('VulkanInstance.DestroyInstance was successfully . . .');

     finally
      VulkanInstance.Free;
     end;

    end else begin
     writeln('LoadVulkanInstanceCommands had failed . . .');
    end;

   end else begin
    writeln('vk.CreateInstance had failed . . .');
   end;

  end;

 end else begin
  writeln('LoadVulkanLibrary has failed . . .');
 end;
 readln;
end.
