unit UnitOrderIndependentTransparencyBuffer;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application;

type { TOrderIndependentTransparencyBuffer }
     TOrderIndependentTransparencyBuffer=class
      private
       fVulkanBuffer:TpvVulkanBuffer;
       fVulkanBufferView:TpvVulkanBufferView;
      public

       constructor Create(const aSize:TpvInt32;const aFormat:TVkFormat;const aBufferUsage:TVkBufferUsageFlags=TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT));

       destructor Destroy; override;

      published

       property VulkanBuffer:TpvVulkanBuffer read fVulkanBuffer;

       property VulkanBufferView:TpvVulkanBufferView read fVulkanBufferView;

      end;

implementation

{ TOrderIndependentTransparencyBuffer }

constructor TOrderIndependentTransparencyBuffer.Create(const aSize:TpvInt32;const aFormat:TVkFormat;const aBufferUsage:TVkBufferUsageFlags);
begin

 inherited Create;

 fVulkanBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                       aSize,
                                       aBufferUsage,
                                       TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                       [],
                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                       0,
                                       0,
                                       0,
                                       0,
                                       0,
                                       []);

 if (aBufferUsage and (TVkBufferUsageFlags(VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT) or
                       TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT)))<>0 then begin
  fVulkanBufferView:=TpvVulkanBufferView.Create(pvApplication.VulkanDevice,
                                                fVulkanBuffer,aFormat,
                                                0,
                                                TVkDeviceSize(VK_WHOLE_SIZE));
 end else begin
  fVulkanBufferView:=nil;
 end;

end;

destructor TOrderIndependentTransparencyBuffer.Destroy;
begin
 FreeAndNil(fVulkanBufferView);
 FreeAndNil(fVulkanBuffer);
 inherited Destroy;
end;

end.