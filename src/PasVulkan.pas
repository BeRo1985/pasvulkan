(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                        Version 2016-05-24-15-17-0000                       *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016, Benjamin Rosseaux (benjamin@rosseaux.de)               *
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
 * 3. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/pasvulkan                                    *
 * 4. Write code, which is compatible with Delphi 7-XE7 and FreePascal >= 3.0 *
 *    so don't use generics/templates, operator overloading and another newer *
 *    syntax features than Delphi 7 has support for that, but if needed, make *
 *    it out-ifdef-able.                                                      *
 * 5. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 6. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 7. Try to use const when possible.                                         *
 * 8. Make sure to comment out writeln, used while debugging.                 *
 * 9. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,     *
 *    x86-64, ARM, ARM64, etc.).                                              *
 * 10. Make sure the code runs on all platforms with Vulkan support           *
 *                                                                            *
 ******************************************************************************)
unit PasVulkan;
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

interface

uses SysUtils,Classes,Vulkan;

type EVulkanException=class(Exception);

     EVulkanErrorException=class(EVulkanException);

     TVulkanObject=class(TObject);

     TVulkanCharString=AnsiString;

     TVulkanCharStringArray=array of TVulkanCharString;
     TPVkCharArray=array of PVkChar;
     TVkPhysicalDeviceArray=array of TVkPhysicalDevice;
     TVkQueueFamilyPropertiesArray=array of TVkQueueFamilyProperties;
     TVkSparseImageFormatPropertiesArray=array of TVkSparseImageFormatProperties;
     TVkSurfaceFormatArray=array of TVkSurfaceFormatKHR;
     TVkPresentModeArray=array of TVkPresentModeKHR;
     TVkDisplayPropertiesArray=array of TVkDisplayPropertiesKHR;
     TVkDisplayPlanePropertiesArray=array of TVkDisplayPlanePropertiesKHR;
     TVkDisplayArray=array of TVkDisplayKHR;
     TVkDisplayModePropertiesArray=array of TVkDisplayModePropertiesKHR;
     TVkImageArray=array of TVkImage;
     TVkCommandBufferArray=array of TVkCommandBuffer;
     TVkDescriptorSetLayoutBindingArray=array of TVkDescriptorSetLayoutBinding;
     TVkDescriptorSetLayoutArray=array of TVkDescriptorSetLayout;
     TVkPushConstantRangeArray=array of TVkPushConstantRange;
     TVkPipelineShaderStageCreateInfoArray=array of TVkPipelineShaderStageCreateInfo;
     TVkPipelineVertexInputStateCreateInfoArray=array of TVkPipelineVertexInputStateCreateInfo;
     TVkAttachmentDescriptionArray=array of TVkAttachmentDescription;
     TVkSubpassDescriptionArray=array of TVkSubpassDescription;
     TVkSubpassDependencyArray=array of TVkSubpassDependency;
     TVkAttachmentReferenceArray=array of TVkAttachmentReference;
     TVkMemoryBarrierArray=array of TVkMemoryBarrier;
     TVkBufferMemoryBarrierArray=array of TVkBufferMemoryBarrier;
     TVkImageMemoryBarrierArray=array of TVkImageMemoryBarrier;
     TVkViewportArray=array of TVkViewport;
     TVkRect2DArray=array of TVkRect2D;
     TVkSampleMaskArray=array of TVkSampleMask;
     TVkVertexInputBindingDescriptionArray=array of TVkVertexInputBindingDescription;
     TVkVertexInputAttributeDescriptionArray=array of TVkVertexInputAttributeDescription;
     TVkPipelineColorBlendAttachmentStateArray=array of TVkPipelineColorBlendAttachmentState;
     TVkDynamicStateArray=array of TVkDynamicState;
     TVkDescriptorPoolSizeArray=array of TVkDescriptorPoolSize;
     TVkDescriptorSetArray=array of TVkDescriptorSet;
     TVkDescriptorImageInfoArray=array of TVkDescriptorImageInfo;
     TVkDescriptorBufferInfoArray=array of TVkDescriptorBufferInfo;
     TVkClearValueArray=array of TVkClearValue;
     TVkResultArray=array of TVkResult;

     TVulkanBaseList=class
      private
       fItemSize:TVkSizeInt;
       fCount:TVkSizeInt;
       fAllocated:TVkSizeInt;
       fMemory:pointer;
       procedure SetCount(const NewCount:TVkSizeInt);
       function GetItem(const Index:TVkSizeInt):pointer;
      protected
       procedure InitializeItem(var Item); virtual;
       procedure FinalizeItem(var Item); virtual;
       procedure CopyItem(const Source;var Destination); virtual;
       procedure ExchangeItem(var Source,Destination); virtual;
       function CompareItem(const Source,Destination):longint; virtual;
      public
       constructor Create(const pItemSize:TVkSizeInt);
       destructor Destroy; override;
       procedure Clear; virtual;
       procedure FillWith(const SourceData;const SourceCount:TVkSizeInt); virtual;
       function Add(const Item):TVkSizeInt;
       function Find(const Item):TVkSizeInt;
       procedure Insert(const Index:TVkSizeInt;const Item);
       procedure Delete(const Index:TVkSizeInt);
       procedure Remove(const Item);
       procedure Exchange(const Index,WithIndex:TVkSizeInt);
       property Count:TVkSizeInt read fCount write SetCount;
       property Allocated:TVkSizeInt read fAllocated;
       property Memory:pointer read fMemory;
       property ItemPointers[const Index:TVkSizeInt]:pointer read GetItem; default;
     end;

     TVulkanObjectList=class(TVulkanBaseList)
      private
       function GetItem(const Index:TVkSizeInt):TVulkanObject;
       procedure SetItem(const Index:TVkSizeInt;const Item:TVulkanObject);
      protected
       procedure InitializeItem(var Item); override;
       procedure FinalizeItem(var Item); override;
       procedure CopyItem(const Source;var Destination); override;
       procedure ExchangeItem(var Source,Destination); override;
       function CompareItem(const Source,Destination):longint; override;
      public
       constructor Create;
       destructor Destroy; override;
       function Add(const Item:TVulkanObject):TVkSizeInt; reintroduce;
       function Find(const Item:TVulkanObject):TVkSizeInt; reintroduce;
       procedure Insert(const Index:TVkSizeInt;const Item:TVulkanObject); reintroduce;
       procedure Remove(const Item:TVulkanObject); reintroduce;
       property Items[const Index:TVkSizeInt]:TVulkanObject read GetItem write SetItem; default;
     end;

     TVkUInt32List=class(TVulkanBaseList)
      private
       function GetItem(const Index:TVkSizeInt):TVkUInt32;
       procedure SetItem(const Index:TVkSizeInt;const Item:TVkUInt32);
      protected
       procedure InitializeItem(var Item); override;
       procedure FinalizeItem(var Item); override;
       procedure CopyItem(const Source;var Destination); override;
       procedure ExchangeItem(var Source,Destination); override;
       function CompareItem(const Source,Destination):longint; override;
      public
       constructor Create;
       destructor Destroy; override;
       function Add(const Item:TVkUInt32):TVkSizeInt; reintroduce;
       function Find(const Item:TVkUInt32):TVkSizeInt; reintroduce;
       procedure Insert(const Index:TVkSizeInt;const Item:TVkUInt32); reintroduce;
       procedure Remove(const Item:TVkUInt32); reintroduce;
       property Items[const Index:TVkSizeInt]:TVkUInt32 read GetItem write SetItem; default;
     end;

     TVkFloatList=class(TVulkanBaseList)
      private
       function GetItem(const Index:TVkSizeInt):TVkFloat;
       procedure SetItem(const Index:TVkSizeInt;const Item:TVkFloat);
      protected
       procedure InitializeItem(var Item); override;
       procedure FinalizeItem(var Item); override;
       procedure CopyItem(const Source;var Destination); override;
       procedure ExchangeItem(var Source,Destination); override;
       function CompareItem(const Source,Destination):longint; override;
      public
       constructor Create;
       destructor Destroy; override;
       function Add(const Item:TVkFloat):TVkSizeInt; reintroduce;
       function Find(const Item:TVkFloat):TVkSizeInt; reintroduce;
       procedure Insert(const Index:TVkSizeInt;const Item:TVkFloat); reintroduce;
       procedure Remove(const Item:TVkFloat); reintroduce;
       property Items[const Index:TVkSizeInt]:TVkFloat read GetItem write SetItem; default;
     end;

     TVkImageViewList=class(TVulkanBaseList)
      private
       function GetItem(const Index:TVkSizeInt):TVkImageView;
       procedure SetItem(const Index:TVkSizeInt;const Item:TVkImageView);
      protected
       procedure InitializeItem(var Item); override;
       procedure FinalizeItem(var Item); override;
       procedure CopyItem(const Source;var Destination); override;
       procedure ExchangeItem(var Source,Destination); override;
       function CompareItem(const Source,Destination):longint; override;
      public
       constructor Create;
       destructor Destroy; override;
       function Add(const Item:TVkImageView):TVkSizeInt; reintroduce;
       function Find(const Item:TVkImageView):TVkSizeInt; reintroduce;
       procedure Insert(const Index:TVkSizeInt;const Item:TVkImageView); reintroduce;
       procedure Remove(const Item:TVkImageView); reintroduce;
       property Items[const Index:TVkSizeInt]:TVkImageView read GetItem write SetItem; default;
     end;

     TVkSamplerList=class(TVulkanBaseList)
      private
       function GetItem(const Index:TVkSizeInt):TVkSampler;
       procedure SetItem(const Index:TVkSizeInt;const Item:TVkSampler);
      protected
       procedure InitializeItem(var Item); override;
       procedure FinalizeItem(var Item); override;
       procedure CopyItem(const Source;var Destination); override;
       procedure ExchangeItem(var Source,Destination); override;
       function CompareItem(const Source,Destination):longint; override;
      public
       constructor Create;
       destructor Destroy; override;
       function Add(const Item:TVkSampler):TVkSizeInt; reintroduce;
       function Find(const Item:TVkSampler):TVkSizeInt; reintroduce;
       procedure Insert(const Index:TVkSizeInt;const Item:TVkSampler); reintroduce;
       procedure Remove(const Item:TVkSampler); reintroduce;
       property Items[const Index:TVkSizeInt]:TVkSampler read GetItem write SetItem; default;
     end;

     TVkDescriptorSetLayoutList=class(TVulkanBaseList)
      private
       function GetItem(const Index:TVkSizeInt):TVkDescriptorSetLayout;
       procedure SetItem(const Index:TVkSizeInt;const Item:TVkDescriptorSetLayout);
      protected
       procedure InitializeItem(var Item); override;
       procedure FinalizeItem(var Item); override;
       procedure CopyItem(const Source;var Destination); override;
       procedure ExchangeItem(var Source,Destination); override;
       function CompareItem(const Source,Destination):longint; override;
      public
       constructor Create;
       destructor Destroy; override;
       function Add(const Item:TVkDescriptorSetLayout):TVkSizeInt; reintroduce;
       function Find(const Item:TVkDescriptorSetLayout):TVkSizeInt; reintroduce;
       procedure Insert(const Index:TVkSizeInt;const Item:TVkDescriptorSetLayout); reintroduce;
       procedure Remove(const Item:TVkDescriptorSetLayout); reintroduce;
       property Items[const Index:TVkSizeInt]:TVkDescriptorSetLayout read GetItem write SetItem; default;
     end;

     TVkSampleMaskList=class(TVulkanBaseList)
      private
       function GetItem(const Index:TVkSizeInt):TVkSampleMask;
       procedure SetItem(const Index:TVkSizeInt;const Item:TVkSampleMask);
      protected
       procedure InitializeItem(var Item); override;
       procedure FinalizeItem(var Item); override;
       procedure CopyItem(const Source;var Destination); override;
       procedure ExchangeItem(var Source,Destination); override;
       function CompareItem(const Source,Destination):longint; override;
      public
       constructor Create;
       destructor Destroy; override;
       function Add(const Item:TVkSampleMask):TVkSizeInt; reintroduce;
       function Find(const Item:TVkSampleMask):TVkSizeInt; reintroduce;
       procedure Insert(const Index:TVkSizeInt;const Item:TVkSampleMask); reintroduce;
       procedure Remove(const Item:TVkSampleMask); reintroduce;
       property Items[const Index:TVkSizeInt]:TVkSampleMask read GetItem write SetItem; default;
     end;

     TVkDynamicStateList=class(TVulkanBaseList)
      private
       function GetItem(const Index:TVkSizeInt):TVkDynamicState;
       procedure SetItem(const Index:TVkSizeInt;const Item:TVkDynamicState);
      protected
       procedure InitializeItem(var Item); override;
       procedure FinalizeItem(var Item); override;
       procedure CopyItem(const Source;var Destination); override;
       procedure ExchangeItem(var Source,Destination); override;
       function CompareItem(const Source,Destination):longint; override;
      public
       constructor Create;
       destructor Destroy; override;
       function Add(const Item:TVkDynamicState):TVkSizeInt; reintroduce;
       function Find(const Item:TVkDynamicState):TVkSizeInt; reintroduce;
       procedure Insert(const Index:TVkSizeInt;const Item:TVkDynamicState); reintroduce;
       procedure Remove(const Item:TVkDynamicState); reintroduce;
       property Items[const Index:TVkSizeInt]:TVkDynamicState read GetItem write SetItem; default;
     end;

     TVkBufferViewList=class(TVulkanBaseList)
      private
       function GetItem(const Index:TVkSizeInt):TVkBufferView;
       procedure SetItem(const Index:TVkSizeInt;const Item:TVkBufferView);
      protected
       procedure InitializeItem(var Item); override;
       procedure FinalizeItem(var Item); override;
       procedure CopyItem(const Source;var Destination); override;
       procedure ExchangeItem(var Source,Destination); override;
       function CompareItem(const Source,Destination):longint; override;
      public
       constructor Create;
       destructor Destroy; override;
       function Add(const Item:TVkBufferView):TVkSizeInt; reintroduce;
       function Find(const Item:TVkBufferView):TVkSizeInt; reintroduce;
       procedure Insert(const Index:TVkSizeInt;const Item:TVkBufferView); reintroduce;
       procedure Remove(const Item:TVkBufferView); reintroduce;
       property Items[const Index:TVkSizeInt]:TVkBufferView read GetItem write SetItem; default;
     end;

     TVkClearValueList=class(TVulkanBaseList)
      private
       function GetItem(const Index:TVkSizeInt):TVkClearValue;
       procedure SetItem(const Index:TVkSizeInt;const Item:TVkClearValue);
      protected
       procedure InitializeItem(var Item); override;
       procedure FinalizeItem(var Item); override;
       procedure CopyItem(const Source;var Destination); override;
       procedure ExchangeItem(var Source,Destination); override;
       function CompareItem(const Source,Destination):longint; override;
      public
       constructor Create;
       destructor Destroy; override;
       function Add(const Item:TVkClearValue):TVkSizeInt; reintroduce;
       function Find(const Item:TVkClearValue):TVkSizeInt; reintroduce;
       procedure Insert(const Index:TVkSizeInt;const Item:TVkClearValue); reintroduce;
       procedure Remove(const Item:TVkClearValue); reintroduce;
       property Items[const Index:TVkSizeInt]:TVkClearValue read GetItem write SetItem; default;
     end;

     TVulkanHandle=class(TVulkanObject)
      private
       fOwnsHandle:boolean;
      protected
      public
       constructor Create;
       destructor Destroy; override;
      published
       property OwnsHandle:boolean read fOwnsHandle write fOwnsHandle;
     end;

     TVulkanAllocationHandle=class(TVulkanHandle)
      private
       fAllocationCallbacks:PVkAllocationCallbacks;
      protected
       procedure SetAllocationCallbacks(const NewAllocationCallbacks:PVkAllocationCallbacks);
       property AllocationCallbacks:PVkAllocationCallbacks read fAllocationCallbacks write SetAllocationCallbacks;
      public
       constructor Create;
       destructor Destroy; override;
     end;

     TVulkanAllocationManager=class(TVulkanObject)
      private
       fAllocationCallbacks:TVkAllocationCallbacks;
      protected
       function AllocationCallback(const Size:TVkSize;const Alignment:TVkSize;const Scope:TVkSystemAllocationScope):PVkVoid; virtual;
       function ReallocationCallback(const Original:PVkVoid;const Size:TVkSize;const Alignment:TVkSize;const Scope:TVkSystemAllocationScope):PVkVoid; virtual;
       procedure FreeCallback(const Memory:PVkVoid); virtual;
       procedure InternalAllocationCallback(const Size:TVkSize;const Type_:TVkInternalAllocationType;const Scope:TVkSystemAllocationScope);
       procedure InternalFreeCallback(const Size:TVkSize;const Type_:TVkInternalAllocationType;const Scope:TVkSystemAllocationScope);
      public
       constructor Create;
       destructor Destroy; override;
       property AllocationCallbacks:TVkAllocationCallbacks read fAllocationCallbacks;
     end;

     TVulkanApplicationInfo=class(TVulkanObject)
      private
       fApplicationInfo:TVkApplicationInfo;
       fApplicationName:TVulkanCharString;
       fEngineName:TVulkanCharString;
       procedure SetApplicationInfo(const NewApplicationInfo:TVkApplicationInfo);
       function GetApplicationName:TVulkanCharString;
       procedure SetApplicationName(const NewApplicationName:TVulkanCharString);
       function GetApplicationVersion:TVkUInt32;
       procedure SetApplicationVersion(const NewApplicationVersion:TVkUInt32);
       function GetEngineName:TVulkanCharString;
       procedure SetEngineName(const NewEngineName:TVulkanCharString);
       function GetEngineVersion:TVkUInt32;
       procedure SetEngineVersion(const NewEngineVersion:TVkUInt32);
       function GetAPIVersion:TVkUInt32;
       procedure SetAPIVersion(const NewAPIVersion:TVkUInt32);
      public
       constructor Create(const pApplicationName:TVulkanCharString='Vulkan application';
                          const pApplicationVersion:TVkUInt32=1;
                          const pEngineName:TVulkanCharString='Vulkan engine';
                          const pEngineVersion:TVkUInt32=1;
                          const pAPIVersion:TVkUInt32=VK_API_VERSION_1_0);
       destructor Destroy; override;
       property ApplicationInfo:TVkApplicationInfo read fApplicationInfo write SetApplicationInfo;
      published
       property ApplicationName:TVulkanCharString read GetApplicationName write SetApplicationName;
       property ApplicationVersion:TVkUInt32 read GetApplicationVersion write SetApplicationVersion;
       property EngineName:TVulkanCharString read GetEngineName write SetEngineName;
       property EngineVersion:TVkUInt32 read GetEngineVersion write SetEngineVersion;
       property APIVersion:TVkUInt32 read GetAPIVersion write SetAPIVersion;
     end;

     TVulkanInstance=class(TVulkanObject)
      private
       fVulkan:TVulkan;
       fApplicationInfo:TVulkanApplicationInfo;
       fInstanceCreateInfo:TVkInstanceCreateInfo;
       fEnabledLayerNames:array of TVulkanCharString;
       fEnabledExtensionNames:array of TVulkanCharString;
       fRawEnabledLayerNames:array of PVkChar;
       fRawEnabledExtensionNames:array of PVkChar;
       fAllocationManager:TVulkanAllocationManager;
       fAllocationCallbacks:PVkAllocationCallbacks;
       fInstance:TVkInstance;
      public
       constructor Create(const pVulkan:TVulkan;
                          const pFlags:TVkInstanceCreateFlags;
                          const pApplicationInfo:TVulkanApplicationInfo;
                          const pEnabledLayerNames:array of TVulkanCharString;
                          const pEnabledExtensionNames:array of TVulkanCharString;
                          const pAllocationManager:TVulkanAllocationManager);
       destructor Destroy; override;
       property Instance:TVkInstance read fInstance;
     end;

{
     TVulkanResource=class
      private
       fDevice:TVkDevice;
       fOwnsResource:boolean;
      public
       constructor Create; reintroduce; virtual;
       destructor Destroy; override;
       procedure Clear; virtual;
       property Device:TVkDevice read fDevice write fDevice;
       property OwnsResource:boolean read fOwnsResource write fOwnsResource;
     end;

     PVulkanDeviceSurfaceFormat=^TVulkanDeviceSurfaceFormat;
     TVulkanDeviceSurfaceFormat=TVkSurfaceFormatKHR;

     TVulkanDeviceSurfaceFormats=array of TVulkanDeviceSurfaceFormat;

     TVulkanDevice=class
      private
       fDevice:TVkDevice;
       fPhysicalDevice:TVkPhysicalDevice;
      public
       constructor Create(const pDevice:TVkDevice;const pPhysicalDevice:TVkPhysicalDevice);
       destructor Destroy; override;
       function GetMemoryType(const pTypeBits:TVkUInt32;const pProperties:TVkFlags):TVkUInt32;
       function GetBestSupportedDepthFormat:TVkFormat;
       function GetGraphicsQueueNodeIndex(const pSurface:TVkSurfaceKHR):TVkUInt32;
       function GetSurfaceFormats(const pSurface:TVkSurfaceKHR):TVulkanDeviceSurfaceFormats;
       function GetSurfaceFormat(const pSurface:TVkSurfaceKHR):TVulkanDeviceSurfaceFormat;
       procedure WaitIdle;
       property Device:TVkDevice read fDevice write fDevice;
       property PhysicalDevice:TVkPhysicalDevice read fPhysicalDevice write fPhysicalDevice;
     end;

     TVulkanInstance=class
      private
       fDevice:TVkDevice;
       fPhysicalDevice:TVkPhysicalDevice;
       fQueue:TVkQueue;
       fInstance:TVkInstance;
       fEnableValidation:boolean;
      public
       constructor Create(const pEnableValidation:boolean);
       destructor Destroy; override;
       property Device:TVkDevice read fDevice write fDevice;
       property PhysicalDevice:TVkPhysicalDevice read fPhysicalDevice write fPhysicalDevice;
       property Queue:TVkDevice read fQueue write fQueue;
       property Instance:TVkDevice read fInstance write fInstance;
       property EnableValidation:boolean read fEnableValidation;
     end;
 }
function VulkanRoundUpToPowerOfTwo(Value:TVkSize):TVkSize;

function VulkanErrorToString(const ErrorCode:TVkResult):TVulkanCharString;

function StringListToVulkanCharStringArray(const StringList:TStringList):TVulkanCharStringArray;

implementation

function VulkanRoundUpToPowerOfTwo(Value:TVkSize):TVkSize;
begin
 dec(Value);
 Value:=Value or (Value shr 1);
 Value:=Value or (Value shr 2);
 Value:=Value or (Value shr 4);
 Value:=Value or (Value shr 8);
 Value:=Value or (Value shr 16);
{$ifdef CPU64}
 Value:=Value or (Value shr 32);
{$endif}
 result:=Value+1;
end;

function VulkanErrorToString(const ErrorCode:TVkResult):TVulkanCharString;
begin
 case ErrorCode of
  VK_SUCCESS:begin
   result:='VK_SUCCESS';
  end;
  VK_NOT_READY:begin
   result:='VK_NOT_READY';
  end;
  VK_TIMEOUT:begin
   result:='VK_TIMEOUT';
  end;
  VK_EVENT_SET:begin
   result:='VK_EVENT_SET';
  end;
  VK_EVENT_RESET:begin
   result:='VK_EVENT_RESET';
  end;
  VK_INCOMPLETE:begin
   result:='VK_INCOMPLETE';
  end;
  VK_ERROR_OUT_OF_HOST_MEMORY:begin
   result:='VK_ERROR_OUT_OF_HOST_MEMORY';
  end;
  VK_ERROR_OUT_OF_DEVICE_MEMORY:begin
   result:='VK_ERROR_OUT_OF_DEVICE_MEMORY';
  end;
  VK_ERROR_INITIALIZATION_FAILED:begin
   result:='VK_ERROR_INITIALIZATION_FAILED';
  end;
  VK_ERROR_DEVICE_LOST:begin
   result:='VK_ERROR_DEVICE_LOST';
  end;
  VK_ERROR_MEMORY_MAP_FAILED:begin
   result:='VK_ERROR_MEMORY_MAP_FAILED';
  end;
  VK_ERROR_LAYER_NOT_PRESENT:begin
   result:='VK_ERROR_LAYER_NOT_PRESENT';
  end;
  VK_ERROR_EXTENSION_NOT_PRESENT:begin
   result:='VK_ERROR_EXTENSION_NOT_PRESENT';
  end;
  VK_ERROR_FEATURE_NOT_PRESENT:begin
   result:='VK_ERROR_FEATURE_NOT_PRESENT';
  end;
  VK_ERROR_INCOMPATIBLE_DRIVER:begin
   result:='VK_ERROR_INCOMPATIBLE_DRIVER';
  end;
  VK_ERROR_TOO_MANY_OBJECTS:begin
   result:='VK_ERROR_TOO_MANY_OBJECTS';
  end;
  VK_ERROR_FORMAT_NOT_SUPPORTED:begin
   result:='VK_ERROR_FORMAT_NOT_SUPPORTED';
  end;
  VK_ERROR_SURFACE_LOST_KHR:begin
   result:='VK_ERROR_SURFACE_LOST_KHR';
  end;
  VK_ERROR_NATIVE_WINDOW_IN_USE_KHR:begin
   result:='VK_ERROR_NATIVE_WINDOW_IN_USE_KHR';
  end;
  VK_SUBOPTIMAL_KHR:begin
   result:='VK_SUBOPTIMAL_KHR';
  end;
  VK_ERROR_OUT_OF_DATE_KHR:begin
   result:='VK_ERROR_OUT_OF_DATE_KHR';
  end;
  VK_ERROR_INCOMPATIBLE_DISPLAY_KHR:begin
   result:='VK_ERROR_INCOMPATIBLE_DISPLAY_KHR';
  end;
  VK_ERROR_VALIDATION_FAILED_EXT:begin
   result:='VK_ERROR_VALIDATION_FAILED_EXT';
  end;
  VK_ERROR_INVALID_SHADER_NV:begin
   result:='VK_ERROR_INVALID_SHADER_NV';
  end;
  else begin
   result:='Unknown error code detected ('+IntToStr(longint(ErrorCode))+')';
  end;
 end;
end;

function StringListToVulkanCharStringArray(const StringList:TStringList):TVulkanCharStringArray;
var i:TVkInt32;
begin
 result:=nil;
 SetLength(result,StringList.Count);
 for i:=0 to StringList.Count-1 do begin
  result[i]:=StringList.Strings[i];
 end;
end;

procedure HandleResultCode(const ResultCode:TVkResult);
begin
 if ResultCode<>VK_SUCCESS then begin
  raise EVulkanException.Create(VulkanErrorToString(ResultCode));
 end;
end;

constructor TVulkanBaseList.Create(const pItemSize:TVkSizeInt);
begin
 inherited Create;
 fItemSize:=pItemSize;
 fCount:=0;
 fAllocated:=0;
 fMemory:=nil;
end;

destructor TVulkanBaseList.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TVulkanBaseList.SetCount(const NewCount:TVkSizeInt);
var Index,NewAllocated:TVkSizeInt;
    Item:pointer;
begin
 if fCount<NewCount then begin
  NewAllocated:=TVkSizeInt(VulkanRoundUpToPowerOfTwo(NewCount))*fItemSize;
  if fAllocated<NewAllocated then begin
   if assigned(fMemory) then begin
    ReallocMem(fMemory,NewAllocated);
   end else begin
    GetMem(fMemory,NewAllocated);
   end;
   FillChar(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(fAllocated*fItemSize)))^,(NewAllocated-fAllocated)*fItemSize,#0);
   fAllocated:=NewAllocated;
  end;
  Index:=fCount;
  inc(TVkPtrUInt(Item),fItemSize*Index);
  while Index<NewCount do begin
   FillChar(Item^,fItemSize,#0);
   InitializeItem(Item^);
   inc(TVkPtrUInt(Item),fItemSize);
   inc(Index);
  end;
  fCount:=NewCount;
 end else if fCount>NewCount then begin
  Item:=fMemory;
  Index:=NewCount;
  inc(TVkPtrUInt(Item),fItemSize*Index);
  while Index<fCount do begin
   FinalizeItem(Item^);
   FillChar(Item^,fItemSize,#0);
   inc(TVkPtrUInt(Item),fItemSize);
   inc(Index);
  end;
  fCount:=NewCount;
  if NewCount<(fAllocated shr 2) then begin
   if NewCount=0 then begin
    if assigned(fMemory) then begin
     FreeMem(fMemory);
     fMemory:=nil;
    end;
    fAllocated:=0;
   end else begin                             
    NewAllocated:=(fAllocated shr 1)*fItemSize;
    if assigned(fMemory) then begin
     ReallocMem(fMemory,NewAllocated);
    end else begin
     GetMem(fMemory,NewAllocated);
    end;
    fAllocated:=NewAllocated;
   end;
  end;
 end;
end;

function TVulkanBaseList.GetItem(const Index:TVkSizeInt):pointer;
begin
 if (Index>=0) and (Index<fCount) then begin
  result:=pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)));
 end else begin
  result:=nil;
 end;
end;

procedure TVulkanBaseList.InitializeItem(var Item);
begin
end;

procedure TVulkanBaseList.FinalizeItem(var Item);
begin
end;

procedure TVulkanBaseList.CopyItem(const Source;var Destination);
begin
 Move(Source,Destination,fItemSize);
end;

procedure TVulkanBaseList.ExchangeItem(var Source,Destination);
var a,b:PVkUInt8;
    c8:TVkUInt8;
    c32:TVkUInt32;
    Index:TVkInt32;
begin
 a:=@Source;
 b:=@Destination;
 for Index:=1 to fItemSize shr 2 do begin
  c32:=PVkUInt32(a)^;
  PVkUInt32(a)^:=PVkUInt32(b)^;
  PVkUInt32(b)^:=c32;
  inc(PVkUInt32(a));
  inc(PVkUInt32(b));
 end;
 for Index:=1 to fItemSize and 3 do begin
  c8:=a^;
  a^:=b^;
  b^:=c8;
  inc(a);
  inc(b);
 end;
end;

function TVulkanBaseList.CompareItem(const Source,Destination):longint;
var a,b:PVkUInt8;
    Index:TVkInt32;
begin
 result:=0;
 a:=@Source;
 b:=@Destination;
 for Index:=1 to fItemSize do begin
  result:=a^-b^;
  if result<>0 then begin
   exit;
  end;
  inc(a);
  inc(b);
 end;
end;

procedure TVulkanBaseList.Clear;
var Index:TVkSizeInt;
    Item:pointer;
begin
 Item:=fMemory;
 Index:=0;
 while Index<fCount do begin
  FinalizeItem(Item^);
  inc(TVkPtrInt(Item),fItemSize);
  inc(Index);
 end;
 if assigned(fMemory) then begin
  FreeMem(fMemory);
  fMemory:=nil;
 end;
 fCount:=0;
 fAllocated:=0;
end;

procedure TVulkanBaseList.FillWith(const SourceData;const SourceCount:TVkSizeInt);
var Index:TVkSizeInt;
    SourceItem,Item:pointer;
begin
 SourceItem:=@SourceData;
 if assigned(SourceItem) and (SourceCount>0) then begin
  SetCount(SourceCount);
  Item:=fMemory;
  Index:=0;
  while Index<fCount do begin
   CopyItem(SourceItem^,Item^);
   inc(TVkPtrInt(SourceItem),fItemSize);
   inc(TVkPtrInt(Item),fItemSize);
   inc(Index);
  end;
 end else begin
  SetCount(0);
 end;
end;

function TVulkanBaseList.Add(const Item):TVkSizeInt;
begin
 result:=fCount;
 SetCount(result+1);
 CopyItem(Item,pointer(TVkPtrInt(TVkPtrInt(fMemory)+(result*fItemSize)))^);
end;

function TVulkanBaseList.Find(const Item):TVkSizeInt;
var Index:TVkSizeInt;
begin
 result:=-1;
 Index:=0;
 while Index<fCount do begin
  if CompareItem(Item,pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^)=0 then begin
   result:=Index;
   break;
  end;
  inc(Index);
 end;
end;

procedure TVulkanBaseList.Insert(const Index:TVkSizeInt;const Item);
begin
 if Index>=0 then begin
  if Index<fCount then begin
   SetCount(fCount+1);
   Move(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^,pointer(TVkPtrInt(TVkPtrInt(fMemory)+((Index+1)*fItemSize)))^,(fCount-Index)*fItemSize);
   FillChar(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^,fItemSize,#0);
  end else begin
   SetCount(Index+1);
  end;
  CopyItem(Item,pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^);
 end;
end;

procedure TVulkanBaseList.Delete(const Index:TVkSizeInt);
begin
 if (Index>=0) and (Index<fCount) then begin
  FinalizeItem(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^);
  Move(pointer(TVkPtrInt(TVkPtrInt(fMemory)+((Index+1)*fItemSize)))^,pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^,(fCount-Index)*fItemSize);
  FillChar(pointer(TVkPtrInt(TVkPtrInt(fMemory)+((fCount-1)*fItemSize)))^,fItemSize,#0);
  SetCount(fCount-1);
 end;
end;

procedure TVulkanBaseList.Remove(const Item);
var Index:TVkSizeInt;
begin
 repeat
  Index:=Find(Item);
  if Index>=0 then begin
   Delete(Index);
  end else begin
   break;
  end;
 until false;
end;

procedure TVulkanBaseList.Exchange(const Index,WithIndex:TVkSizeInt);
begin
 if (Index>=0) and (Index<fCount) and (WithIndex>=0) and (WithIndex<fCount) then begin
  ExchangeItem(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^,pointer(TVkPtrInt(TVkPtrInt(fMemory)+(WithIndex*fItemSize)))^);
 end;
end;

constructor TVulkanObjectList.Create;
begin
 inherited Create(SizeOf(TVulkanObject));
end;

destructor TVulkanObjectList.Destroy;
begin
 inherited Destroy;
end;

function TVulkanObjectList.GetItem(const Index:TVkSizeInt):TVulkanObject;
begin
 if (Index>=0) and (Index<fCount) then begin
  result:=TVulkanObject(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^);
 end else begin
  result:=nil;
 end;
end;

procedure TVulkanObjectList.SetItem(const Index:TVkSizeInt;const Item:TVulkanObject);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVulkanObject(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^):=Item;
 end;
end;

procedure TVulkanObjectList.InitializeItem(var Item);
begin
 TVulkanObject(Item):=nil;
end;

procedure TVulkanObjectList.FinalizeItem(var Item);
begin
 TVulkanObject(Item).Free;
end;

procedure TVulkanObjectList.CopyItem(const Source;var Destination);
begin
 TVulkanObject(Destination):=TVulkanObject(Source);
end;

procedure TVulkanObjectList.ExchangeItem(var Source,Destination);
var Temporary:TVulkanObject;
begin
 Temporary:=TVulkanObject(Source);
 TVulkanObject(Source):=TVulkanObject(Destination);
 TVulkanObject(Destination):=Temporary;
end;

function TVulkanObjectList.CompareItem(const Source,Destination):longint;
begin
 result:=TVkPtrDiff(Source)-TVkPtrDiff(Destination);
end;

function TVulkanObjectList.Add(const Item:TVulkanObject):TVkSizeInt;
begin
 result:=inherited Add(Item);
end;

function TVulkanObjectList.Find(const Item:TVulkanObject):TVkSizeInt;
begin
 result:=inherited Find(Item);
end;

procedure TVulkanObjectList.Insert(const Index:TVkSizeInt;const Item:TVulkanObject);
begin
 inherited Insert(Index,Item);
end;

procedure TVulkanObjectList.Remove(const Item:TVulkanObject);
begin
 inherited Remove(Item);
end;

constructor TVkUInt32List.Create;
begin
 inherited Create(SizeOf(TVkUInt32));
end;

destructor TVkUInt32List.Destroy;
begin
 inherited Destroy;
end;

function TVkUInt32List.GetItem(const Index:TVkSizeInt):TVkUInt32;
begin
 if (Index>=0) and (Index<fCount) then begin
  result:=TVkUInt32(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^);
 end else begin
  result:=0;
 end;
end;

procedure TVkUInt32List.SetItem(const Index:TVkSizeInt;const Item:TVkUInt32);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkUInt32(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^):=Item;
 end;
end;

procedure TVkUInt32List.InitializeItem(var Item);
begin
 TVkUInt32(Item):=0;
end;

procedure TVkUInt32List.FinalizeItem(var Item);
begin
 TVkUInt32(Item):=0;
end;

procedure TVkUInt32List.CopyItem(const Source;var Destination);
begin
 TVkUInt32(Destination):=TVkUInt32(Source);
end;

procedure TVkUInt32List.ExchangeItem(var Source,Destination);
var Temporary:TVkUInt32;
begin
 Temporary:=TVkUInt32(Source);
 TVkUInt32(Source):=TVkUInt32(Destination);
 TVkUInt32(Destination):=Temporary;
end;

function TVkUInt32List.CompareItem(const Source,Destination):longint;
begin
 result:=TVkUInt32(Source)-TVkUInt32(Destination);
end;

function TVkUInt32List.Add(const Item:TVkUInt32):TVkSizeInt;
begin
 result:=inherited Add(Item);
end;

function TVkUInt32List.Find(const Item:TVkUInt32):TVkSizeInt;
begin
 result:=inherited Find(Item);
end;

procedure TVkUInt32List.Insert(const Index:TVkSizeInt;const Item:TVkUInt32);
begin
 inherited Insert(Index,Item);
end;

procedure TVkUInt32List.Remove(const Item:TVkUInt32);
begin
 inherited Remove(Item);
end;

constructor TVkFloatList.Create;
begin
 inherited Create(SizeOf(TVkFloat));
end;

destructor TVkFloatList.Destroy;
begin
 inherited Destroy;
end;

function TVkFloatList.GetItem(const Index:TVkSizeInt):TVkFloat;
begin
 if (Index>=0) and (Index<fCount) then begin
  result:=TVkFloat(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^);
 end else begin
  result:=0;
 end;
end;

procedure TVkFloatList.SetItem(const Index:TVkSizeInt;const Item:TVkFloat);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkFloat(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^):=Item;
 end;
end;

procedure TVkFloatList.InitializeItem(var Item);
begin
 TVkFloat(Item):=0;
end;

procedure TVkFloatList.FinalizeItem(var Item);
begin
 TVkFloat(Item):=0;
end;

procedure TVkFloatList.CopyItem(const Source;var Destination);
begin
 TVkFloat(Destination):=TVkFloat(Source);
end;

procedure TVkFloatList.ExchangeItem(var Source,Destination);
var Temporary:TVkFloat;
begin
 Temporary:=TVkFloat(Source);
 TVkFloat(Source):=TVkFloat(Destination);
 TVkFloat(Destination):=Temporary;
end;

function TVkFloatList.CompareItem(const Source,Destination):longint;
begin
 result:=TVkInt32(Source)-TVkInt32(Destination);
end;

function TVkFloatList.Add(const Item:TVkFloat):TVkSizeInt;
begin
 result:=inherited Add(Item);
end;

function TVkFloatList.Find(const Item:TVkFloat):TVkSizeInt;
begin
 result:=inherited Find(Item);
end;

procedure TVkFloatList.Insert(const Index:TVkSizeInt;const Item:TVkFloat);
begin
 inherited Insert(Index,Item);
end;

procedure TVkFloatList.Remove(const Item:TVkFloat);
begin
 inherited Remove(Item);
end;

constructor TVkImageViewList.Create;
begin
 inherited Create(SizeOf(TVkImageView));
end;

destructor TVkImageViewList.Destroy;
begin
 inherited Destroy;
end;

function TVkImageViewList.GetItem(const Index:TVkSizeInt):TVkImageView;
begin
 if (Index>=0) and (Index<fCount) then begin
  result:=TVkImageView(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^);
 end else begin
  result:=0;
 end;
end;

procedure TVkImageViewList.SetItem(const Index:TVkSizeInt;const Item:TVkImageView);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkImageView(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^):=Item;
 end;
end;

procedure TVkImageViewList.InitializeItem(var Item);
begin
 TVkImageView(Item):=0;
end;

procedure TVkImageViewList.FinalizeItem(var Item);
begin
 TVkImageView(Item):=0;
end;

procedure TVkImageViewList.CopyItem(const Source;var Destination);
begin
 TVkImageView(Destination):=TVkImageView(Source);
end;

procedure TVkImageViewList.ExchangeItem(var Source,Destination);
var Temporary:TVkImageView;
begin
 Temporary:=TVkImageView(Source);
 TVkImageView(Source):=TVkImageView(Destination);
 TVkImageView(Destination):=Temporary;
end;

function TVkImageViewList.CompareItem(const Source,Destination):longint;
begin
 result:=TVkImageView(Source)-TVkImageView(Destination);
end;

function TVkImageViewList.Add(const Item:TVkImageView):TVkSizeInt;
begin
 result:=inherited Add(Item);
end;

function TVkImageViewList.Find(const Item:TVkImageView):TVkSizeInt;
begin
 result:=inherited Find(Item);
end;

procedure TVkImageViewList.Insert(const Index:TVkSizeInt;const Item:TVkImageView);
begin
 inherited Insert(Index,Item);
end;

procedure TVkImageViewList.Remove(const Item:TVkImageView);
begin
 inherited Remove(Item);
end;

constructor TVkSamplerList.Create;
begin
 inherited Create(SizeOf(TVkSampler));
end;

destructor TVkSamplerList.Destroy;
begin
 inherited Destroy;
end;

function TVkSamplerList.GetItem(const Index:TVkSizeInt):TVkSampler;
begin
 if (Index>=0) and (Index<fCount) then begin
  result:=TVkSampler(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^);
 end else begin
  result:=0;
 end;
end;

procedure TVkSamplerList.SetItem(const Index:TVkSizeInt;const Item:TVkSampler);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkSampler(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^):=Item;
 end;
end;

procedure TVkSamplerList.InitializeItem(var Item);
begin
 TVkSampler(Item):=0;
end;

procedure TVkSamplerList.FinalizeItem(var Item);
begin
 TVkSampler(Item):=0;
end;

procedure TVkSamplerList.CopyItem(const Source;var Destination);
begin
 TVkSampler(Destination):=TVkSampler(Source);
end;

procedure TVkSamplerList.ExchangeItem(var Source,Destination);
var Temporary:TVkSampler;
begin
 Temporary:=TVkSampler(Source);
 TVkSampler(Source):=TVkSampler(Destination);
 TVkSampler(Destination):=Temporary;
end;

function TVkSamplerList.CompareItem(const Source,Destination):longint;
begin
 result:=TVkSampler(Source)-TVkSampler(Destination);
end;

function TVkSamplerList.Add(const Item:TVkSampler):TVkSizeInt;
begin
 result:=inherited Add(Item);
end;

function TVkSamplerList.Find(const Item:TVkSampler):TVkSizeInt;
begin
 result:=inherited Find(Item);
end;

procedure TVkSamplerList.Insert(const Index:TVkSizeInt;const Item:TVkSampler);
begin
 inherited Insert(Index,Item);
end;

procedure TVkSamplerList.Remove(const Item:TVkSampler);
begin
 inherited Remove(Item);
end;

constructor TVkDescriptorSetLayoutList.Create;
begin
 inherited Create(SizeOf(TVkDescriptorSetLayout));
end;

destructor TVkDescriptorSetLayoutList.Destroy;
begin
 inherited Destroy;
end;

function TVkDescriptorSetLayoutList.GetItem(const Index:TVkSizeInt):TVkDescriptorSetLayout;
begin
 if (Index>=0) and (Index<fCount) then begin
  result:=TVkDescriptorSetLayout(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^);
 end else begin
  result:=0;
 end;
end;

procedure TVkDescriptorSetLayoutList.SetItem(const Index:TVkSizeInt;const Item:TVkDescriptorSetLayout);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkDescriptorSetLayout(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^):=Item;
 end;
end;

procedure TVkDescriptorSetLayoutList.InitializeItem(var Item);
begin
 TVkDescriptorSetLayout(Item):=0;
end;

procedure TVkDescriptorSetLayoutList.FinalizeItem(var Item);
begin
 TVkDescriptorSetLayout(Item):=0;
end;

procedure TVkDescriptorSetLayoutList.CopyItem(const Source;var Destination);
begin
 TVkDescriptorSetLayout(Destination):=TVkDescriptorSetLayout(Source);
end;

procedure TVkDescriptorSetLayoutList.ExchangeItem(var Source,Destination);
var Temporary:TVkDescriptorSetLayout;
begin
 Temporary:=TVkDescriptorSetLayout(Source);
 TVkDescriptorSetLayout(Source):=TVkDescriptorSetLayout(Destination);
 TVkDescriptorSetLayout(Destination):=Temporary;
end;

function TVkDescriptorSetLayoutList.CompareItem(const Source,Destination):longint;
begin
 result:=TVkDescriptorSetLayout(Source)-TVkDescriptorSetLayout(Destination);
end;

function TVkDescriptorSetLayoutList.Add(const Item:TVkDescriptorSetLayout):TVkSizeInt;
begin
 result:=inherited Add(Item);
end;

function TVkDescriptorSetLayoutList.Find(const Item:TVkDescriptorSetLayout):TVkSizeInt;
begin
 result:=inherited Find(Item);
end;

procedure TVkDescriptorSetLayoutList.Insert(const Index:TVkSizeInt;const Item:TVkDescriptorSetLayout);
begin
 inherited Insert(Index,Item);
end;

procedure TVkDescriptorSetLayoutList.Remove(const Item:TVkDescriptorSetLayout);
begin
 inherited Remove(Item);
end;

constructor TVkSampleMaskList.Create;
begin
 inherited Create(SizeOf(TVkSampleMask));
end;

destructor TVkSampleMaskList.Destroy;
begin
 inherited Destroy;
end;

function TVkSampleMaskList.GetItem(const Index:TVkSizeInt):TVkSampleMask;
begin
 if (Index>=0) and (Index<fCount) then begin
  result:=TVkSampleMask(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^);
 end else begin
  result:=0;
 end;
end;

procedure TVkSampleMaskList.SetItem(const Index:TVkSizeInt;const Item:TVkSampleMask);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkSampleMask(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^):=Item;
 end;
end;

procedure TVkSampleMaskList.InitializeItem(var Item);
begin
 TVkSampleMask(Item):=0;
end;

procedure TVkSampleMaskList.FinalizeItem(var Item);
begin
 TVkSampleMask(Item):=0;
end;

procedure TVkSampleMaskList.CopyItem(const Source;var Destination);
begin
 TVkSampleMask(Destination):=TVkSampleMask(Source);
end;

procedure TVkSampleMaskList.ExchangeItem(var Source,Destination);
var Temporary:TVkSampleMask;
begin
 Temporary:=TVkSampleMask(Source);
 TVkSampleMask(Source):=TVkSampleMask(Destination);
 TVkSampleMask(Destination):=Temporary;
end;

function TVkSampleMaskList.CompareItem(const Source,Destination):longint;
begin
 result:=TVkSampleMask(Source)-TVkSampleMask(Destination);
end;

function TVkSampleMaskList.Add(const Item:TVkSampleMask):TVkSizeInt;
begin
 result:=inherited Add(Item);
end;

function TVkSampleMaskList.Find(const Item:TVkSampleMask):TVkSizeInt;
begin
 result:=inherited Find(Item);
end;

procedure TVkSampleMaskList.Insert(const Index:TVkSizeInt;const Item:TVkSampleMask);
begin
 inherited Insert(Index,Item);
end;

procedure TVkSampleMaskList.Remove(const Item:TVkSampleMask);
begin
 inherited Remove(Item);
end;

constructor TVkDynamicStateList.Create;
begin
 inherited Create(SizeOf(TVkDynamicState));
end;

destructor TVkDynamicStateList.Destroy;
begin
 inherited Destroy;
end;

function TVkDynamicStateList.GetItem(const Index:TVkSizeInt):TVkDynamicState;
begin
 if (Index>=0) and (Index<fCount) then begin
  result:=TVkDynamicState(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^);
 end else begin
  result:=TVkDynamicState(0);
 end;
end;

procedure TVkDynamicStateList.SetItem(const Index:TVkSizeInt;const Item:TVkDynamicState);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkDynamicState(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^):=Item;
 end;
end;

procedure TVkDynamicStateList.InitializeItem(var Item);
begin
 Initialize(TVkDynamicState(Item));
end;

procedure TVkDynamicStateList.FinalizeItem(var Item);
begin
 Finalize(TVkDynamicState(Item));
end;

procedure TVkDynamicStateList.CopyItem(const Source;var Destination);
begin
 TVkDynamicState(Destination):=TVkDynamicState(Source);
end;

procedure TVkDynamicStateList.ExchangeItem(var Source,Destination);
var Temporary:TVkDynamicState;
begin
 Temporary:=TVkDynamicState(Source);
 TVkDynamicState(Source):=TVkDynamicState(Destination);
 TVkDynamicState(Destination):=Temporary;
end;

function TVkDynamicStateList.CompareItem(const Source,Destination):longint;
begin
 result:=TVkSize(TVkDynamicState(Source))-TVkSize(TVkDynamicState(Destination));
end;

function TVkDynamicStateList.Add(const Item:TVkDynamicState):TVkSizeInt;
begin
 result:=inherited Add(Item);
end;

function TVkDynamicStateList.Find(const Item:TVkDynamicState):TVkSizeInt;
begin
 result:=inherited Find(Item);
end;

procedure TVkDynamicStateList.Insert(const Index:TVkSizeInt;const Item:TVkDynamicState);
begin
 inherited Insert(Index,Item);
end;

procedure TVkDynamicStateList.Remove(const Item:TVkDynamicState);
begin
 inherited Remove(Item);
end;

constructor TVkBufferViewList.Create;
begin
 inherited Create(SizeOf(TVkBufferView));
end;

destructor TVkBufferViewList.Destroy;
begin
 inherited Destroy;
end;

function TVkBufferViewList.GetItem(const Index:TVkSizeInt):TVkBufferView;
begin
 if (Index>=0) and (Index<fCount) then begin
  result:=TVkBufferView(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^);
 end else begin
  result:=TVkBufferView(0);
 end;
end;

procedure TVkBufferViewList.SetItem(const Index:TVkSizeInt;const Item:TVkBufferView);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkBufferView(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^):=Item;
 end;
end;

procedure TVkBufferViewList.InitializeItem(var Item);
begin
 Initialize(TVkBufferView(Item));
end;

procedure TVkBufferViewList.FinalizeItem(var Item);
begin
 Finalize(TVkBufferView(Item));
end;

procedure TVkBufferViewList.CopyItem(const Source;var Destination);
begin
 TVkBufferView(Destination):=TVkBufferView(Source);
end;

procedure TVkBufferViewList.ExchangeItem(var Source,Destination);
var Temporary:TVkBufferView;
begin
 Temporary:=TVkBufferView(Source);
 TVkBufferView(Source):=TVkBufferView(Destination);
 TVkBufferView(Destination):=Temporary;
end;

function TVkBufferViewList.CompareItem(const Source,Destination):longint;
begin
 result:=TVkSize(TVkBufferView(Source))-TVkSize(TVkBufferView(Destination));
end;

function TVkBufferViewList.Add(const Item:TVkBufferView):TVkSizeInt;
begin
 result:=inherited Add(Item);
end;

function TVkBufferViewList.Find(const Item:TVkBufferView):TVkSizeInt;
begin
 result:=inherited Find(Item);
end;

procedure TVkBufferViewList.Insert(const Index:TVkSizeInt;const Item:TVkBufferView);
begin
 inherited Insert(Index,Item);
end;

procedure TVkBufferViewList.Remove(const Item:TVkBufferView);
begin
 inherited Remove(Item);
end;

constructor TVkClearValueList.Create;
begin
 inherited Create(SizeOf(TVkClearValue));
end;

destructor TVkClearValueList.Destroy;
begin
 inherited Destroy;
end;

function TVkClearValueList.GetItem(const Index:TVkSizeInt):TVkClearValue;
begin
 if (Index>=0) and (Index<fCount) then begin
  result:=TVkClearValue(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^);
 end else begin
  FillChar(result,SizeOf(TVkClearValue),#0);
 end;
end;

procedure TVkClearValueList.SetItem(const Index:TVkSizeInt;const Item:TVkClearValue);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkClearValue(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(Index*fItemSize)))^):=Item;
 end;
end;

procedure TVkClearValueList.InitializeItem(var Item);
begin
 Initialize(TVkClearValue(Item));
end;

procedure TVkClearValueList.FinalizeItem(var Item);
begin
 Finalize(TVkClearValue(Item));
end;

procedure TVkClearValueList.CopyItem(const Source;var Destination);
begin
 TVkClearValue(Destination):=TVkClearValue(Source);
end;

procedure TVkClearValueList.ExchangeItem(var Source,Destination);
var Temporary:TVkClearValue;
begin
 Temporary:=TVkClearValue(Source);
 TVkClearValue(Source):=TVkClearValue(Destination);
 TVkClearValue(Destination):=Temporary;
end;

function TVkClearValueList.CompareItem(const Source,Destination):longint;
begin
 result:=inherited CompareItem(Source,Destination);
end;

function TVkClearValueList.Add(const Item:TVkClearValue):TVkSizeInt;
begin
 result:=inherited Add(Item);
end;

function TVkClearValueList.Find(const Item:TVkClearValue):TVkSizeInt;
begin
 result:=inherited Find(Item);
end;

procedure TVkClearValueList.Insert(const Index:TVkSizeInt;const Item:TVkClearValue);
begin
 inherited Insert(Index,Item);
end;

procedure TVkClearValueList.Remove(const Item:TVkClearValue);
begin
 inherited Remove(Item);
end;

constructor TVulkanHandle.Create;
begin
 inherited Create;
 fOwnsHandle:=true;
end;

destructor TVulkanHandle.Destroy;
begin
 inherited Destroy;
end;

constructor TVulkanAllocationHandle.Create;
begin
 inherited Create;
 fAllocationCallbacks:=nil;
end;

destructor TVulkanAllocationHandle.Destroy;
begin
 SetAllocationCallbacks(nil);
 inherited Destroy;
end;

procedure TVulkanAllocationHandle.SetAllocationCallbacks(const NewAllocationCallbacks:PVkAllocationCallbacks);
begin
 if assigned(NewAllocationCallbacks) then begin
  if not assigned(fAllocationCallbacks) then begin
   GetMem(fAllocationCallbacks,SizeOf(TVkAllocationCallbacks));
  end;
  fAllocationCallbacks^:=NewAllocationCallbacks^;
 end else begin
  if assigned(fAllocationCallbacks) then begin
   FreeMem(fAllocationCallbacks);
   fAllocationCallbacks:=nil;
  end;
 end;
end;

function VulkanAllocationCallback(UserData:PVkVoid;Size:TVkSize;Alignment:TVkSize;Scope:TVkSystemAllocationScope):PVkVoid; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
begin
 result:=TVulkanAllocationManager(UserData).AllocationCallback(Size,Alignment,Scope);
end;

function VulkanReallocationCallback(UserData,Original:PVkVoid;Size:TVkSize;Alignment:TVkSize;Scope:TVkSystemAllocationScope):PVkVoid; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
begin
 result:=TVulkanAllocationManager(UserData).ReallocationCallback(Original,Size,Alignment,Scope);
end;

procedure VulkanFreeCallback(UserData,Memory:PVkVoid); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
begin
 TVulkanAllocationManager(UserData).FreeCallback(Memory);
end;

procedure VulkanInternalAllocationCallback(UserData:PVkVoid;Size:TVkSize;Type_:TVkInternalAllocationType;Scope:TVkSystemAllocationScope); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
begin
 TVulkanAllocationManager(UserData).InternalAllocationCallback(Size,Type_,Scope);
end;

procedure VulkanInternalFreeCallback(UserData:PVkVoid;Size:TVkSize;Type_:TVkInternalAllocationType;Scope:TVkSystemAllocationScope); {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
begin
 TVulkanAllocationManager(UserData).InternalFreeCallback(Size,Type_,Scope);
end;

constructor TVulkanAllocationManager.Create;
begin
 inherited Create;
 FillChar(fAllocationCallbacks,SizeOf(TVkAllocationCallbacks),#0);
 fAllocationCallbacks.pUserData:=self;
 fAllocationCallbacks.pfnAllocation:=VulkanAllocationCallback;
 fAllocationCallbacks.pfnReallocation:=VulkanReallocationCallback;
 fAllocationCallbacks.pfnFree:=VulkanFreeCallback;
 fAllocationCallbacks.pfnInternalAllocation:=VulkanInternalAllocationCallback;
 fAllocationCallbacks.pfnInternalFree:=VulkanInternalFreeCallback;
end;

destructor TVulkanAllocationManager.Destroy;
begin
 inherited Destroy;
end;

function TVulkanAllocationManager.AllocationCallback(const Size:TVkSize;const Alignment:TVkSize;const Scope:TVkSystemAllocationScope):PVkVoid;
begin
 GetMem(result,Size);
end;

function TVulkanAllocationManager.ReallocationCallback(const Original:PVkVoid;const Size:TVkSize;const Alignment:TVkSize;const Scope:TVkSystemAllocationScope):PVkVoid;
begin
 result:=Original;
 ReallocMem(result,Size);
end;

procedure TVulkanAllocationManager.FreeCallback(const Memory:PVkVoid);
begin
 FreeMem(Memory);
end;

procedure TVulkanAllocationManager.InternalAllocationCallback(const Size:TVkSize;const Type_:TVkInternalAllocationType;const Scope:TVkSystemAllocationScope);
begin
end;

procedure TVulkanAllocationManager.InternalFreeCallback(const Size:TVkSize;const Type_:TVkInternalAllocationType;const Scope:TVkSystemAllocationScope);
begin
end;

constructor TVulkanApplicationInfo.Create(const pApplicationName:TVulkanCharString='Vulkan application';
                                          const pApplicationVersion:TVkUInt32=1;
                                          const pEngineName:TVulkanCharString='Vulkan engine';
                                          const pEngineVersion:TVkUInt32=1;
                                          const pAPIVersion:TVkUInt32=VK_API_VERSION_1_0);
begin
 inherited Create;

 fApplicationName:=pApplicationName;
 fEngineName:=pEngineName;

 FillChar(fApplicationInfo,SizeOf(TVkApplicationInfo),#0);
 fApplicationInfo.sType:=VK_STRUCTURE_TYPE_APPLICATION_INFO;
 fApplicationInfo.pNext:=nil;
 fApplicationInfo.pApplicationName:=PVkChar(fApplicationName);
 fApplicationInfo.applicationVersion:=pApplicationVersion;
 fApplicationInfo.pEngineName:=PVkChar(fEngineName);
 fApplicationInfo.engineVersion:=pEngineVersion;
 fApplicationInfo.apiVersion:=pAPIVersion;

end;

destructor TVulkanApplicationInfo.Destroy;
begin
 fApplicationName:='';
 fEngineName:='';
 inherited Destroy;
end;

procedure TVulkanApplicationInfo.SetApplicationInfo(const NewApplicationInfo:TVkApplicationInfo);
begin
 fApplicationInfo:=NewApplicationInfo;
 fApplicationName:=fApplicationInfo.pApplicationName;
 fEngineName:=fApplicationInfo.pEngineName;
 fApplicationInfo.pApplicationName:=PVkChar(fApplicationName);
 fApplicationInfo.pEngineName:=PVkChar(fEngineName);
end;

function TVulkanApplicationInfo.GetApplicationName:TVulkanCharString;
begin
 result:=fApplicationName;
end;

procedure TVulkanApplicationInfo.SetApplicationName(const NewApplicationName:TVulkanCharString);
begin
 fApplicationName:=NewApplicationName;
 fApplicationInfo.pApplicationName:=PVkChar(fApplicationName);
end;

function TVulkanApplicationInfo.GetApplicationVersion:TVkUInt32;
begin
 result:=fApplicationInfo.applicationVersion;
end;

procedure TVulkanApplicationInfo.SetApplicationVersion(const NewApplicationVersion:TVkUInt32);
begin
 fApplicationInfo.applicationVersion:=NewApplicationVersion;
end;

function TVulkanApplicationInfo.GetEngineName:TVulkanCharString;
begin
 result:=fEngineName;
end;

procedure TVulkanApplicationInfo.SetEngineName(const NewEngineName:TVulkanCharString);
begin
 fEngineName:=NewEngineName;
 fApplicationInfo.pEngineName:=PVkChar(fEngineName);
end;

function TVulkanApplicationInfo.GetEngineVersion:TVkUInt32;
begin
 result:=fApplicationInfo.engineVersion;
end;

procedure TVulkanApplicationInfo.SetEngineVersion(const NewEngineVersion:TVkUInt32);
begin
 fApplicationInfo.engineVersion:=NewEngineVersion;
end;

function TVulkanApplicationInfo.GetAPIVersion:TVkUInt32;
begin
 result:=fApplicationInfo.apiVersion;
end;

procedure TVulkanApplicationInfo.SetAPIVersion(const NewAPIVersion:TVkUInt32);
begin
 fApplicationInfo.apiVersion:=NewAPIVersion;
end;

constructor TVulkanInstance.Create(const pVulkan:TVulkan;
                                   const pFlags:TVkInstanceCreateFlags;
                                   const pApplicationInfo:TVulkanApplicationInfo;
                                   const pEnabledLayerNames:array of TVulkanCharString;
                                   const pEnabledExtensionNames:array of TVulkanCharString;
                                   const pAllocationManager:TVulkanAllocationManager);
var i:TVkInt32;
begin
 inherited Create;

 fInstance:=VK_NULL_INSTANCE;

 fVulkan:=pVulkan;

 fApplicationInfo:=pApplicationInfo;

 fAllocationManager:=pAllocationManager;

 if assigned(fAllocationManager) then begin
  fAllocationCallbacks:=@fAllocationManager.fAllocationCallbacks;
 end else begin
  fAllocationCallbacks:=nil;
 end;

 SetLength(fEnabledLayerNames,length(pEnabledLayerNames));
 SetLength(fRawEnabledLayerNames,length(pEnabledLayerNames));
 for i:=0 to length(pEnabledLayerNames)-1 do begin
  fEnabledLayerNames[i]:=pEnabledLayerNames[i];
  fRawEnabledLayerNames[i]:=PVkChar(fEnabledLayerNames[i]);
 end;

 SetLength(fEnabledExtensionNames,length(pEnabledExtensionNames));
 SetLength(fRawEnabledExtensionNames,length(pEnabledExtensionNames));
 for i:=0 to length(pEnabledExtensionNames)-1 do begin
  fEnabledExtensionNames[i]:=pEnabledExtensionNames[i];
  fRawEnabledExtensionNames[i]:=PVkChar(fEnabledExtensionNames[i]);
 end;

 FillChar(fInstanceCreateInfo,SizeOf(TVkInstanceCreateInfo),#0);
 fInstanceCreateInfo.sType:=VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
 if length(pEnabledLayerNames)>0 then begin
  fInstanceCreateInfo.enabledLayerCount:=length(pEnabledLayerNames);
  fInstanceCreateInfo.ppEnabledLayerNames:=@fRawEnabledLayerNames[0];
 end;
 if length(pEnabledExtensionNames)>0 then begin
  fInstanceCreateInfo.enabledExtensionCount:=length(pEnabledExtensionNames);
  fInstanceCreateInfo.ppEnabledExtensionNames:=@fRawEnabledExtensionNames[0];
 end;

 HandleResultCode(fVulkan.CreateInstance(@fInstanceCreateInfo,fAllocationCallbacks,@Instance));

end;

destructor TVulkanInstance.Destroy;
begin
 if fInstance<>VK_NULL_INSTANCE then begin
  fVulkan.DestroyInstance(fInstance,fAllocationCallbacks);
 end;
 inherited Destroy;
end;

{
constructor TVulkanResource.Create;
begin
 inherited Create;
 fDevice:=VK_NULL_HANDLE;
 fOwnsResource:=false;
end;

destructor TVulkanResource.Destroy;
begin
 inherited Destroy;
end;

procedure TVulkanResource.Clear;
begin
 fDevice:=VK_NULL_HANDLE;
 fOwnsResource:=false;
end;

constructor TVulkanDevice.Create(const pDevice:TVkDevice;const pPhysicalDevice:TVkPhysicalDevice);
begin
 inherited Create;
 fDevice:=pDevice;
 fPhysicalDevice:=pPhysicalDevice;
end;

destructor TVulkanDevice.Destroy;
begin
 inherited Destroy;
end;

function TVulkanDevice.GetMemoryType(const pTypeBits:TVkUInt32;const pProperties:TVkFlags):TVkUInt32;
var i:TVkUInt32;
    DeviceMemoryProperties:TVkPhysicalDeviceMemoryProperties;
begin
 result:=not TVkUInt32(0);
 vkGetPhysicalDeviceMemoryProperties(fPhysicalDevice,@DeviceMemoryProperties);
 for i:=0 to 31 do begin
  if (pTypeBits and (TVkUInt32(1) shl i))<>0 then begin
   if (DeviceMemoryProperties.MemoryTypes[i].PropertyFlags and pProperties)=pProperties then begin
    result:=i;
    exit;
   end;
  end;
 end;
end;

function TVulkanDevice.GetBestSupportedDepthFormat:TVkFormat;
const Formats:array[0..4] of TVkFormat=(VK_FORMAT_D32_SFLOAT_S8_UINT,
                                        VK_FORMAT_D32_SFLOAT,
                                        VK_FORMAT_D24_UNORM_S8_UINT,
                                        VK_FORMAT_D16_UNORM_S8_UINT,
                                        VK_FORMAT_D16_UNORM);
var i:TVkInt32;
    Format:TVkFormat;
    FormatProperties:TVkFormatProperties;
begin
 result:=VK_FORMAT_UNDEFINED;
 for i:=low(Formats) to high(Formats) do begin
  Format:=Formats[i];
  vkGetPhysicalDeviceFormatProperties(PhysicalDevice,Format,@FormatProperties);
  if (FormatProperties.OptimalTilingFeatures and TVkFormatFeatureFlags(VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT))<>0 then begin
   result:=Format;
   exit;
  end;
 end;
end;

function TVulkanDevice.GetGraphicsQueueNodeIndex(const pSurface:TVkSurfaceKHR):TVkUInt32;
var i:TVKInt32;
    QueueCount,GraphicsQueueNodeIndex,PresentQueueNodeIndex:TVkUInt32;
    QueueProperties:array of TVkQueueFamilyProperties;
    SupportsPresent:array of TVkBool32;
begin
 result:=not TVkUInt32(0);

 vkGetPhysicalDeviceQueueFamilyProperties(fPhysicalDevice,@QueueCount,nil);

 QueueProperties:=nil;
 SetLength(QueueProperties,QueueCount);
 try

  SupportsPresent:=nil;
  SetLength(SupportsPresent,QueueCount);
  try

   vkGetPhysicalDeviceQueueFamilyProperties(fPhysicalDevice,@QueueCount,@QueueProperties[0]);

   for i:=0 to QueueCount-1 do begin
    vkGetPhysicalDeviceSurfaceSupportKHR(fPhysicalDevice,i,pSurface,@SupportsPresent[i]);
   end;

   GraphicsQueueNodeIndex:=high(TVkUInt32);
   PresentQueueNodeIndex:=high(TVkUInt32);
   for i:=0 to QueueCount-1 do begin
    if (QueueProperties[i].QueueFlags and TVkQueueFlags(VK_QUEUE_GRAPHICS_BIT))<>0 then begin
     if GraphicsQueueNodeIndex=high(TVkUInt32) then begin
      GraphicsQueueNodeIndex:=i;
     end;
     if SupportsPresent[i]=VK_TRUE then begin
      result:=i;
      break;
     end;
    end;
   end;

  finally
   SetLength(SupportsPresent,0);
  end;
 finally
  SetLength(QueueProperties,0);
 end;
end;

function TVulkanDevice.GetSurfaceFormats(const pSurface:TVkSurfaceKHR):TVulkanDeviceSurfaceFormats;
var FormatCount:TVkUInt32;
begin
 result:=nil;

 FormatCount:=0;
 HandleResultCode(vkGetPhysicalDeviceSurfaceFormatsKHR(fPhysicalDevice,pSurface,@FormatCount,nil));

 if FormatCount>0 then begin
  SetLength(result,FormatCount);
  HandleResultCode(vkGetPhysicalDeviceSurfaceFormatsKHR(fPhysicalDevice,pSurface,@FormatCount,@result[0]));
 end;

 if (FormatCount=0) or (result[0].Format=VK_FORMAT_UNDEFINED) then begin
  SetLength(result,1);
  result[0].Format:=VK_FORMAT_B8G8R8A8_UNORM;
  result[0].ColorSpace:=VK_COLORSPACE_SRGB_NONLINEAR_KHR;
 end;

end;

function TVulkanDevice.GetSurfaceFormat(const pSurface:TVkSurfaceKHR):TVulkanDeviceSurfaceFormat;
var FormatCount:TVkUInt32;
    SurfaceFormats:TVulkanDeviceSurfaceFormats;
begin
 SurfaceFormats:=nil;
 try

  FormatCount:=0;
  HandleResultCode(vkGetPhysicalDeviceSurfaceFormatsKHR(fPhysicalDevice,pSurface,@FormatCount,nil));

  if FormatCount>0 then begin
   SetLength(SurfaceFormats,FormatCount);
   HandleResultCode(vkGetPhysicalDeviceSurfaceFormatsKHR(fPhysicalDevice,pSurface,@FormatCount,@SurfaceFormats[0]));
  end;

  if (FormatCount=0) or (SurfaceFormats[0].Format=VK_FORMAT_UNDEFINED) then begin
   result.Format:=VK_FORMAT_B8G8R8A8_UNORM;
   result.ColorSpace:=VK_COLORSPACE_SRGB_NONLINEAR_KHR;
  end else begin
   result:=SurfaceFormats[0];
  end;

 finally
  SetLength(SurfaceFormats,0);
 end;

end;

procedure TVulkanDevice.WaitIdle;
begin
 vkDeviceWaitIdle(fDevice);
end;

constructor TVulkanInstance.Create(const pEnableValidation:boolean);
const ValidationLayerNames:array[0..8] of PVkChar=
       (
        'VK_LAYER_LUNARG_threading',
        'VK_LAYER_LUNARG_mem_tracker',
        'VK_LAYER_LUNARG_object_tracker',
        'VK_LAYER_LUNARG_draw_state',
        'VK_LAYER_LUNARG_param_checker',
        'VK_LAYER_LUNARG_swapchain',
        'VK_LAYER_LUNARG_device_limits',
        'VK_LAYER_LUNARG_image',
        'VK_LAYER_GOOGLE_unique_objects'
       );
begin
 inherited Create;
 fEnableValidation:=pEnableValidation;
end;

destructor TVulkanInstance.Destroy;
begin
 inherited Destroy;
end;
}

end.
