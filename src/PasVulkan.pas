(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                        Version 2016-06-01-22-16-0000                       *
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

uses {$ifdef Windows}Windows,{$endif}{$ifdef Unix}BaseUnix,UnixType,dl,{$endif}{$ifdef X11}x,xlib,{$endif}{$ifdef XCB}xcb,{$endif}{$ifdef Mir}Mir,{$endif}{$ifdef Wayland}Wayland,{$endif}{$ifdef Android}Android,{$endif}SysUtils,Classes,Math,Vulkan;

type EVulkanException=class(Exception);

     EVulkanResultException=class(EVulkanException)
      private
       fResultCode:TVkResult;
      public
       constructor Create(const pResultCode:TVkResult);
       destructor Destroy; override;
      published
       property ResultCode:TVkResult read fResultCode;
     end;

     TVulkanObject=class(TInterfacedObject);

     TVulkanCharString=AnsiString;

     TVulkanCharStringArray=array of TVulkanCharString;
     TVkInt32Array=array of TVkInt32;
     TVkUInt32Array=array of TVkUInt32;
     TVkFloatArray=array of TVkFloat;
     TVkLayerPropertiesArray=array of TVkLayerProperties;
     TVkExtensionPropertiesArray=array of TVkExtensionProperties;
     TVkLayerExtensionPropertiesArray=array of array of TVkExtensionProperties;
     TPVkCharArray=array of PVkChar;
     TVkPhysicalDeviceArray=array of TVkPhysicalDevice;
     TVkQueueFamilyPropertiesArray=array of TVkQueueFamilyProperties;
     TVkSparseImageFormatPropertiesArray=array of TVkSparseImageFormatProperties;
     TVkSurfaceFormatKHRArray=array of TVkSurfaceFormatKHR;
     TVkPresentModeKHRArray=array of TVkPresentModeKHR;
     TVkDisplayPropertiesKHRArray=array of TVkDisplayPropertiesKHR;
     TVkDisplayPlanePropertiesKHRArray=array of TVkDisplayPlanePropertiesKHR;
     TVkDisplayKHRArray=array of TVkDisplayKHR;
     TVkDisplayModePropertiesKHRArray=array of TVkDisplayModePropertiesKHR;
     TVkDeviceQueueCreateInfoArray=array of TVkDeviceQueueCreateInfo;
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

     TVulkanBaseList=class(TVulkanObject)
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
       fOwnObjects:boolean;
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
       procedure Clear; override;
       function Add(const Item:TVulkanObject):TVkSizeInt; reintroduce;
       function Find(const Item:TVulkanObject):TVkSizeInt; reintroduce;
       procedure Insert(const Index:TVkSizeInt;const Item:TVulkanObject); reintroduce;
       procedure Remove(const Item:TVulkanObject); reintroduce;
       property Items[const Index:TVkSizeInt]:TVulkanObject read GetItem write SetItem; default;
       property OwnObjects:boolean read fOwnObjects write fOwnObjects;
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

     PVulkanAvailableLayer=^TVulkanAvailableLayer;
     TVulkanAvailableLayer=record
      LayerName:TVulkanCharString;
      SpecVersion:TVkUInt32;
      ImplementationVersion:TVkUInt32;
      Description:TVulkanCharString;
     end;

     TVulkanAvailableLayers=array of TVulkanAvailableLayer;

     PVulkanAvailableExtension=^TVulkanAvailableExtension;
     TVulkanAvailableExtension=record
      LayerIndex:TVkUInt32;
      ExtensionName:TVulkanCharString;
      SpecVersion:TVkUInt32;
     end;

     TVulkanAvailableExtensions=array of TVulkanAvailableExtension;

     TVulkanInstance=class;

     TVulkanPhysicalDevice=class;

     TVulkanPhysicalDeviceList=class;

     TVulkanInstanceDebugReportCallback=function(const flags:TVkDebugReportFlagsEXT;const objectType:TVkDebugReportObjectTypeEXT;const object_:TVkUInt64;const location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:TVulkaNCharString;const pMessage:TVulkanCharString):TVkBool32 of object;

     TVulkanInstance=class(TVulkanHandle)
      private    
       fVulkan:TVulkan;
       fApplicationInfo:TVkApplicationInfo;
       fApplicationName:TVulkanCharString;
       fEngineName:TVulkanCharString;
       fValidation:longbool;
       fAllocationManager:TVulkanAllocationManager;
       fAllocationCallbacks:PVkAllocationCallbacks;
       fAvailableLayers:TVulkanAvailableLayers;
       fAvailableExtensions:TVulkanAvailableExtensions;
       fAvailableLayerNames:TStringList;
       fAvailableExtensionNames:TStringList;
       fEnabledLayerNames:TStringList;
       fEnabledExtensionNames:TStringList;
       fInstanceCreateInfo:TVkInstanceCreateInfo;
       fEnabledLayerNameStrings:array of TVulkanCharString;
       fEnabledExtensionNameStrings:array of TVulkanCharString;
       fRawEnabledLayerNameStrings:array of PVkChar;
       fRawEnabledExtensionNameStrings:array of PVkChar;
       fInstanceHandle:TVkInstance;
       fInstanceVulkan:TVulkan;
       fPhysicalDevices:TVulkanPhysicalDeviceList;
       fNeedToEnumeratePhysicalDevices:boolean;
       fDebugReportCallbackCreateInfoEXT:TVkDebugReportCallbackCreateInfoEXT;
       fDebugReportCallbackEXT:TVkDebugReportCallbackEXT;
       fOnInstanceDebugReportCallback:TVulkanInstanceDebugReportCallback;
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
      protected
       function DebugReportCallback(const flags:TVkDebugReportFlagsEXT;const objectType:TVkDebugReportObjectTypeEXT;const object_:TVkUInt64;const location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:TVulkaNCharString;const pMessage:TVulkanCharString):TVkBool32; virtual;
      public
       constructor Create(const pApplicationName:TVulkanCharString='Vulkan application';
                          const pApplicationVersion:TVkUInt32=1;
                          const pEngineName:TVulkanCharString='Vulkan engine';
                          const pEngineVersion:TVkUInt32=1;
                          const pAPIVersion:TVkUInt32=VK_API_VERSION_1_0;
                          const pValidation:boolean=false;
                          const pAllocationManager:TVulkanAllocationManager=nil);
       destructor Destroy; override;
       procedure Initialize;
       procedure RefreshPhysicalDevices;
       procedure InstallDebugReportCallback;
       property ApplicationName:TVulkanCharString read GetApplicationName write SetApplicationName;
       property ApplicationVersion:TVkUInt32 read GetApplicationVersion write SetApplicationVersion;
       property EngineName:TVulkanCharString read GetEngineName write SetEngineName;
       property EngineVersion:TVkUInt32 read GetEngineVersion write SetEngineVersion;
       property APIVersion:TVkUInt32 read GetAPIVersion write SetAPIVersion;
       property Validation:longbool read fValidation write fValidation;
       property ApplicationInfo:TVkApplicationInfo read fApplicationInfo write SetApplicationInfo;
       property AvailableLayers:TVulkanAvailableLayers read fAvailableLayers;
       property AvailableExtensions:TVulkanAvailableExtensions read fAvailableExtensions;
       property AvailableLayerNames:TStringList read fAvailableLayerNames;
       property AvailableExtensionNames:TStringList read fAvailableExtensionNames;
       property EnabledLayerNames:TStringList read fEnabledLayerNames;
       property EnabledExtensionNames:TStringList read fEnabledExtensionNames;
       property Handle:TVkInstance read fInstanceHandle;
       property Commands:TVulkan read fInstanceVulkan;
       property PhysicalDevices:TVulkanPhysicalDeviceList read fPhysicalDevices;
       property OnInstanceDebugReportCallback:TVulkanInstanceDebugReportCallback read fOnInstanceDebugReportCallback write fOnInstanceDebugReportCallback;
     end;

     TVulkanPhysicalDevice=class(TVulkanObject)
      private
       fInstance:TVulkanInstance;
       fPhysicalDeviceHandle:TVkPhysicalDevice;
       fDeviceName:TVulkanCharString;
       fProperties:TVkPhysicalDeviceProperties;
       fMemoryProperties:TVkPhysicalDeviceMemoryProperties;
       fFeatures:TVkPhysicalDeviceFeatures;
       fQueueFamilyProperties:TVkQueueFamilyPropertiesArray;
       fAvailableLayers:TVulkanAvailableLayers;
       fAvailableExtensions:TVulkanAvailableExtensions;
       fAvailableLayerNames:TStringList;
       fAvailableExtensionNames:TStringList;
      public
       constructor Create(const pInstance:TVulkanInstance;const pPhysicalDevice:TVkPhysicalDevice);
       destructor Destroy; override;
       function GetFormatProperties(const pFormat:TVkFormat):TVkFormatProperties;
       function GetImageFormatProperties(const pFormat:TVkFormat;
                                         const pType:TVkImageType;
                                         const pTiling:TVkImageTiling;
                                         const pUsageFlags:TVkImageUsageFlags;
                                         const pCreateFlags:TVkImageCreateFlags):TVkImageFormatProperties;
       function GetSparseImageFormatProperties(const pFormat:TVkFormat;
                                               const pType:TVkImageType;
                                               const pSamples:TVkSampleCountFlagBits;
                                               const pUsageFlags:TVkImageUsageFlags;
                                               const pTiling:TVkImageTiling):TVkSparseImageFormatPropertiesArray;
       function GetSurfaceSupport(const pQueueFamilyIndex:TVkUInt32;const pSurface:TVkSurfaceKHR):boolean;
       function GetSurfaceCapabilities(const pSurface:TVkSurfaceKHR):TVkSurfaceCapabilitiesKHR;
       function GetSurfaceFormats(const pSurface:TVkSurfaceKHR):TVkSurfaceFormatKHRArray;
       function GetSurfacePresentModes(const pSurface:TVkSurfaceKHR):TVkPresentModeKHRArray;
       function GetDisplayProperties:TVkDisplayPropertiesKHRArray;
       function GetDisplayPlaneProperties:TVkDisplayPlanePropertiesKHRArray;
       function GetDisplayPlaneSupportedDisplays(const pPlaneIndex:TVkUInt32):TVkDisplayKHRArray;
       function GetDisplayModeProperties(const pDisplay:TVkDisplayKHR):TVkDisplayModePropertiesKHRArray;
       function GetMemoryType(const pTypeBits:TVkUInt32;const pProperties:TVkFlags):TVkUInt32;
       function GetBestSupportedDepthFormat:TVkFormat;
       function GetQueueNodeIndex(const pSurface:TVkSurfaceKHR;const pQueueFlagBits:TVkQueueFlagBits):TVkInt32;
       function GetSurfaceFormat(const pSurface:TVkSurfaceKHR):TVkSurfaceFormatKHR;
       property Handle:TVkPhysicalDevice read fPhysicalDeviceHandle;
       property DeviceName:TVulkanCharString read fDeviceName;
       property Properties:TVkPhysicalDeviceProperties read fProperties;
       property MemoryProperties:TVkPhysicalDeviceMemoryProperties read fMemoryProperties;
       property Features:TVkPhysicalDeviceFeatures read fFeatures;
       property QueueFamilyProperties:TVkQueueFamilyPropertiesArray read fQueueFamilyProperties;
       property AvailableLayers:TVulkanAvailableLayers read fAvailableLayers;
       property AvailableExtensions:TVulkanAvailableExtensions read fAvailableExtensions;
       property AvailableLayerNames:TStringList read fAvailableLayerNames;
       property AvailableExtensionNames:TStringList read fAvailableExtensionNames;
     end;

     TVulkanPhysicalDeviceList=class(TVulkanObjectList)
      private
       function GetItem(const Index:TVkSizeInt):TVulkanPhysicalDevice;
       procedure SetItem(const Index:TVkSizeInt;const Item:TVulkanPhysicalDevice);
      public
       property Items[const Index:TVkSizeInt]:TVulkanPhysicalDevice read GetItem write SetItem; default;
     end;

     TVulkanSurface=class(TVulkanHandle)
      private
       fInstance:TVulkanInstance;
       fAllocationManager:TVulkanAllocationManager;
       fAllocationCallbacks:PVkAllocationCallbacks;
{$ifdef Windows}
       fSurfaceCreateInfo:TVkWin32SurfaceCreateInfoKHR;
{$endif}
       fSurfaceHandle:TVkSurfaceKHR;
      protected
      public
       constructor Create(const pInstance:TVulkanInstance;
                          {$ifdef Windows}const pInstanceHandle,pWindowHandle:THandle;{$endif}
                          const pAllocationManager:TVulkanAllocationManager=nil);
       destructor Destroy; override;
       procedure Initialize;
       property Handle:TVkSurfaceKHR read fSurfaceHandle;
     end;

     TVulkanDeviceQueueCreateInfo=class;

     TVulkanDeviceQueueCreateInfoList=class;

     TVulkanDevice=class(TVulkanHandle)
      private
       fInstance:TVulkanInstance;
       fPhysicalDevice:TVulkanPhysicalDevice;
       fSurface:TVulkanSurface;
       fDeviceQueueCreateInfoList:TVulkanDeviceQueueCreateInfoList;
       fDeviceQueueCreateInfos:TVkDeviceQueueCreateInfoArray;
       fDeviceCreateInfo:TVkDeviceCreateInfo;
       fEnabledLayerNames:TStringList;
       fEnabledExtensionNames:TStringList;
       fInstanceCreateInfo:TVkInstanceCreateInfo;
       fEnabledLayerNameStrings:array of TVulkanCharString;
       fEnabledExtensionNameStrings:array of TVulkanCharString;
       fRawEnabledLayerNameStrings:array of PVkChar;
       fRawEnabledExtensionNameStrings:array of PVkChar;
       fEnabledFeatures:TVkPhysicalDeviceLimits;
       fPointerToEnabledFeatures:PVkPhysicalDeviceLimits;
       fAllocationManager:TVulkanAllocationManager;
       fAllocationCallbacks:PVkAllocationCallbacks;
       fDeviceHandle:TVkDevice;
       fDeviceVulkan:TVulkan;
       fGraphicQueueFamilyIndex:TVkInt32;
       fComputeQueueFamilyIndex:TVkInt32;
       fTransferQueueFamilyIndex:TVkInt32;
       fSparseBindingQueueFamilyIndex:TVkInt32;
      protected
      public
       constructor Create(const pInstance:TVulkanInstance;
                          const pPhysicalDevice:TVulkanPhysicalDevice=nil;
                          const pSurface:TVulkanSurface=nil;
                          const pAllocationManager:TVulkanAllocationManager=nil);
       destructor Destroy; override;
       procedure AddQueue(const pQueueFamilyIndex:TVkUInt32;const pQueuePriorities:array of TVkFloat);
       procedure AddQueues(const pGraphic:boolean=true;
                           const pCompute:boolean=true;
                           const pTransfer:boolean=true;
                           const pSparseBinding:boolean=false);
       procedure Initialize;
       procedure WaitIdle;
       property PhysicalDevice:TVulkanPhysicalDevice read fPhysicalDevice;
       property EnabledLayerNames:TStringList read fEnabledLayerNames;
       property EnabledExtensionNames:TStringList read fEnabledExtensionNames;
       property EnabledFeatures:PVkPhysicalDeviceLimits read fPointerToEnabledFeatures;
       property Handle:TVkDevice read fDeviceHandle;
       property Commands:TVulkan read fDeviceVulkan;
       property GraphicQueueFamilyIndex:TVkInt32 read fGraphicQueueFamilyIndex;
       property ComputeQueueFamilyIndex:TVkInt32 read fComputeQueueFamilyIndex;
       property TransferQueueFamilyIndex:TVkInt32 read fTransferQueueFamilyIndex;
       property SparseBindingQueueFamilyIndex:TVkInt32 read fSparseBindingQueueFamilyIndex;
     end;

     TVulkanDeviceQueueCreateInfo=class(TVulkanObject)
      private
       fQueueFamilyIndex:TVkUInt32;
       fQueuePriorities:TVkFloatArray;
      public
       constructor Create(const pQueueFamilyIndex:TVkUInt32;const pQueuePriorities:array of TVkFloat);
       destructor Destroy; override;
       property QueueFamilyIndex:TVkUInt32 read fQueueFamilyIndex;
       property QueuePriorities:TVkFloatArray read fQueuePriorities;
     end;

     TVulkanDeviceQueueCreateInfoList=class(TVulkanObjectList)
      private
       function GetItem(const Index:TVkSizeInt):TVulkanDeviceQueueCreateInfo;
       procedure SetItem(const Index:TVkSizeInt;const Item:TVulkanDeviceQueueCreateInfo);
      public
       property Items[const Index:TVkSizeInt]:TVulkanDeviceQueueCreateInfo read GetItem write SetItem; default;
     end;

     TVulkanResource=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fOwnsResource:boolean;
      public
       constructor Create; reintroduce; virtual;
       destructor Destroy; override;
       procedure Clear; virtual;
       property Device:TVulkanDevice read fDevice write fDevice;
       property OwnsResource:boolean read fOwnsResource write fOwnsResource;
     end;

     TVulkanCommandPool=class(TVulkanHandle)
      private
       fDevice:TVulkanDevice;
       fQueueFamilyIndex:TVkUInt32;
       fFlags:TVkCommandPoolCreateFlags;
       fCommandPoolHandle:TVkCommandPool;
       fCommandPoolCreateInfo:TVkCommandPoolCreateInfo;
       fAllocationManager:TVulkanAllocationManager;
       fAllocationCallbacks:PVkAllocationCallbacks;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pQueueFamilyIndex:TVkUInt32;
                          const pFlags:TVkCommandPoolCreateFlags=TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT);
                          const pAllocationManager:TVulkanAllocationManager=nil);
       destructor Destroy; override;
       procedure Initialize;
       property QueueFamilyIndex:TVkUInt32 read fQueueFamilyIndex;
       property Handle:TVkCommandPool read fCommandPoolHandle;
     end;

     TVulkanDeviceMemoryManager=class;

     TVulkanDeviceMemoryChunkBlock=class;

     PVulkanDeviceMemoryChunkBlockRedBlackTreeKey=^TVulkanDeviceMemoryChunkBlockRedBlackTreeKey;
     TVulkanDeviceMemoryChunkBlockRedBlackTreeKey=TVkDeviceSize;

     PVulkanDeviceMemoryChunkBlockRedBlackTreeValue=^TVulkanDeviceMemoryChunkBlockRedBlackTreeValue;
     TVulkanDeviceMemoryChunkBlockRedBlackTreeValue=TVulkanDeviceMemoryChunkBlock;

     PVulkanDeviceMemoryChunkBlockRedBlackTreeNode=^TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
     TVulkanDeviceMemoryChunkBlockRedBlackTreeNode=class(TVulkanObject)
      private
       fKey:TVulkanDeviceMemoryChunkBlockRedBlackTreeKey;
       fValue:TVulkanDeviceMemoryChunkBlockRedBlackTreeValue;
       fLeft:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
       fRight:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
       fParent:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
       fColor:boolean;
      public
       constructor Create(const pKey:TVulkanDeviceMemoryChunkBlockRedBlackTreeKey=0;
                          const pValue:TVulkanDeviceMemoryChunkBlockRedBlackTreeValue=nil;
                          const pLeft:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode=nil;
                          const pRight:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode=nil;
                          const pParent:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode=nil;
                          const pColor:boolean=false);
       destructor Destroy; override;
       procedure Clear;
       function Minimum:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
       function Maximum:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
       function Predecessor:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
       function Successor:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
       property Key:TVulkanDeviceMemoryChunkBlockRedBlackTreeKey read fKey write fKey;
       property Value:TVulkanDeviceMemoryChunkBlockRedBlackTreeValue read fValue write fValue;
       property Left:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode read fLeft write fLeft;
       property Right:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode read fRight write fRight;
       property Parent:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode read fParent write fParent;
       property Color:boolean read fColor write fColor;
     end;

     TVulkanDeviceMemoryChunkBlockRedBlackTree=class(TVulkanObject)
      private
       fRoot:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
      protected
       procedure RotateLeft(x:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode);
       procedure RotateRight(x:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function Find(const pKey:TVulkanDeviceMemoryChunkBlockRedBlackTreeKey):TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
       function Insert(const pKey:TVulkanDeviceMemoryChunkBlockRedBlackTreeKey;
                       const pValue:TVulkanDeviceMemoryChunkBlockRedBlackTreeValue):TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
       procedure Remove(const pNode:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode);
       procedure Delete(const pKey:TVulkanDeviceMemoryChunkBlockRedBlackTreeKey);
       function LeftMost:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
       function RightMost:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
       property Root:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode read fRoot;
     end;

     TVulkanDeviceMemoryChunk=class;

     TVulkanDeviceMemoryChunkBlock=class(TVulkanObject)
      private
       fMemoryChunk:TVulkanDeviceMemoryChunk;
       fOffset:TVkDeviceSize;
       fSize:TVkDeviceSize;
       fUsed:boolean;
       fOffsetRedBlackTreeNode:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
       fSizeRedBlackTreeNode:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
      public
       constructor Create(const pMemoryChunk:TVulkanDeviceMemoryChunk;
                          const pOffset:TVkDeviceSize;
                          const pSize:TVkDeviceSize;
                          const pUsed:boolean);
       destructor Destroy; override;
       procedure Update(const pOffset:TVkDeviceSize;
                        const pSize:TVkDeviceSize;
                        const pUsed:boolean);
       property MemoryChunk:TVulkanDeviceMemoryChunk read fMemoryChunk;
       property Offset:TVkDeviceSize read fOffset;
       property Size:TVkDeviceSize read fSize;
       property Used:boolean read fUsed;
     end;

     TVulkanDeviceMemoryChunk=class(TVulkanObject)
      private
       fMemoryManager:TVulkanDeviceMemoryManager;
       fPreviousMemoryChunk:TVulkanDeviceMemoryChunk;
       fNextMemoryChunk:TVulkanDeviceMemoryChunk;
       fAlignment:TVkDeviceSize;
       fSize:TVkDeviceSize;
       fOffsetRedBlackTree:TVulkanDeviceMemoryChunkBlockRedBlackTree;
       fSizeRedBlackTree:TVulkanDeviceMemoryChunkBlockRedBlackTree;
       fMemoryTypeIndex:TVkUInt32;
       fMemoryPropertyFlags:TVkMemoryPropertyFlags;
       fMemoryHandle:TVkDeviceMemory;
       fMemory:PVkVoid;
      public
       constructor Create(const pMemoryManager:TVulkanDeviceMemoryManager;
                          const pSize:TVkDeviceSize;
                          const pMemoryPropertyFlags:TVkMemoryPropertyFlags;
                          const pMemoryHeapFlags:TVkMemoryHeapFlags=0);
       destructor Destroy; override;
       function AllocateMemory(out pOffset:TVkDeviceSize;const pSize:TVkDeviceSize):boolean;
       function ReallocateMemory(var pOffset:TVkDeviceSize;const pSize:TVkDeviceSize):boolean;
       function FreeMemory(const pOffset:TVkDeviceSize):boolean;
       property MemoryManager:TVulkanDeviceMemoryManager read fMemoryManager;
       property Size:TVkDeviceSize read fSize;
       property MemoryPropertyFlags:TVkMemoryPropertyFlags read fMemoryPropertyFlags;
       property MemoryTypeIndex:TVkUInt32 read fMemoryTypeIndex;
       property Handle:TVkDeviceMemory read fMemoryHandle;
       property Memory:PVkVoid read fMemory;
     end;

     TVulkanDeviceMemoryBlock=class(TVulkanObject)
      private
       fMemoryManager:TVulkanDeviceMemoryManager;
       fMemoryChunk:TVulkanDeviceMemoryChunk;
       fOffset:TVkDeviceSize;
       fSize:TVkDeviceSize;
       fPreviousMemoryBlock:TVulkanDeviceMemoryBlock;
       fNextMemoryBlock:TVulkanDeviceMemoryBlock;
      public
       constructor Create(const pMemoryManager:TVulkanDeviceMemoryManager;
                          const pMemoryChunk:TVulkanDeviceMemoryChunk;
                          const pOffset:TVkDeviceSize;
                          const pSize:TVkDeviceSize);
       destructor Destroy; override;
       property MemoryManager:TVulkanDeviceMemoryManager read fMemoryManager;
       property MemoryChunk:TVulkanDeviceMemoryChunk read fMemoryChunk;
       property Offset:TVkDeviceSize read fOffset;
       property Size:TVkDeviceSize read fSize;
     end;

     TVulkanDeviceMemoryManager=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fAllocationManager:TVulkanAllocationManager;
       fAllocationCallbacks:PVkAllocationCallbacks;
       fFirstMemoryChunk:TVulkanDeviceMemoryChunk;
       fLastMemoryChunk:TVulkanDeviceMemoryChunk;
       fFirstMemoryBlock:TVulkanDeviceMemoryBlock;
       fLastMemoryBlock:TVulkanDeviceMemoryBlock;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pAllocationManager:TVulkanAllocationManager=nil);
       destructor Destroy; override;
       function AllocateMemoryBlock(const pSize:TVkDeviceSize;
                                    const pMemoryPropertyFlags:TVkMemoryPropertyFlags):TVulkanDeviceMemoryBlock;
       function FreeMemoryBlock(const pMemoryBlock:TVulkanDeviceMemoryBlock):boolean;
     end;

     TVulkanBuffer=class(TVulkanHandle)
      private
       fDevice:TVulkanDevice;
       fSize:TVkDeviceSize;
       fBufferHandle:TVkBuffer;
       fMemoryHandle:TVkDeviceMemory;
       fAllocationManager:TVulkanAllocationManager;
       fAllocationCallbacks:PVkAllocationCallbacks;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pInitializationData:pointer;
                          const pSize:TVkDeviceSize;
                          const pAllocationManager:TVulkanAllocationManager=nil);
       destructor Destroy; override;
       function Map:PVkVoid;
       procedure Unmap;
       procedure Bind;
       property Handle:TVkBuffer read fBufferHandle;
       property Size:TVkDeviceSize read fSize;
       property MemoryHandle:TVkDeviceMemory read fMemoryHandle;
     end;

     TVulkanSwapChain=class(TVulkanHandle)
      private
       fDevice:TVulkanDevice;
       fSwapChainHandle:TVkSwapChainKHR;
       fSwapChainCreateInfo:TVkSwapchainCreateInfoKHR;
       fPointerToSwapChainCreateInfo:PVkSwapchainCreateInfoKHR;
       fAllocationManager:TVulkanAllocationManager;
       fAllocationCallbacks:PVkAllocationCallbacks;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pOldSwapChain:TVulkanSwapChain=nil;
                          const pAllocationManager:TVulkanAllocationManager=nil);
       destructor Destroy; override;
       procedure Initialize;
       property Handle:TVkSwapChainKHR read fSwapChainHandle;
       property SwapChainCreateInfo:PVkSwapchainCreateInfoKHR read fPointerToSwapChainCreateInfo;
     end;

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

function VulkanDeviceSizeRoundUpToPowerOfTwo(Value:TVkDeviceSize):TVkDeviceSize;
begin
 dec(Value);
 Value:=Value or (Value shr 1);
 Value:=Value or (Value shr 2);
 Value:=Value or (Value shr 4);
 Value:=Value or (Value shr 8);
 Value:=Value or (Value shr 16);
 Value:=Value or (Value shr 32);
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
  raise EVulkanResultException.Create(ResultCode);
 end;
end;

constructor EVulkanResultException.Create(const pResultCode:TVkResult);
begin
 fResultCode:=pResultCode;
 inherited Create(VulkanErrorToString(fResultCode));
end;

destructor EVulkanResultException.Destroy;
begin
 inherited Destroy;
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
  NewAllocated:=TVkSizeInt(VulkanRoundUpToPowerOfTwo(NewCount));
  if fAllocated<NewAllocated then begin
   if assigned(fMemory) then begin
    ReallocMem(fMemory,NewAllocated*fItemSize);
   end else begin
    GetMem(fMemory,NewAllocated*fItemSize);
   end;
   FillChar(pointer(TVkPtrInt(TVkPtrInt(fMemory)+(fAllocated*fItemSize)))^,(NewAllocated-fAllocated)*fItemSize,#0);
   fAllocated:=NewAllocated;
  end;
  Item:=fMemory;
  Index:=fCount;
  inc(TVkPtrUInt(Item),Index*fItemSize);
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
  inc(TVkPtrUInt(Item),Index*fItemSize);
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
    NewAllocated:=fAllocated shr 1;
    if assigned(fMemory) then begin
     ReallocMem(fMemory,NewAllocated*fItemSize);
    end else begin
     GetMem(fMemory,NewAllocated*fItemSize);
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
 fOwnObjects:=true;
 inherited Create(SizeOf(TVulkanObject));
end;

destructor TVulkanObjectList.Destroy;
begin
 inherited Destroy;
end;

procedure TVulkanObjectList.Clear;
begin
 inherited Clear;
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
 TVulkanObject(pointer(Item)):=nil;
end;

procedure TVulkanObjectList.FinalizeItem(var Item);
begin
 if fOwnObjects then begin
  TVulkanObject(pointer(Item)).Free;
 end;
 TVulkanObject(pointer(Item)):=nil;
end;

procedure TVulkanObjectList.CopyItem(const Source;var Destination);
begin
 TVulkanObject(pointer(Destination)):=TVulkanObject(pointer(Source));
end;

procedure TVulkanObjectList.ExchangeItem(var Source,Destination);
var Temporary:TVulkanObject;
begin
 Temporary:=TVulkanObject(pointer(Source));
 TVulkanObject(pointer(Source)):=TVulkanObject(pointer(Destination));
 TVulkanObject(pointer(Destination)):=Temporary;
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

constructor TVulkanInstance.Create(const pApplicationName:TVulkanCharString='Vulkan application';
                                      const pApplicationVersion:TVkUInt32=1;
                                      const pEngineName:TVulkanCharString='Vulkan engine';
                                      const pEngineVersion:TVkUInt32=1;
                                      const pAPIVersion:TVkUInt32=VK_API_VERSION_1_0;
                                      const pValidation:boolean=false;
                                      const pAllocationManager:TVulkanAllocationManager=nil);
var Index,SubIndex:TVkInt32;
    Count,SubCount:TVkUInt32;
    LayerProperties:TVkLayerPropertiesArray;
    LayerProperty:PVulkanAvailableLayer;
    ExtensionProperties:TVkExtensionPropertiesArray;
    ExtensionProperty:PVulkanAvailableExtension;
begin
 inherited Create;

 if not Vulkan.LoadVulkanLibrary then begin
  raise EVulkanException.Create('Vulkan load error');
 end;

 if not Vulkan.LoadVulkanGlobalCommands then begin
  raise EVulkanException.Create('Vulkan load error');
 end;

 fVulkan:=vk;

 fApplicationName:=pApplicationName;
 fEngineName:=pEngineName;

 fEnabledLayerNameStrings:=nil;
 fEnabledExtensionNameStrings:=nil;

 fRawEnabledLayerNameStrings:=nil;
 fRawEnabledExtensionNameStrings:=nil;

 fInstanceHandle:=VK_NULL_INSTANCE;

 fDebugReportCallbackEXT:=VK_NULL_HANDLE;

 fOnInstanceDebugReportCallback:=nil;

 fInstanceVulkan:=nil;

 fPhysicalDevices:=TVulkanPhysicalDeviceList.Create;
 fNeedToEnumeratePhysicalDevices:=false;

 FillChar(fApplicationInfo,SizeOf(TVkApplicationInfo),#0);
 fApplicationInfo.sType:=VK_STRUCTURE_TYPE_APPLICATION_INFO;
 fApplicationInfo.pNext:=nil;
 fApplicationInfo.pApplicationName:=PVkChar(fApplicationName);
 fApplicationInfo.applicationVersion:=pApplicationVersion;
 fApplicationInfo.pEngineName:=PVkChar(fEngineName);
 fApplicationInfo.engineVersion:=pEngineVersion;
 fApplicationInfo.apiVersion:=pAPIVersion;

 fValidation:=pValidation;

 fAllocationManager:=pAllocationManager;

 if assigned(pAllocationManager) then begin
  fAllocationCallbacks:=@pAllocationManager.fAllocationCallbacks;
 end else begin
  fAllocationCallbacks:=nil;
 end;

 fAvailableLayerNames:=TStringList.Create;
 fAvailableExtensionNames:=TStringList.Create;

 fEnabledLayerNames:=TStringList.Create;
 fEnabledExtensionNames:=TStringList.Create;

 LayerProperties:=nil;
 try
  fAvailableLayers:=nil;
  HandleResultCode(fVulkan.EnumerateInstanceLayerProperties(@Count,nil));
  if Count>0 then begin
   SetLength(LayerProperties,Count);
   SetLength(fAvailableLayers,Count);
   HandleResultCode(fVulkan.EnumerateInstanceLayerProperties(@Count,@LayerProperties[0]));
   for Index:=0 to Count-1 do begin
    LayerProperty:=@fAvailableLayers[Index];
    LayerProperty^.LayerName:=LayerProperties[Index].layerName;
    LayerProperty^.SpecVersion:=LayerProperties[Index].specVersion;
    LayerProperty^.ImplementationVersion:=LayerProperties[Index].implementationVersion;
    LayerProperty^.Description:=LayerProperties[Index].description;
    fAvailableLayerNames.Add(LayerProperty^.LayerName);
   end;
  end;
 finally
  SetLength(LayerProperties,0);
 end;

 ExtensionProperties:=nil;
 try
  fAvailableExtensions:=nil;
  Count:=0;
  for Index:=0 to length(fAvailableLayers)-1 do begin
   HandleResultCode(fVulkan.EnumerateInstanceExtensionProperties(PVkChar(fAvailableLayers[Index].layerName),@SubCount,nil));
   if SubCount>0 then begin
    if SubCount>TVkUInt32(length(ExtensionProperties)) then begin
     SetLength(ExtensionProperties,SubCount);
    end;
    SetLength(fAvailableExtensions,Count+SubCount);
    HandleResultCode(fVulkan.EnumerateInstanceExtensionProperties(PVkChar(fAvailableLayers[Index].layerName),@SubCount,@ExtensionProperties[0]));
    for SubIndex:=0 to SubCount-1 do begin
     ExtensionProperty:=@fAvailableExtensions[Count+TVkUInt32(SubIndex)];
     ExtensionProperty^.LayerIndex:=Index;
     ExtensionProperty^.ExtensionName:=ExtensionProperties[SubIndex].extensionName;
     ExtensionProperty^.SpecVersion:=ExtensionProperties[SubIndex].SpecVersion;
     if fAvailableExtensionNames.IndexOf(ExtensionProperty^.ExtensionName)<0 then begin
      fAvailableExtensionNames.Add(ExtensionProperty^.ExtensionName);
     end;
    end;
    inc(Count,SubCount);
   end;
  end;
 finally
  SetLength(ExtensionProperties,0);
 end;

 fOwnsHandle:=true;

 if fValidation then begin
{ if fAvailableExtensionNames.IndexOf('VK_LAYER_LUNARG_standard_validation')>=0 then begin
   fEnabledExtensionNames.Add(VK_EXT_DEBUG_REPORT_EXTENSION_NAME);
   fEnabledLayerNames.Add('VK_LAYER_LUNARG_standard_validation');
  end;{}
 end;

end;

destructor TVulkanInstance.Destroy;
begin
 if fDebugReportCallbackEXT<>VK_NULL_HANDLE then begin
  fInstanceVulkan.DestroyDebugReportCallbackEXT(fInstanceHandle,fDebugReportCallbackEXT,fAllocationCallbacks);
 end;
 fPhysicalDevices.Free;
 if fInstanceHandle<>VK_NULL_INSTANCE then begin
  fVulkan.DestroyInstance(fInstanceHandle,fAllocationCallbacks);
 end;
 fInstanceVulkan.Free;
 fApplicationName:='';
 fEngineName:='';
 fAvailableLayerNames.Free;
 fAvailableExtensionNames.Free;
 fEnabledLayerNames.Free;
 fEnabledExtensionNames.Free;
 SetLength(fAvailableLayers,0);
 SetLength(fAvailableExtensions,0);
 SetLength(fEnabledLayerNameStrings,0);
 SetLength(fRawEnabledLayerNameStrings,0);
 SetLength(fEnabledExtensionNameStrings,0);
 SetLength(fRawEnabledExtensionNameStrings,0);
 inherited Destroy;
end;

procedure TVulkanInstance.SetApplicationInfo(const NewApplicationInfo:TVkApplicationInfo);
begin
 fApplicationInfo:=NewApplicationInfo;
 fApplicationName:=fApplicationInfo.pApplicationName;
 fEngineName:=fApplicationInfo.pEngineName;
 fApplicationInfo.pApplicationName:=PVkChar(fApplicationName);
 fApplicationInfo.pEngineName:=PVkChar(fEngineName);
end;

function TVulkanInstance.GetApplicationName:TVulkanCharString;
begin
 result:=fApplicationName;
end;

procedure TVulkanInstance.SetApplicationName(const NewApplicationName:TVulkanCharString);
begin
 fApplicationName:=NewApplicationName;
 fApplicationInfo.pApplicationName:=PVkChar(fApplicationName);
end;

function TVulkanInstance.GetApplicationVersion:TVkUInt32;
begin
 result:=fApplicationInfo.applicationVersion;
end;

procedure TVulkanInstance.SetApplicationVersion(const NewApplicationVersion:TVkUInt32);
begin
 fApplicationInfo.applicationVersion:=NewApplicationVersion;
end;

function TVulkanInstance.GetEngineName:TVulkanCharString;
begin
 result:=fEngineName;
end;

procedure TVulkanInstance.SetEngineName(const NewEngineName:TVulkanCharString);
begin
 fEngineName:=NewEngineName;
 fApplicationInfo.pEngineName:=PVkChar(fEngineName);
end;

function TVulkanInstance.GetEngineVersion:TVkUInt32;
begin
 result:=fApplicationInfo.engineVersion;
end;

procedure TVulkanInstance.SetEngineVersion(const NewEngineVersion:TVkUInt32);
begin
 fApplicationInfo.engineVersion:=NewEngineVersion;
end;

function TVulkanInstance.GetAPIVersion:TVkUInt32;
begin
 result:=fApplicationInfo.apiVersion;
end;

procedure TVulkanInstance.SetAPIVersion(const NewAPIVersion:TVkUInt32);
begin
 fApplicationInfo.apiVersion:=NewAPIVersion;
end;

procedure TVulkanInstance.Initialize;
var i:TVkInt32;
    InstanceCommands:PVulkanCommands;
begin

 if fInstanceHandle=VK_NULL_INSTANCE then begin

  SetLength(fEnabledLayerNameStrings,fEnabledLayerNames.Count);
  SetLength(fRawEnabledLayerNameStrings,fEnabledLayerNames.Count);
  for i:=0 to fEnabledLayerNames.Count-1 do begin
   fEnabledLayerNameStrings[i]:=fEnabledLayerNames.Strings[i];
   fRawEnabledLayerNameStrings[i]:=PVkChar(fEnabledLayerNameStrings[i]);
  end;

  SetLength(fEnabledExtensionNameStrings,fEnabledExtensionNames.Count);
  SetLength(fRawEnabledExtensionNameStrings,fEnabledExtensionNames.Count);
  for i:=0 to fEnabledExtensionNames.Count-1 do begin
   fEnabledExtensionNameStrings[i]:=fEnabledExtensionNames.Strings[i];
   fRawEnabledExtensionNameStrings[i]:=PVkChar(fEnabledExtensionNameStrings[i]);
  end;

  FillChar(fInstanceCreateInfo,SizeOf(TVkInstanceCreateInfo),#0);
  fInstanceCreateInfo.sType:=VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
  if length(fEnabledLayerNameStrings)>0 then begin
   fInstanceCreateInfo.enabledLayerCount:=length(fEnabledLayerNameStrings);
   fInstanceCreateInfo.ppEnabledLayerNames:=@fRawEnabledLayerNameStrings[0];
  end;
  if length(fEnabledExtensionNameStrings)>0 then begin
   fInstanceCreateInfo.enabledExtensionCount:=length(fEnabledExtensionNameStrings);
   fInstanceCreateInfo.ppEnabledExtensionNames:=@fRawEnabledExtensionNameStrings[0];
  end;

  HandleResultCode(fVulkan.CreateInstance(@fInstanceCreateInfo,fAllocationCallbacks,@fInstanceHandle));

  GetMem(InstanceCommands,SizeOf(TVulkanCommands));
  try
   FillChar(InstanceCommands^,SizeOf(TVulkanCommands),#0);
   if LoadVulkanInstanceCommands(fVulkan.Commands.GetInstanceProcAddr,fInstanceHandle,InstanceCommands^) then begin
    fInstanceVulkan:=TVulkan.Create(InstanceCommands^);
   end else begin
    raise EVulkanException.Create('Couldn''t load vulkan instance commands');
   end;
  finally
   FreeMem(InstanceCommands);
  end;

  RefreshPhysicalDevices;

 end;
end;

procedure TVulkanInstance.RefreshPhysicalDevices;
var Index,SubIndex:TVkInt32;
    Count:TVkUInt32;
    PhysicalDevices:TVkPhysicalDeviceArray;
    PhysicalDevice:TVulkanPhysicalDevice;
    Found:boolean;
begin
 PhysicalDevices:=nil;
 try
  Count:=0;
  HandleResultCode(fInstanceVulkan.EnumeratePhysicalDevices(fInstanceHandle,@Count,nil));
  if Count>0 then begin
   SetLength(PhysicalDevices,Count);
   HandleResultCode(fInstanceVulkan.EnumeratePhysicalDevices(fInstanceHandle,@Count,@PhysicalDevices[0]));
   for Index:=fPhysicalDevices.Count-1 downto 0 do begin
    Found:=false;
    for SubIndex:=0 to Count-1 do begin
     if fPhysicalDevices[Index].fPhysicalDeviceHandle=PhysicalDevices[SubIndex] then begin
      Found:=true;
      break;
     end;
    end;
    if not Found then begin
     fPhysicalDevices.Delete(Index);
    end;
   end;
   for Index:=0 to Count-1 do begin
    Found:=false;
    for SubIndex:=0 to fPhysicalDevices.Count-1 do begin
     if fPhysicalDevices[SubIndex].fPhysicalDeviceHandle=PhysicalDevices[Index] then begin
      Found:=true;
      break;
     end;
    end;
    if not Found then begin
     PhysicalDevice:=TVulkanPhysicalDevice.Create(self,PhysicalDevices[Index]);
     fPhysicalDevices.Add(PhysicalDevice);
    end;
   end;
  end;
 finally
  SetLength(PhysicalDevices,0);
 end;
end;

function TVulkanInstanceDebugReportCallbackFunction(flags:TVkDebugReportFlagsEXT;objectType:TVkDebugReportObjectTypeEXT;object_:TVkUInt64;location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:PVkChar;const pMessage:PVkChar;pUserData:PVkVoid):TVkBool32; {$ifdef Windows}stdcall;{$else}{$ifdef Android}{$ifdef cpuarm}hardfloat;{$else}cdecl;{$endif}{$else}cdecl;{$endif}{$endif}
begin
 result:=TVulkanInstance(pUserData).DebugReportCallback(flags,objectType,object_,location,messageCode,pLayerPrefix,pMessage);
end;

function TVulkanInstance.DebugReportCallback(const flags:TVkDebugReportFlagsEXT;const objectType:TVkDebugReportObjectTypeEXT;const object_:TVkUInt64;const location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:TVulkaNCharString;const pMessage:TVulkanCharString):TVkBool32;
begin
 if assigned(fOnInstanceDebugReportCallback) then begin
  result:=fOnInstanceDebugReportCallback(flags,objectType,object_,location,messageCode,pLayerPrefix,pMessage);
 end else begin
  result:=VK_FALSE;
 end;
end;

procedure TVulkanInstance.InstallDebugReportCallback;
begin
 if (fDebugReportCallbackEXT=VK_NULL_HANDLE) and assigned(fInstanceVulkan.Commands.CreateDebugReportCallbackEXT) then begin
  FillChar(fDebugReportCallbackCreateInfoEXT,SizeOf(TVkDebugReportCallbackCreateInfoEXT),#0);
  fDebugReportCallbackCreateInfoEXT.sType:=VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT;
  fDebugReportCallbackCreateInfoEXT.flags:=TVkUInt32(VK_DEBUG_REPORT_ERROR_BIT_EXT) or TVkUInt32(VK_DEBUG_REPORT_WARNING_BIT_EXT) or TVkUInt32(VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT);
  fDebugReportCallbackCreateInfoEXT.pfnCallback:=@TVulkanInstanceDebugReportCallbackFunction;
  fDebugReportCallbackCreateInfoEXT.pUserData:=self;
  HandleResultCode(fInstanceVulkan.CreateDebugReportCallbackEXT(fInstanceHandle,@fDebugReportCallbackCreateInfoEXT,fAllocationCallbacks,@fDebugReportCallbackEXT));
 end;
end;

constructor TVulkanPhysicalDevice.Create(const pInstance:TVulkanInstance;const pPhysicalDevice:TVkPhysicalDevice);
var Index,SubIndex:TVkInt32;
    Count,SubCount:TVkUInt32;
    LayerProperties:TVkLayerPropertiesArray;
    LayerProperty:PVulkanAvailableLayer;
    ExtensionProperties:TVkExtensionPropertiesArray;
    ExtensionProperty:PVulkanAvailableExtension;
begin
 inherited Create;

 fInstance:=pInstance;

 fPhysicalDeviceHandle:=pPhysicalDevice;

 fInstance.Commands.GetPhysicalDeviceProperties(fPhysicalDeviceHandle,@fProperties);

 fDeviceName:=fProperties.deviceName;

 fInstance.Commands.GetPhysicalDeviceMemoryProperties(fPhysicalDeviceHandle,@fMemoryProperties);

 fInstance.Commands.GetPhysicalDeviceFeatures(fPhysicalDeviceHandle,@fFeatures);

 fQueueFamilyProperties:=nil;
 Count:=0;
 fInstance.Commands.GetPhysicalDeviceQueueFamilyProperties(fPhysicalDeviceHandle,@Count,nil);
 if Count>0 then begin
  try
   SetLength(fQueueFamilyProperties,Count);
   fInstance.fVulkan.GetPhysicalDeviceQueueFamilyProperties(fPhysicalDeviceHandle,@Count,@fQueueFamilyProperties[0]);
  except
   SetLength(fQueueFamilyProperties,0);
   raise;
  end;
 end;

 fAvailableLayerNames:=TStringList.Create;
 fAvailableExtensionNames:=TStringList.Create;

 LayerProperties:=nil;
 try
  fAvailableLayers:=nil;
  HandleResultCode(fInstance.fVulkan.EnumerateDeviceLayerProperties(fPhysicalDeviceHandle,@Count,nil));
  if Count>0 then begin
   SetLength(LayerProperties,Count);
   SetLength(fAvailableLayers,Count);
   HandleResultCode(fInstance.fVulkan.EnumerateDeviceLayerProperties(fPhysicalDeviceHandle,@Count,@LayerProperties[0]));
   for Index:=0 to Count-1 do begin
    LayerProperty:=@fAvailableLayers[Index];
    LayerProperty^.LayerName:=LayerProperties[Index].layerName;
    LayerProperty^.SpecVersion:=LayerProperties[Index].specVersion;
    LayerProperty^.ImplementationVersion:=LayerProperties[Index].implementationVersion;
    LayerProperty^.Description:=LayerProperties[Index].description;
    fAvailableLayerNames.Add(LayerProperty^.LayerName);
   end;
  end;
 finally
  SetLength(LayerProperties,0);
 end;

 ExtensionProperties:=nil;
 try
  fAvailableExtensions:=nil;
  Count:=0;
  for Index:=0 to length(fAvailableLayers)-1 do begin
   HandleResultCode(fInstance.fVulkan.EnumerateDeviceExtensionProperties(fPhysicalDeviceHandle,PVkChar(fAvailableLayers[Index].layerName),@SubCount,nil));
   if SubCount>0 then begin
    if SubCount>TVkUInt32(length(ExtensionProperties)) then begin
     SetLength(ExtensionProperties,SubCount);
    end;
    SetLength(fAvailableExtensions,Count+SubCount);
    HandleResultCode(fInstance.fVulkan.EnumerateDeviceExtensionProperties(fPhysicalDeviceHandle,PVkChar(fAvailableLayers[Index].layerName),@SubCount,@ExtensionProperties[0]));
    for SubIndex:=0 to SubCount-1 do begin
     ExtensionProperty:=@fAvailableExtensions[Count+TVkUInt32(SubIndex)];
     ExtensionProperty^.LayerIndex:=Index;
     ExtensionProperty^.ExtensionName:=ExtensionProperties[SubIndex].extensionName;
     ExtensionProperty^.SpecVersion:=ExtensionProperties[SubIndex].SpecVersion;
     if fAvailableExtensionNames.IndexOf(ExtensionProperty^.ExtensionName)<0 then begin
      fAvailableExtensionNames.Add(ExtensionProperty^.ExtensionName);
     end;
    end;
    inc(Count,SubCount);
   end;
  end;
 finally
  SetLength(ExtensionProperties,0);
 end;

end;

destructor TVulkanPhysicalDevice.Destroy;
begin
 SetLength(fQueueFamilyProperties,0);
 fAvailableLayerNames.Free;
 fAvailableExtensionNames.Free;
 SetLength(fAvailableLayers,0);
 SetLength(fAvailableExtensions,0);
 inherited Destroy;
end;

function TVulkanPhysicalDevice.GetFormatProperties(const pFormat:TVkFormat):TVkFormatProperties;
begin
 fInstance.Commands.GetPhysicalDeviceFormatProperties(fPhysicalDeviceHandle,pFormat,@result);
end;

function TVulkanPhysicalDevice.GetImageFormatProperties(const pFormat:TVkFormat;
                                                        const pType:TVkImageType;
                                                        const pTiling:TVkImageTiling;
                                                        const pUsageFlags:TVkImageUsageFlags;
                                                        const pCreateFlags:TVkImageCreateFlags):TVkImageFormatProperties;
begin
 fInstance.Commands.GetPhysicalDeviceImageFormatProperties(fPhysicalDeviceHandle,pFormat,pType,pTiling,pUsageFlags,pCreateFlags,@result);
end;

function TVulkanPhysicalDevice.GetSparseImageFormatProperties(const pFormat:TVkFormat;
                                                              const pType:TVkImageType;
                                                              const pSamples:TVkSampleCountFlagBits;
                                                              const pUsageFlags:TVkImageUsageFlags;
                                                              const pTiling:TVkImageTiling):TVkSparseImageFormatPropertiesArray;
var Count:TVkUInt32;
begin
 result:=nil;
 Count:=0;
 fInstance.Commands.GetPhysicalDeviceSparseImageFormatProperties(fPhysicalDeviceHandle,pFormat,pType,pSamples,pUsageFlags,pTiling,@Count,nil);
 if Count>0 then begin
  SetLength(result,Count);
  fInstance.Commands.GetPhysicalDeviceSparseImageFormatProperties(fPhysicalDeviceHandle,pFormat,pType,pSamples,pUsageFlags,pTiling,@Count,@result[0]);
 end;
end;

function TVulkanPhysicalDevice.GetSurfaceSupport(const pQueueFamilyIndex:TVkUInt32;const pSurface:TVkSurfaceKHR):boolean;
var Supported:TVkBool32;
begin
 Supported:=0;
 fInstance.Commands.GetPhysicalDeviceSurfaceSupportKHR(fPhysicalDeviceHandle,pQueueFamilyIndex,pSurface,@Supported);
 result:=Supported<>0;
end;

function TVulkanPhysicalDevice.GetSurfaceCapabilities(const pSurface:TVkSurfaceKHR):TVkSurfaceCapabilitiesKHR;
begin
 fInstance.Commands.GetPhysicalDeviceSurfaceCapabilitiesKHR(fPhysicalDeviceHandle,pSurface,@result);
end;

function TVulkanPhysicalDevice.GetSurfaceFormats(const pSurface:TVkSurfaceKHR):TVkSurfaceFormatKHRArray;
var Count:TVKUInt32;
begin
 result:=nil;
 Count:=0;
 if fInstance.Commands.GetPhysicalDeviceSurfaceFormatsKHR(fPhysicalDeviceHandle,pSurface,@Count,nil)=VK_SUCCESS then begin
  if Count>0 then begin
   try
    SetLength(result,Count);
    HandleResultCode(fInstance.Commands.GetPhysicalDeviceSurfaceFormatsKHR(fPhysicalDeviceHandle,PSurface,@Count,@result[0]));
   except
    SetLength(result,0);
    raise;
   end;
  end;
 end;
end;

function TVulkanPhysicalDevice.GetSurfacePresentModes(const pSurface:TVkSurfaceKHR):TVkPresentModeKHRArray;
var Count:TVKUInt32;
begin
 result:=nil;
 Count:=0;
 if fInstance.Commands.GetPhysicalDeviceSurfacePresentModesKHR(fPhysicalDeviceHandle,pSurface,@Count,nil)=VK_SUCCESS then begin
  if Count>0 then begin
   try
    SetLength(result,Count);
    HandleResultCode(fInstance.Commands.GetPhysicalDeviceSurfacePresentModesKHR(fPhysicalDeviceHandle,PSurface,@Count,@result[0]));
   except
    SetLength(result,0);
    raise;
   end;
  end;
 end;
end;

function TVulkanPhysicalDevice.GetDisplayProperties:TVkDisplayPropertiesKHRArray;
var Count:TVKUInt32;
begin
 result:=nil;
 Count:=0;
 if fInstance.Commands.GetPhysicalDeviceDisplayPropertiesKHR(fPhysicalDeviceHandle,@Count,nil)=VK_SUCCESS then begin
  if Count>0 then begin
   try
    SetLength(result,Count);
    HandleResultCode(fInstance.Commands.GetPhysicalDeviceDisplayPropertiesKHR(fPhysicalDeviceHandle,@Count,@result[0]));
   except
    SetLength(result,0);
    raise;
   end;
  end;
 end;
end;

function TVulkanPhysicalDevice.GetDisplayPlaneProperties:TVkDisplayPlanePropertiesKHRArray;
var Count:TVKUInt32;
begin
 result:=nil;
 Count:=0;
 if fInstance.Commands.GetPhysicalDeviceDisplayPlanePropertiesKHR(fPhysicalDeviceHandle,@Count,nil)=VK_SUCCESS then begin
  if Count>0 then begin
   try
    SetLength(result,Count);
    HandleResultCode(fInstance.Commands.GetPhysicalDeviceDisplayPlanePropertiesKHR(fPhysicalDeviceHandle,@Count,@result[0]));
   except
    SetLength(result,0);
    raise;
   end;
  end;
 end;
end;

function TVulkanPhysicalDevice.GetDisplayPlaneSupportedDisplays(const pPlaneIndex:TVkUInt32):TVkDisplayKHRArray;
var Count:TVKUInt32;
begin
 result:=nil;
 Count:=0;
 if fInstance.Commands.GetDisplayPlaneSupportedDisplaysKHR(fPhysicalDeviceHandle,pPlaneIndex,@Count,nil)=VK_SUCCESS then begin
  if Count>0 then begin
   try
    SetLength(result,Count);
    HandleResultCode(fInstance.Commands.GetDisplayPlaneSupportedDisplaysKHR(fPhysicalDeviceHandle,pPlaneIndex,@Count,@result[0]));
   except
    SetLength(result,0);
    raise;
   end;
  end;
 end;
end;

function TVulkanPhysicalDevice.GetDisplayModeProperties(const pDisplay:TVkDisplayKHR):TVkDisplayModePropertiesKHRArray;
var Count:TVKUInt32;
begin
 result:=nil;
 Count:=0;
 if fInstance.Commands.GetDisplayModePropertiesKHR(fPhysicalDeviceHandle,pDisplay,@Count,nil)=VK_SUCCESS then begin
  if Count>0 then begin
   try
    SetLength(result,Count);
    HandleResultCode(fInstance.Commands.GetDisplayModePropertiesKHR(fPhysicalDeviceHandle,pDisplay,@Count,@result[0]));
   except
    SetLength(result,0);
    raise;
   end;
  end;
 end;
end;

function TVulkanPhysicalDevice.GetMemoryType(const pTypeBits:TVkUInt32;const pProperties:TVkFlags):TVkUInt32;
var i:TVkUInt32;
    DeviceMemoryProperties:TVkPhysicalDeviceMemoryProperties;
begin
 result:=not TVkUInt32(0);
 vkGetPhysicalDeviceMemoryProperties(fPhysicalDeviceHandle,@DeviceMemoryProperties);
 for i:=0 to 31 do begin
  if (pTypeBits and (TVkUInt32(1) shl i))<>0 then begin
   if (DeviceMemoryProperties.MemoryTypes[i].PropertyFlags and pProperties)=pProperties then begin
    result:=i;
    exit;
   end;
  end;
 end;
end;

function TVulkanPhysicalDevice.GetBestSupportedDepthFormat:TVkFormat;
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
  fInstance.fVulkan.GetPhysicalDeviceFormatProperties(fPhysicalDeviceHandle,Format,@FormatProperties);
  if (FormatProperties.OptimalTilingFeatures and TVkFormatFeatureFlags(VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT))<>0 then begin
   result:=Format;
   exit;
  end;
 end;
end;

function TVulkanPhysicalDevice.GetQueueNodeIndex(const pSurface:TVkSurfaceKHR;const pQueueFlagBits:TVkQueueFlagBits):TVkInt32;
var Index:TVkInt32;
    QueueCount:TVkUInt32;
    QueueProperties:array of TVkQueueFamilyProperties;
    SupportsPresent:TVkBool32;
begin
 result:=-1;
 fInstance.fVulkan.GetPhysicalDeviceQueueFamilyProperties(fPhysicalDeviceHandle,@QueueCount,nil);
 QueueProperties:=nil;
 SetLength(QueueProperties,QueueCount);
 try
  fInstance.fVulkan.GetPhysicalDeviceQueueFamilyProperties(fPhysicalDeviceHandle,@QueueCount,@QueueProperties[0]);
  for Index:=0 to QueueCount-1 do begin
   fInstance.fVulkan.GetPhysicalDeviceSurfaceSupportKHR(fPhysicalDeviceHandle,Index,pSurface,@SupportsPresent);
   if ((QueueProperties[Index].QueueFlags and TVkQueueFlags(pQueueFlagBits))<>0) and (SupportsPresent=VK_TRUE) then begin
    result:=Index;
    break;
   end;
  end;
 finally
  SetLength(QueueProperties,0);
 end;
end;

function TVulkanPhysicalDevice.GetSurfaceFormat(const pSurface:TVkSurfaceKHR):TVkSurfaceFormatKHR;
var FormatCount:TVkUInt32;
    SurfaceFormats:TVkSurfaceFormatKHRArray;
begin
 SurfaceFormats:=nil;
 try

  FormatCount:=0;
  HandleResultCode(vkGetPhysicalDeviceSurfaceFormatsKHR(fPhysicalDeviceHandle,pSurface,@FormatCount,nil));

  if FormatCount>0 then begin
   SetLength(SurfaceFormats,FormatCount);
   HandleResultCode(vkGetPhysicalDeviceSurfaceFormatsKHR(fPhysicalDeviceHandle,pSurface,@FormatCount,@SurfaceFormats[0]));
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

function TVulkanPhysicalDeviceList.GetItem(const Index:TVkSizeInt):TVulkanPhysicalDevice;
begin
 result:=TVulkanPhysicalDevice(inherited Items[Index]);
end;

procedure TVulkanPhysicalDeviceList.SetItem(const Index:TVkSizeInt;const Item:TVulkanPhysicalDevice);
begin
 inherited Items[Index]:=Item;
end;

constructor TVulkanSurface.Create(const pInstance:TVulkanInstance;
                                  {$ifdef Windows}const pInstanceHandle,pWindowHandle:THandle;{$endif}
                                  const pAllocationManager:TVulkanAllocationManager=nil);
begin
 inherited Create;

 fInstance:=pInstance;

 if assigned(pAllocationManager) then begin
  fAllocationManager:=pAllocationManager;
 end else begin
  fAllocationManager:=fInstance.fAllocationManager;
 end;

 if assigned(fAllocationManager) then begin
  fAllocationCallbacks:=@fAllocationManager.fAllocationCallbacks;
 end else begin
  fAllocationCallbacks:=nil;
 end;

 fSurfaceHandle:=VK_NULL_HANDLE;

{$ifdef Windows}
 FillChar(fSurfaceCreateInfo,SizeOf(TVkWin32SurfaceCreateInfoKHR),#0);
 fSurfaceCreateInfo.sType:=VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
 fSurfaceCreateInfo.hinstance_:=pInstanceHandle;
 fSurfaceCreateInfo.hwnd_:=pWindowHandle;
{$endif}

end;

destructor TVulkanSurface.Destroy;
begin
 if (fSurfaceHandle<>VK_NULL_HANDLE) and fOwnsHandle then begin
  fInstance.fVulkan.DestroySurfaceKHR(fInstance.fInstanceHandle,fSurfaceHandle,fAllocationCallbacks);
 end;
 inherited Destroy;
end;

procedure TVulkanSurface.Initialize;
begin
 if fSurfaceHandle=VK_NULL_HANDLE then begin
{$ifdef Windows}
  HandleResultCode(fInstance.fVulkan.CreateWin32SurfaceKHR(fInstance.fInstanceHandle,@fSurfaceCreateInfo,fAllocationCallbacks,@fSurfaceHandle));
{$else}
  HandleResultCode(VK_ERROR_INCOMPATIBLE_DRIVER);
{$endif}
 end;
end;

constructor TVulkanDevice.Create(const pInstance:TVulkanInstance;
                                 const pPhysicalDevice:TVulkanPhysicalDevice=nil;
                                 const pSurface:TVulkanSurface=nil;
                                 const pAllocationManager:TVulkanAllocationManager=nil);
var Index,SubIndex:TVkInt32;
    Count,SubCount:TVkUInt32;
    BestPhysicalDevice,CurrentPhysicalDevice:TVulkanPhysicalDevice;
    BestScore,CurrentScore,Temp:int64;
    OK:boolean;
begin
 inherited Create;

 fInstance:=pInstance;

 fDeviceQueueCreateInfoList:=TVulkanDeviceQueueCreateInfoList.Create;

 fDeviceQueueCreateInfos:=nil;

 fEnabledLayerNameStrings:=nil;
 fEnabledExtensionNameStrings:=nil;

 fRawEnabledLayerNameStrings:=nil;
 fRawEnabledExtensionNameStrings:=nil;

 if assigned(pAllocationManager) then begin
  fAllocationManager:=pAllocationManager;
 end else begin
  fAllocationManager:=fInstance.fAllocationManager;
 end;

 if assigned(fAllocationManager) then begin
  fAllocationCallbacks:=@fAllocationManager.fAllocationCallbacks;
 end else begin
  fAllocationCallbacks:=nil;
 end;

 fSurface:=pSurface;

 fDeviceHandle:=VK_NULL_HANDLE;

 fDeviceVulkan:=nil;

 fGraphicQueueFamilyIndex:=-1;
 fComputeQueueFamilyIndex:=-1;
 fTransferQueueFamilyIndex:=-1;
 fSparseBindingQueueFamilyIndex:=-1;

 if assigned(pPhysicalDevice) then begin
  fPhysicalDevice:=pPhysicalDevice;
 end else begin
  BestPhysicalDevice:=nil;
  BestScore:=-$7fffffffffffffff;
  for Index:=0 to fInstance.fPhysicalDevices.Count-1 do begin
   CurrentPhysicalDevice:=fInstance.fPhysicalDevices[Index];
   CurrentScore:=0;
   case CurrentPhysicalDevice.fProperties.deviceType of
    VK_PHYSICAL_DEVICE_TYPE_OTHER:begin
     CurrentScore:=CurrentScore or (int64(1) shl 60);
    end;
    VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU:begin
     CurrentScore:=CurrentScore or (int64(3) shl 60);
    end;
    VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU:begin
     CurrentScore:=CurrentScore or (int64(4) shl 60);
    end;
    VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU:begin
     CurrentScore:=CurrentScore or (int64(2) shl 60);
    end;
    else begin
     CurrentScore:=CurrentScore or (int64(0) shl 60);
    end;
   end;
   OK:=false;
   for SubIndex:=0 to length(CurrentPhysicalDevice.fQueueFamilyProperties)-1 do begin
    if assigned(pSurface) and not CurrentPhysicalDevice.GetSurfaceSupport(SubIndex,pSurface.fSurfaceHandle) then begin
     continue;
    end;
    OK:=true;
    Temp:=0;
    if (CurrentPhysicalDevice.fQueueFamilyProperties[SubIndex].queueFlags and TVkInt32(VK_QUEUE_GRAPHICS_BIT))<>0 then begin
     inc(Temp);
    end;
    if (CurrentPhysicalDevice.fQueueFamilyProperties[SubIndex].queueFlags and TVkInt32(VK_QUEUE_COMPUTE_BIT))<>0 then begin
     inc(Temp);
    end;
    if (CurrentPhysicalDevice.fQueueFamilyProperties[SubIndex].queueFlags and TVkInt32(VK_QUEUE_TRANSFER_BIT))<>0 then begin
     inc(Temp);
    end;
    if (CurrentPhysicalDevice.fQueueFamilyProperties[SubIndex].queueFlags and TVkInt32(VK_QUEUE_SPARSE_BINDING_BIT))<>0 then begin
     inc(Temp);
    end;
    CurrentScore:=CurrentScore or (int64(Temp) shl 55);
   end;
   if not OK then begin
    continue;
   end;
   if (BestScore>CurrentScore) or not assigned(BestPhysicalDevice) then begin
    BestPhysicalDevice:=CurrentPhysicalDevice;
    BestScore:=CurrentScore;
   end;
  end;
  if assigned(BestPhysicalDevice) then begin
   fPhysicalDevice:=BestPhysicalDevice;
  end else begin
   raise EVulkanException.Create('No suitable vulkan device found');
  end;
 end;

 fEnabledLayerNames:=TStringList.Create;
 fEnabledExtensionNames:=TStringList.Create;

 fEnabledFeatures:=fPhysicalDevice.fProperties.limits;

 fPointerToEnabledFeatures:=@fEnabledFeatures;

end;

destructor TVulkanDevice.Destroy;
begin
 fDeviceVulkan.Free;
 if (fDeviceHandle<>VK_NULL_HANDLE) and fOwnsHandle then begin
  fInstance.Commands.DestroyDevice(fDeviceHandle,fAllocationCallbacks);
 end;
 SetLength(fDeviceQueueCreateInfos,0);
 fDeviceQueueCreateInfoList.Free;
 fEnabledLayerNames.Free;
 fEnabledExtensionNames.Free;
 SetLength(fEnabledLayerNameStrings,0);
 SetLength(fRawEnabledLayerNameStrings,0);
 SetLength(fEnabledExtensionNameStrings,0);
 SetLength(fRawEnabledExtensionNameStrings,0);
 inherited Destroy;
end;

procedure TVulkanDevice.AddQueue(const pQueueFamilyIndex:TVkUInt32;const pQueuePriorities:array of TVkFloat);
var QueueFamilyProperties:PVkQueueFamilyProperties;
begin           
 if pQueueFamilyIndex<TVkUInt32(length(fPhysicalDevice.fQueueFamilyProperties)) then begin
  if assigned(fSurface) and not fPhysicalDevice.GetSurfaceSupport(pQueueFamilyIndex,fSurface.fSurfaceHandle) then begin
   raise EVulkanException.Create('Surface doesn''t support queue family index '+IntToStr(pQueueFamilyIndex));
  end;
  QueueFamilyProperties:=@fPhysicalDevice.fQueueFamilyProperties[pQueueFamilyIndex];
  if ((QueueFamilyProperties.queueFlags and TVKUInt32(VK_QUEUE_GRAPHICS_BIT))<>0) and (fGraphicQueueFamilyIndex<0) then begin
   fGraphicQueueFamilyIndex:=pQueueFamilyIndex;
  end;
  if ((QueueFamilyProperties.queueFlags and TVKUInt32(VK_QUEUE_COMPUTE_BIT))<>0) and (fComputeQueueFamilyIndex<0) then begin
   fComputeQueueFamilyIndex:=pQueueFamilyIndex;
  end;
  if ((QueueFamilyProperties.queueFlags and TVKUInt32(VK_QUEUE_TRANSFER_BIT))<>0) and (fTransferQueueFamilyIndex<0) then begin
   fTransferQueueFamilyIndex:=pQueueFamilyIndex;
  end;
  if ((QueueFamilyProperties.queueFlags and TVKUInt32(VK_QUEUE_SPARSE_BINDING_BIT))<>0) and (fSparseBindingQueueFamilyIndex<0) then begin
   fSparseBindingQueueFamilyIndex:=pQueueFamilyIndex;
  end;
  fDeviceQueueCreateInfoList.Add(TVulkanDeviceQueueCreateInfo.Create(pQueueFamilyIndex,pQueuePriorities));
 end else begin
  raise EVulkanException.Create('Queue family index out of bounds');
 end;
end;

procedure TVulkanDevice.AddQueues(const pGraphic:boolean=true;
                                  const pCompute:boolean=true;
                                  const pTransfer:boolean=true;
                                  const pSparseBinding:boolean=false);
var Index:TVkInt32;
    DoAdd:boolean;
    QueueFamilyProperties:PVkQueueFamilyProperties;
begin
 for Index:=0 to length(fPhysicalDevice.fQueueFamilyProperties)-1 do begin
  DoAdd:=false;
  QueueFamilyProperties:=@fPhysicalDevice.fQueueFamilyProperties[Index];
  if assigned(fSurface) and not fPhysicalDevice.GetSurfaceSupport(Index,fSurface.fSurfaceHandle) then begin
   continue;
  end;
  if ((QueueFamilyProperties.queueFlags and TVKUInt32(VK_QUEUE_GRAPHICS_BIT))<>0) and (fGraphicQueueFamilyIndex<0) then begin
   fGraphicQueueFamilyIndex:=Index;
   if pGraphic then begin
    DoAdd:=true;
   end;
  end;
  if ((QueueFamilyProperties.queueFlags and TVKUInt32(VK_QUEUE_COMPUTE_BIT))<>0) and (fComputeQueueFamilyIndex<0) then begin
   fComputeQueueFamilyIndex:=Index;
   if pCompute then begin
    DoAdd:=true;
   end;
  end;
  if ((QueueFamilyProperties.queueFlags and TVKUInt32(VK_QUEUE_TRANSFER_BIT))<>0) and (fTransferQueueFamilyIndex<0) then begin
   fTransferQueueFamilyIndex:=Index;
   if pTransfer then begin
    DoAdd:=true;
   end;
  end;
  if ((QueueFamilyProperties.queueFlags and TVKUInt32(VK_QUEUE_SPARSE_BINDING_BIT))<>0) and (fSparseBindingQueueFamilyIndex<0) then begin
   fSparseBindingQueueFamilyIndex:=Index;
   if pSparseBinding then begin
    DoAdd:=true;
   end;
  end;
  if DoAdd then begin
   fDeviceQueueCreateInfoList.Add(TVulkanDeviceQueueCreateInfo.Create(Index,[1.0]));
  end;
 end;
 if ((fGraphicQueueFamilyIndex<0) and pGraphic) or
    ((fComputeQueueFamilyIndex<0) and pCompute) or
    ((fTransferQueueFamilyIndex<0) and pTransfer) or
    ((fSparseBindingQueueFamilyIndex<0) and pSparseBinding) then begin
  raise EVulkanException.Create('Only unsatisfactory device queue families available');
 end;
end;

procedure TVulkanDevice.Initialize;
var Index:TVkInt32;
    DeviceQueueCreateInfo:PVkDeviceQueueCreateInfo;
    SrcDeviceQueueCreateInfo:TVulkanDeviceQueueCreateInfo;
    DeviceCommands:PVulkanCommands;
begin
 if fDeviceHandle=VK_NULL_HANDLE then begin

  SetLength(fEnabledLayerNameStrings,fEnabledLayerNames.Count);
  SetLength(fRawEnabledLayerNameStrings,fEnabledLayerNames.Count);
  for Index:=0 to fEnabledLayerNames.Count-1 do begin
   fEnabledLayerNameStrings[Index]:=fEnabledLayerNames.Strings[Index];
   fRawEnabledLayerNameStrings[Index]:=PVkChar(fEnabledLayerNameStrings[Index]);
  end;

  SetLength(fEnabledExtensionNameStrings,fEnabledExtensionNames.Count);
  SetLength(fRawEnabledExtensionNameStrings,fEnabledExtensionNames.Count);
  for Index:=0 to fEnabledExtensionNames.Count-1 do begin
   fEnabledExtensionNameStrings[Index]:=fEnabledExtensionNames.Strings[Index];
   fRawEnabledExtensionNameStrings[Index]:=PVkChar(fEnabledExtensionNameStrings[Index]);
  end;

  SetLength(fDeviceQueueCreateInfos,fDeviceQueueCreateInfoList.Count);
  for Index:=0 to fDeviceQueueCreateInfoList.Count-1 do begin
   SrcDeviceQueueCreateInfo:=fDeviceQueueCreateInfoList[Index];
   DeviceQueueCreateInfo:=@fDeviceQueueCreateInfos[Index];
   FillChar(DeviceQueueCreateInfo^,SizeOf(TVkDeviceQueueCreateInfo),#0);
   DeviceQueueCreateInfo^.sType:=VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
   DeviceQueueCreateInfo^.queueFamilyIndex:=SrcDeviceQueueCreateInfo.fQueueFamilyIndex;
   DeviceQueueCreateInfo^.queueCount:=length(SrcDeviceQueueCreateInfo.fQueuePriorities);
   if DeviceQueueCreateInfo^.queueCount>0 then begin
    DeviceQueueCreateInfo^.pQueuePriorities:=@SrcDeviceQueueCreateInfo.fQueuePriorities[0];
   end;
  end;

  FillChar(fDeviceCreateInfo,SizeOf(TVkDeviceCreateInfo),#0);
  fDeviceCreateInfo.sType:=VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
  if length(fDeviceQueueCreateInfos)>0 then begin
   fDeviceCreateInfo.queueCreateInfoCount:=length(fDeviceQueueCreateInfos);
   fDeviceCreateInfo.pQueueCreateInfos:=@fDeviceQueueCreateInfos[0];
  end;
  if length(fEnabledLayerNameStrings)>0 then begin
   fDeviceCreateInfo.enabledLayerCount:=length(fEnabledLayerNameStrings);
   fDeviceCreateInfo.ppEnabledLayerNames:=@fRawEnabledLayerNameStrings[0];
  end;
  if length(fEnabledExtensionNameStrings)>0 then begin
   fDeviceCreateInfo.enabledExtensionCount:=length(fEnabledExtensionNameStrings);
   fDeviceCreateInfo.ppEnabledExtensionNames:=@fRawEnabledExtensionNameStrings[0];
  end;
  fDeviceCreateInfo.pEnabledFeatures:=@fEnabledFeatures;
  HandleResultCode(fInstance.Commands.CreateDevice(fPhysicalDevice.fPhysicalDeviceHandle,@fDeviceCreateInfo,fAllocationCallbacks,@fDeviceHandle));

  GetMem(DeviceCommands,SizeOf(TVulkanCommands));
  try
   FillChar(DeviceCommands^,SizeOf(TVulkanCommands),#0);
   if LoadVulkanDeviceCommands(fInstance.Commands.Commands.GetDeviceProcAddr,fDeviceHandle,DeviceCommands^) then begin
    fDeviceVulkan:=TVulkan.Create(DeviceCommands^);
   end else begin
    raise EVulkanException.Create('Couldn''t load vulkan device commands');
   end;
  finally
   FreeMem(DeviceCommands);
  end;

 end;
end;

procedure TVulkanDevice.WaitIdle;
begin
 fDeviceVulkan.DeviceWaitIdle(fDeviceHandle);
end;

constructor TVulkanDeviceQueueCreateInfo.Create(const pQueueFamilyIndex:TVkUInt32;const pQueuePriorities:array of TVkFloat);
begin
 inherited Create;
 fQueueFamilyIndex:=pQueueFamilyIndex;
 SetLength(fQueuePriorities,length(pQueuePriorities));
 if length(pQueuePriorities)>0 then begin
  Move(pQueuePriorities[0],fQueuePriorities[0],length(pQueuePriorities)*SizeOf(TVkFloat));
 end;
end;

destructor TVulkanDeviceQueueCreateInfo.Destroy;
begin
 SetLength(fQueuePriorities,0);
 inherited Destroy;
end;

function TVulkanDeviceQueueCreateInfoList.GetItem(const Index:TVkSizeInt):TVulkanDeviceQueueCreateInfo;
begin
 result:=TVulkanDeviceQueueCreateInfo(inherited Items[Index]);
end;

procedure TVulkanDeviceQueueCreateInfoList.SetItem(const Index:TVkSizeInt;const Item:TVulkanDeviceQueueCreateInfo);
begin
 inherited Items[Index]:=Item;
end;

constructor TVulkanResource.Create;
begin
 inherited Create;
 fDevice:=nil;
 fOwnsResource:=false;
end;

destructor TVulkanResource.Destroy;
begin
 inherited Destroy;
end;

procedure TVulkanResource.Clear;
begin
 fDevice:=nil;
 fOwnsResource:=false;
end;

constructor TVulkanCommandPool.Create(const pDevice:TVulkanDevice;
                                      const pQueueFamilyIndex:TVkUInt32;
                                      const pFlags:TVkCommandPoolCreateFlags=TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT);
                                      const pAllocationManager:TVulkanAllocationManager=nil);
begin
 inherited Create;

 fDevice:=pDevice;

 if assigned(pAllocationManager) then begin
  fAllocationManager:=pAllocationManager;
 end else begin
  fAllocationManager:=fDevice.fAllocationManager;
 end;

 if assigned(fAllocationManager) then begin
  fAllocationCallbacks:=@fAllocationManager.fAllocationCallbacks;
 end else begin
  fAllocationCallbacks:=nil;
 end;

 fQueueFamilyIndex:=pQueueFamilyIndex;

 fFlags:=pFlags;

 fCommandPoolHandle:=VK_NULL_HANDLE;

 fOwnsHandle:=true;

end;

destructor TVulkanCommandPool.Destroy;
begin
 if (fCommandPoolHandle<>VK_NULL_HANDLE) and fOwnsHandle then begin
  fDevice.fDeviceVulkan.DestroyCommandPool(fDevice.Handle,fCommandPoolHandle,fAllocationCallbacks);
 end;
 inherited Destroy;
end;

procedure TVulkanCommandPool.Initialize;
begin
 if (fCommandPoolHandle=VK_NULL_HANDLE) and fOwnsHandle then begin
  FillChar(fCommandPoolCreateInfo,SizeOf(TVkCommandPoolCreateInfo),#0);
  fCommandPoolCreateInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
  fCommandPoolCreateInfo.queueFamilyIndex:=fQueueFamilyIndex;
  fCommandPoolCreateInfo.flags:=fFlags;
  HandleResultCode(fDevice.fDeviceVulkan.CreateCommandPool(fDevice.Handle,@fCommandPoolCreateInfo,fAllocationCallbacks,@fCommandPoolHandle));
 end;
end;

constructor TVulkanDeviceMemoryChunkBlockRedBlackTreeNode.Create(const pKey:TVkUInt64=0;
                                                                 const pValue:TVulkanDeviceMemoryChunkBlockRedBlackTreeValue=nil;
                                                                 const pLeft:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode=nil;
                                                                 const pRight:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode=nil;
                                                                 const pParent:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode=nil;
                                                                 const pColor:boolean=false);
begin
 inherited Create;
 fKey:=pKey;
 fValue:=pValue;
 fLeft:=pLeft;
 fRight:=pRight;
 fParent:=pParent;
 fColor:=pColor;
end;

destructor TVulkanDeviceMemoryChunkBlockRedBlackTreeNode.Destroy;
begin
 FreeAndNil(fLeft);
 FreeAndNil(fRight);
 inherited Destroy;
end;

procedure TVulkanDeviceMemoryChunkBlockRedBlackTreeNode.Clear;
begin
 fKey:=0;
 fLeft:=nil;
 fRight:=nil;
 fParent:=nil;
 fColor:=false;
end;

function TVulkanDeviceMemoryChunkBlockRedBlackTreeNode.Minimum:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
begin
 result:=self;
 while assigned(result.fLeft) do begin
  result:=result.fLeft;
 end;
end;

function TVulkanDeviceMemoryChunkBlockRedBlackTreeNode.Maximum:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
begin
 result:=self;
 while assigned(result.fRight) do begin
  result:=result.fRight;
 end;
end;

function TVulkanDeviceMemoryChunkBlockRedBlackTreeNode.Predecessor:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
var Last:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
begin
 if assigned(fLeft) then begin
  result:=fLeft;
  while assigned(result) and assigned(result.fRight) do begin
   result:=result.fRight;
  end;
 end else begin
  Last:=self;
  result:=Parent;
  while assigned(result) and (result.fLeft=Last) do begin
   Last:=result;
   result:=result.Parent;
  end;
 end;
end;

function TVulkanDeviceMemoryChunkBlockRedBlackTreeNode.Successor:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
var Last:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
begin
 if assigned(fRight) then begin
  result:=fRight;
  while assigned(result) and assigned(result.fLeft) do begin
   result:=result.fLeft;
  end;
 end else begin
  Last:=self;
  result:=Parent;
  while assigned(result) and (result.fRight=Last) do begin
   Last:=result;
   result:=result.Parent;
  end;
 end;
end;

constructor TVulkanDeviceMemoryChunkBlockRedBlackTree.Create;
begin
 inherited Create;
 fRoot:=nil;
end;

destructor TVulkanDeviceMemoryChunkBlockRedBlackTree.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TVulkanDeviceMemoryChunkBlockRedBlackTree.Clear;
begin
 FreeAndNil(fRoot);
end;

procedure TVulkanDeviceMemoryChunkBlockRedBlackTree.RotateLeft(x:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode);
var y:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
begin
 y:=x.fRight;
 x.fRight:=y.fLeft;
 if assigned(y.fLeft) then begin
  y.fLeft.fParent:=x;
 end;
 y.fParent:=x.fParent;
 if x=fRoot then begin
  fRoot:=y;
 end else if x=x.fParent.fLeft then begin
  x.fparent.fLeft:=y;
 end else begin
  x.fParent.fRight:=y;
 end;
 y.fLeft:=x;
 x.fParent:=y;
end;

procedure TVulkanDeviceMemoryChunkBlockRedBlackTree.RotateRight(x:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode);
var y:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
begin
 y:=x.fLeft;
 x.fLeft:=y.fRight;
 if assigned(y.fRight) then begin
  y.fRight.fParent:=x;
 end;
 y.fParent:=x.fParent;
 if x=fRoot then begin
  fRoot:=y;
 end else if x=x.fParent.fRight then begin
  x.fParent.fRight:=y;
 end else begin
  x.fParent.fLeft:=y;
 end;
 y.fRight:=x;
 x.fParent:=y;
end;

function TVulkanDeviceMemoryChunkBlockRedBlackTree.Find(const pKey:TVulkanDeviceMemoryChunkBlockRedBlackTreeKey):TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
begin
 result:=fRoot;
 while assigned(result) do begin
  if pKey<result.fKey then begin
   result:=result.fLeft;
  end else if pKey>result.fKey then begin
   result:=result.fRight;
  end else begin
   exit;
  end;
 end;
 result:=nil;
end;

function TVulkanDeviceMemoryChunkBlockRedBlackTree.Insert(const pKey:TVulkanDeviceMemoryChunkBlockRedBlackTreeKey;
                                                          const pValue:TVulkanDeviceMemoryChunkBlockRedBlackTreeValue):TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
var x,y,xParentParent:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
begin
 x:=fRoot;
 y:=nil;
 while assigned(x) do begin
  y:=x;
  if pKey<x.fKey then begin
   x:=x.fLeft;
  end else begin
   x:=x.fRight;
  end;
 end;
 result:=TVulkanDeviceMemoryChunkBlockRedBlackTreeNode.Create(pKey,pValue,nil,nil,y,true);
 if assigned(y) then begin
  if pKey<y.fKey then begin
   y.Left:=result;
  end else begin
   y.Right:=result;
  end;
 end else begin
  fRoot:=result;
 end;
 x:=result;
 while (x<>fRoot) and assigned(x.fParent) and assigned(x.fParent.fParent) and x.fParent.fColor do begin
  xParentParent:=x.fParent.fParent;
  if x.fParent=xParentParent.fLeft then begin
   y:=xParentParent.fRight;
   if assigned(y) and y.fColor then begin
    x.fParent.fColor:=false;
    y.fColor:=false;
    xParentParent.fColor:=true;
    x:=xParentParent;
   end else begin
    if x=x.fParent.fRight then begin
     x:=x.fParent;
     RotateLeft(x);
    end;
    x.fParent.fColor:=false;
    xParentParent.fColor:=true;
    RotateRight(xParentParent);
   end;
  end else begin
   y:=xParentParent.fLeft;
   if assigned(y) and y.fColor then begin
    x.fParent.fColor:=false;
    y.fColor:=false;
    x.fParent.fParent.fColor:=true;
    x:=x.fParent.fParent;
   end else begin
    if x=x.fParent.fLeft then begin
     x:=x.fParent;
     RotateRight(x);
    end;
    x.fParent.fColor:=false;
    xParentParent.fColor:=true;
    RotateLeft(xParentParent);
   end;
  end;
 end;
 fRoot.fColor:=false;
end;

procedure TVulkanDeviceMemoryChunkBlockRedBlackTree.Remove(const pNode:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode);
var w,x,y,z,xParent:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
    TemporaryColor:boolean;
begin
 z:=pNode;
 y:=z;
 x:=nil;
 xParent:=nil;
 if assigned(x) and assigned(xParent) then begin
  // For to suppress "Value assigned to '*' never used" hints
 end;
 if assigned(y.fLeft) then begin
  if assigned(y.fRight) then begin
   y:=y.fRight;
   while assigned(y.fLeft) do begin
    y:=y.fLeft;
   end;
   x:=y.fRight;
  end else begin
   x:=y.fLeft;
  end;
 end else begin
  x:=y.fRight;
 end;
 if y<>z then begin
  z.fLeft.fParent:=y;
  y.fLeft:=z.fLeft;
  if y<>z.fRight then begin
   xParent:=y.fParent;
   if assigned(x) then begin
    x.fParent:=y.fParent;
   end;
   y.fParent.fLeft:=x;
   y.fRight:=z.fRight;
   z.fRight.fParent:=y;
  end else begin
   xParent:=y;
  end;
  if fRoot=z then begin
   fRoot:=y;
  end else if z.fParent.fLeft=z then begin
   z.fParent.fLeft:=y;
  end else begin
   z.fParent.fRight:=y;
  end;
  y.fParent:=z.fParent;
  TemporaryColor:=y.fColor;
  y.fColor:=z.fColor;
  z.fColor:=TemporaryColor;
  y:=z;
 end else begin
  xParent:=y.fParent;
  if assigned(x) then begin
   x.fParent:=y.fParent;
  end;
  if fRoot=z then begin
   fRoot:=x;
  end else if z.fParent.fLeft=z then begin
   z.fParent.fLeft:=x;
  end else begin
   z.fParent.fRight:=x;
  end;
 end;
 if assigned(y) then begin
  if not y.fColor then begin
   while (x<>fRoot) and not (assigned(x) and x.fColor) do begin
    if x=xParent.fLeft then begin
     w:=xParent.fRight;
     if w.fColor then begin
      w.fColor:=false;
      xParent.fColor:=true;
      RotateLeft(xParent);
      w:=xParent.fRight;
     end;
     if not ((assigned(w.fLeft) and w.fLeft.fColor) or (assigned(w.fRight) and w.fRight.fColor)) then begin
      w.fColor:=true;
      x:=xParent;
      xParent:=xParent.fParent;
     end else begin
      if not (assigned(w.fRight) and w.fRight.fColor) then begin
       w.fLeft.fColor:=false;
       w.fColor:=true;
       RotateRight(w);
       w:=xParent.fRight;
      end;
      w.fColor:=xParent.fColor;
      xParent.fColor:=false;
      if assigned(w.fRight) then begin
       w.fRight.fColor:=false;
      end;
      RotateLeft(xParent);
      x:=fRoot;
     end;
    end else begin
     w:=xParent.fLeft;
     if w.fColor then begin
      w.fColor:=false;
      xParent.fColor:=true;
      RotateRight(xParent);
      w:=xParent.fLeft;
     end;
     if not ((assigned(w.fLeft) and w.fLeft.fColor) or (assigned(w.fRight) and w.fRight.fColor)) then begin
      w.fColor:=true;
      x:=xParent;
      xParent:=xParent.fParent;
     end else begin
      if not (assigned(w.fLeft) and w.fLeft.fColor) then begin
       w.fRight.fColor:=false;
       w.fColor:=true;
       RotateLeft(w);
       w:=xParent.fLeft;
      end;
      w.fColor:=xParent.fColor;
      xParent.fColor:=false;
      if assigned(w.fLeft) then begin
       w.fLeft.fColor:=false;
      end;
      RotateRight(xParent);
      x:=fRoot;
     end;
    end;
   end;
   if assigned(x) then begin
    x.fColor:=false;
   end;
  end;
  y.Clear;
  y.Free;
 end;
end;

procedure TVulkanDeviceMemoryChunkBlockRedBlackTree.Delete(const pKey:TVulkanDeviceMemoryChunkBlockRedBlackTreeKey);
var Node:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
begin
 Node:=Find(pKey);
 if assigned(Node) then begin
  Remove(Node);
 end;
end;

function TVulkanDeviceMemoryChunkBlockRedBlackTree.LeftMost:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
begin
 result:=fRoot;
 while assigned(result) and assigned(result.fLeft) do begin
  result:=result.fLeft;
 end;
end;

function TVulkanDeviceMemoryChunkBlockRedBlackTree.RightMost:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
begin
 result:=fRoot;
 while assigned(result) and assigned(result.fRight) do begin
  result:=result.fRight;
 end;
end;

constructor TVulkanDeviceMemoryChunkBlock.Create(const pMemoryChunk:TVulkanDeviceMemoryChunk;
                                                 const pOffset:TVkDeviceSize;
                                                 const pSize:TVkDeviceSize;
                                                 const pUsed:boolean);
begin
 inherited Create;
 fMemoryChunk:=pMemoryChunk;
 fOffset:=pOffset;
 fSize:=pSize;
 fUsed:=pUsed;
 fOffsetRedBlackTreeNode:=fMemoryChunk.fOffsetRedBlackTree.Insert(pOffset,self);
 if not fUsed then begin
  fSizeRedBlackTreeNode:=fMemoryChunk.fSizeRedBlackTree.Insert(pSize,self);
 end;
end;

destructor TVulkanDeviceMemoryChunkBlock.Destroy;
begin
 fMemoryChunk.fOffsetRedBlackTree.Remove(fOffsetRedBlackTreeNode);
 if not fUsed then begin
  fMemoryChunk.fSizeRedBlackTree.Remove(fSizeRedBlackTreeNode);
 end;
 inherited Destroy;
end;

procedure TVulkanDeviceMemoryChunkBlock.Update(const pOffset:TVkDeviceSize;
                                               const pSize:TVkDeviceSize;
                                               const pUsed:boolean);
begin
 if fOffset<>pOffset then begin
  fMemoryChunk.fOffsetRedBlackTree.Remove(fOffsetRedBlackTreeNode);
  fOffsetRedBlackTreeNode:=fMemoryChunk.fOffsetRedBlackTree.Insert(pOffset,self);
 end;
 if (fUsed<>pUsed) or (fSize<>pSize) then begin
  if not fUsed then begin
   fMemoryChunk.fSizeRedBlackTree.Remove(fSizeRedBlackTreeNode);
  end;
  if not pUsed then begin
   fSizeRedBlackTreeNode:=fMemoryChunk.fSizeRedBlackTree.Insert(pSize,self);
  end;
 end;
 fOffset:=pOffset;
 fSize:=pSize;
 fUsed:=pUsed;
 inherited Destroy;
end;

constructor TVulkanDeviceMemoryChunk.Create(const pMemoryManager:TVulkanDeviceMemoryManager;
                                            const pSize:TVkDeviceSize;
                                            const pMemoryPropertyFlags:TVkMemoryPropertyFlags;
                                            const pMemoryHeapFlags:TVkMemoryHeapFlags=0);
var Index,HeapIndex:TVkInt32;
    MemoryRequirements:TVkMemoryRequirements;
    MemoryAllocateInfo:TVkMemoryAllocateInfo;
    PhysicalDevice:TVulkanPhysicalDevice;
    CurrentSize,BestSize,Alignment:TVkDeviceSize;
    Found:boolean;
begin
 inherited Create;

 fMemoryManager:=pMemoryManager;

 fSize:=pSize;

 fMemoryPropertyFlags:=pMemoryPropertyFlags;

 fMemoryHandle:=VK_NULL_HANDLE;

 fMemory:=nil;

 fMemoryTypeIndex:=0;
 PhysicalDevice:=fMemoryManager.fDevice.fPhysicalDevice;
 BestSize:=0;
 Found:=false;
 for Index:=0 to length(PhysicalDevice.fMemoryProperties.memoryTypes)-1 do begin
  if (PhysicalDevice.fMemoryProperties.memoryTypes[Index].propertyFlags and pMemoryPropertyFlags)=pMemoryPropertyFlags then begin
   HeapIndex:=PhysicalDevice.fMemoryProperties.memoryTypes[Index].heapIndex;
   CurrentSize:=PhysicalDevice.fMemoryProperties.memoryHeaps[HeapIndex].size;
   if ((PhysicalDevice.fMemoryProperties.memoryHeaps[HeapIndex].flags and pMemoryHeapFlags)=pMemoryHeapFlags) and
      (pSize<=CurrentSize) and (CurrentSize>BestSize) then begin
    BestSize:=CurrentSize;
    fMemoryTypeIndex:=PhysicalDevice.fMemoryProperties.memoryTypes[Index].heapIndex;
    Found:=true;
   end;
  end;
 end;
 if not Found then begin
  raise EVulkanException.Create('No suitable device memory heap available');
 end;

 FillChar(MemoryAllocateInfo,SizeOf(TVkMemoryAllocateInfo),#0);
 MemoryAllocateInfo.sType:=VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
 MemoryAllocateInfo.pNext:=nil;
 MemoryAllocateInfo.allocationSize:=fSize;
 MemoryAllocateInfo.memoryTypeIndex:=fMemoryTypeIndex;

 HandleResultCode(fMemoryManager.fDevice.Commands.AllocateMemory(fMemoryManager.fDevice.Handle,@MemoryAllocateInfo,fMemoryManager.fAllocationCallbacks,@fMemoryHandle));

 if (fMemoryPropertyFlags and TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))<>0 then begin
  fMemoryManager.fDevice.Commands.MapMemory(fMemoryManager.fDevice.Handle,fMemoryHandle,0,TVkUInt64(VK_WHOLE_SIZE),0,@fMemory);
 end;

 // Ensure alignment
 Alignment:=128;
 dec(Alignment);
 Alignment:=Alignment or (Alignment shr 1);
 Alignment:=Alignment or (Alignment shr 2);
 Alignment:=Alignment or (Alignment shr 4);
 Alignment:=Alignment or (Alignment shr 8);
 Alignment:=Alignment or (Alignment shr 16);
 fAlignment:=(Alignment or (Alignment shr 32))+1;

 fOffsetRedBlackTree:=TVulkanDeviceMemoryChunkBlockRedBlackTree.Create;
 fSizeRedBlackTree:=TVulkanDeviceMemoryChunkBlockRedBlackTree.Create;

 TVulkanDeviceMemoryChunkBlock.Create(self,0,pSize,false);

 if assigned(fMemoryManager.fLastMemoryChunk) then begin
  fMemoryManager.fLastMemoryChunk.fNextMemoryChunk:=self;
  fPreviousMemoryChunk:=fMemoryManager.fLastMemoryChunk;
 end else begin
  fMemoryManager.fFirstMemoryChunk:=self;
  fPreviousMemoryChunk:=nil;
 end;
 fMemoryManager.fLastMemoryChunk:=self;
 fNextMemoryChunk:=nil;

end;

destructor TVulkanDeviceMemoryChunk.Destroy;
begin

 if assigned(fOffsetRedBlackTree) then begin
  while assigned(fOffsetRedBlackTree.fRoot) do begin
   fOffsetRedBlackTree.fRoot.fValue.Free;
  end;
 end;

 if assigned(fPreviousMemoryChunk) then begin
  fPreviousMemoryChunk.fNextMemoryChunk:=fNextMemoryChunk;
 end else if fMemoryManager.fFirstMemoryChunk=self then begin
  fMemoryManager.fFirstMemoryChunk:=fNextMemoryChunk;
 end;
 if assigned(fNextMemoryChunk) then begin
  fNextMemoryChunk.fPreviousMemoryChunk:=fPreviousMemoryChunk;
 end else if fMemoryManager.fLastMemoryChunk=self then begin
  fMemoryManager.fLastMemoryChunk:=fPreviousMemoryChunk;
 end;

 if fMemoryHandle<>VK_NULL_HANDLE then begin
  if (fMemoryPropertyFlags and TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))<>0 then begin
   fMemoryManager.fDevice.Commands.UnmapMemory(fMemoryManager.fDevice.Handle,fMemoryHandle);
  end;
  fMemoryManager.fDevice.Commands.FreeMemory(fMemoryManager.fDevice.Handle,fMemoryHandle,fMemoryManager.fAllocationCallbacks);
 end;

 fOffsetRedBlackTree.Free;
 fSizeRedBlackTree.Free;

 fMemoryHandle:=VK_NULL_HANDLE;

 inherited Destroy;
end;

function TVulkanDeviceMemoryChunk.AllocateMemory(out pOffset:TVkDeviceSize;const pSize:TVkDeviceSize):boolean;
var Node,OtherNode:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
    MemoryChunkBlock:TVulkanDeviceMemoryChunkBlock;
    TempOffset,TempSize,Size:TVkDeviceSize;
begin
 result:=false;

 Size:=pSize;

 // Ensure alignment
 if (Size and (fAlignment-1))<>0 then begin
  inc(Size,fAlignment-(Size and (fAlignment-1)));
 end;

 // Best-fit search
 Node:=fSizeRedBlackTree.fRoot;
 while assigned(Node) do begin
  if Size<Node.fKey then begin
   if assigned(Node.fLeft) then begin
    // If free block is too big, then go to left
    Node:=Node.fLeft;
    continue;
   end else begin
    // If free block is too big and there is no left children node, then try to find suitable smaller but not to small free blocks
    while assigned(Node) and (Node.fKey>Size) do begin
     OtherNode:=Node.Predecessor;
     if assigned(OtherNode) and (OtherNode.fKey>=Size) then begin
      Node:=OtherNode;
     end else begin
      break;
     end;
    end;
    break;
   end;
  end else if Size>Node.fKey then begin
   if assigned(Node.fRight) then begin
    // If free block is too small, go to right
    Node:=Node.fRight;
    continue;
   end else begin
    // If free block is too small and there is no right children node, Try to find suitable bigger but not to small free blocks
    while assigned(Node) and (Node.fKey<Size) do begin
     OtherNode:=Node.Successor;
     if assigned(OtherNode) then begin
      Node:=OtherNode;
     end else begin
      break;
     end;
    end;
    break;
   end;
  end else begin
   // Perfect match
   break;
  end;
 end;

 if assigned(Node) and (Node.fKey>=Size) then begin
  MemoryChunkBlock:=Node.fValue;
  TempOffset:=MemoryChunkBlock.Offset;
  TempSize:=MemoryChunkBlock.Size;
  if TempSize=Size then begin
   MemoryChunkBlock.Update(MemoryChunkBlock.Offset,MemoryChunkBlock.Size,true);
  end else begin
   MemoryChunkBlock.Update(TempOffset,Size,true);
   TVulkanDeviceMemoryChunkBlock.Create(self,TempOffset+Size,TempSize-Size,false);
  end;
  pOffset:=TempOffset;
  result:=true;
 end;

end;

function TVulkanDeviceMemoryChunk.ReallocateMemory(var pOffset:TVkDeviceSize;const pSize:TVkDeviceSize):boolean;
var Node,OtherNode:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
    MemoryChunkBlock,OtherMemoryChunkBlock:TVulkanDeviceMemoryChunkBlock;
    Size,TempOffset,TempSize:TVkDeviceSize;
begin
 result:=false;

 Size:=pSize;

 // Ensure alignment
 if (Size and (fAlignment-1))<>0 then begin
  inc(Size,fAlignment-(Size and (fAlignment-1)));
 end;

 Node:=fOffsetRedBlackTree.Find(pOffset);
 if assigned(Node) then begin
  MemoryChunkBlock:=Node.fValue;
  if MemoryChunkBlock.fUsed then begin
   if Size=0 then begin
    result:=FreeMemory(pOffset);
   end else if MemoryChunkBlock.fSize=Size then begin
    result:=true;
   end else begin
    if MemoryChunkBlock.fSize<Size then begin
     OtherNode:=MemoryChunkBlock.fOffsetRedBlackTreeNode.Successor;
     if assigned(OtherNode) and
        (MemoryChunkBlock.fOffsetRedBlackTreeNode<>OtherNode) then begin
      OtherMemoryChunkBlock:=OtherNode.fValue;
      if not OtherMemoryChunkBlock.fUsed then begin
       if (MemoryChunkBlock.fOffset+Size)<(OtherMemoryChunkBlock.fOffset+OtherMemoryChunkBlock.fSize) then begin
        MemoryChunkBlock.Update(MemoryChunkBlock.fOffset,Size,true);
        OtherMemoryChunkBlock.Update(MemoryChunkBlock.fOffset+Size,(OtherMemoryChunkBlock.fOffset+OtherMemoryChunkBlock.fSize)-(MemoryChunkBlock.fOffset+Size),false);
        result:=true;
       end else if (MemoryChunkBlock.fOffset+Size)=(OtherMemoryChunkBlock.fOffset+OtherMemoryChunkBlock.fSize) then begin
        MemoryChunkBlock.Update(MemoryChunkBlock.fOffset,Size,true);
        OtherMemoryChunkBlock.Free;
        result:=true;
       end;
      end;
     end;
    end else if MemoryChunkBlock.fSize>Size then begin
     OtherNode:=MemoryChunkBlock.fOffsetRedBlackTreeNode.Successor;
     if assigned(OtherNode) and
        (MemoryChunkBlock.fOffsetRedBlackTreeNode<>OtherNode) and
        not OtherNode.fValue.fUsed then begin
      OtherMemoryChunkBlock:=OtherNode.fValue;
      TempOffset:=MemoryChunkBlock.fOffset+Size;
      TempSize:=(OtherMemoryChunkBlock.fOffset+OtherMemoryChunkBlock.fSize)-TempOffset;
      MemoryChunkBlock.Update(MemoryChunkBlock.fOffset,Size,true);
      OtherMemoryChunkBlock.Update(TempOffset,TempSize,false);
      result:=true;
     end else begin
      TempOffset:=MemoryChunkBlock.fOffset+Size;
      TempSize:=(MemoryChunkBlock.fOffset+MemoryChunkBlock.fSize)-TempOffset;
      MemoryChunkBlock.Update(MemoryChunkBlock.fOffset,Size,true);
      TVulkanDeviceMemoryChunkBlock.Create(self,TempOffset,TempSize,false);
      result:=true;
     end;
    end;
   end;
  end;
 end;

end;

function TVulkanDeviceMemoryChunk.FreeMemory(const pOffset:TVkDeviceSize):boolean;
var Node,OtherNode:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
    MemoryChunkBlock,OtherMemoryChunkBlock:TVulkanDeviceMemoryChunkBlock;
    TempOffset,TempSize:TVkDeviceSize;
begin
 result:=false;
 Node:=fOffsetRedBlackTree.Find(pOffset);
 if assigned(Node) then begin
  MemoryChunkBlock:=Node.fValue;
  if MemoryChunkBlock.fUsed then begin
   // Freeing including coalescing free blocks
   while assigned(Node) do begin

    // Coalescing previous free block with current block
    OtherNode:=MemoryChunkBlock.fOffsetRedBlackTreeNode.Predecessor;
    if assigned(OtherNode) and not OtherNode.fValue.fUsed then begin
     OtherMemoryChunkBlock:=OtherNode.fValue;
     TempOffset:=OtherMemoryChunkBlock.fOffset;
     TempSize:=(MemoryChunkBlock.fOffset+MemoryChunkBlock.fSize)-TempOffset;
     MemoryChunkBlock.Free;
     OtherMemoryChunkBlock.Update(TempOffset,TempSize,false);
     MemoryChunkBlock:=OtherMemoryChunkBlock;
     Node:=OtherNode;
     continue;
    end;

    // Coalescing current block with next free block
    OtherNode:=MemoryChunkBlock.fOffsetRedBlackTreeNode.Successor;
    if assigned(OtherNode) and not OtherNode.fValue.fUsed then begin
     OtherMemoryChunkBlock:=OtherNode.fValue;
     TempOffset:=MemoryChunkBlock.fOffset;
     TempSize:=(OtherMemoryChunkBlock.fOffset+OtherMemoryChunkBlock.fSize)-TempOffset;
     OtherMemoryChunkBlock.Free;
     MemoryChunkBlock.Update(TempOffset,TempSize,false);
     continue;
    end;

    if MemoryChunkBlock.fUsed then begin
     // Mark block as free
     MemoryChunkBlock.Update(MemoryChunkBlock.fOffset,MemoryChunkBlock.fSize,false);
    end;
    break;

   end;
  end;
 end;
end;

constructor TVulkanDeviceMemoryBlock.Create(const pMemoryManager:TVulkanDeviceMemoryManager;
                                            const pMemoryChunk:TVulkanDeviceMemoryChunk;
                                            const pOffset:TVkDeviceSize;
                                            const pSize:TVkDeviceSize);
begin

 inherited Create;

 fMemoryManager:=pMemoryManager;

 fMemoryChunk:=pMemoryChunk;

 fOffset:=pSize;

 fSize:=pOffset;

 if assigned(fMemoryManager.fLastMemoryBlock) then begin
  fMemoryManager.fLastMemoryBlock.fNextMemoryBlock:=self;
  fPreviousMemoryBlock:=fMemoryManager.fLastMemoryBlock;
 end else begin
  fMemoryManager.fFirstMemoryBlock:=self;
  fPreviousMemoryBlock:=nil;
 end;
 fMemoryManager.fLastMemoryBlock:=self;
 fNextMemoryBlock:=nil;

end;

destructor TVulkanDeviceMemoryBlock.Destroy;
begin
 if assigned(fPreviousMemoryBlock) then begin
  fPreviousMemoryBlock.fNextMemoryBlock:=fNextMemoryBlock;
 end else if fMemoryManager.fFirstMemoryBlock=self then begin
  fMemoryManager.fFirstMemoryBlock:=fNextMemoryBlock;
 end;
 if assigned(fNextMemoryBlock) then begin
  fNextMemoryBlock.fPreviousMemoryBlock:=fPreviousMemoryBlock;
 end else if fMemoryManager.fLastMemoryBlock=self then begin
  fMemoryManager.fLastMemoryBlock:=fPreviousMemoryBlock;
 end;
 inherited Destroy;
end;

constructor TVulkanDeviceMemoryManager.Create(const pDevice:TVulkanDevice;
                                              const pAllocationManager:TVulkanAllocationManager=nil);
begin
 inherited Create;

 fDevice:=pDevice;

 if assigned(pAllocationManager) then begin
  fAllocationManager:=pAllocationManager;
 end else begin
  fAllocationManager:=fDevice.fAllocationManager;
 end;

 if assigned(fAllocationManager) then begin
  fAllocationCallbacks:=@fAllocationManager.fAllocationCallbacks;
 end else begin
  fAllocationCallbacks:=nil;
 end;

 fFirstMemoryChunk:=nil;
 fLastMemoryChunk:=nil;

 fFirstMemoryBlock:=nil;
 fLastMemoryBlock:=nil;

end;

destructor TVulkanDeviceMemoryManager.Destroy;
begin
 while assigned(fFirstMemoryBlock) do begin
  fFirstMemoryBlock.Free;
 end;
 while assigned(fFirstMemoryChunk) do begin
  fFirstMemoryChunk.Free;
 end;
 inherited Destroy;
end;

function TVulkanDeviceMemoryManager.AllocateMemoryBlock(const pSize:TVkDeviceSize;
                                                        const pMemoryPropertyFlags:TVkMemoryPropertyFlags):TVulkanDeviceMemoryBlock;
var Index:TVkInt32;
    MemoryChunk:TVulkanDeviceMemoryChunk;
    Offset:TVkDeviceSize;
begin

 // Try first to allocate a block inside already existent chunks
 MemoryChunk:=fFirstMemoryChunk;
 while assigned(MemoryChunk) do begin
  if (MemoryChunk.fMemoryPropertyFlags and pMemoryPropertyFlags)=pMemoryPropertyFlags then begin
   if MemoryChunk.AllocateMemory(Offset,pSize) then begin
    result:=TVulkanDeviceMemoryBlock.Create(self,MemoryChunk,Offset,pSize);
    exit;
   end;
  end;
  MemoryChunk:=MemoryChunk.fNextMemoryChunk;
 end;

 // Otherwise allocate a block inside a new chunk
 MemoryChunk:=TVulkanDeviceMemoryChunk.Create(self,VulkanDeviceSizeRoundUpToPowerOfTwo(Max(1 shl 25,pSize shl 1)),pMemoryPropertyFlags);
 result:=TVulkanDeviceMemoryBlock.Create(self,MemoryChunk,Offset,pSize);

end;

function TVulkanDeviceMemoryManager.FreeMemoryBlock(const pMemoryBlock:TVulkanDeviceMemoryBlock):boolean;
var MemoryChunk:TVulkanDeviceMemoryChunk;
begin
 result:=assigned(pMemoryBlock);
 if result then begin
  MemoryChunk:=pMemoryBlock.fMemoryChunk;
  result:=MemoryChunk.FreeMemory(pMemoryBlock.fOffset);
  if result then begin
   pMemoryBlock.Free;
   if assigned(MemoryChunk.fOffsetRedBlackTree.fRoot) and
      (MemoryChunk.fOffsetRedBlackTree.fRoot.fValue.fOffset=0) and
      (MemoryChunk.fOffsetRedBlackTree.fRoot.fValue.fSize=MemoryChunk.fSize) and
      not (assigned(MemoryChunk.fOffsetRedBlackTree.fRoot.fLeft) or assigned(MemoryChunk.fOffsetRedBlackTree.fRoot.fRight)) then begin
    MemoryChunk.Free;  
   end;
  end;
 end;
end;

constructor TVulkanBuffer.Create(const pDevice:TVulkanDevice;
                                 const pInitializationData:pointer;
                                 const pSize:TVkDeviceSize;
                                 const pAllocationManager:TVulkanAllocationManager=nil);
begin
 inherited Create;

 fDevice:=pDevice;

 if assigned(pAllocationManager) then begin
  fAllocationManager:=pAllocationManager;
 end else begin
  fAllocationManager:=fDevice.fAllocationManager;
 end;

 if assigned(fAllocationManager) then begin
  fAllocationCallbacks:=@fAllocationManager.fAllocationCallbacks;
 end else begin
  fAllocationCallbacks:=nil;
 end;

 fBufferHandle:=VK_NULL_HANDLE;

 fMemoryHandle:=VK_NULL_HANDLE;

 fOwnsHandle:=true;

 fSize:=pSize;

end;

destructor TVulkanBuffer.Destroy;
begin
 if (fBufferHandle<>VK_NULL_HANDLE) and fOwnsHandle then begin
  fDevice.Commands.DestroyBuffer(fDevice.Handle,fBufferHandle,fAllocationCallbacks);
 end;
 if (fMemoryHandle<>VK_NULL_HANDLE) and fOwnsHandle then begin
  fDevice.Commands.FreeMemory(fDevice.Handle,fMemoryHandle,fAllocationCallbacks);
 end;
 fBufferHandle:=VK_NULL_HANDLE;
 fMemoryHandle:=VK_NULL_HANDLE;
 inherited Destroy;
end;

function TVulkanBuffer.Map:PVkVoid;
begin
 result:=nil;
 HandleResultCode(fDevice.Commands.MapMemory(fDevice.Handle,fMemoryHandle,0,fSize,0,@result));
end;

procedure TVulkanBuffer.Unmap;
begin
 fDevice.Commands.UnmapMemory(fDevice.Handle,fMemoryHandle);
end;

procedure TVulkanBuffer.Bind;
begin
 HandleResultCode(fDevice.Commands.BindBufferMemory(fDevice.Handle,fBufferHandle,fMemoryHandle,0));
end;

constructor TVulkanSwapChain.Create(const pDevice:TVulkanDevice;
                                    const pOldSwapChain:TVulkanSwapChain=nil;
                                    const pAllocationManager:TVulkanAllocationManager=nil);
begin
 inherited Create;

 fDevice:=pDevice;

 if assigned(pAllocationManager) then begin
  fAllocationManager:=pAllocationManager;
 end else begin
  fAllocationManager:=fDevice.fAllocationManager;
 end;

 if assigned(fAllocationManager) then begin
  fAllocationCallbacks:=@fAllocationManager.fAllocationCallbacks;
 end else begin
  fAllocationCallbacks:=nil;
 end;

 fSwapChainHandle:=VK_NULL_HANDLE;

 FillChar(fSwapChainCreateInfo,SizeOf(TVkSwapChainCreateInfoKHR),#0);
 fSwapChainCreateInfo.sType:=VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
 if assigned(pOldSwapChain) then begin
  fSwapChainCreateInfo.oldSwapchain:=pOldSwapChain.fSwapChainHandle;
 end else begin
  fSwapChainCreateInfo.oldSwapchain:=VK_NULL_HANDLE;
 end;

 fPointerToSwapChainCreateInfo:=@fSwapChainCreateInfo;

 fOwnsHandle:=true;

end;

destructor TVulkanSwapChain.Destroy;
begin
 if (fSwapChainHandle<>VK_NULL_HANDLE) and fOwnsHandle then begin
  fDevice.fDeviceVulkan.DestroySwapChainKHR(fDevice.Handle,fSwapChainHandle,fAllocationCallbacks);
 end;
 inherited Destroy;
end;

procedure TVulkanSwapChain.Initialize;
begin
 if (fSwapChainHandle=VK_NULL_HANDLE) and fOwnsHandle then begin
  HandleResultCode(fDevice.fDeviceVulkan.CreateSwapChainKHR(fDevice.Handle,@fSwapChainCreateInfo,fAllocationCallbacks,@fSwapChainHandle));
 end;
end;

end.

