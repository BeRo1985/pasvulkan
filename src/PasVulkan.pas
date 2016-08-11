(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                        Version 2016-08-11-02-54-0000                       *
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
 * 3. This PasVulkan wrapper may be used only with the PasVulkan-own Vulkan   *
 *    Pascal header.                                                          *
 * 4. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/pasvulkan                                    *
 * 5. Write code, which is compatible with Delphi 7-XE7 and FreePascal >= 3.0 *
 *    so don't use generics/templates, operator overloading and another newer *
 *    syntax features than Delphi 7 has support for that, but if needed, make *
 *    it out-ifdef-able.                                                      *
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
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {-$pic off}
 {$define caninline}
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
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
 {$ifndef BCB}
  {$ifdef ver120}
   {$define Delphi4or5}
  {$endif}
  {$ifdef ver130}
   {$define Delphi4or5}
  {$endif}
  {$ifdef ver140}
   {$define Delphi6}
  {$endif}
  {$ifdef ver150}
   {$define Delphi7}
  {$endif}
  {$ifdef ver170}
   {$define Delphi2005}
  {$endif}
 {$else}
  {$ifdef ver120}
   {$define Delphi4or5}
   {$define BCB4}
  {$endif}
  {$ifdef ver130}
   {$define Delphi4or5}
  {$endif}
 {$endif}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}       
  {$ifend}
  {$if CompilerVersion>=14.0}
   {$if CompilerVersion=14.0}
    {$define Delphi6}
   {$ifend}
   {$define Delphi6AndUp}
  {$ifend}
  {$if CompilerVersion>=15.0}
   {$if CompilerVersion=15.0}
    {$define Delphi7}
   {$ifend}
   {$define Delphi7AndUp}
  {$ifend}
  {$if CompilerVersion>=17.0}
   {$if CompilerVersion=17.0}
    {$define Delphi2005}
   {$ifend}
   {$define Delphi2005AndUp}
  {$ifend}
  {$if CompilerVersion>=18.0}
   {$if CompilerVersion=18.0}
    {$define BDS2006}
    {$define Delphi2006}
   {$ifend}
   {$define Delphi2006AndUp}
  {$ifend}
  {$if CompilerVersion>=18.5}
   {$if CompilerVersion=18.5}
    {$define Delphi2007}
   {$ifend}
   {$define Delphi2007AndUp}
  {$ifend}
  {$if CompilerVersion=19.0}
   {$define Delphi2007Net}
  {$ifend}
  {$if CompilerVersion>=20.0}
   {$if CompilerVersion=20.0}
    {$define Delphi2009}
   {$ifend}
   {$define Delphi2009AndUp}
  {$ifend}
  {$if CompilerVersion>=21.0}
   {$if CompilerVersion=21.0}
    {$define Delphi2010}
   {$ifend}
   {$define Delphi2010AndUp}
  {$ifend}
  {$if CompilerVersion>=22.0}
   {$if CompilerVersion=22.0}
    {$define DelphiXE}
   {$ifend}
   {$define DelphiXEAndUp}
  {$ifend}
  {$if CompilerVersion>=23.0}
   {$if CompilerVersion=23.0}
    {$define DelphiXE2}
   {$ifend}
   {$define DelphiXE2AndUp}
  {$ifend}
  {$if CompilerVersion>=24.0}
   {$if CompilerVersion=24.0}
    {$define DelphiXE3}
   {$ifend}
   {$define DelphiXE3AndUp}
  {$ifend}
  {$if CompilerVersion>=25.0}
   {$if CompilerVersion=25.0}
    {$define DelphiXE4}
   {$ifend}
   {$define DelphiXE4AndUp}
  {$ifend}
  {$if CompilerVersion>=26.0}
   {$if CompilerVersion=26.0}
    {$define DelphiXE5}
   {$ifend}
   {$define DelphiXE5AndUp}
  {$ifend}
  {$if CompilerVersion>=27.0}
   {$if CompilerVersion=27.0}
    {$define DelphiXE6}
   {$ifend}
   {$define DelphiXE6AndUp}
  {$ifend}
  {$if CompilerVersion>=28.0}
   {$if CompilerVersion=28.0}
    {$define DelphiXE7}
   {$ifend}
   {$define DelphiXE7AndUp}
  {$ifend}
  {$if CompilerVersion>=29.0}
   {$if CompilerVersion=29.0}
    {$define DelphiXE8}
   {$ifend}
   {$define DelphiXE8AndUp}
  {$ifend}
  {$if CompilerVersion>=30.0}
   {$if CompilerVersion=30.0}
    {$define Delphi10Seattle}
   {$ifend}
   {$define Delphi10SeattleAndUp}
  {$ifend}
  {$if CompilerVersion>=31.0}
   {$if CompilerVersion=31.0}
    {$define Delphi10Berlin}
   {$ifend}
   {$define Delphi10BerlinAndUp}
  {$ifend}
 {$endif}
 {$ifndef Delphi4or5}
  {$ifndef BCB}
   {$define Delphi6AndUp}
  {$endif}
   {$ifndef Delphi6}
    {$define BCB6OrDelphi7AndUp}
    {$ifndef BCB}
     {$define Delphi7AndUp}
    {$endif}
    {$ifndef BCB}
     {$ifndef Delphi7}
      {$ifndef Delphi2005}
       {$define BDS2006AndUp}
      {$endif}
     {$endif}
    {$endif}
   {$endif}
 {$endif}
 {$ifdef Delphi6AndUp}
  {$warn symbol_platform off}
  {$warn symbol_deprecated off}
 {$endif}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$ifdef wince}
 {$define windows}
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

interface

uses {$ifdef Windows}Windows,{$endif}{$ifdef Unix}BaseUnix,UnixType,dl,{$endif}{$ifdef X11}x,xlib,{$endif}{$ifdef XCB}xcb,{$endif}{$ifdef Mir}Mir,{$endif}{$ifdef Wayland}Wayland,{$endif}{$ifdef Android}Android,{$endif}SysUtils,Classes,SyncObjs,Math,Vulkan;

type EVulkanException=class(Exception);

     EVulkanMemoryAllocation=class(EVulkanException);

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

     TVulkanCharString=TVkCharString;

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
     TVkSamplerArray=array of TVkSampler;
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
     TVkCopyDescriptorSetArray=array of TVkCopyDescriptorSet;
     TVkWriteDescriptorSetArray=array of TVkWriteDescriptorSet;
     TVkSpecializationMapEntryArray=array of TVkSpecializationMapEntry;
     TVkPipelineCacheArray=array of TVkPipelineCache;

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

     TVulkanInstance=class(TVulkanObject)
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
       procedure EnumeratePhysicalDevices;
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
       procedure InstallDebugReportCallback;
       property ApplicationInfo:TVkApplicationInfo read fApplicationInfo write SetApplicationInfo;
      published
       property ApplicationName:TVulkanCharString read GetApplicationName write SetApplicationName;
       property ApplicationVersion:TVkUInt32 read GetApplicationVersion write SetApplicationVersion;
       property EngineName:TVulkanCharString read GetEngineName write SetEngineName;
       property EngineVersion:TVkUInt32 read GetEngineVersion write SetEngineVersion;
       property APIVersion:TVkUInt32 read GetAPIVersion write SetAPIVersion;
       property Validation:longbool read fValidation write fValidation;
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

     TVulkanSurface=class;

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
       function HasQueueSupportForSparseBindings(const pQueueFamilyIndex:TVkUInt32):boolean;
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
       function GetSurfaceSupport(const pQueueFamilyIndex:TVkUInt32;const pSurface:TVulkanSurface):boolean;
       function GetSurfaceCapabilities(const pSurface:TVulkanSurface):TVkSurfaceCapabilitiesKHR;
       function GetSurfaceFormats(const pSurface:TVulkanSurface):TVkSurfaceFormatKHRArray;
       function GetSurfacePresentModes(const pSurface:TVulkanSurface):TVkPresentModeKHRArray;
       function GetDisplayProperties:TVkDisplayPropertiesKHRArray;
       function GetDisplayPlaneProperties:TVkDisplayPlanePropertiesKHRArray;
       function GetDisplayPlaneSupportedDisplays(const pPlaneIndex:TVkUInt32):TVkDisplayKHRArray;
       function GetDisplayModeProperties(const pDisplay:TVkDisplayKHR):TVkDisplayModePropertiesKHRArray;
       function GetMemoryType(const pTypeBits:TVkUInt32;const pProperties:TVkFlags):TVkUInt32;
       function GetBestSupportedDepthFormat(const pWithStencil:boolean):TVkFormat;
       function GetQueueNodeIndex(const pSurface:TVulkanSurface;const pQueueFlagBits:TVkQueueFlagBits):TVkInt32;
       function GetSurfaceFormat(const pSurface:TVulkanSurface):TVkSurfaceFormatKHR;
       property Properties:TVkPhysicalDeviceProperties read fProperties;
       property MemoryProperties:TVkPhysicalDeviceMemoryProperties read fMemoryProperties;
       property Features:TVkPhysicalDeviceFeatures read fFeatures;
      published
       property Handle:TVkPhysicalDevice read fPhysicalDeviceHandle;
       property DeviceName:TVulkanCharString read fDeviceName;
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

     PVulkanSurfaceCreateInfo=^TVulkanSurfaceCreateInfo;
{$if defined(Android)}
     TVulkanSurfaceCreateInfo=TVkAndroidSurfaceCreateInfoKHR;
{$elseif defined(Mir)}
     TVulkanSurfaceCreateInfo=TVkMirSurfaceCreateInfoKHR;
{$elseif defined(Wayland)}
     TVulkanSurfaceCreateInfo=TVkWaylandSurfaceCreateInfoKHR;
{$elseif defined(Windows)}
     TVulkanSurfaceCreateInfo=TVkWin32SurfaceCreateInfoKHR;
{$elseif defined(X11)}
     TVulkanSurfaceCreateInfo=TVkX11SurfaceCreateInfoKHR;
{$elseif defined(XCB)}
     TVulkanSurfaceCreateInfo=TVkXCBSurfaceCreateInfoKHR;
{$ifend}

     TVulkanSurface=class(TVulkanObject)
      private
       fInstance:TVulkanInstance;
       fSurfaceCreateInfo:TVulkanSurfaceCreateInfo;
       fSurfaceHandle:TVkSurfaceKHR;
      protected
      public
       constructor Create(const pInstance:TVulkanInstance;
{$if defined(Android)}
                          const pWindow:PANativeWindow
{$elseif defined(Mir)}
                          const pConnection:PMirConnection;const pMirSurface:PMirSurface
{$elseif defined(Wayland)}
                          const pDisplay:Pwl_display;const pSurface:Pwl_surface
{$elseif defined(Windows)}
                          const pInstanceHandle,pWindowHandle:THandle
{$elseif defined(X11)}
                          const pDisplay:PDisplay;const pWindow:TWindow
{$elseif defined(XCB)}
                          const pConnection:Pxcb_connection;pWindow:Pxcb_window
{$ifend}
                         );
       destructor Destroy; override;
      published
       property Handle:TVkSurfaceKHR read fSurfaceHandle;
     end;

     TVulkanDeviceQueueCreateInfo=class;

     TVulkanDeviceQueueCreateInfoList=class;

     TVulkanDeviceMemoryManager=class;

     TVulkanQueue=class;

     TVulkanQueues=array of TVulkanQueue; 

     TVulkanDevice=class(TVulkanObject)
      private
       fInstance:TVulkanInstance;
       fPhysicalDevice:TVulkanPhysicalDevice;
       fSurface:TVulkanSurface;
       fDeviceQueueCreateInfoList:TVulkanDeviceQueueCreateInfoList;
       fDeviceQueueCreateInfos:TVkDeviceQueueCreateInfoArray;
       fEnabledLayerNames:TStringList;
       fEnabledExtensionNames:TStringList;
       fEnabledLayerNameStrings:array of TVulkanCharString;
       fEnabledExtensionNameStrings:array of TVulkanCharString;
       fRawEnabledLayerNameStrings:array of PVkChar;
       fRawEnabledExtensionNameStrings:array of PVkChar;
       fEnabledFeatures:TVkPhysicalDeviceFeatures;
       fPointerToEnabledFeatures:PVkPhysicalDeviceFeatures;
       fAllocationManager:TVulkanAllocationManager;
       fAllocationCallbacks:PVkAllocationCallbacks;
       fDeviceHandle:TVkDevice;
       fDeviceVulkan:TVulkan;
       fPresentQueueFamilyIndex:TVkInt32;
       fGraphicsQueueFamilyIndex:TVkInt32;
       fComputeQueueFamilyIndex:TVkInt32;
       fTransferQueueFamilyIndex:TVkInt32;
       fQueues:TVulkanQueues;
       fPresentQueue:TVulkanQueue;
       fGraphicsQueue:TVulkanQueue;
       fComputeQueue:TVulkanQueue;
       fTransferQueue:TVulkanQueue;
       fMemoryManager:TVulkanDeviceMemoryManager;
      protected
      public
       constructor Create(const pInstance:TVulkanInstance;
                          const pPhysicalDevice:TVulkanPhysicalDevice=nil;
                          const pSurface:TVulkanSurface=nil;
                          const pAllocationManager:TVulkanAllocationManager=nil);
       destructor Destroy; override;
       procedure AddQueue(const pQueueFamilyIndex:TVkUInt32;const pQueuePriorities:array of TVkFloat);
       procedure AddQueues(const pPresent:boolean=true;
                           const pGraphics:boolean=true;
                           const pCompute:boolean=true;
                           const pTransfer:boolean=true;
                           const pSparseBinding:boolean=false);
       procedure Initialize;
       procedure WaitIdle;
       property EnabledFeatures:PVkPhysicalDeviceFeatures read fPointerToEnabledFeatures;
      published
       property PhysicalDevice:TVulkanPhysicalDevice read fPhysicalDevice;
       property Surface:TVulkanSurface read fSurface;
       property EnabledLayerNames:TStringList read fEnabledLayerNames;
       property EnabledExtensionNames:TStringList read fEnabledExtensionNames;
       property Handle:TVkDevice read fDeviceHandle;
       property Commands:TVulkan read fDeviceVulkan;
       property PresentQueueFamilyIndex:TVkInt32 read fPresentQueueFamilyIndex;
       property GraphicsQueueFamilyIndex:TVkInt32 read fGraphicsQueueFamilyIndex;
       property ComputeQueueFamilyIndex:TVkInt32 read fComputeQueueFamilyIndex;
       property TransferQueueFamilyIndex:TVkInt32 read fTransferQueueFamilyIndex;
       property PresentQueue:TVulkanQueue read fPresentQueue;
       property GraphicsQueue:TVulkanQueue read fGraphicsQueue;
       property ComputeQueue:TVulkanQueue read fComputeQueue;
       property TransferQueue:TVulkanQueue read fTransferQueue;
       property MemoryManager:TVulkanDeviceMemoryManager read fMemoryManager;
     end;

     TVulkanDeviceQueueCreateInfo=class(TVulkanObject)
      private
       fQueueFamilyIndex:TVkUInt32;
       fQueuePriorities:TVkFloatArray;
      public
       constructor Create(const pQueueFamilyIndex:TVkUInt32;const pQueuePriorities:array of TVkFloat);
       destructor Destroy; override;
      published
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
      published
       property Device:TVulkanDevice read fDevice write fDevice;
       property OwnsResource:boolean read fOwnsResource write fOwnsResource;
     end;

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
      published
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
      published
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
      published
       property MemoryChunk:TVulkanDeviceMemoryChunk read fMemoryChunk;
       property Offset:TVkDeviceSize read fOffset;
       property Size:TVkDeviceSize read fSize;
       property Used:boolean read fUsed;
     end;

     PVulkanDeviceMemoryManagerChunkList=^TVulkanDeviceMemoryManagerChunkList;
     PVulkanDeviceMemoryManagerChunkLists=^TVulkanDeviceMemoryManagerChunkLists;

     TVulkanDeviceMemoryChunk=class(TVulkanObject)
      private
       fMemoryManager:TVulkanDeviceMemoryManager;
       fPreviousMemoryChunk:TVulkanDeviceMemoryChunk;
       fNextMemoryChunk:TVulkanDeviceMemoryChunk;
       fLock:TCriticalSection;
       fAlignment:TVkDeviceSize;
       fMemoryChunkList:PVulkanDeviceMemoryManagerChunkList;
       fSize:TVkDeviceSize;
       fUsed:TVkDeviceSize;
       fMappedOffset:TVkDeviceSize;
       fMappedSize:TVkDeviceSize;
       fOffsetRedBlackTree:TVulkanDeviceMemoryChunkBlockRedBlackTree;
       fSizeRedBlackTree:TVulkanDeviceMemoryChunkBlockRedBlackTree;
       fMemoryTypeIndex:TVkUInt32;
       fMemoryTypeBits:TVkUInt32;
       fMemoryHeapIndex:TVkUInt32;
       fMemoryPropertyFlags:TVkMemoryPropertyFlags;
       fMemoryHandle:TVkDeviceMemory;
       fMemory:PVkVoid;
      public
       constructor Create(const pMemoryManager:TVulkanDeviceMemoryManager;
                          const pSize:TVkDeviceSize;
                          const pAlignment:TVkDeviceSize;
                          const pMemoryTypeBits:TVkUInt32;
                          const pMemoryPropertyFlags:TVkMemoryPropertyFlags;
                          const pMemoryChunkList:PVulkanDeviceMemoryManagerChunkList;
                          const pMemoryHeapFlags:TVkMemoryHeapFlags=0);
       destructor Destroy; override;
       function AllocateMemory(out pOffset:TVkDeviceSize;const pSize:TVkDeviceSize):boolean;
       function ReallocateMemory(var pOffset:TVkDeviceSize;const pSize:TVkDeviceSize):boolean;
       function FreeMemory(const pOffset:TVkDeviceSize):boolean;
       function MapMemory(const pOffset:TVkDeviceSize=0;const pSize:TVkDeviceSize=TVkDeviceSize(VK_WHOLE_SIZE)):PVkVoid;
       procedure UnmapMemory;
       procedure FlushMappedMemory;
       procedure InvalidateMappedMemory;
       property Memory:PVkVoid read fMemory;
      published
       property MemoryManager:TVulkanDeviceMemoryManager read fMemoryManager;
       property Size:TVkDeviceSize read fSize;
       property MemoryPropertyFlags:TVkMemoryPropertyFlags read fMemoryPropertyFlags;
       property MemoryTypeIndex:TVkUInt32 read fMemoryTypeIndex;
       property MemoryTypeBits:TVkUInt32 read fMemoryTypeBits;
       property MemoryHeapIndex:TVkUInt32 read fMemoryHeapIndex;
       property Handle:TVkDeviceMemory read fMemoryHandle;
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
       function MapMemory(const pOffset:TVkDeviceSize=0;const pSize:TVkDeviceSize=TVkDeviceSize(VK_WHOLE_SIZE)):PVkVoid;
       procedure UnmapMemory;
       procedure FlushMappedMemory;
       procedure InvalidateMappedMemory;
       function Fill(const pData:PVkVoid;const pSize:TVkDeviceSize):TVkDeviceSize;
      published
       property MemoryManager:TVulkanDeviceMemoryManager read fMemoryManager;
       property MemoryChunk:TVulkanDeviceMemoryChunk read fMemoryChunk;
       property Offset:TVkDeviceSize read fOffset;
       property Size:TVkDeviceSize read fSize;
     end;

     TVulkanDeviceMemoryManagerChunkList=record
      First:TVulkanDeviceMemoryChunk;
      Last:TVulkanDeviceMemoryChunk;
     end;

     TVulkanDeviceMemoryManagerChunkLists=array[0..31] of TVulkanDeviceMemoryManagerChunkList;

     TVulkanDeviceMemoryManager=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fLock:TCriticalSection;
       fMemoryChunkLists:TVulkanDeviceMemoryManagerChunkLists;
       fFirstMemoryBlock:TVulkanDeviceMemoryBlock;
       fLastMemoryBlock:TVulkanDeviceMemoryBlock;
      public
       constructor Create(const pDevice:TVulkanDevice);
       destructor Destroy; override;
       function AllocateMemoryBlock(const pSize:TVkDeviceSize;
                                    const pMemoryTypeBits:TVkUInt32;
                                    const pMemoryPropertyFlags:TVkMemoryPropertyFlags;
                                    const pAlignment:TVkDeviceSize=16;
                                    const pOwnSingleMemoryChunk:boolean=false):TVulkanDeviceMemoryBlock;
       function FreeMemoryBlock(const pMemoryBlock:TVulkanDeviceMemoryBlock):boolean;
     end;

     TVulkanQueueFamilyIndices=array of TVkUInt32;

     TVulkanBuffer=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fSize:TVkDeviceSize;
       fMemoryProperties:TVkMemoryPropertyFlags;
       fOwnSingleMemoryChunk:boolean;
       fBufferHandle:TVkBuffer;
       fMemoryRequirements:TVkMemoryRequirements;
       fMemoryBlock:TVulkanDeviceMemoryBlock;
       fQueueFamilyIndices:TVulkanQueueFamilyIndices;
       fCountQueueFamilyIndices:TVkInt32;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pSize:TVkDeviceSize;
                          const pUsage:TVkBufferUsageFlags;
                          const pSharingMode:TVkSharingMode=VK_SHARING_MODE_EXCLUSIVE;
                          const pQueueFamilyIndices:TVkUInt32List=nil;
                          const pMemoryProperties:TVkMemoryPropertyFlags=TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
                          const pOwnSingleMemoryChunk:boolean=false);
       destructor Destroy; override;
       procedure Bind;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkBuffer read fBufferHandle;
       property Size:TVkDeviceSize read fSize;
       property Memory:TVulkanDeviceMemoryBlock read fMemoryBlock;
     end;

     TVulkanBufferView=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fBufferViewHandle:TVkBufferView;
       fBuffer:TVulkanBuffer;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pBuffer:TVulkanBuffer;
                          const pFormat:TVkFormat;
                          const pOffset:TVkDeviceSize;
                          const pRange:TVkDeviceSize); reintroduce; overload;
       constructor Create(const pDevice:TVulkanDevice;
                          const pBufferView:TVkBufferView;
                          const pBuffer:TVulkanBuffer=nil); reintroduce; overload;
       destructor Destroy; override;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkRenderPass read fBufferViewHandle;
       property Buffer:TVulkanBuffer read fBuffer write fBuffer;
     end;

     TVulkanEvent=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fEventHandle:TVkEvent;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pFlags:TVkEventCreateFlags=TVkEventCreateFlags(0));
       destructor Destroy; override;
       function GetStatus:TVkResult;
       function SetEvent:TVkResult;
       function Reset:TVkResult;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkEvent read fEventHandle;
     end;

     TVulkanFence=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fFenceHandle:TVkFence;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pFlags:TVkFenceCreateFlags=TVkFenceCreateFlags(0));
       destructor Destroy; override;
       function GetStatus:TVkResult;
       function Reset:TVkResult; overload;
       class function Reset(const pFences:array of TVulkanFence):TVkResult; overload;
       function WaitFor(const pTimeOut:TVkUInt64=TVKUInt64(TVKInt64(-1))):TVkResult; overload;
       class function WaitFor(const pFences:array of TVulkanFence;const pWaitAll:boolean=true;const pTimeOut:TVkUInt64=TVKUInt64(TVKInt64(-1))):TVkResult; overload;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkFence read fFenceHandle;
     end;

     TVulkanSemaphore=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fSemaphoreHandle:TVkSemaphore;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pFlags:TVkSemaphoreCreateFlags=TVkSemaphoreCreateFlags(0));
       destructor Destroy; override;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkSemaphore read fSemaphoreHandle;
     end;

     TVulkanQueue=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fQueueHandle:TVkQueue;
       fQueueFamilyIndex:TVkUInt32;
       fHasSupportForSparseBindings:boolean;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pQueue:TVkQueue;
                          const pQueueFamilyIndex:TVKUInt32);
       destructor Destroy; override;
       procedure Submit(const pSubmitCount:TVkUInt32;const pSubmits:PVkSubmitInfo;const pFence:TVulkanFence=nil);
       procedure BindSparse(const pBindInfoCount:TVkUInt32;const pBindInfo:PVkBindSparseInfo;const pFence:TVulkanFence=nil);
       procedure WaitIdle;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkQueue read fQueueHandle;
       property QueueFamilyIndex:TVkUInt32 read fQueueFamilyIndex;
       property HasSupportForSparseBindings:boolean read fHasSupportForSparseBindings;
     end;

     TVulkanCommandPool=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fQueueFamilyIndex:TVkUInt32;
       fFlags:TVkCommandPoolCreateFlags;
       fCommandPoolHandle:TVkCommandPool;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pQueueFamilyIndex:TVkUInt32;
                          const pFlags:TVkCommandPoolCreateFlags=TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
       destructor Destroy; override;
      published
       property Device:TVulkanDevice read fDevice;
       property QueueFamilyIndex:TVkUInt32 read fQueueFamilyIndex;
       property Handle:TVkCommandPool read fCommandPoolHandle;
     end;

     TVulkanImage=class;

     TVulkanCommandBuffer=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fCommandPool:TVulkanCommandPool;
       fLevel:TVkCommandBufferLevel;
       fCommandBufferHandle:TVkCommandBuffer;
//     fFence:TVulkanFence;
      public
       constructor Create(const pCommandPool:TVulkanCommandPool;
                          const pLevel:TVkCommandBufferLevel;
                          const pCommandBufferHandle:TVkCommandBuffer); reintroduce; overload;
       constructor Create(const pCommandPool:TVulkanCommandPool;
                          const pLevel:TVkCommandBufferLevel=VK_COMMAND_BUFFER_LEVEL_PRIMARY); reintroduce; overload;
       destructor Destroy; override;
       class function Allocate(const pCommandPool:TVulkanCommandPool;
                               const pLevel:TVkCommandBufferLevel=VK_COMMAND_BUFFER_LEVEL_PRIMARY;
                               const pCommandBufferCount:TVkUInt32=1):TVulkanObjectList;
       procedure BeginRecording(const pFlags:TVkCommandBufferUsageFlags=0;const pInheritanceInfo:PVkCommandBufferInheritanceInfo=nil);
       procedure BeginRecordingPrimary;
       procedure BeginRecordingSecondary(const pRenderPass:TVkRenderPass;const pSubPass:TVkUInt32;const pFrameBuffer:TVkFramebuffer;const pOcclusionQueryEnable:boolean;const pQueryFlags:TVkQueryControlFlags;const pPipelineStatistics:TVkQueryPipelineStatisticFlags);
       procedure EndRecording;
       procedure Reset(const pFlags:TVkCommandBufferResetFlags=TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));
       procedure CmdBindPipeline(pipelineBindPoint:TVkPipelineBindPoint;pipeline:TVkPipeline);
       procedure CmdSetViewport(firstViewport:TVkUInt32;viewportCount:TVkUInt32;const pViewports:PVkViewport);
       procedure CmdSetScissor(firstScissor:TVkUInt32;scissorCount:TVkUInt32;const pScissors:PVkRect2D);
       procedure CmdSetLineWidth(lineWidth:TVkFloat);
       procedure CmdSetDepthBias(depthBiasConstantFactor:TVkFloat;depthBiasClamp:TVkFloat;depthBiasSlopeFactor:TVkFloat);
       procedure CmdSetBlendConstants(const blendConstants:TVkFloat);
       procedure CmdSetDepthBounds(minDepthBounds:TVkFloat;maxDepthBounds:TVkFloat);
       procedure CmdSetStencilCompareMask(faceMask:TVkStencilFaceFlags;compareMask:TVkUInt32);
       procedure CmdSetStencilWriteMask(faceMask:TVkStencilFaceFlags;writeMask:TVkUInt32);
       procedure CmdSetStencilReference(faceMask:TVkStencilFaceFlags;reference:TVkUInt32);
       procedure CmdBindDescriptorSets(pipelineBindPoint:TVkPipelineBindPoint;layout:TVkPipelineLayout;firstSet:TVkUInt32;descriptorSetCount:TVkUInt32;const pDescriptorSets:PVkDescriptorSet;dynamicOffsetCount:TVkUInt32;const pDynamicOffsets:PVkUInt32);
       procedure CmdBindIndexBuffer(buffer:TVkBuffer;offset:TVkDeviceSize;indexType:TVkIndexType);
       procedure CmdBindVertexBuffers(firstBinding:TVkUInt32;bindingCount:TVkUInt32;const pBuffers:PVkBuffer;const pOffsets:PVkDeviceSize);
       procedure CmdDraw(vertexCount:TVkUInt32;instanceCount:TVkUInt32;firstVertex:TVkUInt32;firstInstance:TVkUInt32);
       procedure CmdDrawIndexed(indexCount:TVkUInt32;instanceCount:TVkUInt32;firstIndex:TVkUInt32;vertexOffset:TVkInt32;firstInstance:TVkUInt32);
       procedure CmdDrawIndirect(buffer:TVkBuffer;offset:TVkDeviceSize;drawCount:TVkUInt32;stride:TVkUInt32);
       procedure CmdDrawIndexedIndirect(buffer:TVkBuffer;offset:TVkDeviceSize;drawCount:TVkUInt32;stride:TVkUInt32);
       procedure CmdDispatch(x:TVkUInt32;y:TVkUInt32;z:TVkUInt32);
       procedure CmdDispatchIndirect(buffer:TVkBuffer;offset:TVkDeviceSize);
       procedure CmdCopyBuffer(srcBuffer:TVkBuffer;dstBuffer:TVkBuffer;regionCount:TVkUInt32;const pRegions:PVkBufferCopy);
       procedure CmdCopyImage(srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageCopy);
       procedure CmdBlitImage(srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageBlit;filter:TVkFilter);
       procedure CmdCopyBufferToImage(srcBuffer:TVkBuffer;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkBufferImageCopy);
       procedure CmdCopyImageToBuffer(srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstBuffer:TVkBuffer;regionCount:TVkUInt32;const pRegions:PVkBufferImageCopy);
       procedure CmdUpdateBuffer(dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;dataSize:TVkDeviceSize;const pData:PVkVoid);
       procedure CmdFillBuffer(dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;size:TVkDeviceSize;data:TVkUInt32);
       procedure CmdClearColorImage(image:TVkImage;imageLayout:TVkImageLayout;const pColor:PVkClearColorValue;rangeCount:TVkUInt32;const pRanges:PVkImageSubresourceRange);
       procedure CmdClearDepthStencilImage(image:TVkImage;imageLayout:TVkImageLayout;const pDepthStencil:PVkClearDepthStencilValue;rangeCount:TVkUInt32;const pRanges:PVkImageSubresourceRange);
       procedure CmdClearAttachments(attachmentCount:TVkUInt32;const pAttachments:PVkClearAttachment;rectCount:TVkUInt32;const pRects:PVkClearRect);
       procedure CmdResolveImage(srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageResolve);
       procedure CmdSetEvent(event:TVkEvent;stageMask:TVkPipelineStageFlags);
       procedure CmdResetEvent(event:TVkEvent;stageMask:TVkPipelineStageFlags);
       procedure CmdWaitEvents(eventCount:TVkUInt32;const pEvents:PVkEvent;srcStageMask:TVkPipelineStageFlags;dstStageMask:TVkPipelineStageFlags;memoryBarrierCount:TVkUInt32;const pMemoryBarriers:PVkMemoryBarrier;bufferMemoryBarrierCount:TVkUInt32;const pBufferMemoryBarriers:PVkBufferMemoryBarrier;imageMemoryBarrierCount:TVkUInt32;const pImageMemoryBarriers:PVkImageMemoryBarrier);
       procedure CmdPipelineBarrier(srcStageMask:TVkPipelineStageFlags;dstStageMask:TVkPipelineStageFlags;dependencyFlags:TVkDependencyFlags;memoryBarrierCount:TVkUInt32;const pMemoryBarriers:PVkMemoryBarrier;bufferMemoryBarrierCount:TVkUInt32;const pBufferMemoryBarriers:PVkBufferMemoryBarrier;imageMemoryBarrierCount:TVkUInt32;const pImageMemoryBarriers:PVkImageMemoryBarrier);
       procedure CmdBeginQuery(queryPool:TVkQueryPool;query:TVkUInt32;flags:TVkQueryControlFlags);
       procedure CmdEndQuery(queryPool:TVkQueryPool;query:TVkUInt32);
       procedure CmdResetQueryPool(queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32);
       procedure CmdWriteTimestamp(pipelineStage:TVkPipelineStageFlagBits;queryPool:TVkQueryPool;query:TVkUInt32);
       procedure CmdCopyQueryPoolResults(queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;stride:TVkDeviceSize;flags:TVkQueryResultFlags);
       procedure CmdPushConstants(layout:TVkPipelineLayout;stageFlags:TVkShaderStageFlags;offset:TVkUInt32;size:TVkUInt32;const pValues:PVkVoid);
       procedure CmdBeginRenderPass(const pRenderPassBegin:PVkRenderPassBeginInfo;contents:TVkSubpassContents);
       procedure CmdNextSubpass(contents:TVkSubpassContents);
       procedure CmdEndRenderPass;
       procedure CmdExecuteCommands(commandBufferCount:TVkUInt32;const pCommandBuffers:PVkCommandBuffer);
       procedure CmdExecute(const pCommandBuffer:TVulkanCommandBuffer);
       procedure MetaCmdPresentToDrawImageBarrier(const pImage:TVulkanImage);
       procedure MetaCmdDrawToPresentImageBarrier(const pImage:TVulkanImage);
       procedure Execute(const pQueue:TVulkanQueue;const pFence:TVulkanFence;const pFlags:TVkPipelineStageFlags;const pWaitSemaphore:TVulkanSemaphore=nil;const pSignalSemaphore:TVulkanSemaphore=nil);
      published
       property Device:TVulkanDevice read fDevice;
       property CommandPool:TVulkanCommandPool read fCommandPool;
       property Level:TVkCommandBufferLevel read fLevel;
       property Handle:TVkCommandBuffer read fCommandBufferHandle;
     end;

     TVulkanRenderPassAttachmentDescriptions=array of TVkAttachmentDescription;

     TVulkanRenderPassAttachmentReferences=array of TVkAttachmentReference;

     PVulkanRenderPassSubpassDescription=^TVulkanRenderPassSubpassDescription;
     TVulkanRenderPassSubpassDescription=record
      Flags:TVkSubpassDescriptionFlags;
      PipelineBindPoint:TVkPipelineBindPoint;
      InputAttachments:array of TVkInt32;
      ColorAttachments:array of TVkInt32;
      ResolveAttachments:array of TVkInt32;
      DepthStencilAttachment:TVkInt32;
      PreserveAttachments:array of TVkUInt32;
      pInputAttachments:TVulkanRenderPassAttachmentReferences;
      pColorAttachments:TVulkanRenderPassAttachmentReferences;
      pResolveAttachments:TVulkanRenderPassAttachmentReferences;
     end;

     TVulkanRenderPassSubpassDescriptions=array of TVulkanRenderPassSubpassDescription;

     TVulkanFrameBuffer=class;

     TVulkanRenderPass=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fRenderPassHandle:TVkRenderPass;
       fAttachmentDescriptions:TVulkanRenderPassAttachmentDescriptions;
       fCountAttachmentDescriptions:TVkInt32;
       fAttachmentReferences:TVulkanRenderPassAttachmentReferences;
       fCountAttachmentReferences:TVkInt32;
       fRenderPassSubpassDescriptions:TVulkanRenderPassSubpassDescriptions;
       fSubpassDescriptions:TVkSubpassDescriptionArray;
       fCountSubpassDescriptions:TVkInt32;
       fSubpassDependencies:TVkSubpassDependencyArray;
       fCountSubpassDependencies:TVkInt32;
       fClearValues:TVkClearValueArray;
       function GetClearValue(const Index:TVkUInt32):PVkClearValue;
      public
       constructor Create(const pDevice:TVulkanDevice);
       destructor Destroy; override;
       function AddAttachmentDescription(const pFlags:TVkAttachmentDescriptionFlags;
                                         const pFormat:TVkFormat;
                                         const pSamples:TVkSampleCountFlagBits;
                                         const pLoadOp:TVkAttachmentLoadOp;
                                         const pStoreOp:TVkAttachmentStoreOp;
                                         const pStencilLoadOp:TVkAttachmentLoadOp;
                                         const pStencilStoreOp:TVkAttachmentStoreOp;
                                         const pInitialLayout:TVkImageLayout;
                                         const pFinalLayout:TVkImageLayout):TVkUInt32;
       function AddAttachmentReference(const pAttachment:TVkUInt32;
                                       const pLayout:TVkImageLayout):TVkUInt32;
       function AddSubpassDescription(const pFlags:TVkSubpassDescriptionFlags;
                                      const pPipelineBindPoint:TVkPipelineBindPoint;
                                      const pInputAttachments:array of TVkInt32;
                                      const pColorAttachments:array of TVkInt32;
                                      const pResolveAttachments:array of TVkInt32;
                                      const pDepthStencilAttachment:TVkInt32;
                                      const pPreserveAttachments:array of TVkUInt32):TVkUInt32;
       function AddSubpassDependency(const pSrcSubpass:TVkUInt32;
                                     const pDstSubpass:TVkUInt32;
                                     const pSrcStageMask:TVkPipelineStageFlags;
                                     const pDstStageMask:TVkPipelineStageFlags;
                                     const pSrcAccessMask:TVkAccessFlags;
                                     const pDstAccessMask:TVkAccessFlags;
                                     const pDependencyFlags:TVkDependencyFlags):TVkUInt32;
       procedure Initialize;
       procedure BeginRenderPass(const pCommandBuffer:TVulkanCommandBuffer;
                                 const pFrameBuffer:TVulkanFrameBuffer;
                                 const pSubpassContents:TVkSubpassContents;
                                 const pOffsetX,pOffsetY,pWidth,pHeight:TVkUInt32);
       procedure EndRenderPass(const pCommandBuffer:TVulkanCommandBuffer);
       property ClearValues[const Index:TVkUInt32]:PVkClearValue read GetClearValue;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkRenderPass read fRenderPassHandle;
     end;

     TVulkanSampler=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fSamplerHandle:TVkSampler;
       fDoDestroy:boolean;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pSampler:TVkSampler;
                          const pDoDestroy:boolean=true); reintroduce; overload;
       constructor Create(const pDevice:TVulkanDevice;
                          const pMagFilter:TVkFilter;
                          const pMinFilter:TVkFilter;
                          const pMipmapMode:TVkSamplerMipmapMode;
                          const pAddressModeU:TVkSamplerAddressMode;
                          const pAddressModeV:TVkSamplerAddressMode;
                          const pAddressModeW:TVkSamplerAddressMode;
                          const pMipLodBias:TVkFloat;
                          const pAnisotropyEnable:boolean;
                          const pMaxAnisotropy:TVkFloat;
                          const pCompareEnable:boolean;
                          const pCompareOp:TVkCompareOp;
                          const pMinLod:TVkFloat;
                          const pMaxLod:TVkFloat;
                          const pBorderColor:TVkBorderColor;
                          const pUnnormalizedCoordinates:boolean); reintroduce; overload;
       destructor Destroy; override;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkSampler read fSamplerHandle;
     end;

     TVulkanImageView=class;

     TVulkanImage=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fImageHandle:TVkImage;
       fImageView:TVulkanImageView;
       fDoDestroy:boolean;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pImage:TVkImage;
                          const pImageView:TVulkanImageView=nil;
                          const pDoDestroy:boolean=true); reintroduce; overload;
       constructor Create(const pDevice:TVulkanDevice;
                          const pFlags:TVkImageCreateFlags;
                          const pImageType:TVkImageType;
                          const pFormat:TVkFormat;
                          const pExtentWidth:TVkUInt32;
                          const pExtentHeight:TVkUInt32;
                          const pExtentDepth:TVkUInt32;
                          const pMipLevels:TVkUInt32;
                          const pArrayLayers:TVkUInt32;
                          const pSamples:TVkSampleCountFlagBits;
                          const pTiling:TVkImageTiling;
                          const pUsage:TVkImageUsageFlags;
                          const pSharingMode:TVkSharingMode;
                          const pQueueFamilyIndexCount:TVkUInt32;
                          const pQueueFamilyIndices:PVkUInt32;
                          const pInitialLayout:TVkImageLayout); reintroduce; overload;
       constructor Create(const pDevice:TVulkanDevice;
                          const pFlags:TVkImageCreateFlags;
                          const pImageType:TVkImageType;
                          const pFormat:TVkFormat;
                          const pExtentWidth:TVkUInt32;
                          const pExtentHeight:TVkUInt32;
                          const pExtentDepth:TVkUInt32;
                          const pMipLevels:TVkUInt32;
                          const pArrayLayers:TVkUInt32;
                          const pSamples:TVkSampleCountFlagBits;
                          const pTiling:TVkImageTiling;
                          const pUsage:TVkImageUsageFlags;
                          const pSharingMode:TVkSharingMode;
                          const pQueueFamilyIndices:array of TVkUInt32;
                          const pInitialLayout:TVkImageLayout); reintroduce; overload;
       destructor Destroy; override;
       procedure SetLayout(const pAspectMask:TVkImageAspectFlags;
                           const pOldImageLayout:TVkImageLayout;
                           const pNewImageLayout:TVkImageLayout;
                           const pRange:PVkImageSubresourceRange;
                           const pCommandBuffer:TVulkanCommandBuffer;
                           const pQueue:TVulkanQueue=nil;
                           const pFence:TVulkanFence=nil;
                           const pBeginAndExecuteCommandBuffer:boolean=false;
                           const pSrcQueueFamilyIndex:TVkQueue=TVkQueue(VK_QUEUE_FAMILY_IGNORED);
                           const pDstQueueFamilyIndex:TVkQueue=TVkQueue(VK_QUEUE_FAMILY_IGNORED));
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkImage read fImageHandle;
       property ImageView:TVulkanImageView read fImageView write fImageView;
     end;

     TVulkanImageView=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fImageViewHandle:TVkImageView;
       fImage:TVulkanImage;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pImageView:TVkImageView;
                          const pImage:TVulkanImage=nil); reintroduce; overload;
       constructor Create(const pDevice:TVulkanDevice;
                          const pImage:TVulkanImage;
                          const pImageViewType:TVkImageViewType;
                          const pFormat:TvkFormat;
                          const pComponentRed:TVkComponentSwizzle=VK_COMPONENT_SWIZZLE_IDENTITY;
                          const pComponentGreen:TVkComponentSwizzle=VK_COMPONENT_SWIZZLE_IDENTITY;
                          const pComponentBlue:TVkComponentSwizzle=VK_COMPONENT_SWIZZLE_IDENTITY;
                          const pComponentAlpha:TVkComponentSwizzle=VK_COMPONENT_SWIZZLE_IDENTITY;
                          const pImageAspectFlags:TVkImageAspectFlags=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
                          const pBaseMipLevel:TVkUInt32=0;
                          const pCountMipMapLevels:TVkUInt32=1;
                          const pBaseArrayLayer:TVkUInt32=1;
                          const pCountArrayLayers:TVkUInt32=0); reintroduce; overload;
       destructor Destroy; override;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkRenderPass read fImageViewHandle;
       property Image:TVulkanImage read fImage write fImage;
     end;

     TVulkanFrameBufferAttachment=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fWidth:TVkUInt32;
       fHeight:TVkUInt32;
       fFormat:TVkFormat;
       fImage:TVulkanImage;
       fImageView:TVulkanImageView;
       fMemoryBlock:TVulkanDeviceMemoryBlock;
       fDoDestroy:boolean;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pCommandBuffer:TVulkanCommandBuffer;
                          const pCommandBufferFence:TVulkanFence;
                          const pWidth:TVkUInt32;
                          const pHeight:TVkUInt32;
                          const pFormat:TVkFormat;
                          const pUsage:TVkBufferUsageFlags); reintroduce; overload;
       constructor Create(const pDevice:TVulkanDevice;
                          const pImage:TVulkanImage;
                          const pImageView:TVulkanImageView;
                          const pWidth:TVkUInt32;
                          const pHeight:TVkUInt32;
                          const pFormat:TVkFormat;
                          const pDoDestroy:boolean=true); reintroduce; overload;
       destructor Destroy; override;
      published
       property Device:TVulkanDevice read fDevice;
       property Width:TVkUInt32 read fWidth;
       property Height:TVkUInt32 read fHeight;
       property Format:TVkFormat read fFormat;
       property Image:TVulkanImage read fImage;
       property ImageView:TVulkanImageView read fImageView;
       property Memory:TVulkanDeviceMemoryBlock read fMemoryBlock;
     end;

     TVulkanFrameBufferAttachments=array of TVulkanFrameBufferAttachment;

     TVulkanFrameBufferAttachmentImageViews=array of TVkImageView;

     TVulkanFrameBuffer=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fFrameBufferHandle:TVkFrameBuffer;
       fFrameBufferAttachments:TVulkanFrameBufferAttachments;
       fFrameBufferAttachmentImageViews:TVulkanFrameBufferAttachmentImageViews;
       fCountFrameBufferAttachments:TVkInt32;
       fRenderPass:TVulkanRenderPass;
       fWidth:TVkUInt32;
       fHeight:TVkUInt32;
       fLayers:TVkUInt32;
       fDoDestroy:boolean;
       fDoDestroyAttachments:boolean;
       function GetFrameBufferAttachment(const pIndex:TVkInt32):TVulkanFrameBufferAttachment;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pRenderPass:TVulkanRenderPass;
                          const pWidth:TVkUInt32;
                          const pHeight:TVkUInt32;
                          const pLayers:TVkUInt32); reintroduce; overload;
       constructor Create(const pDevice:TVulkanDevice;
                          const pRenderPass:TVulkanRenderPass;
                          const pWidth:TVkUInt32;
                          const pHeight:TVkUInt32;
                          const pLayers:TVkUInt32;
                          const pFrameBufferAttachments:array of TVulkanFrameBufferAttachment;
                          const pDoDestroyAttachments:boolean=true); reintroduce; overload;
       constructor Create(const pDevice:TVulkanDevice;
                          const pRenderPass:TVulkanRenderPass;
                          const pWidth:TVkUInt32;
                          const pHeight:TVkUInt32;
                          const pLayers:TVkUInt32;
                          const pFrameBufferHandle:TVkFrameBuffer;
                          const pFrameBufferAttachments:array of TVulkanFrameBufferAttachment;
                          const pDoDestroy:boolean=true;
                          const pDoDestroyAttachments:boolean=true); reintroduce; overload;
       destructor Destroy; override;
       function AddAttachment(const pFrameBufferAttachment:TVulkanFrameBufferAttachment):TVkInt32;
       procedure Initialize;
       property Attachments[const pIndex:TVkInt32]:TVulkanFrameBufferAttachment read GetFrameBufferAttachment; default;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkFrameBuffer read fFrameBufferHandle;
       property CountAttachments:TVkInt32 read fCountFrameBufferAttachments;
       property RenderPass:TVulkanRenderPass read fRenderPass;
       property Width:TVkUInt32 read fWidth;
       property Height:TVkUInt32 read fHeight;
       property Layers:TVkUInt32 read fLayers;
     end;

     TVulkanSwapChainImages=array of TVulkanImage;

     TVulkanSwapChain=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fSwapChainHandle:TVkSwapChainKHR;
       fQueueFamilyIndices:TVulkanQueueFamilyIndices;
       fCountQueueFamilyIndices:TVkInt32;
       fImageFormat:TVkFormat;
       fImageColorSpace:TVkColorSpaceKHR;
       fImages:TVulkanSwapChainImages;
       fCurrentImageIndex:TVkUInt32;
       fCountImages:TVkUInt32;
       fWidth:TVkInt32;
       fHeight:TVkInt32;
       function GetImage(const pImageIndex:TVkInt32):TVulkanImage;
       function GetCurrentImage:TVulkanImage;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pOldSwapChain:TVulkanSwapChain=nil;
                          const pDesiredImageWidth:TVkUInt32=0;
                          const pDesiredImageHeight:TVkUInt32=0;
                          const pDesiredImageCount:TVkUInt32=2;
                          const pImageArrayLayers:TVkUInt32=1;
                          const pImageFormat:TVkFormat=VK_FORMAT_UNDEFINED;
                          const pImageColorSpace:TVkColorSpaceKHR=VK_COLOR_SPACE_SRGB_NONLINEAR_KHR;
                          const pImageUsage:TVkImageUsageFlags=TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT);
                          const pImageSharingMode:TVkSharingMode=VK_SHARING_MODE_EXCLUSIVE;
                          const pQueueFamilyIndices:TVkUInt32List=nil;
                          const pCompositeAlpha:TVkCompositeAlphaFlagBitsKHR=VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
                          const pPresentMode:TVkPresentModeKHR=VK_PRESENT_MODE_MAILBOX_KHR;
                          const pClipped:boolean=true;
                          const pDesiredTransform:TVkSurfaceTransformFlagsKHR=TVkSurfaceTransformFlagsKHR($ffffffff));
       destructor Destroy; override;
       function QueuePresent(const pQueue:TVulkanQueue;const pSemaphore:TVulkanSemaphore=nil):TVkResult;
       function AcquireNextImage(const pSemaphore:TVulkanSemaphore=nil;const pFence:TVulkanFence=nil;const pTimeOut:TVkUInt64=TVkUInt64(high(TVkUInt64))):TVkResult;
       property Images[const pImageIndex:TVkInt32]:TVulkanImage read GetImage; default;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkSwapChainKHR read fSwapChainHandle;
       property ImageFormat:TVkFormat read fImageFormat;
       property ImageColorSpace:TVkColorSpaceKHR read fImageColorSpace;
       property CurrentImageIndex:TVkUInt32 read fCurrentImageIndex;
       property CountImages:TVkUInt32 read fCountImages;
       property CurrentImage:TVulkanImage read GetCurrentImage;
       property Width:TVkInt32 read fWidth;
       property Height:TVkInt32 read fHeight;
     end;

     TVulkanRenderTarget=class(TVulkanObject)
      private
      protected
       function GetRenderPass:TVulkanRenderPass; virtual; abstract;
       function GetFrameBuffer:TVulkanFrameBuffer; virtual; abstract;
      public
      published
       property RenderPass:TVulkanRenderPass read GetRenderPass;
       property FrameBuffer:TVulkanFrameBuffer read GetFrameBuffer;
     end;

     TVulkanSwapChainSimpleDirectRenderTargetFrameBuffers=array of TVulkanFrameBuffer;

     TVulkanSwapChainSimpleDirectRenderTarget=class(TVulkanRenderTarget)
      private
       fDevice:TVulkanDevice;
       fSwapChain:TVulkanSwapChain;
       fDepthImageFormat:TVkFormat;
       fDepthFrameBufferAttachment:TVulkanFrameBufferAttachment;
       fFrameBufferColorAttachments:TVulkanFrameBufferAttachments;
       fFrameBuffers:TVulkanSwapChainSimpleDirectRenderTargetFrameBuffers;
       fRenderPass:TVulkanRenderPass;
      protected
       function GetRenderPass:TVulkanRenderPass; override;
       function GetFrameBuffer:TVulkanFrameBuffer; override;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pSwapChain:TVulkanSwapChain;
                          const pCommandBuffer:TVulkanCommandBuffer;
                          const pCommandBufferFence:TVulkanFence;
                          const pDepthImageFormat:TVkFormat=VK_FORMAT_UNDEFINED;
                          const pDepthImageFormatWithStencil:boolean=false);
       destructor Destroy; override;
      published
       property Device:TVulkanDevice read fDevice;
       property SwapChain:TVulkanSwapChain read fSwapChain;
       property DepthImageFormat:TVkFormat read fDepthImageFormat;
     end;

     TVulkanShaderModule=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fShaderModuleHandle:TVkShaderModule;
       fData:PVkVoid;
       fDataAligned:PVkVoid;
       fDataSize:TVkSize;
       procedure Load;
      public
       constructor Create(const pDevice:TVulkanDevice;const pData;const pDataSize:TVkSize); overload;
       constructor Create(const pDevice:TVulkanDevice;const pStream:TStream); overload;
       constructor Create(const pDevice:TVulkanDevice;const pFileName:string); overload;
       destructor Destroy; override;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkShaderModule read fShaderModuleHandle;
     end;

     TVulkanShaderModules=array of TVulkanShaderModule;

     TVulkanDescriptorPool=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fDescriptorPoolHandle:TVkDescriptorPool;
       fDescriptorPoolSizes:TVkDescriptorPoolSizeArray;
       fCountDescriptorPoolSizes:TVkInt32;
       fFlags:TVkDescriptorPoolCreateFlags;
       fMaxSets:TVkUInt32;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pFlags:TVkDescriptorPoolCreateFlags;
                          const pMaxSets:TVkUInt32);
       destructor Destroy; override;
       function AddDescriptorPoolSize(const pType:TVkDescriptorType;const pDescriptorCount:TVkUInt32):TVkInt32;
       procedure Initialize;
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkDescriptorPool read fDescriptorPoolHandle;
     end;

     TVulkanDescriptorSetLayoutBinding=class(TVulkanObject)
      private                     
       fDescriptorSetLayoutBinding:TVkDescriptorSetLayoutBinding;
       fImmutableSamplers:TVkSamplerArray;
       fCountImmutableSamplers:TVkInt32;
       function GetBinding:TVkUInt32;
       procedure SetBinding(const pBinding:TVkUInt32);
       function GetDescriptorType:TVkDescriptorType;
       procedure SetDescriptorType(const pDescriptorType:TVkDescriptorType);
       function GetDescriptorCount:TVkUInt32;
       procedure SetDescriptorCount(const pDescriptorCount:TVkUInt32);
       function GetStageFlags:TVkShaderStageFlags;
       procedure SetStageFlags(const pStageFlags:TVkShaderStageFlags);
      public
       constructor Create(const pBinding:TVkUInt32;
                          const pDescriptorType:TVkDescriptorType;
                          const pDescriptorCount:TVkUInt32;
                          const pStageFlags:TVkShaderStageFlags);
       destructor Destroy; override;
       procedure AddImmutableSampler(const pImmutableSampler:TVulkanSampler);
       procedure AddImmutableSamplers(const pImmutableSamplers:array of TVulkanSampler);
       procedure Initialize;
      published
       property Binding:TVkUInt32 read GetBinding write SetBinding;
       property DescriptorType:TVkDescriptorType read GetDescriptorType write SetDescriptorType;
       property DescriptorCount:TVkUInt32 read GetDescriptorCount write SetDescriptorCount;
       property StageFlags:TVkShaderStageFlags read GetStageFlags write SetStageFlags;
     end;

     TVulkanDescriptorSetLayout=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fDescriptorSetLayoutHandle:TVkDescriptorSetLayout;
       fDescriptorSetLayoutBindingList:TVulkanObjectList;
       fDescriptorSetLayoutBindingArray:TVkDescriptorSetLayoutBindingArray;
      public
       constructor Create(const pDevice:TVulkanDevice);
       destructor Destroy; override;
       procedure AddBinding(const pBinding:TVkUInt32;
                            const pDescriptorType:TVkDescriptorType;
                            const pDescriptorCount:TVkUInt32;
                            const pStageFlags:TVkShaderStageFlags;
                            const pImmutableSamplers:array of TVulkanSampler);
       procedure Initialize;
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkDescriptorSetLayout read fDescriptorSetLayoutHandle;
     end;

     PVulkanDescriptorSetWriteDescriptorSetMetaData=^TVulkanDescriptorSetWriteDescriptorSetMetaData;
     TVulkanDescriptorSetWriteDescriptorSetMetaData=record
      ImageInfo:array of TVkDescriptorImageInfo;
      BufferInfo:array of TVkDescriptorBufferInfo;
      TexelBufferView:array of TVkBufferView;
     end;

     TVulkanDescriptorSetWriteDescriptorSetMetaDataArray=array of TVulkanDescriptorSetWriteDescriptorSetMetaData;

     TVulkanDescriptorSet=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fDescriptorPool:TVulkanDescriptorPool;
       fDescriptorSetLayout:TVulkanDescriptorSetLayout;
       fDescriptorSetHandle:TVkDescriptorSet;
       fDescriptorSetAllocateInfo:TVkDescriptorSetAllocateInfo;
       fCopyDescriptorSetQueue:TVkCopyDescriptorSetArray;
       fCopyDescriptorSetQueueSize:TVkInt32;
       fWriteDescriptorSetQueue:TVkWriteDescriptorSetArray;
       fWriteDescriptorSetQueueMetaData:TVulkanDescriptorSetWriteDescriptorSetMetaDataArray;
       fWriteDescriptorSetQueueSize:TVkInt32;
      public
       constructor Create(const pDescriptorPool:TVulkanDescriptorPool;
                          const pDescriptorSetLayout:TVulkanDescriptorSetLayout);
       destructor Destroy; override;
       class function Allocate(const pDescriptorPool:TVulkanDescriptorPool;
                               const pDescriptorSetLayouts:array of TVulkanDescriptorSetLayout):TVulkanObjectList;
       procedure CopyFromDescriptorSet(const pSourceDescriptorSet:TVulkanDescriptorSet;
                                       const pSourceBinding:TVkUInt32;
                                       const pSourceArrayElement:TVkUInt32;
                                       const pDestinationBinding:TVkUInt32;
                                       const pDestinationArrayElement:TVkUInt32;
                                       const pDescriptorCount:TVkUInt32;
                                       const pDoInstant:boolean=false);
       procedure WriteToDescriptorSet(const pDestinationBinding:TVkUInt32;
                                      const pDestinationArrayElement:TVkUInt32;
                                      const pDescriptorCount:TVkUInt32;
                                      const pDescriptorType:TVkDescriptorType;
                                      const pImageInfo:array of TVkDescriptorImageInfo;
                                      const pBufferInfo:array of TVkDescriptorBufferInfo;
                                      const pTexelBufferView:array of TVkBufferView;
                                      const pDoInstant:boolean=false);
       procedure Flush;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkDescriptorSet read fDescriptorSetHandle;
       property DescriptorPool:TVulkanDescriptorPool read fDescriptorPool;
       property DescriptorSetLayout:TVulkanDescriptorSetLayout read fDescriptorSetLayout;
     end;

     TVulkanPipelineLayout=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fPipelineLayoutHandle:TVkPipelineLayout;
       fDescriptorSetLayouts:TVkDescriptorSetLayoutArray;
       fCountDescriptorSetLayouts:TVkInt32;
       fPushConstantRanges:TVkPushConstantRangeArray;
       fCountPushConstantRanges:TVkInt32;
      public
       constructor Create(const pDevice:TVulkanDevice);
       destructor Destroy; override;
       function AddDescriptorSetLayout(const pDescriptorSetLayout:TVkDescriptorSetLayout):TVkInt32; overload;
       function AddDescriptorSetLayout(const pDescriptorSetLayout:TVulkanDescriptorSetLayout):TVkInt32; overload;
       function AddDescriptorSetLayouts(const pDescriptorSetLayouts:array of TVkDescriptorSetLayout):TVkInt32; overload;
       function AddDescriptorSetLayouts(const pDescriptorSetLayouts:array of TVulkanDescriptorSetLayout):TVkInt32; overload;
       function AddPushConstantRange(const pPushConstantRange:TVkPushConstantRange):TVkInt32; overload;
       function AddPushConstantRange(const pStageFlags:TVkShaderStageFlags;const pOffset,pSize:TVkUInt32):TVkInt32; overload;
       function AddPushConstantRanges(const pPushConstantRanges:array of TVkPushConstantRange):TVkInt32;
       procedure Initialize;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkPipelineLayout read fPipelineLayoutHandle;
     end;

     TVulkanPipelineShaderStage=class(TVulkanObject)
      private
       fPipelineShaderStageCreateInfo:TVkPipelineShaderStageCreateInfo;
       fPointerToPipelineShaderStageCreateInfo:PVkPipelineShaderStageCreateInfo;
       fName:TVkCharString;
       fSpecializationInfo:PVkSpecializationInfo;
       fDoCopyAndDoFree:boolean;
       fSpecializationMapEntries:TVkSpecializationMapEntryArray;
       fCountSpecializationMapEntries:TVkInt32;
       fInitialized:boolean;
       procedure AllocateSpecializationInfo;
      public
       constructor Create(const pStage:TVkShaderStageFlagBits;
                          const pModule:TVulkanShaderModule;
                          const pName:TVkCharString);
       destructor Destroy; override;
       procedure AddSpecializationDataFromMemory(const pData:TVkPointer;const pDataSize:TVkSize;const pDoCopyAndDoFree:boolean=true);
       procedure AddSpecializationDataFromStream(const pStream:TStream);
       procedure AddSpecializationDataFromFile(const pFileName:string);
       function AddSpecializationMapEntry(const pSpecializationMapEntry:TVkSpecializationMapEntry):TVkInt32; overload;
       function AddSpecializationMapEntry(const pConstantID,pOffset:TVkUInt32;const pSize:TVkSize):TVkInt32; overload;
       function AddSpecializationMapEntries(const pSpecializationMapEntries:array of TVkSpecializationMapEntry):TVkInt32;
       procedure Initialize;
       property PipelineShaderStageCreateInfo:PVkPipelineShaderStageCreateInfo read fPointerToPipelineShaderStageCreateInfo;
      published
     end;

     TVulkanPipelineCache=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fPipelineCacheHandle:TVkPipelineCache;
      public
       constructor Create(const pDevice:TVulkanDevice;const pInitialData:pointer=nil;const pInitialDataSize:TVkSize=0);
       constructor CreateFromMemory(const pDevice:TVulkanDevice;const pInitialData:pointer;const pInitialDataSize:TVkSize);
       constructor CreateFromStream(const pDevice:TVulkanDevice;const pStream:TStream);
       constructor CreateFromFile(const pDevice:TVulkanDevice;const pFileName:string);
       destructor Destroy; override;
       procedure SaveToStream(const pStream:TStream);
       procedure SaveToFile(const pFileName:string);
       procedure Merge(const pSourcePipelineCache:TVulkanPipelineCache); overload;
       procedure Merge(const pSourcePipelineCaches:array of TVulkanPipelineCache); overload;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkPipelineCache read fPipelineCacheHandle;
     end;

     TVulkanPipeline=class(TVulkanObject)
      private
       fDevice:TVulkanDevice;
       fPipelineHandle:TVkPipeline;
      public
       constructor Create(const pDevice:TVulkanDevice);
       destructor Destroy; override;
      published
       property Device:TVulkanDevice read fDevice;
       property Handle:TVkPipeline read fPipelineHandle;
     end;

     TVulkanComputePipeline=class(TVulkanPipeline)
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pCache:TVulkanPipelineCache;
                          const pFlags:TVkPipelineCreateFlags;
                          const pStage:TVulkanPipelineShaderStage;
                          const pLayout:TVulkanPipelineLayout;
                          const pBasePipelineHandle:TVulkanPipeline;
                          const pBasePipelineIndex:TVkInt32); reintroduce;
     end;

     TVulkanPipelineState=class(TVulkanObject)
      public
       constructor Create;
       destructor Destroy; override;
     end;

     TVulkanPipelineVertexInputState=class(TVulkanPipelineState)
      private
       fVertexInputStateCreateInfo:TVkPipelineVertexInputStateCreateInfo;
       fPointerToVertexInputStateCreateInfo:PVkPipelineVertexInputStateCreateInfo;
       fVertexInputBindingDescriptions:TVkVertexInputBindingDescriptionArray;
       fCountVertexInputBindingDescriptions:TVkInt32;
       fVertexInputAttributeDescriptions:TVkVertexInputAttributeDescriptionArray;
       fCountVertexInputAttributeDescriptions:TVkInt32;
       function GetVertexInputBindingDescription(const pIndex:TVkInt32):PVkVertexInputBindingDescription;
       function GetVertexInputAttributeDescription(const pIndex:TVkInt32):PVkVertexInputAttributeDescription;
       procedure SetCountVertexInputBindingDescriptions(const pNewCount:TVkInt32);
       procedure SetCountVertexInputAttributeDescriptions(const pNewCount:TVkInt32);
       procedure Initialize;
      public
       constructor Create;
       destructor Destroy; override;
       procedure Assign(const pFrom:TVulkanPipelineVertexInputState);
       function AddVertexInputBindingDescription(const pVertexInputBindingDescription:TVkVertexInputBindingDescription):TVkInt32; overload;
       function AddVertexInputBindingDescription(const pBinding,pStride:TVkUInt32;const pInputRate:TVkVertexInputRate):TVkInt32; overload;
       function AddVertexInputBindingDescriptions(const pVertexInputBindingDescriptions:array of TVkVertexInputBindingDescription):TVkInt32;
       function AddVertexInputAttributeDescription(const pVertexInputAttributeDescription:TVkVertexInputAttributeDescription):TVkInt32; overload;
       function AddVertexInputAttributeDescription(const pLocation,pBinding:TVkUInt32;const pFormat:TVkFormat;const pOffset:TVkUInt32):TVkInt32; overload;
       function AddVertexInputAttributeDescriptions(const pVertexInputAttributeDescriptions:array of TVkVertexInputAttributeDescription):TVkInt32;
       property VertexInputStateCreateInfo:PVkPipelineVertexInputStateCreateInfo read fPointerToVertexInputStateCreateInfo;
       property VertexInputBindingDescriptions[const pIndex:TVkInt32]:PVkVertexInputBindingDescription read GetVertexInputBindingDescription;
       property VertexInputAttributeDescriptions[const pIndex:TVkInt32]:PVkVertexInputAttributeDescription read GetVertexInputAttributeDescription;
      published
       property CountVertexInputBindingDescriptions:TVkInt32 read fCountVertexInputBindingDescriptions write SetCountVertexInputBindingDescriptions;
       property CountVertexInputAttributeDescriptions:TVkInt32 read fCountVertexInputAttributeDescriptions write SetCountVertexInputAttributeDescriptions;
     end;

     TVulkanPipelineInputAssemblyState=class(TVulkanPipelineState)
      private
       fInputAssemblyStateCreateInfo:TVkPipelineInputAssemblyStateCreateInfo;
       fPointerToInputAssemblyStateCreateInfo:PVkPipelineInputAssemblyStateCreateInfo;
       function GetTopology:TVkPrimitiveTopology;
       procedure SetTopology(const pNewValue:TVkPrimitiveTopology);
       function GetPrimitiveRestartEnable:boolean;
       procedure SetPrimitiveRestartEnable(const pNewValue:boolean);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Assign(const pFrom:TVulkanPipelineInputAssemblyState);
       procedure SetInputAssemblyState(const pTopology:TVkPrimitiveTopology;const pPrimitiveRestartEnable:boolean);
       property InputAssemblyStateCreateInfo:PVkPipelineInputAssemblyStateCreateInfo read fPointerToInputAssemblyStateCreateInfo;
      published
       property Topology:TVkPrimitiveTopology read GetTopology write SetTopology;
       property PrimitiveRestartEnable:boolean read GetPrimitiveRestartEnable write SetPrimitiveRestartEnable;
     end;

     TVulkanPipelineTessellationState=class(TVulkanPipelineState)
      private
       fTessellationStateCreateInfo:TVkPipelineTessellationStateCreateInfo;
       fPointerToTessellationStateCreateInfo:PVkPipelineTessellationStateCreateInfo;
       function GetPatchControlPoints:TVkUInt32;
       procedure SetPatchControlPoints(const pNewValue:TVkUInt32);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Assign(const pFrom:TVulkanPipelineTessellationState);
       procedure SetTessellationState(const pPatchControlPoints:TVkUInt32);
       property TessellationStateCreateInfo:PVkPipelineTessellationStateCreateInfo read fPointerToTessellationStateCreateInfo;
      published
       property PatchControlPoints:TVkUInt32 read GetPatchControlPoints write SetPatchControlPoints;
     end;

     TVulkanPipelineViewPortState=class(TVulkanPipelineState)
      private
       fViewportStateCreateInfo:TVkPipelineViewportStateCreateInfo;
       fPointerToViewportStateCreateInfo:PVkPipelineViewportStateCreateInfo;
       fViewPorts:TVkViewportArray;
       fCountViewPorts:TVkInt32;
       fScissors:TVkRect2DArray;
       fCountScissors:TVkInt32;
       function GetViewPort(const pIndex:TVkInt32):PVkViewport;
       function GetScissor(const pIndex:TVkInt32):PVkRect2D;
       procedure SetCountViewPorts(const pNewCount:TVkInt32);
       procedure SetCountScissors(const pNewCount:TVkInt32);
       procedure Initialize;
      public
       constructor Create;
       destructor Destroy; override;
       procedure Assign(const pFrom:TVulkanPipelineViewPortState);
       function AddViewPort(const pViewPort:TVkViewport):TVkInt32; overload;
       function AddViewPort(const pX,pY,pWidth,pHeight,pMinDepth,pMaxDepth:TVkFloat):TVkInt32; overload;
       function AddViewPorts(const pViewPorts:array of TVkViewport):TVkInt32; overload;
       function AddScissor(const pScissor:TVkRect2D):TVkInt32; overload;
       function AddScissor(const pX,pY:TVkInt32;const pWidth,pHeight:TVkUInt32):TVkInt32; overload;
       function AddScissors(const pScissors:array of TVkRect2D):TVkInt32; overload;
       property ViewportStateCreateInfo:PVkPipelineViewportStateCreateInfo read fPointerToViewportStateCreateInfo;
       property ViewPorts[const pIndex:TVkInt32]:PVkViewport read GetViewPort;
       property Scissors[const pIndex:TVkInt32]:PVkRect2D read GetScissor;
      published
       property CountViewPorts:TVkInt32 read fCountViewPorts write SetCountViewPorts;
       property CountScissors:TVkInt32 read fCountScissors write SetCountScissors;
     end;

     TVulkanPipelineRasterizationState=class(TVulkanPipelineState)
      private
       fRasterizationStateCreateInfo:TVkPipelineRasterizationStateCreateInfo;
       fPointerToRasterizationStateCreateInfo:PVkPipelineRasterizationStateCreateInfo;
       function GetDepthClampEnable:boolean;
       procedure SetDepthClampEnable(const pNewValue:boolean);
       function GetRasterizerDiscardEnable:boolean;
       procedure SetRasterizerDiscardEnable(const pNewValue:boolean);
       function GetPolygonMode:TVkPolygonMode;
       procedure SetPolygonMode(const pNewValue:TVkPolygonMode);
       function GetCullMode:TVkCullModeFlags;
       procedure SetCullMode(const pNewValue:TVkCullModeFlags);
       function GetFrontFace:TVkFrontFace;
       procedure SetFrontFace(const pNewValue:TVkFrontFace);
       function GetDepthBiasEnable:boolean;
       procedure SetDepthBiasEnable(const pNewValue:boolean);
       function GetDepthBiasConstantFactor:TVkFloat;
       procedure SetDepthBiasConstantFactor(const pNewValue:TVkFloat);
       function GetDepthBiasClamp:TVkFloat;
       procedure SetDepthBiasClamp(const pNewValue:TVkFloat);
       function GetDepthBiasSlopeFactor:TVkFloat;
       procedure SetDepthBiasSlopeFactor(const pNewValue:TVkFloat);
       function GetLineWidth:TVkFloat;
       procedure SetLineWidth(const pNewValue:TVkFloat);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Assign(const pFrom:TVulkanPipelineRasterizationState);
       procedure SetRasterizationState(const pDepthClampEnable:boolean;
                                       const pRasterizerDiscardEnable:boolean;
                                       const pPolygonMode:TVkPolygonMode;
                                       const pCullMode:TVkCullModeFlags;
                                       const pFrontFace:TVkFrontFace;
                                       const pDepthBiasEnable:boolean;
                                       const pDepthBiasConstantFactor:TVkFloat;
                                       const pDepthBiasClamp:TVkFloat;
                                       const pDepthBiasSlopeFactor:TVkFloat;
                                       const pLineWidth:TVkFloat);
       property RasterizationStateCreateInfo:PVkPipelineRasterizationStateCreateInfo read fPointerToRasterizationStateCreateInfo;
      published
       property DepthClampEnable:boolean read GetDepthClampEnable write SetDepthClampEnable;
       property RasterizerDiscardEnable:boolean read GetRasterizerDiscardEnable write SetRasterizerDiscardEnable;
       property PolygonMode:TVkPolygonMode read GetPolygonMode write SetPolygonMode;
       property CullMode:TVkCullModeFlags read GetCullMode write SetCullMode;
       property FrontFace:TVkFrontFace read GetFrontFace write SetFrontFace;
       property DepthBiasEnable:boolean read GetDepthBiasEnable write SetDepthBiasEnable;
       property DepthBiasConstantFactor:TVkFloat read GetDepthBiasConstantFactor write SetDepthBiasConstantFactor;
       property DepthBiasClamp:TVkFloat read GetDepthBiasClamp write SetDepthBiasClamp;
       property DepthBiasSlopeFactor:TVkFloat read GetDepthBiasSlopeFactor write SetDepthBiasSlopeFactor;
       property LineWidth:TVkFloat read GetLineWidth write SetLineWidth;
     end;

     TVulkanPipelineMultisampleState=class(TVulkanPipelineState)
      private
       fMultisampleStateCreateInfo:TVkPipelineMultisampleStateCreateInfo;
       fPointerToMultisampleStateCreateInfo:PVkPipelineMultisampleStateCreateInfo;
       fSampleMasks:TVkSampleMaskArray;
       fCountSampleMasks:TVkInt32;
       function GetRasterizationSamples:TVkSampleCountFlagBits;
       procedure SetRasterizationSamples(const pNewValue:TVkSampleCountFlagBits);
       function GetSampleShadingEnable:boolean;
       procedure SetSampleShadingEnable(const pNewValue:boolean);
       function GetSampleMask(const pIndex:TVkInt32):TVkSampleMask;
       procedure SetSampleMask(const pIndex:TVkInt32;const pNewValue:TVkSampleMask);
       procedure SetCountSampleMasks(const pNewCount:TVkInt32);
       function GetMinSampleShading:TVkFloat;
       procedure SetMinSampleShading(const pNewValue:TVkFloat);
       function GetAlphaToCoverageEnable:boolean;
       procedure SetAlphaToCoverageEnable(const pNewValue:boolean);
       function GetAlphaToOneEnable:boolean;
       procedure SetAlphaToOneEnable(const pNewValue:boolean);
       procedure Initialize;
      public
       constructor Create;
       destructor Destroy; override;
       procedure Assign(const pFrom:TVulkanPipelineMultisampleState);
       function AddSampleMask(const pSampleMask:TVkSampleMask):TVkInt32;
       function AddSampleMasks(const pSampleMasks:array of TVkSampleMask):TVkInt32;
       procedure SetMultisampleState(const pRasterizationSamples:TVkSampleCountFlagBits;
                                     const pSampleShadingEnable:boolean;
                                     const pMinSampleShading:TVkFloat;
                                     const pSampleMask:array of TVkSampleMask;
                                     const pAlphaToCoverageEnable:boolean;
                                     const pAlphaToOneEnable:boolean);
       property MultisampleStateCreateInfo:PVkPipelineMultisampleStateCreateInfo read fPointerToMultisampleStateCreateInfo;
       property SampleMasks[const pIndex:TVkInt32]:TVkSampleMask read GetSampleMask write SetSampleMask;
      published                                                                            
       property RasterizationSamples:TVkSampleCountFlagBits read GetRasterizationSamples write SetRasterizationSamples;
       property SampleShadingEnable:boolean read GetSampleShadingEnable write SetSampleShadingEnable;
       property MinSampleShading:TVkFloat read GetMinSampleShading write SetMinSampleShading;
       property CountSampleMasks:TVkInt32 read fCountSampleMasks write SetCountSampleMasks;
       property AlphaToCoverageEnable:boolean read GetAlphaToCoverageEnable write SetAlphaToCoverageEnable;
       property AlphaToOneEnable:boolean read GetAlphaToOneEnable write SetAlphaToOneEnable;
     end;

     TVulkanStencilOpState=class(TVulkanObject)
      private
       fStencilOpState:PVkStencilOpState;
       function GetFailOp:TVkStencilOp;
       procedure SetFailOp(const pNewValue:TVkStencilOp);
       function GetPassOp:TVkStencilOp;
       procedure SetPassOp(const pNewValue:TVkStencilOp);
       function GetDepthFailOp:TVkStencilOp;
       procedure SetDepthFailOp(const pNewValue:TVkStencilOp);
       function GetCompareOp:TVkCompareOp;
       procedure SetCompareOp(const pNewValue:TVkCompareOp);
       function GetCompareMask:TVkUInt32;
       procedure SetCompareMask(const pNewValue:TVkUInt32);
       function GetWriteMask:TVkUInt32;
       procedure SetWriteMask(const pNewValue:TVkUInt32);
       function GetReference:TVkUInt32;
       procedure SetReference(const pNewValue:TVkUInt32);
      public
       constructor Create(const pStencilOpState:PVkStencilOpState);
       destructor Destroy; override;
       procedure Assign(const pFrom:TVulkanStencilOpState);
       property StencilOpState:PVkStencilOpState read fStencilOpState;
      published
       property FailOp:TVkStencilOp read GetFailOp write SetFailOp;
       property PassOp:TVkStencilOp read GetPassOp write SetPassOp;
       property DepthFailOp:TVkStencilOp read GetDepthFailOp write SetDepthFailOp;
       property CompareOp:TVkCompareOp read GetCompareOp write SetCompareOp;
       property CompareMask:TVkUInt32 read GetCompareMask write SetCompareMask;
       property WriteMask:TVkUInt32 read GetWriteMask write SetWriteMask;
       property Reference:TVkUInt32 read GetReference write SetReference;
     end;

     TVulkanPipelineDepthStencilState=class(TVulkanPipelineState)
      private
       fDepthStencilStateCreateInfo:TVkPipelineDepthStencilStateCreateInfo;
       fPointerToDepthStencilStateCreateInfo:PVkPipelineDepthStencilStateCreateInfo;
       fFrontStencilOpState:TVulkanStencilOpState;
       fBackStencilOpState:TVulkanStencilOpState;
       function GetDepthTestEnable:boolean;
       procedure SetDepthTestEnable(const pNewValue:boolean);
       function GetDepthWriteEnable:boolean;
       procedure SetDepthWriteEnable(const pNewValue:boolean);
       function GetDepthCompareOp:TVkCompareOp;
       procedure SetDepthCompareOp(const pNewValue:TVkCompareOp);
       function GetDepthBoundsTestEnable:boolean;
       procedure SetDepthBoundsTestEnable(const pNewValue:boolean);
       function GetStencilTestEnable:boolean;
       procedure SetStencilTestEnable(const pNewValue:boolean);
       function GetMinDepthBounds:TVkFloat;
       procedure SetMinDepthBounds(const pNewValue:TVkFloat);
       function GetMaxDepthBounds:TVkFloat;
       procedure SetMaxDepthBounds(const pNewValue:TVkFloat);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Assign(const pFrom:TVulkanPipelineDepthStencilState);
       procedure SetDepthStencilState(const pDepthTestEnable:boolean;
                                      const pDepthWriteEnable:boolean;
                                      const pDepthCompareOp:TVkCompareOp;
                                      const pDepthBoundsTestEnable:boolean;
                                      const pStencilTestEnable:boolean;
                                      const pFront:TVkStencilOpState;
                                      const pBack:TVkStencilOpState;
                                      const pMinDepthBounds:TVkFloat;
                                      const pMaxDepthBounds:TVkFloat);
       property DepthStencilStateCreateInfo:PVkPipelineDepthStencilStateCreateInfo read fPointerToDepthStencilStateCreateInfo;
      published
       property DepthTestEnable:boolean read GetDepthTestEnable write SetDepthTestEnable;
       property DepthWriteEnable:boolean read GetDepthWriteEnable write SetDepthWriteEnable;
       property DepthCompareOp:TVkCompareOp read GetDepthCompareOp write SetDepthCompareOp;
       property DepthBoundsTestEnable:boolean read GetDepthBoundsTestEnable write SetDepthBoundsTestEnable;
       property StencilTestEnable:boolean read GetStencilTestEnable write SetStencilTestEnable;
       property Front:TVulkanStencilOpState read fFrontStencilOpState;
       property Back:TVulkanStencilOpState read fBackStencilOpState;
       property MinDepthBounds:TVkFloat read GetMinDepthBounds write SetMinDepthBounds;
       property MaxDepthBounds:TVkFloat read GetMaxDepthBounds write SetMaxDepthBounds;
     end;

     TVulkanPipelineColorBlendState=class(TVulkanPipelineState)
      private
       fColorBlendStateCreateInfo:TVkPipelineColorBlendStateCreateInfo;
       fPointerToColorBlendStateCreateInfo:PVkPipelineColorBlendStateCreateInfo;
       fColorBlendAttachmentStates:TVkPipelineColorBlendAttachmentStateArray;
       fCountColorBlendAttachmentStates:TVkInt32;
       function GetLogicOpEnable:boolean;
       procedure SetLogicOpEnable(const pNewValue:boolean);
       function GetLogicOp:TVkLogicOp;
       procedure SetLogicOp(const pNewValue:TVkLogicOp);
       procedure SetCountColorBlendAttachmentStates(const pNewCount:TVkInt32);
       function GetColorBlendAttachmentState(const pIndex:TVkInt32):PVkPipelineColorBlendAttachmentState;
       function GetBlendConstant(const pIndex:TVkInt32):TVkFloat;
       procedure SetBlendConstant(const pIndex:TVkInt32;const pNewValue:TVkFloat);
      public
       constructor Create;
       destructor Destroy; override;
       procedure Assign(const pFrom:TVulkanPipelineColorBlendState);
       procedure SetColorBlendState(const pLogicOpEnable:boolean;
                                    const pLogicOp:TVkLogicOp;
                                    const pBlendConstants:array of TVkFloat);
       function AddColorBlendAttachmentState(const pColorBlendAttachmentState:TVkPipelineColorBlendAttachmentState):TVkInt32; overload;
       function AddColorBlendAttachmentState(const pBlendEnable:boolean;
                                             const pSrcColorBlendFactor:TVkBlendFactor;
                                             const pDstColorBlendFactor:TVkBlendFactor;
                                             const pColorBlendOp:TVkBlendOp;
                                             const pSrcAlphaBlendFactor:TVkBlendFactor;
                                             const pDstAlphaBlendFactor:TVkBlendFactor;
                                             const pAlphaBlendOp:TVkBlendOp;
                                             const pColorWriteMask:TVkColorComponentFlags):TVkInt32; overload;
       function AddColorBlendAttachmentStates(const pColorBlendAttachmentStates:array of TVkPipelineColorBlendAttachmentState):TVkInt32;
       procedure Initialize;
       property ColorBlendStateCreateInfo:PVkPipelineColorBlendStateCreateInfo read fPointerToColorBlendStateCreateInfo;
       property ColorBlendAttachmentStates[const pIndex:TVkInt32]:PVkPipelineColorBlendAttachmentState read GetColorBlendAttachmentState;
       property BlendConstants[const pIndex:TVkInt32]:TVkFloat read GetBlendConstant write SetBlendConstant;
      published
       property LogicOpEnable:boolean read GetLogicOpEnable write SetLogicOpEnable;
       property LogicOp:TVkLogicOp read GetLogicOp write SetLogicOp;
       property CountColorBlendAttachmentStates:TVkInt32 read fCountColorBlendAttachmentStates write SetCountColorBlendAttachmentStates;
     end;

     TVulkanPipelineDynamicState=class(TVulkanPipelineState)
      private
       fDynamicStateCreateInfo:TVkPipelineDynamicStateCreateInfo;
       fPointerToDynamicStateCreateInfo:PVkPipelineDynamicStateCreateInfo;
       fDynamicStates:TVkDynamicStateArray;
       fCountDynamicStates:TVkInt32;
       function GetDynamicState(const pIndex:TVkInt32):PVkDynamicState;
       procedure SetCountDynamicStates(const pNewCount:TVkInt32);
       procedure Initialize;
      public                
       constructor Create;
       destructor Destroy; override;
       procedure Assign(const pFrom:TVulkanPipelineDynamicState);
       function AddDynamicState(const pDynamicState:TVkDynamicState):TVkInt32; overload;
       function AddDynamicStates(const pDynamicStates:array of TVkDynamicState):TVkInt32; overload;
       property DynamicStateStateCreateInfo:PVkPipelineDynamicStateCreateInfo read fPointerToDynamicStateCreateInfo;
       property DynamicStates[const pIndex:TVkInt32]:PVkDynamicState read GetDynamicState;
      published
       property CountDynamicStates:TVkInt32 read fCountDynamicStates write SetCountDynamicStates;
     end;

     TVulkanGraphicsPipelineConstructor=class(TVulkanPipeline)
      private
       fGraphicsPipelineCreateInfo:TVkGraphicsPipelineCreateInfo;
       fStages:TVkPipelineShaderStageCreateInfoArray;
       fVertexInputState:TVulkanPipelineVertexInputState;
       fInputAssemblyState:TVulkanPipelineInputAssemblyState;
       fTessellationState:TVulkanPipelineTessellationState;
       fViewPortState:TVulkanPipelineViewPortState;
       fRasterizationState:TVulkanPipelineRasterizationState;
       fMultisampleState:TVulkanPipelineMultisampleState;
       fDepthStencilState:TVulkanPipelineDepthStencilState;
       fColorBlendState:TVulkanPipelineColorBlendState;
       fDynamicState:TVulkanPipelineDynamicState;
       fPipelineCache:TVkPipelineCache;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pCache:TVulkanPipelineCache;
                          const pFlags:TVkPipelineCreateFlags;
                          const pStages:array of TVulkanPipelineShaderStage;
                          const pLayout:TVulkanPipelineLayout;
                          const pRenderPass:TVulkanRenderPass;
                          const pSubPass:TVkUInt32;
                          const pBasePipelineHandle:TVulkanPipeline;
                          const pBasePipelineIndex:TVkInt32); reintroduce;
       destructor Destroy; override;
       procedure Assign(const pFrom:TVulkanGraphicsPipelineConstructor);
       function AddVertexInputBindingDescription(const pVertexInputBindingDescription:TVkVertexInputBindingDescription):TVkInt32; overload;
       function AddVertexInputBindingDescription(const pBinding,pStride:TVkUInt32;const pInputRate:TVkVertexInputRate):TVkInt32; overload;
       function AddVertexInputBindingDescriptions(const pVertexInputBindingDescriptions:array of TVkVertexInputBindingDescription):TVkInt32;
       function AddVertexInputAttributeDescription(const pVertexInputAttributeDescription:TVkVertexInputAttributeDescription):TVkInt32; overload;
       function AddVertexInputAttributeDescription(const pLocation,pBinding:TVkUInt32;const pFormat:TVkFormat;const pOffset:TVkUInt32):TVkInt32; overload;
       function AddVertexInputAttributeDescriptions(const pVertexInputAttributeDescriptions:array of TVkVertexInputAttributeDescription):TVkInt32;
       procedure SetInputAssemblyState(const pTopology:TVkPrimitiveTopology;const pPrimitiveRestartEnable:boolean);
       procedure SetTessellationState(const pPatchControlPoints:TVkUInt32);
       function AddViewPort(const pViewPort:TVkViewport):TVkInt32; overload;
       function AddViewPort(const pX,pY,pWidth,pHeight,pMinDepth,pMaxDepth:TVkFloat):TVkInt32; overload;
       function AddViewPorts(const pViewPorts:array of TVkViewport):TVkInt32; overload;
       function AddScissor(const pScissor:TVkRect2D):TVkInt32; overload;
       function AddScissor(const pX,pY:TVkInt32;const pWidth,pHeight:TVkUInt32):TVkInt32; overload;
       function AddScissors(const pScissors:array of TVkRect2D):TVkInt32; overload;
       procedure SetRasterizationState(const pDepthClampEnable:boolean;
                                       const pRasterizerDiscardEnable:boolean;
                                       const pPolygonMode:TVkPolygonMode;
                                       const pCullMode:TVkCullModeFlags;
                                       const pFrontFace:TVkFrontFace;
                                       const pDepthBiasEnable:boolean;
                                       const pDepthBiasConstantFactor:TVkFloat;
                                       const pDepthBiasClamp:TVkFloat;
                                       const pDepthBiasSlopeFactor:TVkFloat;
                                       const pLineWidth:TVkFloat);
       procedure SetMultisampleState(const pRasterizationSamples:TVkSampleCountFlagBits;
                                     const pSampleShadingEnable:boolean;
                                     const pMinSampleShading:TVkFloat;
                                     const pSampleMask:array of TVkSampleMask;
                                     const pAlphaToCoverageEnable:boolean;
                                     const pAlphaToOneEnable:boolean);
       procedure SetDepthStencilState(const pDepthTestEnable:boolean;
                                      const pDepthWriteEnable:boolean;
                                      const pDepthCompareOp:TVkCompareOp;
                                      const pDepthBoundsTestEnable:boolean;
                                      const pStencilTestEnable:boolean;
                                      const pFront:TVkStencilOpState;
                                      const pBack:TVkStencilOpState;
                                      const pMinDepthBounds:TVkFloat;
                                      const pMaxDepthBounds:TVkFloat);
       procedure SetColorBlendState(const pLogicOpEnable:boolean;
                                    const pLogicOp:TVkLogicOp;
                                    const pBlendConstants:array of TVkFloat);
       function AddColorBlendAttachmentState(const pColorBlendAttachmentState:TVkPipelineColorBlendAttachmentState):TVkInt32; overload;
       function AddColorBlendAttachmentState(const pBlendEnable:boolean;
                                             const pSrcColorBlendFactor:TVkBlendFactor;
                                             const pDstColorBlendFactor:TVkBlendFactor;
                                             const pColorBlendOp:TVkBlendOp;
                                             const pSrcAlphaBlendFactor:TVkBlendFactor;
                                             const pDstAlphaBlendFactor:TVkBlendFactor;
                                             const pAlphaBlendOp:TVkBlendOp;
                                             const pColorWriteMask:TVkColorComponentFlags):TVkInt32; overload;
       function AddColorBlendAttachmentStates(const pColorBlendAttachmentStates:array of TVkPipelineColorBlendAttachmentState):TVkInt32;
       function AddDynamicState(const pDynamicState:TVkDynamicState):TVkInt32;
       function AddDynamicStates(const pDynamicStates:array of TVkDynamicState):TVkInt32;
       procedure Initialize;
      published
       property VertexInputState:TVulkanPipelineVertexInputState read fVertexInputState;
       property InputAssemblyState:TVulkanPipelineInputAssemblyState read fInputAssemblyState;
       property TessellationState:TVulkanPipelineTessellationState read fTessellationState;
       property ViewPortState:TVulkanPipelineViewPortState read fViewPortState;
       property RasterizationState:TVulkanPipelineRasterizationState read fRasterizationState;
       property MultisampleState:TVulkanPipelineMultisampleState read fMultisampleState;
       property DepthStencilState:TVulkanPipelineDepthStencilState read fDepthStencilState;
       property ColorBlendState:TVulkanPipelineColorBlendState read fColorBlendState;
       property DynamicState:TVulkanPipelineDynamicState read fDynamicState;
     end;

     TVulkanGraphicsPipeline=class(TVulkanPipeline)
      private
       fGraphicsPipelineConstructor:TVulkanGraphicsPipelineConstructor;
       function GetVertexInputState:TVulkanPipelineVertexInputState;
       function GetInputAssemblyState:TVulkanPipelineInputAssemblyState;
       function GetTessellationState:TVulkanPipelineTessellationState;
       function GetViewPortState:TVulkanPipelineViewPortState;
       function GetRasterizationState:TVulkanPipelineRasterizationState;
       function GetMultisampleState:TVulkanPipelineMultisampleState;
       function GetDepthStencilState:TVulkanPipelineDepthStencilState;
       function GetColorBlendState:TVulkanPipelineColorBlendState;
       function GetDynamicState:TVulkanPipelineDynamicState;
      public
       constructor Create(const pDevice:TVulkanDevice;
                          const pCache:TVulkanPipelineCache;
                          const pFlags:TVkPipelineCreateFlags;
                          const pStages:array of TVulkanPipelineShaderStage;
                          const pLayout:TVulkanPipelineLayout;
                          const pRenderPass:TVulkanRenderPass;
                          const pSubPass:TVkUInt32;
                          const pBasePipelineHandle:TVulkanPipeline;
                          const pBasePipelineIndex:TVkInt32); reintroduce;
       destructor Destroy; override;
       procedure Assign(const pFrom:TVulkanGraphicsPipeline);
       function AddVertexInputBindingDescription(const pVertexInputBindingDescription:TVkVertexInputBindingDescription):TVkInt32; overload;
       function AddVertexInputBindingDescription(const pBinding,pStride:TVkUInt32;const pInputRate:TVkVertexInputRate):TVkInt32; overload;
       function AddVertexInputBindingDescriptions(const pVertexInputBindingDescriptions:array of TVkVertexInputBindingDescription):TVkInt32;
       function AddVertexInputAttributeDescription(const pVertexInputAttributeDescription:TVkVertexInputAttributeDescription):TVkInt32; overload;
       function AddVertexInputAttributeDescription(const pLocation,pBinding:TVkUInt32;const pFormat:TVkFormat;const pOffset:TVkUInt32):TVkInt32; overload;
       function AddVertexInputAttributeDescriptions(const pVertexInputAttributeDescriptions:array of TVkVertexInputAttributeDescription):TVkInt32;
       procedure SetInputAssemblyState(const pTopology:TVkPrimitiveTopology;const pPrimitiveRestartEnable:boolean);
       procedure SetTessellationState(const pPatchControlPoints:TVkUInt32);
       function AddViewPort(const pViewPort:TVkViewport):TVkInt32; overload;
       function AddViewPort(const pX,pY,pWidth,pHeight,pMinDepth,pMaxDepth:TVkFloat):TVkInt32; overload;
       function AddViewPorts(const pViewPorts:array of TVkViewport):TVkInt32; overload;
       function AddScissor(const pScissor:TVkRect2D):TVkInt32; overload;
       function AddScissor(const pX,pY:TVkInt32;const pWidth,pHeight:TVkUInt32):TVkInt32; overload;
       function AddScissors(const pScissors:array of TVkRect2D):TVkInt32; overload;
       procedure SetRasterizationState(const pDepthClampEnable:boolean;
                                       const pRasterizerDiscardEnable:boolean;
                                       const pPolygonMode:TVkPolygonMode;
                                       const pCullMode:TVkCullModeFlags;
                                       const pFrontFace:TVkFrontFace;
                                       const pDepthBiasEnable:boolean;
                                       const pDepthBiasConstantFactor:TVkFloat;
                                       const pDepthBiasClamp:TVkFloat;
                                       const pDepthBiasSlopeFactor:TVkFloat;
                                       const pLineWidth:TVkFloat);
       procedure SetMultisampleState(const pRasterizationSamples:TVkSampleCountFlagBits;
                                     const pSampleShadingEnable:boolean;
                                     const pMinSampleShading:TVkFloat;
                                     const pSampleMask:array of TVkSampleMask;
                                     const pAlphaToCoverageEnable:boolean;
                                     const pAlphaToOneEnable:boolean);
       procedure SetDepthStencilState(const pDepthTestEnable:boolean;
                                      const pDepthWriteEnable:boolean;
                                      const pDepthCompareOp:TVkCompareOp;
                                      const pDepthBoundsTestEnable:boolean;
                                      const pStencilTestEnable:boolean;
                                      const pFront:TVkStencilOpState;
                                      const pBack:TVkStencilOpState;
                                      const pMinDepthBounds:TVkFloat;
                                      const pMaxDepthBounds:TVkFloat);
       procedure SetColorBlendState(const pLogicOpEnable:boolean;
                                    const pLogicOp:TVkLogicOp;
                                    const pBlendConstants:array of TVkFloat);
       function AddColorBlendAttachmentState(const pColorBlendAttachmentState:TVkPipelineColorBlendAttachmentState):TVkInt32; overload;
       function AddColorBlendAttachmentState(const pBlendEnable:boolean;
                                             const pSrcColorBlendFactor:TVkBlendFactor;
                                             const pDstColorBlendFactor:TVkBlendFactor;
                                             const pColorBlendOp:TVkBlendOp;
                                             const pSrcAlphaBlendFactor:TVkBlendFactor;
                                             const pDstAlphaBlendFactor:TVkBlendFactor;
                                             const pAlphaBlendOp:TVkBlendOp;
                                             const pColorWriteMask:TVkColorComponentFlags):TVkInt32; overload;
       function AddColorBlendAttachmentStates(const pColorBlendAttachmentStates:array of TVkPipelineColorBlendAttachmentState):TVkInt32;
       function AddDynamicState(const pDynamicState:TVkDynamicState):TVkInt32;
       function AddDynamicStates(const pDynamicStates:array of TVkDynamicState):TVkInt32;
       procedure Initialize;
       procedure FreeMemory;
      published
       property VertexInputState:TVulkanPipelineVertexInputState read GetVertexInputState;
       property InputAssemblyState:TVulkanPipelineInputAssemblyState read GetInputAssemblyState;
       property TessellationState:TVulkanPipelineTessellationState read GetTessellationState;
       property ViewPortState:TVulkanPipelineViewPortState read GetViewPortState;
       property RasterizationState:TVulkanPipelineRasterizationState read GetRasterizationState;
       property MultisampleState:TVulkanPipelineMultisampleState read GetMultisampleState;
       property DepthStencilState:TVulkanPipelineDepthStencilState read GetDepthStencilState;
       property ColorBlendState:TVulkanPipelineColorBlendState read GetColorBlendState;
       property DynamicState:TVulkanPipelineDynamicState read GetDynamicState;
     end;

const VulkanImageViewTypeToImageTiling:array[TVkImageViewType] of TVkImageTiling=
       (
        VK_IMAGE_TILING_LINEAR,  // VK_IMAGE_VIEW_TYPE_1D
        VK_IMAGE_TILING_OPTIMAL, // VK_IMAGE_VIEW_TYPE_2D
        VK_IMAGE_TILING_OPTIMAL, // VK_IMAGE_VIEW_TYPE_3D
        VK_IMAGE_TILING_OPTIMAL, // VK_IMAGE_VIEW_TYPE_CUBE
        VK_IMAGE_TILING_LINEAR,  // VK_IMAGE_VIEW_TYPE_1D_ARRAY
        VK_IMAGE_TILING_OPTIMAL, // VK_IMAGE_VIEW_TYPE_2D_ARRAY
        VK_IMAGE_TILING_LINEAR   // VK_IMAGE_VIEW_TYPE_CUBE_ARRAY
       );

function VulkanRoundUpToPowerOfTwo(Value:TVkSize):TVkSize;

function VulkanErrorToString(const ErrorCode:TVkResult):TVulkanCharString;

function StringListToVulkanCharStringArray(const StringList:TStringList):TVulkanCharStringArray;

procedure VulkanSetImageLayout(const pImage:TVkImage;
                               const pAspectMask:TVkImageAspectFlags;
                               const pOldImageLayout:TVkImageLayout;
                               const pNewImageLayout:TVkImageLayout;
                               const pRange:PVkImageSubresourceRange;
                               const pCommandBuffer:TVulkanCommandBuffer;
                               const pQueue:TVulkanQueue=nil;
                               const pFence:TVulkanFence=nil;
                               const pBeginAndExecuteCommandBuffer:boolean=false;
                               const pSrcQueueFamilyIndex:TVkQueue=TVkQueue(VK_QUEUE_FAMILY_IGNORED);
                               const pDstQueueFamilyIndex:TVkQueue=TVkQueue(VK_QUEUE_FAMILY_IGNORED));

implementation

const BooleanToVkBool:array[boolean] of TVkBool32=(VK_FALSE,VK_TRUE);

      CELL_EMPTY=-1;
      CELL_DELETED=-2;
      ENT_EMPTY=-1;
      ENT_DELETED=-2;

function HashData(const Data:pointer;const DataLength:TVkUInt32):TVkUInt32;
const m=TVkUInt32($57559429);
      n=TVkUInt32($5052acdb);
var b:PVkUInt8;
    h,k,len:TVkUInt32;
    p:TVkUInt64;
begin
 Len:=DataLength;
 h:=len;
 k:=h+n+1;
 if len>0 then begin
  b:=Data;
  while len>7 do begin
   begin
    p:=TVkUInt32(pointer(b)^)*UInt64(n);
    h:=h xor TVkUInt32(p and $ffffffff);
    k:=k xor TVkUInt32(p shr 32);
    inc(b,4);
   end;
   begin
    p:=TVkUInt32(pointer(b)^)*UInt64(m);
    k:=k xor TVkUInt32(p and $ffffffff);
    h:=h xor TVkUInt32(p shr 32);
    inc(b,4);
   end;
   dec(len,8);
  end;
  if len>3 then begin
   p:=TVkUInt32(pointer(b)^)*UInt64(n);
   h:=h xor TVkUInt32(p and $ffffffff);
   k:=k xor TVkUInt32(p shr 32);
   inc(b,4);
   dec(len,4);
  end;
  if len>0 then begin
   if len>1 then begin
    p:=word(pointer(b)^);
    inc(b,2);
    dec(len,2);
   end else begin
    p:=0;
   end;
   if len>0 then begin
    p:=p or (byte(b^) shl 16);
   end;
   p:=p*TVkUInt64(m);
   k:=k xor TVkUInt32(p and $ffffffff);
   h:=h xor TVkUInt32(p shr 32);
  end;
 end;
 begin
  p:=(h xor (k+n))*TVkUInt64(n);
  h:=h xor TVkUInt32(p and $ffffffff);
  k:=k xor TVkUInt32(p shr 32);
 end;
 result:=k xor h;
end;

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

{$if defined(fpc)}
function CTZDWord(Value:TVkUInt32):TVkUInt8; inline;
begin
 if Value=0 then begin
  result:=32;
 end else begin
  result:=BSFDWord(Value);
 end;
end;
{$elseif defined(cpu386)}
{$ifndef fpc}
function CTZDWord(Value:TVkUInt32):TVkUInt8; assembler; register; {$ifdef fpc}nostackframe;{$endif}
asm
 bsf eax,eax
 jnz @Done
 mov eax,32
@Done:
end;
{$endif}
{$elseif defined(cpux86_64)}
{$ifndef fpc}
function CTZDWord(Value:TVkUInt32):TVkUInt8; assembler; register; {$ifdef fpc}nostackframe;{$endif}
asm
{$ifndef fpc}
 .NOFRAME
{$endif}
{$ifdef Windows}
 bsf eax,ecx
{$else}
 bsf eax,edi
{$endif}
 jnz @Done
 mov eax,32
@Done:
end;
{$endif}
{$elseif not defined(fpc)}
function CTZDWord(Value:TVkUInt32):TVkUInt8;
const CTZDebruijn32Multiplicator=TVkUInt32($077cb531);
      CTZDebruijn32Shift=27;
      CTZDebruijn32Mask=31;
      CTZDebruijn32Table:array[0..31] of TVkUInt8=(0,1,28,2,29,14,24,3,30,22,20,15,25,17,4,8,31,27,13,23,21,19,16,7,26,12,18,6,11,5,10,9);
begin
 if Value=0 then begin
  result:=32;
 end else begin
  result:=CTZDebruijn32Table[((TVkUInt32(Value and (-Value))*CTZDebruijn32Multiplicator) shr CTZDebruijn32Shift) and CTZDebruijn32Mask];
 end;
end;
{$ifend}

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
   result:='Unknown error code detected ('+TVulkanCharString(IntToStr(longint(ErrorCode)))+')';
  end;
 end;
end;

function StringListToVulkanCharStringArray(const StringList:TStringList):TVulkanCharStringArray;
var i:TVkInt32;
begin
 result:=nil;
 SetLength(result,StringList.Count);
 for i:=0 to StringList.Count-1 do begin
  result[i]:=TVulkanCharString(StringList.Strings[i]);
 end;
end;

procedure HandleResultCode(const ResultCode:TVkResult);
begin
 if ResultCode<>VK_SUCCESS then begin
  raise EVulkanResultException.Create(ResultCode);
 end;
end;

procedure VulkanSetImageLayout(const pImage:TVkImage;
                               const pAspectMask:TVkImageAspectFlags;
                               const pOldImageLayout:TVkImageLayout;
                               const pNewImageLayout:TVkImageLayout;
                               const pRange:PVkImageSubresourceRange;
                               const pCommandBuffer:TVulkanCommandBuffer;
                               const pQueue:TVulkanQueue=nil;
                               const pFence:TVulkanFence=nil;
                               const pBeginAndExecuteCommandBuffer:boolean=false;
                               const pSrcQueueFamilyIndex:TVkQueue=TVkQueue(VK_QUEUE_FAMILY_IGNORED);
                               const pDstQueueFamilyIndex:TVkQueue=TVkQueue(VK_QUEUE_FAMILY_IGNORED));
var ImageMemoryBarrier:TVkImageMemoryBarrier;
    SrcStages,DestStages:TVkPipelineStageFlags;
begin

 if pBeginAndExecuteCommandBuffer then begin
  pCommandBuffer.BeginRecording;
 end;

 FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
 ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 ImageMemoryBarrier.oldLayout:=pOldImageLayout;
 ImageMemoryBarrier.newLayout:=pNewImageLayout;
 ImageMemoryBarrier.srcQueueFamilyIndex:=pSrcQueueFamilyIndex;
 ImageMemoryBarrier.dstQueueFamilyIndex:=pDstQueueFamilyIndex;
 ImageMemoryBarrier.image:=pImage;

 if assigned(pRange) then begin
  ImageMemoryBarrier.subresourceRange:=pRange^;
 end else begin
  ImageMemoryBarrier.subresourceRange.aspectMask:=pAspectMask;
  ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
  ImageMemoryBarrier.subresourceRange.levelCount:=1;
  ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
  ImageMemoryBarrier.subresourceRange.layerCount:=1;
 end;

 case pOldImageLayout of
  VK_IMAGE_LAYOUT_UNDEFINED:begin
   ImageMemoryBarrier.srcAccessMask:=0; //TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
  end;
  VK_IMAGE_LAYOUT_GENERAL:begin
  end;
  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:begin
   ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
  end;
  VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL:begin
   ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT);
  end;
  VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL:begin
  end;
  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:begin
   ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT);
  end;
  VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL:begin
   ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT);
  end;
  VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL:begin
   ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
  end;
  VK_IMAGE_LAYOUT_PREINITIALIZED:begin
   ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_HOST_WRITE_BIT);
  end;
  VK_IMAGE_LAYOUT_PRESENT_SRC_KHR:begin
  end;
 end;

 case pNewImageLayout of
  VK_IMAGE_LAYOUT_UNDEFINED:begin
  end;
  VK_IMAGE_LAYOUT_GENERAL:begin
   if pOldImageLayout=VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL then begin
    ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
   end;
  end;
  VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL:begin
   if pOldImageLayout=VK_IMAGE_LAYOUT_PRESENT_SRC_KHR then begin
    ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT);
   end;
   ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
  end;
  VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL:begin
   ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT);
  end;
  VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL:begin
  end;
  VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL:begin
   if pOldImageLayout=VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL then begin
    ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
   end;
   ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_SHADER_READ_BIT) or TVkAccessFlags(VK_ACCESS_INPUT_ATTACHMENT_READ_BIT);
  end;
  VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL:begin
   ImageMemoryBarrier.srcAccessMask:=ImageMemoryBarrier.srcAccessMask or TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT);
   ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_READ_BIT);
  end;
  VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL:begin
   ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_TRANSFER_WRITE_BIT);
  end;
  VK_IMAGE_LAYOUT_PREINITIALIZED:begin
  end;
  VK_IMAGE_LAYOUT_PRESENT_SRC_KHR:begin
   if pOldImageLayout=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL then begin
    ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
   end;
   ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT);
  end;
 end;

 if pOldImageLayout=VK_IMAGE_LAYOUT_PRESENT_SRC_KHR then begin
  SrcStages:=TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT);
  DestStages:=TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT);
 end else if pNewImageLayout=VK_IMAGE_LAYOUT_PRESENT_SRC_KHR then begin
  SrcStages:=TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT);
  DestStages:=TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT);
 end else begin
  SrcStages:=TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT);
  DestStages:=TVkPipelineStageFlags(VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT);
 end;

 pCommandBuffer.CmdPipelineBarrier(SrcStages,DestStages,0,0,nil,0,nil,1,@ImageMemoryBarrier);

 if pBeginAndExecuteCommandBuffer then begin
  pCommandBuffer.EndRecording;
  pCommandBuffer.Execute(pQueue,pFence,TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT));
 end;

end;

constructor EVulkanResultException.Create(const pResultCode:TVkResult);
begin
 fResultCode:=pResultCode;
 inherited Create(String(VulkanErrorToString(fResultCode)));
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
   FillChar(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(fAllocated)*TVkPtrUInt(fItemSize))))^,(NewAllocated-fAllocated)*fItemSize,#0);
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
  result:=pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))));
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
 CopyItem(Item,pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(result)*TVkPtrUInt(fItemSize))))^);
end;

function TVulkanBaseList.Find(const Item):TVkSizeInt;
var Index:TVkSizeInt;
begin
 result:=-1;
 Index:=0;
 while Index<fCount do begin
  if CompareItem(Item,pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^)=0 then begin
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
   Move(pointer(TVkPtrInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^,pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index+1)*TVkPtrUInt(fItemSize))))^,(fCount-Index)*fItemSize);
   FillChar(pointer(TVkPtrInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^,fItemSize,#0);
  end else begin
   SetCount(Index+1);
  end;
  CopyItem(Item,pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^);
 end;
end;

procedure TVulkanBaseList.Delete(const Index:TVkSizeInt);
begin
 if (Index>=0) and (Index<fCount) then begin
  FinalizeItem(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^);
  Move(pointer(TVkPtrUInt(TVkPtruInt(fMemory)+(TVkPtrUInt(Index+1)*TVkPtrUInt(fItemSize))))^,pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^,(fCount-Index)*fItemSize);
  FillChar(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(fCount-1)*TVkPtrUInt(fItemSize))))^,fItemSize,#0);
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
  ExchangeItem(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^,pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(WithIndex)*TVkPtrUInt(fItemSize))))^);
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
  result:=TVulkanObject(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^);
 end else begin
  result:=nil;
 end;
end;

procedure TVulkanObjectList.SetItem(const Index:TVkSizeInt;const Item:TVulkanObject);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVulkanObject(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^):=Item;
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
  result:=TVkUInt32(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^);
 end else begin
  result:=0;
 end;
end;

procedure TVkUInt32List.SetItem(const Index:TVkSizeInt;const Item:TVkUInt32);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkUInt32(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^):=Item;
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
  result:=TVkFloat(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^);
 end else begin
  result:=0;
 end;
end;

procedure TVkFloatList.SetItem(const Index:TVkSizeInt;const Item:TVkFloat);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkFloat(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^):=Item;
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
  result:=TVkImageView(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^);
 end else begin
  result:=0;
 end;
end;

procedure TVkImageViewList.SetItem(const Index:TVkSizeInt;const Item:TVkImageView);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkImageView(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^):=Item;
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
  result:=TVkSampler(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^);
 end else begin
  result:=0;
 end;
end;

procedure TVkSamplerList.SetItem(const Index:TVkSizeInt;const Item:TVkSampler);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkSampler(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^):=Item;
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
  result:=TVkDescriptorSetLayout(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^);
 end else begin
  result:=0;
 end;
end;

procedure TVkDescriptorSetLayoutList.SetItem(const Index:TVkSizeInt;const Item:TVkDescriptorSetLayout);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkDescriptorSetLayout(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^):=Item;
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
  result:=TVkSampleMask(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^);
 end else begin
  result:=0;
 end;
end;

procedure TVkSampleMaskList.SetItem(const Index:TVkSizeInt;const Item:TVkSampleMask);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkSampleMask(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^):=Item;
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
  result:=TVkDynamicState(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^);
 end else begin
  result:=TVkDynamicState(0);
 end;
end;

procedure TVkDynamicStateList.SetItem(const Index:TVkSizeInt;const Item:TVkDynamicState);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkDynamicState(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^):=Item;
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
  result:=TVkBufferView(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^);
 end else begin
  result:=TVkBufferView(0);
 end;
end;

procedure TVkBufferViewList.SetItem(const Index:TVkSizeInt;const Item:TVkBufferView);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkBufferView(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^):=Item;
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
  result:=TVkClearValue(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^);
 end else begin
  FillChar(result,SizeOf(TVkClearValue),#0);
 end;
end;

procedure TVkClearValueList.SetItem(const Index:TVkSizeInt;const Item:TVkClearValue);
begin
 if (Index>=0) and (Index<fCount) then begin
  TVkClearValue(pointer(TVkPtrUInt(TVkPtrUInt(fMemory)+(TVkPtrUInt(Index)*TVkPtrUInt(fItemSize))))^):=Item;
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
    fAvailableLayerNames.Add(String(LayerProperty^.LayerName));
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
     if fAvailableExtensionNames.IndexOf(String(ExtensionProperty^.ExtensionName))<0 then begin
      fAvailableExtensionNames.Add(String(ExtensionProperty^.ExtensionName));
     end;
    end;
    inc(Count,SubCount);
   end;
  end;
 finally
  SetLength(ExtensionProperties,0);
 end;

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
  fDebugReportCallbackEXT:=VK_NULL_HANDLE;
 end;
 fPhysicalDevices.Free;
 if fInstanceHandle<>VK_NULL_INSTANCE then begin
  fVulkan.DestroyInstance(fInstanceHandle,fAllocationCallbacks);
  fInstanceHandle:=VK_NULL_INSTANCE;
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
    InstanceCreateInfo:TVkInstanceCreateInfo;
begin

 if fInstanceHandle=VK_NULL_INSTANCE then begin

  SetLength(fEnabledLayerNameStrings,fEnabledLayerNames.Count);
  SetLength(fRawEnabledLayerNameStrings,fEnabledLayerNames.Count);
  for i:=0 to fEnabledLayerNames.Count-1 do begin
   fEnabledLayerNameStrings[i]:=TVulkanCharString(fEnabledLayerNames.Strings[i]);
   fRawEnabledLayerNameStrings[i]:=PVkChar(fEnabledLayerNameStrings[i]);
  end;

  SetLength(fEnabledExtensionNameStrings,fEnabledExtensionNames.Count);
  SetLength(fRawEnabledExtensionNameStrings,fEnabledExtensionNames.Count);
  for i:=0 to fEnabledExtensionNames.Count-1 do begin
   fEnabledExtensionNameStrings[i]:=TVulkanCharString(fEnabledExtensionNames.Strings[i]);
   fRawEnabledExtensionNameStrings[i]:=PVkChar(fEnabledExtensionNameStrings[i]);
  end;

  FillChar(InstanceCreateInfo,SizeOf(TVkInstanceCreateInfo),#0);
  InstanceCreateInfo.sType:=VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
  if length(fEnabledLayerNameStrings)>0 then begin
   InstanceCreateInfo.enabledLayerCount:=length(fEnabledLayerNameStrings);
   InstanceCreateInfo.ppEnabledLayerNames:=@fRawEnabledLayerNameStrings[0];
  end;
  if length(fEnabledExtensionNameStrings)>0 then begin
   InstanceCreateInfo.enabledExtensionCount:=length(fEnabledExtensionNameStrings);
   InstanceCreateInfo.ppEnabledExtensionNames:=@fRawEnabledExtensionNameStrings[0];
  end;

  HandleResultCode(fVulkan.CreateInstance(@InstanceCreateInfo,fAllocationCallbacks,@fInstanceHandle));

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

  EnumeratePhysicalDevices;

 end;
end;

procedure TVulkanInstance.EnumeratePhysicalDevices;
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
    fAvailableLayerNames.Add(String(LayerProperty^.LayerName));
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
     if fAvailableExtensionNames.IndexOf(String(ExtensionProperty^.ExtensionName))<0 then begin
      fAvailableExtensionNames.Add(String(ExtensionProperty^.ExtensionName));
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

function TVulkanPhysicalDevice.HasQueueSupportForSparseBindings(const pQueueFamilyIndex:TVkUInt32):boolean;
var QueueFamilyProperties:PVkQueueFamilyProperties;
begin
 result:=false;
 if pQueueFamilyIndex<TVkUInt32(length(fQueueFamilyProperties)) then begin
  QueueFamilyProperties:=@fQueueFamilyProperties[pQueueFamilyIndex];
  if (QueueFamilyProperties.queueFlags and TVKUInt32(VK_QUEUE_SPARSE_BINDING_BIT))<>0 then begin
   result:=true;
  end;
 end;
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

function TVulkanPhysicalDevice.GetSurfaceSupport(const pQueueFamilyIndex:TVkUInt32;const pSurface:TVulkanSurface):boolean;
var Supported:TVkBool32;
begin
 Supported:=0;
 fInstance.Commands.GetPhysicalDeviceSurfaceSupportKHR(fPhysicalDeviceHandle,pQueueFamilyIndex,pSurface.fSurfaceHandle,@Supported);
 result:=Supported<>0;
end;

function TVulkanPhysicalDevice.GetSurfaceCapabilities(const pSurface:TVulkanSurface):TVkSurfaceCapabilitiesKHR;
begin
 fInstance.Commands.GetPhysicalDeviceSurfaceCapabilitiesKHR(fPhysicalDeviceHandle,pSurface.fSurfaceHandle,@result);
end;

function TVulkanPhysicalDevice.GetSurfaceFormats(const pSurface:TVulkanSurface):TVkSurfaceFormatKHRArray;
var Count:TVKUInt32;
begin
 result:=nil;
 Count:=0;
 if fInstance.Commands.GetPhysicalDeviceSurfaceFormatsKHR(fPhysicalDeviceHandle,pSurface.fSurfaceHandle,@Count,nil)=VK_SUCCESS then begin
  if Count>0 then begin
   try
    SetLength(result,Count);
    HandleResultCode(fInstance.Commands.GetPhysicalDeviceSurfaceFormatsKHR(fPhysicalDeviceHandle,pSurface.fSurfaceHandle,@Count,@result[0]));
   except
    SetLength(result,0);
    raise;
   end;
  end;
 end;
end;

function TVulkanPhysicalDevice.GetSurfacePresentModes(const pSurface:TVulkanSurface):TVkPresentModeKHRArray;
var Count:TVKUInt32;
begin
 result:=nil;
 Count:=0;
 if fInstance.Commands.GetPhysicalDeviceSurfacePresentModesKHR(fPhysicalDeviceHandle,pSurface.fSurfaceHandle,@Count,nil)=VK_SUCCESS then begin
  if Count>0 then begin
   try
    SetLength(result,Count);
    HandleResultCode(fInstance.Commands.GetPhysicalDeviceSurfacePresentModesKHR(fPhysicalDeviceHandle,pSurface.fSurfaceHandle,@Count,@result[0]));
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
 result:=TVkUInt32(TVkInt32(-1));
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

function TVulkanPhysicalDevice.GetBestSupportedDepthFormat(const pWithStencil:boolean):TVkFormat;
const Formats:array[0..4] of TVkFormat=(VK_FORMAT_D32_SFLOAT_S8_UINT,
                                        VK_FORMAT_D32_SFLOAT,
                                        VK_FORMAT_D24_UNORM_S8_UINT,
                                        VK_FORMAT_D16_UNORM_S8_UINT,
                                        VK_FORMAT_D16_UNORM);
      WithStencilFormats:array[0..2] of TVkFormat=(VK_FORMAT_D32_SFLOAT_S8_UINT,
                                                   VK_FORMAT_D24_UNORM_S8_UINT,
                                                   VK_FORMAT_D16_UNORM_S8_UINT);
var i:TVkInt32;
    Format:TVkFormat;
    FormatProperties:TVkFormatProperties;
begin
 result:=VK_FORMAT_UNDEFINED;
 if pWithStencil then begin
  for i:=low(WithStencilFormats) to high(WithStencilFormats) do begin
   Format:=WithStencilFormats[i];
   fInstance.fVulkan.GetPhysicalDeviceFormatProperties(fPhysicalDeviceHandle,Format,@FormatProperties);
   if (FormatProperties.OptimalTilingFeatures and TVkFormatFeatureFlags(VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT))<>0 then begin
    result:=Format;
    exit;
   end;
  end;
 end else begin
  for i:=low(Formats) to high(Formats) do begin
   Format:=Formats[i];
   fInstance.fVulkan.GetPhysicalDeviceFormatProperties(fPhysicalDeviceHandle,Format,@FormatProperties);
   if (FormatProperties.OptimalTilingFeatures and TVkFormatFeatureFlags(VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT))<>0 then begin
    result:=Format;
    exit;
   end;
  end;
 end;
end;

function TVulkanPhysicalDevice.GetQueueNodeIndex(const pSurface:TVulkanSurface;const pQueueFlagBits:TVkQueueFlagBits):TVkInt32;
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
   fInstance.fVulkan.GetPhysicalDeviceSurfaceSupportKHR(fPhysicalDeviceHandle,Index,pSurface.fSurfaceHandle,@SupportsPresent);
   if ((QueueProperties[Index].QueueFlags and TVkQueueFlags(pQueueFlagBits))<>0) and (SupportsPresent=VK_TRUE) then begin
    result:=Index;
    break;
   end;
  end;
 finally
  SetLength(QueueProperties,0);
 end;
end;

function TVulkanPhysicalDevice.GetSurfaceFormat(const pSurface:TVulkanSurface):TVkSurfaceFormatKHR;
var FormatCount:TVkUInt32;
    SurfaceFormats:TVkSurfaceFormatKHRArray;
begin
 SurfaceFormats:=nil;
 try

  FormatCount:=0;
  HandleResultCode(vkGetPhysicalDeviceSurfaceFormatsKHR(fPhysicalDeviceHandle,pSurface.fSurfaceHandle,@FormatCount,nil));

  if FormatCount>0 then begin
   SetLength(SurfaceFormats,FormatCount);
   HandleResultCode(vkGetPhysicalDeviceSurfaceFormatsKHR(fPhysicalDeviceHandle,pSurface.fSurfaceHandle,@FormatCount,@SurfaceFormats[0]));
  end;

  if (FormatCount=0) or ((FormatCount=1) and (SurfaceFormats[0].Format=VK_FORMAT_UNDEFINED)) then begin
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
{$if defined(Android)}
                                  const pWindow:PANativeWindow
{$elseif defined(Mir)}
                                  const pConnection:PMirConnection;const pMirSurface:PMirSurface
{$elseif defined(Wayland)}
                                  const pDisplay:Pwl_display;const pSurface:Pwl_surface
{$elseif defined(Windows)}
                                  const pInstanceHandle,pWindowHandle:THandle
{$elseif defined(X11)}
                                  const pDisplay:PDisplay;const pWindow:TWindow
{$elseif defined(XCB)}
                                  const pConnection:Pxcb_connection;pWindow:Pxcb_window
{$ifend}
                                 );
begin
 inherited Create;

 fInstance:=pInstance;

 fSurfaceHandle:=VK_NULL_HANDLE;

 FillChar(fSurfaceCreateInfo,SizeOf(TVulkanSurfaceCreateInfo),#0);
{$if defined(Android)}
 fSurfaceCreateInfo.sType:=VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR;
 fSurfaceCreateInfo.window:=pWindow;
{$elseif defined(Mir)}
 fSurfaceCreateInfo.sType:=VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR;
 fSurfaceCreateInfo.connection:=pConnection;
 fSurfaceCreateInfo.mirSurface:=pMirSurface;
{$elseif defined(Wayland)}
 fSurfaceCreateInfo.sType:=VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR;
 fSurfaceCreateInfo.display:=pDisplay;
 fSurfaceCreateInfo.surface:=pSurface;
{$elseif defined(Windows)}
 fSurfaceCreateInfo.sType:=VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
 fSurfaceCreateInfo.hinstance_:=pInstanceHandle;
 fSurfaceCreateInfo.hwnd_:=pWindowHandle;
{$elseif defined(X11)}
 fSurfaceCreateInfo.sType:=VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
 fSurfaceCreateInfo.dpy:=pDisplay;
 fSurfaceCreateInfo.window:=pWindow;
{$elseif defined(XCB)}
 fSurfaceCreateInfo.connection:=pConnection;
 fSurfaceCreateInfo.window:=pWindow;
{$ifend}

{$if defined(Android)}
 HandleResultCode(fInstance.fVulkan.CreateAndroidSurfaceKHR(fInstance.fInstanceHandle,@fSurfaceCreateInfo,fInstance.fAllocationCallbacks,@fSurfaceHandle));
{$elseif defined(Mir)}
 HandleResultCode(fInstance.fVulkan.CreateMirSurfaceKHR(fInstance.fInstanceHandle,@fSurfaceCreateInfo,fInstance.fAllocationCallbacks,@fSurfaceHandle));
{$elseif defined(Wayland)}
 HandleResultCode(fInstance.fVulkan.CreateWaylandSurfaceKHR(fInstance.fInstanceHandle,@fSurfaceCreateInfo,fInstance.fAllocationCallbacks,@fSurfaceHandle));
{$elseif defined(Windows)}
 HandleResultCode(fInstance.fVulkan.CreateWin32SurfaceKHR(fInstance.fInstanceHandle,@fSurfaceCreateInfo,fInstance.fAllocationCallbacks,@fSurfaceHandle));
{$elseif defined(X11)}
 HandleResultCode(fInstance.fVulkan.CreateX11SurfaceKHR(fInstance.fInstanceHandle,@fSurfaceCreateInfo,fInstance.fAllocationCallbacks,@fSurfaceHandle));
{$elseif defined(XCB)}
 HandleResultCode(fInstance.fVulkan.CreateXCBSurfaceKHR(fInstance.fInstanceHandle,@fSurfaceCreateInfo,fInstance.fAllocationCallbacks,@fSurfaceHandle));
{$else}
 HandleResultCode(VK_ERROR_INCOMPATIBLE_DRIVER);
{$ifend}

end;

destructor TVulkanSurface.Destroy;
begin
 if fSurfaceHandle<>VK_NULL_HANDLE then begin
  fInstance.fVulkan.DestroySurfaceKHR(fInstance.fInstanceHandle,fSurfaceHandle,fInstance.fAllocationCallbacks);
  fSurfaceHandle:=VK_NULL_HANDLE;
 end;
 inherited Destroy;
end;

constructor TVulkanDevice.Create(const pInstance:TVulkanInstance;
                                 const pPhysicalDevice:TVulkanPhysicalDevice=nil;
                                 const pSurface:TVulkanSurface=nil;
                                 const pAllocationManager:TVulkanAllocationManager=nil);
var Index,SubIndex:TVkInt32;
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

 fQueues:=nil;

 fPresentQueueFamilyIndex:=-1;
 fGraphicsQueueFamilyIndex:=-1;
 fComputeQueueFamilyIndex:=-1;
 fTransferQueueFamilyIndex:=-1;

 fPresentQueue:=nil;
 fGraphicsQueue:=nil;
 fComputeQueue:=nil;
 fTransferQueue:=nil;

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
    if assigned(pSurface) and not CurrentPhysicalDevice.GetSurfaceSupport(SubIndex,pSurface) then begin
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

 fEnabledFeatures:=fPhysicalDevice.fFeatures;

 fPointerToEnabledFeatures:=@fEnabledFeatures;

 fMemoryManager:=TVulkanDeviceMemoryManager.Create(self);

end;

destructor TVulkanDevice.Destroy;
var Index:TVkInt32;
begin
 for Index:=0 to length(fQueues)-1 do begin
  if assigned(fQueues[Index]) then begin
   fQueues[Index].Free;
   fQueues[Index]:=nil;
  end;
 end;
 SetLength(fQueues,0);
 fMemoryManager.Free;
 fDeviceVulkan.Free;
 if fDeviceHandle<>VK_NULL_HANDLE then begin
  fInstance.Commands.DestroyDevice(fDeviceHandle,fAllocationCallbacks);
  fDeviceHandle:=VK_NULL_HANDLE;
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
  QueueFamilyProperties:=@fPhysicalDevice.fQueueFamilyProperties[pQueueFamilyIndex];
  if (fPresentQueueFamilyIndex<0) and assigned(fSurface) and fPhysicalDevice.GetSurfaceSupport(pQueueFamilyIndex,fSurface) then begin
   fPresentQueueFamilyIndex:=pQueueFamilyIndex;
  end;
  if ((QueueFamilyProperties.queueFlags and TVKUInt32(VK_QUEUE_GRAPHICS_BIT))<>0) and (fGraphicsQueueFamilyIndex<0) then begin
   fGraphicsQueueFamilyIndex:=pQueueFamilyIndex;
  end;
  if ((QueueFamilyProperties.queueFlags and TVKUInt32(VK_QUEUE_COMPUTE_BIT))<>0) and (fComputeQueueFamilyIndex<0) then begin
   fComputeQueueFamilyIndex:=pQueueFamilyIndex;
  end;
  if ((QueueFamilyProperties.queueFlags and TVKUInt32(VK_QUEUE_TRANSFER_BIT))<>0) and (fTransferQueueFamilyIndex<0) then begin
   fTransferQueueFamilyIndex:=pQueueFamilyIndex;
  end;
  fDeviceQueueCreateInfoList.Add(TVulkanDeviceQueueCreateInfo.Create(pQueueFamilyIndex,pQueuePriorities));
 end else begin
  raise EVulkanException.Create('Queue family index out of bounds');
 end;
end;

procedure TVulkanDevice.AddQueues(const pPresent:boolean=true;
                                  const pGraphics:boolean=true;
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
  if (fPresentQueueFamilyIndex<0) and assigned(fSurface) and fPhysicalDevice.GetSurfaceSupport(Index,fSurface) then begin
   fPresentQueueFamilyIndex:=Index;
   if pPresent then begin
    DoAdd:=true;
   end;
  end;
  if ((QueueFamilyProperties.queueFlags and TVKUInt32(VK_QUEUE_GRAPHICS_BIT))<>0) and (fGraphicsQueueFamilyIndex<0) then begin
   fGraphicsQueueFamilyIndex:=Index;
   if pGraphics then begin
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
  if ((QueueFamilyProperties.queueFlags and TVKUInt32(VK_QUEUE_SPARSE_BINDING_BIT))=0) and pSparseBinding then begin
   raise EVulkanException.Create('Only unsatisfactory device queue families available');
  end;
  if DoAdd then begin
   fDeviceQueueCreateInfoList.Add(TVulkanDeviceQueueCreateInfo.Create(Index,[1.0]));
  end;
 end;
 if ((fPresentQueueFamilyIndex<0) and pPresent) or
    ((fGraphicsQueueFamilyIndex<0) and pGraphics) or
    ((fComputeQueueFamilyIndex<0) and pCompute) or
    ((fTransferQueueFamilyIndex<0) and pTransfer) then begin
  raise EVulkanException.Create('Only unsatisfactory device queue families available');
 end;
end;

procedure TVulkanDevice.Initialize;
var Index:TVkInt32;
    DeviceQueueCreateInfo:PVkDeviceQueueCreateInfo;
    SrcDeviceQueueCreateInfo:TVulkanDeviceQueueCreateInfo;
    DeviceCommands:PVulkanCommands;
    Queue:TVkQueue;
    DeviceCreateInfo:TVkDeviceCreateInfo;
begin
 if fDeviceHandle=VK_NULL_HANDLE then begin

  SetLength(fEnabledLayerNameStrings,fEnabledLayerNames.Count);
  SetLength(fRawEnabledLayerNameStrings,fEnabledLayerNames.Count);
  for Index:=0 to fEnabledLayerNames.Count-1 do begin
   fEnabledLayerNameStrings[Index]:=TVulkanCharString(fEnabledLayerNames.Strings[Index]);
   fRawEnabledLayerNameStrings[Index]:=PVkChar(fEnabledLayerNameStrings[Index]);
  end;

  SetLength(fEnabledExtensionNameStrings,fEnabledExtensionNames.Count);
  SetLength(fRawEnabledExtensionNameStrings,fEnabledExtensionNames.Count);
  for Index:=0 to fEnabledExtensionNames.Count-1 do begin
   fEnabledExtensionNameStrings[Index]:=TVulkanCharString(fEnabledExtensionNames.Strings[Index]);
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

  FillChar(DeviceCreateInfo,SizeOf(TVkDeviceCreateInfo),#0);
  DeviceCreateInfo.sType:=VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
  if length(fDeviceQueueCreateInfos)>0 then begin
   DeviceCreateInfo.queueCreateInfoCount:=length(fDeviceQueueCreateInfos);
   DeviceCreateInfo.pQueueCreateInfos:=@fDeviceQueueCreateInfos[0];
  end;
  if length(fEnabledLayerNameStrings)>0 then begin
   DeviceCreateInfo.enabledLayerCount:=length(fEnabledLayerNameStrings);
   DeviceCreateInfo.ppEnabledLayerNames:=@fRawEnabledLayerNameStrings[0];
  end;
  if length(fEnabledExtensionNameStrings)>0 then begin
   DeviceCreateInfo.enabledExtensionCount:=length(fEnabledExtensionNameStrings);
   DeviceCreateInfo.ppEnabledExtensionNames:=@fRawEnabledExtensionNameStrings[0];
  end;
  DeviceCreateInfo.pEnabledFeatures:=@fEnabledFeatures;
  HandleResultCode(fInstance.Commands.CreateDevice(fPhysicalDevice.fPhysicalDeviceHandle,@DeviceCreateInfo,fAllocationCallbacks,@fDeviceHandle));

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

  SetLength(fQueues,length(fPhysicalDevice.fQueueFamilyProperties));
  for Index:=0 to length(fPhysicalDevice.fQueueFamilyProperties)-1 do begin
   if (Index=fPresentQueueFamilyIndex) or
      (Index=fGraphicsQueueFamilyIndex) or
      (Index=fComputeQueueFamilyIndex) or
      (Index=fTransferQueueFamilyIndex) then begin
    fDeviceVulkan.GetDeviceQueue(fDeviceHandle,Index,0,@Queue);
    fQueues[Index]:=TVulkanQueue.Create(self,Queue,Index);
   end else begin
    fQueues[Index]:=nil;
   end;
  end;

  if fPresentQueueFamilyIndex>=0 then begin
   fPresentQueue:=fQueues[fPresentQueueFamilyIndex];
  end else begin
   fPresentQueue:=nil;
  end;
  if fGraphicsQueueFamilyIndex>=0 then begin
   fGraphicsQueue:=fQueues[fGraphicsQueueFamilyIndex];
  end else begin
   fGraphicsQueue:=nil;
  end;
  if fComputeQueueFamilyIndex>=0 then begin
   fComputeQueue:=fQueues[fComputeQueueFamilyIndex];
  end else begin
   fComputeQueue:=nil;
  end;
  if fTransferQueueFamilyIndex>=0 then begin
   fTransferQueue:=fQueues[fTransferQueueFamilyIndex];
  end else begin
   fTransferQueue:=nil;
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
                                            const pAlignment:TVkDeviceSize;
                                            const pMemoryTypeBits:TVkUInt32;
                                            const pMemoryPropertyFlags:TVkMemoryPropertyFlags;
                                            const pMemoryChunkList:PVulkanDeviceMemoryManagerChunkList;
                                            const pMemoryHeapFlags:TVkMemoryHeapFlags=0);
var Index,HeapIndex:TVkInt32;
    MemoryAllocateInfo:TVkMemoryAllocateInfo;
    PhysicalDevice:TVulkanPhysicalDevice;
    CurrentSize,BestSize:TVkDeviceSize;
    Found:boolean;
begin
 inherited Create;

 fMemoryManager:=pMemoryManager;

 fSize:=pSize;

 fAlignment:=pAlignment;

 fMemoryChunkList:=pMemoryChunkList;

 fUsed:=0;

 fMappedOffset:=0;

 fMappedSize:=fSize;

 fMemoryPropertyFlags:=pMemoryPropertyFlags;

 fMemoryHandle:=VK_NULL_HANDLE;

 fMemory:=nil;

 fMemoryTypeIndex:=0;
 fMemoryTypeBits:=0;
 fMemoryHeapIndex:=0;
 PhysicalDevice:=fMemoryManager.fDevice.fPhysicalDevice;
 BestSize:=0;
 Found:=false;
 for Index:=0 to length(PhysicalDevice.fMemoryProperties.memoryTypes)-1 do begin
  if ((pMemoryTypeBits and (TVkUInt32(1) shl Index))<>0) and
     ((PhysicalDevice.fMemoryProperties.memoryTypes[Index].propertyFlags and pMemoryPropertyFlags)=pMemoryPropertyFlags) then begin
   HeapIndex:=PhysicalDevice.fMemoryProperties.memoryTypes[Index].heapIndex;
   CurrentSize:=PhysicalDevice.fMemoryProperties.memoryHeaps[HeapIndex].size;
   if ((PhysicalDevice.fMemoryProperties.memoryHeaps[HeapIndex].flags and pMemoryHeapFlags)=pMemoryHeapFlags) and
      (pSize<=CurrentSize) and (CurrentSize>BestSize) then begin
    BestSize:=CurrentSize;
    fMemoryTypeIndex:=Index;
    fMemoryTypeBits:=TVkUInt32(1) shl Index;
    fMemoryHeapIndex:=PhysicalDevice.fMemoryProperties.memoryTypes[Index].heapIndex;
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

 HandleResultCode(fMemoryManager.fDevice.Commands.AllocateMemory(fMemoryManager.fDevice.fDeviceHandle,@MemoryAllocateInfo,fMemoryManager.fDevice.fAllocationCallbacks,@fMemoryHandle));

 fOffsetRedBlackTree:=TVulkanDeviceMemoryChunkBlockRedBlackTree.Create;
 fSizeRedBlackTree:=TVulkanDeviceMemoryChunkBlockRedBlackTree.Create;

 TVulkanDeviceMemoryChunkBlock.Create(self,0,pSize,false);

 fLock:=TCriticalSection.Create;

 if assigned(fMemoryChunkList^.First) then begin
  fMemoryChunkList^.First.fPreviousMemoryChunk:=self;
  fNextMemoryChunk:=fMemoryChunkList^.First;
 end else begin
  fMemoryChunkList^.Last:=self;
  fNextMemoryChunk:=nil;
 end;
 fMemoryChunkList^.First:=self;
 fPreviousMemoryChunk:=nil;

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
 end else if fMemoryChunkList^.First=self then begin
  fMemoryChunkList^.First:=fNextMemoryChunk;
 end;
 if assigned(fNextMemoryChunk) then begin
  fNextMemoryChunk.fPreviousMemoryChunk:=fPreviousMemoryChunk;
 end else if fMemoryChunkList^.Last=self then begin
  fMemoryChunkList^.Last:=fPreviousMemoryChunk;
 end;

 if fMemoryHandle<>VK_NULL_HANDLE then begin
  if ((fMemoryPropertyFlags and TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))<>0) and assigned(fMemory) then begin
   fMemoryManager.fDevice.Commands.UnmapMemory(fMemoryManager.fDevice.fDeviceHandle,fMemoryHandle);
   fMemory:=nil;
  end;
  fMemoryManager.fDevice.Commands.FreeMemory(fMemoryManager.fDevice.fDeviceHandle,fMemoryHandle,fMemoryManager.fDevice.fAllocationCallbacks);
 end;

 fOffsetRedBlackTree.Free;
 fSizeRedBlackTree.Free;

 FreeAndNil(fLock);

 fMemoryHandle:=VK_NULL_HANDLE;

 inherited Destroy;
end;

function TVulkanDeviceMemoryChunk.AllocateMemory(out pOffset:TVkDeviceSize;const pSize:TVkDeviceSize):boolean;
var Node,OtherNode:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
    MemoryChunkBlock:TVulkanDeviceMemoryChunkBlock;
    TempOffset,TempSize,Size:TVkDeviceSize;
begin
 result:=false;

 fLock.Acquire;
 try

  Size:=pSize;

  // Ensure alignment
  if (fAlignment>1) and ((Size and (fAlignment-1))<>0) then begin
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
   inc(fUsed,Size);
   result:=true;
  end;

 finally
  fLock.Release;
 end;

end;

function TVulkanDeviceMemoryChunk.ReallocateMemory(var pOffset:TVkDeviceSize;const pSize:TVkDeviceSize):boolean;
var Node,OtherNode:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
    MemoryChunkBlock,OtherMemoryChunkBlock:TVulkanDeviceMemoryChunkBlock;
    Size,TempOffset,TempSize:TVkDeviceSize;
begin
 result:=false;

 fLock.Acquire;
 try

  Size:=pSize;

  // Ensure alignment
  if (Size and (fAlignment-1))<>0 then begin
   inc(Size,fAlignment-(Size and (fAlignment-1)));
  end;

  Node:=fOffsetRedBlackTree.Find(pOffset);
  if assigned(Node) then begin
   MemoryChunkBlock:=Node.fValue;
   if MemoryChunkBlock.fUsed then begin
    dec(fUsed,MemoryChunkBlock.Size);
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
    if result then begin
     inc(fUsed,Size);
    end;
   end;
  end;

 finally
  fLock.Release;
 end;

end;

function TVulkanDeviceMemoryChunk.FreeMemory(const pOffset:TVkDeviceSize):boolean;
var Node,OtherNode:TVulkanDeviceMemoryChunkBlockRedBlackTreeNode;
    MemoryChunkBlock,OtherMemoryChunkBlock:TVulkanDeviceMemoryChunkBlock;
    TempOffset,TempSize:TVkDeviceSize;
begin
 result:=false;

 fLock.Acquire;
 try

  Node:=fOffsetRedBlackTree.Find(pOffset);
  if assigned(Node) then begin

   MemoryChunkBlock:=Node.fValue;
   if MemoryChunkBlock.fUsed then begin

    dec(fUsed,MemoryChunkBlock.fSize);

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

    result:=true;
    
   end;

  end;
  
 finally
  fLock.Release;
 end;
end;

function TVulkanDeviceMemoryChunk.MapMemory(const pOffset:TVkDeviceSize=0;const pSize:TVkDeviceSize=TVkDeviceSize(VK_WHOLE_SIZE)):PVkVoid;
begin
 result:=nil;
 fLock.Acquire;
 try
  if (fMemoryPropertyFlags and TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))<>0 then begin
   if assigned(fMemory) then begin
    raise EVulkanException.Create('Memory is already mapped');
   end else begin
    fMappedOffset:=pOffset;
    fMappedSize:=pSize;
    HandleResultCode(fMemoryManager.fDevice.Commands.MapMemory(fMemoryManager.fDevice.fDeviceHandle,fMemoryHandle,pOffset,pSize,0,@result));
    fMemory:=result;
   end;
  end else begin
   raise EVulkanException.Create('Memory can''t mapped');
  end;
 finally
  fLock.Release;
 end;
end;

procedure TVulkanDeviceMemoryChunk.UnmapMemory;
begin
 fLock.Acquire;
 try
  if (fMemoryPropertyFlags and TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))<>0 then begin
   if assigned(fMemory) then begin
    fMemoryManager.fDevice.Commands.UnmapMemory(fMemoryManager.fDevice.fDeviceHandle,fMemoryHandle);
    fMemory:=nil;
   end else begin
    raise EVulkanException.Create('Non-mapped memory can''t unmapped');
   end;
  end;
 finally
  fLock.Release;
 end;
end;

procedure TVulkanDeviceMemoryChunk.FlushMappedMemory;
var MappedMemoryRange:TVkMappedMemoryRange;
begin
 fLock.Acquire;
 try
  if assigned(fMemory) then begin
   FillChar(MappedMemoryRange,SizeOf(TVkMappedMemoryRange),#0);
   MappedMemoryRange.sType:=VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE;
   MappedMemoryRange.pNext:=nil;
   MappedMemoryRange.memory:=fMemoryHandle;
   MappedMemoryRange.offset:=fMappedOffset;
   MappedMemoryRange.size:=fMappedSize;
   HandleResultCode(vkFlushMappedMemoryRanges(fMemoryManager.fDevice.fDeviceHandle,1,@MappedMemoryRange));
  end else begin
   raise EVulkanException.Create('Non-mapped memory can''t be flushed');
  end;
 finally
  fLock.Release;
 end;
end;

procedure TVulkanDeviceMemoryChunk.InvalidateMappedMemory;
var MappedMemoryRange:TVkMappedMemoryRange;
begin
 fLock.Acquire;
 try
  if assigned(fMemory) then begin
   FillChar(MappedMemoryRange,SizeOf(TVkMappedMemoryRange),#0);
   MappedMemoryRange.sType:=VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE;
   MappedMemoryRange.pNext:=nil;
   MappedMemoryRange.memory:=fMemoryHandle;
   MappedMemoryRange.offset:=fMappedOffset;
   MappedMemoryRange.size:=fMappedSize;
   HandleResultCode(vkInvalidateMappedMemoryRanges(fMemoryManager.fDevice.fDeviceHandle,1,@MappedMemoryRange));
  end else begin
   raise EVulkanException.Create('Non-mapped memory can''t be invalidated');
  end;
 finally
  fLock.Release;
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

 fOffset:=pOffset;

 fSize:=pSize;

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

function TVulkanDeviceMemoryBlock.MapMemory(const pOffset:TVkDeviceSize=0;const pSize:TVkDeviceSize=TVkDeviceSize(VK_WHOLE_SIZE)):PVkVoid;
var Offset,Size:TVkDeviceSize;
begin
 Offset:=fOffset+pOffset;
 if pSize=TVkDeviceSize(VK_WHOLE_SIZE) then begin
  Size:=TVkInt64(Max(0,TVkInt64((fOffset+fSize)-Offset)));
 end else begin
  Size:=Min(TVkInt64(Max(TVkInt64(pSize),0)),TVkInt64(Max(0,TVkInt64((fOffset+fSize)-Offset))));
 end;
 result:=fMemoryChunk.MapMemory(Offset,Size);
end;

procedure TVulkanDeviceMemoryBlock.UnmapMemory;
begin
 fMemoryChunk.UnmapMemory;
end;

procedure TVulkanDeviceMemoryBlock.FlushMappedMemory;
begin
 fMemoryChunk.FlushMappedMemory;
end;

procedure TVulkanDeviceMemoryBlock.InvalidateMappedMemory;
begin
 fMemoryChunk.InvalidateMappedMemory;
end;

function TVulkanDeviceMemoryBlock.Fill(const pData:PVkVoid;const pSize:TVkDeviceSize):TVkDeviceSize;
var Memory:PVkVoid;
begin
 if pSize<=0 then begin
  result:=0;
 end else if pSize>fSize then begin
  result:=fSize;
 end else begin
  result:=pSize;
 end;
 Memory:=MapMemory;
 try
  Move(pData^,Memory^,result);
 finally
  UnmapMemory;
 end;
end;

constructor TVulkanDeviceMemoryManager.Create(const pDevice:TVulkanDevice);
begin
 inherited Create;

 fDevice:=pDevice;

 fLock:=TCriticalSection.Create;

 FillChar(fMemoryChunkLists,SizeOf(TVulkanDeviceMemoryManagerChunkLists),#0);

 fFirstMemoryBlock:=nil;
 fLastMemoryBlock:=nil;

end;

destructor TVulkanDeviceMemoryManager.Destroy;
var Index:TVkInt32;
    MemoryChunkList:PVulkanDeviceMemoryManagerChunkList;
begin
 while assigned(fFirstMemoryBlock) do begin
  fFirstMemoryBlock.Free;
 end;
 for Index:=low(TVulkanDeviceMemoryManagerChunkLists) to high(TVulkanDeviceMemoryManagerChunkLists) do begin
  MemoryChunkList:=@fMemoryChunkLists[Index];
  while assigned(MemoryChunkList^.First) do begin
   MemoryChunkList^.First.Free;
  end;
 enD;
 fLock.Free;
 inherited Destroy;
end;

function TVulkanDeviceMemoryManager.AllocateMemoryBlock(const pSize:TVkDeviceSize;
                                                        const pMemoryTypeBits:TVkUInt32;
                                                        const pMemoryPropertyFlags:TVkMemoryPropertyFlags;
                                                        const pAlignment:TVkDeviceSize=16;
                                                        const pOwnSingleMemoryChunk:boolean=false):TVulkanDeviceMemoryBlock;
var MemoryChunkList:PVulkanDeviceMemoryManagerChunkList;
    MemoryChunk:TVulkanDeviceMemoryChunk;
    Offset,Alignment:TVkDeviceSize;
begin

 result:=nil;

 if pSize=0 then begin
  raise EVulkanMemoryAllocation.Create('Can''t allocate zero-sized memory block');
 end;

 if pOwnSingleMemoryChunk then begin

  Alignment:=1;

  MemoryChunkList:=@fMemoryChunkLists[0];

  fLock.Acquire;
  try
   // Allocate a block inside a new chunk
   MemoryChunk:=TVulkanDeviceMemoryChunk.Create(self,pSize,Alignment,pMemoryTypeBits,pMemoryPropertyFlags,MemoryChunkList);
   if MemoryChunk.AllocateMemory(Offset,pSize) then begin
    result:=TVulkanDeviceMemoryBlock.Create(self,MemoryChunk,Offset,pSize);
   end;
  finally
   fLock.Release;
  end;

 end else begin

  Alignment:=pAlignment-1;
  Alignment:=Alignment or (Alignment shr 1);
  Alignment:=Alignment or (Alignment shr 2);
  Alignment:=Alignment or (Alignment shr 4);
  Alignment:=Alignment or (Alignment shr 8);
  Alignment:=Alignment or (Alignment shr 16);
  Alignment:=(Alignment or (Alignment shr 32))+1;

  MemoryChunkList:=@fMemoryChunkLists[CTZDWord(Alignment) and (high(TVulkanDeviceMemoryManagerChunkLists)-1)];

  fLock.Acquire;
  try

   // Try first to allocate a block inside already existent chunks
   MemoryChunk:=MemoryChunkList^.First;
   while assigned(MemoryChunk) do begin
    if ((pMemoryTypeBits and MemoryChunk.fMemoryTypeBits)<>0) and
       ((MemoryChunk.fMemoryPropertyFlags and pMemoryPropertyFlags)=pMemoryPropertyFlags) and
       ((MemoryChunk.fSize-MemoryChunk.fUsed)>=pSize) then begin
     if MemoryChunk.AllocateMemory(Offset,pSize) then begin
      result:=TVulkanDeviceMemoryBlock.Create(self,MemoryChunk,Offset,pSize);
      break;
     end;
    end;
    MemoryChunk:=MemoryChunk.fNextMemoryChunk;
   end;

   if not assigned(result) then begin
    // Otherwise allocate a block inside a new chunk
    MemoryChunk:=TVulkanDeviceMemoryChunk.Create(self,VulkanDeviceSizeRoundUpToPowerOfTwo(Max(1 shl 24,pSize shl 1)),Alignment,pMemoryTypeBits,pMemoryPropertyFlags,MemoryChunkList);
    if MemoryChunk.AllocateMemory(Offset,pSize) then begin
     result:=TVulkanDeviceMemoryBlock.Create(self,MemoryChunk,Offset,pSize);
    end;
   end;

  finally
   fLock.Release;
  end;

 end;

 if not assigned(result) then begin
  raise EVulkanMemoryAllocation.Create('Couldn''t allocate memory block');
 end;
 
end;

function TVulkanDeviceMemoryManager.FreeMemoryBlock(const pMemoryBlock:TVulkanDeviceMemoryBlock):boolean;
var MemoryChunk:TVulkanDeviceMemoryChunk;
begin
 result:=assigned(pMemoryBlock);
 if result then begin
  fLock.Acquire;
  try
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
  finally
   fLock.Release;
  end;
 end;
end;

constructor TVulkanBuffer.Create(const pDevice:TVulkanDevice;
                                 const pSize:TVkDeviceSize;
                                 const pUsage:TVkBufferUsageFlags;
                                 const pSharingMode:TVkSharingMode=VK_SHARING_MODE_EXCLUSIVE;
                                 const pQueueFamilyIndices:TVkUInt32List=nil;
                                 const pMemoryProperties:TVkMemoryPropertyFlags=TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
                                 const pOwnSingleMemoryChunk:boolean=false);
var Index:TVkInt32;
    BufferCreateInfo:TVkBufferCreateInfo;
begin
 inherited Create;

 fDevice:=pDevice;

 fSize:=pSize;

 fMemoryProperties:=pMemoryProperties;

 fOwnSingleMemoryChunk:=pOwnSingleMemoryChunk;

 fBufferHandle:=VK_NULL_HANDLE;

 fMemoryBlock:=nil;

 fQueueFamilyIndices:=nil;
 if assigned(pQueueFamilyIndices) then begin
  fCountQueueFamilyIndices:=pQueueFamilyIndices.Count;
  SetLength(fQueueFamilyIndices,fCountQueueFamilyIndices);
  for Index:=0 to fCountQueueFamilyIndices-1 do begin
   fQueueFamilyIndices[Index]:=pQueueFamilyIndices.Items[Index];
  end;
 end else begin
  fCountQueueFamilyIndices:=0;
 end;

 FillChar(BufferCreateInfo,SizeOf(TVkBufferCreateInfo),#0);
 BufferCreateInfo.sType:=VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
 BufferCreateInfo.size:=fSize;
 BufferCreateInfo.usage:=pUsage;
 BufferCreateInfo.sharingMode:=pSharingMode;
 if fCountQueueFamilyIndices>0 then begin
  BufferCreateInfo.pQueueFamilyIndices:=@fQueueFamilyIndices[0];
  BufferCreateInfo.queueFamilyIndexCount:=fCountQueueFamilyIndices;
 end;

 try

  HandleResultCode(fDevice.Commands.CreateBuffer(fDevice.fDeviceHandle,@BufferCreateInfo,fDevice.fAllocationCallbacks,@fBufferHandle));

  fDevice.Commands.GetBufferMemoryRequirements(fDevice.fDeviceHandle,fBufferHandle,@fMemoryRequirements);

  fMemoryBlock:=fDevice.fMemoryManager.AllocateMemoryBlock(fMemoryRequirements.Size,
                                                           fMemoryRequirements.memoryTypeBits,
                                                           fMemoryProperties,
                                                           fMemoryRequirements.Alignment,
                                                           fOwnSingleMemoryChunk);
 except

  if fBufferHandle<>VK_NULL_HANDLE then begin
   fDevice.Commands.DestroyBuffer(fDevice.fDeviceHandle,fBufferHandle,fDevice.fAllocationCallbacks);
   fBufferHandle:=VK_NULL_HANDLE;
  end;

  if assigned(fMemoryBlock) then begin
   fDevice.fMemoryManager.FreeMemoryBlock(fMemoryBlock);
   fMemoryBlock:=nil;
  end;

  SetLength(fQueueFamilyIndices,0);

  raise;

 end;

end;

destructor TVulkanBuffer.Destroy;
begin
 if fBufferHandle<>VK_NULL_HANDLE then begin
  fDevice.Commands.DestroyBuffer(fDevice.fDeviceHandle,fBufferHandle,fDevice.fAllocationCallbacks);
  fBufferHandle:=VK_NULL_HANDLE;
 end;
 if assigned(fMemoryBlock) then begin
  fDevice.fMemoryManager.FreeMemoryBlock(fMemoryBlock);
  fMemoryBlock:=nil;
 end;
 SetLength(fQueueFamilyIndices,0);
 inherited Destroy;
end;

procedure TVulkanBuffer.Bind;
begin
 HandleResultCode(fDevice.Commands.BindBufferMemory(fDevice.fDeviceHandle,fBufferHandle,fMemoryBlock.fMemoryChunk.fMemoryHandle,fMemoryBlock.fOffset));
end;

constructor TVulkanBufferView.Create(const pDevice:TVulkanDevice;
                                     const pBuffer:TVulkanBuffer;
                                     const pFormat:TVkFormat;
                                     const pOffset:TVkDeviceSize;
                                     const pRange:TVkDeviceSize);
var BufferViewCreateInfo:TVkBufferViewCreateInfo;
begin

 inherited Create;

 fDevice:=pDevice;

 fBuffer:=pBuffer;

 fBufferViewHandle:=VK_NULL_HANDLE;

 FillChar(BufferViewCreateInfo,SizeOf(TVkBufferViewCreateInfo),#0);
 BufferViewCreateInfo.sType:=VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO;
 BufferViewCreateInfo.pNext:=nil;
 BufferViewCreateInfo.flags:=0;
 BufferViewCreateInfo.buffer:=fBuffer.fBufferHandle;
 BufferViewCreateInfo.format:=pFormat;
 BufferViewCreateInfo.offset:=pOffset;
 BufferViewCreateInfo.range:=pRange;

 HandleResultCode(fDevice.fDeviceVulkan.CreateBufferView(fDevice.fDeviceHandle,@BufferViewCreateInfo,fDevice.fAllocationCallbacks,@fBufferViewHandle));

end;

constructor TVulkanBufferView.Create(const pDevice:TVulkanDevice;
                                     const pBufferView:TVkBufferView;
                                     const pBuffer:TVulkanBuffer=nil);
begin

 inherited Create;

 fDevice:=pDevice;

 fBufferViewHandle:=pBufferView;

 fBuffer:=pBuffer;

end;

destructor TVulkanBufferView.Destroy;
begin
 fBuffer:=nil;
 if fBufferViewHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.DestroyBufferView(fDevice.fDeviceHandle,fBufferViewHandle,fDevice.fAllocationCallbacks);
  fBufferViewHandle:=VK_NULL_HANDLE;
 end;
 inherited Destroy;
end;

constructor TVulkanEvent.Create(const pDevice:TVulkanDevice;
                                const pFlags:TVkEventCreateFlags=TVkEventCreateFlags(0));
var EventCreateInfo:TVkEventCreateInfo;
begin
 inherited Create;

 fDevice:=pDevice;

 fEventHandle:=VK_NULL_HANDLE;

 FillChar(EventCreateInfo,SizeOf(TVkEventCreateInfo),#0);
 EventCreateInfo.sType:=VK_STRUCTURE_TYPE_EVENT_CREATE_INFO;
 EventCreateInfo.pNext:=nil;
 EventCreateInfo.flags:=pFlags;

 HandleResultCode(fDevice.fDeviceVulkan.CreateEvent(fDevice.fDeviceHandle,@EventCreateInfo,fDevice.fAllocationCallbacks,@fEventHandle));

end;

destructor TVulkanEvent.Destroy;
begin
 if fEventHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.DestroyEvent(fDevice.fDeviceHandle,fEventHandle,fDevice.fAllocationCallbacks);
  fEventHandle:=VK_NULL_HANDLE;
 end;
 inherited Destroy;
end;

function TVulkanEvent.GetStatus:TVkResult;
begin
 result:=fDevice.fDeviceVulkan.GetEventStatus(fDevice.fDeviceHandle,fEventHandle);
end;

function TVulkanEvent.SetEvent:TVkResult;
begin
 result:=fDevice.fDeviceVulkan.SetEvent(fDevice.fDeviceHandle,fEventHandle);
 if result<VK_SUCCESS then begin
  HandleResultCode(result);
 end;
end;

function TVulkanEvent.Reset:TVkResult;
begin
 result:=fDevice.fDeviceVulkan.ResetEvent(fDevice.fDeviceHandle,fEventHandle);
 if result<VK_SUCCESS then begin
  HandleResultCode(result);
 end;
end;

constructor TVulkanFence.Create(const pDevice:TVulkanDevice;
                                const pFlags:TVkFenceCreateFlags=TVkFenceCreateFlags(0));
var FenceCreateInfo:TVkFenceCreateInfo;
begin
 inherited Create;

 fDevice:=pDevice;

 fFenceHandle:=VK_NULL_HANDLE;

 FillChar(FenceCreateInfo,SizeOf(TVkFenceCreateInfo),#0);
 FenceCreateInfo.sType:=VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
 FenceCreateInfo.pNext:=nil;
 FenceCreateInfo.flags:=pFlags;

 HandleResultCode(fDevice.fDeviceVulkan.CreateFence(fDevice.fDeviceHandle,@FenceCreateInfo,fDevice.fAllocationCallbacks,@fFenceHandle));

end;

destructor TVulkanFence.Destroy;
begin
 if fFenceHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.DestroyFence(fDevice.fDeviceHandle,fFenceHandle,fDevice.fAllocationCallbacks);
  fFenceHandle:=VK_NULL_HANDLE;
 end;
 inherited Destroy;
end;

function TVulkanFence.GetStatus:TVkResult;
begin
 result:=fDevice.fDeviceVulkan.GetFenceStatus(fDevice.fDeviceHandle,fFenceHandle);
end;

function TVulkanFence.Reset:TVkResult;
begin
 result:=fDevice.fDeviceVulkan.ResetFences(fDevice.fDeviceHandle,1,@fFenceHandle);
 if result<VK_SUCCESS then begin
  HandleResultCode(result);
 end;
end;

class function TVulkanFence.Reset(const pFences:array of TVulkanFence):TVkResult;
var Index:TVkInt32;
    Handles:array of TVkFence;
begin
 Handles:=nil;
 result:=VK_SUCCESS;
 if length(pFences)>0 then begin
  try
   SetLength(Handles,length(pFences));
   for Index:=0 to length(pFences)-1 do begin
    Handles[Index]:=pFences[Index].fFenceHandle;
   end;
   result:=pFences[0].fDevice.fDeviceVulkan.ResetFences(pFences[0].fDevice.fDeviceHandle,length(pFences),@Handles[0]);
  finally
   SetLength(Handles,0);
  end;
  if result<VK_SUCCESS then begin
   HandleResultCode(result);
  end;
 end;
end;

function TVulkanFence.WaitFor(const pTimeOut:TVkUInt64=TVKUInt64(TVKInt64(-1))):TVkResult;
begin
 result:=fDevice.fDeviceVulkan.WaitForFences(fDevice.fDeviceHandle,1,@fFenceHandle,VK_TRUE,pTimeOut);
 if result<VK_SUCCESS then begin
  HandleResultCode(result);
 end;
end;

class function TVulkanFence.WaitFor(const pFences:array of TVulkanFence;const pWaitAll:boolean=true;const pTimeOut:TVkUInt64=TVKUInt64(TVKInt64(-1))):TVkResult;
var Index:TVkInt32;
    Handles:array of TVkFence;
begin
 Handles:=nil;
 result:=VK_SUCCESS;
 if length(pFences)>0 then begin
  try
   SetLength(Handles,length(pFences));
   for Index:=0 to length(pFences)-1 do begin
    Handles[Index]:=pFences[Index].fFenceHandle;
   end;
   if pWaitAll then begin
    result:=pFences[0].fDevice.fDeviceVulkan.WaitForFences(pFences[0].fDevice.fDeviceHandle,length(pFences),@Handles[0],VK_TRUE,pTimeOut);
   end else begin
    result:=pFences[0].fDevice.fDeviceVulkan.WaitForFences(pFences[0].fDevice.fDeviceHandle,length(pFences),@Handles[0],VK_FALSE,pTimeOut);
   end;
  finally
   SetLength(Handles,0);
  end;
  if result<VK_SUCCESS then begin
   HandleResultCode(result);
  end;
 end;
end;

constructor TVulkanSemaphore.Create(const pDevice:TVulkanDevice;
                                    const pFlags:TVkSemaphoreCreateFlags=TVkSemaphoreCreateFlags(0));
var SemaphoreCreateInfo:TVkSemaphoreCreateInfo;
begin
 inherited Create;

 fDevice:=pDevice;

 fSemaphoreHandle:=VK_NULL_HANDLE;

 FillChar(SemaphoreCreateInfo,SizeOf(TVkSemaphoreCreateInfo),#0);
 SemaphoreCreateInfo.sType:=VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
 SemaphoreCreateInfo.pNext:=nil;
 SemaphoreCreateInfo.flags:=pFlags;

 HandleResultCode(fDevice.fDeviceVulkan.CreateSemaphore(fDevice.fDeviceHandle,@SemaphoreCreateInfo,fDevice.fAllocationCallbacks,@fSemaphoreHandle));

end;

destructor TVulkanSemaphore.Destroy;
begin
 if fSemaphoreHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.DestroySemaphore(fDevice.fDeviceHandle,fSemaphoreHandle,fDevice.fAllocationCallbacks);
  fSemaphoreHandle:=VK_NULL_HANDLE;
 end;
 inherited Destroy;
end;

constructor TVulkanQueue.Create(const pDevice:TVulkanDevice;
                                const pQueue:TVkQueue;
                                const pQueueFamilyIndex:TVKUInt32);
begin
 inherited Create;

 fDevice:=pDevice;

 fQueueHandle:=pQueue;

 fQueueFamilyIndex:=pQueueFamilyIndex;

 fHasSupportForSparseBindings:=fDevice.fPhysicalDevice.HasQueueSupportForSparseBindings(pQueueFamilyIndex);

end;

destructor TVulkanQueue.Destroy;
begin
 inherited Destroy;
end;

procedure TVulkanQueue.Submit(const pSubmitCount:TVkUInt32;const pSubmits:PVkSubmitInfo;const pFence:TVulkanFence=nil);
begin
 if assigned(pFence) then begin
  HandleResultCode(fDevice.fDeviceVulkan.QueueSubmit(fQueueHandle,pSubmitCount,pSubmits,pFence.fFenceHandle));
 end else begin
  HandleResultCode(fDevice.fDeviceVulkan.QueueSubmit(fQueueHandle,pSubmitCount,pSubmits,VK_NULL_HANDLE));
 end;
end;

procedure TVulkanQueue.BindSparse(const pBindInfoCount:TVkUInt32;const pBindInfo:PVkBindSparseInfo;const pFence:TVulkanFence=nil);
begin
 if assigned(pFence) then begin
  HandleResultCode(fDevice.fDeviceVulkan.QueueBindSparse(fQueueHandle,pBindInfoCount,pBindInfo,pFence.fFenceHandle));
 end else begin
  HandleResultCode(fDevice.fDeviceVulkan.QueueBindSparse(fQueueHandle,pBindInfoCount,pBindInfo,VK_NULL_HANDLE));
 end;
end;

procedure TVulkanQueue.WaitIdle;
begin
 HandleResultCode(fDevice.fDeviceVulkan.QueueWaitIdle(fQueueHandle));
end;

constructor TVulkanCommandPool.Create(const pDevice:TVulkanDevice;
                                      const pQueueFamilyIndex:TVkUInt32;
                                      const pFlags:TVkCommandPoolCreateFlags=TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
var CommandPoolCreateInfo:TVkCommandPoolCreateInfo;
begin
 inherited Create;

 fDevice:=pDevice;

 fQueueFamilyIndex:=pQueueFamilyIndex;

 fFlags:=pFlags;

 fCommandPoolHandle:=VK_NULL_HANDLE;

 FillChar(CommandPoolCreateInfo,SizeOf(TVkCommandPoolCreateInfo),#0);
 CommandPoolCreateInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
 CommandPoolCreateInfo.queueFamilyIndex:=fQueueFamilyIndex;
 CommandPoolCreateInfo.flags:=fFlags;
 HandleResultCode(fDevice.fDeviceVulkan.CreateCommandPool(fDevice.fDeviceHandle,@CommandPoolCreateInfo,fDevice.fAllocationCallbacks,@fCommandPoolHandle));

end;

destructor TVulkanCommandPool.Destroy;
begin
 if fCommandPoolHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.DestroyCommandPool(fDevice.fDeviceHandle,fCommandPoolHandle,fDevice.fAllocationCallbacks);
  fCommandPoolHandle:=VK_NULL_HANDLE;
 end;
 inherited Destroy;
end;

constructor TVulkanCommandBuffer.Create(const pCommandPool:TVulkanCommandPool;
                                        const pLevel:TVkCommandBufferLevel;
                                        const pCommandBufferHandle:TVkCommandBuffer);
begin

 fDevice:=pCommandPool.fDevice;

 fCommandPool:=pCommandPool;

 fLevel:=pLevel;

 fCommandBufferHandle:=pCommandBufferHandle;

{if fLevel=VK_COMMAND_BUFFER_LEVEL_PRIMARY then begin
  fFence:=TVulkanFence.Create(fDevice);
 end else begin
  fFence:=nil;
 end;{}

end;

constructor TVulkanCommandBuffer.Create(const pCommandPool:TVulkanCommandPool;
                                        const pLevel:TVkCommandBufferLevel=VK_COMMAND_BUFFER_LEVEL_PRIMARY);
var CommandBufferAllocateInfo:TVkCommandBufferAllocateInfo;
begin
 inherited Create;

 fDevice:=pCommandPool.fDevice;

 fCommandPool:=pCommandPool;

 fLevel:=pLevel;

 fCommandBufferHandle:=VK_NULL_HANDLE;

{if fLevel=VK_COMMAND_BUFFER_LEVEL_PRIMARY then begin
  fFence:=TVulkanFence.Create(fDevice);
 end else begin
  fFence:=nil;
 end;{}

 FillChar(CommandBufferAllocateInfo,SizeOf(TVkCommandBufferAllocateInfo),#0);
 CommandBufferAllocateInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
 CommandBufferAllocateInfo.commandPool:=fCommandPool.fCommandPoolHandle;
 CommandBufferAllocateInfo.level:=pLevel;
 CommandBufferAllocateInfo.commandBufferCount:=1;

 HandleResultCode(fDevice.fDeviceVulkan.AllocateCommandBuffers(fDevice.fDeviceHandle,@CommandBufferAllocateInfo,@fCommandBufferHandle));

end;

destructor TVulkanCommandBuffer.Destroy;
begin
 if fCommandBufferHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.FreeCommandBuffers(fDevice.fDeviceHandle,fCommandPool.fCommandPoolHandle,1,@fCommandBufferHandle);
  fCommandBufferHandle:=VK_NULL_HANDLE;
 end;
//FreeAndNil(fFence);
 inherited Destroy;
end;

class function TVulkanCommandBuffer.Allocate(const pCommandPool:TVulkanCommandPool;
                                             const pLevel:TVkCommandBufferLevel=VK_COMMAND_BUFFER_LEVEL_PRIMARY;
                                             const pCommandBufferCount:TVkUInt32=1):TVulkanObjectList;
var Index:TVkInt32;
    CommandBufferHandles:array of TVkCommandBuffer;
    CommandBufferAllocateInfo:TVkCommandBufferAllocateInfo;
begin
 result:=nil;
 CommandBufferHandles:=nil;
 try
  SetLength(CommandBufferHandles,pCommandBufferCount);

  FillChar(CommandBufferAllocateInfo,SizeOf(TVkCommandBufferAllocateInfo),#0);
  CommandBufferAllocateInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
  CommandBufferAllocateInfo.commandPool:=pCommandPool.fCommandPoolHandle;
  CommandBufferAllocateInfo.level:=pLevel;
  CommandBufferAllocateInfo.commandBufferCount:=pCommandBufferCount;

  HandleResultCode(pCommandPool.fDevice.fDeviceVulkan.AllocateCommandBuffers(pCommandPool.fDevice.fDeviceHandle,@CommandBufferAllocateInfo,@CommandBufferHandles[0]));

  result:=TVulkanObjectList.Create;
  for Index:=0 to pCommandBufferCount-1 do begin
   result.Add(TVulkanCommandBuffer.Create(pCommandPool,pLevel,CommandBufferHandles[Index]));
  end;

 finally
  SetLength(CommandBufferHandles,0);
 end;
end;

procedure TVulkanCommandBuffer.BeginRecording(const pFlags:TVkCommandBufferUsageFlags=0;const pInheritanceInfo:PVkCommandBufferInheritanceInfo=nil);
var CommandBufferBeginInfo:TVkCommandBufferBeginInfo;
begin
 FillChar(CommandBufferBeginInfo,SizeOf(TVkCommandBufferBeginInfo),#0);
 CommandBufferBeginInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
 CommandBufferBeginInfo.pNext:=nil;
 CommandBufferBeginInfo.flags:=pFlags;
 CommandBufferBeginInfo.pInheritanceInfo:=pInheritanceInfo;
 HandleResultCode(fDevice.fDeviceVulkan.BeginCommandBuffer(fCommandBufferHandle,@CommandBufferBeginInfo));
end;

procedure TVulkanCommandBuffer.BeginRecordingPrimary;
var CommandBufferBeginInfo:TVkCommandBufferBeginInfo;
begin
 if fLevel=VK_COMMAND_BUFFER_LEVEL_PRIMARY then begin
  FillChar(CommandBufferBeginInfo,SizeOf(TVkCommandBufferBeginInfo),#0);
  CommandBufferBeginInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
  CommandBufferBeginInfo.pNext:=nil;
  CommandBufferBeginInfo.flags:=0;
  CommandBufferBeginInfo.pInheritanceInfo:=nil;
  HandleResultCode(fDevice.fDeviceVulkan.BeginCommandBuffer(fCommandBufferHandle,@CommandBufferBeginInfo));
 end else begin
  raise EVulkanException.Create('BeginRecordingPrimary called from a non-primary command buffer!');
 end;
end;

procedure TVulkanCommandBuffer.BeginRecordingSecondary(const pRenderPass:TVkRenderPass;const pSubPass:TVkUInt32;const pFrameBuffer:TVkFramebuffer;const pOcclusionQueryEnable:boolean;const pQueryFlags:TVkQueryControlFlags;const pPipelineStatistics:TVkQueryPipelineStatisticFlags);
var CommandBufferBeginInfo:TVkCommandBufferBeginInfo;
    InheritanceInfo:TVkCommandBufferInheritanceInfo;
begin
 if fLevel=VK_COMMAND_BUFFER_LEVEL_SECONDARY then begin
  FillChar(InheritanceInfo,SizeOf(TVkCommandBufferInheritanceInfo),#0);
  InheritanceInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO;
  InheritanceInfo.pNext:=nil;
  InheritanceInfo.renderPass:=pRenderPass;
  InheritanceInfo.subpass:=pSubPass;
  InheritanceInfo.framebuffer:=pFrameBuffer;
  if pOcclusionQueryEnable then begin
   InheritanceInfo.occlusionQueryEnable:=VK_TRUE;
  end else begin
   InheritanceInfo.occlusionQueryEnable:=VK_FALSE;
  end;
  InheritanceInfo.queryFlags:=pQueryFlags;
  InheritanceInfo.pipelineStatistics:=pPipelineStatistics;
  FillChar(CommandBufferBeginInfo,SizeOf(TVkCommandBufferBeginInfo),#0);
  CommandBufferBeginInfo.sType:=VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
  CommandBufferBeginInfo.pNext:=nil;
  CommandBufferBeginInfo.flags:=TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT);
  CommandBufferBeginInfo.pInheritanceInfo:=@InheritanceInfo;
  HandleResultCode(fDevice.fDeviceVulkan.BeginCommandBuffer(fCommandBufferHandle,@CommandBufferBeginInfo));
 end else begin
  raise EVulkanException.Create('BeginRecordingSecondary called from a non-secondary command buffer!');
 end;
end;

procedure TVulkanCommandBuffer.EndRecording;
begin
 HandleResultCode(fDevice.fDeviceVulkan.EndCommandBuffer(fCommandBufferHandle));
end;

procedure TVulkanCommandBuffer.Reset(const pFlags:TVkCommandBufferResetFlags=TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));
begin
 HandleResultCode(fDevice.fDeviceVulkan.ResetCommandBuffer(fCommandBufferHandle,pFlags));
end;

procedure TVulkanCommandBuffer.CmdBindPipeline(pipelineBindPoint:TVkPipelineBindPoint;pipeline:TVkPipeline);
begin
 fDevice.fDeviceVulkan.CmdBindPipeline(fCommandBufferHandle,pipelineBindPoint,pipeline);
end;

procedure TVulkanCommandBuffer.CmdSetViewport(firstViewport:TVkUInt32;viewportCount:TVkUInt32;const pViewports:PVkViewport);
begin
 fDevice.fDeviceVulkan.CmdSetViewport(fCommandBufferHandle,firstViewport,viewportCount,pViewports);
end;

procedure TVulkanCommandBuffer.CmdSetScissor(firstScissor:TVkUInt32;scissorCount:TVkUInt32;const pScissors:PVkRect2D);
begin
 fDevice.fDeviceVulkan.CmdSetScissor(fCommandBufferHandle,firstScissor,scissorCount,pScissors);
end;

procedure TVulkanCommandBuffer.CmdSetLineWidth(lineWidth:TVkFloat);
begin
 fDevice.fDeviceVulkan.CmdSetLineWidth(fCommandBufferHandle,lineWidth);
end;

procedure TVulkanCommandBuffer.CmdSetDepthBias(depthBiasConstantFactor:TVkFloat;depthBiasClamp:TVkFloat;depthBiasSlopeFactor:TVkFloat);
begin
 fDevice.fDeviceVulkan.CmdSetDepthBias(fCommandBufferHandle,depthBiasConstantFactor,depthBiasClamp,depthBiasSlopeFactor);
end;

procedure TVulkanCommandBuffer.CmdSetBlendConstants(const blendConstants:TVkFloat);
begin
 fDevice.fDeviceVulkan.CmdSetBlendConstants(fCommandBufferHandle,blendConstants);
end;

procedure TVulkanCommandBuffer.CmdSetDepthBounds(minDepthBounds:TVkFloat;maxDepthBounds:TVkFloat);
begin
 fDevice.fDeviceVulkan.CmdSetDepthBounds(fCommandBufferHandle,minDepthBounds,maxDepthBounds);
end;

procedure TVulkanCommandBuffer.CmdSetStencilCompareMask(faceMask:TVkStencilFaceFlags;compareMask:TVkUInt32);
begin
 fDevice.fDeviceVulkan.CmdSetStencilCompareMask(fCommandBufferHandle,faceMask,compareMask);
end;

procedure TVulkanCommandBuffer.CmdSetStencilWriteMask(faceMask:TVkStencilFaceFlags;writeMask:TVkUInt32);
begin
 fDevice.fDeviceVulkan.CmdSetStencilWriteMask(fCommandBufferHandle,faceMask,writeMask);
end;

procedure TVulkanCommandBuffer.CmdSetStencilReference(faceMask:TVkStencilFaceFlags;reference:TVkUInt32);
begin
 fDevice.fDeviceVulkan.CmdSetStencilReference(fCommandBufferHandle,faceMask,reference);
end;

procedure TVulkanCommandBuffer.CmdBindDescriptorSets(pipelineBindPoint:TVkPipelineBindPoint;layout:TVkPipelineLayout;firstSet:TVkUInt32;descriptorSetCount:TVkUInt32;const pDescriptorSets:PVkDescriptorSet;dynamicOffsetCount:TVkUInt32;const pDynamicOffsets:PVkUInt32);
begin
 fDevice.fDeviceVulkan.CmdBindDescriptorSets(fCommandBufferHandle,pipelineBindPoint,layout,firstSet,descriptorSetCount,pDescriptorSets,dynamicOffsetCount,pDynamicOffsets);
end;

procedure TVulkanCommandBuffer.CmdBindIndexBuffer(buffer:TVkBuffer;offset:TVkDeviceSize;indexType:TVkIndexType);
begin
 fDevice.fDeviceVulkan.CmdBindIndexBuffer(fCommandBufferHandle,buffer,offset,indexType);
end;

procedure TVulkanCommandBuffer.CmdBindVertexBuffers(firstBinding:TVkUInt32;bindingCount:TVkUInt32;const pBuffers:PVkBuffer;const pOffsets:PVkDeviceSize);
begin
 fDevice.fDeviceVulkan.CmdBindVertexBuffers(fCommandBufferHandle,firstBinding,bindingCount,pBuffers,pOffsets);
end;

procedure TVulkanCommandBuffer.CmdDraw(vertexCount:TVkUInt32;instanceCount:TVkUInt32;firstVertex:TVkUInt32;firstInstance:TVkUInt32);
begin
 fDevice.fDeviceVulkan.CmdDraw(fCommandBufferHandle,vertexCount,instanceCount,firstVertex,firstInstance);
end;

procedure TVulkanCommandBuffer.CmdDrawIndexed(indexCount:TVkUInt32;instanceCount:TVkUInt32;firstIndex:TVkUInt32;vertexOffset:TVkInt32;firstInstance:TVkUInt32);
begin
 fDevice.fDeviceVulkan.CmdDrawIndexed(fCommandBufferHandle,indexCount,instanceCount,firstIndex,vertexOffset,firstInstance);
end;

procedure TVulkanCommandBuffer.CmdDrawIndirect(buffer:TVkBuffer;offset:TVkDeviceSize;drawCount:TVkUInt32;stride:TVkUInt32);
begin
 fDevice.fDeviceVulkan.CmdDrawIndirect(fCommandBufferHandle,buffer,offset,drawCount,stride);
end;

procedure TVulkanCommandBuffer.CmdDrawIndexedIndirect(buffer:TVkBuffer;offset:TVkDeviceSize;drawCount:TVkUInt32;stride:TVkUInt32);
begin
 fDevice.fDeviceVulkan.CmdDrawIndexedIndirect(fCommandBufferHandle,buffer,offset,drawCount,stride);
end;

procedure TVulkanCommandBuffer.CmdDispatch(x:TVkUInt32;y:TVkUInt32;z:TVkUInt32);
begin
 fDevice.fDeviceVulkan.CmdDispatch(fCommandBufferHandle,x,y,z);
end;

procedure TVulkanCommandBuffer.CmdDispatchIndirect(buffer:TVkBuffer;offset:TVkDeviceSize);
begin
 fDevice.fDeviceVulkan.CmdDispatchIndirect(fCommandBufferHandle,buffer,offset);
end;

procedure TVulkanCommandBuffer.CmdCopyBuffer(srcBuffer:TVkBuffer;dstBuffer:TVkBuffer;regionCount:TVkUInt32;const pRegions:PVkBufferCopy);
begin
 fDevice.fDeviceVulkan.CmdCopyBuffer(fCommandBufferHandle,srcBuffer,dstBuffer,regionCount,pRegions);
end;

procedure TVulkanCommandBuffer.CmdCopyImage(srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageCopy);
begin
 fDevice.fDeviceVulkan.CmdCopyImage(fCommandBufferHandle,srcImage,srcImageLayout,dstImage,dstImageLayout,regionCount,pRegions);
end;

procedure TVulkanCommandBuffer.CmdBlitImage(srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageBlit;filter:TVkFilter);
begin
 fDevice.fDeviceVulkan.CmdBlitImage(fCommandBufferHandle,srcImage,srcImageLayout,dstImage,dstImageLayout,regionCount,pRegions,filter);
end;

procedure TVulkanCommandBuffer.CmdCopyBufferToImage(srcBuffer:TVkBuffer;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkBufferImageCopy);
begin
 fDevice.fDeviceVulkan.CmdCopyBufferToImage(fCommandBufferHandle,srcBuffer,dstImage,dstImageLayout,regionCount,pRegions);
end;

procedure TVulkanCommandBuffer.CmdCopyImageToBuffer(srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstBuffer:TVkBuffer;regionCount:TVkUInt32;const pRegions:PVkBufferImageCopy);
begin
 fDevice.fDeviceVulkan.CmdCopyImageToBuffer(fCommandBufferHandle,srcImage,srcImageLayout,dstBuffer,regionCount,pRegions);
end;

procedure TVulkanCommandBuffer.CmdUpdateBuffer(dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;dataSize:TVkDeviceSize;const pData:PVkVoid);
begin
 fDevice.fDeviceVulkan.CmdUpdateBuffer(fCommandBufferHandle,dstBuffer,dstOffset,dataSize,pData);
end;

procedure TVulkanCommandBuffer.CmdFillBuffer(dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;size:TVkDeviceSize;data:TVkUInt32);
begin
 fDevice.fDeviceVulkan.CmdFillBuffer(fCommandBufferHandle,dstBuffer,dstOffset,size,data);
end;

procedure TVulkanCommandBuffer.CmdClearColorImage(image:TVkImage;imageLayout:TVkImageLayout;const pColor:PVkClearColorValue;rangeCount:TVkUInt32;const pRanges:PVkImageSubresourceRange);
begin
 fDevice.fDeviceVulkan.CmdClearColorImage(fCommandBufferHandle,image,imageLayout,pColor,rangeCount,pRanges);
end;

procedure TVulkanCommandBuffer.CmdClearDepthStencilImage(image:TVkImage;imageLayout:TVkImageLayout;const pDepthStencil:PVkClearDepthStencilValue;rangeCount:TVkUInt32;const pRanges:PVkImageSubresourceRange);
begin
 fDevice.fDeviceVulkan.CmdClearDepthStencilImage(fCommandBufferHandle,image,imageLayout,pDepthStencil,rangeCount,pRanges);
end;

procedure TVulkanCommandBuffer.CmdClearAttachments(attachmentCount:TVkUInt32;const pAttachments:PVkClearAttachment;rectCount:TVkUInt32;const pRects:PVkClearRect);
begin
 fDevice.fDeviceVulkan.CmdClearAttachments(fCommandBufferHandle,attachmentCount,pAttachments,rectCount,pRects);
end;

procedure TVulkanCommandBuffer.CmdResolveImage(srcImage:TVkImage;srcImageLayout:TVkImageLayout;dstImage:TVkImage;dstImageLayout:TVkImageLayout;regionCount:TVkUInt32;const pRegions:PVkImageResolve);
begin
 fDevice.fDeviceVulkan.CmdResolveImage(fCommandBufferHandle,srcImage,srcImageLayout,dstImage,dstImageLayout,regionCount,pRegions);
end;

procedure TVulkanCommandBuffer.CmdSetEvent(event:TVkEvent;stageMask:TVkPipelineStageFlags);
begin
 fDevice.fDeviceVulkan.CmdSetEvent(fCommandBufferHandle,event,stageMask);
end;

procedure TVulkanCommandBuffer.CmdResetEvent(event:TVkEvent;stageMask:TVkPipelineStageFlags);
begin
 fDevice.fDeviceVulkan.CmdResetEvent(fCommandBufferHandle,event,stageMask);
end;

procedure TVulkanCommandBuffer.CmdWaitEvents(eventCount:TVkUInt32;const pEvents:PVkEvent;srcStageMask:TVkPipelineStageFlags;dstStageMask:TVkPipelineStageFlags;memoryBarrierCount:TVkUInt32;const pMemoryBarriers:PVkMemoryBarrier;bufferMemoryBarrierCount:TVkUInt32;const pBufferMemoryBarriers:PVkBufferMemoryBarrier;imageMemoryBarrierCount:TVkUInt32;const pImageMemoryBarriers:PVkImageMemoryBarrier);
begin
 fDevice.fDeviceVulkan.CmdWaitEvents(fCommandBufferHandle,eventCount,pEvents,srcStageMask,dstStageMask,memoryBarrierCount,pMemoryBarriers,bufferMemoryBarrierCount,pBufferMemoryBarriers,imageMemoryBarrierCount,pImageMemoryBarriers);
end;

procedure TVulkanCommandBuffer.CmdPipelineBarrier(srcStageMask:TVkPipelineStageFlags;dstStageMask:TVkPipelineStageFlags;dependencyFlags:TVkDependencyFlags;memoryBarrierCount:TVkUInt32;const pMemoryBarriers:PVkMemoryBarrier;bufferMemoryBarrierCount:TVkUInt32;const pBufferMemoryBarriers:PVkBufferMemoryBarrier;imageMemoryBarrierCount:TVkUInt32;const pImageMemoryBarriers:PVkImageMemoryBarrier);
begin
 fDevice.fDeviceVulkan.CmdPipelineBarrier(fCommandBufferHandle,srcStageMask,dstStageMask,dependencyFlags,memoryBarrierCount,pMemoryBarriers,bufferMemoryBarrierCount,pBufferMemoryBarriers,imageMemoryBarrierCount,pImageMemoryBarriers);
end;

procedure TVulkanCommandBuffer.CmdBeginQuery(queryPool:TVkQueryPool;query:TVkUInt32;flags:TVkQueryControlFlags);
begin
 fDevice.fDeviceVulkan.CmdBeginQuery(fCommandBufferHandle,queryPool,query,flags);
end;

procedure TVulkanCommandBuffer.CmdEndQuery(queryPool:TVkQueryPool;query:TVkUInt32);
begin
 fDevice.fDeviceVulkan.CmdEndQuery(fCommandBufferHandle,queryPool,query);
end;

procedure TVulkanCommandBuffer.CmdResetQueryPool(queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32);
begin
 fDevice.fDeviceVulkan.CmdResetQueryPool(fCommandBufferHandle,queryPool,firstQuery,queryCount);
end;

procedure TVulkanCommandBuffer.CmdWriteTimestamp(pipelineStage:TVkPipelineStageFlagBits;queryPool:TVkQueryPool;query:TVkUInt32);
begin
 fDevice.fDeviceVulkan.CmdWriteTimestamp(fCommandBufferHandle,pipelineStage,queryPool,query);
end;

procedure TVulkanCommandBuffer.CmdCopyQueryPoolResults(queryPool:TVkQueryPool;firstQuery:TVkUInt32;queryCount:TVkUInt32;dstBuffer:TVkBuffer;dstOffset:TVkDeviceSize;stride:TVkDeviceSize;flags:TVkQueryResultFlags);
begin
 fDevice.fDeviceVulkan.CmdCopyQueryPoolResults(fCommandBufferHandle,queryPool,firstQuery,queryCount,dstBuffer,dstOffset,stride,flags);
end;

procedure TVulkanCommandBuffer.CmdPushConstants(layout:TVkPipelineLayout;stageFlags:TVkShaderStageFlags;offset:TVkUInt32;size:TVkUInt32;const pValues:PVkVoid);
begin
 fDevice.fDeviceVulkan.CmdPushConstants(fCommandBufferHandle,layout,stageFlags,offset,size,pValues);
end;

procedure TVulkanCommandBuffer.CmdBeginRenderPass(const pRenderPassBegin:PVkRenderPassBeginInfo;contents:TVkSubpassContents);
begin
 fDevice.fDeviceVulkan.CmdBeginRenderPass(fCommandBufferHandle,pRenderPassBegin,contents);
end;

procedure TVulkanCommandBuffer.CmdNextSubpass(contents:TVkSubpassContents);
begin
 fDevice.fDeviceVulkan.CmdNextSubpass(fCommandBufferHandle,contents);
end;

procedure TVulkanCommandBuffer.CmdEndRenderPass;
begin
 fDevice.fDeviceVulkan.CmdEndRenderPass(fCommandBufferHandle);
end;

procedure TVulkanCommandBuffer.CmdExecuteCommands(commandBufferCount:TVkUInt32;const pCommandBuffers:PVkCommandBuffer);
begin
 fDevice.fDeviceVulkan.CmdExecuteCommands(fCommandBufferHandle,commandBufferCount,pCommandBuffers);
end;

procedure TVulkanCommandBuffer.CmdExecute(const pCommandBuffer:TVulkanCommandBuffer);
begin
 CmdExecuteCommands(1,@pCommandBuffer.fCommandBufferHandle);
end;

procedure TVulkanCommandBuffer.MetaCmdPresentToDrawImageBarrier(const pImage:TVulkanImage);
var ImageMemoryBarrier:TVkImageMemoryBarrier;
begin
 FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
 ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 ImageMemoryBarrier.pNext:=nil;
 ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT);
 ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
 ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
 ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
 if (fDevice.fPresentQueueFamilyIndex<>fDevice.fGraphicsQueueFamilyIndex) or not
    (assigned(fDevice.fPresentQueue) and assigned(fDevice.fGraphicsQueue)) then begin
  ImageMemoryBarrier.srcQueueFamilyIndex:=fDevice.fPresentQueueFamilyIndex;
  ImageMemoryBarrier.dstQueueFamilyIndex:=fDevice.fGraphicsQueueFamilyIndex;
 end else begin
  ImageMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 end;
 ImageMemoryBarrier.image:=pImage.fImageHandle;
 ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
 ImageMemoryBarrier.subresourceRange.levelCount:=1;
 ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
 ImageMemoryBarrier.subresourceRange.layerCount:=1;
 CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                    0,
                    0,nil,
                    0,nil,
                    1,@ImageMemoryBarrier);
end;

procedure TVulkanCommandBuffer.MetaCmdDrawToPresentImageBarrier(const pImage:TVulkanImage);
var ImageMemoryBarrier:TVkImageMemoryBarrier;
begin
 FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
 ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
 ImageMemoryBarrier.pNext:=nil;
 ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
 ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT);
 ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
 ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
 if (fDevice.fPresentQueueFamilyIndex<>fDevice.fGraphicsQueueFamilyIndex) or not
    (assigned(fDevice.fPresentQueue) and assigned(fDevice.fGraphicsQueue)) then begin
  ImageMemoryBarrier.srcQueueFamilyIndex:=fDevice.fGraphicsQueueFamilyIndex;
  ImageMemoryBarrier.dstQueueFamilyIndex:=fDevice.fPresentQueueFamilyIndex;
 end else begin
  ImageMemoryBarrier.srcQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
  ImageMemoryBarrier.dstQueueFamilyIndex:=VK_QUEUE_FAMILY_IGNORED;
 end;
 ImageMemoryBarrier.image:=pImage.fImageHandle;
 ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
 ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
 ImageMemoryBarrier.subresourceRange.levelCount:=1;
 ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
 ImageMemoryBarrier.subresourceRange.layerCount:=1;
 CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                    TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                    0,
                    0,nil,
                    0,nil,
                    1,@ImageMemoryBarrier);
end;

procedure TVulkanCommandBuffer.Execute(const pQueue:TVulkanQueue;const pFence:TVulkanFence;const pFlags:TVkPipelineStageFlags;const pWaitSemaphore:TVulkanSemaphore=nil;const pSignalSemaphore:TVulkanSemaphore=nil);
var SubmitInfo:TVkSubmitInfo;
begin
 if fLevel=VK_COMMAND_BUFFER_LEVEL_PRIMARY then begin

  FillChar(SubmitInfo,SizeOf(TVkSubmitInfo),#0);
  SubmitInfo.sType:=VK_STRUCTURE_TYPE_SUBMIT_INFO;
  SubmitInfo.pNext:=nil;
  if assigned(pWaitSemaphore) then begin
   SubmitInfo.waitSemaphoreCount:=1;
   SubmitInfo.pWaitSemaphores:=@pWaitSemaphore.fSemaphoreHandle;
  end else begin
   SubmitInfo.waitSemaphoreCount:=0;
   SubmitInfo.pWaitSemaphores:=nil;
  end;
  SubmitInfo.pWaitDstStageMask:=@pFlags;
  SubmitInfo.commandBufferCount:=1;
  SubmitInfo.pCommandBuffers:=@fCommandBufferHandle;
  if assigned(pSignalSemaphore) then begin
   SubmitInfo.signalSemaphoreCount:=1;
   SubmitInfo.pSignalSemaphores:=@pSignalSemaphore.fSemaphoreHandle;
  end else begin
   SubmitInfo.signalSemaphoreCount:=0;
   SubmitInfo.pSignalSemaphores:=nil;
  end;

  if assigned(pFence) then begin

   pQueue.Submit(1,@SubmitInfo,pFence);

   pFence.WaitFor;
   pFence.Reset;

  end else begin

   pQueue.Submit(1,@SubmitInfo,nil);

  end;

 end else begin
  raise EVulkanException.Create('Execute called from a non-primary command buffer!');
 end;
end;

constructor TVulkanRenderPass.Create(const pDevice:TVulkanDevice);
begin
 inherited Create;

 fDevice:=pDevice;

 fRenderPassHandle:=VK_NULL_HANDLE;

 fAttachmentDescriptions:=nil;
 fCountAttachmentDescriptions:=0;

 fAttachmentReferences:=nil;
 fCountAttachmentReferences:=0;

 fRenderPassSubpassDescriptions:=nil;
 fSubpassDescriptions:=nil;
 fCountSubpassDescriptions:=0;

 fSubpassDependencies:=nil;
 fCountSubpassDependencies:=0;

 fClearValues:=nil;

end;

destructor TVulkanRenderPass.Destroy;
begin
 if fRenderPassHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.DestroyRenderPass(fDevice.fDeviceHandle,fRenderPassHandle,fDevice.fAllocationCallbacks);
  fRenderPassHandle:=VK_NULL_HANDLE;
 end;
 SetLength(fAttachmentDescriptions,0);
 SetLength(fAttachmentReferences,0);
 SetLength(fRenderPassSubpassDescriptions,0);
 SetLength(fSubpassDescriptions,0);
 SetLength(fSubpassDependencies,0);
 SetLength(fClearValues,0);
 inherited Destroy;
end;

function TVulkanRenderPass.GetClearValue(const Index:TVkUInt32):PVkClearValue;
begin
 result:=@fClearValues[Index];
end;

function TVulkanRenderPass.AddAttachmentDescription(const pFlags:TVkAttachmentDescriptionFlags;
                                                    const pFormat:TVkFormat;
                                                    const pSamples:TVkSampleCountFlagBits;
                                                    const pLoadOp:TVkAttachmentLoadOp;
                                                    const pStoreOp:TVkAttachmentStoreOp;
                                                    const pStencilLoadOp:TVkAttachmentLoadOp;
                                                    const pStencilStoreOp:TVkAttachmentStoreOp;
                                                    const pInitialLayout:TVkImageLayout;
                                                    const pFinalLayout:TVkImageLayout):TVkUInt32;
var AttachmentDescription:PVkAttachmentDescription;
begin
 result:=fCountAttachmentDescriptions;
 inc(fCountAttachmentDescriptions);
 if fCountAttachmentDescriptions>length(fAttachmentDescriptions) then begin
  SetLength(fAttachmentDescriptions,fCountAttachmentDescriptions*2);
 end;
 AttachmentDescription:=@fAttachmentDescriptions[result];
 AttachmentDescription^.flags:=pFlags;
 AttachmentDescription^.format:=pFormat;
 AttachmentDescription^.samples:=pSamples;
 AttachmentDescription^.loadOp:=pLoadOp;
 AttachmentDescription^.storeOp:=pStoreOp;
 AttachmentDescription^.stencilLoadOp:=pStencilLoadOp;
 AttachmentDescription^.stencilStoreOp:=pStencilStoreOp;
 AttachmentDescription^.initialLayout:=pInitialLayout;
 AttachmentDescription^.finalLayout:=pFinalLayout;
end;

function TVulkanRenderPass.AddAttachmentReference(const pAttachment:TVkUInt32;
                                                  const pLayout:TVkImageLayout):TVkUInt32;
var AttachmentReference:PVkAttachmentReference;
begin
 result:=fCountAttachmentReferences;
 inc(fCountAttachmentReferences);
 if fCountAttachmentReferences>length(fAttachmentReferences) then begin
  SetLength(fAttachmentReferences,fCountAttachmentReferences*2);
 end;
 AttachmentReference:=@fAttachmentReferences[result];
 AttachmentReference^.attachment:=pAttachment;
 AttachmentReference^.layout:=pLayout;
end;

function TVulkanRenderPass.AddSubpassDescription(const pFlags:TVkSubpassDescriptionFlags;
                                                 const pPipelineBindPoint:TVkPipelineBindPoint;
                                                 const pInputAttachments:array of TVkInt32;
                                                 const pColorAttachments:array of TVkInt32;
                                                 const pResolveAttachments:array of TVkInt32;
                                                 const pDepthStencilAttachment:TVkInt32;
                                                 const pPreserveAttachments:array of TVkUInt32):TVkUInt32;
var RenderPassSubpassDescription:PVulkanRenderPassSubpassDescription;
begin
 result:=fCountSubpassDescriptions;
 inc(fCountSubpassDescriptions);
 if fCountSubpassDescriptions>length(fRenderPassSubpassDescriptions) then begin
  SetLength(fRenderPassSubpassDescriptions,fCountSubpassDescriptions*2);
 end;
 RenderPassSubpassDescription:=@fRenderPassSubpassDescriptions[result];
 RenderPassSubpassDescription^.Flags:=pFlags;
 RenderPassSubpassDescription^.PipelineBindPoint:=pPipelineBindPoint;
 begin
  SetLength(RenderPassSubpassDescription^.InputAttachments,length(pInputAttachments));
  if length(pInputAttachments)>0 then begin
   Move(pInputAttachments[0],RenderPassSubpassDescription^.InputAttachments[0],length(pInputAttachments)*SizeOf(TVkInt32));
  end;
 end;
 begin
  SetLength(RenderPassSubpassDescription^.ColorAttachments,length(pColorAttachments));
  if length(pColorAttachments)>0 then begin
   Move(pColorAttachments[0],RenderPassSubpassDescription^.ColorAttachments[0],length(pColorAttachments)*SizeOf(TVkInt32));
  end;
 end;
 begin
  SetLength(RenderPassSubpassDescription^.ResolveAttachments,length(pResolveAttachments));
  if length(pResolveAttachments)>0 then begin
   Move(pResolveAttachments[0],RenderPassSubpassDescription^.ResolveAttachments[0],length(pResolveAttachments)*SizeOf(TVkInt32));
  end;
 end;
 RenderPassSubpassDescription^.DepthStencilAttachment:=pDepthStencilAttachment;
 begin
  SetLength(RenderPassSubpassDescription^.PreserveAttachments,length(pPreserveAttachments));
  if length(pPreserveAttachments)>0 then begin
   Move(pPreserveAttachments[0],RenderPassSubpassDescription^.PreserveAttachments[0],length(pPreserveAttachments)*SizeOf(TVkUInt32));
  end;
 end;
end;

function TVulkanRenderPass.AddSubpassDependency(const pSrcSubpass:TVkUInt32;
                                                const pDstSubpass:TVkUInt32;
                                                const pSrcStageMask:TVkPipelineStageFlags;
                                                const pDstStageMask:TVkPipelineStageFlags;
                                                const pSrcAccessMask:TVkAccessFlags;
                                                const pDstAccessMask:TVkAccessFlags;
                                                const pDependencyFlags:TVkDependencyFlags):TVkUInt32;
var SubpassDependency:PVkSubpassDependency;
begin
 result:=fCountSubpassDependencies;
 inc(fCountSubpassDependencies);
 if fCountSubpassDependencies>length(fSubpassDependencies) then begin
  SetLength(fSubpassDependencies,fCountSubpassDependencies*2);
 end;
 SubpassDependency:=@fSubpassDependencies[result];
 SubpassDependency^.srcSubpass:=pSrcSubpass;
 SubpassDependency^.dstSubpass:=pDstSubpass;
 SubpassDependency^.srcStageMask:=pSrcStageMask;
 SubpassDependency^.dstStageMask:=pDstStageMask;
 SubpassDependency^.srcAccessMask:=pSrcAccessMask;
 SubpassDependency^.dstAccessMask:=pDstAccessMask;
 SubpassDependency^.DependencyFlags:=pDependencyFlags;
end;

procedure TVulkanRenderPass.Initialize;
var Index,SubIndex:TVkInt32;
    AttachmentDescription:PVkAttachmentDescription;
    SubpassDescription:PVkSubpassDescription;
    RenderPassSubpassDescription:PVulkanRenderPassSubpassDescription;
    ClearValue:PVkClearValue;
    RenderPassCreateInfo:TVkRenderPassCreateInfo;
begin

 FillChar(RenderPassCreateInfo,Sizeof(TVkRenderPassCreateInfo),#0);
 RenderPassCreateInfo.sType:=VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO;

 SetLength(fAttachmentDescriptions,fCountAttachmentDescriptions);
 SetLength(fAttachmentReferences,fCountAttachmentReferences);
 SetLength(fRenderPassSubpassDescriptions,fCountSubpassDescriptions);
 SetLength(fSubpassDescriptions,fCountSubpassDescriptions);
 SetLength(fSubpassDependencies,fCountSubpassDependencies);

 SetLength(fClearValues,fCountAttachmentDescriptions);

 if fCountAttachmentDescriptions>0 then begin
  for Index:=0 to fCountAttachmentDescriptions-1 do begin
   AttachmentDescription:=@fAttachmentDescriptions[Index];
   ClearValue:=@fClearValues[Index];
   case AttachmentDescription^.format of
    VK_FORMAT_D32_SFLOAT_S8_UINT,
    VK_FORMAT_D32_SFLOAT,
    VK_FORMAT_D24_UNORM_S8_UINT,
    VK_FORMAT_D16_UNORM_S8_UINT,
    VK_FORMAT_D16_UNORM:begin
     ClearValue^.depthStencil.depth:=1.0;
     ClearValue^.depthStencil.stencil:=0;
    end;
    else begin
     ClearValue^.color.uint32[0]:=0;
     ClearValue^.color.uint32[1]:=0;
     ClearValue^.color.uint32[2]:=0;
     ClearValue^.color.uint32[3]:=0;
    end;
   end;
  end;
  RenderPassCreateInfo.attachmentCount:=fCountAttachmentDescriptions;
  RenderPassCreateInfo.pAttachments:=@fAttachmentDescriptions[0];
 end;

 if fCountSubpassDescriptions>0 then begin
  for Index:=0 to fCountSubpassDescriptions-1 do begin
   SubpassDescription:=@fSubpassDescriptions[Index];
   RenderPassSubpassDescription:=@fRenderPassSubpassDescriptions[Index];
   FillChar(SubpassDescription^,SizeOf(TVkSubpassDescription),#0);
   SubpassDescription^.flags:=RenderPassSubpassDescription^.Flags;
   SubpassDescription^.pipelineBindPoint:=RenderPassSubpassDescription^.PipelineBindPoint;
   begin
    SubpassDescription^.inputAttachmentCount:=length(RenderPassSubpassDescription^.InputAttachments);
    if SubpassDescription^.inputAttachmentCount>0 then begin
     SetLength(RenderPassSubpassDescription^.pInputAttachments,SubpassDescription^.inputAttachmentCount);
     for SubIndex:=0 to length(RenderPassSubpassDescription^.InputAttachments)-1 do begin
      RenderPassSubpassDescription^.pInputAttachments[SubIndex]:=fAttachmentReferences[RenderPassSubpassDescription^.InputAttachments[SubIndex]];
     end;
     SubpassDescription^.pInputAttachments:=@RenderPassSubpassDescription^.pInputAttachments[0];
    end;
   end;
   begin
    SubpassDescription^.ColorAttachmentCount:=length(RenderPassSubpassDescription^.ColorAttachments);
    if SubpassDescription^.ColorAttachmentCount>0 then begin
     SetLength(RenderPassSubpassDescription^.pColorAttachments,SubpassDescription^.ColorAttachmentCount);
     for SubIndex:=0 to length(RenderPassSubpassDescription^.ColorAttachments)-1 do begin
      RenderPassSubpassDescription^.pColorAttachments[SubIndex]:=fAttachmentReferences[RenderPassSubpassDescription^.ColorAttachments[SubIndex]];
     end;
     SubpassDescription^.pColorAttachments:=@RenderPassSubpassDescription^.pColorAttachments[0];
    end;
   end;
   begin
    if (SubpassDescription^.ColorAttachmentCount>0) and
       (SubpassDescription^.ColorAttachmentCount=TVkUInt32(length(RenderPassSubpassDescription^.ResolveAttachments))) then begin
     SetLength(RenderPassSubpassDescription^.pResolveAttachments,SubpassDescription^.ColorAttachmentCount);
     for SubIndex:=0 to length(RenderPassSubpassDescription^.ResolveAttachments)-1 do begin
      RenderPassSubpassDescription^.pResolveAttachments[SubIndex]:=fAttachmentReferences[RenderPassSubpassDescription^.ResolveAttachments[SubIndex]];
     end;
     SubpassDescription^.pResolveAttachments:=@RenderPassSubpassDescription^.pResolveAttachments[0];
    end;
   end;
   if RenderPassSubpassDescription^.DepthStencilAttachment>=0 then begin
    SubpassDescription^.pDepthStencilAttachment:=@fAttachmentReferences[RenderPassSubpassDescription^.DepthStencilAttachment];
   end;
   begin
    SubpassDescription^.PreserveAttachmentCount:=length(RenderPassSubpassDescription^.PreserveAttachments);
    if SubpassDescription^.PreserveAttachmentCount>0 then begin
     SubpassDescription^.pPreserveAttachments:=@RenderPassSubpassDescription^.PreserveAttachments[0];
    end;
   end;
  end;
  RenderPassCreateInfo.subpassCount:=fCountSubpassDescriptions;
  RenderPassCreateInfo.pSubpasses:=@fSubpassDescriptions[0];
 end;

 if fCountSubpassDependencies>0 then begin
  RenderPassCreateInfo.dependencyCount:=fCountSubpassDependencies;
  RenderPassCreateInfo.pDependencies:=@fSubpassDependencies[0];
 end;
 
 HandleResultCode(fDevice.fDeviceVulkan.CreateRenderPass(fDevice.fDeviceHandle,@RenderPassCreateInfo,fDevice.fAllocationCallbacks,@fRenderPassHandle));

end;

procedure TVulkanRenderPass.BeginRenderPass(const pCommandBuffer:TVulkanCommandBuffer;
                                            const pFrameBuffer:TVulkanFrameBuffer;
                                            const pSubpassContents:TVkSubpassContents;
                                            const pOffsetX,pOffsetY,pWidth,pHeight:TVkUInt32);
var RenderPassBeginInfo:TVkRenderPassBeginInfo;
begin
 FillChar(RenderPassBeginInfo,SizeOf(TVkRenderPassBeginInfo),#0);
 RenderPassBeginInfo.sType:=VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO;
 RenderPassBeginInfo.renderPass:=fRenderPassHandle;
 RenderPassBeginInfo.framebuffer:=pFrameBuffer.fFrameBufferHandle;
 RenderPassBeginInfo.renderArea.offset.x:=pOffsetX;
 RenderPassBeginInfo.renderArea.offset.y:=pOffsetY;
 RenderPassBeginInfo.renderArea.extent.width:=pWidth;
 RenderPassBeginInfo.renderArea.extent.height:=pHeight;
 RenderPassBeginInfo.clearValueCount:=length(fClearValues);
 if RenderPassBeginInfo.clearValueCount>0 then begin
  RenderPassBeginInfo.pClearValues:=@fClearValues[0];
 end;
 pCommandBuffer.CmdBeginRenderPass(@RenderPassBeginInfo,pSubpassContents);
end;

procedure TVulkanRenderPass.EndRenderPass(const pCommandBuffer:TVulkanCommandBuffer);
begin
 pCommandBuffer.CmdEndRenderPass;
end;

constructor TVulkanSampler.Create(const pDevice:TVulkanDevice;
                                  const pSampler:TVkSampler;
                                  const pDoDestroy:boolean=true);
begin

 inherited Create;

 fDevice:=pDevice;

 fSamplerHandle:=pSampler;

 fDoDestroy:=pDoDestroy;

end;

constructor TVulkanSampler.Create(const pDevice:TVulkanDevice;
                                  const pMagFilter:TVkFilter;
                                  const pMinFilter:TVkFilter;
                                  const pMipmapMode:TVkSamplerMipmapMode;
                                  const pAddressModeU:TVkSamplerAddressMode;
                                  const pAddressModeV:TVkSamplerAddressMode;
                                  const pAddressModeW:TVkSamplerAddressMode;
                                  const pMipLodBias:TVkFloat;
                                  const pAnisotropyEnable:boolean;
                                  const pMaxAnisotropy:TVkFloat;
                                  const pCompareEnable:boolean;
                                  const pCompareOp:TVkCompareOp;
                                  const pMinLod:TVkFloat;
                                  const pMaxLod:TVkFloat;
                                  const pBorderColor:TVkBorderColor;
                                  const pUnnormalizedCoordinates:boolean);
var SamplerCreateInfo:TVkSamplerCreateInfo;
begin

 inherited Create;

 fDevice:=pDevice;

 fSamplerHandle:=VK_NULL_HANDLE;

 fDoDestroy:=true;

 FillChar(SamplerCreateInfo,SizeOf(TVkSamplerCreateInfo),#0);
 SamplerCreateInfo.sType:=VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
 SamplerCreateInfo.pNext:=nil;
 SamplerCreateInfo.flags:=0;
 SamplerCreateInfo.magFilter:=pMagFilter;
 SamplerCreateInfo.minFilter:=pMinFilter;
 SamplerCreateInfo.mipmapMode:=pMipmapMode;
 SamplerCreateInfo.addressModeU:=pAddressModeU;
 SamplerCreateInfo.addressModeV:=pAddressModeV;
 SamplerCreateInfo.addressModeW:=pAddressModeW;
 SamplerCreateInfo.mipLodBias:=pMipLodBias;
 if pAnisotropyEnable then begin
  SamplerCreateInfo.anisotropyEnable:=VK_TRUE;
 end else begin
  SamplerCreateInfo.anisotropyEnable:=VK_FALSE;
 end;
 SamplerCreateInfo.maxAnisotropy:=pMaxAnisotropy;
 if pCompareEnable then begin
  SamplerCreateInfo.compareEnable:=VK_TRUE;
 end else begin
  SamplerCreateInfo.compareEnable:=VK_FALSE;
 end;
 SamplerCreateInfo.compareOp:=pCompareOp;
 SamplerCreateInfo.minLod:=pMinLod;
 SamplerCreateInfo.maxLod:=pMaxLod;
 SamplerCreateInfo.borderColor:=pBorderColor;
 if pUnnormalizedCoordinates then begin
  SamplerCreateInfo.unnormalizedCoordinates:=VK_TRUE;
 end else begin
  SamplerCreateInfo.unnormalizedCoordinates:=VK_FALSE;
 end;

 HandleResultCode(fDevice.fDeviceVulkan.CreateSampler(fDevice.fDeviceHandle,@SamplerCreateInfo,fDevice.fAllocationCallbacks,@fSamplerHandle));

end;

destructor TVulkanSampler.Destroy;
begin
 if fSamplerHandle<>VK_NULL_HANDLE then begin
  if fDoDestroy then begin
   fDevice.fDeviceVulkan.DestroySampler(fDevice.fDeviceHandle,fSamplerHandle,fDevice.fAllocationCallbacks);
  end;
  fSamplerHandle:=VK_NULL_HANDLE;
 end;
 inherited Destroy;
end;

constructor TVulkanImage.Create(const pDevice:TVulkanDevice;
                                const pImage:TVkImage;
                                const pImageView:TVulkanImageView=nil;
                                const pDoDestroy:boolean=true);
begin

 inherited Create;

 fDevice:=pDevice;

 fImageHandle:=pImage;

 fImageView:=pImageView;

 fDoDestroy:=pDoDestroy;

end;

constructor TVulkanImage.Create(const pDevice:TVulkanDevice;
                                const pFlags:TVkImageCreateFlags;
                                const pImageType:TVkImageType;
                                const pFormat:TVkFormat;
                                const pExtentWidth:TVkUInt32;
                                const pExtentHeight:TVkUInt32;
                                const pExtentDepth:TVkUInt32;
                                const pMipLevels:TVkUInt32;
                                const pArrayLayers:TVkUInt32;
                                const pSamples:TVkSampleCountFlagBits;
                                const pTiling:TVkImageTiling;
                                const pUsage:TVkImageUsageFlags;
                                const pSharingMode:TVkSharingMode;
                                const pQueueFamilyIndexCount:TVkUInt32;
                                const pQueueFamilyIndices:PVkUInt32;
                                const pInitialLayout:TVkImageLayout);
var ImageCreateInfo:TVkImageCreateInfo;
begin

 inherited Create;

 fDevice:=pDevice;

 fImageHandle:=VK_NULL_HANDLE;

 fImageView:=nil;

 fDoDestroy:=true;

 FillChar(ImageCreateInfo,SizeOf(TVkImageCreateInfo),#0);
 ImageCreateInfo.sType:=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
 ImageCreateInfo.pNext:=nil;
 ImageCreateInfo.flags:=pFlags;
 ImageCreateInfo.imageType:=pImageType;
 ImageCreateInfo.format:=pFormat;
 ImageCreateInfo.extent.width:=pExtentWidth;
 ImageCreateInfo.extent.height:=pExtentHeight;
 ImageCreateInfo.extent.depth:=pExtentDepth;
 ImageCreateInfo.mipLevels:=pMipLevels;
 ImageCreateInfo.arrayLayers:=pArrayLayers;
 ImageCreateInfo.samples:=pSamples;
 ImageCreateInfo.tiling:=pTiling;
 ImageCreateInfo.usage:=pUsage;
 ImageCreateInfo.sharingMode:=pSharingMode;
 ImageCreateInfo.queueFamilyIndexCount:=pQueueFamilyIndexCount;
 ImageCreateInfo.pQueueFamilyIndices:=pQueueFamilyIndices;
 ImageCreateInfo.initialLayout:=pInitialLayout;

 HandleResultCode(fDevice.fDeviceVulkan.CreateImage(fDevice.fDeviceHandle,@ImageCreateInfo,fDevice.fAllocationCallbacks,@fImageHandle));

end;

constructor TVulkanImage.Create(const pDevice:TVulkanDevice;
                                const pFlags:TVkImageCreateFlags;
                                const pImageType:TVkImageType;
                                const pFormat:TVkFormat;
                                const pExtentWidth:TVkUInt32;
                                const pExtentHeight:TVkUInt32;
                                const pExtentDepth:TVkUInt32;
                                const pMipLevels:TVkUInt32;
                                const pArrayLayers:TVkUInt32;
                                const pSamples:TVkSampleCountFlagBits;
                                const pTiling:TVkImageTiling;
                                const pUsage:TVkImageUsageFlags;
                                const pSharingMode:TVkSharingMode;
                                const pQueueFamilyIndices:array of TVkUInt32;
                                const pInitialLayout:TVkImageLayout);
var ImageCreateInfo:TVkImageCreateInfo;
begin

 inherited Create;

 fDevice:=pDevice;

 fImageHandle:=VK_NULL_HANDLE;

 fImageView:=nil;

 fDoDestroy:=true;

 FillChar(ImageCreateInfo,SizeOf(TVkImageCreateInfo),#0);
 ImageCreateInfo.sType:=VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
 ImageCreateInfo.pNext:=nil;
 ImageCreateInfo.flags:=pFlags;
 ImageCreateInfo.imageType:=pImageType;
 ImageCreateInfo.format:=pFormat;
 ImageCreateInfo.extent.width:=pExtentWidth;
 ImageCreateInfo.extent.height:=pExtentHeight;
 ImageCreateInfo.extent.depth:=pExtentDepth;
 ImageCreateInfo.mipLevels:=pMipLevels;
 ImageCreateInfo.arrayLayers:=pArrayLayers;
 ImageCreateInfo.samples:=pSamples;
 ImageCreateInfo.tiling:=pTiling;
 ImageCreateInfo.usage:=pUsage;
 ImageCreateInfo.sharingMode:=pSharingMode;
 ImageCreateInfo.queueFamilyIndexCount:=length(pQueueFamilyIndices);
 if ImageCreateInfo.queueFamilyIndexCount>0 then begin
  ImageCreateInfo.pQueueFamilyIndices:=@pQueueFamilyIndices[0];
 end else begin
  ImageCreateInfo.pQueueFamilyIndices:=nil;
 end;
 ImageCreateInfo.initialLayout:=pInitialLayout;

 HandleResultCode(fDevice.fDeviceVulkan.CreateImage(fDevice.fDeviceHandle,@ImageCreateInfo,fDevice.fAllocationCallbacks,@fImageHandle));

end;

destructor TVulkanImage.Destroy;
begin
 if assigned(fImageView) then begin
  if fImageView.fImage=self then begin
   fImageView.fImage:=nil;
  end;
  fImageView:=nil;
 end;
 if fImageHandle<>VK_NULL_HANDLE then begin
  if fDoDestroy then begin
   fDevice.fDeviceVulkan.DestroyImage(fDevice.fDeviceHandle,fImageHandle,fDevice.fAllocationCallbacks);
  end;
  fImageHandle:=VK_NULL_HANDLE;
 end;
 inherited Destroy;
end;

procedure TVulkanImage.SetLayout(const pAspectMask:TVkImageAspectFlags;
                                 const pOldImageLayout:TVkImageLayout;
                                 const pNewImageLayout:TVkImageLayout;
                                 const pRange:PVkImageSubresourceRange;
                                 const pCommandBuffer:TVulkanCommandBuffer;
                                 const pQueue:TVulkanQueue=nil;
                                 const pFence:TVulkanFence=nil;
                                 const pBeginAndExecuteCommandBuffer:boolean=false;
                                 const pSrcQueueFamilyIndex:TVkQueue=TVkQueue(VK_QUEUE_FAMILY_IGNORED);
                                 const pDstQueueFamilyIndex:TVkQueue=TVkQueue(VK_QUEUE_FAMILY_IGNORED));
begin
 VulkanSetImageLayout(fImageHandle,
                      pAspectMask,
                      pOldImageLayout,
                      pNewImageLayout,
                      pRange,
                      pCommandBuffer,
                      pQueue,
                      pFence,
                      pBeginAndExecuteCommandBuffer,
                      pSrcQueueFamilyIndex,
                      pDstQueueFamilyIndex);
end;

constructor TVulkanImageView.Create(const pDevice:TVulkanDevice;
                                    const pImageView:TVkImageView;
                                    const pImage:TVulkanImage=nil);
begin

 inherited Create;

 fDevice:=pDevice;

 fImageViewHandle:=pImageView;

 fImage:=pImage;

end;

constructor TVulkanImageView.Create(const pDevice:TVulkanDevice;
                                    const pImage:TVulkanImage;
                                    const pImageViewType:TVkImageViewType;
                                    const pFormat:TvkFormat;
                                    const pComponentRed:TVkComponentSwizzle=VK_COMPONENT_SWIZZLE_IDENTITY;
                                    const pComponentGreen:TVkComponentSwizzle=VK_COMPONENT_SWIZZLE_IDENTITY;
                                    const pComponentBlue:TVkComponentSwizzle=VK_COMPONENT_SWIZZLE_IDENTITY;
                                    const pComponentAlpha:TVkComponentSwizzle=VK_COMPONENT_SWIZZLE_IDENTITY;
                                    const pImageAspectFlags:TVkImageAspectFlags=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
                                    const pBaseMipLevel:TVkUInt32=0;
                                    const pCountMipMapLevels:TVkUInt32=1;
                                    const pBaseArrayLayer:TVkUInt32=1;
                                    const pCountArrayLayers:TVkUInt32=0);
var ImageViewCreateInfo:TVkImageViewCreateInfo;
begin

 inherited Create;

 fDevice:=pDevice;

 fImage:=pImage;

 fImageViewHandle:=VK_NULL_HANDLE;

 FillChar(ImageViewCreateInfo,SizeOf(TVkImageViewCreateInfo),#0);
 ImageViewCreateInfo.sType:=VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
 ImageViewCreateInfo.pNext:=nil;
 ImageViewCreateInfo.flags:=0;
 ImageViewCreateInfo.image:=pImage.fImageHandle;
 ImageViewCreateInfo.viewType:=pImageViewType;
 ImageViewCreateInfo.format:=pFormat;
 ImageViewCreateInfo.components.r:=pComponentRed;
 ImageViewCreateInfo.components.g:=pComponentGreen;
 ImageViewCreateInfo.components.b:=pComponentBlue;
 ImageViewCreateInfo.components.a:=pComponentAlpha;
 ImageViewCreateInfo.subresourceRange.aspectMask:=pImageAspectFlags;
 ImageViewCreateInfo.subresourceRange.baseMipLevel:=pBaseMipLevel;
 ImageViewCreateInfo.subresourceRange.levelCount:=pCountMipMapLevels;
 ImageViewCreateInfo.subresourceRange.baseArrayLayer:=pBaseArrayLayer;
 ImageViewCreateInfo.subresourceRange.layerCount:=pCountArrayLayers;

 HandleResultCode(fDevice.fDeviceVulkan.CreateImageView(fDevice.fDeviceHandle,@ImageViewCreateInfo,fDevice.fAllocationCallbacks,@fImageViewHandle));

end;

destructor TVulkanImageView.Destroy;
begin
 if assigned(fImage) then begin
  if fImage.fImageView=self then begin
   fImage.fImageView:=nil;
  end;
  fImage:=nil;
 end;
 if fImageViewHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.DestroyImageView(fDevice.fDeviceHandle,fImageViewHandle,fDevice.fAllocationCallbacks);
  fImageViewHandle:=VK_NULL_HANDLE;
 end;
 inherited Destroy;
end;

constructor TVulkanFrameBufferAttachment.Create(const pDevice:TVulkanDevice;
                                                const pCommandBuffer:TVulkanCommandBuffer;
                                                const pCommandBufferFence:TVulkanFence;
                                                const pWidth:TVkUInt32;
                                                const pHeight:TVkUInt32;
                                                const pFormat:TVkFormat;
                                                const pUsage:TVkBufferUsageFlags);
var MemoryRequirements:TVkMemoryRequirements;
    AspectMask:TVkImageAspectFlags;
    ImageLayout:TVkImageLayout;
begin
 inherited Create;

 fDevice:=pDevice;

 fWidth:=pWidth;

 fHeight:=pHeight;

 fFormat:=pFormat;

 fImage:=nil;

 fImageView:=nil;

 fMemoryBlock:=nil;

 fDoDestroy:=true;

 if (pUsage and TVkBufferUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT))<>0 then begin
  AspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
  ImageLayout:=VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
 end else if (pUsage and TVkBufferUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT))<>0 then begin
  if fFormat in [VK_FORMAT_D32_SFLOAT_S8_UINT,VK_FORMAT_D24_UNORM_S8_UINT,VK_FORMAT_D16_UNORM_S8_UINT] then begin
   AspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_DEPTH_BIT) or TVkImageAspectFlags(VK_IMAGE_ASPECT_STENCIL_BIT);
  end else begin
   AspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_DEPTH_BIT);
  end;
  ImageLayout:=VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
 end else begin
  raise EVulkanException.Create('Invalid frame buffer attachment');
 end;

 try

  fImage:=TVulkanImage.Create(fDevice,
                              0,
                              VK_IMAGE_TYPE_2D,
                              fFormat,
                              pWidth,
                              pHeight,
                              1,
                              1,
                              1,
                              VK_SAMPLE_COUNT_1_BIT,
                              VK_IMAGE_TILING_OPTIMAL,
                              pUsage {or TVkImageUsageFlags(VK_IMAGE_USAGE_SAMPLED_BIT)},
                              VK_SHARING_MODE_EXCLUSIVE,
                              [],
                              VK_IMAGE_LAYOUT_UNDEFINED);

  fDevice.fDeviceVulkan.GetImageMemoryRequirements(fDevice.fDeviceHandle,fImage.fImageHandle,@MemoryRequirements);

  fMemoryBlock:=fDevice.fMemoryManager.AllocateMemoryBlock(MemoryRequirements.size,
                                                           MemoryRequirements.memoryTypeBits,
                                                           TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT),
                                                           MemoryRequirements.alignment);
  if not assigned(fMemoryBlock) then begin
   raise EVulkanMemoryAllocation.Create('Memory for frame buffer attachment couldn''t be allocated!');
  end;

  HandleResultCode(fDevice.fDeviceVulkan.BindImageMemory(fDevice.fDeviceHandle,fImage.fImageHandle,fMemoryBlock.fMemoryChunk.fMemoryHandle,fMemoryBlock.fOffset));

  if (pUsage and TVkBufferUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT))<>0 then begin
   fImage.SetLayout(AspectMask,
                    VK_IMAGE_LAYOUT_UNDEFINED,
                    VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                    nil,
                    pCommandBuffer,
                    fDevice.fGraphicsQueue,
                    pCommandBufferFence,
                    true);
  end else begin
   fImage.SetLayout(AspectMask,
                    VK_IMAGE_LAYOUT_UNDEFINED,
                    ImageLayout,
                    nil,
                    pCommandBuffer,
                    fDevice.fGraphicsQueue,
                    pCommandBufferFence,
                    true);
  end;
          
  fImageView:=TVulkanImageView.Create(fDevice,
                                      fImage,
                                      VK_IMAGE_VIEW_TYPE_2D,
                                      fFormat,
                                      VK_COMPONENT_SWIZZLE_IDENTITY,
                                      VK_COMPONENT_SWIZZLE_IDENTITY,
                                      VK_COMPONENT_SWIZZLE_IDENTITY,
                                      VK_COMPONENT_SWIZZLE_IDENTITY,
                                      AspectMask,
                                      0,
                                      1,
                                      0,
                                      1);

  fImage.fImageView:=fImageView;

 except

  FreeAndNil(fImageView);

  FreeAndNil(fImage);

  if assigned(fMemoryBlock) then begin
   fDevice.fMemoryManager.FreeMemoryBlock(fMemoryBlock);
   fMemoryBlock:=nil;
  end;

  raise;

 end;
end;

constructor TVulkanFrameBufferAttachment.Create(const pDevice:TVulkanDevice;
                                                const pImage:TVulkanImage;
                                                const pImageView:TVulkanImageView;
                                                const pWidth:TVkUInt32;
                                                const pHeight:TVkUInt32;
                                                const pFormat:TVkFormat;
                                                const pDoDestroy:boolean=true);
begin

 inherited Create;

 fDevice:=pDevice;

 fWidth:=pWidth;

 fHeight:=pHeight;

 fFormat:=pFormat;

 fImage:=pImage;

 fImageView:=pImageView;

 fMemoryBlock:=nil;

 fDoDestroy:=pDoDestroy;

end;

destructor TVulkanFrameBufferAttachment.Destroy;
begin

 if fDoDestroy then begin

  FreeAndNil(fImageView);

  FreeAndNil(fImage);

  if assigned(fMemoryBlock) then begin
   fDevice.fMemoryManager.FreeMemoryBlock(fMemoryBlock);
   fMemoryBlock:=nil;
  end;

 end else begin

  fImageView:=nil;

  fImage:=nil;

  fMemoryBlock:=nil;

 end;

 inherited Destroy;

end;

constructor TVulkanFrameBuffer.Create(const pDevice:TVulkanDevice;
                                      const pRenderPass:TVulkanRenderPass;
                                      const pWidth:TVkUInt32;
                                      const pHeight:TVkUInt32;
                                      const pLayers:TVkUInt32);
begin

 inherited Create;

 fDevice:=pDevice;

 fFrameBufferHandle:=VK_NULL_HANDLE;

 fFrameBufferAttachments:=nil;

 fFrameBufferAttachmentImageViews:=nil;

 fCountFrameBufferAttachments:=0;

 fRenderPass:=pRenderPass;

 fWidth:=pWidth;

 fHeight:=pHeight;

 fLayers:=pLayers;

 fDoDestroy:=true;

end;

constructor TVulkanFrameBuffer.Create(const pDevice:TVulkanDevice;
                                      const pRenderPass:TVulkanRenderPass;
                                      const pWidth:TVkUInt32;
                                      const pHeight:TVkUInt32;
                                      const pLayers:TVkUInt32;
                                      const pFrameBufferAttachments:array of TVulkanFrameBufferAttachment;
                                      const pDoDestroyAttachments:boolean=true);
begin

 inherited Create;

 fDevice:=pDevice;

 fFrameBufferHandle:=VK_NULL_HANDLE;

 fFrameBufferAttachments:=nil;

 fFrameBufferAttachmentImageViews:=nil;

 fCountFrameBufferAttachments:=length(pFrameBufferAttachments);

 SetLength(fFrameBufferAttachments,fCountFrameBufferAttachments);

 if fCountFrameBufferAttachments>0 then begin
  Move(pFrameBufferAttachments[0],fFrameBufferAttachments[0],fCountFrameBufferAttachments*SizeOf(TVulkanFrameBufferAttachment));
 end;

 fRenderPass:=pRenderPass;

 fWidth:=pWidth;

 fHeight:=pHeight;

 fLayers:=pLayers;

 fDoDestroy:=true;

 fDoDestroyAttachments:=pDoDestroyAttachments;

 Initialize;

end;                                      

constructor TVulkanFrameBuffer.Create(const pDevice:TVulkanDevice;
                                      const pRenderPass:TVulkanRenderPass;
                                      const pWidth:TVkUInt32;
                                      const pHeight:TVkUInt32;
                                      const pLayers:TVkUInt32;
                                      const pFrameBufferHandle:TVkFrameBuffer;
                                      const pFrameBufferAttachments:array of TVulkanFrameBufferAttachment;
                                      const pDoDestroy:boolean=true;
                                      const pDoDestroyAttachments:boolean=true);
begin

 inherited Create;

 fDevice:=pDevice;

 fFrameBufferHandle:=pFrameBufferHandle;

 fFrameBufferAttachments:=nil;

 fFrameBufferAttachmentImageViews:=nil;

 fCountFrameBufferAttachments:=length(pFrameBufferAttachments);

 SetLength(fFrameBufferAttachments,fCountFrameBufferAttachments);

 if fCountFrameBufferAttachments>0 then begin
  Move(pFrameBufferAttachments[0],fFrameBufferAttachments[0],fCountFrameBufferAttachments*SizeOf(TVulkanFrameBufferAttachment));
 end;

 fRenderPass:=pRenderPass;

 fWidth:=pWidth;

 fHeight:=pHeight;

 fLayers:=pLayers;

 fDoDestroy:=pDoDestroy;

 fDoDestroyAttachments:=pDoDestroyAttachments;

end;

destructor TVulkanFrameBuffer.Destroy;
var Index:TVkInt32;
begin

 if fFrameBufferHandle<>VK_NULL_HANDLE then begin
  if fDoDestroy then begin
   fDevice.fDeviceVulkan.DestroyFramebuffer(fDevice.fDeviceHandle,fFrameBufferHandle,fDevice.fAllocationCallbacks);
  end;
  fFrameBufferHandle:=VK_NULL_HANDLE;
 end;

 for Index:=0 to fCountFrameBufferAttachments-1 do begin
  if fDoDestroyAttachments then begin
   FreeAndNil(fFrameBufferAttachments[Index]);
  end else begin
   fFrameBufferAttachments[Index]:=nil;
  end;
 end;

 SetLength(fFrameBufferAttachments,0);

 SetLength(fFrameBufferAttachmentImageViews,0);

 inherited Destroy;
end;

function TVulkanFrameBuffer.GetFrameBufferAttachment(const pIndex:TVkInt32):TVulkanFrameBufferAttachment;
begin
 result:=fFrameBufferAttachments[pIndex];
end;

function TVulkanFrameBuffer.AddAttachment(const pFrameBufferAttachment:TVulkanFrameBufferAttachment):TVkInt32;
begin
 result:=fCountFrameBufferAttachments;
 inc(fCountFrameBufferAttachments);
 if fCountFrameBufferAttachments>length(fFrameBufferAttachments) then begin
  SetLength(fFrameBufferAttachments,fCountFrameBufferAttachments*2);
 end;
 fFrameBufferAttachments[result]:=pFrameBufferAttachment;
end;

procedure TVulkanFrameBuffer.Initialize;
var Index:TVkInt32;
    FrameBufferCreateInfo:TVkFramebufferCreateInfo;
begin
 if fFrameBufferHandle=VK_NULL_HANDLE then begin

  SetLength(fFrameBufferAttachments,fCountFrameBufferAttachments);

  SetLength(fFrameBufferAttachmentImageViews,fCountFrameBufferAttachments);

  for Index:=0 to fCountFrameBufferAttachments-1 do begin
   fFrameBufferAttachmentImageViews[Index]:=fFrameBufferAttachments[Index].fImageView.fImageViewHandle;
  end;

  FillChar(FrameBufferCreateInfo,SizeOf(TVkFramebufferCreateInfo),#0);
  FrameBufferCreateInfo.sType:=VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO;
  FrameBufferCreateInfo.pNext:=nil;
  FrameBufferCreateInfo.flags:=0;
  FrameBufferCreateInfo.renderPass:=fRenderPass.fRenderPassHandle;
  FrameBufferCreateInfo.attachmentCount:=fCountFrameBufferAttachments;
  FrameBufferCreateInfo.pAttachments:=@fFrameBufferAttachmentImageViews[0];
  FrameBufferCreateInfo.width:=fWidth;
  FrameBufferCreateInfo.height:=fHeight;
  FrameBufferCreateInfo.layers:=fLayers;

  HandleResultCode(fDevice.fDeviceVulkan.CreateFramebuffer(fDevice.fDeviceHandle,@FrameBufferCreateInfo,fDevice.fAllocationCallbacks,@fFrameBufferHandle));

 end;
end;

constructor TVulkanSwapChain.Create(const pDevice:TVulkanDevice;
                                    const pOldSwapChain:TVulkanSwapChain=nil;
                                    const pDesiredImageWidth:TVkUInt32=0;
                                    const pDesiredImageHeight:TVkUInt32=0;
                                    const pDesiredImageCount:TVkUInt32=2;
                                    const pImageArrayLayers:TVkUInt32=1;
                                    const pImageFormat:TVkFormat=VK_FORMAT_UNDEFINED;
                                    const pImageColorSpace:TVkColorSpaceKHR=VK_COLOR_SPACE_SRGB_NONLINEAR_KHR;
                                    const pImageUsage:TVkImageUsageFlags=TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT);
                                    const pImageSharingMode:TVkSharingMode=VK_SHARING_MODE_EXCLUSIVE;
                                    const pQueueFamilyIndices:TVkUInt32List=nil;
                                    const pCompositeAlpha:TVkCompositeAlphaFlagBitsKHR=VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
                                    const pPresentMode:TVkPresentModeKHR=VK_PRESENT_MODE_MAILBOX_KHR;
                                    const pClipped:boolean=true;
                                    const pDesiredTransform:TVkSurfaceTransformFlagsKHR=TVkSurfaceTransformFlagsKHR($ffffffff));
var Index:TVkInt32;
    SurfaceCapabilities:TVkSurfaceCapabilitiesKHR;
    SurfacePresetModes:TVkPresentModeKHRArray;
    SurfaceFormat:TVkSurfaceFormatKHR;
    SwapChainImages:array of TVkImage;
    FormatProperties:TVkFormatProperties;
    MemoryRequirements:TVkMemoryRequirements;
    SwapChainCreateInfo:TVkSwapchainCreateInfoKHR;
begin
 inherited Create;

 fDevice:=pDevice;

 fSwapChainHandle:=VK_NULL_HANDLE;

 fQueueFamilyIndices:=nil;

 fImages:=nil;
 
 fCurrentImageIndex:=0;

 fCountImages:=0;

 fWidth:=0;

 fHeight:=0;

 try

  if assigned(pQueueFamilyIndices) then begin
   fCountQueueFamilyIndices:=pQueueFamilyIndices.Count;
   SetLength(fQueueFamilyIndices,fCountQueueFamilyIndices);
   for Index:=0 to fCountQueueFamilyIndices-1 do begin
    fQueueFamilyIndices[Index]:=pQueueFamilyIndices.Items[Index];
   end;
  end else begin
   fCountQueueFamilyIndices:=0;
  end;

  SurfaceCapabilities:=fDevice.fPhysicalDevice.GetSurfaceCapabilities(fDevice.fSurface);

  FillChar(SwapChainCreateInfo,SizeOf(TVkSwapChainCreateInfoKHR),#0);
  SwapChainCreateInfo.sType:=VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;

  SwapChainCreateInfo.surface:=fDevice.fSurface.fSurfaceHandle;

  if SurfaceCapabilities.minImageCount>pDesiredImageCount then begin
   SwapChainCreateInfo.minImageCount:=SurfaceCapabilities.minImageCount;
  end else if (SurfaceCapabilities.maxImageCount<>0) and
              (SurfaceCapabilities.maxImageCount<pDesiredImageCount) then begin
   SwapChainCreateInfo.minImageCount:=SurfaceCapabilities.maxImageCount;
  end else begin
   SwapChainCreateInfo.minImageCount:=pDesiredImageCount;
  end;

  if pImageFormat=VK_FORMAT_UNDEFINED then begin
   SurfaceFormat:=fDevice.fPhysicalDevice.GetSurfaceFormat(fDevice.fSurface);
   SwapChainCreateInfo.imageFormat:=SurfaceFormat.format;
   SwapChainCreateInfo.imageColorSpace:=SurfaceFormat.colorSpace;
  end else begin
   SwapChainCreateInfo.imageFormat:=pImageFormat;
   SwapChainCreateInfo.imageColorSpace:=pImageColorSpace;
  end;

  fImageFormat:=SwapChainCreateInfo.imageFormat;
  fImageColorSpace:=SwapChainCreateInfo.imageColorSpace;
   
  fDevice.fInstance.fVulkan.GetPhysicalDeviceFormatProperties(fDevice.fPhysicalDevice.fPhysicalDeviceHandle,SwapChainCreateInfo.imageFormat,@FormatProperties);
  if (FormatProperties.OptimalTilingFeatures and TVkFormatFeatureFlags(VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT))=0 then begin
   raise EVulkanException.Create('No suitable color image format!');
  end;

  if ((pDesiredImageWidth<>0) and (pDesiredImageHeight<>0)) or
     ((TVkInt32(SurfaceCapabilities.CurrentExtent.Width)<0) or (TVkInt32(SurfaceCapabilities.CurrentExtent.Height)<0)) then begin
   SwapChainCreateInfo.imageExtent.width:=Min(Max(pDesiredImageWidth,SurfaceCapabilities.minImageExtent.width),SurfaceCapabilities.maxImageExtent.width);
   SwapChainCreateInfo.imageExtent.height:=Min(Max(pDesiredImageHeight,SurfaceCapabilities.minImageExtent.height),SurfaceCapabilities.maxImageExtent.height);
  end else begin
   SwapChainCreateInfo.imageExtent:=SurfaceCapabilities.CurrentExtent;
  end;

  fWidth:=SwapChainCreateInfo.imageExtent.width;

  fHeight:=SwapChainCreateInfo.imageExtent.height;

  SwapChainCreateInfo.imageArrayLayers:=pImageArrayLayers;
  SwapChainCreateInfo.imageUsage:=pImageUsage;
  SwapChainCreateInfo.imageSharingMode:=pImageSharingMode;

  if fCountQueueFamilyIndices>0 then begin
   SwapChainCreateInfo.pQueueFamilyIndices:=@fQueueFamilyIndices[0];
   SwapChainCreateInfo.queueFamilyIndexCount:=fCountQueueFamilyIndices;
  end;

  if (pDesiredTransform<>TVkSurfaceTransformFlagsKHR($ffffffff)) and
     ((SurfaceCapabilities.SupportedTransforms and pDesiredTransform)<>0) then begin
   SwapChainCreateInfo.preTransform:=TVkSurfaceTransformFlagBitsKHR(pDesiredTransform);
  end else if (SurfaceCapabilities.SupportedTransforms and TVkSurfaceTransformFlagsKHR(VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR))<>0 then begin
   SwapChainCreateInfo.preTransform:=TVkSurfaceTransformFlagBitsKHR(VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR);
  end else begin
   SwapChainCreateInfo.preTransform:=TVkSurfaceTransformFlagBitsKHR(SurfaceCapabilities.currentTransform);
  end;

  SwapChainCreateInfo.compositeAlpha:=pCompositeAlpha;

  SurfacePresetModes:=nil;
  try
   SurfacePresetModes:=fDevice.fPhysicalDevice.GetSurfacePresentModes(fDevice.fSurface);
   SwapChainCreateInfo.presentMode:=VK_PRESENT_MODE_FIFO_KHR;
   for Index:=0 to length(SurfacePresetModes)-1 do begin
    if SurfacePresetModes[Index]=pPresentMode then begin
     SwapChainCreateInfo.presentMode:=pPresentMode;
     break;
    end;
   end;
  finally                       
   SetLength(SurfacePresetModes,0);
  end;

  if pClipped then begin
   SwapChainCreateInfo.clipped:=VK_TRUE;
  end else begin
   SwapChainCreateInfo.clipped:=VK_FALSE;
  end;

  if assigned(pOldSwapChain) then begin
   SwapChainCreateInfo.oldSwapchain:=pOldSwapChain.fSwapChainHandle;
  end else begin
   SwapChainCreateInfo.oldSwapchain:=VK_NULL_HANDLE;
  end;

  HandleResultCode(fDevice.fDeviceVulkan.CreateSwapChainKHR(fDevice.fDeviceHandle,@SwapChainCreateInfo,fDevice.fAllocationCallbacks,@fSwapChainHandle));

  HandleResultCode(fDevice.fDeviceVulkan.GetSwapchainImagesKHR(fDevice.fDeviceHandle,fSwapChainHandle,@fCountImages,nil));

  SwapChainImages:=nil;
  try
   SetLength(SwapChainImages,fCountImages);

   HandleResultCode(fDevice.fDeviceVulkan.GetSwapchainImagesKHR(fDevice.fDeviceHandle,fSwapChainHandle,@fCountImages,@SwapChainImages[0]));

   SetLength(fImages,fCountImages);
   for Index:=0 to fCountImages-1 do begin
    fImages[Index]:=nil;
   end;

   for Index:=0 to fCountImages-1 do begin
    fImages[Index]:=TVulkanImage.Create(fDevice,SwapChainImages[Index],nil,false);
   end;

  finally
   SetLength(SwapChainImages,0);
  end;

 except

  for Index:=0 to length(fImages)-1 do begin
   FreeAndNil(fImages[Index]);
  end;

  if fSwapChainHandle<>VK_NULL_HANDLE then begin
   fDevice.fDeviceVulkan.DestroySwapChainKHR(fDevice.fDeviceHandle,fSwapChainHandle,fDevice.fAllocationCallbacks);
   fSwapChainHandle:=VK_NULL_HANDLE;
  end;

  SetLength(fQueueFamilyIndices,0);

  SetLength(fImages,0);
  
  raise;

 end;
end;

destructor TVulkanSwapChain.Destroy;
var Index:TVkInt32;
    FrameBufferColorAttachment:TVulkanFrameBufferAttachment;
begin

 for Index:=0 to length(fImages)-1 do begin
  FreeAndNil(fImages[Index]);
 end;

 if fSwapChainHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.DestroySwapChainKHR(fDevice.fDeviceHandle,fSwapChainHandle,fDevice.fAllocationCallbacks);
  fSwapChainHandle:=VK_NULL_HANDLE;
 end;

 SetLength(fQueueFamilyIndices,0);

 SetLength(fImages,0);

 inherited Destroy;
end;

function TVulkanSwapChain.QueuePresent(const pQueue:TVulkanQueue;const pSemaphore:TVulkanSemaphore=nil):TVkResult;
var PresentInfo:TVkPresentInfoKHR;
begin
 FillChar(PresentInfo,SizeOf(TVkPresentInfoKHR),#0);
 PresentInfo.sType:=VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
 PresentInfo.swapchainCount:=1;
 PresentInfo.pSwapchains:=@fSwapChainHandle;
 PresentInfo.pImageIndices:=@fCurrentImageIndex;
 if assigned(pSemaphore) then begin
  PresentInfo.waitSemaphoreCount:=1;
  PresentInfo.pWaitSemaphores:=@pSemaphore.fSemaphoreHandle;
 end;
 result:=fDevice.fInstance.fInstanceVulkan.QueuePresentKHR(pQueue.fQueueHandle,@PresentInfo);
 if result<VK_SUCCESS then begin
  HandleResultCode(result);
 end;
end;

function TVulkanSwapChain.AcquireNextImage(const pSemaphore:TVulkanSemaphore=nil;const pFence:TVulkanFence=nil;const pTimeOut:TVkUInt64=TVkUInt64(high(TVkUInt64))):TVkResult;
var SemaphoreHandle:TVkFence;
    FenceHandle:TVkFence;
begin
 if assigned(pSemaphore) then begin
  SemaphoreHandle:=pSemaphore.fSemaphoreHandle;
 end else begin
  SemaphoreHandle:=VK_NULL_HANDLE;
 end;
 if assigned(pFence) then begin
  FenceHandle:=pFence.fFenceHandle;
 end else begin
  FenceHandle:=VK_NULL_HANDLE;
 end;
 if assigned(pFence) then begin
  FenceHandle:=pFence.fFenceHandle;
 end else begin
  FenceHandle:=VK_NULL_HANDLE;
 end;
 result:=fDevice.fDeviceVulkan.AcquireNextImageKHR(fDevice.fDeviceHandle,fSwapChainHandle,pTimeOut,SemaphoreHandle,FenceHandle,@fCurrentImageIndex);
 if result<VK_SUCCESS then begin
  HandleResultCode(result);
 end;
end;

function TVulkanSwapChain.GetImage(const pImageIndex:TVkInt32):TVulkanImage;
begin
 result:=fImages[pImageIndex];
end;

function TVulkanSwapChain.GetCurrentImage:TVulkanImage;
begin
 result:=fImages[fCurrentImageIndex];
end;

constructor TVulkanSwapChainSimpleDirectRenderTarget.Create(const pDevice:TVulkanDevice;
                                                            const pSwapChain:TVulkanSwapChain;
                                                            const pCommandBuffer:TVulkanCommandBuffer;
                                                            const pCommandBufferFence:TVulkanFence;
                                                            const pDepthImageFormat:TVkFormat=VK_FORMAT_UNDEFINED;
                                                            const pDepthImageFormatWithStencil:boolean=false);
var Index:TVkInt32;
    FrameBufferColorAttachment:TVulkanFrameBufferAttachment;
    Attachments:array[0..1] of TVkImageView;
    FormatProperties:TVkFormatProperties;
    MemoryRequirements:TVkMemoryRequirements;
    ColorAttachmentImage:TVulkanImage;
    ColorAttachmentImageView:TVulkanImageView;
begin

 inherited Create;

 fDevice:=pDevice;

 fSwapChain:=pSwapChain;

 fFrameBufferColorAttachments:=nil;

 fFrameBuffers:=nil;

 fDepthFrameBufferAttachment:=nil;

 fRenderPass:=nil;

 try

  if fDepthImageFormat=VK_FORMAT_UNDEFINED then begin
   fDepthImageFormat:=fDevice.fPhysicalDevice.GetBestSupportedDepthFormat(pDepthImageFormatWithStencil);
  end else begin
   fDepthImageFormat:=pDepthImageFormat;
  end;

  fDevice.fInstance.fVulkan.GetPhysicalDeviceFormatProperties(fDevice.fPhysicalDevice.fPhysicalDeviceHandle,fDepthImageFormat,@FormatProperties);
  if (FormatProperties.OptimalTilingFeatures and TVkFormatFeatureFlags(VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT))=0 then begin
   raise EVulkanException.Create('No suitable depth image format!');
  end;

  begin

   fRenderPass:=TVulkanRenderPass.Create(fDevice);

   fRenderPass.AddSubpassDescription(0,
                                     VK_PIPELINE_BIND_POINT_GRAPHICS,
                                     [],
                                     [fRenderPass.AddAttachmentReference(fRenderPass.AddAttachmentDescription(0,
                                                                                                              fSwapChain.ImageFormat,
                                                                                                              VK_SAMPLE_COUNT_1_BIT,
                                                                                                              VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                              VK_ATTACHMENT_STORE_OP_STORE,
                                                                                                              VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                              VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                              VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, //VK_IMAGE_LAYOUT_UNDEFINED, // VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                                                                                              VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL //VK_IMAGE_LAYOUT_PRESENT_SRC_KHR  // VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                                                             ),
                                                                         VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                        )],
                                     [],
                                     fRenderPass.AddAttachmentReference(fRenderPass.AddAttachmentDescription(0,
                                                                                                             fDepthImageFormat,
                                                                                                             VK_SAMPLE_COUNT_1_BIT,
                                                                                                             VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                             VK_ATTACHMENT_STORE_OP_STORE,
                                                                                                             VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                             VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                             VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, // VK_IMAGE_LAYOUT_UNDEFINED, // VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                                                                                             VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                                            ),
                                                                        VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                       ),
                                     []);

   fRenderPass.Initialize;

  end;

  SetLength(fFrameBufferColorAttachments,fSwapChain.CountImages);

  for Index:=0 to fSwapChain.CountImages-1 do begin
   fFrameBufferColorAttachments[Index]:=nil;
  end;

  for Index:=0 to fSwapChain.CountImages-1 do begin

   ColorAttachmentImage:=nil;

   ColorAttachmentImageView:=nil;

   try
    ColorAttachmentImage:=TVulkanImage.Create(fDevice,fSwapChain.Images[Index].fImageHandle,nil,false);

    ColorAttachmentImage.SetLayout(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                   VK_IMAGE_LAYOUT_UNDEFINED,
                                   VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
                                   nil,
                                   pCommandBuffer,
                                   fDevice.fPresentQueue,
                                   pCommandBufferFence,
                                   true);

    ColorAttachmentImageView:=TVulkanImageView.Create(Device,
                                                      ColorAttachmentImage,
                                                      VK_IMAGE_VIEW_TYPE_2D,
                                                      fSwapChain.ImageFormat,
                                                      VK_COMPONENT_SWIZZLE_IDENTITY,
                                                      VK_COMPONENT_SWIZZLE_IDENTITY,
                                                      VK_COMPONENT_SWIZZLE_IDENTITY,
                                                      VK_COMPONENT_SWIZZLE_IDENTITY,
                                                      TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                      0,
                                                      1,
                                                      0,
                                                      1);

    ColorAttachmentImage.fImageView:=ColorAttachmentImageView;
    ColorAttachmentImageView.fImage:=ColorAttachmentImage;

    fFrameBufferColorAttachments[Index]:=TVulkanFrameBufferAttachment.Create(fDevice,
                                                                             ColorAttachmentImage,
                                                                             ColorAttachmentImageView,
                                                                             fSwapChain.Width,
                                                                             fSwapChain.Height,
                                                                             fSwapChain.ImageFormat,
                                                                             true);

   except
    FreeAndNil(fFrameBufferColorAttachments[Index]);
    FreeAndNil(ColorAttachmentImageView);
    FreeAndNil(ColorAttachmentImage);
    raise;
   end;

  end;

  fDepthFrameBufferAttachment:=TVulkanFrameBufferAttachment.Create(fDevice,
                                                                   pCommandBuffer,
                                                                   pCommandBufferFence,
                                                                   fSwapChain.Width,
                                                                   fSwapChain.Height,
                                                                   fDepthImageFormat,
                                                                   TVkBufferUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT));

  SetLength(fFrameBuffers,fSwapChain.CountImages);
  for Index:=0 to fSwapChain.CountImages-1 do begin
   fFrameBuffers[Index]:=nil;
  end;
  for Index:=0 to fSwapChain.CountImages-1 do begin
   fFrameBuffers[Index]:=TVulkanFrameBuffer.Create(fDevice,
                                                   fRenderPass,
                                                   fSwapChain.Width,
                                                   fSwapChain.Height,
                                                   1,
                                                   [fFrameBufferColorAttachments[Index],fDepthFrameBufferAttachment],
                                                   false);
  end;

 except

  for Index:=0 to length(fFramebuffers)-1 do begin
   FreeAndNil(fFrameBuffers[Index]);
  end;

  FreeAndNil(fRenderPass);

  FreeAndNil(fDepthFrameBufferAttachment);

  for Index:=0 to length(fFrameBufferColorAttachments)-1 do begin
   FreeAndNil(fFrameBufferColorAttachments[Index]);
  end;

  SetLength(fFrameBufferColorAttachments,0);

  SetLength(fFrameBuffers,0);

  raise;

 end;

end;

destructor TVulkanSwapChainSimpleDirectRenderTarget.Destroy;
var Index:TVkInt32;
    FrameBufferColorAttachment:TVulkanFrameBufferAttachment;
begin

 for Index:=0 to length(fFramebuffers)-1 do begin
   FreeAndNil(fFrameBuffers[Index]);
 end;

 FreeAndNil(fRenderPass);

 FreeAndNil(fDepthFrameBufferAttachment);

 for Index:=0 to length(fFrameBufferColorAttachments)-1 do begin
  FreeAndNil(fFrameBufferColorAttachments[Index]);
 end;

 SetLength(fFrameBufferColorAttachments,0);
 SetLength(fFrameBuffers,0);

 inherited Destroy;
end;

function TVulkanSwapChainSimpleDirectRenderTarget.GetRenderPass:TVulkanRenderPass;
begin
 result:=fRenderPass;
end;

function TVulkanSwapChainSimpleDirectRenderTarget.GetFrameBuffer:TVulkanFrameBuffer;
begin
 result:=fFrameBuffers[fSwapChain.CurrentImageIndex];
end;

constructor TVulkanShaderModule.Create(const pDevice:TVulkanDevice;const pData;const pDataSize:TVkSize);
begin

 inherited Create;

 fDevice:=pDevice;

 fShaderModuleHandle:=VK_NULL_HANDLE;

 fData:=nil;

 fDataAligned:=nil;

 fDataSize:=pDataSize;
 if (fDataSize and 3)<>0 then begin
  inc(fDataSize,4-(fDataSize and 3));
 end;

 GetMem(fData,fDataSize+4);
 fDataAligned:=fData;
 if (TVkPtrUInt(fDataAligned) and 3)<>0 then begin
  inc(TVkPtrUInt(fDataAligned),4-(TVkPtrUInt(fDataAligned) and 3));
 end;

 Load;

end;

constructor TVulkanShaderModule.Create(const pDevice:TVulkanDevice;const pStream:TStream);
begin

 inherited Create;

 fDevice:=pDevice;

 fShaderModuleHandle:=VK_NULL_HANDLE;

 fData:=nil;

 fDataAligned:=nil;

 fDataSize:=pStream.Size;
 if (fDataSize and 3)<>0 then begin
  inc(fDataSize,4-(fDataSize and 3));
 end;

 GetMem(fData,fDataSize+4);
 fDataAligned:=fData;
 if (TVkPtrUInt(fDataAligned) and 3)<>0 then begin
  inc(TVkPtrUInt(fDataAligned),4-(TVkPtrUInt(fDataAligned) and 3));
 end;

 if pStream.Seek(0,soBeginning)<>0 then begin
  raise EInOutError.Create('Stream seek error');
 end;

 if pStream.Read(fData^,pStream.Size)<>pStream.Size then begin
  raise EInOutError.Create('Stream read error');
 end;

 Load;

end;

constructor TVulkanShaderModule.Create(const pDevice:TVulkanDevice;const pFileName:string);
var FileStream:TFileStream;
begin
 FileStream:=TFileStream.Create(pFileName,fmOpenRead or fmShareDenyWrite);
 try
  Create(pDevice,FileStream);
 finally
  FileStream.Free;
 end;
end;

destructor TVulkanShaderModule.Destroy;
begin
 if fShaderModuleHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.DestroyShaderModule(fDevice.fDeviceHandle,fShaderModuleHandle,fDevice.fAllocationCallbacks);
  fShaderModuleHandle:=VK_NULL_HANDLE;
 end;
 if assigned(fData) then begin
  FreeMem(fData);
  fData:=nil;
 end;
 inherited Destroy;
end;

procedure TVulkanShaderModule.Load;
var ShaderModuleCreateInfo:TVkShaderModuleCreateInfo;
begin
 if fShaderModuleHandle=VK_NULL_HANDLE then begin
  FillChar(ShaderModuleCreateInfo,SizeOf(TVkShaderModuleCreateInfo),#0);
  ShaderModuleCreateInfo.sType:=VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
  ShaderModuleCreateInfo.codeSize:=fDataSize;
  ShaderModuleCreateInfo.pCode:=fData;
  HandleResultCode(fDevice.fDeviceVulkan.CreateShaderModule(fDevice.fDeviceHandle,@ShaderModuleCreateInfo,fDevice.fAllocationCallbacks,@fShaderModuleHandle));
 end;
end;

constructor TVulkanDescriptorPool.Create(const pDevice:TVulkanDevice;
                                         const pFlags:TVkDescriptorPoolCreateFlags;
                                         const pMaxSets:TVkUInt32);
begin
 inherited Create;

 fDevice:=pDevice;

 fDescriptorPoolHandle:=VK_NULL_HANDLE;

 fFlags:=pFlags;
 fMaxSets:=pMaxSets;

 fDescriptorPoolSizes:=nil;
 fCountDescriptorPoolSizes:=0;

end;

destructor TVulkanDescriptorPool.Destroy;
begin
 if fDescriptorPoolHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.DestroyDescriptorPool(fDevice.fDeviceHandle,fDescriptorPoolHandle,fDevice.fAllocationCallbacks);
  fDescriptorPoolHandle:=VK_NULL_HANDLE;
 end;
 SetLength(fDescriptorPoolSizes,0);
 inherited Destroy;
end;

function TVulkanDescriptorPool.AddDescriptorPoolSize(const pType:TVkDescriptorType;const pDescriptorCount:TVkUInt32):TVkInt32;
var DescriptorPoolSize:PVkDescriptorPoolSize;
begin
 result:=fCountDescriptorPoolSizes;
 inc(fCountDescriptorPoolSizes);
 if fCountDescriptorPoolSizes>length(fDescriptorPoolSizes) then begin
  SetLength(fDescriptorPoolSizes,fCountDescriptorPoolSizes*2);
 end;
 DescriptorPoolSize:=@fDescriptorPoolSizes[result];
 DescriptorPoolSize.type_:=pType;
 DescriptorPoolSize.descriptorCount:=pDescriptorCount;
end;

procedure TVulkanDescriptorPool.Initialize;
var DescriptorPoolCreateInfo:TVkDescriptorPoolCreateInfo;
begin
 if fDescriptorPoolHandle=VK_NULL_HANDLE then begin
  FillChar(DescriptorPoolCreateInfo,SizeOf(TVkDescriptorPoolCreateInfo),#0);
  DescriptorPoolCreateInfo.sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO;
  DescriptorPoolCreateInfo.flags:=fFlags;
  DescriptorPoolCreateInfo.maxSets:=fMaxSets;
  if fCountDescriptorPoolSizes>0 then begin
   SetLength(fDescriptorPoolSizes,fCountDescriptorPoolSizes);
   DescriptorPoolCreateInfo.poolSizeCount:=length(fDescriptorPoolSizes);
   DescriptorPoolCreateInfo.pPoolSizes:=@fDescriptorPoolSizes[0];
  end;
  HandleResultCode(fDevice.fDeviceVulkan.CreateDescriptorPool(fDevice.fDeviceHandle,@DescriptorPoolCreateInfo,fDevice.fAllocationCallbacks,@fDescriptorPoolHandle));
 end;
end;

constructor TVulkanDescriptorSetLayoutBinding.Create(const pBinding:TVkUInt32;
                                                     const pDescriptorType:TVkDescriptorType;
                                                     const pDescriptorCount:TVkUInt32;
                                                     const pStageFlags:TVkShaderStageFlags);
begin
 inherited Create;

 FillChar(fDescriptorSetLayoutBinding,SizeOf(TVkDescriptorSetLayoutBinding),#0);
 fDescriptorSetLayoutBinding.binding:=pBinding;
 fDescriptorSetLayoutBinding.descriptorType:=pDescriptorType;
 fDescriptorSetLayoutBinding.descriptorCount:=pDescriptorCount;
 fDescriptorSetLayoutBinding.stageFlags:=pStageFlags;

 fImmutableSamplers:=nil;
 fCountImmutableSamplers:=0;

end;

destructor TVulkanDescriptorSetLayoutBinding.Destroy;
begin
 SetLength(fImmutableSamplers,0);
 inherited Destroy;
end;

function TVulkanDescriptorSetLayoutBinding.GetBinding:TVkUInt32;
begin
 result:=fDescriptorSetLayoutBinding.binding;
end;

procedure TVulkanDescriptorSetLayoutBinding.SetBinding(const pBinding:TVkUInt32);
begin
 fDescriptorSetLayoutBinding.binding:=pBinding;
end;

function TVulkanDescriptorSetLayoutBinding.GetDescriptorType:TVkDescriptorType;
begin
 result:=fDescriptorSetLayoutBinding.descriptorType;
end;

procedure TVulkanDescriptorSetLayoutBinding.SetDescriptorType(const pDescriptorType:TVkDescriptorType);
begin
 fDescriptorSetLayoutBinding.descriptorType:=pDescriptorType;
end;

function TVulkanDescriptorSetLayoutBinding.GetDescriptorCount:TVkUInt32;
begin
 result:=fDescriptorSetLayoutBinding.DescriptorCount;
end;

procedure TVulkanDescriptorSetLayoutBinding.SetDescriptorCount(const pDescriptorCount:TVkUInt32);
begin
 fDescriptorSetLayoutBinding.descriptorCount:=pDescriptorCount;
end;

function TVulkanDescriptorSetLayoutBinding.GetStageFlags:TVkShaderStageFlags;
begin
 result:=fDescriptorSetLayoutBinding.stageFlags;
end;

procedure TVulkanDescriptorSetLayoutBinding.SetStageFlags(const pStageFlags:TVkShaderStageFlags);
begin
 fDescriptorSetLayoutBinding.stageFlags:=pStageFlags;
end;

procedure TVulkanDescriptorSetLayoutBinding.AddImmutableSampler(const pImmutableSampler:TVulkanSampler);
var Index:TVkInt32;
begin
 Index:=fCountImmutableSamplers;
 inc(fCountImmutableSamplers);
 if fCountImmutableSamplers>length(fImmutableSamplers) then begin
  SetLength(fImmutableSamplers,fCountImmutableSamplers*2);
 end;
 fImmutableSamplers[Index]:=pImmutableSampler.fSamplerHandle;
end;

procedure TVulkanDescriptorSetLayoutBinding.AddImmutableSamplers(const pImmutableSamplers:array of TVulkanSampler);
var Index:TVkInt32;
begin
 for Index:=0 to length(pImmutableSamplers)-1 do begin
  AddImmutableSampler(pImmutableSamplers[Index]);
 end;
end;

procedure TVulkanDescriptorSetLayoutBinding.Initialize;
begin
 SetLength(fImmutableSamplers,fCountImmutableSamplers);
 fDescriptorSetLayoutBinding.pImmutableSamplers:=@fImmutableSamplers[0];
end;

constructor TVulkanDescriptorSetLayout.Create(const pDevice:TVulkanDevice);
begin
 inherited Create;

 fDevice:=pDevice;

 fDescriptorSetLayoutHandle:=VK_NULL_HANDLE;

 fDescriptorSetLayoutBindingList:=TVulkanObjectList.Create;
 fDescriptorSetLayoutBindingList.OwnObjects:=true;

 fDescriptorSetLayoutBindingArray:=nil;

end;

destructor TVulkanDescriptorSetLayout.Destroy;
begin
 if fDescriptorSetLayoutHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.DestroyDescriptorSetLayout(fDevice.fDeviceHandle,fDescriptorSetLayoutHandle,fDevice.fAllocationCallbacks);
  fDescriptorSetLayoutHandle:=VK_NULL_HANDLE;
 end;
 FreeAndNil(fDescriptorSetLayoutBindingList);
 SetLength(fDescriptorSetLayoutBindingArray,0);
 inherited Destroy;
end;

procedure TVulkanDescriptorSetLayout.AddBinding(const pBinding:TVkUInt32;
                                                const pDescriptorType:TVkDescriptorType;
                                                const pDescriptorCount:TVkUInt32;
                                                const pStageFlags:TVkShaderStageFlags;
                                                const pImmutableSamplers:array of TVulkanSampler);
var DescriptorSetLayoutBinding:TVulkanDescriptorSetLayoutBinding;
begin
 DescriptorSetLayoutBinding:=TVulkanDescriptorSetLayoutBinding.Create(pBinding,pDescriptorType,pDescriptorCount,pStageFlags);
 fDescriptorSetLayoutBindingList.Add(DescriptorSetLayoutBinding);
 DescriptorSetLayoutBinding.AddImmutableSamplers(pImmutableSamplers);
 DescriptorSetLayoutBinding.Initialize;
end;

procedure TVulkanDescriptorSetLayout.Initialize;
var Index:TVkInt32;
    DescriptorSetLayoutCreateInfo:TVkDescriptorSetLayoutCreateInfo;
begin
 if fDescriptorSetLayoutHandle=VK_NULL_HANDLE then begin
  FillChar(DescriptorSetLayoutCreateInfo,SizeOf(TVkDescriptorSetLayoutCreateInfo),#0);
  DescriptorSetLayoutCreateInfo.sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
  SetLength(fDescriptorSetLayoutBindingArray,fDescriptorSetLayoutBindingList.Count);
  if length(fDescriptorSetLayoutBindingArray)>0 then begin
   for Index:=0 to length(fDescriptorSetLayoutBindingArray)-1 do begin
    fDescriptorSetLayoutBindingArray[Index]:=TVulkanDescriptorSetLayoutBinding(fDescriptorSetLayoutBindingList[Index]).fDescriptorSetLayoutBinding;
   end;
   DescriptorSetLayoutCreateInfo.bindingCount:=length(fDescriptorSetLayoutBindingArray);
   DescriptorSetLayoutCreateInfo.pBindings:=@fDescriptorSetLayoutBindingArray[0];
  end;
  HandleResultCode(fDevice.fDeviceVulkan.CreateDescriptorSetLayout(fDevice.fDeviceHandle,@DescriptorSetLayoutCreateInfo,fDevice.fAllocationCallbacks,@fDescriptorSetLayoutHandle));
 end;
end;

constructor TVulkanDescriptorSet.Create(const pDescriptorPool:TVulkanDescriptorPool;
                                        const pDescriptorSetLayout:TVulkanDescriptorSetLayout);
begin
 inherited Create;

 fDevice:=pDescriptorPool.fDevice;

 fDescriptorPool:=pDescriptorPool;

 fDescriptorSetLayout:=pDescriptorSetLayout;

 fDescriptorSetHandle:=VK_NULL_HANDLE;

 fCopyDescriptorSetQueue:=nil;
 fCopyDescriptorSetQueueSize:=0;

 fWriteDescriptorSetQueue:=nil;
 fWriteDescriptorSetQueueMetaData:=nil;
 fWriteDescriptorSetQueueSize:=0;

 FillChar(fDescriptorSetAllocateInfo,SizeOf(TVkDescriptorSetAllocateInfo),#0);
 fDescriptorSetAllocateInfo.sType:=VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
 fDescriptorSetAllocateInfo.descriptorPool:=fDescriptorPool.fDescriptorPoolHandle;
 fDescriptorSetAllocateInfo.descriptorSetCount:=1;
 fDescriptorSetAllocateInfo.pSetLayouts:=@fDescriptorSetLayout.fDescriptorSetLayoutHandle;

 fDevice.fDeviceVulkan.AllocateDescriptorSets(fDevice.fDeviceHandle,@fDescriptorSetAllocateInfo,@fDescriptorSetHandle);

end;

destructor TVulkanDescriptorSet.Destroy;
begin
 if fDescriptorSetHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.FreeDescriptorSets(fDevice.fDeviceHandle,fDescriptorPool.fDescriptorPoolHandle,1,@fDescriptorSetHandle);
  fDescriptorSetHandle:=VK_NULL_HANDLE;
 end;
 SetLength(fCopyDescriptorSetQueue,0);
 SetLength(fWriteDescriptorSetQueue,0);
 SetLength(fWriteDescriptorSetQueueMetaData,0);
 inherited Destroy;
end;

class function TVulkanDescriptorSet.Allocate(const pDescriptorPool:TVulkanDescriptorPool;
                                             const pDescriptorSetLayouts:array of TVulkanDescriptorSetLayout):TVulkanObjectList;

var Index:TVkInt32;
begin
 result:=TVulkanObjectList.Create;
 try
  for Index:=0 to length(pDescriptorSetLayouts)-1 do begin
   result.Add(TVulkanDescriptorSet.Create(pDescriptorPool,pDescriptorSetLayouts[Index]));
  end;
 except
  FreeAndNil(result);
  raise;
 end;
end;

procedure TVulkanDescriptorSet.CopyFromDescriptorSet(const pSourceDescriptorSet:TVulkanDescriptorSet;
                                                     const pSourceBinding:TVkUInt32;
                                                     const pSourceArrayElement:TVkUInt32;
                                                     const pDestinationBinding:TVkUInt32;
                                                     const pDestinationArrayElement:TVkUInt32;
                                                     const pDescriptorCount:TVkUInt32;
                                                     const pDoInstant:boolean=false);
 procedure InstantCopyFromDescriptorSet;
 var CopyDescriptorSet:TVkCopyDescriptorSet;
 begin
  FillChar(CopyDescriptorSet,SizeOf(TVkCopyDescriptorSet),#0);
  CopyDescriptorSet.sType:=VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET;
  CopyDescriptorSet.srcSet:=pSourceDescriptorSet.Handle;
  CopyDescriptorSet.srcBinding:=pSourceBinding;
  CopyDescriptorSet.srcArrayElement:=pSourceArrayElement;
  CopyDescriptorSet.dstBinding:=pDestinationBinding;
  CopyDescriptorSet.dstArrayElement:=pDestinationArrayElement;
  CopyDescriptorSet.descriptorCount:=pDescriptorCount;
  fDevice.fDeviceVulkan.UpdateDescriptorSets(fDevice.fDeviceHandle,0,nil,1,@CopyDescriptorSet);
 end;
var Index:TVkInt32;
    CopyDescriptorSet:PVkCopyDescriptorSet;
begin
 if pDoInstant then begin
  InstantCopyFromDescriptorSet; 
 end else begin
  Index:=fCopyDescriptorSetQueueSize;
  inc(fCopyDescriptorSetQueueSize);
  if length(fCopyDescriptorSetQueue)<fCopyDescriptorSetQueueSize then begin
   SetLength(fCopyDescriptorSetQueue,fCopyDescriptorSetQueueSize*2);
  end;
  CopyDescriptorSet:=@fCopyDescriptorSetQueue[Index];
  FillChar(CopyDescriptorSet^,SizeOf(TVkCopyDescriptorSet),#0);
  CopyDescriptorSet^.sType:=VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET;
  CopyDescriptorSet^.srcSet:=pSourceDescriptorSet.Handle;
  CopyDescriptorSet^.srcBinding:=pSourceBinding;
  CopyDescriptorSet^.srcArrayElement:=pSourceArrayElement;
  CopyDescriptorSet^.dstBinding:=pDestinationBinding;
  CopyDescriptorSet^.dstArrayElement:=pDestinationArrayElement;
  CopyDescriptorSet^.descriptorCount:=pDescriptorCount;
 end;
end;

procedure TVulkanDescriptorSet.WriteToDescriptorSet(const pDestinationBinding:TVkUInt32;
                                                    const pDestinationArrayElement:TVkUInt32;
                                                    const pDescriptorCount:TVkUInt32;
                                                    const pDescriptorType:TVkDescriptorType;
                                                    const pImageInfo:array of TVkDescriptorImageInfo;
                                                    const pBufferInfo:array of TVkDescriptorBufferInfo;
                                                    const pTexelBufferView:array of TVkBufferView;
                                                    const pDoInstant:boolean=false);
 procedure InstantWriteToDescriptorSet;
 var WriteDescriptorSet:TVkWriteDescriptorSet;
 begin
  FillChar(WriteDescriptorSet,SizeOf(TVkWriteDescriptorSet),#0);
  WriteDescriptorSet.sType:=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
  WriteDescriptorSet.dstBinding:=pDestinationBinding;
  WriteDescriptorSet.dstArrayElement:=pDestinationArrayElement;
  WriteDescriptorSet.descriptorCount:=pDescriptorCount;
  if length(pImageInfo)>0 then begin
   WriteDescriptorSet.pImageInfo:=@pImageInfo[0];
  end else begin
   WriteDescriptorSet.pImageInfo:=nil;
  end;
  if length(pBufferInfo)>0 then begin
   WriteDescriptorSet.pBufferInfo:=@pBufferInfo[0];
  end else begin
   WriteDescriptorSet.pBufferInfo:=nil;
  end;
  if length(pTexelBufferView)>0 then begin
   WriteDescriptorSet.pTexelBufferView:=@pTexelBufferView[0];
  end else begin
   WriteDescriptorSet.pTexelBufferView:=nil;
  end;
  fDevice.fDeviceVulkan.UpdateDescriptorSets(fDevice.fDeviceHandle,1,@WriteDescriptorSet,0,nil);
 end;
var Index:TVkInt32;
    WriteDescriptorSet:PVkWriteDescriptorSet;
    WriteDescriptorSetMetaData:PVulkanDescriptorSetWriteDescriptorSetMetaData;
begin
 if pDoInstant then begin
  InstantWriteToDescriptorSet;
 end else begin
  Index:=fWriteDescriptorSetQueueSize;
  inc(fWriteDescriptorSetQueueSize);
  if length(fWriteDescriptorSetQueue)<fWriteDescriptorSetQueueSize then begin
   SetLength(fWriteDescriptorSetQueue,fWriteDescriptorSetQueueSize*2);
  end;
  if length(fWriteDescriptorSetQueueMetaData)<fWriteDescriptorSetQueueSize then begin
   SetLength(fWriteDescriptorSetQueueMetaData,fWriteDescriptorSetQueueSize*2);
  end;
  WriteDescriptorSet:=@fWriteDescriptorSetQueue[Index];
  WriteDescriptorSetMetaData:=@fWriteDescriptorSetQueueMetaData[Index];
  FillChar(WriteDescriptorSet^,SizeOf(TVkWriteDescriptorSet),#0);
  WriteDescriptorSet^.sType:=VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
  WriteDescriptorSet^.dstBinding:=pDestinationBinding;
  WriteDescriptorSet^.dstArrayElement:=pDestinationArrayElement;
  WriteDescriptorSet^.descriptorCount:=pDescriptorCount;
  WriteDescriptorSet^.pImageInfo:=nil;
  WriteDescriptorSet^.pBufferInfo:=nil;
  WriteDescriptorSet^.pTexelBufferView:=nil;
  WriteDescriptorSetMetaData^.ImageInfo:=nil;
  WriteDescriptorSetMetaData^.BufferInfo:=nil;
  WriteDescriptorSetMetaData^.TexelBufferView:=nil;
  if length(pImageInfo)>0 then begin
   SetLength(WriteDescriptorSetMetaData^.ImageInfo,length(pImageInfo));
   Move(pImageInfo[0],WriteDescriptorSetMetaData^.ImageInfo[0],length(pImageInfo)*SizeOf(TVkDescriptorImageInfo));
  end;
  if length(pBufferInfo)>0 then begin
   SetLength(WriteDescriptorSetMetaData^.BufferInfo,length(pBufferInfo));
   Move(pBufferInfo[0],WriteDescriptorSetMetaData^.BufferInfo[0],length(pBufferInfo)*SizeOf(TVkDescriptorBufferInfo));
  end;
  if length(pTexelBufferView)>0 then begin
   SetLength(WriteDescriptorSetMetaData^.TexelBufferView,length(pTexelBufferView));
   Move(pTexelBufferView[0],WriteDescriptorSetMetaData^.TexelBufferView[0],length(pTexelBufferView)*SizeOf(TVkBufferView));
  end;
 end;
end;

procedure TVulkanDescriptorSet.Flush;
var Index:TVkInt32;
    WriteDescriptorSet:PVkWriteDescriptorSet;
    WriteDescriptorSetMetaData:PVulkanDescriptorSetWriteDescriptorSetMetaData;
begin
 if fWriteDescriptorSetQueueSize>0 then begin
  for Index:=0 to fWriteDescriptorSetQueueSize-1 do begin
   WriteDescriptorSet:=@fWriteDescriptorSetQueue[Index];
   WriteDescriptorSetMetaData:=@fWriteDescriptorSetQueueMetaData[Index];
   if length(WriteDescriptorSetMetaData^.ImageInfo)>0 then begin
    WriteDescriptorSet^.pImageInfo:=@WriteDescriptorSetMetaData^.ImageInfo[0];
   end else begin
    WriteDescriptorSet^.pImageInfo:=nil;
   end;
   if length(WriteDescriptorSetMetaData^.BufferInfo)>0 then begin
    WriteDescriptorSet^.pBufferInfo:=@WriteDescriptorSetMetaData^.BufferInfo[0];
   end else begin
    WriteDescriptorSet^.pBufferInfo:=nil;
   end;
   if length(WriteDescriptorSetMetaData^.TexelBufferView)>0 then begin
    WriteDescriptorSet^.pTexelBufferView:=@WriteDescriptorSetMetaData^.TexelBufferView[0];
   end else begin
    WriteDescriptorSet^.pTexelBufferView:=nil;
   end;
  end;
  if fCopyDescriptorSetQueueSize>0 then begin
   fDevice.fDeviceVulkan.UpdateDescriptorSets(fDevice.fDeviceHandle,fWriteDescriptorSetQueueSize,@fWriteDescriptorSetQueue[0],fCopyDescriptorSetQueueSize,@fCopyDescriptorSetQueue[0]);
  end else begin
   fDevice.fDeviceVulkan.UpdateDescriptorSets(fDevice.fDeviceHandle,fWriteDescriptorSetQueueSize,@fWriteDescriptorSetQueue[0],0,nil);
  end;
 end else if fCopyDescriptorSetQueueSize>0 then begin
  fDevice.fDeviceVulkan.UpdateDescriptorSets(fDevice.fDeviceHandle,0,nil,fCopyDescriptorSetQueueSize,@fCopyDescriptorSetQueue[0]);
 end;
 fCopyDescriptorSetQueueSize:=0;
 fWriteDescriptorSetQueueSize:=0;
end;

constructor TVulkanPipelineLayout.Create(const pDevice:TVulkanDevice);
begin

 inherited Create;

 fDevice:=pDevice;

 fPipelineLayoutHandle:=VK_NULL_HANDLE;

 fDescriptorSetLayouts:=nil;
 fCountDescriptorSetLayouts:=0;

 fPushConstantRanges:=nil;
 fCountPushConstantRanges:=0;

end;

destructor TVulkanPipelineLayout.Destroy;
begin
 if fPipelineLayoutHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.DestroyPipelineLayout(fDevice.fDeviceHandle,fPipelineLayoutHandle,fDevice.fAllocationCallbacks);
  fPipelineLayoutHandle:=VK_NULL_HANDLE;
 end;
 SetLength(fDescriptorSetLayouts,0);
 SetLength(fPushConstantRanges,0);
 inherited Destroy;
end;

function TVulkanPipelineLayout.AddDescriptorSetLayout(const pDescriptorSetLayout:TVkDescriptorSetLayout):TVkInt32;
begin
 result:=fCountDescriptorSetLayouts;
 inc(fCountDescriptorSetLayouts);
 if fCountDescriptorSetLayouts>length(fDescriptorSetLayouts) then begin
  SetLength(fDescriptorSetLayouts,fCountDescriptorSetLayouts*2);
 end;
 fDescriptorSetLayouts[result]:=pDescriptorSetLayout;
end;

function TVulkanPipelineLayout.AddDescriptorSetLayout(const pDescriptorSetLayout:TVulkanDescriptorSetLayout):TVkInt32;
begin
 result:=fCountDescriptorSetLayouts;
 inc(fCountDescriptorSetLayouts);
 if fCountDescriptorSetLayouts>length(fDescriptorSetLayouts) then begin
  SetLength(fDescriptorSetLayouts,fCountDescriptorSetLayouts*2);
 end;
 fDescriptorSetLayouts[result]:=pDescriptorSetLayout.fDescriptorSetLayoutHandle;
end;

function TVulkanPipelineLayout.AddDescriptorSetLayouts(const pDescriptorSetLayouts:array of TVkDescriptorSetLayout):TVkInt32;
begin
 if length(pDescriptorSetLayouts)>0 then begin
  result:=fCountDescriptorSetLayouts;
  inc(fCountDescriptorSetLayouts,length(pDescriptorSetLayouts));
  if fCountDescriptorSetLayouts>length(fDescriptorSetLayouts) then begin
   SetLength(fDescriptorSetLayouts,fCountDescriptorSetLayouts*2);
  end;
  Move(pDescriptorSetLayouts[0],fDescriptorSetLayouts[result],length(pDescriptorSetLayouts)*SizeOf(TVkDescriptorSetLayout));
 end else begin
  result:=-1;
 end;
end;

function TVulkanPipelineLayout.AddDescriptorSetLayouts(const pDescriptorSetLayouts:array of TVulkanDescriptorSetLayout):TVkInt32;
var Index:TVkInt32;
begin
 if length(pDescriptorSetLayouts)>0 then begin
  result:=fCountDescriptorSetLayouts;
  inc(fCountDescriptorSetLayouts,length(pDescriptorSetLayouts));
  if fCountDescriptorSetLayouts>length(fDescriptorSetLayouts) then begin
   SetLength(fDescriptorSetLayouts,fCountDescriptorSetLayouts*2);
  end;
  for Index:=0 to length(pDescriptorSetLayouts)-1 do begin
   fDescriptorSetLayouts[result+Index]:=pDescriptorSetLayouts[Index].fDescriptorSetLayoutHandle;
  end;
 end else begin
  result:=-1;
 end;
end;

function TVulkanPipelineLayout.AddPushConstantRange(const pPushConstantRange:TVkPushConstantRange):TVkInt32;
begin
 result:=fCountPushConstantRanges;
 inc(fCountPushConstantRanges);
 if fCountPushConstantRanges>length(fPushConstantRanges) then begin
  SetLength(fPushConstantRanges,fCountPushConstantRanges*2);
 end;
 fPushConstantRanges[result]:=pPushConstantRange;
end;

function TVulkanPipelineLayout.AddPushConstantRange(const pStageFlags:TVkShaderStageFlags;const pOffset,pSize:TVkUInt32):TVkInt32;
var PushConstantRange:PVkPushConstantRange;
begin
 result:=fCountPushConstantRanges;
 inc(fCountPushConstantRanges);
 if fCountPushConstantRanges>length(fPushConstantRanges) then begin
  SetLength(fPushConstantRanges,fCountPushConstantRanges*2);
 end;
 PushConstantRange:=@fPushConstantRanges[result];
 PushConstantRange^.stageFlags:=pStageFlags;
 PushConstantRange^.offset:=pOffset;
 PushConstantRange^.size:=pSize;
end;

function TVulkanPipelineLayout.AddPushConstantRanges(const pPushConstantRanges:array of TVkPushConstantRange):TVkInt32;
begin
 if length(pPushConstantRanges)>0 then begin
  result:=fCountPushConstantRanges;
  inc(fCountPushConstantRanges,length(pPushConstantRanges));
  if fCountPushConstantRanges>length(fPushConstantRanges) then begin
   SetLength(fPushConstantRanges,fCountPushConstantRanges*2);
  end;
  Move(pPushConstantRanges[0],fPushConstantRanges[result],length(pPushConstantRanges)*SizeOf(TVkPushConstantRange));
 end else begin
  result:=-1;
 end;
end;

procedure TVulkanPipelineLayout.Initialize;
var PipelineLayoutCreateInfo:TVkPipelineLayoutCreateInfo;
begin

 if fPipelineLayoutHandle=VK_NULL_HANDLE then begin

  FillChar(PipelineLayoutCreateInfo,SizeOf(TVkPipelineLayoutCreateInfo),#0);
  PipelineLayoutCreateInfo.sType:=VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
  PipelineLayoutCreateInfo.pNext:=nil;
  PipelineLayoutCreateInfo.flags:=0;
  PipelineLayoutCreateInfo.setLayoutCount:=0;
  PipelineLayoutCreateInfo.pSetLayouts:=nil;
  PipelineLayoutCreateInfo.pushConstantRangeCount:=0;
  PipelineLayoutCreateInfo.pPushConstantRanges:=nil;
  
  SetLength(fDescriptorSetLayouts,fCountDescriptorSetLayouts);
  PipelineLayoutCreateInfo.setLayoutCount:=fCountDescriptorSetLayouts;
  if fCountDescriptorSetLayouts>0 then begin
   PipelineLayoutCreateInfo.pSetLayouts:=@fDescriptorSetLayouts[0];
  end else begin
   PipelineLayoutCreateInfo.pSetLayouts:=nil;
  end;

  SetLength(fPushConstantRanges,fCountPushConstantRanges);
  PipelineLayoutCreateInfo.pushConstantRangeCount:=fCountPushConstantRanges;
  if fCountPushConstantRanges>0 then begin
   PipelineLayoutCreateInfo.pPushConstantRanges:=@fPushConstantRanges[0];
  end else begin
   PipelineLayoutCreateInfo.pPushConstantRanges:=nil;
  end;

  HandleResultCode(fDevice.fDeviceVulkan.CreatePipelineLayout(fDevice.fDeviceHandle,@PipelineLayoutCreateInfo,fDevice.fAllocationCallbacks,@fPipelineLayoutHandle));

 end;

end;

constructor TVulkanPipelineShaderStage.Create(const pStage:TVkShaderStageFlagBits;
                                              const pModule:TVulkanShaderModule;
                                              const pName:TVkCharString);
begin

 inherited Create;

 fName:=pName;

 FillChar(fPipelineShaderStageCreateInfo,SizeOf(TVkPipelineShaderStageCreateInfo),#0);
 fPipelineShaderStageCreateInfo.sType:=VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO;
 fPipelineShaderStageCreateInfo.pNext:=nil;
 fPipelineShaderStageCreateInfo.flags:=0;
 fPipelineShaderStageCreateInfo.stage:=pStage;
 fPipelineShaderStageCreateInfo.module:=pModule.fShaderModuleHandle;
 fPipelineShaderStageCreateInfo.pName:=PVkChar(fName);
 fPipelineShaderStageCreateInfo.pSpecializationInfo:=nil;

 fPointerToPipelineShaderStageCreateInfo:=@fPipelineShaderStageCreateInfo;

 fSpecializationInfo:=nil;

 fDoCopyAndDoFree:=false;

 fSpecializationMapEntries:=nil;
 fCountSpecializationMapEntries:=0;

 fInitialized:=false;

end;

destructor TVulkanPipelineShaderStage.Destroy;
begin
 fName:='';
 if assigned(fSpecializationInfo) then begin
  if assigned(fSpecializationInfo.pData) and fDoCopyAndDoFree then begin
   FreeMem(fSpecializationInfo.pData);
   fSpecializationInfo.pData:=nil;
   fSpecializationInfo.dataSize:=0;
  end;
  FreeMem(fSpecializationInfo);
  fSpecializationInfo:=nil;
 end;
 SetLength(fSpecializationMapEntries,0);
 inherited Destroy;
end;

procedure TVulkanPipelineShaderStage.AllocateSpecializationInfo;
begin
 if not assigned(fSpecializationInfo) then begin
  GetMem(fSpecializationInfo,SizeOf(TVkSpecializationInfo));
  FillChar(fSpecializationInfo^,SizeOf(TVkSpecializationInfo),#0);
  fPipelineShaderStageCreateInfo.pSpecializationInfo:=fSpecializationInfo;
 end;
end;

procedure TVulkanPipelineShaderStage.AddSpecializationDataFromMemory(const pData:TVkPointer;const pDataSize:TVkSize;const pDoCopyAndDoFree:boolean=true);
begin
 if assigned(fSpecializationInfo) and assigned(fSpecializationInfo.pData) and fDoCopyAndDoFree then begin
  FreeMem(fSpecializationInfo.pData);
  fSpecializationInfo.pData:=nil;
  fSpecializationInfo.dataSize:=0;
 end;
 if assigned(pData) and (pDataSize>0) then begin
  AllocateSpecializationInfo;
  fDoCopyAndDoFree:=pDoCopyAndDoFree;
  if fDoCopyAndDoFree then begin
   GetMem(fSpecializationInfo.pData,pDataSize);
   Move(pData^,fSpecializationInfo.pData^,pDataSize);
  end else begin
   fSpecializationInfo.pData:=pData;
  end;
  fSpecializationInfo.dataSize:=pDataSize;
 end;
end;

procedure TVulkanPipelineShaderStage.AddSpecializationDataFromStream(const pStream:TStream);
begin
 if assigned(fSpecializationInfo) and assigned(fSpecializationInfo.pData) and fDoCopyAndDoFree then begin
  FreeMem(fSpecializationInfo.pData);
  fSpecializationInfo.pData:=nil;
  fSpecializationInfo.dataSize:=0;
 end;
 if assigned(pStream) and (pStream.Size>0) then begin
  AllocateSpecializationInfo;
  fDoCopyAndDoFree:=true;
  GetMem(fSpecializationInfo.pData,pStream.Size);
  if pStream.Seek(0,soBeginning)<>0 then begin
   raise EInOutError.Create('Stream seek error');
  end;
  if pStream.Read(fSpecializationInfo.pData^,pStream.Size)<>pStream.Size then begin
   raise EInOutError.Create('Stream read error');
  end;
  fSpecializationInfo.dataSize:=pStream.Size;
 end;
end;

procedure TVulkanPipelineShaderStage.AddSpecializationDataFromFile(const pFileName:string);
var FileStream:TFileStream;
begin
 FileStream:=TFileStream.Create(pFileName,fmOpenRead or fmShareDenyWrite);
 try
  AddSpecializationDataFromStream(FileStream);
 finally
  FileStream.Free;
 end;
end;

function TVulkanPipelineShaderStage.AddSpecializationMapEntry(const pSpecializationMapEntry:TVkSpecializationMapEntry):TVkInt32;
begin
 result:=fCountSpecializationMapEntries;
 inc(fCountSpecializationMapEntries);
 if length(fSpecializationMapEntries)<fCountSpecializationMapEntries then begin
  SetLength(fSpecializationMapEntries,fCountSpecializationMapEntries*2);
 end;
 fSpecializationMapEntries[result]:=pSpecializationMapEntry;
end;

function TVulkanPipelineShaderStage.AddSpecializationMapEntry(const pConstantID,pOffset:TVkUInt32;const pSize:TVkSize):TVkInt32;
var SpecializationMapEntry:PVkSpecializationMapEntry;
begin
 result:=fCountSpecializationMapEntries;
 inc(fCountSpecializationMapEntries);
 if length(fSpecializationMapEntries)<fCountSpecializationMapEntries then begin
  SetLength(fSpecializationMapEntries,fCountSpecializationMapEntries*2);
 end;
 SpecializationMapEntry:=@fSpecializationMapEntries[result];
 SpecializationMapEntry^.constantID:=pConstantID;
 SpecializationMapEntry^.offset:=pOffset;
 SpecializationMapEntry^.size:=pSize;
end;

function TVulkanPipelineShaderStage.AddSpecializationMapEntries(const pSpecializationMapEntries:array of TVkSpecializationMapEntry):TVkInt32;
begin
 if length(pSpecializationMapEntries)>0 then begin
  result:=fCountSpecializationMapEntries;
  inc(fCountSpecializationMapEntries,length(pSpecializationMapEntries));
  if length(fSpecializationMapEntries)<fCountSpecializationMapEntries then begin
   SetLength(fSpecializationMapEntries,fCountSpecializationMapEntries*2);
  end;
  Move(pSpecializationMapEntries[0],fSpecializationMapEntries[result],length(pSpecializationMapEntries)*SizeOf(TVkSpecializationMapEntry));
 end else begin
  result:=-1;
 end;
end;

procedure TVulkanPipelineShaderStage.Initialize;
begin
 if not fInitialized then begin
  fInitialized:=true;
  if fCountSpecializationMapEntries>0 then begin
   AllocateSpecializationInfo;
   SetLength(fSpecializationMapEntries,fCountSpecializationMapEntries);
   fSpecializationInfo^.mapEntryCount:=fCountSpecializationMapEntries;
   fSpecializationInfo^.pMapEntries:=@fSpecializationMapEntries[0];
  end;
 end;
end;

constructor TVulkanPipelineCache.Create(const pDevice:TVulkanDevice;const pInitialData:pointer=nil;const pInitialDataSize:TVkSize=0);
var PipelineCacheCreateInfo:TVkPipelineCacheCreateInfo;
begin
 inherited Create;

 fDevice:=pDevice;

 fPipelineCacheHandle:=VK_NULL_HANDLE;

 FillChar(PipelineCacheCreateInfo,SizeOf(TVkPipelineCacheCreateInfo),#0);
 PipelineCacheCreateInfo.sType:=VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO;
 PipelineCacheCreateInfo.pNext:=nil;
 PipelineCacheCreateInfo.flags:=0;
 PipelineCacheCreateInfo.pInitialData:=pInitialData;
 PipelineCacheCreateInfo.initialDataSize:=pInitialDataSize;

 HandleResultCode(fDevice.fDeviceVulkan.CreatePipelineCache(fDevice.fDeviceHandle,@PipelineCacheCreateInfo,fDevice.fAllocationCallbacks,@fPipelineCacheHandle));

end;

constructor TVulkanPipelineCache.CreateFromMemory(const pDevice:TVulkanDevice;const pInitialData:pointer;const pInitialDataSize:TVkSize);
begin
 Create(pDevice,pInitialData,pInitialDataSize);
end;

constructor TVulkanPipelineCache.CreateFromStream(const pDevice:TVulkanDevice;const pStream:TStream);
var Data:pointer;
    DataSize:TVkSize;
begin
 fPipelineCacheHandle:=VK_NULL_HANDLE;
 if assigned(pStream) and (pStream.Size>0) then begin
  DataSize:=pStream.Size;
  GetMem(Data,DataSize);
  try
   if pStream.Seek(0,soBeginning)<>0 then begin
    raise EInOutError.Create('Stream seek error');
   end;
   if pStream.Read(Data^,pStream.Size)<>pStream.Size then begin
    raise EInOutError.Create('Stream read error');
   end;
   Create(pDevice,Data,DataSize);
  finally
   FreeMem(Data);
  end;
 end;
end;

constructor TVulkanPipelineCache.CreateFromFile(const pDevice:TVulkanDevice;const pFileName:string);
var FileStream:TFileStream;
begin
 fPipelineCacheHandle:=VK_NULL_HANDLE;
 FileStream:=TFileStream.Create(pFileName,fmOpenRead or fmShareDenyWrite);
 try
  Create(pDevice,FileStream);
 finally
  FileStream.Free;
 end;
end;

destructor TVulkanPipelineCache.Destroy;
begin
 if fPipelineCacheHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.DestroyPipelineCache(fDevice.fDeviceHandle,fPipelineCacheHandle,fDevice.fAllocationCallbacks);
  fPipelineCacheHandle:=VK_NULL_HANDLE;
 end;
 inherited Destroy;
end;

procedure TVulkanPipelineCache.SaveToStream(const pStream:TStream);
var Data:pointer;
    DataSize:TVKSize;
begin
 HandleResultCode(fDevice.fDeviceVulkan.GetPipelineCacheData(fDevice.fDeviceHandle,fPipelineCacheHandle,@DataSize,nil));
 if DataSize>0 then begin
  GetMem(Data,DataSize);
  try
   HandleResultCode(fDevice.fDeviceVulkan.GetPipelineCacheData(fDevice.fDeviceHandle,fPipelineCacheHandle,@DataSize,Data));
   if pStream.Write(Data^,DataSize)<>TVkPtrInt(DataSize) then begin
    raise EInOutError.Create('Stream write error');
   end;
  finally
   FreeMem(Data);
  end;
 end;
end;

procedure TVulkanPipelineCache.SaveToFile(const pFileName:string);
var FileStream:TFileStream;
begin
 FileStream:=TFileStream.Create(pFileName,fmCreate);
 try
  SaveToStream(FileStream);
 finally
  FileStream.Free;
 end;
end;

procedure TVulkanPipelineCache.Merge(const pSourcePipelineCache:TVulkanPipelineCache);
begin
 HandleResultCode(fDevice.fDeviceVulkan.MergePipelineCaches(fDevice.fDeviceHandle,fPipelineCacheHandle,1,@pSourcePipelineCache.fPipelineCacheHandle));
end;

procedure TVulkanPipelineCache.Merge(const pSourcePipelineCaches:array of TVulkanPipelineCache);
var Index:TVkInt32;
    SourcePipelineCaches:TVkPipelineCacheArray;
begin
 if length(pSourcePipelineCaches)>0 then begin
  SourcePipelineCaches:=nil;
  try
   SetLength(SourcePipelineCaches,length(pSourcePipelineCaches));
   for Index:=0 to length(pSourcePipelineCaches)-1 do begin
    SourcePipelineCaches[Index]:=pSourcePipelineCaches[Index].fPipelineCacheHandle;
   end;
   HandleResultCode(fDevice.fDeviceVulkan.MergePipelineCaches(fDevice.fDeviceHandle,fPipelineCacheHandle,length(SourcePipelineCaches),@SourcePipelineCaches[0]));
  finally
   SetLength(SourcePipelineCaches,0);
  end;
 end;
end;

constructor TVulkanPipeline.Create(const pDevice:TVulkanDevice);
begin
 inherited Create;
 fDevice:=pDevice;
 fPipelineHandle:=VK_NULL_HANDLE;
end;

destructor TVulkanPipeline.Destroy;
begin
 if fPipelineHandle<>VK_NULL_HANDLE then begin
  fDevice.fDeviceVulkan.DestroyPipeline(fDevice.fDeviceHandle,fPipelineHandle,fDevice.fAllocationCallbacks);
  fPipelineHandle:=VK_NULL_HANDLE;
 end;
 inherited Destroy;
end;

constructor TVulkanComputePipeline.Create(const pDevice:TVulkanDevice;
                                          const pCache:TVulkanPipelineCache;
                                          const pFlags:TVkPipelineCreateFlags;
                                          const pStage:TVulkanPipelineShaderStage;
                                          const pLayout:TVulkanPipelineLayout;
                                          const pBasePipelineHandle:TVulkanPipeline;
                                          const pBasePipelineIndex:TVkInt32);
var PipelineCache:TVkPipelineCache;
    ComputePipelineCreateInfo:TVkComputePipelineCreateInfo;
begin
 inherited Create(pDevice);

 FillChar(ComputePipelineCreateInfo,SizeOf(TVkComputePipelineCreateInfo),#0);
 ComputePipelineCreateInfo.sType:=VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO;
 ComputePipelineCreateInfo.pNext:=nil;
 ComputePipelineCreateInfo.flags:=pFlags;
 if assigned(pStage) then begin
  pStage.Initialize;
  ComputePipelineCreateInfo.stage:=pStage.fPipelineShaderStageCreateInfo;
 end;
 if assigned(pLayout) then begin
  ComputePipelineCreateInfo.layout:=pLayout.fPipelineLayoutHandle;
 end else begin
  ComputePipelineCreateInfo.layout:=VK_NULL_HANDLE;
 end;
 if assigned(pBasePipelineHandle) then begin
  ComputePipelineCreateInfo.basePipelineHandle:=pBasePipelineHandle.fPipelineHandle;
 end else begin
  ComputePipelineCreateInfo.basePipelineHandle:=VK_NULL_HANDLE;
 end;
 ComputePipelineCreateInfo.basePipelineIndex:=pBasePipelineIndex;

 if assigned(pCache) then begin
  PipelineCache:=pCache.fPipelineCacheHandle;
 end else begin
  PipelineCache:=VK_NULL_HANDLE;
 end;

 HandleResultCode(fDevice.fDeviceVulkan.CreateComputePipelines(fDevice.fDeviceHandle,PipelineCache,1,@ComputePipelineCreateInfo,fDevice.fAllocationCallbacks,@fPipelineHandle));

end;

constructor TVulkanPipelineState.Create;
begin
 inherited Create;
end;

destructor TVulkanPipelineState.Destroy;
begin
 inherited Destroy;
end;

constructor TVulkanPipelineVertexInputState.Create;
begin
 inherited Create;

 FillChar(fVertexInputStateCreateInfo,SizeOf(TVkPipelineVertexInputStateCreateInfo),0);
 fVertexInputStateCreateInfo.sType:=VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO;
 fVertexInputStateCreateInfo.pNext:=nil;
 fVertexInputStateCreateInfo.flags:=0;
 fVertexInputStateCreateInfo.vertexBindingDescriptionCount:=0;
 fVertexInputStateCreateInfo.pVertexBindingDescriptions:=nil;
 fVertexInputStateCreateInfo.vertexAttributeDescriptionCount:=0;
 fVertexInputStateCreateInfo.pVertexAttributeDescriptions:=nil;

 fPointerToVertexInputStateCreateInfo:=@fVertexInputStateCreateInfo;

 fVertexInputBindingDescriptions:=nil;
 fCountVertexInputBindingDescriptions:=0;

 fVertexInputAttributeDescriptions:=nil;
 fCountVertexInputAttributeDescriptions:=0;

end;

destructor TVulkanPipelineVertexInputState.Destroy;
begin
 SetLength(fVertexInputBindingDescriptions,0);
 SetLength(fVertexInputAttributeDescriptions,0);
 inherited Destroy;
end;

function TVulkanPipelineVertexInputState.GetVertexInputBindingDescription(const pIndex:TVkInt32):PVkVertexInputBindingDescription;
begin
 result:=@fVertexInputBindingDescriptions[pIndex];
end;

function TVulkanPipelineVertexInputState.GetVertexInputAttributeDescription(const pIndex:TVkInt32):PVkVertexInputAttributeDescription;
begin
 result:=@fVertexInputAttributeDescriptions[pIndex];
end;

procedure TVulkanPipelineVertexInputState.SetCountVertexInputBindingDescriptions(const pNewCount:TVkInt32);
begin
 fCountVertexInputBindingDescriptions:=pNewCount;
 if length(fVertexInputBindingDescriptions)<fCountVertexInputBindingDescriptions then begin
  SetLength(fVertexInputBindingDescriptions,fCountVertexInputBindingDescriptions*2);
 end;
end;

procedure TVulkanPipelineVertexInputState.SetCountVertexInputAttributeDescriptions(const pNewCount:TVkInt32);
begin
 fCountVertexInputAttributeDescriptions:=pNewCount;
 if length(fVertexInputAttributeDescriptions)<fCountVertexInputAttributeDescriptions then begin
  SetLength(fVertexInputAttributeDescriptions,fCountVertexInputAttributeDescriptions*2);
 end;
end;

procedure TVulkanPipelineVertexInputState.Assign(const pFrom:TVulkanPipelineVertexInputState);
begin
 fVertexInputBindingDescriptions:=copy(pFrom.fVertexInputBindingDescriptions);
 fCountVertexInputBindingDescriptions:=pFrom.fCountVertexInputBindingDescriptions;
 fVertexInputAttributeDescriptions:=copy(pFrom.fVertexInputAttributeDescriptions);
 fCountVertexInputAttributeDescriptions:=pFrom.fCountVertexInputAttributeDescriptions;
end;

function TVulkanPipelineVertexInputState.AddVertexInputBindingDescription(const pVertexInputBindingDescription:TVkVertexInputBindingDescription):TVkInt32;
begin
 result:=fCountVertexInputBindingDescriptions;
 inc(fCountVertexInputBindingDescriptions);
 if length(fVertexInputBindingDescriptions)<fCountVertexInputBindingDescriptions then begin
  SetLength(fVertexInputBindingDescriptions,fCountVertexInputBindingDescriptions*2);
 end;
 fVertexInputBindingDescriptions[result]:=pVertexInputBindingDescription;
end;

function TVulkanPipelineVertexInputState.AddVertexInputBindingDescription(const pBinding,pStride:TVkUInt32;const pInputRate:TVkVertexInputRate):TVkInt32;
var VertexInputBindingDescription:PVkVertexInputBindingDescription;
begin
 result:=fCountVertexInputBindingDescriptions;
 inc(fCountVertexInputBindingDescriptions);
 if length(fVertexInputBindingDescriptions)<fCountVertexInputBindingDescriptions then begin
  SetLength(fVertexInputBindingDescriptions,fCountVertexInputBindingDescriptions*2);
 end;
 VertexInputBindingDescription:=@fVertexInputBindingDescriptions[result];
 VertexInputBindingDescription^.binding:=pBinding;
 VertexInputBindingDescription^.stride:=pStride;
 VertexInputBindingDescription^.inputRate:=pInputRate;
end;

function TVulkanPipelineVertexInputState.AddVertexInputBindingDescriptions(const pVertexInputBindingDescriptions:array of TVkVertexInputBindingDescription):TVkInt32;
begin
 if length(pVertexInputBindingDescriptions)>0 then begin
  result:=fCountVertexInputBindingDescriptions;
  inc(fCountVertexInputBindingDescriptions,length(pVertexInputBindingDescriptions));
  if length(fVertexInputBindingDescriptions)<fCountVertexInputBindingDescriptions then begin
   SetLength(fVertexInputBindingDescriptions,fCountVertexInputBindingDescriptions*2);
  end;
  Move(pVertexInputBindingDescriptions[0],fVertexInputBindingDescriptions[result],length(pVertexInputBindingDescriptions)*SizeOf(TVkVertexInputBindingDescription));
 end else begin
  result:=-1;
 end;
end;

function TVulkanPipelineVertexInputState.AddVertexInputAttributeDescription(const pVertexInputAttributeDescription:TVkVertexInputAttributeDescription):TVkInt32;
begin
 result:=fCountVertexInputAttributeDescriptions;
 inc(fCountVertexInputAttributeDescriptions);
 if length(fVertexInputAttributeDescriptions)<fCountVertexInputAttributeDescriptions then begin
  SetLength(fVertexInputAttributeDescriptions,fCountVertexInputAttributeDescriptions*2);
 end;
 fVertexInputAttributeDescriptions[result]:=pVertexInputAttributeDescription;
end;

function TVulkanPipelineVertexInputState.AddVertexInputAttributeDescription(const pLocation,pBinding:TVkUInt32;const pFormat:TVkFormat;const pOffset:TVkUInt32):TVkInt32;
var VertexInputAttributeDescription:PVkVertexInputAttributeDescription;
begin
 result:=fCountVertexInputAttributeDescriptions;
 inc(fCountVertexInputAttributeDescriptions);
 if length(fVertexInputAttributeDescriptions)<fCountVertexInputAttributeDescriptions then begin
  SetLength(fVertexInputAttributeDescriptions,fCountVertexInputAttributeDescriptions*2);
 end;
 VertexInputAttributeDescription:=@fVertexInputAttributeDescriptions[result];
 VertexInputAttributeDescription^.location:=pLocation;
 VertexInputAttributeDescription^.binding:=pBinding;
 VertexInputAttributeDescription^.format:=pFormat;
 VertexInputAttributeDescription^.offset:=pOffset;
end;

function TVulkanPipelineVertexInputState.AddVertexInputAttributeDescriptions(const pVertexInputAttributeDescriptions:array of TVkVertexInputAttributeDescription):TVkInt32;
begin
 if length(pVertexInputAttributeDescriptions)>0 then begin
  result:=fCountVertexInputAttributeDescriptions;
  inc(fCountVertexInputAttributeDescriptions,length(pVertexInputAttributeDescriptions));
  if length(fVertexInputAttributeDescriptions)<fCountVertexInputAttributeDescriptions then begin
   SetLength(fVertexInputAttributeDescriptions,fCountVertexInputAttributeDescriptions*2);
  end;
  Move(pVertexInputAttributeDescriptions[0],fVertexInputAttributeDescriptions[result],length(pVertexInputAttributeDescriptions)*SizeOf(TVkVertexInputAttributeDescription));
 end else begin
  result:=-1;
 end;
end;

procedure TVulkanPipelineVertexInputState.Initialize;
begin
 SetLength(fVertexInputBindingDescriptions,fCountVertexInputBindingDescriptions);
 SetLength(fVertexInputAttributeDescriptions,fCountVertexInputAttributeDescriptions);
 if (fCountVertexInputBindingDescriptions>0) or (fCountVertexInputAttributeDescriptions>0) then begin
  fVertexInputStateCreateInfo.vertexBindingDescriptionCount:=fCountVertexInputBindingDescriptions;
  if fCountVertexInputBindingDescriptions>0 then begin
   fVertexInputStateCreateInfo.pVertexBindingDescriptions:=@fVertexInputBindingDescriptions[0];
  end;
  fVertexInputStateCreateInfo.vertexAttributeDescriptionCount:=fCountVertexInputAttributeDescriptions;
  if fCountVertexInputAttributeDescriptions>0 then begin
   fVertexInputStateCreateInfo.pVertexAttributeDescriptions:=@fVertexInputAttributeDescriptions[0];
  end;
 end;
end;

constructor TVulkanPipelineInputAssemblyState.Create;
begin
 inherited Create;

 FillChar(fInputAssemblyStateCreateInfo,SizeOf(TVkPipelineInputAssemblyStateCreateInfo),#0);
 fInputAssemblyStateCreateInfo.sType:=VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO;
 fInputAssemblyStateCreateInfo.pNext:=nil;
 fInputAssemblyStateCreateInfo.flags:=0;
 fInputAssemblyStateCreateInfo.topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fInputAssemblyStateCreateInfo.primitiveRestartEnable:=VK_FALSE;

 fPointerToInputAssemblyStateCreateInfo:=@fInputAssemblyStateCreateInfo;

end;

destructor TVulkanPipelineInputAssemblyState.Destroy;
begin
 inherited Destroy;
end;

procedure TVulkanPipelineInputAssemblyState.Assign(const pFrom:TVulkanPipelineInputAssemblyState);
begin
 fInputAssemblyStateCreateInfo:=pFrom.fInputAssemblyStateCreateInfo;
end;

procedure TVulkanPipelineInputAssemblyState.SetInputAssemblyState(const pTopology:TVkPrimitiveTopology;const pPrimitiveRestartEnable:boolean);
begin
 fInputAssemblyStateCreateInfo.topology:=pTopology;
 fInputAssemblyStateCreateInfo.primitiveRestartEnable:=BooleanToVkBool[pPrimitiveRestartEnable];
end;

function TVulkanPipelineInputAssemblyState.GetTopology:TVkPrimitiveTopology;
begin
 result:=fInputAssemblyStateCreateInfo.topology;
end;

procedure TVulkanPipelineInputAssemblyState.SetTopology(const pNewValue:TVkPrimitiveTopology);
begin
 fInputAssemblyStateCreateInfo.topology:=pNewValue;
end;

function TVulkanPipelineInputAssemblyState.GetPrimitiveRestartEnable:boolean;
begin
 result:=fInputAssemblyStateCreateInfo.primitiveRestartEnable<>VK_FALSE;
end;

procedure TVulkanPipelineInputAssemblyState.SetPrimitiveRestartEnable(const pNewValue:boolean);
begin
 fInputAssemblyStateCreateInfo.primitiveRestartEnable:=BooleanToVkBool[pNewValue];
end;

constructor TVulkanPipelineTessellationState.Create;
begin
 inherited Create;

 FillChar(fTessellationStateCreateInfo,SizeOf(TVkPipelineTessellationStateCreateInfo),#0);
 fTessellationStateCreateInfo.sType:=VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO;
 fTessellationStateCreateInfo.pNext:=nil;
 fTessellationStateCreateInfo.flags:=0;
 fTessellationStateCreateInfo.patchControlPoints:=0;

 fPointerToTessellationStateCreateInfo:=@fTessellationStateCreateInfo;

end;

destructor TVulkanPipelineTessellationState.Destroy;
begin
 inherited Destroy;
end;

procedure TVulkanPipelineTessellationState.Assign(const pFrom:TVulkanPipelineTessellationState);
begin
 fTessellationStateCreateInfo:=pFrom.fTessellationStateCreateInfo;
end;

function TVulkanPipelineTessellationState.GetPatchControlPoints:TVkUInt32;
begin
 result:=fTessellationStateCreateInfo.patchControlPoints;
end;

procedure TVulkanPipelineTessellationState.SetPatchControlPoints(const pNewValue:TVkUInt32);
begin
 fTessellationStateCreateInfo.patchControlPoints:=pNewValue;
end;

procedure TVulkanPipelineTessellationState.SetTessellationState(const pPatchControlPoints:TVkUInt32);
begin
 fTessellationStateCreateInfo.patchControlPoints:=pPatchControlPoints;
end;

constructor TVulkanPipelineViewPortState.Create;
begin

 inherited Create;

 FillChar(fViewportStateCreateInfo,SizeOf(TVkPipelineViewportStateCreateInfo),#0);
 fViewportStateCreateInfo.sType:=VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO;
 fViewportStateCreateInfo.pNext:=nil;
 fViewportStateCreateInfo.flags:=0;
 fViewportStateCreateInfo.viewportCount:=0;
 fViewportStateCreateInfo.pViewports:=nil;
 fViewportStateCreateInfo.scissorCount:=0;
 fViewportStateCreateInfo.pScissors:=nil;

 fPointerToViewportStateCreateInfo:=@fViewportStateCreateInfo;

 fViewPorts:=nil;
 fCountViewPorts:=0;

 fScissors:=nil;
 fCountScissors:=0;

end;

destructor TVulkanPipelineViewPortState.Destroy;
begin
 SetLength(fViewPorts,0);
 SetLength(fScissors,0);
 inherited Destroy;
end;

procedure TVulkanPipelineViewPortState.Assign(const pFrom:TVulkanPipelineViewPortState);
begin
 fViewPorts:=copy(pFrom.fViewPorts);
 fCountViewPorts:=pFrom.fCountViewPorts;
 fScissors:=copy(pFrom.fScissors);
 fCountScissors:=pFrom.fCountScissors;
end;

function TVulkanPipelineViewPortState.GetViewPort(const pIndex:TVkInt32):PVkViewport;
begin
 result:=@fViewPorts[pIndex];
end;

function TVulkanPipelineViewPortState.GetScissor(const pIndex:TVkInt32):PVkRect2D;
begin
 result:=@fScissors[pIndex];
end;

procedure TVulkanPipelineViewPortState.SetCountViewPorts(const pNewCount:TVkInt32);
begin
 fCountViewPorts:=pNewCount;
 if length(fViewPorts)<fCountViewPorts then begin
  SetLength(fViewPorts,fCountViewPorts*2);
 end;
end;

procedure TVulkanPipelineViewPortState.SetCountScissors(const pNewCount:TVkInt32);
begin
 fCountScissors:=pNewCount;
 if length(fScissors)<fCountScissors then begin
  SetLength(fScissors,fCountScissors*2);
 end;
end;

function TVulkanPipelineViewPortState.AddViewPort(const pViewPort:TVkViewport):TVkInt32;
begin
 result:=fCountViewPorts;
 inc(fCountViewPorts);
 if length(fViewPorts)<fCountViewPorts then begin
  SetLength(fViewPorts,fCountViewPorts*2);
 end;
 fViewPorts[result]:=pViewPort;
end;

function TVulkanPipelineViewPortState.AddViewPort(const pX,pY,pWidth,pHeight,pMinDepth,pMaxDepth:TVkFloat):TVkInt32;
var Viewport:PVkViewport;
begin
 result:=fCountViewPorts;
 inc(fCountViewPorts);
 if length(fViewPorts)<fCountViewPorts then begin
  SetLength(fViewPorts,fCountViewPorts*2);
 end;
 Viewport:=@fViewPorts[result];
 Viewport^.x:=pX;
 Viewport^.y:=pY;
 Viewport^.width:=pWidth;
 Viewport^.height:=pHeight;
 Viewport^.minDepth:=pMinDepth;
 Viewport^.maxDepth:=pMaxDepth;
end;

function TVulkanPipelineViewPortState.AddViewPorts(const pViewPorts:array of TVkViewport):TVkInt32;
begin
 if length(pViewPorts)>0 then begin
  result:=fCountViewPorts;
  inc(fCountViewPorts,length(pViewPorts));
  if length(fViewPorts)<fCountViewPorts then begin
   SetLength(fViewPorts,fCountViewPorts*2);
  end;
  Move(pViewPorts[0],fViewPorts[result],length(pViewPorts)*SizeOf(TVkViewport));
 end else begin
  result:=-1;
 end;
end;

function TVulkanPipelineViewPortState.AddScissor(const pScissor:TVkRect2D):TVkInt32;
begin
 result:=fCountScissors;
 inc(fCountScissors);
 if length(fScissors)<fCountScissors then begin
  SetLength(fScissors,fCountScissors*2);
 end;
 fScissors[result]:=pScissor;
end;

function TVulkanPipelineViewPortState.AddScissor(const pX,pY:TVkInt32;const pWidth,pHeight:TVkUInt32):TVkInt32;
var Scissor:PVkRect2D;
begin
 result:=fCountScissors;
 inc(fCountScissors);
 if length(fScissors)<fCountScissors then begin
  SetLength(fScissors,fCountScissors*2);
 end;
 Scissor:=@fScissors[result];
 Scissor^.offset.x:=pX;
 Scissor^.offset.y:=pY;
 Scissor^.extent.width:=pWidth;
 Scissor^.extent.height:=pHeight;
end;

function TVulkanPipelineViewPortState.AddScissors(const pScissors:array of TVkRect2D):TVkInt32;
begin
 if length(pScissors)>0 then begin
  result:=fCountScissors;
  inc(fCountScissors,length(pScissors));
  if length(fScissors)<fCountScissors then begin
   SetLength(fScissors,fCountScissors*2);
  end;
  Move(pScissors[0],fScissors[result],length(pScissors)*SizeOf(TVkRect2D));
 end else begin
  result:=-1;
 end;
end;

procedure TVulkanPipelineViewPortState.Initialize;
begin
 SetLength(fViewPorts,fCountViewPorts);
 SetLength(fScissors,fCountScissors);
 if (fCountViewPorts>0) or (fCountScissors>0) then begin
  fViewportStateCreateInfo.viewportCount:=fCountViewPorts;
  if fCountViewPorts>0 then begin
   fViewportStateCreateInfo.pViewports:=@fViewPorts[0];
  end;
  fViewportStateCreateInfo.scissorCount:=fCountScissors;
  if fCountScissors>0 then begin
   fViewportStateCreateInfo.pScissors:=@fScissors[0];
  end;
 end;
end;

constructor TVulkanPipelineRasterizationState.Create;
begin

 inherited Create;

 FillChar(fRasterizationStateCreateInfo,SizeOf(TVkPipelineRasterizationStateCreateInfo),#0);
 fRasterizationStateCreateInfo.sType:=VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO;
 fRasterizationStateCreateInfo.pNext:=nil;
 fRasterizationStateCreateInfo.flags:=0;
 fRasterizationStateCreateInfo.depthClampEnable:=VK_TRUE;
 fRasterizationStateCreateInfo.rasterizerDiscardEnable:=VK_FALSE;
 fRasterizationStateCreateInfo.polygonMode:=VK_POLYGON_MODE_FILL;
 fRasterizationStateCreateInfo.cullMode:=TVkCullModeFlags(VK_CULL_MODE_NONE);
 fRasterizationStateCreateInfo.frontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
 fRasterizationStateCreateInfo.depthBiasEnable:=VK_TRUE;
 fRasterizationStateCreateInfo.depthBiasConstantFactor:=0.0;
 fRasterizationStateCreateInfo.depthBiasClamp:=0.0;
 fRasterizationStateCreateInfo.depthBiasSlopeFactor:=0.0;
 fRasterizationStateCreateInfo.lineWidth:=1.0;

 fPointerToRasterizationStateCreateInfo:=@fRasterizationStateCreateInfo;

end;

destructor TVulkanPipelineRasterizationState.Destroy;
begin
 inherited Destroy;
end;

procedure TVulkanPipelineRasterizationState.Assign(const pFrom:TVulkanPipelineRasterizationState);
begin
 fRasterizationStateCreateInfo:=pFrom.fRasterizationStateCreateInfo;
end;

function TVulkanPipelineRasterizationState.GetDepthClampEnable:boolean;
begin
 result:=fRasterizationStateCreateInfo.depthClampEnable<>VK_FALSE;
end;

procedure TVulkanPipelineRasterizationState.SetDepthClampEnable(const pNewValue:boolean);
begin
 fRasterizationStateCreateInfo.depthClampEnable:=BooleanToVkBool[pNewValue];
end;

function TVulkanPipelineRasterizationState.GetRasterizerDiscardEnable:boolean;
begin
 result:=fRasterizationStateCreateInfo.rasterizerDiscardEnable<>VK_FALSE;
end;

procedure TVulkanPipelineRasterizationState.SetRasterizerDiscardEnable(const pNewValue:boolean);
begin
 fRasterizationStateCreateInfo.rasterizerDiscardEnable:=BooleanToVkBool[pNewValue];
end;

function TVulkanPipelineRasterizationState.GetPolygonMode:TVkPolygonMode;
begin
 result:=fRasterizationStateCreateInfo.polygonMode;
end;

procedure TVulkanPipelineRasterizationState.SetPolygonMode(const pNewValue:TVkPolygonMode);
begin
 fRasterizationStateCreateInfo.polygonMode:=pNewValue;
end;

function TVulkanPipelineRasterizationState.GetCullMode:TVkCullModeFlags;
begin
 result:=fRasterizationStateCreateInfo.cullMode;
end;

procedure TVulkanPipelineRasterizationState.SetCullMode(const pNewValue:TVkCullModeFlags);
begin
 fRasterizationStateCreateInfo.cullMode:=pNewValue;
end;

function TVulkanPipelineRasterizationState.GetFrontFace:TVkFrontFace;
begin
 result:=fRasterizationStateCreateInfo.frontFace;
end;

procedure TVulkanPipelineRasterizationState.SetFrontFace(const pNewValue:TVkFrontFace);
begin
 fRasterizationStateCreateInfo.frontFace:=pNewValue;
end;

function TVulkanPipelineRasterizationState.GetDepthBiasEnable:boolean;
begin
 result:=fRasterizationStateCreateInfo.depthBiasEnable<>VK_FALSE;
end;

procedure TVulkanPipelineRasterizationState.SetDepthBiasEnable(const pNewValue:boolean);
begin
 fRasterizationStateCreateInfo.depthBiasEnable:=BooleanToVkBool[pNewValue];
end;

function TVulkanPipelineRasterizationState.GetDepthBiasConstantFactor:TVkFloat;
begin
 result:=fRasterizationStateCreateInfo.depthBiasConstantFactor;
end;

procedure TVulkanPipelineRasterizationState.SetDepthBiasConstantFactor(const pNewValue:TVkFloat);
begin
 fRasterizationStateCreateInfo.depthBiasConstantFactor:=pNewValue;
end;

function TVulkanPipelineRasterizationState.GetDepthBiasClamp:TVkFloat;
begin
 result:=fRasterizationStateCreateInfo.depthBiasClamp;
end;

procedure TVulkanPipelineRasterizationState.SetDepthBiasClamp(const pNewValue:TVkFloat);
begin
 fRasterizationStateCreateInfo.depthBiasClamp:=pNewValue;
end;

function TVulkanPipelineRasterizationState.GetDepthBiasSlopeFactor:TVkFloat;
begin
 result:=fRasterizationStateCreateInfo.depthBiasSlopeFactor;
end;

procedure TVulkanPipelineRasterizationState.SetDepthBiasSlopeFactor(const pNewValue:TVkFloat);
begin
 fRasterizationStateCreateInfo.depthBiasSlopeFactor:=pNewValue;
end;

function TVulkanPipelineRasterizationState.GetLineWidth:TVkFloat;
begin
 result:=fRasterizationStateCreateInfo.lineWidth;
end;

procedure TVulkanPipelineRasterizationState.SetLineWidth(const pNewValue:TVkFloat);
begin
 fRasterizationStateCreateInfo.lineWidth:=pNewValue;
end;

procedure TVulkanPipelineRasterizationState.SetRasterizationState(const pDepthClampEnable:boolean;
                                                                  const pRasterizerDiscardEnable:boolean;
                                                                  const pPolygonMode:TVkPolygonMode;
                                                                  const pCullMode:TVkCullModeFlags;
                                                                  const pFrontFace:TVkFrontFace;
                                                                  const pDepthBiasEnable:boolean;
                                                                  const pDepthBiasConstantFactor:TVkFloat;
                                                                  const pDepthBiasClamp:TVkFloat;
                                                                  const pDepthBiasSlopeFactor:TVkFloat;
                                                                  const pLineWidth:TVkFloat);
begin
 fRasterizationStateCreateInfo.depthClampEnable:=BooleanToVkBool[pDepthClampEnable];
 fRasterizationStateCreateInfo.rasterizerDiscardEnable:=BooleanToVkBool[pRasterizerDiscardEnable];
 fRasterizationStateCreateInfo.polygonMode:=pPolygonMode;
 fRasterizationStateCreateInfo.cullMode:=pCullMode;
 fRasterizationStateCreateInfo.frontFace:=pFrontFace;
 fRasterizationStateCreateInfo.depthBiasEnable:=BooleanToVkBool[pDepthBiasEnable];
 fRasterizationStateCreateInfo.depthBiasConstantFactor:=pDepthBiasConstantFactor;
 fRasterizationStateCreateInfo.depthBiasClamp:=pDepthBiasClamp;
 fRasterizationStateCreateInfo.depthBiasSlopeFactor:=pDepthBiasSlopeFactor;
 fRasterizationStateCreateInfo.lineWidth:=pLineWidth;
end;

constructor TVulkanPipelineMultisampleState.Create;
begin

 inherited Create;

 FillChar(fMultisampleStateCreateInfo,SizeOf(TVkPipelineMultisampleStateCreateInfo),#0);
 fMultisampleStateCreateInfo.sType:=VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO;
 fMultisampleStateCreateInfo.pNext:=nil;
 fMultisampleStateCreateInfo.flags:=0;
 fMultisampleStateCreateInfo.rasterizationSamples:=VK_SAMPLE_COUNT_1_BIT;
 fMultisampleStateCreateInfo.sampleShadingEnable:=VK_FALSE;
 fMultisampleStateCreateInfo.minSampleShading:=1.0;
 fMultisampleStateCreateInfo.pSampleMask:=nil;
 fMultisampleStateCreateInfo.alphaToCoverageEnable:=VK_FALSE;
 fMultisampleStateCreateInfo.alphaToOneEnable:=VK_FALSE;

 fPointerToMultisampleStateCreateInfo:=@fMultisampleStateCreateInfo;

 fSampleMasks:=nil;
 fCountSampleMasks:=0;

end;

destructor TVulkanPipelineMultisampleState.Destroy;
begin
 SetLength(fSampleMasks,0);
 inherited Destroy;
end;

procedure TVulkanPipelineMultisampleState.Assign(const pFrom:TVulkanPipelineMultisampleState);
begin
 fMultisampleStateCreateInfo:=pFrom.fMultisampleStateCreateInfo;
 fMultisampleStateCreateInfo.pSampleMask:=nil;
 fSampleMasks:=copy(pFrom.fSampleMasks);
 fCountSampleMasks:=pFrom.fCountSampleMasks;
end;

function TVulkanPipelineMultisampleState.AddSampleMask(const pSampleMask:TVkSampleMask):TVkInt32;
begin
 result:=fCountSampleMasks;
 inc(fCountSampleMasks);
 if length(fSampleMasks)<fCountSampleMasks then begin
  SetLength(fSampleMasks,fCountSampleMasks*2);
 end;
 fSampleMasks[result]:=pSampleMask;
end;

function TVulkanPipelineMultisampleState.AddSampleMasks(const pSampleMasks:array of TVkSampleMask):TVkInt32;
begin
 if length(pSampleMasks)>0 then begin
  result:=fCountSampleMasks;
  inc(fCountSampleMasks,length(pSampleMasks));
  if length(fSampleMasks)<fCountSampleMasks then begin
   SetLength(fSampleMasks,fCountSampleMasks*2);
  end;
  Move(pSampleMasks[0],fSampleMasks[result],length(pSampleMasks)*SizeOf(TVkSampleMask));
 end else begin
  result:=-1;
 end;
end;

function TVulkanPipelineMultisampleState.GetRasterizationSamples:TVkSampleCountFlagBits;
begin
 result:=fMultisampleStateCreateInfo.rasterizationSamples;
end;

procedure TVulkanPipelineMultisampleState.SetRasterizationSamples(const pNewValue:TVkSampleCountFlagBits);
begin
 fMultisampleStateCreateInfo.rasterizationSamples:=pNewValue;
end;

function TVulkanPipelineMultisampleState.GetSampleShadingEnable:boolean;
begin
 result:=fMultisampleStateCreateInfo.sampleShadingEnable<>VK_FALSE;
end;

procedure TVulkanPipelineMultisampleState.SetSampleShadingEnable(const pNewValue:boolean);
begin
 fMultisampleStateCreateInfo.sampleShadingEnable:=BooleanToVkBool[pNewValue];
end;

function TVulkanPipelineMultisampleState.GetSampleMask(const pIndex:TVkInt32):TVkSampleMask;
begin
 result:=fSampleMasks[pIndex];
end;

procedure TVulkanPipelineMultisampleState.SetSampleMask(const pIndex:TVkInt32;const pNewValue:TVkSampleMask);
begin
 fSampleMasks[pIndex]:=pNewValue;                                                          
end;

procedure TVulkanPipelineMultisampleState.SetCountSampleMasks(const pNewCount:TVkInt32);
begin
 fCountSampleMasks:=pNewCount;
 if length(fSampleMasks)<fCountSampleMasks then begin
  SetLength(fSampleMasks,fCountSampleMasks*2);
 end;
end;

function TVulkanPipelineMultisampleState.GetMinSampleShading:TVkFloat;
begin
 result:=fMultisampleStateCreateInfo.minSampleShading;
end;

procedure TVulkanPipelineMultisampleState.SetMinSampleShading(const pNewValue:TVkFloat);
begin
 fMultisampleStateCreateInfo.minSampleShading:=pNewValue;
end;

function TVulkanPipelineMultisampleState.GetAlphaToCoverageEnable:boolean;
begin
 result:=fMultisampleStateCreateInfo.alphaToCoverageEnable<>VK_FALSE;
end;

procedure TVulkanPipelineMultisampleState.SetAlphaToCoverageEnable(const pNewValue:boolean);
begin
 fMultisampleStateCreateInfo.alphaToCoverageEnable:=BooleanToVkBool[pNewValue];
end;

function TVulkanPipelineMultisampleState.GetAlphaToOneEnable:boolean;
begin
 result:=fMultisampleStateCreateInfo.alphaToOneEnable<>VK_FALSE;
end;

procedure TVulkanPipelineMultisampleState.SetAlphaToOneEnable(const pNewValue:boolean);
begin
 fMultisampleStateCreateInfo.alphaToOneEnable:=BooleanToVkBool[pNewValue];
end;

procedure TVulkanPipelineMultisampleState.SetMultisampleState(const pRasterizationSamples:TVkSampleCountFlagBits;
                                                              const pSampleShadingEnable:boolean;
                                                              const pMinSampleShading:TVkFloat;
                                                              const pSampleMask:array of TVkSampleMask;
                                                              const pAlphaToCoverageEnable:boolean;
                                                              const pAlphaToOneEnable:boolean);
begin
 fMultisampleStateCreateInfo.rasterizationSamples:=pRasterizationSamples;
 fMultisampleStateCreateInfo.sampleShadingEnable:=BooleanToVkBool[pSampleShadingEnable];
 fMultisampleStateCreateInfo.minSampleShading:=pMinSampleShading;
 fCountSampleMasks:=length(pSampleMask);
 SetLength(fSampleMasks,fCountSampleMasks);
 if length(pSampleMask)>0 then begin
  Move(pSampleMask[0],fSampleMasks[0],length(pSampleMask)*SizeOf(TVkSampleMask));
 end;
 fMultisampleStateCreateInfo.alphaToCoverageEnable:=BooleanToVkBool[pAlphaToCoverageEnable];
 fMultisampleStateCreateInfo.alphaToOneEnable:=BooleanToVkBool[pAlphaToOneEnable];
end;

procedure TVulkanPipelineMultisampleState.Initialize;
begin
 if fCountSampleMasks>0 then begin
  SetLength(fSampleMasks,fCountSampleMasks);
  fMultisampleStateCreateInfo.pSampleMask:=@fSampleMasks[0];
 end else begin
  fMultisampleStateCreateInfo.pSampleMask:=nil;
 end;
end;

constructor TVulkanStencilOpState.Create(const pStencilOpState:PVkStencilOpState);
begin
 inherited Create;
 fStencilOpState:=pStencilOpState;
end;

destructor TVulkanStencilOpState.Destroy;
begin
 inherited Destroy;
end;

procedure TVulkanStencilOpState.Assign(const pFrom:TVulkanStencilOpState);
begin
 fStencilOpState^:=pFrom.fStencilOpState^;
end;

function TVulkanStencilOpState.GetFailOp:TVkStencilOp;
begin
 result:=fStencilOpState^.failOp;
end;

procedure TVulkanStencilOpState.SetFailOp(const pNewValue:TVkStencilOp);
begin
 fStencilOpState^.failOp:=pNewValue;
end;

function TVulkanStencilOpState.GetPassOp:TVkStencilOp;
begin
 result:=fStencilOpState^.passOp;
end;

procedure TVulkanStencilOpState.SetPassOp(const pNewValue:TVkStencilOp);
begin
 fStencilOpState^.passOp:=pNewValue;
end;

function TVulkanStencilOpState.GetDepthFailOp:TVkStencilOp;
begin
 result:=fStencilOpState^.depthFailOp;
end;

procedure TVulkanStencilOpState.SetDepthFailOp(const pNewValue:TVkStencilOp);
begin
 fStencilOpState^.depthFailOp:=pNewValue;
end;

function TVulkanStencilOpState.GetCompareOp:TVkCompareOp;
begin
 result:=fStencilOpState^.compareOp;
end;

procedure TVulkanStencilOpState.SetCompareOp(const pNewValue:TVkCompareOp);
begin
 fStencilOpState^.compareOp:=pNewValue;
end;

function TVulkanStencilOpState.GetCompareMask:TVkUInt32;
begin
 result:=fStencilOpState^.compareMask;
end;

procedure TVulkanStencilOpState.SetCompareMask(const pNewValue:TVkUInt32);
begin
 fStencilOpState^.compareMask:=pNewValue;
end;

function TVulkanStencilOpState.GetWriteMask:TVkUInt32;
begin
 result:=fStencilOpState^.writeMask;
end;

procedure TVulkanStencilOpState.SetWriteMask(const pNewValue:TVkUInt32);
begin
 fStencilOpState^.writeMask:=pNewValue;
end;

function TVulkanStencilOpState.GetReference:TVkUInt32;
begin
 result:=fStencilOpState^.reference;
end;

procedure TVulkanStencilOpState.SetReference(const pNewValue:TVkUInt32);
begin
 fStencilOpState^.reference:=pNewValue;
end;

constructor TVulkanPipelineDepthStencilState.Create;
begin

 inherited Create;

 FillChar(fDepthStencilStateCreateInfo,SizeOf(TVkPipelineDepthStencilStateCreateInfo),#0);
 fDepthStencilStateCreateInfo.sType:=VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO;
 fDepthStencilStateCreateInfo.pNext:=nil;
 fDepthStencilStateCreateInfo.flags:=0;      
 fDepthStencilStateCreateInfo.depthTestEnable:=VK_TRUE;
 fDepthStencilStateCreateInfo.depthWriteEnable:=VK_TRUE;
 fDepthStencilStateCreateInfo.depthCompareOp:=VK_COMPARE_OP_LESS_OR_EQUAL;
 fDepthStencilStateCreateInfo.depthBoundsTestEnable:=VK_FALSE;
 fDepthStencilStateCreateInfo.stencilTestEnable:=VK_FALSE;
 fDepthStencilStateCreateInfo.front.failOp:=VK_STENCIL_OP_KEEP;
 fDepthStencilStateCreateInfo.front.depthFailOp:=VK_STENCIL_OP_KEEP;
 fDepthStencilStateCreateInfo.front.compareOp:=VK_COMPARE_OP_ALWAYS;
 fDepthStencilStateCreateInfo.front.compareMask:=0;
 fDepthStencilStateCreateInfo.front.writeMask:=0;
 fDepthStencilStateCreateInfo.front.reference:=0;
 fDepthStencilStateCreateInfo.back.failOp:=VK_STENCIL_OP_KEEP;
 fDepthStencilStateCreateInfo.back.depthFailOp:=VK_STENCIL_OP_KEEP;
 fDepthStencilStateCreateInfo.back.compareOp:=VK_COMPARE_OP_ALWAYS;
 fDepthStencilStateCreateInfo.back.compareMask:=0;
 fDepthStencilStateCreateInfo.back.writeMask:=0;
 fDepthStencilStateCreateInfo.back.reference:=0;
 fDepthStencilStateCreateInfo.minDepthBounds:=0.0;
 fDepthStencilStateCreateInfo.maxDepthBounds:=1.0;

 fPointerToDepthStencilStateCreateInfo:=@fDepthStencilStateCreateInfo;

 fFrontStencilOpState:=TVulkanStencilOpState.Create(@fDepthStencilStateCreateInfo.front);

 fBackStencilOpState:=TVulkanStencilOpState.Create(@fDepthStencilStateCreateInfo.back);

end;

destructor TVulkanPipelineDepthStencilState.Destroy;
begin
 fFrontStencilOpState.Free;
 fBackStencilOpState.Free;
 inherited Destroy;
end;

procedure TVulkanPipelineDepthStencilState.Assign(const pFrom:TVulkanPipelineDepthStencilState);
begin
 fDepthStencilStateCreateInfo:=pFrom.fDepthStencilStateCreateInfo;
end;

function TVulkanPipelineDepthStencilState.GetDepthTestEnable:boolean;
begin
 result:=fDepthStencilStateCreateInfo.depthTestEnable<>VK_FALSE;
end;

procedure TVulkanPipelineDepthStencilState.SetDepthTestEnable(const pNewValue:boolean);
begin
 fDepthStencilStateCreateInfo.depthTestEnable:=BooleanToVkBool[pNewValue];
end;

function TVulkanPipelineDepthStencilState.GetDepthWriteEnable:boolean;
begin
 result:=fDepthStencilStateCreateInfo.depthWriteEnable<>VK_FALSE;
end;

procedure TVulkanPipelineDepthStencilState.SetDepthWriteEnable(const pNewValue:boolean);
begin
 fDepthStencilStateCreateInfo.depthWriteEnable:=BooleanToVkBool[pNewValue];
end;

function TVulkanPipelineDepthStencilState.GetDepthCompareOp:TVkCompareOp;
begin
 result:=fDepthStencilStateCreateInfo.depthCompareOp;
end;

procedure TVulkanPipelineDepthStencilState.SetDepthCompareOp(const pNewValue:TVkCompareOp);
begin
 fDepthStencilStateCreateInfo.depthCompareOp:=pNewValue;
end;

function TVulkanPipelineDepthStencilState.GetDepthBoundsTestEnable:boolean;
begin
 result:=fDepthStencilStateCreateInfo.depthBoundsTestEnable<>VK_FALSE;
end;

procedure TVulkanPipelineDepthStencilState.SetDepthBoundsTestEnable(const pNewValue:boolean);
begin
 fDepthStencilStateCreateInfo.depthBoundsTestEnable:=BooleanToVkBool[pNewValue];
end;

function TVulkanPipelineDepthStencilState.GetStencilTestEnable:boolean;
begin
 result:=fDepthStencilStateCreateInfo.stencilTestEnable<>VK_FALSE;
end;

procedure TVulkanPipelineDepthStencilState.SetStencilTestEnable(const pNewValue:boolean);
begin
 fDepthStencilStateCreateInfo.stencilTestEnable:=BooleanToVkBool[pNewValue];
end;

function TVulkanPipelineDepthStencilState.GetMinDepthBounds:TVkFloat;
begin
 result:=fDepthStencilStateCreateInfo.minDepthBounds;
end;

procedure TVulkanPipelineDepthStencilState.SetMinDepthBounds(const pNewValue:TVkFloat);
begin
 fDepthStencilStateCreateInfo.minDepthBounds:=pNewValue;
end;

function TVulkanPipelineDepthStencilState.GetMaxDepthBounds:TVkFloat;
begin
 result:=fDepthStencilStateCreateInfo.maxDepthBounds;
end;

procedure TVulkanPipelineDepthStencilState.SetMaxDepthBounds(const pNewValue:TVkFloat);
begin
 fDepthStencilStateCreateInfo.maxDepthBounds:=pNewValue;
end;

procedure TVulkanPipelineDepthStencilState.SetDepthStencilState(const pDepthTestEnable:boolean;
                                                                const pDepthWriteEnable:boolean;
                                                                const pDepthCompareOp:TVkCompareOp;
                                                                const pDepthBoundsTestEnable:boolean;
                                                                const pStencilTestEnable:boolean;
                                                                const pFront:TVkStencilOpState;
                                                                const pBack:TVkStencilOpState;
                                                                const pMinDepthBounds:TVkFloat;
                                                                const pMaxDepthBounds:TVkFloat);
begin
 fDepthStencilStateCreateInfo.depthTestEnable:=BooleanToVkBool[pDepthTestEnable];
 fDepthStencilStateCreateInfo.depthWriteEnable:=BooleanToVkBool[pDepthWriteEnable];
 fDepthStencilStateCreateInfo.depthCompareOp:=pDepthCompareOp;
 fDepthStencilStateCreateInfo.depthBoundsTestEnable:=BooleanToVkBool[pDepthBoundsTestEnable];
 fDepthStencilStateCreateInfo.stencilTestEnable:=BooleanToVkBool[pStencilTestEnable];
 fDepthStencilStateCreateInfo.front:=pFront;
 fDepthStencilStateCreateInfo.back:=pBack;
 fDepthStencilStateCreateInfo.minDepthBounds:=pMinDepthBounds;
 fDepthStencilStateCreateInfo.maxDepthBounds:=pMaxDepthBounds;
end;

constructor TVulkanPipelineColorBlendState.Create;
begin

 inherited Create;

 FillChar(fColorBlendStateCreateInfo,SizeOf(TVkPipelineColorBlendStateCreateInfo),#0);
 fColorBlendStateCreateInfo.sType:=VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO;
 fColorBlendStateCreateInfo.pNext:=nil;
 fColorBlendStateCreateInfo.flags:=0;
 fColorBlendStateCreateInfo.logicOpEnable:=VK_FALSE;
 fColorBlendStateCreateInfo.logicOp:=VK_LOGIC_OP_NO_OP;
 fColorBlendStateCreateInfo.blendConstants[0]:=0.0;
 fColorBlendStateCreateInfo.blendConstants[1]:=0.0;
 fColorBlendStateCreateInfo.blendConstants[2]:=0.0;
 fColorBlendStateCreateInfo.blendConstants[3]:=0.0;

 fPointerToColorBlendStateCreateInfo:=@fColorBlendStateCreateInfo;

 fColorBlendAttachmentStates:=nil;
 fCountColorBlendAttachmentStates:=0;

end;

destructor TVulkanPipelineColorBlendState.Destroy;
begin
 SetLength(fColorBlendAttachmentStates,0);
 inherited Destroy;
end;

procedure TVulkanPipelineColorBlendState.Assign(const pFrom:TVulkanPipelineColorBlendState);
begin
 fColorBlendStateCreateInfo:=pFrom.fColorBlendStateCreateInfo;
 fColorBlendStateCreateInfo.attachmentCount:=0;
 fColorBlendStateCreateInfo.pAttachments:=nil;
end;

function TVulkanPipelineColorBlendState.GetLogicOpEnable:boolean;
begin
 result:=fColorBlendStateCreateInfo.logicOpEnable<>VK_FALSE;
end;

procedure TVulkanPipelineColorBlendState.SetLogicOpEnable(const pNewValue:boolean);
begin
 fColorBlendStateCreateInfo.logicOpEnable:=BooleanToVkBool[pNewValue];
end;

function TVulkanPipelineColorBlendState.GetLogicOp:TVkLogicOp;
begin
 result:=fColorBlendStateCreateInfo.logicOp;
end;

procedure TVulkanPipelineColorBlendState.SetLogicOp(const pNewValue:TVkLogicOp);
begin
 fColorBlendStateCreateInfo.logicOp:=pNewValue;
end;

procedure TVulkanPipelineColorBlendState.SetCountColorBlendAttachmentStates(const pNewCount:TVkInt32);
begin
 fCountColorBlendAttachmentStates:=pNewCount;
 if length(fColorBlendAttachmentStates)<fCountColorBlendAttachmentStates then begin
  SetLength(fColorBlendAttachmentStates,fCountColorBlendAttachmentStates*2);
 end;
end;

function TVulkanPipelineColorBlendState.GetColorBlendAttachmentState(const pIndex:TVkInt32):PVkPipelineColorBlendAttachmentState;
begin
 result:=@fColorBlendAttachmentStates[pIndex];
end;

function TVulkanPipelineColorBlendState.GetBlendConstant(const pIndex:TVkInt32):TVkFloat;
begin
 result:=fColorBlendStateCreateInfo.blendConstants[pIndex];
end;

procedure TVulkanPipelineColorBlendState.SetBlendConstant(const pIndex:TVkInt32;const pNewValue:TVkFloat);
begin
 fColorBlendStateCreateInfo.blendConstants[pIndex]:=pNewValue;
end;

procedure TVulkanPipelineColorBlendState.SetColorBlendState(const pLogicOpEnable:boolean;
                                                            const pLogicOp:TVkLogicOp;
                                                            const pBlendConstants:array of TVkFloat);
var ArrayItemCount:TVkInt32;
begin
 fColorBlendStateCreateInfo.logicOpEnable:=BooleanToVkBool[pLogicOpEnable];
 fColorBlendStateCreateInfo.logicOp:=pLogicOp;
 ArrayItemCount:=length(pBlendConstants);
 if ArrayItemCount>length(fColorBlendStateCreateInfo.blendConstants) then begin
  ArrayItemCount:=length(fColorBlendStateCreateInfo.blendConstants);
 end;
 if ArrayItemCount>0 then begin
  Move(pBlendConstants[0],fColorBlendStateCreateInfo.blendConstants[0],ArrayItemCount*SizeOf(TVkFloat));
 end;
end;

function TVulkanPipelineColorBlendState.AddColorBlendAttachmentState(const pColorBlendAttachmentState:TVkPipelineColorBlendAttachmentState):TVkInt32;
begin
 result:=fCountColorBlendAttachmentStates;
 inc(fCountColorBlendAttachmentStates);
 if length(fColorBlendAttachmentStates)<fCountColorBlendAttachmentStates then begin
  SetLength(fColorBlendAttachmentStates,fCountColorBlendAttachmentStates*2);
 end;
 fColorBlendAttachmentStates[result]:=pColorBlendAttachmentState;
end;

function TVulkanPipelineColorBlendState.AddColorBlendAttachmentState(const pBlendEnable:boolean;
                                                                     const pSrcColorBlendFactor:TVkBlendFactor;
                                                                     const pDstColorBlendFactor:TVkBlendFactor;
                                                                     const pColorBlendOp:TVkBlendOp;
                                                                     const pSrcAlphaBlendFactor:TVkBlendFactor;
                                                                     const pDstAlphaBlendFactor:TVkBlendFactor;
                                                                     const pAlphaBlendOp:TVkBlendOp;
                                                                     const pColorWriteMask:TVkColorComponentFlags):TVkInt32;
var ColorBlendAttachmentState:PVkPipelineColorBlendAttachmentState;
begin
 result:=fCountColorBlendAttachmentStates;
 inc(fCountColorBlendAttachmentStates);
 if length(fColorBlendAttachmentStates)<fCountColorBlendAttachmentStates then begin
  SetLength(fColorBlendAttachmentStates,fCountColorBlendAttachmentStates*2);
 end;
 ColorBlendAttachmentState:=@fColorBlendAttachmentStates[result];
 if pBlendEnable then begin
  ColorBlendAttachmentState^.blendEnable:=VK_TRUE;
 end else begin
  ColorBlendAttachmentState^.blendEnable:=VK_FALSE;
 end;
 ColorBlendAttachmentState^.srcColorBlendFactor:=pSrcColorBlendFactor;
 ColorBlendAttachmentState^.dstColorBlendFactor:=pDstColorBlendFactor;
 ColorBlendAttachmentState^.colorBlendOp:=pColorBlendOp;
 ColorBlendAttachmentState^.srcAlphaBlendFactor:=pSrcAlphaBlendFactor;
 ColorBlendAttachmentState^.dstAlphaBlendFactor:=pDstAlphaBlendFactor;
 ColorBlendAttachmentState^.alphaBlendOp:=pAlphaBlendOp;
 ColorBlendAttachmentState^.colorWriteMask:=pColorWriteMask;
end;

function TVulkanPipelineColorBlendState.AddColorBlendAttachmentStates(const pColorBlendAttachmentStates:array of TVkPipelineColorBlendAttachmentState):TVkInt32;
begin
 if length(pColorBlendAttachmentStates)>0 then begin
  result:=fCountColorBlendAttachmentStates;
  inc(fCountColorBlendAttachmentStates,length(pColorBlendAttachmentStates));
  if length(fColorBlendAttachmentStates)<fCountColorBlendAttachmentStates then begin
   SetLength(fColorBlendAttachmentStates,fCountColorBlendAttachmentStates*2);
  end;
  Move(pColorBlendAttachmentStates[0],fColorBlendAttachmentStates[result],length(pColorBlendAttachmentStates)*SizeOf(TVkRect2D));
 end else begin
  result:=-1;
 end;
end;

procedure TVulkanPipelineColorBlendState.Initialize;
begin
 SetLength(fColorBlendAttachmentStates,fCountColorBlendAttachmentStates);
 if fCountColorBlendAttachmentStates>0 then begin
  fColorBlendStateCreateInfo.attachmentCount:=fCountColorBlendAttachmentStates;
  fColorBlendStateCreateInfo.pAttachments:=@fColorBlendAttachmentStates[0];
 end;
end;

constructor TVulkanPipelineDynamicState.Create;
begin

 inherited Create;

 FillChar(fDynamicStateCreateInfo,SizeOf(TVkPipelineDynamicStateCreateInfo),#0);
 fDynamicStateCreateInfo.sType:=VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO;
 fDynamicStateCreateInfo.pNext:=nil;
 fDynamicStateCreateInfo.flags:=0;
 fDynamicStateCreateInfo.dynamicStateCount:=0;
 fDynamicStateCreateInfo.pDynamicStates:=nil;

 fPointerToDynamicStateCreateInfo:=@fDynamicStateCreateInfo;

 fDynamicStates:=nil;
 fCountDynamicStates:=0;

end;

destructor TVulkanPipelineDynamicState.Destroy;
begin
 SetLength(fDynamicStates,0);
 inherited Destroy;
end;

procedure TVulkanPipelineDynamicState.Assign(const pFrom:TVulkanPipelineDynamicState);
begin
 fDynamicStates:=copy(pFrom.fDynamicStates);
 fCountDynamicStates:=pFrom.fCountDynamicStates;
end;

function TVulkanPipelineDynamicState.GetDynamicState(const pIndex:TVkInt32):PVkDynamicState;
begin
 result:=@fDynamicStates[pIndex];
end;

procedure TVulkanPipelineDynamicState.SetCountDynamicStates(const pNewCount:TVkInt32);
begin
 fCountDynamicStates:=pNewCount;
 if length(fDynamicStates)<fCountDynamicStates then begin
  SetLength(fDynamicStates,fCountDynamicStates*2);
 end;
end;

function TVulkanPipelineDynamicState.AddDynamicState(const pDynamicState:TVkDynamicState):TVkInt32;
begin
 result:=fCountDynamicStates;
 inc(fCountDynamicStates);
 if length(fDynamicStates)<fCountDynamicStates then begin
  SetLength(fDynamicStates,fCountDynamicStates*2);
 end;
 fDynamicStates[result]:=pDynamicState;
end;

function TVulkanPipelineDynamicState.AddDynamicStates(const pDynamicStates:array of TVkDynamicState):TVkInt32;
begin
 if length(pDynamicStates)>0 then begin
  result:=fCountDynamicStates;
  inc(fCountDynamicStates,length(pDynamicStates));
  if length(fDynamicStates)<fCountDynamicStates then begin
   SetLength(fDynamicStates,fCountDynamicStates*2);
  end;
  Move(pDynamicStates[0],fDynamicStates[result],length(pDynamicStates)*SizeOf(TVkDynamicState));
 end else begin
  result:=-1;
 end;
end;

procedure TVulkanPipelineDynamicState.Initialize;
begin
 SetLength(fDynamicStates,fCountDynamicStates);
 fDynamicStateCreateInfo.DynamicStateCount:=fCountDynamicStates;
 if fCountDynamicStates>0 then begin
  fDynamicStateCreateInfo.pDynamicStates:=@fDynamicStates[0];
 end;
end;

constructor TVulkanGraphicsPipelineConstructor.Create(const pDevice:TVulkanDevice;
                                                      const pCache:TVulkanPipelineCache;
                                                      const pFlags:TVkPipelineCreateFlags;
                                                      const pStages:array of TVulkanPipelineShaderStage;
                                                      const pLayout:TVulkanPipelineLayout;
                                                      const pRenderPass:TVulkanRenderPass;
                                                      const pSubPass:TVkUInt32;
                                                      const pBasePipelineHandle:TVulkanPipeline;
                                                      const pBasePipelineIndex:TVkInt32);
var Index:TVkInt32;
begin
 fStages:=nil;

 inherited Create(pDevice);

 fVertexInputState:=TVulkanPipelineVertexInputState.Create;

 fInputAssemblyState:=TVulkanPipelineInputAssemblyState.Create;

 fTessellationState:=TVulkanPipelineTessellationState.Create;

 fViewPortState:=TVulkanPipelineViewPortState.Create;

 fRasterizationState:=TVulkanPipelineRasterizationState.Create;

 fMultisampleState:=TVulkanPipelineMultisampleState.Create;

 fDepthStencilState:=TVulkanPipelineDepthStencilState.Create;

 fColorBlendState:=TVulkanPipelineColorBlendState.Create;

 fDynamicState:=TVulkanPipelineDynamicState.Create;

 FillChar(fGraphicsPipelineCreateInfo,SizeOf(TVkGraphicsPipelineCreateInfo),#0);
 fGraphicsPipelineCreateInfo.sType:=VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO;
 fGraphicsPipelineCreateInfo.pNext:=nil;
 fGraphicsPipelineCreateInfo.flags:=pFlags;
 fGraphicsPipelineCreateInfo.stageCount:=length(pStages);
 if fGraphicsPipelineCreateInfo.stageCount>0 then begin
  SetLength(fStages,fGraphicsPipelineCreateInfo.stageCount);
  for Index:=0 to fGraphicsPipelineCreateInfo.stageCount-1 do begin
   pStages[Index].Initialize;
   fStages[Index]:=pStages[Index].fPipelineShaderStageCreateInfo;
  end;
  fGraphicsPipelineCreateInfo.pStages:=@fStages[0];
 end else begin
  fGraphicsPipelineCreateInfo.pStages:=nil;
 end;
 fGraphicsPipelineCreateInfo.pVertexInputState:=@fVertexInputState.fVertexInputStateCreateInfo;
 fGraphicsPipelineCreateInfo.pInputAssemblyState:=@fInputAssemblyState.fInputAssemblyStateCreateInfo;
 fGraphicsPipelineCreateInfo.pTessellationState:=nil;
 fGraphicsPipelineCreateInfo.pViewportState:=@fViewPortState.fViewportStateCreateInfo;
 fGraphicsPipelineCreateInfo.pRasterizationState:=@fRasterizationState.fRasterizationStateCreateInfo;
 fGraphicsPipelineCreateInfo.pMultisampleState:=@fMultisampleState.fMultisampleStateCreateInfo;
 fGraphicsPipelineCreateInfo.pDepthStencilState:=@fDepthStencilState.fDepthStencilStateCreateInfo;
 fGraphicsPipelineCreateInfo.pColorBlendState:=@fColorBlendState.fColorBlendStateCreateInfo;
 fGraphicsPipelineCreateInfo.pDynamicState:=nil;
 if assigned(pLayout) then begin
  fGraphicsPipelineCreateInfo.layout:=pLayout.fPipelineLayoutHandle;
 end else begin
  fGraphicsPipelineCreateInfo.layout:=VK_NULL_HANDLE;
 end;
 if assigned(pRenderPass) then begin
  fGraphicsPipelineCreateInfo.renderPass:=pRenderPass.fRenderPassHandle;
 end else begin
  fGraphicsPipelineCreateInfo.renderPass:=VK_NULL_HANDLE;
 end;
 fGraphicsPipelineCreateInfo.subpass:=pSubPass;
 if assigned(pBasePipelineHandle) then begin
  fGraphicsPipelineCreateInfo.basePipelineHandle:=pBasePipelineHandle.fPipelineHandle;
 end else begin
  fGraphicsPipelineCreateInfo.basePipelineHandle:=VK_NULL_HANDLE;
 end;
 fGraphicsPipelineCreateInfo.basePipelineIndex:=pBasePipelineIndex;

 if assigned(pCache) then begin
  fPipelineCache:=pCache.fPipelineCacheHandle;
 end else begin
  fPipelineCache:=VK_NULL_HANDLE;
 end;

end;

destructor TVulkanGraphicsPipelineConstructor.Destroy;
begin
 SetLength(fStages,0);
 fVertexInputState.Free;
 fInputAssemblyState.Free;
 fTessellationState.Free;
 fViewPortState.Free;
 fRasterizationState.Free;
 fMultisampleState.Free;
 fDepthStencilState.Free;
 fColorBlendState.Free;
 fDynamicState.Free;
 inherited Destroy;
end;

procedure TVulkanGraphicsPipelineConstructor.Assign(const pFrom:TVulkanGraphicsPipelineConstructor);
begin
 fStages:=copy(pFrom.fStages);
 fVertexInputState.Assign(pFrom.fVertexInputState);
 fInputAssemblyState.Assign(pFrom.fInputAssemblyState);
 fTessellationState.Assign(pFrom.fTessellationState);
 fViewPortState.Assign(pFrom.fViewPortState);
 fRasterizationState.Assign(pFrom.fRasterizationState);
 fMultisampleState.Assign(pFrom.fMultisampleState);
 fDepthStencilState.Assign(pFrom.fDepthStencilState);
 fColorBlendState.Assign(pFrom.fColorBlendState);
 fDynamicState.Assign(pFrom.fDynamicState);
end;

function TVulkanGraphicsPipelineConstructor.AddVertexInputBindingDescription(const pVertexInputBindingDescription:TVkVertexInputBindingDescription):TVkInt32;
begin
 Assert(assigned(fVertexInputState));
 result:=fVertexInputState.AddVertexInputBindingDescription(pVertexInputBindingDescription);
end;

function TVulkanGraphicsPipelineConstructor.AddVertexInputBindingDescription(const pBinding,pStride:TVkUInt32;const pInputRate:TVkVertexInputRate):TVkInt32;
begin
 Assert(assigned(fVertexInputState));
 result:=fVertexInputState.AddVertexInputBindingDescription(pBinding,pStride,pInputRate);
end;

function TVulkanGraphicsPipelineConstructor.AddVertexInputBindingDescriptions(const pVertexInputBindingDescriptions:array of TVkVertexInputBindingDescription):TVkInt32;
begin
 Assert(assigned(fVertexInputState));
 result:=fVertexInputState.AddVertexInputBindingDescriptions(pVertexInputBindingDescriptions);
end;

function TVulkanGraphicsPipelineConstructor.AddVertexInputAttributeDescription(const pVertexInputAttributeDescription:TVkVertexInputAttributeDescription):TVkInt32;
begin
 Assert(assigned(fVertexInputState));
 result:=fVertexInputState.AddVertexInputAttributeDescription(pVertexInputAttributeDescription);
end;

function TVulkanGraphicsPipelineConstructor.AddVertexInputAttributeDescription(const pLocation,pBinding:TVkUInt32;const pFormat:TVkFormat;const pOffset:TVkUInt32):TVkInt32;
begin
 Assert(assigned(fVertexInputState));
 result:=fVertexInputState.AddVertexInputAttributeDescription(pLocation,pBinding,pFormat,pOffset);
end;

function TVulkanGraphicsPipelineConstructor.AddVertexInputAttributeDescriptions(const pVertexInputAttributeDescriptions:array of TVkVertexInputAttributeDescription):TVkInt32;
begin
 Assert(assigned(fVertexInputState));
 result:=fVertexInputState.AddVertexInputAttributeDescriptions(pVertexInputAttributeDescriptions);
end;

procedure TVulkanGraphicsPipelineConstructor.SetInputAssemblyState(const pTopology:TVkPrimitiveTopology;const pPrimitiveRestartEnable:boolean);
begin
 Assert(assigned(fInputAssemblyState));
 fInputAssemblyState.SetInputAssemblyState(pTopology,pPrimitiveRestartEnable);
end;

procedure TVulkanGraphicsPipelineConstructor.SetTessellationState(const pPatchControlPoints:TVkUInt32);
begin
 Assert(assigned(fTessellationState));
 fTessellationState.SetTessellationState(pPatchControlPoints);
end;

function TVulkanGraphicsPipelineConstructor.AddViewPort(const pViewPort:TVkViewport):TVkInt32;
begin
 Assert(assigned(fViewPortState));
 result:=fViewPortState.AddViewPort(pViewPort);
end;

function TVulkanGraphicsPipelineConstructor.AddViewPort(const pX,pY,pWidth,pHeight,pMinDepth,pMaxDepth:TVkFloat):TVkInt32;
begin
 Assert(assigned(fViewPortState));
 result:=fViewPortState.AddViewPort(pX,pY,pWidth,pHeight,pMinDepth,pMaxDepth);
end;

function TVulkanGraphicsPipelineConstructor.AddViewPorts(const pViewPorts:array of TVkViewport):TVkInt32;
begin
 Assert(assigned(fViewPortState));
 result:=fViewPortState.AddViewPorts(pViewPorts);
end;

function TVulkanGraphicsPipelineConstructor.AddScissor(const pScissor:TVkRect2D):TVkInt32;
begin
 Assert(assigned(fViewPortState));
 result:=fViewPortState.AddScissor(pScissor);
end;

function TVulkanGraphicsPipelineConstructor.AddScissor(const pX,pY:TVkInt32;const pWidth,pHeight:TVkUInt32):TVkInt32;
begin
 Assert(assigned(fViewPortState));
 result:=fViewPortState.AddScissor(pX,pY,pWidth,pHeight);
end;

function TVulkanGraphicsPipelineConstructor.AddScissors(const pScissors:array of TVkRect2D):TVkInt32;
begin
 Assert(assigned(fViewPortState));
 result:=fViewPortState.AddScissors(pScissors);
end;

procedure TVulkanGraphicsPipelineConstructor.SetRasterizationState(const pDepthClampEnable:boolean;
                                                                   const pRasterizerDiscardEnable:boolean;
                                                                   const pPolygonMode:TVkPolygonMode;
                                                                   const pCullMode:TVkCullModeFlags;
                                                                   const pFrontFace:TVkFrontFace;
                                                                   const pDepthBiasEnable:boolean;
                                                                   const pDepthBiasConstantFactor:TVkFloat;
                                                                   const pDepthBiasClamp:TVkFloat;
                                                                   const pDepthBiasSlopeFactor:TVkFloat;
                                                                   const pLineWidth:TVkFloat);
begin
 Assert(assigned(fRasterizationState));
 fRasterizationState.SetRasterizationState(pDepthClampEnable,
                                           pRasterizerDiscardEnable,
                                           pPolygonMode,
                                           pCullMode,
                                           pFrontFace,
                                           pDepthBiasEnable,
                                           pDepthBiasConstantFactor,
                                           pDepthBiasClamp,
                                           pDepthBiasSlopeFactor,
                                           pLineWidth);
end;

procedure TVulkanGraphicsPipelineConstructor.SetMultisampleState(const pRasterizationSamples:TVkSampleCountFlagBits;
                                                                 const pSampleShadingEnable:boolean;
                                                                 const pMinSampleShading:TVkFloat;
                                                                 const pSampleMask:array of TVkSampleMask;
                                                                 const pAlphaToCoverageEnable:boolean;
                                                                 const pAlphaToOneEnable:boolean);
begin
 Assert(assigned(fMultisampleState));
 fMultisampleState.SetMultisampleState(pRasterizationSamples,
                                       pSampleShadingEnable,
                                       pMinSampleShading,
                                       pSampleMask,
                                       pAlphaToCoverageEnable,
                                       pAlphaToOneEnable);
end;

procedure TVulkanGraphicsPipelineConstructor.SetDepthStencilState(const pDepthTestEnable:boolean;
                                                                  const pDepthWriteEnable:boolean;
                                                                  const pDepthCompareOp:TVkCompareOp;
                                                                  const pDepthBoundsTestEnable:boolean;
                                                                  const pStencilTestEnable:boolean;
                                                                  const pFront:TVkStencilOpState;
                                                                  const pBack:TVkStencilOpState;
                                                                  const pMinDepthBounds:TVkFloat;
                                                                  const pMaxDepthBounds:TVkFloat);
begin
 Assert(assigned(fDepthStencilState));
 fDepthStencilState.SetDepthStencilState(pDepthTestEnable,
                                         pDepthWriteEnable,
                                         pDepthCompareOp,
                                         pDepthBoundsTestEnable,
                                         pStencilTestEnable,
                                         pFront,
                                         pBack,
                                         pMinDepthBounds,
                                         pMaxDepthBounds);
end;

procedure TVulkanGraphicsPipelineConstructor.SetColorBlendState(const pLogicOpEnable:boolean;
                                                                const pLogicOp:TVkLogicOp;
                                                                const pBlendConstants:array of TVkFloat);
begin
 Assert(assigned(fColorBlendState));
 fColorBlendState.SetColorBlendState(pLogicOpEnable,
                                     pLogicOp,
                                     pBlendConstants);
end;

function TVulkanGraphicsPipelineConstructor.AddColorBlendAttachmentState(const pColorBlendAttachmentState:TVkPipelineColorBlendAttachmentState):TVkInt32;
begin
 Assert(assigned(fColorBlendState));
 result:=fColorBlendState.AddColorBlendAttachmentState(pColorBlendAttachmentState);
end;

function TVulkanGraphicsPipelineConstructor.AddColorBlendAttachmentState(const pBlendEnable:boolean;
                                                                         const pSrcColorBlendFactor:TVkBlendFactor;
                                                                         const pDstColorBlendFactor:TVkBlendFactor;
                                                                         const pColorBlendOp:TVkBlendOp;
                                                                         const pSrcAlphaBlendFactor:TVkBlendFactor;
                                                                         const pDstAlphaBlendFactor:TVkBlendFactor;
                                                                         const pAlphaBlendOp:TVkBlendOp;
                                                                         const pColorWriteMask:TVkColorComponentFlags):TVkInt32;
var ColorBlendAttachmentState:PVkPipelineColorBlendAttachmentState;
begin
 Assert(assigned(fColorBlendState));
 result:=fColorBlendState.AddColorBlendAttachmentState(pBlendEnable,
                                                       pSrcColorBlendFactor,
                                                       pDstColorBlendFactor,
                                                       pColorBlendOp,
                                                       pSrcAlphaBlendFactor,
                                                       pDstAlphaBlendFactor,
                                                       pAlphaBlendOp,
                                                       pColorWriteMask);
end;

function TVulkanGraphicsPipelineConstructor.AddColorBlendAttachmentStates(const pColorBlendAttachmentStates:array of TVkPipelineColorBlendAttachmentState):TVkInt32;
begin
 Assert(assigned(fColorBlendState));
 result:=fColorBlendState.AddColorBlendAttachmentStates(pColorBlendAttachmentStates);
end;

function TVulkanGraphicsPipelineConstructor.AddDynamicState(const pDynamicState:TVkDynamicState):TVkInt32;
begin
 Assert(assigned(fDynamicState));
 result:=fDynamicState.AddDynamicState(pDynamicState);
end;

function TVulkanGraphicsPipelineConstructor.AddDynamicStates(const pDynamicStates:array of TVkDynamicState):TVkInt32;
begin
 Assert(assigned(fDynamicState));
 result:=fDynamicState.AddDynamicStates(pDynamicStates);
end;

procedure TVulkanGraphicsPipelineConstructor.Initialize;
begin
 if fPipelineHandle=VK_NULL_HANDLE then begin

  fVertexInputState.Initialize;

  if fTessellationState.fTessellationStateCreateInfo.patchControlPoints>0 then begin
   fGraphicsPipelineCreateInfo.pTessellationState:=@fTessellationState.fTessellationStateCreateInfo;
  end;

  fViewPortState.Initialize;

  fMultisampleState.Initialize;

  fColorBlendState.Initialize;

  fDynamicState.Initialize;

  if fDynamicState.CountDynamicStates>0 then begin
   fGraphicsPipelineCreateInfo.pDynamicState:=@fDynamicState.fDynamicStateCreateInfo;
  end;

  HandleResultCode(fDevice.fDeviceVulkan.CreateGraphicsPipelines(fDevice.fDeviceHandle,fPipelineCache,1,@fGraphicsPipelineCreateInfo,fDevice.fAllocationCallbacks,@fPipelineHandle));

 end;

end;

constructor TVulkanGraphicsPipeline.Create(const pDevice:TVulkanDevice;
                                           const pCache:TVulkanPipelineCache;
                                           const pFlags:TVkPipelineCreateFlags;
                                           const pStages:array of TVulkanPipelineShaderStage;
                                           const pLayout:TVulkanPipelineLayout;
                                           const pRenderPass:TVulkanRenderPass;
                                           const pSubPass:TVkUInt32;
                                           const pBasePipelineHandle:TVulkanPipeline;
                                           const pBasePipelineIndex:TVkInt32);
begin
 inherited Create(pDevice);
 fGraphicsPipelineConstructor:=TVulkanGraphicsPipelineConstructor.Create(fDevice,
                                                                         pCache,
                                                                         pFlags,
                                                                         pStages,
                                                                         pLayout,
                                                                         pRenderPass,
                                                                         pSubPass,
                                                                         pBasePipelineHandle,
                                                                         pBasePipelineIndex);
end;

destructor TVulkanGraphicsPipeline.Destroy;
begin
 FreeAndNil(fGraphicsPipelineConstructor);
 inherited Destroy;
end;

procedure TVulkanGraphicsPipeline.Assign(const pFrom:TVulkanGraphicsPipeline);
begin
 fGraphicsPipelineConstructor.Assign(pFrom.fGraphicsPipelineConstructor);
end;

function TVulkanGraphicsPipeline.GetVertexInputState:TVulkanPipelineVertexInputState;
begin
 result:=fGraphicsPipelineConstructor.VertexInputState;
end;

function TVulkanGraphicsPipeline.GetInputAssemblyState:TVulkanPipelineInputAssemblyState;
begin
 result:=fGraphicsPipelineConstructor.InputAssemblyState;
end;

function TVulkanGraphicsPipeline.GetTessellationState:TVulkanPipelineTessellationState;
begin
 result:=fGraphicsPipelineConstructor.TessellationState;
end;

function TVulkanGraphicsPipeline.GetViewPortState:TVulkanPipelineViewPortState;
begin
 result:=fGraphicsPipelineConstructor.ViewPortState;
end;

function TVulkanGraphicsPipeline.GetRasterizationState:TVulkanPipelineRasterizationState;
begin
 result:=fGraphicsPipelineConstructor.RasterizationState;
end;

function TVulkanGraphicsPipeline.GetMultisampleState:TVulkanPipelineMultisampleState;
begin
 result:=fGraphicsPipelineConstructor.MultisampleState;
end;

function TVulkanGraphicsPipeline.GetDepthStencilState:TVulkanPipelineDepthStencilState;
begin
 result:=fGraphicsPipelineConstructor.DepthStencilState;
end;

function TVulkanGraphicsPipeline.GetColorBlendState:TVulkanPipelineColorBlendState;
begin
 result:=fGraphicsPipelineConstructor.ColorBlendState;
end;

function TVulkanGraphicsPipeline.GetDynamicState:TVulkanPipelineDynamicState;
begin
 result:=fGraphicsPipelineConstructor.DynamicState;
end;

function TVulkanGraphicsPipeline.AddVertexInputBindingDescription(const pVertexInputBindingDescription:TVkVertexInputBindingDescription):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddVertexInputBindingDescription(pVertexInputBindingDescription);
end;

function TVulkanGraphicsPipeline.AddVertexInputBindingDescription(const pBinding,pStride:TVkUInt32;const pInputRate:TVkVertexInputRate):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddVertexInputBindingDescription(pBinding,pStride,pInputRate);
end;

function TVulkanGraphicsPipeline.AddVertexInputBindingDescriptions(const pVertexInputBindingDescriptions:array of TVkVertexInputBindingDescription):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddVertexInputBindingDescriptions(pVertexInputBindingDescriptions);
end;

function TVulkanGraphicsPipeline.AddVertexInputAttributeDescription(const pVertexInputAttributeDescription:TVkVertexInputAttributeDescription):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddVertexInputAttributeDescription(pVertexInputAttributeDescription);
end;

function TVulkanGraphicsPipeline.AddVertexInputAttributeDescription(const pLocation,pBinding:TVkUInt32;const pFormat:TVkFormat;const pOffset:TVkUInt32):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddVertexInputAttributeDescription(pLocation,pBinding,pFormat,pOffset);
end;

function TVulkanGraphicsPipeline.AddVertexInputAttributeDescriptions(const pVertexInputAttributeDescriptions:array of TVkVertexInputAttributeDescription):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddVertexInputAttributeDescriptions(pVertexInputAttributeDescriptions);
end;

procedure TVulkanGraphicsPipeline.SetInputAssemblyState(const pTopology:TVkPrimitiveTopology;const pPrimitiveRestartEnable:boolean);
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 fGraphicsPipelineConstructor.SetInputAssemblyState(pTopology,pPrimitiveRestartEnable);
end;

procedure TVulkanGraphicsPipeline.SetTessellationState(const pPatchControlPoints:TVkUInt32);
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 fGraphicsPipelineConstructor.SetTessellationState(pPatchControlPoints);
end;

function TVulkanGraphicsPipeline.AddViewPort(const pViewPort:TVkViewport):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddViewPort(pViewPort);
end;

function TVulkanGraphicsPipeline.AddViewPort(const pX,pY,pWidth,pHeight,pMinDepth,pMaxDepth:TVkFloat):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddViewPort(pX,pY,pWidth,pHeight,pMinDepth,pMaxDepth);
end;

function TVulkanGraphicsPipeline.AddViewPorts(const pViewPorts:array of TVkViewport):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddViewPorts(pViewPorts);
end;

function TVulkanGraphicsPipeline.AddScissor(const pScissor:TVkRect2D):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddScissor(pScissor);
end;

function TVulkanGraphicsPipeline.AddScissor(const pX,pY:TVkInt32;const pWidth,pHeight:TVkUInt32):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddScissor(pX,pY,pWidth,pHeight);
end;

function TVulkanGraphicsPipeline.AddScissors(const pScissors:array of TVkRect2D):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddScissors(pScissors);
end;

procedure TVulkanGraphicsPipeline.SetRasterizationState(const pDepthClampEnable:boolean;
                                                        const pRasterizerDiscardEnable:boolean;
                                                        const pPolygonMode:TVkPolygonMode;
                                                        const pCullMode:TVkCullModeFlags;
                                                        const pFrontFace:TVkFrontFace;
                                                        const pDepthBiasEnable:boolean;
                                                        const pDepthBiasConstantFactor:TVkFloat;
                                                        const pDepthBiasClamp:TVkFloat;
                                                        const pDepthBiasSlopeFactor:TVkFloat;
                                                        const pLineWidth:TVkFloat);
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 fGraphicsPipelineConstructor.SetRasterizationState(pDepthClampEnable,
                                                    pRasterizerDiscardEnable,
                                                    pPolygonMode,
                                                    pCullMode,
                                                    pFrontFace,
                                                    pDepthBiasEnable,
                                                    pDepthBiasConstantFactor,
                                                    pDepthBiasClamp,
                                                    pDepthBiasSlopeFactor,
                                                    pLineWidth);
end;

procedure TVulkanGraphicsPipeline.SetMultisampleState(const pRasterizationSamples:TVkSampleCountFlagBits;
                                                      const pSampleShadingEnable:boolean;
                                                      const pMinSampleShading:TVkFloat;
                                                      const pSampleMask:array of TVkSampleMask;
                                                      const pAlphaToCoverageEnable:boolean;
                                                      const pAlphaToOneEnable:boolean);
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 fGraphicsPipelineConstructor.SetMultisampleState(pRasterizationSamples,
                                                  pSampleShadingEnable,
                                                  pMinSampleShading,
                                                  pSampleMask,
                                                  pAlphaToCoverageEnable,
                                                  pAlphaToOneEnable);
end;

procedure TVulkanGraphicsPipeline.SetDepthStencilState(const pDepthTestEnable:boolean;
                                                       const pDepthWriteEnable:boolean;
                                                       const pDepthCompareOp:TVkCompareOp;
                                                       const pDepthBoundsTestEnable:boolean;
                                                       const pStencilTestEnable:boolean;
                                                       const pFront:TVkStencilOpState;
                                                       const pBack:TVkStencilOpState;
                                                       const pMinDepthBounds:TVkFloat;
                                                       const pMaxDepthBounds:TVkFloat);
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 fGraphicsPipelineConstructor.SetDepthStencilState(pDepthTestEnable,
                                                   pDepthWriteEnable,
                                                   pDepthCompareOp,
                                                   pDepthBoundsTestEnable,
                                                   pStencilTestEnable,
                                                   pFront,
                                                   pBack,
                                                   pMinDepthBounds,
                                                   pMaxDepthBounds);
end;

procedure TVulkanGraphicsPipeline.SetColorBlendState(const pLogicOpEnable:boolean;
                                                     const pLogicOp:TVkLogicOp;
                                                     const pBlendConstants:array of TVkFloat);
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 fGraphicsPipelineConstructor.SetColorBlendState(pLogicOpEnable,
                                                 pLogicOp,
                                                 pBlendConstants);
end;

function TVulkanGraphicsPipeline.AddColorBlendAttachmentState(const pColorBlendAttachmentState:TVkPipelineColorBlendAttachmentState):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddColorBlendAttachmentState(pColorBlendAttachmentState);
end;

function TVulkanGraphicsPipeline.AddColorBlendAttachmentState(const pBlendEnable:boolean;
                                                              const pSrcColorBlendFactor:TVkBlendFactor;
                                                              const pDstColorBlendFactor:TVkBlendFactor;
                                                              const pColorBlendOp:TVkBlendOp;
                                                              const pSrcAlphaBlendFactor:TVkBlendFactor;
                                                              const pDstAlphaBlendFactor:TVkBlendFactor;
                                                              const pAlphaBlendOp:TVkBlendOp;
                                                              const pColorWriteMask:TVkColorComponentFlags):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddColorBlendAttachmentState(pBlendEnable,
                                                                   pSrcColorBlendFactor,
                                                                   pDstColorBlendFactor,
                                                                   pColorBlendOp,
                                                                   pSrcAlphaBlendFactor,
                                                                   pDstAlphaBlendFactor,
                                                                   pAlphaBlendOp,
                                                                   pColorWriteMask);
end;

function TVulkanGraphicsPipeline.AddColorBlendAttachmentStates(const pColorBlendAttachmentStates:array of TVkPipelineColorBlendAttachmentState):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddColorBlendAttachmentStates(pColorBlendAttachmentStates);
end;

function TVulkanGraphicsPipeline.AddDynamicState(const pDynamicState:TVkDynamicState):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddDynamicState(pDynamicState);
end;

function TVulkanGraphicsPipeline.AddDynamicStates(const pDynamicStates:array of TVkDynamicState):TVkInt32;
begin
 Assert(assigned(fGraphicsPipelineConstructor));
 result:=fGraphicsPipelineConstructor.AddDynamicStates(pDynamicStates);
end;

procedure TVulkanGraphicsPipeline.Initialize;
begin
 if fPipelineHandle=VK_NULL_HANDLE then begin
  Assert(assigned(fGraphicsPipelineConstructor));
  fGraphicsPipelineConstructor.Initialize;
  fPipelineHandle:=fGraphicsPipelineConstructor.fPipelineHandle;
  fGraphicsPipelineConstructor.fPipelineHandle:=VK_NULL_HANDLE;
 end;
end;

procedure TVulkanGraphicsPipeline.FreeMemory;
begin
 FreeAndNil(fGraphicsPipelineConstructor);
end;

end.


