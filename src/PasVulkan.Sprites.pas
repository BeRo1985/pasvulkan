(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2017, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Sprites;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses SysUtils,
     Classes,
     Math,
     PUCU,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Collections,
     PasVulkan.Framework,
     PasVulkan.XML,
     PasVulkan.VectorPath,
     PasVulkan.Image.BMP,
     PasVulkan.Image.JPEG,
     PasVulkan.Image.PNG,
     PasVulkan.Image.TGA;

type PpvSpriteTextureTexel=^TpvSpriteTextureTexel;
     TpvSpriteTextureTexel=packed record
      r:TpvUInt8;
      g:TpvUInt8;
      b:TpvUInt8;
      a:TpvUInt8;
     end;

     PpvSpriteTextureTexels=^TpvSpriteTextureTexels;
     TpvSpriteTextureTexels=array[0..65535] of TpvSpriteTextureTexel;

     TpvSpriteTexture=class
      private
       fTexture:TpvVulkanTexture;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fUploaded:boolean;
       fDirty:boolean;
       fPixels:PpvSpriteTextureTexels;
      public
       constructor Create(const aPixels:PpvSpriteTextureTexels;const aWidth,aHeight:TpvInt32); reintroduce;
       destructor Destroy; override;
       procedure Upload(const aDevice:TpvVulkanDevice;
                        const aGraphicsQueue:TpvVulkanQueue;
                        const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                        const aGraphicsFence:TpvVulkanFence;
                        const aTransferQueue:TpvVulkanQueue;
                        const aTransferCommandBuffer:TpvVulkanCommandBuffer;
                        const aTransferFence:TpvVulkanFence;
                        const aMipMaps:boolean);
       procedure Unload;
      published
       property Texture:TpvVulkanTexture read fTexture;
       property Width:TpvInt32 read fWidth;
       property Height:TpvInt32 read fHeight;
       property Uploaded:boolean read fUploaded;
       property Dirty:boolean read fDirty write fDirty;
     end;

     PpvSpriteAtlasArrayTextureTexel=^TpvSpriteAtlasArrayTextureTexel;
     TpvSpriteAtlasArrayTextureTexel=TpvSpriteTextureTexel;

     TpvSpriteAtlasArrayTextureTexels=array of TpvSpriteAtlasArrayTextureTexel;

     TpvSpriteAtlasArrayTexture=class;

     PpvSpriteAtlasArrayTextureLayerRectNode=^TpvSpriteAtlasArrayTextureLayerRectNode;
     TpvSpriteAtlasArrayTextureLayerRectNode=record
      Left:PpvSpriteAtlasArrayTextureLayerRectNode;
      Right:PpvSpriteAtlasArrayTextureLayerRectNode;
      x:TpvInt32;
      y:TpvInt32;
      Width:TpvInt32;
      Height:TpvInt32;
      FreeArea:TpvInt32;
      ContentWidth:TpvInt32;
      ContentHeight:TpvInt32;
     end;

     TPVulkanSpriteAtlasArrayTextureLayerRectNodes=array of PpvSpriteAtlasArrayTextureLayerRectNode;

     PpvSpriteAtlasArrayTextureLayer=^TpvSpriteAtlasArrayTextureLayer;
     TpvSpriteAtlasArrayTextureLayer=record
      Next:PpvSpriteAtlasArrayTextureLayer;
      ArrayTexture:TpvSpriteAtlasArrayTexture;
      RootNode:PpvSpriteAtlasArrayTextureLayerRectNode;
     end;

     TpvSpriteAtlasArrayTexture=class
      private
       fTexels:TpvSpriteAtlasArrayTextureTexels;
       fTexture:TpvVulkanTexture;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fLayers:TpvInt32;
       fCountTexels:TpvInt64;
       fSRGB:boolean;
       fUploaded:boolean;
       fDirty:boolean;
       fSpecialSizedArrayTexture:boolean;
       fLayerRootNodes:TPVulkanSpriteAtlasArrayTextureLayerRectNodes;
       fInverseWidth:TpvDouble;
       fInverseHeight:TpvDouble;
      public
       constructor Create(const aSRGB:boolean); reintroduce;
       destructor Destroy; override;
       procedure Resize(const aWidth,aHeight,aLayers:TpvInt32);
       procedure CopyIn(const aData;const aSrcWidth,aSrcHeight,aDestX,aDestY,aDestLayer:TpvInt32);
       function GetTexelPointer(const aX,aY,aLayer:TpvInt32):PpvSpriteTextureTexel;
       procedure Upload(const aDevice:TpvVulkanDevice;
                        const aGraphicsQueue:TpvVulkanQueue;
                        const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                        const aGraphicsFence:TpvVulkanFence;
                        const aTransferQueue:TpvVulkanQueue;
                        const aTransferCommandBuffer:TpvVulkanCommandBuffer;
                        const aTransferFence:TpvVulkanFence;
                        const aMipMaps:boolean);
       procedure Unload;
      published
       property Texture:TpvVulkanTexture read fTexture;
       property Width:TpvInt32 read fWidth;
       property Height:TpvInt32 read fHeight;
       property Layers:TpvInt32 read fLayers;
       property CountTexels:TpvInt64 read fCountTexels;
       property Uploaded:boolean read fUploaded;
       property Dirty:boolean read fDirty write fDirty;
       property InverseWidth:TpvDouble read fInverseWidth;
       property InverseHeight:TpvDouble read fInverseHeight;
     end;

     TpvSpriteAtlasArrayTextures=array of TpvSpriteAtlasArrayTexture;

     PpvSpriteFlag=^TpvSpriteFlag;
     TpvSpriteFlag=
      (
       pvsfSignedDistanceField,
       pvsfRotated
      );

     PpvSpriteFlags=^TpvSpriteFlags;
     TpvSpriteFlags=set of TpvSpriteFlag;

     TpvSprite=class
      private
       fName:TpvRawByteString;
       fFlags:TpvSpriteFlags;
       fArrayTexture:TpvSpriteAtlasArrayTexture;
       fX:TpvInt32;
       fY:TpvInt32;
       fLayer:TpvInt32;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fTrimmedX:TpvInt32;
       fTrimmedY:TpvInt32;
       fTrimmedWidth:TpvInt32;
       fTrimmedHeight:TpvInt32;
       fOffsetX:TpvFloat;
       fOffsetY:TpvFloat;
       fScaleX:TpvFloat;
       fScaleY:TpvFloat;
       function GetSignedDistanceField:boolean; inline;
       procedure SetSignedDistanceField(const aSignedDistanceField:boolean); inline;
       function GetRotated:boolean; inline;
       procedure SetRotated(const aRotated:boolean); inline;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
      published
       property Name:TpvRawByteString read fName write fName;
       property ArrayTexture:TpvSpriteAtlasArrayTexture read fArrayTexture write fArrayTexture;
       property x:TpvInt32 read fX write fX;
       property y:TpvInt32 read fY write fY;
       property Layer:TpvInt32 read fLayer write fLayer;
       property Width:TpvInt32 read fWidth write fWidth;
       property Height:TpvInt32 read fHeight write fHeight;
       property TrimmedX:TpvInt32 read fTrimmedX write fTrimmedX;
       property TrimmedY:TpvInt32 read fTrimmedY write fTrimmedY;
       property TrimmedWidth:TpvInt32 read fTrimmedWidth write fTrimmedWidth;
       property TrimmedHeight:TpvInt32 read fTrimmedHeight write fTrimmedHeight;
       property OffsetX:TpvFloat read fOffsetX write fOffsetX;
       property OffsetY:TpvFloat read fOffsetY write fOffsetY;
       property ScaleX:TpvFloat read fScaleX write fScaleX;
       property ScaleY:TpvFloat read fScaleY write fScaleY;
       property SignedDistanceField:boolean read GetSignedDistanceField write SetSignedDistanceField;
       property Rotated:boolean read GetRotated write SetRotated;
     end;

     TpvSprites=array of TpvSprite;

     TpvSpriteAtlasSpriteStringHashMap=class(TpvStringHashMap<TpvSprite>);

     PpvSpriteNinePatchRegionMode=^TpvSpriteNinePatchRegionMode;
     TpvSpriteNinePatchRegionMode=
      (
       pvsnprmStretch,
       pvsnprmTile
      );

     PpvSpriteNinePatchRegion=^TpvSpriteNinePatchRegion;
     TpvSpriteNinePatchRegion=record
      public
       Mode:TpvSpriteNinePatchRegionMode;
       Left:TpvInt32;
       Top:TpvInt32;
       Width:TpvInt32;
       Height:TpvInt32;
       constructor Create(const aMode:TpvSpriteNinePatchRegionMode;const aLeft,aTop,aWidth,aHeight:TpvInt32);
     end;

     PpvSpriteNinePatchRegions=^TpvSpriteNinePatchRegions;
     TpvSpriteNinePatchRegions=array[0..2,0..2] of TpvSpriteNinePatchRegion;

     PpvSpriteNinePatch=^TpvSpriteNinePatch;
     TpvSpriteNinePatch=record
      Regions:TpvSpriteNinePatchRegions;
     end;

     TpvSpriteAtlas=class
      private
       fDevice:TpvVulkanDevice;
       fArrayTextures:TpvSpriteAtlasArrayTextures;
       fCountArrayTextures:TpvInt32;
       fList:TList;
       fHashMap:TpvSpriteAtlasSpriteStringHashMap;
       fSRGB:boolean;
       fIsUploaded:boolean;
       fMipMaps:boolean;
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fMaximumCountArrayLayers:TpvInt32;
       function GetCount:TpvInt32;
       function GetItem(Index:TpvInt32):TpvSprite;
       procedure SetItem(Index:TpvInt32;Item:TpvSprite);
       function GetSprite(const Name:TpvRawByteString):TpvSprite;
       procedure AddSprite(Sprite:TpvSprite);
       function LoadImage(const aDataPointer:TpvPointer;
                          const aDataSize:TVkSizeInt;
                          var aImageData:TpvPointer;
                          var aImageWidth,aImageHeight:TpvInt32):boolean;
      public
       constructor Create(const aDevice:TpvVulkanDevice;const aSRGB:boolean=false); reintroduce;
       destructor Destroy; override;
       procedure Upload(const aGraphicsQueue:TpvVulkanQueue;
                        const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                        const aGraphicsFence:TpvVulkanFence;
                        const aTransferQueue:TpvVulkanQueue;
                        const aTransferCommandBuffer:TpvVulkanCommandBuffer;
                        const aTransferFence:TpvVulkanFence); virtual;
       procedure Unload; virtual;
       function Uploaded:boolean; virtual;
       procedure ClearAll; virtual;
       function LoadXML(const aTextureStream:TStream;const aStream:TStream):boolean;
       function LoadRawSprite(const aName:TpvRawByteString;aImageData:TpvPointer;const aImageWidth,aImageHeight:TpvInt32;const aAutomaticTrim:boolean=true;const aPadding:TpvInt32=2):TpvSprite;
       function LoadSignedDistanceFieldSprite(const aName:TpvRawByteString;const aVectorPath:TpvVectorPath;const aImageWidth,aImageHeight:TpvInt32;const aScale:TpvDouble=1.0;const aOffsetX:TpvDouble=0.0;const aOffsetY:TpvDouble=0.0;const aAutomaticTrim:boolean=true;const aPadding:TpvInt32=2):TpvSprite; overload;
       function LoadSignedDistanceFieldSprite(const aName,aSVGPath:TpvRawByteString;const aImageWidth,aImageHeight:TpvInt32;const aScale:TpvDouble=1.0;const aOffsetX:TpvDouble=0.0;const aOffsetY:TpvDouble=0.0;const aAutomaticTrim:boolean=true;const aPadding:TpvInt32=2):TpvSprite; overload;
       function LoadSprite(const aName:TpvRawByteString;aStream:TStream;const aAutomaticTrim:boolean=true;const aPadding:TpvInt32=2):TpvSprite;
       function LoadSprites(const aName:TpvRawByteString;aStream:TStream;aSpriteWidth:TpvInt32=64;aSpriteHeight:TpvInt32=64;const aAutomaticTrim:boolean=true;const aPadding:TpvInt32=2):TpvSprites;
       property Device:TpvVulkanDevice read fDevice;
       property Count:TpvInt32 read GetCount;
       property Items[Index:TpvInt32]:TpvSprite read GetItem write SetItem;
       property Sprites[const Name:TpvRawByteString]:TpvSprite read GetSprite; default;
      published
       property MipMaps:boolean read fMipMaps write fMipMaps;
       property Width:TpvInt32 read fWidth write fWidth;
       property Height:TpvInt32 read fHeight write fHeight;
       property MaximumCountArrayLayers:TpvInt32 read fMaximumCountArrayLayers write fMaximumCountArrayLayers;
     end;

implementation

uses PasVulkan.SignedDistanceField2D;

const MipMapLevels:array[boolean] of TpvInt32=(1,-1);

function NewTextureRectNode:PpvSpriteAtlasArrayTextureLayerRectNode;
begin
 GetMem(result,SizeOf(TpvSpriteAtlasArrayTextureLayerRectNode));
 FillChar(result^,SizeOf(TpvSpriteAtlasArrayTextureLayerRectNode),AnsiChar(#0));
end;

procedure FreeTextureRectNode(const Node:PpvSpriteAtlasArrayTextureLayerRectNode);
begin
 if assigned(Node) then begin
  FreeTextureRectNode(Node^.Left);
  FreeTextureRectNode(Node^.Right);
  Node^.Left:=nil;
  Node^.Right:=nil;
  FreeMem(Node);
 end;
end;

function InsertTextureRectNode(const Node:PpvSpriteAtlasArrayTextureLayerRectNode;const Width,Height,Area:TpvInt32):PpvSpriteAtlasArrayTextureLayerRectNode;
var RemainWidth,RemainHeight:TpvInt32;
begin
 result:=nil;
 if (Width<=Node^.Width) and (Height<=Node^.Height) and (Area<=Node^.FreeArea) then begin
  if assigned(Node^.Left) or assigned(Node^.Right) then begin
   // This node has children nodes, so this node has content already, so the subnodes will be processing
   if assigned(Node^.Left) then begin
    result:=InsertTextureRectNode(Node^.Left,Width,Height,Area);
    if assigned(result) then begin
     dec(Node^.FreeArea,Area);
     exit;
    end;
   end;
   if assigned(Node^.Right) then begin
    result:=InsertTextureRectNode(Node^.Right,Width,Height,Area);
    if assigned(result) then begin
     dec(Node^.FreeArea,Area);
     exit;
    end;
   end;
  end else begin
   // No children nodes, so allocate a rect here and subdivide the remained space into two subnodes
   RemainWidth:=Node^.Width-Width;
   RemainHeight:=Node^.Height-Height;
   Node^.Left:=NewTextureRectNode;
   Node^.Right:=NewTextureRectNode;
   if RemainWidth<=RemainHeight then begin
    Node^.Left^.x:=Node^.x+Width;
    Node^.Left^.y:=Node^.y;
    Node^.Left^.Width:=RemainWidth;
    Node^.Left^.Height:=Height;
    Node^.Left^.FreeArea:=Node^.Left^.Width*Node^.Left^.Height;
    Node^.Right^.x:=Node^.x;
    Node^.Right^.y:=Node^.y+Height;
    Node^.Right^.Width:=Node^.Width;
    Node^.Right^.Height:=RemainHeight;
    Node^.Right^.FreeArea:=Node^.Right^.Width*Node^.Right^.Height;
   end else begin
    Node^.Left^.x:=Node^.x;
    Node^.Left^.y:=Node^.y+Height;
    Node^.Left^.Width:=Width;
    Node^.Left^.Height:=RemainHeight;
    Node^.Left^.FreeArea:=Node^.Left^.Width*Node^.Left^.Height;
    Node^.Right^.x:=Node^.x+Width;
    Node^.Right^.y:=Node^.y;
    Node^.Right^.Width:=RemainWidth;
    Node^.Right^.Height:=Node^.Height;
    Node^.Right^.FreeArea:=Node^.Right^.Width*Node^.Right^.Height;
   end;
   Node^.Left^.ContentWidth:=0;
   Node^.Left^.ContentHeight:=0;
   Node^.Right^.ContentWidth:=0;
   Node^.Right^.ContentHeight:=0;
   Node^.ContentWidth:=Width;
   Node^.ContentHeight:=Height;
   dec(Node^.FreeArea,Area);
   result:=Node;
  end;
 end;
end;

constructor TpvSpriteTexture.Create(const aPixels:PpvSpriteTextureTexels;const aWidth,aHeight:TpvInt32);
begin
 inherited Create;

 fTexture:=nil;

 fPixels:=aPixels;

 fWidth:=aWidth;
 fHeight:=aHeight;

 fUploaded:=false;
 fDirty:=true;

end;

destructor TpvSpriteTexture.Destroy;
begin

 Unload;

 FreeAndNil(fTexture);

 fPixels:=nil;

 inherited Destroy;
end;

procedure TpvSpriteTexture.Upload(const aDevice:TpvVulkanDevice;
                                      const aGraphicsQueue:TpvVulkanQueue;
                                      const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                                      const aGraphicsFence:TpvVulkanFence;
                                      const aTransferQueue:TpvVulkanQueue;
                                      const aTransferCommandBuffer:TpvVulkanCommandBuffer;
                                      const aTransferFence:TpvVulkanFence;
                                      const aMipMaps:boolean);
begin

 if not fUploaded then begin

  FreeAndNil(fTexture);

{$if true}
  fTexture:=TpvVulkanTexture.CreateFromMemory(aDevice,
                                            aGraphicsQueue,
                                            aGraphicsCommandBuffer,
                                            aGraphicsFence,
                                            aTransferQueue,
                                            aTransferCommandBuffer,
                                            aTransferFence,
                                            VK_FORMAT_R8G8B8A8_UNORM,
                                            VK_SAMPLE_COUNT_1_BIT,
                                            Max(1,fWidth),
                                            Max(1,fHeight),
                                            0,
                                            0,
                                            1,
                                            MipMapLevels[aMipMaps],
                                            [vtufTransferDst,vtufSampled],
                                            fPixels,
                                            fWidth*fHeight*SizeOf(TpvUInt8)*4,
                                            false,
                                            false,
                                            1,
                                            true);
{$else}
 fTexture:=TpvVulkanTexture.CreateDefault(aDevice,
                                         aGraphicsQueue,
                                         aGraphicsCommandBuffer,
                                         aGraphicsFence,
                                         aTransferQueue,
                                         aTransferCommandBuffer,
                                         aTransferFence,
                                         vtdtCheckerboard,
                                         Max(1,fWidth),
                                         Max(1,fHeight),
                                         1,
                                         1,
                                         1,
                                         aMipMaps,
                                         false);
{$ifend}
  fTexture.WrapModeU:=vtwmClampToBorder;
  fTexture.WrapModeV:=vtwmClampToBorder;
  fTexture.WrapModeW:=vtwmClampToBorder;
  fTexture.BorderColor:=VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK;
  fTexture.UpdateSampler;

  fUploaded:=true;

 end;

end;

procedure TpvSpriteTexture.Unload;
begin

 if fUploaded then begin

  FreeAndNil(fTexture);

  fUploaded:=false;

 end;

end;

constructor TpvSpriteAtlasArrayTexture.Create(const aSRGB:boolean);
begin
 inherited Create;
 fTexels:=nil;
 fTexture:=nil;
 fLayerRootNodes:=nil;
 fWidth:=0;
 fHeight:=0;
 fLayers:=0;
 fCountTexels:=0;
 fSRGB:=aSRGB;
 fUploaded:=false;
 fDirty:=true;
 fSpecialSizedArrayTexture:=false;
end;

destructor TpvSpriteAtlasArrayTexture.Destroy;
var LayerIndex:TpvInt32;
begin
 Unload;
 FreeAndNil(fTexture);
 for LayerIndex:=0 to fLayers-1 do begin
  if assigned(fLayerRootNodes[LayerIndex]) then begin
   FreeTextureRectNode(fLayerRootNodes[LayerIndex]);
   fLayerRootNodes[LayerIndex]:=nil;
  end;
 end;
 fLayerRootNodes:=nil;
 fTexels:=nil;
 inherited Destroy;
end;

procedure TpvSpriteAtlasArrayTexture.Resize(const aWidth,aHeight,aLayers:TpvInt32);
var y,LayerIndex,OldWidth,OldHeight,OldLayers:TpvInt32;
    OldTexels:TpvSpriteAtlasArrayTextureTexels;
begin
 if (fWidth<>aWidth) or
    (fHeight<>aHeight) or
    (fLayers<>aLayers) then begin
  OldWidth:=fWidth;
  OldHeight:=fHeight;
  OldLayers:=fLayers;
  OldTexels:=fTexels;
  try
   fTexels:=nil;
   fWidth:=aWidth;
   fHeight:=aHeight;
   fLayers:=aLayers;
   fCountTexels:=TpvInt64(fWidth)*TpvInt64(fHeight)*TpvInt64(fLayers);
   if fCountTexels>0 then begin
    SetLength(fTexels,fCountTexels);
    FillChar(fTexels[0],fCountTexels*SizeOf(TpvSpriteTextureTexel),#0);
    for LayerIndex:=0 to Min(fLayers,OldLayers)-1 do begin
     for y:=0 to Min(fHeight,OldHeight)-1 do begin
      Move(OldTexels[((TpvInt64(LayerIndex)*OldHeight)+y)*OldWidth],fTexels[((TpvInt64(LayerIndex)*fHeight)+y)*fWidth],Min(fWidth,OldWidth)*SizeOf(TpvSpriteTextureTexel));
     end;
    end;
   end;
   for LayerIndex:=fLayers to Min(OldLayers,length(fLayerRootNodes))-1 do begin
    if assigned(fLayerRootNodes[LayerIndex]) then begin
     FreeTextureRectNode(fLayerRootNodes[LayerIndex]);
     fLayerRootNodes[LayerIndex]:=nil;
    end;
   end;
   SetLength(fLayerRootNodes,fLayers);
   for LayerIndex:=OldLayers to fLayers-1 do begin
    fLayerRootNodes[LayerIndex]:=NewTextureRectNode;
    fLayerRootNodes[LayerIndex]^.x:=0;
    fLayerRootNodes[LayerIndex]^.y:=0;
    fLayerRootNodes[LayerIndex]^.Width:=fWidth;
    fLayerRootNodes[LayerIndex]^.Height:=fHeight;
    fLayerRootNodes[LayerIndex]^.FreeArea:=fWidth*fHeight;
   end;
   fInverseWidth:=1.0/fWidth;
   fInverseHeight:=1.0/fHeight;
  finally
   OldTexels:=nil;
  end;
 end;
end;

procedure TpvSpriteAtlasArrayTexture.CopyIn(const aData;const aSrcWidth,aSrcHeight,aDestX,aDestY,aDestLayer:TpvInt32);
var dy,sx,dw:TpvInt32;
    Src,Dst:PpvSpriteTextureTexel;
begin
 sx:=Min(0,-aDestX);
 dw:=Min(Max(aSrcWidth-sx,0),fWidth-aDestX);
 if dw>0 then begin
  for dy:=Min(Max(aDestY,0),fHeight-1) to Min(Max(aDestY+(aSrcHeight-1),0),fHeight-1) do begin
   Move(PpvSpriteTextureTexels(TpvPointer(@aData))^[((dy-aDestY)*aSrcWidth)+sx],fTexels[(((TpvInt64(aDestLayer)*fHeight)+dy)*fWidth)+aDestX],dw*SizeOf(TpvSpriteTextureTexel));
  end;
 end;
end;

function TpvSpriteAtlasArrayTexture.GetTexelPointer(const aX,aY,aLayer:TpvInt32):PpvSpriteTextureTexel;
begin
 result:=@fTexels[(((TpvInt64(aLayer)*fHeight)+aY)*fWidth)+aX];
end;

procedure TpvSpriteAtlasArrayTexture.Upload(const aDevice:TpvVulkanDevice;
                                           const aGraphicsQueue:TpvVulkanQueue;
                                           const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                                           const aGraphicsFence:TpvVulkanFence;
                                           const aTransferQueue:TpvVulkanQueue;
                                           const aTransferCommandBuffer:TpvVulkanCommandBuffer;
                                           const aTransferFence:TpvVulkanFence;
                                           const aMipMaps:boolean);
begin

 if not fUploaded then begin

  FreeAndNil(fTexture);

{$if true}
  fTexture:=TpvVulkanTexture.CreateFromMemory(aDevice,
                                              aGraphicsQueue,
                                              aGraphicsCommandBuffer,
                                              aGraphicsFence,
                                              aTransferQueue,
                                              aTransferCommandBuffer,
                                              aTransferFence,
                                              TVkFormat(TVkInt32(IfThen(fSRGB,TVkInt32(VK_FORMAT_R8G8B8A8_SRGB),TVkInt32(VK_FORMAT_R8G8B8A8_UNORM)))),
                                              VK_SAMPLE_COUNT_1_BIT,
                                              Max(1,fWidth),
                                              Max(1,fHeight),
                                              0,
                                              Max(1,fLayers),
                                              1,
                                              MipMapLevels[aMipMaps],
                                              [vtufTransferDst,vtufSampled],
                                              @fTexels[0],
                                              fCountTexels*SizeOf(TpvSpriteTextureTexel),
                                              false,
                                              false,
                                              1,
                                              true);
{$else}
 fTexture:=TpvVulkanTexture.CreateDefault(aDevice,
                                          aGraphicsQueue,
                                          aGraphicsCommandBuffer,
                                          aGraphicsFence,
                                          aTransferQueue,
                                          aTransferCommandBuffer,
                                          aTransferFence,
                                          vtdtCheckerboard,
                                          Max(1,fWidth),
                                          Max(1,fHeight),
                                          0,
                                          Max(1,fLayers),
                                          1,
                                          aMipMaps,
                                          false);
{$ifend}
  fTexture.WrapModeU:=vtwmClampToBorder;
  fTexture.WrapModeV:=vtwmClampToBorder;
  fTexture.WrapModeW:=vtwmClampToBorder;
  fTexture.BorderColor:=VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK;
  fTexture.UpdateSampler;

  fUploaded:=true;

 end;

end;

procedure TpvSpriteAtlasArrayTexture.Unload;
begin
 if fUploaded then begin
  FreeAndNil(fTexture);
  fUploaded:=false;
 end;
end;

constructor TpvSprite.Create;
begin
 inherited Create;
 Name:='';
 OffsetX:=0.0;
 OffsetY:=0.0;
 ScaleX:=1.0;
 ScaleY:=1.0;
end;

destructor TpvSprite.Destroy;
begin
 Name:='';
 inherited Destroy;
end;

function TpvSprite.GetSignedDistanceField:boolean;
begin
 result:=pvsfSignedDistanceField in fFlags;
end;

procedure TpvSprite.SetSignedDistanceField(const aSignedDistanceField:boolean);
begin
 if aSignedDistanceField then begin
  Include(fFlags,pvsfSignedDistanceField);
 end else begin
  Exclude(fFlags,pvsfSignedDistanceField);
 end;
end;

function TpvSprite.GetRotated:boolean;
begin
 result:=pvsfRotated in fFlags;
end;

procedure TpvSprite.SetRotated(const aRotated:boolean);
begin
 if aRotated then begin
  Include(fFlags,pvsfRotated);
 end else begin
  Exclude(fFlags,pvsfRotated);
 end;
end;

constructor TpvSpriteNinePatchRegion.Create(const aMode:TpvSpriteNinePatchRegionMode;const aLeft,aTop,aWidth,aHeight:TpvInt32);
begin
 Mode:=aMode;
 Left:=aLeft;
 Top:=aTop;
 Width:=aWidth;
 Height:=aHeight;
end;

constructor TpvSpriteAtlas.Create(const aDevice:TpvVulkanDevice;const aSRGB:boolean=false);
begin
 fDevice:=aDevice;
 fArrayTextures:=nil;
 fCountArrayTextures:=0;
 fList:=TList.Create;
 fHashMap:=TpvSpriteAtlasSpriteStringHashMap.Create(nil);
 fSRGB:=aSRGB; 
 fIsUploaded:=false;
 fMipMaps:=true;
 fWidth:=Min(VULKAN_SPRITEATLASTEXTURE_WIDTH,fDevice.PhysicalDevice.Properties.limits.maxImageDimension2D);
 fHeight:=Min(VULKAN_SPRITEATLASTEXTURE_HEIGHT,fDevice.PhysicalDevice.Properties.limits.maxImageDimension2D);
 fMaximumCountArrayLayers:=fDevice.PhysicalDevice.Properties.limits.maxImageArrayLayers;
 inherited Create;
end;

destructor TpvSpriteAtlas.Destroy;
var Index:TpvInt32;
begin
 Unload;
 for Index:=0 to fCountArrayTextures-1 do begin
  FreeAndNil(fArrayTextures[Index]);
 end;
 fArrayTextures:=nil;
 ClearAll;
 fHashMap.Free;
 fList.Free;
 inherited Destroy;
end;

procedure TpvSpriteAtlas.ClearAll;
var Index:TpvInt32;
begin
 for Index:=0 to fList.Count-1 do begin
  TpvSprite(Items[Index]).Free;
  Items[Index]:=nil;
 end;
 fList.Clear;
 fHashMap.Clear;
end;

procedure TpvSpriteAtlas.Upload(const aGraphicsQueue:TpvVulkanQueue;
                                    const aGraphicsCommandBuffer:TpvVulkanCommandBuffer;
                                    const aGraphicsFence:TpvVulkanFence;
                                    const aTransferQueue:TpvVulkanQueue;
                                    const aTransferCommandBuffer:TpvVulkanCommandBuffer;
                                    const aTransferFence:TpvVulkanFence);
var Index:TpvInt32;
    ArrayTexture:TpvSpriteAtlasArrayTexture;
begin
 if not fIsUploaded then begin
  for Index:=0 to fCountArrayTextures-1 do begin
   ArrayTexture:=fArrayTextures[Index];
   if not ArrayTexture.Uploaded then begin
    ArrayTexture.Upload(fDevice,
                        aGraphicsQueue,
                        aGraphicsCommandBuffer,
                        aGraphicsFence,
                        aTransferQueue,
                        aTransferCommandBuffer,
                        aTransferFence,
                        fMipMaps);
    ArrayTexture.Dirty:=false;
   end;
  end;
  fIsUploaded:=true;
 end;
end;

procedure TpvSpriteAtlas.Unload;
var Index:TpvInt32;
    ArrayTexture:TpvSpriteAtlasArrayTexture;
begin
 if fIsUploaded then begin
  for Index:=0 to fCountArrayTextures-1 do begin
   ArrayTexture:=fArrayTextures[Index];
   if ArrayTexture.Uploaded then begin
    ArrayTexture.Unload;
   end;
  end;
  fIsUploaded:=false;
 end;
end;

function TpvSpriteAtlas.Uploaded:boolean;
begin
 result:=fIsUploaded;
end;

function TpvSpriteAtlas.GetCount:TpvInt32;
begin
 result:=fList.Count;
end;

function TpvSpriteAtlas.GetItem(Index:TpvInt32):TpvSprite;
begin
 result:=TpvSprite(fList.Items[Index]);
end;

procedure TpvSpriteAtlas.SetItem(Index:TpvInt32;Item:TpvSprite);
begin
 fList.Items[Index]:=TpvPointer(Item);
end;

function TpvSpriteAtlas.GetSprite(const Name:TpvRawByteString):TpvSprite;
begin
 result:=fHashMap[Name];
end;

procedure TpvSpriteAtlas.AddSprite(Sprite:TpvSprite);
begin
 fHashMap.Add(Sprite.Name,Sprite);
 fList.Add(Sprite);
end;

function TpvSpriteAtlas.LoadImage(const aDataPointer:TpvPointer;
                                      const aDataSize:TVkSizeInt;
                                      var aImageData:TpvPointer;
                                      var aImageWidth,aImageHeight:TpvInt32):boolean;
type PFirstBytes=^TFirstBytes;
     TFirstBytes=array[0..63] of TpvUInt8;
     PDDSHeader=^TDDSHeader;
     TDDSHeader=packed record
      dwMagic:TpvUInt32;
      dwSize:TpvUInt32;
      dwFlags:TpvUInt32;
      dwHeight:TpvUInt32;
      dwWidth:TpvUInt32;
      dwPitchOrLinearSize:TpvUInt32;
      dwDepth:TpvUInt32;
      dwMipMapCount:TpvUInt32;
     end;
var Index:TpvInt32;
    p8:PpvUInt8;
    p16:PpvUInt16;
    PNGPixelFormat:TpvPNGPixelFormat;
begin
 result:=false;
 if (aDataSize>7) and (PFirstBytes(aDataPointer)^[0]=$89) and (PFirstBytes(aDataPointer)^[1]=$50) and (PFirstBytes(aDataPointer)^[2]=$4e) and (PFirstBytes(aDataPointer)^[3]=$47) and (PFirstBytes(aDataPointer)^[4]=$0d) and (PFirstBytes(aDataPointer)^[5]=$0a) and (PFirstBytes(aDataPointer)^[6]=$1a) and (PFirstBytes(aDataPointer)^[7]=$0a) then begin
  PNGPixelFormat:=pvppfUnknown;
  if LoadPNGImage(aDataPointer,aDataSize,aImageData,aImageWidth,aImageHeight,false,PNGPixelFormat) then begin
   result:=true;
   if PNGPixelFormat=pvppfR16G16B16A16 then begin
    // Convert to R8G8B8A8 in-placve
    p8:=aImageData;
    p16:=aImageData;
    for Index:=1 to aImageWidth*aImageHeight*4 do begin
     p8^:=p16^ shr 8;
     inc(p8);
     inc(p16);
    end;
   end;
  end;
 end else if (aDataSize>2) and (PFirstBytes(aDataPointer)^[0]=TpvUInt8(AnsiChar('B'))) and (PFirstBytes(aDataPointer)^[1]=TpvUInt8(AnsiChar('M'))) then begin
  result:=LoadBMPImage(aDataPointer,aDataSize,aImageData,aImageWidth,aImageHeight,false);
 end else if (aDataSize>2) and (((PFirstBytes(aDataPointer)^[0] xor $ff) or (PFirstBytes(aDataPointer)^[1] xor $d8))=0) then begin
  result:=LoadJPEGImage(aDataPointer,aDataSize,aImageData,aImageWidth,aImageHeight,false);
 end else begin
  result:=LoadTGAImage(aDataPointer,aDataSize,aImageData,aImageWidth,aImageHeight,false);
 end;
end;

function TpvSpriteAtlas.LoadXML(const aTextureStream:TStream;const aStream:TStream):boolean;
var XML:TpvXML;
    MemoryStream:TMemoryStream;
    i,j:TpvInt32;
    XMLItem,XMLChildrenItem:TpvXMLItem;
    XMLTag,XMLChildrenTag:TpvXMLTag;
    SpriteName:TpvRawByteString;
    Sprite:TpvSprite;
    SpriteAtlasArrayTexture:TpvSpriteAtlasArrayTexture;
    ImageData:TpvPointer;
    ImageWidth,ImageHeight:TpvInt32;
begin
 result:=false;
 if assigned(aTextureStream) and assigned(aStream) then begin
  SpriteAtlasArrayTexture:=nil;
  MemoryStream:=TMemoryStream.Create;
  try
   aStream.Seek(0,soBeginning);
   MemoryStream.CopyFrom(aTextureStream,aTextureStream.Size);
   MemoryStream.Seek(0,soBeginning);
   ImageData:=nil;
   try
    if LoadImage(MemoryStream.Memory,MemoryStream.Size,ImageData,ImageWidth,ImageHeight) then begin
     SpriteAtlasArrayTexture:=TpvSpriteAtlasArrayTexture.Create(fSRGB);
     SpriteAtlasArrayTexture.Resize(ImageWidth,ImageHeight,1);
     if length(fArrayTextures)<(fCountArrayTextures+1) then begin
      SetLength(fArrayTextures,(fCountArrayTextures+1)*2);
     end;
     fArrayTextures[fCountArrayTextures]:=SpriteAtlasArrayTexture;
     inc(fCountArrayTextures);
     SpriteAtlasArrayTexture.fSpecialSizedArrayTexture:=true;
     SpriteAtlasArrayTexture.Dirty:=true;
     SpriteAtlasArrayTexture.fLayerRootNodes[0].FreeArea:=0;
     SpriteAtlasArrayTexture.fLayerRootNodes[0].ContentWidth:=ImageWidth;
     SpriteAtlasArrayTexture.fLayerRootNodes[0].ContentHeight:=ImageHeight;
     SpriteAtlasArrayTexture.CopyIn(ImageData^,ImageWidth,ImageHeight,0,0,0);
    end;
   finally
    if assigned(ImageData) then begin
     FreeMem(ImageData);
    end;
   end;
  finally
   MemoryStream.Free;
  end;
  if assigned(SpriteAtlasArrayTexture) then begin
   MemoryStream:=TMemoryStream.Create;
   try
    aStream.Seek(0,soBeginning);
    MemoryStream.CopyFrom(aStream,aStream.Size);
    MemoryStream.Seek(0,soBeginning);
    XML:=TpvXML.Create;
    try
     if XML.Parse(MemoryStream) then begin
      for i:=0 to XML.Root.Items.Count-1 do begin
       XMLItem:=XML.Root.Items[i];
       if assigned(XMLItem) and (XMLItem is TpvXMLTag) then begin
        XMLTag:=TpvXMLTag(XMLItem);
        if XMLTag.Name='TextureAtlas' then begin
         for j:=0 to XMLTag.Items.Count-1 do begin
          XMLChildrenItem:=XMLTag.Items[j];
          if assigned(XMLChildrenItem) and (XMLChildrenItem is TpvXMLTag) then begin
           XMLChildrenTag:=TpvXMLTag(XMLChildrenItem);
           if XMLChildrenTag.Name='sprite' then begin
            SpriteName:=XMLChildrenTag.GetParameter('n','');
            if length(SpriteName)>0 then begin
             Sprite:=TpvSprite.Create;
             Sprite.ArrayTexture:=SpriteAtlasArrayTexture;
             Sprite.Name:=SpriteName;
             Sprite.x:=StrToIntDef(String(XMLChildrenTag.GetParameter('x','0')),0);
             Sprite.y:=StrToIntDef(String(XMLChildrenTag.GetParameter('y','0')),0);
             Sprite.Layer:=0;
             Sprite.Width:=StrToIntDef(String(XMLChildrenTag.GetParameter('oW',XMLChildrenTag.GetParameter('w','0'))),0);
             Sprite.Height:=StrToIntDef(String(XMLChildrenTag.GetParameter('oH',XMLChildrenTag.GetParameter('h','0'))),0);
             Sprite.TrimmedX:=StrToIntDef(String(XMLChildrenTag.GetParameter('oX','0')),0);
             Sprite.TrimmedY:=StrToIntDef(String(XMLChildrenTag.GetParameter('oY','0')),0);
             Sprite.TrimmedWidth:=StrToIntDef(String(XMLChildrenTag.GetParameter('w','0')),0);
             Sprite.TrimmedHeight:=StrToIntDef(String(XMLChildrenTag.GetParameter('h','0')),0);
             Sprite.Rotated:=XMLChildrenTag.GetParameter('r','n')='y';
             AddSprite(Sprite);
            end;
           end;
          end;
         end;
        end;
       end;
      end;
     end;
    finally
     XML.Free;
    end;
   finally
    MemoryStream.Free;
   end;
   result:=true;
  end;
 end;
end;

function TpvSpriteAtlas.LoadRawSprite(const aName:TpvRawByteString;aImageData:TpvPointer;const aImageWidth,aImageHeight:TpvInt32;const aAutomaticTrim:boolean=true;const aPadding:TpvInt32=2):TpvSprite;
var x,y,x0,y0,x1,y1,TextureIndex,LayerIndex,Layer,TotalPadding,PaddingIndex:TpvInt32;
    ArrayTexture:TpvSpriteAtlasArrayTexture;
    Node:PpvSpriteAtlasArrayTextureLayerRectNode;
    Sprite:TpvSprite;
    sp,dp:PpvUInt32;
    OK,SpecialSizedArrayTexture:boolean;
    TrimmedImageData:TpvPointer;
    TrimmedImageWidth:TpvInt32;
    TrimmedImageHeight:TpvInt32;
begin

 result:=nil;

 try

  TotalPadding:=aPadding shl 1;

  ArrayTexture:=nil;

  Node:=nil;

  Layer:=-1;

  if assigned(aImageData) and (aImageWidth>0) and (aImageHeight>0) then begin

   x0:=0;
   y0:=0;
   x1:=aImageWidth;
   y1:=aImageHeight;

   if aAutomaticTrim then begin

    // Trim input

    for x:=0 to aImageWidth-1 do begin
     OK:=true;
     for y:=0 to aImageHeight-1 do begin
      sp:=aImageData;
      inc(sp,(y*aImageWidth)+x);
      if (sp^ and $ff000000)<>0 then begin
       OK:=false;
       break;
      end;
     end;
     if OK then begin
      x0:=x;
     end else begin
      break;
     end;
    end;

    sp:=aImageData;
    for y:=0 to aImageHeight-1 do begin
     OK:=true;
     for x:=0 to aImageWidth-1 do begin
      if (sp^ and $ff000000)<>0 then begin
       OK:=false;
       break;
      end;
      inc(sp);
     end;
     if OK then begin
      y0:=y;
     end else begin
      break;
     end;
    end;

    for x:=aImageWidth-1 downto 0 do begin
     OK:=true;
     for y:=0 to aImageHeight-1 do begin
      sp:=aImageData;
      inc(sp,(y*aImageWidth)+x);
      if (sp^ and $ff000000)<>0 then begin
       OK:=false;
       break;
      end;
     end;
     if OK then begin
      x1:=x+1;
     end else begin
      break;
     end;
    end;

    for y:=aImageHeight-1 downto 0 do begin
     OK:=true;
     sp:=aImageData;
     inc(sp,y*aImageWidth);
     for x:=0 to aImageWidth-1 do begin
      if (sp^ and $ff000000)<>0 then begin
       OK:=false;
       break;
      end;
      inc(sp);
     end;
     if OK then begin
      y1:=y+1;
     end else begin
      break;
     end;
    end;

   end;

   TrimmedImageData:=nil;

   try

    if (x0<x1) and (y0<y1) and not ((x0=0) and (y0=0) and (x1=aImageWidth) and (y1=aImageHeight)) then begin
     TrimmedImageWidth:=x1-x0;
     TrimmedImageHeight:=y1-y0;
     GetMem(TrimmedImageData,TrimmedImageWidth*TrimmedImageHeight*SizeOf(TpvUInt32));
     dp:=TrimmedImageData;
     for y:=y0 to y1-1 do begin
      sp:=aImageData;
      inc(sp,(y*aImageWidth)+x0);
      for x:=x0 to x1-1 do begin
       dp^:=sp^;
       inc(sp);
       inc(dp);
      end;
     end;
    end else begin
     TrimmedImageWidth:=aImageWidth;
     TrimmedImageHeight:=aImageHeight;
     GetMem(TrimmedImageData,TrimmedImageWidth*TrimmedImageHeight*SizeOf(TpvUInt32));
     Move(aImageData^,TrimmedImageData^,TrimmedImageWidth*TrimmedImageHeight*SizeOf(TpvUInt32));
     x0:=0;
     y0:=0;
    end;

    // Get free texture area
    for TextureIndex:=0 to fCountArrayTextures-1 do begin
     ArrayTexture:=fArrayTextures[TextureIndex];
     if not ArrayTexture.fSpecialSizedArrayTexture then begin
      for LayerIndex:=0 to ArrayTexture.fLayers-1 do begin
       if assigned(ArrayTexture.fLayerRootNodes[LayerIndex]) then begin
        // Including 2px texel bilinear interpolation protection border pixels
        Node:=InsertTextureRectNode(ArrayTexture.fLayerRootNodes[LayerIndex],TrimmedImageWidth+TotalPadding,TrimmedImageHeight+TotalPadding,(TrimmedImageWidth+TotalPadding)*(TrimmedImageHeight+TotalPadding));
        if assigned(ArrayTexture) and assigned(Node) then begin
         Layer:=LayerIndex;
         break;
        end;
       end;
       if (Layer>=0) and (assigned(ArrayTexture) and assigned(Node)) then begin
        break;
       end;
      end;
     end;
    end;

    SpecialSizedArrayTexture:=false;

    // First try to resize a already existent atlas array texture by an one new layer, but not on
    // special sized atlas array textures for big sprites, which are larger than a normal atlas
    // array texture in width and height, or for external imported sprite atlases
    if (Layer<0) or not (assigned(ArrayTexture) and assigned(Node)) then begin
     for TextureIndex:=0 to fCountArrayTextures-1 do begin
      ArrayTexture:=fArrayTextures[TextureIndex];
      if ((TrimmedImageWidth+TotalPadding)<=ArrayTexture.fWidth) and
         ((TrimmedImageHeight+TotalPadding)<=ArrayTexture.fHeight) and
         (ArrayTexture.fLayers<fMaximumCountArrayLayers) and
         not ArrayTexture.fSpecialSizedArrayTexture then begin
       LayerIndex:=ArrayTexture.fLayers;
       ArrayTexture.Resize(ArrayTexture.fWidth,ArrayTexture.fHeight,LayerIndex+1);
       Node:=InsertTextureRectNode(ArrayTexture.fLayerRootNodes[LayerIndex],TrimmedImageWidth+TotalPadding,TrimmedImageHeight+TotalPadding,(TrimmedImageWidth+TotalPadding)*(TrimmedImageHeight+TotalPadding));
       if assigned(Node) then begin
        Layer:=LayerIndex;
       end;
      end;
     end;
    end;

    // Otherwise allocate a fresh new atlas array texture
    if (Layer<0) or not (assigned(ArrayTexture) and assigned(Node)) then begin
     Layer:=0;
     SpecialSizedArrayTexture:=(fWidth<=TrimmedImageWidth) or (fHeight<=TrimmedImageHeight);
     ArrayTexture:=TpvSpriteAtlasArrayTexture.Create(fSRGB);
     ArrayTexture.fSpecialSizedArrayTexture:=SpecialSizedArrayTexture;
     ArrayTexture.Resize(Max(fWidth,TrimmedImageWidth),Max(fHeight,TrimmedImageHeight),1);
     if length(fArrayTextures)<(fCountArrayTextures+1) then begin
      SetLength(fArrayTextures,(fCountArrayTextures+1)*2);
     end;
     fArrayTextures[fCountArrayTextures]:=ArrayTexture;
     inc(fCountArrayTextures);
     ArrayTexture.Dirty:=true;
     if SpecialSizedArrayTexture then begin
      Node:=InsertTextureRectNode(ArrayTexture.fLayerRootNodes[Layer],TrimmedImageWidth,TrimmedImageHeight,TrimmedImageWidth*TrimmedImageHeight);
     end else begin
      Node:=InsertTextureRectNode(ArrayTexture.fLayerRootNodes[Layer],TrimmedImageWidth+TotalPadding,TrimmedImageHeight+TotalPadding,(TrimmedImageWidth+TotalPadding)*(TrimmedImageHeight+TotalPadding));
     end;
    end;

    Assert((Layer>=0) and (assigned(ArrayTexture) and assigned(Node)));

    if not ((Layer>=0) and (assigned(ArrayTexture) and assigned(Node))) then begin
     raise Exception.Create('Can''t load raw sprite');
    end;

    begin
     Sprite:=TpvSprite.Create;
     Sprite.ArrayTexture:=ArrayTexture;
     Sprite.Name:=aName;
     if SpecialSizedArrayTexture then begin
      Sprite.x:=Node^.x;
      Sprite.y:=Node^.y;
      Sprite.Width:=aImageWidth;
      Sprite.Height:=aImageHeight;
      Sprite.TrimmedX:=x0;
      Sprite.TrimmedY:=y0;
      Sprite.TrimmedWidth:=TrimmedImageWidth;
      Sprite.TrimmedHeight:=TrimmedImageHeight;
      Sprite.Rotated:=false;
      AddSprite(Sprite);
      for y:=0 to TrimmedImageHeight-1 do begin
       sp:=TrimmedImageData;
       inc(sp,y*TrimmedImageWidth);
       dp:=TpvPointer(ArrayTexture.GetTexelPointer(Sprite.x,Sprite.y+y,Layer));
       Move(sp^,dp^,TrimmedImageWidth*SizeOf(TpvUInt32));
      end;
     end else begin
      Sprite.x:=Node^.x+aPadding;
      Sprite.y:=Node^.y+aPadding;
      Sprite.Width:=aImageWidth;
      Sprite.Height:=aImageHeight;
      Sprite.TrimmedX:=x0;
      Sprite.TrimmedY:=y0;
      Sprite.TrimmedWidth:=TrimmedImageWidth;
      Sprite.TrimmedHeight:=TrimmedImageHeight;
      Sprite.Rotated:=false;
      AddSprite(Sprite);
      for y:=0 to TrimmedImageHeight-1 do begin
       sp:=TrimmedImageData;
       inc(sp,y*TrimmedImageWidth);
       dp:=TpvPointer(ArrayTexture.GetTexelPointer(Sprite.x,Sprite.y+y,Layer));
       Move(sp^,dp^,TrimmedImageWidth*SizeOf(TpvUInt32));
      end;
      begin
       sp:=TrimmedImageData;
       for PaddingIndex:=-1 downto -aPadding do begin
        dp:=TpvPointer(ArrayTexture.GetTexelPointer(Sprite.x,Sprite.y+PaddingIndex,Layer));
        Move(sp^,dp^,TrimmedImageWidth*SizeOf(TpvUInt32));
       end;
       sp:=TrimmedImageData;
       inc(sp,(TrimmedImageHeight-1)*TrimmedImageWidth);
       for PaddingIndex:=0 to aPadding-1 do begin
        dp:=TpvPointer(ArrayTexture.GetTexelPointer(Sprite.x,Sprite.y+TrimmedImageHeight+PaddingIndex,Layer));
        Move(sp^,dp^,TrimmedImageWidth*SizeOf(TpvUInt32));
       end;
      end;
      for y:=-1 to TrimmedImageHeight do begin
       sp:=TpvPointer(ArrayTexture.GetTexelPointer(Sprite.x,Sprite.y,Layer));
       for PaddingIndex:=-1 downto -aPadding do begin
        dp:=TpvPointer(ArrayTexture.GetTexelPointer(Sprite.x+PaddingIndex,Sprite.y,Layer));
        dp^:=sp^;
       end;
       sp:=TpvPointer(ArrayTexture.GetTexelPointer(Sprite.x+(TrimmedImageWidth-1),Sprite.y,Layer));
       for PaddingIndex:=0 to aPadding-1 do begin
        dp:=TpvPointer(ArrayTexture.GetTexelPointer(Sprite.x+TrimmedImageWidth+PaddingIndex,Sprite.y,Layer));
        dp^:=sp^;
       end;
      end;
     end;
     ArrayTexture.Dirty:=true;
    end;

   finally

    if assigned(TrimmedImageData) then begin
     FreeMem(TrimmedImageData);
    end;

   end;

   result:=Sprite;

  end else begin

   raise Exception.Create('Can''t load sprite');

  end;

 finally

 end;

end;

function TpvSpriteAtlas.LoadSignedDistanceFieldSprite(const aName:TpvRawByteString;const aVectorPath:TpvVectorPath;const aImageWidth,aImageHeight:TpvInt32;const aScale:TpvDouble=1.0;const aOffsetX:TpvDouble=0.0;const aOffsetY:TpvDouble=0.0;const aAutomaticTrim:boolean=true;const aPadding:TpvInt32=2):TpvSprite;
var SignedDistanceField:TpvSignedDistanceField2D;
begin
 SignedDistanceField.Pixels:=nil;
 try
  SignedDistanceField.OffsetX:=aOffsetX;
  SignedDistanceField.OffsetY:=aOffsetY;
  SignedDistanceField.Width:=aImageWidth;
  SignedDistanceField.Height:=aImageHeight;
  SetLength(SignedDistanceField.Pixels,aImageWidth*aImageHeight);
  TpvSignedDistanceField2DGenerator.Generate(SignedDistanceField,aVectorPath,aScale);
  result:=LoadRawSprite(aName,@SignedDistanceField.Pixels[0],aImageWidth,aImageHeight,aAutomaticTrim,aPadding);
  result.SignedDistanceField:=true;
 finally
  SignedDistanceField.Pixels:=nil;
 end;
end;

function TpvSpriteAtlas.LoadSignedDistanceFieldSprite(const aName,aSVGPath:TpvRawByteString;const aImageWidth,aImageHeight:TpvInt32;const aScale:TpvDouble=1.0;const aOffsetX:TpvDouble=0.0;const aOffsetY:TpvDouble=0.0;const aAutomaticTrim:boolean=true;const aPadding:TpvInt32=2):TpvSprite;
var VectorPath:TpvVectorPath;
begin
 VectorPath:=TpvVectorPath.CreateFromSVGPath(aSVGPath);
 try
  result:=LoadSignedDistanceFieldSprite(aName,VectorPath,aImageWidth,aImageHeight,aScale,aOffsetX,aOffsetY,aAutomaticTrim,aPadding);
 finally
  VectorPath.Free;
 end;
end;

function TpvSpriteAtlas.LoadSprite(const aName:TpvRawByteString;aStream:TStream;const aAutomaticTrim:boolean=true;const aPadding:TpvInt32=2):TpvSprite;
var InputImageData,ImageData:TpvPointer;
    InputImageDataSize,ImageWidth,ImageHeight:TpvInt32;
begin

 result:=nil;

 if assigned(aStream) then begin

  try

   InputImageDataSize:=aStream.Size;
   GetMem(InputImageData,InputImageDataSize);
   try

    aStream.Seek(0,soBeginning);
    aStream.Read(InputImageData^,InputImageDataSize);
    ImageData:=nil;
    try

     if LoadImage(InputImageData,InputImageDataSize,ImageData,ImageWidth,ImageHeight) then begin

      result:=LoadRawSprite(aName,ImageData,ImageWidth,ImageHeight,aAutomaticTrim,aPadding);

     end else begin
      raise Exception.Create('Can''t load image');
     end;

    finally

     if assigned(ImageData) then begin
      FreeMem(ImageData);
     end;

    end;

   finally

    if assigned(InputImageData) then begin
     FreeMem(InputImageData);
    end;

   end;

  finally

  end;

 end else begin

  raise Exception.Create('Can''t load sprite');

 end;

end;

function TpvSpriteAtlas.LoadSprites(const aName:TpvRawByteString;aStream:TStream;aSpriteWidth:TpvInt32=64;aSpriteHeight:TpvInt32=64;const aAutomaticTrim:boolean=true;const aPadding:TpvInt32=2):TpvSprites;
var InputImageData,ImageData,SpriteData:TpvPointer;
    InputImageDataSize,ImageWidth,ImageHeight,Count,x,y,sy,sw,sh:TpvInt32;
    sp,dp:PpvUInt32;
begin
 result:=nil;

 if assigned(aStream) and (aSpriteWidth>0) and (aSpriteHeight>0) then begin

  try

   InputImageDataSize:=aStream.Size;
   GetMem(InputImageData,InputImageDataSize);
   try

    aStream.Seek(0,soBeginning);
    aStream.Read(InputImageData^,InputImageDataSize);
    ImageData:=nil;
    try

     if LoadImage(InputImageData,InputImageDataSize,ImageData,ImageWidth,ImageHeight) then begin

      GetMem(SpriteData,(aSpriteWidth*aSpriteHeight)*SizeOf(TpvUInt32));
      try

       Count:=((ImageWidth+(aSpriteWidth-1)) div aSpriteWidth)*((ImageHeight+(aSpriteHeight-1)) div aSpriteHeight);
       SetLength(result,Count);

       Count:=0;

       y:=0;
       while y<ImageHeight do begin

        sh:=ImageHeight-y;
        if sh<0 then begin
         sh:=0;
        end else if sh>aSpriteHeight then begin
         sh:=aSpriteHeight;
        end;

        if sh>0 then begin

         x:=0;
         while x<ImageWidth do begin

          FillChar(SpriteData^,(aSpriteWidth*aSpriteHeight)*SizeOf(TpvUInt32),AnsiChar(#0));

          sw:=ImageWidth-x;
          if sw<0 then begin
           sw:=0;
          end else if sw>aSpriteWidth then begin
           sw:=aSpriteWidth;
          end;

          if sw>0 then begin

           sp:=ImageData;
           inc(sp,(ImageWidth*y)+x);

           dp:=SpriteData;

           for sy:=0 to sh-1 do begin
            Move(sp^,dp^,sw*SizeOf(TpvUInt32));
            inc(sp,ImageWidth);
            inc(dp,aSpriteWidth);
           end;

           result[Count]:=LoadRawSprite(aName+TpvRawByteString(IntToStr(Count)),SpriteData,aSpriteWidth,aSpriteHeight,aAutomaticTrim,aPadding);

           inc(Count);

          end else begin

           break;

          end;

          inc(x,aSpriteWidth);
         end;

        end else begin

         break;

        end;

        inc(y,aSpriteHeight);
       end;

       SetLength(result,Count);

      finally

       FreeMem(SpriteData);

      end;

     end else begin

      raise Exception.Create('Can''t load image');

     end;

    finally

     if assigned(ImageData) then begin
      FreeMem(ImageData);
     end;

    end;

   finally

    if assigned(InputImageData) then begin
     FreeMem(InputImageData);
    end;

   end;

  finally

  end;

 end else begin

  raise Exception.Create('Can''t load sprites');

 end;

end;

end.
