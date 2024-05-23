(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2024, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.FileFormats.PPM;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

{$scopedenums on}

interface

uses SysUtils,Classes,Math,PasJSON,PasVulkan.Types,PasVulkan.Math,PasVulkan.Collections;

type EpvPPM=class(Exception);

     { TpvPPM }
     TpvPPM=class
      public
       const CountFrames=16;
             CountPresetAnimations=4;
             MaximalCountVertices=16384;
             MaximalCountIndices=65536;
             Version=1;
       type TVertex=packed record
             Position:TpvVector3; // 12 bytes (must be non-quantized and non-compressed for direct use with hardware raytracing)
             TexCoordU:TpvUInt16; // 2 bytes
             TexCoordV:TpvUInt16; // 2 bytes
             TangentSpace:TpvUInt16PackedTangentSpace; // 8 bytes
            end; // 12+2+2+8 = 24 bytes
            PVertex=^TVertex;
            TVertices=array[0..MaximalCountVertices-1] of TVertex;
            TIndex=TpvUInt32;
            PIndex=^TIndex;
            TIndices=array[0..MaximalCountIndices-1] of TIndex;
            TFrame=record
             Vertices:TVertices;
            end;
            PFrame=^TFrame;
            TFrames=array[0..CountFrames-1] of TFrame;
            PFrames=^TFrames;
            TTimeFrame=record // For merging animations like grow0 grow1 grow2 etc. to grow and so on    
             Time:TpvDouble;
             Frame:TFrame;
            end;
            PTimeFrame=^TTimeFrame;
            TTimeFrames=array of TTimeFrame;
            TAnimation=record
             Frames:TFrames;
            end;
            PAnimation=^TAnimation;
            TAnimations=array[0..CountPresetAnimations-1] of TAnimation;
            TPresetAnimation=record
             Index:TpvSizeInt;
             Name:TpvUTF8String; 
            end;
            PPresetAnimation=^TPresetAnimation;
            TPresetAnimations=array[0..CountPresetAnimations-1] of TPresetAnimation;
            TSignature=array[0..3] of AnsiChar;
            TMaterialHeader=packed record
             BaseColorFactor:TpvVector4;
             EmissiveFactorOcclusionStrength:TpvVector4; // xyz = EmissiveFactor, w = OcclusionStrength
             MetallicRoughnessFactorNormalScale:TpvVector4; // x = MetallicFactor, y = RoughnessFactor, zw = Reserved
             BaseColorTextureSize:TpvUInt32;
             NormalTextureSize:TpvUInt32;
             MetallicRoughnessTextureSize:TpvUInt32;
             OcclusionTextureSize:TpvUInt32;
             EmissiveTextureSize:TpvUInt32; 
            end;
            TFileHeader=packed record
             Signature:TSignature;
             Version:TpvUInt32;
             CountVertices:TpvUInt32;
             CountIndices:TpvUInt32;
             CountFrames:TpvUInt32;
             CountAnimations:TpvUInt32;
             BoundingBoxMin:TpvVector3;
             BoundingBoxMax:TpvVector3;
             BoundingSphere:TpvVector4;
             MaterialHeader:TMaterialHeader;
            end;
            PFileHeader=^TFileHeader;            
       const Signature:TSignature=('P','P','M','F'); // Planet Plant Model File
             PresetAnimations:TPresetAnimations=(
              (Index:0;Name:'grow'),
              (Index:1;Name:'blossoms'),
              (Index:2;Name:'falloff'),
              (Index:3;Name:'wither')
             );
       type { TModel }
            TModel=class
             public
              FileHeader:TFileHeader;
              Indices:TIndices;
              Animations:TAnimations;
              BaseColorTextureData:TBytes;
              NormalTextureData:TBytes;
              MetallicRoughnessTextureData:TBytes;
              OcclusionTextureData:TBytes;
              EmissiveTextureData:TBytes;
             public
              constructor Create; reintroduce;
              destructor Destroy; override;
              procedure LoadFromStream(const aStream:TStream);
              procedure LoadFromFile(const aFileName:TpvUTF8String);
              procedure SaveToStream(const aStream:TStream);
              procedure SaveToFile(const aFileName:TpvUTF8String);
            end;
      private
 
    end;

implementation

{ TpvPPM.TModel }

constructor TpvPPM.TModel.Create;
begin
 inherited Create;
 FillChar(FileHeader,SizeOf(TpvPPM.TFileHeader),#0);
 FillChar(Indices,SizeOf(TpvPPM.TIndices),#0);
 FillChar(Animations,SizeOf(TpvPPM.TAnimations),#0);
 BaseColorTextureData:=nil;
 NormalTextureData:=nil;
 MetallicRoughnessTextureData:=nil;
 OcclusionTextureData:=nil;
 EmissiveTextureData:=nil;
end;

destructor TpvPPM.TModel.Destroy;
begin
 BaseColorTextureData:=nil;
 NormalTextureData:=nil;
 MetallicRoughnessTextureData:=nil;
 OcclusionTextureData:=nil;
 EmissiveTextureData:=nil;
 inherited Destroy;
end;

procedure TpvPPM.TModel.LoadFromStream(const aStream:TStream);
var AnimationIndex,FramesIndex,VerticesIndex:TpvInt32;
begin

 FillChar(FileHeader,SizeOf(TpvPPM.TFileHeader),#0);
 FillChar(Indices,SizeOf(TpvPPM.TIndices),#0);
 FillChar(Animations,SizeOf(TpvPPM.TAnimations),#0);

 aStream.ReadBuffer(FileHeader,SizeOf(TpvPPM.TFileHeader));
 
 if FileHeader.Signature<>TpvPPM.Signature then begin
  raise EpvPPM.Create('Invalid PPM signature');
 end;

 if FileHeader.Version<>TpvPPM.Version then begin
  raise EpvPPM.Create('Invalid or not supported PPM version');
 end;

 if FileHeader.CountIndices>0 then begin
  aStream.ReadBuffer(Indices[0],SizeOf(TpvPPM.TIndex)*FileHeader.CountIndices);
 end;

 for AnimationIndex:=0 to FileHeader.CountAnimations-1 do begin
  for FramesIndex:=0 to FileHeader.CountFrames-1 do begin
   for VerticesIndex:=0 to FileHeader.CountVertices-1 do begin
    aStream.ReadBuffer(Animations[AnimationIndex].Frames[FramesIndex].Vertices[VerticesIndex],SizeOf(TpvPPM.TVertex));
   end;
  end;
 end;

 if FileHeader.MaterialHeader.BaseColorTextureSize>0 then begin
  SetLength(BaseColorTextureData,FileHeader.MaterialHeader.BaseColorTextureSize);
  aStream.ReadBuffer(BaseColorTextureData[0],FileHeader.MaterialHeader.BaseColorTextureSize);
 end else begin
  BaseColorTextureData:=nil;
 end;

 if FileHeader.MaterialHeader.NormalTextureSize>0 then begin
  SetLength(NormalTextureData,FileHeader.MaterialHeader.NormalTextureSize);
  aStream.ReadBuffer(NormalTextureData[0],FileHeader.MaterialHeader.NormalTextureSize);
 end else begin
  NormalTextureData:=nil;
 end;

 if FileHeader.MaterialHeader.MetallicRoughnessTextureSize>0 then begin
  SetLength(MetallicRoughnessTextureData,FileHeader.MaterialHeader.MetallicRoughnessTextureSize);
  aStream.ReadBuffer(MetallicRoughnessTextureData[0],FileHeader.MaterialHeader.MetallicRoughnessTextureSize);
 end else begin
  MetallicRoughnessTextureData:=nil;
 end;

 if FileHeader.MaterialHeader.OcclusionTextureSize>0 then begin
  SetLength(OcclusionTextureData,FileHeader.MaterialHeader.OcclusionTextureSize);
  aStream.ReadBuffer(OcclusionTextureData[0],FileHeader.MaterialHeader.OcclusionTextureSize);
 end else begin
  OcclusionTextureData:=nil;
 end;

 if FileHeader.MaterialHeader.EmissiveTextureSize>0 then begin
  SetLength(EmissiveTextureData,FileHeader.MaterialHeader.EmissiveTextureSize);
  aStream.ReadBuffer(EmissiveTextureData[0],FileHeader.MaterialHeader.EmissiveTextureSize);
 end else begin
  EmissiveTextureData:=nil;
 end;

end;

procedure TpvPPM.TModel.LoadFromFile(const aFileName:TpvUTF8String);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 try
  Stream.LoadFromFile(aFileName);
  Stream.Seek(0,soBeginning);
  LoadFromStream(Stream);
 finally
  Stream.Free;
 end;
end;

procedure TpvPPM.TModel.SaveToStream(const aStream:TStream);
var AnimationIndex,FramesIndex,VerticesIndex:TpvInt32;
begin
 FileHeader.Signature:=TpvPPM.Signature;
 FileHeader.Version:=TpvPPM.Version;
 aStream.WriteBuffer(FileHeader,SizeOf(TpvPPM.TFileHeader));
 aStream.WriteBuffer(Indices[0],SizeOf(TpvPPM.TIndex)*FileHeader.CountIndices);
 for AnimationIndex:=0 to FileHeader.CountAnimations-1 do begin
  for FramesIndex:=0 to FileHeader.CountFrames-1 do begin
   aStream.WriteBuffer(Animations[AnimationIndex].Frames[FramesIndex].Vertices[0],SizeOf(TpvPPM.TVertex)*FileHeader.CountVertices);
  end;
 end;
 if length(BaseColorTextureData)>0 then begin
  aStream.WriteBuffer(BaseColorTextureData[0],length(BaseColorTextureData));
 end;
 if length(NormalTextureData)>0 then begin
  aStream.WriteBuffer(NormalTextureData[0],length(NormalTextureData));
 end;
 if length(MetallicRoughnessTextureData)>0 then begin
  aStream.WriteBuffer(MetallicRoughnessTextureData[0],length(MetallicRoughnessTextureData));
 end;
 if length(OcclusionTextureData)>0 then begin
  aStream.WriteBuffer(OcclusionTextureData[0],length(OcclusionTextureData));
 end;
 if length(EmissiveTextureData)>0 then begin
  aStream.WriteBuffer(EmissiveTextureData[0],length(EmissiveTextureData));
 end;
end;

procedure TpvPPM.TModel.SaveToFile(const aFileName:TpvUTF8String);
var Stream:TMemoryStream;
begin
 Stream:=TMemoryStream.Create;
 try
  SaveToStream(Stream);
  Stream.Seek(0,soBeginning);
  Stream.SaveToFile(aFileName);
 finally
  Stream.Free;
 end;
end;

end.
