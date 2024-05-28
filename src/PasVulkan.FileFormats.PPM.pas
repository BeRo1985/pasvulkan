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
       const CountPresetAnimations=4;
             Version=1;
       type TVertex=packed record
             Position:TpvVector3; // 12 bytes (must be non-quantized and non-compressed for direct use with hardware raytracing)
             TangentSpace:TpvUInt32; // 4 bytes (RGB10A2 snorm)
            end; // 12+4 = 16 bytes
            PVertex=^TVertex;
            TVertices=array of TVertex;
            TFullVertex=record
             Position:TpvVector3;
             Tangent:TpvVector3; 
             Bitangent:TpvVector3;
             Normal:TpvVector3;
            end; // 12+12+12+12 = 48 bytes
            PFullVertex=^TFullVertex;
            TFullVertices=array of TFullVertex;
            TTexCoords=array of TpvUInt16Vector2;
            TIndex=TpvUInt32;
            PIndex=^TIndex;
            TIndices=array of TIndex;
            { TFrame }
            TFrame=record
             Time:TpvDouble;
             Vertices:TVertices;
             FullVertices:TFullVertices;
             procedure Pack;
             procedure Unpack;
            end;
            PFrame=^TFrame;
            TFrames=array of TFrame;
            TAnimation=record
             Frames:TFrames;
            end;
            PAnimation=^TAnimation;
            TAnimations=array of TAnimation;
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
              TexCoords:TTexCoords;
              Indices:TIndices;
              Animations:TAnimations;
              BaseColorTextureStream:TMemoryStream;
              NormalTextureStream:TMemoryStream;
              MetallicRoughnessTextureStream:TMemoryStream;
              OcclusionTextureStream:TMemoryStream;
              EmissiveTextureStream:TMemoryStream;
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

{ TpvPPM.TFrame }

procedure TpvPPM.TFrame.Pack;
var Index:TpvSizeInt;
    FullVertex:PFullVertex;
    Vertex:PVertex;
begin
 if length(Vertices)<>length(FullVertices) then begin
  SetLength(Vertices,length(FullVertices));
 end;
 for Index:=0 to length(Vertices)-1 do begin
  Vertex:=@Vertices[Index];
  FullVertex:=@FullVertices[Index];
  Vertex^.Position:=FullVertex^.Position;
  Vertex^.TangentSpace:=EncodeTangentSpaceAsRGB10A2SNorm(FullVertex^.Tangent,FullVertex^.Bitangent,FullVertex^.Normal);
 end;
end;

procedure TpvPPM.TFrame.Unpack;
var Index:TpvSizeInt;
    FullVertex:PFullVertex;
    Vertex:PVertex;
begin
 if length(FullVertices)<>length(Vertices) then begin
  SetLength(FullVertices,length(Vertices));
 end;
 for Index:=0 to length(FullVertices)-1 do begin
  Vertex:=@Vertices[Index];
  FullVertex:=@FullVertices[Index];
  FullVertex^.Position:=Vertex^.Position;
  DecodeTangentSpaceFromRGB10A2SNorm(Vertex^.TangentSpace,FullVertex^.Tangent,FullVertex^.Bitangent,FullVertex^.Normal);
 end;
end;

{ TpvPPM.TModel }

constructor TpvPPM.TModel.Create;
begin
 inherited Create;
 FillChar(FileHeader,SizeOf(TpvPPM.TFileHeader),#0);
 TexCoords:=nil;
 Indices:=nil;
 Animations:=nil;
 BaseColorTextureStream:=TMemoryStream.Create;
 NormalTextureStream:=TMemoryStream.Create;
 MetallicRoughnessTextureStream:=TMemoryStream.Create;
 OcclusionTextureStream:=TMemoryStream.Create;
 EmissiveTextureStream:=TMemoryStream.Create;
end;

destructor TpvPPM.TModel.Destroy;
begin
 TexCoords:=nil;
 Indices:=nil;
 Animations:=nil;
 FreeAndNil(EmissiveTextureStream);
 FreeAndNil(OcclusionTextureStream);
 FreeAndNil(MetallicRoughnessTextureStream);
 FreeAndNil(NormalTextureStream);
 FreeAndNil(BaseColorTextureStream);
 inherited Destroy;
end;

procedure TpvPPM.TModel.LoadFromStream(const aStream:TStream);
var AnimationIndex,FramesIndex:TpvSizeInt;
    CountFrames:TpvInt32;
begin

 TexCoords:=nil;
 Indices:=nil;
 Animations:=nil;

 FillChar(FileHeader,SizeOf(TpvPPM.TFileHeader),#0);

 aStream.ReadBuffer(FileHeader,SizeOf(TpvPPM.TFileHeader));
 
 if FileHeader.Signature<>TpvPPM.Signature then begin
  raise EpvPPM.Create('Invalid PPM signature');
 end;

 if FileHeader.Version<>TpvPPM.Version then begin
  raise EpvPPM.Create('Invalid or not supported PPM version');
 end;

 if FileHeader.CountIndices>0 then begin
  SetLength(Indices,FileHeader.CountIndices);
  aStream.ReadBuffer(Indices[0],SizeOf(TpvPPM.TIndex)*FileHeader.CountIndices);
 end;

 if FileHeader.CountVertices>0 then begin
  SetLength(TexCoords,FileHeader.CountVertices);
  aStream.ReadBuffer(TexCoords[0],SizeOf(TpvUInt16Vector2)*FileHeader.CountVertices);
 end;

 SetLength(Animations,FileHeader.CountAnimations);
 for AnimationIndex:=0 to FileHeader.CountAnimations-1 do begin
  aStream.ReadBuffer(CountFrames,SizeOf(TpvInt32));
  Animations[AnimationIndex].Frames:=nil;
  if CountFrames>0 then begin
   SetLength(Animations[AnimationIndex].Frames,CountFrames);
   for FramesIndex:=0 to CountFrames-1 do begin
    aStream.ReadBuffer(Animations[AnimationIndex].Frames[FramesIndex].Time,SizeOf(TpvDouble));
    SetLength(Animations[AnimationIndex].Frames[FramesIndex].Vertices,FileHeader.CountVertices);
    if FileHeader.CountVertices>0 then begin
     aStream.ReadBuffer(Animations[AnimationIndex].Frames[FramesIndex].Vertices[0],SizeOf(TpvPPM.TVertex)*FileHeader.CountVertices);
    end;
   end;
  end;
 end;

 BaseColorTextureStream.Clear;
 if FileHeader.MaterialHeader.BaseColorTextureSize>0 then begin
  BaseColorTextureStream.CopyFrom(aStream,FileHeader.MaterialHeader.BaseColorTextureSize);
 end;

 NormalTextureStream.Clear;
 if FileHeader.MaterialHeader.NormalTextureSize>0 then begin
  NormalTextureStream.CopyFrom(aStream,FileHeader.MaterialHeader.NormalTextureSize);
 end;

 MetallicRoughnessTextureStream.Clear;
 if FileHeader.MaterialHeader.MetallicRoughnessTextureSize>0 then begin
  MetallicRoughnessTextureStream.CopyFrom(aStream,FileHeader.MaterialHeader.MetallicRoughnessTextureSize);
 end;

 OcclusionTextureStream.Clear;
 if FileHeader.MaterialHeader.OcclusionTextureSize>0 then begin
  OcclusionTextureStream.CopyFrom(aStream,FileHeader.MaterialHeader.OcclusionTextureSize);
 end;

 EmissiveTextureStream.Clear;
 if FileHeader.MaterialHeader.EmissiveTextureSize>0 then begin
  EmissiveTextureStream.CopyFrom(aStream,FileHeader.MaterialHeader.EmissiveTextureSize);
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
var AnimationIndex,FramesIndex:TpvSizeInt;
    CountFrames:TpvInt32;
begin
 FileHeader.Signature:=TpvPPM.Signature;
 FileHeader.Version:=TpvPPM.Version;
 FileHeader.CountAnimations:=length(Animations);
 aStream.WriteBuffer(FileHeader,SizeOf(TpvPPM.TFileHeader));
 aStream.WriteBuffer(Indices[0],SizeOf(TpvPPM.TIndex)*FileHeader.CountIndices);
 aStream.WriteBuffer(TexCoords[0],SizeOf(TpvUInt16Vector2)*FileHeader.CountVertices);
 for AnimationIndex:=0 to length(Animations)-1 do begin
  CountFrames:=length(Animations[AnimationIndex].Frames);
  aStream.WriteBuffer(CountFrames,SizeOf(TpvInt32));
  for FramesIndex:=0 to CountFrames-1 do begin
   aStream.WriteBuffer(Animations[AnimationIndex].Frames[FramesIndex].Time,SizeOf(TpvDouble));
   aStream.WriteBuffer(Animations[AnimationIndex].Frames[FramesIndex].Vertices[0],SizeOf(TpvPPM.TVertex)*FileHeader.CountVertices);
  end;
 end;
 if BaseColorTextureStream.Size>0 then begin
  BaseColorTextureStream.Seek(0,soBeginning);
  aStream.CopyFrom(BaseColorTextureStream,BaseColorTextureStream.Size);
 end;
 if NormalTextureStream.Size>0 then begin
  NormalTextureStream.Seek(0,soBeginning);
  aStream.CopyFrom(NormalTextureStream,NormalTextureStream.Size);
 end;
 if MetallicRoughnessTextureStream.Size>0 then begin
  MetallicRoughnessTextureStream.Seek(0,soBeginning);
  aStream.CopyFrom(MetallicRoughnessTextureStream,MetallicRoughnessTextureStream.Size);
 end;
 if OcclusionTextureStream.Size>0 then begin
  OcclusionTextureStream.Seek(0,soBeginning);
  aStream.CopyFrom(OcclusionTextureStream,OcclusionTextureStream.Size);
 end;
 if EmissiveTextureStream.Size>0 then begin
  EmissiveTextureStream.Seek(0,soBeginning);
  aStream.CopyFrom(EmissiveTextureStream,EmissiveTextureStream.Size);
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
