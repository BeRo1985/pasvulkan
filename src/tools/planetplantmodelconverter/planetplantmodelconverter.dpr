program planetplantmodelconverter;
{$ifdef fpc}
 {$mode delphi}
{$endif}

uses Classes,
     SysUtils,
     Math,
     PUCU in '../../../externals/pucu/src/PUCU.pas',
     PasMP in '../../../externals/pasmp/src/PasMP.pas',
     PasDblStrUtils in '../../../externals/pasdblstrutils/src/PasDblStrUtils.pas',
     PasJSON in '../../../externals/pasjson/src/PasJSON.pas',
     PasGLTF in '../../../externals/pasgltf/src/PasGLTF.pas',
     Vulkan in '../../Vulkan.pas',
     PasVulkan.Types in '../../PasVulkan.Types.pas',
     PasVulkan.Utils in '../../PasVulkan.Utils.pas',
     PasVulkan.Math in '../../PasVulkan.Math.pas',
     PasVulkan.Collections in '../../PasVulkan.Collections.pas',
     PasVulkan.FileFormats.GLTF in '../../PasVulkan.FileFormats.GLTF.pas';

const CountFrames=16;

      CountPresetAnimations=4;

      MaximalCountVertices=16384;

      MaximalCountIndices=65536;

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

     TFileHeader=packed record
      Signature:TSignature;
      Version:TpvUInt32;
      CountVertices:TpvUInt32;
      CountIndices:TpvUInt32;
      CountFrames:TpvUInt32;
      CountAnimations:TpvUInt32;
     end;
     PFileHeader=^TFileHeader;
     
const Signature:TSignature=('P','P','M','F'); // Planet Plant Model File

      PresetAnimations:TPresetAnimations=(
       (Index:0;Name:'grow'),
       (Index:1;Name:'blossoms'),
       (Index:2;Name:'falloff'),
       (Index:3;Name:'wither')
      );

var Animations:TAnimations;

    Indices:TIndices;

    CountUsedVertices:TpvSizeInt;
    CountUsedIndices:TpvSizeInt;

    GLTF:TpvGLTF;
    GLTFInstance:TpvGLTF.TInstance;

    FileHeader:TFileHeader;

    BaseColorTextureData:TBytes;
    NormalTextureData:TBytes;
    MetallicRoughnessTextureData:TBytes;
    OcclusionTextureData:TBytes;
    EmissiveTextureData:TBytes;
    
function CompareTimeFrames(const a,b:TTimeFrame):TpvInt32;
begin
 result:=Sign(a.Time-b.Time);
end;

function ConvertTimeFramesToNormalizedFrames(var aTimeFrames:TTimeFrames;out aFrames:TFrames):boolean;
var FrameIndex,CurrentFrameIndex,NextFrameIndex,OutFrameIndex,VertexIndex:TpvSizeInt;
    StartTime,EndTime,Time,TimeStep,InterpolationFactor:TpvDouble;
    CurrentFrame,NextFrame:PFrame;
    CurrentVertex,NextVertex,InterpolatedVertex:PVertex;
    CurrentTangent,CurrentBitangent,CurrentNormal,
    NextTangent,NextBitangent,NextNormal,
    InterpolatedTangent,InterpolatedBitangent,InterpolatedNormal:TpvVector3;
    CurrentTangentSpace,NextTangentSpace,InterpolatedTangentSpace:TpvMatrix3x3;
begin

 if length(aTimeFrames)=0 then begin
  result:=false;
  exit;
 end;

 // Sort time frames
 TpvTypedSort<TTimeFrame>.IntroSort(@aTimeFrames[0],0,length(aTimeFrames),CompareTimeFrames);

 // Find start and end time
 StartTime:=aTimeFrames[0].Time;
 EndTime:=aTimeFrames[length(aTimeFrames)-1].Time;

 // Calculate time step
 TimeStep:=(EndTime-StartTime)/CountFrames;

 // Normalize time frames
 Time:=StartTime;
 FrameIndex:=0;
 OutFrameIndex:=0;
 while (Time<(EndTime+TimeStep)) and (OutFrameIndex<CountFrames) do begin

  // Clear frame
  FillChar(aFrames[OutFrameIndex],SizeOf(TFrame),#0);  

  // Advance to next frame
  while (FrameIndex<length(aTimeFrames)) and (aTimeFrames[FrameIndex].Time<Time) do begin
   inc(FrameIndex);
  end;

  // Get current and next frame indices
  CurrentFrameIndex:=FrameIndex; 
  NextFrameIndex:=Min(CurrentFrameIndex+1,length(aTimeFrames)-1);

  // Get current and next frame pointers  
  CurrentFrame:=@aTimeFrames[CurrentFrameIndex].Frame;
  NextFrame:=@aTimeFrames[NextFrameIndex].Frame;

  // Interpolate between current and next frame
  InterpolationFactor:=(Time-aTimeFrames[CurrentFrameIndex].Time)/(aTimeFrames[NextFrameIndex].Time-aTimeFrames[CurrentFrameIndex].Time);
  for VertexIndex:=0 to CountUsedVertices-1 do begin
   
   CurrentVertex:=@CurrentFrame^.Vertices[VertexIndex];
   NextVertex:=@NextFrame^.Vertices[VertexIndex];

   InterpolatedVertex:=@aFrames[OutFrameIndex].Vertices[VertexIndex];
   
   InterpolatedVertex^.Position:=CurrentVertex^.Position.Lerp(NextVertex^.Position,InterpolationFactor);

   InterpolatedVertex^.TexCoordU:=Min(Max(round((CurrentVertex^.TexCoordU*(1.0-InterpolationFactor))+(NextVertex^.TexCoordU*InterpolationFactor)),0),65535);
   InterpolatedVertex^.TexCoordV:=Min(Max(round((CurrentVertex^.TexCoordV*(1.0-InterpolationFactor))+(NextVertex^.TexCoordV*InterpolationFactor)),0),65535);
   
   UnpackUInt16QTangentSpace(CurrentVertex^.TangentSpace,CurrentTangent,CurrentBitangent,CurrentNormal);
   CurrentTangentSpace:=TpvMatrix3x3.Create(CurrentTangent,CurrentBitangent,CurrentNormal);
   
   UnpackUInt16QTangentSpace(NextVertex^.TangentSpace,NextTangent,NextBitangent,NextNormal);
   NextTangentSpace:=TpvMatrix3x3.Create(NextTangent,NextBitangent,NextNormal);

   InterpolatedTangentSpace:=CurrentTangentSpace.Slerp(NextTangentSpace,InterpolationFactor);

   InterpolatedTangent:=InterpolatedTangentSpace.Tangent;
   InterpolatedBitangent:=InterpolatedTangentSpace.Bitangent;
   InterpolatedNormal:=InterpolatedTangentSpace.Normal;

   InterpolatedVertex^.TangentSpace:=PackUInt16QTangentSpace(InterpolatedTangent,InterpolatedBitangent,InterpolatedNormal);
   
  end; 

  Time:=Time+TimeStep; 

  inc(OutFrameIndex);

 end;

end;

function GetMergedAnimation(const aPresetAnimationIndex:TpvSizeInt):TTimeFrames;
const CountNonNormalizedFrames=128; // 128 frames as non normalized frames before normalization into fewer frames
var AnimationPart,Index,FrameIndex,OtherIndex,FoundPresetAnimation,BaseIndex,
    TriangleIndex,VertexIndex:TpvSizeInt;
    ta,tb,t:TpvDouble;
    PresetAnimationName,AnimationName:TpvUTF8String;
    FoundAnimationPart:boolean;
    GLTFBakedVertexIndexedMesh:TpvGLTF.TBakedVertexIndexedMesh;
    GLTFBakedVertexIndexedMeshVertex:TpvGLTF.PVertex;
    PartFrames:TTimeFrames;
    Vertex:TVertex;
begin

 result:=nil; // Nothing yet

 if length(GLTF.Animations)>0 then begin

  PresetAnimationName:=PresetAnimations[aPresetAnimationIndex].Name;

  // Limit the possible parts to the count of animations in the GLTF file, -1 = without number postfix 
  for AnimationPart:=-1 to length(GLTF.Animations)-1 do begin 

   FoundAnimationPart:=false;

   for Index:=0 to length(GLTF.Animations)-1 do begin

    AnimationName:=Trim(LowerCase(GLTF.Animations[Index].Name));

    if ((AnimationPart<0) and (AnimationName=PresetAnimationName)) or 
       ((AnimationPart>=0) and (AnimationName=PresetAnimationName+IntToStr(AnimationPart))) then begin

     FoundAnimationPart:=true;

     PartFrames:=nil;
     try

      SetLength(PartFrames,CountNonNormalizedFrames);

      for FrameIndex:=0 to CountNonNormalizedFrames-1 do begin

       t:=FrameIndex/(CountNonNormalizedFrames-1);

       GLTFInstance.Animation:=Index;

       ta:=GLTF.GetAnimationBeginTime(Index);
       tb:=GLTF.GetAnimationEndTime(Index);

       GLTFInstance.AnimationTime:=(ta*(1.0-t))+(tb*t);

       GLTFInstance.Update;

       result[FrameIndex].Time:=GLTFInstance.AnimationTime; // because non normalized

       FillChar(result[FrameIndex].Frame,SizeOf(TFrame),#0);

       GLTFBakedVertexIndexedMesh:=GLTFInstance.GetBakedVertexIndexedMesh(false,true,-1,[TPasGLTF.TMaterial.TAlphaMode.Opaque,TPasGLTF.TMaterial.TAlphaMode.Blend,TPasGLTF.TMaterial.TAlphaMode.Mask]);
       if assigned(GLTFBakedVertexIndexedMesh) then begin
        try

         for VertexIndex:=0 to GLTFBakedVertexIndexedMesh.Vertices.Count-1 do begin

          GLTFBakedVertexIndexedMeshVertex:=@GLTFBakedVertexIndexedMesh.Vertices.ItemArray[VertexIndex];

          Vertex.Position:=TpvVector3.Create(GLTFBakedVertexIndexedMeshVertex^.Position[0],GLTFBakedVertexIndexedMeshVertex^.Position[1],GLTFBakedVertexIndexedMeshVertex^.Position[2]);

          Vertex.TexCoordU:=Min(Max(round(GLTFBakedVertexIndexedMeshVertex^.TexCoord0[0]*16384.0),0),65535);
          Vertex.TexCoordV:=Min(Max(round(GLTFBakedVertexIndexedMeshVertex^.TexCoord0[1]*16384.0),0),65535);

          Vertex.TangentSpace:=PackUInt16QTangentSpace(
                                TpvVector3.Create(GLTFBakedVertexIndexedMeshVertex^.Tangent[0],GLTFBakedVertexIndexedMeshVertex^.Tangent[1],GLTFBakedVertexIndexedMeshVertex^.Tangent[2]),
                                (TpvVector3.Create(GLTFBakedVertexIndexedMeshVertex^.Normal[0],GLTFBakedVertexIndexedMeshVertex^.Normal[1],GLTFBakedVertexIndexedMeshVertex^.Normal[2]).Cross(TpvVector3.Create(GLTFBakedVertexIndexedMeshVertex^.Tangent[0],GLTFBakedVertexIndexedMeshVertex^.Tangent[1],GLTFBakedVertexIndexedMeshVertex^.Tangent[2])))*GLTFBakedVertexIndexedMeshVertex^.Tangent[3],
                                TpvVector3.Create(GLTFBakedVertexIndexedMeshVertex^.Normal[0],GLTFBakedVertexIndexedMeshVertex^.Normal[1],GLTFBakedVertexIndexedMeshVertex^.Normal[2])
                               );

          result[FrameIndex].Frame.Vertices[VertexIndex]:=Vertex;

         end;

        finally
         FreeAndNil(GLTFBakedVertexIndexedMesh);
        end;
       end;

      end;

      // Append frames
      BaseIndex:=length(result);
      SetLength(result,length(result)+CountNonNormalizedFrames);
      for FrameIndex:=0 to CountNonNormalizedFrames-1 do begin
       result[BaseIndex+FrameIndex]:=PartFrames[FrameIndex];
      end;

     finally
      PartFrames:=nil;
     end; 

     break;

    end;

   end;

  end;

 end;    

end;

function ConvertModel(const aInputFileName,aOutputFileName:String):boolean;
var Index,FrameIndex,OtherIndex,FoundPresetAnimation,
    BaseColorTextureIndex,NormalTextureIndex,MetallicRoughnessTextureIndex,
    OcclusionTextureIndex,EmissiveTextureIndex,
    ImageIndex:TpvSizeInt;
    GLTFBakedVertexIndexedMesh:TpvGLTF.TBakedVertexIndexedMesh;
    ta,tb,t:TpvDouble;
    AnimationName:TpvUTF8String;
    TimeFrames:TTimeFrames;
    Material:TpvGLTF.PMaterial;
    Stream:TMemoryStream;
    BaseColorFactor:TpvVector4;
    MetallicRoughnessFactor:TpvVector2;
    OcclusionStrength:TpvFloat;
    EmissiveFactor:TpvVector3;
begin

 result:=true;

 FillChar(Animations,SizeOf(TAnimations),#0);

 GLTF:=TpvGLTF.Create;
 try

  GLTF.LoadFromFile(aInputFileName);

  GLTFInstance:=GLTF.AcquireInstance;
  try

   if length(GLTF.Animations)>0 then begin

    // Get indices for all animations once for all in advance
    begin

     GLTFInstance.Animation:=-1;
     GLTFInstance.AnimationTime:=0;
     GLTFInstance.Update;

     GLTFBakedVertexIndexedMesh:=GLTFInstance.GetBakedVertexIndexedMesh(false,true,-1,[TPasGLTF.TMaterial.TAlphaMode.Opaque,TPasGLTF.TMaterial.TAlphaMode.Blend,TPasGLTF.TMaterial.TAlphaMode.Mask]);
     if assigned(GLTFBakedVertexIndexedMesh) then begin
      try
       CountUsedVertices:=GLTFBakedVertexIndexedMesh.Vertices.Count;
       CountUsedIndices:=GLTFBakedVertexIndexedMesh.Indices.Count;
       if (CountUsedVertices<=MaximalCountVertices) and (CountUsedIndices<=MaximalCountIndices) then begin
        for Index:=0 to GLTFBakedVertexIndexedMesh.Indices.Count-1 do begin
         Indices[Index]:=GLTFBakedVertexIndexedMesh.Indices.ItemArray[Index];
        end;
       end else begin 
        if CountUsedVertices>MaximalCountVertices then begin
         if CountUsedIndices>MaximalCountIndices then begin
          WriteLn('Error: Too many vertices and indices!');
         end else begin 
          WriteLn('Error: Too many vertices!');
         end;
        end else if CountUsedIndices>MaximalCountIndices then begin
         WriteLn('Error: Too many indices!');
        end;
        result:=false;
       end;         
      finally
       FreeAndNil(GLTFBakedVertexIndexedMesh);
      end;
     end else begin
      WriteLn('Error: No vertex indexed mesh found!');
      result:=false;
     end;

    end;

    if result then begin

     // Get PBR textures
     BaseColorTextureIndex:=-1;
     NormalTextureIndex:=-1;
     MetallicRoughnessTextureIndex:=-1;
     OcclusionTextureIndex:=-1;
     EmissiveTextureIndex:=-1;
     BaseColorFactor:=TpvVector4.Create(1.0,1.0,1.0,1.0);
     MetallicRoughnessFactor:=TpvVector2.Create(1.0,1.0);
     OcclusionStrength:=1.0;
     EmissiveFactor:=TpvVector3.Create(1.0,1.0,1.0);
     if length(GLTF.Materials)>0 then begin
      Material:=@GLTF.Materials[0];
      if Material^.ShadingModel=TpvGLTF.TMaterial.TShadingModel.PBRMetallicRoughness then begin
       BaseColorFactor:=TpvVector4(Pointer(@Material^.PBRMetallicRoughness.BaseColorFactor)^);
       BaseColorTextureIndex:=Material^.PBRMetallicRoughness.BaseColorTexture.Index;
       NormalTextureIndex:=Material^.NormalTexture.Index;
       MetallicRoughnessFactor:=TpvVector2.Create(Material^.PBRMetallicRoughness.MetallicFactor,Material^.PBRMetallicRoughness.RoughnessFactor);
       MetallicRoughnessTextureIndex:=Material^.PBRMetallicRoughness.MetallicRoughnessTexture.Index;
       OcclusionTextureIndex:=Material^.OcclusionTexture.Index;
       OcclusionStrength:=Material^.OcclusionTextureStrength;
       EmissiveTextureIndex:=Material^.EmissiveTexture.Index;
       EmissiveFactor:=TpvVector3(Pointer(@Material^.EmissiveFactor)^);
       if BaseColorTextureIndex>=0 then begin
        ImageIndex:=GLTF.Textures[BaseColorTextureIndex].Image;
        if ImageIndex>=0 then begin
         BaseColorTextureData:=GLTF.Images[ImageIndex].Data;
        end;
       end;
       if NormalTextureIndex>=0 then begin
        ImageIndex:=GLTF.Textures[NormalTextureIndex].Image;
        if ImageIndex>=0 then begin
         NormalTextureData:=GLTF.Images[ImageIndex].Data;
        end;
       end;
       if MetallicRoughnessTextureIndex>=0 then begin
        ImageIndex:=GLTF.Textures[MetallicRoughnessTextureIndex].Image;
        if ImageIndex>=0 then begin
         MetallicRoughnessTextureData:=GLTF.Images[ImageIndex].Data;
        end;
       end;
       if OcclusionTextureIndex>=0 then begin
        ImageIndex:=GLTF.Textures[OcclusionTextureIndex].Image;
        if ImageIndex>=0 then begin
         OcclusionTextureData:=GLTF.Images[ImageIndex].Data;
        end;
       end;
       if EmissiveTextureIndex>=0 then begin
        ImageIndex:=GLTF.Textures[EmissiveTextureIndex].Image;
        if ImageIndex>=0 then begin
         EmissiveTextureData:=GLTF.Images[ImageIndex].Data;
        end;
       end;
      end else begin
       WriteLn('Error: No PBR metallic roughness material found!');
       result:=false;
      end;
     end else begin
      WriteLn('Error: No material found!');
      result:=false;
     end;

    end;

    if result then begin

     // Get all animations
     for Index:=0 to CountPresetAnimations-1 do begin
      TimeFrames:=GetMergedAnimation(Index);
      if length(TimeFrames)>0 then begin
       try
        ConvertTimeFramesToNormalizedFrames(TimeFrames,Animations[Index].Frames);
       finally
        TimeFrames:=nil;
       end;
      end else begin
       // Use last frame of the first animation as fallback 
       for FrameIndex:=0 to CountFrames-1 do begin
        Animations[Index].Frames[FrameIndex]:=Animations[0].Frames[CountFrames-1];
       end;
      end;
     end;

     // Save
     Stream:=TMemoryStream.Create;
     try

      FileHeader.Signature:=Signature;
      FileHeader.Version:=1;
      FileHeader.CountVertices:=CountUsedVertices;
      FileHeader.CountIndices:=CountUsedIndices;
      FileHeader.CountFrames:=CountFrames;
      FileHeader.CountAnimations:=CountPresetAnimations;

      Stream.WriteBuffer(FileHeader,SizeOf(TFileHeader));

      Stream.WriteBuffer(Indices[0],SizeOf(TIndex)*CountUsedIndices);

      for Index:=0 to CountPresetAnimations-1 do begin
       for FrameIndex:=0 to CountFrames-1 do begin
        Stream.WriteBuffer(Animations[Index].Frames[FrameIndex].Vertices[0],SizeOf(TVertex)*CountUsedVertices);
       end;
      end;

      Stream.SaveToFile(aOutputFileName);

     finally
      FreeAndNil(Stream);
     end;

    end;
   
   end;

  finally
   FreeAndNil(GLTFInstance);
  end;

 finally
  FreeAndNil(GLTF);
 end;

end;

var Index:TpvSizeInt;
    Parameter,InputFileName,OutputFileName:String;
begin

 if ParamCount=0 then begin
  writeln('Usage: ',ExtractFileName(ParamStr(0)),' <input file name> <output file name>');
  halt(1);
 end;

 InputFileName:='';
 OutputFileName:='';

 for Index:=1 to ParamCount do begin
  Parameter:=ParamStr(Index);
  if length(Parameter)>0 then begin
   case Parameter[1] of
    '-':begin
    end;
    '/':begin
    end;
    else begin
     if length(InputFileName)=0 then begin
      InputFileName:=Parameter;
     end else if length(OutputFileName)=0 then begin
      OutputFileName:=Parameter;
     end;
    end;
   end;
  end;
 end;

 if (length(InputFileName)=0) or not FileExists(InputFileName) then begin
  writeln('Error: No input file name specified or input file name not found!');
  halt(1);
 end;

 if length(OutputFileName)=0 then begin
  writeln('Error: No output file name specified!');
  halt(1);
 end;

 BaseColorTextureData:=nil;
 NormalTextureData:=nil;
 MetallicRoughnessTextureData:=nil;
 OcclusionTextureData:=nil;
 EmissiveTextureData:=nil;
 
 ConvertModel(InputFileName,OutputFileName);

 BaseColorTextureData:=nil;
 NormalTextureData:=nil;
 MetallicRoughnessTextureData:=nil;
 OcclusionTextureData:=nil;
 EmissiveTextureData:=nil;
 
end.


