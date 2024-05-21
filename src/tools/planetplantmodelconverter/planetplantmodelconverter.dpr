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
      Position:TpvVector3; // 12 bytes
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

const PresetAnimations:TPresetAnimations=(
       (Index:0;Name:'grow'),
       (Index:1;Name:'blossoms'),
       (Index:2;Name:'falloff'),
       (Index:3;Name:'wither')
      );

var Animations:TAnimations;

    Indices:TIndices;

    CountUsedVertices:TpvSizeInt;

    InterpolatedFrame:TFrame;

    GLTF:TpvGLTF;
    GLTFInstance:TpvGLTF.TInstance;

function ConvertTimeFramesToNormalizedFrames(const aTimeFrames:TTimeFrames;out aFrames:TFrames):boolean;
var Index,OtherIndex,FrameIndex,NextFrameIndex,OutFrameIndex,VertexIndex:TpvSizeInt;
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

  // Advance to next frame
  while (FrameIndex<length(aTimeFrames)) and (aTimeFrames[FrameIndex].Time<Time) do begin
   inc(FrameIndex);
  end;
  NextFrameIndex:=Min(FrameIndex+1,length(aTimeFrames)-1);

  CurrentFrame:=@aTimeFrames[FrameIndex].Frame;
  NextFrame:=@aTimeFrames[NextFrameIndex].Frame;

  // Interpolate between current and next frame
  InterpolationFactor:=(Time-aTimeFrames[FrameIndex].Time)/(aTimeFrames[NextFrameIndex].Time-aTimeFrames[FrameIndex].Time);
  for VertexIndex:=0 to CountUsedVertices-1 do begin
   
   CurrentVertex:=@CurrentFrame^.Vertices[VertexIndex];
   NextVertex:=@NextFrame^.Vertices[VertexIndex];
   InterpolatedVertex:=@InterpolatedFrame.Vertices[VertexIndex];
   
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

  aFrames[OutFrameIndex]:=InterpolatedFrame;
  
  Time:=Time+TimeStep; 

  inc(OutFrameIndex);

 end;

end;

function GetMergedAnimation(const aPresetAnimationIndex:TpvSizeInt):TTimeFrames;
const CountNonNormalizedFrames=100; // 100 frames as non normalized frames before normalization into fewer frames
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

procedure ConvertModel(const aInputFileName,aOutputFileName:String);
var Index,FrameIndex,OtherIndex,FoundPresetAnimation:TpvSizeInt;
    GLTFBakedVertexIndexedMesh:TpvGLTF.TBakedVertexIndexedMesh;
    ta,tb,t:TpvDouble;
    AnimationName:TpvUTF8String;
begin

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
       for Index:=0 to GLTFBakedVertexIndexedMesh.Indices.Count-1 do begin
        Indices[Index]:=GLTFBakedVertexIndexedMesh.Indices.ItemArray[Index];
       end;
      finally
       FreeAndNil(GLTFBakedVertexIndexedMesh);
      end;
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

 ConvertModel(InputFileName,OutputFileName);

end.


