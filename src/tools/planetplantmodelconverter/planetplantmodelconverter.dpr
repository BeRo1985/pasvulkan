program planetplantmodelconverter;
{$ifdef fpc}
 {$mode delphi}
{$endif}

uses Classes,
     SysUtils,
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
      Indices:TIndices;
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

    GLTF:TpvGLTF;
    GLTFInstance:TpvGLTF.TInstance;

function GetMergedAnimation(const aPresetAnimationIndex:TpvSizeInt):TTimeFrames;
const CountNonNormalizedFrames=100; // 100 frames as non normalized frames before normalization into fewer frames
var AnimationPart,Index,FrameIndex,OtherIndex,FoundPresetAnimation,BaseIndex,
    TriangleIndex,VertexIndex:TpvSizeInt;
    ta,tb,t:TpvDouble;
    PresetAnimationName,AnimationName:TpvUTF8String;
    FoundAnimationPart:boolean;
    GLTFBakedMesh:TpvGLTF.TBakedMesh;
    GLTFBakedMeshTriangle:TpvGLTF.TBakedMesh.PTriangle;
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

       GLTFBakedMesh:=GLTFInstance.GetBakedMesh(false,true,-1,[TPasGLTF.TMaterial.TAlphaMode.Opaque,TPasGLTF.TMaterial.TAlphaMode.Blend,TPasGLTF.TMaterial.TAlphaMode.Mask]);
       if assigned(GLTFBakedMesh) then begin
        try

         for TriangleIndex:=0 to GLTFBakedMesh.Triangles.Count-1 do begin

          GLTFBakedMeshTriangle:=@GLTFBakedMesh.Triangles.ItemArray[TriangleIndex];

          for VertexIndex:=0 to 2 do begin
           Vertex.Position:=TpvVector3.Create(GLTFBakedMeshTriangle^.Positions[VertexIndex][0],GLTFBakedMeshTriangle^.Positions[VertexIndex][1],GLTFBakedMeshTriangle^.Positions[VertexIndex][2]);

          end;

         end;
        
        finally
         FreeAndNil(GLTFBakedMesh);
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
    GLTFBakedMesh:TpvGLTF.TBakedMesh;
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

    for Index:=0 to length(GLTF.Animations)-1 do begin

     AnimationName:=Trim(LowerCase(GLTF.Animations[Index].Name));

     FoundPresetAnimation:=-1;
     for OtherIndex:=0 to CountPresetAnimations-1 do begin
      if AnimationName=PresetAnimations[OtherIndex].Name then begin
       FoundPresetAnimation:=PresetAnimations[OtherIndex].Index;
       break;
      end;
     end;

     if FoundPresetAnimation>=0 then begin

      GLTFInstance.Animation:=Index;

      ta:=GLTF.GetAnimationBeginTime(Index);
      tb:=GLTF.GetAnimationEndTime(Index);

      for FrameIndex:=0 to CountFrames-1 do begin

       t:=FrameIndex/(CountFrames-1);

       GLTFInstance.AnimationTime:=(ta*(1.0-t))+(tb*t);

       GLTFInstance.Update;

       GLTFBakedMesh:=GLTFInstance.GetBakedMesh(false,true,-1,[TPasGLTF.TMaterial.TAlphaMode.Opaque,TPasGLTF.TMaterial.TAlphaMode.Blend,TPasGLTF.TMaterial.TAlphaMode.Mask]);
       if assigned(GLTFBakedMesh) then begin
        try

        finally
         FreeAndNil(GLTFBakedMesh);
        end;
       end;

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


