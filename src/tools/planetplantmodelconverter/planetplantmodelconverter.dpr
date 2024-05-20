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

const CountPresetAnimations=5;

type TPresetAnimation=record
      Index:TpvSizeInt;
      Name:TpvUTF8String; 
     end;

     TPresetAnimations=array[0..CountPresetAnimations-1] of TPresetAnimation;

const PresetAnimations:TPresetAnimations=(
       (Index:0;Name:'grow'),
       (Index:1;Name:'blossoms'),
       (Index:2;Name:'falloff'),
       (Index:3;Name:'wither'),
       (Index:4;Name:'death')
      );

procedure ConvertModel(const aInputFileName,aOutputFileName:String);
const CountFrames=16;
type TVertex=packed record
      Position:TpvVector3; // 12 bytes
      TexCoordU:TpvUInt16; // 2 bytes
      TexCoordV:TpvUInt16; // 2 bytes
      TangentSpace:TpvUInt16PackedTangentSpace; // 8 bytes
     end; // 12+2+2+8 = 24 bytes
     PVertex=^TVertex;
     TVertices=array of TVertex;
var Index,FrameIndex,OtherIndex,FoundPresetAnimation:TpvSizeInt;
    GLTF:TpvGLTF;
    GLTFInstance:TpvGLTF.TInstance;
    GLTFBakedMesh:TpvGLTF.TBakedMesh;
    ta,tb,t:TpvDouble;
    AnimationName:TpvUTF8String;
begin

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


