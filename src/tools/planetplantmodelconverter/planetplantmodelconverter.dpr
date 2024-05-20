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

procedure ConvertModel(const aInputFileName,aOutputFileName:String);
var GLTF:TpvGLTF;
begin

 GLTF:=TpvGLTF.Create;
 try

  GLTF.LoadFromFile(aInputFileName);

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

