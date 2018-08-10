unit UnitProject;
{$i ..\..\PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses SysUtils,Classes,UnitVersion,UnitGlobals,UnitExternalProcess;

procedure CreateProject;
procedure UpdateProject;
procedure BuildProject;
procedure RunProject;

implementation

{$ifndef fpc}
const DirectorySeparator=PathDelim;

function ExcludeLeadingPathDelimiter(const s:string):string;
begin
 if (length(s)>0) and (s[1]=DirectorySeparator) then begin
  result:=copy(s,2,length(s)-1);
 end else begin
  result:=s;
 end;
end;
{$endif}

function FindBinaryInExecutableEnviromentPath(out aFoundExecutable:UnicodeString;const aBinaryName:UnicodeString;const aAdditionalPaths:UnicodeString=''):boolean;
var Index:Int32;
    PathString,CurrentPath:UnicodeString;
    PathStringList:TStringList;
begin
 result:=false;
 PathString:=UnicodeString(GetEnvironmentVariable('PATH'));
 if length(aAdditionalPaths)>0 then begin
  PathString:=aAdditionalPaths+';'+PathString;
 end;
 PathStringList:=TStringList.Create;
 try
  PathStringList.Delimiter:=';';
  PathStringList.StrictDelimiter:=true;
  PathStringList.DelimitedText:=String(PathString);
  for Index:=0 to PathStringList.Count-1 do begin
   CurrentPath:=UnicodeString(IncludeTrailingPathDelimiter(PathStringList.Strings[Index]));
   if FileExists(CurrentPath+aBinaryName) then begin
    aFoundExecutable:=CurrentPath+aBinaryName;
    result:=true;
    break;
   end;
  end;
 finally
  FreeAndNil(PathStringList);
 end;
end;

function GetRelativeFileList(const aPath:UnicodeString;const aMask:UnicodeString={$ifdef Unix}'*'{$else}'*.*'{$endif};const aParentPath:UnicodeString=''):TStringList;
var SearchRec:{$if declared(TUnicodeSearchRec)}TUnicodeSearchRec{$else}TSearchRec{$ifend};
    SubList:TStringList;
begin
 result:=TStringList.Create;
 try
  if FindFirst(IncludeTrailingPathDelimiter(aPath)+aMask,faAnyFile,SearchRec)=0 then begin
   try
    repeat
     if (SearchRec.Name<>'.') and (SearchRec.Name<>'..') then begin
      if (SearchRec.Attr and faDirectory)<>0 then begin
       result.Add(String(ExcludeLeadingPathDelimiter(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(aParentPath)+SearchRec.Name))));
       SubList:=GetRelativeFileList(IncludeTrailingPathDelimiter(aPath)+SearchRec.Name,
                                    aMask,
                                    IncludeTrailingPathDelimiter(aParentPath)+SearchRec.Name);
       if assigned(SubList) then begin
        try
         result.AddStrings(SubList);
        finally
         FreeAndNil(SubList);
        end;
       end;
      end else begin
       result.Add(String(ExcludeLeadingPathDelimiter(IncludeTrailingPathDelimiter(aParentPath)+SearchRec.Name)));
      end;
     end;
    until FindNext(SearchRec)<>0;
   finally
    FindClose(SearchRec);
   end;
  end;
 except
  FreeAndNil(result);
  raise;
 end;
end;

procedure CopyFile(const aSourceFileName,aDestinationFileName:UnicodeString);
var SourceFileStream,DestinationFileStream:TFileStream;
begin
 SourceFileStream:=TFileStream.Create(String(aSourceFileName),fmOpenRead or fmShareDenyWrite);
 try
  DestinationFileStream:=TFileStream.Create(String(aDestinationFileName),fmCreate);
  try
   if DestinationFileStream.CopyFrom(SourceFileStream,SourceFileStream.Size)<>SourceFileStream.Size then begin
    raise EInOutError.Create('InOutError at copying "'+String(aSourceFileName)+'" to "'+String(aDestinationFileName)+'"');
   end;
  finally
   FreeAndNil(DestinationFileStream);
  end;
 finally
  FreeAndNil(SourceFileStream);
 end;
end;

procedure CopyAndSubstituteTextFile(const aSourceFileName,aDestinationFileName:UnicodeString;const aSubstitutions:array of UnicodeString);
var Index,SubstitutionIndex,CountSubstitutions:Int32;
    StringList:TStringList;
    Line:String;
begin
 CountSubstitutions:=length(aSubstitutions);
 if CountSubstitutions>0 then begin
  StringList:=TStringList.Create;
  try
   StringList.LoadFromFile(String(aSourceFileName));
   for Index:=0 to StringList.Count-1 do begin
    Line:=StringList[Index];
    SubstitutionIndex:=0;
    while (SubstitutionIndex+1)<CountSubstitutions do begin
     Line:=(StringReplace(Line,String(aSubstitutions[SubstitutionIndex]),String(aSubstitutions[SubstitutionIndex+1]),[rfReplaceAll,rfIgnoreCase]));
     inc(SubstitutionIndex,2);
    end;
    StringList[Index]:=Line;
   end;
   StringList.SaveToFile(String(aDestinationFileName));
  finally
   FreeAndNil(StringList);
  end;
 end else begin
  CopyFile(aSourceFileName,aDestinationFileName);
 end;
end;

procedure CreateProject;
var Index:Int32;
    ProjectTemplateFileList,StringList:TStringList;
    ProjectPath,ProjectMetaDataPath,
    ProjectUUIDFileName,
    FileName,SourceFileName,DestinationFileName:UnicodeString;
    ProjectUUID:String;
    GUID:TGUID;
begin

 if not DirectoryExists(PasVulkanProjectTemplatePath) then begin
  WriteLn(ErrOutput,'Fatal: "',PasVulkanProjectTemplatePath,'" doesn''t exist!');
  exit;
 end;

 if length(CurrentProjectName)=0 then begin
  WriteLn(ErrOutput,'Fatal: No valid project name!');
  exit;
 end;

 ProjectPath:=IncludeTrailingPathDelimiter(PasVulkanProjectsPath+CurrentProjectName);
 if DirectoryExists(ProjectPath) then begin
  WriteLn(ErrOutput,'Fatal: "',ProjectPath,'" already exists!');
  exit;
 end;

 WriteLn('Creating "',ProjectPath,'" ...');
 if not ForceDirectories(ProjectPath) then begin
  WriteLn(ErrOutput,'Fatal: "',ProjectPath,'" couldn''t created!');
 end;

 ProjectMetaDataPath:=IncludeTrailingPathDelimiter(ProjectPath+'metadata');
 WriteLn('Creating "',ProjectMetaDataPath,'" ...');
 if not ForceDirectories(ProjectMetaDataPath) then begin
  WriteLn(ErrOutput,'Fatal: "',ProjectMetaDataPath,'" couldn''t created!');
  exit;
 end;

 CreateGUID(GUID);
 ProjectUUID:=LowerCase(GUIDToString(GUID));

 ProjectUUIDFileName:=ProjectMetaDataPath+'uuid';

 WriteLn('Creating "',ProjectUUIDFileName,'" ...');
 StringList:=TStringList.Create;
 try
  StringList.Text:=ProjectUUID;
  StringList.SaveToFile(String(ProjectUUIDFileName));
 finally
  FreeAndNil(StringList);
 end;

 ProjectTemplateFileList:=GetRelativeFileList(PasVulkanProjectTemplatePath);
 if assigned(ProjectTemplateFileList) then begin
  try
   for Index:=0 to ProjectTemplateFileList.Count-1 do begin
    FileName:=UnicodeString(ProjectTemplateFileList.Strings[Index]);
    SourceFileName:=PasVulkanProjectTemplatePath+FileName;
    DestinationFileName:=ProjectPath+FileName;
    if length(DestinationFileName)>0 then begin
     DestinationFileName:=UnicodeString(StringReplace(String(DestinationFileName),'projecttemplate',String(CurrentProjectName),[rfReplaceAll,rfIgnoreCase]));
     if (DestinationFileName[length(DestinationFileName)]=DirectorySeparator) or
        (IncludeTrailingPathDelimiter(ExtractFilePath(DestinationFileName))=DestinationFileName) then begin
      WriteLn('Creating "',DestinationFileName,'" ...');
      if not ForceDirectories(DestinationFileName) then begin
       WriteLn(ErrOutput,'Fatal: "',DestinationFileName,'" couldn''t created!');
       break;
      end;
     end else begin
      WriteLn('Copying "',SourceFileName,'" to "',DestinationFileName,'" ...');
      if FileName='src'+DirectorySeparator+'projecttemplate.dpr' then begin
       CopyAndSubstituteTextFile(SourceFileName,
                                 DestinationFileName,
                                 ['projecttemplate',CurrentProjectName]);
      end else if FileName='src'+DirectorySeparator+'projecttemplate.dproj' then begin
       CopyAndSubstituteTextFile(SourceFileName,
                                 DestinationFileName,
                                 ['projecttemplate',CurrentProjectName,
                                  '{00000000-0000-0000-0000-000000000000}',UnicodeString(ProjectUUID)]);
      end else if FileName='src'+DirectorySeparator+'projecttemplate.lpi' then begin
       CopyAndSubstituteTextFile(SourceFileName,
                                 DestinationFileName,
                                 ['projecttemplate',CurrentProjectName]);
      end else if (ExtractFileExt(FileName)='.pas') or
                  (ExtractFileExt(FileName)='.gradle') or
                  (ExtractFileExt(FileName)='.xml') or
                  (ExtractFileExt(FileName)='.java') then begin
       CopyAndSubstituteTextFile(SourceFileName,
                                 DestinationFileName,
                                 ['projecttemplate',CurrentProjectName]);
      end else begin
       CopyFile(SourceFileName,DestinationFileName);
      end;
     end;
    end;
   end;
  finally
   FreeAndNil(ProjectTemplateFileList);
  end;
 end;

 UpdateProject;

end;

procedure UpdateProject;
var Index:Int32;
    ProjectTemplateFileList,StringList:TStringList;
    ProjectPath,ProjectMetaDataPath,
    ProjectUUIDFileName,
    FileName,SourceFileName,DestinationFileName:UnicodeString;
    ProjectUUID:String;
    GUID:TGUID;
begin

 if not DirectoryExists(PasVulkanProjectTemplatePath) then begin
  WriteLn(ErrOutput,'Fatal: "',PasVulkanProjectTemplatePath,'" doesn''t exist!');
  exit;
 end;

 if length(CurrentProjectName)=0 then begin
  WriteLn(ErrOutput,'Fatal: No valid project name!');
  exit;
 end;

 ProjectPath:=IncludeTrailingPathDelimiter(PasVulkanProjectsPath+CurrentProjectName);
 if not DirectoryExists(ProjectPath) then begin
  WriteLn(ErrOutput,'Fatal: "',ProjectPath,'" not found!');
  exit;
 end;

 ProjectMetaDataPath:=IncludeTrailingPathDelimiter(ProjectPath+'metadata');
 WriteLn('Checking "',ProjectMetaDataPath,'" ...');
 if not DirectoryExists(ProjectMetaDataPath) then begin
  WriteLn(ErrOutput,'Fatal: "',ProjectMetaDataPath,'" not found!');
  exit;
 end;

 ProjectUUIDFileName:=ProjectMetaDataPath+'uuid';
 WriteLn('Reading "',ProjectUUIDFileName,'" ...');
 if not FileExists(ProjectUUIDFileName) then begin
  WriteLn(ErrOutput,'Fatal: "',ProjectUUIDFileName,'" not found!');
  exit;
 end;
 StringList:=TStringList.Create;
 try
  StringList.LoadFromFile(String(ProjectUUIDFileName));
  ProjectUUID:=trim(StringList.Text);
  GUID:=StringToGUID(String(ProjectUUID));
  ProjectUUID:=LowerCase(GUIDToString(GUID));
 finally
  FreeAndNil(StringList);
 end;

 ProjectTemplateFileList:=GetRelativeFileList(PasVulkanProjectTemplatePath);
 if assigned(ProjectTemplateFileList) then begin
  try
   for Index:=0 to ProjectTemplateFileList.Count-1 do begin
    FileName:=UnicodeString(ProjectTemplateFileList.Strings[Index]);
    SourceFileName:=PasVulkanProjectTemplatePath+FileName;
    DestinationFileName:=ProjectPath+FileName;
    if length(DestinationFileName)>0 then begin
     DestinationFileName:=UnicodeString(StringReplace(String(DestinationFileName),'projecttemplate',String(CurrentProjectName),[rfReplaceAll,rfIgnoreCase]));
     if (DestinationFileName[length(DestinationFileName)]=DirectorySeparator) or
        (IncludeTrailingPathDelimiter(ExtractFilePath(DestinationFileName))=DestinationFileName) then begin
      if not DirectoryExists(DestinationFileName) then begin
       WriteLn('Creating "',DestinationFileName,'" ...');
       if not ForceDirectories(DestinationFileName) then begin
        WriteLn(ErrOutput,'Fatal: "',DestinationFileName,'" couldn''t created!');
        break;
       end;
      end;
     end else begin
      if FileName='src'+DirectorySeparator+'projecttemplate.dpr' then begin
       WriteLn('Overwriting "',DestinationFileName,'" with "',SourceFileName,'" ...');
       CopyAndSubstituteTextFile(SourceFileName,
                                 DestinationFileName,
                                 ['projecttemplate',CurrentProjectName]);
      end else if FileName='src'+DirectorySeparator+'projecttemplate.dproj' then begin
      end else if FileName='src'+DirectorySeparator+'projecttemplate.lpi' then begin
      end else begin
      end;
     end;
    end;
   end;
  finally
   FreeAndNil(ProjectTemplateFileList);
  end;
 end;

end;

procedure BuildProject;
type TTargetCPU=(ARM_32,ARM_64,x86_32,x86_64);
     TTargetOS=(Android,Linux,Windows);
var ProjectPath,ProjectSourcePath:UnicodeString;
 procedure BuildWithDelphi;
 var DelphiBatchFileName:UnicodeString;
     Config,Platform:String;
     DelphiBatchFile:TStringList;
 begin

  DelphiBatchFileName:=ProjectSourcePath+'makedelphi.bat';

  DelphiBatchFile:=TStringList.Create;
  try
   DelphiBatchFile.Add('@echo off');
   DelphiBatchFile.Add('call rsvars.bat');
   if CurrentTarget='delphi-x86_64-windows' then begin
    Platform:='Win64';
   end else begin
    Platform:='Win32';
   end;
   case BuildMode of
    TBuildMode.Debug:begin
     Config:='Debug';
    end;
    else {TBuildMode.Release:}begin
     Config:='Release';
    end;
   end;
   DelphiBatchFile.Add('msbuild '+String(CurrentProjectName)+'.dproj /t:Rebuild /p:Config='+Config+';Platform='+Platform);
   DelphiBatchFile.SaveToFile(String(DelphiBatchFileName));
  finally
   FreeAndNil(DelphiBatchFile);
  end;

  if ExecuteCommand(ProjectSourcePath,'cmd',['/c',DelphiBatchFileName]) then begin
   WriteLn('Successful!');
  end else begin
   WriteLn('Errors!');
  end;

 end;
 procedure BuildWithFPC(const aTargetCPU:TTargetCPU;const aTargetOS:TTargetOS);
 const ExecutableFileExtension={$ifdef Windows}'.exe'{$else}''{$endif};
       SourcePaths:array[0..7] of String=
        (
         '.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'src',
         '.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'externals'+DirectorySeparator+'pasmp'+DirectorySeparator+'src',
         '.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'externals'+DirectorySeparator+'pucu'+DirectorySeparator+'src',
         '.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'externals'+DirectorySeparator+'pasjson'+DirectorySeparator+'src',
         '.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'externals'+DirectorySeparator+'pasgltf'+DirectorySeparator+'src',
         '.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'externals'+DirectorySeparator+'kraft'+DirectorySeparator+'src',
         '.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'externals'+DirectorySeparator+'rnl'+DirectorySeparator+'src',
         '.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'externals'+DirectorySeparator+'pasdblstrutils'+DirectorySeparator+'src'
        );
 var Index:Int32;
     Parameters:TStringList;
     FPCExecutable,FoundFPCExecutable:UnicodeString;
 begin
  FPCExecutable:='';
  FoundFPCExecutable:='';
  Parameters:=TStringList.Create;
  try
   Parameters.Add('-Sd');
   Parameters.Add('-B');
   for Index:=0 to length(SourcePaths)-1 do begin
    Parameters.Add('-Fi'+SourcePaths[Index]);
    Parameters.Add('-Fl'+SourcePaths[Index]);
    Parameters.Add('-Fu'+SourcePaths[Index]);
    Parameters.Add('-Fo'+SourcePaths[Index]);
   end;
   case aTargetOS of
    TTargetOS.Android:begin
     Parameters.Add('-Tandroid');
     case BuildMode of
      TBuildMode.Debug:begin
       Parameters.Add('-g');
       Parameters.Add('-gl');
       Parameters.Add('-gw3');
       Parameters.Add('-Xm');
       Parameters.Add('-dDEBUG');
      end;
      else {TBuildMode.Release:}begin
       Parameters.Add('-Xs');
       //Parameters.Add('-dRELEASE');
      end;
     end;
     Parameters.Add('-XX');
     Parameters.Add('-CX');
     Parameters.Add('-Cg');
     Parameters.Add('-dCompileForWithPIC');
     Parameters.Add('-dPasVulkanPasMP');
     Parameters.Add('-dPasVulkanUseSDL2');
     case aTargetCPU of
      TTargetCPU.ARM_32:begin
       if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'ppcrossarm'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='ppcrossarm';
       end else if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'ppcarm'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='ppcarm';
       end else if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'fpc'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='fpc';
       end;
       Parameters.Add('-PARMv7a');
       Parameters.Add('-CpARMv7A');
       Parameters.Add('-CfVFPv3');
       Parameters.Add('-OpARMv7a');
       Parameters.Add('-O-');
       Parameters.Add('-O1');
       Parameters.Add('-olibmain.so');
       Parameters.Add('-FUFPCOutput'+DirectorySeparator+'arm-android');
       Parameters.Add('-FEFPCOutput'+DirectorySeparator+'arm-android');
       Parameters.Add('-Fl.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'libs'+DirectorySeparator+'libpngandroid'+DirectorySeparator+'obj'+DirectorySeparator+'local'+DirectorySeparator+'armeabi-v7a');
       Parameters.Add('-Fo.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'libs'+DirectorySeparator+'libpngandroid'+DirectorySeparator+'obj'+DirectorySeparator+'local'+DirectorySeparator+'armeabi-v7a');
       Parameters.Add('-Fl.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'libs'+DirectorySeparator+'sdl20androidarm32');
       Parameters.Add('-Fo.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'libs'+DirectorySeparator+'sdl20androidarm32');
      end;
      TTargetCPU.x86_32:begin
       if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'ppcross386'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='ppcross386';
       end else if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'ppc386'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='ppc386';
       end else if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'fpc'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='fpc';
       end;
       Parameters.Add('-Pi386');
       Parameters.Add('-CpPENTIUMM');
       Parameters.Add('-CfX87');
       Parameters.Add('-OpPENTIUMM');
       Parameters.Add('-O-');
       Parameters.Add('-O1');
       Parameters.Add('-olibmain.so');
       Parameters.Add('-FUFPCOutput'+DirectorySeparator+'i386-android');
       Parameters.Add('-FEFPCOutput'+DirectorySeparator+'i386-android');
       Parameters.Add('-Fl.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'libs'+DirectorySeparator+'libpngandroid'+DirectorySeparator+'obj'+DirectorySeparator+'local'+DirectorySeparator+'x86');
       Parameters.Add('-Fo.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'libs'+DirectorySeparator+'libpngandroid'+DirectorySeparator+'obj'+DirectorySeparator+'local'+DirectorySeparator+'x86');
       Parameters.Add('-Fl.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'libs'+DirectorySeparator+'sdl20androidi386');
       Parameters.Add('-Fo.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'libs'+DirectorySeparator+'sdl20androidi386');
      end;
     end;
    end;
    TTargetOS.Linux:begin
     Parameters.Add('-Tlinux');
     Parameters.Add('-dPasVulkanPasMP');
     Parameters.Add('-dPasVulkanUseSDL2');
     Parameters.Add('-dPasVulkanUseSDL2WithVulkanSupport');
     Parameters.Add('-dXLIB');
     Parameters.Add('-dXCB');
     Parameters.Add('-dWayland');
     Parameters.Add('-dMir');
     Parameters.Add('-dUseCThreads');
     Parameters.Add('-dSDL');
     Parameters.Add('-dSDL20');
     case BuildMode of
      TBuildMode.Debug:begin
       Parameters.Add('-g');
       Parameters.Add('-gl');
       Parameters.Add('-Xm');
       Parameters.Add('-dDEBUG');
      end;
      else {TBuildMode.Release:}begin
       Parameters.Add('-Xs');
       Parameters.Add('-dRELEASE');
      end;
     end;
     case aTargetCPU of
      TTargetCPU.x86_32:begin
       if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'ppcross386'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='ppcross386';
       end else if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'ppc386'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='ppc386';
       end else if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'fpc'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='fpc';
       end;
       Parameters.Add('-Pi386');
       Parameters.Add('-CpPENTIUMM');
       Parameters.Add('-CfX87');
       Parameters.Add('-OpPENTIUMM');
       Parameters.Add('-O-');
       Parameters.Add('-O1');
       Parameters.Add('-dc_int64');
       Parameters.Add('-Cg-');
       Parameters.Add('-o'+String(CurrentProjectName)+'_x86_32-linux');
       Parameters.Add('-FUFPCOutput'+DirectorySeparator+'x86_32-linux');
       Parameters.Add('-FEFPCOutput'+DirectorySeparator+'x86_32-linux');
      end;
      TTargetCPU.x86_64:begin
       if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'ppcrossx64'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='ppcrossx64';
       end else if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'ppcx64'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='ppcx64';
       end else if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'fpc'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='fpc';
       end;
       Parameters.Add('-Px86_64');
       Parameters.Add('-CpCOREAVX');
       Parameters.Add('-CfSSE64');
       Parameters.Add('-OpCOREAVX');
       Parameters.Add('-O-');
       Parameters.Add('-O1');
       Parameters.Add('-o'+String(CurrentProjectName)+'_x86_64-linux');
       Parameters.Add('-FUFPCOutput'+DirectorySeparator+'x86_64-linux');
       Parameters.Add('-FEFPCOutput'+DirectorySeparator+'x86_64-linux');
      end;
     end;
    end;
    TTargetOS.Windows:begin
     Parameters.Add('-dPasVulkanPasMP');
     Parameters.Add('-dPasVulkanUseSDL2');
     if SDL2StaticLinking then begin
      Parameters.Add('-dSTATICLINK');
     end else begin
      Parameters.Add('-dPasVulkanUseSDL2WithVulkanSupport');
     end;
     Parameters.Add('-dSDL');
     Parameters.Add('-dSDL20');
     case BuildMode of
      TBuildMode.Debug:begin
       Parameters.Add('-g');
       Parameters.Add('-gl');
       Parameters.Add('-Xm');
       Parameters.Add('-dDEBUG');
      end;
      else {TBuildMode.Release:}begin
       Parameters.Add('-Xs');
       Parameters.Add('-dRELEASE');
      end;
     end;
     case aTargetCPU of
      TTargetCPU.x86_32:begin
       if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'ppcross386'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='ppcross386';
       end else if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'ppc386'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='ppc386';
       end else if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'fpc'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='fpc';
       end;
       Parameters.Add('-Pi386');
       Parameters.Add('-Twin32');
       Parameters.Add('-CpPENTIUMM');
       Parameters.Add('-CfX87');
       Parameters.Add('-OpPENTIUMM');
       Parameters.Add('-O-');
       Parameters.Add('-O1');
       Parameters.Add('-dc_int64');
       Parameters.Add('-Cg-');
       Parameters.Add('-o'+String(CurrentProjectName)+'_x86_32-windows.exe');
       Parameters.Add('-FUFPCOutput'+DirectorySeparator+'x86_32-win32');
       Parameters.Add('-FEFPCOutput'+DirectorySeparator+'x86_32-win32');
       if SDL2StaticLinking then begin
        Parameters.Add('-Fl.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'libs'+DirectorySeparator+'sdl20win32');
        Parameters.Add('-Fo.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'libs'+DirectorySeparator+'sdl20win32');
       end;
      end;
      TTargetCPU.x86_64:begin
       if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'ppcrossx64'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='ppcrossx64';
       end else if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'ppcx64'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='ppcx64';
       end else if FindBinaryInExecutableEnviromentPath(FoundFPCExecutable,'fpc'+ExecutableFileExtension,FPCbinaryPath) then begin
        FPCExecutable:='fpc';
       end;
       Parameters.Add('-Px86_64');
       Parameters.Add('-Twin64');
       Parameters.Add('-CpCOREAVX');
       Parameters.Add('-CfSSE64');
       Parameters.Add('-OpCOREAVX');
       Parameters.Add('-O-');
       Parameters.Add('-O1');
       Parameters.Add('-o'+String(CurrentProjectName)+'_x86_64-windows.exe');
       Parameters.Add('-FUFPCOutput'+DirectorySeparator+'x86_64-win64');
       Parameters.Add('-FEFPCOutput'+DirectorySeparator+'x86_64-win64');
       if SDL2StaticLinking then begin
        Parameters.Add('-dc_int64');
        Parameters.Add('-k--allow-multiple-definition');
        Parameters.Add('-Fl.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'libs'+DirectorySeparator+'sdl20win64');
        Parameters.Add('-Fo.'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'..'+DirectorySeparator+'libs'+DirectorySeparator+'sdl20win64');
       end;
      end;
     end;
    end;
   end;
   Parameters.Add(String(CurrentProjectName)+'.dpr');
   if (length(FPCExecutable)>0) and (length(FoundFPCExecutable)>0) then begin
    if ExecuteCommand(ProjectSourcePath,FoundFPCExecutable,Parameters) then begin
     WriteLn('Successful!');
     case aTargetOS of
      TTargetOS.Android:begin
       case aTargetCPU of
        TTargetCPU.ARM_32:begin
         CopyFile(ProjectSourcePath+'FPCOutput'+DirectorySeparator+'arm-android'+DirectorySeparator+'libmain.so',
                  ProjectSourcePath+'android'+DirectorySeparator+'app'+DirectorySeparator+'src'+DirectorySeparator+'main'+DirectorySeparator+'jniLibs'+DirectorySeparator+'armeabi-v7a'+DirectorySeparator+'libmain.so');
        end;
        TTargetCPU.x86_32:begin
         CopyFile(ProjectSourcePath+'FPCOutput'+DirectorySeparator+'i386-android'+DirectorySeparator+'libmain.so',
                  ProjectSourcePath+'android'+DirectorySeparator+'app'+DirectorySeparator+'src'+DirectorySeparator+'main'+DirectorySeparator+'jniLibs'+DirectorySeparator+'x86'+DirectorySeparator+'libmain.so');
        end;
       end;
      end;
      TTargetOS.Linux:begin
       case aTargetCPU of
        TTargetCPU.x86_32:begin
         CopyFile(ProjectSourcePath+'FPCOutput'+DirectorySeparator+'x86_32-linux'+DirectorySeparator+CurrentProjectName+'_x86_32-linux',
                  ProjectPath+'bin'+DirectorySeparator+CurrentProjectName+'_x86_32-linux');
        end;
        TTargetCPU.x86_64:begin
         CopyFile(ProjectSourcePath+'FPCOutput'+DirectorySeparator+'x86_64-linux'+DirectorySeparator+CurrentProjectName+'_x86_64-linux',
                  ProjectPath+'bin'+DirectorySeparator+CurrentProjectName+'_x86_64-linux');
        end;
       end;
      end;
      TTargetOS.Windows:begin
       case aTargetCPU of
        TTargetCPU.x86_32:begin
         CopyFile(ProjectSourcePath+'FPCOutput'+DirectorySeparator+'x86_32-win32'+DirectorySeparator+CurrentProjectName+'_x86_32-windows.exe',
                  ProjectPath+'bin'+DirectorySeparator+CurrentProjectName+'_x86_32-windows.exe');
        end;
        TTargetCPU.x86_64:begin
         CopyFile(ProjectSourcePath+'FPCOutput'+DirectorySeparator+'x86_64-win64'+DirectorySeparator+CurrentProjectName+'_x86_64-windows.exe',
                  ProjectPath+'bin'+DirectorySeparator+CurrentProjectName+'_x86_64-windows.exe');
        end;
       end;
      end;
     end;
    end else begin
     WriteLn('Errors!');
    end;
   end else begin
    WriteLn('Fatal: Target CPU and target OS combination not supported!');
   end;
  finally
   FreeAndNil(Parameters);
  end;
 end;
 procedure BuildForAndroid;
 var ProjectSourceAndroidPath,Task:UnicodeString;
 begin
  ProjectSourceAndroidPath:=UnicodeString(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(ProjectSourcePath)+'android'));
  case BuildMode of
   TBuildMode.Debug:begin
    Task:='assembleDebug';
   end;
   else {TBuildMode.Release:}begin
    Task:='assembleRelease';
   end;
  end;
  if ExecuteCommand(ProjectSourceAndroidPath,'gradlew'{$ifdef Windows}+'.bat'{$endif},Task) then begin
   WriteLn('Successful!');
   case BuildMode of
    TBuildMode.Debug:begin
     CopyFile(ProjectSourceAndroidPath+'app'+DirectorySeparator+'build'+DirectorySeparator+'outputs'+DirectorySeparator+'apk'+DirectorySeparator+'app-debug.apk',
              ProjectPath+'bin'+DirectorySeparator+CurrentProjectName+'_debug.apk');
    end;
    else {TBuildMode.Release:}begin
     CopyFile(ProjectSourceAndroidPath+'app'+DirectorySeparator+'build'+DirectorySeparator+'outputs'+DirectorySeparator+'apk'+DirectorySeparator+'app-release.apk',
              ProjectPath+'bin'+DirectorySeparator+CurrentProjectName+'_release.apk');
    end;
   end;
  end else begin
   WriteLn('Errors!');
  end;
 end;
begin

 if not DirectoryExists(PasVulkanProjectTemplatePath) then begin
  WriteLn(ErrOutput,'Fatal: "',PasVulkanProjectTemplatePath,'" doesn''t exist!');
  exit;
 end;

 if length(CurrentProjectName)=0 then begin
  WriteLn(ErrOutput,'Fatal: No valid project name!');
  exit;
 end;

 ProjectPath:=IncludeTrailingPathDelimiter(PasVulkanProjectsPath+CurrentProjectName);
 if not DirectoryExists(ProjectPath) then begin
  WriteLn(ErrOutput,'Fatal: "',ProjectPath,'" not found!');
  exit;
 end;

 UpdateProject;

 ProjectSourcePath:=IncludeTrailingPathDelimiter(ProjectPath+'src');

 if (CurrentTarget='delphi-x86_32-windows') or
    (CurrentTarget='delphi-x86_64-windows') then begin

  BuildWithDelphi;

 end else if CurrentTarget='fpc-x86_32-windows' then begin

  BuildWithFPC(TTargetCPU.x86_32,TTargetOS.Windows);

 end else if CurrentTarget='fpc-x86_64-windows' then begin

  BuildWithFPC(TTargetCPU.x86_64,TTargetOS.Windows);

 end else if CurrentTarget='fpc-x86_32-linux' then begin

  BuildWithFPC(TTargetCPU.x86_32,TTargetOS.Linux);

 end else if CurrentTarget='fpc-x86_64-linux' then begin

  BuildWithFPC(TTargetCPU.x86_64,TTargetOS.Linux);

 end else if CurrentTarget='fpc-allcpu-android' then begin

  BuildWithFPC(TTargetCPU.ARM_32,TTargetOS.Android);

  BuildWithFPC(TTargetCPU.x86_32,TTargetOS.Android);

  BuildForAndroid;

 end else begin
  WriteLn(ErrOutput,'Fatal: Target "',CurrentTarget,'" not supported!');
 end;

end;

procedure RunProject;
begin
end;

end.

