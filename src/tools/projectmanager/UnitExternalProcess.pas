unit UnitExternalProcess;
{$i ..\..\PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses {$if defined(Win32) or defined(Win64) or defined(Windows)}
      Windows,ShellApi,
     {$ifend}
     {$ifdef fpc}
      process,
     {$endif}
     SysUtils,Classes,UnitVersion,UnitGlobals;

function ExecuteCommand(const aDirectory,aExecutable:UnicodeString;const aParameters:array of UnicodeString):boolean;

implementation

function ExecuteCommand(const aDirectory,aExecutable:UnicodeString;const aParameters:array of UnicodeString):boolean;
{$if (defined(Win32) or defined(Win64) or defined(Windows))} // and not defined(fpc)}
const BufferSize=4096;
var SecurityAttributes:TSecurityAttributes;
    ReadHandle,WriteHandle:THandle;
    StartupInfo:Windows.TStartupInfoW;
    ProcessInformation:TProcessInformation;
    Buffer:array of AnsiChar;
    Index:Int32;
    CountRead:UInt32;
    WaitForSingleObjectResult:HRESULT;
    CmdLine:WideString;
    Parameter:UnicodeString;
    ExitCode:DWORD;
begin
 result:=false;
 SecurityAttributes.nLength:=SizeOf(TSecurityAttributes);
 SecurityAttributes.bInheritHandle:=true;
 SecurityAttributes.lpSecurityDescriptor:=nil;
 if CreatePipe(ReadHandle,WriteHandle,@SecurityAttributes,0) then begin
  try
   FillChar(StartupInfo,SizeOf(TStartupInfoW),#0);
   StartupInfo.cb:=SizeOf(TStartupInfoW);
   StartupInfo.hStdInput:=ReadHandle;
   StartupInfo.hStdOutput:=WriteHandle;
   StartupInfo.hStdError:=WriteHandle;
   StartupInfo.dwFlags:=STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
   StartupInfo.wShowWindow:=SW_HIDE;
   CmdLine:='';
   for Index:=-1 to length(aParameters)-1 do begin
    if Index<0 then begin
     Parameter:=aExecutable;
    end else begin
     Parameter:=aParameters[Index];
    end;
    if (pos(' ',Parameter)>0) and (pos('"',Parameter)=0) then begin
     Parameter:='"'+Parameter+'"';
    end;
    if length(CmdLine)>0 then begin
     CmdLine:=CmdLine+' ';
    end;
    CmdLine:=CmdLine+WideString(Parameter);
   end;
   if CreateProcessW(nil,
                     PWideChar(CmdLine),
                     @SecurityAttributes,
                     @SecurityAttributes,
                     true,
                     NORMAL_PRIORITY_CLASS,
                     nil,
                     nil,
                     StartupInfo,
                     ProcessInformation) then begin
    Buffer:=nil;
    try
     SetLength(Buffer,BufferSize+1);
     try
      repeat
       WaitForSingleObjectResult:=WaitForSingleObject(ProcessInformation.hProcess,100);
       repeat
        CountRead:=0;
        ReadFile(ReadHandle,Buffer[0],BufferSize,CountRead,nil);
        Buffer[CountRead]:=#0;
        OemToAnsi(@Buffer[0],@Buffer[0]);
        Write(PAnsiChar(@Buffer[0])^);
       until CountRead<BufferSize;
      until WaitForSingleObjectResult<>WAIT_TIMEOUT;
     finally
      Buffer:=nil;
     end;
     if GetExitCodeProcess(ProcessInformation.hProcess,DWORD(ExitCode)) then begin
      result:=ExitCode=0;
     end;
    finally
     CloseHandle(ProcessInformation.hProcess);
     CloseHandle(ProcessInformation.hThread);
    end;
   end;
  finally
   CloseHandle(ReadHandle);
   CloseHandle(WriteHandle);
  end;
 end;
end;
{$else}
var ChildProcess:TProcess;
    Index,Count:Int32;
    TempString:UTF8String;
begin
 ChildProcess:=TProcess.Create(nil);
 try
  ChildProcess.Options:=[poUsePipes,poStderrToOutput];
  ChildProcess.ShowWindow:=swoHide;
  ChildProcess.CurrentDirectory:=String(aDirectory);
  ChildProcess.Executable:=String(aExecutable);
  for Index:=0 to length(aParameters)-1 do begin
   ChildProcess.Parameters.Add(String(aParameters[Index]));
  end;
  ChildProcess.Execute;
  while ChildProcess.Running do begin
   Count:=ChildProcess.Output.NumBytesAvailable;
   if Count>0 then begin
    SetLength(TempString,Count);
    ChildProcess.Output.ReadBuffer(TempString[1],Count);
    Write(TempString);
   end;
  end;
  ChildProcess.WaitOnExit;
  result:=ChildProcess.ExitCode=0;
 finally
  ChildProcess.Free;
 end;
end;
{$ifend}

end.

