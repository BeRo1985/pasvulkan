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

function ExecuteCommand(const aCommand,aParameters:UnicodeString):boolean;

implementation

function ExecuteCommand(const aCommand,aParameters:UnicodeString):boolean;
{$if defined(Win32) or defined(Win64) or defined(Windows)}
const BufferSize=4096;
var SecurityAttributes:TSecurityAttributes;
    ReadHandle,WriteHandle:THandle;
    StartupInfo:Windows.TStartupInfoW;
    ProcessInformation:TProcessInformation;
    Buffer:array of AnsiChar;
    CountRead:UInt32;
    WaitForSingleObjectResult:HRESULT;
    CmdLine:WideString;
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
   CmdLine:=WideString(ACommand+' '+AParameters);
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
begin
end;
{$ifend}

end.

