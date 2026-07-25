(*
** Checks the callback and call result dispatch of PasVulkan.Steamworks.Framework without a running
** Steam client, by simulating Steam.
**
** The entry point pointers of PasVulkan.Steamworks are ordinary variables, so after the library has been
** loaded they can be pointed at local functions. A fake SteamAPI_ManualDispatch_GetNextCallback hands
** out a queue of two ordinary callbacks followed by one call completion, and a fake
** SteamAPI_ManualDispatch_GetAPICallResult supplies the payload of the completed call. That covers the
** parts of the protocol that are easy to get wrong and impossible to observe from the outside: whether
** every registered handler is reached, whether a call result fires exactly once, whether every fetched
** callback is freed again, and whether a destroyed handler really stops receiving.
**
** The versioned interface accessors are deliberately NOT faked. Without an initialized client they
** simply return nil, so the framework's interface caching runs through as it would in the real thing.
*)
program steamworksdispatchverify;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses SysUtils,
     PasVulkan.Types,
     PasVulkan.Steamworks,
     PasVulkan.Steamworks.Framework;

const TestAPICall=TSteamAPICall_t($1234);
      TestPlayerCount=4711;
      TestFirstResult=11;
      TestSecondResult=22;

type { TTestCallbackHandler }
     TTestCallbackHandler=class(TpvSteamworks.TCallbackHandler)
      public
       CountCalls:TpvSizeInt;
       LastResult:TpvInt32;
       procedure Handle(const aData:TpvPointer;const aDataSize:TpvSizeInt); override;
     end;

     { TTestCallResultHandler }
     TTestCallResultHandler=class(TpvSteamworks.TCallResultHandler)
      public
       CountCalls:TpvSizeInt;
       LastIOFailure:boolean;
       LastPlayerCount:TpvInt32;
       procedure Handle(const aData:TpvPointer;const aDataSize:TpvSizeInt;const aIOFailure:boolean); override;
     end;

procedure TTestCallbackHandler.Handle(const aData:TpvPointer;const aDataSize:TpvSizeInt);
begin
 inc(CountCalls);
 LastResult:=PSteamServersDisconnected_t(aData)^.m_eResult;
end;

procedure TTestCallResultHandler.Handle(const aData:TpvPointer;const aDataSize:TpvSizeInt;const aIOFailure:boolean);
begin
 inc(CountCalls);
 LastIOFailure:=aIOFailure;
 if assigned(aData) then begin
  LastPlayerCount:=PNumberOfCurrentPlayers_t(aData)^.m_cPlayers;
 end;
end;

{ The simulated Steam }

var QueueIndex:TpvSizeInt;
    CountFreedCallbacks:TpvSizeInt;
    Disconnected:TSteamServersDisconnected_t;
    CallCompleted:TSteamAPICallCompleted_t;
    PlayerCount:TNumberOfCurrentPlayers_t;

procedure FakeManualDispatchRunFrame(const hSteamPipe:THSteamPipe); cdecl;
begin
 QueueIndex:=0;
end;

// Two ordinary callbacks with different payloads, then one call completion, then nothing.
function FakeManualDispatchGetNextCallback(const hSteamPipe:THSteamPipe;const pCallbackMsg:PCallbackMsg_t):TSteamBool; cdecl;
begin

 result:=true;

 case QueueIndex of

  0:begin
   Disconnected.m_eResult:=TestFirstResult;
   pCallbackMsg^.m_iCallback:=SteamServersDisconnected_t_k_iCallback;
   pCallbackMsg^.m_pubParam:=PSteamUInt8(@Disconnected);
   pCallbackMsg^.m_cubParam:=SizeOf(TSteamServersDisconnected_t);
  end;

  1:begin
   Disconnected.m_eResult:=TestSecondResult;
   pCallbackMsg^.m_iCallback:=SteamServersDisconnected_t_k_iCallback;
   pCallbackMsg^.m_pubParam:=PSteamUInt8(@Disconnected);
   pCallbackMsg^.m_cubParam:=SizeOf(TSteamServersDisconnected_t);
  end;

  2:begin
   CallCompleted.m_hAsyncCall:=TestAPICall;
   CallCompleted.m_iCallback:=NumberOfCurrentPlayers_t_k_iCallback;
   CallCompleted.m_cubParam:=SizeOf(TNumberOfCurrentPlayers_t);
   pCallbackMsg^.m_iCallback:=SteamAPICallCompleted_t_k_iCallback;
   pCallbackMsg^.m_pubParam:=PSteamUInt8(@CallCompleted);
   pCallbackMsg^.m_cubParam:=SizeOf(TSteamAPICallCompleted_t);
  end;

  else begin
   result:=false;
  end;

 end;

 inc(QueueIndex);

end;

procedure FakeManualDispatchFreeLastCallback(const hSteamPipe:THSteamPipe); cdecl;
begin
 inc(CountFreedCallbacks);
end;

function FakeManualDispatchGetAPICallResult(const hSteamPipe:THSteamPipe;
                                           const hSteamAPICall:TSteamAPICall_t;
                                           const pCallback:TSteamPointer;
                                           const cubCallback:TSteamInt32;
                                           const iCallbackExpected:TSteamInt32;
                                           const pbFailed:PSteamBool):TSteamBool; cdecl;
begin
 pbFailed^:=false;
 PlayerCount.m_bSuccess:=1;
 PlayerCount.m_cPlayers:=TestPlayerCount;
 Move(PlayerCount,pCallback^,SizeOf(TNumberOfCurrentPlayers_t));
 // The framework has to pass the handle and the expected identifier of the completion straight through.
 result:=(hSteamAPICall=TestAPICall) and (iCallbackExpected=NumberOfCurrentPlayers_t_k_iCallback);
end;

function FakeSteamAPIInit(const pszInternalCheckInterfaceVersions:PSteamChar;const pOutErrMsg:PSteamErrMsg):TESteamAPIInitResult; cdecl;
begin
 result:=k_ESteamAPIInitResult_OK;
end;

procedure FakeManualDispatchInit; cdecl;
begin
end;

function FakeGetHSteamPipe:THSteamPipe; cdecl;
begin
 result:=1;
end;

function FakeGetHSteamUser:THSteamUser; cdecl;
begin
 result:=1;
end;

{ Main }

var CountFailures:TpvSizeInt;

procedure Check(const aWhat:string;const aOK:boolean);
begin
 if aOK then begin
  WriteLn('  ok      ',aWhat);
 end else begin
  WriteLn('  FAILED  ',aWhat);
  inc(CountFailures);
 end;
end;

var Steamworks:TpvSteamworks;
    FirstCallbackHandler,SecondCallbackHandler:TTestCallbackHandler;
    CallResultHandler:TTestCallResultHandler;
begin

 CountFailures:=0;

 Steamworks:=TpvSteamworks.Create;
 try

  Check('LoadLibrary',Steamworks.LoadLibrary(ParamStr(1)));
  if not Steamworks.LibraryLoaded then begin
   WriteLn('  FAILED  cannot continue without the library "',ParamStr(1),'"');
   Halt(1);
  end;

  // Only now, because loading would overwrite the pointers again. The address operator is mandatory: an
  // assignment without it calls the function and stores its result whenever the return type happens to
  // be assignment compatible.
  SteamInternal_SteamAPI_Init:=@FakeSteamAPIInit;
  SteamAPI_ManualDispatch_Init:=@FakeManualDispatchInit;
  SteamAPI_GetHSteamPipe:=@FakeGetHSteamPipe;
  SteamAPI_GetHSteamUser:=@FakeGetHSteamUser;
  SteamAPI_ManualDispatch_RunFrame:=@FakeManualDispatchRunFrame;
  SteamAPI_ManualDispatch_GetNextCallback:=@FakeManualDispatchGetNextCallback;
  SteamAPI_ManualDispatch_FreeLastCallback:=@FakeManualDispatchFreeLastCallback;
  SteamAPI_ManualDispatch_GetAPICallResult:=@FakeManualDispatchGetAPICallResult;

  Check('Initialize',Steamworks.Initialize(ParamStr(1))=k_ESteamAPIInitResult_OK);
  Check('Initialized property',Steamworks.Initialized);
  Check('steam pipe taken over',Steamworks.SteamPipe=1);

  FirstCallbackHandler:=TTestCallbackHandler.Create(Steamworks,SteamServersDisconnected_t_k_iCallback);
  SecondCallbackHandler:=TTestCallbackHandler.Create(Steamworks,SteamServersDisconnected_t_k_iCallback);
  CallResultHandler:=TTestCallResultHandler.Create(Steamworks,
                                                   TestAPICall,
                                                   NumberOfCurrentPlayers_t_k_iCallback,
                                                   SizeOf(TNumberOfCurrentPlayers_t));
  try

   CountFreedCallbacks:=0;
   Steamworks.RunFrame;

   Check('both callback handlers called twice',
         (FirstCallbackHandler.CountCalls=2) and (SecondCallbackHandler.CountCalls=2));
   Check('callback payload arrives',
         (FirstCallbackHandler.LastResult=TestSecondResult) and (SecondCallbackHandler.LastResult=TestSecondResult));
   Check('call result called once',CallResultHandler.CountCalls=1);
   Check('call result payload arrives',CallResultHandler.LastPlayerCount=TestPlayerCount);
   Check('call result without io failure',not CallResultHandler.LastIOFailure);
   Check('every fetched callback freed again',CountFreedCallbacks=3);

   // A call result fires once, so a second frame must not deliver it again.
   Steamworks.RunFrame;
   Check('call result fires only once',CallResultHandler.CountCalls=1);
   Check('callback handlers still active',FirstCallbackHandler.CountCalls=4);

   // A destroyed handler unregisters itself, so it must stop receiving.
   FreeAndNil(SecondCallbackHandler);
   Steamworks.RunFrame;
   Check('destroyed handler stops receiving',FirstCallbackHandler.CountCalls=6);

  finally
   FreeAndNil(CallResultHandler);
   FreeAndNil(SecondCallbackHandler);
   FreeAndNil(FirstCallbackHandler);
  end;

  Check('BufferToUTF8String stops at the zero byte',
        TpvSteamworks.BufferToUTF8String(PAnsiChar('abc'#0'rest'),8)='abc');
  Check('ToUTF8String tolerates nil',TpvSteamworks.ToUTF8String(nil)='');

 finally
  FreeAndNil(Steamworks);
 end;

 if CountFailures=0 then begin
  WriteLn('  all 15 checks passed');
 end else begin
  ExitCode:=1;
 end;

end.
