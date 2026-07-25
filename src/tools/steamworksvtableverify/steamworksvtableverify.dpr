(*
** Checks the four Steamworks interfaces that the caller implements rather than calls, by letting a real
** C++ library invoke the Pascal objects through the C++ interface, exactly the way Steam does it.
**
** The calling side is steamworksvtablecaller.cpp, compiled against the actual Steamworks headers, so
** the vtable expectation over there is genuine. Everything reached from it is a Pascal object built by
** PasVulkan.Steamworks.Framework, which means a passing run proves the hand built vtables really do
** match the C++ ABI of the target.
*)
program steamworksvtableverify;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

uses {$ifdef Windows}
      Windows,
     {$endif}
     {$ifdef Unix}
      dl,
     {$endif}
     SysUtils,
     PasDblStrUtils,
     PasVulkan.Types,
     PasVulkan.Steamworks,
     PasVulkan.Steamworks.Framework;

type { TTestServerListResponse }
     TTestServerListResponse=class(TpvSteamworks.TServerListResponse)
      public
       Log:TpvUTF8String;
       procedure ServerResponded(const aRequest:THServerListRequest;const aServerIndex:TpvInt32); override;
       procedure ServerFailedToRespond(const aRequest:THServerListRequest;const aServerIndex:TpvInt32); override;
       procedure RefreshComplete(const aRequest:THServerListRequest;const aResponse:TEMatchMakingServerResponse); override;
     end;

     { TTestServerPingResponse }
     TTestServerPingResponse=class(TpvSteamworks.TServerPingResponse)
      public
       Log:TpvUTF8String;
       procedure ServerResponded(const aServer:Pgameserveritem_t); override;
       procedure ServerFailedToRespond; override;
     end;

     { TTestServerPlayersResponse }
     TTestServerPlayersResponse=class(TpvSteamworks.TServerPlayersResponse)
      public
       Log:TpvUTF8String;
       procedure AddPlayerToList(const aName:PSteamChar;const aScore:TpvInt32;const aTimePlayed:TpvFloat); override;
       procedure PlayersFailedToRespond; override;
       procedure PlayersRefreshComplete; override;
     end;

     { TTestServerRulesResponse }
     TTestServerRulesResponse=class(TpvSteamworks.TServerRulesResponse)
      public
       Log:TpvUTF8String;
       procedure RulesResponded(const aRule:PSteamChar;const aValue:PSteamChar); override;
       procedure RulesFailedToRespond; override;
       procedure RulesRefreshComplete; override;
     end;

     TCallResponse=procedure(const aResponse:TpvPointer); cdecl;

     TCallPingResponse=procedure(const aResponse:TpvPointer;const aServerItem:Pgameserveritem_t); cdecl;

{ TTestServerListResponse }

procedure TTestServerListResponse.ServerResponded(const aRequest:THServerListRequest;const aServerIndex:TpvInt32);
begin
 Log:=Log+'Responded('+TpvUTF8String(IntToHex(TpvPtrUInt(aRequest),4))+','+TpvUTF8String(IntToStr(aServerIndex))+') ';
end;

procedure TTestServerListResponse.ServerFailedToRespond(const aRequest:THServerListRequest;const aServerIndex:TpvInt32);
begin
 Log:=Log+'Failed('+TpvUTF8String(IntToHex(TpvPtrUInt(aRequest),4))+','+TpvUTF8String(IntToStr(aServerIndex))+') ';
end;

procedure TTestServerListResponse.RefreshComplete(const aRequest:THServerListRequest;const aResponse:TEMatchMakingServerResponse);
begin
 Log:=Log+'Complete('+TpvUTF8String(IntToHex(TpvPtrUInt(aRequest),4))+','+TpvUTF8String(IntToStr(aResponse))+')';
end;

{ TTestServerPingResponse }

procedure TTestServerPingResponse.ServerResponded(const aServer:Pgameserveritem_t);
begin
 Log:=Log+'Responded(ping='+TpvUTF8String(IntToStr(aServer^.m_nPing))+
          ',players='+TpvUTF8String(IntToStr(aServer^.m_nPlayers))+') ';
end;

procedure TTestServerPingResponse.ServerFailedToRespond;
begin
 Log:=Log+'Failed';
end;

{ TTestServerPlayersResponse }

procedure TTestServerPlayersResponse.AddPlayerToList(const aName:PSteamChar;const aScore:TpvInt32;const aTimePlayed:TpvFloat);
begin
 // ConvertDoubleToString of PasDblStrUtils writes locale independently, unlike FloatToStr, which hands
 // back a comma as the decimal separator on a German system.
 Log:=Log+'Add('+TpvSteamworks.ToUTF8String(aName)+','+
          TpvUTF8String(IntToStr(aScore))+','+
          TpvUTF8String(ConvertDoubleToString(aTimePlayed))+') ';
end;

procedure TTestServerPlayersResponse.PlayersFailedToRespond;
begin
 Log:=Log+'Failed ';
end;

procedure TTestServerPlayersResponse.PlayersRefreshComplete;
begin
 Log:=Log+'Complete';
end;

{ TTestServerRulesResponse }

procedure TTestServerRulesResponse.RulesResponded(const aRule:PSteamChar;const aValue:PSteamChar);
begin
 Log:=Log+'Rule('+TpvSteamworks.ToUTF8String(aRule)+'='+TpvSteamworks.ToUTF8String(aValue)+') ';
end;

procedure TTestServerRulesResponse.RulesFailedToRespond;
begin
 Log:=Log+'Failed ';
end;

procedure TTestServerRulesResponse.RulesRefreshComplete;
begin
 Log:=Log+'Complete';
end;

{ Main }

var LibraryHandle:TpvPointer;
    CountFailures:TpvSizeInt;

function LoadCallingSideLibrary(const aLibraryName:string):TpvPointer;
begin
{$ifdef Windows}
 result:={%H-}TpvPointer(Windows.LoadLibrary(PChar(aLibraryName)));
{$else}
{$ifdef Unix}
 result:=dlopen(PAnsiChar(aLibraryName),RTLD_NOW);
{$else}
 result:=nil;
{$endif}
{$endif}
end;

procedure FreeCallingSideLibrary(const aLibraryHandle:TpvPointer);
begin
{$ifdef Windows}
 Windows.FreeLibrary({%H-}HMODULE(aLibraryHandle));
{$else}
{$ifdef Unix}
 dlclose(aLibraryHandle);
{$endif}
{$endif}
end;

function GetCallingSideProcedure(const aName:string):TpvPointer;
begin
{$ifdef Windows}
 result:=Windows.GetProcAddress({%H-}HMODULE(LibraryHandle),PChar(aName));
{$else}
{$ifdef Unix}
 result:=dlsym(LibraryHandle,PAnsiChar(aName));
{$else}
 result:=nil;
{$endif}
{$endif}
 if not assigned(result) then begin
  WriteLn('  FAILED  "',aName,'" not exported by the calling side library');
  inc(CountFailures);
 end;
end;

procedure Check(const aWhat:string;const aExpected,aGot:TpvUTF8String);
begin
 if aExpected=aGot then begin
  WriteLn('  ok      ',aWhat);
 end else begin
  WriteLn('  FAILED  ',aWhat);
  WriteLn('            expected "',aExpected,'"');
  WriteLn('            got      "',aGot,'"');
  inc(CountFailures);
 end;
end;

var CallServerListResponse:TCallResponse;
    CallPingResponse:TCallPingResponse;
    CallPlayersResponse:TCallResponse;
    CallRulesResponse:TCallResponse;
    ServerListResponse:TTestServerListResponse;
    ServerPingResponse:TTestServerPingResponse;
    ServerPlayersResponse:TTestServerPlayersResponse;
    ServerRulesResponse:TTestServerRulesResponse;
    ServerItem:Tgameserveritem_t;
begin

 CountFailures:=0;

 LibraryHandle:=LoadCallingSideLibrary(ParamStr(1));
 if not assigned(LibraryHandle) then begin
  WriteLn('  FAILED  could not load the calling side library "',ParamStr(1),'"');
  Halt(1);
 end;

 try

  CallServerListResponse:=GetCallingSideProcedure('CallServerListResponse');
  CallPingResponse:=GetCallingSideProcedure('CallPingResponse');
  CallPlayersResponse:=GetCallingSideProcedure('CallPlayersResponse');
  CallRulesResponse:=GetCallingSideProcedure('CallRulesResponse');
  if CountFailures>0 then begin
   Halt(1);
  end;

  ServerListResponse:=TTestServerListResponse.Create;
  ServerPingResponse:=TTestServerPingResponse.Create;
  ServerPlayersResponse:=TTestServerPlayersResponse.Create;
  ServerRulesResponse:=TTestServerRulesResponse.Create;
  try

   CallServerListResponse(ServerListResponse.Handle);
   Check('ISteamMatchmakingServerListResponse, 3 methods',
         'Responded(1111,7) Failed(2222,8) Complete(3333,1)',
         ServerListResponse.Log);

   FillChar(ServerItem,SizeOf(Tgameserveritem_t),#0);
   ServerItem.m_nPing:=42;
   ServerItem.m_nPlayers:=13;
   CallPingResponse(ServerPingResponse.Handle,@ServerItem);
   Check('ISteamMatchmakingPingResponse, 2 methods',
         'Responded(ping=42,players=13) Failed',
         ServerPingResponse.Log);

   CallPlayersResponse(ServerPlayersResponse.Handle);
   Check('ISteamMatchmakingPlayersResponse, 3 methods',
         'Add(BeRo,4711,12.5) Failed Complete',
         ServerPlayersResponse.Log);

   CallRulesResponse(ServerRulesResponse.Handle);
   Check('ISteamMatchmakingRulesResponse, 3 methods',
         'Rule(mapname=de_dust2) Failed Complete',
         ServerRulesResponse.Log);

  finally
   FreeAndNil(ServerRulesResponse);
   FreeAndNil(ServerPlayersResponse);
   FreeAndNil(ServerPingResponse);
   FreeAndNil(ServerListResponse);
  end;

 finally
  FreeCallingSideLibrary(LibraryHandle);
 end;

 if CountFailures=0 then begin
  WriteLn('  all 11 methods reached through the hand built vtables');
 end else begin
  ExitCode:=1;
 end;

end.
