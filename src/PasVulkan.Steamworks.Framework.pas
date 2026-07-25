(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2026, Benjamin Rosseaux (benjamin@rosseaux.de)               *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. This PasVulkan wrapper may be used only with the PasVulkan-own Vulkan   *
 *    Pascal header.                                                          *
 * 4. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/pasvulkan                                    *
 * 5. Write code which's compatible with Delphi >= 2009 and FreePascal >=     *
 *    3.1.1                                                                   *
 * 6. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 7. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 8. Try to use const when possible.                                         *
 * 9. Make sure to comment out writeln, used while debugging.                 *
 * 10. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,    *
 *     x86-64, ARM, ARM64, etc.).                                            *
 * 11. Make sure the code runs on all platforms with Vulkan support           *
 *                                                                            *
 ******************************************************************************)
unit PasVulkan.Steamworks.Framework;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$rangechecks off}
{$overflowchecks off}

// Object layer on top of the raw flat bindings in PasVulkan.Steamworks. It owns the parts of the
// Steamworks API that are painful to do by hand: the library and interface lifecycle, the manual
// callback dispatch protocol, and the bookkeeping that turns an asynchronous SteamAPICall_t into a
// call result delivered to a handler.
//
// The raw bindings stay fully usable next to this. Everything here is optional convenience, and the
// cached interface pointers are handed out as-is so that any of the 913 flat interface methods can be
// called directly on them.
//
// Threading: Steam wants its callbacks pumped from a single thread, so RunFrame belongs on the thread
// that also drives the rest of the frame. Handler registration and removal are guarded by a lock and
// may happen from any thread.

interface

uses SysUtils,
     Classes,
     Math,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Collections,
     PasVulkan.Steamworks;

type { TpvSteamworks }
     TpvSteamworks=class
      public
       type EpvSteamworks=class(Exception);

            EpvSteamworksLibrary=class(EpvSteamworks);

            EpvSteamworksHandler=class(EpvSteamworks);

            TCallbackHandler=class;

            TCallResultHandler=class;

            { TCallbackHandler }
            // Receives one callback identifier. Implemented as a virtual class rather than as a
            // procedure reference, so that the unit stays compilable with Delphi as well.
            //
            // A handler registers itself on construction and unregisters itself on destruction, so
            // owning it is all that is needed to keep it alive.
            TCallbackHandler=class
             private
              fSteamworks:TpvSteamworks;
              fCallbackIdentifier:TpvInt32;
             public
              constructor Create(const aSteamworks:TpvSteamworks;const aCallbackIdentifier:TpvInt32); reintroduce;
              destructor Destroy; override;
              // aData points at the callback record belonging to aCallbackIdentifier, so an
              // implementation casts it to the matching PSomething_t of PasVulkan.Steamworks. The
              // pointer is only valid for the duration of this call.
              procedure Handle(const aData:TpvPointer;const aDataSize:TpvSizeInt); virtual; abstract;
             published
              property Steamworks:TpvSteamworks read fSteamworks;
              property CallbackIdentifier:TpvInt32 read fCallbackIdentifier;
            end;

            TCallbackHandlerList=class(TpvObjectGenericList<TCallbackHandler>)
            end;

            TCallbackHandlerListHashMap=class(TpvHashMap<TpvInt32,TCallbackHandlerList>)
            end;

            { TCallResultHandler }
            // Receives the result of a single asynchronous call. The expected size is the size of the
            // result record, because the framework has to allocate the buffer that Steam copies into
            // before the handler can look at it.
            //
            // Unlike a callback handler this one fires at most once, and the framework drops its
            // registration afterwards. Destroying it earlier cancels the pending result.
            TCallResultHandler=class
             private
              fSteamworks:TpvSteamworks;
              fAPICall:TSteamAPICall_t;
              fExpectedCallbackIdentifier:TpvInt32;
              fExpectedDataSize:TpvSizeInt;
             public
              constructor Create(const aSteamworks:TpvSteamworks;
                                 const aAPICall:TSteamAPICall_t;
                                 const aExpectedCallbackIdentifier:TpvInt32;
                                 const aExpectedDataSize:TpvSizeInt); reintroduce;
              destructor Destroy; override;
              // aIOFailure tells that the call itself failed, in which case aData holds nothing
              // meaningful.
              procedure Handle(const aData:TpvPointer;const aDataSize:TpvSizeInt;const aIOFailure:boolean); virtual; abstract;
             published
              property Steamworks:TpvSteamworks read fSteamworks;
              property APICall:TSteamAPICall_t read fAPICall;
              property ExpectedCallbackIdentifier:TpvInt32 read fExpectedCallbackIdentifier;
              property ExpectedDataSize:TpvSizeInt read fExpectedDataSize;
            end;

            TCallResultHandlerHashMap=class(TpvHashMap<TSteamAPICall_t,TCallResultHandler>)
            end;

            TCallResultBuffer=array of TpvUInt8;

            // The four server browser interfaces are the only part of the API that the caller
            // implements instead of calls: Steam takes a pointer to a C++ object and invokes its
            // virtual methods. These classes build such an object, a record whose first field points at
            // a table of method pointers, and route the calls back into overridable methods. Pass
            // Handle into the ISteamMatchmakingServers entry points, and keep the instance alive for as
            // long as the request runs.

            { TServerListResponse }
            TServerListResponse=class
             private
              fResponseObject:TISteamMatchmakingServerListResponseObject;
              fVTable:TISteamMatchmakingServerListResponseVTable;
             public
              constructor Create; reintroduce;
              function Handle:PISteamMatchmakingServerListResponse;
              procedure ServerResponded(const aRequest:THServerListRequest;const aServerIndex:TpvInt32); virtual;
              procedure ServerFailedToRespond(const aRequest:THServerListRequest;const aServerIndex:TpvInt32); virtual;
              procedure RefreshComplete(const aRequest:THServerListRequest;const aResponse:TEMatchMakingServerResponse); virtual;
            end;

            { TServerPingResponse }
            TServerPingResponse=class
             private
              fResponseObject:TISteamMatchmakingPingResponseObject;
              fVTable:TISteamMatchmakingPingResponseVTable;
             public
              constructor Create; reintroduce;
              function Handle:PISteamMatchmakingPingResponse;
              procedure ServerResponded(const aServer:Pgameserveritem_t); virtual;
              procedure ServerFailedToRespond; virtual;
            end;

            { TServerPlayersResponse }
            TServerPlayersResponse=class
             private
              fResponseObject:TISteamMatchmakingPlayersResponseObject;
              fVTable:TISteamMatchmakingPlayersResponseVTable;
             public
              constructor Create; reintroduce;
              function Handle:PISteamMatchmakingPlayersResponse;
              procedure AddPlayerToList(const aName:PSteamChar;const aScore:TpvInt32;const aTimePlayed:TpvFloat); virtual;
              procedure PlayersFailedToRespond; virtual;
              procedure PlayersRefreshComplete; virtual;
            end;

            { TServerRulesResponse }
            TServerRulesResponse=class
             private
              fResponseObject:TISteamMatchmakingRulesResponseObject;
              fVTable:TISteamMatchmakingRulesResponseVTable;
             public
              constructor Create; reintroduce;
              function Handle:PISteamMatchmakingRulesResponse;
              procedure RulesResponded(const aRule:PSteamChar;const aValue:PSteamChar); virtual;
              procedure RulesFailedToRespond; virtual;
              procedure RulesRefreshComplete; virtual;
            end;
      private
       fLibraryLoaded:boolean;
       fInitialized:boolean;
       fGameServerMode:boolean;
       fSteamPipe:THSteamPipe;
       fSteamUser:THSteamUser;
       fLastErrorMessage:TpvUTF8String;
       fLock:TPasMPMultipleReaderSingleWriterLock;
       fCallbackHandlerListHashMap:TCallbackHandlerListHashMap;
       fCallResultHandlerHashMap:TCallResultHandlerHashMap;
       fCallResultBuffer:TCallResultBuffer;
       fClient:PISteamClient;
       fUser:PISteamUser;
       fFriends:PISteamFriends;
       fUtils:PISteamUtils;
       fMatchmaking:PISteamMatchmaking;
       fMatchmakingServers:PISteamMatchmakingServers;
       fUserStats:PISteamUserStats;
       fApps:PISteamApps;
       fNetworking:PISteamNetworking;
       fRemoteStorage:PISteamRemoteStorage;
       fScreenshots:PISteamScreenshots;
       fHTTP:PISteamHTTP;
       fUGC:PISteamUGC;
       fMusic:PISteamMusic;
       fHTMLSurface:PISteamHTMLSurface;
       fInventory:PISteamInventory;
       fVideo:PISteamVideo;
       fParentalSettings:PISteamParentalSettings;
       fInput:PISteamInput;
       fController:PISteamController;
       fParties:PISteamParties;
       fRemotePlay:PISteamRemotePlay;
       fTimeline:PISteamTimeline;
       fNetworkingMessages:PISteamNetworkingMessages;
       fNetworkingSockets:PISteamNetworkingSockets;
       fNetworkingUtils:PISteamNetworkingUtils;
       fGameServer:PISteamGameServer;
       fGameServerStats:PISteamGameServerStats;
       procedure AcquireInterfaces;
       procedure ReleaseInterfaces;
       procedure DispatchCallback(const aCallbackMessage:PCallbackMsg_t);
       procedure DispatchCallResult(const aCallbackMessage:PCallbackMsg_t);
       procedure InternalRegisterCallbackHandler(const aCallbackHandler:TCallbackHandler);
       procedure InternalUnregisterCallbackHandler(const aCallbackHandler:TCallbackHandler);
       procedure InternalRegisterCallResultHandler(const aCallResultHandler:TCallResultHandler);
       procedure InternalUnregisterCallResultHandler(const aCallResultHandler:TCallResultHandler);
      public
       constructor Create; reintroduce;
       destructor Destroy; override;

       // Loads the shared library without talking to Steam yet, so that a build can degrade
       // gracefully when no Steam runtime is present.
       function LoadLibrary(const aLibraryName:string=STEAMWORKS_DEFAULT_LIB_NAME):boolean;

       // Asks Steam to relaunch the executable through the client. Returns true when the caller should
       // quit immediately, which is the whole point of the call. Needs the library loaded but not
       // initialized, and does nothing useful while a steam_appid.txt sits next to the executable.
       function RestartAppIfNecessary(const aOwnAppID:TpvUInt32):boolean;

       // Brings up the client API and switches callback delivery to manual dispatch. Loads the library
       // first when that has not happened yet.
       function Initialize(const aLibraryName:string=STEAMWORKS_DEFAULT_LIB_NAME):TESteamAPIInitResult;

       // The game server counterpart. Uses the same manual dispatch pump afterwards.
       function InitializeGameServer(const aIP:TpvUInt32;
                                     const aGamePort:TpvUInt16;
                                     const aQueryPort:TpvUInt16;
                                     const aServerMode:TEServerMode;
                                     const aVersionString:TpvUTF8String;
                                     const aLibraryName:string=STEAMWORKS_DEFAULT_LIB_NAME):TESteamAPIInitResult;

       procedure Shutdown;

       // Pumps the callback queue once. Call this once per frame from a single thread.
       procedure RunFrame;

       // Frees the thread local memory that most API calls allocate. Only needed on threads other than
       // the one running RunFrame.
       procedure ReleaseCurrentThreadMemory;

       function IsSteamRunning:boolean;

       function GetSteamInstallPath:TpvUTF8String;

       // Turns a zero terminated API string into a Pascal one, tolerating nil.
       class function ToUTF8String(const aValue:PSteamChar):TpvUTF8String; static;

       // The counterpart for the many API methods that fill a caller provided character buffer. The
       // result is trimmed at the first zero byte, so the buffer size may be a generous guess.
       class function BufferToUTF8String(const aBuffer:TpvPointer;const aBufferSize:TpvSizeInt):TpvUTF8String; static;
      published
       property LibraryLoaded:boolean read fLibraryLoaded;
       property Initialized:boolean read fInitialized;
       property GameServerMode:boolean read fGameServerMode;
       property SteamPipe:THSteamPipe read fSteamPipe;
       property SteamUser:THSteamUser read fSteamUser;

       // Non localized explanation of the last failed initialization attempt, straight from Steam.
       property LastErrorMessage:TpvUTF8String read fLastErrorMessage;
      public

       // The raw interface pointers, valid between a successful Initialize and Shutdown. They are
       // meant to be passed straight into the flat SteamAPI_ISteamXxx_Yyy entry points. Pointer typed
       // properties cannot be published, so this block is plain public.
       property Client:PISteamClient read fClient;
       property User:PISteamUser read fUser;
       property Friends:PISteamFriends read fFriends;
       property Utils:PISteamUtils read fUtils;
       property Matchmaking:PISteamMatchmaking read fMatchmaking;
       property MatchmakingServers:PISteamMatchmakingServers read fMatchmakingServers;
       property UserStats:PISteamUserStats read fUserStats;
       property Apps:PISteamApps read fApps;
       property Networking:PISteamNetworking read fNetworking;
       property RemoteStorage:PISteamRemoteStorage read fRemoteStorage;
       property Screenshots:PISteamScreenshots read fScreenshots;
       property HTTP:PISteamHTTP read fHTTP;
       property UGC:PISteamUGC read fUGC;
       property Music:PISteamMusic read fMusic;
       property HTMLSurface:PISteamHTMLSurface read fHTMLSurface;
       property Inventory:PISteamInventory read fInventory;
       property Video:PISteamVideo read fVideo;
       property ParentalSettings:PISteamParentalSettings read fParentalSettings;
       property Input:PISteamInput read fInput;
       property Controller:PISteamController read fController;
       property Parties:PISteamParties read fParties;
       property RemotePlay:PISteamRemotePlay read fRemotePlay;
       property Timeline:PISteamTimeline read fTimeline;
       property NetworkingMessages:PISteamNetworkingMessages read fNetworkingMessages;
       property NetworkingSockets:PISteamNetworkingSockets read fNetworkingSockets;
       property NetworkingUtils:PISteamNetworkingUtils read fNetworkingUtils;
       property GameServer:PISteamGameServer read fGameServer;
       property GameServerStats:PISteamGameServerStats read fGameServerStats;
     end;

implementation

{ TpvSteamworks.TCallbackHandler }

constructor TpvSteamworks.TCallbackHandler.Create(const aSteamworks:TpvSteamworks;const aCallbackIdentifier:TpvInt32);
begin
 inherited Create;
 if not assigned(aSteamworks) then begin
  raise EpvSteamworksHandler.Create('Callback handler needs an owning TpvSteamworks instance');
 end;
 fSteamworks:=aSteamworks;
 fCallbackIdentifier:=aCallbackIdentifier;
 fSteamworks.InternalRegisterCallbackHandler(self);
end;

destructor TpvSteamworks.TCallbackHandler.Destroy;
begin
 if assigned(fSteamworks) then begin
  fSteamworks.InternalUnregisterCallbackHandler(self);
  fSteamworks:=nil;
 end;
 inherited Destroy;
end;

{ TpvSteamworks.TCallResultHandler }

constructor TpvSteamworks.TCallResultHandler.Create(const aSteamworks:TpvSteamworks;
                                                    const aAPICall:TSteamAPICall_t;
                                                    const aExpectedCallbackIdentifier:TpvInt32;
                                                    const aExpectedDataSize:TpvSizeInt);
begin
 inherited Create;
 if not assigned(aSteamworks) then begin
  raise EpvSteamworksHandler.Create('Call result handler needs an owning TpvSteamworks instance');
 end;
 if aAPICall=k_uAPICallInvalid then begin
  raise EpvSteamworksHandler.Create('Call result handler needs a valid SteamAPICall_t handle');
 end;
 fSteamworks:=aSteamworks;
 fAPICall:=aAPICall;
 fExpectedCallbackIdentifier:=aExpectedCallbackIdentifier;
 fExpectedDataSize:=aExpectedDataSize;
 fSteamworks.InternalRegisterCallResultHandler(self);
end;

destructor TpvSteamworks.TCallResultHandler.Destroy;
begin
 if assigned(fSteamworks) then begin
  fSteamworks.InternalUnregisterCallResultHandler(self);
  fSteamworks:=nil;
 end;
 inherited Destroy;
end;

{ TpvSteamworks.TServerListResponse }

// The thunks are plain cdecl procedures rather than methods, because that is what a C++ vtable slot
// holds. Each one finds its owning instance again through the UserData field of the object record.
procedure ServerListResponseServerRespondedThunk(const aSelf:PISteamMatchmakingServerListResponseObject;const hRequest:THServerListRequest;const iServer:TSteamInt32); cdecl;
begin
 TpvSteamworks.TServerListResponse(aSelf^.UserData).ServerResponded(hRequest,iServer);
end;

procedure ServerListResponseServerFailedToRespondThunk(const aSelf:PISteamMatchmakingServerListResponseObject;const hRequest:THServerListRequest;const iServer:TSteamInt32); cdecl;
begin
 TpvSteamworks.TServerListResponse(aSelf^.UserData).ServerFailedToRespond(hRequest,iServer);
end;

procedure ServerListResponseRefreshCompleteThunk(const aSelf:PISteamMatchmakingServerListResponseObject;const hRequest:THServerListRequest;const response:TEMatchMakingServerResponse); cdecl;
begin
 TpvSteamworks.TServerListResponse(aSelf^.UserData).RefreshComplete(hRequest,response);
end;

constructor TpvSteamworks.TServerListResponse.Create;
begin
 inherited Create;
 fVTable.ServerResponded:=ServerListResponseServerRespondedThunk;
 fVTable.ServerFailedToRespond:=ServerListResponseServerFailedToRespondThunk;
 fVTable.RefreshComplete:=ServerListResponseRefreshCompleteThunk;
 fResponseObject.VTable:=@fVTable;
 fResponseObject.UserData:=self;
end;

function TpvSteamworks.TServerListResponse.Handle:PISteamMatchmakingServerListResponse;
begin
 result:=PISteamMatchmakingServerListResponse(@fResponseObject);
end;

procedure TpvSteamworks.TServerListResponse.ServerResponded(const aRequest:THServerListRequest;const aServerIndex:TpvInt32);
begin
end;

procedure TpvSteamworks.TServerListResponse.ServerFailedToRespond(const aRequest:THServerListRequest;const aServerIndex:TpvInt32);
begin
end;

procedure TpvSteamworks.TServerListResponse.RefreshComplete(const aRequest:THServerListRequest;const aResponse:TEMatchMakingServerResponse);
begin
end;

{ TpvSteamworks.TServerPingResponse }

procedure ServerPingResponseServerRespondedThunk(const aSelf:PISteamMatchmakingPingResponseObject;const server:Pgameserveritem_t); cdecl;
begin
 TpvSteamworks.TServerPingResponse(aSelf^.UserData).ServerResponded(server);
end;

procedure ServerPingResponseServerFailedToRespondThunk(const aSelf:PISteamMatchmakingPingResponseObject); cdecl;
begin
 TpvSteamworks.TServerPingResponse(aSelf^.UserData).ServerFailedToRespond;
end;

constructor TpvSteamworks.TServerPingResponse.Create;
begin
 inherited Create;
 fVTable.ServerResponded:=ServerPingResponseServerRespondedThunk;
 fVTable.ServerFailedToRespond:=ServerPingResponseServerFailedToRespondThunk;
 fResponseObject.VTable:=@fVTable;
 fResponseObject.UserData:=self;
end;

function TpvSteamworks.TServerPingResponse.Handle:PISteamMatchmakingPingResponse;
begin
 result:=PISteamMatchmakingPingResponse(@fResponseObject);
end;

procedure TpvSteamworks.TServerPingResponse.ServerResponded(const aServer:Pgameserveritem_t);
begin
end;

procedure TpvSteamworks.TServerPingResponse.ServerFailedToRespond;
begin
end;

{ TpvSteamworks.TServerPlayersResponse }

procedure ServerPlayersResponseAddPlayerToListThunk(const aSelf:PISteamMatchmakingPlayersResponseObject;const pchName:PSteamChar;const nScore:TSteamInt32;const flTimePlayed:TSteamFloat); cdecl;
begin
 TpvSteamworks.TServerPlayersResponse(aSelf^.UserData).AddPlayerToList(pchName,nScore,flTimePlayed);
end;

procedure ServerPlayersResponsePlayersFailedToRespondThunk(const aSelf:PISteamMatchmakingPlayersResponseObject); cdecl;
begin
 TpvSteamworks.TServerPlayersResponse(aSelf^.UserData).PlayersFailedToRespond;
end;

procedure ServerPlayersResponsePlayersRefreshCompleteThunk(const aSelf:PISteamMatchmakingPlayersResponseObject); cdecl;
begin
 TpvSteamworks.TServerPlayersResponse(aSelf^.UserData).PlayersRefreshComplete;
end;

constructor TpvSteamworks.TServerPlayersResponse.Create;
begin
 inherited Create;
 fVTable.AddPlayerToList:=ServerPlayersResponseAddPlayerToListThunk;
 fVTable.PlayersFailedToRespond:=ServerPlayersResponsePlayersFailedToRespondThunk;
 fVTable.PlayersRefreshComplete:=ServerPlayersResponsePlayersRefreshCompleteThunk;
 fResponseObject.VTable:=@fVTable;
 fResponseObject.UserData:=self;
end;

function TpvSteamworks.TServerPlayersResponse.Handle:PISteamMatchmakingPlayersResponse;
begin
 result:=PISteamMatchmakingPlayersResponse(@fResponseObject);
end;

procedure TpvSteamworks.TServerPlayersResponse.AddPlayerToList(const aName:PSteamChar;const aScore:TpvInt32;const aTimePlayed:TpvFloat);
begin
end;

procedure TpvSteamworks.TServerPlayersResponse.PlayersFailedToRespond;
begin
end;

procedure TpvSteamworks.TServerPlayersResponse.PlayersRefreshComplete;
begin
end;

{ TpvSteamworks.TServerRulesResponse }

procedure ServerRulesResponseRulesRespondedThunk(const aSelf:PISteamMatchmakingRulesResponseObject;const pchRule:PSteamChar;const pchValue:PSteamChar); cdecl;
begin
 TpvSteamworks.TServerRulesResponse(aSelf^.UserData).RulesResponded(pchRule,pchValue);
end;

procedure ServerRulesResponseRulesFailedToRespondThunk(const aSelf:PISteamMatchmakingRulesResponseObject); cdecl;
begin
 TpvSteamworks.TServerRulesResponse(aSelf^.UserData).RulesFailedToRespond;
end;

procedure ServerRulesResponseRulesRefreshCompleteThunk(const aSelf:PISteamMatchmakingRulesResponseObject); cdecl;
begin
 TpvSteamworks.TServerRulesResponse(aSelf^.UserData).RulesRefreshComplete;
end;

constructor TpvSteamworks.TServerRulesResponse.Create;
begin
 inherited Create;
 fVTable.RulesResponded:=ServerRulesResponseRulesRespondedThunk;
 fVTable.RulesFailedToRespond:=ServerRulesResponseRulesFailedToRespondThunk;
 fVTable.RulesRefreshComplete:=ServerRulesResponseRulesRefreshCompleteThunk;
 fResponseObject.VTable:=@fVTable;
 fResponseObject.UserData:=self;
end;

function TpvSteamworks.TServerRulesResponse.Handle:PISteamMatchmakingRulesResponse;
begin
 result:=PISteamMatchmakingRulesResponse(@fResponseObject);
end;

procedure TpvSteamworks.TServerRulesResponse.RulesResponded(const aRule:PSteamChar;const aValue:PSteamChar);
begin
end;

procedure TpvSteamworks.TServerRulesResponse.RulesFailedToRespond;
begin
end;

procedure TpvSteamworks.TServerRulesResponse.RulesRefreshComplete;
begin
end;

{ TpvSteamworks }

constructor TpvSteamworks.Create;
begin

 inherited Create;

 fLibraryLoaded:=false;
 fInitialized:=false;
 fGameServerMode:=false;
 fSteamPipe:=0;
 fSteamUser:=0;
 fLastErrorMessage:='';

 fLock:=TPasMPMultipleReaderSingleWriterLock.Create;

 // The handler lists are owned by the map, but the handlers themselves are not owned by their list,
 // since a handler unregisters itself from its own destructor.
 fCallbackHandlerListHashMap:=TCallbackHandlerListHashMap.Create(nil);
 fCallResultHandlerHashMap:=TCallResultHandlerHashMap.Create(nil);

 fCallResultBuffer:=nil;

 ReleaseInterfaces;

end;

destructor TpvSteamworks.Destroy;
var CallbackHandlerList:TCallbackHandlerList;
begin

 Shutdown;

 // The lists belong to this instance even though the handlers inside them do not.
 if assigned(fCallbackHandlerListHashMap) then begin
  for CallbackHandlerList in fCallbackHandlerListHashMap.Values do begin
   CallbackHandlerList.Free;
  end;
 end;

 FreeAndNil(fCallResultHandlerHashMap);
 FreeAndNil(fCallbackHandlerListHashMap);
 FreeAndNil(fLock);

 fCallResultBuffer:=nil;

 inherited Destroy;

end;

procedure TpvSteamworks.ReleaseInterfaces;
begin
 fClient:=nil;
 fUser:=nil;
 fFriends:=nil;
 fUtils:=nil;
 fMatchmaking:=nil;
 fMatchmakingServers:=nil;
 fUserStats:=nil;
 fApps:=nil;
 fNetworking:=nil;
 fRemoteStorage:=nil;
 fScreenshots:=nil;
 fHTTP:=nil;
 fUGC:=nil;
 fMusic:=nil;
 fHTMLSurface:=nil;
 fInventory:=nil;
 fVideo:=nil;
 fParentalSettings:=nil;
 fInput:=nil;
 fController:=nil;
 fParties:=nil;
 fRemotePlay:=nil;
 fTimeline:=nil;
 fNetworkingMessages:=nil;
 fNetworkingSockets:=nil;
 fNetworkingUtils:=nil;
 fGameServer:=nil;
 fGameServerStats:=nil;
end;

// The versioned accessors are the documented way in, and they are what the interface version blob of
// the initialization call validated against the loaded library.
procedure TpvSteamworks.AcquireInterfaces;
begin

 fClient:=SteamClient;
 fUtils:=SteamAPI_SteamUtils_v010;
 fNetworkingUtils:=SteamAPI_SteamNetworkingUtils_SteamAPI_v004;
 fNetworkingMessages:=SteamAPI_SteamNetworkingMessages_SteamAPI_v002;
 fNetworkingSockets:=SteamAPI_SteamNetworkingSockets_SteamAPI_v012;
 fHTTP:=SteamAPI_SteamHTTP_v003;
 fInventory:=SteamAPI_SteamInventory_v003;
 fUGC:=SteamAPI_SteamUGC_v021;
 fNetworking:=SteamAPI_SteamNetworking_v006;

 if fGameServerMode then begin

  // A game server has its own flavour of a few interfaces and none of the user facing ones.
  fGameServer:=SteamAPI_SteamGameServer_v015;
  fGameServerStats:=SteamAPI_SteamGameServerStats_v001;

 end else begin

  fUser:=SteamAPI_SteamUser_v023;
  fFriends:=SteamAPI_SteamFriends_v018;
  fMatchmaking:=SteamAPI_SteamMatchmaking_v009;
  fMatchmakingServers:=SteamAPI_SteamMatchmakingServers_v002;
  fUserStats:=SteamAPI_SteamUserStats_v013;
  fApps:=SteamAPI_SteamApps_v009;
  fRemoteStorage:=SteamAPI_SteamRemoteStorage_v016;
  fScreenshots:=SteamAPI_SteamScreenshots_v003;
  fMusic:=SteamAPI_SteamMusic_v001;
  fHTMLSurface:=SteamAPI_SteamHTMLSurface_v005;
  fVideo:=SteamAPI_SteamVideo_v007;
  fParentalSettings:=SteamAPI_SteamParentalSettings_v001;
  fInput:=SteamAPI_SteamInput_v006;
  fController:=SteamAPI_SteamController_v008;
  fParties:=SteamAPI_SteamParties_v002;
  fRemotePlay:=SteamAPI_SteamRemotePlay_v004;
  fTimeline:=SteamAPI_SteamTimeline_v004;

 end;

end;

function TpvSteamworks.LoadLibrary(const aLibraryName:string=STEAMWORKS_DEFAULT_LIB_NAME):boolean;
begin
 if fLibraryLoaded then begin
  result:=true;
 end else begin
  fLibraryLoaded:=LoadSteamworksLibrary(aLibraryName);
  result:=fLibraryLoaded;
  if not result then begin
   fLastErrorMessage:='Could not load "'+TpvUTF8String(aLibraryName)+'"';
  end;
 end;
end;

function TpvSteamworks.RestartAppIfNecessary(const aOwnAppID:TpvUInt32):boolean;
begin
 if fLibraryLoaded then begin
  result:=SteamAPI_RestartAppIfNecessary(aOwnAppID);
 end else begin
  result:=false;
 end;
end;

function TpvSteamworks.Initialize(const aLibraryName:string=STEAMWORKS_DEFAULT_LIB_NAME):TESteamAPIInitResult;
var ErrorMessage:TSteamErrMsg;
begin

 if fInitialized then begin
  result:=k_ESteamAPIInitResult_OK;
  exit;
 end;

 if not LoadLibrary(aLibraryName) then begin
  result:=k_ESteamAPIInitResult_FailedGeneric;
  exit;
 end;

 FillChar(ErrorMessage,SizeOf(TSteamErrMsg),#0);
 result:=SteamAPI_InitEx(@ErrorMessage);
 if result<>k_ESteamAPIInitResult_OK then begin
  fLastErrorMessage:=BufferToUTF8String(@ErrorMessage,SizeOf(TSteamErrMsg));
  exit;
 end;

 fInitialized:=true;
 fGameServerMode:=false;
 fLastErrorMessage:='';

 // Manual dispatch has to be switched on after initialization and before any of the other manual
 // dispatch entry points, and it rules out SteamAPI_RunCallbacks from then on.
 SteamAPI_ManualDispatch_Init;

 fSteamPipe:=SteamAPI_GetHSteamPipe;
 fSteamUser:=SteamAPI_GetHSteamUser;

 AcquireInterfaces;

end;

function TpvSteamworks.InitializeGameServer(const aIP:TpvUInt32;
                                            const aGamePort:TpvUInt16;
                                            const aQueryPort:TpvUInt16;
                                            const aServerMode:TEServerMode;
                                            const aVersionString:TpvUTF8String;
                                            const aLibraryName:string=STEAMWORKS_DEFAULT_LIB_NAME):TESteamAPIInitResult;
var ErrorMessage:TSteamErrMsg;
begin

 if fInitialized then begin
  result:=k_ESteamAPIInitResult_OK;
  exit;
 end;

 if not LoadLibrary(aLibraryName) then begin
  result:=k_ESteamAPIInitResult_FailedGeneric;
  exit;
 end;

 FillChar(ErrorMessage,SizeOf(TSteamErrMsg),#0);
 result:=SteamGameServer_InitEx(aIP,aGamePort,aQueryPort,aServerMode,PSteamChar(aVersionString),@ErrorMessage);
 if result<>k_ESteamAPIInitResult_OK then begin
  fLastErrorMessage:=BufferToUTF8String(@ErrorMessage,SizeOf(TSteamErrMsg));
  exit;
 end;

 fInitialized:=true;
 fGameServerMode:=true;
 fLastErrorMessage:='';

 SteamAPI_ManualDispatch_Init;

 fSteamPipe:=SteamGameServer_GetHSteamPipe;
 fSteamUser:=SteamGameServer_GetHSteamUser;

 AcquireInterfaces;

end;

procedure TpvSteamworks.Shutdown;
begin

 if fInitialized then begin

  ReleaseInterfaces;

  if fGameServerMode then begin
   SteamGameServer_Shutdown;
  end else begin
   SteamAPI_Shutdown;
  end;

  fSteamPipe:=0;
  fSteamUser:=0;
  fInitialized:=false;
  fGameServerMode:=false;

 end;

 if fLibraryLoaded then begin
  UnloadSteamworksLibrary;
  fLibraryLoaded:=false;
 end;

end;

procedure TpvSteamworks.InternalRegisterCallbackHandler(const aCallbackHandler:TCallbackHandler);
var CallbackHandlerList:TCallbackHandlerList;
begin
 fLock.AcquireWrite;
 try
  CallbackHandlerList:=fCallbackHandlerListHashMap[aCallbackHandler.fCallbackIdentifier];
  if not assigned(CallbackHandlerList) then begin
   // The list does not own its handlers, they own themselves and drop out on destruction.
   CallbackHandlerList:=TCallbackHandlerList.Create(false);
   fCallbackHandlerListHashMap[aCallbackHandler.fCallbackIdentifier]:=CallbackHandlerList;
  end;
  if CallbackHandlerList.IndexOf(aCallbackHandler)<0 then begin
   CallbackHandlerList.Add(aCallbackHandler);
  end;
 finally
  fLock.ReleaseWrite;
 end;
end;

procedure TpvSteamworks.InternalUnregisterCallbackHandler(const aCallbackHandler:TCallbackHandler);
var CallbackHandlerList:TCallbackHandlerList;
begin
 fLock.AcquireWrite;
 try
  CallbackHandlerList:=fCallbackHandlerListHashMap[aCallbackHandler.fCallbackIdentifier];
  if assigned(CallbackHandlerList) then begin
   CallbackHandlerList.Remove(aCallbackHandler);
  end;
 finally
  fLock.ReleaseWrite;
 end;
end;

procedure TpvSteamworks.InternalRegisterCallResultHandler(const aCallResultHandler:TCallResultHandler);
begin
 fLock.AcquireWrite;
 try
  fCallResultHandlerHashMap[aCallResultHandler.fAPICall]:=aCallResultHandler;
 finally
  fLock.ReleaseWrite;
 end;
end;

procedure TpvSteamworks.InternalUnregisterCallResultHandler(const aCallResultHandler:TCallResultHandler);
begin
 fLock.AcquireWrite;
 try
  if fCallResultHandlerHashMap[aCallResultHandler.fAPICall]=aCallResultHandler then begin
   fCallResultHandlerHashMap.Delete(aCallResultHandler.fAPICall);
  end;
 finally
  fLock.ReleaseWrite;
 end;
end;

// Handlers are collected under the read lock and called outside of it, so that a handler is free to
// register or destroy handlers without deadlocking against the dispatch itself.
procedure TpvSteamworks.DispatchCallback(const aCallbackMessage:PCallbackMsg_t);
var Index:TpvSizeInt;
    CallbackHandlerList:TCallbackHandlerList;
    CallbackHandlers:array of TCallbackHandler;
    CountCallbackHandlers:TpvSizeInt;
begin

 CallbackHandlers:=nil;
 CountCallbackHandlers:=0;

 fLock.AcquireRead;
 try
  CallbackHandlerList:=fCallbackHandlerListHashMap[aCallbackMessage^.m_iCallback];
  if assigned(CallbackHandlerList) and (CallbackHandlerList.Count>0) then begin
   CountCallbackHandlers:=CallbackHandlerList.Count;
   SetLength(CallbackHandlers,CountCallbackHandlers);
   for Index:=0 to CountCallbackHandlers-1 do begin
    CallbackHandlers[Index]:=CallbackHandlerList.Items[Index];
   end;
  end;
 finally
  fLock.ReleaseRead;
 end;

 for Index:=0 to CountCallbackHandlers-1 do begin
  CallbackHandlers[Index].Handle(aCallbackMessage^.m_pubParam,aCallbackMessage^.m_cubParam);
 end;

 CallbackHandlers:=nil;

end;

// A SteamAPICallCompleted_t callback only says that some asynchronous call has finished. The payload
// has to be fetched separately, into a buffer of the size that the completion announces.
procedure TpvSteamworks.DispatchCallResult(const aCallbackMessage:PCallbackMsg_t);
var CallCompleted:PSteamAPICallCompleted_t;
    CallResultHandler:TCallResultHandler;
    DataSize:TpvSizeInt;
    Failed:TSteamBool;
begin

 if aCallbackMessage^.m_cubParam<TpvSizeInt(SizeOf(TSteamAPICallCompleted_t)) then begin
  exit;
 end;

 CallCompleted:=PSteamAPICallCompleted_t(aCallbackMessage^.m_pubParam);

 fLock.AcquireRead;
 try
  CallResultHandler:=fCallResultHandlerHashMap[CallCompleted^.m_hAsyncCall];
 finally
  fLock.ReleaseRead;
 end;

 // Not ours, so somebody else asked for this call and will pick the result up.
 if not assigned(CallResultHandler) then begin
  exit;
 end;

 // Steam knows the real size, and the handler's expectation is only the lower bound that its own cast
 // needs. Taking the larger of the two keeps a newer client that grew a record from overrunning us.
 DataSize:=CallCompleted^.m_cubParam;
 if DataSize<CallResultHandler.fExpectedDataSize then begin
  DataSize:=CallResultHandler.fExpectedDataSize;
 end;
 if length(fCallResultBuffer)<DataSize then begin
  SetLength(fCallResultBuffer,DataSize);
 end;
 FillChar(fCallResultBuffer[0],DataSize,#0);

 Failed:=false;
 if SteamAPI_ManualDispatch_GetAPICallResult(fSteamPipe,
                                             CallCompleted^.m_hAsyncCall,
                                             @fCallResultBuffer[0],
                                             CallCompleted^.m_cubParam,
                                             CallCompleted^.m_iCallback,
                                             @Failed) then begin
  CallResultHandler.Handle(@fCallResultBuffer[0],CallCompleted^.m_cubParam,Failed);
 end else begin
  CallResultHandler.Handle(nil,0,true);
 end;

 // A call result fires once, so its registration goes away here rather than waiting for the handler to
 // be destroyed.
 fLock.AcquireWrite;
 try
  if fCallResultHandlerHashMap[CallCompleted^.m_hAsyncCall]=CallResultHandler then begin
   fCallResultHandlerHashMap.Delete(CallCompleted^.m_hAsyncCall);
  end;
 finally
  fLock.ReleaseWrite;
 end;

end;

procedure TpvSteamworks.RunFrame;
var CallbackMessage:TCallbackMsg_t;
begin

 if not fInitialized then begin
  exit;
 end;

 SteamAPI_ManualDispatch_RunFrame(fSteamPipe);

 // Every fetched callback has to be freed again before the next one may be fetched, hence the
 // try..finally around the dispatch.
 while SteamAPI_ManualDispatch_GetNextCallback(fSteamPipe,@CallbackMessage) do begin
  try
   if CallbackMessage.m_iCallback=SteamAPICallCompleted_t_k_iCallback then begin
    DispatchCallResult(@CallbackMessage);
   end else begin
    DispatchCallback(@CallbackMessage);
   end;
  finally
   SteamAPI_ManualDispatch_FreeLastCallback(fSteamPipe);
  end;
 end;

end;

procedure TpvSteamworks.ReleaseCurrentThreadMemory;
begin
 if fLibraryLoaded then begin
  SteamAPI_ReleaseCurrentThreadMemory;
 end;
end;

function TpvSteamworks.IsSteamRunning:boolean;
begin
 if fLibraryLoaded then begin
  result:=SteamAPI_IsSteamRunning;
 end else begin
  result:=false;
 end;
end;

function TpvSteamworks.GetSteamInstallPath:TpvUTF8String;
begin
 if fLibraryLoaded then begin
  result:=ToUTF8String(SteamAPI_GetSteamInstallPath);
 end else begin
  result:='';
 end;
end;

class function TpvSteamworks.ToUTF8String(const aValue:PSteamChar):TpvUTF8String;
begin
 if assigned(aValue) then begin
  result:=TpvUTF8String(aValue);
 end else begin
  result:='';
 end;
end;

class function TpvSteamworks.BufferToUTF8String(const aBuffer:TpvPointer;const aBufferSize:TpvSizeInt):TpvUTF8String;
var Index:TpvSizeInt;
    Characters:PSteamChar;
begin

 result:='';
 if not (assigned(aBuffer) and (aBufferSize>0)) then begin
  exit;
 end;

 // The API fills these buffers with a zero terminated string, but a truncated answer may well use the
 // last byte, so the size is the fallback end.
 Characters:=PSteamChar(aBuffer);
 Index:=0;
 while (Index<aBufferSize) and (Characters[Index]<>#0) do begin
  inc(Index);
 end;

 if Index>0 then begin
  SetLength(result,Index);
  Move(Characters[0],result[1],Index);
 end;

end;

end.
