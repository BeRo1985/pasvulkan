(*
** Copyright (c) Valve Corporation, All rights reserved. (the Steamworks SDK)
** Copyright (c) 2026, Benjamin Rosseaux (benjamin@rosseaux.de, the pascal headers)
**
** Permission is hereby granted, free of charge, to any person obtaining a
** copy of this software and/or associated documentation files (the
** "Materials"), to deal in the Materials without restriction, including
** without limitation the rights to use, copy, modify, merge, publish,
** distribute, sublicense, and/or sell copies of the Materials, and to
** permit persons to whom the Materials are furnished to do so, subject to
** the following conditions:
**
** The above copyright notice and this permission notice shall be included
** in all copies or substantial portions of the Materials.
**
** THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
** MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
*)
(*
** This header is generated from the Steamworks SDK API description file steam_api.json by
** the steamapi2pas tool. Do not edit it, edit the generator instead.
**
** The bindings target the flat C API of the Steamworks redistributable libraries, where every
** interface method is exported as a plain cdecl symbol taking the interface pointer as its
** first argument, so no C++ vtable is ever involved.
*)
unit PasVulkan.Steamworks;
{$ifdef fpc}
 {$mode delphi}
 {$define CAN_INLINE}
 {$notes off}
{$else}
 {$undef CAN_INLINE}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
  {$if CompilerVersion>=18.0}
   {$define CAN_INLINE}
  {$ifend}
 {$endif}
{$endif}
{$ifdef Win32}
 {$define Windows}
{$endif}
{$ifdef Win64}
 {$define Windows}
{$endif}
{$rangechecks off}
{$hints off}
{$scopedenums off}

interface

uses {$if defined(Windows)}
      Windows,
     {$elseif defined(Unix)}
      dl,
     {$ifend}
     SysUtils;

const STEAMWORKS_DEFAULT_LIB_NAME={$ifdef Windows}{$ifdef cpu64}'steam_api64.dll'{$else}'steam_api.dll'{$endif}{$else}{$ifdef Darwin}'libsteam_api.dylib'{$else}'libsteam_api.so'{$endif}{$endif};

      STEAMWORKS_ENCRYPTED_APP_TICKET_DEFAULT_LIB_NAME={$ifdef Windows}{$ifdef cpu64}'sdkencryptedappticket64.dll'{$else}'sdkencryptedappticket.dll'{$endif}{$else}{$ifdef Darwin}'libsdkencryptedappticket.dylib'{$else}'libsdkencryptedappticket.so'{$endif}{$endif};

// The Steamworks headers pack their callback records with 4 bytes on Linux, macOS and FreeBSD
// (VALVE_CALLBACK_PACK_SMALL) and with 8 bytes on Windows (VALVE_CALLBACK_PACK_LARGE).
{$ifdef Windows}{$ifdef fpc}{$packrecords 8}{$else}{$A8}{$endif}{$else}{$ifdef fpc}{$packrecords 4}{$else}{$A4}{$endif}{$endif}

type PPSteamInt8=^PSteamInt8;
     PSteamInt8=^TSteamInt8;
     TSteamInt8={$ifdef fpc}Int8{$else}ShortInt{$endif};

     PPSteamUInt8=^PSteamUInt8;
     PSteamUInt8=^TSteamUInt8;
     TSteamUInt8={$ifdef fpc}UInt8{$else}Byte{$endif};

     PPSteamInt16=^PSteamInt16;
     PSteamInt16=^TSteamInt16;
     TSteamInt16={$ifdef fpc}Int16{$else}SmallInt{$endif};

     PPSteamUInt16=^PSteamUInt16;
     PSteamUInt16=^TSteamUInt16;
     TSteamUInt16={$ifdef fpc}UInt16{$else}Word{$endif};

     PPSteamInt32=^PSteamInt32;
     PSteamInt32=^TSteamInt32;
     TSteamInt32={$ifdef fpc}Int32{$else}LongInt{$endif};

     PPSteamUInt32=^PSteamUInt32;
     PSteamUInt32=^TSteamUInt32;
     TSteamUInt32={$ifdef fpc}UInt32{$else}LongWord{$endif};

     PPSteamInt64=^PSteamInt64;
     PSteamInt64=^TSteamInt64;
     TSteamInt64=Int64;

     PPSteamUInt64=^PSteamUInt64;
     PSteamUInt64=^TSteamUInt64;
     TSteamUInt64=UInt64;

     PPSteamPtrInt=^PSteamPtrInt;
     PSteamPtrInt=^TSteamPtrInt;
     TSteamPtrInt={$ifdef fpc}PtrInt{$else}NativeInt{$endif};

     PPSteamPtrUInt=^PSteamPtrUInt;
     PSteamPtrUInt=^TSteamPtrUInt;
     TSteamPtrUInt={$ifdef fpc}PtrUInt{$else}NativeUInt{$endif};

     PPSteamFloat=^PSteamFloat;
     PSteamFloat=^TSteamFloat;
     TSteamFloat=Single;

     PPSteamDouble=^PSteamDouble;
     PSteamDouble=^TSteamDouble;
     TSteamDouble=Double;

     PPSteamChar=^PSteamChar;
     PSteamChar=PAnsiChar;
     TSteamChar=AnsiChar;

     PPSteamPointer=^PSteamPointer;
     PSteamPointer=^TSteamPointer;
     TSteamPointer=Pointer;

     PPSteamAnsiString=^PSteamAnsiString;
     PSteamAnsiString=^TSteamAnsiString;
     TSteamAnsiString={$if declared(RawByteString)}RawByteString{$else}AnsiString{$ifend};

     PPSteamBool=^PSteamBool;
     PSteamBool=^TSteamBool;
     TSteamBool=ByteBool;

     PPSteamUInt64SteamID=^PSteamUInt64SteamID;
     PSteamUInt64SteamID=^TSteamUInt64SteamID;
     TSteamUInt64SteamID=TSteamUInt64;

     PPSteamUInt64GameID=^PSteamUInt64GameID;
     PSteamUInt64GameID=^TSteamUInt64GameID;
     TSteamUInt64GameID=TSteamUInt64;

type PPSteamAPIWarningMessageHook_t=^PSteamAPIWarningMessageHook_t;
     PSteamAPIWarningMessageHook_t=^TSteamAPIWarningMessageHook_t;
     TSteamAPIWarningMessageHook_t=procedure(const aSeverity:TSteamInt32;const aDebugText:PSteamChar); cdecl;

     { TISteamNetworkingConnectionSignaling }
     PPISteamNetworkingConnectionSignaling=^PISteamNetworkingConnectionSignaling;
     PISteamNetworkingConnectionSignaling=^TISteamNetworkingConnectionSignaling;
     TISteamNetworkingConnectionSignaling=record
     end;

     { TISteamNetworkingSignalingRecvContext }
     PPISteamNetworkingSignalingRecvContext=^PISteamNetworkingSignalingRecvContext;
     PISteamNetworkingSignalingRecvContext=^TISteamNetworkingSignalingRecvContext;
     TISteamNetworkingSignalingRecvContext=record
     end;

     { TSteamDatagramRelayAuthTicket }
     PPSteamDatagramRelayAuthTicket=^PSteamDatagramRelayAuthTicket;
     PSteamDatagramRelayAuthTicket=^TSteamDatagramRelayAuthTicket;
     TSteamDatagramRelayAuthTicket=record
     end;

     { TScePadTriggerEffectParam }
     PPScePadTriggerEffectParam=^PScePadTriggerEffectParam;
     PScePadTriggerEffectParam=^TScePadTriggerEffectParam;
     TScePadTriggerEffectParam=record
     end;

type { TEHTMLMouseButton }
     PPEHTMLMouseButton=^PEHTMLMouseButton;
     PEHTMLMouseButton=^TEHTMLMouseButton;
     TEHTMLMouseButton=TSteamInt32;

const eHTMLMouseButton_Left=0;
      eHTMLMouseButton_Right=1;
      eHTMLMouseButton_Middle=2;

type { TEHTMLKeyModifiers }
     PPEHTMLKeyModifiers=^PEHTMLKeyModifiers;
     PEHTMLKeyModifiers=^TEHTMLKeyModifiers;
     TEHTMLKeyModifiers=TSteamInt32;

const k_eHTMLKeyModifier_None=0;
      k_eHTMLKeyModifier_AltDown=1 shl 0;
      k_eHTMLKeyModifier_CtrlDown=1 shl 1;
      k_eHTMLKeyModifier_ShiftDown=1 shl 2;

type PPAppId_t=^PAppId_t;
     PAppId_t=^TAppId_t;
     TAppId_t=TSteamUInt32;

type PPDepotId_t=^PDepotId_t;
     PDepotId_t=^TDepotId_t;
     TDepotId_t=TSteamUInt32;

type PPRTime32=^PRTime32;
     PRTime32=^TRTime32;
     TRTime32=TSteamUInt32;

type PPSteamAPICall_t=^PSteamAPICall_t;
     PSteamAPICall_t=^TSteamAPICall_t;
     TSteamAPICall_t=TSteamUInt64;

type PPAccountID_t=^PAccountID_t;
     PAccountID_t=^TAccountID_t;
     TAccountID_t=TSteamUInt32;

type PPPartyBeaconID_t=^PPartyBeaconID_t;
     PPartyBeaconID_t=^TPartyBeaconID_t;
     TPartyBeaconID_t=TSteamUInt64;

type PPHAuthTicket=^PHAuthTicket;
     PHAuthTicket=^THAuthTicket;
     THAuthTicket=TSteamUInt32;

type PPHSteamPipe=^PHSteamPipe;
     PHSteamPipe=^THSteamPipe;
     THSteamPipe=TSteamInt32;

type PPHSteamUser=^PHSteamUser;
     PHSteamUser=^THSteamUser;
     THSteamUser=TSteamInt32;

type PPFriendsGroupID_t=^PFriendsGroupID_t;
     PFriendsGroupID_t=^TFriendsGroupID_t;
     TFriendsGroupID_t=TSteamInt16;

type PPHServerListRequest=^PHServerListRequest;
     PHServerListRequest=^THServerListRequest;
     THServerListRequest=TSteamPointer;

type PPHServerQuery=^PHServerQuery;
     PHServerQuery=^THServerQuery;
     THServerQuery=TSteamInt32;

type PPUGCHandle_t=^PUGCHandle_t;
     PUGCHandle_t=^TUGCHandle_t;
     TUGCHandle_t=TSteamUInt64;

type PPPublishedFileUpdateHandle_t=^PPublishedFileUpdateHandle_t;
     PPublishedFileUpdateHandle_t=^TPublishedFileUpdateHandle_t;
     TPublishedFileUpdateHandle_t=TSteamUInt64;

type PPPublishedFileId_t=^PPublishedFileId_t;
     PPublishedFileId_t=^TPublishedFileId_t;
     TPublishedFileId_t=TSteamUInt64;

type PPUGCFileWriteStreamHandle_t=^PUGCFileWriteStreamHandle_t;
     PUGCFileWriteStreamHandle_t=^TUGCFileWriteStreamHandle_t;
     TUGCFileWriteStreamHandle_t=TSteamUInt64;

type PPSteamLeaderboard_t=^PSteamLeaderboard_t;
     PSteamLeaderboard_t=^TSteamLeaderboard_t;
     TSteamLeaderboard_t=TSteamUInt64;

type PPSteamLeaderboardEntries_t=^PSteamLeaderboardEntries_t;
     PSteamLeaderboardEntries_t=^TSteamLeaderboardEntries_t;
     TSteamLeaderboardEntries_t=TSteamUInt64;

type PPSNetSocket_t=^PSNetSocket_t;
     PSNetSocket_t=^TSNetSocket_t;
     TSNetSocket_t=TSteamUInt32;

type PPSNetListenSocket_t=^PSNetListenSocket_t;
     PSNetListenSocket_t=^TSNetListenSocket_t;
     TSNetListenSocket_t=TSteamUInt32;

type PPScreenshotHandle=^PScreenshotHandle;
     PScreenshotHandle=^TScreenshotHandle;
     TScreenshotHandle=TSteamUInt32;

type PPHTTPRequestHandle=^PHTTPRequestHandle;
     PHTTPRequestHandle=^THTTPRequestHandle;
     THTTPRequestHandle=TSteamUInt32;

type PPHTTPCookieContainerHandle=^PHTTPCookieContainerHandle;
     PHTTPCookieContainerHandle=^THTTPCookieContainerHandle;
     THTTPCookieContainerHandle=TSteamUInt32;

type PPInputHandle_t=^PInputHandle_t;
     PInputHandle_t=^TInputHandle_t;
     TInputHandle_t=TSteamUInt64;

type PPInputActionSetHandle_t=^PInputActionSetHandle_t;
     PInputActionSetHandle_t=^TInputActionSetHandle_t;
     TInputActionSetHandle_t=TSteamUInt64;

type PPInputDigitalActionHandle_t=^PInputDigitalActionHandle_t;
     PInputDigitalActionHandle_t=^TInputDigitalActionHandle_t;
     TInputDigitalActionHandle_t=TSteamUInt64;

type PPInputAnalogActionHandle_t=^PInputAnalogActionHandle_t;
     PInputAnalogActionHandle_t=^TInputAnalogActionHandle_t;
     TInputAnalogActionHandle_t=TSteamUInt64;

type PPControllerHandle_t=^PControllerHandle_t;
     PControllerHandle_t=^TControllerHandle_t;
     TControllerHandle_t=TSteamUInt64;

type PPControllerActionSetHandle_t=^PControllerActionSetHandle_t;
     PControllerActionSetHandle_t=^TControllerActionSetHandle_t;
     TControllerActionSetHandle_t=TSteamUInt64;

type PPControllerDigitalActionHandle_t=^PControllerDigitalActionHandle_t;
     PControllerDigitalActionHandle_t=^TControllerDigitalActionHandle_t;
     TControllerDigitalActionHandle_t=TSteamUInt64;

type PPControllerAnalogActionHandle_t=^PControllerAnalogActionHandle_t;
     PControllerAnalogActionHandle_t=^TControllerAnalogActionHandle_t;
     TControllerAnalogActionHandle_t=TSteamUInt64;

type PPUGCQueryHandle_t=^PUGCQueryHandle_t;
     PUGCQueryHandle_t=^TUGCQueryHandle_t;
     TUGCQueryHandle_t=TSteamUInt64;

type PPUGCUpdateHandle_t=^PUGCUpdateHandle_t;
     PUGCUpdateHandle_t=^TUGCUpdateHandle_t;
     TUGCUpdateHandle_t=TSteamUInt64;

type PPHHTMLBrowser=^PHHTMLBrowser;
     PHHTMLBrowser=^THHTMLBrowser;
     THHTMLBrowser=TSteamUInt32;

type PPSteamItemInstanceID_t=^PSteamItemInstanceID_t;
     PSteamItemInstanceID_t=^TSteamItemInstanceID_t;
     TSteamItemInstanceID_t=TSteamUInt64;

type PPSteamItemDef_t=^PSteamItemDef_t;
     PSteamItemDef_t=^TSteamItemDef_t;
     TSteamItemDef_t=TSteamInt32;

type PPSteamInventoryResult_t=^PSteamInventoryResult_t;
     PSteamInventoryResult_t=^TSteamInventoryResult_t;
     TSteamInventoryResult_t=TSteamInt32;

type PPSteamInventoryUpdateHandle_t=^PSteamInventoryUpdateHandle_t;
     PSteamInventoryUpdateHandle_t=^TSteamInventoryUpdateHandle_t;
     TSteamInventoryUpdateHandle_t=TSteamUInt64;

type PPTimelineEventHandle_t=^PTimelineEventHandle_t;
     PTimelineEventHandle_t=^TTimelineEventHandle_t;
     TTimelineEventHandle_t=TSteamUInt64;

type PPRemotePlaySessionID_t=^PRemotePlaySessionID_t;
     PRemotePlaySessionID_t=^TRemotePlaySessionID_t;
     TRemotePlaySessionID_t=TSteamUInt32;

type PPRemotePlayCursorID_t=^PRemotePlayCursorID_t;
     PRemotePlayCursorID_t=^TRemotePlayCursorID_t;
     TRemotePlayCursorID_t=TSteamUInt32;

type PPHSteamNetConnection=^PHSteamNetConnection;
     PHSteamNetConnection=^THSteamNetConnection;
     THSteamNetConnection=TSteamUInt32;

type PPHSteamListenSocket=^PHSteamListenSocket;
     PHSteamListenSocket=^THSteamListenSocket;
     THSteamListenSocket=TSteamUInt32;

type PPHSteamNetPollGroup=^PHSteamNetPollGroup;
     PHSteamNetPollGroup=^THSteamNetPollGroup;
     THSteamNetPollGroup=TSteamUInt32;

type PPSteamNetworkingPOPID=^PSteamNetworkingPOPID;
     PSteamNetworkingPOPID=^TSteamNetworkingPOPID;
     TSteamNetworkingPOPID=TSteamUInt32;

type PPSteamNetworkingMicroseconds=^PSteamNetworkingMicroseconds;
     PSteamNetworkingMicroseconds=^TSteamNetworkingMicroseconds;
     TSteamNetworkingMicroseconds=TSteamInt64;

type PPSteamErrMsg=^PSteamErrMsg;
     PSteamErrMsg=^TSteamErrMsg;
     TSteamErrMsg=array[0..1024-1] of TSteamChar;

type PPSteamNetworkingErrMsg=^PSteamNetworkingErrMsg;
     PSteamNetworkingErrMsg=^TSteamNetworkingErrMsg;
     TSteamNetworkingErrMsg=array[0..1024-1] of TSteamChar;

type { TESteamIPType }
     PPESteamIPType=^PESteamIPType;
     PESteamIPType=^TESteamIPType;
     TESteamIPType=TSteamInt32;

const k_ESteamIPTypeIPv4=0;
      k_ESteamIPTypeIPv6=1;

type { TEUniverse }
     PPEUniverse=^PEUniverse;
     PEUniverse=^TEUniverse;
     TEUniverse=TSteamInt32;

const k_EUniverseInvalid=0;
      k_EUniversePublic=1;
      k_EUniverseBeta=2;
      k_EUniverseInternal=3;
      k_EUniverseDev=4;
      k_EUniverseMax=5;

type { TEResult }
     PPEResult=^PEResult;
     PEResult=^TEResult;
     TEResult=TSteamInt32;

const k_EResultNone=0;
      k_EResultOK=1;
      k_EResultFail=2;
      k_EResultNoConnection=3;
      k_EResultInvalidPassword=5;
      k_EResultLoggedInElsewhere=6;
      k_EResultInvalidProtocolVer=7;
      k_EResultInvalidParam=8;
      k_EResultFileNotFound=9;
      k_EResultBusy=10;
      k_EResultInvalidState=11;
      k_EResultInvalidName=12;
      k_EResultInvalidEmail=13;
      k_EResultDuplicateName=14;
      k_EResultAccessDenied=15;
      k_EResultTimeout=16;
      k_EResultBanned=17;
      k_EResultAccountNotFound=18;
      k_EResultInvalidSteamID=19;
      k_EResultServiceUnavailable=20;
      k_EResultNotLoggedOn=21;
      k_EResultPending=22;
      k_EResultEncryptionFailure=23;
      k_EResultInsufficientPrivilege=24;
      k_EResultLimitExceeded=25;
      k_EResultRevoked=26;
      k_EResultExpired=27;
      k_EResultAlreadyRedeemed=28;
      k_EResultDuplicateRequest=29;
      k_EResultAlreadyOwned=30;
      k_EResultIPNotFound=31;
      k_EResultPersistFailed=32;
      k_EResultLockingFailed=33;
      k_EResultLogonSessionReplaced=34;
      k_EResultConnectFailed=35;
      k_EResultHandshakeFailed=36;
      k_EResultIOFailure=37;
      k_EResultRemoteDisconnect=38;
      k_EResultShoppingCartNotFound=39;
      k_EResultBlocked=40;
      k_EResultIgnored=41;
      k_EResultNoMatch=42;
      k_EResultAccountDisabled=43;
      k_EResultServiceReadOnly=44;
      k_EResultAccountNotFeatured=45;
      k_EResultAdministratorOK=46;
      k_EResultContentVersion=47;
      k_EResultTryAnotherCM=48;
      k_EResultPasswordRequiredToKickSession=49;
      k_EResultAlreadyLoggedInElsewhere=50;
      k_EResultSuspended=51;
      k_EResultCancelled=52;
      k_EResultDataCorruption=53;
      k_EResultDiskFull=54;
      k_EResultRemoteCallFailed=55;
      k_EResultPasswordUnset=56;
      k_EResultExternalAccountUnlinked=57;
      k_EResultPSNTicketInvalid=58;
      k_EResultExternalAccountAlreadyLinked=59;
      k_EResultRemoteFileConflict=60;
      k_EResultIllegalPassword=61;
      k_EResultSameAsPreviousValue=62;
      k_EResultAccountLogonDenied=63;
      k_EResultCannotUseOldPassword=64;
      k_EResultInvalidLoginAuthCode=65;
      k_EResultAccountLogonDeniedNoMail=66;
      k_EResultHardwareNotCapableOfIPT=67;
      k_EResultIPTInitError=68;
      k_EResultParentalControlRestricted=69;
      k_EResultFacebookQueryError=70;
      k_EResultExpiredLoginAuthCode=71;
      k_EResultIPLoginRestrictionFailed=72;
      k_EResultAccountLockedDown=73;
      k_EResultAccountLogonDeniedVerifiedEmailRequired=74;
      k_EResultNoMatchingURL=75;
      k_EResultBadResponse=76;
      k_EResultRequirePasswordReEntry=77;
      k_EResultValueOutOfRange=78;
      k_EResultUnexpectedError=79;
      k_EResultDisabled=80;
      k_EResultInvalidCEGSubmission=81;
      k_EResultRestrictedDevice=82;
      k_EResultRegionLocked=83;
      k_EResultRateLimitExceeded=84;
      k_EResultAccountLoginDeniedNeedTwoFactor=85;
      k_EResultItemDeleted=86;
      k_EResultAccountLoginDeniedThrottle=87;
      k_EResultTwoFactorCodeMismatch=88;
      k_EResultTwoFactorActivationCodeMismatch=89;
      k_EResultAccountAssociatedToMultiplePartners=90;
      k_EResultNotModified=91;
      k_EResultNoMobileDevice=92;
      k_EResultTimeNotSynced=93;
      k_EResultSmsCodeFailed=94;
      k_EResultAccountLimitExceeded=95;
      k_EResultAccountActivityLimitExceeded=96;
      k_EResultPhoneActivityLimitExceeded=97;
      k_EResultRefundToWallet=98;
      k_EResultEmailSendFailure=99;
      k_EResultNotSettled=100;
      k_EResultNeedCaptcha=101;
      k_EResultGSLTDenied=102;
      k_EResultGSOwnerDenied=103;
      k_EResultInvalidItemType=104;
      k_EResultIPBanned=105;
      k_EResultGSLTExpired=106;
      k_EResultInsufficientFunds=107;
      k_EResultTooManyPending=108;
      k_EResultNoSiteLicensesFound=109;
      k_EResultWGNetworkSendExceeded=110;
      k_EResultAccountNotFriends=111;
      k_EResultLimitedUserAccount=112;
      k_EResultCantRemoveItem=113;
      k_EResultAccountDeleted=114;
      k_EResultExistingUserCancelledLicense=115;
      k_EResultCommunityCooldown=116;
      k_EResultNoLauncherSpecified=117;
      k_EResultMustAgreeToSSA=118;
      k_EResultLauncherMigrated=119;
      k_EResultSteamRealmMismatch=120;
      k_EResultInvalidSignature=121;
      k_EResultParseFailure=122;
      k_EResultNoVerifiedPhone=123;
      k_EResultInsufficientBattery=124;
      k_EResultChargerRequired=125;
      k_EResultCachedCredentialInvalid=126;
      K_EResultPhoneNumberIsVOIP=127;
      k_EResultNotSupported=128;
      k_EResultFamilySizeLimitExceeded=129;
      k_EResultOfflineAppCacheInvalid=130;
      k_EResultTryLater=131;

type { TEVoiceResult }
     PPEVoiceResult=^PEVoiceResult;
     PEVoiceResult=^TEVoiceResult;
     TEVoiceResult=TSteamInt32;

const k_EVoiceResultOK=0;
      k_EVoiceResultNotInitialized=1;
      k_EVoiceResultNotRecording=2;
      k_EVoiceResultNoData=3;
      k_EVoiceResultBufferTooSmall=4;
      k_EVoiceResultDataCorrupted=5;
      k_EVoiceResultRestricted=6;
      k_EVoiceResultUnsupportedCodec=7;
      k_EVoiceResultReceiverOutOfDate=8;
      k_EVoiceResultReceiverDidNotAnswer=9;

type { TEDenyReason }
     PPEDenyReason=^PEDenyReason;
     PEDenyReason=^TEDenyReason;
     TEDenyReason=TSteamInt32;

const k_EDenyInvalid=0;
      k_EDenyInvalidVersion=1;
      k_EDenyGeneric=2;
      k_EDenyNotLoggedOn=3;
      k_EDenyNoLicense=4;
      k_EDenyCheater=5;
      k_EDenyLoggedInElseWhere=6;
      k_EDenyUnknownText=7;
      k_EDenyIncompatibleAnticheat=8;
      k_EDenyMemoryCorruption=9;
      k_EDenyIncompatibleSoftware=10;
      k_EDenySteamConnectionLost=11;
      k_EDenySteamConnectionError=12;
      k_EDenySteamResponseTimedOut=13;
      k_EDenySteamValidationStalled=14;
      k_EDenySteamOwnerLeftGuestUser=15;

type { TEBeginAuthSessionResult }
     PPEBeginAuthSessionResult=^PEBeginAuthSessionResult;
     PEBeginAuthSessionResult=^TEBeginAuthSessionResult;
     TEBeginAuthSessionResult=TSteamInt32;

const k_EBeginAuthSessionResultOK=0;
      k_EBeginAuthSessionResultInvalidTicket=1;
      k_EBeginAuthSessionResultDuplicateRequest=2;
      k_EBeginAuthSessionResultInvalidVersion=3;
      k_EBeginAuthSessionResultGameMismatch=4;
      k_EBeginAuthSessionResultExpiredTicket=5;

type { TEAuthSessionResponse }
     PPEAuthSessionResponse=^PEAuthSessionResponse;
     PEAuthSessionResponse=^TEAuthSessionResponse;
     TEAuthSessionResponse=TSteamInt32;

const k_EAuthSessionResponseOK=0;
      k_EAuthSessionResponseUserNotConnectedToSteam=1;
      k_EAuthSessionResponseNoLicenseOrExpired=2;
      k_EAuthSessionResponseVACBanned=3;
      k_EAuthSessionResponseLoggedInElseWhere=4;
      k_EAuthSessionResponseVACCheckTimedOut=5;
      k_EAuthSessionResponseAuthTicketCanceled=6;
      k_EAuthSessionResponseAuthTicketInvalidAlreadyUsed=7;
      k_EAuthSessionResponseAuthTicketInvalid=8;
      k_EAuthSessionResponsePublisherIssuedBan=9;
      k_EAuthSessionResponseAuthTicketNetworkIdentityFailure=10;

type { TEUserHasLicenseForAppResult }
     PPEUserHasLicenseForAppResult=^PEUserHasLicenseForAppResult;
     PEUserHasLicenseForAppResult=^TEUserHasLicenseForAppResult;
     TEUserHasLicenseForAppResult=TSteamInt32;

const k_EUserHasLicenseResultHasLicense=0;
      k_EUserHasLicenseResultDoesNotHaveLicense=1;
      k_EUserHasLicenseResultNoAuth=2;

type { TEAccountType }
     PPEAccountType=^PEAccountType;
     PEAccountType=^TEAccountType;
     TEAccountType=TSteamInt32;

const k_EAccountTypeInvalid=0;
      k_EAccountTypeIndividual=1;
      k_EAccountTypeMultiseat=2;
      k_EAccountTypeGameServer=3;
      k_EAccountTypeAnonGameServer=4;
      k_EAccountTypePending=5;
      k_EAccountTypeContentServer=6;
      k_EAccountTypeClan=7;
      k_EAccountTypeChat=8;
      k_EAccountTypeConsoleUser=9;
      k_EAccountTypeAnonUser=10;
      k_EAccountTypeMax=11;

type { TEChatEntryType }
     PPEChatEntryType=^PEChatEntryType;
     PEChatEntryType=^TEChatEntryType;
     TEChatEntryType=TSteamInt32;

const k_EChatEntryTypeInvalid=0;
      k_EChatEntryTypeChatMsg=1;
      k_EChatEntryTypeTyping=2;
      k_EChatEntryTypeInviteGame=3;
      k_EChatEntryTypeEmote=4;
      k_EChatEntryTypeLeftConversation=6;
      k_EChatEntryTypeEntered=7;
      k_EChatEntryTypeWasKicked=8;
      k_EChatEntryTypeWasBanned=9;
      k_EChatEntryTypeDisconnected=10;
      k_EChatEntryTypeHistoricalChat=11;
      k_EChatEntryTypeLinkBlocked=14;

type { TEChatRoomEnterResponse }
     PPEChatRoomEnterResponse=^PEChatRoomEnterResponse;
     PEChatRoomEnterResponse=^TEChatRoomEnterResponse;
     TEChatRoomEnterResponse=TSteamInt32;

const k_EChatRoomEnterResponseSuccess=1;
      k_EChatRoomEnterResponseDoesntExist=2;
      k_EChatRoomEnterResponseNotAllowed=3;
      k_EChatRoomEnterResponseFull=4;
      k_EChatRoomEnterResponseError=5;
      k_EChatRoomEnterResponseBanned=6;
      k_EChatRoomEnterResponseLimited=7;
      k_EChatRoomEnterResponseClanDisabled=8;
      k_EChatRoomEnterResponseCommunityBan=9;
      k_EChatRoomEnterResponseMemberBlockedYou=10;
      k_EChatRoomEnterResponseYouBlockedMember=11;
      k_EChatRoomEnterResponseRatelimitExceeded=15;

type { TEChatSteamIDInstanceFlags }
     PPEChatSteamIDInstanceFlags=^PEChatSteamIDInstanceFlags;
     PEChatSteamIDInstanceFlags=^TEChatSteamIDInstanceFlags;
     TEChatSteamIDInstanceFlags=TSteamInt32;

const k_EChatAccountInstanceMask=4095;
      k_EChatInstanceFlagClan=524288;
      k_EChatInstanceFlagLobby=262144;
      k_EChatInstanceFlagMMSLobby=131072;

type { TENotificationPosition }
     PPENotificationPosition=^PENotificationPosition;
     PENotificationPosition=^TENotificationPosition;
     TENotificationPosition=TSteamInt32;

const k_EPositionInvalid=-1;
      k_EPositionTopLeft=0;
      k_EPositionTopRight=1;
      k_EPositionBottomLeft=2;
      k_EPositionBottomRight=3;

type { TEBroadcastUploadResult }
     PPEBroadcastUploadResult=^PEBroadcastUploadResult;
     PEBroadcastUploadResult=^TEBroadcastUploadResult;
     TEBroadcastUploadResult=TSteamInt32;

const k_EBroadcastUploadResultNone=0;
      k_EBroadcastUploadResultOK=1;
      k_EBroadcastUploadResultInitFailed=2;
      k_EBroadcastUploadResultFrameFailed=3;
      k_EBroadcastUploadResultTimeout=4;
      k_EBroadcastUploadResultBandwidthExceeded=5;
      k_EBroadcastUploadResultLowFPS=6;
      k_EBroadcastUploadResultMissingKeyFrames=7;
      k_EBroadcastUploadResultNoConnection=8;
      k_EBroadcastUploadResultRelayFailed=9;
      k_EBroadcastUploadResultSettingsChanged=10;
      k_EBroadcastUploadResultMissingAudio=11;
      k_EBroadcastUploadResultTooFarBehind=12;
      k_EBroadcastUploadResultTranscodeBehind=13;
      k_EBroadcastUploadResultNotAllowedToPlay=14;
      k_EBroadcastUploadResultBusy=15;
      k_EBroadcastUploadResultBanned=16;
      k_EBroadcastUploadResultAlreadyActive=17;
      k_EBroadcastUploadResultForcedOff=18;
      k_EBroadcastUploadResultAudioBehind=19;
      k_EBroadcastUploadResultShutdown=20;
      k_EBroadcastUploadResultDisconnect=21;
      k_EBroadcastUploadResultVideoInitFailed=22;
      k_EBroadcastUploadResultAudioInitFailed=23;

type { TEMarketNotAllowedReasonFlags }
     PPEMarketNotAllowedReasonFlags=^PEMarketNotAllowedReasonFlags;
     PEMarketNotAllowedReasonFlags=^TEMarketNotAllowedReasonFlags;
     TEMarketNotAllowedReasonFlags=TSteamInt32;

const k_EMarketNotAllowedReason_None=0;
      k_EMarketNotAllowedReason_TemporaryFailure=1;
      k_EMarketNotAllowedReason_AccountDisabled=2;
      k_EMarketNotAllowedReason_AccountLockedDown=4;
      k_EMarketNotAllowedReason_AccountLimited=8;
      k_EMarketNotAllowedReason_TradeBanned=16;
      k_EMarketNotAllowedReason_AccountNotTrusted=32;
      k_EMarketNotAllowedReason_SteamGuardNotEnabled=64;
      k_EMarketNotAllowedReason_SteamGuardOnlyRecentlyEnabled=128;
      k_EMarketNotAllowedReason_RecentPasswordReset=256;
      k_EMarketNotAllowedReason_NewPaymentMethod=512;
      k_EMarketNotAllowedReason_InvalidCookie=1024;
      k_EMarketNotAllowedReason_UsingNewDevice=2048;
      k_EMarketNotAllowedReason_RecentSelfRefund=4096;
      k_EMarketNotAllowedReason_NewPaymentMethodCannotBeVerified=8192;
      k_EMarketNotAllowedReason_NoRecentPurchases=16384;
      k_EMarketNotAllowedReason_AcceptedWalletGift=32768;
      k_EMarketNotAllowedReason_TradeCooldown=65536;

type { TEDurationControlProgress }
     PPEDurationControlProgress=^PEDurationControlProgress;
     PEDurationControlProgress=^TEDurationControlProgress;
     TEDurationControlProgress=TSteamInt32;

const k_EDurationControlProgress_Full=0;
      k_EDurationControlProgress_Half=1;
      k_EDurationControlProgress_None=2;
      k_EDurationControl_ExitSoon_3h=3;
      k_EDurationControl_ExitSoon_5h=4;
      k_EDurationControl_ExitSoon_Night=5;

type { TEDurationControlNotification }
     PPEDurationControlNotification=^PEDurationControlNotification;
     PEDurationControlNotification=^TEDurationControlNotification;
     TEDurationControlNotification=TSteamInt32;

const k_EDurationControlNotification_None=0;
      k_EDurationControlNotification_1Hour=1;
      k_EDurationControlNotification_3Hours=2;
      k_EDurationControlNotification_HalfProgress=3;
      k_EDurationControlNotification_NoProgress=4;
      k_EDurationControlNotification_ExitSoon_3h=5;
      k_EDurationControlNotification_ExitSoon_5h=6;
      k_EDurationControlNotification_ExitSoon_Night=7;

type { TEDurationControlOnlineState }
     PPEDurationControlOnlineState=^PEDurationControlOnlineState;
     PEDurationControlOnlineState=^TEDurationControlOnlineState;
     TEDurationControlOnlineState=TSteamInt32;

const k_EDurationControlOnlineState_Invalid=0;
      k_EDurationControlOnlineState_Offline=1;
      k_EDurationControlOnlineState_Online=2;
      k_EDurationControlOnlineState_OnlineHighPri=3;

type { TEBetaBranchFlags }
     PPEBetaBranchFlags=^PEBetaBranchFlags;
     PEBetaBranchFlags=^TEBetaBranchFlags;
     TEBetaBranchFlags=TSteamInt32;

const k_EBetaBranch_None=0;
      k_EBetaBranch_Default=1;
      k_EBetaBranch_Available=2;
      k_EBetaBranch_Private=4;
      k_EBetaBranch_Selected=8;
      k_EBetaBranch_Installed=16;

type { TESteamIPv6ConnectivityProtocol }
     PPESteamIPv6ConnectivityProtocol=^PESteamIPv6ConnectivityProtocol;
     PESteamIPv6ConnectivityProtocol=^TESteamIPv6ConnectivityProtocol;
     TESteamIPv6ConnectivityProtocol=TSteamInt32;

const k_ESteamIPv6ConnectivityProtocol_Invalid=0;
      k_ESteamIPv6ConnectivityProtocol_HTTP=1;
      k_ESteamIPv6ConnectivityProtocol_UDP=2;

type { TESteamIPv6ConnectivityState }
     PPESteamIPv6ConnectivityState=^PESteamIPv6ConnectivityState;
     PESteamIPv6ConnectivityState=^TESteamIPv6ConnectivityState;
     TESteamIPv6ConnectivityState=TSteamInt32;

const k_ESteamIPv6ConnectivityState_Unknown=0;
      k_ESteamIPv6ConnectivityState_Good=1;
      k_ESteamIPv6ConnectivityState_Bad=2;

type { TEFriendRelationship }
     PPEFriendRelationship=^PEFriendRelationship;
     PEFriendRelationship=^TEFriendRelationship;
     TEFriendRelationship=TSteamInt32;

const k_EFriendRelationshipNone=0;
      k_EFriendRelationshipBlocked=1;
      k_EFriendRelationshipRequestRecipient=2;
      k_EFriendRelationshipFriend=3;
      k_EFriendRelationshipRequestInitiator=4;
      k_EFriendRelationshipIgnored=5;
      k_EFriendRelationshipIgnoredFriend=6;
      k_EFriendRelationshipSuggested_DEPRECATED=7;
      k_EFriendRelationshipMax=8;

type { TEPersonaState }
     PPEPersonaState=^PEPersonaState;
     PEPersonaState=^TEPersonaState;
     TEPersonaState=TSteamInt32;

const k_EPersonaStateOffline=0;
      k_EPersonaStateOnline=1;
      k_EPersonaStateBusy=2;
      k_EPersonaStateAway=3;
      k_EPersonaStateSnooze=4;
      k_EPersonaStateLookingToTrade=5;
      k_EPersonaStateLookingToPlay=6;
      k_EPersonaStateInvisible=7;
      k_EPersonaStateMax=8;

type { TEFriendFlags }
     PPEFriendFlags=^PEFriendFlags;
     PEFriendFlags=^TEFriendFlags;
     TEFriendFlags=TSteamInt32;

const k_EFriendFlagNone=0;
      k_EFriendFlagBlocked=1;
      k_EFriendFlagFriendshipRequested=2;
      k_EFriendFlagImmediate=4;
      k_EFriendFlagClanMember=8;
      k_EFriendFlagOnGameServer=16;
      k_EFriendFlagRequestingFriendship=128;
      k_EFriendFlagRequestingInfo=256;
      k_EFriendFlagIgnored=512;
      k_EFriendFlagIgnoredFriend=1024;
      k_EFriendFlagChatMember=4096;
      k_EFriendFlagAll=65535;

type { TEOverlayToStoreFlag }
     PPEOverlayToStoreFlag=^PEOverlayToStoreFlag;
     PEOverlayToStoreFlag=^TEOverlayToStoreFlag;
     TEOverlayToStoreFlag=TSteamInt32;

const k_EOverlayToStoreFlag_None=0;
      k_EOverlayToStoreFlag_AddToCart=1;
      k_EOverlayToStoreFlag_AddToCartAndShow=2;

type { TEActivateGameOverlayToWebPageMode }
     PPEActivateGameOverlayToWebPageMode=^PEActivateGameOverlayToWebPageMode;
     PEActivateGameOverlayToWebPageMode=^TEActivateGameOverlayToWebPageMode;
     TEActivateGameOverlayToWebPageMode=TSteamInt32;

const k_EActivateGameOverlayToWebPageMode_Default=0;
      k_EActivateGameOverlayToWebPageMode_Modal=1;

type { TECommunityProfileItemType }
     PPECommunityProfileItemType=^PECommunityProfileItemType;
     PECommunityProfileItemType=^TECommunityProfileItemType;
     TECommunityProfileItemType=TSteamInt32;

const k_ECommunityProfileItemType_AnimatedAvatar=0;
      k_ECommunityProfileItemType_AvatarFrame=1;
      k_ECommunityProfileItemType_ProfileModifier=2;
      k_ECommunityProfileItemType_ProfileBackground=3;
      k_ECommunityProfileItemType_MiniProfileBackground=4;

type { TECommunityProfileItemProperty }
     PPECommunityProfileItemProperty=^PECommunityProfileItemProperty;
     PECommunityProfileItemProperty=^TECommunityProfileItemProperty;
     TECommunityProfileItemProperty=TSteamInt32;

const k_ECommunityProfileItemProperty_ImageSmall=0;
      k_ECommunityProfileItemProperty_ImageLarge=1;
      k_ECommunityProfileItemProperty_InternalName=2;
      k_ECommunityProfileItemProperty_Title=3;
      k_ECommunityProfileItemProperty_Description=4;
      k_ECommunityProfileItemProperty_AppID=5;
      k_ECommunityProfileItemProperty_TypeID=6;
      k_ECommunityProfileItemProperty_Class=7;
      k_ECommunityProfileItemProperty_MovieWebM=8;
      k_ECommunityProfileItemProperty_MovieMP4=9;
      k_ECommunityProfileItemProperty_MovieWebMSmall=10;
      k_ECommunityProfileItemProperty_MovieMP4Small=11;

type { TEPersonaChange }
     PPEPersonaChange=^PEPersonaChange;
     PEPersonaChange=^TEPersonaChange;
     TEPersonaChange=TSteamInt32;

const k_EPersonaChangeName=1;
      k_EPersonaChangeStatus=2;
      k_EPersonaChangeComeOnline=4;
      k_EPersonaChangeGoneOffline=8;
      k_EPersonaChangeGamePlayed=16;
      k_EPersonaChangeGameServer=32;
      k_EPersonaChangeAvatar=64;
      k_EPersonaChangeJoinedSource=128;
      k_EPersonaChangeLeftSource=256;
      k_EPersonaChangeRelationshipChanged=512;
      k_EPersonaChangeNameFirstSet=1024;
      k_EPersonaChangeBroadcast=2048;
      k_EPersonaChangeNickname=4096;
      k_EPersonaChangeSteamLevel=8192;
      k_EPersonaChangeRichPresence=16384;

type { TESteamAPICallFailure }
     PPESteamAPICallFailure=^PESteamAPICallFailure;
     PESteamAPICallFailure=^TESteamAPICallFailure;
     TESteamAPICallFailure=TSteamInt32;

const k_ESteamAPICallFailureNone=-1;
      k_ESteamAPICallFailureSteamGone=0;
      k_ESteamAPICallFailureNetworkFailure=1;
      k_ESteamAPICallFailureInvalidHandle=2;
      k_ESteamAPICallFailureMismatchedCallback=3;

type { TEGamepadTextInputMode }
     PPEGamepadTextInputMode=^PEGamepadTextInputMode;
     PEGamepadTextInputMode=^TEGamepadTextInputMode;
     TEGamepadTextInputMode=TSteamInt32;

const k_EGamepadTextInputModeNormal=0;
      k_EGamepadTextInputModePassword=1;

type { TEGamepadTextInputLineMode }
     PPEGamepadTextInputLineMode=^PEGamepadTextInputLineMode;
     PEGamepadTextInputLineMode=^TEGamepadTextInputLineMode;
     TEGamepadTextInputLineMode=TSteamInt32;

const k_EGamepadTextInputLineModeSingleLine=0;
      k_EGamepadTextInputLineModeMultipleLines=1;

type { TEFloatingGamepadTextInputMode }
     PPEFloatingGamepadTextInputMode=^PEFloatingGamepadTextInputMode;
     PEFloatingGamepadTextInputMode=^TEFloatingGamepadTextInputMode;
     TEFloatingGamepadTextInputMode=TSteamInt32;

const k_EFloatingGamepadTextInputModeModeSingleLine=0;
      k_EFloatingGamepadTextInputModeModeMultipleLines=1;
      k_EFloatingGamepadTextInputModeModeEmail=2;
      k_EFloatingGamepadTextInputModeModeNumeric=3;

type { TETextFilteringContext }
     PPETextFilteringContext=^PETextFilteringContext;
     PETextFilteringContext=^TETextFilteringContext;
     TETextFilteringContext=TSteamInt32;

const k_ETextFilteringContextUnknown=0;
      k_ETextFilteringContextGameContent=1;
      k_ETextFilteringContextChat=2;
      k_ETextFilteringContextName=3;

type { TECheckFileSignature }
     PPECheckFileSignature=^PECheckFileSignature;
     PECheckFileSignature=^TECheckFileSignature;
     TECheckFileSignature=TSteamInt32;

const k_ECheckFileSignatureInvalidSignature=0;
      k_ECheckFileSignatureValidSignature=1;
      k_ECheckFileSignatureFileNotFound=2;
      k_ECheckFileSignatureNoSignaturesFoundForThisApp=3;
      k_ECheckFileSignatureNoSignaturesFoundForThisFile=4;

type { TEMatchMakingServerResponse }
     PPEMatchMakingServerResponse=^PEMatchMakingServerResponse;
     PEMatchMakingServerResponse=^TEMatchMakingServerResponse;
     TEMatchMakingServerResponse=TSteamInt32;

const eServerResponded=0;
      eServerFailedToRespond=1;
      eNoServersListedOnMasterServer=2;

type { TELobbyType }
     PPELobbyType=^PELobbyType;
     PELobbyType=^TELobbyType;
     TELobbyType=TSteamInt32;

const k_ELobbyTypePrivate=0;
      k_ELobbyTypeFriendsOnly=1;
      k_ELobbyTypePublic=2;
      k_ELobbyTypeInvisible=3;
      k_ELobbyTypePrivateUnique=4;

type { TELobbyComparison }
     PPELobbyComparison=^PELobbyComparison;
     PELobbyComparison=^TELobbyComparison;
     TELobbyComparison=TSteamInt32;

const k_ELobbyComparisonEqualToOrLessThan=-2;
      k_ELobbyComparisonLessThan=-1;
      k_ELobbyComparisonEqual=0;
      k_ELobbyComparisonGreaterThan=1;
      k_ELobbyComparisonEqualToOrGreaterThan=2;
      k_ELobbyComparisonNotEqual=3;

type { TELobbyDistanceFilter }
     PPELobbyDistanceFilter=^PELobbyDistanceFilter;
     PELobbyDistanceFilter=^TELobbyDistanceFilter;
     TELobbyDistanceFilter=TSteamInt32;

const k_ELobbyDistanceFilterClose=0;
      k_ELobbyDistanceFilterDefault=1;
      k_ELobbyDistanceFilterFar=2;
      k_ELobbyDistanceFilterWorldwide=3;

type { TEChatMemberStateChange }
     PPEChatMemberStateChange=^PEChatMemberStateChange;
     PEChatMemberStateChange=^TEChatMemberStateChange;
     TEChatMemberStateChange=TSteamInt32;

const k_EChatMemberStateChangeEntered=1;
      k_EChatMemberStateChangeLeft=2;
      k_EChatMemberStateChangeDisconnected=4;
      k_EChatMemberStateChangeKicked=8;
      k_EChatMemberStateChangeBanned=16;

type { TESteamPartyBeaconLocationType }
     PPESteamPartyBeaconLocationType=^PESteamPartyBeaconLocationType;
     PESteamPartyBeaconLocationType=^TESteamPartyBeaconLocationType;
     TESteamPartyBeaconLocationType=TSteamInt32;

const k_ESteamPartyBeaconLocationType_Invalid=0;
      k_ESteamPartyBeaconLocationType_ChatGroup=1;
      k_ESteamPartyBeaconLocationType_Max=2;

type { TESteamPartyBeaconLocationData }
     PPESteamPartyBeaconLocationData=^PESteamPartyBeaconLocationData;
     PESteamPartyBeaconLocationData=^TESteamPartyBeaconLocationData;
     TESteamPartyBeaconLocationData=TSteamInt32;

const k_ESteamPartyBeaconLocationDataInvalid=0;
      k_ESteamPartyBeaconLocationDataName=1;
      k_ESteamPartyBeaconLocationDataIconURLSmall=2;
      k_ESteamPartyBeaconLocationDataIconURLMedium=3;
      k_ESteamPartyBeaconLocationDataIconURLLarge=4;

type { TERemoteStoragePlatform }
     PPERemoteStoragePlatform=^PERemoteStoragePlatform;
     PERemoteStoragePlatform=^TERemoteStoragePlatform;
     TERemoteStoragePlatform=TSteamInt32;

const k_ERemoteStoragePlatformNone=0;
      k_ERemoteStoragePlatformWindows=1;
      k_ERemoteStoragePlatformOSX=2;
      k_ERemoteStoragePlatformPS3=4;
      k_ERemoteStoragePlatformLinux=8;
      k_ERemoteStoragePlatformSwitch=16;
      k_ERemoteStoragePlatformAndroid=32;
      k_ERemoteStoragePlatformIOS=64;
      k_ERemoteStoragePlatformAll=-1;

type { TERemoteStoragePublishedFileVisibility }
     PPERemoteStoragePublishedFileVisibility=^PERemoteStoragePublishedFileVisibility;
     PERemoteStoragePublishedFileVisibility=^TERemoteStoragePublishedFileVisibility;
     TERemoteStoragePublishedFileVisibility=TSteamInt32;

const k_ERemoteStoragePublishedFileVisibilityPublic=0;
      k_ERemoteStoragePublishedFileVisibilityFriendsOnly=1;
      k_ERemoteStoragePublishedFileVisibilityPrivate=2;
      k_ERemoteStoragePublishedFileVisibilityUnlisted=3;

type { TEWorkshopFileType }
     PPEWorkshopFileType=^PEWorkshopFileType;
     PEWorkshopFileType=^TEWorkshopFileType;
     TEWorkshopFileType=TSteamInt32;

const k_EWorkshopFileTypeFirst=0;
      k_EWorkshopFileTypeCommunity=0;
      k_EWorkshopFileTypeMicrotransaction=1;
      k_EWorkshopFileTypeCollection=2;
      k_EWorkshopFileTypeArt=3;
      k_EWorkshopFileTypeVideo=4;
      k_EWorkshopFileTypeScreenshot=5;
      k_EWorkshopFileTypeGame=6;
      k_EWorkshopFileTypeSoftware=7;
      k_EWorkshopFileTypeConcept=8;
      k_EWorkshopFileTypeWebGuide=9;
      k_EWorkshopFileTypeIntegratedGuide=10;
      k_EWorkshopFileTypeMerch=11;
      k_EWorkshopFileTypeControllerBinding=12;
      k_EWorkshopFileTypeSteamworksAccessInvite=13;
      k_EWorkshopFileTypeSteamVideo=14;
      k_EWorkshopFileTypeGameManagedItem=15;
      k_EWorkshopFileTypeClip=16;
      k_EWorkshopFileTypeMax=17;

type { TEWorkshopVote }
     PPEWorkshopVote=^PEWorkshopVote;
     PEWorkshopVote=^TEWorkshopVote;
     TEWorkshopVote=TSteamInt32;

const k_EWorkshopVoteUnvoted=0;
      k_EWorkshopVoteFor=1;
      k_EWorkshopVoteAgainst=2;
      k_EWorkshopVoteLater=3;

type { TEWorkshopFileAction }
     PPEWorkshopFileAction=^PEWorkshopFileAction;
     PEWorkshopFileAction=^TEWorkshopFileAction;
     TEWorkshopFileAction=TSteamInt32;

const k_EWorkshopFileActionPlayed=0;
      k_EWorkshopFileActionCompleted=1;

type { TEWorkshopEnumerationType }
     PPEWorkshopEnumerationType=^PEWorkshopEnumerationType;
     PEWorkshopEnumerationType=^TEWorkshopEnumerationType;
     TEWorkshopEnumerationType=TSteamInt32;

const k_EWorkshopEnumerationTypeRankedByVote=0;
      k_EWorkshopEnumerationTypeRecent=1;
      k_EWorkshopEnumerationTypeTrending=2;
      k_EWorkshopEnumerationTypeFavoritesOfFriends=3;
      k_EWorkshopEnumerationTypeVotedByFriends=4;
      k_EWorkshopEnumerationTypeContentByFriends=5;
      k_EWorkshopEnumerationTypeRecentFromFollowedUsers=6;

type { TEWorkshopVideoProvider }
     PPEWorkshopVideoProvider=^PEWorkshopVideoProvider;
     PEWorkshopVideoProvider=^TEWorkshopVideoProvider;
     TEWorkshopVideoProvider=TSteamInt32;

const k_EWorkshopVideoProviderNone=0;
      k_EWorkshopVideoProviderYoutube=1;

type { TEUGCReadAction }
     PPEUGCReadAction=^PEUGCReadAction;
     PEUGCReadAction=^TEUGCReadAction;
     TEUGCReadAction=TSteamInt32;

const k_EUGCRead_ContinueReadingUntilFinished=0;
      k_EUGCRead_ContinueReading=1;
      k_EUGCRead_Close=2;

type { TERemoteStorageLocalFileChange }
     PPERemoteStorageLocalFileChange=^PERemoteStorageLocalFileChange;
     PERemoteStorageLocalFileChange=^TERemoteStorageLocalFileChange;
     TERemoteStorageLocalFileChange=TSteamInt32;

const k_ERemoteStorageLocalFileChange_Invalid=0;
      k_ERemoteStorageLocalFileChange_FileUpdated=1;
      k_ERemoteStorageLocalFileChange_FileDeleted=2;

type { TERemoteStorageFilePathType }
     PPERemoteStorageFilePathType=^PERemoteStorageFilePathType;
     PERemoteStorageFilePathType=^TERemoteStorageFilePathType;
     TERemoteStorageFilePathType=TSteamInt32;

const k_ERemoteStorageFilePathType_Invalid=0;
      k_ERemoteStorageFilePathType_Absolute=1;
      k_ERemoteStorageFilePathType_APIFilename=2;

type { TELeaderboardDataRequest }
     PPELeaderboardDataRequest=^PELeaderboardDataRequest;
     PELeaderboardDataRequest=^TELeaderboardDataRequest;
     TELeaderboardDataRequest=TSteamInt32;

const k_ELeaderboardDataRequestGlobal=0;
      k_ELeaderboardDataRequestGlobalAroundUser=1;
      k_ELeaderboardDataRequestFriends=2;
      k_ELeaderboardDataRequestUsers=3;

type { TELeaderboardSortMethod }
     PPELeaderboardSortMethod=^PELeaderboardSortMethod;
     PELeaderboardSortMethod=^TELeaderboardSortMethod;
     TELeaderboardSortMethod=TSteamInt32;

const k_ELeaderboardSortMethodNone=0;
      k_ELeaderboardSortMethodAscending=1;
      k_ELeaderboardSortMethodDescending=2;

type { TELeaderboardDisplayType }
     PPELeaderboardDisplayType=^PELeaderboardDisplayType;
     PELeaderboardDisplayType=^TELeaderboardDisplayType;
     TELeaderboardDisplayType=TSteamInt32;

const k_ELeaderboardDisplayTypeNone=0;
      k_ELeaderboardDisplayTypeNumeric=1;
      k_ELeaderboardDisplayTypeTimeSeconds=2;
      k_ELeaderboardDisplayTypeTimeMilliSeconds=3;

type { TELeaderboardUploadScoreMethod }
     PPELeaderboardUploadScoreMethod=^PELeaderboardUploadScoreMethod;
     PELeaderboardUploadScoreMethod=^TELeaderboardUploadScoreMethod;
     TELeaderboardUploadScoreMethod=TSteamInt32;

const k_ELeaderboardUploadScoreMethodNone=0;
      k_ELeaderboardUploadScoreMethodKeepBest=1;
      k_ELeaderboardUploadScoreMethodForceUpdate=2;

type { TEP2PSessionError }
     PPEP2PSessionError=^PEP2PSessionError;
     PEP2PSessionError=^TEP2PSessionError;
     TEP2PSessionError=TSteamInt32;

const k_EP2PSessionErrorNone=0;
      k_EP2PSessionErrorNoRightsToApp=2;
      k_EP2PSessionErrorTimeout=4;
      k_EP2PSessionErrorNotRunningApp_DELETED=1;
      k_EP2PSessionErrorDestinationNotLoggedIn_DELETED=3;
      k_EP2PSessionErrorMax=5;

type { TEP2PSend }
     PPEP2PSend=^PEP2PSend;
     PEP2PSend=^TEP2PSend;
     TEP2PSend=TSteamInt32;

const k_EP2PSendUnreliable=0;
      k_EP2PSendUnreliableNoDelay=1;
      k_EP2PSendReliable=2;
      k_EP2PSendReliableWithBuffering=3;

type { TESNetSocketState }
     PPESNetSocketState=^PESNetSocketState;
     PESNetSocketState=^TESNetSocketState;
     TESNetSocketState=TSteamInt32;

const k_ESNetSocketStateInvalid=0;
      k_ESNetSocketStateConnected=1;
      k_ESNetSocketStateInitiated=10;
      k_ESNetSocketStateLocalCandidatesFound=11;
      k_ESNetSocketStateReceivedRemoteCandidates=12;
      k_ESNetSocketStateChallengeHandshake=15;
      k_ESNetSocketStateDisconnecting=21;
      k_ESNetSocketStateLocalDisconnect=22;
      k_ESNetSocketStateTimeoutDuringConnect=23;
      k_ESNetSocketStateRemoteEndDisconnected=24;
      k_ESNetSocketStateConnectionBroken=25;

type { TESNetSocketConnectionType }
     PPESNetSocketConnectionType=^PESNetSocketConnectionType;
     PESNetSocketConnectionType=^TESNetSocketConnectionType;
     TESNetSocketConnectionType=TSteamInt32;

const k_ESNetSocketConnectionTypeNotConnected=0;
      k_ESNetSocketConnectionTypeUDP=1;
      k_ESNetSocketConnectionTypeUDPRelay=2;

type { TEVRScreenshotType }
     PPEVRScreenshotType=^PEVRScreenshotType;
     PEVRScreenshotType=^TEVRScreenshotType;
     TEVRScreenshotType=TSteamInt32;

const k_EVRScreenshotType_None=0;
      k_EVRScreenshotType_Mono=1;
      k_EVRScreenshotType_Stereo=2;
      k_EVRScreenshotType_MonoCubemap=3;
      k_EVRScreenshotType_MonoPanorama=4;
      k_EVRScreenshotType_StereoPanorama=5;

type { TAudioPlayback_Status }
     PPAudioPlayback_Status=^PAudioPlayback_Status;
     PAudioPlayback_Status=^TAudioPlayback_Status;
     TAudioPlayback_Status=TSteamInt32;

const AudioPlayback_Undefined=0;
      AudioPlayback_Playing=1;
      AudioPlayback_Paused=2;
      AudioPlayback_Idle=3;

type { TEHTTPMethod }
     PPEHTTPMethod=^PEHTTPMethod;
     PEHTTPMethod=^TEHTTPMethod;
     TEHTTPMethod=TSteamInt32;

const k_EHTTPMethodInvalid=0;
      k_EHTTPMethodGET=1;
      k_EHTTPMethodHEAD=2;
      k_EHTTPMethodPOST=3;
      k_EHTTPMethodPUT=4;
      k_EHTTPMethodDELETE=5;
      k_EHTTPMethodOPTIONS=6;
      k_EHTTPMethodPATCH=7;

type { TEHTTPStatusCode }
     PPEHTTPStatusCode=^PEHTTPStatusCode;
     PEHTTPStatusCode=^TEHTTPStatusCode;
     TEHTTPStatusCode=TSteamInt32;

const k_EHTTPStatusCodeInvalid=0;
      k_EHTTPStatusCode100Continue=100;
      k_EHTTPStatusCode101SwitchingProtocols=101;
      k_EHTTPStatusCode200OK=200;
      k_EHTTPStatusCode201Created=201;
      k_EHTTPStatusCode202Accepted=202;
      k_EHTTPStatusCode203NonAuthoritative=203;
      k_EHTTPStatusCode204NoContent=204;
      k_EHTTPStatusCode205ResetContent=205;
      k_EHTTPStatusCode206PartialContent=206;
      k_EHTTPStatusCode300MultipleChoices=300;
      k_EHTTPStatusCode301MovedPermanently=301;
      k_EHTTPStatusCode302Found=302;
      k_EHTTPStatusCode303SeeOther=303;
      k_EHTTPStatusCode304NotModified=304;
      k_EHTTPStatusCode305UseProxy=305;
      k_EHTTPStatusCode307TemporaryRedirect=307;
      k_EHTTPStatusCode308PermanentRedirect=308;
      k_EHTTPStatusCode400BadRequest=400;
      k_EHTTPStatusCode401Unauthorized=401;
      k_EHTTPStatusCode402PaymentRequired=402;
      k_EHTTPStatusCode403Forbidden=403;
      k_EHTTPStatusCode404NotFound=404;
      k_EHTTPStatusCode405MethodNotAllowed=405;
      k_EHTTPStatusCode406NotAcceptable=406;
      k_EHTTPStatusCode407ProxyAuthRequired=407;
      k_EHTTPStatusCode408RequestTimeout=408;
      k_EHTTPStatusCode409Conflict=409;
      k_EHTTPStatusCode410Gone=410;
      k_EHTTPStatusCode411LengthRequired=411;
      k_EHTTPStatusCode412PreconditionFailed=412;
      k_EHTTPStatusCode413RequestEntityTooLarge=413;
      k_EHTTPStatusCode414RequestURITooLong=414;
      k_EHTTPStatusCode415UnsupportedMediaType=415;
      k_EHTTPStatusCode416RequestedRangeNotSatisfiable=416;
      k_EHTTPStatusCode417ExpectationFailed=417;
      k_EHTTPStatusCode4xxUnknown=418;
      k_EHTTPStatusCode421MisdirectedRequest=421;
      k_EHTTPStatusCode422UnprocessableContent=422;
      k_EHTTPStatusCode423Locked=423;
      k_EHTTPStatusCode424FailedDependency=424;
      k_EHTTPStatusCode425TooEarly=425;
      k_EHTTPStatusCode426UpgradeRequired=426;
      k_EHTTPStatusCode428PreconditionRequired=428;
      k_EHTTPStatusCode429TooManyRequests=429;
      k_EHTTPStatusCode431RequestHeaderFieldsTooLarge=431;
      k_EHTTPStatusCode444ConnectionClosed=444;
      k_EHTTPStatusCode451UnavailableForLegalReasons=451;
      k_EHTTPStatusCode500InternalServerError=500;
      k_EHTTPStatusCode501NotImplemented=501;
      k_EHTTPStatusCode502BadGateway=502;
      k_EHTTPStatusCode503ServiceUnavailable=503;
      k_EHTTPStatusCode504GatewayTimeout=504;
      k_EHTTPStatusCode505HTTPVersionNotSupported=505;
      k_EHTTPStatusCode506VariantAlsoNegotiates=506;
      k_EHTTPStatusCode507InsufficientStorage=507;
      k_EHTTPStatusCode508LoopDetected=508;
      k_EHTTPStatusCode510NotExtended=510;
      k_EHTTPStatusCode511NetworkAuthenticationRequired=511;
      k_EHTTPStatusCode5xxUnknown=599;

type { TEInputSourceMode }
     PPEInputSourceMode=^PEInputSourceMode;
     PEInputSourceMode=^TEInputSourceMode;
     TEInputSourceMode=TSteamInt32;

const k_EInputSourceMode_None=0;
      k_EInputSourceMode_Dpad=1;
      k_EInputSourceMode_Buttons=2;
      k_EInputSourceMode_FourButtons=3;
      k_EInputSourceMode_AbsoluteMouse=4;
      k_EInputSourceMode_RelativeMouse=5;
      k_EInputSourceMode_JoystickMove=6;
      k_EInputSourceMode_JoystickMouse=7;
      k_EInputSourceMode_JoystickCamera=8;
      k_EInputSourceMode_ScrollWheel=9;
      k_EInputSourceMode_Trigger=10;
      k_EInputSourceMode_TouchMenu=11;
      k_EInputSourceMode_MouseJoystick=12;
      k_EInputSourceMode_MouseRegion=13;
      k_EInputSourceMode_RadialMenu=14;
      k_EInputSourceMode_SingleButton=15;
      k_EInputSourceMode_Switches=16;

type { TEInputActionOrigin }
     PPEInputActionOrigin=^PEInputActionOrigin;
     PEInputActionOrigin=^TEInputActionOrigin;
     TEInputActionOrigin=TSteamInt32;

const k_EInputActionOrigin_None=0;
      k_EInputActionOrigin_SteamController_A=1;
      k_EInputActionOrigin_SteamController_B=2;
      k_EInputActionOrigin_SteamController_X=3;
      k_EInputActionOrigin_SteamController_Y=4;
      k_EInputActionOrigin_SteamController_LeftBumper=5;
      k_EInputActionOrigin_SteamController_RightBumper=6;
      k_EInputActionOrigin_SteamController_LeftGrip=7;
      k_EInputActionOrigin_SteamController_RightGrip=8;
      k_EInputActionOrigin_SteamController_Start=9;
      k_EInputActionOrigin_SteamController_Back=10;
      k_EInputActionOrigin_SteamController_LeftPad_Touch=11;
      k_EInputActionOrigin_SteamController_LeftPad_Swipe=12;
      k_EInputActionOrigin_SteamController_LeftPad_Click=13;
      k_EInputActionOrigin_SteamController_LeftPad_DPadNorth=14;
      k_EInputActionOrigin_SteamController_LeftPad_DPadSouth=15;
      k_EInputActionOrigin_SteamController_LeftPad_DPadWest=16;
      k_EInputActionOrigin_SteamController_LeftPad_DPadEast=17;
      k_EInputActionOrigin_SteamController_RightPad_Touch=18;
      k_EInputActionOrigin_SteamController_RightPad_Swipe=19;
      k_EInputActionOrigin_SteamController_RightPad_Click=20;
      k_EInputActionOrigin_SteamController_RightPad_DPadNorth=21;
      k_EInputActionOrigin_SteamController_RightPad_DPadSouth=22;
      k_EInputActionOrigin_SteamController_RightPad_DPadWest=23;
      k_EInputActionOrigin_SteamController_RightPad_DPadEast=24;
      k_EInputActionOrigin_SteamController_LeftTrigger_Pull=25;
      k_EInputActionOrigin_SteamController_LeftTrigger_Click=26;
      k_EInputActionOrigin_SteamController_RightTrigger_Pull=27;
      k_EInputActionOrigin_SteamController_RightTrigger_Click=28;
      k_EInputActionOrigin_SteamController_LeftStick_Move=29;
      k_EInputActionOrigin_SteamController_LeftStick_Click=30;
      k_EInputActionOrigin_SteamController_LeftStick_DPadNorth=31;
      k_EInputActionOrigin_SteamController_LeftStick_DPadSouth=32;
      k_EInputActionOrigin_SteamController_LeftStick_DPadWest=33;
      k_EInputActionOrigin_SteamController_LeftStick_DPadEast=34;
      k_EInputActionOrigin_SteamController_Gyro_Move=35;
      k_EInputActionOrigin_SteamController_Gyro_Pitch=36;
      k_EInputActionOrigin_SteamController_Gyro_Yaw=37;
      k_EInputActionOrigin_SteamController_Gyro_Roll=38;
      k_EInputActionOrigin_SteamController_Reserved0=39;
      k_EInputActionOrigin_SteamController_Reserved1=40;
      k_EInputActionOrigin_SteamController_Reserved2=41;
      k_EInputActionOrigin_SteamController_Reserved3=42;
      k_EInputActionOrigin_SteamController_Reserved4=43;
      k_EInputActionOrigin_SteamController_Reserved5=44;
      k_EInputActionOrigin_SteamController_Reserved6=45;
      k_EInputActionOrigin_SteamController_Reserved7=46;
      k_EInputActionOrigin_SteamController_Reserved8=47;
      k_EInputActionOrigin_SteamController_Reserved9=48;
      k_EInputActionOrigin_SteamController_Reserved10=49;
      k_EInputActionOrigin_PS4_X=50;
      k_EInputActionOrigin_PS4_Circle=51;
      k_EInputActionOrigin_PS4_Triangle=52;
      k_EInputActionOrigin_PS4_Square=53;
      k_EInputActionOrigin_PS4_LeftBumper=54;
      k_EInputActionOrigin_PS4_RightBumper=55;
      k_EInputActionOrigin_PS4_Options=56;
      k_EInputActionOrigin_PS4_Share=57;
      k_EInputActionOrigin_PS4_LeftPad_Touch=58;
      k_EInputActionOrigin_PS4_LeftPad_Swipe=59;
      k_EInputActionOrigin_PS4_LeftPad_Click=60;
      k_EInputActionOrigin_PS4_LeftPad_DPadNorth=61;
      k_EInputActionOrigin_PS4_LeftPad_DPadSouth=62;
      k_EInputActionOrigin_PS4_LeftPad_DPadWest=63;
      k_EInputActionOrigin_PS4_LeftPad_DPadEast=64;
      k_EInputActionOrigin_PS4_RightPad_Touch=65;
      k_EInputActionOrigin_PS4_RightPad_Swipe=66;
      k_EInputActionOrigin_PS4_RightPad_Click=67;
      k_EInputActionOrigin_PS4_RightPad_DPadNorth=68;
      k_EInputActionOrigin_PS4_RightPad_DPadSouth=69;
      k_EInputActionOrigin_PS4_RightPad_DPadWest=70;
      k_EInputActionOrigin_PS4_RightPad_DPadEast=71;
      k_EInputActionOrigin_PS4_CenterPad_Touch=72;
      k_EInputActionOrigin_PS4_CenterPad_Swipe=73;
      k_EInputActionOrigin_PS4_CenterPad_Click=74;
      k_EInputActionOrigin_PS4_CenterPad_DPadNorth=75;
      k_EInputActionOrigin_PS4_CenterPad_DPadSouth=76;
      k_EInputActionOrigin_PS4_CenterPad_DPadWest=77;
      k_EInputActionOrigin_PS4_CenterPad_DPadEast=78;
      k_EInputActionOrigin_PS4_LeftTrigger_Pull=79;
      k_EInputActionOrigin_PS4_LeftTrigger_Click=80;
      k_EInputActionOrigin_PS4_RightTrigger_Pull=81;
      k_EInputActionOrigin_PS4_RightTrigger_Click=82;
      k_EInputActionOrigin_PS4_LeftStick_Move=83;
      k_EInputActionOrigin_PS4_LeftStick_Click=84;
      k_EInputActionOrigin_PS4_LeftStick_DPadNorth=85;
      k_EInputActionOrigin_PS4_LeftStick_DPadSouth=86;
      k_EInputActionOrigin_PS4_LeftStick_DPadWest=87;
      k_EInputActionOrigin_PS4_LeftStick_DPadEast=88;
      k_EInputActionOrigin_PS4_RightStick_Move=89;
      k_EInputActionOrigin_PS4_RightStick_Click=90;
      k_EInputActionOrigin_PS4_RightStick_DPadNorth=91;
      k_EInputActionOrigin_PS4_RightStick_DPadSouth=92;
      k_EInputActionOrigin_PS4_RightStick_DPadWest=93;
      k_EInputActionOrigin_PS4_RightStick_DPadEast=94;
      k_EInputActionOrigin_PS4_DPad_North=95;
      k_EInputActionOrigin_PS4_DPad_South=96;
      k_EInputActionOrigin_PS4_DPad_West=97;
      k_EInputActionOrigin_PS4_DPad_East=98;
      k_EInputActionOrigin_PS4_Gyro_Move=99;
      k_EInputActionOrigin_PS4_Gyro_Pitch=100;
      k_EInputActionOrigin_PS4_Gyro_Yaw=101;
      k_EInputActionOrigin_PS4_Gyro_Roll=102;
      k_EInputActionOrigin_PS4_DPad_Move=103;
      k_EInputActionOrigin_PS4_Reserved1=104;
      k_EInputActionOrigin_PS4_Reserved2=105;
      k_EInputActionOrigin_PS4_Reserved3=106;
      k_EInputActionOrigin_PS4_Reserved4=107;
      k_EInputActionOrigin_PS4_Reserved5=108;
      k_EInputActionOrigin_PS4_Reserved6=109;
      k_EInputActionOrigin_PS4_Reserved7=110;
      k_EInputActionOrigin_PS4_Reserved8=111;
      k_EInputActionOrigin_PS4_Reserved9=112;
      k_EInputActionOrigin_PS4_Reserved10=113;
      k_EInputActionOrigin_XBoxOne_A=114;
      k_EInputActionOrigin_XBoxOne_B=115;
      k_EInputActionOrigin_XBoxOne_X=116;
      k_EInputActionOrigin_XBoxOne_Y=117;
      k_EInputActionOrigin_XBoxOne_LeftBumper=118;
      k_EInputActionOrigin_XBoxOne_RightBumper=119;
      k_EInputActionOrigin_XBoxOne_Menu=120;
      k_EInputActionOrigin_XBoxOne_View=121;
      k_EInputActionOrigin_XBoxOne_LeftTrigger_Pull=122;
      k_EInputActionOrigin_XBoxOne_LeftTrigger_Click=123;
      k_EInputActionOrigin_XBoxOne_RightTrigger_Pull=124;
      k_EInputActionOrigin_XBoxOne_RightTrigger_Click=125;
      k_EInputActionOrigin_XBoxOne_LeftStick_Move=126;
      k_EInputActionOrigin_XBoxOne_LeftStick_Click=127;
      k_EInputActionOrigin_XBoxOne_LeftStick_DPadNorth=128;
      k_EInputActionOrigin_XBoxOne_LeftStick_DPadSouth=129;
      k_EInputActionOrigin_XBoxOne_LeftStick_DPadWest=130;
      k_EInputActionOrigin_XBoxOne_LeftStick_DPadEast=131;
      k_EInputActionOrigin_XBoxOne_RightStick_Move=132;
      k_EInputActionOrigin_XBoxOne_RightStick_Click=133;
      k_EInputActionOrigin_XBoxOne_RightStick_DPadNorth=134;
      k_EInputActionOrigin_XBoxOne_RightStick_DPadSouth=135;
      k_EInputActionOrigin_XBoxOne_RightStick_DPadWest=136;
      k_EInputActionOrigin_XBoxOne_RightStick_DPadEast=137;
      k_EInputActionOrigin_XBoxOne_DPad_North=138;
      k_EInputActionOrigin_XBoxOne_DPad_South=139;
      k_EInputActionOrigin_XBoxOne_DPad_West=140;
      k_EInputActionOrigin_XBoxOne_DPad_East=141;
      k_EInputActionOrigin_XBoxOne_DPad_Move=142;
      k_EInputActionOrigin_XBoxOne_LeftGrip_Lower=143;
      k_EInputActionOrigin_XBoxOne_LeftGrip_Upper=144;
      k_EInputActionOrigin_XBoxOne_RightGrip_Lower=145;
      k_EInputActionOrigin_XBoxOne_RightGrip_Upper=146;
      k_EInputActionOrigin_XBoxOne_Share=147;
      k_EInputActionOrigin_XBoxOne_Reserved6=148;
      k_EInputActionOrigin_XBoxOne_Reserved7=149;
      k_EInputActionOrigin_XBoxOne_Reserved8=150;
      k_EInputActionOrigin_XBoxOne_Reserved9=151;
      k_EInputActionOrigin_XBoxOne_Reserved10=152;
      k_EInputActionOrigin_XBox360_A=153;
      k_EInputActionOrigin_XBox360_B=154;
      k_EInputActionOrigin_XBox360_X=155;
      k_EInputActionOrigin_XBox360_Y=156;
      k_EInputActionOrigin_XBox360_LeftBumper=157;
      k_EInputActionOrigin_XBox360_RightBumper=158;
      k_EInputActionOrigin_XBox360_Start=159;
      k_EInputActionOrigin_XBox360_Back=160;
      k_EInputActionOrigin_XBox360_LeftTrigger_Pull=161;
      k_EInputActionOrigin_XBox360_LeftTrigger_Click=162;
      k_EInputActionOrigin_XBox360_RightTrigger_Pull=163;
      k_EInputActionOrigin_XBox360_RightTrigger_Click=164;
      k_EInputActionOrigin_XBox360_LeftStick_Move=165;
      k_EInputActionOrigin_XBox360_LeftStick_Click=166;
      k_EInputActionOrigin_XBox360_LeftStick_DPadNorth=167;
      k_EInputActionOrigin_XBox360_LeftStick_DPadSouth=168;
      k_EInputActionOrigin_XBox360_LeftStick_DPadWest=169;
      k_EInputActionOrigin_XBox360_LeftStick_DPadEast=170;
      k_EInputActionOrigin_XBox360_RightStick_Move=171;
      k_EInputActionOrigin_XBox360_RightStick_Click=172;
      k_EInputActionOrigin_XBox360_RightStick_DPadNorth=173;
      k_EInputActionOrigin_XBox360_RightStick_DPadSouth=174;
      k_EInputActionOrigin_XBox360_RightStick_DPadWest=175;
      k_EInputActionOrigin_XBox360_RightStick_DPadEast=176;
      k_EInputActionOrigin_XBox360_DPad_North=177;
      k_EInputActionOrigin_XBox360_DPad_South=178;
      k_EInputActionOrigin_XBox360_DPad_West=179;
      k_EInputActionOrigin_XBox360_DPad_East=180;
      k_EInputActionOrigin_XBox360_DPad_Move=181;
      k_EInputActionOrigin_XBox360_Reserved1=182;
      k_EInputActionOrigin_XBox360_Reserved2=183;
      k_EInputActionOrigin_XBox360_Reserved3=184;
      k_EInputActionOrigin_XBox360_Reserved4=185;
      k_EInputActionOrigin_XBox360_Reserved5=186;
      k_EInputActionOrigin_XBox360_Reserved6=187;
      k_EInputActionOrigin_XBox360_Reserved7=188;
      k_EInputActionOrigin_XBox360_Reserved8=189;
      k_EInputActionOrigin_XBox360_Reserved9=190;
      k_EInputActionOrigin_XBox360_Reserved10=191;
      k_EInputActionOrigin_Switch_A=192;
      k_EInputActionOrigin_Switch_B=193;
      k_EInputActionOrigin_Switch_X=194;
      k_EInputActionOrigin_Switch_Y=195;
      k_EInputActionOrigin_Switch_LeftBumper=196;
      k_EInputActionOrigin_Switch_RightBumper=197;
      k_EInputActionOrigin_Switch_Plus=198;
      k_EInputActionOrigin_Switch_Minus=199;
      k_EInputActionOrigin_Switch_Capture=200;
      k_EInputActionOrigin_Switch_LeftTrigger_Pull=201;
      k_EInputActionOrigin_Switch_LeftTrigger_Click=202;
      k_EInputActionOrigin_Switch_RightTrigger_Pull=203;
      k_EInputActionOrigin_Switch_RightTrigger_Click=204;
      k_EInputActionOrigin_Switch_LeftStick_Move=205;
      k_EInputActionOrigin_Switch_LeftStick_Click=206;
      k_EInputActionOrigin_Switch_LeftStick_DPadNorth=207;
      k_EInputActionOrigin_Switch_LeftStick_DPadSouth=208;
      k_EInputActionOrigin_Switch_LeftStick_DPadWest=209;
      k_EInputActionOrigin_Switch_LeftStick_DPadEast=210;
      k_EInputActionOrigin_Switch_RightStick_Move=211;
      k_EInputActionOrigin_Switch_RightStick_Click=212;
      k_EInputActionOrigin_Switch_RightStick_DPadNorth=213;
      k_EInputActionOrigin_Switch_RightStick_DPadSouth=214;
      k_EInputActionOrigin_Switch_RightStick_DPadWest=215;
      k_EInputActionOrigin_Switch_RightStick_DPadEast=216;
      k_EInputActionOrigin_Switch_DPad_North=217;
      k_EInputActionOrigin_Switch_DPad_South=218;
      k_EInputActionOrigin_Switch_DPad_West=219;
      k_EInputActionOrigin_Switch_DPad_East=220;
      k_EInputActionOrigin_Switch_ProGyro_Move=221;
      k_EInputActionOrigin_Switch_ProGyro_Pitch=222;
      k_EInputActionOrigin_Switch_ProGyro_Yaw=223;
      k_EInputActionOrigin_Switch_ProGyro_Roll=224;
      k_EInputActionOrigin_Switch_DPad_Move=225;
      k_EInputActionOrigin_Switch_Reserved1=226;
      k_EInputActionOrigin_Switch_Reserved2=227;
      k_EInputActionOrigin_Switch_Reserved3=228;
      k_EInputActionOrigin_Switch_Reserved4=229;
      k_EInputActionOrigin_Switch_Reserved5=230;
      k_EInputActionOrigin_Switch_Reserved6=231;
      k_EInputActionOrigin_Switch_Reserved7=232;
      k_EInputActionOrigin_Switch_Reserved8=233;
      k_EInputActionOrigin_Switch_Reserved9=234;
      k_EInputActionOrigin_Switch_Reserved10=235;
      k_EInputActionOrigin_Switch_RightGyro_Move=236;
      k_EInputActionOrigin_Switch_RightGyro_Pitch=237;
      k_EInputActionOrigin_Switch_RightGyro_Yaw=238;
      k_EInputActionOrigin_Switch_RightGyro_Roll=239;
      k_EInputActionOrigin_Switch_LeftGyro_Move=240;
      k_EInputActionOrigin_Switch_LeftGyro_Pitch=241;
      k_EInputActionOrigin_Switch_LeftGyro_Yaw=242;
      k_EInputActionOrigin_Switch_LeftGyro_Roll=243;
      k_EInputActionOrigin_Switch_LeftGrip_Lower=244;
      k_EInputActionOrigin_Switch_LeftGrip_Upper=245;
      k_EInputActionOrigin_Switch_RightGrip_Lower=246;
      k_EInputActionOrigin_Switch_RightGrip_Upper=247;
      k_EInputActionOrigin_Switch_JoyConButton_N=248;
      k_EInputActionOrigin_Switch_JoyConButton_E=249;
      k_EInputActionOrigin_Switch_JoyConButton_S=250;
      k_EInputActionOrigin_Switch_JoyConButton_W=251;
      k_EInputActionOrigin_Switch_Reserved15=252;
      k_EInputActionOrigin_Switch_Reserved16=253;
      k_EInputActionOrigin_Switch_Reserved17=254;
      k_EInputActionOrigin_Switch_Reserved18=255;
      k_EInputActionOrigin_Switch_Reserved19=256;
      k_EInputActionOrigin_Switch_Reserved20=257;
      k_EInputActionOrigin_PS5_X=258;
      k_EInputActionOrigin_PS5_Circle=259;
      k_EInputActionOrigin_PS5_Triangle=260;
      k_EInputActionOrigin_PS5_Square=261;
      k_EInputActionOrigin_PS5_LeftBumper=262;
      k_EInputActionOrigin_PS5_RightBumper=263;
      k_EInputActionOrigin_PS5_Option=264;
      k_EInputActionOrigin_PS5_Create=265;
      k_EInputActionOrigin_PS5_Mute=266;
      k_EInputActionOrigin_PS5_LeftPad_Touch=267;
      k_EInputActionOrigin_PS5_LeftPad_Swipe=268;
      k_EInputActionOrigin_PS5_LeftPad_Click=269;
      k_EInputActionOrigin_PS5_LeftPad_DPadNorth=270;
      k_EInputActionOrigin_PS5_LeftPad_DPadSouth=271;
      k_EInputActionOrigin_PS5_LeftPad_DPadWest=272;
      k_EInputActionOrigin_PS5_LeftPad_DPadEast=273;
      k_EInputActionOrigin_PS5_RightPad_Touch=274;
      k_EInputActionOrigin_PS5_RightPad_Swipe=275;
      k_EInputActionOrigin_PS5_RightPad_Click=276;
      k_EInputActionOrigin_PS5_RightPad_DPadNorth=277;
      k_EInputActionOrigin_PS5_RightPad_DPadSouth=278;
      k_EInputActionOrigin_PS5_RightPad_DPadWest=279;
      k_EInputActionOrigin_PS5_RightPad_DPadEast=280;
      k_EInputActionOrigin_PS5_CenterPad_Touch=281;
      k_EInputActionOrigin_PS5_CenterPad_Swipe=282;
      k_EInputActionOrigin_PS5_CenterPad_Click=283;
      k_EInputActionOrigin_PS5_CenterPad_DPadNorth=284;
      k_EInputActionOrigin_PS5_CenterPad_DPadSouth=285;
      k_EInputActionOrigin_PS5_CenterPad_DPadWest=286;
      k_EInputActionOrigin_PS5_CenterPad_DPadEast=287;
      k_EInputActionOrigin_PS5_LeftTrigger_Pull=288;
      k_EInputActionOrigin_PS5_LeftTrigger_Click=289;
      k_EInputActionOrigin_PS5_RightTrigger_Pull=290;
      k_EInputActionOrigin_PS5_RightTrigger_Click=291;
      k_EInputActionOrigin_PS5_LeftStick_Move=292;
      k_EInputActionOrigin_PS5_LeftStick_Click=293;
      k_EInputActionOrigin_PS5_LeftStick_DPadNorth=294;
      k_EInputActionOrigin_PS5_LeftStick_DPadSouth=295;
      k_EInputActionOrigin_PS5_LeftStick_DPadWest=296;
      k_EInputActionOrigin_PS5_LeftStick_DPadEast=297;
      k_EInputActionOrigin_PS5_RightStick_Move=298;
      k_EInputActionOrigin_PS5_RightStick_Click=299;
      k_EInputActionOrigin_PS5_RightStick_DPadNorth=300;
      k_EInputActionOrigin_PS5_RightStick_DPadSouth=301;
      k_EInputActionOrigin_PS5_RightStick_DPadWest=302;
      k_EInputActionOrigin_PS5_RightStick_DPadEast=303;
      k_EInputActionOrigin_PS5_DPad_North=304;
      k_EInputActionOrigin_PS5_DPad_South=305;
      k_EInputActionOrigin_PS5_DPad_West=306;
      k_EInputActionOrigin_PS5_DPad_East=307;
      k_EInputActionOrigin_PS5_Gyro_Move=308;
      k_EInputActionOrigin_PS5_Gyro_Pitch=309;
      k_EInputActionOrigin_PS5_Gyro_Yaw=310;
      k_EInputActionOrigin_PS5_Gyro_Roll=311;
      k_EInputActionOrigin_PS5_DPad_Move=312;
      k_EInputActionOrigin_PS5_LeftGrip=313;
      k_EInputActionOrigin_PS5_RightGrip=314;
      k_EInputActionOrigin_PS5_LeftFn=315;
      k_EInputActionOrigin_PS5_RightFn=316;
      k_EInputActionOrigin_PS5_Reserved5=317;
      k_EInputActionOrigin_PS5_Reserved6=318;
      k_EInputActionOrigin_PS5_Reserved7=319;
      k_EInputActionOrigin_PS5_Reserved8=320;
      k_EInputActionOrigin_PS5_Reserved9=321;
      k_EInputActionOrigin_PS5_Reserved10=322;
      k_EInputActionOrigin_PS5_Reserved11=323;
      k_EInputActionOrigin_PS5_Reserved12=324;
      k_EInputActionOrigin_PS5_Reserved13=325;
      k_EInputActionOrigin_PS5_Reserved14=326;
      k_EInputActionOrigin_PS5_Reserved15=327;
      k_EInputActionOrigin_PS5_Reserved16=328;
      k_EInputActionOrigin_PS5_Reserved17=329;
      k_EInputActionOrigin_PS5_Reserved18=330;
      k_EInputActionOrigin_PS5_Reserved19=331;
      k_EInputActionOrigin_PS5_Reserved20=332;
      k_EInputActionOrigin_SteamDeck_A=333;
      k_EInputActionOrigin_SteamDeck_B=334;
      k_EInputActionOrigin_SteamDeck_X=335;
      k_EInputActionOrigin_SteamDeck_Y=336;
      k_EInputActionOrigin_SteamDeck_L1=337;
      k_EInputActionOrigin_SteamDeck_R1=338;
      k_EInputActionOrigin_SteamDeck_Menu=339;
      k_EInputActionOrigin_SteamDeck_View=340;
      k_EInputActionOrigin_SteamDeck_LeftPad_Touch=341;
      k_EInputActionOrigin_SteamDeck_LeftPad_Swipe=342;
      k_EInputActionOrigin_SteamDeck_LeftPad_Click=343;
      k_EInputActionOrigin_SteamDeck_LeftPad_DPadNorth=344;
      k_EInputActionOrigin_SteamDeck_LeftPad_DPadSouth=345;
      k_EInputActionOrigin_SteamDeck_LeftPad_DPadWest=346;
      k_EInputActionOrigin_SteamDeck_LeftPad_DPadEast=347;
      k_EInputActionOrigin_SteamDeck_RightPad_Touch=348;
      k_EInputActionOrigin_SteamDeck_RightPad_Swipe=349;
      k_EInputActionOrigin_SteamDeck_RightPad_Click=350;
      k_EInputActionOrigin_SteamDeck_RightPad_DPadNorth=351;
      k_EInputActionOrigin_SteamDeck_RightPad_DPadSouth=352;
      k_EInputActionOrigin_SteamDeck_RightPad_DPadWest=353;
      k_EInputActionOrigin_SteamDeck_RightPad_DPadEast=354;
      k_EInputActionOrigin_SteamDeck_L2_SoftPull=355;
      k_EInputActionOrigin_SteamDeck_L2=356;
      k_EInputActionOrigin_SteamDeck_R2_SoftPull=357;
      k_EInputActionOrigin_SteamDeck_R2=358;
      k_EInputActionOrigin_SteamDeck_LeftStick_Move=359;
      k_EInputActionOrigin_SteamDeck_L3=360;
      k_EInputActionOrigin_SteamDeck_LeftStick_DPadNorth=361;
      k_EInputActionOrigin_SteamDeck_LeftStick_DPadSouth=362;
      k_EInputActionOrigin_SteamDeck_LeftStick_DPadWest=363;
      k_EInputActionOrigin_SteamDeck_LeftStick_DPadEast=364;
      k_EInputActionOrigin_SteamDeck_LeftStick_Touch=365;
      k_EInputActionOrigin_SteamDeck_RightStick_Move=366;
      k_EInputActionOrigin_SteamDeck_R3=367;
      k_EInputActionOrigin_SteamDeck_RightStick_DPadNorth=368;
      k_EInputActionOrigin_SteamDeck_RightStick_DPadSouth=369;
      k_EInputActionOrigin_SteamDeck_RightStick_DPadWest=370;
      k_EInputActionOrigin_SteamDeck_RightStick_DPadEast=371;
      k_EInputActionOrigin_SteamDeck_RightStick_Touch=372;
      k_EInputActionOrigin_SteamDeck_L4=373;
      k_EInputActionOrigin_SteamDeck_R4=374;
      k_EInputActionOrigin_SteamDeck_L5=375;
      k_EInputActionOrigin_SteamDeck_R5=376;
      k_EInputActionOrigin_SteamDeck_DPad_Move=377;
      k_EInputActionOrigin_SteamDeck_DPad_North=378;
      k_EInputActionOrigin_SteamDeck_DPad_South=379;
      k_EInputActionOrigin_SteamDeck_DPad_West=380;
      k_EInputActionOrigin_SteamDeck_DPad_East=381;
      k_EInputActionOrigin_SteamDeck_Gyro_Move=382;
      k_EInputActionOrigin_SteamDeck_Gyro_Pitch=383;
      k_EInputActionOrigin_SteamDeck_Gyro_Yaw=384;
      k_EInputActionOrigin_SteamDeck_Gyro_Roll=385;
      k_EInputActionOrigin_SteamDeck_Reserved1=386;
      k_EInputActionOrigin_SteamDeck_Reserved2=387;
      k_EInputActionOrigin_SteamDeck_Reserved3=388;
      k_EInputActionOrigin_SteamDeck_Reserved4=389;
      k_EInputActionOrigin_SteamDeck_Reserved5=390;
      k_EInputActionOrigin_SteamDeck_Reserved6=391;
      k_EInputActionOrigin_SteamDeck_Reserved7=392;
      k_EInputActionOrigin_SteamDeck_Reserved8=393;
      k_EInputActionOrigin_SteamDeck_Reserved9=394;
      k_EInputActionOrigin_SteamDeck_Reserved10=395;
      k_EInputActionOrigin_SteamDeck_Reserved11=396;
      k_EInputActionOrigin_SteamDeck_Reserved12=397;
      k_EInputActionOrigin_SteamDeck_Reserved13=398;
      k_EInputActionOrigin_SteamDeck_Reserved14=399;
      k_EInputActionOrigin_SteamDeck_Reserved15=400;
      k_EInputActionOrigin_SteamDeck_Reserved16=401;
      k_EInputActionOrigin_SteamDeck_Reserved17=402;
      k_EInputActionOrigin_SteamDeck_Reserved18=403;
      k_EInputActionOrigin_SteamDeck_Reserved19=404;
      k_EInputActionOrigin_SteamDeck_Reserved20=405;
      k_EInputActionOrigin_Horipad_M1=406;
      k_EInputActionOrigin_Horipad_M2=407;
      k_EInputActionOrigin_Horipad_L4=408;
      k_EInputActionOrigin_Horipad_R4=409;
      k_EInputActionOrigin_LenovoLegionGo_A=410;
      k_EInputActionOrigin_LenovoLegionGo_B=411;
      k_EInputActionOrigin_LenovoLegionGo_X=412;
      k_EInputActionOrigin_LenovoLegionGo_Y=413;
      k_EInputActionOrigin_LenovoLegionGo_LB=414;
      k_EInputActionOrigin_LenovoLegionGo_RB=415;
      k_EInputActionOrigin_LenovoLegionGo_Menu=416;
      k_EInputActionOrigin_LenovoLegionGo_View=417;
      k_EInputActionOrigin_LenovoLegionGo_LeftPad_Touch=418;
      k_EInputActionOrigin_LenovoLegionGo_LeftPad_Swipe=419;
      k_EInputActionOrigin_LenovoLegionGo_LeftPad_Click=420;
      k_EInputActionOrigin_LenovoLegionGo_LeftPad_DPadNorth=421;
      k_EInputActionOrigin_LenovoLegionGo_LeftPad_DPadSouth=422;
      k_EInputActionOrigin_LenovoLegionGo_LeftPad_DPadWest=423;
      k_EInputActionOrigin_LenovoLegionGo_LeftPad_DPadEast=424;
      k_EInputActionOrigin_LenovoLegionGo_RightPad_Touch=425;
      k_EInputActionOrigin_LenovoLegionGo_RightPad_Swipe=426;
      k_EInputActionOrigin_LenovoLegionGo_RightPad_Click=427;
      k_EInputActionOrigin_LenovoLegionGo_RightPad_DPadNorth=428;
      k_EInputActionOrigin_LenovoLegionGo_RightPad_DPadSouth=429;
      k_EInputActionOrigin_LenovoLegionGo_RightPad_DPadWest=430;
      k_EInputActionOrigin_LenovoLegionGo_RightPad_DPadEast=431;
      k_EInputActionOrigin_LenovoLegionGo_LT_SoftPull=432;
      k_EInputActionOrigin_LenovoLegionGo_LT=433;
      k_EInputActionOrigin_LenovoLegionGo_RT_SoftPull=434;
      k_EInputActionOrigin_LenovoLegionGo_RT=435;
      k_EInputActionOrigin_LenovoLegionGo_LeftStick_Move=436;
      k_EInputActionOrigin_LenovoLegionGo_LS=437;
      k_EInputActionOrigin_LenovoLegionGo_LeftStick_DPadNorth=438;
      k_EInputActionOrigin_LenovoLegionGo_LeftStick_DPadSouth=439;
      k_EInputActionOrigin_LenovoLegionGo_LeftStick_DPadWest=440;
      k_EInputActionOrigin_LenovoLegionGo_LeftStick_DPadEast=441;
      k_EInputActionOrigin_LenovoLegionGo_RightStick_Move=442;
      k_EInputActionOrigin_LenovoLegionGo_RS=443;
      k_EInputActionOrigin_LenovoLegionGo_RightStick_DPadNorth=444;
      k_EInputActionOrigin_LenovoLegionGo_RightStick_DPadSouth=445;
      k_EInputActionOrigin_LenovoLegionGo_RightStick_DPadWest=446;
      k_EInputActionOrigin_LenovoLegionGo_RightStick_DPadEast=447;
      k_EInputActionOrigin_LenovoLegionGo_Y1=448;
      k_EInputActionOrigin_LenovoLegionGo_Y2=449;
      k_EInputActionOrigin_LenovoLegionGo_DPad_Move=450;
      k_EInputActionOrigin_LenovoLegionGo_DPad_North=451;
      k_EInputActionOrigin_LenovoLegionGo_DPad_South=452;
      k_EInputActionOrigin_LenovoLegionGo_DPad_West=453;
      k_EInputActionOrigin_LenovoLegionGo_DPad_East=454;
      k_EInputActionOrigin_LenovoLegionGo_Gyro_Move=455;
      k_EInputActionOrigin_LenovoLegionGo_Gyro_Pitch=456;
      k_EInputActionOrigin_LenovoLegionGo_Gyro_Yaw=457;
      k_EInputActionOrigin_LenovoLegionGo_Gyro_Roll=458;
      k_EInputActionOrigin_LenovoLegionGo_Reserved1=459;
      k_EInputActionOrigin_LenovoLegionGo_Reserved2=460;
      k_EInputActionOrigin_LenovoLegionGo_Reserved3=461;
      k_EInputActionOrigin_LenovoLegionGo_Reserved4=462;
      k_EInputActionOrigin_LenovoLegionGo_Reserved5=463;
      k_EInputActionOrigin_LenovoLegionGo_Reserved6=464;
      k_EInputActionOrigin_LenovoLegionGo_Reserved7=465;
      k_EInputActionOrigin_LenovoLegionGo_Reserved8=466;
      k_EInputActionOrigin_LenovoLegionGo_Reserved9=467;
      k_EInputActionOrigin_LenovoLegionGo_Reserved10=468;
      k_EInputActionOrigin_LenovoLegionGo_Reserved11=469;
      k_EInputActionOrigin_LenovoLegionGo_Reserved12=470;
      k_EInputActionOrigin_LenovoLegionGo_Reserved13=471;
      k_EInputActionOrigin_LenovoLegionGo_Reserved14=472;
      k_EInputActionOrigin_LenovoLegionGo_Reserved15=473;
      k_EInputActionOrigin_LenovoLegionGo_Reserved16=474;
      k_EInputActionOrigin_LenovoLegionGo_Reserved17=475;
      k_EInputActionOrigin_LenovoLegionGo_Reserved18=476;
      k_EInputActionOrigin_LenovoLegionGo_Reserved19=477;
      k_EInputActionOrigin_LenovoLegionGo_Reserved20=478;
      k_EInputActionOrigin_Generic_L4=479;
      k_EInputActionOrigin_Generic_R4=480;
      k_EInputActionOrigin_Generic_L5=481;
      k_EInputActionOrigin_Generic_R5=482;
      k_EInputActionOrigin_Generic_PL=483;
      k_EInputActionOrigin_Generic_PR=484;
      k_EInputActionOrigin_Generic_C=485;
      k_EInputActionOrigin_Generic_Z=486;
      k_EInputActionOrigin_Generic_MISC1=487;
      k_EInputActionOrigin_Generic_MISC2=488;
      k_EInputActionOrigin_Generic_MISC3=489;
      k_EInputActionOrigin_Generic_MISC4=490;
      k_EInputActionOrigin_Generic_MISC5=491;
      k_EInputActionOrigin_Generic_MISC6=492;
      k_EInputActionOrigin_Generic_MISC7=493;
      k_EInputActionOrigin_Generic_MISC8=494;
      k_EInputActionOrigin_Count=495;
      k_EInputActionOrigin_MaximumPossibleValue=32767;

type { TEXboxOrigin }
     PPEXboxOrigin=^PEXboxOrigin;
     PEXboxOrigin=^TEXboxOrigin;
     TEXboxOrigin=TSteamInt32;

const k_EXboxOrigin_A=0;
      k_EXboxOrigin_B=1;
      k_EXboxOrigin_X=2;
      k_EXboxOrigin_Y=3;
      k_EXboxOrigin_LeftBumper=4;
      k_EXboxOrigin_RightBumper=5;
      k_EXboxOrigin_Menu=6;
      k_EXboxOrigin_View=7;
      k_EXboxOrigin_LeftTrigger_Pull=8;
      k_EXboxOrigin_LeftTrigger_Click=9;
      k_EXboxOrigin_RightTrigger_Pull=10;
      k_EXboxOrigin_RightTrigger_Click=11;
      k_EXboxOrigin_LeftStick_Move=12;
      k_EXboxOrigin_LeftStick_Click=13;
      k_EXboxOrigin_LeftStick_DPadNorth=14;
      k_EXboxOrigin_LeftStick_DPadSouth=15;
      k_EXboxOrigin_LeftStick_DPadWest=16;
      k_EXboxOrigin_LeftStick_DPadEast=17;
      k_EXboxOrigin_RightStick_Move=18;
      k_EXboxOrigin_RightStick_Click=19;
      k_EXboxOrigin_RightStick_DPadNorth=20;
      k_EXboxOrigin_RightStick_DPadSouth=21;
      k_EXboxOrigin_RightStick_DPadWest=22;
      k_EXboxOrigin_RightStick_DPadEast=23;
      k_EXboxOrigin_DPad_North=24;
      k_EXboxOrigin_DPad_South=25;
      k_EXboxOrigin_DPad_West=26;
      k_EXboxOrigin_DPad_East=27;
      k_EXboxOrigin_Count=28;

type { TESteamControllerPad }
     PPESteamControllerPad=^PESteamControllerPad;
     PESteamControllerPad=^TESteamControllerPad;
     TESteamControllerPad=TSteamInt32;

const k_ESteamControllerPad_Left=0;
      k_ESteamControllerPad_Right=1;

type { TEControllerHapticLocation }
     PPEControllerHapticLocation=^PEControllerHapticLocation;
     PEControllerHapticLocation=^TEControllerHapticLocation;
     TEControllerHapticLocation=TSteamInt32;

const k_EControllerHapticLocation_Left=1;
      k_EControllerHapticLocation_Right=2;
      k_EControllerHapticLocation_Both=3;

type { TEControllerHapticType }
     PPEControllerHapticType=^PEControllerHapticType;
     PEControllerHapticType=^TEControllerHapticType;
     TEControllerHapticType=TSteamInt32;

const k_EControllerHapticType_Off=0;
      k_EControllerHapticType_Tick=1;
      k_EControllerHapticType_Click=2;

type { TESteamInputType }
     PPESteamInputType=^PESteamInputType;
     PESteamInputType=^TESteamInputType;
     TESteamInputType=TSteamInt32;

const k_ESteamInputType_Unknown=0;
      k_ESteamInputType_SteamController=1;
      k_ESteamInputType_XBox360Controller=2;
      k_ESteamInputType_XBoxOneController=3;
      k_ESteamInputType_GenericGamepad=4;
      k_ESteamInputType_PS4Controller=5;
      k_ESteamInputType_AppleMFiController=6;
      k_ESteamInputType_AndroidController=7;
      k_ESteamInputType_SwitchJoyConPair=8;
      k_ESteamInputType_SwitchJoyConSingle=9;
      k_ESteamInputType_SwitchProController=10;
      k_ESteamInputType_MobileTouch=11;
      k_ESteamInputType_PS3Controller=12;
      k_ESteamInputType_PS5Controller=13;
      k_ESteamInputType_SteamDeckController=14;
      k_ESteamInputType_Count=15;
      k_ESteamInputType_MaximumPossibleValue=255;

type { TESteamInputConfigurationEnableType }
     PPESteamInputConfigurationEnableType=^PESteamInputConfigurationEnableType;
     PESteamInputConfigurationEnableType=^TESteamInputConfigurationEnableType;
     TESteamInputConfigurationEnableType=TSteamInt32;

const k_ESteamInputConfigurationEnableType_None=0;
      k_ESteamInputConfigurationEnableType_Playstation=1;
      k_ESteamInputConfigurationEnableType_Xbox=2;
      k_ESteamInputConfigurationEnableType_Generic=4;
      k_ESteamInputConfigurationEnableType_Switch=8;

type { TESteamInputLEDFlag }
     PPESteamInputLEDFlag=^PESteamInputLEDFlag;
     PESteamInputLEDFlag=^TESteamInputLEDFlag;
     TESteamInputLEDFlag=TSteamInt32;

const k_ESteamInputLEDFlag_SetColor=0;
      k_ESteamInputLEDFlag_RestoreUserDefault=1;

type { TESteamInputGlyphSize }
     PPESteamInputGlyphSize=^PESteamInputGlyphSize;
     PESteamInputGlyphSize=^TESteamInputGlyphSize;
     TESteamInputGlyphSize=TSteamInt32;

const k_ESteamInputGlyphSize_Small=0;
      k_ESteamInputGlyphSize_Medium=1;
      k_ESteamInputGlyphSize_Large=2;
      k_ESteamInputGlyphSize_Count=3;

type { TESteamInputGlyphStyle }
     PPESteamInputGlyphStyle=^PESteamInputGlyphStyle;
     PESteamInputGlyphStyle=^TESteamInputGlyphStyle;
     TESteamInputGlyphStyle=TSteamInt32;

const ESteamInputGlyphStyle_Knockout=0;
      ESteamInputGlyphStyle_Light=1;
      ESteamInputGlyphStyle_Dark=2;
      ESteamInputGlyphStyle_NeutralColorABXY=16;
      ESteamInputGlyphStyle_SolidABXY=32;

type { TESteamInputActionEventType }
     PPESteamInputActionEventType=^PESteamInputActionEventType;
     PESteamInputActionEventType=^TESteamInputActionEventType;
     TESteamInputActionEventType=TSteamInt32;

const ESteamInputActionEventType_DigitalAction=0;
      ESteamInputActionEventType_AnalogAction=1;

type { TEControllerActionOrigin }
     PPEControllerActionOrigin=^PEControllerActionOrigin;
     PEControllerActionOrigin=^TEControllerActionOrigin;
     TEControllerActionOrigin=TSteamInt32;

const k_EControllerActionOrigin_None=0;
      k_EControllerActionOrigin_A=1;
      k_EControllerActionOrigin_B=2;
      k_EControllerActionOrigin_X=3;
      k_EControllerActionOrigin_Y=4;
      k_EControllerActionOrigin_LeftBumper=5;
      k_EControllerActionOrigin_RightBumper=6;
      k_EControllerActionOrigin_LeftGrip=7;
      k_EControllerActionOrigin_RightGrip=8;
      k_EControllerActionOrigin_Start=9;
      k_EControllerActionOrigin_Back=10;
      k_EControllerActionOrigin_LeftPad_Touch=11;
      k_EControllerActionOrigin_LeftPad_Swipe=12;
      k_EControllerActionOrigin_LeftPad_Click=13;
      k_EControllerActionOrigin_LeftPad_DPadNorth=14;
      k_EControllerActionOrigin_LeftPad_DPadSouth=15;
      k_EControllerActionOrigin_LeftPad_DPadWest=16;
      k_EControllerActionOrigin_LeftPad_DPadEast=17;
      k_EControllerActionOrigin_RightPad_Touch=18;
      k_EControllerActionOrigin_RightPad_Swipe=19;
      k_EControllerActionOrigin_RightPad_Click=20;
      k_EControllerActionOrigin_RightPad_DPadNorth=21;
      k_EControllerActionOrigin_RightPad_DPadSouth=22;
      k_EControllerActionOrigin_RightPad_DPadWest=23;
      k_EControllerActionOrigin_RightPad_DPadEast=24;
      k_EControllerActionOrigin_LeftTrigger_Pull=25;
      k_EControllerActionOrigin_LeftTrigger_Click=26;
      k_EControllerActionOrigin_RightTrigger_Pull=27;
      k_EControllerActionOrigin_RightTrigger_Click=28;
      k_EControllerActionOrigin_LeftStick_Move=29;
      k_EControllerActionOrigin_LeftStick_Click=30;
      k_EControllerActionOrigin_LeftStick_DPadNorth=31;
      k_EControllerActionOrigin_LeftStick_DPadSouth=32;
      k_EControllerActionOrigin_LeftStick_DPadWest=33;
      k_EControllerActionOrigin_LeftStick_DPadEast=34;
      k_EControllerActionOrigin_Gyro_Move=35;
      k_EControllerActionOrigin_Gyro_Pitch=36;
      k_EControllerActionOrigin_Gyro_Yaw=37;
      k_EControllerActionOrigin_Gyro_Roll=38;
      k_EControllerActionOrigin_PS4_X=39;
      k_EControllerActionOrigin_PS4_Circle=40;
      k_EControllerActionOrigin_PS4_Triangle=41;
      k_EControllerActionOrigin_PS4_Square=42;
      k_EControllerActionOrigin_PS4_LeftBumper=43;
      k_EControllerActionOrigin_PS4_RightBumper=44;
      k_EControllerActionOrigin_PS4_Options=45;
      k_EControllerActionOrigin_PS4_Share=46;
      k_EControllerActionOrigin_PS4_LeftPad_Touch=47;
      k_EControllerActionOrigin_PS4_LeftPad_Swipe=48;
      k_EControllerActionOrigin_PS4_LeftPad_Click=49;
      k_EControllerActionOrigin_PS4_LeftPad_DPadNorth=50;
      k_EControllerActionOrigin_PS4_LeftPad_DPadSouth=51;
      k_EControllerActionOrigin_PS4_LeftPad_DPadWest=52;
      k_EControllerActionOrigin_PS4_LeftPad_DPadEast=53;
      k_EControllerActionOrigin_PS4_RightPad_Touch=54;
      k_EControllerActionOrigin_PS4_RightPad_Swipe=55;
      k_EControllerActionOrigin_PS4_RightPad_Click=56;
      k_EControllerActionOrigin_PS4_RightPad_DPadNorth=57;
      k_EControllerActionOrigin_PS4_RightPad_DPadSouth=58;
      k_EControllerActionOrigin_PS4_RightPad_DPadWest=59;
      k_EControllerActionOrigin_PS4_RightPad_DPadEast=60;
      k_EControllerActionOrigin_PS4_CenterPad_Touch=61;
      k_EControllerActionOrigin_PS4_CenterPad_Swipe=62;
      k_EControllerActionOrigin_PS4_CenterPad_Click=63;
      k_EControllerActionOrigin_PS4_CenterPad_DPadNorth=64;
      k_EControllerActionOrigin_PS4_CenterPad_DPadSouth=65;
      k_EControllerActionOrigin_PS4_CenterPad_DPadWest=66;
      k_EControllerActionOrigin_PS4_CenterPad_DPadEast=67;
      k_EControllerActionOrigin_PS4_LeftTrigger_Pull=68;
      k_EControllerActionOrigin_PS4_LeftTrigger_Click=69;
      k_EControllerActionOrigin_PS4_RightTrigger_Pull=70;
      k_EControllerActionOrigin_PS4_RightTrigger_Click=71;
      k_EControllerActionOrigin_PS4_LeftStick_Move=72;
      k_EControllerActionOrigin_PS4_LeftStick_Click=73;
      k_EControllerActionOrigin_PS4_LeftStick_DPadNorth=74;
      k_EControllerActionOrigin_PS4_LeftStick_DPadSouth=75;
      k_EControllerActionOrigin_PS4_LeftStick_DPadWest=76;
      k_EControllerActionOrigin_PS4_LeftStick_DPadEast=77;
      k_EControllerActionOrigin_PS4_RightStick_Move=78;
      k_EControllerActionOrigin_PS4_RightStick_Click=79;
      k_EControllerActionOrigin_PS4_RightStick_DPadNorth=80;
      k_EControllerActionOrigin_PS4_RightStick_DPadSouth=81;
      k_EControllerActionOrigin_PS4_RightStick_DPadWest=82;
      k_EControllerActionOrigin_PS4_RightStick_DPadEast=83;
      k_EControllerActionOrigin_PS4_DPad_North=84;
      k_EControllerActionOrigin_PS4_DPad_South=85;
      k_EControllerActionOrigin_PS4_DPad_West=86;
      k_EControllerActionOrigin_PS4_DPad_East=87;
      k_EControllerActionOrigin_PS4_Gyro_Move=88;
      k_EControllerActionOrigin_PS4_Gyro_Pitch=89;
      k_EControllerActionOrigin_PS4_Gyro_Yaw=90;
      k_EControllerActionOrigin_PS4_Gyro_Roll=91;
      k_EControllerActionOrigin_XBoxOne_A=92;
      k_EControllerActionOrigin_XBoxOne_B=93;
      k_EControllerActionOrigin_XBoxOne_X=94;
      k_EControllerActionOrigin_XBoxOne_Y=95;
      k_EControllerActionOrigin_XBoxOne_LeftBumper=96;
      k_EControllerActionOrigin_XBoxOne_RightBumper=97;
      k_EControllerActionOrigin_XBoxOne_Menu=98;
      k_EControllerActionOrigin_XBoxOne_View=99;
      k_EControllerActionOrigin_XBoxOne_LeftTrigger_Pull=100;
      k_EControllerActionOrigin_XBoxOne_LeftTrigger_Click=101;
      k_EControllerActionOrigin_XBoxOne_RightTrigger_Pull=102;
      k_EControllerActionOrigin_XBoxOne_RightTrigger_Click=103;
      k_EControllerActionOrigin_XBoxOne_LeftStick_Move=104;
      k_EControllerActionOrigin_XBoxOne_LeftStick_Click=105;
      k_EControllerActionOrigin_XBoxOne_LeftStick_DPadNorth=106;
      k_EControllerActionOrigin_XBoxOne_LeftStick_DPadSouth=107;
      k_EControllerActionOrigin_XBoxOne_LeftStick_DPadWest=108;
      k_EControllerActionOrigin_XBoxOne_LeftStick_DPadEast=109;
      k_EControllerActionOrigin_XBoxOne_RightStick_Move=110;
      k_EControllerActionOrigin_XBoxOne_RightStick_Click=111;
      k_EControllerActionOrigin_XBoxOne_RightStick_DPadNorth=112;
      k_EControllerActionOrigin_XBoxOne_RightStick_DPadSouth=113;
      k_EControllerActionOrigin_XBoxOne_RightStick_DPadWest=114;
      k_EControllerActionOrigin_XBoxOne_RightStick_DPadEast=115;
      k_EControllerActionOrigin_XBoxOne_DPad_North=116;
      k_EControllerActionOrigin_XBoxOne_DPad_South=117;
      k_EControllerActionOrigin_XBoxOne_DPad_West=118;
      k_EControllerActionOrigin_XBoxOne_DPad_East=119;
      k_EControllerActionOrigin_XBox360_A=120;
      k_EControllerActionOrigin_XBox360_B=121;
      k_EControllerActionOrigin_XBox360_X=122;
      k_EControllerActionOrigin_XBox360_Y=123;
      k_EControllerActionOrigin_XBox360_LeftBumper=124;
      k_EControllerActionOrigin_XBox360_RightBumper=125;
      k_EControllerActionOrigin_XBox360_Start=126;
      k_EControllerActionOrigin_XBox360_Back=127;
      k_EControllerActionOrigin_XBox360_LeftTrigger_Pull=128;
      k_EControllerActionOrigin_XBox360_LeftTrigger_Click=129;
      k_EControllerActionOrigin_XBox360_RightTrigger_Pull=130;
      k_EControllerActionOrigin_XBox360_RightTrigger_Click=131;
      k_EControllerActionOrigin_XBox360_LeftStick_Move=132;
      k_EControllerActionOrigin_XBox360_LeftStick_Click=133;
      k_EControllerActionOrigin_XBox360_LeftStick_DPadNorth=134;
      k_EControllerActionOrigin_XBox360_LeftStick_DPadSouth=135;
      k_EControllerActionOrigin_XBox360_LeftStick_DPadWest=136;
      k_EControllerActionOrigin_XBox360_LeftStick_DPadEast=137;
      k_EControllerActionOrigin_XBox360_RightStick_Move=138;
      k_EControllerActionOrigin_XBox360_RightStick_Click=139;
      k_EControllerActionOrigin_XBox360_RightStick_DPadNorth=140;
      k_EControllerActionOrigin_XBox360_RightStick_DPadSouth=141;
      k_EControllerActionOrigin_XBox360_RightStick_DPadWest=142;
      k_EControllerActionOrigin_XBox360_RightStick_DPadEast=143;
      k_EControllerActionOrigin_XBox360_DPad_North=144;
      k_EControllerActionOrigin_XBox360_DPad_South=145;
      k_EControllerActionOrigin_XBox360_DPad_West=146;
      k_EControllerActionOrigin_XBox360_DPad_East=147;
      k_EControllerActionOrigin_SteamV2_A=148;
      k_EControllerActionOrigin_SteamV2_B=149;
      k_EControllerActionOrigin_SteamV2_X=150;
      k_EControllerActionOrigin_SteamV2_Y=151;
      k_EControllerActionOrigin_SteamV2_LeftBumper=152;
      k_EControllerActionOrigin_SteamV2_RightBumper=153;
      k_EControllerActionOrigin_SteamV2_LeftGrip_Lower=154;
      k_EControllerActionOrigin_SteamV2_LeftGrip_Upper=155;
      k_EControllerActionOrigin_SteamV2_RightGrip_Lower=156;
      k_EControllerActionOrigin_SteamV2_RightGrip_Upper=157;
      k_EControllerActionOrigin_SteamV2_LeftBumper_Pressure=158;
      k_EControllerActionOrigin_SteamV2_RightBumper_Pressure=159;
      k_EControllerActionOrigin_SteamV2_LeftGrip_Pressure=160;
      k_EControllerActionOrigin_SteamV2_RightGrip_Pressure=161;
      k_EControllerActionOrigin_SteamV2_LeftGrip_Upper_Pressure=162;
      k_EControllerActionOrigin_SteamV2_RightGrip_Upper_Pressure=163;
      k_EControllerActionOrigin_SteamV2_Start=164;
      k_EControllerActionOrigin_SteamV2_Back=165;
      k_EControllerActionOrigin_SteamV2_LeftPad_Touch=166;
      k_EControllerActionOrigin_SteamV2_LeftPad_Swipe=167;
      k_EControllerActionOrigin_SteamV2_LeftPad_Click=168;
      k_EControllerActionOrigin_SteamV2_LeftPad_Pressure=169;
      k_EControllerActionOrigin_SteamV2_LeftPad_DPadNorth=170;
      k_EControllerActionOrigin_SteamV2_LeftPad_DPadSouth=171;
      k_EControllerActionOrigin_SteamV2_LeftPad_DPadWest=172;
      k_EControllerActionOrigin_SteamV2_LeftPad_DPadEast=173;
      k_EControllerActionOrigin_SteamV2_RightPad_Touch=174;
      k_EControllerActionOrigin_SteamV2_RightPad_Swipe=175;
      k_EControllerActionOrigin_SteamV2_RightPad_Click=176;
      k_EControllerActionOrigin_SteamV2_RightPad_Pressure=177;
      k_EControllerActionOrigin_SteamV2_RightPad_DPadNorth=178;
      k_EControllerActionOrigin_SteamV2_RightPad_DPadSouth=179;
      k_EControllerActionOrigin_SteamV2_RightPad_DPadWest=180;
      k_EControllerActionOrigin_SteamV2_RightPad_DPadEast=181;
      k_EControllerActionOrigin_SteamV2_LeftTrigger_Pull=182;
      k_EControllerActionOrigin_SteamV2_LeftTrigger_Click=183;
      k_EControllerActionOrigin_SteamV2_RightTrigger_Pull=184;
      k_EControllerActionOrigin_SteamV2_RightTrigger_Click=185;
      k_EControllerActionOrigin_SteamV2_LeftStick_Move=186;
      k_EControllerActionOrigin_SteamV2_LeftStick_Click=187;
      k_EControllerActionOrigin_SteamV2_LeftStick_DPadNorth=188;
      k_EControllerActionOrigin_SteamV2_LeftStick_DPadSouth=189;
      k_EControllerActionOrigin_SteamV2_LeftStick_DPadWest=190;
      k_EControllerActionOrigin_SteamV2_LeftStick_DPadEast=191;
      k_EControllerActionOrigin_SteamV2_Gyro_Move=192;
      k_EControllerActionOrigin_SteamV2_Gyro_Pitch=193;
      k_EControllerActionOrigin_SteamV2_Gyro_Yaw=194;
      k_EControllerActionOrigin_SteamV2_Gyro_Roll=195;
      k_EControllerActionOrigin_Switch_A=196;
      k_EControllerActionOrigin_Switch_B=197;
      k_EControllerActionOrigin_Switch_X=198;
      k_EControllerActionOrigin_Switch_Y=199;
      k_EControllerActionOrigin_Switch_LeftBumper=200;
      k_EControllerActionOrigin_Switch_RightBumper=201;
      k_EControllerActionOrigin_Switch_Plus=202;
      k_EControllerActionOrigin_Switch_Minus=203;
      k_EControllerActionOrigin_Switch_Capture=204;
      k_EControllerActionOrigin_Switch_LeftTrigger_Pull=205;
      k_EControllerActionOrigin_Switch_LeftTrigger_Click=206;
      k_EControllerActionOrigin_Switch_RightTrigger_Pull=207;
      k_EControllerActionOrigin_Switch_RightTrigger_Click=208;
      k_EControllerActionOrigin_Switch_LeftStick_Move=209;
      k_EControllerActionOrigin_Switch_LeftStick_Click=210;
      k_EControllerActionOrigin_Switch_LeftStick_DPadNorth=211;
      k_EControllerActionOrigin_Switch_LeftStick_DPadSouth=212;
      k_EControllerActionOrigin_Switch_LeftStick_DPadWest=213;
      k_EControllerActionOrigin_Switch_LeftStick_DPadEast=214;
      k_EControllerActionOrigin_Switch_RightStick_Move=215;
      k_EControllerActionOrigin_Switch_RightStick_Click=216;
      k_EControllerActionOrigin_Switch_RightStick_DPadNorth=217;
      k_EControllerActionOrigin_Switch_RightStick_DPadSouth=218;
      k_EControllerActionOrigin_Switch_RightStick_DPadWest=219;
      k_EControllerActionOrigin_Switch_RightStick_DPadEast=220;
      k_EControllerActionOrigin_Switch_DPad_North=221;
      k_EControllerActionOrigin_Switch_DPad_South=222;
      k_EControllerActionOrigin_Switch_DPad_West=223;
      k_EControllerActionOrigin_Switch_DPad_East=224;
      k_EControllerActionOrigin_Switch_ProGyro_Move=225;
      k_EControllerActionOrigin_Switch_ProGyro_Pitch=226;
      k_EControllerActionOrigin_Switch_ProGyro_Yaw=227;
      k_EControllerActionOrigin_Switch_ProGyro_Roll=228;
      k_EControllerActionOrigin_Switch_RightGyro_Move=229;
      k_EControllerActionOrigin_Switch_RightGyro_Pitch=230;
      k_EControllerActionOrigin_Switch_RightGyro_Yaw=231;
      k_EControllerActionOrigin_Switch_RightGyro_Roll=232;
      k_EControllerActionOrigin_Switch_LeftGyro_Move=233;
      k_EControllerActionOrigin_Switch_LeftGyro_Pitch=234;
      k_EControllerActionOrigin_Switch_LeftGyro_Yaw=235;
      k_EControllerActionOrigin_Switch_LeftGyro_Roll=236;
      k_EControllerActionOrigin_Switch_LeftGrip_Lower=237;
      k_EControllerActionOrigin_Switch_LeftGrip_Upper=238;
      k_EControllerActionOrigin_Switch_RightGrip_Lower=239;
      k_EControllerActionOrigin_Switch_RightGrip_Upper=240;
      k_EControllerActionOrigin_PS4_DPad_Move=241;
      k_EControllerActionOrigin_XBoxOne_DPad_Move=242;
      k_EControllerActionOrigin_XBox360_DPad_Move=243;
      k_EControllerActionOrigin_Switch_DPad_Move=244;
      k_EControllerActionOrigin_PS5_X=245;
      k_EControllerActionOrigin_PS5_Circle=246;
      k_EControllerActionOrigin_PS5_Triangle=247;
      k_EControllerActionOrigin_PS5_Square=248;
      k_EControllerActionOrigin_PS5_LeftBumper=249;
      k_EControllerActionOrigin_PS5_RightBumper=250;
      k_EControllerActionOrigin_PS5_Option=251;
      k_EControllerActionOrigin_PS5_Create=252;
      k_EControllerActionOrigin_PS5_Mute=253;
      k_EControllerActionOrigin_PS5_LeftPad_Touch=254;
      k_EControllerActionOrigin_PS5_LeftPad_Swipe=255;
      k_EControllerActionOrigin_PS5_LeftPad_Click=256;
      k_EControllerActionOrigin_PS5_LeftPad_DPadNorth=257;
      k_EControllerActionOrigin_PS5_LeftPad_DPadSouth=258;
      k_EControllerActionOrigin_PS5_LeftPad_DPadWest=259;
      k_EControllerActionOrigin_PS5_LeftPad_DPadEast=260;
      k_EControllerActionOrigin_PS5_RightPad_Touch=261;
      k_EControllerActionOrigin_PS5_RightPad_Swipe=262;
      k_EControllerActionOrigin_PS5_RightPad_Click=263;
      k_EControllerActionOrigin_PS5_RightPad_DPadNorth=264;
      k_EControllerActionOrigin_PS5_RightPad_DPadSouth=265;
      k_EControllerActionOrigin_PS5_RightPad_DPadWest=266;
      k_EControllerActionOrigin_PS5_RightPad_DPadEast=267;
      k_EControllerActionOrigin_PS5_CenterPad_Touch=268;
      k_EControllerActionOrigin_PS5_CenterPad_Swipe=269;
      k_EControllerActionOrigin_PS5_CenterPad_Click=270;
      k_EControllerActionOrigin_PS5_CenterPad_DPadNorth=271;
      k_EControllerActionOrigin_PS5_CenterPad_DPadSouth=272;
      k_EControllerActionOrigin_PS5_CenterPad_DPadWest=273;
      k_EControllerActionOrigin_PS5_CenterPad_DPadEast=274;
      k_EControllerActionOrigin_PS5_LeftTrigger_Pull=275;
      k_EControllerActionOrigin_PS5_LeftTrigger_Click=276;
      k_EControllerActionOrigin_PS5_RightTrigger_Pull=277;
      k_EControllerActionOrigin_PS5_RightTrigger_Click=278;
      k_EControllerActionOrigin_PS5_LeftStick_Move=279;
      k_EControllerActionOrigin_PS5_LeftStick_Click=280;
      k_EControllerActionOrigin_PS5_LeftStick_DPadNorth=281;
      k_EControllerActionOrigin_PS5_LeftStick_DPadSouth=282;
      k_EControllerActionOrigin_PS5_LeftStick_DPadWest=283;
      k_EControllerActionOrigin_PS5_LeftStick_DPadEast=284;
      k_EControllerActionOrigin_PS5_RightStick_Move=285;
      k_EControllerActionOrigin_PS5_RightStick_Click=286;
      k_EControllerActionOrigin_PS5_RightStick_DPadNorth=287;
      k_EControllerActionOrigin_PS5_RightStick_DPadSouth=288;
      k_EControllerActionOrigin_PS5_RightStick_DPadWest=289;
      k_EControllerActionOrigin_PS5_RightStick_DPadEast=290;
      k_EControllerActionOrigin_PS5_DPad_Move=291;
      k_EControllerActionOrigin_PS5_DPad_North=292;
      k_EControllerActionOrigin_PS5_DPad_South=293;
      k_EControllerActionOrigin_PS5_DPad_West=294;
      k_EControllerActionOrigin_PS5_DPad_East=295;
      k_EControllerActionOrigin_PS5_Gyro_Move=296;
      k_EControllerActionOrigin_PS5_Gyro_Pitch=297;
      k_EControllerActionOrigin_PS5_Gyro_Yaw=298;
      k_EControllerActionOrigin_PS5_Gyro_Roll=299;
      k_EControllerActionOrigin_XBoxOne_LeftGrip_Lower=300;
      k_EControllerActionOrigin_XBoxOne_LeftGrip_Upper=301;
      k_EControllerActionOrigin_XBoxOne_RightGrip_Lower=302;
      k_EControllerActionOrigin_XBoxOne_RightGrip_Upper=303;
      k_EControllerActionOrigin_XBoxOne_Share=304;
      k_EControllerActionOrigin_SteamDeck_A=305;
      k_EControllerActionOrigin_SteamDeck_B=306;
      k_EControllerActionOrigin_SteamDeck_X=307;
      k_EControllerActionOrigin_SteamDeck_Y=308;
      k_EControllerActionOrigin_SteamDeck_L1=309;
      k_EControllerActionOrigin_SteamDeck_R1=310;
      k_EControllerActionOrigin_SteamDeck_Menu=311;
      k_EControllerActionOrigin_SteamDeck_View=312;
      k_EControllerActionOrigin_SteamDeck_LeftPad_Touch=313;
      k_EControllerActionOrigin_SteamDeck_LeftPad_Swipe=314;
      k_EControllerActionOrigin_SteamDeck_LeftPad_Click=315;
      k_EControllerActionOrigin_SteamDeck_LeftPad_DPadNorth=316;
      k_EControllerActionOrigin_SteamDeck_LeftPad_DPadSouth=317;
      k_EControllerActionOrigin_SteamDeck_LeftPad_DPadWest=318;
      k_EControllerActionOrigin_SteamDeck_LeftPad_DPadEast=319;
      k_EControllerActionOrigin_SteamDeck_RightPad_Touch=320;
      k_EControllerActionOrigin_SteamDeck_RightPad_Swipe=321;
      k_EControllerActionOrigin_SteamDeck_RightPad_Click=322;
      k_EControllerActionOrigin_SteamDeck_RightPad_DPadNorth=323;
      k_EControllerActionOrigin_SteamDeck_RightPad_DPadSouth=324;
      k_EControllerActionOrigin_SteamDeck_RightPad_DPadWest=325;
      k_EControllerActionOrigin_SteamDeck_RightPad_DPadEast=326;
      k_EControllerActionOrigin_SteamDeck_L2_SoftPull=327;
      k_EControllerActionOrigin_SteamDeck_L2=328;
      k_EControllerActionOrigin_SteamDeck_R2_SoftPull=329;
      k_EControllerActionOrigin_SteamDeck_R2=330;
      k_EControllerActionOrigin_SteamDeck_LeftStick_Move=331;
      k_EControllerActionOrigin_SteamDeck_L3=332;
      k_EControllerActionOrigin_SteamDeck_LeftStick_DPadNorth=333;
      k_EControllerActionOrigin_SteamDeck_LeftStick_DPadSouth=334;
      k_EControllerActionOrigin_SteamDeck_LeftStick_DPadWest=335;
      k_EControllerActionOrigin_SteamDeck_LeftStick_DPadEast=336;
      k_EControllerActionOrigin_SteamDeck_LeftStick_Touch=337;
      k_EControllerActionOrigin_SteamDeck_RightStick_Move=338;
      k_EControllerActionOrigin_SteamDeck_R3=339;
      k_EControllerActionOrigin_SteamDeck_RightStick_DPadNorth=340;
      k_EControllerActionOrigin_SteamDeck_RightStick_DPadSouth=341;
      k_EControllerActionOrigin_SteamDeck_RightStick_DPadWest=342;
      k_EControllerActionOrigin_SteamDeck_RightStick_DPadEast=343;
      k_EControllerActionOrigin_SteamDeck_RightStick_Touch=344;
      k_EControllerActionOrigin_SteamDeck_L4=345;
      k_EControllerActionOrigin_SteamDeck_R4=346;
      k_EControllerActionOrigin_SteamDeck_L5=347;
      k_EControllerActionOrigin_SteamDeck_R5=348;
      k_EControllerActionOrigin_SteamDeck_DPad_Move=349;
      k_EControllerActionOrigin_SteamDeck_DPad_North=350;
      k_EControllerActionOrigin_SteamDeck_DPad_South=351;
      k_EControllerActionOrigin_SteamDeck_DPad_West=352;
      k_EControllerActionOrigin_SteamDeck_DPad_East=353;
      k_EControllerActionOrigin_SteamDeck_Gyro_Move=354;
      k_EControllerActionOrigin_SteamDeck_Gyro_Pitch=355;
      k_EControllerActionOrigin_SteamDeck_Gyro_Yaw=356;
      k_EControllerActionOrigin_SteamDeck_Gyro_Roll=357;
      k_EControllerActionOrigin_SteamDeck_Reserved1=358;
      k_EControllerActionOrigin_SteamDeck_Reserved2=359;
      k_EControllerActionOrigin_SteamDeck_Reserved3=360;
      k_EControllerActionOrigin_SteamDeck_Reserved4=361;
      k_EControllerActionOrigin_SteamDeck_Reserved5=362;
      k_EControllerActionOrigin_SteamDeck_Reserved6=363;
      k_EControllerActionOrigin_SteamDeck_Reserved7=364;
      k_EControllerActionOrigin_SteamDeck_Reserved8=365;
      k_EControllerActionOrigin_SteamDeck_Reserved9=366;
      k_EControllerActionOrigin_SteamDeck_Reserved10=367;
      k_EControllerActionOrigin_SteamDeck_Reserved11=368;
      k_EControllerActionOrigin_SteamDeck_Reserved12=369;
      k_EControllerActionOrigin_SteamDeck_Reserved13=370;
      k_EControllerActionOrigin_SteamDeck_Reserved14=371;
      k_EControllerActionOrigin_SteamDeck_Reserved15=372;
      k_EControllerActionOrigin_SteamDeck_Reserved16=373;
      k_EControllerActionOrigin_SteamDeck_Reserved17=374;
      k_EControllerActionOrigin_SteamDeck_Reserved18=375;
      k_EControllerActionOrigin_SteamDeck_Reserved19=376;
      k_EControllerActionOrigin_SteamDeck_Reserved20=377;
      k_EControllerActionOrigin_Switch_JoyConButton_N=378;
      k_EControllerActionOrigin_Switch_JoyConButton_E=379;
      k_EControllerActionOrigin_Switch_JoyConButton_S=380;
      k_EControllerActionOrigin_Switch_JoyConButton_W=381;
      k_EControllerActionOrigin_PS5_LeftGrip=382;
      k_EControllerActionOrigin_PS5_RightGrip=383;
      k_EControllerActionOrigin_PS5_LeftFn=384;
      k_EControllerActionOrigin_PS5_RightFn=385;
      k_EControllerActionOrigin_Horipad_M1=386;
      k_EControllerActionOrigin_Horipad_M2=387;
      k_EControllerActionOrigin_Horipad_L4=388;
      k_EControllerActionOrigin_Horipad_R4=389;
      k_EControllerActionOrigin_LenovoLegionGo_A=390;
      k_EControllerActionOrigin_LenovoLegionGo_B=391;
      k_EControllerActionOrigin_LenovoLegionGo_X=392;
      k_EControllerActionOrigin_LenovoLegionGo_Y=393;
      k_EControllerActionOrigin_LenovoLegionGo_LB=394;
      k_EControllerActionOrigin_LenovoLegionGo_RB=395;
      k_EControllerActionOrigin_LenovoLegionGo_Menu=396;
      k_EControllerActionOrigin_LenovoLegionGo_View=397;
      k_EControllerActionOrigin_LenovoLegionGo_LeftPad_Touch=398;
      k_EControllerActionOrigin_LenovoLegionGo_LeftPad_Swipe=399;
      k_EControllerActionOrigin_LenovoLegionGo_LeftPad_Click=400;
      k_EControllerActionOrigin_LenovoLegionGo_LeftPad_DPadNorth=401;
      k_EControllerActionOrigin_LenovoLegionGo_LeftPad_DPadSouth=402;
      k_EControllerActionOrigin_LenovoLegionGo_LeftPad_DPadWest=403;
      k_EControllerActionOrigin_LenovoLegionGo_LeftPad_DPadEast=404;
      k_EControllerActionOrigin_LenovoLegionGo_RightPad_Touch=405;
      k_EControllerActionOrigin_LenovoLegionGo_RightPad_Swipe=406;
      k_EControllerActionOrigin_LenovoLegionGo_RightPad_Click=407;
      k_EControllerActionOrigin_LenovoLegionGo_RightPad_DPadNorth=408;
      k_EControllerActionOrigin_LenovoLegionGo_RightPad_DPadSouth=409;
      k_EControllerActionOrigin_LenovoLegionGo_RightPad_DPadWest=410;
      k_EControllerActionOrigin_LenovoLegionGo_RightPad_DPadEast=411;
      k_EControllerActionOrigin_LenovoLegionGo_LT_SoftPull=412;
      k_EControllerActionOrigin_LenovoLegionGo_LT=413;
      k_EControllerActionOrigin_LenovoLegionGo_RT_SoftPull=414;
      k_EControllerActionOrigin_LenovoLegionGo_RT=415;
      k_EControllerActionOrigin_LenovoLegionGo_LeftStick_Move=416;
      k_EControllerActionOrigin_LenovoLegionGo_LS=417;
      k_EControllerActionOrigin_LenovoLegionGo_LeftStick_DPadNorth=418;
      k_EControllerActionOrigin_LenovoLegionGo_LeftStick_DPadSouth=419;
      k_EControllerActionOrigin_LenovoLegionGo_LeftStick_DPadWest=420;
      k_EControllerActionOrigin_LenovoLegionGo_LeftStick_DPadEast=421;
      k_EControllerActionOrigin_LenovoLegionGo_RightStick_Move=422;
      k_EControllerActionOrigin_LenovoLegionGo_RS=423;
      k_EControllerActionOrigin_LenovoLegionGo_RightStick_DPadNorth=424;
      k_EControllerActionOrigin_LenovoLegionGo_RightStick_DPadSouth=425;
      k_EControllerActionOrigin_LenovoLegionGo_RightStick_DPadWest=426;
      k_EControllerActionOrigin_LenovoLegionGo_RightStick_DPadEast=427;
      k_EControllerActionOrigin_LenovoLegionGo_Y1=428;
      k_EControllerActionOrigin_LenovoLegionGo_Y2=429;
      k_EControllerActionOrigin_LenovoLegionGo_DPad_Move=430;
      k_EControllerActionOrigin_LenovoLegionGo_DPad_North=431;
      k_EControllerActionOrigin_LenovoLegionGo_DPad_South=432;
      k_EControllerActionOrigin_LenovoLegionGo_DPad_West=433;
      k_EControllerActionOrigin_LenovoLegionGo_DPad_East=434;
      k_EControllerActionOrigin_LenovoLegionGo_Gyro_Move=435;
      k_EControllerActionOrigin_LenovoLegionGo_Gyro_Pitch=436;
      k_EControllerActionOrigin_LenovoLegionGo_Gyro_Yaw=437;
      k_EControllerActionOrigin_LenovoLegionGo_Gyro_Roll=438;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved1=439;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved2=440;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved3=441;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved4=442;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved5=443;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved6=444;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved7=445;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved8=446;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved9=447;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved10=448;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved11=449;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved12=450;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved13=451;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved14=452;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved15=453;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved16=454;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved17=455;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved18=456;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved19=457;
      k_EControllerActionOrigin_LenovoLegionGo_Reserved20=458;
      k_EControllerActionOrigin_Generic_L4=459;
      k_EControllerActionOrigin_Generic_R4=460;
      k_EControllerActionOrigin_Generic_L5=461;
      k_EControllerActionOrigin_Generic_R5=462;
      k_EControllerActionOrigin_Generic_PL=463;
      k_EControllerActionOrigin_Generic_PR=464;
      k_EControllerActionOrigin_Generic_C=465;
      k_EControllerActionOrigin_Generic_Z=466;
      k_EControllerActionOrigin_Generic_MISC1=467;
      k_EControllerActionOrigin_Generic_MISC2=468;
      k_EControllerActionOrigin_Generic_MISC3=469;
      k_EControllerActionOrigin_Generic_MISC4=470;
      k_EControllerActionOrigin_Generic_MISC5=471;
      k_EControllerActionOrigin_Generic_MISC6=472;
      k_EControllerActionOrigin_Generic_MISC7=473;
      k_EControllerActionOrigin_Generic_MISC8=474;
      k_EControllerActionOrigin_Count=475;
      k_EControllerActionOrigin_MaximumPossibleValue=32767;

type { TESteamControllerLEDFlag }
     PPESteamControllerLEDFlag=^PESteamControllerLEDFlag;
     PESteamControllerLEDFlag=^TESteamControllerLEDFlag;
     TESteamControllerLEDFlag=TSteamInt32;

const k_ESteamControllerLEDFlag_SetColor=0;
      k_ESteamControllerLEDFlag_RestoreUserDefault=1;

type { TEUGCMatchingUGCType }
     PPEUGCMatchingUGCType=^PEUGCMatchingUGCType;
     PEUGCMatchingUGCType=^TEUGCMatchingUGCType;
     TEUGCMatchingUGCType=TSteamInt32;

const k_EUGCMatchingUGCType_Items=0;
      k_EUGCMatchingUGCType_Items_Mtx=1;
      k_EUGCMatchingUGCType_Items_ReadyToUse=2;
      k_EUGCMatchingUGCType_Collections=3;
      k_EUGCMatchingUGCType_Artwork=4;
      k_EUGCMatchingUGCType_Videos=5;
      k_EUGCMatchingUGCType_Screenshots=6;
      k_EUGCMatchingUGCType_AllGuides=7;
      k_EUGCMatchingUGCType_WebGuides=8;
      k_EUGCMatchingUGCType_IntegratedGuides=9;
      k_EUGCMatchingUGCType_UsableInGame=10;
      k_EUGCMatchingUGCType_ControllerBindings=11;
      k_EUGCMatchingUGCType_GameManagedItems=12;
      k_EUGCMatchingUGCType_All=-1;

type { TEUserUGCList }
     PPEUserUGCList=^PEUserUGCList;
     PEUserUGCList=^TEUserUGCList;
     TEUserUGCList=TSteamInt32;

const k_EUserUGCList_Published=0;
      k_EUserUGCList_VotedOn=1;
      k_EUserUGCList_VotedUp=2;
      k_EUserUGCList_VotedDown=3;
      k_EUserUGCList_WillVoteLater=4;
      k_EUserUGCList_Favorited=5;
      k_EUserUGCList_Subscribed=6;
      k_EUserUGCList_UsedOrPlayed=7;
      k_EUserUGCList_Followed=8;

type { TEUserUGCListSortOrder }
     PPEUserUGCListSortOrder=^PEUserUGCListSortOrder;
     PEUserUGCListSortOrder=^TEUserUGCListSortOrder;
     TEUserUGCListSortOrder=TSteamInt32;

const k_EUserUGCListSortOrder_CreationOrderDesc=0;
      k_EUserUGCListSortOrder_CreationOrderAsc=1;
      k_EUserUGCListSortOrder_TitleAsc=2;
      k_EUserUGCListSortOrder_LastUpdatedDesc=3;
      k_EUserUGCListSortOrder_SubscriptionDateDesc=4;
      k_EUserUGCListSortOrder_VoteScoreDesc=5;
      k_EUserUGCListSortOrder_ForModeration=6;

type { TEUGCQuery }
     PPEUGCQuery=^PEUGCQuery;
     PEUGCQuery=^TEUGCQuery;
     TEUGCQuery=TSteamInt32;

const k_EUGCQuery_RankedByVote=0;
      k_EUGCQuery_RankedByPublicationDate=1;
      k_EUGCQuery_AcceptedForGameRankedByAcceptanceDate=2;
      k_EUGCQuery_RankedByTrend=3;
      k_EUGCQuery_FavoritedByFriendsRankedByPublicationDate=4;
      k_EUGCQuery_CreatedByFriendsRankedByPublicationDate=5;
      k_EUGCQuery_RankedByNumTimesReported=6;
      k_EUGCQuery_CreatedByFollowedUsersRankedByPublicationDate=7;
      k_EUGCQuery_NotYetRated=8;
      k_EUGCQuery_RankedByTotalVotesAsc=9;
      k_EUGCQuery_RankedByVotesUp=10;
      k_EUGCQuery_RankedByTextSearch=11;
      k_EUGCQuery_RankedByTotalUniqueSubscriptions=12;
      k_EUGCQuery_RankedByPlaytimeTrend=13;
      k_EUGCQuery_RankedByTotalPlaytime=14;
      k_EUGCQuery_RankedByAveragePlaytimeTrend=15;
      k_EUGCQuery_RankedByLifetimeAveragePlaytime=16;
      k_EUGCQuery_RankedByPlaytimeSessionsTrend=17;
      k_EUGCQuery_RankedByLifetimePlaytimeSessions=18;
      k_EUGCQuery_RankedByLastUpdatedDate=19;

type { TEItemUpdateStatus }
     PPEItemUpdateStatus=^PEItemUpdateStatus;
     PEItemUpdateStatus=^TEItemUpdateStatus;
     TEItemUpdateStatus=TSteamInt32;

const k_EItemUpdateStatusInvalid=0;
      k_EItemUpdateStatusPreparingConfig=1;
      k_EItemUpdateStatusPreparingContent=2;
      k_EItemUpdateStatusUploadingContent=3;
      k_EItemUpdateStatusUploadingPreviewFile=4;
      k_EItemUpdateStatusCommittingChanges=5;

type { TEItemState }
     PPEItemState=^PEItemState;
     PEItemState=^TEItemState;
     TEItemState=TSteamInt32;

const k_EItemStateNone=0;
      k_EItemStateSubscribed=1;
      k_EItemStateLegacyItem=2;
      k_EItemStateInstalled=4;
      k_EItemStateNeedsUpdate=8;
      k_EItemStateDownloading=16;
      k_EItemStateDownloadPending=32;
      k_EItemStateDisabledLocally=64;

type { TEItemStatistic }
     PPEItemStatistic=^PEItemStatistic;
     PEItemStatistic=^TEItemStatistic;
     TEItemStatistic=TSteamInt32;

const k_EItemStatistic_NumSubscriptions=0;
      k_EItemStatistic_NumFavorites=1;
      k_EItemStatistic_NumFollowers=2;
      k_EItemStatistic_NumUniqueSubscriptions=3;
      k_EItemStatistic_NumUniqueFavorites=4;
      k_EItemStatistic_NumUniqueFollowers=5;
      k_EItemStatistic_NumUniqueWebsiteViews=6;
      k_EItemStatistic_ReportScore=7;
      k_EItemStatistic_NumSecondsPlayed=8;
      k_EItemStatistic_NumPlaytimeSessions=9;
      k_EItemStatistic_NumComments=10;
      k_EItemStatistic_NumSecondsPlayedDuringTimePeriod=11;
      k_EItemStatistic_NumPlaytimeSessionsDuringTimePeriod=12;

type { TEItemPreviewType }
     PPEItemPreviewType=^PEItemPreviewType;
     PEItemPreviewType=^TEItemPreviewType;
     TEItemPreviewType=TSteamInt32;

const k_EItemPreviewType_Image=0;
      k_EItemPreviewType_YouTubeVideo=1;
      k_EItemPreviewType_Sketchfab=2;
      k_EItemPreviewType_EnvironmentMap_HorizontalCross=3;
      k_EItemPreviewType_EnvironmentMap_LatLong=4;
      k_EItemPreviewType_Clip=5;
      k_EItemPreviewType_ReservedMax=255;

type { TEUGCContentDescriptorID }
     PPEUGCContentDescriptorID=^PEUGCContentDescriptorID;
     PEUGCContentDescriptorID=^TEUGCContentDescriptorID;
     TEUGCContentDescriptorID=TSteamInt32;

const k_EUGCContentDescriptor_NudityOrSexualContent=1;
      k_EUGCContentDescriptor_FrequentViolenceOrGore=2;
      k_EUGCContentDescriptor_AdultOnlySexualContent=3;
      k_EUGCContentDescriptor_GratuitousSexualContent=4;
      k_EUGCContentDescriptor_AnyMatureContent=5;

type { TESteamItemFlags }
     PPESteamItemFlags=^PESteamItemFlags;
     PESteamItemFlags=^TESteamItemFlags;
     TESteamItemFlags=TSteamInt32;

const k_ESteamItemNoTrade=1;
      k_ESteamItemRemoved=256;
      k_ESteamItemConsumed=512;

type { TETimelineGameMode }
     PPETimelineGameMode=^PETimelineGameMode;
     PETimelineGameMode=^TETimelineGameMode;
     TETimelineGameMode=TSteamInt32;

const k_ETimelineGameMode_Invalid=0;
      k_ETimelineGameMode_Playing=1;
      k_ETimelineGameMode_Staging=2;
      k_ETimelineGameMode_Menus=3;
      k_ETimelineGameMode_LoadingScreen=4;
      k_ETimelineGameMode_Max=5;

type { TETimelineEventClipPriority }
     PPETimelineEventClipPriority=^PETimelineEventClipPriority;
     PETimelineEventClipPriority=^TETimelineEventClipPriority;
     TETimelineEventClipPriority=TSteamInt32;

const k_ETimelineEventClipPriority_Invalid=0;
      k_ETimelineEventClipPriority_None=1;
      k_ETimelineEventClipPriority_Standard=2;
      k_ETimelineEventClipPriority_Featured=3;

type { TEParentalFeature }
     PPEParentalFeature=^PEParentalFeature;
     PEParentalFeature=^TEParentalFeature;
     TEParentalFeature=TSteamInt32;

const k_EFeatureInvalid=0;
      k_EFeatureStore=1;
      k_EFeatureCommunity=2;
      k_EFeatureProfile=3;
      k_EFeatureFriends=4;
      k_EFeatureNews=5;
      k_EFeatureTrading=6;
      k_EFeatureSettings=7;
      k_EFeatureConsole=8;
      k_EFeatureBrowser=9;
      k_EFeatureParentalSetup=10;
      k_EFeatureLibrary=11;
      k_EFeatureTest=12;
      k_EFeatureSiteLicense=13;
      k_EFeatureKioskMode_Deprecated=14;
      k_EFeatureBlockAlways=15;
      k_EFeatureDesktop=16;
      k_EFeatureMax=17;

type { TESteamDeviceFormFactor }
     PPESteamDeviceFormFactor=^PESteamDeviceFormFactor;
     PESteamDeviceFormFactor=^TESteamDeviceFormFactor;
     TESteamDeviceFormFactor=TSteamInt32;

const k_ESteamDeviceFormFactorUnknown=0;
      k_ESteamDeviceFormFactorPhone=1;
      k_ESteamDeviceFormFactorTablet=2;
      k_ESteamDeviceFormFactorComputer=3;
      k_ESteamDeviceFormFactorTV=4;
      k_ESteamDeviceFormFactorVRHeadset=5;

type { TERemotePlayInputType }
     PPERemotePlayInputType=^PERemotePlayInputType;
     PERemotePlayInputType=^TERemotePlayInputType;
     TERemotePlayInputType=TSteamInt32;

const k_ERemotePlayInputUnknown=0;
      k_ERemotePlayInputMouseMotion=1;
      k_ERemotePlayInputMouseButtonDown=2;
      k_ERemotePlayInputMouseButtonUp=3;
      k_ERemotePlayInputMouseWheel=4;
      k_ERemotePlayInputKeyDown=5;
      k_ERemotePlayInputKeyUp=6;

type { TERemotePlayMouseButton }
     PPERemotePlayMouseButton=^PERemotePlayMouseButton;
     PERemotePlayMouseButton=^TERemotePlayMouseButton;
     TERemotePlayMouseButton=TSteamInt32;

const k_ERemotePlayMouseButtonLeft=1;
      k_ERemotePlayMouseButtonRight=2;
      k_ERemotePlayMouseButtonMiddle=16;
      k_ERemotePlayMouseButtonX1=32;
      k_ERemotePlayMouseButtonX2=64;

type { TERemotePlayMouseWheelDirection }
     PPERemotePlayMouseWheelDirection=^PERemotePlayMouseWheelDirection;
     PERemotePlayMouseWheelDirection=^TERemotePlayMouseWheelDirection;
     TERemotePlayMouseWheelDirection=TSteamInt32;

const k_ERemotePlayMouseWheelUp=1;
      k_ERemotePlayMouseWheelDown=2;
      k_ERemotePlayMouseWheelLeft=3;
      k_ERemotePlayMouseWheelRight=4;

type { TERemotePlayScancode }
     PPERemotePlayScancode=^PERemotePlayScancode;
     PERemotePlayScancode=^TERemotePlayScancode;
     TERemotePlayScancode=TSteamInt32;

const k_ERemotePlayScancodeUnknown=0;
      k_ERemotePlayScancodeA=4;
      k_ERemotePlayScancodeB=5;
      k_ERemotePlayScancodeC=6;
      k_ERemotePlayScancodeD=7;
      k_ERemotePlayScancodeE=8;
      k_ERemotePlayScancodeF=9;
      k_ERemotePlayScancodeG=10;
      k_ERemotePlayScancodeH=11;
      k_ERemotePlayScancodeI=12;
      k_ERemotePlayScancodeJ=13;
      k_ERemotePlayScancodeK=14;
      k_ERemotePlayScancodeL=15;
      k_ERemotePlayScancodeM=16;
      k_ERemotePlayScancodeN=17;
      k_ERemotePlayScancodeO=18;
      k_ERemotePlayScancodeP=19;
      k_ERemotePlayScancodeQ=20;
      k_ERemotePlayScancodeR=21;
      k_ERemotePlayScancodeS=22;
      k_ERemotePlayScancodeT=23;
      k_ERemotePlayScancodeU=24;
      k_ERemotePlayScancodeV=25;
      k_ERemotePlayScancodeW=26;
      k_ERemotePlayScancodeX=27;
      k_ERemotePlayScancodeY=28;
      k_ERemotePlayScancodeZ=29;
      k_ERemotePlayScancode1=30;
      k_ERemotePlayScancode2=31;
      k_ERemotePlayScancode3=32;
      k_ERemotePlayScancode4=33;
      k_ERemotePlayScancode5=34;
      k_ERemotePlayScancode6=35;
      k_ERemotePlayScancode7=36;
      k_ERemotePlayScancode8=37;
      k_ERemotePlayScancode9=38;
      k_ERemotePlayScancode0=39;
      k_ERemotePlayScancodeReturn=40;
      k_ERemotePlayScancodeEscape=41;
      k_ERemotePlayScancodeBackspace=42;
      k_ERemotePlayScancodeTab=43;
      k_ERemotePlayScancodeSpace=44;
      k_ERemotePlayScancodeMinus=45;
      k_ERemotePlayScancodeEquals=46;
      k_ERemotePlayScancodeLeftBracket=47;
      k_ERemotePlayScancodeRightBracket=48;
      k_ERemotePlayScancodeBackslash=49;
      k_ERemotePlayScancodeSemicolon=51;
      k_ERemotePlayScancodeApostrophe=52;
      k_ERemotePlayScancodeGrave=53;
      k_ERemotePlayScancodeComma=54;
      k_ERemotePlayScancodePeriod=55;
      k_ERemotePlayScancodeSlash=56;
      k_ERemotePlayScancodeCapsLock=57;
      k_ERemotePlayScancodeF1=58;
      k_ERemotePlayScancodeF2=59;
      k_ERemotePlayScancodeF3=60;
      k_ERemotePlayScancodeF4=61;
      k_ERemotePlayScancodeF5=62;
      k_ERemotePlayScancodeF6=63;
      k_ERemotePlayScancodeF7=64;
      k_ERemotePlayScancodeF8=65;
      k_ERemotePlayScancodeF9=66;
      k_ERemotePlayScancodeF10=67;
      k_ERemotePlayScancodeF11=68;
      k_ERemotePlayScancodeF12=69;
      k_ERemotePlayScancodeInsert=73;
      k_ERemotePlayScancodeHome=74;
      k_ERemotePlayScancodePageUp=75;
      k_ERemotePlayScancodeDelete=76;
      k_ERemotePlayScancodeEnd=77;
      k_ERemotePlayScancodePageDown=78;
      k_ERemotePlayScancodeRight=79;
      k_ERemotePlayScancodeLeft=80;
      k_ERemotePlayScancodeDown=81;
      k_ERemotePlayScancodeUp=82;
      k_ERemotePlayScancodeKeypadDivide=84;
      k_ERemotePlayScancodeKeypadMultiply=85;
      k_ERemotePlayScancodeKeypadMinus=86;
      k_ERemotePlayScancodeKeypadPlus=87;
      k_ERemotePlayScancodeKeypadEnter=88;
      k_ERemotePlayScancodeKeypad1=89;
      k_ERemotePlayScancodeKeypad2=90;
      k_ERemotePlayScancodeKeypad3=91;
      k_ERemotePlayScancodeKeypad4=92;
      k_ERemotePlayScancodeKeypad5=93;
      k_ERemotePlayScancodeKeypad6=94;
      k_ERemotePlayScancodeKeypad7=95;
      k_ERemotePlayScancodeKeypad8=96;
      k_ERemotePlayScancodeKeypad9=97;
      k_ERemotePlayScancodeKeypad0=98;
      k_ERemotePlayScancodeKeypadPeriod=99;
      k_ERemotePlayScancodeLeftControl=224;
      k_ERemotePlayScancodeLeftShift=225;
      k_ERemotePlayScancodeLeftAlt=226;
      k_ERemotePlayScancodeLeftGUI=227;
      k_ERemotePlayScancodeRightControl=228;
      k_ERemotePlayScancodeRightShift=229;
      k_ERemotePlayScancodeRightALT=230;
      k_ERemotePlayScancodeRightGUI=231;

type { TERemotePlayKeyModifier }
     PPERemotePlayKeyModifier=^PERemotePlayKeyModifier;
     PERemotePlayKeyModifier=^TERemotePlayKeyModifier;
     TERemotePlayKeyModifier=TSteamInt32;

const k_ERemotePlayKeyModifierNone=0;
      k_ERemotePlayKeyModifierLeftShift=1;
      k_ERemotePlayKeyModifierRightShift=2;
      k_ERemotePlayKeyModifierLeftControl=64;
      k_ERemotePlayKeyModifierRightControl=128;
      k_ERemotePlayKeyModifierLeftAlt=256;
      k_ERemotePlayKeyModifierRightAlt=512;
      k_ERemotePlayKeyModifierLeftGUI=1024;
      k_ERemotePlayKeyModifierRightGUI=2048;
      k_ERemotePlayKeyModifierNumLock=4096;
      k_ERemotePlayKeyModifierCapsLock=8192;
      k_ERemotePlayKeyModifierMask=65535;

type { TESteamNetworkingAvailability }
     PPESteamNetworkingAvailability=^PESteamNetworkingAvailability;
     PESteamNetworkingAvailability=^TESteamNetworkingAvailability;
     TESteamNetworkingAvailability=TSteamInt32;

const k_ESteamNetworkingAvailability_CannotTry=-102;
      k_ESteamNetworkingAvailability_Failed=-101;
      k_ESteamNetworkingAvailability_Previously=-100;
      k_ESteamNetworkingAvailability_Retrying=-10;
      k_ESteamNetworkingAvailability_NeverTried=1;
      k_ESteamNetworkingAvailability_Waiting=2;
      k_ESteamNetworkingAvailability_Attempting=3;
      k_ESteamNetworkingAvailability_Current=100;
      k_ESteamNetworkingAvailability_Unknown=0;
      k_ESteamNetworkingAvailability__Force32bit=2147483647;

type { TESteamNetworkingIdentityType }
     PPESteamNetworkingIdentityType=^PESteamNetworkingIdentityType;
     PESteamNetworkingIdentityType=^TESteamNetworkingIdentityType;
     TESteamNetworkingIdentityType=TSteamInt32;

const k_ESteamNetworkingIdentityType_Invalid=0;
      k_ESteamNetworkingIdentityType_SteamID=16;
      k_ESteamNetworkingIdentityType_XboxPairwiseID=17;
      k_ESteamNetworkingIdentityType_SonyPSN=18;
      k_ESteamNetworkingIdentityType_IPAddress=1;
      k_ESteamNetworkingIdentityType_GenericString=2;
      k_ESteamNetworkingIdentityType_GenericBytes=3;
      k_ESteamNetworkingIdentityType_UnknownType=4;
      k_ESteamNetworkingIdentityType__Force32bit=2147483647;

type { TESteamNetworkingFakeIPType }
     PPESteamNetworkingFakeIPType=^PESteamNetworkingFakeIPType;
     PESteamNetworkingFakeIPType=^TESteamNetworkingFakeIPType;
     TESteamNetworkingFakeIPType=TSteamInt32;

const k_ESteamNetworkingFakeIPType_Invalid=0;
      k_ESteamNetworkingFakeIPType_NotFake=1;
      k_ESteamNetworkingFakeIPType_GlobalIPv4=2;
      k_ESteamNetworkingFakeIPType_LocalIPv4=3;
      k_ESteamNetworkingFakeIPType__Force32Bit=2147483647;

type { TESteamNetworkingConnectionState }
     PPESteamNetworkingConnectionState=^PESteamNetworkingConnectionState;
     PESteamNetworkingConnectionState=^TESteamNetworkingConnectionState;
     TESteamNetworkingConnectionState=TSteamInt32;

const k_ESteamNetworkingConnectionState_None=0;
      k_ESteamNetworkingConnectionState_Connecting=1;
      k_ESteamNetworkingConnectionState_FindingRoute=2;
      k_ESteamNetworkingConnectionState_Connected=3;
      k_ESteamNetworkingConnectionState_ClosedByPeer=4;
      k_ESteamNetworkingConnectionState_ProblemDetectedLocally=5;
      k_ESteamNetworkingConnectionState_FinWait=-1;
      k_ESteamNetworkingConnectionState_Linger=-2;
      k_ESteamNetworkingConnectionState_Dead=-3;
      k_ESteamNetworkingConnectionState__Force32Bit=2147483647;

type { TESteamNetConnectionEnd }
     PPESteamNetConnectionEnd=^PESteamNetConnectionEnd;
     PESteamNetConnectionEnd=^TESteamNetConnectionEnd;
     TESteamNetConnectionEnd=TSteamInt32;

const k_ESteamNetConnectionEnd_Invalid=0;
      k_ESteamNetConnectionEnd_App_Min=1000;
      k_ESteamNetConnectionEnd_App_Generic=1000;
      k_ESteamNetConnectionEnd_App_Max=1999;
      k_ESteamNetConnectionEnd_AppException_Min=2000;
      k_ESteamNetConnectionEnd_AppException_Generic=2000;
      k_ESteamNetConnectionEnd_AppException_Max=2999;
      k_ESteamNetConnectionEnd_Local_Min=3000;
      k_ESteamNetConnectionEnd_Local_OfflineMode=3001;
      k_ESteamNetConnectionEnd_Local_ManyRelayConnectivity=3002;
      k_ESteamNetConnectionEnd_Local_HostedServerPrimaryRelay=3003;
      k_ESteamNetConnectionEnd_Local_NetworkConfig=3004;
      k_ESteamNetConnectionEnd_Local_Rights=3005;
      k_ESteamNetConnectionEnd_Local_P2P_ICE_NoPublicAddresses=3006;
      k_ESteamNetConnectionEnd_Local_Max=3999;
      k_ESteamNetConnectionEnd_Remote_Min=4000;
      k_ESteamNetConnectionEnd_Remote_Timeout=4001;
      k_ESteamNetConnectionEnd_Remote_BadCrypt=4002;
      k_ESteamNetConnectionEnd_Remote_BadCert=4003;
      k_ESteamNetConnectionEnd_Remote_BadProtocolVersion=4006;
      k_ESteamNetConnectionEnd_Remote_P2P_ICE_NoPublicAddresses=4007;
      k_ESteamNetConnectionEnd_Remote_Max=4999;
      k_ESteamNetConnectionEnd_Misc_Min=5000;
      k_ESteamNetConnectionEnd_Misc_Generic=5001;
      k_ESteamNetConnectionEnd_Misc_InternalError=5002;
      k_ESteamNetConnectionEnd_Misc_Timeout=5003;
      k_ESteamNetConnectionEnd_Misc_SteamConnectivity=5005;
      k_ESteamNetConnectionEnd_Misc_NoRelaySessionsToClient=5006;
      k_ESteamNetConnectionEnd_Misc_P2P_Rendezvous=5008;
      k_ESteamNetConnectionEnd_Misc_P2P_NAT_Firewall=5009;
      k_ESteamNetConnectionEnd_Misc_PeerSentNoConnection=5010;
      k_ESteamNetConnectionEnd_Misc_Max=5999;
      k_ESteamNetConnectionEnd__Force32Bit=2147483647;

type { TESteamNetworkingConfigScope }
     PPESteamNetworkingConfigScope=^PESteamNetworkingConfigScope;
     PESteamNetworkingConfigScope=^TESteamNetworkingConfigScope;
     TESteamNetworkingConfigScope=TSteamInt32;

const k_ESteamNetworkingConfig_Global=1;
      k_ESteamNetworkingConfig_SocketsInterface=2;
      k_ESteamNetworkingConfig_ListenSocket=3;
      k_ESteamNetworkingConfig_Connection=4;
      k_ESteamNetworkingConfigScope__Force32Bit=2147483647;

type { TESteamNetworkingConfigDataType }
     PPESteamNetworkingConfigDataType=^PESteamNetworkingConfigDataType;
     PESteamNetworkingConfigDataType=^TESteamNetworkingConfigDataType;
     TESteamNetworkingConfigDataType=TSteamInt32;

const k_ESteamNetworkingConfig_Int32=1;
      k_ESteamNetworkingConfig_Int64=2;
      k_ESteamNetworkingConfig_Float=3;
      k_ESteamNetworkingConfig_String=4;
      k_ESteamNetworkingConfig_Ptr=5;
      k_ESteamNetworkingConfigDataType__Force32Bit=2147483647;

type { TESteamNetworkingConfigValue }
     PPESteamNetworkingConfigValue=^PESteamNetworkingConfigValue;
     PESteamNetworkingConfigValue=^TESteamNetworkingConfigValue;
     TESteamNetworkingConfigValue=TSteamInt32;

const k_ESteamNetworkingConfig_Invalid=0;
      k_ESteamNetworkingConfig_TimeoutInitial=24;
      k_ESteamNetworkingConfig_TimeoutConnected=25;
      k_ESteamNetworkingConfig_SendBufferSize=9;
      k_ESteamNetworkingConfig_RecvBufferSize=47;
      k_ESteamNetworkingConfig_RecvBufferMessages=48;
      k_ESteamNetworkingConfig_RecvMaxMessageSize=49;
      k_ESteamNetworkingConfig_RecvMaxSegmentsPerPacket=50;
      k_ESteamNetworkingConfig_ConnectionUserData=40;
      k_ESteamNetworkingConfig_SendRateMin=10;
      k_ESteamNetworkingConfig_SendRateMax=11;
      k_ESteamNetworkingConfig_NagleTime=12;
      k_ESteamNetworkingConfig_IP_AllowWithoutAuth=23;
      k_ESteamNetworkingConfig_IPLocalHost_AllowWithoutAuth=52;
      k_ESteamNetworkingConfig_MTU_PacketSize=32;
      k_ESteamNetworkingConfig_MTU_DataSize=33;
      k_ESteamNetworkingConfig_Unencrypted=34;
      k_ESteamNetworkingConfig_SymmetricConnect=37;
      k_ESteamNetworkingConfig_LocalVirtualPort=38;
      k_ESteamNetworkingConfig_DualWifi_Enable=39;
      k_ESteamNetworkingConfig_EnableDiagnosticsUI=46;
      k_ESteamNetworkingConfig_SendTimeSincePreviousPacket=59;
      k_ESteamNetworkingConfig_FakePacketLoss_Send=2;
      k_ESteamNetworkingConfig_FakePacketLoss_Recv=3;
      k_ESteamNetworkingConfig_FakePacketLag_Send=4;
      k_ESteamNetworkingConfig_FakePacketLag_Recv=5;
      k_ESteamNetworkingConfig_FakePacketJitter_Send_Avg=53;
      k_ESteamNetworkingConfig_FakePacketJitter_Send_Max=54;
      k_ESteamNetworkingConfig_FakePacketJitter_Send_Pct=55;
      k_ESteamNetworkingConfig_FakePacketJitter_Recv_Avg=56;
      k_ESteamNetworkingConfig_FakePacketJitter_Recv_Max=57;
      k_ESteamNetworkingConfig_FakePacketJitter_Recv_Pct=58;
      k_ESteamNetworkingConfig_FakePacketReorder_Send=6;
      k_ESteamNetworkingConfig_FakePacketReorder_Recv=7;
      k_ESteamNetworkingConfig_FakePacketReorder_Time=8;
      k_ESteamNetworkingConfig_FakePacketDup_Send=26;
      k_ESteamNetworkingConfig_FakePacketDup_Recv=27;
      k_ESteamNetworkingConfig_FakePacketDup_TimeMax=28;
      k_ESteamNetworkingConfig_PacketTraceMaxBytes=41;
      k_ESteamNetworkingConfig_FakeRateLimit_Send_Rate=42;
      k_ESteamNetworkingConfig_FakeRateLimit_Send_Burst=43;
      k_ESteamNetworkingConfig_FakeRateLimit_Recv_Rate=44;
      k_ESteamNetworkingConfig_FakeRateLimit_Recv_Burst=45;
      k_ESteamNetworkingConfig_OutOfOrderCorrectionWindowMicroseconds=51;
      k_ESteamNetworkingConfig_Callback_ConnectionStatusChanged=201;
      k_ESteamNetworkingConfig_Callback_AuthStatusChanged=202;
      k_ESteamNetworkingConfig_Callback_RelayNetworkStatusChanged=203;
      k_ESteamNetworkingConfig_Callback_MessagesSessionRequest=204;
      k_ESteamNetworkingConfig_Callback_MessagesSessionFailed=205;
      k_ESteamNetworkingConfig_Callback_CreateConnectionSignaling=206;
      k_ESteamNetworkingConfig_Callback_FakeIPResult=207;
      k_ESteamNetworkingConfig_P2P_STUN_ServerList=103;
      k_ESteamNetworkingConfig_P2P_Transport_ICE_Enable=104;
      k_ESteamNetworkingConfig_P2P_Transport_ICE_Penalty=105;
      k_ESteamNetworkingConfig_P2P_Transport_SDR_Penalty=106;
      k_ESteamNetworkingConfig_P2P_TURN_ServerList=107;
      k_ESteamNetworkingConfig_P2P_TURN_UserList=108;
      k_ESteamNetworkingConfig_P2P_TURN_PassList=109;
      k_ESteamNetworkingConfig_P2P_Transport_ICE_Implementation=110;
      k_ESteamNetworkingConfig_SDRClient_ConsecutitivePingTimeoutsFailInitial=19;
      k_ESteamNetworkingConfig_SDRClient_ConsecutitivePingTimeoutsFail=20;
      k_ESteamNetworkingConfig_SDRClient_MinPingsBeforePingAccurate=21;
      k_ESteamNetworkingConfig_SDRClient_SingleSocket=22;
      k_ESteamNetworkingConfig_SDRClient_ForceRelayCluster=29;
      k_ESteamNetworkingConfig_SDRClient_DevTicket=30;
      k_ESteamNetworkingConfig_SDRClient_ForceProxyAddr=31;
      k_ESteamNetworkingConfig_SDRClient_FakeClusterPing=36;
      k_ESteamNetworkingConfig_SDRClient_LimitPingProbesToNearestN=60;
      k_ESteamNetworkingConfig_LogLevel_AckRTT=13;
      k_ESteamNetworkingConfig_LogLevel_PacketDecode=14;
      k_ESteamNetworkingConfig_LogLevel_Message=15;
      k_ESteamNetworkingConfig_LogLevel_PacketGaps=16;
      k_ESteamNetworkingConfig_LogLevel_P2PRendezvous=17;
      k_ESteamNetworkingConfig_LogLevel_SDRRelayPings=18;
      k_ESteamNetworkingConfig_ECN=999;
      k_ESteamNetworkingConfig_SDRClient_EnableTOSProbes=998;
      k_ESteamNetworkingConfig_DELETED_EnumerateDevVars=35;
      k_ESteamNetworkingConfigValue__Force32Bit=2147483647;

type { TESteamNetworkingGetConfigValueResult }
     PPESteamNetworkingGetConfigValueResult=^PESteamNetworkingGetConfigValueResult;
     PESteamNetworkingGetConfigValueResult=^TESteamNetworkingGetConfigValueResult;
     TESteamNetworkingGetConfigValueResult=TSteamInt32;

const k_ESteamNetworkingGetConfigValue_BadValue=-1;
      k_ESteamNetworkingGetConfigValue_BadScopeObj=-2;
      k_ESteamNetworkingGetConfigValue_BufferTooSmall=-3;
      k_ESteamNetworkingGetConfigValue_OK=1;
      k_ESteamNetworkingGetConfigValue_OKInherited=2;
      k_ESteamNetworkingGetConfigValueResult__Force32Bit=2147483647;

type { TESteamNetworkingSocketsDebugOutputType }
     PPESteamNetworkingSocketsDebugOutputType=^PESteamNetworkingSocketsDebugOutputType;
     PESteamNetworkingSocketsDebugOutputType=^TESteamNetworkingSocketsDebugOutputType;
     TESteamNetworkingSocketsDebugOutputType=TSteamInt32;

const k_ESteamNetworkingSocketsDebugOutputType_None=0;
      k_ESteamNetworkingSocketsDebugOutputType_Bug=1;
      k_ESteamNetworkingSocketsDebugOutputType_Error=2;
      k_ESteamNetworkingSocketsDebugOutputType_Important=3;
      k_ESteamNetworkingSocketsDebugOutputType_Warning=4;
      k_ESteamNetworkingSocketsDebugOutputType_Msg=5;
      k_ESteamNetworkingSocketsDebugOutputType_Verbose=6;
      k_ESteamNetworkingSocketsDebugOutputType_Debug=7;
      k_ESteamNetworkingSocketsDebugOutputType_Everything=8;
      k_ESteamNetworkingSocketsDebugOutputType__Force32Bit=2147483647;

type { TESteamAPIInitResult }
     PPESteamAPIInitResult=^PESteamAPIInitResult;
     PESteamAPIInitResult=^TESteamAPIInitResult;
     TESteamAPIInitResult=TSteamInt32;

const k_ESteamAPIInitResult_OK=0;
      k_ESteamAPIInitResult_FailedGeneric=1;
      k_ESteamAPIInitResult_NoSteamClient=2;
      k_ESteamAPIInitResult_VersionMismatch=3;

type { TEServerMode }
     PPEServerMode=^PEServerMode;
     PEServerMode=^TEServerMode;
     TEServerMode=TSteamInt32;

const eServerModeInvalid=0;
      eServerModeNoAuthentication=1;
      eServerModeAuthentication=2;
      eServerModeAuthenticationAndSecure=3;

type { TISteamClient }
     PPISteamClient=^PISteamClient;
     PISteamClient=^TISteamClient;
     TISteamClient=record
     end;

type { TISteamUser }
     PPISteamUser=^PISteamUser;
     PISteamUser=^TISteamUser;
     TISteamUser=record
     end;

type { TISteamFriends }
     PPISteamFriends=^PISteamFriends;
     PISteamFriends=^TISteamFriends;
     TISteamFriends=record
     end;

type { TISteamUtils }
     PPISteamUtils=^PISteamUtils;
     PISteamUtils=^TISteamUtils;
     TISteamUtils=record
     end;

type { TISteamMatchmaking }
     PPISteamMatchmaking=^PISteamMatchmaking;
     PISteamMatchmaking=^TISteamMatchmaking;
     TISteamMatchmaking=record
     end;

type { TISteamMatchmakingServerListResponse }
     PPISteamMatchmakingServerListResponse=^PISteamMatchmakingServerListResponse;
     PISteamMatchmakingServerListResponse=^TISteamMatchmakingServerListResponse;
     TISteamMatchmakingServerListResponse=record
     end;

type { TISteamMatchmakingPingResponse }
     PPISteamMatchmakingPingResponse=^PISteamMatchmakingPingResponse;
     PISteamMatchmakingPingResponse=^TISteamMatchmakingPingResponse;
     TISteamMatchmakingPingResponse=record
     end;

type { TISteamMatchmakingPlayersResponse }
     PPISteamMatchmakingPlayersResponse=^PISteamMatchmakingPlayersResponse;
     PISteamMatchmakingPlayersResponse=^TISteamMatchmakingPlayersResponse;
     TISteamMatchmakingPlayersResponse=record
     end;

type { TISteamMatchmakingRulesResponse }
     PPISteamMatchmakingRulesResponse=^PISteamMatchmakingRulesResponse;
     PISteamMatchmakingRulesResponse=^TISteamMatchmakingRulesResponse;
     TISteamMatchmakingRulesResponse=record
     end;

type { TISteamMatchmakingServers }
     PPISteamMatchmakingServers=^PISteamMatchmakingServers;
     PISteamMatchmakingServers=^TISteamMatchmakingServers;
     TISteamMatchmakingServers=record
     end;

type { TISteamParties }
     PPISteamParties=^PISteamParties;
     PISteamParties=^TISteamParties;
     TISteamParties=record
     end;

type { TISteamRemoteStorage }
     PPISteamRemoteStorage=^PISteamRemoteStorage;
     PISteamRemoteStorage=^TISteamRemoteStorage;
     TISteamRemoteStorage=record
     end;

type { TISteamUserStats }
     PPISteamUserStats=^PISteamUserStats;
     PISteamUserStats=^TISteamUserStats;
     TISteamUserStats=record
     end;

type { TISteamApps }
     PPISteamApps=^PISteamApps;
     PISteamApps=^TISteamApps;
     TISteamApps=record
     end;

type { TISteamNetworking }
     PPISteamNetworking=^PISteamNetworking;
     PISteamNetworking=^TISteamNetworking;
     TISteamNetworking=record
     end;

type { TISteamScreenshots }
     PPISteamScreenshots=^PISteamScreenshots;
     PISteamScreenshots=^TISteamScreenshots;
     TISteamScreenshots=record
     end;

type { TISteamMusic }
     PPISteamMusic=^PISteamMusic;
     PISteamMusic=^TISteamMusic;
     TISteamMusic=record
     end;

type { TISteamHTTP }
     PPISteamHTTP=^PISteamHTTP;
     PISteamHTTP=^TISteamHTTP;
     TISteamHTTP=record
     end;

type { TISteamInput }
     PPISteamInput=^PISteamInput;
     PISteamInput=^TISteamInput;
     TISteamInput=record
     end;

type { TISteamController }
     PPISteamController=^PISteamController;
     PISteamController=^TISteamController;
     TISteamController=record
     end;

type { TISteamUGC }
     PPISteamUGC=^PISteamUGC;
     PISteamUGC=^TISteamUGC;
     TISteamUGC=record
     end;

type { TISteamHTMLSurface }
     PPISteamHTMLSurface=^PISteamHTMLSurface;
     PISteamHTMLSurface=^TISteamHTMLSurface;
     TISteamHTMLSurface=record
     end;

type { TISteamInventory }
     PPISteamInventory=^PISteamInventory;
     PISteamInventory=^TISteamInventory;
     TISteamInventory=record
     end;

type { TISteamTimeline }
     PPISteamTimeline=^PISteamTimeline;
     PISteamTimeline=^TISteamTimeline;
     TISteamTimeline=record
     end;

type { TISteamVideo }
     PPISteamVideo=^PISteamVideo;
     PISteamVideo=^TISteamVideo;
     TISteamVideo=record
     end;

type { TISteamParentalSettings }
     PPISteamParentalSettings=^PISteamParentalSettings;
     PISteamParentalSettings=^TISteamParentalSettings;
     TISteamParentalSettings=record
     end;

type { TISteamRemotePlay }
     PPISteamRemotePlay=^PISteamRemotePlay;
     PISteamRemotePlay=^TISteamRemotePlay;
     TISteamRemotePlay=record
     end;

type { TISteamNetworkingMessages }
     PPISteamNetworkingMessages=^PISteamNetworkingMessages;
     PISteamNetworkingMessages=^TISteamNetworkingMessages;
     TISteamNetworkingMessages=record
     end;

type { TISteamNetworkingSockets }
     PPISteamNetworkingSockets=^PISteamNetworkingSockets;
     PISteamNetworkingSockets=^TISteamNetworkingSockets;
     TISteamNetworkingSockets=record
     end;

type { TISteamNetworkingUtils }
     PPISteamNetworkingUtils=^PISteamNetworkingUtils;
     PISteamNetworkingUtils=^TISteamNetworkingUtils;
     TISteamNetworkingUtils=record
     end;

type { TISteamGameServer }
     PPISteamGameServer=^PISteamGameServer;
     PISteamGameServer=^TISteamGameServer;
     TISteamGameServer=record
     end;

type { TISteamGameServerStats }
     PPISteamGameServerStats=^PISteamGameServerStats;
     PISteamGameServerStats=^TISteamGameServerStats;
     TISteamGameServerStats=record
     end;

type { TISteamNetworkingFakeUDPPort }
     PPISteamNetworkingFakeUDPPort=^PISteamNetworkingFakeUDPPort;
     PISteamNetworkingFakeUDPPort=^TISteamNetworkingFakeUDPPort;
     TISteamNetworkingFakeUDPPort=record
     end;

type PPCSteamID=^PCSteamID;
     PCSteamID=^TCSteamID;
     TCSteamID=packed record
      m_rgubSteamID:array[0..8-1] of TSteamUInt8; // Account ID in bit 0..31, account instance in bit 32..51, account type in bit 52..55, universe in bit 56..63
     end;

const k_nCSteamIDAccountIDShift=0;
      k_nCSteamIDAccountIDMask=TSteamUInt64($00000000ffffffff);
      k_nCSteamIDAccountInstanceShift=32;
      k_nCSteamIDAccountInstanceMask=TSteamUInt64($00000000000fffff);
      k_nCSteamIDAccountTypeShift=52;
      k_nCSteamIDAccountTypeMask=TSteamUInt64($000000000000000f);
      k_nCSteamIDUniverseShift=56;
      k_nCSteamIDUniverseMask=TSteamUInt64($00000000000000ff);

type PPCGameID=^PCGameID;
     PCGameID=^TCGameID;
     TCGameID=packed record
      m_rgubGameID:array[0..8-1] of TSteamUInt8; // Application ID in bit 0..23, type in bit 24..31, mod ID in bit 32..63
     end;

const k_nCGameIDAppIDShift=0;
      k_nCGameIDAppIDMask=TSteamUInt64($0000000000ffffff);
      k_nCGameIDTypeShift=24;
      k_nCGameIDTypeMask=TSteamUInt64($00000000000000ff);
      k_nCGameIDModIDShift=32;
      k_nCGameIDModIDMask=TSteamUInt64($00000000ffffffff);

type { TSteamIPAddress_t }
     PPSteamIPAddress_t=^PSteamIPAddress_t;
     PSteamIPAddress_t=^TSteamIPAddress_t;
     TSteamIPAddress_t=packed record
      case TSteamInt32 of
       0:(
        m_unIPv4:TSteamUInt32; // Host order
       );
       1:(
        m_rgubIPv6:array[0..16-1] of TSteamUInt8; // Network order, same as inaddr_in6
       );
       2:(
        m_ipv6Qword:array[0..2-1] of TSteamUInt64; // Big endian, for internal use only
       );
       3:(
        m_Padding:array[0..16-1] of TSteamUInt8;
        m_eType:TESteamIPType;
       );
     end;

type { TFriendGameInfo_t }
     PPFriendGameInfo_t=^PFriendGameInfo_t;
     PFriendGameInfo_t=^TFriendGameInfo_t;
     TFriendGameInfo_t=record
      m_gameID:TCGameID;
      m_unGameIP:TSteamUInt32;
      m_usGamePort:TSteamUInt16;
      m_usQueryPort:TSteamUInt16;
      m_steamIDLobby:TCSteamID;
     end;

{$ifdef fpc}{$packrecords c}{$else}{$A8}{$endif}
type { TMatchMakingKeyValuePair_t }
     PPMatchMakingKeyValuePair_t=^PMatchMakingKeyValuePair_t;
     PMatchMakingKeyValuePair_t=^TMatchMakingKeyValuePair_t;
     TMatchMakingKeyValuePair_t=record
      m_szKey:array[0..256-1] of TSteamChar;
      m_szValue:array[0..256-1] of TSteamChar;
     end;

type { Tservernetadr_t }
     PPservernetadr_t=^Pservernetadr_t;
     Pservernetadr_t=^Tservernetadr_t;
     Tservernetadr_t=record
      m_usConnectionPort:TSteamUInt16;
      m_usQueryPort:TSteamUInt16;
      m_unIP:TSteamUInt32;
     end;

type { Tgameserveritem_t }
     PPgameserveritem_t=^Pgameserveritem_t;
     Pgameserveritem_t=^Tgameserveritem_t;
     Tgameserveritem_t=record
      m_NetAdr:Tservernetadr_t;
      m_nPing:TSteamInt32;
      m_bHadSuccessfulResponse:TSteamBool;
      m_bDoNotRefresh:TSteamBool;
      m_szGameDir:array[0..32-1] of TSteamChar;
      m_szMap:array[0..32-1] of TSteamChar;
      m_szGameDescription:array[0..64-1] of TSteamChar;
      m_nAppID:TSteamUInt32;
      m_nPlayers:TSteamInt32;
      m_nMaxPlayers:TSteamInt32;
      m_nBotPlayers:TSteamInt32;
      m_bPassword:TSteamBool;
      m_bSecure:TSteamBool;
      m_ulTimeLastPlayed:TSteamUInt32;
      m_nServerVersion:TSteamInt32;
      m_szServerName:array[0..64-1] of TSteamChar;
      m_szGameTags:array[0..128-1] of TSteamChar;
      m_steamID:TCSteamID;
     end;

{$ifdef Windows}{$ifdef fpc}{$packrecords 8}{$else}{$A8}{$endif}{$else}{$ifdef fpc}{$packrecords 4}{$else}{$A4}{$endif}{$endif}
type { TSteamPartyBeaconLocation_t }
     PPSteamPartyBeaconLocation_t=^PSteamPartyBeaconLocation_t;
     PSteamPartyBeaconLocation_t=^TSteamPartyBeaconLocation_t;
     TSteamPartyBeaconLocation_t=record
      m_eType:TESteamPartyBeaconLocationType;
      m_ulLocationID:TSteamUInt64;
     end;

type { TSteamParamStringArray_t }
     PPSteamParamStringArray_t=^PSteamParamStringArray_t;
     PSteamParamStringArray_t=^TSteamParamStringArray_t;
     TSteamParamStringArray_t=record
      m_ppStrings:PPSteamChar;
      m_nNumStrings:TSteamInt32;
     end;

type { TLeaderboardEntry_t }
     PPLeaderboardEntry_t=^PLeaderboardEntry_t;
     PLeaderboardEntry_t=^TLeaderboardEntry_t;
     TLeaderboardEntry_t=record
      m_steamIDUser:TCSteamID;
      m_nGlobalRank:TSteamInt32;
      m_nScore:TSteamInt32;
      m_cDetails:TSteamInt32;
      m_hUGC:TUGCHandle_t;
     end;

type { TP2PSessionState_t }
     PPP2PSessionState_t=^PP2PSessionState_t;
     PP2PSessionState_t=^TP2PSessionState_t;
     TP2PSessionState_t=record
      m_bConnectionActive:TSteamUInt8;
      m_bConnecting:TSteamUInt8;
      m_eP2PSessionError:TSteamUInt8;
      m_bUsingRelay:TSteamUInt8;
      m_nBytesQueuedForSend:TSteamInt32;
      m_nPacketsQueuedForSend:TSteamInt32;
      m_nRemoteIP:TSteamUInt32;
      m_nRemotePort:TSteamUInt16;
     end;

type { TInputAnalogActionData_t }
     PPInputAnalogActionData_t=^PInputAnalogActionData_t;
     PInputAnalogActionData_t=^TInputAnalogActionData_t;
     TInputAnalogActionData_t=packed record
      eMode:TEInputSourceMode;
      x:TSteamFloat;
      y:TSteamFloat;
      bActive:TSteamBool;
     end;

type { TInputDigitalActionData_t }
     PPInputDigitalActionData_t=^PInputDigitalActionData_t;
     PInputDigitalActionData_t=^TInputDigitalActionData_t;
     TInputDigitalActionData_t=packed record
      bState:TSteamBool;
      bActive:TSteamBool;
     end;

type { TInputMotionData_t }
     PPInputMotionData_t=^PInputMotionData_t;
     PInputMotionData_t=^TInputMotionData_t;
     TInputMotionData_t=packed record
      rotQuatX:TSteamFloat;
      rotQuatY:TSteamFloat;
      rotQuatZ:TSteamFloat;
      rotQuatW:TSteamFloat;
      posAccelX:TSteamFloat;
      posAccelY:TSteamFloat;
      posAccelZ:TSteamFloat;
      rotVelX:TSteamFloat;
      rotVelY:TSteamFloat;
      rotVelZ:TSteamFloat;
     end;

type { TSteamInputActionEventAnalogAction_t }
     PPSteamInputActionEventAnalogAction_t=^PSteamInputActionEventAnalogAction_t;
     PSteamInputActionEventAnalogAction_t=^TSteamInputActionEventAnalogAction_t;
     TSteamInputActionEventAnalogAction_t=packed record
      actionHandle:TInputAnalogActionHandle_t;
      analogActionData:TInputAnalogActionData_t;
     end;

     { TSteamInputActionEventDigitalAction_t }
     PPSteamInputActionEventDigitalAction_t=^PSteamInputActionEventDigitalAction_t;
     PSteamInputActionEventDigitalAction_t=^TSteamInputActionEventDigitalAction_t;
     TSteamInputActionEventDigitalAction_t=packed record
      actionHandle:TInputDigitalActionHandle_t;
      digitalActionData:TInputDigitalActionData_t;
     end;

     { TSteamInputActionEvent_t }
     PPSteamInputActionEvent_t=^PSteamInputActionEvent_t;
     PSteamInputActionEvent_t=^TSteamInputActionEvent_t;
     TSteamInputActionEvent_t=packed record
      controllerHandle:TInputHandle_t;
      eEventType:TESteamInputActionEventType;
      case TSteamInt32 of
       0:(
        analogAction:TSteamInputActionEventAnalogAction_t;
       );
       1:(
        digitalAction:TSteamInputActionEventDigitalAction_t;
       );
     end;

type { TSteamUGCDetails_t }
     PPSteamUGCDetails_t=^PSteamUGCDetails_t;
     PSteamUGCDetails_t=^TSteamUGCDetails_t;
     TSteamUGCDetails_t=record
      m_nPublishedFileId:TPublishedFileId_t;
      m_eResult:TEResult;
      m_eFileType:TEWorkshopFileType;
      m_nCreatorAppID:TAppId_t;
      m_nConsumerAppID:TAppId_t;
      m_rgchTitle:array[0..129-1] of TSteamChar;
      m_rgchDescription:array[0..8000-1] of TSteamChar;
      m_ulSteamIDOwner:TSteamUInt64;
      m_rtimeCreated:TSteamUInt32;
      m_rtimeUpdated:TSteamUInt32;
      m_rtimeAddedToUserList:TSteamUInt32;
      m_eVisibility:TERemoteStoragePublishedFileVisibility;
      m_bBanned:TSteamBool;
      m_bAcceptedForUse:TSteamBool;
      m_bTagsTruncated:TSteamBool;
      m_rgchTags:array[0..1025-1] of TSteamChar;
      m_hFile:TUGCHandle_t;
      m_hPreviewFile:TUGCHandle_t;
      m_pchFileName:array[0..260-1] of TSteamChar;
      m_nFileSize:TSteamInt32;
      m_nPreviewFileSize:TSteamInt32;
      m_rgchURL:array[0..256-1] of TSteamChar;
      m_unVotesUp:TSteamUInt32;
      m_unVotesDown:TSteamUInt32;
      m_flScore:TSteamFloat;
      m_unNumChildren:TSteamUInt32;
      m_ulTotalFilesSize:TSteamUInt64;
     end;

type { TSteamItemDetails_t }
     PPSteamItemDetails_t=^PSteamItemDetails_t;
     PSteamItemDetails_t=^TSteamItemDetails_t;
     TSteamItemDetails_t=record
      m_itemId:TSteamItemInstanceID_t;
      m_iDefinition:TSteamItemDef_t;
      m_unQuantity:TSteamUInt16;
      m_unFlags:TSteamUInt16;
     end;

type { TRemotePlayInputMouseMotion_t }
     PPRemotePlayInputMouseMotion_t=^PRemotePlayInputMouseMotion_t;
     PRemotePlayInputMouseMotion_t=^TRemotePlayInputMouseMotion_t;
     TRemotePlayInputMouseMotion_t=record
      m_bAbsolute:TSteamBool;
      m_flNormalizedX:TSteamFloat;
      m_flNormalizedY:TSteamFloat;
      m_nDeltaX:TSteamInt32;
      m_nDeltaY:TSteamInt32;
     end;

type { TRemotePlayInputMouseWheel_t }
     PPRemotePlayInputMouseWheel_t=^PRemotePlayInputMouseWheel_t;
     PRemotePlayInputMouseWheel_t=^TRemotePlayInputMouseWheel_t;
     TRemotePlayInputMouseWheel_t=record
      m_eDirection:TERemotePlayMouseWheelDirection;
      m_flAmount:TSteamFloat;
     end;

type { TRemotePlayInputKey_t }
     PPRemotePlayInputKey_t=^PRemotePlayInputKey_t;
     PRemotePlayInputKey_t=^TRemotePlayInputKey_t;
     TRemotePlayInputKey_t=record
      m_eScancode:TSteamInt32;
      m_unModifiers:TSteamUInt32;
      m_unKeycode:TSteamUInt32;
     end;

type { TRemotePlayInput_t }
     PPRemotePlayInput_t=^PRemotePlayInput_t;
     PRemotePlayInput_t=^TRemotePlayInput_t;
     TRemotePlayInput_t=record
      m_unSessionID:TRemotePlaySessionID_t;
      m_eType:TERemotePlayInputType;
      case TSteamInt32 of
       0:(
        m_MouseMotion:TRemotePlayInputMouseMotion_t; // Valid when m_eType is k_ERemotePlayInputMouseMotion
       );
       1:(
        m_eMouseButton:TERemotePlayMouseButton; // Valid when m_eType is k_ERemotePlayInputMouseButtonDown or k_ERemotePlayInputMouseButtonUp
       );
       2:(
        m_MouseWheel:TRemotePlayInputMouseWheel_t; // Valid when m_eType is k_ERemotePlayInputMouseWheel
       );
       3:(
        m_Key:TRemotePlayInputKey_t; // Valid when m_eType is k_ERemotePlayInputKeyDown or k_ERemotePlayInputKeyUp
       );
       4:(
        padding:array[0..56-1] of TSteamChar; // Unused space for future use
       );
     end;

type { TSteamNetworkingIPAddr }
     PPSteamNetworkingIPAddr=^PSteamNetworkingIPAddr;
     PSteamNetworkingIPAddr=^TSteamNetworkingIPAddr;
     TSteamNetworkingIPAddr=packed record
      m_rgubData:array[0..18-1] of TSteamUInt8; // See TSteamNetworkingIPAddrView for the field layout
     end;

     { TSteamNetworkingIPAddrIPv4MappedAddress }
     PPSteamNetworkingIPAddrIPv4MappedAddress=^PSteamNetworkingIPAddrIPv4MappedAddress;
     PSteamNetworkingIPAddrIPv4MappedAddress=^TSteamNetworkingIPAddrIPv4MappedAddress;
     TSteamNetworkingIPAddrIPv4MappedAddress=packed record
      m_8zeros:TSteamUInt64;
      m_0000:TSteamUInt16;
      m_ffff:TSteamUInt16;
      m_ip:array[0..4-1] of TSteamUInt8; // Network byte order
     end;

     { TSteamNetworkingIPAddrView }
     PPSteamNetworkingIPAddrView=^PSteamNetworkingIPAddrView;
     PSteamNetworkingIPAddrView=^TSteamNetworkingIPAddrView;
     TSteamNetworkingIPAddrView=packed record
      case TSteamInt32 of
       0:(
        m_ipv6:array[0..16-1] of TSteamUInt8;
        m_port:TSteamUInt16; // Host byte order
       );
       1:(
        m_ipv4:TSteamNetworkingIPAddrIPv4MappedAddress;
       );
     end;

const k_cchSteamNetworkingIPAddrMaxString=48;

type { TSteamNetworkingIdentity }
     PPSteamNetworkingIdentity=^PSteamNetworkingIdentity;
     PSteamNetworkingIdentity=^TSteamNetworkingIdentity;
     TSteamNetworkingIdentity=packed record
      m_rgubData:array[0..136-1] of TSteamUInt8; // See TSteamNetworkingIdentityView for the field layout
     end;

     { TSteamNetworkingIdentityView }
     PPSteamNetworkingIdentityView=^PSteamNetworkingIdentityView;
     PSteamNetworkingIdentityView=^TSteamNetworkingIdentityView;
     TSteamNetworkingIdentityView=packed record
      m_eType:TESteamNetworkingIdentityType;
      m_cbSize:TSteamInt32;
      case TSteamInt32 of
       0:(
        m_steamID64:TSteamUInt64;
       );
       1:(
        m_PSNID:TSteamUInt64;
       );
       2:(
        m_szGenericString:array[0..32-1] of TSteamChar;
       );
       3:(
        m_szXboxPairwiseID:array[0..33-1] of TSteamChar;
       );
       4:(
        m_genericBytes:array[0..32-1] of TSteamUInt8;
       );
       5:(
        m_szUnknownRawString:array[0..128-1] of TSteamChar;
       );
       6:(
        m_ip:TSteamNetworkingIPAddrView;
       );
       7:(
        m_reserved:array[0..32-1] of TSteamUInt32; // Pads the record out to leave room for future expansion
       );
     end;

const k_cchSteamNetworkingIdentityMaxString=128;
      k_cchSteamNetworkingIdentityMaxGenericString=32;
      k_cchSteamNetworkingIdentityMaxXboxPairwiseID=33;
      k_cbSteamNetworkingIdentityMaxGenericBytes=32;

type { TSteamNetConnectionInfo_t }
     PPSteamNetConnectionInfo_t=^PSteamNetConnectionInfo_t;
     PSteamNetConnectionInfo_t=^TSteamNetConnectionInfo_t;
     TSteamNetConnectionInfo_t=record
      m_identityRemote:TSteamNetworkingIdentity;
      m_nUserData:TSteamInt64;
      m_hListenSocket:THSteamListenSocket;
      m_addrRemote:TSteamNetworkingIPAddr;
      m__pad1:TSteamUInt16;
      m_idPOPRemote:TSteamNetworkingPOPID;
      m_idPOPRelay:TSteamNetworkingPOPID;
      m_eState:TESteamNetworkingConnectionState;
      m_eEndReason:TSteamInt32;
      m_szEndDebug:array[0..128-1] of TSteamChar;
      m_szConnectionDescription:array[0..128-1] of TSteamChar;
      m_nFlags:TSteamInt32;
      reserved:array[0..63-1] of TSteamUInt32;
     end;

type { TSteamNetConnectionRealTimeStatus_t }
     PPSteamNetConnectionRealTimeStatus_t=^PSteamNetConnectionRealTimeStatus_t;
     PSteamNetConnectionRealTimeStatus_t=^TSteamNetConnectionRealTimeStatus_t;
     TSteamNetConnectionRealTimeStatus_t=record
      m_eState:TESteamNetworkingConnectionState;
      m_nPing:TSteamInt32;
      m_flConnectionQualityLocal:TSteamFloat;
      m_flConnectionQualityRemote:TSteamFloat;
      m_flOutPacketsPerSec:TSteamFloat;
      m_flOutBytesPerSec:TSteamFloat;
      m_flInPacketsPerSec:TSteamFloat;
      m_flInBytesPerSec:TSteamFloat;
      m_nSendRateBytesPerSecond:TSteamInt32;
      m_cbPendingUnreliable:TSteamInt32;
      m_cbPendingReliable:TSteamInt32;
      m_cbSentUnackedReliable:TSteamInt32;
      m_usecQueueTime:TSteamNetworkingMicroseconds;
      m_usecMaxJitter:TSteamInt32;
      reserved:array[0..15-1] of TSteamUInt32;
     end;

type { TSteamNetConnectionRealTimeLaneStatus_t }
     PPSteamNetConnectionRealTimeLaneStatus_t=^PSteamNetConnectionRealTimeLaneStatus_t;
     PSteamNetConnectionRealTimeLaneStatus_t=^TSteamNetConnectionRealTimeLaneStatus_t;
     TSteamNetConnectionRealTimeLaneStatus_t=record
      m_cbPendingUnreliable:TSteamInt32;
      m_cbPendingReliable:TSteamInt32;
      m_cbSentUnackedReliable:TSteamInt32;
      _reservePad1:TSteamInt32;
      m_usecQueueTime:TSteamNetworkingMicroseconds;
      reserved:array[0..10-1] of TSteamUInt32;
     end;

{$ifdef fpc}{$packrecords c}{$else}{$A8}{$endif}
type { TSteamNetworkingMessage_t }
     PPSteamNetworkingMessage_t=^PSteamNetworkingMessage_t;
     PSteamNetworkingMessage_t=^TSteamNetworkingMessage_t;
     TSteamNetworkingMessage_t_m_pfnFreeData=procedure(const aParameter1:PSteamNetworkingMessage_t); cdecl;
     TSteamNetworkingMessage_t_m_pfnRelease=procedure(const aParameter1:PSteamNetworkingMessage_t); cdecl;
     TSteamNetworkingMessage_t=record
      m_pData:TSteamPointer;
      m_cbSize:TSteamInt32;
      m_conn:THSteamNetConnection;
      m_identityPeer:TSteamNetworkingIdentity;
      m_nConnUserData:TSteamInt64;
      m_usecTimeReceived:TSteamNetworkingMicroseconds;
      m_nMessageNumber:TSteamInt64;
      m_pfnFreeData:TSteamNetworkingMessage_t_m_pfnFreeData;
      m_pfnRelease:TSteamNetworkingMessage_t_m_pfnRelease;
      m_nChannel:TSteamInt32;
      m_nFlags:TSteamInt32;
      m_nUserData:TSteamInt64;
      m_idxLane:TSteamUInt16;
      _pad1__:TSteamUInt16;
     end;

type { TSteamNetworkPingLocation_t }
     PPSteamNetworkPingLocation_t=^PSteamNetworkPingLocation_t;
     PSteamNetworkPingLocation_t=^TSteamNetworkPingLocation_t;
     TSteamNetworkPingLocation_t=record
      m_data:array[0..512-1] of TSteamUInt8;
     end;

type { TSteamNetworkingConfigValue_t }
     PPSteamNetworkingConfigValue_t=^PSteamNetworkingConfigValue_t;
     PSteamNetworkingConfigValue_t=^TSteamNetworkingConfigValue_t;
     TSteamNetworkingConfigValue_t=record
      m_eValue:TESteamNetworkingConfigValue;
      m_eDataType:TESteamNetworkingConfigDataType;
      case TSteamInt32 of
       0:(
        m_int32:TSteamInt32;
       );
       1:(
        m_int64:TSteamInt64;
       );
       2:(
        m_float:TSteamFloat;
       );
       3:(
        m_string:PSteamChar; // Points at a caller owned zero terminated buffer
       );
       4:(
        m_ptr:TSteamPointer;
       );
     end;

{$ifdef Windows}{$ifdef fpc}{$packrecords 8}{$else}{$A8}{$endif}{$else}{$ifdef fpc}{$packrecords 4}{$else}{$A4}{$endif}{$endif}
type { TSteamDatagramHostedAddress }
     PPSteamDatagramHostedAddress=^PSteamDatagramHostedAddress;
     PSteamDatagramHostedAddress=^TSteamDatagramHostedAddress;
     TSteamDatagramHostedAddress=record
      m_cbSize:TSteamInt32;
      m_data:array[0..128-1] of TSteamChar;
     end;

type { TSteamDatagramGameCoordinatorServerLogin }
     PPSteamDatagramGameCoordinatorServerLogin=^PSteamDatagramGameCoordinatorServerLogin;
     PSteamDatagramGameCoordinatorServerLogin=^TSteamDatagramGameCoordinatorServerLogin;
     TSteamDatagramGameCoordinatorServerLogin=record
      m_identity:TSteamNetworkingIdentity;
      m_routing:TSteamDatagramHostedAddress;
      m_nAppID:TAppId_t;
      m_rtime:TRTime32;
      m_cbAppData:TSteamInt32;
      m_appData:array[0..2048-1] of TSteamChar;
     end;

type { TSteamServersConnected_t }
     PPSteamServersConnected_t=^PSteamServersConnected_t;
     PSteamServersConnected_t=^TSteamServersConnected_t;
     TSteamServersConnected_t=record
      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size
     end;

const SteamServersConnected_t_k_iCallback=101;

type { TSteamServerConnectFailure_t }
     PPSteamServerConnectFailure_t=^PSteamServerConnectFailure_t;
     PSteamServerConnectFailure_t=^TSteamServerConnectFailure_t;
     TSteamServerConnectFailure_t=record
      m_eResult:TEResult;
      m_bStillRetrying:TSteamBool;
     end;

const SteamServerConnectFailure_t_k_iCallback=102;

type { TSteamServersDisconnected_t }
     PPSteamServersDisconnected_t=^PSteamServersDisconnected_t;
     PSteamServersDisconnected_t=^TSteamServersDisconnected_t;
     TSteamServersDisconnected_t=record
      m_eResult:TEResult;
     end;

const SteamServersDisconnected_t_k_iCallback=103;

type { TClientGameServerDeny_t }
     PPClientGameServerDeny_t=^PClientGameServerDeny_t;
     PClientGameServerDeny_t=^TClientGameServerDeny_t;
     TClientGameServerDeny_t=record
      m_uAppID:TSteamUInt32;
      m_unGameServerIP:TSteamUInt32;
      m_usGameServerPort:TSteamUInt16;
      m_bSecure:TSteamUInt16;
      m_uReason:TSteamUInt32;
     end;

const ClientGameServerDeny_t_k_iCallback=113;

type { TIPCFailure_t }
     PPIPCFailure_t=^PIPCFailure_t;
     PIPCFailure_t=^TIPCFailure_t;
     TIPCFailure_t=record
      m_eFailureType:TSteamUInt8;
     end;

const IPCFailure_t_k_iCallback=117;

type { TLicensesUpdated_t }
     PPLicensesUpdated_t=^PLicensesUpdated_t;
     PLicensesUpdated_t=^TLicensesUpdated_t;
     TLicensesUpdated_t=record
      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size
     end;

const LicensesUpdated_t_k_iCallback=125;

type { TValidateAuthTicketResponse_t }
     PPValidateAuthTicketResponse_t=^PValidateAuthTicketResponse_t;
     PValidateAuthTicketResponse_t=^TValidateAuthTicketResponse_t;
     TValidateAuthTicketResponse_t=record
      m_SteamID:TCSteamID;
      m_eAuthSessionResponse:TEAuthSessionResponse;
      m_OwnerSteamID:TCSteamID;
     end;

const ValidateAuthTicketResponse_t_k_iCallback=143;

type { TMicroTxnAuthorizationResponse_t }
     PPMicroTxnAuthorizationResponse_t=^PMicroTxnAuthorizationResponse_t;
     PMicroTxnAuthorizationResponse_t=^TMicroTxnAuthorizationResponse_t;
     TMicroTxnAuthorizationResponse_t=record
      m_unAppID:TSteamUInt32;
      m_ulOrderID:TSteamUInt64;
      m_bAuthorized:TSteamUInt8;
     end;

const MicroTxnAuthorizationResponse_t_k_iCallback=152;

type { TEncryptedAppTicketResponse_t }
     PPEncryptedAppTicketResponse_t=^PEncryptedAppTicketResponse_t;
     PEncryptedAppTicketResponse_t=^TEncryptedAppTicketResponse_t;
     TEncryptedAppTicketResponse_t=record
      m_eResult:TEResult;
     end;

const EncryptedAppTicketResponse_t_k_iCallback=154;

type { TGetAuthSessionTicketResponse_t }
     PPGetAuthSessionTicketResponse_t=^PGetAuthSessionTicketResponse_t;
     PGetAuthSessionTicketResponse_t=^TGetAuthSessionTicketResponse_t;
     TGetAuthSessionTicketResponse_t=record
      m_hAuthTicket:THAuthTicket;
      m_eResult:TEResult;
     end;

const GetAuthSessionTicketResponse_t_k_iCallback=163;

type { TGameWebCallback_t }
     PPGameWebCallback_t=^PGameWebCallback_t;
     PGameWebCallback_t=^TGameWebCallback_t;
     TGameWebCallback_t=record
      m_szURL:array[0..256-1] of TSteamChar;
     end;

const GameWebCallback_t_k_iCallback=164;

type { TStoreAuthURLResponse_t }
     PPStoreAuthURLResponse_t=^PStoreAuthURLResponse_t;
     PStoreAuthURLResponse_t=^TStoreAuthURLResponse_t;
     TStoreAuthURLResponse_t=record
      m_szURL:array[0..512-1] of TSteamChar;
     end;

const StoreAuthURLResponse_t_k_iCallback=165;

type { TMarketEligibilityResponse_t }
     PPMarketEligibilityResponse_t=^PMarketEligibilityResponse_t;
     PMarketEligibilityResponse_t=^TMarketEligibilityResponse_t;
     TMarketEligibilityResponse_t=record
      m_bAllowed:TSteamBool;
      m_eNotAllowedReason:TEMarketNotAllowedReasonFlags;
      m_rtAllowedAtTime:TRTime32;
      m_cdaySteamGuardRequiredDays:TSteamInt32;
      m_cdayNewDeviceCooldown:TSteamInt32;
     end;

const MarketEligibilityResponse_t_k_iCallback=166;

type { TDurationControl_t }
     PPDurationControl_t=^PDurationControl_t;
     PDurationControl_t=^TDurationControl_t;
     TDurationControl_t=record
      m_eResult:TEResult;
      m_appid:TAppId_t;
      m_bApplicable:TSteamBool;
      m_csecsLast5h:TSteamInt32;
      m_progress:TEDurationControlProgress;
      m_notification:TEDurationControlNotification;
      m_csecsToday:TSteamInt32;
      m_csecsRemaining:TSteamInt32;
     end;

const DurationControl_t_k_iCallback=167;

type { TGetTicketForWebApiResponse_t }
     PPGetTicketForWebApiResponse_t=^PGetTicketForWebApiResponse_t;
     PGetTicketForWebApiResponse_t=^TGetTicketForWebApiResponse_t;
     TGetTicketForWebApiResponse_t=record
      m_hAuthTicket:THAuthTicket;
      m_eResult:TEResult;
      m_cubTicket:TSteamInt32;
      m_rgubTicket:array[0..2560-1] of TSteamUInt8;
     end;

const GetTicketForWebApiResponse_t_k_iCallback=168;

type { TPersonaStateChange_t }
     PPPersonaStateChange_t=^PPersonaStateChange_t;
     PPersonaStateChange_t=^TPersonaStateChange_t;
     TPersonaStateChange_t=record
      m_ulSteamID:TSteamUInt64;
      m_nChangeFlags:TSteamInt32;
     end;

const PersonaStateChange_t_k_iCallback=304;

type { TGameOverlayActivated_t }
     PPGameOverlayActivated_t=^PGameOverlayActivated_t;
     PGameOverlayActivated_t=^TGameOverlayActivated_t;
     TGameOverlayActivated_t=record
      m_bActive:TSteamUInt8;
      m_bUserInitiated:TSteamBool;
      m_nAppID:TAppId_t;
      m_dwOverlayPID:TSteamUInt32;
     end;

const GameOverlayActivated_t_k_iCallback=331;

type { TGameServerChangeRequested_t }
     PPGameServerChangeRequested_t=^PGameServerChangeRequested_t;
     PGameServerChangeRequested_t=^TGameServerChangeRequested_t;
     TGameServerChangeRequested_t=record
      m_rgchServer:array[0..64-1] of TSteamChar;
      m_rgchPassword:array[0..64-1] of TSteamChar;
     end;

const GameServerChangeRequested_t_k_iCallback=332;

type { TGameLobbyJoinRequested_t }
     PPGameLobbyJoinRequested_t=^PGameLobbyJoinRequested_t;
     PGameLobbyJoinRequested_t=^TGameLobbyJoinRequested_t;
     TGameLobbyJoinRequested_t=record
      m_steamIDLobby:TCSteamID;
      m_steamIDFriend:TCSteamID;
     end;

const GameLobbyJoinRequested_t_k_iCallback=333;

type { TAvatarImageLoaded_t }
     PPAvatarImageLoaded_t=^PAvatarImageLoaded_t;
     PAvatarImageLoaded_t=^TAvatarImageLoaded_t;
     TAvatarImageLoaded_t=record
      m_steamID:TCSteamID;
      m_iImage:TSteamInt32;
      m_iWide:TSteamInt32;
      m_iTall:TSteamInt32;
     end;

const AvatarImageLoaded_t_k_iCallback=334;

type { TClanOfficerListResponse_t }
     PPClanOfficerListResponse_t=^PClanOfficerListResponse_t;
     PClanOfficerListResponse_t=^TClanOfficerListResponse_t;
     TClanOfficerListResponse_t=record
      m_steamIDClan:TCSteamID;
      m_cOfficers:TSteamInt32;
      m_bSuccess:TSteamUInt8;
     end;

const ClanOfficerListResponse_t_k_iCallback=335;

type { TFriendRichPresenceUpdate_t }
     PPFriendRichPresenceUpdate_t=^PFriendRichPresenceUpdate_t;
     PFriendRichPresenceUpdate_t=^TFriendRichPresenceUpdate_t;
     TFriendRichPresenceUpdate_t=record
      m_steamIDFriend:TCSteamID;
      m_nAppID:TAppId_t;
     end;

const FriendRichPresenceUpdate_t_k_iCallback=336;

type { TGameRichPresenceJoinRequested_t }
     PPGameRichPresenceJoinRequested_t=^PGameRichPresenceJoinRequested_t;
     PGameRichPresenceJoinRequested_t=^TGameRichPresenceJoinRequested_t;
     TGameRichPresenceJoinRequested_t=record
      m_steamIDFriend:TCSteamID;
      m_rgchConnect:array[0..256-1] of TSteamChar;
     end;

const GameRichPresenceJoinRequested_t_k_iCallback=337;

type { TGameConnectedClanChatMsg_t }
     PPGameConnectedClanChatMsg_t=^PGameConnectedClanChatMsg_t;
     PGameConnectedClanChatMsg_t=^TGameConnectedClanChatMsg_t;
     TGameConnectedClanChatMsg_t=record
      m_steamIDClanChat:TCSteamID;
      m_steamIDUser:TCSteamID;
      m_iMessageID:TSteamInt32;
     end;

const GameConnectedClanChatMsg_t_k_iCallback=338;

type { TGameConnectedChatJoin_t }
     PPGameConnectedChatJoin_t=^PGameConnectedChatJoin_t;
     PGameConnectedChatJoin_t=^TGameConnectedChatJoin_t;
     TGameConnectedChatJoin_t=record
      m_steamIDClanChat:TCSteamID;
      m_steamIDUser:TCSteamID;
     end;

const GameConnectedChatJoin_t_k_iCallback=339;

type { TGameConnectedChatLeave_t }
     PPGameConnectedChatLeave_t=^PGameConnectedChatLeave_t;
     PGameConnectedChatLeave_t=^TGameConnectedChatLeave_t;
     TGameConnectedChatLeave_t=record
      m_steamIDClanChat:TCSteamID;
      m_steamIDUser:TCSteamID;
      m_bKicked:TSteamBool;
      m_bDropped:TSteamBool;
     end;

const GameConnectedChatLeave_t_k_iCallback=340;

type { TDownloadClanActivityCountsResult_t }
     PPDownloadClanActivityCountsResult_t=^PDownloadClanActivityCountsResult_t;
     PDownloadClanActivityCountsResult_t=^TDownloadClanActivityCountsResult_t;
     TDownloadClanActivityCountsResult_t=record
      m_bSuccess:TSteamBool;
     end;

const DownloadClanActivityCountsResult_t_k_iCallback=341;

type { TJoinClanChatRoomCompletionResult_t }
     PPJoinClanChatRoomCompletionResult_t=^PJoinClanChatRoomCompletionResult_t;
     PJoinClanChatRoomCompletionResult_t=^TJoinClanChatRoomCompletionResult_t;
     TJoinClanChatRoomCompletionResult_t=record
      m_steamIDClanChat:TCSteamID;
      m_eChatRoomEnterResponse:TEChatRoomEnterResponse;
     end;

const JoinClanChatRoomCompletionResult_t_k_iCallback=342;

type { TGameConnectedFriendChatMsg_t }
     PPGameConnectedFriendChatMsg_t=^PGameConnectedFriendChatMsg_t;
     PGameConnectedFriendChatMsg_t=^TGameConnectedFriendChatMsg_t;
     TGameConnectedFriendChatMsg_t=record
      m_steamIDUser:TCSteamID;
      m_iMessageID:TSteamInt32;
     end;

const GameConnectedFriendChatMsg_t_k_iCallback=343;

type { TFriendsGetFollowerCount_t }
     PPFriendsGetFollowerCount_t=^PFriendsGetFollowerCount_t;
     PFriendsGetFollowerCount_t=^TFriendsGetFollowerCount_t;
     TFriendsGetFollowerCount_t=record
      m_eResult:TEResult;
      m_steamID:TCSteamID;
      m_nCount:TSteamInt32;
     end;

const FriendsGetFollowerCount_t_k_iCallback=344;

type { TFriendsIsFollowing_t }
     PPFriendsIsFollowing_t=^PFriendsIsFollowing_t;
     PFriendsIsFollowing_t=^TFriendsIsFollowing_t;
     TFriendsIsFollowing_t=record
      m_eResult:TEResult;
      m_steamID:TCSteamID;
      m_bIsFollowing:TSteamBool;
     end;

const FriendsIsFollowing_t_k_iCallback=345;

type { TFriendsEnumerateFollowingList_t }
     PPFriendsEnumerateFollowingList_t=^PFriendsEnumerateFollowingList_t;
     PFriendsEnumerateFollowingList_t=^TFriendsEnumerateFollowingList_t;
     TFriendsEnumerateFollowingList_t=record
      m_eResult:TEResult;
      m_rgSteamID:array[0..50-1] of TCSteamID;
      m_nResultsReturned:TSteamInt32;
      m_nTotalResultCount:TSteamInt32;
     end;

const FriendsEnumerateFollowingList_t_k_iCallback=346;

type { TUnreadChatMessagesChanged_t }
     PPUnreadChatMessagesChanged_t=^PUnreadChatMessagesChanged_t;
     PUnreadChatMessagesChanged_t=^TUnreadChatMessagesChanged_t;
     TUnreadChatMessagesChanged_t=record
      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size
     end;

const UnreadChatMessagesChanged_t_k_iCallback=348;

type { TOverlayBrowserProtocolNavigation_t }
     PPOverlayBrowserProtocolNavigation_t=^POverlayBrowserProtocolNavigation_t;
     POverlayBrowserProtocolNavigation_t=^TOverlayBrowserProtocolNavigation_t;
     TOverlayBrowserProtocolNavigation_t=record
      rgchURI:array[0..1024-1] of TSteamChar;
     end;

const OverlayBrowserProtocolNavigation_t_k_iCallback=349;

type { TEquippedProfileItemsChanged_t }
     PPEquippedProfileItemsChanged_t=^PEquippedProfileItemsChanged_t;
     PEquippedProfileItemsChanged_t=^TEquippedProfileItemsChanged_t;
     TEquippedProfileItemsChanged_t=record
      m_steamID:TCSteamID;
     end;

const EquippedProfileItemsChanged_t_k_iCallback=350;

type { TEquippedProfileItems_t }
     PPEquippedProfileItems_t=^PEquippedProfileItems_t;
     PEquippedProfileItems_t=^TEquippedProfileItems_t;
     TEquippedProfileItems_t=record
      m_eResult:TEResult;
      m_steamID:TCSteamID;
      m_bHasAnimatedAvatar:TSteamBool;
      m_bHasAvatarFrame:TSteamBool;
      m_bHasProfileModifier:TSteamBool;
      m_bHasProfileBackground:TSteamBool;
      m_bHasMiniProfileBackground:TSteamBool;
      m_bFromCache:TSteamBool;
     end;

const EquippedProfileItems_t_k_iCallback=351;

type { TIPCountry_t }
     PPIPCountry_t=^PIPCountry_t;
     PIPCountry_t=^TIPCountry_t;
     TIPCountry_t=record
      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size
     end;

const IPCountry_t_k_iCallback=701;

type { TLowBatteryPower_t }
     PPLowBatteryPower_t=^PLowBatteryPower_t;
     PLowBatteryPower_t=^TLowBatteryPower_t;
     TLowBatteryPower_t=record
      m_nMinutesBatteryLeft:TSteamUInt8;
     end;

const LowBatteryPower_t_k_iCallback=702;

type { TSteamAPICallCompleted_t }
     PPSteamAPICallCompleted_t=^PSteamAPICallCompleted_t;
     PSteamAPICallCompleted_t=^TSteamAPICallCompleted_t;
     TSteamAPICallCompleted_t=record
      m_hAsyncCall:TSteamAPICall_t;
      m_iCallback:TSteamInt32;
      m_cubParam:TSteamUInt32;
     end;

const SteamAPICallCompleted_t_k_iCallback=703;

type { TSteamShutdown_t }
     PPSteamShutdown_t=^PSteamShutdown_t;
     PSteamShutdown_t=^TSteamShutdown_t;
     TSteamShutdown_t=record
      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size
     end;

const SteamShutdown_t_k_iCallback=704;

type { TCheckFileSignature_t }
     PPCheckFileSignature_t=^PCheckFileSignature_t;
     PCheckFileSignature_t=^TCheckFileSignature_t;
     TCheckFileSignature_t=record
      m_eCheckFileSignature:TECheckFileSignature;
     end;

const CheckFileSignature_t_k_iCallback=705;

type { TGamepadTextInputDismissed_t }
     PPGamepadTextInputDismissed_t=^PGamepadTextInputDismissed_t;
     PGamepadTextInputDismissed_t=^TGamepadTextInputDismissed_t;
     TGamepadTextInputDismissed_t=record
      m_bSubmitted:TSteamBool;
      m_unSubmittedText:TSteamUInt32;
      m_unAppID:TAppId_t;
     end;

const GamepadTextInputDismissed_t_k_iCallback=714;

type { TAppResumingFromSuspend_t }
     PPAppResumingFromSuspend_t=^PAppResumingFromSuspend_t;
     PAppResumingFromSuspend_t=^TAppResumingFromSuspend_t;
     TAppResumingFromSuspend_t=record
      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size
     end;

const AppResumingFromSuspend_t_k_iCallback=736;

type { TFloatingGamepadTextInputDismissed_t }
     PPFloatingGamepadTextInputDismissed_t=^PFloatingGamepadTextInputDismissed_t;
     PFloatingGamepadTextInputDismissed_t=^TFloatingGamepadTextInputDismissed_t;
     TFloatingGamepadTextInputDismissed_t=record
      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size
     end;

const FloatingGamepadTextInputDismissed_t_k_iCallback=738;

type { TFilterTextDictionaryChanged_t }
     PPFilterTextDictionaryChanged_t=^PFilterTextDictionaryChanged_t;
     PFilterTextDictionaryChanged_t=^TFilterTextDictionaryChanged_t;
     TFilterTextDictionaryChanged_t=record
      m_eLanguage:TSteamInt32;
     end;

const FilterTextDictionaryChanged_t_k_iCallback=739;

type { TFavoritesListChanged_t }
     PPFavoritesListChanged_t=^PFavoritesListChanged_t;
     PFavoritesListChanged_t=^TFavoritesListChanged_t;
     TFavoritesListChanged_t=record
      m_nIP:TSteamUInt32;
      m_nQueryPort:TSteamUInt32;
      m_nConnPort:TSteamUInt32;
      m_nAppID:TSteamUInt32;
      m_nFlags:TSteamUInt32;
      m_bAdd:TSteamBool;
      m_unAccountId:TAccountID_t;
     end;

const FavoritesListChanged_t_k_iCallback=502;

type { TLobbyInvite_t }
     PPLobbyInvite_t=^PLobbyInvite_t;
     PLobbyInvite_t=^TLobbyInvite_t;
     TLobbyInvite_t=record
      m_ulSteamIDUser:TSteamUInt64;
      m_ulSteamIDLobby:TSteamUInt64;
      m_ulGameID:TSteamUInt64;
     end;

const LobbyInvite_t_k_iCallback=503;

type { TLobbyEnter_t }
     PPLobbyEnter_t=^PLobbyEnter_t;
     PLobbyEnter_t=^TLobbyEnter_t;
     TLobbyEnter_t=record
      m_ulSteamIDLobby:TSteamUInt64;
      m_rgfChatPermissions:TSteamUInt32;
      m_bLocked:TSteamBool;
      m_EChatRoomEnterResponse:TSteamUInt32;
     end;

const LobbyEnter_t_k_iCallback=504;

type { TLobbyDataUpdate_t }
     PPLobbyDataUpdate_t=^PLobbyDataUpdate_t;
     PLobbyDataUpdate_t=^TLobbyDataUpdate_t;
     TLobbyDataUpdate_t=record
      m_ulSteamIDLobby:TSteamUInt64;
      m_ulSteamIDMember:TSteamUInt64;
      m_bSuccess:TSteamUInt8;
     end;

const LobbyDataUpdate_t_k_iCallback=505;

type { TLobbyChatUpdate_t }
     PPLobbyChatUpdate_t=^PLobbyChatUpdate_t;
     PLobbyChatUpdate_t=^TLobbyChatUpdate_t;
     TLobbyChatUpdate_t=record
      m_ulSteamIDLobby:TSteamUInt64;
      m_ulSteamIDUserChanged:TSteamUInt64;
      m_ulSteamIDMakingChange:TSteamUInt64;
      m_rgfChatMemberStateChange:TSteamUInt32;
     end;

const LobbyChatUpdate_t_k_iCallback=506;

type { TLobbyChatMsg_t }
     PPLobbyChatMsg_t=^PLobbyChatMsg_t;
     PLobbyChatMsg_t=^TLobbyChatMsg_t;
     TLobbyChatMsg_t=record
      m_ulSteamIDLobby:TSteamUInt64;
      m_ulSteamIDUser:TSteamUInt64;
      m_eChatEntryType:TSteamUInt8;
      m_iChatID:TSteamUInt32;
     end;

const LobbyChatMsg_t_k_iCallback=507;

type { TLobbyGameCreated_t }
     PPLobbyGameCreated_t=^PLobbyGameCreated_t;
     PLobbyGameCreated_t=^TLobbyGameCreated_t;
     TLobbyGameCreated_t=record
      m_ulSteamIDLobby:TSteamUInt64;
      m_ulSteamIDGameServer:TSteamUInt64;
      m_unIP:TSteamUInt32;
      m_usPort:TSteamUInt16;
     end;

const LobbyGameCreated_t_k_iCallback=509;

type { TLobbyMatchList_t }
     PPLobbyMatchList_t=^PLobbyMatchList_t;
     PLobbyMatchList_t=^TLobbyMatchList_t;
     TLobbyMatchList_t=record
      m_nLobbiesMatching:TSteamUInt32;
     end;

const LobbyMatchList_t_k_iCallback=510;

type { TLobbyKicked_t }
     PPLobbyKicked_t=^PLobbyKicked_t;
     PLobbyKicked_t=^TLobbyKicked_t;
     TLobbyKicked_t=record
      m_ulSteamIDLobby:TSteamUInt64;
      m_ulSteamIDAdmin:TSteamUInt64;
      m_bKickedDueToDisconnect:TSteamUInt8;
     end;

const LobbyKicked_t_k_iCallback=512;

type { TLobbyCreated_t }
     PPLobbyCreated_t=^PLobbyCreated_t;
     PLobbyCreated_t=^TLobbyCreated_t;
     TLobbyCreated_t=record
      m_eResult:TEResult;
      m_ulSteamIDLobby:TSteamUInt64;
     end;

const LobbyCreated_t_k_iCallback=513;

type { TFavoritesListAccountsUpdated_t }
     PPFavoritesListAccountsUpdated_t=^PFavoritesListAccountsUpdated_t;
     PFavoritesListAccountsUpdated_t=^TFavoritesListAccountsUpdated_t;
     TFavoritesListAccountsUpdated_t=record
      m_eResult:TEResult;
     end;

const FavoritesListAccountsUpdated_t_k_iCallback=516;

type { TJoinPartyCallback_t }
     PPJoinPartyCallback_t=^PJoinPartyCallback_t;
     PJoinPartyCallback_t=^TJoinPartyCallback_t;
     TJoinPartyCallback_t=record
      m_eResult:TEResult;
      m_ulBeaconID:TPartyBeaconID_t;
      m_SteamIDBeaconOwner:TCSteamID;
      m_rgchConnectString:array[0..256-1] of TSteamChar;
     end;

const JoinPartyCallback_t_k_iCallback=5301;

type { TCreateBeaconCallback_t }
     PPCreateBeaconCallback_t=^PCreateBeaconCallback_t;
     PCreateBeaconCallback_t=^TCreateBeaconCallback_t;
     TCreateBeaconCallback_t=record
      m_eResult:TEResult;
      m_ulBeaconID:TPartyBeaconID_t;
     end;

const CreateBeaconCallback_t_k_iCallback=5302;

type { TReservationNotificationCallback_t }
     PPReservationNotificationCallback_t=^PReservationNotificationCallback_t;
     PReservationNotificationCallback_t=^TReservationNotificationCallback_t;
     TReservationNotificationCallback_t=record
      m_ulBeaconID:TPartyBeaconID_t;
      m_steamIDJoiner:TCSteamID;
     end;

const ReservationNotificationCallback_t_k_iCallback=5303;

type { TChangeNumOpenSlotsCallback_t }
     PPChangeNumOpenSlotsCallback_t=^PChangeNumOpenSlotsCallback_t;
     PChangeNumOpenSlotsCallback_t=^TChangeNumOpenSlotsCallback_t;
     TChangeNumOpenSlotsCallback_t=record
      m_eResult:TEResult;
     end;

const ChangeNumOpenSlotsCallback_t_k_iCallback=5304;

type { TAvailableBeaconLocationsUpdated_t }
     PPAvailableBeaconLocationsUpdated_t=^PAvailableBeaconLocationsUpdated_t;
     PAvailableBeaconLocationsUpdated_t=^TAvailableBeaconLocationsUpdated_t;
     TAvailableBeaconLocationsUpdated_t=record
      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size
     end;

const AvailableBeaconLocationsUpdated_t_k_iCallback=5305;

type { TActiveBeaconsUpdated_t }
     PPActiveBeaconsUpdated_t=^PActiveBeaconsUpdated_t;
     PActiveBeaconsUpdated_t=^TActiveBeaconsUpdated_t;
     TActiveBeaconsUpdated_t=record
      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size
     end;

const ActiveBeaconsUpdated_t_k_iCallback=5306;

type { TRemoteStorageFileShareResult_t }
     PPRemoteStorageFileShareResult_t=^PRemoteStorageFileShareResult_t;
     PRemoteStorageFileShareResult_t=^TRemoteStorageFileShareResult_t;
     TRemoteStorageFileShareResult_t=record
      m_eResult:TEResult;
      m_hFile:TUGCHandle_t;
      m_rgchFilename:array[0..260-1] of TSteamChar;
     end;

const RemoteStorageFileShareResult_t_k_iCallback=1307;

type { TRemoteStoragePublishFileResult_t }
     PPRemoteStoragePublishFileResult_t=^PRemoteStoragePublishFileResult_t;
     PRemoteStoragePublishFileResult_t=^TRemoteStoragePublishFileResult_t;
     TRemoteStoragePublishFileResult_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
      m_bUserNeedsToAcceptWorkshopLegalAgreement:TSteamBool;
     end;

const RemoteStoragePublishFileResult_t_k_iCallback=1309;

type { TRemoteStorageDeletePublishedFileResult_t }
     PPRemoteStorageDeletePublishedFileResult_t=^PRemoteStorageDeletePublishedFileResult_t;
     PRemoteStorageDeletePublishedFileResult_t=^TRemoteStorageDeletePublishedFileResult_t;
     TRemoteStorageDeletePublishedFileResult_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
     end;

const RemoteStorageDeletePublishedFileResult_t_k_iCallback=1311;

type { TRemoteStorageEnumerateUserPublishedFilesResult_t }
     PPRemoteStorageEnumerateUserPublishedFilesResult_t=^PRemoteStorageEnumerateUserPublishedFilesResult_t;
     PRemoteStorageEnumerateUserPublishedFilesResult_t=^TRemoteStorageEnumerateUserPublishedFilesResult_t;
     TRemoteStorageEnumerateUserPublishedFilesResult_t=record
      m_eResult:TEResult;
      m_nResultsReturned:TSteamInt32;
      m_nTotalResultCount:TSteamInt32;
      m_rgPublishedFileId:array[0..50-1] of TPublishedFileId_t;
     end;

const RemoteStorageEnumerateUserPublishedFilesResult_t_k_iCallback=1312;

type { TRemoteStorageSubscribePublishedFileResult_t }
     PPRemoteStorageSubscribePublishedFileResult_t=^PRemoteStorageSubscribePublishedFileResult_t;
     PRemoteStorageSubscribePublishedFileResult_t=^TRemoteStorageSubscribePublishedFileResult_t;
     TRemoteStorageSubscribePublishedFileResult_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
     end;

const RemoteStorageSubscribePublishedFileResult_t_k_iCallback=1313;

type { TRemoteStorageEnumerateUserSubscribedFilesResult_t }
     PPRemoteStorageEnumerateUserSubscribedFilesResult_t=^PRemoteStorageEnumerateUserSubscribedFilesResult_t;
     PRemoteStorageEnumerateUserSubscribedFilesResult_t=^TRemoteStorageEnumerateUserSubscribedFilesResult_t;
     TRemoteStorageEnumerateUserSubscribedFilesResult_t=record
      m_eResult:TEResult;
      m_nResultsReturned:TSteamInt32;
      m_nTotalResultCount:TSteamInt32;
      m_rgPublishedFileId:array[0..50-1] of TPublishedFileId_t;
      m_rgRTimeSubscribed:array[0..50-1] of TSteamUInt32;
     end;

const RemoteStorageEnumerateUserSubscribedFilesResult_t_k_iCallback=1314;

type { TRemoteStorageUnsubscribePublishedFileResult_t }
     PPRemoteStorageUnsubscribePublishedFileResult_t=^PRemoteStorageUnsubscribePublishedFileResult_t;
     PRemoteStorageUnsubscribePublishedFileResult_t=^TRemoteStorageUnsubscribePublishedFileResult_t;
     TRemoteStorageUnsubscribePublishedFileResult_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
     end;

const RemoteStorageUnsubscribePublishedFileResult_t_k_iCallback=1315;

type { TRemoteStorageUpdatePublishedFileResult_t }
     PPRemoteStorageUpdatePublishedFileResult_t=^PRemoteStorageUpdatePublishedFileResult_t;
     PRemoteStorageUpdatePublishedFileResult_t=^TRemoteStorageUpdatePublishedFileResult_t;
     TRemoteStorageUpdatePublishedFileResult_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
      m_bUserNeedsToAcceptWorkshopLegalAgreement:TSteamBool;
     end;

const RemoteStorageUpdatePublishedFileResult_t_k_iCallback=1316;

type { TRemoteStorageDownloadUGCResult_t }
     PPRemoteStorageDownloadUGCResult_t=^PRemoteStorageDownloadUGCResult_t;
     PRemoteStorageDownloadUGCResult_t=^TRemoteStorageDownloadUGCResult_t;
     TRemoteStorageDownloadUGCResult_t=record
      m_eResult:TEResult;
      m_hFile:TUGCHandle_t;
      m_nAppID:TAppId_t;
      m_nSizeInBytes:TSteamInt32;
      m_pchFileName:array[0..260-1] of TSteamChar;
      m_ulSteamIDOwner:TSteamUInt64;
     end;

const RemoteStorageDownloadUGCResult_t_k_iCallback=1317;

type { TRemoteStorageGetPublishedFileDetailsResult_t }
     PPRemoteStorageGetPublishedFileDetailsResult_t=^PRemoteStorageGetPublishedFileDetailsResult_t;
     PRemoteStorageGetPublishedFileDetailsResult_t=^TRemoteStorageGetPublishedFileDetailsResult_t;
     TRemoteStorageGetPublishedFileDetailsResult_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
      m_nCreatorAppID:TAppId_t;
      m_nConsumerAppID:TAppId_t;
      m_rgchTitle:array[0..129-1] of TSteamChar;
      m_rgchDescription:array[0..8000-1] of TSteamChar;
      m_hFile:TUGCHandle_t;
      m_hPreviewFile:TUGCHandle_t;
      m_ulSteamIDOwner:TSteamUInt64;
      m_rtimeCreated:TSteamUInt32;
      m_rtimeUpdated:TSteamUInt32;
      m_eVisibility:TERemoteStoragePublishedFileVisibility;
      m_bBanned:TSteamBool;
      m_rgchTags:array[0..1025-1] of TSteamChar;
      m_bTagsTruncated:TSteamBool;
      m_pchFileName:array[0..260-1] of TSteamChar;
      m_nFileSize:TSteamInt32;
      m_nPreviewFileSize:TSteamInt32;
      m_rgchURL:array[0..256-1] of TSteamChar;
      m_eFileType:TEWorkshopFileType;
      m_bAcceptedForUse:TSteamBool;
     end;

const RemoteStorageGetPublishedFileDetailsResult_t_k_iCallback=1318;

type { TRemoteStorageEnumerateWorkshopFilesResult_t }
     PPRemoteStorageEnumerateWorkshopFilesResult_t=^PRemoteStorageEnumerateWorkshopFilesResult_t;
     PRemoteStorageEnumerateWorkshopFilesResult_t=^TRemoteStorageEnumerateWorkshopFilesResult_t;
     TRemoteStorageEnumerateWorkshopFilesResult_t=record
      m_eResult:TEResult;
      m_nResultsReturned:TSteamInt32;
      m_nTotalResultCount:TSteamInt32;
      m_rgPublishedFileId:array[0..50-1] of TPublishedFileId_t;
      m_rgScore:array[0..50-1] of TSteamFloat;
      m_nAppId:TAppId_t;
      m_unStartIndex:TSteamUInt32;
     end;

const RemoteStorageEnumerateWorkshopFilesResult_t_k_iCallback=1319;

type { TRemoteStorageGetPublishedItemVoteDetailsResult_t }
     PPRemoteStorageGetPublishedItemVoteDetailsResult_t=^PRemoteStorageGetPublishedItemVoteDetailsResult_t;
     PRemoteStorageGetPublishedItemVoteDetailsResult_t=^TRemoteStorageGetPublishedItemVoteDetailsResult_t;
     TRemoteStorageGetPublishedItemVoteDetailsResult_t=record
      m_eResult:TEResult;
      m_unPublishedFileId:TPublishedFileId_t;
      m_nVotesFor:TSteamInt32;
      m_nVotesAgainst:TSteamInt32;
      m_nReports:TSteamInt32;
      m_fScore:TSteamFloat;
     end;

const RemoteStorageGetPublishedItemVoteDetailsResult_t_k_iCallback=1320;

type { TRemoteStoragePublishedFileSubscribed_t }
     PPRemoteStoragePublishedFileSubscribed_t=^PRemoteStoragePublishedFileSubscribed_t;
     PRemoteStoragePublishedFileSubscribed_t=^TRemoteStoragePublishedFileSubscribed_t;
     TRemoteStoragePublishedFileSubscribed_t=record
      m_nPublishedFileId:TPublishedFileId_t;
      m_nAppID:TAppId_t;
     end;

const RemoteStoragePublishedFileSubscribed_t_k_iCallback=1321;

type { TRemoteStoragePublishedFileUnsubscribed_t }
     PPRemoteStoragePublishedFileUnsubscribed_t=^PRemoteStoragePublishedFileUnsubscribed_t;
     PRemoteStoragePublishedFileUnsubscribed_t=^TRemoteStoragePublishedFileUnsubscribed_t;
     TRemoteStoragePublishedFileUnsubscribed_t=record
      m_nPublishedFileId:TPublishedFileId_t;
      m_nAppID:TAppId_t;
     end;

const RemoteStoragePublishedFileUnsubscribed_t_k_iCallback=1322;

type { TRemoteStoragePublishedFileDeleted_t }
     PPRemoteStoragePublishedFileDeleted_t=^PRemoteStoragePublishedFileDeleted_t;
     PRemoteStoragePublishedFileDeleted_t=^TRemoteStoragePublishedFileDeleted_t;
     TRemoteStoragePublishedFileDeleted_t=record
      m_nPublishedFileId:TPublishedFileId_t;
      m_nAppID:TAppId_t;
     end;

const RemoteStoragePublishedFileDeleted_t_k_iCallback=1323;

type { TRemoteStorageUpdateUserPublishedItemVoteResult_t }
     PPRemoteStorageUpdateUserPublishedItemVoteResult_t=^PRemoteStorageUpdateUserPublishedItemVoteResult_t;
     PRemoteStorageUpdateUserPublishedItemVoteResult_t=^TRemoteStorageUpdateUserPublishedItemVoteResult_t;
     TRemoteStorageUpdateUserPublishedItemVoteResult_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
     end;

const RemoteStorageUpdateUserPublishedItemVoteResult_t_k_iCallback=1324;

type { TRemoteStorageUserVoteDetails_t }
     PPRemoteStorageUserVoteDetails_t=^PRemoteStorageUserVoteDetails_t;
     PRemoteStorageUserVoteDetails_t=^TRemoteStorageUserVoteDetails_t;
     TRemoteStorageUserVoteDetails_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
      m_eVote:TEWorkshopVote;
     end;

const RemoteStorageUserVoteDetails_t_k_iCallback=1325;

type { TRemoteStorageEnumerateUserSharedWorkshopFilesResult_t }
     PPRemoteStorageEnumerateUserSharedWorkshopFilesResult_t=^PRemoteStorageEnumerateUserSharedWorkshopFilesResult_t;
     PRemoteStorageEnumerateUserSharedWorkshopFilesResult_t=^TRemoteStorageEnumerateUserSharedWorkshopFilesResult_t;
     TRemoteStorageEnumerateUserSharedWorkshopFilesResult_t=record
      m_eResult:TEResult;
      m_nResultsReturned:TSteamInt32;
      m_nTotalResultCount:TSteamInt32;
      m_rgPublishedFileId:array[0..50-1] of TPublishedFileId_t;
     end;

const RemoteStorageEnumerateUserSharedWorkshopFilesResult_t_k_iCallback=1326;

type { TRemoteStorageSetUserPublishedFileActionResult_t }
     PPRemoteStorageSetUserPublishedFileActionResult_t=^PRemoteStorageSetUserPublishedFileActionResult_t;
     PRemoteStorageSetUserPublishedFileActionResult_t=^TRemoteStorageSetUserPublishedFileActionResult_t;
     TRemoteStorageSetUserPublishedFileActionResult_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
      m_eAction:TEWorkshopFileAction;
     end;

const RemoteStorageSetUserPublishedFileActionResult_t_k_iCallback=1327;

type { TRemoteStorageEnumeratePublishedFilesByUserActionResult_t }
     PPRemoteStorageEnumeratePublishedFilesByUserActionResult_t=^PRemoteStorageEnumeratePublishedFilesByUserActionResult_t;
     PRemoteStorageEnumeratePublishedFilesByUserActionResult_t=^TRemoteStorageEnumeratePublishedFilesByUserActionResult_t;
     TRemoteStorageEnumeratePublishedFilesByUserActionResult_t=record
      m_eResult:TEResult;
      m_eAction:TEWorkshopFileAction;
      m_nResultsReturned:TSteamInt32;
      m_nTotalResultCount:TSteamInt32;
      m_rgPublishedFileId:array[0..50-1] of TPublishedFileId_t;
      m_rgRTimeUpdated:array[0..50-1] of TSteamUInt32;
     end;

const RemoteStorageEnumeratePublishedFilesByUserActionResult_t_k_iCallback=1328;

type { TRemoteStoragePublishFileProgress_t }
     PPRemoteStoragePublishFileProgress_t=^PRemoteStoragePublishFileProgress_t;
     PRemoteStoragePublishFileProgress_t=^TRemoteStoragePublishFileProgress_t;
     TRemoteStoragePublishFileProgress_t=record
      m_dPercentFile:TSteamDouble;
      m_bPreview:TSteamBool;
     end;

const RemoteStoragePublishFileProgress_t_k_iCallback=1329;

type { TRemoteStoragePublishedFileUpdated_t }
     PPRemoteStoragePublishedFileUpdated_t=^PRemoteStoragePublishedFileUpdated_t;
     PRemoteStoragePublishedFileUpdated_t=^TRemoteStoragePublishedFileUpdated_t;
     TRemoteStoragePublishedFileUpdated_t=record
      m_nPublishedFileId:TPublishedFileId_t;
      m_nAppID:TAppId_t;
      m_ulUnused:TSteamUInt64;
     end;

const RemoteStoragePublishedFileUpdated_t_k_iCallback=1330;

type { TRemoteStorageFileWriteAsyncComplete_t }
     PPRemoteStorageFileWriteAsyncComplete_t=^PRemoteStorageFileWriteAsyncComplete_t;
     PRemoteStorageFileWriteAsyncComplete_t=^TRemoteStorageFileWriteAsyncComplete_t;
     TRemoteStorageFileWriteAsyncComplete_t=record
      m_eResult:TEResult;
     end;

const RemoteStorageFileWriteAsyncComplete_t_k_iCallback=1331;

type { TRemoteStorageFileReadAsyncComplete_t }
     PPRemoteStorageFileReadAsyncComplete_t=^PRemoteStorageFileReadAsyncComplete_t;
     PRemoteStorageFileReadAsyncComplete_t=^TRemoteStorageFileReadAsyncComplete_t;
     TRemoteStorageFileReadAsyncComplete_t=record
      m_hFileReadAsync:TSteamAPICall_t;
      m_eResult:TEResult;
      m_nOffset:TSteamUInt32;
      m_cubRead:TSteamUInt32;
     end;

const RemoteStorageFileReadAsyncComplete_t_k_iCallback=1332;

type { TRemoteStorageLocalFileChange_t }
     PPRemoteStorageLocalFileChange_t=^PRemoteStorageLocalFileChange_t;
     PRemoteStorageLocalFileChange_t=^TRemoteStorageLocalFileChange_t;
     TRemoteStorageLocalFileChange_t=record
      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size
     end;

const RemoteStorageLocalFileChange_t_k_iCallback=1333;

type { TUserStatsReceived_t }
     PPUserStatsReceived_t=^PUserStatsReceived_t;
     PUserStatsReceived_t=^TUserStatsReceived_t;
     TUserStatsReceived_t=record
      m_nGameID:TSteamUInt64;
      m_eResult:TEResult;
      m_steamIDUser:TCSteamID;
     end;

const UserStatsReceived_t_k_iCallback=1101;

type { TUserStatsStored_t }
     PPUserStatsStored_t=^PUserStatsStored_t;
     PUserStatsStored_t=^TUserStatsStored_t;
     TUserStatsStored_t=record
      m_nGameID:TSteamUInt64;
      m_eResult:TEResult;
     end;

const UserStatsStored_t_k_iCallback=1102;

type { TUserAchievementStored_t }
     PPUserAchievementStored_t=^PUserAchievementStored_t;
     PUserAchievementStored_t=^TUserAchievementStored_t;
     TUserAchievementStored_t=record
      m_nGameID:TSteamUInt64;
      m_bGroupAchievement:TSteamBool;
      m_rgchAchievementName:array[0..128-1] of TSteamChar;
      m_nCurProgress:TSteamUInt32;
      m_nMaxProgress:TSteamUInt32;
     end;

const UserAchievementStored_t_k_iCallback=1103;

type { TLeaderboardFindResult_t }
     PPLeaderboardFindResult_t=^PLeaderboardFindResult_t;
     PLeaderboardFindResult_t=^TLeaderboardFindResult_t;
     TLeaderboardFindResult_t=record
      m_hSteamLeaderboard:TSteamLeaderboard_t;
      m_bLeaderboardFound:TSteamUInt8;
     end;

const LeaderboardFindResult_t_k_iCallback=1104;

type { TLeaderboardScoresDownloaded_t }
     PPLeaderboardScoresDownloaded_t=^PLeaderboardScoresDownloaded_t;
     PLeaderboardScoresDownloaded_t=^TLeaderboardScoresDownloaded_t;
     TLeaderboardScoresDownloaded_t=record
      m_hSteamLeaderboard:TSteamLeaderboard_t;
      m_hSteamLeaderboardEntries:TSteamLeaderboardEntries_t;
      m_cEntryCount:TSteamInt32;
     end;

const LeaderboardScoresDownloaded_t_k_iCallback=1105;

type { TLeaderboardScoreUploaded_t }
     PPLeaderboardScoreUploaded_t=^PLeaderboardScoreUploaded_t;
     PLeaderboardScoreUploaded_t=^TLeaderboardScoreUploaded_t;
     TLeaderboardScoreUploaded_t=record
      m_bSuccess:TSteamUInt8;
      m_hSteamLeaderboard:TSteamLeaderboard_t;
      m_nScore:TSteamInt32;
      m_bScoreChanged:TSteamUInt8;
      m_nGlobalRankNew:TSteamInt32;
      m_nGlobalRankPrevious:TSteamInt32;
     end;

const LeaderboardScoreUploaded_t_k_iCallback=1106;

type { TNumberOfCurrentPlayers_t }
     PPNumberOfCurrentPlayers_t=^PNumberOfCurrentPlayers_t;
     PNumberOfCurrentPlayers_t=^TNumberOfCurrentPlayers_t;
     TNumberOfCurrentPlayers_t=record
      m_bSuccess:TSteamUInt8;
      m_cPlayers:TSteamInt32;
     end;

const NumberOfCurrentPlayers_t_k_iCallback=1107;

type { TUserStatsUnloaded_t }
     PPUserStatsUnloaded_t=^PUserStatsUnloaded_t;
     PUserStatsUnloaded_t=^TUserStatsUnloaded_t;
     TUserStatsUnloaded_t=record
      m_steamIDUser:TCSteamID;
     end;

const UserStatsUnloaded_t_k_iCallback=1108;

type { TUserAchievementIconFetched_t }
     PPUserAchievementIconFetched_t=^PUserAchievementIconFetched_t;
     PUserAchievementIconFetched_t=^TUserAchievementIconFetched_t;
     TUserAchievementIconFetched_t=record
      m_nGameID:TCGameID;
      m_rgchAchievementName:array[0..128-1] of TSteamChar;
      m_bAchieved:TSteamBool;
      m_nIconHandle:TSteamInt32;
     end;

const UserAchievementIconFetched_t_k_iCallback=1109;

type { TGlobalAchievementPercentagesReady_t }
     PPGlobalAchievementPercentagesReady_t=^PGlobalAchievementPercentagesReady_t;
     PGlobalAchievementPercentagesReady_t=^TGlobalAchievementPercentagesReady_t;
     TGlobalAchievementPercentagesReady_t=record
      m_nGameID:TSteamUInt64;
      m_eResult:TEResult;
     end;

const GlobalAchievementPercentagesReady_t_k_iCallback=1110;

type { TLeaderboardUGCSet_t }
     PPLeaderboardUGCSet_t=^PLeaderboardUGCSet_t;
     PLeaderboardUGCSet_t=^TLeaderboardUGCSet_t;
     TLeaderboardUGCSet_t=record
      m_eResult:TEResult;
      m_hSteamLeaderboard:TSteamLeaderboard_t;
     end;

const LeaderboardUGCSet_t_k_iCallback=1111;

type { TGlobalStatsReceived_t }
     PPGlobalStatsReceived_t=^PGlobalStatsReceived_t;
     PGlobalStatsReceived_t=^TGlobalStatsReceived_t;
     TGlobalStatsReceived_t=record
      m_nGameID:TSteamUInt64;
      m_eResult:TEResult;
     end;

const GlobalStatsReceived_t_k_iCallback=1112;

type { TDlcInstalled_t }
     PPDlcInstalled_t=^PDlcInstalled_t;
     PDlcInstalled_t=^TDlcInstalled_t;
     TDlcInstalled_t=record
      m_nAppID:TAppId_t;
     end;

const DlcInstalled_t_k_iCallback=1005;

type { TNewUrlLaunchParameters_t }
     PPNewUrlLaunchParameters_t=^PNewUrlLaunchParameters_t;
     PNewUrlLaunchParameters_t=^TNewUrlLaunchParameters_t;
     TNewUrlLaunchParameters_t=record
      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size
     end;

const NewUrlLaunchParameters_t_k_iCallback=1014;

type { TAppProofOfPurchaseKeyResponse_t }
     PPAppProofOfPurchaseKeyResponse_t=^PAppProofOfPurchaseKeyResponse_t;
     PAppProofOfPurchaseKeyResponse_t=^TAppProofOfPurchaseKeyResponse_t;
     TAppProofOfPurchaseKeyResponse_t=record
      m_eResult:TEResult;
      m_nAppID:TSteamUInt32;
      m_cchKeyLength:TSteamUInt32;
      m_rgchKey:array[0..240-1] of TSteamChar;
     end;

const AppProofOfPurchaseKeyResponse_t_k_iCallback=1021;

type { TFileDetailsResult_t }
     PPFileDetailsResult_t=^PFileDetailsResult_t;
     PFileDetailsResult_t=^TFileDetailsResult_t;
     TFileDetailsResult_t=record
      m_eResult:TEResult;
      m_ulFileSize:TSteamUInt64;
      m_FileSHA:array[0..20-1] of TSteamUInt8;
      m_unFlags:TSteamUInt32;
     end;

const FileDetailsResult_t_k_iCallback=1023;

type { TTimedTrialStatus_t }
     PPTimedTrialStatus_t=^PTimedTrialStatus_t;
     PTimedTrialStatus_t=^TTimedTrialStatus_t;
     TTimedTrialStatus_t=record
      m_unAppID:TAppId_t;
      m_bIsOffline:TSteamBool;
      m_unSecondsAllowed:TSteamUInt32;
      m_unSecondsPlayed:TSteamUInt32;
     end;

const TimedTrialStatus_t_k_iCallback=1030;

type { TP2PSessionRequest_t }
     PPP2PSessionRequest_t=^PP2PSessionRequest_t;
     PP2PSessionRequest_t=^TP2PSessionRequest_t;
     TP2PSessionRequest_t=record
      m_steamIDRemote:TCSteamID;
     end;

const P2PSessionRequest_t_k_iCallback=1202;

type { TP2PSessionConnectFail_t }
     PPP2PSessionConnectFail_t=^PP2PSessionConnectFail_t;
     PP2PSessionConnectFail_t=^TP2PSessionConnectFail_t;
     TP2PSessionConnectFail_t=record
      m_steamIDRemote:TCSteamID;
      m_eP2PSessionError:TSteamUInt8;
     end;

const P2PSessionConnectFail_t_k_iCallback=1203;

type { TSocketStatusCallback_t }
     PPSocketStatusCallback_t=^PSocketStatusCallback_t;
     PSocketStatusCallback_t=^TSocketStatusCallback_t;
     TSocketStatusCallback_t=record
      m_hSocket:TSNetSocket_t;
      m_hListenSocket:TSNetListenSocket_t;
      m_steamIDRemote:TCSteamID;
      m_eSNetSocketState:TSteamInt32;
     end;

const SocketStatusCallback_t_k_iCallback=1201;

type { TScreenshotReady_t }
     PPScreenshotReady_t=^PScreenshotReady_t;
     PScreenshotReady_t=^TScreenshotReady_t;
     TScreenshotReady_t=record
      m_hLocal:TScreenshotHandle;
      m_eResult:TEResult;
     end;

const ScreenshotReady_t_k_iCallback=2301;

type { TScreenshotRequested_t }
     PPScreenshotRequested_t=^PScreenshotRequested_t;
     PScreenshotRequested_t=^TScreenshotRequested_t;
     TScreenshotRequested_t=record
      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size
     end;

const ScreenshotRequested_t_k_iCallback=2302;

type { TPlaybackStatusHasChanged_t }
     PPPlaybackStatusHasChanged_t=^PPlaybackStatusHasChanged_t;
     PPlaybackStatusHasChanged_t=^TPlaybackStatusHasChanged_t;
     TPlaybackStatusHasChanged_t=record
      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size
     end;

const PlaybackStatusHasChanged_t_k_iCallback=4001;

type { TVolumeHasChanged_t }
     PPVolumeHasChanged_t=^PVolumeHasChanged_t;
     PVolumeHasChanged_t=^TVolumeHasChanged_t;
     TVolumeHasChanged_t=record
      m_flNewVolume:TSteamFloat;
     end;

const VolumeHasChanged_t_k_iCallback=4002;

type { THTTPRequestCompleted_t }
     PPHTTPRequestCompleted_t=^PHTTPRequestCompleted_t;
     PHTTPRequestCompleted_t=^THTTPRequestCompleted_t;
     THTTPRequestCompleted_t=record
      m_hRequest:THTTPRequestHandle;
      m_ulContextValue:TSteamUInt64;
      m_bRequestSuccessful:TSteamBool;
      m_eStatusCode:TEHTTPStatusCode;
      m_unBodySize:TSteamUInt32;
     end;

const HTTPRequestCompleted_t_k_iCallback=2101;

type { THTTPRequestHeadersReceived_t }
     PPHTTPRequestHeadersReceived_t=^PHTTPRequestHeadersReceived_t;
     PHTTPRequestHeadersReceived_t=^THTTPRequestHeadersReceived_t;
     THTTPRequestHeadersReceived_t=record
      m_hRequest:THTTPRequestHandle;
      m_ulContextValue:TSteamUInt64;
     end;

const HTTPRequestHeadersReceived_t_k_iCallback=2102;

type { THTTPRequestDataReceived_t }
     PPHTTPRequestDataReceived_t=^PHTTPRequestDataReceived_t;
     PHTTPRequestDataReceived_t=^THTTPRequestDataReceived_t;
     THTTPRequestDataReceived_t=record
      m_hRequest:THTTPRequestHandle;
      m_ulContextValue:TSteamUInt64;
      m_cOffset:TSteamUInt32;
      m_cBytesReceived:TSteamUInt32;
     end;

const HTTPRequestDataReceived_t_k_iCallback=2103;

type { TSteamInputDeviceConnected_t }
     PPSteamInputDeviceConnected_t=^PSteamInputDeviceConnected_t;
     PSteamInputDeviceConnected_t=^TSteamInputDeviceConnected_t;
     TSteamInputDeviceConnected_t=record
      m_ulConnectedDeviceHandle:TInputHandle_t;
     end;

const SteamInputDeviceConnected_t_k_iCallback=2801;

type { TSteamInputDeviceDisconnected_t }
     PPSteamInputDeviceDisconnected_t=^PSteamInputDeviceDisconnected_t;
     PSteamInputDeviceDisconnected_t=^TSteamInputDeviceDisconnected_t;
     TSteamInputDeviceDisconnected_t=record
      m_ulDisconnectedDeviceHandle:TInputHandle_t;
     end;

const SteamInputDeviceDisconnected_t_k_iCallback=2802;

type { TSteamInputConfigurationLoaded_t }
     PPSteamInputConfigurationLoaded_t=^PSteamInputConfigurationLoaded_t;
     PSteamInputConfigurationLoaded_t=^TSteamInputConfigurationLoaded_t;
     TSteamInputConfigurationLoaded_t=record
      m_unAppID:TAppId_t;
      m_ulDeviceHandle:TInputHandle_t;
      m_ulMappingCreator:TCSteamID;
      m_unMajorRevision:TSteamUInt32;
      m_unMinorRevision:TSteamUInt32;
      m_bUsesSteamInputAPI:TSteamBool;
      m_bUsesGamepadAPI:TSteamBool;
     end;

const SteamInputConfigurationLoaded_t_k_iCallback=2803;

type { TSteamInputGamepadSlotChange_t }
     PPSteamInputGamepadSlotChange_t=^PSteamInputGamepadSlotChange_t;
     PSteamInputGamepadSlotChange_t=^TSteamInputGamepadSlotChange_t;
     TSteamInputGamepadSlotChange_t=record
      m_unAppID:TAppId_t;
      m_ulDeviceHandle:TInputHandle_t;
      m_eDeviceType:TESteamInputType;
      m_nOldGamepadSlot:TSteamInt32;
      m_nNewGamepadSlot:TSteamInt32;
     end;

const SteamInputGamepadSlotChange_t_k_iCallback=2804;

type { TSteamUGCQueryCompleted_t }
     PPSteamUGCQueryCompleted_t=^PSteamUGCQueryCompleted_t;
     PSteamUGCQueryCompleted_t=^TSteamUGCQueryCompleted_t;
     TSteamUGCQueryCompleted_t=record
      m_handle:TUGCQueryHandle_t;
      m_eResult:TEResult;
      m_unNumResultsReturned:TSteamUInt32;
      m_unTotalMatchingResults:TSteamUInt32;
      m_bCachedData:TSteamBool;
      m_rgchNextCursor:array[0..256-1] of TSteamChar;
     end;

const SteamUGCQueryCompleted_t_k_iCallback=3401;

type { TSteamUGCRequestUGCDetailsResult_t }
     PPSteamUGCRequestUGCDetailsResult_t=^PSteamUGCRequestUGCDetailsResult_t;
     PSteamUGCRequestUGCDetailsResult_t=^TSteamUGCRequestUGCDetailsResult_t;
     TSteamUGCRequestUGCDetailsResult_t=record
      m_details:TSteamUGCDetails_t;
      m_bCachedData:TSteamBool;
     end;

const SteamUGCRequestUGCDetailsResult_t_k_iCallback=3402;

type { TCreateItemResult_t }
     PPCreateItemResult_t=^PCreateItemResult_t;
     PCreateItemResult_t=^TCreateItemResult_t;
     TCreateItemResult_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
      m_bUserNeedsToAcceptWorkshopLegalAgreement:TSteamBool;
     end;

const CreateItemResult_t_k_iCallback=3403;

type { TSubmitItemUpdateResult_t }
     PPSubmitItemUpdateResult_t=^PSubmitItemUpdateResult_t;
     PSubmitItemUpdateResult_t=^TSubmitItemUpdateResult_t;
     TSubmitItemUpdateResult_t=record
      m_eResult:TEResult;
      m_bUserNeedsToAcceptWorkshopLegalAgreement:TSteamBool;
      m_nPublishedFileId:TPublishedFileId_t;
     end;

const SubmitItemUpdateResult_t_k_iCallback=3404;

type { TItemInstalled_t }
     PPItemInstalled_t=^PItemInstalled_t;
     PItemInstalled_t=^TItemInstalled_t;
     TItemInstalled_t=record
      m_unAppID:TAppId_t;
      m_nPublishedFileId:TPublishedFileId_t;
      m_hLegacyContent:TUGCHandle_t;
      m_unManifestID:TSteamUInt64;
     end;

const ItemInstalled_t_k_iCallback=3405;

type { TDownloadItemResult_t }
     PPDownloadItemResult_t=^PDownloadItemResult_t;
     PDownloadItemResult_t=^TDownloadItemResult_t;
     TDownloadItemResult_t=record
      m_unAppID:TAppId_t;
      m_nPublishedFileId:TPublishedFileId_t;
      m_eResult:TEResult;
     end;

const DownloadItemResult_t_k_iCallback=3406;

type { TUserFavoriteItemsListChanged_t }
     PPUserFavoriteItemsListChanged_t=^PUserFavoriteItemsListChanged_t;
     PUserFavoriteItemsListChanged_t=^TUserFavoriteItemsListChanged_t;
     TUserFavoriteItemsListChanged_t=record
      m_nPublishedFileId:TPublishedFileId_t;
      m_eResult:TEResult;
      m_bWasAddRequest:TSteamBool;
     end;

const UserFavoriteItemsListChanged_t_k_iCallback=3407;

type { TSetUserItemVoteResult_t }
     PPSetUserItemVoteResult_t=^PSetUserItemVoteResult_t;
     PSetUserItemVoteResult_t=^TSetUserItemVoteResult_t;
     TSetUserItemVoteResult_t=record
      m_nPublishedFileId:TPublishedFileId_t;
      m_eResult:TEResult;
      m_bVoteUp:TSteamBool;
     end;

const SetUserItemVoteResult_t_k_iCallback=3408;

type { TGetUserItemVoteResult_t }
     PPGetUserItemVoteResult_t=^PGetUserItemVoteResult_t;
     PGetUserItemVoteResult_t=^TGetUserItemVoteResult_t;
     TGetUserItemVoteResult_t=record
      m_nPublishedFileId:TPublishedFileId_t;
      m_eResult:TEResult;
      m_bVotedUp:TSteamBool;
      m_bVotedDown:TSteamBool;
      m_bVoteSkipped:TSteamBool;
     end;

const GetUserItemVoteResult_t_k_iCallback=3409;

type { TStartPlaytimeTrackingResult_t }
     PPStartPlaytimeTrackingResult_t=^PStartPlaytimeTrackingResult_t;
     PStartPlaytimeTrackingResult_t=^TStartPlaytimeTrackingResult_t;
     TStartPlaytimeTrackingResult_t=record
      m_eResult:TEResult;
     end;

const StartPlaytimeTrackingResult_t_k_iCallback=3410;

type { TStopPlaytimeTrackingResult_t }
     PPStopPlaytimeTrackingResult_t=^PStopPlaytimeTrackingResult_t;
     PStopPlaytimeTrackingResult_t=^TStopPlaytimeTrackingResult_t;
     TStopPlaytimeTrackingResult_t=record
      m_eResult:TEResult;
     end;

const StopPlaytimeTrackingResult_t_k_iCallback=3411;

type { TAddUGCDependencyResult_t }
     PPAddUGCDependencyResult_t=^PAddUGCDependencyResult_t;
     PAddUGCDependencyResult_t=^TAddUGCDependencyResult_t;
     TAddUGCDependencyResult_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
      m_nChildPublishedFileId:TPublishedFileId_t;
     end;

const AddUGCDependencyResult_t_k_iCallback=3412;

type { TRemoveUGCDependencyResult_t }
     PPRemoveUGCDependencyResult_t=^PRemoveUGCDependencyResult_t;
     PRemoveUGCDependencyResult_t=^TRemoveUGCDependencyResult_t;
     TRemoveUGCDependencyResult_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
      m_nChildPublishedFileId:TPublishedFileId_t;
     end;

const RemoveUGCDependencyResult_t_k_iCallback=3413;

type { TAddAppDependencyResult_t }
     PPAddAppDependencyResult_t=^PAddAppDependencyResult_t;
     PAddAppDependencyResult_t=^TAddAppDependencyResult_t;
     TAddAppDependencyResult_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
      m_nAppID:TAppId_t;
     end;

const AddAppDependencyResult_t_k_iCallback=3414;

type { TRemoveAppDependencyResult_t }
     PPRemoveAppDependencyResult_t=^PRemoveAppDependencyResult_t;
     PRemoveAppDependencyResult_t=^TRemoveAppDependencyResult_t;
     TRemoveAppDependencyResult_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
      m_nAppID:TAppId_t;
     end;

const RemoveAppDependencyResult_t_k_iCallback=3415;

type { TGetAppDependenciesResult_t }
     PPGetAppDependenciesResult_t=^PGetAppDependenciesResult_t;
     PGetAppDependenciesResult_t=^TGetAppDependenciesResult_t;
     TGetAppDependenciesResult_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
      m_rgAppIDs:array[0..32-1] of TAppId_t;
      m_nNumAppDependencies:TSteamUInt32;
      m_nTotalNumAppDependencies:TSteamUInt32;
     end;

const GetAppDependenciesResult_t_k_iCallback=3416;

type { TDeleteItemResult_t }
     PPDeleteItemResult_t=^PDeleteItemResult_t;
     PDeleteItemResult_t=^TDeleteItemResult_t;
     TDeleteItemResult_t=record
      m_eResult:TEResult;
      m_nPublishedFileId:TPublishedFileId_t;
     end;

const DeleteItemResult_t_k_iCallback=3417;

type { TUserSubscribedItemsListChanged_t }
     PPUserSubscribedItemsListChanged_t=^PUserSubscribedItemsListChanged_t;
     PUserSubscribedItemsListChanged_t=^TUserSubscribedItemsListChanged_t;
     TUserSubscribedItemsListChanged_t=record
      m_nAppID:TAppId_t;
     end;

const UserSubscribedItemsListChanged_t_k_iCallback=3418;

type { TWorkshopEULAStatus_t }
     PPWorkshopEULAStatus_t=^PWorkshopEULAStatus_t;
     PWorkshopEULAStatus_t=^TWorkshopEULAStatus_t;
     TWorkshopEULAStatus_t=record
      m_eResult:TEResult;
      m_nAppID:TAppId_t;
      m_unVersion:TSteamUInt32;
      m_rtAction:TRTime32;
      m_bAccepted:TSteamBool;
      m_bNeedsAction:TSteamBool;
     end;

const WorkshopEULAStatus_t_k_iCallback=3420;

type { THTML_BrowserReady_t }
     PPHTML_BrowserReady_t=^PHTML_BrowserReady_t;
     PHTML_BrowserReady_t=^THTML_BrowserReady_t;
     THTML_BrowserReady_t=record
      unBrowserHandle:THHTMLBrowser;
     end;

const HTML_BrowserReady_t_k_iCallback=4501;

type { THTML_NeedsPaint_t }
     PPHTML_NeedsPaint_t=^PHTML_NeedsPaint_t;
     PHTML_NeedsPaint_t=^THTML_NeedsPaint_t;
     THTML_NeedsPaint_t=record
      unBrowserHandle:THHTMLBrowser;
      pBGRA:PSteamChar;
      unWide:TSteamUInt32;
      unTall:TSteamUInt32;
      unUpdateX:TSteamUInt32;
      unUpdateY:TSteamUInt32;
      unUpdateWide:TSteamUInt32;
      unUpdateTall:TSteamUInt32;
      unScrollX:TSteamUInt32;
      unScrollY:TSteamUInt32;
      flPageScale:TSteamFloat;
      unPageSerial:TSteamUInt32;
     end;

const HTML_NeedsPaint_t_k_iCallback=4502;

type { THTML_StartRequest_t }
     PPHTML_StartRequest_t=^PHTML_StartRequest_t;
     PHTML_StartRequest_t=^THTML_StartRequest_t;
     THTML_StartRequest_t=record
      unBrowserHandle:THHTMLBrowser;
      pchURL:PSteamChar;
      pchTarget:PSteamChar;
      pchPostData:PSteamChar;
      bIsRedirect:TSteamBool;
     end;

const HTML_StartRequest_t_k_iCallback=4503;

type { THTML_CloseBrowser_t }
     PPHTML_CloseBrowser_t=^PHTML_CloseBrowser_t;
     PHTML_CloseBrowser_t=^THTML_CloseBrowser_t;
     THTML_CloseBrowser_t=record
      unBrowserHandle:THHTMLBrowser;
     end;

const HTML_CloseBrowser_t_k_iCallback=4504;

type { THTML_URLChanged_t }
     PPHTML_URLChanged_t=^PHTML_URLChanged_t;
     PHTML_URLChanged_t=^THTML_URLChanged_t;
     THTML_URLChanged_t=record
      unBrowserHandle:THHTMLBrowser;
      pchURL:PSteamChar;
      pchPostData:PSteamChar;
      bIsRedirect:TSteamBool;
      pchPageTitle:PSteamChar;
      bNewNavigation:TSteamBool;
     end;

const HTML_URLChanged_t_k_iCallback=4505;

type { THTML_FinishedRequest_t }
     PPHTML_FinishedRequest_t=^PHTML_FinishedRequest_t;
     PHTML_FinishedRequest_t=^THTML_FinishedRequest_t;
     THTML_FinishedRequest_t=record
      unBrowserHandle:THHTMLBrowser;
      pchURL:PSteamChar;
      pchPageTitle:PSteamChar;
     end;

const HTML_FinishedRequest_t_k_iCallback=4506;

type { THTML_OpenLinkInNewTab_t }
     PPHTML_OpenLinkInNewTab_t=^PHTML_OpenLinkInNewTab_t;
     PHTML_OpenLinkInNewTab_t=^THTML_OpenLinkInNewTab_t;
     THTML_OpenLinkInNewTab_t=record
      unBrowserHandle:THHTMLBrowser;
      pchURL:PSteamChar;
     end;

const HTML_OpenLinkInNewTab_t_k_iCallback=4507;

type { THTML_ChangedTitle_t }
     PPHTML_ChangedTitle_t=^PHTML_ChangedTitle_t;
     PHTML_ChangedTitle_t=^THTML_ChangedTitle_t;
     THTML_ChangedTitle_t=record
      unBrowserHandle:THHTMLBrowser;
      pchTitle:PSteamChar;
     end;

const HTML_ChangedTitle_t_k_iCallback=4508;

type { THTML_SearchResults_t }
     PPHTML_SearchResults_t=^PHTML_SearchResults_t;
     PHTML_SearchResults_t=^THTML_SearchResults_t;
     THTML_SearchResults_t=record
      unBrowserHandle:THHTMLBrowser;
      unResults:TSteamUInt32;
      unCurrentMatch:TSteamUInt32;
     end;

const HTML_SearchResults_t_k_iCallback=4509;

type { THTML_CanGoBackAndForward_t }
     PPHTML_CanGoBackAndForward_t=^PHTML_CanGoBackAndForward_t;
     PHTML_CanGoBackAndForward_t=^THTML_CanGoBackAndForward_t;
     THTML_CanGoBackAndForward_t=record
      unBrowserHandle:THHTMLBrowser;
      bCanGoBack:TSteamBool;
      bCanGoForward:TSteamBool;
     end;

const HTML_CanGoBackAndForward_t_k_iCallback=4510;

type { THTML_HorizontalScroll_t }
     PPHTML_HorizontalScroll_t=^PHTML_HorizontalScroll_t;
     PHTML_HorizontalScroll_t=^THTML_HorizontalScroll_t;
     THTML_HorizontalScroll_t=record
      unBrowserHandle:THHTMLBrowser;
      unScrollMax:TSteamUInt32;
      unScrollCurrent:TSteamUInt32;
      flPageScale:TSteamFloat;
      bVisible:TSteamBool;
      unPageSize:TSteamUInt32;
     end;

const HTML_HorizontalScroll_t_k_iCallback=4511;

type { THTML_VerticalScroll_t }
     PPHTML_VerticalScroll_t=^PHTML_VerticalScroll_t;
     PHTML_VerticalScroll_t=^THTML_VerticalScroll_t;
     THTML_VerticalScroll_t=record
      unBrowserHandle:THHTMLBrowser;
      unScrollMax:TSteamUInt32;
      unScrollCurrent:TSteamUInt32;
      flPageScale:TSteamFloat;
      bVisible:TSteamBool;
      unPageSize:TSteamUInt32;
     end;

const HTML_VerticalScroll_t_k_iCallback=4512;

type { THTML_LinkAtPosition_t }
     PPHTML_LinkAtPosition_t=^PHTML_LinkAtPosition_t;
     PHTML_LinkAtPosition_t=^THTML_LinkAtPosition_t;
     THTML_LinkAtPosition_t=record
      unBrowserHandle:THHTMLBrowser;
      x:TSteamUInt32;
      y:TSteamUInt32;
      pchURL:PSteamChar;
      bInput:TSteamBool;
      bLiveLink:TSteamBool;
     end;

const HTML_LinkAtPosition_t_k_iCallback=4513;

type { THTML_JSAlert_t }
     PPHTML_JSAlert_t=^PHTML_JSAlert_t;
     PHTML_JSAlert_t=^THTML_JSAlert_t;
     THTML_JSAlert_t=record
      unBrowserHandle:THHTMLBrowser;
      pchMessage:PSteamChar;
     end;

const HTML_JSAlert_t_k_iCallback=4514;

type { THTML_JSConfirm_t }
     PPHTML_JSConfirm_t=^PHTML_JSConfirm_t;
     PHTML_JSConfirm_t=^THTML_JSConfirm_t;
     THTML_JSConfirm_t=record
      unBrowserHandle:THHTMLBrowser;
      pchMessage:PSteamChar;
     end;

const HTML_JSConfirm_t_k_iCallback=4515;

type { THTML_FileOpenDialog_t }
     PPHTML_FileOpenDialog_t=^PHTML_FileOpenDialog_t;
     PHTML_FileOpenDialog_t=^THTML_FileOpenDialog_t;
     THTML_FileOpenDialog_t=record
      unBrowserHandle:THHTMLBrowser;
      pchTitle:PSteamChar;
      pchInitialFile:PSteamChar;
     end;

const HTML_FileOpenDialog_t_k_iCallback=4516;

type { THTML_NewWindow_t }
     PPHTML_NewWindow_t=^PHTML_NewWindow_t;
     PHTML_NewWindow_t=^THTML_NewWindow_t;
     THTML_NewWindow_t=record
      unBrowserHandle:THHTMLBrowser;
      pchURL:PSteamChar;
      unX:TSteamUInt32;
      unY:TSteamUInt32;
      unWide:TSteamUInt32;
      unTall:TSteamUInt32;
      unNewWindow_BrowserHandle_IGNORE:THHTMLBrowser;
     end;

const HTML_NewWindow_t_k_iCallback=4521;

type { THTML_SetCursor_t }
     PPHTML_SetCursor_t=^PHTML_SetCursor_t;
     PHTML_SetCursor_t=^THTML_SetCursor_t;
     THTML_SetCursor_t=record
      unBrowserHandle:THHTMLBrowser;
      eMouseCursor:TSteamUInt32;
     end;

const HTML_SetCursor_t_k_iCallback=4522;

type { THTML_StatusText_t }
     PPHTML_StatusText_t=^PHTML_StatusText_t;
     PHTML_StatusText_t=^THTML_StatusText_t;
     THTML_StatusText_t=record
      unBrowserHandle:THHTMLBrowser;
      pchMsg:PSteamChar;
     end;

const HTML_StatusText_t_k_iCallback=4523;

type { THTML_ShowToolTip_t }
     PPHTML_ShowToolTip_t=^PHTML_ShowToolTip_t;
     PHTML_ShowToolTip_t=^THTML_ShowToolTip_t;
     THTML_ShowToolTip_t=record
      unBrowserHandle:THHTMLBrowser;
      pchMsg:PSteamChar;
     end;

const HTML_ShowToolTip_t_k_iCallback=4524;

type { THTML_UpdateToolTip_t }
     PPHTML_UpdateToolTip_t=^PHTML_UpdateToolTip_t;
     PHTML_UpdateToolTip_t=^THTML_UpdateToolTip_t;
     THTML_UpdateToolTip_t=record
      unBrowserHandle:THHTMLBrowser;
      pchMsg:PSteamChar;
     end;

const HTML_UpdateToolTip_t_k_iCallback=4525;

type { THTML_HideToolTip_t }
     PPHTML_HideToolTip_t=^PHTML_HideToolTip_t;
     PHTML_HideToolTip_t=^THTML_HideToolTip_t;
     THTML_HideToolTip_t=record
      unBrowserHandle:THHTMLBrowser;
     end;

const HTML_HideToolTip_t_k_iCallback=4526;

type { THTML_BrowserRestarted_t }
     PPHTML_BrowserRestarted_t=^PHTML_BrowserRestarted_t;
     PHTML_BrowserRestarted_t=^THTML_BrowserRestarted_t;
     THTML_BrowserRestarted_t=record
      unBrowserHandle:THHTMLBrowser;
      unOldBrowserHandle:THHTMLBrowser;
     end;

const HTML_BrowserRestarted_t_k_iCallback=4527;

type { TSteamInventoryResultReady_t }
     PPSteamInventoryResultReady_t=^PSteamInventoryResultReady_t;
     PSteamInventoryResultReady_t=^TSteamInventoryResultReady_t;
     TSteamInventoryResultReady_t=record
      m_handle:TSteamInventoryResult_t;
      m_result:TEResult;
     end;

const SteamInventoryResultReady_t_k_iCallback=4700;

type { TSteamInventoryFullUpdate_t }
     PPSteamInventoryFullUpdate_t=^PSteamInventoryFullUpdate_t;
     PSteamInventoryFullUpdate_t=^TSteamInventoryFullUpdate_t;
     TSteamInventoryFullUpdate_t=record
      m_handle:TSteamInventoryResult_t;
     end;

const SteamInventoryFullUpdate_t_k_iCallback=4701;

type { TSteamInventoryDefinitionUpdate_t }
     PPSteamInventoryDefinitionUpdate_t=^PSteamInventoryDefinitionUpdate_t;
     PSteamInventoryDefinitionUpdate_t=^TSteamInventoryDefinitionUpdate_t;
     TSteamInventoryDefinitionUpdate_t=record
      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size
     end;

const SteamInventoryDefinitionUpdate_t_k_iCallback=4702;

type { TSteamInventoryEligiblePromoItemDefIDs_t }
     PPSteamInventoryEligiblePromoItemDefIDs_t=^PSteamInventoryEligiblePromoItemDefIDs_t;
     PSteamInventoryEligiblePromoItemDefIDs_t=^TSteamInventoryEligiblePromoItemDefIDs_t;
     TSteamInventoryEligiblePromoItemDefIDs_t=record
      m_result:TEResult;
      m_steamID:TCSteamID;
      m_numEligiblePromoItemDefs:TSteamInt32;
      m_bCachedData:TSteamBool;
     end;

const SteamInventoryEligiblePromoItemDefIDs_t_k_iCallback=4703;

type { TSteamInventoryStartPurchaseResult_t }
     PPSteamInventoryStartPurchaseResult_t=^PSteamInventoryStartPurchaseResult_t;
     PSteamInventoryStartPurchaseResult_t=^TSteamInventoryStartPurchaseResult_t;
     TSteamInventoryStartPurchaseResult_t=record
      m_result:TEResult;
      m_ulOrderID:TSteamUInt64;
      m_ulTransID:TSteamUInt64;
     end;

const SteamInventoryStartPurchaseResult_t_k_iCallback=4704;

type { TSteamInventoryRequestPricesResult_t }
     PPSteamInventoryRequestPricesResult_t=^PSteamInventoryRequestPricesResult_t;
     PSteamInventoryRequestPricesResult_t=^TSteamInventoryRequestPricesResult_t;
     TSteamInventoryRequestPricesResult_t=record
      m_result:TEResult;
      m_rgchCurrency:array[0..4-1] of TSteamChar;
     end;

const SteamInventoryRequestPricesResult_t_k_iCallback=4705;

type { TSteamTimelineGamePhaseRecordingExists_t }
     PPSteamTimelineGamePhaseRecordingExists_t=^PSteamTimelineGamePhaseRecordingExists_t;
     PSteamTimelineGamePhaseRecordingExists_t=^TSteamTimelineGamePhaseRecordingExists_t;
     TSteamTimelineGamePhaseRecordingExists_t=record
      m_rgchPhaseID:array[0..64-1] of TSteamChar;
      m_ulRecordingMS:TSteamUInt64;
      m_ulLongestClipMS:TSteamUInt64;
      m_unClipCount:TSteamUInt32;
      m_unScreenshotCount:TSteamUInt32;
     end;

const SteamTimelineGamePhaseRecordingExists_t_k_iCallback=6001;

type { TSteamTimelineEventRecordingExists_t }
     PPSteamTimelineEventRecordingExists_t=^PSteamTimelineEventRecordingExists_t;
     PSteamTimelineEventRecordingExists_t=^TSteamTimelineEventRecordingExists_t;
     TSteamTimelineEventRecordingExists_t=record
      m_ulEventID:TSteamUInt64;
      m_bRecordingExists:TSteamBool;
     end;

const SteamTimelineEventRecordingExists_t_k_iCallback=6002;

type { TGetVideoURLResult_t }
     PPGetVideoURLResult_t=^PGetVideoURLResult_t;
     PGetVideoURLResult_t=^TGetVideoURLResult_t;
     TGetVideoURLResult_t=record
      m_eResult:TEResult;
      m_unVideoAppID:TAppId_t;
      m_rgchURL:array[0..256-1] of TSteamChar;
     end;

const GetVideoURLResult_t_k_iCallback=4611;

type { TGetOPFSettingsResult_t }
     PPGetOPFSettingsResult_t=^PGetOPFSettingsResult_t;
     PGetOPFSettingsResult_t=^TGetOPFSettingsResult_t;
     TGetOPFSettingsResult_t=record
      m_eResult:TEResult;
      m_unVideoAppID:TAppId_t;
     end;

const GetOPFSettingsResult_t_k_iCallback=4624;

type { TBroadcastUploadStart_t }
     PPBroadcastUploadStart_t=^PBroadcastUploadStart_t;
     PBroadcastUploadStart_t=^TBroadcastUploadStart_t;
     TBroadcastUploadStart_t=record
      m_bIsRTMP:TSteamBool;
     end;

const BroadcastUploadStart_t_k_iCallback=4604;

type { TBroadcastUploadStop_t }
     PPBroadcastUploadStop_t=^PBroadcastUploadStop_t;
     PBroadcastUploadStop_t=^TBroadcastUploadStop_t;
     TBroadcastUploadStop_t=record
      m_eResult:TEBroadcastUploadResult;
     end;

const BroadcastUploadStop_t_k_iCallback=4605;

{$ifdef fpc}{$packrecords c}{$else}{$A8}{$endif}
type { TSteamParentalSettingsChanged_t }
     PPSteamParentalSettingsChanged_t=^PSteamParentalSettingsChanged_t;
     PSteamParentalSettingsChanged_t=^TSteamParentalSettingsChanged_t;
     TSteamParentalSettingsChanged_t=record
      m_Dummy:TSteamUInt8; // The C struct is empty, and an empty record has no defined size
     end;

const SteamParentalSettingsChanged_t_k_iCallback=5001;

{$ifdef Windows}{$ifdef fpc}{$packrecords 8}{$else}{$A8}{$endif}{$else}{$ifdef fpc}{$packrecords 4}{$else}{$A4}{$endif}{$endif}
type { TSteamRemotePlaySessionConnected_t }
     PPSteamRemotePlaySessionConnected_t=^PSteamRemotePlaySessionConnected_t;
     PSteamRemotePlaySessionConnected_t=^TSteamRemotePlaySessionConnected_t;
     TSteamRemotePlaySessionConnected_t=record
      m_unSessionID:TRemotePlaySessionID_t;
     end;

const SteamRemotePlaySessionConnected_t_k_iCallback=5701;

type { TSteamRemotePlaySessionDisconnected_t }
     PPSteamRemotePlaySessionDisconnected_t=^PSteamRemotePlaySessionDisconnected_t;
     PSteamRemotePlaySessionDisconnected_t=^TSteamRemotePlaySessionDisconnected_t;
     TSteamRemotePlaySessionDisconnected_t=record
      m_unSessionID:TRemotePlaySessionID_t;
     end;

const SteamRemotePlaySessionDisconnected_t_k_iCallback=5702;

type { TSteamRemotePlayTogetherGuestInvite_t }
     PPSteamRemotePlayTogetherGuestInvite_t=^PSteamRemotePlayTogetherGuestInvite_t;
     PSteamRemotePlayTogetherGuestInvite_t=^TSteamRemotePlayTogetherGuestInvite_t;
     TSteamRemotePlayTogetherGuestInvite_t=record
      m_szConnectURL:array[0..1024-1] of TSteamChar;
     end;

const SteamRemotePlayTogetherGuestInvite_t_k_iCallback=5703;

type { TSteamRemotePlaySessionAvatarLoaded_t }
     PPSteamRemotePlaySessionAvatarLoaded_t=^PSteamRemotePlaySessionAvatarLoaded_t;
     PSteamRemotePlaySessionAvatarLoaded_t=^TSteamRemotePlaySessionAvatarLoaded_t;
     TSteamRemotePlaySessionAvatarLoaded_t=record
      m_unSessionID:TRemotePlaySessionID_t;
      m_iImage:TSteamInt32;
      m_iWide:TSteamInt32;
      m_iTall:TSteamInt32;
     end;

const SteamRemotePlaySessionAvatarLoaded_t_k_iCallback=5704;

type { TSteamNetworkingMessagesSessionRequest_t }
     PPSteamNetworkingMessagesSessionRequest_t=^PSteamNetworkingMessagesSessionRequest_t;
     PSteamNetworkingMessagesSessionRequest_t=^TSteamNetworkingMessagesSessionRequest_t;
     TSteamNetworkingMessagesSessionRequest_t=packed record
      m_identityRemote:TSteamNetworkingIdentity;
     end;

const SteamNetworkingMessagesSessionRequest_t_k_iCallback=1251;

type { TSteamNetworkingMessagesSessionFailed_t }
     PPSteamNetworkingMessagesSessionFailed_t=^PSteamNetworkingMessagesSessionFailed_t;
     PSteamNetworkingMessagesSessionFailed_t=^TSteamNetworkingMessagesSessionFailed_t;
     TSteamNetworkingMessagesSessionFailed_t=packed record
      m_info:TSteamNetConnectionInfo_t;
     end;

const SteamNetworkingMessagesSessionFailed_t_k_iCallback=1252;

type { TSteamNetConnectionStatusChangedCallback_t }
     PPSteamNetConnectionStatusChangedCallback_t=^PSteamNetConnectionStatusChangedCallback_t;
     PSteamNetConnectionStatusChangedCallback_t=^TSteamNetConnectionStatusChangedCallback_t;
     TSteamNetConnectionStatusChangedCallback_t=record
      m_hConn:THSteamNetConnection;
      m_info:TSteamNetConnectionInfo_t;
      m_eOldState:TESteamNetworkingConnectionState;
     end;

const SteamNetConnectionStatusChangedCallback_t_k_iCallback=1221;

type { TSteamNetAuthenticationStatus_t }
     PPSteamNetAuthenticationStatus_t=^PSteamNetAuthenticationStatus_t;
     PSteamNetAuthenticationStatus_t=^TSteamNetAuthenticationStatus_t;
     TSteamNetAuthenticationStatus_t=record
      m_eAvail:TESteamNetworkingAvailability;
      m_debugMsg:array[0..256-1] of TSteamChar;
     end;

const SteamNetAuthenticationStatus_t_k_iCallback=1222;

{$ifdef fpc}{$packrecords c}{$else}{$A8}{$endif}
type { TSteamRelayNetworkStatus_t }
     PPSteamRelayNetworkStatus_t=^PSteamRelayNetworkStatus_t;
     PSteamRelayNetworkStatus_t=^TSteamRelayNetworkStatus_t;
     TSteamRelayNetworkStatus_t=record
      m_eAvail:TESteamNetworkingAvailability;
      m_bPingMeasurementInProgress:TSteamInt32;
      m_eAvailNetworkConfig:TESteamNetworkingAvailability;
      m_eAvailAnyRelay:TESteamNetworkingAvailability;
      m_debugMsg:array[0..256-1] of TSteamChar;
     end;

const SteamRelayNetworkStatus_t_k_iCallback=1281;

{$ifdef Windows}{$ifdef fpc}{$packrecords 8}{$else}{$A8}{$endif}{$else}{$ifdef fpc}{$packrecords 4}{$else}{$A4}{$endif}{$endif}
type { TGSClientApprove_t }
     PPGSClientApprove_t=^PGSClientApprove_t;
     PGSClientApprove_t=^TGSClientApprove_t;
     TGSClientApprove_t=record
      m_SteamID:TCSteamID;
      m_OwnerSteamID:TCSteamID;
     end;

const GSClientApprove_t_k_iCallback=201;

type { TGSClientDeny_t }
     PPGSClientDeny_t=^PGSClientDeny_t;
     PGSClientDeny_t=^TGSClientDeny_t;
     TGSClientDeny_t=record
      m_SteamID:TCSteamID;
      m_eDenyReason:TEDenyReason;
      m_rgchOptionalText:array[0..128-1] of TSteamChar;
     end;

const GSClientDeny_t_k_iCallback=202;

type { TGSClientKick_t }
     PPGSClientKick_t=^PGSClientKick_t;
     PGSClientKick_t=^TGSClientKick_t;
     TGSClientKick_t=record
      m_SteamID:TCSteamID;
      m_eDenyReason:TEDenyReason;
     end;

const GSClientKick_t_k_iCallback=203;

type { TGSClientAchievementStatus_t }
     PPGSClientAchievementStatus_t=^PGSClientAchievementStatus_t;
     PGSClientAchievementStatus_t=^TGSClientAchievementStatus_t;
     TGSClientAchievementStatus_t=record
      m_SteamID:TSteamUInt64;
      m_pchAchievement:array[0..128-1] of TSteamChar;
      m_bUnlocked:TSteamBool;
     end;

const GSClientAchievementStatus_t_k_iCallback=206;

type { TGSPolicyResponse_t }
     PPGSPolicyResponse_t=^PGSPolicyResponse_t;
     PGSPolicyResponse_t=^TGSPolicyResponse_t;
     TGSPolicyResponse_t=record
      m_bSecure:TSteamUInt8;
     end;

const GSPolicyResponse_t_k_iCallback=115;

type { TGSGameplayStats_t }
     PPGSGameplayStats_t=^PGSGameplayStats_t;
     PGSGameplayStats_t=^TGSGameplayStats_t;
     TGSGameplayStats_t=record
      m_eResult:TEResult;
      m_nRank:TSteamInt32;
      m_unTotalConnects:TSteamUInt32;
      m_unTotalMinutesPlayed:TSteamUInt32;
     end;

const GSGameplayStats_t_k_iCallback=207;

type { TGSClientGroupStatus_t }
     PPGSClientGroupStatus_t=^PGSClientGroupStatus_t;
     PGSClientGroupStatus_t=^TGSClientGroupStatus_t;
     TGSClientGroupStatus_t=record
      m_SteamIDUser:TCSteamID;
      m_SteamIDGroup:TCSteamID;
      m_bMember:TSteamBool;
      m_bOfficer:TSteamBool;
     end;

const GSClientGroupStatus_t_k_iCallback=208;

type { TGSReputation_t }
     PPGSReputation_t=^PGSReputation_t;
     PGSReputation_t=^TGSReputation_t;
     TGSReputation_t=record
      m_eResult:TEResult;
      m_unReputationScore:TSteamUInt32;
      m_bBanned:TSteamBool;
      m_unBannedIP:TSteamUInt32;
      m_usBannedPort:TSteamUInt16;
      m_ulBannedGameID:TSteamUInt64;
      m_unBanExpires:TSteamUInt32;
     end;

const GSReputation_t_k_iCallback=209;

type { TAssociateWithClanResult_t }
     PPAssociateWithClanResult_t=^PAssociateWithClanResult_t;
     PAssociateWithClanResult_t=^TAssociateWithClanResult_t;
     TAssociateWithClanResult_t=record
      m_eResult:TEResult;
     end;

const AssociateWithClanResult_t_k_iCallback=210;

type { TComputeNewPlayerCompatibilityResult_t }
     PPComputeNewPlayerCompatibilityResult_t=^PComputeNewPlayerCompatibilityResult_t;
     PComputeNewPlayerCompatibilityResult_t=^TComputeNewPlayerCompatibilityResult_t;
     TComputeNewPlayerCompatibilityResult_t=record
      m_eResult:TEResult;
      m_cPlayersThatDontLikeCandidate:TSteamInt32;
      m_cPlayersThatCandidateDoesntLike:TSteamInt32;
      m_cClanPlayersThatDontLikeCandidate:TSteamInt32;
      m_SteamIDCandidate:TCSteamID;
     end;

const ComputeNewPlayerCompatibilityResult_t_k_iCallback=211;

type { TGSStatsReceived_t }
     PPGSStatsReceived_t=^PGSStatsReceived_t;
     PGSStatsReceived_t=^TGSStatsReceived_t;
     TGSStatsReceived_t=record
      m_eResult:TEResult;
      m_steamIDUser:TCSteamID;
     end;

const GSStatsReceived_t_k_iCallback=1800;

type { TGSStatsStored_t }
     PPGSStatsStored_t=^PGSStatsStored_t;
     PGSStatsStored_t=^TGSStatsStored_t;
     TGSStatsStored_t=record
      m_eResult:TEResult;
      m_steamIDUser:TCSteamID;
     end;

const GSStatsStored_t_k_iCallback=1801;

type { TGSStatsUnloaded_t }
     PPGSStatsUnloaded_t=^PGSStatsUnloaded_t;
     PGSStatsUnloaded_t=^TGSStatsUnloaded_t;
     TGSStatsUnloaded_t=record
      m_steamIDUser:TCSteamID;
     end;

const GSStatsUnloaded_t_k_iCallback=1108;

type { TSteamNetworkingFakeIPResult_t }
     PPSteamNetworkingFakeIPResult_t=^PSteamNetworkingFakeIPResult_t;
     PSteamNetworkingFakeIPResult_t=^TSteamNetworkingFakeIPResult_t;
     TSteamNetworkingFakeIPResult_t=record
      m_eResult:TEResult;
      m_identity:TSteamNetworkingIdentity;
      m_unIP:TSteamUInt32;
      m_unPorts:array[0..8-1] of TSteamUInt16;
     end;

const SteamNetworkingFakeIPResult_t_k_iCallback=1223;

type { TValvePackingSentinel_t }
     PPValvePackingSentinel_t=^PValvePackingSentinel_t;
     PValvePackingSentinel_t=^TValvePackingSentinel_t;
     TValvePackingSentinel_t=record
      m_u32:TSteamUInt32;
      m_u64:TSteamUInt64;
      m_u16:TSteamUInt16;
      m_d:TSteamDouble;
     end;

type { TCallbackMsg_t }
     PPCallbackMsg_t=^PCallbackMsg_t;
     PCallbackMsg_t=^TCallbackMsg_t;
     TCallbackMsg_t=record
      m_hSteamUser:THSteamUser; // Specific user to whom this callback applies
      m_iCallback:TSteamInt32; // Callback identifier, matches the k_iCallback constant of the callback record
      m_pubParam:PSteamUInt8; // Points to the callback record
      m_cubParam:TSteamInt32; // Size of the data pointed to by m_pubParam
     end;

type PPPFNPreMinidumpCallback=^PPFNPreMinidumpCallback;
     PPFNPreMinidumpCallback=^TPFNPreMinidumpCallback;
     TPFNPreMinidumpCallback=procedure(const aParameter1:TSteamPointer); cdecl;

type PPSteamInputActionEventCallbackPointer=^PSteamInputActionEventCallbackPointer;
     PSteamInputActionEventCallbackPointer=^TSteamInputActionEventCallbackPointer;
     TSteamInputActionEventCallbackPointer=procedure(const aParameter1:PSteamInputActionEvent_t); cdecl;

type PPFnSteamNetConnectionStatusChanged=^PFnSteamNetConnectionStatusChanged;
     PFnSteamNetConnectionStatusChanged=^TFnSteamNetConnectionStatusChanged;
     TFnSteamNetConnectionStatusChanged=procedure(const aParameter1:PSteamNetConnectionStatusChangedCallback_t); cdecl;

type PPFnSteamNetAuthenticationStatusChanged=^PFnSteamNetAuthenticationStatusChanged;
     PFnSteamNetAuthenticationStatusChanged=^TFnSteamNetAuthenticationStatusChanged;
     TFnSteamNetAuthenticationStatusChanged=procedure(const aParameter1:PSteamNetAuthenticationStatus_t); cdecl;

type PPFnSteamRelayNetworkStatusChanged=^PFnSteamRelayNetworkStatusChanged;
     PFnSteamRelayNetworkStatusChanged=^TFnSteamRelayNetworkStatusChanged;
     TFnSteamRelayNetworkStatusChanged=procedure(const aParameter1:PSteamRelayNetworkStatus_t); cdecl;

type PPFnSteamNetworkingMessagesSessionRequest=^PFnSteamNetworkingMessagesSessionRequest;
     PFnSteamNetworkingMessagesSessionRequest=^TFnSteamNetworkingMessagesSessionRequest;
     TFnSteamNetworkingMessagesSessionRequest=procedure(const aParameter1:PSteamNetworkingMessagesSessionRequest_t); cdecl;

type PPFnSteamNetworkingMessagesSessionFailed=^PFnSteamNetworkingMessagesSessionFailed;
     PFnSteamNetworkingMessagesSessionFailed=^TFnSteamNetworkingMessagesSessionFailed;
     TFnSteamNetworkingMessagesSessionFailed=procedure(const aParameter1:PSteamNetworkingMessagesSessionFailed_t); cdecl;

type PPFnSteamNetworkingFakeIPResult=^PFnSteamNetworkingFakeIPResult;
     PFnSteamNetworkingFakeIPResult=^TFnSteamNetworkingFakeIPResult;
     TFnSteamNetworkingFakeIPResult=procedure(const aParameter1:PSteamNetworkingFakeIPResult_t); cdecl;

type PPFSteamNetworkingSocketsDebugOutput=^PFSteamNetworkingSocketsDebugOutput;
     PFSteamNetworkingSocketsDebugOutput=^TFSteamNetworkingSocketsDebugOutput;
     TFSteamNetworkingSocketsDebugOutput=procedure(const aParameter1:TESteamNetworkingSocketsDebugOutputType;const aParameter2:PSteamChar); cdecl;

type { TISteamMatchmakingServerListResponseVTable }
     PPISteamMatchmakingServerListResponseVTable=^PISteamMatchmakingServerListResponseVTable;
     PISteamMatchmakingServerListResponseVTable=^TISteamMatchmakingServerListResponseVTable;
     PISteamMatchmakingServerListResponseObject=^TISteamMatchmakingServerListResponseObject;
     TISteamMatchmakingServerListResponseVTable=record
      ServerResponded:procedure(const aSelf:PISteamMatchmakingServerListResponseObject;const hRequest:THServerListRequest;const iServer:TSteamInt32); cdecl;
      ServerFailedToRespond:procedure(const aSelf:PISteamMatchmakingServerListResponseObject;const hRequest:THServerListRequest;const iServer:TSteamInt32); cdecl;
      RefreshComplete:procedure(const aSelf:PISteamMatchmakingServerListResponseObject;const hRequest:THServerListRequest;const response:TEMatchMakingServerResponse); cdecl;
     end;

     { TISteamMatchmakingServerListResponseObject }
     PPISteamMatchmakingServerListResponseObject=^PISteamMatchmakingServerListResponseObject;
     TISteamMatchmakingServerListResponseObject=record
      VTable:PISteamMatchmakingServerListResponseVTable; // Must stay the first field, this is what Steam reads as the vtable pointer
      UserData:TSteamPointer; // Free for the caller, for finding its own object again inside a method
     end;

type { TISteamMatchmakingPingResponseVTable }
     PPISteamMatchmakingPingResponseVTable=^PISteamMatchmakingPingResponseVTable;
     PISteamMatchmakingPingResponseVTable=^TISteamMatchmakingPingResponseVTable;
     PISteamMatchmakingPingResponseObject=^TISteamMatchmakingPingResponseObject;
     TISteamMatchmakingPingResponseVTable=record
      ServerResponded:procedure(const aSelf:PISteamMatchmakingPingResponseObject;const server:Pgameserveritem_t); cdecl;
      ServerFailedToRespond:procedure(const aSelf:PISteamMatchmakingPingResponseObject); cdecl;
     end;

     { TISteamMatchmakingPingResponseObject }
     PPISteamMatchmakingPingResponseObject=^PISteamMatchmakingPingResponseObject;
     TISteamMatchmakingPingResponseObject=record
      VTable:PISteamMatchmakingPingResponseVTable; // Must stay the first field, this is what Steam reads as the vtable pointer
      UserData:TSteamPointer; // Free for the caller, for finding its own object again inside a method
     end;

type { TISteamMatchmakingPlayersResponseVTable }
     PPISteamMatchmakingPlayersResponseVTable=^PISteamMatchmakingPlayersResponseVTable;
     PISteamMatchmakingPlayersResponseVTable=^TISteamMatchmakingPlayersResponseVTable;
     PISteamMatchmakingPlayersResponseObject=^TISteamMatchmakingPlayersResponseObject;
     TISteamMatchmakingPlayersResponseVTable=record
      AddPlayerToList:procedure(const aSelf:PISteamMatchmakingPlayersResponseObject;const pchName:PSteamChar;const nScore:TSteamInt32;const flTimePlayed:TSteamFloat); cdecl;
      PlayersFailedToRespond:procedure(const aSelf:PISteamMatchmakingPlayersResponseObject); cdecl;
      PlayersRefreshComplete:procedure(const aSelf:PISteamMatchmakingPlayersResponseObject); cdecl;
     end;

     { TISteamMatchmakingPlayersResponseObject }
     PPISteamMatchmakingPlayersResponseObject=^PISteamMatchmakingPlayersResponseObject;
     TISteamMatchmakingPlayersResponseObject=record
      VTable:PISteamMatchmakingPlayersResponseVTable; // Must stay the first field, this is what Steam reads as the vtable pointer
      UserData:TSteamPointer; // Free for the caller, for finding its own object again inside a method
     end;

type { TISteamMatchmakingRulesResponseVTable }
     PPISteamMatchmakingRulesResponseVTable=^PISteamMatchmakingRulesResponseVTable;
     PISteamMatchmakingRulesResponseVTable=^TISteamMatchmakingRulesResponseVTable;
     PISteamMatchmakingRulesResponseObject=^TISteamMatchmakingRulesResponseObject;
     TISteamMatchmakingRulesResponseVTable=record
      RulesResponded:procedure(const aSelf:PISteamMatchmakingRulesResponseObject;const pchRule:PSteamChar;const pchValue:PSteamChar); cdecl;
      RulesFailedToRespond:procedure(const aSelf:PISteamMatchmakingRulesResponseObject); cdecl;
      RulesRefreshComplete:procedure(const aSelf:PISteamMatchmakingRulesResponseObject); cdecl;
     end;

     { TISteamMatchmakingRulesResponseObject }
     PPISteamMatchmakingRulesResponseObject=^PISteamMatchmakingRulesResponseObject;
     TISteamMatchmakingRulesResponseObject=record
      VTable:PISteamMatchmakingRulesResponseVTable; // Must stay the first field, this is what Steam reads as the vtable pointer
      UserData:TSteamPointer; // Free for the caller, for finding its own object again inside a method
     end;

const k_uAppIdInvalid=$0;
      k_uDepotIdInvalid=$0;
      k_uAPICallInvalid=$0;
      k_uAccountIdInvalid=0;
      k_ulPartyBeaconIdInvalid=0;
      k_HAuthTicketInvalid=0;
      k_unSteamAccountIDMask=$ffffffff;
      k_unSteamAccountInstanceMask=$000fffff;
      k_unSteamUserDefaultInstance=1;
      k_cchGameExtraInfoMax=64;
      k_cchMaxSteamErrMsg=1024;
      k_cchMaxFriendsGroupName=64;
      k_cFriendsGroupLimit=100;
      k_FriendsGroupID_Invalid=-1;
      k_cEnumerateFollowersMax=50;
      k_usFriendGameInfoQueryPort_NotInitialized=$ffff;
      k_usFriendGameInfoQueryPort_Error=$fffe;
      k_cubChatMetadataMax=8192;
      k_cbMaxGameServerGameDir=32;
      k_cbMaxGameServerMapName=32;
      k_cbMaxGameServerGameDescription=64;
      k_cbMaxGameServerName=64;
      k_cbMaxGameServerTags=128;
      k_cbMaxGameServerGameData=2048;
      HSERVERQUERY_INVALID=$ffffffff;
      k_unFavoriteFlagNone=$00;
      k_unFavoriteFlagFavorite=$01;
      k_unFavoriteFlagHistory=$02;
      k_unMaxCloudFileChunkSize=100 * 1024 * 1024;
      k_PublishedFileIdInvalid=0;
      k_UGCHandleInvalid=TSteamUInt64($ffffffffffffffff);
      k_PublishedFileUpdateHandleInvalid=TSteamUInt64($ffffffffffffffff);
      k_UGCFileStreamHandleInvalid=TSteamUInt64($ffffffffffffffff);
      k_cchPublishedDocumentTitleMax=128 + 1;
      k_cchPublishedDocumentDescriptionMax=8000;
      k_cchPublishedDocumentChangeDescriptionMax=8000;
      k_unEnumeratePublishedFilesMaxResults=50;
      k_cchTagListMax=1024 + 1;
      k_cchFilenameMax=260;
      k_cchPublishedFileURLMax=256;
      k_cubAppProofOfPurchaseKeyMax=240;
      k_nScreenshotMaxTaggedUsers=32;
      k_nScreenshotMaxTaggedPublishedFiles=32;
      k_cubUFSTagTypeMax=255;
      k_cubUFSTagValueMax=255;
      k_ScreenshotThumbWidth=200;
      k_UGCQueryHandleInvalid=TSteamUInt64($ffffffffffffffff);
      k_UGCUpdateHandleInvalid=TSteamUInt64($ffffffffffffffff);
      kNumUGCResultsPerPage=50;
      k_cchDeveloperMetadataMax=5000;
      INVALID_HTMLBROWSER=0;
      k_SteamItemInstanceIDInvalid=TSteamUInt64($ffffffffffffffff);
      k_SteamInventoryResultInvalid=-1;
      k_SteamInventoryUpdateHandleInvalid=TSteamUInt64($ffffffffffffffff);
      k_unMaxTimelinePriority=1000;
      k_unTimelinePriority_KeepCurrentValue=1000000;
      k_flMaxTimelineEventDuration=600.0;
      k_cchMaxPhaseIDLength=64;
      k_HSteamNetConnection_Invalid=0;
      k_HSteamListenSocket_Invalid=0;
      k_HSteamNetPollGroup_Invalid=0;
      k_cchMaxSteamNetworkingErrMsg=1024;
      k_cchSteamNetworkingMaxConnectionCloseReason=128;
      k_cchSteamNetworkingMaxConnectionDescription=128;
      k_cchSteamNetworkingMaxConnectionAppName=32;
      k_nSteamNetworkConnectionInfoFlags_Unauthenticated=1;
      k_nSteamNetworkConnectionInfoFlags_Unencrypted=2;
      k_nSteamNetworkConnectionInfoFlags_LoopbackBuffers=4;
      k_nSteamNetworkConnectionInfoFlags_Fast=8;
      k_nSteamNetworkConnectionInfoFlags_Relayed=16;
      k_nSteamNetworkConnectionInfoFlags_DualWifi=32;
      k_cbMaxSteamNetworkingSocketsMessageSizeSend=512 * 1024;
      k_nSteamNetworkingSend_Unreliable=0;
      k_nSteamNetworkingSend_NoNagle=1;
      k_nSteamNetworkingSend_UnreliableNoNagle=k_nSteamNetworkingSend_Unreliable or k_nSteamNetworkingSend_NoNagle;
      k_nSteamNetworkingSend_NoDelay=4;
      k_nSteamNetworkingSend_UnreliableNoDelay=k_nSteamNetworkingSend_Unreliable or k_nSteamNetworkingSend_NoDelay or k_nSteamNetworkingSend_NoNagle;
      k_nSteamNetworkingSend_Reliable=8;
      k_nSteamNetworkingSend_ReliableNoNagle=k_nSteamNetworkingSend_Reliable or k_nSteamNetworkingSend_NoNagle;
      k_nSteamNetworkingSend_UseCurrentThread=16;
      k_nSteamNetworkingSend_AutoRestartBrokenSession=32;
      k_cchMaxSteamNetworkingPingLocationString=1024;
      k_nSteamNetworkingPing_Failed=-1;
      k_nSteamNetworkingPing_Unknown=-2;
      k_nSteamNetworkingConfig_P2P_Transport_ICE_Enable_Default=-1;
      k_nSteamNetworkingConfig_P2P_Transport_ICE_Enable_Disable=0;
      k_nSteamNetworkingConfig_P2P_Transport_ICE_Enable_Relay=1;
      k_nSteamNetworkingConfig_P2P_Transport_ICE_Enable_Private=2;
      k_nSteamNetworkingConfig_P2P_Transport_ICE_Enable_Public=4;
      k_nSteamNetworkingConfig_P2P_Transport_ICE_Enable_All=$7fffffff;
      k_SteamDatagramPOPID_dev=( ord('d') shl 16 ) or ( ord('e') shl 8 ) or ord('v');
      STEAMGAMESERVER_QUERY_PORT_SHARED=$ffff;
      MASTERSERVERUPDATERPORT_USEGAMESOCKETSHARE=STEAMGAMESERVER_QUERY_PORT_SHARED;
      k_cbSteamDatagramMaxSerializedTicket=512;
      k_cbMaxSteamDatagramGameCoordinatorServerLoginAppData=2048;
      k_cbMaxSteamDatagramGameCoordinatorServerLoginSerialized=4096;
      k_cbSteamNetworkingSocketsFakeUDPPortRecommendedMTU=1200;
      k_cbSteamNetworkingSocketsFakeUDPPortMaxMessageSize=4096;

const ISteamUser_INTERFACE_VERSION='SteamUser023';
      ISteamFriends_INTERFACE_VERSION='SteamFriends018';
      ISteamUtils_INTERFACE_VERSION='SteamUtils010';
      ISteamMatchmaking_INTERFACE_VERSION='SteamMatchMaking009';
      ISteamMatchmakingServers_INTERFACE_VERSION='SteamMatchMakingServers002';
      ISteamParties_INTERFACE_VERSION='SteamParties002';
      ISteamRemoteStorage_INTERFACE_VERSION='STEAMREMOTESTORAGE_INTERFACE_VERSION016';
      ISteamUserStats_INTERFACE_VERSION='STEAMUSERSTATS_INTERFACE_VERSION013';
      ISteamApps_INTERFACE_VERSION='STEAMAPPS_INTERFACE_VERSION009';
      ISteamNetworking_INTERFACE_VERSION='SteamNetworking006';
      ISteamScreenshots_INTERFACE_VERSION='STEAMSCREENSHOTS_INTERFACE_VERSION003';
      ISteamMusic_INTERFACE_VERSION='STEAMMUSIC_INTERFACE_VERSION001';
      ISteamHTTP_INTERFACE_VERSION='STEAMHTTP_INTERFACE_VERSION003';
      ISteamInput_INTERFACE_VERSION='SteamInput006';
      ISteamController_INTERFACE_VERSION='SteamController008';
      ISteamUGC_INTERFACE_VERSION='STEAMUGC_INTERFACE_VERSION021';
      ISteamHTMLSurface_INTERFACE_VERSION='STEAMHTMLSURFACE_INTERFACE_VERSION_005';
      ISteamInventory_INTERFACE_VERSION='STEAMINVENTORY_INTERFACE_V003';
      ISteamTimeline_INTERFACE_VERSION='STEAMTIMELINE_INTERFACE_V004';
      ISteamVideo_INTERFACE_VERSION='STEAMVIDEO_INTERFACE_V007';
      ISteamParentalSettings_INTERFACE_VERSION='STEAMPARENTALSETTINGS_INTERFACE_VERSION001';
      ISteamRemotePlay_INTERFACE_VERSION='STEAMREMOTEPLAY_INTERFACE_VERSION004';
      ISteamNetworkingMessages_INTERFACE_VERSION='SteamNetworkingMessages002';
      ISteamNetworkingSockets_INTERFACE_VERSION='SteamNetworkingSockets012';
      ISteamNetworkingUtils_INTERFACE_VERSION='SteamNetworkingUtils004';
      ISteamGameServer_INTERFACE_VERSION='SteamGameServer015';
      ISteamGameServerStats_INTERFACE_VERSION='SteamGameServerStats001';

// ISteamClient
var SteamAPI_ISteamClient_CreateSteamPipe:function(const aSelf:PISteamClient):THSteamPipe; cdecl;
    SteamAPI_ISteamClient_BReleaseSteamPipe:function(const aSelf:PISteamClient;const hSteamPipe:THSteamPipe):TSteamBool; cdecl;
    SteamAPI_ISteamClient_ConnectToGlobalUser:function(const aSelf:PISteamClient;const hSteamPipe:THSteamPipe):THSteamUser; cdecl;
    SteamAPI_ISteamClient_CreateLocalUser:function(const aSelf:PISteamClient;const phSteamPipe:PHSteamPipe;const eAccountType:TEAccountType):THSteamUser; cdecl;
    SteamAPI_ISteamClient_ReleaseUser:procedure(const aSelf:PISteamClient;const hSteamPipe:THSteamPipe;const hUser:THSteamUser); cdecl;
    SteamAPI_ISteamClient_GetISteamUser:function(const aSelf:PISteamClient;const hSteamUser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamUser; cdecl;
    SteamAPI_ISteamClient_GetISteamGameServer:function(const aSelf:PISteamClient;const hSteamUser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamGameServer; cdecl;
    SteamAPI_ISteamClient_SetLocalIPBinding:procedure(const aSelf:PISteamClient;const unIP:PSteamIPAddress_t;const usPort:TSteamUInt16); cdecl;
    SteamAPI_ISteamClient_GetISteamFriends:function(const aSelf:PISteamClient;const hSteamUser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamFriends; cdecl;
    SteamAPI_ISteamClient_GetISteamUtils:function(const aSelf:PISteamClient;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamUtils; cdecl;
    SteamAPI_ISteamClient_GetISteamMatchmaking:function(const aSelf:PISteamClient;const hSteamUser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamMatchmaking; cdecl;
    SteamAPI_ISteamClient_GetISteamMatchmakingServers:function(const aSelf:PISteamClient;const hSteamUser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamMatchmakingServers; cdecl;
    SteamAPI_ISteamClient_GetISteamGenericInterface:function(const aSelf:PISteamClient;const hSteamUser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):TSteamPointer; cdecl;
    SteamAPI_ISteamClient_GetISteamUserStats:function(const aSelf:PISteamClient;const hSteamUser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamUserStats; cdecl;
    SteamAPI_ISteamClient_GetISteamGameServerStats:function(const aSelf:PISteamClient;const hSteamuser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamGameServerStats; cdecl;
    SteamAPI_ISteamClient_GetISteamApps:function(const aSelf:PISteamClient;const hSteamUser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamApps; cdecl;
    SteamAPI_ISteamClient_GetISteamNetworking:function(const aSelf:PISteamClient;const hSteamUser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamNetworking; cdecl;
    SteamAPI_ISteamClient_GetISteamRemoteStorage:function(const aSelf:PISteamClient;const hSteamuser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamRemoteStorage; cdecl;
    SteamAPI_ISteamClient_GetISteamScreenshots:function(const aSelf:PISteamClient;const hSteamuser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamScreenshots; cdecl;
    SteamAPI_ISteamClient_GetIPCCallCount:function(const aSelf:PISteamClient):TSteamUInt32; cdecl;
    SteamAPI_ISteamClient_SetWarningMessageHook:procedure(const aSelf:PISteamClient;const pFunction:TSteamAPIWarningMessageHook_t); cdecl;
    SteamAPI_ISteamClient_BShutdownIfAllPipesClosed:function(const aSelf:PISteamClient):TSteamBool; cdecl;
    SteamAPI_ISteamClient_GetISteamHTTP:function(const aSelf:PISteamClient;const hSteamuser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamHTTP; cdecl;
    SteamAPI_ISteamClient_GetISteamController:function(const aSelf:PISteamClient;const hSteamUser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamController; cdecl;
    SteamAPI_ISteamClient_GetISteamUGC:function(const aSelf:PISteamClient;const hSteamUser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamUGC; cdecl;
    SteamAPI_ISteamClient_GetISteamMusic:function(const aSelf:PISteamClient;const hSteamuser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamMusic; cdecl;
    SteamAPI_ISteamClient_GetISteamHTMLSurface:function(const aSelf:PISteamClient;const hSteamuser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamHTMLSurface; cdecl;
    SteamAPI_ISteamClient_GetISteamInventory:function(const aSelf:PISteamClient;const hSteamuser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamInventory; cdecl;
    SteamAPI_ISteamClient_GetISteamVideo:function(const aSelf:PISteamClient;const hSteamuser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamVideo; cdecl;
    SteamAPI_ISteamClient_GetISteamParentalSettings:function(const aSelf:PISteamClient;const hSteamuser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamParentalSettings; cdecl;
    SteamAPI_ISteamClient_GetISteamInput:function(const aSelf:PISteamClient;const hSteamUser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamInput; cdecl;
    SteamAPI_ISteamClient_GetISteamParties:function(const aSelf:PISteamClient;const hSteamUser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamParties; cdecl;
    SteamAPI_ISteamClient_GetISteamRemotePlay:function(const aSelf:PISteamClient;const hSteamUser:THSteamUser;const hSteamPipe:THSteamPipe;const pchVersion:PSteamChar):PISteamRemotePlay; cdecl;

// ISteamUser
    SteamAPI_SteamUser_v023:function:PISteamUser; cdecl;
    SteamAPI_ISteamUser_GetHSteamUser:function(const aSelf:PISteamUser):THSteamUser; cdecl;
    SteamAPI_ISteamUser_BLoggedOn:function(const aSelf:PISteamUser):TSteamBool; cdecl;
    SteamAPI_ISteamUser_GetSteamID:function(const aSelf:PISteamUser):TSteamUInt64SteamID; cdecl;
    SteamAPI_ISteamUser_InitiateGameConnection_DEPRECATED:function(const aSelf:PISteamUser;const pAuthBlob:TSteamPointer;const cbMaxAuthBlob:TSteamInt32;const steamIDGameServer:TSteamUInt64SteamID;const unIPServer:TSteamUInt32;const usPortServer:TSteamUInt16;const bSecure:TSteamBool):TSteamInt32; cdecl;
    SteamAPI_ISteamUser_TerminateGameConnection_DEPRECATED:procedure(const aSelf:PISteamUser;const unIPServer:TSteamUInt32;const usPortServer:TSteamUInt16); cdecl;
    SteamAPI_ISteamUser_TrackAppUsageEvent:procedure(const aSelf:PISteamUser;const gameID:TSteamUInt64GameID;const eAppUsageEvent:TSteamInt32;const pchExtraInfo:PSteamChar); cdecl;
    SteamAPI_ISteamUser_GetUserDataFolder:function(const aSelf:PISteamUser;const pchBuffer:PSteamChar;const cubBuffer:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUser_StartVoiceRecording:procedure(const aSelf:PISteamUser); cdecl;
    SteamAPI_ISteamUser_StopVoiceRecording:procedure(const aSelf:PISteamUser); cdecl;
    SteamAPI_ISteamUser_GetAvailableVoice:function(const aSelf:PISteamUser;const pcbCompressed:PSteamUInt32;const pcbUncompressed_Deprecated:PSteamUInt32;const nUncompressedVoiceDesiredSampleRate_Deprecated:TSteamUInt32):TEVoiceResult; cdecl;
    SteamAPI_ISteamUser_GetVoice:function(const aSelf:PISteamUser;const bWantCompressed:TSteamBool;const pDestBuffer:TSteamPointer;const cbDestBufferSize:TSteamUInt32;const nBytesWritten:PSteamUInt32;const bWantUncompressed_Deprecated:TSteamBool;const pUncompressedDestBuffer_Deprecated:TSteamPointer;const cbUncompressedDestBufferSize_Deprecated:TSteamUInt32;const nUncompressBytesWritten_Deprecated:PSteamUInt32;const nUncompressedVoiceDesiredSampleRate_Deprecated:TSteamUInt32):TEVoiceResult; cdecl;
    SteamAPI_ISteamUser_DecompressVoice:function(const aSelf:PISteamUser;const pCompressed:TSteamPointer;const cbCompressed:TSteamUInt32;const pDestBuffer:TSteamPointer;const cbDestBufferSize:TSteamUInt32;const nBytesWritten:PSteamUInt32;const nDesiredSampleRate:TSteamUInt32):TEVoiceResult; cdecl;
    SteamAPI_ISteamUser_GetVoiceOptimalSampleRate:function(const aSelf:PISteamUser):TSteamUInt32; cdecl;
    SteamAPI_ISteamUser_GetAuthSessionTicket:function(const aSelf:PISteamUser;const pTicket:TSteamPointer;const cbMaxTicket:TSteamInt32;const pcbTicket:PSteamUInt32;const pSteamNetworkingIdentity:PSteamNetworkingIdentity):THAuthTicket; cdecl;
    SteamAPI_ISteamUser_GetAuthTicketForWebApi:function(const aSelf:PISteamUser;const pchIdentity:PSteamChar):THAuthTicket; cdecl;
    SteamAPI_ISteamUser_BeginAuthSession:function(const aSelf:PISteamUser;const pAuthTicket:TSteamPointer;const cbAuthTicket:TSteamInt32;const steamID:TSteamUInt64SteamID):TEBeginAuthSessionResult; cdecl;
    SteamAPI_ISteamUser_EndAuthSession:procedure(const aSelf:PISteamUser;const steamID:TSteamUInt64SteamID); cdecl;
    SteamAPI_ISteamUser_CancelAuthTicket:procedure(const aSelf:PISteamUser;const hAuthTicket:THAuthTicket); cdecl;
    SteamAPI_ISteamUser_UserHasLicenseForApp:function(const aSelf:PISteamUser;const steamID:TSteamUInt64SteamID;const appID:TAppId_t):TEUserHasLicenseForAppResult; cdecl;
    SteamAPI_ISteamUser_BIsBehindNAT:function(const aSelf:PISteamUser):TSteamBool; cdecl;
    SteamAPI_ISteamUser_AdvertiseGame:procedure(const aSelf:PISteamUser;const steamIDGameServer:TSteamUInt64SteamID;const unIPServer:TSteamUInt32;const usPortServer:TSteamUInt16); cdecl;
    SteamAPI_ISteamUser_RequestEncryptedAppTicket:function(const aSelf:PISteamUser;const pDataToInclude:TSteamPointer;const cbDataToInclude:TSteamInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUser_GetEncryptedAppTicket:function(const aSelf:PISteamUser;const pTicket:TSteamPointer;const cbMaxTicket:TSteamInt32;const pcbTicket:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUser_GetGameBadgeLevel:function(const aSelf:PISteamUser;const nSeries:TSteamInt32;const bFoil:TSteamBool):TSteamInt32; cdecl;
    SteamAPI_ISteamUser_GetPlayerSteamLevel:function(const aSelf:PISteamUser):TSteamInt32; cdecl;
    SteamAPI_ISteamUser_RequestStoreAuthURL:function(const aSelf:PISteamUser;const pchRedirectURL:PSteamChar):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUser_BIsPhoneVerified:function(const aSelf:PISteamUser):TSteamBool; cdecl;
    SteamAPI_ISteamUser_BIsTwoFactorEnabled:function(const aSelf:PISteamUser):TSteamBool; cdecl;
    SteamAPI_ISteamUser_BIsPhoneIdentifying:function(const aSelf:PISteamUser):TSteamBool; cdecl;
    SteamAPI_ISteamUser_BIsPhoneRequiringVerification:function(const aSelf:PISteamUser):TSteamBool; cdecl;
    SteamAPI_ISteamUser_GetMarketEligibility:function(const aSelf:PISteamUser):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUser_GetDurationControl:function(const aSelf:PISteamUser):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUser_BSetDurationControlOnlineState:function(const aSelf:PISteamUser;const eNewState:TEDurationControlOnlineState):TSteamBool; cdecl;

// ISteamFriends
    SteamAPI_SteamFriends_v018:function:PISteamFriends; cdecl;
    SteamAPI_ISteamFriends_GetPersonaName:function(const aSelf:PISteamFriends):PSteamChar; cdecl;
    SteamAPI_ISteamFriends_GetPersonaState:function(const aSelf:PISteamFriends):TEPersonaState; cdecl;
    SteamAPI_ISteamFriends_GetFriendCount:function(const aSelf:PISteamFriends;const iFriendFlags:TSteamInt32):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_GetFriendByIndex:function(const aSelf:PISteamFriends;const iFriend:TSteamInt32;const iFriendFlags:TSteamInt32):TSteamUInt64SteamID; cdecl;
    SteamAPI_ISteamFriends_GetFriendRelationship:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID):TEFriendRelationship; cdecl;
    SteamAPI_ISteamFriends_GetFriendPersonaState:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID):TEPersonaState; cdecl;
    SteamAPI_ISteamFriends_GetFriendPersonaName:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID):PSteamChar; cdecl;
    SteamAPI_ISteamFriends_GetFriendGamePlayed:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID;const pFriendGameInfo:PFriendGameInfo_t):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_GetFriendPersonaNameHistory:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID;const iPersonaName:TSteamInt32):PSteamChar; cdecl;
    SteamAPI_ISteamFriends_GetFriendSteamLevel:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_GetPlayerNickname:function(const aSelf:PISteamFriends;const steamIDPlayer:TSteamUInt64SteamID):PSteamChar; cdecl;
    SteamAPI_ISteamFriends_GetFriendsGroupCount:function(const aSelf:PISteamFriends):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_GetFriendsGroupIDByIndex:function(const aSelf:PISteamFriends;const iFG:TSteamInt32):TFriendsGroupID_t; cdecl;
    SteamAPI_ISteamFriends_GetFriendsGroupName:function(const aSelf:PISteamFriends;const friendsGroupID:TFriendsGroupID_t):PSteamChar; cdecl;
    SteamAPI_ISteamFriends_GetFriendsGroupMembersCount:function(const aSelf:PISteamFriends;const friendsGroupID:TFriendsGroupID_t):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_GetFriendsGroupMembersList:procedure(const aSelf:PISteamFriends;const friendsGroupID:TFriendsGroupID_t;const pOutSteamIDMembers:PCSteamID;const nMembersCount:TSteamInt32); cdecl;
    SteamAPI_ISteamFriends_HasFriend:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID;const iFriendFlags:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_GetClanCount:function(const aSelf:PISteamFriends):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_GetClanByIndex:function(const aSelf:PISteamFriends;const iClan:TSteamInt32):TSteamUInt64SteamID; cdecl;
    SteamAPI_ISteamFriends_GetClanName:function(const aSelf:PISteamFriends;const steamIDClan:TSteamUInt64SteamID):PSteamChar; cdecl;
    SteamAPI_ISteamFriends_GetClanTag:function(const aSelf:PISteamFriends;const steamIDClan:TSteamUInt64SteamID):PSteamChar; cdecl;
    SteamAPI_ISteamFriends_GetClanActivityCounts:function(const aSelf:PISteamFriends;const steamIDClan:TSteamUInt64SteamID;const pnOnline:PSteamInt32;const pnInGame:PSteamInt32;const pnChatting:PSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_DownloadClanActivityCounts:function(const aSelf:PISteamFriends;const psteamIDClans:PCSteamID;const cClansToRequest:TSteamInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamFriends_GetFriendCountFromSource:function(const aSelf:PISteamFriends;const steamIDSource:TSteamUInt64SteamID):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_GetFriendFromSourceByIndex:function(const aSelf:PISteamFriends;const steamIDSource:TSteamUInt64SteamID;const iFriend:TSteamInt32):TSteamUInt64SteamID; cdecl;
    SteamAPI_ISteamFriends_IsUserInSource:function(const aSelf:PISteamFriends;const steamIDUser:TSteamUInt64SteamID;const steamIDSource:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_SetInGameVoiceSpeaking:procedure(const aSelf:PISteamFriends;const steamIDUser:TSteamUInt64SteamID;const bSpeaking:TSteamBool); cdecl;
    SteamAPI_ISteamFriends_ActivateGameOverlay:procedure(const aSelf:PISteamFriends;const pchDialog:PSteamChar); cdecl;
    SteamAPI_ISteamFriends_ActivateGameOverlayToUser:procedure(const aSelf:PISteamFriends;const pchDialog:PSteamChar;const steamID:TSteamUInt64SteamID); cdecl;
    SteamAPI_ISteamFriends_ActivateGameOverlayToWebPage:procedure(const aSelf:PISteamFriends;const pchURL:PSteamChar;const eMode:TEActivateGameOverlayToWebPageMode); cdecl;
    SteamAPI_ISteamFriends_ActivateGameOverlayToStore:procedure(const aSelf:PISteamFriends;const nAppID:TAppId_t;const eFlag:TEOverlayToStoreFlag); cdecl;
    SteamAPI_ISteamFriends_SetPlayedWith:procedure(const aSelf:PISteamFriends;const steamIDUserPlayedWith:TSteamUInt64SteamID); cdecl;
    SteamAPI_ISteamFriends_ActivateGameOverlayInviteDialog:procedure(const aSelf:PISteamFriends;const steamIDLobby:TSteamUInt64SteamID); cdecl;
    SteamAPI_ISteamFriends_GetSmallFriendAvatar:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_GetMediumFriendAvatar:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_GetLargeFriendAvatar:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_RequestUserInformation:function(const aSelf:PISteamFriends;const steamIDUser:TSteamUInt64SteamID;const bRequireNameOnly:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_RequestClanOfficerList:function(const aSelf:PISteamFriends;const steamIDClan:TSteamUInt64SteamID):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamFriends_GetClanOwner:function(const aSelf:PISteamFriends;const steamIDClan:TSteamUInt64SteamID):TSteamUInt64SteamID; cdecl;
    SteamAPI_ISteamFriends_GetClanOfficerCount:function(const aSelf:PISteamFriends;const steamIDClan:TSteamUInt64SteamID):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_GetClanOfficerByIndex:function(const aSelf:PISteamFriends;const steamIDClan:TSteamUInt64SteamID;const iOfficer:TSteamInt32):TSteamUInt64SteamID; cdecl;
    SteamAPI_ISteamFriends_SetRichPresence:function(const aSelf:PISteamFriends;const pchKey:PSteamChar;const pchValue:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_ClearRichPresence:procedure(const aSelf:PISteamFriends); cdecl;
    SteamAPI_ISteamFriends_GetFriendRichPresence:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID;const pchKey:PSteamChar):PSteamChar; cdecl;
    SteamAPI_ISteamFriends_GetFriendRichPresenceKeyCount:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_GetFriendRichPresenceKeyByIndex:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID;const iKey:TSteamInt32):PSteamChar; cdecl;
    SteamAPI_ISteamFriends_RequestFriendRichPresence:procedure(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID); cdecl;
    SteamAPI_ISteamFriends_InviteUserToGame:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID;const pchConnectString:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_GetCoplayFriendCount:function(const aSelf:PISteamFriends):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_GetCoplayFriend:function(const aSelf:PISteamFriends;const iCoplayFriend:TSteamInt32):TSteamUInt64SteamID; cdecl;
    SteamAPI_ISteamFriends_GetFriendCoplayTime:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_GetFriendCoplayGame:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID):TAppId_t; cdecl;
    SteamAPI_ISteamFriends_JoinClanChatRoom:function(const aSelf:PISteamFriends;const steamIDClan:TSteamUInt64SteamID):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamFriends_LeaveClanChatRoom:function(const aSelf:PISteamFriends;const steamIDClan:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_GetClanChatMemberCount:function(const aSelf:PISteamFriends;const steamIDClan:TSteamUInt64SteamID):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_GetChatMemberByIndex:function(const aSelf:PISteamFriends;const steamIDClan:TSteamUInt64SteamID;const iUser:TSteamInt32):TSteamUInt64SteamID; cdecl;
    SteamAPI_ISteamFriends_SendClanChatMessage:function(const aSelf:PISteamFriends;const steamIDClanChat:TSteamUInt64SteamID;const pchText:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_GetClanChatMessage:function(const aSelf:PISteamFriends;const steamIDClanChat:TSteamUInt64SteamID;const iMessage:TSteamInt32;const prgchText:TSteamPointer;const cchTextMax:TSteamInt32;const peChatEntryType:PEChatEntryType;const psteamidChatter:PCSteamID):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_IsClanChatAdmin:function(const aSelf:PISteamFriends;const steamIDClanChat:TSteamUInt64SteamID;const steamIDUser:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_IsClanChatWindowOpenInSteam:function(const aSelf:PISteamFriends;const steamIDClanChat:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_OpenClanChatWindowInSteam:function(const aSelf:PISteamFriends;const steamIDClanChat:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_CloseClanChatWindowInSteam:function(const aSelf:PISteamFriends;const steamIDClanChat:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_SetListenForFriendsMessages:function(const aSelf:PISteamFriends;const bInterceptEnabled:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_ReplyToFriendMessage:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID;const pchMsgToSend:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_GetFriendMessage:function(const aSelf:PISteamFriends;const steamIDFriend:TSteamUInt64SteamID;const iMessageID:TSteamInt32;const pvData:TSteamPointer;const cubData:TSteamInt32;const peChatEntryType:PEChatEntryType):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_GetFollowerCount:function(const aSelf:PISteamFriends;const steamID:TSteamUInt64SteamID):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamFriends_IsFollowing:function(const aSelf:PISteamFriends;const steamID:TSteamUInt64SteamID):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamFriends_EnumerateFollowingList:function(const aSelf:PISteamFriends;const unStartIndex:TSteamUInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamFriends_IsClanPublic:function(const aSelf:PISteamFriends;const steamIDClan:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_IsClanOfficialGameGroup:function(const aSelf:PISteamFriends;const steamIDClan:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_GetNumChatsWithUnreadPriorityMessages:function(const aSelf:PISteamFriends):TSteamInt32; cdecl;
    SteamAPI_ISteamFriends_ActivateGameOverlayRemotePlayTogetherInviteDialog:procedure(const aSelf:PISteamFriends;const steamIDLobby:TSteamUInt64SteamID); cdecl;
    SteamAPI_ISteamFriends_RegisterProtocolInOverlayBrowser:function(const aSelf:PISteamFriends;const pchProtocol:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_ActivateGameOverlayInviteDialogConnectString:procedure(const aSelf:PISteamFriends;const pchConnectString:PSteamChar); cdecl;
    SteamAPI_ISteamFriends_RequestEquippedProfileItems:function(const aSelf:PISteamFriends;const steamID:TSteamUInt64SteamID):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamFriends_BHasEquippedProfileItem:function(const aSelf:PISteamFriends;const steamID:TSteamUInt64SteamID;const itemType:TECommunityProfileItemType):TSteamBool; cdecl;
    SteamAPI_ISteamFriends_GetProfileItemPropertyString:function(const aSelf:PISteamFriends;const steamID:TSteamUInt64SteamID;const itemType:TECommunityProfileItemType;const prop:TECommunityProfileItemProperty):PSteamChar; cdecl;
    SteamAPI_ISteamFriends_GetProfileItemPropertyUint:function(const aSelf:PISteamFriends;const steamID:TSteamUInt64SteamID;const itemType:TECommunityProfileItemType;const prop:TECommunityProfileItemProperty):TSteamUInt32; cdecl;

// ISteamUtils
    SteamAPI_SteamUtils_v010:function:PISteamUtils; cdecl;
    SteamAPI_SteamGameServerUtils_v010:function:PISteamUtils; cdecl;
    SteamAPI_ISteamUtils_GetSecondsSinceAppActive:function(const aSelf:PISteamUtils):TSteamUInt32; cdecl;
    SteamAPI_ISteamUtils_GetSecondsSinceComputerActive:function(const aSelf:PISteamUtils):TSteamUInt32; cdecl;
    SteamAPI_ISteamUtils_GetConnectedUniverse:function(const aSelf:PISteamUtils):TEUniverse; cdecl;
    SteamAPI_ISteamUtils_GetServerRealTime:function(const aSelf:PISteamUtils):TSteamUInt32; cdecl;
    SteamAPI_ISteamUtils_GetIPCountry:function(const aSelf:PISteamUtils):PSteamChar; cdecl;
    SteamAPI_ISteamUtils_GetImageSize:function(const aSelf:PISteamUtils;const iImage:TSteamInt32;const pnWidth:PSteamUInt32;const pnHeight:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_GetImageRGBA:function(const aSelf:PISteamUtils;const iImage:TSteamInt32;const pubDest:PSteamUInt8;const nDestBufferSize:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_GetCurrentBatteryPower:function(const aSelf:PISteamUtils):TSteamUInt8; cdecl;
    SteamAPI_ISteamUtils_GetAppID:function(const aSelf:PISteamUtils):TSteamUInt32; cdecl;
    SteamAPI_ISteamUtils_SetOverlayNotificationPosition:procedure(const aSelf:PISteamUtils;const eNotificationPosition:TENotificationPosition); cdecl;
    SteamAPI_ISteamUtils_IsAPICallCompleted:function(const aSelf:PISteamUtils;const hSteamAPICall:TSteamAPICall_t;const pbFailed:PSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_GetAPICallFailureReason:function(const aSelf:PISteamUtils;const hSteamAPICall:TSteamAPICall_t):TESteamAPICallFailure; cdecl;
    SteamAPI_ISteamUtils_GetAPICallResult:function(const aSelf:PISteamUtils;const hSteamAPICall:TSteamAPICall_t;const pCallback:TSteamPointer;const cubCallback:TSteamInt32;const iCallbackExpected:TSteamInt32;const pbFailed:PSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_GetIPCCallCount:function(const aSelf:PISteamUtils):TSteamUInt32; cdecl;
    SteamAPI_ISteamUtils_SetWarningMessageHook:procedure(const aSelf:PISteamUtils;const pFunction:TSteamAPIWarningMessageHook_t); cdecl;
    SteamAPI_ISteamUtils_IsOverlayEnabled:function(const aSelf:PISteamUtils):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_BOverlayNeedsPresent:function(const aSelf:PISteamUtils):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_CheckFileSignature:function(const aSelf:PISteamUtils;const szFileName:PSteamChar):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUtils_ShowGamepadTextInput:function(const aSelf:PISteamUtils;const eInputMode:TEGamepadTextInputMode;const eLineInputMode:TEGamepadTextInputLineMode;const pchDescription:PSteamChar;const unCharMax:TSteamUInt32;const pchExistingText:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_GetEnteredGamepadTextLength:function(const aSelf:PISteamUtils):TSteamUInt32; cdecl;
    SteamAPI_ISteamUtils_GetEnteredGamepadTextInput:function(const aSelf:PISteamUtils;const pchText:PSteamChar;const cchText:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_GetSteamUILanguage:function(const aSelf:PISteamUtils):PSteamChar; cdecl;
    SteamAPI_ISteamUtils_IsSteamRunningInVR:function(const aSelf:PISteamUtils):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_SetOverlayNotificationInset:procedure(const aSelf:PISteamUtils;const nHorizontalInset:TSteamInt32;const nVerticalInset:TSteamInt32); cdecl;
    SteamAPI_ISteamUtils_IsSteamInBigPictureMode:function(const aSelf:PISteamUtils):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_StartVRDashboard:procedure(const aSelf:PISteamUtils); cdecl;
    SteamAPI_ISteamUtils_IsVRHeadsetStreamingEnabled:function(const aSelf:PISteamUtils):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_SetVRHeadsetStreamingEnabled:procedure(const aSelf:PISteamUtils;const bEnabled:TSteamBool); cdecl;
    SteamAPI_ISteamUtils_IsSteamChinaLauncher:function(const aSelf:PISteamUtils):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_InitFilterText:function(const aSelf:PISteamUtils;const unFilterOptions:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_FilterText:function(const aSelf:PISteamUtils;const eContext:TETextFilteringContext;const sourceSteamID:TSteamUInt64SteamID;const pchInputMessage:PSteamChar;const pchOutFilteredText:PSteamChar;const nByteSizeOutFilteredText:TSteamUInt32):TSteamInt32; cdecl;
    SteamAPI_ISteamUtils_GetIPv6ConnectivityState:function(const aSelf:PISteamUtils;const eProtocol:TESteamIPv6ConnectivityProtocol):TESteamIPv6ConnectivityState; cdecl;
    SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck:function(const aSelf:PISteamUtils):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_ShowFloatingGamepadTextInput:function(const aSelf:PISteamUtils;const eKeyboardMode:TEFloatingGamepadTextInputMode;const nTextFieldXPosition:TSteamInt32;const nTextFieldYPosition:TSteamInt32;const nTextFieldWidth:TSteamInt32;const nTextFieldHeight:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_SetGameLauncherMode:procedure(const aSelf:PISteamUtils;const bLauncherMode:TSteamBool); cdecl;
    SteamAPI_ISteamUtils_DismissFloatingGamepadTextInput:function(const aSelf:PISteamUtils):TSteamBool; cdecl;
    SteamAPI_ISteamUtils_DismissGamepadTextInput:function(const aSelf:PISteamUtils):TSteamBool; cdecl;

// ISteamMatchmaking
    SteamAPI_SteamMatchmaking_v009:function:PISteamMatchmaking; cdecl;
    SteamAPI_ISteamMatchmaking_GetFavoriteGameCount:function(const aSelf:PISteamMatchmaking):TSteamInt32; cdecl;
    SteamAPI_ISteamMatchmaking_GetFavoriteGame:function(const aSelf:PISteamMatchmaking;const iGame:TSteamInt32;const pnAppID:PAppId_t;const pnIP:PSteamUInt32;const pnConnPort:PSteamUInt16;const pnQueryPort:PSteamUInt16;const punFlags:PSteamUInt32;const pRTime32LastPlayedOnServer:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamMatchmaking_AddFavoriteGame:function(const aSelf:PISteamMatchmaking;const nAppID:TAppId_t;const nIP:TSteamUInt32;const nConnPort:TSteamUInt16;const nQueryPort:TSteamUInt16;const unFlags:TSteamUInt32;const rTime32LastPlayedOnServer:TSteamUInt32):TSteamInt32; cdecl;
    SteamAPI_ISteamMatchmaking_RemoveFavoriteGame:function(const aSelf:PISteamMatchmaking;const nAppID:TAppId_t;const nIP:TSteamUInt32;const nConnPort:TSteamUInt16;const nQueryPort:TSteamUInt16;const unFlags:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamMatchmaking_RequestLobbyList:function(const aSelf:PISteamMatchmaking):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamMatchmaking_AddRequestLobbyListStringFilter:procedure(const aSelf:PISteamMatchmaking;const pchKeyToMatch:PSteamChar;const pchValueToMatch:PSteamChar;const eComparisonType:TELobbyComparison); cdecl;
    SteamAPI_ISteamMatchmaking_AddRequestLobbyListNumericalFilter:procedure(const aSelf:PISteamMatchmaking;const pchKeyToMatch:PSteamChar;const nValueToMatch:TSteamInt32;const eComparisonType:TELobbyComparison); cdecl;
    SteamAPI_ISteamMatchmaking_AddRequestLobbyListNearValueFilter:procedure(const aSelf:PISteamMatchmaking;const pchKeyToMatch:PSteamChar;const nValueToBeCloseTo:TSteamInt32); cdecl;
    SteamAPI_ISteamMatchmaking_AddRequestLobbyListFilterSlotsAvailable:procedure(const aSelf:PISteamMatchmaking;const nSlotsAvailable:TSteamInt32); cdecl;
    SteamAPI_ISteamMatchmaking_AddRequestLobbyListDistanceFilter:procedure(const aSelf:PISteamMatchmaking;const eLobbyDistanceFilter:TELobbyDistanceFilter); cdecl;
    SteamAPI_ISteamMatchmaking_AddRequestLobbyListResultCountFilter:procedure(const aSelf:PISteamMatchmaking;const cMaxResults:TSteamInt32); cdecl;
    SteamAPI_ISteamMatchmaking_AddRequestLobbyListCompatibleMembersFilter:procedure(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID); cdecl;
    SteamAPI_ISteamMatchmaking_GetLobbyByIndex:function(const aSelf:PISteamMatchmaking;const iLobby:TSteamInt32):TSteamUInt64SteamID; cdecl;
    SteamAPI_ISteamMatchmaking_CreateLobby:function(const aSelf:PISteamMatchmaking;const eLobbyType:TELobbyType;const cMaxMembers:TSteamInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamMatchmaking_JoinLobby:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamMatchmaking_LeaveLobby:procedure(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID); cdecl;
    SteamAPI_ISteamMatchmaking_InviteUserToLobby:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const steamIDInvitee:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamMatchmaking_GetNumLobbyMembers:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID):TSteamInt32; cdecl;
    SteamAPI_ISteamMatchmaking_GetLobbyMemberByIndex:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const iMember:TSteamInt32):TSteamUInt64SteamID; cdecl;
    SteamAPI_ISteamMatchmaking_GetLobbyData:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const pchKey:PSteamChar):PSteamChar; cdecl;
    SteamAPI_ISteamMatchmaking_SetLobbyData:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const pchKey:PSteamChar;const pchValue:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamMatchmaking_GetLobbyDataCount:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID):TSteamInt32; cdecl;
    SteamAPI_ISteamMatchmaking_GetLobbyDataByIndex:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const iLobbyData:TSteamInt32;const pchKey:PSteamChar;const cchKeyBufferSize:TSteamInt32;const pchValue:PSteamChar;const cchValueBufferSize:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamMatchmaking_DeleteLobbyData:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const pchKey:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamMatchmaking_GetLobbyMemberData:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const steamIDUser:TSteamUInt64SteamID;const pchKey:PSteamChar):PSteamChar; cdecl;
    SteamAPI_ISteamMatchmaking_SetLobbyMemberData:procedure(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const pchKey:PSteamChar;const pchValue:PSteamChar); cdecl;
    SteamAPI_ISteamMatchmaking_SendLobbyChatMsg:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const pvMsgBody:TSteamPointer;const cubMsgBody:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamMatchmaking_GetLobbyChatEntry:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const iChatID:TSteamInt32;const pSteamIDUser:PCSteamID;const pvData:TSteamPointer;const cubData:TSteamInt32;const peChatEntryType:PEChatEntryType):TSteamInt32; cdecl;
    SteamAPI_ISteamMatchmaking_RequestLobbyData:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamMatchmaking_SetLobbyGameServer:procedure(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const unGameServerIP:TSteamUInt32;const unGameServerPort:TSteamUInt16;const steamIDGameServer:TSteamUInt64SteamID); cdecl;
    SteamAPI_ISteamMatchmaking_GetLobbyGameServer:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const punGameServerIP:PSteamUInt32;const punGameServerPort:PSteamUInt16;const psteamIDGameServer:PCSteamID):TSteamBool; cdecl;
    SteamAPI_ISteamMatchmaking_SetLobbyMemberLimit:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const cMaxMembers:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamMatchmaking_GetLobbyMemberLimit:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID):TSteamInt32; cdecl;
    SteamAPI_ISteamMatchmaking_SetLobbyType:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const eLobbyType:TELobbyType):TSteamBool; cdecl;
    SteamAPI_ISteamMatchmaking_SetLobbyJoinable:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const bLobbyJoinable:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamMatchmaking_GetLobbyOwner:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID):TSteamUInt64SteamID; cdecl;
    SteamAPI_ISteamMatchmaking_SetLobbyOwner:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const steamIDNewOwner:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamMatchmaking_SetLinkedLobby:function(const aSelf:PISteamMatchmaking;const steamIDLobby:TSteamUInt64SteamID;const steamIDLobbyDependent:TSteamUInt64SteamID):TSteamBool; cdecl;

// ISteamMatchmakingServerListResponse
    SteamAPI_ISteamMatchmakingServerListResponse_ServerResponded:procedure(const aSelf:PISteamMatchmakingServerListResponse;const hRequest:THServerListRequest;const iServer:TSteamInt32); cdecl;
    SteamAPI_ISteamMatchmakingServerListResponse_ServerFailedToRespond:procedure(const aSelf:PISteamMatchmakingServerListResponse;const hRequest:THServerListRequest;const iServer:TSteamInt32); cdecl;
    SteamAPI_ISteamMatchmakingServerListResponse_RefreshComplete:procedure(const aSelf:PISteamMatchmakingServerListResponse;const hRequest:THServerListRequest;const response:TEMatchMakingServerResponse); cdecl;

// ISteamMatchmakingPingResponse
    SteamAPI_ISteamMatchmakingPingResponse_ServerResponded:procedure(const aSelf:PISteamMatchmakingPingResponse;const server:Pgameserveritem_t); cdecl;
    SteamAPI_ISteamMatchmakingPingResponse_ServerFailedToRespond:procedure(const aSelf:PISteamMatchmakingPingResponse); cdecl;

// ISteamMatchmakingPlayersResponse
    SteamAPI_ISteamMatchmakingPlayersResponse_AddPlayerToList:procedure(const aSelf:PISteamMatchmakingPlayersResponse;const pchName:PSteamChar;const nScore:TSteamInt32;const flTimePlayed:TSteamFloat); cdecl;
    SteamAPI_ISteamMatchmakingPlayersResponse_PlayersFailedToRespond:procedure(const aSelf:PISteamMatchmakingPlayersResponse); cdecl;
    SteamAPI_ISteamMatchmakingPlayersResponse_PlayersRefreshComplete:procedure(const aSelf:PISteamMatchmakingPlayersResponse); cdecl;

// ISteamMatchmakingRulesResponse
    SteamAPI_ISteamMatchmakingRulesResponse_RulesResponded:procedure(const aSelf:PISteamMatchmakingRulesResponse;const pchRule:PSteamChar;const pchValue:PSteamChar); cdecl;
    SteamAPI_ISteamMatchmakingRulesResponse_RulesFailedToRespond:procedure(const aSelf:PISteamMatchmakingRulesResponse); cdecl;
    SteamAPI_ISteamMatchmakingRulesResponse_RulesRefreshComplete:procedure(const aSelf:PISteamMatchmakingRulesResponse); cdecl;

// ISteamMatchmakingServers
    SteamAPI_SteamMatchmakingServers_v002:function:PISteamMatchmakingServers; cdecl;
    SteamAPI_ISteamMatchmakingServers_RequestInternetServerList:function(const aSelf:PISteamMatchmakingServers;const iApp:TAppId_t;const ppchFilters:PPMatchMakingKeyValuePair_t;const nFilters:TSteamUInt32;const pRequestServersResponse:PISteamMatchmakingServerListResponse):THServerListRequest; cdecl;
    SteamAPI_ISteamMatchmakingServers_RequestLANServerList:function(const aSelf:PISteamMatchmakingServers;const iApp:TAppId_t;const pRequestServersResponse:PISteamMatchmakingServerListResponse):THServerListRequest; cdecl;
    SteamAPI_ISteamMatchmakingServers_RequestFriendsServerList:function(const aSelf:PISteamMatchmakingServers;const iApp:TAppId_t;const ppchFilters:PPMatchMakingKeyValuePair_t;const nFilters:TSteamUInt32;const pRequestServersResponse:PISteamMatchmakingServerListResponse):THServerListRequest; cdecl;
    SteamAPI_ISteamMatchmakingServers_RequestFavoritesServerList:function(const aSelf:PISteamMatchmakingServers;const iApp:TAppId_t;const ppchFilters:PPMatchMakingKeyValuePair_t;const nFilters:TSteamUInt32;const pRequestServersResponse:PISteamMatchmakingServerListResponse):THServerListRequest; cdecl;
    SteamAPI_ISteamMatchmakingServers_RequestHistoryServerList:function(const aSelf:PISteamMatchmakingServers;const iApp:TAppId_t;const ppchFilters:PPMatchMakingKeyValuePair_t;const nFilters:TSteamUInt32;const pRequestServersResponse:PISteamMatchmakingServerListResponse):THServerListRequest; cdecl;
    SteamAPI_ISteamMatchmakingServers_RequestSpectatorServerList:function(const aSelf:PISteamMatchmakingServers;const iApp:TAppId_t;const ppchFilters:PPMatchMakingKeyValuePair_t;const nFilters:TSteamUInt32;const pRequestServersResponse:PISteamMatchmakingServerListResponse):THServerListRequest; cdecl;
    SteamAPI_ISteamMatchmakingServers_ReleaseRequest:procedure(const aSelf:PISteamMatchmakingServers;const hServerListRequest:THServerListRequest); cdecl;
    SteamAPI_ISteamMatchmakingServers_GetServerDetails:function(const aSelf:PISteamMatchmakingServers;const hRequest:THServerListRequest;const iServer:TSteamInt32):Pgameserveritem_t; cdecl;
    SteamAPI_ISteamMatchmakingServers_CancelQuery:procedure(const aSelf:PISteamMatchmakingServers;const hRequest:THServerListRequest); cdecl;
    SteamAPI_ISteamMatchmakingServers_RefreshQuery:procedure(const aSelf:PISteamMatchmakingServers;const hRequest:THServerListRequest); cdecl;
    SteamAPI_ISteamMatchmakingServers_IsRefreshing:function(const aSelf:PISteamMatchmakingServers;const hRequest:THServerListRequest):TSteamBool; cdecl;
    SteamAPI_ISteamMatchmakingServers_GetServerCount:function(const aSelf:PISteamMatchmakingServers;const hRequest:THServerListRequest):TSteamInt32; cdecl;
    SteamAPI_ISteamMatchmakingServers_RefreshServer:procedure(const aSelf:PISteamMatchmakingServers;const hRequest:THServerListRequest;const iServer:TSteamInt32); cdecl;
    SteamAPI_ISteamMatchmakingServers_PingServer:function(const aSelf:PISteamMatchmakingServers;const unIP:TSteamUInt32;const usPort:TSteamUInt16;const pRequestServersResponse:PISteamMatchmakingPingResponse):THServerQuery; cdecl;
    SteamAPI_ISteamMatchmakingServers_PlayerDetails:function(const aSelf:PISteamMatchmakingServers;const unIP:TSteamUInt32;const usPort:TSteamUInt16;const pRequestServersResponse:PISteamMatchmakingPlayersResponse):THServerQuery; cdecl;
    SteamAPI_ISteamMatchmakingServers_ServerRules:function(const aSelf:PISteamMatchmakingServers;const unIP:TSteamUInt32;const usPort:TSteamUInt16;const pRequestServersResponse:PISteamMatchmakingRulesResponse):THServerQuery; cdecl;
    SteamAPI_ISteamMatchmakingServers_CancelServerQuery:procedure(const aSelf:PISteamMatchmakingServers;const hServerQuery:THServerQuery); cdecl;

// ISteamParties
    SteamAPI_SteamParties_v002:function:PISteamParties; cdecl;
    SteamAPI_ISteamParties_GetNumActiveBeacons:function(const aSelf:PISteamParties):TSteamUInt32; cdecl;
    SteamAPI_ISteamParties_GetBeaconByIndex:function(const aSelf:PISteamParties;const unIndex:TSteamUInt32):TPartyBeaconID_t; cdecl;
    SteamAPI_ISteamParties_GetBeaconDetails:function(const aSelf:PISteamParties;const ulBeaconID:TPartyBeaconID_t;const pSteamIDBeaconOwner:PCSteamID;const pLocation:PSteamPartyBeaconLocation_t;const pchMetadata:PSteamChar;const cchMetadata:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamParties_JoinParty:function(const aSelf:PISteamParties;const ulBeaconID:TPartyBeaconID_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamParties_GetNumAvailableBeaconLocations:function(const aSelf:PISteamParties;const puNumLocations:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamParties_GetAvailableBeaconLocations:function(const aSelf:PISteamParties;const pLocationList:PSteamPartyBeaconLocation_t;const uMaxNumLocations:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamParties_CreateBeacon:function(const aSelf:PISteamParties;const unOpenSlots:TSteamUInt32;const pBeaconLocation:PSteamPartyBeaconLocation_t;const pchConnectString:PSteamChar;const pchMetadata:PSteamChar):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamParties_OnReservationCompleted:procedure(const aSelf:PISteamParties;const ulBeacon:TPartyBeaconID_t;const steamIDUser:TSteamUInt64SteamID); cdecl;
    SteamAPI_ISteamParties_CancelReservation:procedure(const aSelf:PISteamParties;const ulBeacon:TPartyBeaconID_t;const steamIDUser:TSteamUInt64SteamID); cdecl;
    SteamAPI_ISteamParties_ChangeNumOpenSlots:function(const aSelf:PISteamParties;const ulBeacon:TPartyBeaconID_t;const unOpenSlots:TSteamUInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamParties_DestroyBeacon:function(const aSelf:PISteamParties;const ulBeacon:TPartyBeaconID_t):TSteamBool; cdecl;
    SteamAPI_ISteamParties_GetBeaconLocationData:function(const aSelf:PISteamParties;const BeaconLocation:TSteamPartyBeaconLocation_t;const eData:TESteamPartyBeaconLocationData;const pchDataStringOut:PSteamChar;const cchDataStringOut:TSteamInt32):TSteamBool; cdecl;

// ISteamRemoteStorage
    SteamAPI_SteamRemoteStorage_v016:function:PISteamRemoteStorage; cdecl;
    SteamAPI_ISteamRemoteStorage_FileWrite:function(const aSelf:PISteamRemoteStorage;const pchFile:PSteamChar;const pvData:TSteamPointer;const cubData:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_FileRead:function(const aSelf:PISteamRemoteStorage;const pchFile:PSteamChar;const pvData:TSteamPointer;const cubDataToRead:TSteamInt32):TSteamInt32; cdecl;
    SteamAPI_ISteamRemoteStorage_FileWriteAsync:function(const aSelf:PISteamRemoteStorage;const pchFile:PSteamChar;const pvData:TSteamPointer;const cubData:TSteamUInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_FileReadAsync:function(const aSelf:PISteamRemoteStorage;const pchFile:PSteamChar;const nOffset:TSteamUInt32;const cubToRead:TSteamUInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_FileReadAsyncComplete:function(const aSelf:PISteamRemoteStorage;const hReadCall:TSteamAPICall_t;const pvBuffer:TSteamPointer;const cubToRead:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_FileForget:function(const aSelf:PISteamRemoteStorage;const pchFile:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_FileDelete:function(const aSelf:PISteamRemoteStorage;const pchFile:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_FileShare:function(const aSelf:PISteamRemoteStorage;const pchFile:PSteamChar):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_SetSyncPlatforms:function(const aSelf:PISteamRemoteStorage;const pchFile:PSteamChar;const eRemoteStoragePlatform:TERemoteStoragePlatform):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_FileWriteStreamOpen:function(const aSelf:PISteamRemoteStorage;const pchFile:PSteamChar):TUGCFileWriteStreamHandle_t; cdecl;
    SteamAPI_ISteamRemoteStorage_FileWriteStreamWriteChunk:function(const aSelf:PISteamRemoteStorage;const writeHandle:TUGCFileWriteStreamHandle_t;const pvData:TSteamPointer;const cubData:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_FileWriteStreamClose:function(const aSelf:PISteamRemoteStorage;const writeHandle:TUGCFileWriteStreamHandle_t):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_FileWriteStreamCancel:function(const aSelf:PISteamRemoteStorage;const writeHandle:TUGCFileWriteStreamHandle_t):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_FileExists:function(const aSelf:PISteamRemoteStorage;const pchFile:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_FilePersisted:function(const aSelf:PISteamRemoteStorage;const pchFile:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_GetFileSize:function(const aSelf:PISteamRemoteStorage;const pchFile:PSteamChar):TSteamInt32; cdecl;
    SteamAPI_ISteamRemoteStorage_GetFileTimestamp:function(const aSelf:PISteamRemoteStorage;const pchFile:PSteamChar):TSteamInt64; cdecl;
    SteamAPI_ISteamRemoteStorage_GetSyncPlatforms:function(const aSelf:PISteamRemoteStorage;const pchFile:PSteamChar):TERemoteStoragePlatform; cdecl;
    SteamAPI_ISteamRemoteStorage_GetFileCount:function(const aSelf:PISteamRemoteStorage):TSteamInt32; cdecl;
    SteamAPI_ISteamRemoteStorage_GetFileNameAndSize:function(const aSelf:PISteamRemoteStorage;const iFile:TSteamInt32;const pnFileSizeInBytes:PSteamInt32):PSteamChar; cdecl;
    SteamAPI_ISteamRemoteStorage_GetQuota:function(const aSelf:PISteamRemoteStorage;const pnTotalBytes:PSteamUInt64;const puAvailableBytes:PSteamUInt64):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_IsCloudEnabledForAccount:function(const aSelf:PISteamRemoteStorage):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_IsCloudEnabledForApp:function(const aSelf:PISteamRemoteStorage):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_SetCloudEnabledForApp:procedure(const aSelf:PISteamRemoteStorage;const bEnabled:TSteamBool); cdecl;
    SteamAPI_ISteamRemoteStorage_UGCDownload:function(const aSelf:PISteamRemoteStorage;const hContent:TUGCHandle_t;const unPriority:TSteamUInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_GetUGCDownloadProgress:function(const aSelf:PISteamRemoteStorage;const hContent:TUGCHandle_t;const pnBytesDownloaded:PSteamInt32;const pnBytesExpected:PSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_GetUGCDetails:function(const aSelf:PISteamRemoteStorage;const hContent:TUGCHandle_t;const pnAppID:PAppId_t;const ppchName:PPSteamChar;const pnFileSizeInBytes:PSteamInt32;const pSteamIDOwner:PCSteamID):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_UGCRead:function(const aSelf:PISteamRemoteStorage;const hContent:TUGCHandle_t;const pvData:TSteamPointer;const cubDataToRead:TSteamInt32;const cOffset:TSteamUInt32;const eAction:TEUGCReadAction):TSteamInt32; cdecl;
    SteamAPI_ISteamRemoteStorage_GetCachedUGCCount:function(const aSelf:PISteamRemoteStorage):TSteamInt32; cdecl;
    SteamAPI_ISteamRemoteStorage_GetCachedUGCHandle:function(const aSelf:PISteamRemoteStorage;const iCachedContent:TSteamInt32):TUGCHandle_t; cdecl;
    SteamAPI_ISteamRemoteStorage_PublishWorkshopFile:function(const aSelf:PISteamRemoteStorage;const pchFile:PSteamChar;const pchPreviewFile:PSteamChar;const nConsumerAppId:TAppId_t;const pchTitle:PSteamChar;const pchDescription:PSteamChar;const eVisibility:TERemoteStoragePublishedFileVisibility;const pTags:PSteamParamStringArray_t;const eWorkshopFileType:TEWorkshopFileType):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_CreatePublishedFileUpdateRequest:function(const aSelf:PISteamRemoteStorage;const unPublishedFileId:TPublishedFileId_t):TPublishedFileUpdateHandle_t; cdecl;
    SteamAPI_ISteamRemoteStorage_UpdatePublishedFileFile:function(const aSelf:PISteamRemoteStorage;const updateHandle:TPublishedFileUpdateHandle_t;const pchFile:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_UpdatePublishedFilePreviewFile:function(const aSelf:PISteamRemoteStorage;const updateHandle:TPublishedFileUpdateHandle_t;const pchPreviewFile:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_UpdatePublishedFileTitle:function(const aSelf:PISteamRemoteStorage;const updateHandle:TPublishedFileUpdateHandle_t;const pchTitle:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_UpdatePublishedFileDescription:function(const aSelf:PISteamRemoteStorage;const updateHandle:TPublishedFileUpdateHandle_t;const pchDescription:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_UpdatePublishedFileVisibility:function(const aSelf:PISteamRemoteStorage;const updateHandle:TPublishedFileUpdateHandle_t;const eVisibility:TERemoteStoragePublishedFileVisibility):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_UpdatePublishedFileTags:function(const aSelf:PISteamRemoteStorage;const updateHandle:TPublishedFileUpdateHandle_t;const pTags:PSteamParamStringArray_t):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_CommitPublishedFileUpdate:function(const aSelf:PISteamRemoteStorage;const updateHandle:TPublishedFileUpdateHandle_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_GetPublishedFileDetails:function(const aSelf:PISteamRemoteStorage;const unPublishedFileId:TPublishedFileId_t;const unMaxSecondsOld:TSteamUInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_DeletePublishedFile:function(const aSelf:PISteamRemoteStorage;const unPublishedFileId:TPublishedFileId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_EnumerateUserPublishedFiles:function(const aSelf:PISteamRemoteStorage;const unStartIndex:TSteamUInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_SubscribePublishedFile:function(const aSelf:PISteamRemoteStorage;const unPublishedFileId:TPublishedFileId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_EnumerateUserSubscribedFiles:function(const aSelf:PISteamRemoteStorage;const unStartIndex:TSteamUInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_UnsubscribePublishedFile:function(const aSelf:PISteamRemoteStorage;const unPublishedFileId:TPublishedFileId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_UpdatePublishedFileSetChangeDescription:function(const aSelf:PISteamRemoteStorage;const updateHandle:TPublishedFileUpdateHandle_t;const pchChangeDescription:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_GetPublishedItemVoteDetails:function(const aSelf:PISteamRemoteStorage;const unPublishedFileId:TPublishedFileId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_UpdateUserPublishedItemVote:function(const aSelf:PISteamRemoteStorage;const unPublishedFileId:TPublishedFileId_t;const bVoteUp:TSteamBool):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_GetUserPublishedItemVoteDetails:function(const aSelf:PISteamRemoteStorage;const unPublishedFileId:TPublishedFileId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_EnumerateUserSharedWorkshopFiles:function(const aSelf:PISteamRemoteStorage;const steamId:TSteamUInt64SteamID;const unStartIndex:TSteamUInt32;const pRequiredTags:PSteamParamStringArray_t;const pExcludedTags:PSteamParamStringArray_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_PublishVideo:function(const aSelf:PISteamRemoteStorage;const eVideoProvider:TEWorkshopVideoProvider;const pchVideoAccount:PSteamChar;const pchVideoIdentifier:PSteamChar;const pchPreviewFile:PSteamChar;const nConsumerAppId:TAppId_t;const pchTitle:PSteamChar;const pchDescription:PSteamChar;const eVisibility:TERemoteStoragePublishedFileVisibility;const pTags:PSteamParamStringArray_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_SetUserPublishedFileAction:function(const aSelf:PISteamRemoteStorage;const unPublishedFileId:TPublishedFileId_t;const eAction:TEWorkshopFileAction):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_EnumeratePublishedFilesByUserAction:function(const aSelf:PISteamRemoteStorage;const eAction:TEWorkshopFileAction;const unStartIndex:TSteamUInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_EnumeratePublishedWorkshopFiles:function(const aSelf:PISteamRemoteStorage;const eEnumerationType:TEWorkshopEnumerationType;const unStartIndex:TSteamUInt32;const unCount:TSteamUInt32;const unDays:TSteamUInt32;const pTags:PSteamParamStringArray_t;const pUserTags:PSteamParamStringArray_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_UGCDownloadToLocation:function(const aSelf:PISteamRemoteStorage;const hContent:TUGCHandle_t;const pchLocation:PSteamChar;const unPriority:TSteamUInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamRemoteStorage_GetLocalFileChangeCount:function(const aSelf:PISteamRemoteStorage):TSteamInt32; cdecl;
    SteamAPI_ISteamRemoteStorage_GetLocalFileChange:function(const aSelf:PISteamRemoteStorage;const iFile:TSteamInt32;const pEChangeType:PERemoteStorageLocalFileChange;const pEFilePathType:PERemoteStorageFilePathType):PSteamChar; cdecl;
    SteamAPI_ISteamRemoteStorage_BeginFileWriteBatch:function(const aSelf:PISteamRemoteStorage):TSteamBool; cdecl;
    SteamAPI_ISteamRemoteStorage_EndFileWriteBatch:function(const aSelf:PISteamRemoteStorage):TSteamBool; cdecl;

// ISteamUserStats
    SteamAPI_SteamUserStats_v013:function:PISteamUserStats; cdecl;
    SteamAPI_ISteamUserStats_GetStatInt32:function(const aSelf:PISteamUserStats;const pchName:PSteamChar;const pData:PSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_GetStatFloat:function(const aSelf:PISteamUserStats;const pchName:PSteamChar;const pData:PSteamFloat):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_SetStatInt32:function(const aSelf:PISteamUserStats;const pchName:PSteamChar;const nData:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_SetStatFloat:function(const aSelf:PISteamUserStats;const pchName:PSteamChar;const fData:TSteamFloat):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_UpdateAvgRateStat:function(const aSelf:PISteamUserStats;const pchName:PSteamChar;const flCountThisSession:TSteamFloat;const dSessionLength:TSteamDouble):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_GetAchievement:function(const aSelf:PISteamUserStats;const pchName:PSteamChar;const pbAchieved:PSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_SetAchievement:function(const aSelf:PISteamUserStats;const pchName:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_ClearAchievement:function(const aSelf:PISteamUserStats;const pchName:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_GetAchievementAndUnlockTime:function(const aSelf:PISteamUserStats;const pchName:PSteamChar;const pbAchieved:PSteamBool;const punUnlockTime:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_StoreStats:function(const aSelf:PISteamUserStats):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_GetAchievementIcon:function(const aSelf:PISteamUserStats;const pchName:PSteamChar):TSteamInt32; cdecl;
    SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute:function(const aSelf:PISteamUserStats;const pchName:PSteamChar;const pchKey:PSteamChar):PSteamChar; cdecl;
    SteamAPI_ISteamUserStats_IndicateAchievementProgress:function(const aSelf:PISteamUserStats;const pchName:PSteamChar;const nCurProgress:TSteamUInt32;const nMaxProgress:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_GetNumAchievements:function(const aSelf:PISteamUserStats):TSteamUInt32; cdecl;
    SteamAPI_ISteamUserStats_GetAchievementName:function(const aSelf:PISteamUserStats;const iAchievement:TSteamUInt32):PSteamChar; cdecl;
    SteamAPI_ISteamUserStats_RequestUserStats:function(const aSelf:PISteamUserStats;const steamIDUser:TSteamUInt64SteamID):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUserStats_GetUserStatInt32:function(const aSelf:PISteamUserStats;const steamIDUser:TSteamUInt64SteamID;const pchName:PSteamChar;const pData:PSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_GetUserStatFloat:function(const aSelf:PISteamUserStats;const steamIDUser:TSteamUInt64SteamID;const pchName:PSteamChar;const pData:PSteamFloat):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_GetUserAchievement:function(const aSelf:PISteamUserStats;const steamIDUser:TSteamUInt64SteamID;const pchName:PSteamChar;const pbAchieved:PSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_GetUserAchievementAndUnlockTime:function(const aSelf:PISteamUserStats;const steamIDUser:TSteamUInt64SteamID;const pchName:PSteamChar;const pbAchieved:PSteamBool;const punUnlockTime:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_ResetAllStats:function(const aSelf:PISteamUserStats;const bAchievementsToo:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_FindOrCreateLeaderboard:function(const aSelf:PISteamUserStats;const pchLeaderboardName:PSteamChar;const eLeaderboardSortMethod:TELeaderboardSortMethod;const eLeaderboardDisplayType:TELeaderboardDisplayType):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUserStats_FindLeaderboard:function(const aSelf:PISteamUserStats;const pchLeaderboardName:PSteamChar):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUserStats_GetLeaderboardName:function(const aSelf:PISteamUserStats;const hSteamLeaderboard:TSteamLeaderboard_t):PSteamChar; cdecl;
    SteamAPI_ISteamUserStats_GetLeaderboardEntryCount:function(const aSelf:PISteamUserStats;const hSteamLeaderboard:TSteamLeaderboard_t):TSteamInt32; cdecl;
    SteamAPI_ISteamUserStats_GetLeaderboardSortMethod:function(const aSelf:PISteamUserStats;const hSteamLeaderboard:TSteamLeaderboard_t):TELeaderboardSortMethod; cdecl;
    SteamAPI_ISteamUserStats_GetLeaderboardDisplayType:function(const aSelf:PISteamUserStats;const hSteamLeaderboard:TSteamLeaderboard_t):TELeaderboardDisplayType; cdecl;
    SteamAPI_ISteamUserStats_DownloadLeaderboardEntries:function(const aSelf:PISteamUserStats;const hSteamLeaderboard:TSteamLeaderboard_t;const eLeaderboardDataRequest:TELeaderboardDataRequest;const nRangeStart:TSteamInt32;const nRangeEnd:TSteamInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUserStats_DownloadLeaderboardEntriesForUsers:function(const aSelf:PISteamUserStats;const hSteamLeaderboard:TSteamLeaderboard_t;const prgUsers:PCSteamID;const cUsers:TSteamInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUserStats_GetDownloadedLeaderboardEntry:function(const aSelf:PISteamUserStats;const hSteamLeaderboardEntries:TSteamLeaderboardEntries_t;const aIndex:TSteamInt32;const pLeaderboardEntry:PLeaderboardEntry_t;const pDetails:PSteamInt32;const cDetailsMax:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_UploadLeaderboardScore:function(const aSelf:PISteamUserStats;const hSteamLeaderboard:TSteamLeaderboard_t;const eLeaderboardUploadScoreMethod:TELeaderboardUploadScoreMethod;const nScore:TSteamInt32;const pScoreDetails:PSteamInt32;const cScoreDetailsCount:TSteamInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUserStats_AttachLeaderboardUGC:function(const aSelf:PISteamUserStats;const hSteamLeaderboard:TSteamLeaderboard_t;const hUGC:TUGCHandle_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUserStats_GetNumberOfCurrentPlayers:function(const aSelf:PISteamUserStats):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUserStats_RequestGlobalAchievementPercentages:function(const aSelf:PISteamUserStats):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUserStats_GetMostAchievedAchievementInfo:function(const aSelf:PISteamUserStats;const pchName:PSteamChar;const unNameBufLen:TSteamUInt32;const pflPercent:PSteamFloat;const pbAchieved:PSteamBool):TSteamInt32; cdecl;
    SteamAPI_ISteamUserStats_GetNextMostAchievedAchievementInfo:function(const aSelf:PISteamUserStats;const iIteratorPrevious:TSteamInt32;const pchName:PSteamChar;const unNameBufLen:TSteamUInt32;const pflPercent:PSteamFloat;const pbAchieved:PSteamBool):TSteamInt32; cdecl;
    SteamAPI_ISteamUserStats_GetAchievementAchievedPercent:function(const aSelf:PISteamUserStats;const pchName:PSteamChar;const pflPercent:PSteamFloat):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_RequestGlobalStats:function(const aSelf:PISteamUserStats;const nHistoryDays:TSteamInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUserStats_GetGlobalStatInt64:function(const aSelf:PISteamUserStats;const pchStatName:PSteamChar;const pData:PSteamInt64):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_GetGlobalStatDouble:function(const aSelf:PISteamUserStats;const pchStatName:PSteamChar;const pData:PSteamDouble):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_GetGlobalStatHistoryInt64:function(const aSelf:PISteamUserStats;const pchStatName:PSteamChar;const pData:PSteamInt64;const cubData:TSteamUInt32):TSteamInt32; cdecl;
    SteamAPI_ISteamUserStats_GetGlobalStatHistoryDouble:function(const aSelf:PISteamUserStats;const pchStatName:PSteamChar;const pData:PSteamDouble;const cubData:TSteamUInt32):TSteamInt32; cdecl;
    SteamAPI_ISteamUserStats_GetAchievementProgressLimitsInt32:function(const aSelf:PISteamUserStats;const pchName:PSteamChar;const pnMinProgress:PSteamInt32;const pnMaxProgress:PSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUserStats_GetAchievementProgressLimitsFloat:function(const aSelf:PISteamUserStats;const pchName:PSteamChar;const pfMinProgress:PSteamFloat;const pfMaxProgress:PSteamFloat):TSteamBool; cdecl;

// ISteamApps
    SteamAPI_SteamApps_v009:function:PISteamApps; cdecl;
    SteamAPI_ISteamApps_BIsSubscribed:function(const aSelf:PISteamApps):TSteamBool; cdecl;
    SteamAPI_ISteamApps_BIsLowViolence:function(const aSelf:PISteamApps):TSteamBool; cdecl;
    SteamAPI_ISteamApps_BIsCybercafe:function(const aSelf:PISteamApps):TSteamBool; cdecl;
    SteamAPI_ISteamApps_BIsVACBanned:function(const aSelf:PISteamApps):TSteamBool; cdecl;
    SteamAPI_ISteamApps_GetCurrentGameLanguage:function(const aSelf:PISteamApps):PSteamChar; cdecl;
    SteamAPI_ISteamApps_GetAvailableGameLanguages:function(const aSelf:PISteamApps):PSteamChar; cdecl;
    SteamAPI_ISteamApps_BIsSubscribedApp:function(const aSelf:PISteamApps;const appID:TAppId_t):TSteamBool; cdecl;
    SteamAPI_ISteamApps_BIsDlcInstalled:function(const aSelf:PISteamApps;const appID:TAppId_t):TSteamBool; cdecl;
    SteamAPI_ISteamApps_GetEarliestPurchaseUnixTime:function(const aSelf:PISteamApps;const nAppID:TAppId_t):TSteamUInt32; cdecl;
    SteamAPI_ISteamApps_BIsSubscribedFromFreeWeekend:function(const aSelf:PISteamApps):TSteamBool; cdecl;
    SteamAPI_ISteamApps_GetDLCCount:function(const aSelf:PISteamApps):TSteamInt32; cdecl;
    SteamAPI_ISteamApps_BGetDLCDataByIndex:function(const aSelf:PISteamApps;const iDLC:TSteamInt32;const pAppID:PAppId_t;const pbAvailable:PSteamBool;const pchName:PSteamChar;const cchNameBufferSize:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamApps_InstallDLC:procedure(const aSelf:PISteamApps;const nAppID:TAppId_t); cdecl;
    SteamAPI_ISteamApps_UninstallDLC:procedure(const aSelf:PISteamApps;const nAppID:TAppId_t); cdecl;
    SteamAPI_ISteamApps_RequestAppProofOfPurchaseKey:procedure(const aSelf:PISteamApps;const nAppID:TAppId_t); cdecl;
    SteamAPI_ISteamApps_GetCurrentBetaName:function(const aSelf:PISteamApps;const pchName:PSteamChar;const cchNameBufferSize:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamApps_MarkContentCorrupt:function(const aSelf:PISteamApps;const bMissingFilesOnly:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamApps_GetInstalledDepots:function(const aSelf:PISteamApps;const appID:TAppId_t;const pvecDepots:PDepotId_t;const cMaxDepots:TSteamUInt32):TSteamUInt32; cdecl;
    SteamAPI_ISteamApps_GetAppInstallDir:function(const aSelf:PISteamApps;const appID:TAppId_t;const pchFolder:PSteamChar;const cchFolderBufferSize:TSteamUInt32):TSteamUInt32; cdecl;
    SteamAPI_ISteamApps_BIsAppInstalled:function(const aSelf:PISteamApps;const appID:TAppId_t):TSteamBool; cdecl;
    SteamAPI_ISteamApps_GetAppOwner:function(const aSelf:PISteamApps):TSteamUInt64SteamID; cdecl;
    SteamAPI_ISteamApps_GetLaunchQueryParam:function(const aSelf:PISteamApps;const pchKey:PSteamChar):PSteamChar; cdecl;
    SteamAPI_ISteamApps_GetDlcDownloadProgress:function(const aSelf:PISteamApps;const nAppID:TAppId_t;const punBytesDownloaded:PSteamUInt64;const punBytesTotal:PSteamUInt64):TSteamBool; cdecl;
    SteamAPI_ISteamApps_GetAppBuildId:function(const aSelf:PISteamApps):TSteamInt32; cdecl;
    SteamAPI_ISteamApps_RequestAllProofOfPurchaseKeys:procedure(const aSelf:PISteamApps); cdecl;
    SteamAPI_ISteamApps_GetFileDetails:function(const aSelf:PISteamApps;const pszFileName:PSteamChar):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamApps_GetLaunchCommandLine:function(const aSelf:PISteamApps;const pszCommandLine:PSteamChar;const cubCommandLine:TSteamInt32):TSteamInt32; cdecl;
    SteamAPI_ISteamApps_BIsSubscribedFromFamilySharing:function(const aSelf:PISteamApps):TSteamBool; cdecl;
    SteamAPI_ISteamApps_BIsTimedTrial:function(const aSelf:PISteamApps;const punSecondsAllowed:PSteamUInt32;const punSecondsPlayed:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamApps_SetDlcContext:function(const aSelf:PISteamApps;const nAppID:TAppId_t):TSteamBool; cdecl;
    SteamAPI_ISteamApps_GetNumBetas:function(const aSelf:PISteamApps;const pnAvailable:PSteamInt32;const pnPrivate:PSteamInt32):TSteamInt32; cdecl;
    SteamAPI_ISteamApps_GetBetaInfo:function(const aSelf:PISteamApps;const iBetaIndex:TSteamInt32;const punFlags:PSteamUInt32;const punBuildID:PSteamUInt32;const pchBetaName:PSteamChar;const cchBetaName:TSteamInt32;const pchDescription:PSteamChar;const cchDescription:TSteamInt32;const punLastUpdated:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamApps_SetActiveBeta:function(const aSelf:PISteamApps;const pchBetaName:PSteamChar):TSteamBool; cdecl;

// ISteamNetworking
    SteamAPI_SteamNetworking_v006:function:PISteamNetworking; cdecl;
    SteamAPI_SteamGameServerNetworking_v006:function:PISteamNetworking; cdecl;
    SteamAPI_ISteamNetworking_SendP2PPacket:function(const aSelf:PISteamNetworking;const steamIDRemote:TSteamUInt64SteamID;const pubData:TSteamPointer;const cubData:TSteamUInt32;const eP2PSendType:TEP2PSend;const nChannel:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_IsP2PPacketAvailable:function(const aSelf:PISteamNetworking;const pcubMsgSize:PSteamUInt32;const nChannel:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_ReadP2PPacket:function(const aSelf:PISteamNetworking;const pubDest:TSteamPointer;const cubDest:TSteamUInt32;const pcubMsgSize:PSteamUInt32;const psteamIDRemote:PCSteamID;const nChannel:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_AcceptP2PSessionWithUser:function(const aSelf:PISteamNetworking;const steamIDRemote:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_CloseP2PSessionWithUser:function(const aSelf:PISteamNetworking;const steamIDRemote:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_CloseP2PChannelWithUser:function(const aSelf:PISteamNetworking;const steamIDRemote:TSteamUInt64SteamID;const nChannel:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_GetP2PSessionState:function(const aSelf:PISteamNetworking;const steamIDRemote:TSteamUInt64SteamID;const pConnectionState:PP2PSessionState_t):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_AllowP2PPacketRelay:function(const aSelf:PISteamNetworking;const bAllow:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_CreateListenSocket:function(const aSelf:PISteamNetworking;const nVirtualP2PPort:TSteamInt32;const nIP:TSteamIPAddress_t;const nPort:TSteamUInt16;const bAllowUseOfPacketRelay:TSteamBool):TSNetListenSocket_t; cdecl;
    SteamAPI_ISteamNetworking_CreateP2PConnectionSocket:function(const aSelf:PISteamNetworking;const steamIDTarget:TSteamUInt64SteamID;const nVirtualPort:TSteamInt32;const nTimeoutSec:TSteamInt32;const bAllowUseOfPacketRelay:TSteamBool):TSNetSocket_t; cdecl;
    SteamAPI_ISteamNetworking_CreateConnectionSocket:function(const aSelf:PISteamNetworking;const nIP:TSteamIPAddress_t;const nPort:TSteamUInt16;const nTimeoutSec:TSteamInt32):TSNetSocket_t; cdecl;
    SteamAPI_ISteamNetworking_DestroySocket:function(const aSelf:PISteamNetworking;const hSocket:TSNetSocket_t;const bNotifyRemoteEnd:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_DestroyListenSocket:function(const aSelf:PISteamNetworking;const hSocket:TSNetListenSocket_t;const bNotifyRemoteEnd:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_SendDataOnSocket:function(const aSelf:PISteamNetworking;const hSocket:TSNetSocket_t;const pubData:TSteamPointer;const cubData:TSteamUInt32;const bReliable:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_IsDataAvailableOnSocket:function(const aSelf:PISteamNetworking;const hSocket:TSNetSocket_t;const pcubMsgSize:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_RetrieveDataFromSocket:function(const aSelf:PISteamNetworking;const hSocket:TSNetSocket_t;const pubDest:TSteamPointer;const cubDest:TSteamUInt32;const pcubMsgSize:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_IsDataAvailable:function(const aSelf:PISteamNetworking;const hListenSocket:TSNetListenSocket_t;const pcubMsgSize:PSteamUInt32;const phSocket:PSNetSocket_t):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_RetrieveData:function(const aSelf:PISteamNetworking;const hListenSocket:TSNetListenSocket_t;const pubDest:TSteamPointer;const cubDest:TSteamUInt32;const pcubMsgSize:PSteamUInt32;const phSocket:PSNetSocket_t):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_GetSocketInfo:function(const aSelf:PISteamNetworking;const hSocket:TSNetSocket_t;const pSteamIDRemote:PCSteamID;const peSocketStatus:PSteamInt32;const punIPRemote:PSteamIPAddress_t;const punPortRemote:PSteamUInt16):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_GetListenSocketInfo:function(const aSelf:PISteamNetworking;const hListenSocket:TSNetListenSocket_t;const pnIP:PSteamIPAddress_t;const pnPort:PSteamUInt16):TSteamBool; cdecl;
    SteamAPI_ISteamNetworking_GetSocketConnectionType:function(const aSelf:PISteamNetworking;const hSocket:TSNetSocket_t):TESNetSocketConnectionType; cdecl;
    SteamAPI_ISteamNetworking_GetMaxPacketSize:function(const aSelf:PISteamNetworking;const hSocket:TSNetSocket_t):TSteamInt32; cdecl;

// ISteamScreenshots
    SteamAPI_SteamScreenshots_v003:function:PISteamScreenshots; cdecl;
    SteamAPI_ISteamScreenshots_WriteScreenshot:function(const aSelf:PISteamScreenshots;const pubRGB:TSteamPointer;const cubRGB:TSteamUInt32;const nWidth:TSteamInt32;const nHeight:TSteamInt32):TScreenshotHandle; cdecl;
    SteamAPI_ISteamScreenshots_AddScreenshotToLibrary:function(const aSelf:PISteamScreenshots;const pchFilename:PSteamChar;const pchThumbnailFilename:PSteamChar;const nWidth:TSteamInt32;const nHeight:TSteamInt32):TScreenshotHandle; cdecl;
    SteamAPI_ISteamScreenshots_TriggerScreenshot:procedure(const aSelf:PISteamScreenshots); cdecl;
    SteamAPI_ISteamScreenshots_HookScreenshots:procedure(const aSelf:PISteamScreenshots;const bHook:TSteamBool); cdecl;
    SteamAPI_ISteamScreenshots_SetLocation:function(const aSelf:PISteamScreenshots;const hScreenshot:TScreenshotHandle;const pchLocation:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamScreenshots_TagUser:function(const aSelf:PISteamScreenshots;const hScreenshot:TScreenshotHandle;const steamID:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamScreenshots_TagPublishedFile:function(const aSelf:PISteamScreenshots;const hScreenshot:TScreenshotHandle;const unPublishedFileID:TPublishedFileId_t):TSteamBool; cdecl;
    SteamAPI_ISteamScreenshots_IsScreenshotsHooked:function(const aSelf:PISteamScreenshots):TSteamBool; cdecl;
    SteamAPI_ISteamScreenshots_AddVRScreenshotToLibrary:function(const aSelf:PISteamScreenshots;const eType:TEVRScreenshotType;const pchFilename:PSteamChar;const pchVRFilename:PSteamChar):TScreenshotHandle; cdecl;

// ISteamMusic
    SteamAPI_SteamMusic_v001:function:PISteamMusic; cdecl;
    SteamAPI_ISteamMusic_BIsEnabled:function(const aSelf:PISteamMusic):TSteamBool; cdecl;
    SteamAPI_ISteamMusic_BIsPlaying:function(const aSelf:PISteamMusic):TSteamBool; cdecl;
    SteamAPI_ISteamMusic_GetPlaybackStatus:function(const aSelf:PISteamMusic):TAudioPlayback_Status; cdecl;
    SteamAPI_ISteamMusic_Play:procedure(const aSelf:PISteamMusic); cdecl;
    SteamAPI_ISteamMusic_Pause:procedure(const aSelf:PISteamMusic); cdecl;
    SteamAPI_ISteamMusic_PlayPrevious:procedure(const aSelf:PISteamMusic); cdecl;
    SteamAPI_ISteamMusic_PlayNext:procedure(const aSelf:PISteamMusic); cdecl;
    SteamAPI_ISteamMusic_SetVolume:procedure(const aSelf:PISteamMusic;const flVolume:TSteamFloat); cdecl;
    SteamAPI_ISteamMusic_GetVolume:function(const aSelf:PISteamMusic):TSteamFloat; cdecl;

// ISteamHTTP
    SteamAPI_SteamHTTP_v003:function:PISteamHTTP; cdecl;
    SteamAPI_SteamGameServerHTTP_v003:function:PISteamHTTP; cdecl;
    SteamAPI_ISteamHTTP_CreateHTTPRequest:function(const aSelf:PISteamHTTP;const eHTTPRequestMethod:TEHTTPMethod;const pchAbsoluteURL:PSteamChar):THTTPRequestHandle; cdecl;
    SteamAPI_ISteamHTTP_SetHTTPRequestContextValue:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const ulContextValue:TSteamUInt64):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_SetHTTPRequestNetworkActivityTimeout:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const unTimeoutSeconds:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_SetHTTPRequestHeaderValue:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const pchHeaderName:PSteamChar;const pchHeaderValue:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_SetHTTPRequestGetOrPostParameter:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const pchParamName:PSteamChar;const pchParamValue:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_SendHTTPRequest:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const pCallHandle:PSteamAPICall_t):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_SendHTTPRequestAndStreamResponse:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const pCallHandle:PSteamAPICall_t):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_DeferHTTPRequest:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_PrioritizeHTTPRequest:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_GetHTTPResponseHeaderSize:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const pchHeaderName:PSteamChar;const unResponseHeaderSize:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_GetHTTPResponseHeaderValue:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const pchHeaderName:PSteamChar;const pHeaderValueBuffer:PSteamUInt8;const unBufferSize:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_GetHTTPResponseBodySize:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const unBodySize:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_GetHTTPResponseBodyData:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const pBodyDataBuffer:PSteamUInt8;const unBufferSize:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_GetHTTPStreamingResponseBodyData:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const cOffset:TSteamUInt32;const pBodyDataBuffer:PSteamUInt8;const unBufferSize:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_ReleaseHTTPRequest:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_GetHTTPDownloadProgressPct:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const pflPercentOut:PSteamFloat):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_SetHTTPRequestRawPostBody:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const pchContentType:PSteamChar;const pubBody:PSteamUInt8;const unBodyLen:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_CreateCookieContainer:function(const aSelf:PISteamHTTP;const bAllowResponsesToModify:TSteamBool):THTTPCookieContainerHandle; cdecl;
    SteamAPI_ISteamHTTP_ReleaseCookieContainer:function(const aSelf:PISteamHTTP;const hCookieContainer:THTTPCookieContainerHandle):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_SetCookie:function(const aSelf:PISteamHTTP;const hCookieContainer:THTTPCookieContainerHandle;const pchHost:PSteamChar;const pchUrl:PSteamChar;const pchCookie:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_SetHTTPRequestCookieContainer:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const hCookieContainer:THTTPCookieContainerHandle):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_SetHTTPRequestUserAgentInfo:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const pchUserAgentInfo:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_SetHTTPRequestRequiresVerifiedCertificate:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const bRequireVerifiedCertificate:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_SetHTTPRequestAbsoluteTimeoutMS:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const unMilliseconds:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamHTTP_GetHTTPRequestWasTimedOut:function(const aSelf:PISteamHTTP;const hRequest:THTTPRequestHandle;const pbWasTimedOut:PSteamBool):TSteamBool; cdecl;

// ISteamInput
    SteamAPI_SteamInput_v006:function:PISteamInput; cdecl;
    SteamAPI_ISteamInput_Init:function(const aSelf:PISteamInput;const bExplicitlyCallRunFrame:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamInput_Shutdown:function(const aSelf:PISteamInput):TSteamBool; cdecl;
    SteamAPI_ISteamInput_SetInputActionManifestFilePath:function(const aSelf:PISteamInput;const pchInputActionManifestAbsolutePath:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamInput_RunFrame:procedure(const aSelf:PISteamInput;const bReservedValue:TSteamBool); cdecl;
    SteamAPI_ISteamInput_BWaitForData:function(const aSelf:PISteamInput;const bWaitForever:TSteamBool;const unTimeout:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamInput_BNewDataAvailable:function(const aSelf:PISteamInput):TSteamBool; cdecl;
    SteamAPI_ISteamInput_GetConnectedControllers:function(const aSelf:PISteamInput;const handlesOut:PInputHandle_t):TSteamInt32; cdecl;
    SteamAPI_ISteamInput_EnableDeviceCallbacks:procedure(const aSelf:PISteamInput); cdecl;
    SteamAPI_ISteamInput_EnableActionEventCallbacks:procedure(const aSelf:PISteamInput;const pCallback:TSteamInputActionEventCallbackPointer); cdecl;
    SteamAPI_ISteamInput_GetActionSetHandle:function(const aSelf:PISteamInput;const pszActionSetName:PSteamChar):TInputActionSetHandle_t; cdecl;
    SteamAPI_ISteamInput_ActivateActionSet:procedure(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const actionSetHandle:TInputActionSetHandle_t); cdecl;
    SteamAPI_ISteamInput_GetCurrentActionSet:function(const aSelf:PISteamInput;const inputHandle:TInputHandle_t):TInputActionSetHandle_t; cdecl;
    SteamAPI_ISteamInput_ActivateActionSetLayer:procedure(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const actionSetLayerHandle:TInputActionSetHandle_t); cdecl;
    SteamAPI_ISteamInput_DeactivateActionSetLayer:procedure(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const actionSetLayerHandle:TInputActionSetHandle_t); cdecl;
    SteamAPI_ISteamInput_DeactivateAllActionSetLayers:procedure(const aSelf:PISteamInput;const inputHandle:TInputHandle_t); cdecl;
    SteamAPI_ISteamInput_GetActiveActionSetLayers:function(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const handlesOut:PInputActionSetHandle_t):TSteamInt32; cdecl;
    SteamAPI_ISteamInput_GetDigitalActionHandle:function(const aSelf:PISteamInput;const pszActionName:PSteamChar):TInputDigitalActionHandle_t; cdecl;
    SteamAPI_ISteamInput_GetDigitalActionData:function(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const digitalActionHandle:TInputDigitalActionHandle_t):TInputDigitalActionData_t; cdecl;
    SteamAPI_ISteamInput_GetDigitalActionOrigins:function(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const actionSetHandle:TInputActionSetHandle_t;const digitalActionHandle:TInputDigitalActionHandle_t;const originsOut:PEInputActionOrigin):TSteamInt32; cdecl;
    SteamAPI_ISteamInput_GetStringForDigitalActionName:function(const aSelf:PISteamInput;const eActionHandle:TInputDigitalActionHandle_t):PSteamChar; cdecl;
    SteamAPI_ISteamInput_GetAnalogActionHandle:function(const aSelf:PISteamInput;const pszActionName:PSteamChar):TInputAnalogActionHandle_t; cdecl;
    SteamAPI_ISteamInput_GetAnalogActionData:function(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const analogActionHandle:TInputAnalogActionHandle_t):TInputAnalogActionData_t; cdecl;
    SteamAPI_ISteamInput_GetAnalogActionOrigins:function(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const actionSetHandle:TInputActionSetHandle_t;const analogActionHandle:TInputAnalogActionHandle_t;const originsOut:PEInputActionOrigin):TSteamInt32; cdecl;
    SteamAPI_ISteamInput_GetGlyphPNGForActionOrigin:function(const aSelf:PISteamInput;const eOrigin:TEInputActionOrigin;const eSize:TESteamInputGlyphSize;const unFlags:TSteamUInt32):PSteamChar; cdecl;
    SteamAPI_ISteamInput_GetGlyphSVGForActionOrigin:function(const aSelf:PISteamInput;const eOrigin:TEInputActionOrigin;const unFlags:TSteamUInt32):PSteamChar; cdecl;
    SteamAPI_ISteamInput_GetGlyphForActionOrigin_Legacy:function(const aSelf:PISteamInput;const eOrigin:TEInputActionOrigin):PSteamChar; cdecl;
    SteamAPI_ISteamInput_GetStringForActionOrigin:function(const aSelf:PISteamInput;const eOrigin:TEInputActionOrigin):PSteamChar; cdecl;
    SteamAPI_ISteamInput_GetStringForAnalogActionName:function(const aSelf:PISteamInput;const eActionHandle:TInputAnalogActionHandle_t):PSteamChar; cdecl;
    SteamAPI_ISteamInput_StopAnalogActionMomentum:procedure(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const eAction:TInputAnalogActionHandle_t); cdecl;
    SteamAPI_ISteamInput_GetMotionData:function(const aSelf:PISteamInput;const inputHandle:TInputHandle_t):TInputMotionData_t; cdecl;
    SteamAPI_ISteamInput_TriggerVibration:procedure(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const usLeftSpeed:TSteamUInt16;const usRightSpeed:TSteamUInt16); cdecl;
    SteamAPI_ISteamInput_TriggerVibrationExtended:procedure(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const usLeftSpeed:TSteamUInt16;const usRightSpeed:TSteamUInt16;const usLeftTriggerSpeed:TSteamUInt16;const usRightTriggerSpeed:TSteamUInt16); cdecl;
    SteamAPI_ISteamInput_TriggerSimpleHapticEvent:procedure(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const eHapticLocation:TEControllerHapticLocation;const nIntensity:TSteamUInt8;const nGainDB:TSteamChar;const nOtherIntensity:TSteamUInt8;const nOtherGainDB:TSteamChar); cdecl;
    SteamAPI_ISteamInput_SetLEDColor:procedure(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const nColorR:TSteamUInt8;const nColorG:TSteamUInt8;const nColorB:TSteamUInt8;const nFlags:TSteamUInt32); cdecl;
    SteamAPI_ISteamInput_Legacy_TriggerHapticPulse:procedure(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const eTargetPad:TESteamControllerPad;const usDurationMicroSec:TSteamUInt16); cdecl;
    SteamAPI_ISteamInput_Legacy_TriggerRepeatedHapticPulse:procedure(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const eTargetPad:TESteamControllerPad;const usDurationMicroSec:TSteamUInt16;const usOffMicroSec:TSteamUInt16;const unRepeat:TSteamUInt16;const nFlags:TSteamUInt32); cdecl;
    SteamAPI_ISteamInput_ShowBindingPanel:function(const aSelf:PISteamInput;const inputHandle:TInputHandle_t):TSteamBool; cdecl;
    SteamAPI_ISteamInput_GetInputTypeForHandle:function(const aSelf:PISteamInput;const inputHandle:TInputHandle_t):TESteamInputType; cdecl;
    SteamAPI_ISteamInput_GetControllerForGamepadIndex:function(const aSelf:PISteamInput;const nIndex:TSteamInt32):TInputHandle_t; cdecl;
    SteamAPI_ISteamInput_GetGamepadIndexForController:function(const aSelf:PISteamInput;const ulinputHandle:TInputHandle_t):TSteamInt32; cdecl;
    SteamAPI_ISteamInput_GetStringForXboxOrigin:function(const aSelf:PISteamInput;const eOrigin:TEXboxOrigin):PSteamChar; cdecl;
    SteamAPI_ISteamInput_GetGlyphForXboxOrigin:function(const aSelf:PISteamInput;const eOrigin:TEXboxOrigin):PSteamChar; cdecl;
    SteamAPI_ISteamInput_GetActionOriginFromXboxOrigin:function(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const eOrigin:TEXboxOrigin):TEInputActionOrigin; cdecl;
    SteamAPI_ISteamInput_TranslateActionOrigin:function(const aSelf:PISteamInput;const eDestinationInputType:TESteamInputType;const eSourceOrigin:TEInputActionOrigin):TEInputActionOrigin; cdecl;
    SteamAPI_ISteamInput_GetDeviceBindingRevision:function(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const pMajor:PSteamInt32;const pMinor:PSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamInput_GetRemotePlaySessionID:function(const aSelf:PISteamInput;const inputHandle:TInputHandle_t):TSteamUInt32; cdecl;
    SteamAPI_ISteamInput_GetSessionInputConfigurationSettings:function(const aSelf:PISteamInput):TSteamUInt16; cdecl;
    SteamAPI_ISteamInput_SetDualSenseTriggerEffect:procedure(const aSelf:PISteamInput;const inputHandle:TInputHandle_t;const pParam:PScePadTriggerEffectParam); cdecl;

// ISteamController
    SteamAPI_SteamController_v008:function:PISteamController; cdecl;
    SteamAPI_ISteamController_Init:function(const aSelf:PISteamController):TSteamBool; cdecl;
    SteamAPI_ISteamController_Shutdown:function(const aSelf:PISteamController):TSteamBool; cdecl;
    SteamAPI_ISteamController_RunFrame:procedure(const aSelf:PISteamController); cdecl;
    SteamAPI_ISteamController_GetConnectedControllers:function(const aSelf:PISteamController;const handlesOut:PControllerHandle_t):TSteamInt32; cdecl;
    SteamAPI_ISteamController_GetActionSetHandle:function(const aSelf:PISteamController;const pszActionSetName:PSteamChar):TControllerActionSetHandle_t; cdecl;
    SteamAPI_ISteamController_ActivateActionSet:procedure(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t;const actionSetHandle:TControllerActionSetHandle_t); cdecl;
    SteamAPI_ISteamController_GetCurrentActionSet:function(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t):TControllerActionSetHandle_t; cdecl;
    SteamAPI_ISteamController_ActivateActionSetLayer:procedure(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t;const actionSetLayerHandle:TControllerActionSetHandle_t); cdecl;
    SteamAPI_ISteamController_DeactivateActionSetLayer:procedure(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t;const actionSetLayerHandle:TControllerActionSetHandle_t); cdecl;
    SteamAPI_ISteamController_DeactivateAllActionSetLayers:procedure(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t); cdecl;
    SteamAPI_ISteamController_GetActiveActionSetLayers:function(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t;const handlesOut:PControllerActionSetHandle_t):TSteamInt32; cdecl;
    SteamAPI_ISteamController_GetDigitalActionHandle:function(const aSelf:PISteamController;const pszActionName:PSteamChar):TControllerDigitalActionHandle_t; cdecl;
    SteamAPI_ISteamController_GetDigitalActionData:function(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t;const digitalActionHandle:TControllerDigitalActionHandle_t):TInputDigitalActionData_t; cdecl;
    SteamAPI_ISteamController_GetDigitalActionOrigins:function(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t;const actionSetHandle:TControllerActionSetHandle_t;const digitalActionHandle:TControllerDigitalActionHandle_t;const originsOut:PEControllerActionOrigin):TSteamInt32; cdecl;
    SteamAPI_ISteamController_GetAnalogActionHandle:function(const aSelf:PISteamController;const pszActionName:PSteamChar):TControllerAnalogActionHandle_t; cdecl;
    SteamAPI_ISteamController_GetAnalogActionData:function(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t;const analogActionHandle:TControllerAnalogActionHandle_t):TInputAnalogActionData_t; cdecl;
    SteamAPI_ISteamController_GetAnalogActionOrigins:function(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t;const actionSetHandle:TControllerActionSetHandle_t;const analogActionHandle:TControllerAnalogActionHandle_t;const originsOut:PEControllerActionOrigin):TSteamInt32; cdecl;
    SteamAPI_ISteamController_GetGlyphForActionOrigin:function(const aSelf:PISteamController;const eOrigin:TEControllerActionOrigin):PSteamChar; cdecl;
    SteamAPI_ISteamController_GetStringForActionOrigin:function(const aSelf:PISteamController;const eOrigin:TEControllerActionOrigin):PSteamChar; cdecl;
    SteamAPI_ISteamController_StopAnalogActionMomentum:procedure(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t;const eAction:TControllerAnalogActionHandle_t); cdecl;
    SteamAPI_ISteamController_GetMotionData:function(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t):TInputMotionData_t; cdecl;
    SteamAPI_ISteamController_TriggerHapticPulse:procedure(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t;const eTargetPad:TESteamControllerPad;const usDurationMicroSec:TSteamUInt16); cdecl;
    SteamAPI_ISteamController_TriggerRepeatedHapticPulse:procedure(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t;const eTargetPad:TESteamControllerPad;const usDurationMicroSec:TSteamUInt16;const usOffMicroSec:TSteamUInt16;const unRepeat:TSteamUInt16;const nFlags:TSteamUInt32); cdecl;
    SteamAPI_ISteamController_TriggerVibration:procedure(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t;const usLeftSpeed:TSteamUInt16;const usRightSpeed:TSteamUInt16); cdecl;
    SteamAPI_ISteamController_SetLEDColor:procedure(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t;const nColorR:TSteamUInt8;const nColorG:TSteamUInt8;const nColorB:TSteamUInt8;const nFlags:TSteamUInt32); cdecl;
    SteamAPI_ISteamController_ShowBindingPanel:function(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t):TSteamBool; cdecl;
    SteamAPI_ISteamController_GetInputTypeForHandle:function(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t):TESteamInputType; cdecl;
    SteamAPI_ISteamController_GetControllerForGamepadIndex:function(const aSelf:PISteamController;const nIndex:TSteamInt32):TControllerHandle_t; cdecl;
    SteamAPI_ISteamController_GetGamepadIndexForController:function(const aSelf:PISteamController;const ulControllerHandle:TControllerHandle_t):TSteamInt32; cdecl;
    SteamAPI_ISteamController_GetStringForXboxOrigin:function(const aSelf:PISteamController;const eOrigin:TEXboxOrigin):PSteamChar; cdecl;
    SteamAPI_ISteamController_GetGlyphForXboxOrigin:function(const aSelf:PISteamController;const eOrigin:TEXboxOrigin):PSteamChar; cdecl;
    SteamAPI_ISteamController_GetActionOriginFromXboxOrigin:function(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t;const eOrigin:TEXboxOrigin):TEControllerActionOrigin; cdecl;
    SteamAPI_ISteamController_TranslateActionOrigin:function(const aSelf:PISteamController;const eDestinationInputType:TESteamInputType;const eSourceOrigin:TEControllerActionOrigin):TEControllerActionOrigin; cdecl;
    SteamAPI_ISteamController_GetControllerBindingRevision:function(const aSelf:PISteamController;const controllerHandle:TControllerHandle_t;const pMajor:PSteamInt32;const pMinor:PSteamInt32):TSteamBool; cdecl;

// ISteamUGC
    SteamAPI_SteamUGC_v021:function:PISteamUGC; cdecl;
    SteamAPI_SteamGameServerUGC_v021:function:PISteamUGC; cdecl;
    SteamAPI_ISteamUGC_CreateQueryUserUGCRequest:function(const aSelf:PISteamUGC;const unAccountID:TAccountID_t;const eListType:TEUserUGCList;const eMatchingUGCType:TEUGCMatchingUGCType;const eSortOrder:TEUserUGCListSortOrder;const nCreatorAppID:TAppId_t;const nConsumerAppID:TAppId_t;const unPage:TSteamUInt32):TUGCQueryHandle_t; cdecl;
    SteamAPI_ISteamUGC_CreateQueryAllUGCRequestPage:function(const aSelf:PISteamUGC;const eQueryType:TEUGCQuery;const eMatchingeMatchingUGCTypeFileType:TEUGCMatchingUGCType;const nCreatorAppID:TAppId_t;const nConsumerAppID:TAppId_t;const unPage:TSteamUInt32):TUGCQueryHandle_t; cdecl;
    SteamAPI_ISteamUGC_CreateQueryAllUGCRequestCursor:function(const aSelf:PISteamUGC;const eQueryType:TEUGCQuery;const eMatchingeMatchingUGCTypeFileType:TEUGCMatchingUGCType;const nCreatorAppID:TAppId_t;const nConsumerAppID:TAppId_t;const pchCursor:PSteamChar):TUGCQueryHandle_t; cdecl;
    SteamAPI_ISteamUGC_CreateQueryUGCDetailsRequest:function(const aSelf:PISteamUGC;const pvecPublishedFileID:PPublishedFileId_t;const unNumPublishedFileIDs:TSteamUInt32):TUGCQueryHandle_t; cdecl;
    SteamAPI_ISteamUGC_SendQueryUGCRequest:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_GetQueryUGCResult:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32;const pDetails:PSteamUGCDetails_t):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_GetQueryUGCNumTags:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32):TSteamUInt32; cdecl;
    SteamAPI_ISteamUGC_GetQueryUGCTag:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32;const indexTag:TSteamUInt32;const pchValue:PSteamChar;const cchValueSize:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_GetQueryUGCTagDisplayName:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32;const indexTag:TSteamUInt32;const pchValue:PSteamChar;const cchValueSize:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_GetQueryUGCPreviewURL:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32;const pchURL:PSteamChar;const cchURLSize:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_GetQueryUGCMetadata:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32;const pchMetadata:PSteamChar;const cchMetadatasize:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_GetQueryUGCChildren:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32;const pvecPublishedFileID:PPublishedFileId_t;const cMaxEntries:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_GetQueryUGCStatistic:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32;const eStatType:TEItemStatistic;const pStatValue:PSteamUInt64):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_GetQueryUGCNumAdditionalPreviews:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32):TSteamUInt32; cdecl;
    SteamAPI_ISteamUGC_GetQueryUGCAdditionalPreview:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32;const previewIndex:TSteamUInt32;const pchURLOrVideoID:PSteamChar;const cchURLSize:TSteamUInt32;const pchOriginalFileName:PSteamChar;const cchOriginalFileNameSize:TSteamUInt32;const pPreviewType:PEItemPreviewType):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_GetQueryUGCNumKeyValueTags:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32):TSteamUInt32; cdecl;
    SteamAPI_ISteamUGC_GetQueryUGCKeyValueTag:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32;const keyValueTagIndex:TSteamUInt32;const pchKey:PSteamChar;const cchKeySize:TSteamUInt32;const pchValue:PSteamChar;const cchValueSize:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_GetQueryFirstUGCKeyValueTag:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32;const pchKey:PSteamChar;const pchValue:PSteamChar;const cchValueSize:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_GetNumSupportedGameVersions:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32):TSteamUInt32; cdecl;
    SteamAPI_ISteamUGC_GetSupportedGameVersionData:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32;const versionIndex:TSteamUInt32;const pchGameBranchMin:PSteamChar;const pchGameBranchMax:PSteamChar;const cchGameBranchSize:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_GetQueryUGCContentDescriptors:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const aIndex:TSteamUInt32;const pvecDescriptors:PEUGCContentDescriptorID;const cMaxEntries:TSteamUInt32):TSteamUInt32; cdecl;
    SteamAPI_ISteamUGC_ReleaseQueryUGCRequest:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_AddRequiredTag:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const pTagName:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_AddRequiredTagGroup:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const pTagGroups:PSteamParamStringArray_t):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_AddExcludedTag:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const pTagName:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetReturnOnlyIDs:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const bReturnOnlyIDs:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetReturnKeyValueTags:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const bReturnKeyValueTags:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetReturnLongDescription:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const bReturnLongDescription:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetReturnMetadata:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const bReturnMetadata:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetReturnChildren:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const bReturnChildren:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetReturnAdditionalPreviews:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const bReturnAdditionalPreviews:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetReturnTotalOnly:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const bReturnTotalOnly:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetReturnPlaytimeStats:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const unDays:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetLanguage:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const pchLanguage:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetAllowCachedResponse:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const unMaxAgeSeconds:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetAdminQuery:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const bAdminQuery:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetCloudFileNameFilter:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const pMatchCloudFileName:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetMatchAnyTag:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const bMatchAnyTag:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetSearchText:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const pSearchText:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetRankedByTrendDays:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const unDays:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetTimeCreatedDateRange:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const rtStart:TRTime32;const rtEnd:TRTime32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetTimeUpdatedDateRange:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const rtStart:TRTime32;const rtEnd:TRTime32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_AddRequiredKeyValueTag:function(const aSelf:PISteamUGC;const handle:TUGCQueryHandle_t;const pKey:PSteamChar;const pValue:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_RequestUGCDetails:function(const aSelf:PISteamUGC;const nPublishedFileID:TPublishedFileId_t;const unMaxAgeSeconds:TSteamUInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_CreateItem:function(const aSelf:PISteamUGC;const nConsumerAppId:TAppId_t;const eFileType:TEWorkshopFileType):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_StartItemUpdate:function(const aSelf:PISteamUGC;const nConsumerAppId:TAppId_t;const nPublishedFileID:TPublishedFileId_t):TUGCUpdateHandle_t; cdecl;
    SteamAPI_ISteamUGC_SetItemTitle:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const pchTitle:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetItemDescription:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const pchDescription:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetItemUpdateLanguage:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const pchLanguage:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetItemMetadata:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const pchMetaData:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetItemVisibility:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const eVisibility:TERemoteStoragePublishedFileVisibility):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetItemTags:function(const aSelf:PISteamUGC;const updateHandle:TUGCUpdateHandle_t;const pTags:PSteamParamStringArray_t;const bAllowAdminTags:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetItemContent:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const pszContentFolder:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetItemPreview:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const pszPreviewFile:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetAllowLegacyUpload:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const bAllowLegacyUpload:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_RemoveAllItemKeyValueTags:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_RemoveItemKeyValueTags:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const pchKey:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_AddItemKeyValueTag:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const pchKey:PSteamChar;const pchValue:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_AddItemPreviewFile:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const pszPreviewFile:PSteamChar;const aType:TEItemPreviewType):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_AddItemPreviewVideo:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const pszVideoID:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_UpdateItemPreviewFile:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const aIndex:TSteamUInt32;const pszPreviewFile:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_UpdateItemPreviewVideo:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const aIndex:TSteamUInt32;const pszVideoID:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_RemoveItemPreview:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const aIndex:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_AddContentDescriptor:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const descid:TEUGCContentDescriptorID):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_RemoveContentDescriptor:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const descid:TEUGCContentDescriptorID):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetRequiredGameVersions:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const pszGameBranchMin:PSteamChar;const pszGameBranchMax:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SubmitItemUpdate:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const pchChangeNote:PSteamChar):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_GetItemUpdateProgress:function(const aSelf:PISteamUGC;const handle:TUGCUpdateHandle_t;const punBytesProcessed:PSteamUInt64;const punBytesTotal:PSteamUInt64):TEItemUpdateStatus; cdecl;
    SteamAPI_ISteamUGC_SetUserItemVote:function(const aSelf:PISteamUGC;const nPublishedFileID:TPublishedFileId_t;const bVoteUp:TSteamBool):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_GetUserItemVote:function(const aSelf:PISteamUGC;const nPublishedFileID:TPublishedFileId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_AddItemToFavorites:function(const aSelf:PISteamUGC;const nAppId:TAppId_t;const nPublishedFileID:TPublishedFileId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_RemoveItemFromFavorites:function(const aSelf:PISteamUGC;const nAppId:TAppId_t;const nPublishedFileID:TPublishedFileId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_SubscribeItem:function(const aSelf:PISteamUGC;const nPublishedFileID:TPublishedFileId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_UnsubscribeItem:function(const aSelf:PISteamUGC;const nPublishedFileID:TPublishedFileId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_GetNumSubscribedItems:function(const aSelf:PISteamUGC;const bIncludeLocallyDisabled:TSteamBool):TSteamUInt32; cdecl;
    SteamAPI_ISteamUGC_GetSubscribedItems:function(const aSelf:PISteamUGC;const pvecPublishedFileID:PPublishedFileId_t;const cMaxEntries:TSteamUInt32;const bIncludeLocallyDisabled:TSteamBool):TSteamUInt32; cdecl;
    SteamAPI_ISteamUGC_GetItemState:function(const aSelf:PISteamUGC;const nPublishedFileID:TPublishedFileId_t):TSteamUInt32; cdecl;
    SteamAPI_ISteamUGC_GetItemInstallInfo:function(const aSelf:PISteamUGC;const nPublishedFileID:TPublishedFileId_t;const punSizeOnDisk:PSteamUInt64;const pchFolder:PSteamChar;const cchFolderSize:TSteamUInt32;const punTimeStamp:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_GetItemDownloadInfo:function(const aSelf:PISteamUGC;const nPublishedFileID:TPublishedFileId_t;const punBytesDownloaded:PSteamUInt64;const punBytesTotal:PSteamUInt64):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_DownloadItem:function(const aSelf:PISteamUGC;const nPublishedFileID:TPublishedFileId_t;const bHighPriority:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_BInitWorkshopForGameServer:function(const aSelf:PISteamUGC;const unWorkshopDepotID:TDepotId_t;const pszFolder:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SuspendDownloads:procedure(const aSelf:PISteamUGC;const bSuspend:TSteamBool); cdecl;
    SteamAPI_ISteamUGC_StartPlaytimeTracking:function(const aSelf:PISteamUGC;const pvecPublishedFileID:PPublishedFileId_t;const unNumPublishedFileIDs:TSteamUInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_StopPlaytimeTracking:function(const aSelf:PISteamUGC;const pvecPublishedFileID:PPublishedFileId_t;const unNumPublishedFileIDs:TSteamUInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_StopPlaytimeTrackingForAllItems:function(const aSelf:PISteamUGC):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_AddDependency:function(const aSelf:PISteamUGC;const nParentPublishedFileID:TPublishedFileId_t;const nChildPublishedFileID:TPublishedFileId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_RemoveDependency:function(const aSelf:PISteamUGC;const nParentPublishedFileID:TPublishedFileId_t;const nChildPublishedFileID:TPublishedFileId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_AddAppDependency:function(const aSelf:PISteamUGC;const nPublishedFileID:TPublishedFileId_t;const nAppID:TAppId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_RemoveAppDependency:function(const aSelf:PISteamUGC;const nPublishedFileID:TPublishedFileId_t;const nAppID:TAppId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_GetAppDependencies:function(const aSelf:PISteamUGC;const nPublishedFileID:TPublishedFileId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_DeleteItem:function(const aSelf:PISteamUGC;const nPublishedFileID:TPublishedFileId_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_ShowWorkshopEULA:function(const aSelf:PISteamUGC):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_GetWorkshopEULAStatus:function(const aSelf:PISteamUGC):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamUGC_GetUserContentDescriptorPreferences:function(const aSelf:PISteamUGC;const pvecDescriptors:PEUGCContentDescriptorID;const cMaxEntries:TSteamUInt32):TSteamUInt32; cdecl;
    SteamAPI_ISteamUGC_SetItemsDisabledLocally:function(const aSelf:PISteamUGC;const pvecPublishedFileIDs:PPublishedFileId_t;const unNumPublishedFileIDs:TSteamUInt32;const bDisabledLocally:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_SetSubscriptionsLoadOrder:function(const aSelf:PISteamUGC;const pvecPublishedFileIDs:PPublishedFileId_t;const unNumPublishedFileIDs:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_MarkDownloadedItemAsUnused:function(const aSelf:PISteamUGC;const nPublishedFileID:TPublishedFileId_t):TSteamBool; cdecl;
    SteamAPI_ISteamUGC_GetNumDownloadedItems:function(const aSelf:PISteamUGC):TSteamUInt32; cdecl;
    SteamAPI_ISteamUGC_GetDownloadedItems:function(const aSelf:PISteamUGC;const pvecPublishedFileIDs:PPublishedFileId_t;const cMaxEntries:TSteamUInt32):TSteamUInt32; cdecl;

// ISteamHTMLSurface
    SteamAPI_SteamHTMLSurface_v005:function:PISteamHTMLSurface; cdecl;
    SteamAPI_ISteamHTMLSurface_Init:function(const aSelf:PISteamHTMLSurface):TSteamBool; cdecl;
    SteamAPI_ISteamHTMLSurface_Shutdown:function(const aSelf:PISteamHTMLSurface):TSteamBool; cdecl;
    SteamAPI_ISteamHTMLSurface_CreateBrowser:function(const aSelf:PISteamHTMLSurface;const pchUserAgent:PSteamChar;const pchUserCSS:PSteamChar):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamHTMLSurface_RemoveBrowser:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser); cdecl;
    SteamAPI_ISteamHTMLSurface_LoadURL:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const pchURL:PSteamChar;const pchPostData:PSteamChar); cdecl;
    SteamAPI_ISteamHTMLSurface_SetSize:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const unWidth:TSteamUInt32;const unHeight:TSteamUInt32); cdecl;
    SteamAPI_ISteamHTMLSurface_StopLoad:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser); cdecl;
    SteamAPI_ISteamHTMLSurface_Reload:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser); cdecl;
    SteamAPI_ISteamHTMLSurface_GoBack:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser); cdecl;
    SteamAPI_ISteamHTMLSurface_GoForward:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser); cdecl;
    SteamAPI_ISteamHTMLSurface_AddHeader:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const pchKey:PSteamChar;const pchValue:PSteamChar); cdecl;
    SteamAPI_ISteamHTMLSurface_ExecuteJavascript:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const pchScript:PSteamChar); cdecl;
    SteamAPI_ISteamHTMLSurface_MouseUp:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const eMouseButton:TEHTMLMouseButton); cdecl;
    SteamAPI_ISteamHTMLSurface_MouseDown:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const eMouseButton:TEHTMLMouseButton); cdecl;
    SteamAPI_ISteamHTMLSurface_MouseDoubleClick:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const eMouseButton:TEHTMLMouseButton); cdecl;
    SteamAPI_ISteamHTMLSurface_MouseMove:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const x:TSteamInt32;const y:TSteamInt32); cdecl;
    SteamAPI_ISteamHTMLSurface_MouseWheel:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const nDelta:TSteamInt32); cdecl;
    SteamAPI_ISteamHTMLSurface_KeyDown:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const nNativeKeyCode:TSteamUInt32;const eHTMLKeyModifiers:TEHTMLKeyModifiers;const bIsSystemKey:TSteamBool); cdecl;
    SteamAPI_ISteamHTMLSurface_KeyUp:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const nNativeKeyCode:TSteamUInt32;const eHTMLKeyModifiers:TEHTMLKeyModifiers); cdecl;
    SteamAPI_ISteamHTMLSurface_KeyChar:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const cUnicodeChar:TSteamUInt32;const eHTMLKeyModifiers:TEHTMLKeyModifiers); cdecl;
    SteamAPI_ISteamHTMLSurface_SetHorizontalScroll:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const nAbsolutePixelScroll:TSteamUInt32); cdecl;
    SteamAPI_ISteamHTMLSurface_SetVerticalScroll:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const nAbsolutePixelScroll:TSteamUInt32); cdecl;
    SteamAPI_ISteamHTMLSurface_SetKeyFocus:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const bHasKeyFocus:TSteamBool); cdecl;
    SteamAPI_ISteamHTMLSurface_ViewSource:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser); cdecl;
    SteamAPI_ISteamHTMLSurface_CopyToClipboard:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser); cdecl;
    SteamAPI_ISteamHTMLSurface_PasteFromClipboard:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser); cdecl;
    SteamAPI_ISteamHTMLSurface_Find:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const pchSearchStr:PSteamChar;const bCurrentlyInFind:TSteamBool;const bReverse:TSteamBool); cdecl;
    SteamAPI_ISteamHTMLSurface_StopFind:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser); cdecl;
    SteamAPI_ISteamHTMLSurface_GetLinkAtPosition:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const x:TSteamInt32;const y:TSteamInt32); cdecl;
    SteamAPI_ISteamHTMLSurface_SetCookie:procedure(const aSelf:PISteamHTMLSurface;const pchHostname:PSteamChar;const pchKey:PSteamChar;const pchValue:PSteamChar;const pchPath:PSteamChar;const nExpires:TRTime32;const bSecure:TSteamBool;const bHTTPOnly:TSteamBool); cdecl;
    SteamAPI_ISteamHTMLSurface_SetPageScaleFactor:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const flZoom:TSteamFloat;const nPointX:TSteamInt32;const nPointY:TSteamInt32); cdecl;
    SteamAPI_ISteamHTMLSurface_SetBackgroundMode:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const bBackgroundMode:TSteamBool); cdecl;
    SteamAPI_ISteamHTMLSurface_SetDPIScalingFactor:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const flDPIScaling:TSteamFloat); cdecl;
    SteamAPI_ISteamHTMLSurface_OpenDeveloperTools:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser); cdecl;
    SteamAPI_ISteamHTMLSurface_AllowStartRequest:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const bAllowed:TSteamBool); cdecl;
    SteamAPI_ISteamHTMLSurface_JSDialogResponse:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const bResult:TSteamBool); cdecl;
    SteamAPI_ISteamHTMLSurface_FileLoadDialogResponse:procedure(const aSelf:PISteamHTMLSurface;const unBrowserHandle:THHTMLBrowser;const pchSelectedFiles:PPSteamChar); cdecl;

// ISteamInventory
    SteamAPI_SteamInventory_v003:function:PISteamInventory; cdecl;
    SteamAPI_SteamGameServerInventory_v003:function:PISteamInventory; cdecl;
    SteamAPI_ISteamInventory_GetResultStatus:function(const aSelf:PISteamInventory;const resultHandle:TSteamInventoryResult_t):TEResult; cdecl;
    SteamAPI_ISteamInventory_GetResultItems:function(const aSelf:PISteamInventory;const resultHandle:TSteamInventoryResult_t;const pOutItemsArray:PSteamItemDetails_t;const punOutItemsArraySize:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_GetResultItemProperty:function(const aSelf:PISteamInventory;const resultHandle:TSteamInventoryResult_t;const unItemIndex:TSteamUInt32;const pchPropertyName:PSteamChar;const pchValueBuffer:PSteamChar;const punValueBufferSizeOut:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_GetResultTimestamp:function(const aSelf:PISteamInventory;const resultHandle:TSteamInventoryResult_t):TSteamUInt32; cdecl;
    SteamAPI_ISteamInventory_CheckResultSteamID:function(const aSelf:PISteamInventory;const resultHandle:TSteamInventoryResult_t;const steamIDExpected:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_DestroyResult:procedure(const aSelf:PISteamInventory;const resultHandle:TSteamInventoryResult_t); cdecl;
    SteamAPI_ISteamInventory_GetAllItems:function(const aSelf:PISteamInventory;const pResultHandle:PSteamInventoryResult_t):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_GetItemsByID:function(const aSelf:PISteamInventory;const pResultHandle:PSteamInventoryResult_t;const pInstanceIDs:PSteamItemInstanceID_t;const unCountInstanceIDs:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_SerializeResult:function(const aSelf:PISteamInventory;const resultHandle:TSteamInventoryResult_t;const pOutBuffer:TSteamPointer;const punOutBufferSize:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_DeserializeResult:function(const aSelf:PISteamInventory;const pOutResultHandle:PSteamInventoryResult_t;const pBuffer:TSteamPointer;const unBufferSize:TSteamUInt32;const bRESERVED_MUST_BE_FALSE:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_GenerateItems:function(const aSelf:PISteamInventory;const pResultHandle:PSteamInventoryResult_t;const pArrayItemDefs:PSteamItemDef_t;const punArrayQuantity:PSteamUInt32;const unArrayLength:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_GrantPromoItems:function(const aSelf:PISteamInventory;const pResultHandle:PSteamInventoryResult_t):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_AddPromoItem:function(const aSelf:PISteamInventory;const pResultHandle:PSteamInventoryResult_t;const itemDef:TSteamItemDef_t):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_AddPromoItems:function(const aSelf:PISteamInventory;const pResultHandle:PSteamInventoryResult_t;const pArrayItemDefs:PSteamItemDef_t;const unArrayLength:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_ConsumeItem:function(const aSelf:PISteamInventory;const pResultHandle:PSteamInventoryResult_t;const itemConsume:TSteamItemInstanceID_t;const unQuantity:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_ExchangeItems:function(const aSelf:PISteamInventory;const pResultHandle:PSteamInventoryResult_t;const pArrayGenerate:PSteamItemDef_t;const punArrayGenerateQuantity:PSteamUInt32;const unArrayGenerateLength:TSteamUInt32;const pArrayDestroy:PSteamItemInstanceID_t;const punArrayDestroyQuantity:PSteamUInt32;const unArrayDestroyLength:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_TransferItemQuantity:function(const aSelf:PISteamInventory;const pResultHandle:PSteamInventoryResult_t;const itemIdSource:TSteamItemInstanceID_t;const unQuantity:TSteamUInt32;const itemIdDest:TSteamItemInstanceID_t):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_SendItemDropHeartbeat:procedure(const aSelf:PISteamInventory); cdecl;
    SteamAPI_ISteamInventory_TriggerItemDrop:function(const aSelf:PISteamInventory;const pResultHandle:PSteamInventoryResult_t;const dropListDefinition:TSteamItemDef_t):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_TradeItems:function(const aSelf:PISteamInventory;const pResultHandle:PSteamInventoryResult_t;const steamIDTradePartner:TSteamUInt64SteamID;const pArrayGive:PSteamItemInstanceID_t;const pArrayGiveQuantity:PSteamUInt32;const nArrayGiveLength:TSteamUInt32;const pArrayGet:PSteamItemInstanceID_t;const pArrayGetQuantity:PSteamUInt32;const nArrayGetLength:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_LoadItemDefinitions:function(const aSelf:PISteamInventory):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_GetItemDefinitionIDs:function(const aSelf:PISteamInventory;const pItemDefIDs:PSteamItemDef_t;const punItemDefIDsArraySize:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_GetItemDefinitionProperty:function(const aSelf:PISteamInventory;const iDefinition:TSteamItemDef_t;const pchPropertyName:PSteamChar;const pchValueBuffer:PSteamChar;const punValueBufferSizeOut:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_RequestEligiblePromoItemDefinitionsIDs:function(const aSelf:PISteamInventory;const steamID:TSteamUInt64SteamID):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamInventory_GetEligiblePromoItemDefinitionIDs:function(const aSelf:PISteamInventory;const steamID:TSteamUInt64SteamID;const pItemDefIDs:PSteamItemDef_t;const punItemDefIDsArraySize:PSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_StartPurchase:function(const aSelf:PISteamInventory;const pArrayItemDefs:PSteamItemDef_t;const punArrayQuantity:PSteamUInt32;const unArrayLength:TSteamUInt32):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamInventory_RequestPrices:function(const aSelf:PISteamInventory):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamInventory_GetNumItemsWithPrices:function(const aSelf:PISteamInventory):TSteamUInt32; cdecl;
    SteamAPI_ISteamInventory_GetItemsWithPrices:function(const aSelf:PISteamInventory;const pArrayItemDefs:PSteamItemDef_t;const pCurrentPrices:PSteamUInt64;const pBasePrices:PSteamUInt64;const unArrayLength:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_GetItemPrice:function(const aSelf:PISteamInventory;const iDefinition:TSteamItemDef_t;const pCurrentPrice:PSteamUInt64;const pBasePrice:PSteamUInt64):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_StartUpdateProperties:function(const aSelf:PISteamInventory):TSteamInventoryUpdateHandle_t; cdecl;
    SteamAPI_ISteamInventory_RemoveProperty:function(const aSelf:PISteamInventory;const handle:TSteamInventoryUpdateHandle_t;const nItemID:TSteamItemInstanceID_t;const pchPropertyName:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_SetPropertyString:function(const aSelf:PISteamInventory;const handle:TSteamInventoryUpdateHandle_t;const nItemID:TSteamItemInstanceID_t;const pchPropertyName:PSteamChar;const pchPropertyValue:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_SetPropertyBool:function(const aSelf:PISteamInventory;const handle:TSteamInventoryUpdateHandle_t;const nItemID:TSteamItemInstanceID_t;const pchPropertyName:PSteamChar;const bValue:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_SetPropertyInt64:function(const aSelf:PISteamInventory;const handle:TSteamInventoryUpdateHandle_t;const nItemID:TSteamItemInstanceID_t;const pchPropertyName:PSteamChar;const nValue:TSteamInt64):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_SetPropertyFloat:function(const aSelf:PISteamInventory;const handle:TSteamInventoryUpdateHandle_t;const nItemID:TSteamItemInstanceID_t;const pchPropertyName:PSteamChar;const flValue:TSteamFloat):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_SubmitUpdateProperties:function(const aSelf:PISteamInventory;const handle:TSteamInventoryUpdateHandle_t;const pResultHandle:PSteamInventoryResult_t):TSteamBool; cdecl;
    SteamAPI_ISteamInventory_InspectItem:function(const aSelf:PISteamInventory;const pResultHandle:PSteamInventoryResult_t;const pchItemToken:PSteamChar):TSteamBool; cdecl;

// ISteamTimeline
    SteamAPI_SteamTimeline_v004:function:PISteamTimeline; cdecl;
    SteamAPI_ISteamTimeline_SetTimelineTooltip:procedure(const aSelf:PISteamTimeline;const pchDescription:PSteamChar;const flTimeDelta:TSteamFloat); cdecl;
    SteamAPI_ISteamTimeline_ClearTimelineTooltip:procedure(const aSelf:PISteamTimeline;const flTimeDelta:TSteamFloat); cdecl;
    SteamAPI_ISteamTimeline_SetTimelineGameMode:procedure(const aSelf:PISteamTimeline;const eMode:TETimelineGameMode); cdecl;
    SteamAPI_ISteamTimeline_AddInstantaneousTimelineEvent:function(const aSelf:PISteamTimeline;const pchTitle:PSteamChar;const pchDescription:PSteamChar;const pchIcon:PSteamChar;const unIconPriority:TSteamUInt32;const flStartOffsetSeconds:TSteamFloat;const ePossibleClip:TETimelineEventClipPriority):TTimelineEventHandle_t; cdecl;
    SteamAPI_ISteamTimeline_AddRangeTimelineEvent:function(const aSelf:PISteamTimeline;const pchTitle:PSteamChar;const pchDescription:PSteamChar;const pchIcon:PSteamChar;const unIconPriority:TSteamUInt32;const flStartOffsetSeconds:TSteamFloat;const flDuration:TSteamFloat;const ePossibleClip:TETimelineEventClipPriority):TTimelineEventHandle_t; cdecl;
    SteamAPI_ISteamTimeline_StartRangeTimelineEvent:function(const aSelf:PISteamTimeline;const pchTitle:PSteamChar;const pchDescription:PSteamChar;const pchIcon:PSteamChar;const unPriority:TSteamUInt32;const flStartOffsetSeconds:TSteamFloat;const ePossibleClip:TETimelineEventClipPriority):TTimelineEventHandle_t; cdecl;
    SteamAPI_ISteamTimeline_UpdateRangeTimelineEvent:procedure(const aSelf:PISteamTimeline;const ulEvent:TTimelineEventHandle_t;const pchTitle:PSteamChar;const pchDescription:PSteamChar;const pchIcon:PSteamChar;const unPriority:TSteamUInt32;const ePossibleClip:TETimelineEventClipPriority); cdecl;
    SteamAPI_ISteamTimeline_EndRangeTimelineEvent:procedure(const aSelf:PISteamTimeline;const ulEvent:TTimelineEventHandle_t;const flEndOffsetSeconds:TSteamFloat); cdecl;
    SteamAPI_ISteamTimeline_RemoveTimelineEvent:procedure(const aSelf:PISteamTimeline;const ulEvent:TTimelineEventHandle_t); cdecl;
    SteamAPI_ISteamTimeline_DoesEventRecordingExist:function(const aSelf:PISteamTimeline;const ulEvent:TTimelineEventHandle_t):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamTimeline_StartGamePhase:procedure(const aSelf:PISteamTimeline); cdecl;
    SteamAPI_ISteamTimeline_EndGamePhase:procedure(const aSelf:PISteamTimeline); cdecl;
    SteamAPI_ISteamTimeline_SetGamePhaseID:procedure(const aSelf:PISteamTimeline;const pchPhaseID:PSteamChar); cdecl;
    SteamAPI_ISteamTimeline_DoesGamePhaseRecordingExist:function(const aSelf:PISteamTimeline;const pchPhaseID:PSteamChar):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamTimeline_AddGamePhaseTag:procedure(const aSelf:PISteamTimeline;const pchTagName:PSteamChar;const pchTagIcon:PSteamChar;const pchTagGroup:PSteamChar;const unPriority:TSteamUInt32); cdecl;
    SteamAPI_ISteamTimeline_SetGamePhaseAttribute:procedure(const aSelf:PISteamTimeline;const pchAttributeGroup:PSteamChar;const pchAttributeValue:PSteamChar;const unPriority:TSteamUInt32); cdecl;
    SteamAPI_ISteamTimeline_OpenOverlayToGamePhase:procedure(const aSelf:PISteamTimeline;const pchPhaseID:PSteamChar); cdecl;
    SteamAPI_ISteamTimeline_OpenOverlayToTimelineEvent:procedure(const aSelf:PISteamTimeline;const ulEvent:TTimelineEventHandle_t); cdecl;

// ISteamVideo
    SteamAPI_SteamVideo_v007:function:PISteamVideo; cdecl;
    SteamAPI_ISteamVideo_GetVideoURL:procedure(const aSelf:PISteamVideo;const unVideoAppID:TAppId_t); cdecl;
    SteamAPI_ISteamVideo_IsBroadcasting:function(const aSelf:PISteamVideo;const pnNumViewers:PSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamVideo_GetOPFSettings:procedure(const aSelf:PISteamVideo;const unVideoAppID:TAppId_t); cdecl;
    SteamAPI_ISteamVideo_GetOPFStringForApp:function(const aSelf:PISteamVideo;const unVideoAppID:TAppId_t;const pchBuffer:PSteamChar;const pnBufferSize:PSteamInt32):TSteamBool; cdecl;

// ISteamParentalSettings
    SteamAPI_SteamParentalSettings_v001:function:PISteamParentalSettings; cdecl;
    SteamAPI_ISteamParentalSettings_BIsParentalLockEnabled:function(const aSelf:PISteamParentalSettings):TSteamBool; cdecl;
    SteamAPI_ISteamParentalSettings_BIsParentalLockLocked:function(const aSelf:PISteamParentalSettings):TSteamBool; cdecl;
    SteamAPI_ISteamParentalSettings_BIsAppBlocked:function(const aSelf:PISteamParentalSettings;const nAppID:TAppId_t):TSteamBool; cdecl;
    SteamAPI_ISteamParentalSettings_BIsAppInBlockList:function(const aSelf:PISteamParentalSettings;const nAppID:TAppId_t):TSteamBool; cdecl;
    SteamAPI_ISteamParentalSettings_BIsFeatureBlocked:function(const aSelf:PISteamParentalSettings;const eFeature:TEParentalFeature):TSteamBool; cdecl;
    SteamAPI_ISteamParentalSettings_BIsFeatureInBlockList:function(const aSelf:PISteamParentalSettings;const eFeature:TEParentalFeature):TSteamBool; cdecl;

// ISteamRemotePlay
    SteamAPI_SteamRemotePlay_v004:function:PISteamRemotePlay; cdecl;
    SteamAPI_ISteamRemotePlay_GetSessionCount:function(const aSelf:PISteamRemotePlay):TSteamUInt32; cdecl;
    SteamAPI_ISteamRemotePlay_GetSessionID:function(const aSelf:PISteamRemotePlay;const iSessionIndex:TSteamInt32):TRemotePlaySessionID_t; cdecl;
    SteamAPI_ISteamRemotePlay_BSessionRemotePlayTogether:function(const aSelf:PISteamRemotePlay;const unSessionID:TRemotePlaySessionID_t):TSteamBool; cdecl;
    SteamAPI_ISteamRemotePlay_GetSessionSteamID:function(const aSelf:PISteamRemotePlay;const unSessionID:TRemotePlaySessionID_t):TSteamUInt64SteamID; cdecl;
    SteamAPI_ISteamRemotePlay_GetSessionGuestID:function(const aSelf:PISteamRemotePlay;const unSessionID:TRemotePlaySessionID_t):TSteamUInt32; cdecl;
    SteamAPI_ISteamRemotePlay_GetSmallSessionAvatar:function(const aSelf:PISteamRemotePlay;const unSessionID:TRemotePlaySessionID_t):TSteamInt32; cdecl;
    SteamAPI_ISteamRemotePlay_GetMediumSessionAvatar:function(const aSelf:PISteamRemotePlay;const unSessionID:TRemotePlaySessionID_t):TSteamInt32; cdecl;
    SteamAPI_ISteamRemotePlay_GetLargeSessionAvatar:function(const aSelf:PISteamRemotePlay;const unSessionID:TRemotePlaySessionID_t):TSteamInt32; cdecl;
    SteamAPI_ISteamRemotePlay_GetSessionClientName:function(const aSelf:PISteamRemotePlay;const unSessionID:TRemotePlaySessionID_t):PSteamChar; cdecl;
    SteamAPI_ISteamRemotePlay_GetSessionClientFormFactor:function(const aSelf:PISteamRemotePlay;const unSessionID:TRemotePlaySessionID_t):TESteamDeviceFormFactor; cdecl;
    SteamAPI_ISteamRemotePlay_BGetSessionClientResolution:function(const aSelf:PISteamRemotePlay;const unSessionID:TRemotePlaySessionID_t;const pnResolutionX:PSteamInt32;const pnResolutionY:PSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamRemotePlay_ShowRemotePlayTogetherUI:function(const aSelf:PISteamRemotePlay):TSteamBool; cdecl;
    SteamAPI_ISteamRemotePlay_BSendRemotePlayTogetherInvite:function(const aSelf:PISteamRemotePlay;const steamIDFriend:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamRemotePlay_BEnableRemotePlayTogetherDirectInput:function(const aSelf:PISteamRemotePlay):TSteamBool; cdecl;
    SteamAPI_ISteamRemotePlay_DisableRemotePlayTogetherDirectInput:procedure(const aSelf:PISteamRemotePlay); cdecl;
    SteamAPI_ISteamRemotePlay_GetInput:function(const aSelf:PISteamRemotePlay;const pInput:PRemotePlayInput_t;const unMaxEvents:TSteamUInt32):TSteamUInt32; cdecl;
    SteamAPI_ISteamRemotePlay_SetMouseVisibility:procedure(const aSelf:PISteamRemotePlay;const unSessionID:TRemotePlaySessionID_t;const bVisible:TSteamBool); cdecl;
    SteamAPI_ISteamRemotePlay_SetMousePosition:procedure(const aSelf:PISteamRemotePlay;const unSessionID:TRemotePlaySessionID_t;const flNormalizedX:TSteamFloat;const flNormalizedY:TSteamFloat); cdecl;
    SteamAPI_ISteamRemotePlay_CreateMouseCursor:function(const aSelf:PISteamRemotePlay;const nWidth:TSteamInt32;const nHeight:TSteamInt32;const nHotX:TSteamInt32;const nHotY:TSteamInt32;const pBGRA:TSteamPointer;const nPitch:TSteamInt32):TRemotePlayCursorID_t; cdecl;
    SteamAPI_ISteamRemotePlay_SetMouseCursor:procedure(const aSelf:PISteamRemotePlay;const unSessionID:TRemotePlaySessionID_t;const unCursorID:TRemotePlayCursorID_t); cdecl;

// ISteamNetworkingMessages
    SteamAPI_SteamNetworkingMessages_SteamAPI_v002:function:PISteamNetworkingMessages; cdecl;
    SteamAPI_SteamGameServerNetworkingMessages_SteamAPI_v002:function:PISteamNetworkingMessages; cdecl;
    SteamAPI_ISteamNetworkingMessages_SendMessageToUser:function(const aSelf:PISteamNetworkingMessages;const identityRemote:PSteamNetworkingIdentity;const pubData:TSteamPointer;const cubData:TSteamUInt32;const nSendFlags:TSteamInt32;const nRemoteChannel:TSteamInt32):TEResult; cdecl;
    SteamAPI_ISteamNetworkingMessages_ReceiveMessagesOnChannel:function(const aSelf:PISteamNetworkingMessages;const nLocalChannel:TSteamInt32;const ppOutMessages:PPSteamNetworkingMessage_t;const nMaxMessages:TSteamInt32):TSteamInt32; cdecl;
    SteamAPI_ISteamNetworkingMessages_AcceptSessionWithUser:function(const aSelf:PISteamNetworkingMessages;const identityRemote:PSteamNetworkingIdentity):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingMessages_CloseSessionWithUser:function(const aSelf:PISteamNetworkingMessages;const identityRemote:PSteamNetworkingIdentity):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingMessages_CloseChannelWithUser:function(const aSelf:PISteamNetworkingMessages;const identityRemote:PSteamNetworkingIdentity;const nLocalChannel:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingMessages_GetSessionConnectionInfo:function(const aSelf:PISteamNetworkingMessages;const identityRemote:PSteamNetworkingIdentity;const pConnectionInfo:PSteamNetConnectionInfo_t;const pQuickStatus:PSteamNetConnectionRealTimeStatus_t):TESteamNetworkingConnectionState; cdecl;

// ISteamNetworkingSockets
    SteamAPI_SteamNetworkingSockets_SteamAPI_v012:function:PISteamNetworkingSockets; cdecl;
    SteamAPI_SteamGameServerNetworkingSockets_SteamAPI_v012:function:PISteamNetworkingSockets; cdecl;
    SteamAPI_ISteamNetworkingSockets_CreateListenSocketIP:function(const aSelf:PISteamNetworkingSockets;const localAddress:PSteamNetworkingIPAddr;const nOptions:TSteamInt32;const pOptions:PSteamNetworkingConfigValue_t):THSteamListenSocket; cdecl;
    SteamAPI_ISteamNetworkingSockets_ConnectByIPAddress:function(const aSelf:PISteamNetworkingSockets;const address:PSteamNetworkingIPAddr;const nOptions:TSteamInt32;const pOptions:PSteamNetworkingConfigValue_t):THSteamNetConnection; cdecl;
    SteamAPI_ISteamNetworkingSockets_CreateListenSocketP2P:function(const aSelf:PISteamNetworkingSockets;const nLocalVirtualPort:TSteamInt32;const nOptions:TSteamInt32;const pOptions:PSteamNetworkingConfigValue_t):THSteamListenSocket; cdecl;
    SteamAPI_ISteamNetworkingSockets_ConnectP2P:function(const aSelf:PISteamNetworkingSockets;const identityRemote:PSteamNetworkingIdentity;const nRemoteVirtualPort:TSteamInt32;const nOptions:TSteamInt32;const pOptions:PSteamNetworkingConfigValue_t):THSteamNetConnection; cdecl;
    SteamAPI_ISteamNetworkingSockets_AcceptConnection:function(const aSelf:PISteamNetworkingSockets;const hConn:THSteamNetConnection):TEResult; cdecl;
    SteamAPI_ISteamNetworkingSockets_CloseConnection:function(const aSelf:PISteamNetworkingSockets;const hPeer:THSteamNetConnection;const nReason:TSteamInt32;const pszDebug:PSteamChar;const bEnableLinger:TSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingSockets_CloseListenSocket:function(const aSelf:PISteamNetworkingSockets;const hSocket:THSteamListenSocket):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingSockets_SetConnectionUserData:function(const aSelf:PISteamNetworkingSockets;const hPeer:THSteamNetConnection;const nUserData:TSteamInt64):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingSockets_GetConnectionUserData:function(const aSelf:PISteamNetworkingSockets;const hPeer:THSteamNetConnection):TSteamInt64; cdecl;
    SteamAPI_ISteamNetworkingSockets_SetConnectionName:procedure(const aSelf:PISteamNetworkingSockets;const hPeer:THSteamNetConnection;const pszName:PSteamChar); cdecl;
    SteamAPI_ISteamNetworkingSockets_GetConnectionName:function(const aSelf:PISteamNetworkingSockets;const hPeer:THSteamNetConnection;const pszName:PSteamChar;const nMaxLen:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingSockets_SendMessageToConnection:function(const aSelf:PISteamNetworkingSockets;const hConn:THSteamNetConnection;const pData:TSteamPointer;const cbData:TSteamUInt32;const nSendFlags:TSteamInt32;const pOutMessageNumber:PSteamInt64):TEResult; cdecl;
    SteamAPI_ISteamNetworkingSockets_SendMessages:procedure(const aSelf:PISteamNetworkingSockets;const nMessages:TSteamInt32;const pMessages:PPSteamNetworkingMessage_t;const pOutMessageNumberOrResult:PSteamInt64); cdecl;
    SteamAPI_ISteamNetworkingSockets_FlushMessagesOnConnection:function(const aSelf:PISteamNetworkingSockets;const hConn:THSteamNetConnection):TEResult; cdecl;
    SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnConnection:function(const aSelf:PISteamNetworkingSockets;const hConn:THSteamNetConnection;const ppOutMessages:PPSteamNetworkingMessage_t;const nMaxMessages:TSteamInt32):TSteamInt32; cdecl;
    SteamAPI_ISteamNetworkingSockets_GetConnectionInfo:function(const aSelf:PISteamNetworkingSockets;const hConn:THSteamNetConnection;const pInfo:PSteamNetConnectionInfo_t):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingSockets_GetConnectionRealTimeStatus:function(const aSelf:PISteamNetworkingSockets;const hConn:THSteamNetConnection;const pStatus:PSteamNetConnectionRealTimeStatus_t;const nLanes:TSteamInt32;const pLanes:PSteamNetConnectionRealTimeLaneStatus_t):TEResult; cdecl;
    SteamAPI_ISteamNetworkingSockets_GetDetailedConnectionStatus:function(const aSelf:PISteamNetworkingSockets;const hConn:THSteamNetConnection;const pszBuf:PSteamChar;const cbBuf:TSteamInt32):TSteamInt32; cdecl;
    SteamAPI_ISteamNetworkingSockets_GetListenSocketAddress:function(const aSelf:PISteamNetworkingSockets;const hSocket:THSteamListenSocket;const address:PSteamNetworkingIPAddr):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingSockets_CreateSocketPair:function(const aSelf:PISteamNetworkingSockets;const pOutConnection1:PHSteamNetConnection;const pOutConnection2:PHSteamNetConnection;const bUseNetworkLoopback:TSteamBool;const pIdentity1:PSteamNetworkingIdentity;const pIdentity2:PSteamNetworkingIdentity):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingSockets_ConfigureConnectionLanes:function(const aSelf:PISteamNetworkingSockets;const hConn:THSteamNetConnection;const nNumLanes:TSteamInt32;const pLanePriorities:PSteamInt32;const pLaneWeights:PSteamUInt16):TEResult; cdecl;
    SteamAPI_ISteamNetworkingSockets_GetIdentity:function(const aSelf:PISteamNetworkingSockets;const pIdentity:PSteamNetworkingIdentity):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingSockets_InitAuthentication:function(const aSelf:PISteamNetworkingSockets):TESteamNetworkingAvailability; cdecl;
    SteamAPI_ISteamNetworkingSockets_GetAuthenticationStatus:function(const aSelf:PISteamNetworkingSockets;const pDetails:PSteamNetAuthenticationStatus_t):TESteamNetworkingAvailability; cdecl;
    SteamAPI_ISteamNetworkingSockets_CreatePollGroup:function(const aSelf:PISteamNetworkingSockets):THSteamNetPollGroup; cdecl;
    SteamAPI_ISteamNetworkingSockets_DestroyPollGroup:function(const aSelf:PISteamNetworkingSockets;const hPollGroup:THSteamNetPollGroup):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingSockets_SetConnectionPollGroup:function(const aSelf:PISteamNetworkingSockets;const hConn:THSteamNetConnection;const hPollGroup:THSteamNetPollGroup):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnPollGroup:function(const aSelf:PISteamNetworkingSockets;const hPollGroup:THSteamNetPollGroup;const ppOutMessages:PPSteamNetworkingMessage_t;const nMaxMessages:TSteamInt32):TSteamInt32; cdecl;
    SteamAPI_ISteamNetworkingSockets_ReceivedRelayAuthTicket:function(const aSelf:PISteamNetworkingSockets;const pvTicket:TSteamPointer;const cbTicket:TSteamInt32;const pOutParsedTicket:PSteamDatagramRelayAuthTicket):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingSockets_FindRelayAuthTicketForServer:function(const aSelf:PISteamNetworkingSockets;const identityGameServer:PSteamNetworkingIdentity;const nRemoteVirtualPort:TSteamInt32;const pOutParsedTicket:PSteamDatagramRelayAuthTicket):TSteamInt32; cdecl;
    SteamAPI_ISteamNetworkingSockets_ConnectToHostedDedicatedServer:function(const aSelf:PISteamNetworkingSockets;const identityTarget:PSteamNetworkingIdentity;const nRemoteVirtualPort:TSteamInt32;const nOptions:TSteamInt32;const pOptions:PSteamNetworkingConfigValue_t):THSteamNetConnection; cdecl;
    SteamAPI_ISteamNetworkingSockets_GetHostedDedicatedServerPort:function(const aSelf:PISteamNetworkingSockets):TSteamUInt16; cdecl;
    SteamAPI_ISteamNetworkingSockets_GetHostedDedicatedServerPOPID:function(const aSelf:PISteamNetworkingSockets):TSteamNetworkingPOPID; cdecl;
    SteamAPI_ISteamNetworkingSockets_GetHostedDedicatedServerAddress:function(const aSelf:PISteamNetworkingSockets;const pRouting:PSteamDatagramHostedAddress):TEResult; cdecl;
    SteamAPI_ISteamNetworkingSockets_CreateHostedDedicatedServerListenSocket:function(const aSelf:PISteamNetworkingSockets;const nLocalVirtualPort:TSteamInt32;const nOptions:TSteamInt32;const pOptions:PSteamNetworkingConfigValue_t):THSteamListenSocket; cdecl;
    SteamAPI_ISteamNetworkingSockets_GetGameCoordinatorServerLogin:function(const aSelf:PISteamNetworkingSockets;const pLoginInfo:PSteamDatagramGameCoordinatorServerLogin;const pcbSignedBlob:PSteamInt32;const pBlob:TSteamPointer):TEResult; cdecl;
    SteamAPI_ISteamNetworkingSockets_ConnectP2PCustomSignaling:function(const aSelf:PISteamNetworkingSockets;const pSignaling:PISteamNetworkingConnectionSignaling;const pPeerIdentity:PSteamNetworkingIdentity;const nRemoteVirtualPort:TSteamInt32;const nOptions:TSteamInt32;const pOptions:PSteamNetworkingConfigValue_t):THSteamNetConnection; cdecl;
    SteamAPI_ISteamNetworkingSockets_ReceivedP2PCustomSignal:function(const aSelf:PISteamNetworkingSockets;const pMsg:TSteamPointer;const cbMsg:TSteamInt32;const pContext:PISteamNetworkingSignalingRecvContext):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingSockets_GetCertificateRequest:function(const aSelf:PISteamNetworkingSockets;const pcbBlob:PSteamInt32;const pBlob:TSteamPointer;const errMsg:PSteamNetworkingErrMsg):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingSockets_SetCertificate:function(const aSelf:PISteamNetworkingSockets;const pCertificate:TSteamPointer;const cbCertificate:TSteamInt32;const errMsg:PSteamNetworkingErrMsg):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingSockets_ResetIdentity:procedure(const aSelf:PISteamNetworkingSockets;const pIdentity:PSteamNetworkingIdentity); cdecl;
    SteamAPI_ISteamNetworkingSockets_RunCallbacks:procedure(const aSelf:PISteamNetworkingSockets); cdecl;
    SteamAPI_ISteamNetworkingSockets_BeginAsyncRequestFakeIP:function(const aSelf:PISteamNetworkingSockets;const nNumPorts:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingSockets_GetFakeIP:procedure(const aSelf:PISteamNetworkingSockets;const idxFirstPort:TSteamInt32;const pInfo:PSteamNetworkingFakeIPResult_t); cdecl;
    SteamAPI_ISteamNetworkingSockets_CreateListenSocketP2PFakeIP:function(const aSelf:PISteamNetworkingSockets;const idxFakePort:TSteamInt32;const nOptions:TSteamInt32;const pOptions:PSteamNetworkingConfigValue_t):THSteamListenSocket; cdecl;
    SteamAPI_ISteamNetworkingSockets_GetRemoteFakeIPForConnection:function(const aSelf:PISteamNetworkingSockets;const hConn:THSteamNetConnection;const pOutAddr:PSteamNetworkingIPAddr):TEResult; cdecl;
    SteamAPI_ISteamNetworkingSockets_CreateFakeUDPPort:function(const aSelf:PISteamNetworkingSockets;const idxFakeServerPort:TSteamInt32):PISteamNetworkingFakeUDPPort; cdecl;

// ISteamNetworkingUtils
    SteamAPI_SteamNetworkingUtils_SteamAPI_v004:function:PISteamNetworkingUtils; cdecl;
    SteamAPI_ISteamNetworkingUtils_AllocateMessage:function(const aSelf:PISteamNetworkingUtils;const cbAllocateBuffer:TSteamInt32):PSteamNetworkingMessage_t; cdecl;
    SteamAPI_ISteamNetworkingUtils_InitRelayNetworkAccess:procedure(const aSelf:PISteamNetworkingUtils); cdecl;
    SteamAPI_ISteamNetworkingUtils_GetRelayNetworkStatus:function(const aSelf:PISteamNetworkingUtils;const pDetails:PSteamRelayNetworkStatus_t):TESteamNetworkingAvailability; cdecl;
    SteamAPI_ISteamNetworkingUtils_GetLocalPingLocation:function(const aSelf:PISteamNetworkingUtils;const aResult:PSteamNetworkPingLocation_t):TSteamFloat; cdecl;
    SteamAPI_ISteamNetworkingUtils_EstimatePingTimeBetweenTwoLocations:function(const aSelf:PISteamNetworkingUtils;const location1:PSteamNetworkPingLocation_t;const location2:PSteamNetworkPingLocation_t):TSteamInt32; cdecl;
    SteamAPI_ISteamNetworkingUtils_EstimatePingTimeFromLocalHost:function(const aSelf:PISteamNetworkingUtils;const remoteLocation:PSteamNetworkPingLocation_t):TSteamInt32; cdecl;
    SteamAPI_ISteamNetworkingUtils_ConvertPingLocationToString:procedure(const aSelf:PISteamNetworkingUtils;const location:PSteamNetworkPingLocation_t;const pszBuf:PSteamChar;const cchBufSize:TSteamInt32); cdecl;
    SteamAPI_ISteamNetworkingUtils_ParsePingLocationString:function(const aSelf:PISteamNetworkingUtils;const pszString:PSteamChar;const aResult:PSteamNetworkPingLocation_t):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_CheckPingDataUpToDate:function(const aSelf:PISteamNetworkingUtils;const flMaxAgeSeconds:TSteamFloat):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_GetPingToDataCenter:function(const aSelf:PISteamNetworkingUtils;const popID:TSteamNetworkingPOPID;const pViaRelayPoP:PSteamNetworkingPOPID):TSteamInt32; cdecl;
    SteamAPI_ISteamNetworkingUtils_GetDirectPingToPOP:function(const aSelf:PISteamNetworkingUtils;const popID:TSteamNetworkingPOPID):TSteamInt32; cdecl;
    SteamAPI_ISteamNetworkingUtils_GetPOPCount:function(const aSelf:PISteamNetworkingUtils):TSteamInt32; cdecl;
    SteamAPI_ISteamNetworkingUtils_GetPOPList:function(const aSelf:PISteamNetworkingUtils;const list:PSteamNetworkingPOPID;const nListSz:TSteamInt32):TSteamInt32; cdecl;
    SteamAPI_ISteamNetworkingUtils_GetLocalTimestamp:function(const aSelf:PISteamNetworkingUtils):TSteamNetworkingMicroseconds; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetDebugOutputFunction:procedure(const aSelf:PISteamNetworkingUtils;const eDetailLevel:TESteamNetworkingSocketsDebugOutputType;const pfnFunc:TFSteamNetworkingSocketsDebugOutput); cdecl;
    SteamAPI_ISteamNetworkingUtils_IsFakeIPv4:function(const aSelf:PISteamNetworkingUtils;const nIPv4:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_GetIPv4FakeIPType:function(const aSelf:PISteamNetworkingUtils;const nIPv4:TSteamUInt32):TESteamNetworkingFakeIPType; cdecl;
    SteamAPI_ISteamNetworkingUtils_GetRealIdentityForFakeIP:function(const aSelf:PISteamNetworkingUtils;const fakeIP:PSteamNetworkingIPAddr;const pOutRealIdentity:PSteamNetworkingIdentity):TEResult; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueInt32:function(const aSelf:PISteamNetworkingUtils;const eValue:TESteamNetworkingConfigValue;const val:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueFloat:function(const aSelf:PISteamNetworkingUtils;const eValue:TESteamNetworkingConfigValue;const val:TSteamFloat):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueString:function(const aSelf:PISteamNetworkingUtils;const eValue:TESteamNetworkingConfigValue;const val:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValuePtr:function(const aSelf:PISteamNetworkingUtils;const eValue:TESteamNetworkingConfigValue;const val:TSteamPointer):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueInt32:function(const aSelf:PISteamNetworkingUtils;const hConn:THSteamNetConnection;const eValue:TESteamNetworkingConfigValue;const val:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueFloat:function(const aSelf:PISteamNetworkingUtils;const hConn:THSteamNetConnection;const eValue:TESteamNetworkingConfigValue;const val:TSteamFloat):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueString:function(const aSelf:PISteamNetworkingUtils;const hConn:THSteamNetConnection;const eValue:TESteamNetworkingConfigValue;const val:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamNetConnectionStatusChanged:function(const aSelf:PISteamNetworkingUtils;const fnCallback:TFnSteamNetConnectionStatusChanged):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamNetAuthenticationStatusChanged:function(const aSelf:PISteamNetworkingUtils;const fnCallback:TFnSteamNetAuthenticationStatusChanged):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamRelayNetworkStatusChanged:function(const aSelf:PISteamNetworkingUtils;const fnCallback:TFnSteamRelayNetworkStatusChanged):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_FakeIPResult:function(const aSelf:PISteamNetworkingUtils;const fnCallback:TFnSteamNetworkingFakeIPResult):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_MessagesSessionRequest:function(const aSelf:PISteamNetworkingUtils;const fnCallback:TFnSteamNetworkingMessagesSessionRequest):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_MessagesSessionFailed:function(const aSelf:PISteamNetworkingUtils;const fnCallback:TFnSteamNetworkingMessagesSessionFailed):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetConfigValue:function(const aSelf:PISteamNetworkingUtils;const eValue:TESteamNetworkingConfigValue;const eScopeType:TESteamNetworkingConfigScope;const scopeObj:TSteamPtrInt;const eDataType:TESteamNetworkingConfigDataType;const pArg:TSteamPointer):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_SetConfigValueStruct:function(const aSelf:PISteamNetworkingUtils;const opt:PSteamNetworkingConfigValue_t;const eScopeType:TESteamNetworkingConfigScope;const scopeObj:TSteamPtrInt):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_GetConfigValue:function(const aSelf:PISteamNetworkingUtils;const eValue:TESteamNetworkingConfigValue;const eScopeType:TESteamNetworkingConfigScope;const scopeObj:TSteamPtrInt;const pOutDataType:PESteamNetworkingConfigDataType;const pResult:TSteamPointer;const cbResult:PSteamPtrUInt):TESteamNetworkingGetConfigValueResult; cdecl;
    SteamAPI_ISteamNetworkingUtils_GetConfigValueInfo:function(const aSelf:PISteamNetworkingUtils;const eValue:TESteamNetworkingConfigValue;const pOutDataType:PESteamNetworkingConfigDataType;const pOutScope:PESteamNetworkingConfigScope):PSteamChar; cdecl;
    SteamAPI_ISteamNetworkingUtils_IterateGenericEditableConfigValues:function(const aSelf:PISteamNetworkingUtils;const eCurrent:TESteamNetworkingConfigValue;const bEnumerateDevVars:TSteamBool):TESteamNetworkingConfigValue; cdecl;
    SteamAPI_ISteamNetworkingUtils_SteamNetworkingIPAddr_ToString:procedure(const aSelf:PISteamNetworkingUtils;const addr:PSteamNetworkingIPAddr;const buf:PSteamChar;const cbBuf:TSteamUInt32;const bWithPort:TSteamBool); cdecl;
    SteamAPI_ISteamNetworkingUtils_SteamNetworkingIPAddr_ParseString:function(const aSelf:PISteamNetworkingUtils;const pAddr:PSteamNetworkingIPAddr;const pszStr:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamNetworkingUtils_SteamNetworkingIPAddr_GetFakeIPType:function(const aSelf:PISteamNetworkingUtils;const addr:PSteamNetworkingIPAddr):TESteamNetworkingFakeIPType; cdecl;
    SteamAPI_ISteamNetworkingUtils_SteamNetworkingIdentity_ToString:procedure(const aSelf:PISteamNetworkingUtils;const identity:PSteamNetworkingIdentity;const buf:PSteamChar;const cbBuf:TSteamUInt32); cdecl;
    SteamAPI_ISteamNetworkingUtils_SteamNetworkingIdentity_ParseString:function(const aSelf:PISteamNetworkingUtils;const pIdentity:PSteamNetworkingIdentity;const pszStr:PSteamChar):TSteamBool; cdecl;

// ISteamGameServer
    SteamAPI_SteamGameServer_v015:function:PISteamGameServer; cdecl;
    SteamAPI_ISteamGameServer_SetProduct:procedure(const aSelf:PISteamGameServer;const pszProduct:PSteamChar); cdecl;
    SteamAPI_ISteamGameServer_SetGameDescription:procedure(const aSelf:PISteamGameServer;const pszGameDescription:PSteamChar); cdecl;
    SteamAPI_ISteamGameServer_SetModDir:procedure(const aSelf:PISteamGameServer;const pszModDir:PSteamChar); cdecl;
    SteamAPI_ISteamGameServer_SetDedicatedServer:procedure(const aSelf:PISteamGameServer;const bDedicated:TSteamBool); cdecl;
    SteamAPI_ISteamGameServer_LogOn:procedure(const aSelf:PISteamGameServer;const pszToken:PSteamChar); cdecl;
    SteamAPI_ISteamGameServer_LogOnAnonymous:procedure(const aSelf:PISteamGameServer); cdecl;
    SteamAPI_ISteamGameServer_LogOff:procedure(const aSelf:PISteamGameServer); cdecl;
    SteamAPI_ISteamGameServer_BLoggedOn:function(const aSelf:PISteamGameServer):TSteamBool; cdecl;
    SteamAPI_ISteamGameServer_BSecure:function(const aSelf:PISteamGameServer):TSteamBool; cdecl;
    SteamAPI_ISteamGameServer_GetSteamID:function(const aSelf:PISteamGameServer):TSteamUInt64SteamID; cdecl;
    SteamAPI_ISteamGameServer_WasRestartRequested:function(const aSelf:PISteamGameServer):TSteamBool; cdecl;
    SteamAPI_ISteamGameServer_SetMaxPlayerCount:procedure(const aSelf:PISteamGameServer;const cPlayersMax:TSteamInt32); cdecl;
    SteamAPI_ISteamGameServer_SetBotPlayerCount:procedure(const aSelf:PISteamGameServer;const cBotplayers:TSteamInt32); cdecl;
    SteamAPI_ISteamGameServer_SetServerName:procedure(const aSelf:PISteamGameServer;const pszServerName:PSteamChar); cdecl;
    SteamAPI_ISteamGameServer_SetMapName:procedure(const aSelf:PISteamGameServer;const pszMapName:PSteamChar); cdecl;
    SteamAPI_ISteamGameServer_SetPasswordProtected:procedure(const aSelf:PISteamGameServer;const bPasswordProtected:TSteamBool); cdecl;
    SteamAPI_ISteamGameServer_SetSpectatorPort:procedure(const aSelf:PISteamGameServer;const unSpectatorPort:TSteamUInt16); cdecl;
    SteamAPI_ISteamGameServer_SetSpectatorServerName:procedure(const aSelf:PISteamGameServer;const pszSpectatorServerName:PSteamChar); cdecl;
    SteamAPI_ISteamGameServer_ClearAllKeyValues:procedure(const aSelf:PISteamGameServer); cdecl;
    SteamAPI_ISteamGameServer_SetKeyValue:procedure(const aSelf:PISteamGameServer;const pKey:PSteamChar;const pValue:PSteamChar); cdecl;
    SteamAPI_ISteamGameServer_SetGameTags:procedure(const aSelf:PISteamGameServer;const pchGameTags:PSteamChar); cdecl;
    SteamAPI_ISteamGameServer_SetGameData:procedure(const aSelf:PISteamGameServer;const pchGameData:PSteamChar); cdecl;
    SteamAPI_ISteamGameServer_SetRegion:procedure(const aSelf:PISteamGameServer;const pszRegion:PSteamChar); cdecl;
    SteamAPI_ISteamGameServer_SetAdvertiseServerActive:procedure(const aSelf:PISteamGameServer;const bActive:TSteamBool); cdecl;
    SteamAPI_ISteamGameServer_GetAuthSessionTicket:function(const aSelf:PISteamGameServer;const pTicket:TSteamPointer;const cbMaxTicket:TSteamInt32;const pcbTicket:PSteamUInt32;const pSnid:PSteamNetworkingIdentity):THAuthTicket; cdecl;
    SteamAPI_ISteamGameServer_BeginAuthSession:function(const aSelf:PISteamGameServer;const pAuthTicket:TSteamPointer;const cbAuthTicket:TSteamInt32;const steamID:TSteamUInt64SteamID):TEBeginAuthSessionResult; cdecl;
    SteamAPI_ISteamGameServer_EndAuthSession:procedure(const aSelf:PISteamGameServer;const steamID:TSteamUInt64SteamID); cdecl;
    SteamAPI_ISteamGameServer_CancelAuthTicket:procedure(const aSelf:PISteamGameServer;const hAuthTicket:THAuthTicket); cdecl;
    SteamAPI_ISteamGameServer_UserHasLicenseForApp:function(const aSelf:PISteamGameServer;const steamID:TSteamUInt64SteamID;const appID:TAppId_t):TEUserHasLicenseForAppResult; cdecl;
    SteamAPI_ISteamGameServer_RequestUserGroupStatus:function(const aSelf:PISteamGameServer;const steamIDUser:TSteamUInt64SteamID;const steamIDGroup:TSteamUInt64SteamID):TSteamBool; cdecl;
    SteamAPI_ISteamGameServer_GetGameplayStats:procedure(const aSelf:PISteamGameServer); cdecl;
    SteamAPI_ISteamGameServer_GetServerReputation:function(const aSelf:PISteamGameServer):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamGameServer_GetPublicIP:function(const aSelf:PISteamGameServer):TSteamIPAddress_t; cdecl;
    SteamAPI_ISteamGameServer_HandleIncomingPacket:function(const aSelf:PISteamGameServer;const pData:TSteamPointer;const cbData:TSteamInt32;const srcIP:TSteamUInt32;const srcPort:TSteamUInt16):TSteamBool; cdecl;
    SteamAPI_ISteamGameServer_GetNextOutgoingPacket:function(const aSelf:PISteamGameServer;const pOut:TSteamPointer;const cbMaxOut:TSteamInt32;const pNetAdr:PSteamUInt32;const pPort:PSteamUInt16):TSteamInt32; cdecl;
    SteamAPI_ISteamGameServer_AssociateWithClan:function(const aSelf:PISteamGameServer;const steamIDClan:TSteamUInt64SteamID):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamGameServer_ComputeNewPlayerCompatibility:function(const aSelf:PISteamGameServer;const steamIDNewPlayer:TSteamUInt64SteamID):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamGameServer_SendUserConnectAndAuthenticate_DEPRECATED:function(const aSelf:PISteamGameServer;const unIPClient:TSteamUInt32;const pvAuthBlob:TSteamPointer;const cubAuthBlobSize:TSteamUInt32;const pSteamIDUser:PCSteamID):TSteamBool; cdecl;
    SteamAPI_ISteamGameServer_CreateUnauthenticatedUserConnection:function(const aSelf:PISteamGameServer):TSteamUInt64SteamID; cdecl;
    SteamAPI_ISteamGameServer_SendUserDisconnect_DEPRECATED:procedure(const aSelf:PISteamGameServer;const steamIDUser:TSteamUInt64SteamID); cdecl;
    SteamAPI_ISteamGameServer_BUpdateUserData:function(const aSelf:PISteamGameServer;const steamIDUser:TSteamUInt64SteamID;const pchPlayerName:PSteamChar;const uScore:TSteamUInt32):TSteamBool; cdecl;

// ISteamGameServerStats
    SteamAPI_SteamGameServerStats_v001:function:PISteamGameServerStats; cdecl;
    SteamAPI_ISteamGameServerStats_RequestUserStats:function(const aSelf:PISteamGameServerStats;const steamIDUser:TSteamUInt64SteamID):TSteamAPICall_t; cdecl;
    SteamAPI_ISteamGameServerStats_GetUserStatInt32:function(const aSelf:PISteamGameServerStats;const steamIDUser:TSteamUInt64SteamID;const pchName:PSteamChar;const pData:PSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamGameServerStats_GetUserStatFloat:function(const aSelf:PISteamGameServerStats;const steamIDUser:TSteamUInt64SteamID;const pchName:PSteamChar;const pData:PSteamFloat):TSteamBool; cdecl;
    SteamAPI_ISteamGameServerStats_GetUserAchievement:function(const aSelf:PISteamGameServerStats;const steamIDUser:TSteamUInt64SteamID;const pchName:PSteamChar;const pbAchieved:PSteamBool):TSteamBool; cdecl;
    SteamAPI_ISteamGameServerStats_SetUserStatInt32:function(const aSelf:PISteamGameServerStats;const steamIDUser:TSteamUInt64SteamID;const pchName:PSteamChar;const nData:TSteamInt32):TSteamBool; cdecl;
    SteamAPI_ISteamGameServerStats_SetUserStatFloat:function(const aSelf:PISteamGameServerStats;const steamIDUser:TSteamUInt64SteamID;const pchName:PSteamChar;const fData:TSteamFloat):TSteamBool; cdecl;
    SteamAPI_ISteamGameServerStats_UpdateUserAvgRateStat:function(const aSelf:PISteamGameServerStats;const steamIDUser:TSteamUInt64SteamID;const pchName:PSteamChar;const flCountThisSession:TSteamFloat;const dSessionLength:TSteamDouble):TSteamBool; cdecl;
    SteamAPI_ISteamGameServerStats_SetUserAchievement:function(const aSelf:PISteamGameServerStats;const steamIDUser:TSteamUInt64SteamID;const pchName:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamGameServerStats_ClearUserAchievement:function(const aSelf:PISteamGameServerStats;const steamIDUser:TSteamUInt64SteamID;const pchName:PSteamChar):TSteamBool; cdecl;
    SteamAPI_ISteamGameServerStats_StoreUserStats:function(const aSelf:PISteamGameServerStats;const steamIDUser:TSteamUInt64SteamID):TSteamAPICall_t; cdecl;

// ISteamNetworkingFakeUDPPort
    SteamAPI_ISteamNetworkingFakeUDPPort_DestroyFakeUDPPort:procedure(const aSelf:PISteamNetworkingFakeUDPPort); cdecl;
    SteamAPI_ISteamNetworkingFakeUDPPort_SendMessageToFakeIP:function(const aSelf:PISteamNetworkingFakeUDPPort;const remoteAddress:PSteamNetworkingIPAddr;const pData:TSteamPointer;const cbData:TSteamUInt32;const nSendFlags:TSteamInt32):TEResult; cdecl;
    SteamAPI_ISteamNetworkingFakeUDPPort_ReceiveMessages:function(const aSelf:PISteamNetworkingFakeUDPPort;const ppOutMessages:PPSteamNetworkingMessage_t;const nMaxMessages:TSteamInt32):TSteamInt32; cdecl;
    SteamAPI_ISteamNetworkingFakeUDPPort_ScheduleCleanup:procedure(const aSelf:PISteamNetworkingFakeUDPPort;const remoteAddress:PSteamNetworkingIPAddr); cdecl;

// SteamIPAddress_t
    SteamAPI_SteamIPAddress_t_IsSet:function(const aSelf:PSteamIPAddress_t):TSteamBool; cdecl;

// MatchMakingKeyValuePair_t
    SteamAPI_MatchMakingKeyValuePair_t_Construct:procedure(const aSelf:PMatchMakingKeyValuePair_t); cdecl;

// servernetadr_t
    SteamAPI_servernetadr_t_Construct:procedure(const aSelf:Pservernetadr_t); cdecl;
    SteamAPI_servernetadr_t_Init:procedure(const aSelf:Pservernetadr_t;const ip:TSteamUInt32;const usQueryPort:TSteamUInt16;const usConnectionPort:TSteamUInt16); cdecl;
    SteamAPI_servernetadr_t_GetQueryPort:function(const aSelf:Pservernetadr_t):TSteamUInt16; cdecl;
    SteamAPI_servernetadr_t_SetQueryPort:procedure(const aSelf:Pservernetadr_t;const usPort:TSteamUInt16); cdecl;
    SteamAPI_servernetadr_t_GetConnectionPort:function(const aSelf:Pservernetadr_t):TSteamUInt16; cdecl;
    SteamAPI_servernetadr_t_SetConnectionPort:procedure(const aSelf:Pservernetadr_t;const usPort:TSteamUInt16); cdecl;
    SteamAPI_servernetadr_t_GetIP:function(const aSelf:Pservernetadr_t):TSteamUInt32; cdecl;
    SteamAPI_servernetadr_t_SetIP:procedure(const aSelf:Pservernetadr_t;const unIP:TSteamUInt32); cdecl;
    SteamAPI_servernetadr_t_GetConnectionAddressString:function(const aSelf:Pservernetadr_t):PSteamChar; cdecl;
    SteamAPI_servernetadr_t_GetQueryAddressString:function(const aSelf:Pservernetadr_t):PSteamChar; cdecl;
    SteamAPI_servernetadr_t_IsLessThan:function(const aSelf:Pservernetadr_t;const netadr:Pservernetadr_t):TSteamBool; cdecl;
    SteamAPI_servernetadr_t_Assign:procedure(const aSelf:Pservernetadr_t;const that:Pservernetadr_t); cdecl;

// gameserveritem_t
    SteamAPI_gameserveritem_t_Construct:procedure(const aSelf:Pgameserveritem_t); cdecl;
    SteamAPI_gameserveritem_t_GetName:function(const aSelf:Pgameserveritem_t):PSteamChar; cdecl;
    SteamAPI_gameserveritem_t_SetName:procedure(const aSelf:Pgameserveritem_t;const pName:PSteamChar); cdecl;

// SteamNetworkingIPAddr
    SteamAPI_SteamNetworkingIPAddr_Clear:procedure(const aSelf:PSteamNetworkingIPAddr); cdecl;
    SteamAPI_SteamNetworkingIPAddr_IsIPv6AllZeros:function(const aSelf:PSteamNetworkingIPAddr):TSteamBool; cdecl;
    SteamAPI_SteamNetworkingIPAddr_SetIPv6:procedure(const aSelf:PSteamNetworkingIPAddr;const ipv6:PSteamUInt8;const nPort:TSteamUInt16); cdecl;
    SteamAPI_SteamNetworkingIPAddr_SetIPv4:procedure(const aSelf:PSteamNetworkingIPAddr;const nIP:TSteamUInt32;const nPort:TSteamUInt16); cdecl;
    SteamAPI_SteamNetworkingIPAddr_IsIPv4:function(const aSelf:PSteamNetworkingIPAddr):TSteamBool; cdecl;
    SteamAPI_SteamNetworkingIPAddr_GetIPv4:function(const aSelf:PSteamNetworkingIPAddr):TSteamUInt32; cdecl;
    SteamAPI_SteamNetworkingIPAddr_SetIPv6LocalHost:procedure(const aSelf:PSteamNetworkingIPAddr;const nPort:TSteamUInt16); cdecl;
    SteamAPI_SteamNetworkingIPAddr_IsLocalHost:function(const aSelf:PSteamNetworkingIPAddr):TSteamBool; cdecl;
    SteamAPI_SteamNetworkingIPAddr_ToString:procedure(const aSelf:PSteamNetworkingIPAddr;const buf:PSteamChar;const cbBuf:TSteamUInt32;const bWithPort:TSteamBool); cdecl;
    SteamAPI_SteamNetworkingIPAddr_ParseString:function(const aSelf:PSteamNetworkingIPAddr;const pszStr:PSteamChar):TSteamBool; cdecl;
    SteamAPI_SteamNetworkingIPAddr_IsEqualTo:function(const aSelf:PSteamNetworkingIPAddr;const x:PSteamNetworkingIPAddr):TSteamBool; cdecl;
    SteamAPI_SteamNetworkingIPAddr_GetFakeIPType:function(const aSelf:PSteamNetworkingIPAddr):TESteamNetworkingFakeIPType; cdecl;
    SteamAPI_SteamNetworkingIPAddr_IsFakeIP:function(const aSelf:PSteamNetworkingIPAddr):TSteamBool; cdecl;

// SteamNetworkingIdentity
    SteamAPI_SteamNetworkingIdentity_Clear:procedure(const aSelf:PSteamNetworkingIdentity); cdecl;
    SteamAPI_SteamNetworkingIdentity_IsInvalid:function(const aSelf:PSteamNetworkingIdentity):TSteamBool; cdecl;
    SteamAPI_SteamNetworkingIdentity_SetSteamID:procedure(const aSelf:PSteamNetworkingIdentity;const steamID:TSteamUInt64SteamID); cdecl;
    SteamAPI_SteamNetworkingIdentity_GetSteamID:function(const aSelf:PSteamNetworkingIdentity):TSteamUInt64SteamID; cdecl;
    SteamAPI_SteamNetworkingIdentity_SetSteamID64:procedure(const aSelf:PSteamNetworkingIdentity;const steamID:TSteamUInt64); cdecl;
    SteamAPI_SteamNetworkingIdentity_GetSteamID64:function(const aSelf:PSteamNetworkingIdentity):TSteamUInt64; cdecl;
    SteamAPI_SteamNetworkingIdentity_SetXboxPairwiseID:function(const aSelf:PSteamNetworkingIdentity;const pszString:PSteamChar):TSteamBool; cdecl;
    SteamAPI_SteamNetworkingIdentity_GetXboxPairwiseID:function(const aSelf:PSteamNetworkingIdentity):PSteamChar; cdecl;
    SteamAPI_SteamNetworkingIdentity_SetPSNID:procedure(const aSelf:PSteamNetworkingIdentity;const id:TSteamUInt64); cdecl;
    SteamAPI_SteamNetworkingIdentity_GetPSNID:function(const aSelf:PSteamNetworkingIdentity):TSteamUInt64; cdecl;
    SteamAPI_SteamNetworkingIdentity_SetIPAddr:procedure(const aSelf:PSteamNetworkingIdentity;const addr:PSteamNetworkingIPAddr); cdecl;
    SteamAPI_SteamNetworkingIdentity_GetIPAddr:function(const aSelf:PSteamNetworkingIdentity):PSteamNetworkingIPAddr; cdecl;
    SteamAPI_SteamNetworkingIdentity_SetIPv4Addr:procedure(const aSelf:PSteamNetworkingIdentity;const nIPv4:TSteamUInt32;const nPort:TSteamUInt16); cdecl;
    SteamAPI_SteamNetworkingIdentity_GetIPv4:function(const aSelf:PSteamNetworkingIdentity):TSteamUInt32; cdecl;
    SteamAPI_SteamNetworkingIdentity_GetFakeIPType:function(const aSelf:PSteamNetworkingIdentity):TESteamNetworkingFakeIPType; cdecl;
    SteamAPI_SteamNetworkingIdentity_IsFakeIP:function(const aSelf:PSteamNetworkingIdentity):TSteamBool; cdecl;
    SteamAPI_SteamNetworkingIdentity_SetLocalHost:procedure(const aSelf:PSteamNetworkingIdentity); cdecl;
    SteamAPI_SteamNetworkingIdentity_IsLocalHost:function(const aSelf:PSteamNetworkingIdentity):TSteamBool; cdecl;
    SteamAPI_SteamNetworkingIdentity_SetGenericString:function(const aSelf:PSteamNetworkingIdentity;const pszString:PSteamChar):TSteamBool; cdecl;
    SteamAPI_SteamNetworkingIdentity_GetGenericString:function(const aSelf:PSteamNetworkingIdentity):PSteamChar; cdecl;
    SteamAPI_SteamNetworkingIdentity_SetGenericBytes:function(const aSelf:PSteamNetworkingIdentity;const data:TSteamPointer;const cbLen:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_SteamNetworkingIdentity_GetGenericBytes:function(const aSelf:PSteamNetworkingIdentity;const cbLen:PSteamInt32):PSteamUInt8; cdecl;
    SteamAPI_SteamNetworkingIdentity_IsEqualTo:function(const aSelf:PSteamNetworkingIdentity;const x:PSteamNetworkingIdentity):TSteamBool; cdecl;
    SteamAPI_SteamNetworkingIdentity_ToString:procedure(const aSelf:PSteamNetworkingIdentity;const buf:PSteamChar;const cbBuf:TSteamUInt32); cdecl;
    SteamAPI_SteamNetworkingIdentity_ParseString:function(const aSelf:PSteamNetworkingIdentity;const pszStr:PSteamChar):TSteamBool; cdecl;

// SteamNetworkingMessage_t
    SteamAPI_SteamNetworkingMessage_t_Release:procedure(const aSelf:PSteamNetworkingMessage_t); cdecl;

// SteamNetworkingConfigValue_t
    SteamAPI_SteamNetworkingConfigValue_t_SetInt32:procedure(const aSelf:PSteamNetworkingConfigValue_t;const eVal:TESteamNetworkingConfigValue;const data:TSteamInt32); cdecl;
    SteamAPI_SteamNetworkingConfigValue_t_SetInt64:procedure(const aSelf:PSteamNetworkingConfigValue_t;const eVal:TESteamNetworkingConfigValue;const data:TSteamInt64); cdecl;
    SteamAPI_SteamNetworkingConfigValue_t_SetFloat:procedure(const aSelf:PSteamNetworkingConfigValue_t;const eVal:TESteamNetworkingConfigValue;const data:TSteamFloat); cdecl;
    SteamAPI_SteamNetworkingConfigValue_t_SetPtr:procedure(const aSelf:PSteamNetworkingConfigValue_t;const eVal:TESteamNetworkingConfigValue;const data:TSteamPointer); cdecl;
    SteamAPI_SteamNetworkingConfigValue_t_SetString:procedure(const aSelf:PSteamNetworkingConfigValue_t;const eVal:TESteamNetworkingConfigValue;const data:PSteamChar); cdecl;

// SteamDatagramHostedAddress
    SteamAPI_SteamDatagramHostedAddress_Clear:procedure(const aSelf:PSteamDatagramHostedAddress); cdecl;
    SteamAPI_SteamDatagramHostedAddress_GetPopID:function(const aSelf:PSteamDatagramHostedAddress):TSteamNetworkingPOPID; cdecl;
    SteamAPI_SteamDatagramHostedAddress_SetDevAddress:procedure(const aSelf:PSteamDatagramHostedAddress;const nIP:TSteamUInt32;const nPort:TSteamUInt16;const popid:TSteamNetworkingPOPID); cdecl;

// Free standing entry points
    SteamInternal_SteamAPI_Init:function(const pszInternalCheckInterfaceVersions:PSteamChar;const pOutErrMsg:PSteamErrMsg):TESteamAPIInitResult; cdecl;
    SteamAPI_InitFlat:function(const pOutErrMsg:PSteamErrMsg):TESteamAPIInitResult; cdecl;
    SteamAPI_InitSafe:function:TSteamBool; cdecl;
    SteamAPI_Shutdown:procedure; cdecl;
    SteamAPI_RestartAppIfNecessary:function(const unOwnAppID:TSteamUInt32):TSteamBool; cdecl;
    SteamAPI_IsSteamRunning:function:TSteamBool; cdecl;
    SteamAPI_GetSteamInstallPath:function:PSteamChar; cdecl;
    SteamAPI_ReleaseCurrentThreadMemory:procedure; cdecl;
    SteamAPI_GetHSteamPipe:function:THSteamPipe; cdecl;
    SteamAPI_GetHSteamUser:function:THSteamUser; cdecl;
    SteamAPI_RunCallbacks:procedure; cdecl;
    SteamAPI_SetTryCatchCallbacks:procedure(const bTryCatchCallbacks:TSteamBool); cdecl;
    SteamAPI_ManualDispatch_Init:procedure; cdecl;
    SteamAPI_ManualDispatch_RunFrame:procedure(const hSteamPipe:THSteamPipe); cdecl;
    SteamAPI_ManualDispatch_GetNextCallback:function(const hSteamPipe:THSteamPipe;const pCallbackMsg:PCallbackMsg_t):TSteamBool; cdecl;
    SteamAPI_ManualDispatch_FreeLastCallback:procedure(const hSteamPipe:THSteamPipe); cdecl;
    SteamAPI_ManualDispatch_GetAPICallResult:function(const hSteamPipe:THSteamPipe;const hSteamAPICall:TSteamAPICall_t;const pCallback:TSteamPointer;const cubCallback:TSteamInt32;const iCallbackExpected:TSteamInt32;const pbFailed:PSteamBool):TSteamBool; cdecl;
    SteamAPI_RegisterCallback:procedure(const pCallback:TSteamPointer;const iCallback:TSteamInt32); cdecl;
    SteamAPI_UnregisterCallback:procedure(const pCallback:TSteamPointer); cdecl;
    SteamAPI_RegisterCallResult:procedure(const pCallback:TSteamPointer;const hAPICall:TSteamAPICall_t); cdecl;
    SteamAPI_UnregisterCallResult:procedure(const pCallback:TSteamPointer;const hAPICall:TSteamAPICall_t); cdecl;
    SteamClient:function:PISteamClient; cdecl;
    SteamInternal_ContextInit:function(const pContextInitData:TSteamPointer):TSteamPointer; cdecl;
    SteamInternal_CreateInterface:function(const ver:PSteamChar):TSteamPointer; cdecl;
    SteamInternal_FindOrCreateUserInterface:function(const hSteamUser:THSteamUser;const pszVersion:PSteamChar):TSteamPointer; cdecl;
    SteamInternal_FindOrCreateGameServerInterface:function(const hSteamUser:THSteamUser;const pszVersion:PSteamChar):TSteamPointer; cdecl;
    SteamInternal_GameServer_Init_V2:function(const unIP:TSteamUInt32;const usGamePort:TSteamUInt16;const usQueryPort:TSteamUInt16;const eServerMode:TEServerMode;const pchVersionString:PSteamChar;const pszInternalCheckInterfaceVersions:PSteamChar;const pOutErrMsg:PSteamErrMsg):TESteamAPIInitResult; cdecl;
    SteamGameServer_Shutdown:procedure; cdecl;
    SteamGameServer_RunCallbacks:procedure; cdecl;
    SteamGameServer_BSecure:function:TSteamBool; cdecl;
    SteamGameServer_GetSteamID:function:TSteamUInt64SteamID; cdecl;
    SteamGameServer_GetHSteamPipe:function:THSteamPipe; cdecl;
    SteamGameServer_GetHSteamUser:function:THSteamUser; cdecl;
    SteamAPI_SetBreakpadAppID:procedure(const unAppID:TSteamUInt32); cdecl;
    SteamAPI_SetMiniDumpComment:procedure(const pchMsg:PSteamChar); cdecl;
    SteamAPI_UseBreakpadCrashHandler:procedure(const pchVersion:PSteamChar;const pchDate:PSteamChar;const pchTime:PSteamChar;const bFullMemoryDumps:TSteamBool;const pvContext:TSteamPointer;const m_pfnPreMinidumpCallback:TPFNPreMinidumpCallback); cdecl;
    SteamAPI_WriteMiniDump:procedure(const uStructuredExceptionCode:TSteamUInt32;const pvExceptionInfo:TSteamPointer;const uBuildID:TSteamUInt32); cdecl;

// Free standing entry points of the encrypted app ticket library
    SteamEncryptedAppTicket_BDecryptTicket:function(const rgubTicketEncrypted:PSteamUInt8;const cubTicketEncrypted:TSteamUInt32;const rgubTicketDecrypted:PSteamUInt8;const pcubTicketDecrypted:PSteamUInt32;const rgubKey:PSteamUInt8;const cubKey:TSteamInt32):TSteamBool; cdecl;
    SteamEncryptedAppTicket_BIsTicketForApp:function(const rgubTicketDecrypted:PSteamUInt8;const cubTicketDecrypted:TSteamUInt32;const nAppID:TAppId_t):TSteamBool; cdecl;
    SteamEncryptedAppTicket_GetTicketIssueTime:function(const rgubTicketDecrypted:PSteamUInt8;const cubTicketDecrypted:TSteamUInt32):TRTime32; cdecl;
    SteamEncryptedAppTicket_GetTicketSteamID:procedure(const rgubTicketDecrypted:PSteamUInt8;const cubTicketDecrypted:TSteamUInt32;const psteamID:PCSteamID); cdecl;
    SteamEncryptedAppTicket_GetTicketAppID:function(const rgubTicketDecrypted:PSteamUInt8;const cubTicketDecrypted:TSteamUInt32):TAppId_t; cdecl;
    SteamEncryptedAppTicket_BUserOwnsAppInTicket:function(const rgubTicketDecrypted:PSteamUInt8;const cubTicketDecrypted:TSteamUInt32;const nAppID:TAppId_t):TSteamBool; cdecl;
    SteamEncryptedAppTicket_BUserIsVacBanned:function(const rgubTicketDecrypted:PSteamUInt8;const cubTicketDecrypted:TSteamUInt32):TSteamBool; cdecl;
    SteamEncryptedAppTicket_BGetAppDefinedValue:function(const rgubTicketDecrypted:PSteamUInt8;const cubTicketDecrypted:TSteamUInt32;const pValue:PSteamUInt32):TSteamBool; cdecl;
    SteamEncryptedAppTicket_GetUserVariableData:function(const rgubTicketDecrypted:PSteamUInt8;const cubTicketDecrypted:TSteamUInt32;const pcubUserData:PSteamUInt32):PSteamUInt8; cdecl;
    SteamEncryptedAppTicket_BIsTicketSigned:function(const rgubTicketDecrypted:PSteamUInt8;const cubTicketDecrypted:TSteamUInt32;const pubRSAKey:PSteamUInt8;const cubRSAKey:TSteamUInt32):TSteamBool; cdecl;
    SteamEncryptedAppTicket_BIsLicenseBorrowed:function(const rgubTicketDecrypted:PSteamUInt8;const cubTicketDecrypted:TSteamUInt32):TSteamBool; cdecl;
    SteamEncryptedAppTicket_BIsLicenseTemporary:function(const rgubTicketDecrypted:PSteamUInt8;const cubTicketDecrypted:TSteamUInt32):TSteamBool; cdecl;

function SteamIDToUInt64(const aSteamID:TCSteamID):TSteamUInt64;
function UInt64ToSteamID(const aValue:TSteamUInt64):TCSteamID;
function GameIDToUInt64(const aGameID:TCGameID):TSteamUInt64;
function UInt64ToGameID(const aValue:TSteamUInt64):TCGameID;

function SteamAPI_InitEx(const aOutErrorMessage:PSteamErrMsg):TESteamAPIInitResult;
function SteamAPI_Init:boolean;
function SteamGameServer_InitEx(const aIP:TSteamUInt32;const aGamePort:TSteamUInt16;const aQueryPort:TSteamUInt16;const aServerMode:TEServerMode;const aVersionString:PSteamChar;const aOutErrorMessage:PSteamErrMsg):TESteamAPIInitResult;
function SteamGameServer_Init(const aIP:TSteamUInt32;const aGamePort:TSteamUInt16;const aQueryPort:TSteamUInt16;const aServerMode:TEServerMode;const aVersionString:PSteamChar):boolean;
procedure SteamGameServer_ReleaseCurrentThreadMemory;
function SteamGameServerClient:PISteamClient;

type PPSteamworksCallbackHandler=^PSteamworksCallbackHandler;
     PSteamworksCallbackHandler=^TSteamworksCallbackHandler;
     TSteamworksCallbackHandler=procedure(const aCallbackMessage:PCallbackMsg_t);

procedure SteamworksManualDispatchRunFrame(const aSteamPipe:THSteamPipe;const aCallbackHandler:TSteamworksCallbackHandler);

var SteamworksLibraryHandle:TSteamPointer=nil;
    SteamworksEncryptedAppTicketLibraryHandle:TSteamPointer=nil;

function SteamworksLoadLibrary(const aLibraryName:string):TSteamPointer;
function SteamworksFreeLibrary(const aLibraryHandle:TSteamPointer):boolean;
function SteamworksGetProcAddress(const aLibraryHandle:TSteamPointer;const aProcName:string):TSteamPointer;

function LoadSteamworksLibrary(const aLibraryName:string=STEAMWORKS_DEFAULT_LIB_NAME):boolean;
procedure UnloadSteamworksLibrary;

function LoadSteamworksEncryptedAppTicketLibrary(const aLibraryName:string=STEAMWORKS_ENCRYPTED_APP_TICKET_DEFAULT_LIB_NAME):boolean;
procedure UnloadSteamworksEncryptedAppTicketLibrary;

implementation

function SteamAPI_InitEx(const aOutErrorMessage:PSteamErrMsg):TESteamAPIInitResult;
var InterfaceVersions:TSteamAnsiString;
begin

 InterfaceVersions:=ISteamUtils_INTERFACE_VERSION+#0+
                    ISteamNetworkingUtils_INTERFACE_VERSION+#0+
                    ISteamApps_INTERFACE_VERSION+#0+
                    ISteamController_INTERFACE_VERSION+#0+
                    ISteamFriends_INTERFACE_VERSION+#0+
                    ISteamHTMLSurface_INTERFACE_VERSION+#0+
                    ISteamHTTP_INTERFACE_VERSION+#0+
                    ISteamInput_INTERFACE_VERSION+#0+
                    ISteamInventory_INTERFACE_VERSION+#0+
                    ISteamMatchmakingServers_INTERFACE_VERSION+#0+
                    ISteamMatchmaking_INTERFACE_VERSION+#0+
                    ISteamMusic_INTERFACE_VERSION+#0+
                    ISteamNetworkingMessages_INTERFACE_VERSION+#0+
                    ISteamNetworkingSockets_INTERFACE_VERSION+#0+
                    ISteamNetworking_INTERFACE_VERSION+#0+
                    ISteamParentalSettings_INTERFACE_VERSION+#0+
                    ISteamParties_INTERFACE_VERSION+#0+
                    ISteamRemotePlay_INTERFACE_VERSION+#0+
                    ISteamRemoteStorage_INTERFACE_VERSION+#0+
                    ISteamScreenshots_INTERFACE_VERSION+#0+
                    ISteamUGC_INTERFACE_VERSION+#0+
                    ISteamUserStats_INTERFACE_VERSION+#0+
                    ISteamUser_INTERFACE_VERSION+#0+
                    ISteamVideo_INTERFACE_VERSION+#0+
                    #0;

 result:=SteamInternal_SteamAPI_Init(PSteamChar(InterfaceVersions),aOutErrorMessage);

end;

function SteamAPI_Init:boolean;
begin
 result:=SteamAPI_InitEx(nil)=k_ESteamAPIInitResult_OK;
end;

function SteamGameServer_InitEx(const aIP:TSteamUInt32;const aGamePort:TSteamUInt16;const aQueryPort:TSteamUInt16;const aServerMode:TEServerMode;const aVersionString:PSteamChar;const aOutErrorMessage:PSteamErrMsg):TESteamAPIInitResult;
var InterfaceVersions:TSteamAnsiString;
begin

 InterfaceVersions:=ISteamUtils_INTERFACE_VERSION+#0+
                    ISteamNetworkingUtils_INTERFACE_VERSION+#0+
                    ISteamGameServer_INTERFACE_VERSION+#0+
                    ISteamGameServerStats_INTERFACE_VERSION+#0+
                    ISteamHTTP_INTERFACE_VERSION+#0+
                    ISteamInventory_INTERFACE_VERSION+#0+
                    ISteamNetworking_INTERFACE_VERSION+#0+
                    ISteamNetworkingMessages_INTERFACE_VERSION+#0+
                    ISteamNetworkingSockets_INTERFACE_VERSION+#0+
                    ISteamUGC_INTERFACE_VERSION+#0+
                    #0;

 result:=SteamInternal_GameServer_Init_V2(aIP,aGamePort,aQueryPort,aServerMode,aVersionString,PSteamChar(InterfaceVersions),aOutErrorMessage);

end;

function SteamGameServer_Init(const aIP:TSteamUInt32;const aGamePort:TSteamUInt16;const aQueryPort:TSteamUInt16;const aServerMode:TEServerMode;const aVersionString:PSteamChar):boolean;
begin
 result:=SteamGameServer_InitEx(aIP,aGamePort,aQueryPort,aServerMode,aVersionString,nil)=k_ESteamAPIInitResult_OK;
end;

procedure SteamGameServer_ReleaseCurrentThreadMemory;
begin
 SteamAPI_ReleaseCurrentThreadMemory;
end;

function SteamGameServerClient:PISteamClient;
begin
 result:=SteamClient;
end;

procedure SteamworksManualDispatchRunFrame(const aSteamPipe:THSteamPipe;const aCallbackHandler:TSteamworksCallbackHandler);
var CallbackMessage:TCallbackMsg_t;
begin

 SteamAPI_ManualDispatch_RunFrame(aSteamPipe);

 while SteamAPI_ManualDispatch_GetNextCallback(aSteamPipe,@CallbackMessage) do begin
  try
   if assigned(aCallbackHandler) then begin
    aCallbackHandler(@CallbackMessage);
   end;
  finally
   SteamAPI_ManualDispatch_FreeLastCallback(aSteamPipe);
  end;
 end;

end;

function SteamIDToUInt64(const aSteamID:TCSteamID):TSteamUInt64;
begin
 Move(aSteamID.m_rgubSteamID[0],result,SizeOf(TSteamUInt64));
end;

function UInt64ToSteamID(const aValue:TSteamUInt64):TCSteamID;
begin
 Move(aValue,result.m_rgubSteamID[0],SizeOf(TSteamUInt64));
end;

function GameIDToUInt64(const aGameID:TCGameID):TSteamUInt64;
begin
 Move(aGameID.m_rgubGameID[0],result,SizeOf(TSteamUInt64));
end;

function UInt64ToGameID(const aValue:TSteamUInt64):TCGameID;
begin
 Move(aValue,result.m_rgubGameID[0],SizeOf(TSteamUInt64));
end;

function SteamworksLoadLibrary(const aLibraryName:string):TSteamPointer;
begin
{$ifdef Windows}
 result:={%H-}TSteamPointer(LoadLibrary(PChar(aLibraryName)));
{$else}
{$ifdef Unix}
 result:=dlopen(PChar(aLibraryName),RTLD_NOW or RTLD_GLOBAL);
{$else}
 result:=nil;
{$endif}
{$endif}
end;

function SteamworksFreeLibrary(const aLibraryHandle:TSteamPointer):boolean;
begin
 result:=assigned(aLibraryHandle);
 if result then begin
{$ifdef Windows}
  result:=FreeLibrary({%H-}HMODULE(aLibraryHandle));
{$else}
{$ifdef Unix}
  result:=dlclose(aLibraryHandle)=0;
{$else}
  result:=false;
{$endif}
{$endif}
 end;
end;

function SteamworksGetProcAddress(const aLibraryHandle:TSteamPointer;const aProcName:string):TSteamPointer;
begin
{$ifdef Windows}
 result:=GetProcAddress({%H-}HMODULE(aLibraryHandle),PChar(aProcName));
{$else}
{$ifdef Unix}
 result:=dlsym(aLibraryHandle,PChar(aProcName));
{$else}
 result:=nil;
{$endif}
{$endif}
end;

function LoadSteamworksLibrary(const aLibraryName:string=STEAMWORKS_DEFAULT_LIB_NAME):boolean;
var CountMissingEntryPoints:TSteamInt32;
 // The target is untyped so that one helper can fill in every differently typed entry point.
 procedure LoadEntryPoint(out aTarget;const aName:string);
 begin
  TSteamPointer(aTarget):=SteamworksGetProcAddress(SteamworksLibraryHandle,aName);
  if not assigned(TSteamPointer(aTarget)) then begin
   inc(CountMissingEntryPoints);
  end;
 end;
begin

 if assigned(SteamworksLibraryHandle) then begin
  result:=true;
  exit;
 end;

 SteamworksLibraryHandle:=SteamworksLoadLibrary(aLibraryName);
 if not assigned(SteamworksLibraryHandle) then begin
  result:=false;
  exit;
 end;

 CountMissingEntryPoints:=0;

 // ISteamClient
 LoadEntryPoint(SteamAPI_ISteamClient_CreateSteamPipe,'SteamAPI_ISteamClient_CreateSteamPipe');
 LoadEntryPoint(SteamAPI_ISteamClient_BReleaseSteamPipe,'SteamAPI_ISteamClient_BReleaseSteamPipe');
 LoadEntryPoint(SteamAPI_ISteamClient_ConnectToGlobalUser,'SteamAPI_ISteamClient_ConnectToGlobalUser');
 LoadEntryPoint(SteamAPI_ISteamClient_CreateLocalUser,'SteamAPI_ISteamClient_CreateLocalUser');
 LoadEntryPoint(SteamAPI_ISteamClient_ReleaseUser,'SteamAPI_ISteamClient_ReleaseUser');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamUser,'SteamAPI_ISteamClient_GetISteamUser');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamGameServer,'SteamAPI_ISteamClient_GetISteamGameServer');
 LoadEntryPoint(SteamAPI_ISteamClient_SetLocalIPBinding,'SteamAPI_ISteamClient_SetLocalIPBinding');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamFriends,'SteamAPI_ISteamClient_GetISteamFriends');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamUtils,'SteamAPI_ISteamClient_GetISteamUtils');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamMatchmaking,'SteamAPI_ISteamClient_GetISteamMatchmaking');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamMatchmakingServers,'SteamAPI_ISteamClient_GetISteamMatchmakingServers');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamGenericInterface,'SteamAPI_ISteamClient_GetISteamGenericInterface');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamUserStats,'SteamAPI_ISteamClient_GetISteamUserStats');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamGameServerStats,'SteamAPI_ISteamClient_GetISteamGameServerStats');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamApps,'SteamAPI_ISteamClient_GetISteamApps');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamNetworking,'SteamAPI_ISteamClient_GetISteamNetworking');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamRemoteStorage,'SteamAPI_ISteamClient_GetISteamRemoteStorage');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamScreenshots,'SteamAPI_ISteamClient_GetISteamScreenshots');
 LoadEntryPoint(SteamAPI_ISteamClient_GetIPCCallCount,'SteamAPI_ISteamClient_GetIPCCallCount');
 LoadEntryPoint(SteamAPI_ISteamClient_SetWarningMessageHook,'SteamAPI_ISteamClient_SetWarningMessageHook');
 LoadEntryPoint(SteamAPI_ISteamClient_BShutdownIfAllPipesClosed,'SteamAPI_ISteamClient_BShutdownIfAllPipesClosed');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamHTTP,'SteamAPI_ISteamClient_GetISteamHTTP');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamController,'SteamAPI_ISteamClient_GetISteamController');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamUGC,'SteamAPI_ISteamClient_GetISteamUGC');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamMusic,'SteamAPI_ISteamClient_GetISteamMusic');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamHTMLSurface,'SteamAPI_ISteamClient_GetISteamHTMLSurface');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamInventory,'SteamAPI_ISteamClient_GetISteamInventory');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamVideo,'SteamAPI_ISteamClient_GetISteamVideo');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamParentalSettings,'SteamAPI_ISteamClient_GetISteamParentalSettings');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamInput,'SteamAPI_ISteamClient_GetISteamInput');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamParties,'SteamAPI_ISteamClient_GetISteamParties');
 LoadEntryPoint(SteamAPI_ISteamClient_GetISteamRemotePlay,'SteamAPI_ISteamClient_GetISteamRemotePlay');

 // ISteamUser
 LoadEntryPoint(SteamAPI_SteamUser_v023,'SteamAPI_SteamUser_v023');
 LoadEntryPoint(SteamAPI_ISteamUser_GetHSteamUser,'SteamAPI_ISteamUser_GetHSteamUser');
 LoadEntryPoint(SteamAPI_ISteamUser_BLoggedOn,'SteamAPI_ISteamUser_BLoggedOn');
 LoadEntryPoint(SteamAPI_ISteamUser_GetSteamID,'SteamAPI_ISteamUser_GetSteamID');
 LoadEntryPoint(SteamAPI_ISteamUser_InitiateGameConnection_DEPRECATED,'SteamAPI_ISteamUser_InitiateGameConnection_DEPRECATED');
 LoadEntryPoint(SteamAPI_ISteamUser_TerminateGameConnection_DEPRECATED,'SteamAPI_ISteamUser_TerminateGameConnection_DEPRECATED');
 LoadEntryPoint(SteamAPI_ISteamUser_TrackAppUsageEvent,'SteamAPI_ISteamUser_TrackAppUsageEvent');
 LoadEntryPoint(SteamAPI_ISteamUser_GetUserDataFolder,'SteamAPI_ISteamUser_GetUserDataFolder');
 LoadEntryPoint(SteamAPI_ISteamUser_StartVoiceRecording,'SteamAPI_ISteamUser_StartVoiceRecording');
 LoadEntryPoint(SteamAPI_ISteamUser_StopVoiceRecording,'SteamAPI_ISteamUser_StopVoiceRecording');
 LoadEntryPoint(SteamAPI_ISteamUser_GetAvailableVoice,'SteamAPI_ISteamUser_GetAvailableVoice');
 LoadEntryPoint(SteamAPI_ISteamUser_GetVoice,'SteamAPI_ISteamUser_GetVoice');
 LoadEntryPoint(SteamAPI_ISteamUser_DecompressVoice,'SteamAPI_ISteamUser_DecompressVoice');
 LoadEntryPoint(SteamAPI_ISteamUser_GetVoiceOptimalSampleRate,'SteamAPI_ISteamUser_GetVoiceOptimalSampleRate');
 LoadEntryPoint(SteamAPI_ISteamUser_GetAuthSessionTicket,'SteamAPI_ISteamUser_GetAuthSessionTicket');
 LoadEntryPoint(SteamAPI_ISteamUser_GetAuthTicketForWebApi,'SteamAPI_ISteamUser_GetAuthTicketForWebApi');
 LoadEntryPoint(SteamAPI_ISteamUser_BeginAuthSession,'SteamAPI_ISteamUser_BeginAuthSession');
 LoadEntryPoint(SteamAPI_ISteamUser_EndAuthSession,'SteamAPI_ISteamUser_EndAuthSession');
 LoadEntryPoint(SteamAPI_ISteamUser_CancelAuthTicket,'SteamAPI_ISteamUser_CancelAuthTicket');
 LoadEntryPoint(SteamAPI_ISteamUser_UserHasLicenseForApp,'SteamAPI_ISteamUser_UserHasLicenseForApp');
 LoadEntryPoint(SteamAPI_ISteamUser_BIsBehindNAT,'SteamAPI_ISteamUser_BIsBehindNAT');
 LoadEntryPoint(SteamAPI_ISteamUser_AdvertiseGame,'SteamAPI_ISteamUser_AdvertiseGame');
 LoadEntryPoint(SteamAPI_ISteamUser_RequestEncryptedAppTicket,'SteamAPI_ISteamUser_RequestEncryptedAppTicket');
 LoadEntryPoint(SteamAPI_ISteamUser_GetEncryptedAppTicket,'SteamAPI_ISteamUser_GetEncryptedAppTicket');
 LoadEntryPoint(SteamAPI_ISteamUser_GetGameBadgeLevel,'SteamAPI_ISteamUser_GetGameBadgeLevel');
 LoadEntryPoint(SteamAPI_ISteamUser_GetPlayerSteamLevel,'SteamAPI_ISteamUser_GetPlayerSteamLevel');
 LoadEntryPoint(SteamAPI_ISteamUser_RequestStoreAuthURL,'SteamAPI_ISteamUser_RequestStoreAuthURL');
 LoadEntryPoint(SteamAPI_ISteamUser_BIsPhoneVerified,'SteamAPI_ISteamUser_BIsPhoneVerified');
 LoadEntryPoint(SteamAPI_ISteamUser_BIsTwoFactorEnabled,'SteamAPI_ISteamUser_BIsTwoFactorEnabled');
 LoadEntryPoint(SteamAPI_ISteamUser_BIsPhoneIdentifying,'SteamAPI_ISteamUser_BIsPhoneIdentifying');
 LoadEntryPoint(SteamAPI_ISteamUser_BIsPhoneRequiringVerification,'SteamAPI_ISteamUser_BIsPhoneRequiringVerification');
 LoadEntryPoint(SteamAPI_ISteamUser_GetMarketEligibility,'SteamAPI_ISteamUser_GetMarketEligibility');
 LoadEntryPoint(SteamAPI_ISteamUser_GetDurationControl,'SteamAPI_ISteamUser_GetDurationControl');
 LoadEntryPoint(SteamAPI_ISteamUser_BSetDurationControlOnlineState,'SteamAPI_ISteamUser_BSetDurationControlOnlineState');

 // ISteamFriends
 LoadEntryPoint(SteamAPI_SteamFriends_v018,'SteamAPI_SteamFriends_v018');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetPersonaName,'SteamAPI_ISteamFriends_GetPersonaName');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetPersonaState,'SteamAPI_ISteamFriends_GetPersonaState');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendCount,'SteamAPI_ISteamFriends_GetFriendCount');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendByIndex,'SteamAPI_ISteamFriends_GetFriendByIndex');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendRelationship,'SteamAPI_ISteamFriends_GetFriendRelationship');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendPersonaState,'SteamAPI_ISteamFriends_GetFriendPersonaState');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendPersonaName,'SteamAPI_ISteamFriends_GetFriendPersonaName');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendGamePlayed,'SteamAPI_ISteamFriends_GetFriendGamePlayed');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendPersonaNameHistory,'SteamAPI_ISteamFriends_GetFriendPersonaNameHistory');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendSteamLevel,'SteamAPI_ISteamFriends_GetFriendSteamLevel');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetPlayerNickname,'SteamAPI_ISteamFriends_GetPlayerNickname');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendsGroupCount,'SteamAPI_ISteamFriends_GetFriendsGroupCount');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendsGroupIDByIndex,'SteamAPI_ISteamFriends_GetFriendsGroupIDByIndex');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendsGroupName,'SteamAPI_ISteamFriends_GetFriendsGroupName');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendsGroupMembersCount,'SteamAPI_ISteamFriends_GetFriendsGroupMembersCount');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendsGroupMembersList,'SteamAPI_ISteamFriends_GetFriendsGroupMembersList');
 LoadEntryPoint(SteamAPI_ISteamFriends_HasFriend,'SteamAPI_ISteamFriends_HasFriend');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetClanCount,'SteamAPI_ISteamFriends_GetClanCount');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetClanByIndex,'SteamAPI_ISteamFriends_GetClanByIndex');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetClanName,'SteamAPI_ISteamFriends_GetClanName');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetClanTag,'SteamAPI_ISteamFriends_GetClanTag');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetClanActivityCounts,'SteamAPI_ISteamFriends_GetClanActivityCounts');
 LoadEntryPoint(SteamAPI_ISteamFriends_DownloadClanActivityCounts,'SteamAPI_ISteamFriends_DownloadClanActivityCounts');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendCountFromSource,'SteamAPI_ISteamFriends_GetFriendCountFromSource');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendFromSourceByIndex,'SteamAPI_ISteamFriends_GetFriendFromSourceByIndex');
 LoadEntryPoint(SteamAPI_ISteamFriends_IsUserInSource,'SteamAPI_ISteamFriends_IsUserInSource');
 LoadEntryPoint(SteamAPI_ISteamFriends_SetInGameVoiceSpeaking,'SteamAPI_ISteamFriends_SetInGameVoiceSpeaking');
 LoadEntryPoint(SteamAPI_ISteamFriends_ActivateGameOverlay,'SteamAPI_ISteamFriends_ActivateGameOverlay');
 LoadEntryPoint(SteamAPI_ISteamFriends_ActivateGameOverlayToUser,'SteamAPI_ISteamFriends_ActivateGameOverlayToUser');
 LoadEntryPoint(SteamAPI_ISteamFriends_ActivateGameOverlayToWebPage,'SteamAPI_ISteamFriends_ActivateGameOverlayToWebPage');
 LoadEntryPoint(SteamAPI_ISteamFriends_ActivateGameOverlayToStore,'SteamAPI_ISteamFriends_ActivateGameOverlayToStore');
 LoadEntryPoint(SteamAPI_ISteamFriends_SetPlayedWith,'SteamAPI_ISteamFriends_SetPlayedWith');
 LoadEntryPoint(SteamAPI_ISteamFriends_ActivateGameOverlayInviteDialog,'SteamAPI_ISteamFriends_ActivateGameOverlayInviteDialog');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetSmallFriendAvatar,'SteamAPI_ISteamFriends_GetSmallFriendAvatar');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetMediumFriendAvatar,'SteamAPI_ISteamFriends_GetMediumFriendAvatar');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetLargeFriendAvatar,'SteamAPI_ISteamFriends_GetLargeFriendAvatar');
 LoadEntryPoint(SteamAPI_ISteamFriends_RequestUserInformation,'SteamAPI_ISteamFriends_RequestUserInformation');
 LoadEntryPoint(SteamAPI_ISteamFriends_RequestClanOfficerList,'SteamAPI_ISteamFriends_RequestClanOfficerList');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetClanOwner,'SteamAPI_ISteamFriends_GetClanOwner');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetClanOfficerCount,'SteamAPI_ISteamFriends_GetClanOfficerCount');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetClanOfficerByIndex,'SteamAPI_ISteamFriends_GetClanOfficerByIndex');
 LoadEntryPoint(SteamAPI_ISteamFriends_SetRichPresence,'SteamAPI_ISteamFriends_SetRichPresence');
 LoadEntryPoint(SteamAPI_ISteamFriends_ClearRichPresence,'SteamAPI_ISteamFriends_ClearRichPresence');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendRichPresence,'SteamAPI_ISteamFriends_GetFriendRichPresence');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendRichPresenceKeyCount,'SteamAPI_ISteamFriends_GetFriendRichPresenceKeyCount');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendRichPresenceKeyByIndex,'SteamAPI_ISteamFriends_GetFriendRichPresenceKeyByIndex');
 LoadEntryPoint(SteamAPI_ISteamFriends_RequestFriendRichPresence,'SteamAPI_ISteamFriends_RequestFriendRichPresence');
 LoadEntryPoint(SteamAPI_ISteamFriends_InviteUserToGame,'SteamAPI_ISteamFriends_InviteUserToGame');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetCoplayFriendCount,'SteamAPI_ISteamFriends_GetCoplayFriendCount');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetCoplayFriend,'SteamAPI_ISteamFriends_GetCoplayFriend');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendCoplayTime,'SteamAPI_ISteamFriends_GetFriendCoplayTime');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendCoplayGame,'SteamAPI_ISteamFriends_GetFriendCoplayGame');
 LoadEntryPoint(SteamAPI_ISteamFriends_JoinClanChatRoom,'SteamAPI_ISteamFriends_JoinClanChatRoom');
 LoadEntryPoint(SteamAPI_ISteamFriends_LeaveClanChatRoom,'SteamAPI_ISteamFriends_LeaveClanChatRoom');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetClanChatMemberCount,'SteamAPI_ISteamFriends_GetClanChatMemberCount');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetChatMemberByIndex,'SteamAPI_ISteamFriends_GetChatMemberByIndex');
 LoadEntryPoint(SteamAPI_ISteamFriends_SendClanChatMessage,'SteamAPI_ISteamFriends_SendClanChatMessage');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetClanChatMessage,'SteamAPI_ISteamFriends_GetClanChatMessage');
 LoadEntryPoint(SteamAPI_ISteamFriends_IsClanChatAdmin,'SteamAPI_ISteamFriends_IsClanChatAdmin');
 LoadEntryPoint(SteamAPI_ISteamFriends_IsClanChatWindowOpenInSteam,'SteamAPI_ISteamFriends_IsClanChatWindowOpenInSteam');
 LoadEntryPoint(SteamAPI_ISteamFriends_OpenClanChatWindowInSteam,'SteamAPI_ISteamFriends_OpenClanChatWindowInSteam');
 LoadEntryPoint(SteamAPI_ISteamFriends_CloseClanChatWindowInSteam,'SteamAPI_ISteamFriends_CloseClanChatWindowInSteam');
 LoadEntryPoint(SteamAPI_ISteamFriends_SetListenForFriendsMessages,'SteamAPI_ISteamFriends_SetListenForFriendsMessages');
 LoadEntryPoint(SteamAPI_ISteamFriends_ReplyToFriendMessage,'SteamAPI_ISteamFriends_ReplyToFriendMessage');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFriendMessage,'SteamAPI_ISteamFriends_GetFriendMessage');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetFollowerCount,'SteamAPI_ISteamFriends_GetFollowerCount');
 LoadEntryPoint(SteamAPI_ISteamFriends_IsFollowing,'SteamAPI_ISteamFriends_IsFollowing');
 LoadEntryPoint(SteamAPI_ISteamFriends_EnumerateFollowingList,'SteamAPI_ISteamFriends_EnumerateFollowingList');
 LoadEntryPoint(SteamAPI_ISteamFriends_IsClanPublic,'SteamAPI_ISteamFriends_IsClanPublic');
 LoadEntryPoint(SteamAPI_ISteamFriends_IsClanOfficialGameGroup,'SteamAPI_ISteamFriends_IsClanOfficialGameGroup');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetNumChatsWithUnreadPriorityMessages,'SteamAPI_ISteamFriends_GetNumChatsWithUnreadPriorityMessages');
 LoadEntryPoint(SteamAPI_ISteamFriends_ActivateGameOverlayRemotePlayTogetherInviteDialog,'SteamAPI_ISteamFriends_ActivateGameOverlayRemotePlayTogetherInviteDialog');
 LoadEntryPoint(SteamAPI_ISteamFriends_RegisterProtocolInOverlayBrowser,'SteamAPI_ISteamFriends_RegisterProtocolInOverlayBrowser');
 LoadEntryPoint(SteamAPI_ISteamFriends_ActivateGameOverlayInviteDialogConnectString,'SteamAPI_ISteamFriends_ActivateGameOverlayInviteDialogConnectString');
 LoadEntryPoint(SteamAPI_ISteamFriends_RequestEquippedProfileItems,'SteamAPI_ISteamFriends_RequestEquippedProfileItems');
 LoadEntryPoint(SteamAPI_ISteamFriends_BHasEquippedProfileItem,'SteamAPI_ISteamFriends_BHasEquippedProfileItem');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetProfileItemPropertyString,'SteamAPI_ISteamFriends_GetProfileItemPropertyString');
 LoadEntryPoint(SteamAPI_ISteamFriends_GetProfileItemPropertyUint,'SteamAPI_ISteamFriends_GetProfileItemPropertyUint');

 // ISteamUtils
 LoadEntryPoint(SteamAPI_SteamUtils_v010,'SteamAPI_SteamUtils_v010');
 LoadEntryPoint(SteamAPI_SteamGameServerUtils_v010,'SteamAPI_SteamGameServerUtils_v010');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetSecondsSinceAppActive,'SteamAPI_ISteamUtils_GetSecondsSinceAppActive');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetSecondsSinceComputerActive,'SteamAPI_ISteamUtils_GetSecondsSinceComputerActive');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetConnectedUniverse,'SteamAPI_ISteamUtils_GetConnectedUniverse');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetServerRealTime,'SteamAPI_ISteamUtils_GetServerRealTime');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetIPCountry,'SteamAPI_ISteamUtils_GetIPCountry');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetImageSize,'SteamAPI_ISteamUtils_GetImageSize');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetImageRGBA,'SteamAPI_ISteamUtils_GetImageRGBA');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetCurrentBatteryPower,'SteamAPI_ISteamUtils_GetCurrentBatteryPower');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetAppID,'SteamAPI_ISteamUtils_GetAppID');
 LoadEntryPoint(SteamAPI_ISteamUtils_SetOverlayNotificationPosition,'SteamAPI_ISteamUtils_SetOverlayNotificationPosition');
 LoadEntryPoint(SteamAPI_ISteamUtils_IsAPICallCompleted,'SteamAPI_ISteamUtils_IsAPICallCompleted');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetAPICallFailureReason,'SteamAPI_ISteamUtils_GetAPICallFailureReason');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetAPICallResult,'SteamAPI_ISteamUtils_GetAPICallResult');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetIPCCallCount,'SteamAPI_ISteamUtils_GetIPCCallCount');
 LoadEntryPoint(SteamAPI_ISteamUtils_SetWarningMessageHook,'SteamAPI_ISteamUtils_SetWarningMessageHook');
 LoadEntryPoint(SteamAPI_ISteamUtils_IsOverlayEnabled,'SteamAPI_ISteamUtils_IsOverlayEnabled');
 LoadEntryPoint(SteamAPI_ISteamUtils_BOverlayNeedsPresent,'SteamAPI_ISteamUtils_BOverlayNeedsPresent');
 LoadEntryPoint(SteamAPI_ISteamUtils_CheckFileSignature,'SteamAPI_ISteamUtils_CheckFileSignature');
 LoadEntryPoint(SteamAPI_ISteamUtils_ShowGamepadTextInput,'SteamAPI_ISteamUtils_ShowGamepadTextInput');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetEnteredGamepadTextLength,'SteamAPI_ISteamUtils_GetEnteredGamepadTextLength');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetEnteredGamepadTextInput,'SteamAPI_ISteamUtils_GetEnteredGamepadTextInput');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetSteamUILanguage,'SteamAPI_ISteamUtils_GetSteamUILanguage');
 LoadEntryPoint(SteamAPI_ISteamUtils_IsSteamRunningInVR,'SteamAPI_ISteamUtils_IsSteamRunningInVR');
 LoadEntryPoint(SteamAPI_ISteamUtils_SetOverlayNotificationInset,'SteamAPI_ISteamUtils_SetOverlayNotificationInset');
 LoadEntryPoint(SteamAPI_ISteamUtils_IsSteamInBigPictureMode,'SteamAPI_ISteamUtils_IsSteamInBigPictureMode');
 LoadEntryPoint(SteamAPI_ISteamUtils_StartVRDashboard,'SteamAPI_ISteamUtils_StartVRDashboard');
 LoadEntryPoint(SteamAPI_ISteamUtils_IsVRHeadsetStreamingEnabled,'SteamAPI_ISteamUtils_IsVRHeadsetStreamingEnabled');
 LoadEntryPoint(SteamAPI_ISteamUtils_SetVRHeadsetStreamingEnabled,'SteamAPI_ISteamUtils_SetVRHeadsetStreamingEnabled');
 LoadEntryPoint(SteamAPI_ISteamUtils_IsSteamChinaLauncher,'SteamAPI_ISteamUtils_IsSteamChinaLauncher');
 LoadEntryPoint(SteamAPI_ISteamUtils_InitFilterText,'SteamAPI_ISteamUtils_InitFilterText');
 LoadEntryPoint(SteamAPI_ISteamUtils_FilterText,'SteamAPI_ISteamUtils_FilterText');
 LoadEntryPoint(SteamAPI_ISteamUtils_GetIPv6ConnectivityState,'SteamAPI_ISteamUtils_GetIPv6ConnectivityState');
 LoadEntryPoint(SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck,'SteamAPI_ISteamUtils_IsSteamRunningOnSteamDeck');
 LoadEntryPoint(SteamAPI_ISteamUtils_ShowFloatingGamepadTextInput,'SteamAPI_ISteamUtils_ShowFloatingGamepadTextInput');
 LoadEntryPoint(SteamAPI_ISteamUtils_SetGameLauncherMode,'SteamAPI_ISteamUtils_SetGameLauncherMode');
 LoadEntryPoint(SteamAPI_ISteamUtils_DismissFloatingGamepadTextInput,'SteamAPI_ISteamUtils_DismissFloatingGamepadTextInput');
 LoadEntryPoint(SteamAPI_ISteamUtils_DismissGamepadTextInput,'SteamAPI_ISteamUtils_DismissGamepadTextInput');

 // ISteamMatchmaking
 LoadEntryPoint(SteamAPI_SteamMatchmaking_v009,'SteamAPI_SteamMatchmaking_v009');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_GetFavoriteGameCount,'SteamAPI_ISteamMatchmaking_GetFavoriteGameCount');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_GetFavoriteGame,'SteamAPI_ISteamMatchmaking_GetFavoriteGame');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_AddFavoriteGame,'SteamAPI_ISteamMatchmaking_AddFavoriteGame');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_RemoveFavoriteGame,'SteamAPI_ISteamMatchmaking_RemoveFavoriteGame');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_RequestLobbyList,'SteamAPI_ISteamMatchmaking_RequestLobbyList');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_AddRequestLobbyListStringFilter,'SteamAPI_ISteamMatchmaking_AddRequestLobbyListStringFilter');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_AddRequestLobbyListNumericalFilter,'SteamAPI_ISteamMatchmaking_AddRequestLobbyListNumericalFilter');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_AddRequestLobbyListNearValueFilter,'SteamAPI_ISteamMatchmaking_AddRequestLobbyListNearValueFilter');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_AddRequestLobbyListFilterSlotsAvailable,'SteamAPI_ISteamMatchmaking_AddRequestLobbyListFilterSlotsAvailable');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_AddRequestLobbyListDistanceFilter,'SteamAPI_ISteamMatchmaking_AddRequestLobbyListDistanceFilter');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_AddRequestLobbyListResultCountFilter,'SteamAPI_ISteamMatchmaking_AddRequestLobbyListResultCountFilter');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_AddRequestLobbyListCompatibleMembersFilter,'SteamAPI_ISteamMatchmaking_AddRequestLobbyListCompatibleMembersFilter');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_GetLobbyByIndex,'SteamAPI_ISteamMatchmaking_GetLobbyByIndex');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_CreateLobby,'SteamAPI_ISteamMatchmaking_CreateLobby');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_JoinLobby,'SteamAPI_ISteamMatchmaking_JoinLobby');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_LeaveLobby,'SteamAPI_ISteamMatchmaking_LeaveLobby');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_InviteUserToLobby,'SteamAPI_ISteamMatchmaking_InviteUserToLobby');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_GetNumLobbyMembers,'SteamAPI_ISteamMatchmaking_GetNumLobbyMembers');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_GetLobbyMemberByIndex,'SteamAPI_ISteamMatchmaking_GetLobbyMemberByIndex');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_GetLobbyData,'SteamAPI_ISteamMatchmaking_GetLobbyData');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_SetLobbyData,'SteamAPI_ISteamMatchmaking_SetLobbyData');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_GetLobbyDataCount,'SteamAPI_ISteamMatchmaking_GetLobbyDataCount');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_GetLobbyDataByIndex,'SteamAPI_ISteamMatchmaking_GetLobbyDataByIndex');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_DeleteLobbyData,'SteamAPI_ISteamMatchmaking_DeleteLobbyData');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_GetLobbyMemberData,'SteamAPI_ISteamMatchmaking_GetLobbyMemberData');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_SetLobbyMemberData,'SteamAPI_ISteamMatchmaking_SetLobbyMemberData');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_SendLobbyChatMsg,'SteamAPI_ISteamMatchmaking_SendLobbyChatMsg');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_GetLobbyChatEntry,'SteamAPI_ISteamMatchmaking_GetLobbyChatEntry');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_RequestLobbyData,'SteamAPI_ISteamMatchmaking_RequestLobbyData');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_SetLobbyGameServer,'SteamAPI_ISteamMatchmaking_SetLobbyGameServer');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_GetLobbyGameServer,'SteamAPI_ISteamMatchmaking_GetLobbyGameServer');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_SetLobbyMemberLimit,'SteamAPI_ISteamMatchmaking_SetLobbyMemberLimit');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_GetLobbyMemberLimit,'SteamAPI_ISteamMatchmaking_GetLobbyMemberLimit');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_SetLobbyType,'SteamAPI_ISteamMatchmaking_SetLobbyType');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_SetLobbyJoinable,'SteamAPI_ISteamMatchmaking_SetLobbyJoinable');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_GetLobbyOwner,'SteamAPI_ISteamMatchmaking_GetLobbyOwner');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_SetLobbyOwner,'SteamAPI_ISteamMatchmaking_SetLobbyOwner');
 LoadEntryPoint(SteamAPI_ISteamMatchmaking_SetLinkedLobby,'SteamAPI_ISteamMatchmaking_SetLinkedLobby');

 // ISteamMatchmakingServerListResponse
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServerListResponse_ServerResponded,'SteamAPI_ISteamMatchmakingServerListResponse_ServerResponded');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServerListResponse_ServerFailedToRespond,'SteamAPI_ISteamMatchmakingServerListResponse_ServerFailedToRespond');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServerListResponse_RefreshComplete,'SteamAPI_ISteamMatchmakingServerListResponse_RefreshComplete');

 // ISteamMatchmakingPingResponse
 LoadEntryPoint(SteamAPI_ISteamMatchmakingPingResponse_ServerResponded,'SteamAPI_ISteamMatchmakingPingResponse_ServerResponded');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingPingResponse_ServerFailedToRespond,'SteamAPI_ISteamMatchmakingPingResponse_ServerFailedToRespond');

 // ISteamMatchmakingPlayersResponse
 LoadEntryPoint(SteamAPI_ISteamMatchmakingPlayersResponse_AddPlayerToList,'SteamAPI_ISteamMatchmakingPlayersResponse_AddPlayerToList');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingPlayersResponse_PlayersFailedToRespond,'SteamAPI_ISteamMatchmakingPlayersResponse_PlayersFailedToRespond');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingPlayersResponse_PlayersRefreshComplete,'SteamAPI_ISteamMatchmakingPlayersResponse_PlayersRefreshComplete');

 // ISteamMatchmakingRulesResponse
 LoadEntryPoint(SteamAPI_ISteamMatchmakingRulesResponse_RulesResponded,'SteamAPI_ISteamMatchmakingRulesResponse_RulesResponded');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingRulesResponse_RulesFailedToRespond,'SteamAPI_ISteamMatchmakingRulesResponse_RulesFailedToRespond');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingRulesResponse_RulesRefreshComplete,'SteamAPI_ISteamMatchmakingRulesResponse_RulesRefreshComplete');

 // ISteamMatchmakingServers
 LoadEntryPoint(SteamAPI_SteamMatchmakingServers_v002,'SteamAPI_SteamMatchmakingServers_v002');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_RequestInternetServerList,'SteamAPI_ISteamMatchmakingServers_RequestInternetServerList');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_RequestLANServerList,'SteamAPI_ISteamMatchmakingServers_RequestLANServerList');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_RequestFriendsServerList,'SteamAPI_ISteamMatchmakingServers_RequestFriendsServerList');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_RequestFavoritesServerList,'SteamAPI_ISteamMatchmakingServers_RequestFavoritesServerList');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_RequestHistoryServerList,'SteamAPI_ISteamMatchmakingServers_RequestHistoryServerList');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_RequestSpectatorServerList,'SteamAPI_ISteamMatchmakingServers_RequestSpectatorServerList');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_ReleaseRequest,'SteamAPI_ISteamMatchmakingServers_ReleaseRequest');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_GetServerDetails,'SteamAPI_ISteamMatchmakingServers_GetServerDetails');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_CancelQuery,'SteamAPI_ISteamMatchmakingServers_CancelQuery');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_RefreshQuery,'SteamAPI_ISteamMatchmakingServers_RefreshQuery');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_IsRefreshing,'SteamAPI_ISteamMatchmakingServers_IsRefreshing');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_GetServerCount,'SteamAPI_ISteamMatchmakingServers_GetServerCount');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_RefreshServer,'SteamAPI_ISteamMatchmakingServers_RefreshServer');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_PingServer,'SteamAPI_ISteamMatchmakingServers_PingServer');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_PlayerDetails,'SteamAPI_ISteamMatchmakingServers_PlayerDetails');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_ServerRules,'SteamAPI_ISteamMatchmakingServers_ServerRules');
 LoadEntryPoint(SteamAPI_ISteamMatchmakingServers_CancelServerQuery,'SteamAPI_ISteamMatchmakingServers_CancelServerQuery');

 // ISteamParties
 LoadEntryPoint(SteamAPI_SteamParties_v002,'SteamAPI_SteamParties_v002');
 LoadEntryPoint(SteamAPI_ISteamParties_GetNumActiveBeacons,'SteamAPI_ISteamParties_GetNumActiveBeacons');
 LoadEntryPoint(SteamAPI_ISteamParties_GetBeaconByIndex,'SteamAPI_ISteamParties_GetBeaconByIndex');
 LoadEntryPoint(SteamAPI_ISteamParties_GetBeaconDetails,'SteamAPI_ISteamParties_GetBeaconDetails');
 LoadEntryPoint(SteamAPI_ISteamParties_JoinParty,'SteamAPI_ISteamParties_JoinParty');
 LoadEntryPoint(SteamAPI_ISteamParties_GetNumAvailableBeaconLocations,'SteamAPI_ISteamParties_GetNumAvailableBeaconLocations');
 LoadEntryPoint(SteamAPI_ISteamParties_GetAvailableBeaconLocations,'SteamAPI_ISteamParties_GetAvailableBeaconLocations');
 LoadEntryPoint(SteamAPI_ISteamParties_CreateBeacon,'SteamAPI_ISteamParties_CreateBeacon');
 LoadEntryPoint(SteamAPI_ISteamParties_OnReservationCompleted,'SteamAPI_ISteamParties_OnReservationCompleted');
 LoadEntryPoint(SteamAPI_ISteamParties_CancelReservation,'SteamAPI_ISteamParties_CancelReservation');
 LoadEntryPoint(SteamAPI_ISteamParties_ChangeNumOpenSlots,'SteamAPI_ISteamParties_ChangeNumOpenSlots');
 LoadEntryPoint(SteamAPI_ISteamParties_DestroyBeacon,'SteamAPI_ISteamParties_DestroyBeacon');
 LoadEntryPoint(SteamAPI_ISteamParties_GetBeaconLocationData,'SteamAPI_ISteamParties_GetBeaconLocationData');

 // ISteamRemoteStorage
 LoadEntryPoint(SteamAPI_SteamRemoteStorage_v016,'SteamAPI_SteamRemoteStorage_v016');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_FileWrite,'SteamAPI_ISteamRemoteStorage_FileWrite');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_FileRead,'SteamAPI_ISteamRemoteStorage_FileRead');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_FileWriteAsync,'SteamAPI_ISteamRemoteStorage_FileWriteAsync');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_FileReadAsync,'SteamAPI_ISteamRemoteStorage_FileReadAsync');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_FileReadAsyncComplete,'SteamAPI_ISteamRemoteStorage_FileReadAsyncComplete');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_FileForget,'SteamAPI_ISteamRemoteStorage_FileForget');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_FileDelete,'SteamAPI_ISteamRemoteStorage_FileDelete');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_FileShare,'SteamAPI_ISteamRemoteStorage_FileShare');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_SetSyncPlatforms,'SteamAPI_ISteamRemoteStorage_SetSyncPlatforms');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_FileWriteStreamOpen,'SteamAPI_ISteamRemoteStorage_FileWriteStreamOpen');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_FileWriteStreamWriteChunk,'SteamAPI_ISteamRemoteStorage_FileWriteStreamWriteChunk');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_FileWriteStreamClose,'SteamAPI_ISteamRemoteStorage_FileWriteStreamClose');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_FileWriteStreamCancel,'SteamAPI_ISteamRemoteStorage_FileWriteStreamCancel');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_FileExists,'SteamAPI_ISteamRemoteStorage_FileExists');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_FilePersisted,'SteamAPI_ISteamRemoteStorage_FilePersisted');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_GetFileSize,'SteamAPI_ISteamRemoteStorage_GetFileSize');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_GetFileTimestamp,'SteamAPI_ISteamRemoteStorage_GetFileTimestamp');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_GetSyncPlatforms,'SteamAPI_ISteamRemoteStorage_GetSyncPlatforms');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_GetFileCount,'SteamAPI_ISteamRemoteStorage_GetFileCount');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_GetFileNameAndSize,'SteamAPI_ISteamRemoteStorage_GetFileNameAndSize');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_GetQuota,'SteamAPI_ISteamRemoteStorage_GetQuota');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_IsCloudEnabledForAccount,'SteamAPI_ISteamRemoteStorage_IsCloudEnabledForAccount');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_IsCloudEnabledForApp,'SteamAPI_ISteamRemoteStorage_IsCloudEnabledForApp');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_SetCloudEnabledForApp,'SteamAPI_ISteamRemoteStorage_SetCloudEnabledForApp');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_UGCDownload,'SteamAPI_ISteamRemoteStorage_UGCDownload');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_GetUGCDownloadProgress,'SteamAPI_ISteamRemoteStorage_GetUGCDownloadProgress');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_GetUGCDetails,'SteamAPI_ISteamRemoteStorage_GetUGCDetails');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_UGCRead,'SteamAPI_ISteamRemoteStorage_UGCRead');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_GetCachedUGCCount,'SteamAPI_ISteamRemoteStorage_GetCachedUGCCount');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_GetCachedUGCHandle,'SteamAPI_ISteamRemoteStorage_GetCachedUGCHandle');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_PublishWorkshopFile,'SteamAPI_ISteamRemoteStorage_PublishWorkshopFile');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_CreatePublishedFileUpdateRequest,'SteamAPI_ISteamRemoteStorage_CreatePublishedFileUpdateRequest');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_UpdatePublishedFileFile,'SteamAPI_ISteamRemoteStorage_UpdatePublishedFileFile');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_UpdatePublishedFilePreviewFile,'SteamAPI_ISteamRemoteStorage_UpdatePublishedFilePreviewFile');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_UpdatePublishedFileTitle,'SteamAPI_ISteamRemoteStorage_UpdatePublishedFileTitle');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_UpdatePublishedFileDescription,'SteamAPI_ISteamRemoteStorage_UpdatePublishedFileDescription');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_UpdatePublishedFileVisibility,'SteamAPI_ISteamRemoteStorage_UpdatePublishedFileVisibility');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_UpdatePublishedFileTags,'SteamAPI_ISteamRemoteStorage_UpdatePublishedFileTags');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_CommitPublishedFileUpdate,'SteamAPI_ISteamRemoteStorage_CommitPublishedFileUpdate');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_GetPublishedFileDetails,'SteamAPI_ISteamRemoteStorage_GetPublishedFileDetails');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_DeletePublishedFile,'SteamAPI_ISteamRemoteStorage_DeletePublishedFile');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_EnumerateUserPublishedFiles,'SteamAPI_ISteamRemoteStorage_EnumerateUserPublishedFiles');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_SubscribePublishedFile,'SteamAPI_ISteamRemoteStorage_SubscribePublishedFile');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_EnumerateUserSubscribedFiles,'SteamAPI_ISteamRemoteStorage_EnumerateUserSubscribedFiles');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_UnsubscribePublishedFile,'SteamAPI_ISteamRemoteStorage_UnsubscribePublishedFile');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_UpdatePublishedFileSetChangeDescription,'SteamAPI_ISteamRemoteStorage_UpdatePublishedFileSetChangeDescription');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_GetPublishedItemVoteDetails,'SteamAPI_ISteamRemoteStorage_GetPublishedItemVoteDetails');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_UpdateUserPublishedItemVote,'SteamAPI_ISteamRemoteStorage_UpdateUserPublishedItemVote');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_GetUserPublishedItemVoteDetails,'SteamAPI_ISteamRemoteStorage_GetUserPublishedItemVoteDetails');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_EnumerateUserSharedWorkshopFiles,'SteamAPI_ISteamRemoteStorage_EnumerateUserSharedWorkshopFiles');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_PublishVideo,'SteamAPI_ISteamRemoteStorage_PublishVideo');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_SetUserPublishedFileAction,'SteamAPI_ISteamRemoteStorage_SetUserPublishedFileAction');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_EnumeratePublishedFilesByUserAction,'SteamAPI_ISteamRemoteStorage_EnumeratePublishedFilesByUserAction');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_EnumeratePublishedWorkshopFiles,'SteamAPI_ISteamRemoteStorage_EnumeratePublishedWorkshopFiles');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_UGCDownloadToLocation,'SteamAPI_ISteamRemoteStorage_UGCDownloadToLocation');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_GetLocalFileChangeCount,'SteamAPI_ISteamRemoteStorage_GetLocalFileChangeCount');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_GetLocalFileChange,'SteamAPI_ISteamRemoteStorage_GetLocalFileChange');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_BeginFileWriteBatch,'SteamAPI_ISteamRemoteStorage_BeginFileWriteBatch');
 LoadEntryPoint(SteamAPI_ISteamRemoteStorage_EndFileWriteBatch,'SteamAPI_ISteamRemoteStorage_EndFileWriteBatch');

 // ISteamUserStats
 LoadEntryPoint(SteamAPI_SteamUserStats_v013,'SteamAPI_SteamUserStats_v013');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetStatInt32,'SteamAPI_ISteamUserStats_GetStatInt32');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetStatFloat,'SteamAPI_ISteamUserStats_GetStatFloat');
 LoadEntryPoint(SteamAPI_ISteamUserStats_SetStatInt32,'SteamAPI_ISteamUserStats_SetStatInt32');
 LoadEntryPoint(SteamAPI_ISteamUserStats_SetStatFloat,'SteamAPI_ISteamUserStats_SetStatFloat');
 LoadEntryPoint(SteamAPI_ISteamUserStats_UpdateAvgRateStat,'SteamAPI_ISteamUserStats_UpdateAvgRateStat');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetAchievement,'SteamAPI_ISteamUserStats_GetAchievement');
 LoadEntryPoint(SteamAPI_ISteamUserStats_SetAchievement,'SteamAPI_ISteamUserStats_SetAchievement');
 LoadEntryPoint(SteamAPI_ISteamUserStats_ClearAchievement,'SteamAPI_ISteamUserStats_ClearAchievement');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetAchievementAndUnlockTime,'SteamAPI_ISteamUserStats_GetAchievementAndUnlockTime');
 LoadEntryPoint(SteamAPI_ISteamUserStats_StoreStats,'SteamAPI_ISteamUserStats_StoreStats');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetAchievementIcon,'SteamAPI_ISteamUserStats_GetAchievementIcon');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute,'SteamAPI_ISteamUserStats_GetAchievementDisplayAttribute');
 LoadEntryPoint(SteamAPI_ISteamUserStats_IndicateAchievementProgress,'SteamAPI_ISteamUserStats_IndicateAchievementProgress');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetNumAchievements,'SteamAPI_ISteamUserStats_GetNumAchievements');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetAchievementName,'SteamAPI_ISteamUserStats_GetAchievementName');
 LoadEntryPoint(SteamAPI_ISteamUserStats_RequestUserStats,'SteamAPI_ISteamUserStats_RequestUserStats');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetUserStatInt32,'SteamAPI_ISteamUserStats_GetUserStatInt32');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetUserStatFloat,'SteamAPI_ISteamUserStats_GetUserStatFloat');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetUserAchievement,'SteamAPI_ISteamUserStats_GetUserAchievement');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetUserAchievementAndUnlockTime,'SteamAPI_ISteamUserStats_GetUserAchievementAndUnlockTime');
 LoadEntryPoint(SteamAPI_ISteamUserStats_ResetAllStats,'SteamAPI_ISteamUserStats_ResetAllStats');
 LoadEntryPoint(SteamAPI_ISteamUserStats_FindOrCreateLeaderboard,'SteamAPI_ISteamUserStats_FindOrCreateLeaderboard');
 LoadEntryPoint(SteamAPI_ISteamUserStats_FindLeaderboard,'SteamAPI_ISteamUserStats_FindLeaderboard');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetLeaderboardName,'SteamAPI_ISteamUserStats_GetLeaderboardName');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetLeaderboardEntryCount,'SteamAPI_ISteamUserStats_GetLeaderboardEntryCount');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetLeaderboardSortMethod,'SteamAPI_ISteamUserStats_GetLeaderboardSortMethod');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetLeaderboardDisplayType,'SteamAPI_ISteamUserStats_GetLeaderboardDisplayType');
 LoadEntryPoint(SteamAPI_ISteamUserStats_DownloadLeaderboardEntries,'SteamAPI_ISteamUserStats_DownloadLeaderboardEntries');
 LoadEntryPoint(SteamAPI_ISteamUserStats_DownloadLeaderboardEntriesForUsers,'SteamAPI_ISteamUserStats_DownloadLeaderboardEntriesForUsers');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetDownloadedLeaderboardEntry,'SteamAPI_ISteamUserStats_GetDownloadedLeaderboardEntry');
 LoadEntryPoint(SteamAPI_ISteamUserStats_UploadLeaderboardScore,'SteamAPI_ISteamUserStats_UploadLeaderboardScore');
 LoadEntryPoint(SteamAPI_ISteamUserStats_AttachLeaderboardUGC,'SteamAPI_ISteamUserStats_AttachLeaderboardUGC');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetNumberOfCurrentPlayers,'SteamAPI_ISteamUserStats_GetNumberOfCurrentPlayers');
 LoadEntryPoint(SteamAPI_ISteamUserStats_RequestGlobalAchievementPercentages,'SteamAPI_ISteamUserStats_RequestGlobalAchievementPercentages');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetMostAchievedAchievementInfo,'SteamAPI_ISteamUserStats_GetMostAchievedAchievementInfo');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetNextMostAchievedAchievementInfo,'SteamAPI_ISteamUserStats_GetNextMostAchievedAchievementInfo');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetAchievementAchievedPercent,'SteamAPI_ISteamUserStats_GetAchievementAchievedPercent');
 LoadEntryPoint(SteamAPI_ISteamUserStats_RequestGlobalStats,'SteamAPI_ISteamUserStats_RequestGlobalStats');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetGlobalStatInt64,'SteamAPI_ISteamUserStats_GetGlobalStatInt64');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetGlobalStatDouble,'SteamAPI_ISteamUserStats_GetGlobalStatDouble');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetGlobalStatHistoryInt64,'SteamAPI_ISteamUserStats_GetGlobalStatHistoryInt64');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetGlobalStatHistoryDouble,'SteamAPI_ISteamUserStats_GetGlobalStatHistoryDouble');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetAchievementProgressLimitsInt32,'SteamAPI_ISteamUserStats_GetAchievementProgressLimitsInt32');
 LoadEntryPoint(SteamAPI_ISteamUserStats_GetAchievementProgressLimitsFloat,'SteamAPI_ISteamUserStats_GetAchievementProgressLimitsFloat');

 // ISteamApps
 LoadEntryPoint(SteamAPI_SteamApps_v009,'SteamAPI_SteamApps_v009');
 LoadEntryPoint(SteamAPI_ISteamApps_BIsSubscribed,'SteamAPI_ISteamApps_BIsSubscribed');
 LoadEntryPoint(SteamAPI_ISteamApps_BIsLowViolence,'SteamAPI_ISteamApps_BIsLowViolence');
 LoadEntryPoint(SteamAPI_ISteamApps_BIsCybercafe,'SteamAPI_ISteamApps_BIsCybercafe');
 LoadEntryPoint(SteamAPI_ISteamApps_BIsVACBanned,'SteamAPI_ISteamApps_BIsVACBanned');
 LoadEntryPoint(SteamAPI_ISteamApps_GetCurrentGameLanguage,'SteamAPI_ISteamApps_GetCurrentGameLanguage');
 LoadEntryPoint(SteamAPI_ISteamApps_GetAvailableGameLanguages,'SteamAPI_ISteamApps_GetAvailableGameLanguages');
 LoadEntryPoint(SteamAPI_ISteamApps_BIsSubscribedApp,'SteamAPI_ISteamApps_BIsSubscribedApp');
 LoadEntryPoint(SteamAPI_ISteamApps_BIsDlcInstalled,'SteamAPI_ISteamApps_BIsDlcInstalled');
 LoadEntryPoint(SteamAPI_ISteamApps_GetEarliestPurchaseUnixTime,'SteamAPI_ISteamApps_GetEarliestPurchaseUnixTime');
 LoadEntryPoint(SteamAPI_ISteamApps_BIsSubscribedFromFreeWeekend,'SteamAPI_ISteamApps_BIsSubscribedFromFreeWeekend');
 LoadEntryPoint(SteamAPI_ISteamApps_GetDLCCount,'SteamAPI_ISteamApps_GetDLCCount');
 LoadEntryPoint(SteamAPI_ISteamApps_BGetDLCDataByIndex,'SteamAPI_ISteamApps_BGetDLCDataByIndex');
 LoadEntryPoint(SteamAPI_ISteamApps_InstallDLC,'SteamAPI_ISteamApps_InstallDLC');
 LoadEntryPoint(SteamAPI_ISteamApps_UninstallDLC,'SteamAPI_ISteamApps_UninstallDLC');
 LoadEntryPoint(SteamAPI_ISteamApps_RequestAppProofOfPurchaseKey,'SteamAPI_ISteamApps_RequestAppProofOfPurchaseKey');
 LoadEntryPoint(SteamAPI_ISteamApps_GetCurrentBetaName,'SteamAPI_ISteamApps_GetCurrentBetaName');
 LoadEntryPoint(SteamAPI_ISteamApps_MarkContentCorrupt,'SteamAPI_ISteamApps_MarkContentCorrupt');
 LoadEntryPoint(SteamAPI_ISteamApps_GetInstalledDepots,'SteamAPI_ISteamApps_GetInstalledDepots');
 LoadEntryPoint(SteamAPI_ISteamApps_GetAppInstallDir,'SteamAPI_ISteamApps_GetAppInstallDir');
 LoadEntryPoint(SteamAPI_ISteamApps_BIsAppInstalled,'SteamAPI_ISteamApps_BIsAppInstalled');
 LoadEntryPoint(SteamAPI_ISteamApps_GetAppOwner,'SteamAPI_ISteamApps_GetAppOwner');
 LoadEntryPoint(SteamAPI_ISteamApps_GetLaunchQueryParam,'SteamAPI_ISteamApps_GetLaunchQueryParam');
 LoadEntryPoint(SteamAPI_ISteamApps_GetDlcDownloadProgress,'SteamAPI_ISteamApps_GetDlcDownloadProgress');
 LoadEntryPoint(SteamAPI_ISteamApps_GetAppBuildId,'SteamAPI_ISteamApps_GetAppBuildId');
 LoadEntryPoint(SteamAPI_ISteamApps_RequestAllProofOfPurchaseKeys,'SteamAPI_ISteamApps_RequestAllProofOfPurchaseKeys');
 LoadEntryPoint(SteamAPI_ISteamApps_GetFileDetails,'SteamAPI_ISteamApps_GetFileDetails');
 LoadEntryPoint(SteamAPI_ISteamApps_GetLaunchCommandLine,'SteamAPI_ISteamApps_GetLaunchCommandLine');
 LoadEntryPoint(SteamAPI_ISteamApps_BIsSubscribedFromFamilySharing,'SteamAPI_ISteamApps_BIsSubscribedFromFamilySharing');
 LoadEntryPoint(SteamAPI_ISteamApps_BIsTimedTrial,'SteamAPI_ISteamApps_BIsTimedTrial');
 LoadEntryPoint(SteamAPI_ISteamApps_SetDlcContext,'SteamAPI_ISteamApps_SetDlcContext');
 LoadEntryPoint(SteamAPI_ISteamApps_GetNumBetas,'SteamAPI_ISteamApps_GetNumBetas');
 LoadEntryPoint(SteamAPI_ISteamApps_GetBetaInfo,'SteamAPI_ISteamApps_GetBetaInfo');
 LoadEntryPoint(SteamAPI_ISteamApps_SetActiveBeta,'SteamAPI_ISteamApps_SetActiveBeta');

 // ISteamNetworking
 LoadEntryPoint(SteamAPI_SteamNetworking_v006,'SteamAPI_SteamNetworking_v006');
 LoadEntryPoint(SteamAPI_SteamGameServerNetworking_v006,'SteamAPI_SteamGameServerNetworking_v006');
 LoadEntryPoint(SteamAPI_ISteamNetworking_SendP2PPacket,'SteamAPI_ISteamNetworking_SendP2PPacket');
 LoadEntryPoint(SteamAPI_ISteamNetworking_IsP2PPacketAvailable,'SteamAPI_ISteamNetworking_IsP2PPacketAvailable');
 LoadEntryPoint(SteamAPI_ISteamNetworking_ReadP2PPacket,'SteamAPI_ISteamNetworking_ReadP2PPacket');
 LoadEntryPoint(SteamAPI_ISteamNetworking_AcceptP2PSessionWithUser,'SteamAPI_ISteamNetworking_AcceptP2PSessionWithUser');
 LoadEntryPoint(SteamAPI_ISteamNetworking_CloseP2PSessionWithUser,'SteamAPI_ISteamNetworking_CloseP2PSessionWithUser');
 LoadEntryPoint(SteamAPI_ISteamNetworking_CloseP2PChannelWithUser,'SteamAPI_ISteamNetworking_CloseP2PChannelWithUser');
 LoadEntryPoint(SteamAPI_ISteamNetworking_GetP2PSessionState,'SteamAPI_ISteamNetworking_GetP2PSessionState');
 LoadEntryPoint(SteamAPI_ISteamNetworking_AllowP2PPacketRelay,'SteamAPI_ISteamNetworking_AllowP2PPacketRelay');
 LoadEntryPoint(SteamAPI_ISteamNetworking_CreateListenSocket,'SteamAPI_ISteamNetworking_CreateListenSocket');
 LoadEntryPoint(SteamAPI_ISteamNetworking_CreateP2PConnectionSocket,'SteamAPI_ISteamNetworking_CreateP2PConnectionSocket');
 LoadEntryPoint(SteamAPI_ISteamNetworking_CreateConnectionSocket,'SteamAPI_ISteamNetworking_CreateConnectionSocket');
 LoadEntryPoint(SteamAPI_ISteamNetworking_DestroySocket,'SteamAPI_ISteamNetworking_DestroySocket');
 LoadEntryPoint(SteamAPI_ISteamNetworking_DestroyListenSocket,'SteamAPI_ISteamNetworking_DestroyListenSocket');
 LoadEntryPoint(SteamAPI_ISteamNetworking_SendDataOnSocket,'SteamAPI_ISteamNetworking_SendDataOnSocket');
 LoadEntryPoint(SteamAPI_ISteamNetworking_IsDataAvailableOnSocket,'SteamAPI_ISteamNetworking_IsDataAvailableOnSocket');
 LoadEntryPoint(SteamAPI_ISteamNetworking_RetrieveDataFromSocket,'SteamAPI_ISteamNetworking_RetrieveDataFromSocket');
 LoadEntryPoint(SteamAPI_ISteamNetworking_IsDataAvailable,'SteamAPI_ISteamNetworking_IsDataAvailable');
 LoadEntryPoint(SteamAPI_ISteamNetworking_RetrieveData,'SteamAPI_ISteamNetworking_RetrieveData');
 LoadEntryPoint(SteamAPI_ISteamNetworking_GetSocketInfo,'SteamAPI_ISteamNetworking_GetSocketInfo');
 LoadEntryPoint(SteamAPI_ISteamNetworking_GetListenSocketInfo,'SteamAPI_ISteamNetworking_GetListenSocketInfo');
 LoadEntryPoint(SteamAPI_ISteamNetworking_GetSocketConnectionType,'SteamAPI_ISteamNetworking_GetSocketConnectionType');
 LoadEntryPoint(SteamAPI_ISteamNetworking_GetMaxPacketSize,'SteamAPI_ISteamNetworking_GetMaxPacketSize');

 // ISteamScreenshots
 LoadEntryPoint(SteamAPI_SteamScreenshots_v003,'SteamAPI_SteamScreenshots_v003');
 LoadEntryPoint(SteamAPI_ISteamScreenshots_WriteScreenshot,'SteamAPI_ISteamScreenshots_WriteScreenshot');
 LoadEntryPoint(SteamAPI_ISteamScreenshots_AddScreenshotToLibrary,'SteamAPI_ISteamScreenshots_AddScreenshotToLibrary');
 LoadEntryPoint(SteamAPI_ISteamScreenshots_TriggerScreenshot,'SteamAPI_ISteamScreenshots_TriggerScreenshot');
 LoadEntryPoint(SteamAPI_ISteamScreenshots_HookScreenshots,'SteamAPI_ISteamScreenshots_HookScreenshots');
 LoadEntryPoint(SteamAPI_ISteamScreenshots_SetLocation,'SteamAPI_ISteamScreenshots_SetLocation');
 LoadEntryPoint(SteamAPI_ISteamScreenshots_TagUser,'SteamAPI_ISteamScreenshots_TagUser');
 LoadEntryPoint(SteamAPI_ISteamScreenshots_TagPublishedFile,'SteamAPI_ISteamScreenshots_TagPublishedFile');
 LoadEntryPoint(SteamAPI_ISteamScreenshots_IsScreenshotsHooked,'SteamAPI_ISteamScreenshots_IsScreenshotsHooked');
 LoadEntryPoint(SteamAPI_ISteamScreenshots_AddVRScreenshotToLibrary,'SteamAPI_ISteamScreenshots_AddVRScreenshotToLibrary');

 // ISteamMusic
 LoadEntryPoint(SteamAPI_SteamMusic_v001,'SteamAPI_SteamMusic_v001');
 LoadEntryPoint(SteamAPI_ISteamMusic_BIsEnabled,'SteamAPI_ISteamMusic_BIsEnabled');
 LoadEntryPoint(SteamAPI_ISteamMusic_BIsPlaying,'SteamAPI_ISteamMusic_BIsPlaying');
 LoadEntryPoint(SteamAPI_ISteamMusic_GetPlaybackStatus,'SteamAPI_ISteamMusic_GetPlaybackStatus');
 LoadEntryPoint(SteamAPI_ISteamMusic_Play,'SteamAPI_ISteamMusic_Play');
 LoadEntryPoint(SteamAPI_ISteamMusic_Pause,'SteamAPI_ISteamMusic_Pause');
 LoadEntryPoint(SteamAPI_ISteamMusic_PlayPrevious,'SteamAPI_ISteamMusic_PlayPrevious');
 LoadEntryPoint(SteamAPI_ISteamMusic_PlayNext,'SteamAPI_ISteamMusic_PlayNext');
 LoadEntryPoint(SteamAPI_ISteamMusic_SetVolume,'SteamAPI_ISteamMusic_SetVolume');
 LoadEntryPoint(SteamAPI_ISteamMusic_GetVolume,'SteamAPI_ISteamMusic_GetVolume');

 // ISteamHTTP
 LoadEntryPoint(SteamAPI_SteamHTTP_v003,'SteamAPI_SteamHTTP_v003');
 LoadEntryPoint(SteamAPI_SteamGameServerHTTP_v003,'SteamAPI_SteamGameServerHTTP_v003');
 LoadEntryPoint(SteamAPI_ISteamHTTP_CreateHTTPRequest,'SteamAPI_ISteamHTTP_CreateHTTPRequest');
 LoadEntryPoint(SteamAPI_ISteamHTTP_SetHTTPRequestContextValue,'SteamAPI_ISteamHTTP_SetHTTPRequestContextValue');
 LoadEntryPoint(SteamAPI_ISteamHTTP_SetHTTPRequestNetworkActivityTimeout,'SteamAPI_ISteamHTTP_SetHTTPRequestNetworkActivityTimeout');
 LoadEntryPoint(SteamAPI_ISteamHTTP_SetHTTPRequestHeaderValue,'SteamAPI_ISteamHTTP_SetHTTPRequestHeaderValue');
 LoadEntryPoint(SteamAPI_ISteamHTTP_SetHTTPRequestGetOrPostParameter,'SteamAPI_ISteamHTTP_SetHTTPRequestGetOrPostParameter');
 LoadEntryPoint(SteamAPI_ISteamHTTP_SendHTTPRequest,'SteamAPI_ISteamHTTP_SendHTTPRequest');
 LoadEntryPoint(SteamAPI_ISteamHTTP_SendHTTPRequestAndStreamResponse,'SteamAPI_ISteamHTTP_SendHTTPRequestAndStreamResponse');
 LoadEntryPoint(SteamAPI_ISteamHTTP_DeferHTTPRequest,'SteamAPI_ISteamHTTP_DeferHTTPRequest');
 LoadEntryPoint(SteamAPI_ISteamHTTP_PrioritizeHTTPRequest,'SteamAPI_ISteamHTTP_PrioritizeHTTPRequest');
 LoadEntryPoint(SteamAPI_ISteamHTTP_GetHTTPResponseHeaderSize,'SteamAPI_ISteamHTTP_GetHTTPResponseHeaderSize');
 LoadEntryPoint(SteamAPI_ISteamHTTP_GetHTTPResponseHeaderValue,'SteamAPI_ISteamHTTP_GetHTTPResponseHeaderValue');
 LoadEntryPoint(SteamAPI_ISteamHTTP_GetHTTPResponseBodySize,'SteamAPI_ISteamHTTP_GetHTTPResponseBodySize');
 LoadEntryPoint(SteamAPI_ISteamHTTP_GetHTTPResponseBodyData,'SteamAPI_ISteamHTTP_GetHTTPResponseBodyData');
 LoadEntryPoint(SteamAPI_ISteamHTTP_GetHTTPStreamingResponseBodyData,'SteamAPI_ISteamHTTP_GetHTTPStreamingResponseBodyData');
 LoadEntryPoint(SteamAPI_ISteamHTTP_ReleaseHTTPRequest,'SteamAPI_ISteamHTTP_ReleaseHTTPRequest');
 LoadEntryPoint(SteamAPI_ISteamHTTP_GetHTTPDownloadProgressPct,'SteamAPI_ISteamHTTP_GetHTTPDownloadProgressPct');
 LoadEntryPoint(SteamAPI_ISteamHTTP_SetHTTPRequestRawPostBody,'SteamAPI_ISteamHTTP_SetHTTPRequestRawPostBody');
 LoadEntryPoint(SteamAPI_ISteamHTTP_CreateCookieContainer,'SteamAPI_ISteamHTTP_CreateCookieContainer');
 LoadEntryPoint(SteamAPI_ISteamHTTP_ReleaseCookieContainer,'SteamAPI_ISteamHTTP_ReleaseCookieContainer');
 LoadEntryPoint(SteamAPI_ISteamHTTP_SetCookie,'SteamAPI_ISteamHTTP_SetCookie');
 LoadEntryPoint(SteamAPI_ISteamHTTP_SetHTTPRequestCookieContainer,'SteamAPI_ISteamHTTP_SetHTTPRequestCookieContainer');
 LoadEntryPoint(SteamAPI_ISteamHTTP_SetHTTPRequestUserAgentInfo,'SteamAPI_ISteamHTTP_SetHTTPRequestUserAgentInfo');
 LoadEntryPoint(SteamAPI_ISteamHTTP_SetHTTPRequestRequiresVerifiedCertificate,'SteamAPI_ISteamHTTP_SetHTTPRequestRequiresVerifiedCertificate');
 LoadEntryPoint(SteamAPI_ISteamHTTP_SetHTTPRequestAbsoluteTimeoutMS,'SteamAPI_ISteamHTTP_SetHTTPRequestAbsoluteTimeoutMS');
 LoadEntryPoint(SteamAPI_ISteamHTTP_GetHTTPRequestWasTimedOut,'SteamAPI_ISteamHTTP_GetHTTPRequestWasTimedOut');

 // ISteamInput
 LoadEntryPoint(SteamAPI_SteamInput_v006,'SteamAPI_SteamInput_v006');
 LoadEntryPoint(SteamAPI_ISteamInput_Init,'SteamAPI_ISteamInput_Init');
 LoadEntryPoint(SteamAPI_ISteamInput_Shutdown,'SteamAPI_ISteamInput_Shutdown');
 LoadEntryPoint(SteamAPI_ISteamInput_SetInputActionManifestFilePath,'SteamAPI_ISteamInput_SetInputActionManifestFilePath');
 LoadEntryPoint(SteamAPI_ISteamInput_RunFrame,'SteamAPI_ISteamInput_RunFrame');
 LoadEntryPoint(SteamAPI_ISteamInput_BWaitForData,'SteamAPI_ISteamInput_BWaitForData');
 LoadEntryPoint(SteamAPI_ISteamInput_BNewDataAvailable,'SteamAPI_ISteamInput_BNewDataAvailable');
 LoadEntryPoint(SteamAPI_ISteamInput_GetConnectedControllers,'SteamAPI_ISteamInput_GetConnectedControllers');
 LoadEntryPoint(SteamAPI_ISteamInput_EnableDeviceCallbacks,'SteamAPI_ISteamInput_EnableDeviceCallbacks');
 LoadEntryPoint(SteamAPI_ISteamInput_EnableActionEventCallbacks,'SteamAPI_ISteamInput_EnableActionEventCallbacks');
 LoadEntryPoint(SteamAPI_ISteamInput_GetActionSetHandle,'SteamAPI_ISteamInput_GetActionSetHandle');
 LoadEntryPoint(SteamAPI_ISteamInput_ActivateActionSet,'SteamAPI_ISteamInput_ActivateActionSet');
 LoadEntryPoint(SteamAPI_ISteamInput_GetCurrentActionSet,'SteamAPI_ISteamInput_GetCurrentActionSet');
 LoadEntryPoint(SteamAPI_ISteamInput_ActivateActionSetLayer,'SteamAPI_ISteamInput_ActivateActionSetLayer');
 LoadEntryPoint(SteamAPI_ISteamInput_DeactivateActionSetLayer,'SteamAPI_ISteamInput_DeactivateActionSetLayer');
 LoadEntryPoint(SteamAPI_ISteamInput_DeactivateAllActionSetLayers,'SteamAPI_ISteamInput_DeactivateAllActionSetLayers');
 LoadEntryPoint(SteamAPI_ISteamInput_GetActiveActionSetLayers,'SteamAPI_ISteamInput_GetActiveActionSetLayers');
 LoadEntryPoint(SteamAPI_ISteamInput_GetDigitalActionHandle,'SteamAPI_ISteamInput_GetDigitalActionHandle');
 LoadEntryPoint(SteamAPI_ISteamInput_GetDigitalActionData,'SteamAPI_ISteamInput_GetDigitalActionData');
 LoadEntryPoint(SteamAPI_ISteamInput_GetDigitalActionOrigins,'SteamAPI_ISteamInput_GetDigitalActionOrigins');
 LoadEntryPoint(SteamAPI_ISteamInput_GetStringForDigitalActionName,'SteamAPI_ISteamInput_GetStringForDigitalActionName');
 LoadEntryPoint(SteamAPI_ISteamInput_GetAnalogActionHandle,'SteamAPI_ISteamInput_GetAnalogActionHandle');
 LoadEntryPoint(SteamAPI_ISteamInput_GetAnalogActionData,'SteamAPI_ISteamInput_GetAnalogActionData');
 LoadEntryPoint(SteamAPI_ISteamInput_GetAnalogActionOrigins,'SteamAPI_ISteamInput_GetAnalogActionOrigins');
 LoadEntryPoint(SteamAPI_ISteamInput_GetGlyphPNGForActionOrigin,'SteamAPI_ISteamInput_GetGlyphPNGForActionOrigin');
 LoadEntryPoint(SteamAPI_ISteamInput_GetGlyphSVGForActionOrigin,'SteamAPI_ISteamInput_GetGlyphSVGForActionOrigin');
 LoadEntryPoint(SteamAPI_ISteamInput_GetGlyphForActionOrigin_Legacy,'SteamAPI_ISteamInput_GetGlyphForActionOrigin_Legacy');
 LoadEntryPoint(SteamAPI_ISteamInput_GetStringForActionOrigin,'SteamAPI_ISteamInput_GetStringForActionOrigin');
 LoadEntryPoint(SteamAPI_ISteamInput_GetStringForAnalogActionName,'SteamAPI_ISteamInput_GetStringForAnalogActionName');
 LoadEntryPoint(SteamAPI_ISteamInput_StopAnalogActionMomentum,'SteamAPI_ISteamInput_StopAnalogActionMomentum');
 LoadEntryPoint(SteamAPI_ISteamInput_GetMotionData,'SteamAPI_ISteamInput_GetMotionData');
 LoadEntryPoint(SteamAPI_ISteamInput_TriggerVibration,'SteamAPI_ISteamInput_TriggerVibration');
 LoadEntryPoint(SteamAPI_ISteamInput_TriggerVibrationExtended,'SteamAPI_ISteamInput_TriggerVibrationExtended');
 LoadEntryPoint(SteamAPI_ISteamInput_TriggerSimpleHapticEvent,'SteamAPI_ISteamInput_TriggerSimpleHapticEvent');
 LoadEntryPoint(SteamAPI_ISteamInput_SetLEDColor,'SteamAPI_ISteamInput_SetLEDColor');
 LoadEntryPoint(SteamAPI_ISteamInput_Legacy_TriggerHapticPulse,'SteamAPI_ISteamInput_Legacy_TriggerHapticPulse');
 LoadEntryPoint(SteamAPI_ISteamInput_Legacy_TriggerRepeatedHapticPulse,'SteamAPI_ISteamInput_Legacy_TriggerRepeatedHapticPulse');
 LoadEntryPoint(SteamAPI_ISteamInput_ShowBindingPanel,'SteamAPI_ISteamInput_ShowBindingPanel');
 LoadEntryPoint(SteamAPI_ISteamInput_GetInputTypeForHandle,'SteamAPI_ISteamInput_GetInputTypeForHandle');
 LoadEntryPoint(SteamAPI_ISteamInput_GetControllerForGamepadIndex,'SteamAPI_ISteamInput_GetControllerForGamepadIndex');
 LoadEntryPoint(SteamAPI_ISteamInput_GetGamepadIndexForController,'SteamAPI_ISteamInput_GetGamepadIndexForController');
 LoadEntryPoint(SteamAPI_ISteamInput_GetStringForXboxOrigin,'SteamAPI_ISteamInput_GetStringForXboxOrigin');
 LoadEntryPoint(SteamAPI_ISteamInput_GetGlyphForXboxOrigin,'SteamAPI_ISteamInput_GetGlyphForXboxOrigin');
 LoadEntryPoint(SteamAPI_ISteamInput_GetActionOriginFromXboxOrigin,'SteamAPI_ISteamInput_GetActionOriginFromXboxOrigin');
 LoadEntryPoint(SteamAPI_ISteamInput_TranslateActionOrigin,'SteamAPI_ISteamInput_TranslateActionOrigin');
 LoadEntryPoint(SteamAPI_ISteamInput_GetDeviceBindingRevision,'SteamAPI_ISteamInput_GetDeviceBindingRevision');
 LoadEntryPoint(SteamAPI_ISteamInput_GetRemotePlaySessionID,'SteamAPI_ISteamInput_GetRemotePlaySessionID');
 LoadEntryPoint(SteamAPI_ISteamInput_GetSessionInputConfigurationSettings,'SteamAPI_ISteamInput_GetSessionInputConfigurationSettings');
 LoadEntryPoint(SteamAPI_ISteamInput_SetDualSenseTriggerEffect,'SteamAPI_ISteamInput_SetDualSenseTriggerEffect');

 // ISteamController
 LoadEntryPoint(SteamAPI_SteamController_v008,'SteamAPI_SteamController_v008');
 LoadEntryPoint(SteamAPI_ISteamController_Init,'SteamAPI_ISteamController_Init');
 LoadEntryPoint(SteamAPI_ISteamController_Shutdown,'SteamAPI_ISteamController_Shutdown');
 LoadEntryPoint(SteamAPI_ISteamController_RunFrame,'SteamAPI_ISteamController_RunFrame');
 LoadEntryPoint(SteamAPI_ISteamController_GetConnectedControllers,'SteamAPI_ISteamController_GetConnectedControllers');
 LoadEntryPoint(SteamAPI_ISteamController_GetActionSetHandle,'SteamAPI_ISteamController_GetActionSetHandle');
 LoadEntryPoint(SteamAPI_ISteamController_ActivateActionSet,'SteamAPI_ISteamController_ActivateActionSet');
 LoadEntryPoint(SteamAPI_ISteamController_GetCurrentActionSet,'SteamAPI_ISteamController_GetCurrentActionSet');
 LoadEntryPoint(SteamAPI_ISteamController_ActivateActionSetLayer,'SteamAPI_ISteamController_ActivateActionSetLayer');
 LoadEntryPoint(SteamAPI_ISteamController_DeactivateActionSetLayer,'SteamAPI_ISteamController_DeactivateActionSetLayer');
 LoadEntryPoint(SteamAPI_ISteamController_DeactivateAllActionSetLayers,'SteamAPI_ISteamController_DeactivateAllActionSetLayers');
 LoadEntryPoint(SteamAPI_ISteamController_GetActiveActionSetLayers,'SteamAPI_ISteamController_GetActiveActionSetLayers');
 LoadEntryPoint(SteamAPI_ISteamController_GetDigitalActionHandle,'SteamAPI_ISteamController_GetDigitalActionHandle');
 LoadEntryPoint(SteamAPI_ISteamController_GetDigitalActionData,'SteamAPI_ISteamController_GetDigitalActionData');
 LoadEntryPoint(SteamAPI_ISteamController_GetDigitalActionOrigins,'SteamAPI_ISteamController_GetDigitalActionOrigins');
 LoadEntryPoint(SteamAPI_ISteamController_GetAnalogActionHandle,'SteamAPI_ISteamController_GetAnalogActionHandle');
 LoadEntryPoint(SteamAPI_ISteamController_GetAnalogActionData,'SteamAPI_ISteamController_GetAnalogActionData');
 LoadEntryPoint(SteamAPI_ISteamController_GetAnalogActionOrigins,'SteamAPI_ISteamController_GetAnalogActionOrigins');
 LoadEntryPoint(SteamAPI_ISteamController_GetGlyphForActionOrigin,'SteamAPI_ISteamController_GetGlyphForActionOrigin');
 LoadEntryPoint(SteamAPI_ISteamController_GetStringForActionOrigin,'SteamAPI_ISteamController_GetStringForActionOrigin');
 LoadEntryPoint(SteamAPI_ISteamController_StopAnalogActionMomentum,'SteamAPI_ISteamController_StopAnalogActionMomentum');
 LoadEntryPoint(SteamAPI_ISteamController_GetMotionData,'SteamAPI_ISteamController_GetMotionData');
 LoadEntryPoint(SteamAPI_ISteamController_TriggerHapticPulse,'SteamAPI_ISteamController_TriggerHapticPulse');
 LoadEntryPoint(SteamAPI_ISteamController_TriggerRepeatedHapticPulse,'SteamAPI_ISteamController_TriggerRepeatedHapticPulse');
 LoadEntryPoint(SteamAPI_ISteamController_TriggerVibration,'SteamAPI_ISteamController_TriggerVibration');
 LoadEntryPoint(SteamAPI_ISteamController_SetLEDColor,'SteamAPI_ISteamController_SetLEDColor');
 LoadEntryPoint(SteamAPI_ISteamController_ShowBindingPanel,'SteamAPI_ISteamController_ShowBindingPanel');
 LoadEntryPoint(SteamAPI_ISteamController_GetInputTypeForHandle,'SteamAPI_ISteamController_GetInputTypeForHandle');
 LoadEntryPoint(SteamAPI_ISteamController_GetControllerForGamepadIndex,'SteamAPI_ISteamController_GetControllerForGamepadIndex');
 LoadEntryPoint(SteamAPI_ISteamController_GetGamepadIndexForController,'SteamAPI_ISteamController_GetGamepadIndexForController');
 LoadEntryPoint(SteamAPI_ISteamController_GetStringForXboxOrigin,'SteamAPI_ISteamController_GetStringForXboxOrigin');
 LoadEntryPoint(SteamAPI_ISteamController_GetGlyphForXboxOrigin,'SteamAPI_ISteamController_GetGlyphForXboxOrigin');
 LoadEntryPoint(SteamAPI_ISteamController_GetActionOriginFromXboxOrigin,'SteamAPI_ISteamController_GetActionOriginFromXboxOrigin');
 LoadEntryPoint(SteamAPI_ISteamController_TranslateActionOrigin,'SteamAPI_ISteamController_TranslateActionOrigin');
 LoadEntryPoint(SteamAPI_ISteamController_GetControllerBindingRevision,'SteamAPI_ISteamController_GetControllerBindingRevision');

 // ISteamUGC
 LoadEntryPoint(SteamAPI_SteamUGC_v021,'SteamAPI_SteamUGC_v021');
 LoadEntryPoint(SteamAPI_SteamGameServerUGC_v021,'SteamAPI_SteamGameServerUGC_v021');
 LoadEntryPoint(SteamAPI_ISteamUGC_CreateQueryUserUGCRequest,'SteamAPI_ISteamUGC_CreateQueryUserUGCRequest');
 LoadEntryPoint(SteamAPI_ISteamUGC_CreateQueryAllUGCRequestPage,'SteamAPI_ISteamUGC_CreateQueryAllUGCRequestPage');
 LoadEntryPoint(SteamAPI_ISteamUGC_CreateQueryAllUGCRequestCursor,'SteamAPI_ISteamUGC_CreateQueryAllUGCRequestCursor');
 LoadEntryPoint(SteamAPI_ISteamUGC_CreateQueryUGCDetailsRequest,'SteamAPI_ISteamUGC_CreateQueryUGCDetailsRequest');
 LoadEntryPoint(SteamAPI_ISteamUGC_SendQueryUGCRequest,'SteamAPI_ISteamUGC_SendQueryUGCRequest');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetQueryUGCResult,'SteamAPI_ISteamUGC_GetQueryUGCResult');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetQueryUGCNumTags,'SteamAPI_ISteamUGC_GetQueryUGCNumTags');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetQueryUGCTag,'SteamAPI_ISteamUGC_GetQueryUGCTag');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetQueryUGCTagDisplayName,'SteamAPI_ISteamUGC_GetQueryUGCTagDisplayName');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetQueryUGCPreviewURL,'SteamAPI_ISteamUGC_GetQueryUGCPreviewURL');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetQueryUGCMetadata,'SteamAPI_ISteamUGC_GetQueryUGCMetadata');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetQueryUGCChildren,'SteamAPI_ISteamUGC_GetQueryUGCChildren');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetQueryUGCStatistic,'SteamAPI_ISteamUGC_GetQueryUGCStatistic');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetQueryUGCNumAdditionalPreviews,'SteamAPI_ISteamUGC_GetQueryUGCNumAdditionalPreviews');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetQueryUGCAdditionalPreview,'SteamAPI_ISteamUGC_GetQueryUGCAdditionalPreview');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetQueryUGCNumKeyValueTags,'SteamAPI_ISteamUGC_GetQueryUGCNumKeyValueTags');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetQueryUGCKeyValueTag,'SteamAPI_ISteamUGC_GetQueryUGCKeyValueTag');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetQueryFirstUGCKeyValueTag,'SteamAPI_ISteamUGC_GetQueryFirstUGCKeyValueTag');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetNumSupportedGameVersions,'SteamAPI_ISteamUGC_GetNumSupportedGameVersions');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetSupportedGameVersionData,'SteamAPI_ISteamUGC_GetSupportedGameVersionData');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetQueryUGCContentDescriptors,'SteamAPI_ISteamUGC_GetQueryUGCContentDescriptors');
 LoadEntryPoint(SteamAPI_ISteamUGC_ReleaseQueryUGCRequest,'SteamAPI_ISteamUGC_ReleaseQueryUGCRequest');
 LoadEntryPoint(SteamAPI_ISteamUGC_AddRequiredTag,'SteamAPI_ISteamUGC_AddRequiredTag');
 LoadEntryPoint(SteamAPI_ISteamUGC_AddRequiredTagGroup,'SteamAPI_ISteamUGC_AddRequiredTagGroup');
 LoadEntryPoint(SteamAPI_ISteamUGC_AddExcludedTag,'SteamAPI_ISteamUGC_AddExcludedTag');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetReturnOnlyIDs,'SteamAPI_ISteamUGC_SetReturnOnlyIDs');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetReturnKeyValueTags,'SteamAPI_ISteamUGC_SetReturnKeyValueTags');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetReturnLongDescription,'SteamAPI_ISteamUGC_SetReturnLongDescription');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetReturnMetadata,'SteamAPI_ISteamUGC_SetReturnMetadata');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetReturnChildren,'SteamAPI_ISteamUGC_SetReturnChildren');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetReturnAdditionalPreviews,'SteamAPI_ISteamUGC_SetReturnAdditionalPreviews');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetReturnTotalOnly,'SteamAPI_ISteamUGC_SetReturnTotalOnly');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetReturnPlaytimeStats,'SteamAPI_ISteamUGC_SetReturnPlaytimeStats');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetLanguage,'SteamAPI_ISteamUGC_SetLanguage');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetAllowCachedResponse,'SteamAPI_ISteamUGC_SetAllowCachedResponse');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetAdminQuery,'SteamAPI_ISteamUGC_SetAdminQuery');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetCloudFileNameFilter,'SteamAPI_ISteamUGC_SetCloudFileNameFilter');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetMatchAnyTag,'SteamAPI_ISteamUGC_SetMatchAnyTag');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetSearchText,'SteamAPI_ISteamUGC_SetSearchText');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetRankedByTrendDays,'SteamAPI_ISteamUGC_SetRankedByTrendDays');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetTimeCreatedDateRange,'SteamAPI_ISteamUGC_SetTimeCreatedDateRange');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetTimeUpdatedDateRange,'SteamAPI_ISteamUGC_SetTimeUpdatedDateRange');
 LoadEntryPoint(SteamAPI_ISteamUGC_AddRequiredKeyValueTag,'SteamAPI_ISteamUGC_AddRequiredKeyValueTag');
 LoadEntryPoint(SteamAPI_ISteamUGC_RequestUGCDetails,'SteamAPI_ISteamUGC_RequestUGCDetails');
 LoadEntryPoint(SteamAPI_ISteamUGC_CreateItem,'SteamAPI_ISteamUGC_CreateItem');
 LoadEntryPoint(SteamAPI_ISteamUGC_StartItemUpdate,'SteamAPI_ISteamUGC_StartItemUpdate');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetItemTitle,'SteamAPI_ISteamUGC_SetItemTitle');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetItemDescription,'SteamAPI_ISteamUGC_SetItemDescription');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetItemUpdateLanguage,'SteamAPI_ISteamUGC_SetItemUpdateLanguage');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetItemMetadata,'SteamAPI_ISteamUGC_SetItemMetadata');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetItemVisibility,'SteamAPI_ISteamUGC_SetItemVisibility');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetItemTags,'SteamAPI_ISteamUGC_SetItemTags');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetItemContent,'SteamAPI_ISteamUGC_SetItemContent');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetItemPreview,'SteamAPI_ISteamUGC_SetItemPreview');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetAllowLegacyUpload,'SteamAPI_ISteamUGC_SetAllowLegacyUpload');
 LoadEntryPoint(SteamAPI_ISteamUGC_RemoveAllItemKeyValueTags,'SteamAPI_ISteamUGC_RemoveAllItemKeyValueTags');
 LoadEntryPoint(SteamAPI_ISteamUGC_RemoveItemKeyValueTags,'SteamAPI_ISteamUGC_RemoveItemKeyValueTags');
 LoadEntryPoint(SteamAPI_ISteamUGC_AddItemKeyValueTag,'SteamAPI_ISteamUGC_AddItemKeyValueTag');
 LoadEntryPoint(SteamAPI_ISteamUGC_AddItemPreviewFile,'SteamAPI_ISteamUGC_AddItemPreviewFile');
 LoadEntryPoint(SteamAPI_ISteamUGC_AddItemPreviewVideo,'SteamAPI_ISteamUGC_AddItemPreviewVideo');
 LoadEntryPoint(SteamAPI_ISteamUGC_UpdateItemPreviewFile,'SteamAPI_ISteamUGC_UpdateItemPreviewFile');
 LoadEntryPoint(SteamAPI_ISteamUGC_UpdateItemPreviewVideo,'SteamAPI_ISteamUGC_UpdateItemPreviewVideo');
 LoadEntryPoint(SteamAPI_ISteamUGC_RemoveItemPreview,'SteamAPI_ISteamUGC_RemoveItemPreview');
 LoadEntryPoint(SteamAPI_ISteamUGC_AddContentDescriptor,'SteamAPI_ISteamUGC_AddContentDescriptor');
 LoadEntryPoint(SteamAPI_ISteamUGC_RemoveContentDescriptor,'SteamAPI_ISteamUGC_RemoveContentDescriptor');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetRequiredGameVersions,'SteamAPI_ISteamUGC_SetRequiredGameVersions');
 LoadEntryPoint(SteamAPI_ISteamUGC_SubmitItemUpdate,'SteamAPI_ISteamUGC_SubmitItemUpdate');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetItemUpdateProgress,'SteamAPI_ISteamUGC_GetItemUpdateProgress');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetUserItemVote,'SteamAPI_ISteamUGC_SetUserItemVote');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetUserItemVote,'SteamAPI_ISteamUGC_GetUserItemVote');
 LoadEntryPoint(SteamAPI_ISteamUGC_AddItemToFavorites,'SteamAPI_ISteamUGC_AddItemToFavorites');
 LoadEntryPoint(SteamAPI_ISteamUGC_RemoveItemFromFavorites,'SteamAPI_ISteamUGC_RemoveItemFromFavorites');
 LoadEntryPoint(SteamAPI_ISteamUGC_SubscribeItem,'SteamAPI_ISteamUGC_SubscribeItem');
 LoadEntryPoint(SteamAPI_ISteamUGC_UnsubscribeItem,'SteamAPI_ISteamUGC_UnsubscribeItem');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetNumSubscribedItems,'SteamAPI_ISteamUGC_GetNumSubscribedItems');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetSubscribedItems,'SteamAPI_ISteamUGC_GetSubscribedItems');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetItemState,'SteamAPI_ISteamUGC_GetItemState');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetItemInstallInfo,'SteamAPI_ISteamUGC_GetItemInstallInfo');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetItemDownloadInfo,'SteamAPI_ISteamUGC_GetItemDownloadInfo');
 LoadEntryPoint(SteamAPI_ISteamUGC_DownloadItem,'SteamAPI_ISteamUGC_DownloadItem');
 LoadEntryPoint(SteamAPI_ISteamUGC_BInitWorkshopForGameServer,'SteamAPI_ISteamUGC_BInitWorkshopForGameServer');
 LoadEntryPoint(SteamAPI_ISteamUGC_SuspendDownloads,'SteamAPI_ISteamUGC_SuspendDownloads');
 LoadEntryPoint(SteamAPI_ISteamUGC_StartPlaytimeTracking,'SteamAPI_ISteamUGC_StartPlaytimeTracking');
 LoadEntryPoint(SteamAPI_ISteamUGC_StopPlaytimeTracking,'SteamAPI_ISteamUGC_StopPlaytimeTracking');
 LoadEntryPoint(SteamAPI_ISteamUGC_StopPlaytimeTrackingForAllItems,'SteamAPI_ISteamUGC_StopPlaytimeTrackingForAllItems');
 LoadEntryPoint(SteamAPI_ISteamUGC_AddDependency,'SteamAPI_ISteamUGC_AddDependency');
 LoadEntryPoint(SteamAPI_ISteamUGC_RemoveDependency,'SteamAPI_ISteamUGC_RemoveDependency');
 LoadEntryPoint(SteamAPI_ISteamUGC_AddAppDependency,'SteamAPI_ISteamUGC_AddAppDependency');
 LoadEntryPoint(SteamAPI_ISteamUGC_RemoveAppDependency,'SteamAPI_ISteamUGC_RemoveAppDependency');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetAppDependencies,'SteamAPI_ISteamUGC_GetAppDependencies');
 LoadEntryPoint(SteamAPI_ISteamUGC_DeleteItem,'SteamAPI_ISteamUGC_DeleteItem');
 LoadEntryPoint(SteamAPI_ISteamUGC_ShowWorkshopEULA,'SteamAPI_ISteamUGC_ShowWorkshopEULA');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetWorkshopEULAStatus,'SteamAPI_ISteamUGC_GetWorkshopEULAStatus');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetUserContentDescriptorPreferences,'SteamAPI_ISteamUGC_GetUserContentDescriptorPreferences');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetItemsDisabledLocally,'SteamAPI_ISteamUGC_SetItemsDisabledLocally');
 LoadEntryPoint(SteamAPI_ISteamUGC_SetSubscriptionsLoadOrder,'SteamAPI_ISteamUGC_SetSubscriptionsLoadOrder');
 LoadEntryPoint(SteamAPI_ISteamUGC_MarkDownloadedItemAsUnused,'SteamAPI_ISteamUGC_MarkDownloadedItemAsUnused');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetNumDownloadedItems,'SteamAPI_ISteamUGC_GetNumDownloadedItems');
 LoadEntryPoint(SteamAPI_ISteamUGC_GetDownloadedItems,'SteamAPI_ISteamUGC_GetDownloadedItems');

 // ISteamHTMLSurface
 LoadEntryPoint(SteamAPI_SteamHTMLSurface_v005,'SteamAPI_SteamHTMLSurface_v005');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_Init,'SteamAPI_ISteamHTMLSurface_Init');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_Shutdown,'SteamAPI_ISteamHTMLSurface_Shutdown');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_CreateBrowser,'SteamAPI_ISteamHTMLSurface_CreateBrowser');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_RemoveBrowser,'SteamAPI_ISteamHTMLSurface_RemoveBrowser');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_LoadURL,'SteamAPI_ISteamHTMLSurface_LoadURL');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_SetSize,'SteamAPI_ISteamHTMLSurface_SetSize');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_StopLoad,'SteamAPI_ISteamHTMLSurface_StopLoad');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_Reload,'SteamAPI_ISteamHTMLSurface_Reload');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_GoBack,'SteamAPI_ISteamHTMLSurface_GoBack');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_GoForward,'SteamAPI_ISteamHTMLSurface_GoForward');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_AddHeader,'SteamAPI_ISteamHTMLSurface_AddHeader');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_ExecuteJavascript,'SteamAPI_ISteamHTMLSurface_ExecuteJavascript');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_MouseUp,'SteamAPI_ISteamHTMLSurface_MouseUp');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_MouseDown,'SteamAPI_ISteamHTMLSurface_MouseDown');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_MouseDoubleClick,'SteamAPI_ISteamHTMLSurface_MouseDoubleClick');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_MouseMove,'SteamAPI_ISteamHTMLSurface_MouseMove');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_MouseWheel,'SteamAPI_ISteamHTMLSurface_MouseWheel');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_KeyDown,'SteamAPI_ISteamHTMLSurface_KeyDown');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_KeyUp,'SteamAPI_ISteamHTMLSurface_KeyUp');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_KeyChar,'SteamAPI_ISteamHTMLSurface_KeyChar');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_SetHorizontalScroll,'SteamAPI_ISteamHTMLSurface_SetHorizontalScroll');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_SetVerticalScroll,'SteamAPI_ISteamHTMLSurface_SetVerticalScroll');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_SetKeyFocus,'SteamAPI_ISteamHTMLSurface_SetKeyFocus');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_ViewSource,'SteamAPI_ISteamHTMLSurface_ViewSource');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_CopyToClipboard,'SteamAPI_ISteamHTMLSurface_CopyToClipboard');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_PasteFromClipboard,'SteamAPI_ISteamHTMLSurface_PasteFromClipboard');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_Find,'SteamAPI_ISteamHTMLSurface_Find');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_StopFind,'SteamAPI_ISteamHTMLSurface_StopFind');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_GetLinkAtPosition,'SteamAPI_ISteamHTMLSurface_GetLinkAtPosition');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_SetCookie,'SteamAPI_ISteamHTMLSurface_SetCookie');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_SetPageScaleFactor,'SteamAPI_ISteamHTMLSurface_SetPageScaleFactor');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_SetBackgroundMode,'SteamAPI_ISteamHTMLSurface_SetBackgroundMode');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_SetDPIScalingFactor,'SteamAPI_ISteamHTMLSurface_SetDPIScalingFactor');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_OpenDeveloperTools,'SteamAPI_ISteamHTMLSurface_OpenDeveloperTools');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_AllowStartRequest,'SteamAPI_ISteamHTMLSurface_AllowStartRequest');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_JSDialogResponse,'SteamAPI_ISteamHTMLSurface_JSDialogResponse');
 LoadEntryPoint(SteamAPI_ISteamHTMLSurface_FileLoadDialogResponse,'SteamAPI_ISteamHTMLSurface_FileLoadDialogResponse');

 // ISteamInventory
 LoadEntryPoint(SteamAPI_SteamInventory_v003,'SteamAPI_SteamInventory_v003');
 LoadEntryPoint(SteamAPI_SteamGameServerInventory_v003,'SteamAPI_SteamGameServerInventory_v003');
 LoadEntryPoint(SteamAPI_ISteamInventory_GetResultStatus,'SteamAPI_ISteamInventory_GetResultStatus');
 LoadEntryPoint(SteamAPI_ISteamInventory_GetResultItems,'SteamAPI_ISteamInventory_GetResultItems');
 LoadEntryPoint(SteamAPI_ISteamInventory_GetResultItemProperty,'SteamAPI_ISteamInventory_GetResultItemProperty');
 LoadEntryPoint(SteamAPI_ISteamInventory_GetResultTimestamp,'SteamAPI_ISteamInventory_GetResultTimestamp');
 LoadEntryPoint(SteamAPI_ISteamInventory_CheckResultSteamID,'SteamAPI_ISteamInventory_CheckResultSteamID');
 LoadEntryPoint(SteamAPI_ISteamInventory_DestroyResult,'SteamAPI_ISteamInventory_DestroyResult');
 LoadEntryPoint(SteamAPI_ISteamInventory_GetAllItems,'SteamAPI_ISteamInventory_GetAllItems');
 LoadEntryPoint(SteamAPI_ISteamInventory_GetItemsByID,'SteamAPI_ISteamInventory_GetItemsByID');
 LoadEntryPoint(SteamAPI_ISteamInventory_SerializeResult,'SteamAPI_ISteamInventory_SerializeResult');
 LoadEntryPoint(SteamAPI_ISteamInventory_DeserializeResult,'SteamAPI_ISteamInventory_DeserializeResult');
 LoadEntryPoint(SteamAPI_ISteamInventory_GenerateItems,'SteamAPI_ISteamInventory_GenerateItems');
 LoadEntryPoint(SteamAPI_ISteamInventory_GrantPromoItems,'SteamAPI_ISteamInventory_GrantPromoItems');
 LoadEntryPoint(SteamAPI_ISteamInventory_AddPromoItem,'SteamAPI_ISteamInventory_AddPromoItem');
 LoadEntryPoint(SteamAPI_ISteamInventory_AddPromoItems,'SteamAPI_ISteamInventory_AddPromoItems');
 LoadEntryPoint(SteamAPI_ISteamInventory_ConsumeItem,'SteamAPI_ISteamInventory_ConsumeItem');
 LoadEntryPoint(SteamAPI_ISteamInventory_ExchangeItems,'SteamAPI_ISteamInventory_ExchangeItems');
 LoadEntryPoint(SteamAPI_ISteamInventory_TransferItemQuantity,'SteamAPI_ISteamInventory_TransferItemQuantity');
 LoadEntryPoint(SteamAPI_ISteamInventory_SendItemDropHeartbeat,'SteamAPI_ISteamInventory_SendItemDropHeartbeat');
 LoadEntryPoint(SteamAPI_ISteamInventory_TriggerItemDrop,'SteamAPI_ISteamInventory_TriggerItemDrop');
 LoadEntryPoint(SteamAPI_ISteamInventory_TradeItems,'SteamAPI_ISteamInventory_TradeItems');
 LoadEntryPoint(SteamAPI_ISteamInventory_LoadItemDefinitions,'SteamAPI_ISteamInventory_LoadItemDefinitions');
 LoadEntryPoint(SteamAPI_ISteamInventory_GetItemDefinitionIDs,'SteamAPI_ISteamInventory_GetItemDefinitionIDs');
 LoadEntryPoint(SteamAPI_ISteamInventory_GetItemDefinitionProperty,'SteamAPI_ISteamInventory_GetItemDefinitionProperty');
 LoadEntryPoint(SteamAPI_ISteamInventory_RequestEligiblePromoItemDefinitionsIDs,'SteamAPI_ISteamInventory_RequestEligiblePromoItemDefinitionsIDs');
 LoadEntryPoint(SteamAPI_ISteamInventory_GetEligiblePromoItemDefinitionIDs,'SteamAPI_ISteamInventory_GetEligiblePromoItemDefinitionIDs');
 LoadEntryPoint(SteamAPI_ISteamInventory_StartPurchase,'SteamAPI_ISteamInventory_StartPurchase');
 LoadEntryPoint(SteamAPI_ISteamInventory_RequestPrices,'SteamAPI_ISteamInventory_RequestPrices');
 LoadEntryPoint(SteamAPI_ISteamInventory_GetNumItemsWithPrices,'SteamAPI_ISteamInventory_GetNumItemsWithPrices');
 LoadEntryPoint(SteamAPI_ISteamInventory_GetItemsWithPrices,'SteamAPI_ISteamInventory_GetItemsWithPrices');
 LoadEntryPoint(SteamAPI_ISteamInventory_GetItemPrice,'SteamAPI_ISteamInventory_GetItemPrice');
 LoadEntryPoint(SteamAPI_ISteamInventory_StartUpdateProperties,'SteamAPI_ISteamInventory_StartUpdateProperties');
 LoadEntryPoint(SteamAPI_ISteamInventory_RemoveProperty,'SteamAPI_ISteamInventory_RemoveProperty');
 LoadEntryPoint(SteamAPI_ISteamInventory_SetPropertyString,'SteamAPI_ISteamInventory_SetPropertyString');
 LoadEntryPoint(SteamAPI_ISteamInventory_SetPropertyBool,'SteamAPI_ISteamInventory_SetPropertyBool');
 LoadEntryPoint(SteamAPI_ISteamInventory_SetPropertyInt64,'SteamAPI_ISteamInventory_SetPropertyInt64');
 LoadEntryPoint(SteamAPI_ISteamInventory_SetPropertyFloat,'SteamAPI_ISteamInventory_SetPropertyFloat');
 LoadEntryPoint(SteamAPI_ISteamInventory_SubmitUpdateProperties,'SteamAPI_ISteamInventory_SubmitUpdateProperties');
 LoadEntryPoint(SteamAPI_ISteamInventory_InspectItem,'SteamAPI_ISteamInventory_InspectItem');

 // ISteamTimeline
 LoadEntryPoint(SteamAPI_SteamTimeline_v004,'SteamAPI_SteamTimeline_v004');
 LoadEntryPoint(SteamAPI_ISteamTimeline_SetTimelineTooltip,'SteamAPI_ISteamTimeline_SetTimelineTooltip');
 LoadEntryPoint(SteamAPI_ISteamTimeline_ClearTimelineTooltip,'SteamAPI_ISteamTimeline_ClearTimelineTooltip');
 LoadEntryPoint(SteamAPI_ISteamTimeline_SetTimelineGameMode,'SteamAPI_ISteamTimeline_SetTimelineGameMode');
 LoadEntryPoint(SteamAPI_ISteamTimeline_AddInstantaneousTimelineEvent,'SteamAPI_ISteamTimeline_AddInstantaneousTimelineEvent');
 LoadEntryPoint(SteamAPI_ISteamTimeline_AddRangeTimelineEvent,'SteamAPI_ISteamTimeline_AddRangeTimelineEvent');
 LoadEntryPoint(SteamAPI_ISteamTimeline_StartRangeTimelineEvent,'SteamAPI_ISteamTimeline_StartRangeTimelineEvent');
 LoadEntryPoint(SteamAPI_ISteamTimeline_UpdateRangeTimelineEvent,'SteamAPI_ISteamTimeline_UpdateRangeTimelineEvent');
 LoadEntryPoint(SteamAPI_ISteamTimeline_EndRangeTimelineEvent,'SteamAPI_ISteamTimeline_EndRangeTimelineEvent');
 LoadEntryPoint(SteamAPI_ISteamTimeline_RemoveTimelineEvent,'SteamAPI_ISteamTimeline_RemoveTimelineEvent');
 LoadEntryPoint(SteamAPI_ISteamTimeline_DoesEventRecordingExist,'SteamAPI_ISteamTimeline_DoesEventRecordingExist');
 LoadEntryPoint(SteamAPI_ISteamTimeline_StartGamePhase,'SteamAPI_ISteamTimeline_StartGamePhase');
 LoadEntryPoint(SteamAPI_ISteamTimeline_EndGamePhase,'SteamAPI_ISteamTimeline_EndGamePhase');
 LoadEntryPoint(SteamAPI_ISteamTimeline_SetGamePhaseID,'SteamAPI_ISteamTimeline_SetGamePhaseID');
 LoadEntryPoint(SteamAPI_ISteamTimeline_DoesGamePhaseRecordingExist,'SteamAPI_ISteamTimeline_DoesGamePhaseRecordingExist');
 LoadEntryPoint(SteamAPI_ISteamTimeline_AddGamePhaseTag,'SteamAPI_ISteamTimeline_AddGamePhaseTag');
 LoadEntryPoint(SteamAPI_ISteamTimeline_SetGamePhaseAttribute,'SteamAPI_ISteamTimeline_SetGamePhaseAttribute');
 LoadEntryPoint(SteamAPI_ISteamTimeline_OpenOverlayToGamePhase,'SteamAPI_ISteamTimeline_OpenOverlayToGamePhase');
 LoadEntryPoint(SteamAPI_ISteamTimeline_OpenOverlayToTimelineEvent,'SteamAPI_ISteamTimeline_OpenOverlayToTimelineEvent');

 // ISteamVideo
 LoadEntryPoint(SteamAPI_SteamVideo_v007,'SteamAPI_SteamVideo_v007');
 LoadEntryPoint(SteamAPI_ISteamVideo_GetVideoURL,'SteamAPI_ISteamVideo_GetVideoURL');
 LoadEntryPoint(SteamAPI_ISteamVideo_IsBroadcasting,'SteamAPI_ISteamVideo_IsBroadcasting');
 LoadEntryPoint(SteamAPI_ISteamVideo_GetOPFSettings,'SteamAPI_ISteamVideo_GetOPFSettings');
 LoadEntryPoint(SteamAPI_ISteamVideo_GetOPFStringForApp,'SteamAPI_ISteamVideo_GetOPFStringForApp');

 // ISteamParentalSettings
 LoadEntryPoint(SteamAPI_SteamParentalSettings_v001,'SteamAPI_SteamParentalSettings_v001');
 LoadEntryPoint(SteamAPI_ISteamParentalSettings_BIsParentalLockEnabled,'SteamAPI_ISteamParentalSettings_BIsParentalLockEnabled');
 LoadEntryPoint(SteamAPI_ISteamParentalSettings_BIsParentalLockLocked,'SteamAPI_ISteamParentalSettings_BIsParentalLockLocked');
 LoadEntryPoint(SteamAPI_ISteamParentalSettings_BIsAppBlocked,'SteamAPI_ISteamParentalSettings_BIsAppBlocked');
 LoadEntryPoint(SteamAPI_ISteamParentalSettings_BIsAppInBlockList,'SteamAPI_ISteamParentalSettings_BIsAppInBlockList');
 LoadEntryPoint(SteamAPI_ISteamParentalSettings_BIsFeatureBlocked,'SteamAPI_ISteamParentalSettings_BIsFeatureBlocked');
 LoadEntryPoint(SteamAPI_ISteamParentalSettings_BIsFeatureInBlockList,'SteamAPI_ISteamParentalSettings_BIsFeatureInBlockList');

 // ISteamRemotePlay
 LoadEntryPoint(SteamAPI_SteamRemotePlay_v004,'SteamAPI_SteamRemotePlay_v004');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_GetSessionCount,'SteamAPI_ISteamRemotePlay_GetSessionCount');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_GetSessionID,'SteamAPI_ISteamRemotePlay_GetSessionID');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_BSessionRemotePlayTogether,'SteamAPI_ISteamRemotePlay_BSessionRemotePlayTogether');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_GetSessionSteamID,'SteamAPI_ISteamRemotePlay_GetSessionSteamID');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_GetSessionGuestID,'SteamAPI_ISteamRemotePlay_GetSessionGuestID');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_GetSmallSessionAvatar,'SteamAPI_ISteamRemotePlay_GetSmallSessionAvatar');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_GetMediumSessionAvatar,'SteamAPI_ISteamRemotePlay_GetMediumSessionAvatar');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_GetLargeSessionAvatar,'SteamAPI_ISteamRemotePlay_GetLargeSessionAvatar');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_GetSessionClientName,'SteamAPI_ISteamRemotePlay_GetSessionClientName');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_GetSessionClientFormFactor,'SteamAPI_ISteamRemotePlay_GetSessionClientFormFactor');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_BGetSessionClientResolution,'SteamAPI_ISteamRemotePlay_BGetSessionClientResolution');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_ShowRemotePlayTogetherUI,'SteamAPI_ISteamRemotePlay_ShowRemotePlayTogetherUI');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_BSendRemotePlayTogetherInvite,'SteamAPI_ISteamRemotePlay_BSendRemotePlayTogetherInvite');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_BEnableRemotePlayTogetherDirectInput,'SteamAPI_ISteamRemotePlay_BEnableRemotePlayTogetherDirectInput');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_DisableRemotePlayTogetherDirectInput,'SteamAPI_ISteamRemotePlay_DisableRemotePlayTogetherDirectInput');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_GetInput,'SteamAPI_ISteamRemotePlay_GetInput');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_SetMouseVisibility,'SteamAPI_ISteamRemotePlay_SetMouseVisibility');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_SetMousePosition,'SteamAPI_ISteamRemotePlay_SetMousePosition');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_CreateMouseCursor,'SteamAPI_ISteamRemotePlay_CreateMouseCursor');
 LoadEntryPoint(SteamAPI_ISteamRemotePlay_SetMouseCursor,'SteamAPI_ISteamRemotePlay_SetMouseCursor');

 // ISteamNetworkingMessages
 LoadEntryPoint(SteamAPI_SteamNetworkingMessages_SteamAPI_v002,'SteamAPI_SteamNetworkingMessages_SteamAPI_v002');
 LoadEntryPoint(SteamAPI_SteamGameServerNetworkingMessages_SteamAPI_v002,'SteamAPI_SteamGameServerNetworkingMessages_SteamAPI_v002');
 LoadEntryPoint(SteamAPI_ISteamNetworkingMessages_SendMessageToUser,'SteamAPI_ISteamNetworkingMessages_SendMessageToUser');
 LoadEntryPoint(SteamAPI_ISteamNetworkingMessages_ReceiveMessagesOnChannel,'SteamAPI_ISteamNetworkingMessages_ReceiveMessagesOnChannel');
 LoadEntryPoint(SteamAPI_ISteamNetworkingMessages_AcceptSessionWithUser,'SteamAPI_ISteamNetworkingMessages_AcceptSessionWithUser');
 LoadEntryPoint(SteamAPI_ISteamNetworkingMessages_CloseSessionWithUser,'SteamAPI_ISteamNetworkingMessages_CloseSessionWithUser');
 LoadEntryPoint(SteamAPI_ISteamNetworkingMessages_CloseChannelWithUser,'SteamAPI_ISteamNetworkingMessages_CloseChannelWithUser');
 LoadEntryPoint(SteamAPI_ISteamNetworkingMessages_GetSessionConnectionInfo,'SteamAPI_ISteamNetworkingMessages_GetSessionConnectionInfo');

 // ISteamNetworkingSockets
 LoadEntryPoint(SteamAPI_SteamNetworkingSockets_SteamAPI_v012,'SteamAPI_SteamNetworkingSockets_SteamAPI_v012');
 LoadEntryPoint(SteamAPI_SteamGameServerNetworkingSockets_SteamAPI_v012,'SteamAPI_SteamGameServerNetworkingSockets_SteamAPI_v012');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_CreateListenSocketIP,'SteamAPI_ISteamNetworkingSockets_CreateListenSocketIP');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_ConnectByIPAddress,'SteamAPI_ISteamNetworkingSockets_ConnectByIPAddress');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_CreateListenSocketP2P,'SteamAPI_ISteamNetworkingSockets_CreateListenSocketP2P');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_ConnectP2P,'SteamAPI_ISteamNetworkingSockets_ConnectP2P');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_AcceptConnection,'SteamAPI_ISteamNetworkingSockets_AcceptConnection');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_CloseConnection,'SteamAPI_ISteamNetworkingSockets_CloseConnection');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_CloseListenSocket,'SteamAPI_ISteamNetworkingSockets_CloseListenSocket');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_SetConnectionUserData,'SteamAPI_ISteamNetworkingSockets_SetConnectionUserData');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_GetConnectionUserData,'SteamAPI_ISteamNetworkingSockets_GetConnectionUserData');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_SetConnectionName,'SteamAPI_ISteamNetworkingSockets_SetConnectionName');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_GetConnectionName,'SteamAPI_ISteamNetworkingSockets_GetConnectionName');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_SendMessageToConnection,'SteamAPI_ISteamNetworkingSockets_SendMessageToConnection');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_SendMessages,'SteamAPI_ISteamNetworkingSockets_SendMessages');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_FlushMessagesOnConnection,'SteamAPI_ISteamNetworkingSockets_FlushMessagesOnConnection');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnConnection,'SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnConnection');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_GetConnectionInfo,'SteamAPI_ISteamNetworkingSockets_GetConnectionInfo');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_GetConnectionRealTimeStatus,'SteamAPI_ISteamNetworkingSockets_GetConnectionRealTimeStatus');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_GetDetailedConnectionStatus,'SteamAPI_ISteamNetworkingSockets_GetDetailedConnectionStatus');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_GetListenSocketAddress,'SteamAPI_ISteamNetworkingSockets_GetListenSocketAddress');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_CreateSocketPair,'SteamAPI_ISteamNetworkingSockets_CreateSocketPair');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_ConfigureConnectionLanes,'SteamAPI_ISteamNetworkingSockets_ConfigureConnectionLanes');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_GetIdentity,'SteamAPI_ISteamNetworkingSockets_GetIdentity');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_InitAuthentication,'SteamAPI_ISteamNetworkingSockets_InitAuthentication');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_GetAuthenticationStatus,'SteamAPI_ISteamNetworkingSockets_GetAuthenticationStatus');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_CreatePollGroup,'SteamAPI_ISteamNetworkingSockets_CreatePollGroup');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_DestroyPollGroup,'SteamAPI_ISteamNetworkingSockets_DestroyPollGroup');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_SetConnectionPollGroup,'SteamAPI_ISteamNetworkingSockets_SetConnectionPollGroup');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnPollGroup,'SteamAPI_ISteamNetworkingSockets_ReceiveMessagesOnPollGroup');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_ReceivedRelayAuthTicket,'SteamAPI_ISteamNetworkingSockets_ReceivedRelayAuthTicket');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_FindRelayAuthTicketForServer,'SteamAPI_ISteamNetworkingSockets_FindRelayAuthTicketForServer');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_ConnectToHostedDedicatedServer,'SteamAPI_ISteamNetworkingSockets_ConnectToHostedDedicatedServer');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_GetHostedDedicatedServerPort,'SteamAPI_ISteamNetworkingSockets_GetHostedDedicatedServerPort');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_GetHostedDedicatedServerPOPID,'SteamAPI_ISteamNetworkingSockets_GetHostedDedicatedServerPOPID');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_GetHostedDedicatedServerAddress,'SteamAPI_ISteamNetworkingSockets_GetHostedDedicatedServerAddress');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_CreateHostedDedicatedServerListenSocket,'SteamAPI_ISteamNetworkingSockets_CreateHostedDedicatedServerListenSocket');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_GetGameCoordinatorServerLogin,'SteamAPI_ISteamNetworkingSockets_GetGameCoordinatorServerLogin');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_ConnectP2PCustomSignaling,'SteamAPI_ISteamNetworkingSockets_ConnectP2PCustomSignaling');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_ReceivedP2PCustomSignal,'SteamAPI_ISteamNetworkingSockets_ReceivedP2PCustomSignal');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_GetCertificateRequest,'SteamAPI_ISteamNetworkingSockets_GetCertificateRequest');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_SetCertificate,'SteamAPI_ISteamNetworkingSockets_SetCertificate');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_ResetIdentity,'SteamAPI_ISteamNetworkingSockets_ResetIdentity');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_RunCallbacks,'SteamAPI_ISteamNetworkingSockets_RunCallbacks');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_BeginAsyncRequestFakeIP,'SteamAPI_ISteamNetworkingSockets_BeginAsyncRequestFakeIP');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_GetFakeIP,'SteamAPI_ISteamNetworkingSockets_GetFakeIP');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_CreateListenSocketP2PFakeIP,'SteamAPI_ISteamNetworkingSockets_CreateListenSocketP2PFakeIP');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_GetRemoteFakeIPForConnection,'SteamAPI_ISteamNetworkingSockets_GetRemoteFakeIPForConnection');
 LoadEntryPoint(SteamAPI_ISteamNetworkingSockets_CreateFakeUDPPort,'SteamAPI_ISteamNetworkingSockets_CreateFakeUDPPort');

 // ISteamNetworkingUtils
 LoadEntryPoint(SteamAPI_SteamNetworkingUtils_SteamAPI_v004,'SteamAPI_SteamNetworkingUtils_SteamAPI_v004');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_AllocateMessage,'SteamAPI_ISteamNetworkingUtils_AllocateMessage');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_InitRelayNetworkAccess,'SteamAPI_ISteamNetworkingUtils_InitRelayNetworkAccess');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_GetRelayNetworkStatus,'SteamAPI_ISteamNetworkingUtils_GetRelayNetworkStatus');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_GetLocalPingLocation,'SteamAPI_ISteamNetworkingUtils_GetLocalPingLocation');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_EstimatePingTimeBetweenTwoLocations,'SteamAPI_ISteamNetworkingUtils_EstimatePingTimeBetweenTwoLocations');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_EstimatePingTimeFromLocalHost,'SteamAPI_ISteamNetworkingUtils_EstimatePingTimeFromLocalHost');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_ConvertPingLocationToString,'SteamAPI_ISteamNetworkingUtils_ConvertPingLocationToString');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_ParsePingLocationString,'SteamAPI_ISteamNetworkingUtils_ParsePingLocationString');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_CheckPingDataUpToDate,'SteamAPI_ISteamNetworkingUtils_CheckPingDataUpToDate');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_GetPingToDataCenter,'SteamAPI_ISteamNetworkingUtils_GetPingToDataCenter');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_GetDirectPingToPOP,'SteamAPI_ISteamNetworkingUtils_GetDirectPingToPOP');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_GetPOPCount,'SteamAPI_ISteamNetworkingUtils_GetPOPCount');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_GetPOPList,'SteamAPI_ISteamNetworkingUtils_GetPOPList');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_GetLocalTimestamp,'SteamAPI_ISteamNetworkingUtils_GetLocalTimestamp');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetDebugOutputFunction,'SteamAPI_ISteamNetworkingUtils_SetDebugOutputFunction');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_IsFakeIPv4,'SteamAPI_ISteamNetworkingUtils_IsFakeIPv4');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_GetIPv4FakeIPType,'SteamAPI_ISteamNetworkingUtils_GetIPv4FakeIPType');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_GetRealIdentityForFakeIP,'SteamAPI_ISteamNetworkingUtils_GetRealIdentityForFakeIP');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueInt32,'SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueInt32');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueFloat,'SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueFloat');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueString,'SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValueString');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValuePtr,'SteamAPI_ISteamNetworkingUtils_SetGlobalConfigValuePtr');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueInt32,'SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueInt32');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueFloat,'SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueFloat');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueString,'SteamAPI_ISteamNetworkingUtils_SetConnectionConfigValueString');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamNetConnectionStatusChanged,'SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamNetConnectionStatusChanged');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamNetAuthenticationStatusChanged,'SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamNetAuthenticationStatusChanged');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamRelayNetworkStatusChanged,'SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_SteamRelayNetworkStatusChanged');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_FakeIPResult,'SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_FakeIPResult');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_MessagesSessionRequest,'SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_MessagesSessionRequest');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_MessagesSessionFailed,'SteamAPI_ISteamNetworkingUtils_SetGlobalCallback_MessagesSessionFailed');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetConfigValue,'SteamAPI_ISteamNetworkingUtils_SetConfigValue');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SetConfigValueStruct,'SteamAPI_ISteamNetworkingUtils_SetConfigValueStruct');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_GetConfigValue,'SteamAPI_ISteamNetworkingUtils_GetConfigValue');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_GetConfigValueInfo,'SteamAPI_ISteamNetworkingUtils_GetConfigValueInfo');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_IterateGenericEditableConfigValues,'SteamAPI_ISteamNetworkingUtils_IterateGenericEditableConfigValues');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SteamNetworkingIPAddr_ToString,'SteamAPI_ISteamNetworkingUtils_SteamNetworkingIPAddr_ToString');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SteamNetworkingIPAddr_ParseString,'SteamAPI_ISteamNetworkingUtils_SteamNetworkingIPAddr_ParseString');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SteamNetworkingIPAddr_GetFakeIPType,'SteamAPI_ISteamNetworkingUtils_SteamNetworkingIPAddr_GetFakeIPType');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SteamNetworkingIdentity_ToString,'SteamAPI_ISteamNetworkingUtils_SteamNetworkingIdentity_ToString');
 LoadEntryPoint(SteamAPI_ISteamNetworkingUtils_SteamNetworkingIdentity_ParseString,'SteamAPI_ISteamNetworkingUtils_SteamNetworkingIdentity_ParseString');

 // ISteamGameServer
 LoadEntryPoint(SteamAPI_SteamGameServer_v015,'SteamAPI_SteamGameServer_v015');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetProduct,'SteamAPI_ISteamGameServer_SetProduct');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetGameDescription,'SteamAPI_ISteamGameServer_SetGameDescription');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetModDir,'SteamAPI_ISteamGameServer_SetModDir');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetDedicatedServer,'SteamAPI_ISteamGameServer_SetDedicatedServer');
 LoadEntryPoint(SteamAPI_ISteamGameServer_LogOn,'SteamAPI_ISteamGameServer_LogOn');
 LoadEntryPoint(SteamAPI_ISteamGameServer_LogOnAnonymous,'SteamAPI_ISteamGameServer_LogOnAnonymous');
 LoadEntryPoint(SteamAPI_ISteamGameServer_LogOff,'SteamAPI_ISteamGameServer_LogOff');
 LoadEntryPoint(SteamAPI_ISteamGameServer_BLoggedOn,'SteamAPI_ISteamGameServer_BLoggedOn');
 LoadEntryPoint(SteamAPI_ISteamGameServer_BSecure,'SteamAPI_ISteamGameServer_BSecure');
 LoadEntryPoint(SteamAPI_ISteamGameServer_GetSteamID,'SteamAPI_ISteamGameServer_GetSteamID');
 LoadEntryPoint(SteamAPI_ISteamGameServer_WasRestartRequested,'SteamAPI_ISteamGameServer_WasRestartRequested');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetMaxPlayerCount,'SteamAPI_ISteamGameServer_SetMaxPlayerCount');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetBotPlayerCount,'SteamAPI_ISteamGameServer_SetBotPlayerCount');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetServerName,'SteamAPI_ISteamGameServer_SetServerName');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetMapName,'SteamAPI_ISteamGameServer_SetMapName');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetPasswordProtected,'SteamAPI_ISteamGameServer_SetPasswordProtected');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetSpectatorPort,'SteamAPI_ISteamGameServer_SetSpectatorPort');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetSpectatorServerName,'SteamAPI_ISteamGameServer_SetSpectatorServerName');
 LoadEntryPoint(SteamAPI_ISteamGameServer_ClearAllKeyValues,'SteamAPI_ISteamGameServer_ClearAllKeyValues');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetKeyValue,'SteamAPI_ISteamGameServer_SetKeyValue');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetGameTags,'SteamAPI_ISteamGameServer_SetGameTags');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetGameData,'SteamAPI_ISteamGameServer_SetGameData');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetRegion,'SteamAPI_ISteamGameServer_SetRegion');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SetAdvertiseServerActive,'SteamAPI_ISteamGameServer_SetAdvertiseServerActive');
 LoadEntryPoint(SteamAPI_ISteamGameServer_GetAuthSessionTicket,'SteamAPI_ISteamGameServer_GetAuthSessionTicket');
 LoadEntryPoint(SteamAPI_ISteamGameServer_BeginAuthSession,'SteamAPI_ISteamGameServer_BeginAuthSession');
 LoadEntryPoint(SteamAPI_ISteamGameServer_EndAuthSession,'SteamAPI_ISteamGameServer_EndAuthSession');
 LoadEntryPoint(SteamAPI_ISteamGameServer_CancelAuthTicket,'SteamAPI_ISteamGameServer_CancelAuthTicket');
 LoadEntryPoint(SteamAPI_ISteamGameServer_UserHasLicenseForApp,'SteamAPI_ISteamGameServer_UserHasLicenseForApp');
 LoadEntryPoint(SteamAPI_ISteamGameServer_RequestUserGroupStatus,'SteamAPI_ISteamGameServer_RequestUserGroupStatus');
 LoadEntryPoint(SteamAPI_ISteamGameServer_GetGameplayStats,'SteamAPI_ISteamGameServer_GetGameplayStats');
 LoadEntryPoint(SteamAPI_ISteamGameServer_GetServerReputation,'SteamAPI_ISteamGameServer_GetServerReputation');
 LoadEntryPoint(SteamAPI_ISteamGameServer_GetPublicIP,'SteamAPI_ISteamGameServer_GetPublicIP');
 LoadEntryPoint(SteamAPI_ISteamGameServer_HandleIncomingPacket,'SteamAPI_ISteamGameServer_HandleIncomingPacket');
 LoadEntryPoint(SteamAPI_ISteamGameServer_GetNextOutgoingPacket,'SteamAPI_ISteamGameServer_GetNextOutgoingPacket');
 LoadEntryPoint(SteamAPI_ISteamGameServer_AssociateWithClan,'SteamAPI_ISteamGameServer_AssociateWithClan');
 LoadEntryPoint(SteamAPI_ISteamGameServer_ComputeNewPlayerCompatibility,'SteamAPI_ISteamGameServer_ComputeNewPlayerCompatibility');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SendUserConnectAndAuthenticate_DEPRECATED,'SteamAPI_ISteamGameServer_SendUserConnectAndAuthenticate_DEPRECATED');
 LoadEntryPoint(SteamAPI_ISteamGameServer_CreateUnauthenticatedUserConnection,'SteamAPI_ISteamGameServer_CreateUnauthenticatedUserConnection');
 LoadEntryPoint(SteamAPI_ISteamGameServer_SendUserDisconnect_DEPRECATED,'SteamAPI_ISteamGameServer_SendUserDisconnect_DEPRECATED');
 LoadEntryPoint(SteamAPI_ISteamGameServer_BUpdateUserData,'SteamAPI_ISteamGameServer_BUpdateUserData');

 // ISteamGameServerStats
 LoadEntryPoint(SteamAPI_SteamGameServerStats_v001,'SteamAPI_SteamGameServerStats_v001');
 LoadEntryPoint(SteamAPI_ISteamGameServerStats_RequestUserStats,'SteamAPI_ISteamGameServerStats_RequestUserStats');
 LoadEntryPoint(SteamAPI_ISteamGameServerStats_GetUserStatInt32,'SteamAPI_ISteamGameServerStats_GetUserStatInt32');
 LoadEntryPoint(SteamAPI_ISteamGameServerStats_GetUserStatFloat,'SteamAPI_ISteamGameServerStats_GetUserStatFloat');
 LoadEntryPoint(SteamAPI_ISteamGameServerStats_GetUserAchievement,'SteamAPI_ISteamGameServerStats_GetUserAchievement');
 LoadEntryPoint(SteamAPI_ISteamGameServerStats_SetUserStatInt32,'SteamAPI_ISteamGameServerStats_SetUserStatInt32');
 LoadEntryPoint(SteamAPI_ISteamGameServerStats_SetUserStatFloat,'SteamAPI_ISteamGameServerStats_SetUserStatFloat');
 LoadEntryPoint(SteamAPI_ISteamGameServerStats_UpdateUserAvgRateStat,'SteamAPI_ISteamGameServerStats_UpdateUserAvgRateStat');
 LoadEntryPoint(SteamAPI_ISteamGameServerStats_SetUserAchievement,'SteamAPI_ISteamGameServerStats_SetUserAchievement');
 LoadEntryPoint(SteamAPI_ISteamGameServerStats_ClearUserAchievement,'SteamAPI_ISteamGameServerStats_ClearUserAchievement');
 LoadEntryPoint(SteamAPI_ISteamGameServerStats_StoreUserStats,'SteamAPI_ISteamGameServerStats_StoreUserStats');

 // ISteamNetworkingFakeUDPPort
 LoadEntryPoint(SteamAPI_ISteamNetworkingFakeUDPPort_DestroyFakeUDPPort,'SteamAPI_ISteamNetworkingFakeUDPPort_DestroyFakeUDPPort');
 LoadEntryPoint(SteamAPI_ISteamNetworkingFakeUDPPort_SendMessageToFakeIP,'SteamAPI_ISteamNetworkingFakeUDPPort_SendMessageToFakeIP');
 LoadEntryPoint(SteamAPI_ISteamNetworkingFakeUDPPort_ReceiveMessages,'SteamAPI_ISteamNetworkingFakeUDPPort_ReceiveMessages');
 LoadEntryPoint(SteamAPI_ISteamNetworkingFakeUDPPort_ScheduleCleanup,'SteamAPI_ISteamNetworkingFakeUDPPort_ScheduleCleanup');

 // SteamIPAddress_t
 LoadEntryPoint(SteamAPI_SteamIPAddress_t_IsSet,'SteamAPI_SteamIPAddress_t_IsSet');

 // MatchMakingKeyValuePair_t
 LoadEntryPoint(SteamAPI_MatchMakingKeyValuePair_t_Construct,'SteamAPI_MatchMakingKeyValuePair_t_Construct');

 // servernetadr_t
 LoadEntryPoint(SteamAPI_servernetadr_t_Construct,'SteamAPI_servernetadr_t_Construct');
 LoadEntryPoint(SteamAPI_servernetadr_t_Init,'SteamAPI_servernetadr_t_Init');
 LoadEntryPoint(SteamAPI_servernetadr_t_GetQueryPort,'SteamAPI_servernetadr_t_GetQueryPort');
 LoadEntryPoint(SteamAPI_servernetadr_t_SetQueryPort,'SteamAPI_servernetadr_t_SetQueryPort');
 LoadEntryPoint(SteamAPI_servernetadr_t_GetConnectionPort,'SteamAPI_servernetadr_t_GetConnectionPort');
 LoadEntryPoint(SteamAPI_servernetadr_t_SetConnectionPort,'SteamAPI_servernetadr_t_SetConnectionPort');
 LoadEntryPoint(SteamAPI_servernetadr_t_GetIP,'SteamAPI_servernetadr_t_GetIP');
 LoadEntryPoint(SteamAPI_servernetadr_t_SetIP,'SteamAPI_servernetadr_t_SetIP');
 LoadEntryPoint(SteamAPI_servernetadr_t_GetConnectionAddressString,'SteamAPI_servernetadr_t_GetConnectionAddressString');
 LoadEntryPoint(SteamAPI_servernetadr_t_GetQueryAddressString,'SteamAPI_servernetadr_t_GetQueryAddressString');
 LoadEntryPoint(SteamAPI_servernetadr_t_IsLessThan,'SteamAPI_servernetadr_t_IsLessThan');
 LoadEntryPoint(SteamAPI_servernetadr_t_Assign,'SteamAPI_servernetadr_t_Assign');

 // gameserveritem_t
 LoadEntryPoint(SteamAPI_gameserveritem_t_Construct,'SteamAPI_gameserveritem_t_Construct');
 LoadEntryPoint(SteamAPI_gameserveritem_t_GetName,'SteamAPI_gameserveritem_t_GetName');
 LoadEntryPoint(SteamAPI_gameserveritem_t_SetName,'SteamAPI_gameserveritem_t_SetName');

 // SteamNetworkingIPAddr
 LoadEntryPoint(SteamAPI_SteamNetworkingIPAddr_Clear,'SteamAPI_SteamNetworkingIPAddr_Clear');
 LoadEntryPoint(SteamAPI_SteamNetworkingIPAddr_IsIPv6AllZeros,'SteamAPI_SteamNetworkingIPAddr_IsIPv6AllZeros');
 LoadEntryPoint(SteamAPI_SteamNetworkingIPAddr_SetIPv6,'SteamAPI_SteamNetworkingIPAddr_SetIPv6');
 LoadEntryPoint(SteamAPI_SteamNetworkingIPAddr_SetIPv4,'SteamAPI_SteamNetworkingIPAddr_SetIPv4');
 LoadEntryPoint(SteamAPI_SteamNetworkingIPAddr_IsIPv4,'SteamAPI_SteamNetworkingIPAddr_IsIPv4');
 LoadEntryPoint(SteamAPI_SteamNetworkingIPAddr_GetIPv4,'SteamAPI_SteamNetworkingIPAddr_GetIPv4');
 LoadEntryPoint(SteamAPI_SteamNetworkingIPAddr_SetIPv6LocalHost,'SteamAPI_SteamNetworkingIPAddr_SetIPv6LocalHost');
 LoadEntryPoint(SteamAPI_SteamNetworkingIPAddr_IsLocalHost,'SteamAPI_SteamNetworkingIPAddr_IsLocalHost');
 LoadEntryPoint(SteamAPI_SteamNetworkingIPAddr_ToString,'SteamAPI_SteamNetworkingIPAddr_ToString');
 LoadEntryPoint(SteamAPI_SteamNetworkingIPAddr_ParseString,'SteamAPI_SteamNetworkingIPAddr_ParseString');
 LoadEntryPoint(SteamAPI_SteamNetworkingIPAddr_IsEqualTo,'SteamAPI_SteamNetworkingIPAddr_IsEqualTo');
 LoadEntryPoint(SteamAPI_SteamNetworkingIPAddr_GetFakeIPType,'SteamAPI_SteamNetworkingIPAddr_GetFakeIPType');
 LoadEntryPoint(SteamAPI_SteamNetworkingIPAddr_IsFakeIP,'SteamAPI_SteamNetworkingIPAddr_IsFakeIP');

 // SteamNetworkingIdentity
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_Clear,'SteamAPI_SteamNetworkingIdentity_Clear');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_IsInvalid,'SteamAPI_SteamNetworkingIdentity_IsInvalid');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_SetSteamID,'SteamAPI_SteamNetworkingIdentity_SetSteamID');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_GetSteamID,'SteamAPI_SteamNetworkingIdentity_GetSteamID');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_SetSteamID64,'SteamAPI_SteamNetworkingIdentity_SetSteamID64');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_GetSteamID64,'SteamAPI_SteamNetworkingIdentity_GetSteamID64');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_SetXboxPairwiseID,'SteamAPI_SteamNetworkingIdentity_SetXboxPairwiseID');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_GetXboxPairwiseID,'SteamAPI_SteamNetworkingIdentity_GetXboxPairwiseID');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_SetPSNID,'SteamAPI_SteamNetworkingIdentity_SetPSNID');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_GetPSNID,'SteamAPI_SteamNetworkingIdentity_GetPSNID');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_SetIPAddr,'SteamAPI_SteamNetworkingIdentity_SetIPAddr');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_GetIPAddr,'SteamAPI_SteamNetworkingIdentity_GetIPAddr');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_SetIPv4Addr,'SteamAPI_SteamNetworkingIdentity_SetIPv4Addr');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_GetIPv4,'SteamAPI_SteamNetworkingIdentity_GetIPv4');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_GetFakeIPType,'SteamAPI_SteamNetworkingIdentity_GetFakeIPType');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_IsFakeIP,'SteamAPI_SteamNetworkingIdentity_IsFakeIP');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_SetLocalHost,'SteamAPI_SteamNetworkingIdentity_SetLocalHost');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_IsLocalHost,'SteamAPI_SteamNetworkingIdentity_IsLocalHost');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_SetGenericString,'SteamAPI_SteamNetworkingIdentity_SetGenericString');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_GetGenericString,'SteamAPI_SteamNetworkingIdentity_GetGenericString');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_SetGenericBytes,'SteamAPI_SteamNetworkingIdentity_SetGenericBytes');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_GetGenericBytes,'SteamAPI_SteamNetworkingIdentity_GetGenericBytes');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_IsEqualTo,'SteamAPI_SteamNetworkingIdentity_IsEqualTo');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_ToString,'SteamAPI_SteamNetworkingIdentity_ToString');
 LoadEntryPoint(SteamAPI_SteamNetworkingIdentity_ParseString,'SteamAPI_SteamNetworkingIdentity_ParseString');

 // SteamNetworkingMessage_t
 LoadEntryPoint(SteamAPI_SteamNetworkingMessage_t_Release,'SteamAPI_SteamNetworkingMessage_t_Release');

 // SteamNetworkingConfigValue_t
 LoadEntryPoint(SteamAPI_SteamNetworkingConfigValue_t_SetInt32,'SteamAPI_SteamNetworkingConfigValue_t_SetInt32');
 LoadEntryPoint(SteamAPI_SteamNetworkingConfigValue_t_SetInt64,'SteamAPI_SteamNetworkingConfigValue_t_SetInt64');
 LoadEntryPoint(SteamAPI_SteamNetworkingConfigValue_t_SetFloat,'SteamAPI_SteamNetworkingConfigValue_t_SetFloat');
 LoadEntryPoint(SteamAPI_SteamNetworkingConfigValue_t_SetPtr,'SteamAPI_SteamNetworkingConfigValue_t_SetPtr');
 LoadEntryPoint(SteamAPI_SteamNetworkingConfigValue_t_SetString,'SteamAPI_SteamNetworkingConfigValue_t_SetString');

 // SteamDatagramHostedAddress
 LoadEntryPoint(SteamAPI_SteamDatagramHostedAddress_Clear,'SteamAPI_SteamDatagramHostedAddress_Clear');
 LoadEntryPoint(SteamAPI_SteamDatagramHostedAddress_GetPopID,'SteamAPI_SteamDatagramHostedAddress_GetPopID');
 LoadEntryPoint(SteamAPI_SteamDatagramHostedAddress_SetDevAddress,'SteamAPI_SteamDatagramHostedAddress_SetDevAddress');

 // Free standing entry points
 LoadEntryPoint(SteamInternal_SteamAPI_Init,'SteamInternal_SteamAPI_Init');
 LoadEntryPoint(SteamAPI_InitFlat,'SteamAPI_InitFlat');
 LoadEntryPoint(SteamAPI_InitSafe,'SteamAPI_InitSafe');
 LoadEntryPoint(SteamAPI_Shutdown,'SteamAPI_Shutdown');
 LoadEntryPoint(SteamAPI_RestartAppIfNecessary,'SteamAPI_RestartAppIfNecessary');
 LoadEntryPoint(SteamAPI_IsSteamRunning,'SteamAPI_IsSteamRunning');
 LoadEntryPoint(SteamAPI_GetSteamInstallPath,'SteamAPI_GetSteamInstallPath');
 LoadEntryPoint(SteamAPI_ReleaseCurrentThreadMemory,'SteamAPI_ReleaseCurrentThreadMemory');
 LoadEntryPoint(SteamAPI_GetHSteamPipe,'SteamAPI_GetHSteamPipe');
 LoadEntryPoint(SteamAPI_GetHSteamUser,'SteamAPI_GetHSteamUser');
 LoadEntryPoint(SteamAPI_RunCallbacks,'SteamAPI_RunCallbacks');
 LoadEntryPoint(SteamAPI_SetTryCatchCallbacks,'SteamAPI_SetTryCatchCallbacks');
 LoadEntryPoint(SteamAPI_ManualDispatch_Init,'SteamAPI_ManualDispatch_Init');
 LoadEntryPoint(SteamAPI_ManualDispatch_RunFrame,'SteamAPI_ManualDispatch_RunFrame');
 LoadEntryPoint(SteamAPI_ManualDispatch_GetNextCallback,'SteamAPI_ManualDispatch_GetNextCallback');
 LoadEntryPoint(SteamAPI_ManualDispatch_FreeLastCallback,'SteamAPI_ManualDispatch_FreeLastCallback');
 LoadEntryPoint(SteamAPI_ManualDispatch_GetAPICallResult,'SteamAPI_ManualDispatch_GetAPICallResult');
 LoadEntryPoint(SteamAPI_RegisterCallback,'SteamAPI_RegisterCallback');
 LoadEntryPoint(SteamAPI_UnregisterCallback,'SteamAPI_UnregisterCallback');
 LoadEntryPoint(SteamAPI_RegisterCallResult,'SteamAPI_RegisterCallResult');
 LoadEntryPoint(SteamAPI_UnregisterCallResult,'SteamAPI_UnregisterCallResult');
 LoadEntryPoint(SteamClient,'SteamClient');
 LoadEntryPoint(SteamInternal_ContextInit,'SteamInternal_ContextInit');
 LoadEntryPoint(SteamInternal_CreateInterface,'SteamInternal_CreateInterface');
 LoadEntryPoint(SteamInternal_FindOrCreateUserInterface,'SteamInternal_FindOrCreateUserInterface');
 LoadEntryPoint(SteamInternal_FindOrCreateGameServerInterface,'SteamInternal_FindOrCreateGameServerInterface');
 LoadEntryPoint(SteamInternal_GameServer_Init_V2,'SteamInternal_GameServer_Init_V2');
 LoadEntryPoint(SteamGameServer_Shutdown,'SteamGameServer_Shutdown');
 LoadEntryPoint(SteamGameServer_RunCallbacks,'SteamGameServer_RunCallbacks');
 LoadEntryPoint(SteamGameServer_BSecure,'SteamGameServer_BSecure');
 LoadEntryPoint(SteamGameServer_GetSteamID,'SteamGameServer_GetSteamID');
 LoadEntryPoint(SteamGameServer_GetHSteamPipe,'SteamGameServer_GetHSteamPipe');
 LoadEntryPoint(SteamGameServer_GetHSteamUser,'SteamGameServer_GetHSteamUser');
 LoadEntryPoint(SteamAPI_SetBreakpadAppID,'SteamAPI_SetBreakpadAppID');
 LoadEntryPoint(SteamAPI_SetMiniDumpComment,'SteamAPI_SetMiniDumpComment');
 LoadEntryPoint(SteamAPI_UseBreakpadCrashHandler,'SteamAPI_UseBreakpadCrashHandler');
 LoadEntryPoint(SteamAPI_WriteMiniDump,'SteamAPI_WriteMiniDump');

 // A missing entry point means the loaded library is older than these bindings.
 result:=CountMissingEntryPoints=0;
 if not result then begin
  UnloadSteamworksLibrary;
 end;

end;

procedure UnloadSteamworksLibrary;
begin
 if assigned(SteamworksLibraryHandle) then begin
  SteamworksFreeLibrary(SteamworksLibraryHandle);
  SteamworksLibraryHandle:=nil;
 end;
end;

function LoadSteamworksEncryptedAppTicketLibrary(const aLibraryName:string=STEAMWORKS_ENCRYPTED_APP_TICKET_DEFAULT_LIB_NAME):boolean;
var CountMissingEntryPoints:TSteamInt32;
 procedure LoadEntryPoint(out aTarget;const aName:string);
 begin
  TSteamPointer(aTarget):=SteamworksGetProcAddress(SteamworksEncryptedAppTicketLibraryHandle,aName);
  if not assigned(TSteamPointer(aTarget)) then begin
   inc(CountMissingEntryPoints);
  end;
 end;
begin

 if assigned(SteamworksEncryptedAppTicketLibraryHandle) then begin
  result:=true;
  exit;
 end;

 SteamworksEncryptedAppTicketLibraryHandle:=SteamworksLoadLibrary(aLibraryName);
 if not assigned(SteamworksEncryptedAppTicketLibraryHandle) then begin
  result:=false;
  exit;
 end;

 CountMissingEntryPoints:=0;

 LoadEntryPoint(SteamEncryptedAppTicket_BDecryptTicket,'SteamEncryptedAppTicket_BDecryptTicket');
 LoadEntryPoint(SteamEncryptedAppTicket_BIsTicketForApp,'SteamEncryptedAppTicket_BIsTicketForApp');
 LoadEntryPoint(SteamEncryptedAppTicket_GetTicketIssueTime,'SteamEncryptedAppTicket_GetTicketIssueTime');
 LoadEntryPoint(SteamEncryptedAppTicket_GetTicketSteamID,'SteamEncryptedAppTicket_GetTicketSteamID');
 LoadEntryPoint(SteamEncryptedAppTicket_GetTicketAppID,'SteamEncryptedAppTicket_GetTicketAppID');
 LoadEntryPoint(SteamEncryptedAppTicket_BUserOwnsAppInTicket,'SteamEncryptedAppTicket_BUserOwnsAppInTicket');
 LoadEntryPoint(SteamEncryptedAppTicket_BUserIsVacBanned,'SteamEncryptedAppTicket_BUserIsVacBanned');
 LoadEntryPoint(SteamEncryptedAppTicket_BGetAppDefinedValue,'SteamEncryptedAppTicket_BGetAppDefinedValue');
 LoadEntryPoint(SteamEncryptedAppTicket_GetUserVariableData,'SteamEncryptedAppTicket_GetUserVariableData');
 LoadEntryPoint(SteamEncryptedAppTicket_BIsTicketSigned,'SteamEncryptedAppTicket_BIsTicketSigned');
 LoadEntryPoint(SteamEncryptedAppTicket_BIsLicenseBorrowed,'SteamEncryptedAppTicket_BIsLicenseBorrowed');
 LoadEntryPoint(SteamEncryptedAppTicket_BIsLicenseTemporary,'SteamEncryptedAppTicket_BIsLicenseTemporary');

 result:=CountMissingEntryPoints=0;
 if not result then begin
  UnloadSteamworksEncryptedAppTicketLibrary;
 end;

end;

procedure UnloadSteamworksEncryptedAppTicketLibrary;
begin
 if assigned(SteamworksEncryptedAppTicketLibraryHandle) then begin
  SteamworksFreeLibrary(SteamworksEncryptedAppTicketLibraryHandle);
  SteamworksEncryptedAppTicketLibraryHandle:=nil;
 end;
end;

end.
