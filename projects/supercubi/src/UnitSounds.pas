unit UnitSounds;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}

{$ifdef fpc}
 {$optimization level1}
{$ifend}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasMP,
     PasJSON,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Math.Double,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Collections,
     PasVulkan.Audio,
     PasVulkan.Resources,
     PasVulkan.FrameGraph,
     PasVulkan.TimerQuery,
     PasVulkan.Scene,
     PasVulkan.Scene3D,
     PasVulkan.Scene3D.Renderer,
     PasVulkan.Scene3D.Renderer.Instance,
     PasVulkan.Scene3D.Renderer.EnvironmentCubeMap,
     PasVulkan.EntityComponentSystem,
     POCA,
     PasVulkan.POCA;

type TSoundManager=class
      public
       type TSound=class;
            TSounds=TpvObjectGenericList<TSound>;
            TSoundExistHashList=TpvHashMap<TSound,Boolean>;
            TSoundHashList=TpvStringHashMap<TSound>;
            { TSound }
            TSound=class
             private
              fSoundManager:TSoundManager;
              fGame:TObject;
              fIndex:TpvSizeInt;
              fUID:TPasMPUInt64;
              fUIDString:TpvUTF8String;
              fName:TpvUTF8String;
              fFileName:TpvUTF8String;
              fSoundSample:TpvAudioSoundSample;
              fReady:TPasMPBool32;
              fPolyphony:TpvInt32;
              fLoop:TpvInt32;
              fRealVoices:TpvInt32;
              fFadeOutDuration:TpvDouble;
              fPOCAInstanceValue:TPOCAValue;
              fUIDPOCAKeyValue:TPOCAValue;
              fReferenceCounter:TpvInt32;
             public
              constructor Create(const aSounds:TSoundManager;const aName,aFileName:TpvUTF8String;const aPolyphony:TpvInt32;const aLoop,aRealVoices:TpvInt32;const aFadeOutDuration:TpvDouble); reintroduce;
              destructor Destroy; override;
              procedure AfterConstruction; override;
              procedure BeforeDestruction; override;
              procedure IncRef;
              procedure DecRef;
              procedure BackgroundLoad;
              procedure Reload;
              procedure UpdateAudio;
             public
              function Play(Volume,Panning,Rate:TpvFloat;VoiceIndexPointer:TpvPointer=nil):TpvID;
              function PlaySpatialization(Volume,Panning,Rate:TpvFloat;Spatialization:LongBool;const Position,Velocity:TpvVector3;const Local:LongBool=false;const VoiceIndexPointer:TpvPointer=nil):TpvID;
              procedure Stop(GlobalVoiceID:TpvID);
              procedure KeyOff(GlobalVoiceID:TpvID);
              function SetVolume(GlobalVoiceID:TpvID;Volume:TpvFloat):TpvID;
              function SetPanning(GlobalVoiceID:TpvID;Panning:TpvFloat):TpvID;
              function SetRate(GlobalVoiceID:TpvID;Rate:TpvFloat):TpvID;
              function SetPosition(GlobalVoiceID:TpvID;Spatialization:LongBool;const Origin,Velocity:TpvVector3;const Local:LongBool=false):TpvID;
              function SetEffectMix(GlobalVoiceID:TpvID;Active:LongBool):TpvID;
              function IsPlaying:boolean;
              function IsVoicePlaying(GlobalVoiceID:TpvID):boolean;
              function SampleOnIntervalHook(const aSampleVoice:TpvAudioSoundSampleVoice;const aDeltaSamples:TpvInt32):boolean;
             public
              property UID:TPasMPUInt64 read fUID;
              property Ready:TPasMPBool32 read fReady write fReady;
              property Polyphony:TpvInt32 read fPolyphony write fPolyphony;
              property Loop:TpvInt32 read fLoop write fLoop;
              property RealVoices:TpvInt32 read fRealVoices write fRealVoices;
              property SoundSample:TpvAudioSoundSample read fSoundSample;
              property POCAInstanceValue:TPOCAValue read fPOCAInstanceValue;
             published
            end;
            TUIDFreeList=TpvDynamicFastStack<TPasMPUInt64>;
      private
       fGame:TObject;
       fSounds:TSounds;
       fSoundExistHashList:TSoundExistHashList;
       fSoundHashList:TSoundHashList;
       fLock:TPasMPCriticalSection;
       fUIDCounter:TPasMPUInt64;
       fUIDFreeListLock:TPasMPCriticalSection;
       fUIDFreeList:TUIDFreeList;
       fPOCASubContext:PPOCAContext;
       fPOCASoundHash:TPOCAValue;
       fPOCASoundGhostHash:TPOCAValue;
      public
       constructor Create(const aGame:TObject); reintroduce;
       destructor Destroy; override;
       procedure BackgroundLoad;
       procedure UpdateAudio;
       function Add(const aName,aFileName:TpvUTF8String;const aPolyphony:TpvInt32=1;const aLoop:TpvInt32=1;const aRealVoices:TpvInt32=-1;const aFadeOutDuration:TpvDouble=0.0):TSound;
       function Find(const aName:TpvUTF8String):TSound;
       procedure Remove(const aSound:TSound); overload;
       procedure Remove(const aName:TpvUTF8String); overload;
       procedure Clear;
      public
     end;

var GameSounds:TSoundManager=nil;

implementation

uses UnitApplication,
     UnitScreenMain;

// POCA API

const POCASoundGhost:TPOCAGhostType=
       (
        Destroy:nil;
        CanDestroy:nil;
        Mark:nil;
        ExistKey:nil;
        GetKey:nil;
        SetKey:nil;
        Name:'Sound'
       );

function POCASoundsCreate(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var ArgumentIndex:TPOCAInt32;
    Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
    Name,FileName:TpvUTF8String;
    Polyphony,Loop,RealVoices:TpvInt32;
    FadeOutDuration:TpvDouble;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) then begin
  Sounds:=TSoundManager(aUserData);
  ArgumentIndex:=0;
  if ArgumentIndex<aCountArguments then begin
   Name:=POCAGetStringValue(aContext,aArguments[ArgumentIndex]);
   inc(ArgumentIndex);
  end else begin
   Name:='';
  end;
  if ArgumentIndex<aCountArguments then begin
   FileName:=POCAGetStringValue(aContext,aArguments[ArgumentIndex]);
   inc(ArgumentIndex);
  end else begin
   FileName:='';
  end;
  if ArgumentIndex<aCountArguments then begin
   Polyphony:=trunc(POCAGetNumberValue(aContext,aArguments[ArgumentIndex]));
   inc(ArgumentIndex);
  end else begin
   Polyphony:=1;
  end;
  if ArgumentIndex<aCountArguments then begin
   Loop:=trunc(POCAGetNumberValue(aContext,aArguments[ArgumentIndex]));
   inc(ArgumentIndex);
  end else begin
   Loop:=1;
  end;
  if ArgumentIndex<aCountArguments then begin
   RealVoices:=trunc(POCAGetNumberValue(aContext,aArguments[ArgumentIndex]));
   inc(ArgumentIndex);
  end else begin
   RealVoices:=-1;
  end;
  if ArgumentIndex<aCountArguments then begin
   FadeOutDuration:=POCAGetNumberValue(aContext,aArguments[ArgumentIndex]);
   inc(ArgumentIndex);
  end else begin
   FadeOutDuration:=0.0;
  end;
  if assigned(Sounds) then begin
   Sound:=Sounds.Add(Name,FileName,Polyphony,Loop,RealVoices,FadeOutDuration);
   if assigned(Sound) then begin
    Sound.BackgroundLoad;
    result:=Sound.fPOCAInstanceValue;
   end else begin
    result.CastedUInt64:=POCAValueNullCastedUInt64;
   end;
  end else begin
   result.CastedUInt64:=POCAValueNullCastedUInt64;
  end;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundsFind(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var ArgumentIndex:TPOCAInt32;
    Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
    Name:TpvUTF8String;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) then begin
  Sounds:=TSoundManager(aUserData);
  ArgumentIndex:=0;
  if ArgumentIndex<aCountArguments then begin
   Name:=POCAGetStringValue(aContext,aArguments[ArgumentIndex]);
   inc(ArgumentIndex);
  end else begin
   Name:='';
  end;
  if assigned(Sounds) then begin
   Sound:=Sounds.Find(Name);
   if assigned(Sound) then begin
    result:=Sound.fPOCAInstanceValue;
   end else begin
    result.CastedUInt64:=POCAValueNullCastedUInt64;
   end;
  end else begin
   result.CastedUInt64:=POCAValueNullCastedUInt64;
  end;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundsRemove(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var ArgumentIndex:TPOCAInt32;
    Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
    Name:TpvUTF8String;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) then begin
  Sounds:=TSoundManager(aUserData);
  ArgumentIndex:=0;
  if ArgumentIndex<aCountArguments then begin
   if POCAGhostGetType(aArguments[ArgumentIndex])=@POCASoundGhost then begin
    Sound:=POCAGhostFastGetPointer(aArguments[ArgumentIndex]);
    inc(ArgumentIndex);
   end else begin
    Name:=POCAGetStringValue(aContext,aArguments[ArgumentIndex]);
    inc(ArgumentIndex);
    if assigned(Sounds) then begin
     Sound:=Sounds.Find(Name);
    end else begin
     Sound:=nil;
    end;
   end;
  end else begin
   Sound:=nil;
  end;
  if assigned(Sounds) and assigned(Sound) then begin
   Sounds.Remove(Sound);
  end;
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundsClear(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) then begin
  TSoundManager(aUserData).Clear;
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashValid(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  result.Num:=ord(Sounds.fSoundExistHashList.ExistKey(Sound)) and 1;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashDestroy(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   Sound.free;
   result.Num:=1.0;
  end else begin
   result.CastedUInt64:=POCAValueNullCastedUInt64;
  end;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashReload(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
  Sounds:=TSoundManager(aUserData);
  if assigned(Sounds) then begin
   Sound:=POCAGhostFastGetPointer(aThis);
   if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
    Sound.Reload;
    result:=Sound.fPOCAInstanceValue;
   end;
  end;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashGetName(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   result:=POCANewString(Sounds.fPOCASubContext,Sound.fName);
  end else begin
   result.CastedUInt64:=POCAValueNullCastedUInt64;
  end;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashGetUID(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   if Sound.fUID<=TpvUInt64($001fffffffffffff) then begin
    result:=POCANewNumber(Sounds.fPOCASubContext,Sound.fUID);
   end else begin
    result:=POCANewUniqueString(Sounds.fPOCASubContext,Sound.fUIDString);
   end;
  end else begin
   result.CastedUInt64:=POCAValueNullCastedUInt64;
  end;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashGetFileName(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   result:=POCANewString(Sounds.fPOCASubContext,Sound.fFileName);
  end else begin
   result.CastedUInt64:=POCAValueNullCastedUInt64;
  end;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashGetPolyphony(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   result:=POCANewNumber(Sounds.fPOCASubContext,Sound.fPolyphony);
  end else begin
   result.CastedUInt64:=POCAValueNullCastedUInt64;
  end;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashGetLoop(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   result:=POCANewNumber(Sounds.fPOCASubContext,Sound.fLoop);
  end else begin
   result.CastedUInt64:=POCAValueNullCastedUInt64;
  end;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashGetRealVoices(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   result:=POCANewNumber(Sounds.fPOCASubContext,Sound.fRealVoices);
  end else begin
   result.CastedUInt64:=POCAValueNullCastedUInt64;
  end;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashGetFadeOutDuration(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   result:=POCANewNumber(Sounds.fPOCASubContext,Sound.fFadeOutDuration);
  end else begin
   result.CastedUInt64:=POCAValueNullCastedUInt64;
  end;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashPlay(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var ArgumentIndex:TPOCAInt32;
    Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
    Volume,Panning,Rate:TpvFloat;
    VoiceIndexPointer:TpvPointer;
    VoiceID:TpvID;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   Volume:=1.0;
   Panning:=0.0;
   Rate:=1.0;
   VoiceIndexPointer:=nil;
   ArgumentIndex:=0;
   if ArgumentIndex<aCountArguments then begin
    Volume:=POCAGetNumberValue(aContext,aArguments[ArgumentIndex]);
    inc(ArgumentIndex);
   end;
   if ArgumentIndex<aCountArguments then begin
    Panning:=POCAGetNumberValue(aContext,aArguments[ArgumentIndex]);
    inc(ArgumentIndex);
   end;
   if ArgumentIndex<aCountArguments then begin
    Rate:=POCAGetNumberValue(aContext,aArguments[ArgumentIndex]);
    inc(ArgumentIndex);
   end;
   VoiceIndexPointer:=nil;
   VoiceID:=Sound.Play(Volume,Panning,Rate,VoiceIndexPointer);
   if VoiceID<=$001fffffffffffff then begin
    result:=POCANewNumber(aContext,VoiceID);
   end else begin
    result:=POCANewUniqueString(aContext,UIntToStr(VoiceID));
   end;
  end else begin
   result.CastedUInt64:=POCAValueNullCastedUInt64;
  end;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashPlaySpatialization(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var ArgumentIndex:TPOCAInt32;
    Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
    Volume,Panning,Rate:TpvFloat;
    Spatialization:LongBool;
    Position,Velocity:TpvVector3D;
    Local:LongBool;
    VoiceIndexPointer:TpvPointer;
    VoiceID:TpvID;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   Volume:=1.0;
   Panning:=0.0;
   Rate:=1.0;
   Spatialization:=false;
   Position:=TpvVector3.Origin;
   Velocity:=TpvVector3.Origin;
   Local:=false;
   VoiceIndexPointer:=nil;
   ArgumentIndex:=0;
   if ArgumentIndex<aCountArguments then begin
    Volume:=POCAGetNumberValue(aContext,aArguments[ArgumentIndex]);
    inc(ArgumentIndex);
   end;
   if ArgumentIndex<aCountArguments then begin
    Panning:=POCAGetNumberValue(aContext,aArguments[ArgumentIndex]);
    inc(ArgumentIndex);
   end;
   if ArgumentIndex<aCountArguments then begin
    Rate:=POCAGetNumberValue(aContext,aArguments[ArgumentIndex]);
    inc(ArgumentIndex);
   end;
   if ArgumentIndex<aCountArguments then begin
    Spatialization:=(POCAGetNumberValue(aContext,aArguments[ArgumentIndex])<>0);
    inc(ArgumentIndex);
   end;
   if Spatialization then begin
    if ArgumentIndex<aCountArguments then begin
     Position:=POCAGetVector3Value(aArguments[ArgumentIndex]);
     inc(ArgumentIndex);
    end else begin
     Position:=TpvVector3.Origin;
    end;
    if ArgumentIndex<aCountArguments then begin
     Velocity:=POCAGetVector3Value(aArguments[ArgumentIndex]);
     inc(ArgumentIndex);
    end else begin
     Velocity:=TpvVector3.Origin;
    end;
   end else begin
    Position:=TpvVector3.Origin;
    Velocity:=TpvVector3.Origin;
   end;
   if ArgumentIndex<aCountArguments then begin
    Local:=POCAGetBooleanValue(aContext,aArguments[ArgumentIndex]);
    inc(ArgumentIndex);
   end else begin
    Local:=false;
   end;
   VoiceID:=Sound.PlaySpatialization(Volume,Panning,Rate,Spatialization,Position,Velocity,Local,VoiceIndexPointer);
   if VoiceID<=$001fffffffffffff then begin
    result:=POCANewNumber(aContext,VoiceID);
   end else begin
    result:=POCANewUniqueString(aContext,UIntToStr(VoiceID));
   end;
  end else begin
   result.CastedUInt64:=POCAValueNullCastedUInt64;
  end;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashStop(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var ArgumentIndex:TPOCAInt32;
    Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
    GlobalVoiceID:TpvID;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   ArgumentIndex:=0;
   if ArgumentIndex<aCountArguments then begin
    if POCAGetValueType(aArguments[ArgumentIndex])=pvtNumber then begin
     GlobalVoiceID:=trunc(POCAGetNumberValue(aContext,aArguments[ArgumentIndex]));
    end else begin
     GlobalVoiceID:=StrToUInt64Def(POCAGetStringValue(aContext,aArguments[ArgumentIndex]),0);
    end;
    inc(ArgumentIndex);
   end else begin
    GlobalVoiceID:=0;
   end;
   Sound.Stop(GlobalVoiceID);
  end;
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashKeyOff(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var ArgumentIndex:TPOCAInt32;
    Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
    GlobalVoiceID:TpvID;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   ArgumentIndex:=0;
   if ArgumentIndex<aCountArguments then begin
    if POCAGetValueType(aArguments[ArgumentIndex])=pvtNumber then begin
     GlobalVoiceID:=trunc(POCAGetNumberValue(aContext,aArguments[ArgumentIndex]));
    end else begin
     GlobalVoiceID:=StrToUInt64Def(POCAGetStringValue(aContext,aArguments[ArgumentIndex]),0);
    end;
    inc(ArgumentIndex);
   end else begin
    GlobalVoiceID:=0;
   end;
   Sound.KeyOff(GlobalVoiceID);
  end;
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashSetVolume(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var ArgumentIndex:TPOCAInt32;
    Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
    GlobalVoiceID:TpvID;
    Volume:TpvFloat;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   ArgumentIndex:=0;
   if ArgumentIndex<aCountArguments then begin
    if POCAGetValueType(aArguments[ArgumentIndex])=pvtNumber then begin
     GlobalVoiceID:=trunc(POCAGetNumberValue(aContext,aArguments[ArgumentIndex]));
    end else begin
     GlobalVoiceID:=StrToUInt64Def(POCAGetStringValue(aContext,aArguments[ArgumentIndex]),0);
    end;
    inc(ArgumentIndex);
   end else begin
    GlobalVoiceID:=0;
   end;
   if ArgumentIndex<aCountArguments then begin
    Volume:=POCAGetNumberValue(aContext,aArguments[ArgumentIndex]);
    inc(ArgumentIndex);
   end else begin
    Volume:=1.0;
   end;
   Sound.SetVolume(GlobalVoiceID,Volume);
  end;
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashSetPanning(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var ArgumentIndex:TPOCAInt32;
    Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
    GlobalVoiceID:TpvID;
    Panning:TpvFloat;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   ArgumentIndex:=0;
   if ArgumentIndex<aCountArguments then begin
    if POCAGetValueType(aArguments[ArgumentIndex])=pvtNumber then begin
     GlobalVoiceID:=trunc(POCAGetNumberValue(aContext,aArguments[ArgumentIndex]));
    end else begin
     GlobalVoiceID:=StrToUInt64Def(POCAGetStringValue(aContext,aArguments[ArgumentIndex]),0);
    end;
    inc(ArgumentIndex);
   end else begin
    GlobalVoiceID:=0;
   end;
   if ArgumentIndex<aCountArguments then begin
    Panning:=POCAGetNumberValue(aContext,aArguments[ArgumentIndex]);
    inc(ArgumentIndex);
   end else begin
    Panning:=0.0;
   end;
   Sound.SetPanning(GlobalVoiceID,Panning);
  end;
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashSetRate(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var ArgumentIndex:TPOCAInt32;
    Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
    GlobalVoiceID:TpvID;
    Rate:TpvFloat;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   ArgumentIndex:=0;
   if ArgumentIndex<aCountArguments then begin
    if POCAGetValueType(aArguments[ArgumentIndex])=pvtNumber then begin
     GlobalVoiceID:=trunc(POCAGetNumberValue(aContext,aArguments[ArgumentIndex]));
    end else begin
     GlobalVoiceID:=StrToUInt64Def(POCAGetStringValue(aContext,aArguments[ArgumentIndex]),0);
    end;
    inc(ArgumentIndex);
   end else begin
    GlobalVoiceID:=0;
   end;
   if ArgumentIndex<aCountArguments then begin
    Rate:=POCAGetNumberValue(aContext,aArguments[ArgumentIndex]);
    inc(ArgumentIndex);
   end else begin
    Rate:=1.0;
   end;
   Sound.SetRate(GlobalVoiceID,Rate);
  end;
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashSetPosition(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var ArgumentIndex:TPOCAInt32;
    Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
    GlobalVoiceID:TpvID;
    Spatialization:LongBool;
    Origin:TpvVector3D;
    Velocity:TpvVector3D;
    Local:LongBool;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   ArgumentIndex:=0;
   if ArgumentIndex<aCountArguments then begin
    if POCAGetValueType(aArguments[ArgumentIndex])=pvtNumber then begin
     GlobalVoiceID:=trunc(POCAGetNumberValue(aContext,aArguments[ArgumentIndex]));
    end else begin
     GlobalVoiceID:=StrToUInt64Def(POCAGetStringValue(aContext,aArguments[ArgumentIndex]),0);
    end;
    inc(ArgumentIndex);
   end else begin
    GlobalVoiceID:=0;
   end;
   if ArgumentIndex<aCountArguments then begin
    Spatialization:=POCAGetBooleanValue(aContext,aArguments[ArgumentIndex]);
    inc(ArgumentIndex);
   end else begin
    Spatialization:=false;
   end;
   if Spatialization then begin
    if ArgumentIndex<aCountArguments then begin
     Origin:=POCAGetVector3Value(aArguments[ArgumentIndex]);
     inc(ArgumentIndex);
    end else begin
     Origin:=TpvVector3.Origin;
    end;
    if ArgumentIndex<aCountArguments then begin
     Velocity:=POCAGetVector3Value(aArguments[ArgumentIndex]);
     inc(ArgumentIndex);
    end else begin
     Velocity:=TpvVector3.Origin;
    end;
   end else begin
    Origin:=TpvVector3.Origin;
    Velocity:=TpvVector3.Origin;
   end;
   if ArgumentIndex<aCountArguments then begin
    Local:=POCAGetBooleanValue(aContext,aArguments[ArgumentIndex]);
    inc(ArgumentIndex);
   end else begin
    Local:=false;
   end;
   Sound.SetPosition(GlobalVoiceID,Spatialization,Origin,Velocity,Local);
  end;
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashSetEffectMix(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var ArgumentIndex:TPOCAInt32;
    Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
    GlobalVoiceID:TpvID;
    EffectMix:Boolean;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   ArgumentIndex:=0;
   if ArgumentIndex<aCountArguments then begin
    if POCAGetValueType(aArguments[ArgumentIndex])=pvtNumber then begin
     GlobalVoiceID:=trunc(POCAGetNumberValue(aContext,aArguments[ArgumentIndex]));
    end else begin
     GlobalVoiceID:=StrToUInt64Def(POCAGetStringValue(aContext,aArguments[ArgumentIndex]),0);
    end;
    inc(ArgumentIndex);
   end else begin
    GlobalVoiceID:=0;
   end;
   if ArgumentIndex<aCountArguments then begin
    EffectMix:=POCAGetBooleanValue(aContext,aArguments[ArgumentIndex]);
    inc(ArgumentIndex);
   end else begin
    EffectMix:=false;
   end;
   Sound.SetEffectMix(GlobalVoiceID,EffectMix);
  end;
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashIsPlaying(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var ArgumentIndex:TPOCAInt32;
    Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   result.Num:=ord(Sound.IsPlaying) and 1;
  end else begin
   result.CastedUInt64:=POCAValueNullCastedUInt64;
  end;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

function POCASoundHashIsVoicePlaying(aContext:PPOCAContext;const aThis:TPOCAValue;const aArguments:PPOCAValues;const aCountArguments:TPOCAInt32;const aUserData:TPOCAPointer):TPOCAValue;
var ArgumentIndex:TPOCAInt32;
    Sounds:TSoundManager;
    Sound:TSoundManager.TSound;
    GlobalVoiceID:TpvID;
begin
 if assigned(aUserData) and (TObject(aUserData) is TSoundManager) and (POCAGhostGetType(aThis)=@POCASoundGhost) then begin
  Sounds:=TSoundManager(aUserData);
  Sound:=POCAGhostFastGetPointer(aThis);
  if assigned(Sound) and Sounds.fSoundExistHashList.ExistKey(Sound) then begin
   ArgumentIndex:=0;
   if ArgumentIndex<aCountArguments then begin
    if POCAGetValueType(aArguments[ArgumentIndex])=pvtNumber then begin
     GlobalVoiceID:=trunc(POCAGetNumberValue(aContext,aArguments[ArgumentIndex]));
    end else begin
     GlobalVoiceID:=StrToUInt64Def(POCAGetStringValue(aContext,aArguments[ArgumentIndex]),0);
    end;
    inc(ArgumentIndex);
   end else begin
    GlobalVoiceID:=0;
   end;
   result.Num:=ord(Sound.IsVoicePlaying(GlobalVoiceID)) and 1;
  end else begin
   result.CastedUInt64:=POCAValueNullCastedUInt64;
  end;
 end else begin
  result.CastedUInt64:=POCAValueNullCastedUInt64;
 end;
end;

{ TSoundManager.TSound }

constructor TSoundManager.TSound.Create(const aSounds:TSoundManager;const aName,aFileName:TpvUTF8String;const aPolyphony:TpvInt32;const aLoop,aRealVoices:TpvInt32;const aFadeOutDuration:TpvDouble);
begin

 inherited Create;

 fSoundManager:=aSounds;

 fName:=aName;

 fFileName:=aFileName;

 fReady:=false;

 fPolyphony:=aPolyphony;

 fLoop:=aLoop;

 fRealVoices:=aRealVoices;

 fFadeOutDuration:=aFadeOutDuration;

 fReferenceCounter:=0;

 fSoundManager.fUIDFreeListLock.Acquire;
 try
  if not fSoundManager.fUIDFreeList.Pop(fUID) then begin
   repeat
    fUID:=TPasMPInterlocked.Increment(fSoundManager.fUIDCounter);
   until fUID<>0;
  end;
 finally
  fSoundManager.fUIDFreeListLock.Release;
 end;

end;

destructor TSoundManager.TSound.Destroy;
begin

 if assigned(pvApplication.Audio) then begin
  pvApplication.Audio.Lock;
  try
   if assigned(fSoundSample) then begin
    try
     fSoundSample.DecRef;
    finally
     fSoundSample:=nil;
    end;
   end;
  finally
   pvApplication.Audio.Unlock;
  end;
 end;

 if fUID<>0 then begin
  fSoundManager.fUIDFreeListLock.Acquire;
  try
   fSoundManager.fUIDFreeList.Push(fUID);
  finally
   fSoundManager.fUIDFreeListLock.Release;
  end;
 end;

 inherited Destroy;

end;

procedure TSoundManager.TSound.AfterConstruction;
begin
 inherited AfterConstruction;

 if assigned(fSoundManager) then begin

  fSoundManager.fLock.Acquire;
  try

   fIndex:=fSoundManager.fSounds.Add(self);
   fSoundManager.fSoundHashList.Add(fName,self);
   fSoundManager.fSoundExistHashList.Add(self,true);

   fPOCAInstanceValue:=POCANewGhost(fSoundManager.fPOCASubContext,@POCASoundGhost,self);
   POCAGhostSetHashValue(fPOCAInstanceValue,fSoundManager.fPOCASoundGhostHash);
   if fUID<=TpvUInt64($001fffffffffffff) then begin
    fUIDPOCAKeyValue:=POCANewNumber(fSoundManager.fPOCASubContext,fUID);
   end else begin
    fUIDPOCAKeyValue:=POCANewUniqueString(fSoundManager.fPOCASubContext,fUIDString);
   end;
   POCAHashSet(fSoundManager.fPOCASubContext,fSoundManager.fPOCASoundHash,fUIDPOCAKeyValue,fPOCAInstanceValue);

   //POCAResetTemporarySaves(fSounds.fPOCASubContext);

  finally
   fSoundManager.fLock.Release;
  end;

 end;
end;

procedure TSoundManager.TSound.BeforeDestruction;
var OtherSound:TSound;
begin
 if assigned(fSoundManager) and assigned(fSoundManager.fSounds) then begin

  fSoundManager.fLock.Acquire;
  try

   if not POCAIsValueNull(fPOCAInstanceValue) then begin
    POCAHashDelete(fSoundManager.fPOCASubContext,fSoundManager.fPOCASoundHash,fUIDPOCAKeyValue);
    fPOCAInstanceValue:=POCAValueNull;
   end;

   if fIndex>=0 then begin
    try
     if (fIndex+1)<fSoundManager.fSounds.Count then begin
      OtherSound:=fSoundManager.fSounds[fSoundManager.fSounds.Count-1];
      OtherSound.fIndex:=fIndex;
      fSoundManager.fSounds[fIndex]:=OtherSound;
      fIndex:=fSoundManager.fSounds.Count-1;
     end;
     fSoundManager.fSounds.Extract(fIndex);
    finally
     fIndex:=-1;
    end;
    if fSoundManager.fSoundHashList.ExistKey(fName) then begin
     fSoundManager.fSoundHashList.Delete(fName);
    end;
    if fSoundManager.fSoundExistHashList.ExistKey(self) then begin
     fSoundManager.fSoundExistHashList.Delete(self);
    end;
   end;

  finally
   fSoundManager.fLock.Release;
  end;

 end;

 inherited BeforeDestruction;
end;

procedure TSoundManager.TSound.IncRef;
begin
 if assigned(self) then begin
  TPasMPInterlocked.Increment(fReferenceCounter);
 end;
end;

procedure TSoundManager.TSound.DecRef;
begin
 if assigned(self) then begin
  if TPasMPInterlocked.Decrement(fReferenceCounter)=0 then begin
   Free;
  end;
 end;
end;

procedure TSoundManager.TSound.BackgroundLoad;
begin

 if assigned(pvApplication.Audio) then begin
  pvApplication.Audio.Lock;
  try
   if pvApplication.Assets.ExistAsset(fFileName) then begin
    fSoundSample:=pvApplication.Audio.Samples.Load(fName,pvApplication.Assets.GetAssetStream(fFileName),true,fPolyphony,fLoop,fRealVoices);
    fSoundSample.IncRef;
    if fFadeOutDuration>0.0 then begin
     fSoundSample.OnIntervalHook:=SampleOnIntervalHook;
    end;
   end else begin
    fSoundSample:=nil;
   end;
  finally
   pvApplication.Audio.Unlock;
  end;
 end;

end;

procedure TSoundManager.TSound.Reload;
begin
 if assigned(pvApplication.Audio) then begin
  pvApplication.Audio.Lock;
  try
   try
    fSoundSample.DecRef;
   finally
    fSoundSample:=nil;
   end;
   BackgroundLoad;
  finally
   pvApplication.Audio.Unlock;
  end;
 end;
end;

procedure TSoundManager.TSound.UpdateAudio;
begin
end;

function TSoundManager.TSound.Play(Volume,Panning,Rate:TpvFloat;VoiceIndexPointer:TpvPointer=nil):TpvID;
begin
 if assigned(pvApplication.Audio) and assigned(fSoundSample) then begin
  result:=pvApplication.Audio.CommandQueue.SampleVoicePlay(fSoundSample,Volume,Panning,Rate);
 end else begin
  result:=0;
 end;
end;

function TSoundManager.TSound.PlaySpatialization(Volume,Panning,Rate:TpvFloat;Spatialization:LongBool;const Position,Velocity:TpvVector3;const Local:LongBool=false;const VoiceIndexPointer:TpvPointer=nil):TpvID;
begin
 if assigned(pvApplication.Audio) and assigned(fSoundSample) then begin
  result:=pvApplication.Audio.CommandQueue.SampleVoicePlaySpatialization(fSoundSample,Volume,Panning,Rate,Spatialization,Position,Velocity,Local);
 end else begin
  result:=0;
 end;
end;

procedure TSoundManager.TSound.Stop(GlobalVoiceID:TpvID);
begin
 if assigned(pvApplication.Audio) and assigned(fSoundSample) and (GlobalVoiceID>0) then begin
  pvApplication.Audio.CommandQueue.SampleVoiceStop(GlobalVoiceID);
 end;
end;

procedure TSoundManager.TSound.KeyOff(GlobalVoiceID:TpvID);
begin
 if assigned(pvApplication.Audio) and assigned(fSoundSample) and (GlobalVoiceID>0) then begin
  pvApplication.Audio.CommandQueue.SampleVoiceKeyOff(GlobalVoiceID);
 end;
end;

function TSoundManager.TSound.SetVolume(GlobalVoiceID:TpvID;Volume:TpvFloat):TpvID;
begin
 if assigned(pvApplication.Audio) and assigned(fSoundSample) and (GlobalVoiceID>0) then begin
  pvApplication.Audio.CommandQueue.SampleVoiceSetVolume(GlobalVoiceID,Volume);
  result:=GlobalVoiceID;
 end else begin
  result:=0;
 end;
end;

function TSoundManager.TSound.SetPanning(GlobalVoiceID:TpvID;Panning:TpvFloat):TpvID;
begin
 if assigned(pvApplication.Audio) and assigned(fSoundSample) and (GlobalVoiceID>0) then begin
  pvApplication.Audio.CommandQueue.SampleVoiceSetPanning(GlobalVoiceID,Panning);
  result:=GlobalVoiceID;
 end else begin
  result:=0;
 end;
end;

function TSoundManager.TSound.SetRate(GlobalVoiceID:TpvID;Rate:TpvFloat):TpvID;
begin
 if assigned(pvApplication.Audio) and assigned(fSoundSample) and (GlobalVoiceID>0) then begin
  pvApplication.Audio.CommandQueue.SampleVoiceSetRate(GlobalVoiceID,Rate);
  result:=GlobalVoiceID;
 end else begin
  result:=0;
 end;
end;

function TSoundManager.TSound.SetPosition(GlobalVoiceID:TpvID;Spatialization:LongBool;const Origin,Velocity:TpvVector3;const Local:LongBool=false):TpvID;
begin
 if assigned(pvApplication.Audio) and assigned(fSoundSample) and (GlobalVoiceID>0) then begin
  pvApplication.Audio.CommandQueue.SampleVoiceSetPosition(GlobalVoiceID,Spatialization,Origin,Velocity,Local);
  result:=GlobalVoiceID;
 end else begin
  result:=0;
 end;
end;

function TSoundManager.TSound.SetEffectMix(GlobalVoiceID:TpvID;Active:LongBool):TpvID;
begin
 if assigned(pvApplication.Audio) and assigned(fSoundSample) and (GlobalVoiceID>0) then begin
  pvApplication.Audio.CommandQueue.SampleVoiceSetEffectMix(GlobalVoiceID,Active);
  result:=GlobalVoiceID;
 end else begin
  result:=0;
 end;
end;

function TSoundManager.TSound.IsPlaying:boolean;
begin
 if assigned(pvApplication.Audio) and assigned(fSoundSample) then begin
  pvApplication.Audio.Lock;
  try
   result:=fSoundSample.IsPlaying;
  finally
   pvApplication.Audio.Unlock;
  end;
 end else begin
  result:=false;
 end;
end;

function TSoundManager.TSound.IsVoicePlaying(GlobalVoiceID:TpvID):boolean;
begin
 if assigned(pvApplication.Audio) and assigned(fSoundSample) and (GlobalVoiceID>0) then begin
  result:=pvApplication.Audio.GlobalVoiceManager.CheckGlobalVoiceID(GlobalVoiceID);
 end else begin
  result:=false;
 end;
end;

function TSoundManager.TSound.SampleOnIntervalHook(const aSampleVoice:TpvAudioSoundSampleVoice;const aDeltaSamples:TpvInt32):boolean;
var v:PpvDouble;
begin
 result:=false;
 if assigned(aSampleVoice) then begin
  if aSampleVoice.KeyOff then begin
   v:=pointer(@aSampleVoice.OtherTag);
   if aSampleVoice.Tag=High(TpvUInt64) then begin
    aSampleVoice.Tag:=0;
    v^:=1.0;
   end;
   if IsZero(v^) then begin
    aSampleVoice.Active:=false;
   end else begin
    v^:=fFadeOutDuration-Min(Max(aSampleVoice.Tag/aSampleVoice.Sample.SampleRate,0.0),fFadeOutDuration);
   end;
   aSampleVoice.DynamicVolume:=Min(Max(trunc(sqr(v^)*32768.0),0),32768);
// aSampleVoice.DynamicRateFactor:=Min(Max(trunc(((sqr(v^)*0.5)+0.5)*65536.0),0),65536);
   aSampleVoice.Tag:=aSampleVoice.Tag+aDeltaSamples;
   result:=true;
  end;
 end;
end;

{ TSoundManager }

constructor TSoundManager.Create(const aGame:TObject);
begin

 inherited Create;

 GameSounds:=self;

 fGame:=aGame;

 fLock:=TPasMPCriticalSection.Create;

 fUIDFreeListLock:=TPasMPCriticalSection.Create;

 fUIDFreeList.Initialize;

 fPOCASubContext:=POCAContextCreate(ScreenMain.POCAContext.Instance);

 fPOCASoundGhostHash:=POCANewHash(fPOCASubContext);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'valid',POCASoundHashValid,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'destroy',POCASoundHashDestroy,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'reload',POCASoundHashReload,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'getName',POCASoundHashGetName,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'getUID',POCASoundHashGetUID,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'getFileName',POCASoundHashGetFileName,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'getPolyphony',POCASoundHashGetPolyphony,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'getLoop',POCASoundHashGetLoop,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'getRealVoices',POCASoundHashGetRealVoices,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'getFadeOutDuration',POCASoundHashGetFadeOutDuration,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'play',POCASoundHashPlay,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'playSpatialization',POCASoundHashPlaySpatialization,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'stop',POCASoundHashStop,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'keyOff',POCASoundHashKeyOff,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'setVolume',POCASoundHashSetVolume,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'setPanning',POCASoundHashSetPanning,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'setRate',POCASoundHashSetRate,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'setPosition',POCASoundHashSetPosition,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'setEffectMix',POCASoundHashSetEffectMix,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'isPlaying',POCASoundHashIsPlaying,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundGhostHash,'isVoicePlaying',POCASoundHashIsVoicePlaying,nil,self);

 fPOCASoundHash:=POCANewHash(fPOCASubContext);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundHash,'create',POCASoundsCreate,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundHash,'find',POCASoundsFind,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundHash,'remove',POCASoundsRemove,nil,self);
 POCAAddNativeFunction(fPOCASubContext,fPOCASoundHash,'clear',POCASoundsClear,nil,self);
 POCAHashSetString(fPOCASubContext,fPOCASubContext^.Instance^.Globals.NameSpace,'SoundManager',fPOCASoundHash);

 POCAHashSetString(fPOCASubContext,fPOCASubContext^.Instance^.Globals.NameSpace,'SoundHash',fPOCASoundGhostHash);

 fUIDCounter:=0;

 fSoundHashList:=TSoundHashList.Create(nil);

 fSoundExistHashList:=TSoundExistHashList.Create(false);

 fSounds:=TSounds.Create(true);

end;

destructor TSoundManager.Destroy;
begin

 while fSounds.Count>0 do begin
  fSounds[fSounds.Count-1].Free;
 end;
 FreeAndNil(fSounds);

 FreeAndNil(fSoundExistHashList);

 FreeAndNil(fSoundHashList);

 POCAHashDeleteString(fPOCASubContext,fPOCASubContext^.Instance^.Globals.NameSpace,'SoundManager');
 POCAHashDeleteString(fPOCASubContext,fPOCASubContext^.Instance^.Globals.NameSpace,'SoundHash');
 POCAContextDestroy(fPOCASubContext);

 fUIDFreeList.Finalize;

 FreeAndNil(fUIDFreeListLock);

 FreeAndNil(fLock);

 GameSounds:=nil;

 inherited Destroy;

end;

procedure TSoundManager.BackgroundLoad;
var Index:TpvSizeInt;
begin
 fLock.Acquire;
 try
  for Index:=0 to fSounds.Count-1 do begin
   fSounds[Index].BackgroundLoad;
  end;
 finally
  fLock.Release;
 end;
end;

procedure TSoundManager.UpdateAudio;
var Index:TpvSizeInt;
begin
 fLock.Acquire;
 try
  for Index:=0 to fSounds.Count-1 do begin
   fSounds[Index].UpdateAudio;
  end;
 finally
  fLock.Release;
 end;
end;

function TSoundManager.Add(const aName,aFileName:TpvUTF8String;const aPolyphony:TpvInt32;const aLoop:TpvInt32;const aRealVoices:TpvInt32;const aFadeOutDuration:TpvDouble):TSound;
begin
 result:=TSound.Create(self,aName,aFileName,aPolyphony,aLoop,aRealVoices,aFadeOutDuration);
end;

function TSoundManager.Find(const aName:TpvUTF8String):TSound;
begin
 fLock.Acquire;
 try
  if fSoundHashList.ExistKey(aName) then begin
   result:=fSoundHashList[aName];
  end else begin
   result:=nil;
  end;
 finally
  fLock.Release;
 end;
end;

procedure TSoundManager.Remove(const aSound:TSound);
begin
 if assigned(aSound) then begin
  aSound.Free;
 end;
end;

procedure TSoundManager.Remove(const aName:TpvUTF8String);
var Sound:TSound;
begin
 fLock.Acquire;
 try
  Sound:=Find(aName);
  if assigned(Sound) then begin
   Sound.Free;
  end;
 finally
  fLock.Release;
 end;
end;

procedure TSoundManager.Clear;
begin
 fLock.Acquire;
 try
  while fSounds.Count>0 do begin
   fSounds[fSounds.Count-1].Free;
  end;
 finally
  fLock.Release;
 end;
end;

initialization
end.
