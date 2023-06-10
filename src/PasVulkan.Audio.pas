(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                        Version 2017-07-13-03-19-0000                       *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2020, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
 *     x86-64, ARM, ARM64, etc.).                                             *
 * 11. Make sure the code runs on all platforms with Vulkan support           *
 *                                                                            *
 ******************************************************************************)
 unit PasVulkan.Audio;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses {$ifdef windows}Windows,{$endif}SysUtils,Classes,Math,SyncObjs,
     PasMP,
     PasJSON,
     PasVulkan.Types,
     {$ifdef UseExternalOGGVorbisTremorLibrary}
      PasVulkan.Audio.OGGVorbisTremor.ExternalLibrary,
     {$else}
      PasVulkan.Audio.OGGVorbisTremor,
     {$endif}
     PasVulkan.Collections,
     PasVulkan.Math,
     PasVulkan.Audio.HRTFTables,
     PasVulkan.Resources;

const SampleFixUp=1024;

      SoundLoopModeNONE=0;
      SoundLoopModeFORWARD=1;
      SoundLoopModePINGPONG=2;
      SoundLoopModeBACKWARD=3;

      FixedPointBits=12;
      FixedPointFactor=1 shl FixedPointBits;

      ResamplerFixedPointBits=32;
      ResamplerFixedPointFactor=TpvInt64($100000000);
      ResamplerFixedPointMask=TpvUInt32($ffffffff);

      ResamplerEpsilon:TpvFloat=1e-12;

      ResamplerSINCValueBits=15;
      ResamplerSINCValueLength=1 shl ResamplerSINCValueBits;
      ResamplerSINCFracBits=12;
      ResamplerSINCLength=1 shl ResamplerSINCFracBits;
      ResamplerSINCWidthBits=3;
      ResamplerSINCWidth=1 shl ResamplerSINCWidthBits;
      ResamplerSINCFracShift=ResamplerFixedPointBits-ResamplerSINCFracBits;
      ResamplerSINCFracMask=(1 shl ResamplerSINCFracShift)-1;

      ResamplerCubicSplineValueBits=12;
      ResamplerCubicSplineValueLength=1 shl ResamplerCubicSplineValueBits;
      ResamplerCubicSplineFracBits=12;
      ResamplerCubicSplineLength=1 shl ResamplerCubicSplineFracBits;
      ResamplerCubicSplineFracShift=ResamplerFixedPointBits-ResamplerCubicSplineFracBits;
      ResamplerCubicSplineFracMask=ResamplerCubicSplineLength-1;

      ResamplerLinearInterpolationValueBits=12;
      ResamplerLinearInterpolationValueLength=1 shl ResamplerLinearInterpolationValueBits;
      ResamplerLinearInterpolationFracBits=12;
      ResamplerLinearInterpolationLength=1 shl ResamplerLinearInterpolationFracBits;
      ResamplerLinearInterpolationFracShift=ResamplerFixedPointBits-ResamplerLinearInterpolationFracBits;
      ResamplerLinearInterpolationFracMask=ResamplerLinearInterpolationLength-1;

      ResamplerBufferBits=12;
      ResamplerBufferSize=1 shl ResamplerBufferBits;
      ResamplerBufferMask=ResamplerBufferSize-1;

      HalfPI=pi*0.5;

      TwoPI=pi*2.0;
                            
      WorldUnitsToMeters=1.0;

      MetersToWorldUnits=1.0;

      WorldUnitsToSoundUnits=WorldUnitsToMeters;

{     DopplerFactor=1.0;
      DopplerVelocity=2200.0;}

      DopplerFactor=WorldUnitsToSoundUnits;
      DopplerVelocity=1.0;

      SpeedOfSoundAir=343.3;
      SpeedOfSoundUnderwater=1522.0; //(1484+1560)*0.5;
      
      SpeedOfSoundAirToUnderwater=SpeedOfSoundAir/SpeedOfSoundUnderwater;

      HalfPanning=0.707106; // sin(HalfPI*0.5)

      HF_DAMP=0.25;
      HF_DAMP_HALF=HF_DAMP*0.5;
      HF_DAMP_FACTOR=1.0-(HF_DAMP*0.25);
      HF_FREQUENCY=3300;

      MinDistance=8.0;
      MaxDistance=65536.0;

      MinAbsorptionDistance=16;
      MaxAbsorptionDistance=4096;

      AttenuationRollOff=0.1;

      AirAbsorptionGainHF=0.99426; // -0.05dB
      AirAbsorptionFactor=0.1;

      // ear-to-ear-distance about 15-20mm
      EAR_DELAY_AIR=(((0.15+0.20)*0.5)/SpeedOfSoundAir)*1000.0;
      EAR_DELAY_UNDERWATER=(((0.15+0.20)*0.5)/SpeedOfSoundUnderwater)*1000.0;

      WATER_LOWPASS_FREQUENCY=300;

      WATER_BOOST_START_FREQUENCY=500;
      WATER_BOOST_END_FREQUENCY=1000;
      WATER_BOOST_FACTOR=4.0;

      LowPassBits=14;
      LowPassLength=1 shl LowPassBits;
      LowPassShift=16;
      LowPassShiftLength=1 shl LowPassShift;

      SpatializationDelayShift=16;
      SpatializationDelayLength=1 shl SpatializationDelayShift;
      SpatializationDelayMask=SpatializationDelayLength-1;

      MaxReverbAllPassFilters=16;

      PitchShifterOutputShift=12;
      PitchShifterOutputLen=1 shl PitchShifterOutputShift;

      PitchShifterBufferShift=10;
      PitchShifterBufferSize=1 shl PitchShifterBufferShift;
      PitchShifterBufferMask=PitchShifterBufferSize-1;

      SPATIALIZATION_FAST=0;
      SPATIALIZATION_PSEUDO=1;
      SPATIALIZATION_HRTF=2;

      ConeScale=1.0;
      InnerAngle=360.0;
      OuterAngle=360.0;
      OuterGain=0.0;
      OuterGainHF=1.0;

type PpvAudioInt32=^TpvInt32;

     PPPpvAudioInt32s=^TPPpvAudioInt32s;
     PPpvAudioInt32s=^TPpvAudioInt32s;
     PpvAudioInt32s=^TpvAudioInt32s;
     TPPpvAudioInt32s=array[0..$ffff] of PPpvAudioInt32s;
     TPpvAudioInt32s=array[0..$ffff] of PpvAudioInt32s;
     TpvAudioInt32s=array[0..$ffff] of TpvInt32;

     PpvAudioFloat=PpvFloat;
     TpvAudioFloat=TpvFloat;

     PpvAudioFloats=^TpvAudioFloats;
     TpvAudioFloats=array[0..$ffff] of TpvFloat;

     PPpvAudioFloats=^TPpvAudioFloats;
     TPpvAudioFloats=array[0..$ffff] of PpvAudioFloats;
     
     PpvAudioSoundSampleValue=^TpvAudioSoundSampleValue;
     TpvAudioSoundSampleValue=TpvInt32;

     PpvAudioSoundSampleStereoValue=^TpvAudioSoundSampleStereoValue;
     TpvAudioSoundSampleStereoValue=array[0..1] of TpvInt32;

     PpvAudioSoundSampleValues=^TpvAudioSoundSampleValues;
     TpvAudioSoundSampleValues=array[0..($7ffffff0 div sizeof(TpvAudioSoundSampleValue))-1] of TpvAudioSoundSampleValue;

     PpvAudioSoundSampleLoop=^TpvAudioSoundSampleLoop;
     TpvAudioSoundSampleLoop=record
      Mode:TpvInt32;
      StartSample:TpvInt32;
      EndSample:TpvInt32;
     end;

     TpvAudioSoundSample=class;

     TpvAudio=class;

     TpvAudioStringHashMap=class(TpvStringHashMap<TpvPointer>);

     TpvAudioHRTFCoefs=array[0..HRIR_MAX_LENGTH-1] of TpvInt32;

     TpvAudioHRTFHistory=array[0..HRIR_MAX_LENGTH-1] of TpvInt32;

     TpvAudioSoundSampleVoiceLowPassHistory=array[0..1] of TpvInt32;

     TpvAudioSoundSampleVoice=class
      private
       Previous:TpvAudioSoundSampleVoice;
       Next:TpvAudioSoundSampleVoice;
       NextFree:TpvAudioSoundSampleVoice;
       IsOnList:LongBool;
       AudioEngine:TpvAudio;
       Sample:TpvAudioSoundSample;
       Index:TpvInt32;
       MixToEffect:LongBool;
       Active:LongBool;
       KeyOff:LongBool;
       Backwards:LongBool;
       Volume:TpvInt32;
       Panning:TpvInt32;
       Age:TpvInt64;
       Position:TpvInt64;
       Increment:TpvInt64;
       IncrementLast:TpvInt64;
       IncrementCurrent:TpvInt64;
       IncrementIncrement:TpvInt64;
       IncrementRampingRemain:TpvInt32;
       IncrementRampingStepRemain:TpvInt32;
       MixIncrement:TpvInt64;
       RampingSamples:TpvInt32;
       MulLeft:TpvInt32;
       MulRight:TpvInt32;
       VolumeLeft:TpvInt32;
       VolumeRight:TpvInt32;
       VolumeLeftLast:TpvInt32;
       VolumeRightLast:TpvInt32;
       VolumeLeftCurrent:TpvInt32;
       VolumeRightCurrent:TpvInt32;
       VolumeLeftIncrement:TpvInt32;
       VolumeRightIncrement:TpvInt32;
       VolumeRampingRemain:TpvInt32;
       HRTFLeftCoefs:TpvAudioHRTFCoefs;
       HRTFRightCoefs:TpvAudioHRTFCoefs;
       HRTFLeftCoefsCurrent:TpvAudioHRTFCoefs;
       HRTFRightCoefsCurrent:TpvAudioHRTFCoefs;
       HRTFLeftCoefsIncrement:TpvAudioHRTFCoefs;
       HRTFRightCoefsIncrement:TpvAudioHRTFCoefs;
       HRTFLeftHistory:TpvAudioHRTFHistory;
       HRTFRightHistory:TpvAudioHRTFHistory;
       HRTFHistoryIndex:TpvInt32;
       HRTFLeftDelay:TpvInt32;
       HRTFRightDelay:TpvInt32;
       HRTFLeftDelayCurrent:TpvInt32;
       HRTFRightDelayCurrent:TpvInt32;
       HRTFLeftDelayLast:TpvInt32;
       HRTFRightDelayLast:TpvInt32;
       HRTFLeftDelayIncrement:TpvInt32;
       HRTFRightDelayIncrement:TpvInt32;
       HRTFCounter:TpvInt32;
       HRTFRampingRemain:TpvInt32;
       HRTFRampingStepRemain:TpvInt32;
       HRTFLength:TpvInt32;
       HRTFMask:TpvInt32;
       Spatialization:LongBool;
       SpatializationOrigin:TpvVector3;
       SpatializationVelocity:TpvVector3;
       SpatializationVolumeLast:TpvFloat;
       SpatializationDelayLeft:TpvInt32;
       SpatializationDelayRight:TpvInt32;
       SpatializationDelayLeftIndex:TpvInt32;
       SpatializationDelayRightIndex:TpvInt32;
       SpatializationDelayLeftLast:TpvInt32;
       SpatializationDelayRightLast:TpvInt32;
       SpatializationDelayLeftCurrent:TpvInt32;
       SpatializationDelayRightCurrent:TpvInt32;
       SpatializationDelayLeftIncrement:TpvInt32;
       SpatializationDelayRightIncrement:TpvInt32;
       SpatializationDelayRampingRemain:TpvInt32;
       SpatializationDelayLeftLine:array of TpvInt32;
       SpatializationDelayRightLine:array of TpvInt32;
       SpatializationLowPassLeftCoef:TpvInt32;
       SpatializationLowPassRightCoef:TpvInt32;
       SpatializationLowPassLeftLastCoef:TpvInt32;
       SpatializationLowPassRightLastCoef:TpvInt32;
       SpatializationLowPassLeftCurrentCoef:TpvInt32;
       SpatializationLowPassRightCurrentCoef:TpvInt32;
       SpatializationLowPassLeftIncrementCoef:TpvInt32;
       SpatializationLowPassRightIncrementCoef:TpvInt32;
       SpatializationLowPassLeftHistory:TpvAudioSoundSampleVoiceLowPassHistory;
       SpatializationLowPassRightHistory:TpvAudioSoundSampleVoiceLowPassHistory;
       SpatializationLowPassRampingRemain:TpvInt32;
       LastDirection:TpvVector3;
       LastLeft:TpvInt32;
       LastRight:TpvInt32;
       NewLastLeft:TpvInt32;
       NewLastRight:TpvInt32;
       Rate:TpvFloat;
       DopplerRate:TpvFloat;
       VoiceIndexPointer:TpvPointer;
       procedure UpdateSpatialization;
       function GetSampleLength(CountSamplesValue:TpvInt32):TpvInt32;
       procedure PreClickRemoval(Buffer:TpvPointer);
       procedure PostClickRemoval(Buffer:TpvPointer;Remain:TpvInt32);
       procedure MixProcSpatializationHRTF(Buffer:TpvPointer;ToDo:TpvInt32);
       procedure MixProcSpatializationPSEUDO(Buffer:TpvPointer;ToDo:TpvInt32);
       procedure MixProcVolumeRamping(Buffer:TpvPointer;ToDo:TpvInt32);
       procedure MixProcNormal(Buffer:TpvPointer;ToDo:TpvInt32);
       procedure UpdateIncrementRamping;
       procedure UpdateVolumeRamping(MixVolume:TpvInt32);
       procedure UpdateSpatializationDelayRamping;
       procedure UpdateSpatializationLowPassRamping;
      public
       constructor Create(AAudioEngine:TpvAudio;ASample:TpvAudioSoundSample;AIndex:TpvInt32);
       destructor Destroy; override;
       procedure Enqueue;
       procedure Dequeue;
       procedure Init(AVolume,APanning,ARate:TpvFloat);
       procedure MixTo(Buffer:PpvAudioSoundSampleValues;MixVolume:TpvInt32);
     end;

     TpvAudioSoundSampleVoices=array of TpvAudioSoundSampleVoice;

     TpvAudioSoundSamples=class;

     TpvAudioSoundSample=class
      public
       AudioEngine:TpvAudio;
       SoundSamples:TpvAudioSoundSamples;
       Name:TpvRawByteString;
       Data:PpvAudioSoundSampleValues;
       SampleLength:TpvInt32;
       SampleRate:TpvInt32;
       Loop:TpvAudioSoundSampleLoop;
       SustainLoop:TpvAudioSoundSampleLoop;
       Voices:TpvAudioSoundSampleVoices;
       ReferenceCounter:TpvInt32;
       SamplePolyphony:TpvInt32;
       FreeVoice:TpvAudioSoundSampleVoice;
       constructor Create(AAudioEngine:TpvAudio;ASoundSamples:TpvAudioSoundSamples);
       destructor Destroy; override;
       procedure IncRef;
       procedure DecRef;
       procedure CorrectPolyphony;
       procedure FixUp;
       procedure SetPolyphony(Polyphony:TpvInt32);
       function Play(Volume,Panning,Rate:TpvFloat;VoiceIndexPointer:TpvPointer=nil):TpvInt32;
       procedure Stop(VoiceNumber:TpvInt32);
       procedure KeyOff(VoiceNumber:TpvInt32);
       function SetVolume(VoiceNumber:TpvInt32;Volume:TpvFloat):TpvInt32;
       function SetPanning(VoiceNumber:TpvInt32;Panning:TpvFloat):TpvInt32;
       function SetRate(VoiceNumber:TpvInt32;Rate:TpvFloat):TpvInt32;
       function SetPosition(VoiceNumber:TpvInt32;Spatialization:LongBool;const Origin,Velocity:TpvVector3):TpvInt32;
       function SetEffectMix(VoiceNumber:TpvInt32;Active:LongBool):TpvInt32;
       function IsPlaying:boolean;
       function IsVoicePlaying(VoiceNumber:TpvInt32):boolean;
     end;

     PpvAudioSoundMusicBufferSample=^TpvAudioSoundMusicBufferSample;
     TpvAudioSoundMusicBufferSample=record
      Left,Right:TpvInt32;
     end;

     TpvAudioResamplerBuffer=array[0..ResamplerBufferSize-1] of TpvAudioSoundMusicBufferSample;

     PpvAudioResamplerCubicSplineSubArray=^TpvAudioResamplerCubicSplineSubArray;
     TpvAudioResamplerCubicSplineSubArray=packed array[0..3] of TpvInt32;

     PpvAudioResamplerCubicSplineArray=^TpvAudioResamplerCubicSplineArray;
     TpvAudioResamplerCubicSplineArray=packed array[0..ResamplerCubicSplineLength-1] of TpvAudioResamplerCubicSplineSubArray;

     PpvAudioResamplerSINCSubArray=^TpvAudioResamplerSINCSubArray;
     TpvAudioResamplerSINCSubArray=packed array[0..ResamplerSINCWidth-1] of TpvInt32;

     PpvAudioResamplerSINCArray=^TpvAudioResamplerSINCArray;
     TpvAudioResamplerSINCArray=packed array[0..ResamplerSINCLength-1] of TpvAudioResamplerSINCSubArray;

     TpvAudioSoundMusics=class;

     TpvAudioSoundMusic=class
      public
       AudioEngine:TpvAudio;
       SoundMusics:TpvAudioSoundMusics;
       Name:TpvRawByteString;
       Data:TStream;
       Active:LongBool;
       Loop:LongBool;
       KeyOff:LongBool;
       Volume:TpvInt32;
       VolumeLeft:TpvInt32;
       VolumeRight:TpvInt32;
       VolumeLeftCurrent:TpvInt32;
       VolumeRightCurrent:TpvInt32;
       VolumeLeftInc:TpvInt32;
       VolumeRightInc:TpvInt32;
       Panning:TpvInt32;
       VolumeRampingRemain:TpvInt32;
       Age:TpvInt64;
       Position:TpvInt64;
       Increment:TpvInt64;
       LastLeft:TpvInt32;
       LastRight:TpvInt32;
       NewLastLeft:TpvInt32;
       NewLastRight:TpvInt32;
       ResamplerBuffer:TpvAudioResamplerBuffer;
       ResamplerBufferPosition:TpvInt32;
       ResamplerCurrentSample:TpvAudioSoundMusicBufferSample;
       ResamplerLastSample:TpvAudioSoundMusicBufferSample;
       ResamplerPosition:TpvInt64;
       ResamplerIncrement:TpvInt64;
       ResamplerOriginalIncrement:TpvInt64;
       OutBuffer:array[0..4095] of TpvAudioSoundMusicBufferSample;
       OutBufferPosition:TpvInt32;
       OutBufferSize:TpvInt32;
       InBuffer:array[0..4095] of TpvAudioSoundMusicBufferSample;
       InBufferPosition:TpvInt32;
       InBufferSize:TpvInt32;
       PCMBuffer:array [0..65535] of smallint;
       Channels:TpvInt32;
       SampleRate:TpvInt32;
       BitStream:TpvInt32;
       LastSample:TpvAudioSoundMusicBufferSample;
       Table:TpvAudioResamplerSINCArray;
       vf:POggVorbis_File;
       constructor Create(AAudioEngine:TpvAudio;ASoundMusics:TpvAudioSoundMusics);
       destructor Destroy; override;
       procedure InitSINC;
       procedure Play(AVolume,APanning,ARate:TpvFloat;ALoop:boolean);
       procedure Stop;
       procedure SetVolume(AVolume:TpvFloat);
       procedure SetPanning(APanning:TpvFloat);
       procedure SetRate(ARate:TpvFloat);
       procedure GetNextInBuffer;
       procedure Resample;
       procedure MixTo(Buffer:PpvAudioSoundSampleValues;MixVolume:TpvInt32);
       function IsPlaying:boolean;
     end;

     IpvAudioSoundSampleResource=interface(IpvResource)['{9E4ABC9F-7EBE-49D8-BD78-146A875F44FF}']
       procedure FixUp;
       procedure SetPolyphony(Polyphony:TpvInt32);
       function Play(Volume,Panning,Rate:TpvFloat;VoiceIndexPointer:TpvPointer=nil):TpvInt32;
       procedure Stop(VoiceNumber:TpvInt32);
       procedure KeyOff(VoiceNumber:TpvInt32);
       function SetVolume(VoiceNumber:TpvInt32;Volume:TpvFloat):TpvInt32;
       function SetPanning(VoiceNumber:TpvInt32;Panning:TpvFloat):TpvInt32;
       function SetRate(VoiceNumber:TpvInt32;Rate:TpvFloat):TpvInt32;
       function SetPosition(VoiceNumber:TpvInt32;Spatialization:LongBool;const Origin,Velocity:TpvVector3):TpvInt32;
       function SetEffectMix(VoiceNumber:TpvInt32;Active:LongBool):TpvInt32;
       function IsPlaying:boolean;
       function IsVoicePlaying(VoiceNumber:TpvInt32):boolean;
     end;

     TpvAudioSoundSampleResource=class(TpvResource,IpvAudioSoundSampleResource)
      private
       fSample:TpvAudioSoundSample;
      public
       constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
       destructor Destroy; override;
       function BeginLoad(const aStream:TStream):boolean; override;
       procedure FixUp;
       procedure SetPolyphony(Polyphony:TpvInt32);
       function Play(Volume,Panning,Rate:TpvFloat;VoiceIndexPointer:TpvPointer=nil):TpvInt32;
       procedure Stop(VoiceNumber:TpvInt32);
       procedure KeyOff(VoiceNumber:TpvInt32);
       function SetVolume(VoiceNumber:TpvInt32;Volume:TpvFloat):TpvInt32;
       function SetPanning(VoiceNumber:TpvInt32;Panning:TpvFloat):TpvInt32;
       function SetRate(VoiceNumber:TpvInt32;Rate:TpvFloat):TpvInt32;
       function SetPosition(VoiceNumber:TpvInt32;Spatialization:LongBool;const Origin,Velocity:TpvVector3):TpvInt32;
       function SetEffectMix(VoiceNumber:TpvInt32;Active:LongBool):TpvInt32;
       function IsPlaying:boolean;
       function IsVoicePlaying(VoiceNumber:TpvInt32):boolean;
      published
       property Sample:TpvAudioSoundSample read fSample;
     end;

     IpvAudioSoundMusicResource=interface(IpvResource)['{4F43005B-109A-4DF4-808E-4ECAA3BF00A6}']
       procedure Play(AVolume,APanning,ARate:TpvFloat;ALoop:boolean);
       procedure Stop;
       procedure SetVolume(AVolume:TpvFloat);
       procedure SetPanning(APanning:TpvFloat);
       procedure SetRate(ARate:TpvFloat);
       function IsPlaying:boolean;
     end;

     TpvAudioSoundMusicResource=class(TpvResource,IpvAudioSoundMusicResource)
      private
       fMusic:TpvAudioSoundMusic;
      public
       constructor Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil); override;
       destructor Destroy; override;
       function BeginLoad(const aStream:TStream):boolean; override;
       procedure Play(AVolume,APanning,ARate:TpvFloat;ALoop:boolean);
       procedure Stop;
       procedure SetVolume(AVolume:TpvFloat);
       procedure SetPanning(APanning:TpvFloat);
       procedure SetRate(ARate:TpvFloat);
       function IsPlaying:boolean;
      published
       property Music:TpvAudioSoundMusic read fMusic;
     end;

     TpvAudioSoundSamples=class(TList)
      private
       function GetItem(Index:TpvInt32):TpvAudioSoundSample;
       procedure SetItem(Index:TpvInt32;Item:TpvAudioSoundSample);
      public
       AudioEngine:TpvAudio;
       HashMap:TpvAudioStringHashMap;
       constructor Create(AAudioEngine:TpvAudio);
       destructor Destroy; override;
       function Load(Name:TpvRawByteString;Stream:TStream;DoFree:boolean=true;Polyphony:TpvInt32=1;Loop:TpvUInt32=1):TpvAudioSoundSample;
       property Items[Index:TpvInt32]:TpvAudioSoundSample read GetItem write SetItem; default;
     end;

     TpvAudioSoundMusics=class(TList)
      private
       function GetItem(Index:TpvInt32):TpvAudioSoundMusic;
       procedure SetItem(Index:TpvInt32;Item:TpvAudioSoundMusic);
      public
       AudioEngine:TpvAudio;
       HashMap:TpvAudioStringHashMap;
       constructor Create(AAudioEngine:TpvAudio);
       destructor Destroy; override;
       function Load(Name:TpvRawByteString;Stream:TStream;DoFree:boolean=true):TpvAudioSoundMusic;
       property Items[Index:TpvInt32]:TpvAudioSoundMusic read GetItem write SetItem; default;
     end;

     TpvAudioUpdateHook=procedure of object;

     PpvAudioPitchShifterBuffer=^TpvAudioPitchShifterBuffer;
     TpvAudioPitchShifterBuffer=array[0..PitchShifterBufferSize+16] of TpvAudioSoundSampleStereoValue;

     PpvAudioPitchShifterFadeBuffer=^TpvAudioPitchShifterFadeBuffer;
     TpvAudioPitchShifterFadeBuffer=array[0..PitchShifterBufferSize+1] of TpvAudioSoundSampleValue;

     TpvAudioPitchShifter=class
      public
       AudioEngine:TpvAudio;
       PitchShifterFadeBuffer:TpvAudioPitchShifterFadeBuffer;
       WorkBuffer:TpvAudioPitchShifterBuffer;
       p1:TpvUInt32;
       p2:TpvUInt32;
       InputPointer:TpvUInt32;
       Factor:TpvFloat;
       constructor Create(AAudioEngine:TpvAudio);
       destructor Destroy; override;
       procedure Reset;
       procedure Process(Buffer:TpvPointer;Samples:TpvInt32);
     end;

     TpvAudioReverbAllPassBufferSample=array[0..1] of TpvInt32;

     TpvAudioReverbAllPassBuffer=array of TpvAudioReverbAllPassBufferSample;

     TpvAudioReverbBuffer=array of TpvInt32;

     TpvAudioReverb=class
      private
       AllPassBuffer:array[0..MaxReverbAllPassFilters-1] of TpvAudioReverbAllPassBuffer;
       Counter:array[0..MaxReverbAllPassFilters-1,0..3] of TpvInt32;
       LastLowPassLeft:TpvInt32;
       LastLowPassRight:TpvInt32;
       LeftBuffer:TpvAudioReverbBuffer;
       RightBuffer:TpvAudioReverbBuffer;
       LeftDelayedCounter:TpvInt32;
       RightDelayedCounter:TpvInt32;
       LeftCounter:TpvInt32;
       RightCounter:TpvInt32;
       SampleBufferSize:TpvInt32;
       SampleBufferMask:TpvInt32;
       CutOff:TpvInt32;
      public
       AudioEngine:TpvAudio;
       PreDelay:TpvInt32;
       CombFilterSeparation:TpvInt32;
       RoomSize:TpvInt32;
       FeedBack:TpvFloat;
       Absortion:TpvInt32;
       Dry:TpvFloat;
       Wet:TpvFloat;
       AllPassFilters:TpvInt32;
       constructor Create(AAudioEngine:TpvAudio);
       destructor Destroy; override;
       procedure Reset;
       procedure Init;
       procedure Process(Buffer:TpvPointer;Samples:TpvInt32);
     end;

     TpvAudioThread=class(TThread)
      protected
       procedure Execute; override;
      public
       AudioEngine:TpvAudio;
       Buffer:TpvPointer;
       Event:TEvent;
       Sleeping:TpvInt32;
       constructor Create(AAudioEngine:TpvAudio);
       destructor Destroy; override;
       procedure Start;
      published
       property Terminated;
     end;

     TpvAudio=class
      private
       procedure CalcEvIndices(ev:TpvFloat;evidx:PpvAudioInt32s;var evmu:TpvFloat);
       procedure CalcAzIndices(evidx:TpvInt32;az:TpvFloat;azidx:PpvAudioInt32s;var azmu:TpvFloat);
       procedure GetLerpedHRTFCoefs(Elevation,Azimuth:TpvFloat;var LeftCoefs,RightCoefs:TpvAudioHRTFCoefs;var LeftDelay,RightDelay:TpvInt32);
      public
       Samples:TpvAudioSoundSamples;
       Musics:TpvAudioSoundMusics;
       SpatializationMode:TpvInt32;
       HRTF:LongBool;
       HRTFPreset:PpvAudioHRTFPreset;
       VoiceFirst:TpvAudioSoundSampleVoice;
       VoiceLast:TpvAudioSoundSampleVoice;
       SampleRate:TpvInt32;
       Channels:TpvInt32;
       Bits:TpvInt32;
       SpatializationWaterLowPassCW:TpvFloat;
       SpatializationWaterWaterBoostLowPassCW:TpvInt32;
       SpatializationWaterWaterBoostHighPassCW:TpvInt32;
       SpatializationWaterWaterBoost:TpvInt32;
       SpatializationLowPassCW:TpvFloat;
       SpatializationDelayAir:TpvFloat;
       SpatializationDelayUnderwater:TpvFloat;
       SpatializationDelayPowerOfTwo:TpvInt32;
       SpatializationDelayMask:TpvInt32;
       BufferSamples:TpvInt32;
       BufferChannelSamples:TpvInt32;
       BufferOutputChannelSamples:TpvInt32;
       MixingBufferSize:TpvInt32;
       OutputBufferSize:TpvInt32;
       MixingBuffer:PpvAudioSoundSampleValues;
       EffectMixingBuffer:PpvAudioSoundSampleValues;
       OutputBuffer:TpvPointer;
       MasterVolume:TpvInt32;
       SampleVolume:TpvInt32;
       MusicVolume:TpvInt32;
       RampingSamples:TpvInt32;
       RampingStepSamples:TpvInt32;
       AGCActive:LongBool;
       AGC:TpvInt32;
       AGCCounter:TpvInt32;
       AGCInterval:TpvInt32;
       CriticalSection:TCriticalSection;
       CubicSplineTable:TpvAudioResamplerCubicSplineArray;
       ListenerOrigin:TpvVector3;
       ListenerVelocity:TpvVector3;
       ListenerMatrix:TpvMatrix4x4;
       ListenerUnderwater:LongBool;
       LowPassLeft:TpvInt32;
       LowPassRight:TpvInt32;
       LowPassLast:TpvInt32;
       LowPassCurrent:TpvInt32;
       LowPassIncrement:TpvInt32;
       LowPassRampingLength:TpvInt32;
       WaterBoostLowPassLeft:array[0..3] of TpvInt32;
       WaterBoostLowPassRight:array[0..3] of TpvInt32;
       WaterBoostMiddlePassLeft:TpvInt32;
       WaterBoostMiddlePassRight:TpvInt32;
       WaterBoostHighPassLeft:array[0..3] of TpvInt32;
       WaterBoostHighPassRight:array[0..3] of TpvInt32;
       WaterBoostHistoryLeft:array[0..3] of TpvInt32;
       WaterBoostHistoryRight:array[0..3] of TpvInt32;
       UpdateHook:TpvAudioUpdateHook;
       PitchShifter:TpvAudioPitchShifter;
       Reverb:TpvAudioReverb;
       PanningLUT:array[0..$10000] of TpvInt32;
       RingBuffer:TPasMPSingleProducerSingleConsumerRingBuffer;
       Thread:TpvAudioThread;
       IsReady:LongBool;
       IsMuted:LongBool;
       IsActive:LongBool;
       constructor Create(ASampleRate,AChannels,ABits,ABufferSamples:TpvInt32);
       destructor Destroy; override;
       procedure SetMixerMasterVolume(NewVolume:TpvFloat);
       procedure SetMixerMusicVolume(NewVolume:TpvFloat);
       procedure SetMixerSampleVolume(NewVolume:TpvFloat);
       procedure SetMixerAGC(Enabled:boolean);
       procedure Setup;
       procedure ClipBuffer(p:TpvPointer;Range:TpvInt32);
       procedure FillBuffer;
       procedure Lock;
       procedure Unlock;
       procedure SetActive(Active:boolean);
       procedure Mute;
       procedure Unmute;
     end;

implementation

const PositionShift=32;
      PositionFactor:TpvInt64=$100000000;//(1 shl PositionShift);
      PositionMask=$ffffffff;//TpvInt64(1 shl PositionShift)-1;
      PositionDivFactor=1/$100000000;//(1 shl PositionShift);

      SpatializationlRemainBits=14;
      SpatializationlRemainFactor=1 shl SpatializationlRemainBits;

var AudioInstance:TpvAudio=nil;

function SwapWordLittleEndian(Value:TpvUInt16):TpvUInt16; {$ifdef cpu386}register;{$endif}
begin
{$ifdef big_endian}
 result:=((Value and $ff00) shr 8) or ((Value and $ff) shl 8);
{$else}
 result:=Value;
{$endif}
end;

function SwapDWordLittleEndian(Value:TpvUInt32):TpvUInt32; {$ifdef cpu386}register;{$endif}
begin
{$ifdef big_endian}
 result:=((Value and $ff000000) shr 24) or ((Value and $00ff0000) shr 8) or
         ((Value and $0000ff00) shl 8) or ((Value and $000000ff) shl 24);
{$else}
 result:=Value;
{$endif}
end;

function SwapWordBigEndian(Value:TpvUInt16):TpvUInt16; {$ifdef cpu386}register;{$endif}
begin
{$ifdef little_endian}
 result:=((Value and $ff00) shr 8) or ((Value and $ff) shl 8);
{$else}
 result:=Value;
{$endif}
end;

function SwapDWordBigEndian(Value:TpvUInt32):TpvUInt32; {$ifdef cpu386}register;{$endif}
begin
{$ifdef little_endian}
 result:=((Value and $ff000000) shr 24) or ((Value and $00ff0000) shr 8) or
         ((Value and $0000ff00) shl 8) or ((Value and $000000ff) shl 24);
{$else}
 result:=Value;
{$endif}
end;

procedure SwapLittleEndianData16(var Data); {$ifdef cpu386}register;{$endif}
{$ifdef big_endian}
var Value:TpvUInt16 absolute Data;
begin
 Value:=((Value and $ff00) shr 8) or ((Value and $ff) shl 8);
{$else}
begin
{$endif}
end;

procedure SwapLittleEndianData32(var Data); {$ifdef cpu386}register;{$endif}
{$ifdef big_endian}
var Value:TpvUInt32 absolute Data;
begin
 Value:=((Value and $ff000000) shr 24) or ((Value and $00ff0000) shr 8) or
        ((Value and $0000ff00) shl 8) or ((Value and $000000ff) shl 24);
{$else}
begin
{$endif}
end;

procedure SwapBigEndianData16(var Data); {$ifdef cpu386}register;{$endif}
{$ifdef little_endian}
var Value:TpvUInt16 absolute Data;
begin
 Value:=((Value and $ff00) shr 8) or ((Value and $ff) shl 8);
{$else}
begin
 result:=Value;
{$endif}
end;

procedure SwapBigEndianData32(var Data); {$ifdef cpu386}register;{$endif}
{$ifdef little_endian}
var Value:TpvUInt32 absolute Data;
begin
 Value:=((Value and $ff000000) shr 24) or ((Value and $00ff0000) shr 8) or
        ((Value and $0000ff00) shl 8) or ((Value and $000000ff) shl 24);
{$else}
begin
{$endif}
end;

{$ifndef HasSAR}
function SARLongint(Value,Shift:TpvInt32):TpvInt32;
{$if defined(cpu386)} assembler; register; {$ifdef fpc}nostackframe;{$endif}
asm
 mov ecx,edx
 sar eax,cl
end;
{$elseif defined(cpux64) or defined(cpuamd64) or defined(cpux86_64)} assembler; register; {$ifdef fpc}nostackframe;{$endif}
asm
{$ifndef fpc}
 .noframe
{$endif}
{$if defined(Win32) or defined(Win64) or defined(Windows)}
 mov eax,ecx
 mov ecx,edx
{$else}
 mov eax,edi
 mov ecx,esi
{$ifend}
 sar eax,cl
end;
{$elseif defined(cpuarm)} assembler; {$ifdef fpc}nostackframe;{$endif}
asm
 mov r0,r0,asr r1
{$if defined(cpuarm_has_bx)}
 bx lr
{$else}
 mov pc,lr
{$ifend}
end;
{$else} {$ifdef caninline}inline;{$endif}
begin
{$ifdef HasSAR}
 result:=SARLongint(Value,Shift);
{$else}
 Shift:=Shift and 31;
 result:=(TpvUInt32(Value) shr Shift) or (TpvUInt32(TpvInt32(TpvUInt32(0-TpvUInt32(TpvUInt32(Value) shr 31)) and TpvUInt32(0-TpvUInt32(ord(Shift<>0))))) shl (32-Shift));
{$endif}
end;
{$ifend}
{$endif}

function Clamp(x,a,b:TpvDouble):TpvDouble;
begin
 if x<a then begin
  result:=a;
 end else if x>b then begin
  result:=b;
 end else begin
  result:=x;
 end;
end;

function Lerp(a,b,x:TpvFloat):TpvFloat;
begin
 if x<0 then begin
  result:=a;
 end else if x>1.0 then begin
  result:=b;
 end else begin
  result:=(a*(1.0-x))+(b*x);
 end;
end;
 
function IntMod(x,y:TpvInt32):TpvInt32;
begin
 result:=x mod y;
{if y>0 then begin
  while result<=-y do begin
   inc(result,y);
  end;
  while result>=y do begin
   dec(result,y);
  end;
 end;}
end;

function CalculateDelta(OldGain,NewGain:TpvFloat;OldDir,NewDir:TpvVector3):TpvFloat;
var GainChange,AngleChange:TpvFloat;
begin
 OldGain:=Max(OldGain,0.0001);
 NewGain:=Max(NewGain,0.0001);
 GainChange:=abs(log10(NewGain/OldGain)/log10(0.0001));
 AngleChange:=0.0;
 if (GainChange>0.0001) or (NewGain>0.0001) then begin
  if (abs(NewDir.x-OldDir.x)>0.000001) or (abs(NewDir.y-OldDir.y)>0.000001) or (abs(NewDir.z-OldDir.z)>0.000001) then begin
   AngleChange:=ArcCos(Min(Max(OldDir.Dot(NewDir),0.0),1.0))/pi;
  end;
 end;
 result:=Min(Max(AngleChange*25.0,GainChange)*2.0,1.0);
end;
 
constructor TpvAudioSoundSampleVoice.Create(AAudioEngine:TpvAudio;ASample:TpvAudioSoundSample;AIndex:TpvInt32);
begin
 inherited Create;
 Previous:=nil;
 Next:=nil;
 NextFree:=nil;
 IsOnList:=false;
 AudioEngine:=AAudioEngine;
 Sample:=ASample;
 Index:=AIndex;
 MixToEffect:=false;
 Active:=false;
 LastLeft:=0;
 LastRight:=0;
 NewLastLeft:=0;
 NewLastRight:=0;
 MulLeft:=32768;
 MulRight:=32768;
 VolumeRampingRemain:=0;
 IncrementRampingRemain:=0;
 IncrementRampingStepRemain:=0;
 FillChar(HRTFLeftCoefs,SizeOf(TpvAudioHRTFCoefs),AnsiChar(#0));
 FillChar(HRTFRightCoefs,SizeOf(TpvAudioHRTFCoefs),AnsiChar(#0));
 FillChar(HRTFLeftCoefsCurrent,SizeOf(TpvAudioHRTFCoefs),AnsiChar(#0));
 FillChar(HRTFRightCoefsCurrent,SizeOf(TpvAudioHRTFCoefs),AnsiChar(#0));
 FillChar(HRTFLeftCoefsIncrement,SizeOf(TpvAudioHRTFCoefs),AnsiChar(#0));
 FillChar(HRTFRightCoefsIncrement,SizeOf(TpvAudioHRTFCoefs),AnsiChar(#0));
 FillChar(HRTFLeftHistory,SizeOf(TpvAudioHRTFHistory),AnsiChar(#0));
 FillChar(HRTFRightHistory,SizeOf(TpvAudioHRTFHistory),AnsiChar(#0));
 HRTFHistoryIndex:=0;
 HRTFLeftDelay:=0;
 HRTFRightDelay:=0;
 HRTFLeftDelayCurrent:=0;
 HRTFRightDelayCurrent:=0;
 HRTFLeftDelayLast:=0;
 HRTFRightDelayLast:=0;
 HRTFLeftDelayIncrement:=0;
 HRTFRightDelayIncrement:=0;
 HRTFCounter:=0;
 HRTFRampingRemain:=0;
 HRTFRampingStepRemain:=0;
 LastDirection:=TpvVector3.Null;
 SpatializationVolumeLast:=0;
 SpatializationDelayLeft:=0;
 SpatializationDelayRight:=0;
 SpatializationDelayLeftLast:=SpatializationDelayLeft;
 SpatializationDelayRightLast:=SpatializationDelayRight;
 SpatializationDelayLeftCurrent:=SpatializationDelayLeft;
 SpatializationDelayRightCurrent:=SpatializationDelayRight;
 SpatializationDelayLeftIncrement:=0;
 SpatializationDelayRightIncrement:=0;
 SpatializationDelayRampingRemain:=0;
 SpatializationDelayLeftLine:=nil;
 SpatializationDelayRightLine:=nil;
 SetLength(SpatializationDelayLeftLine,AudioEngine.SpatializationDelayPowerOfTwo);
 SetLength(SpatializationDelayRightLine,AudioEngine.SpatializationDelayPowerOfTwo);
 SpatializationLowPassLeftCoef:=LowPassLength shl LowPassShift;
 SpatializationLowPassRightCoef:=LowPassLength shl LowPassShift;
 SpatializationLowPassLeftLastCoef:=SpatializationLowPassLeftCoef;
 SpatializationLowPassRightLastCoef:=SpatializationLowPassRightCoef;
 SpatializationLowPassLeftCurrentCoef:=SpatializationLowPassLeftCoef;
 SpatializationLowPassRightCurrentCoef:=SpatializationLowPassRightCoef;
 SpatializationLowPassLeftIncrementCoef:=0;
 SpatializationLowPassRightIncrementCoef:=0;
 SpatializationLowPassRampingRemain:=0;
 RampingSamples:=AudioEngine.RampingSamples;
 VoiceIndexPointer:=nil;
 Spatialization:=false;
 SpatializationOrigin:=TpvVector3.Null;
 SpatializationVelocity:=TpvVector3.Null;
 if AudioEngine.HRTF then begin
  HRTFLength:=AudioEngine.HRTFPreset^.irSize;
 end else begin
  HRTFLength:=HRIR_MAX_LENGTH;
 end;
 HRTFMask:=HRTFLength-1;
end;

destructor TpvAudioSoundSampleVoice.Destroy;
begin
 Dequeue;
 SetLength(SpatializationDelayLeftLine,0);
 SetLength(SpatializationDelayRightLine,0);
 inherited Destroy;
end;

procedure TpvAudioSoundSampleVoice.Enqueue;
begin
 if not IsOnList then begin
  if assigned(AudioEngine.VoiceLast) then begin
   AudioEngine.VoiceLast.Next:=self;
   Previous:=AudioEngine.VoiceLast;
   AudioEngine.VoiceLast:=self;
  end else begin
   Previous:=nil;
   AudioEngine.VoiceFirst:=self;
   AudioEngine.VoiceLast:=self;
  end;
  Next:=nil;
  IsOnList:=true;
 end;
end;

procedure TpvAudioSoundSampleVoice.Dequeue;
begin
 if IsOnList then begin
  if assigned(Previous) then begin
   Previous.Next:=Next;
  end else if AudioEngine.VoiceFirst=self then begin
   AudioEngine.VoiceFirst:=Next;
  end;
  if assigned(Next) then begin
   Next.Previous:=Previous;
  end else if AudioEngine.VoiceLast=self then begin
   AudioEngine.VoiceLast:=Previous;
  end;
  Previous:=nil;
  Next:=nil;
  IsOnList:=false;
 end;
end;

procedure TpvAudioSoundSampleVoice.Init(AVolume,APanning,ARate:TpvFloat);
begin
 Active:=true;
 KeyOff:=false;
 Backwards:=false;
 Volume:=round(Clamp(AVolume,-1.0,1.0)*65536);
 Panning:=round(Clamp(APanning,-1.0,1.0)*65536);
 Rate:=ARate;
 DopplerRate:=1.0;
 Increment:=(Sample.SampleRate*round((Rate*DopplerRate)*TpvInt64($100000000))) div Sample.AudioEngine.SampleRate;
 IncrementCurrent:=Increment shl 16;
 IncrementLast:=Increment;
 IncrementIncrement:=0;
 IncrementRampingRemain:=0;
 IncrementRampingStepRemain:=0;
 Age:=0;
 Position:=0;
 if AudioEngine.SpatializationMode=SPATIALIZATION_HRTF then begin
  FillChar(HRTFLeftCoefs,SizeOf(TpvAudioHRTFCoefs),AnsiChar(#0));
  FillChar(HRTFRightCoefs,SizeOf(TpvAudioHRTFCoefs),AnsiChar(#0));
  FillChar(HRTFLeftCoefsCurrent,SizeOf(TpvAudioHRTFCoefs),AnsiChar(#0));
  FillChar(HRTFRightCoefsCurrent,SizeOf(TpvAudioHRTFCoefs),AnsiChar(#0));
  FillChar(HRTFLeftCoefsIncrement,SizeOf(TpvAudioHRTFCoefs),AnsiChar(#0));
  FillChar(HRTFRightCoefsIncrement,SizeOf(TpvAudioHRTFCoefs),AnsiChar(#0));
  FillChar(HRTFLeftHistory,SizeOf(TpvAudioHRTFHistory),AnsiChar(#0));
  FillChar(HRTFRightHistory,SizeOf(TpvAudioHRTFHistory),AnsiChar(#0));
  HRTFHistoryIndex:=0;
  HRTFLeftDelay:=0;
  HRTFRightDelay:=0;
  HRTFLeftDelayCurrent:=0;
  HRTFRightDelayCurrent:=0;
  HRTFLeftDelayLast:=0;
  HRTFRightDelayLast:=0;
  HRTFLeftDelayIncrement:=0;
  HRTFRightDelayIncrement:=0;
  HRTFRampingRemain:=0;
  HRTFRampingStepRemain:=0;
  HRTFLength:=AudioEngine.HRTFPreset^.irSize;
  HRTFMask:=HRTFLength-1;
 end;
 VolumeRampingRemain:=0;
 VolumeLeft:=0;
 VolumeRight:=0;
 VolumeLeftLast:=0;
 VolumeRightLast:=0;
 VolumeLeftCurrent:=0;
 VolumeRightCurrent:=0;
 VolumeLeftIncrement:=0;
 VolumeRightIncrement:=0;
 inc(LastLeft,NewLastLeft);
 inc(LastRight,NewLastRight);
 NewLastLeft:=0;
 NewLastRight:=0;
 if AudioEngine.SpatializationMode in [SPATIALIZATION_PSEUDO,SPATIALIZATION_HRTF] then begin
  SpatializationDelayLeft:=0;
  SpatializationDelayRight:=0;
  SpatializationDelayLeftLast:=SpatializationDelayLeft;
  SpatializationDelayRightLast:=SpatializationDelayRight;
  SpatializationDelayLeftCurrent:=SpatializationDelayLeft;
  SpatializationDelayRightCurrent:=SpatializationDelayRight;
  SpatializationDelayLeftIncrement:=0;
  SpatializationDelayRightIncrement:=0;
  SpatializationDelayRampingRemain:=0;
{end;
 if AudioEngine.SpatializationMode=SPATIALIZATION_PSEUDO then begin}
  SpatializationLowPassLeftCoef:=LowPassLength shl LowPassShift;
  SpatializationLowPassRightCoef:=LowPassLength shl LowPassShift;
  SpatializationLowPassLeftLastCoef:=SpatializationLowPassLeftCoef;
  SpatializationLowPassRightLastCoef:=SpatializationLowPassRightCoef;
  SpatializationLowPassLeftCurrentCoef:=SpatializationLowPassLeftCoef;
  SpatializationLowPassRightCurrentCoef:=SpatializationLowPassRightCoef;
  SpatializationLowPassLeftIncrementCoef:=0;
  SpatializationLowPassRightIncrementCoef:=0;
  SpatializationLowPassLeftHistory[0]:=0;
  SpatializationLowPassLeftHistory[1]:=0;
  SpatializationLowPassRightHistory[0]:=0;
  SpatializationLowPassRightHistory[1]:=0;
  SpatializationLowPassRampingRemain:=0;
 end;
 RampingSamples:=AudioEngine.RampingSamples;
end;

procedure TpvAudioSoundSampleVoice.UpdateSpatialization;
 function LowPassCoef(Gain,cw:TpvFloat):TpvFloat;
 begin
  if Gain<0.01 then begin
   Gain:=0.01;
  end;
  if Gain<0.9999 then begin
   result:=((1.0-(Gain*cw))-sqrt((2.0*(Gain*(1.0-cw)))-(sqr(Gain)*(1.0-sqr(cw)))))/(1.0-Gain);
   if result<-1.0 then begin
    result:=-1.0;
   end else if result>1.0 then begin
    result:=1.0;
   end;
  end else begin
   result:=0.0;
  end;
 end;
const LeftSpeakerAngle=-(pi*0.5);
      RightSpeakerAngle=(pi*0.5);
var Distance,ClampedDistance,AttenuationDistance,Attenuation,Spatialization,SpatializationVolume,SpatializationDelay,
    LeftHFGain,RightHFGain,Gain,Factor,SpeedOfSound,DopplerListener,DopplerSource,Angle,DirectionGain,
    Elevation,Azimuth,Delta{,Scale,ConeVolume,ConeHF}:TpvFloat;
    SourceVector,NormalizedSourceVector,RotatedSourceVector,Direction,SourceToListenerVector:TpvVector3;
    Counter:TpvInt32;
    DoIt,IsLocal:boolean;
begin
 SourceVector:=SpatializationOrigin-Sample.AudioEngine.ListenerOrigin;

 IsLocal:=SpatializationOrigin=Sample.AudioEngine.ListenerOrigin;

 Distance:=SourceVector.Length;

 NormalizedSourceVector:=SourceVector.Normalize;

 ClampedDistance:=Clamp(Distance,MinDistance,MaxDistance);
 AttenuationDistance:=MinDistance+(AttenuationRollOff*(ClampedDistance-MinDistance));
 if AttenuationDistance>0.0 then begin
  Attenuation:=MinDistance/AttenuationDistance;
 end else begin
  Attenuation:=1.0;
 end;

{Angle:=(ArcCos(Vector3Dot(NormalizedSourceVector,PVector3(TpvPointer(@Sample.AudioEngine.ListenerMatrix[2,0]))^)*ConeScale)*RAD2DEG)*2.0;
 if (Angle>InnerAngle) and (Angle<=OuterAngle) then begin
  Scale:=(Angle-InnerAngle)/(OuterAngle-InnerAngle);
  ConeVolume:=FloatLerp(1.0,OuterGain,Scale);
  ConeHF:=FloatLerp(1.0,OuterGainHF,Scale);
 end else if Angle>OuterAngle then begin
  ConeVolume:=OuterGain;
  ConeHF:=OuterGainHF;
 end else begin
  ConeVolume:=1.0;
  ConeHF:=1.0;
 end;}

 SpatializationVolume:=Attenuation{*ConeVolume};
 if SpatializationVolume<0.0 then begin
  SpatializationVolume:=0.0;
 end else if SpatializationVolume>1.0 then begin
  SpatializationVolume:=1.0;
 end;

 RotatedSourceVector:=Sample.AudioEngine.ListenerMatrix.MulBasis(NormalizedSourceVector).Normalize;

 RampingSamples:=AudioEngine.RampingSamples;

 if IsLocal then begin
  DirectionGain:=1.0;
 end else begin
  DirectionGain:=sqrt(sqr(RotatedSourceVector.x)+sqr(RotatedSourceVector.z));
 end;

 if IsLocal then begin
  Angle:=0.0;
 end else begin
  if abs(RotatedSourceVector.x)>EPSILON then begin
   // x<>0
   if abs(RotatedSourceVector.z)>EPSILON then begin // x<>0 z<>0
    Angle:=ArcTan2(RotatedSourceVector.x,-RotatedSourceVector.z);
   end else begin // x<>0 z=0
    if RotatedSourceVector.x<0 then begin
     Angle:=pi*0.5;
    end else begin
     Angle:=-(pi*0.5);
    end;
   end;
  end else begin
   // x=0
   if RotatedSourceVector.z>EPSILON then begin // x=0 z<0
    Angle:=-pi;
   end else begin // x=0 z>=0
    Angle:=0.0;
   end;
  end;
 end;

 DoIt:=false;
 Direction:=RotatedSourceVector;
 if Age=0 then begin
  SpatializationVolumeLast:=SpatializationVolume;
  LastDirection:=Direction;
 end else begin
  Delta:=CalculateDelta(SpatializationVolumeLast,SpatializationVolume,LastDirection,Direction);
  if Delta>0.001 then begin
   DoIt:=true;
   RampingSamples:=trunc(Max(floor((Delta*(AudioEngine.SampleRate*0.015))+0.5),1.0));
   SpatializationVolumeLast:=SpatializationVolume;
   LastDirection:=Direction;
  end;
 end;

 if IsLocal then begin
  Spatialization:=0.0;
 end else begin
  Spatialization:=Angle;
  if (Spatialization>=LeftSpeakerAngle) and (Spatialization<RightSpeakerAngle) then begin
   Spatialization:=(Angle-(LeftSpeakerAngle+((RightSpeakerAngle-LeftSpeakerAngle)*0.5)))/(RightSpeakerAngle-LeftSpeakerAngle);
  end else begin
   if Spatialization<LeftSpeakerAngle then begin
    Spatialization:=((Angle+(pi*2.0))-LeftSpeakerAngle)/((LeftSpeakerAngle-RightSpeakerAngle)+(pi*2.0));
   end else begin
    Spatialization:=(Angle-LeftSpeakerAngle)/((LeftSpeakerAngle-RightSpeakerAngle)+(pi*2.0));
   end;
  end;
 end;

//Spatialization:=RotatedSourceVector.x;
 if Spatialization<-1.0 then begin
  Spatialization:=-1.0;
 end else if Spatialization>1.0 then begin
  Spatialization:=1.0;
 end;
 case AudioEngine.SpatializationMode of
  SPATIALIZATION_HRTF:begin
   MulLeft:=round(Clamp(SpatializationVolume,-1.0,1.0)*32768.0);
   MulRight:=MulLeft;
  end;
  else begin
   MulLeft:=round(Clamp(SpatializationVolume*DirectionGain*sin(HalfPI*Min(Max(0.0,0.5*(1.0-(Spatialization*HF_DAMP_FACTOR))),1.0)),-1.0,1.0)*32768.0);
   MulRight:=round(Clamp(SpatializationVolume*DirectionGain*sin(HalfPI*Min(Max(0.0,0.5*(1.0+(Spatialization*HF_DAMP_FACTOR))),1.0)),-1.0,1.0)*32768.0);
  end;
 end;

 LeftHFGain:=1.0;
 RightHFGain:=1.0;

 case AudioEngine.SpatializationMode of
  SPATIALIZATION_PSEUDO:begin
   SpatializationDelayLeft:=0;
   SpatializationDelayRight:=0;
   if AudioEngine.ListenerUnderwater then begin
    SpatializationDelay:=AudioEngine.SpatializationDelayUnderwater;
   end else begin
    SpatializationDelay:=AudioEngine.SpatializationDelayAir;
   end;
   if Spatialization<0 then begin
    RightHFGain:=1.0+(Spatialization*HF_DAMP_HALF);
    SpatializationDelayRight:=round((SpatializationDelay*(-Spatialization))*SpatializationDelayLength);
   end else if Spatialization>0 then begin
    LeftHFGain:=1.0-(Spatialization*HF_DAMP_HALF);
    SpatializationDelayLeft:=round((SpatializationDelay*Spatialization)*SpatializationDelayLength);
   end;
   if RotatedSourceVector.z>0 then begin
    Gain:=1.0-(RotatedSourceVector.z*HF_DAMP);
    LeftHFGain:=LeftHFGain*Gain;
    RightHFGain:=RightHFGain*Gain;
   end;
  end;
  SPATIALIZATION_HRTF:begin
   if Distance>EPSILON then begin
    Elevation:=ArcSin(Clamp(RotatedSourceVector.y,-1.0,1.0));
    Azimuth:=Angle;
   end else begin
    Elevation:=0.0;
    Azimuth:=0.0;
   end;
   if Age=0 then begin
    AudioEngine.GetLerpedHRTFCoefs(Elevation,Azimuth,HRTFLeftCoefs,HRTFRightCoefs,HRTFLeftDelay,HRTFRightDelay);
    HRTFRampingRemain:=0;
    HRTFLeftCoefsCurrent:=HRTFLeftCoefs;
    HRTFRightCoefsCurrent:=HRTFRightCoefs;
    for Counter:=0 to HRTFMask do begin
     HRTFLeftCoefsIncrement[Counter]:=0;
     HRTFRightCoefsIncrement[Counter]:=0;
    end;
    SpatializationDelayLeft:=HRTFLeftDelay;
    SpatializationDelayRight:=HRTFRightDelay;
   end else begin
    if DoIt then begin
     HRTFRampingRemain:=RampingSamples;
     AudioEngine.GetLerpedHRTFCoefs(Elevation,Azimuth,HRTFLeftCoefs,HRTFRightCoefs,HRTFLeftDelay,HRTFRightDelay);
     for Counter:=0 to HRTFMask do begin
      HRTFLeftCoefsIncrement[Counter]:=(HRTFLeftCoefs[Counter]-HRTFLeftCoefsCurrent[Counter]) div HRTFRampingRemain;
      HRTFRightCoefsIncrement[Counter]:=(HRTFRightCoefs[Counter]-HRTFRightCoefsCurrent[Counter]) div HRTFRampingRemain;
     end;
     SpatializationDelayLeft:=HRTFLeftDelay;
     SpatializationDelayRight:=HRTFRightDelay;
    end;
   end;
  end;
 end;

 if AudioEngine.SpatializationMode in [SPATIALIZATION_PSEUDO,SPATIALIZATION_HRTF] then begin
  if Distance>MinAbsorptionDistance then begin                                                     
   Factor:=power(AirAbsorptionGainHF,AirAbsorptionFactor*((Clamp(Distance,MinAbsorptionDistance,MaxAbsorptionDistance)-MinAbsorptionDistance)*WorldUnitsToMeters));
   LeftHFGain:=LeftHFGain*Factor;
   RightHFGain:=RightHFGain*Factor;
  end;
  SpatializationLowPassLeftCoef:=round(LowPassCoef(LeftHFGain,AudioEngine.SpatializationLowPassCW)*(LowPassLength shl LowPassShift));
  SpatializationLowPassRightCoef:=round(LowPassCoef(RightHFGain,AudioEngine.SpatializationLowPassCW)*(LowPassLength shl LowPassShift));
 end;

 Factor:=DopplerFactor;
 if Factor>0.0 then begin
  if AudioEngine.ListenerUnderwater then begin
   SpeedOfSound:=SpeedOfSoundUnderwater;
  end else begin
   SpeedOfSound:=SpeedOfSoundAir;
  end;
  SpeedOfSound:=SpeedOfSound*DopplerVelocity;
  if (SpeedOfSound>0.0) and (SpeedOfSound<1.0) then begin
   Factor:=Factor/SpeedOfSound;
   SpeedOfSound:=1.0;
  end;
  SourceToListenerVector:=(SpatializationOrigin-Sample.AudioEngine.ListenerOrigin).Normalize;
  DopplerListener:=Min(AudioEngine.ListenerVelocity.Dot(SourceToListenerVector)*Factor,SpeedOfSound/Factor);
  DopplerSource:=Min(SpatializationVelocity.Dot(SourceToListenerVector)*Factor,SpeedOfSound/Factor);
  DopplerRate:=Clamp(SpeedOfSound+DopplerListener,1.0,(SpeedOfSound*2.0)-1.0)/
               Clamp(SpeedOfSound+DopplerSource,1.0,(SpeedOfSound*2.0)-1.0);
  Increment:=(TpvInt64(Sample.SampleRate)*round((Rate*DopplerRate)*TpvInt64($100000000))) div AudioEngine.SampleRate;
 end;
end;

function TpvAudioSoundSampleVoice.GetSampleLength(CountSamplesValue:TpvInt32):TpvInt32;
var SmpInc,SmpLen,SmpLoopStart,SmpLoopEnd,CountSamples,SampleBufferSize,Difference:TpvInt64;
    LoopMode:TpvInt32;
begin
 SmpInc:=IncrementCurrent shr 16;
 if SmpInc=0 then begin
  result:=0;
 end else begin
  SmpLen:=TpvInt64(Sample.SampleLength) shl 32;
  if (Sample.SustainLoop.Mode<>SoundLoopModeNONE) and not KeyOff then begin
   LoopMode:=Sample.SustainLoop.Mode;
   SmpLoopStart:=TpvInt64(Sample.SustainLoop.StartSample) shl 32;
   SmpLoopEnd:=TpvInt64(Sample.SustainLoop.EndSample) shl 32;
  end else if Sample.Loop.Mode<>SoundLoopModeNONE then begin
   LoopMode:=Sample.Loop.Mode;
   SmpLoopStart:=TpvInt64(Sample.Loop.StartSample) shl 32;
   SmpLoopEnd:=TpvInt64(Sample.Loop.EndSample) shl 32;
  end else begin
   LoopMode:=SoundLoopModeNONE;
   SmpLoopStart:=0;
   SmpLoopEnd:=SmpLen;
  end;
  if LoopMode<>SoundLoopModeNONE then begin
   if Position<SmpLoopStart then begin
    if Backwards then begin
     if LoopMode=SoundLoopModePINGPONG then begin
      Position:=SmpLoopStart-(SmpLoopStart-Position);
      Backwards:=false;
      if (Position<SmpLoopStart) or (Position>=((SmpLoopStart+SmpLoopEnd) div 2)) then begin
       Position:=SmpLoopStart;
      end;
     end else if LoopMode=SoundLoopModeBACKWARD then begin
      Position:=SmpLen-(SmpLoopStart-Position);
      if Position>=SmpLoopEnd then begin
       Position:=Position+(SmpLoopStart-SmpLoopEnd);
       if Position<SmpLoopStart then begin
        Position:=SmpLoopStart;
       end;
      end;
     end else begin
      Position:=SmpLoopEnd-(SmpLoopStart-Position);
      Backwards:=false;
      if Position>=SmpLoopEnd then begin
       Position:=Position+(SmpLoopStart-SmpLoopEnd);
       if Position<SmpLoopStart then begin
        Position:=SmpLoopStart;
       end;
      end;
     end;
    end else begin
     if Position<0 then begin
      Position:=0;
     end;
    end;
   end else if Position>=SmpLoopEnd then begin
    if LoopMode=SoundLoopModeBACKWARD then begin
     Backwards:=true;
    end else if LoopMode=SoundLoopModePINGPONG then begin
     Position:=SmpLoopEnd-(Position-SmpLoopEnd);
     Backwards:=true;
     if (Position<SmpLoopStart) or (Position>=((SmpLoopStart+SmpLoopEnd) div 2)) then begin
      Position:=SmpLoopStart;
     end;
    end else begin
     Position:=Position+(SmpLoopStart-SmpLoopEnd);
     if Position<SmpLoopStart then begin
      Position:=SmpLoopStart;
     end;
    end;
   end;
  end;
  if (Position<0) OR (Position>=SmpLen) OR
     ((Position<SmpLoopStart) AND
      ((Position<0) OR Backwards)) then begin
   result:=0;
   exit;
  end;
  CountSamples:=CountSamplesValue;
  SampleBufferSize:=SpatializationlRemainFactor div ((SmpInc div PositionFactor)+1);
  if SampleBufferSize<2 then begin
   SampleBufferSize:=2;
  end;
  if CountSamplesValue>SampleBufferSize then begin
   CountSamplesValue:=SampleBufferSize;
  end;
  if Backwards then begin
   Difference:=(Position-(SmpInc*(TpvInt64(CountSamplesValue)-1)));
   if Difference<SmpLoopStart then begin
    CountSamples:=(((Position-SmpLoopStart)-1) div SmpInc)+1;
   end;
  end else begin
   Difference:=(Position+(SmpInc*(TpvInt64(CountSamplesValue)-1)));
   if Difference>=SmpLoopEnd then begin
    CountSamples:=(((SmpLoopEnd-Position)-1) div SmpInc)+1;
   end;
  end;
  if CountSamples<=1 then begin
   result:=1;
   exit;
  end else if CountSamples>CountSamplesValue then begin
   result:=CountSamplesValue;
   exit;
  end;
  result:=CountSamples;
 end;
end;

procedure TpvAudioSoundSampleVoice.PreClickRemoval(Buffer:TpvPointer);
var LocalLeft,LocalRight,Counter:TpvInt32;
    Buf:PpvAudioInt32;
begin
 LocalLeft:=LastLeft;
 LocalRight:=LastRight;
 if (LocalLeft<>0) or (LocalRight<>0) then begin
  Buf:=TpvPointer(Buffer);
  for Counter:=1 to Sample.AudioEngine.BufferSamples do begin
   dec(LocalLeft,SARLongint(LocalLeft+(SARLongint(-LocalLeft,31) and $ff),8));
   dec(LocalRight,SARLongint(LocalRight+(SARLongint(-LocalRight,31) and $ff),8));
   Buf^:=Buf^+SARLongint(LocalLeft,12);
   inc(Buf);
   Buf^:=Buf^+SARLongint(LocalRight,12);
   inc(Buf);
  end;
  LastLeft:=LocalLeft;
  LastRight:=LocalRight;
 end;
end;

procedure TpvAudioSoundSampleVoice.PostClickRemoval(Buffer:TpvPointer;Remain:TpvInt32);
var LocalNewLastLeft,LocalNewLastRight:TpvInt32;
    Buf:PpvAudioInt32;
begin
 Buf:=Buffer;
 LocalNewLastLeft:=NewLastLeft;
 LocalNewLastRight:=NewLastRight;
 if (Remain>0) and ((LocalNewLastLeft<>0) or (LocalNewLastRight<>0)) then begin
  while Remain>0 do begin
   dec(LocalNewLastLeft,SARLongint(LocalNewLastLeft+(SARLongint(-LocalNewLastLeft,31) and $ff),8));
   dec(LocalNewLastRight,SARLongint(LocalNewLastRight+(SARLongint(-LocalNewLastRight,31) and $ff),8));
   Buf^:=Buf^+SARLongint(LocalNewLastLeft,12);
   inc(Buf);
   Buf^:=Buf^+SARLongint(LocalNewLastRight,12);
   inc(Buf);
   dec(Remain);
  end;
 end;
 NewLastLeft:=LocalNewLastLeft;
 NewLastRight:=LocalNewLastRight;
end;

procedure TpvAudioSoundSampleVoice.MixProcSpatializationHRTF(Buffer:TpvPointer;ToDo:TpvInt32);
var vl,vr,vli,vri,pll,plr,plli,plri,pdl,pdr,pdli,pdri,pdlip,pdrip,pdm,hhi,hm,p,m,s,v,i:TpvInt32;
    pllh,plrh:TpvAudioSoundSampleVoiceLowPassHistory;
    pdlp,pdrp,hl,hr,hli,hri,hlh,hrh:PpvAudioInt32s;
    Buf:PpvAudioInt32;
    p64,i64:TpvInt64;
    d:PpvAudioSoundSampleValues;
begin
 Buf:=TpvPointer(Buffer);
 d:=Sample.Data;
 vl:=VolumeLeftCurrent;
 vr:=VolumeRightCurrent;
 vli:=VolumeLeftIncrement;
 vri:=VolumeRightIncrement;
 pll:=SpatializationLowPassLeftCurrentCoef;
 plr:=SpatializationLowPassRightCurrentCoef;
 plli:=SpatializationLowPassLeftIncrementCoef;
 plri:=SpatializationLowPassRightIncrementCoef;
 pllh:=SpatializationLowPassLeftHistory;
 plrh:=SpatializationLowPassRightHistory;
 pdl:=SpatializationDelayLeftCurrent;
 pdr:=SpatializationDelayRightCurrent;
 pdli:=SpatializationDelayLeftIncrement;
 pdri:=SpatializationDelayRightIncrement;
 pdlip:=SpatializationDelayLeftIndex;
 pdrip:=SpatializationDelayRightIndex;
 pdlp:=@SpatializationDelayLeftLine[0];
 pdrp:=@SpatializationDelayRightLine[0];
 pdm:=AudioEngine.SpatializationDelayMask;
 hl:=@HRTFLeftCoefsCurrent[0];
 hr:=@HRTFRightCoefsCurrent[0];
 hli:=@HRTFLeftCoefsIncrement[0];
 hri:=@HRTFRightCoefsIncrement[0];
 hlh:=@HRTFLeftHistory[0];
 hrh:=@HRTFRightHistory[0];
 hhi:=HRTFHistoryIndex;
 hm:=HRTFMask;
 p64:=Position;
 i64:=MixIncrement;
 while ToDo>0 do begin
  dec(ToDo);
  p:=TpvUInt32(p64 shr PositionShift) shl 1;
  m:=(TpvUInt32(p64 and $ffffffff) shr (PositionShift-12)) and $fff;
  s:=d^[p];
{$ifdef UseDIV}
  inc(s,((d^[p+2]-s)*m) div 4096);
{$else}
  inc(s,SARLongint((d^[p+2]-s)*m,12));
{$endif}
{$ifdef UseDIV}
  inc(s,((pllh[0]-s)*(pll div LowPassShiftLength)) div LowPassLength);
{$else}
  inc(s,SARLongint((pllh[0]-s)*SARLongint(pll,LowPassShift),LowPassBits));
{$endif}
  pllh[0]:=s;
{$ifdef UseDIV}
  inc(s,((pllh[1]-s)*(pll div LowPassShiftLength)) div LowPassLength);
{$else}
  inc(s,SARLongint((pllh[1]-s)*SARLongint(pll,LowPassShift),LowPassBits));
{$endif}
  pllh[1]:=s;
  pdlp^[pdlip]:=s;
{$ifdef UseDIV}
  v:=pdl div SpatializationDelayLength;
{$else}
  v:=SARLongint(pdl,SpatializationDelayShift);
{$endif}
  s:=pdlp^[(pdlip-v) and pdm];
{$ifdef UseDIV}
  inc(s,((pdlp^[(pdlip-(v+1)) and pdm]-s)*((pdl and SpatializationDelayMask) shr 4)) div (SpatializationDelayLength shr 4));
{$else}
  inc(s,SARLongint((pdlp^[(pdlip-(v+1)) and pdm]-s)*((pdl and SpatializationDelayMask) shr 4),SpatializationDelayShift-4));
{$endif}
{$ifdef UseDIV}
  s:=(s*(vl div 32768)) div 32768;
{$else}
  s:=SARLongint(s*SARLongint(vl,15),15);
{$endif}
  hlh^[hhi and hm]:=0;
  for i:=0 to hm do begin
   v:=(hhi+1+i) and hm;
{$ifdef UseDIV}
   inc(hlh^[v],((hl^[i] div 32768)*s) div 32768);
{$else}
   inc(hlh^[v],SARLongint(SARLongint(hl^[i],15)*s,15));
{$endif}
   inc(hl^[i],hli^[i]);
  end;
  inc(Buf^,hlh^[(hhi+1) and hm]);
  inc(Buf);
  s:=d^[p+1];
{$ifdef UseDIV}
  inc(s,((d^[p+3]-s)*m) div 4096);
{$else}
  inc(s,SARLongint((d^[p+3]-s)*m,12));
{$endif}
{$ifdef UseDIV}
  inc(s,((plrh[0]-s)*(plr div LowPassShiftLength)) div LowPassLength);
{$else}
  inc(s,SARLongint((plrh[0]-s)*SARLongint(plr,LowPassShift),LowPassBits));
{$endif}
  plrh[0]:=s;
{$ifdef UseDIV}
  inc(s,((plrh[1]-s)*(plr div LowPassShiftLength)) div LowPassLength);
{$else}
  inc(s,SARLongint((plrh[1]-s)*SARLongint(plr,LowPassShift),LowPassBits));
{$endif}
  plrh[1]:=s;
  pdrp^[pdrip]:=s;
{$ifdef UseDIV}
  v:=pdr div SpatializationDelayLength;
{$else}
  v:=SARLongint(pdr,SpatializationDelayShift);
{$endif}
  s:=pdrp^[(pdrip-v) and pdm];
{$ifdef UseDIV}
  inc(s,((pdrp^[(pdrip-(v+1)) and pdm]-s)*((pdr and SpatializationDelayMask) shr 4)) div (SpatializationDelayLength shr 4));
{$else}
  inc(s,SARLongint((pdrp^[(pdrip-(v+1)) and pdm]-s)*((pdr and SpatializationDelayMask) shr 4),SpatializationDelayShift-4));
{$endif}
{$ifdef UseDIV}
  s:=(s*(vr div 32768)) div 32768;
{$else}
  s:=SARLongint(s*SARLongint(vr,15),15);
{$endif}
  hrh^[hhi and hm]:=0;
  for i:=0 to hm do begin
   v:=(hhi+1+i) and hm;
{$ifdef UseDIV}
   inc(hrh^[v],((hr^[i] div 32768)*s) div 32768);
{$else}
   inc(hrh^[v],SARLongint(SARLongint(hr^[i],15)*s,15));
{$endif}
   inc(hr^[i],hri^[i]);
  end;
  inc(Buf^,hrh^[(hhi+1) and hm]);
  inc(Buf);
  inc(vl,vli);
  inc(vr,vri);
  inc(pll,plli);
  inc(plr,plri);
  inc(pdl,pdli);
  inc(pdr,pdri);
  pdlip:=(pdlip+1) and pdm;
  pdrip:=(pdrip+1) and pdm;
  hhi:=(hhi+1) and hm;
  inc(p64,i64);
 end;
 VolumeLeftCurrent:=vl;
 VolumeRightCurrent:=vr;
 SpatializationLowPassLeftCurrentCoef:=pll;
 SpatializationLowPassRightCurrentCoef:=plr;
 SpatializationLowPassLeftHistory:=pllh;
 SpatializationLowPassRightHistory:=plrh;
 SpatializationDelayLeftCurrent:=pdl;
 SpatializationDelayRightCurrent:=pdr;
 SpatializationDelayLeftIndex:=pdlip;
 SpatializationDelayRightIndex:=pdrip;
 HRTFHistoryIndex:=hhi;
 Position:=p64;
end;

procedure TpvAudioSoundSampleVoice.MixProcSpatializationPSEUDO(Buffer:TpvPointer;ToDo:TpvInt32);
var vl,vr,vli,vri,pll,plr,plli,plri,pdl,pdr,pdli,pdri,pdlip,pdrip,pdm,p,m,s,v:TpvInt32;
    pllh,plrh:TpvAudioSoundSampleVoiceLowPassHistory;
    pdlp,pdrp:PpvAudioInt32s;
    Buf:PpvAudioInt32;
    p64,i64:TpvInt64;
    d:PpvAudioSoundSampleValues;
begin
 Buf:=TpvPointer(Buffer);
 d:=Sample.Data;
 vl:=VolumeLeftCurrent;
 vr:=VolumeRightCurrent;
 vli:=VolumeLeftIncrement;
 vri:=VolumeRightIncrement;
 pll:=SpatializationLowPassLeftCurrentCoef;
 plr:=SpatializationLowPassRightCurrentCoef;
 plli:=SpatializationLowPassLeftIncrementCoef;
 plri:=SpatializationLowPassRightIncrementCoef;
 pllh:=SpatializationLowPassLeftHistory;
 plrh:=SpatializationLowPassRightHistory;
 pdl:=SpatializationDelayLeftCurrent;
 pdr:=SpatializationDelayRightCurrent;
 pdli:=SpatializationDelayLeftIncrement;
 pdri:=SpatializationDelayRightIncrement;
 pdlip:=SpatializationDelayLeftIndex;
 pdrip:=SpatializationDelayRightIndex;
 pdlp:=@SpatializationDelayLeftLine[0];
 pdrp:=@SpatializationDelayRightLine[0];
 pdm:=AudioEngine.SpatializationDelayMask;
 p64:=Position;
 i64:=MixIncrement;
 while ToDo>0 do begin
  dec(ToDo);
  p:=TpvUInt32(p64 shr PositionShift) shl 1;
  m:=(TpvUInt32(p64 and $ffffffff) shr (PositionShift-12)) and $fff;
  s:=d^[p];
{$ifdef UseDIV}
  inc(s,((d^[p+2]-s)*m) div 4096);
{$else}
  inc(s,SARLongint((d^[p+2]-s)*m,12));
{$endif}
{$ifdef UseDIV}
  s:=(s*(vl div 32768)) div 32768;
{$else}
  s:=SARLongint(s*SARLongint(vl,15),15);
{$endif}
{$ifdef UseDIV}
  inc(s,((pllh[0]-s)*(pll div LowPassShiftLength)) div LowPassLength);
{$else}
  inc(s,SARLongint((pllh[0]-s)*SARLongint(pll,LowPassShift),LowPassBits));
{$endif}
  pllh[0]:=s;
{$ifdef UseDIV}
  inc(s,((pllh[1]-s)*(pll div LowPassShiftLength)) div LowPassLength);
{$else}
  inc(s,SARLongint((pllh[1]-s)*SARLongint(pll,LowPassShift),LowPassBits));
{$endif}
  pllh[1]:=s;
  pdlp^[pdlip]:=s;
{$ifdef UseDIV}
  v:=pdl div SpatializationDelayLength;
{$else}
  v:=SARLongint(pdl,SpatializationDelayShift);
{$endif}
  s:=pdlp^[(pdlip-v) and pdm];
{$ifdef UseDIV}
  inc(s,((pdlp^[(pdlip-(v+1)) and pdm]-s)*((pdl and SpatializationDelayMask) shr 4)) div (SpatializationDelayLength shr 4));
{$else}
  inc(s,SARLongint((pdlp^[(pdlip-(v+1)) and pdm]-s)*((pdl and SpatializationDelayMask) shr 4),SpatializationDelayShift-4));
{$endif}
  inc(Buf^,s);
  inc(Buf);
  s:=d^[p+1];
{$ifdef UseDIV}
  inc(s,((d^[p+3]-s)*m) div 4096);
{$else}
  inc(s,SARLongint((d^[p+3]-s)*m,12));
{$endif}
{$ifdef UseDIV}
  s:=(s*(vr div 32768)) div 32768;
{$else}
  s:=SARLongint(s*SARLongint(vr,15),15);
{$endif}
{$ifdef UseDIV}
  inc(s,((plrh[0]-s)*(plr div LowPassShiftLength)) div LowPassLength);
{$else}
  inc(s,SARLongint((plrh[0]-s)*SARLongint(plr,LowPassShift),LowPassBits));
{$endif}
  plrh[0]:=s;
{$ifdef UseDIV}
  inc(s,((plrh[1]-s)*(plr div LowPassShiftLength)) div LowPassLength);
{$else}
  inc(s,SARLongint((plrh[1]-s)*SARLongint(plr,LowPassShift),LowPassBits));
{$endif}
  plrh[1]:=s;
  pdrp^[pdrip]:=s;
{$ifdef UseDIV}
  v:=pdr div SpatializationDelayLength;
{$else}
  v:=SARLongint(pdr,SpatializationDelayShift);
{$endif}
  s:=pdrp^[(pdrip-v) and pdm];
{$ifdef UseDIV}
  inc(s,((pdrp^[(pdrip-(v+1)) and pdm]-s)*((pdr and SpatializationDelayMask) shr 4)) div (SpatializationDelayLength shr 4));
{$else}
  inc(s,SARLongint((pdrp^[(pdrip-(v+1)) and pdm]-s)*((pdr and SpatializationDelayMask) shr 4),SpatializationDelayShift-4));
{$endif}
  inc(Buf^,s);
  inc(Buf);
  inc(vl,vli);
  inc(vr,vri);
  inc(pll,plli);
  inc(plr,plri);
  inc(pdl,pdli);
  inc(pdr,pdri);
  pdlip:=(pdlip+1) and pdm;
  pdrip:=(pdrip+1) and pdm;
  inc(p64,i64);
 end;
 VolumeLeftCurrent:=vl;
 VolumeRightCurrent:=vr;
 SpatializationLowPassLeftCurrentCoef:=pll;
 SpatializationLowPassRightCurrentCoef:=plr;
 SpatializationLowPassLeftHistory:=pllh;
 SpatializationLowPassRightHistory:=plrh;
 SpatializationDelayLeftCurrent:=pdl;
 SpatializationDelayRightCurrent:=pdr;
 SpatializationDelayLeftIndex:=pdlip;
 SpatializationDelayRightIndex:=pdrip;
 Position:=p64;
end;

procedure TpvAudioSoundSampleVoice.MixProcVolumeRamping(Buffer:TpvPointer;ToDo:TpvInt32);
var vl,vr,vli,vri,p,m,s:TpvInt32;
    Buf:PpvAudioInt32;
    p64,i64:TpvInt64;
    d:PpvAudioSoundSampleValues;
begin
 Buf:=TpvPointer(Buffer);
 d:=Sample.Data;
 vl:=VolumeLeftCurrent;
 vr:=VolumeRightCurrent;
 vli:=VolumeLeftIncrement;
 vri:=VolumeRightIncrement;
 p64:=Position;
 i64:=MixIncrement;
 while ToDo>0 do begin
  dec(ToDo);
  p:=TpvUInt32(p64 shr PositionShift) shl 1;
  m:=(TpvUInt32(p64 and $ffffffff) shr (PositionShift-12)) and $fff;
  s:=d^[p];
{$ifdef UseDIV}
  inc(Buf^,(TpvInt32(s+(((d^[p+2]-s)*m) div 4096))*(vl div 32768)) div 32768);
{$else}
  inc(Buf^,SARLongint(TpvInt32(s+SARLongint((d^[p+2]-s)*m,12))*SARLongint(vl,15),15));
{$endif}
  inc(Buf);
  s:=d^[p+1];
{$ifdef UseDIV}
  inc(Buf^,(TpvInt32(s+(((d^[p+3]-s)*m) div 4096))*(vr div 32768)) div 32768);
{$else}
  inc(Buf^,SARLongint(TpvInt32(s+SARLongint((d^[p+3]-s)*m,12))*SARLongint(vr,15),15));
{$endif}
  inc(Buf);
  inc(vl,vli);
  inc(vr,vri);
  inc(p64,i64);
 end;
 VolumeLeftCurrent:=vl;
 VolumeRightCurrent:=vr;
 Position:=p64;
end;

procedure TpvAudioSoundSampleVoice.MixProcNormal(Buffer:TpvPointer;ToDo:TpvInt32);
var vl,vr,p,m,s:TpvInt32;
    Buf:PpvAudioInt32;
    p64,i64:TpvInt64;
    d:PpvAudioSoundSampleValues;
begin
 Buf:=TpvPointer(Buffer);
 d:=Sample.Data;
{$ifdef UseDIV}
 vl:=VolumeLeft div 32768;
 vr:=VolumeRight div 32768;
{$else}
 vl:=SARLongint(VolumeLeft,15);
 vr:=SARLongint(VolumeRight,15);
{$endif}
 p64:=Position;
 i64:=MixIncrement;
 while ToDo>0 do begin
  dec(ToDo);
  p:=TpvUInt32(p64 shr PositionShift) shl 1;
  m:=(TpvUInt32(p64 and $ffffffff) shr (PositionShift-12)) and $fff;
  s:=d^[p];
{$ifdef UseDIV}
  inc(Buf^,(TpvInt32(s+(((d^[p+2]-s)*m) div 4096))*vl) div 32768);
{$else}
  inc(Buf^,SARLongint(TpvInt32(s+SARLongint((d^[p+2]-s)*m,12))*vl,15));
{$endif}
  inc(Buf);
  s:=d^[p+1];
{$ifdef UseDIV}
  inc(Buf^,(TpvInt32(s+(((d^[p+3]-s)*m) div 4096))*vr) div 32768);
{$else}
  inc(Buf^,SARLongint(TpvInt32(s+SARLongint((d^[p+3]-s)*m,12))*vr,15));
{$endif}
  inc(Buf);
  inc(p64,i64);
 end;
 Position:=p64;
end;

procedure TpvAudioSoundSampleVoice.UpdateIncrementRamping;
begin
 if Age=0 then begin
  IncrementRampingRemain:=0;
  IncrementRampingStepRemain:=0;
  IncrementCurrent:=Increment shl 16;
  IncrementLast:=Increment;
  IncrementIncrement:=0;
 end else if IncrementLast<>Increment then begin
  IncrementRampingRemain:=RampingSamples;
  IncrementRampingStepRemain:=Min(Max(RampingSamples div 10,1),RampingSamples);
  IncrementLast:=Increment;
  IncrementIncrement:=((Increment shl 16)-IncrementCurrent) div IncrementRampingRemain;
 end;
end;

procedure TpvAudioSoundSampleVoice.UpdateVolumeRamping(MixVolume:TpvInt32);
var Pan:TpvInt32;
begin
 MixVolume:=SARLongint(SARLongint(Volume,1)*MixVolume,15);
 if Spatialization then begin
  VolumeLeft:=SARLongint(MixVolume*MulLeft,15);
  VolumeRight:=SARLongint(MixVolume*MulRight,15);
  if VolumeLeft<-32768 then begin
   VolumeLeft:=-32768;
  end else if VolumeLeft>=32768 then begin
   VolumeLeft:=32768;
  end;
  if VolumeRight<-32768 then begin
   VolumeRight:=-32768;
  end else if VolumeRight>=32768 then begin
   VolumeRight:=32768;
  end;
 end else begin
  Pan:=Panning+65536;
  if Pan<0 then begin
   Pan:=0;
  end else if Pan>=131072 then begin
   Pan:=131072;
  end;                                    
  VolumeLeft:=SARLongint(AudioEngine.PanningLUT[SARLongint(131072-Pan,1)]*MixVolume,15);
  VolumeRight:=SARLongint(AudioEngine.PanningLUT[SARLongint(Pan,1)]*MixVolume,15);
  if VolumeLeft<0 then begin
   VolumeLeft:=0;
  end else if VolumeLeft>=32768 then begin
   VolumeLeft:=32768;
  end;
  if VolumeRight<0 then begin
   VolumeRight:=0;
  end else if VolumeRight>=32768 then begin
   VolumeRight:=32768;
  end;
 end;
 VolumeLeft:=VolumeLeft shl 15;
 VolumeRight:=VolumeRight shl 15;
 if Age=0 then begin
  VolumeRampingRemain:=0;
  VolumeLeftLast:=VolumeLeft;
  VolumeRightLast:=VolumeRight;
  VolumeLeftCurrent:=VolumeLeft;
  VolumeRightCurrent:=VolumeRight;
  VolumeLeftIncrement:=0;
  VolumeRightIncrement:=0;
 end else if (VolumeLeftLast<>VolumeLeft) or (VolumeRightLast<>VolumeRight) then begin
  VolumeRampingRemain:=RampingSamples;
  VolumeLeftLast:=VolumeLeft;
  VolumeRightLast:=VolumeRight;
  VolumeLeftIncrement:=(VolumeLeft-VolumeLeftCurrent) div VolumeRampingRemain;
  VolumeRightIncrement:=(VolumeRight-VolumeRightCurrent) div VolumeRampingRemain;
 end;
end;

procedure TpvAudioSoundSampleVoice.UpdateSpatializationDelayRamping;
begin
 if Age=0 then begin
  SpatializationDelayRampingRemain:=0;
  SpatializationDelayLeftLast:=SpatializationDelayLeft;
  SpatializationDelayRightLast:=SpatializationDelayRight;
  SpatializationDelayLeftCurrent:=SpatializationDelayLeft;
  SpatializationDelayRightCurrent:=SpatializationDelayRight;
  SpatializationDelayLeftIncrement:=0;
  SpatializationDelayRightIncrement:=0;
  if AudioEngine.SpatializationDelayPowerOfTwo>0 then begin
   FillChar(SpatializationDelayLeftLine[0],AudioEngine.SpatializationDelayPowerOfTwo*SizeOf(TpvInt32),AnsiChar(#0));
   FillChar(SpatializationDelayRightLine[0],AudioEngine.SpatializationDelayPowerOfTwo*SizeOf(TpvInt32),AnsiChar(#0));
  end;
 end else if (SpatializationDelayLeftLast<>SpatializationDelayLeft) or (SpatializationDelayRightLast<>SpatializationDelayRight) then begin
  SpatializationDelayRampingRemain:=RampingSamples;
  SpatializationDelayLeftLast:=SpatializationDelayLeft;
  SpatializationDelayRightLast:=SpatializationDelayRight;
  SpatializationDelayLeftIncrement:=(SpatializationDelayLeft-SpatializationDelayLeftCurrent) div SpatializationDelayRampingRemain;
  SpatializationDelayRightIncrement:=(SpatializationDelayRight-SpatializationDelayRightCurrent) div SpatializationDelayRampingRemain;
 end;
end;

procedure TpvAudioSoundSampleVoice.UpdateSpatializationLowPassRamping;
begin
 if Age=0 then begin
  SpatializationLowPassRampingRemain:=0;
  SpatializationLowPassLeftLastCoef:=SpatializationLowPassLeftCoef;
  SpatializationLowPassRightLastCoef:=SpatializationLowPassRightCoef;
  SpatializationLowPassLeftCurrentCoef:=SpatializationLowPassLeftCoef;
  SpatializationLowPassRightCurrentCoef:=SpatializationLowPassRightCoef;
  SpatializationLowPassLeftIncrementCoef:=0;
  SpatializationLowPassRightIncrementCoef:=0;
  SpatializationLowPassLeftHistory[0]:=0;
  SpatializationLowPassLeftHistory[1]:=0;
  SpatializationLowPassRightHistory[0]:=0;
  SpatializationLowPassRightHistory[1]:=0;
 end else if (SpatializationLowPassLeftLastCoef<>SpatializationLowPassLeftCoef) or (SpatializationLowPassRightLastCoef<>SpatializationLowPassRightCoef) then begin
  SpatializationLowPassRampingRemain:=RampingSamples;
  SpatializationLowPassLeftLastCoef:=SpatializationLowPassLeftCoef;
  SpatializationLowPassRightLastCoef:=SpatializationLowPassRightCoef;
  SpatializationLowPassLeftIncrementCoef:=(SpatializationLowPassLeftCoef-SpatializationLowPassLeftCurrentCoef) div SpatializationLowPassRampingRemain;
  SpatializationLowPassRightIncrementCoef:=(SpatializationLowPassRightCoef-SpatializationLowPassRightCurrentCoef) div SpatializationLowPassRampingRemain;
 end;
end;

procedure TpvAudioSoundSampleVoice.MixTo(Buffer:PpvAudioSoundSampleValues;MixVolume:TpvInt32);
var Remain,ToDo,Counter:TpvInt32;
    Buf:PpvAudioInt32;
    BufEx:PpvAudioInt32s;
begin
 PreClickRemoval(Buffer);
 if Active then begin
  Buf:=TpvPointer(Buffer);

  if Spatialization then begin
   UpdateSpatialization;
  end;

  UpdateIncrementRamping;

  UpdateVolumeRamping(MixVolume);

  if Spatialization and (AudioEngine.SpatializationMode in [SPATIALIZATION_PSEUDO,SPATIALIZATION_HRTF]) then begin
   UpdateSpatializationDelayRamping;
   UpdateSpatializationLowPassRamping;
  end;

  if (VolumeRampingRemain or VolumeLeft or VolumeRight)=0 then begin
   if IncrementRampingRemain>0 then begin
    IncrementRampingStepRemain:=IncrementRampingRemain;
   end;
  end;

  NewLastLeft:=0;
  NewLastRight:=0;
  Remain:=Sample.AudioEngine.BufferSamples;
  while (Remain>0) and Active do begin

   ToDo:=Remain;
   if IncrementRampingRemain>0 then begin
    if ToDo>=IncrementRampingRemain then begin
     ToDo:=IncrementRampingRemain;
    end;
    if (IncrementRampingStepRemain>0) and (ToDo>=IncrementRampingStepRemain) then begin
     ToDo:=IncrementRampingStepRemain;
    end;
   end;
   if (VolumeRampingRemain>0) and (ToDo>=VolumeRampingRemain) then begin
    ToDo:=VolumeRampingRemain;
   end;
   if Spatialization and (AudioEngine.SpatializationMode in [SPATIALIZATION_PSEUDO,SPATIALIZATION_HRTF]) then begin
    case AudioEngine.SpatializationMode of
     SPATIALIZATION_PSEUDO:begin
      if (SpatializationLowPassRampingRemain>0) and (ToDo>=SpatializationLowPassRampingRemain) then begin
       ToDo:=SpatializationLowPassRampingRemain;
      end;
     end;
     SPATIALIZATION_HRTF:begin
      if (HRTFRampingRemain>0) and (ToDo>=HRTFRampingRemain) then begin
       ToDo:=HRTFRampingRemain;
      end;
     end;
    end;
    if (SpatializationDelayRampingRemain>0) and (ToDo>=SpatializationDelayRampingRemain) then begin
     ToDo:=SpatializationDelayRampingRemain;
    end;
   end;

   ToDo:=GetSampleLength(ToDo);
   if ToDo=0 then begin
    Active:=false;
    break;
   end;

   dec(Remain,ToDo);
   inc(Age,ToDo);

   if Backwards then begin
    MixIncrement:=-(IncrementCurrent shr 16);
   end else begin
    MixIncrement:=IncrementCurrent shr 16;
   end;

   if IncrementRampingRemain>0 then begin
    inc(IncrementCurrent,IncrementIncrement*ToDo);
   end;

   if (VolumeRampingRemain or VolumeLeftCurrent or VolumeRightCurrent)=0 then begin
    inc(Position,MixIncrement*ToDo);
    if Spatialization and (AudioEngine.SpatializationMode in [SPATIALIZATION_PSEUDO,SPATIALIZATION_HRTF]) then begin
     if (AudioEngine.SpatializationMode=SPATIALIZATION_HRTF) and (HRTFRampingRemain>0) then begin
      for Counter:=0 to HRTFMask do begin
       inc(HRTFLeftCoefs[Counter],HRTFLeftCoefsIncrement[Counter]*ToDo);
       inc(HRTFRightCoefs[Counter],HRTFRightCoefsIncrement[Counter]*ToDo);
      end;
     end;
     inc(SpatializationLowPassLeftCurrentCoef,SpatializationLowPassLeftIncrementCoef*ToDo);
     inc(SpatializationLowPassRightCurrentCoef,SpatializationLowPassRightIncrementCoef*ToDo);
     SpatializationLowPassLeftHistory[0]:=0;
     SpatializationLowPassLeftHistory[1]:=0;
     SpatializationLowPassRightHistory[0]:=0;
     SpatializationLowPassRightHistory[1]:=0;
     inc(SpatializationDelayLeftCurrent,SpatializationDelayLeftIncrement*ToDo);
     inc(SpatializationDelayRightCurrent,SpatializationDelayRightIncrement*ToDo);
     Counter:=ToDo;
     if Counter>AudioEngine.SpatializationDelayPowerOfTwo then begin
      Counter:=AudioEngine.SpatializationDelayPowerOfTwo;
     end;
     while Counter>0 do begin
      dec(Counter);
      SpatializationDelayLeftLine[SpatializationDelayLeftIndex]:=0;
      SpatializationDelayLeftIndex:=(SpatializationDelayLeftIndex+1) and AudioEngine.SpatializationDelayMask;
      SpatializationDelayRightLine[SpatializationDelayRightIndex]:=0;
      SpatializationDelayRightIndex:=(SpatializationDelayRightIndex+1) and AudioEngine.SpatializationDelayMask;
     end;
    end;
    NewLastLeft:=0;
    NewLastRight:=0;
   end else begin
    BufEx:=@PpvAudioInt32s(Buf)^[(ToDo-1) shl 1];
    NewLastLeft:=BufEx^[0];
    NewLastRight:=BufEx^[1];
    if Spatialization and (AudioEngine.SpatializationMode in [SPATIALIZATION_PSEUDO,SPATIALIZATION_HRTF]) then begin
     case AudioEngine.SpatializationMode of
      SPATIALIZATION_PSEUDO:begin
       MixProcSpatializationPSEUDO(Buf,ToDo);
      end;
      SPATIALIZATION_HRTF:begin
       MixProcSpatializationHRTF(Buf,ToDo);
      end;
     end;
    end else begin
     // Even for fast fake 3D stereo spatialization
     if VolumeRampingRemain>0 then begin
      MixProcVolumeRamping(Buf,ToDo);
     end else begin
      MixProcNormal(Buf,ToDo);
     end;
    end;
    NewLastLeft:=(BufEx^[0]-NewLastLeft) shl 12;
    NewLastRight:=(BufEx^[1]-NewLastRight) shl 12;
   end;

   if IncrementRampingRemain>0 then begin
    if IncrementRampingStepRemain>0 then begin
     dec(IncrementRampingStepRemain,ToDo);
     if IncrementRampingStepRemain=0 then begin
      IncrementRampingStepRemain:=AudioEngine.RampingStepSamples;
     end;
    end;
    dec(IncrementRampingRemain,ToDo);
    if IncrementRampingRemain=0 then begin
     IncrementRampingStepRemain:=0;
     IncrementCurrent:=Increment shl 16;
     IncrementIncrement:=0;
    end;
   end;

   if VolumeRampingRemain>0 then begin
    dec(VolumeRampingRemain,ToDo);
    if VolumeRampingRemain=0 then begin
     VolumeLeftCurrent:=VolumeLeft;
     VolumeRightCurrent:=VolumeRight;
     VolumeLeftIncrement:=0;
     VolumeRightIncrement:=0;
    end;
   end;

   if Spatialization and (AudioEngine.SpatializationMode in [SPATIALIZATION_PSEUDO,SPATIALIZATION_HRTF]) then begin
    if (AudioEngine.SpatializationMode=SPATIALIZATION_HRTF) and (HRTFRampingRemain>0) then begin
     dec(HRTFRampingRemain,ToDo);
     if HRTFRampingRemain=0 then begin
      for Counter:=0 to HRTFMask do begin
       HRTFLeftCoefsCurrent[Counter]:=HRTFLeftCoefs[Counter];
       HRTFRightCoefsCurrent[Counter]:=HRTFRightCoefs[Counter];
       HRTFLeftCoefsIncrement[Counter]:=0;
       HRTFRightCoefsIncrement[Counter]:=0;
      end;
     end;
    end;
    if SpatializationLowPassRampingRemain>0 then begin
     dec(SpatializationLowPassRampingRemain,ToDo);
     if SpatializationLowPassRampingRemain=0 then begin
      SpatializationLowPassLeftCurrentCoef:=SpatializationLowPassLeftCoef;
      SpatializationLowPassRightCurrentCoef:=SpatializationLowPassRightCoef;
      SpatializationLowPassLeftIncrementCoef:=0;
      SpatializationLowPassRightIncrementCoef:=0;
     end;
    end;
    if SpatializationDelayRampingRemain>0 then begin
     dec(SpatializationDelayRampingRemain,ToDo);
     if SpatializationDelayRampingRemain=0 then begin
      SpatializationDelayLeftCurrent:=SpatializationDelayLeft;
      SpatializationDelayRightCurrent:=SpatializationDelayRight;
      SpatializationDelayLeftIncrement:=0;
      SpatializationDelayRightIncrement:=0;
     end;
    end;
   end;

   inc(Buf,ToDo shl 1);
  end;

  PostClickRemoval(Buf,Remain);

  if not Active then begin
   if assigned(VoiceIndexPointer) then begin
    InterlockedExchange(TpvInt32(VoiceIndexPointer^),-1);
    VoiceIndexPointer:=nil;
   end;
  end;
 end else begin
  if (LastLeft=0) and (LastRight=0) then begin
   Dequeue;
   NextFree:=Sample.FreeVoice;
   Sample.FreeVoice:=self;
  end;
 end;
end;

constructor TpvAudioSoundSample.Create(AAudioEngine:TpvAudio;ASoundSamples:TpvAudioSoundSamples);
begin
 inherited Create;
 AudioEngine:=AAudioEngine;
 SoundSamples:=ASoundSamples;
 Name:='';
{SoundSamples.Add(self);
 if length(Name)>0 then begin
  SoundSamples.HashMap.Add(Name,self);
 end;}
 Data:=nil;
 Loop.Mode:=SoundLoopModeNONE;
 SustainLoop.Mode:=SoundLoopModeNONE;
 Voices:=nil;
 ReferenceCounter:=0;
 SamplePolyphony:=0;
end;

destructor TpvAudioSoundSample.Destroy;
var i:TpvInt32;
    Voice:TpvAudioSoundSampleVoice;
begin
 SoundSamples.Remove(self);
 if length(Name)>0 then begin
  SoundSamples.HashMap.Delete(Name);
 end;
 for i:=0 to length(Voices)-1 do begin
  Voice:=Voices[i];
  if assigned(Voice) then begin
   Voice.Destroy;
   Voices[i]:=nil;
  end;
 end;
 SetLength(Voices,0);
 if assigned(Data) then begin
  dec(PpvAudioInt32(Data),2*SampleFixUp);
  FreeMem(Data);
  Data:=nil;
 end;
 Name:='';
 inherited Destroy;
end;

procedure TpvAudioSoundSample.IncRef;
begin
 inc(ReferenceCounter);
end;

procedure TpvAudioSoundSample.DecRef;
begin
 if ReferenceCounter>0 then begin
  dec(ReferenceCounter);
  if ReferenceCounter=0 then begin
   Destroy;
  end;
 end;
end;

procedure TpvAudioSoundSample.CorrectPolyphony;
begin
 if (ReferenceCounter>0) and (SamplePolyphony>0) then begin
  SetPolyphony(SamplePolyphony*ReferenceCounter);
 end;
end;

procedure TpvAudioSoundSample.FixUp;
var Counter,LoopStart,LoopEnd:TpvInt32;
begin
 if assigned(Data) then begin
  if SampleLength>0 then begin
   for Counter:=0 to SampleFixUp-1 do begin
    Data^[(-(Counter+1)*2)]:=Data^[0];
    Data^[(-(Counter+1)*2)+1]:=Data^[1];
    Data^[(SampleLength+Counter)*2]:=Data^[(SampleLength-1)*2];
    Data^[((SampleLength+Counter)*2)+1]:=Data^[((SampleLength-1)*2)+1];
   end;
   if Loop.Mode in [SoundLoopModeFORWARD,SoundLoopModeBACKWARD] then begin
    LoopStart:=Loop.StartSample;
    LoopEnd:=Loop.EndSample;
    if (LoopStart>0) and (LoopEnd>0) and (LoopEnd<=SampleLength) then begin
     for Counter:=0 to SampleFixUp-1 do begin
      Data^[(LoopEnd+Counter)*2]:=Data^[(LoopStart+Counter)*2];
      Data^[((LoopEnd+Counter)*2)+1]:=Data^[((LoopStart+Counter)*2)+1];
     end;
    end;
   end;
   if SustainLoop.Mode in [SoundLoopModeFORWARD,SoundLoopModeBACKWARD] then begin
    LoopStart:=SustainLoop.StartSample;
    LoopEnd:=SustainLoop.EndSample;
    if (LoopStart>0) and (LoopEnd>0) and (LoopEnd<=SampleLength) then begin
     for Counter:=0 to SampleFixUp-1 do begin
      Data^[(LoopEnd+Counter)*2]:=Data^[(LoopStart+Counter)*2];
      Data^[((LoopEnd+Counter)*2)+1]:=Data^[((LoopStart+Counter)*2)+1];
     end;
    end;
   end;
  end;
 end;
end;

procedure TpvAudioSoundSample.SetPolyphony(Polyphony:TpvInt32);
var i:TpvInt32;
begin
 for i:=0 to length(Voices)-1 do begin
  if assigned(Voices[i]) then begin
   Voices[i].Destroy;
   Voices[i]:=nil;
  end;
 end;
 FreeVoice:=nil;
 SetLength(Voices,Polyphony);
 for i:=0 to length(Voices)-1 do begin
  Voices[i]:=TpvAudioSoundSampleVoice.Create(AudioEngine,self,i);
  Voices[i].NextFree:=FreeVoice;
  FreeVoice:=Voices[i];
 end;
end;

function TpvAudioSoundSample.Play(Volume,Panning,Rate:TpvFloat;VoiceIndexPointer:TpvPointer=nil):TpvInt32;
var BestVoice,BestVolume,i:TpvInt32;
    BestAge:TpvInt64;
    Voice:TpvAudioSoundSampleVoice;
begin
 if assigned(FreeVoice) then begin
  Voice:=FreeVoice;
  FreeVoice:=Voice.NextFree;
  BestVoice:=Voice.Index;
 end else begin
  BestVoice:=-1;
  for i:=0 to length(Voices)-1 do begin
   if not Voices[i].Active then begin
    BestVoice:=i;
    break;
   end;
  end;
  if BestVoice<0 then begin
   BestVolume:=$7fffffff;
   BestAge:=0;
   for i:=0 to length(Voices)-1 do begin
    Voice:=Voices[i];
    if (Voice.Age>BestAge) or ((Voice.Age=BestAge) and (Voice.Volume<=BestVolume)) then begin
     BestAge:=Voices[i].Age;
     BestVolume:=Voices[i].Volume;
     BestVoice:=i;
    end;
   end;
  end;
 end;
 if (BestVoice>=0) and (BestVoice<length(Voices)) then begin
  Voice:=Voices[BestVoice];
  if assigned(Voice.VoiceIndexPointer) then begin
   InterlockedExchange(TpvInt32(Voice.VoiceIndexPointer^),-1);
   Voice.VoiceIndexPointer:=nil;
  end;
  Voice.Init(Volume,Panning,Rate);
  Voice.VoiceIndexPointer:=VoiceIndexPointer;
  if assigned(VoiceIndexPointer) then begin
   InterlockedExchange(TpvInt32(VoiceIndexPointer^),BestVoice);
  end;
  Voice.Enqueue;
 end;
 result:=BestVoice;
end;

procedure TpvAudioSoundSample.Stop(VoiceNumber:TpvInt32);
var Voice:TpvAudioSoundSampleVoice;
begin
 if (VoiceNumber>=0) and (VoiceNumber<length(Voices)) then begin
  Voice:=Voices[VoiceNumber];
  Voice.Active:=false;
  if assigned(Voice.VoiceIndexPointer) then begin
   InterlockedExchange(TpvInt32(Voice.VoiceIndexPointer^),-1);
   Voice.VoiceIndexPointer:=nil;
  end;
 end;
end;

procedure TpvAudioSoundSample.KeyOff(VoiceNumber:TpvInt32);
var Voice:TpvAudioSoundSampleVoice;
begin
 if (VoiceNumber>=0) and (VoiceNumber<length(Voices)) then begin
  Voice:=Voices[VoiceNumber];
  Voice.KeyOff:=true;
  if (Loop.Mode<>SoundLoopModeNONE) or (SustainLoop.Mode=SoundLoopModeNONE) then begin
   Voice.Active:=false;
   if assigned(Voice.VoiceIndexPointer) then begin
    TpvInt32(Voice.VoiceIndexPointer^):=-1;
    Voice.VoiceIndexPointer:=nil;
   end;
  end;
 end;
end;

function TpvAudioSoundSample.SetVolume(VoiceNumber:TpvInt32;Volume:TpvFloat):TpvInt32;
var Voice:TpvAudioSoundSampleVoice;
begin
 result:=VoiceNumber;
 if (VoiceNumber>=0) and (VoiceNumber<length(Voices)) then begin
  Voice:=Voices[VoiceNumber];
  Voice.Volume:=round(Clamp(Volume,-1.0,1.0)*65536.0);
 end;
end;

function TpvAudioSoundSample.SetPanning(VoiceNumber:TpvInt32;Panning:TpvFloat):TpvInt32;
var Voice:TpvAudioSoundSampleVoice;
begin
 result:=VoiceNumber;
 if (VoiceNumber>=0) and (VoiceNumber<length(Voices)) then begin
  Voice:=Voices[VoiceNumber];
  Voice.Panning:=round(Clamp(Panning,-1.0,1.0)*65536.0);
 end;
end;

function TpvAudioSoundSample.SetRate(VoiceNumber:TpvInt32;Rate:TpvFloat):TpvInt32;
var Voice:TpvAudioSoundSampleVoice;
begin
 result:=VoiceNumber;
 if (VoiceNumber>=0) and (VoiceNumber<length(Voices)) then begin
  Voice:=Voices[VoiceNumber];
  Voice.Rate:=Rate;
  Voice.Increment:=(SampleRate*round((Rate*Voice.DopplerRate)*TpvInt64($100000000))) div AudioEngine.SampleRate;
 end;
end;

function TpvAudioSoundSample.SetPosition(VoiceNumber:TpvInt32;Spatialization:LongBool;const Origin,Velocity:TpvVector3):TpvInt32;
var Voice:TpvAudioSoundSampleVoice;
begin
 result:=VoiceNumber;
 if (VoiceNumber>=0) and (VoiceNumber<length(Voices)) then begin
  Voice:=Voices[VoiceNumber];
  Voice.Spatialization:=Spatialization;
  Voice.SpatializationOrigin:=Origin;
  Voice.SpatializationVelocity:=Velocity;
 end;
end;

function TpvAudioSoundSample.SetEffectMix(VoiceNumber:TpvInt32;Active:LongBool):TpvInt32;
var Voice:TpvAudioSoundSampleVoice;
begin
 result:=VoiceNumber;
 if (VoiceNumber>=0) and (VoiceNumber<length(Voices)) then begin
  Voice:=Voices[VoiceNumber];
  Voice.MixToEffect:=Active;
 end;
end;

function TpvAudioSoundSample.IsPlaying:boolean;
var i:TpvInt32;
begin
 result:=false;
 for i:=0 to length(Voices)-1 do begin
  if Voices[i].Active then begin
   result:=true;
   break;
  end;
 end;
end;

function TpvAudioSoundSample.IsVoicePlaying(VoiceNumber:TpvInt32):boolean;
begin
 result:=((VoiceNumber>=0) and (VoiceNumber<length(Voices))) and Voices[VoiceNumber].Active;
end;

constructor TpvAudioSoundMusic.Create(AAudioEngine:TpvAudio;ASoundMusics:TpvAudioSoundMusics);
begin
 inherited Create;
 AudioEngine:=AAudioEngine;
 SoundMusics:=ASoundMusics;
 Name:='';
{SoundMusics.Add(self);
 if length(Name)>0 then begin
  SoundMusics.HashMap.Add(Name,self);
 end;}
 Active:=false;
 VolumeRampingRemain:=0;
 LastLeft:=0;
 LastRight:=0;
 LastSample.Left:=0;
 LastSample.Right:=0;
 Panning:=0;
 Volume:=0;
 VolumeLeft:=-1;
 VolumeRight:=-1;
 VolumeLeftCurrent:=0;
 VolumeRightCurrent:=0;
 VolumeLeftInc:=0;
 VolumeRightInc:=0;
 vf:=nil;
end;

destructor TpvAudioSoundMusic.Destroy;
begin
 SoundMusics.Remove(self);
 if length(Name)>0 then begin
  SoundMusics.HashMap.Delete(Name);
 end;
 if assigned(vf) then begin
  ov_clear(vf);
  Dispose(vf);
 end;
 if assigned(Data) then begin
  Data.Destroy;
 end;
 Name:='';
 inherited Destroy;
end;

procedure TpvAudioSoundMusic.InitSINC;
const Points=ResamplerSINCWidth;
      ThePoints=Points;
      HalfPoints=Points*0.5;
      ExtendedPI=3.1415926535897932384626433832795;
var FracValue,Value,SincValue,WindowValue,WindowFactor,WindowParameter,
    OtherPosition,Position,ResamplerSINCCutOff:{$ifdef cpuarm}TpvFloat{$else}TpvDouble{$endif};
    Counter,SubCounter:TpvInt32;
begin
 ResamplerSINCCutOff:=Clamp(AudioEngine.SampleRate/SampleRate,0.0,1.0);
 for Counter:=0 to ResamplerSINCLength-1 do begin
  FracValue:=(Counter/ResamplerSINCLength)-0.5;
  WindowFactor:=(2*ExtendedPI)/ThePoints;
  for SubCounter:=0 to Points-1 do begin
   OtherPosition:=SubCounter-FracValue;
   Position:=OtherPosition-HalfPoints;
   if abs(Position)<ResamplerEpsilon then begin
    Value:=ResamplerSINCCutOff;
   end else begin
    SincValue:=sin(ResamplerSINCCutOff*Position*ExtendedPI)/(Position*ExtendedPI);
    WindowParameter:=OtherPosition*WindowFactor;
    WindowValue:=0.42-(0.50*cos(WindowParameter))+(0.08*cos(2.0*WindowParameter)); // Blackman exact
    Value:=SincValue*WindowValue;
   end;
   Table[Counter,SubCounter]:=round(Value*ResamplerSINCValueLength);
  end;
 end;
end;

procedure TpvAudioSoundMusic.Play(AVolume,APanning,ARate:TpvFloat;ALoop:boolean);
var Pan,VolLeft,VolRight,MixVolume:TpvInt32;
begin
 inc(LastLeft,LastSample.Left);
 inc(LastRight,LastSample.Right);
 LastSample.Left:=0;
 LastSample.Right:=0;
 ov_raw_seek(vf,0);
 InBufferPosition:=InBufferSize+1;
 OutBufferPosition:=OutBufferSize+1;
 Volume:=round(AVolume*65536.0);
 Panning:=round(APanning*65536.0);
 Loop:=ALoop;
 VolumeRampingRemain:=0;
 Pan:=Panning+65536;
 if Pan<0 then begin
  Pan:=0;
 end else if Pan>=131072 then begin
  Pan:=131071;
 end;
 MixVolume:=SARLongint(Volume*AudioEngine.MusicVolume,16);
 VolLeft:=SARLongint((131072-Pan)*MixVolume,17);
 VolRight:=SARLongint(Pan*MixVolume,17);
 if VolLeft<0 then begin
  VolLeft:=0;
 end else if VolLeft>=4096 then begin
  VolLeft:=4095;
 end;
 if VolRight<0 then begin
  VolRight:=0;
 end else if VolRight>=4096 then begin
  VolRight:=4095;
 end;
 VolLeft:=VolLeft shl 12;
 VolRight:=VolRight shl 12;
 VolumeLeftCurrent:=VolLeft;
 VolumeRightCurrent:=VolRight;
 VolumeLeft:=VolLeft;
 VolumeRight:=VolRight;
 ResamplerPosition:=0;
 if AudioEngine.SampleRate=SampleRate then begin
  ResamplerOriginalIncrement:=ResamplerFixedPointFactor;
 end else begin
  ResamplerOriginalIncrement:=((SampleRate*ResamplerFixedPointFactor)+(AudioEngine.SampleRate div 2)) div AudioEngine.SampleRate;
 end;
 ResamplerIncrement:=(abs(round(ARate*65536.0))*ResamplerOriginalIncrement) shr 16;
 ResamplerBufferPosition:=0;
 FillChar(ResamplerBuffer,SizeOf(ResamplerBuffer),AnsiChar(#0));
 FillChar(ResamplerCurrentSample,SizeOf(TpvAudioSoundMusicBufferSample),AnsiChar(#0));
 FillChar(ResamplerLastSample,SizeOf(TpvAudioSoundMusicBufferSample),AnsiChar(#0));
 Active:=true;
end;

procedure TpvAudioSoundMusic.Stop;
begin
 Active:=false;
 inc(LastLeft,LastSample.Left);
 inc(LastRight,LastSample.Right);
 LastSample.Left:=0;
 LastSample.Right:=0;
end;

procedure TpvAudioSoundMusic.SetVolume(AVolume:TpvFloat);
begin
 Volume:=round(AVolume*65536.0);
 VolumeRampingRemain:=AudioEngine.RampingSamples;
end;

procedure TpvAudioSoundMusic.SetPanning(APanning:TpvFloat);
begin
 Panning:=round(APanning*65536.0);
 VolumeRampingRemain:=AudioEngine.RampingSamples;
end;

procedure TpvAudioSoundMusic.SetRate(ARate:TpvFloat);
begin
 ResamplerIncrement:=(abs(round(ARate*65536.0))*ResamplerOriginalIncrement) shr 16;
end;

procedure TpvAudioSoundMusic.GetNextInBuffer;
var ToRead,BytesIn,Position,Tries:TpvInt32;
begin
 InBufferSize:=0;
 Position:=0;
 Tries:=0;
 while InBufferSize=0 do begin
  ToRead:=((length(InBuffer)-InBufferSize))*sizeof(smallint)*Channels;
  BytesIn:=ov_read(vf,@PCMBuffer[Position],ToRead,@bitstream);
  if BytesIn=OV_HOLE then begin
   continue;
  end else if (BytesIn=OV_EBADLINK) or (BytesIn=OV_EINVAL) or (BytesIn=0) then begin
   if Loop and (Tries<3) then begin
    ov_raw_seek(vf,0);
    inc(Tries);
    continue;
   end else begin
    if InBufferSize=0 then begin
     InBufferSize:=length(InBuffer);
     FillChar(PCMBuffer[0],SizeOf(SmallInt)*length(PCMBuffer),AnsiChar(#0));
    end; 
    Active:=false;
   end;
   break;
  end;
  inc(Position,BytesIn div sizeof(smallint));
  inc(InBufferSize,BytesIn div (sizeof(smallint)*Channels));
 end;
 case Channels of
  1:begin
   // Mono
   for Position:=0 to InBufferSize-1 do begin
    InBuffer[Position].Left:=PCMBuffer[Position];
    InBuffer[Position].Right:=PCMBuffer[Position];
   end;
  end;
  2:begin
   // Left, right
   for Position:=0 to InBufferSize-1 do begin
    InBuffer[Position].Left:=PCMBuffer[(Position*2)+0];
    InBuffer[Position].Right:=PCMBuffer[(Position*2)+1];
   end;
  end;
  3:begin
   // Left, middle, right
   for Position:=0 to InBufferSize-1 do begin
    InBuffer[Position].Left:=SARLongint(PCMBuffer[(Position*3)+0]+PCMBuffer[(Position*3)+1],1);
    InBuffer[Position].Right:=SARLongint(PCMBuffer[(Position*3)+2]+PCMBuffer[(Position*3)+1],1);
   end;
  end;
  4:begin
   // Front left, front right, back left, back right
   for Position:=0 to InBufferSize-1 do begin
    InBuffer[Position].Left:=SARLongint(PCMBuffer[(Position*4)+0]+PCMBuffer[(Position*4)+2],1);
    InBuffer[Position].Right:=SARLongint(PCMBuffer[(Position*4)+1]+PCMBuffer[(Position*4)+3],1);
   end;
  end;
  5:begin
   // Front left, front middle, front right, back left, back right
   for Position:=0 to InBufferSize-1 do begin
    InBuffer[Position].Left:=(PCMBuffer[(Position*5)+0]+PCMBuffer[(Position*5)+1]+PCMBuffer[(Position*5)+3]) div 3;
    InBuffer[Position].Right:=(PCMBuffer[(Position*5)+2]+PCMBuffer[(Position*5)+1]+PCMBuffer[(Position*5)+4]) div 3;
   end;
  end;
  6:begin
   // Front left, front middle, front right, back left, back right, LFE channel (subwoofer)
   for Position:=0 to InBufferSize-1 do begin
    InBuffer[Position].Left:=SARLongint(PCMBuffer[(Position*6)+0]+PCMBuffer[(Position*6)+1]+PCMBuffer[(Position*6)+3]+PCMBuffer[(Position*6)+5],2);
    InBuffer[Position].Right:=SARLongint(PCMBuffer[(Position*6)+2]+PCMBuffer[(Position*6)+1]+PCMBuffer[(Position*6)+4]+PCMBuffer[(Position*6)+5],2);
   end;
  end;
  else begin
   // Undefined, so get only the first two channels in account
   for Position:=0 to InBufferSize-1 do begin
    InBuffer[Position].Left:=PCMBuffer[(Position*Channels)+0];
    InBuffer[Position].Right:=PCMBuffer[(Position*Channels)+1];
   end;
  end;
 end;
 InBufferPosition:=0;
end;

procedure TpvAudioSoundMusic.Resample;
var Counter{$ifdef cpuarm},Factor{$endif}:TpvInt32;
    OutSample:PpvAudioSoundMusicBufferSample;
{$ifdef cpuarm}
    InSamples:array[-1..0] of PpvAudioSoundMusicBufferSample;
{$else}
    SINCSubArray:PpvAudioResamplerSINCSubArray;
    InSamples:array[-3..0] of PpvAudioSoundMusicBufferSample;
    SubArray:PpvAudioResamplerCubicSplineSubArray;
{$endif}
begin
 if ResamplerIncrement=ResamplerFixedPointFactor then begin
  for Counter:=low(OutBuffer) to high(OutBuffer) do begin
   if InBufferPosition>=InBufferSize then begin
    GetNextInBuffer;
   end;
   OutBuffer[Counter]:=InBuffer[InBufferPosition];
   inc(InBufferPosition);
  end;
 end else begin
{$ifdef cpuarm}
  // Use simple but fast linear interpolation for up- and downsampling on mobile embedded ARM platforms
  for Counter:=low(OutBuffer) to high(OutBuffer) do begin
   inc(ResamplerPosition,ResamplerIncrement);
   while ResamplerPosition>=ResamplerFixedPointFactor do begin
    dec(ResamplerPosition,ResamplerFixedPointFactor);
    if InBufferPosition>=InBufferSize then begin
     GetNextInBuffer;
    end;
    ResamplerBuffer[ResamplerBufferPosition and ResamplerBufferMask]:=InBuffer[InBufferPosition];
    inc(InBufferPosition);
    ResamplerBufferPosition:=(ResamplerBufferPosition+1) and ResamplerBufferMask;
   end;
   Factor:=(TpvInt32(ResamplerPosition and ResamplerFixedPointMask) shr ResamplerLinearInterpolationFracShift) and ResamplerLinearInterpolationFracMask;
   InSamples[-1]:=@ResamplerBuffer[(ResamplerBufferPosition-2) and ResamplerBufferMask];
   InSamples[0]:=@ResamplerBuffer[(ResamplerBufferPosition-1) and ResamplerBufferMask];
   OutSample:=@OutBuffer[Counter];
   OutSample^.Left:=InSamples[-1]^.Left+SARLongint((InSamples[0]^.Left-InSamples[-1]^.Left)*Factor,ResamplerLinearInterpolationValueBits);
   OutSample^.Right:=InSamples[-1]^.Right+SARLongint((InSamples[0]^.Right-InSamples[-1]^.Right)*Factor,ResamplerLinearInterpolationValueBits);
  end;
{$else}
  if (ResamplerIncrement<ResamplerFixedPointFactor) or (ResamplerIncrement<>ResamplerOriginalIncrement) then begin
   // Upsample (and downsample if not original resampling rate) with cubic spline
   for Counter:=low(OutBuffer) to high(OutBuffer) do begin
    inc(ResamplerPosition,ResamplerIncrement);
    while ResamplerPosition>=ResamplerFixedPointFactor do begin
     dec(ResamplerPosition,ResamplerFixedPointFactor);
     if InBufferPosition>=InBufferSize then begin
      GetNextInBuffer;
     end;
     ResamplerBuffer[ResamplerBufferPosition and ResamplerBufferMask]:=InBuffer[InBufferPosition];
     inc(InBufferPosition);
     ResamplerBufferPosition:=(ResamplerBufferPosition+1) and ResamplerBufferMask;
    end;
    SubArray:=@AudioEngine.CubicSplineTable[(TpvUInt32(ResamplerPosition and ResamplerFixedPointMask) shr ResamplerCubicSplineFracShift) and ResamplerCubicSplineFracMask];
    InSamples[-3]:=@ResamplerBuffer[(ResamplerBufferPosition-4) and ResamplerBufferMask];
    InSamples[-2]:=@ResamplerBuffer[(ResamplerBufferPosition-3) and ResamplerBufferMask];
    InSamples[-1]:=@ResamplerBuffer[(ResamplerBufferPosition-2) and ResamplerBufferMask];
    InSamples[0]:=@ResamplerBuffer[(ResamplerBufferPosition-1) and ResamplerBufferMask];
    OutSample:=@OutBuffer[Counter];
    OutSample^.Left:=SARLongint((SubArray^[0]*InSamples[-3]^.Left)+(SubArray^[1]*InSamples[-1]^.Left)+(SubArray^[2]*InSamples[-2]^.Left)+(SubArray^[3]*InSamples[0]^.Left),ResamplerCubicSplineValueBits);
    OutSample^.Right:=SARLongint((SubArray^[0]*InSamples[-3]^.Right)+(SubArray^[1]*InSamples[-1]^.Right)+(SubArray^[2]*InSamples[-2]^.Right)+(SubArray^[3]*InSamples[0]^.Right),ResamplerCubicSplineValueBits);
   end;
  end else begin
   // Downsample with SINC
   for Counter:=low(OutBuffer) to high(OutBuffer) do begin
    inc(ResamplerPosition,ResamplerIncrement);
    while ResamplerPosition>=ResamplerFixedPointFactor do begin
     dec(ResamplerPosition,ResamplerFixedPointFactor);
     if InBufferPosition>=InBufferSize then begin
      GetNextInBuffer;
     end;
     ResamplerBuffer[ResamplerBufferPosition and ResamplerBufferMask]:=InBuffer[InBufferPosition];
     inc(InBufferPosition);
     ResamplerBufferPosition:=(ResamplerBufferPosition+1) and ResamplerBufferMask;
    end;
    SINCSubArray:=@Table[(TpvUInt32(ResamplerPosition and ResamplerFixedPointMask) shr ResamplerSINCFracShift) and ResamplerSINCFracMask];
    OutSample:=@OutBuffer[Counter];
    OutSample^.Left:=SARLongint(SARLongint((SINCSubArray^[0]*ResamplerBuffer[(ResamplerBufferPosition-8) and ResamplerBufferMask].Left)+
                                           (SINCSubArray^[1]*ResamplerBuffer[(ResamplerBufferPosition-7) and ResamplerBufferMask].Left)+
                                           (SINCSubArray^[2]*ResamplerBuffer[(ResamplerBufferPosition-6) and ResamplerBufferMask].Left)+
                                           (SINCSubArray^[3]*ResamplerBuffer[(ResamplerBufferPosition-5) and ResamplerBufferMask].Left),ResamplerSINCValueBits)+
                                SARLongint((SINCSubArray^[4]*ResamplerBuffer[(ResamplerBufferPosition-4) and ResamplerBufferMask].Left)+
                                           (SINCSubArray^[5]*ResamplerBuffer[(ResamplerBufferPosition-3) and ResamplerBufferMask].Left)+
                                           (SINCSubArray^[6]*ResamplerBuffer[(ResamplerBufferPosition-2) and ResamplerBufferMask].Left)+
                                           (SINCSubArray^[7]*ResamplerBuffer[(ResamplerBufferPosition-1) and ResamplerBufferMask].Left),ResamplerSINCValueBits),1);
    OutSample^.Right:=SARLongint(SARLongint((SINCSubArray^[0]*ResamplerBuffer[(ResamplerBufferPosition-8) and ResamplerBufferMask].Right)+
                                            (SINCSubArray^[1]*ResamplerBuffer[(ResamplerBufferPosition-7) and ResamplerBufferMask].Right)+
                                            (SINCSubArray^[2]*ResamplerBuffer[(ResamplerBufferPosition-6) and ResamplerBufferMask].Right)+
                                            (SINCSubArray^[3]*ResamplerBuffer[(ResamplerBufferPosition-5) and ResamplerBufferMask].Right),ResamplerSINCValueBits)+
                                 SARLongint((SINCSubArray^[4]*ResamplerBuffer[(ResamplerBufferPosition-4) and ResamplerBufferMask].Right)+
                                            (SINCSubArray^[5]*ResamplerBuffer[(ResamplerBufferPosition-3) and ResamplerBufferMask].Right)+
                                            (SINCSubArray^[6]*ResamplerBuffer[(ResamplerBufferPosition-2) and ResamplerBufferMask].Right)+
                                            (SINCSubArray^[7]*ResamplerBuffer[(ResamplerBufferPosition-1) and ResamplerBufferMask].Right),ResamplerSINCValueBits),1);
   end;
  end;
{$endif}
 end;
 OutBufferPosition:=0;
 OutBufferSize:=length(OutBuffer);
end;

procedure TpvAudioSoundMusic.MixTo(Buffer:PpvAudioSoundSampleValues;MixVolume:TpvInt32);
var Counter,Pan,VolLeft,VolRight:TpvInt32;
    Sample:TpvAudioSoundMusicBufferSample;
    BufferSample:PpvAudioSoundSampleValue;
begin
 if Active or ((VolumeRampingRemain>0) or ((LastLeft<>0) or (LastRight<>0))) then begin
  Pan:=Panning+65536;
  if Pan<0 then begin
   Pan:=0;
  end else if Pan>=131072 then begin
   Pan:=131071;
  end;
  MixVolume:=SARLongint(Volume*MixVolume,16);
  VolLeft:=SARLongint((131072-Pan)*MixVolume,17);
  VolRight:=SARLongint(Pan*MixVolume,17);
  if VolLeft<0 then begin
   VolLeft:=0;
  end else if VolLeft>=4096 then begin
   VolLeft:=4095;
  end;
  if VolRight<0 then begin
   VolRight:=0;
  end else if VolRight>=4096 then begin
   VolRight:=4095;
  end;
  VolLeft:=VolLeft shl 12;
  VolRight:=VolRight shl 12;
  if (VolumeLeft<>VolLeft) or (VolumeRight<>VolRight) then begin
   VolumeLeft:=VolLeft;
   VolumeRight:=VolRight;
   if VolumeRampingRemain=0 then begin
    VolumeLeftCurrent:=VolumeLeft;
    VolumeRightCurrent:=VolumeRight;
    VolumeLeftInc:=0;
    VolumeRightInc:=0;
   end else begin
    VolumeLeftInc:=(VolumeLeft-VolumeLeftCurrent) div VolumeRampingRemain;
    VolumeRightInc:=(VolumeRight-VolumeRightCurrent) div VolumeRampingRemain;
   end;
  end;
  BufferSample:=@Buffer^[0];
  for Counter:=1 to AudioEngine.BufferSamples do begin
   if Active then begin
    if OutBufferPosition>=OutBufferSize then begin
     Resample;
    end;
    Sample:=OutBuffer[OutBufferPosition];
    inc(OutBufferPosition);
   end else begin
    Sample.Left:=0;
    Sample.Right:=0;
   end;
   if VolumeRampingRemain>0 then begin
    dec(VolumeRampingRemain);
    VolLeft:=SARLongint(VolumeLeftCurrent,12);
    VolRight:=SARLongint(VolumeRightCurrent,12);
    inc(VolumeLeftCurrent,VolumeLeftInc);
    inc(VolumeRightCurrent,VolumeRightInc);
   end else begin
    VolLeft:=SARLongint(VolumeLeft,12);
    VolRight:=SARLongint(VolumeRight,12);
   end;
   Sample.Left:=SARLongint(Sample.Left*VolLeft,12);
   Sample.Right:=SARLongint(Sample.Right*VolRight,12);
   LastSample:=Sample;
   if (LastLeft<>0) or (LastRight<>0) then begin
    dec(LastLeft,SARLongint(LastLeft+(SARLongint(-LastLeft,31) and $ff),8));
    dec(LastRight,SARLongint(LastRight+(SARLongint(-LastRight,31) and $ff),8));
    inc(Sample.Left,SARLongint(LastLeft,12));
    inc(Sample.Right,SARLongint(LastRight,12));
   end;
   inc(BufferSample^,Sample.Left);
   inc(BufferSample);
   inc(BufferSample^,Sample.Right);
   inc(BufferSample);
  end;
  if VolumeRampingRemain=0 then begin
   VolumeLeftCurrent:=VolumeLeft;
   VolumeRightCurrent:=VolumeRight;
   VolumeLeftInc:=0;
   VolumeRightInc:=0;
  end;
 end;
end;

function TpvAudioSoundMusic.IsPlaying:boolean;
begin
 result:=Active;
end;

constructor TpvAudioSoundSampleResource.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);
 fSample:=nil;
end;

destructor TpvAudioSoundSampleResource.Destroy;
begin
 FreeAndNil(fSample);
 inherited Destroy;
end;

function TpvAudioSoundSampleResource.BeginLoad(const aStream:TStream):boolean;
begin
 if assigned(MetaData) then begin
  fSample:=AudioInstance.Samples.Load(TPasJSON.GetString(TPasJSONItemObject(MetaData).Properties['name'],FileName),
                                      aStream,
                                      false,
                                      TPasJSON.GetInt64(TPasJSONItemObject(MetaData).Properties['polyphony'],1),
                                      TPasJSON.GetInt64(TPasJSONItemObject(MetaData).Properties['loop'],1));
 end else begin
  fSample:=AudioInstance.Samples.Load(FileName,
                                      aStream,
                                      false,
                                      1,
                                      1);
 end;
 result:=assigned(fSample);
end;

procedure TpvAudioSoundSampleResource.FixUp;
begin
 fSample.FixUp;
end;

procedure TpvAudioSoundSampleResource.SetPolyphony(Polyphony:TpvInt32);
begin
 fSample.SetPolyphony(Polyphony);
end;

function TpvAudioSoundSampleResource.Play(Volume,Panning,Rate:TpvFloat;VoiceIndexPointer:TpvPointer=nil):TpvInt32;
begin
 result:=fSample.Play(Volume,Panning,Rate,VoiceIndexPointer);
end;

procedure TpvAudioSoundSampleResource.Stop(VoiceNumber:TpvInt32);
begin
 fSample.Stop(VoiceNumber);
end;

procedure TpvAudioSoundSampleResource.KeyOff(VoiceNumber:TpvInt32);
begin
 fSample.KeyOff(VoiceNumber);
end;

function TpvAudioSoundSampleResource.SetVolume(VoiceNumber:TpvInt32;Volume:TpvFloat):TpvInt32;
begin
 result:=fSample.SetVolume(VoiceNumber,Volume);
end;

function TpvAudioSoundSampleResource.SetPanning(VoiceNumber:TpvInt32;Panning:TpvFloat):TpvInt32;
begin
 result:=fSample.SetPanning(VoiceNumber,Panning);
end;

function TpvAudioSoundSampleResource.SetRate(VoiceNumber:TpvInt32;Rate:TpvFloat):TpvInt32;
begin
 result:=fSample.SetRate(VoiceNumber,Rate);
end;

function TpvAudioSoundSampleResource.SetPosition(VoiceNumber:TpvInt32;Spatialization:LongBool;const Origin,Velocity:TpvVector3):TpvInt32;
begin
 result:=fSample.SetPosition(VoiceNumber,Spatialization,Origin,Velocity);
end;

function TpvAudioSoundSampleResource.SetEffectMix(VoiceNumber:TpvInt32;Active:LongBool):TpvInt32;
begin
 result:=fSample.SetEffectMix(VoiceNumber,Active);
end;

function TpvAudioSoundSampleResource.IsPlaying:boolean;
begin
 result:=fSample.IsPlaying;
end;

function TpvAudioSoundSampleResource.IsVoicePlaying(VoiceNumber:TpvInt32):boolean;
begin
 result:=fSample.IsVoicePlaying(VoiceNumber);
end;

constructor TpvAudioSoundMusicResource.Create(const aResourceManager:TpvResourceManager;const aParent:TpvResource=nil);
begin
 inherited Create(aResourceManager,aParent);
 fMusic:=nil;
end;

destructor TpvAudioSoundMusicResource.Destroy;
begin
 FreeAndNil(fMusic);
 inherited Destroy;
end;

function TpvAudioSoundMusicResource.BeginLoad(const aStream:TStream):boolean;
begin
 if assigned(MetaData) then begin
  fMusic:=AudioInstance.Musics.Load(TPasJSON.GetString(TPasJSONItemObject(MetaData).Properties['name'],FileName),
                                aStream,
                                false);
 end else begin
  fMusic:=AudioInstance.Musics.Load(FileName,
                                aStream,
                                false);
 end;
 result:=assigned(fMusic);
end;

procedure TpvAudioSoundMusicResource.Play(AVolume,APanning,ARate:TpvFloat;ALoop:boolean);
begin
 fMusic.Play(AVolume,APanning,ARate,ALoop);
end;

procedure TpvAudioSoundMusicResource.Stop;
begin
 fMusic.Stop;
end;

procedure TpvAudioSoundMusicResource.SetVolume(AVolume:TpvFloat);
begin
 fMusic.SetVolume(AVolume);
end;

procedure TpvAudioSoundMusicResource.SetPanning(APanning:TpvFloat);
begin
 fMusic.SetPanning(APanning);
end;

procedure TpvAudioSoundMusicResource.SetRate(ARate:TpvFloat);
begin
 fMusic.SetRate(ARate);
end;

function TpvAudioSoundMusicResource.IsPlaying:boolean;
begin
 result:=fMusic.IsPlaying;
end;

constructor TpvAudioSoundSamples.Create(AAudioEngine:TpvAudio);
begin
 inherited Create;
 AudioEngine:=AAudioEngine;
 HashMap:=TpvAudioStringHashMap.Create(nil);
end;

destructor TpvAudioSoundSamples.Destroy;
begin
 while Count>0 do begin
  TpvAudioSoundSample(inherited Items[0]).Free;
 end;
 Clear;
 HashMap.Free;
 inherited Destroy;
end;

function TpvAudioSoundSamples.GetItem(Index:TpvInt32):TpvAudioSoundSample;
begin
 if (Index>=0) and (Index<Count) then begin
  result:=TpvAudioSoundSample(inherited Items[Index]);
 end else begin
  result:=nil;
 end;
end;

procedure TpvAudioSoundSamples.SetItem(Index:TpvInt32;Item:TpvAudioSoundSample);
begin
 if (Index>=0) and (Index<Count) then begin
  inherited Items[Index]:=TpvPointer(Item);
 end;
end;

function oggread(ptr:TpvPointer;size,nmemb:PasAudioOGGPtrUInt;datasource:TpvPointer):PasAudioOGGPtrUInt; {$ifdef UseExternalOGGVorbisTremorLibrary}cdecl;{$endif}
begin
 result:=TStream(datasource).Read(ptr^,nmemb*size);
end;

function oggseek(datasource:TpvPointer;offset:TpvInt64;whence:TpvInt32):TpvInt32; {$ifdef UseExternalOGGVorbisTremorLibrary}cdecl;{$endif}
begin
 case whence of
  SEEK_SET:begin
   TStream(datasource).Seek(offset,soFromBeginning);
  end;
  SEEK_CUR:begin
   TStream(datasource).Seek(offset,soFromCurrent);
  end;
  SEEK_END:begin
   TStream(datasource).Seek(offset,soFromEnd);
  end;
 end;
 result:=TStream(datasource).Position;
end;

function oggclose(datasource:TpvPointer):TpvInt32; {$ifdef UseExternalOGGVorbisTremorLibrary}cdecl;{$endif}
begin
 result:=0;
end;

function oggtell(datasource:TpvPointer):TpvInt32; {$ifdef UseExternalOGGVorbisTremorLibrary}cdecl;{$endif}
begin
 result:=TStream(datasource).Position;
end;

const _ov_open_callbacks:ov_callbacks=(read_func:oggread;seek_func:oggseek;close_func:oggclose;tell_func:oggtell);

function TpvAudioSoundSamples.Load(Name:TpvRawByteString;Stream:TStream;DoFree:boolean=true;Polyphony:TpvInt32=1;Loop:TpvUInt32=1):TpvAudioSoundSample;
var DataStream:TMemoryStream;
//  WSMPOffset:TpvUInt32;
    DestSample:TpvAudioSoundSample;
 function LoadWAV:boolean;
 type TWaveFileHeader=packed record
       Signatur:array[1..4] of ansichar;
       Size:TpvUInt32;
       WAVESignatur:array[1..4] of ansichar;
      end;
      TWaveFormatHeader=packed record
       FormatTag:TpvUInt16;
       Kaenale:TpvUInt16;
       SamplesProSekunde:TpvUInt32;
       AvgBytesProSekunde:TpvUInt32;
       SampleGroesse:TpvUInt16;
       BitsProSample:TpvUInt16;
      end;
      TWaveSampleHeader=packed record
       Manufacturer:TpvUInt32;
       Produkt:TpvUInt32;
       SamplePeriode:TpvUInt32;
       BasisNote:TpvUInt32;
       PitchFraction:TpvUInt32;
       SMTPEFormat:TpvUInt32;
       SMTPEOffset:TpvUInt32;
       SampleLoops:TpvUInt32;
       SamplerData:TpvUInt32;
      end;
      TWaveSampleLoopHeader=packed record
       Identifier:TpvUInt32;
       SchleifenTyp:TpvUInt32;
       SchleifeStart:TpvUInt32;
       SchleifeEnde:TpvUInt32;
       Fraction:TpvUInt32;
       AnzahlSpielen:TpvUInt32;
      end;
      TWaveInfoHeader=array[1..4] of ansichar;
      TWaveXtraHeader=packed record
       Flags:TpvUInt32;
       Pan:TpvUInt16;
       Volume:TpvUInt16;
       GlobalVolume:TpvUInt16;
       Reserviert:TpvUInt16;
       VibType:TpvUInt8;
       VibSweep:TpvUInt8;
       VibDepth:TpvUInt8;
       VibRate:TpvUInt8;
      end;
      TWaveChunkHeader=packed record
       Signatur:array[1..4] of ansichar;
       Size:TpvUInt32;
      end;
      PSample24Value=^TSample24Value;
      TSample24Value=packed record
       A,B,C:TpvUInt8;
      end;
 const IMAADPCMUnpackTable:array[0..88] of TpvUInt16=(
        7,         8,     9,    10,    11,    12,    13,    14,
        16,       17,    19,    21,    23,    25,    28,    31,
        34,       37,    41,    45,    50,    55,    60,    66,
        73,       80,    88,    97,   107,   118,   130,   143,
        157,     173,   190,   209,   230,   253,   279,   307,
        337,     371,   408,   449,   494,   544,   598,   658,
        724,     796,   876,   963,  1060,  1166,  1282,  1411,
        1552,   1707,  1878,  2066,  2272,  2499,  2749,  3024,
        3327,   3660,  4026,  4428,  4871,  5358,  5894,  6484,
        7132,   7845,  8630,  9493, 10442, 11487, 12635, 13899,
        15289, 16818, 18500, 20350, 22385, 24623, 27086, 29794,
        32767);
        IMAADPCMIndexTable:array[0..7] of shortint=(-1,-1,-1,-1,2,4,6,8);
 var Header:TWaveFileHeader;
     WaveFormatHeader:TWaveFormatHeader;
     WaveFormatHeaderPCM:TWaveFormatHeader;
     WaveFormatHeaderTemp:TWaveFormatHeader;
     WaveSampleHeader:TWaveSampleHeader;
     WaveSampleLoopHeader:TWaveSampleLoopHeader;
     WaveInfoHeader:TWaveInfoHeader;
     WaveXtraHeader:TWaveXtraHeader;
     WaveChunkHeader:TWaveChunkHeader;
     WaveFormatHeaderExists:boolean;
     WaveFormatHeaderPCMExists:boolean;
     Fact:TpvUInt32;
     Data:TpvUInt32;
     Smpl:TpvUInt32;
     Xtra:TpvUInt32;
     List:TpvUInt32;
     Next:TpvInt32;
     SampleGroesse:TpvUInt32;
     PB:pbyte;
     PW:pword;
     PDW:plongword;
     I:TpvInt32;
     Size,ADPCMLength:TpvInt32;
     ADPCMPointer,ADPCMWorkPointer:pbyte;
     ADPCMCode,ADPCMDiff,ADPCMPredictor,ADPCMStepIndex:TpvInt32;
     ADPCMStep:TpvUInt32;
     Bits,Kaenale:TpvUInt32;
     FloatingPoint:boolean;
     SampleLength{,LengthEx},SampleRate:TpvUInt32;
//   Panning:boolean;
     DataPointer:TpvPointer;
     RealSize:TpvInt32;
     SchleifeStart:TpvUInt32;
     SchleifeEnde:TpvUInt32;
     SustainSchleifeStart:TpvUInt32;
     SustainSchleifeEnde:TpvUInt32;
//   Pan,Volume,GlobalVolume:TpvUInt32;
     Counter,EndValue:TpvInt32;
     LW32:TpvUInt32;
     L32:TpvInt32 absolute LW32;
     S8:pshortint;
     S16:psmallint;
     S24:PSample24Value;
     S32:PpvAudioInt32;
     S32F:PpvAudioFloat;
     S32Out:PpvAudioInt32;
     SampleData:TpvPointer;
     SampleDataSize:TpvUInt32;
//   ItemNr:TpvInt32;
     LoopType:TpvUInt8;
     SustainLoopType:TpvUInt8;
 begin
  result:=false;
  if DataStream.Seek(0,soFromBeginning)<>0 then begin
   exit;
  end;
  if DataStream.Read(Header,sizeof(TWaveFileHeader))<>sizeof(TWaveFileHeader) then begin
   exit;
  end;
  if (Header.Signatur<>'RIFF') and (Header.Signatur<>'LIST') then begin
   exit;
  end;
  if (Header.WAVESignatur<>'WAVE') and (Header.WAVESignatur<>'wave') then begin
   exit;
  end;
 //IF ASSIGNED(WSMPOffset) THEN WSMPOffset^:=0;
  FILLCHAR(WaveFormatHeader,sizeof(TWaveFormatHeader),#0);
  FILLCHAR(WaveFormatHeaderPCM,sizeof(TWaveFormatHeader),#0);
  WaveFormatHeaderExists:=false;
  WaveFormatHeaderPCMExists:=false;
  Fact:=0;
  Data:=0;
  Smpl:=0;
  Xtra:=0;
  List:=0;
  LoopType:=SoundLoopModeNONE;
  SustainLoopType:=SoundLoopModeNONE;
  while (DataStream.Position+8)<DataStream.Size do begin
   if DataStream.Read(WaveChunkHeader,sizeof(TWaveChunkHeader))<>sizeof(TWaveChunkHeader) then begin
    result:=false;
    exit;
   end;
   Next:=DataStream.Position+TpvInt32(WaveChunkHeader.Size);
   if (WaveChunkHeader.Signatur='fmt ') or (WaveChunkHeader.Signatur='fmt'#0) then begin
    if not WaveFormatHeaderExists then begin
     WaveFormatHeaderExists:=true;
     if DataStream.Read(WaveFormatHeader,sizeof(TWaveFormatHeader))<>sizeof(TWaveFormatHeader) then begin
      result:=false;
      exit;
     end;
    end;
   end else if (WaveChunkHeader.Signatur='pcm ') or (WaveChunkHeader.Signatur='pcm'#0) then begin
    if not WaveFormatHeaderPCMExists then begin
     WaveFormatHeaderPCMExists:=true;
     if DataStream.Read(WaveFormatHeaderPCM,sizeof(TWaveFormatHeader))<>sizeof(TWaveFormatHeader) then begin
      result:=false;
      exit;
     end;
    end;
   end else if WaveChunkHeader.Signatur='fact' then begin
    if Fact=0 then begin
     if DataStream.Read(Fact,4)<>4 then begin
      result:=false;
      exit;
     end;
    end;
   end else if WaveChunkHeader.Signatur='data' then begin
    if Data=0 then begin
     Data:=DataStream.Position-sizeof(TWaveChunkHeader);
    end;
   end else if WaveChunkHeader.Signatur='smpl' then begin
    if Smpl=0 then begin
     Smpl:=DataStream.Position-sizeof(TWaveChunkHeader);
    end;
   end else if WaveChunkHeader.Signatur='xtra' then begin
    if Xtra=0 then begin
     Xtra:=DataStream.Position-sizeof(TWaveChunkHeader);
    end;
   end else if WaveChunkHeader.Signatur='list' then begin
    if List=0 then begin
     List:=DataStream.Position-sizeof(TWaveChunkHeader);
    end;
   end else if WaveChunkHeader.Signatur='wsmp' then begin
//  WSMPOffset:=DataStream.Position;
   end;
   if DataStream.Seek(Next,soFromBeginning)<>Next then begin
    result:=false;
    exit;
   end;
  end;
  if WaveFormatHeaderExists and WaveFormatHeaderPCMExists then begin
   if (SwapWordLittleEndian(WaveFormatHeader.FormatTag)<>1) and (SwapWordLittleEndian(WaveFormatHeader.FormatTag)<>3) then begin
    WaveFormatHeaderTemp:=WaveFormatHeader;
    WaveFormatHeader:=WaveFormatHeaderPCM;
    WaveFormatHeaderPCM:=WaveFormatHeaderTemp;
   end else begin
 // WaveFormatHeaderPCMExists:=false;
   end;
  end;
  if (SwapWordLittleEndian(WaveFormatHeader.FormatTag)=1) or (SwapWordLittleEndian(WaveFormatHeader.FormatTag)=3) or (SwapWordLittleEndian(WaveFormatHeader.FormatTag)=$fffe) then begin
   if (SwapWordLittleEndian(WaveFormatHeader.Kaenale)<>1) and (SwapWordLittleEndian(WaveFormatHeader.Kaenale)<>2) then begin
    result:=false;
    exit;
   end;
   if (SwapWordLittleEndian(WaveFormatHeader.BitsProSample)<>8) and (SwapWordLittleEndian(WaveFormatHeader.BitsProSample)<>16) and (SwapWordLittleEndian(WaveFormatHeader.BitsProSample)<>24) and (SwapWordLittleEndian(WaveFormatHeader.BitsProSample)<>32) then begin
    result:=false;
    exit;
   end;
  end else if SwapWordLittleEndian(WaveFormatHeader.FormatTag)=17 then begin
   if SwapWordLittleEndian(WaveFormatHeader.Kaenale)<>1 then begin
    result:=false;
    exit;
   end;
   if SwapWordLittleEndian(WaveFormatHeader.BitsProSample)<>4 then begin
    result:=false;
    exit;
   end;
  end else begin
   result:=false;
   exit;
  end;
  if Data=0 then begin
   result:=false;
   exit;
  end;
  if DataStream.Seek(Data,soFromBeginning)<>TpvInt32(Data) then begin
   result:=false;
   exit;
  end;
  if DataStream.Read(WaveChunkHeader,sizeof(TWaveChunkHeader))<>sizeof(TWaveChunkHeader) then begin
   result:=false;
   exit;
  end;
  Bits:=SwapWordLittleEndian(WaveFormatHeader.BitsProSample);
  Kaenale:=SwapWordLittleEndian(WaveFormatHeader.Kaenale);
  FloatingPoint:=WaveFormatHeader.FormatTag=3;
  if SwapWordLittleEndian(WaveFormatHeader.FormatTag)=17 then begin
   SampleGroesse:=1;
   SampleLength:=(((SwapDWordLittleEndian(WaveChunkHeader.Size)-4)*2)+1) div SampleGroesse;
// LengthEx:=SampleLength;
   FloatingPoint:=false;
  end else begin
   SampleGroesse:=(Kaenale*(Bits shr 3));
   SampleLength:=SwapDWordLittleEndian(WaveChunkHeader.Size) div SampleGroesse;
// LengthEx:=SwapDWordLittleEndian(WaveChunkHeader.Size) div (Bits shr 3);
  end;
  SampleRate:=SwapDWordLittleEndian(WaveFormatHeader.SamplesProSekunde);
//Panning:=false;
  case SwapWordLittleEndian(WaveFormatHeader.FormatTag) of
   1,3,$fffe:begin
    GetMem(DataPointer,SwapDWordLittleEndian(WaveChunkHeader.Size));
    if DataStream.Read(DataPointer^,SwapDWordLittleEndian(WaveChunkHeader.Size))<>TpvInt32(SwapDWordLittleEndian(WaveChunkHeader.Size)) then begin
     result:=false;
     exit;
    end;
    RealSize:=WaveChunkHeader.Size;
    if (Bits=8) and (RealSize>0) then begin
     PB:=DataPointer;
     for I:=1 to RealSize do begin
      PB^:=PB^ xor $80;
      inc(PB);
     end;
    end;
    if (Bits=16) and (RealSize>0) then begin
     PW:=DataPointer;
     for I:=1 to RealSize do begin
      SwapLittleEndianData16(PW^);
      inc(PW);
     end;
    end;
    if (Bits=32) and (RealSize>0) then begin
     PDW:=DataPointer;
     for I:=1 to RealSize do begin
      SwapLittleEndianData32(PDW^);
      inc(PDW);
     end;
    end;
   end;
   17:begin
    Bits:=16;
    ADPCMLength:=SwapDWordLittleEndian(WaveChunkHeader.Size);
    RealSize:=((ADPCMLength-4)*4)+1;
    GetMem(DataPointer,RealSize);
    GetMem(ADPCMPointer,ADPCMLength);
    if DataStream.Read(ADPCMPointer^,ADPCMLength)<>ADPCMLength then begin
     result:=false;
     exit;
    end;
    ADPCMWorkPointer:=ADPCMPointer;
    ADPCMPredictor:=psmallint(ADPCMWorkPointer)^;
    psmallint(DataPointer)^:=ADPCMPredictor;
    ADPCMStepIndex:=TpvUInt8(TpvPointer(@PAnsiChar(ADPCMWorkPointer)[2])^);
    inc(ADPCMWorkPointer,4);
    ADPCMLength:=(ADPCMLength-4) shl 1;
    for I:=0 to ADPCMLength-1 do begin
     ADPCMCode:=TpvUInt8(TpvPointer(@PAnsiChar(ADPCMWorkPointer)[I shr 1])^);
     ADPCMCode:=(ADPCMCode shr ((I and 1) shl 2)) and $f;
     ADPCMStep:=IMAADPCMUnpackTable[ADPCMStepIndex];
     ADPCMDiff:=ADPCMStep shr 3;
     if (ADPCMCode and 1)<>0 then inc(ADPCMDiff,ADPCMStep shr 2);
     if (ADPCMCode and 2)<>0 then inc(ADPCMDiff,ADPCMStep shr 1);
     if (ADPCMCode and 4)<>0 then inc(ADPCMDiff,ADPCMStep);
     if (ADPCMCode and 8)<>0 then ADPCMDiff:=-ADPCMDiff;
     inc(ADPCMPredictor,ADPCMDiff);
     if ADPCMPredictor<-$8000 then begin
      ADPCMPredictor:=-$8000;
     end else if ADPCMPredictor>$7fff then begin
      ADPCMPredictor:=$7fff;
     end;
     smallint(TpvPointer(@PAnsiChar(DataPointer)[(I+1)*sizeof(smallint)])^):=ADPCMPredictor;
     inc(ADPCMStepIndex,IMAADPCMIndexTable[ADPCMCode and 7]);
     if ADPCMStepIndex<0 then begin
      ADPCMStepIndex:=0;
     end else if ADPCMStepIndex>88 then begin
      ADPCMStepIndex:=88;
     end;
    end;
    FreeMem(ADPCMPointer);
   end;
   else begin
 // DataPointer:=NIL;
    result:=false;
    exit;
   end;
  end;
  if Smpl<>0 then begin
   if DataStream.Seek(Smpl,soFromBeginning)<>TpvInt32(Smpl) then begin
    result:=false;
    exit;
   end;
   if DataStream.Read(WaveChunkHeader,sizeof(TWaveChunkHeader))<>sizeof(TWaveChunkHeader) then begin
    result:=false;
    exit;
   end;
   if DataStream.Read(WaveSampleHeader,sizeof(TWaveSampleHeader))<>sizeof(TWaveSampleHeader) then begin
    result:=false;
    exit;
   end;
   if SwapDWordLittleEndian(WaveSampleHeader.SampleLoops)>1 then begin
    if DataStream.Read(WaveSampleLoopHeader,sizeof(TWaveSampleLoopHeader))<>sizeof(TWaveSampleLoopHeader) then begin
     result:=false;
     exit;
    end;
    case WaveSampleLoopHeader.SchleifenTyp of
     1:SustainLoopType:=SoundLoopModePINGPONG;
     2:SustainLoopType:=SoundLoopModeBACKWARD;
     else SustainLoopType:=SoundLoopModeFORWARD;
    end;
    SustainSchleifeStart:=SwapDWordLittleEndian(WaveSampleLoopHeader.SchleifeStart);
    SustainSchleifeEnde:=SwapDWordLittleEndian(WaveSampleLoopHeader.SchleifeEnde);
    if SustainSchleifeStart>=SustainSchleifeEnde then begin
     SustainLoopType:=SoundLoopModeNONE;
    end;
   end;
   if SwapDWordLittleEndian(WaveSampleHeader.SampleLoops)>0 then begin
    if DataStream.Read(WaveSampleLoopHeader,sizeof(TWaveSampleLoopHeader))<>sizeof(TWaveSampleLoopHeader) then begin
     result:=false;
     exit;
    end;
    case WaveSampleLoopHeader.SchleifenTyp of
     1:LoopType:=SoundLoopModePINGPONG;
     2:LoopType:=SoundLoopModeBACKWARD;
     else LoopType:=SoundLoopModeFORWARD;
    end;
    SchleifeStart:=SwapDWordLittleEndian(WaveSampleLoopHeader.SchleifeStart);
    SchleifeEnde:=SwapDWordLittleEndian(WaveSampleLoopHeader.SchleifeEnde);
    if SchleifeStart>=SchleifeEnde then begin
     LoopType:=SoundLoopModeNONE;
    end;
   end;
  end;
  if List<>0 then begin
   if DataStream.Seek(List,soFromBeginning)<>TpvInt32(List) then begin
    result:=false;
    exit;
   end;
   if DataStream.Read(WaveChunkHeader,sizeof(TWaveChunkHeader))<>sizeof(TWaveChunkHeader) then begin
    result:=false;
    exit;
   end;
   if DataStream.Read(WaveInfoHeader,sizeof(TWaveInfoHeader))<>sizeof(TWaveInfoHeader) then begin
    result:=false;
    exit;
   end;
   if WaveInfoHeader='INFO' then begin
    Size:=DataStream.Position+TpvInt32(SwapDWordLittleEndian(WaveChunkHeader.Size));
    while (DataStream.Position+8)<Size do begin
     if DataStream.Read(WaveChunkHeader,sizeof(TWaveChunkHeader))<>sizeof(TWaveChunkHeader) then begin
      result:=false;
      exit;
     end;
     Next:=DataStream.Position+TpvInt32(SwapDWordLittleEndian(WaveChunkHeader.Size));
     if DataStream.Seek(Next,soFromBeginning)<>Next then begin
      result:=false;
      exit;
     end;
    end;
   end;
  end;
  if Xtra<>0 then begin
   if DataStream.Seek(Xtra,soFromBeginning)<>TpvInt32(Xtra) then begin
    result:=false;
    exit;
   end;
   if DataStream.Read(WaveChunkHeader,sizeof(TWaveChunkHeader))<>sizeof(TWaveChunkHeader) then begin
    result:=false;
    exit;
   end;
   if DataStream.Read(WaveXtraHeader,sizeof(TWaveXtraHeader))<>sizeof(TWaveXtraHeader) then begin
    result:=false;
    exit;
   end;
   SwapLittleEndianData32(WaveXtraHeader.Flags);
   SwapLittleEndianData16(WaveXtraHeader.Pan);
   SwapLittleEndianData16(WaveXtraHeader.Volume);
   SwapLittleEndianData16(WaveXtraHeader.GlobalVolume);
   SwapLittleEndianData16(WaveXtraHeader.Reserviert);
{  Pan:=WaveXtraHeader.Pan;
   Volume:=WaveXtraHeader.Volume;
   GlobalVolume:=WaveXtraHeader.GlobalVolume;}
  end;
  if assigned(DataPointer) then begin
   SampleDataSize:=SampleLength*2*sizeof(TpvInt32);
   GetMem(SampleData,SampleDataSize);
   S32Out:=SampleData;
   case Kaenale of
    1:begin
     EndValue:=SampleLength;
     case Bits of
      8:begin
       S8:=DataPointer;
       for Counter:=1 to EndValue do begin
        S32Out^:=S8^ shl 8;
        inc(S32Out);
        S32Out^:=S8^ shl 8;
        inc(S32Out);
        inc(S8);
       end;
      end;
      16:begin
       S16:=DataPointer;
       for Counter:=1 to EndValue do begin
        S32Out^:=S16^;
        inc(S32Out);
        S32Out^:=S16^;
        inc(S32Out);
        inc(S16);
       end;
      end;
      24:begin
       S24:=DataPointer;
       for Counter:=1 to EndValue do begin
        LW32:=(S24^.A shl 8) or (S24^.B shl 16) or (S24^.C shl 24);
        S32Out^:=SARLongint(L32,8);
        inc(S32Out);
        S32Out^:=SARLongint(L32,8);
        inc(S32Out);
        inc(S24);
       end;
      end;
      32:begin
       if FloatingPoint then begin
        S32F:=DataPointer;
        for Counter:=1 to EndValue do begin
         S32Out^:=round(S32F^*32767);
         inc(S32Out);
         S32Out^:=round(S32F^*32767);
         inc(S32Out);
         inc(S32F);
        end;
       end else begin
        S32:=DataPointer;
        for Counter:=1 to EndValue do begin
         S32Out^:=SARLongint(S32^,16);
         inc(S32Out);
         S32Out^:=SARLongint(S32^,16);
         inc(S32Out);
         inc(S32);
        end;
       end;
      end;
     end;
    end;
    2:begin
     EndValue:=SampleLength*2;
     case Bits of
      8:begin
       S8:=DataPointer;
       for Counter:=1 to EndValue do begin
        S32Out^:=S8^ shl 8;
        inc(S8);
        inc(S32Out);
       end;
      end;
      16:begin
       S16:=DataPointer;
       for Counter:=1 to EndValue do begin
        S32Out^:=S16^;
        inc(S16);
        inc(S32Out);
       end;
      end;
      24:begin
       S24:=DataPointer;
       for Counter:=1 to EndValue do begin
        LW32:=(S24^.A shl 8) or (S24^.B shl 16) or (S24^.C shl 24);
        S32Out^:=SARLongint(L32,8);
        inc(S24);
        inc(S32Out);
       end;
      end;
      32:begin
       if FloatingPoint then begin
        S32F:=DataPointer;
        for Counter:=1 to EndValue do begin
         S32Out^:=round(S32F^*32767);
         inc(S32F);
         inc(S32Out);
        end;
       end else begin
        S32:=DataPointer;
        for Counter:=1 to EndValue do begin
         S32Out^:=SARLongint(S32^,16);
         inc(S32);
         inc(S32Out);
        end;
       end;
      end;
     end;
    end;
   end;
   try
    DestSample.SampleLength:=SampleLength;
    DestSample.SampleRate:=SampleRate;
    DestSample.Loop.Mode:=LoopType;
    DestSample.Loop.StartSample:=SchleifeStart;
    DestSample.Loop.EndSample:=SchleifeEnde;
    DestSample.SustainLoop.Mode:=SustainLoopType;
    DestSample.SustainLoop.StartSample:=SustainSchleifeStart;
    DestSample.SustainLoop.EndSample:=SustainSchleifeEnde;
    GetMem(DestSample.Data,(SampleLength+(2*SampleFixUp))*2*sizeof(TpvInt32));
    FillChar(DestSample.Data^,(SampleLength+(2*SampleFixUp))*2*sizeof(TpvInt32),#0);
    inc(PpvAudioInt32(DestSample.Data),2*SampleFixUp);
    System.Move(SampleData^,DestSample.Data^,SampleLength*2*sizeof(TpvInt32));
   finally
    FreeMem(SampleData);
    FreeMem(DataPointer);
   end;
  end;
  result:=true;
 end;
 function LoadOGG:boolean;
 var vf:POggVorbis_File;
     FinalData:PpvAudioInt32s;
     Data:PSmallints;
     DataEx:PSmallint;
     Channels,SampleRate,TotalSamples,Total,BytesIn,bitstream,i,Bytes:TpvInt32;
     info:Pvorbis_info;
 begin
  result:=false;
  New(vf);
  try
   if DataStream.Seek(0,soFromBeginning)=0 then begin
    if ov_open_callbacks(DataStream,vf,nil,0,_ov_open_callbacks)=0 then begin
     info:=ov_info(vf,-1);
     Channels:=info^.channels;
     SampleRate:=info^.rate;
     TotalSamples:=ov_pcm_total(vf,-1);
     GetMem(Data,(TotalSamples+4096)*Channels*sizeof(smallint));
     Bytes:=TotalSamples*Channels*sizeof(smallint);
     FillChar(Data^,Bytes,AnsiChar(#0));
     DataEx:=@Data[0];
     Total:=0;
     bitstream:=0;
     while Total<Bytes do begin
      BytesIn:=ov_read(vf,DataEx,Bytes-Total,@bitstream);
      if BytesIn=OV_HOLE then begin
       continue;
      end else if (BytesIn=OV_EBADLINK) or (BytesIn=OV_EINVAL) or (BytesIn=0) then begin
       break;
      end;
      inc(PAnsiChar(TpvPointer(DataEx)),BytesIn);
      inc(Total,BytesIn);
     end;
     if Total>0 then begin
      GetMem(FinalData,(TotalSamples+(2*SampleFixUp))*2*sizeof(TpvInt32));
      FillChar(FinalData^,(TotalSamples+(2*SampleFixUp))*2*sizeof(TpvInt32),#0);
      inc(PpvAudioInt32(FinalData),2*SampleFixUp);
      case Channels of
       1:begin
        // Mono
        for i:=0 to TotalSamples-1 do begin
         FinalData^[(i*2)+0]:=Data^[i];
         FinalData^[(i*2)+1]:=Data^[i];
        end;
       end;
       2:begin
        // Left, right
        for i:=0 to TotalSamples-1 do begin
         FinalData^[(i*2)+0]:=Data^[(i*2)+0];
         FinalData^[(i*2)+1]:=Data^[(i*2)+1];
        end;
       end;
       3:begin
        // Left, middle, right
        for i:=0 to TotalSamples-1 do begin
         FinalData^[(i*2)+0]:=SARLongint(Data^[(i*3)+0]+Data^[(i*3)+1],1);
         FinalData^[(i*2)+1]:=SARLongint(Data^[(i*3)+2]+Data^[(i*3)+1],1);
        end;
       end;
       4:begin
        // Front left, front right, back left, back right
        for i:=0 to TotalSamples-1 do begin
         FinalData^[(i*2)+0]:=SARLongint(Data^[(i*4)+0]+Data^[(i*4)+2],1);
         FinalData^[(i*2)+1]:=SARLongint(Data^[(i*4)+1]+Data^[(i*4)+3],1);
        end;
       end;
       5:begin
        // Front left, front middle, front right, back left, back right
        for i:=0 to TotalSamples-1 do begin
         FinalData^[(i*2)+0]:=(Data^[(i*5)+0]+Data^[(i*5)+1]+Data^[(i*5)+3]) div 3;
         FinalData^[(i*2)+1]:=(Data^[(i*5)+2]+Data^[(i*5)+1]+Data^[(i*5)+4]) div 3;
        end;
       end;
       6:begin
        // Front left, front middle, front right, back left, back right, LFE channel (subwoofer)
        for i:=0 to TotalSamples-1 do begin
         FinalData^[(i*2)+0]:=SARLongint(Data^[(i*6)+0]+Data^[(i*6)+1]+Data^[(i*6)+3]+Data^[(i*6)+5],2);
         FinalData^[(i*2)+1]:=SARLongint(Data^[(i*6)+2]+Data^[(i*6)+1]+Data^[(i*6)+4]+Data^[(i*6)+5],2);
        end;
       end;
       else begin
        // Undefined, so get only the first two channels in account
        for i:=0 to TotalSamples-1 do begin
         FinalData^[(i*2)+0]:=Data^[(i*Channels)+0];
         FinalData^[(i*2)+1]:=Data^[(i*Channels)+1];
        end;
       end;
      end;
      FreeMem(Data);
      DestSample.SampleLength:=TotalSamples;
      DestSample.SampleRate:=SampleRate;
      DestSample.Loop.Mode:=SoundLoopModeNONE;
      DestSample.Loop.StartSample:=0;
      DestSample.Loop.EndSample:=0;
      DestSample.SustainLoop.Mode:=SoundLoopModeNONE;
      DestSample.SustainLoop.StartSample:=0;
      DestSample.SustainLoop.EndSample:=0;
      DestSample.Data:=TpvPointer(FinalData);
      result:=true;
     end else begin
      FreeMem(Data);
     end;
     ov_clear(vf);
    end;
   end;
  finally
   Dispose(vf);
  end;
 end;
var OK:boolean;
    Signature:array[0..3] of ansichar;
begin
 try
  if Loop=0 then begin
   Name:=Name+#0+'oneshot';
  end else if Loop>1 then begin
   Name:=Name+#0+'loop';
  end;
  result:=HashMap[Name];
  if not assigned(result) then begin
   AudioEngine.CriticalSection.Enter;
   OK:=false;
   try
    DestSample:=TpvAudioSoundSample.Create(AudioEngine,self);
    try
     if assigned(Stream) and (Stream.Size>4) then begin
      DestSample.SamplePolyphony:=Polyphony;
      DestSample.SetPolyphony(Polyphony);
      DataStream:=TMemoryStream.Create;
      try
       if Stream.Seek(0,soFromBeginning)=0 then begin
        DataStream.LoadFromStream(Stream);
        if DataStream.Seek(0,soFromBeginning)=0 then begin
         if DataStream.Read(Signature,SizeOf(Signature))=SizeOf(Signature) then begin
          if DataStream.Seek(0,soFromBeginning)=0 then begin
           if (Signature[0]='O') and (Signature[1]='g') and (Signature[2]='g') and (Signature[3]='S') then begin
            OK:=LoadOGG;
           end else if (Signature[0]='R') and (Signature[1]='I') and (Signature[2]='F') and (Signature[3]='F') then begin
            OK:=LoadWAV;
           end else begin
            OK:=false;
           end;
          end;
         end;
        end;
       end;
      finally
       DataStream.Free;
      end;
     end;
     if OK then begin
      if Loop>1 then begin
       if DestSample.Loop.Mode=SoundLoopModeNONE then begin
        DestSample.Loop.Mode:=SoundLoopModeFORWARD;
        DestSample.Loop.StartSample:=0;
        DestSample.Loop.EndSample:=DestSample.SampleLength;
       end;
      end else if Loop=0 then begin
       DestSample.Loop.Mode:=SoundLoopModeNONE;
       DestSample.SustainLoop.Mode:=SoundLoopModeNONE;
      end;
      DestSample.FixUp;
      Add(DestSample);
      DestSample.Name:=Name;
      HashMap.Add(Name,DestSample);
      result:=DestSample;
      DestSample:=nil;
     end;
    finally
     if assigned(DestSample) then begin
      DestSample.Free;
     end;
    end;
   finally
    AudioEngine.CriticalSection.Leave;
   end;
  end;
  if assigned(result) then begin
   result.IncRef;
  end;
 finally
  if DoFree then begin
   Stream.Free;
  end;
 end;
end;

constructor TpvAudioSoundMusics.Create(AAudioEngine:TpvAudio);
begin
 inherited Create;
 AudioEngine:=AAudioEngine;
 HashMap:=TpvAudioStringHashMap.Create(nil);
end;

destructor TpvAudioSoundMusics.Destroy;
begin
 while Count>0 do begin
  TpvAudioSoundMusic(inherited Items[0]).Free;
 end;
 Clear;
 HashMap.Free;
 inherited Destroy;
end;

function TpvAudioSoundMusics.GetItem(Index:TpvInt32):TpvAudioSoundMusic;
begin
 if (Index>=0) and (Index<Count) then begin
  result:=TpvAudioSoundMusic(inherited Items[Index]);
 end else begin
  result:=nil;
 end;
end;

procedure TpvAudioSoundMusics.SetItem(Index:TpvInt32;Item:TpvAudioSoundMusic);
begin
 if (Index>=0) and (Index<Count) then begin
  inherited Items[Index]:=TpvPointer(Item);
 end;
end;

function TpvAudioSoundMusics.Load(Name:TpvRawByteString;Stream:TStream;DoFree:boolean=true):TpvAudioSoundMusic;
var vf:POggVorbis_File;
    Data:TMemoryStream;
    Music:TpvAudioSoundMusic;
    info:Pvorbis_info;
begin
 try
  result:=HashMap[Name];
  if not assigned(result) then begin
   AudioEngine.CriticalSection.Enter;
   try
    New(vf);
    try
     Data:=TMemoryStream.Create;
     try
      if Stream.Seek(0,soFromBeginning)=0 then begin
       Data.LoadFromStream(Stream);
       if Data.Seek(0,soFromBeginning)=0 then begin
        if ov_open_callbacks(Data,vf,nil,0,_ov_open_callbacks)=0 then begin
         Music:=TpvAudioSoundMusic.Create(AudioEngine,self);
         Music.vf:=vf;
         Music.Data:=Data;
         info:=ov_info(vf,-1);
         Music.Channels:=info^.channels;
         Music.SampleRate:=info^.rate;
         Add(Music);
         Music.Name:=Name;
         HashMap.Add(Name,Music);
         result:=Music;
         Music.InitSINC;
         vf:=nil;
         Data:=nil;
        end else begin
         Data.Free;
         Dispose(vf);
         vf:=nil;
        end;
       end else begin
        Data.Free;
        Dispose(vf);
        vf:=nil;
       end;
      end else begin
       Data.Free;
       Dispose(vf);
       vf:=nil;
      end;
     except
      if assigned(Data) then begin
       Data.Destroy;
      end;
     end;
    except
     if assigned(vf) then begin
      Dispose(vf);
     end;
    end;
   finally
    AudioEngine.CriticalSection.Leave;
   end;
  end;
 finally
  if DoFree then begin
   Stream.Free;
  end;
  Name:='';
 end;
end;

constructor TpvAudioPitchShifter.Create(AAudioEngine:TpvAudio);
var i:TpvInt32;
begin
 inherited Create;
 AudioEngine:=AAudioEngine;
 for i:=0 to PitchShifterBufferSize-1 do begin
  PitchShifterFadeBuffer[i]:=round((0.5+(0.5*cos(((i/(PitchShifterBufferSize-1))-0.5)*2*pi)))*4096);
 end;
 Reset;
end;

destructor TpvAudioPitchShifter.Destroy;
begin
 inherited Destroy;
end;

procedure TpvAudioPitchShifter.Reset;
begin
 FillChar(WorkBuffer,SizeOf(TpvAudioPitchShifterBuffer),AnsiChar(#0));
 p1:=0;
 p2:=(PitchShifterBufferSize shr 1) shl PitchShifterOutputShift;
 InputPointer:=0;
 Factor:=1;
end;

procedure TpvAudioPitchShifter.Process(Buffer:TpvPointer;Samples:TpvInt32);
var Sample:PpvAudioSoundSampleStereoValue;
    Counter,v1,v2:TpvInt32;
    OutputIncrement,up1,up2:TpvUInt32;
begin
 Sample:=Buffer;
 OutputIncrement:=round(Factor*PitchShifterOutputLen)-PitchShifterOutputLen;
 if Factor<>1.0 then begin
  for Counter:=1 to Samples do begin
   WorkBuffer[InputPointer and PitchShifterBufferMask]:=Sample^;
   up1:=p1 shr PitchShifterOutputShift;
   up2:=p2 shr PitchShifterOutputShift;
   v1:=PitchShifterFadeBuffer[up1 and PitchShifterBufferMask];
   v2:=PitchShifterFadeBuffer[up2 and PitchShifterBufferMask];
{$ifdef UseDIV}
   Sample^[0]:=((WorkBuffer[(InputPointer-up1) and PitchShifterBufferMask,0]*v1)+
                (WorkBuffer[(InputPointer-up2) and PitchShifterBufferMask,0]*v2)) div 4096;
   Sample^[1]:=((WorkBuffer[(InputPointer-up1) and PitchShifterBufferMask,1]*v1)+
                (WorkBuffer[(InputPointer-up2) and PitchShifterBufferMask,1]*v2)) div 4096;
{$else}
   Sample^[0]:=SARLongint((WorkBuffer[(InputPointer-up1) and PitchShifterBufferMask,0]*v1)+
                          (WorkBuffer[(InputPointer-up2) and PitchShifterBufferMask,0]*v2),12);
   Sample^[1]:=SARLongint((WorkBuffer[(InputPointer-up1) and PitchShifterBufferMask,1]*v1)+
                          (WorkBuffer[(InputPointer-up2) and PitchShifterBufferMask,1]*v2),12);
{$endif}
   dec(p1,OutputIncrement);
   dec(p2,OutputIncrement);
   InputPointer:=(InputPointer+1) and PitchShifterBufferMask;
   inc(Sample);
  end;
 end else begin
  for Counter:=1 to Samples do begin
   WorkBuffer[InputPointer and PitchShifterBufferMask]:=Sample^;
   InputPointer:=(InputPointer+1) and PitchShifterBufferMask;
   dec(p1,OutputIncrement);
   dec(p2,OutputIncrement);
   inc(Sample);
  end;
 end;
end;

constructor TpvAudioReverb.Create(AAudioEngine:TpvAudio);
var i:TpvInt32;
begin
 inherited Create;
 AudioEngine:=AAudioEngine;
 PreDelay:=6400;
 CombFilterSeparation:=1400;
 RoomSize:=700;
 FeedBack:=0.82;
 Absortion:=1000;
 Dry:=0.75;
 Wet:=0.25;
 AllPassFilters:=16;
 FillChar(AllPassBuffer,SizeOf(AllPassBuffer),AnsiChar(#0));
 FillChar(Counter,SizeOf(Counter),AnsiChar(#0));
 LastLowPassLeft:=0;
 LastLowPassRight:=0;
 LeftBuffer:=nil;
 RightBuffer:=nil;
 LeftDelayedCounter:=0;
 RightDelayedCounter:=0;
 LeftCounter:=0;
 RightCounter:=0;
 SampleBufferSize:=TPasMPMath.RoundUpToPowerOfTwo(AudioEngine.SampleRate*2);
 SampleBufferMask:=SampleBufferSize-1;
 for i:=0 to length(AllPassBuffer)-1 do begin
  SetLength(AllPassBuffer[i],SampleBufferSize);
 end;
 SetLength(LeftBuffer,SampleBufferSize);
 SetLength(RightBuffer,SampleBufferSize);
 Reset;
end;

destructor TpvAudioReverb.Destroy;
var i:TpvInt32;
begin
 for i:=0 to length(AllPassBuffer)-1 do begin
  SetLength(AllPassBuffer[i],0);
 end;
 SetLength(LeftBuffer,0);
 SetLength(RightBuffer,0);
 inherited Destroy;
end;

procedure TpvAudioReverb.Reset;
var i:TpvInt32;
begin
 for i:=0 to length(AllPassBuffer)-1 do begin
  FillChar(AllPassBuffer[i,0],length(AllPassBuffer[i])*SizeOf(TpvAudioReverbAllPassBufferSample),AnsiChar(#0));
 end;
 FillChar(Counter,sizeof(Counter),AnsiChar(#0));
 FillChar(LeftBuffer[0],length(LeftBuffer)*SizeOf(TpvInt32),AnsiChar(#0));
 FillChar(RightBuffer[0],length(RightBuffer)*SizeOf(TpvInt32),AnsiChar(#0));
 LastLowPassLeft:=0;
 LastLowPassRight:=0;
 LeftDelayedCounter:=0;
 RightDelayedCounter:=0;
 LeftCounter:=0;
 RightCounter:=0;
 Init;
end;

procedure TpvAudioReverb.Init;
var AllPassFilterCounter,TimeEx,Time:TpvInt32;
begin
 if AllPassFilters>=MaxReverbAllPassFilters then begin
  AllPassFilters:=MaxReverbAllPassFilters;
 end;
 CutOff:=round(sin(Absortion*(0.0001*(44100/AudioEngine.SampleRate))*pi*0.5)*4096);
 LeftCounter:=(SampleBufferSize-4) and SampleBufferMask;
 RightCounter:=(SampleBufferSize-4) and SampleBufferMask;
 LeftDelayedCounter:=(LeftCounter-round((PreDelay*0.00001)*AudioEngine.SampleRate)) and SampleBufferMask;
 RightDelayedCounter:=(RightCounter-round(((PreDelay*0.00001)+(CombFilterSeparation*0.00001))*AudioEngine.SampleRate)) and SampleBufferMask;
 TimeEx:=trunc(min(max(((RoomSize*0.01)/0.17)+0.5,1),640));
 for AllPassFilterCounter:=0 to AllPassFilters-1 do begin
  Time:=(TimeEx*(AllPassFilterCounter+1))+(AllPassFilterCounter*AllPassFilterCounter);
  Counter[AllPassFilterCounter,0]:=(SampleBufferSize-4) and SampleBufferMask;
  Counter[AllPassFilterCounter,2]:=(Counter[AllPassFilterCounter,0]-Time) and SampleBufferMask;
  Counter[AllPassFilterCounter,1]:=(SampleBufferSize-4) and SampleBufferMask;
  Counter[AllPassFilterCounter,3]:=(Counter[AllPassFilterCounter,1]-(Time+round(AllPassFilterCounter*1.3))) and SampleBufferMask;
 end;
end;

procedure TpvAudioReverb.Process(Buffer:TpvPointer;Samples:TpvInt32);
const ExtraAccurateBits=0;
      ExtraAccurateLength=1 shl ExtraAccurateBits;
var Sample:PpvAudioSoundSampleStereoValue;
    SampleCounter,AllPassFilterCounter,WetFactor,DryFactor,FeedBackFactor,
    OutputLeft,OutputRight,LeftOutput,RightOutput:TpvInt32;
begin
 Sample:=Buffer;
 WetFactor:=round(Wet*4096);
 DryFactor:=round(Dry*4096);
 FeedBackFactor:=round(FeedBack*4096);
 for SampleCounter:=1 to Samples do begin
  LeftBuffer[LeftCounter]:=Sample^[0] shl ExtraAccurateBits;
  RightBuffer[RightCounter]:=Sample^[1] shl ExtraAccurateBits;

  OutputLeft:=LeftBuffer[LeftDelayedCounter];
  OutputRight:=RightBuffer[RightDelayedCounter];

  LeftCounter:=(LeftCounter+1) and SampleBufferMask;
  RightCounter:=(RightCounter+1) and SampleBufferMask;
  LeftDelayedCounter:=(LeftDelayedCounter+1) and SampleBufferMask;
  RightDelayedCounter:=(RightDelayedCounter+1) and SampleBufferMask;

  for AllPassFilterCounter:=0 to AllPassFilters-1 do begin
{$ifdef UseDIV}
   LeftOutput:=((OutputLeft*(-FeedBackFactor)) div 4096)+AllPassBuffer[AllPassFilterCounter,Counter[AllPassFilterCounter,2],0];
   RightOutput:=((OutputRight*(-FeedBackFactor)) div 4096)+AllPassBuffer[AllPassFilterCounter,Counter[AllPassFilterCounter,3],1];
   AllPassBuffer[AllPassFilterCounter,Counter[AllPassFilterCounter,0],0]:=OutputLeft+((LeftOutput*FeedBackFactor) div 4096);
   AllPassBuffer[AllPassFilterCounter,Counter[AllPassFilterCounter,1],1]:=OutputRight+((RightOutput*FeedBackFactor) div 4096);
{$else}
   LeftOutput:=SARLongint(OutputLeft*(-FeedBackFactor),12)+AllPassBuffer[AllPassFilterCounter,Counter[AllPassFilterCounter,2],0];
   RightOutput:=SARLongint(OutputRight*(-FeedBackFactor),12)+AllPassBuffer[AllPassFilterCounter,Counter[AllPassFilterCounter,3],1];
   AllPassBuffer[AllPassFilterCounter,Counter[AllPassFilterCounter,0],0]:=OutputLeft+SARLongint(LeftOutput*FeedBackFactor,12);
   AllPassBuffer[AllPassFilterCounter,Counter[AllPassFilterCounter,1],1]:=OutputRight+SARLongint(RightOutput*FeedBackFactor,12);
{$endif}
   OutputLeft:=LeftOutput;
   OutputRight:=RightOutput;
   Counter[AllPassFilterCounter,0]:=(Counter[AllPassFilterCounter,0]+1) and SampleBufferMask;
   Counter[AllPassFilterCounter,1]:=(Counter[AllPassFilterCounter,1]+1) and SampleBufferMask;
   Counter[AllPassFilterCounter,2]:=(Counter[AllPassFilterCounter,2]+1) and SampleBufferMask;
   Counter[AllPassFilterCounter,3]:=(Counter[AllPassFilterCounter,3]+1) and SampleBufferMask;
  end;

{$ifdef UseDIV}
  inc(LastLowPassLeft,(CutOff*(OutputLeft-LastLowPassLeft)) div 4096);
  inc(LastLowPassRight,(CutOff*(OutputRight-LastLowPassRight)) div 4096);

  Sample^[0]:=((Sample^[0]*DryFactor)+((LastLowPassLeft div ExtraAccurateLength)*WetFactor)) div 4096;
  Sample^[1]:=((Sample^[1]*DryFactor)+((LastLowPassRight div ExtraAccurateLength)*WetFactor)) div 4096;
{$else}
  inc(LastLowPassLeft,SARLongint(CutOff*(OutputLeft-LastLowPassLeft),12));
  inc(LastLowPassRight,SARLongint(CutOff*(OutputRight-LastLowPassRight),12));

  Sample^[0]:=SARLongint((Sample^[0]*DryFactor)+(SARLongint(LastLowPassLeft,ExtraAccurateBits)*WetFactor),12);
  Sample^[1]:=SARLongint((Sample^[1]*DryFactor)+(SARLongint(LastLowPassRight,ExtraAccurateBits)*WetFactor),12);
{$endif}

  inc(Sample);
 end;
end;

constructor TpvAudioThread.Create(AAudioEngine:TpvAudio);
begin
 AudioEngine:=AAudioEngine;
 GetMem(Buffer,AudioEngine.OutputBufferSize);
 FillChar(Buffer^,AudioEngine.OutputBufferSize,AnsiChar(#0));
 FreeOnTerminate:=false;
 Event:=TEvent.Create(nil,false,false,'');
//Priority:=tpHighest;
 inherited Create(false);
end;

destructor TpvAudioThread.Destroy;
begin
 Terminate;
 Event.SetEvent;
 WaitFor;
 Event.Destroy;
 FreeMem(Buffer);
 inherited Destroy;
end;

procedure TpvAudioThread.Start;
begin
 Event.SetEvent;
end;

procedure TpvAudioThread.Execute;
begin
 try
(*{$ifdef windows}
  SetThreadAffinityMask(GetCurrentThread,1 shl GDFW.CPUCores[1]);
{$ifdef win32}
{$ifndef fpc}
  SetThreadIdealProcessor(GetCurrentThread,GDFW.CPUCores[1]);
{$endif}
{$endif}
{$endif}//*)
  SetExceptionMask([exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision]);
  while not Terminated do begin
   if AudioEngine.IsReady and AudioEngine.IsActive then begin
    AudioEngine.CriticalSection.Enter;
    try
     AudioEngine.FillBuffer;
    finally
     AudioEngine.CriticalSection.Leave;
    end;
    if AudioEngine.IsMuted then begin
     FillChar(pansichar(AudioEngine.OutputBuffer)[0],AudioEngine.OutputBufferSize,#0);
    end;
    repeat
     if AudioEngine.RingBuffer.AvailableForWrite>=AudioEngine.OutputBufferSize then begin
      AudioEngine.RingBuffer.Write(AudioEngine.OutputBuffer,AudioEngine.OutputBufferSize);
      break;
     end else begin
      Sleep(1);
     end;
    until Terminated or not (AudioEngine.IsReady and AudioEngine.IsActive);
   end else begin
    InterlockedExchange(Sleeping,-1);
    Event.WaitFor($ffffffff);
    InterlockedExchange(Sleeping,0);
   end;
  end;
 except
  on e:Exception do begin
//   DumpExceptionCallStack(e);
   raise;
  end;
 end;
end;

constructor TpvAudio.Create(ASampleRate,AChannels,ABits,ABufferSamples:TpvInt32);
var i,TableLengthSize:TpvInt32;
    X,TableLength:{$ifdef cpuarm}TpvFloat{$else}TpvDouble{$endif};
begin
 inherited Create;
 AudioInstance:=self;
 CriticalSection:=TCriticalSection.Create;
 SampleRate:=ASampleRate;
 Channels:=AChannels;
 Bits:=ABits;
 BufferSamples:=ABufferSamples;
 BufferChannelSamples:=BufferSamples*2;
 BufferOutputChannelSamples:=BufferSamples*Channels;
 MixingBufferSize:=(BufferSamples*2*32) shr 3;
 OutputBufferSize:=(BufferSamples*Channels*Bits) shr 3;
 GetMem(MixingBuffer,MixingBufferSize);
 GetMem(EffectMixingBuffer,MixingBufferSize);
 GetMem(OutputBuffer,OutputBufferSize);
 SpatializationWaterLowPassCW:=Min(Max(2*sin(pi*(WATER_LOWPASS_FREQUENCY/SampleRate)),0.0),1.0);
 SpatializationWaterWaterBoostLowPassCW:=round(Min(Max(2*sin(pi*(WATER_BOOST_START_FREQUENCY/SampleRate)),0.0),1.0)*4096);
 SpatializationWaterWaterBoostHighPassCW:=round(Min(Max(2*sin(pi*(WATER_BOOST_END_FREQUENCY/SampleRate)),0.0),1.0)*4096);
 SpatializationWaterWaterBoost:=round(WATER_BOOST_FACTOR*4096);
 SpatializationLowPassCW:=Min(Max(cos(2*pi*(HF_FREQUENCY/SampleRate)),0.0),1.0);
 SpatializationDelayAir:=(SampleRate*EAR_DELAY_AIR)*0.001;
 SpatializationDelayUnderwater:=(SampleRate*EAR_DELAY_UNDERWATER)*0.001;
 SpatializationDelayPowerOfTwo:=TPasMPMath.RoundUpToPowerOfTwo(Max(round(Max(SpatializationDelayAir,SpatializationDelayUnderwater)+0.5),HRIR_MAX_DELAY));
 SpatializationDelayMask:=SpatializationDelayPowerOfTwo-1;
 Samples:=TpvAudioSoundSamples.Create(self);
 Musics:=TpvAudioSoundMusics.Create(self);
 HRTFPreset:=@DefaultHRTFPreset;
 HRTF:=ASampleRate=HRTFPreset^.SampleRate;
 iF HRTF then begin
  SpatializationMode:=SPATIALIZATION_HRTF;
 end else begin
  SpatializationMode:=SPATIALIZATION_PSEUDO;
 end;
 MasterVolume:=4096;
 SampleVolume:=32768;
 MusicVolume:=4096;
 AGCActive:=true;
 AGC:=256;
 AGCCounter:=0;
 AGCInterval:=(SampleRate*25) div 1000;
 if AGCInterval<1 then begin
  AGCInterval:=1;
 end;
 RampingSamples:=Max(10,(SampleRate*10) div 1000);
 RampingStepSamples:=Min(Max(1,SampleRate div 1000),RampingSamples);
 TableLengthSize:=1 shl ResamplerCubicSplineFracBits;
 if TableLengthSize>0 then begin
  TableLength:=1/TableLengthSize;
  for i:=0 to TableLengthSize-1 do begin
   x:=i*TableLength;
   CubicSplineTable[i,0]:=round(((-0.5*x*x*x)+(1.0*x*x)-(0.5*x))*ResamplerCubicSplineValueLength);
   CubicSplineTable[i,1]:=round(((1.5*x*x*x)-(2.5*x*x)+1.0)*ResamplerCubicSplineValueLength);
   CubicSplineTable[i,2]:=round(((-1.5*x*x*x)+(2.0*x*x)+(0.5*x))*ResamplerCubicSplineValueLength);
   CubicSplineTable[i,3]:=round(((0.5*x*x*x)-(0.5*x*x))*ResamplerCubicSplineValueLength);
  end;
 end;
 ListenerOrigin:=TpvVector3.Null;
 ListenerVelocity:=TpvVector3.Null;
 ListenerMatrix:=TpvMatrix4x4.Identity;
 ListenerUnderwater:=false;
 LowPassLeft:=0;
 LowPassRight:=0;
 LowPassLast:=LowPassLength shl LowPassShift;
 LowPassCurrent:=LowPassLength shl LowPassShift;
 LowPassIncrement:=0;
 LowPassRampingLength:=0;
 WaterBoostMiddlePassLeft:=0;
 WaterBoostMiddlePassRight:=0;
 FillChar(WaterBoostLowPassLeft,SizeOf(WaterBoostLowPassLeft),AnsiChar(#0));
 FillChar(WaterBoostLowPassRight,SizeOf(WaterBoostLowPassRight),AnsiChar(#0));
 FillChar(WaterBoostHighPassLeft,SizeOf(WaterBoostHighPassLeft),AnsiChar(#0));
 FillChar(WaterBoostHighPassRight,SizeOf(WaterBoostHighPassRight),AnsiChar(#0));
 FillChar(WaterBoostHistoryLeft,SizeOf(WaterBoostHistoryLeft),AnsiChar(#0));
 FillChar(WaterBoostHistoryRight,SizeOf(WaterBoostHistoryRight),AnsiChar(#0));
 UpdateHook:=nil;
 VoiceFirst:=nil;
 VoiceLast:=nil;
 PitchShifter:=TpvAudioPitchShifter.Create(self);
 Reverb:=TpvAudioReverb.Create(self);
 for i:=0 to high(PanningLUT) do begin
  PanningLUT[i]:=round(sin(HalfPI*(i/high(PanningLUT)))*32768.0);
 end;
 PanningLUT[$10000]:=32768;
 RingBuffer:=TPasMPSingleProducerSingleConsumerRingBuffer.Create(OutputBufferSize*2);
 Thread:=TpvAudioThread.Create(self);
 IsReady:=true;
 IsMuted:=false;
 IsActive:=true;
end;

destructor TpvAudio.Destroy;
begin
 FreeAndNil(Thread);
 FreeAndNil(RingBuffer);
 FreeAndNil(Reverb);
 FreeAndNil(PitchShifter);
 FreeAndNil(Samples);
 FreeAndNil(Musics);
 FreeMem(MixingBuffer);
 FreeMem(EffectMixingBuffer);
 FreeMem(OutputBuffer);
 FreeAndNil(CriticalSection);
 AudioInstance:=nil;
 inherited Destroy;
end;

procedure TpvAudio.CalcEvIndices(ev:TpvFloat;evidx:PpvAudioInt32s;var evmu:TpvFloat);
const pi2=pi*2;
begin
 ev:=(pi2+ev)*((HRTFPreset^.evCount-1)/pi);
 evidx^[0]:=IntMod(trunc(ev),HRTFPreset^.evCount);
 evidx^[1]:=Min(evidx^[0]+1,HRTFPreset^.evCount-1);
 evmu:=ev-trunc(ev);
end;


procedure TpvAudio.CalcAzIndices(evidx:TpvInt32;az:TpvFloat;azidx:PpvAudioInt32s;var azmu:TpvFloat);
const pi2=pi*2;
begin
 az:=(pi2+az)*(PpvAudioInt32s(HRTFPreset^.azCount)^[evidx]/pi2);
 azidx^[0]:=IntMod(trunc(az),PpvAudioInt32s(HRTFPreset^.azCount)^[evidx]);
 azidx^[1]:=IntMod(azidx^[0]+1,PpvAudioInt32s(HRTFPreset^.azCount)^[evidx]);
 azmu:=az-floor(az);
end;
                                                                                                      
procedure TpvAudio.GetLerpedHRTFCoefs(Elevation,Azimuth:TpvFloat;var LeftCoefs,RightCoefs:TpvAudioHRTFCoefs;var LeftDelay,RightDelay:TpvInt32);
var evidx,azidx:array[0..1] of TpvInt32;
    mu:array[0..2] of TpvFloat;
    b:array[0..3] of TpvFloat;
    lidx,ridx:array[0..3] of TpvInt32;
    Factor:TpvFloat;
    i:TpvInt32;
begin
 CalcEvIndices(Elevation,@evidx[0],mu[2]);
 CalcAzIndices(evidx[0],Azimuth,@azidx[0],mu[0]);
 lidx[0]:=PpvAudioInt32s(HRTFPreset^.evOffset)^[evidx[0]]+azidx[0];
 lidx[1]:=PpvAudioInt32s(HRTFPreset^.evOffset)^[evidx[0]]+azidx[1];
 ridx[0]:=PpvAudioInt32s(HRTFPreset^.evOffset)^[evidx[0]]+IntMod(PpvAudioInt32s(HRTFPreset^.azCount)^[evidx[0]]-azidx[0],PpvAudioInt32s(HRTFPreset^.azCount)^[evidx[0]]);
 ridx[1]:=PpvAudioInt32s(HRTFPreset^.evOffset)^[evidx[0]]+IntMod(PpvAudioInt32s(HRTFPreset^.azCount)^[evidx[0]]-azidx[1],PpvAudioInt32s(HRTFPreset^.azCount)^[evidx[0]]);
 CalcAzIndices(evidx[1],Azimuth,@azidx[0],mu[1]);
 lidx[2]:=PpvAudioInt32s(HRTFPreset^.evOffset)^[evidx[1]]+azidx[0];
 lidx[3]:=PpvAudioInt32s(HRTFPreset^.evOffset)^[evidx[1]]+azidx[1];
 ridx[2]:=PpvAudioInt32s(HRTFPreset^.evOffset)^[evidx[1]]+IntMod(PpvAudioInt32s(HRTFPreset^.azCount)^[evidx[1]]-azidx[0],PpvAudioInt32s(HRTFPreset^.azCount)^[evidx[1]]);
 ridx[3]:=PpvAudioInt32s(HRTFPreset^.evOffset)^[evidx[1]]+IntMod(PpvAudioInt32s(HRTFPreset^.azCount)^[evidx[1]]-azidx[1],PpvAudioInt32s(HRTFPreset^.azCount)^[evidx[1]]);
 b[0]:=(1.0-mu[0])*(1.0-mu[2]);
 b[1]:=mu[0]*(1.0-mu[2]);
 b[2]:=(1.0-mu[1])*mu[2];
 b[3]:=mu[1]*mu[2];
 if ListenerUnderwater then begin
  Factor:=SpeedOfSoundAirToUnderwater;
 end else begin
  Factor:=1;
 end;
 LeftDelay:=trunc((((PpvAudioInt32s(HRTFPreset^.Delays)^[lidx[0]]*b[0])+
                    (PpvAudioInt32s(HRTFPreset^.Delays)^[lidx[1]]*b[1])+
                    (PpvAudioInt32s(HRTFPreset^.Delays)^[lidx[2]]*b[2])+
                    (PpvAudioInt32s(HRTFPreset^.Delays)^[lidx[3]]*b[3]))*Factor)*SpatializationDelayLength);
 RightDelay:=trunc((((PpvAudioInt32s(HRTFPreset^.Delays)^[ridx[0]]*b[0])+
                     (PpvAudioInt32s(HRTFPreset^.Delays)^[ridx[1]]*b[1])+
                     (PpvAudioInt32s(HRTFPreset^.Delays)^[ridx[2]]*b[2])+
                     (PpvAudioInt32s(HRTFPreset^.Delays)^[ridx[3]]*b[3]))*Factor)*SpatializationDelayLength);
 lidx[0]:=lidx[0]*HRTFPreset^.irSize;
 lidx[1]:=lidx[1]*HRTFPreset^.irSize;
 lidx[2]:=lidx[2]*HRTFPreset^.irSize;
 lidx[3]:=lidx[3]*HRTFPreset^.irSize;
 ridx[0]:=ridx[0]*HRTFPreset^.irSize;
 ridx[1]:=ridx[1]*HRTFPreset^.irSize;
 ridx[2]:=ridx[2]*HRTFPreset^.irSize;
 ridx[3]:=ridx[3]*HRTFPreset^.irSize;
 for i:=0 to HRTFPreset^.irSize-1 do begin
  LeftCoefs[i]:=trunc(((PpvAudioInt32s(HRTFPreset^.Coefs)^[lidx[0]+i]*b[0])+
                       (PpvAudioInt32s(HRTFPreset^.Coefs)^[lidx[1]+i]*b[1])+
                       (PpvAudioInt32s(HRTFPreset^.Coefs)^[lidx[2]+i]*b[2])+
                       (PpvAudioInt32s(HRTFPreset^.Coefs)^[lidx[3]+i]*b[3]))*32768.0);
  RightCoefs[i]:=trunc(((PpvAudioInt32s(HRTFPreset^.Coefs)^[ridx[0]+i]*b[0])+
                        (PpvAudioInt32s(HRTFPreset^.Coefs)^[ridx[1]+i]*b[1])+
                        (PpvAudioInt32s(HRTFPreset^.Coefs)^[ridx[2]+i]*b[2])+
                        (PpvAudioInt32s(HRTFPreset^.Coefs)^[ridx[3]+i]*b[3]))*32768.0);
 end;
end;


procedure TpvAudio.SetMixerMasterVolume(NewVolume:TpvFloat);
begin
 CriticalSection.Enter;
 try
  MasterVolume:=round(NewVolume*4096);
 finally
  CriticalSection.Leave;
 end;
end;

procedure TpvAudio.SetMixerMusicVolume(NewVolume:TpvFloat);
begin
 CriticalSection.Enter;
 try
  MusicVolume:=round(NewVolume*4096);
 finally
  CriticalSection.Leave;
 end;
end;

procedure TpvAudio.SetMixerSampleVolume(NewVolume:TpvFloat);
begin
 CriticalSection.Enter;
 try
  SampleVolume:=round(NewVolume*32768);
 finally
  CriticalSection.Leave;
 end;
end;

procedure TpvAudio.SetMixerAGC(Enabled:boolean);
begin
 CriticalSection.Enter;
 try
  AGCActive:=Enabled;
 finally
  CriticalSection.Leave;
 end;
end;

procedure TpvAudio.Setup;
var i:TpvInt32;
begin
 for i:=0 to Samples.Count-1 do begin
  Samples[i].CorrectPolyphony;
 end;
end;

procedure TpvAudio.ClipBuffer(p:TpvPointer;Range:TpvInt32);
type PpvAudioInt32=^TpvInt32;
var pl:PpvAudioInt32;
    i:TpvInt32;
begin
 pl:=p;
{$ifdef UnrolledLoops}
 for i:=1 to BufferChannelSamples shr 2 do begin
  pl^:=SARLongint(TpvInt32((abs(pl^+Range)-1)-abs(pl^-(Range-1))),1);
  inc(pl);
  pl^:=SARLongint(TpvInt32((abs(pl^+Range)-1)-abs(pl^-(Range-1))),1);
  inc(pl);
  pl^:=SARLongint(TpvInt32((abs(pl^+Range)-1)-abs(pl^-(Range-1))),1);
  inc(pl);
  pl^:=SARLongint(TpvInt32((abs(pl^+Range)-1)-abs(pl^-(Range-1))),1);
  inc(pl);
 end;
 for i:=1 to BufferChannelSamples and 3 do begin
  pl^:=SARLongint(TpvInt32((abs(pl^+Range)-1)-abs(pl^-(Range-1))),1);
  inc(pl);
 end;
{$else}
 for i:=1 to BufferChannelSamples do begin
  pl^:=SARLongint(TpvInt32((abs(pl^+Range)-1)-abs(pl^-(Range-1))),1);
  inc(pl);
 end;
{$endif}
end;

procedure TpvAudio.FillBuffer;
type pbyte=^TpvUInt8;
     psmallint=^smallint;
     PpvAudioInt32=^TpvInt32;
var i,jl,jr,Sample,HighPass,Samples,ToDo,LowPassCoef,Coef:TpvInt32;
    p:TpvPointer;
    pl,pll,plr:PpvAudioInt32;
    ps:psmallint;
    pb:pbyte;
    Voice,NextVoice:TpvAudioSoundSampleVoice;
begin
 CriticalSection.Enter;
 try

  if assigned(UpdateHook) then begin
   UpdateHook;
  end;

  // Clearing
  FillChar(MixingBuffer^,MixingBufferSize,AnsiChar(#0));
  FillChar(EffectMixingBuffer^,MixingBufferSize,AnsiChar(#0));

  // Mixing all sample voices
  Voice:=VoiceFirst;
  while assigned(Voice) do begin
   NextVoice:=Voice.Next;
   if Voice.MixToEffect then begin
    p:=EffectMixingBuffer;
   end else begin
    p:=MixingBuffer;
   end;
   Voice.MixTo(p,SampleVolume);
   Voice:=NextVoice;
  end;

  // Mixing all music streams
  for i:=0 to Musics.Count-1 do begin
   Musics[i].MixTo(MixingBuffer,MusicVolume);
  end;

  if ListenerUnderwater then begin
   PitchShifter.Factor:=0.7937005; // 2^((-4)/12)
  end else begin
   PitchShifter.Factor:=1.0;
  end;
  PitchShifter.Process(EffectMixingBuffer,BufferSamples);

  if ListenerUnderwater then begin
   ClipBuffer(EffectMixingBuffer,524288);
   pl:=TpvPointer(EffectMixingBuffer);
   for i:=1 to BufferSamples do begin
{$ifdef UseDIV}
    Sample:=pl^;
    inc(WaterBoostLowPassLeft[0],((Sample-WaterBoostLowPassLeft[0])*SpatializationWaterWaterBoostLowPassCW) div 4096);
    inc(WaterBoostLowPassLeft[1],((WaterBoostLowPassLeft[0]-WaterBoostLowPassLeft[1])*SpatializationWaterWaterBoostLowPassCW) div 4096);
    inc(WaterBoostLowPassLeft[2],((WaterBoostLowPassLeft[1]-WaterBoostLowPassLeft[2])*SpatializationWaterWaterBoostLowPassCW) div 4096);
    inc(WaterBoostLowPassLeft[3],((WaterBoostLowPassLeft[2]-WaterBoostLowPassLeft[3])*SpatializationWaterWaterBoostLowPassCW) div 4096);
    inc(WaterBoostHighPassLeft[0],((Sample-WaterBoostHighPassLeft[0])*SpatializationWaterWaterBoostHighPassCW) div 4096);
    inc(WaterBoostHighPassLeft[1],((WaterBoostHighPassLeft[0]-WaterBoostHighPassLeft[1])*SpatializationWaterWaterBoostHighPassCW) div 4096);
    inc(WaterBoostHighPassLeft[2],((WaterBoostHighPassLeft[1]-WaterBoostHighPassLeft[2])*SpatializationWaterWaterBoostHighPassCW) div 4096);
    inc(WaterBoostHighPassLeft[3],((WaterBoostHighPassLeft[2]-WaterBoostHighPassLeft[3])*SpatializationWaterWaterBoostHighPassCW) div 4096);
    HighPass:=WaterBoostHistoryLeft[3]-WaterBoostHighPassLeft[3];
    WaterBoostMiddlePassLeft:=WaterBoostHistoryLeft[3]-(WaterBoostLowPassLeft[3]+HighPass);
    WaterBoostHistoryLeft[3]:=WaterBoostHistoryLeft[2];
    WaterBoostHistoryLeft[2]:=WaterBoostHistoryLeft[1];
    WaterBoostHistoryLeft[1]:=Sample;
    pl^:=WaterBoostLowPassLeft[3]+((WaterBoostMiddlePassLeft*SpatializationWaterWaterBoost) div 4096)+HighPass;
    inc(pl);
    Sample:=pl^;
    inc(WaterBoostLowPassRight[0],((Sample-WaterBoostLowPassRight[0])*SpatializationWaterWaterBoostLowPassCW) div 4096);
    inc(WaterBoostLowPassRight[1],((WaterBoostLowPassRight[0]-WaterBoostLowPassRight[1])*SpatializationWaterWaterBoostLowPassCW) div 4096);
    inc(WaterBoostLowPassRight[2],((WaterBoostLowPassRight[1]-WaterBoostLowPassRight[2])*SpatializationWaterWaterBoostLowPassCW) div 4096);
    inc(WaterBoostLowPassRight[3],((WaterBoostLowPassRight[2]-WaterBoostLowPassRight[3])*SpatializationWaterWaterBoostLowPassCW) div 4096);
    inc(WaterBoostHighPassRight[0],((Sample-WaterBoostHighPassRight[0])*SpatializationWaterWaterBoostHighPassCW) div 4096);
    inc(WaterBoostHighPassRight[1],((WaterBoostHighPassRight[0]-WaterBoostHighPassRight[1])*SpatializationWaterWaterBoostHighPassCW) div 4096);
    inc(WaterBoostHighPassRight[2],((WaterBoostHighPassRight[1]-WaterBoostHighPassRight[2])*SpatializationWaterWaterBoostHighPassCW) div 4096);
    inc(WaterBoostHighPassRight[3],((WaterBoostHighPassRight[2]-WaterBoostHighPassRight[3])*SpatializationWaterWaterBoostHighPassCW) div 4096);
    HighPass:=WaterBoostHistoryRight[3]-WaterBoostHighPassRight[3];
    WaterBoostMiddlePassRight:=WaterBoostHistoryRight[3]-(WaterBoostLowPassRight[3]+HighPass);
    WaterBoostHistoryRight[3]:=WaterBoostHistoryRight[2];
    WaterBoostHistoryRight[2]:=WaterBoostHistoryRight[1];
    WaterBoostHistoryRight[1]:=Sample;
    pl^:=WaterBoostLowPassRight[3]+((WaterBoostMiddlePassRight*SpatializationWaterWaterBoost) div 4096)+HighPass;
    inc(pl);
{$else}
    Sample:=pl^;
    inc(WaterBoostLowPassLeft[0],SARLongint((Sample-WaterBoostLowPassLeft[0])*SpatializationWaterWaterBoostLowPassCW,12));
    inc(WaterBoostLowPassLeft[1],SARLongint((WaterBoostLowPassLeft[0]-WaterBoostLowPassLeft[1])*SpatializationWaterWaterBoostLowPassCW,12));
    inc(WaterBoostLowPassLeft[2],SARLongint((WaterBoostLowPassLeft[1]-WaterBoostLowPassLeft[2])*SpatializationWaterWaterBoostLowPassCW,12));
    inc(WaterBoostLowPassLeft[3],SARLongint((WaterBoostLowPassLeft[2]-WaterBoostLowPassLeft[3])*SpatializationWaterWaterBoostLowPassCW,12));
    inc(WaterBoostHighPassLeft[0],SARLongint((Sample-WaterBoostHighPassLeft[0])*SpatializationWaterWaterBoostHighPassCW,12));
    inc(WaterBoostHighPassLeft[1],SARLongint((WaterBoostHighPassLeft[0]-WaterBoostHighPassLeft[1])*SpatializationWaterWaterBoostHighPassCW,12));
    inc(WaterBoostHighPassLeft[2],SARLongint((WaterBoostHighPassLeft[1]-WaterBoostHighPassLeft[2])*SpatializationWaterWaterBoostHighPassCW,12));
    inc(WaterBoostHighPassLeft[3],SARLongint((WaterBoostHighPassLeft[2]-WaterBoostHighPassLeft[3])*SpatializationWaterWaterBoostHighPassCW,12));
    HighPass:=WaterBoostHistoryLeft[3]-WaterBoostHighPassLeft[3];
    WaterBoostMiddlePassLeft:=WaterBoostHistoryLeft[3]-(WaterBoostLowPassLeft[3]+HighPass);
    WaterBoostHistoryLeft[3]:=WaterBoostHistoryLeft[2];
    WaterBoostHistoryLeft[2]:=WaterBoostHistoryLeft[1];
    WaterBoostHistoryLeft[1]:=Sample;                                   
    pl^:=WaterBoostLowPassLeft[3]+SARLongint(WaterBoostMiddlePassLeft*SpatializationWaterWaterBoost,12)+HighPass;
    inc(pl);
    Sample:=pl^;
    inc(WaterBoostLowPassRight[0],SARLongint((Sample-WaterBoostLowPassRight[0])*SpatializationWaterWaterBoostLowPassCW,12));
    inc(WaterBoostLowPassRight[1],SARLongint((WaterBoostLowPassRight[0]-WaterBoostLowPassRight[1])*SpatializationWaterWaterBoostLowPassCW,12));
    inc(WaterBoostLowPassRight[2],SARLongint((WaterBoostLowPassRight[1]-WaterBoostLowPassRight[2])*SpatializationWaterWaterBoostLowPassCW,12));
    inc(WaterBoostLowPassRight[3],SARLongint((WaterBoostLowPassRight[2]-WaterBoostLowPassRight[3])*SpatializationWaterWaterBoostLowPassCW,12));
    inc(WaterBoostHighPassRight[0],SARLongint((Sample-WaterBoostHighPassRight[0])*SpatializationWaterWaterBoostHighPassCW,12));
    inc(WaterBoostHighPassRight[1],SARLongint((WaterBoostHighPassRight[0]-WaterBoostHighPassRight[1])*SpatializationWaterWaterBoostHighPassCW,12));
    inc(WaterBoostHighPassRight[2],SARLongint((WaterBoostHighPassRight[1]-WaterBoostHighPassRight[2])*SpatializationWaterWaterBoostHighPassCW,12));
    inc(WaterBoostHighPassRight[3],SARLongint((WaterBoostHighPassRight[2]-WaterBoostHighPassRight[3])*SpatializationWaterWaterBoostHighPassCW,12));
    HighPass:=WaterBoostHistoryRight[3]-WaterBoostHighPassRight[3];
    WaterBoostMiddlePassRight:=WaterBoostHistoryRight[3]-(WaterBoostLowPassRight[3]+HighPass);
    WaterBoostHistoryRight[3]:=WaterBoostHistoryRight[2];
    WaterBoostHistoryRight[2]:=WaterBoostHistoryRight[1];
    WaterBoostHistoryRight[1]:=Sample;
    pl^:=WaterBoostLowPassRight[3]+SARLongint(WaterBoostMiddlePassRight*SpatializationWaterWaterBoost,12)+HighPass;
    inc(pl);
{$endif}
   end;
   ClipBuffer(EffectMixingBuffer,524288);
  end else begin
   FillChar(WaterBoostLowPassLeft,SizeOf(WaterBoostLowPassLeft),AnsiChar(#0));
   FillChar(WaterBoostLowPassRight,SizeOf(WaterBoostLowPassRight),AnsiChar(#0));
   FillChar(WaterBoostHighPassLeft,SizeOf(WaterBoostHighPassLeft),AnsiChar(#0));
   FillChar(WaterBoostHighPassRight,SizeOf(WaterBoostHighPassRight),AnsiChar(#0));
   FillChar(WaterBoostHistoryLeft,SizeOf(WaterBoostHistoryLeft),AnsiChar(#0));
   FillChar(WaterBoostHistoryRight,SizeOf(WaterBoostHistoryRight),AnsiChar(#0));
  end;

  if ListenerUnderwater then begin
   LowPassCoef:=round(SpatializationWaterLowPassCW*LowPassLength) shl LowPassShift;
  end else begin
   LowPassCoef:=LowPassLength shl LowPassShift;
  end;
  if LowPassLast<>LowPassCoef then begin
   LowPassLast:=LowPassCoef;
   if ListenerUnderwater then begin
    LowPassRampingLength:=RampingSamples;
   end else begin
    LowPassRampingLength:=Max(RampingSamples,(SampleRate+2) shr 2);
   end;
   LowPassIncrement:=(LowPassCoef-LowPassCurrent) div LowPassRampingLength;
  end;
  pl:=TpvPointer(EffectMixingBuffer);
  if (LowPassRampingLength>0) or (LowPassCoef<>(LowPassLength shl LowPassShift)) then begin
   Samples:=BufferSamples;
   while Samples>0 do begin
    ToDo:=Samples;
    if (LowPassRampingLength>0) and (ToDo>LowPassRampingLength) then begin
     ToDo:=LowPassRampingLength;
    end;
    dec(Samples,ToDo);
{$ifdef UseDIV}
    if LowPassRampingLength>0 then begin
     dec(LowPassRampingLength,ToDo);
     for i:=1 to ToDo do begin
      Coef:=LowPassCurrent div LowPassShiftLength;
      inc(LowPassCurrent,LowPassIncrement);
      inc(LowPassLeft,((pl^-LowPassLeft)*Coef) div LowPassLength);
      pl^:=LowPassLeft;
      inc(pl);
      inc(LowPassRight,((pl^-LowPassRight)*Coef) div LowPassLength);
      pl^:=LowPassRight;
      inc(pl);
     end;
     if LowPassRampingLength=0 then begin
      LowPassCurrent:=LowPassCoef;
     end;
    end else begin
     Coef:=LowPassCurrent div LowPassShiftLength;
     for i:=1 to ToDo do begin
      inc(LowPassLeft,((pl^-LowPassLeft)*Coef) div LowPassLength);
      pl^:=LowPassLeft;
      inc(pl);
      inc(LowPassRight,((pl^-LowPassRight)*Coef) div LowPassLength);
      pl^:=LowPassRight;
      inc(pl);
     end;
    end;
{$else}
    if LowPassRampingLength>0 then begin
     dec(LowPassRampingLength,ToDo);
     for i:=1 to ToDo do begin
      Coef:=SARLongint(LowPassCurrent,LowPassShift);
      inc(LowPassCurrent,LowPassIncrement);
      inc(LowPassLeft,SARLongint((pl^-LowPassLeft)*Coef,LowPassBits));
      pl^:=LowPassLeft;
      inc(pl);
      inc(LowPassRight,SARLongint((pl^-LowPassRight)*Coef,LowPassBits));
      pl^:=LowPassRight;
      inc(pl);
     end;
     if LowPassRampingLength=0 then begin
      LowPassCurrent:=LowPassCoef;
     end;
    end else begin
     Coef:=SARLongint(LowPassCurrent,LowPassShift);
     for i:=1 to ToDo do begin
      inc(LowPassLeft,SARLongint((pl^-LowPassLeft)*Coef,LowPassBits));
      pl^:=LowPassLeft;
      inc(pl);
      inc(LowPassRight,SARLongint((pl^-LowPassRight)*Coef,LowPassBits));
      pl^:=LowPassRight;
      inc(pl);
     end;
    end;
{$endif}
   end;
  end else if BufferSamples>1 then begin
   inc(pl,(BufferSamples-1) shl 1);
   LowPassLeft:=pl^;
   inc(pl);
   LowPassRight:=pl^;
  end;

  if ListenerUnderwater then begin
   Reverb.Dry:=1.0;
   Reverb.Wet:=1.0;
  end else begin
   Reverb.Dry:=1.0;
   Reverb.Wet:=0.0;
  end;
  Reverb.Process(EffectMixingBuffer,BufferSamples);

  pl:=TpvPointer(MixingBuffer);
  pll:=TpvPointer(EffectMixingBuffer);
  for i:=1 to BufferSamples do begin
   inc(pl^,pll^);
   inc(pl);
   inc(pll);
   inc(pl^,pll^);
   inc(pl);
   inc(pll);
  end;

  // Apply master volume
  if MasterVolume<>4096 then begin
   ClipBuffer(MixingBuffer,524288);
   pl:=TpvPointer(MixingBuffer);
 {$ifdef UnrolledLoops}
   for i:=1 to BufferChannelSamples shr 2 do begin
    pl^:=SARLongint(pl^*MasterVolume,12);
    inc(pl);
    pl^:=SARLongint(pl^*MasterVolume,12);
    inc(pl);
    pl^:=SARLongint(pl^*MasterVolume,12);
    inc(pl);
    pl^:=SARLongint(pl^*MasterVolume,12);
    inc(pl);
   end;
   for i:=1 to BufferChannelSamples and 3 do begin
    pl^:=SARLongint(pl^*MasterVolume,12);
    inc(pl);
   end;
 {$else}
   for i:=1 to BufferChannelSamples do begin
    pl^:=SARLongint(pl^*MasterVolume,12);
    inc(pl);
   end;
 {$endif}
  end;

  if AGCActive then begin
  // Automatic gain control
   pl:=TpvPointer(MixingBuffer);
   for i:=1 to BufferSamples do begin
    jl:=SARLongint(pl^*AGC,8);
    pl^:=SARLongint(TpvInt32((abs(jl+32768)-1)-abs(jl-32767)),1);
    inc(pl);
    jr:=SARLongint(pl^*AGC,8);
    pl^:=SARLongint(TpvInt32((abs(jr+32768)-1)-abs(jr-32767)),1);
    inc(pl);
    if ((jl<-32768) or (jl>32767)) or ((jr<-32768) or (jr>32767)) then begin
     dec(AGC);
     AGCCounter:=0;
    end else begin
     if AGC<256 then begin
      inc(AGCCounter);
      if AGCCounter>=AGCInterval then begin
       AGCCounter:=0;
       inc(AGC);
      end;
     end;
    end;
   end;
  end else begin
   // Clipping (condition-less!)
   pl:=TpvPointer(MixingBuffer);
{$ifdef UnrolledLoops}
   for i:=1 to BufferChannelSamples shr 2 do begin
    pl^:=SARLongint(TpvInt32((abs(pl^+32768)-1)-abs(pl^-32767)),1);
    inc(pl);
    pl^:=SARLongint(TpvInt32((abs(pl^+32768)-1)-abs(pl^-32767)),1);
    inc(pl);
    pl^:=SARLongint(TpvInt32((abs(pl^+32768)-1)-abs(pl^-32767)),1);
    inc(pl);
    pl^:=SARLongint(TpvInt32((abs(pl^+32768)-1)-abs(pl^-32767)),1);
    inc(pl);
   end;
   for i:=1 to BufferChannelSamples and 3 do begin
    pl^:=SARLongint(TpvInt32((abs(pl^+32768)-1)-abs(pl^-32767)),1);
    inc(pl);
   end;
{$else}
   for i:=1 to BufferChannelSamples do begin
    pl^:=SARLongint(TpvInt32((abs(pl^+32768)-1)-abs(pl^-32767)),1);
    inc(pl);
   end;
{$endif}
  end;

  // Downmixing
  if Channels=1 then begin
   pl:=TpvPointer(MixingBuffer);
   pll:=pl;
   plr:=pl;
   inc(plr);
{$ifdef UnrolledLoops}
   for i:=1 to BufferSamples shr 3 do begin
    pl^:=SARLongint(pll^+plr^,1);
    inc(pl);
    inc(pll,2);
    inc(plr,2);
    pl^:=SARLongint(pll^+plr^,1);
    inc(pl);
    inc(pll,2);
    inc(plr,2);
    pl^:=SARLongint(pll^+plr^,1);
    inc(pl);
    inc(pll,2);
    inc(plr,2);
    pl^:=SARLongint(pll^+plr^,1);
    inc(pl);
    inc(pll,2);
    inc(plr,2);
   end;
   for i:=1 to BufferSamples and 3 do begin
    pl^:=SARLongint(pll^+plr^,1);
    inc(pl);
    inc(pll,2);
    inc(plr,2);
   end;
{$else}
   for i:=1 to BufferSamples do begin
    pl^:=SARLongint(pll^+plr^,1);
    inc(pl);
    inc(pll,2);
    inc(plr,2);
   end;
{$endif}
  end;
  case Bits of
   8:begin
    pb:=TpvPointer(OutputBuffer);
    pl:=TpvPointer(MixingBuffer);
{$ifdef UnrolledLoops}
    for i:=1 to BufferOutputChannelSamples shr 2 do begin
     pb^:=(pl^+32768) shr 8;
     inc(pb);
     inc(pl);
     pb^:=(pl^+32768) shr 8;
     inc(pb);
     inc(pl);
     pb^:=(pl^+32768) shr 8;
     inc(pb);
     inc(pl);
     pb^:=(pl^+32768) shr 8;
     inc(pb);
     inc(pl);
    end;
    for i:=1 to BufferOutputChannelSamples and 3 do begin
     pb^:=(pl^+32768) shr 8;
     inc(pb);
     inc(pl);
    end;
{$else}
    for i:=1 to BufferOutputChannelSamples do begin
     pb^:=(pl^+32768) shr 8;
     inc(pb);
     inc(pl);
    end;
{$endif}
   end;
   16:begin
    ps:=TpvPointer(OutputBuffer);
    pl:=TpvPointer(MixingBuffer);
{$ifdef UnrolledLoops}
    for i:=1 to BufferOutputChannelSamples shr 2 do begin
     ps^:=pl^;
     inc(ps);
     inc(pl);
     ps^:=pl^;
     inc(ps);
     inc(pl);
     ps^:=pl^;
     inc(ps);
     inc(pl);
     ps^:=pl^;
     inc(ps);
     inc(pl);
    end;
    for i:=1 to BufferOutputChannelSamples and 3 do begin
     ps^:=pl^;
     inc(ps);
     inc(pl);
    end;
{$else}
    for i:=1 to BufferOutputChannelSamples do begin
     ps^:=pl^;
     inc(ps);
     inc(pl);
    end;
{$endif}
   end;
  end;
 finally
  CriticalSection.Leave;
 end;
end;

procedure TpvAudio.Lock;
begin
 CriticalSection.Enter;
 try
  IsReady:=false;
 finally
  CriticalSection.Leave;
 end;
end;

procedure TpvAudio.Unlock;
begin
 CriticalSection.Enter;
 try
  IsReady:=true;
 finally
  CriticalSection.Leave;
 end;
end;

procedure TpvAudio.SetActive(Active:boolean);
begin
 CriticalSection.Enter;
 try
  IsActive:=Active;
 finally
  CriticalSection.Leave;
 end;
end;

procedure TpvAudio.Mute;
begin
 CriticalSection.Enter;
 try
  IsMuted:=true;
 finally
  CriticalSection.Leave;
 end;
end;

procedure TpvAudio.Unmute;
begin
 CriticalSection.Enter;
 try
  IsMuted:=false;
 finally
  CriticalSection.Leave;
 end;
end;

initialization
end.


