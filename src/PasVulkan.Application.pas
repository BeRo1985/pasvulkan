(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016-2017, Benjamin Rosseaux (benjamin@rosseaux.de)          *
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
unit PasVulkan.Application;
{$i PasVulkan.inc}
{$ifndef fpc}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}

interface

uses {$if defined(Unix)}
      BaseUnix,
      Unix,
      UnixType,
      {$ifdef linux}
       linux,
      {$endif}
      ctypes,
     {$elseif defined(Windows)}
      Windows,
      MMSystem,
      Registry,
     {$ifend}
     SysUtils,
     Classes,
     SyncObjs,
     Math,
     PasMP,
     PUCU,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
{$if defined(PasVulkanUseSDL2)}
     PasVulkan.SDL2,
{$ifend}
     PasVulkan.Android,
     PasVulkan.Audio;

const MaxSwapChainImages=3;

      FrameTimesHistorySize=1 shl 10;

      LOG_NONE=0;
      LOG_INFO=1;
      LOG_VERBOSE=2;
      LOG_DEBUG=3;
      LOG_ERROR=4;

      EVENT_NONE=0;
      EVENT_KEY=1;
      EVENT_POINTER=2;
      EVENT_SCROLLED=3;

      KEYCODE_ANYKEY=-1;
      KEYCODE_UNKNOWN=0;
      KEYCODE_FIRST=0;
      KEYCODE_BACKSPACE=8;
      KEYCODE_TAB=9;
      KEYCODE_RETURN=13;
      KEYCODE_PAUSE=19;
      KEYCODE_ESCAPE=27;
      KEYCODE_SPACE=32;
      KEYCODE_EXCLAIM=33;
      KEYCODE_QUOTEDBL=34;
      KEYCODE_HASH=35;
      KEYCODE_DOLLAR=36;
      KEYCODE_AMPERSAND=38;
      KEYCODE_QUOTE=39;
      KEYCODE_LEFTPAREN=40;
      KEYCODE_RIGHTPAREN=41;
      KEYCODE_ASTERISK=42;
      KEYCODE_PLUS=43;
      KEYCODE_COMMA=44;
      KEYCODE_MINUS=45;
      KEYCODE_PERIOD=46;
      KEYCODE_SLASH=47;
      KEYCODE_0=48;
      KEYCODE_1=49;
      KEYCODE_2=50;
      KEYCODE_3=51;
      KEYCODE_4=52;
      KEYCODE_5=53;
      KEYCODE_6=54;
      KEYCODE_7=55;
      KEYCODE_8=56;
      KEYCODE_9=57;
      KEYCODE_COLON=58;
      KEYCODE_SEMICOLON=59;
      KEYCODE_LESS=60;
      KEYCODE_EQUALS=61;
      KEYCODE_GREATER=62;
      KEYCODE_QUESTION=63;
      KEYCODE_AT=64;
      KEYCODE_LEFTBRACKET=91;
      KEYCODE_BACKSLASH=92;
      KEYCODE_RIGHTBRACKET=93;
      KEYCODE_CARET=94;
      KEYCODE_UNDERSCORE=95;
      KEYCODE_BACKQUOTE=96;
      KEYCODE_A=97;
      KEYCODE_B=98;
      KEYCODE_C=99;
      KEYCODE_D=100;
      KEYCODE_E=101;
      KEYCODE_F=102;
      KEYCODE_G=103;
      KEYCODE_H=104;
      KEYCODE_I=105;
      KEYCODE_J=106;
      KEYCODE_K=107;
      KEYCODE_L=108;
      KEYCODE_M=109;
      KEYCODE_N=110;
      KEYCODE_O=111;
      KEYCODE_P=112;
      KEYCODE_Q=113;
      KEYCODE_R=114;
      KEYCODE_S=115;
      KEYCODE_T=116;
      KEYCODE_U=117;
      KEYCODE_V=118;
      KEYCODE_W=119;
      KEYCODE_X=120;
      KEYCODE_Y=121;
      KEYCODE_Z=122;
      KEYCODE_DELETE=177;
      KEYCODE_F1=256;
      KEYCODE_F2=257;
      KEYCODE_F3=258;
      KEYCODE_F4=259;
      KEYCODE_F5=260;
      KEYCODE_F6=261;
      KEYCODE_F7=262;
      KEYCODE_F8=263;
      KEYCODE_F9=264;
      KEYCODE_F10=265;
      KEYCODE_F11=266;
      KEYCODE_F12=267;
      KEYCODE_F13=268;
      KEYCODE_F14=269;
      KEYCODE_F15=270;
      KEYCODE_F16=271;
      KEYCODE_F17=272;
      KEYCODE_F18=273;
      KEYCODE_F19=274;
      KEYCODE_F20=275;
      KEYCODE_F21=276;
      KEYCODE_F22=277;
      KEYCODE_F23=278;
      KEYCODE_F24=279;
      KEYCODE_KP0=280;
      KEYCODE_KP1=281;
      KEYCODE_KP2=282;
      KEYCODE_KP3=283;
      KEYCODE_KP4=284;
      KEYCODE_KP5=285;
      KEYCODE_KP6=286;
      KEYCODE_KP7=287;
      KEYCODE_KP8=288;
      KEYCODE_KP9=289;
      KEYCODE_KP_PERIOD=290;
      KEYCODE_KP_DIVIDE=291;
      KEYCODE_KP_MULTIPLY=292;
      KEYCODE_KP_MINUS=293;
      KEYCODE_KP_PLUS=294;
      KEYCODE_KP_ENTER=295;
      KEYCODE_KP_EQUALS=296;
      KEYCODE_UP=297;
      KEYCODE_DOWN=298;
      KEYCODE_RIGHT=299;
      KEYCODE_LEFT=300;
      KEYCODE_INSERT=301;
      KEYCODE_HOME=302;
      KEYCODE_END=303;
      KEYCODE_PAGEUP=304;
      KEYCODE_PAGEDOWN=305;
      KEYCODE_CAPSLOCK=306;
      KEYCODE_NUMLOCK=307;
      KEYCODE_SCROLLOCK=308;
      KEYCODE_RSHIFT=309;
      KEYCODE_LSHIFT=310;
      KEYCODE_RCTRL=311;
      KEYCODE_LCTRL=312;
      KEYCODE_RALT=313;
      KEYCODE_LALT=314;
      KEYCODE_MODE=315;
      KEYCODE_HELP=316;
      KEYCODE_PRINTSCREEN=317;
      KEYCODE_SYSREQ=318;
      KEYCODE_MENU=319;
      KEYCODE_POWER=320;
      KEYCODE_APPLICATION=321;
      KEYCODE_SELECT=322;
      KEYCODE_STOP=323;
      KEYCODE_AGAIN=324;
      KEYCODE_UNDO=325;
      KEYCODE_CUT=326;
      KEYCODE_COPY=327;
      KEYCODE_PASTE=328;
      KEYCODE_FIND=329;
      KEYCODE_MUTE=330;
      KEYCODE_VOLUMEUP=331;
      KEYCODE_VOLUMEDOWN=332;
      KEYCODE_KP_EQUALSAS400=333;
      KEYCODE_ALTERASE=334;
      KEYCODE_CANCEL=335;
      KEYCODE_CLEAR=336;
      KEYCODE_PRIOR=337;
      KEYCODE_RETURN2=338;
      KEYCODE_SEPARATOR=339;
      KEYCODE_OUT=340;
      KEYCODE_OPER=341;
      KEYCODE_CLEARAGAIN=342;
      KEYCODE_CRSEL=343;
      KEYCODE_EXSEL=344;
      KEYCODE_KP_00=345;
      KEYCODE_KP_000=346;
      KEYCODE_THOUSANDSSEPARATOR=34;
      KEYCODE_DECIMALSEPARATOR=348;
      KEYCODE_CURRENCYUNIT=349;
      KEYCODE_CURRENCYSUBUNIT=350;
      KEYCODE_KP_LEFTPAREN=351;
      KEYCODE_KP_RIGHTPAREN=352;
      KEYCODE_KP_LEFTBRACE=353;
      KEYCODE_KP_RIGHTBRACE=354;
      KEYCODE_KP_TAB=355;
      KEYCODE_KP_BACKSPACE=356;
      KEYCODE_KP_A=357;
      KEYCODE_KP_B=358;
      KEYCODE_KP_C=359;
      KEYCODE_KP_D=360;
      KEYCODE_KP_E=361;
      KEYCODE_KP_F=362;
      KEYCODE_KP_XOR=363;
      KEYCODE_KP_POWER=364;
      KEYCODE_KP_PERCENT=365;
      KEYCODE_KP_LESS=366;
      KEYCODE_KP_GREATER=368;
      KEYCODE_KP_AMPERSAND=369;
      KEYCODE_KP_DBLAMPERSAND=370;
      KEYCODE_KP_VERTICALBAR=371;
      KEYCODE_KP_DBLVERTICALBAR=372;
      KEYCODE_KP_COLON=373;
      KEYCODE_KP_HASH=374;
      KEYCODE_KP_SPACE=375;
      KEYCODE_KP_AT=376;
      KEYCODE_KP_EXCLAM=377;
      KEYCODE_KP_MEMSTORE=378;
      KEYCODE_KP_MEMRECALL=379;
      KEYCODE_KP_MEMCLEAR=380;
      KEYCODE_KP_MEMADD=381;
      KEYCODE_KP_MEMSUBTRACT=382;
      KEYCODE_KP_MEMMULTIPLY=383;
      KEYCODE_KP_MEMDIVIDE=384;
      KEYCODE_KP_PLUSMINUS=385;
      KEYCODE_KP_CLEAR=386;
      KEYCODE_KP_CLEARENTRY=387;
      KEYCODE_KP_BINARY=388;
      KEYCODE_KP_OCTAL=389;
      KEYCODE_KP_DECIMAL=390;
      KEYCODE_KP_HEXADECIMAL=391;
      KEYCODE_LGUI=392;
      KEYCODE_RGUI=393;
      KEYCODE_AUDIONEXT=394;
      KEYCODE_AUDIOPREV=395;
      KEYCODE_AUDIOSTOP=396;
      KEYCODE_AUDIOPLAY=397;
      KEYCODE_AUDIOMUTE=398;
      KEYCODE_MEDIASELECT=399;
      KEYCODE_WWW=400;
      KEYCODE_MAIL=401;
      KEYCODE_CALCULATOR=402;
      KEYCODE_COMPUTER=403;
      KEYCODE_AC_SEARCH=404;
      KEYCODE_AC_HOME=405;
      KEYCODE_AC_BACK=406;
      KEYCODE_AC_FORWARD=407;
      KEYCODE_AC_STOP=408;
      KEYCODE_AC_REFRESH=409;
      KEYCODE_AC_BOOKMARKS=410;
      KEYCODE_BRIGHTNESSDOWN=411;
      KEYCODE_BRIGHTNESSUP=412;
      KEYCODE_DISPLAYSWITCH=413;
      KEYCODE_KBDILLUMTOGGLE=414;
      KEYCODE_KBDILLUMDOWN=415;
      KEYCODE_KBDILLUMUP=416;
      KEYCODE_EJECT=417;
      KEYCODE_SLEEP=418;
      KEYCODE_INTERNATIONAL1=419;
      KEYCODE_INTERNATIONAL2=420;
      KEYCODE_INTERNATIONAL3=421;
      KEYCODE_INTERNATIONAL4=422;
      KEYCODE_INTERNATIONAL5=423;
      KEYCODE_INTERNATIONAL6=424;
      KEYCODE_INTERNATIONAL7=425;
      KEYCODE_INTERNATIONAL8=426;
      KEYCODE_INTERNATIONAL9=427;
      KEYCODE_LANG1=428;
      KEYCODE_LANG2=429;
      KEYCODE_LANG3=430;
      KEYCODE_LANG4=431;
      KEYCODE_LANG5=432;
      KEYCODE_LANG6=433;
      KEYCODE_LANG7=434;
      KEYCODE_LANG8=435;
      KEYCODE_LANG9=436;
      KEYCODE_LOCKINGCAPSLOCK=437;
      KEYCODE_LOCKINGNUMLOCK=438;
      KEYCODE_LOCKINGSCROLLLOCK=439;
      KEYCODE_NONUSBACKSLASH=440;
      KEYCODE_NONUSHASH=441;
      KEYCODE_BACK=442;
      KEYCODE_CAMERA=443;
      KEYCODE_CALL=444;
      KEYCODE_CENTER=445;
      KEYCODE_FORWARD_DEL=446;
      KEYCODE_DPAD_CENTER=447;
      KEYCODE_DPAD_LEFT=448;
      KEYCODE_DPAD_RIGHT=449;
      KEYCODE_DPAD_DOWN=450;
      KEYCODE_DPAD_UP=451;
      KEYCODE_ENDCALL=452;
      KEYCODE_ENVELOPE=453;
      KEYCODE_EXPLORER=454;
      KEYCODE_FOCUS=455;
      KEYCODE_GRAVE=456;
      KEYCODE_HEADSETHOOK=457;
      KEYCODE_AUDIO_FAST_FORWARD=458;
      KEYCODE_AUDIO_REWIND=459;
      KEYCODE_NOTIFICATION=460;
      KEYCODE_PICTSYMBOLS=461;
      KEYCODE_SWITCH_CHARSET=462;
      KEYCODE_BUTTON_CIRCLE=463;
      KEYCODE_BUTTON_A=464;
      KEYCODE_BUTTON_B=465;
      KEYCODE_BUTTON_C=466;
      KEYCODE_BUTTON_X=467;
      KEYCODE_BUTTON_Y=468;
      KEYCODE_BUTTON_Z=469;
      KEYCODE_BUTTON_L1=470;
      KEYCODE_BUTTON_R1=471;
      KEYCODE_BUTTON_L2=472;
      KEYCODE_BUTTON_R2=473;
      KEYCODE_BUTTON_THUMBL=474;
      KEYCODE_BUTTON_THUMBR=475;
      KEYCODE_BUTTON_START=476;
      KEYCODE_BUTTON_SELECT=477;
      KEYCODE_BUTTON_MODE=478;

      ORIENTATION_LANDSCAPE=0;
      ORIENTATION_PORTRAIT=1;

      PERIPHERAL_HARDWAREKEYBOARD=0;
      PERIPHERAL_ONSCEENKEYBOARD=1;
      PERIPHERAL_MULTITOUCHSCREEN=2;
      PERIPHERAL_ACCELEROMETER=3;
      PERIPHERAL_COMPASS=4;
      PERIPHERAL_VIBRATOR=5;

      JOYSTICK_HAT_NONE=0;
      JOYSTICK_HAT_LEFTUP=1;
      JOYSTICK_HAT_UP=2;
      JOYSTICK_HAT_RIGHTUP=3;
      JOYSTICK_HAT_LEFT=4;
      JOYSTICK_HAT_CENTERED=5;
      JOYSTICK_HAT_RIGHT=6;
      JOYSTICK_HAT_LEFTDOWN=7;
      JOYSTICK_HAT_DOWN=8;
      JOYSTICK_HAT_RIGHTDOWN=9;

      GAME_CONTROLLER_BINDTYPE_NONE=0;
      GAME_CONTROLLER_BINDTYPE_BUTTON=1;
      GAME_CONTROLLER_BINDTYPE_AXIS=2;
      GAME_CONTROLLER_BINDTYPE_HAT=3;

      GAME_CONTROLLER_AXIS_INVALID=-1;
      GAME_CONTROLLER_AXIS_LEFTX=0;
      GAME_CONTROLLER_AXIS_LEFTY=1;
      GAME_CONTROLLER_AXIS_RIGHTX=2;
      GAME_CONTROLLER_AXIS_RIGHTY=3;
      GAME_CONTROLLER_AXIS_TRIGGERLEFT=4;
      GAME_CONTROLLER_AXIS_TRIGGERRIGHT=5;
      GAME_CONTROLLER_AXIS_MAX=6;

      GAME_CONTROLLER_BUTTON_INVALID=-1;
      GAME_CONTROLLER_BUTTON_A=0;
      GAME_CONTROLLER_BUTTON_B=1;
      GAME_CONTROLLER_BUTTON_X=2;
      GAME_CONTROLLER_BUTTON_Y=3;
      GAME_CONTROLLER_BUTTON_BACK=4;
      GAME_CONTROLLER_BUTTON_GUIDE=5;
      GAME_CONTROLLER_BUTTON_START=6;
      GAME_CONTROLLER_BUTTON_LEFTSTICK=7;
      GAME_CONTROLLER_BUTTON_RIGHTSTICK=8;
      GAME_CONTROLLER_BUTTON_LEFTSHOULDER=9;
      GAME_CONTROLLER_BUTTON_RIGHTSHOULDER=10;
      GAME_CONTROLLER_BUTTON_DPAD_UP=11;
      GAME_CONTROLLER_BUTTON_DPAD_DOWN=12;
      GAME_CONTROLLER_BUTTON_DPAD_LEFT=13;
      GAME_CONTROLLER_BUTTON_DPAD_RIGHT=14;
      GAME_CONTROLLER_BUTTON_MAX=15;

type EpvApplication=class(Exception)
      private
       fTag:string;
       fLogLevel:TpvInt32;
      public
       constructor Create(const aTag,aMessage:string;const aLogLevel:TpvInt32=LOG_NONE); reintroduce; virtual;
       destructor Destroy; override;
      published
       property Tag:string read fTag write fTag;
       property LogLevel:TpvInt32 read fLogLevel write fLogLevel;
     end;

     EpvApplicationClass=class of EpvApplication;

     TpvApplicationRunnable=procedure of object;

     TpvApplicationRunnableList=array of TpvApplicationRunnable;

     TpvApplication=class;

     TpvApplicationClass=class of TpvApplication;

     TpvApplicationRawByteString={$if declared(RawByteString)}RawByteString{$else}AnsiString{$ifend};

     TpvApplicationUnicodeString={$if declared(UnicodeString)}UnicodeString{$else}WideString{$ifend};

     TpvApplicationUTF8String={$if declared(UnicodeString)}UTF8String{$else}AnsiString{$ifend};

     TpvApplicationOnStep=procedure(const aVulkanApplication:TpvApplication) of object;

     PPpvApplicationHighResolutionTime=^PpvApplicationHighResolutionTime;
     PpvApplicationHighResolutionTime=^TpvApplicationHighResolutionTime;
     TpvApplicationHighResolutionTime=TpvInt64;

     TpvApplicationHighResolutionTimer=class
      private
       fFrequency:TpvInt64;
       fFrequencyShift:TpvInt32;
       fMillisecondInterval:TpvApplicationHighResolutionTime;
       fTwoMillisecondsInterval:TpvApplicationHighResolutionTime;
       fFourMillisecondsInterval:TpvApplicationHighResolutionTime;
       fQuarterSecondInterval:TpvApplicationHighResolutionTime;
       fMinuteInterval:TpvApplicationHighResolutionTime;
       fHourInterval:TpvApplicationHighResolutionTime;
      public
       constructor Create;
       destructor Destroy; override;
       function GetTime:TpvInt64;
       procedure Sleep(const aDelay:TpvApplicationHighResolutionTime);
       function ToFloatSeconds(const aTime:TpvApplicationHighResolutionTime):TpvDouble;
       function FromFloatSeconds(const aTime:TpvDouble):TpvApplicationHighResolutionTime;
       function ToMilliseconds(const aTime:TpvApplicationHighResolutionTime):TpvInt64;
       function FromMilliseconds(const aTime:TpvInt64):TpvApplicationHighResolutionTime;
       function ToMicroseconds(const aTime:TpvApplicationHighResolutionTime):TpvInt64;
       function FromMicroseconds(const aTime:TpvInt64):TpvApplicationHighResolutionTime;
       function ToNanoseconds(const aTime:TpvApplicationHighResolutionTime):TpvInt64;
       function FromNanoseconds(const aTime:TpvInt64):TpvApplicationHighResolutionTime;
       property Frequency:TpvInt64 read fFrequency;
       property MillisecondInterval:TpvApplicationHighResolutionTime read fMillisecondInterval;
       property TwoMillisecondsInterval:TpvApplicationHighResolutionTime read fTwoMillisecondsInterval;
       property FourMillisecondsInterval:TpvApplicationHighResolutionTime read fFourMillisecondsInterval;
       property QuarterSecondInterval:TpvApplicationHighResolutionTime read fQuarterSecondInterval;
       property SecondInterval:TpvApplicationHighResolutionTime read fFrequency;
       property MinuteInterval:TpvApplicationHighResolutionTime read fMinuteInterval;
       property HourInterval:TpvApplicationHighResolutionTime read fHourInterval;
     end;

     PpvApplicationInputKeyEventType=^TpvApplicationInputKeyEventType;
     TpvApplicationInputKeyEventType=
      (
       DOWN,
       UP,
       TYPED,
       UNICODE
      );

     PpvApplicationInputKeyModifier=^TpvApplicationInputKeyModifier;
     TpvApplicationInputKeyModifier=
      (
       LSHIFT,
       RSHIFT,
       LCTRL,
       RCTRL,
       LALT,
       RALT,
       LMETA,
       RMETA,
       NUM,
       CAPS,
       MODE,
       RESERVED,
       CTRL,
       SHIFT,
       ALT,
       META
      );

     PpvApplicationInputKeyModifiers=^TpvApplicationInputKeyModifiers;
     TpvApplicationInputKeyModifiers=set of TpvApplicationInputKeyModifier;

     PpvApplicationInputKeyEvent=^TpvApplicationInputKeyEventType;
     TpvApplicationInputKeyEvent=record
      public
       KeyEventType:TpvApplicationInputKeyEventType;
       KeyCode:TpvInt32;
       KeyModifiers:TpvApplicationInputKeyModifiers;
       constructor Create(const aKeyEventType:TpvApplicationInputKeyEventType;
                          const aKeyCode:TpvInt32;
                          const aKeyModifiers:TpvApplicationInputKeyModifiers);
     end;

     PpvApplicationInputPointerEventType=^TpvApplicationInputPointerEventType;
     TpvApplicationInputPointerEventType=
      (
       DOWN,
       UP,
       MOTION,
       DRAG
      );

     PpvApplicationInputPointerButton=^TpvApplicationInputPointerButton;
     TpvApplicationInputPointerButton=
      (
       LEFT,
       MIDDLE,
       RIGHT
      );

     PpvApplicationInputPointerButtons=^TpvApplicationInputPointerButtons;
     TpvApplicationInputPointerButtons=set of TpvApplicationInputPointerButton;

     PpvApplicationInputPointerEvent=^TpvApplicationInputPointerEventType;
     TpvApplicationInputPointerEvent=record
      public
       PointerEventType:TpvApplicationInputPointerEventType;
       Position:TpvVector2;
       RelativePosition:TpvVector2;
       Pressure:TpvFloat;
       PointerID:TpvInt32;
       Button:TpvApplicationInputPointerButton;
       Buttons:TpvApplicationInputPointerButtons;
       KeyModifiers:TpvApplicationInputKeyModifiers;
       constructor Create(const aPointerEventType:TpvApplicationInputPointerEventType;
                          const aPosition:TpvVector2;
                          const aPressure:TpvFloat;
                          const aPointerID:TpvInt32;
                          const aButton:TpvApplicationInputPointerButton;
                          const aButtons:TpvApplicationInputPointerButtons;
                          const aKeyModifiers:TpvApplicationInputKeyModifiers); overload;
       constructor Create(const aPointerEventType:TpvApplicationInputPointerEventType;
                          const aPosition:TpvVector2;
                          const aRelativePosition:TpvVector2;
                          const aPressure:TpvFloat;
                          const aPointerID:TpvInt32;
                          const aButtons:TpvApplicationInputPointerButtons;
                          const aKeyModifiers:TpvApplicationInputKeyModifiers); overload;
     end;

     TpvApplicationInputProcessor=class
      public
       constructor Create; virtual;
       destructor Destroy; override;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; virtual;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; virtual;
       function Scrolled(const aRelativeAmount:TpvVector2):boolean; virtual;
     end;

     PpvApplicationInputProcessorQueueEvent=^TpvApplicationInputProcessorQueueEvent;
     TpvApplicationInputProcessorQueueEvent=record
      Next:PpvApplicationInputProcessorQueueEvent;
      Time:TpvInt64;
      case Event:TpvInt32 of
       EVENT_KEY:(
        KeyEvent:TpvApplicationInputKeyEvent;
       );
       EVENT_POINTER:(
        PointerEvent:TpvApplicationInputPointerEvent;
       );
       EVENT_SCROLLED:(
        RelativeAmount:TpvVector2;
       );
     end;

     TpvApplicationInputProcessorQueue=class(TpvApplicationInputProcessor)
      private
       fProcessor:TpvApplicationInputProcessor;
       fCriticalSection:TPasMPCriticalSection;
       fQueuedEvents:PpvApplicationInputProcessorQueueEvent;
       fLastQueuedEvent:PpvApplicationInputProcessorQueueEvent;
       fFreeEvents:PpvApplicationInputProcessorQueueEvent;
       fCurrentEventTime:TpvInt64;
       function NewEvent:PpvApplicationInputProcessorQueueEvent;
       procedure FreeEvent(const aEvent:PpvApplicationInputProcessorQueueEvent);
       procedure PushEvent(const aEvent:PpvApplicationInputProcessorQueueEvent);
      public
       constructor Create; override;
       destructor Destroy; override;
       procedure SetProcessor(aProcessor:TpvApplicationInputProcessor);
       function GetProcessor:TpvApplicationInputProcessor;
       procedure Drain;
       function GetCurrentEventTime:TpvInt64;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;
       function Scrolled(const aRelativeAmount:TpvVector2):boolean; override;
     end;

     TpvApplicationInputMultiplexer=class(TpvApplicationInputProcessor)
      private
       fProcessors:TList;
      public
       constructor Create; override;
       destructor Destroy; override;
       procedure AddProcessor(const aProcessor:TpvApplicationInputProcessor);
       procedure AddProcessors(const aProcessors:array of TpvApplicationInputProcessor);
       procedure InsertProcessor(const aIndex:TpvInt32;const aProcessor:TpvApplicationInputProcessor);
       procedure RemoveProcessor(const aProcessor:TpvApplicationInputProcessor); overload;
       procedure RemoveProcessor(const aIndex:TpvInt32); overload;
       procedure ClearProcessors;
       function CountProcessors:TpvInt32;
       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;
       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;
       function Scrolled(const aRelativeAmount:TpvVector2):boolean; override;
     end;

     TpvApplicationInputTextInputCallback=procedure(aSuccessful:boolean;const aText:TpvApplicationRawByteString) of object;

     TpvApplicationInput=class;

     TpvApplicationJoystick=class
      private
       fIndex:TpvInt32;
       fID:TpvInt32;
{$if defined(PasVulkanUseSDL2)}
       fJoystick:PSDL_Joystick;
       fGameController:PSDL_GameController;
{$ifend}
       fCountAxes:TpvInt32;
       fCountBalls:TpvInt32;
       fCountHats:TpvInt32;
       fCountButtons:TpvInt32;
       procedure Initialize;
      public
{$if defined(PasVulkanUseSDL2)}
       constructor Create(const aIndex:TpvInt32;const aJoystick:PSDL_Joystick;const aGameController:PSDL_GameController); reintroduce;
{$else}
       constructor Create(const aIndex:TpvInt32); reintroduce;
{$ifend}
       destructor Destroy; override;
       function IsGameController:boolean;
       function Index:TpvInt32;
       function ID:TpvInt32;
       function Name:TpvApplicationRawByteString;
       function GUID:TGUID;
       function DeviceGUID:TGUID;
       function CountAxes:TpvInt32;
       function CountBalls:TpvInt32;
       function CountHats:TpvInt32;
       function CountButtons:TpvInt32;
       procedure Update;
       function GetAxis(const aAxisIndex:TpvInt32):TpvInt32;
       function GetBall(const aBallIndex:TpvInt32;out aDeltaX,aDeltaY:TpvInt32):boolean;
       function GetHat(const aHatIndex:TpvInt32):TpvInt32;
       function GetButton(const aButtonIndex:TpvInt32):boolean;
       function IsGameControllerAttached:boolean;
       function GetGameControllerAxis(const aAxis:TpvInt32):TpvInt32;
       function GetGameControllerButton(const aButton:TpvInt32):boolean;
       function GetGameControllerName:TpvApplicationRawByteString;
       function GetGameControllerMapping:TpvApplicationRawByteString;
     end;

     TpvApplicationEvent=record
{$if defined(PasVulkanUseSDL2)}
      SDLEvent:TSDL_Event;
{$else}
      Dummy:TpvUInt32;
{$ifend}
     end;

     TpvApplicationInput=class
      private
       fVulkanApplication:TpvApplication;
       fKeyCodeNames:array[-1..1023] of TpvApplicationRawByteString;
       fCriticalSection:TPasMPCriticalSection;
       fProcessor:TpvApplicationInputProcessor;
       fEvents:array of TpvApplicationEvent;
       fEventTimes:array of int64;
       fEventCount:TpvInt32;
       fCurrentEventTime:int64;
       fKeyDown:array[0..$ffff] of boolean;
       fKeyDownCount:TpvInt32;
       fJustKeyDown:array[0..$ffff] of boolean;
       fPointerX:array[0..$ffff] of TpvFloat;
       fPointerY:array[0..$ffff] of TpvFloat;
       fPointerDown:array[0..$ffff] of TpvApplicationInputPointerButtons;
       fPointerJustDown:array[0..$ffff] of TpvApplicationInputPointerButtons;
       fPointerPressure:array[0..$ffff] of TpvFloat;
       fPointerDeltaX:array[0..$ffff] of TpvFloat;
       fPointerDeltaY:array[0..$ffff] of TpvFloat;
       fPointerDownCount:TpvInt32;
       fMouseX:TpvInt32;
       fMouseY:TpvInt32;
       fMouseDown:TpvApplicationInputPointerButtons;
       fMouseJustDown:TpvApplicationInputPointerButtons;
       fMouseDeltaX:TpvInt32;
       fMouseDeltaY:TpvInt32;
       fJustTouched:longbool;
       fMaxPointerID:TpvInt32;
       fJoysticks:TList;
       fMainJoystick:TpvApplicationJoystick;
{$if defined(PasVulkanUseSDL2)}
       function TranslateSDLKeyCode(const aKeyCode,aScanCode:TpvInt32):TpvInt32;
       function TranslateSDLKeyModifier(const aKeyModifier:TpvInt32):TpvApplicationInputKeyModifiers;
{$else}
{$ifend}
       procedure AddEvent(const aEvent:TpvApplicationEvent);
       procedure ProcessEvents;
      public
       constructor Create(const aVulkanApplication:TpvApplication); reintroduce;
       destructor Destroy; override;
       function GetAccelerometerX:TpvFloat;
       function GetAccelerometerY:TpvFloat;
       function GetAccelerometerZ:TpvFloat;
       function GetOrientationAzimuth:TpvFloat;
       function GetOrientationPitch:TpvFloat;
       function GetOrientationRoll:TpvFloat;
       function GetMaxPointerID:TpvInt32;
       function GetPointerX(const aPointerID:TpvInt32=0):TpvFloat;
       function GetPointerDeltaX(const aPointerID:TpvInt32=0):TpvFloat;
       function GetPointerY(const aPointerID:TpvInt32=0):TpvFloat;
       function GetPointerDeltaY(const aPointerID:TpvInt32=0):TpvFloat;
       function GetPointerPressure(const aPointerID:TpvInt32=0):TpvFloat;
       function IsPointerTouched(const aPointerID:TpvInt32=0;const aButtonMask:TpvApplicationInputPointerButtons=[TpvApplicationInputPointerButton.LEFT,TpvApplicationInputPointerButton.MIDDLE,TpvApplicationInputPointerButton.RIGHT]):boolean;
       function IsPointerJustTouched(const aPointerID:TpvInt32=0;const aButtonMask:TpvApplicationInputPointerButtons=[TpvApplicationInputPointerButton.LEFT,TpvApplicationInputPointerButton.MIDDLE,TpvApplicationInputPointerButton.RIGHT]):boolean;
       function IsTouched:boolean;
       function JustTouched:boolean;
       function IsButtonPressed(const aButton:TpvApplicationInputPointerButton):boolean;
       function IsKeyPressed(const aKeyCode:TpvInt32):boolean;
       function IsKeyJustPressed(const aKeyCode:TpvInt32):boolean;
       function GetKeyName(const aKeyCode:TpvInt32):TpvApplicationRawByteString;
       function GetKeyModifiers:TpvApplicationInputKeyModifiers;
       procedure StartTextInput;
       procedure StopTextInput;
       procedure GetTextInput(const aCallback:TpvApplicationInputTextInputCallback;const aTitle,aText:TpvApplicationRawByteString;const aPlaceholder:TpvApplicationRawByteString='');
       procedure SetOnscreenKeyboardVisible(const aVisible:boolean);
       procedure Vibrate(const aMilliseconds:TpvInt32); overload;
       procedure Vibrate(const aPattern:array of TpvInt32;const aRepeats:TpvInt32); overload;
       procedure CancelVibrate;
       procedure GetRotationMatrix(const aMatrix3x3:pointer);
       function GetCurrentEventTime:TpvInt64;
       procedure SetCatchBackKey(const aCatchBack:boolean);
       procedure SetCatchMenuKey(const aCatchMenu:boolean);
       procedure SetInputProcessor(const aProcessor:TpvApplicationInputProcessor);
       function GetInputProcessor:TpvApplicationInputProcessor;
       function IsPeripheralAvailable(const aPeripheral:TpvInt32):boolean;
       function GetNativeOrientation:TpvInt32;
       procedure SetCursorCatched(const aCatched:boolean);
       function IsCursorCatched:boolean;
       procedure SetCursorPosition(const pX,pY:TpvInt32);
       function GetJoystickCount:TpvInt32;
       function GetJoystick(const aIndex:TpvInt32=-1):TpvApplicationJoystick;
     end;

     TpvApplicationLifecycleListener=class
      public
       constructor Create; reintroduce; virtual;
       destructor Destroy; override;
       function Resume:boolean; virtual;
       function Pause:boolean; virtual;
       function LowMemory:boolean; virtual;
       function Terminate:boolean; virtual;
     end;

     TpvApplicationScreen=class
      public

       constructor Create; virtual;

       destructor Destroy; override;

       procedure Show; virtual;

       procedure Hide; virtual;

       procedure Resume; virtual;

       procedure Pause; virtual;

       procedure LowMemory; virtual;

       procedure Resize(const aWidth,aHeight:TpvInt32); virtual;

       procedure AfterCreateSwapChain; virtual;

       procedure BeforeDestroySwapChain; virtual;

       function HandleEvent(const aEvent:TpvApplicationEvent):boolean; virtual;

       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; virtual;

       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; virtual;

       function Scrolled(const aRelativeAmount:TpvVector2):boolean; virtual;

       function CanBeParallelProcessed:boolean; virtual;

       procedure Update(const aDeltaTime:TpvDouble); virtual;

       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); virtual;

       procedure UpdateAudio; virtual;

     end;

     TpvApplicationScreenClass=class of TpvApplicationScreen;

     TpvApplicationAssets=class
      private
       fVulkanApplication:TpvApplication;
{$ifndef Android}
       fBasePath:string;
{$endif}
       function CorrectFileName(const aFileName:string):string;
      public
       constructor Create(const aVulkanApplication:TpvApplication);
       destructor Destroy; override;
       function ExistAsset(const aFileName:string):boolean;
       function GetAssetStream(const aFileName:string):TStream;
       function GetAssetSize(const aFileName:string):int64;
     end;

     TpvApplicationFiles=class
      private
       fVulkanApplication:TpvApplication;
      public
       constructor Create(const aVulkanApplication:TpvApplication);
       destructor Destroy; override;
       function GetCacheStoragePath:string;
       function GetLocalStoragePath:string;
       function GetRoamingStoragePath:string;
       function GetExternalStoragePath:string;
       function IsCacheStorageAvailable:boolean;
       function IsLocalStorageAvailable:boolean;
       function IsRoamingStorageAvailable:boolean;
       function IsExternalStorageAvailable:boolean;
     end;

     TpvApplicationClipboard=class
      private
       fVulkanApplication:TpvApplication;
      public
       constructor Create(const aVulkanApplication:TpvApplication);
       destructor Destroy; override;
       function HasText:boolean;
       function GetText:TpvApplicationUTF8String;
       procedure SetText(const aTextString:TpvApplicationUTF8String);
     end;

     TpvApplicationCommandPools=array of array of TpvVulkanCommandPool;

     TpvApplicationCommandBuffers=array of array of TpvVulkanCommandBuffer;

     TpvApplicationCommandBufferFences=array of array of TpvVulkanFence;

     TpvApplicationOnEvent=function(const aVulkanApplication:TpvApplication;const aEvent:TpvApplicationEvent):boolean of object;

     PpvApplicationPresentMode=^TpvApplicationPresentMode;
     TpvApplicationPresentMode=
      (
       Immediate=0,
       Mailbox=1,
       FIFO=2,
       FIFORelaxed=3,
       NoVSync={$ifdef fpc}0{$else}TpvApplicationPresentMode.Immediate{$endif},
       GreedyVSync={$ifdef fpc}1{$else}TpvApplicationPresentMode.Mailbox{$endif},
       Vsync={$ifdef fpc}2{$else}TpvApplicationPresentMode.FIFO{$endif}
      );

     TpvApplication=class
      private
       type //PpvApplicationVulkanRecreationKind=^TpvApplicationVulkanRecreationKind;
            TpvApplicationVulkanRecreationKind=
             (
              None,
              SwapChain,
              Surface,
              Device
             );
       const PresentModeToVulkanPresentMode:array[TpvApplicationPresentMode.Immediate..TpvApplicationPresentMode.FIFORelaxed] of TVkPresentModeKHR=
              (
               VK_PRESENT_MODE_IMMEDIATE_KHR,
               VK_PRESENT_MODE_MAILBOX_KHR,
               VK_PRESENT_MODE_FIFO_KHR,
               VK_PRESENT_MODE_FIFO_RELAXED_KHR
              );
      private

       fTitle:string;
       fVersion:TpvUInt32;

       fPathName:string;

       fCacheStoragePath:string;

       fLocalStoragePath:string;

       fRoamingStoragePath:string;

       fExternalStoragePath:string;

       fPasMPInstance:TPasMP;

       fDoDestroyGlobalPasMPInstance:TPasMPBool32;

       fHighResolutionTimer:TpvApplicationHighResolutionTimer;

       fAssets:TpvApplicationAssets;

       fFiles:TpvApplicationFiles;

       fInput:TpvApplicationInput;

       fClipboard:TpvApplicationClipboard;

       fAudio:TpvAudio;

       fRunnableList:TpvApplicationRunnableList;
       fRunnableListCount:TpvInt32;
       fRunnableListCriticalSection:TPasMPCriticalSection;

       fLifecycleListenerList:TList;
       fLifecycleListenerListCriticalSection:TPasMPCriticalSection;

       fCurrentWidth:TpvInt32;
       fCurrentHeight:TpvInt32;
       fCurrentFullscreen:TpvInt32;
       fCurrentPresentMode:TpvInt32;
       fCurrentVisibleMouseCursor:TpvInt32;
       fCurrentCatchMouse:TpvInt32;
       fCurrentHideSystemBars:TpvInt32;
       fCurrentBlocking:TpvInt32;

       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fFullscreen:boolean;
       fPresentMode:TpvApplicationPresentMode;
       fResizable:boolean;
       fVisibleMouseCursor:boolean;
       fCatchMouse:boolean;
       fHideSystemBars:boolean;
       fAndroidSeparateMouseAndTouch:boolean;
       fUseAudio:boolean;
       fBlocking:boolean;

{$if defined(PasVulkanUseSDL2)}
       fSDLVersion:TSDL_Version;

{$if defined(PasVulkanUseSDL2WithVulkanSupport)}
       fSDLVersionWithVulkanSupport:boolean;
{$ifend}
{$ifend}

       fDebugging:boolean;

       fActive:boolean;

       fTerminated:boolean;

{$if defined(fpc) and defined(android) and not defined(PasVulkanUseSDL2)}
       fAndroidApp:TpvPointer;
       fAndroidWindow:PANativeWindow;
       fAndroidReady:TPasMPBool32;
       fAndroidQuit:TPasMPBool32;
       fAndroidAppProcessMessages:procedure(const aAndroidApp:TpvPointer;const aWait:boolean);
{$ifend}

{$if defined(PasVulkanUseSDL2)}
       fSDLWaveFormat:TSDL_AudioSpec;

       fSDLDisplayMode:TSDL_DisplayMode;
       fSurfaceWindow:PSDL_Window;
{$ifend}

       fEvent:TpvApplicationEvent;

       fLastPressedKeyEvent:TpvApplicationEvent;
       fKeyRepeatTimeAccumulator:TpvApplicationHighResolutionTime;
       fKeyRepeatInterval:TpvApplicationHighResolutionTime;
       fKeyRepeatInitialInterval:TpvApplicationHighResolutionTime;
       fNativeKeyRepeat:boolean;

       fScreenWidth:TpvInt32;
       fScreenHeight:TpvInt32;

{$if defined(PasVulkanUseSDL2)}
       fVideoFlags:TSDLUInt32;
{$ifend}

       fGraphicsReady:boolean;

       fVulkanDebugging:boolean;

       fVulkanValidation:boolean;

       fVulkanDebuggingEnabled:boolean;

       fVulkanInstance:TpvVulkanInstance;

       fVulkanDevice:TpvVulkanDevice;

       fVulkanPipelineCache:TpvVulkanPipelineCache;

       fVulkanPipelineCacheFileName:string;
       
       fCountCPUThreads:TpvInt32;

       fAvailableCPUCores:TPasMPAvailableCPUCores;

       fVulkanCountCommandQueues:TpvInt32;

       fVulkanCommandPools:array of TpvApplicationCommandPools;
       fVulkanCommandBuffers:array of TpvApplicationCommandBuffers;
       fVulkanCommandBufferFences:array of TpvApplicationCommandBufferFences;

       fVulkanUniversalCommandPools:TpvApplicationCommandPools;
       fVulkanUniversalCommandBuffers:TpvApplicationCommandBuffers;
       fVulkanUniversalCommandBufferFences:TpvApplicationCommandBufferFences;

       fVulkanPresentCommandPools:TpvApplicationCommandPools;
       fVulkanPresentCommandBuffers:TpvApplicationCommandBuffers;
       fVulkanPresentCommandBufferFences:TpvApplicationCommandBufferFences;

       fVulkanGraphicsCommandPools:TpvApplicationCommandPools;
       fVulkanGraphicsCommandBuffers:TpvApplicationCommandBuffers;
       fVulkanGraphicsCommandBufferFences:TpvApplicationCommandBufferFences;

       fVulkanComputeCommandPools:TpvApplicationCommandPools;
       fVulkanComputeCommandBuffers:TpvApplicationCommandBuffers;
       fVulkanComputeCommandBufferFences:TpvApplicationCommandBufferFences;

       fVulkanTransferCommandPools:TpvApplicationCommandPools;
       fVulkanTransferCommandBuffers:TpvApplicationCommandBuffers;
       fVulkanTransferCommandBufferFences:TpvApplicationCommandBufferFences;

       fVulkanSurface:TpvVulkanSurface;

       fGraphicsPipelinesReady:boolean;

       fSkipNextDrawFrame:boolean;

//     fVulkanPresentationSurface:TpvVulkanPresentationSurface;

       fOnEvent:TpvApplicationOnEvent;

       fOnStep:TpvApplicationOnStep;

       fScreen:TpvApplicationScreen;

       fStartScreen:TpvApplicationScreenClass;

       fNextScreen:TpvApplicationScreen;

       fNextScreenClass:TpvApplicationScreenClass;

       fHasNewNextScreen:boolean;

       fHasLastTime:boolean;

       fLastTime:TpvApplicationHighResolutionTime;
       fNowTime:TpvApplicationHighResolutionTime;
       fDeltaTime:TpvApplicationHighResolutionTime;
       fFloatDeltaTime:TpvDouble;

       fFrameTimesHistoryDeltaTimes:array[0..FrameTimesHistorySize-1] of TpvDouble;
       fFrameTimesHistoryTimePoints:array[0..FrameTimesHistorySize-1] of TpvApplicationHighResolutionTime;
       fFrameTimesHistoryReadIndex:TPasMPInt32;
       fFrameTimesHistoryWriteIndex:TPasMPInt32;

       fFramesPerSecond:TpvDouble;

       fFrameCounter:TpvInt64;

       fUpdateFrameCounter:TpvInt64;
       
       fDrawFrameCounter:TpvInt64;

       fCountSwapChainImages:TpvInt32;

       fDesiredCountSwapChainImages:TpvInt32;

       fUpdateSwapChainImageIndex:TpvInt32;

       fDrawSwapChainImageIndex:TpvInt32;

       fRealUsedDrawSwapChainImageIndex:TpvInt32;

       fVulkanRecreationKind:TpvApplicationVulkanRecreationKind;

       fVulkanWaitSemaphore:TpvVulkanSemaphore;

       fVulkanWaitFence:TpvVulkanFence;

       fVulkanSwapChain:TpvVulkanSwapChain;

       fVulkanOldSwapChain:TpvVulkanSwapChain;

       fVulkanTransferInflightCommandsFromOldSwapChain:boolean;

       fVulkanWaitFences:array[0..MaxSwapChainImages-1] of TpvVulkanFence;

       fVulkanWaitFencesReady:array[0..MaxSwapChainImages-1] of boolean;

       fVulkanPresentCompleteSemaphores:array[0..MaxSwapChainImages-1] of TpvVulkanSemaphore;

       fVulkanPresentCompleteFences:array[0..MaxSwapChainImages-1] of TpvVulkanFence;

       fVulkanPresentCompleteFencesReady:array[0..MaxSwapChainImages-1] of boolean;

       fVulkanDepthImageFormat:TVkFormat;

       fVulkanDepthFrameBufferAttachment:TpvVulkanFrameBufferAttachment;

       fVulkanFrameBufferColorAttachments:TpvVulkanFrameBufferAttachments;

       fVulkanRenderPass:TpvVulkanRenderPass;

       fVulkanFrameBuffers:TpvVulkanSwapChainSimpleDirectRenderTargetFrameBuffers;

       fVulkanCommandPool:TpvVulkanCommandPool;

       fVulkanBlankCommandBuffers:array[0..MaxSwapChainImages-1] of TpvVulkanCommandBuffer;

       fVulkanBlankCommandBufferSemaphores:array[0..MaxSwapChainImages-1] of TpvVulkanSemaphore;

       fVulkanPresentToDrawImageBarrierCommandBuffers:array[0..MaxSwapChainImages-1] of TpvVulkanCommandBuffer;

       fVulkanPresentToDrawImageBarrierCommandBufferSemaphores:array[0..MaxSwapChainImages-1] of TpvVulkanSemaphore;

       fVulkanDrawToPresentImageBarrierCommandBuffers:array[0..MaxSwapChainImages-1] of TpvVulkanCommandBuffer;

       fVulkanDrawToPresentImageBarrierCommandBufferSemaphores:array[0..MaxSwapChainImages-1] of TpvVulkanSemaphore;

       procedure SetDesiredCountSwapChainImages(const aDesiredCountSwapChainImages:TpvInt32);

       procedure InitializeGraphics;
       procedure DeinitializeGraphics;

       procedure InitializeAudio;
       procedure DeinitializeAudio;

      protected

       procedure VulkanDebugLn(const What:string);

       function VulkanOnDebugReportCallback(const aFlags:TVkDebugReportFlagsEXT;const aObjectType:TVkDebugReportObjectTypeEXT;const aObject:TpvUInt64;const aLocation:TVkSize;aMessageCode:TpvInt32;const aLayerPrefix,aMessage:string):TVkBool32;

       procedure VulkanWaitIdle;

       procedure CreateVulkanDevice(const aSurface:TpvVulkanSurface=nil);

       procedure CreateVulkanInstance;
       procedure DestroyVulkanInstance;

       procedure CreateVulkanSurface;
       procedure DestroyVulkanSurface;

       procedure CreateVulkanSwapChain;
       procedure DestroyVulkanSwapChain;

       procedure CreateVulkanRenderPass;
       procedure DestroyVulkanRenderPass;

       procedure CreateVulkanFrameBuffers;
       procedure DestroyVulkanFrameBuffers;

       procedure CreateVulkanCommandBuffers;
       procedure DestroyVulkanCommandBuffers;

       function AcquireVulkanBackBuffer:boolean;
       function PresentVulkanBackBuffer:boolean;

       procedure SetScreen(const aScreen:TpvApplicationScreen);
       procedure SetNextScreen(const aNextScreen:TpvApplicationScreen);
       procedure SetNextScreenClass(const aNextScreenClass:TpvApplicationScreenClass);

       procedure UpdateFrameTimesHistory;

       procedure UpdateJobFunction(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32);
       procedure DrawJobFunction(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32);

       procedure UpdateAudioHook;

       function IsVisibleToUser:boolean;

       function WaitForReadyState:boolean;

      public

       constructor Create; reintroduce; virtual;
       destructor Destroy; override;

       class procedure Log(const aLevel:TpvInt32;const aWhere,aWhat:string); static;

       procedure ReadConfig; virtual;
       procedure SaveConfig; virtual;

       procedure PostRunnable(const aRunnable:TpvApplicationRunnable);

       procedure AddLifecycleListener(const aLifecycleListener:TpvApplicationLifecycleListener);
       procedure RemoveLifecycleListener(const aLifecycleListener:TpvApplicationLifecycleListener);

       procedure Initialize;

       procedure Terminate;

       procedure ProcessRunnables;

       procedure ProcessMessages;

       procedure Run;

       procedure Setup; virtual;

       procedure Start; virtual;

       procedure Stop; virtual;

       procedure Load; virtual;

       procedure Unload; virtual;

       procedure Resume; virtual;

       procedure Pause; virtual;

       procedure LowMemory; virtual;

       procedure Resize(const aWidth,aHeight:TpvInt32); virtual;

       procedure AfterCreateSwapChain; virtual;

       procedure BeforeDestroySwapChain; virtual;

       function HandleEvent(const aEvent:TpvApplicationEvent):boolean; virtual;

       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; virtual;

       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; virtual;

       function Scrolled(const aRelativeAmount:TpvVector2):boolean; virtual;

       function CanBeParallelProcessed:boolean; virtual;

       procedure Update(const aDeltaTime:TpvDouble); virtual;

       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); virtual;

       procedure UpdateAudio; virtual;

       class procedure Main; virtual;

       property VulkanFrameBuffers:TpvVulkanSwapChainSimpleDirectRenderTargetFrameBuffers read fVulkanFrameBuffers;

      published

       property PasMPInstance:TPasMP read fPasMPInstance;

       property HighResolutionTimer:TpvApplicationHighResolutionTimer read fHighResolutionTimer;

       property Assets:TpvApplicationAssets read fAssets;

       property Files:TpvApplicationFiles read fFiles;

       property Input:TpvApplicationInput read fInput;

       property Clipboard:TpvApplicationClipboard read fClipboard;

       property Audio:TpvAudio read fAudio;

       property Title:string read fTitle write fTitle;
       property Version:TpvUInt32 read fVersion write fVersion;

       property PathName:string read fPathName write fPathName;

       property Width:TpvInt32 read fWidth write fWidth;
       property Height:TpvInt32 read fHeight write fHeight;

       property Fullscreen:boolean read fFullscreen write fFullscreen;

       property PresentMode:TpvApplicationPresentMode read fPresentMode write fPresentMode;

       property Resizable:boolean read fResizable write fResizable;

       property VisibleMouseCursor:boolean read fVisibleMouseCursor write fVisibleMouseCursor;

       property CatchMouse:boolean read fCatchMouse write fCatchMouse;

       property HideSystemBars:boolean read fHideSystemBars write fHideSystemBars;

       property AndroidSeparateMouseAndTouch:boolean read fAndroidSeparateMouseAndTouch write fAndroidSeparateMouseAndTouch;

       property UseAudio:boolean read fUseAudio write fUseAudio;

       property Blocking:boolean read fBlocking write fBlocking;

       property Debugging:boolean read fDebugging;
       
       property Active:boolean read fActive;

       property Terminated:boolean read fTerminated;

       property CountCPUThreads:TpvInt32 read fCountCPUThreads;

       property OnEvent:TpvApplicationOnEvent read fOnEvent write fOnEvent;
       property OnStep:TpvApplicationOnStep read fOnStep write fOnStep;

       property VulkanDebugging:boolean read fVulkanDebugging write fVulkanDebugging;

       property VulkanValidation:boolean read fVulkanValidation write fVulkanValidation;

       property VulkanDebuggingEnabled:boolean read fVulkanDebuggingEnabled;

       property VulkanInstance:TpvVulkanInstance read fVulkanInstance;

       property VulkanDevice:TpvVulkanDevice read fVulkanDevice;

       property VulkanPipelineCache:TpvVulkanPipelineCache read fVulkanPipelineCache;

       property VulkanPipelineCacheFileName:string read fVulkanPipelineCacheFileName write fVulkanPipelineCacheFileName; 

       property VulkanUniversalCommandPools:TpvApplicationCommandPools read fVulkanUniversalCommandPools;
       property VulkanUniversalCommandBuffers:TpvApplicationCommandBuffers read fVulkanUniversalCommandBuffers;
       property VulkamUniversalCommandBufferFences:TpvApplicationCommandBufferFences read fVulkanUniversalCommandBufferFences;

       property VulkanPresentCommandPools:TpvApplicationCommandPools read fVulkanPresentCommandPools;
       property VulkanPresentCommandBuffers:TpvApplicationCommandBuffers read fVulkanPresentCommandBuffers;
       property VulkanPresentCommandBufferFences:TpvApplicationCommandBufferFences read fVulkanPresentCommandBufferFences;

       property VulkanGraphicsCommandPools:TpvApplicationCommandPools read fVulkanGraphicsCommandPools;
       property VulkanGraphicsCommandBuffers:TpvApplicationCommandBuffers read fVulkanGraphicsCommandBuffers;
       property VulkanGraphicsCommandBufferFences:TpvApplicationCommandBufferFences read fVulkanGraphicsCommandBufferFences;

       property VulkanComputeCommandPools:TpvApplicationCommandPools read fVulkanComputeCommandPools;
       property VulkanComputeCommandBuffers:TpvApplicationCommandBuffers read fVulkanComputeCommandBuffers;
       property VulkanComputeCommandBufferFences:TpvApplicationCommandBufferFences read fVulkanComputeCommandBufferFences;

       property VulkanTransferCommandPools:TpvApplicationCommandPools read fVulkanTransferCommandPools;
       property VulkanTransferCommandBuffers:TpvApplicationCommandBuffers read fVulkanTransferCommandBuffers;
       property VulkanTransferCommandBufferFences:TpvApplicationCommandBufferFences read fVulkanTransferCommandBufferFences;

       property VulkanSwapChain:TpvVulkanSwapChain read fVulkanSwapChain;

       property VulkanTransferInflightCommandsFromOldSwapChain:boolean read fVulkanTransferInflightCommandsFromOldSwapChain write fVulkanTransferInflightCommandsFromOldSwapChain;

       property VulkanDepthImageFormat:TVkFormat read fVulkanDepthImageFormat;

       property VulkanRenderPass:TpvVulkanRenderPass read fVulkanRenderPass;

       property StartScreen:TpvApplicationScreenClass read fStartScreen write fStartScreen;

       property Screen:TpvApplicationScreen read fScreen write SetScreen;

       property NextScreen:TpvApplicationScreen read fNextScreen write SetNextScreen;

       property NextScreenClass:TpvApplicationScreenClass read fNextScreenClass write SetNextScreenClass;

       property DeltaTime:TpvDouble read fFloatDeltaTime;

       property FramesPerSecond:TpvDouble read fFramesPerSecond;

       property FrameCounter:TpvInt64 read fFrameCounter;

       property UpdateFrameCounter:TpvInt64 read fUpdateFrameCounter;

       property DrawFrameCounter:TpvInt64 read fDrawFrameCounter;

       property DesiredCountSwapChainImages:TpvInt32 read fDesiredCountSwapChainImages write SetDesiredCountSwapChainImages;

       property CountSwapChainImages:TpvInt32 read fCountSwapChainImages;

       property UpdateSwapChainImageIndex:TpvInt32 read fUpdateSwapChainImageIndex;

       property DrawSwapChainImageIndex:TpvInt32 read fDrawSwapChainImageIndex;

       property RealUsedDrawSwapChainImageIndex:TpvInt32 read fRealUsedDrawSwapChainImageIndex;

     end;

var pvApplication:TpvApplication=nil;

{$if defined(fpc) and defined(android)}
     AndroidJavaVM:PJavaVM=nil;
     AndroidJavaEnv:PJNIEnv=nil;
     AndroidJavaClass:jclass=nil;
     AndroidJavaObject:jobject=nil;

     AndroidActivity:PANativeActivity=nil;

     AndroidSavedState:TpvPointer=nil;
     AndroidSavedStateSize:TpvSizeUInt=0;

     AndroidAssetManager:PAAssetManager=nil;

     AndroidInternalDataPath:string='';
     AndroidExternalDataPath:string='';
     AndroidLibraryPath:string='';

     AndroidDeviceName:TpvUTF8String='';

function AndroidGetManufacturerName:TpvApplicationUnicodeString;
function AndroidGetModelName:TpvApplicationUnicodeString;
function AndroidGetDeviceName:TpvApplicationUnicodeString;
function Android_JNI_GetEnv:PJNIEnv; cdecl;

{$if not defined(PasVulkanUseSDL2)}
procedure Android_ANativeActivity_onCreate(aActivity:PANativeActivity;aSavedState:pointer;aSavedStateSize:cuint32;const aApplicationClass:TpvApplicationClass);
{$ifend}

{$ifend}

implementation

const BoolToInt:array[boolean] of TpvInt32=(0,1);

      BoolToLongBool:array[boolean] of longbool=(false,true);

{$if defined(fpc) and defined(Windows)}
function IsDebuggerPresent:longbool; stdcall; external 'kernel32.dll' name 'IsDebuggerPresent';
{$ifend}

{$if defined(Unix)}
procedure signal_handler(aSignal:cint); cdecl;
begin
 case aSignal of
  SIGINT,SIGTERM,SIGKILL:begin
   if assigned(pvApplication) then begin
    pvApplication.Terminate;
   end;
  end;
 end;
end;

procedure InstallSignalHandlers;
begin
 fpsignal(SIGTERM,signal_handler);
 fpsignal(SIGINT,signal_handler);
 fpsignal(SIGHUP,signalhandler(SIG_IGN));
 fpsignal(SIGCHLD,signalhandler(SIG_IGN));
 fpsignal(SIGPIPE,signalhandler(SIG_IGN));
 fpsignal(SIGALRM,signalhandler(SIG_IGN));
 fpsignal(SIGWINCH,signalhandler(SIG_IGN));
end;
{$ifend}

{$ifdef unix}

function GetAppDataCacheStoragePath(Postfix:TpvApplicationRawByteString):TpvApplicationRawByteString;
{$ifdef darwin}
var TruePath:TpvApplicationRawByteString;
{$endif}
begin
{$ifdef darwin}
{$ifdef darwinsandbox}
 if DirectoryExists(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'Library')+'Containers') then begin
  if length(Postfix)>0 then begin
   TruePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'Library')+'Containers')+Postfix;
   if not DirectoryExists(TruePath) then begin
    CreateDir(TruePath);
   end;
   result:=TruePath;
  end else begin
   result:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'Library')+'Containers';
  end;
 end else{$endif} begin
  if length(Postfix)>0 then begin
   result:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'.'+Postfix;
   if not DirectoryExists(result) then begin
    TruePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'Library')+'Engine Support')+Postfix;
    if not DirectoryExists(TruePath) then begin
     CreateDir(TruePath);
    end;
    if DirectoryExists(TruePath) then begin
     fpSymLink(PAnsiChar(TruePath),PAnsiChar(result));
    end else begin
     TruePath:=result;
    end;
    if not DirectoryExists(result) then begin
     CreateDir(result);
    end;
   end;
  end else begin
   result:=GetEnvironmentVariable('HOME');
   if DirectoryExists(result) then begin
    TruePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(result)+'Library')+'Application Support';
    if DirectoryExists(TruePath) then begin
     result:=TruePath;
    end;
   end;
  end;
 end;
 result:=IncludeTrailingPathDelimiter(result)+'cache';
 if not DirectoryExists(result) then begin
  CreateDir(result);
 end;
{$else}
 result:=GetEnvironmentVariable('XDG_CACHE_HOME');
 if (length(result)=0) or not DirectoryExists(result) then begin
  result:=GetEnvironmentVariable('HOME');
  if not DirectoryExists(result) then begin
   CreateDir(result);
  end;
  result:=IncludeTrailingPathDelimiter(result)+'.cache';
  if not DirectoryExists(result) then begin
   CreateDir(result);
  end;
 end;
 if length(Postfix)>0 then begin
  result:=IncludeTrailingPathDelimiter(result)+Postfix;
  if not DirectoryExists(result) then begin
   CreateDir(result);
  end;
 end;
 result:=IncludeTrailingPathDelimiter(result);
{$endif}
 result:=IncludeTrailingPathDelimiter(result);
end;

function GetAppDataLocalStoragePath(Postfix:TpvApplicationRawByteString):TpvApplicationRawByteString;
{$ifdef darwin}
var TruePath:TpvApplicationRawByteString;
{$endif}
begin
{$ifdef darwin}
{$ifdef darwinsandbox}
 if DirectoryExists(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'Library')+'Containers') then begin
  if length(Postfix)>0 then begin
   TruePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'Library')+'Containers')+Postfix;
   if not DirectoryExists(TruePath) then begin
    CreateDir(TruePath);
   end;
   result:=TruePath;
  end else begin
   result:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'Library')+'Containers';
  end;
 end else{$endif} begin
  if length(Postfix)>0 then begin
   result:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'.'+Postfix;
   if not DirectoryExists(result) then begin
    TruePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'Library')+'Engine Support')+Postfix;
    if not DirectoryExists(TruePath) then begin
     CreateDir(TruePath);
    end;
    if DirectoryExists(TruePath) then begin
     fpSymLink(PAnsiChar(TruePath),PAnsiChar(result));
    end else begin
     TruePath:=result;
    end;
    if not DirectoryExists(result) then begin
     CreateDir(result);
    end;
   end;
  end else begin
   result:=GetEnvironmentVariable('HOME');
   if DirectoryExists(result) then begin
    TruePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(result)+'Library')+'Application Support';
    if DirectoryExists(TruePath) then begin
     result:=TruePath;
    end;
   end;
  end;
 end;
 result:=IncludeTrailingPathDelimiter(result)+'local';
 if not DirectoryExists(result) then begin
  CreateDir(result);
 end;
{$else}
 result:=GetEnvironmentVariable('XDG_DATA_HOME');
 if (length(result)=0) or not DirectoryExists(result) then begin
  result:=GetEnvironmentVariable('HOME');
  if not DirectoryExists(result) then begin
   CreateDir(result);
  end;
  result:=IncludeTrailingPathDelimiter(result)+'.local';
  if not DirectoryExists(result) then begin
   CreateDir(result);
  end;
  result:=IncludeTrailingPathDelimiter(result)+'share';
  if not DirectoryExists(result) then begin
   CreateDir(result);
  end;
 end;
 if length(Postfix)>0 then begin
  result:=IncludeTrailingPathDelimiter(result)+Postfix;
  if not DirectoryExists(result) then begin
   CreateDir(result);
  end;
 end;
{$endif}
 result:=IncludeTrailingPathDelimiter(result);
end;

function GetAppDataRoamingStoragePath(Postfix:TpvApplicationRawByteString):TpvApplicationRawByteString;
{$ifdef darwin}
var TruePath:TpvApplicationRawByteString;
{$endif}
begin
{$ifdef darwin}
{$ifdef darwinsandbox}
 if DirectoryExists(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'Library')+'Containers') then begin
  if length(Postfix)>0 then begin
   TruePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'Library')+'Containers')+Postfix;
   if not DirectoryExists(TruePath) then begin
    CreateDir(TruePath);
   end;
   result:=TruePath;
  end else begin
   result:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'Library')+'Containers';
  end;
 end else{$endif} begin
  if length(Postfix)>0 then begin
   result:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'.'+Postfix;
   if not DirectoryExists(result) then begin
    TruePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'))+'Library')+'Engine Support')+Postfix;
    if not DirectoryExists(TruePath) then begin
     CreateDir(TruePath);
    end;
    if DirectoryExists(TruePath) then begin
     fpSymLink(PAnsiChar(TruePath),PAnsiChar(result));
    end else begin
     TruePath:=result;
    end;
    if not DirectoryExists(result) then begin
     CreateDir(result);
    end;
   end;
  end else begin
   result:=GetEnvironmentVariable('HOME');
   if DirectoryExists(result) then begin
    TruePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(result)+'Library')+'Application Support';
    if DirectoryExists(TruePath) then begin
     result:=TruePath;
    end;
   end;
  end;
 end;
 result:=IncludeTrailingPathDelimiter(result)+'roaming';
 if not DirectoryExists(result) then begin
  CreateDir(result);
 end;
{$else}
 result:=GetEnvironmentVariable('XDG_CONFIG_HOME');
 if (length(result)=0) or not DirectoryExists(result) then begin
  result:=GetEnvironmentVariable('HOME');
  if not DirectoryExists(result) then begin
   CreateDir(result);
  end;
  result:=IncludeTrailingPathDelimiter(result)+'.config';
  if not DirectoryExists(result) then begin
   CreateDir(result);
  end;
 end;
 if length(Postfix)>0 then begin
  result:=IncludeTrailingPathDelimiter(result)+Postfix;
  if not DirectoryExists(result) then begin
   CreateDir(result);
  end;
 end;
{$endif}
 result:=IncludeTrailingPathDelimiter(result);
end;
{$else}
function ExpandEnvironmentStrings(const s:TpvApplicationRawByteString):TpvApplicationRawByteString;
var i:TpvInt32;
begin
 i:=ExpandEnvironmentStringsA(pansichar(s),nil,0);
 if i>0 then begin
  SetLength(result,i);
  ExpandEnvironmentStringsA(pansichar(s),pansichar(result),i);
  SetLength(result,i-1);
 end else begin
  result:='';
 end;
end;

function GetEnvironmentVariable(const s:TpvApplicationRawByteString):TpvApplicationRawByteString;
var i:TpvInt32;
begin
 i:=GetEnvironmentVariableA(pansichar(s),nil,0);
 if i>0 then begin
  SetLength(result,i);
  GetEnvironmentVariableA(pansichar(s),pansichar(result),i);
  SetLength(result,i-1);
 end else begin
  result:='';
 end;
end;

function GetAppDataCacheStoragePath(Postfix:string):string;
type TSHGetFolderPath=function(hwndOwner:hwnd;nFolder:TpvInt32;nToken:Windows.THandle;dwFlags:TpvInt32;lpszPath:PWideChar):hresult; stdcall;
     TSHGetKnownFolderPath=function(const rfid:TGUID;dwFlags:DWord;hToken:THandle;out ppszPath:PWideChar):HResult; stdcall;
const LocalLowGUID:TGUID='{A520A1A4-1780-4FF6-BD18-167343C5AF16}';
      CSIDL_LOCALAPPDATA=$001c;
var SHGetFolderPath:TSHGetFolderPath;
    SHGetKnownFolderPath:TSHGetKnownFolderPath;
    FilePath:PWideChar;
    LibHandle:Windows.THandle;
    Reg:TRegistry;
begin
 result:='';
 try
  // First try over the SHELL32.DLL from Windows >= Vista
  LibHandle:=LoadLibrary('SHELL32.DLL');
  if LibHandle<>0 then begin
   try
    SHGetKnownFolderPath:=GetProcAddress(LibHandle,'SHGetKnownFolderPath');
    FilePath:=nil;
    if assigned(SHGetKnownFolderPath) and
       (SHGetKnownFolderPath(LocalLowGUID,0,0,FilePath)>=0) then begin
     result:=String(WideString(FilePath));
    end;
   finally
    FreeLibrary(LibHandle);
   end;
  end;
  if length(result)=0 then begin
   // Other try over the SHFOLDER.DLL from MSIE >= 5.0 on Win9x or from Windows >= 2000
   LibHandle:=LoadLibrary('SHFOLDER.DLL');
   if LibHandle<>0 then begin
    try
     SHGetFolderPath:=GetProcAddress(LibHandle,'SHGetFolderPathW');
     GetMem(FilePath,4096*2);
     FillChar(FilePath^,4096*2,ansichar(#0));
     try
      if SHGetFolderPath(0,CSIDL_LOCALAPPDATA,0,0,FilePath)=0 then begin
       result:=String(WideString(FilePath));
       if (length(result)>0) and DirectoryExists(ExpandFileName(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(result)+'..')+'LocalLow')) then begin
        result:=ExpandFileName(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(result)+'..')+'LocalLow');
       end;
      end;
     finally
      FreeMem(FilePath);
     end;
    finally
     FreeLibrary(LibHandle);
    end;
   end;
  end;
 except
  result:='';
 end;
 if length(result)=0 then begin
  // Other try over the %localappdata% enviroment variable
  result:=String(GetEnvironmentVariable('localappdata'));
  if length(result)=0 then begin
   try
    // Again ather try over the windows registry
    Reg:=TRegistry.Create;
    try
     Reg.RootKey:=HKEY_CURRENT_USER;
     if Reg.OpenKeyReadOnly('Volatile Environment') then begin
      try
       try
        result:=Reg.ReadString('LOCALAPPDATA');
        if (length(result)>0) and DirectoryExists(ExpandFileName(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(result)+'..')+'LocalLow')) then begin
         result:=ExpandFileName(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(result)+'..')+'LocalLow');
        end;
       except
        result:='';
       end;
      finally
       Reg.CloseKey;
      end;
     end;
    finally
     Reg.Free;
    end;
   except
    result:='';
   end;
   if length(result)=0 then begin
    // Fallback for Win9x without SHFOLDER.DLL from MSIE >= 5.0
    result:=String(GetEnvironmentVariable('windir'));
    if length(result)>0 then begin
     // For german Win9x installations
     result:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(String(result))+'Lokale Einstellungen')+'Anwendungsdaten';
     if not DirectoryExists(String(result)) then begin
      // For all other language Win9x installations
      result:=IncludeTrailingPathDelimiter(String(result))+'Local Settings';
      if not DirectoryExists(String(result)) then begin
       result:=IncludeTrailingPathDelimiter(String(result))+'Engine Data';
       if not DirectoryExists(String(result)) then begin
        CreateDir(String(result));
       end;
      end;
     end;
    end else begin
     // Oops!!! So use simply our own program directory then!
     result:=ExtractFilePath(ParamStr(0));
    end;
   end;
  end;
 end;
 if length(Postfix)>0 then begin
  result:=String(IncludeTrailingPathDelimiter(String(result))+String(Postfix));
  if not DirectoryExists(String(result)) then begin
   CreateDir(String(result));
  end;
 end;
 result:=IncludeTrailingPathDelimiter(String(result));
end;

function GetAppDataLocalStoragePath(Postfix:string):string;
type TSHGetFolderPath=function(hwndOwner:hwnd;nFolder:TpvInt32;nToken:Windows.THandle;dwFlags:TpvInt32;lpszPath:PWideChar):hresult; stdcall;
const CSIDL_LOCALAPPDATA=$001c;
var SHGetFolderPath:TSHGetFolderPath;
    FilePath:PWideChar;
    LibHandle:Windows.THandle;
    Reg:TRegistry;
begin
 result:='';
 try
  // First try over the SHFOLDER.DLL from MSIE >= 5.0 on Win9x or from Windows >= 2000
  LibHandle:=LoadLibrary('SHFOLDER.DLL');
  if LibHandle<>0 then begin
   try
    SHGetFolderPath:=GetProcAddress(LibHandle,'SHGetFolderPathW');
    GetMem(FilePath,4096*2);
    FillChar(FilePath^,4096*2,ansichar(#0));
    try
     if SHGetFolderPath(0,CSIDL_LOCALAPPDATA,0,0,FilePath)=0 then begin
      result:=String(WideString(FilePath));
     end;
    finally
     FreeMem(FilePath);
    end;
   finally
    FreeLibrary(LibHandle);
   end;
  end;
 except
  result:='';
 end;
 if length(result)=0 then begin
   // Other try over the %localappdata% enviroment variable
  result:=String(GetEnvironmentVariable('localappdata'));
  if length(result)=0 then begin
   try
    // Again ather try over the windows registry
    Reg:=TRegistry.Create;
    try
     Reg.RootKey:=HKEY_CURRENT_USER;
     if Reg.OpenKeyReadOnly('Volatile Environment') then begin
      try
       try
        result:=Reg.ReadString('LOCALAPPDATA');
       except
        result:='';
       end;
      finally
       Reg.CloseKey;
      end;
     end;
    finally
     Reg.Free;
    end;
   except
    result:='';
   end;
   if length(result)=0 then begin
    // Fallback for Win9x without SHFOLDER.DLL from MSIE >= 5.0
    result:=String(GetEnvironmentVariable('windir'));
    if length(result)>0 then begin
     // For german Win9x installations
     result:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(String(result))+'Lokale Einstellungen')+'Anwendungsdaten';
     if not DirectoryExists(String(result)) then begin
      // For all other language Win9x installations
      result:=IncludeTrailingPathDelimiter(String(result))+'Local Settings';
      if not DirectoryExists(String(result)) then begin
       result:=IncludeTrailingPathDelimiter(String(result))+'Engine Data';
       if not DirectoryExists(String(result)) then begin
        CreateDir(String(result));
       end;
      end;
     end;
    end else begin
     // Oops!!! So use simply our own program directory then!
     result:=ExtractFilePath(ParamStr(0));
    end;
   end;
  end;
 end;
 if length(Postfix)>0 then begin
  result:=String(IncludeTrailingPathDelimiter(String(result))+String(Postfix));
  if not DirectoryExists(String(result)) then begin
   CreateDir(String(result));
  end;
 end;
 result:=IncludeTrailingPathDelimiter(String(result));
end;

function GetAppDataRoamingStoragePath(Postfix:string):string;
type TSHGetFolderPath=function(hwndOwner:hwnd;nFolder:TpvInt32;nToken:Windows.THandle;dwFlags:TpvInt32;lpszPath:PWideChar):hresult; stdcall;
const CSIDL_APPDATA=$001a;
var SHGetFolderPath:TSHGetFolderPath;
    FilePath:PWideChar;
    LibHandle:Windows.THandle;
    Reg:TRegistry;
begin
 result:='';
 try
  // First try over the SHFOLDER.DLL from MSIE >= 5.0 on Win9x or from Windows >= 2000
  LibHandle:=LoadLibrary('SHFOLDER.DLL');
  if LibHandle<>0 then begin
   try
    SHGetFolderPath:=GetProcAddress(LibHandle,'SHGetFolderPathW');
    GetMem(FilePath,4096*2);
    FillChar(FilePath^,4096*2,ansichar(#0));
    try
     if SHGetFolderPath(0,CSIDL_APPDATA,0,0,FilePath)=0 then begin
      result:=String(WideString(FilePath));
     end;
    finally
     FreeMem(FilePath);
    end;
   finally
    FreeLibrary(LibHandle);
   end;
  end;
 except
  result:='';
 end;
 if length(result)=0 then begin
   // Other try over the %appdata% enviroment variable
  result:=String(GetEnvironmentVariable('appdata'));
  if length(result)=0 then begin
   try
    // Again ather try over the windows registry
    Reg:=TRegistry.Create;
    try
     Reg.RootKey:=HKEY_CURRENT_USER;
     if Reg.OpenKeyReadOnly('Volatile Environment') then begin
      try
       try
        result:=Reg.ReadString('APPDATA');
       except
        result:='';
       end;
      finally
       Reg.CloseKey;
      end;
     end;
    finally
     Reg.Free;
    end;
   except
    result:='';
   end;
   if length(result)=0 then begin
    // Fallback for Win9x without SHFOLDER.DLL from MSIE >= 5.0
    result:=String(GetEnvironmentVariable('windir'));
    if length(result)>0 then begin
     // For german Win9x installations
     result:=IncludeTrailingPathDelimiter(String(result))+'Anwendungsdaten';
     if not DirectoryExists(String(result)) then begin
      // For all other language Win9x installations
      result:=IncludeTrailingPathDelimiter(String(result))+'Engine Data';
      if not DirectoryExists(String(result)) then begin
       CreateDir(String(result));
      end;
     end;
    end else begin
     // Oops!!! So use simply our own program directory then!
     result:=ExtractFilePath(ParamStr(0));
    end;
   end;
  end;
 end;
 if length(Postfix)>0 then begin
  result:=String(IncludeTrailingPathDelimiter(String(result))+String(Postfix));
  if not DirectoryExists(String(result)) then begin
   CreateDir(String(result));
  end;
 end;
 result:=IncludeTrailingPathDelimiter(String(result));
end;
{$endif}

constructor EpvApplication.Create(const aTag,aMessage:string;const aLogLevel:TpvInt32=LOG_NONE);
begin
 inherited Create(aMessage);
 fTag:=aTag;
 fLogLevel:=aLogLevel;
end;

destructor EpvApplication.Destroy;
begin
 fTag:='';
 inherited Destroy;
end;

constructor TpvApplicationHighResolutionTimer.Create;
begin
 inherited Create;
 fFrequencyShift:=0;
{$ifdef windows}
 if QueryPerformanceFrequency(fFrequency) then begin
  while (fFrequency and $ffffffffe0000000)<>0 do begin
   fFrequency:=fFrequency shr 1;
   inc(fFrequencyShift);
  end;
 end else begin
  fFrequency:=1000;
 end;
{$else}
{$ifdef linux}
  fFrequency:=1000000000;
{$else}
{$ifdef unix}
  fFrequency:=1000000;
{$else}
  fFrequency:=1000;
{$endif}
{$endif}
{$endif}
 fMillisecondInterval:=(fFrequency+500) div 1000;
 fTwoMillisecondsInterval:=(fFrequency+250) div 500;
 fFourMillisecondsInterval:=(fFrequency+125) div 250;
 fQuarterSecondInterval:=(fFrequency+2) div 4;
 fMinuteInterval:=fFrequency*60;
 fHourInterval:=fFrequency*3600;
end;

destructor TpvApplicationHighResolutionTimer.Destroy;
begin
 inherited Destroy;
end;

function TpvApplicationHighResolutionTimer.GetTime:TpvInt64;
{$ifdef linux}
var NowTimeSpec:TimeSpec;
    ia,ib:TpvInt64;
{$else}
{$ifdef unix}
var tv:timeval;
    tz:timezone;
    ia,ib:TpvInt64;
{$endif}
{$endif}
begin
{$ifdef windows}
 if not QueryPerformanceCounter(result) then begin
  result:=timeGetTime;
 end;
{$else}
{$ifdef linux}
 clock_gettime(CLOCK_MONOTONIC,@NowTimeSpec);
 ia:=TpvInt64(NowTimeSpec.tv_sec)*TpvInt64(1000000000);
 ib:=NowTimeSpec.tv_nsec;
 result:=ia+ib;
{$else}
{$ifdef unix}
  tz.tz_minuteswest:=0;
  tz.tz_dsttime:=0;
  fpgettimeofday(@tv,@tz);
  ia:=TpvInt64(tv.tv_sec)*TpvInt64(1000000);
  ib:=tv.tv_usec;
  result:=ia+ib;
{$else}
 result:=SDL_GetTicks;
{$endif}
{$endif}
{$endif}
 result:=result shr fFrequencyShift;
end;

procedure TpvApplicationHighResolutionTimer.Sleep(const aDelay:TpvInt64);
var EndTime,NowTime{$ifdef unix},SleepTime{$endif}:TpvInt64;
{$ifdef unix}
    req,rem:timespec;
{$endif}
begin
 if aDelay>0 then begin
{$ifdef windows}
  NowTime:=GetTime;
  EndTime:=NowTime+aDelay;
  while (NowTime+fTwoMillisecondsInterval)<EndTime do begin
   Sleep(1);
   NowTime:=GetTime;
  end;
  while (NowTime+fMillisecondInterval)<EndTime do begin
   Sleep(0);
   NowTime:=GetTime;
  end;
  while NowTime<EndTime do begin
   NowTime:=GetTime;
  end;
{$else}
{$ifdef linux}
  NowTime:=GetTime;
  EndTime:=NowTime+aDelay;
  while true do begin
   SleepTime:=abs(EndTime-NowTime);
   if SleepTime>=fFourMillisecondsInterval then begin
    SleepTime:=(SleepTime+2) shr 2;
    if SleepTime>0 then begin
     req.tv_sec:=SleepTime div 1000000000;
     req.tv_nsec:=SleepTime mod 10000000000;
     fpNanoSleep(@req,@rem);
     NowTime:=GetTime;
     continue;
    end;
   end;
   break;
  end;
  while (NowTime+fTwoMillisecondsInterval)<EndTime do begin
   ThreadSwitch;
   NowTime:=GetTime;
  end;
  while NowTime<EndTime do begin
   NowTime:=GetTime;
  end;
{$else}
{$ifdef unix}
  NowTime:=GetTime;
  EndTime:=NowTime+aDelay;
  while true do begin
   SleepTime:=abs(EndTime-NowTime);
   if SleepTime>=fFourMillisecondsInterval then begin
    SleepTime:=(SleepTime+2) shr 2;
    if SleepTime>0 then begin
     req.tv_sec:=SleepTime div 1000000;
     req.tv_nsec:=(SleepTime mod 1000000)*1000;
     fpNanoSleep(@req,@rem);
     NowTime:=GetTime;
     continue;
    end;
   end;
   break;
  end;
  while (NowTime+fTwoMillisecondsInterval)<EndTime do begin
   ThreadSwitch;
   NowTime:=GetTime;
  end;
  while NowTime<EndTime do begin
   NowTime:=GetTime;
  end;
{$else}
  NowTime:=GetTime;
  EndTime:=NowTime+aDelay;
  while (NowTime+4)<EndTime then begin
   SDL_Delay(1);
   NowTime:=GetTime;
  end;
  while (NowTime+2)<EndTime do begin
   SDL_Delay(0);
   NowTime:=GetTime;
  end;
  while NowTime<EndTime do begin
   NowTime:=GetTime;
  end;
{$endif}
{$endif}
{$endif}
 end;
end;

function TpvApplicationHighResolutionTimer.ToFloatSeconds(const aTime:TpvApplicationHighResolutionTime):TpvDouble;
begin
 if fFrequency<>0 then begin
  result:=aTime/fFrequency;
 end else begin
  result:=0;
 end;
end;

function TpvApplicationHighResolutionTimer.FromFloatSeconds(const aTime:TpvDouble):TpvApplicationHighResolutionTime;
begin
 if fFrequency<>0 then begin
  result:=trunc(aTime*fFrequency);
 end else begin
  result:=0;
 end;
end;

function TpvApplicationHighResolutionTimer.ToMilliseconds(const aTime:TpvApplicationHighResolutionTime):TpvInt64;
begin
 result:=aTime;
 if fFrequency<>1000 then begin
  result:=((aTime*1000)+((fFrequency+1) shr 1)) div fFrequency;
 end;
end;

function TpvApplicationHighResolutionTimer.FromMilliseconds(const aTime:TpvInt64):TpvApplicationHighResolutionTime;
begin
 result:=aTime;
 if fFrequency<>1000 then begin
  result:=((aTime*fFrequency)+500) div 1000;
 end;
end;

function TpvApplicationHighResolutionTimer.ToMicroseconds(const aTime:TpvApplicationHighResolutionTime):TpvInt64;
begin
 result:=aTime;
 if fFrequency<>1000000 then begin
  result:=((aTime*1000000)+((fFrequency+1) shr 1)) div fFrequency;
 end;
end;

function TpvApplicationHighResolutionTimer.FromMicroseconds(const aTime:TpvInt64):TpvApplicationHighResolutionTime;
begin
 result:=aTime;
 if fFrequency<>1000000 then begin
  result:=((aTime*fFrequency)+500000) div 1000000;
 end;
end;

function TpvApplicationHighResolutionTimer.ToNanoseconds(const aTime:TpvApplicationHighResolutionTime):TpvInt64;
begin
 result:=aTime;
 if fFrequency<>1000000000 then begin
  result:=((aTime*1000000000)+((fFrequency+1) shr 1)) div fFrequency;
 end;
end;

function TpvApplicationHighResolutionTimer.FromNanoseconds(const aTime:TpvInt64):TpvApplicationHighResolutionTime;
begin
 result:=aTime;
 if fFrequency<>1000000000 then begin
  result:=((aTime*fFrequency)+500000000) div 1000000000;
 end;
end;

constructor TpvApplicationInputKeyEvent.Create(const aKeyEventType:TpvApplicationInputKeyEventType;
                                               const aKeyCode:TpvInt32;
                                               const aKeyModifiers:TpvApplicationInputKeyModifiers);
begin
 KeyEventType:=aKeyEventType;
 KeyCode:=aKeyCode;
 KeyModifiers:=aKeyModifiers;
end;

constructor TpvApplicationInputPointerEvent.Create(const aPointerEventType:TpvApplicationInputPointerEventType;
                                                   const aPosition:TpvVector2;
                                                   const aPressure:TpvFloat;
                                                   const aPointerID:TpvInt32;
                                                   const aButton:TpvApplicationInputPointerButton;
                                                   const aButtons:TpvApplicationInputPointerButtons;
                                                   const aKeyModifiers:TpvApplicationInputKeyModifiers);
begin
 PointerEventType:=aPointerEventType;
 Position:=aPosition;
 Pressure:=aPressure;
 PointerID:=aPointerID;
 Button:=aButton;
 Buttons:=aButtons;
 KeyModifiers:=aKeyModifiers;
end;

constructor TpvApplicationInputPointerEvent.Create(const aPointerEventType:TpvApplicationInputPointerEventType;
                                                   const aPosition:TpvVector2;
                                                   const aRelativePosition:TpvVector2;
                                                   const aPressure:TpvFloat;
                                                   const aPointerID:TpvInt32;
                                                   const aButtons:TpvApplicationInputPointerButtons;
                                                   const aKeyModifiers:TpvApplicationInputKeyModifiers);
begin
 PointerEventType:=aPointerEventType;
 Position:=aPosition;
 RelativePosition:=aRelativePosition;
 Pressure:=aPressure;
 PointerID:=aPointerID;
 Buttons:=aButtons;
 KeyModifiers:=aKeyModifiers;
end;

constructor TpvApplicationInputProcessor.Create;
begin
end;

destructor TpvApplicationInputProcessor.Destroy;
begin
end;

function TpvApplicationInputProcessor.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=false;
end;

function TpvApplicationInputProcessor.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
begin
 result:=false;
end;

function TpvApplicationInputProcessor.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 result:=false;
end;

constructor TpvApplicationInputProcessorQueue.Create;
begin
 inherited Create;
 fProcessor:=nil;
 fCriticalSection:=TPasMPCriticalSection.Create;
 fQueuedEvents:=nil;
 fLastQueuedEvent:=nil;
 fFreeEvents:=nil;
 fCurrentEventTime:=0;
end;

destructor TpvApplicationInputProcessorQueue.Destroy;
var CurrentEvent,NextEvent:PpvApplicationInputProcessorQueueEvent;
begin
 CurrentEvent:=fQueuedEvents;
 while assigned(CurrentEvent) do begin
  NextEvent:=CurrentEvent^.Next;
  Finalize(CurrentEvent^);
  FreeMem(CurrentEvent);
  CurrentEvent:=NextEvent;
 end;
 CurrentEvent:=fFreeEvents;
 while assigned(CurrentEvent) do begin
  NextEvent:=CurrentEvent^.Next;
  Finalize(CurrentEvent^);
  FreeMem(CurrentEvent);
  CurrentEvent:=NextEvent;
 end;
 FreeAndNil(fCriticalSection);
 inherited Destroy;
end;

function TpvApplicationInputProcessorQueue.NewEvent:PpvApplicationInputProcessorQueueEvent;
begin
 if assigned(fFreeEvents) then begin
  result:=fFreeEvents;
  fFreeEvents:=result^.Next;
  result^.Next:=nil;
  result^.Event:=EVENT_NONE;
 end else begin
  GetMem(result,SizeOf(TpvApplicationInputProcessorQueueEvent));
  FillChar(result^,SizeOf(TpvApplicationInputProcessorQueueEvent),AnsiChar(#0));
 end;
 Initialize(result^);
 result^.Time:=pvApplication.fHighResolutionTimer.GetTime;
end;

procedure TpvApplicationInputProcessorQueue.FreeEvent(const aEvent:PpvApplicationInputProcessorQueueEvent);
begin
 if assigned(aEvent) then begin
  Finalize(aEvent^);
  FreeMem(aEvent);
 end;
end;

procedure TpvApplicationInputProcessorQueue.PushEvent(const aEvent:PpvApplicationInputProcessorQueueEvent);
begin
 if assigned(fLastQueuedEvent) then begin
  fLastQueuedEvent^.Next:=aEvent;
 end else begin
  fQueuedEvents:=aEvent;
 end;
 fLastQueuedEvent:=aEvent;
 aEvent^.Next:=nil;
end;

procedure TpvApplicationInputProcessorQueue.SetProcessor(aProcessor:TpvApplicationInputProcessor);
begin
 fProcessor:=aProcessor;
end;

function TpvApplicationInputProcessorQueue.GetProcessor:TpvApplicationInputProcessor;
begin
 result:=fProcessor;
end;

procedure TpvApplicationInputProcessorQueue.Drain;
var Events,LastQueuedEvent,CurrentEvent,NextEvent:PpvApplicationInputProcessorQueueEvent;
begin
 fCriticalSection.Acquire;
 try
  Events:=fQueuedEvents;
  LastQueuedEvent:=fLastQueuedEvent;
  fQueuedEvents:=nil;
  fLastQueuedEvent:=nil;
 finally
  fCriticalSection.Release;
 end;
 CurrentEvent:=Events;
 while assigned(CurrentEvent) do begin
  NextEvent:=CurrentEvent^.Next;
  fCurrentEventTime:=CurrentEvent^.Time;
  if assigned(fProcessor) then begin
   case CurrentEvent^.Event of
    EVENT_KEY:begin
     fProcessor.KeyEvent(CurrentEvent^.KeyEvent);
    end;
    EVENT_POINTER:begin
     fProcessor.PointerEvent(CurrentEvent^.PointerEvent);
    end;
    EVENT_SCROLLED:begin
     fProcessor.Scrolled(CurrentEvent^.RelativeAmount);
    end;
   end;
  end;
  FreeEvent(CurrentEvent);
  CurrentEvent:=NextEvent;
 end;
end;

function TpvApplicationInputProcessorQueue.GetCurrentEventTime:TpvInt64;
begin
 result:=fCurrentEventTime;
end;

function TpvApplicationInputProcessorQueue.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
var Event:PpvApplicationInputProcessorQueueEvent;
begin
 result:=false;
 fCriticalSection.Acquire;
 try
  Event:=NewEvent;
  if assigned(Event) then begin
   Event^.Event:=EVENT_KEY;
   Event^.KeyEvent:=aKeyEvent;
   PushEvent(Event);
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInputProcessorQueue.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var Event:PpvApplicationInputProcessorQueueEvent;
begin
 result:=false;
 fCriticalSection.Acquire;
 try
  Event:=NewEvent;
  if assigned(Event) then begin
   Event^.Event:=EVENT_POINTER;
   Event^.PointerEvent:=aPointerEvent;
   PushEvent(Event);
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInputProcessorQueue.Scrolled(const aRelativeAmount:TpvVector2):boolean;
var Event:PpvApplicationInputProcessorQueueEvent;
begin
 result:=false;
 fCriticalSection.Acquire;
 try
  Event:=NewEvent;
  if assigned(Event) then begin
   Event^.Event:=EVENT_SCROLLED;
   Event^.RelativeAmount:=aRelativeAmount;
   PushEvent(Event);
  end;
 finally
  fCriticalSection.Release;
 end;
end;

constructor TpvApplicationInputMultiplexer.Create;
begin
 inherited Create;
 fProcessors:=TList.Create;
end;

destructor TpvApplicationInputMultiplexer.Destroy;
begin
 FreeAndNil(fProcessors);
 inherited Destroy;
end;

procedure TpvApplicationInputMultiplexer.AddProcessor(const aProcessor:TpvApplicationInputProcessor);
begin
 fProcessors.Add(aProcessor);
end;

procedure TpvApplicationInputMultiplexer.AddProcessors(const aProcessors:array of TpvApplicationInputProcessor);
var i:TpvInt32;
begin
 for i:=0 to length(aProcessors)-1 do begin
  fProcessors.Add(aProcessors[i]);
 end;
end;

procedure TpvApplicationInputMultiplexer.InsertProcessor(const aIndex:TpvInt32;const aProcessor:TpvApplicationInputProcessor);
begin
 fProcessors.Insert(aIndex,aProcessor);
end;

procedure TpvApplicationInputMultiplexer.RemoveProcessor(const aProcessor:TpvApplicationInputProcessor);
begin
 fProcessors.Remove(aProcessor);
end;

procedure TpvApplicationInputMultiplexer.RemoveProcessor(const aIndex:TpvInt32);
begin
 fProcessors.Delete(aIndex);
end;

procedure TpvApplicationInputMultiplexer.ClearProcessors;
begin
 fProcessors.Clear;
end;

function TpvApplicationInputMultiplexer.CountProcessors:TpvInt32;
begin
 result:=fProcessors.Count;
end;

function TpvApplicationInputMultiplexer.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
var i:TpvInt32;
    p:TpvApplicationInputProcessor;
begin
 result:=false;
 for i:=0 to fProcessors.Count-1 do begin
  p:=fProcessors.Items[i];
  if assigned(p) and p.KeyEvent(aKeyEvent) then begin
   result:=true;
   exit;
  end;
 end;
end;

function TpvApplicationInputMultiplexer.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var i:TpvInt32;
    p:TpvApplicationInputProcessor;
begin
 result:=false;
 for i:=0 to fProcessors.Count-1 do begin
  p:=fProcessors.Items[i];
  if assigned(p) and p.PointerEvent(aPointerEvent) then begin
   result:=true;
   exit;
  end;
 end;
end;

function TpvApplicationInputMultiplexer.Scrolled(const aRelativeAmount:TpvVector2):boolean;
var i:TpvInt32;
    p:TpvApplicationInputProcessor;
begin
 result:=false;
 for i:=0 to fProcessors.Count-1 do begin
  p:=fProcessors.Items[i];
  if assigned(p) then begin
   if p.Scrolled(aRelativeAmount) then begin
    result:=true;
    exit;
   end;
  end;
 end;
end;

{$if defined(PasVulkanUseSDL2)}
constructor TpvApplicationJoystick.Create(const aIndex:TpvInt32;const aJoystick:PSDL_Joystick;const aGameController:PSDL_GameController);
begin
 inherited Create;
 fIndex:=aIndex;
 fJoystick:=aJoystick;
 fGameController:=aGameController;
 if assigned(fJoystick) then begin
  fID:=SDL_JoystickInstanceID(fJoystick);
 end else begin
  fID:=-1;
 end;
end;
{$else}
constructor TpvApplicationJoystick.Create(const aIndex:TpvInt32);
begin
 inherited Create;
 fIndex:=aIndex;
 fID:=-1;
end;
{$ifend}

destructor TpvApplicationJoystick.Destroy;
begin
{$if defined(PasVulkanUseSDL2)}
 if assigned(fGameController) then begin
  SDL_GameControllerClose(fGameController);
 end else if assigned(fJoystick) then begin
  SDL_JoystickClose(fJoystick);
 end;
{$ifend}
 inherited Destroy;
end;

procedure TpvApplicationJoystick.Initialize;
begin
{$if defined(PasVulkanUseSDL2)}
 fCountAxes:=SDL_JoystickNumAxes(fJoystick);
 fCountBalls:=SDL_JoystickNumBalls(fJoystick);
 fCountHats:=SDL_JoystickNumHats(fJoystick);
 fCountButtons:=SDL_JoystickNumButtons(fJoystick);
{$ifend}
end;

function TpvApplicationJoystick.IsGameController:boolean;
begin
{$if defined(PasVulkanUseSDL2)}
 result:=assigned(fGameController);
{$else}
 result:=false;
{$ifend}
end;

function TpvApplicationJoystick.Index:TpvInt32;
begin
 result:=fIndex;
end;

function TpvApplicationJoystick.ID:TpvInt32;
begin
{$if defined(PasVulkanUseSDL2)}
 result:=SDL_JoystickInstanceID(fJoystick);
{$else}
 result:=0;
{$ifend}
end;

function TpvApplicationJoystick.Name:TpvApplicationRawByteString;
begin
{$if defined(PasVulkanUseSDL2)}
 result:=SDL_JoystickName(fJoystick);
{$else}
 result:='';
{$ifend}
end;

function TpvApplicationJoystick.GUID:TGUID;
begin
{$if defined(PasVulkanUseSDL2)}
 result:=SDL_JoystickGetGUID(fJoystick);
{$else}
 FillChar(result,SizeOf(TGUID),#0);
{$ifend}
end;

function TpvApplicationJoystick.DeviceGUID:TGUID;
begin
{$if defined(PasVulkanUseSDL2)}
 result:=SDL_JoystickGetDeviceGUID(fJoystick);
{$else}
 FillChar(result,SizeOf(TGUID),#0);
{$ifend}
end;

function TpvApplicationJoystick.CountAxes:TpvInt32;
begin
 result:=fCountAxes;
end;

function TpvApplicationJoystick.CountBalls:TpvInt32;
begin
 result:=fCountBalls;
end;

function TpvApplicationJoystick.CountHats:TpvInt32;
begin
 result:=fCountHats;
end;

function TpvApplicationJoystick.CountButtons:TpvInt32;
begin
 result:=fCountButtons;
end;

procedure TpvApplicationJoystick.Update;
begin
{$if defined(PasVulkanUseSDL2)}
 SDL_JoystickUpdate;
{$else}
{$ifend}
end;

function TpvApplicationJoystick.GetAxis(const aAxisIndex:TpvInt32):TpvInt32;
begin
{$if defined(PasVulkanUseSDL2)}
 result:=SDL_JoystickGetAxis(fJoystick,aAxisIndex);
{$else}
 result:=0;
{$ifend}
end;

function TpvApplicationJoystick.GetBall(const aBallIndex:TpvInt32;out aDeltaX,aDeltaY:TpvInt32):boolean;
begin
{$if defined(PasVulkanUseSDL2)}
 result:=SDL_JoystickGetBall(fJoystick,aBallIndex,@aDeltaX,@aDeltaY)<>0;
{$else}
 result:=false;
{$ifend}
end;

function TpvApplicationJoystick.GetHat(const aHatIndex:TpvInt32):TpvInt32;
begin
{$if defined(PasVulkanUseSDL2)}
 case SDL_JoystickGetHat(fJoystick,aHatIndex) of
  SDL_HAT_LEFTUP:begin
   result:=JOYSTICK_HAT_LEFTUP;
  end;
  SDL_HAT_UP:begin
   result:=JOYSTICK_HAT_UP;
  end;
  SDL_HAT_RIGHTUP:begin
   result:=JOYSTICK_HAT_RIGHTUP;
  end;
  SDL_HAT_LEFT:begin
   result:=JOYSTICK_HAT_LEFT;
  end;
  SDL_HAT_CENTERED:begin
   result:=JOYSTICK_HAT_CENTERED;
  end;
  SDL_HAT_RIGHT:begin
   result:=JOYSTICK_HAT_RIGHT;
  end;
  SDL_HAT_LEFTDOWN:begin
   result:=JOYSTICK_HAT_LEFTDOWN;
  end;
  SDL_HAT_DOWN:begin
   result:=JOYSTICK_HAT_DOWN;
  end;
  SDL_HAT_RIGHTDOWN:begin
   result:=JOYSTICK_HAT_RIGHTDOWN;
  end;
  else begin
   result:=JOYSTICK_HAT_NONE;
  end;
 end;
{$else}
 result:=JOYSTICK_HAT_NONE;
{$ifend}
end;

function TpvApplicationJoystick.GetButton(const aButtonIndex:TpvInt32):boolean;
begin
{$if defined(PasVulkanUseSDL2)}
 result:=SDL_JoystickGetButton(fJoystick,aButtonIndex)<>0;
{$else}
 result:=false;
{$ifend}
end;

function TpvApplicationJoystick.IsGameControllerAttached:boolean;
begin
{$if defined(PasVulkanUseSDL2)}
 if assigned(fGameController) then begin
  result:=SDL_GameControllerGetAttached(fGameController)<>0;
 end else begin
  result:=false;
 end;
{$else}
 result:=false;
{$ifend}
end;

function TpvApplicationJoystick.GetGameControllerAxis(const aAxis:TpvInt32):TpvInt32;
begin
{$if defined(PasVulkanUseSDL2)}
 if assigned(fGameController) then begin
  case aAxis of
   GAME_CONTROLLER_AXIS_LEFTX:begin
    result:=SDL_GameControllerGetAxis(fGameController,SDL_CONTROLLER_AXIS_LEFTX);
   end;
   GAME_CONTROLLER_AXIS_LEFTY:begin
    result:=SDL_GameControllerGetAxis(fGameController,SDL_CONTROLLER_AXIS_LEFTY);
   end;
   GAME_CONTROLLER_AXIS_RIGHTX:begin
    result:=SDL_GameControllerGetAxis(fGameController,SDL_CONTROLLER_AXIS_RIGHTX);
   end;
   GAME_CONTROLLER_AXIS_RIGHTY:begin
    result:=SDL_GameControllerGetAxis(fGameController,SDL_CONTROLLER_AXIS_RIGHTY);
   end;
   GAME_CONTROLLER_AXIS_TRIGGERLEFT:begin
    result:=SDL_GameControllerGetAxis(fGameController,SDL_CONTROLLER_AXIS_TRIGGERLEFT);
   end;
   GAME_CONTROLLER_AXIS_TRIGGERRIGHT:begin
    result:=SDL_GameControllerGetAxis(fGameController,SDL_CONTROLLER_AXIS_TRIGGERRIGHT);
   end;
   GAME_CONTROLLER_AXIS_MAX:begin
    result:=SDL_GameControllerGetAxis(fGameController,SDL_CONTROLLER_AXIS_MAX);
   end;
   else begin
    result:=0;
   end;
  end;
 end else begin
  result:=0;
 end;
{$else}
 result:=0;
{$ifend}
end;

function TpvApplicationJoystick.GetGameControllerButton(const aButton:TpvInt32):boolean;
begin
{$if defined(PasVulkanUseSDL2)}
 if assigned(fGameController) then begin
  case aButton of
   GAME_CONTROLLER_BUTTON_A:begin
    result:=SDL_GameControllerGetButton(fGameController,SDL_CONTROLLER_BUTTON_A)<>0;
   end;
   GAME_CONTROLLER_BUTTON_B:begin
    result:=SDL_GameControllerGetButton(fGameController,SDL_CONTROLLER_BUTTON_B)<>0;
   end;
   GAME_CONTROLLER_BUTTON_X:begin
    result:=SDL_GameControllerGetButton(fGameController,SDL_CONTROLLER_BUTTON_X)<>0;
   end;
   GAME_CONTROLLER_BUTTON_Y:begin
    result:=SDL_GameControllerGetButton(fGameController,SDL_CONTROLLER_BUTTON_Y)<>0;
   end;
   GAME_CONTROLLER_BUTTON_BACK:begin
    result:=SDL_GameControllerGetButton(fGameController,SDL_CONTROLLER_BUTTON_BACK)<>0;
   end;
   GAME_CONTROLLER_BUTTON_GUIDE:begin
    result:=SDL_GameControllerGetButton(fGameController,SDL_CONTROLLER_BUTTON_GUIDE)<>0;
   end;
   GAME_CONTROLLER_BUTTON_START:begin
    result:=SDL_GameControllerGetButton(fGameController,SDL_CONTROLLER_BUTTON_START)<>0;
   end;
   GAME_CONTROLLER_BUTTON_LEFTSTICK:begin
    result:=SDL_GameControllerGetButton(fGameController,SDL_CONTROLLER_BUTTON_LEFTSTICK)<>0;
   end;
   GAME_CONTROLLER_BUTTON_RIGHTSTICK:begin
    result:=SDL_GameControllerGetButton(fGameController,SDL_CONTROLLER_BUTTON_RIGHTSTICK)<>0;
   end;
   GAME_CONTROLLER_BUTTON_LEFTSHOULDER:begin
    result:=SDL_GameControllerGetButton(fGameController,SDL_CONTROLLER_BUTTON_LEFTSHOULDER)<>0;
   end;
   GAME_CONTROLLER_BUTTON_RIGHTSHOULDER:begin
    result:=SDL_GameControllerGetButton(fGameController,SDL_CONTROLLER_BUTTON_RIGHTSHOULDER)<>0;
   end;
   GAME_CONTROLLER_BUTTON_DPAD_UP:begin
    result:=SDL_GameControllerGetButton(fGameController,SDL_CONTROLLER_BUTTON_DPAD_UP)<>0;
   end;
   GAME_CONTROLLER_BUTTON_DPAD_DOWN:begin
    result:=SDL_GameControllerGetButton(fGameController,SDL_CONTROLLER_BUTTON_DPAD_DOWN)<>0;
   end;
   GAME_CONTROLLER_BUTTON_DPAD_LEFT:begin
    result:=SDL_GameControllerGetButton(fGameController,SDL_CONTROLLER_BUTTON_DPAD_LEFT)<>0;
   end;
   GAME_CONTROLLER_BUTTON_DPAD_RIGHT:begin
    result:=SDL_GameControllerGetButton(fGameController,SDL_CONTROLLER_BUTTON_DPAD_RIGHT)<>0;
   end;
   else begin
    result:=false;
   end;
  end;
 end else begin
  result:=false;
 end;
{$else}
 result:=false;
{$ifend}
end;

function TpvApplicationJoystick.GetGameControllerName:TpvApplicationRawByteString;
begin
{$if defined(PasVulkanUseSDL2)}
 if assigned(fGameController) then begin
  result:=SDL_GameControllerName(fGameController);
 end else begin
  result:='';
 end;
{$else}
 result:='';
{$ifend}
end;

function TpvApplicationJoystick.GetGameControllerMapping:TpvApplicationRawByteString;
begin
{$if defined(PasVulkanUseSDL2)}
 if assigned(fGameController) then begin
  result:=SDL_GameControllerMapping(fGameController);
 end else begin
  result:='';
 end;
{$else}
 result:='';
{$ifend}
end;

constructor TpvApplicationInput.Create(const aVulkanApplication:TpvApplication);
begin
 inherited Create;
 fVulkanApplication:=aVulkanApplication;
 FillChar(fKeyCodeNames,SizeOf(fKeyCodeNames),AnsiChar(#0));
 fKeyCodeNames[KEYCODE_ANYKEY]:='ANYKEY';
 fKeyCodeNames[KEYCODE_UNKNOWN]:='UNKNOWN';
 fKeyCodeNames[KEYCODE_FIRST]:='FIRST';
 fKeyCodeNames[KEYCODE_BACKSPACE]:='BACKSPACE';
 fKeyCodeNames[KEYCODE_TAB]:='TAB';
 fKeyCodeNames[KEYCODE_RETURN]:='RETURN';
 fKeyCodeNames[KEYCODE_PAUSE]:='PAUSE';
 fKeyCodeNames[KEYCODE_ESCAPE]:='ESCAPE';
 fKeyCodeNames[KEYCODE_SPACE]:='SPACE';
 fKeyCodeNames[KEYCODE_EXCLAIM]:='EXCLAIM';
 fKeyCodeNames[KEYCODE_QUOTEDBL]:='QUOTEDBL';
 fKeyCodeNames[KEYCODE_HASH]:='HASH';
 fKeyCodeNames[KEYCODE_DOLLAR]:='DOLLAR';
 fKeyCodeNames[KEYCODE_AMPERSAND]:='AMPERSAND';
 fKeyCodeNames[KEYCODE_QUOTE]:='QUOTE';
 fKeyCodeNames[KEYCODE_LEFTPAREN]:='LEFTPAREN';
 fKeyCodeNames[KEYCODE_RIGHTPAREN]:='RIGHTPAREN';
 fKeyCodeNames[KEYCODE_ASTERISK]:='ASTERISK';
 fKeyCodeNames[KEYCODE_PLUS]:='PLUS';
 fKeyCodeNames[KEYCODE_COMMA]:='COMMA';
 fKeyCodeNames[KEYCODE_MINUS]:='MINUS';
 fKeyCodeNames[KEYCODE_PERIOD]:='PERIOD';
 fKeyCodeNames[KEYCODE_SLASH]:='SLASH';
 fKeyCodeNames[KEYCODE_0]:='0';
 fKeyCodeNames[KEYCODE_1]:='1';
 fKeyCodeNames[KEYCODE_2]:='2';
 fKeyCodeNames[KEYCODE_3]:='3';
 fKeyCodeNames[KEYCODE_4]:='4';
 fKeyCodeNames[KEYCODE_5]:='5';
 fKeyCodeNames[KEYCODE_6]:='6';
 fKeyCodeNames[KEYCODE_7]:='7';
 fKeyCodeNames[KEYCODE_8]:='8';
 fKeyCodeNames[KEYCODE_9]:='9';
 fKeyCodeNames[KEYCODE_COLON]:='COLON';
 fKeyCodeNames[KEYCODE_SEMICOLON]:='SEMICOLON';
 fKeyCodeNames[KEYCODE_LESS]:='LESS';
 fKeyCodeNames[KEYCODE_EQUALS]:='EQUALS';
 fKeyCodeNames[KEYCODE_GREATER]:='GREATER';
 fKeyCodeNames[KEYCODE_QUESTION]:='QUESTION';
 fKeyCodeNames[KEYCODE_AT]:='AT';
 fKeyCodeNames[KEYCODE_LEFTBRACKET]:='LEFTBRACKET';
 fKeyCodeNames[KEYCODE_BACKSLASH]:='BACKSLASH';
 fKeyCodeNames[KEYCODE_RIGHTBRACKET]:='RIGHTBRACKET';
 fKeyCodeNames[KEYCODE_CARET]:='CARET';
 fKeyCodeNames[KEYCODE_UNDERSCORE]:='UNDERSCORE';
 fKeyCodeNames[KEYCODE_BACKQUOTE]:='BACKQUOTE';
 fKeyCodeNames[KEYCODE_a]:='a';
 fKeyCodeNames[KEYCODE_b]:='b';
 fKeyCodeNames[KEYCODE_c]:='c';
 fKeyCodeNames[KEYCODE_d]:='d';
 fKeyCodeNames[KEYCODE_e]:='e';
 fKeyCodeNames[KEYCODE_f]:='f';
 fKeyCodeNames[KEYCODE_g]:='g';
 fKeyCodeNames[KEYCODE_h]:='h';
 fKeyCodeNames[KEYCODE_i]:='i';
 fKeyCodeNames[KEYCODE_j]:='j';
 fKeyCodeNames[KEYCODE_k]:='k';
 fKeyCodeNames[KEYCODE_l]:='l';
 fKeyCodeNames[KEYCODE_m]:='m';
 fKeyCodeNames[KEYCODE_n]:='n';
 fKeyCodeNames[KEYCODE_o]:='o';
 fKeyCodeNames[KEYCODE_p]:='p';
 fKeyCodeNames[KEYCODE_q]:='q';
 fKeyCodeNames[KEYCODE_r]:='r';
 fKeyCodeNames[KEYCODE_s]:='s';
 fKeyCodeNames[KEYCODE_t]:='t';
 fKeyCodeNames[KEYCODE_u]:='u';
 fKeyCodeNames[KEYCODE_v]:='v';
 fKeyCodeNames[KEYCODE_w]:='w';
 fKeyCodeNames[KEYCODE_x]:='x';
 fKeyCodeNames[KEYCODE_y]:='y';
 fKeyCodeNames[KEYCODE_z]:='z';
 fKeyCodeNames[KEYCODE_DELETE]:='DELETE';
 fKeyCodeNames[KEYCODE_F1]:='F1';
 fKeyCodeNames[KEYCODE_F2]:='F2';
 fKeyCodeNames[KEYCODE_F3]:='F3';
 fKeyCodeNames[KEYCODE_F4]:='F4';
 fKeyCodeNames[KEYCODE_F5]:='F5';
 fKeyCodeNames[KEYCODE_F6]:='F6';
 fKeyCodeNames[KEYCODE_F7]:='F7';
 fKeyCodeNames[KEYCODE_F8]:='F8';
 fKeyCodeNames[KEYCODE_F9]:='F9';
 fKeyCodeNames[KEYCODE_F10]:='F10';
 fKeyCodeNames[KEYCODE_F11]:='F11';
 fKeyCodeNames[KEYCODE_F12]:='F12';
 fKeyCodeNames[KEYCODE_F13]:='F13';
 fKeyCodeNames[KEYCODE_F14]:='F14';
 fKeyCodeNames[KEYCODE_F15]:='F15';
 fKeyCodeNames[KEYCODE_F16]:='F16';
 fKeyCodeNames[KEYCODE_F17]:='F17';
 fKeyCodeNames[KEYCODE_F18]:='F18';
 fKeyCodeNames[KEYCODE_F19]:='F19';
 fKeyCodeNames[KEYCODE_F20]:='F20';
 fKeyCodeNames[KEYCODE_F21]:='F21';
 fKeyCodeNames[KEYCODE_F22]:='F22';
 fKeyCodeNames[KEYCODE_F23]:='F23';
 fKeyCodeNames[KEYCODE_F24]:='F24';
 fKeyCodeNames[KEYCODE_KP0]:='KP0';
 fKeyCodeNames[KEYCODE_KP1]:='KP1';
 fKeyCodeNames[KEYCODE_KP2]:='KP2';
 fKeyCodeNames[KEYCODE_KP3]:='KP3';
 fKeyCodeNames[KEYCODE_KP4]:='KP4';
 fKeyCodeNames[KEYCODE_KP5]:='KP5';
 fKeyCodeNames[KEYCODE_KP6]:='KP6';
 fKeyCodeNames[KEYCODE_KP7]:='KP7';
 fKeyCodeNames[KEYCODE_KP8]:='KP8';
 fKeyCodeNames[KEYCODE_KP9]:='KP9';
 fKeyCodeNames[KEYCODE_KP_PERIOD]:='KP_PERIOD';
 fKeyCodeNames[KEYCODE_KP_DIVIDE]:='KP_DIVIDE';
 fKeyCodeNames[KEYCODE_KP_MULTIPLY]:='KP_MULTIPLY';
 fKeyCodeNames[KEYCODE_KP_MINUS]:='KP_MINUS';
 fKeyCodeNames[KEYCODE_KP_PLUS]:='KP_PLUS';
 fKeyCodeNames[KEYCODE_KP_ENTER]:='KP_ENTER';
 fKeyCodeNames[KEYCODE_KP_EQUALS]:='KP_EQUALS';
 fKeyCodeNames[KEYCODE_UP]:='UP';
 fKeyCodeNames[KEYCODE_DOWN]:='DOWN';
 fKeyCodeNames[KEYCODE_RIGHT]:='RIGHT';
 fKeyCodeNames[KEYCODE_LEFT]:='LEFT';
 fKeyCodeNames[KEYCODE_INSERT]:='INSERT';
 fKeyCodeNames[KEYCODE_HOME]:='HOME';
 fKeyCodeNames[KEYCODE_END]:='END';
 fKeyCodeNames[KEYCODE_PAGEUP]:='PAGEUP';
 fKeyCodeNames[KEYCODE_PAGEDOWN]:='PAGEDOWN';
 fKeyCodeNames[KEYCODE_CAPSLOCK]:='CAPSLOCK';
 fKeyCodeNames[KEYCODE_NUMLOCK]:='NUMLOCK';
 fKeyCodeNames[KEYCODE_SCROLLOCK]:='SCROLLOCK';
 fKeyCodeNames[KEYCODE_RSHIFT]:='RSHIFT';
 fKeyCodeNames[KEYCODE_LSHIFT]:='LSHIFT';
 fKeyCodeNames[KEYCODE_RCTRL]:='RCTRL';
 fKeyCodeNames[KEYCODE_LCTRL]:='LCTRL';
 fKeyCodeNames[KEYCODE_RALT]:='RALT';
 fKeyCodeNames[KEYCODE_LALT]:='LALT';
 fKeyCodeNames[KEYCODE_MODE]:='MODE';
 fKeyCodeNames[KEYCODE_HELP]:='HELP';
 fKeyCodeNames[KEYCODE_PRINTSCREEN]:='PRINTSCREEN';
 fKeyCodeNames[KEYCODE_SYSREQ]:='SYSREQ';
 fKeyCodeNames[KEYCODE_MENU]:='MENU';
 fKeyCodeNames[KEYCODE_POWER]:='POWER';
 fKeyCodeNames[KEYCODE_APPLICATION]:='Engine';
 fKeyCodeNames[KEYCODE_SELECT]:='SELECT';
 fKeyCodeNames[KEYCODE_STOP]:='STOP';
 fKeyCodeNames[KEYCODE_AGAIN]:='AGAIN';
 fKeyCodeNames[KEYCODE_UNDO]:='UNDO';
 fKeyCodeNames[KEYCODE_CUT]:='CUT';
 fKeyCodeNames[KEYCODE_COPY]:='COPY';
 fKeyCodeNames[KEYCODE_PASTE]:='PASTE';
 fKeyCodeNames[KEYCODE_FIND]:='FIND';
 fKeyCodeNames[KEYCODE_MUTE]:='MUTE';
 fKeyCodeNames[KEYCODE_VOLUMEUP]:='VOLUMEUP';
 fKeyCodeNames[KEYCODE_VOLUMEDOWN]:='VOLUMEDOWN';
 fKeyCodeNames[KEYCODE_KP_EQUALSAS400]:='KP_EQUALSAS400';
 fKeyCodeNames[KEYCODE_ALTERASE]:='ALTERASE';
 fKeyCodeNames[KEYCODE_CANCEL]:='CANCEL';
 fKeyCodeNames[KEYCODE_CLEAR]:='CLEAR';
 fKeyCodeNames[KEYCODE_PRIOR]:='PRIOR';
 fKeyCodeNames[KEYCODE_RETURN2]:='RETURN2';
 fKeyCodeNames[KEYCODE_SEPARATOR]:='SEPARATOR';
 fKeyCodeNames[KEYCODE_OUT]:='OUT';
 fKeyCodeNames[KEYCODE_OPER]:='OPER';
 fKeyCodeNames[KEYCODE_CLEARAGAIN]:='CLEARAGAIN';
 fKeyCodeNames[KEYCODE_CRSEL]:='CRSEL';
 fKeyCodeNames[KEYCODE_EXSEL]:='EXSEL';
 fKeyCodeNames[KEYCODE_KP_00]:='KP_00';
 fKeyCodeNames[KEYCODE_KP_000]:='KP_000';
 fKeyCodeNames[KEYCODE_THOUSANDSSEPARATOR]:='THOUSANDSSEPARATOR';
 fKeyCodeNames[KEYCODE_DECIMALSEPARATOR]:='DECIMALSEPARATOR';
 fKeyCodeNames[KEYCODE_CURRENCYUNIT]:='CURRENCYUNIT';
 fKeyCodeNames[KEYCODE_CURRENCYSUBUNIT]:='CURRENCYSUBUNIT';
 fKeyCodeNames[KEYCODE_KP_LEFTPAREN]:='KP_LEFTPAREN';
 fKeyCodeNames[KEYCODE_KP_RIGHTPAREN]:='KP_RIGHTPAREN';
 fKeyCodeNames[KEYCODE_KP_LEFTBRACE]:='KP_LEFTBRACE';
 fKeyCodeNames[KEYCODE_KP_RIGHTBRACE]:='KP_RIGHTBRACE';
 fKeyCodeNames[KEYCODE_KP_TAB]:='KP_TAB';
 fKeyCodeNames[KEYCODE_KP_BACKSPACE]:='KP_BACKSPACE';
 fKeyCodeNames[KEYCODE_KP_A]:='KP_A';
 fKeyCodeNames[KEYCODE_KP_B]:='KP_B';
 fKeyCodeNames[KEYCODE_KP_C]:='KP_C';
 fKeyCodeNames[KEYCODE_KP_D]:='KP_D';
 fKeyCodeNames[KEYCODE_KP_E]:='KP_E';
 fKeyCodeNames[KEYCODE_KP_F]:='KP_F';
 fKeyCodeNames[KEYCODE_KP_XOR]:='KP_XOR';
 fKeyCodeNames[KEYCODE_KP_POWER]:='KP_POWER';
 fKeyCodeNames[KEYCODE_KP_PERCENT]:='KP_PERCENT';
 fKeyCodeNames[KEYCODE_KP_LESS]:='KP_LESS';
 fKeyCodeNames[KEYCODE_KP_GREATER]:='KP_GREATER';
 fKeyCodeNames[KEYCODE_KP_AMPERSAND]:='KP_AMPERSAND';
 fKeyCodeNames[KEYCODE_KP_DBLAMPERSAND]:='KP_DBLAMPERSAND';
 fKeyCodeNames[KEYCODE_KP_VERTICALBAR]:='KP_VERTICALBAR';
 fKeyCodeNames[KEYCODE_KP_DBLVERTICALBAR]:='KP_DBLVERTICALBAR';
 fKeyCodeNames[KEYCODE_KP_COLON]:='KP_COLON';
 fKeyCodeNames[KEYCODE_KP_HASH]:='KP_HASH';
 fKeyCodeNames[KEYCODE_KP_SPACE]:='KP_SPACE';
 fKeyCodeNames[KEYCODE_KP_AT]:='KP_AT';
 fKeyCodeNames[KEYCODE_KP_EXCLAM]:='KP_EXCLAM';
 fKeyCodeNames[KEYCODE_KP_MEMSTORE]:='KP_MEMSTORE';
 fKeyCodeNames[KEYCODE_KP_MEMRECALL]:='KP_MEMRECALL';
 fKeyCodeNames[KEYCODE_KP_MEMCLEAR]:='KP_MEMCLEAR';
 fKeyCodeNames[KEYCODE_KP_MEMADD]:='KP_MEMADD';
 fKeyCodeNames[KEYCODE_KP_MEMSUBTRACT]:='KP_MEMSUBTRACT';
 fKeyCodeNames[KEYCODE_KP_MEMMULTIPLY]:='KP_MEMMULTIPLY';
 fKeyCodeNames[KEYCODE_KP_MEMDIVIDE]:='KP_MEMDIVIDE';
 fKeyCodeNames[KEYCODE_KP_PLUSMINUS]:='KP_PLUSMINUS';
 fKeyCodeNames[KEYCODE_KP_CLEAR]:='KP_CLEAR';
 fKeyCodeNames[KEYCODE_KP_CLEARENTRY]:='KP_CLEARENTRY';
 fKeyCodeNames[KEYCODE_KP_BINARY]:='KP_BINARY';
 fKeyCodeNames[KEYCODE_KP_OCTAL]:='KP_OCTAL';
 fKeyCodeNames[KEYCODE_KP_DECIMAL]:='KP_DECIMAL';
 fKeyCodeNames[KEYCODE_KP_HEXADECIMAL]:='KP_HEXADECIMAL';
 fKeyCodeNames[KEYCODE_LGUI]:='LGUI';
 fKeyCodeNames[KEYCODE_RGUI]:='RGUI';
 fKeyCodeNames[KEYCODE_AUDIONEXT]:='AUDIONEXT';
 fKeyCodeNames[KEYCODE_AUDIOPREV]:='AUDIOPREV';
 fKeyCodeNames[KEYCODE_AUDIOSTOP]:='AUDIOSTOP';
 fKeyCodeNames[KEYCODE_AUDIOPLAY]:='AUDIOPLAY';
 fKeyCodeNames[KEYCODE_AUDIOMUTE]:='AUDIOMUTE';
 fKeyCodeNames[KEYCODE_MEDIASELECT]:='MEDIASELECT';
 fKeyCodeNames[KEYCODE_WWW]:='WWW';
 fKeyCodeNames[KEYCODE_MAIL]:='MAIL';
 fKeyCodeNames[KEYCODE_CALCULATOR]:='CALCULATOR';
 fKeyCodeNames[KEYCODE_COMPUTER]:='COMPUTER';
 fKeyCodeNames[KEYCODE_AC_SEARCH]:='AC_SEARCH';
 fKeyCodeNames[KEYCODE_AC_HOME]:='AC_HOME';
 fKeyCodeNames[KEYCODE_AC_BACK]:='AC_BACK';
 fKeyCodeNames[KEYCODE_AC_FORWARD]:='AC_FORWARD';
 fKeyCodeNames[KEYCODE_AC_STOP]:='AC_STOP';
 fKeyCodeNames[KEYCODE_AC_REFRESH]:='AC_REFRESH';
 fKeyCodeNames[KEYCODE_AC_BOOKMARKS]:='AC_BOOKMARKS';
 fKeyCodeNames[KEYCODE_BRIGHTNESSDOWN]:='BRIGHTNESSDOWN';
 fKeyCodeNames[KEYCODE_BRIGHTNESSUP]:='BRIGHTNESSUP';
 fKeyCodeNames[KEYCODE_DISPLAYSWITCH]:='DISPLAYSWITCH';
 fKeyCodeNames[KEYCODE_KBDILLUMTOGGLE]:='KBDILLUMTOGGLE';
 fKeyCodeNames[KEYCODE_KBDILLUMDOWN]:='KBDILLUMDOWN';
 fKeyCodeNames[KEYCODE_KBDILLUMUP]:='KBDILLUMUP';
 fKeyCodeNames[KEYCODE_EJECT]:='EJECT';
 fKeyCodeNames[KEYCODE_SLEEP]:='SLEEP';
 fKeyCodeNames[KEYCODE_INTERNATIONAL1]:='INTERNATIONAL1';
 fKeyCodeNames[KEYCODE_INTERNATIONAL2]:='INTERNATIONAL2';
 fKeyCodeNames[KEYCODE_INTERNATIONAL3]:='INTERNATIONAL3';
 fKeyCodeNames[KEYCODE_INTERNATIONAL4]:='INTERNATIONAL4';
 fKeyCodeNames[KEYCODE_INTERNATIONAL5]:='INTERNATIONAL5';
 fKeyCodeNames[KEYCODE_INTERNATIONAL6]:='INTERNATIONAL6';
 fKeyCodeNames[KEYCODE_INTERNATIONAL7]:='INTERNATIONAL7';
 fKeyCodeNames[KEYCODE_INTERNATIONAL8]:='INTERNATIONAL8';
 fKeyCodeNames[KEYCODE_INTERNATIONAL9]:='INTERNATIONAL9';
 fKeyCodeNames[KEYCODE_LANG1]:='LANG1';
 fKeyCodeNames[KEYCODE_LANG2]:='LANG2';
 fKeyCodeNames[KEYCODE_LANG3]:='LANG3';
 fKeyCodeNames[KEYCODE_LANG4]:='LANG4';
 fKeyCodeNames[KEYCODE_LANG5]:='LANG5';
 fKeyCodeNames[KEYCODE_LANG6]:='LANG6';
 fKeyCodeNames[KEYCODE_LANG7]:='LANG7';
 fKeyCodeNames[KEYCODE_LANG8]:='LANG8';
 fKeyCodeNames[KEYCODE_LANG9]:='LANG9';
 fKeyCodeNames[KEYCODE_LOCKINGCAPSLOCK]:='LOCKINGCAPSLOCK';
 fKeyCodeNames[KEYCODE_LOCKINGNUMLOCK]:='LOCKINGNUMLOCK';
 fKeyCodeNames[KEYCODE_LOCKINGSCROLLLOCK]:='LOCKINGSCROLLLOCK';
 fKeyCodeNames[KEYCODE_NONUSBACKSLASH]:='NONUSBACKSLASH';
 fKeyCodeNames[KEYCODE_NONUSHASH]:='NONUSHASH';
 fKeyCodeNames[KEYCODE_BACK]:='BACK';
 fKeyCodeNames[KEYCODE_CAMERA]:='CAMERA';
 fKeyCodeNames[KEYCODE_CALL]:='CALL';
 fKeyCodeNames[KEYCODE_CENTER]:='CENTER';
 fKeyCodeNames[KEYCODE_FORWARD_DEL]:='FORWARD_DEL';
 fKeyCodeNames[KEYCODE_DPAD_CENTER]:='DPAD_CENTER';
 fKeyCodeNames[KEYCODE_DPAD_LEFT]:='DPAD_LEFT';
 fKeyCodeNames[KEYCODE_DPAD_RIGHT]:='DPAD_RIGHT';
 fKeyCodeNames[KEYCODE_DPAD_DOWN]:='DPAD_DOWN';
 fKeyCodeNames[KEYCODE_DPAD_UP]:='DPAD_UP';
 fKeyCodeNames[KEYCODE_ENDCALL]:='ENDCALL';
 fKeyCodeNames[KEYCODE_ENVELOPE]:='ENVELOPE';
 fKeyCodeNames[KEYCODE_EXPLORER]:='EXPLORER';
 fKeyCodeNames[KEYCODE_FOCUS]:='FOCUS';
 fKeyCodeNames[KEYCODE_GRAVE]:='GRAVE';
 fKeyCodeNames[KEYCODE_HEADSETHOOK]:='HEADSETHOOK';
 fKeyCodeNames[KEYCODE_AUDIO_FAST_FORWARD]:='AUDIO_FAST_FORWARD';
 fKeyCodeNames[KEYCODE_AUDIO_REWIND]:='AUDIO_REWIND';
 fKeyCodeNames[KEYCODE_NOTIFICATION]:='NOTIFICATION';
 fKeyCodeNames[KEYCODE_PICTSYMBOLS]:='PICTSYMBOLS';
 fKeyCodeNames[KEYCODE_SWITCH_CHARSET]:='SWITCH_CHARSET';
 fKeyCodeNames[KEYCODE_BUTTON_CIRCLE]:='BUTTON_CIRCLE';
 fKeyCodeNames[KEYCODE_BUTTON_A]:='BUTTON_A';
 fKeyCodeNames[KEYCODE_BUTTON_B]:='BUTTON_B';
 fKeyCodeNames[KEYCODE_BUTTON_C]:='BUTTON_C';
 fKeyCodeNames[KEYCODE_BUTTON_X]:='BUTTON_X';
 fKeyCodeNames[KEYCODE_BUTTON_Y]:='BUTTON_Y';
 fKeyCodeNames[KEYCODE_BUTTON_Z]:='BUTTON_Z';
 fKeyCodeNames[KEYCODE_BUTTON_L1]:='BUTTON_L1';
 fKeyCodeNames[KEYCODE_BUTTON_R1]:='BUTTON_R1';
 fKeyCodeNames[KEYCODE_BUTTON_L2]:='BUTTON_L2';
 fKeyCodeNames[KEYCODE_BUTTON_R2]:='BUTTON_R2';
 fKeyCodeNames[KEYCODE_BUTTON_THUMBL]:='BUTTON_THUMBL';
 fKeyCodeNames[KEYCODE_BUTTON_THUMBR]:='BUTTON_THUMBR';
 fKeyCodeNames[KEYCODE_BUTTON_START]:='BUTTON_START';
 fKeyCodeNames[KEYCODE_BUTTON_SELECT]:='BUTTON_SELECT';
 fKeyCodeNames[KEYCODE_BUTTON_MODE]:='BUTTON_MODE';
 fCriticalSection:=TPasMPCriticalSection.Create;
 fProcessor:=nil;
 fEvents:=nil;
 fEventTimes:=nil;
 fEventCount:=0;
 fCurrentEventTime:=0;
 FillChar(fKeyDown,SizeOf(fKeyDown),AnsiChar(#0));
 fKeyDownCount:=0;
 FillChar(fJustKeyDown,SizeOf(fJustKeyDown),AnsiChar(#0));
 FillChar(fPointerX,SizeOf(fPointerX),AnsiChar(#0));
 FillChar(fPointerY,SizeOf(fPointerY),AnsiChar(#0));
 FillChar(fPointerDown,SizeOf(fPointerDown),AnsiChar(#0));
 FillChar(fPointerJustDown,SizeOf(fPointerJustDown),AnsiChar(#0));
 FillChar(fPointerPressure,SizeOf(fPointerPressure),AnsiChar(#0));
 FillChar(fPointerDeltaX,SizeOf(fPointerDeltaX),AnsiChar(#0));
 FillChar(fPointerDeltaY,SizeOf(fPointerDeltaY),AnsiChar(#0));
 fPointerDownCount:=0;
 fMouseX:=0;
 fMouseY:=0;
 fMouseDown:=[];
 fMouseJustDown:=[];
 fMouseDeltaX:=0;
 fMouseDeltaY:=0;
 fJustTouched:=false;
 fMaxPointerID:=-1;
 SetLength(fEvents,1024);
 SetLength(fEventTimes,1024);
 fJoysticks:=TList.Create;
 fMainJoystick:=nil;
end;

destructor TpvApplicationInput.Destroy;
begin
 while fJoysticks.Count>0 do begin
  TpvApplicationJoystick(fJoysticks[fJoysticks.Count-1]).Free;
  fJoysticks.Delete(fJoysticks.Count-1);
 end;
 fJoysticks.Free;
 SetLength(fEvents,0);
 fCriticalSection.Free;
 inherited Destroy;
end;

{$if defined(PasVulkanUseSDL2)}
function TpvApplicationInput.TranslateSDLKeyCode(const aKeyCode,aScanCode:TpvInt32):TpvInt32;
begin
 case aKeyCode of
  SDLK_BACKSPACE:begin
   result:=KEYCODE_BACKSPACE;
  end;
  SDLK_TAB:begin
   result:=KEYCODE_TAB;
  end;
  SDLK_RETURN:begin
   result:=KEYCODE_RETURN;
  end;
  SDLK_PAUSE:begin
   result:=KEYCODE_PAUSE;
  end;
  SDLK_ESCAPE:begin
   result:=KEYCODE_ESCAPE;
  end;
  SDLK_SPACE:begin
   result:=KEYCODE_SPACE;
  end;
  SDLK_EXCLAIM:begin
   result:=KEYCODE_EXCLAIM;
  end;
  SDLK_QUOTEDBL:begin
   result:=KEYCODE_QUOTEDBL;
  end;
  SDLK_HASH:begin
   result:=KEYCODE_HASH;
  end;
  SDLK_DOLLAR:begin
   result:=KEYCODE_DOLLAR;
  end;
  SDLK_AMPERSAND:begin
   result:=KEYCODE_AMPERSAND;
  end;
  SDLK_QUOTE:begin
   result:=KEYCODE_QUOTE;
  end;
  SDLK_LEFTPAREN:begin
   result:=KEYCODE_LEFTPAREN;
  end;
  SDLK_RIGHTPAREN:begin
   result:=KEYCODE_RIGHTPAREN;
  end;
  SDLK_ASTERISK:begin
   result:=KEYCODE_ASTERISK;
  end;
  SDLK_PLUS:begin
   result:=KEYCODE_PLUS;
  end;
  SDLK_COMMA:begin
   result:=KEYCODE_COMMA;
  end;
  SDLK_MINUS:begin
   result:=KEYCODE_MINUS;
  end;
  SDLK_PERIOD:begin
   result:=KEYCODE_PERIOD;
  end;
  SDLK_SLASH:begin
   result:=KEYCODE_SLASH;
  end;
  SDLK_0:begin
   result:=KEYCODE_0;
  end;
  SDLK_1:begin
   result:=KEYCODE_1;
  end;
  SDLK_2:begin
   result:=KEYCODE_2;
  end;
  SDLK_3:begin
   result:=KEYCODE_3;
  end;
  SDLK_4:begin
   result:=KEYCODE_4;
  end;
  SDLK_5:begin
   result:=KEYCODE_5;
  end;
  SDLK_6:begin
   result:=KEYCODE_6;
  end;
  SDLK_7:begin
   result:=KEYCODE_7;
  end;
  SDLK_8:begin
   result:=KEYCODE_8;
  end;
  SDLK_9:begin
   result:=KEYCODE_9;
  end;
  SDLK_COLON:begin
   result:=KEYCODE_COLON;
  end;
  SDLK_SEMICOLON:begin
   result:=KEYCODE_SEMICOLON;
  end;
  SDLK_LESS:begin
   result:=KEYCODE_LESS;
  end;
  SDLK_EQUALS:begin
   result:=KEYCODE_EQUALS;
  end;
  SDLK_GREATER:begin
   result:=KEYCODE_GREATER;
  end;
  SDLK_QUESTION:begin
   result:=KEYCODE_QUESTION;
  end;
  SDLK_AT:begin
   result:=KEYCODE_AT;
  end;
  SDLK_LEFTBRACKET:begin
   result:=KEYCODE_LEFTBRACKET;
  end;
  SDLK_BACKSLASH:begin
   result:=KEYCODE_BACKSLASH;
  end;
  SDLK_RIGHTBRACKET:begin
   result:=KEYCODE_RIGHTBRACKET;
  end;
  SDLK_CARET:begin
   result:=KEYCODE_CARET;
  end;
  SDLK_UNDERSCORE:begin
   result:=KEYCODE_UNDERSCORE;
  end;
  SDLK_BACKQUOTE:begin
   result:=KEYCODE_BACKQUOTE;
  end;
  SDLK_a:begin
   result:=KEYCODE_a;
  end;
  SDLK_b:begin
   result:=KEYCODE_b;
  end;
  SDLK_c:begin
   result:=KEYCODE_c;
  end;
  SDLK_d:begin
   result:=KEYCODE_d;
  end;
  SDLK_e:begin
   result:=KEYCODE_e;
  end;
  SDLK_f:begin
   result:=KEYCODE_f;
  end;
  SDLK_g:begin
   result:=KEYCODE_g;
  end;
  SDLK_h:begin
   result:=KEYCODE_h;
  end;
  SDLK_i:begin
   result:=KEYCODE_i;
  end;
  SDLK_j:begin
   result:=KEYCODE_j;
  end;
  SDLK_k:begin
   result:=KEYCODE_k;
  end;
  SDLK_l:begin
   result:=KEYCODE_l;
  end;
  SDLK_m:begin
   result:=KEYCODE_m;
  end;
  SDLK_n:begin
   result:=KEYCODE_n;
  end;
  SDLK_o:begin
   result:=KEYCODE_o;
  end;
  SDLK_p:begin
   result:=KEYCODE_p;
  end;
  SDLK_q:begin
   result:=KEYCODE_q;
  end;
  SDLK_r:begin
   result:=KEYCODE_r;
  end;
  SDLK_s:begin
   result:=KEYCODE_s;
  end;
  SDLK_t:begin
   result:=KEYCODE_t;
  end;
  SDLK_u:begin
   result:=KEYCODE_u;
  end;
  SDLK_v:begin
   result:=KEYCODE_v;
  end;
  SDLK_w:begin
   result:=KEYCODE_w;
  end;
  SDLK_x:begin
   result:=KEYCODE_x;
  end;
  SDLK_y:begin
   result:=KEYCODE_y;
  end;
  SDLK_z:begin
   result:=KEYCODE_z;
  end;
  SDLK_DELETE:begin
   result:=KEYCODE_DELETE;
  end;
  SDLK_F1:begin
   result:=KEYCODE_F1;
  end;
  SDLK_F2:begin
   result:=KEYCODE_F2;
  end;
  SDLK_F3:begin
   result:=KEYCODE_F3;
  end;
  SDLK_F4:begin
   result:=KEYCODE_F4;
  end;
  SDLK_F5:begin
   result:=KEYCODE_F5;
  end;
  SDLK_F6:begin
   result:=KEYCODE_F6;
  end;
  SDLK_F7:begin
   result:=KEYCODE_F7;
  end;
  SDLK_F8:begin
   result:=KEYCODE_F8;
  end;
  SDLK_F9:begin
   result:=KEYCODE_F9;
  end;
  SDLK_F10:begin
   result:=KEYCODE_F10;
  end;
  SDLK_F11:begin
   result:=KEYCODE_F11;
  end;
  SDLK_F12:begin
   result:=KEYCODE_F12;
  end;
  SDLK_F13:begin
   result:=KEYCODE_F13;
  end;
  SDLK_F14:begin
   result:=KEYCODE_F14;
  end;
  SDLK_F15:begin
   result:=KEYCODE_F15;
  end;
  SDLK_F16:begin
   result:=KEYCODE_F16;
  end;
  SDLK_F17:begin
   result:=KEYCODE_F17;
  end;
  SDLK_F18:begin
   result:=KEYCODE_F18;
  end;
  SDLK_F19:begin
   result:=KEYCODE_F19;
  end;
  SDLK_F20:begin
   result:=KEYCODE_F20;
  end;
  SDLK_F21:begin
   result:=KEYCODE_F21;
  end;
  SDLK_F22:begin
   result:=KEYCODE_F22;
  end;
  SDLK_F23:begin
   result:=KEYCODE_F23;
  end;
  SDLK_F24:begin
   result:=KEYCODE_F24;
  end;
  SDLK_KP0:begin
   result:=KEYCODE_KP0;
  end;
  SDLK_KP1:begin
   result:=KEYCODE_KP1;
  end;
  SDLK_KP2:begin
   result:=KEYCODE_KP2;
  end;
  SDLK_KP3:begin
   result:=KEYCODE_KP3;
  end;
  SDLK_KP4:begin
   result:=KEYCODE_KP4;
  end;
  SDLK_KP5:begin
   result:=KEYCODE_KP5;
  end;
  SDLK_KP6:begin
   result:=KEYCODE_KP6;
  end;
  SDLK_KP7:begin
   result:=KEYCODE_KP7;
  end;
  SDLK_KP8:begin
   result:=KEYCODE_KP8;
  end;
  SDLK_KP9:begin
   result:=KEYCODE_KP9;
  end;
  SDLK_KP_PERIOD:begin
   result:=KEYCODE_KP_PERIOD;
  end;
  SDLK_KP_DIVIDE:begin
   result:=KEYCODE_KP_DIVIDE;
  end;
  SDLK_KP_MULTIPLY:begin
   result:=KEYCODE_KP_MULTIPLY;
  end;
  SDLK_KP_MINUS:begin
   result:=KEYCODE_KP_MINUS;
  end;
  SDLK_KP_PLUS:begin
   result:=KEYCODE_KP_PLUS;
  end;
  SDLK_KP_ENTER:begin
   result:=KEYCODE_KP_ENTER;
  end;
  SDLK_KP_EQUALS:begin
   result:=KEYCODE_KP_EQUALS;
  end;
  SDLK_UP:begin
   result:=KEYCODE_UP;
  end;
  SDLK_DOWN:begin
   result:=KEYCODE_DOWN;
  end;
  SDLK_RIGHT:begin
   result:=KEYCODE_RIGHT;
  end;
  SDLK_LEFT:begin
   result:=KEYCODE_LEFT;
  end;
  SDLK_INSERT:begin
   result:=KEYCODE_INSERT;
  end;
  SDLK_HOME:begin
   result:=KEYCODE_HOME;
  end;
  SDLK_END:begin
   result:=KEYCODE_END;
  end;
  SDLK_PAGEUP:begin
   result:=KEYCODE_PAGEUP;
  end;
  SDLK_PAGEDOWN:begin
   result:=KEYCODE_PAGEDOWN;
  end;
  SDLK_CAPSLOCK:begin
   result:=KEYCODE_CAPSLOCK;
  end;
  SDLK_NUMLOCK:begin
   result:=KEYCODE_NUMLOCK;
  end;
  SDLK_SCROLLOCK:begin
   result:=KEYCODE_SCROLLOCK;
  end;
  SDLK_RSHIFT:begin
   result:=KEYCODE_RSHIFT;
  end;
  SDLK_LSHIFT:begin
   result:=KEYCODE_LSHIFT;
  end;
  SDLK_RCTRL:begin
   result:=KEYCODE_RCTRL;
  end;
  SDLK_LCTRL:begin
   result:=KEYCODE_LCTRL;
  end;
  SDLK_RALT:begin
   result:=KEYCODE_RALT;
  end;
  SDLK_LALT:begin
   result:=KEYCODE_LALT;
  end;
  SDLK_MODE:begin
   result:=KEYCODE_MODE;
  end;
  SDLK_HELP:begin
   result:=KEYCODE_HELP;
  end;
  SDLK_PRINTSCREEN:begin
   result:=KEYCODE_PRINTSCREEN;
  end;
  SDLK_SYSREQ:begin
   result:=KEYCODE_SYSREQ;
  end;
  SDLK_MENU:begin
   result:=KEYCODE_MENU;
  end;
  SDLK_POWER:begin
   result:=KEYCODE_POWER;
  end;
  SDLK_APPLICATION:begin
   result:=KEYCODE_APPLICATION;
  end;
  SDLK_SELECT:begin
   result:=KEYCODE_SELECT;
  end;
  SDLK_STOP:begin
   result:=KEYCODE_STOP;
  end;
  SDLK_AGAIN:begin
   result:=KEYCODE_AGAIN;
  end;
  SDLK_UNDO:begin
   result:=KEYCODE_UNDO;
  end;
  SDLK_CUT:begin
   result:=KEYCODE_CUT;
  end;
  SDLK_COPY:begin
   result:=KEYCODE_COPY;
  end;
  SDLK_PASTE:begin
   result:=KEYCODE_PASTE;
  end;
  SDLK_FIND:begin
   result:=KEYCODE_FIND;
  end;
  SDLK_MUTE:begin
   result:=KEYCODE_MUTE;
  end;
  SDLK_VOLUMEUP:begin
   result:=KEYCODE_VOLUMEUP;
  end;
  SDLK_VOLUMEDOWN:begin
   result:=KEYCODE_VOLUMEDOWN;
  end;
  SDLK_KP_EQUALSAS400:begin
   result:=KEYCODE_KP_EQUALSAS400;
  end;
  SDLK_ALTERASE:begin
   result:=KEYCODE_ALTERASE;
  end;
  SDLK_CANCEL:begin
   result:=KEYCODE_CANCEL;
  end;
  SDLK_CLEAR:begin
   result:=KEYCODE_CLEAR;
  end;
  SDLK_PRIOR:begin
   result:=KEYCODE_PRIOR;
  end;
  SDLK_RETURN2:begin
   result:=KEYCODE_RETURN2;
  end;
  SDLK_SEPARATOR:begin
   result:=KEYCODE_SEPARATOR;
  end;
  SDLK_OUT:begin
   result:=KEYCODE_OUT;
  end;
  SDLK_OPER:begin
   result:=KEYCODE_OPER;
  end;
  SDLK_CLEARAGAIN:begin
   result:=KEYCODE_CLEARAGAIN;
  end;
  SDLK_CRSEL:begin
   result:=KEYCODE_CRSEL;
  end;
  SDLK_EXSEL:begin
   result:=KEYCODE_EXSEL;
  end;
  SDLK_KP_00:begin
   result:=KEYCODE_KP_00;
  end;
  SDLK_KP_000:begin
   result:=KEYCODE_KP_000;
  end;
  SDLK_THOUSANDSSEPARATOR:begin
   result:=KEYCODE_THOUSANDSSEPARATOR;
  end;
  SDLK_DECIMALSEPARATOR:begin
   result:=KEYCODE_DECIMALSEPARATOR;
  end;
  SDLK_CURRENCYUNIT:begin
   result:=KEYCODE_CURRENCYUNIT;
  end;
  SDLK_CURRENCYSUBUNIT:begin
   result:=KEYCODE_CURRENCYSUBUNIT;
  end;
  SDLK_KP_LEFTPAREN:begin
   result:=KEYCODE_KP_LEFTPAREN;
  end;
  SDLK_KP_RIGHTPAREN:begin
   result:=KEYCODE_KP_RIGHTPAREN;
  end;
  SDLK_KP_LEFTBRACE:begin
   result:=KEYCODE_KP_LEFTBRACE;
  end;
  SDLK_KP_RIGHTBRACE:begin
   result:=KEYCODE_KP_RIGHTBRACE;
  end;
  SDLK_KP_TAB:begin
   result:=KEYCODE_KP_TAB;
  end;
  SDLK_KP_BACKSPACE:begin
   result:=KEYCODE_KP_BACKSPACE;
  end;
  SDLK_KP_A:begin
   result:=KEYCODE_KP_A;
  end;
  SDLK_KP_B:begin
   result:=KEYCODE_KP_B;
  end;
  SDLK_KP_C:begin
   result:=KEYCODE_KP_C;
  end;
  SDLK_KP_D:begin
   result:=KEYCODE_KP_D;
  end;
  SDLK_KP_E:begin
   result:=KEYCODE_KP_E;
  end;
  SDLK_KP_F:begin
   result:=KEYCODE_KP_F;
  end;
  SDLK_KP_XOR:begin
   result:=KEYCODE_KP_XOR;
  end;
  SDLK_KP_POWER:begin
   result:=KEYCODE_KP_POWER;
  end;
  SDLK_KP_PERCENT:begin
   result:=KEYCODE_KP_PERCENT;
  end;
  SDLK_KP_LESS:begin
   result:=KEYCODE_KP_LESS;
  end;
  SDLK_KP_GREATER:begin
   result:=KEYCODE_KP_GREATER;
  end;
  SDLK_KP_AMPERSAND:begin
   result:=KEYCODE_KP_AMPERSAND;
  end;
  SDLK_KP_DBLAMPERSAND:begin
   result:=KEYCODE_KP_DBLAMPERSAND;
  end;
  SDLK_KP_VERTICALBAR:begin
   result:=KEYCODE_KP_VERTICALBAR;
  end;
  SDLK_KP_DBLVERTICALBAR:begin
   result:=KEYCODE_KP_DBLVERTICALBAR;
  end;
  SDLK_KP_COLON:begin
   result:=KEYCODE_KP_COLON;
  end;
  SDLK_KP_HASH:begin
   result:=KEYCODE_KP_HASH;
  end;
  SDLK_KP_SPACE:begin
   result:=KEYCODE_KP_SPACE;
  end;
  SDLK_KP_AT:begin
   result:=KEYCODE_KP_AT;
  end;
  SDLK_KP_EXCLAM:begin
   result:=KEYCODE_KP_EXCLAM;
  end;
  SDLK_KP_MEMSTORE:begin
   result:=KEYCODE_KP_MEMSTORE;
  end;
  SDLK_KP_MEMRECALL:begin
   result:=KEYCODE_KP_MEMRECALL;
  end;
  SDLK_KP_MEMCLEAR:begin
   result:=KEYCODE_KP_MEMCLEAR;
  end;
  SDLK_KP_MEMADD:begin
   result:=KEYCODE_KP_MEMADD;
  end;
  SDLK_KP_MEMSUBTRACT:begin
   result:=KEYCODE_KP_MEMSUBTRACT;
  end;
  SDLK_KP_MEMMULTIPLY:begin
   result:=KEYCODE_KP_MEMMULTIPLY;
  end;
  SDLK_KP_MEMDIVIDE:begin
   result:=KEYCODE_KP_MEMDIVIDE;
  end;
  SDLK_KP_PLUSMINUS:begin
   result:=KEYCODE_KP_PLUSMINUS;
  end;
  SDLK_KP_CLEAR:begin
   result:=KEYCODE_KP_CLEAR;
  end;
  SDLK_KP_CLEARENTRY:begin
   result:=KEYCODE_KP_CLEARENTRY;
  end;
  SDLK_KP_BINARY:begin
   result:=KEYCODE_KP_BINARY;
  end;
  SDLK_KP_OCTAL:begin
   result:=KEYCODE_KP_OCTAL;
  end;
  SDLK_KP_DECIMAL:begin
   result:=KEYCODE_KP_DECIMAL;
  end;
  SDLK_KP_HEXADECIMAL:begin
   result:=KEYCODE_KP_HEXADECIMAL;
  end;
  SDLK_LGUI:begin
   result:=KEYCODE_LGUI;
  end;
  SDLK_RGUI:begin
   result:=KEYCODE_RGUI;
  end;
  SDLK_AUDIONEXT:begin
   result:=KEYCODE_AUDIONEXT;
  end;
  SDLK_AUDIOPREV:begin
   result:=KEYCODE_AUDIOPREV;
  end;
  SDLK_AUDIOSTOP:begin
   result:=KEYCODE_AUDIOSTOP;
  end;
  SDLK_AUDIOPLAY:begin
   result:=KEYCODE_AUDIOPLAY;
  end;
  SDLK_AUDIOMUTE:begin
   result:=KEYCODE_AUDIOMUTE;
  end;
  SDLK_MEDIASELECT:begin
   result:=KEYCODE_MEDIASELECT;
  end;
  SDLK_WWW:begin
   result:=KEYCODE_WWW;
  end;
  SDLK_MAIL:begin
   result:=KEYCODE_MAIL;
  end;
  SDLK_CALCULATOR:begin
   result:=KEYCODE_CALCULATOR;
  end;
  SDLK_COMPUTER:begin
   result:=KEYCODE_COMPUTER;
  end;
  SDLK_AC_SEARCH:begin
   result:=KEYCODE_AC_SEARCH;
  end;
  SDLK_AC_HOME:begin
   result:=KEYCODE_AC_HOME;
  end;
  SDLK_AC_BACK:begin
   result:=KEYCODE_AC_BACK;
  end;
  SDLK_AC_FORWARD:begin
   result:=KEYCODE_AC_FORWARD;
  end;
  SDLK_AC_STOP:begin
   result:=KEYCODE_AC_STOP;
  end;
  SDLK_AC_REFRESH:begin
   result:=KEYCODE_AC_REFRESH;
  end;
  SDLK_AC_BOOKMARKS:begin
   result:=KEYCODE_AC_BOOKMARKS;
  end;
  SDLK_BRIGHTNESSDOWN:begin
   result:=KEYCODE_BRIGHTNESSDOWN;
  end;
  SDLK_BRIGHTNESSUP:begin
   result:=KEYCODE_BRIGHTNESSUP;
  end;
  SDLK_DISPLAYSWITCH:begin
   result:=KEYCODE_DISPLAYSWITCH;
  end;
  SDLK_KBDILLUMTOGGLE:begin
   result:=KEYCODE_KBDILLUMTOGGLE;
  end;
  SDLK_KBDILLUMDOWN:begin
   result:=KEYCODE_KBDILLUMDOWN;
  end;
  SDLK_KBDILLUMUP:begin
   result:=KEYCODE_KBDILLUMUP;
  end;
  SDLK_EJECT:begin
   result:=KEYCODE_EJECT;
  end;
  SDLK_SLEEP:begin
   result:=KEYCODE_SLEEP;
  end;
  else begin
   case aScanCode of
    SDL_SCANCODE_INTERNATIONAL1:begin
     result:=KEYCODE_INTERNATIONAL1;
    end;
    SDL_SCANCODE_INTERNATIONAL2:begin
     result:=KEYCODE_INTERNATIONAL2;
    end;
    SDL_SCANCODE_INTERNATIONAL3:begin
     result:=KEYCODE_INTERNATIONAL3;
    end;
    SDL_SCANCODE_INTERNATIONAL4:begin
     result:=KEYCODE_INTERNATIONAL4;
    end;
    SDL_SCANCODE_INTERNATIONAL5:begin
     result:=KEYCODE_INTERNATIONAL5;
    end;
    SDL_SCANCODE_INTERNATIONAL6:begin
     result:=KEYCODE_INTERNATIONAL6;
    end;
    SDL_SCANCODE_INTERNATIONAL7:begin
     result:=KEYCODE_INTERNATIONAL7;
    end;
    SDL_SCANCODE_INTERNATIONAL8:begin
     result:=KEYCODE_INTERNATIONAL8;
    end;
    SDL_SCANCODE_INTERNATIONAL9:begin
     result:=KEYCODE_INTERNATIONAL9;
    end;
    SDL_SCANCODE_LANG1:begin
     result:=KEYCODE_LANG1;
    end;
    SDL_SCANCODE_LANG2:begin
     result:=KEYCODE_LANG2;
    end;
    SDL_SCANCODE_LANG3:begin
     result:=KEYCODE_LANG3;
    end;
    SDL_SCANCODE_LANG4:begin
     result:=KEYCODE_LANG4;
    end;
    SDL_SCANCODE_LANG5:begin
     result:=KEYCODE_LANG5;
    end;
    SDL_SCANCODE_LANG6:begin
     result:=KEYCODE_LANG6;
    end;
    SDL_SCANCODE_LANG7:begin
     result:=KEYCODE_LANG7;
    end;
    SDL_SCANCODE_LANG8:begin
     result:=KEYCODE_LANG8;
    end;
    SDL_SCANCODE_LANG9:begin
     result:=KEYCODE_LANG9;
    end;
    SDL_SCANCODE_LOCKINGCAPSLOCK:begin
     result:=KEYCODE_LOCKINGCAPSLOCK;
    end;
    SDL_SCANCODE_LOCKINGNUMLOCK:begin
     result:=KEYCODE_LOCKINGNUMLOCK;
    end;
    SDL_SCANCODE_LOCKINGSCROLLLOCK:begin
     result:=KEYCODE_LOCKINGSCROLLLOCK;
    end;
    SDL_SCANCODE_NONUSBACKSLASH:begin
     result:=KEYCODE_NONUSBACKSLASH;
    end;
    SDL_SCANCODE_NONUSHASH:begin
     result:=KEYCODE_NONUSHASH;
    end;
    else begin
     result:=KEYCODE_UNKNOWN;
    end;
   end;
  end;
 end;
end;

function TpvApplicationInput.TranslateSDLKeyModifier(const aKeyModifier:TpvInt32):TpvApplicationInputKeyModifiers;
begin
 result:=[];
 if (aKeyModifier and PasVulkan.SDL2.KMOD_LSHIFT)<>0 then begin
  Include(result,TpvApplicationInputKeyModifier.LSHIFT);
 end;
 if (aKeyModifier and PasVulkan.SDL2.KMOD_RSHIFT)<>0 then begin
  Include(result,TpvApplicationInputKeyModifier.RSHIFT);
  Include(result,TpvApplicationInputKeyModifier.SHIFT);
 end;
 if (aKeyModifier and PasVulkan.SDL2.KMOD_LCTRL)<>0 then begin
  Include(result,TpvApplicationInputKeyModifier.LCTRL);
  Include(result,TpvApplicationInputKeyModifier.CTRL);
 end;
 if (aKeyModifier and PasVulkan.SDL2.KMOD_RCTRL)<>0 then begin
  Include(result,TpvApplicationInputKeyModifier.RCTRL);
  Include(result,TpvApplicationInputKeyModifier.CTRL);
 end;
 if (aKeyModifier and PasVulkan.SDL2.KMOD_LALT)<>0 then begin
  Include(result,TpvApplicationInputKeyModifier.LALT);
  Include(result,TpvApplicationInputKeyModifier.ALT);
 end;
 if (aKeyModifier and PasVulkan.SDL2.KMOD_RALT)<>0 then begin
  Include(result,TpvApplicationInputKeyModifier.RALT);
  Include(result,TpvApplicationInputKeyModifier.ALT);
 end;
 if (aKeyModifier and PasVulkan.SDL2.KMOD_LMETA)<>0 then begin
  Include(result,TpvApplicationInputKeyModifier.LMETA);
  Include(result,TpvApplicationInputKeyModifier.META);
 end;
 if (aKeyModifier and PasVulkan.SDL2.KMOD_RMETA)<>0 then begin
  Include(result,TpvApplicationInputKeyModifier.RMETA);
  Include(result,TpvApplicationInputKeyModifier.META);
 end;
 if (aKeyModifier and PasVulkan.SDL2.KMOD_NUM)<>0 then begin
  Include(result,TpvApplicationInputKeyModifier.NUM);
 end;
 if (aKeyModifier and PasVulkan.SDL2.KMOD_CAPS)<>0 then begin
  Include(result,TpvApplicationInputKeyModifier.CAPS);
 end;
 if (aKeyModifier and PasVulkan.SDL2.KMOD_MODE)<>0 then begin
  Include(result,TpvApplicationInputKeyModifier.MODE);
 end;
 if (aKeyModifier and PasVulkan.SDL2.KMOD_RESERVED)<>0 then begin
  Include(result,TpvApplicationInputKeyModifier.RESERVED);
 end;
end;

const SDL_KEYTYPED=$30000;

{$else}
{$ifend}

procedure TpvApplicationInput.AddEvent(const aEvent:TpvApplicationEvent);
begin
 if fEventCount>=length(fEvents) then begin
  SetLength(fEvents,(fEventCount+1)*2);
  SetLength(fEventTimes,(fEventCount+1)*2);
 end;
 fEvents[fEventCount]:=aEvent;
 fEventTimes[fEventCount]:=pvApplication.fHighResolutionTimer.ToNanoseconds(pvApplication.fHighResolutionTimer.GetTime);
 inc(fEventCount);
end;

procedure TpvApplicationInput.ProcessEvents;
{$if defined(PasVulkanUseSDL2)}
var Index,PointerID,KeyCode,Position:TpvInt32;
    KeyModifiers:TpvApplicationInputKeyModifiers;
    Event:PSDL_Event;
    OK:boolean;
begin
 fCriticalSection.Acquire;
 try
  fJustTouched:=false;
  if fEventCount>0 then begin
   for Index:=0 to fEventCount-1 do begin
    Event:=@fEvents[Index];
    fCurrentEventTime:=fEventTimes[fEventCount];
    case Event^.type_ of
     SDL_KEYDOWN,SDL_KEYUP,SDL_KEYTYPED:begin
      KeyCode:=TranslateSDLKeyCode(Event^.key.keysym.sym,Event^.key.keysym.scancode);
      KeyModifiers:=TranslateSDLKeyModifier(Event^.key.keysym.modifier);
      case Event^.type_ of
       SDL_KEYDOWN:begin
        fKeyDown[KeyCode and $ffff]:=true;
        inc(fKeyDownCount);
        fJustKeyDown[KeyCode and $ffff]:=true;
        if (not pvApplication.KeyEvent(TpvApplicationInputKeyEvent.Create(TpvApplicationInputKeyEventType.DOWN,KeyCode,KeyModifiers))) and assigned(fProcessor) then begin
         fProcessor.KeyEvent(TpvApplicationInputKeyEvent.Create(TpvApplicationInputKeyEventType.DOWN,KeyCode,KeyModifiers));
        end;
       end;
       SDL_KEYUP:begin
        fKeyDown[KeyCode and $ffff]:=false;
        if fKeyDownCount>0 then begin
         dec(fKeyDownCount);
        end;
        fJustKeyDown[KeyCode and $ffff]:=false;
        if (not pvApplication.KeyEvent(TpvApplicationInputKeyEvent.Create(TpvApplicationInputKeyEventType.UP,KeyCode,KeyModifiers))) and assigned(fProcessor) then begin
         fProcessor.KeyEvent(TpvApplicationInputKeyEvent.Create(TpvApplicationInputKeyEventType.UP,KeyCode,KeyModifiers));
        end;
       end;
       SDL_KEYTYPED:begin
        if (not pvApplication.KeyEvent(TpvApplicationInputKeyEvent.Create(TpvApplicationInputKeyEventType.TYPED,KeyCode,KeyModifiers))) and assigned(fProcessor) then begin
         fProcessor.KeyEvent(TpvApplicationInputKeyEvent.Create(TpvApplicationInputKeyEventType.TYPED,KeyCode,KeyModifiers));
        end;
       end;
      end;
     end;
     SDL_TEXTINPUT:begin
      KeyModifiers:=[];
      Position:=0;
      while Position<length(Event^.tedit.text) do begin
       KeyCode:=PUCUUTF8PtrCodeUnitGetCharAndIncFallback(PAnsiChar(TpvPointer(@Event^.tedit.text[0])),length(Event^.tedit.text),Position);
       case KeyCode of
        0:begin
         break;
        end;
        else begin
         if (not pvApplication.KeyEvent(TpvApplicationInputKeyEvent.Create(TpvApplicationInputKeyEventType.UNICODE,KeyCode,KeyModifiers))) and assigned(fProcessor) then begin
          fProcessor.KeyEvent(TpvApplicationInputKeyEvent.Create(TpvApplicationInputKeyEventType.UNICODE,KeyCode,KeyModifiers));
         end;
        end;
       end;
      end;
     end;
     SDL_MOUSEMOTION:begin
      KeyModifiers:=GetKeyModifiers;
      fMouseX:=Event^.motion.x;
      fMouseY:=Event^.motion.y;
      fMouseDeltaX:=Event^.motion.xrel;
      fMouseDeltaY:=Event^.motion.yrel;
      OK:=pvApplication.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.MOTION,
                                                                            TpvVector2.Create(Event^.motion.x,Event^.motion.y),
                                                                            TpvVector2.Create(Event^.motion.xrel,Event^.motion.yrel),
                                                                            ord(fMouseDown<>[]) and 1,
                                                                            0,
                                                                            fMouseDown,
                                                                            KeyModifiers));
      if assigned(fProcessor) and not OK then begin
       fProcessor.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.MOTION,
                                                                      TpvVector2.Create(Event^.motion.x,Event^.motion.y),
                                                                      TpvVector2.Create(Event^.motion.xrel,Event^.motion.yrel),
                                                                      ord(fMouseDown<>[]) and 1,
                                                                      0,
                                                                      fMouseDown,
                                                                      KeyModifiers));
      end;
     end;
     SDL_MOUSEBUTTONDOWN:begin
      KeyModifiers:=GetKeyModifiers;
      fMaxPointerID:=max(fMaxPointerID,0);
 {    fMouseDeltaX:=Event^.button.x-fMouseX;
      fMouseDeltaY:=Event^.button.y-fMouseY;}
      fMouseX:=Event^.button.x;
      fMouseY:=Event^.button.y;
      case Event^.button.button of
       SDL_BUTTON_LEFT:begin
        Include(fMouseDown,TpvApplicationInputPointerButton.LEFT);
        Include(fMouseJustDown,TpvApplicationInputPointerButton.LEFT);
        fJustTouched:=true;
        if (not pvApplication.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.DOWN,TpvVector2.Create(Event^.motion.x,Event^.motion.y),1.0,0,TpvApplicationInputPointerButton.LEFT,fMouseDown,KeyModifiers))) and assigned(fProcessor) then begin
         fProcessor.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.DOWN,TpvVector2.Create(Event^.motion.x,Event^.motion.y),1.0,0,TpvApplicationInputPointerButton.LEFT,fMouseDown,KeyModifiers));
        end;
       end;
       SDL_BUTTON_RIGHT:begin
        Include(fMouseDown,TpvApplicationInputPointerButton.RIGHT);
        Include(fMouseJustDown,TpvApplicationInputPointerButton.RIGHT);
        fJustTouched:=true;
        if (not pvApplication.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.DOWN,TpvVector2.Create(Event^.motion.x,Event^.motion.y),1.0,0,TpvApplicationInputPointerButton.RIGHT,fMouseDown,KeyModifiers))) and assigned(fProcessor) then begin
         fProcessor.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.DOWN,TpvVector2.Create(Event^.motion.x,Event^.motion.y),1.0,0,TpvApplicationInputPointerButton.RIGHT,fMouseDown,KeyModifiers));
        end;
       end;
       SDL_BUTTON_MIDDLE:begin
        Include(fMouseDown,TpvApplicationInputPointerButton.MIDDLE);
        Include(fMouseJustDown,TpvApplicationInputPointerButton.MIDDLE);
        fJustTouched:=true;
        if (not pvApplication.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.DOWN,TpvVector2.Create(Event^.motion.x,Event^.motion.y),1.0,0,TpvApplicationInputPointerButton.MIDDLE,fMouseDown,KeyModifiers))) and assigned(fProcessor) then begin
         fProcessor.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.DOWN,TpvVector2.Create(Event^.motion.x,Event^.motion.y),1.0,0,TpvApplicationInputPointerButton.MIDDLE,fMouseDown,KeyModifiers));
        end;
       end;
      end;
     end;
     SDL_MOUSEBUTTONUP:begin
      KeyModifiers:=GetKeyModifiers;
      fMaxPointerID:=max(fMaxPointerID,0);
 {    fMouseDeltaX:=Event^.button.x-fMouseX;
      fMouseDeltaY:=Event^.button.y-fMouseY;}
      fMouseX:=Event^.button.x;
      fMouseY:=Event^.button.y;
      case Event^.button.button of
       SDL_BUTTON_LEFT:begin
        Exclude(fMouseDown,TpvApplicationInputPointerButton.LEFT);
        Exclude(fMouseJustDown,TpvApplicationInputPointerButton.LEFT);
        if (not pvApplication.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.UP,TpvVector2.Create(Event^.motion.x,Event^.motion.y),1.0,0,TpvApplicationInputPointerButton.LEFT,fMouseDown,KeyModifiers))) and assigned(fProcessor) then begin
         fProcessor.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.UP,TpvVector2.Create(Event^.motion.x,Event^.motion.y),1.0,0,TpvApplicationInputPointerButton.LEFT,fMouseDown,KeyModifiers));
        end;
       end;
       SDL_BUTTON_RIGHT:begin
        Exclude(fMouseDown,TpvApplicationInputPointerButton.RIGHT);
        Exclude(fMouseJustDown,TpvApplicationInputPointerButton.RIGHT);
        if (not pvApplication.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.UP,TpvVector2.Create(Event^.motion.x,Event^.motion.y),1.0,0,TpvApplicationInputPointerButton.RIGHT,fMouseDown,KeyModifiers))) and assigned(fProcessor) then begin
         fProcessor.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.UP,TpvVector2.Create(Event^.motion.x,Event^.motion.y),1.0,0,TpvApplicationInputPointerButton.RIGHT,fMouseDown,KeyModifiers));
        end;
       end;
       SDL_BUTTON_MIDDLE:begin
        Exclude(fMouseDown,TpvApplicationInputPointerButton.MIDDLE);
        Exclude(fMouseJustDown,TpvApplicationInputPointerButton.MIDDLE);
        if (not pvApplication.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.UP,TpvVector2.Create(Event^.motion.x,Event^.motion.y),1.0,0,TpvApplicationInputPointerButton.MIDDLE,fMouseDown,KeyModifiers))) and assigned(fProcessor) then begin
         fProcessor.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.UP,TpvVector2.Create(Event^.motion.x,Event^.motion.y),1.0,0,TpvApplicationInputPointerButton.MIDDLE,fMouseDown,KeyModifiers));
        end;
       end;
      end;
     end;
     SDL_MOUSEWHEEL:begin
      if (not pvApplication.Scrolled(TpvVector2.Create(Event^.wheel.x,Event^.wheel.y))) and assigned(fProcessor) then begin
       fProcessor.Scrolled(TpvVector2.Create(Event^.wheel.x,Event^.wheel.y));
      end;
     end;
     SDL_FINGERMOTION:begin
      KeyModifiers:=GetKeyModifiers;
      PointerID:=Event^.tfinger.fingerId and $ffff;
      fMaxPointerID:=max(fMaxPointerID,PointerID+1);
      fPointerX[PointerID]:=Event^.tfinger.x*pvApplication.fWidth;
      fPointerY[PointerID]:=Event^.tfinger.y*pvApplication.fHeight;
      fPointerPressure[PointerID]:=Event^.tfinger.pressure;
      fPointerDeltaX[PointerID]:=Event^.tfinger.dx*pvApplication.fWidth;
      fPointerDeltaY[PointerID]:=Event^.tfinger.dy*pvApplication.fHeight;
      if (not pvApplication.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.MOTION,TpvVector2.Create(fPointerX[PointerID],fPointerY[PointerID]),TpvVector2.Create(fPointerDeltaX[PointerID],fPointerDeltaY[PointerID]),fPointerPressure[PointerID],PointerID+1,fPointerDown[PointerID],KeyModifiers))) and assigned(fProcessor) then begin
       fProcessor.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.MOTION,TpvVector2.Create(fPointerX[PointerID],fPointerY[PointerID]),TpvVector2.Create(fPointerDeltaX[PointerID],fPointerDeltaY[PointerID]),fPointerPressure[PointerID],PointerID+1,fPointerDown[PointerID],KeyModifiers));
      end;
     end;
     SDL_FINGERDOWN:begin
      KeyModifiers:=GetKeyModifiers;
      inc(fPointerDownCount);
      PointerID:=Event^.tfinger.fingerId and $ffff;
      fMaxPointerID:=max(fMaxPointerID,PointerID+1);
      fPointerX[PointerID]:=Event^.tfinger.x*pvApplication.fWidth;
      fPointerY[PointerID]:=Event^.tfinger.y*pvApplication.fHeight;
      fPointerPressure[PointerID]:=Event^.tfinger.pressure;
      fPointerDeltaX[PointerID]:=Event^.tfinger.dx*pvApplication.fWidth;
      fPointerDeltaY[PointerID]:=Event^.tfinger.dy*pvApplication.fHeight;
      Include(fPointerDown[PointerID],TpvApplicationInputPointerButton.LEFT);
      Include(fPointerJustDown[PointerID],TpvApplicationInputPointerButton.LEFT);
      fJustTouched:=true;
      if (not pvApplication.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.DOWN,TpvVector2.Create(fPointerX[PointerID],fPointerY[PointerID]),fPointerPressure[PointerID],PointerID+1,TpvApplicationInputPointerButton.LEFT,fPointerDown[PointerID],KeyModifiers))) and assigned(fProcessor) then begin
       fProcessor.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.DOWN,TpvVector2.Create(fPointerX[PointerID],fPointerY[PointerID]),fPointerPressure[PointerID],PointerID+1,TpvApplicationInputPointerButton.LEFT,fPointerDown[PointerID],KeyModifiers));
      end;
     end;
     SDL_FINGERUP:begin
      KeyModifiers:=GetKeyModifiers;
      if fPointerDownCount>0 then begin
       dec(fPointerDownCount);
      end;
      PointerID:=Event^.tfinger.fingerId and $ffff;
      fMaxPointerID:=max(fMaxPointerID,PointerID+1);
      fPointerX[PointerID]:=Event^.tfinger.x*pvApplication.fWidth;
      fPointerY[PointerID]:=Event^.tfinger.y*pvApplication.fHeight;
      fPointerPressure[PointerID]:=Event^.tfinger.pressure;
      fPointerDeltaX[PointerID]:=Event^.tfinger.dx*pvApplication.fWidth;
      fPointerDeltaY[PointerID]:=Event^.tfinger.dy*pvApplication.fHeight;
      Exclude(fPointerDown[PointerID],TpvApplicationInputPointerButton.LEFT);
      Exclude(fPointerJustDown[PointerID],TpvApplicationInputPointerButton.LEFT);
      if (not pvApplication.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.UP,TpvVector2.Create(fPointerX[PointerID],fPointerY[PointerID]),fPointerPressure[PointerID],PointerID+1,TpvApplicationInputPointerButton.LEFT,fPointerDown[PointerID],KeyModifiers))) and assigned(fProcessor) then begin
       fProcessor.PointerEvent(TpvApplicationInputPointerEvent.Create(TpvApplicationInputPointerEventType.UP,TpvVector2.Create(fPointerX[PointerID],fPointerY[PointerID]),fPointerPressure[PointerID],PointerID+1,TpvApplicationInputPointerButton.LEFT,fPointerDown[PointerID],KeyModifiers));
      end;
     end;
    end;
   end;
  end;
 finally
  fCriticalSection.Release;
  fEventCount:=0;
 end;
end;
{$else}
begin
 fEventCount:=0;
end;
{$ifend}

function TpvApplicationInput.GetAccelerometerX:TpvFloat;
begin
 result:=0.0;
end;

function TpvApplicationInput.GetAccelerometerY:TpvFloat;
begin
 result:=0.0;
end;

function TpvApplicationInput.GetAccelerometerZ:TpvFloat;
begin
 result:=0.0;
end;

function TpvApplicationInput.GetOrientationAzimuth:TpvFloat;
begin
 result:=0.0;
end;

function TpvApplicationInput.GetOrientationPitch:TpvFloat;
begin
 result:=0.0;
end;

function TpvApplicationInput.GetOrientationRoll:TpvFloat;
begin
 result:=0.0;
end;

function TpvApplicationInput.GetMaxPointerID:TpvInt32;
begin
 fCriticalSection.Acquire;
 try
  result:=fMaxPointerID;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.GetPointerX(const aPointerID:TpvInt32=0):TpvFloat;
begin
 fCriticalSection.Acquire;
 try
  if aPointerID=0 then begin
   result:=fMouseX;
  end else if (aPointerID>0) and (aPointerID<=$10000) then begin
   result:=fPointerX[aPointerID-1];
  end else begin
   result:=0.0;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.GetPointerDeltaX(const aPointerID:TpvInt32=0):TpvFloat;
begin
 fCriticalSection.Acquire;
 try
  if aPointerID=0 then begin
   result:=fMouseDeltaX;
  end else if (aPointerID>0) and (aPointerID<=$10000) then begin
   result:=fPointerDeltaX[aPointerID-1];
  end else begin
   result:=0.0;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.GetPointerY(const aPointerID:TpvInt32=0):TpvFloat;
begin
 fCriticalSection.Acquire;
 try
  if aPointerID=0 then begin
   result:=fMouseY;
  end else if (aPointerID>0) and (aPointerID<=$10000) then begin
   result:=fPointerY[aPointerID-1];
  end else begin
   result:=0.0;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.GetPointerDeltaY(const aPointerID:TpvInt32=0):TpvFloat;
begin
 fCriticalSection.Acquire;
 try
  if aPointerID=0 then begin
   result:=fMouseDeltaY;
  end else if (aPointerID>0) and (aPointerID<=$10000) then begin
   result:=fPointerDeltaY[aPointerID-1];
  end else begin
   result:=0.0;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.GetPointerPressure(const aPointerID:TpvInt32=0):TpvFloat;
begin
 fCriticalSection.Acquire;
 try
  if aPointerID=0 then begin
   result:=ord(fMouseDown<>[]) and 1;
  end else if (aPointerID>0) and (aPointerID<=$10000) then begin
   result:=fPointerPressure[aPointerID-1];
  end else begin
   result:=0.0;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.IsPointerTouched(const aPointerID:TpvInt32=0;const aButtonMask:TpvApplicationInputPointerButtons=[TpvApplicationInputPointerButton.LEFT,TpvApplicationInputPointerButton.MIDDLE,TpvApplicationInputPointerButton.RIGHT]):boolean;
begin
 fCriticalSection.Acquire;
 try
  if aPointerID=0 then begin
   result:=(fMouseDown*aButtonMask)<>[];
  end else if (aPointerID>0) and (aPointerID<=$10000) then begin
   result:=(fPointerDown[aPointerID-1]*aButtonMask)<>[];
  end else begin
   result:=false;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.IsPointerJustTouched(const aPointerID:TpvInt32=0;const aButtonMask:TpvApplicationInputPointerButtons=[TpvApplicationInputPointerButton.LEFT,TpvApplicationInputPointerButton.MIDDLE,TpvApplicationInputPointerButton.RIGHT]):boolean;
begin
 fCriticalSection.Acquire;
 try
  if aPointerID=0 then begin
   result:=(fMouseJustDown*aButtonMask)<>[];
   fMouseJustDown:=fMouseJustDown-aButtonMask;
  end else if (aPointerID>0) and (aPointerID<=$10000) then begin
   result:=(fPointerJustDown[aPointerID-1]*aButtonMask)<>[];
   fPointerJustDown[aPointerID-1]:=fPointerJustDown[aPointerID-1]-aButtonMask;
  end else begin
   result:=false;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.IsTouched:boolean;
begin
 fCriticalSection.Acquire;
 try
  result:=(fMouseDown<>[]) or (fPointerDownCount<>0);
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.JustTouched:boolean;
begin
 fCriticalSection.Acquire;
 try
  result:=fJustTouched;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.IsButtonPressed(const aButton:TpvApplicationInputPointerButton):boolean;
begin
 fCriticalSection.Acquire;
 try
  result:=aButton in fMouseDown;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.IsKeyPressed(const aKeyCode:TpvInt32):boolean;
begin
 fCriticalSection.Acquire;
 try
  case aKeyCode of
   KEYCODE_ANYKEY:begin
    result:=fKeyDownCount>0;
   end;
   $0000..$ffff:begin
    result:=fKeyDown[aKeyCode and $ffff];
   end;
   else begin
    result:=false;
   end;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.IsKeyJustPressed(const aKeyCode:TpvInt32):boolean;
begin
 fCriticalSection.Acquire;
 try
  case aKeyCode of
   $0000..$ffff:begin
    result:=fJustKeyDown[aKeyCode and $ffff];
    fJustKeyDown[aKeyCode and $ffff]:=false;
   end;
   else begin
    result:=false;
   end;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.GetKeyName(const aKeyCode:TpvInt32):TpvApplicationRawByteString;
begin
 if (aKeyCode>=low(fKeyCodeNames)) and (aKeyCode<=high(fKeyCodeNames)) then begin
  result:=fKeyCodeNames[aKeyCode];
 end else begin
  result:='';
 end;
end;

function TpvApplicationInput.GetKeyModifiers:TpvApplicationInputKeyModifiers;
begin
{$if defined(PasVulkanUseSDL2)}
 result:=TranslateSDLKeyModifier(SDL_GetModState);
{$else}
 result:=[];
{$ifend}
end;

procedure TpvApplicationInput.StartTextInput;
begin
{$if defined(PasVulkanUseSDL2)}
 SDL_StartTextInput;
{$ifend}
end;

procedure TpvApplicationInput.StopTextInput;
begin
{$if defined(PasVulkanUseSDL2)}
 SDL_StopTextInput;
{$ifend}
end;

procedure TpvApplicationInput.GetTextInput(const aCallback:TpvApplicationInputTextInputCallback;const aTitle,aText:TpvApplicationRawByteString;const aPlaceholder:TpvApplicationRawByteString='');
begin
end;

procedure TpvApplicationInput.SetOnscreenKeyboardVisible(const aVisible:boolean);
begin
end;

procedure TpvApplicationInput.Vibrate(const aMilliseconds:TpvInt32);
begin
end;

procedure TpvApplicationInput.Vibrate(const aPattern:array of TpvInt32;const aRepeats:TpvInt32);
begin
end;

procedure TpvApplicationInput.CancelVibrate;
begin
end;

procedure TpvApplicationInput.GetRotationMatrix(const aMatrix3x3:pointer);
begin
end;

function TpvApplicationInput.GetCurrentEventTime:int64;
begin
 result:=fCurrentEventTime;
end;

procedure TpvApplicationInput.SetCatchBackKey(const aCatchBack:boolean);
begin
end;

procedure TpvApplicationInput.SetCatchMenuKey(const aCatchMenu:boolean);
begin
end;

procedure TpvApplicationInput.SetInputProcessor(const aProcessor:TpvApplicationInputProcessor);
begin
 fCriticalSection.Acquire;
 try
  fProcessor:=aProcessor;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.GetInputProcessor:TpvApplicationInputProcessor;
begin
 fCriticalSection.Acquire;
 try
  result:=fProcessor;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.IsPeripheralAvailable(const aPeripheral:TpvInt32):boolean;
begin
 fCriticalSection.Acquire;
 try
  case aPeripheral of
   PERIPHERAL_HARDWAREKEYBOARD,PERIPHERAL_MULTITOUCHSCREEN:begin
    result:=true;
   end;
   PERIPHERAL_ONSCEENKEYBOARD,PERIPHERAL_ACCELEROMETER,PERIPHERAL_COMPASS,PERIPHERAL_VIBRATOR:begin
    result:=false;
   end;
   else begin
    result:=false;
   end;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.GetNativeOrientation:TpvInt32;
{$if defined(PasVulkanUseSDL2)}
var SDLDisplayMode:TSDL_DisplayMode;
begin
 if SDL_GetDesktopDisplayMode(SDL_GetWindowDisplayIndex(pvApplication.fSurfaceWindow),@SDLDisplayMode)=0 then begin
  if SDLDisplayMode.w<SDLDisplayMode.h then begin
   result:=ORIENTATION_LANDSCAPE;
  end else begin
   result:=ORIENTATION_PORTRAIT;
  end;
 end else begin
  result:=ORIENTATION_LANDSCAPE;
 end;
end;
{$else}
begin
 result:=ORIENTATION_LANDSCAPE;
end;
{$ifend}

procedure TpvApplicationInput.SetCursorCatched(const aCatched:boolean);
begin
 fCriticalSection.Acquire;
 try
  pvApplication.fCatchMouse:=aCatched;
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.IsCursorCatched:boolean;
begin
 fCriticalSection.Acquire;
 try
  result:=pvApplication.fCatchMouse;
 finally
  fCriticalSection.Release;
 end;
end;

procedure TpvApplicationInput.SetCursorPosition(const pX,pY:TpvInt32);
begin
 fCriticalSection.Acquire;
 try
{$if defined(PasVulkanUseSDL2)}
  SDL_WarpMouseInWindow(pvApplication.fSurfaceWindow,pX,pY);
{$else}
{$ifend}
 finally
  fCriticalSection.Release;
 end;
end;

function TpvApplicationInput.GetJoystickCount:TpvInt32;
begin
{$if defined(PasVulkanUseSDL2)}
 result:=SDL_NumJoysticks;
{$else}
 result:=0;
{$ifend}
end;

function TpvApplicationInput.GetJoystick(const aIndex:TpvInt32=-1):TpvApplicationJoystick;
{$if defined(PasVulkanUseSDL2)}
var ListIndex:TpvInt32;
begin
 if (aIndex>=0) and (aIndex<SDL_NumJoysticks) then begin
  result:=nil;
  for ListIndex:=0 to fJoysticks.Count-1 do begin
   if TpvApplicationJoystick(fJoysticks[ListIndex]).fIndex=aIndex then begin
    result:=TpvApplicationJoystick(fJoysticks[ListIndex]);
    exit;
   end;
  end;
 end else begin
  result:=fMainJoystick;
 end;
end;
{$else}
begin
 result:=fMainJoystick;
end;
{$ifend}

constructor TpvApplicationLifecycleListener.Create;
begin
 inherited Create;
end;

destructor TpvApplicationLifecycleListener.Destroy;
begin
 inherited Destroy;
end;

function TpvApplicationLifecycleListener.Resume:boolean;
begin
 result:=false;
end;

function TpvApplicationLifecycleListener.Pause:boolean;
begin
 result:=false;
end;

function TpvApplicationLifecycleListener.LowMemory:boolean;
begin
 result:=false;
end;

function TpvApplicationLifecycleListener.Terminate:boolean;
begin
 result:=false;
end;

constructor TpvApplicationScreen.Create;
begin
 inherited Create;
end;

destructor TpvApplicationScreen.Destroy;
begin
 inherited Destroy;
end;

procedure TpvApplicationScreen.Show;
begin
end;

procedure TpvApplicationScreen.Hide;
begin
end;

procedure TpvApplicationScreen.Resume;
begin
end;

procedure TpvApplicationScreen.Pause;
begin
end;

procedure TpvApplicationScreen.LowMemory;
begin
end;

procedure TpvApplicationScreen.Resize(const aWidth,aHeight:TpvInt32);
begin
end;

procedure TpvApplicationScreen.AfterCreateSwapChain;
begin
end;

procedure TpvApplicationScreen.BeforeDestroySwapChain;
begin
end;

function TpvApplicationScreen.HandleEvent(const aEvent:TpvApplicationEvent):boolean;
begin
 result:=false;
end;

function TpvApplicationScreen.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=false;
end;

function TpvApplicationScreen.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
begin
 result:=false;
end;

function TpvApplicationScreen.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 result:=false;
end;

function TpvApplicationScreen.CanBeParallelProcessed:boolean;
begin
 result:=false;
end;

procedure TpvApplicationScreen.Update(const aDeltaTime:TpvDouble);
begin
end;

procedure TpvApplicationScreen.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
begin
end;

procedure TpvApplicationScreen.UpdateAudio;
begin
end;

constructor TpvApplicationAssets.Create(const aVulkanApplication:TpvApplication);
begin
 inherited Create;
 fVulkanApplication:=aVulkanApplication;
{$if not defined(Android)}
{$if defined(PasVulkanAdjustDelphiWorkingDirectory)}
 fBasePath:=IncludeTrailingPathDelimiter(ExpandFileName(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'..')+'..')+'..')+'assets')));
{$elseif defined(PasVulkanUseCurrentWorkingDirectory)}
 fBasePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(ExtractFilePath(GetCurrentDir))+'assets');
{$else}
 fBasePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'assets');
{$ifend}
{$ifend}
end;

destructor TpvApplicationAssets.Destroy;
begin
 inherited Destroy;
end;

function TpvApplicationAssets.CorrectFileName(const aFileName:string):string;
begin
 result:=StringReplace(StringReplace({$ifndef Android}fBasePath+{$endif}aFileName,'/',PathDelim,[rfReplaceAll]),'\',PathDelim,[rfReplaceAll]);
end;

function TpvApplicationAssets.ExistAsset(const aFileName:string):boolean;
{$ifdef Android}
var Asset:PAAsset;
begin
 result:=false;
 if assigned(AndroidAssetManager) then begin
  Asset:=AAssetManager_open(AndroidAssetManager,pansichar(TpvApplicationRawByteString(CorrectFileName(aFileName))),AASSET_MODE_UNKNOWN);
  if assigned(Asset) then begin
   AAsset_close(Asset);
   result:=true;
  end;
 end else begin
  raise Exception.Create('Asset manager is null');
 end;
end;
{$else}
begin
 result:=FileExists(CorrectFileName(aFileName));
end;
{$endif}

function TpvApplicationAssets.GetAssetStream(const aFileName:string):TStream;
{$ifdef Android}
var Asset:PAAsset;
    Size:int64;
begin
 result:=nil;
 if assigned(AndroidAssetManager) then begin
  Asset:=AAssetManager_open(AndroidAssetManager,pansichar(TpvApplicationRawByteString(CorrectFileName(aFileName))),AASSET_MODE_UNKNOWN);
  if assigned(Asset) then begin
   try
    Size:=AAsset_getLength(Asset);
    result:=TMemoryStream.Create;
    result.Size:=Size;
    AAsset_read(Asset,TMemoryStream(result).Memory,Size);
  //Move(AAsset_getBuffer(Asset)^,Data^,Size);
   finally
    AAsset_close(Asset);
   end;
  end else begin
   raise Exception.Create('Asset "'+aFileName+'" not found');
  end;
 end else begin
  raise Exception.Create('Asset manager is null');
 end;
end;
{$else}
begin
 result:=TFileStream.Create(CorrectFileName(aFileName),fmOpenRead or fmShareDenyWrite);
end;
{$endif}

function TpvApplicationAssets.GetAssetSize(const aFileName:string):int64;
{$ifdef Android}
var Asset:PAAsset;
begin
 result:=0;
 if assigned(AndroidAssetManager) then begin
  Asset:=AAssetManager_open(AndroidAssetManager,pansichar(TpvApplicationRawByteString(CorrectFileName(aFileName))),AASSET_MODE_UNKNOWN);
  if assigned(Asset) then begin
   try
    result:=AAsset_getLength(Asset);
   finally
    AAsset_close(Asset);
   end;
  end else begin
   raise Exception.Create('Asset "'+aFileName+'" not found');
  end;
 end else begin
  raise Exception.Create('Asset manager is null');
 end;
end;
{$else}
var Stream:TStream;
begin
 Stream:=TFileStream.Create(CorrectFileName(aFileName),fmOpenRead or fmShareDenyWrite);
 try
  result:=Stream.Size;
 finally
  Stream.Free;
 end;
end;
{$endif}

constructor TpvApplicationFiles.Create(const aVulkanApplication:TpvApplication);
begin
 inherited Create;
 fVulkanApplication:=aVulkanApplication;
end;

destructor TpvApplicationFiles.Destroy;
begin
 inherited Destroy;
end;

function TpvApplicationFiles.GetCacheStoragePath:string;
begin
 if length(fVulkanApplication.fCacheStoragePath)>0 then begin
  result:=IncludeTrailingPathDelimiter(fVulkanApplication.fCacheStoragePath);
 end else begin
  result:='';
 end;
end;

function TpvApplicationFiles.GetLocalStoragePath:string;
begin
 if length(fVulkanApplication.fLocalStoragePath)>0 then begin
  result:=IncludeTrailingPathDelimiter(fVulkanApplication.fLocalStoragePath);
 end else begin
  result:='';
 end;
end;

function TpvApplicationFiles.GetRoamingStoragePath:string;
begin
 if length(fVulkanApplication.fRoamingStoragePath)>0 then begin
  result:=IncludeTrailingPathDelimiter(fVulkanApplication.fRoamingStoragePath);
 end else begin
  result:='';
 end;
end;

function TpvApplicationFiles.GetExternalStoragePath:string;
begin
 if length(fVulkanApplication.fExternalStoragePath)>0 then begin
  result:=IncludeTrailingPathDelimiter(fVulkanApplication.fExternalStoragePath);
 end else begin
  result:='';
 end;
end;

function TpvApplicationFiles.IsCacheStorageAvailable:boolean;
begin
 result:=length(fVulkanApplication.fCacheStoragePath)>0;
end;

function TpvApplicationFiles.IsLocalStorageAvailable:boolean;
begin
 result:=length(fVulkanApplication.fLocalStoragePath)>0;
end;

function TpvApplicationFiles.IsRoamingStorageAvailable:boolean;
begin
 result:=length(fVulkanApplication.fRoamingStoragePath)>0;
end;

function TpvApplicationFiles.IsExternalStorageAvailable:boolean;
begin
 result:=length(fVulkanApplication.fExternalStoragePath)>0;
end;

constructor TpvApplicationClipboard.Create(const aVulkanApplication:TpvApplication);
begin
 inherited Create;
 fVulkanApplication:=aVulkanApplication;
end;

destructor TpvApplicationClipboard.Destroy;
begin
 inherited Destroy;
end;

function TpvApplicationClipboard.HasText:boolean;
begin
{$if defined(PasVulkanUseSDL2)}
 result:=SDL_HasClipboardText<>SDL_FALSE;
{$else}
 result:=false;
{$ifend}
end;

function TpvApplicationClipboard.GetText:TpvApplicationUTF8String;
{$if defined(PasVulkanUseSDL2)}
var p:PAnsiChar;
    l:TpvInt32;
begin
 result:='';
 p:=SDL_GetClipboardText;
 if assigned(p) then begin
  try
   l:=StrLen(p);
   if l>0 then begin
    SetLength(result,l);
    Move(p^,result[1],l);
   end;
  finally
   SDL_free(p);
  end;
 end;
end;
{$else}
begin
 result:='';
end;
{$ifend}

procedure TpvApplicationClipboard.SetText(const aTextString:TpvApplicationUTF8String);
begin
{$if defined(PasVulkanUseSDL2)}
 SDL_SetClipboardText(PAnsiChar(aTextString));
{$else}
{$ifend}
end;

procedure AudioFillBuffer(AudioEngine:TpvAudio;Buffer:TpvPointer;Len:TpvInt32);
begin
 while (AudioEngine.IsReady and AudioEngine.IsActive) and assigned(AudioEngine.Thread) and not AudioEngine.Thread.Terminated do begin
  if AudioEngine.RingBuffer.AvailableForRead>=Len then begin
   AudioEngine.RingBuffer.Read(Buffer,Len);
   exit;
  end;
  if AudioEngine.Thread.Sleeping<>0 then begin
   AudioEngine.Thread.Event.SetEvent;
  end;
  Sleep(1);
 end;
 FillChar(Buffer^,Len,0);
end;

{$if defined(PasVulkanUseSDL2)}
procedure SDLFillBuffer(UserData:TpvPointer;Stream:PSDLUInt8;Remain:TSDLInt32); cdecl;
begin
 AudioFillBuffer(UserData,Stream,Remain);
end;
{$else}
{$ifend}

constructor TpvApplication.Create;
begin

{$if defined(PasVulkanUseSDL2)}
 SDL_SetMainReady;

 SDL_SetHint(SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING,'1');
{$else}
{$ifend}

 inherited Create;

 VulkanDisableFloatingPointExceptions;

{$if defined(Release)}
 fDebugging:=false;
{$elseif defined(Windows)}
 fDebugging:={$ifdef fpc}IsDebuggerPresent{$else}DebugHook<>0{$endif};
{$else}
 fDebugging:=true;
{$ifend}

 fTitle:='PasVulkan Application';
 fVersion:=$0100;

 fPathName:='PasVulkanApplication';

 fCacheStoragePath:='';

 fLocalStoragePath:='';

 fRoamingStoragePath:='';

 fExternalStoragePath:='';

 if assigned(GlobalPasMP) then begin
  fPasMPInstance:=GlobalPasMP;
  fDoDestroyGlobalPasMPInstance:=false;
 end else begin
  GlobalPasMPMaximalThreads:=-1;
  GlobalPasMPThreadHeadRoomForForeignTasks:=0;
  GlobalPasMPDoCPUCorePinning:=false;
  GlobalPasMPSleepingOnIdle:=true;
  GlobalPasMPAllWorkerThreadsHaveOwnSystemThreads:=false;
  GlobalPasMPProfiling:=false;
  fPasMPInstance:=TPasMP.GetGlobalInstance;
  fDoDestroyGlobalPasMPInstance:=true;
 end;

 fHighResolutionTimer:=TpvApplicationHighResolutionTimer.Create;

 fAssets:=TpvApplicationAssets.Create(self);

 fFiles:=TpvApplicationFiles.Create(self);

 fInput:=TpvApplicationInput.Create(self);

 fClipboard:=TpvApplicationClipboard.Create(self);

 fAudio:=nil;

 fRunnableList:=nil;
 fRunnableListCount:=0;
 fRunnableListCriticalSection:=TPasMPCriticalSection.Create;

 fLifecycleListenerList:=TList.Create;
 fLifecycleListenerListCriticalSection:=TPasMPCriticalSection.Create;

{$if defined(PasVulkanUseSDL2)}
 fLastPressedKeyEvent.SDLEvent.type_:=0;
{$else}
{$ifend}
 fKeyRepeatTimeAccumulator:=0;
 fKeyRepeatInterval:=fHighResolutionTimer.MillisecondInterval*100;
 fKeyRepeatInitialInterval:=fHighResolutionTimer.MillisecondInterval*400;
 fNativeKeyRepeat:=true;

 fCurrentWidth:=-1;
 fCurrentHeight:=-1;
 fCurrentFullscreen:=-1;
 fCurrentPresentMode:=High(TpvInt32);
 fCurrentVisibleMouseCursor:=-1;
 fCurrentCatchMouse:=-1;
 fCurrentHideSystemBars:=-1;
 fCurrentBlocking:=-1;

 fWidth:=1280;
 fHeight:=720;
 fFullscreen:=false;
 fPresentMode:=TpvApplicationPresentMode.Immediate;
 fResizable:=true;
 fVisibleMouseCursor:=false;
 fCatchMouse:=false;
 fHideSystemBars:=false;
 fAndroidSeparateMouseAndTouch:=true;
 fUseAudio:=false;
 fBlocking:=true;

 fActive:=true;

 fTerminated:=false;

 fGraphicsReady:=false;

 fSkipNextDrawFrame:=false;

 fVulkanDebugging:=false;

 fVulkanDebuggingEnabled:=false;

 fVulkanValidation:=false;

 fVulkanInstance:=nil;

 fVulkanDevice:=nil;

 fVulkanPipelineCache:=nil;

 fCountCPUThreads:=Max(1,TPasMP.GetCountOfHardwareThreads(fAvailableCPUCores));
{$if defined(fpc) and defined(android)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar(TpvApplicationRawByteString('Detected CPU thread count: '+IntToStr(fCountCPUThreads))));
{$ifend}

 fVulkanCountCommandQueues:=0;

 fVulkanCommandPools:=nil;
 fVulkanCommandBuffers:=nil;
 fVulkanCommandBufferFences:=nil;

 fVulkanUniversalCommandPools:=nil;
 fVulkanUniversalCommandBuffers:=nil;
 fVulkanUniversalCommandBufferFences:=nil;

 fVulkanPresentCommandPools:=nil;
 fVulkanPresentCommandBuffers:=nil;
 fVulkanPresentCommandBufferFences:=nil;

 fVulkanGraphicsCommandPools:=nil;
 fVulkanGraphicsCommandBuffers:=nil;
 fVulkanGraphicsCommandBufferFences:=nil;

 fVulkanComputeCommandPools:=nil;
 fVulkanComputeCommandBuffers:=nil;
 fVulkanComputeCommandBufferFences:=nil;

 fVulkanTransferCommandPools:=nil;
 fVulkanTransferCommandBuffers:=nil;
 fVulkanTransferCommandBufferFences:=nil;

 fVulkanRecreationKind:=TpvApplicationVulkanRecreationKind.None;

 fVulkanSwapChain:=nil;

 fVulkanOldSwapChain:=nil;

 fVulkanTransferInflightCommandsFromOldSwapChain:=false;

 fScreen:=nil;

 fNextScreen:=nil;

 fNextScreenClass:=nil;
 
 fHasNewNextScreen:=false;

 fHasLastTime:=false;

 fLastTime:=0;
 fNowTime:=0;
 fDeltaTime:=0;

 fFrameCounter:=0;

 fUpdateFrameCounter:=0;

 fDrawFrameCounter:=0;

 SetDesiredCountSwapChainImages(3);

 fCountSwapChainImages:=1;

 fUpdateSwapChainImageIndex:=0;

 fDrawSwapChainImageIndex:=0;

 fRealUsedDrawSwapChainImageIndex:=0;
 
 fOnEvent:=nil;

 pvApplication:=self;

end;

destructor TpvApplication.Destroy;
begin

 FreeAndNil(fLifecycleListenerList);
 FreeAndNil(fLifecycleListenerListCriticalSection);

 fRunnableList:=nil;
 fRunnableListCount:=0;
 FreeAndNil(fRunnableListCriticalSection);

 FreeAndNil(fAudio);

 FreeAndNil(fClipboard);

 FreeAndNil(fInput);

 FreeAndNil(fFiles);

 FreeAndNil(fAssets);

 FreeAndNil(fHighResolutionTimer);

 if fDoDestroyGlobalPasMPInstance then begin
  TPasMP.DestroyGlobalInstance;
 end;

 fPasMPInstance:=nil;

 pvApplication:=nil;

 inherited Destroy;
end;

class procedure TpvApplication.Log(const aLevel:TpvInt32;const aWhere,aWhat:string);
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 case aLevel of
  LOG_NONE:begin
  end;
  LOG_INFO:begin
   __android_log_write(ANDROID_LOG_INFO,PAnsiChar(AnsiString(aWhere)),PAnsiChar(AnsiString(aWhat)));
  end;
  LOG_VERBOSE:begin
   __android_log_write(ANDROID_LOG_VERBOSE,PAnsiChar(AnsiString(aWhere)),PAnsiChar(AnsiString(aWhat)));
  end;
  LOG_DEBUG:begin
   __android_log_write(ANDROID_LOG_DEBUG,PAnsiChar(AnsiString(aWhere)),PAnsiChar(AnsiString(aWhat)));
  end;
  LOG_ERROR:begin
   __android_log_write(ANDROID_LOG_ERROR,PAnsiChar(AnsiString(aWhere)),PAnsiChar(AnsiString(aWhat)));
  end;
 end;
{$elseif not defined(Release)}
 case aLevel of
  LOG_NONE:begin
  end;
  LOG_INFO:begin
   WriteLn('[Info] ',aWhere,': ',aWhat);
  end;
  LOG_VERBOSE:begin
   WriteLn('[Verbose] ',aWhere,': ',aWhat);
  end;
  LOG_DEBUG:begin
   WriteLn('[Debug] ',aWhere,': ',aWhat);
  end;
  LOG_ERROR:begin
   WriteLn('[Error] ',aWhere,': ',aWhat);
  end;
 end;
{$ifend}
end;

procedure TpvApplication.SetDesiredCountSwapChainImages(const aDesiredCountSwapChainImages:TpvInt32);
begin
 if aDesiredCountSwapChainImages<1 then begin
  fDesiredCountSwapChainImages:=1;
 end else if aDesiredCountSwapChainImages>MaxSwapChainImages then begin
  fDesiredCountSwapChainImages:=MaxSwapChainImages;
 end else begin
  fDesiredCountSwapChainImages:=aDesiredCountSwapChainImages;
 end;
end;

procedure TpvApplication.VulkanDebugLn(const What:string);
{$if defined(Windows)}
var StdOut:Windows.THandle;
begin
 StdOut:=GetStdHandle(Std_Output_Handle);
 Win32Check(StdOut<>Invalid_Handle_Value);
 if StdOut<>0 then begin
  WriteLn(What);
 end;
end;
{$elseif (defined(fpc) and defined(android)) and not defined(Release)}
begin
 __android_log_write(ANDROID_LOG_DEBUG,'PasVulkanApplication',PAnsiChar(TpvUTF8String(What)));
end;
{$else}
begin
 WriteLn(What);
end;
{$ifend}

function TpvApplication.VulkanOnDebugReportCallback(const aFlags:TVkDebugReportFlagsEXT;const aObjectType:TVkDebugReportObjectTypeEXT;const aObject:TpvUInt64;const aLocation:TVkSize;aMessageCode:TpvInt32;const aLayerPrefix,aMessage:string):TVkBool32;
var Prefix:string;
begin
 try
  Prefix:='';
  if (aFlags and TVkDebugReportFlagsEXT(VK_DEBUG_REPORT_ERROR_BIT_EXT))<>0 then begin
   Prefix:=Prefix+'ERROR: ';
  end;
  if (aFlags and TVkDebugReportFlagsEXT(VK_DEBUG_REPORT_WARNING_BIT_EXT))<>0 then begin
   Prefix:=Prefix+'WARNING: ';
  end;
  if (aFlags and TVkDebugReportFlagsEXT(VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT))<>0 then begin
   Prefix:=Prefix+'PERFORMANCE: ';
  end;
  if (aFlags and TVkDebugReportFlagsEXT(VK_DEBUG_REPORT_INFORMATION_BIT_EXT))<>0 then begin
   Prefix:=Prefix+'INFORMATION: ';
  end;
  if (aFlags and TVkDebugReportFlagsEXT(VK_DEBUG_REPORT_DEBUG_BIT_EXT))<>0 then begin
   Prefix:=Prefix+'DEBUG: ';
  end;
  VulkanDebugLn('[Debug] '+Prefix+'['+aLayerPrefix+'] Code '+IntToStr(aMessageCode)+' : '+aMessage);
 finally
  result:=VK_FALSE;
 end;
end;

procedure TpvApplication.VulkanWaitIdle;
var Index:TpvInt32;
begin
 if assigned(fVulkanDevice) then begin
  fVulkanDevice.WaitIdle;
  for Index:=0 to fCountSwapChainImages-1 do begin
   if fVulkanPresentCompleteFencesReady[Index] then begin
    fVulkanPresentCompleteFences[Index].WaitFor;
    fVulkanPresentCompleteFences[Index].Reset;
    fVulkanPresentCompleteFencesReady[Index]:=false;
   end;
   if fVulkanWaitFencesReady[Index] and assigned(fVulkanWaitFences[Index]) then begin
    fVulkanWaitFences[Index].WaitFor;
    fVulkanWaitFences[Index].Reset;
    fVulkanWaitFencesReady[Index]:=false;
   end;
  end;
  for Index:=0 to length(fVulkanDevice.Queues)-1 do begin
   if assigned(fVulkanDevice.Queues[Index]) then begin
    fVulkanDevice.Queues[Index].WaitIdle;
   end;
  end;
  fVulkanDevice.WaitIdle;
 end;
end;

procedure TpvApplication.CreateVulkanDevice(const aSurface:TpvVulkanSurface=nil);
var QueueFamilyIndex,ThreadIndex,SwapChainImageIndex:TpvInt32;
    FormatProperties:TVkFormatProperties;
begin
 if not assigned(VulkanDevice) then begin

  fVulkanDevice:=TpvVulkanDevice.Create(VulkanInstance,nil,aSurface,nil);
  fVulkanDevice.AddQueues;
  fVulkanDevice.EnabledExtensionNames.Add(VK_KHR_SWAPCHAIN_EXTENSION_NAME);
  fVulkanDevice.Initialize;

  if (length(fVulkanPipelineCacheFileName)>0) and FileExists(fVulkanPipelineCacheFileName) then begin
   try
    fVulkanPipelineCache:=TpvVulkanPipelineCache.CreateFromFile(fVulkanDevice,fVulkanPipelineCacheFileName);
   except
    on e:EpvVulkanPipelineCacheException do begin
     fVulkanPipelineCache:=TpvVulkanPipelineCache.Create(fVulkanDevice);
    end;
   end;
  end else begin
   fVulkanPipelineCache:=TpvVulkanPipelineCache.Create(fVulkanDevice);
  end;

  fVulkanCountCommandQueues:=length(fVulkanDevice.PhysicalDevice.QueueFamilyProperties);
  SetLength(fVulkanCommandPools,fVulkanCountCommandQueues,fCountCPUThreads+1,MaxSwapChainImages);
  SetLength(fVulkanCommandBuffers,fVulkanCountCommandQueues,fCountCPUThreads+1,MaxSwapChainImages);
  SetLength(fVulkanCommandBufferFences,fVulkanCountCommandQueues,fCountCPUThreads+1,MaxSwapChainImages);
  for QueueFamilyIndex:=0 to length(fVulkanDevice.PhysicalDevice.QueueFamilyProperties)-1 do begin
   if (QueueFamilyIndex=fVulkanDevice.UniversalQueueFamilyIndex) or
      (QueueFamilyIndex=fVulkanDevice.PresentQueueFamilyIndex) or
      (QueueFamilyIndex=fVulkanDevice.GraphicsQueueFamilyIndex) or
      (QueueFamilyIndex=fVulkanDevice.ComputeQueueFamilyIndex) or
      (QueueFamilyIndex=fVulkanDevice.TransferQueueFamilyIndex) then begin
    for ThreadIndex:=0 to fCountCPUThreads do begin
     for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
      fVulkanCommandPools[QueueFamilyIndex,ThreadIndex,SwapChainImageIndex]:=TpvVulkanCommandPool.Create(fVulkanDevice,QueueFamilyIndex,TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
      fVulkanCommandBuffers[QueueFamilyIndex,ThreadIndex,SwapChainImageIndex]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPools[QueueFamilyIndex,ThreadIndex,SwapChainImageIndex],VK_COMMAND_BUFFER_LEVEL_PRIMARY);
      fVulkanCommandBufferFences[QueueFamilyIndex,ThreadIndex,SwapChainImageIndex]:=TpvVulkanFence.Create(fVulkanDevice);
     end;
    end;
   end;
  end;

  if fVulkanDevice.UniversalQueueFamilyIndex>=0 then begin
   fVulkanUniversalCommandPools:=fVulkanCommandPools[fVulkanDevice.UniversalQueueFamilyIndex];
   fVulkanUniversalCommandBuffers:=fVulkanCommandBuffers[fVulkanDevice.UniversalQueueFamilyIndex];
   fVulkanUniversalCommandBufferFences:=fVulkanCommandBufferFences[fVulkanDevice.UniversalQueueFamilyIndex];
  end else begin
   fVulkanUniversalCommandPools:=nil;
   fVulkanUniversalCommandBuffers:=nil;
   fVulkanUniversalCommandBufferFences:=nil;
  end;

  if fVulkanDevice.PresentQueueFamilyIndex>=0 then begin
   fVulkanPresentCommandPools:=fVulkanCommandPools[fVulkanDevice.PresentQueueFamilyIndex];
   fVulkanPresentCommandBuffers:=fVulkanCommandBuffers[fVulkanDevice.PresentQueueFamilyIndex];
   fVulkanPresentCommandBufferFences:=fVulkanCommandBufferFences[fVulkanDevice.PresentQueueFamilyIndex];
  end else begin
   fVulkanPresentCommandPools:=nil;
   fVulkanPresentCommandBuffers:=nil;
   fVulkanPresentCommandBufferFences:=nil;
  end;

  if fVulkanDevice.GraphicsQueueFamilyIndex>=0 then begin
   fVulkanGraphicsCommandPools:=fVulkanCommandPools[fVulkanDevice.GraphicsQueueFamilyIndex];
   fVulkanGraphicsCommandBuffers:=fVulkanCommandBuffers[fVulkanDevice.GraphicsQueueFamilyIndex];
   fVulkanGraphicsCommandBufferFences:=fVulkanCommandBufferFences[fVulkanDevice.GraphicsQueueFamilyIndex];
  end else begin
   fVulkanGraphicsCommandPools:=nil;
   fVulkanGraphicsCommandBuffers:=nil;
   fVulkanGraphicsCommandBufferFences:=nil;
  end;

  if fVulkanDevice.ComputeQueueFamilyIndex>=0 then begin
   fVulkanComputeCommandPools:=fVulkanCommandPools[fVulkanDevice.ComputeQueueFamilyIndex];
   fVulkanComputeCommandBuffers:=fVulkanCommandBuffers[fVulkanDevice.ComputeQueueFamilyIndex];
   fVulkanComputeCommandBufferFences:=fVulkanCommandBufferFences[fVulkanDevice.ComputeQueueFamilyIndex];
  end else begin
   fVulkanComputeCommandPools:=nil;
   fVulkanComputeCommandBuffers:=nil;
   fVulkanComputeCommandBufferFences:=nil;
  end;

  if fVulkanDevice.TransferQueueFamilyIndex>=0 then begin
   fVulkanTransferCommandPools:=fVulkanCommandPools[fVulkanDevice.TransferQueueFamilyIndex];
   fVulkanTransferCommandBuffers:=fVulkanCommandBuffers[fVulkanDevice.TransferQueueFamilyIndex];
   fVulkanTransferCommandBufferFences:=fVulkanCommandBufferFences[fVulkanDevice.TransferQueueFamilyIndex];
  end else begin
   fVulkanTransferCommandPools:=nil;
   fVulkanTransferCommandBuffers:=nil;
   fVulkanTransferCommandBufferFences:=nil;
  end;

  fVulkanDepthImageFormat:=fVulkanDevice.PhysicalDevice.GetBestSupportedDepthFormat(false);

  fVulkanInstance.Commands.GetPhysicalDeviceFormatProperties(fVulkanDevice.PhysicalDevice.Handle,fVulkanDepthImageFormat,@FormatProperties);
  if (FormatProperties.OptimalTilingFeatures and TVkFormatFeatureFlags(VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT))=0 then begin
   raise EpvVulkanException.Create('No suitable depth image format!');
  end;

 end;
end;

procedure TpvApplication.CreateVulkanInstance;
{$if defined(PasVulkanUseSDL2)}
type TExtensions=array of PAnsiChar;
var i:TpvInt32;
    SDL_SysWMinfo:TSDL_SysWMinfo;
    CountExtensions:TSDLInt32;
    Extensions:TExtensions;
begin
 if not assigned(fVulkanInstance) then begin
  SDL_VERSION(SDL_SysWMinfo.version);
  if {$if defined(PasVulkanUseSDL2WithVulkanSupport)}fSDLVersionWithVulkanSupport or{$ifend}
     (SDL_GetWindowWMInfo(fSurfaceWindow,@SDL_SysWMinfo)<>0) then begin
   fVulkanInstance:=TpvVulkanInstance.Create(TpvVulkanCharString(Title),Version,
                                             'PasVulkanApplication',$0100,
                                              VK_API_VERSION_1_0,false,nil);
   for i:=0 to fVulkanInstance.AvailableLayerNames.Count-1 do begin
    VulkanDebugLn('Layer: '+fVulkanInstance.AvailableLayerNames[i]);
   end;
   for i:=0 to fVulkanInstance.AvailableExtensionNames.Count-1 do begin
    VulkanDebugLn('Extension: '+fVulkanInstance.AvailableExtensionNames[i]);
   end;
{$if defined(PasVulkanUseSDL2WithVulkanSupport)}
   if fSDLVersionWithVulkanSupport then begin
    if not SDL_Vulkan_GetInstanceExtensions(fSurfaceWindow,@CountExtensions,nil) then begin
     raise EpvVulkanException.Create('Vulkan initialization failure at SDL_Vulkan_GetInstanceExtensions: '+String(SDL_GetError));
    end;
    Extensions:=nil;
    try
     SetLength(Extensions,CountExtensions);
     if not SDL_Vulkan_GetInstanceExtensions(fSurfaceWindow,@CountExtensions,@Extensions[0]) then begin
      raise EpvVulkanException.Create('Vulkan initialization failure at SDL_Vulkan_GetInstanceExtensions: '+String(SDL_GetError));
     end;
     for i:=0 to CountExtensions-1 do begin
      fVulkanInstance.EnabledExtensionNames.Add(String(Extensions[i]));
     end;
    finally
     Extensions:=nil;
    end;
   end else{$ifend} begin
    fVulkanInstance.EnabledExtensionNames.Add(VK_KHR_SURFACE_EXTENSION_NAME);
    case SDL_SysWMinfo.subsystem of
{$if defined(Android)}
     SDL_SYSWM_ANDROID:begin
      fVulkanInstance.EnabledExtensionNames.Add(VK_KHR_ANDROID_SURFACE_EXTENSION_NAME);
     end;
{$ifend}
{$if defined(Mir) and defined(Unix)}
     SDL_SYSWM_MIR:begin
      fVulkanInstance.EnabledExtensionNames.Add(VK_KHR_MIR_SURFACE_EXTENSION_NAME);
     end;
{$ifend}
{$if defined(Wayland) and defined(Unix)}
     SDL_SYSWM_WAYLAND:begin
      fVulkanInstance.EnabledExtensionNames.Add(VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME);
     end;
{$ifend}
{$if defined(Windows)}
     SDL_SYSWM_WINDOWS:begin
      fVulkanInstance.EnabledExtensionNames.Add(VK_KHR_WIN32_SURFACE_EXTENSION_NAME);
     end;
{$ifend}
{$if (defined(XLIB) or defined(XCB)) and defined(Unix)}
     SDL_SYSWM_X11:begin
{$if defined(XLIB) and defined(Unix)}
      fVulkanInstance.EnabledExtensionNames.Add(VK_KHR_XLIB_SURFACE_EXTENSION_NAME);
{$elseif defined(XCB) and defined(Unix)}
      fVulkanInstance.EnabledExtensionNames.Add(VK_KHR_XCB_SURFACE_EXTENSION_NAME);
{$ifend}
     end;
{$ifend}
     else begin
      raise EpvVulkanException.Create('Vulkan initialization failure');
     end;
    end;
   end;
   if fVulkanDebugging and
      (fVulkanInstance.AvailableExtensionNames.IndexOf(VK_EXT_DEBUG_REPORT_EXTENSION_NAME)>=0) then begin
    fVulkanInstance.EnabledExtensionNames.Add(VK_EXT_DEBUG_REPORT_EXTENSION_NAME);
    fVulkanDebuggingEnabled:=true;
    if fVulkanValidation then begin
{$if defined(Android)}
     if fVulkanInstance.AvailableLayerNames.IndexOf('VK_LAYER_GOOGLE_threading')>=0 then begin
      fVulkanInstance.EnabledLayerNames.Add('VK_LAYER_GOOGLE_threading');
     end;
     if fVulkanInstance.AvailableLayerNames.IndexOf('VK_LAYER_LUNARG_parameter_validation')>=0 then begin
      fVulkanInstance.EnabledLayerNames.Add('VK_LAYER_LUNARG_parameter_validation');
     end;
     if fVulkanInstance.AvailableLayerNames.IndexOf('VK_LAYER_LUNARG_object_tracker')>=0 then begin
      fVulkanInstance.EnabledLayerNames.Add('VK_LAYER_LUNARG_object_tracker');
     end;
     if fVulkanInstance.AvailableLayerNames.IndexOf('VK_LAYER_LUNARG_core_validation')>=0 then begin
      fVulkanInstance.EnabledLayerNames.Add('VK_LAYER_LUNARG_core_validation');
     end;
     if fVulkanInstance.AvailableLayerNames.IndexOf('VK_LAYER_LUNARG_swapchain')>=0 then begin
      fVulkanInstance.EnabledLayerNames.Add('VK_LAYER_LUNARG_swapchain');
     end;
     if fVulkanInstance.AvailableLayerNames.IndexOf('VK_LAYER_GOOGLE_unique_objects')>=0 then begin
      fVulkanInstance.EnabledLayerNames.Add('VK_LAYER_GOOGLE_unique_objects');
     end;
{$else}
     if fVulkanInstance.AvailableLayerNames.IndexOf('VK_LAYER_LUNARG_standard_validation')>=0 then begin
      fVulkanInstance.EnabledLayerNames.Add('VK_LAYER_LUNARG_standard_validation');
     end;
{$ifend}
    end;
   end else begin
    fVulkanDebuggingEnabled:=false;
   end;
   fVulkanInstance.Initialize;
   if fVulkanDebuggingEnabled then begin
    fVulkanInstance.OnInstanceDebugReportCallback:=VulkanOnDebugReportCallback;
    fVulkanInstance.InstallDebugReportCallback;
   end;
  end;
 end;
end;
{$else}
begin
 Assert(false);
end;
{$ifend}

procedure TpvApplication.DestroyVulkanInstance;
var Index,SubIndex,SubSubIndex:TpvInt32;
begin

 if length(fVulkanPipelineCacheFileName)>0 then begin
  try
   fVulkanPipelineCache.SaveToFile(fVulkanPipelineCacheFileName);
  except
  end;
 end;

 fVulkanUniversalCommandPools:=nil;
 fVulkanUniversalCommandBuffers:=nil;
 fVulkanUniversalCommandBufferFences:=nil;

 fVulkanPresentCommandPools:=nil;
 fVulkanPresentCommandBuffers:=nil;
 fVulkanPresentCommandBufferFences:=nil;

 fVulkanGraphicsCommandPools:=nil;
 fVulkanGraphicsCommandBuffers:=nil;
 fVulkanGraphicsCommandBufferFences:=nil;

 fVulkanComputeCommandPools:=nil;
 fVulkanComputeCommandBuffers:=nil;
 fVulkanComputeCommandBufferFences:=nil;

 fVulkanTransferCommandPools:=nil;
 fVulkanTransferCommandBuffers:=nil;
 fVulkanTransferCommandBufferFences:=nil;

 for Index:=0 to fVulkanCountCommandQueues-1 do begin
  for SubIndex:=0 to fCountCPUThreads do begin
   for SubSubIndex:=0 to MaxSwapChainImages-1 do begin
    FreeAndNil(fVulkanCommandBufferFences[Index,SubIndex,SubSubIndex]);
    FreeAndNil(fVulkanCommandBuffers[Index,SubIndex,SubSubIndex]);
    FreeAndNil(fVulkanCommandPools[Index,SubIndex,SubSubIndex]);
   end;
  end;
 end;

 fVulkanCommandPools:=nil;
 fVulkanCommandBuffers:=nil;
 fVulkanCommandBufferFences:=nil;

 //FreeAndNil(VulkanPresentationSurface);
 FreeAndNil(fVulkanPipelineCache);
 FreeAndNil(fVulkanDevice);
 FreeAndNil(fVulkanInstance);
//VulkanPresentationSurface:=nil;

 fVulkanDevice:=nil;
 fVulkanInstance:=nil;

end;

procedure TpvApplication.CreateVulkanSurface;
{$if defined(PasVulkanUseSDL2)}
var SDL_SysWMinfo:TSDL_SysWMinfo;
    VulkanSurfaceCreateInfo:TpvVulkanSurfaceCreateInfo;
{$if defined(PasVulkanUseSDL2WithVulkanSupport)}
    VulkanSurface:TVkSurfaceKHR;
{$ifend}
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TpvApplication.AllocateVulkanSurface');
{$ifend}
 if not assigned(fVulkanSurface) then begin
{$if defined(PasVulkanUseSDL2WithVulkanSupport)}if fSDLVersionWithVulkanSupport then begin
   if not SDL_Vulkan_CreateSurface(fSurfaceWindow,fVulkanInstance.Handle,@VulkanSurface) then begin
    raise EpvVulkanException.Create('Vulkan initialization failure at SDL_Vulkan_CreateSurface: '+String(SDL_GetError));
   end;
   fVulkanSurface:=TpvVulkanSurface.CreateHandle(fVulkanInstance,VulkanSurface);
  end else{$ifend} begin
   SDL_VERSION(SDL_SysWMinfo.version);
   if SDL_GetWindowWMInfo(fSurfaceWindow,@SDL_SysWMinfo)<>0 then begin
    FillChar(VulkanSurfaceCreateInfo,SizeOf(TpvVulkanSurfaceCreateInfo),#0);
    case SDL_SysWMinfo.subsystem of
{$if defined(Android)}
     SDL_SYSWM_ANDROID:begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
      __android_log_write(ANDROID_LOG_DEBUG,'PasVulkanApplication',PAnsiChar('Got native window 0x'+IntToHex(PtrUInt(SDL_SysWMinfo.Window),SizeOf(PtrUInt)*2)));
{$ifend}
      VulkanSurfaceCreateInfo.Android.sType:=VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR;
      VulkanSurfaceCreateInfo.Android.window:=SDL_SysWMinfo.Window;
     end;
{$ifend}
{$if defined(Mir) and defined(Unix)}
     SDL_SYSWM_MIR:begin
      VulkanSurfaceCreateInfo.Mir.sType:=VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR;
      VulkanSurfaceCreateInfo.Mir.connection:=SDL_SysWMinfo.Mir.Connection;
      VulkanSurfaceCreateInfo.Mir.mirSurface:=SDL_SysWMinfo.Mir.Surface;
     end;
{$ifend}
{$if defined(Wayland) and defined(Unix)}
     SDL_SYSWM_WAYLAND:begin
      VulkanSurfaceCreateInfo.Wayland.sType:=VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR;
      VulkanSurfaceCreateInfo.Wayland.display:=SDL_SysWMinfo.Wayland.Display;
      VulkanSurfaceCreateInfo.Wayland.surface:=SDL_SysWMinfo.Wayland.surface;
     end;
{$ifend}
{$if defined(Windows)}
     SDL_SYSWM_WINDOWS:begin
      VulkanSurfaceCreateInfo.Win32.sType:=VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
      VulkanSurfaceCreateInfo.Win32.hwnd_:=SDL_SysWMinfo.Window;
     end;
{$ifend}
{$if defined(XLIB) and defined(Unix)}
     SDL_SYSWM_X11:begin
      VulkanSurfaceCreateInfo.XLIB.sType:=VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR;
      VulkanSurfaceCreateInfo.XLIB.Dpy:=SDL_SysWMinfo.X11.Display;
      VulkanSurfaceCreateInfo.XLIB.Window:=SDL_SysWMinfo.X11.Window;
     end;
{$ifend}
{$if (defined(XCB) and not defined(XLIB)) and defined(Unix)}
     SDL_SYSWM_X11:begin
      raise EpvVulkanException.Create('Vulkan initialization failure');
      exit;
     end;
{$ifend}
     else begin
      raise EpvVulkanException.Create('Vulkan initialization failure');
      exit;
     end;
    end;

    fVulkanSurface:=TpvVulkanSurface.Create(fVulkanInstance,VulkanSurfaceCreateInfo);

   end;

  end;

  if assigned(fVulkanSurface) and not assigned(fVulkanDevice) then begin
   CreateVulkanDevice(fVulkanSurface);
   if not assigned(fVulkanDevice) then begin
    raise EpvVulkanSurfaceException.Create('Device does not support surface');
   end;
  end;

 end;
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TpvApplication.AllocateVulkanSurface');
{$ifend}
end;
{$else}
begin
 Assert(false);
end;
{$ifend}

procedure TpvApplication.DestroyVulkanSurface;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TpvApplication.DestroyVulkanSurface');
{$ifend}
 FreeAndNil(fVulkanSurface);
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TpvApplication.DestroyVulkanSurface');
{$ifend}
end;

procedure TpvApplication.CreateVulkanSwapChain;
var Index:TpvInt32;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TpvApplication.CreateVulkanSwapChain');
{$ifend}

 DestroyVulkanSwapChain;

 fVulkanSwapChain:=TpvVulkanSwapChain.Create(fVulkanDevice,
                                             fVulkanSurface,
                                             fVulkanOldSwapChain,
                                             fWidth,
                                             fHeight,
                                             IfThen(fPresentMode<>TpvApplicationPresentMode.Immediate,fDesiredCountSwapChainImages,1),
                                             1,
                                             VK_FORMAT_UNDEFINED,
                                             VK_COLOR_SPACE_SRGB_NONLINEAR_KHR,
                                             TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT),
                                             VK_SHARING_MODE_EXCLUSIVE,
                                             nil,
                                             VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                                             PresentModeToVulkanPresentMode[fPresentMode],
                                             true,
                                             TVkSurfaceTransformFlagsKHR($ffffffff),
                                             true);

 fCountSwapChainImages:=fVulkanSwapChain.CountImages;

 fUpdateSwapChainImageIndex:=1;

 fDrawSwapChainImageIndex:=0;

 fRealUsedDrawSwapChainImageIndex:=0;

 for Index:=0 to fCountSwapChainImages-1 do begin
  fVulkanWaitFences[Index]:=TpvVulkanFence.Create(fVulkanDevice);
  fVulkanWaitFencesReady[Index]:=false;
  fVulkanPresentCompleteSemaphores[Index]:=TpvVulkanSemaphore.Create(fVulkanDevice);
  fVulkanPresentCompleteFences[Index]:=TpvVulkanFence.Create(fVulkanDevice);
  fVulkanPresentCompleteFencesReady[Index]:=false;
 end;

{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TpvApplication.DestroyVulkanSwapChain');
{$ifend}
end;

procedure TpvApplication.DestroyVulkanSwapChain;
var Index:TpvInt32;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TpvApplication.DestroyVulkanSwapChain');
{$ifend}
 for Index:=0 to fCountSwapChainImages-1 do begin
  fVulkanWaitFencesReady[Index]:=false;
  fVulkanPresentCompleteFencesReady[Index]:=false;
  FreeAndNil(fVulkanWaitFences[Index]);
  FreeAndNil(fVulkanPresentCompleteSemaphores[Index]);
  FreeAndNil(fVulkanPresentCompleteFences[Index]);
 end;
 FreeAndNil(fVulkanSwapChain);
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TpvApplication.DestroyVulkanSwapChain');
{$ifend}
end;

procedure TpvApplication.CreateVulkanRenderPass;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TpvApplication.CreateVulkanRenderPass');
{$ifend}

 DestroyVulkanRenderPass;

 fVulkanRenderPass:=TpvVulkanRenderPass.Create(pvApplication.VulkanDevice);

 fVulkanRenderPass.AddSubpassDescription(0,
                                         VK_PIPELINE_BIND_POINT_GRAPHICS,
                                         [],
                                         [fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                              fVulkanSwapChain.ImageFormat,
                                                                                                                              VK_SAMPLE_COUNT_1_BIT,
                                                                                                                              VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                                              VK_ATTACHMENT_STORE_OP_STORE,
                                                                                                                              VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                              VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                              VK_IMAGE_LAYOUT_UNDEFINED, //VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, //VK_IMAGE_LAYOUT_UNDEFINED, // VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                                                                                                                              VK_IMAGE_LAYOUT_PRESENT_SRC_KHR //VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL //VK_IMAGE_LAYOUT_PRESENT_SRC_KHR  // VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                                                                             ),
                                                                             VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                            )],
                                         [],
                                         fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                             fVulkanDepthImageFormat,
                                                                                                                             VK_SAMPLE_COUNT_1_BIT,
                                                                                                                             VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                                             VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                             VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                             VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                             VK_IMAGE_LAYOUT_UNDEFINED, //VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, // VK_IMAGE_LAYOUT_UNDEFINED, // VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                                                                                                                             VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                                                            ),
                                                                                  VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                 ),
                                         []);
 fVulkanRenderPass.AddSubpassDependency(VK_SUBPASS_EXTERNAL,
                                        0,
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                        TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                        TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                        TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
 fVulkanRenderPass.AddSubpassDependency(0,
                                        VK_SUBPASS_EXTERNAL,
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                        TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                        TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                        TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
 fVulkanRenderPass.Initialize;

 fVulkanRenderPass.ClearValues[0].color.float32[0]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[1]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[2]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[3]:=1.0;

{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TpvApplication.CreateVulkanRenderPass');
{$ifend}
end;

procedure TpvApplication.DestroyVulkanRenderPass;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TpvApplication.DestroyVulkanRenderPass');
{$ifend}
 FreeAndNil(fVulkanRenderPass);
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TpvApplication.DestroyVulkanRenderPass');
{$ifend}
end;

procedure TpvApplication.CreateVulkanFrameBuffers;
var Index:TpvInt32;
    ColorAttachmentImage:TpvVulkanImage;
    ColorAttachmentImageView:TpvVulkanImageView;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TpvApplication.CreateVulkanFrameBuffers');
{$ifend}

 DestroyVulkanFrameBuffers;

 SetLength(fVulkanFrameBufferColorAttachments,fVulkanSwapChain.CountImages);

 for Index:=0 to fVulkanSwapChain.CountImages-1 do begin
  fVulkanFrameBufferColorAttachments[Index]:=nil;
 end;

 for Index:=0 to fVulkanSwapChain.CountImages-1 do begin

  ColorAttachmentImage:=nil;

  ColorAttachmentImageView:=nil;

  try

   ColorAttachmentImage:=TpvVulkanImage.Create(fVulkanDevice,fVulkanSwapChain.Images[Index].Handle,nil,false);

   ColorAttachmentImage.SetLayout(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                  VK_IMAGE_LAYOUT_UNDEFINED,
                                  VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
                                  TVkAccessFlags(0),
                                  TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                  TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                  TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                  nil,
                                  VulkanPresentCommandBuffers[0,0],
                                  fVulkanDevice.PresentQueue,
                                  VulkanPresentCommandBufferFences[0,0],
                                  true);

   ColorAttachmentImageView:=TpvVulkanImageView.Create(fVulkanDevice,
                                                       ColorAttachmentImage,
                                                       VK_IMAGE_VIEW_TYPE_2D,
                                                       fVulkanSwapChain.ImageFormat,
                                                       VK_COMPONENT_SWIZZLE_IDENTITY,
                                                       VK_COMPONENT_SWIZZLE_IDENTITY,
                                                       VK_COMPONENT_SWIZZLE_IDENTITY,
                                                       VK_COMPONENT_SWIZZLE_IDENTITY,
                                                       TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                                       0,
                                                       1,
                                                       0,
                                                       1);

   ColorAttachmentImage.ImageView:=ColorAttachmentImageView;
   ColorAttachmentImageView.Image:=ColorAttachmentImage;

   fVulkanFrameBufferColorAttachments[Index]:=TpvVulkanFrameBufferAttachment.Create(fVulkanDevice,
                                                                                  ColorAttachmentImage,
                                                                                  ColorAttachmentImageView,
                                                                                  fVulkanSwapChain.Width,
                                                                                  fVulkanSwapChain.Height,
                                                                                  fVulkanSwapChain.ImageFormat,
                                                                                  true);

  except
   FreeAndNil(fVulkanFrameBufferColorAttachments[Index]);
   FreeAndNil(ColorAttachmentImageView);
   FreeAndNil(ColorAttachmentImage);
   raise;
  end;

 end;

 fVulkanDepthFrameBufferAttachment:=TpvVulkanFrameBufferAttachment.Create(fVulkanDevice,
                                                                          fVulkanDevice.GraphicsQueue,
                                                                          VulkanGraphicsCommandBuffers[0,0],
                                                                          VulkanGraphicsCommandBufferFences[0,0],
                                                                          fVulkanSwapChain.Width,
                                                                          fVulkanSwapChain.Height,
                                                                          fVulkanDepthImageFormat,
                                                                          TVkBufferUsageFlags(VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT));

 SetLength(fVulkanFrameBuffers,fVulkanSwapChain.CountImages);
 for Index:=0 to fVulkanSwapChain.CountImages-1 do begin
  fVulkanFrameBuffers[Index]:=nil;
 end;
 for Index:=0 to fVulkanSwapChain.CountImages-1 do begin
  fVulkanFrameBuffers[Index]:=TpvVulkanFrameBuffer.Create(fVulkanDevice,
                                                        fVulkanRenderPass,
                                                        fVulkanSwapChain.Width,
                                                        fVulkanSwapChain.Height,
                                                        1,
                                                        [fVulkanFrameBufferColorAttachments[Index],fVulkanDepthFrameBufferAttachment],
                                                        false);
 end;

{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TpvApplication.CreateVulkanFrameBuffers');
{$ifend}
end;

procedure TpvApplication.DestroyVulkanFrameBuffers;
var Index:TpvInt32;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TpvApplication.DestroyVulkanFrameBuffers');
{$ifend}
 for Index:=0 to length(fVulkanFrameBufferColorAttachments)-1 do begin
  FreeAndNil(fVulkanFrameBufferColorAttachments[Index]);
 end;
 fVulkanFrameBufferColorAttachments:=nil;
 FreeAndNil(fVulkanDepthFrameBufferAttachment);
 for Index:=0 to length(fVulkanFrameBuffers)-1 do begin
  FreeAndNil(fVulkanFrameBuffers[Index]);
 end;
 fVulkanFrameBuffers:=nil;
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TpvApplication.DestroyVulkanFrameBuffers');
{$ifend}
end;

procedure TpvApplication.CreateVulkanCommandBuffers;
var Index:TpvInt32;
    ImageMemoryBarrier:TVkImageMemoryBarrier;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TpvApplication.CreateVulkanFrameBuffers');
{$ifend}

 DestroyVulkanCommandBuffers;

 fVulkanCommandPool:=TpvVulkanCommandPool.Create(fVulkanDevice,
                                               fVulkanDevice.GraphicsQueueFamilyIndex,
                                               TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 for Index:=0 to CountSwapChainImages-1 do begin

  fVulkanBlankCommandBuffers[Index]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fVulkanBlankCommandBufferSemaphores[Index]:=TpvVulkanSemaphore.Create(fVulkanDevice);

  if (fVulkanDevice.PresentQueueFamilyIndex<>fVulkanDevice.GraphicsQueueFamilyIndex) or
     ((assigned(fVulkanDevice.PresentQueue) and assigned(fVulkanDevice.GraphicsQueue)) and
      (fVulkanDevice.PresentQueue<>fVulkanDevice.GraphicsQueue)) then begin

   // If present and graphics queue families are different, then image barriers are required

   FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
   ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
   ImageMemoryBarrier.pNext:=nil;
   ImageMemoryBarrier.srcAccessMask:=0;
   ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
   ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_UNDEFINED;
   ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
   ImageMemoryBarrier.srcQueueFamilyIndex:=fVulkanDevice.PresentQueueFamilyIndex;
   ImageMemoryBarrier.dstQueueFamilyIndex:=fVulkanDevice.GraphicsQueueFamilyIndex;
   ImageMemoryBarrier.image:=fVulkanFrameBufferColorAttachments[Index].Image.Handle;
   ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
   ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
   ImageMemoryBarrier.subresourceRange.levelCount:=1;
   ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
   ImageMemoryBarrier.subresourceRange.layerCount:=1;

   fVulkanPresentToDrawImageBarrierCommandBuffers[Index]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
   fVulkanPresentToDrawImageBarrierCommandBufferSemaphores[Index]:=TpvVulkanSemaphore.Create(fVulkanDevice);
   fVulkanPresentToDrawImageBarrierCommandBuffers[Index].BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT));
   fVulkanPresentToDrawImageBarrierCommandBuffers[Index].CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                                                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                                                            0,
                                                                            0,nil,
                                                                            0,nil,
                                                                            1,@ImageMemoryBarrier);
   fVulkanPresentToDrawImageBarrierCommandBuffers[Index].EndRecording;

   FillChar(ImageMemoryBarrier,SizeOf(TVkImageMemoryBarrier),#0);
   ImageMemoryBarrier.sType:=VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
   ImageMemoryBarrier.pNext:=nil;
   ImageMemoryBarrier.srcAccessMask:=TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT);
   ImageMemoryBarrier.dstAccessMask:=TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT);
   ImageMemoryBarrier.oldLayout:=VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
   ImageMemoryBarrier.newLayout:=VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
   ImageMemoryBarrier.srcQueueFamilyIndex:=fVulkanDevice.GraphicsQueueFamilyIndex;
   ImageMemoryBarrier.dstQueueFamilyIndex:=fVulkanDevice.PresentQueueFamilyIndex;
   ImageMemoryBarrier.image:=fVulkanFrameBufferColorAttachments[Index].Image.Handle;
   ImageMemoryBarrier.subresourceRange.aspectMask:=TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT);
   ImageMemoryBarrier.subresourceRange.baseMipLevel:=0;
   ImageMemoryBarrier.subresourceRange.levelCount:=1;
   ImageMemoryBarrier.subresourceRange.baseArrayLayer:=0;
   ImageMemoryBarrier.subresourceRange.layerCount:=1;

   fVulkanDrawToPresentImageBarrierCommandBuffers[Index]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
   fVulkanDrawToPresentImageBarrierCommandBufferSemaphores[Index]:=TpvVulkanSemaphore.Create(fVulkanDevice);
   fVulkanDrawToPresentImageBarrierCommandBuffers[Index].BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT));
   fVulkanDrawToPresentImageBarrierCommandBuffers[Index].CmdPipelineBarrier(TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                                                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                                                            0,
                                                                            0,nil,
                                                                            0,nil,
                                                                            1,@ImageMemoryBarrier);
   fVulkanDrawToPresentImageBarrierCommandBuffers[Index].EndRecording;

  end else begin

   fVulkanPresentToDrawImageBarrierCommandBuffers[Index]:=nil;
   fVulkanPresentToDrawImageBarrierCommandBufferSemaphores[Index]:=nil;

   fVulkanDrawToPresentImageBarrierCommandBuffers[Index]:=nil;
   fVulkanDrawToPresentImageBarrierCommandBufferSemaphores[Index]:=nil;

  end;

 end;

{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TpvApplication.CreateVulkanCommandBuffers');
{$ifend}
end;

procedure TpvApplication.DestroyVulkanCommandBuffers;
var Index:TpvInt32;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TpvApplication.DestroyVulkanCommandBuffers');
{$ifend}
 for Index:=0 to CountSwapChainImages-1 do begin
  FreeAndNil(fVulkanBlankCommandBuffers[Index]);
  FreeAndNil(fVulkanBlankCommandBufferSemaphores[Index]);
  FreeAndNil(fVulkanPresentToDrawImageBarrierCommandBuffers[Index]);
  FreeAndNil(fVulkanPresentToDrawImageBarrierCommandBufferSemaphores[Index]);
  FreeAndNil(fVulkanDrawToPresentImageBarrierCommandBuffers[Index]);
  FreeAndNil(fVulkanDrawToPresentImageBarrierCommandBufferSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TpvApplication.DestroyVulkanCommandBuffers');
{$ifend}
end;

procedure TpvApplication.SetScreen(const aScreen:TpvApplicationScreen);
begin
 if fScreen<>aScreen then begin
  if assigned(fScreen) then begin
   if assigned(fVulkanSurface) then begin
    VulkanWaitIdle;
    if fGraphicsPipelinesReady then begin
     fScreen.BeforeDestroySwapChain;
    end else begin
     BeforeDestroySwapChain;
    end;
   end;
   fScreen.Pause;
   fScreen.Hide;
   fScreen.Free;
  end;
  fScreen:=aScreen;
  if assigned(fScreen) then begin
   fScreen.Show;
   if assigned(fScreen) then begin
    fScreen.Resize(fWidth,fHeight);
   end;
   fScreen.Resume;
   if assigned(fVulkanSurface) then begin
    VulkanWaitIdle;
    if fGraphicsPipelinesReady then begin
     fScreen.AfterCreateSwapChain;
    end else begin
     AfterCreateSwapChain;
    end;
   end;
   if CanBeParallelProcessed then begin
    // At parallel processing, skip the next first screen frame, due to double buffering at the parallel processing approach
    fSkipNextDrawFrame:=true;
   end;
  end;
 end;
end;

function TpvApplication.AcquireVulkanBackBuffer:boolean;
var ImageIndex:TpvInt32;
    TimeOut:TpvUInt64;
begin
 result:=false;

 if not assigned(fVulkanSwapChain) then begin
  fVulkanRecreationKind:=TpvApplicationVulkanRecreationKind.Surface;
 end;

 if fVulkanRecreationKind=TpvApplicationVulkanRecreationKind.None then begin

  if fVulkanPresentCompleteFencesReady[fRealUsedDrawSwapChainImageIndex] then begin
   if fVulkanPresentCompleteFences[fRealUsedDrawSwapChainImageIndex].GetStatus<>VK_SUCCESS then begin
    if fBlocking then begin
     fVulkanPresentCompleteFences[fRealUsedDrawSwapChainImageIndex].WaitFor;
    end else begin
     exit;
    end;
   end;
   fVulkanPresentCompleteFences[fRealUsedDrawSwapChainImageIndex].Reset;
   fVulkanPresentCompleteFencesReady[fRealUsedDrawSwapChainImageIndex]:=false;
  end;

  if not fBlocking then begin

   if fVulkanWaitFencesReady[fRealUsedDrawSwapChainImageIndex] then begin
    if fVulkanWaitFences[fRealUsedDrawSwapChainImageIndex].GetStatus<>VK_SUCCESS then begin
     exit;
    end;
    fVulkanWaitFences[fRealUsedDrawSwapChainImageIndex].Reset;
    fVulkanWaitFencesReady[fRealUsedDrawSwapChainImageIndex]:=false;
   end;

  end;

  if (fVulkanSwapChain.Width<>Width) or
     (fVulkanSwapChain.Height<>Height) or
     (fVulkanSwapChain.PresentMode<>PresentModeToVulkanPresentMode[fPresentMode]) then begin
   if fVulkanRecreationKind<TpvApplicationVulkanRecreationKind.SwapChain then begin
    fVulkanRecreationKind:=TpvApplicationVulkanRecreationKind.SwapChain;
   end;
   VulkanDebugLn('New surface dimension size and/or vertical synchronization setting detected!');
  end else begin
   try
    if fBlocking then begin
     TimeOut:=TpvUInt64(high(TpvUInt64));
    end else begin
     TimeOut:=0;
    end;
    case fVulkanSwapChain.AcquireNextImage(fVulkanPresentCompleteSemaphores[fDrawSwapChainImageIndex],
                                           fVulkanPresentCompleteFences[fDrawSwapChainImageIndex],
                                           TimeOut) of
     VK_SUCCESS:begin
      fVulkanPresentCompleteFencesReady[fDrawSwapChainImageIndex]:=true;
      fRealUsedDrawSwapChainImageIndex:=fVulkanSwapChain.CurrentImageIndex;
     end;
     VK_SUBOPTIMAL_KHR:begin
      if fVulkanRecreationKind<TpvApplicationVulkanRecreationKind.SwapChain then begin
       fVulkanRecreationKind:=TpvApplicationVulkanRecreationKind.SwapChain;
      end;
      VulkanDebugLn('Suboptimal surface detected!');
     end;
     else {VK_SUCCESS,VK_TIMEOUT:}begin
      exit;
     end;
    end;
   except
    on VulkanResultException:EpvVulkanResultException do begin
     case VulkanResultException.ResultCode of
      VK_ERROR_SURFACE_LOST_KHR:begin
       if fVulkanRecreationKind<TpvApplicationVulkanRecreationKind.Surface then begin
        fVulkanRecreationKind:=TpvApplicationVulkanRecreationKind.Surface;
       end;
       VulkanDebugLn(VulkanResultException.ClassName+': '+VulkanResultException.Message);
      end;
      VK_ERROR_OUT_OF_DATE_KHR,
      VK_SUBOPTIMAL_KHR:begin
       if fVulkanRecreationKind<TpvApplicationVulkanRecreationKind.SwapChain then begin
        fVulkanRecreationKind:=TpvApplicationVulkanRecreationKind.SwapChain;
       end;
       VulkanDebugLn(VulkanResultException.ClassName+': '+VulkanResultException.Message);
      end;
      else begin
       raise;
      end;
     end;
    end;
   end;
  end;

 end;

 if fVulkanRecreationKind in [TpvApplicationVulkanRecreationKind.SwapChain,TpvApplicationVulkanRecreationKind.Surface] then begin

  for ImageIndex:=0 to fCountSwapChainImages-1 do begin
   if fVulkanPresentCompleteFencesReady[ImageIndex] then begin
    fVulkanPresentCompleteFences[ImageIndex].WaitFor;
    fVulkanPresentCompleteFences[ImageIndex].Reset;
    fVulkanPresentCompleteFencesReady[ImageIndex]:=false;
   end;
   if fVulkanWaitFencesReady[ImageIndex] then begin
    fVulkanWaitFences[ImageIndex].WaitFor;
    fVulkanWaitFences[ImageIndex].Reset;
    fVulkanWaitFencesReady[ImageIndex]:=false;
   end;
  end;

  fVulkanDevice.WaitIdle;

  if fVulkanRecreationKind=TpvApplicationVulkanRecreationKind.Surface then begin
   VulkanDebugLn('Recreating vulkan surface... ');
  end else begin
   VulkanDebugLn('Recreating vulkan swap chain... ');
  end;
  if fVulkanTransferInflightCommandsFromOldSwapChain then begin
   fVulkanOldSwapChain:=fVulkanSwapChain;
  end else begin
   fVulkanOldSwapChain:=nil;
  end;
  try
   VulkanWaitIdle;
   BeforeDestroySwapChain;
   if fVulkanTransferInflightCommandsFromOldSwapChain then begin
    fVulkanSwapChain:=nil;
   end;
   DestroyVulkanCommandBuffers;
   DestroyVulkanFrameBuffers;
   DestroyVulkanRenderPass;
   DestroyVulkanSwapChain;
   if fVulkanRecreationKind=TpvApplicationVulkanRecreationKind.Surface then begin
    DestroyVulkanSurface;
    CreateVulkanSurface;
   end;
   CreateVulkanSwapChain;
   CreateVulkanRenderPass;
   CreateVulkanFrameBuffers;
   CreateVulkanCommandBuffers;
   VulkanWaitIdle;
   AfterCreateSwapChain;
  finally
   FreeAndNil(fVulkanOldSwapChain);
  end;
  if fVulkanRecreationKind=TpvApplicationVulkanRecreationKind.Surface then begin
   VulkanDebugLn('Recreated vulkan surface... ');
  end else begin
   VulkanDebugLn('Recreated vulkan swap chain... ');
  end;

  fVulkanRecreationKind:=TpvApplicationVulkanRecreationKind.None;

  fVulkanWaitSemaphore:=nil;
  fVulkanWaitFence:=nil;

 end else begin

  if fBlocking then begin
   if fVulkanWaitFencesReady[fRealUsedDrawSwapChainImageIndex] then begin
    if fVulkanWaitFences[fRealUsedDrawSwapChainImageIndex].GetStatus<>VK_SUCCESS then begin
     fVulkanWaitFences[fRealUsedDrawSwapChainImageIndex].WaitFor;
    end;
    fVulkanWaitFences[fRealUsedDrawSwapChainImageIndex].Reset;
    fVulkanWaitFencesReady[fRealUsedDrawSwapChainImageIndex]:=false;
   end;
  end;

  fVulkanWaitSemaphore:=fVulkanPresentCompleteSemaphores[fDrawSwapChainImageIndex];

  if assigned(fVulkanPresentToDrawImageBarrierCommandBuffers[fRealUsedDrawSwapChainImageIndex]) then begin
   // If present and graphics queue families are different, then a image barrier is required
   fVulkanPresentToDrawImageBarrierCommandBuffers[fRealUsedDrawSwapChainImageIndex].Execute(fVulkanDevice.GraphicsQueue,
                                                                                       TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                                                                       fVulkanWaitSemaphore,
                                                                                       fVulkanPresentToDrawImageBarrierCommandBufferSemaphores[fRealUsedDrawSwapChainImageIndex],
                                                                                       nil,
                                                                                       false);
   fVulkanWaitSemaphore:=fVulkanPresentToDrawImageBarrierCommandBufferSemaphores[fRealUsedDrawSwapChainImageIndex];
  end;

  if assigned(fVulkanDrawToPresentImageBarrierCommandBuffers[fRealUsedDrawSwapChainImageIndex]) then begin
   fVulkanWaitFence:=nil;
  end else begin
   fVulkanWaitFence:=fVulkanWaitFences[fRealUsedDrawSwapChainImageIndex];
  end;

  result:=true;

 end;
end;

function TpvApplication.PresentVulkanBackBuffer:boolean;
begin
 result:=false;

 if assigned(fVulkanDrawToPresentImageBarrierCommandBuffers[fRealUsedDrawSwapChainImageIndex]) then begin
  // If present and graphics queue families are different, then a image barrier is required
  fVulkanDrawToPresentImageBarrierCommandBuffers[fRealUsedDrawSwapChainImageIndex].Execute(fVulkanDevice.GraphicsQueue,
                                                                                           TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                                                                           fVulkanWaitSemaphore,
                                                                                           fVulkanDrawToPresentImageBarrierCommandBufferSemaphores[fRealUsedDrawSwapChainImageIndex],
                                                                                           fVulkanWaitFences[fRealUsedDrawSwapChainImageIndex],
                                                                                           false);
  fVulkanWaitSemaphore:=fVulkanDrawToPresentImageBarrierCommandBufferSemaphores[fRealUsedDrawSwapChainImageIndex];
  fVulkanWaitFence:=fVulkanWaitFences[fRealUsedDrawSwapChainImageIndex];
 end;
 fVulkanWaitFencesReady[fRealUsedDrawSwapChainImageIndex]:=true;

//fVulkanDevice.GraphicsQueue.WaitIdle; // A GPU/CPU graphics queue synchronization point only for debug cases here, when something got run wrong

 try
  case fVulkanSwapChain.QueuePresent(fVulkanDevice.GraphicsQueue,fVulkanWaitSemaphore) of
   VK_SUCCESS:begin
    //fVulkanDevice.WaitIdle; // A GPU/CPU frame synchronization point only for debug cases here, when something got run wrong
    inc(fDrawSwapChainImageIndex);
    if fDrawSwapChainImageIndex>=fCountSwapChainImages then begin
     dec(fDrawSwapChainImageIndex,fCountSwapChainImages);
    end;
    fUpdateSwapChainImageIndex:=fDrawSwapChainImageIndex+1;
    if fUpdateSwapChainImageIndex>=fCountSwapChainImages then begin
     dec(fUpdateSwapChainImageIndex,fCountSwapChainImages);
    end;
    result:=true;
   end;
   VK_SUBOPTIMAL_KHR:begin
    if fVulkanRecreationKind<TpvApplicationVulkanRecreationKind.SwapChain then begin
     fVulkanRecreationKind:=TpvApplicationVulkanRecreationKind.SwapChain;
    end;
   end;
  end;
 except
  on VulkanResultException:EpvVulkanResultException do begin
   case VulkanResultException.ResultCode of
    VK_ERROR_SURFACE_LOST_KHR:begin
     if fVulkanRecreationKind<TpvApplicationVulkanRecreationKind.Surface then begin
      fVulkanRecreationKind:=TpvApplicationVulkanRecreationKind.Surface;
     end;
    end;
    VK_ERROR_OUT_OF_DATE_KHR,
    VK_SUBOPTIMAL_KHR:begin
     if fVulkanRecreationKind<TpvApplicationVulkanRecreationKind.SwapChain then begin
      fVulkanRecreationKind:=TpvApplicationVulkanRecreationKind.SwapChain;
     end;
    end;
    else begin
     raise;
    end;
   end;
  end;
 end;

end;

procedure TpvApplication.SetNextScreen(const aNextScreen:TpvApplicationScreen);
begin
 if (fScreen<>aNextScreen) and (fNextScreen<>aNextScreen) then begin
  if assigned(fNextScreen) then begin
   fNextScreen.Free;
  end;
  fNextScreen:=aNextScreen;
  fHasNewNextScreen:=true;
 end;
end;

procedure TpvApplication.SetNextScreenClass(const aNextScreenClass:TpvApplicationScreenClass);
begin
 if (not (fScreen is aNextScreenClass)) and (fNextScreenClass<>aNextScreenClass) then begin
  fNextScreenClass:=aNextScreenClass;
  fHasNewNextScreen:=true;
 end;
end;

procedure TpvApplication.ReadConfig;
begin
end;

procedure TpvApplication.SaveConfig;
begin
end;

procedure TpvApplication.PostRunnable(const aRunnable:TpvApplicationRunnable);
var Index:TpvInt32;
begin
 fRunnableListCriticalSection.Acquire;
 try
  Index:=fRunnableListCount;
  inc(fRunnableListCount);
  if Index>=length(fRunnableList) then begin
   SetLength(fRunnableList,(Index+1)*2);
  end;
  fRunnableList[Index]:=aRunnable;
 finally
  fRunnableListCriticalSection.Release;
 end;
end;

procedure TpvApplication.AddLifecycleListener(const aLifecycleListener:TpvApplicationLifecycleListener);
begin
 fLifecycleListenerListCriticalSection.Acquire;
 try
  if fLifecycleListenerList.IndexOf(aLifecycleListener)<0 then begin
   fLifecycleListenerList.Add(aLifecycleListener);
  end;
 finally
  fLifecycleListenerListCriticalSection.Release;
 end;
end;

procedure TpvApplication.RemoveLifecycleListener(const aLifecycleListener:TpvApplicationLifecycleListener);
var Index:TpvInt32;
begin
 fLifecycleListenerListCriticalSection.Acquire;
 try
  Index:=fLifecycleListenerList.IndexOf(aLifecycleListener);
  if Index>=0 then begin
   fLifecycleListenerList.Delete(Index);
  end;
 finally
  fLifecycleListenerListCriticalSection.Release;
 end;
end;

procedure TpvApplication.Initialize;
begin
end;

procedure TpvApplication.Terminate;
begin
 fTerminated:=true;
end;

procedure TpvApplication.InitializeGraphics;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TpvApplication.InitializeGraphics');
{$ifend}
 if not fGraphicsReady then begin
  try
   fGraphicsReady:=true;
   CreateVulkanSurface;
   CreateVulkanSwapChain;
   CreateVulkanRenderPass;
   CreateVulkanFrameBuffers;
   CreateVulkanCommandBuffers;
   VulkanWaitIdle;
   AfterCreateSwapChain;
  except
   Terminate;
   raise;
  end;
 end;
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TpvApplication.InitializeGraphics');
{$ifend}
end;

procedure TpvApplication.DeinitializeGraphics;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TpvApplication.DeinitializeGraphics');
{$ifend}
 if fGraphicsReady then begin
  VulkanWaitIdle;
  BeforeDestroySwapChain;
  DestroyVulkanCommandBuffers;
  DestroyVulkanFrameBuffers;
  DestroyVulkanRenderPass;
  DestroyVulkanSwapChain;
  DestroyVulkanSurface;
  fGraphicsReady:=false;
 end;
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TpvApplication.DeinitializeGraphics');
{$ifend}
end;

procedure TpvApplication.InitializeAudio;
{$if defined(PasVulkanUseSDL2)}
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TpvApplication.InitializeAudio . . .');
{$ifend}
 if fUseAudio and not assigned(fAudio) then begin
  FillChar(fSDLWaveFormat,SizeOf(TSDL_AudioSpec),#0);
  fSDLWaveFormat.Channels:=2;
  fSDLWaveFormat.Format:=AUDIO_S16;
  fSDLWaveFormat.Freq:=44100;
  fSDLWaveFormat.Callback:=@SDLFillBuffer;
  fSDLWaveFormat.silence:=0;
  fSDLWaveFormat.Samples:=1024;
  fSDLWaveFormat.Size:=((fSDLWaveFormat.Samples*fSDLWaveFormat.Channels*(fSDLWaveFormat.Format and $ff))+7) shr 3;
  fAudio:=TpvAudio.Create(fSDLWaveFormat.Freq,
                          fSDLWaveFormat.Channels,
                          fSDLWaveFormat.Format and $ff,
                          fSDLWaveFormat.Samples);
  fAudio.SetMixerAGC(true);
  fAudio.UpdateHook:=UpdateAudioHook;
  fSDLWaveFormat.userdata:=fAudio;
  if SDL_OpenAudio(@fSDLWaveFormat,nil)<0 then begin
   raise EpvApplication.Create('SDL','Unable to initialize SDL audio: '+SDL_GetError,LOG_ERROR);
  end;
  SDL_PauseAudio(1);
 end;
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TpvApplication.InitializeAudio . . .');
{$ifend}
end;
{$else}
begin
 if fUseAudio and not assigned(fAudio) then begin
  Assert(false);
 end;
end;
{$ifend}

procedure TpvApplication.DeinitializeAudio;
{$if defined(PasVulkanUseSDL2)}
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TpvApplication.DeinitializeAudio . . .');
{$ifend}
 if assigned(fAudio) then begin
  SDL_CloseAudio;
  FreeAndNil(fAudio);
 end;
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TpvApplication.DeinitializeAudio . . .');
{$ifend}
end;
{$else}
begin
 if assigned(fAudio) then begin
  FreeAndNil(fAudio);
 end;
end;
{$ifend}

procedure TpvApplication.UpdateFrameTimesHistory;
var Index,Count:TpvInt32;
    SumOfFrameTimes:TpvDouble;
begin

 if fFloatDeltaTime>0.0 then begin

  fFrameTimesHistoryDeltaTimes[fFrameTimesHistoryWriteIndex]:=fFloatDeltaTime;
  fFrameTimesHistoryTimePoints[fFrameTimesHistoryWriteIndex]:=fNowTime;
  inc(fFrameTimesHistoryWriteIndex);
  if fFrameTimesHistoryWriteIndex>=FrameTimesHistorySize then begin
   fFrameTimesHistoryWriteIndex:=0;
  end;

  while (fFrameTimesHistoryReadIndex<>fFrameTimesHistoryWriteIndex) and
        ((fNowTime-fFrameTimesHistoryTimePoints[fFrameTimesHistoryReadIndex])>=fHighResolutionTimer.SecondInterval) do begin
   inc(fFrameTimesHistoryReadIndex);
   if fFrameTimesHistoryReadIndex>=FrameTimesHistorySize then begin
    fFrameTimesHistoryReadIndex:=0;
   end;
  end;
 end;

 SumOfFrameTimes:=0.0;
 Count:=0;
 Index:=fFrameTimesHistoryReadIndex;
 while Index<>fFrameTimesHistoryWriteIndex do begin
  SumOfFrameTimes:=SumOfFrameTimes+fFrameTimesHistoryDeltaTimes[Index];
  inc(Count);
  inc(Index);
  if Index>FrameTimesHistorySize then begin
   Index:=0;
  end;
 end;
 if (Count>0) and (SumOfFrameTimes>0.0) then begin
  fFramesPerSecond:=Count/SumOfFrameTimes;
 end else if fFloatDeltaTime>0.0 then begin
  fFramesPerSecond:=1.0/fFloatDeltaTime;
 end else begin
  fFramesPerSecond:=0.0;
 end;

end;

procedure TpvApplication.UpdateJobFunction(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32);
begin
 Update(Min(Max(fFloatDeltaTime,0.0),0.25));
end;

procedure TpvApplication.DrawJobFunction(const aJob:PPasMPJob;const aThreadIndex:TPasMPInt32);
begin
 Draw(fRealUsedDrawSwapChainImageIndex,fVulkanWaitSemaphore,fVulkanWaitFence);
end;

procedure TpvApplication.UpdateAudioHook;
begin
 UpdateAudio;
end;

procedure TpvApplication.ProcessRunnables;
var Index,Count:TpvInt32;
begin
 fRunnableListCriticalSection.Acquire;
 try
  Count:=fRunnableListCount;
  if Count>0 then begin
   Index:=0;
   while Index<Count do begin
    if assigned(fRunnableList[Index]) then begin
     fRunnableListCriticalSection.Release;
     try
      fRunnableList[Index]();
     finally
      fRunnableListCriticalSection.Acquire;
     end;
    end;
    inc(Index);
   end;
   if Count<fRunnableListCount then begin
    Count:=fRunnableListCount-Count;
    Index:=0;
    while Index<Count do begin
     fRunnableList[Index]:=fRunnableList[fRunnableListCount+Index];
     inc(Index);
    end;
    fRunnableListCount:=Count;
   end else begin
    fRunnableListCount:=0;
   end;
  end;
 finally
  fRunnableListCriticalSection.Release;
 end;
end;

function TpvApplication.IsVisibleToUser:boolean;
{$if defined(PasVulkanUseSDL2)}
const FullScreenFocusActiveFlags=SDL_WINDOW_SHOWN or SDL_WINDOW_INPUT_FOCUS {sor SDL_WINDOW_MOUSE_FOCUS};
var WindowFlags:TSDLUInt32;
begin
 WindowFlags:=SDL_GetWindowFlags(fSurfaceWindow);
 result:=((fCurrentFullScreen=0) or
          ((WindowFlags and FullScreenFocusActiveFlags)=FullScreenFocusActiveFlags)) and
         ((WindowFlags and SDL_WINDOW_MINIMIZED)=0);
end;
{$else}
begin
 result:=true;
end;
{$ifend}

function TpvApplication.WaitForReadyState:boolean;
{$if defined(PasVulkanUseSDL2)}
begin
 result:=true;
end;
{$elseif defined(fpc) and defined(Android)}
begin
 while not ((fAndroidReady and assigned(PANativeWindow)) or fAndroidQuit) do begin
  fAndroidAppProcessMessages(fAndroidApp,true);
 end;
 result:=fAndroidReady and not fAndroidQuit;
end;
{$else}
begin
 result:=false;
end;
{$ifend}

procedure TpvApplication.ProcessMessages;
var Index,Counter:TpvInt32;
    Joystick:TpvApplicationJoystick;
{$if defined(PasVulkanUseSDL2)}
    SDLJoystick:PSDL_Joystick;
    SDLGameController:PSDL_GameController;
{$else}
{$ifend}
    OK,Found,DoUpdateMainJoystick:boolean;
    Jobs:array[0..1] of PPasMPJob;
begin

 ProcessRunnables;

 DoUpdateMainJoystick:=false;

 if fCurrentHideSystemBars<>ord(fHideSystemBars) then begin
{$if defined(PasVulkanUseSDL2)}
  if fHideSystemBars then begin
   SDL_SetHint(SDL_HINT_ANDROID_HIDE_SYSTEM_BARS,'1');
  end else begin
   SDL_SetHint(SDL_HINT_ANDROID_HIDE_SYSTEM_BARS,'0');
  end;
{$else}
{$ifend}
 end;

 if fCurrentVisibleMouseCursor<>ord(fVisibleMouseCursor) then begin
  fCurrentVisibleMouseCursor:=ord(fVisibleMouseCursor);
{$if defined(PasVulkanUseSDL2)}
  if fVisibleMouseCursor then begin
   SDL_ShowCursor(1);
  end else begin
   SDL_ShowCursor(0);
  end;
{$else}
{$ifend}
 end;

 if fCurrentCatchMouse<>ord(fCatchMouse) then begin
  fCurrentCatchMouse:=ord(fCatchMouse);
{$if defined(PasVulkanUseSDL2)}
  if fCatchMouse then begin
   SDL_SetRelativeMouseMode(1);
  end else begin
   SDL_SetRelativeMouseMode(0);
  end;
{$else}
{$ifend}
 end;

 if fHasNewNextScreen then begin
  fHasNewNextScreen:=false;
  if assigned(fNextScreenClass) then begin
   SetScreen(fNextScreenClass.Create);
  end else if fScreen<>fNextScreen then begin
   SetScreen(fNextScreen);
  end;
  fNextScreen:=nil;
 end;

 if (fCurrentWidth<>fWidth) or (fCurrentHeight<>fHeight) or (fCurrentPresentMode<>TpvInt32(fPresentMode)) then begin
  fCurrentWidth:=fWidth;
  fCurrentHeight:=fHeight;
  fCurrentPresentMode:=TpvInt32(fPresentMode);
  if not fFullscreen then begin
{$if defined(PasVulkanUseSDL2)}
   SDL_SetWindowSize(fSurfaceWindow,fWidth,fHeight);
{$else}
{$ifend}
  end;
  if fGraphicsReady then begin
   DeinitializeGraphics;
   InitializeGraphics;
  end;
 end;

 fInput.fCriticalSection.Acquire;
 try
  fInput.fEventCount:=0;

  fInput.fMouseDeltaX:=0;
  fInput.fMouseDeltaY:=0;
  FillChar(fInput.fPointerDeltaX,SizeOf(fInput.fPointerDeltaX[0])*max(fInput.fMaxPointerID+1,0),AnsiChar(#0));
  FillChar(fInput.fPointerDeltaY,SizeOf(fInput.fPointerDeltaY[0])*max(fInput.fMaxPointerID+1,0),AnsiChar(#0));

{$if defined(PasVulkanUseSDL2)}
  if fLastPressedKeyEvent.SDLEvent.type_<>0 then begin
   if fKeyRepeatTimeAccumulator>0 then begin
    dec(fKeyRepeatTimeAccumulator,fDeltaTime);
    while fKeyRepeatTimeAccumulator<0 do begin
     inc(fKeyRepeatTimeAccumulator,fKeyRepeatInterval);
     fInput.AddEvent(fLastPressedKeyEvent);
    end;
   end;
  end;

  while SDL_PollEvent(@fEvent.SDLEvent)<>0 do begin
   if HandleEvent(fEvent) then begin
    continue;
   end;
   case fEvent.SDLEvent.type_ of
    SDL_QUITEV,
    SDL_APP_TERMINATING:begin
     VulkanWaitIdle;
     Pause;
     DeinitializeGraphics;
     Terminate;
    end;
    SDL_APP_LOWMEMORY:begin
     LowMemory;
    end;                       
    SDL_APP_WILLENTERBACKGROUND:begin
     writeln('SDL_APP_WILLENTERBACKGROUND');
{$if defined(fpc) and defined(android)}
     __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar(TpvApplicationRawByteString('SDL_APP_WILLENTERBACKGROUND')));
{$ifend}
     fActive:=false;
     VulkanWaitIdle;
     Pause;
     DeinitializeGraphics;
     fHasLastTime:=false;
    end;
    SDL_APP_DIDENTERBACKGROUND:begin
     writeln('SDL_APP_DIDENTERBACKGROUND');
{$if defined(fpc) and defined(android)}
     __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar(TpvApplicationRawByteString('SDL_APP_DIDENTERBACKGROUND')));
{$ifend}
    end;
    SDL_APP_WILLENTERFOREGROUND:begin
     writeln('SDL_APP_WILLENTERFOREGROUND');
{$if defined(fpc) and defined(android)}
     __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar(TpvApplicationRawByteString('SDL_APP_WILLENTERFOREGROUND')));
{$ifend}
    end;
    SDL_APP_DIDENTERFOREGROUND:begin
     writeln('SDL_APP_DIDENTERFOREGROUND');
     InitializeGraphics;
     Resume;
     fActive:=true;
     fHasLastTime:=false;
{$if defined(fpc) and defined(android)}
     __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar(TpvApplicationRawByteString('SDL_APP_DIDENTERFOREGROUND')));
{$ifend}
    end;
    SDL_RENDER_TARGETS_RESET,
    SDL_RENDER_DEVICE_RESET:begin
     VulkanWaitIdle;
     if fActive then begin
      Pause;
     end;
     if fGraphicsReady then begin
      DeinitializeGraphics;
      InitializeGraphics;
     end;
     if fActive then begin
      Resume;
     end;
     fHasLastTime:=false;
    end;
    SDL_WINDOWEVENT:begin
     case fEvent.SDLEvent.window.event of
      SDL_WINDOWEVENT_RESIZED:begin
       fWidth:=fEvent.SDLEvent.window.Data1;
       fHeight:=fEvent.SDLEvent.window.Data2;
{$if defined(PasVulkanUseSDL2) and defined(PasVulkanUseSDL2WithVulkanSupport)}
       if fSDLVersionWithVulkanSupport then begin
        SDL_Vulkan_GetDrawableSize(fSurfaceWindow,@fWidth,@fHeight);
       end;
{$ifend}
       fCurrentWidth:=fWidth;
       fCurrentHeight:=fHeight;
       if fGraphicsReady then begin
        VulkanDebugLn('New surface dimension size detected!');
{$if true}
        if fVulkanRecreationKind<TpvApplicationVulkanRecreationKind.SwapChain then begin
         fVulkanRecreationKind:=TpvApplicationVulkanRecreationKind.SwapChain;
        end;
{$else}
        DeinitializeGraphics;
        InitializeGraphics;
{$ifend}
       end;
       if assigned(fScreen) then begin
        fScreen.Resize(fWidth,fHeight);
       end;
      end;
     end;
    end;
    SDL_JOYDEVICEADDED:begin
     Index:=fEvent.SDLEvent.jdevice.which;
     Found:=false;
     for Counter:=0 to fInput.fJoysticks.Count-1 do begin
      Joystick:=TpvApplicationJoystick(fInput.fJoysticks.Items[Counter]);
      if assigned(Joystick) and (Joystick.Index=Index) then begin
       Found:=true;
       break;
      end;
     end;
     if not Found then begin
      if SDL_IsGameController(Index)<>0 then begin
       SDLGameController:=SDL_GameControllerOpen(Index);
       if assigned(SDLGameController) then begin
        SDLJoystick:=SDL_GameControllerGetJoystick(SDLGameController);
       end else begin
        SDLJoystick:=nil;
       end;
      end else begin
       SDLGameController:=nil;
       SDLJoystick:=SDL_JoystickOpen(Index);
      end;
      if assigned(SDLJoystick) then begin
       Joystick:=TpvApplicationJoystick.Create(Index,SDLJoystick,SDLGameController);
       if Index<fInput.fJoysticks.Count then begin
        fInput.fJoysticks.Items[Index]:=Joystick;
       end else begin
        while fInput.fJoysticks.Count<Index do begin
         fInput.fJoysticks.Add(nil);
        end;
        fInput.fJoysticks.Add(Joystick);
       end;
       Joystick.Initialize;
       DoUpdateMainJoystick:=true;
      end;
     end;
    end;
    SDL_JOYDEVICEREMOVED:begin
     for Counter:=0 to fInput.fJoysticks.Count-1 do begin
      Joystick:=TpvApplicationJoystick(fInput.fJoysticks.Items[Counter]);
      if assigned(Joystick) and (Joystick.ID=fEvent.SDLEvent.jdevice.which) then begin
       Joystick.Free;
       fInput.fJoysticks.Delete(Counter);
       DoUpdateMainJoystick:=true;
       break;
      end;
     end;
    end;
    SDL_CONTROLLERDEVICEADDED:begin
    end;
    SDL_CONTROLLERDEVICEREMOVED:begin
    end;
    SDL_CONTROLLERDEVICEREMAPPED:begin
    end;
    SDL_KEYDOWN:begin
     OK:=true;
     case fEvent.SDLEvent.key.keysym.sym of
      SDLK_F4:begin
       if ((fEvent.SDLEvent.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0) then begin
        OK:=false;
        if fEvent.SDLEvent.key.repeat_=0 then begin
         Terminate;
        end;
       end;
      end;
      SDLK_RETURN:begin
       if ((fEvent.SDLEvent.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0) then begin
        if fEvent.SDLEvent.key.repeat_=0 then begin
         OK:=false;
         fFullScreen:=not fFullScreen;
        end;
       end;
      end;
     end;
     if OK then begin
      if fNativeKeyRepeat then begin
       if fEvent.SDLEvent.key.repeat_=0 then begin
        fInput.AddEvent(fEvent);
       end;
       fEvent.SDLEvent.type_:=SDL_KEYTYPED;
       fInput.AddEvent(fEvent);
      end else if fEvent.SDLEvent.key.repeat_=0 then begin
       fInput.AddEvent(fEvent);
       fEvent.SDLEvent.type_:=SDL_KEYTYPED;
       fInput.AddEvent(fEvent);
       fLastPressedKeyEvent:=fEvent;
       fKeyRepeatTimeAccumulator:=fKeyRepeatInitialInterval;
      end;
     end;
    end;
    SDL_KEYUP:begin
     OK:=true;
     case fEvent.SDLEvent.key.keysym.sym of
      SDLK_F4:begin
       if ((fEvent.SDLEvent.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0) then begin
        OK:=false;
       end;
      end;
      SDLK_RETURN:begin
       if ((fEvent.SDLEvent.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0) then begin
        OK:=false;
       end;
      end;
     end;
     if OK then begin
      if fEvent.SDLEvent.key.repeat_=0 then begin
       fInput.AddEvent(fEvent);
       fLastPressedKeyEvent.SDLEvent.type_:=0;
      end;
     end;
    end;
    SDL_TEXTINPUT:begin
     fInput.AddEvent(fEvent);
    end;
    SDL_MOUSEMOTION:begin
     fInput.AddEvent(fEvent);
    end;
    SDL_MOUSEBUTTONDOWN:begin
     fInput.AddEvent(fEvent);
    end;
    SDL_MOUSEBUTTONUP:begin
     fInput.AddEvent(fEvent);
    end;
    SDL_MOUSEWHEEL:begin
     fInput.AddEvent(fEvent);
    end;
    SDL_FINGERMOTION:begin
     fInput.AddEvent(fEvent);
    end;
    SDL_FINGERDOWN:begin
     fInput.AddEvent(fEvent);
    end;
    SDL_FINGERUP:begin
     fInput.AddEvent(fEvent);
    end;
   end;
  end;
{$else}
{$ifend}
  if DoUpdateMainJoystick then begin
   fInput.fMainJoystick:=nil;
   if fInput.fJoysticks.Count>0 then begin
    for Counter:=0 to fInput.fJoysticks.Count-1 do begin
     Joystick:=TpvApplicationJoystick(fInput.fJoysticks.Items[Counter]);
     if assigned(Joystick) then begin
      fInput.fMainJoystick:=Joystick;
      break;
     end;
    end;
   end;
  end;
  fInput.ProcessEvents;
 finally
  fInput.fCriticalSection.Release;
 end;

 if assigned(fOnStep) then begin
  fOnStep(self);
 end;

 if fCurrentFullScreen<>ord(fFullScreen) then begin
  fCurrentFullScreen:=ord(fFullScreen);
  if fVulkanRecreationKind=TpvApplicationVulkanRecreationKind.None then begin
   fVulkanRecreationKind:=TpvApplicationVulkanRecreationKind.SwapChain;
  end;
{$if defined(PasVulkanUseSDL2)}
  if fFullScreen then begin
   SDL_SetWindowFullscreen(fSurfaceWindow,SDL_WINDOW_FULLSCREEN_DESKTOP);
  end else begin
   SDL_SetWindowFullscreen(fSurfaceWindow,0);
  end;
{$else}
{$ifend}
 end;

 if fGraphicsReady and IsVisibleToUser then begin

  if fSkipNextDrawFrame then begin

   fSkipNextDrawFrame:=false;

   fNowTime:=fHighResolutionTimer.GetTime;
   if fHasLastTime then begin
    fDeltaTime:=fNowTime-fLastTime;
   end else begin
    fDeltaTime:=0;
   end;
   fFloatDeltaTime:=fHighResolutionTimer.ToFloatSeconds(fDeltaTime);
   fLastTime:=fNowTime;
   fHasLastTime:=true;

   UpdateFrameTimesHistory;

   fUpdateFrameCounter:=fFrameCounter;
   fDrawFrameCounter:=fFrameCounter;
   UpdateJobFunction(nil,0);
   inc(fFrameCounter);

  end else begin

   if AcquireVulkanBackBuffer then begin

    fNowTime:=fHighResolutionTimer.GetTime;
    if fHasLastTime then begin
     fDeltaTime:=fNowTime-fLastTime;
    end else begin
     fDeltaTime:=0;
    end;
    fFloatDeltaTime:=fHighResolutionTimer.ToFloatSeconds(fDeltaTime);
    fLastTime:=fNowTime;
    fHasLastTime:=true;

    UpdateFrameTimesHistory;

    try

     if CanBeParallelProcessed and (fCountSwapChainImages>1) then begin

      fUpdateFrameCounter:=fFrameCounter;

      fDrawFrameCounter:=fFrameCounter-1;

      Jobs[0]:=fPasMPInstance.Acquire(UpdateJobFunction);
      Jobs[1]:=fPasMPInstance.Acquire(DrawJobFunction);
      fPasMPInstance.Invoke(Jobs);

     end else begin

      fUpdateFrameCounter:=fFrameCounter;

      fDrawFrameCounter:=fFrameCounter;

      UpdateJobFunction(nil,0);

      DrawJobFunction(nil,0);

     end;

    finally
     PresentVulkanBackBuffer;
    end;

    inc(fFrameCounter);

   end;

  end;

 end else begin

  fDeltaTime:=0;

 end;

end;

procedure TpvApplication.Run;
var Index:TpvInt32;
{$if defined(Android)}
    AndroidEnv:PJNIEnv;
    AndroidActivity,AndroidFile,AndroidNull:jobject;
    AndroidGetCacheDir,AndroidGetFilesDir,AndroidGetNoBackupFilesDir,
    AndroidGetExternalFilesDir,AndroidGetAbsolutePath:jmethodID;
    AndroidActivityClass,AndroidFileClass:jclass;
    AndroidPath:jstring;
{$ifend}
begin
 VulkanDisableFloatingPointExceptions;

{$if defined(Android)}

 AndroidEnv:=SDL_AndroidGetJNIEnv;

 AndroidActivity:=SDL_AndroidGetActivity;

 AndroidActivityClass:=AndroidEnv^.GetObjectClass(AndroidEnv,AndroidActivity);

 AndroidGetCacheDir:=AndroidEnv^.GetMethodID(AndroidEnv,AndroidActivityClass,'getCacheDir','()Ljava/io/File;');

 AndroidGetFilesDir:=AndroidEnv^.GetMethodID(AndroidEnv,AndroidActivityClass,'getFilesDir','()Ljava/io/File;');

 AndroidGetNoBackupFilesDir:=AndroidEnv^.GetMethodID(AndroidEnv,AndroidActivityClass,'getNoBackupFilesDir','()Ljava/io/File;');

 AndroidGetExternalFilesDir:=AndroidEnv^.GetMethodID(AndroidEnv,AndroidActivityClass,'getExternalFilesDir','(Ljava/lang/String;)Ljava/io/File;');

 AndroidFileClass:=AndroidEnv^.FindClass(AndroidEnv,'java/io/File');
 AndroidGetAbsolutePath:=AndroidEnv^.GetMethodID(AndroidEnv,AndroidFileClass,'getAbsolutePath','()Ljava/lang/String;');

 AndroidFile:=AndroidEnv^.CallObjectMethod(AndroidEnv,AndroidActivity,AndroidGetCacheDir);
 try
  AndroidPath:=AndroidEnv^.CallObjectMethod(AndroidEnv,AndroidFile,AndroidGetAbsolutePath);
  if assigned(AndroidPath) then begin
   try
    fCacheStoragePath:=IncludeTrailingPathDelimiter(JStringToString(AndroidEnv,AndroidPath));
    if length(fCacheStoragePath)=0 then begin
     fCacheStoragePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'cache');
     if not DirectoryExists(fCacheStoragePath) then begin
      CreateDir(fCacheStoragePath);
     end;
    end;
   finally
    FreeJString(AndroidEnv,AndroidPath);
   end;
  end else begin
   fCacheStoragePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'cache');
   if not DirectoryExists(fCacheStoragePath) then begin
    CreateDir(fCacheStoragePath);
   end;
  end;
 finally
  AndroidEnv^.DeleteLocalRef(AndroidEnv,AndroidFile);
 end;

 AndroidFile:=AndroidEnv^.CallObjectMethod(AndroidEnv,AndroidActivity,AndroidGetNoBackupFilesDir);
 try
  AndroidPath:=AndroidEnv^.CallObjectMethod(AndroidEnv,AndroidFile,AndroidGetAbsolutePath);
  if assigned(AndroidPath) then begin
   try
    fLocalStoragePath:=IncludeTrailingPathDelimiter(JStringToString(AndroidEnv,AndroidPath));
    if length(fLocalStoragePath)=0 then begin
     fLocalStoragePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'local');
     if not DirectoryExists(fLocalStoragePath) then begin
      CreateDir(fLocalStoragePath);
     end;
    end;
   finally
    FreeJString(AndroidEnv,AndroidPath);
   end;
  end else begin
   fLocalStoragePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'local');
   if not DirectoryExists(fLocalStoragePath) then begin
    CreateDir(fLocalStoragePath);
   end;
  end;
 finally
  AndroidEnv^.DeleteLocalRef(AndroidEnv,AndroidFile);
 end;

 AndroidFile:=AndroidEnv^.CallObjectMethod(AndroidEnv,AndroidActivity,AndroidGetFilesDir);
 try
  AndroidPath:=AndroidEnv^.CallObjectMethod(AndroidEnv,AndroidFile,AndroidGetAbsolutePath);
  if assigned(AndroidPath) then begin
   try
    fRoamingStoragePath:=IncludeTrailingPathDelimiter(JStringToString(AndroidEnv,AndroidPath));
    if length(fRoamingStoragePath)=0 then begin
     fRoamingStoragePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'local');
     if not DirectoryExists(fRoamingStoragePath) then begin
      CreateDir(fRoamingStoragePath);
     end;
    end;
   finally
    FreeJString(AndroidEnv,AndroidPath);
   end;
  end else begin
   fRoamingStoragePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetAppConfigDir(false))+'local');
   if not DirectoryExists(fRoamingStoragePath) then begin
    CreateDir(fRoamingStoragePath);
   end;
  end;
 finally
  AndroidEnv^.DeleteLocalRef(AndroidEnv,AndroidFile);
 end;

 AndroidNull:=nil;
 AndroidFile:=AndroidEnv^.CallObjectMethodA(AndroidEnv,AndroidActivity,AndroidGetExternalFilesDir,@AndroidNull);
 try
  AndroidPath:=AndroidEnv^.CallObjectMethod(AndroidEnv,AndroidFile,AndroidGetAbsolutePath);
  if assigned(AndroidPath) then begin
   try
    fExternalStoragePath:=IncludeTrailingPathDelimiter(JStringToString(AndroidEnv,AndroidPath));
    if length(fExternalStoragePath)=0 then begin
     fExternalStoragePath:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('EXTERNAL_STORAGE'));
    end;
   finally
    FreeJString(AndroidEnv,AndroidPath);
   end;
  end else begin
   fExternalStoragePath:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('EXTERNAL_STORAGE'));
  end;
 finally
  AndroidEnv^.DeleteLocalRef(AndroidEnv,AndroidFile);
 end;

{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar('Cache storage data path: '+fCacheStoragePath));
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar('Local storage data path: '+fLocalStoragePath));
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar('Roaming storage data path: '+fRoamingStoragePath));
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar('External storage data path: '+fExternalStoragePath));
{$ifend}

{$elseif (defined(Windows) or defined(Linux) or defined(Unix)) and not defined(Android)}

 fCacheStoragePath:=GetAppDataCacheStoragePath(fPathName);

 fLocalStoragePath:=GetAppDataLocalStoragePath(fPathName);

 fRoamingStoragePath:=GetAppDataRoamingStoragePath(fPathName);

{$if defined(Windows)}
 fExternalStoragePath:='C:\';
{$else}
 fExternalStoragePath:='/';
{$ifend}

{$if not defined(Release)}
 Log(LOG_VERBOSE,'PasVulkanApplication','Cache storage data path: '+fCacheStoragePath);
 Log(LOG_VERBOSE,'PasVulkanApplication','Local storage data path: '+fLocalStoragePath);
 Log(LOG_VERBOSE,'PasVulkanApplication','Roaming storage data path: '+fRoamingStoragePath);
 Log(LOG_VERBOSE,'PasVulkanApplication','External storage data path: '+fExternalStoragePath);
{$ifend}

{$ifend}

 fVulkanPipelineCacheFileName:=IncludeTrailingPathDelimiter(fCacheStoragePath)+'vulkan_pipeline_cache.bin';

 ReadConfig;

{$if defined(PasVulkanUseSDL2)}
 SDL_GetVersion(fSDLVersion);

{$if defined(PasVulkanUseSDL2WithVulkanSupport)}
 fSDLVersionWithVulkanSupport:=(fSDLVersion.Major>=3) or
                               ((fSDLVersion.Major=2) and
                                (((fSDLVersion.Minor=0) and (fSDLVersion.Patch>=6)) or
                                 (fSDLVersion.Minor>=1)
                                )
                               );

 if fSDLVersionWithVulkanSupport then begin
  SDL_Vulkan_LoadLibrary(VK_DEFAULT_LIB_NAME);
 end;
{$ifend}
{$ifend}

{$if defined(PasVulkanUseSDL2)}
 if fAndroidSeparateMouseAndTouch then begin
  SDL_SetHint(SDL_HINT_ANDROID_SEPARATE_MOUSE_AND_TOUCH,'1');
 end else begin
  SDL_SetHint(SDL_HINT_ANDROID_SEPARATE_MOUSE_AND_TOUCH,'0');
 end;
{$else}
{$ifend}

{$if defined(PasVulkanUseSDL2)}
 if fHideSystemBars then begin
  SDL_SetHint(SDL_HINT_ANDROID_HIDE_SYSTEM_BARS,'1');
 end else begin
  SDL_SetHint(SDL_HINT_ANDROID_HIDE_SYSTEM_BARS,'0');
 end;
{$else}
{$ifend}
 fCurrentHideSystemBars:=ord(fHideSystemBars);

 if WaitForReadyState then begin

{$if defined(PasVulkanUseSDL2)}
  if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_EVENTS or SDL_INIT_TIMER)<0 then begin
   raise EpvApplication.Create('SDL','Unable to initialize SDL: '+SDL_GetError,LOG_ERROR);
  end;
{$else}
{$ifend}

{$if defined(Unix) and not defined(Android)}
 InstallSignalHandlers;
{$ifend}

{$if defined(PasVulkanUseSDL2)}
  if SDL_GetCurrentDisplayMode(0,@fSDLDisplayMode)=0 then begin
   fScreenWidth:=fSDLDisplayMode.w;
   fScreenHeight:=fSDLDisplayMode.h;
  end else begin
   fScreenWidth:=-1;
   fScreenHeight:=-1;
  end;
{$else}
  fScreenWidth:=-1;
  fScreenHeight:=-1;
{$ifend}

{$if defined(PasVulkanUseSDL2)}
  fVideoFlags:=SDL_WINDOW_ALLOW_HIGHDPI;
{$if defined(PasVulkanUseSDL2WithVulkanSupport)}
  if fSDLVersionWithVulkanSupport then begin
   fVideoFlags:=fVideoFlags or SDL_WINDOW_VULKAN;
  end;
{$ifend}
  if fFullscreen then begin
{$ifndef Android}
   if (fWidth=fScreenWidth) and (fHeight=fScreenHeight) then begin
    fVideoFlags:=fVideoFlags or SDL_WINDOW_FULLSCREEN_DESKTOP;
   end else begin
    fVideoFlags:=fVideoFlags or SDL_WINDOW_FULLSCREEN;
   end;
{$endif}
   fCurrentFullscreen:=ord(true);
  end else begin
   fCurrentFullscreen:=0;
  end;
{$ifndef Android}
  if fResizable then begin
   fVideoFlags:=fVideoFlags or SDL_WINDOW_RESIZABLE;
  end;
{$endif}

{$if defined(fpc) and defined(android)}
  fVideoFlags:=fVideoFlags or SDL_WINDOW_FULLSCREEN or SDL_WINDOW_VULKAN;
  fFullscreen:=true;
  fCurrentFullscreen:=ord(true);
  fWidth:=fScreenWidth;
  fHeight:=fScreenHeight;
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar(TpvApplicationRawByteString('Window size: '+IntToStr(fWidth)+'x'+IntToStr(fHeight))));
{$ifend}

{$if defined(PasVulkanUseSDL2WithVulkanSupport)}
  repeat
{$ifend}
   fSurfaceWindow:=SDL_CreateWindow(PAnsiChar(TpvApplicationRawByteString(fTitle)),
{$ifdef Android}
                                    SDL_WINDOWPOS_CENTERED_MASK,
                                    SDL_WINDOWPOS_CENTERED_MASK,
{$else}
                                    ((fScreenWidth-fWidth)+1) div 2,
                                    ((fScreenHeight-fHeight)+1) div 2,
{$endif}
                                    fWidth,
                                    fHeight,
                                    SDL_WINDOW_SHOWN or fVideoFlags);
   if not assigned(fSurfaceWindow) then begin
{$if defined(PasVulkanUseSDL2WithVulkanSupport)}
    // For faulty SDL2 >= 2.0.6 builds with corrupt or missing builtin Vulkan support
    if fSDLVersionWithVulkanSupport then begin
     fSDLVersionWithVulkanSupport:=false;
     fVideoFlags:=fVideoFlags and not SDL_WINDOW_VULKAN;
     continue;
    end;
{$ifend}
    raise EpvApplication.Create('SDL','Unable to initialize SDL: '+SDL_GetError,LOG_ERROR);
   end;
{$if defined(PasVulkanUseSDL2WithVulkanSupport)}
   break;
  until false;
{$ifend}
{$else}
{$ifend}

{$if defined(PasVulkanUseSDL2) and defined(PasVulkanUseSDL2WithVulkanSupport)}
  if fSDLVersionWithVulkanSupport then begin
   SDL_Vulkan_GetDrawableSize(fSurfaceWindow,@fWidth,@fHeight);
  end;
{$ifend}

  fCurrentWidth:=fWidth;
  fCurrentHeight:=fHeight;

  fCurrentPresentMode:=TpvInt32(fPresentMode);

{$if defined(PasVulkanUseSDL2)}
 {SDL_EventState(SDL_MOUSEMOTION,SDL_ENABLE);
  SDL_EventState(SDL_MOUSEBUTTONDOWN,SDL_ENABLE);
  SDL_EventState(SDL_MOUSEBUTTONUP,SDL_ENABLE);
  SDL_EventState(SDL_KEYDOWN,SDL_ENABLE);
  SDL_EventState(SDL_KEYUP,SDL_ENABLE);
  SDL_EventState(SDL_QUITEV,SDL_ENABLE);
  SDL_EventState(SDL_WINDOWEVENT,SDL_ENABLE);}
{$else}
{$ifend}

  FillChar(fFrameTimesHistoryDeltaTimes,SizeOf(fFrameTimesHistoryDeltaTimes),#0);
  FillChar(fFrameTimesHistoryTimePoints,SizeOf(fFrameTimesHistoryTimePoints),#$ff);
  fFrameTimesHistoryReadIndex:=0;
  fFrameTimesHistoryWriteIndex:=0;

  fFramesPerSecond:=0.0;

  fSkipNextDrawFrame:=false;

  try

   CreateVulkanInstance;
   try

    Start;
    try

     InitializeGraphics;
     try

      InitializeAudio;
      try

       Load;
       try

        fLifecycleListenerListCriticalSection.Acquire;
        try
         for Index:=0 to fLifecycleListenerList.Count-1 do begin
          if TpvApplicationLifecycleListener(fLifecycleListenerList[Index]).Resume then begin
           break;
          end;
         end;
        finally
         fLifecycleListenerListCriticalSection.Release;
        end;

        if assigned(fStartScreen) then begin
         SetScreen(fStartScreen.Create);
        end;
        try

         if assigned(fAudio) then begin
{$if defined(PasVulkanUseSDL2)}
          SDL_PauseAudio(0);
{$else}
{$ifend}
         end;
         try

          while not fTerminated do begin
           ProcessMessages;
          end;

         finally
          if assigned(fAudio) then begin
{$if defined(PasVulkanUseSDL2)}
           SDL_PauseAudio(1);
{$else}
{$ifend}
          end;
         end;

        finally

         SetScreen(nil);

         FreeAndNil(fNextScreen);
         FreeAndNil(fScreen);

        end;

        fLifecycleListenerListCriticalSection.Acquire;
        try
         for Index:=0 to fLifecycleListenerList.Count-1 do begin
          if TpvApplicationLifecycleListener(fLifecycleListenerList[Index]).Pause then begin
           break;
          end;
         end;
         for Index:=0 to fLifecycleListenerList.Count-1 do begin
          if TpvApplicationLifecycleListener(fLifecycleListenerList[Index]).Terminate then begin
           break;
          end;
         end;
        finally
         fLifecycleListenerListCriticalSection.Release;
        end;

       finally

        VulkanWaitIdle;

        Unload;

       end;

      finally
       DeinitializeAudio;
      end;

     finally
      DeinitializeGraphics;
     end;

    finally

     Stop;

    end;

   finally
    DestroyVulkanInstance;
   end;

  finally

{$if defined(PasVulkanUseSDL2)}
   if assigned(fSurfaceWindow) then begin
    SDL_DestroyWindow(fSurfaceWindow);
    fSurfaceWindow:=nil;
   end;
{$else}
{$ifend}

   SaveConfig;

  end;

 end;

{$if defined(PasVulkanUseSDL2) and defined(PasVulkanUseSDL2WithVulkanSupport)}
 if fSDLVersionWithVulkanSupport then begin
  SDL_Vulkan_UnloadLibrary;
 end;
{$ifend}

end;

procedure TpvApplication.Setup;
begin
end;

procedure TpvApplication.Start;
begin
end;

procedure TpvApplication.Stop; 
begin
end;

procedure TpvApplication.Load;
begin
end;

procedure TpvApplication.Unload;
begin
end;

procedure TpvApplication.Resume;
var Index:TpvInt32;
begin

 fLifecycleListenerListCriticalSection.Acquire;
 try
  for Index:=0 to fLifecycleListenerList.Count-1 do begin
   if TpvApplicationLifecycleListener(fLifecycleListenerList[Index]).Resume then begin
    break;
   end;
  end;
 finally
  fLifecycleListenerListCriticalSection.Release;
 end;

 if assigned(fScreen) then begin
  fScreen.Resume;
 end;

end;

procedure TpvApplication.Pause;
var Index:TpvInt32;
begin

 if assigned(fScreen) then begin
  fScreen.Pause;
 end;

 fLifecycleListenerListCriticalSection.Acquire;
 try
  for Index:=0 to fLifecycleListenerList.Count-1 do begin
   if TpvApplicationLifecycleListener(fLifecycleListenerList[Index]).Pause then begin
    break;
   end;
  end;
 finally
  fLifecycleListenerListCriticalSection.Release;
 end;

end;

procedure TpvApplication.LowMemory;
var Index:TpvInt32;
begin

 fLifecycleListenerListCriticalSection.Acquire;
 try
  for Index:=0 to fLifecycleListenerList.Count-1 do begin
   if TpvApplicationLifecycleListener(fLifecycleListenerList[Index]).LowMemory then begin
    break;
   end;
  end;
 finally
  fLifecycleListenerListCriticalSection.Release;
 end;

 if assigned(fScreen) then begin
  fScreen.LowMemory;
 end;

end;

procedure TpvApplication.Resize(const aWidth,aHeight:TpvInt32);
begin
 if assigned(fScreen) then begin
  fScreen.Resize(aWidth,aHeight);
 end;
end;

procedure TpvApplication.AfterCreateSwapChain;
begin
 if assigned(fScreen) then begin
  fScreen.AfterCreateSwapChain;
 end;
end;                           

procedure TpvApplication.BeforeDestroySwapChain;
begin
 if assigned(fScreen) then begin
  fScreen.BeforeDestroySwapChain;
 end;
end;

function TpvApplication.HandleEvent(const aEvent:TpvApplicationEvent):boolean;
begin
 if assigned(fOnEvent) and fOnEvent(self,aEvent) then begin
  result:=true;
 end else if assigned(fScreen) and fScreen.HandleEvent(aEvent) then begin
  result:=true;
 end else begin
  result:=false;
 end;
end;

function TpvApplication.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 if assigned(fScreen) then begin
  result:=fScreen.KeyEvent(aKeyEvent);
 end else begin
  result:=false;
 end;
end;

function TpvApplication.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
begin
 if assigned(fScreen) then begin
  result:=fScreen.PointerEvent(aPointerEvent);
 end else begin
  result:=false;
 end;
end;

function TpvApplication.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 if assigned(fScreen) then begin
  result:=fScreen.Scrolled(aRelativeAmount);
 end else begin
  result:=false;
 end;
end;

function TpvApplication.CanBeParallelProcessed:boolean;
begin
 if assigned(fScreen) then begin
  result:=fScreen.CanBeParallelProcessed;
 end else begin
  result:=false;
 end;
end;

procedure TpvApplication.Update(const aDeltaTime:TpvDouble);
begin
 if assigned(fScreen) then begin
  fScreen.Update(aDeltaTime);
 end;
end;

procedure TpvApplication.UpdateAudio;
begin
 if assigned(fScreen) then begin
  fScreen.UpdateAudio;
 end;
end;

procedure TpvApplication.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
var VulkanCommandBuffer:TpvVulkanCommandBuffer;
begin
 if assigned(fScreen) then begin
  fScreen.Draw(aSwapChainImageIndex,aWaitSemaphore,aWaitFence);
 end else begin

  VulkanCommandBuffer:=fVulkanBlankCommandBuffers[aSwapChainImageIndex];

  VulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

  VulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

  fVulkanRenderPass.BeginRenderPass(VulkanCommandBuffer,
                                    fVulkanFrameBuffers[aSwapChainImageIndex],
                                    VK_SUBPASS_CONTENTS_INLINE,
                                    0,
                                    0,
                                    fVulkanSwapChain.Width,
                                    fVulkanSwapChain.Height);

  fVulkanRenderPass.EndRenderPass(VulkanCommandBuffer);

  VulkanCommandBuffer.Execute(fVulkanDevice.GraphicsQueue,
                              TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                              aWaitSemaphore,
                              fVulkanBlankCommandBufferSemaphores[aSwapChainImageIndex],
                              aWaitFence,
                              false);

  aWaitSemaphore:=fVulkanBlankCommandBufferSemaphores[aSwapChainImageIndex];

 end;
end;

class procedure TpvApplication.Main;
begin
 pvApplication:=self.Create;
 try
  pvApplication.Setup;
  pvApplication.Run;
 finally
  FreeAndNil(pvApplication);
 end;
end;

{$if defined(fpc) and defined(android)}
function AndroidGetManufacturerName:TpvApplicationUnicodeString;
var AndroisOSBuild:jclass;
    ManufacturerID:JFieldID;
    ManufacturerStringObject:JString;
    ManufacturerStringLength:JSize;
    ManufacturerStringChars:PJChar;
begin
 result:='';
 AndroisOSBuild:=AndroidJavaEnv^.FindClass(AndroidJavaEnv,'android/os/Build');
 ManufacturerID:=AndroidJavaEnv^.GetStaticFieldID(AndroidJavaEnv,AndroisOSBuild,'MANUFACTURER','Ljava/lang/String;');
 ManufacturerStringObject:=AndroidJavaEnv^.GetStaticObjectField(AndroidJavaEnv,AndroisOSBuild,ManufacturerID);
 ManufacturerStringLength:=AndroidJavaEnv^.GetStringLength(AndroidJavaEnv,ManufacturerStringObject);
 ManufacturerStringChars:=AndroidJavaEnv^.GetStringChars(AndroidJavaEnv,ManufacturerStringObject,nil);
 if assigned(ManufacturerStringChars) then begin
  if ManufacturerStringLength>0 then begin
   SetLength(result,ManufacturerStringLength);
   Move(ManufacturerStringChars^,result[1],ManufacturerStringLength*SizeOf(WideChar));
  end;
  AndroidJavaEnv^.ReleaseStringChars(AndroidJavaEnv,ManufacturerStringObject,ManufacturerStringChars);
 end;
end;

function AndroidGetModelName:TpvApplicationUnicodeString;
var AndroisOSBuild:jclass;
    ModelID:JFieldID;
    ModelStringObject:JString;
    ModelStringLength:JSize;
    ModelStringChars:PJChar;
begin
 result:='';
 AndroisOSBuild:=AndroidJavaEnv^.FindClass(AndroidJavaEnv,'android/os/Build');
 ModelID:=AndroidJavaEnv^.GetStaticFieldID(AndroidJavaEnv,AndroisOSBuild,'MODEL','Ljava/lang/String;');
 ModelStringObject:=AndroidJavaEnv^.GetStaticObjectField(AndroidJavaEnv,AndroisOSBuild,ModelID);
 ModelStringLength:=AndroidJavaEnv^.GetStringLength(AndroidJavaEnv,ModelStringObject);
 ModelStringChars:=AndroidJavaEnv^.GetStringChars(AndroidJavaEnv,ModelStringObject,nil);
 if assigned(ModelStringChars) then begin
  if ModelStringLength>0 then begin
   SetLength(result,ModelStringLength);
   Move(ModelStringChars^,result[1],ModelStringLength*SizeOf(WideChar));
  end;
  AndroidJavaEnv^.ReleaseStringChars(AndroidJavaEnv,ModelStringObject,ModelStringChars);
 end;
end;

function AndroidGetDeviceName:TpvApplicationUnicodeString;
begin
 result:=AndroidGetManufacturerName+' '+AndroidGetModelName;
end;

function Android_JNI_GetEnv:PJNIEnv; cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_JNI_GetEnv . . .');
{$ifend}
 if assigned(pvApplication) then begin
  result:=AndroidJavaEnv;
 end else begin
  result:=nil;
 end;
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_JNI_GetEnv . . .');
{$ifend}
end;

{$if not defined(PasVulkanUseSDL2)}
function LibCMalloc(Size:ptruint):pointer; cdecl; external 'c' name 'malloc';
procedure LibCFree(p:pointer); cdecl; external 'c' name 'free';

function DumpExceptionCallStack(e:Exception):string;
var i:int32;
    Frames:PPointer;
begin
 result:='Program exception! '+LineEnding+'Stack trace:'+LineEnding+LineEnding;
 if assigned(e) then begin
  result:=result+'Exception class: '+e.ClassName+LineEnding+'Message: '+E.Message+LineEnding;
 end;
 result:=result+BackTraceStrFunc(ExceptAddr);
 Frames:=ExceptFrames;
 for i:=0 to ExceptFrameCount-1 do begin
  result:=result+LineEnding+BackTraceStrFunc(Frames);
  inc(Frames);
 end;
end;

type PLooperID=^TLooperID;
     TLooperID=
      (
       LOOPER_ID_MAIN=1,
       LOOPER_ID_INPUT,
       LOOPER_ID_USER
      );

     PAppCmd=^TAppCmd;
     TAppCmd=
      (
       APP_CMD_INPUT_CHANGED=1,
       APP_CMD_INIT_WINDOW,
       APP_CMD_TERM_WINDOW,
       APP_CMD_WINDOW_RESIZED,
       APP_CMD_WINDOW_REDRAW_NEEDED,
       APP_CMD_CONTENT_RECT_CHANGED,
       APP_CMD_GAINED_FOCUS,
       APP_CMD_LOST_FOCUS,
       APP_CMD_CONFIG_CHANGED,
       APP_CMD_LOW_MEMORY,
       APP_CMD_START,
       APP_CMD_RESUME,
       APP_CMD_SAVE_STATE,
       APP_CMD_PAUSE,
       APP_CMD_STOP,
       APP_CMD_DESTROY
      );

     PAndroidApp=^TAndroidApp;

     PAndroidPollSource=^TAndroidPollSource;
     TAndroidPollSource=packed record
      public
       fID:TLooperID;
       fApp:PAndroidApp;
       fProcess:procedure(const aApp:PAndroidApp;const aSource:PAndroidPollSource); cdecl;
     end;

     TAndroidAppThread=class(TThread)
      private
       fAndroidApp:PAndroidApp;
      protected
       procedure Execute;
      public
       constructor Create(const aAndroidApp:PAndroidApp);
     end;

     TAndroidApp=packed record
      public
       fUserData:TpvPointer;
       fActivity:PANativeActivity;
       fConfiguration:PAConfiguration;
       fSavedState:TpvPointer;
       fSavedStateSize:TPasMPUInt32;
       fLooper:PALooper;
       fInputQueue:PAInputQueue;
       fWindow:PANativeWindow;
       fContentRect:TARect;
       fActivityState:TAppCmd;
       fDestroyRequested:TPasMPBool32;
       fConditionVariableLock:TPasMPConditionVariableLock;
       fConditionVariable:TPasMPConditionVariable;
       fApplication:TpvApplication;
       fApplicationClass:TpvApplicationClass;
       fCmdPollSource:TAndroidPollSource;
       fInputPollSource:TAndroidPollSource;
       fRunning:TPasMPBool32;
       fStateSaved:TPasMPBool32;
       fDestroyed:TPasMPBool32;
       fRedrawNeeded:TPasMPBool32;
       fPendingInputQueue:PAInputQueue;
       fPendingWindow:PANativeWindow;
       fPendingContentRect:TARect;
       fMsgPipe:TFilDes;
       constructor Create(const aActivity:PANativeActivity;
                          const aApplicationClass:TpvApplicationClass;
                          const aSavedState:TpvPointer;
                          const aSavedStateSize:TpvNativeUInt);
       procedure Destroy;
       function AllocateSavedState(const aSize:TpvNativeUInt):TpvPointer;
       procedure FreeSavedState;
       procedure SendCmd(const aCmd:TAppCmd);
       procedure SetInput(const aInputQueue:PAInputQueue);
       procedure SetWindow(const aWindow:PANativeWindow);
       procedure SetActivityState(const aCmd:TAppCmd);
       procedure ProcessInputEvent(const aEvent:PAInputEvent);
       procedure ProcessCmd(const aCmd:TAppCmd);
     end;

procedure AppProcessInput(const aApp:PAndroidApp;const aSource:PAndroidPollSource); cdecl;
var Event:PAInputEvent;
    Handled,Processed:boolean;
begin
 Event:=nil;
 Processed:=false;
 while AInputQueue_getEvent(aApp^.fInputQueue,@Event)>=0 do begin
  if AInputQueue_preDispatchEvent(aApp^.fInputQueue,Event)=0 then begin
   Handled:=false;
   aApp^.ProcessInputEvent(Event);
   AInputQueue_finishEvent(aApp^.fInputQueue,Event,IfThen(Handled,1,0));
   Processed:=true;
  end;
 end;
 if not Processed then begin
  __android_log_write(ANDROID_LOG_ERROR,'PasVulkanApplication','Failure reading next input event . . .');
 end;
end;

procedure AppProcessCmd(const aApp:PAndroidApp;const aSource:PAndroidPollSource); cdecl;
var Cmd:TAppCmd;
begin
 if fpread(aApp^.fMsgPipe[0],@Cmd,SizeOf(TAppCmd))=SizeOf(TAppCmd) then begin
  case Cmd of
   APP_CMD_INPUT_CHANGED:begin
    aApp^.fConditionVariableLock.Acquire;
    try
     if assigned(aApp^.fInputQueue) then begin
      AInputQueue_detachLooper(aApp^.fInputQueue);
     end;
     aApp^.fInputQueue:=aApp^.fPendingInputQueue;
     if assigned(aApp^.fInputQueue) then begin
      AInputQueue_attachLooper(aApp^.fInputQueue,aApp^.fLooper,TpvInt32(LOOPER_ID_INPUT),nil,@aApp^.fInputPollSource);
     end;
     aApp^.fConditionVariable.Broadcast;
    finally
     aApp^.fConditionVariableLock.Release;
    end;
   end;
   APP_CMD_INIT_WINDOW:begin
    aApp^.fConditionVariableLock.Acquire;
    try
     aApp^.fWindow:=aApp^.fPendingWindow;
     aApp^.fConditionVariable.Broadcast;
    finally
     aApp^.fConditionVariableLock.Release;
    end;
   end;
   APP_CMD_TERM_WINDOW:begin
    aApp^.fConditionVariableLock.Acquire;
    try
     aApp^.fWindow:=nil;
     aApp^.fConditionVariable.Broadcast;
    finally
     aApp^.fConditionVariableLock.Release;
    end;
   end;
   APP_CMD_WINDOW_RESIZED:begin
   end;
   APP_CMD_WINDOW_REDRAW_NEEDED:begin
   end;
   APP_CMD_CONTENT_RECT_CHANGED:begin
   end;
   APP_CMD_GAINED_FOCUS:begin
   end;
   APP_CMD_LOST_FOCUS:begin
   end;
   APP_CMD_CONFIG_CHANGED:begin
    AConfiguration_fromAssetManager(aApp^.fConfiguration,aApp^.fActivity^.assetManager);
   end;
   APP_CMD_LOW_MEMORY:begin
   end;
   APP_CMD_START:begin
    aApp^.fConditionVariableLock.Acquire;
    try
     aApp^.fActivityState:=APP_CMD_START;
     aApp^.fConditionVariable.Broadcast;
    finally
     aApp^.fConditionVariableLock.Release;
    end;
   end;
   APP_CMD_RESUME:begin
    aApp^.fConditionVariableLock.Acquire;
    try
     aApp^.fActivityState:=APP_CMD_RESUME;
     aApp^.fConditionVariable.Broadcast;
    finally
     aApp^.fConditionVariableLock.Release;
    end;
   end;
   APP_CMD_SAVE_STATE:begin
    aApp^.FreeSavedState;
   end;
   APP_CMD_PAUSE:begin
    aApp^.fConditionVariableLock.Acquire;
    try
     aApp^.fActivityState:=APP_CMD_PAUSE;
     aApp^.fConditionVariable.Broadcast;
    finally
     aApp^.fConditionVariableLock.Release;
    end;
   end;
   APP_CMD_STOP:begin
    aApp^.fConditionVariableLock.Acquire;
    try
     aApp^.fActivityState:=APP_CMD_STOP;
     aApp^.fConditionVariable.Broadcast;
    finally
     aApp^.fConditionVariableLock.Release;
    end;
   end;
   APP_CMD_DESTROY:begin
    TPasMPInterlocked.Write(aApp^.fDestroyRequested,true);
   end;
  end;
  aApp^.ProcessCmd(Cmd);
  case Cmd of
   APP_CMD_INPUT_CHANGED:begin
   end;
   APP_CMD_INIT_WINDOW:begin
   end;
   APP_CMD_TERM_WINDOW:begin
    aApp^.fConditionVariableLock.Acquire;
    try
     aApp^.fWindow:=nil;
     aApp^.fConditionVariable.Broadcast;
    finally
     aApp^.fConditionVariableLock.Release;
    end;
   end;
   APP_CMD_WINDOW_RESIZED:begin
   end;
   APP_CMD_WINDOW_REDRAW_NEEDED:begin
   end;
   APP_CMD_CONTENT_RECT_CHANGED:begin
   end;
   APP_CMD_GAINED_FOCUS:begin
   end;
   APP_CMD_LOST_FOCUS:begin
   end;
   APP_CMD_CONFIG_CHANGED:begin
   end;
   APP_CMD_LOW_MEMORY:begin
   end;
   APP_CMD_START:begin
   end;
   APP_CMD_RESUME:begin
   end;
   APP_CMD_SAVE_STATE:begin
    aApp^.fConditionVariableLock.Acquire;
    try
     TPasMPInterlocked.Write(aApp^.fStateSaved,true);
     aApp^.fConditionVariable.Broadcast;
    finally
     aApp^.fConditionVariableLock.Release;
    end;
   end;
   APP_CMD_PAUSE:begin
   end;
   APP_CMD_STOP:begin
   end;
   APP_CMD_DESTROY:begin
   end;
  end;
 end else begin
  __android_log_write(ANDROID_LOG_ERROR,'PasVulkanApplication','Pipe read error . . .');
 end;
end;

procedure AndroidAppProcessMessages(const aAndroidApp:TpvPointer;const aWait:boolean);
var Events:TpvInt32;
    Source:PAndroidPollSource;
begin
 Events:=0;
 Source:=nil;
 while ALooper_pollAll(IfThen(aWait,-1,0),nil,@Events,@Source)>=0 do begin
  if assigned(Source) then begin
   Source^.fProcess(Source^.fApp,Source);
  end;
 end;
end;

constructor TAndroidAppThread.Create(const aAndroidApp:PAndroidApp);
begin
 fAndroidApp:=aAndroidApp;
 FreeOnTerminate:=true;
 inherited Create(false);
end;

procedure TAndroidAppThread.Execute;
var Looper:PALooper;
begin
 try

{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TAndroidAppThread.Execute . . .');
{$ifend}

  try

   fAndroidApp^.fConfiguration:=AConfiguration_new();
   AConfiguration_fromAssetManager(fAndroidApp^.fConfiguration,fAndroidApp^.fActivity^.assetManager);

   fAndroidApp^.fCmdPollSource.fID:=LOOPER_ID_MAIN;
   fAndroidApp^.fCmdPollSource.fApp:=fAndroidApp;
   fAndroidApp^.fCmdPollSource.fProcess:=@AppProcessCmd;

   fAndroidApp^.fInputPollSource.fID:=LOOPER_ID_INPUT;
   fAndroidApp^.fInputPollSource.fApp:=fAndroidApp;
   fAndroidApp^.fInputPollSource.fProcess:=@AppProcessInput;

   Looper:=ALooper_prepare(ALOOPER_PREPARE_ALLOW_NON_CALLBACKS);
   ALooper_addFd(Looper,fAndroidApp^.fMsgPipe[0],TpvInt32(LOOPER_ID_MAIN),ALOOPER_EVENT_INPUT,nil,@fAndroidApp^.fCmdPollSource);
   fAndroidApp^.fLooper:=Looper;

   fAndroidApp^.fApplication:=fAndroidApp^.fApplicationClass.Create;
   try
    fAndroidApp^.fApplication.fAndroidApp:=fAndroidApp;
    fAndroidApp^.fApplication.fAndroidWindow:=nil;
    fAndroidApp^.fApplication.fAndroidReady:=false;
    fAndroidApp^.fApplication.fAndroidQuit:=false;
    fAndroidApp^.fApplication.fAndroidAppProcessMessages:=AndroidAppProcessMessages;
    fAndroidApp^.fApplication.Setup;
    fAndroidApp^.fConditionVariableLock.Acquire;
    try
     TPasMPInterlocked.Write(fAndroidApp^.fRunning,true);
     fAndroidApp^.fConditionVariable.Broadcast;
    finally
     fAndroidApp^.fConditionVariableLock.Release;
    end;
    try
     fAndroidApp^.fApplication.Run;
    finally
     fAndroidApp^.fConditionVariableLock.Acquire;
     try
      TPasMPInterlocked.Write(fAndroidApp^.fRunning,false);
      fAndroidApp^.fConditionVariable.Broadcast;
     finally
      fAndroidApp^.fConditionVariableLock.Release;
     end;
    end;
   finally
    FreeAndNil(fAndroidApp^.fApplication);
   end;

  finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
   __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TAndroidAppThread.Execute . . .');
{$ifend}
  end;

 finally
  fAndroidApp^.fConditionVariableLock.Acquire;
  try
   TPasMPInterlocked.Write(fAndroidApp^.fDestroyed,true);
   fAndroidApp^.fConditionVariable.Broadcast;
  finally
   fAndroidApp^.fConditionVariableLock.Release;
  end;
 end;
end;

constructor TAndroidApp.Create(const aActivity:PANativeActivity;
                               const aApplicationClass:TpvApplicationClass;
                               const aSavedState:TpvPointer;
                               const aSavedStateSize:TpvNativeUInt);
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TAndroidApp.Create . . .');
{$ifend}
 try
  try

   FillChar(self,SizeOf(TAndroidApp),#0);

   fActivity:=aActivity;

   fConditionVariableLock:=TPasMPConditionVariableLock.Create;

   fConditionVariable:=TPasMPConditionVariable.Create;

   if assigned(aSavedState) then begin
    fSavedState:=AllocateSavedState(aSavedStateSize);
    fSavedStateSize:=aSavedStateSize;
    Move(aSavedState^,fSavedState^,aSavedStateSize);
   end;

   if fppipe(fMsgPipe)<>0 then begin
    raise Exception.Create('fppipe');
   end;

   TAndroidAppThread.Create(@self);

   fConditionVariableLock.Acquire;
   try
    while not TPasMPInterlocked.Read(fRunning) do begin
     fConditionVariable.Wait(fConditionVariableLock);
    end;
   finally
    fConditionVariableLock.Release;
   end;

  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TAndroidApp.Create . . .');
{$ifend}
 end;
end;

procedure TAndroidApp.Destroy;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TAndroidApp.Destroy . . .');
{$ifend}
 try
  try

   fConditionVariableLock.Acquire;
   try
    SendCmd(APP_CMD_DESTROY);
    while not TPasMPInterlocked.Read(fDestroyed) do begin
     fConditionVariable.Wait(fConditionVariableLock);
    end;
   finally
    fConditionVariableLock.Release;
   end;

   fpclose(fMsgPipe[0]);
   fpclose(fMsgPipe[1]);

   FreeAndNil(fConditionVariable);
   FreeAndNil(fConditionVariableLock);

  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TAndroidApp.Destroy . . .');
{$ifend}
 end;
end;

function TAndroidApp.AllocateSavedState(const aSize:TpvNativeUInt):TpvPointer;
begin
 result:=LibCMalloc(aSize);
end;

procedure TAndroidApp.FreeSavedState;
begin
 if assigned(fSavedState) then begin
  LibCFree(fSavedState);
  fSavedState:=nil;
  fSavedStateSize:=0;
 end;
end;

procedure TAndroidApp.SendCmd(const aCmd:TAppCmd);
begin
 if fpwrite(fMsgPipe[1],aCmd,SizeOf(TAppCmd))<>SizeOf(TAppCmd) then begin
 end;
end;

procedure TAndroidApp.SetInput(const aInputQueue:PAInputQueue);
begin
 fConditionVariableLock.Acquire;
 try
  fPendingInputQueue:=aInputQueue;
  SendCmd(APP_CMD_INPUT_CHANGED);
  while fInputQueue<>fPendingInputQueue do begin
   fConditionVariable.Wait(fConditionVariableLock);
  end;
 finally
  fConditionVariableLock.Release;
 end;
end;

procedure TAndroidApp.SetWindow(const aWindow:PANativeWindow);
begin
 fConditionVariableLock.Acquire;
 try
  if assigned(fPendingWindow) then begin
   SendCmd(APP_CMD_TERM_WINDOW);
  end;
  fPendingWindow:=aWindow;
  if assigned(aWindow) then begin
   SendCmd(APP_CMD_INIT_WINDOW);
  end;
  while fWindow<>fPendingWindow do begin
   fConditionVariable.Wait(fConditionVariableLock);
  end;
 finally
  fConditionVariableLock.Release;
 end;
end;

procedure TAndroidApp.SetActivityState(const aCmd:TAppCmd);
begin
 fConditionVariableLock.Acquire;
 try
  SendCmd(aCmd);
  while fActivityState<>aCmd do begin
   fConditionVariable.Wait(fConditionVariableLock);
  end;
 finally
  fConditionVariableLock.Release;
 end;
end;

procedure TAndroidApp.ProcessInputEvent(const aEvent:PAInputEvent);
var EventType,EventSource,EventKeyCode,EventAction:TpvInt32;
begin
 if assigned(aEvent) then begin
  EventType:=AInputEvent_getType(aEvent);
  case EventType of
   AINPUT_EVENT_TYPE_KEY:begin
    EventKeyCode:=AKeyEvent_getKeyCode(aEvent);
    EventAction:=AKeyEvent_getAction(aEvent);
    if (EventKeyCode<>AKEYCODE_UNKNOWN) and
       (EventAction in [AKEY_EVENT_ACTION_DOWN,
                        AKEY_EVENT_ACTION_UP,
                        AKEY_EVENT_ACTION_MULTIPLE]) then begin

    end;
   end;
   AINPUT_EVENT_TYPE_MOTION:begin
    EventSource:=AInputEvent_getSource(aEvent);
    case EventSource of
     AINPUT_SOURCE_JOYSTICK:begin
     end;
     AINPUT_SOURCE_TOUCHSCREEN:begin
     end;
     AINPUT_SOURCE_MOUSE:begin
     end;
    end;
   end;
  end;
 end;
end;

procedure TAndroidApp.ProcessCmd(const aCmd:TAppCmd);
begin
 case aCmd of
  APP_CMD_INPUT_CHANGED:begin
  end;
  APP_CMD_INIT_WINDOW:begin
   if assigned(fApplication) then begin
    fApplication.fAndroidWindow:=fWindow;
    fApplication.fAndroidReady:=true;
   end;
  end;
  APP_CMD_TERM_WINDOW:begin
   if assigned(fApplication) then begin
    fApplication.fAndroidWindow:=nil;
    fApplication.fAndroidReady:=false;
   end;
  end;
  APP_CMD_WINDOW_RESIZED:begin
  end;
  APP_CMD_WINDOW_REDRAW_NEEDED:begin
  end;
  APP_CMD_CONTENT_RECT_CHANGED:begin
  end;
  APP_CMD_GAINED_FOCUS:begin
  end;
  APP_CMD_LOST_FOCUS:begin
  end;
  APP_CMD_CONFIG_CHANGED:begin
  end;
  APP_CMD_LOW_MEMORY:begin
  end;
  APP_CMD_START:begin
  end;
  APP_CMD_RESUME:begin
  end;
  APP_CMD_SAVE_STATE:begin
  end;
  APP_CMD_PAUSE:begin
  end;
  APP_CMD_STOP:begin
   if assigned(fApplication) then begin
    fApplication.fAndroidQuit:=true;
   end;
   ANativeActivity_finish(fActivity);
  end;
  APP_CMD_DESTROY:begin
   if assigned(fApplication) then begin
    fApplication.fAndroidQuit:=true;
   end;
  end;
 end;
end;

procedure Android_ANativeActivity_onStart(aActivity:PANativeActivity); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onStart . . .');
{$ifend}
 try
  try
   PAndroidApp(aActivity^.instance)^.SetActivityState(APP_CMD_START);
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onStart . . .');
{$ifend}
 end;
end;

procedure Android_ANativeActivity_onResume(aActivity:PANativeActivity); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onResime . . .');
{$ifend}
 try
  try
   PAndroidApp(aActivity^.instance)^.SetActivityState(APP_CMD_RESUME);
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onResume . . .');
{$ifend}
 end;
end;

function Android_ANativeActivity_onSaveInstanceState(aActivity:PANativeActivity;aOutSize:Psize_t):TpvPointer; cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onSaveInstanceState . . .');
{$ifend}
 try
  try
   PAndroidApp(aActivity^.instance)^.fConditionVariableLock.Acquire;
   try
    PAndroidApp(aActivity^.instance)^.fStateSaved:=false;
    PAndroidApp(aActivity^.instance)^.SendCmd(APP_CMD_SAVE_STATE);
    while not PAndroidApp(aActivity^.instance)^.fStateSaved do begin
     PAndroidApp(aActivity^.instance)^.fConditionVariable.Wait(PAndroidApp(aActivity^.instance)^.fConditionVariableLock);
    end;
    if assigned(PAndroidApp(aActivity^.instance)^.fSavedState) then begin
     result:=TPasMPInterlocked.Exchange(PAndroidApp(aActivity^.instance)^.fSavedState,nil);
     aOutSize^:=TPasMPInterlocked.Exchange(PAndroidApp(aActivity^.instance)^.fSavedStateSize,0);
    end else begin
     result:=nil;
    end;
   finally
    PAndroidApp(aActivity^.instance)^.fConditionVariableLock.Release;
   end;
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onSaveInstanceState . . .');
{$ifend}
 end;
end;

procedure Android_ANativeActivity_onPause(aActivity:PANativeActivity); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onPause . . .');
{$ifend}
 try
  try
   PAndroidApp(aActivity^.instance)^.SetActivityState(APP_CMD_PAUSE);
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onPause . . .');
{$ifend}
 end;
end;

procedure Android_ANativeActivity_onStop(aActivity:PANativeActivity); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onStop . .');
{$ifend}
 try
  try
   PAndroidApp(aActivity^.instance)^.SetActivityState(APP_CMD_STOP);
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onStop . . .');
{$ifend}
 end;
end;

procedure Android_ANativeActivity_onDestroy(aActivity:PANativeActivity); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onDestroy . . .');
{$ifend}
 try
  try
   PAndroidApp(aActivity^.instance)^.Destroy;
   LibCFree(aActivity^.instance);
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onDestroy . . .');
{$ifend}
 end;
end;

procedure Android_ANativeActivity_onWindowFocusChanged(aActivity:PANativeActivity;aHasFocus:cint); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onWindowFocusChanged . .');
{$ifend}
 try
  try
   if aHasFocus<>0 then begin
    PAndroidApp(aActivity^.instance)^.SetActivityState(APP_CMD_GAINED_FOCUS);
   end else begin
    PAndroidApp(aActivity^.instance)^.SetActivityState(APP_CMD_LOST_FOCUS);
   end;
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onWindowFocusChanged . . .');
{$ifend}
 end;
end;

procedure Android_ANativeActivity_onNativeWindowCreated(aActivity:PANativeActivity;aWindow:PANativeWindow); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onNativeWindowCreated . .');
{$ifend}
 try
  try
   PAndroidApp(aActivity^.instance)^.SetWindow(aWindow);
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onNativeWindowCreated . . .');
{$ifend}
 end;
end;

procedure Android_ANativeActivity_onNativeWindowResized(aActivity:PANativeActivity;aWindow:PANativeWindow); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onNativeWindowResized . .');
{$ifend}
 try
  try
   PAndroidApp(aActivity^.instance)^.SendCmd(APP_CMD_WINDOW_RESIZED);
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onNativeWindowResized . . .');
{$ifend}
 end;
end;

procedure Android_ANativeActivity_onNativeWindowRedrawNeeded(aActivity:PANativeActivity;aWindow:PANativeWindow); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onNativeWindowRedrawNeeded . .');
{$ifend}
 try
  try
   PAndroidApp(aActivity^.instance)^.SendCmd(APP_CMD_WINDOW_REDRAW_NEEDED);
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onNativeWindowRedrawNeeded . . .');
{$ifend}
 end;
end;

procedure Android_ANativeActivity_onNativeWindowDestroyed(aActivity:PANativeActivity;aWindow:PANativeWindow); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onNativeWindowDestroyed . .');
{$ifend}
 try
  try
   PAndroidApp(aActivity^.instance)^.SetWindow(nil);
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onNativeWindowDestroyed . . .');
{$ifend}
 end;
end;

procedure Android_ANativeActivity_onInputQueueCreated(aActivity:PANativeActivity;aQueue:PAInputQueue); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onInputQueueCreated . .');
{$ifend}
 try
  try
   PAndroidApp(aActivity^.instance)^.SetInput(aQueue);
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onInputQueueCreated . . .');
{$ifend}
 end;
end;

procedure Android_ANativeActivity_onInputQueueDestroyed(aActivity:PANativeActivity;aQueue:PAInputQueue); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onInputQueueDestroyed . .');
{$ifend}
 try
  try
   PAndroidApp(aActivity^.instance)^.SetInput(nil);
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onInputQueueDestroyed . . .');
{$ifend}
 end;
end;

procedure Android_ANativeActivity_onContentRectChanged(aActivity:PANativeActivity;aRect:PARect); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onContentRectChanged . .');
{$ifend}
 try
  try
   PAndroidApp(aActivity^.instance)^.SendCmd(APP_CMD_CONTENT_RECT_CHANGED);
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onContentRectChanged . . .');
{$ifend}
 end;
end;

procedure Android_ANativeActivity_onConfigurationChanged(aActivity:PANativeActivity); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onConfigurationChanged . .');
{$ifend}
 try
  try
   PAndroidApp(aActivity^.instance)^.SendCmd(APP_CMD_CONFIG_CHANGED);
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onConfigurationChanged . . .');
{$ifend}
 end;
end;

procedure Android_ANativeActivity_onLowMemory(aActivity:PANativeActivity); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onLowMemory . .');
{$ifend}
 try
  try
   PAndroidApp(aActivity^.instance)^.SendCmd(APP_CMD_LOW_MEMORY);
  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onLowMemory . . .');
{$ifend}
 end;
end;

procedure Android_ANativeActivity_onCreate(aActivity:PANativeActivity;aSavedState:pointer;aSavedStateSize:cuint32;const aApplicationClass:TpvApplicationClass);
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_ANativeActivity_onCreate . . .');
{$ifend}
 try

  try

   AndroidActivity:=aActivity;

   AndroidSavedState:=aSavedState;
   AndroidSavedStateSize:=aSavedStateSize;

   AndroidJavaVM:=aActivity^.VM;
   AndroidJavaEnv:=aActivity^.env;
   AndroidJavaObject:=aActivity^.clazz;

   AndroidAssetManager:=aActivity^.assetManager;

   AndroidInternalDataPath:=aActivity^.internalDataPath;
   AndroidExternalDataPath:=aActivity^.externalDataPath;
   AndroidLibraryPath:=IncludeTrailingPathDelimiter(ExtractFilePath(aActivity^.internalDataPath))+'lib';

   aActivity^.callbacks^.onStart:=@Android_ANativeActivity_onStart;
   aActivity^.callbacks^.onResume:=@Android_ANativeActivity_onResume;
   aActivity^.callbacks^.onSaveInstanceState:=@Android_ANativeActivity_onSaveInstanceState;
   aActivity^.callbacks^.onStop:=@Android_ANativeActivity_onStop;
   aActivity^.callbacks^.onPause:=@Android_ANativeActivity_onPause;
   aActivity^.callbacks^.onDestroy:=@Android_ANativeActivity_onDestroy;
   aActivity^.callbacks^.onWindowFocusChanged:=@Android_ANativeActivity_onWindowFocusChanged;
   aActivity^.callbacks^.onNativeWindowCreated:=@Android_ANativeActivity_onNativeWindowCreated;
   aActivity^.callbacks^.onNativeWindowResized:=@Android_ANativeActivity_onNativeWindowResized;
   aActivity^.callbacks^.onNativeWindowRedrawNeeded:=@Android_ANativeActivity_onNativeWindowRedrawNeeded;
   aActivity^.callbacks^.onNativeWindowDestroyed:=@Android_ANativeActivity_onNativeWindowDestroyed;
   aActivity^.callbacks^.onInputQueueCreated:=@Android_ANativeActivity_onInputQueueCreated;
   aActivity^.callbacks^.onInputQueueDestroyed:=@Android_ANativeActivity_onInputQueueDestroyed;
   aActivity^.callbacks^.onContentRectChanged:=@Android_ANativeActivity_onContentRectChanged;
   aActivity^.callbacks^.onConfigurationChanged:=@Android_ANativeActivity_onConfigurationChanged;
   aActivity^.callbacks^.onLowMemory:=@Android_ANativeActivity_onLowMemory;

   aActivity^.instance:=LibCMalloc(SizeOf(TAndroidApp));

   PAndroidApp(aActivity^.instance)^:=TAndroidApp.Create(aActivity,aApplicationClass,aSavedState,aSavedStateSize);

  except
   on e:Exception do begin
    __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(DumpExceptionCallStack(e))));
   end;
  end;
 finally
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_ANativeActivity_onCreate . . .');
{$ifend}
 end;
end;
{$ifend}

{$ifend}

{$ifdef Windows}
initialization
 timeBeginPeriod(1);
finalization
 timeEndPeriod(1);
{$endif}
end.
