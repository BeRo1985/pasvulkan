(******************************************************************************
 *                              PasVulkanApplication                          *
 ******************************************************************************
 *                        Version 2017-05-13-13-51-0000                       *
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
 * 5. Write code, which is compatible with Delphi 7-XE7 and FreePascal >= 3.0 *
 *    so don't use generics/templates, operator overloading and another newer *
 *    syntax features than Delphi 7 has support for that, but if needed, make *
 *    it out-ifdef-able.                                                      *
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
unit PasVulkanApplication;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}

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
     Vulkan,
     PasVulkan,
     PasVulkanSDL2,
     PasVulkanAndroid;

const MaxSwapChainImages=3;

      FrameTimesHistorySize=1 shl 10;

      LOG_NONE=0;
      LOG_ERROR=1;
      LOG_INFO=2;
      LOG_DEBUG=3;

      EVENT_NONE=0;
      EVENT_KEY_DOWN=1;
      EVENT_KEY_UP=2;
      EVENT_KEY_TYPED=3;
      EVENT_TOUCH_DOWN=4;
      EVENT_TOUCH_UP=5;
      EVENT_TOUCH_DRAGGED=6;
      EVENT_MOUSE_MOVED=7;
      EVENT_SCROLLED=8;

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
      KEYCODE_a=97;
      KEYCODE_b=98;
      KEYCODE_c=99;
      KEYCODE_d=100;
      KEYCODE_e=101;
      KEYCODE_f=102;
      KEYCODE_g=103;
      KEYCODE_h=104;
      KEYCODE_i=105;
      KEYCODE_j=106;
      KEYCODE_k=107;
      KEYCODE_l=108;
      KEYCODE_m=109;
      KEYCODE_n=110;
      KEYCODE_o=111;
      KEYCODE_p=112;
      KEYCODE_q=113;
      KEYCODE_r=114;
      KEYCODE_s=115;
      KEYCODE_t=116;
      KEYCODE_u=117;
      KEYCODE_v=118;
      KEYCODE_w=119;
      KEYCODE_x=120;
      KEYCODE_y=121;
      KEYCODE_z=122;
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

      KEYMODIFIER_NONE=$0000;
      KEYMODIFIER_LSHIFT=$0001;
      KEYMODIFIER_RSHIFT=$0002;
      KEYMODIFIER_LCTRL=$0040;
      KEYMODIFIER_RCTRL=$0080;
      KEYMODIFIER_LALT=$0100;
      KEYMODIFIER_RALT=$0200;
      KEYMODIFIER_LMETA=$0400;
      KEYMODIFIER_RMETA=$0800;
      KEYMODIFIER_NUM=$1000;
      KEYMODIFIER_CAPS=$2000;
      KEYMODIFIER_MODE=$4000;
      KEYMODIFIER_RESERVED=$8000;

      KEYMODIFIER_CTRL=(KEYMODIFIER_LCTRL or KEYMODIFIER_RCTRL);
      KEYMODIFIER_SHIFT=(KEYMODIFIER_LSHIFT or KEYMODIFIER_RSHIFT);
      KEYMODIFIER_ALT=(KEYMODIFIER_LALT or KEYMODIFIER_RALT);
      KEYMODIFIER_META=(KEYMODIFIER_LMETA or KEYMODIFIER_RMETA);

      BUTTON_NONE=0;
      BUTTON_LEFT=1;
      BUTTON_MIDDLE=2;
      BUTTON_RIGHT=3;
      BUTTON_WHEELDOWN=4;
      BUTTON_WHEELUP=5;

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

type EVulkanApplication=class(Exception);

     TVulkanApplication=class;

     TVulkanApplicationRawByteString={$if declared(RawByteString)}RawByteString{$else}AnsiString{$ifend};

     TVulkanApplicationUnicodeString={$if declared(UnicodeString)}UnicodeString{$else}WideString{$ifend};

     TVulkanApplicationOnEvent=function(const fVulkanApplication:TVulkanApplication;const pEvent:TSDL_Event):boolean of object;

     TVulkanApplicationOnStep=procedure(const fVulkanApplication:TVulkanApplication) of object;

     PPVulkanApplicationHighResolutionTime=^PVulkanApplicationHighResolutionTime;
     PVulkanApplicationHighResolutionTime=^TVulkanApplicationHighResolutionTime;
     TVulkanApplicationHighResolutionTime=TVkInt64;

     TVulkanApplicationHighResolutionTimer=class
      private
       fFrequency:TVkInt64;
       fFrequencyShift:TVkInt32;
       fMillisecondInterval:TVulkanApplicationHighResolutionTime;
       fTwoMillisecondsInterval:TVulkanApplicationHighResolutionTime;
       fFourMillisecondsInterval:TVulkanApplicationHighResolutionTime;
       fQuarterSecondInterval:TVulkanApplicationHighResolutionTime;
       fMinuteInterval:TVulkanApplicationHighResolutionTime;
       fHourInterval:TVulkanApplicationHighResolutionTime;
      public
       constructor Create;
       destructor Destroy; override;
       function GetTime:TVkInt64;
       procedure Sleep(const pDelay:TVulkanApplicationHighResolutionTime);
       function ToFloatSeconds(const pTime:TVulkanApplicationHighResolutionTime):double;
       function FromFloatSeconds(const pTime:double):TVulkanApplicationHighResolutionTime;
       function ToMilliseconds(const pTime:TVulkanApplicationHighResolutionTime):TVkInt64;
       function FromMilliseconds(const pTime:TVkInt64):TVulkanApplicationHighResolutionTime;
       function ToMicroseconds(const pTime:TVulkanApplicationHighResolutionTime):TVkInt64;
       function FromMicroseconds(const pTime:TVkInt64):TVulkanApplicationHighResolutionTime;
       function ToNanoseconds(const pTime:TVulkanApplicationHighResolutionTime):TVkInt64;
       function FromNanoseconds(const pTime:TVkInt64):TVulkanApplicationHighResolutionTime;
       property Frequency:TVkInt64 read fFrequency;
       property MillisecondInterval:TVulkanApplicationHighResolutionTime read fMillisecondInterval;
       property TwoMillisecondsInterval:TVulkanApplicationHighResolutionTime read fTwoMillisecondsInterval;
       property FourMillisecondsInterval:TVulkanApplicationHighResolutionTime read fFourMillisecondsInterval;
       property QuarterSecondInterval:TVulkanApplicationHighResolutionTime read fQuarterSecondInterval;
       property SecondInterval:TVulkanApplicationHighResolutionTime read fFrequency;
       property MinuteInterval:TVulkanApplicationHighResolutionTime read fMinuteInterval;
       property HourInterval:TVulkanApplicationHighResolutionTime read fHourInterval;
     end;

     TVulkanApplicationInputProcessor=class
      public
       constructor Create; virtual;
       destructor Destroy; override;
       function KeyDown(const pKeyCode,pKeyModifier:TVkInt32):boolean; virtual;
       function KeyUp(const pKeyCode,pKeyModifier:TVkInt32):boolean; virtual;
       function KeyTyped(const pKeyCode,pKeyModifier:TVkInt32):boolean; virtual;
       function TouchDown(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean; virtual;
       function TouchUp(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean; virtual;
       function TouchDragged(const pScreenX,pScreenY,pPressure:single;const pPointerID:TVkInt32):boolean; virtual;
       function MouseMoved(const pScreenX,pScreenY:TVkInt32):boolean; virtual;
       function Scrolled(const pAmount:TVkInt32):boolean; virtual;
     end;

     PVulkanApplicationInputProcessorQueueEvent=^TVulkanApplicationInputProcessorQueueEvent;
     TVulkanApplicationInputProcessorQueueEvent=record
      Next:PVulkanApplicationInputProcessorQueueEvent;
      Time:TVkInt64;
      case Event:TVkInt32 of
       EVENT_KEY_DOWN,EVENT_KEY_UP,EVENT_KEY_TYPED:(
        KeyCode:TVkInt32;
        KeyModifier:TVkInt32;
       );
       EVENT_TOUCH_DOWN,EVENT_TOUCH_UP,EVENT_TOUCH_DRAGGED:(
        ScreenX:single;
        ScreenY:single;
        Pressure:single;
        PointerID:TVkInt32;
        Button:TVkInt32;
       );
       EVENT_MOUSE_MOVED:(
        MouseScreenX:TVkInt32;
        MouseScreenY:TVkInt32;
       );
       EVENT_SCROLLED:(
        Amount:TVkInt32;
       );
     end;

     TVulkanApplicationInputProcessorQueue=class(TVulkanApplicationInputProcessor)
      private
       fProcessor:TVulkanApplicationInputProcessor;
       fCriticalSection:TPasMPCriticalSection;
       fQueuedEvents:PVulkanApplicationInputProcessorQueueEvent;
       fLastQueuedEvent:PVulkanApplicationInputProcessorQueueEvent;
       fFreeEvents:PVulkanApplicationInputProcessorQueueEvent;
       fCurrentEventTime:TVkInt64;
       function NewEvent:PVulkanApplicationInputProcessorQueueEvent;
       procedure PushEvent(Event:PVulkanApplicationInputProcessorQueueEvent);
      public
       constructor Create; override;
       destructor Destroy; override;
       procedure SetProcessor(pProcessor:TVulkanApplicationInputProcessor);
       function GetProcessor:TVulkanApplicationInputProcessor;
       procedure Drain;
       function GetCurrentEventTime:TVkInt64;
       function KeyDown(const pKeyCode,pKeyModifier:TVkInt32):boolean; override;
       function KeyUp(const pKeyCode,pKeyModifier:TVkInt32):boolean; override;
       function KeyTyped(const pKeyCode,pKeyModifier:TVkInt32):boolean; override;
       function TouchDown(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean; override;
       function TouchUp(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean; override;
       function TouchDragged(const pScreenX,pScreenY,pPressure:single;const pPointerID:TVkInt32):boolean; override;
       function MouseMoved(const pScreenX,pScreenY:TVkInt32):boolean; override;
       function Scrolled(const pAmount:TVkInt32):boolean; override;
     end;

     TVulkanApplicationInputMultiplexer=class(TVulkanApplicationInputProcessor)
      private
       fProcessors:TList;
      public
       constructor Create; override;
       destructor Destroy; override;
       procedure AddProcessor(const pProcessor:TVulkanApplicationInputProcessor);
       procedure AddProcessors(const pProcessors:array of TVulkanApplicationInputProcessor);
       procedure InsertProcessor(const pIndex:TVkInt32;const pProcessor:TVulkanApplicationInputProcessor);
       procedure RemoveProcessor(const pProcessor:TVulkanApplicationInputProcessor); overload;
       procedure RemoveProcessor(const pIndex:TVkInt32); overload;
       procedure ClearProcessors;
       function CountProcessors:TVkInt32;
       function KeyDown(const pKeyCode,pKeyModifier:TVkInt32):boolean; override;
       function KeyUp(const pKeyCode,pKeyModifier:TVkInt32):boolean; override;
       function KeyTyped(const pKeyCode,pKeyModifier:TVkInt32):boolean; override;
       function TouchDown(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean; override;
       function TouchUp(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean; override;
       function TouchDragged(const pScreenX,pScreenY,pPressure:single;const pPointerID:TVkInt32):boolean; override;
       function MouseMoved(const pScreenX,pScreenY:TVkInt32):boolean; override;
       function Scrolled(const pAmount:TVkInt32):boolean; override;
     end;

     TVulkanApplicationInputTextInputCallback=procedure(pSuccessful:boolean;const pText:TVulkanApplicationRawByteString) of object;

     TVulkanApplicationInput=class;

     TVulkanApplicationJoystick=class
      private
       fIndex:TVkInt32;
       fID:TVkInt32;
       fJoystick:PSDL_Joystick;
       fGameController:PSDL_GameController;
       fCountAxes:TVkInt32;
       fCountBalls:TVkInt32;
       fCountHats:TVkInt32;
       fCountButtons:TVkInt32;
       procedure Initialize;
      public
       constructor Create(const pIndex:TVkInt32;const pJoystick:PSDL_Joystick;const pGameController:PSDL_GameController); reintroduce;
       destructor Destroy; override;
       function IsGameController:boolean;
       function Index:TVkInt32;
       function ID:TVkInt32;
       function Name:TVulkanApplicationRawByteString;
       function GUID:TGUID;
       function DeviceGUID:TGUID;
       function CountAxes:TVkInt32;
       function CountBalls:TVkInt32;
       function CountHats:TVkInt32;
       function CountButtons:TVkInt32;
       procedure Update;
       function GetAxis(const pAxisIndex:TVkInt32):TVkInt32;
       function GetBall(const pBallIndex:TVkInt32;out pDeltaX,pDeltaY:TVkInt32):boolean;
       function GetHat(const pHatIndex:TVkInt32):TVkInt32;
       function GetButton(const pButtonIndex:TVkInt32):boolean;
       function IsGameControllerAttached:boolean;
       function GetGameControllerAxis(const pAxis:TVkInt32):TVkInt32;
       function GetGameControllerButton(const pButton:TVkInt32):boolean;
       function GetGameControllerName:TVulkanApplicationRawByteString;
       function GetGameControllerMapping:TVulkanApplicationRawByteString;
     end;

     TVulkanApplicationInput=class
      private
       fVulkanApplication:TVulkanApplication;
       fKeyCodeNames:array[-1..1023] of TVulkanApplicationRawByteString;
       fCriticalSection:TPasMPCriticalSection;
       fProcessor:TVulkanApplicationInputProcessor;
       fEvents:array of TSDL_Event;
       fEventTimes:array of int64;
       fEventCount:TVkInt32;
       fCurrentEventTime:int64;
       fKeyDown:array[0..$ffff] of boolean;
       fKeyDownCount:TVkInt32;
       fPointerX:array[0..$ffff] of single;
       fPointerY:array[0..$ffff] of single;
       fPointerDown:array[0..$ffff] of boolean;
       fPointerJustDown:array[0..$ffff] of boolean;
       fPointerPressure:array[0..$ffff] of single;
       fPointerDeltaX:array[0..$ffff] of single;
       fPointerDeltaY:array[0..$ffff] of single;
       fPointerDownCount:TVkInt32;
       fMouseX:TVkInt32;
       fMouseY:TVkInt32;
       fMouseDown:TVkInt32;
       fMouseJustDown:TVkInt32;
       fMouseDeltaX:TVkInt32;
       fMouseDeltaY:TVkInt32;
       fJustTouched:longbool;
       fMaxPointerID:TVkInt32;
       fJoysticks:TList;
       fMainJoystick:TVulkanApplicationJoystick;
       function TranslateSDLKeyCode(const pKeyCode,pScanCode:TVkInt32):TVkInt32;
       function TranslateSDLKeyModifier(const pKeyModifier:TVkInt32):TVkInt32;
       procedure AddEvent(const pEvent:TSDL_Event);
       procedure ProcessEvents;
      public
       constructor Create(const pVulkanApplication:TVulkanApplication); reintroduce;
       destructor Destroy; override;
       function GetAccelerometerX:single;
       function GetAccelerometerY:single;
       function GetAccelerometerZ:single;
       function GetOrientationAzimuth:single;
       function GetOrientationPitch:single;
       function GetOrientationRoll:single;
       function GetMaxPointerID:TVkInt32;
       function GetPointerX(const pPointerID:TVkInt32=0):single;
       function GetPointerDeltaX(const pPointerID:TVkInt32=0):single;
       function GetPointerY(const pPointerID:TVkInt32=0):single;
       function GetPointerDeltaY(const pPointerID:TVkInt32=0):single;
       function GetPointerPressure(const pPointerID:TVkInt32=0):single;
       function IsPointerTouched(const pPointerID:TVkInt32=0):boolean;
       function IsPointerJustTouched(const pPointerID:TVkInt32=0):boolean;
       function IsTouched:boolean;
       function JustTouched:boolean;
       function IsButtonPressed(const pButton:TVkInt32):boolean;
       function IsKeyPressed(const pKeyCode:TVkInt32):boolean;
       function GetKeyName(const pKeyCode:TVkInt32):TVulkanApplicationRawByteString;
       function GetKeyModifier:TVkInt32;
       procedure GetTextInput(const pCallback:TVulkanApplicationInputTextInputCallback;const pTitle,pText:TVulkanApplicationRawByteString;const pPlaceholder:TVulkanApplicationRawByteString='');
       procedure SetOnscreenKeyboardVisible(const pVisible:boolean);
       procedure Vibrate(const pMilliseconds:TVkInt32); overload;
       procedure Vibrate(const pPattern:array of TVkInt32;const pRepeats:TVkInt32); overload;
       procedure CancelVibrate;
       procedure GetRotationMatrix(const pMatrix3x3:pointer);
       function GetCurrentEventTime:TVkInt64;
       procedure SetCatchBackKey(const pCatchBack:boolean);
       procedure SetCatchMenuKey(const pCatchMenu:boolean);
       procedure SetInputProcessor(const pProcessor:TVulkanApplicationInputProcessor);
       function GetInputProcessor:TVulkanApplicationInputProcessor;
       function IsPeripheralAvailable(const pPeripheral:TVkInt32):boolean;
       function GetNativeOrientation:TVkInt32;
       procedure SetCursorCatched(const pCatched:boolean);
       function IsCursorCatched:boolean;
       procedure SetCursorPosition(const pX,pY:TVkInt32);
       function GetJoystickCount:TVkInt32;
       function GetJoystick(const pIndex:TVkInt32=-1):TVulkanApplicationJoystick;
     end;

     TVulkanScreen=class
      public

       constructor Create; virtual;

       destructor Destroy; override;

       procedure Show; virtual;

       procedure Hide; virtual;

       procedure Resume; virtual;

       procedure Pause; virtual;

       procedure Resize(const pWidth,pHeight:TSDLInt32); virtual;

       procedure AfterCreateSwapChain; virtual;

       procedure BeforeDestroySwapChain; virtual;

       function HandleEvent(const pEvent:TSDL_Event):boolean; virtual;

       function CanBeParallelProcessed:boolean; virtual;

       procedure Update(const pDeltaTime:double); virtual;

       procedure Draw(const pSwapChainImageIndex:TVkInt32;var pWaitSemaphore:TVulkanSemaphore;const pWaitFence:TVulkanFence=nil); virtual;

     end;

     TVulkanScreenClass=class of TVulkanScreen;

     TVulkanApplicationAssets=class
      private
       fVulkanApplication:TVulkanApplication;
{$ifndef Android}
       fBasePath:string;
{$endif}
       function CorrectFileName(const pFileName:string):string;
      public
       constructor Create(const pVulkanApplication:TVulkanApplication);
       destructor Destroy; override;
       function ExistAsset(const pFileName:string):boolean;
       function GetAssetStream(const pFileName:string):TStream;
       function GetAssetSize(const pFileName:string):int64;
     end;

     TVulkanApplicationFiles=class
      private
       fVulkanApplication:TVulkanApplication;
      public
       constructor Create(const pVulkanApplication:TVulkanApplication);
       destructor Destroy; override;
       function GetLocalStoragePath:string;
       function GetRoamingStoragePath:string;
       function GetExternalStoragePath:string;
       function IsLocalStorageAvailable:boolean;
       function IsRoamingStorageAvailable:boolean;
       function IsExternalStorageAvailable:boolean;
     end;

     TVulkanApplicationCommandPools=array of array of TVulkanCommandPool;

     TVulkanApplicationCommandBuffers=array of array of TVulkanCommandBuffer;

     TVulkanApplicationCommandBufferFences=array of array of TVulkanFence;

     PVulkanApplicationVulkanRecreationKind=^TVulkanApplicationVulkanRecreationKind;
     TVulkanApplicationVulkanRecreationKind=
      (
       vavrkNone,
       vavrkSwapChain,
       vavrkSurface,
       vavrkDevice
      );

     TVulkanApplication=class
      private

       fTitle:string;
       fVersion:TVkUInt32;

       fPathName:string;

       fLocalDataPath:string;
       fRoamingDataPath:string;
       fExternalStoragePath:string;

       fPasMPInstance:TPasMP;

       fHighResolutionTimer:TVulkanApplicationHighResolutionTimer;

       fAssets:TVulkanApplicationAssets;

       fFiles:TVulkanApplicationFiles;

       fInput:TVulkanApplicationInput;

       fCurrentWidth:TSDLInt32;
       fCurrentHeight:TSDLInt32;
       fCurrentFullscreen:TSDLInt32;
       fCurrentVSync:TSDLInt32;
       fCurrentVisibleMouseCursor:TSDLInt32;
       fCurrentCatchMouse:TSDLInt32;
       fCurrentHideSystemBars:TSDLInt32;
       fCurrentBlocking:TSDLInt32;

       fWidth:TSDLInt32;
       fHeight:TSDLInt32;
       fFullscreen:boolean;
       fVSync:boolean;
       fResizable:boolean;
       fVisibleMouseCursor:boolean;
       fCatchMouse:boolean;
       fHideSystemBars:boolean;
       fBlocking:boolean;

       fActive:boolean;

       fTerminated:boolean;

       fSDLDisplayMode:TSDL_DisplayMode;
       fSurfaceWindow:PSDL_Window;

       fEvent:TSDL_Event;

       fLastPressedKeyEvent:TSDL_Event;
       fKeyRepeatTimeAccumulator:TVulkanApplicationHighResolutionTime;
       fKeyRepeatInterval:TVulkanApplicationHighResolutionTime;
       fKeyRepeatInitialInterval:TVulkanApplicationHighResolutionTime;

       fScreenWidth:TSDLInt32;
       fScreenHeight:TSDLInt32;

       fVideoFlags:TSDLUInt32;

       fGraphicsReady:boolean;

       fVulkanDebugging:boolean;

       fVulkanValidation:boolean;

       fVulkanDebuggingEnabled:boolean;

       fVulkanInstance:TVulkanInstance;

       fVulkanDevice:TVulkanDevice;

       fCountCPUThreads:TVkInt32;

       fAvailableCPUCores:TPasMPAvailableCPUCores;

       fVulkanCountCommandQueues:TVkInt32;

       fVulkanCommandPools:array of TVulkanApplicationCommandPools;
       fVulkanCommandBuffers:array of TVulkanApplicationCommandBuffers;
       fVulkanCommandBufferFences:array of TVulkanApplicationCommandBufferFences;

       fVulkanPresentCommandPools:TVulkanApplicationCommandPools;
       fVulkanPresentCommandBuffers:TVulkanApplicationCommandBuffers;
       fVulkanPresentCommandBufferFences:TVulkanApplicationCommandBufferFences;

       fVulkanGraphicsCommandPools:TVulkanApplicationCommandPools;
       fVulkanGraphicsCommandBuffers:TVulkanApplicationCommandBuffers;
       fVulkanGraphicsCommandBufferFences:TVulkanApplicationCommandBufferFences;

       fVulkanComputeCommandPools:TVulkanApplicationCommandPools;
       fVulkanComputeCommandBuffers:TVulkanApplicationCommandBuffers;
       fVulkanComputeCommandBufferFences:TVulkanApplicationCommandBufferFences;

       fVulkanTransferCommandPools:TVulkanApplicationCommandPools;
       fVulkanTransferCommandBuffers:TVulkanApplicationCommandBuffers;
       fVulkanTransferCommandBufferFences:TVulkanApplicationCommandBufferFences;

       fVulkanSurface:TVulkanSurface;

       fGraphicsPipelinesReady:boolean;

//      fVulkanPresentationSurface:TVulkanPresentationSurface;

       fOnEvent:TVulkanApplicationOnEvent;

       fOnStep:TVulkanApplicationOnStep;

       fScreen:TVulkanScreen;

       fStartScreen:TVulkanScreenClass;

       fNextScreen:TVulkanScreen;

       fHasNewNextScreen:boolean;

       fHasLastTime:boolean;

       fLastTime:TVulkanApplicationHighResolutionTime;
       fNowTime:TVulkanApplicationHighResolutionTime;
       fDeltaTime:TVulkanApplicationHighResolutionTime;
       fFloatDeltaTime:double;

       fFrameTimesHistoryDeltaTimes:array[0..FrameTimesHistorySize-1] of double;
       fFrameTimesHistoryTimePoints:array[0..FrameTimesHistorySize-1] of TVulkanApplicationHighResolutionTime;
       fFrameTimesHistoryReadIndex:TPasMPInt32;
       fFrameTimesHistoryWriteIndex:TPasMPInt32;

       fFramesPerSecond:double;

       fFrameCounter:TVkInt64;

       fCountSwapChainImages:TVkInt32;

       fCurrentImageIndex:TVkInt32;

       fCurrentSwapChainImageIndex:TVkInt32;

       fVulkanRecreationKind:TVulkanApplicationVulkanRecreationKind;

       fVulkanWaitSemaphore:TVulkanSemaphore;

       fVulkanWaitFence:TVulkanFence;

       fVulkanSwapChain:TVulkanSwapChain;

       fVulkanOldSwapChain:TVulkanSwapChain;

       fVulkanWaitFences:array[0..MaxSwapChainImages-1] of TVulkanFence;

       fVulkanWaitFencesReady:array[0..MaxSwapChainImages-1] of boolean;

       fVulkanPresentCompleteSemaphores:array[0..MaxSwapChainImages-1] of TVulkanSemaphore;

       fVulkanDepthImageFormat:TVkFormat;

       fVulkanDepthFrameBufferAttachment:TVulkanFrameBufferAttachment;

       fVulkanFrameBufferColorAttachments:TVulkanFrameBufferAttachments;

       fVulkanRenderPass:TVulkanRenderPass;

       fVulkanFrameBuffers:TVulkanSwapChainSimpleDirectRenderTargetFrameBuffers;

       fVulkanBlankCommandPool:TVulkanCommandPool;

       fVulkanBlankCommandBuffers:array[0..MaxSwapChainImages-1] of TVulkanCommandBuffer;

       fVulkanBlankCommandBufferSemaphores:array[0..MaxSwapChainImages-1] of TVulkanSemaphore;

       procedure InitializeGraphics;
       procedure DeinitializeGraphics;

      protected

       procedure VulkanDebugLn(const What:TVkCharString);

       function VulkanOnDebugReportCallback(const flags:TVkDebugReportFlagsEXT;const objectType:TVkDebugReportObjectTypeEXT;const object_:TVkUInt64;const location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:TVulkaNCharString;const pMessage:TVulkanCharString):TVkBool32;

       procedure VulkanWaitIdle;

       procedure CreateVulkanDevice(const pSurface:TVulkanSurface=nil);

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

       procedure CreateVulkanBlankCommandBuffers;
       procedure DestroyVulkanBlankCommandBuffers;

       function AcquireVulkanBackBuffer:boolean;
       function PresentVulkanBackBuffer:boolean;

       procedure SetScreen(const pScreen:TVulkanScreen);
       procedure SetNextScreen(const pNextScreen:TVulkanScreen);

       procedure UpdateFrameTimesHistory;

       procedure UpdateJobFunction(const pJob:PPasMPJob;const pThreadIndex:TPasMPInt32);
       procedure DrawJobFunction(const pJob:PPasMPJob;const pThreadIndex:TPasMPInt32);

      public

       constructor Create; reintroduce; virtual;
       destructor Destroy; override;

       procedure ReadConfig; virtual;
       procedure SaveConfig; virtual;

       procedure Initialize;

       procedure Terminate;

       function HandleEvent(const pEvent:TSDL_Event):boolean; virtual;

       procedure ProcessMessages;

       procedure Run;

       procedure Start; virtual;

       procedure Stop; virtual;

       procedure Load; virtual;

       procedure Unload; virtual;

       procedure AfterCreateSwapChain; virtual;

       procedure BeforeDestroySwapChain; virtual;

       procedure Update(const pDeltaTime:double); virtual;

       procedure Draw(const pSwapChainImageIndex:TVkInt32;var pWaitSemaphore:TVulkanSemaphore;const pWaitFence:TVulkanFence=nil); virtual;

       procedure Resume; virtual;

       procedure Pause; virtual;

       class procedure Main; virtual;

       property VulkanFrameBuffers:TVulkanSwapChainSimpleDirectRenderTargetFrameBuffers read fVulkanFrameBuffers;
       
      published

       property PasMPInstance:TPasMP read fPasMPInstance;

       property HighResolutionTimer:TVulkanApplicationHighResolutionTimer read fHighResolutionTimer;

       property Assets:TVulkanApplicationAssets read fAssets;

       property Files:TVulkanApplicationFiles read fFiles;

       property Input:TVulkanApplicationInput read fInput;

       property Title:string read fTitle write fTitle;
       property Version:TVkUInt32 read fVersion write fVersion;

       property PathName:string read fPathName write fPathName;

       property Width:TSDLInt32 read fWidth write fWidth;
       property Height:TSDLInt32 read fHeight write fHeight;

       property Fullscreen:boolean read fFullscreen write fFullscreen;

       property VSync:boolean read fVSync write fVSync;

       property Resizable:boolean read fResizable write fResizable;

       property VisibleMouseCursor:boolean read fVisibleMouseCursor write fVisibleMouseCursor;

       property CatchMouse:boolean read fCatchMouse write fCatchMouse;

       property HideSystemBars:boolean read fHideSystemBars write fHideSystemBars;

       property Blocking:boolean read fBlocking write fBlocking;

       property Active:boolean read fActive;

       property Terminated:boolean read fTerminated;

       property CountCPUThreads:TVkInt32 read fCountCPUThreads;

       property OnEvent:TVulkanApplicationOnEvent read fOnEvent write fOnEvent;
       property OnStep:TVulkanApplicationOnStep read fOnStep write fOnStep;

       property VulkanDebugging:boolean read fVulkanDebugging write fVulkanDebugging;

       property VulkanValidation:boolean read fVulkanValidation write fVulkanValidation;

       property VulkanDebuggingEnabled:boolean read fVulkanDebuggingEnabled;

       property VulkanInstance:TVulkanInstance read fVulkanInstance;

       property VulkanDevice:TVulkanDevice read fVulkanDevice;

       property VulkanPresentCommandPools:TVulkanApplicationCommandPools read fVulkanPresentCommandPools;
       property VulkanPresentCommandBuffers:TVulkanApplicationCommandBuffers read fVulkanPresentCommandBuffers;
       property VulkanPresentCommandBufferFences:TVulkanApplicationCommandBufferFences read fVulkanPresentCommandBufferFences;

       property VulkanGraphicsCommandPools:TVulkanApplicationCommandPools read fVulkanGraphicsCommandPools;
       property VulkanGraphicsCommandBuffers:TVulkanApplicationCommandBuffers read fVulkanGraphicsCommandBuffers;
       property VulkanGraphicsCommandBufferFences:TVulkanApplicationCommandBufferFences read fVulkanGraphicsCommandBufferFences;

       property VulkanComputeCommandPools:TVulkanApplicationCommandPools read fVulkanComputeCommandPools;
       property VulkanComputeCommandBuffers:TVulkanApplicationCommandBuffers read fVulkanComputeCommandBuffers;
       property VulkanComputeCommandBufferFences:TVulkanApplicationCommandBufferFences read fVulkanComputeCommandBufferFences;

       property VulkanTransferCommandPools:TVulkanApplicationCommandPools read fVulkanTransferCommandPools;
       property VulkanTransferCommandBuffers:TVulkanApplicationCommandBuffers read fVulkanTransferCommandBuffers;
       property VulkanTransferCommandBufferFences:TVulkanApplicationCommandBufferFences read fVulkanTransferCommandBufferFences;

       property VulkanSwapChain:TVulkanSwapChain read fVulkanSwapChain;

       property VulkanDepthImageFormat:TVkFormat read fVulkanDepthImageFormat;

       property VulkanRenderPass:TVulkanRenderPass read fVulkanRenderPass;

       property StartScreen:TVulkanScreenClass read fStartScreen write fStartScreen;

       property Screen:TVulkanScreen read fScreen write SetScreen;

       property NextScreen:TVulkanScreen read fNextScreen write SetNextScreen;

       property FramesPerSecond:double read fFramesPerSecond;

       property FrameCounter:TVkInt64 read fFrameCounter;

       property CountSwapChainImages:TVkInt32 read fCountSwapChainImages;

       property CurrentSwapChainImageIndex:TVkInt32 read fCurrentSwapChainImageIndex;

     end;

var VulkanApplication:TVulkanApplication=nil;

{$if defined(fpc) and defined(android)}
     AndroidJavaEnv:PJNIEnv=nil;
     AndroidJavaClass:jclass=nil;
     AndroidJavaObject:jobject=nil;

     AndroidActivity:PANativeActivity=nil;

     AndroidAssetManager:PAAssetManager=nil;

     AndroidInternalDataPath:string='';
     AndroidExternalDataPath:string='';
     AndroidLibraryPath:string='';

     AndroidDeviceName:string='';

function AndroidGetDeviceName:TVulkanApplicationUnicodeString;
function Android_JNI_GetEnv:PJNIEnv; cdecl;
procedure ANativeActivity_onCreate(pActivity:PANativeActivity;pSavedState:pointer;pSavedStateSize:cuint32); cdecl;
{$ifend}

implementation

const BoolToInt:array[boolean] of TVkInt32=(0,1);

      BoolToLongBool:array[boolean] of longbool=(false,true);

{$if defined(Unix)}
procedure signal_handler(pSignal:cint); cdecl;
begin
 case pSignal of
  SIGINT,SIGTERM,SIGKILL:begin
   if assigned(VulkanApplication) then begin
    VulkanApplication.Terminate;
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
function GetAppDataLocalPath(Postfix:TVulkanApplicationRawByteString):TVulkanApplicationRawByteString;
{$ifdef darwin}
var TruePath:TVulkanApplicationRawByteString;
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
 result:=IncludeTrailingPathDelimiter(result);
{$else}
 result:=GetEnvironmentVariable('HOME');
 if not DirectoryExists(result) then begin
  CreateDir(result);
 end;
 if length(Postfix)>0 then begin
  result:=IncludeTrailingPathDelimiter(result)+'.'+Postfix;
  if not DirectoryExists(result) then begin
   CreateDir(result);
  end;
 end;
 result:=IncludeTrailingPathDelimiter(result);
{$endif}
 result:=IncludeTrailingPathDelimiter(result)+'local';
 if not DirectoryExists(result) then begin
  CreateDir(result);
 end;
 result:=IncludeTrailingPathDelimiter(result);
end;

function GetAppDataRoamingPath(Postfix:TVulkanApplicationRawByteString):TVulkanApplicationRawByteString;
{$ifdef darwin}
var TruePath:TVulkanApplicationRawByteString;
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
 result:=IncludeTrailingPathDelimiter(result);
{$else}
 result:=GetEnvironmentVariable('HOME');
 if not DirectoryExists(result) then begin
  CreateDir(result);
 end;
 if length(Postfix)>0 then begin
  result:=IncludeTrailingPathDelimiter(result)+'.'+Postfix;
  if not DirectoryExists(result) then begin
   CreateDir(result);
  end;
 end;
 result:=IncludeTrailingPathDelimiter(result);
{$endif}
 result:=IncludeTrailingPathDelimiter(result)+'roaming';
 if not DirectoryExists(result) then begin
  CreateDir(result);
 end;
 result:=IncludeTrailingPathDelimiter(result);
end;
{$else}
function ExpandEnvironmentStrings(const s:TVulkanApplicationRawByteString):TVulkanApplicationRawByteString;
var i:TVkInt32;
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

function GetEnvironmentVariable(const s:TVulkanApplicationRawByteString):TVulkanApplicationRawByteString;
var i:TVkInt32;
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

function GetAppDataLocalPath(Postfix:TVulkanApplicationRawByteString):TVulkanApplicationRawByteString;
type TSHGetFolderPath=function(hwndOwner:hwnd;nFolder:TVkInt32;nToken:Windows.THandle;dwFlags:TVkInt32;lpszPath:PAnsiChar):hresult; stdcall;
const CSIDL_LOCALAPPDATA=$001c;
var SHGetFolderPath:TSHGetFolderPath;
    FilePath:PAnsiChar;
    LibHandle:Windows.THandle;
    Reg:TRegistry;
begin
 result:='';
 try
  // First try over the SHFOLDER.DLL from MSIE >= 5.0 on Win9x or from Windows >= 2000
  LibHandle:=LoadLibrary('SHFOLDER.DLL');
  if LibHandle<>0 then begin
   try
    SHGetFolderPath:=GetProcAddress(LibHandle,'SHGetFolderPathA');
    GetMem(FilePath,4096);
    FillChar(FilePath^,4096,ansichar(#0));
    try
     if SHGetFolderPath(0,CSIDL_LOCALAPPDATA,0,0,FilePath)=0 then begin
      result:=FilePath;
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
  result:=GetEnvironmentVariable('localappdata');
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
    result:=GetEnvironmentVariable('windir');
    if length(result)>0 then begin
     // For german Win9x installations
     result:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(result)+'Lokale Einstellungen')+'Anwendungsdaten';
     if not DirectoryExists(result) then begin
      // For all other language Win9x installations
      result:=IncludeTrailingPathDelimiter(result)+'Local Settings';
      if not DirectoryExists(result) then begin
       result:=IncludeTrailingPathDelimiter(result)+'Engine Data';
       if not DirectoryExists(result) then begin
        CreateDir(result);
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
  result:=IncludeTrailingPathDelimiter(result)+Postfix;
  if not DirectoryExists(result) then begin
   CreateDir(result);
  end;
 end;
 result:=IncludeTrailingPathDelimiter(result);
end;

function GetAppDataRoamingPath(Postfix:TVulkanApplicationRawByteString):TVulkanApplicationRawByteString;
type TSHGetFolderPath=function(hwndOwner:hwnd;nFolder:TVkInt32;nToken:Windows.THandle;dwFlags:TVkInt32;lpszPath:PAnsiChar):hresult; stdcall;
const CSIDL_APPDATA=$001a;
var SHGetFolderPath:TSHGetFolderPath;
    FilePath:PAnsiChar;
    LibHandle:Windows.THandle;
    Reg:TRegistry;
begin
 result:='';
 try
  // First try over the SHFOLDER.DLL from MSIE >= 5.0 on Win9x or from Windows >= 2000
  LibHandle:=LoadLibrary('SHFOLDER.DLL');
  if LibHandle<>0 then begin
   try
    SHGetFolderPath:=GetProcAddress(LibHandle,'SHGetFolderPathA');
    GetMem(FilePath,4096);
    FillChar(FilePath^,4096,ansichar(#0));
    try
     if SHGetFolderPath(0,CSIDL_APPDATA,0,0,FilePath)=0 then begin
      result:=FilePath;
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
  result:=GetEnvironmentVariable('appdata');
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
    result:=GetEnvironmentVariable('windir');
    if length(result)>0 then begin
     // For german Win9x installations
     result:=IncludeTrailingPathDelimiter(result)+'Anwendungsdaten';
     if not DirectoryExists(result) then begin
      // For all other language Win9x installations
      result:=IncludeTrailingPathDelimiter(result)+'Engine Data';
      if not DirectoryExists(result) then begin
       CreateDir(result);
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
  result:=IncludeTrailingPathDelimiter(result)+Postfix;
  if not DirectoryExists(result) then begin
   CreateDir(result);
  end;
 end;
 result:=IncludeTrailingPathDelimiter(result);
end;
{$endif}

constructor TVulkanApplicationHighResolutionTimer.Create;
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

destructor TVulkanApplicationHighResolutionTimer.Destroy;
begin
 inherited Destroy;
end;

function TVulkanApplicationHighResolutionTimer.GetTime:TVkInt64;
{$ifdef linux}
var NowTimeSpec:TimeSpec;
    ia,ib:TVkInt64;
{$else}
{$ifdef unix}
var tv:timeval;
    tz:timezone;
    ia,ib:TVkInt64;
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
 ia:=TVkInt64(NowTimeSpec.tv_sec)*TVkInt64(1000000000);
 ib:=NowTimeSpec.tv_nsec;
 result:=ia+ib;
{$else}
{$ifdef unix}
  tz.tz_minuteswest:=0;
  tz.tz_dsttime:=0;
  fpgettimeofday(@tv,@tz);
  ia:=TVkInt64(tv.tv_sec)*TVkInt64(1000000);
  ib:=tv.tv_usec;
  result:=ia+ib;
{$else}
 result:=SDL_GetTicks;
{$endif}
{$endif}
{$endif}
 result:=result shr fFrequencyShift;
end;

procedure TVulkanApplicationHighResolutionTimer.Sleep(const pDelay:TVkInt64);
var EndTime,NowTime{$ifdef unix},SleepTime{$endif}:TVkInt64;
{$ifdef unix}
    req,rem:timespec;
{$endif}
begin
 if pDelay>0 then begin
{$ifdef windows}
  NowTime:=GetTime;
  EndTime:=NowTime+pDelay;
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
  EndTime:=NowTime+pDelay;
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
  EndTime:=NowTime+pDelay;
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
  EndTime:=NowTime+pDelay;
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

function TVulkanApplicationHighResolutionTimer.ToFloatSeconds(const pTime:TVulkanApplicationHighResolutionTime):double;
begin
 if fFrequency<>0 then begin
  result:=pTime/fFrequency;
 end else begin
  result:=0;
 end;
end;

function TVulkanApplicationHighResolutionTimer.FromFloatSeconds(const pTime:double):TVulkanApplicationHighResolutionTime;
begin
 if fFrequency<>0 then begin
  result:=trunc(pTime*fFrequency);
 end else begin
  result:=0;
 end;
end;

function TVulkanApplicationHighResolutionTimer.ToMilliseconds(const pTime:TVulkanApplicationHighResolutionTime):TVkInt64;
begin
 result:=pTime;
 if fFrequency<>1000 then begin
  result:=((pTime*1000)+((fFrequency+1) shr 1)) div fFrequency;
 end;
end;

function TVulkanApplicationHighResolutionTimer.FromMilliseconds(const pTime:TVkInt64):TVulkanApplicationHighResolutionTime;
begin
 result:=pTime;
 if fFrequency<>1000 then begin
  result:=((pTime*fFrequency)+500) div 1000;
 end;
end;

function TVulkanApplicationHighResolutionTimer.ToMicroseconds(const pTime:TVulkanApplicationHighResolutionTime):TVkInt64;
begin
 result:=pTime;
 if fFrequency<>1000000 then begin
  result:=((pTime*1000000)+((fFrequency+1) shr 1)) div fFrequency;
 end;
end;

function TVulkanApplicationHighResolutionTimer.FromMicroseconds(const pTime:TVkInt64):TVulkanApplicationHighResolutionTime;
begin
 result:=pTime;
 if fFrequency<>1000000 then begin
  result:=((pTime*fFrequency)+500000) div 1000000;
 end;
end;

function TVulkanApplicationHighResolutionTimer.ToNanoseconds(const pTime:TVulkanApplicationHighResolutionTime):TVkInt64;
begin
 result:=pTime;
 if fFrequency<>1000000000 then begin
  result:=((pTime*1000000000)+((fFrequency+1) shr 1)) div fFrequency;
 end;
end;

function TVulkanApplicationHighResolutionTimer.FromNanoseconds(const pTime:TVkInt64):TVulkanApplicationHighResolutionTime;
begin
 result:=pTime;
 if fFrequency<>1000000000 then begin
  result:=((pTime*fFrequency)+500000000) div 1000000000;
 end;
end;

constructor TVulkanApplicationInputProcessor.Create;
begin
end;

destructor TVulkanApplicationInputProcessor.Destroy;
begin
end;

function TVulkanApplicationInputProcessor.KeyDown(const pKeyCode,pKeyModifier:TVkInt32):boolean;
begin
 result:=false;
end;

function TVulkanApplicationInputProcessor.KeyUp(const pKeyCode,pKeyModifier:TVkInt32):boolean;
begin
 result:=false;
end;

function TVulkanApplicationInputProcessor.KeyTyped(const pKeyCode,pKeyModifier:TVkInt32):boolean;
begin
 result:=false;
end;

function TVulkanApplicationInputProcessor.TouchDown(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean;
begin
 result:=false;
end;

function TVulkanApplicationInputProcessor.TouchUp(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean;
begin
 result:=false;
end;

function TVulkanApplicationInputProcessor.TouchDragged(const pScreenX,pScreenY,pPressure:single;const pPointerID:TVkInt32):boolean;
begin
 result:=false;
end;

function TVulkanApplicationInputProcessor.MouseMoved(const pScreenX,pScreenY:TVkInt32):boolean;
begin
 result:=false;
end;

function TVulkanApplicationInputProcessor.Scrolled(const pAmount:TVkInt32):boolean;
begin
 result:=false;
end;

constructor TVulkanApplicationInputProcessorQueue.Create;
begin
 inherited Create;
 fProcessor:=nil;
 fCriticalSection:=TPasMPCriticalSection.Create;
 fQueuedEvents:=nil;
 fLastQueuedEvent:=nil;
 fFreeEvents:=nil;
 fCurrentEventTime:=0;
end;

destructor TVulkanApplicationInputProcessorQueue.Destroy;
var CurrentEvent,NextEvent:PVulkanApplicationInputProcessorQueueEvent;
begin
 CurrentEvent:=fQueuedEvents;
 while assigned(CurrentEvent) do begin
  NextEvent:=CurrentEvent^.Next;
  FreeMem(CurrentEvent);
  CurrentEvent:=NextEvent;
 end;
 CurrentEvent:=fFreeEvents;
 while assigned(CurrentEvent) do begin
  NextEvent:=CurrentEvent^.Next;
  FreeMem(CurrentEvent);
  CurrentEvent:=NextEvent;
 end;
 FreeAndNil(fCriticalSection);
 inherited Destroy;
end;

function TVulkanApplicationInputProcessorQueue.NewEvent:PVulkanApplicationInputProcessorQueueEvent;
begin
 if assigned(fFreeEvents) then begin
  result:=fFreeEvents;
  fFreeEvents:=result^.Next;
  result^.Next:=nil;
  result^.Event:=EVENT_NONE;
 end else begin
  GetMem(result,SizeOf(TVulkanApplicationInputProcessorQueueEvent));
  FillChar(result^,SizeOf(TVulkanApplicationInputProcessorQueueEvent),AnsiChar(#0));
 end;
 result^.Time:=VulkanApplication.fHighResolutionTimer.GetTime;
end;

procedure TVulkanApplicationInputProcessorQueue.PushEvent(Event:PVulkanApplicationInputProcessorQueueEvent);
begin
 if assigned(fLastQueuedEvent) then begin
  fLastQueuedEvent^.Next:=Event;
 end else begin
  fQueuedEvents:=Event;
 end;
 fLastQueuedEvent:=Event;
 Event^.Next:=nil;
end;

procedure TVulkanApplicationInputProcessorQueue.SetProcessor(pProcessor:TVulkanApplicationInputProcessor);
begin
 fProcessor:=pProcessor;
end;

function TVulkanApplicationInputProcessorQueue.GetProcessor:TVulkanApplicationInputProcessor;
begin
 result:=fProcessor;
end;

procedure TVulkanApplicationInputProcessorQueue.Drain;
var Events,LastQueuedEvent,CurrentEvent,NextEvent:PVulkanApplicationInputProcessorQueueEvent;
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
    EVENT_KEY_DOWN:begin
     fProcessor.KeyDown(CurrentEvent^.KeyCode,CurrentEvent^.KeyModifier);
    end;
    EVENT_KEY_UP:begin
     fProcessor.KeyUp(CurrentEvent^.KeyCode,CurrentEvent^.KeyModifier);
    end;
    EVENT_KEY_TYPED:begin
     fProcessor.KeyTyped(CurrentEvent^.KeyCode,CurrentEvent^.KeyModifier);
    end;
    EVENT_TOUCH_DOWN:begin
     fProcessor.TouchDown(CurrentEvent^.ScreenX,CurrentEvent^.ScreenY,CurrentEvent^.Pressure,CurrentEvent^.PointerID,CurrentEvent^.Button);
    end;
    EVENT_TOUCH_UP:begin
     fProcessor.TouchUp(CurrentEvent^.ScreenX,CurrentEvent^.ScreenY,CurrentEvent^.Pressure,CurrentEvent^.PointerID,CurrentEvent^.Button);
    end;
    EVENT_TOUCH_DRAGGED:begin
     fProcessor.TouchDragged(CurrentEvent^.ScreenX,CurrentEvent^.ScreenY,CurrentEvent^.Pressure,CurrentEvent^.PointerID);
    end;
    EVENT_MOUSE_MOVED:begin
     fProcessor.MouseMoved(CurrentEvent^.MouseScreenX,CurrentEvent^.MouseScreenY);
    end;
    EVENT_SCROLLED:begin
     fProcessor.Scrolled(CurrentEvent^.Amount);
    end;
   end;
  end;
  CurrentEvent:=NextEvent;
 end;
 fCriticalSection.Acquire;
 try
  if assigned(fLastQueuedEvent) then begin
   fLastQueuedEvent^.Next:=Events;
  end else begin
   fQueuedEvents:=Events;
  end;
  fLastQueuedEvent:=LastQueuedEvent;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInputProcessorQueue.GetCurrentEventTime:TVkInt64;
begin
 result:=fCurrentEventTime;
end;

function TVulkanApplicationInputProcessorQueue.KeyDown(const pKeyCode,pKeyModifier:TVkInt32):boolean;
var Event:PVulkanApplicationInputProcessorQueueEvent;
begin
 result:=false;
 fCriticalSection.Acquire;
 try
  Event:=NewEvent;
  if assigned(Event) then begin
   Event^.Event:=EVENT_KEY_DOWN;
   Event^.KeyCode:=pKeyCode;
   Event^.KeyModifier:=pKeyModifier;
   PushEvent(Event);
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInputProcessorQueue.KeyUp(const pKeyCode,pKeyModifier:TVkInt32):boolean;
var Event:PVulkanApplicationInputProcessorQueueEvent;
begin
 result:=false;
 fCriticalSection.Acquire;
 try
  Event:=NewEvent;
  if assigned(Event) then begin
   Event^.Event:=EVENT_KEY_UP;
   Event^.KeyCode:=pKeyCode;
   Event^.KeyModifier:=pKeyModifier;
   PushEvent(Event);
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInputProcessorQueue.KeyTyped(const pKeyCode,pKeyModifier:TVkInt32):boolean;
var Event:PVulkanApplicationInputProcessorQueueEvent;
begin
 result:=false;
 fCriticalSection.Acquire;
 try
  Event:=NewEvent;
  if assigned(Event) then begin
   Event^.Event:=EVENT_KEY_TYPED;
   Event^.KeyCode:=pKeyCode;
   Event^.KeyModifier:=pKeyModifier;
   PushEvent(Event);
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInputProcessorQueue.TouchDown(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean;
var Event:PVulkanApplicationInputProcessorQueueEvent;
begin
 result:=false;
 fCriticalSection.Acquire;
 try
  Event:=NewEvent;
  if assigned(Event) then begin
   Event^.Event:=EVENT_TOUCH_DOWN;
   Event^.ScreenX:=pScreenX;
   Event^.ScreenY:=pScreenY;
   Event^.Pressure:=pPressure;
   Event^.PointerID:=pPointerID;
   Event^.Button:=pButton;
   PushEvent(Event);
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInputProcessorQueue.TouchUp(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean;
var Event:PVulkanApplicationInputProcessorQueueEvent;
begin
 result:=false;
 fCriticalSection.Acquire;
 try
  Event:=NewEvent;
  if assigned(Event) then begin
   Event^.Event:=EVENT_TOUCH_UP;
   Event^.ScreenX:=pScreenX;
   Event^.ScreenY:=pScreenY;
   Event^.Pressure:=pPressure;
   Event^.PointerID:=pPointerID;
   Event^.Button:=pButton;
   PushEvent(Event);
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInputProcessorQueue.TouchDragged(const pScreenX,pScreenY,pPressure:single;const pPointerID:TVkInt32):boolean;
var Event:PVulkanApplicationInputProcessorQueueEvent;
begin
 result:=false;
 fCriticalSection.Acquire;
 try
  Event:=NewEvent;
  if assigned(Event) then begin
   Event^.Event:=EVENT_TOUCH_DRAGGED;
   Event^.ScreenX:=pScreenX;
   Event^.ScreenY:=pScreenY;
   Event^.PointerID:=pPointerID;
   PushEvent(Event);
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInputProcessorQueue.MouseMoved(const pScreenX,pScreenY:TVkInt32):boolean;
var Event:PVulkanApplicationInputProcessorQueueEvent;
begin
 result:=false;
 fCriticalSection.Acquire;
 try
  Event:=NewEvent;
  if assigned(Event) then begin
   Event^.Event:=EVENT_MOUSE_MOVED;
   Event^.MouseScreenX:=pScreenX;
   Event^.MouseScreenY:=pScreenY;
   PushEvent(Event);
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInputProcessorQueue.Scrolled(const pAmount:TVkInt32):boolean;
var Event:PVulkanApplicationInputProcessorQueueEvent;
begin
 result:=false;
 fCriticalSection.Acquire;
 try
  Event:=NewEvent;
  if assigned(Event) then begin
   Event^.Event:=EVENT_SCROLled;
   Event^.Amount:=pAmount;
   PushEvent(Event);
  end;
 finally
  fCriticalSection.Release;
 end;
end;

constructor TVulkanApplicationInputMultiplexer.Create;
begin
 inherited Create;
 fProcessors:=TList.Create;
end;

destructor TVulkanApplicationInputMultiplexer.Destroy;
begin
 FreeAndNil(fProcessors);
 inherited Destroy;
end;

procedure TVulkanApplicationInputMultiplexer.AddProcessor(const pProcessor:TVulkanApplicationInputProcessor);
begin
 fProcessors.Add(pProcessor);
end;

procedure TVulkanApplicationInputMultiplexer.AddProcessors(const pProcessors:array of TVulkanApplicationInputProcessor);
var i:TVkInt32;
begin
 for i:=0 to length(pProcessors)-1 do begin
  fProcessors.Add(pProcessors[i]);
 end;
end;

procedure TVulkanApplicationInputMultiplexer.InsertProcessor(const pIndex:TVkInt32;const pProcessor:TVulkanApplicationInputProcessor);
begin
 fProcessors.Insert(pIndex,pProcessor);
end;

procedure TVulkanApplicationInputMultiplexer.RemoveProcessor(const pProcessor:TVulkanApplicationInputProcessor);
begin
 fProcessors.Remove(pProcessor);
end;

procedure TVulkanApplicationInputMultiplexer.RemoveProcessor(const pIndex:TVkInt32);
begin
 fProcessors.Delete(pIndex);
end;

procedure TVulkanApplicationInputMultiplexer.ClearProcessors;
begin
 fProcessors.Clear;
end;

function TVulkanApplicationInputMultiplexer.CountProcessors:TVkInt32;
begin
 result:=fProcessors.Count;
end;

function TVulkanApplicationInputMultiplexer.KeyDown(const pKeyCode,pKeyModifier:TVkInt32):boolean;
var i:TVkInt32;
    p:TVulkanApplicationInputProcessor;
begin
 result:=false;
 for i:=0 to fProcessors.Count-1 do begin
  p:=fProcessors.Items[i];
  if assigned(p) then begin
   if p.KeyDown(pKeyCode,pKeyModifier) then begin
    result:=true;
    exit;
   end;
  end;
 end;
end;

function TVulkanApplicationInputMultiplexer.KeyUp(const pKeyCode,pKeyModifier:TVkInt32):boolean;
var i:TVkInt32;
    p:TVulkanApplicationInputProcessor;
begin
 result:=false;
 for i:=0 to fProcessors.Count-1 do begin
  p:=fProcessors.Items[i];
  if assigned(p) then begin
   if p.KeyUp(pKeyCode,pKeyModifier) then begin
    result:=true;
    exit;
   end;
  end;
 end;
end;

function TVulkanApplicationInputMultiplexer.KeyTyped(const pKeyCode,pKeyModifier:TVkInt32):boolean;
var i:TVkInt32;
    p:TVulkanApplicationInputProcessor;
begin
 result:=false;
 for i:=0 to fProcessors.Count-1 do begin
  p:=fProcessors.Items[i];
  if assigned(p) then begin
   if p.KeyTyped(pKeyCode,pKeyModifier) then begin
    result:=true;
    exit;
   end;
  end;
 end;
end;

function TVulkanApplicationInputMultiplexer.TouchDown(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean;
var i:TVkInt32;
    p:TVulkanApplicationInputProcessor;
begin
 result:=false;
 for i:=0 to fProcessors.Count-1 do begin
  p:=fProcessors.Items[i];
  if assigned(p) then begin
   if p.TouchDown(pScreenX,pScreenY,pPressure,pPointerID,pButton) then begin
    result:=true;
    exit;
   end;
  end;
 end;
end;

function TVulkanApplicationInputMultiplexer.TouchUp(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean;
var i:TVkInt32;
    p:TVulkanApplicationInputProcessor;
begin
 result:=false;
 for i:=0 to fProcessors.Count-1 do begin
  p:=fProcessors.Items[i];
  if assigned(p) then begin
   if p.TouchUp(pScreenX,pScreenY,pPressure,pPointerID,pButton) then begin
    result:=true;
    exit;
   end;
  end;
 end;
end;

function TVulkanApplicationInputMultiplexer.TouchDragged(const pScreenX,pScreenY,pPressure:single;const pPointerID:TVkInt32):boolean;
var i:TVkInt32;
    p:TVulkanApplicationInputProcessor;
begin
 result:=false;
 for i:=0 to fProcessors.Count-1 do begin
  p:=fProcessors.Items[i];
  if assigned(p) then begin
   if p.TouchDragged(pScreenX,pScreenY,pPressure,pPointerID) then begin
    result:=true;
    exit;
   end;
  end;
 end;
end;

function TVulkanApplicationInputMultiplexer.MouseMoved(const pScreenX,pScreenY:TVkInt32):boolean;
var i:TVkInt32;
    p:TVulkanApplicationInputProcessor;
begin
 result:=false;
 for i:=0 to fProcessors.Count-1 do begin
  p:=fProcessors.Items[i];
  if assigned(p) then begin
   if p.MouseMoved(pScreenX,pScreenY) then begin
    result:=true;
    exit;
   end;
  end;
 end;
end;

function TVulkanApplicationInputMultiplexer.Scrolled(const pAmount:TVkInt32):boolean;
var i:TVkInt32;
    p:TVulkanApplicationInputProcessor;
begin
 result:=false;
 for i:=0 to fProcessors.Count-1 do begin
  p:=fProcessors.Items[i];
  if assigned(p) then begin
   if p.Scrolled(pAmount) then begin
    result:=true;
    exit;
   end;
  end;
 end;
end;

constructor TVulkanApplicationJoystick.Create(const pIndex:TVkInt32;const pJoystick:PSDL_Joystick;const pGameController:PSDL_GameController);
begin
 inherited Create;
 fIndex:=pIndex;
 fJoystick:=pJoystick;
 fGameController:=pGameController;
 if assigned(fJoystick) then begin
  fID:=SDL_JoystickInstanceID(fJoystick);
 end else begin
  fID:=-1;
 end;
end;

destructor TVulkanApplicationJoystick.Destroy;
begin
 if assigned(fGameController) then begin
  SDL_GameControllerClose(fGameController);
 end else if assigned(fJoystick) then begin
  SDL_JoystickClose(fJoystick);
 end;
 inherited Destroy;
end;

procedure TVulkanApplicationJoystick.Initialize;
begin
 fCountAxes:=SDL_JoystickNumAxes(fJoystick);
 fCountBalls:=SDL_JoystickNumBalls(fJoystick);
 fCountHats:=SDL_JoystickNumHats(fJoystick);
 fCountButtons:=SDL_JoystickNumButtons(fJoystick);
end;

function TVulkanApplicationJoystick.IsGameController:boolean;
begin
 result:=assigned(fGameController);
end;

function TVulkanApplicationJoystick.Index:TVkInt32;
begin
 result:=fIndex;
end;

function TVulkanApplicationJoystick.ID:TVkInt32;
begin
 result:=SDL_JoystickInstanceID(fJoystick);
end;

function TVulkanApplicationJoystick.Name:TVulkanApplicationRawByteString;
begin
 result:=SDL_JoystickName(fJoystick);
end;

function TVulkanApplicationJoystick.GUID:TGUID;
begin
 result:=SDL_JoystickGetGUID(fJoystick);
end;

function TVulkanApplicationJoystick.DeviceGUID:TGUID;
begin
 result:=SDL_JoystickGetDeviceGUID(fJoystick);
end;

function TVulkanApplicationJoystick.CountAxes:TVkInt32;
begin
 result:=fCountAxes;
end;

function TVulkanApplicationJoystick.CountBalls:TVkInt32;
begin
 result:=fCountBalls;
end;

function TVulkanApplicationJoystick.CountHats:TVkInt32;
begin
 result:=fCountHats;
end;

function TVulkanApplicationJoystick.CountButtons:TVkInt32;
begin
 result:=fCountButtons;
end;

procedure TVulkanApplicationJoystick.Update;
begin
 SDL_JoystickUpdate;
end;

function TVulkanApplicationJoystick.GetAxis(const pAxisIndex:TVkInt32):TVkInt32;
begin
 result:=SDL_JoystickGetAxis(fJoystick,pAxisIndex);
end;

function TVulkanApplicationJoystick.GetBall(const pBallIndex:TVkInt32;out pDeltaX,pDeltaY:TVkInt32):boolean;
begin
 result:=SDL_JoystickGetBall(fJoystick,pBallIndex,@pDeltaX,@pDeltaY)<>0;
end;

function TVulkanApplicationJoystick.GetHat(const pHatIndex:TVkInt32):TVkInt32;
begin
 case SDL_JoystickGetHat(fJoystick,pHatIndex) of
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
end;

function TVulkanApplicationJoystick.GetButton(const pButtonIndex:TVkInt32):boolean;
begin
 result:=SDL_JoystickGetButton(fJoystick,pButtonIndex)<>0;
end;

function TVulkanApplicationJoystick.IsGameControllerAttached:boolean;
begin
 if assigned(fGameController) then begin
  result:=SDL_GameControllerGetAttached(fGameController)<>0;
 end else begin
  result:=false;
 end;
end;

function TVulkanApplicationJoystick.GetGameControllerAxis(const pAxis:TVkInt32):TVkInt32;
begin
 if assigned(fGameController) then begin
  case pAxis of
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
end;

function TVulkanApplicationJoystick.GetGameControllerButton(const pButton:TVkInt32):boolean;
begin
 if assigned(fGameController) then begin
  case pButton of
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
end;

function TVulkanApplicationJoystick.GetGameControllerName:TVulkanApplicationRawByteString;
begin
 if assigned(fGameController) then begin
  result:=SDL_GameControllerName(fGameController);
 end else begin
  result:='';
 end;
end;

function TVulkanApplicationJoystick.GetGameControllerMapping:TVulkanApplicationRawByteString;
begin
 if assigned(fGameController) then begin
  result:=SDL_GameControllerMapping(fGameController);
 end else begin
  result:='';
 end;
end;

constructor TVulkanApplicationInput.Create(const pVulkanApplication:TVulkanApplication);
begin
 inherited Create;
 fVulkanApplication:=pVulkanApplication;
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
 fMouseDown:=0;
 fMouseJustDown:=0;
 fMouseDeltaX:=0;
 fMouseDeltaY:=0;
 fJustTouched:=false;
 fMaxPointerID:=-1;
 SetLength(fEvents,1024);
 SetLength(fEventTimes,1024);
 fJoysticks:=TList.Create;
 fMainJoystick:=nil;
end;

destructor TVulkanApplicationInput.Destroy;
begin
 while fJoysticks.Count>0 do begin
  TVulkanApplicationJoystick(fJoysticks[fJoysticks.Count-1]).Free;
  fJoysticks.Delete(fJoysticks.Count-1);
 end;
 fJoysticks.Free;
 SetLength(fEvents,0);
 fCriticalSection.Free;
 inherited Destroy;
end;

function TVulkanApplicationInput.TranslateSDLKeyCode(const pKeyCode,pScanCode:TVkInt32):TVkInt32;
begin
 case pKeyCode of
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
   case pScanCode of
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

function TVulkanApplicationInput.TranslateSDLKeyModifier(const pKeyModifier:TVkInt32):TVkInt32;
begin
 result:=0;
 if (pKeyModifier and PasVulkanSDL2.KMOD_LSHIFT)<>0 then begin
  result:=result or KEYMODIFIER_LSHIFT;
 end;
 if (pKeyModifier and PasVulkanSDL2.KMOD_RSHIFT)<>0 then begin
  result:=result or KEYMODIFIER_RSHIFT;
 end;
 if (pKeyModifier and PasVulkanSDL2.KMOD_LCTRL)<>0 then begin
  result:=result or KEYMODIFIER_LCTRL;
 end;
 if (pKeyModifier and PasVulkanSDL2.KMOD_RCTRL)<>0 then begin
  result:=result or KEYMODIFIER_RCTRL;
 end;
 if (pKeyModifier and PasVulkanSDL2.KMOD_LALT)<>0 then begin
  result:=result or KEYMODIFIER_LALT;
 end;
 if (pKeyModifier and PasVulkanSDL2.KMOD_RALT)<>0 then begin
  result:=result or KEYMODIFIER_RALT;
 end;
 if (pKeyModifier and PasVulkanSDL2.KMOD_LMETA)<>0 then begin
  result:=result or KEYMODIFIER_LMETA;
 end;
 if (pKeyModifier and PasVulkanSDL2.KMOD_RMETA)<>0 then begin
  result:=result or KEYMODIFIER_RMETA;
 end;
 if (pKeyModifier and PasVulkanSDL2.KMOD_NUM)<>0 then begin
  result:=result or KEYMODIFIER_NUM;
 end;
 if (pKeyModifier and PasVulkanSDL2.KMOD_CAPS)<>0 then begin
  result:=result or KEYMODIFIER_CAPS;
 end;
 if (pKeyModifier and PasVulkanSDL2.KMOD_MODE)<>0 then begin
  result:=result or KEYMODIFIER_MODE;
 end;
 if (pKeyModifier and PasVulkanSDL2.KMOD_RESERVED)<>0 then begin
  result:=result or KEYMODIFIER_RESERVED;
 end;
end;

const SDL_KEYTYPED=$30000;

procedure TVulkanApplicationInput.AddEvent(const pEvent:TSDL_Event);
begin
 if fEventCount>=length(fEvents) then begin
  SetLength(fEvents,(fEventCount+1)*2);
  SetLength(fEventTimes,(fEventCount+1)*2);
 end;
 fEvents[fEventCount]:=pEvent;
 fEventTimes[fEventCount]:=VulkanApplication.fHighResolutionTimer.ToNanoseconds(VulkanApplication.fHighResolutionTimer.GetTime);
 inc(fEventCount);
end;

procedure TVulkanApplicationInput.ProcessEvents;
var Index,PointerID,KeyCode,KeyModifier:TVkInt32;
    Event:PSDL_Event;
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
      KeyModifier:=TranslateSDLKeyModifier(Event^.key.keysym.modifier);
      case Event^.type_ of
       SDL_KEYDOWN:begin
        fKeyDown[KeyCode and $ffff]:=true;
        inc(fKeyDownCount);
        if assigned(fProcessor) then begin
         fProcessor.KeyDown(KeyCode,KeyModifier);
        end;
       end;
       SDL_KEYUP:begin
        fKeyDown[KeyCode and $ffff]:=false;
        if fKeyDownCount>0 then begin
         dec(fKeyDownCount);
        end;
        if assigned(fProcessor) then begin
         fProcessor.KeyUp(KeyCode,KeyModifier);
        end;
       end;
       SDL_KEYTYPED:begin
        if assigned(fProcessor) then begin
         fProcessor.KeyTyped(KeyCode,KeyModifier);
        end;
       end;
      end;
     end;
     SDL_MOUSEMOTION:begin
      fMouseX:=Event^.motion.x;
      fMouseY:=Event^.motion.y;
      fMouseDeltaX:=Event^.motion.xrel;
      fMouseDeltaY:=Event^.motion.yrel;
      if assigned(fProcessor) then begin
       if fMouseDown<>0 then begin
        fProcessor.TouchDragged(Event^.motion.x,Event^.motion.y,1.0,0);
       end else begin
        fProcessor.MouseMoved(Event^.motion.x,Event^.motion.y);
       end;
      end;
     end;
     SDL_MOUSEBUTTONDOWN:begin
      fMaxPointerID:=max(fMaxPointerID,0);
 {    fMouseDeltaX:=Event^.button.x-fMouseX;
      fMouseDeltaY:=Event^.button.y-fMouseY;}
      fMouseX:=Event^.button.x;
      fMouseY:=Event^.button.y;
      case Event^.button.button of
       SDL_BUTTON_LEFT:begin
        fMouseDown:=fMouseDown or 1;
        fMouseJustDown:=fMouseJustDown or 1;
        fJustTouched:=true;
        if assigned(fProcessor) then begin
         fProcessor.TouchDown(Event^.motion.x,Event^.motion.y,1.0,0,BUTTON_LEFT);
        end;
       end;
       SDL_BUTTON_RIGHT:begin
        fMouseDown:=fMouseDown or 2;
        fMouseJustDown:=fMouseJustDown or 2;
        fJustTouched:=true;
        if assigned(fProcessor) then begin
         fProcessor.TouchDown(Event^.motion.x,Event^.motion.y,1.0,0,BUTTON_RIGHT);
        end;
       end;
       SDL_BUTTON_MIDDLE:begin
        fMouseDown:=fMouseDown or 4;
        fMouseJustDown:=fMouseJustDown or 4;
        fJustTouched:=true;
        if assigned(fProcessor) then begin
         fProcessor.TouchDown(Event^.motion.x,Event^.motion.y,1.0,0,BUTTON_MIDDLE);
        end;
       end;
      end;
     end;
     SDL_MOUSEBUTTONUP:begin
      fMaxPointerID:=max(fMaxPointerID,0);
 {    fMouseDeltaX:=Event^.button.x-fMouseX;
      fMouseDeltaY:=Event^.button.y-fMouseY;}
      fMouseX:=Event^.button.x;
      fMouseY:=Event^.button.y;
      case Event^.button.button of
       SDL_BUTTON_LEFT:begin
        fMouseDown:=fMouseDown and not 1;
        fMouseJustDown:=fMouseJustDown and not 1;
        if assigned(fProcessor) then begin
         fProcessor.TouchUp(Event^.motion.x,Event^.motion.y,1.0,0,BUTTON_LEFT);
        end;
       end;
       SDL_BUTTON_RIGHT:begin
        fMouseDown:=fMouseDown and not 2;
        fMouseJustDown:=fMouseJustDown and not 2;
        if assigned(fProcessor) then begin
         fProcessor.TouchUp(Event^.motion.x,Event^.motion.y,1.0,0,BUTTON_RIGHT);
        end;
       end;
       SDL_BUTTON_MIDDLE:begin
        fMouseDown:=fMouseDown and not 4;
        fMouseJustDown:=fMouseJustDown and not 4;
        if assigned(fProcessor) then begin
         fProcessor.TouchUp(Event^.motion.x,Event^.motion.y,1.0,0,BUTTON_MIDDLE);
        end;
       end;
      end;
     end;
     SDL_MOUSEWHEEL:begin
      if assigned(fProcessor) then begin
       fProcessor.Scrolled(Event^.wheel.x+Event^.wheel.y);
      end;
     end;
     SDL_FINGERMOTION:begin
      PointerID:=Event^.tfinger.fingerId and $ffff;
      fMaxPointerID:=max(fMaxPointerID,PointerID+1);
      fPointerX[PointerID]:=Event^.tfinger.x*VulkanApplication.fWidth;
      fPointerY[PointerID]:=Event^.tfinger.y*VulkanApplication.fHeight;
      fPointerPressure[PointerID]:=Event^.tfinger.pressure;
      fPointerDeltaX[PointerID]:=Event^.tfinger.dx*VulkanApplication.fWidth;
      fPointerDeltaY[PointerID]:=Event^.tfinger.dy*VulkanApplication.fHeight;
      if assigned(fProcessor) then begin
       fProcessor.TouchDragged(fPointerX[PointerID],fPointerY[PointerID],fPointerPressure[PointerID],PointerID+1);
      end;
     end;
     SDL_FINGERDOWN:begin
      inc(fPointerDownCount);
      PointerID:=Event^.tfinger.fingerId and $ffff;
      fMaxPointerID:=max(fMaxPointerID,PointerID+1);
      fPointerX[PointerID]:=Event^.tfinger.x;
      fPointerY[PointerID]:=Event^.tfinger.y;
      fPointerPressure[PointerID]:=Event^.tfinger.pressure;
      fPointerDeltaX[PointerID]:=Event^.tfinger.dx;
      fPointerDeltaY[PointerID]:=Event^.tfinger.dy;
      fPointerDown[PointerID]:=true;
      fPointerJustDown[PointerID]:=true;
      fJustTouched:=true;
      if assigned(fProcessor) then begin
       fProcessor.TouchDown(fPointerX[PointerID],fPointerY[PointerID],fPointerPressure[PointerID],PointerID+1,0);
      end;
     end;
     SDL_FINGERUP:begin
      if fPointerDownCount>0 then begin
       dec(fPointerDownCount);
      end;
      PointerID:=Event^.tfinger.fingerId and $ffff;
      fMaxPointerID:=max(fMaxPointerID,PointerID+1);
      fPointerX[PointerID]:=Event^.tfinger.x;
      fPointerY[PointerID]:=Event^.tfinger.y;
      fPointerPressure[PointerID]:=Event^.tfinger.pressure;
      fPointerDeltaX[PointerID]:=Event^.tfinger.dx;
      fPointerDeltaY[PointerID]:=Event^.tfinger.dy;
      fPointerDown[PointerID]:=false;
      fPointerJustDown[PointerID]:=false;
      if assigned(fProcessor) then begin
       fProcessor.TouchUp(fPointerX[PointerID],fPointerY[PointerID],fPointerPressure[PointerID],PointerID+1,0);
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

function TVulkanApplicationInput.GetAccelerometerX:single;
begin
 result:=0.0;
end;

function TVulkanApplicationInput.GetAccelerometerY:single;
begin
 result:=0.0;
end;

function TVulkanApplicationInput.GetAccelerometerZ:single;
begin
 result:=0.0;
end;

function TVulkanApplicationInput.GetOrientationAzimuth:single;
begin
 result:=0.0;
end;

function TVulkanApplicationInput.GetOrientationPitch:single;
begin
 result:=0.0;
end;

function TVulkanApplicationInput.GetOrientationRoll:single;
begin
 result:=0.0;
end;

function TVulkanApplicationInput.GetMaxPointerID:TVkInt32;
begin
 fCriticalSection.Acquire;
 try
  result:=fMaxPointerID;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.GetPointerX(const pPointerID:TVkInt32=0):single;
begin
 fCriticalSection.Acquire;
 try
  if pPointerID=0 then begin
   result:=fMouseX;
  end else if (pPointerID>0) and (pPointerID<=$10000) then begin
   result:=fPointerX[pPointerID-1];
  end else begin
   result:=0.0;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.GetPointerDeltaX(const pPointerID:TVkInt32=0):single;
begin
 fCriticalSection.Acquire;
 try
  if pPointerID=0 then begin
   result:=fMouseDeltaX;
  end else if (pPointerID>0) and (pPointerID<=$10000) then begin
   result:=fPointerDeltaX[pPointerID-1];
  end else begin
   result:=0.0;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.GetPointerY(const pPointerID:TVkInt32=0):single;
begin
 fCriticalSection.Acquire;
 try
  if pPointerID=0 then begin
   result:=fMouseY;
  end else if (pPointerID>0) and (pPointerID<=$10000) then begin
   result:=fPointerY[pPointerID-1];
  end else begin
   result:=0.0;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.GetPointerDeltaY(const pPointerID:TVkInt32=0):single;
begin
 fCriticalSection.Acquire;
 try
  if pPointerID=0 then begin
   result:=fMouseDeltaY;
  end else if (pPointerID>0) and (pPointerID<=$10000) then begin
   result:=fPointerDeltaY[pPointerID-1];
  end else begin
   result:=0.0;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.GetPointerPressure(const pPointerID:TVkInt32=0):single;
begin
 fCriticalSection.Acquire;
 try
  if pPointerID=0 then begin
   result:=ord(fMouseDown<>0) and 1;
  end else if (pPointerID>0) and (pPointerID<=$10000) then begin
   result:=fPointerPressure[pPointerID-1];
  end else begin
   result:=0.0;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.IsPointerTouched(const pPointerID:TVkInt32=0):boolean;
begin
 fCriticalSection.Acquire;
 try
  if pPointerID=0 then begin
   result:=fMouseDown<>0;
  end else if (pPointerID>0) and (pPointerID<=$10000) then begin
   result:=fPointerDown[pPointerID-1];
  end else begin
   result:=false;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.IsPointerJustTouched(const pPointerID:TVkInt32=0):boolean;
begin
 fCriticalSection.Acquire;
 try
  if pPointerID=0 then begin
   result:=fMouseJustDown<>0;
   fMouseJustDown:=0;
  end else if (pPointerID>0) and (pPointerID<=$10000) then begin
   result:=fPointerJustDown[pPointerID-1];
   fPointerJustDown[pPointerID-1]:=false;
  end else begin
   result:=false;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.IsTouched:boolean;
begin
 fCriticalSection.Acquire;
 try
  result:=(fMouseDown<>0) or (fPointerDownCount<>0);
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.JustTouched:boolean;
begin
 fCriticalSection.Acquire;
 try
  result:=fJustTouched;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.IsButtonPressed(const pButton:TVkInt32):boolean;
begin
 fCriticalSection.Acquire;
 try
  case pButton of
   BUTTON_LEFT:begin
    result:=(fMouseDown and 1)<>0;
   end;
   BUTTON_RIGHT:begin
    result:=(fMouseDown and 2)<>0;
   end;
   BUTTON_MIDDLE:begin
    result:=(fMouseDown and 4)<>0;
   end;
   else begin
    result:=false;
   end;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.IsKeyPressed(const pKeyCode:TVkInt32):boolean;
begin
 fCriticalSection.Acquire;
 try
  case pKeyCode of
   KEYCODE_ANYKEY:begin
    result:=fKeyDownCount>0;
   end;
   $0000..$ffffF:begin
    result:=fKeyDown[pKeyCode and $ffff];
   end;
   else begin
    result:=false;
   end;
  end;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.GetKeyName(const pKeyCode:TVkInt32):TVulkanApplicationRawByteString;
begin
 if (pKeyCode>=low(fKeyCodeNames)) and (pKeyCode<=high(fKeyCodeNames)) then begin
  result:=fKeyCodeNames[pKeyCode];
 end else begin
  result:='';
 end;
end;

function TVulkanApplicationInput.GetKeyModifier:TVkInt32;
begin
 result:=TranslateSDLKeyModifier(SDL_GetModState);
end;

procedure TVulkanApplicationInput.GetTextInput(const pCallback:TVulkanApplicationInputTextInputCallback;const pTitle,pText:TVulkanApplicationRawByteString;const pPlaceholder:TVulkanApplicationRawByteString='');
begin
end;

procedure TVulkanApplicationInput.SetOnscreenKeyboardVisible(const pVisible:boolean);
begin
end;

procedure TVulkanApplicationInput.Vibrate(const pMilliseconds:TVkInt32);
begin
end;

procedure TVulkanApplicationInput.Vibrate(const pPattern:array of TVkInt32;const pRepeats:TVkInt32);
begin
end;

procedure TVulkanApplicationInput.CancelVibrate;
begin
end;

procedure TVulkanApplicationInput.GetRotationMatrix(const pMatrix3x3:pointer);
begin
end;

function TVulkanApplicationInput.GetCurrentEventTime:int64;
begin
 result:=fCurrentEventTime;
end;

procedure TVulkanApplicationInput.SetCatchBackKey(const pCatchBack:boolean);
begin
end;

procedure TVulkanApplicationInput.SetCatchMenuKey(const pCatchMenu:boolean);
begin
end;

procedure TVulkanApplicationInput.SetInputProcessor(const pProcessor:TVulkanApplicationInputProcessor);
begin
 fCriticalSection.Acquire;
 try
  fProcessor:=pProcessor;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.GetInputProcessor:TVulkanApplicationInputProcessor;
begin
 fCriticalSection.Acquire;
 try
  result:=fProcessor;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.IsPeripheralAvailable(const pPeripheral:TVkInt32):boolean;
begin
 fCriticalSection.Acquire;
 try
  case pPeripheral of
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

function TVulkanApplicationInput.GetNativeOrientation:TVkInt32;
var SDLDisplayMode:TSDL_DisplayMode;
begin
 if SDL_GetDesktopDisplayMode(SDL_GetWindowDisplayIndex(VulkanApplication.fSurfaceWindow),@SDLDisplayMode)=0 then begin
  if SDLDisplayMode.w<SDLDisplayMode.h then begin
   result:=ORIENTATION_LANDSCAPE;
  end else begin
   result:=ORIENTATION_PORTRAIT;
  end;
 end else begin
  result:=ORIENTATION_LANDSCAPE;
 end;
end;

procedure TVulkanApplicationInput.SetCursorCatched(const pCatched:boolean);
begin
 fCriticalSection.Acquire;
 try
  VulkanApplication.fCatchMouse:=pCatched;
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.IsCursorCatched:boolean;
begin
 fCriticalSection.Acquire;
 try
  result:=VulkanApplication.fCatchMouse;
 finally
  fCriticalSection.Release;
 end;
end;

procedure TVulkanApplicationInput.SetCursorPosition(const pX,pY:TVkInt32);
begin
 fCriticalSection.Acquire;
 try
  SDL_WarpMouseInWindow(VulkanApplication.fSurfaceWindow,pX,pY);
 finally
  fCriticalSection.Release;
 end;
end;

function TVulkanApplicationInput.GetJoystickCount:TVkInt32;
begin
 result:=SDL_NumJoysticks;
end;

function TVulkanApplicationInput.GetJoystick(const pIndex:TVkInt32=-1):TVulkanApplicationJoystick;
var ListIndex:TVkInt32;
begin
 if (pIndex>=0) and (pIndex<SDL_NumJoysticks) then begin
  result:=nil;
  for ListIndex:=0 to fJoysticks.Count-1 do begin
   if TVulkanApplicationJoystick(fJoysticks[ListIndex]).fIndex=pIndex then begin
    result:=TVulkanApplicationJoystick(fJoysticks[ListIndex]);
    exit;
   end;
  end;
 end else begin
  result:=fMainJoystick;
 end;
end;

constructor TVulkanScreen.Create;
begin
 inherited Create;
end;

destructor TVulkanScreen.Destroy;
begin
 inherited Destroy;
end;

procedure TVulkanScreen.Show;
begin
end;

procedure TVulkanScreen.Hide;
begin
end;

procedure TVulkanScreen.Resume;
begin
end;

procedure TVulkanScreen.Pause;
begin
end;

procedure TVulkanScreen.Resize(const pWidth,pHeight:TSDLInt32);
begin
end;

procedure TVulkanScreen.AfterCreateSwapChain;
begin
end;

procedure TVulkanScreen.BeforeDestroySwapChain; 
begin
end;

function TVulkanScreen.HandleEvent(const pEvent:TSDL_Event):boolean;
begin
 result:=false;
end;

function TVulkanScreen.CanBeParallelProcessed:boolean;
begin
 result:=false;
end;

procedure TVulkanScreen.Update(const pDeltaTime:double);
begin
end;

procedure TVulkanScreen.Draw(const pSwapChainImageIndex:TVkInt32;var pWaitSemaphore:TVulkanSemaphore;const pWaitFence:TVulkanFence=nil);
begin
end;

constructor TVulkanApplicationAssets.Create(const pVulkanApplication:TVulkanApplication);
begin
 inherited Create;
 fVulkanApplication:=pVulkanApplication;
{$ifndef Android}
 fBasePath:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'assets');
{$endif}
end;

destructor TVulkanApplicationAssets.Destroy;
begin
 inherited Destroy;
end;

function TVulkanApplicationAssets.CorrectFileName(const pFileName:string):string;
begin
 result:=StringReplace(StringReplace({$ifndef Android}fBasePath+{$endif}pFileName,'/',PathDelim,[rfReplaceAll]),'\',PathDelim,[rfReplaceAll]);
end;

function TVulkanApplicationAssets.ExistAsset(const pFileName:string):boolean;
{$ifdef Android}
var Asset:PAAsset;
begin
 result:=false;
 if assigned(AndroidAssetManager) then begin
  Asset:=AAssetManager_open(AndroidAssetManager,pansichar(TVulkanApplicationRawByteString(CorrectFileName(pFileName))),AASSET_MODE_UNKNOWN);
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
 result:=FileExists(CorrectFileName(pFileName));
end;
{$endif}

function TVulkanApplicationAssets.GetAssetStream(const pFileName:string):TStream;
{$ifdef Android}
var Asset:PAAsset;
    Size:int64;
begin
 result:=nil;
 if assigned(AndroidAssetManager) then begin
  Asset:=AAssetManager_open(AndroidAssetManager,pansichar(TVulkanApplicationRawByteString(CorrectFileName(pFileName))),AASSET_MODE_UNKNOWN);
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
   raise Exception.Create('Asset "'+pFileName+'" not found');
  end;
 end else begin
  raise Exception.Create('Asset manager is null');
 end;
end;
{$else}
begin
 result:=TFileStream.Create(CorrectFileName(pFileName),fmOpenRead or fmShareDenyWrite);
end;
{$endif}

function TVulkanApplicationAssets.GetAssetSize(const pFileName:string):int64;
{$ifdef Android}
var Asset:PAAsset;
begin
 result:=0;
 if assigned(AndroidAssetManager) then begin
  Asset:=AAssetManager_open(AndroidAssetManager,pansichar(TVulkanApplicationRawByteString(CorrectFileName(pFileName))),AASSET_MODE_UNKNOWN);
  if assigned(Asset) then begin
   try
    result:=AAsset_getLength(Asset);
   finally
    AAsset_close(Asset);
   end;
  end else begin
   raise Exception.Create('Asset "'+pFileName+'" not found');
  end;
 end else begin
  raise Exception.Create('Asset manager is null');
 end;
end;
{$else}
var Stream:TStream;
begin
 Stream:=TFileStream.Create(CorrectFileName(pFileName),fmOpenRead or fmShareDenyWrite);
 try
  result:=Stream.Size;
 finally
  Stream.Free;
 end;
end;
{$endif}

constructor TVulkanApplicationFiles.Create(const pVulkanApplication:TVulkanApplication);
begin
 inherited Create;
 fVulkanApplication:=pVulkanApplication;
end;

destructor TVulkanApplicationFiles.Destroy;
begin
 inherited Destroy;
end;

function TVulkanApplicationFiles.GetLocalStoragePath:string;
begin
 if length(fVulkanApplication.fLocalDataPath)>0 then begin
  result:=IncludeTrailingPathDelimiter(fVulkanApplication.fLocalDataPath);
 end else begin
  result:='';
 end;
end;

function TVulkanApplicationFiles.GetRoamingStoragePath:string;
begin
 if length(fVulkanApplication.fRoamingDataPath)>0 then begin
  result:=IncludeTrailingPathDelimiter(fVulkanApplication.fRoamingDataPath);
 end else begin
  result:='';
 end;
end;

function TVulkanApplicationFiles.GetExternalStoragePath:string;
begin
 if length(fVulkanApplication.fExternalStoragePath)>0 then begin
  result:=IncludeTrailingPathDelimiter(fVulkanApplication.fExternalStoragePath);
 end else begin
  result:='';
 end;
end;

function TVulkanApplicationFiles.IsLocalStorageAvailable:boolean;
begin
 result:=length(fVulkanApplication.fLocalDataPath)>0;
end;

function TVulkanApplicationFiles.IsRoamingStorageAvailable:boolean;
begin
 result:=length(fVulkanApplication.fRoamingDataPath)>0;
end;

function TVulkanApplicationFiles.IsExternalStorageAvailable:boolean;
begin
 result:=length(fVulkanApplication.fExternalStoragePath)>0;
end;

constructor TVulkanApplication.Create;
begin

 SDL_SetMainReady;

 SDL_SetHint(SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING,'1');
 SDL_SetHint(SDL_HINT_ANDROID_SEPARATE_MOUSE_AND_TOUCH,'1');

 inherited Create;

 fTitle:='SDL2 Vulkan Application';
 fVersion:=$0100;

 fPathName:='SDL2VulkanApplication';

 fLocalDataPath:='';

 fRoamingDataPath:='';

 fExternalStoragePath:='';

 fPasMPInstance:=TPasMP.Create(-1,0,false,true,false,false);

 fHighResolutionTimer:=TVulkanApplicationHighResolutionTimer.Create;

 fAssets:=TVulkanApplicationAssets.Create(self);

 fFiles:=TVulkanApplicationFiles.Create(self);

 fInput:=TVulkanApplicationInput.Create(self);

 fLastPressedKeyEvent.type_:=0;
 fKeyRepeatTimeAccumulator:=0;
 fKeyRepeatInterval:=fHighResolutionTimer.MillisecondInterval*100;
 fKeyRepeatInitialInterval:=fHighResolutionTimer.MillisecondInterval*400;

 fCurrentWidth:=-1;
 fCurrentHeight:=-1;
 fCurrentFullscreen:=-1;
 fCurrentVSync:=-1;
 fCurrentVisibleMouseCursor:=-1;
 fCurrentCatchMouse:=-1;
 fCurrentHideSystemBars:=-1;
 fCurrentBlocking:=-1;

 fWidth:=1280;
 fHeight:=720;
 fFullscreen:=false;
 fVSync:=false;
 fResizable:=true;
 fVisibleMouseCursor:=false;
 fCatchMouse:=false;
 fHideSystemBars:=false;
 fBlocking:=true;

 fActive:=true;

 fTerminated:=false;

 fGraphicsReady:=false;

 fVulkanDebugging:=false;

 fVulkanDebuggingEnabled:=false;

 fVulkanValidation:=false;

 fVulkanInstance:=nil;

 fVulkanDevice:=nil;

 fCountCPUThreads:=Max(1,TPasMP.GetCountOfHardwareThreads(fAvailableCPUCores));
{$if defined(fpc) and defined(android)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar(TVulkanApplicationRawByteString('Detected CPU thread count: '+IntToStr(fCountCPUThreads))));
{$ifend}

 fVulkanCountCommandQueues:=0;
 
 fVulkanCommandPools:=nil;
 fVulkanCommandBuffers:=nil;
 fVulkanCommandBufferFences:=nil;

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

 fVulkanRecreationKind:=vavrkNone;

 fVulkanSwapChain:=nil;

 fVulkanOldSwapChain:=nil;

 fScreen:=nil;

 fNextScreen:=nil;

 fHasNewNextScreen:=false;

 fHasLastTime:=false;

 fLastTime:=0;
 fNowTime:=0;
 fDeltaTime:=0;

 fFrameCounter:=0;

 fCountSwapChainImages:=1;

 fCurrentImageIndex:=0;
 
 fCurrentSwapChainImageIndex:=0;
 
 fOnEvent:=nil;

 VulkanApplication:=self;

end;

destructor TVulkanApplication.Destroy;
begin
 FreeAndNil(fInput);
 FreeAndNil(fFiles);
 FreeAndNil(fAssets);
 FreeAndNil(fHighResolutionTimer);
 FreeAndNil(fPasMPInstance);
 VulkanApplication:=nil;
 inherited Destroy;
end;

procedure TVulkanApplication.VulkanDebugLn(const What:TVkCharString);
{$if defined(Windows)}
var StdOut:THandle;
begin
 StdOut:=GetStdHandle(Std_Output_Handle);
 Win32Check(StdOut<>Invalid_Handle_Value);
 if StdOut<>0 then begin
  WriteLn(What);
 end;
end;
{$elseif (defined(fpc) and defined(android)) and not defined(Release)}
begin
 __android_log_write(ANDROID_LOG_DEBUG,'PasVulkanApplication',PAnsiChar(What));
end;
{$else}
begin
 WriteLn(What);
end;
{$ifend}

function TVulkanApplication.VulkanOnDebugReportCallback(const flags:TVkDebugReportFlagsEXT;const objectType:TVkDebugReportObjectTypeEXT;const object_:TVkUInt64;const location:TVkSize;messageCode:TVkInt32;const pLayerPrefix:TVulkaNCharString;const pMessage:TVulkanCharString):TVkBool32;
begin
 VulkanDebugLn('[Debug] '+pLayerPrefix+': '+pMessage);
 result:=VK_FALSE;
end;

procedure TVulkanApplication.VulkanWaitIdle;
var Index:TVkInt32;
begin
 if assigned(fVulkanDevice) then begin
  fVulkanDevice.WaitIdle;
  for Index:=0 to fCountSwapChainImages-1 do begin
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

procedure TVulkanApplication.CreateVulkanDevice(const pSurface:TVulkanSurface=nil);
var QueueFamilyIndex,ThreadIndex,SwapChainImageIndex:TVkInt32;
    FormatProperties:TVkFormatProperties;
begin
 if not assigned(VulkanDevice) then begin

  fVulkanDevice:=TVulkanDevice.Create(VulkanInstance,nil,pSurface,nil);
  fVulkanDevice.AddQueues;
  fVulkanDevice.EnabledExtensionNames.Add(VK_KHR_SWAPCHAIN_EXTENSION_NAME);
  fVulkanDevice.Initialize;

  fVulkanCountCommandQueues:=length(fVulkanDevice.PhysicalDevice.QueueFamilyProperties);
  SetLength(fVulkanCommandPools,fVulkanCountCommandQueues,fCountCPUThreads+1,MaxSwapChainImages);
  SetLength(fVulkanCommandBuffers,fVulkanCountCommandQueues,fCountCPUThreads+1,MaxSwapChainImages);
  SetLength(fVulkanCommandBufferFences,fVulkanCountCommandQueues,fCountCPUThreads+1,MaxSwapChainImages);
  for QueueFamilyIndex:=0 to length(fVulkanDevice.PhysicalDevice.QueueFamilyProperties)-1 do begin
   if (QueueFamilyIndex=fVulkanDevice.PresentQueueFamilyIndex) or
      (QueueFamilyIndex=fVulkanDevice.GraphicsQueueFamilyIndex) or
      (QueueFamilyIndex=fVulkanDevice.ComputeQueueFamilyIndex) or
      (QueueFamilyIndex=fVulkanDevice.TransferQueueFamilyIndex) then begin
    for ThreadIndex:=0 to fCountCPUThreads do begin
     for SwapChainImageIndex:=0 to MaxSwapChainImages-1 do begin
      fVulkanCommandPools[QueueFamilyIndex,ThreadIndex,SwapChainImageIndex]:=TVulkanCommandPool.Create(fVulkanDevice,QueueFamilyIndex,TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
      fVulkanCommandBuffers[QueueFamilyIndex,ThreadIndex,SwapChainImageIndex]:=TVulkanCommandBuffer.Create(fVulkanCommandPools[QueueFamilyIndex,ThreadIndex,SwapChainImageIndex],VK_COMMAND_BUFFER_LEVEL_PRIMARY);
      fVulkanCommandBufferFences[QueueFamilyIndex,ThreadIndex,SwapChainImageIndex]:=TVulkanFence.Create(fVulkanDevice);
     end;
    end;
   end;
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
   raise EVulkanException.Create('No suitable depth image format!');
  end;

 end;
end;

procedure TVulkanApplication.CreateVulkanInstance;
var i:TVkInt32;
    SDL_SysWMinfo:TSDL_SysWMinfo;
begin
 if not assigned(fVulkanInstance) then begin
  SDL_VERSION(SDL_SysWMinfo.version);
  if SDL_GetWindowWMInfo(fSurfaceWindow,@SDL_SysWMinfo)<>0 then begin
   fVulkanInstance:=TVulkanInstance.Create(Title,Version,
                                           'PasVulkanApplication',$0100,
                                           VK_API_VERSION_1_0,false,nil);
   for i:=0 to fVulkanInstance.AvailableLayerNames.Count-1 do begin
    VulkanDebugLn('Layer: '+fVulkanInstance.AvailableLayerNames[i]);
   end;
   for i:=0 to fVulkanInstance.AvailableExtensionNames.Count-1 do begin
    VulkanDebugLn('Extension: '+fVulkanInstance.AvailableExtensionNames[i]);
   end;
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
     raise EVulkanException.Create('Vulkan initialization failure');
    end;
   end;
   if fVulkanDebugging and
      (fVulkanInstance.AvailableExtensionNames.IndexOf(VK_EXT_DEBUG_REPORT_EXTENSION_NAME)>=0) then begin
    fVulkanInstance.EnabledExtensionNames.Add(VK_EXT_DEBUG_REPORT_EXTENSION_NAME);
    fVulkanDebuggingEnabled:=true;
    if fVulkanValidation then begin
     if fVulkanInstance.AvailableLayerNames.IndexOf('VK_LAYER_LUNARG_standard_validation')>=0 then begin
      fVulkanInstance.EnabledLayerNames.Add('VK_LAYER_LUNARG_standard_validation');
     end;
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

procedure TVulkanApplication.DestroyVulkanInstance;
var Index,SubIndex,SubSubIndex:TVkInt32;
begin

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
 FreeAndNil(fVulkanDevice);
 FreeAndNil(fVulkanInstance);
//VulkanPresentationSurface:=nil;

 fVulkanDevice:=nil;
 fVulkanInstance:=nil;

end;

procedure TVulkanApplication.CreateVulkanSurface;
var SDL_SysWMinfo:TSDL_SysWMinfo;
    VulkanSurfaceCreateInfo:TVulkanSurfaceCreateInfo;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TVulkanApplication.AllocateVulkanSurface');
{$ifend}
 if not assigned(fVulkanSurface) then begin
  SDL_VERSION(SDL_SysWMinfo.version);
  if SDL_GetWindowWMInfo(fSurfaceWindow,@SDL_SysWMinfo)<>0 then begin
   FillChar(VulkanSurfaceCreateInfo,SizeOf(TVulkanSurfaceCreateInfo),#0);
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
     raise EVulkanException.Create('Vulkan initialization failure');
     exit;
    end;
{$ifend}
    else begin
     raise EVulkanException.Create('Vulkan initialization failure');
     exit;
    end;
   end;

   fVulkanSurface:=TVulkanSurface.Create(fVulkanInstance,VulkanSurfaceCreateInfo);

   if not assigned(fVulkanDevice) then begin
    CreateVulkanDevice(fVulkanSurface);
    if not assigned(fVulkanDevice) then begin
     raise EVulkanSurfaceException.Create('Device does not support surface');
    end;
   end;

  end;
 end;
{$if (defined(fpc) and defined(android)) and not defined(Release)}

  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TVulkanApplication.AllocateVulkanSurface');
{$ifend}
end;

procedure TVulkanApplication.DestroyVulkanSurface;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering TVulkanApplication.FreeVulkanSurface');
{$ifend}
 FreeAndNil(fVulkanSurface);
{$if (defined(fpc) and defined(android)) and not defined(Release)}
  __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving TVulkanApplication.FreeVulkanSurface');
{$ifend}
end;

procedure TVulkanApplication.CreateVulkanSwapChain;
var Index:TVkInt32;
begin
 DestroyVulkanSwapChain;

 fVulkanSwapChain:=TVulkanSwapChain.Create(fVulkanDevice,
                                           fVulkanSurface,
                                           fVulkanOldSwapChain,
                                           fWidth,
                                           fHeight,
                                           IfThen(fVSync,MaxSwapChainImages,1),
                                           1,
                                           VK_FORMAT_UNDEFINED,
                                           VK_COLOR_SPACE_SRGB_NONLINEAR_KHR,
                                           TVkImageUsageFlags(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT),
                                           VK_SHARING_MODE_EXCLUSIVE,
                                           nil,
                                           VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
                                           TVkPresentModeKHR(integer(IfThen(fVSync,integer(VK_PRESENT_MODE_FIFO_KHR),integer(VK_PRESENT_MODE_IMMEDIATE_KHR)))));

 fCountSwapChainImages:=fVulkanSwapChain.CountImages;

 fCurrentImageIndex:=0;

 fCurrentSwapChainImageIndex:=0;

 for Index:=0 to fCountSwapChainImages-1 do begin
  fVulkanWaitFences[Index]:=TVulkanFence.Create(fVulkanDevice);
  fVulkanWaitFencesReady[Index]:=false;
  fVulkanPresentCompleteSemaphores[Index]:=TVulkanSemaphore.Create(fVulkanDevice);
 end;

end;

procedure TVulkanApplication.DestroyVulkanSwapChain;
var Index:TVkInt32;
begin
 for Index:=0 to fCountSwapChainImages-1 do begin
  fVulkanWaitFencesReady[Index]:=false;
  FreeAndNil(fVulkanWaitFences[Index]);
  FreeAndNil(fVulkanPresentCompleteSemaphores[Index]);
 end;
 FreeAndNil(fVulkanSwapChain);
end;

procedure TVulkanApplication.CreateVulkanRenderPass;
begin

 DestroyVulkanRenderPass;

 fVulkanRenderPass:=TVulkanRenderPass.Create(VulkanApplication.VulkanDevice);

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

end;

procedure TVulkanApplication.DestroyVulkanRenderPass;
begin
 FreeAndNil(fVulkanRenderPass);
end;

procedure TVulkanApplication.CreateVulkanFrameBuffers;
var Index:TVkInt32;
    ColorAttachmentImage:TVulkanImage;
    ColorAttachmentImageView:TVulkanImageView;
begin

 DestroyVulkanFrameBuffers;

 SetLength(fVulkanFrameBufferColorAttachments,fVulkanSwapChain.CountImages);

 for Index:=0 to fVulkanSwapChain.CountImages-1 do begin
  fVulkanFrameBufferColorAttachments[Index]:=nil;
 end;

 for Index:=0 to fVulkanSwapChain.CountImages-1 do begin

  ColorAttachmentImage:=nil;

  ColorAttachmentImageView:=nil;

  try

   ColorAttachmentImage:=TVulkanImage.Create(fVulkanDevice,fVulkanSwapChain.Images[Index].Handle,nil,false);

   ColorAttachmentImage.SetLayout(TVkImageAspectFlags(VK_IMAGE_ASPECT_COLOR_BIT),
                                  VK_IMAGE_LAYOUT_UNDEFINED,
                                  VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
                                  nil,
                                  VulkanPresentCommandBuffers[0,0],
                                  fVulkanDevice.PresentQueue,
                                  VulkanPresentCommandBufferFences[0,0],
                                  true);

   ColorAttachmentImageView:=TVulkanImageView.Create(fVulkanDevice,
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

   fVulkanFrameBufferColorAttachments[Index]:=TVulkanFrameBufferAttachment.Create(fVulkanDevice,
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

 fVulkanDepthFrameBufferAttachment:=TVulkanFrameBufferAttachment.Create(fVulkanDevice,
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
  fVulkanFrameBuffers[Index]:=TVulkanFrameBuffer.Create(fVulkanDevice,
                                                        fVulkanRenderPass,
                                                        fVulkanSwapChain.Width,
                                                        fVulkanSwapChain.Height,
                                                        1,
                                                        [fVulkanFrameBufferColorAttachments[Index],fVulkanDepthFrameBufferAttachment],
                                                        false);
 end;

end;

procedure TVulkanApplication.DestroyVulkanFrameBuffers;
var Index:TVkInt32;
begin
 for Index:=0 to length(fVulkanFrameBufferColorAttachments)-1 do begin
  FreeAndNil(fVulkanFrameBufferColorAttachments[Index]);
 end;
 fVulkanFrameBufferColorAttachments:=nil;
 FreeAndNil(fVulkanDepthFrameBufferAttachment);
 for Index:=0 to length(fVulkanFrameBuffers)-1 do begin
  FreeAndNil(fVulkanFrameBuffers[Index]);
 end;
 fVulkanFrameBuffers:=nil;
end;

procedure TVulkanApplication.CreateVulkanBlankCommandBuffers;
var Index:TVkInt32;
begin
 DestroyVulkanBlankCommandBuffers;

 fVulkanBlankCommandPool:=TVulkanCommandPool.Create(fVulkanDevice,
                                                    fVulkanDevice.GraphicsQueueFamilyIndex,
                                                    TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 for Index:=0 to CountSwapChainImages-1 do begin
  fVulkanBlankCommandBuffers[Index]:=TVulkanCommandBuffer.Create(fVulkanBlankCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fVulkanBlankCommandBufferSemaphores[Index]:=TVulkanSemaphore.Create(fVulkanDevice);
 end;

end;

procedure TVulkanApplication.DestroyVulkanBlankCommandBuffers;
var Index:TVkInt32;
begin
 for Index:=0 to CountSwapChainImages-1 do begin
  FreeAndNil(fVulkanBlankCommandBuffers[Index]);
  FreeAndNil(fVulkanBlankCommandBufferSemaphores[Index]);
 end;
 FreeAndNil(fVulkanBlankCommandPool);
end;

procedure TVulkanApplication.SetScreen(const pScreen:TVulkanScreen);
begin
 if fScreen<>pScreen then begin
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
  fScreen:=pScreen;
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
  end;
 end;
end;

function TVulkanApplication.AcquireVulkanBackBuffer:boolean;
var ImageIndex:TVkInt32;
    TimeOut:TVkUInt64;
begin
 result:=false;

 if fVulkanRecreationKind=vavrkNone then begin

  if (fVulkanSwapChain.Width<>Width) or
     (fVulkanSwapChain.Height<>Height) or
     ((fVSync and (fVulkanSwapChain.PresentMode<>VK_PRESENT_MODE_FIFO_KHR)) or
      ((not fVSync) and (fVulkanSwapChain.PresentMode<>VK_PRESENT_MODE_IMMEDIATE_KHR))) then begin
   if fVulkanRecreationKind<vavrkSwapChain then begin
    fVulkanRecreationKind:=vavrkSwapChain;
   end;
   VulkanDebugLn('New surface dimension size and/or vertical synchronization setting detected!');
  end else begin
   try
    if fBlocking then begin
     TimeOut:=TVkUInt64(high(TVkUInt64));
    end else begin
     TimeOut:=0;
    end;
    case fVulkanSwapChain.AcquireNextImage(fVulkanPresentCompleteSemaphores[fCurrentImageIndex],
                                           nil,
                                           TimeOut) of
     VK_SUCCESS:begin
      fCurrentSwapChainImageIndex:=fVulkanSwapChain.CurrentImageIndex;
     end;
     VK_SUBOPTIMAL_KHR:begin
      if fVulkanRecreationKind<vavrkSwapChain then begin
       fVulkanRecreationKind:=vavrkSwapChain;
      end;
      VulkanDebugLn('Suboptimal surface detected!');
     end;
     else {VK_SUCCESS,VK_TIMEOUT:}begin
      exit;
     end;
    end;
   except
    on VulkanResultException:EVulkanResultException do begin
     case VulkanResultException.ResultCode of
      VK_ERROR_SURFACE_LOST_KHR:begin
       if fVulkanRecreationKind<vavrkSurface then begin
        fVulkanRecreationKind:=vavrkSurface;
       end;
       VulkanDebugLn(VulkanResultException.ClassName+': '+VulkanResultException.Message);
      end;
      VK_ERROR_OUT_OF_DATE_KHR,
      VK_SUBOPTIMAL_KHR:begin
       if fVulkanRecreationKind<vavrkSwapChain then begin
        fVulkanRecreationKind:=vavrkSwapChain;
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

 if fVulkanRecreationKind in [vavrkSwapChain,vavrkSurface] then begin

  for ImageIndex:=0 to fCountSwapChainImages-1 do begin
   if fVulkanWaitFencesReady[ImageIndex] then begin
    fVulkanWaitFences[ImageIndex].WaitFor;
    fVulkanWaitFences[ImageIndex].Reset;
    fVulkanWaitFencesReady[ImageIndex]:=false;
   end;
  end;

  fVulkanDevice.WaitIdle;

  if fVulkanRecreationKind=vavrkSurface then begin
   VulkanDebugLn('Recreating vulkan surface... ');
  end else begin
   VulkanDebugLn('Recreating vulkan swap chain... ');
  end;
  fVulkanOldSwapChain:=fVulkanSwapChain;
  try
   VulkanWaitIdle;
   BeforeDestroySwapChain;
   fVulkanSwapChain:=nil;
   DestroyVulkanBlankCommandBuffers;
   DestroyVulkanFrameBuffers;
   DestroyVulkanRenderPass;
   DestroyVulkanSwapChain;
   if fVulkanRecreationKind=vavrkSurface then begin
    DestroyVulkanSurface;
    CreateVulkanSurface;
   end;
   CreateVulkanSwapChain;
   CreateVulkanRenderPass;
   CreateVulkanFrameBuffers;
   CreateVulkanBlankCommandBuffers;
   VulkanWaitIdle;
   AfterCreateSwapChain;
  finally
   FreeAndNil(fVulkanOldSwapChain);
  end;
  if fVulkanRecreationKind=vavrkSurface then begin
   VulkanDebugLn('Recreated vulkan surface... ');
  end else begin
   VulkanDebugLn('Recreated vulkan swap chain... ');
  end;

  fVulkanRecreationKind:=vavrkNone;

  fVulkanWaitSemaphore:=nil;
  fVulkanWaitFence:=nil;

 end else begin

  if fVulkanWaitFencesReady[fCurrentSwapChainImageIndex] then begin
   if fVulkanWaitFences[fCurrentSwapChainImageIndex].GetStatus<>VK_SUCCESS then begin
    if fBlocking then begin
     fVulkanWaitFences[fCurrentSwapChainImageIndex].WaitFor;
    end else begin
     exit;
    end;
   end;
   fVulkanWaitFences[fCurrentSwapChainImageIndex].Reset;
   fVulkanWaitFencesReady[fCurrentSwapChainImageIndex]:=false;
  end;

  fVulkanWaitSemaphore:=fVulkanPresentCompleteSemaphores[fCurrentImageIndex];

  fVulkanWaitFence:=fVulkanWaitFences[fCurrentSwapChainImageIndex];

  result:=true;

 end;
end;

function TVulkanApplication.PresentVulkanBackBuffer:boolean;
begin
 result:=false;

 fVulkanWaitFencesReady[fCurrentSwapChainImageIndex]:=true;

//fVulkanDevice.GraphicsQueue.WaitIdle; // A GPU/CPU graphics queue synchronization point only for debug cases here, when something got run wrong

 try
  case fVulkanSwapChain.QueuePresent(fVulkanDevice.GraphicsQueue,fVulkanWaitSemaphore) of
   VK_SUCCESS:begin
    //fVulkanDevice.WaitIdle; // A GPU/CPU frame synchronization point only for debug cases here, when something got run wrong
    inc(fCurrentImageIndex);
    if fCurrentImageIndex>=fCountSwapChainImages then begin
     fCurrentImageIndex:=0;
    end;
    result:=true;
   end;
   VK_SUBOPTIMAL_KHR:begin
    if fVulkanRecreationKind<vavrkSwapChain then begin
     fVulkanRecreationKind:=vavrkSwapChain;
    end;
   end;
  end;
 except
  on VulkanResultException:EVulkanResultException do begin
   case VulkanResultException.ResultCode of
    VK_ERROR_SURFACE_LOST_KHR:begin
     if fVulkanRecreationKind<vavrkSurface then begin
      fVulkanRecreationKind:=vavrkSurface;
     end;
    end;
    VK_ERROR_OUT_OF_DATE_KHR,
    VK_SUBOPTIMAL_KHR:begin
     if fVulkanRecreationKind<vavrkSwapChain then begin
      fVulkanRecreationKind:=vavrkSwapChain;
     end;
    end;
    else begin
     raise;
    end;
   end;
  end;
 end;

end;

procedure TVulkanApplication.SetNextScreen(const pNextScreen:TVulkanScreen);
begin
 if (fScreen<>pNextScreen) and (fNextScreen<>pNextScreen) then begin
  if assigned(fNextScreen) then begin
   fNextScreen.Free;
  end;
  fNextScreen:=pNextScreen;
  fHasNewNextScreen:=true;
 end;
end;

procedure TVulkanApplication.ReadConfig;
begin
end;

procedure TVulkanApplication.SaveConfig;
begin
end;

procedure TVulkanApplication.Initialize;
begin
end;

procedure TVulkanApplication.Terminate;
begin
 fTerminated:=true;
end;

procedure TVulkanApplication.InitializeGraphics;
begin
 if not fGraphicsReady then begin
  try
   fGraphicsReady:=true;
   CreateVulkanSurface;
   CreateVulkanSwapChain;
   CreateVulkanRenderPass;
   CreateVulkanFrameBuffers;
   CreateVulkanBlankCommandBuffers;
   VulkanWaitIdle;
   AfterCreateSwapChain;
  except
   Terminate;
   raise;
  end;
 end;
end;

procedure TVulkanApplication.DeinitializeGraphics;
begin
 if fGraphicsReady then begin
  VulkanWaitIdle;
  BeforeDestroySwapChain;
  DestroyVulkanBlankCommandBuffers;
  DestroyVulkanFrameBuffers;
  DestroyVulkanRenderPass;
  DestroyVulkanSwapChain;
  DestroyVulkanSurface;
  fGraphicsReady:=false;
 end;
end;

function TVulkanApplication.HandleEvent(const pEvent:TSDL_Event):boolean;
begin
 if assigned(fOnEvent) and fOnEvent(self,pEvent) then begin
  result:=true;
 end else if assigned(fScreen) and fScreen.HandleEvent(pEvent) then begin
  result:=true;
 end else begin
  result:=false;
 end;
end;

procedure TVulkanApplication.UpdateFrameTimesHistory;
var Index,Count:TVkInt32;
    SumOfFrameTimes:double;
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

procedure TVulkanApplication.UpdateJobFunction(const pJob:PPasMPJob;const pThreadIndex:TPasMPInt32);
begin
 Update(Min(Max(fFloatDeltaTime,0.0),0.25));
end;

procedure TVulkanApplication.DrawJobFunction(const pJob:PPasMPJob;const pThreadIndex:TPasMPInt32);
begin
 Draw(fCurrentSwapChainImageIndex,fVulkanWaitSemaphore,fVulkanWaitFence);
end;

procedure TVulkanApplication.ProcessMessages;
var Index,Counter:TVkInt32;
    Joystick:TVulkanApplicationJoystick;
    SDLJoystick:PSDL_Joystick;
    SDLGameController:PSDL_GameController;
    OK,Found,DoUpdateMainJoystick:boolean;
    Jobs:array[0..1] of PPasMPJob;
begin

 DoUpdateMainJoystick:=false;

 if fCurrentHideSystemBars<>ord(fHideSystemBars) then begin
  if fHideSystemBars then begin
   SDL_SetHint(SDL_HINT_ANDROID_HIDE_SYSTEM_BARS,'1');
  end else begin
   SDL_SetHint(SDL_HINT_ANDROID_HIDE_SYSTEM_BARS,'0');
  end;
 end;

 if fCurrentVisibleMouseCursor<>ord(fVisibleMouseCursor) then begin
  fCurrentVisibleMouseCursor:=ord(fVisibleMouseCursor);
  if fVisibleMouseCursor then begin
   SDL_ShowCursor(1);
  end else begin
   SDL_ShowCursor(0);
  end;
 end;

 if fCurrentCatchMouse<>ord(fCatchMouse) then begin
  fCurrentCatchMouse:=ord(fCatchMouse);
  if fCatchMouse then begin
   SDL_SetRelativeMouseMode(1);
  end else begin
   SDL_SetRelativeMouseMode(0);
  end;
 end;

 if fHasNewNextScreen then begin
  fHasNewNextScreen:=false;
  if fScreen<>fNextScreen then begin
   SetScreen(fNextScreen);
  end;
  fNextScreen:=nil;
 end;

 if (fCurrentWidth<>fWidth) or (fCurrentHeight<>fHeight) or (fCurrentVSync<>ord(fVSync)) then begin
  fCurrentWidth:=fWidth;
  fCurrentHeight:=fHeight;
  fCurrentVSync:=ord(fVSync);
  if not fFullscreen then begin
   SDL_SetWindowSize(fSurfaceWindow,fWidth,fHeight);
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

  if fLastPressedKeyEvent.type_<>0 then begin
   dec(fKeyRepeatTimeAccumulator,fDeltaTime);
   while fKeyRepeatTimeAccumulator<0 do begin
    inc(fKeyRepeatTimeAccumulator,fKeyRepeatInterval);
    fInput.AddEvent(fLastPressedKeyEvent);
   end;
  end;

  while SDL_PollEvent(@fEvent)<>0 do begin
   if HandleEvent(fEvent) then begin
    continue;
   end;
   case fEvent.type_ of
    SDL_QUITEV,
    SDL_APP_TERMINATING:begin
     VulkanWaitIdle;
     Pause;
     DeinitializeGraphics;
     Terminate;
    end;
    SDL_APP_WILLENTERBACKGROUND:begin
{$if defined(fpc) and defined(android)}
     __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar(TVulkanApplicationRawByteString('SDL_APP_WILLENTERBACKGROUND')));
{$ifend}
     fActive:=false;
     VulkanWaitIdle;
     Pause;
     DeinitializeGraphics;
     fHasLastTime:=false;
    end;
    SDL_APP_DIDENTERBACKGROUND:begin
{$if defined(fpc) and defined(android)}
     __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar(TVulkanApplicationRawByteString('SDL_APP_DIDENTERBACKGROUND')));
{$ifend}
    end;
    SDL_APP_WILLENTERFOREGROUND:begin
{$if defined(fpc) and defined(android)}
     __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar(TVulkanApplicationRawByteString('SDL_APP_WILLENTERFOREGROUND')));
{$ifend}
    end;
    SDL_APP_DIDENTERFOREGROUND:begin
     InitializeGraphics;
     Resume;
     fActive:=true;
     fHasLastTime:=false;
{$if defined(fpc) and defined(android)}
     __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar(TVulkanApplicationRawByteString('SDL_APP_DIDENTERFOREGROUND')));
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
     case fEvent.window.event of
      SDL_WINDOWEVENT_RESIZED:begin
       fWidth:=fEvent.window.Data1;
       fHeight:=fEvent.window.Data2;
       fCurrentWidth:=fWidth;
       fCurrentHeight:=fHeight;
       if fGraphicsReady then begin
        VulkanDebugLn('New surface dimension size detected!');
{$if true}
        if fVulkanRecreationKind<vavrkSwapChain then begin
         fVulkanRecreationKind:=vavrkSwapChain;
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
     Index:=fEvent.jdevice.which;
     Found:=false;
     for Counter:=0 to fInput.fJoysticks.Count-1 do begin
      Joystick:=TVulkanApplicationJoystick(fInput.fJoysticks.Items[Counter]);
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
       end;
      end else begin
       SDLGameController:=nil;
       SDLJoystick:=SDL_JoystickOpen(Index);
      end;
      if assigned(SDLJoystick) then begin
       Joystick:=TVulkanApplicationJoystick.Create(Index,SDLJoystick,SDLGameController);
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
      Joystick:=TVulkanApplicationJoystick(fInput.fJoysticks.Items[Counter]);
      if assigned(Joystick) and (Joystick.ID=fEvent.jdevice.which) then begin
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
     case fEvent.key.keysym.sym of
      SDLK_F4:begin
       if ((fEvent.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0) then begin
        OK:=false;
        if fEvent.key.repeat_=0 then begin
         Terminate;
        end;
       end;
      end;
      SDLK_RETURN:begin
       if ((fEvent.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0) then begin
        if fEvent.key.repeat_=0 then begin
         OK:=false;
         fFullScreen:=not fFullScreen;
        end;
       end;
      end;
     end;
     if OK then begin
      if fEvent.key.repeat_=0 then begin
       fInput.AddEvent(fEvent);
       fEvent.type_:=SDL_KEYTYPED;
       fInput.AddEvent(fEvent);
       fLastPressedKeyEvent:=fEvent;
       fKeyRepeatTimeAccumulator:=fKeyRepeatInitialInterval;
      end;
     end;
    end;
    SDL_KEYUP:begin
     OK:=true;
     case fEvent.key.keysym.sym of
      SDLK_F4:begin
       if ((fEvent.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0) then begin
        OK:=false;
       end;
      end;
      SDLK_RETURN:begin
       if ((fEvent.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0) then begin
        OK:=false;
       end;
      end;
     end;
     if OK then begin
      if fEvent.key.repeat_=0 then begin
       fInput.AddEvent(fEvent);
       fLastPressedKeyEvent.type_:=0;
      end;
     end;
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
  if DoUpdateMainJoystick then begin
   fInput.fMainJoystick:=nil;
   if fInput.fJoysticks.Count>0 then begin
    for Counter:=0 to fInput.fJoysticks.Count-1 do begin
     Joystick:=TVulkanApplicationJoystick(fInput.fJoysticks.Items[Counter]);
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
  if fFullScreen then begin
   SDL_SetWindowFullscreen(fSurfaceWindow,SDL_WINDOW_FULLSCREEN_DESKTOP);
  end else begin
   SDL_SetWindowFullscreen(fSurfaceWindow,0);
  end;
 end;

 if fGraphicsReady then begin

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

    if assigned(fScreen) and fScreen.CanBeParallelProcessed then begin

     Jobs[0]:=fPasMPInstance.Acquire(UpdateJobFunction);
     Jobs[1]:=fPasMPInstance.Acquire(DrawJobFunction);
     fPasMPInstance.Invoke(Jobs);

     inc(fFrameCounter);
     
    end else begin

     UpdateJobFunction(nil,0);

     inc(fFrameCounter);

     DrawJobFunction(nil,0);

    end;

   finally
    PresentVulkanBackBuffer;
   end;

  end;

 end else begin
  fDeltaTime:=0;
 end;

end;

procedure TVulkanApplication.Run;
begin

{$if defined(Android)}

 fLocalDataPath:=IncludeTrailingPathDelimiter(GetAppConfigDir(false));

 fRoamingDataPath:=IncludeTrailingPathDelimiter(GetAppConfigDir(false));

 fExternalStoragePath:=IncludeTrailingPathDelimiter(GetEnvironmentVariable('EXTERNAL_STORAGE'));

{$elseif (defined(Windows) or defined(Linux) or defined(Unix)) and not defined(Android)}
 fLocalDataPath:=GetAppDataLocalPath(fPathName);

 fRoamingDataPath:=GetAppDataRoamingPath(fPathName);

{$if defined(Windows)}
 fExternalStoragePath:='C:\';
{$else}
 fExternalStoragePath:='/';
{$ifend}

{$ifend}

 ReadConfig;

 if fHideSystemBars then begin
  SDL_SetHint(SDL_HINT_ANDROID_HIDE_SYSTEM_BARS,'1');
 end else begin
  SDL_SetHint(SDL_HINT_ANDROID_HIDE_SYSTEM_BARS,'0');
 end;
 fCurrentHideSystemBars:=ord(fHideSystemBars);

 if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_EVENTS or SDL_INIT_TIMER)<0 then begin
  raise EVulkanApplication.Create('Unable to initialize SDL: '+SDL_GetError);
 end;

{$ifdef Unix}
 InstallSignalHandlers;
{$endif}

 if SDL_GetCurrentDisplayMode(0,@fSDLDisplayMode)=0 then begin
  fScreenWidth:=fSDLDisplayMode.w;
  fScreenHeight:=fSDLDisplayMode.h;
 end else begin
  fScreenWidth:=-1;
  fScreenHeight:=-1;
 end;

 fVideoFlags:=0;
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
{$ifdef Android}
 fVideoFlags:=fVideoFlags or SDL_WINDOW_ALLOW_HIGHDPI;

 SDL_GL_SetAttribute(SDL_GL_RED_SIZE,8);
 SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE,8);
 SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE,8);
 SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE,8);
 SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE,0);
 SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE,24);

 SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION,3);
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION,2);
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK,SDL_GL_CONTEXT_PROFILE_ES);
 SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS,0);

{$endif}

{$if defined(fpc) and defined(android)}
 fVideoFlags:=fVideoFlags or SDL_WINDOW_FULLSCREEN or SDL_WINDOW_VULKAN;
 fFullscreen:=true;
 fCurrentFullscreen:=ord(true);
 fWidth:=fScreenWidth;
 fHeight:=fScreenHeight;
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar(TVulkanApplicationRawByteString('Window size: '+IntToStr(fWidth)+'x'+IntToStr(fHeight))));
{$ifend}

 fSurfaceWindow:=SDL_CreateWindow(PAnsiChar(TVulkanApplicationRawByteString(fTitle)),
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
  raise EVulkanApplication.Create('Unable to initialize SDL: '+SDL_GetError);
 end;

 fCurrentWidth:=fWidth;
 fCurrentHeight:=fHeight;

 fCurrentVSync:=ord(fVSync);

 SDL_EventState(SDL_MOUSEMOTION,SDL_ENABLE);
 SDL_EventState(SDL_MOUSEBUTTONDOWN,SDL_ENABLE);
 SDL_EventState(SDL_KEYDOWN,SDL_ENABLE);
 SDL_EventState(SDL_QUITEV,SDL_ENABLE);
 SDL_EventState(SDL_WINDOWEVENT,SDL_ENABLE);

 FillChar(fFrameTimesHistoryDeltaTimes,SizeOf(fFrameTimesHistoryDeltaTimes),#0);
 FillChar(fFrameTimesHistoryTimePoints,SizeOf(fFrameTimesHistoryTimePoints),#$ff);
 fFrameTimesHistoryReadIndex:=0;
 fFrameTimesHistoryWriteIndex:=0;

 fFramesPerSecond:=0.0;

 try

  CreateVulkanInstance;
  try
          
   Start;
   try

    InitializeGraphics;
    try

     Load;
     try

      if assigned(fStartScreen) then begin
       SetScreen(fStartScreen.Create);
      end;
      try

       while not fTerminated do begin
        ProcessMessages;
       end;

      finally

       SetScreen(nil);

       FreeAndNil(fNextScreen);
       FreeAndNil(fScreen);

      end;

     finally

      VulkanWaitIdle;

      Unload;

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

  if assigned(fSurfaceWindow) then begin
   SDL_DestroyWindow(fSurfaceWindow);
   fSurfaceWindow:=nil;
  end;

  SaveConfig;

 end;

end;

procedure TVulkanApplication.Start;
begin
end;

procedure TVulkanApplication.Stop; 
begin
end;

procedure TVulkanApplication.Load;
begin
end;

procedure TVulkanApplication.Unload;
begin
end;

procedure TVulkanApplication.AfterCreateSwapChain;
begin
 if assigned(fScreen) then begin
  fScreen.AfterCreateSwapChain;
 end;
end;                           

procedure TVulkanApplication.BeforeDestroySwapChain;
begin
 if assigned(fScreen) then begin
  fScreen.BeforeDestroySwapChain;
 end;
end;

procedure TVulkanApplication.Update(const pDeltaTime:double);
begin
 if assigned(fScreen) then begin
  fScreen.Update(pDeltaTime);
 end;
end;

procedure TVulkanApplication.Draw(const pSwapChainImageIndex:TVkInt32;var pWaitSemaphore:TVulkanSemaphore;const pWaitFence:TVulkanFence=nil);
var VulkanCommandBuffer:TVulkanCommandBuffer;
begin
 if assigned(fScreen) then begin
  fScreen.Draw(pSwapChainImageIndex,pWaitSemaphore,pWaitFence);
 end else begin

  VulkanCommandBuffer:=fVulkanBlankCommandBuffers[pSwapChainImageIndex];

  VulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

  VulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

  fVulkanRenderPass.BeginRenderPass(VulkanCommandBuffer,
                                    fVulkanFrameBuffers[pSwapChainImageIndex],
                                    VK_SUBPASS_CONTENTS_INLINE,
                                    0,
                                    0,
                                    fVulkanSwapChain.Width,
                                    fVulkanSwapChain.Height);

  fVulkanRenderPass.EndRenderPass(VulkanCommandBuffer);

  VulkanCommandBuffer.Execute(fVulkanDevice.GraphicsQueue,
                              TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                              pWaitSemaphore,
                              fVulkanBlankCommandBufferSemaphores[pSwapChainImageIndex],
                              pWaitFence,
                              false);

  pWaitSemaphore:=fVulkanBlankCommandBufferSemaphores[pSwapChainImageIndex];

 end;
end;

procedure TVulkanApplication.Resume;
begin
 if assigned(fScreen) then begin
  fScreen.Resume;
 end;
end;

procedure TVulkanApplication.Pause;
begin
 if assigned(fScreen) then begin
  fScreen.Pause;
 end;
end;

class procedure TVulkanApplication.Main;
begin
end;

{$if defined(fpc) and defined(android)}
function AndroidGetDeviceName:TVulkanApplicationUnicodeString;
var AndroisOSBuild:jclass;
    ModelID:JFieldID;
    ModelStringObject:JString;
    ModelStringChars:PJChar;
begin
 AndroisOSBuild:=AndroidJavaEnv^.FindClass(AndroidJavaEnv,'android/os/Build');
 ModelID:=AndroidJavaEnv^.GetStaticFieldID(AndroidJavaEnv,AndroisOSBuild,'MODEL','Ljava/lang/String;');
 ModelStringObject:=AndroidJavaEnv^.GetStaticObjectField(AndroidJavaEnv,AndroisOSBuild,ModelID);
 ModelStringChars:=AndroidJavaEnv^.GetStringChars(AndroidJavaEnv,ModelStringObject,nil);
 result:=PWideChar(Pointer(ModelStringChars));
 AndroidJavaEnv^.ReleaseStringChars(AndroidJavaEnv,ModelStringObject,ModelStringChars);
end;

function Android_JNI_GetEnv:PJNIEnv; cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Android_JNI_GetEnv . . .');
{$ifend}
 if assigned(VulkanApplication) then begin
  result:=AndroidJavaEnv;
 end else begin
  result:=nil;
 end;
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Android_JNI_GetEnv . . .');
{$ifend}
end;

procedure ANativeActivity_onCreate(pActivity:PANativeActivity;pSavedState:pointer;pSavedStateSize:cuint32); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering ANativeActivity_onCreate . . .');
{$ifend}
 AndroidActivity:=pActivity;
 AndroidAssetManager:=pActivity^.assetManager;
 AndroidInternalDataPath:=pActivity^.internalDataPath;
 AndroidExternalDataPath:=pActivity^.externalDataPath;
 AndroidLibraryPath:=IncludeTrailingPathDelimiter(ExtractFilePath(pActivity^.internalDataPath))+'lib';
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving ANativeActivity_onCreate . . .');
{$ifend}
end;
{$ifend}

{$ifdef Windows}
initialization
 timeBeginPeriod(1);
finalization
 timeEndPeriod(1);
{$endif}
end.
