(******************************************************************************
 *                                 PasVulkan                                  *
 ******************************************************************************
 *                       Version see PasVulkan.Framework.pas                  *
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
unit PasVulkan.SDL2;
{$ifdef fpc}
 {$mode delphi}
 {$packrecords c}
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

{$ifdef linux}
  {$define unix}
{$endif}
{$ifdef freebsd}
  {$define unix}
{$endif}
{$ifdef darwin}
  {$define unix}
{$endif}
{$ifdef haiku}
  {$define unix}
{$endif}

{$ifdef unix}
  {$ifndef darwin}
    {$linklib c}
  {$endif}
  {$ifdef haiku}
    {$linklib root}
  {$else}
    {$ifndef android}
      {$linklib pthread}
    {$endif}
  {$endif}
{$endif}

{$ifdef staticlink}
  {$ifdef fpc}
    {$ifdef linux}
      {$linklib m}
    {$endif}
    {$ifdef win32}
      {$linklib SDL2}
      {$linklib SDL2main}
      {$linklib winmm}
      {$linklib kernel32}
      {$linklib gdi32}
      {$linklib user32}
      {$linklib imm32}
      {$linklib ole32}
      {$linklib oleaut32}
      {$linklib msvcrt}
      {$linklib version}
      {$linklib uuid}
      {$linklib shell32}
      {$linklib dinput8}
      {$linklib dxerr8}
      {$linklib dxguid}
      {$linklib moldname}
    {$endif}
    {$ifdef win64}
      {$linklib SDL2}
      {$linklib SDL2main}
      {$linklib winmm}
      {$linklib kernel32}
      {$linklib gdi32}
      {$linklib user32}
      {$linklib imm32}
      {$linklib ole32}
      {$linklib oleaut32}
      {$linklib msvcrt}
      {$linklib version}
      {$linklib uuid}
      {$linklib shell32}
      {$linklib dinput8}
      {$linklib dxerr8}
      {$linklib dxguid}
      {$linklib moldname}
    {$endif}
  {$endif}
{$endif}

interface

uses {$if defined(Windows)}
      Windows,
     {$elseif defined(Unix)}
      BaseUnix,
      Unix,
      UnixType,
      dl,
      {$if not (defined(GP2X) or defined(Darwin) or defined(SkyOS) or defined(Android))}
       x,
       xlib,
      {$ifend}
     {$ifend}
     {$ifdef staticlink}
      PasVulkan.StaticLinking,
     {$endif}
     {$ifdef Android}
      PasVulkan.Android,
     {$endif}
     Vulkan,
     SysUtils,Classes;

const SDL2LibName={$if defined(Win32)}
                   'sdl2.dll'
                  {$elseif defined(Win64)}
                  'sdl264.dll'
                  {$elseif defined(Darwin)}
                   'SDL2'
                  {$else}
                   'libSDL2.so'
                  {$ifend};

      SDL_MAJOR_VERSION=2;
      SDL_MINOR_VERSION=0;
      SDL_PATCHLEVEL=5;

      SDL_FALSE=0;
      SDL_TRUE=1;

      SDL_INIT_TIMER=$00000001;
      SDL_INIT_AUDIO=$00000010;
      SDL_INIT_VIDEO=$00000020;
      SDL_INIT_JOYSTICK=$00000200;
      SDL_INIT_HAPTIC=$00001000;
      SDL_INIT_GAMECONTROLLER=$00002000;
      SDL_INIT_EVENTS=$000041000;
      SDL_INIT_NOPARACHUTE=$00100000;
      SDL_INIT_EVERYTHING=$0000FFFF;

      SDL_ALLEVENTS=$FFFFFFFF;
      SDL_APPINPUTFOCUS=$02;
      SDL_BUTTON_WHEELUP=4;
      SDL_BUTTON_WHEELDOWN=5;

      // SDL_Event types
      SDL_FIRSTEVENT=0;
      SDL_QUITEV=$100;
      SDL_APP_TERMINATING=$101;
      SDL_APP_LOWMEMORY=$102;
      SDL_APP_WILLENTERBACKGROUND=$103;
      SDL_APP_DIDENTERBACKGROUND=$104;
      SDL_APP_WILLENTERFOREGROUND=$105;
      SDL_APP_DIDENTERFOREGROUND=$106;
      SDL_DISPLAYEVENT=$150;
      SDL_WINDOWEVENT=$200;
      SDL_SYSWMEVENT=$201;
      SDL_KEYDOWN=$300;
      SDL_KEYUP=$301;
      SDL_TEXTEDITING=$302;
      SDL_TEXTINPUT=$303;
      SDL_MOUSEMOTION=$400;
      SDL_MOUSEBUTTONDOWN=$401;
      SDL_MOUSEBUTTONUP=$402;
      SDL_MOUSEWHEEL=$403;
      SDL_INPUTMOTION=$500;
      SDL_INPUTBUTTONDOWN=$501;
      SDL_INPUTBUTTONUP=$502;
      SDL_INPUTWHEEL=$503;
      SDL_INPUTPROXIMITYIN=$504;
      SDL_INPUTPROXIMITYOUT=$505;
      SDL_JOYAXISMOTION=$600;
      SDL_JOYBALLMOTION=$601;
      SDL_JOYHATMOTION=$602;
      SDL_JOYBUTTONDOWN=$603;
      SDL_JOYBUTTONUP=$604;
      SDL_JOYDEVICEADDED=$605;
      SDL_JOYDEVICEREMOVED=$606;
      SDL_CONTROLLERAXISMOTION=$650;
      SDL_CONTROLLERBUTTONDOWN=$651;
      SDL_CONTROLLERBUTTONUP=$652;
      SDL_CONTROLLERDEVICEADDED=$653;
      SDL_CONTROLLERDEVICEREMOVED=$654;
      SDL_CONTROLLERDEVICEREMAPPED=$655;
      SDL_FINGERDOWN=$700;
      SDL_FINGERUP=$701;
      SDL_FINGERMOTION=$702;
      SDL_DOLLARGESTURE=$800;
      SDL_DOLLARRECORD=$801;
      SDL_MULTIGESTURE=$802;
      SDL_CLIPBOARDUPDATE=$900;
      SDL_DROPFILE=$1000;
      SDL_DROPTEXT=$1001;
      SDL_DROPBEGIN=$1002;
      SDL_DROPCOMPLETE=$1003;
      SDL_AUDIODEVICEADDED=$1100;
      SDL_AUDIODEVICEREMOVED=$1101;
      SDL_SENSORUPDATE=$1200;
      SDL_RENDER_TARGETS_RESET=$2000;
      SDL_RENDER_DEVICE_RESET=$2001;
      SDL_USEREVENT=$8000;
      SDL_LASTEVENT=$ffff;
      // no compatibility events $7000

      // SDL_Surface flags
      SDL_SWSURFACE=$00000000;  //*< Not used */
      SDL_PREALLOC=$00000001;  //*< Surface uses preallocated memory */
      SDL_RLEACCEL=$00000002;  //*< Surface is RLE encoded */
      SDL_DONTFREE=$00000004;  //*< Surface is referenced internally */
      SDL_SRCALPHA=$00010000;
      SDL_SRCCOLORKEY=$00020000;
      SDL_ANYFORMAT=$00100000;
      SDL_HWPALETTE=$00200000;
      SDL_DOUBLEBUF=$00400000;
      SDL_FULLSCREEN=$00800000;
      SDL_RESIZABLE=$01000000;
      SDL_NOFRAME=$02000000;
      SDL_OPENGL=$04000000;
      SDL_HWSURFACE=$08000001;  //*< Not used */
      SDL_ASYNCBLIT=$08000000;  //*< Not used */
      SDL_RLEACCELOK=$08000000;  //*< Not used */
      SDL_HWACCEL=$08000000;  //*< Not used */

      // SDL_Renderer flags
      SDL_RENDERER_SOFTWARE=$00000001;     //*< The renderer is a software fallback */
      SDL_RENDERER_ACCELERATED=$00000002;     //*< The renderer uses hardware acceleration */
      SDL_RENDERER_PRESENTVSYNC=$00000004;

      // SDL_WindowFlags (enum)
      SDL_WINDOW_FULLSCREEN=$00000001;      //*< fullscreen window, implies borderless */
      SDL_WINDOW_OPENGL=$00000002;      //*< window usable with OpenGL context */
      SDL_WINDOW_SHOWN=$00000004;      //*< window is visible */
      SDL_WINDOW_HIDDEN=$00000008;      //*< window is not visible */
      SDL_WINDOW_BORDERLESS=$00000010;      //*< no window decoration */
      SDL_WINDOW_RESIZABLE=$00000020;      //*< window can be resized */
      SDL_WINDOW_MINIMIZED=$00000040;      //*< window is minimized */
      SDL_WINDOW_MAXIMIZED=$00000080;      //*< window is maximized */
      SDL_WINDOW_INPUT_GRABBED=$00000100;      //*< window has grabbed input focus */
      SDL_WINDOW_INPUT_FOCUS=$00000200;      //*< window has input focus */
      SDL_WINDOW_MOUSE_FOCUS=$00000400;      //*< window has mouse focus */
      SDL_WINDOW_FOREIGN=$00000800;      //*< window not created by SDL */
      SDL_WINDOW_FULLSCREEN_DESKTOP=SDL_WINDOW_FULLSCREEN or $00001000; //*< fake fullscreen window, that takes the size of the desktop */
      SDL_WINDOW_ALLOW_HIGHDPI=$00002000;      //**< window should be created in high-DPI mode if supported */
      SDL_WINDOW_MOUSE_CAPTURE=$00004000;      //**< window has mouse captured (unrelated to INPUT_GRABBED) */
      SDL_WINDOW_ALWAYS_ON_TOP=$00008000;      //**< window should always be above others */
      SDL_WINDOW_SKIP_TASKBAR=$00010000;     //**< window should not be added to the taskbar */
      SDL_WINDOW_UTILITY=$00020000;      //**< window should be treated as a utility window */
      SDL_WINDOW_TOOLTIP=$00040000;      //**< window should be treated as a tooltip */
      SDL_WINDOW_POPUP_MENU=$00080000;      //**< window should be treated as a popup menu */
{$if defined(Android) and not defined(PasVulkanUseSDL2WithVulkanSupport)}
      // In the case if PasVulkan uses on Android still a self-patched SDL 2.0.5 version
    	SDL_WINDOW_VULKAN=$00100000;   //**< window usable with Vulkan */
{$else}
      // SDL 2.0.6
    	SDL_WINDOW_VULKAN=$10000000;         //**< window usable with Vulkan */
{$ifend}

      SDL_WINDOWPOS_CENTERED_MASK=$2FFF0000;

      // SDL_WindowEventID (enum)
      SDL_WINDOWEVENT_NONE=0;    //*< Never used
      SDL_WINDOWEVENT_SHOWN=1;    //*< Window has been shown
      SDL_WINDOWEVENT_HIDDEN=2;    //*< Window has been hidden
      SDL_WINDOWEVENT_EXPOSED=3;    //*< Window has been exposed and should be redrawn
      SDL_WINDOWEVENT_MOVED=4;    //*< Window has been moved to data1, data2
      SDL_WINDOWEVENT_RESIZED=5;    //*< Window size changed to data1xdata2
      SDL_WINDOWEVENT_SIZE_CHANGED=6;    //*< The window size has changed, [...] */
      SDL_WINDOWEVENT_MINIMIZED=7;    //*< Window has been minimized
      SDL_WINDOWEVENT_MAXIMIZED=8;    //*< Window has been maximized
      SDL_WINDOWEVENT_RESTORED=9;    //*< Window has been restored to normal size and position
      SDL_WINDOWEVENT_ENTER=10;   //*< Window has gained mouse focus
      SDL_WINDOWEVENT_LEAVE=11;   //*< Window has lost mouse focus
      SDL_WINDOWEVENT_FOCUS_GAINED=12;   //*< Window has gained keyboard focus
      SDL_WINDOWEVENT_FOCUS_LOST=13;   //*< Window has lost keyboard focus
      SDL_WINDOWEVENT_CLOSE=14;   //*< The window manager requests that the window be closed */

      SDL_BLENDMODE_NONE=$00000000; // **< No blending
      SDL_BLENDMODE_BLEND=$00000001; // **< dst = (src * A) + (dst * (1-A))
      SDL_BLENDMODE_ADD=$00000002;  // **< dst = (src * A) + dst
      SDL_BLENDMODE_MOD=$00000004;  // **< dst = src * dst

      SDL_DISPLAYEVENT_NONE=0; // Never used
      SDL_DISPLAYEVENT_ORIENTATION=1; // Display orientation has changed to data1

      SDL_ORIENTATION_UNKNOWN=0; // The display orientation can't be determined
      SDL_ORIENTATION_LANDSCAPE=1; //  The display is in landscape mode, with the right side up, relative to portrait mode
      SDL_ORIENTATION_LANDSCAPE_FLIPPED=2; // The display is in landscape mode, with the left side up, relative to portrait mode
      SDL_ORIENTATION_PORTRAIT=3; //  The display is in portrait mode
      SDL_ORIENTATION_PORTRAIT_FLIPPED=4; // The display is in portrait mode, upside dowm

{$ifdef fpc_big_endian}
      RMask=$FF000000;
      GMask=$00FF0000;
      BMask=$0000FF00;
      AMask=$000000FF;
      RShift=24;
      GShift=16;
      BShift=8;
      AShift=0;
{$else}
      RMask=$000000FF;
      GMask=$0000FF00;
      BMask=$00FF0000;
      AMask=$FF000000;
      RShift=0;
      GShift=8;
      BShift=16;
      AShift=24;
{$endif}

      // SDL_mixer
      MIX_MAX_VOLUME=128;
      MIX_INIT_FLAC=$00000001;
      MIX_INIT_MOD=$00000002;
      MIX_INIT_MP3=$00000004;
      MIX_INIT_OGG=$00000008;

      // SDL_TTF
      TTF_STYLE_NORMAL=0;
      TTF_STYLE_BOLD=1;
      TTF_STYLE_ITALIC=2;

      // SDL Joystick
      SDL_HAT_CENTERED=$00;
      SDL_HAT_UP=$01;
      SDL_HAT_RIGHT=$02;
      SDL_HAT_DOWN=$04;
      SDL_HAT_LEFT=$08;
      SDL_HAT_RIGHTUP=SDL_HAT_RIGHT or SDL_HAT_UP;
      SDL_HAT_RIGHTDOWN=SDL_HAT_RIGHT or SDL_HAT_DOWN;
      SDL_HAT_LEFTUP=SDL_HAT_LEFT  or SDL_HAT_UP;
      SDL_HAT_LEFTDOWN=SDL_HAT_LEFT  or SDL_HAT_DOWN;

      // SDL_image
      IMG_INIT_JPG=$00000001;
      IMG_INIT_PNG=$00000002;
      IMG_INIT_TIF=$00000004;

      // SDL_EventMask type definition

      // Enumeration of valid key mods (possibly OR'd together)
      KMOD_NONE=$0000;
      KMOD_LSHIFT=$0001;
      KMOD_RSHIFT=$0002;
      KMOD_LCTRL=$0040;
      KMOD_RCTRL=$0080;
      KMOD_LALT=$0100;
      KMOD_RALT=$0200;
      KMOD_LMETA=$0400;
      KMOD_RMETA=$0800;
      KMOD_NUM=$1000;
      KMOD_CAPS=$2000;
      KMOD_MODE=$4000;
      KMOD_RESERVED=$8000;

      KMOD_CTRL=(KMOD_LCTRL or KMOD_RCTRL);
      KMOD_SHIFT=(KMOD_LSHIFT or KMOD_RSHIFT);
      KMOD_ALT=(KMOD_LALT or KMOD_RALT);
      KMOD_META=(KMOD_LMETA or KMOD_RMETA);

      SDL_SCANCODE_UNKNOWN=0;

      SDL_SCANCODE_A=4;
      SDL_SCANCODE_B=5;
      SDL_SCANCODE_C=6;
      SDL_SCANCODE_D=7;
      SDL_SCANCODE_E=8;
      SDL_SCANCODE_F=9;
      SDL_SCANCODE_G=10;
      SDL_SCANCODE_H=11;
      SDL_SCANCODE_I=12;
      SDL_SCANCODE_J=13;
      SDL_SCANCODE_K=14;
      SDL_SCANCODE_L=15;
      SDL_SCANCODE_M=16;
      SDL_SCANCODE_N=17;
      SDL_SCANCODE_O=18;
      SDL_SCANCODE_P=19;
      SDL_SCANCODE_Q=20;
      SDL_SCANCODE_R=21;
      SDL_SCANCODE_S=22;
      SDL_SCANCODE_T=23;
      SDL_SCANCODE_U=24;
      SDL_SCANCODE_V=25;
      SDL_SCANCODE_W=26;
      SDL_SCANCODE_X=27;
      SDL_SCANCODE_Y=28;
      SDL_SCANCODE_Z=29;

      SDL_SCANCODE_1=30;
      SDL_SCANCODE_2=31;
      SDL_SCANCODE_3=32;
      SDL_SCANCODE_4=33;
      SDL_SCANCODE_5=34;
      SDL_SCANCODE_6=35;
      SDL_SCANCODE_7=36;
      SDL_SCANCODE_8=37;
      SDL_SCANCODE_9=38;
      SDL_SCANCODE_0=39;

      SDL_SCANCODE_RETURN=40;
      SDL_SCANCODE_ESCAPE=41;
      SDL_SCANCODE_BACKSPACE=42;
      SDL_SCANCODE_TAB=43;
      SDL_SCANCODE_SPACE=44;

      SDL_SCANCODE_MINUS=45;
      SDL_SCANCODE_EQUALS=46;
      SDL_SCANCODE_LEFTBRACKET=47;
      SDL_SCANCODE_RIGHTBRACKET=48;
      SDL_SCANCODE_BACKSLASH=49;
      SDL_SCANCODE_NONUSHASH=50;
      SDL_SCANCODE_SEMICOLON=51;
      SDL_SCANCODE_APOSTROPHE=52;
      SDL_SCANCODE_GRAVE=53;
      SDL_SCANCODE_COMMA=54;
      SDL_SCANCODE_PERIOD=55;
      SDL_SCANCODE_SLASH=56;

      SDL_SCANCODE_CAPSLOCK=57;

      SDL_SCANCODE_F1=58;
      SDL_SCANCODE_F2=59;
      SDL_SCANCODE_F3=60;
      SDL_SCANCODE_F4=61;
      SDL_SCANCODE_F5=62;
      SDL_SCANCODE_F6=63;
      SDL_SCANCODE_F7=64;
      SDL_SCANCODE_F8=65;
      SDL_SCANCODE_F9=66;
      SDL_SCANCODE_F10=67;
      SDL_SCANCODE_F11=68;
      SDL_SCANCODE_F12=69;

      SDL_SCANCODE_PRINTSCREEN=70;
      SDL_SCANCODE_SCROLLLOCK=71;
      SDL_SCANCODE_PAUSE=72;
      SDL_SCANCODE_INSERT=73;
      SDL_SCANCODE_HOME=74;
      SDL_SCANCODE_PAGEUP=75;
      SDL_SCANCODE_DELETE=76;
      SDL_SCANCODE_END=77;
      SDL_SCANCODE_PAGEDOWN=78;
      SDL_SCANCODE_RIGHT=79;
      SDL_SCANCODE_LEFT=80;
      SDL_SCANCODE_DOWN=81;
      SDL_SCANCODE_UP=82;

      SDL_SCANCODE_NUMLOCKCLEAR=83;
      SDL_SCANCODE_KP_DIVIDE=84;
      SDL_SCANCODE_KP_MULTIPLY=85;
      SDL_SCANCODE_KP_MINUS=86;
      SDL_SCANCODE_KP_PLUS=87;
      SDL_SCANCODE_KP_ENTER=88;
      SDL_SCANCODE_KP_1=89;
      SDL_SCANCODE_KP_2=90;
      SDL_SCANCODE_KP_3=91;
      SDL_SCANCODE_KP_4=92;
      SDL_SCANCODE_KP_5=93;
      SDL_SCANCODE_KP_6=94;
      SDL_SCANCODE_KP_7=95;
      SDL_SCANCODE_KP_8=96;
      SDL_SCANCODE_KP_9=97;
      SDL_SCANCODE_KP_0=98;
      SDL_SCANCODE_KP_PERIOD=99;

      SDL_SCANCODE_NONUSBACKSLASH=100;
      SDL_SCANCODE_APPLICATION=101;
      SDL_SCANCODE_POWER=102;
      SDL_SCANCODE_KP_EQUALS=103;
      SDL_SCANCODE_F13=104;
      SDL_SCANCODE_F14=105;
      SDL_SCANCODE_F15=106;
      SDL_SCANCODE_F16=107;
      SDL_SCANCODE_F17=108;
      SDL_SCANCODE_F18=109;
      SDL_SCANCODE_F19=110;
      SDL_SCANCODE_F20=111;
      SDL_SCANCODE_F21=112;
      SDL_SCANCODE_F22=113;
      SDL_SCANCODE_F23=114;
      SDL_SCANCODE_F24=115;
      SDL_SCANCODE_EXECUTE=116;
      SDL_SCANCODE_HELP=117;
      SDL_SCANCODE_MENU=118;
      SDL_SCANCODE_SELECT=119;
      SDL_SCANCODE_STOP=120;
      SDL_SCANCODE_AGAIN=121;
      SDL_SCANCODE_UNDO=122;
      SDL_SCANCODE_CUT=123;
      SDL_SCANCODE_COPY=124;
      SDL_SCANCODE_PASTE=125;
      SDL_SCANCODE_FIND=126;
      SDL_SCANCODE_MUTE=127;
      SDL_SCANCODE_VOLUMEUP=128;
      SDL_SCANCODE_VOLUMEDOWN=129;
      SDL_SCANCODE_LOCKINGCAPSLOCK=130;
      SDL_SCANCODE_LOCKINGNUMLOCK=131;
      SDL_SCANCODE_LOCKINGSCROLLLOCK=132;
      SDL_SCANCODE_KP_COMMA=133;
      SDL_SCANCODE_KP_EQUALSAS400=134;

      SDL_SCANCODE_INTERNATIONAL1=135;
      SDL_SCANCODE_INTERNATIONAL2=136;
      SDL_SCANCODE_INTERNATIONAL3=137;
      SDL_SCANCODE_INTERNATIONAL4=138;
      SDL_SCANCODE_INTERNATIONAL5=139;
      SDL_SCANCODE_INTERNATIONAL6=140;
      SDL_SCANCODE_INTERNATIONAL7=141;
      SDL_SCANCODE_INTERNATIONAL8=142;
      SDL_SCANCODE_INTERNATIONAL9=143;
      SDL_SCANCODE_LANG1=144;
      SDL_SCANCODE_LANG2=145;
      SDL_SCANCODE_LANG3=146;
      SDL_SCANCODE_LANG4=147;
      SDL_SCANCODE_LANG5=148;
      SDL_SCANCODE_LANG6=149;
      SDL_SCANCODE_LANG7=150;
      SDL_SCANCODE_LANG8=151;
      SDL_SCANCODE_LANG9=152;

      SDL_SCANCODE_ALTERASE=153;
      SDL_SCANCODE_SYSREQ=154;
      SDL_SCANCODE_CANCEL=155;
      SDL_SCANCODE_CLEAR=156;
      SDL_SCANCODE_PRIOR=157;
      SDL_SCANCODE_RETURN2=158;
      SDL_SCANCODE_SEPARATOR=159;
      SDL_SCANCODE_OUT=160;
      SDL_SCANCODE_OPER=161;
      SDL_SCANCODE_CLEARAGAIN=162;
      SDL_SCANCODE_CRSEL=163;
      SDL_SCANCODE_EXSEL=164;

      SDL_SCANCODE_KP_00=176;
      SDL_SCANCODE_KP_000=177;
      SDL_SCANCODE_THOUSANDSSEPARATOR=178;
      SDL_SCANCODE_DECIMALSEPARATOR=179;
      SDL_SCANCODE_CURRENCYUNIT=180;
      SDL_SCANCODE_CURRENCYSUBUNIT=181;
      SDL_SCANCODE_KP_LEFTPAREN=182;
      SDL_SCANCODE_KP_RIGHTPAREN=183;
      SDL_SCANCODE_KP_LEFTBRACE=184;
      SDL_SCANCODE_KP_RIGHTBRACE=185;
      SDL_SCANCODE_KP_TAB=186;
      SDL_SCANCODE_KP_BACKSPACE=187;
      SDL_SCANCODE_KP_A=188;
      SDL_SCANCODE_KP_B=189;
      SDL_SCANCODE_KP_C=190;
      SDL_SCANCODE_KP_D=191;
      SDL_SCANCODE_KP_E=192;
      SDL_SCANCODE_KP_F=193;
      SDL_SCANCODE_KP_XOR=194;
      SDL_SCANCODE_KP_POWER=195;
      SDL_SCANCODE_KP_PERCENT=196;
      SDL_SCANCODE_KP_LESS=197;
      SDL_SCANCODE_KP_GREATER=198;
      SDL_SCANCODE_KP_AMPERSAND=199;
      SDL_SCANCODE_KP_DBLAMPERSAND=200;
      SDL_SCANCODE_KP_VERTICALBAR=201;
      SDL_SCANCODE_KP_DBLVERTICALBAR=202;
      SDL_SCANCODE_KP_COLON=203;
      SDL_SCANCODE_KP_HASH=204;
      SDL_SCANCODE_KP_SPACE=205;
      SDL_SCANCODE_KP_AT=206;
      SDL_SCANCODE_KP_EXCLAM=207;
      SDL_SCANCODE_KP_MEMSTORE=208;
      SDL_SCANCODE_KP_MEMRECALL=209;
      SDL_SCANCODE_KP_MEMCLEAR=210;
      SDL_SCANCODE_KP_MEMADD=211;
      SDL_SCANCODE_KP_MEMSUBTRACT=212;
      SDL_SCANCODE_KP_MEMMULTIPLY=213;
      SDL_SCANCODE_KP_MEMDIVIDE=214;
      SDL_SCANCODE_KP_PLUSMINUS=215;
      SDL_SCANCODE_KP_CLEAR=216;
      SDL_SCANCODE_KP_CLEARENTRY=217;
      SDL_SCANCODE_KP_BINARY=218;
      SDL_SCANCODE_KP_OCTAL=219;
      SDL_SCANCODE_KP_DECIMAL=220;
      SDL_SCANCODE_KP_HEXADECIMAL=221;

      SDL_SCANCODE_LCTRL=224;
      SDL_SCANCODE_LSHIFT=225;
      SDL_SCANCODE_LALT=226;
      SDL_SCANCODE_LGUI=227;
      SDL_SCANCODE_RCTRL=228;
      SDL_SCANCODE_RSHIFT=229;
      SDL_SCANCODE_RALT=230;
      SDL_SCANCODE_RGUI=231;

      SDL_SCANCODE_MODE=257;

      SDL_SCANCODE_AUDIONEXT=258;
      SDL_SCANCODE_AUDIOPREV=259;
      SDL_SCANCODE_AUDIOSTOP=260;
      SDL_SCANCODE_AUDIOPLAY=261;
      SDL_SCANCODE_AUDIOMUTE=262;
      SDL_SCANCODE_MEDIASELECT=263;
      SDL_SCANCODE_WWW=264;
      SDL_SCANCODE_MAIL=265;
      SDL_SCANCODE_CALCULATOR=266;
      SDL_SCANCODE_COMPUTER=267;
      SDL_SCANCODE_AC_SEARCH=268;
      SDL_SCANCODE_AC_HOME=269;
      SDL_SCANCODE_AC_BACK=270;
      SDL_SCANCODE_AC_FORWARD=271;
      SDL_SCANCODE_AC_STOP=272;
      SDL_SCANCODE_AC_REFRESH=273;
      SDL_SCANCODE_AC_BOOKMARKS=274;

      SDL_SCANCODE_BRIGHTNESSDOWN=275;
      SDL_SCANCODE_BRIGHTNESSUP=276;
      SDL_SCANCODE_DISPLAYSWITCH=277;
      SDL_SCANCODE_KBDILLUMTOGGLE=278;
      SDL_SCANCODE_KBDILLUMDOWN=279;
      SDL_SCANCODE_KBDILLUMUP=280;
      SDL_SCANCODE_EJECT=281;
      SDL_SCANCODE_SLEEP=282;

      SDL_NUM_SCANCODES=512;

      SDLK_UNKNOWN=0;

      SDLK_RETURN=13;
      SDLK_ESCAPE=27;
      SDLK_BACKSPACE=8;
      SDLK_TAB=9;
      SDLK_SPACE=32;
      SDLK_EXCLAIM=ord('!');
      SDLK_QUOTEDBL=ord('"');
      SDLK_HASH=ord('#');
      SDLK_PERCENT=ord('%');
      SDLK_DOLLAR=ord('$');
      SDLK_AMPERSAND=ord('&');
      SDLK_QUOTE=ord('''');
      SDLK_LEFTPAREN=ord('(');
      SDLK_RIGHTPAREN=ord(')');
      SDLK_ASTERISK=ord('*');
      SDLK_PLUS=ord('+');
      SDLK_COMMA=ord(',');
      SDLK_MINUS=ord('-');
      SDLK_PERIOD=ord('.');
      SDLK_SLASH=ord('/');
      SDLK_0=ord('0');
      SDLK_1=ord('1');
      SDLK_2=ord('2');
      SDLK_3=ord('3');
      SDLK_4=ord('4');
      SDLK_5=ord('5');
      SDLK_6=ord('6');
      SDLK_7=ord('7');
      SDLK_8=ord('8');
      SDLK_9=ord('9');
      SDLK_COLON=ord(':');
      SDLK_SEMICOLON=ord(';');
      SDLK_LESS=ord('<');
      SDLK_EQUALS=ord('=');
      SDLK_GREATER=ord('>');
      SDLK_QUESTION=ord('?');
      SDLK_AT=ord('@');

      SDLK_LEFTBRACKET=ord('[');
      SDLK_BACKSLASH=ord('\');
      SDLK_RIGHTBRACKET=ord(']');
      SDLK_CARET=ord('^');
      SDLK_UNDERSCORE=ord('_');
      SDLK_BACKQUOTE=ord('`');
      SDLK_a=ord('a');
      SDLK_b=ord('b');
      SDLK_c=ord('c');
      SDLK_d=ord('d');
      SDLK_e=ord('e');
      SDLK_f=ord('f');
      SDLK_g=ord('g');
      SDLK_h=ord('h');
      SDLK_i=ord('i');
      SDLK_j=ord('j');
      SDLK_k=ord('k');
      SDLK_l=ord('l');
      SDLK_m=ord('m');
      SDLK_n=ord('n');
      SDLK_o=ord('o');
      SDLK_p=ord('p');
      SDLK_q=ord('q');
      SDLK_r=ord('r');
      SDLK_s=ord('s');
      SDLK_t=ord('t');
      SDLK_u=ord('u');
      SDLK_v=ord('v');
      SDLK_w=ord('w');
      SDLK_x=ord('x');
      SDLK_y=ord('y');
      SDLK_z=ord('z');

      SDL_SCANCODE_TO_KEYCODE=1 shl 30;

      SDLK_CAPSLOCK=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_CAPSLOCK);

      SDLK_F1=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F1);
      SDLK_F2=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F2);
      SDLK_F3=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F3);
      SDLK_F4=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F4);
      SDLK_F5=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F5);
      SDLK_F6=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F6);
      SDLK_F7=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F7);
      SDLK_F8=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F8);
      SDLK_F9=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F9);
      SDLK_F10=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F10);
      SDLK_F11=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F11);
      SDLK_F12=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F12);

      SDLK_PRINTSCREEN=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_PRINTSCREEN);
      SDLK_SCROLLLOCK=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_SCROLLLOCK);
      SDLK_SCROLLOCK=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_SCROLLLOCK);
      SDLK_PAUSE=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_PAUSE);
      SDLK_INSERT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_INSERT);
      SDLK_HOME=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_HOME);
      SDLK_PAGEUP=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_PAGEUP);
      SDLK_DELETE=127; //'\177';
      SDLK_END=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_END);
      SDLK_PAGEDOWN=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_PAGEDOWN);
      SDLK_RIGHT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_RIGHT);
      SDLK_LEFT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_LEFT);
      SDLK_DOWN=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_DOWN);
      SDLK_UP=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_UP);

      SDLK_NUMLOCK=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_NUMLOCKCLEAR);
      SDLK_NUMLOCKCLEAR=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_NUMLOCKCLEAR);
      SDLK_KP_DIVIDE=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_DIVIDE);
      SDLK_KP_MULTIPLY=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_MULTIPLY);
      SDLK_KP_MINUS=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_MINUS);
      SDLK_KP_PLUS=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_PLUS);
      SDLK_KP_ENTER=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_ENTER);
      SDLK_KP_1=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_1);
      SDLK_KP_2=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_2);
      SDLK_KP_3=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_3);
      SDLK_KP_4=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_4);
      SDLK_KP_5=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_5);
      SDLK_KP_6=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_6);
      SDLK_KP_7=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_7);
      SDLK_KP_8=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_8);
      SDLK_KP_9=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_9);
      SDLK_KP_0=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_0);
      SDLK_KP1=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_1);
      SDLK_KP2=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_2);
      SDLK_KP3=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_3);
      SDLK_KP4=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_4);
      SDLK_KP5=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_5);
      SDLK_KP6=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_6);
      SDLK_KP7=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_7);
      SDLK_KP8=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_8);
      SDLK_KP9=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_9);
      SDLK_KP0=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_0);
      SDLK_KP_PERIOD=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_PERIOD);

      SDLK_APPLICATION=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_APPLICATION);
      SDLK_POWER=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_POWER);
      SDLK_KP_EQUALS=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_EQUALS);
      SDLK_F13=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F13);
      SDLK_F14=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F14);
      SDLK_F15=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F15);
      SDLK_F16=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F16);
      SDLK_F17=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F17);
      SDLK_F18=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F18);
      SDLK_F19=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F19);
      SDLK_F20=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F20);
      SDLK_F21=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F21);
      SDLK_F22=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F22);
      SDLK_F23=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F23);
      SDLK_F24=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_F24);
      SDLK_EXECUTE=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_EXECUTE);
      SDLK_HELP=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_HELP);
      SDLK_MENU=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_MENU);
      SDLK_SELECT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_SELECT);
      SDLK_STOP=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_STOP);
      SDLK_AGAIN=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_AGAIN);
      SDLK_UNDO=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_UNDO);
      SDLK_CUT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_CUT);
      SDLK_COPY=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_COPY);
      SDLK_PASTE=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_PASTE);
      SDLK_FIND=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_FIND);
      SDLK_MUTE=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_MUTE);
      SDLK_VOLUMEUP=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_VOLUMEUP);
      SDLK_VOLUMEDOWN=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_VOLUMEDOWN);
      SDLK_KP_COMMA=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_COMMA);
      SDLK_KP_EQUALSAS400=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_EQUALSAS400);

      SDLK_ALTERASE=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_ALTERASE);
      SDLK_SYSREQ=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_SYSREQ);
      SDLK_CANCEL=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_CANCEL);
      SDLK_CLEAR=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_CLEAR);
      SDLK_PRIOR=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_PRIOR);
      SDLK_RETURN2=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_RETURN2);
      SDLK_SEPARATOR=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_SEPARATOR);
      SDLK_OUT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_OUT);
      SDLK_OPER=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_OPER);
      SDLK_CLEARAGAIN=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_CLEARAGAIN);
      SDLK_CRSEL=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_CRSEL);
      SDLK_EXSEL=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_EXSEL);

      SDLK_KP_00=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_00);
      SDLK_KP_000=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_000);
      SDLK_THOUSANDSSEPARATOR=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_THOUSANDSSEPARATOR);
      SDLK_DECIMALSEPARATOR=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_DECIMALSEPARATOR);
      SDLK_CURRENCYUNIT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_CURRENCYUNIT);
      SDLK_CURRENCYSUBUNIT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_CURRENCYSUBUNIT);
      SDLK_KP_LEFTPAREN=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_LEFTPAREN);
      SDLK_KP_RIGHTPAREN=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_RIGHTPAREN);
      SDLK_KP_LEFTBRACE=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_LEFTBRACE);
      SDLK_KP_RIGHTBRACE=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_RIGHTBRACE);
      SDLK_KP_TAB=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_TAB);
      SDLK_KP_BACKSPACE=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_BACKSPACE);
      SDLK_KP_A=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_A);
      SDLK_KP_B=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_B);
      SDLK_KP_C=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_C);
      SDLK_KP_D=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_D);
      SDLK_KP_E=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_E);
      SDLK_KP_F=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_F);
      SDLK_KP_XOR=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_XOR);
      SDLK_KP_POWER=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_POWER);
      SDLK_KP_PERCENT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_PERCENT);
      SDLK_KP_LESS=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_LESS);
      SDLK_KP_GREATER=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_GREATER);
      SDLK_KP_AMPERSAND=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_AMPERSAND);
      SDLK_KP_DBLAMPERSAND=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_DBLAMPERSAND);
      SDLK_KP_VERTICALBAR=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_VERTICALBAR);
      SDLK_KP_DBLVERTICALBAR=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_DBLVERTICALBAR);
      SDLK_KP_COLON=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_COLON);
      SDLK_KP_HASH=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_HASH);
      SDLK_KP_SPACE=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_SPACE);
      SDLK_KP_AT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_AT);
      SDLK_KP_EXCLAM=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_EXCLAM);
      SDLK_KP_MEMSTORE=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_MEMSTORE);
      SDLK_KP_MEMRECALL=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_MEMRECALL);
      SDLK_KP_MEMCLEAR=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_MEMCLEAR);
      SDLK_KP_MEMADD=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_MEMADD);
      SDLK_KP_MEMSUBTRACT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_MEMSUBTRACT);
      SDLK_KP_MEMMULTIPLY=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_MEMMULTIPLY);
      SDLK_KP_MEMDIVIDE=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_MEMDIVIDE);
      SDLK_KP_PLUSMINUS=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_PLUSMINUS);
      SDLK_KP_CLEAR=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_CLEAR);
      SDLK_KP_CLEARENTRY=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_CLEARENTRY);
      SDLK_KP_BINARY=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_BINARY);
      SDLK_KP_OCTAL=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_OCTAL);
      SDLK_KP_DECIMAL=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_DECIMAL);
      SDLK_KP_HEXADECIMAL=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KP_HEXADECIMAL);

      SDLK_LCTRL=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_LCTRL);
      SDLK_LSHIFT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_LSHIFT);
      SDLK_LALT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_LALT);
      SDLK_LGUI=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_LGUI);
      SDLK_RCTRL=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_RCTRL);
      SDLK_RSHIFT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_RSHIFT);
      SDLK_RALT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_RALT);
      SDLK_RGUI=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_RGUI);

      SDLK_MODE=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_MODE);

      SDLK_AUDIONEXT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_AUDIONEXT);
      SDLK_AUDIOPREV=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_AUDIOPREV);
      SDLK_AUDIOSTOP=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_AUDIOSTOP);
      SDLK_AUDIOPLAY=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_AUDIOPLAY);
      SDLK_AUDIOMUTE=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_AUDIOMUTE);
      SDLK_MEDIASELECT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_MEDIASELECT);
      SDLK_WWW=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_WWW);
      SDLK_MAIL=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_MAIL);
      SDLK_CALCULATOR=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_CALCULATOR);
      SDLK_COMPUTER=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_COMPUTER);
      SDLK_AC_SEARCH=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_AC_SEARCH);
      SDLK_AC_HOME=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_AC_HOME);
      SDLK_AC_BACK=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_AC_BACK);
      SDLK_AC_FORWARD=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_AC_FORWARD);
      SDLK_AC_STOP=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_AC_STOP);
      SDLK_AC_REFRESH=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_AC_REFRESH);
      SDLK_AC_BOOKMARKS=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_AC_BOOKMARKS);

      SDLK_BRIGHTNESSDOWN=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_BRIGHTNESSDOWN);
      SDLK_BRIGHTNESSUP=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_BRIGHTNESSUP);
      SDLK_DISPLAYSWITCH=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_DISPLAYSWITCH);
      SDLK_KBDILLUMTOGGLE=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KBDILLUMTOGGLE);
      SDLK_KBDILLUMDOWN=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KBDILLUMDOWN);
      SDLK_KBDILLUMUP=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_KBDILLUMUP);
      SDLK_EJECT=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_EJECT);
      SDLK_SLEEP=SDL_SCANCODE_TO_KEYCODE or (SDL_SCANCODE_SLEEP);

      SDL_BUTTON_LEFT=1;
      SDL_BUTTON_MIDDLE=2;
      SDL_BUTTON_RIGHT=3;
      SDL_BUTTON_X1=6;
      SDL_BUTTON_X2=7;

      SDL_QUERY=-1;
      SDL_IGNORE=0;
      SDL_DISABLE=0;
      SDL_ENABLE=1;

      SDL_AUDIO_MASK_BITSIZE=$ff;
      SDL_AUDIO_MASK_DATATYPE=1 shl 8;
      SDL_AUDIO_MASK_ENDIAN=1 shl 12;
      SDL_AUDIO_MASK_SIGNED=1 shl 15;

      AUDIO_U8=$0008; // Unsigned 8-bit samples
      AUDIO_S8=$8008; // Signed 8-bit samples
      AUDIO_U16LSB=$0010; // Unsigned 16-bit samples
      AUDIO_S16LSB=$8010; // Signed 16-bit samples
      AUDIO_S32LSB=$8020;
      AUDIO_F32LSB=$8120;
      AUDIO_U16MSB=$1010; // As above, but big-endian uint8 order
      AUDIO_S16MSB=$9010; // As above, but big-endian uint8 order
      AUDIO_S32MSB=$9020;
      AUDIO_F32MSB=$9120;
      AUDIO_U16=AUDIO_U16LSB;
      AUDIO_S16=AUDIO_S16LSB;
      AUDIO_S32=AUDIO_S32LSB;
      AUDIO_F32=AUDIO_F32LSB;

      SDL_AUDIO_ALLOW_FREQUENCY_CHANGE=$00000001;
      SDL_AUDIO_ALLOW_FORMAT_CHANGE=$00000002;
      SDL_AUDIO_ALLOW_CHANNELS_CHANGE=$00000004;
      SDL_AUDIO_ALLOW_ANY_CHANGE=SDL_AUDIO_ALLOW_FREQUENCY_CHANGE or SDL_AUDIO_ALLOW_FORMAT_CHANGE or SDL_AUDIO_ALLOW_CHANNELS_CHANGE;

      SDL_SYSWM_UNKNOWN=0;
      SDL_SYSWM_WINDOWS=1;
      SDL_SYSWM_X11=2;
      SDL_SYSWM_DIRECTFB=3;
      SDL_SYSWM_COCOA=4;
      SDL_SYSWM_UIKIT=5;
      SDL_SYSWM_WAYLAND=6;
      SDL_SYSWM_MIR=7;
      SDL_SYSWM_WINRT=8;
      SDL_SYSWM_ANDROID=9;
      SDL_SYSWM_VIVANTE=10;

      SDL_ALPHA_OPAQUE = 255;
      SDL_ALPHA_TRANSPARENT = 0;

      {** Pixel type. *}
      SDL_PIXELTYPE_UNKNOWN = 0;
      SDL_PIXELTYPE_INDEX1 = 1;
      SDL_PIXELTYPE_INDEX4 = 2;
      SDL_PIXELTYPE_INDEX8 = 3;
      SDL_PIXELTYPE_PACKED8 = 4;
      SDL_PIXELTYPE_PACKED16 = 5;
      SDL_PIXELTYPE_PACKED32 = 6;
      SDL_PIXELTYPE_ARRAYU8 = 7;
      SDL_PIXELTYPE_ARRAYU16 = 8;
      SDL_PIXELTYPE_ARRAYU32 = 9;
      SDL_PIXELTYPE_ARRAYF16 = 10;
      SDL_PIXELTYPE_ARRAYF32 = 11;

      {** Bitmap pixel order, high bit -> low bit. *}
      SDL_BITMAPORDER_NONE = 0;
      SDL_BITMAPORDER_4321 = 1;
      SDL_BITMAPORDER_1234 = 2;

      {** Packed component order, high bit -> low bit. *}

      SDL_PACKEDORDER_NONE = 0;
      SDL_PACKEDORDER_XRGB = 1;
      SDL_PACKEDORDER_RGBX = 2;
      SDL_PACKEDORDER_ARGB = 3;
      SDL_PACKEDORDER_RGBA = 4;
      SDL_PACKEDORDER_XBGR = 5;
      SDL_PACKEDORDER_BGRX = 6;
      SDL_PACKEDORDER_ABGR = 7;
      SDL_PACKEDORDER_BGRA = 8;

      {** Array component order, low byte -> high byte. *}
      SDL_ARRAYORDER_NONE = 0;
      SDL_ARRAYORDER_RGB  = 1;
      SDL_ARRAYORDER_RGBA = 2;
      SDL_ARRAYORDER_ARGB = 3;
      SDL_ARRAYORDER_BGR  = 4;
      SDL_ARRAYORDER_BGRA = 5;
      SDL_ARRAYORDER_ABGR = 6;

      {** Packed component layout. *}
      SDL_PACKEDLAYOUT_NONE = 0;
      SDL_PACKEDLAYOUT_332  = 1;
      SDL_PACKEDLAYOUT_4444 = 2;
      SDL_PACKEDLAYOUT_1555 = 3;
      SDL_PACKEDLAYOUT_5551 = 4;
      SDL_PACKEDLAYOUT_565  = 5;
      SDL_PACKEDLAYOUT_8888 = 6;
      SDL_PACKEDLAYOUT_2101010 = 7;
      SDL_PACKEDLAYOUT_1010102 = 8;

      SDL_PIXELFORMAT_UNKNOWN=0;
      SDL_PIXELFORMAT_INDEX1LSB=(1 shl 28) or (SDL_PIXELTYPE_INDEX1 shl 24) or (SDL_BITMAPORDER_4321 shl 20) or (0 shl 16) or (1 shl 8) or (0 shl 0);
      SDL_PIXELFORMAT_INDEX1MSB=(1 shl 28) or (SDL_PIXELTYPE_INDEX1 shl 24) or (SDL_BITMAPORDER_1234 shl 20) or (0 shl 16) or (1 shl 8) or (0 shl 0);
      SDL_PIXELFORMAT_INDEX4LSB=(1 shl 28) or (SDL_PIXELTYPE_INDEX4 shl 24) or (SDL_BITMAPORDER_4321 shl 20) or (0 shl 16) or (4 shl 8) or (0 shl 0);
      SDL_PIXELFORMAT_INDEX4MSB=(1 shl 28) or (SDL_PIXELTYPE_INDEX4 shl 24) or (SDL_BITMAPORDER_1234 shl 20) or (0 shl 16) or (4 shl 8) or (0 shl 0);
      SDL_PIXELFORMAT_INDEX8=(1 shl 28) or (SDL_PIXELTYPE_PACKED8 shl 24) or (0 shl 20) or (0 shl 16) or (8 shl 8) or (1 shl 0);
      SDL_PIXELFORMAT_RGB332=(1 shl 28) or (SDL_PIXELTYPE_PACKED8 shl 24) or (SDL_PACKEDORDER_XRGB shl 20) or (SDL_PACKEDLAYOUT_332 shl 16) or (8 shl 8) or (1 shl 0);
      SDL_PIXELFORMAT_RGB444=(1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_XRGB shl 20) or (SDL_PACKEDLAYOUT_4444 shl 16) or (12 shl 8) or (2 shl 0);
      SDL_PIXELFORMAT_RGB555=(1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_XRGB shl 20) or (SDL_PACKEDLAYOUT_1555 shl 16) or (15 shl 8) or (2 shl 0);
      SDL_PIXELFORMAT_BGR555=(1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_XBGR shl 20) or (SDL_PACKEDLAYOUT_1555 shl 16) or (15 shl 8) or (2 shl 0);
      SDL_PIXELFORMAT_ARGB4444=(1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_ARGB shl 20) or (SDL_PACKEDLAYOUT_4444 shl 16) or (16 shl 8) or (2 shl 0);
      SDL_PIXELFORMAT_RGBA4444=(1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_RGBA shl 20) or (SDL_PACKEDLAYOUT_4444 shl 16) or (16 shl 8) or (2 shl 0);
      SDL_PIXELFORMAT_ABGR4444=(1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_ABGR shl 20) or (SDL_PACKEDLAYOUT_4444 shl 16) or (16 shl 8) or (2 shl 0);
      SDL_PIXELFORMAT_BGRA4444=(1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_BGRA shl 20) or (SDL_PACKEDLAYOUT_4444 shl 16) or (16 shl 8) or (2 shl 0);
      SDL_PIXELFORMAT_ARGB1555=(1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_ARGB shl 20) or (SDL_PACKEDLAYOUT_1555 shl 16) or (16 shl 8) or (2 shl 0);
      SDL_PIXELFORMAT_RGBA5551=(1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_RGBA shl 20) or (SDL_PACKEDLAYOUT_5551 shl 16) or (16 shl 8) or (2 shl 0);
      SDL_PIXELFORMAT_ABGR1555=(1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_ABGR shl 20) or (SDL_PACKEDLAYOUT_1555 shl 16) or (16 shl 8) or (2 shl 0);
      SDL_PIXELFORMAT_BGRA5551=(1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_BGRA shl 20) or (SDL_PACKEDLAYOUT_5551 shl 16) or (16 shl 8) or (2 shl 0);
      SDL_PIXELFORMAT_RGB565=(1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_XRGB shl 20) or (SDL_PACKEDLAYOUT_565 shl 16) or (16 shl 8) or (2 shl 0);
      SDL_PIXELFORMAT_BGR565=(1 shl 28) or (SDL_PIXELTYPE_PACKED16 shl 24) or (SDL_PACKEDORDER_XBGR shl 20) or (SDL_PACKEDLAYOUT_1555 shl 16) or (16 shl 8) or (2 shl 0);
      SDL_PIXELFORMAT_RGB24=(1 shl 28) or (SDL_PIXELTYPE_ARRAYU8 shl 24) or (SDL_ARRAYORDER_RGB shl 20) or (0 shl 16) or (24 shl 8) or (3 shl 0);
      SDL_PIXELFORMAT_BGR24=(1 shl 28) or (SDL_PIXELTYPE_ARRAYU8 shl 24) or (SDL_ARRAYORDER_BGR shl 20) or (0 shl 16) or (24 shl 8) or (3 shl 0);
      SDL_PIXELFORMAT_RGB888=(1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_XRGB shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (24 shl 8) or (4 shl 0);
      SDL_PIXELFORMAT_RGBX8888=(1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_RGBX shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (24 shl 8) or (4 shl 0);
      SDL_PIXELFORMAT_BGR888=(1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_XBGR shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (24 shl 8) or (4 shl 0);
      SDL_PIXELFORMAT_BGRX8888=(1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_BGRX shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (24 shl 8) or (4 shl 0);
      SDL_PIXELFORMAT_ARGB8888=(1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_ARGB shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (32 shl 8) or (4 shl 0);
      SDL_PIXELFORMAT_RGBA8888=(1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_RGBA shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (32 shl 8) or (4 shl 0);
      SDL_PIXELFORMAT_ABGR8888=(1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_ABGR shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (32 shl 8) or (4 shl 0);
      SDL_PIXELFORMAT_BGRA8888=(1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_RGBX shl 20) or (SDL_PACKEDLAYOUT_8888 shl 16) or (32 shl 8) or (4 shl 0);
      SDL_PIXELFORMAT_ARGB2101010=(1 shl 28) or (SDL_PIXELTYPE_PACKED32 shl 24) or (SDL_PACKEDORDER_ARGB shl 20) or (SDL_PACKEDLAYOUT_2101010 shl 16) or (32 shl 8) or (4 shl 0);
      SDL_PIXELFORMAT_YV12=(ord('Y') shl 0) or (ord('V') shl 8) or (ord('1') shl 16) or (ord('2') shl 24);
      SDL_PIXELFORMAT_IYUV=(ord('I') shl 0) or (ord('Y') shl 8) or (ord('U') shl 16) or (ord('V') shl 24);
      SDL_PIXELFORMAT_YUY2=(ord('Y') shl 0) or (ord('U') shl 8) or (ord('Y') shl 16) or (ord('2') shl 24);
      SDL_PIXELFORMAT_UYVY=(ord('U') shl 0) or (ord('Y') shl 8) or (ord('U') shl 16) or (ord('V') shl 24);
      SDL_PIXELFORMAT_NV12=(ord('N') shl 0) or (ord('V') shl 8) or (ord('1') shl 16) or (ord('2') shl 24);
      SDL_PIXELFORMAT_NV21=(ord('N') shl 0) or (ord('V') shl 8) or (ord('2') shl 16) or (ord('1') shl 24);
{$ifdef fpc_big_endian}
      SDL_PIXELFORMAT_RGBA32=SDL_PIXELFORMAT_RGBA8888;
      SDL_PIXELFORMAT_ARGB32=SDL_PIXELFORMAT_ARGB8888;
      SDL_PIXELFORMAT_BGRA32=SDL_PIXELFORMAT_BGRA8888;
      SDL_PIXELFORMAT_ABGR32=SDL_PIXELFORMAT_ABGR8888;
{$else}
      SDL_PIXELFORMAT_RGBA32=SDL_PIXELFORMAT_ABGR8888;
      SDL_PIXELFORMAT_ARGB32=SDL_PIXELFORMAT_BGRA8888;
      SDL_PIXELFORMAT_BGRA32=SDL_PIXELFORMAT_ARGB8888;
      SDL_PIXELFORMAT_ABGR32=SDL_PIXELFORMAT_RGBA8888;
{$endif}

      SDL_TEXTUREACCESS_STATIC=0;
      SDL_TEXTUREACCESS_STREAMING=1;
      SDL_TEXTUREACCESS_TARGET=2;

      // SDL_GLprofile (enum)
      SDL_GL_CONTEXT_PROFILE_CORE=1;
      SDL_GL_CONTEXT_PROFILE_COMPATIBILITY=2;
      SDL_GL_CONTEXT_PROFILE_ES=4;

      // SDL_GLcontextFlag (enum)
      SDL_GL_CONTEXT_DEBUG_FLAG=1;
      SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG=2;
      SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG=4;
      SDL_GL_CONTEXT_RESET_ISOLATION_FLAG=8;

      SDL_CONTROLLER_BINDTYPE_NONE=0;
      SDL_CONTROLLER_BINDTYPE_BUTTON=1;
      SDL_CONTROLLER_BINDTYPE_AXIS=2;
      SDL_CONTROLLER_BINDTYPE_HAT=3;

      SDL_CONTROLLER_AXIS_INVALID=-1;
      SDL_CONTROLLER_AXIS_LEFTX=0;
      SDL_CONTROLLER_AXIS_LEFTY=1;
      SDL_CONTROLLER_AXIS_RIGHTX=2;
      SDL_CONTROLLER_AXIS_RIGHTY=3;
      SDL_CONTROLLER_AXIS_TRIGGERLEFT=4;
      SDL_CONTROLLER_AXIS_TRIGGERRIGHT=5;
      SDL_CONTROLLER_AXIS_MAX=6;

      SDL_CONTROLLER_BUTTON_INVALID=-1;
      SDL_CONTROLLER_BUTTON_A=0;
      SDL_CONTROLLER_BUTTON_B=1;
      SDL_CONTROLLER_BUTTON_X=2;
      SDL_CONTROLLER_BUTTON_Y=3;
      SDL_CONTROLLER_BUTTON_BACK=4;
      SDL_CONTROLLER_BUTTON_GUIDE=5;
      SDL_CONTROLLER_BUTTON_START=6;
      SDL_CONTROLLER_BUTTON_LEFTSTICK=7;
      SDL_CONTROLLER_BUTTON_RIGHTSTICK=8;
      SDL_CONTROLLER_BUTTON_LEFTSHOULDER=9;
      SDL_CONTROLLER_BUTTON_RIGHTSHOULDER=10;
      SDL_CONTROLLER_BUTTON_DPAD_UP=11;
      SDL_CONTROLLER_BUTTON_DPAD_DOWN=12;
      SDL_CONTROLLER_BUTTON_DPAD_LEFT=13;
      SDL_CONTROLLER_BUTTON_DPAD_RIGHT=14;
      SDL_CONTROLLER_BUTTON_MAX=15;

      SDL_LOG_CATEGORY_APPLICATION=0;
      SDL_LOG_CATEGORY_ERROR=1;
      SDL_LOG_CATEGORY_ASSERT=2;
      SDL_LOG_CATEGORY_SYSTEM=3;
      SDL_LOG_CATEGORY_AUDIO=4;
      SDL_LOG_CATEGORY_VIDEO=5;
      SDL_LOG_CATEGORY_RENDER=6;
      SDL_LOG_CATEGORY_INPUT=7;
      SDL_LOG_CATEGORY_TEST=8;
      SDL_LOG_CATEGORY_RESERVED1=9;
      SDL_LOG_CATEGORY_RESERVED2=10;
      SDL_LOG_CATEGORY_RESERVED3=11;
      SDL_LOG_CATEGORY_RESERVED4=12;
      SDL_LOG_CATEGORY_RESERVED5=13;
      SDL_LOG_CATEGORY_RESERVED6=14;
      SDL_LOG_CATEGORY_RESERVED7=15;
      SDL_LOG_CATEGORY_RESERVED8=16;
      SDL_LOG_CATEGORY_RESERVED9=17;
      SDL_LOG_CATEGORY_RESERVED10=18;
      SDL_LOG_CATEGORY_CUSTOM=19;

      SDL_HINT_FRAMEBUFFER_ACCELERATION='SDL_FRAMEBUFFER_ACCELERATION';
      SDL_HINT_RENDER_DRIVER='SDL_RENDER_DRIVER';
      SDL_HINT_RENDER_OPENGL_SHADERS='SDL_RENDER_OPENGL_SHADERS';
      SDL_HINT_RENDER_DIRECT3D_THREADSAFE='SDL_RENDER_DIRECT3D_THREADSAFE';
      SDL_HINT_RENDER_DIRECT3D11_DEBUG='SDL_RENDER_DIRECT3D11_DEBUG';
      SDL_HINT_RENDER_SCALE_QUALITY='SDL_RENDER_SCALE_QUALITY';
      SDL_HINT_RENDER_VSYNC='SDL_RENDER_VSYNC';
      SDL_HINT_VIDEO_ALLOW_SCREENSAVER='SDL_VIDEO_ALLOW_SCREENSAVER';
      SDL_HINT_VIDEO_X11_XVIDMODE='SDL_VIDEO_X11_XVIDMODE';
      SDL_HINT_VIDEO_X11_XINERAMA='SDL_VIDEO_X11_XINERAMA';
      SDL_HINT_VIDEO_X11_XRANDR='SDL_VIDEO_X11_XRANDR';
      SDL_HINT_VIDEO_X11_NET_WM_PING='SDL_VIDEO_X11_NET_WM_PING';
      SDL_HINT_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN='SDL_WINDOW_FRAME_USABLE_WHILE_CURSOR_HIDDEN';
      SDL_HINT_WINDOWS_ENABLE_MESSAGELOOP='SDL_WINDOWS_ENABLE_MESSAGELOOP';
      SDL_HINT_GRAB_KEYBOARD='SDL_GRAB_KEYBOARD';
      SDL_HINT_MOUSE_RELATIVE_MODE_WARP='SDL_MOUSE_RELATIVE_MODE_WARP';
      SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH='SDL_MOUSE_FOCUS_CLICKTHROUGH';
      SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS='SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS';
      SDL_HINT_IDLE_TIMER_DISABLED='SDL_IOS_IDLE_TIMER_DISABLED';
      SDL_HINT_ORIENTATIONS='SDL_IOS_ORIENTATIONS';
      SDL_HINT_APPLE_TV_CONTROLLER_UI_EVENTS='SDL_APPLE_TV_CONTROLLER_UI_EVENTS';
      SDL_HINT_APPLE_TV_REMOTE_ALLOW_ROTATION='SDL_APPLE_TV_REMOTE_ALLOW_ROTATION';
      SDL_HINT_ACCELEROMETER_AS_JOYSTICK='SDL_ACCELEROMETER_AS_JOYSTICK';
      SDL_HINT_XINPUT_ENABLED='SDL_XINPUT_ENABLED';
      SDL_HINT_XINPUT_USE_OLD_JOYSTICK_MAPPING='SDL_XINPUT_USE_OLD_JOYSTICK_MAPPING';
      SDL_HINT_GAMECONTROLLERCONFIG='SDL_GAMECONTROLLERCONFIG';
      SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS='SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS';
      SDL_HINT_ALLOW_TOPMOST='SDL_ALLOW_TOPMOST';
      SDL_HINT_TIMER_RESOLUTION='SDL_TIMER_RESOLUTION';
      SDL_HINT_THREAD_STACK_SIZE='SDL_THREAD_STACK_SIZE';
      SDL_HINT_VIDEO_HIGHDPI_DISABLED='SDL_VIDEO_HIGHDPI_DISABLED';
      SDL_HINT_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK='SDL_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK';
      SDL_HINT_VIDEO_WIN_D3DCOMPILER='SDL_VIDEO_WIN_D3DCOMPILER';
      SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT='SDL_VIDEO_WINDOW_SHARE_PIXEL_FORMAT';
      SDL_HINT_WINRT_PRIVACY_POLICY_URL='SDL_WINRT_PRIVACY_POLICY_URL';
      SDL_HINT_WINRT_PRIVACY_POLICY_LABEL='SDL_WINRT_PRIVACY_POLICY_LABEL';
      SDL_HINT_WINRT_HANDLE_BACK_BUTTON='SDL_WINRT_HANDLE_BACK_BUTTON';
      SDL_HINT_VIDEO_MAC_FULLSCREEN_SPACES='SDL_VIDEO_MAC_FULLSCREEN_SPACES';
      SDL_HINT_MAC_BACKGROUND_APP='SDL_MAC_BACKGROUND_APP';
      SDL_HINT_ANDROID_APK_EXPANSION_MAIN_FILE_VERSION='SDL_ANDROID_APK_EXPANSION_MAIN_FILE_VERSION';
      SDL_HINT_ANDROID_APK_EXPANSION_PATCH_FILE_VERSION='SDL_ANDROID_APK_EXPANSION_PATCH_FILE_VERSION';
      SDL_HINT_IME_INTERNAL_EDITING='SDL_IME_INTERNAL_EDITING';
      SDL_HINT_ANDROID_SEPARATE_MOUSE_AND_TOUCH='SDL_ANDROID_SEPARATE_MOUSE_AND_TOUCH';
      SDL_HINT_MOUSE_TOUCH_EVENTS='SDL_MOUSE_TOUCH_EVENTS';
      SDL_HINT_TOUCH_MOUSE_EVENTS='SDL_TOUCH_MOUSE_EVENTS';
      SDL_HINT_ANDROID_BLOCK_ON_PAUSE='SDL_ANDROID_BLOCK_ON_PAUSE';
      SDL_HINT_ANDROID_TRAP_BACK_BUTTON='SDL_ANDROID_TRAP_BACK_BUTTON';
      SDL_HINT_EMSCRIPTEN_KEYBOARD_ELEMENT='SDL_EMSCRIPTEN_KEYBOARD_ELEMENT';
      SDL_HINT_NO_SIGNAL_HANDLERS='SDL_NO_SIGNAL_HANDLERS';
      SDL_HINT_WINDOWS_NO_CLOSE_ON_ALT_F4='SDL_WINDOWS_NO_CLOSE_ON_ALT_F4';
      SDL_HINT_BMP_SAVE_LEGACY_FORMAT='SDL_BMP_SAVE_LEGACY_FORMAT';
      SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING='SDL_WINDOWS_DISABLE_THREAD_NAMING';
      SDL_HINT_RPI_VIDEO_LAYER='SDL_RPI_VIDEO_LAYER';
      SDL_HINT_AUDIO_RESAMPLING_MODE='SDL_AUDIO_RESAMPLING_MODE';
      SDL_HINT_WINRT_REMEMBER_WINDOW_FULLSCREEN_PREFERENCE='SDL_WINRT_REMEMBER_WINDOW_FULLSCREEN_PREFERENCE';
      SDL_HINT_ANDROID_HIDE_SYSTEM_BARS='SDL_ANDROID_HIDE_SYSTEM_BARS';
      SDL_HINT_MOUSE_NORMAL_SPEED_SCALE='SDL_MOUSE_NORMAL_SPEED_SCALE';
      SDL_HINT_MOUSE_RELATIVE_SPEED_SCALE='SDL_MOUSE_RELATIVE_SPEED_SCALE';

      SDL_MESSAGEBOX_ERROR=$00000010;   //**< error dialog */
      SDL_MESSAGEBOX_WARNING=$00000020;   //**< warning dialog */
      SDL_MESSAGEBOX_INFORMATION=$00000040;   //**< informational dialog */

      SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT=$00000001;
      SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT=$00000002;

      SDL_MESSAGEBOX_COLOR_BACKGROUND=0;
      SDL_MESSAGEBOX_COLOR_TEXT=1;
      SDL_MESSAGEBOX_COLOR_BUTTON_BORDER=2;
      SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND=3;
      SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED=4;

      SDL_RWOPS_UNKNOWN=0;
      SDL_RWOPS_WINFILE=1;
      SDL_RWOPS_STDFILE=2;
      SDL_RWOPS_JNIFILE=3;
      SDL_RWOPS_MEMORY=4;
      SDL_RWOPS_MEMORY_RO=5;
      
type PSDLInt8=^TSDLInt8;
     TSDLInt8={$ifdef fpc}Int8{$else}ShortInt{$endif};

     PSDLUInt8=^TSDLUInt8;
     TSDLUInt8={$ifdef fpc}UInt8{$else}Byte{$endif};

     PSDLInt16=^TSDLInt16;
     TSDLInt16={$ifdef fpc}Int16{$else}SmallInt{$endif};

     PSDLUInt16=^TSDLUInt16;
     TSDLUInt16={$ifdef fpc}UInt16{$else}Word{$endif};

     PSDLInt32=^TSDLInt32;
     TSDLInt32={$ifdef fpc}Int32{$else}LongInt{$endif};

     PSDLUInt32=^TSDLUInt32;
     TSDLUInt32={$ifdef fpc}UInt32{$else}LongWord{$endif};

     PSDLSizeInt=^TSDLSizeInt;
     TSDLSizeInt={$ifdef fpc}SizeInt{$else}NativeInt{$endif};

     PSDLSizeUInt=^TSDLSizeUInt;
     TSDLSizeUInt={$ifdef fpc}SizeUInt{$else}NativeUInt{$endif};

     PSDLInt64=^TSDLInt64;
     TSDLInt64=Int64;

     PSDLUInt64=^TSDLUInt64;
     TSDLUInt64=UInt64;

     PSDLFloat=^TSDLFloat;
     TSDLFloat=Single;

     PSDLDouble=^TSDLDouble;
     TSDLDouble=Double;

     PSDL_Window=pointer;
     PSDL_Renderer=pointer;
     PSDL_Texture=pointer;
     PSDL_GLContext=pointer;

     PSDL_HintPriority=^TSDL_HintPriority;
     TSDL_HintPriority=
      (
       SDL_HINT_DEFAULT,
       SDL_HINT_NORMAL,
       SDL_HINT_OVERRIDE
      );

     PSDL_GameControllerBindType=^TSDL_GameControllerBindType;
     TSDL_GameControllerBindType=TSDLInt32;

     PSDL_GameControllerBind=^TSDL_GameControllerBind;
     TSDL_GameControllerBind=record
      case BindType:TSDL_GameControllerBindType of
       SDL_CONTROLLER_BINDTYPE_BUTTON:(
        Button:TSDLInt32;
       );
       SDL_CONTROLLER_BINDTYPE_AXIS:(
        Axis:TSDLInt32;
       );
       SDL_CONTROLLER_BINDTYPE_HAT:(
        Hat:TSDLInt32;
        HatMask:TSDLInt32;
       );
     end;

     PSDL_GameControllerAxis=^TSDL_GameControllerAxis;
     TSDL_GameControllerAxis=TSDLInt32;

     PSDL_GameControllerButton=^TSDL_GameControllerButton;
     TSDL_GameControllerButton=TSDLInt32;

     PSDL_AudioDeviceID=^TSDL_AudioDeviceID;
     TSDL_AudioDeviceID=TSDLUInt32;

     PSDL_BlendMode=^TSDL_BlendMode;
     TSDL_BlendMode=TSDLUInt32;

     PSDL_DisplayEventID=^TSDL_DisplayEventID;
     TSDL_DisplayEventID=TSDLUInt32;

     PSDL_DisplayOrientation=^TSDL_DisplayOrientation;
     TSDL_DisplayOrientation=TSDLUInt32;

     PSDL_RendererFlip=^TSDL_RendererFlip;
     TSDL_RendererFlip=
      (
       SDL_FLIP_NONE=0,
       SDL_FLIP_HORIZONTAL=1,
       SDL_FLIP_VERTICAL=2
      );

     PSDL_RendererInfo=^TSDL_RendererInfo;
     TSDL_RendererInfo=record
      name:PAnsiChar;
      flags:TSDLUInt32;
      num_texture_formats:TSDLUInt32;
      texture_formats:array[0..15] of TSDLUInt32;
      max_texture_width:TSDLInt32;
      max_texture_height:TSDLInt32;
     end;

     PSDL_AudioStatus=^TSDL_AudioStatus;
     TSDL_AudioStatus=(
      SDL_AUDIO_STOPPED=0,
      SDL_AUDIO_PLAYING=1,
      SDL_AUDIO_PAUSED=2
     );

     PSDL_AudioFormat=^TSDL_AudioFormat;
     TSDL_AudioFormat=TSDLUInt16;

     TSDL_AudioSpecCallback=procedure(userdata:pointer;stream:PSDLUInt8;len:TSDLInt32); cdecl;

     PSDL_AudioSpec=^TSDL_AudioSpec;
     TSDL_AudioSpec=record
      freq:TSDLInt32; // DSP frequency -- samples per second
      format:TSDL_AudioFormat; // Audio data format
      channels:TSDLUInt8; // Number of channels:1 mono, 2 stereo
      silence:TSDLUInt8; // Audio buffer silence value (calculated)
      samples:TSDLUInt16; // Audio buffer size in samples
      padding:TSDLUInt16; // Necessary for some compile environments
      size:TSDLUInt32; // Audio buffer size in bytes (calculated)
      callback:TSDL_AudioSpecCallback;
      userdata:Pointer;
     end;

     PSDL_AudioCVT=^TSDL_AudioCVT;

     TSDL_AudioFilter=procedure(cvt:PSDL_AudioCVT;format:TSDL_AudioFormat); cdecl;

     TSDL_AudioCVT=record
      needed:TSDLInt32;
      src_format:TSDL_AudioFormat;
      dst_format:TSDL_AudioFormat;
      rate_incr:TSDLDouble;
      Buf:PSDLUInt8;
      len:TSDLInt32;
      len_cvt:TSDLInt32;
      len_mult:TSDLInt32;
      len_ratio:TSDLDouble;
      filters:array[0..9] of TSDL_AudioFilter;
      filter_index:TSDLInt32;
     end;

     PSDL_Rect=^TSDL_Rect;
     TSDL_Rect=record
      x,y,w,h:TSDLInt32;
     end;

     PSDL_Point=^TSDL_Point;
     TSDL_Point=record
      X,Y:TSDLInt32;
     end;

     PSDL_Color=^TSDL_Color;
     TSDL_Color=record
      case TSDLUInt8 of
       0:(
        r:TSDLUInt8;
        g:TSDLUInt8;
        b:TSDLUInt8;
        a:TSDLUInt8;
       );
       1:(
        r_:TSDLUInt8;
        g_:TSDLUInt8;
        b_:TSDLUInt8;
        unused:TSDLUInt8;
       );
       2:(
        value:TSDLUInt32;
       );
     end;

     PSDL_Colour=^TSDL_Colour;
     TSDL_Colour=TSDL_Color;

     PSDL_Palette=^TSDL_Palette;
     TSDL_Palette=record
      ncolors:TSDLInt32;
      colors:PSDL_Color;
      version:TSDLUInt32;
      refcount:TSDLInt32;
     end;

     PSDL_PixelFormat=^TSDL_PixelFormat;
     TSDL_PixelFormat=record
      format:TSDLUInt32;
      palette:PSDL_Palette;
      BitsPerPixel:TSDLUInt8;
      BytesPerPixel:TSDLUInt8;
      padding:array[0..1] of TSDLUInt8;
      RMask:TSDLUInt32;
      GMask:TSDLUInt32;
      BMask:TSDLUInt32;
      AMask:TSDLUInt32;
      Rloss:TSDLUInt8;
      Gloss:TSDLUInt8;
      Bloss:TSDLUInt8;
      Aloss:TSDLUInt8;
      Rshift:TSDLUInt8;
      Gshift:TSDLUInt8;
      Bshift:TSDLUInt8;
      Ashift:TSDLUInt8;
      refcount:TSDLInt32;
      next:PSDL_PixelFormat;
     end;

     SDL_eventaction=(SDL_ADDEVENT=0,SDL_PEEPEVENT,SDL_GETEVENT);

     PSDL_Surface=^TSDL_Surface;
     TSDL_Surface=record
      flags:TSDLUInt32;
      format:PSDL_PixelFormat;
      w,h:TSDLInt32;
      pitch:TSDLInt32;
      pixels:Pointer;
      offset:TSDLInt32;
      userdata:Pointer;
      locked:TSDLInt32;
      lock_data:Pointer;
      clip_rect:TSDL_Rect;
      map:Pointer;
      refcount:TSDLInt32;
     end;

     PSDL_RWops=^TSDL_RWops;

     TSeek=function(context:PSDL_RWops;offset:TSDLInt32;whence:TSDLInt32):TSDLInt32; cdecl;
     TRead=function(context:PSDL_RWops;Ptr:Pointer;size:TSDLInt32;maxnum:TSDLInt32):TSDLInt32;  cdecl;
     TWrite=function(context:PSDL_RWops;Ptr:Pointer;size:TSDLInt32;num:TSDLInt32):TSDLInt32; cdecl;
     TClose=function(context:PSDL_RWops):TSDLInt32; cdecl;

     TStdio=record
      autoclose:boolean;
      fp:pointer;
     end;

     TMem=record
      base:PSDLUInt8;
      here:PSDLUInt8;
      stop:PSDLUInt8;
     end;

     TUnknown=record
      data1:pointer;
     end;

     TSDL_RWops=record
      seek:TSeek;
      read:TRead;
      write:TWrite;
      close:TClose;
      type_:TSDLUInt32;
      case TSDLUInt8 of
       0:(stdio:TStdio);
       1:(mem:TMem);
       2:(unknown:TUnknown);
       3:(
        AndroidIO:record
         fileNameRef:Pointer;
         inputStreamRef:Pointer;
         readableByteChannelRef:Pointer;
         readMethod:Pointer;
         assetFileDescriptorRef:Pointer;
         position:TSDLInt32;
         size:TSDLInt32;
         offset:TSDLInt32;
         fd:TSDLInt32;
        end;
       );
       4:(
        WindowsIO:record
         Append:TSDLUInt8;
         h:Pointer;
         Buffer:record
          Data:Pointer;
          Size:TSDLSizeUInt;
          Left:TSDLSizeUInt;
         end;
        end;
       );
     end;

     PSDL_version=^TSDL_version;
     TSDL_version=record
      major:TSDLUInt8;
      minor:TSDLUInt8;
      patch:TSDLUInt8;
     end;

     TSDL_SysWm=TSDLInt32;

{$if defined(Windows)}
     // The Windows custom event structure
     PSDL_SysWMmsg=^TSDL_SysWMmsg;
     TSDL_SysWMmsg=record
      version:TSDL_version;
      subsystem:TSDL_SysWm;
      h_wnd:HWND; // The window for the message
      msg:UInt; // The type of message
      w_Param:WPARAM; // TSDLUInt16 message parameter
      lParam:LPARAM; // LONG message parameter
     end;
{$elseif defined(Unix)}
     // The Unix custom event structure
     PSDL_SysWMmsg=^TSDL_SysWMmsg;
     TSDL_SysWMmsg=record
      version:TSDL_version;
      subsystem:TSDL_SysWm;
 {$if not (defined(GP2X) or defined(Darwin) or defined(SkyOS) or defined(Android))}
      event:TXEvent;
 {$ifend}
     end;
{$else}
     // The generic custom event structure
     PSDL_SysWMmsg=^TSDL_SysWMmsg;
     TSDL_SysWMmsg=record
      version:TSDL_version;
      data:TSDLInt32;
     end;
{$ifend}

{$if defined(Windows)}
     // The Windows custom window manager information structure
     PSDL_SysWMinfo=^TSDL_SysWMinfo;
     TSDL_SysWMinfo=record
      version:TSDL_version;
      subsystem:TSDL_SysWm;
      window:HWnd;	// The display window
      hdc_:Hdc;
      hinstance:HModule;
     end;
{$elseif defined(Android)}
     // The Windows custom window manager information structure
     PSDL_SysWMinfo=^TSDL_SysWMinfo;
     TSDL_SysWMinfo=record
      version:TSDL_version;
      subsystem:TSDL_SysWm;
      window:PANativeWindow;	// The display window
      EGLsurface:pointer;
     end;
{$elseif defined(Unix)}
     // The Unix custom window manager information structure
 {$if not (defined(GP2X) or defined(Darwin) or defined(SkyOS) or defined(Android))}
     TSDL_SysWMinfoX11=record
      display:PDisplay;	// The X11 display
      window:TWindow ;		// The X11 display window */
      lock_func:Pointer;
      unlock_func:Pointer;
      fswindow:TWindow ;	// The X11 fullscreen window */
      wmwindow:TWindow ;	// The X11 managed input window */
     end;

     TSDL_SysWMinfoWayland=record
      display:pointer;
      surface:pointer;
      shell_surface:pointer;
     end;

     TSDL_SysWMinfoMIR=record
      connection:pointer;
      surface:pointer;
     end;

 {$ifend}
     PSDL_SysWMinfo=^TSDL_SysWMinfo;
     TSDL_SysWMinfo=record
      version:TSDL_version ;
      subsystem:TSDL_SysWm;
 {$if not (defined(GP2X) or defined(Darwin) or defined(SkyOS) or defined(Android))}
      case TSDL_SysWm of
       0:(
        X11:TSDL_SysWMinfoX11;
       );
       1:(
        Wayland:TSDL_SysWMinfoWayland;
       );
       2:(
        Mir:TSDL_SysWMinfoMir;
       );
 {$ifend}
     end;
{$else}
     // The generic custom window manager information structure
     PSDL_SysWMinfo=^TSDL_SysWMinfo;
     TSDL_SysWMinfo=record
      version:TSDL_version;
      subsystem:TSDL_SysWm;
      data:TSDLInt32;
     end;
{$ifend}

     // SDL_Event type definition
     TSDL_KeySym=record
      scancode:TSDLInt32;
      sym:TSDLInt32;
      modifier:TSDLUInt16;
      unicode:TSDLUInt32;
     end;

     TSDL_DisplayEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      windowID:TSDLUInt32;
      event:TSDLUInt8;
      padding1,padding2,padding3:TSDLUInt8;
      data1:TSDLInt32;
     end;

     TSDL_WindowEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      windowID:TSDLUInt32;
      event:TSDLUInt8;
      padding1,padding2,padding3:TSDLUInt8;
      data1,data2:TSDLInt32;
     end;

     // available in sdl12 but not exposed
     TSDL_TextEditingEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      windowID:TSDLUInt32;
      text:array[0..31] of TSDLUInt8;
      start,lenght:TSDLInt32;
     end;

     // available in sdl12 but not exposed
     TSDL_TextInputEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      windowID:TSDLUInt32;
      text:array[0..31] of TSDLUInt8;
     end;

     TSDL_TouchFingerEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      touchId:int64;
      fingerId:int64;
      x:single;
      y:single;
      dx:single;
      dy:single;
      pressure:single;
     end;

     TSDL_MultiGestureEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      windowId:TSDLUInt32;
      touchId:int64;
      dTheta,dDist,x,y:Single;
      numFingers,padding:TSDLUInt16;
     end;

     TSDL_DollarGestureEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      windowId:TSDLUInt32;
      touchId:int64;
      gesturedId:int64;
      numFingers:TSDLUInt32;
      error:Single;
     end;

     TSDL_SysWMEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      msg:PSDL_SysWMmsg;
     end;

     TSDL_DropEvent=record
      type_:TSDLUInt32;
      TimeStamp:TSDLUInt32;
      FileName:pansichar;
     end;

     TSDL_KeyboardEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      windowID:TSDLUInt32;
      state, repeat_, padding2, padding3:TSDLUInt8;
      keysym:TSDL_KeySym;
     end;

     TSDL_MouseMotionEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      windowID:TSDLUInt32;
      which:TSDLUInt32;
      state:TSDLUInt32;
      x:TSDLInt32;
      y:TSDLInt32;
      xrel:TSDLInt32;
      yrel:TSDLInt32;
     end;

     TSDL_MouseButtonEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      which:TSDLUInt32;
      windowID:TSDLUInt32;
      button:TSDLUInt8;
      state:TSDLUInt8;
      padding1:TSDLUInt8;
      padding2:TSDLUInt8;
      x:TSDLInt32;
      y:TSDLInt32;
     end;

     TSDL_MouseWheelEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      windowID:TSDLUInt32;
      which:TSDLUInt32;
      x:TSDLInt32;
      y:TSDLInt32;
      Direction:TSDLInt32;
     end;

     TSDL_JoyAxisEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      which:TSDLUInt8;
      axis:TSDLUInt8;
      padding1:TSDLUInt8;
      padding2:TSDLUInt8;
      padding3:TSDLUInt8;
      value:TSDLInt16;
      padding4:TSDLUInt16;
     end;

     TSDL_JoyBallEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      which:TSDLUInt8;
      ball:TSDLUInt8;
      padding1:TSDLUInt8;
      padding2:TSDLUInt8;
      padding3:TSDLUInt8;
      xrel:TSDLInt16;
      yrel:TSDLInt16;
     end;

     TSDL_JoyHatEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      which:TSDLUInt8;
      hat:TSDLUInt8;
      value:TSDLUInt8;
      padding1:TSDLUInt8;
     end;

     TSDL_JoyButtonEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      which:TSDLUInt8;
      button:TSDLUInt8;
      state:TSDLUInt8;
      padding1:TSDLUInt8;
      padding2:TSDLUInt8;
     end;

     TSDL_JoyDeviceEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      which:TSDLInt32;
      padding1:TSDLUInt8;
     end;

     TSDL_ControllerAxisEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      which:TSDLUInt8;
      axis:TSDLUInt8;
      padding1:TSDLUInt8;
      padding2:TSDLUInt8;
      padding3:TSDLUInt8;
      value:TSDLInt16;
      padding4:TSDLUInt16;
     end;

     TSDL_ControllerButtonEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      button:TSDLUInt8;
      state:TSDLUInt8;
      padding1:TSDLUInt8;
      padding2:TSDLUInt8;
     end;

     TSDL_ControllerDeviceEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      which:TSDLInt32;
     end;

     TSDL_AudioDeviceEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      which:TSDLInt32;
      iscapture:TSDLUInt8;
      padding1:TSDLUInt8;
      padding2:TSDLUInt8;
      padding3:TSDLUInt8;
     end;

     TSDL_SensorEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      which:TSDLInt32;
      Data:array[0..5] of TSDLFloat;
     end;

     TSDL_QuitEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
     end;

     TSDL_UserEvent=record
      type_:TSDLUInt32;
      timeStamp:TSDLUInt32;
      windowID:TSDLUInt32;
      code:TSDLInt32;
      data1,data2:Pointer;
     end;

     PSDL_Event=^TSDL_Event;
     TSDL_Event=record
      case TSDLInt32 of
       SDL_FIRSTEVENT:(type_:TSDLInt32);
       SDL_DISPLAYEVENT:(display:TSDL_DisplayEvent);
       SDL_WINDOWEVENT:(window:TSDL_WindowEvent);
       SDL_KEYDOWN,
       SDL_KEYUP:(key:TSDL_KeyboardEvent);
       SDL_TEXTEDITING:(edit:TSDL_TextEditingEvent);
       SDL_TEXTINPUT:(tedit:TSDL_TextInputEvent);
       SDL_MOUSEMOTION:(motion:TSDL_MouseMotionEvent);
       SDL_MOUSEBUTTONDOWN,
       SDL_MOUSEBUTTONUP:(button:TSDL_MouseButtonEvent);
       SDL_MOUSEWHEEL:(wheel:TSDL_MouseWheelEvent);
       SDL_JOYAXISMOTION:(jaxis:TSDL_JoyAxisEvent);
       SDL_JOYBALLMOTION:(jball:TSDL_JoyBallEvent);
       SDL_JOYHATMOTION:(jhat:TSDL_JoyHatEvent);
       SDL_JOYBUTTONDOWN,
       SDL_JOYBUTTONUP:(jbutton:TSDL_JoyButtonEvent);
       SDL_JOYDEVICEADDED,
       SDL_JOYDEVICEREMOVED:(jdevice:TSDL_JoyDeviceEvent);
       SDL_CONTROLLERAXISMOTION:(caxis:TSDL_ControllerAxisEvent);
       SDL_CONTROLLERBUTTONDOWN,
       SDL_CONTROLLERBUTTONUP:(cbutton:TSDL_ControllerButtonEvent);
       SDL_CONTROLLERDEVICEADDED,
       SDL_CONTROLLERDEVICEREMOVED,
       SDL_CONTROLLERDEVICEREMAPPED:(cdevice:TSDL_ControllerDeviceEvent);
       SDL_AUDIODEVICEADDED,
       SDL_AUDIODEVICEREMOVED:(adevice:TSDL_AudioDeviceEvent);
       SDL_SENSORUPDATE:(sensor:TSDL_SensorEvent);
       SDL_QUITEV:(quit:TSDL_QuitEvent);
       SDL_USEREVENT:(user:TSDL_UserEvent);
       SDL_SYSWMEVENT:(syswm:TSDL_SysWMEvent);
       SDL_FINGERDOWN,
       SDL_FINGERUP,
       SDL_FINGERMOTION:(tfinger:TSDL_TouchFingerEvent);
       SDL_MULTIGESTURE:(mgesture:TSDL_MultiGestureEvent);
       SDL_DOLLARGESTURE:(dgesture:TSDL_DollarGestureEvent);
       SDL_DROPFILE:(drop:TSDL_DropEvent);
       TSDLInt32(SDL_ALLEVENTS):(foo:shortstring);
     end;

     TSDL_EventFilter=function(event:PSDL_Event):TSDLInt32; cdecl;

     PSDL_MessageBoxColor=^TSDL_MessageBoxColor;
     TSDL_MessageBoxColor=record
      r,g,b:TSDLUInt8;
     end;

     PSDL_MessageBoxColorScheme=^TSDL_MessageBoxColorScheme;
     TSDL_MessageBoxColorScheme=record
      colors:array[0..4] of TSDL_MessageBoxColor;
     end;

     PSDL_MessageBoxButtonData=^TSDL_MessageBoxButtonData;
     TSDL_MessageBoxButtonData=record
      flags:TSDLUInt32;
      buttonid:TSDLInt32;
      text:PAnsiChar;
     end;

     PSDL_MessageBoxData=^TSDL_MessageBoxData;
     TSDL_MessageBoxData=record
      flags:TSDLUInt32;
      window:PSDL_Window;
      title:PAnsiChar;
      message:PAnsiChar;
      numbuttons:TSDLInt32;
      buttons:PSDL_MessageBoxButtonData;
      colorScheme:PSDL_MessageBoxColorScheme;
     end;

     PSDLUInt8Array=^TSDLUInt8Array;
     TSDLUInt8Array=array[0..65535] of TSDLUInt8;

     PSDLUInt32Array=^TSDLUInt32Array;
     TSDLUInt32Array=array[0..16383] of TSDLUInt32;

     PSDL_Thread=Pointer;
     PSDL_mutex=Pointer;

     TSDL_GLattr=
      (
       SDL_GL_RED_SIZE,
       SDL_GL_GREEN_SIZE,
       SDL_GL_BLUE_SIZE,
       SDL_GL_ALPHA_SIZE,
       SDL_GL_BUFFER_SIZE,
       SDL_GL_DOUBLEBUFFER,
       SDL_GL_DEPTH_SIZE,
       SDL_GL_STENCIL_SIZE,
       SDL_GL_ACCUM_RED_SIZE,
       SDL_GL_ACCUM_GREEN_SIZE,
       SDL_GL_ACCUM_BLUE_SIZE,
       SDL_GL_ACCUM_ALPHA_SIZE,
       SDL_GL_STEREO,
       SDL_GL_MULTISAMPLEBUFFERS,
       SDL_GL_MULTISAMPLESAMPLES,
       SDL_GL_ACCELERATED_VISUAL,
       SDL_GL_RETAINED_BACKING,
       SDL_GL_CONTEXT_MAJOR_VERSION,
       SDL_GL_CONTEXT_MINOR_VERSION,
       SDL_GL_CONTEXT_EGL,
       SDL_GL_CONTEXT_FLAGS,
       SDL_GL_CONTEXT_PROFILE_MASK,
       SDL_GL_SHARE_WITH_CURRENT_CONTEXT,
       SDL_GL_FRAMEBUFFER_SRGB_CAPABLE
      );

{    TSDL_ArrayByteOrder=  // array component order, low TSDLUInt8 -> high TSDLUInt8
      (
       SDL_ARRAYORDER_NONE,
       SDL_ARRAYORDER_RGB,
       SDL_ARRAYORDER_RGBA,
       SDL_ARRAYORDER_ARGB,
       SDL_ARRAYORDER_BGR,
       SDL_ARRAYORDER_BGRA,
       SDL_ARRAYORDER_ABGR
      );}

     TSDL_PixelFormatEnum=TSDLInt32;

     // Joystick/Controller support
     PSDL_Joystick=^TSDL_Joystick;
     TSDL_Joystick=record
     end;

     PSDL_GameController=^TSDL_GameController;
     TSDL_GameController=record
     end;

     PSDL_DisplayMode=^TSDL_DisplayMode;
     TSDL_DisplayMode=record
      format:TSDLUInt32;
      w:TSDLInt32;
      h:TSDLInt32;
      refrsh_rate:TSDLInt32;
      driverdata:pointer;
     end;

     PSDL_LogCategory=^TSDL_LogCategory;
     TSDL_LogCategory=TSDLInt32;

     TSDL_LogPriority=
      (
       SDL_LOG_PRIORITY_VERBOSE=1,
       SDL_LOG_PRIORITY_DEBUG,
       SDL_LOG_PRIORITY_INFO,
       SDL_LOG_PRIORITY_WARN,
       SDL_LOG_PRIORITY_ERROR,
       SDL_LOG_PRIORITY_CRITICAL,
       SDL_NUM_LOG_PRIORITIES
      );

     PSDL_LogOutputCallback=^TSDL_LogOutputCallback;
     TSDL_LogOutputCallback=procedure(UserData:pointer;category:TSDLInt32;priority:TSDL_LogPriority;message:PAnsiChar); cdecl;

     TSDL_HintCallback=procedure(userdata:pointer;name,oldValue,newValue:PAnsiChar); cdecl;

{$ifdef android}
procedure SDL_Android_Init(Env:PJNIEnv;cls:JClass); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
{$endif}

procedure  SDL_SetMainReady(); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_Init(flags:TSDLUInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_InitSubSystem(flags:TSDLUInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_Quit; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_Delay(msec:TSDLUInt32); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetTicks:TSDLUInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GetPerformanceCounter:uint64; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetPerformanceFrequency:uint64; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_LockSurface(Surface:PSDL_Surface):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_UnlockSurface(Surface:PSDL_Surface); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_LockTexture(Texture:PSDL_Texture;rect:PSDL_Rect;const Pixels,Pitch:pointer):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_UnlockTexture(Texture:PSDL_Texture); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_QueryTexture(Texture:PSDL_Texture;format:PSDLUInt32;access,w,h:PSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GL_BindTexture(Texture:PSDL_Texture;texW,texH:PSDLFloat):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GL_UnbindTexture(Texture:PSDL_Texture):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetTextureAlphaMod(Texture:PSDL_Texture;alpha:PSDLUInt8):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_UpdateTexture(Texture:PSDL_Texture;rect:PSDL_Rect;format:TSDLUInt32;pixels:Pointer;pitch:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_UpdateYUVTexture(Texture:PSDL_Texture;rect:PSDL_Rect;Yplane:Pointer;Ypitch:TSDLInt32;Uplane:Pointer;Upitch:TSDLInt32;Vplane:Pointer;Vpitch:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_SetTextureAlphaMod(Texture:PSDL_Texture;alpha:TSDLUInt8):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetTextureColorMod(Texture:PSDL_Texture;r,g,b:PSDLUInt8):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_SetTextureColorMod(Texture:PSDL_Texture;r,g,b:TSDLUInt8):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetTextureBlendMode(Texture:PSDL_Texture;blend_mode:PSDL_BlendMode):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_SetTextureBlendMode(Texture:PSDL_Texture;blend_mode:TSDL_BlendMode):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GetError:pansichar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_SetVideoMode(width,height,bpp:TSDLInt32;flags:TSDLUInt32):PSDL_Surface; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_CreateRGBSurface(flags:TSDLUInt32;Width,Height,Depth:TSDLInt32;RMask,GMask,BMask,AMask:TSDLUInt32):PSDL_Surface; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_CreateRGBSurfaceFrom(pixels:Pointer;width,height,depth,pitch:TSDLInt32;RMask,GMask,BMask,AMask:TSDLUInt32):PSDL_Surface; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_CreateRGBSurfaceWithFormat(flags:TSDLUInt32;width,height,depth:TSDLInt32;format:TSDLUInt32):PSDL_Surface; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_FreeSurface(Surface:PSDL_Surface); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_SetColorKey(surface:PSDL_Surface;flag,key:TSDLUInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_SetAlpha(surface:PSDL_Surface;flag,key:TSDLUInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_ConvertSurface(src:PSDL_Surface;fmt:PSDL_PixelFormat;flags:TSDLInt32):PSDL_Surface; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_UpperBlit(src:PSDL_Surface;srcrect:PSDL_Rect;dst:PSDL_Surface;dstrect:PSDL_Rect):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_FillRect(dst:PSDL_Surface;dstrect:PSDL_Rect;color:TSDLUInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_UpdateRect(Screen:PSDL_Surface;x,y:TSDLInt32;w,h:TSDLUInt32); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_Flip(Screen:PSDL_Surface):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_GetRGB(pixel:TSDLUInt32;fmt:PSDL_PixelFormat;r,g,b:PSDLUInt8); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_GetRGBA(pixel:TSDLUInt32;fmt:PSDL_PixelFormat;r,g,b,a:PSDLUInt8); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_MapRGB(format:PSDL_PixelFormat;r,g,b:TSDLUInt8):TSDLUInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_MapRGBA(format:PSDL_PixelFormat;r,g,b,a:TSDLUInt8):TSDLUInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_DisplayFormat(Surface:PSDL_Surface):PSDL_Surface; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_DisplayFormatAlpha(Surface:PSDL_Surface):PSDL_Surface; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_RWFromFile(filename,mode:pansichar):PSDL_RWops; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RWFromMem(mem:Pointer;size:TSDLInt32):PSDL_RWops; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RWFromConstMem(mem:Pointer;size:TSDLInt32):PSDL_RWops; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_SaveBMP_RW(surface:PSDL_Surface;dst:PSDL_RWops;freedst:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_AllocRW:PSDL_RWops; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_FreeRW(rw:PSDL_RWops); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GetClipboardText:PAnsiChar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_SetClipboardText(const Text:PAnsiChar):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_HasClipboardText:TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_SetWindowTitle(window:PSDL_Window;title:pansichar); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_SetWindowFullscreen(window:PSDL_Window;fullscreen:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_SetWindowSize(window:PSDL_Window;x,y:TSDLInt32); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_SetWindowPosition(window:PSDL_Window;x,y:TSDLInt32); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_SetWindowResizable(window:PSDL_Window;b:boolean); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_SetWindowBordered(window:PSDL_Window;b:boolean); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GetWindowFlags(window:PSDL_Window):TSDLUInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_GetWindowSize(window:PSDL_Window;var x,y:TSDLInt32); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_GetWindowPosition(window:PSDL_Window;var x,y:TSDLInt32); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_CreateWindow(title:pansichar;x,y,w,h:TSDLInt32;flags:TSDLUInt32):PSDL_Window; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_CreateRenderer(window:PSDL_Window;index:TSDLInt32;flags:TSDLUInt32):PSDL_Renderer; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_CreateTexture(renderer:PSDL_Texture;format:TSDLUInt32;access,w,h:TSDLInt32):PSDL_Texture; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_DestroyWindow(window:PSDL_Window):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_DestroyRenderer(renderer:PSDL_Renderer):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_DestroyTexture(texture:PSDL_Texture):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_MinimizeWindow(window:PSDL_Window); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_MaximizeWindow(window:PSDL_Window); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_RestoreWindow(window:PSDL_Window); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GL_MakeCurrent(window:PSDL_Window;context:PSDL_GLContext):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GL_CreateContext(window:PSDL_Window):PSDL_GLContext; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_GL_DeleteContext(context:PSDL_GLContext); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GL_SwapWindow(window:PSDL_Window):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GL_GetDrawableSize(window:PSDL_Window;w,h:PSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GL_SetSwapInterval(interval:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_VideoQuit; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GetNumVideoDisplays:TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GetNumVideoDrivers:TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetVideoDriver(Index:TSDLInt32):pansichar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetCurrentVideoDriver:pansichar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_VideoInit(drivername:pansichar):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_ShowWindow(window:PSDL_Window); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GetNumRenderDrivers:TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_SetRenderDrawBlendMode(renderer:PSDL_Renderer;blendMode:TSDL_BlendMode):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetRenderDrawBlendMode(renderer:PSDL_Renderer;blendMode:PSDL_BlendMode):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_SetRenderDrawColor(renderer:PSDL_Renderer;r,g,b,a:TSDLUInt8):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetRenderDrawColor(renderer:PSDL_Renderer;r,g,b,a:PSDLUInt8):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetRenderer(window:PSDL_Window):PSDL_Renderer; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetRendererTarget(renderer:PSDL_Renderer):PSDL_Texture; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetRendererDriverInfo(index:TSDLInt32;info:PSDL_RendererInfo):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetRendererInfo(renderer:PSDL_Renderer;info:PSDL_RendererInfo):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetRendererOutputSize(renderer:PSDL_Renderer;w,h:PSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderFillRect(renderer:PSDL_Renderer;rect:PSDL_Rect):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderFillRects(renderer:PSDL_Renderer;rects:PSDL_Rect;Count:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderDrawLine(renderer:PSDL_Renderer;x1,y1,x2,y2:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderDrawLines(renderer:PSDL_Renderer;points:PSDL_Point;count:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderDrawPoint(renderer:PSDL_Renderer;x,y:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderDrawPoints(renderer:PSDL_Renderer;points:PSDL_Point;count:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderDrawRect(renderer:PSDL_Renderer;rect:PSDL_Rect):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderDrawRects(renderer:PSDL_Renderer;rects:PSDL_Rect;count:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderClear(renderer:PSDL_Renderer):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderCopy(renderer:PSDL_Renderer;texture:PSDL_Texture;srcrect,dstrect:PSDL_Rect):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderCopyEx(renderer:PSDL_Renderer;texture:PSDL_Texture;srcrect,dstrect:PSDL_Rect;angle:TSDLDouble;center:PSDL_Point;flip:TSDL_RendererFlip):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_RenderPresent(renderer:PSDL_Renderer); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderReadPixels(renderer:PSDL_Renderer;rect:PSDL_Rect;format:TSDLUInt32;pixels:Pointer;pitch:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderSetViewport(window:PSDL_Window;rect:PSDL_Rect):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_RenderGetClipRect(renderer:PSDL_Renderer;rect:PSDL_Rect); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderGetIntegerScale(renderer:PSDL_Renderer):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderGetLogicalSize(renderer:PSDL_Renderer;w,h:PSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderGetScale(renderer:PSDL_Renderer;scaleX,scaleY:PSDLFloat):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderGetViewport(renderer:PSDL_Renderer;rect:PSDL_Rect):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderIsClipEnabled(renderer:PSDL_Renderer):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderSetClipRect(renderer:PSDL_Renderer;rect:PSDL_Rect):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderSetIntegerScale(renderer:PSDL_Renderer;enable:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderSetLogicalSize(renderer:PSDL_Renderer;w,h:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderSetScale(renderer:PSDL_Renderer;scaleX,scaleY:TSDLFloat):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_RenderTargetSupported(renderer:PSDL_Renderer):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_SetRenderTarget(renderer:PSDL_Renderer;texture:PSDL_Texture):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};


function SDL_GetRelativeMouseMode:TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_SetRelativeMouseMode(enabled:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetRelativeMouseState(x,y:PLongInt):TSDLUInt8; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_PixelFormatEnumToMasks(format:{TSDL_ArrayByteOrder}TSDLInt32;bpp:PLongInt;Rmask,Gmask,Bmask,Amask:PLongInt):Boolean; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_WarpMouseInWindow(window:PSDL_Window;x,y:TSDLInt32); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_StartTextInput; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_StopTextInput; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_PeepEvents(event:PSDL_Event; numevents:TSDLInt32;action:SDL_eventaction;minType,maxType:TSDLUInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_CreateThread(fn:Pointer;name:pansichar;data:Pointer):PSDL_Thread; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GetMouseState(x,y:PLongInt):TSDLUInt8; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetKeyName(key:TSDLUInt32):pansichar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_PumpEvents; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_PushEvent(event:PSDL_Event):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_PollEvent(event:PSDL_Event):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_WaitEvent(event:PSDL_Event):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_SetEventFilter(filter:TSDL_EventFilter); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_ShowCursor(toggle:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_malloc(Size:TSDLInt32):pointer; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_realloc(mem:pointer;Size:TSDLInt32):pointer; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_free(mem:pointer); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_EventState(type_:TSDLUInt32;state:TSDLInt32):TSDLUInt8; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_WM_SetIcon(icon:PSDL_Surface;mask:TSDLUInt8); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_WM_SetCaption(title:pansichar;icon:pansichar); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_WM_ToggleFullScreen(surface:PSDL_Surface):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_WaitThread(thread:PSDL_Thread;status:PLongInt); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_CreateMutex:PSDL_mutex; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_DestroyMutex(mutex:PSDL_mutex); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_LockMutex(mutex:PSDL_mutex):TSDLInt32; cdecl; external SDL2LibName name 'SDL_mutexP';
function SDL_UnlockMutex(mutex:PSDL_mutex):TSDLInt32; cdecl; external SDL2LibName name 'SDL_mutexV';

function SDL_GL_GetAttribute(attr:TSDL_GLAttr;var value:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GL_SetAttribute(attr:TSDL_GLattr;value:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_GL_SwapBuffers(); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_NumJoysticks:TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickName(joy:PSDL_Joystick):pansichar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickInstanceID(joy:PSDL_Joystick):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickOpen(idx:TSDLInt32):PSDL_Joystick; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickOpened(idx:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickIndex(joy:PSDL_Joystick):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickNumAxes(joy:PSDL_Joystick):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickNumBalls(joy:PSDL_Joystick):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickNumHats(joy:PSDL_Joystick):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickNumButtons(joy:PSDL_Joystick):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_JoystickUpdate; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickEventState(state:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickGetGUID(joy:PSDL_Joystick):TGUID; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickGetDeviceGUID(joy:PSDL_Joystick):TGUID; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickGetAxis(joy:PSDL_Joystick;axis:TSDLInt32):TSDLInt16; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickGetBall(joy:PSDL_Joystick;ball:TSDLInt32;dx:PInteger;dy:PInteger):Word; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickGetHat(joy:PSDL_Joystick;hat:TSDLInt32):TSDLUInt8; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_JoystickGetButton(joy:PSDL_Joystick;button:TSDLInt32):TSDLUInt8; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_JoystickClose(joy:PSDL_Joystick); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_IsGameController(idx:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerOpen(idx:TSDLInt32):PSDL_GameController; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_GameControllerClose(gamecontroller:PSDL_GameController); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_GameControllerUpdate; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerAddMapping(mappingString:pansichar):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerAddMappingsFromFile(filename:pansichar):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerAddMappingsFromRW(rw:pointer;freerw:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerEventState(state:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerGetAttached(gamecontroller:PSDL_GameController):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerGetAxis(gamecontroller:PSDL_GameController;axis:TSDL_GameControllerAxis):TSDLInt16; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerGetAxisFromString(pchString:pansichar):TSDL_GameControllerAxis; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerGetBindForAxis(gamecontroller:PSDL_GameController;axis:TSDL_GameControllerAxis):TSDL_GameControllerBindType; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerGetBindForButton(gamecontroller:PSDL_GameController;button:TSDL_GameControllerButton):TSDL_GameControllerBindType; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerGetButton(gamecontroller:PSDL_GameController;button:TSDL_GameControllerButton):TSDLUInt8; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerGetButtonFromString(pchString:pansichar):TSDL_GameControllerButton; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerGetJoystick(gamecontroller:PSDL_GameController):PSDL_Joystick; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerGetStringForAxis(axis:TSDL_GameControllerAxis):pansichar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerGetStringForButton(button:TSDL_GameControllerButton):pansichar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerMapping(gamecontroller:PSDL_GameController):pansichar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerMappingForGUID(const guid:TGUID):pansichar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerName(gamecontroller:PSDL_GameController):pansichar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GameControllerNameForIndex(idx:TSDLInt32):pansichar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GL_LoadLibrary(filename:pansichar):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GL_GetProcAddress(procname:pansichar):Pointer; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GetModState:TSDLUInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_AudioInit(const aDriverName:PAnsiChar):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_AudioQuit; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_BuildAudioCVT(cvt:PSDL_AudioCVT;src_format:TSDL_AudioFormat;src_channels:TSDLUInt8;src_rate:TSDLInt32;dst_format:TSDL_AudioFormat;dst_channels:TSDLUInt8;dst_rate:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_LockAudio; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_UnlockAudio; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_LockAudioDevice(dev:TSDL_AudioDeviceID); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_UnlockAudioDevice(dev:TSDL_AudioDeviceID); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_CloseAudio; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_CloseAudioDevice(dev:TSDL_AudioDeviceID); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GetCurrentAudioDriver:PAnsiChar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetAudioDriver(index:TSDLInt32):PAnsiChar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetNumAudioDrivers:TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GetNumAudioDevices(iscapture:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetAudioDeviceName(index,iscapture:TSDLInt32):PAnsiChar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_OpenAudio(desired,obtained:PSDL_AudioSpec):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_OpenAudioDevice(device:PAnsiChar;iscapture:TSDLInt32;desired,obtained:PSDL_AudioSpec;allowed_changes:TSDLInt32):TSDL_AudioDeviceID; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GetAudioStatus:TSDL_AudioStatus; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GetAudioDeviceStatus(dev:TSDL_AudioDeviceID):TSDL_AudioStatus; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_PauseAudio(pause_on:TSDLInt32); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_PauseAudioDevice(dev:TSDL_AudioDeviceID;pause_on:TSDLInt32); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_FreeWAV(audio_buf:PSDLUInt8); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_LoadWAV_RW(src:PSDL_RWops;freesrc:TSDLInt32;spec:PSDL_AudioSpec;audio_buf:PSDLUInt8;audio_len:PSDLUInt32):PSDL_AudioSpec; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_ConvertAudio(cvt:PSDL_AudioCVT); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_MixAudio(dst,src:PSDLUInt8;len:TSDLUInt32;volume:TSDLInt32); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_MixAudioFormat(dst,src:PSDLUInt8;format:TSDL_AudioFormat;len:TSDLUInt32;volume:TSDLInt32); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

//function SDL_BlitSurface(src:PSDL_Surface;srcrect:PSDL_Rect;dst:PSDL_Surface;dstrect:PSDL_Rect):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

{$ifdef Windows}
function SDL_putenv(const text:pansichar):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_getenv(const text:pansichar):pansichar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
{$endif}

//procedure SDL_WarpMouse(x,y:Word); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetKeyboardState(numkeys:PLongInt):PByteArray; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_AllocFormat(format:TSDLUInt32):PSDL_PixelFormat; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_FreeFormat(pixelformat:PSDL_PixelFormat); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
//function SDL_VideoDriverName(namebuf:pansichar;maxlen:TSDLInt32):pansichar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_BUTTON(Button:TSDLInt32):TSDLInt32;

function SDL_GetClosestDisplayMode(displayIndex:TSDLInt32;mode,closest:PSDL_DisplayMode):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetCurrentDisplayMode(displayIndex:TSDLInt32;mode:PSDL_DisplayMode):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetDesktopDisplayMode(displayIndex:TSDLInt32;mode:PSDL_DisplayMode):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetDisplayBounds(displayIndex:TSDLInt32;rect:PSDL_rect):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetDisplayMode(displayIndex,modeIndex:TSDLInt32;mode:PSDL_DisplayMode):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetNumDisplayModes(displayIndex:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetWindowDisplayIndex(window:PSDL_Window):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetWindowDisplayMode(window:PSDL_Window;mode:PSDL_DisplayMode):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_SetWindowDisplayMode(window:PSDL_Window;mode:PSDL_DisplayMode):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetWindowWMInfo(window:PSDL_Window;info:PSDL_SysWMinfo):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_SetWindowInputFocus(window:PSDL_Window):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_RaiseWindow(window:PSDL_Window):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_DisableScreenSaver; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_EnableScreenSaver; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_IsScreenSaverEnabled:TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_GetScancodeFromKey(KeyCode:TSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_LogSetAllPriority(priority:TSDL_LogPriority); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_LogSetPriority(category:TSDL_LogCategory;priority:TSDL_LogPriority); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_LogGetPriority(category:TSDL_LogCategory):TSDL_LogPriority; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_LogResetPriorities; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_Log(const fmt:PAnsiChar); cdecl; varargs; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_LogVerbose(category:TSDL_LogCategory;const fmt:PAnsiChar); cdecl; varargs; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_LogDebug(category:TSDL_LogCategory;const fmt:PAnsiChar); cdecl; varargs; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_LogInfo(category:TSDL_LogCategory;const fmt:PAnsiChar); cdecl; varargs; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_LogWarn(category:TSDL_LogCategory;const fmt:PAnsiChar); cdecl; varargs; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_LogError(category:TSDL_LogCategory;const fmt:PAnsiChar); cdecl; varargs; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_LogCritical(category:TSDL_LogCategory;const fmt:PAnsiChar); cdecl; varargs; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_LogMessage(category:TSDL_LogCategory;priority:TSDL_LogPriority;const fmt:PAnsiChar); cdecl; varargs; external {$ifndef staticlink}SDL2LibName{$endif};
//procedure SDL_LogMessageV(category:TSDL_LogCategory;priority:TSDL_LogPriority;const fmt:PAnsiChar;ap:TVA_List); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_LogGetOutputFunction(LogCallback:PSDL_LogOutputCallback;UserData:PPointer); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_LogSetOutputFunction(LogCallback:TSDL_LogOutputCallback;UserData:pointer); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_ShowMessageBox(const messageboxdata:PSDL_MessageBoxData;const buttonid:PSDLInt32):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_ShowSimpleMessageBox(flags:TSDLUInt32;title,message_:PAnsiChar;window:PSDL_Window):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

function SDL_SetHintWithPriority(name,value:PAnsiChar;priority:TSDL_HintPriority):boolean; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_SetHint(name,value:PAnsichar):boolean; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetHint(name:PAnsichar):PAnsichar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetHintBoolean(name:PAnsichar;default_value:boolean):PAnsichar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_AddHintCallback(name:PAnsichar;callback:TSDL_HintCallback;userdata:pointer); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_DelHintCallback(name:PAnsichar;callback:TSDL_HintCallback;userdata:pointer); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_ClearHints; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

{$if defined(Android)}
function SDL_AndroidGetJNIEnv:pointer; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_AndroidGetActivity:pointer; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
{$ifend}

{$if defined(PasVulkanUseSDL2WithVulkanSupport)}
{$if defined(PasVulkanUseSDL2WithStaticVulkanSupport)}
function SDL_Vulkan_LoadLibrary(path:PAnsiChar):TSDLInt32; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_Vulkan_GetVkGetInstanceProcAddr:pointer; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_Vulkan_UnloadLibrary; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_Vulkan_GetInstanceExtensions(window:PSDL_Window;pCount:PSDLUInt32;names:pointer{PPAnsiChar}):boolean; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_Vulkan_CreateSurface(window:PSDL_Window;instance_:TVkInstance;surface:PVkSurfaceKHR):boolean; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
procedure SDL_Vulkan_GetDrawableSize(window:PSDL_Window;w,h:PSDLInt32); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
{$else}
type TSDL_Vulkan_LoadLibrary=function(path:PAnsiChar):TSDLInt32; cdecl;
     TSDL_Vulkan_GetVkGetInstanceProcAddr=function:pointer; cdecl;
     TSDL_Vulkan_UnloadLibrary=procedure; cdecl;
     TSDL_Vulkan_GetInstanceExtensions=function(window:PSDL_Window;pCount:PSDLUInt32;names:pointer{PPAnsiChar}):boolean; cdecl;
     TSDL_Vulkan_CreateSurface=function(window:PSDL_Window;instance_:TVkInstance;surface:PVkSurfaceKHR):boolean; cdecl;
     TSDL_Vulkan_GetDrawableSize=procedure(window:PSDL_Window;w,h:PSDLInt32); cdecl;

var SDL_Vulkan_LoadLibrary:TSDL_Vulkan_LoadLibrary=nil;
    SDL_Vulkan_GetVkGetInstanceProcAddr:TSDL_Vulkan_GetVkGetInstanceProcAddr=nil;
    SDL_Vulkan_UnloadLibrary:TSDL_Vulkan_UnloadLibrary=nil;
    SDL_Vulkan_GetInstanceExtensions:TSDL_Vulkan_GetInstanceExtensions=nil;
    SDL_Vulkan_CreateSurface:TSDL_Vulkan_CreateSurface=nil;
    SDL_Vulkan_GetDrawableSize:TSDL_Vulkan_GetDrawableSize=nil;

{$define PasVulkanUseDynamicSDL2}

{$ifend}
{$ifend}

{$ifdef PasVulkanUseDynamicSDL2}
var SDL_Library:pointer=nil;
{$endif}

procedure SDL_GetVersion(out Version:TSDL_Version); cdecl; external {$ifndef staticlink}SDL2LibName{$endif};
function SDL_GetRevision:PAnsiChar; cdecl; external {$ifndef staticlink}SDL2LibName{$endif};

procedure SDL_VERSION(out Version:TSDL_Version);

implementation

{$ifdef PasVulkanUseDynamicSDL2}
function sdl2LoadLibrary(const LibraryName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}
begin
{$ifdef Windows}
 result:={%H-}pointer(LoadLibrary(PChar(LibraryName)));
{$else}
{$ifdef Unix}
 result:=dlopen(PChar(LibraryName),RTLD_NOW or RTLD_LAZY);
{$else}
 result:=nil;
{$endif}
{$endif}
end;

function sdl2FreeLibrary(LibraryHandle:pointer):boolean; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=assigned(LibraryHandle);
 if result then begin
{$ifdef Windows}
  result:=FreeLibrary({%H-}HMODULE(LibraryHandle));
{$else}
{$ifdef Unix}
  result:=dlclose(LibraryHandle)=0;
{$else}
  result:=false;
{$endif}
{$endif}
 end;
end;

function sdl2GetProcAddress(LibraryHandle:pointer;const ProcName:string):pointer; {$ifdef CAN_INLINE}inline;{$endif}
begin
{$ifdef Windows}
 result:=GetProcAddress({%H-}HMODULE(LibraryHandle),PChar(ProcName));
{$else}
{$ifdef Unix}
 result:=dlsym(LibraryHandle,PChar(ProcName));
{$else}
 result:=nil;
{$endif}
{$endif}
end;

function LoadSDL2Library(const LibraryName:string=SDL2LibName):boolean;
begin
 SDL_Library:=sdl2LoadLibrary(LibraryName);
 result:=assigned(SDL_Library);
 if result then begin
  SDL_Vulkan_LoadLibrary:=sdl2GetProcAddress(SDL_Library,'SDL_Vulkan_LoadLibrary');
  SDL_Vulkan_GetVkGetInstanceProcAddr:=sdl2GetProcAddress(SDL_Library,'SDL_Vulkan_GetVkGetInstanceProcAddr');
  SDL_Vulkan_UnloadLibrary:=sdl2GetProcAddress(SDL_Library,'SDL_Vulkan_UnloadLibrary');
  SDL_Vulkan_GetInstanceExtensions:=sdl2GetProcAddress(SDL_Library,'SDL_Vulkan_GetInstanceExtensions');
  SDL_Vulkan_CreateSurface:=sdl2GetProcAddress(SDL_Library,'SDL_Vulkan_CreateSurface');
  SDL_Vulkan_GetDrawableSize:=sdl2GetProcAddress(SDL_Library,'SDL_Vulkan_GetDrawableSize');
 end;
end;

{$endif}

function SDL_BUTTON(Button:TSDLInt32):TSDLInt32;
begin
 result:=1 shl (Button-1);
end;

procedure SDL_VERSION(out Version:TSDL_Version);
begin
 Version.major:=SDL_MAJOR_VERSION;
 Version.minor:=SDL_MINOR_VERSION;
 Version.patch:=SDL_PATCHLEVEL;
end;

{$ifdef PasVulkanUseDynamicSDL2}
initialization
 LoadSDL2Library;
finalization
 if assigned(SDL_Library) then begin
  sdl2FreeLibrary(SDL_Library);
 end;
{$endif}
end.

