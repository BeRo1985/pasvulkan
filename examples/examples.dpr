{$ifdef fpc}
 {$ifdef android}
  {$define fpcandroid}
 {$endif}
{$endif}
{$ifdef fpcandroid}library{$else}program{$endif} examples;
{$ifdef fpc}
 {$mode delphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$if defined(win32) or defined(win64)}
 {$apptype console}
 {$define Windows}
{$ifend}

(*{$if defined(fpc) and defined(Unix)}
   cthreads,
   BaseUnix,
  {$elseif defined(Windows)}
   Windows,
  {$ifend}*)

uses
  {$if defined(fpc) and defined(Unix)}
   cthreads,
   BaseUnix,
  {$elseif defined(Windows)}
   Windows,
  {$ifend}
  SysUtils,
  Classes,
  PasDblStrUtils in '..\externals\pasdblstrutils\src\PasDblStrUtils.pas',
  PUCU in '..\externals\pucu\src\PUCU.pas',
  PasMP in '..\externals\pasmp\src\PasMP.pas',
  kraft in '..\externals\kraft\src\kraft.pas',
  PasJSON in '..\externals\pasjson\src\PasJSON.pas',
  Vulkan in '..\src\Vulkan.pas',
  PasVulkan.Math in '..\src\PasVulkan.Math.pas',
  PasVulkan.Types in '..\src\PasVulkan.Types.pas',
  PasVulkan.Utils in '..\src\PasVulkan.Utils.pas',
  PasVulkan.CircularDoublyLinkedList in '..\src\PasVulkan.CircularDoublyLinkedList.pas',
  PasVulkan.Collections in '..\src\PasVulkan.Collections.pas',
  PasVulkan.TextEditor in '..\src\PasVulkan.TextEditor.pas',
  PasVulkan.XML in '..\src\PasVulkan.XML.pas',
  PasVulkan.VectorPath in '..\src\PasVulkan.VectorPath.pas',
  PasVulkan.TrueTypeFont in '..\src\PasVulkan.TrueTypeFont.pas',
  PasVulkan.Streams in '..\src\PasVulkan.Streams.pas',
  PasVulkan.Assets in '..\src\PasVulkan.Assets.pas',
  PasVulkan.Compression.Deflate in '..\src\PasVulkan.Compression.Deflate.pas',
  PasVulkan.Archive.ZIP in '..\src\PasVulkan.Archive.ZIP.pas',
  PasVulkan.Image.PNG.ExternalLibrary in '..\src\PasVulkan.Image.PNG.ExternalLibrary.pas',
  PasVulkan.Image.PNG in '..\src\PasVulkan.Image.PNG.pas',
  PasVulkan.Image.BMP in '..\src\PasVulkan.Image.BMP.pas',
  PasVulkan.Image.JPEG in '..\src\PasVulkan.Image.JPEG.pas',
  PasVulkan.Image.TGA in '..\src\PasVulkan.Image.TGA.pas',
  PasVulkan.Framework in '..\src\PasVulkan.Framework.pas',
  PasVulkan.ConvexHullGenerator2D in '..\src\PasVulkan.ConvexHullGenerator2D.pas',
  PasVulkan.Sprites in '..\src\PasVulkan.Sprites.pas',
  PasVulkan.Canvas in '..\src\PasVulkan.Canvas.pas',
  PasVulkan.SignedDistanceField2D in '..\src\PasVulkan.SignedDistanceField2D.pas',
  PasVulkan.Font in '..\src\PasVulkan.Font.pas',
  PasVulkan.Android in '..\src\PasVulkan.Android.pas',
  PasVulkan.SDL2 in '..\src\PasVulkan.SDL2.pas',
  PasVulkan.StaticLinking in '..\src\PasVulkan.StaticLinking.pas',
  PasVulkan.Audio.HRTFTables in '..\src\PasVulkan.Audio.HRTFTables.pas',
  PasVulkan.Audio.OGGVorbisTremor in '..\src\PasVulkan.Audio.OGGVorbisTremor.pas',
  PasVulkan.Audio.OGGVorbisTremor.ExternalLibrary in '..\src\PasVulkan.Audio.OGGVorbisTremor.ExternalLibrary.pas',
  PasVulkan.Audio in '..\src\PasVulkan.Audio.pas',
  PasVulkan.Application in '..\src\PasVulkan.Application.pas',
  PasVulkan.GUI in '..\src\PasVulkan.GUI.pas',
  UnitChunkStream in 'UnitChunkStream.pas',
  UnitRegisteredExamplesList in 'UnitRegisteredExamplesList.pas',
  UnitExampleApplication in 'UnitExampleApplication.pas',
  UnitTextOverlay in 'UnitTextOverlay.pas',
  UnitModel in 'UnitModel.pas',
  UnitScreenBlank in 'UnitScreenBlank.pas',
  UnitScreenMainMenu in 'UnitScreenMainMenu.pas',
  UnitScreenExit in 'UnitScreenExit.pas',
  UnitScreenExampleEmpty in 'UnitScreenExampleEmpty.pas',
  UnitScreenExampleTriangle in 'UnitScreenExampleTriangle.pas',
  UnitScreenExampleCube in 'UnitScreenExampleCube.pas',
  UnitScreenExampleDragon in 'UnitScreenExampleDragon.pas',
  UnitScreenExampleCanvas in 'UnitScreenExampleCanvas.pas',
  UnitScreenExampleGUI in 'UnitScreenExampleGUI.pas';

// {$if defined(fpc) and defined(android)}

{$if defined(fpc) and defined(android)}
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
{$ifend}

{$if defined(fpc) and defined(android) and defined(PasVulkanUseSDL2)}
procedure Java_org_libsdl_app_SDLActivity_nativeSetAssetManager(pJavaEnv:PJNIEnv;pJavaClass:jclass;pAssetManager:JObject); cdecl;
begin
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Java_org_libsdl_app_SDLActivity_nativeSetAssetManager . . .');
{$ifend}
 AndroidAssetManager:=AAssetManager_fromJava(pJavaEnv,pAssetManager);
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Java_org_libsdl_app_SDLActivity_nativeSetAssetManager . . .');
{$ifend}
end;
{$ifend}

{$if defined(PasVulkanUseSDL2)}
{$if defined(fpc) and defined(android)}
procedure Java_org_libsdl_app_SDLActivity_nativeInit(pJavaEnv:PJNIEnv;pJavaClass:jclass;pJavaObject:jobject); cdecl;
var s:string;
{$else}
procedure SDLMain;
{$ifend}
begin
{$if defined(fpc) and defined(android)}
 AndroidJavaEnv:=pJavaEnv;
 AndroidJavaClass:=pJavaClass;
 AndroidJavaObject:=pJavaObject;
 AndroidDeviceName:=TpvUTF8String(AndroidGetDeviceName);
 SDL_Android_Init(pJavaEnv,pJavaClass);
{$ifend}
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Java_org_libsdl_app_SDLActivity_nativeInit . . .');
{$ifend}
{$if defined(fpc) and defined(android)}
 try
{$ifend}
  TExampleApplication.Main;
{$if defined(fpc) and defined(android)}
 except
  on e:Exception do begin
   s:=DumpExceptionCallStack(e);
   __android_log_write(ANDROID_LOG_FATAL,'PasVulkanApplication',PAnsiChar(AnsiString(s)));
   SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,'PasVulkanApplication',PAnsiChar(AnsiString(s)),nil);
   raise;
  end;
 end;
{$ifend}
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Java_org_libsdl_app_SDLActivity_nativeInit . . .');
{$ifend}
{$if defined(fpc) and defined(android)}
 SDL_Quit;
{$ifend}
end;
{$ifend}

{$if defined(fpc) and defined(android)}
{$if defined(PasVulkanUseSDL2)}
exports JNI_OnLoad name 'JNI_OnLoad',
        JNI_OnUnload name 'JNI_OnUnload',
        Android_JNI_GetEnv name 'Android_JNI_GetEnv',
        Java_org_libsdl_app_SDLActivity_nativeSetAssetManager name 'Java_org_libsdl_app_SDLActivity_nativeSetAssetManager',
        Java_org_libsdl_app_SDLActivity_nativeInit name 'Java_org_libsdl_app_SDLActivity_nativeInit';
{$else}
procedure ANativeActivity_onCreate(aActivity:PANativeActivity;aSavedState:pointer;aSavedStateSize:TpvUint32); cdecl;
begin
 Android_ANativeActivity_onCreate(aActivity,aSavedState,aSavedStateSize,TExampleApplication);
end;

exports ANativeActivity_onCreate name 'ANativeActivity_onCreate';
{$ifend}
{$ifend}

{$if defined(fpc) and defined(Windows)}
function IsDebuggerPresent:longbool; stdcall; external 'kernel32.dll' name 'IsDebuggerPresent';
{$ifend}

{$if defined(Windows)}
function AttachConsole(dwProcessId:DWord):Bool; stdcall; external 'kernel32.dll';

const ATTACH_PARENT_PROCESS=DWORD(-1);
{$ifend}

{$if not (defined(fpc) and defined(android))}
begin
{$if defined(Windows) and not defined(Release)}
 // Workaround for a random-console-missing-issue with Delphi 10.2 Tokyo
 if (GetStdHandle(STD_OUTPUT_HANDLE)=0) and not AttachConsole(ATTACH_PARENT_PROCESS) then begin
  AllocConsole;
 end;
{$ifend}
{$if defined(PasVulkanUseSDL2)}
 SDLMain;
{$else}
 TExampleApplication.Main;
{$ifend}
{$ifdef Windows}
 if {$ifdef fpc}IsDebuggerPresent{$else}DebugHook<>0{$endif} then begin
  writeln('Press return to exit . . . ');
  readln;
 end;
{$endif}
{$if defined(PasVulkanUseSDL2)}
 SDL_Quit;
{$ifend}
{$if defined(fpc) and defined(Linux)}
 // Workaround for a segv-exception-issue with closed-source NVidia drivers on Linux at program exit
 fpkill(fpgetpid,9);
{$ifend}
{$ifend}
end.
