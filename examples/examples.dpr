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

//{$if defined(fpc) and defined(Unix)}cthreads,{$ifend}

uses
  {$if defined(fpc) and defined(Unix)}
  cthreads,
  {$ifend}
  SysUtils,
  Classes,
  PasDblStrUtils in '..\externals\pasdblstrutils\src\PasDblStrUtils.pas',
  PUCU in '..\externals\pucu\src\PUCU.pas',
  PasMP in '..\externals\pasmp\src\PasMP.pas',
  kraft in '..\externals\kraft\src\kraft.pas',
  Vulkan in '..\src\Vulkan.pas',
  PasVulkan.Math in '..\src\PasVulkan.Math.pas',
  PasVulkan.Types in '..\src\PasVulkan.Types.pas',
  PasVulkan.Utils in '..\src\PasVulkan.Utils.pas',
  PasVulkan.Collections in '..\src\PasVulkan.Collections.pas',
  PasVulkan.JSON in '..\src\PasVulkan.JSON.pas',
  PasVulkan.XML in '..\src\PasVulkan.XML.pas',
  PasVulkan.TrueTypeFont in '..\src\PasVulkan.TrueTypeFont.pas',
  PasVulkan.Streams in '..\src\PasVulkan.Streams.pas',
  PasVulkan.Assets in '..\src\PasVulkan.Assets.pas',
  PasVulkan.Compression.Deflate in '..\src\PasVulkan.Compression.Deflate.pas',
  PasVulkan.Image.PNG in '..\src\PasVulkan.Image.PNG.pas',
  PasVulkan.Image.BMP in '..\src\PasVulkan.Image.BMP.pas',
  PasVulkan.Image.JPEG in '..\src\PasVulkan.Image.JPEG.pas',
  PasVulkan.Image.TGA in '..\src\PasVulkan.Image.TGA.pas',
  PasVulkan.Framework in '..\src\PasVulkan.Framework.pas',
  PasVulkan.Sprites in '..\src\PasVulkan.Sprites.pas',
  PasVulkan.Canvas in '..\src\PasVulkan.Canvas.pas',
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
  UnitSDFFont in 'UnitSDFFont.pas',
  UnitScreenBlank in 'UnitScreenBlank.pas',
  UnitScreenMainMenu in 'UnitScreenMainMenu.pas',
  UnitScreenExit in 'UnitScreenExit.pas',
  UnitScreenExampleEmpty in 'UnitScreenExampleEmpty.pas',
  UnitScreenExampleTriangle in 'UnitScreenExampleTriangle.pas',
  UnitScreenExampleCube in 'UnitScreenExampleCube.pas',
  UnitScreenExampleDragon in 'UnitScreenExampleDragon.pas',
  UnitScreenExampleCanvas in 'UnitScreenExampleCanvas.pas';

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

{$if defined(fpc) and defined(android)}
exports JNI_OnLoad name 'JNI_OnLoad',
        JNI_OnUnload name 'JNI_OnUnload',
        Android_JNI_GetEnv name 'Android_JNI_GetEnv',
        Java_org_libsdl_app_SDLActivity_nativeSetAssetManager name 'Java_org_libsdl_app_SDLActivity_nativeSetAssetManager',
        Java_org_libsdl_app_SDLActivity_nativeInit name 'Java_org_libsdl_app_SDLActivity_nativeInit',
        ANativeActivity_onCreate name 'ANativeActivity_onCreate';
{$ifend}

{$if defined(fpc) and defined(Windows)}
function IsDebuggerPresent:longbool; stdcall; external 'kernel32.dll' name 'IsDebuggerPresent';
{$ifend}

{$if not (defined(fpc) and defined(android))}
begin
 SDLMain;
{$ifdef Windows}
 if {$ifdef fpc}IsDebuggerPresent{$else}DebugHook<>0{$endif} then begin
  writeln('Press return to exit . . . ');
  readln;
 end;
{$endif}
 SDL_Quit;
{$ifend}
end.
