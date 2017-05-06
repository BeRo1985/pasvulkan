{$if defined(fpc) and defined(android)}library{$else}program{$ifend} examples;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$if defined(win32) or defined(win64)}
 {$apptype console}
{$ifend}

uses
  {$if defined(fpc) and defined(Unix)}cthreads,{$ifend}
  SysUtils,
  Classes,
  Vulkan in '..\src\Vulkan.pas',
  PasVulkan in '..\src\PasVulkan.pas',
  PasVulkanAndroid in '..\src\PasVulkanAndroid.pas',
  PasVulkanSDL2 in '..\src\PasVulkanSDL2.pas',
  PasVulkanStaticLinking in '..\src\PasVulkanStaticLinking.pas',
  PasVulkanApplication in '..\src\PasVulkanApplication.pas',
  UnitScreenExampleTriangle in 'UnitScreenExampleTriangle.pas';

type TExampleVulkanApplication=class(TVulkanApplication)
      public
       function HandleEvent(const pEvent:TSDL_Event):boolean; override;
     end;

function TExampleVulkanApplication.HandleEvent(const pEvent:TSDL_Event):boolean;
begin
 result:=false;
 case pEvent.type_ of
  SDL_KEYDOWN:begin
   case pEvent.key.keysym.sym of
    SDLK_F4:begin
     if ((pEvent.key.keysym.modifier and ((KMOD_LALT or KMOD_RALT) or (KMOD_LMETA or KMOD_RMETA)))<>0) and (pEvent.key.repeat_=0) then begin
      Terminate;
      result:=true;
     end;
    end;
   end;
  end;
 end;
 if not result then begin
  result:=inherited HandleEvent(pEvent);
 end;
end;

{$if defined(fpc) and defined(android)}
procedure Java_org_libsdl_app_SDLActivity_nativeInit(pJavaEnv:PJNIEnv;pJavaClass:jclass;pJavaObject:jobject); cdecl;
{$else}
procedure SDLMain;
{$ifend}
begin
{$if defined(fpc) and defined(android)}
 SDL_Android_Init(pJavaEnv,pJavaClass);
{$ifend}
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Java_org_libsdl_app_SDLActivity_nativeInit . . .');
{$ifend}
 VulkanApplication:=TExampleVulkanApplication.Create;
 try
{$if defined(fpc) and defined(android)}
  VulkanApplication.JavaEnv:=pJavaEnv;
  VulkanApplication.JavaClass:=pJavaClass;
  VulkanApplication.JavaObject:=pJavaObject;
{$ifend}
{$ifndef Release}
  VulkanApplication.VulkanDebugging:=true;
  VulkanApplication.VulkanValidation:=true;
{$endif}
  VulkanApplication.Title:='SDL Vulkan Examples Application';
  VulkanApplication.StartScreen:=TScreenExampleTriangle;
  VulkanApplication.VisibleMouseCursor:=true;
  VulkanApplication.CatchMouse:=false;
  VulkanApplication.Run;
 finally
  FreeAndNil(VulkanApplication);
 end;
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Leaving Java_org_libsdl_app_SDLActivity_nativeInit . . .');
{$ifend}
end;

{$if defined(fpc) and defined(android)}
exports JNI_OnLoad name 'JNI_OnLoad',
        JNI_OnUnload name 'JNI_OnUnload',
        Android_JNI_GetEnv name 'Android_JNI_GetEnv',
        Java_org_libsdl_app_SDLActivity_nativeInit name 'Java_org_libsdl_app_SDLActivity_nativeInit',
        ANativeActivity_onCreate name 'ANativeActivity_onCreate';
{$ifend}

begin
{$if defined(fpc) and defined(android)}
{$else}
 SDLMain;
{$ifndef fpc}
 if DebugHook<>0 then begin
  writeln('Press return to exit . . . ');
  readln;
 end;
{$endif}
{$ifend}
end.
