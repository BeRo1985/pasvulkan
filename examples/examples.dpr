{$if defined(fpc) and defined(android)}library{$else}program{$ifend} examples;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$if defined(win32) or defined(win64)}
 {$apptype console}
 {$define Windows}
{$ifend}

//{$if defined(fpc) and defined(Unix)}cthreads,{$ifend}

uses
  {$if defined(fpc) and defined(Unix)}cthreads,{$ifend}
  SysUtils,
  Classes,
  PasMP in '..\externals\pasmp\src\PasMP.pas',
  Vulkan in '..\src\Vulkan.pas',
  PasVulkan in '..\src\PasVulkan.pas',
  PasVulkanAndroid in '..\src\PasVulkanAndroid.pas',
  PasVulkanSDL2 in '..\src\PasVulkanSDL2.pas',
  PasVulkanStaticLinking in '..\src\PasVulkanStaticLinking.pas',
  PasVulkanApplication in '..\src\PasVulkanApplication.pas',
  UnitTextOverlay in 'UnitTextOverlay.pas',
  UnitScreenExampleTriangle in 'UnitScreenExampleTriangle.pas',
  UnitSDFFont in 'UnitSDFFont.pas';

type TExampleVulkanApplication=class(TVulkanApplication)
      private
       fTextOverlay:TTextOverlay;
      public
       function HandleEvent(const pEvent:TSDL_Event):boolean; override;
       procedure Start; override;
       procedure Stop; override;
       procedure AfterCreateSwapChain; override;
       procedure BeforeDestroySwapChain; override;
       procedure Resume; override;
       procedure Pause; override;
       procedure Draw; override;
     end;

function TExampleVulkanApplication.HandleEvent(const pEvent:TSDL_Event):boolean;
const QuitMsgBoxButtons:array[0..1] of TSDL_MessageBoxButtonData=
{$ifdef Windows}
       ((flags:SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT;buttonid:0;text:'No'),
        (flags:SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT;buttonid:1;text:'Yes'));
{$else}
       ((flags:SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT;buttonid:1;text:'Yes'),
        (flags:SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT;buttonid:0;text:'No'));
{$endif}
      QuitMsgBoxColorScheme:TSDL_MessageBoxColorScheme=
       (colors:((r:0;g:0;b:0),
                (r:255;g:255;b:255),
                (r:192;g:192;b:192),
                (r:64;g:64;b:64),
                (r:128;g:128;b:128)));
      QuitMsgBoxData:TSDL_MessageBoxData=
       (
        flags:SDL_MESSAGEBOX_WARNING;
        window:nil;
        title:'PasVulkanApplication';
        message:'Are you sure to exit?';
        numbuttons:length(QuitMsgBoxButtons);
        buttons:@QuitMsgBoxButtons[0];
        colorScheme:@QuitMsgBoxColorScheme;
       );
var QuitMsgBoxDataButtonID:TSDLInt32;
begin
 result:=false;
 case pEvent.type_ of
  SDL_KEYDOWN:begin
{$if defined(fpc) and defined(android)}
   __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication',PAnsiChar(AnsiString('Keydown: '+IntToStr(pEvent.key.keysym.sym))));
{$ifend}
   case pEvent.key.keysym.sym of
{$ifdef Android}SDLK_AC_BACK,{$endif}SDLK_ESCAPE:begin
     QuitMsgBoxDataButtonID:=-1;
     if (SDL_ShowMessageBox(@QuitMsgBoxData,@QuitMsgBoxDataButtonID)=0) and (QuitMsgBoxDataButtonID<>0) then begin
      Terminate;
     end;
     result:=true;
    end;
   end;
  end;
 end;
 if not result then begin
  result:=inherited HandleEvent(pEvent);
 end;
end;

procedure TExampleVulkanApplication.Start;
begin
 fTextOverlay:=TTextOverlay.Create;
 fTextOverlay.Load;
end;

procedure TExampleVulkanApplication.Stop;
begin
 fTextOverlay.Unload;
 FreeAndNil(fTextOverlay);
end;

procedure TExampleVulkanApplication.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;
 fTextOverlay.AfterCreateSwapChain;
end;

procedure TExampleVulkanApplication.BeforeDestroySwapChain;
begin
 inherited BeforeDestroySwapChain;
 fTextOverlay.BeforeDestroySwapChain;
end;

procedure TExampleVulkanApplication.Resume;
begin
 inherited Resume;
 fTextOverlay.Load;
end;

procedure TExampleVulkanApplication.Pause;
begin
 fTextOverlay.Unload;
 inherited Pause;
end;

procedure TExampleVulkanApplication.Draw;
begin
 inherited Draw;
end;

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
 SDL_Android_Init(pJavaEnv,pJavaClass);
{$ifend}
{$if (defined(fpc) and defined(android)) and not defined(Release)}
 __android_log_write(ANDROID_LOG_VERBOSE,'PasVulkanApplication','Entering Java_org_libsdl_app_SDLActivity_nativeInit . . .');
{$ifend}
{$if defined(fpc) and defined(android)}
 try
{$ifend}
  VulkanApplication:=TExampleVulkanApplication.Create;
  try
 {$ifndef Release}
   VulkanApplication.VulkanDebugging:=true;
   VulkanApplication.VulkanValidation:=true;
 {$endif}
   VulkanApplication.Title:='SDL Vulkan Examples Application';
   VulkanApplication.PathName:='SDLVulkanExamplesApplication';
   VulkanApplication.StartScreen:=TScreenExampleTriangle;
   VulkanApplication.VisibleMouseCursor:=true;
   VulkanApplication.CatchMouse:=false;
   VulkanApplication.HideSystemBars:=true;
   VulkanApplication.Run;
  finally
   FreeAndNil(VulkanApplication);
  end;
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

begin
{$if defined(fpc) and defined(android)}
{$else}
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
