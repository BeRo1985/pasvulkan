unit UnitScreenMain;
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

{$scopedenums on}

{$define UseMomentBasedOrderIndependentTransparency}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasMP,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Resources,
     PasVulkan.FrameGraph,
     PasVulkan.TimerQuery,
     PasVulkan.Scene3D,
     PasVulkan.Scene3D.Renderer,
     PasVulkan.Scene3D.Renderer.Instance,
     PasVulkan.Scene3D.Renderer.SkyCubeMap;

type { TScreenMain }
     TScreenMain=class(TpvApplicationScreen)
      public
       type TCameraMode=(
             Orbit,
             FirstPerson
            );
            PCameraMode=^TCameraMode;
            { TInFlightFrameState }
            TInFlightFrameState=record
             Ready:TPasMPBool32;
            end;
            PInFlightFrameState=^TInFlightFrameState;
            TInFlightFrameStates=array[0..MaxInFlightFrames+1] of TInFlightFrameState;
            PInFlightFrameStates=^TInFlightFrameStates;
      private
       fWidth:TpvInt32;
       fHeight:TpvInt32;
       fCountInFlightFrames:TpvSizeInt;
       fInFlightFrameStates:TInFlightFrameStates;
       fScene3D:TpvScene3D;
       fRenderer:TpvScene3DRenderer;
       fRendererInstance:TpvScene3DRendererInstance;
       fPrimaryDirectionalLight:TpvScene3D.TLight;
       fGroup:TpvScene3D.TGroup;
       fGroupInstance:TpvScene3D.TGroup.TInstance;
       fTime:Double;
       fCameraMode:TCameraMode;
       fCameraRotationX:TpvScalar;
       fCameraRotationY:TpvScalar;
       fZoom:TpvScalar;
       fCameraIndex:TpvSizeInt;
       fCameraMatrix:TpvMatrix4x4;
       fCameraSpeed:TpvScalar;
       fUpdateLock:TPasMPCriticalSection;
       fAnimationIndex:TpvInt32;
       fKeyLeft:boolean;
       fKeyRight:boolean;
       fKeyForwards:boolean;
       fKeyBackwards:boolean;
       fKeyUp:boolean;
       fKeyDown:boolean;
       fKeyPitchInc:boolean;
       fKeyPitchDec:boolean;
       fKeyYawInc:boolean;
       fKeyYawDec:boolean;
       fKeyRollInc:boolean;
       fKeyRollDec:boolean;
       fOldFPS:TpvInt32;
       fFPSTimeAccumulator:TpvDouble;
       fFrameTimeString:string;
      public

       constructor Create; override;

       destructor Destroy; override;

       procedure Show; override;

       procedure Hide; override;

       procedure Resume; override;

       procedure Pause; override;

       procedure Resize(const aWidth,aHeight:TpvInt32); override;

       procedure AfterCreateSwapChain; override;

       procedure BeforeDestroySwapChain; override;

       function CanBeParallelProcessed:boolean; override;

       procedure Update(const aDeltaTime:TpvDouble); override;

       function IsReadyForDrawOfInFlightFrameIndex(const aInFlightFrameIndex:TpvInt32):boolean; override;

       procedure DrawUpdate(const aInFlightFrameIndex:TpvInt32;const aFrameCounter:TpvInt64;const aDeltaTime:TpvDouble);

       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;

       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;

       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;

       function Scrolled(const aRelativeAmount:TpvVector2):boolean; override;

       function DragDropFileEvent(aFileName:TpvUTF8String):boolean; override;

       procedure OnFinish(const aResource:TpvResource;const aSuccess:boolean);

       procedure LoadGLTF(const aFileName:TpvUTF8String);

     end;

implementation

uses PasGLTF,
     UnitApplication,
     PasVulkan.Frustum;

{ TScreenMain }

constructor TScreenMain.Create;
var Center,Bounds:TpvVector3;
    CameraRotationX,CameraRotationY:TpvScalar;
begin
 inherited Create;

 fCountInFlightFrames:=pvApplication.CountInFlightFrames;

 fOldFPS:=-1;

 fFPSTimeAccumulator:=0;

 fAnimationIndex:=0;

 fCameraMode:=TCameraMode.Orbit;

 fKeyLeft:=false;
 fKeyRight:=false;
 fKeyForwards:=false;
 fKeyBackwards:=false;
 fKeyUp:=false;
 fKeyDown:=false;
 fKeyPitchInc:=false;
 fKeyPitchDec:=false;
 fKeyYawInc:=false;
 fKeyYawDec:=false;
 fKeyRollInc:=false;
 fKeyRollDec:=false;

 fUpdateLock:=TPasMPCriticalSection.Create;

 fScene3D:=TpvScene3D.Create(pvApplication.ResourceManager,nil,pvApplication.VulkanDevice,TpvScene3DRenderer.CheckBufferDeviceAddress(pvApplication.VulkanDevice),fCountInFlightFrames);

 fPrimaryDirectionalLight:=TpvScene3D.TLight.Create(fScene3D);
 fPrimaryDirectionalLight.Type_:=TpvScene3D.TLightData.TType.PrimaryDirectional;
 fPrimaryDirectionalLight.Color:=TpvVector3.InlineableCreate(1.7,1.15,0.70);
 fPrimaryDirectionalLight.Matrix:=TpvMatrix4x4.CreateConstructZ(-fScene3D.PrimaryLightDirection);
 fPrimaryDirectionalLight.Intensity:=1.0;
 fPrimaryDirectionalLight.Range:=0.0;
 fPrimaryDirectionalLight.CastShadows:=true;
 fPrimaryDirectionalLight.Data.Visible:=true;
 fPrimaryDirectionalLight.Visible:=true;
 fPrimaryDirectionalLight.Update;

 fScene3D.Upload;

 fRenderer:=TpvScene3DRenderer.Create(fScene3D,pvApplication.VulkanDevice,pvApplication.VulkanPipelineCache,fCountInFlightFrames);
 fRenderer.AntialiasingMode:=UnitApplication.Application.AntialiasingMode;
 fRenderer.ShadowMode:=UnitApplication.Application.ShadowMode;
 fRenderer.TransparencyMode:=UnitApplication.Application.TransparencyMode;
 fRenderer.DepthOfFieldMode:=UnitApplication.Application.DepthOfFieldMode;
 fRenderer.MaxMSAA:=UnitApplication.Application.MaxMSAA;
 fRenderer.MaxShadowMSAA:=UnitApplication.Application.MaxShadowMSAA;
 fRenderer.ShadowMapSize:=UnitApplication.Application.ShadowMapSize;
 fRenderer.Prepare;

 fRenderer.AcquirePersistentResources;

 fRendererInstance:=TpvScene3DRendererInstance.Create(fRenderer,UnitApplication.Application.VirtualReality);

 fRendererInstance.Prepare;

 fRendererInstance.AcquirePersistentResources;

 Center:=TpvVector3.InlineableCreate(0.0,0.0,0.0);

 Bounds:=TpvVector3.InlineableCreate(10.0,10.0,10.0);

 fCameraSpeed:=1.0;

 CameraRotationX:=0.0;
 CameraRotationY:=0.0;

 fCameraIndex:=-1;

 fCameraMatrix:=TpvMatrix4x4.CreateLookAt(Center+(TpvVector3.Create(sin(CameraRotationX*PI*2.0)*cos(-CameraRotationY*PI*2.0),
                                                                     sin(-CameraRotationY*PI*2.0),
                                                                     cos(CameraRotationX*PI*2.0)*cos(-CameraRotationY*PI*2.0)).Normalize*
                                                           (Max(Max(Bounds[0],Bounds[1]),Bounds[2])*2.0*1.0)),
                                           Center,
                                           TpvVector3.Create(0.0,1.0,0.0)).SimpleInverse;

 if {(fFrameGraph.DrawFrameIndex>=fFrameGraph.CountInFlightFrames) and} (length(GLTFFileName)>0) then begin
  try
   LoadGLTF(GLTFFileName);
  finally
   GLTFFileName:='';
  end;
 end;

 FillChar(fInFlightFrameStates,SizeOf(TInFlightFrameStates),#0);

end;

destructor TScreenMain.Destroy;
var Index:TpvSizeInt;
begin
 fRendererInstance.ReleasePersistentResources;
 FreeAndNil(fRendererInstance);
 fRenderer.ReleasePersistentResources;
 FreeAndNil(fRenderer);
 fScene3D.Unload;
 FreeAndNil(fGroupInstance);
 FreeAndNil(fGroup);
 FreeAndNil(fPrimaryDirectionalLight);
 FreeAndNil(fScene3D);
 FreeAndNil(fUpdateLock);
 inherited Destroy;
end;

procedure TScreenMain.Show;
var Index:TpvSizeInt;
    Stream:TStream;
    GraphicsQueue:TpvVulkanQueue;
    GraphicsCommandPool:TpvVulkanCommandPool;
    GraphicsCommandBuffer:TpvVulkanCommandBuffer;
    GraphicsFence:TpvVulkanFence;
begin

 inherited Show;

 fTime:=0.0;

 fCameraRotationX:=0.0;//frac(fTime*0.03125);

 fCameraRotationY:=0.0;

 fZoom:=1.0;

 pvApplication.SkipNextDrawFrame:=true;

 FillChar(fInFlightFrameStates,SizeOf(TInFlightFrameStates),#0);

end;

procedure TScreenMain.Hide;
begin
 inherited Hide;
end;

procedure TScreenMain.Resume;
begin
 inherited Resume;
 pvApplication.SkipNextDrawFrame:=true;
end;

procedure TScreenMain.Pause;
begin
 inherited Pause;
end;

procedure TScreenMain.Resize(const aWidth,aHeight:TpvInt32);
begin
 inherited Resize(aWidth,aHeight);
 pvApplication.SkipNextDrawFrame:=true;
end;

procedure TScreenMain.AfterCreateSwapChain;
var Index:TpvSizeInt;
begin

 inherited AfterCreateSwapChain;

 if assigned(UnitApplication.Application.VirtualReality) then begin

  fWidth:=UnitApplication.Application.VirtualReality.Width;

  fHeight:=UnitApplication.Application.VirtualReality.Height;

 end else begin

  fWidth:=pvApplication.VulkanSwapChain.Width;

  fHeight:=pvApplication.VulkanSwapChain.Height;

 end;

 fRendererInstance.Width:=fWidth;
 fRendererInstance.Height:=fHeight;

 fRendererInstance.AcquireVolatileResources;

 pvApplication.SkipNextDrawFrame:=true;

end;

procedure TScreenMain.BeforeDestroySwapChain;
var Index:TpvSizeInt;
begin
 fRendererInstance.ReleaseVolatileResources;
 inherited BeforeDestroySwapChain;
end;

function TScreenMain.CanBeParallelProcessed:boolean;
begin
 result:=false;
end;

procedure TScreenMain.Update(const aDeltaTime:TpvDouble);
const Directions:array[boolean,boolean] of TpvScalar=
       (
        (0,1),
        (-1,0)
       );
var RotationSpeed,MovementSpeed:TpvDouble;
    FPS:TpvInt32;
    FPSString:string;
    FrameTime:TpvDouble;
begin

 RotationSpeed:=aDeltaTime*1.0;
 MovementSpeed:=aDeltaTime*1.0*fCameraSpeed;

 if fKeyPitchInc or fKeyPitchDec or fKeyYawInc or fKeyYawDec or fKeyRollInc or fKeyRollDec then begin
  fCameraMatrix:=(TpvMatrix4x4.CreateFromQuaternion(TpvQuaternion.CreateFromEuler(TpvVector3.Create(Directions[fKeyPitchInc,fKeyPitchDec],
                                                                                                    Directions[fKeyYawDec,fKeyYawInc],
                                                                                                    Directions[fKeyRollInc,fKeyRollDec])*-RotationSpeed).Normalize)*fCameraMatrix).OrthoNormalize;
 end;
 if fKeyLeft or fKeyRight or fKeyForwards or fKeyBackwards or fKeyUp or fKeyDown then begin
  fCameraMatrix:=fCameraMatrix*
                 TpvMatrix4x4.CreateTranslation((fCameraMatrix.ToMatrix3x3*
                                                 TpvVector3.InlineableCreate(Directions[fKeyLeft,fKeyRight],
                                                                             Directions[fKeyDown,fKeyUp],
                                                                             Directions[fKeyForwards,fKeyBackwards]))*MovementSpeed);
 end;

 inherited Update(aDeltaTime);

 fRendererInstance.Update(pvApplication.UpdateInFlightFrameIndex,pvApplication.UpdateFrameCounter);

 FPS:=round(pvApplication.FramesPerSecond*100.0);
 fFPSTimeAccumulator:=fFPSTimeAccumulator+aDeltaTime;
 if (fFPSTimeAccumulator>=0.25) or (length(fFrameTimeString)=0) then begin
  fFPSTimeAccumulator:=frac(fFPSTimeAccumulator*4)*0.25;
  fOldFPS:=Low(Int32);
  if assigned(fRendererInstance.FrameGraph.LastTimerQueryResults) then begin
   FrameTime:=fRendererInstance.FrameGraph.LastTimerQueryResults[fRendererInstance.FrameGraph.LastTimerQueryResults.Count-1].Duration;
  end else begin
   FrameTime:=0.0;
  end;
  Str(FrameTime:1:5,fFrameTimeString);
 end;

 if abs(fOldFPS-FPS)>=100 then begin
  fOldFPS:=FPS;
  str((FPS*0.01):4:2,FPSString);
  pvApplication.WindowTitle:=pvApplication.Title+' ['+FPSString+' FPS] ['+fFrameTimeString+' ms frame time]';
 end;

//DrawUpdate(pvApplication.UpdateInFlightFrameIndex,pvApplication.DeltaTime);

end;

function TScreenMain.IsReadyForDrawOfInFlightFrameIndex(const aInFlightFrameIndex:TpvInt32):boolean;
begin
 result:=TPasMPInterlocked.Read(fInFlightFrameStates[aInFlightFrameIndex].Ready);
end;

procedure TScreenMain.DrawUpdate(const aInFlightFrameIndex:TpvInt32;const aFrameCounter:TpvInt64;const aDeltaTime:TpvDouble);
var Index:TpvSizeInt;
    ModelMatrix,CameraMatrix,ViewMatrix,ProjectionMatrix:TpvMatrix4x4;
    Center,Bounds:TpvVector3;
    t0,t1:Double;
    View:TpvScene3D.TView;
    InFlightFrameState:PInFlightFrameState;
    BlendFactor,Factor:single;
begin

 InFlightFrameState:=@fInFlightFrameStates[aInFlightFrameIndex];

 begin

  fUpdateLock.Acquire;
  try

   ModelMatrix:=TpvMatrix4x4.Identity; // TpvMatrix4x4.CreateRotate(State^.AnglePhases[0]*TwoPI,TpvVector3.Create(0.0,0.0,1.0))*TpvMatrix4x4.CreateRotate(State^.AnglePhases[1]*TwoPI,TpvVector3.Create(0.0,1.0,0.0));

   if assigned(fGroupInstance) then begin

    fGroupInstance.ModelMatrix:=ModelMatrix;

    begin
     BlendFactor:=1.0-exp(-(pvApplication.DeltaTime*4.0));
     for Index:=-1 to fGroupInstance.Group.Animations.Count-1 do begin
      Factor:=fGroupInstance.Automations[Index].Factor;
      if Index=fAnimationIndex then begin
       if Factor<0.0 then begin
        Factor:=0.0;
        fGroupInstance.Automations[Index].ShadowTime:=0.0;
       end;
       Factor:=(Factor*(1.0-BlendFactor))+(1.0*BlendFactor);
      end else if Factor>0.0 then begin
       Factor:=Factor*(1.0-BlendFactor);
       if Factor<1e-5 then begin
        Factor:=-1.0;
       end;
      end;
      if Factor>0.0 then begin
       if Index>=0 then begin
        t0:=fGroupInstance.Group.Animations[Index].GetAnimationBeginTime;
        t1:=fGroupInstance.Group.Animations[Index].GetAnimationEndTime;
        fGroupInstance.Automations[Index].Time:=fGroupInstance.Automations[Index].ShadowTime+t0;
        fGroupInstance.Automations[Index].ShadowTime:=ModuloPos(fGroupInstance.Automations[Index].ShadowTime+pvApplication.DeltaTime,t1-t0);
        fGroupInstance.Automations[Index].Complete:=true;
       end else begin
        fGroupInstance.Automations[Index].Time:=0.0;
        fGroupInstance.Automations[Index].Complete:=false;
       end;
      end else begin
       fGroupInstance.Automations[Index].Time:=0.0;
      end;
      fGroupInstance.Automations[Index].Factor:=Factor;
     end;
    end;

   end;

   fScene3D.Update(aInFlightFrameIndex);

   fScene3D.TransferViewsToPreviousViews;

   fScene3D.ClearViews;

   Center:=(fScene3D.BoundingBox.Min+fScene3D.BoundingBox.Max)*0.5;

   Bounds:=(fScene3D.BoundingBox.Max-fScene3D.BoundingBox.Min)*0.5;

   case fCameraMode of
    TCameraMode.FirstPerson:begin
     ViewMatrix:=fCameraMatrix.SimpleInverse;//TpvMatrix4x4.CreateTranslation(-fCameraPosition)*TpvMatrix4x4.CreateFromQuaternion(fCameraOrientation);
    end;
    else begin
     ViewMatrix:=TpvMatrix4x4.CreateLookAt(Center+(TpvVector3.Create(sin(fCameraRotationX*PI*2.0)*cos(-fCameraRotationY*PI*2.0),
                                                                     sin(-fCameraRotationY*PI*2.0),
                                                                     cos(fCameraRotationX*PI*2.0)*cos(-fCameraRotationY*PI*2.0)).Normalize*
                                                           (Max(Max(Bounds[0],Bounds[1]),Bounds[2])*2.0*fZoom)),
                                           Center,
                                           TpvVector3.Create(0.0,1.0,0.0));//*TpvMatrix4x4.FlipYClipSpace;
     fCameraMatrix:=ViewMatrix.SimpleInverse;
    end;
   end;

   fScene3D.ResetRenderPasses;

   fRendererInstance.Reset;

   if assigned(fGroup) and
      assigned(fGroupInstance) and
      (fGroup.CameraNodeIndices.Count>0) and
      (fCameraIndex>=0) and
      (fCameraIndex<fGroup.CameraNodeIndices.Count) then begin
    if fGroupInstance.GetCamera(fGroup.CameraNodeIndices[fCameraIndex],
                                CameraMatrix,
                                ViewMatrix,
                                ProjectionMatrix,
                                true,
                                true,
                                nil,
                                nil,
                                -(fRendererInstance.Width/fRendererInstance.Height)) then begin
     fRendererInstance.CameraMatrix:=CameraMatrix;
     if not assigned(UnitApplication.Application.VirtualReality) then begin
      View.ViewMatrix:=ViewMatrix;
      View.ProjectionMatrix:=ProjectionMatrix;
      View.InverseViewMatrix:=ViewMatrix.Inverse;
      View.InverseProjectionMatrix:=ProjectionMatrix.Inverse;
      fRendererInstance.AddView(View);
     end;
    end else begin
     fRendererInstance.CameraMatrix:=fCameraMatrix;
    end;
   end else begin
    fRendererInstance.CameraMatrix:=fCameraMatrix;
   end;

   fRendererInstance.DrawUpdate(aInFlightFrameIndex,aFrameCounter);

   fScene3D.UpdateViews(aInFlightFrameIndex);

   TPasMPInterlocked.Write(InFlightFrameState^.Ready,true);

   fTime:=fTime+pvApplication.DeltaTime;

  finally
   fUpdateLock.Release;
  end;

 end;

end;

procedure TScreenMain.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
var InFlightFrameState:TScreenMain.PInFlightFrameState;
    InFlightFrameIndex:Int32;
begin

 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);

 InFlightFrameIndex:=pvApplication.DrawInFlightFrameIndex;

 InFlightFrameState:=@fInFlightFrameStates[InFlightFrameIndex];

 DrawUpdate(InFlightFrameIndex,pvApplication.DrawFrameCounter,pvApplication.DeltaTime);

 fRenderer.Flush(InFlightFrameIndex,aWaitSemaphore);

 fRendererInstance.Draw(pvApplication.SwapChainImageIndex,
                        pvApplication.DrawInFlightFrameIndex,
                        pvApplication.DrawFrameCounter,
                        aWaitSemaphore,
                        aWaitFence);

 TPasMPInterlocked.Write(InFlightFrameState^.Ready,false);

end;

function TScreenMain.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
var MaxLen:TpvSizeInt;
    Result_:TpvTimerQuery.TResult;
begin
 result:=inherited KeyEvent(aKeyEvent);
 if aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down then begin
  case aKeyEvent.KeyCode of
   KEYCODE_ESCAPE:begin
    pvApplication.Terminate;
   end;
   KEYCODE_F8:begin
    if assigned(fRendererInstance.FrameGraph.LastTimerQueryResults) then begin
     writeln('=================================================');
     MaxLen:=1;
     for Result_ in fRendererInstance.FrameGraph.LastTimerQueryResults do begin
      if Result_.Valid then begin
       MaxLen:=Max(MaxLen,length(Result_.Name));
      end;
     end;
     for Result_ in fRendererInstance.FrameGraph.LastTimerQueryResults do begin
      if Result_.Valid then begin
       writeln(Result_.Name:MaxLen,': ',Result_.Duration:1:5,' ms');
      end;
     end;
    end;
   end;
   KEYCODE_U:begin
    fCameraSpeed:=fCameraSpeed*0.5;
   end;
   KEYCODE_I:begin
    fCameraSpeed:=fCameraSpeed*2.0;
   end;
   KEYCODE_O:begin
    fCameraMode:=TCameraMode.Orbit;
   end;
   KEYCODE_P:begin
    fCameraMode:=TCameraMode.FirstPerson;
   end;
   KEYCODE_L:begin
    pvApplication.CatchMouse:=not pvApplication.CatchMouse;
   end;
   KEYCODE_V,KEYCODE_B:begin
    if assigned(fGroupInstance) then begin
     if fAnimationIndex<0 then begin
      fAnimationIndex:=fGroupInstance.Group.Animations.Count-1;
     end else begin
      dec(fAnimationIndex);
     end;
    end;
   end;
   KEYCODE_N,KEYCODE_M:begin
    if assigned(fGroupInstance) then begin
     inc(fAnimationIndex);
     if fAnimationIndex>=fGroupInstance.Group.Animations.Count then begin
      fAnimationIndex:=-1;
     end;
    end;
   end;
   KEYCODE_0..KEYCODE_9:begin
    if assigned(fGroupInstance) then begin
     if ((aKeyEvent.KeyCode-(KEYCODE_0+1))>=-1) and ((aKeyEvent.KeyCode-(KEYCODE_0+1))<fGroupInstance.Group.Animations.Count) then begin
      fAnimationIndex:=aKeyEvent.KeyCode-(KEYCODE_0+1);
     end;
    end;
   end;
   KEYCODE_BACKSPACE:begin
    if abs(PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^.y*PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.y)<0.5 then begin
     PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.x:=0.0;
     PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.y:=1.0;
     PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.z:=0.0;
    end else begin
     PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^).Normalize;
    end;
    PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^).Normalize;
    PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^).Normalize;
    PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^).Normalize;
    fCameraMatrix:=fCameraMatrix.RobustOrthoNormalize;
   end;
  end;
 end;
 if aKeyEvent.KeyEventType in [TpvApplicationInputKeyEventType.Down,TpvApplicationInputKeyEventType.Up] then begin
  case aKeyEvent.KeyCode of
   KEYCODE_LEFT,KEYCODE_A:begin
    fKeyLeft:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_RIGHT,KEYCODE_S:begin
    fKeyRight:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_UP,KEYCODE_W:begin
    fKeyForwards:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_DOWN,KEYCODE_D:begin
    fKeyBackwards:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_PAGEUP,KEYCODE_R:begin
    fKeyUp:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_PAGEDOWN,KEYCODE_F:begin
    fKeyDown:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_T:begin
    fKeyPitchInc:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_G:begin
    fKeyPitchDec:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_E:begin
    fKeyYawInc:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_Q:begin
    fKeyYawDec:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_X:begin
    fKeyRollInc:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_C:begin
    fKeyRollDec:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_HOME:begin
    if (aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down) and assigned(fGroup) then begin
     inc(fCameraIndex);
     if fCameraIndex>=fGroup.CameraNodeIndices.Count then begin
      fCameraIndex:=-1;
     end;
    end;
   end;
   KEYCODE_END:begin
    if (aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down) and assigned(fGroup) then begin
     dec(fCameraIndex);
     if fCameraIndex<=-2 then begin
      fCameraIndex:=fGroup.CameraNodeIndices.Count-1;
     end;
    end;
   end;
  end;
 end;
end;

function TScreenMain.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
begin
 result:=inherited PointerEvent(aPointerEvent);
 if not result then begin
  if (aPointerEvent.PointerEventType=TpvApplicationInputPointerEventType.Motion) and
     (pvApplication.CatchMouse or (TpvApplicationInputPointerButton.Left in aPointerEvent.Buttons)) then begin
   fUpdateLock.Acquire;
   try
    case fCameraMode of
     TCameraMode.FirstPerson:begin
      fCameraMatrix:=TpvMatrix4x4.CreateFromQuaternion(TpvQuaternion.CreateFromEuler(TpvVector3.InlineableCreate(aPointerEvent.RelativePosition.y,aPointerEvent.RelativePosition.x,0.0)*0.002)).Transpose*fCameraMatrix;
      if abs(PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^.y*PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.y)<0.5 then begin
       PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.x:=0.0;
       PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.y:=1.0;
       PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.z:=0.0;
      end else begin
       PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^).Normalize;
      end;
      PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^).Normalize;
      PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^).Normalize;
      PpvVector3(pointer(@fCameraMatrix.RawComponents[2,0]))^:=PpvVector3(pointer(@fCameraMatrix.RawComponents[0,0]))^.Cross(PpvVector3(pointer(@fCameraMatrix.RawComponents[1,0]))^).Normalize;
      fCameraMatrix:=fCameraMatrix.RobustOrthoNormalize;
     end;
     else begin
      fCameraRotationX:=frac(fCameraRotationX+(1.0-(aPointerEvent.RelativePosition.x*(1.0/pvApplication.VulkanSwapChain.Width))));
      fCameraRotationY:=frac(fCameraRotationY+(1.0-(aPointerEvent.RelativePosition.y*(1.0/pvApplication.VulkanSwapChain.Height))));
     end;
    end;
   finally
    fUpdateLock.Release;
   end;
   result:=true;
  end;
 end;
end;

function TScreenMain.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 result:=inherited Scrolled(aRelativeAmount);
 if not result then begin
  fUpdateLock.Acquire;
  try
   case fCameraMode of
    TCameraMode.FirstPerson:begin
     fCameraMatrix:=fCameraMatrix*TpvMatrix4x4.CreateTranslation((fCameraMatrix.ToMatrix3x3*TpvVector3.ZAxis).Normalize*(aRelativeAmount.x+aRelativeAmount.y)*fCameraSpeed);
    end;
    else begin
     fZoom:=Max(1e-4,fZoom+((aRelativeAmount.x+aRelativeAmount.y)*0.1));
    end;
   end;
  finally
   fUpdateLock.Release;
  end;
  result:=true;
 end;
end;

function TScreenMain.DragDropFileEvent(aFileName:TpvUTF8String):boolean;
begin
 LoadGLTF(aFileName);
 result:=true;
end;

procedure TScreenMain.OnFinish(const aResource:TpvResource;const aSuccess:boolean);
var Center,Bounds:TpvVector3;
    CameraRotationX,CameraRotationY:TpvScalar;
begin

 if assigned(aResource) and (aResource is TpvScene3D.TGroup) then begin

  if assigned(fGroupInstance) then begin
   fGroupInstance.DeferredFree;
   fGroupInstance:=nil;
  end;

  if assigned(fGroup) then begin
   fGroup.DeferredFree;
   fGroup:=nil;
  end;

  fGroup:=TpvScene3D.TGroup(aResource);

  fGroupInstance:=fGroup.CreateInstance;

  fCameraIndex:=-1;

  if assigned(fGroup) then begin

   Center:=(fGroup.BoundingBox.Min+fGroup.BoundingBox.Max)*0.5;

   Bounds:=(fGroup.BoundingBox.Max-fGroup.BoundingBox.Min)*0.5;

   fCameraSpeed:=Max(1.0,fGroup.BoundingBox.Radius)*0.1;

  end else begin

   Center:=TpvVector3.InlineableCreate(0.0,0.0,0.0);

   Bounds:=TpvVector3.InlineableCreate(10.0,10.0,10.0);

   fCameraSpeed:=1.0;

  end;

  CameraRotationX:=0.0;
  CameraRotationY:=0.0;

  fZoom:=1.0;

  fCameraMatrix:=TpvMatrix4x4.CreateLookAt(Center+(TpvVector3.Create(sin(CameraRotationX*PI*2.0)*cos(-CameraRotationY*PI*2.0),
                                                                      sin(-CameraRotationY*PI*2.0),
                                                                      cos(CameraRotationX*PI*2.0)*cos(-CameraRotationY*PI*2.0)).Normalize*
                                                            (Max(Max(Bounds[0],Bounds[1]),Bounds[2])*2.0*1.0)),
                                            Center,
                                            TpvVector3.Create(0.0,1.0,0.0)).SimpleInverse;

  fCameraRotationX:=0.0;
  fCameraRotationY:=0.0;

 end;

end;

procedure TScreenMain.LoadGLTF(const aFileName:TpvUTF8String);
begin

 if assigned(fGroupInstance) then begin
  fGroupInstance.DeferredFree;
  fGroupInstance:=nil;
 end;

 if assigned(fGroup) then begin
  fGroup.DeferredFree;
  fGroup:=nil;
 end;

 pvApplication.ResourceManager.BackgroundLoadResource(TpvScene3D.TGroup,aFileName,OnFinish,fScene3D);

 pvApplication.SetFocus;

end;

end.
