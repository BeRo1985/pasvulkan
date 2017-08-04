unit UnitScreenExampleGUI;
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

interface

uses SysUtils,
     Classes,
     Math,
     UnitRegisteredExamplesList,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Sprites,
     PasVulkan.Canvas,
     PasVulkan.GUI,
     PasVulkan.Font,
     PasVulkan.TrueTypeFont;

type TScreenExampleGUI=class(TpvApplicationScreen)
      private
       fVulkanRenderPass:TpvVulkanRenderPass;
       fVulkanCommandPool:TpvVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxSwapChainImages-1] of TpvVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxSwapChainImages-1] of TpvVulkanSemaphore;
       fVulkanCanvas:TpvCanvas;
       fScreenToCanvasScale:TpvVector2;
       fGUIInstance:TpvGUIInstance;
       fGUIWindow:TpvGUIWindow;
       fGUILabel:TpvGUILabel;
       fGUIOtherWindow:TpvGUIWindow;
       fLastMousePosition:TpvVector2;
       fLastMouseButtons:TpvApplicationInputPointerButtons;
       fReady:boolean;
       fSelectedIndex:TpvInt32;
       fStartY:TpvFloat;
       fTime:TpvDouble;
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

       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;

       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;

       function Scrolled(const aRelativeAmount:TpvVector2):boolean; override;

       function CanBeParallelProcessed:boolean; override;

       procedure Update(const aDeltaTime:TpvDouble); override;

       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;

     end;

implementation

uses UnitExampleApplication,UnitTextOverlay,UnitScreenMainMenu;

const SpritesVertices:array[0..2,0..1,0..2] of TpvFloat=
       (((0.5,0.5,0.0),(1.0,0.0,0.0)),
        ((-0.5,0.5,0.0),(0.0,1.0,0.0)),
        ((0.0,-0.5,0.0),(0.0,0.0,1.0)));

      SpritesIndices:array[0..2] of TpvInt32=(0,1,2);

      UniformBuffer:array[0..2,0..3,0..3] of TpvFloat=
       (((1.0,0.0,0.0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0.0,1.0)),  // Projection matrix
        ((1.0,0.0,0.0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0.0,1.0)),  // Model matrix
        ((1.0,0.0,0.0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0.0,1.0))); // View matrix

      Offsets:array[0..0] of TVkDeviceSize=(0);

      FontSize=3.0;

constructor TScreenExampleGUI.Create;
begin
 inherited Create;
 fSelectedIndex:=-1;
 fReady:=false;
 fTime:=0.48;
 fLastMousePosition:=TpvVector2.Null;
 fLastMouseButtons:=[];
end;

destructor TScreenExampleGUI.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenExampleGUI.Show;
var Stream:TStream;
    Index:TpvInt32;
begin
 inherited Show;

 pvApplication.VisibleMouseCursor:=false;

 fVulkanCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                 pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                 TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
 for Index:=0 to MaxSwapChainImages-1 do begin
  fVulkanRenderCommandBuffers[Index]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fVulkanRenderSemaphores[Index]:=TpvVulkanSemaphore.Create(pvApplication.VulkanDevice);
 end;

 fVulkanRenderPass:=nil;

 fVulkanCanvas:=TpvCanvas.Create(pvApplication.VulkanDevice,
                                 pvApplication.VulkanDevice.GraphicsQueue,
                                 pvApplication.VulkanGraphicsCommandBuffers[0,0],
                                 pvApplication.VulkanGraphicsCommandBufferFences[0,0],
                                 pvApplication.VulkanDevice.TransferQueue,
                                 pvApplication.VulkanTransferCommandBuffers[0,0],
                                 pvApplication.VulkanTransferCommandBufferFences[0,0],
                                 pvApplication.VulkanPipelineCache);

 fGUIInstance:=TpvGUIInstance.Create(pvApplication.VulkanDevice);
 fGUIInstance.Canvas:=fVulkanCanvas;

 fGUIWindow:=TpvGUIWindow.Create(fGUIInstance);
 fGUIWindow.Left:=50;
 fGUIWindow.Top:=200;
 fGUIWindow.Title:='An example window';
 fGUIWindow.Layout:=TpvGUIBoxLayout.Create(fGUIWindow,pvglaLeading,pvgloVertical,8.0,8.0);

 fGUILabel:=TpvGUILabel.Create(fGUIWindow);
 fGUILabel.Caption:='An example label';

 fGUIOtherWindow:=TpvGUIWindow.Create(fGUIInstance);
 fGUIOtherWindow.Left:=550;
 fGUIOtherWindow.Top:=200;
 fGUIOtherWindow.Title:='An another example window';
 fGUIOtherWindow.Layout:=TpvGUIBoxLayout.Create(fGUIOtherWindow,pvglaLeading,pvgloVertical,8.0,8.0);

 fGUILabel:=TpvGUILabel.Create(fGUIOtherWindow);
 fGUILabel.Caption:='An other example label';

 fGUILabel:=TpvGUILabel.Create(fGUIOtherWindow);
 fGUILabel.Caption:='An another example label';

end;

procedure TScreenExampleGUI.Hide;
var Index:TpvInt32;
begin
 FreeAndNil(fGUIInstance);
 FreeAndNil(fVulkanCanvas);
 FreeAndNil(fVulkanRenderPass);
 for Index:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[Index]);
  FreeAndNil(fVulkanRenderSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);
 pvApplication.VisibleMouseCursor:=true;
 inherited Hide;
end;

procedure TScreenExampleGUI.Resume;
begin
 inherited Resume;
end;

procedure TScreenExampleGUI.Pause;
begin
 inherited Pause;
end;

procedure TScreenExampleGUI.Resize(const aWidth,aHeight:TpvInt32);
begin
 inherited Resize(aWidth,aHeight);
end;

procedure TScreenExampleGUI.AfterCreateSwapChain;
var SwapChainImageIndex:TpvInt32;
begin
 inherited AfterCreateSwapChain;

 FreeAndNil(fVulkanRenderPass);

 fVulkanRenderPass:=TpvVulkanRenderPass.Create(pvApplication.VulkanDevice);

 fVulkanRenderPass.AddSubpassDescription(0,
                                         VK_PIPELINE_BIND_POINT_GRAPHICS,
                                         [],
                                         [fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                              pvApplication.VulkanSwapChain.ImageFormat,
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
                                                                                                                             pvApplication.VulkanDepthImageFormat,
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

 fVulkanCanvas.VulkanRenderPass:=fVulkanRenderPass;
 fVulkanCanvas.CountBuffers:=pvApplication.CountSwapChainImages;
 if pvApplication.Width<pvApplication.Height then begin
  fVulkanCanvas.Width:=(720*pvApplication.Width) div pvApplication.Height;
  fVulkanCanvas.Height:=720;
 end else begin
  fVulkanCanvas.Width:=1280;
  fVulkanCanvas.Height:=(1280*pvApplication.Height) div pvApplication.Width;
 end;
 fVulkanCanvas.Viewport.x:=0;
 fVulkanCanvas.Viewport.y:=0;
 fVulkanCanvas.Viewport.width:=pvApplication.Width;
 fVulkanCanvas.Viewport.height:=pvApplication.Height;
 fScreenToCanvasScale:=TpvVector2.Create(fVulkanCanvas.Width,fVulkanCanvas.Height)/TpvVector2.Create(pvApplication.Width,pvApplication.Height);

 for SwapChainImageIndex:=0 to length(fVulkanRenderCommandBuffers)-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[SwapChainImageIndex]);
  fVulkanRenderCommandBuffers[SwapChainImageIndex]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
 end;

 fGUIInstance.CountBuffers:=pvApplication.CountSwapChainImages;
 fGUIInstance.Width:=fVulkanCanvas.Width;
 fGUIInstance.Height:=fVulkanCanvas.Height;
 fGUIInstance.MousePosition:=TpvVector2.Create(fGUIInstance.Width*0.5,fGUIInstance.Height*0.5);

 pvApplication.Input.SetCursorPosition(pvApplication.Width div 2,pvApplication.Height div 2);

 fGUIInstance.AfterCreateSwapChain;

 fGUIInstance.PerformLayout;

end;

procedure TScreenExampleGUI.BeforeDestroySwapChain;
begin
 fGUIInstance.BeforeDestroySwapChain;
 fVulkanCanvas.VulkanRenderPass:=nil;
 FreeAndNil(fVulkanRenderPass);
 inherited BeforeDestroySwapChain;
end;

function TScreenExampleGUI.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=false;
 if fReady and not fGUIInstance.KeyEvent(aKeyEvent) then begin
  if aKeyEvent.KeyEventType=KEYEVENT_DOWN then begin
   case aKeyEvent.KeyCode of
    KEYCODE_AC_BACK,KEYCODE_ESCAPE:begin
     pvApplication.NextScreen:=TScreenMainMenu.Create;
    end;
    KEYCODE_UP:begin
     if fSelectedIndex<=0 then begin
      fSelectedIndex:=0;
     end else begin
      dec(fSelectedIndex);
     end;
    end;
    KEYCODE_DOWN:begin
     if fSelectedIndex>=0 then begin
      fSelectedIndex:=0;
     end else begin
      inc(fSelectedIndex);
     end;
    end;
    KEYCODE_PAGEUP:begin
     if fSelectedIndex<0 then begin
      fSelectedIndex:=0;
     end;
    end;
    KEYCODE_PAGEDOWN:begin
     if fSelectedIndex<0 then begin
      fSelectedIndex:=0;
     end;
    end;
    KEYCODE_HOME:begin
     fSelectedIndex:=0;
    end;
    KEYCODE_END:begin
     fSelectedIndex:=0;
    end;
    KEYCODE_RETURN,KEYCODE_SPACE:begin
     if fSelectedIndex=0 then begin
      pvApplication.NextScreen:=TScreenMainMenu.Create;
     end;
    end;
   end;
  end;
 end;
end;

function TScreenExampleGUI.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var Index:TpvInt32;
    cy:TpvFloat;
    LocalPointerEvent:TpvApplicationInputPointerEvent;
begin
 result:=false;
 if fReady then begin
  LocalPointerEvent:=aPointerEvent;
  LocalPointerEvent.Position:=LocalPointerEvent.Position*fScreenToCanvasScale;
  LocalPointerEvent.RelativePosition:=LocalPointerEvent.RelativePosition*fScreenToCanvasScale;
  if not fGUIInstance.PointerEvent(LocalPointerEvent) then begin
   case aPointerEvent.PointerEventType of
    POINTEREVENT_DOWN:begin
     fSelectedIndex:=-1;
     cy:=fStartY;
     for Index:=0 to 0 do begin
      if (aPointerEvent.Position.y>=cy) and (aPointerEvent.Position.y<(cy+(ExampleApplication.TextOverlay.FontCharHeight*FontSize))) then begin
       fSelectedIndex:=Index;
       if fSelectedIndex=0 then begin
        pvApplication.NextScreen:=TScreenMainMenu.Create;
       end;
      end;
      cy:=cy+((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize);
     end;
    end;
    POINTEREVENT_UP:begin
    end;
    POINTEREVENT_MOTION:begin
     fSelectedIndex:=-1;
     cy:=fStartY;
     for Index:=0 to 0 do begin
      if (aPointerEvent.Position.y>=cy) and (aPointerEvent.Position.y<(cy+(ExampleApplication.TextOverlay.FontCharHeight*FontSize))) then begin
       fSelectedIndex:=Index;
      end;
      cy:=cy+((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize);
     end;
    end;
    POINTEREVENT_DRAG:begin
    end;
   end;
  end;
 end;
 case aPointerEvent.PointerEventType of
  POINTEREVENT_DOWN:begin
   Include(fLastMouseButtons,aPointerEvent.Button);
   fLastMousePosition:=aPointerEvent.Position*fScreenToCanvasScale;
  end;
  POINTEREVENT_UP:begin
   Exclude(fLastMouseButtons,aPointerEvent.Button);
   fLastMousePosition:=aPointerEvent.Position*fScreenToCanvasScale;
  end;
  POINTEREVENT_MOTION:begin
   fLastMousePosition:=aPointerEvent.Position*fScreenToCanvasScale;
  end;
 end;
end;

function TScreenExampleGUI.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 if fReady then begin
  result:=fGUIInstance.Scrolled(fLastMousePosition,aRelativeAmount);
 end else begin
  result:=false;
 end;
end;

function TScreenExampleGUI.CanBeParallelProcessed:boolean;
begin
 result:=true;
end;

procedure TScreenExampleGUI.Update(const aDeltaTime:TpvDouble);
const BoolToInt:array[boolean] of TpvInt32=(0,1);
      Options:array[0..0] of string=('Back');
var Index,SubIndex:TpvInt32;
    cy:TpvFloat;
    rbs:TpvUTF8String;
    s:string;
    IsSelected:boolean;
    SrcRect:TpvRect;
    DstRect:TpvRect;
begin
 inherited Update(aDeltaTime);

 fVulkanCanvas.Start(pvApplication.UpdateSwapChainImageIndex);

 fVulkanCanvas.ViewMatrix:=TpvMatrix4x4.Identity;

 fVulkanCanvas.BlendingMode:=pvcbmAlphaBlending;

{$if true}
 fVulkanCanvas.Color:=TpvVector4.Create(IfThen(BUTTON_LEFT in fLastMouseButtons,1.0,0.0),
                                        IfThen(BUTTON_RIGHT in fLastMouseButtons,1.0,0.0),
                                        1.0,
                                        1.0);
 fVulkanCanvas.DrawFilledCircle(fLastMousePosition,16.0);
 fVulkanCanvas.Color:=TpvVector4.Create(0.5,1.0,0.5,1.0);
 fVulkanCanvas.DrawFilledCircle(fLastMousePosition,4.0);
 fVulkanCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
{$ifend}

 fGUIInstance.DrawWidgetBounds:=false;
 fGUIInstance.UpdateBufferIndex:=pvApplication.UpdateSwapChainImageIndex;
 fGUIInstance.DeltaTime:=aDeltaTime;
 fGUIInstance.Update;

 fVulkanCanvas.Stop;

 ExampleApplication.TextOverlay.AddText(pvApplication.Width*0.5,ExampleApplication.TextOverlay.FontCharHeight*1.0,2.0,toaCenter,'GUI');
 fStartY:=pvApplication.Height-((((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize)*1.25)-(4*FontSize));
 cy:=fStartY;
 for Index:=0 to 0 do begin
  IsSelected:=fSelectedIndex=Index;
  s:=' '+Options[Index]+' ';
  if IsSelected then begin
   s:='>'+s+'<';
  end;
  ExampleApplication.TextOverlay.AddText(pvApplication.Width*0.5,cy,FontSize,toaCenter,TpvRawByteString(s),MenuColors[IsSelected,0,0],MenuColors[IsSelected,0,1],MenuColors[IsSelected,0,2],MenuColors[IsSelected,0,3],MenuColors[IsSelected,1,0],MenuColors[IsSelected,1,1],MenuColors[IsSelected,1,2],MenuColors[IsSelected,1,3]);
  cy:=cy+((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize);
 end;

 fTime:=fTime+aDeltaTime;

 fReady:=true;
end;

procedure TScreenExampleGUI.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
const Offsets:array[0..0] of TVkDeviceSize=(0);
var VulkanCommandBuffer:TpvVulkanCommandBuffer;
    VulkanSwapChain:TpvVulkanSwapChain;
begin

 begin

  begin

   VulkanCommandBuffer:=fVulkanRenderCommandBuffers[aSwapChainImageIndex];
   VulkanSwapChain:=pvApplication.VulkanSwapChain;

   fGUIInstance.DrawBufferIndex:=pvApplication.DrawSwapChainImageIndex;
   fGUIInstance.Draw;

   VulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

   VulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

   fVulkanCanvas.ExecuteUpload(VulkanCommandBuffer,
                               pvApplication.DrawSwapChainImageIndex);

   fVulkanRenderPass.BeginRenderPass(VulkanCommandBuffer,
                                     pvApplication.VulkanFrameBuffers[aSwapChainImageIndex],
                                     VK_SUBPASS_CONTENTS_INLINE,
                                     0,
                                     0,
                                     VulkanSwapChain.Width,
                                     VulkanSwapChain.Height);

   fVulkanCanvas.ExecuteDraw(VulkanCommandBuffer,
                             pvApplication.DrawSwapChainImageIndex);

   fVulkanRenderPass.EndRenderPass(VulkanCommandBuffer);

   VulkanCommandBuffer.EndRecording;

   VulkanCommandBuffer.Execute(pvApplication.VulkanDevice.GraphicsQueue,
                               TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                               aWaitSemaphore,
                               fVulkanRenderSemaphores[aSwapChainImageIndex],
                               aWaitFence,
                               false);

   aWaitSemaphore:=fVulkanRenderSemaphores[aSwapChainImageIndex];

  end;
 end;

end;

initialization
 RegisterExample('GUI',TScreenExampleGUI);
end.
