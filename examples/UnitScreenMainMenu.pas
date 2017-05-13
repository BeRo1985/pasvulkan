unit UnitScreenMainMenu;
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

uses SysUtils,Classes,Vulkan,PasVulkan,PasVulkanAndroid,PasVulkanSDL2,PasVulkanApplication,UnitRegisteredExamplesList;

type TScreenMainMenu=class(TVulkanScreen)
      private
       fTriangleVertexShaderModule:TVulkanShaderModule;
       fTriangleFragmentShaderModule:TVulkanShaderModule;
       fVulkanPipelineShaderStageTriangleVertex:TVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageTriangleFragment:TVulkanPipelineShaderStage;
       fVulkanPipelineCache:TVulkanPipelineCache;
       fVulkanRenderPass:TVulkanRenderPass;
       fVulkanCommandPool:TVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxSwapChainImages-1] of TVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxSwapChainImages-1] of TVulkanSemaphore;
       fReady:boolean;
       fSelectedIndex:TVkInt32;
      public

       constructor Create; override;

       destructor Destroy; override;

       procedure Show; override;

       procedure Hide; override;

       procedure Resume; override;

       procedure Pause; override;

       procedure Resize(const pWidth,pHeight:TSDLInt32); override;

       procedure AfterCreateSwapChain; override;

       procedure BeforeDestroySwapChain; override;

       function HandleEvent(const pEvent:TSDL_Event):boolean; override;

       function KeyDown(const pKeyCode,pKeyModifier:TVkInt32):boolean; override;

       function KeyUp(const pKeyCode,pKeyModifier:TVkInt32):boolean; override;

       function KeyTyped(const pKeyCode,pKeyModifier:TVkInt32):boolean; override;

       function TouchDown(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean; override;

       function TouchUp(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean; override;

       function TouchDragged(const pScreenX,pScreenY,pPressure:single;const pPointerID:TVkInt32):boolean; override;

       function MouseMoved(const pScreenX,pScreenY:TVkInt32):boolean; override;

       function Scrolled(const pAmount:TVkInt32):boolean; override;

       procedure Update(const pDeltaTime:double); override;

       procedure Draw(const pSwapChainImageIndex:TVkInt32;var pWaitSemaphore:TVulkanSemaphore;const pWaitFence:TVulkanFence=nil); override;

     end;

implementation

uses UnitExampleVulkanApplication,UnitTextOverlay;


constructor TScreenMainMenu.Create;
begin
 inherited Create;
 fReady:=false;
end;

destructor TScreenMainMenu.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMainMenu.Show;
var Stream:TStream;
    Index:TVkInt32;
begin
 inherited Show;

 fVulkanCommandPool:=TVulkanCommandPool.Create(VulkanApplication.VulkanDevice,
                                               VulkanApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                               TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
 for Index:=0 to MaxSwapChainImages-1 do begin
  fVulkanRenderCommandBuffers[Index]:=TVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fVulkanRenderSemaphores[Index]:=TVulkanSemaphore.Create(VulkanApplication.VulkanDevice);
 end;

 Stream:=VulkanApplication.Assets.GetAssetStream('shaders/triangle/triangle_vert.spv');
 try
  fTriangleVertexShaderModule:=TVulkanShaderModule.Create(VulkanApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=VulkanApplication.Assets.GetAssetStream('shaders/triangle/triangle_frag.spv');
 try
  fTriangleFragmentShaderModule:=TVulkanShaderModule.Create(VulkanApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 fVulkanPipelineShaderStageTriangleVertex:=TVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fTriangleVertexShaderModule,'main');

 fVulkanPipelineShaderStageTriangleFragment:=TVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fTriangleFragmentShaderModule,'main');

 fVulkanPipelineCache:=TVulkanPipelineCache.Create(VulkanApplication.VulkanDevice);

 fVulkanRenderPass:=nil;

 fSelectedIndex:=-1;

end;

procedure TScreenMainMenu.Hide;
var Index:TVkInt32;
begin
 FreeAndNil(fVulkanRenderPass);
 FreeAndNil(fVulkanPipelineCache);
 FreeAndNil(fVulkanPipelineShaderStageTriangleVertex);
 FreeAndNil(fVulkanPipelineShaderStageTriangleFragment);
 FreeAndNil(fTriangleFragmentShaderModule);
 FreeAndNil(fTriangleVertexShaderModule);
 for Index:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[Index]);
  FreeAndNil(fVulkanRenderSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);
 inherited Hide;
end;

procedure TScreenMainMenu.Resume;
begin
 inherited Resume;
end;

procedure TScreenMainMenu.Pause;
begin
 inherited Pause;
end;

procedure TScreenMainMenu.Resize(const pWidth,pHeight:TSDLInt32);
begin
 inherited Resize(pWidth,pHeight);
end;

procedure TScreenMainMenu.AfterCreateSwapChain;
var SwapChainImageIndex:TVkInt32;
    VulkanCommandBuffer:TVulkanCommandBuffer;
begin
 inherited AfterCreateSwapChain;

 FreeAndNil(fVulkanRenderPass);

 fVulkanRenderPass:=TVulkanRenderPass.Create(VulkanApplication.VulkanDevice);

 fVulkanRenderPass.AddSubpassDescription(0,
                                         VK_PIPELINE_BIND_POINT_GRAPHICS,
                                         [],
                                         [fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                              VulkanApplication.VulkanSwapChain.ImageFormat,
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
                                                                                                                             VulkanApplication.VulkanDepthImageFormat,
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

 for SwapChainImageIndex:=0 to length(fVulkanRenderCommandBuffers)-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[SwapChainImageIndex]);
 end;

 for SwapChainImageIndex:=0 to VulkanApplication.CountSwapChainImages-1 do begin

  fVulkanRenderCommandBuffers[SwapChainImageIndex]:=TVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

  VulkanCommandBuffer:=fVulkanRenderCommandBuffers[SwapChainImageIndex];

  VulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT));

  fVulkanRenderPass.BeginRenderPass(VulkanCommandBuffer,
                                    VulkanApplication.VulkanFrameBuffers[SwapChainImageIndex],
                                    VK_SUBPASS_CONTENTS_INLINE,
                                    0,
                                    0,
                                    VulkanApplication.VulkanSwapChain.Width,
                                    VulkanApplication.VulkanSwapChain.Height);

  fVulkanRenderPass.EndRenderPass(VulkanCommandBuffer);

  VulkanCommandBuffer.EndRecording;

 end;

end;

procedure TScreenMainMenu.BeforeDestroySwapChain;
begin
 FreeAndNil(fVulkanRenderPass);
 inherited BeforeDestroySwapChain;
end;

function TScreenMainMenu.HandleEvent(const pEvent:TSDL_Event):boolean;
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
    SDLK_AC_BACK,SDLK_ESCAPE:begin
     QuitMsgBoxDataButtonID:=-1;
     if (SDL_ShowMessageBox(@QuitMsgBoxData,@QuitMsgBoxDataButtonID)=0) and (QuitMsgBoxDataButtonID<>0) then begin
      VulkanApplication.Terminate;
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

function TScreenMainMenu.KeyDown(const pKeyCode,pKeyModifier:TVkInt32):boolean;
begin
 result:=false;
 if fReady then begin
  case pKeyCode of
   KEYCODE_UP:begin
    if fSelectedIndex<=0 then begin
     fSelectedIndex:=RegisteredExamplesList.Count-1;
    end else begin
     dec(fSelectedIndex);
    end;
   end;
   KEYCODE_DOWN:begin
    if fSelectedIndex>=(RegisteredExamplesList.Count-1) then begin
     fSelectedIndex:=0;
    end else begin
     inc(fSelectedIndex);
    end;
   end;
   KEYCODE_RETURN:begin
    VulkanApplication.NextScreen:=TVulkanScreenClass(RegisteredExamplesList.Objects[fSelectedIndex]).Create;
   end;
  end;
 end;
end;

function TScreenMainMenu.KeyUp(const pKeyCode,pKeyModifier:TVkInt32):boolean;
begin
 result:=false;
end;

function TScreenMainMenu.KeyTyped(const pKeyCode,pKeyModifier:TVkInt32):boolean;
begin
 result:=false;
end;

function TScreenMainMenu.TouchDown(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean;
var Index:TVkInt32;
    cy:single;
begin
 result:=false;
 if fReady then begin
  cy:=ExampleVulkanApplication.TextOverlay.FontCharHeight*5.0;
  for Index:=0 to RegisteredExamplesList.Count-1 do begin
   if (pScreenY>=cy) and (pScreenY<=(cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*2.0))) then begin
    fSelectedIndex:=Index;
    VulkanApplication.NextScreen:=TVulkanScreenClass(RegisteredExamplesList.Objects[fSelectedIndex]).Create;
   end;
   cy:=cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*2.0)+8;
  end;
 end;
end;

function TScreenMainMenu.TouchUp(const pScreenX,pScreenY,pPressure:single;const pPointerID,pButton:TVkInt32):boolean;
begin
 result:=false;
end;

function TScreenMainMenu.TouchDragged(const pScreenX,pScreenY,pPressure:single;const pPointerID:TVkInt32):boolean;
var Index:TVkInt32;
    cy:single;
begin
 result:=false;
 if fReady then begin
  cy:=ExampleVulkanApplication.TextOverlay.FontCharHeight*5.0;
  for Index:=0 to RegisteredExamplesList.Count-1 do begin
   if (pScreenY>=cy) and (pScreenY<=(cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*2.0))) then begin
    fSelectedIndex:=Index;
   end;
   cy:=cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*2.0)+8;
  end;
 end;
end;

function TScreenMainMenu.MouseMoved(const pScreenX,pScreenY:TVkInt32):boolean;
var Index:TVkInt32;
    cy:single;
begin
 result:=false;
 if fReady then begin
  cy:=ExampleVulkanApplication.TextOverlay.FontCharHeight*5.0;
  for Index:=0 to RegisteredExamplesList.Count-1 do begin
   if (pScreenY>=cy) and (pScreenY<=(cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*2.0))) then begin
    fSelectedIndex:=Index;
   end;
   cy:=cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*2.0)+8;
  end;
 end;
end;

function TScreenMainMenu.Scrolled(const pAmount:TVkInt32):boolean;
begin
 result:=false;
end;

procedure TScreenMainMenu.Update(const pDeltaTime:double);
const BoolToInt:array[boolean] of TVkInt32=(0,1);
var Index:TVkInt32;
    cy:single;
    s:string;
    IsSelected:boolean;
begin
 inherited Update(pDeltaTime);
 cy:=ExampleVulkanApplication.TextOverlay.FontCharHeight*5.0;
 for Index:=0 to RegisteredExamplesList.Count-1 do begin
  IsSelected:=fSelectedIndex=Index;
  s:=' '+RegisteredExamplesList[Index]+' ';
  if IsSelected then begin
   s:='>'+s+'<';
  end;
  ExampleVulkanApplication.TextOverlay.AddText(VulkanApplication.Width*0.5,cy,2.0,toaCenter,s,1.0-BoolToInt[IsSelected],1.0-BoolToInt[IsSelected],1.0,(BoolToInt[IsSelected]*0.95)+0.05,1.0,1.0,1.0,1.0);
  cy:=cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*2.0)+8;
 end;
 fReady:=true;
end;

procedure TScreenMainMenu.Draw(const pSwapChainImageIndex:TVkInt32;var pWaitSemaphore:TVulkanSemaphore;const pWaitFence:TVulkanFence=nil);
begin
 inherited Draw(pSwapChainImageIndex,pWaitSemaphore,nil);
 if assigned(fVulkanRenderPass) then begin

  fVulkanRenderCommandBuffers[pSwapChainImageIndex].Execute(VulkanApplication.VulkanDevice.GraphicsQueue,
                                                            TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                                            pWaitSemaphore,
                                                            fVulkanRenderSemaphores[pSwapChainImageIndex],
                                                            pWaitFence,
                                                            false);

  pWaitSemaphore:=fVulkanRenderSemaphores[pSwapChainImageIndex];

 end;
end;

end.
