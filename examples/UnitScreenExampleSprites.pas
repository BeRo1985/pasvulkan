unit UnitScreenExampleSprites;
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

uses SysUtils,Classes,Vulkan,PasVulkan,PasVulkanApplication,UnitRegisteredExamplesList;

type TScreenExampleSprites=class(TVulkanApplicationScreen)
      private
       fVulkanRenderPass:TVulkanRenderPass;
       fVulkanCommandPool:TVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxSwapChainImages-1] of TVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxSwapChainImages-1] of TVulkanSemaphore;
       fVulkanSpriteAtlas:TVulkanSpriteAtlas;
       fVulkanSpriteBatch:TVulkanSpriteBatch;
       fVulkanSpriteTest:TVulkanSprite;
       fReady:boolean;
       fSelectedIndex:TVkInt32;
       fStartY:single;
       fTime:double;
      public

       constructor Create; override;

       destructor Destroy; override;

       procedure Show; override;

       procedure Hide; override;

       procedure Resume; override;

       procedure Pause; override;

       procedure Resize(const aWidth,aHeight:TVkInt32); override;

       procedure AfterCreateSwapChain; override;

       procedure BeforeDestroySwapChain; override;

       function KeyDown(const aKeyCode,aKeyModifier:TVkInt32):boolean; override;

       function KeyUp(const aKeyCode,aKeyModifier:TVkInt32):boolean; override;

       function KeyTyped(const aKeyCode,aKeyModifier:TVkInt32):boolean; override;

       function TouchDown(const aScreenX,aScreenY,aPressure:single;const aPointerID,aButton:TVkInt32):boolean; override;

       function TouchUp(const aScreenX,aScreenY,aPressure:single;const aPointerID,aButton:TVkInt32):boolean; override;

       function TouchDragged(const aScreenX,aScreenY,aPressure:single;const aPointerID:TVkInt32):boolean; override;

       function MouseMoved(const aScreenX,aScreenY:TVkInt32):boolean; override;

       function Scrolled(const aAmount:TVkInt32):boolean; override;

       procedure Update(const aDeltaTime:double); override;

       procedure Draw(const aSwapChainImageIndex:TVkInt32;var aWaitSemaphore:TVulkanSemaphore;const aWaitFence:TVulkanFence=nil); override;

     end;

implementation

uses UnitExampleVulkanApplication,UnitTextOverlay,UnitScreenMainMenu;

const SpritesVertices:array[0..2,0..1,0..2] of TVkFloat=
       (((0.5,0.5,0.0),(1.0,0.0,0.0)),
        ((-0.5,0.5,0.0),(0.0,1.0,0.0)),
        ((0.0,-0.5,0.0),(0.0,0.0,1.0)));

      SpritesIndices:array[0..2] of TVkInt32=(0,1,2);

      UniformBuffer:array[0..2,0..3,0..3] of TVkFloat=
       (((1.0,0.0,0.0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0.0,1.0)),  // Projection matrix
        ((1.0,0.0,0.0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0.0,1.0)),  // Model matrix
        ((1.0,0.0,0.0,0.0),(0.0,1.0,0.0,0.0),(0.0,0.0,1.0,0.0),(0.0,0.0,0.0,1.0))); // View matrix

      Offsets:array[0..0] of TVkDeviceSize=(0);

      FontSize=3.0;

constructor TScreenExampleSprites.Create;
begin
 inherited Create;
 fSelectedIndex:=-1;
 fReady:=false;
 fTime:=0;
end;

destructor TScreenExampleSprites.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenExampleSprites.Show;
var Stream:TStream;
    Index,x,y:TVkInt32;
    RawSprite:pointer;
begin
 inherited Show;

 fVulkanCommandPool:=TVulkanCommandPool.Create(VulkanApplication.VulkanDevice,
                                               VulkanApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                               TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
 for Index:=0 to MaxSwapChainImages-1 do begin
  fVulkanRenderCommandBuffers[Index]:=TVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fVulkanRenderSemaphores[Index]:=TVulkanSemaphore.Create(VulkanApplication.VulkanDevice);
 end;

 fVulkanRenderPass:=nil;

 fVulkanSpriteAtlas:=TVulkanSpriteAtlas.Create(VulkanApplication.VulkanDevice);

 GetMem(RawSprite,256*256*4);
 try
  FillChar(RawSprite^,256*256*4,#$ff);
  Index:=0;
  for y:=0 to 255 do begin
   for x:=0 to 255 do begin
    TVKUInt8(PVulkanRawByteChar(RawSprite)[Index+0]):=x;
    TVKUInt8(PVulkanRawByteChar(RawSprite)[Index+1]):=y;
    TVKUInt8(PVulkanRawByteChar(RawSprite)[Index+2]):=(x*y) shr 8;
    inc(Index,4);
   end;
  end;
  fVulkanSpriteTest:=fVulkanSpriteAtlas.LoadRawSprite('test',RawSprite,256,256);
 finally
  FreeMem(RawSprite);
 end;

 fVulkanSpriteAtlas.Upload(VulkanApplication.VulkanDevice.GraphicsQueue,
                           VulkanApplication.VulkanGraphicsCommandBuffers[0,0],
                           VulkanApplication.VulkanGraphicsCommandBufferFences[0,0],
                           VulkanApplication.VulkanDevice.TransferQueue,
                           VulkanApplication.VulkanTransferCommandBuffers[0,0],
                           VulkanApplication.VulkanTransferCommandBufferFences[0,0]);

end;

procedure TScreenExampleSprites.Hide;
var Index:TVkInt32;
begin
 FreeAndNil(fVulkanSpriteAtlas);
 FreeAndNil(fVulkanRenderPass);
 for Index:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[Index]);
  FreeAndNil(fVulkanRenderSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);
 inherited Hide;
end;

procedure TScreenExampleSprites.Resume;
begin
 inherited Resume;
end;

procedure TScreenExampleSprites.Pause;
begin
 inherited Pause;
end;

procedure TScreenExampleSprites.Resize(const aWidth,aHeight:TVkInt32);
begin
 inherited Resize(aWidth,aHeight);
end;

procedure TScreenExampleSprites.AfterCreateSwapChain;
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

 fVulkanSpriteBatch:=TVulkanSpriteBatch.Create(VulkanApplication.VulkanDevice,
                                               VulkanApplication.VulkanDevice.GraphicsQueue,
                                               VulkanApplication.VulkanGraphicsCommandBuffers[0,0],
                                               VulkanApplication.VulkanGraphicsCommandBufferFences[0,0],
                                               VulkanApplication.VulkanDevice.TransferQueue,
                                               VulkanApplication.VulkanTransferCommandBuffers[0,0],
                                               VulkanApplication.VulkanTransferCommandBufferFences[0,0],
                                               VulkanApplication.VulkanPipelineCache,
                                               fVulkanRenderPass,
                                               2,
                                               VulkanApplication.Width,
                                               VulkanApplication.Height);

 for SwapChainImageIndex:=0 to length(fVulkanRenderCommandBuffers)-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[SwapChainImageIndex]);
  fVulkanRenderCommandBuffers[SwapChainImageIndex]:=TVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
 end;

end;

procedure TScreenExampleSprites.BeforeDestroySwapChain;
begin
 FreeAndNil(fVulkanSpriteBatch);
 FreeAndNil(fVulkanRenderPass);
 inherited BeforeDestroySwapChain;
end;

function TScreenExampleSprites.KeyDown(const aKeyCode,aKeyModifier:TVkInt32):boolean;
begin
 result:=false;
 if fReady then begin
  case aKeyCode of
   KEYCODE_AC_BACK,KEYCODE_ESCAPE:begin
    VulkanApplication.NextScreen:=TScreenMainMenu.Create;
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
    fSelectedIndex:=0
   end;
   KEYCODE_RETURN,KEYCODE_SPACE:begin
    if fSelectedIndex=0 then begin
     VulkanApplication.NextScreen:=TScreenMainMenu.Create;
    end;
   end;
  end;
 end;
end;

function TScreenExampleSprites.KeyUp(const aKeyCode,aKeyModifier:TVkInt32):boolean;
begin
 result:=false;
end;

function TScreenExampleSprites.KeyTyped(const aKeyCode,aKeyModifier:TVkInt32):boolean;
begin
 result:=false;
end;

function TScreenExampleSprites.TouchDown(const aScreenX,aScreenY,aPressure:single;const aPointerID,aButton:TVkInt32):boolean;
var Index:TVkInt32;
    cy:single;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 0 do begin
   if (aScreenY>=cy) and (aScreenY<=(cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
    if fSelectedIndex=0 then begin
     VulkanApplication.NextScreen:=TScreenMainMenu.Create;
    end;
   end;
   cy:=cy+((ExampleVulkanApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExampleSprites.TouchUp(const aScreenX,aScreenY,aPressure:single;const aPointerID,aButton:TVkInt32):boolean;
begin
 result:=false;
end;

function TScreenExampleSprites.TouchDragged(const aScreenX,aScreenY,aPressure:single;const aPointerID:TVkInt32):boolean;
var Index:TVkInt32;
    cy:single;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 0 do begin
   if (aScreenY>=cy) and (aScreenY<=(cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
   end;
   cy:=cy+((ExampleVulkanApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExampleSprites.MouseMoved(const aScreenX,aScreenY:TVkInt32):boolean;
var Index:TVkInt32;
    cy:single;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 0 do begin
   if (aScreenY>=cy) and (aScreenY<=(cy+(ExampleVulkanApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
   end;
   cy:=cy+((ExampleVulkanApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExampleSprites.Scrolled(const aAmount:TVkInt32):boolean;
begin
 result:=false;
end;

procedure TScreenExampleSprites.Update(const aDeltaTime:double);
const BoolToInt:array[boolean] of TVkInt32=(0,1);
      Options:array[0..0] of string=('Back');
var Index:TVkInt32;
    cy:single;
    s:string;
    IsSelected:boolean;
    SrcRect:TVulkanSpriteRect;
    DstRect:TVulkanSpriteRect;
begin
 inherited Update(aDeltaTime);

 fVulkanSpriteBatch.Start(VulkanApplication.UpdateFrameCounter and 1);
 SrcRect:=VulkanSpriteRect(0,0,255,255);
 DstRect.Left:=((VulkanApplication.Width-fVulkanSpriteTest.Width)*0.5)+(sin(fTime*pi*2.0*0.1)*128.0);
 DstRect.Top:=((VulkanApplication.Height-fVulkanSpriteTest.Height)*0.5)+(sin(fTime*pi*3.0*0.1)*128.0);
 DstRect.Right:=DstRect.Left+fVulkanSpriteTest.Width;
 DstRect.Bottom:=DstRect.Top+fVulkanSpriteTest.Height;
 fVulkanSpriteBatch.Draw(fVulkanSpriteTest,SrcRect,DstRect,VulkanSpritePoint(fVulkanSpriteTest.Width*0.5,fVulkanSpriteTest.Height*0.5),sin(fTime*pi*1.3*0.1)*pi*2.0,VulkanSpriteColor(1.0,1.0,1.0,1.0));
 fVulkanSpriteBatch.Stop;

 ExampleVulkanApplication.TextOverlay.AddText(VulkanApplication.Width*0.5,ExampleVulkanApplication.TextOverlay.FontCharHeight*1.0,2.0,toaCenter,'Sprites');
 fStartY:=VulkanApplication.Height-((((ExampleVulkanApplication.TextOverlay.FontCharHeight+4)*FontSize)*1.25)-(4*FontSize));
 cy:=fStartY;
 for Index:=0 to 0 do begin
  IsSelected:=fSelectedIndex=Index;
  s:=' '+Options[Index]+' ';
  if IsSelected then begin
   s:='>'+s+'<';
  end;
  ExampleVulkanApplication.TextOverlay.AddText(VulkanApplication.Width*0.5,cy,FontSize,toaCenter,s,MenuColors[IsSelected,0,0],MenuColors[IsSelected,0,1],MenuColors[IsSelected,0,2],MenuColors[IsSelected,0,3],MenuColors[IsSelected,1,0],MenuColors[IsSelected,1,1],MenuColors[IsSelected,1,2],MenuColors[IsSelected,1,3]);
  cy:=cy+((ExampleVulkanApplication.TextOverlay.FontCharHeight+4)*FontSize);
 end;

 fTime:=fTime+aDeltaTime;

 fReady:=true;
end;

procedure TScreenExampleSprites.Draw(const aSwapChainImageIndex:TVkInt32;var aWaitSemaphore:TVulkanSemaphore;const aWaitFence:TVulkanFence=nil);
const Offsets:array[0..0] of TVkDeviceSize=(0);
var BufferIndex,Size:TVkInt32;
    VulkanVertexBuffer:TVulkanBuffer;
    VulkanCommandBuffer:TVulkanCommandBuffer;
    VulkanSwapChain:TVulkanSwapChain;
    p:pointer;
begin

 begin

  begin

   VulkanCommandBuffer:=fVulkanRenderCommandBuffers[aSwapChainImageIndex];
   VulkanSwapChain:=VulkanApplication.VulkanSwapChain;

   VulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

   VulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

   fVulkanRenderPass.BeginRenderPass(VulkanCommandBuffer,
                                     VulkanApplication.VulkanFrameBuffers[aSwapChainImageIndex],
                                     VK_SUBPASS_CONTENTS_INLINE,
                                     0,
                                     0,
                                     VulkanSwapChain.Width,
                                     VulkanSwapChain.Height);

   fVulkanSpriteBatch.ExecuteDraw(VulkanCommandBuffer,
                                  VulkanApplication.DrawFrameCounter and 1);

   fVulkanRenderPass.EndRenderPass(VulkanCommandBuffer);

   VulkanCommandBuffer.EndRecording;

   VulkanCommandBuffer.Execute(VulkanApplication.VulkanDevice.GraphicsQueue,
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
 RegisterExample('Sprites',TScreenExampleSprites);
end.
