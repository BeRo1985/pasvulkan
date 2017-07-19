unit UnitScreenExampleCanvas;
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
     UnitRegisteredExamplesList,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Sprites,
     PasVulkan.Canvas,
     PasVulkan.Font,
     PasVulkan.TrueTypeFont;

type TScreenExampleCanvas=class(TpvApplicationScreen)
      private
       fVulkanRenderPass:TpvVulkanRenderPass;
       fVulkanCommandPool:TpvVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxSwapChainImages-1] of TpvVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxSwapChainImages-1] of TpvVulkanSemaphore;
       fVulkanSpriteAtlas:TpvSpriteAtlas;
       fVulkanFontSpriteAtlas:TpvSpriteAtlas;
       fVulkanCanvas:TpvCanvas;
       fVulkanFont:TpvFont;
       fVulkanSpriteTest:TpvSprite;
       fVulkanSpriteSmiley0:TpvSprite;
       fVulkanSpriteAppIcon:TpvSprite;
       fVulkanSpriteDancer0:TpvSprite;
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

       function KeyDown(const aKeyCode,aKeyModifier:TpvInt32):boolean; override;

       function KeyUp(const aKeyCode,aKeyModifier:TpvInt32):boolean; override;

       function KeyTyped(const aKeyCode,aKeyModifier:TpvInt32):boolean; override;

       function TouchDown(const aScreenX,aScreenY,aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; override;

       function TouchUp(const aScreenX,aScreenY,aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean; override;

       function TouchDragged(const aScreenX,aScreenY,aPressure:TpvFloat;const aPointerID:TpvInt32):boolean; override;

       function MouseMoved(const aScreenX,aScreenY:TpvInt32):boolean; override;

       function Scrolled(const aAmount:TpvInt32):boolean; override;

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

constructor TScreenExampleCanvas.Create;
begin
 inherited Create;
 fSelectedIndex:=-1;
 fReady:=false;
 fTime:=0;
end;

destructor TScreenExampleCanvas.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenExampleCanvas.Show;
var Stream:TStream;
    Index,x,y:TpvInt32;
    RawSprite:pointer;
    TrueTypeFont:TpvTrueTypeFont;
begin
 inherited Show;

 fVulkanCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                 pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                 TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
 for Index:=0 to MaxSwapChainImages-1 do begin
  fVulkanRenderCommandBuffers[Index]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fVulkanRenderSemaphores[Index]:=TpvVulkanSemaphore.Create(pvApplication.VulkanDevice);
 end;

 fVulkanRenderPass:=nil;

 fVulkanSpriteAtlas:=TpvSpriteAtlas.Create(pvApplication.VulkanDevice);

 fVulkanFontSpriteAtlas:=TpvSpriteAtlas.Create(pvApplication.VulkanDevice);
 fVulkanFontSpriteAtlas.MipMaps:=false;

 //Stream:=pvApplication.Assets.GetAssetStream('fonts/linbiolinum_r.otf');
 //Stream:=pvApplication.Assets.GetAssetStream('fonts/notosans.ttf');
 Stream:=pvApplication.Assets.GetAssetStream('fonts/vera.ttf');
 try
  TrueTypeFont:=TpvTrueTypeFont.Create(Stream,72);
  try
   TrueTypeFont.Size:=-64;
   TrueTypeFont.Hinting:=false;
   fVulkanFont:=TpvFont.CreateFromTrueTypeFont(pvApplication.VulkanDevice,fVulkanFontSpriteAtlas,TrueTypeFont,[TpvFont.CodePointRange(0,255)]);
  finally
   TrueTypeFont.Free;
  end;
 finally
  Stream.Free;
 end;

 GetMem(RawSprite,256*256*4);
 try
  FillChar(RawSprite^,256*256*4,#$ff);
  Index:=0;
  for y:=0 to 255 do begin
   for x:=0 to 255 do begin
    if (x in [0,255]) or (y in [0,255]) then begin
     TVKUInt8(PpvVulkanRawByteChar(RawSprite)[Index+0]):=0;
     TVKUInt8(PpvVulkanRawByteChar(RawSprite)[Index+1]):=0;
     TVKUInt8(PpvVulkanRawByteChar(RawSprite)[Index+2]):=0;
    end else begin
     TVKUInt8(PpvVulkanRawByteChar(RawSprite)[Index+0]):=x;
     TVKUInt8(PpvVulkanRawByteChar(RawSprite)[Index+1]):=y;
     TVKUInt8(PpvVulkanRawByteChar(RawSprite)[Index+2]):=(x*y) shr 8;
    end;
    inc(Index,4);
   end;
  end;
  fVulkanSpriteTest:=fVulkanSpriteAtlas.LoadRawSprite('test',RawSprite,256,256);
 finally
  FreeMem(RawSprite);
 end;

 Stream:=pvApplication.Assets.GetAssetStream('sprites/smiley0.png');
 try
  fVulkanSpriteSmiley0:=fVulkanSpriteAtlas.LoadSprite('smiley0',Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('sprites/appicon.png');
 try
  fVulkanSpriteAppIcon:=fVulkanSpriteAtlas.LoadSprite('appicon',Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('sprites/dancer0.png');
 try
  fVulkanSpriteDancer0:=fVulkanSpriteAtlas.LoadSprite('dancer0',Stream);
 finally
  Stream.Free;
 end;

 fVulkanFontSpriteAtlas.Upload(pvApplication.VulkanDevice.GraphicsQueue,
                               pvApplication.VulkanGraphicsCommandBuffers[0,0],
                               pvApplication.VulkanGraphicsCommandBufferFences[0,0],
                               pvApplication.VulkanDevice.TransferQueue,
                               pvApplication.VulkanTransferCommandBuffers[0,0],
                               pvApplication.VulkanTransferCommandBufferFences[0,0]);

 fVulkanSpriteAtlas.Upload(pvApplication.VulkanDevice.GraphicsQueue,
                           pvApplication.VulkanGraphicsCommandBuffers[0,0],
                           pvApplication.VulkanGraphicsCommandBufferFences[0,0],
                           pvApplication.VulkanDevice.TransferQueue,
                           pvApplication.VulkanTransferCommandBuffers[0,0],
                           pvApplication.VulkanTransferCommandBufferFences[0,0]);

end;

procedure TScreenExampleCanvas.Hide;
var Index:TpvInt32;
begin
 FreeAndNil(fVulkanFont);
 FreeAndNil(fVulkanFontSpriteAtlas);
 FreeAndNil(fVulkanSpriteAtlas);
 FreeAndNil(fVulkanRenderPass);
 for Index:=0 to MaxSwapChainImages-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[Index]);
  FreeAndNil(fVulkanRenderSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);
 inherited Hide;
end;

procedure TScreenExampleCanvas.Resume;
begin
 inherited Resume;
end;

procedure TScreenExampleCanvas.Pause;
begin
 inherited Pause;
end;

procedure TScreenExampleCanvas.Resize(const aWidth,aHeight:TpvInt32);
begin
 inherited Resize(aWidth,aHeight);
end;

procedure TScreenExampleCanvas.AfterCreateSwapChain;
var SwapChainImageIndex:TpvInt32;
    VulkanCommandBuffer:TpvVulkanCommandBuffer;
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

 fVulkanCanvas:=TpvCanvas.Create(pvApplication.VulkanDevice,
                                       pvApplication.VulkanDevice.GraphicsQueue,
                                       pvApplication.VulkanGraphicsCommandBuffers[0,0],
                                       pvApplication.VulkanGraphicsCommandBufferFences[0,0],
                                       pvApplication.VulkanDevice.TransferQueue,
                                       pvApplication.VulkanTransferCommandBuffers[0,0],
                                       pvApplication.VulkanTransferCommandBufferFences[0,0],
                                       pvApplication.VulkanPipelineCache,
                                       fVulkanRenderPass,
                                       pvApplication.CountSwapChainImages);
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

 for SwapChainImageIndex:=0 to length(fVulkanRenderCommandBuffers)-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[SwapChainImageIndex]);
  fVulkanRenderCommandBuffers[SwapChainImageIndex]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
 end;

end;

procedure TScreenExampleCanvas.BeforeDestroySwapChain;
begin
 FreeAndNil(fVulkanCanvas);
 FreeAndNil(fVulkanRenderPass);
 inherited BeforeDestroySwapChain;
end;

function TScreenExampleCanvas.KeyDown(const aKeyCode,aKeyModifier:TpvInt32):boolean;
begin
 result:=false;
 if fReady then begin
  case aKeyCode of
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
    fSelectedIndex:=0
   end;
   KEYCODE_RETURN,KEYCODE_SPACE:begin
    if fSelectedIndex=0 then begin
     pvApplication.NextScreen:=TScreenMainMenu.Create;
    end;
   end;
  end;
 end;
end;

function TScreenExampleCanvas.KeyUp(const aKeyCode,aKeyModifier:TpvInt32):boolean;
begin
 result:=false;
end;

function TScreenExampleCanvas.KeyTyped(const aKeyCode,aKeyModifier:TpvInt32):boolean;
begin
 result:=false;
end;

function TScreenExampleCanvas.TouchDown(const aScreenX,aScreenY,aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
var Index:TpvInt32;
    cy:TpvFloat;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 0 do begin
   if (aScreenY>=cy) and (aScreenY<=(cy+(ExampleApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
    if fSelectedIndex=0 then begin
     pvApplication.NextScreen:=TScreenMainMenu.Create;
    end;
   end;
   cy:=cy+((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExampleCanvas.TouchUp(const aScreenX,aScreenY,aPressure:TpvFloat;const aPointerID,aButton:TpvInt32):boolean;
begin
 result:=false;
end;

function TScreenExampleCanvas.TouchDragged(const aScreenX,aScreenY,aPressure:TpvFloat;const aPointerID:TpvInt32):boolean;
var Index:TpvInt32;
    cy:TpvFloat;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 0 do begin
   if (aScreenY>=cy) and (aScreenY<=(cy+(ExampleApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
   end;
   cy:=cy+((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExampleCanvas.MouseMoved(const aScreenX,aScreenY:TpvInt32):boolean;
var Index:TpvInt32;
    cy:TpvFloat;
begin
 result:=false;
 if fReady then begin
  fSelectedIndex:=-1;
  cy:=fStartY;
  for Index:=0 to 0 do begin
   if (aScreenY>=cy) and (aScreenY<=(cy+(ExampleApplication.TextOverlay.FontCharHeight*FontSize))) then begin
    fSelectedIndex:=Index;
   end;
   cy:=cy+((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize);
  end;
 end;
end;

function TScreenExampleCanvas.Scrolled(const aAmount:TpvInt32):boolean;
begin
 result:=false;
end;

procedure TScreenExampleCanvas.Update(const aDeltaTime:TpvDouble);
const BoolToInt:array[boolean] of TpvInt32=(0,1);
      Options:array[0..0] of string=('Back');
var Index:TpvInt32;
    cy,LocalFontSize:TpvFloat;
    rbs:TpvUTF8String;
    s:string;
    IsSelected:boolean;
    SrcRect:TpvRect;
    DstRect:TpvRect;
begin
 inherited Update(aDeltaTime);

 fVulkanCanvas.Start(pvApplication.UpdateSwapChainImageIndex);

 fVulkanCanvas.BlendingMode:=vsbbmNone;

 SrcRect:=TpvRect.Create(0,0,fVulkanSpriteTest.Width,fVulkanSpriteTest.Height);
 DstRect.Left:=((fVulkanCanvas.Width-fVulkanSpriteTest.Width)*0.5)+(cos(fTime*pi*2.0*0.1)*128.0);
 DstRect.Top:=((fVulkanCanvas.Height-fVulkanSpriteTest.Height)*0.5)+(sin(fTime*pi*3.0*0.1)*128.0);
 DstRect.Right:=DstRect.Left+fVulkanSpriteTest.Width;
 DstRect.Bottom:=DstRect.Top+fVulkanSpriteTest.Height;
 fVulkanCanvas.DrawSprite(fVulkanSpriteTest,SrcRect,DstRect,TpvVector2.Create(fVulkanSpriteTest.Width*0.5,fVulkanSpriteTest.Height*0.5),sin(fTime*pi*1.3*0.1)*pi*2.0,TpvVector4.Create(1.0,1.0,1.0,1.0));

 fVulkanCanvas.BlendingMode:=vsbbmAlphaBlending;

 SrcRect:=TpvRect.Create(0,0,fVulkanSpriteAppIcon.Width,fVulkanSpriteAppIcon.Height);
 DstRect.Left:=((fVulkanCanvas.Width-fVulkanSpriteAppIcon.Width)*0.5)+(sin(fTime*pi*2.0*0.1)*128.0);
 DstRect.Top:=((fVulkanCanvas.Height-fVulkanSpriteAppIcon.Height)*0.5)+(cos(fTime*pi*3.0*0.1)*128.0);
 DstRect.Right:=DstRect.Left+fVulkanSpriteAppIcon.Width;
 DstRect.Bottom:=DstRect.Top+fVulkanSpriteAppIcon.Height;
 fVulkanCanvas.DrawSprite(fVulkanSpriteAppIcon,SrcRect,DstRect,TpvVector2.Create(fVulkanSpriteAppIcon.Width*0.5,fVulkanSpriteAppIcon.Height*0.5),cos(fTime*pi*1.7*0.1)*pi*2.0,TpvVector4.Create(1.0,1.0,1.0,1.0));
 {
 SrcRect:=TpvRect.Create(0,0,fVulkanSpriteSmiley0.Width,fVulkanSpriteSmiley0.Height);
 DstRect.Left:=((fVulkanCanvas.Width-fVulkanSpriteSmiley0.Width)*0.5)+(sin(fTime*pi*1.7*0.1)*128.0);
 DstRect.Top:=((fVulkanCanvas.Height-fVulkanSpriteSmiley0.Height)*0.5)+(cos(fTime*pi*2.3*0.1)*128.0);
 DstRect.Right:=DstRect.Left+fVulkanSpriteSmiley0.Width;
 DstRect.Bottom:=DstRect.Top+fVulkanSpriteSmiley0.Height;
 fVulkanCanvas.DrawSprite(fVulkanSpriteSmiley0,SrcRect,DstRect,TpvVector2.Create(fVulkanSpriteSmiley0.Width*0.5,fVulkanSpriteSmiley0.Height*0.5),sin(fTime*pi*2.1*0.1)*pi*2.0,TpvVector4.Create(1.0,1.0,1.0,0.9));
}
 fVulkanCanvas.BlendingMode:=vsbbmAdditiveBlending;

 SrcRect:=TpvRect.Create(0,0,fVulkanSpriteDancer0.Width,fVulkanSpriteDancer0.Height);
 DstRect.Left:=((fVulkanCanvas.Width-fVulkanSpriteDancer0.Width)*0.5)+(cos(fTime*pi*1.7*0.1)*128.0);
 DstRect.Top:=((fVulkanCanvas.Height-fVulkanSpriteDancer0.Height)*0.5)+(sin(fTime*pi*2.3*0.1)*128.0);
 DstRect.Right:=DstRect.Left+fVulkanSpriteDancer0.Width;
 DstRect.Bottom:=DstRect.Top+fVulkanSpriteDancer0.Height;
 fVulkanCanvas.DrawSprite(fVulkanSpriteDancer0,SrcRect,DstRect,TpvVector2.Create(fVulkanSpriteDancer0.Width*0.5,fVulkanSpriteDancer0.Height*0.5),cos(fTime*pi*1.5*0.1)*pi*2.0,TpvVector4.Create(1.0,1.0,1.0,0.5));

 fVulkanCanvas.BlendingMode:=vsbbmAlphaBlending;

 SrcRect:=TpvRect.Create(0,0,fVulkanSpriteSmiley0.Width,fVulkanSpriteSmiley0.Height);
 DstRect.Left:=((fVulkanCanvas.Width-fVulkanSpriteSmiley0.Width)*0.5)+(sin(fTime*pi*1.7*0.1)*128.0);
 DstRect.Top:=((fVulkanCanvas.Height-fVulkanSpriteSmiley0.Height)*0.5)+(cos(fTime*pi*2.3*0.1)*128.0);
 DstRect.Right:=DstRect.Left+fVulkanSpriteSmiley0.Width;
 DstRect.Bottom:=DstRect.Top+fVulkanSpriteSmiley0.Height;
 fVulkanCanvas.DrawSprite(fVulkanSpriteSmiley0,SrcRect,DstRect,TpvVector2.Create(fVulkanSpriteSmiley0.Width*0.5,fVulkanSpriteSmiley0.Height*0.5),sin(fTime*pi*2.1*0.1)*pi*2.0,TpvVector4.Create(1.0,1.0,1.0,1.0));

 fVulkanCanvas.RenderingMode:=vsbrmFont;

 fVulkanCanvas.BlendingMode:=vsbbmAlphaBlending;

 LocalFontSize:=(-56.0)+(sin((fTime*0.1)*pi*2.0)*48.0);

 rbs:='This is an example text';

 fVulkanFont.Draw(fVulkanCanvas,
                  rbs,
                  ((fVulkanCanvas.Width-fVulkanFont.TextWidth(rbs,LocalFontSize))*0.5)+0.0,
                  ((fVulkanCanvas.Height-fVulkanFont.TextHeight(rbs,LocalFontSize))*0.5)+(sin(fTime*pi*0.07)*(fVulkanCanvas.Height*0.3275)),
                  LocalFontSize,
                  TpvVector4.Create((sin((fTime*0.43)*pi*2.0)*0.5)+0.5,
                                    (cos((fTime*0.29)*pi*2.0)*0.5)+0.5,
                                    (sin((fTime*0.23)*pi*2.0)*0.5)+0.5,
                                    (cos((fTime*0.17)*pi*2.0)*0.25)+0.75));

 fVulkanCanvas.Stop;

 ExampleApplication.TextOverlay.AddText(pvApplication.Width*0.5,ExampleApplication.TextOverlay.FontCharHeight*1.0,2.0,toaCenter,'Canvas');
 fStartY:=pvApplication.Height-((((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize)*1.25)-(4*FontSize));
 cy:=fStartY;
 for Index:=0 to 0 do begin
  IsSelected:=fSelectedIndex=Index;
  s:=' '+Options[Index]+' ';
  if IsSelected then begin
   s:='>'+s+'<';
  end;
  ExampleApplication.TextOverlay.AddText(pvApplication.Width*0.5,cy,FontSize,toaCenter,s,MenuColors[IsSelected,0,0],MenuColors[IsSelected,0,1],MenuColors[IsSelected,0,2],MenuColors[IsSelected,0,3],MenuColors[IsSelected,1,0],MenuColors[IsSelected,1,1],MenuColors[IsSelected,1,2],MenuColors[IsSelected,1,3]);
  cy:=cy+((ExampleApplication.TextOverlay.FontCharHeight+4)*FontSize);
 end;

 fTime:=fTime+aDeltaTime;

 fReady:=true;
end;

procedure TScreenExampleCanvas.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
const Offsets:array[0..0] of TVkDeviceSize=(0);
var BufferIndex,Size:TpvInt32;
    VulkanVertexBuffer:TpvVulkanBuffer;
    VulkanCommandBuffer:TpvVulkanCommandBuffer;
    VulkanSwapChain:TpvVulkanSwapChain;
    p:pointer;
begin

 begin

  begin

   VulkanCommandBuffer:=fVulkanRenderCommandBuffers[aSwapChainImageIndex];
   VulkanSwapChain:=pvApplication.VulkanSwapChain;

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
 RegisterExample('Canvas',TScreenExampleCanvas);
end.
