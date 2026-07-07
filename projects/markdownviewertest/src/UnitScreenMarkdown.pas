unit UnitScreenMarkdown;
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

{$ifdef fpc}
 {$optimization level1}
{$ifend}

interface

uses SysUtils,
     Classes,
     Math,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Sprites,
     PasVulkan.Canvas,
     PasVulkan.Font,
     PasVulkan.TrueTypeFont,
     PasVulkan.PasHTMLDownCanvasRenderer;

type { TScreenMarkdown }

     TScreenMarkdown=class(TpvApplicationScreen)
      public
       const Margin=24.0;
      private
       fVulkanGraphicsCommandPool:TpvVulkanCommandPool;
       fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
       fVulkanTransferCommandPool:TpvVulkanCommandPool;
       fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanTransferCommandBufferFence:TpvVulkanFence;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fVulkanCommandPool:TpvVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxInFlightFrames-1] of TpvVulkanSemaphore;
       fVulkanFontSpriteAtlas:TpvSpriteAtlas;
       fVulkanCanvas:TpvCanvas;
       fVulkanFont:TpvFont;
       fMarkDownRenderer:TpvMarkDownRenderer;
       fLayoutWidth:TpvFloat;
       fContentWidth:TpvFloat;
       fContentHeight:TpvFloat;
       fScrollY:TpvFloat;
       fReady:boolean;
       function GetSampleMarkDown:TpvUTF8String;
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

       procedure Check(const aDeltaTime:TpvDouble); override;

       procedure Update(const aDeltaTime:TpvDouble); override;

       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;

     end;

implementation

uses UnitApplication;

{ TScreenMarkdown }

function TScreenMarkdown.GetSampleMarkDown:TpvUTF8String;
begin
 result:='# PasVulkan Markdown Renderer'+#10+
         ''+#10+
         'This is a **TpvCanvas** rendering test for *PasVulkan.PasHTMLDownCanvasRenderer*.'+#10+
         ''+#10+
         '## Inline formatting'+#10+
         ''+#10+
         'Text can be **bold**, *italic*, ~~struck through~~ and contain `inline code`.'+#10+
         'You can also mix ***bold and italic*** together.'+#10+
         ''+#10+
         '## Lists'+#10+
         ''+#10+
         '- First item'+#10+
         '- Second item with a [link](https://rosseaux.net)'+#10+
         '- Third item'+#10+
         ''+#10+
         '1. One'+#10+
         '2. Two'+#10+
         '3. Three'+#10+
         ''+#10+
         '## Blockquote'+#10+
         ''+#10+
         '> The quick brown fox jumps over the lazy dog.'+#10+
         ''+#10+
         '## Code block'+#10+
         ''+#10+
         '```'+#10+
         'procedure Hello;'+#10+
         'begin'+#10+
         ' writeln(''Hello, world!'');'+#10+
         'end;'+#10+
         '```'+#10+
         ''+#10+
         '## Table'+#10+
         ''+#10+
         '| Name | Value |'+#10+
         '| ---- | ----- |'+#10+
         '| Alpha | 1 |'+#10+
         '| Beta | 2 |'+#10+
         ''+#10+
         '---'+#10+
         ''+#10+
         'That is the end of the sample document.'+#10;
end;

constructor TScreenMarkdown.Create;
begin

 inherited Create;

 fReady:=false;

 fScrollY:=0.0;
 fLayoutWidth:=-1.0;
 fContentWidth:=0.0;
 fContentHeight:=0.0;

 fMarkDownRenderer:=TpvMarkDownRenderer.Create;
 fMarkDownRenderer.BaseFontSize:=16;

 // dark theme so the document is readable on the black clear color
 fMarkDownRenderer.BGColor:=TpvVector4.Create(0.0,0.0,0.0,1.0);
 fMarkDownRenderer.FontColor:=TpvVector4.Create(0.9,0.9,0.9,1.0);
 fMarkDownRenderer.FontQuoteColor:=TpvVector4.Create(0.6,0.75,0.9,1.0);
 fMarkDownRenderer.BGCodeColor:=TpvVector4.Create(0.12,0.12,0.14,1.0);
 fMarkDownRenderer.FontCodeColor:=TpvVector4.Create(0.7,0.9,0.7,1.0);
 fMarkDownRenderer.BGMarkColor:=TpvVector4.Create(0.9,0.85,0.2,1.0);
 fMarkDownRenderer.FontMarkColor:=TpvVector4.Create(0.0,0.0,0.0,1.0);
 fMarkDownRenderer.BGThinkColor:=TpvVector4.Create(0.1,0.1,0.1,1.0);
 fMarkDownRenderer.FontThinkColor:=TpvVector4.Create(0.55,0.55,0.55,1.0);

 fMarkDownRenderer.Parse(GetSampleMarkDown,false);

end;

destructor TScreenMarkdown.Destroy;
begin
 FreeAndNil(fMarkDownRenderer);
 inherited Destroy;
end;

procedure TScreenMarkdown.Show;
var Stream:TStream;
    Index:TpvInt32;
    TrueTypeFont:TpvTrueTypeFont;
begin
 inherited Show;

 fVulkanGraphicsCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                         pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                         TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanGraphicsCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                         pvApplication.VulkanDevice.TransferQueueFamilyIndex,
                                                         TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanTransferCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                 pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                 TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
 for Index:=0 to MaxInFlightFrames-1 do begin
  fVulkanRenderCommandBuffers[Index]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fVulkanRenderSemaphores[Index]:=TpvVulkanSemaphore.Create(pvApplication.VulkanDevice);
 end;

 fVulkanRenderPass:=nil;

 fVulkanCanvas:=TpvCanvas.Create(pvApplication.VulkanDevice,
                                 pvApplication.VulkanPipelineCache,
                                 MaxInFlightFrames);

 fVulkanFontSpriteAtlas:=TpvSpriteAtlas.Create(pvApplication.VulkanDevice,false);
 fVulkanFontSpriteAtlas.MipMaps:=false;
 fVulkanFontSpriteAtlas.UseConvexHullTrimming:=false;

 // build the signed-distance-field font atlas from the bundled TrueType font
 Stream:=pvApplication.Assets.GetAssetStream('fonts/vga.ttf');
 try
  TrueTypeFont:=TpvTrueTypeFont.Create(Stream,72);
  try
   TrueTypeFont.Size:=-64;
   TrueTypeFont.Hinting:=false;
   fVulkanFont:=TpvFont.CreateFromTrueTypeFont(fVulkanFontSpriteAtlas,
                                               TrueTypeFont,
                                               [TpvFontCodePointRange.Create(0,65535)],
                                               true,
                                               2,
                                               1);
  finally
   TrueTypeFont.Free;
  end;
 finally
  Stream.Free;
 end;

 fVulkanFontSpriteAtlas.Upload(pvApplication.VulkanDevice.GraphicsQueue,
                               fVulkanGraphicsCommandBuffer,
                               fVulkanGraphicsCommandBufferFence,
                               pvApplication.VulkanDevice.TransferQueue,
                               fVulkanTransferCommandBuffer,
                               fVulkanTransferCommandBufferFence);

 // only a single face is bundled here, so use it for both proportional and
 // monospace slots; the bold/italic variants fall back to it in SelectFont
 fMarkDownRenderer.Font:=fVulkanFont;
 fMarkDownRenderer.MonoFont:=fVulkanFont;

 // force a re-layout on the next Update
 fLayoutWidth:=-1.0;

end;

procedure TScreenMarkdown.Hide;
var Index:TpvInt32;
begin
 fMarkDownRenderer.Font:=nil;
 fMarkDownRenderer.MonoFont:=nil;
 FreeAndNil(fVulkanFont);
 FreeAndNil(fVulkanFontSpriteAtlas);
 FreeAndNil(fVulkanCanvas);
 FreeAndNil(fVulkanRenderPass);
 for Index:=0 to MaxInFlightFrames-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[Index]);
  FreeAndNil(fVulkanRenderSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanTransferCommandPool);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandPool);
 inherited Hide;
end;

procedure TScreenMarkdown.Resume;
begin
 inherited Resume;
end;

procedure TScreenMarkdown.Pause;
begin
 inherited Pause;
end;

procedure TScreenMarkdown.Resize(const aWidth,aHeight:TpvInt32);
begin
 inherited Resize(aWidth,aHeight);
end;

procedure TScreenMarkdown.AfterCreateSwapChain;
var Index:TpvInt32;
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
                                                                                                                              VK_IMAGE_LAYOUT_UNDEFINED,
                                                                                                                              VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
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
                                                                                                                             VK_IMAGE_LAYOUT_UNDEFINED,
                                                                                                                             VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                                                            ),
                                                                                  VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                 ),
                                         []);
 fVulkanRenderPass.AddSubpassDependency(VK_SUBPASS_EXTERNAL,
                                        0,
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT),
                                        TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                        TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT),
                                        TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
 fVulkanRenderPass.AddSubpassDependency(0,
                                        VK_SUBPASS_EXTERNAL,
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT) or TVkPipelineStageFlags(VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                        TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT) or TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT),
                                        TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                        TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
 fVulkanRenderPass.Initialize;

 fVulkanRenderPass.ClearValues[0].color.float32[0]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[1]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[2]:=0.0;
 fVulkanRenderPass.ClearValues[0].color.float32[3]:=1.0;

 fVulkanCanvas.VulkanRenderPass:=fVulkanRenderPass;
 fVulkanCanvas.CountBuffers:=pvApplication.CountInFlightFrames;
 fVulkanCanvas.Width:=pvApplication.Width;
 fVulkanCanvas.Height:=pvApplication.Height;
 fVulkanCanvas.Viewport.x:=0;
 fVulkanCanvas.Viewport.y:=0;
 fVulkanCanvas.Viewport.width:=pvApplication.Width;
 fVulkanCanvas.Viewport.height:=pvApplication.Height;

 // window size changed, so the document has to be laid out again
 fLayoutWidth:=-1.0;

 for Index:=0 to length(fVulkanRenderCommandBuffers)-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[Index]);
  fVulkanRenderCommandBuffers[Index]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
 end;

end;

procedure TScreenMarkdown.BeforeDestroySwapChain;
begin
 fVulkanCanvas.VulkanRenderPass:=nil;
 FreeAndNil(fVulkanRenderPass);
 inherited BeforeDestroySwapChain;
end;

function TScreenMarkdown.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=false;
end;

function TScreenMarkdown.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
begin
 result:=false;
end;

function TScreenMarkdown.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 // simple mouse-wheel vertical scrolling through the document
 fScrollY:=fScrollY+(aRelativeAmount.y*(fMarkDownRenderer.BaseFontSize*3.0));
 result:=true;
end;

function TScreenMarkdown.CanBeParallelProcessed:boolean;
begin
 result:=true;
end;

procedure TScreenMarkdown.Check(const aDeltaTime:TpvDouble);
begin
 inherited Check(aDeltaTime);
end;

procedure TScreenMarkdown.Update(const aDeltaTime:TpvDouble);
var MaxWidth,MaxScrollY:TpvFloat;
begin

 inherited Update(aDeltaTime);

 fVulkanCanvas.Start(pvApplication.UpdateInFlightFrameIndex);

 fVulkanCanvas.ViewMatrix:=TpvMatrix4x4.Identity;

 fVulkanCanvas.BlendingMode:=TpvCanvasBlendingMode.AlphaBlending;

 // (re-)calculate the layout whenever the available width changed
 MaxWidth:=fVulkanCanvas.Width-(Margin*2.0);
 if MaxWidth<1.0 then begin
  MaxWidth:=1.0;
 end;
 if fLayoutWidth<>MaxWidth then begin
  fMarkDownRenderer.Calculate(fVulkanCanvas,MaxWidth,fContentWidth,fContentHeight);
  fLayoutWidth:=MaxWidth;
 end;

 // clamp the scroll offset to the actual content height
 MaxScrollY:=Max(0.0,(fContentHeight+(Margin*2.0))-fVulkanCanvas.Height);
 if fScrollY<0.0 then begin
  fScrollY:=0.0;
 end else if fScrollY>MaxScrollY then begin
  fScrollY:=MaxScrollY;
 end;

 fMarkDownRenderer.Render(fVulkanCanvas,Margin,Margin-fScrollY);

 fVulkanCanvas.Stop;

 fReady:=true;

end;

procedure TScreenMarkdown.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
var VulkanCommandBuffer:TpvVulkanCommandBuffer;
    VulkanSwapChain:TpvVulkanSwapChain;
begin

 VulkanCommandBuffer:=fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex];
 VulkanSwapChain:=pvApplication.VulkanSwapChain;

 VulkanCommandBuffer.Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));

 VulkanCommandBuffer.BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

 fVulkanCanvas.ExecuteUpload(pvApplication.VulkanDevice.TransferQueue,
                             fVulkanTransferCommandBuffer,
                             fVulkanTransferCommandBufferFence,
                             pvApplication.DrawInFlightFrameIndex);

 fVulkanRenderPass.BeginRenderPass(VulkanCommandBuffer,
                                   pvApplication.VulkanFrameBuffers[aSwapChainImageIndex],
                                   VK_SUBPASS_CONTENTS_INLINE,
                                   0,
                                   0,
                                   VulkanSwapChain.Width,
                                   VulkanSwapChain.Height);

 fVulkanCanvas.ExecuteDraw(VulkanCommandBuffer,
                           pvApplication.DrawInFlightFrameIndex);

 fVulkanRenderPass.EndRenderPass(VulkanCommandBuffer);

 VulkanCommandBuffer.EndRecording;

 VulkanCommandBuffer.Execute(pvApplication.VulkanDevice.GraphicsQueue,
                             TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                             aWaitSemaphore,
                             fVulkanRenderSemaphores[pvApplication.DrawInFlightFrameIndex],
                             aWaitFence,
                             false);

 aWaitSemaphore:=fVulkanRenderSemaphores[pvApplication.DrawInFlightFrameIndex];

end;

initialization
end.
