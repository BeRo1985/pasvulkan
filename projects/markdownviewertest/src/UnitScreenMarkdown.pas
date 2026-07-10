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
       fFontRegular:TpvFont;
       fFontBold:TpvFont;
       fFontItalic:TpvFont;
       fFontBoldItalic:TpvFont;
       fFontMono:TpvFont;
       fMarkDownRenderer:TpvMarkDownRenderer;
       fLayoutWidth:TpvFloat;
       fContentWidth:TpvFloat;
       fContentHeight:TpvFloat;
       fScrollY:TpvFloat;
       fReady:boolean;
       fKeyUp:boolean;
       fKeyDown:boolean;
       fKeyPageUp:boolean;
       fKeyPageDown:boolean;
       function GetSampleMarkDown:TpvUTF8String;
       function LoadFont(const aAssetName:TpvUTF8String):TpvFont;
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

uses {$if defined(Windows)}Windows,ShellApi,{$elseif defined(fpc) and defined(unix)}BaseUnix,{$ifend}UnitApplication;

{$if defined(fpc) and defined(unix)}
// posix_spawn instead of fork/fpSystem: it doesn't duplicate the address space
// of a process that carries big Vulkan/driver state (glibc implements it with a
// vfork-style CLONE_VM, on macOS it is a real spawn syscall), and it can't
// deadlock the way a forked child of a multithreaded process can
function posix_spawn(aPID:pPid;aPath:PAnsiChar;aFileActions:pointer;aAttr:pointer;aArgv:PPAnsiChar;aEnvp:PPAnsiChar):cint; cdecl; external 'c' name 'posix_spawn';
{$ifend}

// hand an external http(s) link over to the OS default browser
procedure OpenURLInBrowser(const aURL:TpvUTF8String);
{$if defined(Windows)}
var WideURL:UnicodeString;
begin
 WideURL:=UnicodeString(aURL);
 ShellExecuteW(0,'open',PWideChar(WideURL),nil,nil,SW_SHOWNORMAL);
end;
{$elseif defined(fpc) and defined(unix)}
var ChildPID:TPid;
    CommandLine:TpvRawByteString;
    Argv:array[0..3] of PAnsiChar;
begin
 // single-quote the URL for the shell; embedded single quotes get URL-escaped
 CommandLine:={$if defined(darwin)}'open '''{$else}'xdg-open '''{$ifend}+RawByteString(StringReplace(String(aURL),'''','%27',[rfReplaceAll]))+''' >/dev/null 2>&1 &';
 Argv[0]:='sh';
 Argv[1]:='-c';
 Argv[2]:=PAnsiChar(CommandLine);
 Argv[3]:=nil;
 ChildPID:=0;
 if posix_spawn(@ChildPID,'/bin/sh',nil,nil,PPAnsiChar(@Argv[0]),envp)=0 then begin
  // the shell backgrounds the opener and exits right away, so this returns
  // immediately, leaves no zombie behind and never waits on the browser
  repeat
  until (FpWaitPid(ChildPID,nil,0)<>-1) or (fpgeterrno<>ESysEINTR);
 end;
end;
{$else}
begin
end;
{$ifend}

{ TScreenMarkdown }

function TScreenMarkdown.GetSampleMarkDown:TpvUTF8String;
begin
 result:='# PasVulkan Markdown Renderer'+#10+
         ''+#10+
         'This is a **TpvCanvas** rendering test for *PasVulkan.PasHTMLDownCanvasRenderer*.'+#10+
         ''+#10+
         '## Contents'+#10+
         ''+#10+
         '- [Inline formatting](#inline-formatting)'+#10+
         '- [Lists](#lists)'+#10+
         '- [Blockquote](#blockquote)'+#10+
         '- [Code block](#code-block)'+#10+
         '- [Table](#table)'+#10+
         '- [Links and anchors](#links-and-anchors)'+#10+
         ''+#10+
         '## Inline formatting'+#10+
         ''+#10+
         'Text can be **bold**, *italic*, ~~struck through~~ and contain `inline code`.'+#10+
         'You can also mix ***bold and italic*** together. **a**+**b** = c'+#10+
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
         '## Links and anchors'+#10+
         ''+#10+
         'Links are clickable: heading anchors like [Contents](#contents) scroll inside the'+#10+
         'document, external links like [rosseaux.net](https://rosseaux.net) open in the browser.'+#10+
         'Explicit targets also work: <a name="explicit-anchor"></a>this line carries the'+#10+
         'anchor [#explicit-anchor](#explicit-anchor).'+#10+
         ''+#10+
         '---'+#10+
         ''+#10+
         'That is the end of the sample document. [Back to top](#pasvulkan-markdown-renderer)'+#10;
end;

constructor TScreenMarkdown.Create;
begin

 inherited Create;

 fReady:=false;

 fKeyUp:=false;
 fKeyDown:=false;

 fKeyPageUp:=false;
 fKeyPageDown:=false;

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
 // soft pastel coral for headings, to sit next to the blue quote and green code accents
 fMarkDownRenderer.FontHeaderColor:=TpvVector4.Create(0.94,0.62,0.62,1.0);
 fMarkDownRenderer.BGCodeColor:=TpvVector4.Create(0.12,0.12,0.14,1.0);
 fMarkDownRenderer.FontCodeColor:=TpvVector4.Create(0.7,0.9,0.7,1.0);
 fMarkDownRenderer.BGMarkColor:=TpvVector4.Create(0.9,0.85,0.2,1.0);
 fMarkDownRenderer.FontMarkColor:=TpvVector4.Create(0.0,0.0,0.0,1.0);
 fMarkDownRenderer.BGThinkColor:=TpvVector4.Create(0.1,0.1,0.1,1.0);
 fMarkDownRenderer.FontThinkColor:=TpvVector4.Create(0.55,0.55,0.55,1.0);
 // light blue for clickable links, to stand out from the plain text
 fMarkDownRenderer.FontLinkColor:=TpvVector4.Create(0.4,0.65,1.0,1.0);

 fMarkDownRenderer.Parse(GetSampleMarkDown,false);

end;

destructor TScreenMarkdown.Destroy;
begin
 FreeAndNil(fMarkDownRenderer);
 inherited Destroy;
end;

function TScreenMarkdown.LoadFont(const aAssetName:TpvUTF8String):TpvFont;
var Stream:TStream;
    TrueTypeFont:TpvTrueTypeFont;
begin
 // build one signed-distance-field face into the shared font atlas; the range
 // is kept to Latin-1 to keep the atlas small (widen it for other scripts)
 Stream:=pvApplication.Assets.GetAssetStream(aAssetName);
 try
  TrueTypeFont:=TpvTrueTypeFont.Create(Stream,72);
  try
   TrueTypeFont.Size:=-64;
   TrueTypeFont.Hinting:=false;
   result:=TpvFont.CreateFromTrueTypeFont(fVulkanFontSpriteAtlas,
                                          TrueTypeFont,
                                          [TpvFontCodePointRange.Create(0,255)],
                                          true,
                                          2,
                                          1);
  finally
   TrueTypeFont.Free;
  end;
 finally
  Stream.Free;
 end;
end;

procedure TScreenMarkdown.Show;
var Index:TpvInt32;
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

 // build all faces into the shared signed-distance-field font atlas
 fFontRegular:=LoadFont('fonts/notosans.ttf');
 fFontBold:=LoadFont('fonts/notosansbold.ttf');
 fFontItalic:=LoadFont('fonts/notosansitalic.ttf');
 fFontBoldItalic:=LoadFont('fonts/notosansbolditalic.ttf');
 fFontMono:=LoadFont('fonts/notomono.ttf');

 fVulkanFontSpriteAtlas.Upload(pvApplication.VulkanDevice.GraphicsQueue,
                               fVulkanGraphicsCommandBuffer,
                               fVulkanGraphicsCommandBufferFence,
                               pvApplication.VulkanDevice.TransferQueue,
                               fVulkanTransferCommandBuffer,
                               fVulkanTransferCommandBufferFence);

 // proportional Noto Sans family + Noto Mono for code; the monospace
 // bold/italic slots stay unset and fall back to MonoFont in SelectFont
 fMarkDownRenderer.Font:=fFontRegular;
 fMarkDownRenderer.BoldFont:=fFontBold;
 fMarkDownRenderer.ItalicFont:=fFontItalic;
 fMarkDownRenderer.BoldItalicFont:=fFontBoldItalic;
 fMarkDownRenderer.MonoFont:=fFontMono;

 // force a re-layout on the next Update
 fLayoutWidth:=-1.0;

end;

procedure TScreenMarkdown.Hide;
var Index:TpvInt32;
begin
 fMarkDownRenderer.Font:=nil;
 fMarkDownRenderer.BoldFont:=nil;
 fMarkDownRenderer.ItalicFont:=nil;
 fMarkDownRenderer.BoldItalicFont:=nil;
 fMarkDownRenderer.MonoFont:=nil;
 FreeAndNil(fFontRegular);
 FreeAndNil(fFontBold);
 FreeAndNil(fFontItalic);
 FreeAndNil(fFontBoldItalic);
 FreeAndNil(fFontMono);
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
 result:=inherited KeyEvent(aKeyEvent);
 if aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down then begin
  case aKeyEvent.KeyCode of
   KEYCODE_ESCAPE:begin
    pvApplication.Terminate;
   end;
   KEYCODE_HOME:begin
    fScrollY:=0;
   end;
   KEYCODE_END:begin
    fScrollY:=Infinity;
   end;
   KEYCODE_SPACE:begin
    fScrollY:=fScrollY+480.0;
   end;
   else begin
   end;
  end;
 end;
 if aKeyEvent.KeyEventType in [TpvApplicationInputKeyEventType.Down,TpvApplicationInputKeyEventType.Up] then begin
  case aKeyEvent.KeyCode of
   KEYCODE_UP,KEYCODE_W:begin
    fKeyUp:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_DOWN,KEYCODE_S:begin
    fKeyDown:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_PAGEUP:begin
    fKeyPageUp:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   KEYCODE_PAGEDOWN:begin
    fKeyPageDown:=aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down;
   end;
   else begin
   end;
  end;
 end;
end;

function TScreenMarkdown.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var Href:TpvMarkDownRendererUTF8String;
    AnchorY:TpvMarkDownRendererFloat;
begin
 result:=false;
 // left click on a link: scroll to in-document anchors, open external http(s)
 // links in the OS default browser (the canvas covers the whole window 1:1,
 // so the pointer position only has to be shifted by the render offset)
 if fReady and
    (aPointerEvent.PointerEventType=TpvApplicationInputPointerEventType.Down) and
    (TpvApplicationInputPointerButton.Left in aPointerEvent.Buttons) then begin
  if fMarkDownRenderer.HitTestLink(aPointerEvent.Position.x-Margin,
                                   (aPointerEvent.Position.y-Margin)+fScrollY,
                                   Href) then begin
   if (length(Href)>0) and (Href[1]='#') then begin
    if fMarkDownRenderer.ResolveAnchor(Href,AnchorY) then begin
     fScrollY:=AnchorY; // clamped against the content height in Update
    end;
   end else if (Copy(String(Href),1,7)='http://') or (Copy(String(Href),1,8)='https://') then begin
    OpenURLInBrowser(Href);
   end;
   result:=true;
  end;
 end;
end;

function TScreenMarkdown.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 // simple mouse-wheel vertical scrolling through the document
 fScrollY:=fScrollY-(aRelativeAmount.y*(fMarkDownRenderer.BaseFontSize*3.0));
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
 if fKeyUp<>fKeyDown then begin
  fScrollY:=fScrollY+((aDeltaTime*128.0)*(((ord(fKeyDown) and 1)*2)-1));
 end else if fKeyPageUp<>fKeyPageDown then begin
  fScrollY:=fScrollY+((aDeltaTime*(fVulkanCanvas.Height*4.0))*(((ord(fKeyPageDown) and 1)*2)-1));
 end;
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
