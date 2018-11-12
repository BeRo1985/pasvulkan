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

interface

uses SysUtils,
     Classes,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     PasVulkan.Sprites,
     PasVulkan.Canvas,
     PasVulkan.GUI,
     PasVulkan.Font,
     PasVulkan.TrueTypeFont,
     PasVulkan.TextEditor;

type TScreenMain=class(TpvApplicationScreen)
      private
       fVulkanGraphicsCommandPool:TpvVulkanCommandPool;
       fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
       fVulkanTransferCommandPool:TpvVulkanCommandPool;
       fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanTransferCommandBufferFence:TpvVulkanFence;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fVulkanCommandPool:TpvVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxSwapChainImages-1] of TpvVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxSwapChainImages-1] of TpvVulkanSemaphore;
       fVulkanCanvas:TpvCanvas;
       fScreenToCanvasScale:TpvVector2;
       fGUIInstance:TpvGUIInstance;
       fGUIRootPanel:TpvGUIPanel;
       fGUIRootSplitterPanel0:TpvGUISplitterPanel;
       fGUIRootSplitterPanel1:TpvGUISplitterPanel;
       fGUILeftTabPanel:TpvGUITabPanel;
       fGUILeftToolPanel:TpvGUIPanel;
       fGUIVulkanCanvas:TpvGUIVulkanCanvas;
       fGUIGridSizePanel:TpvGUIPanel;
       fLabelGridSizeWidth:TpvGUILabel;
       fIntegerEditGridSizeWidth:TpvGUIIntegerEdit;
       fLabelGridSizeHeight:TpvGUILabel;
       fIntegerEditGridSizeHeight:TpvGUIIntegerEdit;
       fLabelGridSizeDepth:TpvGUILabel;
       fIntegerEditGridSizeDepth:TpvGUIIntegerEdit;
       fGUIWorldSizePanel:TpvGUIPanel;
       fLabelWorldSizeWidth:TpvGUILabel;
       fFloatEditWorldSizeWidth:TpvGUIFloatEdit;
       fLabelWorldSizeHeight:TpvGUILabel;
       fFloatEditWorldSizeHeight:TpvGUIFloatEdit;
       fLabelWorldSizeDepth:TpvGUILabel;
       fFloatEditWorldSizeDepth:TpvGUIFloatEdit;
       fGUISignedDistanceFieldCodeEditorTab:TpvGUITab;
       fGUIMeshFragmentCodeEditorTab:TpvGUITab;
       fGUISignedDistanceFieldCodeEditor:TpvGUIMultiLineTextEdit;
       fGUIMeshFragmentCodeEditor:TpvGUIMultiLineTextEdit;
       fGUIUpdateButton:TpvGUIButton;
       fGUIUpdateProgressBar:TpvGUIProgressBar;
       fGUIWindow:TpvGUIWindow;
       fGUILabel:TpvGUILabel;
       fGUIButton:TpvGUIButton;
       fGUITextEdit:TpvGUITextEdit;
       fGUIOtherWindow:TpvGUIWindow;
       fGUIYetOtherWindow:TpvGUIWindow;
       fLastMousePosition:TpvVector2;
       fLastMouseButtons:TpvApplicationInputPointerButtons;
       fReady:boolean;
       fNewProjectMessageDialogVisible:boolean;
       fTerminationMessageDialogVisible:boolean;
       fTime:TpvDouble;
       procedure NewProject;
       procedure OnNewProjectMessageDialogButtonClick(const aSender:TpvGUIObject;const aID:TpvInt32);
       procedure OnNewProjectMessageDialogDestroy(const aSender:TpvGUIObject);
       procedure ShowNewProjectMessageDialogDestroy(const aSender:TpvGUIObject);
       procedure OnTerminationMessageDialogButtonClick(const aSender:TpvGUIObject;const aID:TpvInt32);
       procedure OnTerminationMessageDialogDestroy(const aSender:TpvGUIObject);
       procedure ShowTerminationMessageDialogDestroy(const aSender:TpvGUIObject);
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

constructor TScreenMain.Create;
begin
 inherited Create;

 fReady:=false;

 fNewProjectMessageDialogVisible:=false;

 fTerminationMessageDialogVisible:=false;

 fTime:=0.48;

 fLastMousePosition:=TpvVector2.Null;

 fLastMouseButtons:=[];

end;

destructor TScreenMain.Destroy;
begin
 inherited Destroy;
end;

procedure TScreenMain.NewProject;
begin

 fGUISignedDistanceFieldCodeEditor.Text:=#13#10+
                                         'const float normalOffsetFactor = 1e-4;'#13#10+
                                         #13#10+
                                         'float getDistance(vec3 position){'#13#10+
                                         '  return length(position) - 1.0;'#13#10+
                                         '}'#13#10+
                                         #13#10+
                                         'mat2x4 getParameters(vec3 position){'#13#10+
                                         '  return mat2x4(vec4(vec3(1.0), 0.0), // first three values => rgb color'#13#10+
                                         '                vec4(0.0));'#13#10+
                                         '}'#13#10;

 fGUIMeshFragmentCodeEditor.Text:=#13#10+
                                  'vec4 getFragmentColor(vec3 position,'#13#10+
                                  '                      mat3 tangentSpace,'#13#10+
                                  '                      mat2x4 parameters,'#13#10+
                                  '                      vec3 rayDirection){'#13#10+
                                  '  return vec4(max(0.0,'#13#10+
                                  '                  -dot(tangentSpace[2],'#13#10+
                                  '                       rayDirection)) * parameters[0].xyz,'#13#10+
                                  '              1.0);'#13#10+
                                  '}'#13#10;

 fIntegerEditGridSizeWidth.Value:=64;

 fIntegerEditGridSizeHeight.Value:=64;

 fIntegerEditGridSizeDepth.Value:=64;

 fFloatEditWorldSizeWidth.Value:=2.0;

 fFloatEditWorldSizeHeight.Value:=2.0;

 fFloatEditWorldSizeDepth.Value:=2.0;

end;

procedure TScreenMain.OnNewProjectMessageDialogButtonClick(const aSender:TpvGUIObject;const aID:TpvInt32);
begin
 if aID=0 then begin
  NewProject;
 end;
end;

procedure TScreenMain.OnNewProjectMessageDialogDestroy(const aSender:TpvGUIObject);
begin
 fNewProjectMessageDialogVisible:=false;
end;

procedure TScreenMain.ShowNewProjectMessageDialogDestroy(const aSender:TpvGUIObject);
var MessageDialog:TpvGUIMessageDialog;
begin
 if not fNewProjectMessageDialogVisible then begin
  fNewProjectMessageDialogVisible:=true;
  MessageDialog:=TpvGUIMessageDialog.Create(fGUIInstance,
                                            'Question',
                                            'Do you really want to create a new project?',
                                            [TpvGUIMessageDialogButton.Create(0,'Yes',KEYCODE_RETURN,fGUIInstance.Skin.IconThumbUp,24.0),
                                             TpvGUIMessageDialogButton.Create(1,'No',KEYCODE_ESCAPE,fGUIInstance.Skin.IconThumbDown,24.0)],
                                            fGUIInstance.Skin.IconDialogQuestion);
  MessageDialog.OnButtonClick:=OnNewProjectMessageDialogButtonClick;
  MessageDialog.OnDestroy:=OnNewProjectMessageDialogDestroy;
 end;
end;

procedure TScreenMain.OnTerminationMessageDialogButtonClick(const aSender:TpvGUIObject;const aID:TpvInt32);
begin
 if aID=0 then begin
  pvApplication.Terminate;
 end;
end;

procedure TScreenMain.OnTerminationMessageDialogDestroy(const aSender:TpvGUIObject);
begin
 fTerminationMessageDialogVisible:=false;
end;

procedure TScreenMain.ShowTerminationMessageDialogDestroy(const aSender:TpvGUIObject);
var MessageDialog:TpvGUIMessageDialog;
begin
 if not fTerminationMessageDialogVisible then begin
  fTerminationMessageDialogVisible:=true;
  MessageDialog:=TpvGUIMessageDialog.Create(fGUIInstance,
                                            'Question',
                                            'Do you really want to quit this application?',
                                            [TpvGUIMessageDialogButton.Create(0,'Yes',KEYCODE_RETURN,fGUIInstance.Skin.IconThumbUp,24.0),
                                             TpvGUIMessageDialogButton.Create(1,'No',KEYCODE_ESCAPE,fGUIInstance.Skin.IconThumbDown,24.0)],
                                            fGUIInstance.Skin.IconDialogQuestion);
  MessageDialog.OnButtonClick:=OnTerminationMessageDialogButtonClick;
  MessageDialog.OnDestroy:=OnTerminationMessageDialogDestroy;
 end;
end;

procedure TScreenMain.Show;
var Index:TpvInt32;
    WindowMenu:TpvGUIWindowMenu;
    MenuItem:TpvGUIMenuItem;
    PopupMenu:TpvGUIPopupMenu;
//    Popup:TpvGUIPopup;
    Panel:TpvGUIPanel;
    Window:TpvGUIWindow;
    IntegerEdit:TpvGUIIntegerEdit;
    FloatEdit:TpvGUIFloatEdit;
    ScrollBar:TpvGUIScrollBar;
    Slider:TpvGUISlider;
    ScrollPanel:TpvGUIScrollPanel;
    TabPanel:TpvGUITabPanel;
begin

 inherited Show;

 pvApplication.VisibleMouseCursor:=false;

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
 for Index:=0 to MaxSwapChainImages-1 do begin
  fVulkanRenderCommandBuffers[Index]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  fVulkanRenderSemaphores[Index]:=TpvVulkanSemaphore.Create(pvApplication.VulkanDevice);
 end;

 fVulkanRenderPass:=nil;

 fVulkanCanvas:=TpvCanvas.Create(pvApplication.VulkanDevice,
                                 pvApplication.VulkanDevice.GraphicsQueue,
                                 fVulkanGraphicsCommandBuffer,
                                 fVulkanGraphicsCommandBufferFence,
                                 pvApplication.VulkanDevice.TransferQueue,
                                 fVulkanTransferCommandBuffer,
                                 fVulkanTransferCommandBufferFence,
                                 pvApplication.VulkanPipelineCache,
                                 MaxSwapChainImages);

 fGUIInstance:=TpvGUIInstance.Create(pvApplication.VulkanDevice,fVulkanCanvas);

 fGUIInstance.Width:=pvApplication.Width;
 fGUIInstance.Height:=pvApplication.Height;

 begin
  WindowMenu:=fGUIInstance.AddMenu;

  MenuItem:=TpvGUIMenuItem.Create(WindowMenu);
  MenuItem.Caption:='File';

  begin
   PopupMenu:=TpvGUIPopupMenu.Create(MenuItem);

   MenuItem:=TpvGUIMenuItem.Create(PopupMenu);
   MenuItem.Icon:=fGUIInstance.Skin.IconContentDelete;
   MenuItem.IconHeight:=12;
   MenuItem.Caption:='New';
   MenuItem.ShortcutHint:='Shift+Ctrl+N';
   MenuItem.OnClick:=ShowNewProjectMessageDialogDestroy;

   MenuItem:=TpvGUIMenuItem.Create(PopupMenu);
   MenuItem.Icon:=fGUIInstance.Skin.IconContentPaste;
   MenuItem.IconHeight:=12;
   MenuItem.Caption:='Open';
   MenuItem.ShortcutHint:='Ctrl+O';

   MenuItem:=TpvGUIMenuItem.Create(PopupMenu);
   MenuItem.Icon:=fGUIInstance.Skin.IconContentCopy;
   MenuItem.IconHeight:=12;
   MenuItem.Caption:='Save';
   MenuItem.ShortcutHint:='Ctrl+S';

   MenuItem:=TpvGUIMenuItem.Create(PopupMenu);
   MenuItem.Icon:=fGUIInstance.Skin.IconContentCopy;
   MenuItem.IconHeight:=12;
   MenuItem.Caption:='Save as';
   MenuItem.ShortcutHint:='Shift+Ctrl+S';

   MenuItem:=TpvGUIMenuItem.Create(PopupMenu);
   MenuItem.Caption:='-';

   MenuItem:=TpvGUIMenuItem.Create(PopupMenu);
   MenuItem.Icon:=fGUIInstance.Skin.IconWindowClose;
   MenuItem.IconHeight:=12;
   MenuItem.Caption:='Exit';
   MenuItem.ShortcutHint:='Alt+F4';
   MenuItem.OnClick:=ShowTerminationMessageDialogDestroy;

  end;

 end;

 fGUIInstance.Content.Layout:=TpvGUIFillLayout.Create(fGUIInstance.Content,0.0);

 fGUIRootPanel:=TpvGUIPanel.Create(fGUIInstance.Content);
 fGUIRootPanel.Background:=true;
 fGUIRootPanel.Layout:=TpvGUIFillLayout.Create(fGUIRootPanel,0.0);

 fGUIRootSplitterPanel0:=TpvGUISplitterPanel.Create(fGUIRootPanel);
 fGUIRootSplitterPanel0.Orientation:=TpvGUISplitterPanelOrientation.Horizontal;
 fGUIRootSplitterPanel0.LeftTopPanel.Layout:=TpvGUIAdvancedGridLayout.Create(fGUIRootSplitterPanel0.LeftTopPanel,4.0);
 fGUIRootSplitterPanel0.LeftTopPanel.Background:=true;
 fGUIRootSplitterPanel0.RightBottomPanel.Layout:=TpvGUIFillLayout.Create(fGUIRootSplitterPanel0.RightBottomPanel,4.0);
 fGUIRootSplitterPanel0.RightBottomPanel.Background:=true;

 fGUIRootSplitterPanel1:=TpvGUISplitterPanel.Create(fGUIRootSplitterPanel0.RightBottomPanel);
 fGUIRootSplitterPanel1.Orientation:=TpvGUISplitterPanelOrientation.Vertical;
 fGUIRootSplitterPanel1.LeftTopPanel.Layout:=TpvGUIFillLayout.Create(fGUIRootSplitterPanel1.LeftTopPanel,4.0);
 fGUIRootSplitterPanel1.LeftTopPanel.Background:=true;

 fGUIRootSplitterPanel1.RightBottomPanel.Layout:=TpvGUIFlowLayout.Create(fGUIRootSplitterPanel1.RightBottomPanel,
                                                                         TpvGUILayoutOrientation.Horizontal,
                                                                         8.0);
 fGUIRootSplitterPanel1.RightBottomPanel.Background:=true;

 TpvGUIAdvancedGridLayout(fGUIRootSplitterPanel0.LeftTopPanel.Layout).Rows.Add(200.0,1.0);
 TpvGUIAdvancedGridLayout(fGUIRootSplitterPanel0.LeftTopPanel.Layout).Rows.Add(70.0,0.0);
 TpvGUIAdvancedGridLayout(fGUIRootSplitterPanel0.LeftTopPanel.Layout).Columns.Add(0.0,1.0);

 fGUILeftTabPanel:=TpvGUITabPanel.Create(fGUIRootSplitterPanel0.LeftTopPanel);
 TpvGUIAdvancedGridLayout(fGUIRootSplitterPanel0.LeftTopPanel.Layout).Anchors[fGUILeftTabPanel]:=TpvGUIAdvancedGridLayoutAnchor.Create(0,0,1,1);
 fGUILeftTabPanel.VisibleHeader:=true;
 fGUILeftTabPanel.VisibleContent:=true;
 fGUILeftTabPanel.VisibleContentBackground:=true;
 fGUISignedDistanceFieldCodeEditorTab:=fGUILeftTabPanel.Tabs.Add('Signed distance field');
 fGUIMeshFragmentCodeEditorTab:=fGUILeftTabPanel.Tabs.Add('Mesh fragment');
 fGUILeftTabPanel.TabIndex:=0;

 fGUILeftToolPanel:=TpvGUIPanel.Create(fGUIRootSplitterPanel0.LeftTopPanel);
 TpvGUIAdvancedGridLayout(fGUIRootSplitterPanel0.LeftTopPanel.Layout).Anchors[fGUILeftToolPanel]:=TpvGUIAdvancedGridLayoutAnchor.Create(0,1,1,1,4.0,0.0,4.0,0.0);
 fGUILeftToolPanel.Layout:=TpvGUIGridLayout.Create(fGUILeftToolPanel,
                                                   1,
                                                   TpvGUILayoutAlignment.Fill,
                                                   TpvGUILayoutAlignment.Middle,
                                                   TpvGUILayoutOrientation.Horizontal,
                                                   4.0,
                                                   4.0,
                                                   4.0);
 fGUILeftToolPanel.Background:=false;

 fGUISignedDistanceFieldCodeEditorTab.Content:=TpvGUIPanel.Create(fGUILeftTabPanel.Content);
 fGUISignedDistanceFieldCodeEditorTab.Content.Layout:=TpvGUIFillLayout.Create(fGUISignedDistanceFieldCodeEditorTab.Content,4.0);

 fGUIMeshFragmentCodeEditorTab.Content:=TpvGUIPanel.Create(fGUILeftTabPanel.Content);
 fGUIMeshFragmentCodeEditorTab.Content.Layout:=TpvGUIFillLayout.Create(fGUIMeshFragmentCodeEditorTab.Content,4.0);

 fGUISignedDistanceFieldCodeEditor:=TpvGUIMultiLineTextEdit.Create(fGUISignedDistanceFieldCodeEditorTab.Content);
 fGUISignedDistanceFieldCodeEditor.TextEditor.SyntaxHighlighting:=TpvTextEditor.TSyntaxHighlighting.GetSyntaxHighlightingClassByFileExtension('.glsl').Create(fGUISignedDistanceFieldCodeEditor.TextEditor);
 fGUISignedDistanceFieldCodeEditor.TextEditor.TabWidth:=2;
 fGUISignedDistanceFieldCodeEditor.LineWrap:=false;

 fGUIMeshFragmentCodeEditor:=TpvGUIMultiLineTextEdit.Create(fGUIMeshFragmentCodeEditorTab.Content);
 fGUIMeshFragmentCodeEditor.TextEditor.SyntaxHighlighting:=TpvTextEditor.TSyntaxHighlighting.GetSyntaxHighlightingClassByFileExtension('.glsl').Create(fGUIMeshFragmentCodeEditor.TextEditor);
 fGUIMeshFragmentCodeEditor.TextEditor.TabWidth:=2;
 fGUIMeshFragmentCodeEditor.LineWrap:=false;

 fGUIUpdateButton:=TpvGUIButton.Create(fGUILeftToolPanel);
 fGUIUpdateButton.Caption:='Update (F9)';

 fGUIUpdateProgressBar:=TpvGUIProgressBar.Create(fGUILeftToolPanel);
 fGUIUpdateProgressBar.Orientation:=TpvGUIProgressBarOrientation.Horizontal;
 fGUIUpdateProgressBar.Width:=30.0;
 fGUIUpdateProgressBar.MinimumValue:=0;
 fGUIUpdateProgressBar.MaximumValue:=65536;
 fGUIUpdateProgressBar.Value:=0;

 fGUIVulkanCanvas:=TpvGUIVulkanCanvas.Create(fGUIRootSplitterPanel1.LeftTopPanel);

 begin

  fGUIGridSizePanel:=TpvGUIPanel.Create(fGUIRootSplitterPanel1.RightBottomPanel);
  fGUIGridSizePanel.Layout:=TpvGUIGroupLayout.Create(fGUIGridSizePanel,
                                                     15,
                                                     6,
                                                     14,
                                                     20);
  fGUIGridSizePanel.Background:=true;

  begin

   fLabelGridSizeWidth:=TpvGUILabel.Create(fGUIGridSizePanel);
   fLabelGridSizeWidth.Caption:='Grid width';
   fLabelGridSizeWidth.TextHorizontalAlignment:=TpvGUITextAlignment.Leading;

   fIntegerEditGridSizeWidth:=TpvGUIIntegerEdit.Create(fGUIGridSizePanel);
   fIntegerEditGridSizeWidth.FixedWidth:=96.0;
   fIntegerEditGridSizeWidth.FixedHeight:=32.0;
   fIntegerEditGridSizeWidth.MinimumValue:=1;
   fIntegerEditGridSizeWidth.MaximumValue:=4096;
   fIntegerEditGridSizeWidth.SmallStep:=1;
   fIntegerEditGridSizeWidth.LargeStep:=16;
  //fIntegerEditGridSizeWidth.OnChange:=IntegerEditGridSizeWidthOnChange;

  end;

  begin

   fLabelGridSizeHeight:=TpvGUILabel.Create(fGUIGridSizePanel);
   fLabelGridSizeHeight.Caption:='Grid height';
   fLabelGridSizeHeight.TextHorizontalAlignment:=TpvGUITextAlignment.Leading;

   fIntegerEditGridSizeHeight:=TpvGUIIntegerEdit.Create(fGUIGridSizePanel);
   fIntegerEditGridSizeHeight.FixedWidth:=96.0;
   fIntegerEditGridSizeHeight.FixedHeight:=32.0;
   fIntegerEditGridSizeHeight.MinimumValue:=1;
   fIntegerEditGridSizeHeight.MaximumValue:=4096;
   fIntegerEditGridSizeHeight.SmallStep:=1;
   fIntegerEditGridSizeHeight.LargeStep:=16;
  //fIntegerEditGridSizeHeight.OnChange:=IntegerEditGridSizeHeightOnChange;

  end;

  begin

   fLabelGridSizeDepth:=TpvGUILabel.Create(fGUIGridSizePanel);
   fLabelGridSizeDepth.Caption:='Grid depth';
   fLabelGridSizeDepth.TextHorizontalAlignment:=TpvGUITextAlignment.Leading;

   fIntegerEditGridSizeDepth:=TpvGUIIntegerEdit.Create(fGUIGridSizePanel);
   fIntegerEditGridSizeDepth.FixedWidth:=96.0;
   fIntegerEditGridSizeDepth.FixedHeight:=32.0;
   fIntegerEditGridSizeDepth.MinimumValue:=1;
   fIntegerEditGridSizeDepth.MaximumValue:=4096;
   fIntegerEditGridSizeDepth.SmallStep:=1;
   fIntegerEditGridSizeDepth.LargeStep:=16;
  //fIntegerEditGridSizeDepth.OnChange:=IntegerEditGridSizeDepthOnChange;

  end;

 end;

 begin

  fGUIWorldSizePanel:=TpvGUIPanel.Create(fGUIRootSplitterPanel1.RightBottomPanel);
  fGUIWorldSizePanel.Layout:=TpvGUIGroupLayout.Create(fGUIWorldSizePanel,
                                                     15,
                                                     6,
                                                     14,
                                                     20);
  fGUIWorldSizePanel.Background:=true;

  begin

   fLabelWorldSizeWidth:=TpvGUILabel.Create(fGUIWorldSizePanel);
   fLabelWorldSizeWidth.Caption:='World width';
   fLabelWorldSizeWidth.TextHorizontalAlignment:=TpvGUITextAlignment.Leading;

   fFloatEditWorldSizeWidth:=TpvGUIFloatEdit.Create(fGUIWorldSizePanel);
   fFloatEditWorldSizeWidth.FixedWidth:=96.0;
   fFloatEditWorldSizeWidth.FixedHeight:=32.0;
   fFloatEditWorldSizeWidth.MinimumValue:=0.0;
   fFloatEditWorldSizeWidth.MaximumValue:=4096.0;
   fFloatEditWorldSizeWidth.SmallStep:=0.1;
   fFloatEditWorldSizeWidth.LargeStep:=1.0;
  //fFloatEditWorldSizeWidth.OnChange:=FloatEditWorldSizeWidthOnChange;

  end;

  begin

   fLabelWorldSizeHeight:=TpvGUILabel.Create(fGUIWorldSizePanel);
   fLabelWorldSizeHeight.Caption:='World height';
   fLabelWorldSizeHeight.TextHorizontalAlignment:=TpvGUITextAlignment.Leading;

   fFloatEditWorldSizeHeight:=TpvGUIFloatEdit.Create(fGUIWorldSizePanel);
   fFloatEditWorldSizeHeight.FixedWidth:=96.0;
   fFloatEditWorldSizeHeight.FixedHeight:=32.0;
   fFloatEditWorldSizeHeight.MinimumValue:=0.0;
   fFloatEditWorldSizeHeight.MaximumValue:=4096.0;
   fFloatEditWorldSizeHeight.SmallStep:=0.1;
   fFloatEditWorldSizeHeight.LargeStep:=1.0;
  //fFloatEditWorldSizeHeight.OnChange:=FloatEditWorldSizeHeightOnChange;

  end;

  begin

   fLabelWorldSizeDepth:=TpvGUILabel.Create(fGUIWorldSizePanel);
   fLabelWorldSizeDepth.Caption:='World depth';
   fLabelWorldSizeDepth.TextHorizontalAlignment:=TpvGUITextAlignment.Leading;

   fFloatEditWorldSizeDepth:=TpvGUIFloatEdit.Create(fGUIWorldSizePanel);
   fFloatEditWorldSizeDepth.FixedWidth:=96.0;
   fFloatEditWorldSizeDepth.FixedHeight:=32.0;
   fFloatEditWorldSizeDepth.MinimumValue:=0.0;
   fFloatEditWorldSizeDepth.MaximumValue:=4096.0;
   fFloatEditWorldSizeDepth.SmallStep:=0.1;
   fFloatEditWorldSizeDepth.LargeStep:=1.0;
  //fFloatEditWorldSizeDepth.OnChange:=FloatEditWorldSizeDepthOnChange;

  end;

 end;

 TpvGUIFileDialog.Create(fGUIInstance).Path:='';

 NewProject;

end;

procedure TScreenMain.Hide;
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
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanTransferCommandPool);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandPool);
 pvApplication.VisibleMouseCursor:=true;
 inherited Hide;
end;

procedure TScreenMain.Resume;
begin
 inherited Resume;
end;

procedure TScreenMain.Pause;
begin
 inherited Pause;
end;

procedure TScreenMain.Resize(const aWidth,aHeight:TpvInt32);
begin
 inherited Resize(aWidth,aHeight);
end;

procedure TScreenMain.AfterCreateSwapChain;
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
{fVulkanCanvas.Width:=(pvApplication.Width*12) div 16;
 fVulkanCanvas.Height:=(pvApplication.Height*12) div 16;}
 fVulkanCanvas.Viewport.x:=0;
 fVulkanCanvas.Viewport.y:=0;
 fVulkanCanvas.Viewport.width:=pvApplication.Width;
 fVulkanCanvas.Viewport.height:=pvApplication.Height;
 fScreenToCanvasScale:=TpvVector2.Create(fVulkanCanvas.Width,fVulkanCanvas.Height)/TpvVector2.Create(pvApplication.Width,pvApplication.Height);

 for SwapChainImageIndex:=0 to length(fVulkanRenderCommandBuffers)-1 do begin
  FreeAndNil(fVulkanRenderCommandBuffers[SwapChainImageIndex]);
  fVulkanRenderCommandBuffers[SwapChainImageIndex]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
 end;

 fGUIInstance.VulkanRenderPass:=fVulkanRenderPass;
 fGUIInstance.CountBuffers:=pvApplication.CountSwapChainImages;
 fGUIInstance.Width:=fVulkanCanvas.Width;
 fGUIInstance.Height:=fVulkanCanvas.Height;
 fGUIInstance.MousePosition:=TpvVector2.Create(fGUIInstance.Width*0.5,fGUIInstance.Height*0.5);

 pvApplication.Input.SetCursorPosition(pvApplication.Width div 2,pvApplication.Height div 2);

 fGUIInstance.AfterCreateSwapChain;

 fGUIInstance.PerformLayout;

end;

procedure TScreenMain.BeforeDestroySwapChain;
begin
 fGUIInstance.BeforeDestroySwapChain;
 fVulkanCanvas.VulkanRenderPass:=nil;
 FreeAndNil(fVulkanRenderPass);
 inherited BeforeDestroySwapChain;
end;

function TScreenMain.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=false;
 if fReady and not fGUIInstance.KeyEvent(aKeyEvent) then begin
  if aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down then begin
   case aKeyEvent.KeyCode of
    KEYCODE_QUIT,KEYCODE_F4:begin
     if (aKeyEvent.KeyCode=KEYCODE_QUIT) or
        ((aKeyEvent.KeyCode=KEYCODE_F4) and
         ((aKeyEvent.KeyModifiers*[TpvApplicationInputKeyModifier.ALT,
                                   TpvApplicationInputKeyModifier.CTRL,
                                   TpvApplicationInputKeyModifier.SHIFT,
                                   TpvApplicationInputKeyModifier.META])=[TpvApplicationInputKeyModifier.ALT])) then begin
      ShowTerminationMessageDialogDestroy(nil);
      result:=true;
     end;
    end;
    KEYCODE_N:begin
     if (aKeyEvent.KeyModifiers*[TpvApplicationInputKeyModifier.ALT,
                                 TpvApplicationInputKeyModifier.CTRL,
                                 TpvApplicationInputKeyModifier.SHIFT,
                                 TpvApplicationInputKeyModifier.META])=[TpvApplicationInputKeyModifier.CTRL,TpvApplicationInputKeyModifier.SHIFT] then begin
      ShowNewProjectMessageDialogDestroy(nil);
      result:=true;
     end;
    end;
   end;
  end;
 end;
end;

function TScreenMain.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
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
  end;
 end;
 case aPointerEvent.PointerEventType of
  TpvApplicationInputPointerEventType.Down:begin
   Include(fLastMouseButtons,aPointerEvent.Button);
   fLastMousePosition:=aPointerEvent.Position*fScreenToCanvasScale;
  end;
  TpvApplicationInputPointerEventType.Up:begin
   Exclude(fLastMouseButtons,aPointerEvent.Button);
   fLastMousePosition:=aPointerEvent.Position*fScreenToCanvasScale;
  end;
  TpvApplicationInputPointerEventType.Motion:begin
   fLastMousePosition:=aPointerEvent.Position*fScreenToCanvasScale;
  end;
 end;
end;

function TScreenMain.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 if fReady then begin
  result:=fGUIInstance.Scrolled(fLastMousePosition,aRelativeAmount);
 end else begin
  result:=false;
 end;
end;

function TScreenMain.CanBeParallelProcessed:boolean;
begin
 result:=true;
end;

procedure TScreenMain.Check(const aDeltaTime:TpvDouble);
begin

 inherited Check(aDeltaTime);

 fGUIInstance.UpdateBufferIndex:=pvApplication.UpdateSwapChainImageIndex;
 fGUIInstance.DeltaTime:=aDeltaTime;
 fGUIInstance.Check;

end;

procedure TScreenMain.Update(const aDeltaTime:TpvDouble);
const BoolToInt:array[boolean] of TpvInt32=(0,1);
      Options:array[0..0] of string=('Back');
var Index:TpvInt32;
    cy:TpvFloat;
    s:string;
    IsSelected:boolean;
begin
 inherited Update(aDeltaTime);

 fVulkanCanvas.Start(pvApplication.UpdateSwapChainImageIndex);

 fVulkanCanvas.ViewMatrix:=TpvMatrix4x4.Identity;

 fVulkanCanvas.BlendingMode:=TpvCanvasBlendingMode.AlphaBlending;

{$if false}
 fVulkanCanvas.Color:=TpvVector4.Create(IfThen(TpvApplicationInputPointerButton.Left in fLastMouseButtons,1.0,0.0),
                                        IfThen(TpvApplicationInputPointerButton.Right in fLastMouseButtons,1.0,0.0),
                                        1.0,
                                        1.0);
 fVulkanCanvas.DrawFilledCircle(fLastMousePosition,16.0);
 fVulkanCanvas.Color:=TpvVector4.Create(0.5,1.0,0.5,1.0);
 fVulkanCanvas.DrawFilledCircle(fLastMousePosition,4.0);
 fVulkanCanvas.Color:=TpvVector4.Create(1.0,1.0,1.0,1.0);
{$ifend}

// fGUIUpdateProgressBar.Value:=trunc(fTime*65536) and $ffff;

 fGUIInstance.DrawWidgetBounds:=false;
 fGUIInstance.UpdateBufferIndex:=pvApplication.UpdateSwapChainImageIndex;
 fGUIInstance.DeltaTime:=aDeltaTime;
 fGUIInstance.Update;
 fGUIInstance.Draw;

 fVulkanCanvas.Stop;

 fTime:=fTime+aDeltaTime;

 fReady:=true;
end;

procedure TScreenMain.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
const Offsets:array[0..0] of TVkDeviceSize=(0);
var VulkanCommandBuffer:TpvVulkanCommandBuffer;
    VulkanSwapChain:TpvVulkanSwapChain;
begin

 begin

  begin

   VulkanCommandBuffer:=fVulkanRenderCommandBuffers[aSwapChainImageIndex];
   VulkanSwapChain:=pvApplication.VulkanSwapChain;

   fGUIInstance.DrawBufferIndex:=pvApplication.DrawSwapChainImageIndex;
   fGUIInstance.ExecuteDraw(aWaitSemaphore);

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

end.
