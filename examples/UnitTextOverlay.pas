unit UnitTextOverlay;
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

uses SysUtils,Classes,Vulkan,PasVulkan,PasVulkanSDL2,PasVulkanApplication;

const TextOverlayBufferCharSize=65536;

type PTextOverlayBufferCharVertex=^TTextOverlayBufferCharVertex;
     TTextOverlayBufferCharVertex=packed record
      x,y,u,v:single;
      r,g,b,c:single;
     end;

     PTextOverlayBufferCharVertices=^TTextOverlayBufferCharVertices;
     TTextOverlayBufferCharVertices=array[0..3] of TTextOverlayBufferCharVertex;

     PTextOverlayBufferChar=^TTextOverlayBufferChar;
     TTextOverlayBufferChar=packed record
      Vertices:TTextOverlayBufferCharVertices;
     end;

     PTextOverlayBufferChars=^TTextOverlayBufferChars;
     TTextOverlayBufferChars=array[0..TextOverlayBufferCharSize-1] of TTextOverlayBufferChar;

     PTextOverlayIndices=^TTextOverlayIndices;
     TTextOverlayIndices=array[0..(TextOverlayBufferCharSize*6)-1] of TVkInt32;

     TTextOverlayAlignment=
      (
       toaLeft,
       toaCenter,
       toaRight
      );

     TTextOverlay=class
      private
       fLoaded:boolean;
       fBufferChars:TTextOverlayBufferChars;
       fCountBufferChars:TVkInt32;
       fIndices:TTextOverlayIndices;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
       procedure Load;
       procedure Unload;
       procedure AfterCreateSwapChain;
       procedure BeforeDestroySwapChain;
       procedure Reset;
       procedure AddText(const pX,pY:single;const pAlignment:TTextOverlayAlignment;const pText:AnsiString;const pR:single=1.0;const pG:single=1.0;const pB:single=1.0);
     end;

implementation

constructor TTextOverlay.Create;
var Index:TVkInt32;
begin
 inherited Create;

 for Index:=0 to TextOverlayBufferCharSize-1 do begin
  fIndices[(Index*6)+0]:=(Index*4)+0;
  fIndices[(Index*6)+1]:=(Index*4)+1;
  fIndices[(Index*6)+2]:=(Index*4)+2;
  fIndices[(Index*6)+3]:=(Index*4)+2;
  fIndices[(Index*6)+4]:=(Index*4)+3;
  fIndices[(Index*6)+5]:=(Index*4)+0;
 end;

 fCountBufferChars:=0;

end;

destructor TTextOverlay.Destroy;
begin
 inherited Destroy;
end;

procedure TTextOverlay.Load;
begin
end;

procedure TTextOverlay.Unload;
begin
end;

procedure TTextOverlay.AfterCreateSwapChain;
begin
end;

procedure TTextOverlay.BeforeDestroySwapChain;
begin
end;

procedure TTextOverlay.Reset;
begin
 fCountBufferChars:=0;
end;

procedure TTextOverlay.AddText(const pX,pY:single;const pAlignment:TTextOverlayAlignment;const pText:AnsiString;const pR:single=1.0;const pG:single=1.0;const pB:single=1.0);
var Index,EdgeIndex:TVkInt32;
    BufferChar:PTextOverlayBufferChar;
    CurrentChar:byte;
    FontCharWidth,FontCharHeight,InvWidth,InvHeight,cX:single;
begin
 InvWidth:=1.0/VulkanApplication.Width;
 InvHeight:=1.0/VulkanApplication.Height;
 FontCharWidth:=VulkanApplication.Width/24.0;
 FontCharHeight:=VulkanApplication.Height/80.0;
 case pAlignment of
  toaLeft:begin
   cX:=pX;
  end;
  toaCenter:begin
   cX:=pX-((length(pText)*FontCharWidth)*0.5);
  end;
  else {toaRight:}begin
   cX:=pX-(length(pText)*FontCharWidth);
  end;
 end;
 for Index:=1 to length(pText) do begin
  CurrentChar:=Byte(AnsiChar(pText[Index]));
  if CurrentChar<>32 then begin
   if fCountBufferChars<TextOverlayBufferCharSize then begin
    BufferChar:=@fBufferChars[fCountBufferChars];
    inc(fCountBufferChars);
    for EdgeIndex:=0 to 3 do begin
     BufferChar^.Vertices[EdgeIndex].x:=(cX+((EdgeIndex and 1)*FontCharWidth))*InvWidth;
     BufferChar^.Vertices[EdgeIndex].y:=(pY+((EdgeIndex shr 1)*FontCharHeight))*InvHeight;
     BufferChar^.Vertices[EdgeIndex].u:=EdgeIndex and 1;
     BufferChar^.Vertices[EdgeIndex].v:=EdgeIndex shr 1;
     BufferChar^.Vertices[EdgeIndex].r:=pR;
     BufferChar^.Vertices[EdgeIndex].g:=pG;
     BufferChar^.Vertices[EdgeIndex].b:=pB;
     BufferChar^.Vertices[EdgeIndex].c:=CurrentChar;
    end;
   end;
  end;
  cX:=cX+FontCharWidth;
 end;
end;

end.
