program convert;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef fpc_little_endian}
  {$define little_endian}
 {$else}
  {$ifdef fpc_big_endian}
   {$define big_endian}
  {$endif}
 {$endif}
 {$ifdef fpc_has_internal_sar}
  {$define HasSAR}
 {$endif}
 {-$pic off}
 {$define CAN_INLINE}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define little_endian}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define delphi} 
 {$undef HasSAR}
 {$define UseDIV}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
{$endif}
{$ifdef cpu386}
 {$define cpux86}
{$endif}
{$ifdef cpuamd64}
 {$define cpux86}
{$endif}
{$ifdef Win32}
 {$define Windows}
{$endif}
{$ifdef Win64}
 {$define Windows}
{$endif}
{$ifdef WinCE}
 {$define Windows}
{$endif}
{$ifdef Windows}
 {$define Win}
{$endif}
{$ifdef sdl20}
 {$define sdl}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}
{$ifndef HAS_TYPE_DOUBLE}
 {$error No double floating point precision}
{$endif}
{$ifdef fpc}
 {$define CAN_INLINE}
{$else}
 {$undef CAN_INLINE}
 {$ifdef ver180}
  {$define CAN_INLINE}
 {$else}
  {$ifdef conditionalexpressions}
   {$if compilerversion>=18}
    {$define CAN_INLINE}
   {$ifend}
  {$endif}
 {$endif}
{$endif}
{$ifdef windows}
 {$apptype console}
{$endif}
{$undef UNICODE}

uses SysUtils,Classes;

var StringList:TStringList;

procedure ConvertFile(SrcFileName,ArrayName:string);
var ms:TMemoryStream;
    i,j:integer;
    b:byte;
    s:string;
begin
 ms:=TMemoryStream.Create;
 try
  ms.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+StringReplace(StringReplace(SrcFileName,'/',SysUtils.PathDelim,[rfReplaceAll]),'\',SysUtils.PathDelim,[rfReplaceAll]));
  StringList.Add('const '+ArrayName+'DataSize='+IntToStr(ms.Size)+';');
  StringList.Add('      '+ArrayName+'Data:array[0..'+ArrayName+'DataSize-1] of byte=');
  StringList.Add('       (');
  s:='        ';
  j:=0;
  for i:=1 to ms.Size do begin
   ms.ReadBuffer(b,SizeOf(byte));
   s:=s+'$'+LowerCase(IntToHex(b,2));
   if (i+1)<=ms.Size then begin
    s:=s+',';
   end;
   inc(j);
   if j>=16 then begin
    j:=0;
    StringList.Add(s);
    s:='        ';
   end;
  end;
  StringList.Add(s);
  StringList.Add('       );');
  StringList.Add('');
 finally
  ms.Free;
 end;
end;

begin
 StringList:=TStringList.Create;
 try
  ConvertFile('shaders/canvas/canvas_frag_atlas_texture.spv','CanvasFragmentAtlasTextureSPIRV');
  ConvertFile('shaders/canvas/canvas_frag_texture.spv','CanvasFragmentTextureSPIRV');
  ConvertFile('shaders/canvas/canvas_frag_no_texture.spv','CanvasFragmentNoTextureSPIRV');
  ConvertFile('shaders/canvas/canvas_vert.spv','CanvasVertexSPIRV');
  StringList.SaveToFile(IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'..')+'PasVulkanAssets.inc');
 finally
  StringList.Free;
 end;
end.
