program mapsymbols;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$apptype console}

// Builds a compact PasVulkan symbol table for an executable and appends it to
// that executable, so that a shipped build can symbolicate its own crash logs
// without any separate symbol file next to it.
//
// Two frontends feed the same table:
//
//   Delphi  : the .map file, which is the only place where a Delphi build keeps
//             line numbers at all.
//   FreePascal : the DWARF .debug_line section, either inside the executable or
//             inside the external debug file a .gnu_debuglink points at.
//
// The FreePascal case is worth spelling out. Lazarus links the debug information
// into a separate .dbg by default, and for a large project that file is about 510 MB,
// of which .debug_info alone takes 488 MB while .debug_line takes 5.8 MB. Since
// only the line information is needed to make a crash log readable, the table
// built here lands in a size which can actually be shipped, and the lnfodwrf
// unit no longer needs that 510 MB file sitting next to the executable.

uses SysUtils,
     Classes,
     PasVulkan.Types,
     PasVulkan.SymbolTable,
     UnitSymbolBuilder,
     UnitImageFile,
     UnitDwarfLine,
     UnitMapFile;

type TCollector=class
      public
       Builder:TSymbolBuilder;
       ImageBase:TpvUInt64;
       WantSymbols:Boolean;
       // Link time window covered by executable sections. Anything outside is
       // not code of this image.
       CodeLow:TpvUInt64;
       CodeHigh:TpvUInt64;
       SymbolsAdded:TpvSizeInt;
       DiscardedRows:TpvSizeInt;
       // Range of the compilation unit currently being read, accumulated over
       // the rows which survived the plausibility check below.
       CurrentLow:TpvUInt64;
       CurrentHigh:TpvUInt64;
       HaveCurrent:Boolean;
       procedure OnLineRow(const aAddress:TpvUInt64;const aLineNumber:TpvUInt32);
       procedure OnLineUnit(const aFileName:String);
       procedure OnSymbol(const aAddress:TpvUInt64;const aName:String);
     end;

// Turns a FreePascal mangled symbol into something a reader recognizes.
//
//   SYSINIT_$$_SETUPENTRYINFORMATION              -> SYSINIT.SETUPENTRYINFORMATION
//   SYSINIT_$$_LINKIN$POINTER$POINTER$$POINTER    -> SYSINIT.LINKIN
//   PASVULKAN.TYPES_$$_TFOO_$__$$_BAR$LONGINT     -> PASVULKAN.TYPES.TFOO.BAR
//
// A name which does not carry the FreePascal unit marker is passed through, so
// that Delphi names and plain C symbols stay untouched.
function DemangleName(const aName:String):String;
var MarkerPosition,DollarPosition,Index:TpvSizeInt;
    UnitPart,RestPart:String;
begin

 result:=aName;

 MarkerPosition:=Pos('_$$_',aName);
 if MarkerPosition<2 then begin
  exit;
 end;

 UnitPart:=Copy(aName,1,MarkerPosition-1);
 RestPart:=Copy(aName,MarkerPosition+4,length(aName));

 // A method carries the class before the routine, separated by its own marker.
 MarkerPosition:=Pos('_$__$$_',RestPart);
 if MarkerPosition>0 then begin
  RestPart:=Copy(RestPart,1,MarkerPosition-1)+'.'+Copy(RestPart,MarkerPosition+7,length(RestPart));
 end;

 // Everything from the first remaining dollar sign on is the parameter and
 // result type encoding, or a generated hash suffix.
 DollarPosition:=0;
 for Index:=1 to length(RestPart) do begin
  if RestPart[Index]='$' then begin
   DollarPosition:=Index;
   break;
  end;
 end;
 if DollarPosition>0 then begin
  RestPart:=Copy(RestPart,1,DollarPosition-1);
 end;

 while (length(RestPart)>0) and (RestPart[length(RestPart)]='_') do begin
  Delete(RestPart,length(RestPart),1);
 end;

 if length(RestPart)=0 then begin
  exit;
 end;

 result:=UnitPart+'.'+RestPart;

end;

procedure TCollector.OnLineRow(const aAddress:TpvUInt64;const aLineNumber:TpvUInt32);
begin

 // FreePascal emits line information for code which the linker then discards.
 // The leftover sequences keep addresses near zero rather than a real virtual
 // address, and objdump shows exactly the same rows, so this is not a parsing
 // artefact but genuinely dead information which has to be dropped. Testing
 // against the executable sections rather than against the image base is what
 // makes this work for a position independent image too, where the base is
 // zero and a plain lower bound would let everything through.
 if (aAddress<CodeLow) or (aAddress>=CodeHigh) then begin
  inc(DiscardedRows);
  exit;
 end;

 if not HaveCurrent then begin
  CurrentLow:=aAddress;
  CurrentHigh:=aAddress;
  HaveCurrent:=true;
 end else begin
  if aAddress<CurrentLow then begin
   CurrentLow:=aAddress;
  end;
  if aAddress>CurrentHigh then begin
   CurrentHigh:=aAddress;
  end;
 end;

 // A line number of zero is an end of sequence marker, which bounds the code
 // but is not a line of its own.
 if aLineNumber>0 then begin
  Builder.AddLine(aAddress-ImageBase,aLineNumber);
 end;

end;

procedure TCollector.OnLineUnit(const aFileName:String);
var Name:String;
begin
 if HaveCurrent and (CurrentHigh>=CurrentLow) then begin
  // DWARF names the source file, not the Pascal unit, so the unit name is taken
  // from the file name, which is what a reader expects to see anyway.
  Name:=ChangeFileExt(ExtractFileName(aFileName),'');
  Builder.AddUnit(Name,aFileName,CurrentLow-ImageBase,(CurrentHigh-CurrentLow)+1);
 end;
 HaveCurrent:=false;
 CurrentLow:=0;
 CurrentHigh:=0;
end;

procedure TCollector.OnSymbol(const aAddress:TpvUInt64;const aName:String);
begin
 if not WantSymbols then begin
  exit;
 end;
 if (aAddress<CodeLow) or (aAddress>=CodeHigh) then begin
  exit;
 end;
 // The unit range markers are not routines and would only be noise in a stack
 // trace, since they sit exactly on the boundary between two units.
 if (Pos('DEBUGSTART_$',aName)=1) or (Pos('DEBUGEND_$',aName)=1) then begin
  exit;
 end;
 Builder.AddSymbol(aAddress-ImageBase,DemangleName(aName));
 inc(SymbolsAdded);
end;

var ExecutableFileName,MapFileName,DebugFileName,Parameter:String;
    ParameterIndex:TpvSizeInt;
    WantSymbols,WantLines,ForceMap,ForceDwarf:Boolean;
    Image,DebugImage,SymbolImage:TImageFile;
    Section:TImageSection;
    LineData:TMemoryStream;
    DwarfReader:TDwarfLineReader;
    MapReader:TMapFileReader;
    Builder:TSymbolBuilder;
    Collector:TCollector;
    Resolved,Probes:TpvSizeInt;
    UsedDwarf:Boolean;

begin

 ExecutableFileName:='';
 MapFileName:='';
 WantSymbols:=true;
 WantLines:=true;
 ForceMap:=false;
 ForceDwarf:=false;

 ParameterIndex:=1;
 while ParameterIndex<=ParamCount do begin
  Parameter:=ParamStr(ParameterIndex);
  inc(ParameterIndex);
  if Parameter='--no-symbols' then begin
   WantSymbols:=false;
  end else if Parameter='--no-lines' then begin
   WantLines:=false;
  end else if Parameter='--map' then begin
   ForceMap:=true;
  end else if Parameter='--dwarf' then begin
   ForceDwarf:=true;
  end else if length(ExecutableFileName)=0 then begin
   ExecutableFileName:=Parameter;
  end else if length(MapFileName)=0 then begin
   MapFileName:=Parameter;
  end;
 end;

 if length(ExecutableFileName)=0 then begin
  WriteLn('mapsymbols - appends a PasVulkan symbol table to an executable, so that it');
  WriteLn('             can symbolicate its own crash logs.');
  WriteLn;
  WriteLn('  mapsymbols <executable> [mapfile] [options]');
  WriteLn;
  WriteLn('It reads a Delphi .map file, or the DWARF line information of a FreePascal');
  WriteLn('build, whichever the executable actually offers. DWARF is also picked up out');
  WriteLn('of the external debug file which a .gnu_debuglink section points at.');
  WriteLn;
  WriteLn('  --map          force the Delphi map frontend');
  WriteLn('  --dwarf        force the DWARF frontend');
  WriteLn('  --no-symbols   omit routine names, which is by far the larger part');
  WriteLn('  --no-lines     omit line numbers');
  WriteLn;
  WriteLn('Running it again on the same executable replaces the previous table.');
  ExitCode:=1;
  exit;
 end;

 if not FileExists(ExecutableFileName) then begin
  WriteLn('Executable not found: ',ExecutableFileName);
  ExitCode:=1;
  exit;
 end;

 Image:=nil;
 DebugImage:=nil;
 Builder:=nil;
 Collector:=nil;
 LineData:=nil;
 DwarfReader:=nil;
 MapReader:=nil;
 try

  Image:=TImageFile.Create;
  if not Image.Open(ExecutableFileName) then begin
   WriteLn('Could not read ',ExecutableFileName,', is that a PE executable?');
   ExitCode:=1;
   exit;
  end;

  WriteLn('Image base $',IntToHex(Image.ImageBase,16));

  Builder:=TSymbolBuilder.Create;
  Collector:=TCollector.Create;
  Collector.Builder:=Builder;
  Collector.ImageBase:=Image.ImageBase;
  Collector.CodeLow:=Image.CodeLow;
  Collector.CodeHigh:=Image.CodeHigh;
  Builder.ImageBase:=Image.ImageBase;
  Collector.WantSymbols:=WantSymbols;
  Collector.SymbolsAdded:=0;
  Collector.DiscardedRows:=0;
  Collector.HaveCurrent:=false;

  // Decide which frontend applies. A Delphi build never carries DWARF, so
  // preferring DWARF when it is present is unambiguous.
  UsedDwarf:=false;
  if not ForceMap then begin
   SymbolImage:=Image;
   if not Image.FindSection('.debug_line',Section) then begin
    DebugFileName:=Image.DebugLinkFileName;
    if length(DebugFileName)>0 then begin
     WriteLn('Following the debug link to ',ExtractFileName(DebugFileName),'.');
     DebugImage:=TImageFile.Create;
     if DebugImage.Open(DebugFileName) and DebugImage.FindSection('.debug_line',Section) then begin
      SymbolImage:=DebugImage;
     end else begin
      FreeAndNil(DebugImage);
     end;
    end;
   end;

   if assigned(DebugImage) then begin
    LineData:=DebugImage.ReadSection('.debug_line');
   end else begin
    LineData:=Image.ReadSection('.debug_line');
   end;

   if assigned(LineData) and (LineData.Size>0) then begin
    if WantLines then begin
     DwarfReader:=TDwarfLineReader.Create(LineData.Memory,TpvSizeInt(LineData.Size));
     DwarfReader.Parse(Collector.OnLineRow,Collector.OnLineUnit);
     WriteLn('DWARF: ',DwarfReader.UnitCount,' compilation units, ',DwarfReader.RowCount,' line rows, ',Collector.DiscardedRows,' of them for discarded code.');
     if DwarfReader.SkippedUnitCount>0 then begin
      WriteLn('Warning: ',DwarfReader.SkippedUnitCount,' compilation units were skipped, most likely DWARF 5.');
     end;
    end;
    // The symbol table normally lives in whichever image carries the debug
    // information, since a stripped executable has none left.
    SymbolImage.EnumerateSymbols(Collector.OnSymbol);
    if (Collector.SymbolsAdded=0) and WantSymbols and (SymbolImage<>Image) then begin
     Image.EnumerateSymbols(Collector.OnSymbol);
    end;
    UsedDwarf:=true;
   end;
  end;

  if not UsedDwarf then begin

   if ForceDwarf then begin
    WriteLn('No DWARF line information found in ',ExecutableFileName,'.');
    ExitCode:=1;
    exit;
   end;

   if length(MapFileName)=0 then begin
    MapFileName:=ChangeFileExt(ExecutableFileName,'.map');
   end;

   if not FileExists(MapFileName) then begin
    WriteLn('Neither DWARF line information nor a map file was found.');
    WriteLn('Expected a map file at ',MapFileName,'.');
    WriteLn('For Delphi, enable a detailed map file, which is DCC_MapFile set to 3.');
    WriteLn('For FreePascal, build with debug information, which is -gw2 -gl.');
    ExitCode:=1;
    exit;
   end;

   MapReader:=TMapFileReader.Create(Builder,Image.ImageBase,WantSymbols,WantLines);
   MapReader.Parse(MapFileName);

  end;

  if Builder.UnitCount=0 then begin
   WriteLn('No code ranges were found, so there is nothing to write.');
   ExitCode:=1;
   exit;
  end;

  Builder.Finish;

  // Everything needed has been read, and the executable is about to be written
  // to, so the read handles have to go first.
  Image.Close;
  if assigned(DebugImage) then begin
   DebugImage.Close;
  end;

  WriteLn(Builder.UnitCount,' units, ',Builder.SymbolCount,' symbols, ',Builder.LineCount,' line records.');

  if Builder.LineCount=0 then begin
   WriteLn('Warning: no line numbers were found, so only routine names will resolve.');
  end;

  Builder.AppendToFile(ExecutableFileName);

  if Builder.SelfCheck(ExecutableFileName,Resolved,Probes) then begin
   WriteLn('Self check: ',Resolved,' of ',Probes,' probes resolved to the expected line.');
  end else begin
   WriteLn('Self check FAILED: ',Resolved,' of ',Probes,' probes resolved to the expected line.');
   ExitCode:=1;
  end;

 finally
  FreeAndNil(MapReader);
  FreeAndNil(DwarfReader);
  FreeAndNil(LineData);
  FreeAndNil(Collector);
  FreeAndNil(Builder);
  FreeAndNil(DebugImage);
  FreeAndNil(Image);
 end;

end.
