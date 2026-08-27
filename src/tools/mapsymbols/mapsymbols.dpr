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
// into a separate .dbg by default, and on a large project that file can reach
// several hundred megabytes, of which .debug_info takes almost all and
// .debug_line barely one percent. Since only the line information is needed to
// make a crash log readable, the table built here lands in a size which can
// actually be shipped, and the lnfodwrf unit no longer needs that file sitting
// next to the executable.

uses SysUtils,
     Classes,
     PasVulkan.Types,
     PasVulkan.SymbolTable,
     UnitSymbolBuilder,
     UnitImageFile,
     UnitDWARFLine,
     UnitDWARFWriter,
     UnitELFWriter,
     UnitPEInjector,
     UnitPDBWriter,
     UnitDbgHelpCheck,
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

// Emits everything which was collected a second time, as DWARF inside a
// standalone ELF file. Nothing about the original executable changes.
procedure WriteDebugFile(const aBuilder:TSymbolBuilder;const aDWARFWriter:TDWARFWriter;const aFileName:String);
var ELFWriter:TELFWriter;
    Index:TpvSizeInt;
    SymbolRecord,NextSymbol:TSymbolBuilder.TSymbolRecord;
    UnitRecord:TSymbolBuilder.TUnitRecord;
    ImageBase,Low,High,SymbolSize:TpvUInt64;
    Have:Boolean;
begin

 ELFWriter:=TELFWriter.Create;
 try

  ELFWriter.AddDebugSection('.debug_info',aDWARFWriter.DebugInfo);
  ELFWriter.AddDebugSection('.debug_abbrev',aDWARFWriter.DebugAbbrev);
  ELFWriter.AddDebugSection('.debug_line',aDWARFWriter.DebugLine);

  ImageBase:=aBuilder.ImageBase;

  // The overall code range, taken from the units, so that the file states which
  // addresses it is about.
  Have:=false;
  Low:=0;
  High:=0;
  for Index:=0 to aBuilder.UnitCount-1 do begin
   UnitRecord:=aBuilder.GetUnit(Index);
   if not Have then begin
    Low:=ImageBase+UnitRecord.StartRVA;
    High:=Low+UnitRecord.Size;
    Have:=true;
   end else begin
    if (ImageBase+UnitRecord.StartRVA)<Low then begin
     Low:=ImageBase+UnitRecord.StartRVA;
    end;
    if (ImageBase+UnitRecord.StartRVA+UnitRecord.Size)>High then begin
     High:=ImageBase+UnitRecord.StartRVA+UnitRecord.Size;
    end;
   end;
  end;
  if Have then begin
   ELFWriter.SetTextRange(Low,High-Low);
  end;

  for Index:=0 to aBuilder.SymbolCount-1 do begin
   SymbolRecord:=aBuilder.GetSymbol(Index);
   SymbolSize:=0;
   if (Index+1)<aBuilder.SymbolCount then begin
    NextSymbol:=aBuilder.GetSymbol(Index+1);
    if NextSymbol.RVA>SymbolRecord.RVA then begin
     SymbolSize:=NextSymbol.RVA-SymbolRecord.RVA;
    end;
   end;
   ELFWriter.AddSymbol(SymbolRecord.Name,ImageBase+SymbolRecord.RVA,SymbolSize);
  end;

  ELFWriter.SaveToFile(aFileName);

  WriteLn('Wrote ',aFileName,' with ',aDWARFWriter.DebugLine.Size,' bytes of line programs and ',
          aDWARFWriter.DebugInfo.Size,' bytes of compile units.');

 finally
  FreeAndNil(ELFWriter);
 end;

end;

// Puts the same DWARF sections into the executable itself, so that no separate
// file is needed at all. Has to happen before the symbol table is appended,
// since that has to stay at the very end of the file.
function InjectDebugSections(const aDWARFWriter:TDWARFWriter;const aPDBWriter:TPDBWriter;const aPDBFileName,aFileName:String):Boolean;
var Injector:TPEInjector;
begin
 Injector:=TPEInjector.Create;
 try
  if assigned(aDWARFWriter) then begin
   Injector.AddSection('.debug_info',aDWARFWriter.DebugInfo);
   Injector.AddSection('.debug_abbrev',aDWARFWriter.DebugAbbrev);
   Injector.AddSection('.debug_line',aDWARFWriter.DebugLine);
  end;
  if assigned(aPDBWriter) then begin
   Injector.AddCodeViewDirectory(aPDBWriter.GUIDPointer,aPDBWriter.Age,aPDBFileName,aPDBWriter.Signature);
  end;
  result:=Injector.InjectInto(aFileName);
  WriteLn(Injector.Message);
 finally
  FreeAndNil(Injector);
 end;
end;

// Emits the same information a third time, as a PDB.
function WritePDBFile(const aBuilder:TSymbolBuilder;const aImage:TImageFile;const aFileName:String):TPDBWriter;
var PDBWriter:TPDBWriter;
    Index:TpvSizeInt;
begin
 PDBWriter:=TPDBWriter.Create(aBuilder);
 result:=PDBWriter;
 try
  for Index:=0 to length(aImage.Sections)-1 do begin
   PDBWriter.AddSection(aImage.Sections[Index].Name,
                        TpvUInt32(aImage.Sections[Index].VirtualAddress-aImage.ImageBase),
                        TpvUInt32(aImage.Sections[Index].VirtualSize),
                        TpvUInt32(aImage.Sections[Index].RawSize),
                        0);
  end;
  // The signature is derived from what was collected rather than from the
  // clock, so that building the same input twice gives the same identity.
  PDBWriter.SetIdentity(TpvUInt32(aBuilder.LineCount*2654435761),1);
  PDBWriter.SaveToFile(aFileName);
  WriteLn('Wrote ',aFileName,'.');
 except
  FreeAndNil(result);
  raise;
 end;
end;

var ExecutableFileName,MapFileName,DebugFileName,Parameter:String;
    ParameterIndex:TpvSizeInt;
    WantSymbols,WantLines,ForceMap,ForceDWARF:Boolean;
    DebugOutputFileName:String;
    InjectIntoExecutable:Boolean;
    PDBOutputFileName:String;
    DWARFWriter:TDWARFWriter;
    PDBWriter:TPDBWriter;
    Image,DebugImage,SymbolImage:TImageFile;
    Section:TImageSection;
    LineData:TMemoryStream;
    DWARFReader:TDWARFLineReader;
    MapReader:TMapFileReader;
    Builder:TSymbolBuilder;
    Collector:TCollector;
    Resolved,Probes:TpvSizeInt;
    DbgHelpResolved,DbgHelpProbes:TpvSizeInt;
    DbgHelpAvailable:Boolean;
    UsedDWARF:Boolean;

begin

 ExecutableFileName:='';
 MapFileName:='';
 WantSymbols:=true;
 WantLines:=true;
 ForceMap:=false;
 ForceDWARF:=false;
 DebugOutputFileName:='';
 InjectIntoExecutable:=false;
 PDBOutputFileName:='';

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
   ForceDWARF:=true;
  end else if (Parameter='--pdb') and (ParameterIndex<=ParamCount) then begin
   PDBOutputFileName:=ParamStr(ParameterIndex);
   inc(ParameterIndex);
  end else if Parameter='--pe-debug' then begin
   InjectIntoExecutable:=true;
  end else if (Parameter='--gdb') and (ParameterIndex<=ParamCount) then begin
   DebugOutputFileName:=ParamStr(ParameterIndex);
   inc(ParameterIndex);
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
  WriteLn('  --gdb <file>   additionally write the same information as a standalone');
  WriteLn('                 ELF debug file, which addr2line, gdb and everything else');
  WriteLn('                 built on DWARF can read, also for a Delphi build');
  WriteLn('  --pdb <file>   additionally write a PDB, for the Microsoft debuggers');
  WriteLn('  --pe-debug     put those DWARF sections into the executable itself, so');
  WriteLn('                 that no separate file is needed. Needs room in the section');
  WriteLn('                 header table and says so when there is none');
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
 DWARFReader:=nil;
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
  UsedDWARF:=false;
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
     DWARFReader:=TDWARFLineReader.Create(LineData.Memory,TpvSizeInt(LineData.Size));
     DWARFReader.Parse(Collector.OnLineRow,Collector.OnLineUnit);
     WriteLn('DWARF: ',DWARFReader.UnitCount,' compilation units, ',DWARFReader.RowCount,' line rows, ',Collector.DiscardedRows,' of them for discarded code.');
     if DWARFReader.SkippedUnitCount>0 then begin
      WriteLn('Warning: ',DWARFReader.SkippedUnitCount,' compilation units were skipped, most likely DWARF 5.');
     end;
    end;
    // The symbol table normally lives in whichever image carries the debug
    // information, since a stripped executable has none left.
    SymbolImage.EnumerateSymbols(Collector.OnSymbol);
    if (Collector.SymbolsAdded=0) and WantSymbols and (SymbolImage<>Image) then begin
     Image.EnumerateSymbols(Collector.OnSymbol);
    end;
    UsedDWARF:=true;
   end;
  end;

  if not UsedDWARF then begin

   if ForceDWARF then begin
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

  DWARFWriter:=nil;
  PDBWriter:=nil;
  try

   if InjectIntoExecutable or (length(DebugOutputFileName)>0) then begin
    DWARFWriter:=TDWARFWriter.Create(Builder);
    DWARFWriter.Build;
   end;

   // The pdb has to exist before the executable can name it, since the identity
   // in the debug directory has to be the one the pdb was written with.
   if length(PDBOutputFileName)>0 then begin
    PDBWriter:=WritePDBFile(Builder,Image,PDBOutputFileName);
   end;

   // Injection changes the size of the file, so it has to come first. The
   // appended table is found through a footer at the very end and would be
   // orphaned by anything written behind it.
   // The DWARF sections only go in when that was actually asked for, even if a
   // writer exists because a separate debug file was requested as well.
   if InjectIntoExecutable then begin
    InjectDebugSections(DWARFWriter,PDBWriter,ExtractFileName(PDBOutputFileName),ExecutableFileName);
   end else if assigned(PDBWriter) then begin
    InjectDebugSections(nil,PDBWriter,ExtractFileName(PDBOutputFileName),ExecutableFileName);
   end;

   Builder.AppendToFile(ExecutableFileName);

   if length(DebugOutputFileName)>0 then begin
    WriteDebugFile(Builder,DWARFWriter,DebugOutputFileName);
   end;

  finally
   FreeAndNil(PDBWriter);
   FreeAndNil(DWARFWriter);
  end;

  // The pdb is checked through dbghelp, which is a different reader than the
  // one which wrote it, and which also has to accept the debug directory in the
  // executable before it will look at the pdb at all.
  if length(PDBOutputFileName)>0 then begin
   if CheckPDBWithDbgHelp(Builder,ExecutableFileName,DbgHelpResolved,DbgHelpProbes,DbgHelpAvailable) then begin
    WriteLn('Debugger check: ',DbgHelpResolved,' of ',DbgHelpProbes,' probes resolved to the expected line.');
   end else if DbgHelpAvailable then begin
    WriteLn('Debugger check FAILED: ',DbgHelpResolved,' of ',DbgHelpProbes,' probes resolved to the expected line.');
    ExitCode:=1;
   end else begin
    WriteLn('Debugger check skipped, dbghelp did not accept the executable.');
   end;
  end;

  if Builder.SelfCheck(ExecutableFileName,Resolved,Probes) then begin
   WriteLn('Self check: ',Resolved,' of ',Probes,' probes resolved to the expected line.');
  end else begin
   WriteLn('Self check FAILED: ',Resolved,' of ',Probes,' probes resolved to the expected line.');
   ExitCode:=1;
  end;

 finally
  FreeAndNil(MapReader);
  FreeAndNil(DWARFReader);
  FreeAndNil(LineData);
  FreeAndNil(Collector);
  FreeAndNil(Builder);
  FreeAndNil(DebugImage);
  FreeAndNil(Image);
 end;

end.
