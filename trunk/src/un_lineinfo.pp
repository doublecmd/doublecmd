{
    This file is part of the chelinfo library.

    Copyright (c) 2008 by Anton Rzheshevski
    Parts (c) 2006 Thomas Schatzl, member of the FreePascal
    Development team
    Parts (c) 2000 Peter Vreman (adapted from original stabs line
    reader)

    Dwarf LineInfo Extractor

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
    2008, Anton Rzheshevski aka Cheb:
    Like dr. Frankenshtein I sewn this library together
    from the dead meat of the the FPC RTL modules
    lineinfo.pp and lnfodwrf.pp.
    These (as of Jan. 2008 / FPC 2.2.0) both didn't work
    and had several limitations (e.g. inability to be used
    from a DLL)

    SUPPORTED TARGETS: Linux-32, Linux-64, Win32, Win64.

    Based on lnfodwrf.pp from FreePascal RTL.

    Oct 2009, by cobines
    - Removed the structures holding debugging info. Now the state machine
      is run for each address requiring line info, like in lnfodwrf.pp.
      It uses less memory but is slower. But since it is usually called only
      on unhandled exceptions the speed doesn't matter much.
    - Updated the code to lnfodwrf.pp from FPC 2.5.1 (rev. 14154).
}

{---------------------------------------------------------------------------

 Generic Dwarf lineinfo reader

 The line info reader is based on the information contained in

   DWARF Debugging Information Format Version 3
   Chapter 6.2 "Line Number Information"

 from the

   DWARF Debugging Information Format Workgroup.

 For more information on this document see also

   http://dwarf.freestandards.org/

---------------------------------------------------------------------------}

{$mode delphi}
{$longstrings on}
{$codepage utf-8}
{$coperators on}
{$pointermath on}

{.$DEFINE DEBUG_DWARF_PARSER}

unit un_lineinfo;

interface

uses
  SysUtils, Classes;
  
function GetLineInfo(addr: pointer;
                      var moduleFile, sourceFile: ansistring;
                      var line, column: integer): Boolean;
{
  The format of returned information:
  "moduleFile" *always* receives the full name of the executable file
    (the main exe or one of dlls it uses) the addr belongs to.
    In Linux, it returns the real file name, with all symlinks
    resolved.
  "line" can be negative, which means no line info has been found
    for this address. See LineInfoError (below) for details.
  "sourceFile" returns the source file name. It either doesn't or does
    contain a full path. If the source was in the same directory
    as the program itself, there will be no path. If the source
    was in the different directory, there will be a full path
    (for the moment when the program was compiled, NOT for the
    current location of that source).
  "column" is positive ONLY when there is more than one address
    stored for the same source line. FreePascal generates this
    on VERY rare occasions, mostly for the arithmetic formulas
    spanning several lines. So most of the time column will
    receive -1.
}

procedure InitLineInfo;
{
  Installs the custom BackTraceStr handler.
}

procedure AddLineInfoPath(const Path: String);
{
 Adds a path that will be searched for .zdli files.
 Paths can be absolute or relative to the directory with the executable module.
}

procedure GetModuleByAddr(addr: pointer; var baseaddr: pointer; var filename: string);
{
  This function allows you to know which executable (i.e. the main exe
    or one of the dlls loaded by it) owns this part of the virtual
    addres space.
  baseaddr receives the exe/dll base address
    (always NIL for the main exe in Linux).
  The mechnaism is made with possibility of a DLL relocation
    in mind, but that particular feature is untested.
  This function is used by GetLineInfo() to determine which executable
    to load line the info from.
}

var
  LineInfoError: WideString = '';

implementation

uses
  {$ifdef unix}
    dl
  {$else}
    windows
  {$endif}
  , un_xtrctdwrflnfo, zstream;

{$MACRO ON}

{$ifdef DEBUG_DWARF_PARSER}
  {$define DEBUG_WRITELN := WriteLn}
  {$define DEBUG_COMMENT :=  }
{$else}
  {$define DEBUG_WRITELN := //}
  {$define DEBUG_COMMENT := //}
{$endif}

var
  {You can store the .zdli files in a different folder than the Exe itself.
   Just fill in this array.
   Paths can be absolute or relative to BaseModulePath.
   For example:
     AddLineInfoPath('debug') - will search in <executable_path>/debug/
  }
  LineInfoPaths: array of string = nil;

  {Path where the executable module is.}
  BaseModulePath: String;

function ChelinfoBackTraceStr(addr : Pointer) : ShortString;
var
  exe, src: ansistring;
  line, column: integer;
  Store  : TBackTraceStrFunc;
begin
  { reset to prevent infinite recursion if problems inside the code }
  Store := BackTraceStrFunc;
  BackTraceStrFunc := @SysBackTraceStr;
  GetLineInfo(addr, exe, src, line, column);
  { create string }
  Result := '  $' + HexStr(addr);
  if line >= 0 then
  begin
    Result += ' line ' + IntToStr(line);
    if column >= 0 then
      Result += ', column ' + IntToStr(column);
    Result += ' of ' + src;
  end;
  Result += ' in ' + exe;
  BackTraceStrFunc := Store;
end;

{$packrecords default}

type
  Bool8 = ByteBool;

{ DWARF 2 default opcodes}
const
  { Extended opcodes }
  DW_LNE_END_SEQUENCE = 1;
  DW_LNE_SET_ADDRESS = 2;
  DW_LNE_DEFINE_FILE = 3;
  { Standard opcodes }
  DW_LNS_COPY = 1;
  DW_LNS_ADVANCE_PC = 2;
  DW_LNS_ADVANCE_LINE = 3;
  DW_LNS_SET_FILE = 4;
  DW_LNS_SET_COLUMN = 5;
  DW_LNS_NEGATE_STMT = 6;
  DW_LNS_SET_BASIC_BLOCK = 7;
  DW_LNS_CONST_ADD_PC = 8;
  DW_LNS_FIXED_ADVANCE_PC = 9;
  DW_LNS_SET_PROLOGUE_END = 10;
  DW_LNS_SET_EPILOGUE_BEGIN = 11;
  DW_LNS_SET_ISA = 12;

type
  { state record for the line info state machine }
  TMachineState = record
    address : QWord;  // can hold 32-bit or 64-bit addresses (depending on DWARF type)
    file_id : DWord;
    line : QWord;
    column : DWord;
    is_stmt : Boolean;
    basic_block : Boolean;
    end_sequence : Boolean;
    prolouge_end : Boolean;
    epilouge_begin : Boolean;
    isa : QWord;
    append_row : Boolean;
    first_row : Boolean;
  end;


{ DWARF line number program header preceding the line number program, 64 bit version }
  TLineNumberProgramHeader64 = packed record
    magic : DWord;
    unit_length : QWord;
    version : Word;
    length : QWord;
    minimum_instruction_length : Byte;
    default_is_stmt : Bool8;
    line_base : ShortInt;
    line_range : Byte;
    opcode_base : Byte;
  end;

{ DWARF line number program header preceding the line number program, 32 bit version }
  TLineNumberProgramHeader32 = packed record
    unit_length : DWord;
    version : Word;
    length : DWord;
    minimum_instruction_length : Byte;
    default_is_stmt : Bool8;
    line_base : ShortInt;
    line_range : Byte;
    opcode_base : Byte;
  end;

procedure GetModuleByAddr(addr: pointer; var baseaddr: pointer; var filename: string);
{$ifdef unix}
var
  dlinfo: dl_info;
begin
  FillChar(dlinfo, sizeof(dlinfo), 0);
  dladdr(addr, @dlinfo);
  baseaddr:= dlinfo.dli_fbase;
  filename:= String(dlinfo.dli_fname);
  {$if not defined(darwin)}
  if ExtractFileName(filename) = ExtractFileName(ParamStr(0))
    then baseaddr:= nil;
  {$endif}
end;
{$else}
var
  Tmm: TMemoryBasicInformation;
  TST: array[0..Max_Path] of Char;
begin
  if VirtualQuery(addr, @Tmm, SizeOf(Tmm)) <> sizeof(Tmm)
    then raise Exception.Create('The VirualQuery() call failed.');
  baseaddr:=Tmm.AllocationBase;
  TST[0]:= #0;
  GetModuleFileName(THandle(Tmm.AllocationBase), TST, SizeOf(TST));
  filename:= String(PChar(@TST));
end;
{$endif}

procedure InitLineInfo;
begin
  BackTraceStrFunc := @ChelinfoBacktraceStr;
  BaseModulePath := ExtractFilePath(ParamStr(0));
end;

procedure AddLineInfoPath(const Path: String);
begin
  SetLength(LineInfoPaths, Length(LineInfoPaths) + 1);
  LineInfoPaths[Length(LineInfoPaths) - 1] := ExcludeTrailingPathDelimiter(Path);
end;

function GetLineInfo(addr: Pointer;
                     var moduleFile, sourceFile: ansistring;
                     var line, column: integer): Boolean;
var
  dli: TStream = nil; // Stream holding uncompressed debug line info.
  unit_base, next_base : QWord;
  unit_length: QWord;

  { Returns the next Byte from the input stream, or -1 if there has been
    an error }
  function ReadNext() : Longint; overload;
  var
    bytesread : Longint;
    b : Byte;
  begin
    ReadNext := -1;
    if (dli.Position < next_base) then begin
      bytesread := dli.Read(b, sizeof(b));
      ReadNext := b;
      if (bytesread <> 1) then
        ReadNext := -1;
    end;
  end;

  { Reads the next size bytes into dest. Returns true if successful,
    false otherwise. Note that dest may be partially overwritten after
    returning false. }
  function ReadNext(var dest; size : SizeInt) : Boolean; overload;
  var
    bytesread : SizeInt;
  begin
    bytesread := 0;
    if ((dli.Position + size) < next_base) then begin
      bytesread := dli.Read(dest, size);
    end;
    ReadNext := (bytesread = size);
  end;

  { Reads an unsigned LEB encoded number from the input stream }
  function ReadULEB128() : QWord;
  var
    shift : Byte;
    data : PtrInt;
    val : QWord;
  begin
    shift := 0;
    result := 0;
    data := ReadNext();
    while (data <> -1) do begin
      val := data and $7f;
      result := result or (val shl shift);
      inc(shift, 7);
      if ((data and $80) = 0) then
        break;
      data := ReadNext();
    end;
  end;

  { Reads a signed LEB encoded number from the input stream }
  function ReadLEB128() : Int64;
  var
    shift : Byte;
    data : PtrInt;
    val : Int64;
  begin
    shift := 0;
    result := 0;
    data := ReadNext();
    while (data <> -1) do begin
      val := data and $7f;
      result := result or (val shl shift);
      inc(shift, 7);
      if ((data and $80) = 0) then
        break;
      data := ReadNext();
    end;
    { extend sign. Note that we can not use shl/shr since the latter does not
      translate to arithmetic shifting for signed types }
    result := (not ((result and (1 shl (shift-1)))-1)) or result;
  end;

  procedure SkipULEB128();
  var temp : QWord;
  begin
    temp := ReadULEB128();
    DEBUG_WRITELN('Skipping ULEB128 : ', temp);
  end;

  procedure SkipLEB128();
  var temp : Int64;
  begin
    temp := ReadLEB128();
    DEBUG_WRITELN('Skipping LEB128 : ', temp);
  end;

  function ReadString(): ansistring;
  var a: ansichar;
  begin
    Result:= '';
    while (true) do begin
      dli.Read(a, sizeof(a));
      if a = #0 then Exit;
      Result+= a;
    end;
  end;

  function CalculateAddressIncrement(_opcode : Integer; const header : TLineNumberProgramHeader64) : Int64;
  begin
    Result := _opcode - header.opcode_base; // adjusted_opcode
    Result := (Result div header.line_range) * header.minimum_instruction_length;
  end;

  { initializes the line info state to the default values }
  procedure InitStateRegisters(var state : TMachineState; const header : TLineNumberProgramHeader64);
  begin
    with state do begin
      address := 0;
      file_id := 1;
      line := 1;
      column := 0;
      is_stmt := header.default_is_stmt;
      basic_block := false;
      end_sequence := false;
      prolouge_end := false;
      epilouge_begin := false;
      isa := 0;
      append_row := false;
      first_row := true;
    end;
  end;

  { Skips all line info directory entries }
  procedure SkipDirectories();
  var s : ShortString;
  begin
    while (true) do begin
      s := ReadString();
      if (s = '') then break;
      DEBUG_WRITELN('Skipping directory : ', s);
    end;
  end;

  { Skips the filename section from the current file stream }
  procedure SkipFilenames();
  var s : ShortString;
  begin
    while (true) do begin
      s := ReadString();
      if (s = '') then break;
      DEBUG_WRITELN('Skipping filename : ', s);
      SkipULEB128(); { skip the directory index for the file }
      SkipULEB128(); { skip last modification time for file }
      SkipULEB128(); { skip length of file }
    end;
  end;

  function GetFullFilename(const filenameStart, directoryStart : Int64; const file_id : DWord) : ShortString;
  var
    i : DWord;
    filename, directory : ShortString;
    dirindex : QWord;
    {$IFDEF DEBUG_DWARF_PARSER}
    oldPos: Int64;
    {$ENDIF}
  begin
    filename := '';
    directory := '';
    i := 1;
    {$IFDEF DEBUG_DWARF_PARSER}
    oldPos := dli.Position;
    {$ENDIF}
    dli.Seek(filenameStart, soBeginning);
    while (i <= file_id) do begin
      filename := ReadString();
      DEBUG_WRITELN('Found "', filename, '"');
      if (filename = '') then break;
      dirindex := ReadULEB128(); { read the directory index for the file }
      SkipULEB128(); { skip last modification time for file }
      SkipULEB128(); { skip length of file }
      inc(i);
    end;
    { if we could not find the file index, exit }
    if (filename = '') then begin
      GetFullFilename := '';
    end
    else begin
      dli.Seek(directoryStart, soBeginning);
      i := 1;
      while (i <= dirindex) do begin
        directory := ReadString();
        if (directory = '') then break;
        inc(i);
      end;
      if (directory<>'') and (directory[length(directory)]<>'/') then
        directory:=directory+'/';
      GetFullFilename := directory + filename;
    end;
    {$IFDEF DEBUG_DWARF_PARSER}
    dli.Position := oldPos;
    {$ENDIF}
  end;

  function ParseCompilationUnit(const addr : PtrUInt) : Boolean;
  var
    state : TMachineState;
    { we need both headers on the stack, although we only use the 64 bit one internally }
    header64 : TLineNumberProgramHeader64;
    header32 : TLineNumberProgramHeader32;

    header_length: QWord;

    {$ifdef DEBUG_DWARF_PARSER}s : ShortString;{$endif}

    adjusted_opcode : Int64;
    opcode, extended_opcode : Integer;
    extended_opcode_length : PtrInt;
    addrIncrement, lineIncrement: PtrInt;

    numoptable : array[1..255] of Byte;

    { the offset into the file where the include directories are stored for this compilation unit }
    include_directories : Int64;
    { the offset into the file where the file names are stored for this compilation unit }
    file_names : Int64;

    i: integer;

    prev_line : QWord;
    prev_column : DWord;
    prev_file : DWord;

    { Reads an address from the current input stream }
    function ReadAddress() : PtrUInt;
    begin
      ReadNext(Result, sizeof(PtrUInt));
    end;

    { Reads an unsigned Half from the current input stream }
    function ReadUHalf() : Word;
    begin
      dli.Read(Result, SizeOf(Result));
    end;

  begin
    Result := False;  // Not found yet.

    // First DWORD is either unit length of 32-bit or magic value of 64-bit DWARF.
    dli.Seek(unit_base, soBeginning);
    dli.Read(header64.magic, sizeof(header64.magic));
    dli.Seek(-sizeof(header64.magic), soCurrent);

    if (header64.magic <> $ffffffff) then begin
      DEBUG_WRITELN('32 bit DWARF detected');

      dli.Read(header32, sizeof(header32));

      header64.magic := $ffffffff;
      header64.unit_length := header32.unit_length;
      header64.version := header32.version;
      header64.length := header32.length;
      header64.minimum_instruction_length := header32.minimum_instruction_length;
      header64.default_is_stmt := header32.default_is_stmt;
      header64.line_base := header32.line_base;
      header64.line_range := header32.line_range;
      header64.opcode_base := header32.opcode_base;

      header_length := QWord(header32.length) +
                       sizeof(header32.length) +
                       sizeof(header32.version) +
                       sizeof(header32.unit_length);

      unit_length := QWord(header32.unit_length) +
                     sizeof(header32.unit_length);

    end else begin
      DEBUG_WRITELN('64 bit DWARF detected');

      dli.Read(header64, sizeof(header64));

      header_length := header64.length +
                       sizeof(header64.magic) +
                       sizeof(header64.length) +
                       sizeof(header64.version) +
                       sizeof(header64.unit_length);

      unit_length := header64.unit_length +
                     sizeof(header64.magic) +
                     sizeof(header64.unit_length);
    end;

    next_base:= unit_base + unit_length;

    // Read opcodes lengths table.
    fillchar(numoptable, sizeof(numoptable), #0);
    if not ReadNext(numoptable, header64.opcode_base - 1) then
      Exit;
    DEBUG_WRITELN('Opcode parameter count table');
    for i := 1 to header64.opcode_base - 1 do begin
      DEBUG_WRITELN('Opcode[', i, '] - ', numoptable[i], ' parameters');
    end;

    DEBUG_WRITELN('Reading directories...');
    include_directories := dli.Position;
    SkipDirectories();
    DEBUG_WRITELN('Reading filenames...');
    file_names := dli.Position;
    SkipFilenames();

    // Position stream after header to read state machine code.
    dli.Seek(unit_base + header_length, soBeginning);

    InitStateRegisters(state, header64);

    opcode := ReadNext();
    while (opcode <> -1) do begin
      case (opcode) of
        { extended opcode }
        0 : begin
          extended_opcode_length := ReadULEB128();
          extended_opcode := ReadNext();
          if extended_opcode = -1 then
            break;
          case (extended_opcode) of
            DW_LNE_END_SEQUENCE : begin
              state.end_sequence := true;
              state.append_row := true;
              DEBUG_WRITELN('DW_LNE_END_SEQUENCE');
            end;
            DW_LNE_SET_ADDRESS : begin
              // Size of address should be extended_opcode_length - 1.
              state.address := ReadAddress();
              DEBUG_WRITELN('DW_LNE_SET_ADDRESS (', hexstr(pointer(state.address)), ')');
            end;
            DW_LNE_DEFINE_FILE : begin
              {$ifdef DEBUG_DWARF_PARSER}s := {$endif}ReadString();
              SkipULEB128();
              SkipULEB128();
              SkipULEB128();
              DEBUG_WRITELN('DW_LNE_DEFINE_FILE (', s, ')');
            end;
            else begin
              DEBUG_WRITELN('Unknown extended opcode ', extended_opcode, ' (length ', extended_opcode_length, ')');
              dli.Position:= dli.Position + extended_opcode_length - 1;
            end;
          end;
        end;
        DW_LNS_COPY : begin
          state.basic_block := false;
          state.prolouge_end := false;
          state.epilouge_begin := false;
          state.append_row := true;
          DEBUG_WRITELN('DW_LNS_COPY');
        end;
        DW_LNS_ADVANCE_PC : begin
          state.address := state.address +
                           ReadULEB128() * header64.minimum_instruction_length;
          DEBUG_WRITELN('DW_LNS_ADVANCE_PC (', hexstr(state.address, sizeof(state.address)*2), ')');
        end;
        DW_LNS_ADVANCE_LINE : begin
          state.line := state.line + ReadLEB128();
          DEBUG_WRITELN('DW_LNS_ADVANCE_LINE (', state.line, ')');
        end;
        DW_LNS_SET_FILE : begin
          state.file_id := ReadULEB128();
          DEBUG_WRITELN('DW_LNS_SET_FILE (', state.file_id, ')');
        end;
        DW_LNS_SET_COLUMN : begin
          state.column := ReadULEB128();
          DEBUG_WRITELN('DW_LNS_SET_COLUMN (', state.column, ')');
        end;
        DW_LNS_NEGATE_STMT : begin
          state.is_stmt := not state.is_stmt;
          DEBUG_WRITELN('DW_LNS_NEGATE_STMT (', state.is_stmt, ')');
        end;
        DW_LNS_SET_BASIC_BLOCK : begin
          state.basic_block := true;
          DEBUG_WRITELN('DW_LNS_SET_BASIC_BLOCK');
        end;
        DW_LNS_CONST_ADD_PC : begin
          state.address := state.address + CalculateAddressIncrement(255, header64);
          DEBUG_WRITELN('DW_LNS_CONST_ADD_PC (', hexstr(state.address, sizeof(state.address)*2), ')');
        end;
        DW_LNS_FIXED_ADVANCE_PC : begin
          state.address := state.address + ReadUHalf();
          DEBUG_WRITELN('DW_LNS_FIXED_ADVANCE_PC (', hexstr(state.address, sizeof(state.address)*2), ')');
        end;
        DW_LNS_SET_PROLOGUE_END : begin
          state.prolouge_end := true;
          DEBUG_WRITELN('DW_LNS_SET_PROLOGUE_END');
        end;
        DW_LNS_SET_EPILOGUE_BEGIN : begin
          state.epilouge_begin := true;
          DEBUG_WRITELN('DW_LNS_SET_EPILOGUE_BEGIN');
        end;
        DW_LNS_SET_ISA : begin
          state.isa := ReadULEB128();
          DEBUG_WRITELN('DW_LNS_SET_ISA (', state.isa, ')');
        end;
        else begin { special opcode }
          if (opcode < header64.opcode_base) then begin
            DEBUG_WRITELN('Unknown standard opcode $', hexstr(opcode, 2), '; skipping');
            for i := 1 to numoptable[opcode] do
              SkipLEB128();
          end else begin
            adjusted_opcode := opcode - header64.opcode_base;
            addrIncrement := CalculateAddressIncrement(opcode, header64);
            state.address := state.address + addrIncrement;
            lineIncrement := header64.line_base + (adjusted_opcode mod header64.line_range);
            state.line := state.line + lineIncrement;
            DEBUG_WRITELN('Special opcode $', hexstr(opcode, 2), ' address increment: ', addrIncrement, ' new line: ', lineIncrement);
            state.basic_block := false;
            state.prolouge_end := false;
            state.epilouge_begin := false;
            state.append_row := true;
          end;
        end;
      end; //case

      if (state.append_row) then begin
        {$IFDEF DEBUG_DWARF_PARSER}
        Writeln('Address = ', hexstr(pointer(state.address)),
                ', file_id = ', state.file_id,
                ', file = ' , GetFullFilename(file_names, include_directories, state.file_id),
                ', line = ', state.line, ' column = ', state.column);
        {$ENDIF}

        if (state.first_row) then begin
          if (state.address > addr) then
            break;
          state.first_row := false;
        end;

        { when we have found the address we need to return the previous
          line because that contains the call instruction }
        if (state.address >= addr) then
        begin
          line := prev_line;
          column := prev_column;
          sourceFile := GetFullFilename(file_names, include_directories, prev_file);
          Exit(True);
        end
        else
          begin
            { save line information }
            prev_file := state.file_id;
            prev_line := state.line;
            prev_column := state.column;
          end;

        state.append_row := false;

        if (state.end_sequence) then begin
          // Reset state machine when sequence ends.
          InitStateRegisters(state, header64);
        end;
      end;

      opcode := ReadNext();
    end; //while
  end;

  type
    TPathType = ( ptNone, ptRelative, ptAbsolute );

  function GetPathType(sPath : String): TPathType;
  begin
    Result := ptNone;
  {$IFDEF MSWINDOWS}
  {check for drive/unc info}
    if ( Pos( '\\', sPath ) > 0 ) or ( Pos( DriveDelim, sPath ) > 0 ) then
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  { UNIX absolute paths start with a slash }
    if (sPath[1] = PathDelim) then
  {$ENDIF UNIX}
      Result := ptAbsolute
    else if ( Pos( PathDelim, sPath ) > 0 ) then
      Result := ptRelative;
  end;

  function ExpandAbsolutePath(Path: String): String;
  var
    I, J: Integer;
  begin
    {First remove all references to '\.\'}
    I := Pos (DirectorySeparator + '.' + DirectorySeparator, Path);
    while I <> 0 do
        begin
            Delete (Path, I, 2);
            I := Pos (DirectorySeparator + '.' + DirectorySeparator, Path);
        end;

    {Then remove all references to '\..\'}
    I := Pos (DirectorySeparator + '..', Path);
    while (I <> 0) do
        begin
            J := Pred (I);
            while (J > 0) and (Path [J] <> DirectorySeparator) do
                Dec (J);
            if (J = 0) then
                Delete (Path, I, 3)
            else
                Delete (Path, J, I - J + 3);
            I := Pos (DirectorySeparator + '..', Path);
        end;

    Result := Path;
  end;

  function GetAbsoluteFileName(const sPath, sRelativeFileName : String) : String;
  begin
    case GetPathType(sRelativeFileName) of
      ptNone, ptRelative:
        Result := ExpandAbsolutePath(sPath + sRelativeFileName);
      else
        Result := sRelativeFileName;
    end;
  end;

var
  i: Integer;
  dc, ts: TStream;
  DwarfLineInfo: Pointer;
  externalFile: AnsiString;
  DwarfSize: Qword;
  base_addr: Pointer;
  ExeImageBase: QWord;
begin
  Result := False;

  moduleFile := '';
  sourceFile := '';
  line := -1;
  column := -1;
  LineInfoError:= '';

  GetModuleByAddr(addr, base_addr, moduleFile);
  if moduleFile = '' then
    Exit(False); // No module found at this address.

  // Never read modules or .zdli files from current directory.
  // If module path is relative make it relative to BaseModulePath.
  // (for example ./doublecmd must be expanded).
  moduleFile := GetAbsoluteFileName(BaseModulePath, moduleFile);

  DEBUG_WRITELN('Module ', moduleFile, ' at $', hexStr(base_addr));

  try
    try
     {  First, try the external file with line information.
        Failing that, try to parse the executable itself }

      externalFile := DlnNameByExename(moduleFile);
      i:= -1;
      repeat
        DEBUG_WRITELN('Checking external file: ', externalFile);

        if FileExists(externalFile) then
          break
        else
          externalFile := '';

        inc(i);
        if i > high(LineInfoPaths) then
          break;

        // Check additional paths.
        externalFile := GetAbsoluteFileName(BaseModulePath, LineInfoPaths[i]);
        externalFile := IncludeTrailingPathDelimiter(externalFile) +
                        DlnNameByExename(ExtractFileName(moduleFile));
      until False;

      if externalFile <> ''
        //and (FileAge(moduleFile) <= FileAge(externalFile))
      then begin
        DEBUG_WRITELN('Reading debug info from external file ', externalFile);
        //the compression streams are unable to seek,
        //so we decompress to a memory stream first.
        ts := TFileStream.Create(externalFile, fmOpenRead or fmShareDenyNone);
        dc := TDecompressionStream.Create(ts);
        dli := TMemoryStream.Create;
        dc.Read(DwarfSize, SizeOf(DwarfSize));       // 8 bytes (QWORD)
        dc.Read(ExeImageBase, SizeOf(ExeImageBase)); // 8 bytes (QWORD)
        dli.CopyFrom(dc, DwarfSize);
        FreeAndNil(dc);
        FreeAndNil(ts);
      end
      else begin
        DEBUG_WRITELN('Reading debug info from ', moduleFile);
        if not ExtractDwarfLineInfo(moduleFile, DwarfLineInfo, DwarfSize, ExeImageBase) then begin
          DEBUG_WRITELN('Debug info not found.');
          LineInfoError:= ExtractDwarfLineInfoError;
          Exit(false);
        end;
        dli:= TMemoryStream.Create;
        dli.Write(DwarfLineInfo^, DwarfSize);
        FreeMem(DwarfLineInfo);
      end;

      DEBUG_WRITELN('dwarf line info: ', dli.size, ' bytes.');

      // Account for the possible relocation (in 99% cases ExeImagebase = base_addr)
{$PUSH}
{$overflowchecks off}
{$rangechecks off}
      addr := addr - base_addr + Pointer(ExeImageBase);
{$POP}

      next_base := 0;
      while next_base < dli.Size do begin
        unit_base := next_base;
        if ParseCompilationUnit(PtrUInt(addr)) then
          break; // Found line info
      end;

      Result := True;

    except
      LineInfoError := 'Crashed parsing the dwarf line info: ' +
                       (ExceptObject as Exception).Message;
      Result := False;
    end;

  finally
    if Assigned(dli) then
      FreeAndNil(dli);
  end;

  if not Result then
    DEBUG_WRITELN('Cannot read DWARF debug line info: ', LineInfoError);
end;

initialization
  InitLineInfo;

end.
