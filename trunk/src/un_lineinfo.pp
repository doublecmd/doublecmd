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

    SUPPORTED TAGRETS: LINUX-32 AND WIN32 | FPC 2.2.0
    NOTE: Unlike the FPC RTL modules, this one does NOT
      have the "initialization" section: them buggers
      don't work in the Linux dlls.
      You must call the initialization function manually.
}
{
    Oct 2009, by cobines
      Fixed retrieving line numbers.
      Changes to many variables types, so that it can work with DWARF-64
        in the future (and also fixes some bugs).

      Works with FPC 2.5.1 and as far as I know should work with 64-bit binaries.
      DWARF-64 may not work (adapted from implementation in lnfodwrf.pp
                             in FPC RTL, but not tested).
}

{
    Based on lnfodwrf.pp from FreePascal RTL.
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

unit un_lineinfo;

interface

uses
  SysUtils, Classes, zstream;
  
  procedure GetLineInfo(addr: pointer; var exe, src: ansistring; var line, column: integer);
  {
    The format of returned information:
    "exe" *always* receives the full name of the executable file
      (the main exe or one of dlls it uses) the addr belongs to.
      In Linux, it returns the real file name, with all symlinks
      resolved.
    "line" can be negative, which means no line info has been found
      for this address. See LineInfoError (below) for details.
    "src" returns the source file name. It either doesn't or does
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

  function InitLineInfo(someaddr: pointer): longbool;
  {
    This function is called by GetLineInfo() anyway if it doesnt't
      find a loaded line info for the executable to which the
      requested addres belongs.
    Also installs the custom BackTraceStr handler.

    Input:
    someaddr is adress of any function that belongs to the executable
      you want to pre-load the line info for. For example, a function
      exported from a particular dll.
    If you pass NIL, it will load the line info for the executable
      yhis module was compiled with.
      
    Output:
    Returns false if it failed to load the line info for the particular
      executable. In this case look LineInfoError for explanation
    Returns true if the line info for the particular executable is loaded ok.
    Returns true and does nothing if line info for that executable is
      already loaded.
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
    LineInfoPaths: array of string = nil;
    {you can store the .zdli files in a different folder than the EXe itself.
      Just fill in this array.}

implementation

  uses
    {$ifdef unix}
      dl
    {$else}
      windows
    {$endif}
    {$ifdef cge}
     ,{$ifdef cgemodule} mo_hub {$else} cge {$endif}
    {$endif}
    , un_xtrctdwrflnfo;

{$MACRO ON}

//{$define DEBUG_WRITE := WriteLn}
{$define DEBUG_WRITE := //}
{$define DEBUG_ADDLOG := //}

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
    Result:='  $' + HexStr(addr);
    if line < 0 then
    begin
      //Result+= '(no debug info: ' + LineInfoError + ')'
    end
    else begin
      Result+= ' line ' + IntToStr(line);
      if column >=0 then Result+= ', column ' + IntToStr(column);
      Result += ' of ' + src;
    end;
    Result+= ' in ' + exe;
    BackTraceStrFunc := Store;
  end;




{$packrecords default}
  var
    initialized: boolean = false;

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
      isa : QWord; //DWord;
      append_row : Boolean;
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

    TDwarfChunk = packed record
      addr: pointer;
      line: integer;
      column, fileind: smallint; // it is sooo unlikely for them
    end;                         // to go beyond the 32767 limit...
    TFileInfo = packed record
      name: ansistring;
      dirind: Cardinal;
    end;
    TDwarftable = array of TDwarfChunk;
    TCompilationUnit = record
      dTable: TDwarftable;
      Files: array of TFileInfo;
      Dirs: array of ansistring;
    end;
    TExecutableUnit = record
      name: string;
      CompilationUnit: array of TCompilationUnit;
    end;
  var
    base_addr: pointer = nil;
    ExecutableUnit: array of TExecutableUnit;
    

  procedure GetModuleByAddr(addr: pointer; var baseaddr: pointer; var filename: string);
  {$ifdef unix}
  var
    dlinfo: dl_info;
  begin
    FillChar(dlinfo, sizeof(dlinfo), 0);
    dladdr(addr, @dlinfo);
    baseaddr:= dlinfo.dli_fbase;
    filename:= String(dlinfo.dli_fname);
    if ExtractFileName(filename) = ExtractFileName(ParamStr(0))
      then baseaddr:= nil;
//    if filename = BacktrackSymlink(ParamStr(0)) then baseaddr:= nil; //doesn't work!
//addlog ('----------'#10#13'  %0'#10#13'  %1',[BacktrackSymlink(ParamStr(0)), filename]);
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


  function InitLineInfo(someaddr: pointer): longbool;
  var
    dwarfsize: SizeInt;
    dli, dc, ts: TStream;
    unit_base, next_base: dword; // QWord?
    header_length: QWord;
    unit_length: QWord;
    state : TMachineState;
    { we need both headers on the stack, although we only use the 64 bit one internally }
    header64 : TLineNumberProgramHeader64;
    temp_length: DWord;  // same type as header64.magic and header32.unit_length
    numoptable : array[1..255] of Byte;
    i, din: PtrInt;
    s: ansistring;

    opcode, extended_opcode : Byte;
    extended_opcode_length : PtrInt;
    adjusted_opcode : Int64;
    addrIncrement, lineIncrement: PtrInt;
    _dwarf: pointer;
    ExeImageBase: cardinal;

    filename, exname: ansistring;

    { Returns the next Byte from the input stream, or -1 if there has been
      an error }
    function ReadNext() : Longint;
    var
      bytesread : Longint;
      b : Byte;
    begin
      ReadNext := -1;
      if (dli.Position < next_base) then begin
        bytesread := dli.Read(b, sizeof(b));
        ReadNext := b;
      end;
      if (bytesread <> 1) then
        ReadNext := -1;
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
      DEBUG_ADDLOG('Skipping ULEB128 : %0',[temp]);
    end;

    procedure SkipLEB128();
    var temp : Int64;
    begin
      temp := ReadLEB128();
      DEBUG_ADDLOG('Skipping LEB128 : %0',[temp]);
    end;
    
    function CalculateAddressIncrement(_opcode : Byte) : Int64;
    begin
      Result := _opcode - header64.opcode_base; // adjusted_opcode
      Result := (Result div header64.line_range) * header64.minimum_instruction_length;
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

    { Dwarf reader }

    { initializes the line info state to the default values }
    procedure InitStateRegisters();
    begin
      with state do begin
        address := 0;
        file_id := 1;
        line := 1;
        column := 0;
        is_stmt := header64.default_is_stmt;
        basic_block := false;
        end_sequence := false;
        prolouge_end := false;
        epilouge_begin := false;
        isa := 0;
        append_row := false;
      end;
    end;
    
    function ParseCompilationUnit(var CompilationUnit: array of TCompilationUnit): boolean;
    var
      j: integer;
      header32 : TLineNumberProgramHeader32;
      DwarfBits: Integer; // 32 or 64

      procedure AddRow;
      begin
        With CompilationUnit[high(CompilationUnit)] do begin
          SetLength(dtable, length(dtable) + 1);
          with dtable[high(dtable)] do begin
            //account for thepossible relocation (in 99% cases ExeImagebase = base_addr)
            addr:= state.address - ExeImagebase + base_addr;
            line:= state.line;
            column:= state.column;
            fileind:= state.file_id - 1;
            {$ifdef cge}
              if {$ifdef cgemodule} VerboseLog {$else} DebugMode {$endif}//  or true
                then
                  if (fileind < 0) or (fileind > high(files))
                    then AddLog('dwrflnfo: %0 %1:%2, INVALID fileind %3!', [addr, line, column, fileind])
                    else AddLog('dwrflnfo: %0 %1:%2 %3', [addr, line, column, files[fileind].name]);
            {$else}
              DEBUG_WRITE('dwrflnfo: ', IntToHex(cardinal(addr),8), ' ', line,' ', column, ' ',files[fileind].name);
            {$endif}
          end;
        end;
      end;

      { Reads an address from the current input stream.
        Reads 32-bit or 64-bit address, depending on DWARF version. }
      function ReadAddress : QWord;
      var
        aWord32: DWord;
        aWord64: QWord;
      begin
        case DwarfBits of
          32: begin
                dli.Read(aWord32, Sizeof(aWord32));
                Result := QWord(aWord32);
              end;
          64: begin
                dli.Read(aWord64, SizeOf(aWord64));
                Result := aWord64;
              end;
          else
            raise Exception.Create('Invalid DWARF data');
        end;
      end;

      { Reads an unsigned Half from the current input stream }
      function ReadUHalf() : Word;
      begin
        dli.Read(Result, SizeOf(Result));
      end;

    begin
      Result:= true;
      fillchar(CompilationUnit[high(CompilationUnit)], sizeof(TCompilationUnit), 0);

     // a hack: the next compilation unit can have an unpredictable ofset,
     // so we try to find it by checking the most common values of the header

     // Why is this needed? In tests j is always 0.

      j:= 0;
//      repeat
        if unit_base + j + sizeof(header32) + 2 > dli.Size then begin
          DEBUG_ADDLOG('The stream end reached, no more units.');
          Exit(false);
        end;

        dli.Seek(unit_base + j, soBeginning);
        dli.Read(temp_length, sizeof(temp_length));
        dli.Seek(-sizeof(temp_length), soCurrent);

        if (temp_length <> $ffffffff) then begin
          DEBUG_WRITE('32 bit DWARF detected');
          DwarfBits := 32;

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

          header_length := header32.length +
                           sizeof(header32.length) +
                           sizeof(header32.version) +
                           sizeof(header32.unit_length);

          unit_length := header32.unit_length + sizeof(header32.unit_length);

        end else begin
          DEBUG_WRITE('64 bit DWARF detected');
          DwarfBits := 64;

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

{
        with header64 do begin
          if (version = 2) and (line_range = 255) and (opcode_base = 13) then begin
            unit_base+= j;
            DEBUG_ADDLOG('rand_offset=%8, p=%9, unit_length %0 version %1  length %2  min_instr_leng %3 def_is_stmt %4 line_base %5 line_range %6 opcode_base %7',        [unit_length, version, length, minimum_instruction_length, default_is_stmt, line_base, line_range, opcode_base, j, pointer(unit_base)]);
            Break;
          end;
        end;
        inc(j);
      until false;
}

      next_base:= unit_base + unit_length;

      // Read opcodes lengths table.
      fillchar(numoptable, sizeof(numoptable), #0);
      dli.Read(numoptable, header64.opcode_base - 1);
      DEBUG_WRITE('Opcode parameter count table');
      for j := 1 to header64.opcode_base - 1 do begin
        DEBUG_WRITE('Opcode[', j, '] - ', numoptable[j], ' parameters');
      end;

      With CompilationUnit[high(CompilationUnit)] do begin
        SetLength(dirs, 1);
        dirs[0]:=''; //the project directory
        while (true) do begin
          s:= ReadString();
          if (s = '') then break;
          SetLength(dirs, length(dirs) + 1);
          dirs[high(dirs)]:= s;
          DEBUG_ADDLOG('Dir %0: %1',[high(dirs), AnsiToWide(s)]);
        end;


        while (true) do begin
          s:= ReadString;
          if (s = '') then break;
          SetLength(files, length(files) + 1);
          with files[high(files)] do begin
            name:= s;
            dirind:= ReadULEB128(); { the directory index for the file }
            DEBUG_ADDLOG('File %0 (dir %2): %1',[high(files), AnsiToWide(name), dirind]);
          end;
          SkipULEB128(); { skip last modification time for file }
          SkipULEB128(); { skip length of file }
        end;

        // Position stream after header to read state machine code.
        dli.Seek(unit_base + header_length, soBeginning);

        InitStateRegisters();

        while dli.Position < unit_base + unit_length do begin
          opcode := ReadNext();
          if opcode = byte(-1) then
            Break;

          DEBUG_ADDLOG('Next opcode: %0  (stream pos. %1 ( %2 / %3 )',[opcode, dli.position, dli.position - unit_base, unit_length]);

          case (opcode) of
            { extended opcode }
            0 : begin
              extended_opcode_length := ReadULEB128();
              extended_opcode := ReadNext();
              if extended_opcode = byte(-1) then
                break;

              case (extended_opcode) of
                DW_LNE_END_SEQUENCE : begin
                  state.end_sequence := true;
                  state.append_row := true;
                  DEBUG_ADDLOG('DW_LNE_END_SEQUENCE');
                end;
                DW_LNE_SET_ADDRESS : begin
                  state.address := ReadAddress();
                  DEBUG_ADDLOG('DW_LNE_SET_ADDRESS (%0)', [pointer(state.address)]);
                end;
                DW_LNE_DEFINE_FILE : begin
                  {$ifdef DEBUG_DWARF_PARSER}s := {$endif}ReadString();
                  SkipULEB128();
                  SkipULEB128();
                  SkipULEB128();
                  DEBUG_ADDLOG('DW_LNE_DEFINE_FILE (' + s + ')');
                end;
                else begin
                  DEBUG_ADDLOG('Unknown extended opcode (opcode %0 length %1)', [extended_opcode, extended_opcode_length]);
                  dli.Position:= dli.Position + extended_opcode_length - 1;
                end;
              end;
            end;
            DW_LNS_COPY : begin
              state.basic_block := false;
              state.prolouge_end := false;
              state.epilouge_begin := false;
              state.append_row := true;
              DEBUG_ADDLOG('DW_LNS_COPY');
            end;
            DW_LNS_ADVANCE_PC : begin
              state.address := state.address +
                               ReadULEB128() * header64.minimum_instruction_length;
              DEBUG_ADDLOG('DW_LNS_ADVANCE_PC (' + hexstr(state.address, sizeof(state.address)*2) + ')');
            end;
            DW_LNS_ADVANCE_LINE : begin
              state.line := state.line + ReadLEB128();
              DEBUG_ADDLOG('DW_LNS_ADVANCE_LINE (%0)', [state.line]);
            end;
            DW_LNS_SET_FILE : begin
              state.file_id := ReadULEB128();
              DEBUG_ADDLOG('DW_LNS_SET_FILE (%0)', [state.file_id]);
            end;
            DW_LNS_SET_COLUMN : begin
              state.column := ReadULEB128();
              DEBUG_ADDLOG('DW_LNS_SET_COLUMN (%0)', [state.column]);
            end;
            DW_LNS_NEGATE_STMT : begin
              state.is_stmt := not state.is_stmt;
              DEBUG_ADDLOG('DW_LNS_NEGATE_STMT (%0)',[state.is_stmt]);
            end;
            DW_LNS_SET_BASIC_BLOCK : begin
              state.basic_block := true;
              DEBUG_ADDLOG('DW_LNS_SET_BASIC_BLOCK');
            end;
            DW_LNS_CONST_ADD_PC : begin
              state.address := state.address + CalculateAddressIncrement(255);
              DEBUG_ADDLOG('DW_LNS_CONST_ADD_PC (' + hexstr(state.address, sizeof(state.address)*2) + ')');
            end;
            DW_LNS_FIXED_ADVANCE_PC : begin
              state.address := state.address + ReadUHalf();
              DEBUG_ADDLOG('DW_LNS_FIXED_ADVANCE_PC (' + hexstr(state.address, sizeof(state.address)*2) + ')');
            end;
            DW_LNS_SET_PROLOGUE_END : begin
              state.prolouge_end := true;
              DEBUG_ADDLOG('DW_LNS_SET_PROLOGUE_END');
            end;
            DW_LNS_SET_EPILOGUE_BEGIN : begin
              state.epilouge_begin := true;
              DEBUG_ADDLOG('DW_LNS_SET_EPILOGUE_BEGIN');
            end;
            DW_LNS_SET_ISA : begin
              state.isa := ReadULEB128();
              DEBUG_ADDLOG('DW_LNS_SET_ISA (%0)', [state.isa]);
            end;
            else begin { special opcode }
              if (opcode < header64.opcode_base) then begin
                DEBUG_ADDLOG('Unknown standard opcode $' + hexstr(opcode, 2) + '; skipping');
                for j := 1 to numoptable[opcode] do
                  SkipLEB128();
              end else begin
                adjusted_opcode := opcode - header64.opcode_base;
                addrIncrement := CalculateAddressIncrement(opcode);
                state.address := state.address + addrIncrement;
                lineIncrement := header64.line_base + (adjusted_opcode mod header64.line_range);
                state.line := state.line + lineIncrement;
                DEBUG_ADDLOG('Special opcode $' + hexstr(opcode, 2) + ' address increment: %0 new line: %1', [addrIncrement, lineIncrement]);
                state.basic_block := false;
                state.prolouge_end := false;
                state.epilouge_begin := false;
                state.append_row := true;
              end;
            end;
          end; //case

          if (state.append_row) then begin

            AddRow;
            state.append_row := false;

            if (state.end_sequence) then begin
              // Reset state machine when sequence ends.
              InitStateRegisters();
            end;
          end;

        end; //while
      end; //with
      Result:= true;
    end;
    
    
  begin
    if someaddr = nil then someaddr:=@InitLineInfo;
    GetModuleByAddr(someaddr, base_addr, filename);
    if filename = '' then
      Exit(True); // No module found at this address.

    din:= -1;
    for i:=0 to high(ExecutableUnit) do
      if ExecutableUnit[i].name = filename then din:=i;
    if din < 0 then begin
      SetLength(ExecutableUnit, length(ExecutableUnit) + 1);
      din:= high(ExecutableUnit); // it gets added at the end, of course
      ExecutableUnit[din].name:= filename;
      ExecutableUnit[din].CompilationUnit:= nil;
    end
    else
      Exit(true); //already initialized for this exe/dll

    BackTraceStrFunc := @ChelinfoBacktraceStr;

    {$ifdef cge}
    AddLog(RuEn('Загрузка информации для самоотладки...', 'Loading the self-debugging info...'));
    if {$ifdef cgemodule} VerboseLog {$else} DebugMode {$endif} or true
      then AddLog('  %2 is %1, base %0', [base_addr, filename, someaddr]);
    {$else}
//writeln('-- ',IntToHex(cardinal(base_addr), 8), '  ',filename);
    {$endif}

    try
      Result:= false;
     {  First, try the external file with line information.
        Failing that, try to parse the executable itself }

      exname:= DlnNameByExename(filename);
      i:= -1;
      repeat
//{$ifdef cge}addlog('%0  %1', [exname, FileExists(exname)]);{$endif}
        if FileExists(exname)
          then break
          else exname:='';
        inc(i);
        if i > high(LineInfoPaths) then break;
        exname:= LineInfopaths[i] + DlnNameByExename(ExtractFileName(filename));
      until false;
      
      if exname <> ''
        //and (FileAge(filename) <= FileAge(DlnNameByExename(filename)))
      then begin
        WriteLn('Reading debug info from external file ', exname);
        //the compression streams are unable to seek,
        //so we decompress to a memory stream first.
        ts:=TFileStream.Create(exname, fmOpenRead);
        dc:=TDecompressionStream.Create(ts);
        dli:= TMemoryStream.Create;
        dc.Read(DwarfSize, 4);
        dc.Read(ExeImageBase, 4);
        dli.CopyFrom(dc, DwarfSize);
        dc.Free;
        ts.Free;
      end
      else begin
        WriteLn('Reading debug info from ', filename);
        if not ExtractDwarfLineInfo(filename, _dwarf, DwarfSize, ExeImageBase) then begin
          WriteLn('Debug info not found.');
          LineInfoError:= ExtractDwarfLineInfoError;
          Exit(false);
        end;
        dli:= TMemoryStream.Create;
        dli.Write(_dwarf^, DwarfSize);
        FreeMem(_dwarf);
      end;

      dli.Position:= 0;
      next_base:= 0;
      
      DEBUG_WRITE('dwarf line info: ',dli.size,' bytes.');
      
      With ExecutableUnit[din] do
        while next_base < dli.Size do begin
          SetLength(CompilationUnit, length(CompilationUnit) + 1);
          unit_base:= next_base;
          if not ParseCompilationUnit(CompilationUnit) then begin
            SetLength(CompilationUnit, length(CompilationUnit) - 1);
            Break;
          end;
        end;
    except
      LineInfoError:=
        {$ifdef cge}RuEn('Крах при парсинге отладочной информации: ',{$endif}
        'Crashed parsing the dwarf line info: '{$ifdef cge}){$endif}
        + (ExceptObject as Exception).Message;
      dli.Free;
      Result:=false;
    end;
    if {Result and} (length(ExecutableUnit[din].CompilationUnit) > 0)
      then begin
        initialized:= true;
        {$ifdef cge}
        AddLog(RuEn('  найдено %0 блоков.','  found %0 units.'), [length(ExecutableUnit[din].CompilationUnit)])
        {$else}
        DEBUG_WRITE('  found ', length(ExecutableUnit[din].CompilationUnit), ' compilation units.');
        {$endif}
      end
      else begin
        {$ifdef cge}
        AddLog(RuEn('  не найдено ни одного блока для %0.','  not found any units in %0.'), [ExecutableUnit[din].name]);
        {$endif}
        LineInfoError:= 'no compilation units found.';
        Result:=false;
      end;
    dli.Free;
    Result:= True;
  end;
  



  procedure GetLineInfo(addr: pointer; var exe, src: ansistring; var line, column: integer);
  var
    i,j,k, ei: integer;
    ubase: pointer;
    prev_src: ansistring;
    prev_line: Integer;
    prev_column: Integer;
  begin
    src:='';
    exe:='';
    line:= -1;
    column:= -1;
    prev_line := 0;
    prev_column := -1;
    prev_src := '';
    //LineInfoError:= '';

    if not initialized then
      if not InitLineInfo(addr) then exit;
    try
      GetModuleByAddr(addr, ubase, exe);
      if exe = '' then
        Exit; // No module found at this address.

      ei:= -1;
      for i:=0 to high(ExecutableUnit) do
        if ExecutableUnit[i].name = exe then ei:=i;
      if ei < 0 then begin
        if not InitLineInfo(addr) then exit;
        ei:= high(ExecutableUnit); // it gets added at the end, of course
      end;

      with ExecutableUnit[ei] do
      begin
        for j:=0 to high(CompilationUnit) do
        begin
          with ExecutableUnit[ei].CompilationUnit[j] do
          begin
            for i:=0 to high(dtable) do
            begin
              {$IFDEF DEBUG}
              write('DTable ',i, ' [' , hexStr(dtable[i].addr));
              if i<high(dtable) then
                write('-', hexStr(dtable[i+1].addr))
              else
                write('         ');
              writeln('], line ', dtable[i].line, ', file ', Files[dtable[i].fileind].name);
             {$ENDIF}

              // Addresses can only rise in a single sequence, so when we find
              // a higher address then all of the following addresses are also higher.
              if (dtable[i].addr >= addr) then begin

                // Special check for first address in the table.
                if (i = 0) then
                begin
                  // If the address is the same we can return this row's data, which should be accurate.
                  if (dtable[i].addr = addr) then
                  begin
                    src:= Dirs[Files[dtable[i].fileind].dirind];
                    if src <> '' then
                      src := src + PathDelim;
                    src := src + Files[dtable[i].fileind].name;
                    line:= dtable[i].line;
                    column:= -1;
                  end;
                  // else
                  // If it is higher then the address was not found.
                end
                else
                begin
                  { when we have found the address we need to return the previous
                    line because that contains the call instruction }
                  src:= prev_src;
                  line:= prev_line;
                  column:= -1;

                  //now check if the same line appears twice with different columns
                  // (for example two instructions on the same line).
                  // if not, then leave column at -1.
                  for k := 0 to high(dtable) do
                    if (dtable[k].line    = dtable[i].line) and
                       (dtable[k].column <> dtable[i].column) and
                       (dtable[k].fileind = dtable[i].fileind)
                    then
                      column := prev_column; // set column
                end;

                LineInfoError:= '';
                Exit;
              end;

              prev_src := Dirs[Files[dtable[i].fileind].dirind];
              if prev_src <> '' then
                prev_src := prev_src + PathDelim;
              prev_src := prev_src + Files[dtable[i].fileind].name;
              prev_line := dtable[i].line;
              prev_column := dtable[i].column;
            end;
          end;
        end;
      end;
    except
      LineInfoError:= (ExceptObject as Exception).Message;
    end;
  end;
  
end.

