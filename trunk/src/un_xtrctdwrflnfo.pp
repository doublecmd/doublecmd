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
    These as of Jan. 2008 / FPC 2.2.0 both didn't work
    and had several limitations (e.g. inability to be used
    from a DLL)

    Oct 2009, by cobines
      Now can extract from ELF32, ELF64, PE regardless of base platform (Linux and Win).
      Fixed reading .debug_line section from PE files (now long section name is used).
}

{$mode delphi}
{$longstrings on}

{$ifndef endian_little}
  {$fatal powerpc architecture not supported}
{$endif}


unit un_xtrctdwrflnfo;


interface

uses
  SysUtils, Classes;

function ExtractDwarfLineInfo(
  ExeFileName: ansistring;
  out _dlnfo: pointer;
  out _dlnfoSize: QWord;
  out Imagebase: QWord): longbool;
{
Reads the dwarf line info from an executable.
  In case of error, see ExtractDwarfLineInfoError for details.
ImageBase is nil for unix DLLs
  in all other cases the value it receives must be substracted
  from the addresses in the dwarf line info (and then the real
  base address added, to account for the possible relocation)
NOTE: currently in unix it is also NIL for the main executable,
  corresponding the GetModuleByAddr() in un_lineinfo
  also returning NIL as the base address for the main executable.
}

function DlnNameByExename(exename: string): string;
{generates file names with .zdli extension.
 Use in cases when both your windows and linux binaries are placed
   in the same folder }

var
  ExtractDwarfLineInfoError: WideString = '';


implementation

uses
  zstream;

const
  // Cannot use reference counted strings because they are emptied
  // when a function from this module is called two or more times
  // while an exception is being processed (don't know why).
  DwarfDebugLine: shortstring = '.debug_line';
  DwarfZDebugLine: shortstring = '.zdebug_line';

type
  TCheckResult = (header_not_found, header_invalid, no_debug_info, found_debug_info);

{$MACRO ON}

{$ifdef DEBUG_DWARF_PARSER}
  {$define DEBUG_WRITELN := WriteLn}
  {$define DEBUG_COMMENT :=  }
{$else}
  {$define DEBUG_WRITELN := //}
  {$define DEBUG_COMMENT := //}
{$endif}

{ ELF Header structures types}
type
  Elf32_Half = Word;
  Elf64_Half = Word;
  { Types for signed and unsigned 32-bit quantities.   }
  Elf32_Word = DWord;
  Elf32_Sword = Longint;
  Elf64_Word = DWord;
  Elf64_Sword = Longint;
  { Types for signed and unsigned 64-bit quantities.   }
  Elf32_Xword = QWord;
  Elf32_Sxword = Int64;
  Elf64_Xword = QWord;
  Elf64_Sxword = Int64;
  { Type of addresses.   }
  Elf32_Addr = DWord;
  Elf64_Addr = QWord;
  { Type of file offsets.   }
  Elf32_Off = DWord;
  Elf64_Off = QWord;
  { Type for section indices, which are 16-bit quantities.   }
  Elf32_Section = Word;
  Elf64_Section = Word;
  { Type for version symbol information.   }
  Elf32_Versym = Elf32_Half;
  Elf64_Versym = Elf64_Half;
{ some constants from the corresponding header files }
const
  El_NIDENT = 16;
  { some important indices into the e_ident signature of an ELF file }
  EI_MAG0 = 0;
  EI_MAG1 = 1;
  EI_MAG2 = 2;
  EI_MAG3 = 3;
  EI_CLASS = 4;
  { the first byte of the e_ident array must be of this value }
  ELFMAG0 = $7f;
  { the second byte of the e_ident array must be of this value }
  ELFMAG1 = Byte('E');
  { the third byte of the e_ident array must be of this value }
  ELFMAG2 = Byte('L');
  { the fourth byte of the e_ident array must be of this value }
  ELFMAG3 = Byte('F');

  { the fifth byte specifies the bitness of the header; all other values are invalid }
  ELFCLASS32 = 1;
  ELFCLASS64 = 2;

{$packrecords c}

type
   { The ELF file header.  This appears at the start of every ELF file, 32 bit version }
  TElf32_Ehdr = record
    e_ident : array[0..El_NIDENT-1] of Byte; { file identification }
    e_type : Elf32_Half; { file type }
    e_machine : Elf32_Half; { machine architecture }
    e_version : Elf32_Word; { ELF format version }
    e_entry : Elf32_Addr; { entry point }
    e_phoff : Elf32_Off; { program header file offset }
    e_shoff : Elf32_Off; { section header file offset }
    e_flags : Elf32_Word; { architecture specific flags }
    e_ehsize : Elf32_Half; { size of ELF header in bytes }
    e_phentsize : Elf32_Half; { size of program header entry }
    e_phnum : Elf32_Half; { number of program header entries }
    e_shentsize : Elf32_Half; { size of section header entry }
    e_shnum : Elf32_Half; { number of section header entry }
    e_shstrndx : Elf32_Half; { section name strings section index }
  end;

  { ELF32 Section header }
  TElf32_Shdr = record
    sh_name : Elf32_Word; { section name }
    sh_type : Elf32_Word; { section type }
    sh_flags : Elf32_Word; { section flags }
    sh_addr : Elf32_Addr; { virtual address }
    sh_offset : Elf32_Off; { file offset }
    sh_size : Elf32_Word; { section size }
    sh_link : Elf32_Word; { misc info }
    sh_info : Elf32_Word; { misc info }
    sh_addralign : Elf32_Word; { memory alignment }
    sh_entsize : Elf32_Word; { entry size if table }
  end;

  { The ELF file header.  This appears at the start of every ELF file, 64 bit version }
  TElf64_Ehdr = record
    e_ident : array[0..El_NIDENT-1] of Byte;
    e_type : Elf64_Half;
    e_machine : Elf64_Half;
    e_version : Elf64_Word;
    e_entry : Elf64_Addr;
    e_phoff : Elf64_Off;
    e_shoff : Elf64_Off;
    e_flags : Elf64_Word;
    e_ehsize : Elf64_Half;
    e_phentsize : Elf64_Half;
    e_phnum : Elf64_Half;
    e_shentsize : Elf64_Half;
    e_shnum : Elf64_Half;
    e_shstrndx : Elf64_Half;
  end;

  { ELF64 Section header }
  TElf64_Shdr = record
    sh_name : Elf64_Word;
    sh_type : Elf64_Word;
    sh_flags : Elf64_Xword;
    sh_addr : Elf64_Addr;
    sh_offset : Elf64_Off;
    sh_size : Elf64_Xword;
    sh_link : Elf64_Word;
    sh_info : Elf64_Word;
    sh_addralign : Elf64_Xword;
    sh_entsize : Elf64_Xword;
  end;

  {$packrecords default}

type
  tdosheader = packed record
     e_magic : word;
     e_cblp : word;
     e_cp : word;
     e_crlc : word;
     e_cparhdr : word;
     e_minalloc : word;
     e_maxalloc : word;
     e_ss : word;
     e_sp : word;
     e_csum : word;
     e_ip : word;
     e_cs : word;
     e_lfarlc : word;
     e_ovno : word;
     e_res : array[0..3] of word;
     e_oemid : word;
     e_oeminfo : word;
     e_res2 : array[0..9] of word;
     e_lfanew : longint;
  end;
  tpeheader = packed record
     PEMagic : longint;
     Machine : word;
     NumberOfSections : word;
     TimeDateStamp : longint;
     PointerToSymbolTable : longint;
     NumberOfSymbols : longint;
     SizeOfOptionalHeader : word;
     Characteristics : word;
     Magic : word;
     MajorLinkerVersion : byte;
     MinorLinkerVersion : byte;
     SizeOfCode : longint;
     SizeOfInitializedData : longint;
     SizeOfUninitializedData : longint;
     AddressOfEntryPoint : longint;
     BaseOfCode : longint;
     BaseOfData : longint;
     ImageBase : longint;
     SectionAlignment : longint;
     FileAlignment : longint;
     MajorOperatingSystemVersion : word;
     MinorOperatingSystemVersion : word;
     MajorImageVersion : word;
     MinorImageVersion : word;
     MajorSubsystemVersion : word;
     MinorSubsystemVersion : word;
     Reserved1 : longint;
     SizeOfImage : longint;
     SizeOfHeaders : longint;
     CheckSum : longint;
     Subsystem : word;
     DllCharacteristics : word;
     SizeOfStackReserve : longint;
     SizeOfStackCommit : longint;
     SizeOfHeapReserve : longint;
     SizeOfHeapCommit : longint;
     LoaderFlags : longint;
     NumberOfRvaAndSizes : longint;
     DataDirectory : array[1..$80] of byte;
  end;
  tcoffsechdr=packed record
    name     : array[0..7] of char;
    vsize    : longint;
    rvaofs   : longint;
    datalen  : longint;
    datapos  : longint;
    relocpos : longint;
    lineno1  : longint;
    nrelocs  : word;
    lineno2  : word;
    flags    : longint;
  end;
  coffsymbol=packed record
    name    : array[0..3] of char; { real is [0..7], which overlaps the strofs ! }
    strofs  : longint;
    value   : longint;
    section : smallint;
    empty   : word;
    typ     : byte;
    aux     : byte;
  end;

function DlnNameByExename(exename: string): string;
begin
  Result := ChangeFileExt(exename, '.zdli');
end;

function cntostr(cn: pchar): string;
var
  i: integer = 0;
begin
  Result:='';
  repeat
    if cn^ = #0 then break;
    Result+= cn^;
    inc(i);
    inc(cn);
  until i = 8;
end;

function ExtractElf32(
  f: TFileStream;
  out DwarfLineInfoSize: QWord;
  out DwarfLineInfoOffset: Int64;
  out Imagebase: QWord;
  out IsCompressed: Boolean): boolean;
var
  header : TElf32_Ehdr;
  strtab_header : TElf32_Shdr;
  cursec_header : TElf32_Shdr;
  i: Integer;
  buf : array[0..20] of char;
  sectionName: string;
begin
  DwarfLineInfoOffset := 0;
  DwarfLineInfoSize := 0;
  Imagebase:= 0;
  IsCompressed := False;
  Result := False;

  if (f.read(header, sizeof(header)) <> sizeof(header)) then begin
    ExtractDwarfLineInfoError:='Could not read the ELF header!';
    Exit(false);
  end;

  { seek to the start of section headers }

  { first get string section header }
  f.Position:= header.e_shoff + (header.e_shstrndx * header.e_shentsize);
  if (f.read(strtab_header, sizeof(strtab_header)) <> sizeof(strtab_header)) then begin
    ExtractDwarfLineInfoError:='Could not read string section header';
    Exit(false);
  end;

  for i := 0 to (header.e_shnum-1) do begin
    // Section nr 0 is reserved.
    if i = 0 then Continue;

    f.Position:= header.e_shoff + (i * header.e_shentsize);
    if (f.Read(cursec_header, sizeof(cursec_header)) <> sizeof(cursec_header)) then begin
      ExtractDwarfLineInfoError:='Could not read next section header';
      Exit(false);
    end;
    { paranoia TODO: check cursec_header.e_shentsize }

    f.Position:= strtab_header.sh_offset + cursec_header.sh_name;
    if (f.Read(buf, sizeof(buf)) <> sizeof(buf)) then begin
      ExtractDwarfLineInfoError:='Could not read section name';
      Exit(false);
    end;
    buf[sizeof(buf)-1] := #0;

    sectionName := StrPas(pchar(@buf[0]));

    DEBUG_WRITELN('Section ', i, ': ', sectionName, ', offset ', IntToStr(cursec_header.sh_offset), ', size ', IntToStr(cursec_header.sh_size));

    if sectionName = DwarfDebugLine then begin
      DEBUG_WRITELN(sectionName + ' section found');
      DwarfLineInfoOffset := cursec_header.sh_offset;
      DwarfLineInfoSize := cursec_header.sh_size;
      { more checks }
      DEBUG_WRITELN(' offset ', DwarfLineInfoOffset, ',  size ', DwarfLineInfoSize);
      Result := (DwarfLineInfoOffset >= 0) and (DwarfLineInfoSize > 0);
      break;
    end
    else if sectionName = DwarfZDebugLine then begin
      DEBUG_WRITELN(sectionName + ' section found');
      DwarfLineInfoOffset := cursec_header.sh_offset;
      DEBUG_WRITELN(' offset ', DwarfLineInfoOffset);
      IsCompressed:= true;
      Result := (DwarfLineInfoOffset >= 0);
      break;
    end;
  end;
end;

function ExtractElf64(
  f: TFileStream;
  out DwarfLineInfoSize: QWord;
  out DwarfLineInfoOffset: Int64;
  out Imagebase: QWord;
  out IsCompressed: Boolean): boolean;
var
  header : TElf64_Ehdr;
  strtab_header : TElf64_Shdr;
  cursec_header : TElf64_Shdr;
  i: Integer;
  buf : array[0..20] of char;
  sectionName: string;
begin
  DwarfLineInfoOffset := 0;
  DwarfLineInfoSize := 0;
  Imagebase:= 0;
  IsCompressed := False;
  Result := False;

  if (f.read(header, sizeof(header)) <> sizeof(header)) then begin
    ExtractDwarfLineInfoError:='Could not read the ELF header!';
    Exit(false);
  end;

  { seek to the start of section headers }

  { first get string section header }
  f.Position:= header.e_shoff + (header.e_shstrndx * header.e_shentsize);
  if (f.read(strtab_header, sizeof(strtab_header)) <> sizeof(strtab_header)) then begin
    ExtractDwarfLineInfoError:='Could not read string section header';
    Exit(false);
  end;

  for i := 0 to (header.e_shnum-1) do begin
    f.Position:= header.e_shoff + (i * header.e_shentsize);
    if (f.Read(cursec_header, sizeof(cursec_header)) <> sizeof(cursec_header)) then begin
      ExtractDwarfLineInfoError:='Could not read next section header';
      Exit(false);
    end;
    { paranoia TODO: check cursec_header.e_shentsize }

    f.Position:= strtab_header.sh_offset + cursec_header.sh_name;
    if (f.Read(buf, sizeof(buf)) <> sizeof(buf)) then begin
      ExtractDwarfLineInfoError:='Could not read section name';
      Exit(false);
    end;
    buf[sizeof(buf)-1] := #0;

    DEBUG_WRITELN('This section is ', pchar(@buf[0]), ', offset ', IntToStr(cursec_header.sh_offset), ', size ', IntToStr(cursec_header.sh_size));

    sectionName := StrPas(pchar(@buf[0]));

    if sectionName = DwarfDebugLine then begin
      DEBUG_WRITELN(sectionName + ' section found');
      DwarfLineInfoOffset := cursec_header.sh_offset;
      DwarfLineInfoSize := cursec_header.sh_size;
      { more checks }
      DEBUG_WRITELN(' offset ', DwarfLineInfoOffset, ',  size ', DwarfLineInfoSize);
      Result := (DwarfLineInfoOffset >= 0) and (DwarfLineInfoSize > 0);
      break;
    end;
    if sectionName = DwarfZDebugLine then begin
      DEBUG_WRITELN(sectionName + ' section found');
      DwarfLineInfoOffset := cursec_header.sh_offset;
      DEBUG_WRITELN(' offset ', DwarfLineInfoOffset);
      IsCompressed:= true;
      Result := (DwarfLineInfoOffset >= 0);
      break;
    end;
  end;
end;

function CheckWindowsExe(
  f: TFileStream;
  out DwarfLineInfoSize: QWord;
  out DwarfLineInfoOffset: Int64;
  out Imagebase: QWord;
  out IsCompressed: Boolean): TCheckResult;

var
  dosheader  : tdosheader;
  peheader   : tpeheader;
  coffsec    : tcoffsechdr;
  stringsSectionOffset: PtrUInt;
  sectionName : String;
  i: Integer;

  function GetLongSectionName(numberedSectionName: String): String;
  var
    sectionNameBuf : array[0..255] of char;
    stringOffset : Cardinal;
    oldOffset: Int64;
    code: Integer;
  begin
     Val(Copy(numberedSectionName, 2, 8), stringOffset, code);
     if code=0 then
       begin
         fillchar(sectionNameBuf, sizeof(sectionNameBuf), 0);
         oldOffset := f.Position;
         f.Seek(stringsSectionOffset + stringOffset, soBeginning);
         f.Read(sectionNameBuf, sizeof(sectionNameBuf));
         f.Seek(oldOffset, soBeginning);
         Result := StrPas(sectionNameBuf);
       end
     else
       Result := '';
  end;

begin
  DwarfLineInfoOffset := 0;
  DwarfLineInfoSize := 0;
  Imagebase:= 0;
  IsCompressed := False;
  Result := header_not_found;

  { read and check header }
  if f.Size >= sizeof(tdosheader) then
  begin
    f.Read(dosheader, sizeof(tdosheader));
    if dosheader.e_magic = $5A4D then
    begin
      f.Position:= dosheader.e_lfanew;
      if (f.Size - f.Position) >= sizeof(tpeheader) then
      begin
        f.Read(peheader, sizeof(tpeheader));
        if peheader.pemagic = $4550 then
        begin
          // Found header.
          DEBUG_WRITELN('Found Windows Portable Executable header.');

          stringsSectionOffset := peheader.PointerToSymbolTable
                                + peheader.NumberOfSymbols * sizeof(coffsymbol);

          { read section info }
          for i:=1 to peheader.NumberOfSections do
           begin
             f.Read(coffsec, sizeof(tcoffsechdr));
             sectionName := cntostr(@coffsec.name);

             if Length(sectionName) <= 0 then
               continue;

             if sectionName[1]='/' then
               // Section name longer than 8 characters.
               sectionName := GetLongSectionName(sectionName);

             DEBUG_WRITELN(sectionName);

             if sectionName = DwarfDebugLine then begin
               DwarfLineInfoOffset:= coffsec.datapos;
               DwarfLineInfoSize:= coffsec.datalen;
               break;
             end;

             if sectionName = DwarfZDebugLine then begin
               DwarfLineInfoOffset:= coffsec.datapos;
               IsCompressed:= true;
               break;
             end;
           end;

          if DwarfLineInfoOffset > 0 then
            Result := found_debug_info
          else
            Result := no_debug_info;

          ImageBase:= peheader.Imagebase;
        end;
      end;
    end;
  end;
end;

function CheckUnixElf(
  f: TFileStream;
  out DwarfLineInfoSize: QWord;
  out DwarfLineInfoOffset: Int64;
  out Imagebase: QWord;
  out IsCompressed: Boolean): TCheckResult;
var
  fileIdentBuf : array[0..El_NIDENT-1] of byte;
begin
  if f.Size >= El_NIDENT then
  begin
    if (f.read(fileIdentBuf, El_NIDENT) <> El_NIDENT) then begin
      Exit(header_not_found);
    end;

    { more paranoia checks }
    if (fileIdentBuf[EI_MAG0] <> ELFMAG0) or
       (fileIdentBuf[EI_MAG1] <> ELFMAG1) or
       (fileIdentBuf[EI_MAG2] <> ELFMAG2) or
       (fileIdentBuf[EI_MAG3] <> ELFMAG3) then
    begin
      ExtractDwarfLineInfoError:='Invalid ELF magic header.';
      Exit(header_not_found);
    end;

    // Found header.

    f.Seek(0, soBeginning);

    case fileIdentBuf[EI_CLASS] of
      ELFCLASS32:
        begin
          DEBUG_WRITELN('Found Unix ELF 32-bit header.');

          if ExtractElf32(f, DwarfLineInfoSize, DwarfLineInfoOffset, Imagebase, IsCompressed) then
            Result := found_debug_info
           else
            Result := no_debug_info;
        end;
      ELFCLASS64:
        begin
          DEBUG_WRITELN('Found Unix ELF 64-bit header.');

          if ExtractElf64(f, DwarfLineInfoSize, DwarfLineInfoOffset, Imagebase, IsCompressed) then
            Result := found_debug_info
           else
            Result := no_debug_info;
        end;
      else
        begin
          Exit(header_invalid);
        end;
    end;

    Imagebase:= 0;
  end;
end;

function ExtractDwarfLineInfo(
  ExeFileName: ansistring;
  out _dlnfo: pointer;
  out _dlnfoSize: QWord;
  out Imagebase: QWord): longbool;
var
  DwarfOffset : int64;
  DwarfSize : QWord;
  IsCompressed: boolean = False;
  f : TFileStream;
  DC: TDecompressionStream;
  CheckResult: TCheckResult;
begin
  DEBUG_WRITELN('Reading dwarf line info from ', ExeFileName);
  Result := False;

  f:= TFileStream.Create(ExeFileName, fmOpenRead or fmShareDenyNone);

  try
    { Check for Windows PE. }
    CheckResult := CheckWindowsExe(f, DwarfSize, DwarfOffset, Imagebase, IsCompressed);

    { Check for Unix ELF. }
    if CheckResult = header_not_found then
    begin
      f.Seek(0, soBeginning);
      CheckResult := CheckUnixElf(f, DwarfSize, DwarfOffset, Imagebase, IsCompressed);
    end;

    if CheckResult = found_debug_info then begin
      Result := True;
      if IsCompressed then begin
        f.Position:= DwarfOffset;
        DC:= TDecompressionStream.Create(f);
        DC.Read(DwarfSize, sizeof(DwarfSize)); // 8 bytes (QWORD)
        DC.Read(ImageBase, sizeof(ImageBase)); // 8 bytes (QWORD)
        _dlnfoSize:= DwarfSize;
        GetMem(_dlnfo, DwarfSize);
        DC.Read(_dlnfo^, DwarfSize);
        DC.Free;
      end
      else begin
        GetMem(_dlnfo, DwarfSize);
        _dlnfoSize:= DwarfSize;
        f.Position:= DwarfOffset;
        f.Read(_dlnfo^, DwarfSize);
      end;
    end
    else
      case CheckResult of
        header_not_found:
          ExtractDwarfLineInfoError := 'File not supported.';
        header_invalid:
          ExtractDwarfLineInfoError := 'Invalid header.';
        no_debug_info:
          ExtractDwarfLineInfoError := 'The debug line info section not found.';
      end;

  finally
    f.Free;
  end;
end;

end.