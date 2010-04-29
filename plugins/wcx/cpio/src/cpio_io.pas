//***************************************************************
// This file is part of RPMWCX, a archiver plugin for
// Windows Commander.
// Copyright (C) 2000 Mandryka Yurij  e-mail:braingroup@hotmail.ru
//***************************************************************
{
  Add some changes for Lazarus and Linux compability
  Copyright (C) 2007  Koblov Alexander (Alexx2000@mail.ru)
}
//***************************************************************
// Part of code (functions DirectoryExists and ForceDirectories)
// got from Delphi source code
//***************************************************************

//***************************************************************
// This code based on Christian Ghisler (support@ghisler.com) sources
//***************************************************************

{$A-,I-}
unit cpio_io;

interface

uses
  cpio_def, Classes;

type
  TStrBuf = array[1..260] of Char;

function  CPIO_ReadHeader(var f : file; var header : CPIO_Header) : Boolean;
function  IsCPIOArchive(FileName: UTF8String): Boolean;

function  AlignFilePointer(var f : file; align : Integer) : Boolean;
procedure copy_str2buf(var buf : TStrBuf; s : AnsiString);
function  CreateDirectories(Dir : String) : Boolean;
function  correct_filename(oldname : AnsiString) : AnsiString;

implementation

uses
  SysUtils;


{$IFNDEF FPC} // for compiling under Delphi
Const
  DirSeparators : set of char = ['/','\'];

Procedure DoDirSeparators (Var FileName : String);

VAr I : longint;

begin
  For I:=1 to Length(FileName) do
    If FileName[I] in DirSeparators then
      FileName[i]:=PathDelim;
end;
{$ENDIF}

procedure copy_str2buf(var buf : TStrBuf; s : AnsiString);
var
  i_char : Integer;
begin
  FillChar(buf, Sizeof(buf), 0);
  if Length(s) = 0 then Exit;
  if Length(s) > 259 then
    SetLength(s, 259);
  s := s + #0;
  for i_char := 1 to Length(s) do
    buf[i_char] := s[i_char];
end;

function AlignFilePointer;
var
  start : Integer;
  mul   : LongWord;
begin
  Result := False;
  start := FilePos(f);
  case align of
    2 : mul := $FFFFFFFE;
    4 : mul := $FFFFFFFC;
    8 : mul := $FFFFFFF8;
  else
    Exit;
  end;{case}
  if (start mod align) <> 0 then begin
    start := start and mul;
    Inc(start, align);
  end;
  Seek(f, start);
  if IOResult = 0 then Result := True;
end;

function ExcludeTrailingBackslash(Dir:string):string;
begin
  if (length(dir)>0) and (dir[length(dir)]='\') then
    result:=copy(dir,1,length(dir)-1)
  else
    result:=dir;
end;

function CreateDirectories(Dir : String) : Boolean;
begin
  Result := True;
  if Length(Dir) = 0 then
    Result := False
  else begin
    Dir := ExcludeTrailingBackslash(Dir);
    if (Length(Dir) < 3) or DirectoryExists(Dir)
      or (ExtractFilePath(Dir) = Dir) then Exit;
    Result := CreateDirectories(ExtractFilePath(Dir)) and CreateDir(Dir);
  end;
end;

function OctalToDec(Octal: String): Longword;
var
  i: Integer;
begin
  Result := 0;

  for i := 1 to Length(Octal) do
  begin
    Result := Result shl 3;
    case Octal[i] of
      '0'..'7':
        Result := Result + Longword(Ord(Octal[i]) - Ord('0'));
    end;
  end;
end;

function HexToDec(Hex: String): Longword;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(Hex) do
  begin
    Result := Result shl 4;
    case Hex[i] of
      '0'..'9':
        Result := Result + LongWord(Ord(Hex[i]) - Ord('0'));
      'A'..'F':
        Result := Result + LongWord(Ord(Hex[i]) - Ord('A')) + 10;
      'a'..'f':
        Result := Result + LongWord(Ord(Hex[i]) - Ord('a')) + 10;
    end;
  end;
end;

function CPIO_ReadHeader(var f : file; var header : CPIO_Header): Boolean;
var
  Buffer     : array [0..259] of AnsiChar;
  OldHdr     : TOldBinaryHeader absolute Buffer;
  OdcHdr     : TOldCharHeader absolute Buffer;
  NewHdr     : TNewCharHeader absolute Buffer;
  ofs        : Integer;
begin
  Result := False;

  {First, check the type of header}
  BlockRead(f, Buffer[0], 6);
  if IOResult <> 0 then Exit;
  header.IsOldHeader := False;

  // Old binary format.
  if PWord(@Buffer[0])^ = $71C7 then
  begin
    header.IsOldHeader := True;
    BlockRead(f, Buffer[6], SizeOf(TOldBinaryHeader) - 6);
    if IOResult <> 0 then Exit;
    with header, OldHdr do
    begin
      magic     := c_magic;
      dev_major := c_dev;
      dev_minor := 0;
      inode     := c_ino;
      mode      := c_mode;
      uid       := c_uid;
      gid       := c_gid;
      nlink     := c_nlink;
      mtime     := 65536 * c_mtime1 + c_mtime2;
      filesize  := 65536 * c_filesize1 + c_filesize2;
      namesize  := c_namesize;
    end;
  end

  // Old Ascii format.
  else if strlcomp(Buffer, '070707', 6) = 0 then
  begin
    BlockRead(f, Buffer[6], SizeOf(TOldCharHeader) - 6);
    if IOResult <> 0 then Exit;
    with header, OdcHdr do
    begin
      magic     := OctalToDec(c_magic);
      dev_major := OctalToDec(c_dev);
      dev_minor := 0;
      inode     := OctalToDec(c_ino);
      mode      := OctalToDec(c_mode);
      uid       := OctalToDec(c_uid);
      gid       := OctalToDec(c_gid);
      nlink     := OctalToDec(c_nlink);
      mtime     := OctalToDec(c_mtime);
      filesize  := OctalToDec(c_filesize);
      namesize  := OctalToDec(c_namesize);
    end;
  end

  // New Ascii format.
  else if (strlcomp(Buffer, '070701', 6) = 0) or
          (strlcomp(Buffer, '070702', 6) = 0) then
  begin
    BlockRead(f, Buffer[6], SizeOf(TNewCharHeader) - 6);
    if IOResult <> 0 then Exit;
    with header, NewHdr do
    begin
      magic     := HexToDec(c_magic);
      dev_major := HexToDec(c_devmajor);
      dev_minor := HexToDec(c_devminor);
      inode     := HexToDec(c_ino);
      mode      := HexToDec(c_mode);
      uid       := HexToDec(c_uid);
      gid       := HexToDec(c_gid);
      nlink     := HexToDec(c_nlink);
      mtime     := HexToDec(c_mtime);
      filesize  := HexToDec(c_filesize);
      namesize  := HexToDec(c_namesize);
    end;
  end
  else
    Exit;

  with header do
  begin
    if namesize = 0 then exit;   {Error!}
    {Read name}
    ofs:=0;
    if namesize > 259 then
    begin
      ofs := namesize - 259;
      namesize := 259;
    end;
    FillChar(Buffer, SizeOf(Buffer), #0);
    BlockRead(f, Buffer, namesize);
    if IOResult <> 0 then Exit;
    SetString(filename, Buffer, namesize);
    if ofs <> 0 then
      Seek(f, FilePos(f) + ofs);
    origname := filename;
    DoDirSeparators(filename);
    if IsOldHeader then begin
      if not AlignFilePointer(f, 2) then Exit;
    end else
      if not AlignFilePointer(f, 4) then Exit;

    //Correct file name started with "./" or "/"
    filename := correct_filename(filename);
  end;

  Result := True;
end;

function IsCPIOArchive(FileName: UTF8String): Boolean;
type
  TAsciiHeader = array[0..5] of AnsiChar;
const
  sOld: TAsciiHeader = ('0', '7', '0', '7', '0', '7');
  sNew: TAsciiHeader = ('0', '7', '0', '7', '0', '1');
  sCrc: TAsciiHeader = ('0', '7', '0', '7', '0', '2');
var
  Buf: TAsciiHeader;
  Stream: TFileStream;
begin
  Result := False;
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    if (Stream.Size >= 6) and (Stream.Read(Buf[0], 6) = 6) then
    begin
      Result := // Binary format
                (PWord(@Buf[0])^ = $71C7) or
                // Ascii formats
                CompareMem(@Buf[0], @sOld[0], 6) or
                CompareMem(@Buf[0], @sNew[0], 6) or
                CompareMem(@Buf[0], @sCrc[0], 6);
    end;
  finally
    Stream.Free;
  end;
end;

function correct_filename(oldname : AnsiString) : AnsiString;
begin
  Result := oldname;
  if Length(oldname) > 1 then begin
    case oldname[1] of
      '.' :
        case oldname[2] of
          '/', '\' : System.Delete(oldname, 1, 2);
        end;{case}
      '/', '\' : System.Delete(oldname, 1, 1);
    end;{case}
  end;
  Result := oldname;
end;

end.
