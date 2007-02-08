//***************************************************************
// This file is part of RPMWCX, a archiver plugin for
// Windows Commander.
// Copyright (C) 2000 Mandryka Yurij  e-mail:braingroup@hotmail.ru
//***************************************************************

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
  cpio_def;

type
  TStrBuf = array[1..260] of Char;

function  CPIO_ReadHeader(var f : file; var header : CPIO_Header) : Boolean;

function  AlignFilePointer(var f : file; align : Integer) : Boolean;
procedure copy_str2buf(var buf : TStrBuf; s : AnsiString);
procedure BackSlashes(var S : AnsiString);
function  CreateDirectories(Dir : String) : Boolean;
function  correct_filename(oldname : AnsiString) : AnsiString;

implementation

uses
  Windows, SysUtils;

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

procedure BackSlashes(var S : AnsiString);
var i_pos : Integer;
begin
  for i_pos := 1 to Length(S) do
    if S[i_pos] = '/' then S[i_pos] := '\';
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

function DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
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

function swapbytes(w:word):word;
begin
  result:=256*lo(w)+hi(w);
end;

function CPIO_ReadHeader;
var
  tmp_buf    : array [0..259] of Char;
  oldhdr     : toldhdr absolute tmp_buf;
  code, i    : Integer;
  loadlen,ofs: Integer;
  value      : LongWord;
begin
  Result := False;
  {First, check the type of header (old or new)}
  tmp_buf[0]:='$';
  BlockRead(f, tmp_buf[1], 6);
  if IOResult <> 0 then Exit;
  header.oldhdrtype:=false;
  if (tmp_buf[1]=#$71) and (tmp_buf[2]=#$C7) then begin  {Old format!}
    header.oldhdrtype:=true;
    move(tmp_buf[1],tmp_buf[0],6);
    BlockRead(f, tmp_buf[6], sizeof(tOldHdr)-6);
    if IOResult <> 0 then Exit;
    with header,oldhdr do begin
      records[1]:=swapbytes(c_magic);
      records[2]:=swapbytes(c_ino);
      records[3]:=swapbytes(c_mode);
      records[4]:=swapbytes(c_uid);
      records[5]:=swapbytes(c_gid);
      records[6]:=swapbytes(c_nlink);
      records[7]:=65536*swapbytes(c_mtime1)+swapbytes(c_mtime2);
      records[8]:=65536*swapbytes(c_filesize1)+swapbytes(c_filesize2);
      records[9]:=0;
      records[10]:=0;
      records[11]:=0;
      records[12]:=0;
      records[13]:=swapbytes(c_namesize);
      records[14]:=0;
    end;
  end else if strlcomp(tmp_buf,'$0707',5)=0 then begin
    tmp_buf[7]:=#0;
    Val(AnsiString(tmp_buf), value, code);
    header.records[1] := value;
    for i := 2 to 14 do begin
      loadlen := 8;
      tmp_buf[0]:='$';
      BlockRead(f, tmp_buf[1], loadlen);
      if IOResult <> 0 then Exit;
      tmp_buf[loadlen+1]:=#0;
      Val(AnsiString(tmp_buf), value, code);
      header.records[i] := value;
    end;
  end else
    exit;

  if header.records[13]<0 then exit;   {Error!}
  {Read name}
  ofs:=0;
  if header.records[13]>259 then begin
    ofs:=header.records[13]-259;
    header.records[13]:=259;
  end;
  fillchar(tmp_buf,sizeof(tmp_buf),#0);
  BlockRead(f, tmp_buf, header.records[13]);
  if IOResult <> 0 then Exit;
  tmp_buf[header.records[13]]:=#0;
  header.filename:=strpas(tmp_buf);
  if ofs<>0 then
    Seek(f,filepos(f)+ofs);
  header.origname := header.filename;
  BackSlashes(header.filename);
  if header.oldhdrtype then begin
    if not AlignFilePointer(f, 2) then Exit;
  end else
    if not AlignFilePointer(f, 4) then Exit;

  //Correct file name started with "./" or "/"
  header.filename := correct_filename(header.filename);

  Result := True;
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
