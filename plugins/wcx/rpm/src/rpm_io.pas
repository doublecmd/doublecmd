//***************************************************************
// This file is part of RPMWCX, a archiver plugin for
// Windows Commander.
// Copyright (C) 2000 Mandryka Yurij  e-mail:braingroup@hotmail.ru
//***************************************************************

//***************************************************************
// This code based on Christian Ghisler (support@ghisler.com) sources
//***************************************************************

//***************************************************************
// This code was improved by Sergio Daniel Freue (sfreue@dc.uba.ar)
//***************************************************************

{$A-,I-}
unit rpm_io;

interface

uses
  SysUtils,
  rpm_def;

type
  TStrBuf = array[1..260] of Char;

function  RPM_ReadLead(var f : file; var lead : RPM_Lead) : Boolean;
function  RPM_ReadSignature(var f : file; sig_type : Word; var signature : RPM_Header) : Boolean;
function  RPM_ReadHeader(var f : file; align_data : Boolean; var header : RPM_Header; var info : RPM_InfoRec) : Boolean;
function  RPM_ReadEntry(var f : file; data_start : LongInt; var entry : RPM_EntryInfo) : Boolean;
function  RPM_ProcessEntry(var f : file; data_start : LongInt; var entry : RPM_EntryInfo; var info : RPM_InfoRec) : Boolean;

procedure swap_value(var value; size : Integer);
procedure copy_str2buf(var buf : TStrBuf; s : AnsiString);
function  get_archivename(var fname : String;datasig:RPM_DataSig) : String;
function  read_string(var f : file; var s : AnsiString) : Boolean;
function  read_int32(var f : file; var int32 : LongWord) : Boolean;

implementation

uses
  Classes;

procedure swap_value(var value; size:Integer);
type
  byte_array = array[1..MaxInt] of Byte;
var
  i      : Integer;
  avalue : Byte;
begin
  for i:=1 to size div 2 do
  begin
    avalue := byte_array(value)[i];
    byte_array(value)[i] := byte_array(value)[size + 1 - i];
    byte_array(value)[size + 1 - i] := avalue;
  end;
end;

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

function get_archivename(var fname : String;datasig:RPM_DataSig) : String;
var
  tmp_str : String;
  i_char  : Integer;
  fgFound : Boolean;
begin
  tmp_str := ExtractFileName(fname);
  fgFound := False;
  for i_char := Length(tmp_str) downto 1 do
    if tmp_str[i_char] = '.' then begin
      fgFound := True;
      Break;
    end;
  if fgFound then
    SetLength(tmp_str, i_char - 1);
  if (datasig[0] = #031) and (datasig[1] = #139) then
    tmp_str := tmp_str + '.cpio.gz'
  else if (datasig[0]='B') and (datasig[1]='Z') and (datasig[2]='h') then
    tmp_str := tmp_str + '.cpio.bz2'
  else if CompareByte(datasig, #253'7zXZ'#000, 6) = 0 then
    tmp_str := tmp_str + '.cpio.xz'
  else
    tmp_str := tmp_str + '.cpio.lzma';
  Result := tmp_str;
end;

function RPM_ReadLead;
begin
  Result := False;
  BlockRead(f, lead, Sizeof(Lead));
  if IOResult = 0 then Result := True;
  with lead do begin
    swap_value(rpmtype, 2);
    swap_value(archnum, 2);
    swap_value(osnum, 2);
    swap_value(signature_type, 2);
  end;
end;

function  RPM_ReadHeader;
var
  i_entry  : LongWord;
  start    : Integer;
  entry    : RPM_EntryInfo;
begin
  Result := False;
  BlockRead(f, header, Sizeof(header));
  if IOResult = 0 then begin
    with header do begin
      swap_value(count, 4);
      swap_value(data_size, 4);
      start := FilePos(f) + LongInt(count) * Sizeof(entry);
      for i_entry := 0 to count - 1 do begin
        if not RPM_ReadEntry(f, start, entry) then Exit
        else
          if not RPM_ProcessEntry(f, start, entry, info) then Exit;
      end;
    end;
    start := start + LongInt(header.data_size);
    // Move file pointer on padded to a multiple of 8 bytes position
    if align_data then
      if (start mod 8) <> 0 then begin
        start := start and $FFFFFFF8;
        Inc(start, 8);
      end;
    Seek(f, start);
    Result := True;
  end;
end;

function  RPM_ReadEntry;
begin
  Result := False;
  BlockRead(f, entry, Sizeof(entry));
  if IOResult = 0 then Result := True;
  with entry do begin
    swap_value(tag, 4);
    swap_value(etype, 4);
    swap_value(offset, 4);
    offset := data_start + LongInt(offset);
    swap_value(count, 4);
  end;
end;

function RPM_ReadSignature;
var
  info : RPM_InfoRec;
begin
  Result := False;
  case sig_type of
    RPMSIG_PGP262_1024 : ;  // Old PGP signature
    RPMSIG_MD5         : ;  //
    RPMSIG_MD5_PGP     : ;  //
    RPMSIG_HEADERSIG   :    // New header signature
      begin
        if RPM_ReadHeader(f, True, signature, info) then Result := True;
      end;
  end;{case signature type}
end;

procedure CRtoCRLF(var instr:string);
var s:string;
    i,l:integer;
    ch,ch2:char;
begin
  instr:=instr+' ';   {Avoid overflow}
  l:=length(instr)-1;
  for i:=1 to l do begin
    ch:=instr[i];
    ch2:=instr[i+1];
    if ((ch=#13) and (ch2<>#10)) or ((ch=#10) and (ch2<>#13)) then
      s:=s+#13#10
    else
      s:=s+ch;
  end;
  instr:=s;
end;

function RPM_ProcessEntry;
var
  save_pos : Integer;
  fgError  : Boolean;
begin
  result:=true;
  if entry.tag = RPMTAG_FILENAMES then exit;
  fgError := False;
  save_pos := FilePos(f);
  Seek(f, entry.offset);
  if IOResult = 0 then begin
    case entry.tag of
      RPMTAG_NAME :
        if entry.etype = 6 then fgError := not read_string(f, info.name);
      RPMTAG_VERSION :

        if entry.etype = 6 then fgError := not read_string(f, info.version);

      RPMTAG_RELEASE :

        if entry.etype = 6 then fgError := not read_string(f, info.release);

      RPMTAG_SUMMARY :

        if entry.etype = 9 then fgError := not read_string(f, info.summary);

      RPMTAG_DESCRIPTION :

        if entry.etype = 9 then begin

          fgError := not read_string(f, info.description);

          if not fgError then

            CRtoCRLF(info.description);

        end;

      RPMTAG_BUILDTIME :

        if entry.etype = 4 then fgError := not read_int32(f, info.buildtime);

      RPMTAG_DISTRIBUTION :

        if entry.etype = 6 then fgError := not read_string(f, info.distribution);

      RPMTAG_VENDOR :

        if entry.etype = 6 then fgError := not read_string(f, info.vendor);

      RPMTAG_LICENSE :

        if entry.etype = 6 then fgError := not read_string(f, info.license);

      RPMTAG_PACKAGER :

        if entry.etype = 6 then fgError := not read_string(f, info.packager);

      RPMTAG_GROUP :

        if entry.etype = 9 then fgError := not read_string(f, info.group);

      RPMTAG_OS :

        if entry.etype = 6 then fgError := not read_string(f, info.os);

      RPMTAG_ARCH :

        if entry.etype = 6 then fgError := not read_string(f, info.arch);

      RPMTAG_SOURCERPM :

        if entry.etype = 6 then fgError := not read_string(f, info.sourcerpm);
    end;{case}

  end
  else fgError := True;
  Result := not fgError;
  Seek(f, save_pos);
end;

function read_string(var f : file; var s : AnsiString) : Boolean;
var
  i_char  : Char;
  fgError : Boolean;
begin
  fgError := False;
  SetLength(s, 0);
  while not eof(f) do begin
    BlockRead(f, i_char, 1);
    if IOResult <> 0 then begin
      fgError := True;
      Break;
    end;
    if i_char = #0
      then Break
      else s := s + i_char;
  end;
  Result := not fgError;
end;

function  read_int32(var f : file; var int32 : LongWord) : Boolean;
begin
  BlockRead(f, int32, Sizeof(LongWord));
  swap_value(int32, Sizeof(LongWord));
  if IOResult = 0 then Result := True else Result := False;
end;

procedure RPM_CreateInfoRec(var info : RPM_InfoRec);
begin
end;

procedure RPM_DeleteInfoRec(var info : RPM_InfoRec);
begin
end;

end.
