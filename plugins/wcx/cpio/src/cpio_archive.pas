//***************************************************************
// This file is part of RPMWCX, a archiver plugin for
// Windows Commander.
// Copyright (C) 2000 Mandryka Yurij  e-mail:braingroup@hotmail.ru
//***************************************************************
{
  Add some changes for Lazarus and Linux compability
  Copyright (C) 2007-2009  Koblov Alexander (Alexx2000@mail.ru)
}
//***************************************************************
// This code based on Christian Ghisler (support@ghisler.com) sources
//***************************************************************


// History
// 2001-02-04 Bug: Error Opening rpm file on CD (readonly)
//            Fix: Add FileMode = 0 before Reset
//            Who: Oliver Haeger <haeger@inghb.de>
// 2001-02-27 Bug: My or Ghisler I don't know : WC incorrectly
//                 work with names in archive started with
//                 "./" or "/" (normal UNIX filenames form)

unit cpio_archive;

interface

{$mode delphi}{$A-,I-}
{$include calling.inc}

uses
  Classes,
  SysUtils,
  WcxPlugin,
  cpio_def, cpio_io;

type
  PArchiveRec = ^TArchiveRec;
  TArchiveRec = record
    handle_io      : THandle;
    handle_file    : file;
    fname          : AnsiString;
    fdate          : Integer;
    process_proc   : TProcessDataProc;
    changevol_proc : TChangeVolProc;
    last_header    : CPIO_Header;
    hard_links     : TStringArray;
    hard_ready     : Boolean;
    hard_index     : Integer;
    hard_result    : Integer;
    hard_name      : String;
  end;{ArchiveRec}

function  GetPackerCaps : Integer; dcpcall; export;
function  GetBackgroundFlags: Integer; dcpcall; export;
function  OpenArchive(var ArchiveData : TOpenArchiveData) : TArcHandle; dcpcall; export;
function  CloseArchive(hArcData : TArcHandle) : Integer; dcpcall; export;
function  ReadHeader(hArcData : TArcHandle; var HeaderData : THeaderData) : Integer; dcpcall; export;
function  ProcessFile(hArcData : TArcHandle; Operation : Integer; DestPath : PChar; DestName : PChar) : Integer; dcpcall; export;
procedure SetProcessDataProc(hArcData : TArcHandle; ProcessDataProc : TProcessDataProc); dcpcall; export;
procedure SetChangeVolProc(hArcData : TArcHandle; ChangeVolProc : TChangeVolProc); dcpcall; export;
function  CanYouHandleThisFile(FileName: PAnsiChar): LongBool; dcpcall; export;

implementation

uses
  DCDateTimeUtils, DCBasicTypes, DCFileAttributes, DCOSUtils;

function GetPackerCaps: Integer;
begin
  Result := PK_CAPS_MULTIPLE;
end;

function GetBackgroundFlags: Integer;
begin
  Result := BACKGROUND_UNPACK;
end;

function OpenArchive(var ArchiveData : TOpenArchiveData) : TArcHandle;
var
  arch      : THandle;
  filename  : String;
  fgError   : Boolean;
  arec      : PArchiveRec absolute Result;
begin
  arec := nil;
  arch := 0;
  fgError := False;

  filename := String(ArchiveData.ArcName);
  arch := FileOpen(filename, fmOpenRead or fmShareDenyNone);
  if arch = feInvalidHandle then
  begin
    fgError := True;
  end
  else begin
    New(arec);
    with arec^ do begin
      handle_io := arch;
      fname := filename;
      fdate := FileAge(filename);
      process_proc := nil;
      changevol_proc := nil;
      hard_index := -1;
      if fdate = -1 then fdate := 0;
    end;
    AssignFile(arec^.handle_file, filename);
    FileMode := 0;
    Reset(arec^.handle_file, 1);
    if IOResult <> 0 then begin
      fgError := True;
    end;{ioresult}
  end;{arch = -1}
  if fgError then begin
    if arec <> nil then begin
      CloseFile(arec^.handle_file);
      Dispose(arec);
    end;
    FileClose(arch);
    Result := 0;
    ArchiveData.OpenResult := E_EOPEN
  end;
end;

function CloseArchive(hArcData: TArcHandle): Integer;
var
  arec : PArchiveRec absolute hArcData;
begin
  CloseFile(arec^.handle_file);
  FileClose(arec^.handle_io);
  Dispose(arec);
  Result := E_SUCCESS;
end;

function ReadHeader(hArcData : TArcHandle; var HeaderData : THeaderData): Integer;
var
  ofs         : Int64;
  Index       : Integer;
  header      : CPIO_Header;
  hard_header : CPIO_Header;
  arec        : PArchiveRec absolute hArcData;

  procedure CopyHeader(var header: CPIO_Header; const fname: String);
  begin
    with HeaderData do
    begin
      copy_str2buf(TStrBuf(ArcName), arec^.fname);
      copy_str2buf(TStrBuf(FileName), fname);
      PackSize := header.filesize;
      UnpSize  := header.filesize;
      FileAttr := UnixToWcxFileAttr(header.mode);
      FileTime := UnixFileTimeToWcxTime(TUnixFileTime(header.mtime));
    end;
  end;

begin
  Result := E_EREAD;

  if arec^.hard_index > -1 then
  begin
    CopyHeader(arec^.last_header, arec^.hard_links[arec^.hard_index]);
    Result := E_SUCCESS;
    Exit;
  end;

  while True do
  begin
    if CPIO_ReadHeader(arec^.handle_file, header) then
    begin
      if header.filename = '.' then
        Continue;
      if header.filename = 'TRAILER!!!' then
      begin
        Result := E_END_ARCHIVE;
        Break;
      end;
      // File is a hard link
      if (header.nlink > 1) and (header.header_type = htNewChr) and
         (header.filesize = 0) and ((header.mode and S_IFMT) <> S_IFDIR) then
      begin
        hard_header:= header;
        SetLength(arec^.hard_links, header.nlink);
        arec^.hard_links[0]:= header.filename;
        // Read all file hard links, they follow one by one
        for Index := 1 to header.nlink - 1 do
        begin
          ofs:= FilePos(arec^.handle_file);
          if CPIO_ReadHeader(arec^.handle_file, header) then
          begin
            if (header.inode = hard_header.inode) then
              arec^.hard_links[Index] := header.filename
            else begin // Zero size hard link
              SetLength(arec^.hard_links, Index);
              Seek(arec^.handle_file, ofs);
              header:= hard_header;
              Break;
            end;
            // The last hard link in the sequence has a non-zero size
            if (header.filesize > 0) then
            begin
              SetLength(arec^.hard_links, Index + 1);
              Break;
            end;
          end
          else begin
            Result:= E_EREAD;
            Break;
          end;
        end;
        arec^.hard_index:= 0;
        arec^.hard_ready:= False;
        CopyHeader(header, arec^.hard_links[0]);
        Result := E_SUCCESS;
        Break;
      end;
      CopyHeader(header, header.filename);
      Result := E_SUCCESS;
      Break;
    end{if header readed}
    else begin
      Result := E_EREAD;
      Break;
    end;
  end;{while true}
  arec^.last_header := header;
end;

function ProcessFile(hArcData: TArcHandle; Operation: Integer; DestPath: PChar; DestName: PChar): Integer;
var
  handle_file : file;
  cpio_file   : file;
  cpio_name   : String;
  cpio_dir    : String;
  buf         : Pointer;
  buf_size    : LongWord;
  fsize       : LongWord;
  fgReadError : Boolean;
  fgWriteError: Boolean;
  fAborted    : Boolean;
  head        : CPIO_Header;
  arec        : PArchiveRec absolute hArcData;

  procedure NextLink;
  begin
    Inc(arec^.hard_index);
    if (arec^.hard_index > High(arec^.hard_links)) then
    begin
      arec^.hard_index:= -1;
    end;
  end;

begin
  head := arec^.last_header;
  case Operation of
    PK_TEST : begin
      if (arec^.hard_index > -1) and (arec^.hard_ready) then
      begin
        NextLink;
        Exit(arec^.hard_result);
      end;
      faborted:=false;
      fsize := head.filesize;
      buf_size := 65536;
      GetMem(buf, buf_size);
      fgReadError := False;
      while not faborted do begin
        if fsize < buf_size then Break;
        BlockRead(arec^.handle_file, buf^, buf_size);
        if IOResult <> 0 then begin
          fgReadError := True;
          Break;
        end;{if IO error}
        Dec(fsize, buf_size);
        if Assigned(arec^.process_proc) then
          if arec^.process_proc(nil, buf_size)=0 then
            faborted:=true;
      end;{while}
      if not fgReadError and not faborted then begin
        if fsize <> 0 then begin
          BlockRead(arec^.handle_file, buf^, fsize);
          if IOResult <> 0 then fgReadError := True;
          if Assigned(arec^.process_proc) then
            arec^.process_proc(nil, fsize);
        end;
      end;
      if faborted then Result:=E_EABORTED
      else if fgReadError then Result := E_EREAD
      else begin
        Result := 0;
        case arec^.last_header.header_type of
          htOldBin:
            if not AlignFilePointer(arec^.handle_file, 2) then Result := E_EREAD;
          htNewChr:
            if not AlignFilePointer(arec^.handle_file, 4) then Result := E_EREAD;
        end;
      end;
      FreeMem(buf, 65536);
      if (arec^.hard_index > -1) then
      begin
        arec^.hard_ready:= True;
        arec^.hard_result:= Result;
      end;
    end;{PK_TEST}
    PK_SKIP : begin
      if (arec^.hard_index > -1) and (arec^.hard_index <= High(arec^.hard_links)) then
      begin
        NextLink;
        Exit(E_SUCCESS);
      end;
      Seek(arec^.handle_file, FilePos(arec^.handle_file) + Int64(head.filesize));
      if IOResult = 0 then begin
        Result := 0;
        case arec^.last_header.header_type of
          htOldBin:
            if not AlignFilePointer(arec^.handle_file, 2) then Result := E_EREAD;
          htNewChr:
            if not AlignFilePointer(arec^.handle_file, 4) then Result := E_EREAD;
        end;
      end else Result := E_EREAD;
    end;{PK_SKIP}
    PK_EXTRACT : begin
      cpio_name := String(DestName);
      cpio_dir := ExtractFileDir(cpio_name);
      if CreateDirectories(cpio_dir) then
      begin
        if (arec^.hard_index > -1) and (arec^.hard_ready) then
        begin
          // Try to restore a hard link
          if CreateHardLink(arec^.hard_name, cpio_name) then
          begin
            NextLink;
            Exit(E_SUCCESS);
          end
          // Create a copy instead
          else begin
            AssignFile(handle_file, arec^.hard_name);
            FileMode := 0;
            Reset(handle_file, 1);
            if IOResult <> 0 then
            begin
              NextLink;
              Exit(E_EOPEN);
            end;
          end;
        end
        else begin
          handle_file:= arec^.handle_file;
        end;
        AssignFile(cpio_file, cpio_name);
        Rewrite(cpio_file, 1);
        if IOResult <> 0 then Result := E_ECREATE
        else begin
          if Assigned(arec^.process_proc) then
          begin
            arec^.process_proc(PAnsiChar(arec^.last_header.filename), 0);
          end;
          fsize := head.filesize;
          buf_size := 65536;
          GetMem(buf, buf_size);
          fgReadError := False;
          fgWriteError :=False;
          fAborted := False;
          while fsize > 0 do
          begin
            if fsize < buf_size then
            begin
              buf_size:= fsize;
            end;
            BlockRead(handle_file, buf^, buf_size);
            if IOResult <> 0 then begin
              fgReadError := True;
              Break;
            end;{if IO error}
            BlockWrite(cpio_file, buf^, buf_size);
            if IOResult <> 0 then begin
              fgWriteError:= True;
              Break;
            end;
            Dec(fsize, buf_size);
            if Assigned(arec^.process_proc) then
            begin
              if arec^.process_proc(nil, buf_size) = 0 then
              begin
                fAborted:= True;
                Break;
              end;
            end;
          end;{while}
          if fAborted then Result:= E_EABORTED
          else if fgWriteError then Result := E_EWRITE
          else if fgReadError then Result := E_EREAD
          else begin
            Result := E_SUCCESS;
            if FileRec(handle_file).Handle = FileRec(arec^.handle_file).Handle then
            begin
              case arec^.last_header.header_type of
                htOldBin:
                  if not AlignFilePointer(arec^.handle_file, 2) then Result := E_EREAD;
                htNewChr:
                  if not AlignFilePointer(arec^.handle_file, 4) then Result := E_EREAD;
              end;
            end;
          end;
          CloseFile(cpio_file);
          if Result <> 0 then
            Erase(cpio_file)
          else begin
            mbFileSetAttr(cpio_name, UnixToWcxFileAttr(head.mode));
            FileSetDate(cpio_name, UnixFileTimeToWcxTime(TUnixFileTime(head.mtime)));
          end;
          FreeMem(buf, 65536);
        end;
        if (arec^.hard_index > -1) then
        begin
          if (not arec^.hard_ready) then
          begin
            arec^.hard_ready:= True;
            arec^.hard_result:= Result;
            arec^.hard_name:= cpio_name;
          end;
          if FileRec(handle_file).Handle <> FileRec(arec^.handle_file).Handle then
            CloseFile(handle_file);
        end;
      end
      else Result := E_ECREATE;
    end{PK_EXTRACT}
  else
    Result := 0;
  end;{case operation}
  if (arec^.hard_index > -1) then NextLink;
end;

procedure SetProcessDataProc(hArcData: TArcHandle; ProcessDataProc: TProcessDataProc);
var
  arec : PArchiveRec absolute hArcData;
begin
  if hArcData <> wcxInvalidHandle then
  begin
    arec^.process_proc := ProcessDataProc;
  end;
end;

procedure SetChangeVolProc(hArcData: TArcHandle; ChangeVolProc: TChangeVolProc);
var
  arec : PArchiveRec absolute hArcData;
begin
  if hArcData <> wcxInvalidHandle then
  begin
    arec^.changevol_proc := ChangeVolProc;
  end;
end;

function CanYouHandleThisFile;
begin
  try
    Result:= IsCPIOArchive(StrPas(FileName));
  except
    Result := False;
  end;
end;

end.
