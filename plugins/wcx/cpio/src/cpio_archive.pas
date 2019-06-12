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
  WcxPlugin,
  cpio_def, cpio_io;

type
  PArchiveRec = ^TArchiveRec;
  TArchiveRec = record
    handle_io      : THandle;
    handle_file    : file;
    fname          : AnsiString;
    fdate          : Integer;
    fgEndArchive   : Boolean;
    process_proc   : TProcessDataProc;
    changevol_proc : TChangeVolProc;
    last_header    : CPIO_Header;
  end;{ArchiveRec}

function  GetPackerCaps : Integer; dcpcall;
function  GetBackgroundFlags: Integer; dcpcall;
function  OpenArchive(var ArchiveData : TOpenArchiveData) : TArcHandle; dcpcall;
function  CloseArchive(hArcData : TArcHandle) : Integer; dcpcall;
function  ReadHeader(hArcData : TArcHandle; var HeaderData : THeaderData) : Integer; dcpcall;
function  ProcessFile(hArcData : TArcHandle; Operation : Integer; DestPath : PChar; DestName : PChar) : Integer; dcpcall;
procedure SetProcessDataProc(hArcData : TArcHandle; ProcessDataProc : TProcessDataProc); dcpcall;
procedure SetChangeVolProc(hArcData : TArcHandle; ChangeVolProc : TChangeVolProc); dcpcall;
function  CanYouHandleThisFile(FileName: PAnsiChar): Boolean; dcpcall;

implementation

uses
  SysUtils, DCDateTimeUtils, DCBasicTypes, DCFileAttributes, DCOSUtils;

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
  if arch = -1 then begin
    fgError := True;
  end
  else begin
    New(arec);
    with arec^ do begin
      handle_io := arch;
      fname := filename;
      fdate := FileAge(filename);
      fgEndArchive := False;
      process_proc := nil;
      changevol_proc := nil;
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
  header  : CPIO_Header;
  arec    : PArchiveRec absolute hArcData;
begin
  Result := E_EREAD;
  if arec^.fgEndArchive then Result := E_END_ARCHIVE
  else begin
    while True do begin
      if CPIO_ReadHeader(arec^.handle_file, header) then begin
        if header.filename = 'TRAILER!!!' then begin
          Result := E_END_ARCHIVE;
          Break
        end
        else begin
          if header.filesize <> 0 then begin
            with HeaderData do begin
              copy_str2buf(TStrBuf(ArcName), arec^.fname);
              copy_str2buf(TStrBuf(FileName), header.filename);
              PackSize := header.filesize;
              UnpSize  := header.filesize;
              FileAttr := UnixToWcxFileAttr(header.mode);
              FileTime := UnixFileTimeToWcxTime(TUnixFileTime(header.mtime));
            end;{with}
            Result := 0;
            Break;
          end
          else
            Continue;
        end;{not end of file "TRAILER!!!"}
      end{if header readed}
      else begin
        Result := E_EREAD;
        Break;
      end;
    end;{while true}
    arec^.last_header := header;
  end;{if not end of archive}
end;

function ProcessFile(hArcData: TArcHandle; Operation: Integer; DestPath: PChar; DestName: PChar): Integer;
var
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
begin
  head := arec^.last_header;
  case Operation of
    PK_TEST : begin
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
        if arec^.last_header.IsOldHeader then begin
          if not AlignFilePointer(arec^.handle_file, 2) then Result := E_EREAD;
        end else
          if not AlignFilePointer(arec^.handle_file, 4) then Result := E_EREAD;
      end;
      FreeMem(buf, 65536);
    end;{PK_TEST}
    PK_SKIP : begin
      Seek(arec^.handle_file, FilePos(arec^.handle_file) + LongInt(head.filesize));
      if IOResult = 0 then begin
        Result := 0;
        if arec^.last_header.IsOldHeader then begin
          if not AlignFilePointer(arec^.handle_file, 2) then Result := E_EREAD;
        end else
          if not AlignFilePointer(arec^.handle_file, 4) then Result := E_EREAD;
      end else Result := E_EREAD;
    end;{PK_SKIP}
    PK_EXTRACT : begin
      cpio_name := String(DestName);
      cpio_dir := ExtractFileDir(cpio_name);
      if CreateDirectories(cpio_dir) then begin
        AssignFile(cpio_file, cpio_name);
        Rewrite(cpio_file, 1);
        if IOResult <> 0 then Result := E_ECREATE
        else begin
          fsize := head.filesize;
          buf_size := 65536;
          GetMem(buf, buf_size);
          fgReadError := False;
          fgWriteError :=False;
          fAborted := False;
          while not fAborted do begin
            if fsize < buf_size then Break;
            BlockRead(arec^.handle_file, buf^, buf_size);
            if IOResult <> 0 then begin
              fgReadError := True;
              Break;
            end;{if IO error}
            BlockWrite(cpio_file, buf^, buf_size);
            if ioresult<>0 then begin
              fgWriteError:=true;
              break;
            end;
            Dec(fsize, buf_size);
            if Assigned(arec^.process_proc) then
              if arec^.process_proc(nil, buf_size)=0 then
                fAborted:=true;
          end;{while}
          if not fgReadError then begin
            if fsize <> 0 then begin
              BlockRead(arec^.handle_file, buf^, fsize);
              if IOResult <> 0 then fgReadError := True;
              BlockWrite(cpio_file, buf^, fsize);
              if ioresult<>0 then
                fgWriteError:=true;
              if Assigned(arec^.process_proc) then
                if arec^.process_proc(nil, fsize)=0 then
                  fAborted:=true;
            end;
          end;
          if fAborted then Result:= E_EABORTED
          else if fgWriteError then Result := E_EWRITE
          else if fgReadError then Result := E_EREAD
          else begin
            Result := 0;
            if arec^.last_header.IsOldHeader then begin
              if not AlignFilePointer(arec^.handle_file, 2) then Result := E_EREAD;
            end else
              if not AlignFilePointer(arec^.handle_file, 4) then Result := E_EREAD;
          end;
          CloseFile(cpio_file);
          mbFileSetAttr(cpio_name, UnixToWcxFileAttr(head.mode));
          FileSetDate(cpio_name, UnixFileTimeToWcxTime(TUnixFileTime(head.mtime)));
          if result<>0 then
            Erase(cpio_file);
          FreeMem(buf, 65536);
        end;
      end
      else Result := E_ECREATE;
    end{PK_EXTRACT}
  else
    Result := 0;
  end;{case operation}
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
