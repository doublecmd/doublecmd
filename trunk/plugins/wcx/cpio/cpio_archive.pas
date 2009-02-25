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


{$A-,I-}
unit cpio_archive;

interface

uses
  Classes,
  uWCXhead, uUnixTime,
  cpio_def, cpio_io;

const
  MAX_ARCHIVE_LIST = 20;

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

var
  aList : TList;

function  GetPackerCaps : Integer; stdcall;

function  OpenArchive(var ArchiveData : TOpenArchiveData) : TArcHandle; stdcall;
function  CloseArchive(hArcData : TArcHandle) : Integer; stdcall;
function  ReadHeader(hArcData : TArcHandle; var HeaderData : THeaderData) : Integer; stdcall;
function  ProcessFile(hArcData : TArcHandle; Operation : Integer; DestPath : PChar; DestName : PChar) : Integer; stdcall;
procedure SetProcessDataProc(hArcData : TArcHandle; ProcessDataProc : TProcessDataProc); stdcall;
procedure SetChangeVolProc(hArcData : TArcHandle; ChangeVolProc : TChangeVolProc); stdcall;

implementation

uses
  SysUtils;

function GetArchiveID(hArcData : THandle) : Integer;
var
  i_rec   : Integer;
  arec    : PArchiveRec;
begin
  Result := -1;
  if aList.Count = 0 then Exit;
  for i_rec := 0 to (aList.Count - 1) do begin
    arec := aList.Items[i_rec];
    if arec^.handle_io = hArcData then begin
      Result := i_rec;
      Break;
    end;
  end;
end;

function GetPackerCaps;
begin
  Result := 0;
end;

function OpenArchive;
var
  arch      : THandle;
  arec      : PArchiveRec;
  filename  : String;
  fgError   : Boolean;
begin
  arec := nil;
  arch := 0;
  fgError := False;
  if aList.Count >= MAX_ARCHIVE_LIST then begin
    fgError := True;
  end
  else begin
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
  end;{max count reached}
  if fgError then begin
    if arec <> nil then begin
      CloseFile(arec^.handle_file);
      Dispose(arec);
    end;
    FileClose(arch);
    Result := 0;
    ArchiveData.OpenResult := E_EOPEN
  end
  else begin
    aList.Add(arec);
    Result := arch;
  end;
end;

function CloseArchive;
var
  i_rec   : Integer;
  arec    : PArchiveRec;
begin
  if aList.Count <> 0 then begin
    i_rec := GetArchiveID(hArcData);
    if i_rec <> -1 then begin
      arec := aList.Items[i_rec];
      CloseFile(arec^.handle_file);
      FileClose(hArcData);
      Dispose(arec);
      aList.Delete(i_rec);
    end;
  end;
  Result := E_SUCCESS;
end;

function ReadHeader;
var
  i_rec   : Integer;
  arec    : PArchiveRec;
  header  : CPIO_Header;
begin
  Result := E_EREAD;
  i_rec := GetArchiveID(hArcData);
  if i_rec <> -1 then begin
    arec := aList.Items[i_rec];
    if arec^.fgEndArchive then Result := E_END_ARCHIVE
    else begin
      while True do begin
        if CPIO_ReadHeader(arec^.handle_file, header) then begin
          if header.filename = 'TRAILER!!!' then begin
            Result := E_END_ARCHIVE;
            Break
          end
          else begin
            if header.records[08] <> 0 then begin
              with HeaderData do begin
                copy_str2buf(TStrBuf(ArcName), arec^.fname);
                copy_str2buf(TStrBuf(FileName), header.filename);
                PackSize := header.records[08];
                UnpSize  := header.records[08];
                FileAttr := $20;
                FileTime := UnixTimeToDosTime(header.records[07], True);
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
end;

function ProcessFile;
var
  i_rec       : Integer;
  arec        : PArchiveRec;
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
begin
  i_rec := GetArchiveID(hArcData);
  arec := aList.Items[i_rec];
  head := arec^.last_header;
  case Operation of
    PK_TEST : begin
      faborted:=false;
      fsize := head.records[08];
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
        if arec^.last_header.oldhdrtype then begin
          if not AlignFilePointer(arec^.handle_file, 2) then Result := E_EREAD;
        end else
          if not AlignFilePointer(arec^.handle_file, 4) then Result := E_EREAD;
      end;
      FreeMem(buf, 65536);
    end;{PK_TEST}
    PK_SKIP : begin
      Seek(arec^.handle_file, FilePos(arec^.handle_file) + LongInt(head.records[08]));
      if IOResult = 0 then begin
        Result := 0;
        if arec^.last_header.oldhdrtype then begin
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
          fsize := head.records[08];
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
            if arec^.last_header.oldhdrtype then begin
              if not AlignFilePointer(arec^.handle_file, 2) then Result := E_EREAD;
            end else
              if not AlignFilePointer(arec^.handle_file, 4) then Result := E_EREAD;
          end;
          FileSetDate(tfilerec(cpio_file).handle,UnixTimeToDosTime(head.records[07], True));
          CloseFile(cpio_file);
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

procedure SetProcessDataProc;
var
  i_rec    : Integer;
  arec     : PArchiveRec;
begin
  i_rec := GetArchiveID(hArcData);
  if i_rec <> -1 then begin
    arec := aList.Items[i_rec];
    arec^.process_proc := ProcessDataProc;
  end;
end;

procedure SetChangeVolProc;
var
  i_rec    : Integer;
  arec     : PArchiveRec;
begin
  i_rec := GetArchiveID(hArcData);
  if i_rec <> -1 then begin
    arec := aList.Items[i_rec];
    arec^.changevol_proc := ChangeVolProc;
  end;
end;

initialization
  aList := TList.Create;
finalization
  aList.Free;
end.
