unit deb_archive;

interface

{$mode delphi}{$A-,I-}
{$include calling.inc}

uses
  Classes,
  WcxPlugin,
  deb_def, deb_io;

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
    last_header    : deb_Header;
  end;{ArchiveRec}

function  GetPackerCaps : Integer; dcpcall;
function  GetBackgroundFlags: Integer; dcpcall;
function  OpenArchive(var ArchiveData : TOpenArchiveData) : TArcHandle; dcpcall;
function  CloseArchive(hArcData : TArcHandle) : Integer; dcpcall;
function  ReadHeader(hArcData : TArcHandle; var HeaderData : THeaderData) : Integer; dcpcall;
function  ProcessFile(hArcData : TArcHandle; Operation : Integer; DestPath : PChar; DestName : PChar) : Integer; dcpcall;
procedure SetProcessDataProc(hArcData : TArcHandle; ProcessDataProc : TProcessDataProc); dcpcall;
procedure SetChangeVolProc(hArcData : TArcHandle; ChangeVolProc : TChangeVolProc); dcpcall;

implementation

uses
  SysUtils, DCDateTimeUtils, DCBasicTypes, DCFileAttributes;

function GetPackerCaps: Integer;
begin
  Result := PK_CAPS_MULTIPLE;
end;

function GetBackgroundFlags: Integer;
begin
  Result:= BACKGROUND_UNPACK;
end;

function OpenArchive(var ArchiveData: TOpenArchiveData): TArcHandle;
var
  arch      : THandle;
  filename  : String;
  fgError   : Boolean;
  arec      : PArchiveRec absolute Result;

  function SignatureProbe: integer; //0 Ales Gut; 1 IO error; 2 is not DEBIAN PKG
  const
    deb_signature: array [0..20] of Char ='!<arch>'#10'debian-binary';
  var
    tmp_buf    : array [0..20] of Char;
    j          : integer;
  begin
    Result:=2;
    BlockRead(arec^.handle_file, tmp_buf, 21);
    if IOResult <> 0 then begin Result:=1; Exit; end;
    for j:=0 to 20 do if deb_signature[j] <> tmp_buf[j] then Exit;
    Result:=0;
  end;

begin
  ArchiveData.OpenResult := E_EOPEN;
  arec := nil;
  arch := 0;
  fgError := False;

  filename := String(ArchiveData.ArcName);
  arch := FileOpen(filename, fmOpenRead or fmShareDenyNone);
  if arch = THandle(-1) then begin
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
      last_header.size:=0;
      last_header.pos:=size_deb_signature;
    end;
    AssignFile(arec^.handle_file, filename);
    FileMode := 0;
    Reset(arec^.handle_file, 1);
    if IOResult <> 0 then begin
      fgError := True;
    end else begin
      case SignatureProbe of
        1:  begin
              ArchiveData.OpenResult := E_EREAD;
              fgError := True;
            end;
        2:  begin
              ArchiveData.OpenResult := E_UNKNOWN_FORMAT;
              fgError := True;
            end
        else begin
              Seek(arec^.handle_file, size_deb_signature);
              if IOResult <> 0 then fgError := True;
        end;
      end;{case SignatureProbe}
    end;{ioresult}
  end;{arch = -1}
  if fgError then begin
    if arec <> nil then begin
      CloseFile(arec^.handle_file);
      Dispose(arec);
    end;
    FileClose(arch);
    Result := 0;
  end
  else begin
    ArchiveData.OpenResult := E_SUCCESS;
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

function ReadHeader(hArcData: TArcHandle; var HeaderData: THeaderData): Integer;
var
  header  : deb_Header;
  arec    : PArchiveRec absolute hArcData;
begin
  Result := E_EREAD;
  if arec^.fgEndArchive then Result := E_END_ARCHIVE
  else begin
    while True do begin
      if not deb_ReadHeader(arec^.handle_file, header, arec^.last_header) then begin
          Result := E_END_ARCHIVE;
          Break
      end
      else begin
          with HeaderData do begin
            StrPCopy(ArcName, arec^.fname);
            StrPCopy(FileName, header.filename);
            PackSize := header.size;
            UnpSize  := header.size;
            UnpVer   := 2;
            HostOS   := 0;
            FileCRC  := 0;
            FileAttr := UnixToWcxFileAttr(header.mode);
            FileTime := UnixFileTimeToWcxTime(TUnixFileTime(header.time));
          end;{with}
          Result := E_SUCCESS;
          Break;
      end{if header readed}
    end;{while true}
    arec^.last_header := header;
  end;{if not end of archive}
end;

function ProcessFile(hArcData: TArcHandle; Operation: Integer; DestPath: PChar; DestName: PChar): Integer;
var
  targz_file  : file;
  targz_name  : String;
  buf         : Pointer;
  buf_size    : LongWord;
  fsize       : LongWord;
  fpos        : LongWord;
  fgReadError : Boolean;
  fgWriteError: Boolean;
  fAborted    : Boolean;
  head        : deb_Header;
  arec        : PArchiveRec absolute hArcData;
begin
  head := arec^.last_header;
  case Operation of
    PK_TEST : begin
      fAborted:=false;
      fsize := head.size;
      fpos := head.pos;
      buf_size := 65536;
      GetMem(buf, buf_size);
      fgReadError := False;
      Seek(arec^.handle_file, fpos);
      if IOResult <> 0 then begin fgReadError := True; fAborted:=True; end;
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
             fAborted:=true;
      end;{while}
      if not fgReadError and not faborted then begin
         if fsize <> 0 then begin
           BlockRead(arec^.handle_file, buf^, fsize);
           if IOResult <> 0 then fgReadError := True;
           if Assigned(arec^.process_proc) then
             if arec^.process_proc(nil, fsize)=0 then
               fAborted:=true;
         end;
      end;
      if fAborted then Result:=E_EABORTED
      else if fgReadError then Result := E_EREAD
      else Result := 0;
      Seek(arec^.handle_file, size_deb_signature);
      FreeMem(buf, 65536);
    end;{PK_TEST}
    PK_SKIP : Result := 0;
    PK_EXTRACT : begin
      targz_name := String(DestName);
        AssignFile(targz_file, targz_name);
        Rewrite(targz_file, 1);
        if IOResult <> 0 then Result := E_ECREATE
        else begin
          fgReadError := False;
          fgWriteError :=False;
          fAborted := False;
          fsize := head.size;
          fpos := head.pos;
          buf_size := 65536;
          GetMem(buf, buf_size);
          Seek(arec^.handle_file, fpos);
          if IOResult <> 0 then begin fgReadError := True; fAborted:=True; end;
          while not fAborted do begin
            if fsize < buf_size then Break;
            BlockRead(arec^.handle_file, buf^, buf_size);
            if IOResult <> 0 then begin
              fgReadError := True;
              Break;
            end;{if IO error}
            BlockWrite(targz_file, buf^, buf_size);
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
              BlockWrite(targz_file, buf^, fsize);
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
          else Result := 0;
          FileSetDate(tfilerec(targz_file).handle, UnixFileTimeToWcxTime(TUnixFileTime(head.time)));
          CloseFile(targz_file);
          Seek(arec^.handle_file, size_deb_signature);
          if result<>0 then
            Erase(targz_file);
          FreeMem(buf, 65536);
        end;
    end;{PK_EXTRACT}
  else
    Result := E_SUCCESS;
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

end.
