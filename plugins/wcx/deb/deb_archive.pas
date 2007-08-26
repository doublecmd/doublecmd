unit deb_archive;

interface

{$A-,I-}

uses
  Classes,
  uWCXhead, uUnixTime,
  deb_def, deb_io;

const
  MAX_ARCHIVE_LIST = 20;

type
  PArchiveRec = ^TArchiveRec;
  TArchiveRec = record
    handle_io      : Integer;
    handle_file    : file;
    fname          : AnsiString;
    fdate          : Integer;
    fgEndArchive   : Boolean;
    process_proc   : TProcessDataProc;
    changevol_proc : TChangeVolProc;
    last_header    : deb_Header;
  end;{ArchiveRec}

var
  aList : TList;

function  GetPackerCaps : Integer; stdcall;
function  OpenArchive(var ArchiveData : TOpenArchiveData) : Integer; stdcall;
function  CloseArchive(hArcData : Integer) : Integer; stdcall;
function  ReadHeader(hArcData : Integer; var HeaderData : THeaderData) : Integer; stdcall;
function  ProcessFile(hArcData : Integer; Operation : Integer; DestPath : PChar; DestName : PChar) : Integer; stdcall;
procedure SetProcessDataProc(hArcData : Integer; ProcessDataProc : TProcessDataProc); stdcall;
procedure SetChangeVolProc(hArcData : Integer; ChangeVolProc : TChangeVolProc); stdcall;

implementation

uses
  SysUtils;

function GetArchiveID(hArcData : Integer) : Integer;
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
  Result := PK_CAPS_MULTIPLE;
end;

function OpenArchive;
var
  arch      : Integer;
  arec      : PArchiveRec;
  filename  : String;
  fgError   : Boolean;

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
  end;{max count reached}
  if fgError then begin
    if arec <> nil then begin
      CloseFile(arec^.handle_file);
      Dispose(arec);
    end;
    FileClose(arch);
    Result := 0;
  end
  else begin
    aList.Add(arec);
    Result := arch;
    ArchiveData.OpenResult := E_SUCCESS;
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
  header  : deb_Header;
begin
  Result := E_EREAD;
  i_rec := GetArchiveID(hArcData);
  if i_rec <> -1 then begin
    arec := aList.Items[i_rec];
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
              FileCRC  := header.CRC;
              FileAttr := $20;
              FileTime := UnixTimeToDosTime(header.time, True);
            end;{with}
            Result := E_SUCCESS;
            Break;
        end{if header readed}
      end;{while true}
      arec^.last_header := header;
    end;{if not end of archive}
  end;
end;

function ProcessFile;
var
  i_rec       : Integer;
  arec        : PArchiveRec;
  targz_file   : file;
  targz_name   : String;
  buf         : Pointer;
  buf_size    : LongWord;
  fsize       : LongWord;
  fpos        : LongWord;
  fgReadError : Boolean;
  fgWriteError: Boolean;
  fAborted    : Boolean;
  head        : deb_Header;
begin
  i_rec := GetArchiveID(hArcData);
  arec := aList.Items[i_rec];
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
          i_rec := GetArchiveID(hArcData);
          arec := aList.Items[i_rec];
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
          FileSetDate(tfilerec(targz_file).handle,UnixTimeToDosTime(head.time, True));
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
