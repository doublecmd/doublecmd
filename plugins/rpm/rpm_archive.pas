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


// History
// 2001-02-04 Bug: Error Opening rpm file on CD (readonly)
//            Fix: Add FileMode = 0 before Reset
//            Who: Oliver Haeger <haeger@inghb.de>


{$A-,I-}
unit rpm_archive;

interface

uses
  Classes,
  wcx,
  rpm_def, rpm_io;

const
  MAX_ARCHIVE_LIST = 20;

type
  PArchiveRec = ^TArchiveRec;
  TArchiveRec = record
    handle_io      : Integer;
    handle_file    : file;
    fname          : AnsiString;
    fdate          : Integer;
    headers        : Integer;
    header         : RPM_Header;
    arch_len       : LongWord;
    process_proc   : TProcessDataProc;
    changevol_proc : TChangeVolProc;
//- RPM tags -------------------------------------------
    info           : RPM_InfoRec;
    is_bz2file     : boolean;
  end;{ArchiveRec}

var
  aList : TList;

function  GetPackerCaps : Integer; {$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
function  OpenArchive(var ArchiveData : TOpenArchiveData) : Integer; {$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
function  CloseArchive(hArcData : Integer) : Integer; {$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
function  ReadHeader(hArcData : Integer; var HeaderData : THeaderData) : Integer; {$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
function  ProcessFile(hArcData : Integer; Operation : Integer; DestPath : PChar; DestName : PChar) : Integer; {$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
procedure SetProcessDataProc(hArcData : Integer; ProcessDataProc : TProcessDataProc); {$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};
procedure SetChangeVolProc(hArcData : Integer; ChangeVolProc : TChangeVolProc); {$IFNDEF WIN32}cdecl{$ELSE}stdcall{$ENDIF};

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
  Result := PK_CAPS_OPTIONS or PK_CAPS_MULTIPLE;
end;

function OpenArchive;
var
  arch      : Integer;
  arec      : PArchiveRec;
  filename  : String;
  r_lead    : RPM_Lead;
  signature : RPM_Header;
  datasig   : array[0..3] of char;
  fgError   : Boolean;
  headerend : integer;
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
        headers := HDR_INFO;
        arch_len := 0;
        fdate := FileAge(filename);
        process_proc := nil;
        changevol_proc := nil;
        if fdate = -1 then fdate := 0;
      end;
      AssignFile(arec^.handle_file, filename);
      FileMode := 0;
      Reset(arec^.handle_file, 1);
      if IOResult <> 0 then begin
        fgError := True;
      end
      else begin
        RPM_ReadLead(arec^.handle_file, r_lead);
        if r_lead.magic <> RPM_MAGIC then fgError := True
        else begin
          if not RPM_ReadSignature(arec^.handle_file, r_lead.signature_type, signature) then fgError := True
          else
            if not RPM_ReadHeader(arec^.handle_file, False, arec^.header, arec^.info) then fgError := True
            else
              arec^.arch_len := FileSize(arec^.handle_file) - FilePos(arec^.handle_file);
          if not fgError then begin
            headerend:=FilePos(arec^.handle_file);
            BlockRead(arec^.handle_file, datasig, 3);
            Seek(arec^.handle_file, headerend);
            arec^.is_bz2file:=(datasig[0]='B') and (datasig[1]='Z') and (datasig[2]='h');
          end;
        end;
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
  i_rec    : Integer;
  arec     : PArchiveRec;
begin
  Result := E_END_ARCHIVE;
  i_rec := GetArchiveID(hArcData);
  if i_rec <> -1 then begin
    arec := aList.Items[i_rec];
    Result := E_SUCCESS;
    with HeaderData do
      begin
        case arec^.headers of
          HDR_DATA: begin
              copy_str2buf(TStrBuf(FileName), get_archivename(arec^.fname,arec^.is_bz2file));
              PackSize := arec^.arch_len;
              UnpSize  := arec^.arch_len;
            end;
          HDR_INFO: begin
              copy_str2buf(TStrBuf(FileName), 'INFO.TXT');
              PackSize := 0;
              UnpSize  := 0;
            end;
          else
            Result := E_END_ARCHIVE;
        end;
        if Result = E_SUCCESS then
          begin
            copy_str2buf(TStrBuf(ArcName), arec^.fname);
            FileAttr := $20;
            FileTime := UnixTimeToDosTime(arec^.info.buildtime, True);
            Inc(arec^.headers);
          end;
      end;
  end;
end;

function ProcessFile;
var
  i_rec       : Integer;
  arec        : PArchiveRec;
  rpm_file    : file;
  rpm_name    : String;
  buf         : Pointer;
  buf_size    : LongWord;
  fsize       : LongWord;
  fgReadError : Boolean;
  fgWriteError: Boolean;
  faborted    : Boolean;
  testonly    : Boolean;

//Helper function to output one line of text to rpm_file
function Line(S: AnsiString): Integer;
begin
  Result := 0;
  if not fgReadError and not fgWriteError then 
    if testonly then
      Result := Length(S) + 2
    else
      begin
        S := S + #13#10;
        BlockWrite(rpm_file, S[1], Length(S), Result);
        if IOResult <> 0 then fgWriteError := True;
      end;
end;

begin
  case Operation of
// Because rpm archive doesn't contains length of _alone_ attached
// gzipped cpio archive, plugin cann't skip or test rpm archive
// correctly without extracting archive.
    PK_SKIP : Result := E_SUCCESS;
    PK_TEST,
    PK_EXTRACT : begin
      testonly:=Operation=PK_TEST;
      if not testonly then begin
        rpm_name := String(DestName);
        AssignFile(rpm_file, rpm_name);
        Rewrite(rpm_file, 1);
      end;
      if not testonly and (IOResult <> 0) then begin
        Result := E_EWRITE
      end
      else begin
        i_rec := GetArchiveID(hArcData);
        arec := aList.Items[i_rec];
        fgReadError := False;
        fgWriteError := False;
        faborted:=false;
        case (arec^.headers-1) of
          HDR_DATA: begin
              fsize := arec^.arch_len;
              buf_size := 65536;
              GetMem(buf, buf_size);
              while not faborted do begin
                if fsize < buf_size then Break;
                BlockRead(arec^.handle_file, buf^, buf_size);
                if IOResult <> 0 then begin
                  fgReadError := True;
                  Break;
                end;{if IO error}
                if not testonly then begin
                  BlockWrite(rpm_file, buf^, buf_size);
                  if IOResult <> 0 then begin
                    fgWriteError := True;
                    Break;
                  end;{if IO error}
                end;
                Dec(fsize, buf_size);
                if Assigned(arec^.process_proc) then
                  if arec^.process_proc(nil, buf_size)=0 then
                    faborted:=true;
              end;{while}
              if not fgReadError and not fgWriteError and not faborted then begin
                if fsize <> 0 then begin
                  BlockRead(arec^.handle_file, buf^, fsize);
                  if IOResult <> 0 then fgReadError := True;
                  if not testonly and not fgReadError then begin
                    BlockWrite(rpm_file, buf^, fsize);
                    if IOResult <> 0 then
                      fgWriteError := True;
                  end;
                  if Assigned(arec^.process_proc) then
                    if arec^.process_proc(nil, fsize)=0 then
                      faborted:=true;
                end;
              end;
              Freemem(buf, buf_size);
//Other pseudo-files
            end;
          HDR_INFO: with arec^.info do begin
              Line('NAME:         ' + name);
              Line('VERSION:      ' + version);
              Line('RELEASE:      ' + release);
              Line('SUMMARY:      ' + summary);
              Line('DISTRIBUTION: ' + distribution);
              Line('VENDOR:       ' + vendor);
              Line('LICENSE:      ' + license);
              Line('PACKAGER:     ' + packager);
              Line('GROUP:        ' + group);
              Line('OS:           ' + os);
              Line('ARCH:         ' + arch);
              Line('SOURCE RPM:   ' + sourcerpm);
              Line('DESCRIPTION:  ');
              Line(description);
            end;
        end;
        if faborted then Result:=E_EABORTED
        else if fgReadError then Result := E_BAD_DATA
        else if fgWriteError then Result:= E_EWRITE
        else
          Result := E_SUCCESS;
        if not testonly then begin
          if result = E_SUCCESS then
            FileSetDate(tfilerec(rpm_file).handle,UnixTimeToDosTime(arec^.info.buildtime, True));
          CloseFile(rpm_file);
          if result <> E_SUCCESS then
            Erase(rpm_file);
        end;
      end;
    end
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
