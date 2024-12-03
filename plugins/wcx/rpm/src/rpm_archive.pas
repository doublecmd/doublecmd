//***************************************************************
// This file is part of RPMWCX, a archiver plugin for
// Windows Commander.
// Copyright (C) 2000 Mandryka Yurij  e-mail:braingroup@hotmail.ru
//***************************************************************
{
  Add some changes for Lazarus and Linux compability
  Copyright (C) 2007-2012  Koblov Alexander (Alexx2000@mail.ru)
}
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

unit rpm_archive;

{$mode delphi}{$A-,I-}
{$include calling.inc}

interface

uses
  Classes,
  WcxPlugin,
  rpm_def, rpm_io;

type
  PArchiveRec = ^TArchiveRec;
  TArchiveRec = record
    handle_io      : THandle;
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
    deps           : RPM_DepsRec;
    datasig        : RPM_DataSig;
  end;{ArchiveRec}

function  GetPackerCaps : Integer; dcpcall; export;
function  GetBackgroundFlags: Integer; dcpcall; export;
function  OpenArchive(var ArchiveData : TOpenArchiveData) : TArcHandle; dcpcall; export;
function  CloseArchive(hArcData : TArcHandle) : Integer; dcpcall; export;
function  ReadHeader(hArcData : TArcHandle; var HeaderData : THeaderData) : Integer; dcpcall; export;
function  ProcessFile(hArcData : TArcHandle; Operation : Integer; DestPath : PChar; DestName : PChar) : Integer; dcpcall; export;
procedure SetProcessDataProc(hArcData : TArcHandle; ProcessDataProc : TProcessDataProc); dcpcall; export;
procedure SetChangeVolProc(hArcData : TArcHandle; ChangeVolProc : TChangeVolProc); dcpcall; export;

implementation

uses
  SysUtils, DCDateTimeUtils, DCBasicTypes, DCFileAttributes;

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
  r_lead    : RPM_Lead;
  signature : RPM_Header;
  fgError   : Boolean;
  headerend : integer;
  arec      : PArchiveRec absolute Result;
begin
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
          if not RPM_ReadHeader(arec^.handle_file, False, arec^.header, arec^.info, arec^.deps) then fgError := True
          else
            arec^.arch_len := FileSize(arec^.handle_file) - FilePos(arec^.handle_file);
        if not fgError then begin
          headerend:=FilePos(arec^.handle_file);
          BlockRead(arec^.handle_file, arec^.datasig, SizeOf(RPM_DataSig));
          Seek(arec^.handle_file, headerend);
        end;
      end;
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

function ReadHeader(hArcData: TArcHandle; var HeaderData: THeaderData): Integer;
var
  arec : PArchiveRec absolute hArcData;
begin
  Result := E_SUCCESS;
  with HeaderData do
    begin
      case arec^.headers of
        HDR_DATA: begin
            copy_str2buf(TStrBuf(FileName), get_archivename(arec^.fname,arec^.datasig));
            PackSize := arec^.arch_len;
            UnpSize  := arec^.arch_len;
          end;
        HDR_INFO: begin
            copy_str2buf(TStrBuf(FileName), 'INFO.TXT');
            PackSize := -1;
            UnpSize  := -1;
          end;
        else
          Result := E_END_ARCHIVE;
      end;
      if Result = E_SUCCESS then
        begin
          copy_str2buf(TStrBuf(ArcName), arec^.fname);
          FileAttr := GENERIC_ATTRIBUTE_FILE;
          FileTime := UnixFileTimeToWcxTime(TUnixFileTime(arec^.info.buildtime));
          Inc(arec^.headers);
        end;
    end;
end;

function ProcessFile(hArcData: TArcHandle; Operation: Integer; DestPath: PChar; DestName: PChar): Integer;
var
  rpm_file    : file;
  rpm_name    : String;
  index       : Integer;
  buf         : Pointer;
  buf_size    : LongWord;
  fsize       : LongWord;
  fgReadError : Boolean;
  fgWriteError: Boolean;
  faborted    : Boolean;
  testonly    : Boolean;
  arec        : PArchiveRec absolute hArcData;

  // Helper function to output one line of text to rpm_file
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
              if Length(arec^.deps.names) > 0 then
              begin
                Line(EmptyStr);
                Line('REQUIRES:     ');
                for index:= 0 to High(arec^.deps.names) do
                begin
                  Line('  ' + arec^.deps.names[index]);
                end;
              end;
            end;
        end;
        if faborted then Result:=E_EABORTED
        else if fgReadError then Result := E_BAD_DATA
        else if fgWriteError then Result:= E_EWRITE
        else
          Result := E_SUCCESS;
        if not testonly then begin
          if result = E_SUCCESS then
            FileSetDate(tfilerec(rpm_file).handle, UnixFileTimeToWcxTime(TUnixFileTime(arec^.info.buildtime)));
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
