unit uFileCopyEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCOSUtils;

const
  FILE_COPY_NO_BUFFERING = $01;

type
  TFileCopyProgress = function(TotalBytes, DoneBytes: Int64; UserData: Pointer): LongBool;
  TFileCopyEx = function(const Source, Target: String; Options: UInt32;
                         UpdateProgress: TFileCopyProgress; UserData: Pointer): LongBool;

var
  FileCopyEx: TFileCopyEx = nil;
  CopyAttributesOptionEx: TCopyAttributesOptions = [];

{$IF DEFINED(DARWIN_COPY)}
  function CopyFileF(Source, Target: THandle; Options: UInt32; UpdateProgress: TFileCopyProgress; UserData: Pointer): LongBool;
{$ENDIF}

implementation

{$IF DEFINED(MSWINDOWS)}
uses
  Windows, DCWindows;

type
  TCopyInfo = class
    UserData: Pointer;
    UpdateProgress: TFileCopyProgress;
  end;

function Progress(TotalFileSize, TotalBytesTransferred, StreamSize, StreamBytesTransferred: LARGE_INTEGER; dwStreamNumber, dwCallbackReason: DWord; hSourceFile, hDestinationFile: THandle; lpdata: pointer): Dword; Stdcall;
var
  ACopyInfo: TCopyInfo absolute lpData;
begin
  if ACopyInfo.UpdateProgress(TotalFileSize.QuadPart, TotalBytesTransferred.QuadPart, ACopyInfo.UserData) then
    Result:= PROGRESS_CONTINUE
  else begin
    Result:= PROGRESS_CANCEL;
  end;
end;

function CopyFile(const Source, Target: String; Options: UInt32;
  UpdateProgress: TFileCopyProgress; UserData: Pointer): LongBool;
var
  ACopyInfo: TCopyInfo;
  dwCopyFlags: DWORD = COPY_FILE_ALLOW_DECRYPTED_DESTINATION;
begin
  ACopyInfo:= TCopyInfo.Create;
  ACopyInfo.UserData:= UserData;
  ACopyInfo.UpdateProgress:= UpdateProgress;
  if (Options and FILE_COPY_NO_BUFFERING <> 0) then
  begin
    if (Win32MajorVersion > 5) then
      dwCopyFlags:= dwCopyFlags or COPY_FILE_NO_BUFFERING;
  end;
  Result:= CopyFileExW(PWideChar(UTF16LongName(Source)), PWideChar(UTF16LongName(Target)), @Progress, ACopyInfo, nil, dwCopyFlags) <> 0;
  ACopyInfo.Free;
end;

initialization
  FileCopyEx:= @CopyFile;
  CopyAttributesOptionEx:= [caoCopyTimeEx, caoCopyAttrEx];
{$ELSEIF DEFINED(DARWIN_COPY)}
uses
  InitC, CTypes;

const
  // copyfile_state_t values
  COPYFILE_STATE_STATUS_CB  = 6;
  COPYFILE_STATE_STATUS_CTX = 7;
  COPYFILE_STATE_COPIED     = 8;

const
  // flags for copyfile
  COPYFILE_DATA = (1 << 3);

  // progress function what
  COPYFILE_COPY_DATA = 4;

  // progress function stages
  COPYFILE_START    = 1;
  COPYFILE_FINISH   = 2;
  COPYFILE_ERR      = 3;
  COPYFILE_PROGRESS = 4;

  // callback function return values
  COPYFILE_CONTINUE = 0;
  COPYFILE_SKIP     = 1;
  COPYFILE_QUIT     = 2;

type
  TCopyInfo = class
    FileSize: Int64;
    UserData: Pointer;
    UpdateProgress: TFileCopyProgress;
  end;

type
  copyfile_flags_t = type cuint32;
  copyfile_state_t = type pointer;

function copyfile_state_free(s: copyfile_state_t): cint; cdecl; external clib;
function copyfile_state_alloc(): copyfile_state_t; cdecl; external clib;

function copyfile_state_get(s: copyfile_state_t; flag: cuint32; dst: pointer): cint; cdecl; external clib;
function copyfile_state_set(s: copyfile_state_t; flag: cuint32; const src: pointer): cint; cdecl; external clib;

function fcopyfile(from_fd, to_fd: cint; s: copyfile_state_t; flags: copyfile_flags_t): cint; cdecl; external clib;

function Callback(what, stage: cint; state: copyfile_state_t; src, dst: pansichar; ctx: pointer): cint; cdecl;
var
  BytesTransferred: coff_t;
  ACopyInfo: TCopyInfo absolute ctx;
begin
  if (what = COPYFILE_COPY_DATA) and (stage = COPYFILE_PROGRESS) then
  begin
    if (copyfile_state_get(state, COPYFILE_STATE_COPIED, @BytesTransferred) = 0) then
    begin
      if ACopyInfo.UpdateProgress(ACopyInfo.FileSize, BytesTransferred, ACopyInfo.UserData) then
        Result:= COPYFILE_CONTINUE
      else begin
        Result:= COPYFILE_QUIT;
      end;
      Exit;
    end;
  end;
  Result:= COPYFILE_CONTINUE;
end;

function CopyFileF(Source, Target: THandle; Options: UInt32; UpdateProgress: TFileCopyProgress; UserData: Pointer): LongBool;
var
  ACopyInfo: TCopyInfo;
  LastError: Integer = 0;
  AState: copyfile_state_t;
begin
  Result:= False;

  AState:= copyfile_state_alloc();

  if Assigned(AState) then
  begin
    ACopyInfo:= TCopyInfo.Create;
    ACopyInfo.UserData:= UserData;
    ACopyInfo.FileSize:= FileGetSize(Source);
    ACopyInfo.UpdateProgress:= UpdateProgress;

    copyfile_state_set(AState, COPYFILE_STATE_STATUS_CB, @Callback);
    copyfile_state_set(AState, COPYFILE_STATE_STATUS_CTX, ACopyInfo);

    Result:= fcopyfile(Source, Target, AState, COPYFILE_DATA) = 0;

    if not Result then LastError:= cerrno;

    copyfile_state_free(AState);
    ACopyInfo.Free;

    if (LastError <> 0) then
    begin
      SetLastOSError(LastError);
    end;
  end;
end;
{$ENDIF}

end.

