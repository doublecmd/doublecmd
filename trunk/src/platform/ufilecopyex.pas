unit uFileCopyEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  FILE_COPY_NO_BUFFERING = $01;

type
  TFileCopyProgress = function(TotalBytes, DoneBytes: Int64; UserData: Pointer): LongBool;
  TFileCopyEx = function(const Source, Target: String; Options: UInt32;
                         UpdateProgress: TFileCopyProgress; UserData: Pointer): LongBool;

var
  FileCopyEx: TFileCopyEx = nil;

implementation

{$IF DEFINED(MSWINDOWS)}
uses
  Windows, DCWindows, DCOSUtils;

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
  if (Options and FILE_COPY_NO_BUFFERING <> 0) then begin
    dwCopyFlags:= dwCopyFlags or COPY_FILE_NO_BUFFERING;
  end;
  Result:= CopyFileExW(PWideChar(UTF16LongName(Source)), PWideChar(UTF16LongName(Target)), @Progress, ACopyInfo, nil, dwCopyFlags) <> 0;
  ACopyInfo.Free;
end;

initialization
  FileCopyEx:= @CopyFile;
{$ENDIF}

end.

