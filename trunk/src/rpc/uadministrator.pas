unit uAdministrator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function FileOpenUAC(const FileName: String; Mode: LongWord): System.THandle;
function FileCreateUAC(const FileName: String; Mode: LongWord): System.THandle;

function DeleteFileUAC(const FileName: String): LongBool;
function RenameFileUAC(const OldName, NewName: String): LongBool;

function CreateDirectoryUAC(const Directory: String): Boolean;
function RemoveDirectoryUAC(const Directory: String): Boolean;

type

  { TFileStreamUAC class }

  TFileStreamUAC = class(THandleStream)
  private
    FHandle: THandle;
    FFileName: String;
  protected
    procedure SetSize64(const NewSize: Int64); override;
  public
    constructor Create(const AFileName: String; Mode: LongWord);
    destructor Destroy; override;
    function Flush: Boolean;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    property FileName: String read FFileName;
  end;

implementation

uses
  RtlConsts, DCOSUtils, Dialogs, UITypes, uElevation, uSuperUser;

resourcestring
  rsElevationRequired = 'You need to provide administrator permission';
  rsElevationRequiredDelete = 'to delete this object:';
  rsElevationRequiredOpen = 'to open this object:';
  rsElevationRequiredCreate = 'to create this object:';
  rsElevationRequiredRename = 'to rename this object:';

function RequestElevation(const Message, FileName: String): Boolean;
var
  Text: String;
begin
  Text:= rsElevationRequired + LineEnding;
  Text += Message + LineEnding + FileName;
  Result:= MessageDlg(mbSysErrorMessage, Text, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

function FileOpenUAC(const FileName: String; Mode: LongWord): System.THandle;
begin
  Result:= mbFileOpen(FileName, Mode);
  if (Result = feInvalidHandle) and ElevationRequired then
  begin
    if RequestElevation(rsElevationRequiredOpen, FileName) then
      Result:= TWorkerProxy.Instance.FileOpen(FileName, Mode);
  end;
end;

function FileCreateUAC(const FileName: String; Mode: LongWord): System.THandle;
begin
  Result:= mbFileCreate(FileName, Mode);
  if (Result = feInvalidHandle) and ElevationRequired then
  begin
    if RequestElevation(rsElevationRequiredCreate, FileName) then
      Result:= TWorkerProxy.Instance.FileCreate(FileName, Mode);
  end;
end;

function DeleteFileUAC(const FileName: String): LongBool;
begin
  Result:= mbDeleteFile(FileName);
  if (not Result) and ElevationRequired then
  begin
    if RequestElevation(rsElevationRequiredDelete, FileName) then
      Result:= TWorkerProxy.Instance.DeleteFile(FileName);
  end;
end;

function RenameFileUAC(const OldName, NewName: String): LongBool;
begin
  Result:= mbRenameFile(OldName, NewName);
  if (not Result) and ElevationRequired then
  begin
    if RequestElevation(rsElevationRequiredRename, OldName) then
      Result:= TWorkerProxy.Instance.RenameFile(OldName, NewName);
  end;
end;

function CreateDirectoryUAC(const Directory: String): Boolean;
begin
  Result:= mbCreateDir(Directory);
  if (not Result) and ElevationRequired then
  begin
    if RequestElevation(rsElevationRequiredCreate, Directory) then
      Result:= TWorkerProxy.Instance.CreateDirectory(Directory);
  end;
end;

function RemoveDirectoryUAC(const Directory: String): Boolean;
begin
  Result:= mbRemoveDir(Directory);
  if (not Result) and ElevationRequired then
  begin
    if RequestElevation(rsElevationRequiredDelete, Directory) then
      Result:= TWorkerProxy.Instance.RemoveDirectory(Directory);
  end;
end;

{ TFileStreamUAC }

procedure TFileStreamUAC.SetSize64(const NewSize: Int64);
begin
  FileAllocate(FHandle, NewSize);
end;

constructor TFileStreamUAC.Create(const AFileName: String; Mode: LongWord);
begin
  if (Mode and fmCreate) <> 0 then
    begin
      FHandle:= FileCreateUAC(AFileName, Mode);
      if FHandle = feInvalidHandle then
        raise EFCreateError.CreateFmt(SFCreateError, [AFileName])
      else
        inherited Create(FHandle);
    end
  else
    begin
      FHandle:= FileOpenUAC(AFileName, Mode);
      if FHandle = feInvalidHandle then
        raise EFOpenError.CreateFmt(SFOpenError, [AFilename])
      else
        inherited Create(FHandle);
    end;
  FFileName:= AFileName;
end;

destructor TFileStreamUAC.Destroy;
begin
  inherited Destroy;
  // Close handle after destroying the base object, because it may use Handle in Destroy.
  if FHandle <> feInvalidHandle then FileClose(FHandle);
end;

function TFileStreamUAC.Flush: Boolean;
begin
  Result:= FileFlush(FHandle);
end;

function TFileStreamUAC.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result:= FileRead(FHandle, Buffer, Count);
  if Result = -1 then
    raise EReadError.Create(mbSysErrorMessage(GetLastOSError));
end;

procedure ElevateProcedure;
begin
  ExecCmdAdmin(ParamStr(0), ['--service', IntToStr(GetProcessID)]);
end;

procedure Initialize;
begin
  ElevateSelf:= @ElevateProcedure;
end;

initialization
  Initialize;

end.

