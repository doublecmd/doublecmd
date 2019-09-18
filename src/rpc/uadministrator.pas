unit uAdministrator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCBasicTypes;

function FileExistsUAC(const FileName: String): Boolean;
function FileGetAttrUAC(const FileName: String): TFileAttrs;
function FileSetAttrUAC(const FileName: String; Attr: TFileAttrs): Integer;
function FileSetTimeUAC(const FileName: String;
                        ModificationTime: DCBasicTypes.TFileTime;
                        CreationTime    : DCBasicTypes.TFileTime = 0;
                        LastAccessTime  : DCBasicTypes.TFileTime = 0): LongBool;

function FileOpenUAC(const FileName: String; Mode: LongWord): System.THandle;
function FileCreateUAC(const FileName: String; Mode: LongWord): System.THandle;

function DeleteFileUAC(const FileName: String): LongBool;
function RenameFileUAC(const OldName, NewName: String): LongBool;

function ForceDirectoriesUAC(const Path: String): Boolean;
function CreateDirectoryUAC(const Directory: String): Boolean;
function RemoveDirectoryUAC(const Directory: String): Boolean;
function DirectoryExistsUAC(const Directory : String): Boolean;

function CreateSymbolicLinkUAC(const Path, LinkName: String) : Boolean;
function CreateHardLinkUAC(const Path, LinkName: String) : Boolean;

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

threadvar
  ElevateAction: Boolean;

implementation

uses
  RtlConsts, DCStrUtils, DCOSUtils, LCLType, uShowMsg, uElevation, uSuperUser,
  fElevation;

resourcestring
  rsElevationRequired = 'You need to provide administrator permission';
  rsElevationRequiredDelete = 'to delete this object:';
  rsElevationRequiredOpen = 'to open this object:';
  rsElevationRequiredCreate = 'to create this object:';
  rsElevationRequiredRename = 'to rename this object:';
  rsElevationRequiredHardLink = 'to create this hard link:';
  rsElevationRequiredSymLink = 'to create this symbolic link:';
  rsElevationRequiredGetAttributes = 'to get attributes of this object:';
  rsElevationRequiredSetAttributes = 'to set attributes of this object:';

function RequestElevation(const Message, FileName: String): Boolean;
var
  Text: String;
begin
  if ElevateAction then Exit(True);
  Text:= rsElevationRequired + LineEnding;
  Text += Message + LineEnding + FileName;
  case ShowElevation(mbSysErrorMessage, Text) of
    mmrOK: Result:= True;
    mmrCancel: Result:= False;
    mmrAll: begin
      Result:= True;
      ElevateAction:= True;
    end;
  end;
end;

function FileExistsUAC(const FileName: String): Boolean;
var
  LastError: Integer;
begin
  Result:= mbFileExists(FileName);
  if (not Result) and ElevationRequired then
  begin
    LastError:= GetLastOSError;
    if RequestElevation(rsElevationRequiredGetAttributes, FileName) then
      Result:= TWorkerProxy.Instance.FileExists(FileName)
    else
      SetLastOSError(LastError);
  end;
end;

function FileGetAttrUAC(const FileName: String): TFileAttrs;
var
  LastError: Integer;
begin
  Result:= mbFileGetAttr(FileName);
  if (Result = faInvalidAttributes) and ElevationRequired then
  begin
    LastError:= GetLastOSError;
    if RequestElevation(rsElevationRequiredGetAttributes, FileName) then
      Result:= TWorkerProxy.Instance.FileGetAttr(FileName)
    else
      SetLastOSError(LastError);
  end;
end;

function FileSetAttrUAC(const FileName: String; Attr: TFileAttrs): Integer;
var
  LastError: Integer;
begin
  Result:= mbFileSetAttr(FileName, Attr);
  if (Result <> 0) and ElevationRequired then
  begin
    LastError:= GetLastOSError;
    if RequestElevation(rsElevationRequiredSetAttributes, FileName) then
      Result:= TWorkerProxy.Instance.FileSetAttr(FileName, Attr)
    else
      SetLastOSError(LastError);
  end;
end;

function FileSetTimeUAC(const FileName: String;
                        ModificationTime: DCBasicTypes.TFileTime;
                        CreationTime    : DCBasicTypes.TFileTime;
                        LastAccessTime  : DCBasicTypes.TFileTime): LongBool;
var
  LastError: Integer;
begin
  Result:= mbFileSetTime(FileName, ModificationTime, CreationTime, LastAccessTime);
  if (not Result) and ElevationRequired then
  begin
    LastError:= GetLastOSError;
    if RequestElevation(rsElevationRequiredSetAttributes, FileName) then
      Result:= TWorkerProxy.Instance.FileSetTime(FileName, ModificationTime, CreationTime, LastAccessTime)
    else
      SetLastOSError(LastError);
  end;
end;

function FileOpenUAC(const FileName: String; Mode: LongWord): System.THandle;
var
  LastError: Integer;
begin
  Result:= mbFileOpen(FileName, Mode);
  if (Result = feInvalidHandle) and ElevationRequired then
  begin
    LastError:= GetLastOSError;
    if RequestElevation(rsElevationRequiredOpen, FileName) then
      Result:= TWorkerProxy.Instance.FileOpen(FileName, Mode)
    else
      SetLastOSError(LastError);
  end;
end;

function FileCreateUAC(const FileName: String; Mode: LongWord): System.THandle;
var
  LastError: Integer;
begin
  Result:= mbFileCreate(FileName, Mode);
  if (Result = feInvalidHandle) and ElevationRequired then
  begin
    LastError:= GetLastOSError;
    if RequestElevation(rsElevationRequiredCreate, FileName) then
      Result:= TWorkerProxy.Instance.FileCreate(FileName, Mode)
    else
      SetLastOSError(LastError);
  end;
end;

function DeleteFileUAC(const FileName: String): LongBool;
var
  LastError: Integer;
begin
  Result:= mbDeleteFile(FileName);
  if (not Result) and ElevationRequired then
  begin
    LastError:= GetLastOSError;
    if RequestElevation(rsElevationRequiredDelete, FileName) then
      Result:= TWorkerProxy.Instance.DeleteFile(FileName)
    else
      SetLastOSError(LastError);
  end;
end;

function RenameFileUAC(const OldName, NewName: String): LongBool;
var
  LastError: Integer;
begin
  Result:= mbRenameFile(OldName, NewName);
  if (not Result) and ElevationRequired then
  begin
    LastError:= GetLastOSError;
    if RequestElevation(rsElevationRequiredRename, OldName) then
      Result:= TWorkerProxy.Instance.RenameFile(OldName, NewName)
    else
      SetLastOSError(LastError);
  end;
end;

function CreateDirectoryUAC(const Directory: String): Boolean;
var
  LastError: Integer;
begin
  Result:= mbCreateDir(Directory);
  if (not Result) and ElevationRequired then
  begin
    LastError:= GetLastOSError;
    if RequestElevation(rsElevationRequiredCreate, Directory) then
      Result:= TWorkerProxy.Instance.CreateDirectory(Directory)
    else
      SetLastOSError(LastError);
  end;
end;

function RemoveDirectoryUAC(const Directory: String): Boolean;
var
  LastError: Integer;
begin
  Result:= mbRemoveDir(Directory);
  if (not Result) and ElevationRequired then
  begin
    LastError:= GetLastOSError;
    if RequestElevation(rsElevationRequiredDelete, Directory) then
      Result:= TWorkerProxy.Instance.RemoveDirectory(Directory)
    else
      SetLastOSError(LastError);
  end;
end;

function DirectoryExistsUAC(const Directory: String): Boolean;
var
  LastError: Integer;
begin
  Result:= mbDirectoryExists(Directory);
  if (not Result) and ElevationRequired then
  begin
    LastError:= GetLastOSError;
    if RequestElevation(rsElevationRequiredGetAttributes, Directory) then
      Result:= TWorkerProxy.Instance.DirectoryExists(Directory)
    else
      SetLastOSError(LastError);
  end;
end;

function CreateHardLinkUAC(const Path, LinkName: String): Boolean;
var
  LastError: Integer;
begin
  Result:= CreateHardLink(Path, LinkName);
  if (not Result) and ElevationRequired then
  begin
    LastError:= GetLastOSError;
    if RequestElevation(rsElevationRequiredHardLink, LinkName) then
      Result:= TWorkerProxy.Instance.CreateHardLink(Path, LinkName)
    else
      SetLastOSError(LastError);
  end;
end;

function CreateSymbolicLinkUAC(const Path, LinkName: String): Boolean;
var
  LastError: Integer;
begin
  Result:= CreateSymLink(Path, LinkName);
  if (not Result) and ElevationRequired then
  begin
    LastError:= GetLastOSError;
    if RequestElevation(rsElevationRequiredSymLink, LinkName) then
      Result:= TWorkerProxy.Instance.CreateSymbolicLink(Path, LinkName)
    else
      SetLastOSError(LastError);
  end;
end;

function ForceDirectoriesUAC(const Path: String): Boolean;
var
  Index: Integer;
  ADirectory: String;
  ADirectoryPath: String;
begin
  if Path = '' then Exit;
  ADirectoryPath := IncludeTrailingPathDelimiter(Path);
  Index:= 1;
  if Pos('\\', ADirectoryPath) = 1 then // if network path
  begin
    Index := CharPos(PathDelim, ADirectoryPath, 3); // index of the end of computer name
    Index := CharPos(PathDelim, ADirectoryPath, Index + 1); // index of the end of first remote directory
  end;

  // Move past path delimiter at the beginning.
  if (Index = 1) and (ADirectoryPath[Index] = PathDelim) then
    Index := Index + 1;

  while Index <= Length(ADirectoryPath) do
  begin
    if ADirectoryPath[Index] = PathDelim then
    begin
      ADirectory:= Copy(ADirectoryPath, 1, Index - 1);

      if not DirectoryExistsUAC(ADirectory) then
      begin
        Result:= CreateDirectoryUAC(ADirectory);
        if not Result then Exit;
      end;
    end;
    Inc(Index);
  end;
  Result := True;
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

end.

