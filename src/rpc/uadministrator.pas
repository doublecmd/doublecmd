unit uAdministrator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCBasicTypes, DCClassesUtf8, DCOSUtils, uFindEx;

procedure PushPop(var Elevate: TDuplicates);

function FileExistsUAC(const FileName: String): Boolean;
function FileGetAttrUAC(const FileName: String; FollowLink: Boolean = False): TFileAttrs;
function FileSetAttrUAC(const FileName: String; Attr: TFileAttrs): Boolean;
function FileSetTimeUAC(const FileName: String;
                        ModificationTime: DCBasicTypes.TFileTime;
                        CreationTime    : DCBasicTypes.TFileTime = 0;
                        LastAccessTime  : DCBasicTypes.TFileTime = 0): LongBool;
function FileSetReadOnlyUAC(const FileName: String; ReadOnly: Boolean): Boolean;
function FileCopyAttrUAC(const sSrc, sDst: String;
                         Options: TCopyAttributesOptions): TCopyAttributesOptions;

function FileOpenUAC(const FileName: String; Mode: LongWord): System.THandle;
function FileCreateUAC(const FileName: String; Mode: LongWord): System.THandle;

function DeleteFileUAC(const FileName: String): LongBool;
function RenameFileUAC(const OldName, NewName: String): LongBool;

function FindFirstUAC(const Path: String; Flags: UInt32; out SearchRec: TSearchRecEx): Integer;
function FindNextUAC(var SearchRec: TSearchRecEx): Integer;
procedure FindCloseUAC(var SearchRec: TSearchRecEx);

function ForceDirectoriesUAC(const Path: String): Boolean;
function CreateDirectoryUAC(const Directory: String): Boolean;
function RemoveDirectoryUAC(const Directory: String): Boolean;
function DirectoryExistsUAC(const Directory : String): Boolean;

function CreateSymbolicLinkUAC(const Path, LinkName: String) : Boolean;
function CreateHardLinkUAC(const Path, LinkName: String) : Boolean;

type

  { TFileStreamUAC class }

  TFileStreamUAC = class(TFileStreamEx)
  public
    constructor Create(const AFileName: String; Mode: LongWord); override;
  end;

  { TStringListUAC }

  TStringListUAC = class(TStringListEx)
  public
    procedure LoadFromFile(const FileName: String); override;
    procedure SaveToFile(const FileName: String); override;
  end;

threadvar
  ElevateAction: TDuplicates;

implementation

uses
  RtlConsts, DCStrUtils, LCLType, uShowMsg, uElevation, uSuperUser,
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

procedure PushPop(var Elevate: TDuplicates);
var
  AValue: TDuplicates;
begin
  AValue:= ElevateAction;
  ElevateAction:= Elevate;
  Elevate:= AValue;
end;

function RequestElevation(const Message, FileName: String): Boolean;
var
  Text: String;
begin
  case ElevateAction of
    dupAccept: Exit(True);
    dupError: Exit(False);
  end;
  Text:= rsElevationRequired + LineEnding;
  Text += Message + LineEnding + FileName;
  case ShowElevation(mbSysErrorMessage, Text) of
    mmrOK: Result:= True;
    mmrSkip: Result:= False;
    mmrSkipAll: begin
      Result:= False;
      ElevateAction:= dupError;
    end;
    mmrAll: begin
      Result:= True;
      ElevateAction:= dupAccept;
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

function FileGetAttrUAC(const FileName: String; FollowLink: Boolean): TFileAttrs;
var
  LastError: Integer;
begin
  if not FollowLink then
    Result:= mbFileGetAttr(FileName)
  else begin
    Result:= mbFileGetAttrNoLinks(FileName);
  end;
  if (Result = faInvalidAttributes) and ElevationRequired then
  begin
    LastError:= GetLastOSError;
    if RequestElevation(rsElevationRequiredGetAttributes, FileName) then
      Result:= TWorkerProxy.Instance.FileGetAttr(FileName, FollowLink)
    else
      SetLastOSError(LastError);
  end;
end;

function FileSetAttrUAC(const FileName: String; Attr: TFileAttrs): Boolean;
var
  LastError: Integer;
begin
  Result:= mbFileSetAttr(FileName, Attr);
  if (not Result) and ElevationRequired then
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

function FileSetReadOnlyUAC(const FileName: String; ReadOnly: Boolean): Boolean;
var
  LastError: Integer;
begin
  Result:= mbFileSetReadOnly(FileName, ReadOnly);
  if (not Result) and ElevationRequired then
  begin
    LastError:= GetLastOSError;
    if RequestElevation(rsElevationRequiredSetAttributes, FileName) then
      Result:= TWorkerProxy.Instance.FileSetReadOnly(FileName, ReadOnly)
    else
      SetLastOSError(LastError);
  end;
end;

function FileCopyAttrUAC(const sSrc, sDst: String;
  Options: TCopyAttributesOptions): TCopyAttributesOptions;
var
  Option: TCopyAttributesOption;
  Errors: TCopyAttributesResult;
begin
  Result:= mbFileCopyAttr(sSrc, sDst, Options, @Errors);
  if (Result <> []) then
  begin
    for Option in Result do
    begin
      if ElevationRequired(Errors[Option]) then
      begin
        if RequestElevation(rsElevationRequiredSetAttributes, sDst) then
          Result:= TWorkerProxy.Instance.FileCopyAttr(sSrc, sDst, Result)
        else
          SetLastOSError(Errors[Option]);
        Break;
      end;
    end;
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

function FindFirstUAC(const Path: String; Flags: UInt32; out
  SearchRec: TSearchRecEx): Integer;
begin
  Result:= FindFirstEx(Path, Flags, SearchRec);
  if (Result <> 0) and ElevationRequired(Result) then
  begin
    if RequestElevation(rsElevationRequiredOpen, Path) then
    begin
      SearchRec.Flags:= SearchRec.Flags or fffElevated;
      Result:= TWorkerProxy.Instance.FindFirst(Path, Flags, SearchRec)
    end;
  end;
end;

function FindNextUAC(var SearchRec: TSearchRecEx): Integer;
begin
  if (SearchRec.Flags and fffElevated <> 0) then
    Result:= TWorkerProxy.Instance.FindNext(SearchRec)
  else
    Result:= FindNextEx(SearchRec);
end;

procedure FindCloseUAC(var SearchRec: TSearchRecEx);
begin
  if (SearchRec.Flags and fffElevated <> 0) then
    TWorkerProxy.Instance.FindClose(SearchRec)
  else
    FindCloseEx(SearchRec);
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

constructor TFileStreamUAC.Create(const AFileName: String; Mode: LongWord);
var
  AHandle: System.THandle;
begin
  if (Mode and fmCreate) <> 0 then
    begin
      AHandle:= FileCreateUAC(AFileName, Mode);
      if AHandle = feInvalidHandle then
        raise EFCreateError.CreateFmt(SFCreateError, [AFileName])
      else
        inherited Create(AHandle);
    end
  else
    begin
      AHandle:= FileOpenUAC(AFileName, Mode);
      if AHandle = feInvalidHandle then
        raise EFOpenError.CreateFmt(SFOpenError, [AFilename])
      else
        inherited Create(AHandle);
    end;
  FFileName:= AFileName;
end;

{ TStringListUAC }

procedure TStringListUAC.LoadFromFile(const FileName: String);
var
  fsFileStream: TFileStreamUAC;
begin
  fsFileStream:= TFileStreamUAC.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(fsFileStream);
  finally
    fsFileStream.Free;
  end;
end;

procedure TStringListUAC.SaveToFile(const FileName: String);
var
  AMode: LongWord;
  fsFileStream: TFileStreamUAC;
begin
  if not FileExistsUAC(FileName) then
    AMode:= fmCreate
  else begin
    AMode:= fmOpenWrite or fmShareDenyWrite;
  end;
  fsFileStream:= TFileStreamUAC.Create(FileName, AMode);
  try
    SaveToStream(fsFileStream);
    if (AMode <> fmCreate) then fsFileStream.Size:= fsFileStream.Position;
  finally
    fsFileStream.Free;
  end;
end;

end.
