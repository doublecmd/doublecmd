unit uTempFileSystemFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSystemFileSource;

type

  ITempFileSystemFileSource = interface(IFileSystemFileSource)
    ['{1B6CFF05-15D5-45AF-A382-9C12C1A52024}']

    function GetDeleteAtEnd: Boolean;
    procedure SetDeleteAtEnd(NewDeleteAtEnd: Boolean);

    property DeleteAtEnd: Boolean read GetDeleteAtEnd write SetDeleteAtEnd;
    property FileSystemRoot: String read GetRootDir;
  end;

  { TTempFileSystemFileSource }

  {en
     Filesystem file source that stores temporary files.

     Operations can be done like on a regular file system but all the contents
     can be deleted when the file source is destroyed, depending on DeleteAtEnd
     property.
  }
  TTempFileSystemFileSource = class(TFileSystemFileSource, ITempFileSystemFileSource)

  private
    FDeleteAtEnd: Boolean;
    FTempRootDir: String;

    function GetDeleteAtEnd: Boolean;
    procedure SetDeleteAtEnd(NewDeleteAtEnd: Boolean);

  protected

  public
    constructor Create; override;
    constructor Create(const aPath: String); virtual; overload;
    destructor Destroy; override;

    class function GetFileSource: ITempFileSystemFileSource;

    function IsPathAtRoot(Path: String): Boolean; override;
    function GetParentDir(sPath: String): String; override;
    function GetRootDir(sPath: String): String; override; overload;
    function GetRootDir: String; override; overload;

    function GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean; override;

    property DeleteAtEnd: Boolean read FDeleteAtEnd write FDeleteAtEnd default True;
    property FilesystemRoot: String read FTempRootDir;
  end;
  
  ETempFileSourceException = class(Exception);
  ECannotCreateTempFileSourceException = class(ETempFileSourceException);

implementation

uses
  uOSUtils, uDCUtils, uFileProcs;

function GetTempDirName: String;
const
  MaxTries = 100;
var
  sDir: String;
  TryNumber: Integer = 0;
begin
  Result := GetTempDir + '_dc';

  repeat
    sDir := Result + IntToStr(Random(MaxInt)); // or use CreateGUID()
    Inc(TryNumber, 1);
    if TryNumber = MaxTries then
      Exit('');
  until not mbDirectoryExists(sDir);

  Result := IncludeTrailingPathDelimiter(sDir);
end;

// ----------------------------------------------------------------------------

constructor TTempFileSystemFileSource.Create;
begin
  Create('');
end;

constructor TTempFileSystemFileSource.Create(const aPath: String);
begin
  inherited Create;

  if (aPath <> EmptyStr) and mbDirectoryExists(aPath) then
    FTempRootDir := aPath
  else
    FTempRootDir := GetTempDirName;

  if (FTempRootDir = EmptyStr) or (mbForceDirectory(FTempRootDir) = False) then
  begin
    FDeleteAtEnd := False;
    raise ECannotCreateTempFileSourceException.Create('Cannot create temp file source');
  end;

  FCurrentAddress := FTempRootDir;
  FDeleteAtEnd := True;

  FTempRootDir := IncludeTrailingPathDelimiter(FTempRootDir);
end;

destructor TTempFileSystemFileSource.Destroy;
begin
  inherited Destroy;

  if FDeleteAtEnd and mbDirectoryExists(FTempRootDir) then
  begin
    DelTree(FCurrentAddress);
    mbRemoveDir(FCurrentAddress);
  end;
end;

function TTempFileSystemFileSource.GetDeleteAtEnd: Boolean;
begin
  Result := FDeleteAtEnd;
end;

procedure TTempFileSystemFileSource.SetDeleteAtEnd(NewDeleteAtEnd: Boolean);
begin
  FDeleteAtEnd := NewDeleteAtEnd;
end;

class function TTempFileSystemFileSource.GetFileSource: ITempFileSystemFileSource;
begin
  Result := TTempFileSystemFileSource.Create;
end;

function TTempFileSystemFileSource.GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean;
begin
  Result := GetDiskFreeSpace(FTempRootDir, FreeSize, TotalSize);
end;

function TTempFileSystemFileSource.IsPathAtRoot(Path: String): Boolean;
begin
  Result := (IncludeTrailingPathDelimiter(Path) = FTempRootDir);
end;

function TTempFileSystemFileSource.GetParentDir(sPath: String): String;
begin
  if IsPathAtRoot(sPath) then
    Result := ''
  else
    Result := uDCUtils.GetParentDir(sPath);
end;

function TTempFileSystemFileSource.GetRootDir(sPath: String): String;
begin
  Result := FTempRootDir;
end;

function TTempFileSystemFileSource.GetRootDir: String;
begin
  Result := FTempRootDir;
end;

end.

