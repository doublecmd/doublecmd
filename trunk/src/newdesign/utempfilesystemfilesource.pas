unit uTempFileSystemFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSystemFileSource;

type

  ITempFileSystemFileSource = interface(IFileSystemFileSource)
    ['{1B6CFF05-15D5-45AF-A382-9C12C1A52024}']

    function GetDeleteOnDestroy: Boolean;
    procedure SetDeleteOnDestroy(NewDeleteOnDestroy: Boolean);

    property DeleteOnDestroy: Boolean read GetDeleteOnDestroy write SetDeleteOnDestroy;
    property FileSystemRoot: String read GetRootDir;
  end;

  { TTempFileSystemFileSource }

  {en
     Filesystem file source that stores temporary files.

     Operations can be done like on a regular file system but all the contents
     can be deleted when the file source is destroyed, depending on DeleteOnDestroy
     property.
  }
  TTempFileSystemFileSource = class(TFileSystemFileSource, ITempFileSystemFileSource)

  private
    FDeleteOnDestroy: Boolean;
    FTempRootDir: String;

    function GetDeleteOnDestroy: Boolean;
    procedure SetDeleteOnDestroy(NewDeleteOnDestroy: Boolean);

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

    property DeleteOnDestroy: Boolean read FDeleteOnDestroy write FDeleteOnDestroy default True;
    property FilesystemRoot: String read FTempRootDir;
  end;
  
  ETempFileSourceException = class(Exception);
  ECannotCreateTempFileSourceException = class(ETempFileSourceException);

implementation

uses
  DCOSUtils, uOSUtils, DCStrUtils, uFileProcs;

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
  begin
    FTempRootDir := GetTempName(GetTempDir + '_dc');

    if (FTempRootDir = EmptyStr) or (mbForceDirectory(FTempRootDir) = False) then
    begin
      FDeleteOnDestroy := False;
      raise ECannotCreateTempFileSourceException.Create('Cannot create temp file source');
    end;
  end;

  FCurrentAddress := FTempRootDir;
  FDeleteOnDestroy := True;

  FTempRootDir := IncludeTrailingPathDelimiter(FTempRootDir);
end;

destructor TTempFileSystemFileSource.Destroy;
begin
  inherited Destroy;

  if FDeleteOnDestroy and mbDirectoryExists(FTempRootDir) then
  begin
    DelTree(FCurrentAddress);
    mbRemoveDir(FCurrentAddress);
  end;
end;

function TTempFileSystemFileSource.GetDeleteOnDestroy: Boolean;
begin
  Result := FDeleteOnDestroy;
end;

procedure TTempFileSystemFileSource.SetDeleteOnDestroy(NewDeleteOnDestroy: Boolean);
begin
  FDeleteOnDestroy := NewDeleteOnDestroy;
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
    Result := DCStrUtils.GetParentDir(sPath);
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
