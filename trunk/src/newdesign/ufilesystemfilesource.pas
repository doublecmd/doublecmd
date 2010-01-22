unit uFileSystemFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDCUtils,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uLocalFileSource,
  uFileSource,
  uFileSourceProperty,
  uFileProperty,
  uFile
  ;

type

  {en
     Real file system.
  }

  IFileSystemFileSource = interface(ILocalFileSource)
    ['{59EDCF45-F151-4AE2-9DCE-3586E6191496}']
  end;

  { TFileSystemFileSource }

  TFileSystemFileSource = class(TLocalFileSource, IFileSystemFileSource)

  protected
    function GetCurrentWorkingDirectory: String; override;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

  public
    constructor Create; override;

    class function GetFileSource: IFileSystemFileSource;

    function GetSupportedFileProperties: TFilePropertiesTypes; override;
    function GetOperationsTypes: TFileSourceOperationTypes; override;
    function GetFilePropertiesDescriptions: TFilePropertiesDescriptions; override;
    function GetProperties: TFileSourceProperties; override;

    function CreateFiles(const APath: String): TFiles; override;

    function IsPathAtRoot(Path: String): Boolean; override;

    function GetRootDir(sPath: String): String; override; overload;
    function GetRootDir: String; override; overload;
    function GetPathType(sPath : String): TPathType; override;

    function GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean; override;

    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOperation(var SourceFiles: TFiles;
                                 TargetPath: String): TFileSourceOperation; override;
    function CreateCopyInOperation(SourceFileSource: IFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(TargetFileSource: IFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String): TFileSourceOperation; override;
    function CreateMoveOperation(var SourceFiles: TFiles;
                                 TargetPath: String): TFileSourceOperation; override;
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; override;
    function CreateWipeOperation(var FilesToWipe: TFiles): TFileSourceOperation; override;
    function CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation; override;
    function CreateExecuteOperation(const ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation; override;
    function CreateCalcChecksumOperation(var theFiles: TFiles;
                                         aTargetPath: String;
                                         aTargetMask: String): TFileSourceOperation; override;
    function CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation; override;
    function CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                            var theNewProperties: TFileProperties): TFileSourceOperation; override;

    procedure Reload(const PathsToReload: TPathsArray); override;
    // ------------------------------------------------------
  end;

  { TFileSystemFileSourceConnection }

  TFileSystemFileSourceConnection = class(TFileSourceConnection)
  protected
    procedure SetCurrentPath(NewPath: String); override;
  end;

implementation

uses
  uOSUtils,
  uFileSystemWatcher,
  uFileSystemFile,
  uFileSystemListOperation,
  uFileSystemCopyOperation,
  uFileSystemMoveOperation,
  uFileSystemDeleteOperation,
  uFileSystemWipeOperation,
  uFileSystemCreateDirectoryOperation,
  uFileSystemExecuteOperation,
  uFileSystemCalcChecksumOperation,
  uFileSystemCalcStatisticsOperation,
  uFileSystemSetFilePropertyOperation;

constructor TFileSystemFileSource.Create;
begin
  inherited Create;
end;

class function TFileSystemFileSource.GetFileSource: IFileSystemFileSource;
var
  aFileSource: IFileSource;
begin
  aFileSource := FileSourceManager.Find(TFileSystemFileSource, '');
  if not Assigned(aFileSource) then
    Result := TFileSystemFileSource.Create
  else
    Result := aFileSource as IFileSystemFileSource;
end;

function TFileSystemFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList,
             fsoCopy,
             fsoCopyIn,
             fsoCopyOut,
             fsoMove,
             fsoDelete,
             fsoWipe,
             fsoCreateDirectory,
             fsoCalcChecksum,
             fsoCalcStatistics,
             fsoSetFileProperty,
             fsoExecute];
end;

function TFileSystemFileSource.GetFilePropertiesDescriptions: TFilePropertiesDescriptions;
begin
  SetLength(Result, 2);

  Result[0] := TFileSizeProperty.GetDescription;
  Result[1] := TFileModificationDateTimeProperty.GetDescription;
end;

function TFileSystemFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [
    fspDirectAccess
{$IFDEF UNIX}
  , fspCaseSensitive
{$ENDIF}
  ];
end;

function TFileSystemFileSource.CreateFiles(const APath: String): TFiles;
begin
  Result := TFileSystemFiles.Create(APath);
end;

function TFileSystemFileSource.GetCurrentWorkingDirectory: String;
begin
  Result := mbGetCurrentDir();
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function TFileSystemFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  if not mbDirectoryExists(NewDir) then
    Result := False
  else
    Result := mbSetCurrentDir(NewDir);
end;

function TFileSystemFileSource.IsPathAtRoot(Path: String): Boolean;
begin
  Result := (uDCUtils.GetParentDir(Path) = '');
end;

function TFileSystemFileSource.GetRootDir(sPath : String): String;
begin
  Result := uDCUtils.GetRootDir(sPath);
end;

function TFileSystemFileSource.GetRootDir: String;
begin
  Result := Self.GetRootDir(mbGetCurrentDir);
end;

function TFileSystemFileSource.GetPathType(sPath : String): TPathType;
begin
  Result := uDCUtils.GetPathType(sPath);
end;

function TFileSystemFileSource.GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean;
begin
  Result := GetDiskFreeSpace(Path, FreeSize, TotalSize);
end;

function TFileSystemFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := TFileSystemFile.GetSupportedProperties;
end;

function TFileSystemFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemListOperation.Create(TargetFileSource, TargetPath);
end;

function TFileSystemFileSource.CreateCopyOperation(var SourceFiles: TFiles;
  TargetPath: String): TFileSourceOperation;
var
  FileSource: IFileSource;
begin
  FileSource := Self;
  Result := TFileSystemCopyOperation.Create(FileSource, FileSource, SourceFiles, TargetPath);
end;

function TFileSystemFileSource.CreateCopyInOperation(SourceFileSource: IFileSource;
                                                     var SourceFiles: TFiles;
                                                     TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemCopyInOperation.Create(
                SourceFileSource, TargetFileSource,
                SourceFiles, TargetPath);
end;

function TFileSystemFileSource.CreateCopyOutOperation(TargetFileSource: IFileSource;
                                                      var SourceFiles: TFiles;
                                                      TargetPath: String): TFileSourceOperation;
var
  SourceFileSource: IFileSource;
begin
  SourceFileSource := Self;
  Result := TFileSystemCopyOutOperation.Create(
                SourceFileSource, TargetFileSource,
                SourceFiles, TargetPath);
end;

function TFileSystemFileSource.CreateMoveOperation(var SourceFiles: TFiles;
                                                   TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemMoveOperation.Create(TargetFileSource, SourceFiles, TargetPath);
end;

function TFileSystemFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemDeleteOperation.Create(TargetFileSource, FilesToDelete);
end;

function TFileSystemFileSource.CreateWipeOperation(var FilesToWipe: TFiles): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemWipeOperation.Create(TargetFileSource, FilesToWipe);
end;

function TFileSystemFileSource.CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemCreateDirectoryOperation.Create(TargetFileSource, BasePath, DirectoryPath);
end;

function TFileSystemFileSource.CreateExecuteOperation(const ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:=  TFileSystemExecuteOperation.Create(TargetFileSource, ExecutableFile, BasePath, Verb);
end;

function TFileSystemFileSource.CreateCalcChecksumOperation(var theFiles: TFiles;
                                                           aTargetPath: String;
                                                           aTargetMask: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemCalcChecksumOperation.Create(
                TargetFileSource,
                theFiles,
                aTargetPath,
                aTargetMask);
end;

function TFileSystemFileSource.CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemCalcStatisticsOperation.Create(TargetFileSource, theFiles);
end;

function TFileSystemFileSource.CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                                              var theNewProperties: TFileProperties): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TFileSystemSetFilePropertyOperation.Create(
                TargetFileSource,
                theTargetFiles,
                theNewProperties);
end;

procedure TFileSystemFileSource.Reload(const PathsToReload: TPathsArray);
begin
  // Don't reload if file watcher is used.
  if not IsFileSystemWatcher then
    inherited Reload(PathsToReload);
end;

{ TFileSystemFileSourceConnection }

procedure TFileSystemFileSourceConnection.SetCurrentPath(NewPath: String);
begin
  if not mbDirectoryExists(NewPath) then
    NewPath := mbGetCurrentDir
  else
    mbSetCurrentDir(NewPath);

  inherited SetCurrentPath(NewPath);
end;

end.

