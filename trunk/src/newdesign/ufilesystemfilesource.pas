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

  { TFileSystemFileSource }

  TFileSystemFileSource = class(TLocalFileSource)

  protected
    procedure SetCurrentPath(NewPath: String); override;

  public
    constructor Create; override;
    constructor Create(Path: String); overload;

    function Clone: TFileSystemFileSource; override;
    procedure CloneTo(FileSource: TFileSource); override;

    class function GetSupportedFileProperties: TFilePropertiesTypes; override;
    class function GetOperationsTypes: TFileSourceOperationTypes; override;
    class function GetFilePropertiesDescriptions: TFilePropertiesDescriptions; override;
    class function GetProperties: TFileSourceProperties; override;

    function IsAtRootPath: Boolean; override;

    class function GetRootDir(sPath: String): String; override;
    class function GetPathType(sPath : String): TPathType; override;

    function GetFreeSpace(out FreeSize, TotalSize : Int64) : Boolean; override;

    function CreateListOperation: TFileSourceOperation; override;
    function CreateCopyInOperation(var SourceFileSource: TFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(var TargetFileSource: TFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String): TFileSourceOperation; override;
    function CreateMoveOperation(var SourceFiles: TFiles;
                                 TargetPath: String): TFileSourceOperation; override;
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; override;
    function CreateWipeOperation(var FilesToWipe: TFiles): TFileSourceOperation; override;
    function CreateCreateDirectoryOperation(DirectoryPath: String): TFileSourceOperation; override;
    function CreateExecuteOperation(ExecutablePath, Verb: String): TFileSourceOperation; override;
    function CreateCalcChecksumOperation(var theFiles: TFiles;
                                         aTargetPath: String;
                                         aTargetMask: String): TFileSourceOperation; override;
    function CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation; override;
    // ------------------------------------------------------
  end;

implementation

uses
  uOSUtils,
  uFileSystemFile,
  uFileSystemListOperation,
  uFileSystemCopyOperation,
  uFileSystemMoveOperation,
  uFileSystemDeleteOperation,
  uFileSystemWipeOperation,
  uFileSystemCreateDirectoryOperation,
  uFileSystemExecuteOperation,
  uFileSystemCalcChecksumOperation,
  uFileSystemCalcStatisticsOperation;

constructor TFileSystemFileSource.Create;
begin
  Create(mbGetCurrentDir);
end;

constructor TFileSystemFileSource.Create(Path: String);
begin
  inherited Create;
  inherited SetCurrentPath(Path);
  FCurrentAddress := '';
end;

function TFileSystemFileSource.Clone: TFileSystemFileSource;
begin
  Result := TFileSystemFileSource.Create(FCurrentPath);
  CloneTo(Result);
end;

procedure TFileSystemFileSource.CloneTo(FileSource: TFileSource);
begin
  if Assigned(FileSource) then
  begin
    inherited CloneTo(FileSource);
  end;
end;

class function TFileSystemFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList,
             fsoCopyIn,
             fsoCopyOut,
             fsoMove,
             fsoDelete,
             fsoWipe,
             fsoCreateDirectory,
             fsoCalcChecksum,
             fsoCalcStatistics,
             fsoSetName,
             fsoSetAttribute,
             fsoExecute];
             //fsoSetPath / fsoChangePath
end;

class function TFileSystemFileSource.GetFilePropertiesDescriptions: TFilePropertiesDescriptions;
begin
  SetLength(Result, 2);

  Result[0] := TFileSizeProperty.GetDescription;
  Result[1] := TFileModificationDateTimeProperty.GetDescription;
end;

class function TFileSystemFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [
    fspDirectAccess
{$IFDEF UNIX}
  , fspCaseSensitive
{$ENDIF}
  ];
end;

procedure TFileSystemFileSource.SetCurrentPath(NewPath: String);
begin
  if not mbDirectoryExists(NewPath) then
    NewPath := mbGetCurrentDir
  else
    mbSetCurrentDir(NewPath);

  inherited SetCurrentPath(NewPath);
end;

function TFileSystemFileSource.IsAtRootPath: Boolean;
begin
  Result := (uDCUtils.GetParentDir(CurrentPath) = '');
end;

class function TFileSystemFileSource.GetRootDir(sPath : String): String;
begin
  Result := uDCUtils.GetRootDir(sPath);
end;

class function TFileSystemFileSource.GetPathType(sPath : String): TPathType;
begin
  Result := uDCUtils.GetPathType(sPath);
end;

function TFileSystemFileSource.GetFreeSpace(out FreeSize, TotalSize : Int64) : Boolean;
begin
  Result := GetDiskFreeSpace(CurrentPath, FreeSize, TotalSize);
end;

class function TFileSystemFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := TFileSystemFile.GetSupportedProperties;
end;

function TFileSystemFileSource.CreateListOperation: TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TFileSystemListOperation.Create(TargetFileSource);
end;

function TFileSystemFileSource.CreateCopyInOperation(var SourceFileSource: TFileSource;
                                                     var SourceFiles: TFiles;
                                                     TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TFileSystemCopyInOperation.Create(
                SourceFileSource, TargetFileSource,
                SourceFiles, TargetPath);
end;

function TFileSystemFileSource.CreateCopyOutOperation(var TargetFileSource: TFileSource;
                                                      var SourceFiles: TFiles;
                                                      TargetPath: String): TFileSourceOperation;
var
  SourceFileSource: TFileSource;
begin
  SourceFileSource := Self.Clone;
  Result := TFileSystemCopyOutOperation.Create(
                SourceFileSource, TargetFileSource,
                SourceFiles, TargetPath);
end;

function TFileSystemFileSource.CreateMoveOperation(var SourceFiles: TFiles;
                                                   TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TFileSystemMoveOperation.Create(TargetFileSource, SourceFiles, TargetPath);
end;

function TFileSystemFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TFileSystemDeleteOperation.Create(TargetFileSource, FilesToDelete);
end;

function TFileSystemFileSource.CreateWipeOperation(var FilesToWipe: TFiles): TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TFileSystemWipeOperation.Create(TargetFileSource, FilesToWipe);
end;

function TFileSystemFileSource.CreateCreateDirectoryOperation(DirectoryPath: String): TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TFileSystemCreateDirectoryOperation.Create(TargetFileSource, DirectoryPath);
end;

function TFileSystemFileSource.CreateExecuteOperation(ExecutablePath, Verb: String): TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result:=  TFileSystemExecuteOperation.Create(TargetFileSource, ExecutablePath, Verb);
end;

function TFileSystemFileSource.CreateCalcChecksumOperation(var theFiles: TFiles;
                                                           aTargetPath: String;
                                                           aTargetMask: String): TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TFileSystemCalcChecksumOperation.Create(
                TargetFileSource,
                theFiles,
                aTargetPath,
                aTargetMask);
end;

function TFileSystemFileSource.CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TFileSystemCalcStatisticsOperation.Create(TargetFileSource, theFiles);
end;

end.

