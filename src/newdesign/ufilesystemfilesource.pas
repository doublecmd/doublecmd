unit uFileSystemFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
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

    function GetFiles: TFiles; override;

    function CreateListOperation: TFileSourceOperation; override;
    function CreateCopyInOperation(var SourceFileSource: TFileSource;
                                   var SourceFiles: TFiles;
                                   TargetPath: String;
                                   RenameMask: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(var TargetFileSource: TFileSource;
                                    var SourceFiles: TFiles;
                                    TargetPath: String;
                                    RenameMask: String): TFileSourceOperation; override;
    function CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation; override;

    // ------------------------------------------------------
  end;

implementation

uses
  uOSUtils, uFileSystemFile,
  uFileSystemListOperation,
  uFileSystemCopyOperation,
  uFileSystemDeleteOperation;

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
             fsoDelete,
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

class function TFileSystemFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := TFileSystemFile.GetSupportedProperties;
end;

function TFileSystemFileSource.GetFiles: TFiles;
var
  ListOperation: TFileSystemListOperation;
begin
  Result := nil;

  ListOperation := CreateListOperation as TFileSystemListOperation;
  try
    ListOperation.Execute;
    Result := ListOperation.ReleaseFiles;

  finally
    FreeAndNil(ListOperation);
  end;
end;

function TFileSystemFileSource.CreateListOperation: TFileSourceOperation;
var
  TargetFileSource: TFileSystemFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TFileSystemListOperation.Create(TargetFileSource);
end;

function TFileSystemFileSource.CreateCopyInOperation(var SourceFileSource: TFileSource;
                                                     var SourceFiles: TFiles;
                                                     TargetPath: String;
                                                     RenameMask: String): TFileSourceOperation;
var
  TargetFileSource: TFileSystemFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TFileSystemCopyInOperation.Create(
                SourceFileSource, TargetFileSource,
                SourceFiles, TargetPath, RenameMask);
end;

function TFileSystemFileSource.CreateCopyOutOperation(var TargetFileSource: TFileSource;
                                                      var SourceFiles: TFiles;
                                                      TargetPath: String;
                                                      RenameMask: String): TFileSourceOperation;
var
  SourceFileSource: TFileSystemFileSource;
begin
  SourceFileSource := Self.Clone;
  Result := TFileSystemCopyOutOperation.Create(
                SourceFileSource, TargetFileSource,
                SourceFiles, TargetPath, RenameMask);
end;

function TFileSystemFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
var
  TargetFileSource: TFileSystemFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TFileSystemDeleteOperation.Create(TargetFileSource, FilesToDelete);
end;

end.

