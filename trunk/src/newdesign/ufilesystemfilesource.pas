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
    function CreateCopyInOperation(SourceFileSource: TFileSource;
                                   SourceFiles: TFiles;
                                   TargetPath: String;
                                   FileMask: String): TFileSourceOperation; override;
    function CreateCopyOutOperation(TargetFileSource: TFileSource;
                                    SourceFiles: TFiles;
                                    TargetPath: String;
                                    FileMask: String): TFileSourceOperation; override;

    // ------------------------------------------------------
  end;

implementation

uses
  uOSUtils, uFileSystemFile, uFileSystemListOperation, uFileSystemCopyOperation;

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

  ListOperation := TFileSystemListOperation.Create(Self);
  try
    ListOperation.Execute;
    Result := ListOperation.ReleaseFiles;

  finally
    FreeAndNil(ListOperation);
  end;
end;

function TFileSystemFileSource.CreateListOperation: TFileSourceOperation;
begin
  Result := TFileSystemListOperation.Create(Self);
end;

function TFileSystemFileSource.CreateCopyInOperation(SourceFileSource: TFileSource;
                                                     SourceFiles: TFiles;
                                                     TargetPath: String;
                                                     FileMask: String): TFileSourceOperation;
begin
  Result := TFileSystemCopyInOperation.Create(
                SourceFileSource, Self,
                SourceFiles, TargetPath, FileMask);
end;

function TFileSystemFileSource.CreateCopyOutOperation(TargetFileSource: TFileSource;
                                                      SourceFiles: TFiles;
                                                      TargetPath: String;
                                                      FileMask: String): TFileSourceOperation;
begin
  Result := TFileSystemCopyOutOperation.Create(
                Self, TargetFileSource,
                SourceFiles, TargetPath, FileMask);
end;

end.

