unit uFileSystemFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uLocalFileSource,
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

    class function GetSupportedFileProperties: TFilePropertiesTypes; override;
    class function GetOperationsTypes: TFileSourceOperationTypes; override;
    class function GetFilePropertiesDescriptions: TFilePropertiesDescriptions; override;
    class function GetProperties: TFileSourceProperties; override;

    function GetOperation(OperationType: TFileSourceOperationType): TFileSourceOperation; override;
    function GetFiles: TFiles; override;

    // ------------------------------------------------------
  end;

implementation

uses
  uFileSystemListOperation, uOSUtils, uFileSystemFile;

constructor TFileSystemFileSource.Create;
begin
  Create(mbGetCurrentDir);
end;

constructor TFileSystemFileSource.Create(Path: String);
begin
  inherited Create;
  FCurrentPath := Path;
  FCurrentAddress := '';
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
    NewPath := mbGetCurrentDir;

  FCurrentPath := NewPath;
end;

class function TFileSystemFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := TFileSystemFile.GetSupportedProperties;
end;

function TFileSystemFileSource.GetOperation(OperationType: TFileSourceOperationType): TFileSourceOperation;
begin
  Result := nil;
  case OperationType of
    fsoList:
      Result := TFileSystemListOperation.Create(Self);
  end;
end;

function TFileSystemFileSource.GetFiles: TFiles;
var
  ListOperation: TFileSystemListOperation;
begin
  ListOperation := TFileSystemListOperation.Create(Self);

  try
    ListOperation.Execute;
    Result := ListOperation.ReleaseFiles;

  finally
    FreeAndNil(ListOperation);
  end;
end;

end.

