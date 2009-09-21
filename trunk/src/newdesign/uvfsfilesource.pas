unit uVfsFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uWFXModule,
  uFile, uFileSourceProperty, uFileSourceOperationTypes,
  uVirtualFileSource, uFileProperty, uFileSource, uFileSourceOperation;

type

  TVfsFileSource = class(TVirtualFileSource)
  private
    FWFXModuleList: TWFXModuleList;

  protected
    class function GetSupportedFileProperties: TFilePropertiesTypes; override;

  public
    constructor Create(aWFXModuleList: TWFXModuleList); reintroduce;
    destructor Destroy; override;

    function Clone: TVfsFileSource; override;
    procedure CloneTo(FileSource: TFileSource); override;

    // Retrieve operations permitted on the source.  = capabilities?
    class function GetOperationsTypes: TFileSourceOperationTypes; override;

    // Returns a list of property types supported by this source for each file.
    class function GetFilePropertiesDescriptions: TFilePropertiesDescriptions; override;

    // Retrieve some properties of the file source.
    class function GetProperties: TFileSourceProperties; override;

    // These functions create an operation object specific to the file source.
    // Each parameter will be owned by the operation (will be freed).
    function CreateListOperation: TFileSourceOperation; override;

    property VfsFileList: TWFXModuleList read FWFXModuleList;

  end;


implementation

uses
  LCLProc, uGlobs,
  uVfsListOperation;

constructor TVfsFileSource.Create(aWFXModuleList: TWFXModuleList);
begin
  inherited Create;
  FWFXModuleList:= TWFXModuleList.Create;
  FWFXModuleList.Assign(aWFXModuleList);
end;

destructor TVfsFileSource.Destroy;
begin
  FreeThenNil(FWFXModuleList);
  inherited Destroy;
end;

function TVfsFileSource.Clone: TVfsFileSource;
begin
  Result := TVfsFileSource.Create(FWFXModuleList);
  CloneTo(Result);
end;

procedure TVfsFileSource.CloneTo(FileSource: TFileSource);
begin
  if Assigned(FileSource) then
  begin
    inherited CloneTo(FileSource);
  end;
end;

class function TVfsFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList];
end;

class function TVfsFileSource.GetFilePropertiesDescriptions: TFilePropertiesDescriptions;
begin
  Result := nil;
end;

class function TVfsFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [fspVirtual];
end;

class function TVfsFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := [];
end;

function TVfsFileSource.CreateListOperation: TFileSourceOperation;
var
  TargetFileSource: TFileSource;
begin
  TargetFileSource := Self.Clone;
  Result := TVfsListOperation.Create(TargetFileSource);
end;

end.

