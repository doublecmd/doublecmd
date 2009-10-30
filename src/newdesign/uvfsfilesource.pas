unit uVfsFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uWFXModule,
  uFile, uFileSourceProperty, uFileSourceOperationTypes,
  uVirtualFileSource, uFileProperty, uFileSource, uFileSourceOperation;

type

  IVfsFileSource = interface(IVirtualFileSource)
    ['{87D0A3EF-C168-44C1-8B10-3AEC0753846A}']

    function GetWfxModuleList: TWFXModuleList;

    property VfsFileList: TWFXModuleList read GetWfxModuleList;
  end;

  TVfsFileSource = class(TVirtualFileSource, IVfsFileSource)
  private
    FWFXModuleList: TWFXModuleList;

    function GetWfxModuleList: TWFXModuleList;

  protected
    function GetSupportedFileProperties: TFilePropertiesTypes; override;

  public
    constructor Create(aWFXModuleList: TWFXModuleList); reintroduce;
    destructor Destroy; override;

    // Retrieve operations permitted on the source.  = capabilities?
    function GetOperationsTypes: TFileSourceOperationTypes; override;

    // Returns a list of property types supported by this source for each file.
    function GetFilePropertiesDescriptions: TFilePropertiesDescriptions; override;

    // Retrieve some properties of the file source.
    function GetProperties: TFileSourceProperties; override;

    // These functions create an operation object specific to the file source.
    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;

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

function TVfsFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList];
end;

function TVfsFileSource.GetFilePropertiesDescriptions: TFilePropertiesDescriptions;
begin
  Result := nil;
end;

function TVfsFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [fspVirtual];
end;

function TVfsFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := [];
end;

function TVfsFileSource.GetWfxModuleList: TWFXModuleList;
begin
  Result := FWFXModuleList;
end;

function TVfsFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TVfsListOperation.Create(TargetFileSource, TargetPath);
end;

end.

