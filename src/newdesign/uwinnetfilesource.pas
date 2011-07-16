unit uWinNetFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  uFileSourceProperty, uFileSourceOperationTypes,
  uVirtualFileSource, uFileProperty, uFileSource,
  uFileSourceOperation, uFile;

type
  { IWinNetFileSource }

  IWinNetFileSource = interface(IVirtualFileSource)
    ['{55329161-3CFC-4F15-B66D-6649B42E9357}']

    function GetProviderName: WideString;

    property ProviderName: WideString read GetProviderName;
  end;

  { TWinNetFileSource }

  TWinNetFileSource = class(TVirtualFileSource, IWinNetFileSource)
  private
    FProviderName: array[0..MAX_PATH-1] of WideChar;
    function GetProviderName: WideString;
  protected
    function GetSupportedFileProperties: TFilePropertiesTypes; override;

  public
    constructor Create; override;

    function IsPathAtRoot(Path: String): Boolean; override;
    function GetRootDir(sPath : String): String; override;

    class function CreateFile(const APath: String): TFile; override;

    // Retrieve operations permitted on the source.  = capabilities?
    function GetOperationsTypes: TFileSourceOperationTypes; override;

    // Retrieve some properties of the file source.
    function GetProperties: TFileSourceProperties; override;

    // These functions create an operation object specific to the file source.
    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation; override;

  end;

implementation

uses
  LCLProc, uWinNetListOperation, uWinNetExecuteOperation,
  Windows, JwaWinNetWk, uVfsModule;

function TWinNetFileSource.IsPathAtRoot(Path: String): Boolean;
begin
  Result:= (Path = GetRootDir)
end;

function TWinNetFileSource.GetRootDir(sPath: String): String;
begin
  Result:= '\\';
end;

class function TWinNetFileSource.CreateFile(const APath: String): TFile;
begin
  Result := TFile.Create(APath);

  with Result do
  begin
    SizeProperty := TFileSizeProperty.Create;
    AttributesProperty := TNtfsFileAttributesProperty.Create;
    ModificationTimeProperty := TFileModificationDateTimeProperty.Create;
  end;
end;

function TWinNetFileSource.GetOperationsTypes: TFileSourceOperationTypes;
begin
  Result := [fsoList, fsoExecute];
end;

function TWinNetFileSource.GetProperties: TFileSourceProperties;
begin
  Result := [fspVirtual];
end;

function TWinNetFileSource.GetProviderName: WideString;
begin
  Result:= WideString(FProviderName);
end;

function TWinNetFileSource.GetSupportedFileProperties: TFilePropertiesTypes;
begin
  Result := inherited GetSupportedFileProperties +
            [fpSize, fpAttributes, fpModificationTime];
end;

constructor TWinNetFileSource.Create;
var
  lpBufferSize: DWORD;
begin
  inherited Create;
  lpBufferSize:= MAX_PATH;
  if WNetGetProviderNameW(WNNC_NET_LANMAN, @FProviderName, lpBufferSize) <> NO_ERROR then
    RaiseLastOSError;
end;

function TWinNetFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result := TWinNetListOperation.Create(TargetFileSource, TargetPath);
end;

function TWinNetFileSource.CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:=  TWinNetExecuteOperation.Create(TargetFileSource, ExecutableFile, BasePath, Verb);
end;

end.


