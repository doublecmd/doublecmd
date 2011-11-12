unit uWinNetFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  uFileSourceProperty, uFileSourceOperationTypes,
  uVirtualFileSource, uFileSystemFileSource, uFileProperty, uFileSource,
  uFileSourceOperation, uFile;

type
  { IWinNetFileSource }

  IWinNetFileSource = interface(IVirtualFileSource)
    ['{55329161-3CFC-4F15-B66D-6649B42E9357}']

    function GetProviderName: WideString;

    property ProviderName: WideString read GetProviderName;
  end;

  { TWinNetFileSource }

  TWinNetFileSource = class(TFileSystemFileSource, IWinNetFileSource)
  private
    FProviderName: array[0..MAX_PATH-1] of WideChar;
    function GetProviderName: WideString;
    function IsNetworkPath(const Path: UTF8String): Boolean;

  protected
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

  public
    constructor Create; override;

    class function IsSupportedPath(const Path: String): Boolean; override;

    function GetParentDir(sPath : String): String; override;
    function IsPathAtRoot(Path: String): Boolean; override;
    function GetRootDir(sPath: String): String; override; overload;
    function GetRootDir: String; override; overload;

    // Retrieve some properties of the file source.
    function GetProperties: TFileSourceProperties; override;

    // These functions create an operation object specific to the file source.
    function CreateListOperation(TargetPath: String): TFileSourceOperation; override;
    function CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation; override;

  end;

implementation

uses
  LCLProc, uWinNetListOperation, uWinNetExecuteOperation,
  Windows, JwaWinNetWk, uVfsModule, uShowMsg, uOSUtils, uDCUtils;

function TWinNetFileSource.GetParentDir(sPath: String): String;
var
  nFile: TNetResourceW;
  lpBuffer: array [0..4095] of Byte;
  ParentPath: TNetResourceW absolute lpBuffer;
  dwBufferSize: DWORD;
  dwResult: DWORD;
  FilePath: WideString;
begin
  Result:= GetRootDir;
  if Pos('\\', sPath) = 1 then
  begin
    FilePath:= UTF8Decode(ExcludeTrailingPathDelimiter(sPath));
    FillByte(nFile, SizeOf(TNetResourceW), 0);
    with nFile do
    begin
      dwScope := RESOURCE_GLOBALNET;
      dwType := RESOURCETYPE_DISK;
      dwDisplayType := RESOURCEDISPLAYTYPE_SERVER;
      dwUsage := RESOURCEUSAGE_CONTAINER;
      lpRemoteName := PWideChar(FilePath);
      lpProvider := @FProviderName;
    end;
    dwBufferSize:= SizeOf(lpBuffer);
    dwResult := WNetGetResourceParentW(nFile, @lpBuffer, dwBufferSize);
    if dwResult <> NO_ERROR then
      msgError(mbSysErrorMessage(GetLastError))
    else
      begin
        FilePath:= WideString(ParentPath.lpRemoteName);
        Result := IncludeFrontPathDelimiter(UTF8Encode(FilePath));
      end;
  end;
end;

function TWinNetFileSource.IsPathAtRoot(Path: String): Boolean;
begin
  Result := (uDCUtils.GetParentDir(Path) = '');
end;

function TWinNetFileSource.GetRootDir(sPath: String): String;
begin
  Result:= PathDelim;
end;

function TWinNetFileSource.GetRootDir: String;
begin
  Result:= PathDelim;
end;

function TWinNetFileSource.GetProperties: TFileSourceProperties;
begin
  Result := inherited GetProperties + [fspVirtual];
end;

function TWinNetFileSource.GetProviderName: WideString;
begin
  Result:= WideString(FProviderName);
end;

function TWinNetFileSource.IsNetworkPath(const Path: UTF8String): Boolean;
begin
  Result:= (NumCountChars(PathDelim, ExcludeTrailingPathDelimiter(Path)) < 3);
end;

function TWinNetFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  if IsNetworkPath(NewDir) then
    Result:= True
  else
    Result:= inherited SetCurrentWorkingDirectory(NewDir);
end;

constructor TWinNetFileSource.Create;
var
  dwBufferSize: DWORD;
begin
  inherited Create;
  dwBufferSize:= MAX_PATH;
  if WNetGetProviderNameW(WNNC_NET_LANMAN, @FProviderName, dwBufferSize) <> NO_ERROR then
    RaiseLastOSError;
end;

class function TWinNetFileSource.IsSupportedPath(const Path: String): Boolean;
begin
  Result:= (Pos('\\', Path) = 1);
end;

function TWinNetFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  if IsNetworkPath(TargetPath) then
    Result:= TWinNetListOperation.Create(TargetFileSource, TargetPath)
  else
    Result:= inherited CreateListOperation(TargetPath);
end;

function TWinNetFileSource.CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  if IsNetworkPath(BasePath) then
    Result:= TWinNetExecuteOperation.Create(TargetFileSource, ExecutableFile, BasePath, Verb)
  else
    Result:= inherited CreateExecuteOperation(ExecutableFile, BasePath, Verb);
end;

end.


