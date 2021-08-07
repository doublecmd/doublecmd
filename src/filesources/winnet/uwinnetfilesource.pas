unit uWinNetFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  uFileSourceProperty,
  uVirtualFileSource, uFileSystemFileSource, uFileProperty, uFileSource,
  uFileSourceOperation, uFile;

type
  { IWinNetFileSource }

  IWinNetFileSource = interface(IVirtualFileSource)
    ['{55329161-3CFC-4F15-B66D-6649B42E9357}']

    function GetSamba1: Boolean;
    function GetProviderName: UnicodeString;

    function IsNetworkPath(const Path: String): Boolean;

    property Samba1: Boolean read GetSamba1;
    property ProviderName: UnicodeString read GetProviderName;
  end;

  { TWinNetFileSource }

  TWinNetFileSource = class(TFileSystemFileSource, IWinNetFileSource)
  private
    FSamba1: Boolean;
    FProviderName: array[0..MAX_PATH-1] of WideChar;
    function GetProviderName: UnicodeString;
    function GetSamba1: Boolean;

  protected
    function IsNetworkPath(const Path: String): Boolean;
    function SetCurrentWorkingDirectory(NewDir: String): Boolean; override;

  public
    constructor Create; override;

    class function IsSupportedPath(const Path: String): Boolean; override;
    class function GetMainIcon(out Path: String): Boolean; override;

    function GetParentDir(sPath : String): String; override;
    function IsPathAtRoot(Path: String): Boolean; override;
    function GetRootDir(sPath: String): String; override; overload;
    function GetRootDir: String; override; overload;

    function GetFreeSpace(Path: String; out FreeSize, TotalSize : Int64) : Boolean; override;

    // Retrieve some properties of the file source.
    function GetProperties: TFileSourceProperties; override;

    // These functions create an operation object specific to the file source.
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
    function CreateSplitOperation(var aSourceFile: TFile;
                                  aTargetPath: String): TFileSourceOperation; override;
    function CreateCombineOperation(var SourceFiles: TFiles;
                                    aTargetFile: String): TFileSourceOperation; override;
    function CreateCreateDirectoryOperation(BasePath: String; DirectoryPath: String): TFileSourceOperation; override;
    function CreateExecuteOperation(var ExecutableFile: TFile; BasePath, Verb: String): TFileSourceOperation; override;
    function CreateCalcChecksumOperation(var theFiles: TFiles;
                                         aTargetPath: String;
                                         aTargetMask: String): TFileSourceOperation; override;
    function CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation; override;
    function CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                            var theNewProperties: TFileProperties): TFileSourceOperation; override;

  end;

implementation

uses
  LazUTF8, uWinNetListOperation, uWinNetExecuteOperation, uMyWindows,
  Windows, JwaWinNetWk, uVfsModule, uShowMsg, DCOSUtils, DCStrUtils;

function TWinNetFileSource.GetParentDir(sPath: String): String;
var
  nFile: TNetResourceW;
  lpBuffer: array [0..4095] of Byte;
  ParentPath: TNetResourceW absolute lpBuffer;
  dwBufferSize: DWORD;
  dwResult: DWORD;
  FilePath: UnicodeString;
begin
  Result:= GetRootDir;
  if Pos('\\', sPath) = 1 then
  begin
    if not FSamba1 then
    begin
      if IsNetworkPath(sPath) then
        Result:= ExcludeFrontPathDelimiter(DCStrUtils.GetParentDir(sPath))
      else begin
        Result:= DCStrUtils.GetParentDir(sPath);
      end;
      Exit;
    end;
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
      msgError(mbWinNetErrorMessage(GetLastError))
    else
      begin
        FilePath:= UnicodeString(ParentPath.lpRemoteName);
        Result := IncludeFrontPathDelimiter(UTF16ToUTF8(FilePath));
        Result := IncludeTrailingPathDelimiter(Result);
      end;
  end;
end;

function TWinNetFileSource.IsPathAtRoot(Path: String): Boolean;
begin
  Result := (DCStrUtils.GetParentDir(Path) = '');
end;

function TWinNetFileSource.GetRootDir(sPath: String): String;
begin
  Result:= PathDelim;
end;

function TWinNetFileSource.GetRootDir: String;
begin
  Result:= PathDelim;
end;

function TWinNetFileSource.GetFreeSpace(Path: String; out FreeSize,
                                        TotalSize: Int64): Boolean;
begin
  if IsNetworkPath(Path) then
    Result:= False
  else
    Result:= inherited GetFreeSpace(Path, FreeSize, TotalSize);
end;

function TWinNetFileSource.GetProperties: TFileSourceProperties;
begin
  Result := inherited GetProperties + [fspVirtual] - [fspNoneParent];
end;

function TWinNetFileSource.GetProviderName: UnicodeString;
begin
  Result:= UnicodeString(FProviderName);
end;

function TWinNetFileSource.GetSamba1: Boolean;
begin
  Result:= FSamba1;
end;

function TWinNetFileSource.IsNetworkPath(const Path: String): Boolean;
begin
  Result:= (NumCountChars(PathDelim, ExcludeTrailingPathDelimiter(Path)) < 3);
end;

function TWinNetFileSource.SetCurrentWorkingDirectory(NewDir: String): Boolean;
begin
  if IsNetworkPath(NewDir) then
    Result:= True
  else
    Result:= mbSetCurrentDir(NewDir);
end;

constructor TWinNetFileSource.Create;
var
  dwBufferSize: DWORD = MAX_PATH;
begin
  inherited Create;

  if WNetGetProviderNameW(WNNC_NET_LANMAN, @FProviderName, dwBufferSize) <> NO_ERROR then
    raise EOSError.Create(mbWinNetErrorMessage(GetLastError));

  FSamba1:= (Win32MajorVersion < 6) or (GetServiceStatus('mrxsmb10') = SERVICE_RUNNING);
end;

class function TWinNetFileSource.IsSupportedPath(const Path: String): Boolean;
begin
  Result:= (Pos('\\', Path) = 1);
end;

class function TWinNetFileSource.GetMainIcon(out Path: String): Boolean;
begin
  Result:= True;
  Path:= '%SystemRoot%\System32\shell32.dll,17';
end;

function TWinNetFileSource.CreateListOperation(TargetPath: String): TFileSourceOperation;
var
  TargetFileSource: IFileSource;
begin
  TargetFileSource := Self;
  Result:= TWinNetListOperation.Create(TargetFileSource, TargetPath);
end;

function TWinNetFileSource.CreateCopyOperation(var SourceFiles: TFiles;
  TargetPath: String): TFileSourceOperation;
begin
  if IsNetworkPath(TargetPath) then
    Result:= nil
  else
    Result:= inherited CreateCopyOperation(SourceFiles, TargetPath);
end;

function TWinNetFileSource.CreateCopyInOperation(SourceFileSource: IFileSource;
  var SourceFiles: TFiles; TargetPath: String): TFileSourceOperation;
begin
  if IsNetworkPath(TargetPath) then
    Result:= nil
  else
    Result:=inherited CreateCopyInOperation(SourceFileSource, SourceFiles, TargetPath);
end;

function TWinNetFileSource.CreateCopyOutOperation(TargetFileSource: IFileSource;
                                                  var SourceFiles: TFiles;
                                                  TargetPath: String): TFileSourceOperation;
begin
  if IsNetworkPath(SourceFiles.Path) then
    Result:= nil
  else
    Result:= inherited CreateCopyOutOperation(TargetFileSource, SourceFiles, TargetPath);
end;

function TWinNetFileSource.CreateMoveOperation(var SourceFiles: TFiles;
  TargetPath: String): TFileSourceOperation;
begin
  if IsNetworkPath(SourceFiles.Path) then
    Result:= nil
  else
    Result:= inherited CreateMoveOperation(SourceFiles, TargetPath);
end;

function TWinNetFileSource.CreateDeleteOperation(var FilesToDelete: TFiles): TFileSourceOperation;
begin
  if IsNetworkPath(FilesToDelete.Path) then
    Result:= nil
  else
    Result:= inherited CreateDeleteOperation(FilesToDelete);
end;

function TWinNetFileSource.CreateWipeOperation(var FilesToWipe: TFiles): TFileSourceOperation;
begin
  if IsNetworkPath(FilesToWipe.Path) then
    Result:= nil
  else
    Result:= inherited CreateWipeOperation(FilesToWipe);
end;

function TWinNetFileSource.CreateSplitOperation(var aSourceFile: TFile;
  aTargetPath: String): TFileSourceOperation;
begin
  if IsNetworkPath(aSourceFile.Path) then
    Result:= nil
  else
    Result:= inherited CreateSplitOperation(aSourceFile, aTargetPath);
end;

function TWinNetFileSource.CreateCombineOperation(var SourceFiles: TFiles;
  aTargetFile: String): TFileSourceOperation;
begin
  if IsNetworkPath(SourceFiles.Path) then
    Result:= nil
  else
    Result:= inherited CreateCombineOperation(SourceFiles, aTargetFile);
end;

function TWinNetFileSource.CreateCreateDirectoryOperation(BasePath: String;
  DirectoryPath: String): TFileSourceOperation;
begin
  if IsNetworkPath(BasePath) then
    Result:= nil
  else
    Result:= inherited CreateCreateDirectoryOperation(BasePath, DirectoryPath);
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

function TWinNetFileSource.CreateCalcChecksumOperation(var theFiles: TFiles;
  aTargetPath: String; aTargetMask: String): TFileSourceOperation;
begin
  if IsNetworkPath(theFiles.Path) then
    Result:= nil
  else
    Result:= inherited CreateCalcChecksumOperation(theFiles, aTargetPath, aTargetMask);
end;

function TWinNetFileSource.CreateCalcStatisticsOperation(var theFiles: TFiles): TFileSourceOperation;
begin
  if (NumCountChars(PathDelim, ExcludeTrailingPathDelimiter(theFiles.Path)) < 2) then
    Result:= nil
  else
    Result:= inherited CreateCalcStatisticsOperation(theFiles);
end;

function TWinNetFileSource.CreateSetFilePropertyOperation(var theTargetFiles: TFiles;
                                                          var theNewProperties: TFileProperties
                                                          ): TFileSourceOperation;
begin
  if IsNetworkPath(theTargetFiles.Path) then
    Result:= nil
  else
    Result:= inherited CreateSetFilePropertyOperation(theTargetFiles, theNewProperties);
end;

end.

