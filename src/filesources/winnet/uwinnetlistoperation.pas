unit uWinNetListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uWinNetFileSource,
  uFileSource;

type

  { TWinNetListOperation }

  TWinNetListOperation = class(TFileSourceListOperation)
  private
    FWinNetFileSource: IWinNetFileSource;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  LCLProc, uFile, Windows, JwaWinNetWk, DCStrUtils, uShowMsg,
  DCOSUtils, uOSUtils;

type
  PNetResourceArray = ^TNetResource;

constructor TWinNetListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FWinNetFileSource := aFileSource as IWinNetFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TWinNetListOperation.MainExecute;
var
  I: DWORD;
  aFile: TFile;
  nFile: TNetResourceW;
  nFileList: PNetResourceArray;
  dwResult: DWORD;
  dwCount, dwBufferSize: DWORD;
  hEnum: THandle = INVALID_HANDLE_VALUE;
  lpBuffer: Pointer = nil;
  FilePath: UTF8String;
  FileName: WideString;
begin
  FFiles.Clear;
  with FWinNetFileSource do
  try
    FillChar(nFile, SizeOf(TNetResource), #0);
    nFile.dwScope:= RESOURCE_GLOBALNET;
    nFile.dwType:= RESOURCETYPE_ANY;
    nFile.lpProvider:= PWideChar(ProviderName);

    if not IsPathAtRoot(Path) then
    begin
      FilePath:= ExcludeTrailingPathDelimiter(Path);
      // Workstation/Server
      if Pos('\\', FilePath) = 1 then
        begin
          FileName:= UTF8Decode(FilePath);
          nFile.lpRemoteName:= PWideChar(FileName);
        end
      else // Domain/Workgroup
        begin
          FileName:= UTF8Decode(ExcludeFrontPathDelimiter(FilePath));
          nFile.lpRemoteName:= PWideChar(FileName);
        end;
    end;

    dwResult := WNetOpenEnumW(RESOURCE_GLOBALNET, RESOURCETYPE_ANY, 0, @nFile, hEnum);
    if (dwResult <> NO_ERROR) then Exit;
    dwCount := DWORD(-1);
    // 512 Kb must be enough
    dwBufferSize:= $80000;
    // Allocate output buffer
    GetMem(lpBuffer, dwBufferSize);
    // Enumerate all resources
    dwResult:= WNetEnumResource(hEnum, dwCount, lpBuffer, dwBufferSize);
    if dwResult = ERROR_NO_MORE_ITEMS then Exit;
    if (dwResult <> NO_ERROR) then Exit;
    nFileList:= PNetResourceArray(lpBuffer);
    for I := 0 to dwCount - 1 do
    begin
      CheckOperationState;
      aFile := TWinNetFileSource.CreateFile(Path);
      aFile.FullPath:= UTF8Encode(WideString(nFileList^.lpRemoteName));
      aFile.CommentProperty.Value:= UTF8Encode(WideString(nFileList^.lpComment));
      if nFileList^.dwDisplayType = RESOURCEDISPLAYTYPE_SHARE then
        aFile.Attributes:= faFolder;
      FFiles.Add(aFile);
      Inc(nFileList);
    end;

  finally
    if (hEnum <> INVALID_HANDLE_VALUE) then
      dwResult := WNetCloseEnum(hEnum);
    if (dwResult <> NO_ERROR) and (dwResult <> ERROR_NO_MORE_ITEMS) then
      msgError(Thread, mbSysErrorMessage(dwResult));
    if Assigned(lpBuffer) then
      FreeMem(lpBuffer);
  end;
end;

end.

