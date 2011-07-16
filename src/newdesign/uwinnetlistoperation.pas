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
  LCLProc, uFile, uGlobs, Windows, JwaWinNetWk, uDCUtils;

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
  hEnum: THandle;
  lpBuffer: Pointer = nil;
  FilePath: UTF8String;
  FileName: WideString;
begin
  FFiles.Clear;
  with FWinNetFileSource do
  begin
    FillChar(nFile, SizeOf(TNetResource), #0);
    nFile.dwScope:= RESOURCE_GLOBALNET;
    nFile.dwType:= RESOURCETYPE_ANY;
    nFile.lpProvider:= PWideChar(ProviderName);

    if not IsPathAtRoot(Path) then
    begin
      FilePath:= ExcludeTrailingPathDelimiter(Path);
      FileName:= UTF8Decode(ExtractFileName(FilePath));
      case NumCountChars(PathDelim, FilePath) of
        2: // Workgroup/Domen
          nFile.lpRemoteName:= PWideChar(FileName);
        3: // Workstation/Server
          nFile.lpRemoteName:= PWideChar('\\' + FileName);
      end;
    end;

    try
      dwResult := WNetOpenEnumW(RESOURCE_GLOBALNET, RESOURCETYPE_ANY, 0, @nFile, hEnum);
      if (dwResult <> NO_ERROR) then RaiseLastOSError;
      dwBufferSize := 0;
      dwCount := DWORD(-1);
      // Query buffer size for all resources
      dwResult:= WNetEnumResource(hEnum, dwCount, @nFile, dwBufferSize);
      if (dwResult <> ERROR_MORE_DATA) then RaiseLastOSError;
      dwCount := DWORD(-1);
      // Allocate output buffer
      GetMem(lpBuffer, dwBufferSize);
      // Enumerate all resources
      dwResult:= WNetEnumResource(hEnum, dwCount, lpBuffer, dwBufferSize);
      if dwResult = ERROR_NO_MORE_ITEMS then Exit;
      if (dwResult <> NO_ERROR) then RaiseLastOSError;
      nFileList:= PNetResourceArray(lpBuffer);
      for I := 0 to dwCount - 1 do
      begin
        aFile := TWinNetFileSource.CreateFile(Path);
        aFile.FullPath:= UTF8Encode(WideString(nFileList^.lpRemoteName));
        if nFileList^.dwDisplayType in [RESOURCEDISPLAYTYPE_DOMAIN, RESOURCEDISPLAYTYPE_GROUP, RESOURCEDISPLAYTYPE_SERVER] then
          aFile.Attributes:= FILE_ATTRIBUTE_DIRECTORY;
        FFiles.Add(aFile);
        Inc(nFileList);
      end;

      dwResult := WNetCloseEnum(hEnum);
      if dwResult <> NO_ERROR then RaiseLastOSError;

    finally
      if Assigned(lpBuffer) then
        FreeMem(lpBuffer);
    end;
  end;
end;

end.

