unit uWinNetListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSystemListOperation,
  uWinNetFileSource,
  uFileSource;

type

  { TWinNetListOperation }

  TWinNetListOperation = class(TFileSystemListOperation)
  private
    FWinNetFileSource: IWinNetFileSource;
  private
    procedure ShareEnum;
    procedure ShellEnum;
    procedure WorkgroupEnum;
    function Linux: Boolean;
    function Connect: Boolean;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  LazUTF8, uFile, Windows, JwaWinNetWk, JwaLmCons, JwaLmShare, JwaLmApiBuf,
  StrUtils, DCStrUtils, uShowMsg, DCOSUtils, uOSUtils, uNetworkThread, uMyWindows,
  ShlObj, ComObj, uShellFolder, uShlObjAdditional;

function TWinNetListOperation.Linux: Boolean;
var
  APath: String;
begin
  Result:= CheckWin32Version(10);
  if Result then begin
    APath:= LowerCase(Path);
    Result:= StrBegins(APath, '\\wsl$\') or StrBegins(APath, '\\wsl.localhost\');
  end;
end;

function TWinNetListOperation.Connect: Boolean;
var
  dwResult: DWORD;
  ServerPath: UnicodeString;
  AbortMethod: TThreadMethod;
begin
  if GetCurrentThreadId = MainThreadID then
    AbortMethod:= nil
  else begin
    AbortMethod:= @CheckOperationState;
  end;
  if FWinNetFileSource.IsNetworkPath(Path) then
    ServerPath:= UTF8Decode(ExcludeTrailingPathDelimiter(Path))
  else begin
    dwResult:= NPos(PathDelim, Path, 4);
    if dwResult = 0 then dwResult:= MaxInt;
    ServerPath:= UTF8Decode(Copy(Path, 1, dwResult - 1));
  end;
  dwResult:= TNetworkThread.Connect(nil, PWideChar(ServerPath), RESOURCETYPE_ANY, AbortMethod);
  if dwResult <> NO_ERROR then
  begin
    if dwResult = ERROR_CANCELLED then RaiseAbortOperation;
    msgError(Thread, mbWinNetErrorMessage(dwResult));
    Exit(False);
  end;
  Result:= True;
end;

procedure TWinNetListOperation.WorkgroupEnum;
var
  I: DWORD;
  aFile: TFile;
  nFile: TNetResourceW;
  nFileList: PNetResourceW;
  dwResult: DWORD;
  dwCount, dwBufferSize: DWORD;
  hEnum: THandle = INVALID_HANDLE_VALUE;
  lpBuffer: Pointer = nil;
  FilePath: String;
  FileName: UnicodeString;
begin
  with FWinNetFileSource do
  try
    ZeroMemory(@nFile, SizeOf(TNetResourceW));
    nFile.dwScope:= RESOURCE_GLOBALNET;
    nFile.dwType:= RESOURCETYPE_ANY;
    nFile.lpProvider:= PWideChar(ProviderName);

    if not IsPathAtRoot(Path) then
    begin
      FilePath:= ExcludeTrailingPathDelimiter(Path);
      FileName:= UTF8Decode(ExcludeFrontPathDelimiter(FilePath));
      nFile.lpRemoteName:= PWideChar(FileName);
    end;

    dwResult := WNetOpenEnumW(RESOURCE_GLOBALNET, RESOURCETYPE_ANY, 0, @nFile, hEnum);
    if (dwResult <> NO_ERROR) then Exit;
    dwCount := DWORD(-1);
    // 1024 Kb must be enough
    dwBufferSize:= $100000;
    // Allocate output buffer
    GetMem(lpBuffer, dwBufferSize);
    // Enumerate all resources
    dwResult:= WNetEnumResourceW(hEnum, dwCount, lpBuffer, dwBufferSize);
    if dwResult = ERROR_NO_MORE_ITEMS then Exit;
    if (dwResult <> NO_ERROR) then Exit;
    nFileList:= PNetResourceW(lpBuffer);
    for I := 0 to dwCount - 1 do
    begin
      CheckOperationState;
      aFile:= TWinNetFileSource.CreateFile(Path);
      aFile.FullPath:= UTF16ToUTF8(UnicodeString(nFileList^.lpRemoteName));
      aFile.CommentProperty.Value:= UTF16ToUTF8(UnicodeString(nFileList^.lpComment));
      if nFileList^.dwDisplayType = RESOURCEDISPLAYTYPE_SHARE then
        aFile.Attributes:= faFolder;
      FFiles.Add(aFile);
      Inc(nFileList);
    end;

  finally
    if (hEnum <> INVALID_HANDLE_VALUE) then
      dwResult := WNetCloseEnum(hEnum);
    if (dwResult <> NO_ERROR) and (dwResult <> ERROR_NO_MORE_ITEMS) then
      msgError(Thread, mbWinNetErrorMessage(dwResult));
    if Assigned(lpBuffer) then
      FreeMem(lpBuffer);
  end;
end;

procedure TWinNetListOperation.ShareEnum;
var
  I: DWORD;
  aFile: TFile;
  dwResult: NET_API_STATUS;
  dwEntriesRead: DWORD = 0;
  dwTotalEntries: DWORD = 0;
  ServerPath: UnicodeString;
  BufPtr, nFileList: PShareInfo1;
begin
  if not Connect then Exit;

  ServerPath:= UTF8Decode(ExcludeTrailingPathDelimiter(Path));

  BufPtr:= nil;
  repeat
    // Call the NetShareEnum function
    dwResult:= NetShareEnum (PWideChar(ServerPath), 1, PByte(BufPtr), MAX_PREFERRED_LENGTH, @dwEntriesRead, @dwTotalEntries, nil);
    // If the call succeeds
    if (dwResult = ERROR_SUCCESS) or (dwResult = ERROR_MORE_DATA) then
    begin
      nFileList:= BufPtr;
      // Loop through the entries
      for I:= 1 to dwEntriesRead do
      begin
        CheckOperationState;
        aFile:= TWinNetFileSource.CreateFile(Path);
        aFile.Name:= UTF16ToUTF8(UnicodeString(nFileList^.shi1_netname));
        aFile.CommentProperty.Value:= UTF16ToUTF8(UnicodeString(nFileList^.shi1_remark));
        case (nFileList^.shi1_type and $FF) of
          STYPE_DISKTREE:
            aFile.Attributes:= FILE_ATTRIBUTE_DIRECTORY;
          STYPE_IPC:
            aFile.Attributes:= FILE_ATTRIBUTE_SYSTEM;
        end;
        // Mark special items as hidden
        if (nFileList^.shi1_type and STYPE_SPECIAL = STYPE_SPECIAL) then
          aFile.Attributes:= aFile.Attributes or FILE_ATTRIBUTE_HIDDEN;
        // Mark special items as hidden
        if (lstrcmpiW(nFileList^.shi1_netname, 'FAX$') = 0) then
          aFile.Attributes:= aFile.Attributes or FILE_ATTRIBUTE_HIDDEN;
        // Mark special items as hidden
        if (lstrcmpiW(nFileList^.shi1_netname, 'PRINT$') = 0) then
          aFile.Attributes:= aFile.Attributes or FILE_ATTRIBUTE_HIDDEN;
        FFiles.Add(aFile);
        Inc(nFileList);
      end;
      // Free the allocated buffer
      NetApiBufferFree(BufPtr);
    end;

  // Continue to call NetShareEnum while there are more entries
  until (dwResult <> ERROR_MORE_DATA);

  // Show error if failed
  if (dwResult <> ERROR_SUCCESS) then
    msgError(Thread, mbSysErrorMessage(dwResult));
end;

procedure TWinNetListOperation.ShellEnum;
var
  AFile: TFile;
  NumIDs: LongWord = 0;
  AFolder: IShellFolder;
  EnumIDList: IEnumIDList;
  DesktopFolder: IShellFolder;
  PIDL, NetworkPIDL: PItemIDList;
begin
  try
    OleCheckUTF8(SHGetDesktopFolder(DesktopFolder));
    OleCheckUTF8(SHGetFolderLocation(0, CSIDL_NETWORK, 0, 0, {%H-}NetworkPIDL));
    OleCheckUTF8(DesktopFolder.BindToObject(NetworkPIDL, nil, IID_IShellFolder, Pointer(AFolder)));
    OleCheckUTF8(AFolder.EnumObjects(0, SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN, EnumIDList));

    while EnumIDList.Next(1, PIDL, NumIDs) = S_OK do
    begin
      CheckOperationState;

      aFile:= TWinNetFileSource.CreateFile(Path);
      AFile.FullPath:= GetDisplayName(AFolder, PIDL, SHGDN_FORPARSING or SHGDN_FORADDRESSBAR);

      FFiles.Add(AFile);
    end;
  except
    on E: Exception do msgError(Thread, E.Message);
  end;
end;

constructor TWinNetListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FWinNetFileSource := aFileSource as IWinNetFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TWinNetListOperation.MainExecute;
begin
  FFiles.Clear;
  with FWinNetFileSource do
  begin
    // Shared directory
    if not IsNetworkPath(Path) then
    begin
      if Linux or Connect then
        inherited MainExecute;
    end
    else begin
      // Workstation/Server
      if (IsPathAtRoot(Path) = False) and (Pos('\\', Path) = 1) then
        ShareEnum
      // Root/Domain/Workgroup
      else if not Samba1 then
        ShellEnum
      else
        WorkgroupEnum;
    end;
  end;
end;

end.

