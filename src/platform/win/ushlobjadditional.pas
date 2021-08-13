(*
   Daniel U. Thibault
   <D.U.Thibault@Bigfoot.com>
   19 August 1999
   Updated 15 September 1999

   Constants, types that have appeared since ShlObj.pas.

   The values marked //MISSING VALUES remain unidentified (two sets).

   Koblov Alexander (Alexx2000@mail.ru)
   15 July 2007
   Add some functions, constants and types for Lazarus compability
*)

unit uShlObjAdditional;

{$mode delphi}

interface

uses
   Windows,
   ShlObj,
   ShellApi,
   ActiveX;

const
   { The operation was canceled by the user }
   HRESULT_ERROR_CANCELLED = HRESULT($800704C7);
   { User canceled the current action }
   COPYENGINE_E_USER_CANCELLED = HRESULT($80270000);

const
  IID_IImageList: TGUID = '{46EB5926-582E-4017-9FDF-E8998DAA0950}';

{ IShellIconOverlay Interface }
{
   Used to return the icon overlay index or its icon index for an IShellFolder object,
   this is always implemented with IShellFolder

   [Member functions]
   IShellIconOverlay::GetOverlayIndex

   Parameters:
     pidl            object to identify icon overlay for.
     pdwIndex        the Overlay Index in the system image list

   IShellIconOverlay::GetOverlayIconIndex
   This method is only used for those who are interested in seeing the real bits
   of the Overlay Icon
   Returns:
      S_OK,  if the index of an Overlay is found
      S_FALSE, if no Overlay exists for this file
      E_FAIL, if pidl is bad
   Parameters:
     pdwIconIndex    the Overlay Icon index in the system image list
}
const
   IID_IShellIconOverlay : TGUID = (
   D1:$7D688A70; D2:$C613; D3:$11D0; D4:($99,$9B,$00,$C0,$4F,$D6,$55,$E1));
   SID_IShellIconOverlay = '{7D688A70-C613-11D0-999B-00C04FD655E1}';

type
   IShellIconOverlay = interface(IUnknown)
      [SID_IShellIconOverlay]
      function GetOverlayIndex(pidl : PItemIDList; var Index : Integer) : HResult; stdcall;
      function GetOverlayIconIndex(pidl : PItemIDList; var IconIndex : Integer) : HResult; stdcall;
   end; { IShellIconOverlay }

{$IF FPC_FULLVERSION < 30200}
   PSHColumnID = ^TSHColumnID;

   IShellFolder2 = interface(IShellFolder)
      ['{93F2F68C-1D1B-11d3-A30E-00C04F79ABD1}']
      function GetDefaultSearchGUID(out guid:TGUID):HResult;StdCall;
      function EnumSearches(out ppenum:IEnumExtraSearch):HResult;StdCall;
      function GetDefaultColumn(dwres:DWORD;psort :pulong; pdisplay:pulong):HResult;StdCall;
      function GetDefaultColumnState(icolumn:UINT;pscflag:PSHCOLSTATEF):HResult;StdCall;
      function GetDetailsEx(pidl:LPCITEMIDLIST;pscid:PSHCOLUMNID; pv : pOLEvariant):HResult;StdCall;
      function GetDetailsOf(pidl:LPCITEMIDLIST;iColumn:UINT;psd:PSHELLDETAILS):HResult;StdCall;
      function MapColumnToSCID(iColumn:UINT;pscid:PSHCOLUMNID):HResult;StdCall;
   end;
{$ENDIF}

const
  SIID_DRIVENET = 9;
  SIID_ZIPFILE = 105;

  PKEY_StorageProviderState: PROPERTYKEY = (fmtid: '{E77E90DF-6271-4F5B-834F-2DD1F245DDA4}'; pid: 3);

type
  TSHStockIconInfo = record
    cbSize: DWORD;
    hIcon: HICON;
    iSysImageIndex: Int32;
    iIcon: Int32;
    szPath: array[0..MAX_PATH-1] of WCHAR;
  end;

function SHGetSystemImageList(iImageList: Integer): HIMAGELIST;
function SHGetStockIconInfo(siid: Int32; uFlags: UINT; out psii: TSHStockIconInfo): Boolean;
function SHChangeIconDialog(hOwner: HWND; var FileName: String; var IconIndex: Integer): Boolean;
function SHGetStorePropertyValue(const FileName: String; const Key: PROPERTYKEY): Integer;
function SHGetOverlayIconIndex(const sFilePath, sFileName: String): Integer;
function SHGetInfoTip(const sFilePath, sFileName: String): String;
function SHFileIsLinkToFolder(const FileName: String; out LinkTarget: String): Boolean;

function SHGetFolderLocation(hwnd: HWND; csidl: Longint; hToken: HANDLE; dwFlags: DWORD; var ppidl: LPITEMIDLIST): HRESULT; stdcall; external shell32 name 'SHGetFolderLocation';

function PathIsUNCW(pwszPath: LPCWSTR): WINBOOL; stdcall; external 'shlwapi' name 'PathIsUNCW';
function PathFindNextComponentW(pwszPath: LPCWSTR): LPWSTR; stdcall; external 'shlwapi' name 'PathFindNextComponentW';
function StrRetToBufW(pstr: PSTRRET; pidl: PItemIDList; pszBuf: LPWSTR; cchBuf: UINT): HRESULT; stdcall; external 'shlwapi.dll';

procedure OleErrorUTF8(ErrorCode: HResult);
procedure OleCheckUTF8(Result: HResult);

implementation

uses
  SysUtils, JwaShlGuid, ComObj, LazUTF8, DCOSUtils;

var
  SHGetPropertyStoreFromParsingName: function(pszPath: PCWSTR; const pbc: IBindCtx;
                                              flags: GETPROPERTYSTOREFLAGS;
                                              const riid: TIID; out ppv): HRESULT; stdcall;

function SHGetImageListFallback(iImageList: Integer; const riid: TGUID; var ImageList: HIMAGELIST): HRESULT; stdcall;
var
  FileInfo: TSHFileInfoW;
  Flags: UINT = SHGFI_SYSICONINDEX;
begin
  if not IsEqualGUID(riid, IID_IImageList) then Exit(E_NOINTERFACE);
  case iImageList of
  SHIL_LARGE,
  SHIL_EXTRALARGE:
    Flags:= Flags or SHGFI_LARGEICON;
  SHIL_SMALL:
    Flags:= Flags or SHGFI_SMALLICON;
  end;
  ZeroMemory(@FileInfo, SizeOf(TSHFileInfoW));
  ImageList:= SHGetFileInfoW('', 0, FileInfo, SizeOf(FileInfo), Flags);
  if ImageList <> 0 then Exit(S_OK) else Exit(E_FAIL);
end;

function SHGetSystemImageList(iImageList: Integer): HIMAGELIST;
var
  ShellHandle: THandle;
  SHGetImageList: function(iImageList: Integer; const riid: TGUID; var ImageList: HIMAGELIST): HRESULT; stdcall;
begin
  Result:= 0;
  ShellHandle:= GetModuleHandle(Shell32);
  if (ShellHandle <> 0) then
  begin
    @SHGetImageList:= GetProcAddress(ShellHandle, 'SHGetImageList');
    if @SHGetImageList = nil then
    begin
      @SHGetImageList:= GetProcAddress(ShellHandle, PAnsiChar(727));
      if @SHGetImageList = nil then SHGetImageList:= @SHGetImageListFallback;
    end;
    SHGetImageList(iImageList, IID_IImageList, Result);
  end;
end;

function SHGetStockIconInfo(siid: Int32; uFlags: UINT; out psii: TSHStockIconInfo): Boolean;
var
  SHGetStockIconInfo: function(siid: Int32; uFlags: UINT; var psii: TSHStockIconInfo): HRESULT; stdcall;
begin
  Result:= False;
  if (Win32MajorVersion > 5) then
  begin
    @SHGetStockIconInfo:= GetProcAddress(GetModuleHandle(Shell32), 'SHGetStockIconInfo');
    if Assigned(SHGetStockIconInfo) then
    begin
      psii.cbSize:= SizeOf(TSHStockIconInfo);
      Result:= SHGetStockIconInfo(siid, uFlags, psii) = S_OK;
    end;
  end;
end;

function SHChangeIconDialog(hOwner: HWND; var FileName: String; var IconIndex: Integer): Boolean;
type
  TSHChangeIconProcW = function(Wnd: HWND; szFileName: PWideChar; Reserved: Integer;
                                var lpIconIndex: Integer): BOOL; stdcall;
var
  ShellHandle: THandle;
  SHChangeIconW: TSHChangeIconProcW;
  FileNameW: array[0..MAX_PATH] of WideChar;
begin
  Result := True;
  IconIndex := 0;
  ShellHandle := GetModuleHandle(Shell32);
  if ShellHandle <> 0 then
  begin
    @SHChangeIconW := Windows.GetProcAddress(ShellHandle, PAnsiChar(62));
    if Assigned(SHChangeIconW) then
    begin
      FileNameW := UTF8Decode(FileName);
      Result := SHChangeIconW(hOwner, FileNameW, SizeOf(FileNameW), IconIndex);
      if Result then FileName := UTF16ToUTF8(UnicodeString(FileNameW));
    end
  end;
end;

function SHGetStorePropertyValue(const FileName: String; const Key: PROPERTYKEY): Integer;
var
  AValue: Variant;
  AStorage: IPropertyStore;
begin
  if Succeeded(SHGetPropertyStoreFromParsingName(PWideChar(UTF8Decode(FileName)), nil, GPS_DEFAULT, IPropertyStore, AStorage)) then
  begin
    if Succeeded(AStorage.GetValue(@Key, TPROPVARIANT(AValue))) then
      Exit(AValue);
  end;
  Result:= -1;
end;

function SHGetOverlayIconIndex(const sFilePath, sFileName: String): Integer;
var
  Folder,
  DesktopFolder: IShellFolder;
  Pidl,
  ParentPidl: PItemIDList;
  IconOverlay: IShellIconOverlay;
  pchEaten: ULONG;
  dwAttributes: ULONG = 0;
  wsTemp: WideString;
begin
  Result:= -1;

  if SHGetDesktopFolder(DesktopFolder) = S_OK then
  begin
    wsTemp:= UTF8Decode(sFilePath);
    if DesktopFolder.ParseDisplayName(0, nil, PWideChar(wsTemp), pchEaten, ParentPidl, dwAttributes) = S_OK then
    begin
      if DesktopFolder.BindToObject(ParentPidl, nil, IID_IShellFolder, Folder) = S_OK then
      begin
        // Get an IShellIconOverlay interface for the folder.
        // If this fails then this version of
        // the shell does not have this
        // interface.
        if Folder.QueryInterface(IID_IShellIconOverlay, IconOverlay) = S_OK then
          begin
            // Get a pidl for the file.
            wsTemp:= UTF8Decode(sFileName);
            if Folder.ParseDisplayName(0, nil, PWideChar(wsTemp), pchEaten, Pidl, dwAttributes) = S_OK then
            begin
              // Get the overlay icon index.
              if IconOverlay.GetOverlayIconIndex(Pidl, Result) <> S_OK then
                Result:= -1
              // Microsoft OneDrive returns invalid zero index, ignore
              else if (Result = 0) and (Win32MajorVersion >= 10) then
                Result:= -1;

              CoTaskMemFree(Pidl);
            end;
        end;
      end;

      CoTaskMemFree(ParentPidl);
    end;

    DesktopFolder:= nil;
  end; // SHGetDesktopFolder
end;

function SHGetInfoTip(const sFilePath, sFileName: String): String;
var
  DesktopFolder, Folder: IShellFolder;
  pidlFolder: PItemIDList = nil;
  pidlFile: PItemIDList = nil;
  queryInfo: IQueryInfo;
  ppwszTip: PWideChar = nil;
  pchEaten: ULONG;
  dwAttributes: ULONG = 0;
  wsTemp: WideString;
begin
  Result:= EmptyStr;
  if Succeeded(SHGetDesktopFolder(DesktopFolder)) then
    try
      wsTemp:= UTF8Decode(sFilePath);
      if Succeeded(DesktopFolder.ParseDisplayName(0, nil, PWideChar(wsTemp), pchEaten, pidlFolder, dwAttributes)) then
        if Succeeded(DesktopFolder.BindToObject(pidlFolder, nil, IID_IShellFolder, Folder)) then
          try
            wsTemp:= UTF8Decode(sFileName);
            if Succeeded(Folder.ParseDisplayName(0, nil, PWideChar(wsTemp), pchEaten, pidlFile, dwAttributes)) then
              if Succeeded(Folder.GetUIObjectOf(0, 1, pidlFile, IID_IQueryInfo, nil, queryInfo)) then
                if Succeeded(queryInfo.GetInfoTip(QITIPF_USESLOWTIP, ppwszTip)) then
                  Result:= UTF16ToUTF8(WideString(ppwszTip));
          finally
            Folder:= nil;
            queryInfo:= nil;
            if Assigned(ppwszTip) then
              CoTaskMemFree(ppwszTip);
            if Assigned(pidlFile) then
              CoTaskMemFree(pidlFile);
          end;
    finally
      DesktopFolder:= nil;
      if Assigned(pidlFolder) then
        CoTaskMemFree(pidlFolder);
    end;
end;

function SHFileIsLinkToFolder(const FileName: String; out LinkTarget: String): Boolean;
var
  Unknown: IUnknown;
  ShellLink: IShellLinkW;
  PersistFile: IPersistFile;
  FindData: TWin32FindDataW;
  pszFile:LPWSTR;
begin
  Result := False;
  try
    Unknown := CreateComObject(CLSID_ShellLink);
    ShellLink := Unknown as IShellLinkW;
    PersistFile := Unknown as IPersistFile;
    if Failed(PersistFile.Load(PWideChar(UTF8Decode(FileName)), OF_READ)) then Exit;
    pszFile:= GetMem(MAX_PATH * 2);
    try
      if Failed(ShellLink.GetPath(pszFile, MAX_PATH, @FindData, 0)) then Exit;
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
      begin
        LinkTarget := UTF16ToUTF8(WideString(pszFile));
        Result := (LinkTarget <> EmptyStr);
      end;
    finally
      FreeMem(pszFile);
    end;
  except
    LinkTarget := EmptyStr;
  end;
end;

procedure OleErrorUTF8(ErrorCode: HResult);
begin
  raise EOleError.Create(mbSysErrorMessage(ErrorCode));
end;

procedure OleCheckUTF8(Result: HResult);
begin
  if not Succeeded(Result) then OleErrorUTF8(Result);
end;

initialization
  SHGetPropertyStoreFromParsingName:= GetProcAddress(GetModuleHandle('shell32.dll'),
                                                     'SHGetPropertyStoreFromParsingName');

end. { ShlObjAdditional }
