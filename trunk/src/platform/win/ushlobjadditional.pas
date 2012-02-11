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
   ActiveX;

const
   { User canceled the current action }
   COPYENGINE_E_USER_CANCELLED: HRESULT = HRESULT($80270000);

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

function SHChangeIconDialog(hOwner: THandle; var FileName: UTF8String; var IconIndex: Integer): Boolean;
function SHGetOverlayIconIndex(const sFilePath, sFileName: UTF8String): Integer;
function SHGetInfoTip(const sFilePath, sFileName: UTF8String): UTF8String;
function SHFileIsLinkToFolder(const FileName: UTF8String; out LinkTarget: UTF8String): Boolean;

function PathIsUNCA(pszPath: LPCSTR): WINBOOL; stdcall; external 'shlwapi' name 'PathIsUNCA';
function PathIsUNCW(pwszPath: LPCWSTR): WINBOOL; stdcall; external 'shlwapi' name 'PathIsUNCW';

function PathFindNextComponentA(pszPath: LPCSTR): LPSTR; stdcall; external 'shlwapi' name 'PathFindNextComponentA';
function PathFindNextComponentW(pwszPath: LPCWSTR): LPWSTR; stdcall; external 'shlwapi' name 'PathFindNextComponentW';

procedure OleErrorUTF8(ErrorCode: HResult);
procedure OleCheckUTF8(Result: HResult);

implementation

uses
  SysUtils, JwaShlGuid, ComObj;

const
   Shell32 = 'shell32.dll';

{ **** UBPFD *********** by delphibase.endimus.com ****
>> Calls icon selection dialog. Modified function for calling
"Change icon" dialog.

Dependencies: Windows, SysUtils
Author:       Alex Sal'nikov, alex-co@narod.ru, Moscow
Copyright:    Modified JVCL library
Date:         15 july 2003 ã.
***************************************************** }

function SHChangeIconDialog(hOwner: THandle; var FileName: UTF8String; var IconIndex: Integer): Boolean;
type
  TSHChangeIconProc = function(Wnd: HWND; szFileName: PChar; Reserved: Integer;
                               var lpIconIndex: Integer): DWORD; stdcall;
  TSHChangeIconProcW = function(Wnd: HWND; szFileName: PWideChar;Reserved: Integer;
                                var lpIconIndex: Integer): DWORD; stdcall;
var
  ShellHandle: THandle;
  SHChangeIcon: TSHChangeIconProc;
  SHChangeIconW: TSHChangeIconProcW;
  Buf: array[0..MAX_PATH] of AnsiChar;
  BufW: array[0..MAX_PATH] of WideChar;
begin
  Result := False;
  SHChangeIcon := nil;
  SHChangeIconW := nil;
  ShellHandle := Windows.LoadLibrary(PChar(Shell32));
  try
    if ShellHandle <> 0 then
    begin
      if Win32Platform = VER_PLATFORM_WIN32_NT then
        SHChangeIconW := TSHChangeIconProcW(Windows.GetProcAddress(ShellHandle, PChar(62)))
      else
        SHChangeIcon := TSHChangeIconProc(Windows.GetProcAddress(ShellHandle, PChar(62)));
    end;

    if Assigned(SHChangeIconW) then
    begin
      BufW := UTF8Decode(FileName);
      Result := SHChangeIconW(hOwner, BufW, SizeOf(BufW), IconIndex) = 1;
      if Result then
        FileName := UTF8Encode(WideString(BufW));
    end
    else if Assigned(SHChangeIcon) then
    begin
      Buf := UTF8ToAnsi(FileName);
      Result := SHChangeIcon(hOwner, Buf, SizeOf(Buf), IconIndex) = 1;
      if Result then
        FileName := AnsiToUTF8(Buf);
    end
    else
      begin
        IconIndex := 0;
        Result := True;
      end;
  finally
    if ShellHandle <> 0 then
      FreeLibrary(ShellHandle);
  end;
end;

function SHGetOverlayIconIndex(const sFilePath, sFileName: UTF8String): Integer;
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

function SHGetInfoTip(const sFilePath, sFileName: UTF8String): UTF8String;
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
                  Result:= UTF8Encode(WideString(ppwszTip));
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

function SHFileIsLinkToFolder(const FileName: UTF8String; out LinkTarget: UTF8String): Boolean;
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
        LinkTarget := UTF8Encode(WideString(pszFile));
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
  raise EOleError.Create(UTF8Encode(SysErrorMessage(ErrorCode)));
end;

procedure OleCheckUTF8(Result: HResult);
begin
  if not Succeeded(Result) then OleErrorUTF8(Result);
end;

end. { ShlObjAdditional }
