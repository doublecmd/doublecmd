{
   Double Commander
   -------------------------------------------------------------------------
   Windows thumbnail provider

   Copyright (C) 2012 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uThumbnailProvider;

{$mode delphi}

interface

uses
  uThumbnails;

implementation

uses
  SysUtils, Forms, Graphics, Windows, ActiveX, ShlObj;

const
  SIIGBF_RESIZETOFIT   = $00000000;
  SIIGBF_BIGGERSIZEOK  = $00000001;
  SIIGBF_MEMORYONLY    = $00000002;
  SIIGBF_ICONONLY      = $00000004;
  SIIGBF_THUMBNAILONLY = $00000008;
  SIIGBF_INCACHEONLY   = $00000010;

const
  IID_IExtractImage: TGUID = '{BB2E617C-0920-11d1-9A0B-00C04FC2D6C1}';

type
  SIIGBF = Integer;

  IShellItemImageFactory = interface(IUnknown)
    ['{BCC18B79-BA16-442F-80C4-8A59C30C463B}']
    function GetImage(size: TSize; flags: SIIGBF; out phbm: HBITMAP): HRESULT; stdcall;
  end;

  IExtractImage = interface(IUnknown)
    ['{BB2E617C-0920-11d1-9A0B-00C04FC2D6C1}']
    function GetLocation(pszPathBuffer: LPWSTR; cchMax: DWORD; out pdwPriority: DWORD;
      const prgSize: LPSIZE; dwRecClrDepth: DWORD; var pdwFlags: DWORD): HRESULT; stdcall;
    function Extract(out phBmpImage: HBITMAP): HRESULT; stdcall;
  end;

var
  SHCreateItemFromParsingName: function(pszPath: LPCWSTR; const pbc: IBindCtx;
                                        const riid: TIID; out ppv): HRESULT; stdcall;

function GetThumbnailOld(const aFileName: UTF8String; aSize: TSize; out Bitmap: HBITMAP): HRESULT;
var
  Folder,
  DesktopFolder: IShellFolder;
  Pidl,
  ParentPidl: PItemIDList;
  Image: IExtractImage;
  pchEaten: ULONG;
  wsTemp: WideString;
  dwPriority: DWORD;
  Status: HRESULT;
  dwRecClrDepth: DWORD;
  dwAttributes: ULONG = 0;
  dwFlags: DWORD = IEIFLAG_SCREEN or IEIFLAG_QUALITY;
begin
  Result:= E_FAIL;

  if SHGetDesktopFolder(DesktopFolder) = S_OK then
  begin
    wsTemp:= UTF8Decode(ExtractFilePath(aFileName));
    if DesktopFolder.ParseDisplayName(0, nil, PWideChar(wsTemp), pchEaten, ParentPidl, dwAttributes) = S_OK then
    begin
      if DesktopFolder.BindToObject(ParentPidl, nil, IID_IShellFolder, Folder) = S_OK then
      begin
        wsTemp:= UTF8Decode(ExtractFileName(aFileName));
        if Folder.ParseDisplayName(0, nil, PWideChar(wsTemp), pchEaten, Pidl, dwAttributes) = S_OK then
        begin
          if Succeeded(Folder.GetUIObjectOf(0, 1, Pidl, IID_IExtractImage, nil, Image)) then
          begin
            SetLength(wsTemp, MAX_PATH * SizeOf(WideChar));
            dwRecClrDepth:= GetDeviceCaps(Application.MainForm.Canvas.Handle, BITSPIXEL);
            Status:= Image.GetLocation(PWideChar(wsTemp), Length(wsTemp), dwPriority, @aSize, dwRecClrDepth, dwFlags);
            if (Status = NOERROR) or (Status = E_PENDING) then
            begin
              Result:= Image.Extract(Bitmap);
            end;
          end;
          CoTaskMemFree(Pidl);
        end;
        Folder:= nil;
      end;
      CoTaskMemFree(ParentPidl);
    end;
    DesktopFolder:= nil;
  end; // SHGetDesktopFolder
end;

function GetThumbnailNew(const aFileName: UTF8String; aSize: TSize; out Bitmap: HBITMAP): HRESULT;
var
  ShellItemImage: IShellItemImageFactory;
begin
  Result := E_FAIL;
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE);
  Result:= SHCreateItemFromParsingName(PWideChar(UTF8Decode(aFileName)), nil,
                                       IShellItemImageFactory, ShellItemImage);
  if Succeeded(Result) then
  begin
    Result:= ShellItemImage.GetImage(aSize, SIIGBF_THUMBNAILONLY, Bitmap);
  end;
end;

function GetThumbnail(const aFileName: UTF8String; aSize: TSize): Graphics.TBitmap;
var
  Bitmap: HBITMAP;
  Status: HRESULT = E_FAIL;
begin
  Result:= nil;

  if (Win32MajorVersion > 5) then
  begin
    Status:= GetThumbnailNew(aFileName, aSize, Bitmap);
  end;

  if Failed(Status) then
  begin
    Status:= GetThumbnailOld(aFileName, aSize, Bitmap);
  end;

  if Succeeded(Status) then
  begin
    Result:= Graphics.TBitmap.Create;
    Result.Handle:= Bitmap;
    DeleteObject(Bitmap);
  end;
end;

initialization
  SHCreateItemFromParsingName:= GetProcAddress(GetModuleHandle('shell32.dll'),
                                               'SHCreateItemFromParsingName');
  TThumbnailManager.RegisterProvider(@GetThumbnail);

end.

