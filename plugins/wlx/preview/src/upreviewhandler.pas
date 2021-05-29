{
   Double commander
   -------------------------------------------------------------------------
   Explorer preview handler

   Copyright (C) 2021 Alexander Koblov (alexx2000@mail.ru)

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

unit uPreviewHandler;

{$mode delphi}

interface

uses
  Classes, SysUtils, Windows, ShlObj, ActiveX;

type
  IPreviewHandler = interface(IUnknown)
    ['{8895B1C6-B41F-4C1C-A562-0D564250836F}']
    function SetWindow(hwnd: HWND; const prc: RECT): HRESULT; stdcall;
    function SetRect(const prc: PRECT): HRESULT; stdcall;
    function DoPreview(): HRESULT; stdcall;
    function Unload(): HRESULT; stdcall;
    function SetFocus(): HRESULT; stdcall;
    function QueryFocus(out phwnd: HWND): HRESULT; stdcall;
    function TranslateAccelerator(pmsg: PMSG): HRESULT; stdcall;
  end;

function GetPreviewHandler(const FileName: UnicodeString): IPreviewHandler;

implementation

uses
  ComObj;

type
  IInitializeWithFile = interface(IUnknown)
    ['{B7D14566-0509-4CCE-A71F-0A554233BD9B}']
    function Initialize(pszFilePath: LPCWSTR; grfMode: DWORD): HRESULT; stdcall;
  end;

  IInitializeWithStream = interface(IUnknown)
    ['{B824B49D-22AC-4161-AC8A-9916E8FA3F7F}']
    function Initialize(const pstream: IStream; grfMode: DWORD): HRESULT; stdcall;
  end;

  IInitializeWithItem = interface(IUnknown)
    ['{7F73BE3F-FB79-493C-A6C7-7EE14E245841}']
    function Initialize(const psi: IShellItem; grfMode: DWORD): HRESULT; stdcall;
  end;

var
  SHCreateItemFromParsingName: function(pszPath: LPCWSTR; const pbc: IBindCtx;
                                        const riid: TIID; out ppv): HRESULT; stdcall;

function AssocQueryStringW(flags: DWORD; str: DWORD; pszAssoc: LPCWSTR; pszExtra: LPCWSTR;
                           pszOut: LPWSTR; pcchOut: PDWORD): HRESULT; stdcall; external 'shlwapi.dll';

function GetShellClass(const FileExt: UnicodeString; interfaceID: TGUID): TGUID;
const
  ASSOCSTR_SHELLEXTENSION = 16;
  ASSOCF_INIT_DEFAULTTOSTAR  = $00000004;
var
  Res: HRESULT;
  cchOut: DWORD = MAX_PATH;
  ABuffer: array[0..MAX_PATH] of WideChar;
begin
  Res := AssocQueryStringW(ASSOCF_INIT_DEFAULTTOSTAR, ASSOCSTR_SHELLEXTENSION, PWideChar(FileExt),
                           PWideChar(UTF8Decode(GuidToString(interfaceID))), ABuffer, @cchOut);
  if (Res <> S_OK) then Exit(Default(TGUID));
  Res := CLSIDFromString(ABuffer, @Result);
  if (Res <> NOERROR) then Exit(Default(TGUID));
end;

function GetPreviewHandler(const FileName: UnicodeString): IPreviewHandler;
var
  Res: HRESULT;
  ClassID: TGUID;
  AStream: IStream;
  AFile: TFileStream;
  AShellItem: IShellItem;
  AInitializeWithFile: IInitializeWithFile;
  AInitializeWithItem: IInitializeWithItem;
  AInitializeWithStream: IInitializeWithStream;
begin
  ClassID:= GetShellClass(ExtractFileExt(FileName), IPreviewHandler);
  if IsEqualGUID(ClassID, Default(TGUID)) then Exit(nil);
  Result:= CreateComObject(ClassID) as IPreviewHandler;
  if Assigned(Result) then
  begin
    if Supports(Result, IInitializeWithFile, AInitializeWithFile) then
      Res:= AInitializeWithFile.Initialize(PWideChar(FileName), STGM_READ)
    else if Supports(Result, IInitializeWithStream, AInitializeWithStream) then
    try
      AFile:= TFileStream.Create(UTF8Encode(FileName), fmOpenRead or fmShareDenyNone);
      AStream:= TStreamAdapter.Create(AFile, soOwned) as IStream;
      Res:= AInitializeWithStream.Initialize(AStream, STGM_READ);
    except
      Res:= E_FAIL;
    end
    else if (Win32MajorVersion > 5) and Supports(Result, IInitializeWithItem, AInitializeWithItem) then
    begin
      Res:= SHCreateItemFromParsingName(PWideChar(FileName), nil, IShellItem, AShellItem);
      if Succeeded(Res) then Res:= AInitializeWithItem.Initialize(AShellItem, STGM_READ);
    end
    else begin
      Res:= E_FAIL;
    end;
    if not Succeeded(Res) then
    begin
      Result:= nil;
      AStream:= nil;
    end;
  end;
end;

initialization
  if (Win32MajorVersion > 5) then
    SHCreateItemFromParsingName:= GetProcAddress(GetModuleHandle('shell32.dll'),
                                                 'SHCreateItemFromParsingName');

end.

