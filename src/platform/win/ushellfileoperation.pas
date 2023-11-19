unit uShellFileOperation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Windows, ActiveX, ShlObj, ComObj, ShlWAPI, ShellAPI,
  uShellFolder;

const
  CLSID_FileOperation: TGUID = '{3ad05575-8857-4850-9277-11b85bdb8e09}';

type

  IOperationsProgressDialog = IUnknown;

  { IObjectWithPropertyKey }

  IObjectWithPropertyKey = interface(IUnknown)
    ['{fc0ca0a7-c316-4fd2-9031-3e628e6d4f23}']
    function SetPropertyKey(key: REFPROPERTYKEY): HRESULT; stdcall;
    function GetPropertyKey(var pkey: PROPERTYKEY): HRESULT; stdcall;
  end;

  { IPropertyChange }

  IPropertyChange = interface(IObjectWithPropertyKey)
    ['{f917bc8a-1bba-4478-a245-1bde03eb9431}']
    function ApplyToPropVariant(propvarIn: REFPROPVARIANT; ppropvarOut: PPROPVARIANT): HRESULT; stdcall;
  end;

  { IPropertyChangeArray }

  IPropertyChangeArray = interface(IUnknown)
    ['{380f5cad-1b5e-42f2-805d-637fd392d31e}']
      function GetCount(pcOperations: PUINT): HRESULT; stdcall;
      function GetAt(iIndex: UINT; const riid: REFIID; out ppv): HRESULT; stdcall;
      function InsertAt(iIndex: UINT; ppropChange: IPropertyChange): HRESULT; stdcall;
      function Append(ppropChange: IPropertyChange): HRESULT; stdcall;
      function AppendOrReplace(ppropChange: IPropertyChange): HRESULT; stdcall;
      function RemoveAt(iIndex: UINT): HRESULT; stdcall;
      function IsKeyInArray(key: REFPROPERTYKEY): HRESULT; stdcall;
  end;

  { IFileOperation }

  IFileOperation = interface(IUnknown)
    ['{947aab5f-0a5c-4c13-b4d6-4bf7836fc9f8}']
      function Advise(pfops: IFileOperationProgressSink; pdwCookie: PDWORD): HRESULT; stdcall;
      function Unadvise(dwCookie: DWORD): HRESULT; stdcall;
      function SetOperationFlags(dwOperationFlags: DWORD): HRESULT; stdcall;
      function SetProgressMessage(pszMessage: LPCWSTR): HRESULT; stdcall;
      function SetProgressDialog(popd: IOperationsProgressDialog): HRESULT; stdcall;
      function SetProperties(pproparray: IPropertyChangeArray): HRESULT; stdcall;
      function SetOwnerWindow(hwndOwner: HWND): HRESULT; stdcall;
      function ApplyPropertiesToItem(psiItem: IShellItem): HRESULT; stdcall;
      function ApplyPropertiesToItems(punkItems: IUnknown): HRESULT; stdcall;
      function RenameItem(psiItem: IShellItem; pszNewName: LPCWSTR;
                          pfopsItem: IFileOperationProgressSink): HRESULT; stdcall;
      function RenameItems(pUnkItems: IUnknown; pszNewName: LPCWSTR): HRESULT; stdcall;
      function MoveItem(psiItem: IShellItem; psiDestinationFolder: IShellItem;
                        pszNewName: LPCWSTR; pfopsItem: IFileOperationProgressSink): HRESULT; stdcall;
      function MoveItems(punkItems: IUnknown; psiDestinationFolder: IShellItem): HRESULT; stdcall;
      function CopyItem(psiItem: IShellItem; psiDestinationFolder: IShellItem;
                        pszCopyName: LPCWSTR; pfopsItem: IFileOperationProgressSink): HRESULT; stdcall;
      function CopyItems(punkItems: IUnknown; psiDestinationFolder: IShellItem): HRESULT; stdcall;
      function DeleteItem(psiItem: IShellItem; pfopsItem: IFileOperationProgressSink): HRESULT; stdcall;
      function DeleteItems(punkItems: IUnknown): HRESULT; stdcall;
      function NewItem(psiDestinationFolder: IShellItem; dwFileAttributes: DWORD;
                       pszName: LPCWSTR; pszTemplateName: LPCWSTR; pfopsItem: IFileOperationProgressSink): HRESULT; stdcall;
      function PerformOperations(): HRESULT; stdcall;
      function GetAnyOperationsAborted(pfAnyOperationsAborted: PBOOL): HRESULT; stdcall;
  end;

implementation

end.

