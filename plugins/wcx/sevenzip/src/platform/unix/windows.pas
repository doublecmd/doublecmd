unit Windows;

{$mode delphi}

interface

uses
  SysUtils, Types;

const
  ERROR_SUCCESS = 0;
  MAXDWORD = High(DWORD);
  E_OUTOFMEMORY = HRESULT($8007000E);
  FILE_ATTRIBUTE_ARCHIVE = faArchive;
  FILE_ATTRIBUTE_DIRECTORY = faDirectory;
  // MessageBox: Flags
  MB_OK                   = $00000000;
  MB_ABORTRETRYIGNORE     = $00000002;
  MB_ICONERROR            = $00000010;
  // MessageBox: Result
  IDOK         = 1;
  IDABORT      = 3;
  IDRETRY      = 4;
  IDIGNORE     = 5;

type
  UCHAR = byte;
  WCHAR = WideChar;

  HWND = UIntPtr;

  UINT   = cardinal;
  ULONG  = cardinal;
  USHORT = word;

  SHORT = smallint;
  WINT  = longint;
  LONG  = longint;
  LONG64= int64;
  ULONG64 = qword;
  ULONG32 = cardinal;
  DWORD = cardinal;

  LONGLONG  = int64;
  ULONGLONG  = qword;

  LARGE_INTEGER = record
     case byte of
       0: (LowPart : DWORD;
           HighPart : LONG);
       1: (QuadPart : LONGLONG);
    end;
  PLARGE_INTEGER = ^LARGE_INTEGER;
  _LARGE_INTEGER = LARGE_INTEGER;

  TLargeInteger = Int64;
  PLargeInteger = ^TLargeInteger;

  ULARGE_INTEGER = record
     case byte of
       0: (LowPart : DWORD;
           HighPart : DWORD);
       1: (QuadPart : ULONGLONG);
    end;
  PULARGE_INTEGER = ^ULARGE_INTEGER;
  _ULARGE_INTEGER = ULARGE_INTEGER;

  TULargeInteger = QWord;
  PULargeInteger = ^TULargeInteger;

  CLSID = TGUID;

  LPSTR = Pchar;
  LPWSTR = PWideChar;

procedure ZeroMemory(Destination: Pointer; Length: UIntPtr); inline;
procedure CopyMemory(Destination, Source: Pointer; Length: UIntPtr); inline;

implementation

procedure ZeroMemory(Destination: Pointer; Length: UIntPtr);
begin
  FillChar(Destination^, Length, 0);
end;

procedure CopyMemory(Destination, Source: Pointer; Length: UIntPtr);
begin
  Move(Source^, Destination^, Length);
end;

end.
