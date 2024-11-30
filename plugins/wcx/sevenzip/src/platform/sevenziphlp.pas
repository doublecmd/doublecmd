unit SevenZipHlp;

{$mode delphi}

interface

uses
  Classes, SysUtils, ActiveX;

const
  FILE_ATTRIBUTE_UNIX_EXTENSION =  $8000;

const
{$IF DEFINED(MSWINDOWS)}
  SevenZipSfxExt = '.exe';
  SevenZipSfxName = '7z.sfx';
  SevenZipDefaultLibraryPath = '%ProgramFiles%\7-Zip\';
{$ELSE}
  SevenZipSfxExt = '.run';
  SevenZipSfxName = '7zCon.sfx';
  SevenZipDefaultLibraryPath = '/usr/lib/7zip/';
{$ENDIF}

function GetNumberOfProcessors: LongWord;
function SysAttrToSevenZip(Attr: Cardinal): Cardinal;
function SevenZipToWcxAttr(Attr: Cardinal): Cardinal;
function WideToBinary(const Value: WideString): TBstr;
procedure VarStringClear(var PropVariant: TPropVariant);
function BinaryToUnicode(const bstrVal: TBstr): UnicodeString;
function CWideCharToWideString(const Value: Pointer): WideString;
function MessageBox(const Text: String; Caption: PAnsiChar; Flags: LongInt): Integer; overload;

function FileMove(const OldName, NewName: String): Boolean;

type
  TMessageBoxFunction = function(Text, Caption: PAnsiChar; Flags: LongInt): Integer; winapi;

var
  MessageBoxFunction: TMessageBoxFunction = nil;

implementation

uses
{$IF DEFINED(MSWINDOWS)}
  Windows, DCWindows
{$ELSE}
  SevenZip, DCUnix, FileUtil, DCOSUtils, DCFileAttributes
{$ENDIF}
  ;

function GetNumberOfProcessors: LongWord;
{$IF DEFINED(MSWINDOWS)}
var
  SystemInfo: TSYSTEMINFO;
  SystemAffinityMask: DWORD_PTR = 0;
  ProcessAffinityMask: DWORD_PTR = 0;
begin
  if GetProcessAffinityMask(GetCurrentProcess, ProcessAffinityMask, SystemAffinityMask) then
  begin
    Result:= PopCnt(ProcessAffinityMask);
  end
  else begin
    GetSystemInfo(@SystemInfo);
    Result:= SystemInfo.dwNumberOfProcessors;
  end;
end;
{$ELSEIF DEFINED(LINUX) OR DEFINED(DARWIN) OR DEFINED(FREEBSD)}
begin
  Result:= sysconf(_SC_NPROCESSORS_ONLN);
end;
{$ELSE}
begin
  Result:= 1;
end;
{$ENDIF}

function SysAttrToSevenZip(Attr: Cardinal): Cardinal;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= Attr;
end;
{$ELSE}
begin
  Result:= UnixToWinFileAttr(Attr);
  Result:= Result or FILE_ATTRIBUTE_UNIX_EXTENSION or ((Attr and $FFFF) << 16);
end;
{$ENDIF}

function SevenZipToWcxAttr(Attr: Cardinal): Cardinal;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= Attr;
end;
{$ELSE}
begin
  if (Attr and FILE_ATTRIBUTE_UNIX_EXTENSION <> 0) then
    Result:= (Attr >> 16)
  else begin
    Result:= WinToUnixFileAttr(Attr);
  end;
end;
{$ENDIF}

procedure VarStringClear(var PropVariant: TPropVariant);
begin
  PropVariant.vt:= VT_EMPTY;
  SysFreeString(PropVariant.bstrVal);
end;

function WideToBinary(const Value: WideString): TBstr;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= SysAllocString(PWideChar(Value));
end;
{$ELSE}
var
  S: UCS4String;
begin
  S:= WideStringToUCS4String(Value);
  Result:= SysAllocString(PUCS4Char(S));
end;
{$ENDIF}

function BinaryToUnicode(const bstrVal: TBstr): UnicodeString;
var
  PropSize: Cardinal;
{$IF DEFINED(MSWINDOWS)}
begin
  PropSize:= SysStringByteLen(bstrVal);
  SetLength(Result, PropSize div SizeOf(WideChar));
  Move(bstrVal^, PWideChar(Result)^, PropSize);
end;
{$ELSE}
var
  S: UCS4String;
begin
  PropSize:= SysStringByteLen(bstrVal);
  SetLength(S, PropSize div SizeOf(UCS4Char) + 1);
  Move(bstrVal^, Pointer(S)^, PropSize);
  Result:= UCS4StringToUnicodeString(S);
end;
{$ENDIF}

function CWideCharToWideString(const Value: Pointer): WideString;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= WideString(PWideChar(Value));
end;
{$ELSE}
var
  P: PUCS4Char;
  S: UCS4String;
  Len: Integer = 0;
begin
  P:= PUCS4Char(Value);
  while (P^ <> 0) do
  begin
    Inc(P);
    Inc(Len);
  end;
  SetLength(S, Len + 1);
  Move(Value^, Pointer(S)^, Len * SizeOf(UCS4Char));
  Result:= UCS4StringToUnicodeString(S);
end;
{$ENDIF}

function MessageBox(const Text: String; Caption: PAnsiChar; Flags: LongInt): Integer;
begin
  if (@MessageBoxFunction = nil) then
    Result:= -1
  else begin
    Result:= MessageBoxFunction(PAnsiChar(Text), Caption, Flags);
  end;
end;

function FileMove(const OldName, NewName: String): Boolean;
{$IF DEFINED(MSWINDOWS)}
begin
  Result:= MoveFileExW(PWideChar(UTF16LongName(OldName)), PWideChar(UTF16LongName(NewName)),
                       MOVEFILE_REPLACE_EXISTING or MOVEFILE_COPY_ALLOWED);
end;
{$ELSE}
begin
  Result:= RenameFile(OldName, NewName);
  if (not Result) and (GetLastOSError = ERROR_NOT_SAME_DEVICE) then
  begin
    Result:= CopyFile(OldName, NewName, [cffOverwriteFile, cffPreserveTime]);
    if Result then mbDeleteFile(OldName);
  end;
end;
{$ENDIF}

end.
