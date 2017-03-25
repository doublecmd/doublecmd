{
  Double Commander
  -------------------------------------------------------------------------
  SevenZip archiver plugin

  Copyright (C) 2015 Alexander Koblov (alexx2000@mail.ru)

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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

unit DCJclAlternative;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Windows, LazUTF8Classes;

// JclBase.pas -----------------------------------------------------------------
type
  EJclError = class(Exception);
  TDynByteArray = array of Byte;
  TDynCardinalArray = array of Cardinal;

type
  JclBase = class
  type
    PPInt64 = ^PInt64;
  end;

// JclStreams.pas --------------------------------------------------------------
type
  TJclStream = TStream;
  TJclOnVolume = function(Index: Integer): TStream of object;
  TJclOnVolumeMaxSize = function(Index: Integer): Int64 of object;

type

  { TJclDynamicSplitStream }

  TJclDynamicSplitStream = class(TJclStream)
  private
    FVolume: TStream;
    FOnVolume: TJclOnVolume;
    FOnVolumeMaxSize: TJclOnVolumeMaxSize;
  private
    function LoadVolume: Boolean;
    function GetVolume(Index: Integer): TStream;
    function GetVolumeMaxSize(Index: Integer): Int64;
  protected
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(ADummy: Boolean = False);

    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;

    property OnVolume: TJclOnVolume read FOnVolume write FOnVolume;
    property OnVolumeMaxSize: TJclOnVolumeMaxSize read FOnVolumeMaxSize write FOnVolumeMaxSize;
  end;

  function StreamCopy(Source, Target: TStream): Int64;

// JclDateTime.pas -------------------------------------------------------------
function LocalDateTimeToFileTime(DateTime: TDateTime): TFileTime;

// JclFileUtils.pas ------------------------------------------------------------
const
  DirDelimiter = DirectorySeparator;
  DirSeparator = PathSeparator;

type
  TJclOnAddDirectory = procedure(const Directory: String) of object;
  TJclOnAddFile = procedure(const Directory: String; const FileInfo: TSearchRec) of object;

function PathAddSeparator(const Path: String): String; inline;
function PathRemoveSeparator(const Path: String): String; inline;
function PathGetRelativePath(const Base, Path: String): String; inline;

function PathCanonicalize(const Path: WideString): WideString;
function IsFileNameMatch(const FileName, Mask: WideString): Boolean; inline;

procedure BuildFileList(const SourceFile: String; FileAttr: Integer; InnerList: TStrings; Dummy: Boolean);
procedure EnumFiles(const Path: String; OnAddFile: TJclOnAddFile; ExcludeAttributes: Integer);
procedure EnumDirectories(const Path: String; OnAddDirectory: TJclOnAddDirectory;
                          DummyBoolean: Boolean; const DummyString: String; DummyPointer: Pointer);

function FileDelete(const FileName: String): Boolean; inline;
function FindUnusedFileName(const FileName, FileExt: String): String;
function FileMove(const OldName, NewName: String; Replace: Boolean = False): Boolean;

// JclSysUtils.pas -------------------------------------------------------------
type
  TModuleHandle = HINST;

const
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(0);

type
  JclSysUtils = class
    class function LoadModule(var Module: TModuleHandle; FileName: String): Boolean;
    class procedure UnloadModule(var Module: TModuleHandle);
  end;

function GUIDEquals(const GUID1, GUID2: TGUID): Boolean; inline;
function GetModuleSymbol(Module: TModuleHandle; SymbolName: String): Pointer; inline;

// JclStrings.pas --------------------------------------------------------------
procedure StrTokenToStrings(const Token: String; Separator: AnsiChar; var Strings: TStrings);

// JclWideStrings.pas ----------------------------------------------------------
type
  TFPWideStrObjMap = specialize TFPGMap<WideString, TObject>;

type

  { TJclWideStringList }

  TJclWideStringList = class(TFPWideStrObjMap)
  private
    FCaseSensitive: Boolean;
  private
    procedure SetCaseSensitive(AValue: Boolean);
    function CompareWideStringProc(Key1, Key2: Pointer): Integer;
    function CompareTextWideStringProc(Key1, Key2: Pointer): Integer;
  protected
    function Get(Index: Integer): WideString;
    function GetObject(Index: Integer): TObject;
    procedure Put(Index: Integer; const S: WideString);
    procedure PutObject(Index: Integer; AObject: TObject);
  public
    constructor Create;
    function AddObject(const S: WideString; AObject: TObject): Integer;

    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Strings[Index: Integer]: WideString read Get write Put; default;
  end;

// Classes.pas -----------------------------------------------------------------
type
  TFileStream = TFileStreamUTF8;

// SysUtils.pas -----------------------------------------------------------------
function FileExists(const FileName: String): Boolean; inline;

// Windows.pas -----------------------------------------------------------------
function CreateFile(lpFileName: LPCSTR; dwDesiredAccess: DWORD; dwShareMode: DWORD; lpSecurityAttributes: LPSECURITY_ATTRIBUTES;
                    dwCreationDisposition: DWORD; dwFlagsAndAttributes: DWORD; hTemplateFile: HANDLE): HANDLE; inline;
function GetFileAttributesEx(lpFileName: LPCSTR; fInfoLevelId: TGET_FILEEX_INFO_LEVELS; lpFileInformation: Pointer): BOOL; inline;

implementation

uses
  LazFileUtils;

function StreamCopy(Source, Target: TStream): Int64;
begin
  Result:= Target.CopyFrom(Source, Source.Size);
end;

function LocalDateTimeToFileTime(DateTime: TDateTime): TFileTime;
begin
  Int64(Result) := Round((Extended(DateTime) + 109205.0) * 864000000000.0);
  Windows.LocalFileTimeToFileTime(@Result, @Result);
end;

function PathAddSeparator(const Path: String): String;
begin
  Result:= IncludeTrailingPathDelimiter(Path);
end;

function PathRemoveSeparator(const Path: String): String;
begin
  Result:= ExcludeTrailingPathDelimiter(Path);
end;

function PathGetRelativePath(const Base, Path: String): String;
begin
  Result:= ExtractRelativePath(Base, Path);
end;

function PathMatchSpecW(pszFile, pszSpec: LPCWSTR): BOOL; stdcall; external 'shlwapi.dll';
function PathCanonicalizeW(lpszDst, lpszSrc: LPCWSTR): BOOL; stdcall; external 'shlwapi.dll';

function PathCanonicalize(const Path: WideString): WideString;
begin
  SetLength(Result, MAX_PATH);
  if PathCanonicalizeW(PWideChar(Result), PWideChar(Path)) then
    Result:= PWideChar(Result)
  else begin
    Result:= EmptyWideStr;
  end;
end;

function IsFileNameMatch(const FileName, Mask: WideString): Boolean;
begin
  Result:= PathMatchSpecW(PWideChar(FileName), PWideChar(Mask));
end;

procedure BuildFileList(const SourceFile: String; FileAttr: Integer;
                        InnerList: TStrings; Dummy: Boolean);
begin
  raise Exception.Create('Not implemented');
end;

procedure EnumFiles(const Path: String; OnAddFile: TJclOnAddFile; ExcludeAttributes: Integer);
begin
  raise Exception.Create('Not implemented');
end;

procedure EnumDirectories(const Path: String; OnAddDirectory: TJclOnAddDirectory;
                          DummyBoolean: Boolean; const DummyString: String; DummyPointer: Pointer);
begin
  raise Exception.Create('Not implemented');
end;

function FileDelete(const FileName: String): Boolean;
begin
  Result:= DeleteFileW(PWideChar(UTF8Decode(FileName)));
end;

function FindUnusedFileName(const FileName, FileExt: String): String;
var
  Counter: Int64 = 0;
begin
  Result:= FileName + ExtensionSeparator + FileExt;
  if FileExists(Result) then
  repeat
    Inc(Counter);
    Result:= FileName + IntToStr(Counter) + ExtensionSeparator + FileExt;
  until not FileExists(Result);
end;

function FileMove(const OldName, NewName: String; Replace: Boolean): Boolean;
const
  dwFlags: array[Boolean] of DWORD = (0, MOVEFILE_REPLACE_EXISTING);
begin
  Result:= MoveFileExW(PWideChar(UTF8Decode(OldName)), PWideChar(UTF8Decode(NewName)),
                       dwFlags[Replace] or MOVEFILE_COPY_ALLOWED);
end;

function GUIDEquals(const GUID1, GUID2: TGUID): Boolean;
begin
  Result:= IsEqualGUID(GUID1, GUID2);
end;

class function JclSysUtils.LoadModule(var Module: TModuleHandle; FileName: String): Boolean;
begin
  Module:= LoadLibraryW(PWideChar(UTF8Decode(FileName)));
  Result:= Module <> INVALID_MODULEHANDLE_VALUE;
end;

function GetModuleSymbol(Module: TModuleHandle; SymbolName: String): Pointer;
begin
  Result:= GetProcAddress(Module, PAnsiChar(SymbolName));
end;

class procedure JclSysUtils.UnloadModule(var Module: TModuleHandle);
begin
  if Module <> INVALID_MODULEHANDLE_VALUE then
  begin
    FreeLibrary(Module);
    Module:= INVALID_MODULEHANDLE_VALUE;
  end;
end;

procedure StrTokenToStrings(const Token: String; Separator: AnsiChar; var Strings: TStrings);
var
  Start: Integer = 1;
  Len, Finish: Integer;
begin
  Len:= Length(Token);
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for Finish:= 1 to Len - 1 do
    begin
      if Token[Finish] = Separator then
      begin
        Strings.Add(Copy(Token, Start, Finish - Start));
        Start:= Finish + 1;
      end;
    end;
    if Start <= Len then
    begin
      Strings.Add(Copy(Token, Start, Len - Start + 1));
    end;
  finally
    Strings.EndUpdate;
  end;
end;

function FileExists(const FileName: String): Boolean;
begin
  Result:= FileExistsUTF8(FileName);
end;

function CreateFile(lpFileName: LPCSTR; dwDesiredAccess: DWORD; dwShareMode: DWORD; lpSecurityAttributes: LPSECURITY_ATTRIBUTES;
                    dwCreationDisposition: DWORD; dwFlagsAndAttributes: DWORD; hTemplateFile: HANDLE): HANDLE;
begin
  Result:= CreateFileW(PWideChar(UTF8Decode(lpFileName)), dwDesiredAccess, dwShareMode,
                       lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
end;

function GetFileAttributesEx(lpFileName: LPCSTR; fInfoLevelId: TGET_FILEEX_INFO_LEVELS; lpFileInformation: Pointer): BOOL;
begin
  Result:= GetFileAttributesExW(PWideChar(UTF8Decode(lpFileName)), fInfoLevelId, lpFileInformation);
end;

{ TJclDynamicSplitStream }

function TJclDynamicSplitStream.LoadVolume: Boolean;
begin
  Result:= Assigned(FVolume);
  if not Result then
  begin
    FVolume:= GetVolume(0);
    GetVolumeMaxSize(0);
    Result := Assigned(FVolume);
    if Result then FVolume.Seek(0, soBeginning);
  end;
end;

function TJclDynamicSplitStream.GetVolume(Index: Integer): TStream;
begin
  if Assigned(FOnVolume) then
    Result:= FOnVolume(Index)
  else begin
    Result:= nil;
  end;
end;

function TJclDynamicSplitStream.GetVolumeMaxSize(Index: Integer): Int64;
begin
  if Assigned(FOnVolumeMaxSize) then
    Result:= FOnVolumeMaxSize(Index)
  else begin
    Result:= 0;
  end;
end;

function TJclDynamicSplitStream.GetSize: Int64;
begin
  if not LoadVolume then
    Result:= 0
  else begin
    Result:= FVolume.Size;
  end;
end;

procedure TJclDynamicSplitStream.SetSize(const NewSize: Int64);
begin
  if LoadVolume then FVolume.Size:= NewSize;
end;

constructor TJclDynamicSplitStream.Create(ADummy: Boolean);
begin
  inherited Create;
end;

function TJclDynamicSplitStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if not LoadVolume then
    Result:= 0
  else begin
    Result:= FVolume.Seek(Offset, Origin);
  end;
end;

function TJclDynamicSplitStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  if not LoadVolume then
    Result:= 0
  else begin
    Result:= FVolume.Read(Buffer, Count);
  end;
end;

function TJclDynamicSplitStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  if not LoadVolume then
    Result:= 0
  else begin
    Result:= FVolume.Write(Buffer, Count);
  end;
end;

{ TJclWideStringList }

procedure TJclWideStringList.SetCaseSensitive(AValue: Boolean);
begin
  if FCaseSensitive <> AValue then
  begin
    FCaseSensitive:= AValue;
    if FCaseSensitive then
      OnKeyPtrCompare := @CompareWideStringProc
    else begin
      OnKeyPtrCompare := @CompareTextWideStringProc;
    end;
    if Sorted then Sort;
  end;
end;

function TJclWideStringList.Get(Index: Integer): WideString;
begin
  Result := Keys[Index];
end;

function TJclWideStringList.GetObject(Index: Integer): TObject;
begin
  Result := Data[Index];
end;

procedure TJclWideStringList.Put(Index: Integer; const S: WideString);
begin
  Keys[Index] := S;
end;

procedure TJclWideStringList.PutObject(Index: Integer; AObject: TObject);
begin
  Data[Index] := AObject;
end;

function TJclWideStringList.CompareWideStringProc(Key1, Key2: Pointer): Integer;
begin
{$if FPC_FULLVERSION<30002}
  Result:= WideStringManager.CompareWideStringProc(WideString(Key1^), WideString(Key2^));
{$else}
  Result:= WideStringManager.CompareWideStringProc(WideString(Key1^), WideString(Key2^), []);
{$endif}
end;

function TJclWideStringList.CompareTextWideStringProc(Key1, Key2: Pointer): Integer;
begin
{$if FPC_FULLVERSION<30002}
  Result:= WideStringManager.CompareTextWideStringProc(WideString(Key1^), WideString(Key2^));
{$else}
  Result:= WideStringManager.CompareWideStringProc(WideString(Key1^), WideString(Key2^), [coIgnoreCase]);
{$endif}
end;

constructor TJclWideStringList.Create;
begin
  inherited Create;
  OnKeyPtrCompare := @CompareTextWideStringProc;
end;

function TJclWideStringList.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  Result:= inherited Add(S, AObject);
end;

end.

