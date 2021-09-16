{
  Double Commander
  -------------------------------------------------------------------------
  SevenZip archiver plugin

  Copyright (C) 2014 Alexander Koblov (alexx2000@mail.ru)

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

unit SevenZipAdv;

{$mode delphi}

interface

uses
  Classes, SysUtils, SevenZip, JclCompression;

type
  TBytes = array of Byte;
  TCardinalArray = array of Cardinal;
  TJclCompressionArchiveClassArray = array of TJclCompressionArchiveClass;

type

   { TArchiveFormat }

   TArchiveFormat = class
     Name: UnicodeString;
     Extension: UnicodeString;
     AddExtension: UnicodeString;
     Update: WordBool;
     KeepName: WordBool;
     ClassID: TGUID;
     StartSignature: TBytes;
   end;

   { TJclXzCompressArchiveEx }

   TJclXzCompressArchiveEx = class(TJclSevenzipCompressArchive)
   public
     class function ArchiveExtensions: string; override;
     class function ArchiveName: string; override;
     class function ArchiveSubExtensions: string; override;
     class function ArchiveCLSID: TGUID; override;
   end;

   { TJclSevenzipUpdateArchiveHelper }

   TJclSevenzipUpdateArchiveHelper = class helper for TJclSevenzipUpdateArchive
     procedure RemoveDirectory(const PackedName: WideString); overload;
   end;

  { TJclSevenzipDecompressArchiveHelper }

  TJclSevenzipDecompressArchiveHelper = class helper for TJclSevenzipDecompressArchive
    procedure ProcessSelected(const SelectedArray: TCardinalArray; Verify: Boolean);
  end;

function FindUpdateFormats(const AFileName: TFileName): TJclUpdateArchiveClassArray;
function FindCompressFormats(const AFileName: TFileName): TJclCompressArchiveClassArray;
function FindDecompressFormats(const AFileName: TFileName): TJclDecompressArchiveClassArray;

function GetNestedArchiveName(const ArchiveName: String; Item: TJclCompressionItem): WideString;
function ExpandEnvironmentStrings(const FileName: UnicodeString): UnicodeString;
function WideExtractFilePath(const FileName: WideString): WideString;
function GetModulePath(out ModulePath: AnsiString): Boolean;

implementation

uses
  CTypes, ActiveX, Windows, LazFileUtils, LazUTF8;

type
  TArchiveFormats = array of TArchiveFormat;
  TJclSevenzipUpdateArchiveClass = class of TJclSevenzipUpdateArchive;
  TJclSevenzipCompressArchiveClass = class of TJclSevenzipCompressArchive;
  TJclSevenzipDecompressArchiveClass = class of TJclSevenzipDecompressArchive;
  TJclArchiveType = (atUpdateArchive, atCompressArchive, atDecompressArchive);

type
  TArchiveFormatCache = record
    ArchiveName: String;
    ArchiveClassArray: TJclCompressionArchiveClassArray;
  end;

var
  Mutex: TRTLCriticalSection;
  ArchiveFormatsX: TArchiveFormats;

var
  UpdateFormatsCache: TArchiveFormatCache;
  CompressFormatsCache: TArchiveFormatCache;
  DecompressFormatsCache: TArchiveFormatCache;

function _wcsnicmp(const s1, s2: pwidechar; count: csize_t): cint; cdecl; external 'msvcrt.dll';

function ReadStringProp(FormatIndex: Cardinal; PropID: TPropID;
  out Value: UnicodeString): LongBool;
var
  PropSize: Cardinal;
  PropValue: TPropVariant;
begin
  Result:= Succeeded(GetHandlerProperty2(FormatIndex, PropID, PropValue));
  Result:= Result and (PropValue.vt = VT_BSTR);
  if Result then
  try
    PropSize:= SysStringByteLen(PropValue.bstrVal);
    SetLength(Value, PropSize div SizeOf(WideChar));
    CopyMemory(PWideChar(Value), PropValue.bstrVal, PropSize);
  finally
    SysFreeString(PropValue.bstrVal);
  end;
end;

{$OPTIMIZATION OFF}
function ReadBooleanProp(FormatIndex: Cardinal;
  PropID: TPropID; out Value: WordBool): LongBool;
var
  PropValue: TPropVariant;
begin
  Result:= Succeeded(GetHandlerProperty2(FormatIndex, PropID, PropValue));
  Result:= Result and (PropValue.vt = VT_BOOL);
  if Result then Value:= PropValue.boolVal;
end;
{$OPTIMIZATION DEFAULT}

procedure LoadArchiveFormats(var ArchiveFormats: TArchiveFormats);
var
  Idx: Integer = 0;
  PropSize: Cardinal;
  PropValue: TPropVariant;
  ArchiveFormat: TArchiveFormat;
  Index, NumberOfFormats: Cardinal;
begin
  if (not Is7ZipLoaded) and (not Load7Zip) then Exit;

  if not Succeeded(GetNumberOfFormats(@NumberOfFormats)) then
    Exit;
  SetLength(ArchiveFormats, NumberOfFormats);
  for Index := Low(ArchiveFormats) to High(ArchiveFormats) do
  begin
    // Archive format GUID
    if Succeeded(GetHandlerProperty2(Index, kClassID, PropValue)) then
    begin
      if PropValue.vt = VT_BSTR then
      try
        if SysStringByteLen(PropValue.bstrVal) <> SizeOf(TGUID) then
          Continue
        else begin
          ArchiveFormat:= TArchiveFormat.Create;
          ArchiveFormat.ClassID:= PGUID(PropValue.bstrVal)^;
        end;
      finally
        SysFreeString(PropValue.bstrVal);
      end;
    end;
    // Archive format signature
    if Succeeded(GetHandlerProperty2(Index, kStartSignature, PropValue)) then
    begin
      if PropValue.vt = VT_BSTR then
      try
        PropSize:= SysStringByteLen(PropValue.bstrVal);
        if (PropSize > 0) then
        begin
          SetLength(ArchiveFormat.StartSignature, PropSize);
          CopyMemory(@ArchiveFormat.StartSignature[0], PropValue.bstrVal, PropSize);
        end;
      finally
        SysFreeString(PropValue.bstrVal);
      end;
    end;

    ReadStringProp(Index, kArchiveName, ArchiveFormat.Name);
    ReadStringProp(Index, kExtension, ArchiveFormat.Extension);
    ReadStringProp(Index, kAddExtension, ArchiveFormat.AddExtension);
    ReadBooleanProp(Index, kUpdate, ArchiveFormat.Update);
    ReadBooleanProp(Index, kKeepName, ArchiveFormat.KeepName);

    ArchiveFormats[Idx]:= ArchiveFormat;
    Inc(Idx);
  end;
  SetLength(ArchiveFormats, Idx);
end;

function Contains(const ArrayToSearch: TJclCompressionArchiveClassArray; const ArchiveClass: TJclCompressionArchiveClass): Boolean;
var
  Index: Integer;
begin
  for Index := Low(ArrayToSearch) to High(ArrayToSearch) do
    if ArrayToSearch[Index] = ArchiveClass then
      Exit(True);
  Result := False;
end;

function FindArchiveFormat(const ClassID: TGUID; ArchiveType: TJclArchiveType): TJclCompressionArchiveClass;
var
  Index: Integer;
  UpdateClass: TJclSevenzipUpdateArchiveClass;
  CompressClass: TJclSevenzipCompressArchiveClass;
  DecompressClass: TJclSevenzipDecompressArchiveClass;
begin
  case ArchiveType of
    atUpdateArchive:
      for Index:= 0 to GetArchiveFormats.UpdateFormatCount - 1 do
      begin
        UpdateClass:= TJclSevenzipUpdateArchiveClass(GetArchiveFormats.UpdateFormats[Index]);
        if IsEqualGUID(ClassID, UpdateClass.ArchiveCLSID) then
          Exit(GetArchiveFormats.UpdateFormats[Index]);
      end;
    atCompressArchive:
      for Index:= 0 to GetArchiveFormats.CompressFormatCount - 1 do
      begin
        CompressClass:= TJclSevenzipCompressArchiveClass(GetArchiveFormats.CompressFormats[Index]);
        if IsEqualGUID(ClassID, CompressClass.ArchiveCLSID) then
          Exit(GetArchiveFormats.CompressFormats[Index]);
      end;
    atDecompressArchive:
      for Index:= 0 to GetArchiveFormats.DecompressFormatCount - 1 do
      begin
        DecompressClass:= TJclSevenzipDecompressArchiveClass(GetArchiveFormats.DecompressFormats[Index]);
        if IsEqualGUID(ClassID, DecompressClass.ArchiveCLSID) then
          Exit(GetArchiveFormats.DecompressFormats[Index]);
      end;
  end;
  Result:= nil;
end;

procedure FindArchiveFormats(const AFileName: TFileName; ArchiveType: TJclArchiveType; var Result: TJclCompressionArchiveClassArray);
const
  BufferSize = 524288;
var
  AFile: THandle;
  Buffer: TBytes;
  Idx, Index: Integer;
  ArchiveFormat: TArchiveFormat;
  ArchiveClass: TJclCompressionArchiveClass;
begin
  if Length(ArchiveFormatsX) = 0 then LoadArchiveFormats(ArchiveFormatsX);

  AFile:= FileOpenUTF8(AFileName, fmOpenRead or fmShareDenyNone);
  if AFile = feInvalidHandle then Exit;
  try
    SetLength(Buffer, BufferSize);
    if FileRead(AFile, Buffer[0], BufferSize) = 0 then
      Exit;
  finally
    FileClose(AFile);
  end;

  for Index := Low(ArchiveFormatsX) to High(ArchiveFormatsX) do
  begin
    ArchiveFormat:= ArchiveFormatsX[Index];

    if (not ArchiveFormat.Update) and (ArchiveType in [atUpdateArchive, atCompressArchive]) then
      Continue;

    // Skip container types
    if IsEqualGUID(ArchiveFormat.ClassID, CLSID_CFormatPe) then Continue;
    if IsEqualGUID(ArchiveFormat.ClassID, CLSID_CFormatIso) then Continue;
    if IsEqualGUID(ArchiveFormat.ClassID, CLSID_CFormatUdf) then Continue;

    if Length(ArchiveFormat.StartSignature) = 0 then Continue;
    for Idx:= 0 to Pred(BufferSize) - Length(ArchiveFormat.StartSignature) do
    begin
      if CompareMem(@Buffer[Idx], @ArchiveFormat.StartSignature[0], Length(ArchiveFormat.StartSignature)) then
      begin
        ArchiveClass:= FindArchiveFormat(ArchiveFormat.ClassID, ArchiveType);
        if Assigned(ArchiveClass) and not Contains(Result, ArchiveClass) then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := ArchiveClass;
        end;
        Break;
      end;
    end;
  end;
end;

function FindUpdateFormats(const AFileName: TFileName): TJclUpdateArchiveClassArray;
var
  ArchiveClassArray: TJclCompressionArchiveClassArray absolute Result;
begin
  System.EnterCriticalSection(Mutex);
  try
    // Try to find archive type in cache
    if UpdateFormatsCache.ArchiveName = AFileName then
      Exit(TJclUpdateArchiveClassArray(UpdateFormatsCache.ArchiveClassArray))
    else begin
      UpdateFormatsCache.ArchiveName:= AFileName;
      SetLength(UpdateFormatsCache.ArchiveClassArray, 0);
    end;

    Result:= GetArchiveFormats.FindUpdateFormats(AFileName);

    FindArchiveFormats(AFileName, atUpdateArchive, ArchiveClassArray);

    // Save archive type in cache
    UpdateFormatsCache.ArchiveClassArray:= ArchiveClassArray;
  finally
    System.LeaveCriticalSection(Mutex);
  end;
end;

function FindCompressFormats(const AFileName: TFileName): TJclCompressArchiveClassArray;
var
  ArchiveClassArray: TJclCompressionArchiveClassArray absolute Result;
begin
  System.EnterCriticalSection(Mutex);
  try
    // Try to find archive type in cache
    if CompressFormatsCache.ArchiveName = AFileName then
      Exit(TJclCompressArchiveClassArray(CompressFormatsCache.ArchiveClassArray))
    else begin
      CompressFormatsCache.ArchiveName:= AFileName;
      SetLength(CompressFormatsCache.ArchiveClassArray, 0);
    end;

    Result:= GetArchiveFormats.FindCompressFormats(AFileName);

    FindArchiveFormats(AFileName, atCompressArchive, ArchiveClassArray);

    // Save archive type in cache
    CompressFormatsCache.ArchiveClassArray:= ArchiveClassArray;
  finally
    System.LeaveCriticalSection(Mutex);
  end;
end;

function FindDecompressFormats(const AFileName: TFileName): TJclDecompressArchiveClassArray;
var
  ArchiveClassArray: TJclCompressionArchiveClassArray absolute Result;
begin
  System.EnterCriticalSection(Mutex);
  try
    // Try to find archive type in cache
    if DecompressFormatsCache.ArchiveName = AFileName then
      Exit(TJclDecompressArchiveClassArray(DecompressFormatsCache.ArchiveClassArray))
    else begin
      DecompressFormatsCache.ArchiveName:= AFileName;
      SetLength(DecompressFormatsCache.ArchiveClassArray, 0);
    end;

    Result:= GetArchiveFormats.FindDecompressFormats(AFileName);

    FindArchiveFormats(AFileName, atDecompressArchive, ArchiveClassArray);

    // Save archive type in cache
    DecompressFormatsCache.ArchiveClassArray:= ArchiveClassArray;
  finally
    System.LeaveCriticalSection(Mutex);
  end;
end;

{ TJclXzCompressArchiveEx }

class function TJclXzCompressArchiveEx.ArchiveExtensions: string;
begin
  Result:= TJclXzCompressArchive.ArchiveExtensions;
end;

class function TJclXzCompressArchiveEx.ArchiveName: string;
begin
  Result:= TJclXzCompressArchive.ArchiveName;
end;

class function TJclXzCompressArchiveEx.ArchiveSubExtensions: string;
begin
  Result:= TJclXzCompressArchive.ArchiveSubExtensions;
end;

class function TJclXzCompressArchiveEx.ArchiveCLSID: TGUID;
begin
  Result:= TJclXzCompressArchive.ArchiveCLSID;
end;

{ TJclSevenzipUpdateArchiveHelper }

procedure TJclSevenzipUpdateArchiveHelper.RemoveDirectory(const PackedName: WideString);
var
  DirectoryName: WideString;
  AItem: TJclCompressionItem;
  Index, PackedNamesIndex: Integer;
begin
  DirectoryName:= Copy(PackedName, 1, Length(PackedName) - 1);
  // Remove directory
  for Index := 0 to ItemCount - 1 do
  begin
    AItem := Items[Index];
    // Can be with or without path delimiter at end
    if WideSameText(AItem.PackedName, PackedName) or WideSameText(AItem.PackedName, DirectoryName) then
    begin
      FItems.Delete(Index);
      PackedNamesIndex := -1;
      if (FPackedNames <> nil) and FPackedNames.Find(PackedName, PackedNamesIndex) then
        FPackedNames.Delete(PackedNamesIndex);
      Break;
    end;
  end;
  // Remove directory content
  for Index := ItemCount - 1 downto 0 do
  begin
    if (_wcsnicmp(PWideChar(PackedName), PWideChar(Items[Index].PackedName), Length(PackedName)) = 0) then
    begin
      if (FPackedNames <> nil) and FPackedNames.Find(Items[Index].PackedName, PackedNamesIndex) then
        FPackedNames.Delete(PackedNamesIndex);
      FItems.Delete(Index);
    end;
  end;
end;

{ TJclSevenzipDecompressArchiveHelper }

procedure TJclSevenzipDecompressArchiveHelper.ProcessSelected(const SelectedArray: TCardinalArray; Verify: Boolean);
var
  AExtractCallback: IArchiveExtractCallback;
begin
  CheckNotDecompressing;

  FDecompressing := True;
  AExtractCallback := TJclSevenzipExtractCallback.Create(Self);
  try
    OpenArchive;

    SevenzipCheck(InArchive.Extract(@SelectedArray[0], Length(SelectedArray), Cardinal(Verify), AExtractCallback));
    CheckOperationSuccess;
  finally
    FDestinationDir := '';
    FDecompressing := False;
    AExtractCallback := nil;
  end;
end;

function GetNestedArchiveName(const ArchiveName: String; Item: TJclCompressionItem): WideString;
var
  Extension: String;
begin
  Result:= Item.NestedArchiveName;
  Extension:= LowerCase(ExtractFileExt(ArchiveName));
  if (Extension = '.tbz') or (Extension = '.tgz') or (Extension = '.txz') then
  begin
    Result:= Result + '.tar';
  end;
end;

function ExpandEnvironmentStrings(const FileName: UnicodeString): UnicodeString;
var
  dwSize: DWORD;
begin
  SetLength(Result, MAX_PATH + 1);
  dwSize:= ExpandEnvironmentStringsW(PWideChar(FileName), PWideChar(Result), MAX_PATH);
  if dwSize > 0 then SetLength(Result, dwSize - 1);
end;

function WideExtractFilePath(const FileName: WideString): WideString;
var
  Index: Integer;
begin
  for Index:= Length(FileName) downto 1 do
  case FileName[Index] of
    PathDelim: Exit(Copy(FileName, 1, Index));
  end;
  Result:= EmptyWideStr;
end;

function GetModulePath(out ModulePath: AnsiString): Boolean;
var
  lpBuffer: TMemoryBasicInformation;
  ModuleName: array[0..MAX_PATH] of WideChar;
begin
  Result:= VirtualQuery(@GetModulePath, @lpBuffer, SizeOf(lpBuffer)) = SizeOf(lpBuffer);
  if Result then
  begin
    Result:= GetModuleFileNameW(THandle(lpBuffer.AllocationBase), ModuleName, MAX_PATH) > 0;
    if Result then
    begin
      ModulePath:= ExtractFilePath(Utf16ToUtf8(WideString(ModuleName)));
    end;
  end;
end;

initialization
  InitCriticalSection(Mutex);

finalization
  DoneCriticalSection(Mutex);

end.

