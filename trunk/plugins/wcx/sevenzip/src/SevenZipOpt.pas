{
  Double Commander
  -------------------------------------------------------------------------
  SevenZip archiver plugin, compression options

  Copyright (C) 2014-2017 Alexander Koblov (alexx2000@mail.ru)

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

unit SevenZipOpt;

{$mode delphi}

interface

uses
  Classes, SysUtils, Windows, IniFiles, JclCompression, SevenZip;

const
  cKilo = 1024;
  cMega = cKilo * cKilo;
  cGiga = cKilo * cKilo * cKilo;

const
  kNoSolidBlockSize = 0;
  kSolidBlockSize = 64;

const
  DeflateDict: array[0..0] of PtrInt =
  (
   cKilo * 32
  );

  Deflate64Dict: array[0..0] of PtrInt =
  (
   cKilo * 64
  );

  Bzip2Dict: array[0..8] of PtrInt =
  (
   cKilo * 100,
   cKilo * 200,
   cKilo * 300,
   cKilo * 400,
   cKilo * 500,
   cKilo * 600,
   cKilo * 700,
   cKilo * 800,
   cKilo * 900
  );

  LZMADict: array[0..21] of PtrInt =
  (
   cKilo * 64,
   cMega,
   cMega * 2,
   cMega * 3,
   cMega * 4,
   cMega * 6,
   cMega * 8,
   cMega * 12,
   cMega * 16,
   cMega * 24,
   cMega * 32,
   cMega * 48,
   cMega * 64,
   cMega * 96,
   cMega * 128,
   cMega * 192,
   cMega * 256,
   cMega * 384,
   cMega * 512,
   cMega * 768,
   cMega * 1024,
   cMega * 1536
  );

  PPMdDict: array[0..19] of PtrInt =
  (
   cMega,
   cMega * 2,
   cMega * 3,
   cMega * 4,
   cMega * 6,
   cMega * 8,
   cMega * 12,
   cMega * 16,
   cMega * 24,
   cMega * 32,
   cMega * 48,
   cMega * 64,
   cMega * 96,
   cMega * 128,
   cMega * 192,
   cMega * 256,
   cMega * 384,
   cMega * 512,
   cMega * 768,
   cMega * 1024
  );

  DeflateWordSize: array[0..11] of PtrInt =
    (8, 12, 16, 24, 32, 48, 64, 96, 128, 192, 256, 258);

  Deflate64WordSize: array[0..11] of PtrInt =
    (8, 12, 16, 24, 32, 48, 64, 96, 128, 192, 256, 257);

  LZMAWordSize: array[0..11] of PtrInt =
    (8, 12, 16, 24, 32, 48, 64, 96, 128, 192, 256, 273);

  PPMdWordSize: array[0..14] of PtrInt =
    (2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16);

  // Stored as block size / 1024
  SolidBlock: array[0..16] of PtrInt =
  (
    cKilo,
    cKilo * 2,
    cKilo * 4,
    cKilo * 8,
    cKilo * 16,
    cKilo * 32,
    cKilo * 64,
    cKilo * 128,
    cKilo * 256,
    cKilo * 512,
    cMega,
    cMega * 2,
    cMega * 4,
    cMega * 8,
    cMega * 16,
    cMega * 32,
    cMega * 64
  );

const
  TargetCPU = {$I %FPCTARGETCPU%}; // Target CPU of FPC

type

  TCompressionLevel =
  (
    clStore    = 0,
    clFastest  = 1,
    clFast     = 3,
    clNormal   = 5,
    clMaximum  = 7,
    clUltra    = 9
  );

  TArchiveFormat = (afSevenZip, afBzip2, afGzip, afTar, afWim, afXz, afZip);

  PPasswordData = ^TPasswordData;
  TPasswordData = record
    EncryptHeader: Boolean;
    Password: array[0..MAX_PATH] of WideChar;
  end;

  TFormatOptions = record
    Level: PtrInt;
    Method: PtrInt;
    Dictionary: PtrInt;
    WordSize: PtrInt;
    SolidSize: PtrInt;
    ThreadCount: PtrInt;
    ArchiveCLSID: PGUID;
    Parameters: WideString;
  end;

function GetNumberOfProcessors: LongWord;
function FormatFileSize(ASize: Int64; AGiga: Boolean = True): String;

procedure SetArchiveOptions(AJclArchive: IInterface);

procedure LoadConfiguration;
procedure SaveConfiguration;

const
  DefaultIniName = 'sevenzip.ini';

var
  ConfigFile: AnsiString;
  LibraryPath: AnsiString;

const
  ArchiveExtension: array[TArchiveFormat] of WideString =
  ('7z', 'bzip2', 'gzip', 'tar', 'wim', 'xz', 'zip');

const
  MethodName: array [TJclCompressionMethod] of WideString =
  (kCopyMethodName, kDeflateMethodName, kDeflate64MethodName,
   kBZip2MethodName, kLZMAMethodName, kLZMA2MethodName, kPPMdMethodName);

const
  DefaultConfig: array[TArchiveFormat] of TFormatOptions =
  (
   (Level: PtrInt(clNormal); Method: PtrInt(cmLZMA2); Dictionary: cMega * 16; WordSize: 32; SolidSize: cMega * 2; ThreadCount: 2; ArchiveCLSID: @CLSID_CFormat7z; Parameters: '';),
   (Level: PtrInt(clNormal); Method: PtrInt(cmBZip2); Dictionary: cKilo * 900; WordSize: 0; SolidSize: 0; ThreadCount: 2; ArchiveCLSID: @CLSID_CFormatBZ2; Parameters: '';),
   (Level: PtrInt(clNormal); Method: PtrInt(cmDeflate); Dictionary: cKilo * 32; WordSize: 32; SolidSize: 0; ThreadCount: 1; ArchiveCLSID: @CLSID_CFormatGZip; Parameters: '';),
   (Level: PtrInt(clStore); Method: 0; Dictionary: 0; WordSize: 0; SolidSize: 0; ThreadCount: 1; ArchiveCLSID: @CLSID_CFormatTar; Parameters: '';),
   (Level: PtrInt(clStore); Method: 0; Dictionary: 0; WordSize: 0; SolidSize: 0; ThreadCount: 1; ArchiveCLSID: @CLSID_CFormatWim; Parameters: '';),
   (Level: PtrInt(clNormal); Method: PtrInt(cmLZMA2); Dictionary: cMega * 16; WordSize: 32; SolidSize: 0; ThreadCount: 2; ArchiveCLSID: @CLSID_CFormatXz; Parameters: '';),
   (Level: PtrInt(clNormal); Method: PtrInt(cmDeflate); Dictionary: cKilo * 32; WordSize: 32; SolidSize: 0; ThreadCount: 2; ArchiveCLSID: @CLSID_CFormatZip; Parameters: '';)
  );

var
  PluginConfig: array[TArchiveFormat] of TFormatOptions;

implementation

uses
  ActiveX, LazUTF8, SevenZipAdv, SevenZipCodecs;

function GetNumberOfProcessors: LongWord;
var
  SystemInfo: TSYSTEMINFO;
begin
  GetSystemInfo(@SystemInfo);
  Result:= SystemInfo.dwNumberOfProcessors;
end;

function FormatFileSize(ASize: Int64; AGiga: Boolean): String;
begin
  if ((ASize div cGiga) > 0) and AGiga then
    Result:= IntToStr(ASize div cGiga) + ' GB'
  else
  if (ASize div cMega) >0 then
    Result:= IntToStr(ASize div cMega) + ' MB'
  else
  if (ASize div cKilo) > 0 then
    Result:= IntToStr(ASize div cKilo) + ' KB'
  else
    Result:= IntToStr(ASize);
end;

procedure SetArchiveCustom(AJclArchive: IInterface; AFormat: TArchiveFormat);
var
  Index: Integer;
  Start: Integer = 1;
  Parameters: WideString;
  MethodStandard: Boolean;
  Method: TJclCompressionMethod;
  JclArchive: TJclCompressionArchive;

  procedure AddProperty(const Name: WideString; const Value: TPropVariant);
  begin
    with JclArchive do
    begin
      SetLength(PropNames, Length(PropNames) + 1);
      PropNames[High(PropNames)] := Name;
      SetLength(PropValues, Length(PropValues) + 1);
      PropValues[High(PropValues)] := Value;
    end;
  end;

  procedure AddCardinalProperty(const Name: WideString; Value: Cardinal);
  var
    PropValue: TPropVariant;
  begin
    PropValue.vt := VT_UI4;
    PropValue.ulVal := Value;
    AddProperty(Name, PropValue);
  end;

  procedure AddWideStringProperty(const Name: WideString; const Value: WideString);
  var
    PropValue: TPropVariant;
  begin
    PropValue.vt := VT_BSTR;
    PropValue.bstrVal := SysAllocString(PWideChar(Value));
    AddProperty(Name, PropValue);
  end;

  procedure AddOption(Finish: Integer);
  var
    C: WideChar;
    IValue: Int64;
    PropValue: TPropVariant;
    Option, Value: WideString;
  begin
    Option:= Copy(Parameters, Start, Finish - Start);
    Start:= Pos('=', Option);
    if Start = 0 then
    begin
      PropValue.vt:= VT_EMPTY;
      C:= Option[Length(Option)];
      if C = '+' then
        Variant(PropValue):= True
      else if C = '-' then begin
        Variant(PropValue):= False;
      end;
      if (PropValue.vt <> VT_EMPTY) then
      begin
        Delete(Option, Length(Option), 1);
      end;
      AddProperty(Option, PropValue);
    end
    else begin
      Value:= Copy(Option, Start + 1, MaxInt);
      SetLength(Option, Start - 1);
      if TryStrToInt64(AnsiString(Value), IValue) then
        AddCardinalProperty(Option, IValue)
      else
        AddWideStringProperty(Option, Value);
    end;
  end;

begin
  JclArchive:= AJclArchive as TJclCompressionArchive;
  // Parse additional parameters
  Parameters:= Trim(PluginConfig[AFormat].Parameters);
  if Length(Parameters) > 0 then
  begin
    for Index:= 1 to Length(Parameters) do
    begin
      if Parameters[Index] = #32 then
      begin
        AddOption(Index);
        Start:= Index + 1;
      end;
    end;
    AddOption(MaxInt);
  end;
  Parameters:= WideUpperCase(Parameters);
  MethodStandard:= PluginConfig[AFormat].Method <= cmMaximum;
  // Set word size parameter
  if MethodStandard then
  begin
    Method:= TJclCompressionMethod(PluginConfig[AFormat].Method);
    case Method of
      cmLZMA, cmLZMA2,
      cmDeflate, cmDeflate64:
        begin
          if (Pos('FB=', Parameters) = 0) and (Pos('1=', Parameters) = 0) then
            AddCardinalProperty('fb', PluginConfig[AFormat].WordSize);
        end;
      cmPPMd:
        begin
          if Pos('O=', Parameters) = 0 then
            AddCardinalProperty('o', PluginConfig[AFormat].WordSize);
        end;
    end;
  end;
  // Set 7-zip compression method
  if IsEqualGUID(CLSID_CFormat7z, PluginConfig[AFormat].ArchiveCLSID^) then
  begin
    if Pos('0=', Parameters) = 0 then
    begin
      if MethodStandard then
        AddWideStringProperty('0', MethodName[Method])
      else begin
        AddWideStringProperty('0', GetCodecName(PluginConfig[AFormat].Method));
      end;
    end;
    if MethodStandard and (Method <> cmCopy) and (Pos('D=', Parameters) = 0) then begin
      AddWideStringProperty('D', WideString(IntToStr(PluginConfig[AFormat].Dictionary) + 'B'));
    end;
  end;
end;

procedure SetArchiveOptions(AJclArchive: IInterface);
var
  MethodStd: Boolean;
  ArchiveCLSID: TGUID;
  SolidBlockSize: Int64;
  Index: TArchiveFormat;
  Solid: IJclArchiveSolid;
  CompressHeader: IJclArchiveCompressHeader;
  DictionarySize: IJclArchiveDictionarySize;
  CompressionLevel: IJclArchiveCompressionLevel;
  MultiThreadStrategy: IJclArchiveNumberOfThreads;
  CompressionMethod: IJclArchiveCompressionMethod;
  SaveCreationDateTime: IJclArchiveSaveCreationDateTime;
  SaveLastAccessDateTime: IJclArchiveSaveLastAccessDateTime;
begin
  if AJclArchive is TJclSevenzipCompressArchive then
    ArchiveCLSID:= (AJclArchive as TJclSevenzipCompressArchive).ArchiveCLSID
  else begin
    ArchiveCLSID:= (AJclArchive as TJclSevenzipUpdateArchive).ArchiveCLSID
  end;
  for Index:= Low(PluginConfig) to High(PluginConfig) do
  begin
    if IsEqualGUID(ArchiveCLSID, PluginConfig[Index].ArchiveCLSID^) then
    begin
      MethodStd:= (PluginConfig[Index].Method <= cmMaximum);

      if MethodStd and Supports(AJclArchive, IJclArchiveCompressionMethod, CompressionMethod) and Assigned(CompressionMethod) then
        CompressionMethod.SetCompressionMethod(TJclCompressionMethod(PluginConfig[Index].Method));

      if Supports(AJclArchive, IJclArchiveCompressionLevel, CompressionLevel) and Assigned(CompressionLevel) then
        CompressionLevel.SetCompressionLevel(PluginConfig[Index].Level);

      if PluginConfig[Index].Level <> PtrInt(clStore) then
      begin
        if MethodStd and Supports(AJclArchive, IJclArchiveDictionarySize, DictionarySize) and Assigned(DictionarySize) then
          DictionarySize.SetDictionarySize(PluginConfig[Index].Dictionary);

        if Supports(AJclArchive, IJclArchiveSolid, Solid) and Assigned(Solid) then
        begin
          SolidBlockSize:= Int64(PluginConfig[Index].SolidSize);
          if SolidBlockSize <> kSolidBlockSize then SolidBlockSize:= SolidBlockSize * cKilo;
          Solid.SetSolidBlockSize(SolidBlockSize);
        end;

        if Supports(AJclArchive, IJclArchiveNumberOfThreads, MultiThreadStrategy) and Assigned(MultiThreadStrategy) then
          MultiThreadStrategy.SetNumberOfThreads(PluginConfig[Index].ThreadCount);

        if Supports(AJclArchive, IJclArchiveSaveCreationDateTime, SaveCreationDateTime) and Assigned(SaveCreationDateTime) then
          SaveCreationDateTime.SetSaveCreationDateTime(False);

        if Supports(AJclArchive, IJclArchiveSaveLastAccessDateTime, SaveLastAccessDateTime) and Assigned(SaveLastAccessDateTime) then
          SaveLastAccessDateTime.SetSaveLastAccessDateTime(False);

        if Supports(AJclArchive, IJclArchiveCompressHeader, CompressHeader) and Assigned(CompressHeader) then
          CompressHeader.SetCompressHeader(True);
      end;

      try
        SetArchiveCustom(AJclArchive, Index);
      except
        on E: Exception do
          Messagebox(0, PAnsiChar(E.Message), nil, MB_OK or MB_ICONERROR);
      end;

      Exit;
    end;
  end;
end;

procedure LoadConfiguration;
var
  Ini: TIniFile;
  Section: AnsiString;
  ArchiveFormat: TArchiveFormat;
begin
  try
    Ini:= TIniFile.Create(ConfigFile);
    try
      LibraryPath:= Ini.ReadString('Library', TargetCPU, EmptyStr);
      LibraryPath:= Utf16ToUtf8(ExpandEnvironmentStrings(UTF8Decode(LibraryPath)));
      for ArchiveFormat:= Low(TArchiveFormat) to High(TArchiveFormat) do
      begin
        Section:= GUIDToString(PluginConfig[ArchiveFormat].ArchiveCLSID^);
        PluginConfig[ArchiveFormat].Level:= Ini.ReadInteger(Section, 'Level', DefaultConfig[ArchiveFormat].Level);
        PluginConfig[ArchiveFormat].Method:= Ini.ReadInteger(Section, 'Method', DefaultConfig[ArchiveFormat].Method);
        PluginConfig[ArchiveFormat].Dictionary:= Ini.ReadInteger(Section, 'Dictionary', DefaultConfig[ArchiveFormat].Dictionary);
        PluginConfig[ArchiveFormat].WordSize:= Ini.ReadInteger(Section, 'WordSize', DefaultConfig[ArchiveFormat].WordSize);
        PluginConfig[ArchiveFormat].SolidSize:= Ini.ReadInteger(Section, 'SolidSize', DefaultConfig[ArchiveFormat].SolidSize);
        PluginConfig[ArchiveFormat].ThreadCount:= Ini.ReadInteger(Section, 'ThreadCount', DefaultConfig[ArchiveFormat].ThreadCount);
        PluginConfig[ArchiveFormat].Parameters:= Ini.ReadString(Section, 'Parameters', DefaultConfig[ArchiveFormat].Parameters);
      end;
    finally
      Ini.Free;
    end;
  except
    on E: Exception do
      MessageBox(0, PAnsiChar(E.Message), nil, MB_OK or MB_ICONERROR);
  end;
end;

procedure SaveConfiguration;
var
  Ini: TIniFile;
  Section: AnsiString;
  ArchiveFormat: TArchiveFormat;
begin
  try
    Ini:= TIniFile.Create(ConfigFile);
    try
      for ArchiveFormat:= Low(TArchiveFormat) to High(TArchiveFormat) do
      begin
        Section:= GUIDToString(PluginConfig[ArchiveFormat].ArchiveCLSID^);
        Ini.WriteString(Section, 'Format', ArchiveExtension[ArchiveFormat]);
        Ini.WriteInteger(Section, 'Level', PluginConfig[ArchiveFormat].Level);
        Ini.WriteInteger(Section, 'Method', PluginConfig[ArchiveFormat].Method);
        Ini.WriteInteger(Section, 'Dictionary', PluginConfig[ArchiveFormat].Dictionary);
        Ini.WriteInteger(Section, 'WordSize', PluginConfig[ArchiveFormat].WordSize);
        Ini.WriteInteger(Section, 'SolidSize', PluginConfig[ArchiveFormat].SolidSize);
        Ini.WriteInteger(Section, 'ThreadCount', PluginConfig[ArchiveFormat].ThreadCount);
        Ini.WriteString(Section, 'Parameters', PluginConfig[ArchiveFormat].Parameters);
      end;
    finally
      Ini.Free;
    end;
  except
    on E: Exception do
      MessageBox(0, PAnsiChar(E.Message), nil, MB_OK or MB_ICONERROR);
  end;
end;

initialization
  CopyMemory(@PluginConfig[Low(PluginConfig)],
             @DefaultConfig[Low(DefaultConfig)], SizeOf(PluginConfig));

end.

