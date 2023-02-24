unit ZipOpt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, AbUtils, AbZipTyp;

type
  TArchiveFormat = (
    afNil,
    afZip,
    afZipx,
    afGzip,
    afBzip2,
    afXzip,
    afLzma,
    afZstd
  );

  TFormatOptions = record
    Level: PtrInt;
    Method: PtrInt;
  end;

const
  ARCHIVE_FORMAT: array[TAbArchiveType] of TArchiveFormat =
  (
    afNil, afZip, afZip, afZip, afNil, afGzip, afGzip, afNil,
    afBzip2, afBzip2, afXzip, afXzip, afLzma, afLzma, afZstd, afZstd
  );

const
  DefaultConfig: array[TArchiveFormat] of TFormatOptions =
  (
   (Level: 0; Method: 0;),
   (Level: 6; Method: PtrInt(cmDeflated);),
   (Level: 7; Method: PtrInt(cmXz);),
   (Level: 6; Method: PtrInt(cmDeflated);),
   (Level: 9; Method: PtrInt(cmBzip2);),
   (Level: 7; Method: PtrInt(cmXz);),
   (Level: 7; Method: PtrInt(cmLZMA);),
   (Level: 11; Method: PtrInt(cmZstd);)
  );

var
  PluginConfig: array[TArchiveFormat] of TFormatOptions;

procedure LoadConfiguration;
procedure SaveConfiguration;

implementation

uses
  TypInfo, DCClassesUtf8, Extension, ZipFunc;

procedure LoadConfiguration;
var
  Ini: TIniFileEx;
  Section: AnsiString;
  ArchiveFormat: TArchiveFormat;
begin
  try
    Ini:= TIniFileEx.Create(gStartupInfo.PluginConfDir + IniFileName);
    try
      for ArchiveFormat:= Succ(Low(TArchiveFormat)) to High(TArchiveFormat) do
      begin
        Section:= Copy(GetEnumName(TypeInfo(TArchiveFormat), PtrInt(ArchiveFormat)), 3, MaxInt);
        PluginConfig[ArchiveFormat].Level:= Ini.ReadInteger(Section, 'Level', DefaultConfig[ArchiveFormat].Level);
        PluginConfig[ArchiveFormat].Method:= Ini.ReadInteger(Section, 'Method', DefaultConfig[ArchiveFormat].Method);
      end;
      gTarAutoHandle:= Ini.ReadBool('Configuration', 'TarAutoHandle', True);
      // Backward compatibility
      case Ini.ReadInteger('Configuration', 'DeflationOption', -1) of
        IntPtr(doSuperFast): PluginConfig[afZip].Level:= 1;
        IntPtr(doFast): PluginConfig[afZip].Level:= 3;
        IntPtr(doNormal): PluginConfig[afZip].Level:= 6;
        IntPtr(doMaximum): PluginConfig[afZip].Level:= 9;
      end;
      case Ini.ReadInteger('Configuration', 'CompressionMethodToUse', -1) of
        IntPtr(smStored): PluginConfig[afZip].Method:= IntPtr(cmStored);
        IntPtr(smDeflated): PluginConfig[afZip].Method:= IntPtr(cmDeflated);
        IntPtr(smBestMethod): PluginConfig[afZip].Method:= IntPtr(cmEnhancedDeflated);
      end;
    finally
      Ini.Free;
    end;
  except
    // Ignore
  end;
end;

procedure SaveConfiguration;
var
  Ini: TIniFileEx;
  Section: AnsiString;
  ArchiveFormat: TArchiveFormat;
begin
  try
    Ini:= TIniFileEx.Create(gStartupInfo.PluginConfDir + IniFileName);
    try
      for ArchiveFormat:= Succ(Low(TArchiveFormat)) to High(TArchiveFormat) do
      begin
        Section:= Copy(GetEnumName(TypeInfo(TArchiveFormat), PtrInt(ArchiveFormat)), 3, MaxInt);
        Ini.WriteInteger(Section, 'Level', PluginConfig[ArchiveFormat].Level);
        Ini.WriteInteger(Section, 'Method', PluginConfig[ArchiveFormat].Method);
      end;
      Ini.DeleteKey('Configuration', 'DeflationOption');
      Ini.DeleteKey('Configuration', 'CompressionMethodToUse');
      Ini.WriteBool('Configuration', 'TarAutoHandle', gTarAutoHandle);
      Ini.UpdateFile;
    finally
      Ini.Free;
    end;
  except
    on E: Exception do
    begin
      gStartupInfo.MessageBox(PAnsiChar(E.Message), nil, MB_OK or MB_ICONERROR);
    end;
  end;
end;

initialization
  Move(DefaultConfig[Low(DefaultConfig)], PluginConfig[Low(PluginConfig)], SizeOf(PluginConfig));

end.

