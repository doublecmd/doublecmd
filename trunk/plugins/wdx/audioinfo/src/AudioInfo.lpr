library AudioInfo;

{$mode objfpc}{$H+}
{$include calling.inc}

uses
  FPCAdds, SysUtils, Classes, LazUTF8, WdxPlugin, AudioData, DCOSUtils;

const
  DETECT_STRING: String = '(EXT="MP3") | (EXT="MP2") | (EXT="MP1") | (EXT="OGG") | (EXT="WMA") | ' +
                          '(EXT="WAV") | (EXT="VQF") | (EXT="AAC") | (EXT="APE") | (EXT="MPC") | ' +
                          '(EXT="FLAC") | (EXT="CDA") | (EXT="TTA") | (EXT="AC3") | (EXT="DTS") | ' +
                          '(EXT="WV") | (EXT="WVC") | (EXT="OFR") | (EXT="OFS") | (EXT="M4A") | ' +
                          '(EXT="MP4")';

const
  FIELD_COUNT = 20;

  FIELD_NAME: array[0..Pred(FIELD_COUNT)] of String = (
    'Channels',
    'Duration',
    'Duration (H/M/S)',
    'Sample rate',
    'Bitrate',
    'Bitrate type',
    'Title',
    'Artist',
    'Album',
    'Track',
    'Track (zero-filled)',
    'Date',
    'Genre',
    'Comment',
    'Composer',
    'Copyright',
    'Link',
    'Encoder',
    'Tags',
    'Full text'
  );

  FIELD_TYPE: array[0..Pred(FIELD_COUNT)] of Integer = (
    ft_multiplechoice,
    ft_time,
    ft_string,
    ft_numeric_32,
    ft_numeric_32,
    ft_multiplechoice,
    ft_stringw,
    ft_stringw,
    ft_stringw,
    ft_numeric_32,
    ft_stringw,
    ft_stringw,
    ft_stringw,
    ft_stringw,
    ft_stringw,
    ft_stringw,
    ft_stringw,
    ft_stringw,
    ft_stringw,
    ft_fulltextw
  );

  FIELD_UNIT: array[0..Pred(FIELD_COUNT)] of String = (
    'Unknown|Mono|Stereo|Joint Stereo|Dual Channel', '', '', 'Hz|kHz',
    '', 'CBR|VBR|Unknown', '', '', '', '', '', '', '', '', '', '', '', '', '', '');

var
  DataAudio: TAudioData;
  CurrentFileName: String;

function ContentGetSupportedField(FieldIndex: Integer;
  FieldName, Units: PAnsiChar; MaxLen: Integer): Integer; dcpcall;
begin
  if (FieldIndex < 0) or (FieldIndex >= FIELD_COUNT) then
  begin
    Result := FT_NOMOREFIELDS;
    Exit;
  end;

  Result := FIELD_TYPE[FieldIndex];
  StrPLCopy(Units, FIELD_UNIT[FieldIndex], MaxLen - 1);
  StrPLCopy(FieldName, FIELD_NAME[FieldIndex], MaxLen - 1);
end;

function ContentGetValue(FileName: PAnsiChar; FieldIndex, UnitIndex: Integer;
  FieldValue: PByte; MaxLen, Flags: Integer): Integer; dcpcall;
begin
  Result:= ft_nosuchfield;
end;

function ContentGetValueW(FileName: PWideChar; FieldIndex, UnitIndex: Integer;
  FieldValue: PByte; MaxLen, Flags: Integer): Integer; dcpcall;
var
  Value: String;
  FileNameU: String;
  FullText: UnicodeString;
  ValueI: PInteger absolute FieldValue;
  Time: ptimeformat absolute FieldValue;
begin
  if (FieldIndex < 0) or (FieldIndex >= FIELD_COUNT) then
  begin
    Result:= ft_nosuchfield;
    Exit;
  end;

  FileNameU:= UTF16ToUTF8(UnicodeString(FileName));

  if not mbFileExists(FileNameU) then
  begin
    Result:= ft_fileerror;
    Exit;
  end;

  if CurrentFileName <> FileNameU then
  try
    CurrentFileName:= FileNameU;
    DataAudio.LoadFromFile(FileNameU);
  except
    Exit(ft_fileerror);
  end;

  Result:= FIELD_TYPE[FieldIndex];

  case FieldIndex of
    0: Value:= DataAudio.Channels;
    1:
      begin
        Time^.wHour:= DataAudio.Duration div 3600;
        Time^.wMinute:= DataAudio.Duration mod 3600 div 60;
        Time^.wSecond:= DataAudio.Duration mod 60;
      end;
    2: Value:= DataAudio.DurationHMS;
    3:
      case UnitIndex of
        0: ValueI^:= DataAudio.SampleRate;
        1: ValueI^:= DataAudio.SampleRate div 1000;
      end;
    4: ValueI^:= DataAudio.BitRate;
    5: Value:= DataAudio.BitRateType;
    6: Value:= DataAudio.Title;
    7: Value:= DataAudio.Artist;
    8: Value:= DataAudio.Album;
    9: ValueI^:= DataAudio.Track;
   10: Value:= Format('%.2d', [DataAudio.Track]);
   11: Value:= DataAudio.Date;
   12: Value:= DataAudio.Genre;
   13: Value:= DataAudio.Comment;
   14: Value:= DataAudio.Composer;
   15: Value:= DataAudio.Copyright;
   16: Value:= DataAudio.URL;
   17: Value:= DataAudio.Encoder;
   18: Value:= DataAudio.Tags;
   19:
     begin
       if UnitIndex = -1 then
         Result:= ft_fieldempty
       else begin
         MaxLen:= MaxLen div SizeOf(WideChar) - 1;
         FullText:= Copy(DataAudio.FullText, UnitIndex + 1, MaxLen);
         if Length(FullText) = 0 then
           Result:= ft_fieldempty
         else begin
           StrPLCopy(PWideChar(FieldValue), FullText, MaxLen);
         end;
       end;
     end;
  end;

  case Result of
    ft_string,
    ft_stringw,
    ft_multiplechoice:
      begin
        if Length(Value) = 0 then
          Result:= ft_fieldempty
        else begin
          if Result <> ft_stringw then
            StrPLCopy(PAnsiChar(FieldValue), Value, MaxLen - 1)
          else begin
            MaxLen:= MaxLen div SizeOf(WideChar) - 1;
            StrPLCopy(PWideChar(FieldValue), UTF8ToUTF16(Value), MaxLen);
          end;
        end;
      end;
    ft_numeric_32:
      if ValueI^ = 0 then Result:= ft_fieldempty;
  end;
end;

procedure ContentSetDefaultParams(dps: PContentDefaultParamStruct); dcpcall;
begin
  DataAudio:= TAudioData.Create;
end;

procedure ContentPluginUnloading; dcpcall;
begin
  FreeAndNil(DataAudio);
end;

procedure ContentGetDetectString(DetectString: PAnsiChar; MaxLen: Integer); dcpcall;
begin
  StrPLCopy(DetectString, DETECT_STRING, MaxLen - 1);
end;

exports
  ContentGetSupportedField,
  ContentGetValue,
  ContentGetValueW,
  ContentGetDetectString,
  ContentSetDefaultParams,
  ContentPluginUnloading;

begin

end.

