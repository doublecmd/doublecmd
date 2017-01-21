{
   Double commander
   -------------------------------------------------------------------------
   This content plugin can show information about audio files

   Copyright (C) 2016 Alexander Koblov (alexx2000@mail.ru)

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

unit AudioData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCStrUtils, MPEGaudio, Musepack, OggVorbis, ID3v1, ID3v2,
  APEtag, FLACfile, Monkey, AACfile, CDAtrack, WMAfile, WAVfile, TTA, TwinVQ,
  AC3, DTS, WAVPackfile, OptimFROG, MP4file;

type

  { TAudioData }

  TAudioData = class
  private
    FDTS: TDTS;
    FAC3: TAC3;
    FTTA: TTTA;
    FTwinVQ: TTwinVQ;
    FMonkey: TMonkey;
    FMP4file: TMP4file;
    FAACfile: TAACfile;
    FWMAfile: TWMAfile;
    FWAVfile: TWAVfile;
    FFLACfile: TFLACfile;
    FMPEGplus: TMPEGplus;
    FCDAtrack: TCDAtrack;
    FMPEGaudio: TMPEGaudio;
    FOggVorbis: TOggVorbis;
    FOptimFrog: TOptimFrog;
    FWAVPackfile: TWAVPackfile;
  private
    procedure Clear;
    procedure ReadID3v1(ID3v1: TID3v1);
    procedure ReadID3v2(ID3v2: TID3v2);
    procedure ReadAPEtag(APEtag: TAPEtag);
    procedure AppendTag(const ATag: String);
    function FormatChannels(AChannels: Integer): String;
    function FormatDuration(ADuration: Integer): String;
    procedure UpdateValue(var AValue: Integer; AData: Integer);
    procedure UpdateValue(var AValue: String; const AData: String);
  protected
    FFileName: String;
    function ReadDTS: Boolean;
    function ReadAC3: Boolean;
    function ReadTTA: Boolean;
    function ReadTwinVQ: Boolean;
    function ReadMonkey: Boolean;
    function ReadMP4file: Boolean;
    function ReadAACfile: Boolean;
    function ReadWMAfile: Boolean;
    function ReadWAVfile: Boolean;
    function ReadFLACfile: Boolean;
    function ReadMPEGplus: Boolean;
    function ReadCDAtrack: Boolean;
    function ReadMPEGaudio: Boolean;
    function ReadOggVorbis: Boolean;
    function ReadOptimFrog: Boolean;
    function ReadWAVPackfile: Boolean;
  public
    Album, Artist, Title: String;
    Track,
    Duration,
    SampleRate,
    BitRate: Integer;
    DurationHMS,
    BitRateType,
    Channels,
    Date, Genre, Comment, Tags,
    Encoder, Composer, Copyright, URL: String;
    FullText: UnicodeString;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadFromFile(const FileName: String): Boolean;
  end;

implementation

uses
  LazUTF8;

{ TAudioData }

procedure TAudioData.Clear;
begin
  Track:= 0;
  BitRate:= 0;
  Duration:= 0;
  SampleRate:= 0;
  URL:= EmptyStr;
  Tags:= EmptyStr;
  Date:= EmptyStr;
  Genre:= EmptyStr;
  Title:= EmptyStr;
  Album:= EmptyStr;
  Artist:= EmptyStr;
  Comment:= EmptyStr;
  Encoder:= EmptyStr;
  Channels:= EmptyStr;
  Composer:= EmptyStr;
  Copyright:= EmptyStr;
  DurationHMS:= EmptyStr;
  FullText:= EmptyWideStr;
  BitRateType:= 'Unknown';
end;

procedure TAudioData.ReadID3v1(ID3v1: TID3v1);
begin
  if ID3v1.Exists then
  begin
    UpdateValue(Date, ID3v1.Year);
    UpdateValue(Track, ID3v1.Track);
    UpdateValue(Album, ID3v1.Album);
    UpdateValue(Title, ID3v1.Title);
    UpdateValue(Genre, ID3v1.Genre);
    UpdateValue(Artist, ID3v1.Artist);
    UpdateValue(Comment, ID3v1.Comment);
    case ID3v1.VersionID of
      TAG_VERSION_1_0: AppendTag('ID3v1.0');
      TAG_VERSION_1_1: AppendTag('ID3v1.1');
      else AppendTag('ID3v1');
    end;
  end;
end;

procedure TAudioData.ReadID3v2(ID3v2: TID3v2);
begin
  if ID3v2.Exists then
  begin
    UpdateValue(URL, ID3v2.Link);
    UpdateValue(Date, ID3v2.Year);
    UpdateValue(Track, ID3v2.Track);
    UpdateValue(Album, ID3v2.Album);
    UpdateValue(Title, ID3v2.Title);
    UpdateValue(Genre, ID3v2.Genre);
    UpdateValue(Artist, ID3v2.Artist);
    UpdateValue(Comment, ID3v2.Comment);
    UpdateValue(Encoder, ID3v2.Encoder);
    UpdateValue(Composer, ID3v2.Composer);
    UpdateValue(Copyright, ID3v2.Copyright);
    case ID3v2.VersionID of
      TAG_VERSION_2_2: AppendTag('ID3v2.2');
      TAG_VERSION_2_3: AppendTag('ID3v2.3');
      TAG_VERSION_2_4: AppendTag('ID3v2.4');
      else AppendTag('ID3v2');
    end;
  end;
end;

procedure TAudioData.ReadAPEtag(APEtag: TAPEtag);
begin
  if APEtag.Exists then
  begin
    UpdateValue(Date, APEtag.Year);
    UpdateValue(Track, APEtag.Track);
    UpdateValue(Album, APEtag.Album);
    UpdateValue(Title, APEtag.Title);
    UpdateValue(Genre, APEtag.Genre);
    UpdateValue(Artist, APEtag.Artist);
    UpdateValue(Comment, APEtag.Comment);
    UpdateValue(Composer, APEtag.Composer);
    UpdateValue(Copyright, APEtag.Copyright);
    AppendTag('APEv' + IntToStr(APEtag.Version div 1000));
  end;
end;

procedure TAudioData.AppendTag(const ATag: String);
begin
  if Length(Tags) = 0 then
    Tags:= ATag
  else begin
    Tags:= Tags + ' ' + ATag;
  end;
end;

function TAudioData.FormatChannels(AChannels: Integer): String;
begin
  case AChannels of
    0: Result:= 'Unknown';
    1: Result:= 'Mono';
    2: Result:= 'Stereo'
    else Result:= IntToStr(AChannels) + ' ch';
  end;
end;

function TAudioData.FormatDuration(ADuration: Integer): String;
var
  AHour, AMinute, ASecond: Integer;
begin
  AHour:= ADuration div 3600;
  AMinute:= ADuration mod 3600 div 60;
  ASecond:= ADuration mod 60;
  Result:= Format('%.2d:%.2d:%.2d', [AHour, AMinute, ASecond]);
end;

procedure TAudioData.UpdateValue(var AValue: Integer; AData: Integer);
begin
  if AValue <> 0 then AValue:= AData;
end;

procedure TAudioData.UpdateValue(var AValue: String; const AData: String);
begin
  if Length(AValue) = 0 then AValue:= AData;
end;

function TAudioData.ReadDTS: Boolean;
begin
  Result:= FDTS.ReadFromFile(FFileName) and FDTS.Valid;
  if Result then
  begin
    BitRate:= FDTS.BitRate;
    Duration:= Round(FDTS.Duration);
    DurationHMS:= FormatDuration(Duration);
    Channels:= FormatChannels(FDTS.Channels);
    SampleRate:= FDTS.SampleRate;
  end;
end;

function TAudioData.ReadAC3: Boolean;
begin
  Result:= FAC3.ReadFromFile(FFileName) and FAC3.Valid;
  if Result then
  begin
    BitRate:= FAC3.BitRate;
    Duration:= Round(FAC3.Duration);
    DurationHMS:= FormatDuration(Duration);
    Channels:= FormatChannels(FAC3.Channels);
    SampleRate:= FAC3.SampleRate;
  end;
end;

function TAudioData.ReadTTA: Boolean;
begin
  Result:= FTTA.ReadFromFile(FFileName) and FTTA.Valid;
  if Result then
  begin
    BitRate:= Round(FTTA.BitRate);
    Duration:= Round(FTTA.Duration);
    DurationHMS:= FormatDuration(Duration);
    Channels:= FormatChannels(FTTA.Channels);
    SampleRate:= FTTA.SampleRate;
    ReadAPEtag(FTTA.APEtag);
    ReadID3v2(FTTA.ID3v2);
    ReadID3v1(FTTA.ID3v1);
  end;
end;

function TAudioData.ReadTwinVQ: Boolean;
begin
  Result:= FTwinVQ.ReadFromFile(FFileName) and FTwinVQ.Valid;
  if Result then
  begin
    BitRate:= FTwinVQ.BitRate;
    Channels:= FTwinVQ.ChannelMode;
    Duration:= Round(FTwinVQ.Duration);
    DurationHMS:= FormatDuration(Duration);
    SampleRate:= FTwinVQ.SampleRate;

    Album:= FTwinVQ.Album;
    Title:= FTwinVQ.Title;
    Artist:= FTwinVQ.Author;
    Comment:= FTwinVQ.Comment;
  end;
end;

function TAudioData.ReadMonkey: Boolean;
begin
  Result:= FMonkey.ReadFromFile(FFileName) and FMonkey.Valid;
  if Result then
  begin
    Channels:= FMonkey.ChannelMode;
    BitRate:= Round(FMonkey.BitRate) div 1000000;
    Duration:= Round(FMonkey.Duration);
    DurationHMS:= FormatDuration(Duration);
    SampleRate:= FMonkey.SampleRate;
    ReadAPEtag(FMonkey.APEtag);
    ReadID3v2(FMonkey.ID3v2);
    ReadID3v1(FMonkey.ID3v1);
  end;
end;

function TAudioData.ReadMP4file: Boolean;
begin
  Result:= FMP4file.ReadFromFile(FFileName) and FMP4file.Valid;
  if Result then
  begin
    SampleRate:= FMP4file.SampleRate;
    BitRate:= Round(FMP4file.BitRate);
    Duration:= Round(FMP4file.Duration);
    DurationHMS:= FormatDuration(Duration);
    Channels:= FormatChannels(FMP4file.Channels);

    Date:= FMP4file.Year;
    Track:= FMP4file.Track;
    Genre:= FMP4file.Genre;
    Title:= FMP4file.Title;
    Album:= FMP4file.Album;
    Artist:= FMP4file.Artist;
    Comment:= FMP4file.Comment;
    Encoder:= FMP4file.Encoder;
    Composer:= FMP4file.Composer;
    Copyright:= FMP4file.Copyright;
  end;
end;

function TAudioData.ReadAACfile: Boolean;
begin
  Result:= FAACfile.ReadFromFile(FFileName) and FAACfile.Valid;
  if Result then
  begin
    BitRate:= FAACfile.BitRate;
    BitRateType:= FAACfile.BitRateType;
    Duration:= Round(FAACfile.Duration);
    DurationHMS:= FormatDuration(Duration);
    Channels:= FormatChannels(FAACfile.Channels);
    SampleRate:= FAACfile.SampleRate;
    ReadID3v2(FAACfile.ID3v2);
    ReadID3v1(FAACfile.ID3v1);
    ReadAPEtag(FAACfile.APEtag);
  end;
end;

function TAudioData.ReadWMAfile: Boolean;
begin
  Result:= FWMAfile.ReadFromFile(FFileName) and FWMAfile.Valid;
  if Result then
  begin
    BitRate:= FWMAfile.BitRate;
    Channels:= FWMAfile.ChannelMode;
    Duration:= Round(FWMAfile.Duration);
    DurationHMS:= FormatDuration(Duration);
    SampleRate:= FWMAfile.SampleRate;

    Date:= FWMAfile.Year;
    Track:= FWMAfile.Track;
    Album:= FWMAfile.Album;
    Title:= FWMAfile.Title;
    Genre:= FWMAfile.Genre;
    Artist:= FWMAfile.Artist;
    Comment:= FWMAfile.Comment;
  end;
end;

function TAudioData.ReadWAVfile: Boolean;
begin
  Result:= FWAVfile.ReadFromFile(FFileName) and FWAVfile.Valid;
  if Result then
  begin
    BitRate:= Round(FWAVfile.BitRate);
    Channels:= FWAVfile.ChannelMode;
    Duration:= Round(FWAVfile.Duration);
    DurationHMS:= FormatDuration(Duration);
    SampleRate:= FWAVfile.SampleRate;
  end;
end;

function TAudioData.ReadFLACfile: Boolean;
begin
  Result:= FFLACfile.ReadFromFile(FFileName) and FFLACfile.Valid;
  if Result then
  begin
    BitRate:= FFLACfile.BitRate;
    Channels:= FFLACfile.ChannelMode;
    Duration:= Round(FFLACfile.Duration);
    DurationHMS:= FormatDuration(Duration);
    SampleRate:= FFLACfile.SampleRate;

    URL:= FFLACfile.Link;
    Date:= FFLACfile.Year;
    Track:= FFLACfile.Track;
    Album:= FFLACfile.Album;
    Title:= FFLACfile.Title;
    Genre:= FFLACfile.Genre;
    Artist:= FFLACfile.Artist;
    Comment:= FFLACfile.Comment;
    Encoder:= FFLACfile.Encoder;
    Composer:= FFLACfile.Composer;
    Copyright:= FFLACfile.Copyright;
  end;
end;

function TAudioData.ReadMPEGplus: Boolean;
begin
  Result:= FMPEGplus.ReadFromFile(FFileName) and FMPEGplus.Valid;
  if Result then
  begin
    BitRate:= FMPEGplus.BitRate;
    Channels:= FMPEGplus.ChannelMode;
    Duration:= Round(FMPEGplus.Duration);
    DurationHMS:= FormatDuration(Duration);
    SampleRate:= FMPEGplus.SampleRate;
    ReadID3v2(FMPEGplus.ID3v2);
    ReadID3v1(FMPEGplus.ID3v1);
    ReadAPEtag(FMPEGplus.APEtag);
  end;
end;

function TAudioData.ReadCDAtrack: Boolean;
begin
  Result:= FCDAtrack.ReadFromFile(FFileName) and FCDAtrack.Valid;
  if Result then
  begin
    Duration:= Round(FCDAtrack.Duration);
    DurationHMS:= FormatDuration(Duration);
    Track:= FCDAtrack.Track;
    Album:= FCDAtrack.Album;
    Title:= FCDAtrack.Title;
    Artist:= FCDAtrack.Artist;
  end;
end;

function TAudioData.ReadMPEGaudio: Boolean;
begin
  Result:= FMPEGaudio.ReadFromFile(FFileName) and FMPEGaudio.Valid;
  if Result then
  begin
    if FMPEGaudio.VBR.Found then
      BitRateType:= 'VBR'
    else begin
      BitRateType:= 'CBR';
    end;
    BitRate:= FMPEGaudio.BitRate;
    Channels:= FMPEGaudio.ChannelMode;
    Duration:= Round(FMPEGaudio.Duration);
    DurationHMS:= FormatDuration(Duration);
    SampleRate:= FMPEGaudio.SampleRate;
    ReadID3v2(FMPEGaudio.ID3v2);
    ReadID3v1(FMPEGaudio.ID3v1);
  end;
end;

function TAudioData.ReadOggVorbis: Boolean;
begin
  Result:= FOggVorbis.ReadFromFile(FFileName) and FOggVorbis.Valid;
  if Result then
  begin
    BitRate:= FOggVorbis.BitRate;
    Channels:= FOggVorbis.ChannelMode;
    Duration:= Round(FOggVorbis.Duration);
    DurationHMS:= FormatDuration(Duration);
    SampleRate:= FOggVorbis.SampleRate;

    Date:= FOggVorbis.Date;
    Track:= FOggVorbis.Track;
    Album:= FOggVorbis.Album;
    Title:= FOggVorbis.Title;
    Genre:= FOggVorbis.Genre;
    Artist:= FOggVorbis.Artist;
    Comment:= FOggVorbis.Comment;
    Encoder:= FOggVorbis.Encoder;
  end;
end;

function TAudioData.ReadOptimFrog: Boolean;
begin
  Result:= FOptimFrog.ReadFromFile(FFileName) and FOptimFrog.Valid;
  if Result then
  begin
    BitRate:= FOptimFrog.BitRate;
    Channels:= FOptimFrog.ChannelMode;
    Duration:= Round(FOptimFrog.Duration);
    DurationHMS:= FormatDuration(Duration);
    SampleRate:= FOptimFrog.SampleRate;
    ReadID3v2(FOptimFrog.ID3v2);
    ReadID3v1(FOptimFrog.ID3v1);
    ReadAPEtag(FOptimFrog.APEtag);
  end;
end;

function TAudioData.ReadWAVPackfile: Boolean;
begin
  Result:= FWAVPackfile.ReadFromFile(FFileName) and FWAVPackfile.Valid;
  if Result then
  begin
    Encoder:= FWAVPackfile.Encoder;
    Channels:= FWAVPackfile.ChannelMode;
    BitRate:= Round(FWAVPackfile.BitRate);
    Duration:= Round(FWAVPackfile.Duration);
    DurationHMS:= FormatDuration(Duration);
    SampleRate:= FWAVPackfile.SampleRate;
    ReadAPEtag(FWAVPackfile.APEtag);
  end;
end;

constructor TAudioData.Create;
begin
  FDTS:= TDTS.Create;
  FAC3:= TAC3.Create;
  FTTA:= TTTA.Create;
  FTwinVQ:= TTwinVQ.Create;
  FMonkey:= TMonkey.Create;
  FMP4file:= TMP4file.Create;
  FAACfile:= TAACfile.Create;
  FWMAfile:= TWMAfile.Create;
  FWAVfile:= TWAVfile.Create;
  FCDAtrack:= TCDAtrack.Create;
  FMPEGplus:= TMPEGplus.Create;
  FFLACfile:= TFLACfile.Create;
  FMPEGaudio:= TMPEGaudio.Create;
  FOggVorbis:= TOggVorbis.Create;
  FOptimFrog:= TOptimFrog.Create;
  FWAVPackfile:= TWAVPackfile.Create;
end;

destructor TAudioData.Destroy;
begin
  FDTS.Free;
  FAC3.Free;
  FTTA.Free;
  FTwinVQ.Free;
  FMonkey.Free;
  FMP4file.Free;
  FAACfile.Free;
  FWMAfile.Free;
  FWAVfile.Free;
  FCDAtrack.Free;
  FMPEGplus.Free;
  FFLACfile.Free;
  FMPEGaudio.Free;
  FOggVorbis.Free;
  FOptimFrog.Free;
  FWAVPackfile.Free;
  inherited Destroy;
end;

function TAudioData.LoadFromFile(const FileName: String): Boolean;
var
  FileExt: String;
begin
  Clear;

  FFileName:= FileName;
  FileExt:= LowerCase(ExtractOnlyFileExt(FileName));

  if (FileExt = 'mp3') or (FileExt = 'mp2') or (FileExt = 'mp1') then
  begin
    Result:= ReadMPEGaudio;
  end
  else if (FileExt = 'mpc') then
  begin
    Result:= ReadMPEGplus;
  end
  else if (FileExt = 'ogg') then
  begin
    Result:= ReadOggVorbis;
  end
  else if (FileExt = 'flac') then
  begin
    Result:= ReadFLACfile;
  end
  else if (FileExt = 'ape') then
  begin
    Result:= ReadMonkey;
  end
  else if (FileExt = 'aac') then
  begin
    Result:= ReadAACfile;
  end
  else if (FileExt = 'cda') then
  begin
    Result:= ReadCDAtrack;
  end
  else if (FileExt = 'wma') then
  begin
    Result:= ReadWMAfile;
  end
  else if (FileExt = 'wav') then
  begin
    Result:= ReadWAVfile;
  end
  else if (FileExt = 'tta') then
  begin
    Result:= ReadTTA;
  end
  else if (FileExt = 'vqf') then
  begin
    Result:= ReadTwinVQ;
  end
  else if (FileExt = 'ac3') then
  begin
    Result:= ReadAC3;
  end
  else if (FileExt = 'dts') then
  begin
    Result:= ReadDTS;
  end
  else if (FileExt = 'wv') or (FileExt = 'wvc') then
  begin
    Result:= ReadWAVPackfile;
  end
  else if (FileExt = 'ofr') or (FileExt = 'ofs') then
  begin
    Result:= ReadOptimFrog;
  end
  else if (FileExt = 'mp4') or (FileExt = 'm4a') then
  begin
    Result:= ReadMP4file;
  end
  else Result:= False;

  if Result then
  begin
    FullText:= UTF8ToUTF16(Title + LineEnding + Artist + LineEnding + Album + LineEnding +
                 Comment + LineEnding + Composer + LineEnding + Copyright + LineEnding +
                 URL + LineEnding + Encoder + LineEnding);
  end;
end;

end.

