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
  APEtag, FLACfile, Monkey, AACfile, CDAtrack, WMAfile, WAVfile, TTA, TwinVQ;

type

  { TAudioData }

  TAudioData = class
  private
    FTTA: TTTA;
    FTwinVQ: TTwinVQ;
    FMonkey: TMonkey;
    FAACfile: TAACfile;
    FWMAfile: TWMAfile;
    FWAVfile: TWAVfile;
    FFLACfile: TFLACfile;
    FMPEGplus: TMPEGplus;
    FCDAtrack: TCDAtrack;
    FMPEGaudio: TMPEGaudio;
    FOggVorbis: TOggVorbis;
  private
    procedure Clear;
    procedure ReadID3v1(ID3v1: TID3v1);
    procedure ReadID3v2(ID3v2: TID3v2);
    procedure ReadAPEtag(APEtag: TAPEtag);
    function FormatDuration(ADuration: Integer): String;
    procedure UpdateValue(var AValue: Integer; AData: Integer);
    procedure UpdateValue(var AValue: String; const AData: String);
  protected
    FFileName: String;
    function ReadTTA: Boolean;
    function ReadTwinVQ: Boolean;
    function ReadMonkey: Boolean;
    function ReadAACfile: Boolean;
    function ReadWMAfile: Boolean;
    function ReadWAVfile: Boolean;
    function ReadFLACfile: Boolean;
    function ReadMPEGplus: Boolean;
    function ReadCDAtrack: Boolean;
    function ReadMPEGaudio: Boolean;
    function ReadOggVorbis: Boolean;
  public
    Album, Artist, Title: String;
    Track,
    Duration,
    SampleRate,
    BitRate: Integer;
    DurationHMS,
    BitRateType,
    Channels,
    Date, Genre, Comment,
    Encoder, Composer, Copyright, URL, FullText: String;
  public
    constructor Create;
    destructor Destroy; override;
    function LoadFromFile(const FileName: String): Boolean;
  end;

implementation

{ TAudioData }

procedure TAudioData.Clear;
begin
  Track:= 0;
  BitRate:= 0;
  Duration:= 0;
  SampleRate:= 0;
  URL:= EmptyStr;
  Date:= EmptyStr;
  Genre:= EmptyStr;
  Title:= EmptyStr;
  Album:= EmptyStr;
  Artist:= EmptyStr;
  Comment:= EmptyStr;
  Encoder:= EmptyStr;
  Channels:= EmptyStr;
  Composer:= EmptyStr;
  FullText:= EmptyStr;
  Copyright:= EmptyStr;
  DurationHMS:= EmptyStr;
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

function TAudioData.ReadTTA: Boolean;
begin
  Result:= FTTA.ReadFromFile(FFileName) and FTTA.Valid;
  if Result then
  begin
    // Channels:= FTTA.ChannelMode;
    BitRate:= Round(FTTA.BitRate);
    Duration:= Round(FTTA.Duration);
    DurationHMS:= FormatDuration(Duration);
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
    BitRate:= Round(FMonkey.BitRate);
    Duration:= Round(FMonkey.Duration);
    DurationHMS:= FormatDuration(Duration);
    SampleRate:= FMonkey.SampleRate;
    ReadAPEtag(FMonkey.APEtag);
    ReadID3v2(FMonkey.ID3v2);
    ReadID3v1(FMonkey.ID3v1);
  end;
end;

function TAudioData.ReadAACfile: Boolean;
begin
  Result:= FAACfile.ReadFromFile(FFileName) and FAACfile.Valid;
  if Result then
  begin
    BitRate:= FAACfile.BitRate;
    BitRateType:= FAACfile.BitRateType;
    // Channels:= FAACfile.ChannelMode;
    Duration:= Round(FAACfile.Duration);
    DurationHMS:= FormatDuration(Duration);
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

constructor TAudioData.Create;
begin
  FTTA:= TTTA.Create;
  FTwinVQ:= TTwinVQ.Create;
  FMonkey:= TMonkey.Create;
  FAACfile:= TAACfile.Create;
  FWMAfile:= TWMAfile.Create;
  FWAVfile:= TWAVfile.Create;
  FCDAtrack:= TCDAtrack.Create;
  FMPEGplus:= TMPEGplus.Create;
  FFLACfile:= TFLACfile.Create;
  FMPEGaudio:= TMPEGaudio.Create;
  FOggVorbis:= TOggVorbis.Create;
end;

destructor TAudioData.Destroy;
begin
  FTTA.Free;
  FTwinVQ.Free;
  FMonkey.Free;
  FAACfile.Free;
  FWMAfile.Free;
  FWAVfile.Free;
  FCDAtrack.Free;
  FMPEGplus.Free;
  FFLACfile.Free;
  FMPEGaudio.Free;
  FOggVorbis.Free;
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
  else Result:= False;

  if Result then
  begin
    FullText:= Title + LineEnding + Artist + LineEnding + Album + LineEnding +
               Comment + LineEnding + Composer + LineEnding + Copyright + LineEnding +
               URL + LineEnding + Encoder + LineEnding;
  end;
end;

end.

