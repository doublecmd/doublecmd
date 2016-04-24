{
   Double commander
   -------------------------------------------------------------------------
   Class TMP4file - for manipulating with M4A audio file information

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

unit MP4file;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCClassesUtf8;

type
  TAtomName = array [0..3] of AnsiChar;

  { TMP4file }

  TMP4file = class
  private
    FFileSize: Int64;
    FStream: TStream;
    function GetValid: Boolean;
  private
    FChannels: Word;
    FBitRate: Double;
    FDuration: Double;
    FSampleSize: Word;
    FSampleRate: LongWord;
  private
    FYear,
    FGenre,
    FTitle,
    FAlbum,
    FArtist,
    FEncoder,
    FComment,
    FComposer,
    FCopyright: String;
    FTrack: LongWord;
  protected
    procedure ResetData;
    procedure ReadMovieHeader;
    procedure ReadMetaDataItemList;
    procedure ReadSampleDescription;
    function ReadAtomData: String;
    function ReadGenreData: String;
    function ReadTrackData: LongWord;
    function FindAtomHeader(const AName: TAtomName; ASize: PInt64 = nil): Boolean;
    function LoadAtomHeader(out AtomName: TAtomName; out AtomSize: Int64): Boolean;
  public
    constructor Create;                                      { Create object }
    destructor Destroy; override;                           { Destroy object }
    function ReadFromFile(const FileName: String): Boolean;    { Load header }
    property FileSize: Int64 read FFileSize;             { File size (bytes) }
    property Channels: Word read FChannels;             { Number of channels }
    property SampleRate: LongWord read FSampleRate;       { Sample rate (hz) }
    property BitRate: Double read FBitRate;               { Bit rate (bit/s) }
    property Duration: Double read FDuration;           { Duration (seconds) }
    property Year: String read FYear;                         { Release year }
    property Genre: String read FGenre;                         { Genre name }
    property Track: LongWord read FTrack;                     { Track number }
    property Title: String read FTitle;                         { Song title }
    property Album: String read FAlbum;                        { Album title }
    property Artist: String read FArtist;                      { Artist name }
    property Comment: String read FComment;                        { Comment }
    property Encoder: String read FEncoder;                        { Encoder }
    property Composer: String read FComposer;                     { Composer }
    property Copyright: String read FCopyright;                  { Copyright }
    property Valid: Boolean read GetValid;              { True if data valid }
  end;

implementation

uses
  ID3v1;

{ TMP4file }

function TMP4file.FindAtomHeader(const AName: TAtomName; ASize: PInt64): Boolean;
var
  AtomSize: Int64;
  APosition: Int64;
  AtomName: TAtomName;
begin
  repeat
    if not LoadAtomHeader(AtomName, AtomSize) then
      Break;
    if SameText(AtomName, AName) then
    begin
      if Assigned(ASize) then ASize^:= AtomSize;
      Exit(True);
    end
    else begin
      APosition:= FStream.Seek(AtomSize, soCurrent);
    end;
  until (APosition >= FFileSize);
  Result:= False;
end;

function TMP4file.GetValid: Boolean;
begin
  Result:= (FDuration > 0.0) and (FBitRate > 0.0);
end;

function TMP4file.LoadAtomHeader(out AtomName: TAtomName; out AtomSize: Int64): Boolean;
begin
  AtomSize:= SwapEndian(FStream.ReadDWord);
  FillChar({%H-}AtomName, SizeOf(TAtomName), #0);
  FStream.Read(AtomName, SizeOf(TAtomName));
  if AtomSize <> 1 then
    AtomSize:= AtomSize - 8
  else begin
    AtomSize:= Int64(SwapEndian(FStream.ReadQWord)) - 16;
  end;
  Result:= not ((AtomSize < 0) or (AtomSize > FFileSize));
end;

procedure TMP4file.ResetData;
begin
  FTrack:= 0;
  FBitRate:= 0;
  FChannels:= 0;
  FDuration:= 0.0;
  FSampleSize:= 0;
  FSampleRate:= 0;
  FYear:= EmptyStr;
  FGenre:= EmptyStr;
  FTitle:= EmptyStr;
  FAlbum:= EmptyStr;
  FArtist:= EmptyStr;
  FEncoder:= EmptyStr;
  FComment:= EmptyStr;
  FComposer:= EmptyStr;
  FCopyright:= EmptyStr;
end;

procedure TMP4file.ReadMovieHeader;
var
  AVersion: Byte;
  MediaSize: Int64;
  ADuration: QWord = 0;
  TimeScale: LongWord = 0;
begin
  FStream.Seek(0, soBeginning);
  if FindAtomHeader('moov') and FindAtomHeader('mvhd') then
  begin
    AVersion:= FStream.ReadByte;
    FStream.Seek(3, soCurrent);
    if AVersion = 0 then
    begin
      FStream.Seek(8, soCurrent);
      TimeScale:= SwapEndian(FStream.ReadDWord);
      ADuration:= SwapEndian(FStream.ReadDWord);
    end
    else if AVersion = 1 then
    begin
      FStream.Seek(16, soCurrent);
      TimeScale:= SwapEndian(FStream.ReadDWord);
      ADuration:= SwapEndian(FStream.ReadQWord);
    end;
    if TimeScale > 0 then FDuration:= ADuration / TimeScale;
  end;
  FStream.Seek(0, soBeginning);
  if (FDuration > 0) and FindAtomHeader('mdat', @MediaSize) then
  begin
    FBitRate:= MediaSize * 8 / FDuration / 1000;
  end;
end;

function TMP4file.ReadAtomData: String;
var
  AtomSize: Int64;
  DataType: LongWord;
  Buffer: array[Byte] of AnsiChar;
begin
  Result:= EmptyStr;
  if FindAtomHeader('data', @AtomSize) then
  begin
    if AtomSize - 8 > High(Byte) then Exit;
    DataType:= SwapEndian(FStream.ReadDWord);
    if DataType = 1 then
    begin
      FStream.Seek(4, soCurrent);
      FStream.Read({%H-}Buffer, AtomSize - 8);
      SetString(Result, Buffer, AtomSize - 8);
    end;
  end;
end;

function TMP4file.ReadGenreData: String;
var
  AtomSize: Int64;
  AGenre: Word = 0;
begin
  Result:= EmptyStr;
  if FindAtomHeader('data', @AtomSize) then
  begin
    FStream.Seek(8, soCurrent);
    AGenre:= SwapEndian(FStream.ReadWord);
    FStream.Seek(AtomSize - 10, soCurrent);
  end;
  if (AGenre > 0) and (AGenre < MAX_MUSIC_GENRES) then
  begin
    Result:= aTAG_MusicGenre[AGenre - 1];
  end;
end;

function TMP4file.ReadTrackData: LongWord;
var
  AtomSize: Int64;
begin
  Result:= 0;
  if FindAtomHeader('data', @AtomSize) then
  begin
    FStream.Seek(8, soCurrent);
    Result:= SwapEndian(FStream.ReadDWord);
    FStream.Seek(AtomSize - 12, soCurrent);
  end;
end;

procedure TMP4file.ReadMetaDataItemList;
var
  AtomSize: Int64;
  AtomFinish: Int64;
  AtomName: TAtomName;
begin
  FStream.Seek(0, soBeginning);
  if not FindAtomHeader('moov') then Exit;
  if not FindAtomHeader('udta') then Exit;

  if FindAtomHeader('meta') then
  begin
    FStream.Seek(4, soCurrent);
    if FindAtomHeader('ilst', @AtomSize) then
    begin
      AtomFinish := FStream.Position + AtomSize;
      while FStream.Position < AtomFinish do
      begin
        LoadAtomHeader(AtomName, AtomSize);
        if SameText('trkn', AtomName) then
          FTrack:= ReadTrackData
        else if SameText('gnre', AtomName) then
          FGenre:= ReadGenreData
        else if SameText('cprt', AtomName) then
          FCopyright:= ReadAtomData
        else if SameText(#169'art', AtomName) then
          FArtist:= ReadAtomData
        else if SameText(#169'alb', AtomName) then
          FAlbum:= ReadAtomData
        else if SameText(#169'cmt', AtomName) then
          FComment:= ReadAtomData
        else if SameText(#169'day', AtomName) then
          FYear:= ReadAtomData
        else if SameText(#169'nam', AtomName) then
          FTitle:= ReadAtomData
        else if SameText(#169'too', AtomName) then
          FEncoder:= ReadAtomData
        else if SameText(#169'wrt', AtomName) then
          FComposer:= ReadAtomData
        else if SameText(#169'gen', AtomName) then
          FGenre:= ReadAtomData
        else FStream.Seek(AtomSize, soCurrent);
      end;
    end;
  end;
end;

procedure TMP4file.ReadSampleDescription;
var
  Number: LongWord;
begin
  if not FindAtomHeader('moov') then Exit;
  if not FindAtomHeader('trak') then Exit;
  if not FindAtomHeader('mdia') then Exit;
  if not FindAtomHeader('minf') then Exit;
  if not FindAtomHeader('stbl') then Exit;

  if FindAtomHeader('stsd') then
  begin
    FStream.Seek(4, soCurrent);
    Number:= SwapEndian(FStream.ReadDWord);
    if Number = 1 then
    begin
      if FindAtomHeader('mp4a') then
      begin
        FStream.Seek(16, soCurrent);
        FChannels:= SwapEndian(FStream.ReadWord);
        FSampleSize:= SwapEndian(FStream.ReadWord);
        FStream.Seek(2, soCurrent);
        FSampleRate:= SwapEndian(FStream.ReadDWord);
      end;
    end;
  end;
end;

constructor TMP4file.Create;
begin

end;

destructor TMP4file.Destroy;
begin
  inherited Destroy;
end;

function TMP4file.ReadFromFile(const FileName: String): Boolean;
var
  AtomSize: Int64;
  AtomName: TAtomName;
begin
  ResetData;
  FStream:= TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    FFileSize:= FStream.Size;
    Result:= LoadAtomHeader(AtomName, AtomSize) and SameText(AtomName, 'ftyp');
    if Result then
    begin
      FStream.Seek(AtomSize, soCurrent);
      ReadSampleDescription;
      ReadMovieHeader;
      ReadMetaDataItemList;
    end;
  finally
    FreeAndNil(FStream);
  end;
end;

end.

