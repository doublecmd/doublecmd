{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library                                                         }
{ Class TOggVorbis - for manipulating with Ogg Vorbis file information        }
{                                                                             }
{ http://mac.sourceforge.net/atl/                                             }
{ e-mail: macteam@users.sourceforge.net                                       }
{                                                                             }
{ Copyright (c) 2000-2002 by Jurgen Faul                                      }
{ Copyright (c) 2003-2005 by The MAC Team                                     }
{                                                                             }
{ Version 1.9 (April 2005) by Gambit                                          }
{   - updated to unicode file access                                          }
{                                                                             }
{ Version 1.83 (26 march 2005) by Kurtnoise                                   }
{   - Added multichannel support                                              }
{                                                                             }
{ Version 1.82 (23 March 2005) by Gambit                                      }
{   - fixed nominal bitrate info (eg 192 was 193 sometimes)                   }
{                                                                             }
{ Version 1.81 (21 June 2004) by Gambit                                       }
{   - Added Encoder property                                                  }
{                                                                             }
{ Version 1.8 (13 April 2004) by Gambit                                       }
{   - Added Ratio property                                                    }
{                                                                             }
{ Version 1.7 (20 August 2003) by Madah                                       }
{   - Minor fix: changed FSampleRate into Integer                             }
{     ... so that samplerates>65535 works.                                    }
{                                                                             }
{ Version 1.6 (2 October 2002)                                                }
{   - Writing support for Vorbis tag                                          }
{   - Changed several properties                                              }
{   - Fixed bug with long Vorbis tag fields                                   }
{                                                                             }
{ Version 1.2 (18 February 2002)                                              }
{   - Added property BitRateNominal                                           }
{   - Fixed bug with Vorbis tag fields                                        }
{                                                                             }
{ Version 1.1 (21 October 2001)                                               }
{   - Support for UTF-8                                                       }
{   - Fixed bug with vendor info detection                                    }
{                                                                             }
{ Version 1.0 (15 August 2001)                                                }
{   - File info: file size, channel mode, sample rate, duration, bit rate     }
{   - Vorbis tag: title, artist, album, track, date, genre, comment, vendor   }
{                                                                             }
{ This library is free software; you can redistribute it and/or               }
{ modify it under the terms of the GNU Lesser General Public                  }
{ License as published by the Free Software Foundation; either                }
{ version 2.1 of the License, or (at your option) any later version.          }
{                                                                             }
{ This library is distributed in the hope that it will be useful,             }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of              }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           }
{ Lesser General Public License for more details.                             }
{                                                                             }
{ You should have received a copy of the GNU Lesser General Public            }
{ License along with this library; if not, write to the Free Software         }
{ Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA   }
{                                                                             }
{ *************************************************************************** }

unit OggVorbis;

interface

uses
  Classes, SysUtils, DCClassesUtf8, DCOSUtils;

const
  { Used with ChannelModeID property }
  VORBIS_CM_MONO = 1;                                    { Code for mono mode }
  VORBIS_CM_STEREO = 2;                                { Code for stereo mode }
  VORBIS_CM_MULTICHANNEL = 6;                    { Code for Multichannel Mode }

  { Channel mode names }
  VORBIS_MODE: array [0..3] of string = ('Unknown', 'Mono', 'Stereo', 'Multichannel');

type
  { Class TOggVorbis }
  TOggVorbis = class(TObject)
    private
      { Private declarations }
      FFileSize: Integer;
      FChannelModeID: Byte;
      FSampleRate: integer;
      FBitRateNominal: Word;
      FSamples: Integer;
      FID3v2Size: Integer;
      FTitle: string;
      FArtist: string;
      FAlbum: string;
      FTrack: Word;
      FDate: string;
      FGenre: string;
      FComment: string;
      FVendor: string;
      procedure FResetData;
      function FGetChannelMode: string;
      function FGetDuration: Double;
      function FGetBitRate: Word;
      function FHasID3v2: Boolean;
      function FIsValid: Boolean;
      function FGetRatio: Double;
      function FGetEncoder: String;
    public
      { Public declarations }
      constructor Create;                                     { Create object }
      destructor Destroy; override;                          { Destroy object }
      function ReadFromFile(const FileName: String): Boolean;     { Load data }
      function SaveTag(const FileName: String): Boolean;      { Save tag data }
      function ClearTag(const FileName: String): Boolean;    { Clear tag data }
      property FileSize: Integer read FFileSize;          { File size (bytes) }
      property ChannelModeID: Byte read FChannelModeID;   { Channel mode code }
      property ChannelMode: string read FGetChannelMode;  { Channel mode name }
      property SampleRate: integer read FSampleRate;       { Sample rate (hz) }
      property BitRateNominal: Word read FBitRateNominal;  { Nominal bit rate }
      property Title: string read FTitle write FTitle;           { Song title }
      property Artist: string read FArtist write FArtist;       { Artist name }
      property Album: string read FAlbum write FAlbum;           { Album name }
      property Track: Word read FTrack write FTrack;           { Track number }
      property Date: string read FDate write FDate;                    { Year }
      property Genre: string read FGenre write FGenre;           { Genre name }
      property Comment: string read FComment write FComment;        { Comment }
      property Vendor: string read FVendor;                   { Vendor string }
      property Duration: Double read FGetDuration;       { Duration (seconds) }
      property BitRate: Word read FGetBitRate;             { Average bit rate }
      property ID3v2: Boolean read FHasID3v2;      { True if ID3v2 tag exists }
      property Valid: Boolean read FIsValid;             { True if file valid }
      property Ratio: Double read FGetRatio;          { Compression ratio (%) }
      property Encoder: String read FGetEncoder;             { Encoder string }
  end;

implementation

const
  { Ogg page header ID }
  OGG_PAGE_ID = 'OggS';

  { Vorbis parameter frame ID }
  VORBIS_PARAMETERS_ID = #1 + 'vorbis';

  { Vorbis tag frame ID }
  VORBIS_TAG_ID = #3 + 'vorbis';

  { Max. number of supported comment fields }
  VORBIS_FIELD_COUNT = 9;

  { Names of supported comment fields }
  VORBIS_FIELD: array [1..VORBIS_FIELD_COUNT] of string =
    ('TITLE', 'ARTIST', 'ALBUM', 'TRACKNUMBER', 'DATE', 'GENRE', 'COMMENT',
    'PERFORMER', 'DESCRIPTION');

  { CRC table for checksum calculating }
  CRC_TABLE: array [0..$FF] of Cardinal = (
    $00000000, $04C11DB7, $09823B6E, $0D4326D9, $130476DC, $17C56B6B,
    $1A864DB2, $1E475005, $2608EDB8, $22C9F00F, $2F8AD6D6, $2B4BCB61,
    $350C9B64, $31CD86D3, $3C8EA00A, $384FBDBD, $4C11DB70, $48D0C6C7,
    $4593E01E, $4152FDA9, $5F15ADAC, $5BD4B01B, $569796C2, $52568B75,
    $6A1936C8, $6ED82B7F, $639B0DA6, $675A1011, $791D4014, $7DDC5DA3,
    $709F7B7A, $745E66CD, $9823B6E0, $9CE2AB57, $91A18D8E, $95609039,
    $8B27C03C, $8FE6DD8B, $82A5FB52, $8664E6E5, $BE2B5B58, $BAEA46EF,
    $B7A96036, $B3687D81, $AD2F2D84, $A9EE3033, $A4AD16EA, $A06C0B5D,
    $D4326D90, $D0F37027, $DDB056FE, $D9714B49, $C7361B4C, $C3F706FB,
    $CEB42022, $CA753D95, $F23A8028, $F6FB9D9F, $FBB8BB46, $FF79A6F1,
    $E13EF6F4, $E5FFEB43, $E8BCCD9A, $EC7DD02D, $34867077, $30476DC0,
    $3D044B19, $39C556AE, $278206AB, $23431B1C, $2E003DC5, $2AC12072,
    $128E9DCF, $164F8078, $1B0CA6A1, $1FCDBB16, $018AEB13, $054BF6A4,
    $0808D07D, $0CC9CDCA, $7897AB07, $7C56B6B0, $71159069, $75D48DDE,
    $6B93DDDB, $6F52C06C, $6211E6B5, $66D0FB02, $5E9F46BF, $5A5E5B08,
    $571D7DD1, $53DC6066, $4D9B3063, $495A2DD4, $44190B0D, $40D816BA,
    $ACA5C697, $A864DB20, $A527FDF9, $A1E6E04E, $BFA1B04B, $BB60ADFC,
    $B6238B25, $B2E29692, $8AAD2B2F, $8E6C3698, $832F1041, $87EE0DF6,
    $99A95DF3, $9D684044, $902B669D, $94EA7B2A, $E0B41DE7, $E4750050,
    $E9362689, $EDF73B3E, $F3B06B3B, $F771768C, $FA325055, $FEF34DE2,
    $C6BCF05F, $C27DEDE8, $CF3ECB31, $CBFFD686, $D5B88683, $D1799B34,
    $DC3ABDED, $D8FBA05A, $690CE0EE, $6DCDFD59, $608EDB80, $644FC637,
    $7A089632, $7EC98B85, $738AAD5C, $774BB0EB, $4F040D56, $4BC510E1,
    $46863638, $42472B8F, $5C007B8A, $58C1663D, $558240E4, $51435D53,
    $251D3B9E, $21DC2629, $2C9F00F0, $285E1D47, $36194D42, $32D850F5,
    $3F9B762C, $3B5A6B9B, $0315D626, $07D4CB91, $0A97ED48, $0E56F0FF,
    $1011A0FA, $14D0BD4D, $19939B94, $1D528623, $F12F560E, $F5EE4BB9,
    $F8AD6D60, $FC6C70D7, $E22B20D2, $E6EA3D65, $EBA91BBC, $EF68060B,
    $D727BBB6, $D3E6A601, $DEA580D8, $DA649D6F, $C423CD6A, $C0E2D0DD,
    $CDA1F604, $C960EBB3, $BD3E8D7E, $B9FF90C9, $B4BCB610, $B07DABA7,
    $AE3AFBA2, $AAFBE615, $A7B8C0CC, $A379DD7B, $9B3660C6, $9FF77D71,
    $92B45BA8, $9675461F, $8832161A, $8CF30BAD, $81B02D74, $857130C3,
    $5D8A9099, $594B8D2E, $5408ABF7, $50C9B640, $4E8EE645, $4A4FFBF2,
    $470CDD2B, $43CDC09C, $7B827D21, $7F436096, $7200464F, $76C15BF8,
    $68860BFD, $6C47164A, $61043093, $65C52D24, $119B4BE9, $155A565E,
    $18197087, $1CD86D30, $029F3D35, $065E2082, $0B1D065B, $0FDC1BEC,
    $3793A651, $3352BBE6, $3E119D3F, $3AD08088, $2497D08D, $2056CD3A,
    $2D15EBE3, $29D4F654, $C5A92679, $C1683BCE, $CC2B1D17, $C8EA00A0,
    $D6AD50A5, $D26C4D12, $DF2F6BCB, $DBEE767C, $E3A1CBC1, $E760D676,
    $EA23F0AF, $EEE2ED18, $F0A5BD1D, $F464A0AA, $F9278673, $FDE69BC4,
    $89B8FD09, $8D79E0BE, $803AC667, $84FBDBD0, $9ABC8BD5, $9E7D9662,
    $933EB0BB, $97FFAD0C, $AFB010B1, $AB710D06, $A6322BDF, $A2F33668,
    $BCB4666D, $B8757BDA, $B5365D03, $B1F740B4);

type
  { Ogg page header }
  OggHeader = packed record
    ID: array [1..4] of Char;                                 { Always "OggS" }
    StreamVersion: Byte;                           { Stream structure version }
    TypeFlag: Byte;                                        { Header type flag }
    AbsolutePosition: Int64;                      { Absolute granule position }
    Serial: Integer;                                   { Stream serial number }
    PageNumber: Integer;                               { Page sequence number }
    Checksum: Integer;                                        { Page checksum }
    Segments: Byte;                                 { Number of page segments }
    LacingValues: array [1..$FF] of Byte;     { Lacing values - segment sizes }
  end;

  { Vorbis parameter header }
  VorbisHeader = packed record
    ID: array [1..7] of Char;                          { Always #1 + "vorbis" }
    BitstreamVersion: array [1..4] of Byte;        { Bitstream version number }
    ChannelMode: Byte;                                   { Number of channels }
    SampleRate: Integer;                                   { Sample rate (hz) }
    BitRateMaximal: Integer;                           { Bit rate upper limit }
    BitRateNominal: Integer;                               { Nominal bit rate }
    BitRateMinimal: Integer;                           { Bit rate lower limit }
    BlockSize: Byte;                   { Coded size for small and long blocks }
    StopFlag: Byte;                                                { Always 1 }
  end;

  { Vorbis tag data }
  VorbisTag = record
    ID: array [1..7] of Char;                          { Always #3 + "vorbis" }
    Fields: Integer;                                   { Number of tag fields }
    FieldData: array [0..VORBIS_FIELD_COUNT] of string;      { Tag field data }
  end;

  { File data }
  FileInfo = record
    FPage, SPage, LPage: OggHeader;             { First, second and last page }
    Parameters: VorbisHeader;                       { Vorbis parameter header }
    Tag: VorbisTag;                                         { Vorbis tag data }
    FileSize: Integer;                                    { File size (bytes) }
    Samples: Integer;                               { Total number of samples }
    ID3v2Size: Integer;                              { ID3v2 tag size (bytes) }
    SPagePos: Integer;                          { Position of second Ogg page }
    TagEndPos: Integer;                                    { Tag end position }
  end;

{ ********************* Auxiliary functions & procedures ******************** }

function GetID3v2Size(const Source: TFileStreamEx): Integer;
type
  ID3v2Header = record
    ID: array [1..3] of Char;
    Version: Byte;
    Revision: Byte;
    Flags: Byte;
    Size: array [1..4] of Byte;
  end;
var
  Header: ID3v2Header;
begin
  { Get ID3v2 tag size (if exists) }
  Result := 0;
  Source.Seek(0, soFromBeginning);
  Source.Read(Header, SizeOf(Header));
  if Header.ID = 'ID3' then
  begin
    Result :=
      Header.Size[1] * $200000 +
      Header.Size[2] * $4000 +
      Header.Size[3] * $80 +
      Header.Size[4] + 10;
    if Header.Flags and $10 = $10 then Inc(Result, 10);
    if Result > Source.Size then Result := 0;
  end;
end;

{ --------------------------------------------------------------------------- }

procedure SetTagItem(const Data: string; var Info: FileInfo);
var
  Separator, Index: Integer;
  FieldID, FieldData: string;
begin
  { Set Vorbis tag item if supported comment field found }
  Separator := Pos('=', Data);
  if Separator > 0 then
  begin
    FieldID := UpperCase(Copy(Data, 1, Separator - 1));
    FieldData := Copy(Data, Separator + 1, Length(Data) - Length(FieldID));
    for Index := 1 to VORBIS_FIELD_COUNT do
      if VORBIS_FIELD[Index] = FieldID then
        Info.Tag.FieldData[Index] := Trim(FieldData);
  end
  else
    if Info.Tag.FieldData[0] = '' then Info.Tag.FieldData[0] := Data;
end;

{ --------------------------------------------------------------------------- }

procedure ReadTag(const Source: TFileStreamEx; var Info: FileInfo);
var
  Index, Size, Position: Integer;
  Data: array [1..250] of Char;
begin
  { Read Vorbis tag }
  Index := 0;
  repeat
    FillChar(Data, SizeOf(Data), 0);
    Source.Read(Size, SizeOf(Size));
    Position := Source.Position;
    if Size > SizeOf(Data) then Source.Read(Data, SizeOf(Data))
    else Source.Read(Data, Size);
    { Set Vorbis tag item }
    SetTagItem(Trim(Data), Info);
    Source.Seek(Position + Size, soFromBeginning);
    if Index = 0 then Source.Read(Info.Tag.Fields, SizeOf(Info.Tag.Fields));
    Inc(Index);
  until Index > Info.Tag.Fields;
  Info.TagEndPos := Source.Position;
end;

{ --------------------------------------------------------------------------- }

function GetSamples(const Source: TFileStreamEx): Integer;
var
  Index, DataIndex, Iterator: Integer;
  Data: array [0..250] of Char;
  Header: OggHeader;
begin
  { Get total number of samples }
  Result := 0;
  for Index := 1 to 50 do
  begin
    DataIndex := Source.Size - (SizeOf(Data) - 10) * Index - 10;
    Source.Seek(DataIndex, soFromBeginning);
    Source.Read(Data, SizeOf(Data));
    { Get number of PCM samples from last Ogg packet header }
    for Iterator := SizeOf(Data) - 10 downto 0 do
      if Data[Iterator] +
        Data[Iterator + 1] +
        Data[Iterator + 2] +
        Data[Iterator + 3] = OGG_PAGE_ID then
      begin
        Source.Seek(DataIndex + Iterator, soFromBeginning);
        Source.Read(Header, SizeOf(Header));
        Result := Header.AbsolutePosition;
        exit;
      end;
  end;
end;

{ --------------------------------------------------------------------------- }

function GetInfo(const FileName: String; var Info: FileInfo): Boolean;
var
  SourceFile: TFileStreamEx;
begin
  { Get info from file }
  Result := false;
  SourceFile := nil;
  try
    SourceFile := TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyWrite);
    Info.FileSize := SourceFile.Size;
    Info.ID3v2Size := GetID3v2Size(SourceFile);
    SourceFile.Seek(Info.ID3v2Size, soFromBeginning);
    SourceFile.Read(Info.FPage, SizeOf(Info.FPage));
    if Info.FPage.ID <> OGG_PAGE_ID then exit;
    SourceFile.Seek(Info.ID3v2Size + Info.FPage.Segments + 27, soFromBeginning);
    { Read Vorbis parameter header }
    SourceFile.Read(Info.Parameters, SizeOf(Info.Parameters));
    if Info.Parameters.ID <> VORBIS_PARAMETERS_ID then exit;
    Info.SPagePos := SourceFile.Position;
    SourceFile.Read(Info.SPage, SizeOf(Info.SPage));
    SourceFile.Seek(Info.SPagePos + Info.SPage.Segments + 27, soFromBeginning);
    SourceFile.Read(Info.Tag.ID, SizeOf(Info.Tag.ID));
    { Read Vorbis tag }
    if Info.Tag.ID = VORBIS_TAG_ID then ReadTag(SourceFile, Info);
    { Get total number of samples }
    Info.Samples := GetSamples(SourceFile);
    Result := true;
  finally
    SourceFile.Free;
  end;
end;

{ --------------------------------------------------------------------------- }

function GetTrack(const TrackString: string): Byte;
var
  Index, Value, Code: Integer;
begin
  { Extract track from string }
  Index := Pos('/', TrackString);
  if Index = 0 then Val(TrackString, Value, Code)
  else Val(Copy(TrackString, 1, Index), Value, Code);
  if Code = 0 then Result := Value
  else Result := 0;
end;

{ --------------------------------------------------------------------------- }

function BuildTag(const Info: FileInfo): TStringStream;
var
  Index, Fields, Size: Integer;
  FieldData: string;
begin
  { Build Vorbis tag }
  Result := TStringStream.Create('');
  Fields := 0;
  for Index := 1 to VORBIS_FIELD_COUNT do
    if Info.Tag.FieldData[Index] <> '' then Inc(Fields);
  { Write frame ID, vendor info and number of fields }
  Result.Write(Info.Tag.ID, SizeOf(Info.Tag.ID));
  Size := Length(Info.Tag.FieldData[0]);
  Result.Write(Size, SizeOf(Size));
  Result.WriteString(Info.Tag.FieldData[0]);
  Result.Write(Fields, SizeOf(Fields));
  { Write tag fields }
  for Index := 1 to VORBIS_FIELD_COUNT do
    if Info.Tag.FieldData[Index] <> '' then
    begin
      FieldData := VORBIS_FIELD[Index] +
        '=' + Info.Tag.FieldData[Index];
      Size := Length(FieldData);
      Result.Write(Size, SizeOf(Size));
      Result.WriteString(FieldData);
    end;
end;

{ --------------------------------------------------------------------------- }

procedure SetLacingValues(var Info: FileInfo; const NewTagSize: Integer);
var
  Index, Position, Value: Integer;
  Buffer: array [1..$FF] of Byte;
begin
  { Set new lacing values for the second Ogg page }
  Position := 1;
  Value := 0;
  for Index := Info.SPage.Segments downto 1 do
  begin
    if Info.SPage.LacingValues[Index] < $FF then
    begin
      Position := Index;
      Value := 0;
    end;
    Inc(Value, Info.SPage.LacingValues[Index]);
  end;
  Value := Value + NewTagSize -
    (Info.TagEndPos - Info.SPagePos - Info.SPage.Segments - 27);
  { Change lacing values at the beginning }
  for Index := 1 to Value div $FF do Buffer[Index] := $FF;
  Buffer[(Value div $FF) + 1] := Value mod $FF;
  if Position < Info.SPage.Segments then
    for Index := Position + 1 to Info.SPage.Segments do
      Buffer[Index - Position + (Value div $FF) + 1] :=
        Info.SPage.LacingValues[Index];
  Info.SPage.Segments := Info.SPage.Segments - Position + (Value div $FF) + 1;
  for Index := 1 to Info.SPage.Segments do
    Info.SPage.LacingValues[Index] := Buffer[Index];
end;

{ --------------------------------------------------------------------------- }

procedure CalculateCRC(var CRC: Cardinal; const Data; Size: Cardinal);
var
  Buffer: ^Byte;
  Index: Cardinal;
begin
  { Calculate CRC through data }
  Buffer := Addr(Data);
  for Index := 1 to Size do
  begin
    CRC := (CRC shl 8) xor CRC_TABLE[((CRC shr 24) and $FF) xor Buffer^];
    Inc(Buffer);
  end;
end;

{ --------------------------------------------------------------------------- }

procedure SetCRC(const Destination: TFileStreamEx; Info: FileInfo);
var
  Index: Integer;
  Value: Cardinal;
  Data: array [1..$FF] of Byte;
begin
  { Calculate and set checksum for Vorbis tag }
  Value := 0;
  CalculateCRC(Value, Info.SPage, Info.SPage.Segments + 27);
  Destination.Seek(Info.SPagePos + Info.SPage.Segments + 27, soFromBeginning);
  for Index := 1 to Info.SPage.Segments do
    if Info.SPage.LacingValues[Index] > 0 then
    begin
      Destination.Read(Data, Info.SPage.LacingValues[Index]);
      CalculateCRC(Value, Data, Info.SPage.LacingValues[Index]);
    end;
  Destination.Seek(Info.SPagePos + 22, soFromBeginning);
  Destination.Write(Value, SizeOf(Value));
end;

{ --------------------------------------------------------------------------- }

function RebuildFile(FileName: String; Tag: TStream; Info: FileInfo): Boolean;
var
  Source, Destination: TFileStreamEx;
  BufferName: String;
begin
  { Rebuild the file with the new Vorbis tag }
  Result := false;
  if (not mbFileExists(FileName)) or (mbFileSetReadOnly(FileName, False) <> True) then exit;
  try
    { Create file streams }
    BufferName := FileName + '~';
    Source := TFileStreamEx.Create(FileName, fmOpenRead);
    Destination := TFileStreamEx.Create(BufferName, fmCreate);
    { Copy data blocks }
    Destination.CopyFrom(Source, Info.SPagePos);
    Destination.Write(Info.SPage, Info.SPage.Segments + 27);
    Destination.CopyFrom(Tag, 0);
    Source.Seek(Info.TagEndPos, soFromBeginning);
    Destination.CopyFrom(Source, Source.Size - Info.TagEndPos);
    SetCRC(Destination, Info);
    Source.Free;
    Destination.Free;
    { Replace old file and delete temporary file }
    if (mbDeleteFile(FileName)) and (mbRenameFile(BufferName, FileName)) then
      Result := true
    else
      raise Exception.Create('');
  except
    { Access error }
    if mbFileExists(BufferName) then mbDeleteFile(BufferName);
  end;
end;

{ ********************** Private functions & procedures ********************* }

procedure TOggVorbis.FResetData;
begin
  { Reset variables }
  FFileSize := 0;
  FChannelModeID := 0;
  FSampleRate := 0;
  FBitRateNominal := 0;
  FSamples := 0;
  FID3v2Size := 0;
  FTitle := '';
  FArtist := '';
  FAlbum := '';
  FTrack := 0;
  FDate := '';
  FGenre := '';
  FComment := '';
  FVendor := '';
end;

{ --------------------------------------------------------------------------- }

function TOggVorbis.FGetChannelMode: string;
begin
  if FChannelModeID > 2 then Result := VORBIS_MODE[3] else
     Result := VORBIS_MODE[FChannelModeID];
end;

{ --------------------------------------------------------------------------- }

function TOggVorbis.FGetDuration: Double;
begin
  { Calculate duration time }
  if FSamples > 0 then
    if FSampleRate > 0 then
      Result := FSamples / FSampleRate
    else
      Result := 0
  else
    if (FBitRateNominal > 0) and (FChannelModeID > 0) then
      Result := (FFileSize - FID3v2Size) /
        FBitRateNominal / FChannelModeID / 125 * 2
    else
      Result := 0;
end;

{ --------------------------------------------------------------------------- }

function TOggVorbis.FGetBitRate: Word;
begin
  { Calculate average bit rate }
  Result := 0;
  if FGetDuration > 0 then
    Result := Round((FFileSize - FID3v2Size) / FGetDuration / 125);
end;

{ --------------------------------------------------------------------------- }

function TOggVorbis.FHasID3v2: Boolean;
begin
  { Check for ID3v2 tag }
  Result := FID3v2Size > 0;
end;

{ --------------------------------------------------------------------------- }

function TOggVorbis.FIsValid: Boolean;
begin
  { Check for file correctness }
  Result := (FChannelModeID in [VORBIS_CM_MONO, VORBIS_CM_STEREO, VORBIS_CM_MULTICHANNEL]) and
    (FSampleRate > 0) and (FGetDuration > 0.1) and (FGetBitRate > 0);
end;

{ ********************** Public functions & procedures ********************** }

constructor TOggVorbis.Create;
begin
  { Object constructor }
  FResetData;
  inherited;
end;

{ --------------------------------------------------------------------------- }

destructor TOggVorbis.Destroy;
begin
  { Object destructor }
  inherited;
end;

{ --------------------------------------------------------------------------- }

function TOggVorbis.ReadFromFile(const FileName: String): Boolean;
var
  Info: FileInfo;
begin
  { Read data from file }
  Result := false;
  FResetData;
  FillChar(Info, SizeOf(Info), 0);
  if GetInfo(FileName, Info) then
  begin
    { Fill variables }
    FFileSize := Info.FileSize;
    FChannelModeID := Info.Parameters.ChannelMode;
    FSampleRate := Info.Parameters.SampleRate;
    FBitRateNominal := Info.Parameters.BitRateNominal div 1000;
    FSamples := Info.Samples;
    FID3v2Size := Info.ID3v2Size;
    FTitle := Info.Tag.FieldData[1];
    if Info.Tag.FieldData[2] <> '' then FArtist := Info.Tag.FieldData[2]
    else FArtist := Info.Tag.FieldData[8];
    FAlbum := Info.Tag.FieldData[3];
    FTrack := GetTrack(Info.Tag.FieldData[4]);
    FDate := Info.Tag.FieldData[5];
    FGenre := Info.Tag.FieldData[6];
    if Info.Tag.FieldData[7] <> '' then FComment := Info.Tag.FieldData[7]
    else FComment := Info.Tag.FieldData[9];
    FVendor := Info.Tag.FieldData[0];
    Result := true;
  end;
end;

{ --------------------------------------------------------------------------- }

function TOggVorbis.SaveTag(const FileName: String): Boolean;
var
  Info: FileInfo;
  Tag: TStringStream;
begin
  { Save Vorbis tag }
  Result := false;
  FillChar(Info, SizeOf(Info), 0);
  if GetInfo(FileName, Info) then
  begin
    { Prepare tag data and save to file }
    Info.Tag.FieldData[1] := Trim(FTitle);
    Info.Tag.FieldData[2] := Trim(FArtist);
    Info.Tag.FieldData[3] := Trim(FAlbum);
    if FTrack > 0 then Info.Tag.FieldData[4] := IntToStr(FTrack)
    else Info.Tag.FieldData[4] := '';
    Info.Tag.FieldData[5] := Trim(FDate);
    Info.Tag.FieldData[6] := Trim(FGenre);
    Info.Tag.FieldData[7] := Trim(FComment);
    Info.Tag.FieldData[8] := '';
    Info.Tag.FieldData[9] := '';
    Tag := BuildTag(Info);
    Info.SPage.Checksum := 0;
    SetLacingValues(Info, Tag.Size);
    Result := RebuildFile(FileName, Tag, Info);
    Tag.Free;
  end;
end;

{ --------------------------------------------------------------------------- }

function TOggVorbis.ClearTag(const FileName: String): Boolean;
begin
  { Clear Vorbis tag }
  FTitle := '';
  FArtist := '';
  FAlbum := '';
  FTrack := 0;
  FDate := '';
  FGenre := '';
  FComment := '';
  Result := SaveTag(FileName);
end;

{ --------------------------------------------------------------------------- }

function TOggVorbis.FGetRatio: Double;
begin
  { Get compression ratio }
  if FIsValid then
    //Result := FFileSize / (FSamples * FChannelModeID * FBitsPerSample / 8 + 44) * 100
    Result := FFileSize / (FSamples * (FChannelModeID * 16 / 8) + 44) * 100
  else
    Result := 0;
end;

{ --------------------------------------------------------------------------- }

function TOggVorbis.FGetEncoder: String;
begin
       if FVendor = 'Xiphophorus libVorbis I 20000508' then Result := '1.0 beta 1 or beta 2'
  else if FVendor = 'Xiphophorus libVorbis I 20001031' then Result := '1.0 beta 3'
  else if FVendor = 'Xiphophorus libVorbis I 20010225' then Result := '1.0 beta 4'
  else if FVendor = 'Xiphophorus libVorbis I 20010615' then Result := '1.0 rc1'
  else if FVendor = 'Xiphophorus libVorbis I 20010813' then Result := '1.0 rc2'
  else if FVendor = 'Xiphophorus libVorbis I 20010816 (gtune 1)' then Result := '1.0 RC2 GT1'
  else if FVendor = 'Xiphophorus libVorbis I 20011014 (GTune 2)' then Result := '1.0 RC2 GT2'
  else if FVendor = 'Xiphophorus libVorbis I 20011217' then Result := '1.0 rc3'
  else if FVendor = 'Xiphophorus libVorbis I 20011231' then Result := '1.0 rc3'
//prolly an earlier build of 1.0
//else if FVendor = 'Xiph.Org libVorbis I 20020711' then Result := '1.0'
  else if FVendor = 'Xiph.Org libVorbis I 20020717' then Result := '1.0'
  else if FVendor = 'Xiph.Org/Sjeng.Org libVorbis I 20020717 (GTune 3, beta 1)' then Result := '1.0 GT3b1'
  else if FVendor = 'Xiph.Org libVorbis I 20030308' then Result := 'Post 1.0 CVS'
  else if FVendor = 'Xiph.Org libVorbis I 20030909 (1.0.1)' then Result := '1.0.1'
  else if FVendor = 'Xiph.Org libVorbis I 20030909' then Result := '1.0.1'
  else if FVendor = 'Xiph.Org/Sjeng.Org libVorbis I 20030909 (GTune 3, beta 2) EXPERIMENTAL' then Result := 'Experimental GT3b2'
  else if FVendor = 'Xiph.Org libVorbis I 20031230 (1.0.1)' then Result := 'Post 1.0.1 CVS'
  else if FVendor = 'Xiph.Org/Sjeng.Org libVorbis I 20031230 (GTune 3, beta 2)' then Result := 'GT3b2'
  else if FVendor = 'AO; aoTuV b2 [20040420]  (based on Xiph.Org''s 1.0.1)' then Result := '1.0.1 aoTuV beta 2'
  else if FVendor = 'Xiph.Org libVorbis I 20040629' then Result := '1.1'
  else if FVendor = 'Xiph.Org libVorbis I 20040920' then Result := '1.1 with impulse_trigger_profile'
  else if FVendor = 'AO; aoTuV b3 [20041120] (based on Xiph.Org''s libVorbis)' then Result := '1.1 aoTuV beta 3'
  else Result := FVendor;
end;

{ --------------------------------------------------------------------------- }

end.
