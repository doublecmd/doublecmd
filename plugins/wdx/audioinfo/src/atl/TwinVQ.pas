{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library                                                         }
{ Class TTwinVQ - for extracting information from TwinVQ file header          }
{                                                                             }
{ http://mac.sourceforge.net/atl/                                             }
{ e-mail: macteam@users.sourceforge.net                                       }
{                                                                             }
{ Copyright (c) 2000-2002 by Jurgen Faul                                      }
{ Copyright (c) 2003-2005 by The MAC Team                                     }
{                                                                             }
{ Version 1.3 (April 2005) by Gambit                                          }
{   - updated to unicode file access                                          }
{                                                                             }
{ Version 1.2 (April 2004) by Gambit                                          }
{   - Added Ratio property                                                    }
{                                                                             }
{ Version 1.1 (13 August 2002)                                                }
{   - Added property Album                                                    }
{   - Support for Twin VQ 2.0                                                 }
{                                                                             }
{ Version 1.0 (6 August 2001)                                                 }
{   - File info: channel mode, bit rate, sample rate, file size, duration     }
{   - Tag info: title, comment, author, copyright, compressed file name       }
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

unit TwinVQ;

interface

uses
  Classes, SysUtils, DCClassesUtf8;

const
  { Used with ChannelModeID property }
  TWIN_CM_MONO = 1;                                     { Index for mono mode }
  TWIN_CM_STEREO = 2;                                 { Index for stereo mode }

  { Channel mode names }
  TWIN_MODE: array [0..2] of string = ('Unknown', 'Mono', 'Stereo');

type
  { Class TTwinVQ }
  TTwinVQ = class(TObject)
    private
      { Private declarations }
      FValid: Boolean;
      FChannelModeID: Byte;
      FBitRate: Byte;
      FSampleRate: Word;
      FFileSize: Cardinal;
      FDuration: Double;
      FTitle: string;
      FComment: string;
      FAuthor: string;
      FCopyright: string;
      FOriginalFile: string;
      FAlbum: string;
      procedure FResetData;
      function FGetChannelMode: string;
      function FIsCorrupted: Boolean;
      function FGetRatio: Double;
    public
      { Public declarations }
      constructor Create;                                     { Create object }
      function ReadFromFile(const FileName: String): Boolean;   { Load header }
      property Valid: Boolean read FValid;             { True if header valid }
      property ChannelModeID: Byte read FChannelModeID;   { Channel mode code }
      property ChannelMode: string read FGetChannelMode;  { Channel mode name }
      property BitRate: Byte read FBitRate;                  { Total bit rate }
      property SampleRate: Word read FSampleRate;          { Sample rate (hz) }
      property FileSize: Cardinal read FFileSize;         { File size (bytes) }
      property Duration: Double read FDuration;          { Duration (seconds) }
      property Title: string read FTitle;                        { Title name }
      property Comment: string read FComment;                       { Comment }
      property Author: string read FAuthor;                     { Author name }
      property Copyright: string read FCopyright;                 { Copyright }
      property OriginalFile: string read FOriginalFile;  { Original file name }
      property Album: string read FAlbum;                       { Album title }
      property Corrupted: Boolean read FIsCorrupted; { True if file corrupted }
      property Ratio: Double read FGetRatio;          { Compression ratio (%) }
  end;

implementation

const
  { Twin VQ header ID }
  TWIN_ID = 'TWIN';
  
  { Max. number of supported tag-chunks }
  TWIN_CHUNK_COUNT = 6;

  { Names of supported tag-chunks }
  TWIN_CHUNK: array [1..TWIN_CHUNK_COUNT] of string =
    ('NAME', 'COMT', 'AUTH', '(c) ', 'FILE', 'ALBM');

type
  { TwinVQ chunk header }
  ChunkHeader = record
    ID: array [1..4] of Char;                                      { Chunk ID }
    Size: Cardinal;                                              { Chunk size }
  end;

  { File header data - for internal use }
  HeaderInfo = record
    { Real structure of TwinVQ file header }
    ID: array [1..4] of Char;                                 { Always "TWIN" }
    Version: array [1..8] of Char;                               { Version ID }
    Size: Cardinal;                                             { Header size }
    Common: ChunkHeader;                                { Common chunk header }
    ChannelMode: Cardinal;               { Channel mode: 0 - mono, 1 - stereo }
    BitRate: Cardinal;                                       { Total bit rate }
    SampleRate: Cardinal;                                 { Sample rate (khz) }
    SecurityLevel: Cardinal;                                       { Always 0 }
    { Extended data }
    FileSize: Cardinal;                                   { File size (bytes) }
    Tag: array [1..TWIN_CHUNK_COUNT] of string;             { Tag information }
  end;

{ ********************* Auxiliary functions & procedures ******************** }

function ReadHeader(const FileName: String; var Header: HeaderInfo): Boolean;
var
  SourceFile: TFileStreamEx;
  Transferred: Integer;
begin
  try
    Result := true;
    { Set read-access and open file }
    SourceFile := TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyWrite);
    { Read header and get file size }
    Transferred := SourceFile.Read(Header, 40);
    Header.FileSize := SourceFile.Size;
    SourceFile.Free;
    { if transfer is not complete }
    if Transferred < 40 then Result := false;
  except
    { Error }
    Result := false;
  end;
end;

{ --------------------------------------------------------------------------- }

function GetChannelModeID(const Header: HeaderInfo): Byte;
begin
  { Get channel mode from header }
  case Swap(Header.ChannelMode shr 16) of
    0: Result := TWIN_CM_MONO;
    1: Result := TWIN_CM_STEREO
    else Result := 0;
  end;
end;

{ --------------------------------------------------------------------------- }

function GetBitRate(const Header: HeaderInfo): Byte;
begin
  { Get bit rate from header }
  Result := Swap(Header.BitRate shr 16);
end;

{ --------------------------------------------------------------------------- }

function GetSampleRate(const Header: HeaderInfo): Word;
begin
  { Get real sample rate from header }
  Result := Swap(Header.SampleRate shr 16);
  case Result of
    11: Result := 11025;
    22: Result := 22050;
    44: Result := 44100;
    else Result := Result * 1000;
  end;
end;

{ --------------------------------------------------------------------------- }

function GetDuration(const Header: HeaderInfo): Double;
begin
  { Get duration from header }
  Result := Abs((Header.FileSize - Swap(Header.Size shr 16) - 20)) / 125 /
    Swap(Header.BitRate shr 16);
end;

{ --------------------------------------------------------------------------- }

function HeaderEndReached(const Chunk: ChunkHeader): Boolean;
begin
  { Check for header end }
  Result := (Ord(Chunk.ID[1]) < 32) or
    (Ord(Chunk.ID[2]) < 32) or
    (Ord(Chunk.ID[3]) < 32) or
    (Ord(Chunk.ID[4]) < 32) or
    (Chunk.ID = 'DATA');
end;

{ --------------------------------------------------------------------------- }

procedure SetTagItem(const ID, Data: string; var Header: HeaderInfo);
var
  Iterator: Byte;
begin
  { Set tag item if supported tag-chunk found }
  for Iterator := 1 to TWIN_CHUNK_COUNT do
    if TWIN_CHUNK[Iterator] = ID then Header.Tag[Iterator] := Data;
end;

{ --------------------------------------------------------------------------- }

procedure ReadTag(const FileName: String; var Header: HeaderInfo);
var
  SourceFile: TFileStreamEx;
  Chunk: ChunkHeader;
  Data: array [1..250] of Char;
begin
  try
    { Set read-access, open file }
    SourceFile := TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyWrite);
    SourceFile.Seek(16, soFromBeginning);
    repeat
    begin
      FillChar(Data, SizeOf(Data), 0);
      { Read chunk header }
      SourceFile.Read(Chunk, 8);
      { Read chunk data and set tag item if chunk header valid }
      if HeaderEndReached(Chunk) then break;
      SourceFile.Read(Data, Swap(Chunk.Size shr 16) mod SizeOf(Data));
      SetTagItem(Chunk.ID, Data, Header);
    end;
    until SourceFile.Position >= SourceFile.Size;
    SourceFile.Free;
  except
  end;
end;

{ ********************** Private functions & procedures ********************* }

procedure TTwinVQ.FResetData;
begin
  FValid := false;
  FChannelModeID := 0;
  FBitRate := 0;
  FSampleRate := 0;
  FFileSize := 0;
  FDuration := 0;
  FTitle := '';
  FComment := '';
  FAuthor := '';
  FCopyright := '';
  FOriginalFile := '';
  FAlbum := '';
end;

{ --------------------------------------------------------------------------- }

function TTwinVQ.FGetChannelMode: string;
begin
  Result := TWIN_MODE[FChannelModeID];
end;

{ --------------------------------------------------------------------------- }

function TTwinVQ.FIsCorrupted: Boolean;
begin
  { Check for file corruption }
  Result := (FValid) and
    ((FChannelModeID = 0) or
    (FBitRate < 8) or (FBitRate > 192) or
    (FSampleRate < 8000) or (FSampleRate > 44100) or
    (FDuration < 0.1) or (FDuration > 10000));
end;

{ ********************** Public functions & procedures ********************** }

constructor TTwinVQ.Create;
begin
  inherited;
  FResetData;
end;

{ --------------------------------------------------------------------------- }

function TTwinVQ.ReadFromFile(const FileName: String): Boolean;
var
  Header: HeaderInfo;
begin
  { Reset data and load header from file to variable }
  FResetData;
  Result := ReadHeader(FileName, Header);
  { Process data if loaded and header valid }
  if (Result) and (Header.ID = TWIN_ID) then
  begin
    FValid := true;
    { Fill properties with header data }
    FChannelModeID := GetChannelModeID(Header);
    FBitRate := GetBitRate(Header);
    FSampleRate := GetSampleRate(Header);
    FFileSize := Header.FileSize;
    FDuration := GetDuration(Header);
    { Get tag information and fill properties }
    ReadTag(FileName, Header);
    FTitle := Trim(Header.Tag[1]);
    FComment := Trim(Header.Tag[2]);
    FAuthor := Trim(Header.Tag[3]);
    FCopyright := Trim(Header.Tag[4]);
    FOriginalFile := Trim(Header.Tag[5]);
    FAlbum := Trim(Header.Tag[6]);
  end;
end;

{ --------------------------------------------------------------------------- }

function TTwinVQ.FGetRatio: Double;
begin
  { Get compression ratio }
  if FValid then
    Result := FFileSize / ((FDuration * FSampleRate) * (FChannelModeID * 16 / 8) + 44) * 100
  else
    Result := 0;
end;

{ --------------------------------------------------------------------------- }

end.
