{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library                                                         }
{ Class TOptimFROG - for manipulating with OptimFROG file information         }
{                                                                             }
{ http://mac.sourceforge.net/atl/                                             }
{ e-mail: macteam@users.sourceforge.net                                       }
{                                                                             }
{ Copyright (c) 2003-2005 by Erik Stenborg                                    }
{                                                                             }
{ Version 1.1 (April 2005) by Gambit                                          }
{   - updated to unicode file access                                          }
{                                                                             }
{ Version 1.0 (10 July 2003)                                                  }
{   - Support for OptimFROG files via modification of TMonkey class by Jurgen }
{   - Class TID3v1: reading & writing support for ID3v1 tags                  }
{   - Class TID3v2: reading & writing support for ID3v2 tags                  }
{   - Class TAPEtag: reading & writing support for APE tags                   }
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

unit OptimFROG;

interface

uses
  Classes, SysUtils, ID3v1, ID3v2, APEtag, DCClassesUtf8;

const
  OFR_COMPRESSION: array [0..9] of String = ('fast', 'normal', 'high', 'extra',
    'best', 'ultra', 'insane', 'highnew', 'extranew', 'bestnew');

  OFR_BITS: array [0..10] of ShortInt = (8, 8, 16, 16, 24, 24, 32, 32,
    -32, -32, -32); //negative value corresponds to floating point type.

  OFR_CHANNELMODE: array [0..1] of String = ('Mono', 'Stereo');

type
  { Real structure of OptimFROG header }
  TOfrHeader = packed record
    ID: array [1..4] of Char;                                 { Always 'OFR ' }
    Size: Cardinal;
    Length: Cardinal;
    HiLength: Word;
    SampleType, ChannelMode: Byte;
    SampleRate: Integer;
    EncoderID: Word;
    CompressionID: Byte;
  end;

  { Class TOptimFrog }
  TOptimFrog = class(TObject)
    private
      { Private declarations }
      FFileLength: Int64;
      FHeader: TOfrHeader;
      FID3v1: TID3v1;
      FID3v2: TID3v2;
      FAPEtag: TAPEtag;
      procedure FResetData;
      function FGetValid: Boolean;
      function FGetVersion: string;
      function FGetCompression: string;
      function FGetBits: ShortInt;
      function FGetChannelMode: string;
      function FGetSamples: Int64;
      function FGetDuration: Double;
      function FGetRatio: Double;
      function FGetSampleRate: Integer;
      function FGetChannels: Byte;
      function FGetBitrate: Integer;
    public
      { Public declarations }
      constructor Create;                                     { Create object }
      destructor Destroy; override;                          { Destroy object }
      function ReadFromFile(const FileName: String): Boolean;    {Load header }
      property FileLength: Int64 read FFileLength;      { File length (bytes) }
      property Header: TOfrHeader read FHeader;            { OptimFROG header }
      property ID3v1: TID3v1 read FID3v1;                    { ID3v1 tag data }
      property ID3v2: TID3v2 read FID3v2;                    { ID3v2 tag data }
      property APEtag: TAPEtag read FAPEtag;                   { APE tag data }
      property Valid: Boolean read FGetValid;          { True if header valid }
      property Version: string read FGetVersion;            { Encoder version }
      property Compression: string read FGetCompression;  { Compression level }
      property Bits: ShortInt read FGetBits;                { Bits per sample }
      property ChannelMode: string read FGetChannelMode;       { Channel mode }
      property Samples: Int64 read FGetSamples;           { Number of samples }
      property Duration: Double read FGetDuration;       { Duration (seconds) }
      property SampleRate: Integer read FGetSampleRate;    { Sample rate (Hz) }
      property Ratio: Double read FGetRatio;          { Compression ratio (%) }
      property Channels: Byte read FGetChannels;
      property Bitrate: Integer read FGetBitrate;
  end;

implementation

{ ********************** Private functions & procedures ********************* }

procedure TOptimFrog.FResetData;
begin
  { Reset data }
  FFileLength := 0;
  FillChar(FHeader, SizeOf(FHeader), 0);
  FID3v1.ResetData;
  FID3v2.ResetData;
  FAPEtag.ResetData;
end;

{ --------------------------------------------------------------------------- }

function TOptimFrog.FGetValid: Boolean;
begin
  Result :=
    (FHeader.ID = 'OFR ') and
    (FHeader.SampleRate > 0) and
    (FHeader.SampleType in [0..10]) and
    (FHeader.ChannelMode in [0..1]) and
    (FHeader.CompressionID shr 3 in [0..9]);
end;

{ --------------------------------------------------------------------------- }

function TOptimFrog.FGetVersion: string;
begin
  { Get encoder version }
  Result := Format('%5.3f', [((FHeader.EncoderID shr 4) + 4500) / 1000]);
end;

{ --------------------------------------------------------------------------- }

function TOptimFrog.FGetCompression: string;
begin
  { Get compression level }
  Result := OFR_COMPRESSION[FHeader.CompressionID shr 3]
end;

{ --------------------------------------------------------------------------- }

function TOptimFrog.FGetBits: ShortInt;
begin
  { Get number of bits per sample }
  Result := OFR_BITS[FHeader.SampleType]
end;

{ --------------------------------------------------------------------------- }

function TOptimFrog.FGetChannelMode: string;
begin
  { Get channel mode }
  Result := OFR_CHANNELMODE[FHeader.ChannelMode]
end;

{ --------------------------------------------------------------------------- }

function TOptimFrog.FGetSamples: Int64;
var
  Res: array [0..1] of Cardinal absolute Result;
begin
  { Get number of samples }
  Res[0] := Header.Length shr Header.ChannelMode;
  Res[1] := Header.HiLength shr Header.ChannelMode;
end;

{ --------------------------------------------------------------------------- }

function TOptimFrog.FGetDuration: Double;
begin
  { Get song duration }
  if FHeader.SampleRate > 0 then
    Result := FGetSamples / FHeader.SampleRate
  else
    Result := 0;
end;

{ --------------------------------------------------------------------------- }

function TOptimFrog.FGetSampleRate: Integer;
begin
  Result := Header.SampleRate;
end;

{ --------------------------------------------------------------------------- }

function TOptimFrog.FGetRatio: Double;
begin
  { Get compression ratio }
  if FGetValid then
    Result := FFileLength /
      (FGetSamples * ((FHeader.ChannelMode+1) * Abs(FGetBits) / 8) + 44) * 100
  else
    Result := 0;
end;

{ ********************** Public functions & procedures ********************** }

constructor TOptimFrog.Create;
begin
  { Create object }
  inherited;
  FID3v1 := TID3v1.Create;
  FID3v2 := TID3v2.Create;
  FAPEtag := TAPEtag.Create;
  FResetData;
end;

{ --------------------------------------------------------------------------- }

destructor TOptimFrog.Destroy;
begin
  { Destroy object }
  FID3v1.Free;
  FID3v2.Free;
  FAPEtag.Free;
  inherited;
end;

{ --------------------------------------------------------------------------- }

function TOptimFrog.ReadFromFile(const FileName: String): Boolean;
var
  SourceFile: TFileStreamEx;
begin
  Result := False;
  SourceFile := nil;
  try
    { Reset data and search for file tag }
    FResetData;
    FID3v1.ReadFromFile(FileName);
    FID3v2.ReadFromFile(FileName);
    FAPEtag.ReadFromFile(FileName);
    { Set read-access, open file and get file length }
    SourceFile := TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyWrite);
    FFileLength := SourceFile.Size;
    { Read header data }
    SourceFile.Seek(ID3v2.Size, soFromBeginning);
    SourceFile.Read(FHeader, SizeOf(FHeader));
    if FHeader.ID = 'OFR ' then
      Result := True;
  finally
    SourceFile.Free;
  end;
end;

{ --------------------------------------------------------------------------- }

function TOptimFrog.FGetChannels: Byte;
begin
  Result := Header.ChannelMode + 1;
end;

{ --------------------------------------------------------------------------- }

function TOptimFrog.FGetBitrate: Integer;
begin
  Result := Round(FFileLength * 8.0 / (FGetSamples / FHeader.SampleRate * 1000));
end;

{ --------------------------------------------------------------------------- }

end.
