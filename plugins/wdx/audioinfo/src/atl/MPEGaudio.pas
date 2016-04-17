{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library                                                         }
{ Class TMPEGaudio - for manipulating with MPEG audio file information        }
{                                                                             }
{ http://mac.sourceforge.net/atl/                                             }
{ e-mail: macteam@users.sourceforge.net                                       }
{                                                                             }
{ Copyright (c) 2000-2002 by Jurgen Faul                                      }
{ Copyright (c) 2003-2005 by The MAC Team                                     }
{                                                                             }
{ Version 2.1 (April 2005) by Gambit                                          }
{   - updated to unicode file access                                          }
{                                                                             }
{ Version 2.00 (December 2004) by e-w@re                                      }
{   - returns the correct MPEG data position in file                          }
{   - added property MPEGstart -> returns start of MPEG data in file          }
{   - added property MPEGend   -> returns end of MPEG data in file            }
{                                                                             }
{ Version 1.99 (April 2004) by Gambit                                         }
{   - Improved LAME detection                                                 }
{      (checks for the LAME string in the padding)                            }
{                                                                             }
{ Version 1.91 (April 2004) by Gambit                                         }
{   - Added Ratio property                                                    }
{                                                                             }
{ Version 1.9 (22 February 2004) by Gambit                                    }
{   - Added Samples property                                                  }
{                                                                             }
{ Version 1.8 (29 June 2003) by Gambit                                        }
{   - Reads ape tags in mp3 files                                             }
{                                                                             }
{ Version 1.7 (4 November 2002)                                               }
{   - Ability to recognize QDesign MPEG audio encoder                         }
{   - Fixed bug with MPEG Layer II                                            }
{   - Fixed bug with very big files                                           }
{                                                                             }
{ Version 1.6 (23 May 2002)                                                   }
{   - Improved reading performance (up to 50% faster)                         }
{                                                                             }
{ Version 1.1 (11 September 2001)                                             }
{   - Improved encoder guessing for CBR files                                 }
{                                                                             }
{ Version 1.0 (31 August 2001)                                                }
{   - Support for MPEG audio (versions 1, 2, 2.5, layers I, II, III)          }
{   - Support for Xing & FhG VBR                                              }
{   - Ability to guess audio encoder (Xing, FhG, LAME, Blade, GoGo, Shine)    }
{   - Class TID3v1: reading & writing support for ID3v1 tags                  }
{   - Class TID3v2: reading & writing support for ID3v2 tags                  }
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

unit MPEGaudio;

interface

uses
  Classes, SysUtils, ID3v1, ID3v2, APEtag;

const
  { Table for bit rates }
  MPEG_BIT_RATE: array [0..3, 0..3, 0..15] of Word =
    (
    { For MPEG 2.5 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 0)),
    { Reserved }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
    { For MPEG 2 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 0)),
    { For MPEG 1 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384, 0),
    (0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448, 0))
    );

  { Sample rate codes }
  MPEG_SAMPLE_RATE_LEVEL_3 = 0;                                     { Level 3 }
  MPEG_SAMPLE_RATE_LEVEL_2 = 1;                                     { Level 2 }
  MPEG_SAMPLE_RATE_LEVEL_1 = 2;                                     { Level 1 }
  MPEG_SAMPLE_RATE_UNKNOWN = 3;                               { Unknown value }

  { Table for sample rates }
  MPEG_SAMPLE_RATE: array [0..3, 0..3] of Word =
    (
    (11025, 12000, 8000, 0),                                   { For MPEG 2.5 }
    (0, 0, 0, 0),                                                  { Reserved }
    (22050, 24000, 16000, 0),                                    { For MPEG 2 }
    (44100, 48000, 32000, 0)                                     { For MPEG 1 }
    );

  { VBR header ID for Xing/FhG }
  VBR_ID_XING = 'Xing';                                         { Xing VBR ID }
  VBR_ID_FHG = 'VBRI';                                           { FhG VBR ID }

  { MPEG version codes }
  MPEG_VERSION_2_5 = 0;                                            { MPEG 2.5 }
  MPEG_VERSION_UNKNOWN = 1;                                 { Unknown version }
  MPEG_VERSION_2 = 2;                                                { MPEG 2 }
  MPEG_VERSION_1 = 3;                                                { MPEG 1 }

  { MPEG version names }
  MPEG_VERSION: array [0..3] of string =
    ('MPEG 2.5', 'MPEG ?', 'MPEG 2', 'MPEG 1');

  { MPEG layer codes }
  MPEG_LAYER_UNKNOWN = 0;                                     { Unknown layer }
  MPEG_LAYER_III = 1;                                             { Layer III }
  MPEG_LAYER_II = 2;                                               { Layer II }
  MPEG_LAYER_I = 3;                                                 { Layer I }

  { MPEG layer names }
  MPEG_LAYER: array [0..3] of string =
    ('Layer ?', 'Layer III', 'Layer II', 'Layer I');

  { Channel mode codes }
  MPEG_CM_STEREO = 0;                                                { Stereo }
  MPEG_CM_JOINT_STEREO = 1;                                    { Joint Stereo }
  MPEG_CM_DUAL_CHANNEL = 2;                                    { Dual Channel }
  MPEG_CM_MONO = 3;                                                    { Mono }
  MPEG_CM_UNKNOWN = 4;                                         { Unknown mode }

  { Channel mode names }
  MPEG_CM_MODE: array [0..4] of string =
    ('Stereo', 'Joint Stereo', 'Dual Channel', 'Mono', 'Unknown');

  { Extension mode codes (for Joint Stereo) }
  MPEG_CM_EXTENSION_OFF = 0;                        { IS and MS modes set off }
  MPEG_CM_EXTENSION_IS = 1;                             { Only IS mode set on }
  MPEG_CM_EXTENSION_MS = 2;                             { Only MS mode set on }
  MPEG_CM_EXTENSION_ON = 3;                          { IS and MS modes set on }
  MPEG_CM_EXTENSION_UNKNOWN = 4;                     { Unknown extension mode }

  { Emphasis mode codes }
  MPEG_EMPHASIS_NONE = 0;                                              { None }
  MPEG_EMPHASIS_5015 = 1;                                          { 50/15 ms }
  MPEG_EMPHASIS_UNKNOWN = 2;                               { Unknown emphasis }
  MPEG_EMPHASIS_CCIT = 3;                                         { CCIT J.17 }

  { Emphasis names }
  MPEG_EMPHASIS: array [0..3] of string =
    ('None', '50/15 ms', 'Unknown', 'CCIT J.17');

  { Encoder codes }
  MPEG_ENCODER_UNKNOWN = 0;                                 { Unknown encoder }
  MPEG_ENCODER_XING = 1;                                               { Xing }
  MPEG_ENCODER_FHG = 2;                                                 { FhG }
  MPEG_ENCODER_LAME = 3;                                               { LAME }
  MPEG_ENCODER_BLADE = 4;                                             { Blade }
  MPEG_ENCODER_GOGO = 5;                                               { GoGo }
  MPEG_ENCODER_SHINE = 6;                                             { Shine }
  MPEG_ENCODER_QDESIGN = 7;                                         { QDesign }

  { Encoder names }
  MPEG_ENCODER: array [0..7] of string =
    ('Unknown', 'Xing', 'FhG', 'LAME', 'Blade', 'GoGo', 'Shine', 'QDesign');

type
  hFileInt = Integer;

  { Xing/FhG VBR header data }
  VBRData = record
    Found: Boolean;                                { True if VBR header found }
    ID: array [1..4] of Char;                   { Header ID: "Xing" or "VBRI" }
    Frames: Integer;                                 { Total number of frames }
    Bytes: Integer;                                   { Total number of bytes }
    Scale: Byte;                                         { VBR scale (1..100) }
    VendorID: string;                                { Vendor ID (if present) }
  end;

  { MPEG frame header data}
  FrameData = record
    Found: Boolean;                                     { True if frame found }
    Position: Integer;                           { Frame position in the file }
    Size: Word;                                          { Frame size (bytes) }
    Xing: Boolean;                                     { True if Xing encoder }
    Data: array [1..4] of Byte;                 { The whole frame header data }
    VersionID: Byte;                                        { MPEG version ID }
    LayerID: Byte;                                            { MPEG layer ID }
    ProtectionBit: Boolean;                        { True if protected by CRC }
    BitRateID: Word;                                            { Bit rate ID }
    SampleRateID: Word;                                      { Sample rate ID }
    PaddingBit: Boolean;                               { True if frame padded }
    PrivateBit: Boolean;                                  { Extra information }
    ModeID: Byte;                                           { Channel mode ID }
    ModeExtensionID: Byte;             { Mode extension ID (for Joint Stereo) }
    CopyrightBit: Boolean;                        { True if audio copyrighted }
    OriginalBit: Boolean;                            { True if original media }
    EmphasisID: Byte;                                           { Emphasis ID }
  end;

  { Class TMPEGaudio }
  TMPEGaudio = class(TObject)
    private
      { Private declarations }
      FFileLength: Integer;
      FVendorID: string;
      FVBR: VBRData;
      FFrame: FrameData;
      FMPEGStart: Int64;
      FMPEGEnd: Int64;
      FAudioSizeTag: Int64;
      FID3v1: TID3v1;
      FID3v2: TID3v2;
      FAPEtag: TAPEtag;
      procedure FResetData;
      function FGetVersion: string;
      function FGetLayer: string;
      function FGetBitRate: Word;
      function FGetSampleRate: Word;
      function FGetChannelMode: string;
      function FGetEmphasis: string;
      function FGetFrames: Integer;
      function FGetDuration: Double;
      function FGetVBREncoderID: Byte;
      function FGetCBREncoderID: Byte;
      function FGetEncoderID: Byte;
      function FGetEncoder: string;
      function FGetValid: Boolean;
      function FGetSamples: Cardinal;
      function FGetRatio: Double;
    public
      { Public declarations }
      constructor Create;                                     { Create object }
      destructor Destroy; override;                          { Destroy object }
      function ReadFromFile(const FileName: String): Boolean; { Load data }
      property FileLength: Integer read FFileLength;    { File length (bytes) }
      property VBR: VBRData read FVBR;                      { VBR header data }
      property Frame: FrameData read FFrame;              { Frame header data }
      property ID3v1: TID3v1 read FID3v1;                    { ID3v1 tag data }
      property ID3v2: TID3v2 read FID3v2;                    { ID3v2 tag data }
      property APEtag: TAPEtag read FAPEtag;                   { APE tag data }
      property Version: string read FGetVersion;          { MPEG version name }
      property Layer: string read FGetLayer;                { MPEG layer name }
      property BitRate: Word read FGetBitRate;            { Bit rate (kbit/s) }
      property SampleRate: Word read FGetSampleRate;       { Sample rate (hz) }
      property ChannelMode: string read FGetChannelMode;  { Channel mode name }
      property Emphasis: string read FGetEmphasis;            { Emphasis name }
      property Frames: Integer read FGetFrames;      { Total number of frames }
      property Duration: Double read FGetDuration;      { Song duration (sec) }
      property EncoderID: Byte read FGetEncoderID;       { Guessed encoder ID }
      property Encoder: string read FGetEncoder;       { Guessed encoder name }
      property Valid: Boolean read FGetValid;       { True if MPEG file valid }
      property Samples: Cardinal read FGetSamples;
      property Ratio: Double read FGetRatio;          { Compression ratio (%) }
      property MPEGStart: Int64 read FMPEGStart;{Returns start pos of MPEG data}
      property MPEGEnd: Int64 read FMPEGEnd;   { Returns end pos of MPEG data }
      property AudioSizeTag: Int64 read FAudioSizeTag;  { Returns ID3v2 TSIZ value or 0 }
  end;

implementation

uses
  DCOSUtils;

const
  { Limitation constants }
  MAX_MPEG_FRAME_LENGTH = 1729;                      { Max. MPEG frame length }
  MIN_MPEG_BIT_RATE = 8;                                { Min. bit rate value }
  MAX_MPEG_BIT_RATE = 448;                              { Max. bit rate value }
  MIN_ALLOWED_DURATION = 0.1;                      { Min. song duration value }

  { VBR Vendor ID strings }
  VENDOR_ID_LAME = 'LAME';                                         { For LAME }
  VENDOR_ID_GOGO_NEW = 'GOGO';                               { For GoGo (New) }
  VENDOR_ID_GOGO_OLD = 'MPGE';                               { For GoGo (Old) }

  hINVALID_HANDLE_VALUE = hFileInt(-1);

{ ********************* Auxiliary functions & procedures ******************** }

function IsFrameHeader(const HeaderData: array of Byte): Boolean;
begin
  { Check for valid frame header }
  if ((HeaderData[0] and $FF) <> $FF) or
    ((HeaderData[1] and $E0) <> $E0) or
    (((HeaderData[1] shr 3) and 3) = 1) or
    (((HeaderData[1] shr 1) and 3) = 0) or
    ((HeaderData[2] and $F0) = $F0) or
    ((HeaderData[2] and $F0) = 0) or
    (((HeaderData[2] shr 2) and 3) = 3) or
    ((HeaderData[3] and 3) = 2) then
    Result := false
  else
    Result := true;
end;

{ --------------------------------------------------------------------------- }

procedure DecodeHeader(const HeaderData: array of Byte; var Frame: FrameData);
begin
  { Decode frame header data }
  Move(HeaderData, Frame.Data, SizeOf(Frame.Data));
  Frame.VersionID := (HeaderData[1] shr 3) and 3;
  Frame.LayerID := (HeaderData[1] shr 1) and 3;
  Frame.ProtectionBit := (HeaderData[1] and 1) <> 1;
  Frame.BitRateID := HeaderData[2] shr 4;
  Frame.SampleRateID := (HeaderData[2] shr 2) and 3;
  Frame.PaddingBit := ((HeaderData[2] shr 1) and 1) = 1;
  Frame.PrivateBit := (HeaderData[2] and 1) = 1;
  Frame.ModeID := (HeaderData[3] shr 6) and 3;
  Frame.ModeExtensionID := (HeaderData[3] shr 4) and 3;
  Frame.CopyrightBit := ((HeaderData[3] shr 3) and 1) = 1;
  Frame.OriginalBit := ((HeaderData[3] shr 2) and 1) = 1;
  Frame.EmphasisID := HeaderData[3] and 3;
end;

{ --------------------------------------------------------------------------- }

function ValidFrameAt(const Index: Word; Data: array of Byte): Boolean;
var
  HeaderData: array [1..4] of Byte;
begin
  { Check for frame at given position }
  HeaderData[1] := Data[Index];
  HeaderData[2] := Data[Index + 1];
  HeaderData[3] := Data[Index + 2];
  HeaderData[4] := Data[Index + 3];
  if IsFrameHeader(HeaderData) then Result := true
  else Result := false;
end;

{ --------------------------------------------------------------------------- }

function GetCoefficient(const Frame: FrameData): Byte;
begin
  { Get frame size coefficient }
  if Frame.VersionID = MPEG_VERSION_1 then
    if Frame.LayerID = MPEG_LAYER_I then Result := 48
    else Result := 144
  else
    if Frame.LayerID = MPEG_LAYER_I then Result := 24
    else if Frame.LayerID = MPEG_LAYER_II then Result := 144
    else Result := 72;
end;

{ --------------------------------------------------------------------------- }

function GetBitRate(const Frame: FrameData): Word;
begin
  { Get bit rate }
  Result := MPEG_BIT_RATE[Frame.VersionID, Frame.LayerID, Frame.BitRateID];
end;

{ --------------------------------------------------------------------------- }

function GetSampleRate(const Frame: FrameData): Word;
begin
  { Get sample rate }
  Result := MPEG_SAMPLE_RATE[Frame.VersionID, Frame.SampleRateID];
end;

{ --------------------------------------------------------------------------- }

function GetPadding(const Frame: FrameData): Byte;
begin
  { Get frame padding }
  if Frame.PaddingBit then
    if Frame.LayerID = MPEG_LAYER_I then Result := 4
    else Result := 1
  else Result := 0;
end;

{ --------------------------------------------------------------------------- }

function GetFrameLength(const Frame: FrameData): Word;
var
  Coefficient, BitRate, SampleRate, Padding: Word;
begin
  { Calculate MPEG frame length }
  Coefficient := GetCoefficient(Frame);
  BitRate := GetBitRate(Frame);
  SampleRate := GetSampleRate(Frame);
  Padding := GetPadding(Frame);
  Result := Trunc(Coefficient * BitRate * 1000 / SampleRate) + Padding;
end;

{ --------------------------------------------------------------------------- }

function IsXing(const Index: Word; Data: array of Byte): Boolean;
begin
  { Get true if Xing encoder }
  Result :=
    (Data[Index] = 0) and
    (Data[Index + 1] = 0) and
    (Data[Index + 2] = 0) and
    (Data[Index + 3] = 0) and
    (Data[Index + 4] = 0) and
    (Data[Index + 5] = 0);
end;

{ --------------------------------------------------------------------------- }

function GetXingInfo(const Index: Word; Data: array of Byte): VBRData;
begin
  { Extract Xing VBR info at given position }
  FillChar(Result, SizeOf(Result), 0);
  Result.Found := true;
  Result.ID := VBR_ID_XING;
  Result.Frames :=
    Data[Index + 8] * $1000000 +
    Data[Index + 9] * $10000 +
    Data[Index + 10] * $100 +
    Data[Index + 11];
  Result.Bytes :=
    Data[Index + 12] * $1000000 +
    Data[Index + 13] * $10000 +
    Data[Index + 14] * $100 +
    Data[Index + 15];
  Result.Scale := Data[Index + 119];
  { Vendor ID can be not present }
  Result.VendorID :=
    Chr(Data[Index + 120]) +
    Chr(Data[Index + 121]) +
    Chr(Data[Index + 122]) +
    Chr(Data[Index + 123]) +
    Chr(Data[Index + 124]) +
    Chr(Data[Index + 125]) +
    Chr(Data[Index + 126]) +
    Chr(Data[Index + 127]);
end;

{ --------------------------------------------------------------------------- }

function GetFhGInfo(const Index: Word; Data: array of Byte): VBRData;
begin
  { Extract FhG VBR info at given position }
  FillChar(Result, SizeOf(Result), 0);
  Result.Found := true;
  Result.ID := VBR_ID_FHG;
  Result.Scale := Data[Index + 9];
  Result.Bytes :=
    Data[Index + 10] * $1000000 +
    Data[Index + 11] * $10000 +
    Data[Index + 12] * $100 +
    Data[Index + 13];
  Result.Frames :=
    Data[Index + 14] * $1000000 +
    Data[Index + 15] * $10000 +
    Data[Index + 16] * $100 +
    Data[Index + 17];
end;

{ --------------------------------------------------------------------------- }

function FindVBR(const Index: Word; Data: array of Byte): VBRData;
begin
  { Check for VBR header at given position }
  FillChar(Result, SizeOf(Result), 0);
  if Chr(Data[Index]) +
    Chr(Data[Index + 1]) +
    Chr(Data[Index + 2]) +
    Chr(Data[Index + 3]) = VBR_ID_XING then Result := GetXingInfo(Index, Data);
  if Chr(Data[Index]) +
    Chr(Data[Index + 1]) +
    Chr(Data[Index + 2]) +
    Chr(Data[Index + 3]) = VBR_ID_FHG then Result := GetFhGInfo(Index, Data);
end;

{ --------------------------------------------------------------------------- }

function GetVBRDeviation(const Frame: FrameData): Byte;
begin
  { Calculate VBR deviation }
  if Frame.VersionID = MPEG_VERSION_1 then
    if Frame.ModeID <> MPEG_CM_MONO then Result := 36
    else Result := 21
  else
    if Frame.ModeID <> MPEG_CM_MONO then Result := 21
    else Result := 13;
end;

{ --------------------------------------------------------------------------- }

function FindFrame(const Data: array of Byte; var VBR: VBRData): FrameData;
var
  HeaderData: array [1..4] of Byte;
  Iterator, VBRIdx: Integer;
begin
  { Search for valid frame }
  FillChar(Result, SizeOf(Result), 0);
  Move(Data, HeaderData, SizeOf(HeaderData));
  for Iterator := 0 to SizeOf(Data) - MAX_MPEG_FRAME_LENGTH do
  begin
    { Decode data if frame header found }
    if IsFrameHeader(HeaderData) then
    begin
      DecodeHeader(HeaderData, Result);
      { Check for next frame and try to find VBR header }
      VBRIdx := Iterator + GetFrameLength(Result);
      if (VBRIdx < SizeOf(Data)) and ValidFrameAt(VBRIdx, Data) then
      begin
        Result.Found := true;
        Result.Position := Iterator;
        Result.Size := GetFrameLength(Result);
        Result.Xing := IsXing(Iterator + SizeOf(HeaderData), Data);
        VBR := FindVBR(Iterator + GetVBRDeviation(Result), Data);
        break;
      end;
    end;
    { Prepare next data block }
    HeaderData[1] := HeaderData[2];
    HeaderData[2] := HeaderData[3];
    HeaderData[3] := HeaderData[4];
    HeaderData[4] := Data[Iterator + SizeOf(HeaderData)];
  end;
end;

{ --------------------------------------------------------------------------- }

function FindVendorID(const Data: array of Byte; Size: Word): string;
var
  Iterator: Integer;
  VendorID: string;
begin
  { Search for vendor ID }
  Result := '';
  if (SizeOf(Data) - Size - 8) < 0 then Size := SizeOf(Data) - 8;
  for Iterator := 0 to Size do
  begin
    VendorID :=
      Chr(Data[SizeOf(Data) - Iterator - 8]) +
      Chr(Data[SizeOf(Data) - Iterator - 7]) +
      Chr(Data[SizeOf(Data) - Iterator - 6]) +
      Chr(Data[SizeOf(Data) - Iterator - 5]);
    if VendorID = VENDOR_ID_LAME then
    begin
      Result := VendorID +
        Chr(Data[SizeOf(Data) - Iterator - 4]) +
        Chr(Data[SizeOf(Data) - Iterator - 3]) +
        Chr(Data[SizeOf(Data) - Iterator - 2]) +
        Chr(Data[SizeOf(Data) - Iterator - 1]);
      break;
    end;
    if VendorID = VENDOR_ID_GOGO_NEW then
    begin
      Result := VendorID;
      break;
    end;
  end;
end;

{ ********************** Private functions & procedures ********************* }

procedure TMPEGaudio.FResetData;
begin
  { Reset all variables }
  FFileLength := 0;
  FMPEGStart := 0;
  FMPEGEnd := 0;
  FAudioSizeTag := 0;
  FVendorID := '';
  FillChar(FVBR, SizeOf(FVBR), 0);
  FillChar(FFrame, SizeOf(FFrame), 0);
  FFrame.VersionID := MPEG_VERSION_UNKNOWN;
  FFrame.SampleRateID := MPEG_SAMPLE_RATE_UNKNOWN;
  FFrame.ModeID := MPEG_CM_UNKNOWN;
  FFrame.ModeExtensionID := MPEG_CM_EXTENSION_UNKNOWN;
  FFrame.EmphasisID := MPEG_EMPHASIS_UNKNOWN;
  FID3v1.ResetData;
  FID3v2.ResetData;
  FAPEtag.ResetData;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetVersion: string;
begin
  { Get MPEG version name }
  Result := MPEG_VERSION[FFrame.VersionID];
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetLayer: string;
begin
  { Get MPEG layer name }
  Result := MPEG_LAYER[FFrame.LayerID];
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetBitRate: Word;
begin
  { Get bit rate, calculate average bit rate if VBR header found }
  if (FVBR.Found) and (FVBR.Frames > 0) then
    Result := Round((FVBR.Bytes / FVBR.Frames - GetPadding(FFrame)) *
      GetSampleRate(FFrame) / GetCoefficient(FFrame) / 1000)
  else
    Result := GetBitRate(FFrame);
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetSampleRate: Word;
begin
  { Get sample rate }
  Result := GetSampleRate(FFrame);
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetChannelMode: string;
begin
  { Get channel mode name }
  Result := MPEG_CM_MODE[FFrame.ModeID];
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetEmphasis: string;
begin
  { Get emphasis name }
  Result := MPEG_EMPHASIS[FFrame.EmphasisID];
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetFrames: Integer;
var
  MPEGSize: Integer;
begin
  { Get total number of frames, calculate if VBR header not found }
  if FVBR.Found then
    Result := FVBR.Frames
  else
  begin
    MPEGSize := FMPEGEnd - FMPEGStart;
    Result := (MPEGSize - FFrame.Position) div GetFrameLength(FFrame);
  end;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetDuration: Double;
var
  MPEGSize: Int64;
begin
  { Calculate song duration }
  if FFrame.Found then
    if (FVBR.Found) and (FVBR.Frames > 0) then
      Result := FVBR.Frames * GetCoefficient(FFrame) * 8 /
        GetSampleRate(FFrame)
    else
    begin
      MPEGSize := FMPEGEnd - FMPEGStart;
      Result := (MPEGSize - FFrame.Position) / GetBitRate(FFrame) / 1000 * 8;
    end
  else
    Result := 0;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetVBREncoderID: Byte;
begin
  { Guess VBR encoder and get ID }
  Result := 0;
  if Copy(FVBR.VendorID, 1, 4) = VENDOR_ID_LAME then
    Result := MPEG_ENCODER_LAME;
  if Copy(FVBR.VendorID, 1, 4) = VENDOR_ID_GOGO_NEW then
    Result := MPEG_ENCODER_GOGO;
  if Copy(FVBR.VendorID, 1, 4) = VENDOR_ID_GOGO_OLD then
    Result := MPEG_ENCODER_GOGO;
  if (FVBR.ID = VBR_ID_XING) and
    (Copy(FVBR.VendorID, 1, 4) <> VENDOR_ID_LAME) and
    (Copy(FVBR.VendorID, 1, 4) <> VENDOR_ID_GOGO_NEW) and
    (Copy(FVBR.VendorID, 1, 4) <> VENDOR_ID_GOGO_OLD) then
    Result := MPEG_ENCODER_XING;
  if FVBR.ID = VBR_ID_FHG then
    Result := MPEG_ENCODER_FHG;
  if (Copy(FVendorID, 1, 4) = VENDOR_ID_LAME) then
    Result := MPEG_ENCODER_LAME;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetCBREncoderID: Byte;
begin
  { Guess CBR encoder and get ID }
  Result := MPEG_ENCODER_FHG;
  if (FFrame.OriginalBit) and
    (FFrame.ProtectionBit) then
    Result := MPEG_ENCODER_LAME;
  if (GetBitRate(FFrame) <= 160) and
    (FFrame.ModeID = MPEG_CM_STEREO) then
    Result := MPEG_ENCODER_BLADE;
  if (FFrame.CopyrightBit) and
    (FFrame.OriginalBit) and
    (not FFrame.ProtectionBit) then
    Result := MPEG_ENCODER_XING;
  if (FFrame.Xing) and
    (FFrame.OriginalBit) then
    Result := MPEG_ENCODER_XING;
  if FFrame.LayerID = MPEG_LAYER_II then
    Result := MPEG_ENCODER_QDESIGN;
  if (FFrame.ModeID = MPEG_CM_DUAL_CHANNEL) and
    (FFrame.ProtectionBit) then
    Result := MPEG_ENCODER_SHINE;
  if Copy(FVendorID, 1, 4) = VENDOR_ID_LAME then
    Result := MPEG_ENCODER_LAME;
  if Copy(FVendorID, 1, 4) = VENDOR_ID_GOGO_NEW then
    Result := MPEG_ENCODER_GOGO;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetEncoderID: Byte;
begin
  { Get guessed encoder ID }
  if FFrame.Found then
    if FVBR.Found then Result := FGetVBREncoderID
    else Result := FGetCBREncoderID
  else
    Result := 0;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetEncoder: string;
var
  VendorID: string;
begin
  { Get guessed encoder name and encoder version for LAME }
  Result := MPEG_ENCODER[FGetEncoderID];
  if FVBR.VendorID <> '' then VendorID := FVBR.VendorID;
  if FVendorID <> '' then VendorID := FVendorID;
  if (FGetEncoderID = MPEG_ENCODER_LAME) and
    (Length(VendorID) >= 8) and
    (VendorID[5] in ['0'..'9']) and
    (VendorID[6] = '.') and
    (VendorID[7] in ['0'..'9']) and
    (VendorID[8] in ['0'..'9']) then
    Result :=
      Result + #32 +
      VendorID[5] +
      VendorID[6] +
      VendorID[7] +
      VendorID[8];
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetValid: Boolean;
begin
  { Check for right MPEG file data }
  Result :=
    (FFrame.Found) and
    (FGetBitRate >= MIN_MPEG_BIT_RATE) and
    (FGetBitRate <= MAX_MPEG_BIT_RATE) and
    (FGetDuration >= MIN_ALLOWED_DURATION);
end;

{ ********************** Public functions & procedures ********************** }

constructor TMPEGaudio.Create;
begin
  { Object constructor }
  inherited;
  FID3v1 := TID3v1.Create;
  FID3v2 := TID3v2.Create;
  FAPEtag := TAPEtag.Create;
  FResetData;
end;

{ --------------------------------------------------------------------------- }

destructor TMPEGaudio.Destroy;
begin
  { Object destructor }
  FID3v1.Free;
  FID3v2.Free;
  FAPEtag.Free;
  inherited;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.ReadFromFile(const FileName: String): Boolean;
var
    SourceFile: hFileInt;
    Data: array [1..MAX_MPEG_FRAME_LENGTH * 2] of Byte;
    Transferred: DWORD;
    Position : Int64;
    tmp : Integer;
    str: string;
    Value: Int64;
    Code: Integer;
begin
    FResetData;
    SourceFile := hINVALID_HANDLE_VALUE;
    try
        SourceFile := mbFileOpen(FileName, fmOpenRead  or fmShareDenyWrite);
        if (SourceFile = hINVALID_HANDLE_VALUE) then
        begin
            Result := false;
            Exit;
        end;

        { At first search for tags & Lyrics3 then search for a MPEG frame and VBR data }
        if (FID3v2.ReadFromFile(FileName)) and
           (FID3v1.ReadFromFile(FileName)) then
        begin
            FFileLength := mbFileSize(FileName);
            Position := FID3v2.Size;
            FileSeek(SourceFile, Position, soFromBeginning);
            Transferred:= FileRead(SourceFile, Data, SizeOf(Data));
            FFrame := FindFrame(Data, FVBR);
            // Search for vendor ID at the beginning
            FVendorID := FindVendorID(Data, FFrame.Size * 5);

            { patched by e-w@re }
            { Try to find the first frame if no frame at the beginning found ]}
            if (not FFrame.Found) and (Transferred = SizeOf(Data)) then
            repeat
                Transferred:= FileRead(SourceFile, Data, SizeOf(Data));
                Inc(Position, Transferred);
                FFrame := FindFrame(Data, FVBR);
            until (FFrame.Found) or (Transferred < SizeOf(Data));

            if FFrame.Found then
            begin
                FFrame.Position := Position + FFrame.Position;
                FMPEGStart := FFrame.Position;
                tmp := FID3v1.TagSize;
                FMPEGEnd := FFileLength - tmp;
            end;

            if FID3v2.Exists then
            begin
                str := FID3v2.TSIZ;
                if Length(str) > 0 then
                try
                    Val(str, Value, Code);
                    if (Code = 0) then FAudioSizeTag := Value;
                except
                    // ignore
                end;
            end;

            { Search for vendor ID at the end if CBR encoded }
            if (FFrame.Found) and (FVendorID = '') then
            begin
              if not FID3v1.Exists then Position := FFileLength - SizeOf(Data)
              else Position := FFileLength - SizeOf(Data) - 128;
              FileSeek(SourceFile, Position, soFromBeginning);
              Transferred:= FileRead(SourceFile, Data, SizeOf(Data));
              FVendorID := FindVendorID(Data, FFrame.Size * 5);
            end;
      end;
      FileClose(SourceFile);
      Result := true;
    except
        if (SourceFile <> hINVALID_HANDLE_VALUE) then FileClose(SourceFile);
        Result := false;
    end;
    if not FFrame.Found then FResetData;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetSamples: Cardinal;
begin
  Result := 0;
  if FFrame.Found then
  // rework, it's the same
    if (FVBR.Found) and (FVBR.Frames > 0) then
      Result := FVBR.Frames * GetCoefficient(FFrame) * 8
    else
      Result := FGetFrames * GetCoefficient(FFrame) * 8;
end;

{ --------------------------------------------------------------------------- }

function TMPEGaudio.FGetRatio: Double;
begin
  { Get compression ratio }
  if FGetValid then
  begin
    //Result := FFileSize / (FGetSamples * FChannels * FBits / 8 + 44) * 100
    if ChannelMode = 'Mono' then
      Result := FFileLength / (FGetSamples * (1 * 16 / 8) + 44) * 100
    else
      Result := FFileLength / (FGetSamples * (2 * 16 / 8) + 44) * 100;
  end
  else
    Result := 0;
end;

{ --------------------------------------------------------------------------- }

end.
