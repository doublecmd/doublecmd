{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library                                                         }
{ Class TDTS - for manipulating with DTS Files                                }
{                                                                             }
{ http://mac.sourceforge.net/atl/                                             }
{ e-mail: macteam@users.sourceforge.net                                       }
{                                                                             }
{ Copyright (c) 2005 by Gambit                                                }
{                                                                             }
{ Version 1.1 (April 2005) by Gambit                                          }
{   - updated to unicode file access                                          }
{                                                                             }
{ Version 1.0 (10 January 2005)                                               }
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

unit DTS;

interface

uses
  Classes, SysUtils, DCClassesUtf8;

const
  BIRATES: array[0..31] of Integer = (32, 56, 64, 96, 112, 128, 192, 224, 256,
                                      320, 384, 448, 512, 576, 640, 768, 960,
                                      1024, 1152, 1280, 1344, 1408, 1411, 1472,
                                      1536, 1920, 2048, 3072, 3840, 0, -1, 1);
                                                                  //open, variable, lossless
type
	{ Class TDTS }
  TDTS = class(TObject)
  private
    { Private declarations }
    FFileSize: Int64;
    FValid: Boolean;

    FChannels: Cardinal;
    FBits: Cardinal;
    FSampleRate: Cardinal;

    FBitrate: Word;
    FDuration: Double;

    function FGetRatio: Double;
    procedure FResetData;

  public
    { Public declarations }
    constructor Create;                                        { Create object }
    destructor Destroy; override;                             { Destroy object }

    function ReadFromFile(const FileName: String): Boolean;  { Load header }

    property FileSize: Int64 read FFileSize;
    property Valid: Boolean	read FValid;

    property Channels: Cardinal read FChannels;
    property Bits: Cardinal read FBits;
    property SampleRate: Cardinal read FSampleRate;

    property Bitrate: Word read FBitrate;
    property Duration: Double read FDuration;
    property Ratio: Double read FGetRatio;             { Compression ratio (%) }
	end;

implementation


{ ********************** Private functions & procedures ********************* }

procedure TDTS.FResetData;
begin
	{ Reset all data }
  FFileSize := 0;
  FValid := False;

  FChannels := 0;
  FBits := 0;
  FSampleRate := 0;

  FBitrate := 0;
  FDuration := 0;
end;


{ ********************** Public functions & procedures ********************** }

constructor TDTS.Create;
begin
  { Create object }
  inherited;
  FResetData;
end;

(* -------------------------------------------------------------------------- *)

destructor TDTS.Destroy;
begin
  inherited;
end;

(* -------------------------------------------------------------------------- *)

function TDTS.ReadFromFile(const FileName: String): Boolean;
var
  f: TFileStreamEx;
  SignatureChunk: Cardinal;
  tehWord: Word;
  gayDTS: array[0..7] of Byte;
begin
  Result := False;
  FResetData;

  f:=nil;

  try
    f := TFileStreamEx.create(FileName, fmOpenRead or fmShareDenyWrite);
                                                                                                     //0x7FFE8001
    if (f.Read(SignatureChunk, SizeOf(SignatureChunk)) = SizeOf(SignatureChunk)) and (SignatureChunk = 25230975) then
    begin
			FillChar(gayDTS, SizeOf(gayDTS),0);
      f.Seek(3, soFromCurrent);
      f.Read(gayDTS, SizeOf(gayDTS));

      FFileSize := f.Size;
      FValid := TRUE;

      tehWord := gayDTS[1] or (gayDTS[0] shl 8);

      case ((tehWord and $0FC0) shr 6) of
             0: FChannels := 1;
          1..4: FChannels := 2;
          5..6: FChannels := 3;
          7..8: FChannels := 4;
             9: FChannels := 5;
        10..12: FChannels := 6;
            13: FChannels := 7;
        14..15: FChannels := 8;
      else FChannels := 0;
      end;

      case ((tehWord and $3C) shr 2) of
         1: FSampleRate := 8000;
         2: FSampleRate := 16000;
         3: FSampleRate := 32000;
         6: FSampleRate := 11025;
         7: FSampleRate := 22050;
         8: FSampleRate := 44100;
        11: FSampleRate := 12000;
        12: FSampleRate := 24000;
        13: FSampleRate := 48000;
      else FSampleRate := 0;
      end;

      tehWord := 0;
      tehWord := gayDTS[2] or (gayDTS[1] shl 8);

      FBitrate := BIRATES[(tehWord and $03E0) shr 5];

      tehWord := 0;
      tehWord := gayDTS[7] or (gayDTS[6] shl 8);

      case ((tehWord and $01C0) shr 6) of
          0..1: FBits := 16;
          2..3: FBits := 20;
          4..5: FBits := 24;
      else FBits := 16;
      end;

      FDuration := FFileSize * 8 / 1000 / FBitrate;

      Result := True;
    end;

  finally
    f.free;
  end;
end;

(* -------------------------------------------------------------------------- *)

function TDTS.FGetRatio: Double;
begin
  { Get compression ratio }
  if FValid then
    Result := FFileSize / ((FDuration * FSampleRate) * (FChannels * FBits / 8) + 44) * 100
  else
    Result := 0;
end;

(* -------------------------------------------------------------------------- *)

end.
