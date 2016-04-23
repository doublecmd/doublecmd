{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library                                                         }
{ Class TAC3 - for manipulating with AC3 Files                                }
{                                                                             }
{ http://mac.sourceforge.net/atl/                                             }
{ e-mail: macteam@users.sourceforge.net                                       }
{                                                                             }
{ Copyright (c) 2005 by Gambit                                                }
{                                                                             }
{ Version 1.1 (April 2005) by Gambit                                          }
{   - updated to unicode file access                                          }
{                                                                             }
{ Version 1.0 (05 January 2005)                                               }
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

unit AC3;

interface

uses
  Classes, SysUtils, DCClassesUtf8;

const
  BIRATES: array[0..18] of Integer = (32, 40, 48, 56, 64, 80, 96, 112, 128, 160,
                                      192, 224, 256, 320, 384, 448, 512, 576, 640);

type
	{ Class TAC3 }
  TAC3 = class(TObject)
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
    constructor Create;                                     { Create object }
    destructor Destroy; override;                          { Destroy object }

    function ReadFromFile(const FileName: String): Boolean;   { Load header }

    property FileSize: Int64 read FFileSize;
    property Valid: Boolean	read FValid;

    property Channels: Cardinal read FChannels;
    property Bits: Cardinal read FBits;
    property SampleRate: Cardinal read FSampleRate;

    property Bitrate: Word read FBitrate;
    property Duration: Double read FDuration;
    property Ratio: Double read FGetRatio;          { Compression ratio (%) }
	end;

implementation


{ ********************** Private functions & procedures ********************* }

procedure TAC3.FResetData;
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

constructor TAC3.Create;
begin
  { Create object }
  inherited;
  FResetData;
end;

(* -------------------------------------------------------------------------- *)

destructor TAC3.Destroy;
begin
  inherited;
end;

(* -------------------------------------------------------------------------- *)

function TAC3.ReadFromFile(const FileName: String): Boolean;
var
  f: TFileStreamEx;
  SignatureChunk: Word;
  tehByte: Byte;
begin
  Result := False;
  FResetData;

  f:=nil;

  try
    f := TFileStreamEx.create(FileName, fmOpenRead or fmShareDenyWrite);
                                                                                                     //0x0B77
    if (f.Read(SignatureChunk, SizeOf(SignatureChunk)) = SizeOf(SignatureChunk)) and (SignatureChunk = 30475) then
    begin
			FillChar(tehByte, SizeOf(tehByte),0);
      f.Seek(2, soFromCurrent);
      f.Read(tehByte, SizeOf(tehByte));

      FFileSize := f.Size;
      FValid := TRUE;

      case (tehByte and $C0) of
          0: FSampleRate := 48000;
        $40: FSampleRate := 44100;
        $80: FSampleRate := 32000;
      else FSampleRate := 0;
      end;

      FBitrate := BIRATES[(tehByte and $3F) shr 1];

			FillChar(tehByte, SizeOf(tehByte),0);
      f.Seek(1, soFromCurrent);
      f.Read(tehByte, SizeOf(tehByte));

      case (tehByte and $E0) of
          0: FChannels := 2;
        $20: FChannels := 1;
        $40: FChannels := 2;
        $60: FChannels := 3;
        $80: FChannels := 3;
        $A0: FChannels := 4;
        $C0: FChannels := 4;
        $E0: FChannels := 5;
      else FChannels := 0;
      end;

      FBits := 16;
      FDuration := FFileSize * 8 / 1000 / FBitrate;

      Result := True;
    end;

  finally
    f.free;
  end;
end;

(* -------------------------------------------------------------------------- *)

function TAC3.FGetRatio: Double;
begin
  { Get compression ratio }
  if FValid then
    Result := FFileSize / ((FDuration * FSampleRate) * (FChannels * FBits / 8) + 44) * 100
  else
    Result := 0;
end;

(* -------------------------------------------------------------------------- *)

end.
