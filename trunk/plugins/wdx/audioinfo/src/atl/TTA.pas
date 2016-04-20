{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library                                                         }
{ Class TTTA - for manipulating with TTA Files                                }
{                                                                             }
{ http://mac.sourceforge.net/atl/                                             }
{ e-mail: macteam@users.sourceforge.net                                       }
{                                                                             }
{ Copyright (c) 2004-2005 by Gambit                                           }
{                                                                             }
{ Version 1.1 (April 2005) by Gambit                                          }
{   - updated to unicode file access                                          }
{                                                                             }
{ Version 1.0 (12 August 2004)                                                }
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

unit TTA;

interface

uses
  Classes, SysUtils, ID3v1, ID3v2, APEtag, DCClassesUtf8;

type

  tta_header = packed record
    //TTAid: array[0..3] of Char;
    AudioFormat: Word;
    NumChannels: Word;
    BitsPerSample: Word;
    SampleRate: Longword;
    DataLength: Longword;
    CRC32: Longword;
  end;

	{ Class TTTA }
  TTTA = class(TObject)
  private
    { Private declarations }
    FFileSize: Int64;
    FValid: Boolean;

    FAudioFormat: Cardinal;
    FChannels: Cardinal;
    FBits: Cardinal;
    FSampleRate: Cardinal;
    FSamples: Cardinal;
    FCRC32: Cardinal;

    FBitrate: Double;
    FDuration: Double;

    FID3v1: TID3v1;
    FID3v2: TID3v2;
    FAPEtag: TAPEtag;    

    function FGetRatio: Double;
    procedure FResetData;

  public
    { Public declarations }
    constructor Create;                                     { Create object }
    destructor Destroy; override;                          { Destroy object }

    function ReadFromFile(const FileName: String): Boolean;   { Load header }

    property FileSize: Int64 read FFileSize;
    property Valid: Boolean	read FValid;

    property AudioFormat: Cardinal read FAudioFormat;
    property Channels: Cardinal read FChannels;
    property Bits: Cardinal read FBits;
    property SampleRate: Cardinal read FSampleRate;
    property Samples: Cardinal read FSamples;           { Number of samples }
    property CRC32: Cardinal read FCRC32;

    property Bitrate: Double read FBitrate;
    property Duration: Double read FDuration;
    property Ratio: Double read FGetRatio;          { Compression ratio (%) }

    property ID3v1: TID3v1 read FID3v1;                    { ID3v1 tag data }
    property ID3v2: TID3v2 read FID3v2;                    { ID3v2 tag data }
    property APEtag: TAPEtag read FAPEtag;                   { APE tag data }
	end;

implementation


{ ********************** Private functions & procedures ********************* }

procedure TTTA.FResetData;
begin
	{ Reset all data }
  FFileSize := 0;
  FValid := False;

  FAudioFormat := 0;
  FChannels := 0;
  FBits := 0;
  FSampleRate := 0;
  FSamples := 0;
  FCRC32 := 0;

  FBitrate := 0;
  FDuration := 0;

  FID3v1.ResetData;
  FID3v2.ResetData;
  FAPEtag.ResetData;
end;


{ ********************** Public functions & procedures ********************** }

constructor TTTA.Create;
begin
  { Create object }
  inherited;
  FID3v1   := TID3v1.Create;
  FID3v2   := TID3v2.Create;
  FAPEtag  := TAPEtag.Create;
  FResetData;
end;

(* -------------------------------------------------------------------------- *)

destructor TTTA.Destroy;
begin
  FID3v1.Free;
  FID3v2.Free;
  FAPEtag.Free;
  inherited;
end;

(* -------------------------------------------------------------------------- *)

function TTTA.ReadFromFile(const FileName: String): Boolean;
var
  f: TFileStreamEx;
  SignatureChunk: array[0..3] of Char;
  ttaheader: tta_header;
  TagSize: Int64;
begin
  Result := False;
  FResetData;
  // load tags first
  FID3v2.ReadFromFile(FileName);
  FID3v1.ReadFromFile(FileName);
  FAPEtag.ReadFromFile(FileName);
  // calulate total tag size
  TagSize := 0;
  if FID3v1.Exists then inc(TagSize,128);
  if FID3v2.Exists then inc(TagSize, FID3v2.Size);
  if FAPEtag.Exists then inc(TagSize, FAPETag.Size);
  // begin reading data from file
  f:=nil;

  try
    f := TFileStreamEx.create(FileName, fmOpenRead or fmShareDenyWrite);
    // seek past id3v2-tag
    if FID3v2.Exists then
    begin
      f.Seek(FID3v2.Size, soFromBeginning);
    end;

    if (f.Read(SignatureChunk, SizeOf(SignatureChunk)) = SizeOf(SignatureChunk)) and (StrLComp(SignatureChunk,'TTA1',4) = 0) then
    begin
      // start looking for chunks
			FillChar(ttaheader, SizeOf(ttaheader),0);
      f.Read(ttaheader, SizeOf(ttaheader));

      FFileSize := f.Size;
      FValid := TRUE;

      FAudioFormat := ttaheader.AudioFormat;
      FChannels := ttaheader.NumChannels;
      FBits := ttaheader.BitsPerSample;
      FSampleRate := ttaheader.SampleRate;
      FSamples := ttaheader.DataLength;
      FCRC32 := ttaheader.CRC32;

      FBitrate := FFileSize * 8 / (FSamples / FSampleRate) / 1000;
      FDuration := ttaheader.DataLength / ttaheader.SampleRate;

      Result := True;
    end;

  finally
    f.free;
  end;
end;

(* -------------------------------------------------------------------------- *)

function TTTA.FGetRatio: Double;
begin
  { Get compression ratio }
  if FValid then
    Result := FFileSize / (FSamples * (FChannels * FBits / 8) + 44) * 100
  else
    Result := 0;
end;

(* -------------------------------------------------------------------------- *)

end.
