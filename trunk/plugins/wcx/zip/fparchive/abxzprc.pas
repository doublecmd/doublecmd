(* ***** BEGIN LICENSE BLOCK *****
 * Compress item to .zipx archive
 *
 * Copyright (C) 2015-2016 Alexander Koblov (alexx2000@mail.ru)
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:

 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * ***** END LICENSE BLOCK ***** *)

{**********************************************************}
{* ABBREVIA: AbXzPrc.pas                                  *}
{**********************************************************}
{* ABBREVIA: TZipHashStream class                         *}
{**********************************************************}

unit AbXzPrc;

{$mode delphi}

interface

uses
  Classes, SysUtils, BufStream, AbZipTyp, AbArcTyp;

type

  { TZipHashStream }

  TZipHashStream = class(TReadBufStream)
  private
    FSize: Int64;
    FHash: LongInt;
    FOnProgress: TAbProgressEvent;
  public
    constructor Create(ASource : TStream); reintroduce;
    function Read(var ABuffer; ACount : LongInt) : Integer; override;
    property OnProgress : TAbProgressEvent read FOnProgress write FOnProgress;
    property Hash: LongInt read FHash;
  end;

procedure DoCompressXz(Archive : TAbZipArchive; Item : TAbZipItem; OutStream, InStream : TStream);

implementation

uses
  AbXz, AbDfBase, AbExcept;

procedure DoCompressXz(Archive : TAbZipArchive; Item : TAbZipItem; OutStream, InStream : TStream);
var
  ASource: TZipHashStream;
  LzmaCompression: TLzmaCompression;
begin
  Item.CompressionMethod := cmXz;
  ASource := TZipHashStream.Create(InStream);
  try
    ASource.OnProgress := Archive.OnProgress;
    LzmaCompression := TLzmaCompression.Create(ASource, OutStream);
    try
      LzmaCompression.Code(Item.UncompressedSize);
    finally
      LzmaCompression.Free;
    end;
    Item.CRC32 := not ASource.Hash;
  finally
    ASource.Free;
  end;
end;

{ TZipHashStream }

constructor TZipHashStream.Create(ASource: TStream);
begin
  FHash := -1;
  FSize := ASource.Size;
  inherited Create(ASource);
end;

function TZipHashStream.Read(var ABuffer; ACount: LongInt): Integer;
var
  Abort: Boolean = False;
begin
  Result := inherited Read(ABuffer, ACount);
  AbUpdateCRCBuffer(FHash, ABuffer, Result);
  if Assigned(FOnProgress) then
  begin
    FOnProgress(GetPosition * 100 div FSize, Abort);
    if Abort then raise EAbUserAbort.Create;
  end;
end;

end.

