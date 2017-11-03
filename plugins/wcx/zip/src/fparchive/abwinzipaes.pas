(* ***** BEGIN LICENSE BLOCK *****
 *
 * WinZip AES decryption stream
 *
 * Copyright (C) 2017 Alexander Koblov (alexx2000@mail.ru)
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

unit AbWinZipAes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DCPrijndael, HMAC;

const
  Ab_WinZipAesID : Word = $9901;

type

  { TWinZipAesRec }

  PWinZipAesRec = ^TWinZipAesRec;
  TWinZipAesRec = packed record
    Version: Word;
    Vendor: Word;
    Strength: Byte;
    Method: Word;
  end;

  { TAbWinZipAesDecryptStream }

  TAbWinZipAesDecryptStream = class(TStream)
  private
    FKey          : TBytes;
    FOwnsStream   : Boolean;
    FReady        : Boolean;
    FStream       : TStream;
    FDataStream   : TStream;
    FPassword     : AnsiString;
    FContext      : THMAC_Context;
    FDecoder      : TDCP_rijndael;
    FExtraField   : TWinZipAesRec;
  public
    constructor Create(aStream : TStream;
                       aExtraField: PWinZipAesRec;
                       const aPassword : AnsiString);
    destructor Destroy; override;

    function IsValid : Boolean;
    function Verify : Boolean;

    function Read(var aBuffer; aCount : Longint) : Longint; override;
    function Seek(aOffset : Longint; aOrigin : Word) : Longint; override;
    function Write(const aBuffer; aCount : Longint) : Longint; override;

    property ExtraField : TWinZipAesRec read FExtraField;
    property OwnsStream : Boolean read FOwnsStream write FOwnsStream;
  end;

implementation

uses
  AbUnzOutStm, DCPcrypt2, SHA1, Hash, kdf;

const
  CTR : array[0..15] of Byte = (1, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0);

const
  MAC_LENGTH = 10;
  PWD_VER_LENGTH = 2;
  KEY_LENGTH: array[1..3] of Byte = (16, 24, 32);
  SALT_LENGTH: array[1..3] of Byte = (8, 12, 16);

{ TAbWinZipAesDecryptStream }

constructor TAbWinZipAesDecryptStream.Create(aStream: TStream;
  aExtraField: PWinZipAesRec; const aPassword: AnsiString);
begin
  inherited Create;

  FStream := aStream;
  FExtraField := aExtraField^;
  FPassword := aPassword;

  FDecoder := TDCP_rijndael.Create(nil);
end;

destructor TAbWinZipAesDecryptStream.Destroy;
begin
  FDecoder.Free;
  FDataStream.Free;
  if FOwnsStream then
    FStream.Free;
  inherited Destroy;
end;

function TAbWinZipAesDecryptStream.IsValid: Boolean;
var
  F: WordRec;
  Salt: AnsiString;
  HashDesc: PHashDesc;
  AKeyLength: Integer;
  ASaltLength: Integer;
  AExtraLength: Integer;
begin
  // Integer mode value indicating AES encryption strength
  if not FExtraField.Strength in [1..3] then Exit(False);

  AKeyLength := KEY_LENGTH[FExtraField.Strength];
  ASaltLength := SALT_LENGTH[FExtraField.Strength];
  AExtraLength := AKeyLength * 2 + PWD_VER_LENGTH;

  SetLength(FKey, AExtraLength);
  HashDesc:= FindHash_by_ID(_SHA1);

  // Read salt value
  SetLength(Salt, ASaltLength);
  FStream.Read(Salt[1], ASaltLength);
  // Read password verification value
  FStream.Read({%H-}F, PWD_VER_LENGTH);

  pbkdf2(HashDesc, Pointer(FPassword), Length(FPassword),
                   Pointer(Salt), Length(Salt), 1000, FKey[0], AExtraLength);

  Result := (FKey[AExtraLength - 2] = F.Lo) and (FKey[AExtraLength - 1] = F.Hi);

  if Result then
  begin
    FReady := True;
    FDecoder.Init(FKey[0], AKeyLength * 8, @CTR[0]);
    // Initialize for authentication using second key part
    hmac_init(FContext, HashDesc, @FKey[AKeyLength], AKeyLength);
    // Create encrypted file data stream
    AExtraLength := ASaltLength + PWD_VER_LENGTH + MAC_LENGTH;
    FDataStream := TAbUnzipSubsetStream.Create(FStream, FStream.Size - AExtraLength);
  end
  else begin
    FReady := False;
    FStream.Seek(-(ASaltLength + PWD_VER_LENGTH), soCurrent);
  end
end;

function TAbWinZipAesDecryptStream.Verify: Boolean;
var
  AMac: THashDigest;
  ABuffer: array[0..MAC_LENGTH - 1] of Byte;
begin
  hmac_final(FContext, {%H-}AMac);
  FStream.Read({%H-}ABuffer[0], MAC_LENGTH);
  Result := CompareByte(ABuffer[0], AMac[0], MAC_LENGTH) = 0;
end;

function TAbWinZipAesDecryptStream.Read(var aBuffer; aCount: Longint): Longint;
begin
  Assert(FReady, 'TAbWinZipAesDecryptStream.Read: the stream header has not been verified');
  Result := FDataStream.Read(aBuffer, aCount);
  if Result > 0 then
  begin
    hmac_updateXL(FContext, @aBuffer, Result);
    FDecoder.DecryptCTR(aBuffer, aBuffer, Result);
  end;
end;

function TAbWinZipAesDecryptStream.Seek(aOffset: Longint; aOrigin: Word): Longint;
begin
  Result := FDataStream.Seek(aOffset, aOrigin);
end;

function TAbWinZipAesDecryptStream.Write(const aBuffer; aCount: Longint): Longint;
begin
  Assert(False, 'TAbWinZipAesDecryptStream.Write: the stream is read-only');
  Result := 0;
end;

end.

