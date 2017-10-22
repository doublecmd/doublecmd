(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbDfCryS.pas                                *}
{*********************************************************}
{* Deflate encryption streams                            *}
{*********************************************************}

unit AbDfCryS;

{$I AbDefine.inc}

interface

uses
  Classes;

type
  TAbZipEncryptHeader = array [0..11] of byte;

  TAbZipDecryptEngine = class
    private
      FReady : boolean;
      FState : array [0..2] of longint;
    protected
      procedure zdeInitState(const aPassphrase : AnsiString);
    public
      constructor Create;

      function Decode(aCh : byte) : byte;
        {-decodes a byte}
      procedure DecodeBuffer(var aBuffer; aCount : integer);
        {-decodes a buffer}

      function VerifyHeader(const aHeader     : TAbZipEncryptHeader;
                            const aPassphrase : AnsiString;
                                  aCheckValue : longint) : boolean;
        {-validate an encryption header}
    end;

  TAbDfDecryptStream = class(TStream)
    private
      FCheckValue : longint;
      FEngine     : TAbZipDecryptEngine;
      FOwnsStream : Boolean;
      FPassphrase : AnsiString;
      FReady      : boolean;
      FStream     : TStream;
    protected
    public
      constructor Create(aStream     : TStream;
                         aCheckValue : longint;
                   const aPassphrase : AnsiString);
      destructor Destroy; override;

      function IsValid : boolean;

      function Read(var aBuffer; aCount : longint) : longint; override;
      function Seek(aOffset : longint; aOrigin : word) : longint; override;
      function Write(const aBuffer; aCount : longint) : longint; override;

      property OwnsStream : Boolean
        read FOwnsStream
        write FOwnsStream;
  end;

  TAbZipEncryptEngine = class
    private
      FReady : boolean;
      FState : array [0..2] of longint;
    protected
      procedure zeeInitState(const aPassphrase : AnsiString);
    public
      constructor Create;

      function Encode(aCh : byte) : byte;
        {-encodes a byte}
      procedure EncodeBuffer(var aBuffer; aCount : integer);
        {-encodes a buffer}

      procedure CreateHeader(var aHeader     : TAbZipEncryptHeader;
                           const aPassphrase : AnsiString;
                                 aCheckValue : longint);
        {-generate an encryption header}
    end;

  TAbDfEncryptStream = class(TStream)
    private
      FBuffer  : PAnsiChar;
      FBufSize : integer;
      FEngine  : TAbZipEncryptEngine;
      FStream  : TStream;
    protected
    public
      constructor Create(aStream     : TStream;
                         aCheckValue : longint;
                   const aPassphrase : AnsiString);
      destructor Destroy; override;

      function Read(var aBuffer; aCount : longint) : longint; override;
      function Seek(aOffset : longint; aOrigin : word) : longint; override;
      function Write(const aBuffer; aCount : longint) : longint; override;
  end;

implementation

{Notes: the ZIP spec defines a couple of primitive routines for
        performing encryption. For speed Abbrevia inlines them into
        the respective methods of the encryption/decryption engines

        char crc32(long,char)
          return updated CRC from current CRC and next char

        update_keys(char):
          Key(0) <- crc32(key(0),char)
          Key(1) <- Key(1) + (Key(0) & 000000ffH)
          Key(1) <- Key(1) * 134775813 + 1
          Key(2) <- crc32(key(2),key(1) >> 24)
        end update_keys

        char decrypt_byte()
          local unsigned short temp
          temp <- Key(2) | 2
          decrypt_byte <- (temp * (temp ^ 1)) >> 8
        end decrypt_byte
}

uses
  AbUtils;

{---magic numbers from ZIP spec---}
const
  StateInit1 = 305419896;
  StateInit2 = 591751049;
  StateInit3 = 878082192;
  MagicNumber = 134775813;

{===internal encryption class========================================}
constructor TAbZipDecryptEngine.Create;
begin
  {create the ancestor}
  inherited Create;

  {we're not ready for decryption yet since a header hasn't been
   properly verified with VerifyHeader}
  FReady := false;
end;
{--------}
function TAbZipDecryptEngine.Decode(aCh : byte) : byte;
var
  Temp : longint;
begin
  {check for programming error}
  Assert(FReady,
         'TAbZipDecryptEngine.Decode: must successfully call VerifyHeader first');

  {calculate the decoded byte (uses inlined decrypt_byte)}
  Temp := (FState[2] and $FFFF) or 2;
  Result := aCh xor ((Temp * (Temp xor 1)) shr 8);

  {mix the decoded byte into the state (uses inlined update_keys)}
  FState[0] := AbUpdateCrc32(Result, FState[0]);
  FState[1] := FState[1] + (FState[0] and $FF);
  FState[1] := (FState[1] * MagicNumber) + 1;
  FState[2] := AbUpdateCrc32(FState[1] shr 24, FState[2]);
end;
{--------}
procedure TAbZipDecryptEngine.DecodeBuffer(var aBuffer; aCount : integer);
var
  i      : integer;
  Temp   : longint;
  Buffer : PAnsiChar;
  WorkState : array [0..2] of longint;
begin
  {check for programming error}
  Assert(FReady,
         'TAbZipDecryptEngine.Decode: must successfully call VerifyHeader first');

  {move the state to a local variable--for better speed}
  WorkState[0] := FState[0];
  WorkState[1] := FState[1];
  WorkState[2] := FState[2];

  {reference the buffer as a PChar--easier arithmetic}
  Buffer := @aBuffer;

  {for each byte in the buffer...}
  for i := 0 to pred(aCount) do begin

    {calculate the next decoded byte (uses inlined decrypt_byte)}
    Temp := (WorkState[2] and $FFFF) or 2;
    Buffer^ := AnsiChar(
                  byte(Buffer^) xor ((Temp * (Temp xor 1)) shr 8));

    {mix the decoded byte into the state (uses inlined update_keys)}
    WorkState[0] := AbUpdateCrc32(byte(Buffer^), WorkState[0]);
    WorkState[1] := WorkState[1] + (WorkState[0] and $FF);
    WorkState[1] := (WorkState[1] * MagicNumber) + 1;
    WorkState[2] := AbUpdateCrc32(WorkState[1] shr 24, WorkState[2]);

    {move onto the next byte}
    inc(Buffer);
  end;

  {save the state}
  FState[0] := WorkState[0];
  FState[1] := WorkState[1];
  FState[2] := WorkState[2];
end;
{--------}
function TAbZipDecryptEngine.VerifyHeader(const aHeader     : TAbZipEncryptHeader;
                                          const aPassphrase : AnsiString;
                                                aCheckValue : longint) : boolean;
type
  TLongAsBytes = packed record
    L1, L2, L3, L4 : byte
  end;
var
  i    : integer;
  Temp : longint;
  WorkHeader : TAbZipEncryptHeader;
begin
  {check for programming errors}
  Assert(aPassphrase <> '',
         'TAbZipDecryptEngine.VerifyHeader: need a passphrase');

  {initialize the decryption state}
  zdeInitState(aPassphrase);

  {decrypt the bytes in the header}
  for i := 0 to 11 do begin

    {calculate the next decoded byte (uses inlined decrypt_byte)}
    Temp := (FState[2] and $FFFF) or 2;
    WorkHeader[i] := aHeader[i] xor ((Temp * (Temp xor 1)) shr 8);

    {mix the decoded byte into the state (uses inlined update_keys)}
    FState[0] := AbUpdateCrc32(WorkHeader[i], FState[0]);
    FState[1] := FState[1] + (FState[0] and $FF);
    FState[1] := (FState[1] * MagicNumber) + 1;
    FState[2] := AbUpdateCrc32(FState[1] shr 24, FState[2]);
  end;

  {the header is valid if the twelfth byte of the decrypted header
   equals the fourth byte of the check value}
  Result := WorkHeader[11] = TLongAsBytes(aCheckValue).L4;

  {note: zips created with PKZIP prior to version 2.0 also checked
         that the tenth byte of the decrypted header equals the third
         byte of the check value}
  FReady := Result;
end;
{--------}
procedure TAbZipDecryptEngine.zdeInitState(const aPassphrase : AnsiString);
var
  i : integer;
begin
  {initialize the decryption state}
  FState[0] := StateInit1;
  FState[1] := StateInit2;
  FState[2] := StateInit3;

  {mix in the passphrase to the state (uses inlined update_keys)}
  for i := 1 to length(aPassphrase) do begin
    FState[0] := AbUpdateCrc32(byte(aPassphrase[i]), FState[0]);
    FState[1] := FState[1] + (FState[0] and $FF);
    FState[1] := (FState[1] * MagicNumber) + 1;
    FState[2] := AbUpdateCrc32(FState[1] shr 24, FState[2]);
  end;
end;
{====================================================================}


{====================================================================}
constructor TAbDfDecryptStream.Create(aStream     : TStream;
                                      aCheckValue : longint;
                                const aPassphrase : AnsiString);
begin
  {create the ancestor}
  inherited Create;

  {save the parameters}
  FStream := aStream;
  FCheckValue := aCheckValue;
  FPassphrase := aPassphrase;

  {create the decryption engine}
  FEngine := TAbZipDecryptEngine.Create;
end;
{--------}
destructor TAbDfDecryptStream.Destroy;                     {new !!.02}
begin
  FEngine.Free;
  if FOwnsStream then
    FStream.Free;
  inherited Destroy;
end;
{--------}
function TAbDfDecryptStream.IsValid : boolean;
var
  Header : TAbZipEncryptHeader;
begin
  {read the header from the stream}
  FStream.ReadBuffer(Header, sizeof(Header));

  {check to see if the decryption engine agrees it's valid}
  Result := FEngine.VerifyHeader(Header, FPassphrase, FCheckValue);

  {if it isn't valid, reposition the stream, ready for the next try}
  if not Result then begin
    FStream.Seek(-sizeof(Header), soCurrent);
    FReady := false;
  end

  {otherwise, the stream is ready for decrypting data}
  else
    FReady := true;
end;
{--------}
function TAbDfDecryptStream.Read(var aBuffer; aCount : longint) : longint;
begin
  {check for programming error}
  Assert(FReady,
         'TAbDfDecryptStream.Read: the stream header has not been verified');

  {read the data from the underlying stream}
  Result := FStream.Read(aBuffer, aCount);

  {decrypt the data}
  FEngine.DecodeBuffer(aBuffer, Result);
end;
{--------}
function TAbDfDecryptStream.Seek(aOffset : longint; aOrigin : word) : longint;
begin
  Result := FStream.Seek(aOffset, aOrigin);
end;
{--------}
function TAbDfDecryptStream.Write(const aBuffer; aCount : longint) : longint;
begin
  {check for programming error}
  Assert(false,
         'TAbDfDecryptStream.Write: the stream is read-only');
  Result := 0;
end;
{====================================================================}


{===TAbZipEncryptEngine==============================================}
constructor TAbZipEncryptEngine.Create;
begin
  {create the ancestor}
  inherited Create;

  {we're not ready for encryption yet since a header hasn't been
   properly generated with CreateHeader}
  FReady := false;
end;
{--------}
procedure TAbZipEncryptEngine.CreateHeader(
                                var aHeader     : TAbZipEncryptHeader;
                              const aPassphrase : AnsiString;
                                    aCheckValue : longint);
type
  TLongAsBytes = packed record
    L1, L2, L3, L4 : byte
  end;
var
  Ch   : byte;
  i    : integer;
  Temp : longint;
  WorkHeader : TAbZipEncryptHeader;
begin
  {check for programming errors}
  Assert(aPassphrase <> '',
         'TAbZipEncryptEngine.CreateHeader: need a passphrase');

  {set the first ten bytes of the header with random values (in fact,
   we use a random value for each byte and mix it in with the state)}

  {initialize the decryption state}
  zeeInitState(aPassphrase);

  {for the first ten bytes...}
  for i := 0 to 9 do begin

    {get a random value}
    Ch := Random( 256 );

    {calculate the XOR encoding byte (uses inlined decrypt_byte)}
    Temp := (FState[2] and $FFFF) or 2;
    Temp := (Temp * (Temp xor 1)) shr 8;

    {mix the unencoded byte into the state (uses inlined update_keys)}
    FState[0] := AbUpdateCrc32(Ch, FState[0]);
    FState[1] := FState[1] + (FState[0] and $FF);
    FState[1] := (FState[1] * MagicNumber) + 1;
    FState[2] := AbUpdateCrc32(FState[1] shr 24, FState[2]);

    {set the current byte of the header}
    WorkHeader[i] := Ch xor Temp;
  end;

  {now encrypt the first ten bytes of the header (this merely sets up
   the state so that we can encrypt the last two bytes)}

  {reinitialize the decryption state}
  zeeInitState(aPassphrase);

  {for the first ten bytes...}
  for i := 0 to 9 do begin

    {calculate the XOR encoding byte (uses inlined decrypt_byte)}
    Temp := (FState[2] and $FFFF) or 2;
    Temp := (Temp * (Temp xor 1)) shr 8;

    {mix the unencoded byte into the state (uses inlined update_keys)}
    FState[0] := AbUpdateCrc32(WorkHeader[i], FState[0]);
    FState[1] := FState[1] + (FState[0] and $FF);
    FState[1] := (FState[1] * MagicNumber) + 1;
    FState[2] := AbUpdateCrc32(FState[1] shr 24, FState[2]);

    {set the current byte of the header}
    WorkHeader[i] := WorkHeader[i] xor Temp;
  end;

  {now initialize byte 10 of the header, and encrypt it}
  Ch := TLongAsBytes(aCheckValue).L3;
  Temp := (FState[2] and $FFFF) or 2;
  Temp := (Temp * (Temp xor 1)) shr 8;
  FState[0] := AbUpdateCrc32(Ch, FState[0]);
  FState[1] := FState[1] + (FState[0] and $FF);
  FState[1] := (FState[1] * MagicNumber) + 1;
  FState[2] := AbUpdateCrc32(FState[1] shr 24, FState[2]);
  WorkHeader[10] := Ch xor Temp;

  {now initialize byte 11 of the header, and encrypt it}
  Ch := TLongAsBytes(aCheckValue).L4;
  Temp := (FState[2] and $FFFF) or 2;
  Temp := (Temp * (Temp xor 1)) shr 8;
  FState[0] := AbUpdateCrc32(Ch, FState[0]);
  FState[1] := FState[1] + (FState[0] and $FF);
  FState[1] := (FState[1] * MagicNumber) + 1;
  FState[2] := AbUpdateCrc32(FState[1] shr 24, FState[2]);
  WorkHeader[11] := Ch xor Temp;

  {we're now ready to encrypt}
  FReady := true;

  {return the header}
  aHeader := WorkHeader;
end;
{--------}
function TAbZipEncryptEngine.Encode(aCh : byte) : byte;
var
  Temp : longint;
begin
  {check for programming error}
  Assert(FReady,
         'TAbZipEncryptEngine.Encode: must call CreateHeader first');

  {calculate the encoded byte (uses inlined decrypt_byte)}
  Temp := (FState[2] and $FFFF) or 2;
  Result := aCh xor (Temp * (Temp xor 1)) shr 8;

  {mix the unencoded byte into the state (uses inlined update_keys)}
  FState[0] := AbUpdateCrc32(aCh, FState[0]);
  FState[1] := FState[1] + (FState[0] and $FF);
  FState[1] := (FState[1] * MagicNumber) + 1;
  FState[2] := AbUpdateCrc32(FState[1] shr 24, FState[2]);
end;
{--------}
procedure TAbZipEncryptEngine.EncodeBuffer(var aBuffer; aCount : integer);
var
  Ch     : byte;
  i      : integer;
  Temp   : longint;
  Buffer : PAnsiChar;
  WorkState : array [0..2] of longint;
begin
  {check for programming error}
  Assert(FReady,
         'TAbZipEncryptEngine.EncodeBuffer: must call CreateHeader first');

  {move the state to a local variable--for better speed}
  WorkState[0] := FState[0];
  WorkState[1] := FState[1];
  WorkState[2] := FState[2];

  {reference the buffer as a PChar--easier arithmetic}
  Buffer := @aBuffer;

  {for each byte in the buffer...}
  for i := 0 to pred(aCount) do begin

    {calculate the next encoded byte (uses inlined decrypt_byte)}
    Temp := (WorkState[2] and $FFFF) or 2;
    Ch := byte(Buffer^);
    Buffer^ := AnsiChar(Ch xor ((Temp * (Temp xor 1)) shr 8));

    {mix the decoded byte into the state (uses inlined update_keys)}
    WorkState[0] := AbUpdateCrc32(Ch, WorkState[0]);
    WorkState[1] := WorkState[1] + (WorkState[0] and $FF);
    WorkState[1] := (WorkState[1] * MagicNumber) + 1;
    WorkState[2] := AbUpdateCrc32(WorkState[1] shr 24, WorkState[2]);

    {move onto the next byte}
    inc(Buffer);
  end;

  {save the state}
  FState[0] := WorkState[0];
  FState[1] := WorkState[1];
  FState[2] := WorkState[2];
end;
{--------}
procedure TAbZipEncryptEngine.zeeInitState(const aPassphrase : AnsiString);
var
  i : integer;
begin
  {initialize the decryption state}
  FState[0] := StateInit1;
  FState[1] := StateInit2;
  FState[2] := StateInit3;

  {mix in the passphrase to the state (uses inlined update_keys)}
  for i := 1 to length(aPassphrase) do begin
    FState[0] := AbUpdateCrc32(byte(aPassphrase[i]), FState[0]);
    FState[1] := FState[1] + (FState[0] and $FF);
    FState[1] := (FState[1] * MagicNumber) + 1;
    FState[2] := AbUpdateCrc32(FState[1] shr 24, FState[2]);
  end;
end;
{====================================================================}


{===TAbDfEncryptStream===============================================}
constructor TAbDfEncryptStream.Create(aStream     : TStream;
                                      aCheckValue : longint;
                                const aPassphrase : AnsiString);
var
  Header : TAbZipEncryptHeader;
begin
  {create the ancestor}
  inherited Create;

  {save the stream parameter}
  FStream := aStream;

  {create the encryption engine}
  FEngine := TAbZipEncryptEngine.Create;

  {generate the encryption header, write it to the stream}
  FEngine.CreateHeader(Header, aPassphrase, aCheckValue);
  aStream.WriteBuffer(Header, sizeof(Header));
end;
{--------}
destructor TAbDfEncryptStream.Destroy;
begin
  {free the internal buffer if used}
  if (FBuffer <> nil) then
    FreeMem(FBuffer);

  {free the engine}
  FEngine.Free;

  {destroy the ancestor}
  inherited Destroy;
end;
{--------}
function TAbDfEncryptStream.Read(var aBuffer; aCount : longint) : longint;
begin
  {check for programming error}
  Assert(false,
         'TAbDfEncryptStream.Read: the stream is write-only');
  Result := 0;
end;
{--------}
function TAbDfEncryptStream.Seek(aOffset : longint; aOrigin : word) : longint;
begin
  Result := FStream.Seek(aOffset, aOrigin);
end;
{--------}
function TAbDfEncryptStream.Write(const aBuffer; aCount : longint) : longint;
begin
  {note: since we cannot alter a const parameter, we should copy the
         data to our own buffer, encrypt it and then write it}

  {check that our buffer is large enough}
  if (FBufSize < aCount) then begin
    if (FBuffer <> nil) then
      FreeMem(FBuffer);
    GetMem(FBuffer, aCount);
    FBufSize := aCount;
  end;

  {copy the data to our buffer}
  Move(aBuffer, FBuffer^, aCount);

  {encrypt the data in our buffer}
  FEngine.EncodeBuffer(FBuffer^, aCount);

  {write the data in our buffer to the underlying stream}
  Result := FStream.Write(FBuffer^, aCount);
end;
{====================================================================}


end.



