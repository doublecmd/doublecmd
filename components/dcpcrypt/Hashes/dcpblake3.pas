{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of BLAKE3                               *}
{******************************************************************************}
{* Copyright (C) 2020 Alexander Koblov (alexx2000@mail.ru)                    *}
{* Permission is hereby granted, free of charge, to any person obtaining a    *}
{* copy of this software and associated documentation files (the "Software"), *}
{* to deal in the Software without restriction, including without limitation  *}
{* the rights to use, copy, modify, merge, publish, distribute, sublicense,   *}
{* and/or sell copies of the Software, and to permit persons to whom the      *}
{* Software is furnished to do so, subject to the following conditions:       *}
{*                                                                            *}
{* The above copyright notice and this permission notice shall be included in *}
{* all copies or substantial portions of the Software.                        *}
{*                                                                            *}
{* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *}
{* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *}
{* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *}
{* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *}
{* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *}
{* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *}
{* DEALINGS IN THE SOFTWARE.                                                  *}
{******************************************************************************}
unit DCPblake3;

{$mode delphi}

interface

uses
  Classes, SysUtils, CTypes, DCPcrypt2, DCPconst, DCblake3, Hash;

type

  { TDCP_blake3 }

  TDCP_blake3 = class(TDCP_hash)
  protected
    S: blake3_hasher;
  public
    class function GetId: integer; override;
    class function GetAlgorithm: string; override;
    class function GetHashSize: integer; override;
    class function SelfTest: boolean; override;
    procedure Init; override;
    procedure Burn; override;
    procedure Update(const Buffer; Size: longword); override;
    procedure Final(var Digest); override;
  end;

implementation

{ TDCP_blake3 }

class function TDCP_blake3.GetId: integer;
begin
  Result:= DCP_blake3;
end;

class function TDCP_blake3.GetAlgorithm: string;
begin
  Result:= 'BLAKE3';
end;

class function TDCP_blake3.GetHashSize: integer;
begin
  Result:= 256;
end;

class function TDCP_blake3.SelfTest: boolean;
const
  Test1Out: array[0..31] of byte=
    ($64, $37, $b3, $ac, $38, $46, $51, $33, $ff, $b6, $3b, $75, $27, $3a, $8d, $b5,
     $48, $c5, $58, $46, $5d, $79, $db, $03, $fd, $35, $9c, $6c, $d5, $bd, $9d, $85);
  Test2Out: array[0..31] of byte=
    ($c1, $90, $12, $cc, $2a, $af, $0d, $c3, $d8, $e5, $c4, $5a, $1b, $79, $11, $4d,
     $2d, $f4, $2a, $bb, $2a, $41, $0b, $f5, $4b, $e0, $9e, $89, $1a, $f0, $6f, $f8 );
var
  TestHash: TDCP_blake3;
  TestOut: array[0..31] of byte;
begin
  dcpFillChar(TestOut, SizeOf(TestOut), 0);
  TestHash:= TDCP_blake3.Create(nil);
  TestHash.Init;
  TestHash.UpdateStr('abc');
  TestHash.Final(TestOut);
  Result:= boolean(CompareMem(@TestOut,@Test1Out,Sizeof(Test1Out)));
  TestHash.Init;
  TestHash.UpdateStr('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq');
  TestHash.Final(TestOut);
  Result:= boolean(CompareMem(@TestOut,@Test2Out,Sizeof(Test2Out))) and Result;
  TestHash.Free;
end;

procedure TDCP_blake3.Init;
begin
  blake3_hasher_init(@S);
  fInitialized:= true;
end;

procedure TDCP_blake3.Burn;
begin
  fInitialized:= false;
end;

procedure TDCP_blake3.Update(const Buffer; Size: longword);
begin
  blake3_hasher_update(@S, @Buffer, Size);
end;

procedure TDCP_blake3.Final(var Digest);
var
  Hash: array[0..Pred(BLAKE3_OUT_LEN)] of cuint8;
begin
  if not fInitialized then
    raise EDCP_hash.Create('Hash not initialized');
  blake3_hasher_finalize(@S, Hash, SizeOf(Hash));
  Move(Hash, Digest, Sizeof(Hash));
  Burn;
end;

end.

