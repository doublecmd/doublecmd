{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of BLAKE2S, BLAKE2SP, BLAKE2B, BLAKE2BP *}
{******************************************************************************}
{* Copyright (C) 2014-2018 Alexander Koblov (alexx2000@mail.ru)               *}
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
unit DCPblake2;

{$mode delphi}

interface

uses
  Classes, SysUtils, CTypes, DCPcrypt2, DCPconst, DCblake2, Hash;

type

  { TDCP_blake2s }

  TDCP_blake2s = class(TDCP_hash)
  protected
    S: blake2s_state;
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

  { TDCP_blake2sp }

  TDCP_blake2sp = class(TDCP_hash)
  protected
    S: blake2sp_state;
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

  { TDCP_blake2b }

  TDCP_blake2b = class(TDCP_hash)
  protected
    S: blake2b_state;
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

  { TDCP_blake2bp }

  TDCP_blake2bp = class(TDCP_hash)
  protected
    S: blake2bp_state;
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

{ TDCP_blake2s }

class function TDCP_blake2s.GetId: integer;
begin
  Result:= DCP_blake2s;
end;

class function TDCP_blake2s.GetAlgorithm: string;
begin
  Result:= 'BLAKE2S';
end;

class function TDCP_blake2s.GetHashSize: integer;
begin
  Result:= 256;
end;

class function TDCP_blake2s.SelfTest: boolean;
const
  Test1Out: array[0..31] of byte=
    ($50, $8c, $5e, $8c, $32, $7c, $14, $e2, $e1, $a7, $2b, $a3, $4e, $eb, $45, $2f,
     $37, $45, $8b, $20, $9e, $d6, $3a, $29, $4d, $99, $9b, $4c, $86, $67, $59, $82);
  Test2Out: array[0..31] of byte=
    ($6f, $4d, $f5, $11, $6a, $6f, $33, $2e, $da, $b1, $d9, $e1, $0e, $e8, $7d, $f6,
     $55, $7b, $ea, $b6, $25, $9d, $76, $63, $f3, $bc, $d5, $72, $2c, $13, $f1, $89 );
var
  TestHash: TDCP_blake2s;
  TestOut: array[0..31] of byte;
begin
  dcpFillChar(TestOut, SizeOf(TestOut), 0);
  TestHash:= TDCP_blake2s.Create(nil);
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

procedure TDCP_blake2s.Init;
begin
  if blake2s_init( @S, BLAKE2S_OUTBYTES ) < 0 then
    raise EDCP_hash.Create('blake2s_init');
  fInitialized:= true;
end;

procedure TDCP_blake2s.Burn;
begin
  fInitialized:= false;
end;

procedure TDCP_blake2s.Update(const Buffer; Size: longword);
begin
  if blake2s_update(@S, @Buffer, Size) < 0 then
    raise EDCP_hash.Create('blake2s_update');
end;

procedure TDCP_blake2s.Final(var Digest);
var
  Hash: array[0..Pred(BLAKE2S_OUTBYTES)] of cuint8;
begin
  if not fInitialized then
    raise EDCP_hash.Create('Hash not initialized');
  if blake2s_final(@S, Hash, SizeOf(Hash)) < 0 then
    raise EDCP_hash.Create('blake2s_final');
  Move(Hash, Digest, Sizeof(Hash));
  Burn;
end;

{ TDCP_blake2sp }

class function TDCP_blake2sp.GetId: integer;
begin
  Result:= DCP_blake2sp;
end;

class function TDCP_blake2sp.GetAlgorithm: string;
begin
  Result:= 'BLAKE2SP';
end;

class function TDCP_blake2sp.GetHashSize: integer;
begin
  Result:= 256;
end;

class function TDCP_blake2sp.SelfTest: boolean;
const
  Test1Out: array[0..31] of byte=
    ($70, $f7, $5b, $58, $f1, $fe, $ca, $b8, $21, $db, $43, $c8, $8a, $d8, $4e, $dd,
     $e5, $a5, $26, $00, $61, $6c, $d2, $25, $17, $b7, $bb, $14, $d4, $40, $a7, $d5);
  Test2Out: array[0..31] of byte=
    ($3d, $10, $7e, $42, $f1, $7c, $13, $c8, $2b, $43, $6e, $bb, $65, $1a, $48, $de,
     $f6, $7e, $77, $72, $fa, $06, $f4, $73, $8e, $e9, $68, $c7, $f4, $d8, $b4, $8b);
var
  TestHash: TDCP_blake2sp;
  TestOut: array[0..31] of byte;
begin
  dcpFillChar(TestOut, SizeOf(TestOut), 0);
  TestHash:= TDCP_blake2sp.Create(nil);
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

procedure TDCP_blake2sp.Init;
begin
  if blake2sp_init( @S, BLAKE2S_OUTBYTES ) < 0 then
    raise EDCP_hash.Create('blake2sp_init');
  fInitialized:= true;
end;

procedure TDCP_blake2sp.Burn;
begin
  fInitialized:= false;
end;

procedure TDCP_blake2sp.Update(const Buffer; Size: longword);
begin
  if blake2sp_update(@S, @Buffer, Size) < 0 then
    raise EDCP_hash.Create('blake2sp_update');
end;

procedure TDCP_blake2sp.Final(var Digest);
var
  Hash: array[0..Pred(BLAKE2S_OUTBYTES)] of cuint8;
begin
  if not fInitialized then
    raise EDCP_hash.Create('Hash not initialized');
  if blake2sp_final(@S, Hash, SizeOf(Hash)) < 0 then
    raise EDCP_hash.Create('blake2sp_final');
  Move(Hash, Digest, Sizeof(Hash));
  Burn;
end;

{ TDCP_blake2b }

class function TDCP_blake2b.GetId: integer;
begin
  Result:= DCP_blake2b;
end;

class function TDCP_blake2b.GetAlgorithm: string;
begin
  Result:= 'BLAKE2B';
end;

class function TDCP_blake2b.GetHashSize: integer;
begin
  Result:= 512;
end;

class function TDCP_blake2b.SelfTest: boolean;
const
  Test1Out: array[0..63] of byte =
    ($ba, $80, $a5, $3f, $98, $1c, $4d, $0d, $6a, $27, $97, $b6, $9f, $12, $f6, $e9,
     $4c, $21, $2f, $14, $68, $5a, $c4, $b7, $4b, $12, $bb, $6f, $db, $ff, $a2, $d1,
     $7d, $87, $c5, $39, $2a, $ab, $79, $2d, $c2, $52, $d5, $de, $45, $33, $cc, $95,
     $18, $d3, $8a, $a8, $db, $f1, $92, $5a, $b9, $23, $86, $ed, $d4, $00, $99, $23);
  Test2Out: array[0..63] of byte =
    ($72, $85, $ff, $3e, $8b, $d7, $68, $d6, $9b, $e6, $2b, $3b, $f1, $87, $65, $a3,
     $25, $91, $7f, $a9, $74, $4a, $c2, $f5, $82, $a2, $08, $50, $bc, $2b, $11, $41,
     $ed, $1b, $3e, $45, $28, $59, $5a, $cc, $90, $77, $2b, $df, $2d, $37, $dc, $8a,
     $47, $13, $0b, $44, $f3, $3a, $02, $e8, $73, $0e, $5a, $d8, $e1, $66, $e8, $88);
var
  TestHash: TDCP_blake2b;
  TestOut: array[0..63] of byte;
begin
  dcpFillChar(TestOut, SizeOf(TestOut), 0);
  TestHash:= TDCP_blake2b.Create(nil);
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

procedure TDCP_blake2b.Init;
begin
  if blake2b_init( @S, BLAKE2B_OUTBYTES ) < 0 then
    raise EDCP_hash.Create('blake2b_init');
  fInitialized:= true;
end;

procedure TDCP_blake2b.Burn;
begin
  fInitialized:= false;
end;

procedure TDCP_blake2b.Update(const Buffer; Size: longword);
begin
  if blake2b_update(@S, @Buffer, Size) < 0 then
    raise EDCP_hash.Create('blake2b_update');
end;

procedure TDCP_blake2b.Final(var Digest);
var
  Hash: array[0..Pred(BLAKE2B_OUTBYTES)] of cuint8;
begin
  if not fInitialized then
    raise EDCP_hash.Create('Hash not initialized');
  if blake2b_final(@S, Hash, SizeOf(Hash)) < 0 then
    raise EDCP_hash.Create('blake2b_final');
  Move(Hash, Digest, Sizeof(Hash));
  Burn;
end;

{ TDCP_blake2bp }

class function TDCP_blake2bp.GetId: integer;
begin
  Result:= DCP_blake2bp;
end;

class function TDCP_blake2bp.GetAlgorithm: string;
begin
  Result:= 'BLAKE2BP';
end;

class function TDCP_blake2bp.GetHashSize: integer;
begin
  Result:= 512;
end;

class function TDCP_blake2bp.SelfTest: boolean;
const
  Test1Out: array[0..63] of byte =
    ($b9, $1a, $6b, $66, $ae, $87, $52, $6c, $40, $0b, $0a, $8b, $53, $77, $4d, $c6,
     $52, $84, $ad, $8f, $65, $75, $f8, $14, $8f, $f9, $3d, $ff, $94, $3a, $6e, $cd,
     $83, $62, $13, $0f, $22, $d6, $da, $e6, $33, $aa, $0f, $91, $df, $4a, $c8, $9a,
     $af, $f3, $1d, $0f, $1b, $92, $3c, $89, $8e, $82, $02, $5d, $ed, $bd, $ad, $6e);
  Test2Out: array[0..63] of byte =
    ($c5, $a0, $34, $1e, $eb, $b6, $15, $50, $3e, $22, $93, $30, $e0, $6a, $3d, $ce,
     $88, $05, $b4, $34, $ca, $75, $8e, $89, $9e, $72, $ac, $40, $ba, $c3, $6e, $63,
     $7b, $70, $09, $8a, $24, $ae, $5c, $3c, $4d, $39, $a1, $83, $a4, $3e, $b9, $74,
     $82, $3e, $3d, $db, $5b, $09, $e0, $7a, $d1, $e5, $26, $e9, $05, $f6, $5b, $c4);
var
  TestHash: TDCP_blake2bp;
  TestOut: array[0..63] of byte;
begin
  dcpFillChar(TestOut, SizeOf(TestOut), 0);
  TestHash:= TDCP_blake2bp.Create(nil);
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

procedure TDCP_blake2bp.Init;
begin
  if blake2bp_init( @S, BLAKE2B_OUTBYTES ) < 0 then
    raise EDCP_hash.Create('blake2bp_init');
  fInitialized:= true;
end;

procedure TDCP_blake2bp.Burn;
begin
  fInitialized:= false;
end;

procedure TDCP_blake2bp.Update(const Buffer; Size: longword);
begin
  if blake2bp_update(@S, @Buffer, Size) < 0 then
    raise EDCP_hash.Create('blake2bp_update');
end;

procedure TDCP_blake2bp.Final(var Digest);
var
  Hash: array[0..Pred(BLAKE2B_OUTBYTES)] of cuint8;
begin
  if not fInitialized then
    raise EDCP_hash.Create('Hash not initialized');
  if blake2bp_final(@S, Hash, SizeOf(Hash)) < 0 then
    raise EDCP_hash.Create('blake2bp_final');
  Move(Hash, Digest, Sizeof(Hash));
  Burn;
end;

end.

