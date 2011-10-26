{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of RipeMD-128 ***************************}
{******************************************************************************}
{* Copyright (c) 2002 David Barton                                            *}
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
unit DCPripemd128;

{$MODE Delphi}

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPconst;

type
  TDCP_ripemd128= class(TDCP_hash)
  protected
    LenHi, LenLo: longword;
    Index: DWord;
    CurrentHash: array[0..3] of DWord;
    HashBuffer: array[0..63] of byte;
    procedure Compress;
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



{******************************************************************************}
{******************************************************************************}
implementation
{$R-}{$Q-}

procedure TDCP_ripemd128.Compress;
var
  X: array[0..15] of DWord;
  a, aa, b, bb, c, cc, d, dd, t: dword;
begin
  dcpFillChar(X, SizeOf(X), 0);
  Move(HashBuffer,X,Sizeof(X));
  a:= CurrentHash[0]; aa:= a;
  b:= CurrentHash[1]; bb:= b;
  c:= CurrentHash[2]; cc:= c;
  d:= CurrentHash[3]; dd:= d;

  t:= a + (b xor c xor d) + X[ 0]; a:= (t shl 11) or (t shr (32-11));
  t:= d + (a xor b xor c) + X[ 1]; d:= (t shl 14) or (t shr (32-14));
  t:= c + (d xor a xor b) + X[ 2]; c:= (t shl 15) or (t shr (32-15));
  t:= b + (c xor d xor a) + X[ 3]; b:= (t shl 12) or (t shr (32-12));
  t:= a + (b xor c xor d) + X[ 4]; a:= (t shl 5) or (t shr (32-5));
  t:= d + (a xor b xor c) + X[ 5]; d:= (t shl 8) or (t shr (32-8));
  t:= c + (d xor a xor b) + X[ 6]; c:= (t shl 7) or (t shr (32-7));
  t:= b + (c xor d xor a) + X[ 7]; b:= (t shl 9) or (t shr (32-9));
  t:= a + (b xor c xor d) + X[ 8]; a:= (t shl 11) or (t shr (32-11));
  t:= d + (a xor b xor c) + X[ 9]; d:= (t shl 13) or (t shr (32-13));
  t:= c + (d xor a xor b) + X[10]; c:= (t shl 14) or (t shr (32-14));
  t:= b + (c xor d xor a) + X[11]; b:= (t shl 15) or (t shr (32-15));
  t:= a + (b xor c xor d) + X[12]; a:= (t shl 6) or (t shr (32-6));
  t:= d + (a xor b xor c) + X[13]; d:= (t shl 7) or (t shr (32-7));
  t:= c + (d xor a xor b) + X[14]; c:= (t shl 9) or (t shr (32-9));
  t:= b + (c xor d xor a) + X[15]; b:= (t shl 8) or (t shr (32-8));

  t:= a + ((b and c) or (not b and d)) + X[ 7]+$5A827999; a:= (t shl 7) or (t shr (32-7));
  t:= d + ((a and b) or (not a and c)) + X[ 4]+$5A827999; d:= (t shl 6) or (t shr (32-6));
  t:= c + ((d and a) or (not d and b)) + X[13]+$5A827999; c:= (t shl 8) or (t shr (32-8));
  t:= b + ((c and d) or (not c and a)) + X[ 1]+$5A827999; b:= (t shl 13) or (t shr (32-13));
  t:= a + ((b and c) or (not b and d)) + X[10]+$5A827999; a:= (t shl 11) or (t shr (32-11));
  t:= d + ((a and b) or (not a and c)) + X[ 6]+$5A827999; d:= (t shl 9) or (t shr (32-9));
  t:= c + ((d and a) or (not d and b)) + X[15]+$5A827999; c:= (t shl 7) or (t shr (32-7));
  t:= b + ((c and d) or (not c and a)) + X[ 3]+$5A827999; b:= (t shl 15) or (t shr (32-15));
  t:= a + ((b and c) or (not b and d)) + X[12]+$5A827999; a:= (t shl 7) or (t shr (32-7));
  t:= d + ((a and b) or (not a and c)) + X[ 0]+$5A827999; d:= (t shl 12) or (t shr (32-12));
  t:= c + ((d and a) or (not d and b)) + X[ 9]+$5A827999; c:= (t shl 15) or (t shr (32-15));
  t:= b + ((c and d) or (not c and a)) + X[ 5]+$5A827999; b:= (t shl 9) or (t shr (32-9));
  t:= a + ((b and c) or (not b and d)) + X[ 2]+$5A827999; a:= (t shl 11) or (t shr (32-11));
  t:= d + ((a and b) or (not a and c)) + X[14]+$5A827999; d:= (t shl 7) or (t shr (32-7));
  t:= c + ((d and a) or (not d and b)) + X[11]+$5A827999; c:= (t shl 13) or (t shr (32-13));
  t:= b + ((c and d) or (not c and a)) + X[ 8]+$5A827999; b:= (t shl 12) or (t shr (32-12));

  t:= a + ((b or not c) xor d) + X[ 3]+$6ED9EBA1; a:= (t shl 11) or (t shr (32-11));
  t:= d + ((a or not b) xor c) + X[10]+$6ED9EBA1; d:= (t shl 13) or (t shr (32-13));
  t:= c + ((d or not a) xor b) + X[14]+$6ED9EBA1; c:= (t shl 6) or (t shr (32-6));
  t:= b + ((c or not d) xor a) + X[ 4]+$6ED9EBA1; b:= (t shl 7) or (t shr (32-7));
  t:= a + ((b or not c) xor d) + X[ 9]+$6ED9EBA1; a:= (t shl 14) or (t shr (32-14));
  t:= d + ((a or not b) xor c) + X[15]+$6ED9EBA1; d:= (t shl 9) or (t shr (32-9));
  t:= c + ((d or not a) xor b) + X[ 8]+$6ED9EBA1; c:= (t shl 13) or (t shr (32-13));
  t:= b + ((c or not d) xor a) + X[ 1]+$6ED9EBA1; b:= (t shl 15) or (t shr (32-15));
  t:= a + ((b or not c) xor d) + X[ 2]+$6ED9EBA1; a:= (t shl 14) or (t shr (32-14));
  t:= d + ((a or not b) xor c) + X[ 7]+$6ED9EBA1; d:= (t shl 8) or (t shr (32-8));
  t:= c + ((d or not a) xor b) + X[ 0]+$6ED9EBA1; c:= (t shl 13) or (t shr (32-13));
  t:= b + ((c or not d) xor a) + X[ 6]+$6ED9EBA1; b:= (t shl 6) or (t shr (32-6));
  t:= a + ((b or not c) xor d) + X[13]+$6ED9EBA1; a:= (t shl 5) or (t shr (32-5));
  t:= d + ((a or not b) xor c) + X[11]+$6ED9EBA1; d:= (t shl 12) or (t shr (32-12));
  t:= c + ((d or not a) xor b) + X[ 5]+$6ED9EBA1; c:= (t shl 7) or (t shr (32-7));
  t:= b + ((c or not d) xor a) + X[12]+$6ED9EBA1; b:= (t shl 5) or (t shr (32-5));

  t:= a + ((b and d) or (c and not d)) + X[ 1]+$8F1BBCDC; a:= (t shl 11) or (t shr (32-11));
  t:= d + ((a and c) or (b and not c)) + X[ 9]+$8F1BBCDC; d:= (t shl 12) or (t shr (32-12));
  t:= c + ((d and b) or (a and not b)) + X[11]+$8F1BBCDC; c:= (t shl 14) or (t shr (32-14));
  t:= b + ((c and a) or (d and not a)) + X[10]+$8F1BBCDC; b:= (t shl 15) or (t shr (32-15));
  t:= a + ((b and d) or (c and not d)) + X[ 0]+$8F1BBCDC; a:= (t shl 14) or (t shr (32-14));
  t:= d + ((a and c) or (b and not c)) + X[ 8]+$8F1BBCDC; d:= (t shl 15) or (t shr (32-15));
  t:= c + ((d and b) or (a and not b)) + X[12]+$8F1BBCDC; c:= (t shl 9) or (t shr (32-9));
  t:= b + ((c and a) or (d and not a)) + X[ 4]+$8F1BBCDC; b:= (t shl 8) or (t shr (32-8));
  t:= a + ((b and d) or (c and not d)) + X[13]+$8F1BBCDC; a:= (t shl 9) or (t shr (32-9));
  t:= d + ((a and c) or (b and not c)) + X[ 3]+$8F1BBCDC; d:= (t shl 14) or (t shr (32-14));
  t:= c + ((d and b) or (a and not b)) + X[ 7]+$8F1BBCDC; c:= (t shl 5) or (t shr (32-5));
  t:= b + ((c and a) or (d and not a)) + X[15]+$8F1BBCDC; b:= (t shl 6) or (t shr (32-6));
  t:= a + ((b and d) or (c and not d)) + X[14]+$8F1BBCDC; a:= (t shl 8) or (t shr (32-8));
  t:= d + ((a and c) or (b and not c)) + X[ 5]+$8F1BBCDC; d:= (t shl 6) or (t shr (32-6));
  t:= c + ((d and b) or (a and not b)) + X[ 6]+$8F1BBCDC; c:= (t shl 5) or (t shr (32-5));
  t:= b + ((c and a) or (d and not a)) + X[ 2]+$8F1BBCDC; b:= (t shl 12) or (t shr (32-12));

  t:= aa + ((bb and dd) or (cc and not dd)) + X[ 5]+$50A28BE6; aa:= (t shl 8) or (t shr (32-8));
  t:= dd + ((aa and cc) or (bb and not cc)) + X[14]+$50A28BE6; dd:= (t shl 9) or (t shr (32-9));
  t:= cc + ((dd and bb) or (aa and not bb)) + X[ 7]+$50A28BE6; cc:= (t shl 9) or (t shr (32-9));
  t:= bb + ((cc and aa) or (dd and not aa)) + X[ 0]+$50A28BE6; bb:= (t shl 11) or (t shr (32-11));
  t:= aa + ((bb and dd) or (cc and not dd)) + X[ 9]+$50A28BE6; aa:= (t shl 13) or (t shr (32-13));
  t:= dd + ((aa and cc) or (bb and not cc)) + X[ 2]+$50A28BE6; dd:= (t shl 15) or (t shr (32-15));
  t:= cc + ((dd and bb) or (aa and not bb)) + X[11]+$50A28BE6; cc:= (t shl 15) or (t shr (32-15));
  t:= bb + ((cc and aa) or (dd and not aa)) + X[ 4]+$50A28BE6; bb:= (t shl 5) or (t shr (32-5));
  t:= aa + ((bb and dd) or (cc and not dd)) + X[13]+$50A28BE6; aa:= (t shl 7) or (t shr (32-7));
  t:= dd + ((aa and cc) or (bb and not cc)) + X[ 6]+$50A28BE6; dd:= (t shl 7) or (t shr (32-7));
  t:= cc + ((dd and bb) or (aa and not bb)) + X[15]+$50A28BE6; cc:= (t shl 8) or (t shr (32-8));
  t:= bb + ((cc and aa) or (dd and not aa)) + X[ 8]+$50A28BE6; bb:= (t shl 11) or (t shr (32-11));
  t:= aa + ((bb and dd) or (cc and not dd)) + X[ 1]+$50A28BE6; aa:= (t shl 14) or (t shr (32-14));
  t:= dd + ((aa and cc) or (bb and not cc)) + X[10]+$50A28BE6; dd:= (t shl 14) or (t shr (32-14));
  t:= cc + ((dd and bb) or (aa and not bb)) + X[ 3]+$50A28BE6; cc:= (t shl 12) or (t shr (32-12));
  t:= bb + ((cc and aa) or (dd and not aa)) + X[12]+$50A28BE6; bb:= (t shl 6) or (t shr (32-6));

  t:= aa + ((bb or not cc) xor dd) + X[ 6]+$5C4DD124; aa:= (t shl 9) or (t shr (32-9));
  t:= dd + ((aa or not bb) xor cc) + X[11]+$5C4DD124; dd:= (t shl 13) or (t shr (32-13));
  t:= cc + ((dd or not aa) xor bb) + X[ 3]+$5C4DD124; cc:= (t shl 15) or (t shr (32-15));
  t:= bb + ((cc or not dd) xor aa) + X[ 7]+$5C4DD124; bb:= (t shl 7) or (t shr (32-7));
  t:= aa + ((bb or not cc) xor dd) + X[ 0]+$5C4DD124; aa:= (t shl 12) or (t shr (32-12));
  t:= dd + ((aa or not bb) xor cc) + X[13]+$5C4DD124; dd:= (t shl 8) or (t shr (32-8));
  t:= cc + ((dd or not aa) xor bb) + X[ 5]+$5C4DD124; cc:= (t shl 9) or (t shr (32-9));
  t:= bb + ((cc or not dd) xor aa) + X[10]+$5C4DD124; bb:= (t shl 11) or (t shr (32-11));
  t:= aa + ((bb or not cc) xor dd) + X[14]+$5C4DD124; aa:= (t shl 7) or (t shr (32-7));
  t:= dd + ((aa or not bb) xor cc) + X[15]+$5C4DD124; dd:= (t shl 7) or (t shr (32-7));
  t:= cc + ((dd or not aa) xor bb) + X[ 8]+$5C4DD124; cc:= (t shl 12) or (t shr (32-12));
  t:= bb + ((cc or not dd) xor aa) + X[12]+$5C4DD124; bb:= (t shl 7) or (t shr (32-7));
  t:= aa + ((bb or not cc) xor dd) + X[ 4]+$5C4DD124; aa:= (t shl 6) or (t shr (32-6));
  t:= dd + ((aa or not bb) xor cc) + X[ 9]+$5C4DD124; dd:= (t shl 15) or (t shr (32-15));
  t:= cc + ((dd or not aa) xor bb) + X[ 1]+$5C4DD124; cc:= (t shl 13) or (t shr (32-13));
  t:= bb + ((cc or not dd) xor aa) + X[ 2]+$5C4DD124; bb:= (t shl 11) or (t shr (32-11));

  t:= aa + ((bb and cc) or (not bb and dd)) + X[15]+$6D703EF3; aa:= (t shl 9) or (t shr (32-9));
  t:= dd + ((aa and bb) or (not aa and cc)) + X[ 5]+$6D703EF3; dd:= (t shl 7) or (t shr (32-7));
  t:= cc + ((dd and aa) or (not dd and bb)) + X[ 1]+$6D703EF3; cc:= (t shl 15) or (t shr (32-15));
  t:= bb + ((cc and dd) or (not cc and aa)) + X[ 3]+$6D703EF3; bb:= (t shl 11) or (t shr (32-11));
  t:= aa + ((bb and cc) or (not bb and dd)) + X[ 7]+$6D703EF3; aa:= (t shl 8) or (t shr (32-8));
  t:= dd + ((aa and bb) or (not aa and cc)) + X[14]+$6D703EF3; dd:= (t shl 6) or (t shr (32-6));
  t:= cc + ((dd and aa) or (not dd and bb)) + X[ 6]+$6D703EF3; cc:= (t shl 6) or (t shr (32-6));
  t:= bb + ((cc and dd) or (not cc and aa)) + X[ 9]+$6D703EF3; bb:= (t shl 14) or (t shr (32-14));
  t:= aa + ((bb and cc) or (not bb and dd)) + X[11]+$6D703EF3; aa:= (t shl 12) or (t shr (32-12));
  t:= dd + ((aa and bb) or (not aa and cc)) + X[ 8]+$6D703EF3; dd:= (t shl 13) or (t shr (32-13));
  t:= cc + ((dd and aa) or (not dd and bb)) + X[12]+$6D703EF3; cc:= (t shl 5) or (t shr (32-5));
  t:= bb + ((cc and dd) or (not cc and aa)) + X[ 2]+$6D703EF3; bb:= (t shl 14) or (t shr (32-14));
  t:= aa + ((bb and cc) or (not bb and dd)) + X[10]+$6D703EF3; aa:= (t shl 13) or (t shr (32-13));
  t:= dd + ((aa and bb) or (not aa and cc)) + X[ 0]+$6D703EF3; dd:= (t shl 13) or (t shr (32-13));
  t:= cc + ((dd and aa) or (not dd and bb)) + X[ 4]+$6D703EF3; cc:= (t shl 7) or (t shr (32-7));
  t:= bb + ((cc and dd) or (not cc and aa)) + X[13]+$6D703EF3; bb:= (t shl 5) or (t shr (32-5));

  t:= aa + (bb xor cc xor dd) + X[ 8]; aa:= (t shl 15) or (t shr (32-15));
  t:= dd + (aa xor bb xor cc) + X[ 6]; dd:= (t shl 5) or (t shr (32-5));
  t:= cc + (dd xor aa xor bb) + X[ 4]; cc:= (t shl 8) or (t shr (32-8));
  t:= bb + (cc xor dd xor aa) + X[ 1]; bb:= (t shl 11) or (t shr (32-11));
  t:= aa + (bb xor cc xor dd) + X[ 3]; aa:= (t shl 14) or (t shr (32-14));
  t:= dd + (aa xor bb xor cc) + X[11]; dd:= (t shl 14) or (t shr (32-14));
  t:= cc + (dd xor aa xor bb) + X[15]; cc:= (t shl 6) or (t shr (32-6));
  t:= bb + (cc xor dd xor aa) + X[ 0]; bb:= (t shl 14) or (t shr (32-14));
  t:= aa + (bb xor cc xor dd) + X[ 5]; aa:= (t shl 6) or (t shr (32-6));
  t:= dd + (aa xor bb xor cc) + X[12]; dd:= (t shl 9) or (t shr (32-9));
  t:= cc + (dd xor aa xor bb) + X[ 2]; cc:= (t shl 12) or (t shr (32-12));
  t:= bb + (cc xor dd xor aa) + X[13]; bb:= (t shl 9) or (t shr (32-9));
  t:= aa + (bb xor cc xor dd) + X[ 9]; aa:= (t shl 12) or (t shr (32-12));
  t:= dd + (aa xor bb xor cc) + X[ 7]; dd:= (t shl 5) or (t shr (32-5));
  t:= cc + (dd xor aa xor bb) + X[10]; cc:= (t shl 15) or (t shr (32-15));
  t:= bb + (cc xor dd xor aa) + X[14]; bb:= (t shl 8) or (t shr (32-8));

  Inc(dd,c + CurrentHash[1]);
  CurrentHash[1]:= CurrentHash[2] + d + aa;
  CurrentHash[2]:= CurrentHash[3] + a + bb;
  CurrentHash[3]:= CurrentHash[0] + b + cc;
  CurrentHash[0]:= dd;

  FillChar(X,Sizeof(X),0);
  Index:= 0;
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
end;

class function TDCP_ripemd128.GetHashSize: integer;
begin
  Result:= 128;
end;

class function TDCP_ripemd128.GetId: integer;
begin
  Result:= DCP_ripemd128;
end;

class function TDCP_ripemd128.GetAlgorithm: string;
begin
  Result:= 'RipeMD-128';
end;

class function TDCP_ripemd128.SelfTest: boolean;
const
  Test1Out: array[0..15] of byte=
    ($86,$be,$7a,$fa,$33,$9d,$0f,$c7,$cf,$c7,$85,$e7,$2f,$57,$8d,$33);
  Test2Out: array[0..15] of byte=
    ($fd,$2a,$a6,$07,$f7,$1d,$c8,$f5,$10,$71,$49,$22,$b3,$71,$83,$4e);
var
  TestHash: TDCP_ripemd128;
  TestOut: array[0..15] of byte;
begin
  dcpFillChar(TestOut, SizeOf(TestOut), 0);
  TestHash:= TDCP_ripemd128.Create(nil);
  TestHash.Init;
  TestHash.UpdateStr('a');
  TestHash.Final(TestOut);
  Result:= CompareMem(@TestOut,@Test1Out,Sizeof(Test1Out));
  TestHash.Init;
  TestHash.UpdateStr('abcdefghijklmnopqrstuvwxyz');
  TestHash.Final(TestOut);
  Result:= CompareMem(@TestOut,@Test2Out,Sizeof(Test2Out)) and Result;
  TestHash.Free;
end;

procedure TDCP_ripemd128.Init;
begin
  Burn;
  CurrentHash[0]:= $67452301;
  CurrentHash[1]:= $efcdab89;
  CurrentHash[2]:= $98badcfe;
  CurrentHash[3]:= $10325476;
  fInitialized:= true;
end;

procedure TDCP_ripemd128.Burn;
begin
  LenHi:= 0; LenLo:= 0;
  Index:= 0;
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
  FillChar(CurrentHash,Sizeof(CurrentHash),0);
  fInitialized:= false;
end;

procedure TDCP_ripemd128.Update(const Buffer; Size: longword);
var
  PBuf: ^byte;
begin
  if not fInitialized then
    raise EDCP_hash.Create('Hash not initialized');

  Inc(LenHi,Size shr 29);
  Inc(LenLo,Size*8);
  if LenLo< (Size*8) then
    Inc(LenHi);

  PBuf:= @Buffer;
  while Size> 0 do
  begin
    if (Sizeof(HashBuffer)-Index)<= DWord(Size) then
    begin
      Move(PBuf^,HashBuffer[Index],Sizeof(HashBuffer)-Index);
      Dec(Size,Sizeof(HashBuffer)-Index);
      Inc(PBuf,Sizeof(HashBuffer)-Index);
      Compress;
    end
    else
    begin
      Move(PBuf^,HashBuffer[Index],Size);
      Inc(Index,Size);
      Size:= 0;
    end;
  end;
end;

procedure TDCP_ripemd128.Final(var Digest);
begin
  if not fInitialized then
    raise EDCP_hash.Create('Hash not initialized');
  HashBuffer[Index]:= $80;
  if Index>= 56 then
    Compress;
  PDWord(@HashBuffer[56])^:= LenLo;
  PDWord(@HashBuffer[60])^:= LenHi;
  Compress;
  Move(CurrentHash,Digest,Sizeof(CurrentHash));
  Burn;
end;


end.
