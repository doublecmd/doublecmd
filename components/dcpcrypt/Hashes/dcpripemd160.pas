{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of RipeMD-160 ***************************}
{******************************************************************************}
{* Copyright (c) 1999-2002 David Barton                                       *}
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
unit DCPripemd160;

{$MODE Delphi}

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPconst;

type
  TDCP_ripemd160= class(TDCP_hash)
  protected
    LenHi, LenLo: longword;
    Index: DWord;
    CurrentHash: array[0..4] of DWord;
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

procedure TDCP_ripemd160.Compress;
var
  aa, bb, cc, dd, ee, aaa, bbb, ccc, ddd, eee: DWord;
  X: array[0..15] of DWord;
begin
  dcpFillChar(X, SizeOf(X), 0);
  Move(HashBuffer,X,Sizeof(X));
  aa:= CurrentHash[0];
  aaa:= CurrentHash[0];
  bb:= CurrentHash[1];
  bbb:= CurrentHash[1];
  cc:= CurrentHash[2];
  ccc:= CurrentHash[2];
  dd:= CurrentHash[3];
  ddd:= CurrentHash[3];
  ee:= CurrentHash[4];
  eee:= CurrentHash[4];

  aa:= aa + (bb xor cc xor dd) + X[ 0];
  aa:= ((aa shl 11) or (aa shr (32-11))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));
  ee:= ee + (aa xor bb xor cc) + X[ 1];
  ee:= ((ee shl 14) or (ee shr (32-14))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));
  dd:= dd + (ee xor aa xor bb) + X[ 2];
  dd:= ((dd shl 15) or (dd shr (32-15))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));
  cc:= cc + (dd xor ee xor aa) + X[ 3];
  cc:= ((cc shl 12) or (cc shr (32-12))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));
  bb:= bb + (cc xor dd xor ee) + X[ 4];
  bb:= ((bb shl 5) or (bb shr (32-5))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));
  aa:= aa + (bb xor cc xor dd) + X[ 5];
  aa:= ((aa shl 8) or (aa shr (32-8))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));
  ee:= ee + (aa xor bb xor cc) + X[ 6];
  ee:= ((ee shl 7) or (ee shr (32-7))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));
  dd:= dd + (ee xor aa xor bb) + X[ 7];
  dd:= ((dd shl 9) or (dd shr (32-9))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));
  cc:= cc + (dd xor ee xor aa) + X[ 8];
  cc:= ((cc shl 11) or (cc shr (32-11))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));
  bb:= bb + (cc xor dd xor ee) + X[ 9];
  bb:= ((bb shl 13) or (bb shr (32-13))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));
  aa:= aa + (bb xor cc xor dd) + X[10];
  aa:= ((aa shl 14) or (aa shr (32-14))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));
  ee:= ee + (aa xor bb xor cc) + X[11];
  ee:= ((ee shl 15) or (ee shr (32-15))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));
  dd:= dd + (ee xor aa xor bb) + X[12];
  dd:= ((dd shl 6) or (dd shr (32-6))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));
  cc:= cc + (dd xor ee xor aa) + X[13];
  cc:= ((cc shl 7) or (cc shr (32-7))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));
  bb:= bb + (cc xor dd xor ee) + X[14];
  bb:= ((bb shl 9) or (bb shr (32-9))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));
  aa:= aa + (bb xor cc xor dd) + X[15];
  aa:= ((aa shl 8) or (aa shr (32-8))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));

  ee:= ee + ((aa and bb) or ((not aa) and cc)) + X[ 7] + $5a827999;
  ee:= ((ee shl 7) or (ee shr (32-7))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));
  dd:= dd + ((ee and aa) or ((not ee) and bb)) + X[ 4] + $5a827999;
  dd:= ((dd shl 6) or (dd shr (32-6))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));
  cc:= cc + ((dd and ee) or ((not dd) and aa)) + X[13] + $5a827999;
  cc:= ((cc shl 8) or (cc shr (32-8))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));
  bb:= bb + ((cc and dd) or ((not cc) and ee)) + X[ 1] + $5a827999;
  bb:= ((bb shl 13) or (bb shr (32-13))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));
  aa:= aa + ((bb and cc) or ((not bb) and dd)) + X[10] + $5a827999;
  aa:= ((aa shl 11) or (aa shr (32-11))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));
  ee:= ee + ((aa and bb) or ((not aa) and cc)) + X[ 6] + $5a827999;
  ee:= ((ee shl 9) or (ee shr (32-9))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));
  dd:= dd + ((ee and aa) or ((not ee) and bb)) + X[15] + $5a827999;
  dd:= ((dd shl 7) or (dd shr (32-7))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));
  cc:= cc + ((dd and ee) or ((not dd) and aa)) + X[ 3] + $5a827999;
  cc:= ((cc shl 15) or (cc shr (32-15))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));
  bb:= bb + ((cc and dd) or ((not cc) and ee)) + X[12] + $5a827999;
  bb:= ((bb shl 7) or (bb shr (32-7))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));
  aa:= aa + ((bb and cc) or ((not bb) and dd)) + X[ 0] + $5a827999;
  aa:= ((aa shl 12) or (aa shr (32-12))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));
  ee:= ee + ((aa and bb) or ((not aa) and cc)) + X[ 9] + $5a827999;
  ee:= ((ee shl 15) or (ee shr (32-15))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));
  dd:= dd + ((ee and aa) or ((not ee) and bb)) + X[ 5] + $5a827999;
  dd:= ((dd shl 9) or (dd shr (32-9))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));
  cc:= cc + ((dd and ee) or ((not dd) and aa)) + X[ 2] + $5a827999;
  cc:= ((cc shl 11) or (cc shr (32-11))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));
  bb:= bb + ((cc and dd) or ((not cc) and ee)) + X[14] + $5a827999;
  bb:= ((bb shl 7) or (bb shr (32-7))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));
  aa:= aa + ((bb and cc) or ((not bb) and dd)) + X[11] + $5a827999;
  aa:= ((aa shl 13) or (aa shr (32-13))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));
  ee:= ee + ((aa and bb) or ((not aa) and cc)) + X[ 8] + $5a827999;
  ee:= ((ee shl 12) or (ee shr (32-12))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));

  dd:= dd + ((ee or (not aa)) xor bb) + X[ 3] + $6ed9eba1;
  dd:= ((dd shl 11) or (dd shr (32-11))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));
  cc:= cc + ((dd or (not ee)) xor aa) + X[10] + $6ed9eba1;
  cc:= ((cc shl 13) or (cc shr (32-13))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));
  bb:= bb + ((cc or (not dd)) xor ee) + X[14] + $6ed9eba1;
  bb:= ((bb shl 6) or (bb shr (32-6))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));
  aa:= aa + ((bb or (not cc)) xor dd) + X[ 4] + $6ed9eba1;
  aa:= ((aa shl 7) or (aa shr (32-7))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));
  ee:= ee + ((aa or (not bb)) xor cc) + X[ 9] + $6ed9eba1;
  ee:= ((ee shl 14) or (ee shr (32-14))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));
  dd:= dd + ((ee or (not aa)) xor bb) + X[15] + $6ed9eba1;
  dd:= ((dd shl 9) or (dd shr (32-9))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));
  cc:= cc + ((dd or (not ee)) xor aa) + X[ 8] + $6ed9eba1;
  cc:= ((cc shl 13) or (cc shr (32-13))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));
  bb:= bb + ((cc or (not dd)) xor ee) + X[ 1] + $6ed9eba1;
  bb:= ((bb shl 15) or (bb shr (32-15))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));
  aa:= aa + ((bb or (not cc)) xor dd) + X[ 2] + $6ed9eba1;
  aa:= ((aa shl 14) or (aa shr (32-14))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));
  ee:= ee + ((aa or (not bb)) xor cc) + X[ 7] + $6ed9eba1;
  ee:= ((ee shl 8) or (ee shr (32-8))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));
  dd:= dd + ((ee or (not aa)) xor bb) + X[ 0] + $6ed9eba1;
  dd:= ((dd shl 13) or (dd shr (32-13))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));
  cc:= cc + ((dd or (not ee)) xor aa) + X[ 6] + $6ed9eba1;
  cc:= ((cc shl 6) or (cc shr (32-6))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));
  bb:= bb + ((cc or (not dd)) xor ee) + X[13] + $6ed9eba1;
  bb:= ((bb shl 5) or (bb shr (32-5))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));
  aa:= aa + ((bb or (not cc)) xor dd) + X[11] + $6ed9eba1;
  aa:= ((aa shl 12) or (aa shr (32-12))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));
  ee:= ee + ((aa or (not bb)) xor cc) + X[ 5] + $6ed9eba1;
  ee:= ((ee shl 7) or (ee shr (32-7))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));
  dd:= dd + ((ee or (not aa)) xor bb) + X[12] + $6ed9eba1;
  dd:= ((dd shl 5) or (dd shr (32-5))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));

  cc:= cc + ((dd and aa) or (ee and (not aa))) + X[ 1] + $8f1bbcdc;
  cc:= ((cc shl 11) or (cc shr (32-11))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));
  bb:= bb + ((cc and ee) or (dd and (not ee))) + X[ 9] + $8f1bbcdc;
  bb:= ((bb shl 12) or (bb shr (32-12))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));
  aa:= aa + ((bb and dd) or (cc and (not dd))) + X[11] + $8f1bbcdc;
  aa:= ((aa shl 14) or (aa shr (32-14))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));
  ee:= ee + ((aa and cc) or (bb and (not cc))) + X[10] + $8f1bbcdc;
  ee:= ((ee shl 15) or (ee shr (32-15))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));
  dd:= dd + ((ee and bb) or (aa and (not bb))) + X[ 0] + $8f1bbcdc;
  dd:= ((dd shl 14) or (dd shr (32-14))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));
  cc:= cc + ((dd and aa) or (ee and (not aa))) + X[ 8] + $8f1bbcdc;
  cc:= ((cc shl 15) or (cc shr (32-15))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));
  bb:= bb + ((cc and ee) or (dd and (not ee))) + X[12] + $8f1bbcdc;
  bb:= ((bb shl 9) or (bb shr (32-9))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));
  aa:= aa + ((bb and dd) or (cc and (not dd))) + X[ 4] + $8f1bbcdc;
  aa:= ((aa shl 8) or (aa shr (32-8))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));
  ee:= ee + ((aa and cc) or (bb and (not cc))) + X[13] + $8f1bbcdc;
  ee:= ((ee shl 9) or (ee shr (32-9))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));
  dd:= dd + ((ee and bb) or (aa and (not bb))) + X[ 3] + $8f1bbcdc;
  dd:= ((dd shl 14) or (dd shr (32-14))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));
  cc:= cc + ((dd and aa) or (ee and (not aa))) + X[ 7] + $8f1bbcdc;
  cc:= ((cc shl 5) or (cc shr (32-5))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));
  bb:= bb + ((cc and ee) or (dd and (not ee))) + X[15] + $8f1bbcdc;
  bb:= ((bb shl 6) or (bb shr (32-6))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));
  aa:= aa + ((bb and dd) or (cc and (not dd))) + X[14] + $8f1bbcdc;
  aa:= ((aa shl 8) or (aa shr (32-8))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));
  ee:= ee + ((aa and cc) or (bb and (not cc))) + X[ 5] + $8f1bbcdc;
  ee:= ((ee shl 6) or (ee shr (32-6))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));
  dd:= dd + ((ee and bb) or (aa and (not bb))) + X[ 6] + $8f1bbcdc;
  dd:= ((dd shl 5) or (dd shr (32-5))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));
  cc:= cc + ((dd and aa) or (ee and (not aa))) + X[ 2] + $8f1bbcdc;
  cc:= ((cc shl 12) or (cc shr (32-12))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));

  bb:= bb + (cc xor (dd or (not ee))) + X[ 4] + $a953fd4e;
  bb:= ((bb shl 9) or (bb shr (32-9))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));
  aa:= aa + (bb xor (cc or (not dd))) + X[ 0] + $a953fd4e;
  aa:= ((aa shl 15) or (aa shr (32-15))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));
  ee:= ee + (aa xor (bb or (not cc))) + X[ 5] + $a953fd4e;
  ee:= ((ee shl 5) or (ee shr (32-5))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));
  dd:= dd + (ee xor (aa or (not bb))) + X[ 9] + $a953fd4e;
  dd:= ((dd shl 11) or (dd shr (32-11))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));
  cc:= cc + (dd xor (ee or (not aa))) + X[ 7] + $a953fd4e;
  cc:= ((cc shl 6) or (cc shr (32-6))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));
  bb:= bb + (cc xor (dd or (not ee))) + X[12] + $a953fd4e;
  bb:= ((bb shl 8) or (bb shr (32-8))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));
  aa:= aa + (bb xor (cc or (not dd))) + X[ 2] + $a953fd4e;
  aa:= ((aa shl 13) or (aa shr (32-13))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));
  ee:= ee + (aa xor (bb or (not cc))) + X[10] + $a953fd4e;
  ee:= ((ee shl 12) or (ee shr (32-12))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));
  dd:= dd + (ee xor (aa or (not bb))) + X[14] + $a953fd4e;
  dd:= ((dd shl 5) or (dd shr (32-5))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));
  cc:= cc + (dd xor (ee or (not aa))) + X[ 1] + $a953fd4e;
  cc:= ((cc shl 12) or (cc shr (32-12))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));
  bb:= bb + (cc xor (dd or (not ee))) + X[ 3] + $a953fd4e;
  bb:= ((bb shl 13) or (bb shr (32-13))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));
  aa:= aa + (bb xor (cc or (not dd))) + X[ 8] + $a953fd4e;
  aa:= ((aa shl 14) or (aa shr (32-14))) + ee;
  cc:= ((cc shl 10) or (cc shr (32-10)));
  ee:= ee + (aa xor (bb or (not cc))) + X[11] + $a953fd4e;
  ee:= ((ee shl 11) or (ee shr (32-11))) + dd;
  bb:= ((bb shl 10) or (bb shr (32-10)));
  dd:= dd + (ee xor (aa or (not bb))) + X[ 6] + $a953fd4e;
  dd:= ((dd shl 8) or (dd shr (32-8))) + cc;
  aa:= ((aa shl 10) or (aa shr (32-10)));
  cc:= cc + (dd xor (ee or (not aa))) + X[15] + $a953fd4e;
  cc:= ((cc shl 5) or (cc shr (32-5))) + bb;
  ee:= ((ee shl 10) or (ee shr (32-10)));
  bb:= bb + (cc xor (dd or (not ee))) + X[13] + $a953fd4e;
  bb:= ((bb shl 6) or (bb shr (32-6))) + aa;
  dd:= ((dd shl 10) or (dd shr (32-10)));

  aaa:= aaa + (bbb xor (ccc or (not ddd))) + X[ 5] + $50a28be6;
  aaa:= ((aaa shl 8) or (aaa shr (32-8))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));
  eee:= eee + (aaa xor (bbb or (not ccc))) + X[14] + $50a28be6;
  eee:= ((eee shl 9) or (eee shr (32-9))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));
  ddd:= ddd + (eee xor (aaa or (not bbb))) + X[ 7] + $50a28be6;
  ddd:= ((ddd shl 9) or (ddd shr (32-9))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));
  ccc:= ccc + (ddd xor (eee or (not aaa))) + X[ 0] + $50a28be6;
  ccc:= ((ccc shl 11) or (ccc shr (32-11))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));
  bbb:= bbb + (ccc xor (ddd or (not eee))) + X[ 9] + $50a28be6;
  bbb:= ((bbb shl 13) or (bbb shr (32-13))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));
  aaa:= aaa + (bbb xor (ccc or (not ddd))) + X[ 2] + $50a28be6;
  aaa:= ((aaa shl 15) or (aaa shr (32-15))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));
  eee:= eee + (aaa xor (bbb or (not ccc))) + X[11] + $50a28be6;
  eee:= ((eee shl 15) or (eee shr (32-15))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));
  ddd:= ddd + (eee xor (aaa or (not bbb))) + X[ 4] + $50a28be6;
  ddd:= ((ddd shl 5) or (ddd shr (32-5))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));
  ccc:= ccc + (ddd xor (eee or (not aaa))) + X[13] + $50a28be6;
  ccc:= ((ccc shl 7) or (ccc shr (32-7))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));
  bbb:= bbb + (ccc xor (ddd or (not eee))) + X[ 6] + $50a28be6;
  bbb:= ((bbb shl 7) or (bbb shr (32-7))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));
  aaa:= aaa + (bbb xor (ccc or (not ddd))) + X[15] + $50a28be6;
  aaa:= ((aaa shl 8) or (aaa shr (32-8))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));
  eee:= eee + (aaa xor (bbb or (not ccc))) + X[ 8] + $50a28be6;
  eee:= ((eee shl 11) or (eee shr (32-11))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));
  ddd:= ddd + (eee xor (aaa or (not bbb))) + X[ 1] + $50a28be6;
  ddd:= ((ddd shl 14) or (ddd shr (32-14))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));
  ccc:= ccc + (ddd xor (eee or (not aaa))) + X[10] + $50a28be6;
  ccc:= ((ccc shl 14) or (ccc shr (32-14))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));
  bbb:= bbb + (ccc xor (ddd or (not eee))) + X[ 3] + $50a28be6;
  bbb:= ((bbb shl 12) or (bbb shr (32-12))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));
  aaa:= aaa + (bbb xor (ccc or (not ddd))) + X[12] + $50a28be6;
  aaa:= ((aaa shl 6) or (aaa shr (32-6))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));

  eee:= eee + ((aaa and ccc) or (bbb and (not ccc))) + X[ 6] + $5c4dd124;
  eee:= ((eee shl 9) or (eee shr (32-9))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));
  ddd:= ddd + ((eee and bbb) or (aaa and (not bbb))) + X[11] + $5c4dd124;
  ddd:= ((ddd shl 13) or (ddd shr (32-13))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));
  ccc:= ccc + ((ddd and aaa) or (eee and (not aaa))) + X[ 3] + $5c4dd124;
  ccc:= ((ccc shl 15) or (ccc shr (32-15))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));
  bbb:= bbb + ((ccc and eee) or (ddd and (not eee))) + X[ 7] + $5c4dd124;
  bbb:= ((bbb shl 7) or (bbb shr (32-7))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));
  aaa:= aaa + ((bbb and ddd) or (ccc and (not ddd))) + X[ 0] + $5c4dd124;
  aaa:= ((aaa shl 12) or (aaa shr (32-12))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));
  eee:= eee + ((aaa and ccc) or (bbb and (not ccc))) + X[13] + $5c4dd124;
  eee:= ((eee shl 8) or (eee shr (32-8))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));
  ddd:= ddd + ((eee and bbb) or (aaa and (not bbb))) + X[ 5] + $5c4dd124;
  ddd:= ((ddd shl 9) or (ddd shr (32-9))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));
  ccc:= ccc + ((ddd and aaa) or (eee and (not aaa))) + X[10] + $5c4dd124;
  ccc:= ((ccc shl 11) or (ccc shr (32-11))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));
  bbb:= bbb + ((ccc and eee) or (ddd and (not eee))) + X[14] + $5c4dd124;
  bbb:= ((bbb shl 7) or (bbb shr (32-7))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));
  aaa:= aaa + ((bbb and ddd) or (ccc and (not ddd))) + X[15] + $5c4dd124;
  aaa:= ((aaa shl 7) or (aaa shr (32-7))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));
  eee:= eee + ((aaa and ccc) or (bbb and (not ccc))) + X[ 8] + $5c4dd124;
  eee:= ((eee shl 12) or (eee shr (32-12))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));
  ddd:= ddd + ((eee and bbb) or (aaa and (not bbb))) + X[12] + $5c4dd124;
  ddd:= ((ddd shl 7) or (ddd shr (32-7))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));
  ccc:= ccc + ((ddd and aaa) or (eee and (not aaa))) + X[ 4] + $5c4dd124;
  ccc:= ((ccc shl 6) or (ccc shr (32-6))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));
  bbb:= bbb + ((ccc and eee) or (ddd and (not eee))) + X[ 9] + $5c4dd124;
  bbb:= ((bbb shl 15) or (bbb shr (32-15))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));
  aaa:= aaa + ((bbb and ddd) or (ccc and (not ddd))) + X[ 1] + $5c4dd124;
  aaa:= ((aaa shl 13) or (aaa shr (32-13))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));
  eee:= eee + ((aaa and ccc) or (bbb and (not ccc))) + X[ 2] + $5c4dd124;
  eee:= ((eee shl 11) or (eee shr (32-11))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));

  ddd:= ddd + ((eee or (not aaa)) xor bbb) + X[15] + $6d703ef3;
  ddd:= ((ddd shl 9) or (ddd shr (32-9))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));
  ccc:= ccc + ((ddd or (not eee)) xor aaa) + X[ 5] + $6d703ef3;
  ccc:= ((ccc shl 7) or (ccc shr (32-7))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));
  bbb:= bbb + ((ccc or (not ddd)) xor eee) + X[ 1] + $6d703ef3;
  bbb:= ((bbb shl 15) or (bbb shr (32-15))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));
  aaa:= aaa + ((bbb or (not ccc)) xor ddd) + X[ 3] + $6d703ef3;
  aaa:= ((aaa shl 11) or (aaa shr (32-11))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));
  eee:= eee + ((aaa or (not bbb)) xor ccc) + X[ 7] + $6d703ef3;
  eee:= ((eee shl 8) or (eee shr (32-8))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));
  ddd:= ddd + ((eee or (not aaa)) xor bbb) + X[14] + $6d703ef3;
  ddd:= ((ddd shl 6) or (ddd shr (32-6))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));
  ccc:= ccc + ((ddd or (not eee)) xor aaa) + X[ 6] + $6d703ef3;
  ccc:= ((ccc shl 6) or (ccc shr (32-6))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));
  bbb:= bbb + ((ccc or (not ddd)) xor eee) + X[ 9] + $6d703ef3;
  bbb:= ((bbb shl 14) or (bbb shr (32-14))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));
  aaa:= aaa + ((bbb or (not ccc)) xor ddd) + X[11] + $6d703ef3;
  aaa:= ((aaa shl 12) or (aaa shr (32-12))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));
  eee:= eee + ((aaa or (not bbb)) xor ccc) + X[ 8] + $6d703ef3;
  eee:= ((eee shl 13) or (eee shr (32-13))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));
  ddd:= ddd + ((eee or (not aaa)) xor bbb) + X[12] + $6d703ef3;
  ddd:= ((ddd shl 5) or (ddd shr (32-5))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));
  ccc:= ccc + ((ddd or (not eee)) xor aaa) + X[ 2] + $6d703ef3;
  ccc:= ((ccc shl 14) or (ccc shr (32-14))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));
  bbb:= bbb + ((ccc or (not ddd)) xor eee) + X[10] + $6d703ef3;
  bbb:= ((bbb shl 13) or (bbb shr (32-13))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));
  aaa:= aaa + ((bbb or (not ccc)) xor ddd) + X[ 0] + $6d703ef3;
  aaa:= ((aaa shl 13) or (aaa shr (32-13))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));
  eee:= eee + ((aaa or (not bbb)) xor ccc) + X[ 4] + $6d703ef3;
  eee:= ((eee shl 7) or (eee shr (32-7))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));
  ddd:= ddd + ((eee or (not aaa)) xor bbb) + X[13] + $6d703ef3;
  ddd:= ((ddd shl 5) or (ddd shr (32-5))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));

  ccc:= ccc + ((ddd and eee) or ((not ddd) and aaa)) + X[ 8] + $7a6d76e9;
  ccc:= ((ccc shl 15) or (ccc shr (32-15))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));
  bbb:= bbb + ((ccc and ddd) or ((not ccc) and eee)) + X[ 6] + $7a6d76e9;
  bbb:= ((bbb shl 5) or (bbb shr (32-5))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));
  aaa:= aaa + ((bbb and ccc) or ((not bbb) and ddd)) + X[ 4] + $7a6d76e9;
  aaa:= ((aaa shl 8) or (aaa shr (32-8))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));
  eee:= eee + ((aaa and bbb) or ((not aaa) and ccc)) + X[ 1] + $7a6d76e9;
  eee:= ((eee shl 11) or (eee shr (32-11))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));
  ddd:= ddd + ((eee and aaa) or ((not eee) and bbb)) + X[ 3] + $7a6d76e9;
  ddd:= ((ddd shl 14) or (ddd shr (32-14))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));
  ccc:= ccc + ((ddd and eee) or ((not ddd) and aaa)) + X[11] + $7a6d76e9;
  ccc:= ((ccc shl 14) or (ccc shr (32-14))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));
  bbb:= bbb + ((ccc and ddd) or ((not ccc) and eee)) + X[15] + $7a6d76e9;
  bbb:= ((bbb shl 6) or (bbb shr (32-6))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));
  aaa:= aaa + ((bbb and ccc) or ((not bbb) and ddd)) + X[ 0] + $7a6d76e9;
  aaa:= ((aaa shl 14) or (aaa shr (32-14))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));
  eee:= eee + ((aaa and bbb) or ((not aaa) and ccc)) + X[ 5] + $7a6d76e9;
  eee:= ((eee shl 6) or (eee shr (32-6))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));
  ddd:= ddd + ((eee and aaa) or ((not eee) and bbb)) + X[12] + $7a6d76e9;
  ddd:= ((ddd shl 9) or (ddd shr (32-9))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));
  ccc:= ccc + ((ddd and eee) or ((not ddd) and aaa)) + X[ 2] + $7a6d76e9;
  ccc:= ((ccc shl 12) or (ccc shr (32-12))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));
  bbb:= bbb + ((ccc and ddd) or ((not ccc) and eee)) + X[13] + $7a6d76e9;
  bbb:= ((bbb shl 9) or (bbb shr (32-9))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));
  aaa:= aaa + ((bbb and ccc) or ((not bbb) and ddd)) + X[ 9] + $7a6d76e9;
  aaa:= ((aaa shl 12) or (aaa shr (32-12))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));
  eee:= eee + ((aaa and bbb) or ((not aaa) and ccc)) + X[ 7] + $7a6d76e9;
  eee:= ((eee shl 5) or (eee shr (32-5))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));
  ddd:= ddd + ((eee and aaa) or ((not eee) and bbb)) + X[10] + $7a6d76e9;
  ddd:= ((ddd shl 15) or (ddd shr (32-15))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));
  ccc:= ccc + ((ddd and eee) or ((not ddd) and aaa)) + X[14] + $7a6d76e9;
  ccc:= ((ccc shl 8) or (ccc shr (32-8))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));

  bbb:= bbb + (ccc xor ddd xor eee) + X[12];
  bbb:= ((bbb shl 8) or (bbb shr (32-8))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));
  aaa:= aaa + (bbb xor ccc xor ddd) + X[15];
  aaa:= ((aaa shl 5) or (aaa shr (32-5))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));
  eee:= eee + (aaa xor bbb xor ccc) + X[10];
  eee:= ((eee shl 12) or (eee shr (32-12))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));
  ddd:= ddd + (eee xor aaa xor bbb) + X[ 4];
  ddd:= ((ddd shl 9) or (ddd shr (32-9))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));
  ccc:= ccc + (ddd xor eee xor aaa) + X[ 1];
  ccc:= ((ccc shl 12) or (ccc shr (32-12))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));
  bbb:= bbb + (ccc xor ddd xor eee) + X[ 5];
  bbb:= ((bbb shl 5) or (bbb shr (32-5))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));
  aaa:= aaa + (bbb xor ccc xor ddd) + X[ 8];
  aaa:= ((aaa shl 14) or (aaa shr (32-14))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));
  eee:= eee + (aaa xor bbb xor ccc) + X[ 7];
  eee:= ((eee shl 6) or (eee shr (32-6))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));
  ddd:= ddd + (eee xor aaa xor bbb) + X[ 6];
  ddd:= ((ddd shl 8) or (ddd shr (32-8))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));
  ccc:= ccc + (ddd xor eee xor aaa) + X[ 2];
  ccc:= ((ccc shl 13) or (ccc shr (32-13))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));
  bbb:= bbb + (ccc xor ddd xor eee) + X[13];
  bbb:= ((bbb shl 6) or (bbb shr (32-6))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));
  aaa:= aaa + (bbb xor ccc xor ddd) + X[14];
  aaa:= ((aaa shl 5) or (aaa shr (32-5))) + eee;
  ccc:= ((ccc shl 10) or (ccc shr (32-10)));
  eee:= eee + (aaa xor bbb xor ccc) + X[ 0];
  eee:= ((eee shl 15) or (eee shr (32-15))) + ddd;
  bbb:= ((bbb shl 10) or (bbb shr (32-10)));
  ddd:= ddd + (eee xor aaa xor bbb) + X[ 3];
  ddd:= ((ddd shl 13) or (ddd shr (32-13))) + ccc;
  aaa:= ((aaa shl 10) or (aaa shr (32-10)));
  ccc:= ccc + (ddd xor eee xor aaa) + X[ 9];
  ccc:= ((ccc shl 11) or (ccc shr (32-11))) + bbb;
  eee:= ((eee shl 10) or (eee shr (32-10)));
  bbb:= bbb + (ccc xor ddd xor eee) + X[11];
  bbb:= ((bbb shl 11) or (bbb shr (32-11))) + aaa;
  ddd:= ((ddd shl 10) or (ddd shr (32-10)));

  ddd:= ddd + cc + CurrentHash[1];
  CurrentHash[1]:= CurrentHash[2] + dd + eee;
  CurrentHash[2]:= CurrentHash[3] + ee + aaa;
  CurrentHash[3]:= CurrentHash[4] + aa + bbb;
  CurrentHash[4]:= CurrentHash[0] + bb + ccc;
  CurrentHash[0]:= ddd;
  FillChar(X,Sizeof(X),0);
  Index:= 0;
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
end;

class function TDCP_ripemd160.GetHashSize: integer;
begin
  Result:= 160;
end;

class function TDCP_ripemd160.GetId: integer;
begin
  Result:= DCP_ripemd160;
end;

class function TDCP_ripemd160.GetAlgorithm: string;
begin
  Result:= 'RipeMD-160';
end;

class function TDCP_ripemd160.SelfTest: boolean;
const
  Test1Out: array[0..19] of byte=
    ($0B,$DC,$9D,$2D,$25,$6B,$3E,$E9,$DA,$AE,$34,$7B,$E6,$F4,$DC,$83,$5A,$46,$7F,$FE);
  Test2Out: array[0..19] of byte=
    ($F7,$1C,$27,$10,$9C,$69,$2C,$1B,$56,$BB,$DC,$EB,$5B,$9D,$28,$65,$B3,$70,$8D,$BC);
var
  TestHash: TDCP_ripemd160;
  TestOut: array[0..19] of byte;
begin
  dcpFillChar(TestOut, SizeOf(TestOut), 0);
  TestHash:= TDCP_ripemd160.Create(nil);
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

procedure TDCP_ripemd160.Init;
begin
  Burn;
  CurrentHash[0]:= $67452301;
  CurrentHash[1]:= $efcdab89;
  CurrentHash[2]:= $98badcfe;
  CurrentHash[3]:= $10325476;
  CurrentHash[4]:= $c3d2e1f0;
  fInitialized:= true;
end;

procedure TDCP_ripemd160.Burn;
begin
  LenHi:= 0; LenLo:= 0;
  Index:= 0;
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
  FillChar(CurrentHash,Sizeof(CurrentHash),0);
  fInitialized:= false;
end;

procedure TDCP_ripemd160.Update(const Buffer; Size: longword);
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

procedure TDCP_ripemd160.Final(var Digest);
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
