{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of MD4 **********************************}
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
unit DCPmd4;

{$MODE Delphi}

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPconst;

type
  TDCP_md4= class(TDCP_hash)
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

function LRot32(a, b: longword): longword;
begin
  Result:= (a shl b) or (a shr (32-b));
end;

procedure TDCP_md4.Compress;
var
  Data: array[0..15] of dword;
  A, B, C, D: dword;
begin
  dcpFillChar(Data, SizeOf(Data), 0);
  Move(HashBuffer,Data,Sizeof(Data));
  A:= CurrentHash[0];
  B:= CurrentHash[1];
  C:= CurrentHash[2];
  D:= CurrentHash[3];

  A:= LRot32(A + (D xor (B and (C xor D))) + Data[ 0],3);
  D:= LRot32(D + (C xor (A and (B xor C))) + Data[ 1],7);
  C:= LRot32(C + (B xor (D and (A xor B))) + Data[ 2],11);
  B:= LRot32(B + (A xor (C and (D xor A))) + Data[ 3],19);
  A:= LRot32(A + (D xor (B and (C xor D))) + Data[ 4],3);
  D:= LRot32(D + (C xor (A and (B xor C))) + Data[ 5],7);
  C:= LRot32(C + (B xor (D and (A xor B))) + Data[ 6],11);
  B:= LRot32(B + (A xor (C and (D xor A))) + Data[ 7],19);
  A:= LRot32(A + (D xor (B and (C xor D))) + Data[ 8],3);
  D:= LRot32(D + (C xor (A and (B xor C))) + Data[ 9],7);
  C:= LRot32(C + (B xor (D and (A xor B))) + Data[10],11);
  B:= LRot32(B + (A xor (C and (D xor A))) + Data[11],19);
  A:= LRot32(A + (D xor (B and (C xor D))) + Data[12],3);
  D:= LRot32(D + (C xor (A and (B xor C))) + Data[13],7);
  C:= LRot32(C + (B xor (D and (A xor B))) + Data[14],11);
  B:= LRot32(B + (A xor (C and (D xor A))) + Data[15],19);

  A:= LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[ 0] + $5a827999,3);
  D:= LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[ 4] + $5a827999,5);
  C:= LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[ 8] + $5a827999,9);
  B:= LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[12] + $5a827999,13);
  A:= LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[ 1] + $5a827999,3);
  D:= LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[ 5] + $5a827999,5);
  C:= LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[ 9] + $5a827999,9);
  B:= LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[13] + $5a827999,13);
  A:= LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[ 2] + $5a827999,3);
  D:= LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[ 6] + $5a827999,5);
  C:= LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[10] + $5a827999,9);
  B:= LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[14] + $5a827999,13);
  A:= LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[ 3] + $5a827999,3);
  D:= LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[ 7] + $5a827999,5);
  C:= LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[11] + $5a827999,9);
  B:= LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[15] + $5a827999,13);

  A:= LRot32(A + (B xor C xor D) + Data[ 0] + $6ed9eba1,3);
  D:= LRot32(D + (A xor B xor C) + Data[ 8] + $6ed9eba1,9);
  C:= LRot32(C + (D xor A xor B) + Data[ 4] + $6ed9eba1,11);
  B:= LRot32(B + (C xor D xor A) + Data[12] + $6ed9eba1,15);
  A:= LRot32(A + (B xor C xor D) + Data[ 2] + $6ed9eba1,3);
  D:= LRot32(D + (A xor B xor C) + Data[10] + $6ed9eba1,9);
  C:= LRot32(C + (D xor A xor B) + Data[ 6] + $6ed9eba1,11);
  B:= LRot32(B + (C xor D xor A) + Data[14] + $6ed9eba1,15);
  A:= LRot32(A + (B xor C xor D) + Data[ 1] + $6ed9eba1,3);
  D:= LRot32(D + (A xor B xor C) + Data[ 9] + $6ed9eba1,9);
  C:= LRot32(C + (D xor A xor B) + Data[ 5] + $6ed9eba1,11);
  B:= LRot32(B + (C xor D xor A) + Data[13] + $6ed9eba1,15);
  A:= LRot32(A + (B xor C xor D) + Data[ 3] + $6ed9eba1,3);
  D:= LRot32(D + (A xor B xor C) + Data[11] + $6ed9eba1,9);
  C:= LRot32(C + (D xor A xor B) + Data[ 7] + $6ed9eba1,11);
  B:= LRot32(B + (C xor D xor A) + Data[15] + $6ed9eba1,15);

  Inc(CurrentHash[0],A);
  Inc(CurrentHash[1],B);
  Inc(CurrentHash[2],C);
  Inc(CurrentHash[3],D);
  Index:= 0;
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
end;

class function TDCP_md4.GetHashSize: integer;
begin
  Result:= 128;
end;

class function TDCP_md4.GetId: integer;
begin
  Result:= DCP_md4;
end;

class function TDCP_md4.GetAlgorithm: string;
begin
  Result:= 'MD4';
end;

class function TDCP_md4.SelfTest: boolean;
const
  Test1Out: array[0..15] of byte=
    ($a4,$48,$01,$7a,$af,$21,$d8,$52,$5f,$c1,$0a,$e8,$7a,$a6,$72,$9d);
  Test2Out: array[0..15] of byte=
    ($d7,$9e,$1c,$30,$8a,$a5,$bb,$cd,$ee,$a8,$ed,$63,$df,$41,$2d,$a9);
var
  TestHash: TDCP_md4;
  TestOut: array[0..19] of byte;
begin
  dcpFillChar(TestOut, SizeOf(TestOut), 0);
  TestHash:= TDCP_md4.Create(nil);
  TestHash.Init;
  TestHash.UpdateStr('abc');
  TestHash.Final(TestOut);
  Result:= CompareMem(@TestOut,@Test1Out,Sizeof(Test1Out));
  TestHash.Init;
  TestHash.UpdateStr('abcdefghijklmnopqrstuvwxyz');
  TestHash.Final(TestOut);
  Result:= CompareMem(@TestOut,@Test2Out,Sizeof(Test2Out)) and Result;
  TestHash.Free;
end;

procedure TDCP_md4.Init;
begin
  Burn;
  CurrentHash[0]:= $67452301;
  CurrentHash[1]:= $efcdab89;
  CurrentHash[2]:= $98badcfe;
  CurrentHash[3]:= $10325476;
  fInitialized:= true;
end;

procedure TDCP_md4.Burn;
begin
  LenHi:= 0; LenLo:= 0;
  Index:= 0;
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
  FillChar(CurrentHash,Sizeof(CurrentHash),0);
  fInitialized:= false;
end;

procedure TDCP_md4.Update(const Buffer; Size: longword);
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

procedure TDCP_md4.Final(var Digest);
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
