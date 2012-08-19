{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of Tiger ********************************}
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
unit DCPtiger;

{$MODE Delphi}

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPconst;

type
  TDCP_tiger= class(TDCP_hash)
  protected
    Len: int64;
    Index: DWord;
    CurrentHash: array[0..2] of int64;
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

{$INCLUDE DCPtiger.inc}

procedure TDCP_tiger.Compress;
var
  a, b, c, aa, bb, cc: int64;
  x: array[0..7] of int64;
begin
  dcpFillChar(x, SizeOf(x), 0);
  a:= CurrentHash[0]; aa:= a;
  b:= CurrentHash[1]; bb:= b;
  c:= CurrentHash[2]; cc:= c;

  Move(HashBuffer,x,Sizeof(x));

  c:= c xor x[0];
  a:= a - (t1[c and $FF] xor t2[(c shr 16) and $FF] xor t3[(c shr 32) and $FF] xor t4[(c shr 48) and $FF]);
  b:= b + (t4[(c shr 8) and $FF] xor t3[(c shr 24) and $FF] xor t2[(c shr 40) and $FF] xor t1[(c shr 56) and $FF]);
  b:= b * 5;
  a:= a xor x[1];
  b:= b - (t1[a and $FF] xor t2[(a shr 16) and $FF] xor t3[(a shr 32) and $FF] xor t4[(a shr 48) and $FF]);
  c:= c + (t4[(a shr 8) and $FF] xor t3[(a shr 24) and $FF] xor t2[(a shr 40) and $FF] xor t1[(a shr 56) and $FF]);
  c:= c * 5;
  b:= b xor x[2];
  c:= c - (t1[b and $FF] xor t2[(b shr 16) and $FF] xor t3[(b shr 32) and $FF] xor t4[(b shr 48) and $FF]);
  a:= a + (t4[(b shr 8) and $FF] xor t3[(b shr 24) and $FF] xor t2[(b shr 40) and $FF] xor t1[(b shr 56) and $FF]);
  a:= a * 5;
  c:= c xor x[3];
  a:= a - (t1[c and $FF] xor t2[(c shr 16) and $FF] xor t3[(c shr 32) and $FF] xor t4[(c shr 48) and $FF]);
  b:= b + (t4[(c shr 8) and $FF] xor t3[(c shr 24) and $FF] xor t2[(c shr 40) and $FF] xor t1[(c shr 56) and $FF]);
  b:= b * 5;
  a:= a xor x[4];
  b:= b - (t1[a and $FF] xor t2[(a shr 16) and $FF] xor t3[(a shr 32) and $FF] xor t4[(a shr 48) and $FF]);
  c:= c + (t4[(a shr 8) and $FF] xor t3[(a shr 24) and $FF] xor t2[(a shr 40) and $FF] xor t1[(a shr 56) and $FF]);
  c:= c * 5;
  b:= b xor x[5];
  c:= c - (t1[b and $FF] xor t2[(b shr 16) and $FF] xor t3[(b shr 32) and $FF] xor t4[(b shr 48) and $FF]);
  a:= a + (t4[(b shr 8) and $FF] xor t3[(b shr 24) and $FF] xor t2[(b shr 40) and $FF] xor t1[(b shr 56) and $FF]);
  a:= a * 5;
  c:= c xor x[6];
  a:= a - (t1[c and $FF] xor t2[(c shr 16) and $FF] xor t3[(c shr 32) and $FF] xor t4[(c shr 48) and $FF]);
  b:= b + (t4[(c shr 8) and $FF] xor t3[(c shr 24) and $FF] xor t2[(c shr 40) and $FF] xor t1[(c shr 56) and $FF]);
  b:= b * 5;
  a:= a xor x[7];
  b:= b - (t1[a and $FF] xor t2[(a shr 16) and $FF] xor t3[(a shr 32) and $FF] xor t4[(a shr 48) and $FF]);
  c:= c + (t4[(a shr 8) and $FF] xor t3[(a shr 24) and $FF] xor t2[(a shr 40) and $FF] xor t1[(a shr 56) and $FF]);
  c:= c * 5;
  x[0]:= x[0] - (x[7] xor $A5A5A5A5A5A5A5A5);
  x[1]:= x[1] xor x[0];
  x[2]:= x[2] + x[1];
  x[3]:= x[3] - (x[2] xor ((not x[1]) shl 19));
  x[4]:= x[4] xor x[3];
  x[5]:= x[5] + x[4];
  x[6]:= x[6] - (x[5] xor ((not x[4]) shr 23));
  x[7]:= x[7] xor x[6];
  x[0]:= x[0] + x[7];
  x[1]:= x[1] - (x[0] xor ((not x[7]) shl 19));
  x[2]:= x[2] xor x[1];
  x[3]:= x[3] + x[2];
  x[4]:= x[4] - (x[3] xor ((not x[2]) shr 23));
  x[5]:= x[5] xor x[4];
  x[6]:= x[6] + x[5];
  x[7]:= x[7] - (x[6] xor $0123456789ABCDEF);
  b:= b xor x[0];
  c:= c - (t1[b and $FF] xor t2[(b shr 16) and $FF] xor t3[(b shr 32) and $FF] xor t4[(b shr 48) and $FF]);
  a:= a + (t4[(b shr 8) and $FF] xor t3[(b shr 24) and $FF] xor t2[(b shr 40) and $FF] xor t1[(b shr 56) and $FF]);
  a:= a * 7;
  c:= c xor x[1];
  a:= a - (t1[c and $FF] xor t2[(c shr 16) and $FF] xor t3[(c shr 32) and $FF] xor t4[(c shr 48) and $FF]);
  b:= b + (t4[(c shr 8) and $FF] xor t3[(c shr 24) and $FF] xor t2[(c shr 40) and $FF] xor t1[(c shr 56) and $FF]);
  b:= b * 7;
  a:= a xor x[2];
  b:= b - (t1[a and $FF] xor t2[(a shr 16) and $FF] xor t3[(a shr 32) and $FF] xor t4[(a shr 48) and $FF]);
  c:= c + (t4[(a shr 8) and $FF] xor t3[(a shr 24) and $FF] xor t2[(a shr 40) and $FF] xor t1[(a shr 56) and $FF]);
  c:= c * 7;
  b:= b xor x[3];
  c:= c - (t1[b and $FF] xor t2[(b shr 16) and $FF] xor t3[(b shr 32) and $FF] xor t4[(b shr 48) and $FF]);
  a:= a + (t4[(b shr 8) and $FF] xor t3[(b shr 24) and $FF] xor t2[(b shr 40) and $FF] xor t1[(b shr 56) and $FF]);
  a:= a * 7;
  c:= c xor x[4];
  a:= a - (t1[c and $FF] xor t2[(c shr 16) and $FF] xor t3[(c shr 32) and $FF] xor t4[(c shr 48) and $FF]);
  b:= b + (t4[(c shr 8) and $FF] xor t3[(c shr 24) and $FF] xor t2[(c shr 40) and $FF] xor t1[(c shr 56) and $FF]);
  b:= b * 7;
  a:= a xor x[5];
  b:= b - (t1[a and $FF] xor t2[(a shr 16) and $FF] xor t3[(a shr 32) and $FF] xor t4[(a shr 48) and $FF]);
  c:= c + (t4[(a shr 8) and $FF] xor t3[(a shr 24) and $FF] xor t2[(a shr 40) and $FF] xor t1[(a shr 56) and $FF]);
  c:= c * 7;
  b:= b xor x[6];
  c:= c - (t1[b and $FF] xor t2[(b shr 16) and $FF] xor t3[(b shr 32) and $FF] xor t4[(b shr 48) and $FF]);
  a:= a + (t4[(b shr 8) and $FF] xor t3[(b shr 24) and $FF] xor t2[(b shr 40) and $FF] xor t1[(b shr 56) and $FF]);
  a:= a * 7;
  c:= c xor x[7];
  a:= a - (t1[c and $FF] xor t2[(c shr 16) and $FF] xor t3[(c shr 32) and $FF] xor t4[(c shr 48) and $FF]);
  b:= b + (t4[(c shr 8) and $FF] xor t3[(c shr 24) and $FF] xor t2[(c shr 40) and $FF] xor t1[(c shr 56) and $FF]);
  b:= b * 7;
  x[0]:= x[0] - (x[7] xor $A5A5A5A5A5A5A5A5);
  x[1]:= x[1] xor x[0];
  x[2]:= x[2] + x[1];
  x[3]:= x[3] - (x[2] xor ((not x[1]) shl 19));
  x[4]:= x[4] xor x[3];
  x[5]:= x[5] + x[4];
  x[6]:= x[6] - (x[5] xor ((not x[4]) shr 23));
  x[7]:= x[7] xor x[6];
  x[0]:= x[0] + x[7];
  x[1]:= x[1] - (x[0] xor ((not x[7]) shl 19));
  x[2]:= x[2] xor x[1];
  x[3]:= x[3] + x[2];
  x[4]:= x[4] - (x[3] xor ((not x[2]) shr 23));
  x[5]:= x[5] xor x[4];
  x[6]:= x[6] + x[5];
  x[7]:= x[7] - (x[6] xor $0123456789ABCDEF);
  a:= a xor x[0];
  b:= b - (t1[a and $FF] xor t2[(a shr 16) and $FF] xor t3[(a shr 32) and $FF] xor t4[(a shr 48) and $FF]);
  c:= c + (t4[(a shr 8) and $FF] xor t3[(a shr 24) and $FF] xor t2[(a shr 40) and $FF] xor t1[(a shr 56) and $FF]);
  c:= c * 9;
  b:= b xor x[1];
  c:= c - (t1[b and $FF] xor t2[(b shr 16) and $FF] xor t3[(b shr 32) and $FF] xor t4[(b shr 48) and $FF]);
  a:= a + (t4[(b shr 8) and $FF] xor t3[(b shr 24) and $FF] xor t2[(b shr 40) and $FF] xor t1[(b shr 56) and $FF]);
  a:= a * 9;
  c:= c xor x[2];
  a:= a - (t1[c and $FF] xor t2[(c shr 16) and $FF] xor t3[(c shr 32) and $FF] xor t4[(c shr 48) and $FF]);
  b:= b + (t4[(c shr 8) and $FF] xor t3[(c shr 24) and $FF] xor t2[(c shr 40) and $FF] xor t1[(c shr 56) and $FF]);
  b:= b * 9;
  a:= a xor x[3];
  b:= b - (t1[a and $FF] xor t2[(a shr 16) and $FF] xor t3[(a shr 32) and $FF] xor t4[(a shr 48) and $FF]);
  c:= c + (t4[(a shr 8) and $FF] xor t3[(a shr 24) and $FF] xor t2[(a shr 40) and $FF] xor t1[(a shr 56) and $FF]);
  c:= c * 9;
  b:= b xor x[4];
  c:= c - (t1[b and $FF] xor t2[(b shr 16) and $FF] xor t3[(b shr 32) and $FF] xor t4[(b shr 48) and $FF]);
  a:= a + (t4[(b shr 8) and $FF] xor t3[(b shr 24) and $FF] xor t2[(b shr 40) and $FF] xor t1[(b shr 56) and $FF]);
  a:= a * 9;
  c:= c xor x[5];
  a:= a - (t1[c and $FF] xor t2[(c shr 16) and $FF] xor t3[(c shr 32) and $FF] xor t4[(c shr 48) and $FF]);
  b:= b + (t4[(c shr 8) and $FF] xor t3[(c shr 24) and $FF] xor t2[(c shr 40) and $FF] xor t1[(c shr 56) and $FF]);
  b:= b * 9;
  a:= a xor x[6];
  b:= b - (t1[a and $FF] xor t2[(a shr 16) and $FF] xor t3[(a shr 32) and $FF] xor t4[(a shr 48) and $FF]);
  c:= c + (t4[(a shr 8) and $FF] xor t3[(a shr 24) and $FF] xor t2[(a shr 40) and $FF] xor t1[(a shr 56) and $FF]);
  c:= c * 9;
  b:= b xor x[7];
  c:= c - (t1[b and $FF] xor t2[(b shr 16) and $FF] xor t3[(b shr 32) and $FF] xor t4[(b shr 48) and $FF]);
  a:= a + (t4[(b shr 8) and $FF] xor t3[(b shr 24) and $FF] xor t2[(b shr 40) and $FF] xor t1[(b shr 56) and $FF]);
  a:= a * 9;

  CurrentHash[0]:= a xor aa;
  CurrentHash[1]:= b - bb;
  CurrentHash[2]:= c + cc;
  Index:= 0;
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
end;

class function TDCP_tiger.GetHashSize: integer;
begin
  Result:= 192;
end;

class function TDCP_tiger.GetId: integer;
begin
  Result:= DCP_tiger;
end;

class function TDCP_tiger.GetAlgorithm: string;
begin
  Result:= 'Tiger';
end;

class function TDCP_tiger.SelfTest: boolean;
const
  Test1Out: array[0..2] of int64=
    ($87FB2A9083851CF7,$470D2CF810E6DF9E,$B586445034A5A386);
  Test2Out: array[0..2] of int64=
    ($0C410A042968868A,$1671DA5A3FD29A72,$5EC1E457D3CDB303);
var
  TestHash: TDCP_tiger;
  TestOut: array[0..2] of int64;
begin
  dcpFillChar(TestOut, SizeOf(TestOut), 0);
  TestHash:= TDCP_tiger.Create(nil);
  TestHash.Init;
  TestHash.UpdateStr('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-');
  TestHash.Final(TestOut);
  Result:= CompareMem(@TestOut,@Test1Out,Sizeof(Test1Out));
  TestHash.Init;
  TestHash.UpdateStr('Tiger - A Fast New Hash Function, by Ross Anderson and Eli Biham');
  TestHash.Final(TestOut);
  Result:= CompareMem(@TestOut,@Test2Out,Sizeof(Test2Out)) and Result;
  TestHash.Free;
end;

procedure TDCP_tiger.Init;
begin
  Burn;
  fInitialized:= true;
  CurrentHash[0]:= $0123456789ABCDEF;
  CurrentHash[1]:= $FEDCBA9876543210;
  CurrentHash[2]:= $F096A5B4C3B2E187;
end;

procedure TDCP_tiger.Burn;
begin
  Len:= 0;
  Index:= 0;
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
  FillChar(CurrentHash,Sizeof(CurrentHash),0);
  fInitialized:= false;
end;

procedure TDCP_tiger.Update(const Buffer; Size: longword);
var
  PBuf: ^byte;
begin
  if not fInitialized then
    raise EDCP_hash.Create('Hash not initialized');

  Inc(Len,Size*8);

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

procedure TDCP_tiger.Final(var Digest);
begin
  if not fInitialized then
    raise EDCP_hash.Create('Hash not initialized');
  HashBuffer[Index]:= $01;
  if Index>= 56 then
    Compress;
  Pint64(@HashBuffer[56])^:= Len;
  Compress;
  Move(CurrentHash,Digest,Sizeof(CurrentHash));
  Burn;
end;

end.
