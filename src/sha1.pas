{
***************************************************
* A binary compatible SHA1 implementation         *
* written by Dave Barton (davebarton@bigfoot.com) *
***************************************************
* 160bit hash size                                *
***************************************************
}
unit SHA1;

{$mode delphi}{$H+}

interface

uses
  SysUtils;

type
  TSHA1Digest= array[0..19] of byte;
  TSHA1Context= record
    Hash: array[0..4] of DWord;
    Hi, Lo: integer;
    Buffer: array[0..63] of byte;
    Index: integer;
  end;
  PSHA1Context = ^TSHA1Context;

procedure SHA1Init(var Context: TSHA1Context);
procedure SHA1Update(var Context: TSHA1Context; var Buffer; Len: integer);
procedure SHA1Final(var Context: TSHA1Context; var Digest: TSHA1Digest);

//******************************************************************************
implementation
{$R-}

function LRot32(X: DWord; n: Byte): DWord;
begin
  Result:= (x shl n) or (x shr (32 - n));
end;

//******************************************************************************
function F1(x, y, z: DWord): DWord;
begin
  Result:= z xor (x and (y xor z));
end;
function F2(x, y, z: DWord): DWord;
begin
  Result:= x xor y xor z;
end;
function F3(x, y, z: DWord): DWord;
begin
  Result:= (x and y) or (z and (x or y));
end;

//******************************************************************************
function RB(A: DWord): DWord;
begin
  Result:= (A shr 24) or ((A shr 8) and $FF00) or ((A shl 8) and $FF0000) or (A shl 24);
end;

procedure SHA1Compress(var Data: TSHA1Context);
var
  A, B, C, D, E, T: DWord;
  W: array[0..79] of DWord;
  i: integer;
begin
  Move(Data.Buffer,W,Sizeof(Data.Buffer));
  for i:= 0 to 15 do
    W[i]:= RB(W[i]);
  for i:= 16 to 79 do
    W[i]:= LRot32(W[i-3] xor W[i-8] xor W[i-14] xor W[i-16],1);
  A:= Data.Hash[0]; B:= Data.Hash[1]; C:= Data.Hash[2]; D:= Data.Hash[3]; E:= Data.Hash[4];
  for i:= 0 to 19 do
  begin
    T:= LRot32(A,5) + F1(B,C,D) + E + W[i] + $5A827999;
    E:= D; D:= C; C:= LRot32(B,30); B:= A; A:= T;
  end;
  for i:= 20 to 39 do
  begin
    T:= LRot32(A,5) + F2(B,C,D) + E + W[i] + $6ED9EBA1;
    E:= D; D:= C; C:= LRot32(B,30); B:= A; A:= T;
  end;
  for i:= 40 to 59 do
  begin
    T:= LRot32(A,5) + F3(B,C,D) + E + W[i] + $8F1BBCDC;
    E:= D; D:= C; C:= LRot32(B,30); B:= A; A:= T;
  end;
  for i:= 60 to 79 do
  begin
    T:= LRot32(A,5) + F2(B,C,D) + E + W[i] + $CA62C1D6;
    E:= D; D:= C; C:= LRot32(B,30); B:= A; A:= T;
  end;
  Data.Hash[0]:= Data.Hash[0] + A;
  Data.Hash[1]:= Data.Hash[1] + B;
  Data.Hash[2]:= Data.Hash[2] + C;
  Data.Hash[3]:= Data.Hash[3] + D;
  Data.Hash[4]:= Data.Hash[4] + E;
  FillChar(W,Sizeof(W),0);
  FillChar(Data.Buffer,Sizeof(Data.Buffer),0);
end;

//******************************************************************************
procedure SHA1Init(var Context: TSHA1Context);
begin
  Context.Hi:= 0; Context.Lo:= 0;
  Context.Index:= 0;
  FillChar(Context.Buffer,Sizeof(Context.Buffer),0);
  Context.Hash[0]:= $67452301;
  Context.Hash[1]:= $EFCDAB89;
  Context.Hash[2]:= $98BADCFE;
  Context.Hash[3]:= $10325476;
  Context.Hash[4]:= $C3D2E1F0;
end;

//******************************************************************************
procedure SHA1UpdateLen(var Context: TSHA1Context; Len: integer);
var
  i, k: integer;
begin
  for k:= 0 to 7 do
  begin
    i:= Context.Lo;
    Inc(Context.Lo,Len);
    if Context.Lo< i then
      Inc(Context.Hi);
  end;
end;

//******************************************************************************
procedure SHA1Update(var Context: TSHA1Context; var Buffer; Len: integer);
type
  PByte= ^Byte;
var
  Src: Pointer;
begin
  Src:= @Buffer;
  SHA1UpdateLen(Context,Len);
  while Len> 0 do
  begin
    Context.Buffer[Context.Index]:= PByte(Src)^;
    Inc(PByte(Src));
    Inc(Context.Index);
    Dec(Len);
    if Context.Index= 64 then
    begin
      Context.Index:= 0;
      SHA1Compress(Context);
    end;
  end;
end;

//******************************************************************************
procedure SHA1Final(var Context: TSHA1Context; var Digest: TSHA1Digest);
type
  PDWord= ^DWord;
begin
  Context.Buffer[Context.Index]:= $80;
  if Context.Index>= 56 then
    SHA1Compress(Context);
  PDWord(@Context.Buffer[56])^:= RB(Context.Hi);
  PDWord(@Context.Buffer[60])^:= RB(Context.Lo);
  SHA1Compress(Context);
  Context.Hash[0]:= RB(Context.Hash[0]);
  Context.Hash[1]:= RB(Context.Hash[1]);
  Context.Hash[2]:= RB(Context.Hash[2]);
  Context.Hash[3]:= RB(Context.Hash[3]);
  Context.Hash[4]:= RB(Context.Hash[4]);
  Move(Context.Hash,Digest,Sizeof(Digest));
  FillChar(Context,Sizeof(Context),0);
end;


end.
