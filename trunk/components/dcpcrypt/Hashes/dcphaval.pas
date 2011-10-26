{******************************************************************************}
{* DCPcrypt v2.0 written by David Barton (crypto@cityinthesky.co.uk) **********}
{******************************************************************************}
{* A binary compatible implementation of Haval ********************************}
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
unit DCPhaval;

{$MODE Delphi}

interface
uses
  Classes, Sysutils, DCPcrypt2, DCPconst;

type
  TDCP_haval= class(TDCP_hash)
  protected
    LenHi, LenLo: longword;
    Index: DWord;
    CurrentHash: array[0..7] of DWord;
    HashBuffer: array[0..127] of byte;
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

{ Choose how many passes (previous versions of DCPcrypt uses 5 passes) }
{ ONLY UNCOMMENT ONE! }
//{$DEFINE PASS3}
//{$DEFINE PASS4}
{$DEFINE PASS5}

{ Choose digest length (previous versions of DCPcrypt uses 256bits) }
{ ONLY UNCOMMENT ONE! }
//{$DEFINE DIGEST128}
//{$DEFINE DIGEST160}
//{$DEFINE DIGEST192}
//{$DEFINE DIGEST224}
{$DEFINE DIGEST256}


{******************************************************************************}
{******************************************************************************}
implementation
{$R-}{$Q-}

procedure TDCP_haval.Compress;
var
  t7, t6, t5, t4, t3, t2, t1, t0: DWord;
  W: array[0..31] of DWord;
  Temp: dword;
begin
  dcpFillChar(W, SizeOf(W), 0);
  t0:= CurrentHash[0];
  t1:= CurrentHash[1];
  t2:= CurrentHash[2];
  t3:= CurrentHash[3];
  t4:= CurrentHash[4];
  t5:= CurrentHash[5];
  t6:= CurrentHash[6];
  t7:= CurrentHash[7];
  Move(HashBuffer,W,Sizeof(W));

{$IFDEF PASS3}
  {$INCLUDE DCPhaval3.inc}
{$ELSE}
  {$IFDEF PASS4}
    {$INCLUDE DCPhaval4.inc}
  {$ELSE}
    {$INCLUDE DCPhaval5.inc}
  {$ENDIF}
{$ENDIF}

  Inc(CurrentHash[0],t0);
  Inc(CurrentHash[1],t1);
  Inc(CurrentHash[2],t2);
  Inc(CurrentHash[3],t3);
  Inc(CurrentHash[4],t4);
  Inc(CurrentHash[5],t5);
  Inc(CurrentHash[6],t6);
  Inc(CurrentHash[7],t7);
  FillChar(W,Sizeof(W),0);
  Index:= 0;
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
end;

class function TDCP_haval.GetHashSize: integer;
begin
{$IFDEF DIGEST128}
  Result:= 128;
{$ELSE}
  {$IFDEF DIGEST160}
    Result:= 160;
  {$ELSE}
    {$IFDEF DIGEST192}
      Result:= 192;
    {$ELSE}
      {$IFDEF DIGEST224}
        Result:= 224;
      {$ELSE}
        Result:= 256;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

class function TDCP_haval.GetId: integer;
begin
  Result:= DCP_haval;
end;

class function TDCP_haval.GetAlgorithm: string;
begin
  Result:= 'Haval (';
{$IFDEF DIGEST128}
  Result:= Result+'128bit, ';
{$ELSE}
  {$IFDEF DIGEST160}
    Result:= Result+'160bit, ';
  {$ELSE}
    {$IFDEF DIGEST192}
      Result:= Result+'192bit, ';
    {$ELSE}
      {$IFDEF DIGEST224}
        Result:= Result+'224bit, ';
      {$ELSE}
        Result:= Result+'256bit, ';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$IFDEF PASS3}
  Result:= Result+'3 passes)';
{$ELSE}
  {$IFDEF PASS4}
    Result:= Result+'4 passes)';
  {$ELSE}
    Result:= Result+'5 passes)';
  {$ENDIF}
{$ENDIF}
end;

class function TDCP_haval.SelfTest: boolean;
{$IFDEF PASS3}
  {$IFDEF DIGEST128}
    const
      Test1Out: array[0..15] of byte=
        ($1B,$DC,$55,$6B,$29,$AD,$02,$EC,$09,$AF,$8C,$66,$47,$7F,$2A,$87);
    var
      TestHash: TDCP_haval;
      TestOut: array[0..15] of byte;
    begin
      TestHash:= TDCP_haval.Create(nil);
      TestHash.Init;
      TestHash.UpdateStr('');
      TestHash.Final(TestOut);
      Result:= CompareMem(@TestOut,@Test1Out,Sizeof(Test1Out));
      TestHash.Free;
  {$ELSE}
    {$IFDEF DIGEST160}
      const
        Test1Out: array[0..19] of byte=
          ($5E,$16,$10,$FC,$ED,$1D,$3A,$DB,$0B,$B1,
           $8E,$92,$AC,$2B,$11,$F0,$BD,$99,$D8,$ED);
      var
        TestHash: TDCP_haval;
        TestOut: array[0..19] of byte;
      begin
        TestHash:= TDCP_haval.Create(nil);
        TestHash.Init;
        TestHash.UpdateStr('a');
        TestHash.Final(TestOut);
        Result:= CompareMem(@TestOut,@Test1Out,Sizeof(Test1Out));
        TestHash.Free;
    {$ELSE}
      begin
        Result:= true;
    {$ENDIF}
  {$ENDIF}
{$ELSE}
  {$IFDEF PASS4}
    {$IFDEF DIGEST192}
      const
        Test1Out: array[0..23] of byte=
          ($74,$AA,$31,$18,$2F,$F0,$9B,$CC,$E4,$53,$A7,$F7,
           $1B,$5A,$7C,$5E,$80,$87,$2F,$A9,$0C,$D9,$3A,$E4);
      var
        TestHash: TDCP_haval;
        TestOut: array[0..23] of byte;
      begin
        TestHash:= TDCP_haval.Create(nil);
        TestHash.Init;
        TestHash.UpdateStr('HAVAL');
        TestHash.Final(TestOut);
        Result:= CompareMem(@TestOut,@Test1Out,Sizeof(Test1Out));
        TestHash.Free;
    {$ELSE}
      {$IFDEF DIGEST224}
        const
          Test1Out: array[0..27] of byte=
            ($14,$4C,$B2,$DE,$11,$F0,$5D,$F7,$C3,$56,$28,$2A,$3B,$48,
             $57,$96,$DA,$65,$3F,$6B,$70,$28,$68,$C7,$DC,$F4,$AE,$76);
        var
          TestHash: TDCP_haval;
          TestOut: array[0..27] of byte;
        begin
          TestHash:= TDCP_haval.Create(nil);
          TestHash.Init;
          TestHash.UpdateStr('0123456789');
          TestHash.Final(TestOut);
          Result:= CompareMem(@TestOut,@Test1Out,Sizeof(Test1Out));
          TestHash.Free;
      {$ELSE}
        begin
          Result:= true;
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
    {$IFDEF DIGEST256}
      const
        Test1Out: array[0..31] of byte=
          ($1A,$1D,$C8,$09,$9B,$DA,$A7,$F3,$5B,$4D,$A4,$E8,$05,$F1,$A2,$8F,
           $EE,$90,$9D,$8D,$EE,$92,$01,$98,$18,$5C,$BC,$AE,$D8,$A1,$0A,$8D);
        Test2Out: array[0..31] of byte=
          ($C5,$64,$7F,$C6,$C1,$87,$7F,$FF,$96,$74,$2F,$27,$E9,$26,$6B,$68,
           $74,$89,$4F,$41,$A0,$8F,$59,$13,$03,$3D,$9D,$53,$2A,$ED,$DB,$39);
      var
        TestHash: TDCP_haval;
        TestOut: array[0..31] of byte;
      begin
        dcpFillChar(TestOut, SizeOf(TestOut), 0);
        TestHash:= TDCP_haval.Create(nil);
        TestHash.Init;
        TestHash.UpdateStr('abcdefghijklmnopqrstuvwxyz');
        TestHash.Final(TestOut);
        Result:= CompareMem(@TestOut,@Test1Out,Sizeof(Test1Out));
        TestHash.Init;
        TestHash.UpdateStr('ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789');
        TestHash.Final(TestOut);
        Result:= CompareMem(@TestOut,@Test2Out,Sizeof(Test2Out)) and Result;
        TestHash.Free;
    {$ELSE}
      begin
        Result:= true;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

procedure TDCP_haval.Init;
begin
  Burn;
  CurrentHash[0]:= $243F6A88;
  CurrentHash[1]:= $85A308D3;
  CurrentHash[2]:= $13198A2E;
  CurrentHash[3]:= $03707344;
  CurrentHash[4]:= $A4093822;
  CurrentHash[5]:= $299F31D0;
  CurrentHash[6]:= $082EFA98;
  CurrentHash[7]:= $EC4E6C89;
  fInitialized:= true;
end;

procedure TDCP_haval.Burn;
begin
  LenHi:= 0; LenLo:= 0;
  Index:= 0;
  FillChar(HashBuffer,Sizeof(HashBuffer),0);
  FillChar(CurrentHash,Sizeof(CurrentHash),0);
  fInitialized:= false;
end;

procedure TDCP_haval.Update(const Buffer; Size: longword);
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

procedure TDCP_haval.Final(var Digest);
{$IFNDEF DIGEST256}
  {$IFNDEF DIGEST224}
    var
      temp: dword;
  {$ENDIF}
{$ENDIF}
begin
  if not fInitialized then
    raise EDCP_hash.Create('Hash not initialized');
  HashBuffer[Index]:= $80;
  if Index>= 118 then
    Compress;
{$IFDEF PASS3}
  {$IFDEF DIGEST128}
    HashBuffer[118]:= ((128 and 3) shl 6) or (3 shl 3) or 1;
    HashBuffer[119]:= (128 shr 2) and $FF;
  {$ELSE}
    {$IFDEF DIGEST160}
      HashBuffer[118]:= ((160 and 3) shl 6) or (3 shl 3) or 1;
      HashBuffer[119]:= (160 shr 2) and $FF;
    {$ELSE}
      {$IFDEF DIGEST192}
        HashBuffer[118]:= ((192 and 3) shl 6) or (3 shl 3) or 1;
        HashBuffer[119]:= (192 shr 2) and $FF;
      {$ELSE}
        {$IFDEF DIGEST224}
          HashBuffer[118]:= ((224 and 3) shl 6) or (3 shl 3) or 1;
          HashBuffer[119]:= (224 shr 2) and $FF;
        {$ELSE}
          HashBuffer[118]:= ((256 and 3) shl 6) or (3 shl 3) or 1;
          HashBuffer[119]:= (256 shr 2) and $FF;
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ELSE}
  {$IFDEF PASS4}
    {$IFDEF DIGEST128}
      HashBuffer[118]:= ((128 and 3) shl 6) or (4 shl 3) or 1;
      HashBuffer[119]:= (128 shr 2) and $FF;
    {$ELSE}
      {$IFDEF DIGEST160}
        HashBuffer[118]:= ((160 and 3) shl 6) or (4 shl 3) or 1;
        HashBuffer[119]:= (160 shr 2) and $FF;
      {$ELSE}
        {$IFDEF DIGEST192}
          HashBuffer[118]:= ((192 and 3) shl 6) or (4 shl 3) or 1;
          HashBuffer[119]:= (192 shr 2) and $FF;
        {$ELSE}
          {$IFDEF DIGEST224}
            HashBuffer[118]:= ((224 and 3) shl 6) or (4 shl 3) or 1;
            HashBuffer[119]:= (224 shr 2) and $FF;
          {$ELSE}
            HashBuffer[118]:= ((256 and 3) shl 6) or (4 shl 3) or 1;
            HashBuffer[119]:= (256 shr 2) and $FF;
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
    {$IFDEF DIGEST128}
      HashBuffer[118]:= ((128 and 3) shl 6) or (5 shl 3) or 1;
      HashBuffer[119]:= (2128 shr 2) and $FF;
    {$ELSE}
      {$IFDEF DIGEST160}
        HashBuffer[118]:= ((160 and 3) shl 6) or (5 shl 3) or 1;
        HashBuffer[119]:= (160 shr 2) and $FF;
      {$ELSE}
        {$IFDEF DIGEST192}
          HashBuffer[118]:= ((192 and 3) shl 6) or (5 shl 3) or 1;
          HashBuffer[119]:= (192 shr 2) and $FF;
        {$ELSE}
          {$IFDEF DIGEST224}
            HashBuffer[118]:= ((224 and 3) shl 6) or (5 shl 3) or 1;
            HashBuffer[119]:= (224 shr 2) and $FF;
          {$ELSE}
            HashBuffer[118]:= ((256 and 3) shl 6) or (5 shl 3) or 1;
            HashBuffer[119]:= (256 shr 2) and $FF;
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
  PDWord(@HashBuffer[120])^:= LenLo;
  PDWord(@HashBuffer[124])^:= LenHi;
  Compress;
{$IFDEF DIGEST128}
  temp:= (CurrentHash[7] and $000000FF) or
         (CurrentHash[6] and $FF000000) or
         (CurrentHash[5] and $00FF0000) or
         (CurrentHash[4] and $0000FF00);
  Inc(CurrentHash[0],(temp shr 8) or (temp shl 24));
  temp:= (CurrentHash[7] and $0000FF00) or
         (CurrentHash[6] and $000000FF) or
         (CurrentHash[5] and $FF000000) or
         (CurrentHash[4] and $00FF0000);
  Inc(CurrentHash[1],(temp shr 16) or (temp shl 16));
  temp:= (CurrentHash[7] and $00FF0000) or
         (CurrentHash[6] and $0000FF00) or
         (CurrentHash[5] and $000000FF) or
         (CurrentHash[4] and $FF000000);
  Inc(CurrentHash[2],(temp shr 24) or (temp shl 8));
  temp:= (CurrentHash[7] and $FF000000) or
         (CurrentHash[6] and $00FF0000) or
         (CurrentHash[5] and $0000FF00) or
         (CurrentHash[4] and $000000FF);
  Inc(CurrentHash[3],temp);
  Move(CurrentHash,Digest,128 div 8);
{$ELSE}
  {$IFDEF DIGEST160}
    temp:= (CurrentHash[7] and $3F) or
           (CurrentHash[6] and ($7F shl 25)) or
           (CurrentHash[5] and ($3F shl 19));
    Inc(CurrentHash[0],(temp shr 19) or (temp shl 13));
    temp:= (CurrentHash[7] and ($3F shl 6)) or
           (CurrentHash[6] and $3F) or
           (CurrentHash[5] and ($7F shl 25));
    Inc(CurrentHash[1],(temp shr 25) or (temp shl 7));
    temp:= (CurrentHash[7] and ($7F shl 12)) or
           (CurrentHash[6] and ($3F shl 6)) or
           (CurrentHash[5] and $3F);
    Inc(CurrentHash[2],temp);
    temp:= (CurrentHash[7] and ($3F shl 19)) or
           (CurrentHash[6] and ($7F shl 12)) or
           (CurrentHash[5] and ($3F shl 6));
    Inc(CurrentHash[3],temp shr 6);
    temp:= (CurrentHash[7] and ($7F shl 25)) or
           (CurrentHash[6] and ($3F shl 19)) or
           (CurrentHash[5] and ($7F shl 12));
    Inc(CurrentHash[4],temp shr 12);
    Move(CurrentHash,Digest,160 div 8);
  {$ELSE}
    {$IFDEF DIGEST192}
      temp:= (CurrentHash[7] and $1F) or
             (CurrentHash[6] and ($3F shl 26));
      Inc(CurrentHash[0],(temp shr 26) or (temp shl 6));
      temp:= (CurrentHash[7] and ($1F shl 5)) or
             (CurrentHash[6] and $1F);
      Inc(CurrentHash[1],temp);
      temp:= (CurrentHash[7] and ($3F shl 10)) or
             (CurrentHash[6] and ($1F shl 5));
      Inc(CurrentHash[2],temp shr 5);
      temp:= (CurrentHash[7] and ($1F shl 16)) or
             (CurrentHash[6] and ($3F shl 10));
      Inc(CurrentHash[3],temp shr 10);
      temp:= (CurrentHash[7] and ($1F shl 21)) or
             (CurrentHash[6] and ($1F shl 16));
      Inc(CurrentHash[4],temp shr 16);
      temp:= (CurrentHash[7] and ($3F shl 26)) or
             (CurrentHash[6] and ($1F shl 21));
      Inc(CurrentHash[5],temp shr 21);
      Move(CurrentHash,Digest,192 div 8);
    {$ELSE}
      {$IFDEF DIGEST224}
        Inc(CurrentHash[0],(CurrentHash[7] shr 27) and $1F);
        Inc(CurrentHash[1],(CurrentHash[7] shr 22) and $1F);
        Inc(CurrentHash[2],(CurrentHash[7] shr 18) and $F);
        Inc(CurrentHash[3],(CurrentHash[7] shr 13) and $1F);
        Inc(CurrentHash[4],(CurrentHash[7] shr 9) and $F);
        Inc(CurrentHash[5],(CurrentHash[7] shr 4) and $1F);
        Inc(CurrentHash[6],CurrentHash[7] and $F);
        Move(CurrentHash,Digest,224 div 8);
      {$ELSE}
        Move(CurrentHash,Digest,256 div 8);
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
  Burn;
end;

end.
