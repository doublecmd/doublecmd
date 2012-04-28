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
 * Craig Peterson <capeterson@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbUnzPrc.pas                                *}
{*********************************************************}
{* ABBREVIA: UnZip procedures                            *}
{*********************************************************}

unit AbUnzPrc;

{$I AbDefine.inc}

interface

uses
  Classes,
  AbArcTyp,
  AbZipTyp;

type
  TAbUnzipHelper = class( TObject )
  protected {private}
    {internal variables}
    FOutWriter : TStream;
    FOutStream : TStream;
    FUnCompressedSize : LongInt;
    FCompressionMethod : TAbZipCompressionMethod;
    FDictionarySize : TAbZipDictionarySize;
    FShannonFanoTreeCount : Byte;

    FOutBuf : PAbByteArray;          {output buffer}
    FOutSent : LongInt;              {number of bytes sent to output buffer}
    FOutPos : Cardinal;              {current position in output buffer}
    FBitSValid : Byte;               {Number of valid bits}

    FInBuf : TAbByteArray4K;
    FInPos : Integer;                {current position in input buffer}
    FInCnt : Integer;                {number of bytes in input buffer}
    FInEof  : Boolean;               {set when stream read returns 0}
    FCurByte : Byte;                 {current input byte}
    FBitsLeft : Byte;                {bits left to process in FCurByte}

    FZStream : TStream;
  protected
    procedure uzFlushOutBuf;
      {-Flushes the output buffer}

    function uzReadBits(Bits : Byte) : Integer;
      {-Read the specified number of bits}
    procedure uzReadNextPrim;
      {-does less likely part of uzReadNext}

    {$IFDEF UnzipImplodeSupport}
    procedure uzUnImplode;
      {-Extract an imploded file}
    {$ENDIF}

    {$IFDEF UnzipReduceSupport}
    procedure uzUnReduce;
      {-Extract a reduced file}
    {$ENDIF}

    {$IFDEF UnzipShrinkSupport}
    procedure uzUnShrink;
      {-Extract a shrunk file}
    {$ENDIF}
    procedure uzWriteByte(B : Byte);
      {write to output}
  public
    constructor Create( InputStream, OutputStream : TStream );
    destructor Destroy;
      override;

    procedure Execute;

    property UnCompressedSize : LongInt
      read FUncompressedSize
      write FUncompressedSize;
    property CompressionMethod : TAbZipCompressionMethod
      read FCompressionMethod
      write FCompressionMethod;
    property DictionarySize : TAbZipDictionarySize
      read FDictionarySize
      write FDictionarySize;
    property ShannonFanoTreeCount : Byte
      read FShannonFanoTreeCount
      write FShannonFanoTreeCount;
  end;



  procedure AbUnzipToStream( Sender : TObject; Item : TAbZipItem;
    OutStream : TStream);

  procedure AbUnzip(Sender : TObject; Item : TAbZipItem; const UseName : string);

  procedure AbTestZipItem(Sender : TObject; Item : TAbZipItem);

  procedure InflateStream(CompressedStream, UnCompressedStream : TStream);
  {-Inflates everything in CompressedStream to UncompressedStream
    no encryption is tried, no check on CRC is done, uses the whole
    compressedstream - no Progress events - no Frills!}

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils,
  {$IFDEF UnzipBzip2Support}
  AbBzip2,
  {$ENDIF}
  {$IFDEF UnzipLzmaSupport}
  AbLzma,
  {$ENDIF}
  {$IFDEF UnzipPPMdSupport}
  AbPPMd,
  {$ENDIF}
  {$IFDEF UnzipWavPackSupport}
  AbWavPack,
  {$ENDIF}
  AbBitBkt,
  AbConst,
  AbDfBase,
  AbDfCryS,
  AbDfDec,
  AbExcept,
  AbSpanSt,
  AbSWStm,
  AbUnzOutStm,
  AbUtils,
  DCClassesUtf8;

{ -------------------------------------------------------------------------- }
procedure AbReverseBits(var W : Word);
  {-Reverse the order of the bits in W}
register;
const
  RevTable : array[0..255] of Byte = ($00, $80, $40, $C0, $20, $A0, $60,
   $E0, $10, $90, $50, $D0, $30, $B0, $70, $F0, $08, $88, $48, $C8, $28,
   $A8, $68, $E8, $18, $98, $58, $D8, $38, $B8, $78, $F8, $04, $84, $44,
   $C4, $24, $A4, $64, $E4, $14, $94, $54, $D4, $34, $B4, $74, $F4, $0C,
   $8C, $4C, $CC, $2C, $AC, $6C, $EC, $1C, $9C, $5C, $DC, $3C, $BC, $7C,
   $FC, $02, $82, $42, $C2, $22, $A2, $62, $E2, $12, $92, $52, $D2, $32,
   $B2, $72, $F2, $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA, $1A, $9A, $5A,
   $DA, $3A, $BA, $7A, $FA, $06, $86, $46, $C6, $26, $A6, $66, $E6, $16,
   $96, $56, $D6, $36, $B6, $76, $F6, $0E, $8E, $4E, $CE, $2E, $AE, $6E,
   $EE, $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE, $01, $81, $41, $C1, $21,
   $A1, $61, $E1, $11, $91, $51, $D1, $31, $B1, $71, $F1, $09, $89, $49,
   $C9, $29, $A9, $69, $E9, $19, $99, $59, $D9, $39, $B9, $79, $F9, $05,
   $85, $45, $C5, $25, $A5, $65, $E5, $15, $95, $55, $D5, $35, $B5, $75,
   $F5, $0D, $8D, $4D, $CD, $2D, $AD, $6D, $ED, $1D, $9D, $5D, $DD, $3D,
   $BD, $7D, $FD, $03, $83, $43, $C3, $23, $A3, $63, $E3, $13, $93, $53,
   $D3, $33, $B3, $73, $F3, $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB, $1B,
   $9B, $5B, $DB, $3B, $BB, $7B, $FB, $07, $87, $47, $C7, $27, $A7, $67,
   $E7, $17, $97, $57, $D7, $37, $B7, $77, $F7, $0F, $8F, $4F, $CF, $2F,
   $AF, $6F, $EF, $1F, $9F, $5F, $DF, $3F, $BF, $7F, $FF);
begin
  W := RevTable[Byte(W shr 8)] or Word(RevTable[Byte(W)] shl 8);
end;


{ TAbUnzipHelper implementation ============================================ }

{ -------------------------------------------------------------------------- }
constructor TAbUnzipHelper.Create( InputStream, OutputStream : TStream );
begin
  inherited Create;
  FOutBuf := AllocMem( AbBufferSize );
  FOutPos := 0;                                                        
  FZStream := InputStream;
  FOutStream := OutputStream;
  FUncompressedSize := 0;
  FDictionarySize := dsInvalid;
  FShannonFanoTreeCount := 0;
  FCompressionMethod := cmDeflated;
end;
{ -------------------------------------------------------------------------- }
destructor TAbUnzipHelper.Destroy;
begin
  FreeMem( FOutBuf, AbBufferSize );                                    
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
procedure TAbUnzipHelper.Execute;
begin
  {parent class handles exceptions via OnExtractFailure}
  FBitsLeft := 0;
  FCurByte := 0;
  FInCnt := 0;
  FOutSent := 0;
  FOutPos := 0;
  FInEof := False;

  {set the output stream; for Imploded/Reduced files this has to be
   buffered, for all other types of compression, the code buffers the
   output data nicely and so the given output stream can be used.}
  {$IFDEF UnzipImplodeSupport}
  if (FCompressionMethod = cmImploded) then
    FOutWriter := TabSlidingWindowStream.Create(FOutStream)
  else
  {$ENDIF}
  {$IFDEF UnzipReduceSupport}
  if (FCompressionMethod >= cmReduced1) and
     (FCompressionMethod <= cmReduced4) then
    FOutWriter := TabSlidingWindowStream.Create(FOutStream)
  else
  {$ENDIF}
    FOutWriter := FOutStream;
  FInPos := 1+SizeOf(FInBuf);

{  GetMem( FInBuf, SizeOf(FInBuf^) );}
  try
    {uncompress it with the appropriate method}
    case FCompressionMethod of
      {$IFDEF UnzipShrinkSupport}
      cmShrunk               : uzUnshrink;
      {$ENDIF}
      {$IFDEF UnzipReduceSupport}
      cmReduced1..cmReduced4 : uzUnReduce;
      {$ENDIF}
      {$IFDEF UnzipImplodeSupport}
      cmImploded             : uzUnImplode;
      {$ENDIF}
      {cmTokenized}
      {cmEnhancedDeflated}
      {cmDCLImploded}
    else
      raise EAbZipInvalidMethod.Create;
    end;

  finally
    uzFlushOutBuf;
    {free any memory}
    if (FOutWriter <> FOutStream) then
      FOutWriter.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbUnzipHelper.uzReadNextPrim;
begin
  FInCnt := FZStream.Read( FInBuf, sizeof( FInBuf ) );
  FInEof := FInCnt = 0;
  {load first byte in buffer and set position counter}
  FCurByte := FInBuf[1];
  FInPos := 2;
end;
{ -------------------------------------------------------------------------- }
procedure TAbUnzipHelper.uzFlushOutBuf;
  {-flushes the output buffer}
begin
  if (FOutPos <> 0) then begin
    FOutWriter.Write( FOutBuf^, FOutPos );
    Inc( FOutSent, FOutPos );
    FOutPos := 0;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbUnzipHelper.uzWriteByte(B : Byte);
  {-Write one byte to the output buffer}
begin
  FOutBuf^[FOutPos] := B;
  inc(FOutPos);
  if (FOutPos = AbBufferSize) or
     (LongInt(FOutPos) + FOutSent = FUncompressedSize) then
    uzFlushOutBuf;
end;
{ -------------------------------------------------------------------------- }
function TAbUnzipHelper.uzReadBits(Bits : Byte) : Integer;
  {-Read the specified number of bits}
var
  SaveCurByte, Delta, SaveBitsLeft : Byte;
begin
  {read next byte if we're out of bits}
  if FBitsLeft = 0 then begin
    {do we still have a byte buffered?}
    if FInPos <= FInCnt then begin
      {get next byte out of buffer and advance position counter}
      FCurByte := FInBuf[FInPos];
      Inc(FInPos);
    end
    {are there any left to read?}
    else
      uzReadNextPrim;

    FBitsLeft := 8;
  end;
  if ( Bits < FBitsLeft ) then begin
    Dec( FBitsLeft, Bits );
    Result := ((1 shl Bits) - 1) and FCurByte;
    FCurByte := FCurByte shr Bits;
  end
  else if ( Bits = FBitsLeft ) then begin
    Result := FCurByte;
    FCurByte := 0;
    FBitsLeft := 0;
  end
  else begin
    SaveCurByte := FCurByte;
    SaveBitsLeft := FBitsLeft;
    {number of additional bits that we need}
    Delta := Bits - FBitsLeft;
    {do we still have a byte buffered?}
    if FInPos <= FInCnt then begin
      {get next byte out of buffer and advance position counter}
      FCurByte := FInBuf[FInPos];
      Inc(FInPos);
    end
    {are there any left to read?}
    else
      uzReadNextPrim;

    FBitsLeft := 8;
    Result := ( uzReadBits( Delta ) shl SaveBitsLeft ) or SaveCurByte;
  end;
end;
{$IFDEF UnzipImplodeSupport}
{ -------------------------------------------------------------------------- }
procedure TAbUnzipHelper.uzUnImplode;
    {-Extract an imploded file}
const
  szLengthTree = SizeOf(TAbSfTree)-(192*SizeOf(TAbSfEntry));
  szDistanceTree = SizeOf(TAbSfTree)-(192*SizeOf(TAbSfEntry));
  szLitTree = SizeOf(TAbSfTree);
var
  Length : Integer;
  DIndex : LongInt;
  Distance : Integer;
  SPos : LongInt;
  MyByte : Byte;
  DictBits : Integer;             {number of bits used in sliding dictionary}
  MinMatchLength : Integer;       {minimum match length}
  LitTree : PAbSfTree;              {Literal tree}
  LengthTree : PAbSfTree;           {Length tree}
  DistanceTree : PAbSfTree;         {Distance tree}

  procedure uzLoadTree(var T; TreeSize : Integer);
    {-Load one Shannon-Fano tree}
  var
    I : Word;
    Tree : TAbSfTree absolute T;

    procedure GenerateTree;
      {-Generate a Shannon-Fano tree}
    var
      C : Word;
      CodeIncrement : Integer;
      LastBitLength : Integer;
      I : Integer;
    begin
      C := 0;
      CodeIncrement := 0;
      LastBitLength := 0;

      for I := Tree.Entries-1 downto 0 do
        with Tree.Entry[I] do begin
          Inc(C, CodeIncrement);
          if BitLength <> LastBitLength then begin
            LastBitLength := BitLength;
            CodeIncrement := 1 shl (16-LastBitLength);
          end;
          Code := C;
        end;
    end;

    procedure SortLengths;
      {-Sort the bit lengths in ascending order, while retaining the order
        of the original lengths stored in the file}
    var
      XL : Integer;
      XGL : Integer;
      TXP  : PAbSfEntry;
      TXGP : PAbSfEntry;
      X, Gap : Integer;
      Done : Boolean;
      LT : LongInt;
    begin
      Gap := Tree.Entries shr 1;
      repeat
        repeat
          Done := True;
          for X := 0 to (Tree.Entries-1)-Gap do begin
            TXP := @Tree.Entry[X];
            TXGP := @Tree.Entry[X+Gap];
            XL := TXP^.BitLength;
            XGL := TXGP^.BitLength;
            if (XL > XGL) or
               ((XL = XGL) and (TXP^.Value > TXGP^.Value)) then begin
              LT := TXP^.L;
              TXP^.L := TXGP^.L;
              TXGP^.L := LT;
              Done := False;
            end;
          end;
        until Done;

        Gap := Gap shr 1;
      until (Gap = 0);
    end;

    procedure uzReadLengths;
      {-Read bit lengths for a tree}
    var
      TreeBytes : Integer;
      I, J, K : Integer;
      Num, Len : Integer;
      B : Byte;
    begin
      {get number of bytes in compressed tree}
      TreeBytes := uzReadBits(8)+1;

      I := 0;
      Tree.MaxLength := 0;

      {High nibble: Number of values at this bit length + 1.
       Low  nibble: Bits needed to represent value + 1}
      for J := 1 to TreeBytes do begin
        B := uzReadBits(8);
        Len := (B and $0F)+1;
        Num := (B shr 4)+1;

        for K := I to I+Num-1 do
          with Tree, Entry[K] do begin
            if Len > MaxLength then
              MaxLength := Len;
            BitLength := Len;
            Value := K;
          end;
        Inc(I, Num);
      end;
    end;

  begin
    Tree.Entries := TreeSize;
    uzReadLengths;
    SortLengths;
    GenerateTree;
    for I := 0 to TreeSize-1 do
      AbReverseBits(Tree.Entry[I].Code);
  end;

  function uzReadTree(var T) : Byte;
    {-Read next byte using a Shannon-Fano tree}
  var
    Bits : Integer;
    CV   : Word;
    E    : Integer;
    Cur  : Integer;
  var
    Tree : TAbSfTree absolute T;
  begin
    Result := 0;
    Bits := 0;
    CV := 0;
    Cur := 0;
    E := Tree.Entries;
    repeat
      CV := CV or (uzReadBits(1) shl Bits);
      Inc(Bits);
      while Tree.Entry[Cur].BitLength < Bits do begin
        Inc(Cur);
        if Cur >= E then
          Exit;
      end;
      while Tree.Entry[Cur].BitLength = Bits do begin
        if Tree.Entry[Cur].Code = CV then begin
          Result := Tree.Entry[Cur].Value;
          Exit;
        end;
        Inc(Cur);
        if Cur >= E then
          Exit;
      end;
    until False;
  end;

begin
  {do we have an 8K dictionary?}
  if FDictionarySize = ds8K then
    DictBits := 7
  else
    DictBits := 6;

  {allocate trees}
  LengthTree := AllocMem(szLengthTree);
  DistanceTree := AllocMem(szDistanceTree);
  LitTree := nil;
  try
    {do we have a Literal tree?}
    MinMatchLength := FShannonFanoTreeCount;
    if MinMatchLength = 3 then begin
      LitTree := AllocMem(szLitTree);
      uzLoadTree(LitTree^, 256);
    end;

    {load the other two trees}
    uzLoadTree(LengthTree^, 64);
    uzLoadTree(DistanceTree^, 64);

    while (not FInEof) and (FOutSent + LongInt(FOutPos) < FUncompressedSize) do
      {is data literal?}
      if Boolean(uzReadBits(1)) then begin
        {if MinMatchLength = 3 then we have a Literal tree}
        if (MinMatchLength = 3) then
          uzWriteByte( uzReadTree(LitTree^) )
        else
          uzWriteByte( uzReadBits(8) );
      end
      else begin
        {data is a sliding dictionary}
        Distance := uzReadBits(DictBits);

        {using the Distance Shannon-Fano tree, read and decode the
         upper 6 bits of the Distance value}
        Distance := Distance or (uzReadTree(DistanceTree^) shl DictBits);

        {using the Length Shannon-Fano tree, read and decode the Length value}
        Length := uzReadTree(LengthTree^);
        if Length = 63 then
          Inc(Length, uzReadBits(8));
        Inc(Length, MinMatchLength);

        {move backwards Distance+1 bytes in the output stream, and copy
         Length characters from this position to the output stream.
         (if this position is before the start of the output stream,
         then assume that all the data before the start of the output
         stream is filled with zeros)}
        DIndex := (FOutSent + LongInt(FOutPos))-(Distance+1);
        while Length > 0 do begin
          if DIndex < 0 then
            uzWriteByte(0)
          else begin
            uzFlushOutBuf;
            SPos := FOutWriter.Position;
            FOutWriter.Position := DIndex;
            FOutWriter.Read( MyByte, 1 );
            FOutWriter.Position := SPos;
            uzWriteByte(MyByte);
          end;
          Inc(DIndex);
          Dec(Length);
        end;
      end;
  finally
    if (LitTree <> nil) then
      FreeMem(LitTree, szLitTree);
    FreeMem(LengthTree, szLengthTree);
    FreeMem(DistanceTree, szDistanceTree);
  end;
end;
{$ENDIF UnzipImplodeSupport}
{ -------------------------------------------------------------------------- }
{$IFDEF UnzipReduceSupport}
procedure TAbUnzipHelper.uzUnReduce;
const
  FactorMasks : array[1..4] of Byte = ($7F, $3F, $1F, $0F);
  DLE = 144;
var
  C, Last : Byte;
  OpI : LongInt;
  I, J, Sz : Integer;
  D : Word;
  SPos : LongInt;
  MyByte : Byte;
  Factor : Byte;                  {reduction Factor}
  FactorMask : Byte;              {bit mask to use based on Factor}
  Followers : PAbFollowerSets;      {array of follower sets}
  State : Integer;                {used while processing reduced files}
  V : Integer;                    {"}
  Len : Integer;                  {"}

  function BitsNeeded( i : Byte ) : Word;
  begin
    dec( i );
    Result := 0;
    repeat
      inc( Result );
      i := i shr 1;
    until i = 0;
  end;

begin
  GetMem(Followers, SizeOf(TAbFollowerSets));
  try
    Factor := Ord( FCompressionMethod ) - 1;
    FactorMask := FactorMasks[Factor];
    State := 0;
    C := 0;
    V := 0;
    Len := 0;
    D := 0;

    {load follower sets}
    for I := 255 downto 0 do begin
      Sz := uzReadBits(6);
      Followers^[I].Size := Sz;
      Dec(Sz);
      for J := 0 to Sz do
        Followers^[I].FSet[J] := uzReadBits(8);
    end;

    while (not FInEof) and ((FOutSent + LongInt(FOutPos)) < FUncompressedSize) do begin
      Last := C;
      with Followers^[Last] do
        if Size = 0 then
          C := uzReadBits(8)
        else begin
          C := uzReadBits(1);
          if C <> 0 then
            C := uzReadBits(8)
          else
            C := FSet[uzReadBits(BitsNeeded(Size))];
        end;

      if FInEof then
        Exit;

      case State of
        0 :
          if C <> DLE then
            uzWriteByte(C)
          else
            State := 1;
        1 :
          if C <> 0 then begin
            V := C;
            Len := V and FactorMask;
            if Len = FactorMask then
              State := 2
            else
              State := 3;
          end
          else begin
            uzWriteByte(DLE);
            State := 0;
          end;

        2 :
          begin
            Inc(Len, C);
            State := 3;
          end;

        3 :
          begin
            case Factor of
              1 : D := (V shr 7) and $01;
              2 : D := (V shr 6) and $03;
              3 : D := (V shr 5) and $07;
              4 : D := (V shr 4) and $0f;
            else
              raise EAbZipInvalidFactor.Create;
            end;
            {Delphi raises compiler Hints here, saying D might
             be undefined...  If Factor is not in [1..4], the
             exception gets raised, and we never execute the following
             line}
            OpI := (FOutSent + LongInt(FOutPos))-(Swap(D)+C+1);

            for I := 0 to Len+2 do begin
              if OpI < 0 then
                uzWriteByte(0)
              else if OpI >= FOutSent then
                uzWriteByte(FOutBuf[OpI - FOutSent])
              else begin
                SPos := FOutWriter.Position;
                FOutWriter.Position := OpI;
                FOutWriter.Read( MyByte, 1 );
                FOutWriter.Position := SPos;
                uzWriteByte(MyByte);
              end;
              Inc(OpI);
            end;

            State := 0;
          end;
      end;
    end;
  finally
    FreeMem(Followers, SizeOf(Followers^));
  end;
end;
{$ENDIF UnzipReduceSupport}
{ -------------------------------------------------------------------------- }
{$IFDEF UnzipShrinkSupport}
procedure TAbUnzipHelper.uzUnShrink;
  {-Extract a file that was shrunk}
const
  MaxBits = 13;
  InitBits = 9;
  FirstFree = 257;
  Clear = 256;
  MaxCodeMax = 8192; {= 1 shl MaxBits}
  Unused = -1;
var
  CodeSize : SmallInt;
  NextFree : SmallInt;
  BaseChar : SmallInt;
  NewCode : SmallInt;
  OldCode : SmallInt;
  SaveCode : SmallInt;
  N, R : SmallInt;
  I : Integer;
  PrefixTable : PAbIntArray8K;      {used while processing shrunk files}
  SuffixTable : PAbByteArray8K;     {"}
  Stack : PAbByteArray8K;           {"}
  StackIndex : Integer;           {"}
begin
  CodeSize := InitBits;
{  MaxCode := (1 shl InitBits)-1;}
  NextFree := FirstFree;

  PrefixTable := nil;
  SuffixTable := nil;
  Stack := nil;

  try
    GetMem(PrefixTable, SizeOf(PrefixTable^));
    SuffixTable := AllocMem(SizeOf(SuffixTable^));
    GetMem(Stack, SizeOf(Stack^));

    FillChar(PrefixTable^, SizeOf(PrefixTable^), $FF);
    for NewCode := 255 downto 0 do begin
      PrefixTable^[NewCode] := 0;
      SuffixTable^[NewCode] := NewCode;
    end;

    OldCode := uzReadBits(CodeSize);
    if FInEof then
      Exit;
    BaseChar := OldCode;

    uzWriteByte(BaseChar);

    StackIndex := 0;
    while (not FInEof) do begin
      NewCode := uzReadBits(CodeSize);
      while (NewCode = Clear) and (not FInEof) do begin
        case uzReadBits(CodeSize) of
          1 : begin
                Inc(CodeSize);
              end;
          2 : begin
                {mark all nodes as potentially unused}
                for I := FirstFree to pred( NextFree ) do
                  PrefixTable^[I] := PrefixTable^[I] or LongInt($8000);

                {unmark those used by other nodes}
                for N := FirstFree to NextFree-1 do begin
                  {reference to another node?}
                  R := PrefixTable^[N] and $7FFF;
                  {flag node as referenced}
                  if R >= FirstFree then
                    PrefixTable^[R] := PrefixTable^[R] and $7FFF;
                end;

                {clear the ones that are still marked}
                for I := FirstFree to pred( NextFree ) do
                  if PrefixTable^[I] < 0 then
                    PrefixTable^[I] := -1;

                {recalculate NextFree}
                NextFree := FirstFree;
                while (NextFree < MaxCodeMax) and
                      (PrefixTable^[NextFree] <> -1) do
                  Inc(NextFree);
              end;
        end;

        NewCode := uzReadBits(CodeSize);
      end;

      if FInEof then
        Exit;

      {save current code}
      SaveCode := NewCode;

      {special case}
      if PrefixTable^[NewCode] = Unused then begin
        Stack^[StackIndex] := BaseChar;
        Inc(StackIndex);
        NewCode := OldCode;
      end;

      {generate output characters in reverse order}
      while (NewCode >= FirstFree) do begin
        if PrefixTable^[NewCode] = Unused then begin
          Stack^[StackIndex] := BaseChar;
          Inc(StackIndex);
          NewCode := OldCode;
        end else begin
          Stack^[StackIndex] := SuffixTable^[NewCode];
          Inc(StackIndex);
          NewCode := PrefixTable^[NewCode];
        end;
      end;

      BaseChar := SuffixTable^[NewCode];
      uzWriteByte(BaseChar);

      {put them out in forward order}
      while (StackIndex > 0) do begin
        Dec(StackIndex);
        uzWriteByte(Stack^[StackIndex]);
      end;

      {add new entry to tables}
      NewCode := NextFree;
      if NewCode < MaxCodeMax then begin
        PrefixTable^[NewCode] := OldCode;
        SuffixTable^[NewCode] := BaseChar;
        while (NextFree < MaxCodeMax) and
              (PrefixTable^[NextFree] <> Unused) do
          Inc(NextFree);
      end;

      {remember previous code}
      OldCode := SaveCode;
    end;
  finally
    FreeMem(PrefixTable, SizeOf(PrefixTable^));
    FreeMem(SuffixTable, SizeOf(SuffixTable^));
    FreeMem(Stack, SizeOf(Stack^));
  end;
end;
{$ENDIF}
{ -------------------------------------------------------------------------- }
procedure RequestPassword(Archive : TAbZipArchive; var Abort : Boolean);
var
  APassPhrase : AnsiString;
begin
  APassPhrase := Archive.Password;
  Abort := False;
  if Assigned(Archive.OnNeedPassword) then begin
    Archive.OnNeedPassword(Archive, APassPhrase);
    if APassPhrase = '' then
      Abort := True
    else
      Archive.Password := APassPhrase;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure CheckPassword(Archive : TAbZipArchive; var Tries : Integer; var Abort : Boolean);
begin
  { if current password empty }
  if Archive.Password = '' then begin
    { request password }
    RequestPassword(Archive, Abort);
    { increment tries }
    Inc(Tries);
  end;

  { if current password still empty }
  if Archive.Password = '' then begin
    { abort }
    raise EAbZipInvalidPassword.Create;
  end;
end;


{ -------------------------------------------------------------------------- }
procedure DoInflate(Archive : TAbZipArchive; Item : TAbZipItem; InStream, OutStream : TStream);
var
  Hlpr  : TAbDeflateHelper;
begin
  Hlpr := TAbDeflateHelper.Create;
  try
    if Item.CompressionMethod = cmEnhancedDeflated then
      Hlpr.Options := Hlpr.Options or dfc_UseDeflate64;

    Hlpr.StreamSize := Item.CompressedSize;

    Inflate(InStream, OutStream, Hlpr);
  finally
    Hlpr.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure DoLegacyUnzip(Archive : TAbZipArchive; Item : TAbZipItem; InStream, OutStream : TStream);
var
  Helper     : TAbUnzipHelper;
begin
  Helper := TAbUnzipHelper.Create(InStream, OutStream);
  try {Helper}
    Helper.DictionarySize       := Item.DictionarySize;
    Helper.UnCompressedSize     := Item.UncompressedSize;
    Helper.CompressionMethod    := Item.CompressionMethod;
    Helper.ShannonFanoTreeCount := Item.ShannonFanoTreeCount;
    Helper.Execute;
  finally
    Helper.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
{$IFDEF UnzipBzip2Support}
procedure DoExtractBzip2(Archive : TAbZipArchive; Item : TAbZipItem; InStream, OutStream : TStream);
var
  Bzip2Stream: TStream;
begin
  Bzip2Stream := TBZDecompressionStream.Create(InStream);
  try
    OutStream.CopyFrom(Bzip2Stream, Item.UncompressedSize);
  finally
    Bzip2Stream.Free;
  end;
end;
{$ENDIF}
{ -------------------------------------------------------------------------- }
{$IFDEF UnzipLzmaSupport}
procedure DoExtractLzma(Archive : TAbZipArchive; Item : TAbZipItem;
  InStream, OutStream : TStream);
var
  Header: packed record
    MajorVer, MinorVer: Byte;
    PropSize: Word;
  end;
  Properties: array of Byte;
begin
  InStream.ReadBuffer(Header, SizeOf(Header));
  SetLength(Properties, Header.PropSize);
  InStream.ReadBuffer(Properties[0], Header.PropSize);
  LzmaDecodeStream(PByte(Properties), Header.PropSize, InStream, OutStream,
    Item.UncompressedSize);
end;
{$ENDIF}
{ -------------------------------------------------------------------------- }
function ExtractPrep(ZipArchive: TAbZipArchive; Item: TAbZipItem): TStream;
var
  LFH         : TAbZipLocalFileHeader;
  Abort       : Boolean;
  Tries       : Integer;
  CheckValue  : LongInt;
  DecryptStream: TAbDfDecryptStream;
begin
  { validate }
  if (Lo(Item.VersionNeededToExtract) > Ab_ZipVersion) then
    raise EAbZipVersion.Create;

  { seek to compressed file }
  if ZipArchive.FStream is TAbSpanReadStream then
    TAbSpanReadStream(ZipArchive.FStream).SeekImage(Item.DiskNumberStart,
                                                    Item.RelativeOffset)
  else
    ZipArchive.FStream.Position := Item.RelativeOffset;

  { get local header info for Item}
  LFH := TAbZipLocalFileHeader.Create;
  try
    { select appropriate CRC value based on General Purpose Bit Flag }
    { also get whether the file is stored, while we've got the local file header }
    LFH.LoadFromStream(ZipArchive.FStream);
    if (LFH.GeneralPurposeBitFlag and AbHasDataDescriptorFlag = AbHasDataDescriptorFlag) then
    { if bit 3 is set, then the data descriptor record is appended
      to the compressed data }
      CheckValue := LFH.LastModFileTime shl $10
    else
      CheckValue := Item.CRC32;
  finally
    LFH.Free;
  end;

  Result := TAbUnzipSubsetStream.Create(ZipArchive.FStream,
    Item.CompressedSize);

  { get decrypting stream }
  if Item.IsEncrypted then begin
    try
      { need to decrypt }
      Tries := 0;
      Abort := False;
      CheckPassword(ZipArchive, Tries, Abort);
      while True do begin
        if Abort then
          raise EAbUserAbort.Create;
        { check for valid password }
        DecryptStream := TAbDfDecryptStream.Create(Result,
          CheckValue, ZipArchive.Password);
        if DecryptStream.IsValid then begin
          DecryptStream.OwnsStream := True;
          Result := DecryptStream;
          Break;
        end;
        FreeAndNil(DecryptStream);
        { prompt again }
        Inc(Tries);
        if (Tries > ZipArchive.PasswordRetries) then
          raise EAbZipInvalidPassword.Create;
        RequestPassword(ZipArchive, Abort);
      end;
    except
      Result.Free;
      raise;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure DoExtract(aZipArchive: TAbZipArchive; aItem: TAbZipItem;
  aInStream, aOutStream: TStream);
var
  OutStream : TAbUnzipOutputStream;
begin
  if aItem.UncompressedSize = 0 then
    Exit;

  OutStream := TAbUnzipOutputStream.Create(aOutStream);
  try
    OutStream.UncompressedSize := aItem.UncompressedSize;
    OutStream.OnProgress := aZipArchive.OnProgress;

    { determine storage type }
    case aItem.CompressionMethod of
      cmStored: begin
        { unstore aItem }
        OutStream.CopyFrom(aInStream, aItem.UncompressedSize);
      end;
      cmDeflated, cmEnhancedDeflated: begin
        { inflate aItem }
        DoInflate(aZipArchive, aItem, aInStream, OutStream);
      end;
      {$IFDEF UnzipBzip2Support}
      cmBzip2: begin
        DoExtractBzip2(aZipArchive, aItem, aInStream, OutStream);
      end;
      {$ENDIF}
      {$IFDEF UnzipLzmaSupport}
      cmLZMA: begin
        DoExtractLzma(aZipArchive, aItem, aInStream, OutStream);
      end;
      {$ENDIF}
      {$IFDEF UnzipPPMdSupport}
      cmPPMd: begin
        DecompressPPMd(aInStream, OutStream);
      end;
      {$ENDIF}
      {$IFDEF UnzipWavPackSupport}
      cmWavPack: begin
        DecompressWavPack(aInStream, OutStream);
      end;
      {$ENDIF}
      cmShrunk..cmImploded: begin
        DoLegacyUnzip(aZipArchive, aItem, aInStream, OutStream);
      end;
      else
        raise EAbZipInvalidMethod.Create;
    end;

    { check CRC }
    if OutStream.CRC32 <> aItem.CRC32 then
      if Assigned(aZipArchive.OnProcessItemFailure) then
        aZipArchive.OnProcessItemFailure(aZipArchive, aItem, ptExtract,
          ecAbbrevia, AbZipBadCRC)
      else
        raise EAbZipBadCRC.Create;
  finally
    OutStream.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure AbUnzipToStream( Sender : TObject; Item : TAbZipItem; OutStream : TStream);
var
  ZipArchive : TAbZipArchive;
  InStream : TStream;
begin
  ZipArchive := Sender as TAbZipArchive;
  if not Assigned(OutStream) then
    raise EAbBadStream.Create;

  InStream := ExtractPrep(ZipArchive, Item);
  try
    DoExtract(ZipArchive, Item, InStream, OutStream);
  finally
    InStream.Free
  end;
end;
{ -------------------------------------------------------------------------- }
procedure AbUnzip(Sender : TObject; Item : TAbZipItem; const UseName : string);
  {create the output filestream and pass it to DoExtract}
var
  InStream, OutStream : TStream;
  ZipArchive : TAbZipArchive;
begin
  ZipArchive := TAbZipArchive(Sender);

  if Item.IsDirectory then
    AbCreateDirectory(UseName)
  else begin
    InStream := ExtractPrep(ZipArchive, Item);
    try
      OutStream := TFileStreamEx.Create(UseName, fmCreate or fmShareDenyWrite);
      try
        try    {OutStream}
          DoExtract(ZipArchive, Item, InStream, OutStream);
        finally {OutStream}
          OutStream.Free;
        end;   {OutStream}
      except
        if ExceptObject is EAbUserAbort then
          ZipArchive.FStatus := asInvalid;
        DeleteFile(UseName);
        raise;
      end;
    finally
      InStream.Free
    end;
  end;

  AbSetFileTime(UseName, Item.LastModTimeAsDateTime);
  AbSetFileAttr(UseName, Item.NativeFileAttributes);
end;
{ -------------------------------------------------------------------------- }
procedure AbTestZipItem(Sender : TObject; Item : TAbZipItem);
  {extract item to bit bucket and verify its local file header}
var
  BitBucket  : TAbBitBucketStream;
  FieldSize  : Word;
  LFH        : TAbZipLocalFileHeader;
  Zip64Field : PZip64LocalHeaderRec;
  ZipArchive : TAbZipArchive;
begin
  ZipArchive := TAbZipArchive(Sender);

  if (Lo(Item.VersionNeededToExtract) > Ab_ZipVersion) then
    raise EAbZipVersion.Create;

  { seek to compressed file }
  if ZipArchive.FStream is TAbSpanReadStream then
    TAbSpanReadStream(ZipArchive.FStream).SeekImage(Item.DiskNumberStart,
                                                    Item.RelativeOffset)
  else
    ZipArchive.FStream.Position := Item.RelativeOffset;

  BitBucket := nil;
  LFH := nil;
  try
    BitBucket := TAbBitBucketStream.Create(0);
    LFH := TAbZipLocalFileHeader.Create;
      {get the item's local file header}
    ZipArchive.FStream.Seek(Item.RelativeOffset, soBeginning);
    LFH.LoadFromStream(ZipArchive.FStream);
    ZipArchive.FStream.Seek(Item.RelativeOffset, soBeginning);

      {currently a single exception is raised for any LFH error}
    if (LFH.VersionNeededToExtract <> Item.VersionNeededToExtract) then
      raise EAbZipInvalidLFH.Create;
    if (LFH.GeneralPurposeBitFlag <> Item.GeneralPurposeBitFlag) then
      raise EAbZipInvalidLFH.Create;
    if (LFH.LastModFileTime <> Item.LastModFileTime) then
      raise EAbZipInvalidLFH.Create;
    if (LFH.LastModFileDate <> Item.LastModFileDate) then
      raise EAbZipInvalidLFH.Create;
    if (LFH.CRC32 <> Item.CRC32) then
      raise EAbZipInvalidLFH.Create;
    if LFH.ExtraField.Get(Ab_Zip64SubfieldID, Pointer(Zip64Field), FieldSize) then begin
      if (Zip64Field.CompressedSize <> Item.CompressedSize) then
        raise EAbZipInvalidLFH.Create;
      if (Zip64Field.UncompressedSize <> Item.UncompressedSize) then
        raise EAbZipInvalidLFH.Create;
    end
    else begin
      if (LFH.CompressedSize <> Item.CompressedSize) then
        raise EAbZipInvalidLFH.Create;
      if (LFH.UncompressedSize <> Item.UncompressedSize) then
        raise EAbZipInvalidLFH.Create;
    end;
    if (LFH.FileName <> Item.RawFileName) then
      raise EAbZipInvalidLFH.Create;

      {any CRC errors will raise exception during extraction}
    AbUnZipToStream(Sender, Item, BitBucket);
  finally
    BitBucket.Free;
    LFH.Free;
  end;

end;
{ -------------------------------------------------------------------------- }
procedure InflateStream( CompressedStream, UnCompressedStream : TStream );
  {-Inflates everything in CompressedStream to UncompressedStream
    no encryption is tried, no check on CRC is done, uses the whole
    compressedstream - no Progress events - no Frills!}
begin
  Inflate(CompressedStream, UncompressedStream, nil);
end;

end.

