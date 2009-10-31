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
{* ABBREVIA: AbUnzPrc.pas 3.05                           *}
{*********************************************************}
{* ABBREVIA: UnZip procedures                            *}
{*********************************************************}

{$I AbDefine.inc}

unit AbUnzPrc;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  AbArcTyp,
  AbZipTyp,
  AbUtils,
  AbDfBase,
  AbDfDec,
  AbDfCryS,
  AbZipCry,
  AbSWStm,
  AbSpanSt,
  Classes;

type
  TAbUnzipHelper = class( TObject )
  protected {private}
    {internal variables}
    FOutWriter : TStream;
    FOutStream : TStream;
    FUnCompressedSize : LongInt;
    FCRC32 : LongInt;
    FCompressedSize : LongInt;
    FCompressionMethod : TAbZipCompressionMethod;
    FDecoder : TAbZDecoder;
    FDictionarySize : TAbZipDictionarySize;
    FShannonFanoTreeCount : Byte;
    FOnProgress : TAbProgressEvent;
    FOnRequestNthDisk : TAbRequestNthDiskEvent;
    FCurrentProgress : Byte;
    {spanning variables}
    FSpanned : Boolean;
    FArchiveName : string;
    FArchive : TAbArchive;
    FCurrentDisk : Word;
    FMode : Word;

    FOutBuf : PAbByteArray;          {output buffer}
    FOutSent : LongInt;              {number of bytes sent to output buffer}
    FOutPos : Cardinal;              {current position in output buffer}
    FBitSValid : Byte;               {Number of valid bits}

    FInBuf : TAbByteArray4K;
    FInPos : Integer;                {current position in input buffer}
    FInCnt : Integer;                {number of bytes in input buffer}
    FInLeft : LongInt;               {bytes remaining in compressed input file}
    FInEof  : Boolean;               {set when FInLeft = 0}
    FCurByte : Byte;                 {current input byte}
    FBitsLeft : Byte;                {bits left to process in FCurByte}
    FdBitStrBuf : Word;              {Bit string output buffer}
    {cannot change FdBitStrBuf to a Cardinal}
    FiOverflowBuf : Cardinal;        {Bit overflow holding buffer}


    FiSlide : PAbiSlide;             {Sliding window buffer}
    FiSlidePos : Cardinal;               {Current position in Slide}


    FZStream : TStream;
  protected
    procedure DoProgress( Progress : Byte; var Abort : Boolean );
      virtual;
    procedure DoRequestNthDisk( DiskNumber : Byte; var Abort : Boolean );
      virtual;
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
    constructor Create( var InputStream : TStream; OutputStream : TStream;
                        aDecoder : TObject );
    destructor Destroy;
      override;

    property Archive : TAbArchive
      read FArchive write FArchive;
    property ArchiveName : string
      read FArchiveName
      write FArchiveName;
    property CurrentDisk : Word
      read FCurrentDisk
      write FCurrentDisk;
    function Execute : LongInt;
      {returns the CRC}
    property Mode : Word
      read FMode
      write FMode;
    property OnProgress : TAbProgressEvent
      read FOnProgress
      write FOnProgress;
    property OnRequestNthDisk : TAbRequestNthDiskEvent
      read FOnRequestNthDisk
      write FOnRequestNthDisk;
    property Spanned : Boolean
      read FSpanned
      write FSpanned;
    property CompressedSize : LongInt
      read FCompressedSize
      write FCompressedSize;
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

  procedure AbUnzip(Sender : TObject; Item : TAbZipItem; NewName : string);

  procedure AbTestZipItem(Sender : TObject; Item : TAbZipItem);

  procedure InflateStream(CompressedStream, UnCompressedStream : TStream);
  {-Inflates everything in CompressedStream to UncompressedStream
    no encryption is tried, no check on CRC is done, uses the whole
    compressedstream - no Progress events - no Frills!}

  function CopyFileTo(const Source, Destination: string;FailifExists:boolean): Boolean;
implementation

uses
  AbConst,
  AbExcept,
  AbTempFileStream,  
  AbBitBkt,
  {$IFNDEF NoQt}
  {$IFDEF LINUX}
  QControls,
  QDialogs,
  {$ENDIF}
  {$ENDIF}
  SysUtils;

{ -------------------------------------------------------------------------- }
procedure AbReverseBits(var W : Word);{$IFDEF UseGreedyAsm}assembler;{$ENDIF}
  {-Reverse the order of the bits in W}
register;
var
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
{$IFDEF UseGreedyAsm}
asm
  push eax                 // save EAX
  mov  eax, [eax]          // read value into EAX
  xor  ecx, ecx            // zero ECX
  mov  cl, al              // prepare for table lookup
  lea  edx, RevTable       // get address to table
  mov  al, [edx+ecx]       // table lookup for low byte
  mov  cl, ah              // prepare high byte for table lookup
  mov  ah, al              // reverse bytes
  mov  al, [edx+ecx]       // table lookup for high (now low) byte
  pop  edx                 // restore address to W
  mov  [edx], eax          // move value to W
end;
{$ENDIF}

{$IFDEF UseGreedyPascal}
var
  X: Word;
begin
  X:= W;
  WordRec(W).Lo:= RevTable[WordRec(X).Hi];
  WordRec(W).Hi:= RevTable[WordRec(X).Lo];
end;
{$ENDIF}

{ TAbUnzipHelper implementation ============================================ }

{ -------------------------------------------------------------------------- }
constructor TAbUnzipHelper.Create( var InputStream : TStream;
                                   OutputStream : TStream;
                                   aDecoder : TObject );
begin
  inherited Create;
  FOutBuf := AllocMem( AbBufferSize );                                 
  FOutPos := 0;                                                        
  FZStream := InputStream;
  FOutStream := OutputStream;
  FDecoder := TAbZDecoder( aDecoder );
  FCompressedSize := FZStream.Size;
  FUncompressedSize := 0;
  FDictionarySize := dsInvalid;
  FShannonFanoTreeCount := 0;
  FCompressionMethod := cmDeflated;
  {starting value for output CRC}
  FCRC32 := -1;
  FCurrentProgress := 0;
  FSpanned := False;
  FMode := 0;
  FArchiveName := '';
  FCurrentDisk := 0;
end;
{ -------------------------------------------------------------------------- }
destructor TAbUnzipHelper.Destroy;
begin
  FreeMem( FOutBuf, AbBufferSize );                                    
  inherited Destroy;
end;
{ -------------------------------------------------------------------------- }
function TAbUnzipHelper.Execute : LongInt;
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
  FInLeft := FCompressedSize;
  FInPos := 1+SizeOf(FInBuf);

{  GetMem( FInBuf, SizeOf(FInBuf^) );}
  try
    if Assigned( FDecoder ) then begin
      FDecoder.ReadEncryptionHeader;
      Dec( FInLeft, 12 );
    end;

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
  Result := not FCRC32;
end;
{ -------------------------------------------------------------------------- }
procedure TAbUnzipHelper.uzReadNextPrim;
var
  L : LongInt;
  NeedDisk : Boolean;
  Abort : Boolean;
begin
  if (FInLeft = 0) then begin
    {we're done}
    FInEof := True;
    FInPos := FInCnt+1;
  end
  else begin
      {spanning stuff}
    if Spanned and (FZStream.Size = FZStream.Position) then begin
      {need the next disk!}
      CurrentDisk := CurrentDisk + 1;
      if not (FZStream is TFileStream) then                           
        raise EAbZipBadSpanStream.Create;
      TAbZipArchive(FArchive).DoRequestNthImage(CurrentDisk, FZStream, Abort );
    end;
    NeedDisk := False;
    if FInLeft > sizeof( FInBuf ) then
      L := sizeOf( FInBuf )
    else
      L := FInLeft;
    if L >= ( FZStream.Size - FZStream.Position ) then begin
      NeedDisk := True;
      L := FZStream.Size - FZStream.Position;
    end;
    FInCnt := FZStream.Read( FInBuf, L );
    if (FInCnt = 0) then                                               
      raise EAbReadError.Create;
    if FDecoder <> nil then
      FDecoder.DecodeBuffer( FInBuf[1], FInCnt );                      

    {decrement count of bytes left to go}
    Dec(FInLeft, FInCnt);
    FInEof :=  ( not NeedDisk ) and ( FZStream.Position = FZStream.Size );

    {load first byte in buffer and set position counter}
    FCurByte := FInBuf[1];
    FInPos := 2;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbUnzipHelper.uzFlushOutBuf;
  {-flushes the output buffer}
var
  tempCRC : LongInt;
  Abort : Boolean;
  NewProgress : byte;
begin
  if (FOutPos <> 0) then begin
    FOutWriter.Write( FOutBuf^, FOutPos );
    inc( FOutSent, FOutPos );

    tempCRC := FCRC32;
    AbUpdateCRC( tempCRC, FOutBuf^, FOutPos );
    FOutPos := 0;
    FCRC32 := tempCRC;
  end;
  Abort := False;
  NewProgress := AbPercentage(FOutSent, FUncompressedSize);
  if (NewProgress <> FCurrentProgress) then
    DoProgress(NewProgress, Abort);
  if Abort then
    raise EAbUserAbort.Create;
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
procedure TAbUnzipHelper.DoRequestNthDisk( DiskNumber : Byte;
                                           var Abort : Boolean );
var
  pMessage : string;
  pCaption : string;
begin
  if Assigned( FOnRequestNthDisk ) then
    FOnRequestNthDisk( Self, DiskNumber, Abort )
  else begin
    pMessage:= Format(AbStrRes(AbDiskNumRequest), [DiskNumber]);
    pCaption := AbStrRes(AbDiskRequest);
    {$IFDEF MSWINDOWS}
    Abort := Windows.MessageBox( 0, PChar(pMessage), PChar(pCaption),
      MB_TASKMODAL or MB_OKCANCEL ) = IDCANCEL;
    {$ENDIF}
    {$IFDEF LINUX}
    {$IFDEF NoQt}
    WriteLn(pMessage);
    {$ELSE }
    Abort := QDialogs.MessageDlg(pCaption, pMessage, mtWarning, mbOKCancel, 0) = mrCancel;
    {$ENDIF}
    {$ENDIF}
  end;
end;
{ -------------------------------------------------------------------------- }
procedure TAbUnzipHelper.DoProgress(Progress : Byte; var Abort : Boolean);
begin
  if (not Assigned(FOnProgress)) or (Progress = FCurrentProgress) then
    Exit
  else
    FCurrentProgress := Progress;
  FOnProgress(Progress, Abort);
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
  const
    Bits : Integer = 0;
    CV   : Word = 0;
    E    : Integer = 0;
    Cur  : Integer = 0;
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
  OpO : LongInt;
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

            if OpI >= 8192 then
              OpO := OpI mod 8192
            else
              OpO := OpI;
            for I := 0 to Len+2 do begin
              if OpI < 0 then
                uzWriteByte(0)
              else begin
                SPos := FOutWriter.Position;
                FOutWriter.Position := OpO;
                FOutWriter.Read( MyByte, 1 );
                FOutWriter.Position := SPos;
                uzWriteByte(MyByte);
              end;
              Inc(OpI);
              Inc(OpO);
              if OpO >= 8192 then
                OpO := 0;
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
  APassPhrase : string;
begin
  APassPhrase := Archive.Password; {!!.05  SF.NET Bug 698162}
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
function DoInflate(Archive : TAbZipArchive; Item : TAbZipItem; OutStream : TStream; TheCRC : LongInt) : LongInt;
var
  Hlpr  : TAbDeflateHelper;
  Tries : Integer;
  Successful : Boolean;
  Abort : Boolean;
begin
  Hlpr := TAbDeflateHelper.Create;

  if (OutStream is TAbBitBucketStream) then  { we're just validating the item }
    Hlpr.Options := Hlpr.Options or dfc_TestOnly;

  if Item.CompressionMethod = cmEnhancedDeflated then
    Hlpr.Options := Hlpr.Options or dfc_UseDeflate64;

  Hlpr.StreamSize := Item.CompressedSize;                                {!!.02}
  Hlpr.OnProgressStep := Archive.DoInflateProgress;

  Result := 0;

  try
    if not (Item.IsEncrypted) then begin
      { just inflate it }
      Result := Inflate(Archive.FStream, OutStream, Hlpr);
    end
    else begin { it's encrypted }
      Tries := 0;
      Successful := False;
      Abort := False;
      CheckPassword(Archive, Tries, Abort);
      if Abort then
        raise EAbUserAbort.Create;


      Hlpr.CheckValue := TheCRC;
      repeat
        try
          { attempt to inflate }
          Hlpr.Passphrase := Archive.Password;
          Result := Inflate(Archive.FStream, OutStream, Hlpr);
          Successful := True;
        except
          on E:EAbInflatePasswordError do begin { bad password? }
            { request password }
            RequestPassword(Archive, Abort);
            if Abort then
              raise EAbUserAbort.Create;
            { increment tries }
            Inc(Tries);
          end;
        end;
        if (Tries > Archive.PasswordRetries) then begin
          raise EAbZipInvalidPassword.Create;
        end;
      until Successful or Abort or (Tries >= Archive.PasswordRetries);

    end; { if encrypted }

  finally
    Hlpr.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
function DoExtractStored(Archive : TAbZipArchive; Item : TAbZipItem; OutStream : TStream; TheCRC : LongInt) : LongInt;
var
  DataRead    : Int64;
  CRC32       : LongInt;
  Percent     : LongInt;
  LastPercent : LongInt;

  Tries : Integer;
  Total       : Int64;
  Remaining   : Int64;
  SizeToRead  : Int64;
  Abort       : Boolean;
  Buffer      : array [0..1023] of byte;
  DecryptStream : TAbDfDecryptStream;
begin
  { setup }
  Total   := 0;
  Remaining := Item.UncompressedSize;
  Abort   := False;
  CRC32   := -1;
  Percent := 0;
  LastPercent := 0;

  SizeToRead := SizeOf(Buffer);
  if SizeToRead > Remaining then
    SizeToRead := Remaining;


  { if Item is not encrypted }
  if not Item.IsEncrypted then begin
    try

      { just extract it }

      { get first bufferful }
      DataRead := Archive.FStream.Read(Buffer, SizeToRead);
      { while more data has been read and we're not told to bail }
      while (DataRead > 0) and (Total < Item.UncompressedSize) and not Abort do begin
        { report progress }
        if Assigned(Archive.OnProgress) then begin
          Total := Total + DataRead;
          Remaining := Remaining - DataRead;
          Percent := Round((100.0 * Total) / Item.UncompressedSize);
          if (LastPercent <> Percent) then
            Archive.OnProgress(Percent, Abort);
          LastPercent := Percent;
        end;

        { update CRC }
        AbUpdateCRCBuffer(CRC32, Buffer, DataRead);

        { write data }
        OutStream.WriteBuffer(Buffer, DataRead);

        { get next bufferful }
        SizeToRead := SizeOf(Buffer);
        if SizeToRead > Remaining then
          SizeToRead := Remaining;
        DataRead := Archive.FStream.Read(Buffer, SizeToRead);
      end;
    except
      on EAbUserAbort do
        Abort := True;
    end;

  end
  else begin
    try
      { need to decrypt }
      Tries := 0;
      Abort := False;

      CheckPassword(Archive, Tries, Abort);
      if Abort then
        raise EAbUserAbort.Create;

      { check for valid password }
      DecryptStream := TAbDfDecryptStream.Create(Archive.FStream, TheCRC, Archive.Password);
      try 
          while not Abort and not DecryptStream.IsValid and (Tries < Archive.PasswordRetries) do begin
            RequestPassword(Archive, Abort);
            if Abort then
              raise EAbUserAbort.Create;
            DecryptStream.Free;
            DecryptStream := TAbDfDecryptStream.Create(Archive.FStream, TheCRC, Archive.Password);
            Inc(Tries);
          end;
          if (Tries > Archive.PasswordRetries) then
            raise EAbZipInvalidPassword.Create;

          { got good Password, so extract }
          { get first bufferful (decrypting) }
          {  DecryptStream.Position := 0;                            }{!!.01}{!!.02}
          DataRead := DecryptStream.Read(Buffer, SizeToRead);
          { while more data has been read and we're not told to bail }
          while (DataRead > 0) and not Abort do begin
            { report progress }
            if Assigned(Archive.OnProgress) then begin
              Total := Total + DataRead;
              Remaining := Remaining - DataRead;                             {!!.01}
              Percent := Round((100.0 * Total) / Item.UncompressedSize);
              if (LastPercent <> Percent) then
                Archive.OnProgress(Percent, Abort);
              LastPercent := Percent;
            end;

            { update CRC }
            AbUpdateCRCBuffer(CRC32, Buffer, DataRead);

            { write data }
            OutStream.WriteBuffer(Buffer, DataRead);

            { get next bufferful (decrypting) }
            SizeToRead := SizeOf(Buffer);
            if SizeToRead > Remaining then
              SizeToRead := Remaining;
            DataRead := DecryptStream.Read(Buffer, SizeToRead);
          end;
      finally
      	DecryptStream.Free();

      end;
    except
      on EAbUserAbort do
        Abort := True;
    end;
  end;


  { finish CRC calculation }
  Result := not CRC32;

  { show final progress increment }
  if (Percent < 100) and Assigned(Archive.OnProgress) then
    Archive.OnProgress(100, Abort);

  { User wants to bail }
  if Abort then begin
    raise EAbUserAbort.Create;
  end;
end;
{ -------------------------------------------------------------------------- }
function DoOtherUnzip(Archive : TAbZipArchive; Item : TAbZipItem; OutStream : TStream; TheCRC : LongInt) : LongInt;
var
  Decoder    : TAbZDecoder;
  Helper     : TAbUnzipHelper;

begin
  Decoder := nil;
  if Item.IsEncrypted then
    Decoder := TAbZDecoder.Create(Archive.Password, Archive.FStream, TheCRC, True);

  if Assigned(Decoder) then begin
    Decoder.OnNeedPassword := Archive.OnNeedPassword;
    Decoder.Password := Archive.Password;
    Decoder.Retries := Archive.PasswordRetries;
  end;

  Helper := TAbUnzipHelper.Create(Archive.FStream, OutStream, Decoder);
  try {Helper}
    Helper.DictionarySize       := Item.DictionarySize;
    Helper.CompressedSize       := Item.CompressedSize;
    Helper.UnCompressedSize     := Item.UncompressedSize;
    Helper.CompressionMethod    := Item.CompressionMethod;
    Helper.ShannonFanoTreeCount := Item.ShannonFanoTreeCount;
    Helper.OnProgress           := Archive.OnProgress;
    Helper.OnRequestNthDisk     := Archive.OnRequestNthDisk;
    Helper.Archive              := Archive;
    if Archive.Spanned then
      with Archive do begin
        Helper.ArchiveName := ArchiveName;
        Helper.CurrentDisk := CurrentDisk;
        Helper.Spanned := True;
        Helper.Mode := Mode;
      end;
    Result := Helper.Execute;
  finally
    Helper.Free;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure AbUnzipToStream( Sender : TObject; Item : TAbZipItem; OutStream : TStream);
var
  LFH         : TAbZipLocalFileHeader;
  TheCRC      : LongInt;
  OutCRC      : LongInt;
  Abort       : Boolean;
  ZipArchive  : TAbZipArchive;

  procedure Validate;
  begin
    if not Assigned(OutStream) then
      raise EAbBadStream.Create;
    if (Lo(Item.VersionNeededToExtract) > Ab_ZipVersion) then
      raise EAbZipVersion.Create;
  end;

  procedure CheckForSpanning;
  begin
    if ZipArchive.Spanned then begin                                 {!!.02}
      if (ZipArchive.CurrentDisk <> Item.DiskNumberStart) then begin {!!.02}
        ZipArchive.CurrentDisk := Item.DiskNumberStart;
        if not (ZipArchive.FStream is TAbSpanStream) then
          raise EAbZipBadSpanStream.Create;
        if not TAbSpanStream(ZipArchive.FStream).SpanStreamInCharge then begin {!!.02}
          ZipArchive.DoRequestNthImage(ZipArchive.CurrentDisk,
            ZipArchive.FStream, Abort );
          if Abort then
            raise EAbUserAbort.Create;
        end;                                                         {!!.02}
      end;
    end;                                                             {!!.02}
  end;

  procedure GetHeader;
  begin
    {get past the item's local file header}
    ZipArchive.FStream.Seek(Item.RelativeOffset, soBeginning);

    { select appropriate CRC value based on General Purpose Bit Flag }
    { also get whether the file is stored, while we've got the local file header }
    LFH.LoadFromStream(ZipArchive.FStream);
    if (LFH.GeneralPurposeBitFlag and AbHasDataDescriptorFlag = AbHasDataDescriptorFlag) then
    { if bit 3 is set, then the data descriptor record is appended
      to the compressed data }
      TheCRC := LFH.LastModFileTime shl $10
    else
      TheCRC := Item.CRC32;
  end;
begin
  ZipArchive := Sender as TAbZipArchive;
  LFH := TAbZipLocalFileHeader.Create;

  try     {LFH}
    Validate;
    CheckForSpanning;

    { get local header info for Item}
    GetHeader;

    { determine storage type }
    case LFH.CompressionMethod of
     cmStored: begin
      { unstore item }
      OutCRC := DoExtractStored(ZipArchive, Item, OutStream, TheCRC);
     end;
     cmDeflated, cmEnhancedDeflated: begin
      { inflate Item }
      OutCRC := DoInflate(ZipArchive, Item, OutStream, TheCRC);
     end;
     else begin { Shrunk, Imploded, or Reduced }
       OutCrc := DoOtherUnzip(ZipArchive, Item, OutStream, TheCrc);
     end;
    end;

    { check CRC }
    if (OutCRC <> TheCRC) and (OutCRC <> Item.CRC32) then                {!!.01}
      if Assigned(ZipArchive.OnProcessItemFailure) then                  {!!.01}
        ZipArchive.OnProcessItemFailure(ZipArchive, Item, ptExtract,     {!!.01}
          ecAbbrevia, AbZipBadCRC)                                       {!!.01}
      else                                                               {!!.01}
        raise EAbZipBadCRC.Create;                                       {!!.01}

{!!.01 -- removed}
    { set spanning info }
//    if ZipArchive.Spanned then
//      ZipArchive.CurrentDisk := TAbSpanStream(ZipArchive.FStream).SpanNumber;
{!!.01 -- end removed}

  finally {LFH}
    LFH.Free;
  end;    {LFH}
end;
{ -------------------------------------------------------------------------- }
procedure AbUnzip(Sender : TObject; Item : TAbZipItem; NewName : string);
  {create the output filestream and pass it to AbUnzipToStream}
var
  Confirm    : Boolean;
  {$IFDEF AbUnZipClobber}
  OutStream  : TFileStream;
  {$ENDIF}
  MemoryStream   : TMemoryStream;
  OutStream      : TFileStream;
  TempFileStream : TAbTempFileStream;
  TempFileName   : String;
(*  {$IFDEF AbUnZipMemory}
  TempOut    : TMemoryStream;
  OutStream  : TFileStream;
  {$ENDIF}
  {$IFDEF AbUnZipTempFile}
  TempOut    : TAbTempFileStream;
  TempFile   : String;
  {$ENDIF}*)
  ZipArchive : TAbZipArchive;
begin
  ZipArchive := TAbZipArchive(Sender);

  {BaseDirectory is the drive:\directory\sub where we currently want files}
  {NewName is the optionalpath\sub\filename.ext where we want the file}
  Confirm := AbConfirmPath(ZipArchive.BaseDirectory, NewName,
    ZipArchive.ExtractOptions, ZipArchive.OnConfirmOverwrite);

  if not Confirm then
    Exit;
  // The problem with Create the FileStream here is that overwrites the existing
  // File which is no problem, unless the archive is password protected and
  // then Password is found out to be wrong.   Ways to resolve:
  // 1. extract to TMemoryStream then Write MemoryStream to Disk if everything is ok.
  // 2. Move the password check up in the process (if possible)
  // 3. Write to a Temp File if everything ok copy to new location.
  // Given all the options that I have now, I am going to try option #1 for now,
  // it may cause problems on memory overhead for some, but lets see if it does.
  // It also may speed up this routine for many.
  // NOTE: Instead of doing what I stated above, I added compiler define logic to allow you to chose for now.
  try
  {$IFDEF AbUnZipClobber}
    OutStream := TFileStream.Create(NewName, fmCreate or fmShareDenyWrite); {!!.01}
    try    {OutStream}
      AbUnZipToStream(Sender, Item, OutStream);
    // Some Network Operating Systems cache the file and when we set attributes they are truncated
      AbFlushOsBuffers(OutStream.Handle);
    finally {OutStream}
      OutStream.Free;
    end;   {OutStream}
  {$ENDIF}

    // Unpack to memory for files less than 1MB
    if Item.UncompressedSize < 1024 * 1024 then
    begin
  //  {$IFDEF AbUnZipMemory}
      MemoryStream := TMemoryStream.Create;
      try
        MemoryStream.Size := Item.UncompressedSize;// This causes all the memory to allocated at once which is faster
        MemoryStream.Position := 0;
        AbUnZipToStream(Sender, Item, MemoryStream);
        OutStream := TFileStream.Create(NewName, fmCreate or fmShareDenyWrite); {!!.01}
        try
          // Copy Memory Stream To File Stream.
          MemoryStream.SaveToStream(OutStream);
          // Some Operating Systems cache the file and when we set attributes they are truncated
          AbFlushOsBuffers(OutStream.Handle);
        finally
         OutStream.Free;
        end;
      finally
        MemoryStream.Free;
      end;
  //  {$ENDIF}

    end else begin  // for larger files copy through temp file

  //  {$IFDEF AbUnZipTempFile}
      TempFileStream := TAbTempFileStream.Create(false);
      try
        AbUnZipToStream(Sender, Item, TempFileStream);
        TempFileName := TempFileStream.FileName;
      finally
        TempFileStream.Free;
      end;

      // Now copy the temp File to correct location
      CopyFileTo(pchar(TempFileName),pchar(NewName),false);

      // Check that it exists
      if Not FileExists(NewName) then
      begin
        raise EAbException.CreateFmt(abMoveFileErrorS,[TempFileName,NewName]); // TODO: Add Own Exception Class
      end;
      // Now Delete the Temp File
      DeleteFile(TempFileName);
    end;
  // {$ENDIF}


    // [ 880505 ]  Need to Set Attributes after File is closed {!!.05}
    {!!.05 Moved to after OutStream.Free to make sure File Handle is closed}

    AbSetFileTime(NewName, Item.SystemSpecificLastModFileTime);
    AbFileSetAttr(NewName, Item.SystemSpecificAttributes);

  except
    on E : EAbUserAbort do begin
      ZipArchive.FStatus := asInvalid;
      if FileExists(NewName) then
        DeleteFile(NewName);
      raise;
    end else begin
      if FileExists(NewName) then
        DeleteFile(NewName);
      raise;
    end;
  end;
end;
{ -------------------------------------------------------------------------- }
procedure AbTestZipItem(Sender : TObject; Item : TAbZipItem);
  {extract item to bit bucket and verify its local file header}
var
  BitBucket  : TAbBitBucketStream;
  LFH        : TAbZipLocalFileHeader;
  ZipArchive : TAbZipArchive;
  Abort : Boolean;
begin
  ZipArchive := TAbZipArchive(Sender);

  if (Lo(Item.VersionNeededToExtract) > Ab_ZipVersion) then
    raise EAbZipVersion.Create;

  if ZipArchive.Spanned and (ZipArchive.CurrentDisk <> Item.DiskNumberStart) then
  begin
    ZipArchive.CurrentDisk := Item.DiskNumberStart;
    if not (ZipArchive.FStream is TAbSpanStream) then                  {!!.04}
      raise EAbZipBadSpanStream.Create;
    ZipArchive.DoRequestNthImage(ZipArchive.CurrentDisk,
      ZipArchive.FStream, Abort );
    if Abort then
      raise EAbUserAbort.Create;
  end;

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
    if (LFH.CompressedSize <> Item.CompressedSize) then
      raise EAbZipInvalidLFH.Create;
    if (LFH.UncompressedSize <> Item.UncompressedSize) then
      raise EAbZipInvalidLFH.Create;
    if (CompareStr(StrPas(LFH.FileName), Item.FileName) <> 0) then
      raise EAbZipInvalidLFH.Create;
    if (CompareStr(StrPas(LFH.ExtraField), Item.ExtraField) <> 0) then
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

function CopyFileTo(const Source, Destination: string;failifExists:boolean): Boolean;
{$IFDEF LINUX}
var
SourceStream: TFileStream;
{$ENDIF}
begin
// -TODO: Change to use a Linux copy function
// There is no native Linux copy function (at least "cp" doesn't use one
// and I can't find one anywhere (Johannes Berg))
{$IFDEF LINUX}
   Result := false;
   if not FileExists(Destination) then
   begin
      SourceStream := TFileStream.Create(Source, fmOpenRead);
      try
         with TFileStream.Create(Destination, fmCreate) do
         try
            CopyFrom(SourceStream, 0);    // copy whole stream
         finally
            Free;
         end;
      finally
         SourceStream.free;
      end;
      Result := true;
   end;
{$ENDIF LINUX}
{$IFDEF MSWINDOWS}
   Result := CopyFile(PChar(Source), PChar(Destination), failifExists);
{$ENDIF MSWINDOWS}
end;
end.

