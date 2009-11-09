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
{* ABBREVIA: AbDfStrm.pas 3.05                           *}
{*********************************************************}
{* Deflate streams unit for various streams              *}
{*********************************************************}

unit AbDfStrm;

{$I AbDefine.inc}

interface

uses
  SysUtils,
  Classes,
  AbDfBase,
  AbDfInW,
  AbDfHufD;

type
  TAb32bit = longint; { a 32-bit type}

  PAbDfLitBuckets = ^TAbDfLitBuckets;
  TAbDfLitBuckets = array [0..285] of integer;
  PAbDfDistBuckets = ^TAbDfDistBuckets;
  TAbDfDistBuckets = array [0..31] of integer;
  PAbDfCodeLenBuckets = ^TAbDfCodeLenBuckets;
  TAbDfCodeLenBuckets = array [0..18] of integer;


const
  AbExtractMask : array [1..31] of TAb32bit =
       ($00000001, $00000003, $00000007, $0000000F,
        $0000001F, $0000003F, $0000007F, $000000FF,
        $000001FF, $000003FF, $000007FF, $00000FFF,
        $00001FFF, $00003FFF, $00007FFF, $0000FFFF,
        $0001FFFF, $0003FFFF, $0007FFFF, $000FFFFF,
        $001FFFFF, $003FFFFF, $007FFFFF, $00FFFFFF,
        $01FFFFFF, $03FFFFFF, $07FFFFFF, $0FFFFFFF,
        $1FFFFFFF, $3FFFFFFF, $7FFFFFFF);

type
  TAbDfInBitStream = class { input bit stream}
    private
      FBitBuffer : TAb32bit;
      FBitsLeft  : integer;
      FBufEnd    : PAnsiChar;
      FBuffer    : PAnsiChar;
      FBufPos    : PAnsiChar;
      FByteCount : longint;
      FFakeCount : integer;
      FOnProgress: TAbProgressStep;
      {$IFOPT C+}
      FPeekCount : integer;
      {$ENDIF}
      FStream    : TStream;
      FStreamSize: longint;
    protected
      function ibsFillBuffer : boolean;
    public
      constructor Create(aStream     : TStream;
                         aOnProgress : TAbProgressStep;
                         aStreamSize : longint);
      destructor Destroy; override;

      procedure AlignToByte;
      procedure DiscardBits(aCount : integer);
      procedure DiscardMoreBits(aCount : integer);
      function PeekBits(aCount : integer) : integer;
      function PeekMoreBits(aCount : integer) : integer;
      function ReadBit : boolean;
      function ReadBits(aCount : integer) : integer;
      procedure ReadBuffer(var aBuffer; aCount : integer);

      property BitBuffer : TAb32bit read FBitBuffer write FBitBuffer;
      property BitsLeft  : integer read FBitsLeft write FBitsLeft;
  end;

type
  TAbDfOutBitStream = class { output bit stream}
    private
      FBitBuffer : TAb32bit;
      FBitsUsed  : integer;
      FBufEnd    : PAnsiChar;
      FBuffer    : PAnsiChar;
      FBufPos    : PAnsiChar;
      FStream    : TStream;
    protected
      procedure obsEmptyBuffer;
    public
      constructor Create(aStream : TStream);
      destructor Destroy; override;

      procedure AlignToByte;
      function Position : longint;
      procedure WriteBit(aBit : boolean);
      procedure WriteBits(aBits : integer; aCount : integer);
      procedure WriteBuffer(var aBuffer; aCount : integer);
      procedure WriteMoreBits(aBits : integer; aCount : integer);

      property BitBuffer : TAb32bit read FBitBuffer write FBitBuffer;
      property BitsUsed  : integer read FBitsUsed write FBitsUsed;
  end;

type
  TAbDfLZStream = class { LZ77 token stream}
    private
      FCurPos      : PAnsiChar;
      FDistBuckets : PAbDfDistBuckets;
      FDistCount   : integer;
      FLitBuckets  : PAbDfLitBuckets;
      FLitCount    : integer;
      FLog         : TAbLogger;
      FSlideWin    : TAbDfInputWindow;
      FStartOfs    : Int64;
      FStoredSize  : LongWord;
      FStream      : PAnsiChar;
      FStrmEnd     : PAnsiChar;
      {$IFDEF UseLogging}
      FSWPos       : longint;
      {$ENDIF}
      FUseDeflate64: boolean;
    protected
      function lzsGetApproxSize : LongWord;
      function lzsGetStaticSize : integer;
      function lzsGetStoredSize : integer;
      function lzsIsFull : boolean;
    public
      constructor Create(aSlideWin     : TAbDfInputWindow;
                         aUseDeflate64 : boolean;
                         aLog          : TAbLogger);
      destructor Destroy; override;

      function AddLenDist(aLen : integer; aDist : integer) : boolean;
        { returns true if the stream is "full"}
      function AddLiteral(aCh : AnsiChar) : boolean;
        { returns true if the stream is "full"}

      procedure Clear;
      procedure Encode(aBitStrm      : TAbDfOutBitStream;
                       aLitTree      : TAbDfDecodeHuffmanTree;
                       aDistTree     : TAbDfDecodeHuffmanTree;
                       aUseDeflate64 : boolean);
      procedure Rewind;

      procedure ReadStoredBuffer(var aBuffer; aCount : integer);

      property LenDistCount : integer read FDistCount;
      property LiteralCount : integer read FLitCount;

      property DistBuckets : PAbDfDistBuckets read FDistBuckets;
      property LitBuckets : PAbDfLitBuckets read FLitBuckets;

      property StaticSize  : integer read lzsGetStaticSize;{ in bits}
      property StoredSize  : integer read lzsGetStoredSize;{ in bytes}
  end;

type
  TAbDfCodeLenStream = class { codelength token stream}
    private
      FBuckets  : PAbDfCodeLenBuckets;
      FPosition : PAnsiChar;
      FStream   : PAnsiChar; {array [0..285+32*2] of byte;}
      FStrmEnd  : PAnsiChar;
    protected
    public
      constructor Create(aLog : TAbLogger);
      destructor Destroy; override;

      procedure Build(const aCodeLens : array of integer;
                            aCount    : integer);
      procedure Encode(aBitStrm : TAbDfOutBitStream;
                       aTree    : TAbDfDecodeHuffmanTree);

      property Buckets : PAbDfCodeLenBuckets read FBuckets;
  end;

implementation

uses
  AbDfXlat;

type
  PAb32bit = ^TAb32bit;

const
  BitStreamBufferSize = 16*1024;

{===TAbDfInBitStream=================================================}
constructor TAbDfInBitStream.Create(aStream     : TStream;
                                    aOnProgress : TAbProgressStep;
                                    aStreamSize : longint);
begin
  {protect against dumb programming mistakes}
  Assert(aStream <> nil,
         'TAbDfInBitStream.Create: Cannot create a bit stream wrapping a nil stream');

  {create the ancestor}
  inherited Create;

  {save the stream instance, allocate the buffer}
  FStream := aStream;
  GetMem(FBuffer, BitStreamBufferSize);

  {save the on progress handler}
  if Assigned(aOnProgress) and (aStreamSize > 0) then begin
    FOnProgress := aOnProgress;
    //FStreamSize := aStreamSize;
    FStreamSize := aStream.Size - aStream.Position;
  end;
end;
{--------}
destructor TAbDfInBitStream.Destroy;
begin
  {if we did some work...}
  if (FBuffer <> nil) then begin

    {reposition the underlying stream to the point where we stopped;
     this position is equal to...
      the position of the underlying stream, PLUS
      the number of fake bytes we added, LESS
      the number of bytes in the buffer, PLUS
      the position in the buffer, PLUS
      the number of complete bytes in the bit buffer}
    FStream.Seek(FStream.Position +
                 FFakeCount -
                 (FBufEnd - FBuffer) +
                 (FBufPos - FBuffer) -
                 (FBitsLeft div 8), soFromBeginning);

    {free the buffer}
    FreeMem(FBuffer);
  end;

  {destroy the ancestor}
  inherited Destroy;
end;
{--------}
procedure TAbDfInBitStream.AlignToByte;
begin
  {get rid of the odd bits by shifting them out of the bit cache}
  FBitBuffer := FBitBuffer shr (FBitsLeft mod 8);
  dec(FBitsLeft, FBitsLeft mod 8);
end;
{--------}
procedure TAbDfInBitStream.DiscardBits(aCount : integer);
var
  BitsToGo : integer;
begin
  {aCount comes from a (possibly corrupt) stream, so check that it is
   the correct range, 1..32}
  if (aCount <= 0) or (aCount > 32) then
    raise EAbInternalInflateError.Create(
       'count of bits must be between 1 and 32 inclusive [TAbDfInBitStream.DiscardBits]');

  {$IFOPT C+}
  {verify that the count of bits to discard is less than or equal to
   the recent count from PeekBits--a programming error}
  Assert((aCount <= FPeekCount),
         'TAbDfInBitStream.DiscardBits: discarding more bits than peeked');
  {since we're discarding bits already peeked, reset the peek count}
  FPeekCount := 0;
  {$ENDIF}

  {if we have more than enough bits in our bit buffer, update the
   bitbuffer and the number of bits left}
  if (aCount <= FBitsLeft) then begin
    FBitBuffer := FBitBuffer shr aCount;
    dec(FBitsLeft, aCount);
  end

  {otherwise we shall have to read another integer out of the buffer
   to satisfy the request}
  else begin
    {check that there is data in the buffer, if not it's indicates a
     corrupted stream: PeekBits should have filled it}
    if (FBufPos = FBufEnd) then
      raise EAbInternalInflateError.Create(
         'no more compressed data in stream [TAbDfInBitStream.DiscardBits]');

    {refill the bit buffer}
    BitsToGo := aCount - FBitsLeft;
    FBitBuffer := PAb32bit(FBufPos)^;
    inc(FBufPos, sizeof(TAb32bit));
    FBitBuffer := FBitBuffer shr BitsToGo;
    FBitsLeft := 32 - BitsToGo;
  end;
end;
{--------}
procedure TAbDfInBitStream.DiscardMoreBits(aCount : integer);
var
  BitsToGo : integer;
begin
  {aCount comes from a (possibly corrupt) stream, so check that it is
   the correct range, 1..32}
  if (aCount <= 0) or (aCount > 32) then
    raise EAbInternalInflateError.Create(
       'count of bits must be between 1 and 32 inclusive [TAbDfInBitStream.DiscardMoreBits]');

  {$IFOPT C+}
  {verify that the count of bits to discard is less than or equal to
   the recent count from PeekBits--a programming error}
  Assert((aCount <= FPeekCount),
         'TAbDfInBitStream.DiscardBits: discarding more bits than peeked');
  {since we're discarding bits already peeked, reset the peek count}
  FPeekCount := 0;
  {$ENDIF}

  {check that there is data in the buffer, if not it's indicates a
   corrupted stream: PeekBits/PeekMoreBits should have filled it}
  if (FBufPos = FBufEnd) then
    raise EAbInternalInflateError.Create(
       'no more compressed data in stream [TAbDfInBitStream.DiscardBits]');

  {refill the bit buffer}
  BitsToGo := aCount - FBitsLeft;
  FBitBuffer := PAb32bit(FBufPos)^;
  inc(FBufPos, sizeof(TAb32bit));
  FBitBuffer := FBitBuffer shr BitsToGo;
  FBitsLeft := 32 - BitsToGo;
end;
{--------}
function TAbDfInBitStream.ibsFillBuffer : boolean;
var
  BytesRead   : longint;
  BytesToRead : longint;
  i           : integer;
  Percent     : integer;
  Buffer      : PAnsiChar;
  BufferCount : integer;
begin
  {check for dumb programming mistakes: this routine should only be
   called if there are less than 4 bytes unused in the buffer}
  Assert((FBufEnd - FBufPos) < sizeof(longint),
         'TAbDfInBitStream.ibsFillBuffer: the buffer should be almost empty');

  {if there are still 1, 2, or three bytes unused, move them to the
   front of the buffer}
  Buffer := FBuffer;
  while (FBufPos <> FBufEnd) do begin
    Buffer^ := FBufPos^;
    inc(FBufPos);
    inc(Buffer);
  end;

  {fill the buffer}
  BytesToRead := BitStreamBufferSize - (Buffer - FBuffer);
  BytesRead := FStream.Read(Buffer^, BytesToRead);

  {reset the internal pointers}
  FBufPos := FBuffer;
  FBufEnd := Buffer + BytesRead;
  BufferCount := FBufEnd - FBuffer;

  {if, as a result of the read, no data is in the buffer, return
   false; the caller will decide what to do about the problem}
  if (BufferCount = 0) then
    Result := false

  {otherwise there is data to be processed}
  else begin
    Result := true;

    {if we didn't read anything from the stream, we need to make sure
     that enough buffer is zeroed out so that reading longint values
     don't produce (dreadfully) bogus values}
    if (BytesRead = 0) and ((BufferCount mod 4) <> 0) then begin
      FFakeCount := 4 - (BufferCount mod 4);
      for i := 0 to pred(FFakeCount) do begin
        FBufEnd^ := #0;
        inc(FBufEnd);
      end;
    end;

    {fire the progress event}
    if Assigned(FOnProgress) then begin
      inc(FByteCount, BytesRead);
      Percent := Round((100.0 * FByteCount) / FStreamSize);
      FOnProgress(Percent);
    end;
  end;
end;
{--------}
function TAbDfInBitStream.PeekBits(aCount : integer) : integer;
var
  BitsToGo : integer;
  TempBuffer : integer;
begin
  {check that aCount is in the correct range 1..32}
  Assert((0 <= aCount) and (aCount <= 32),
         'TAbDfInBitStream.PeekBits: count of bits must be between 1 and 32 inclusive');

  {if we have more than enough bits in our bit buffer, return as many
   as needed}
  if (aCount <= FBitsLeft) then
    Result := FBitBuffer and AbExtractMask[aCount]

  {otherwise we shall have to read another integer out of the buffer
   to satisfy the request; note that this will fill the stream buffer
   if required}
  else begin
    BitsToGo := aCount - FBitsLeft;
    Result := FBitBuffer;
    if ((FBufEnd - FBufPos) < sizeof(TAb32bit)) then
      if not ibsFillBuffer then
        TempBuffer := 0
      else
        TempBuffer := PAb32bit(FBufPos)^
    else
      TempBuffer := PAb32bit(FBufPos)^;
    Result := Result +
             ((TempBuffer and AbExtractMask[BitsToGo]) shl FBitsLeft);
  end;

  {$IFOPT C+}
  {save the number of bits peeked for an assertion check later}
  FPeekCount := aCount;
  {$ENDIF}
end;
{--------}
function TAbDfInBitStream.PeekMoreBits(aCount : integer) : integer;
var
  BitsToGo : integer;
  TempBuffer : integer;
begin
  BitsToGo := aCount - FBitsLeft;
  Result := FBitBuffer;
  if ((FBufEnd - FBufPos) < sizeof(TAb32bit)) then
    if not ibsFillBuffer then
      TempBuffer := 0
    else
      TempBuffer := PAb32bit(FBufPos)^
  else
    TempBuffer := PAb32bit(FBufPos)^;
  Result := Result +
           ((TempBuffer and AbExtractMask[BitsToGo]) shl FBitsLeft);
end;
{--------}
function TAbDfInBitStream.ReadBit : boolean;
begin
  if (FBitsLeft = 0) then begin
    if ((FBufEnd - FBufPos) < sizeof(TAb32bit)) then
      if not ibsFillBuffer then
        raise EAbInternalInflateError.Create(
          'no more compressed data in stream [TAbDfInBitStream.ReadBit]');
    FBitBuffer := PAb32bit(FBufPos)^;
    inc(FBufPos, sizeof(TAb32bit));
    FBitsLeft := 32;
  end;
  Result := Odd(FBitBuffer);
  FBitBuffer := FBitBuffer shr 1;
  dec(FBitsLeft);
end;
{--------}
function TAbDfInBitStream.ReadBits(aCount : integer) : integer;
var
  BitsToGo : integer;
begin
  {aCount comes from a (possibly corrupt) stream, so check that it is
   the correct range, 1..16}
  if (aCount <= 0) or (aCount > 16) then
    raise EAbInternalInflateError.Create(
       'count of bits must be between 1 and 16 inclusive [TAbDfInBitStream.ReadBits]');

  {if we have more than enough bits in our bit buffer, return as many
   as needed, and update the bitbuffer and the number of bits left}
  if (aCount <= FBitsLeft) then begin
    Result := FBitBuffer and AbExtractMask[aCount];
    FBitBuffer := FBitBuffer shr aCount;
    dec(FBitsLeft, aCount);
  end

  {if we have exactly enough bits in our bit buffer, return them all,
   and update the bitbuffer and the number of bits left}
  else if (aCount = FBitsLeft) then begin
    Result := FBitBuffer;
    FBitBuffer := 0;
    FBitsLeft := 0;
  end

  {otherwise we shall have to read another integer out of the buffer
   to satisfy the request}
  else begin
    BitsToGo := aCount - FBitsLeft;
    Result := FBitBuffer;
    if ((FBufEnd - FBufPos) < sizeof(TAb32bit)) then
      if not ibsFillBuffer then
        raise EAbInternalInflateError.Create(
          'no more compressed data in stream [TAbDfInBitStream.ReadBits]');
    FBitBuffer := PAb32bit(FBufPos)^;
    inc(FBufPos, sizeof(TAb32bit));
    Result := Result +
              ((FBitBuffer and AbExtractMask[BitsToGo]) shl FBitsLeft);
    FBitBuffer := FBitBuffer shr BitsToGo;
    FBitsLeft := 32 - BitsToGo;
  end;
end;
{--------}
procedure TAbDfInBitStream.ReadBuffer(var aBuffer; aCount : integer);
var
  i : integer;
  Buffer : PAnsiChar;
  BytesToRead   : integer;
  BytesInBuffer : integer;
begin
  {this method is designed to read a set of bytes and this can only be
   done if the stream has been byte aligned--if it isn't, it's a
   programming error}
  Assert((FBitsLeft mod 8) = 0,
         'TAbDfInBitStream.ReadBuffer. cannot read a buffer unless the stream is byte-aligned');

  {get the address of the user buffer as a PChar: easier arithmetic}
  Buffer := @aBuffer;

  {if we have some bits left in the bit buffer, we need to copy those
   first}
  if (FBitsLeft > 0) then begin
    BytesToRead := FBitsLeft div 8;
    for i := 0 to pred(BytesToRead) do begin
      Buffer^ := AnsiChar(FBitBuffer and $FF);
      inc(Buffer);
      FBitBuffer := FBitBuffer shr 8;
    end;
    {calculate the count of bytes still to read}
    dec(aCount, BytesToRead);
  end;

  {calculate the number of bytes to copy}
  BytesInBuffer := FBufEnd - FBufPos;
  if (aCount <= BytesInBuffer) then
    BytesToRead := aCount
  else
    BytesToRead := BytesInBuffer;

  {copy the data from our buffer to the user buffer}
  Move(FBufPos^, Buffer^, BytesToRead);

  {update variables}
  dec(aCount, BytesToRead);
  inc(FBufPos, BytesToRead);

  {while there is still data to copy, keep on filling our internal
   buffer and copy it to the user buffer}
  while (aCount <> 0) do begin

    {increment the user buffer pointer past the data just copied}
    inc(Buffer, BytesToRead);

    {fill our buffer}
    if not ibsFillBuffer then
      raise EAbInternalInflateError.Create(
        'no more compressed data in stream [TAbDfInBitStream.ReadBuffer]');

    {calculate the number of bytes to copy}
    BytesInBuffer := FBufEnd - FBufPos;
    if (aCount <= BytesInBuffer) then
      BytesToRead := aCount
    else
      BytesToRead := BytesInBuffer;

    {copy the data from our buffer to the user buffer}
    Move(FBufPos^, Buffer^, BytesToRead);

    {update variables}
    dec(aCount, BytesToRead);
    inc(FBufPos, BytesToRead);
  end;

  {now we've copied everything over, reset the bit variables: they're
   empty and need refilling}
  FBitBuffer := 0;
  FBitsLeft := 0;
end;
{====================================================================}


{===TAbDfOutBitStream================================================}
constructor TAbDfOutBitStream.Create(aStream : TStream);
begin
  {protect against dumb programming mistakes}
  Assert(aStream <> nil,
         'TAbDfOutBitStream.Create: Cannot create a bit stream wrapping a nil stream');

  {create the ancestor}
  inherited Create;

  {save the stream instance, allocate the buffer}
  FStream := aStream;
  GetMem(FBuffer, BitStreamBufferSize);
  FBufEnd := FBuffer + BitStreamBufferSize;
  FBufPos := FBuffer;
end;
{--------}
destructor TAbDfOutBitStream.Destroy;
var
  i : integer;
begin
  {if the buffer was allocated...}
  if (FBuffer <> nil) then begin

    {if there are still some bits in the bit buffer...}
    if (FBitsUsed <> 0) then begin

      {pad the bit buffer to a byte boundary}
      AlignToByte;

      {empty the main buffer if there isn't enough room to copy over
       the 1 to 4 bytes in the bit buffer}
      if ((FBufEnd - FBufPos) < FBitsUsed div 8) then
        obsEmptyBuffer;

      {flush the bit buffer}
      for i := 1 to (FBitsUsed div 8) do begin
        FBufPos^ := AnsiChar(FBitBuffer);
        FBitBuffer := FBitBuffer shr 8;
        inc(FBufPos);
      end;
    end;

    {if there is some data in the main buffer, empty it}
    if (FBufPos <> FBuffer) then
      obsEmptyBuffer;

    {free the buffer}
    FreeMem(FBuffer);
  end;

  {destroy the ancestor}
  inherited Destroy;
end;
{--------}
procedure TAbDfOutBitStream.AlignToByte;
begin
  {round up the number of bits used to the nearest 8}
  FBitsUsed := (FBitsUsed + 7) and $F8;

  {if the bit buffer is now full, flush it to the main buffer}
  if (FBitsUsed = 32) then begin
    if ((FBufEnd - FBufPos) < sizeof(TAb32bit)) then
      obsEmptyBuffer;
    PAb32bit(FBufPos)^ := FBitBuffer;
    inc(FBufPos, sizeof(TAb32bit));
    FBitBuffer := 0;
    FBitsUsed := 0;
  end;
end;
{--------}
procedure TAbDfOutBitStream.obsEmptyBuffer;
var
  ByteCount    : integer;
  BytesWritten : longint;
begin
  {empty the buffer}
  ByteCount := FBufPos - FBuffer;
  BytesWritten := FStream.Write(FBuffer^, ByteCount);

  {if we couldn't write the correct number of bytes, it's an error}
  if (BytesWritten <> ByteCount) then
    raise EAbInternalDeflateError.Create(
       'could not write to the output stream [TAbDfInBitStream.obsEmptyBuffer]');

  {reset the pointers}
  FBufPos := FBuffer;
end;
{--------}
function TAbDfOutBitStream.Position : longint;
begin
  Assert(false,
         'TAbDfOutBitStream.Position: not implemented yet!');
  Result := -1;
end;
{--------}
procedure TAbDfOutBitStream.WriteBit(aBit : boolean);
begin
  {only set the corresponding bit in the bit buffer if the passed bit
   is set (the bit buffer is set to zero when emptied, so we don't
   actually have to record clear bits)}
  if aBit then
    FBitBuffer := FBitBuffer or (1 shl FBitsUsed);

  {we've now got one more bit}
  inc(FBitsUsed);

  {if the bit buffer is now full, flush it to the main buffer}
  if (FBitsUsed = 32) then begin
    if ((FBufEnd - FBufPos) < sizeof(TAb32bit)) then
      obsEmptyBuffer;
    PAb32bit(FBufPos)^ := FBitBuffer;
    inc(FBufPos, sizeof(TAb32bit));
    FBitBuffer := 0;
    FBitsUsed := 0;
  end;
end;
{--------}
procedure TAbDfOutBitStream.WriteBits(aBits : integer; aCount : integer);
begin
  {protect against programming mistakes...}
  {..the count should be in the range 1 to 16 (BTW, the latter is only
     used once: Deflate64 with length symbol 258)}
  Assert((0 < aCount) and (aCount <= 16),
         'TAbDfOutBitStream.WriteBits: aCount should be from 1 to 16');
  {..there shouldn't be more than aCount bits}
  Assert((aBits shr aCount) = 0,
         'TAbDfOutBitStream.WriteBits: aBits has more than aCount bits');

  {copy as many bits as we can to the bit buffer}
  FBitBuffer := FBitBuffer or (aBits shl FBitsUsed);

  {increment the number of bits used}
  inc(FBitsUsed, aCount);

  {if we've overshot...}
  if (FBitsUsed >= 32) then begin

    {the bit buffer is now full, so flush it}
    if ((FBufEnd - FBufPos) < sizeof(TAb32bit)) then
      obsEmptyBuffer;
    PAb32bit(FBufPos)^ := FBitBuffer;
    inc(FBufPos, sizeof(TAb32bit));

    {patch up the bit buffer and the number of bits used}
    dec(FBitsUsed, 32);
    FBitBuffer := aBits shr (aCount - FBitsUsed);
  end;
end;
{--------}
procedure TAbDfOutBitStream.WriteBuffer(var aBuffer; aCount : integer);
var
  Buffer : PAnsiChar;
  BytesToCopy : integer;
begin
  {guard against dumb programming errors: we must be byte aligned}
  Assert((FBitsUsed and $7) = 0,
         'TAbDfOutBitStream.WriteBuffer: must be byte aligned');

  {use the user buffer as a PChar}
  Buffer := @aBuffer;

  {flush the bit buffer to the underlying stream}
  while (FBitsUsed <> 0) do begin
    if (FBufEnd = FBufPos) then
      obsEmptyBuffer;
    FBufPos^ := AnsiChar(FBitBuffer and $FF);
    inc(FBufPos);
    FBitBuffer := FBitBuffer shr 8;
    dec(FBitsUsed, 8);
  end;

  {copy over the data to the underlying stream}
  BytesToCopy := FBufEnd - FBufPos;
  if (BytesToCopy > aCount) then
    BytesToCopy := aCount;
  Move(Buffer^, FBufPos^, BytesToCopy);
  inc(FBufPos, BytesToCopy);
  dec(aCount, BytesToCopy);
  while (aCount <> 0) do begin
    inc(Buffer, BytesToCopy);
    obsEmptyBuffer;
    BytesToCopy := FBufEnd - FBufPos;
    if (BytesToCopy > aCount) then
      BytesToCopy := aCount;
    Move(Buffer^, FBufPos^, BytesToCopy);
    inc(FBufPos, BytesToCopy);
    dec(aCount, BytesToCopy);
  end;

  {finish with a flushed buffer}
  obsEmptyBuffer;
end;
{--------}
procedure TAbDfOutBitStream.WriteMoreBits(aBits : integer; aCount : integer);
begin
  {the bit buffer is now full, so flush it}
  if ((FBufEnd - FBufPos) < sizeof(TAb32bit)) then
    obsEmptyBuffer;
  PAb32bit(FBufPos)^ := FBitBuffer;
  inc(FBufPos, sizeof(TAb32bit));

  {patch up the bit buffer and the number of bits used}
  dec(FBitsUsed, 32);
  FBitBuffer := aBits shr (aCount - FBitsUsed);
end;
{====================================================================}


{===TAbDfLZStream====================================================}
const
  {Implementation note: this stream size has been chosen so that if
   the data must be stored, a block size of about 64K will result}
  StreamSize = 160 * 1024;
type
  PWord = ^word;
{--------}
constructor TAbDfLZStream.Create(aSlideWin     : TAbDfInputWindow;
                                 aUseDeflate64 : boolean;
                                 aLog          : TAbLogger);
begin
  {create the ancestor}
  inherited Create;

  {save the sliding window and the logger}
  FSlideWin := aSlideWin;
  FUseDeflate64 := aUseDeflate64;
  FLog := aLog;

  {create the buckets}
  New(FDistBuckets);
  New(FLitBuckets);

  {create the memory stream, allocate its buffer, position at start}
  GetMem(FStream, StreamSize);
  Clear;
end;
{--------}
destructor TAbDfLZStream.Destroy;
begin
  {free the buckets}
  if (FDistBuckets <> nil) then
    Dispose(FDistBuckets);
  if (FLitBuckets <> nil) then
    Dispose(FLitBuckets);

  {free the memory stream}
  if (FStream <> nil) then
    FreeMem(FStream);

  {destroy the ancestor}
  inherited Destroy;
end;
{--------}
{$IFDEF UseLogging}
procedure AddLenDistToLog(aLog     : TAbLogger;
                          aPosn    : longint;
                          aLen     : integer;
                          aDist    : integer;
                          aOverLap : boolean);
begin
  {NOTE the reason for this separate routine is to avoid string
        allocations and try..finally blocks in the main method: an
        optimization issue}
  if aOverLap then
    aLog.WriteLine(Format('%8x Len: %-3d, Dist: %-5d  **overlap**',
                          [aPosn, aLen, aDist]))
  else
    aLog.WriteLine(Format('%8x Len: %-3d, Dist: %-5d',
                          [aPosn, aLen, aDist]));
end;
{$ENDIF}
{--------}
function TAbDfLZStream.AddLenDist(aLen : integer; aDist : integer)
                                                            : boolean;
var
  LenSymbol  : integer;
  DistSymbol : integer;
  CurPos     : PAnsiChar;
begin
  {$IFDEF UseLogging}
  {log it}
  if (FLog <> nil) then begin
    if (aLen > aDist) then
      AddLenDistToLog(FLog, FSWPos, aLen, aDist, true)
    else
      AddLenDistToLog(FLog, FSWPos, aLen, aDist, false);
    inc(FSWPos, aLen);
  end;
  {$ENDIF}

  {write a length/distance record to the stream}
  CurPos := FCurPos;
  CurPos^ := AnsiChar(false);
  inc(CurPos);
  PWord(CurPos)^ := word(aLen - 1);
  inc(CurPos, sizeof(word));
  PWord(CurPos)^ := word(aDist - 1);
  inc(CurPos, sizeof(word));
  FCurPos := CurPos;

  {increment the various counters}
  inc(FDistCount);
  inc(FStoredSize, aLen);

  {convert the length and distance to their symbols}
  {$IFOPT C+} {if Assertions are on}
  LenSymbol := AbSymbolTranslator.TranslateLength(aLen);
  DistSymbol := AbSymbolTranslator.TranslateDistance(aDist);
  {$ELSE}
  if (3 <= aLen) and (aLen <= 258) then
    LenSymbol := AbSymbolTranslator.LenSymbols[aLen-3] + 257
  else
    LenSymbol := 285;
  if (aDist <= 256) then
    DistSymbol := AbSymbolTranslator.ShortDistSymbols[aDist - 1]
  else if (aDist <= 32768) then
    DistSymbol := AbSymbolTranslator.MediumDistSymbols[((aDist - 1) div 128) - 2]
  else
    DistSymbol := AbSymbolTranslator.LongDistSymbols[((aDist - 1) div 16384) - 2];
  {$ENDIF}

  {increment the buckets}
  inc(FLitBuckets^[LenSymbol]);
  inc(FDistBuckets^[DistSymbol]);

  {return whether the stream is full and needs encoding}
  Result := lzsIsFull;
end;
{--------}
{$IFDEF UseLogging}
procedure AddLiteralToLog(aLog     : TAbLogger;
                          aPosn    : longint;
                          aCh      : AnsiChar);
begin
  {NOTE the reason for this separate routine is to avoid string
        allocations and try..finally blocks in the main method: an
        optimization issue}
  if (' ' < aCh) and (aCh <= '~') then
    aLog.WriteLine(Format('%8x Char: %3d $%2x [%s]', [aPosn, ord(aCh), ord(aCh), aCh]))
  else
    aLog.WriteLine(Format('%8x Char: %3d $%2x', [aPosn, ord(aCh), ord(aCh)]));
end;
{$ENDIF}
{--------}
function TAbDfLZStream.AddLiteral(aCh : AnsiChar) : boolean;
var
  CurPos : PAnsiChar;
begin
  {$IFDEF UseLogging}
  {log it}
  if (FLog <> nil) then begin
    AddLiteralToLog(FLog, FSWPos, aCh);
    inc(FSWPos);
  end;
  {$ENDIF}

  {write a literal to the internal stream}
  CurPos := FCurPos;
  CurPos^ := AnsiChar(true);
  inc(CurPos);
  CurPos^ := aCh;
  inc(CurPos);
  FCurPos := CurPos;

  {increment the various counters}
  inc(FLitCount);
  inc(FLitBuckets^[byte(aCh)]);
  inc(FStoredSize);

  {return whether the stream is full and needs encoding}
  Result := lzsIsFull;
end;
{--------}
procedure TAbDfLZStream.Clear;
begin
  {position the stream at the start}
  Rewind;

  {reset all variables}
  FStrmEnd := nil;
  FLitCount := 0;
  FDistCount := 0;
  FStartOfs := FSlideWin.Position;
  FStoredSize := 0;
  {$IFDEF UseLogging}
  FSWPos := FStartOfs;
  {$ENDIF}

  {reset the buckets}
  FillChar(FLitBuckets^, sizeof(FLitBuckets^), 0);
  FLitBuckets^[256] := 1; { end-of-block marker: it's always there...}
  FillChar(FDistBuckets^, sizeof(FDistBuckets^), 0);
end;
{--------}
procedure TAbDfLZStream.Encode(aBitStrm      : TAbDfOutBitStream;
                               aLitTree      : TAbDfDecodeHuffmanTree;
                               aDistTree     : TAbDfDecodeHuffmanTree;
                               aUseDeflate64 : boolean);
var
  Len       : integer;
  Dist      : integer;
  Symbol    : integer;
  CurPos    : PAnsiChar;
  StrmEnd   : PAnsiChar;
  Code      : longint;
  ExtraBits : longint;
begin
  {rewind the LZ77 stream}
  Rewind;

  {for speed use local variables}
  CurPos := FCurPos;
  StrmEnd := FStrmEnd;

  {while there are still items in the stream...}
  while (CurPos < StrmEnd) do begin

    {if the next item is a literal...}
    if boolean(CurPos^) then begin

      {encode the literal character as a symbol}
      inc(CurPos);
      {$IFOPT C+} {if Assertions are on}
      Code := aLitTree.Encode(byte(CurPos^));
      {$ELSE}
      Code := aLitTree.Encodes^[byte(CurPos^)];
      {$ENDIF}
      inc(CurPos);

      {write the code out to the bit stream}
      {$IFOPT C+}
      aBitStrm.WriteBits(Code and $FFFF, (Code shr 16) and $FF);
      {$ELSE}
      with aBitStrm do begin
        BitBuffer := BitBuffer or ((Code and $FFFF) shl BitsUsed);
        BitsUsed := BitsUsed + ((Code shr 16) and $FF);
        if (BitsUsed >= 32) then
          WriteMoreBits(Code and $FFFF, (Code shr 16) and $FF);
      end;
      {$ENDIF}
    end

    {otherwise it's a length/distance pair}
    else begin

      {DO THE LENGTH FIRST-------------------------------------------}
      {get the length from the stream}
      inc(CurPos);
      Len := integer(PWord(CurPos)^) + 1;
      inc(CurPos, sizeof(word));

      {translate it to a symbol and convert that to a code using the
       literal/length huffman tree}
      {$IFOPT C+} {if Assertions are on}
      Symbol := AbSymbolTranslator.TranslateLength(Len);
      Code := aLitTree.Encode(Symbol);
      {$ELSE}
      if (3 <= Len) and (Len <= 258) then
        Symbol := AbSymbolTranslator.LenSymbols[Len-3] + 257
      else
        Symbol := 285;
      Code := aLitTree.Encodes^[Symbol];
      {$ENDIF}

      {output the length code}
      {$IFOPT C+}
      aBitStrm.WriteBits(Code and $FFFF, (Code shr 16) and $FF);
      {$ELSE}
      with aBitStrm do begin
        BitBuffer := BitBuffer or ((Code and $FFFF) shl BitsUsed);
        BitsUsed := BitsUsed + ((Code shr 16) and $FF);
        if (BitsUsed >= 32) then
          WriteMoreBits(Code and $FFFF, (Code shr 16) and $FF);
      end;
      {$ENDIF}

      {if the length symbol were 285, its definition changes from Deflate
       to Deflate64, so make it a special case: for Deflate there are no
       extra bits, for Deflate64 output the (length - 3) as 16 bits}
      if (Symbol = 285) then begin
        if aUseDeflate64 then begin
          {$IFOPT C+}
          aBitStrm.WriteBits(Len - 3, 16);
          {$ELSE}
          with aBitStrm do begin
            BitBuffer := BitBuffer or ((Len - 3) shl BitsUsed);
            BitsUsed := BitsUsed + 16;
            if (BitsUsed >= 32) then
              WriteMoreBits(Len - 3, 16);
          end;
          {$ENDIF}
        end;
      end

      {otherwise if there are extra bits to be output for this length,
       calculate them and output them}
      else begin
        ExtraBits := Code shr 24;
        if (ExtraBits <> 0) then begin
          {$IFOPT C+}
          aBitStrm.WriteBits((Len - dfc_LengthBase[Symbol - 257]),
                              ExtraBits);
          {$ELSE}
          with aBitStrm do begin
            BitBuffer := BitBuffer or
                  ((Len - dfc_LengthBase[Symbol - 257]) shl BitsUsed);
            BitsUsed := BitsUsed + ExtraBits;
            if (BitsUsed >= 32) then
              WriteMoreBits((Len - dfc_LengthBase[Symbol - 257]),
                            ExtraBits);
          end;
          {$ENDIF}
        end;
      end;

      {DO THE DISTANCE NEXT------------------------------------------}
      {get the distance from the stream}
      Dist := integer(PWord(CurPos)^) + 1;
      inc(CurPos, sizeof(word));

      {translate it to a symbol and convert that to a code using the
       distance huffman tree}
      {$IFOPT C+} {if Assertions are on}
      Symbol := AbSymbolTranslator.TranslateDistance(Dist);
      Assert(aUseDeflate64 or (Symbol < 30),
         'TAbDfLZStream.Encode: a Deflate64 distance symbol has been generated for Deflate');
      Code := aDistTree.Encode(Symbol);
      {$ELSE}
      if (Dist <= 256) then
        Symbol := AbSymbolTranslator.ShortDistSymbols[Dist - 1]
      else if (Dist <= 32768) then
        Symbol := AbSymbolTranslator.MediumDistSymbols[((Dist - 1) div 128) - 2]
      else
        Symbol := AbSymbolTranslator.LongDistSymbols[((Dist - 1) div 16384) - 2];
      Code := aDistTree.Encodes^[Symbol];
      {$ENDIF}

      {output the distance code}
      {$IFOPT C+}
      aBitStrm.WriteBits(Code and $FFFF, (Code shr 16) and $FF);
      {$ELSE}
      with aBitStrm do begin
        BitBuffer := BitBuffer or ((Code and $FFFF) shl BitsUsed);
        BitsUsed := BitsUsed + ((Code shr 16) and $FF);
        if (BitsUsed >= 32) then
          WriteMoreBits(Code and $FFFF, (Code shr 16) and $FF);
      end;
      {$ENDIF}

      {if there are extra bits to be output for this distance, calculate
       them and output them}
      ExtraBits := Code shr 24;
      if (ExtraBits <> 0) then begin
        {$IFOPT C+}
        aBitStrm.WriteBits((Dist - dfc_DistanceBase[Symbol]),
                           ExtraBits);
        {$ELSE}
        with aBitStrm do begin
          BitBuffer := BitBuffer or
                     ((Dist - dfc_DistanceBase[Symbol]) shl BitsUsed);
          BitsUsed := BitsUsed + ExtraBits;
          if (BitsUsed >= 32) then
            WriteMoreBits((Dist - dfc_DistanceBase[Symbol]),
                          ExtraBits);
        end;
        {$ENDIF}
      end;
    end;
  end;

{clear the stream; ready for some more items}
{  Clear;}
end;
{--------}
function TAbDfLZStream.lzsGetApproxSize : LongWord;
var
  i : integer;
begin
  {note: calculates an approximate compressed size without taking too
         long about it. The average encoded bit length for literals
         and lengths is assumed to be 8. Distances are assumed to
         follow the static tree definition (ie, 5 bits per distance,
         plus any extra bits).
         There are FLitCount literals, FDistCount lengths, and
         FDistCount distances}
  Result := (13 * FDistCount) + (8 * FLitCount);
  for i := 4 to 31 do
    inc(Result, FDistBuckets^[i] * dfc_DistExtraBits[i]);
  Result := Result div 8;
end;
{--------}
function TAbDfLZStream.lzsGetStaticSize : integer;
var
  i : integer;
begin
  Result := 0;
  for i := 0 to 143 do
    inc(Result, FLitBuckets^[i] * 8);
  for i := 144 to 255 do
    inc(Result, FLitBuckets^[i] * 9);
  inc(Result, FLitBuckets^[256] * 7);
  for i := 257 to 279 do
    inc(Result, FLitBuckets^[i] *
                (7 + dfc_LitExtraBits[i - dfc_LitExtraOffset]));
  for i := 280 to 284 do
    inc(Result, FLitBuckets^[i] *
                (8 + dfc_LitExtraBits[i - dfc_LitExtraOffset]));
  if FUseDeflate64 then
    inc(Result, FLitBuckets^[285] * (8 + 16))
  else
    inc(Result, FLitBuckets^[285] * 8);

  for i := 0 to 31 do
    inc(Result, FDistBuckets^[i] * (5 + dfc_DistExtraBits[i]));
end;
{--------}
function TAbDfLZStream.lzsGetStoredSize : integer;
begin
  Result := FStoredSize;
{Result := FSlideWin.Position - FStartOfs;}
end;
{--------}
function TAbDfLZStream.lzsIsFull : boolean;
begin
  {if the number of hits on the (eventual) literal tree is a multiple
   of 8192, the stream is full if the majority were straight literals
   and we're getting approx 50% compression}
  if (((FLitCount + FDistCount) and $1FFF) = 0) then begin
    Result := (FDistCount < FLitCount) and
              (lzsGetApproxSize < (FStoredSize div 2));
    if Result then
      Exit;
  end;

  {otherwise the stream is full if the number of hits on the literal
   tree or on the distance tree is 32768}
{    Result := (FCurPos - FStream) > (StreamSIze - 100);}
  Result := (FDistCount >= 32768) or
            ((FLitCount + FDistCount) >= 32768);
end;
{--------}
procedure TAbDfLZStream.ReadStoredBuffer(var aBuffer; aCount : integer);
begin
  FSlideWin.ReadBuffer(aBuffer, aCount, FStartOfs);
  inc(FStartOfs, aCount);
end;
{--------}
procedure TAbDfLZStream.Rewind;
begin
  {position the stream at the beginning}
  FStrmEnd := FCurPos;
  FCurPos := FStream;
end;
{====================================================================}


{===TAbDfCodeLenStream===============================================}
constructor TAbDfCodeLenStream.Create(aLog : TAbLogger);
begin
  {create the ancestor}
  inherited Create;

  {allocate the stream (to contain all literals and distances and
   possible extra data}
  GetMem(FStream, (285 + 32) * 2);
  FPosition := FStream;

  {allocate the buckets}
  FBuckets := AllocMem(sizeof(TAbDfCodeLenBuckets));
end;
{--------}
destructor TAbDfCodeLenStream.Destroy;
begin
  {free the stream}
  if (FStream <> nil) then
    FreeMem(FStream);

  {free the buckets}
  if (FBuckets <> nil) then
    Dispose(FBuckets);

  {destroy the ancestor}
  inherited Destroy;
end;
{--------}
procedure TAbDfCodeLenStream.Build(const aCodeLens : array of integer;
                                         aCount    : integer);
var
  i : integer;
  State       : (ScanStart, ScanNormal, Got2nd, Got3rd);
  Count       : integer;
  ThisCount   : integer;
  CodeLen     : integer;
  PrevCodeLen : integer;
  CurPos      : PAnsiChar;
  Buckets     : PAbDfCodeLenBuckets;
begin
  {start the automaton}
  State := ScanStart;
  CurPos := FStream;
  Buckets := FBuckets;
  Count := 0;
  PrevCodeLen := 0;

  {for all the codelengths in the array (plus a fake one at the end to
   ensure all codeslengths are counted)...}
  for i := 0 to aCount do begin

    {get the current codelength}
    if (i = aCount) then
      CodeLen := -1
    else
      CodeLen := aCodeLens[i];

    {switch based on the state...}
    case State of
      ScanStart :
        begin
          PrevCodeLen := CodeLen;
          State := ScanNormal;
        end;

      ScanNormal :
        begin
          {if the current code is the same as the previous, move to
           the next state}
          if (CodeLen = PrevCodeLen) then
            State := Got2nd

          {otherwise output the previous code}
          else begin
            CurPos^ := AnsiChar(PrevCodeLen);
            inc(CurPos);
            inc(Buckets^[PrevCodeLen]);
            PrevCodeLen := CodeLen;
          end;
        end;

      Got2nd :
        begin
          {if the current code is the same as the previous, move to
           the next state; we now have three similar codes in a row}
          if (CodeLen = PrevCodeLen) then begin
            State := Got3rd;
            Count := 3;
          end

          {otherwise output the previous two similar codes, move back
           to the initial state}
          else begin
            CurPos^ := AnsiChar(PrevCodeLen);
            inc(CurPos);
            CurPos^ := AnsiChar(PrevCodeLen);
            inc(CurPos);
            inc(Buckets^[PrevCodeLen], 2);
            PrevCodeLen := CodeLen;
            State := ScanNormal;
          end;
        end;

      Got3rd:
        begin
          {if the current code is the same as the previous, increment
           the count of similar codes}
          if (CodeLen = PrevCodeLen) then
            inc(Count)

          {otherwise we need to output the repeat values...}
          else begin

            {if the previous code were a zero code...}
            if (PrevCodeLen = 0) then begin

              {while there are zero codes to be output...}
              while (Count <> 0) do begin

                {if there are less than three zero codes, output them
                 individually}
                if (Count < 3) then begin
                  while (Count <> 0) do begin
                    CurPos^ := #0;
                    inc(CurPos);
                    inc(Buckets^[0]);
                    dec(Count);
                  end;
                end

                {if there are less than 11 successive zero codes,
                 output a 17 code and the count of zeros}
                else if (Count < 11) then begin
                  CurPos^ := #17;
                  inc(CurPos);
                  inc(Buckets^[17]);
                  CurPos^ := AnsiChar(Count - 3);
                  inc(CurPos);
                  Count := 0;
                end

                {otherwise output an 18 code and the count of zeros}
                else begin
                  ThisCount := Count;
                  if (ThisCount > 138) then
                    ThisCount := 138;
                  CurPos^ := #18;
                  inc(CurPos);
                  inc(Buckets^[18]);
                  CurPos^ := AnsiChar(ThisCount - 11);
                  inc(CurPos);
                  dec(Count, ThisCount);
                end;
              end;
            end

            {otherwise the previous code was a non-zero code...}
            else begin

              {output the first code}
              CurPos^ := AnsiChar(PrevCodeLen);
              inc(CurPos);
              inc(Buckets^[PrevCodeLen]);
              dec(Count);

              {while there are more codes to be output...}
              while (Count <> 0) do begin

                {if there are less than three codes, output them
                 individually}
                if (Count < 3) then begin
                  while (Count <> 0) do begin
                    CurPos^ := AnsiChar(PrevCodeLen);
                    inc(CurPos);
                    inc(Buckets^[PrevCodeLen]);
                    dec(Count);
                  end;
                end

                {otherwise output an 16 code and the count}
                else begin
                  ThisCount := Count;
                  if (ThisCount > 6) then
                    ThisCount := 6;
                  CurPos^ := #16;
                  inc(CurPos);
                  inc(Buckets^[16]);
                  CurPos^ := AnsiChar(ThisCount - 3);
                  inc(CurPos);
                  dec(Count, ThisCount);
                end;
              end;
            end;

            {move back to the initial state}
            PrevCodeLen := CodeLen;
            State := ScanNormal;
          end;
        end;
    end;
  end;

  {set the read position}
  FStrmEnd := CurPos;
  FPosition := FStream;
end;
{--------}
procedure TAbDfCodeLenStream.Encode(aBitStrm : TAbDfOutBitStream;
                                    aTree    : TAbDfDecodeHuffmanTree);
var
  Symbol    : integer;
  ExtraData : integer;
  Code      : longint;
  CurPos    : PAnsiChar;
  StrmEnd   : PAnsiChar;
begin
  {prepare for the loop}
  CurPos := FPosition;
  StrmEnd := FStrmEnd;

  {while there are tokens in the stream...}
  while (CurPos <> StrmEnd) do begin

    {get the next symbol}
    Symbol := ord(CurPos^);
    inc(CurPos);

    {if the symbol is 0..15, get the code and output it}
    if (Symbol <= 15) then begin
      {$IFOPT C+} {if Assertions are on}
      Code := aTree.Encode(Symbol);
      {$ELSE}
      Code:= aTree.Encodes^[Symbol];
      {$ENDIF}
      aBitStrm.WriteBits(Code and $FFFF, (Code shr 16) and $FF);
    end

    {otherwise the symbol is 16, 17, or 18}
    else begin
      {get the extra data}
      ExtraData := ord(CurPos^);
      inc(CurPos);
      {get the code and output it}
      {$IFOPT C+} {if Assertions are on}
      Code := aTree.Encode(Symbol);
      {$ELSE}
      Code:= aTree.Encodes^[Symbol];
      {$ENDIF}
      aBitStrm.WriteBits(Code and $FFFF, (Code shr 16) and $FF);
      if (Symbol = 16) then
        aBitStrm.WriteBits(ExtraData, 2)
      else if (Symbol = 17) then
        aBitStrm.WriteBits(ExtraData, 3)
      else {Symbol = 18}
        aBitStrm.WriteBits(ExtraData, 7);
    end;
  end;
end;
{====================================================================}

end.
