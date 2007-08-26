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
{* ABBREVIA: AbDfEnc.pas 3.04                            *}
{*********************************************************}
{* Deflate encoding unit                                 *}
{*********************************************************}

unit AbDfEnc;

{$I AbDefine.inc}

interface

uses
  SysUtils,
  Classes,
  AbDfBase;

function Deflate(aSource : TStream; aDest : TStream;
                 aHelper : TAbDeflateHelper) : longint;

implementation

uses
  AbDfInW,
  AbDfHufD,
  AbDfStrm,
  AbDfXlat,
  AbDfCryS,
  AbDfPkMg;

{====================================================================}
function CalcDynamicBitCount(aUseDeflate64: boolean;
                             aLitBuckets  : PAbDfLitBuckets;
                             aDistBuckets : PAbDfDistBuckets;
                             aCodeBuckets : PAbDfCodeLenBuckets;
                       const aCodeLens    : array of integer;
                       const aCLCodeLens  : array of integer;
                             aLitCount    : integer;
                             aDistCount   : integer;
                             aCodeCount   : integer) : longint;
var
  Symbol     : integer;
  LastSymbol : integer;
  Inx        : integer;
begin
  {note: this routine calculates the number of bits required to
         compress a given block}

  {a dynamic block starts off with 5 bits of literal symbol count, 5
   bits of distance symbol count, 4 bits of codelength symbol count,
   and then 3 bits for every codelength symbol used}
  Result := 5 + 5 + 4 +
            (aCodeCount * 3);

  {add in the bits needed to compress the literal and distance trees}
  inc(Result, aCodeBuckets^[16] * (aCLCodeLens[16] + 2));
  inc(Result, aCodeBuckets^[17] * (aCLCodeLens[16] + 3));
  inc(Result, aCodeBuckets^[18] * (aCLCodeLens[16] + 7));
  for Symbol := 3 to pred(aCodeCount) do begin
    Inx := dfc_CodeLengthIndex[Symbol];
    Assert(Inx <=15,
           'CalcDynamicBitCount: the index array of codelengths is corrupted');
    inc(Result, aCodeBuckets^[Inx] * aCLCodeLens[Inx])
  end;

  {make the literal symbol 285 a special case}
  LastSymbol := pred(aLitCount);
  if (LastSymbol = 285) then
    LastSymbol := 284;

  {add in all the bits needed to compress the literals (except 285)}
  for Symbol := 0 to LastSymbol do
    if (Symbol < dfc_LitExtraOffset) then
      inc(Result, aLitBuckets^[Symbol] * aCodeLens[Symbol])
    else
      inc(Result, aLitBuckets^[Symbol] *
                  (aCodeLens[Symbol] +
                   dfc_LitExtraBits[Symbol - dfc_LitExtraOffset]));

  {add in all the bits needed to compress the literal symbol 285}
  if (pred(aLitCount) = 285) then
    if (not aUseDeflate64) then
      inc(Result, aLitBuckets^[285] * aCodeLens[285])
    else
      inc(Result, aLitBuckets^[285] * (aCodeLens[285] + 16));

  {add in all the bits needed to compress the distances}
  for Symbol := 0 to pred(aDistCount) do
    inc(Result, aDistBuckets^[Symbol] *
                (aCodeLens[aLitCount + Symbol] +
                 dfc_DistExtraBits[Symbol]));
end;
{====================================================================}


{====================================================================}
procedure OutputEndOfBlock(aBitStrm : TAbDfOutBitStream;
                           aLitTree : TAbDfDecodeHuffmanTree);
var
  Code : longint;
begin
  {note: this routine encodes the end-of-block character (symbol 256)
         and then writes out the code to the bit stream}

  {encode the end-of-block character as a symbol}
  {$IFOPT C+} {if Assertions are on }
  Code := aLitTree.Encode(256);
  {$ELSE}
  Code := aLitTree.Encodes^[256];
  {$ENDIF}

  {write the code out to the bit stream}
  aBitStrm.WriteBits(Code and $FFFF, (Code shr 16) and $FF);
end;
{--------}
procedure EncodeLZStreamStored(aFinalBlock   : boolean;
                               aStream       : TAbDfLZStream;
                               aBitStrm      : TAbDfOutBitStream;
                               aDataSize     : integer;
                               aLog          : TAbLogger);
var
  BlockHeader : packed record
    bhSize    : word;
    bhNotSize : word;
  end;
  Buffer    : pointer;
  Code      : integer;
  BlockSize : integer;
begin
  {note: this routine writes out an incompressible block to the bit
         stream (the store algorithm)}

  {allocate the maximum buffer we can write at once}
  GetMem(Buffer, 64 * 1024);
  try

    {while there's more incompressible data to store...}
    while (aDataSize <> 0) do begin

      {calculate the block size to write this time}
      if (aDataSize > $FFFF) then begin
        BlockSize := $FFFF;
        dec(aDataSize, $FFFF);
      end
      else begin
        BlockSize := aDataSize;
        aDataSize := 0;
      end;

      {$IFDEF UseLogging}
      {log it}
      if (aLog <> nil) then begin
        aLog.WriteLine('..Writing new block...');
        aLog.WriteLine(Format('..final block? %d', [ord(aFinalBlock)]));
        aLog.WriteLine('..block type? 0');
        aLog.WriteLine(Format('..block size:  %d', [BlockSize]));
      end;
      {$ENDIF}

      {output the block information to the bit stream}
      if aFinalBlock then
        Code := 1 + (0 shl 1)
      else
        Code := 0 + (0 shl 1);
      aBitStrm.WriteBits(Code, 3);

      {align the bit stream to the nearest byte}
      aBitStrm.AlignToByte;

      {write the stored block header}
      BlockHeader.bhSize := BlockSize;
      BlockHeader.bhNotSize := not BlockHeader.bhSize;
      aBitStrm.WriteBuffer(BlockHeader, sizeof(BlockHeader));

      {get and write this block}
      aStream.ReadStoredBuffer(Buffer^, BlockSize);
      aBitStrm.WriteBuffer(Buffer^, BlockSize);
    end;
  finally
    FreeMem(Buffer);
  end;

  {clear the stream, ready for the next block}
  aStream.Clear;
end;
{--------}
procedure EncodeLZStreamStatic(aFinalBlock   : boolean;
                               aUseDeflate64 : boolean;
                               aStream       : TAbDfLZStream;
                               aBitStrm      : TAbDfOutBitStream;
                               aLog          : TAbLogger);
var
  Code : integer;
begin
  {note: this routine writes out the stream of LZ77 tokens for the
         current block to the bit stream, using the static huffman
         trees to encode the token symbols}

  {$IFDEF UseLogging}
  {log it}
  if (aLog <> nil) then begin
    aLog.WriteLine('..Writing new block...');
    aLog.WriteLine(Format('..final block? %d', [ord(aFinalBlock)]));
    aLog.WriteLine('..block type? 1');
  end;
  {$ENDIF}

  {output the block information to the bit stream}
  if aFinalBlock then
    Code := 1 + (1 shl 1)
  else
    Code := 0 + (1 shl 1);
  aBitStrm.WriteBits(Code, 3);

  {encode the LZ77 stream}
  aStream.Encode(aBitStrm,
                 AbStaticLiteralTree, AbStaticDistanceTree,
                 aUseDeflate64);

  {output the end-of-block marker to the bit stream}
  OutputEndOfBlock(aBitStrm, AbStaticLiteralTree);
  {$IFDEF UseLogging}
  if (aLog <> nil) then
    aLog.WriteLine('Char: end-of-block marker (#256)');
  {$ENDIF}
end;
{--------}
procedure EncodeLZStreamDynamic(aFinalBlock   : boolean;
                                aUseDeflate64 : boolean;
                                aUseBest      : boolean;
                                aStream       : TAbDfLZStream;
                                aBitStrm      : TAbDfOutBitStream;
                                aLog          : TAbLogger);
var
  i : integer;
  LitTree     : TAbDfDecodeHuffmanTree;
  DistTree    : TAbDfDecodeHuffmanTree;
  CodeLenTree : TAbDfDecodeHuffmanTree;
  CodeLenStream : TAbDfCodeLenStream;
  CodeLens      : array [0..285+32] of integer;
  CLCodeLens    : array [0..18] of integer;
  LitCodeCount  : integer;
  DistCodeCount : integer;
  LenCodeCount  : integer;
  BitCount      : integer;
  Code          : integer;
  StaticSize    : integer;
  StoredSize    : integer;
begin
  {note: this routine writes out the stream of LZ77 tokens for the
         current block to the bit stream, using the dynamic huffman
         trees to encode the token symbols; if the routine determines
         that the data can better be compressed using the static
         huffman trees or should be stored as is, it'll switch
         algorithms}

  {prepare for the try..finally}
  LitTree := nil;
  DistTree := nil;
  CodeLenTree := nil;
  CodeLenStream := nil;

  try

    {calculate the code lengths for the literal symbols}
    GenerateCodeLengths(15, aStream.LitBuckets^, CodeLens, 0, aLog);

    {calculate the number of the used codelengths for the literals}
    LitCodeCount := 286;
    repeat
      dec(LitCodeCount);
    until (CodeLens[LitCodeCount] <> 0);
    inc(LitCodeCount);

    {calculate the code lengths for the distance symbols}
    GenerateCodeLengths(15, aStream.DistBuckets^, CodeLens,
                                                  LitCodeCount, aLog);

    {calculate the number of the used codelengths for the distances}
    DistCodeCount := 32;
    repeat
      dec(DistCodeCount);
    until (CodeLens[DistCodeCount + LitCodeCount] <> 0);
    inc(DistCodeCount);

    {calculate the code lengths array as a stream of items}
    CodeLenStream := TAbDfCodeLenStream.Create(aLog);
    CodeLenStream.Build(CodeLens, LitCodeCount + DistCodeCount);

    {calculate the codelengths for the code lengths}
    GenerateCodeLengths(7, CodeLenStream.Buckets^, CLCodeLens, 0, nil);

    {calculate the number of the used codelengths for the code lengths}
    LenCodeCount := 19;
    repeat
      dec(LenCodeCount);
    until (CLCodeLens[dfc_CodeLengthIndex[LenCodeCount]] <> 0);
    inc(LenCodeCount);
    {..there's a minimum of four, though}
    if (LenCodeCount < 4) then
      LenCodeCount := 4;

    {if we have to work out and use the best method...}
    if aUseBest then begin

      {calculate the number of bits required for the compressed data
       using dynamic huffman trees}
      BitCount := CalcDynamicBitCount(aUseDeflate64,
                                      aStream.LitBuckets,
                                      aStream.DistBuckets,
                                      CodeLenStream.Buckets,
                                      CodeLens,
                                      CLCodeLens,
                                      LitCodeCount,
                                      DistCodeCount,
                                      LenCodeCount);

      {choose the algorithm with the smallest size}
      StaticSize := aStream.StaticSize;
      StoredSize := (aStream.StoredSize + 4) * 8;
      if (StaticSize < BitCount) then begin
        if (StoredSize < StaticSize) then
          EncodeLZStreamStored(aFinalBlock, aStream, aBitStrm,
                               (StoredSize div 8) - 4, aLog)
        else
          EncodeLZStreamStatic(aFinalBlock, aUseDeflate64,
                               aStream, aBitStrm, aLog);
        Exit;
      end
      else if (StoredSize < BitCount) then begin
        EncodeLZStreamStored(aFinalBlock, aStream, aBitStrm,
                             (StoredSize div 8) - 4, aLog);
        Exit;
      end;
    end;

    {create the code lengths tree}
    CodeLenTree := TAbDfDecodeHuffmanTree.Create(19, 7, huEncoding);
    CodeLenTree.Build(CLCodeLens, 0, 19, [0], $FFFF);

    {$IFDEF UseLogging}
    {log the tree}
    if (aLog <> nil) then begin
      aLog.WriteLine('Code lengths tree');
      CodeLenTree.DebugPrint(aLog);
    end;
    {$ENDIF}

    {calculate the literal encoding tree}
    LitTree := TAbDfDecodeHuffmanTree.Create(286, 15, huEncoding);
    LitTree.Build(CodeLens, 0, LitCodeCount,
                                  dfc_LitExtraBits, dfc_LitExtraOffset);

    {$IFDEF UseLogging}
    {log the tree}
    if (aLog <> nil) then begin
      aLog.WriteLine('Literal/length tree');
      LitTree.DebugPrint(aLog);
    end;
    {$ENDIF}

    {calculate the distance tree}
    if aUseDeflate64 then
      DistTree := TAbDfDecodeHuffmanTree.Create(32, 15, huEncoding)
    else
      DistTree := TAbDfDecodeHuffmanTree.Create(30, 15, huEncoding);
    DistTree.Build(CodeLens, LitCodeCount, DistCodeCount,
                                dfc_DistExtraBits, dfc_DistExtraOffset);

    {$IFDEF UseLogging}
    if (aLog <> nil) then begin
      {log the tree}
      aLog.WriteLine('Distance tree');
      DistTree.DebugPrint(aLog);

      {log the new block}
      aLog.WriteLine('..Writing new block...');
      aLog.WriteLine(Format('..final block? %d', [ord(aFinalBlock)]));
      aLog.WriteLine('..block type? 2');
      aLog.WriteLine(Format('Count of literals:     %d', [LitCodeCount]));
      aLog.WriteLine(Format('Count of distances:    %d', [DistCodeCount]));
      aLog.WriteLine(Format('Count of code lengths: %d', [LenCodeCount]));
    end;
    {$ENDIF}

    {output the block information to the bit stream}
    if aFinalBlock then
      Code := 1 + (2 shl 1)
    else
      Code := 0 + (2 shl 1);
    aBitStrm.WriteBits(Code, 3);

    {output the various counts to the bit stream}
    Code := (LitCodeCount - 257) +
            ((DistCodeCount - 1) shl 5) +
            ((LenCodeCount - 4) shl 10);
    aBitStrm.WriteBits(Code, 14);

    {output the code length codelengths to the bit stream}
    for i := 0 to pred(LenCodeCount) do
      aBitStrm.WriteBits(CLCodeLens[dfc_CodeLengthIndex[i]], 3);

    {encode and write the codelength stream to the bit stream}
    CodeLenStream.Encode(aBitStrm, CodeLenTree);

    {encode and write the LZ77 stream to the bit stream}
    aStream.Encode(aBitStrm, LitTree, DistTree, aUseDeflate64);

    {output the end-of-block marker to the bit stream}
    OutputEndOfBlock(aBitStrm, LitTree);
    {$IFDEF UseLogging}
    if (aLog <> nil) then
      aLog.WriteLine('Char: end-of-block marker (#256)');
    {$ENDIF}

  finally
    LitTree.Free;
    DistTree.Free;
    CodeLenTree.Free;
    CodeLenStream.Free;
  end;
end;
{====================================================================}


{===Single algorithm Static/Dynamic Huffman tree deflate=============}
function DeflateStaticDynamic(aStatic : boolean;
                              aUseBest: boolean;
                              aSource : TStream; aDest : TStream;
                              aHelper : TAbDeflateHelper;
                              aLog    : TAbLogger) : longint;
var
  i : integer;
  SlideWin     : TAbDfInputWindow;
  BitStrm      : TAbDfOutBitStream;
  LZ77Stream   : TAbDfLZStream;
  KeyLen       : integer;
  Match        : TAbDfMatch;
  PrevMatch    : TAbDfMatch;
  UseDeflate64 : boolean;
  UseCRC32     : boolean;
  GotMatch     : boolean;
  LZStrmIsFull : boolean;
  TestForBinary: boolean;
begin
  {note: turn on the following define to see when and how the lazy
         matching algorithm works}
  {$IFDEF UseLogging}
    {$DEFINE UseLazyMatchLogging}
  {$ENDIF}

  {$IFDEF UseLogging}
  if (aLog <> nil) then
    if aStatic then
      aLog.WriteLine('..compressing source data with static huffman trees')
    else
      aLog.WriteLine('..compressing source data with dynamic huffman trees');
  {$ENDIF}

  {prepare for the try..finally}
  SlideWin := nil;
  BitStrm := nil;
  LZ77Stream := nil;
  try

    {create the sliding window}
    UseDeflate64 := (aHelper.Options and dfc_UseDeflate64) <> 0;
    UseCRC32 := (aHelper.Options and dfc_UseAdler32) = 0;
    SlideWin := TAbDfInputWindow.Create(aSource,
                                        aHelper.StreamSize,
                                        aHelper.WindowSize,
                                        aHelper.ChainLength,
                                        UseDeflate64, UseCRC32);
    SlideWin.OnProgress := aHelper.OnProgressStep;

    {create the bit stream}
    BitStrm := TAbDfOutBitStream.Create(aDest);

    {create the LZ77 stream}
    LZ77Stream := TAbDfLZStream.Create(SlideWin, UseDeflate64, aLog);
    LZStrmIsFull := false;
    TestForBinary := true;

    {set the previous match to be a literal character: this will
     ensure that no lazy matching goes on with the first key read}
    PrevMatch.maLen := 0;

    {get the first key length}
    KeyLen := SlideWin.GetNextKeyLength;

    {while the current key is three characters long...}
    while (KeyLen = 3) do begin

      {tweak for binary/text}
      {note: the test for whether a stream is binary or not is to
             check whether there are any #0 characters in the first
             1024 bytes: if there are the stream is binary.
             this test and tweaking is based on experimentation
             compression ratios for binary and text files based on the
             PKZIP 'n' option.}
      if TestForBinary and (LZ77Stream.StoredSize = 1024) then begin
        if (aHelper.PKZipOption = 'n') then
          if (LZ77Stream.LitBuckets^[0] = 0) then begin
            aHelper.AmpleLength := aHelper.AmpleLength * 2;
            aHelper.MaxLazyLength := aHelper.MaxLazyLength * 2;
            aHelper.ChainLength := aHelper.ChainLength * 2;
            SlideWin.ChainLen := aHelper.ChainLength;
          end;
        TestForBinary := false;
      end;

      {if the LZ77 stream is full, empty it}
      if LZStrmIsFull then begin
        if aStatic then
          EncodeLZStreamStatic(false, UseDeflate64,
                               LZ77Stream, BitStrm, aLog)
        else
          EncodeLZStreamDynamic(false, UseDeflate64, aUseBest,
                                LZ77Stream, BitStrm, aLog);
        LZ77Stream.Clear;
        LZStrmIsFull := false;
      end;

      {try and find a match of three or more characters (note: this
       has the side effect of adding the current key to the internal
       hash table); this routine will only return true if it finds a
       match greater than the previous match}
      GotMatch := SlideWin.FindLongestMatch(aHelper.AmpleLength,
                                            Match, PrevMatch);

      {if the maximum match length were three and the distance exceeds
       4096 bytes, it's most likely that we'll get better compression
       by outputting the three literal bytes rather than by outputting
       a length symbol, a distance symbol, and at least ten extra
       bits for the extra distance value}
      if (Match.maLen = 3) and (Match.maDist > 4096) then
        GotMatch := false;

      {if we found a match...}
      if GotMatch then begin

        {if there were no previous match, we can't do any lazy match
         processing now, so save the current match details ready for
         lazy matching the next time through, and advance the sliding
         window}
        if (PrevMatch.maLen = 0) then begin
          PrevMatch.maLen := Match.maLen;
          PrevMatch.maDist := Match.maDist;
          PrevMatch.maLit := Match.maLit;
          SlideWin.AdvanceByOne;
        end

        {otherwise the previous match is smaller than this one, so
         we're going to accept this match in preference; throw away
         the previous match, output the previous literal character
         instead and save these match details}
        else begin
          {$IFDEF UseLazyMatchLogging}
          if (aLog <> nil) then
            aLog.WriteLine(
               Format(
                  '..this match longer, rejecting previous one (%d,%d)',
                  [PrevMatch.maLen, PrevMatch.maDist]));
          {$ENDIF}
          LZStrmIsFull := LZ77Stream.AddLiteral(PrevMatch.maLit);
          PrevMatch.maLen := Match.maLen;
          PrevMatch.maDist := Match.maDist;
          PrevMatch.maLit := Match.maLit;
          SlideWin.AdvanceByOne;
        end;

        {if, by this point, we're storing up a match, check to see
         if it equals or exceeds the maximum lazy match length; if
         it does then output the match right now and avoid checking
         for a lazy match}
        if (PrevMatch.maLen >= aHelper.MaxLazyLength) then begin
          {$IFDEF UseLazyMatchLogging}
          if (aLog <> nil) then
            if ((aHelper.Options and dfc_UseLazyMatch) <> 0) then
              aLog.WriteLine('..match longer than max lazy match, using it');
          {$ENDIF}
          LZStrmIsFull :=
             LZ77Stream.AddLenDist(PrevMatch.maLen, PrevMatch.maDist);
          SlideWin.Advance(PrevMatch.maLen - 1, PrevMatch.maLen - 1);
          PrevMatch.maLen := 0;
        end;
      end

      {otherwise, we don't have a match at all: so we possibly just
       need to output a literal character}
      else begin
        {if there was a previous match, output it and discard the
         results of this match}
        if (PrevMatch.maLen <> 0) then begin
          LZStrmIsFull :=
             LZ77Stream.AddLenDist(PrevMatch.maLen, PrevMatch.maDist);
          SlideWin.Advance(PrevMatch.maLen - 1, PrevMatch.maLen - 2);
          PrevMatch.maLen := 0;
        end

        {otherwise there was no previous match or it's already been
         output, so output this literal}
        else begin
          LZStrmIsFull := LZ77Stream.AddLiteral(Match.maLit);
          SlideWin.AdvanceByOne;
          PrevMatch.maLen := 0;
        end;
      end;

      {get the next key}
      KeyLen := SlideWin.GetNextKeyLength;
    end;

    {if the last key read were one or two characters in length, save
     them as literal character encodings}
    if (KeyLen > 0) then begin
      {if there's a match pending, it'll be of length 3: output it}
      if (PrevMatch.maLen <> 0) then begin
        Assert(PrevMatch.maLen = 3,
               'DeflateStaticDynamic: previous match should be length 3');
        LZ77Stream.AddLenDist(PrevMatch.maLen, PrevMatch.maDist);
      end
      {otherwise, output the one or two final literals}
      else
        for i := 1 to KeyLen do
          LZ77Stream.AddLiteral(SlideWin.GetNextChar);
    end;

    {empty the LZ77 stream}
    if aStatic then
      EncodeLZStreamStatic(true, UseDeflate64,
                           LZ77Stream, BitStrm, aLog)
    else
      EncodeLZStreamDynamic(true, UseDeflate64, aUseBest,
                            LZ77Stream, BitStrm, aLog);

    {calculate the checksum of the input stream}
    Result := SlideWin.Checksum;
  finally
    {free the objects}
    SlideWin.Free;
    BitStrm.Free;
    LZ77Stream.Free;
  end;{try..finally}

  {$IFDEF UseLogging}
  {log it}
  if (aLog <> nil) then
    aLog.WriteLine(Format('..checksum: %8x', [Result]))
  {$ENDIF}
end;
{====================================================================}


{===Simple storing===================================================}
function DeflateStored(aSource : TStream; aDest : TStream;
                       aHelper : TAbDeflateHelper;
                       aLog    : TAbLogger) : longint;
const
  StoredBlockSize = $FFFF;
var
  Buffer    : PAnsiChar;
  BytesRead : longint;
  ByteCount : longint;
  BytesToGo : longint;
  CurPos    : longint;
  Size      : longint;
  Percent   : longint;
  CheckSum  : longint;
  UseCRC32  : boolean;
  BlockHeader : packed record
    bhInfo    : byte;
    bhSize    : word;
    bhNotSize : word;
  end;
begin
  {note: this routine merely stores the aSource stream data, no
         compression is attempted or done}
  {$IFDEF UseLogging}
  if (aLog <> nil) then
    aLog.WriteLine('..storing source data to destination, no compression');
  {$ENDIF}

  {initialize}
  ByteCount := 0;
  UseCRC32 := (aHelper.Options and dfc_UseAdler32) = 0;
  if UseCRC32 then
    Checksum := -1  { CRC32 starts off with all bits set}
  else
    CheckSum := 1;  { Adler32 starts off with a value of 1}
  if (aHelper.StreamSize > 0) then
    BytesToGo := aHelper.StreamSize
  else begin
    CurPos := aSource.Seek(0, soFromCurrent);
    Size := aSource.Seek(0, soFromEnd);
    aSource.Seek(CurPos, soFromBeginning);
    BytesToGo := Size - CurPos;
  end;

  {get a buffer}
  GetMem(Buffer, StoredBlockSize);
  try

    {while there is still data to be stored...}
    while (BytesToGo <> 0) do begin

      {read the next block}
      BytesRead := aSource.Read(Buffer^, StoredBlockSize);

      {fire the progress event}
      if Assigned(aHelper.OnProgressStep) then begin
        inc(ByteCount, BytesRead);
        Percent := Round((100.0 * ByteCount) / aHelper.StreamSize);
        aHelper.OnProgressStep(Percent);
      end;

      {update the checksum}
      if UseCRC32 then
        AbUpdateCRCBuffer(Checksum, Buffer^, BytesRead)
      else
        AbUpdateAdlerBuffer(Checksum, Buffer^, BytesRead);

      {write the block header}
      if (BytesRead = BytesToGo) then
        BlockHeader.bhInfo := 1  {ie, final block, stored}
      else
        BlockHeader.bhInfo := 0; {ie, not final block, stored}
      BlockHeader.bhSize := BytesRead;
      BlockHeader.bhNotSize := not BlockHeader.bhSize;
      aDest.WriteBuffer(BlockHeader, sizeof(BlockHeader));

      {write the block of data}
      aDest.WriteBuffer(Buffer^, BytesRead);

      {$IFDEF UseLogging}
      {log it}
      if (aLog <> nil) then begin
        if (BlockHeader.bhInfo = 0) then
          aLog.WriteLine(Format('..block size: %d', [BytesRead]))
        else
          aLog.WriteLine(Format('..block size: %d (final block)',
                                [BytesRead]));
      end;
      {$ENDIF}

      {decrement the number of bytes to go}
      dec(BytesToGo, BytesRead);
    end;
  finally
    FreeMem(Buffer);
  end;

  {return the checksum}
  {note: the CRC32 checksum algorithm requires a post-conditioning
         step after being calculated (the result is NOTted), whereas
         Adler32 does not}
  if UseCRC32 then
    Result := not Checksum
  else
    Result := Checksum;

  {$IFDEF UseLogging}
  {log it}
  if (aLog <> nil) then
    aLog.WriteLine(Format('..checksum: %8x', [Result]))
  {$ENDIF}
end;
{====================================================================}


{===Interfaced routine===============================================}
function Deflate(aSource : TStream; aDest : TStream;
                 aHelper : TAbDeflateHelper) : longint;
var
  Helper   : TAbDeflateHelper;
  Log      : TAbLogger;
  DestStrm : TStream;
  SourceStartPos : longint;
  DestStartPos   : longint;
begin
  {pre-conditions: streams are allocated,
                   options enable some kind of archiving}
  Assert(aSource <> nil, 'Deflate: aSource stream cannot be nil');
  Assert(aDest <> nil, 'Deflate: aDest stream cannot be nil');
  Assert((aHelper = nil) or ((aHelper.Options and $07) <> 0),
         'Deflate: aHelper.Options must enable some kind of archiving');

  {$IFDEF DefeatWarnings}
  Result := 0;
  {$ENDIF}

  {prepare for the try..finally}
  Helper := nil;
  Log := nil;
  DestStrm := nil;

  try {finally}
    try {except}
      {create our helper; assign the passed one to it}
      Helper := TAbDeflateHelper.Create;
      if (aHelper <> nil) then
        Helper.Assign(aHelper);

      {save the current positions of both streams}
      SourceStartPos := aSource.Position;
      DestStartPos := aDest.Position;

      {if the helper's stream size is -1, and it has a progress event
       handler, calculate the stream size from the stream itself}
      if Assigned(Helper.OnProgressStep) then begin
        if (Helper.StreamSize = -1) then
          Helper.StreamSize := aSource.Size;
      end

      {otherwise we certainly can't do any progress reporting}
      else begin
        Helper.OnProgressStep := nil;
        Helper.StreamSize := 0;
      end;

      {if lazy matching is not requested, ensure the maximum lazy
       match length is zero: this make the LZ77 code a little easier
       to understand}
      if ((Helper.Options and dfc_UseLazyMatch) = 0) then
        Helper.MaxLazyLength := 0;

      {patch up the various lengths in the helper if they specify the
       maximum (that is, are equal to -1)}
      if (Helper.AmpleLength = -1) then
        Helper.AmpleLength := MaxLongInt;
      if (Helper.MaxLazyLength = -1) then
        Helper.MaxLazyLength := MaxLongInt;
      if (Helper.ChainLength = -1) then
        Helper.ChainLength := MaxLongInt;

      {create the logger, if requested}
      if (Helper.LogFile <> '') then begin
        Log := TAbLogger.Create(Helper.LogFile);
        Log.WriteLine('DEFLATING STREAM...');
        {$IFNDEF UseLogging}
        Log.WriteLine('Need to recompile the app with UseLogging turned on');
        {$ENDIF}
      end;

      {if a passphrase was specified, create an encryption stream
       wrapping the destination}
      if (Helper.Passphrase <> '') then begin
        {$IFDEF UseLogging}
        Log.WriteLine('(passphrase is set: stream is encrypted)');
        {$ENDIF}
        DestStrm := TAbDfEncryptStream.Create(
                         aDest, Helper.CheckValue, Helper.Passphrase);
      end
      {otherwise, just use the destination stream without wrapping}
      else
        DestStrm := aDest;

      {use the helper's options property to decide what to do}
      case (Helper.Options and $07) of
        dfc_CanUseStored :
          Result := DeflateStored(aSource, DestStrm, Helper, Log);
        dfc_CanUseStatic :
          Result := DeflateStaticDynamic(true, false, aSource, DestStrm, Helper, Log);
        dfc_CanUseDynamic :
          Result := DeflateStaticDynamic(false, false, aSource, DestStrm, Helper, Log);
      else
        Result := DeflateStaticDynamic(false, true, aSource, DestStrm, Helper, Log);
      end;

      {save the uncompressed and compressed sizes}
      if (aHelper <> nil) then begin
        aHelper.NormalSize := aSource.Position - SourceStartPos;
        aHelper.CompressedSize := aDest.Position - DestStartPos;
      end;
    except
      on E : EAbInternalDeflateError do begin
        {$IFDEF UseLogging}
        if (Log <> nil) then
          Log.WriteLine(Format('Internal exception raised: %s',
                                [E.Message]));
        {$ENDIF}
        raise EAbDeflateError.Create(E.Message);
      end;
    end;
  finally
    if (Helper.Passphrase <> '') then
      DestStrm.Free;
    Helper.Free;
    Log.Free;
  end;
  {WARNING NOTE: the compiler will warn that the return value of this
                 function might be undefined. However, it is wrong: it
                 has been fooled by the code. If you don't want to see
                 this warning again, enable the DefeatWarnings
                 compiler define in AbDefine.inc.}
end;
{====================================================================}

end.

