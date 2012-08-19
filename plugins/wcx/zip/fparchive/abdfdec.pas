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
{* ABBREVIA: AbDfDec.pas                                 *}
{*********************************************************}
{* Deflate decoding unit                                 *}
{*********************************************************}

unit AbDfDec;

{$I AbDefine.inc}

interface

uses
  Classes,
  AbDfBase;

function Inflate(aSource : TStream; aDest : TStream;
                 aHelper : TAbDeflateHelper) : longint;

implementation

uses
  SysUtils,
  AbDfStrm,
  AbDfHufD,
  AbDfOutW,
  AbDfCryS;


{===Helper routines==================================================}
procedure ReadLitDistCodeLengths(aInStrm      : TAbDfInBitStream;
                                 aCodeLenTree : TAbDfDecodeHuffmanTree;
                             var aCodeLens    : array of integer;
                                 aCount       : integer;
                             var aTotalBits   : integer);
var
  i : integer;
  SymbolCount   : integer;
  LookupValue   : integer;
  EncodedSymbol : longint;
  Symbol        : integer;
  SymbolCodeLen : integer;
  RepeatCount   : integer;
  BitBuffer     : TAb32bit;
  BitCount      : integer;
begin
  {$IFDEF UseLogging}
  {we need to calculate the total number of bits in the code lengths
   for reporting purposes, so zero the count}
  aTotalBits := 0;
  {$ENDIF}

  {clear the code lengths array}
  FillChar(aCodeLens, sizeof(aCodeLens), 0);

  {read all the Symbols required in the bit stream}
  SymbolCount := 0;
  while (SymbolCount < aCount) do begin
    {grab the lookup set of bits}
    BitCount := aCodeLenTree.LookupBitLength + 7;
    {$IFOPT C+}
    BitBuffer := aInStrm.PeekBits(BitCount);
    {$ELSE}
    if (aInStrm.BitsLeft < BitCount) then
      BitBuffer := aInStrm.PeekMoreBits(BitCount)
    else
      BitBuffer := aInStrm.BitBuffer and AbExtractMask[BitCount];
    {$ENDIF}
    LookupValue :=
       BitBuffer and AbExtractMask[aCodeLenTree.LookupBitLength];

    {get the encoded Symbol}
    {$IFOPT C+} {if Assertions are on}
    EncodedSymbol := aCodeLenTree.Decode(LookupValue);
    {$ELSE}
    EncodedSymbol := aCodeLenTree.Decodes^[LookupValue];
    {$ENDIF}

    {extract the data}
    Symbol := EncodedSymbol and $FFFF;
    SymbolCodeLen := (EncodedSymbol shr 16) and $FF;

    {$IFDEF UseLogging}
    {keep count of the total number of bits read}
    inc(aTotalBits, SymbolCodeLen);
    {$ENDIF}

    {check that the symbol is between 0 and 18}
    if not ((0 <= Symbol) and (Symbol <= 18)) then
      raise EAbInternalInflateError.Create(
         'decoded a symbol not between 0 and 18 {ReadLitDistCodeLengths}');

    {check that the codelength is in range}
    if not ((0 < SymbolCodeLen) and
            (SymbolCodeLen <= aCodeLenTree.LookupBitLength)) then
      raise EAbInternalInflateError.Create(
         'decoded a code length out of range {ReadLitDistCodeLengths}');

    {for a Symbol of 0..15, just save the value}
    if (Symbol <= 15) then begin
      aCodeLens[SymbolCount] := Symbol;
      inc(SymbolCount);
      {$IFOPT C+}
      aInStrm.DiscardBits(SymbolCodeLen);
      {$ELSE}
      if (aInStrm.BitsLeft < SymbolCodeLen) then
        aInStrm.DiscardMoreBits(SymbolCodeLen)
      else begin
        aInStrm.BitBuffer := aInStrm.BitBuffer shr SymbolCodeLen;
        aInStrm.BitsLeft := aInStrm.BitsLeft - SymbolCodeLen;
      end;
      {$ENDIF}
    end

    {for a Symbol of 16, get two more bits and copy the previous
     code length that many times + 3}
    else if (Symbol = 16) then begin
      RepeatCount := 3 + ((BitBuffer shr SymbolCodeLen) and $3);
      Symbol := aCodeLens[SymbolCount-1];
      for i := 0 to pred(RepeatCount) do
        aCodeLens[SymbolCount+i] := Symbol;
      inc(SymbolCount, RepeatCount);
      BitCount := SymbolCodeLen + 2;
      {$IFOPT C+}
      aInStrm.DiscardBits(BitCount);
      {$ELSE}
      if (aInStrm.BitsLeft < BitCount) then
        aInStrm.DiscardMoreBits(BitCount)
      else begin
        aInStrm.BitBuffer := aInStrm.BitBuffer shr BitCount;
        aInStrm.BitsLeft := aInStrm.BitsLeft - BitCount;
      end;
      {$ENDIF}
      {$IFDEF UseLogging}
      inc(aTotalBits, 2);
      {$ENDIF}
    end

    {for a Symbol of 17, get three more bits and copy a zero code
     length that many times + 3}
    else if (Symbol = 17) then begin
      RepeatCount := 3 + ((BitBuffer shr SymbolCodeLen) and $7);
      {note: the codelengths array was aet to zeros at the start so
             the following two lines are not needed
      for i := 0 to pred(RepeatCount) do
        aCodeLens[SymbolCount+i] := 0;}
      inc(SymbolCount, RepeatCount);
      BitCount := SymbolCodeLen + 3;
      {$IFOPT C+}
      aInStrm.DiscardBits(BitCount);
      {$ELSE}
      if (aInStrm.BitsLeft < BitCount) then
        aInStrm.DiscardMoreBits(BitCount)
      else begin
        aInStrm.BitBuffer := aInStrm.BitBuffer shr BitCount;
        aInStrm.BitsLeft := aInStrm.BitsLeft - BitCount;
      end;
      {$ENDIF}
      {$IFDEF UseLogging}
      inc(aTotalBits, 3);
      {$ENDIF}
    end

    {for a Symbol of 18, get seven more bits and copy a zero code
     length that many times + 11}
    else if (Symbol = 18) then begin
      RepeatCount := 11 + ((BitBuffer shr SymbolCodeLen) and $7F);
      {note: the codelengths array was aet to zeros at the start so
             the following two lines are not needed
      for i := 0 to pred(RepeatCount) do
        aCodeLens[SymbolCount+i] := 0;}
      inc(SymbolCount, RepeatCount);
      BitCount := SymbolCodeLen + 7;
      {$IFOPT C+}
      aInStrm.DiscardBits(BitCount);
      {$ELSE}
      if (aInStrm.BitsLeft < BitCount) then
        aInStrm.DiscardMoreBits(BitCount)
      else begin
        aInStrm.BitBuffer := aInStrm.BitBuffer shr BitCount;
        aInStrm.BitsLeft := aInStrm.BitsLeft - BitCount;
      end;
      {$ENDIF}
      {$IFDEF UseLogging}
      inc(aTotalBits, 7);
      {$ENDIF}
    end;
  end;
end;
{--------}
procedure DecodeData(aInStrm       : TAbDfInBitStream;
                     aOutWindow    : TAbDfOutputWindow;
                     aLiteralTree  : TAbDfDecodeHuffmanTree;
                     aDistanceTree : TAbDfDecodeHuffmanTree;
                     aDeflate64    : boolean);
var
  LookupValue   : integer;
  EncodedSymbol : longint;
  Symbol        : integer;
  SymbolCodeLen : integer;
  ExtraBitCount : integer;
  Length        : integer;
  Distance      : integer;
  BitBuffer     : TAb32bit;
  BitCount      : integer;
begin
  {extract the first symbol (it's got to be a literal/length symbol)}
  {..grab the lookup set of bits}
  if aDeflate64 then
    BitCount := aLiteralTree.LookupBitLength + 16
  else
    BitCount := aLiteralTree.LookupBitLength + 5;
  {$IFOPT C+}
  BitBuffer := aInStrm.PeekBits(BitCount);
  {$ELSE}
  if (aInStrm.BitsLeft < BitCount) then
    BitBuffer := aInStrm.PeekMoreBits(BitCount)
  else
    BitBuffer := aInStrm.BitBuffer and AbExtractMask[BitCount];
  {$ENDIF}
  LookupValue :=
     BitBuffer and AbExtractMask[aLiteralTree.LookupBitLength];
  {..get the encoded symbol}
  {$IFOPT C+} {if Assertions are on}
  EncodedSymbol := aLiteralTree.Decode(LookupValue);
  {$ELSE}
  EncodedSymbol := aLiteralTree.Decodes^[LookupValue];
  {$ENDIF}
  {..extract the data}
  Symbol := EncodedSymbol and $FFFF;
  SymbolCodeLen := (EncodedSymbol shr 16) and $FF;
//  ExtraBitCount := EncodedSymbol shr 24;

  {repeat until we get the end-of-block symbol}
  while ((Symbol <> 256) {and (ExtraBitCount <> 15)}) do begin
    {for a literal, just output it to the sliding window}
    if (Symbol < 256) then begin
      aOutWindow.AddLiteral(AnsiChar(Symbol));
      {$IFOPT C+}
      aInStrm.DiscardBits(SymbolCodeLen);
      {$ELSE}
      if (aInStrm.BitsLeft < SymbolCodeLen) then
        aInStrm.DiscardMoreBits(SymbolCodeLen)
      else begin
        aInStrm.BitBuffer := aInStrm.BitBuffer shr SymbolCodeLen;
        aInStrm.BitsLeft := aInStrm.BitsLeft - SymbolCodeLen;
      end;
      {$ENDIF}
    end

    {for a length value, we need to get any extra bits, and then the
     distance (plus any extra bits for that), and then add the
     duplicated characters to the sliding window}
    else begin

      {check that the length symbol is less than or equal to 285}
      if (Symbol > 285) then
        raise EAbInternalInflateError.Create(
           'decoded an invalid length symbol: greater than 285 [DecodeData]');

      {calculate the length (if need be, by calculating the number of
       extra bits that encode the length)}
      if (not aDeflate64) and (Symbol = 285) then begin
        Length := 258;
        {$IFOPT C+}
        aInStrm.DiscardBits(SymbolCodeLen);
        {$ELSE}
        if (aInStrm.BitsLeft < SymbolCodeLen) then
          aInStrm.DiscardMoreBits(SymbolCodeLen)
        else begin
          aInStrm.BitBuffer := aInStrm.BitBuffer shr SymbolCodeLen;
          aInStrm.BitsLeft := aInStrm.BitsLeft - SymbolCodeLen;
        end;
        {$ENDIF}
      end
      else begin
        ExtraBitCount := EncodedSymbol shr 24;
        if (ExtraBitCount = 0) then begin
          Length := dfc_LengthBase[Symbol - 257];
          {$IFOPT C+}
          aInStrm.DiscardBits(SymbolCodeLen);
          {$ELSE}
          if (aInStrm.BitsLeft < SymbolCodeLen) then
            aInStrm.DiscardMoreBits(SymbolCodeLen)
          else begin
            aInStrm.BitBuffer := aInStrm.BitBuffer shr SymbolCodeLen;
            aInStrm.BitsLeft := aInStrm.BitsLeft - SymbolCodeLen;
          end;
          {$ENDIF}
        end
        else begin
          Length := dfc_LengthBase[Symbol - 257] +
                    ((BitBuffer shr SymbolCodeLen) and
                     AbExtractMask[ExtraBitCount]);
          BitCount := SymbolCodeLen + ExtraBitCount;
          {$IFOPT C+}
          aInStrm.DiscardBits(BitCount);
          {$ELSE}
          if (aInStrm.BitsLeft < BitCount) then
            aInStrm.DiscardMoreBits(BitCount)
          else begin
            aInStrm.BitBuffer := aInStrm.BitBuffer shr BitCount;
            aInStrm.BitsLeft := aInStrm.BitsLeft - BitCount;
          end;
          {$ENDIF}
        end;
      end;

      {extract the distance}
      {..grab the lookup set of bits}
      BitCount := aDistanceTree.LookupBitLength + 14;
      {$IFOPT C+}
      BitBuffer := aInStrm.PeekBits(BitCount);
      {$ELSE}
      if (aInStrm.BitsLeft < BitCount) then
        BitBuffer := aInStrm.PeekMoreBits(BitCount)
      else
        BitBuffer := aInStrm.BitBuffer and AbExtractMask[BitCount];
      {$ENDIF}
      LookupValue :=
         BitBuffer and AbExtractMask[aDistanceTree.LookupBitLength];
      {..get the encoded symbol}
      {$IFOPT C+} {if Assertions are on}
      EncodedSymbol := aDistanceTree.Decode(LookupValue);
      {$ELSE}
      EncodedSymbol := aDistanceTree.Decodes^[LookupValue];
      {$ENDIF}
      {..extract the data}
      Symbol := EncodedSymbol and $FFFF;
      SymbolCodeLen := (EncodedSymbol shr 16) and $FF;

      {check that the distance symbol is less than or equal to 29}
      if (not aDeflate64) and (Symbol > 29) then
        raise EAbInternalInflateError.Create(
           'decoded an invalid distance symbol: greater than 29 [DecodeData]');

      {..calculate the extra bits for the distance}
      ExtraBitCount := EncodedSymbol shr 24;
      {..calculate the distance}
      if (ExtraBitCount = 0) then begin
        Distance := dfc_DistanceBase[Symbol];
        {$IFOPT C+}
        aInStrm.DiscardBits(SymbolCodeLen);
        {$ELSE}
        if (aInStrm.BitsLeft < SymbolCodeLen) then
          aInStrm.DiscardMoreBits(SymbolCodeLen)
        else begin
          aInStrm.BitBuffer := aInStrm.BitBuffer shr SymbolCodeLen;
          aInStrm.BitsLeft := aInStrm.BitsLeft - SymbolCodeLen;
        end;
        {$ENDIF}
      end
      else begin
        Distance := dfc_DistanceBase[Symbol] +
                    ((BitBuffer shr SymbolCodeLen) and
                     AbExtractMask[ExtraBitCount]);
        BitCount := SymbolCodeLen + ExtraBitCount;
        {$IFOPT C+}
        aInStrm.DiscardBits(BitCount);
        {$ELSE}
        if (aInStrm.BitsLeft < BitCount) then
          aInStrm.DiscardMoreBits(BitCount)
        else begin
          aInStrm.BitBuffer := aInStrm.BitBuffer shr BitCount;
          aInStrm.BitsLeft := aInStrm.BitsLeft - BitCount;
        end;
        {$ENDIF}
      end;

      {duplicate the characters in the sliding window}
      aOutWindow.AddLenDist(Length, Distance);
    end;

    {extract the next symbol}
    {..grab the lookup set of bits}
    if aDeflate64 then
      BitCount := aLiteralTree.LookupBitLength + 16
    else
      BitCount := aLiteralTree.LookupBitLength + 5;
    {$IFOPT C+}
    BitBuffer := aInStrm.PeekBits(BitCount);
    {$ELSE}
    if (aInStrm.BitsLeft < BitCount) then
      BitBuffer := aInStrm.PeekMoreBits(BitCount)
    else
      BitBuffer := aInStrm.BitBuffer and AbExtractMask[BitCount];
    {$ENDIF}
    LookupValue :=
       BitBuffer and AbExtractMask[aLiteralTree.LookupBitLength];
    {..get the encoded symbol}
    {$IFOPT C+} {if Assertions are on}
    EncodedSymbol := aLiteralTree.Decode(LookupValue);
    {$ELSE}
    EncodedSymbol := aLiteralTree.Decodes^[LookupValue];
    {$ENDIF}
    {..extract the data}
    Symbol := EncodedSymbol and $FFFF;
    SymbolCodeLen := (EncodedSymbol shr 16) and $FF;
  end;

  {discard the bits for the end-of-block marker}
  {$IFOPT C+}
  aInStrm.DiscardBits(SymbolCodeLen);
  {$ELSE}
  if (aInStrm.BitsLeft < SymbolCodeLen) then
    aInStrm.DiscardMoreBits(SymbolCodeLen)
  else begin
    aInStrm.BitBuffer := aInStrm.BitBuffer shr SymbolCodeLen;
    aInStrm.BitsLeft := aInStrm.BitsLeft - SymbolCodeLen;
  end;
  {$ENDIF}
end;
{--------}
procedure InflateStoredBlock(aInStrm    : TAbDfInBitStream;
                             aOutWindow : TAbDfOutputWindow;
                             aLog       : TAbLogger);
const
  BufferSize = 16 * 1024;
var
  LenNotLen : packed record
    Len    : word;
    NotLen : word;
  end;
  BytesToGo    : integer;
  BytesToWrite : integer;
  Buffer       : pointer;
begin
  {$IFDEF UseLogging}
  {log it}
  if (aLog <> nil) then
    aLog.WriteLine('....a stored block');
  {$ENDIF}

  {align the input bit stream to the nearest byte boundary}
  aInStrm.AlignToByte;

  {read the length of the stored data and the notted length}
  aInStrm.ReadBuffer(LenNotLen, sizeof(LenNotLen));

  {$IFDEF UseLogging}
  {log it}
  if (aLog <> nil) then
    aLog.WriteLine(Format('..block length: %d (%-4x, NOT %-4x)',
                          [LenNotLen.Len, LenNotLen.Len, LenNotLen.NotLen]));
  {$ENDIF}

  {check that NOT of the length equals the notted length}
  if ((not LenNotLen.Len) <> LenNotLen.NotLen) then
    raise EAbInternalInflateError.Create(
       'invalid stored block (length and NOT length do not match) [InflateStoredBlock]');

  {calculate the number of bytes to copy from the stored block}
  BytesToGo := LenNotLen.Len;

  {allocate a large buffer}
  GetMem(Buffer, BufferSize);

  {copy all the data in the stored block to the output window}
  try
    {while there are still some bytes to copy...}
    while (BytesToGo <> 0) do begin
      {calculate the number of bytes this time}
      if (BytesToGo > BufferSize) then
        BytesToWrite := BufferSize
      else
        BytesToWrite := BytesToGo;

      {read that many bytes and write them to the output window}
      aInStrm.ReadBuffer(Buffer^, BytesToWrite);
      aOutWindow.AddBuffer(Buffer^, BytesToWrite);

      {calculate the number of bytes still to copy}
      dec(BytesToGo, BytesToWrite);
    end;
  finally
    FreeMem(Buffer);
  end;
end;
{--------}
procedure InflateStaticBlock(aInStrm    : TAbDfInBitStream;
                             aOutWindow : TAbDfOutputWindow;
                             aLog       : TAbLogger;
                             aDeflate64 : boolean);
begin
  {$IFDEF UseLogging}
  {log it}
  if (aLog <> nil) then
    aLog.WriteLine('....a static huffman tree block');
  {$ENDIF}

  {decode the data with the static trees}
  DecodeData(aInStrm, aOutWindow,
             AbStaticLiteralTree, AbStaticDistanceTree, aDeflate64);
end;
{--------}
procedure InflateDynamicBlock(aInStrm    : TAbDfInBitStream;
                              aOutWindow : TAbDfOutputWindow;
                              aLog       : TAbLogger;
                              aDeflate64 : boolean);
var
  i : integer;
  LitCount      : integer;
  DistCount     : integer;
  CodeLenCount  : integer;
  CodeLens      : array [0..285+32] of integer;
  CodeLenTree   : TAbDfDecodeHuffmanTree;
  LiteralTree   : TAbDfDecodeHuffmanTree;
  DistanceTree  : TAbDfDecodeHuffmanTree;
  TotalBits     : integer;
begin
  {$IFDEF UseLogging}
  {log it}
  if (aLog <> nil) then
    aLog.WriteLine('....a dynamic huffman tree block');
  {$ENDIF}

  {prepare for the try..finally}
  CodeLenTree := nil;
  LiteralTree := nil;
  DistanceTree := nil;

  try
    {decode the number of literal, distance and codelength codes}
    LitCount := aInStrm.ReadBits(5) + 257;
    DistCount := aInStrm.ReadBits(5) + 1;
    CodeLenCount := aInStrm.ReadBits(4) + 4;

    {$IFDEF UseLogging}
    {log it}
    if (aLog <> nil) then begin
      aLog.WriteLine(Format('Count of literals:     %d', [LitCount]));
      aLog.WriteLine(Format('Count of distances:    %d', [DistCount]));
      aLog.WriteLine(Format('Count of code lengths: %d', [CodeLenCount]));
    end;
    {$ENDIF}

    {verify that the counts are valid}
    if (LitCount > 286) then
      raise EAbInternalInflateError.Create(
         'count of literal codes in dynamic block is greater than 286 [InflateDynamicBlock]');
    if (not aDeflate64) and (DistCount > 30) then
      raise EAbInternalInflateError.Create(
         'count of distance codes in dynamic block is greater than 30 [InflateDynamicBlock]');

    {read the codelengths}
    FillChar(CodeLens, 19 * sizeof(integer), 0);
    for i := 0 to pred(CodeLenCount) do
      CodeLens[dfc_CodeLengthIndex[i]] := aInStrm.ReadBits(3);

    {$IFDEF UseLogging}
    {log them}
    if (aLog <> nil) then begin
      aLog.WriteLine('CodeLength Huffman tree: code lengths');
      for i := 0 to 18 do
        aLog.WriteStr(Format('%-3d', [CodeLens[i]]));
      aLog.WriteLine('');
      aLog.WriteLine(Format('..total bits: %d', [CodeLenCount * 3]));
    end;
    {$ENDIF}

    {create the codelength huffman tree}
    CodeLenTree := TAbDfDecodeHuffmanTree.Create(19, 7, huDecoding);
    CodeLenTree.Build(CodeLens, 0, 19, [0], $FFFF);

    {$IFDEF UseLogging}
    {log the tree}
    if (aLog <> nil) then begin
      aLog.WriteLine('Code lengths tree');
      CodeLenTree.DebugPrint(aLog);
    end;
    {$ENDIF}

    {read the codelengths for both the literal/length and distance
     huffman trees}
    ReadLitDistCodeLengths(aInStrm, CodeLenTree, CodeLens,
                           LitCount + DistCount, TotalBits);

    {$IFDEF UseLoggingx}
    {log them}
    if (aLog <> nil) then begin
      aLog.WriteLine('Literal/length & Dist Huffman trees: code lengths');
      for i := 0 to pred(LitCount + DistCount) do
        aLog.WriteLine(Format('%3d: %3d', [i, CodeLens[i]]));
      aLog.WriteLine('');
      aLog.WriteLine(Format('..total bits: %d', [TotalBits]));
    end;
    {$ENDIF}

    {create the literal huffman tree}
    LiteralTree := TAbDfDecodeHuffmanTree.Create(286, 15, huDecoding);
    LiteralTree.Build(CodeLens, 0, LitCount,
                      dfc_LitExtraBits, dfc_LitExtraOffset);

    {$IFDEF UseLogging}
    {log the tree}
    if (aLog <> nil) then begin
      aLog.WriteLine('Literal/length tree');
      LiteralTree.DebugPrint(aLog);
    end;
    {$ENDIF}

    {create the distance huffman tree}
    if aDeflate64 then
      DistanceTree := TAbDfDecodeHuffmanTree.Create(32, 15, huDecoding)
    else
      DistanceTree := TAbDfDecodeHuffmanTree.Create(30, 15, huDecoding);
    DistanceTree.Build(CodeLens, LitCount, DistCount,
                       dfc_DistExtraBits, dfc_DistExtraOffset);

    {$IFDEF UseLogging}
    {log the tree}
    if (aLog <> nil) then begin
      aLog.WriteLine('Distance tree');
      DistanceTree.DebugPrint(aLog);
    end;
    {$ENDIF}

    {using the literal and distance trees, decode the bit stream}
    DecodeData(aInStrm, aOutWindow,
               LiteralTree, DistanceTree, aDeflate64);
  finally
    CodeLenTree.Free;
    LiteralTree.Free;
    DistanceTree.Free;
  end;
end;
{====================================================================}


{===Interfaced routine===============================================}
function Inflate(aSource : TStream; aDest : TStream;
                 aHelper : TAbDeflateHelper) : longint;
var
  Helper       : TAbDeflateHelper;
  InBitStrm    : TAbDfInBitStream;
  OutWindow    : TAbDfOutputWindow;
  Log          : TAbLogger;
  UseDeflate64 : boolean;
  UseCRC32     : boolean;
  IsFinalBlock : boolean;
  BlockType    : integer;
  TestOnly     : boolean;
  SourceStartPos : longint;
  DestStartPos   : longint;
  {$IFDEF UseLogging}
  StartPosn    : longint;
  {$ENDIF}
begin
  {$IFDEF DefeatWarnings}
  Result := 0;
  SourceStartPos := 0;
  DestStartPos := 0;
  TestOnly := False;
  {$ENDIF}

  {$IFDEF UseLogging}
  StartPosn := 0;
  {$ENDIF}

  {pre-conditions: streams must be allocated of course}
  Assert(aSource <> nil, 'Deflate: aSource stream cannot be nil');
  Assert(aDest <> nil, 'Deflate: aDest stream cannot be nil');

  {prepare for the try..finally}
  Helper := nil;
  InBitStrm := nil;
  OutWindow := nil;
  Log := nil;

  try {finally}
    try {except}
      {create our helper; assign the passed one to it}
      Helper := TAbDeflateHelper.Create;
      if (aHelper <> nil) then
        Helper.Assign(aHelper);

      {get the initial start positions of both streams}
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

      {create the logger, if requested}
      if (Helper.LogFile <> '') then begin
        Log := TAbLogger.Create(Helper.LogFile);
        Log.WriteLine('INFLATING STREAM...');
        {$IFNDEF UseLogging}
        Log.WriteLine('Need to recompile the app with UseLogging turned on');
        {$ENDIF}
      end;

      InBitStrm := TAbDfInBitStream.Create(aSource,
                                           Helper.OnProgressStep,
                                           Helper.StreamSize);

      {create the output sliding window}
      UseDeflate64 := (Helper.Options and dfc_UseDeflate64) <> 0;
      UseCRC32 := (Helper.Options and dfc_UseAdler32) = 0;
      TestOnly := (Helper.Options and dfc_TestOnly) <> 0;
      OutWindow := TAbDfOutputWindow.Create(
              aDest, UseDeflate64, UseCRC32, Helper.PartialSize,
              TestOnly, Log);

      {start decoding the deflated stream}
      repeat
        {read the final block flag and the block type}
        IsFinalBlock := InBitStrm.ReadBit;
        BlockType := InBitStrm.ReadBits(2);

        {$IFDEF UseLogging}
        {log it}
        if (Log <> nil) then begin
          Log.WriteLine('');
          Log.WriteLine('Starting new block');
          Log.WriteLine(Format('..final block? %d', [ord(IsFinalBlock)]));
          Log.WriteLine(Format('..block type? %d', [BlockType]));
          StartPosn := OutWindow.Position;
        end;
        {$ENDIF}

        case BlockType of
          0 : InflateStoredBlock(InBitStrm, OutWindow, Log);
          1 : InflateStaticBlock(InBitStrm, OutWindow, Log, UseDeflate64);
          2 : InflateDynamicBlock(InBitStrm, OutWindow, Log, UseDeflate64);
        else
          raise EAbInternalInflateError.Create(
             'starting new block, but invalid block type [Inflate]');
        end;

        {$IFDEF UseLogging}
        {log it}
        if (Log <> nil) then
          Log.WriteLine(Format('---block end---  (decoded size %d bytes)',
                               [OutWindow.Position - StartPosn]));
        {$ENDIF}
      until IsFinalBlock;

      {get the uncompressed stream's checksum}
      Result := OutWindow.Checksum;
      if TestOnly and (aHelper <> nil) then
        aHelper.NormalSize := OutWindow.Position;
      {$IFDEF UseLogging}
      {log it}
      if (Log <> nil) then
        Log.WriteLine(Format('End of compressed stream, checksum %-8x',
                             [Result]));
      {$ENDIF}
    except
      on E : EAbPartSizedInflate do begin
        {nothing, just swallow the exception}
        Result := 0;
      end;
      on E : EAbAbortProgress do begin
        {nothing, just swallow the exception}
        Result := 0;
      end;
      on E : EAbInternalInflateError do begin
        if (Log <> nil) then
          Log.WriteLine(Format('Internal exception raised: %s',
                                [E.Message]));
        raise EAbInflateError.Create(E.Message);
      end;
    end;
  finally
    Helper.Free;
    OutWindow.Free;
    InBitStrm.Free;
    Log.Free;
  end;

  {if there's a helper return the compressed and uncompressed sizes}
  if (aHelper <> nil) then begin
    if not TestOnly then
      aHelper.NormalSize := aDest.Position - DestStartPos;
    aHelper.CompressedSize := aSource.Position - SourceStartPos;
  end;

  {WARNING NOTE: the compiler will warn that the return value of this
                 function might be undefined. However, it is wrong: it
                 has been fooled by the code. If you don't want to see
                 this warning again, enable the DefeatWarnings
                 compiler define in AbDefine.inc.}
end;
{====================================================================}

end.
