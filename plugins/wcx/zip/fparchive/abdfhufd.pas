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
{* ABBREVIA: AbDfHufD.pas                                *}
{*********************************************************}
{* Deflate Huffman tree for decoder                      *}
{*********************************************************}

unit AbDfHufD;

{$I AbDefine.inc}

{Activate this compiler define and rebuild if you want the complete
 huffman tree output to print to the current log. The output is
 voluminous to say the least...}
{$IFDEF UseLogging}
{.$DEFINE EnableMegaLog}
{$ENDIF}

{Notes:

The object of this class is to build a decoder array, not to build a
Huffman tree particularly. We don't want to decode huffman strings bit
by bit. moving down the Huffman tree sometimes left, sometimes right.
Instead we want to grab a set of bits and look them up in an array.
Sometimes we'll grab too many bits, sure, but we can deal with that
later. So, the object of the exercise is to calculate the code for a
symbol, reverse it ('cos that's how the input bit stream will present
it to us) and set that element of the array to the decoded symbol
value (plus some extra information: bit lengths).

If the alphabet size were 19 (the codelengths huffman tree) and the
maximum code length 5, for example, the decoder array would be 2^5
elements long, much larger than the alphabet size. The user of this
class will be presenting sets of 5 bits for us to decode. We would
like to look up these 5 bits in the array (as an index) and have the
symbol returned. Now, since the alphabet size is much less than the
number of elements in the decoder array, we must set the other
elements in the array as well. Consider a symbol that has a code of
110 in this scenario. The reversed code is 011, or 3, so we'd be
setting element 3. However we should also be setting elements 01011,
10011, and 11011 to this symbol information as well, since the lookup
will be 5 bits long.

Because the code is a huffman code from a prefix tree, we won't get
any index clashes between actual codes by this "filling in" process.

For the codelength Huffman tree, the maximum code length is at most 7.
This equates to a 128 element array. For the literal and distance
trees, the max code length is at most 15. This equates to a 32768
element array.

For a given lookup value the decoder will return a 32-bit value. The
lower 16 bits is the decoded symbol, the next 8 bits is the code
length for that symbol, the last 8 bits (the most significant) are the
number of extra bits that must be extracted from the input bit stream.
}

interface

uses
  AbDfBase;

type
  TAbDfHuffmanUsage = (   {usage of a huffman decoder..}
       huEncoding,        {..encoding}
       huDecoding,        {..decoding}
       huBoth);           {..both (used for static trees)}

  TAbDfDecodeHuffmanTree = class
    private
      FAlphaSize     : integer;
      FDecodes       : PAbDfLongintList;
      FDefMaxCodeLen : integer;
      FEncodes       : PAbDfLongintList;
      {$IFOPT C+}
      FMask          : integer;
      {$ENDIF}
      FMaxCodeLen    : integer;
      FUsage         : TAbDfHuffmanUsage;
    protected
    public
      constructor Create(aAlphabetSize : integer;
                         aDefMaxCodeLen: integer;
                         aUsage        : TAbDfHuffmanUsage);
      destructor Destroy; override;

      procedure Build(const aCodeLengths : array of integer;
                            aStartInx    : integer;
                            aCount       : integer;
                      const aExtraBits   : array of byte;
                            aExtraOffset : integer);
      function Decode(aLookupBits : integer) : longint;
      function Encode(aSymbol : integer) : longint;

      {$IFDEF UseLogging}
      procedure DebugPrint(aLog : TAbLogger);
      {$ENDIF}

      property LookupBitLength : integer read FMaxCodeLen;
      property Decodes : PAbDfLongintList read FDecodes;
      property Encodes : PAbDfLongintList read FEncodes;
  end;

var
  AbStaticLiteralTree  : TAbDfDecodeHuffmanTree;
  AbStaticDistanceTree : TAbDfDecodeHuffmanTree;

implementation

uses
  SysUtils;

const
  PowerOfTwo : array [0..dfc_MaxCodeLength] of integer =
               (1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048,
                4096, 8192, 16384, 32768);

{===Debug helper routine=============================================}
{$IFDEF EnableMegaLog}
function CodeToStr(aCode : longint; aLen : integer) : string;
var
  i : integer;
begin
  if (aLen = 0) then
    Result := 'no code'
  else begin
    SetLength(Result, 32);
    FillChar(Result[1], 32, ' ');
    for i := 32 downto (33-aLen) do begin
      if Odd(aCode) then
        Result[i] := '1'
      else
        Result[i] := '0';
      aCode := aCode shr 1;
    end;
  end;
end;
{$ENDIF}
{====================================================================}


{===TAbDfDecodeHuffmanTree===========================================}
constructor TAbDfDecodeHuffmanTree.Create(
                                 aAlphabetSize : integer;
                                 aDefMaxCodeLen: integer;
                                 aUsage        : TAbDfHuffmanUsage);
begin
  {protect against dumb programming mistakes}
  Assert(aAlphabetSize >= 2,
         'TAbDfDecodeHuffmanTree.Create: a huffman tree must be for at least two symbols');

  {let the ancestor initialize}
  inherited Create;

  {save the alphabet size, etc}
  FAlphaSize := aAlphabetSize;
  FDefMaxCodeLen := aDefMaxCodeLen;
  FUsage := aUsage;

  {allocate the encoder array (needs to be initialized to zeros)}
  if (aUsage <> huDecoding) then
    FEncodes := AllocMem(FAlphaSize * sizeof(longint));
end;
{--------}
destructor TAbDfDecodeHuffmanTree.Destroy;
begin
  {destroy the codes arrays}
  if (FDecodes <> nil) then
    FreeMem(FDecodes);
  if (FEncodes <> nil) then
    FreeMem(FEncodes);

  {let the ancestor die}
  inherited Destroy;
end;
{--------}
procedure TAbDfDecodeHuffmanTree.Build(
                                const aCodeLengths : array of integer;
                                      aStartInx    : integer;
                                      aCount       : integer;
                                const aExtraBits   : array of byte;
                                      aExtraOffset : integer);
const
  ByteRevTable : array [0..255] of byte = (
   $00, $80, $40, $C0, $20, $A0, $60, $E0, $10, $90, $50, $D0,
   $30, $B0, $70, $F0, $08, $88, $48, $C8, $28, $A8, $68, $E8,
   $18, $98, $58, $D8, $38, $B8, $78, $F8, $04, $84, $44, $C4,
   $24, $A4, $64, $E4, $14, $94, $54, $D4, $34, $B4, $74, $F4,
   $0C, $8C, $4C, $CC, $2C, $AC, $6C, $EC, $1C, $9C, $5C, $DC,
   $3C, $BC, $7C, $FC, $02, $82, $42, $C2, $22, $A2, $62, $E2,
   $12, $92, $52, $D2, $32, $B2, $72, $F2, $0A, $8A, $4A, $CA,
   $2A, $AA, $6A, $EA, $1A, $9A, $5A, $DA, $3A, $BA, $7A, $FA,
   $06, $86, $46, $C6, $26, $A6, $66, $E6, $16, $96, $56, $D6,
   $36, $B6, $76, $F6, $0E, $8E, $4E, $CE, $2E, $AE, $6E, $EE,
   $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE, $01, $81, $41, $C1,
   $21, $A1, $61, $E1, $11, $91, $51, $D1, $31, $B1, $71, $F1,
   $09, $89, $49, $C9, $29, $A9, $69, $E9, $19, $99, $59, $D9,
   $39, $B9, $79, $F9, $05, $85, $45, $C5, $25, $A5, $65, $E5,
   $15, $95, $55, $D5, $35, $B5, $75, $F5, $0D, $8D, $4D, $CD,
   $2D, $AD, $6D, $ED, $1D, $9D, $5D, $DD, $3D, $BD, $7D, $FD,
   $03, $83, $43, $C3, $23, $A3, $63, $E3, $13, $93, $53, $D3,
   $33, $B3, $73, $F3, $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB,
   $1B, $9B, $5B, $DB, $3B, $BB, $7B, $FB, $07, $87, $47, $C7,
   $27, $A7, $67, $E7, $17, $97, $57, $D7, $37, $B7, $77, $F7,
   $0F, $8F, $4F, $CF, $2F, $AF, $6F, $EF, $1F, $9F, $5F, $DF,
   $3F, $BF, $7F, $FF);
var
  i : integer;
  Symbol      : integer;
  LengthCount : array [0..dfc_MaxCodeLength] of integer;
  NextCode    : array [0..dfc_MaxCodeLength] of integer;
  Code        : longint;
  CodeLen     : integer;
  CodeData    : longint;
  DecoderLen  : integer;
  CodeIncr    : integer;
  Decodes     : PAbDfLongintList;
  Encodes     : PAbDfLongintList;
  {$IFDEF CPU386}
  DecodesEnd  : pointer;
  {$ENDIF}
  TablePtr    : pointer;
begin
  {count the number of instances of each code length and calculate the
   maximum code length at the same time}
  FillChar(LengthCount, sizeof(LengthCount), 0);
  FMaxCodeLen := 0;
  for i := 0 to pred(aCount) do begin
    CodeLen := aCodeLengths[i + aStartInx];
    Assert((CodeLen <= FDefMaxCodeLen),
           Format('TAbDfDecodeHuffmanTree.Build: a code length is greater than %d',
                  [FDefMaxCodeLen]));
    if (CodeLen > FMaxCodeLen) then
      FMaxCodeLen := CodeLen;
    inc(LengthCount[CodeLen]);
  end;

  {now we know the maximum code length we can allocate our decoder
   array}
  {$IFNDEF CPU386}
  DecoderLen := 0;
  {$ENDIF}
  if (FUsage <> huEncoding) then begin
    DecoderLen := PowerOfTwo[FMaxCodeLen];
    GetMem(FDecodes, DecoderLen * sizeof(longint));
    {$IFDEF CPU386}
    DecodesEnd := PAnsiChar(FDecodes) + (DecoderLen * sizeof(longint));
    {$ENDIF}
    {$IFOPT C+}
    FillChar(FDecodes^, DecoderLen * sizeof(longint), $FF);
    FMask := not (DecoderLen - 1);
    {$ENDIF}
  end;

  {calculate the start codes for each code length}
  Code := 0;
  LengthCount[0] := 0;
  for i := 1 to FDefMaxCodeLen do begin
    Code := (Code + LengthCount[i-1]) shl 1;
    NextCode[i] := Code;
  end;

  {for speed and convenience}
  Decodes := FDecodes;
  Encodes := FEncodes;
  TablePtr := @ByteRevTable;

  {for each symbol...}
  for Symbol := 0 to pred(aCount) do begin
    {calculate the code length}
    CodeLen := aCodeLengths[Symbol + aStartInx];

    {if the code length were zero, just set the relevant entry in the
     encoder array; the decoder array doesn't need anything}
    if (CodeLen = 0) then begin
      if (FUsage <> huDecoding) then
        Encodes^[Symbol] := -1
    end

    {otherwise we need to fill elements in both the encoder and
     decoder arrays}
    else begin
      {calculate *reversed* code}
      Code := NextCode[CodeLen];
      {$IFDEF CPU386}
      asm
        push esi
        mov eax, Code
        mov esi, TablePtr
        xor ecx, ecx
        xor edx, edx
        mov cl, ah
        mov dl, al
        mov al, [esi+ecx]
        mov ah, [esi+edx]
        mov ecx, 16
        pop esi
        sub ecx, CodeLen
        shr eax, cl
        mov Code, eax
      end;
      {$ELSE}
      CodeData:= Code;
      LongRec(Code).Bytes[1]:= ByteRevTable[LongRec(CodeData).Bytes[0]];
      LongRec(Code).Bytes[0]:= ByteRevTable[LongRec(CodeData).Bytes[1]];
      Code:= Code shr (16-CodeLen);
      {$ENDIF}

      {set the code data (bit count, extra bits required, symbol),
       everywhere the reversed code would appear in the decoder array;
       set the code data in the encoder array as well}
      if (Symbol >= aExtraOffset) then begin
        if (FUsage <> huEncoding) then
          CodeData := Symbol +                  { symbol}
                      (CodeLen shl 16) +        { code length}
                      (aExtraBits[Symbol-aExtraOffset] shl 24);
                                                { extra bits required}
        if (FUsage <> huDecoding) then
          Encodes^[Symbol] := Code +            { code}
                      (CodeLen shl 16) +        { code length}
                      (aExtraBits[Symbol-aExtraOffset] shl 24)
                                                { extra bits required}
      end
      else begin
        if (FUsage <> huEncoding) then
          CodeData := Symbol +                  { symbol}
                      (CodeLen shl 16);         { code length}
        if (FUsage <> huDecoding) then
          Encodes^[Symbol] := Code +            { code}
                      (CodeLen shl 16);         { code length}
      end;

      {OPTIMIZATION NOTE: the following code

            CodeIncr := PowerOfTwo[CodeLen];
            while Code < DecoderLen do begin
              Decodes^[Code] := CodeData;
              inc(Code, CodeIncr);
            end;

      was replaced by the asm code below to improve the speed. The
      code in the loop is the big time sink in this routine so it was
      best to replace it.}
      if (FUsage <> huEncoding) then begin
        {$IFDEF CPU386}
        CodeIncr := PowerOfTwo[CodeLen] * sizeof(longint);
        asm
          push edi                { save edi}
          mov eax, Decodes        { get the Decodes array}
          mov edi, DecodesEnd     { get the end of the Decodes array}
          mov edx, Code           { get Code and..}
          shl edx, 1              { ..multiply by 4}
          shl edx, 1
          add eax, edx            { eax => first element to be set}
          mov edx, CodeData       { get the CodeData}
          mov ecx, CodeIncr       { get the increment per loop}
        @@1:
          mov [eax], edx          { set the element}
          add eax, ecx            { move to the next element}
          cmp eax, edi            { if we haven't gone past the end..}
          jl @@1                  { ..go back for the next one}
          pop edi                 { retrieve edi}
        end;
        {$ELSE}
        CodeIncr := PowerOfTwo[CodeLen];
        while Code < DecoderLen do begin
          Decodes^[Code] := CodeData;
          inc(Code, CodeIncr);
        end;
        {$ENDIF}
      end;

      {we've used this code up for this symbol, so increment for the
       next symbol at this code length}
      inc(NextCode[CodeLen]);
    end;
  end;
end;
{--------}
{$IFDEF UseLogging}
procedure TAbDfDecodeHuffmanTree.DebugPrint(aLog : TAbLogger);
{$IFDEF EnableMegaLog}
var
  i : integer;
  Code : longint;
{$ENDIF}
begin
  {to print the huffman tree, we must have a logger...}
  Assert(aLog <> nil,
         'TAbDfDecodeHuffmanTree.DebugPrint needs a logger object to which to print');

  if (FUsage <> huEncoding) then begin
    aLog.WriteLine('Huffman decoder array');
    aLog.WriteLine(Format('Alphabet size:  %d', [FAlphaSize]));
    aLog.WriteLine(Format('Max codelength: %d', [FMaxCodeLen]));

    {$IFDEF EnableMegaLog}
    aLog.WriteLine('Index Len Xtra Symbol                    Reversed Code');
    for i := 0 to pred(PowerOfTwo[FMaxCodeLen]) do begin
      Code := FDecodes^[i];
      if (Code = -1) then
        aLog.WriteLine(Format('%5d%49s', [i, 'no code']))
      else
        aLog.WriteLine(Format('%5d%4d%5d%7d%33s',
                       [i,
                        ((Code shr 16) and $FF),
                        ((Code shr 24) and $FF),
                        (Code and $FFFF),
                        CodeToStr(i, ((Code shr 16) and $FF))]));
    end;
    aLog.WriteLine('---end decoder array---');
    {$ENDIF}
  end;

  if (FUsage <> huDecoding) then begin
    aLog.WriteLine('Huffman encoder array');
    aLog.WriteLine(Format('Alphabet size: %d', [FAlphaSize]));

    {$IFDEF EnableMegaLog}
    aLog.WriteLine('Symbol Len Xtra                    Reversed Code');
    for i := 0 to pred(FAlphaSize) do begin
      Code := FEncodes^[i];
      if (Code = -1) then
        aLog.WriteLine(Format('%6d%42s', [i, 'no code']))
      else
        aLog.WriteLine(Format('%6d%4d%5d%33s',
                       [i,
                        ((Code shr 16) and $FF),
                        ((Code shr 24) and $FF),
                        CodeToStr((Code and $FFFF), ((Code shr 16) and $FF))]));
    end;
    aLog.WriteLine('---end encoder array---');
    {$ENDIF}
  end;
end;
{$ENDIF}
{--------}
function TAbDfDecodeHuffmanTree.Decode(aLookupBits : integer) : longint;
begin
  {protect against dumb programming mistakes (note: FMask only exists
   if assertions are on)}
  {$IFOPT C+}
  Assert((aLookupBits and FMask) = 0,
         'TAbDfDecodeHuffmanTree.Decode: trying to decode too many bits, use LookupBitLength property');
  {$ENDIF}

  {return the code data}
  Result := FDecodes^[aLookupBits];
end;
{--------}
function TAbDfDecodeHuffmanTree.Encode(aSymbol : integer) : longint;
begin
  {protect against dumb programming mistakes}
  Assert((0 <= aSymbol) and (aSymbol < FAlphaSize),
         'TAbDfDecodeHuffmanTree.Encode: trying to encode a symbol that is not in the alphabet');

  {return the code data}
  Result := FEncodes^[aSymbol];

  {if the result is -1, it's another programming mistake: the user is
   attempting to get a code for a symbol that wasn't being used}
  Assert(Result <> -1,
         'TAbDfDecodeHuffmanTree.Encode: trying to encode a symbol that was not used');
end;
{====================================================================}


{===BuildStaticTrees=================================================}
procedure BuildStaticTrees;
var
  i        : integer;
  CodeLens : array [0..287] of integer;
begin
  {this routine builds the static huffman trees, those whose code
   lengths are determined by the deflate spec}

  {the static literal tree first}
  for i := 0 to 143 do
    CodeLens[i] := 8;
  for i := 144 to 255 do
    CodeLens[i] := 9;
  for i := 256 to 279 do
    CodeLens[i] := 7;
  for i := 280 to 287 do
    CodeLens[i] := 8;
  AbStaticLiteralTree := TAbDfDecodeHuffmanTree.Create(288, 15, huBoth);
  AbStaticLiteralTree.Build(CodeLens, 0, 288,
                            dfc_LitExtraBits, dfc_LitExtraOffset);

  {the static distance tree afterwards}
  for i := 0 to 31 do
    CodeLens[i] := 5;
  AbStaticDistanceTree := TAbDfDecodeHuffmanTree.Create(32, 15, huBoth);
  AbStaticDistanceTree.Build(CodeLens, 0, 32,
                             dfc_DistExtraBits, dfc_DistExtraOffset);
end;
{====================================================================}

initialization
  BuildStaticTrees;

finalization
  AbStaticLiteralTree.Free;
  AbStaticDistanceTree.Free;
  
end.
