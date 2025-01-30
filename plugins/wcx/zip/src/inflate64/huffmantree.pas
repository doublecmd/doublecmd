// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE.txt file in the directory for more information.
// The Pascal translation by Alexander Koblov.

unit HuffmanTree;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, InputBuffer;

type

  { IHuffmanTree }

  IHuffmanTree = interface(IUnknown)
    ['{83791289-1747-4815-AC47-B57A0A0C39C6}']
    function GetNextSymbol(input: TInputBuffer): Integer;
  end;

  { THuffmanTree }

  THuffmanTree = class(TInterfacedObject, IHuffmanTree)
  public
    const MAX_LITERAL_TREE_ELEMENTS = Integer(288);
    const MAX_DIST_TREE_ELEMENTS = Integer(32);
    const END_OF_BLOCK_CODE = Integer(256);
    const NUMBER_OF_CODE_LENGTH_TREE_ELEMENTS = Integer(19);
  private
    _tableBits: Integer;
    _table: TSmallIntDynArray;
    _left: TSmallIntDynArray;
    _right: TSmallIntDynArray;
    _codeLengthArray: TBytes;

    _tableMask: Integer;
  private
    _StaticLiteralLengthTree: THuffmanTree; static;
    _StaticDistanceTree: THuffmanTree; static;
  private
    class procedure CreateStaticTrees;
    class procedure FreeStaticTrees;
    // Generate the array contains huffman codes lengths for static huffman tree
    class function GetStaticLiteralTreeLength: TBytes;
    class function GetStaticDistanceTreeLength: TBytes;
    // Calculate the huffman code for each character based on the code length for each character.
    function CalculateHuffmanCode: TCardinalDynArray;
    procedure CreateTable;
  public
    class property StaticLiteralLengthTree: THuffmanTree read _StaticLiteralLengthTree;
    class property StaticDistanceTree: THuffmanTree read _StaticDistanceTree;
  public
    constructor Create(constref codeLengths: TBytes);
    // This function will try to get enough bits from input and try to decode the bits
    function GetNextSymbol(input: TInputBuffer): Integer;
  end;

implementation

// Reverse 'length' of the bits in code
function BitReverse(code: Cardinal; length: Integer): Cardinal;
begin
    Result := 0;

    Assert((length > 0) and (length <= 16), 'Invalid len');
    repeat
        Result := Result or (code and 1);
        Result := Result << 1;
        code := code >> 1;
        Dec(length);
    until (length = 0);

    Result := Result >> 1;
end;

{ THuffmanTree }

class procedure THuffmanTree.CreateStaticTrees;
begin
  _StaticLiteralLengthTree:= THuffmanTree.Create(GetStaticLiteralTreeLength());
  _StaticDistanceTree:= THuffmanTree.Create(GetStaticDistanceTreeLength());
  _StaticLiteralLengthTree._AddRef;
  _StaticDistanceTree._AddRef;
end;

class procedure THuffmanTree.FreeStaticTrees;
begin
  _StaticLiteralLengthTree._Release;
  _StaticDistanceTree._Release;
end;

class function THuffmanTree.GetStaticLiteralTreeLength: TBytes;
var
  i: Integer;
  literalTreeLength: TBytes;
begin
  SetLength(literalTreeLength, MAX_LITERAL_TREE_ELEMENTS);

  for i := 0 to 143 do
  begin
      literalTreeLength[i] := 8;
  end;

  for i := 144 to 255 do
  begin
      literalTreeLength[i] := 9;
  end;

  for i := 256 to 279 do
  begin
      literalTreeLength[i] := 7;
  end;

  for i := 280 to 287 do
  begin
      literalTreeLength[i] := 8;
  end;

  Result := literalTreeLength;
end;

class function THuffmanTree.GetStaticDistanceTreeLength: TBytes;
var
  i: Integer;
  staticDistanceTreeLength: TBytes;
begin
  SetLength(staticDistanceTreeLength, MAX_DIST_TREE_ELEMENTS);
  for i := 0 to Pred(MAX_DIST_TREE_ELEMENTS) do
  begin
      staticDistanceTreeLength[i] := 5;
  end;
  Result := staticDistanceTreeLength;
end;

function THuffmanTree.CalculateHuffmanCode: TCardinalDynArray;
var
  i, bits: Integer;
  tempCode: Cardinal = 0;
  code: TCardinalDynArray;
  len, codeLength: Integer;
  bitLengthCount: TCardinalDynArray;
  nextCode: array[0..16] of Cardinal;
begin
  SetLength(bitLengthCount, 17);
  SetLength(code, MAX_LITERAL_TREE_ELEMENTS);

  for i := 0 to High(_codeLengthArray) do
  begin
      codeLength := _codeLengthArray[i];
      Inc(bitLengthCount[codeLength]);
  end;
  bitLengthCount[0] := 0; // clear count for length 0

  for bits := 1 to 16 do
  begin
      tempCode := (tempCode + bitLengthCount[bits - 1]) << 1;
      nextCode[bits] := tempCode;
  end;

  for i := 0 to High(_codeLengthArray) do
  begin
      len := _codeLengthArray[i];

      if (len > 0) then
      begin
          code[i] := BitReverse(nextCode[len], len);
          Inc(nextCode[len]);
      end;
  end;
  Result:=  code;
end;

procedure THuffmanTree.CreateTable;
var
  avail, value: Int16;
  increment, locs: Integer;
  array_: TSmallIntDynArray;
  codeArray: TCardinalDynArray;
  j, ch, len, start, index: Integer;
  overflowBits, codeBitMask: Integer;
begin
  codeArray := CalculateHuffmanCode();

  avail := Int16(Length(_codeLengthArray));

  for ch := 0 to High(_codeLengthArray) do
  begin
      // length of this code
      len := _codeLengthArray[ch];
      if (len > 0) then
      begin
          // start value (bit reversed)
          start := Integer(codeArray[ch]);

          if (len <= _tableBits) then
          begin
              // If a particular symbol is shorter than nine bits,
              // then that symbol's translation is duplicated
              // in all those entries that start with that symbol's bits.
              // For example, if the symbol is four bits, then it's duplicated
              // 32 times in a nine-bit table. If a symbol is nine bits long,
              // it appears in the table once.
              //
              // Make sure that in the loop below, code is always
              // less than table_size.
              //
              // On last iteration we store at array index:
              //    initial_start_at + (locs-1)*increment
              //  = initial_start_at + locs*increment - increment
              //  = initial_start_at + (1 << tableBits) - increment
              //  = initial_start_at + table_size - increment
              //
              // Therefore we must ensure:
              //     initial_start_at + table_size - increment < table_size
              // or: initial_start_at < increment
              //
              increment := 1 << len;
              if (start >= increment) then
              begin
                  raise Exception.Create('Deflate64: invalid Huffman data');
              end;

              // Note the bits in the table are reverted.
              locs := 1 << (_tableBits - len);
              for j := 0 to Pred(locs) do
              begin
                  _table[start] := Int16(ch);
                  start += increment;
              end;
          end
          else
          begin
              // For any code which has length longer than num_elements,
              // build a binary tree.

              overflowBits := len - _tableBits; // the nodes we need to respent the data.
              codeBitMask := 1 << _tableBits; // mask to get current bit (the bits can't fit in the table)

              // the left, right table is used to repesent the
              // the rest bits. When we got the first part (number bits.) and look at
              // tbe table, we will need to follow the tree to find the real character.
              // This is in place to avoid bloating the table if there are
              // a few ones with long code.
              index := start and ((1 << _tableBits) - 1);
              array_ := _table;

              repeat
                  value := array_[index];

                  if (value = 0) then
                  begin
                      // set up next pointer if this node is not used before.
                      array_[index] := Int16(-avail); // use next available slot.
                      value := Int16(-avail);
                      Inc(avail);
                  end;

                  if (value > 0) then
                  begin
                      // prevent an IndexOutOfRangeException from array[index]
                      raise Exception.Create('Deflate64: invalid Huffman data');
                  end;

                  Assert(
                      value < 0,
                      'CreateTable: Only negative numbers are used for tree pointers!'
                  );

                  if ((start and codeBitMask) = 0) then
                  begin
                      // if current bit is 0, go change the left array
                      array_ := _left;
                  end
                  else
                  begin
                      // if current bit is 1, set value in the right array
                      array_ := _right;
                  end;
                  index := -value; // go to next node

                  codeBitMask := codeBitMask << 1;
                  Dec(overflowBits);
              until (overflowBits = 0);

              array_[index] := Int16(ch);
          end;
      end;
  end;
end;

constructor THuffmanTree.Create(constref codeLengths: TBytes);
begin
  Assert(
      (Length(codeLengths) = MAX_LITERAL_TREE_ELEMENTS)
          or (Length(codeLengths) = MAX_DIST_TREE_ELEMENTS)
          or (Length(codeLengths) = NUMBER_OF_CODE_LENGTH_TREE_ELEMENTS),
      'we only expect three kinds of Length here'
  );
  _codeLengthArray := codeLengths;

  if (Length(_codeLengthArray) = MAX_LITERAL_TREE_ELEMENTS) then
  begin
      // bits for Literal/Length tree table
      _tableBits := 9;
  end
  else
  begin
      // bits for distance tree table and code length tree table
      _tableBits := 7;
  end;
  _tableMask := (1 << _tableBits) - 1;

  SetLength(_table, 1 << _tableBits);

  // I need to find proof that left and right array will always be
  // enough. I think they are.
  SetLength(_left, 2 * Length(_codeLengthArray));
  SetLength(_right, 2 * Length(_codeLengthArray));

  CreateTable();
end;

function THuffmanTree.GetNextSymbol(input: TInputBuffer): Integer;
var
  bitBuffer, mask: Cardinal;
  symbol, codeLength: Integer;
begin
  // Try to load 16 bits into input buffer if possible and get the bitBuffer value.
  // If there aren't 16 bits available we will return all we have in the
  // input buffer.
  bitBuffer := input.TryLoad16Bits();
  if (input.AvailableBits = 0) then
  begin // running out of input.
      Exit(-1);
  end;

  // decode an element
  symbol := _table[bitBuffer and _tableMask];
  if (symbol < 0) then
  begin //  this will be the start of the binary tree
      // navigate the tree
      mask := Cardinal(1) << _tableBits;
      repeat
          symbol := -symbol;
          if ((bitBuffer and mask) = 0) then
          begin
              symbol := _left[symbol];
          end
          else
          begin
              symbol := _right[symbol];
          end;

          mask := mask << 1;
      until (symbol >= 0);
  end;

  codeLength := _codeLengthArray[symbol];

  // huffman code lengths must be at least 1 bit long
  if (codeLength <= 0) then
  begin
      raise Exception.Create('Deflate64: invalid Huffman data');
  end;

  //
  // If this code is longer than the # bits we had in the bit buffer (i.e.
  // we read only part of the code), we can hit the entry in the table or the tree
  // for another symbol. However the length of another symbol will not match the
  // available bits count.
  if (codeLength > input.AvailableBits) then
  begin
      // We already tried to load 16 bits and maximum length is 15,
      // so this means we are running out of input.
      Exit(-1);
  end;

  input.SkipBits(codeLength);
  Result := symbol;
end;

initialization
  THuffmanTree.CreateStaticTrees;

finalization
  THuffmanTree.FreeStaticTrees;

end.

