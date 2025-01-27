// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE.txt file in the directory for more information.
// The Pascal translation by Alexander Koblov.

unit InflaterManaged;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, OutputWindow, InputBuffer, HuffmanTree;

type

  TBlockType =
  (
      Uncompressed = 0,
      Static = 1,
      Dynamic = 2
  );

  // Do not rearrange the enum values.
  TInflaterState =
  (
      ReadingHeader = 0, // Only applies to GZIP

      ReadingBFinal = 2, // About to read bfinal bit
      ReadingBType = 3, // About to read blockType bits

      ReadingNumLitCodes = 4, // About to read # literal codes
      ReadingNumDistCodes = 5, // About to read # dist codes
      ReadingNumCodeLengthCodes = 6, // About to read # code length codes
      ReadingCodeLengthCodes = 7, // In the middle of reading the code length codes
      ReadingTreeCodesBefore = 8, // In the middle of reading tree codes (loop top)
      ReadingTreeCodesAfter = 9, // In the middle of reading tree codes (extension; code > 15)

      DecodeTop = 10, // About to decode a literal (char/match) in a compressed block
      HaveInitialLength = 11, // Decoding a match, have the literal code (base length)
      HaveFullLength = 12, // Ditto, now have the full match length (incl. extra length bits)
      HaveDistCode = 13, // Ditto, now have the distance code also, need extra dist bits

      //* uncompressed blocks */
      UncompressedAligning = 15,
      UncompressedByte1 = 16,
      UncompressedByte2 = 17,
      UncompressedByte3 = 18,
      UncompressedByte4 = 19,
      DecodingUncompressed = 20,

      // These three apply only to GZIP
      StartReadingFooter = 21, // (Initialisation for reading footer)
      ReadingFooter = 22,
      VerifyingFooter = 23,

      Done = 24 // Finished
  );

type

  { TInflaterManaged }

  TInflaterManaged = class
  private
    _output: TOutputWindow;
    _input: TInputBuffer;
    _literalLengthTree: IHuffmanTree;
    _distanceTree: IHuffmanTree;

    _state: TInflaterState;

    _bfinal: Integer;
    _blockType: TBlockType;

    // uncompressed block
    _blockLengthBuffer: array[0..3] of Byte;
    _blockLength: Integer;

    // compressed block
    _length: Integer;
    _distanceCode: Integer;
    _extraBits: Integer;

    _loopCounter: Integer;
    _literalLengthCodeCount: Integer;
    _distanceCodeCount: Integer;
    _codeLengthCodeCount: Integer;
    _codeArraySize: Integer;
    _lengthCode: Integer;

    _codeList: TBytes; // temporary array to store the code length for literal/Length and distance
    _codeLengthTreeCodeLength: TBytes;
    _deflate64: Boolean;
    _codeLengthTree: IHuffmanTree;
  private
    procedure Reset;
    function Decode: Boolean;
    function DecodeUncompressedBlock(out endOfBlock: Boolean): Boolean;
    function DecodeBlock(out endOfBlockCodeSeen: Boolean): Boolean;
    function DecodeDynamicBlockHeader: Boolean;
  public
    constructor Create(deflate64: Boolean);
    destructor Destroy; override;
    procedure SetInput(inputBytes: PByte; offset, length: Integer);
    function Finished: Boolean;
    function AvailableOutput: Integer;
    function Inflate(bytes: PByte; offset, length: Integer): Integer;
  end;

implementation

const
  // Extra bits for length code 257 - 285.
  S_EXTRA_LENGTH_BITS: array[0..28] of Byte =
  (
      0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3,
      3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 16
  );

  // The base length for length code 257 - 285.
  // The formula to get the real length for a length code is lengthBase[code - 257] + (value stored in extraBits)
  S_LENGTH_BASE: array[0..28] of Integer =
  (
      3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51,
      59, 67, 83, 99, 115, 131, 163, 195, 227, 3
  );

  // The base distance for distance code 0 - 31
  // The real distance for a distance code is  distanceBasePosition[code] + (value stored in extraBits)
  S_DISTANCE_BASE_POSITION: array[0..31] of Integer =
  (
      1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513,
      769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577, 32769, 49153
  );

  // code lengths for code length alphabet is stored in following order
  S_CODE_ORDER: array[0..18] of Byte = (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);

  S_STATIC_DISTANCE_TREE_TABLE: array[0..31] of Byte =
  (
      $00, $10, $08, $18, $04, $14, $0c, $1c, $02, $12, $0a, $1a,
      $06, $16, $0e, $1e, $01, $11, $09, $19, $05, $15, $0d, $1d,
      $03, $13, $0b, $1b, $07, $17, $0f, $1f
  );

{ TInflaterManaged }

procedure TInflaterManaged.Reset;
begin
  _state := TInflaterState.ReadingBFinal; // start by reading BFinal bit
end;

function TInflaterManaged.Decode: Boolean;
var
  eob: Boolean = false;
begin
  result := false;

  if (Finished()) then
  begin
      Exit(true);
  end;

  if (_state = TInflaterState.ReadingBFinal) then
  begin
      // reading bfinal bit
      // Need 1 bit
      if (not _input.EnsureBitsAvailable(1)) then
      begin
          Exit(false);
      end;

      _bfinal := _input.GetBits(1);
      _state := TInflaterState.ReadingBType;
  end;

  if (_state = TInflaterState.ReadingBType) then
  begin
      // Need 2 bits
      if (not _input.EnsureBitsAvailable(2)) then
      begin
          _state := TInflaterState.ReadingBType;
          Exit(false);
      end;

      _blockType := TBlockType(_input.GetBits(2));

      if (_blockType = TBlockType.Dynamic) then
      begin
          _state := TInflaterState.ReadingNumLitCodes;
      end
      else if (_blockType = TBlockType.Static) then
      begin
          _literalLengthTree := THuffmanTree.StaticLiteralLengthTree;
          _distanceTree := THuffmanTree.StaticDistanceTree;
          _state := TInflaterState.DecodeTop;
      end
      else if (_blockType = TBlockType.Uncompressed) then
      begin
          _state := TInflaterState.UncompressedAligning;
      end
      else
      begin
          raise Exception.Create('Deflate64: unknown block type');
      end;
  end;

  if (_blockType = TBlockType.Dynamic) then
  begin
      if (_state < TInflaterState.DecodeTop) then
      begin
          // we are reading the header
          result := DecodeDynamicBlockHeader();
      end
      else
      begin
          result := DecodeBlock(eob); // this can returns true when output is full
      end;
  end
  else if (_blockType = TBlockType.Static) then
  begin
      result := DecodeBlock(eob);
  end
  else if (_blockType = TBlockType.Uncompressed) then
  begin
      result := DecodeUncompressedBlock(eob);
  end
  else
  begin
      raise Exception.Create('Deflate64: unknown block type');
  end;

  //
  // If we reached the end of the block and the block we were decoding had
  // bfinal=1 (final block)
  //
  if (eob and (_bfinal <> 0)) then
  begin
      _state := TInflaterState.Done;
  end;
end;

function TInflaterManaged.DecodeUncompressedBlock(out endOfBlock: Boolean
  ): Boolean;
var
  bits, bytesCopied: Integer;
  blockLengthComplement: Integer;
begin
  endOfBlock := false;
  while (true) do
  begin
      case (_state) of
          TInflaterState.UncompressedAligning: // initial state when calling this function
            begin
              // we must skip to a byte boundary
              _input.SkipToByteBoundary();
              _state := TInflaterState.UncompressedByte1;
              Continue;
            end;
          TInflaterState.UncompressedByte1, // decoding block length
          TInflaterState.UncompressedByte2,
          TInflaterState.UncompressedByte3,
          TInflaterState.UncompressedByte4:
            begin
              bits := _input.GetBits(8);
              if (bits < 0) then
              begin
                  Exit(false);
              end;

              _blockLengthBuffer[Integer(_state) - Integer(TInflaterState.UncompressedByte1)] := byte(bits);
              if (_state = TInflaterState.UncompressedByte4) then
              begin
                  _blockLength := _blockLengthBuffer[0] + (_blockLengthBuffer[1] * 256);
                  blockLengthComplement :=
                      _blockLengthBuffer[2] + (_blockLengthBuffer[3] * 256);

                  // make sure complement matches
                  if (UInt16(_blockLength) <> UInt16((not blockLengthComplement))) then
                  begin
                      raise Exception.Create('Deflate64: invalid block length');
                  end;
              end;

              Inc(_state);
            end;

          TInflaterState.DecodingUncompressed: // copying block data
            begin
              // Directly copy bytes from input to output.
              bytesCopied := _output.CopyFrom(_input, _blockLength);
              _blockLength -= bytesCopied;

              if (_blockLength = 0) then
              begin
                  // Done with this block, need to re-init bit buffer for next block
                  _state := TInflaterState.ReadingBFinal;
                  endOfBlock := true;
                  Exit(true);
              end;

              // We can fail to copy all bytes for two reasons:
              //    Running out of Input
              //    running out of free space in output window
              if (_output.FreeBytes = 0) then
              begin
                  Exit(true);
              end;

              Exit(false);
            end;
          else begin
              //*Fail*/
              Assert(false, 'check why we are here!');
              raise Exception.Create('Deflate64: unknown state');
          end;
      end;
  end;
end;

function TInflaterManaged.DecodeBlock(out endOfBlockCodeSeen: Boolean): Boolean;
var
  freeBytes, symbol, bits, offset: Integer;
begin
  endOfBlockCodeSeen := false;

  freeBytes := _output.FreeBytes; // it is a little bit faster than frequently accessing the property
  while (freeBytes > 65536) do
  begin
      // With Deflate64 we can have up to a 64kb length, so we ensure at least that much space is available
      // in the OutputWindow to avoid overwriting previous unflushed output data.
      case (_state) of

          TInflaterState.DecodeTop:
            begin
              // decode an element from the literal tree

              // TODO: optimize this!!!
              symbol := _literalLengthTree.GetNextSymbol(_input);
              if (symbol < 0) then
              begin
                  // running out of input
                  Exit(false);
              end;

              if (symbol < 256) then
              begin
                  // literal
                  _output.Write(Byte(symbol));
                  Dec(freeBytes);
              end
              else if (symbol = 256) then
              begin
                  // end of block
                  endOfBlockCodeSeen := true;
                  // Reset state
                  _state := TInflaterState.ReadingBFinal;
                  Exit(true);
              end
              else
              begin
                  // length/distance pair
                  symbol -= 257; // length code started at 257
                  if (symbol < 8) then
                  begin
                      symbol += 3; // match length = 3,4,5,6,7,8,9,10
                      _extraBits := 0;
                  end
                  else if ((not _deflate64) and (symbol = 28)) then
                  begin
                      // extra bits for code 285 is 0
                      symbol := 258; // code 285 means length 258
                      _extraBits := 0;
                  end
                  else
                  begin
                      if (symbol < 0) or (symbol >= Length(S_EXTRA_LENGTH_BITS)) then
                      begin
                          raise Exception.Create('Deflate64: invalid data');
                      end;
                      _extraBits := S_EXTRA_LENGTH_BITS[symbol];
                      Assert(_extraBits <> 0, 'We handle other cases separately!');
                  end;
                  _length := symbol;
                  _state := TInflaterState.HaveInitialLength;
                  Continue;
              end;
            end;

          TInflaterState.HaveInitialLength:
            begin
              if (_extraBits > 0) then
              begin
                  _state := TInflaterState.HaveInitialLength;
                  bits := _input.GetBits(_extraBits);
                  if (bits < 0) then
                  begin
                      Exit(false);
                  end;

                  if (_length < 0) or (_length >= Length(S_LENGTH_BASE)) then
                  begin
                      raise Exception.Create('Deflate64: invalid data');
                  end;
                  _length := S_LENGTH_BASE[_length] + bits;
              end;
              _state := TInflaterState.HaveFullLength;
              Continue;
            end;


          TInflaterState.HaveFullLength:
            begin
              if (_blockType = TBlockType.Dynamic) then
              begin
                  _distanceCode := _distanceTree.GetNextSymbol(_input);
              end
              else
              begin
                  // get distance code directly for static block
                  _distanceCode := _input.GetBits(5);
                  if (_distanceCode >= 0) then
                  begin
                      _distanceCode := S_STATIC_DISTANCE_TREE_TABLE[_distanceCode];
                  end;
              end;

              if (_distanceCode < 0) then
              begin
                  // running out input
                  Exit(false);
              end;

              _state := TInflaterState.HaveDistCode;
              Continue;
            end;

          TInflaterState.HaveDistCode:
            begin
              // To avoid a table lookup we note that for distanceCode > 3,
              // extra_bits = (distanceCode-2) >> 1
              if (_distanceCode > 3) then
              begin
                  _extraBits := (_distanceCode - 2) >> 1;
                  bits := _input.GetBits(_extraBits);
                  if (bits < 0) then
                  begin
                      Exit(false);
                  end;
                  offset := S_DISTANCE_BASE_POSITION[_distanceCode] + bits;
              end
              else
              begin
                  offset := _distanceCode + 1;
              end;

              _output.WriteLengthDistance(_length, offset);
              freeBytes -= _length;
              _state := TInflaterState.DecodeTop;
            end;

          else begin
              //*Fail*/
              Assert(false, 'check why we are here!');
              raise Exception.Create('Deflate64: unknown state');
          end;
      end;
  end;

  Result := true;
end;

function TInflaterManaged.DecodeDynamicBlockHeader: Boolean;
var
  i, j, bits, repeatCount, previousCode: Integer;
  literalTreeCodeLength, distanceTreeCodeLength: TBytes;
begin
  while (True) do
  begin
    case (_state) of

      TInflaterState.ReadingNumLitCodes:
        begin
          _literalLengthCodeCount := _input.GetBits(5);
          if (_literalLengthCodeCount < 0) then
          begin
              Exit(false);
          end;
          _literalLengthCodeCount += 257;
          _state := TInflaterState.ReadingNumDistCodes;
          Continue;
        end;

      TInflaterState.ReadingNumDistCodes:
        begin
          _distanceCodeCount := _input.GetBits(5);
          if (_distanceCodeCount < 0) then
          begin
              Exit(false);
          end;
          _distanceCodeCount += 1;
          _state := TInflaterState.ReadingNumCodeLengthCodes;
          Continue;
        end;

      TInflaterState.ReadingNumCodeLengthCodes:
        begin
          _codeLengthCodeCount := _input.GetBits(4);
          if (_codeLengthCodeCount < 0) then
          begin
              Exit(false);
          end;
          _codeLengthCodeCount += 4;
          _loopCounter := 0;
          _state := TInflaterState.ReadingCodeLengthCodes;
          Continue;
        end;

      TInflaterState.ReadingCodeLengthCodes:
        begin
          while (_loopCounter < _codeLengthCodeCount) do
          begin
              bits := _input.GetBits(3);
              if (bits < 0) then
              begin
                  Exit(false);
              end;
              _codeLengthTreeCodeLength[S_CODE_ORDER[_loopCounter]] := Byte(bits);
              Inc(_loopCounter);
          end;

          for i := _codeLengthCodeCount to High(S_CODE_ORDER) do
          begin
              _codeLengthTreeCodeLength[S_CODE_ORDER[i]] := 0;
          end;

          // create huffman tree for code length
          _codeLengthTree := THuffmanTree.Create(_codeLengthTreeCodeLength);
          _codeArraySize := _literalLengthCodeCount + _distanceCodeCount;
          _loopCounter := 0; // reset loop count

          _state := TInflaterState.ReadingTreeCodesBefore;
          Continue;
        end;

      TInflaterState.ReadingTreeCodesBefore,
      TInflaterState.ReadingTreeCodesAfter:
        begin
          while (_loopCounter < _codeArraySize) do
          begin
              if (_state = TInflaterState.ReadingTreeCodesBefore) then
              begin
                  _lengthCode := _codeLengthTree.GetNextSymbol(_input);
                  if (_lengthCode < 0) then
                  begin
                      Exit(false);
                  end;
              end;

              // The alphabet for code lengths is as follows:
              //  0 - 15: Represent code lengths of 0 - 15
              //  16: Copy the previous code length 3 - 6 times.
              //  The next 2 bits indicate repeat length
              //         (0 = 3, ... , 3 = 6)
              //      Example:  Codes 8, 16 (+2 bits 11),
              //                16 (+2 bits 10) will expand to
              //                12 code lengths of 8 (1 + 6 + 5)
              //  17: Repeat a code length of 0 for 3 - 10 times.
              //    (3 bits of length)
              //  18: Repeat a code length of 0 for 11 - 138 times
              //    (7 bits of length)
              if (_lengthCode <= 15) then
              begin
                  _codeList[_loopCounter] := Byte(_lengthCode);
                  Inc(_loopCounter);
              end
              else
              begin
                  if (_lengthCode = 16) then
                  begin
                      if (not _input.EnsureBitsAvailable(2)) then
                      begin
                          _state := TInflaterState.ReadingTreeCodesAfter;
                          Exit(false);
                      end;

                      if (_loopCounter = 0) then
                      begin
                          // can't have "prev code" on first code
                          raise Exception(EmptyStr);
                      end;

                      previousCode := _codeList[_loopCounter - 1];
                      repeatCount := _input.GetBits(2) + 3;

                      if (_loopCounter + repeatCount > _codeArraySize) then
                      begin
                          raise Exception(EmptyStr);
                      end;

                      for j := 0 to Pred(repeatCount) do
                      begin
                          _codeList[_loopCounter] := previousCode;
                          Inc(_loopCounter);
                      end;
                  end
                  else if (_lengthCode = 17) then
                  begin
                      if (not _input.EnsureBitsAvailable(3)) then
                      begin
                          _state := TInflaterState.ReadingTreeCodesAfter;
                          Exit(false);
                      end;

                      repeatCount := _input.GetBits(3) + 3;

                      if (_loopCounter + repeatCount > _codeArraySize) then
                      begin
                          raise Exception(EmptyStr);
                      end;

                      for j := 0 to Pred(repeatCount) do
                      begin
                          _codeList[_loopCounter] := 0;
                          Inc(_loopCounter);
                      end;
                  end
                  else
                  begin
                      // code == 18
                      if (not _input.EnsureBitsAvailable(7)) then
                      begin
                          _state := TInflaterState.ReadingTreeCodesAfter;
                          Exit(false);
                      end;

                      repeatCount := _input.GetBits(7) + 11;

                      if (_loopCounter + repeatCount > _codeArraySize) then
                      begin
                          raise Exception(EmptyStr);
                      end;

                      for j := 0 to Pred(repeatCount) do
                      begin
                          _codeList[_loopCounter] := 0;
                          Inc(_loopCounter);
                      end;
                  end;
              end;
              _state := TInflaterState.ReadingTreeCodesBefore; // we want to read the next code.
          end;
        end;

      else
          //*Fail*/
          Assert(false, 'check why we are here!');
          raise Exception.Create('Deflate64: unknown state');
      end;
    Break;
  end;

  SetLength(literalTreeCodeLength, THuffmanTree.MAX_LITERAL_TREE_ELEMENTS);
  SetLength(distanceTreeCodeLength, THuffmanTree.MAX_DIST_TREE_ELEMENTS);

  // Create literal and distance tables
  Move(_codeList[0], literalTreeCodeLength[0], _literalLengthCodeCount);
  Move(_codeList[_literalLengthCodeCount], distanceTreeCodeLength[0], _distanceCodeCount);

  // Make sure there is an end-of-block code, otherwise how could we ever end?
  if (literalTreeCodeLength[THuffmanTree.END_OF_BLOCK_CODE] = 0) then
  begin
      raise Exception(EmptyStr);
  end;

  _literalLengthTree := THuffmanTree.Create(literalTreeCodeLength);
  _distanceTree := THuffmanTree.Create(distanceTreeCodeLength);
  _state := TInflaterState.DecodeTop;
  Result := true;
end;

constructor TInflaterManaged.Create(deflate64: Boolean);
begin
  _output := TOutputWindow.Create;
  _input := TInputBuffer.Create;

  SetLength(_codeList, THuffmanTree.MAX_LITERAL_TREE_ELEMENTS + THuffmanTree.MAX_DIST_TREE_ELEMENTS);

  SetLength(_codeLengthTreeCodeLength, THuffmanTree.NUMBER_OF_CODE_LENGTH_TREE_ELEMENTS);
  _deflate64 := deflate64;

  Reset();
end;

destructor TInflaterManaged.Destroy;
begin
  inherited Destroy;
  _output.Free;
  _input.Free;
end;

procedure TInflaterManaged.SetInput(inputBytes: PByte; offset, length: Integer);
begin
  _input.SetInput(inputBytes, offset, length); // append the bytes
end;

function TInflaterManaged.Finished: Boolean;
begin
  Result := (_state = TInflaterState.Done) or (_state = TInflaterState.VerifyingFooter);
end;

function TInflaterManaged.AvailableOutput: Integer;
begin
  Result := _output.AvailableBytes;
end;

function TInflaterManaged.Inflate(bytes: PByte; offset, length: Integer): Integer;
var
  copied: Integer;
  count: Integer = 0;
begin
  // copy bytes from output to outputbytes if we have available bytes
  // if buffer is not filled up. keep decoding until no input are available
  // if decodeBlock returns false. Throw an exception.
  repeat
      copied := _output.CopyTo(bytes, offset, length);
      if (copied > 0) then
      begin
          offset += copied;
          count += copied;
          length -= copied;
      end;

      if (length = 0) then
      begin // filled in the bytes array
          break;
      end;
      // Decode will return false when more input is needed
  until not ((not Finished() and Decode()));

  Result := count;
end;

end.

