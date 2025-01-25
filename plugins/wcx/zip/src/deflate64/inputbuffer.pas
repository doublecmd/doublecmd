// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE.txt file in the directory for more information.
// The Pascal translation by Alexander Koblov.

unit InputBuffer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TInputBuffer }

  TInputBuffer = class
  private
    _buffer: PByte; // byte array to store input
    _start: Integer; // start poisition of the buffer
    _end: Integer;   // end position of the buffer
    _bitBuffer: Cardinal; // store the bits here, we can quickly shift in this buffer
    _bitsInBuffer: Integer; // number of bits available in bitBuffer
  public
    // Total bytes available in the input buffer
    function AvailableBytes: Integer;
    // Ensure that count bits are in the bit buffer
    function EnsureBitsAvailable(count: Integer): Boolean;
    // This function will try to load 16 or more bits into bitBuffer
    function TryLoad16Bits: Cardinal;
    // Gets count bits from the input buffer. Returns -1 if not enough bits available
    function GetBits(count: Integer): Integer;
    // Copies length bytes from input buffer to output buffer starting at output[offset]
    function CopyTo(output: PByte; offset, length: Integer): Integer;
    // Return true is all input bytes are used
    function NeedsInput: Boolean;
    // Set the byte array to be processed
    procedure SetInput(buffer: PByte; offset, length: Integer);
    // Skip n bits in the buffer
    procedure SkipBits(n: Integer);
    /// Skips to the next byte boundary
    procedure SkipToByteBoundary;
    // Total bits available in the input buffer
    property AvailableBits: Integer read _bitsInBuffer;
  end;

implementation

{ TInputBuffer }

function TInputBuffer.AvailableBytes: Integer;
begin
  Result:= (_end - _start) + (_bitsInBuffer div 8);
end;

function TInputBuffer.EnsureBitsAvailable(count: Integer): Boolean;
begin
  Assert((0 < count) and (count <= 16), 'count is invalid.');

  // manual inlining to improve perf
  if (_bitsInBuffer < count) then
  begin
      if (NeedsInput()) then
      begin
          Exit(false);
      end;
      // insert a byte to bitbuffer
      _bitBuffer := _bitBuffer or Cardinal(_buffer[_start]) << _bitsInBuffer;
      _bitsInBuffer += 8;
      Inc(_start);

      if (_bitsInBuffer < count) then
      begin
          if (NeedsInput()) then
          begin
              Exit(false);
          end;
          // insert a byte to bitbuffer
          _bitBuffer := _bitBuffer or Cardinal(_buffer[_start]) << _bitsInBuffer;
          _bitsInBuffer += 8;
          Inc(_start);
      end;
  end;

  Result := true;
end;

function TInputBuffer.TryLoad16Bits: Cardinal;
begin
  if (_bitsInBuffer < 8) then
  begin
      if (_start < _end) then
      begin
          _bitBuffer := _bitBuffer or Cardinal(_buffer[_start]) << _bitsInBuffer;
          _bitsInBuffer += 8;
          Inc(_start);
      end;

      if (_start < _end) then
      begin
          _bitBuffer := _bitBuffer or Cardinal(_buffer[_start]) << _bitsInBuffer;
          _bitsInBuffer += 8;
          Inc(_start);
      end;
  end
  else if (_bitsInBuffer < 16) then
  begin
      if (_start < _end) then
      begin
          _bitBuffer := _bitBuffer or Cardinal(_buffer[_start]) << _bitsInBuffer;
          _bitsInBuffer += 8;
          Inc(_start);
      end;
  end;

  Result := _bitBuffer;
end;

function TInputBuffer.GetBits(count: Integer): Integer;
begin
  Assert((0 < count) and (count <= 16), 'count is invalid.');

  if (not EnsureBitsAvailable(count)) then
  begin
      Exit(-1);
  end;

  result := Integer(_bitBuffer) and (((Cardinal(1) << count) - 1));
  _bitBuffer := _bitBuffer >> count;
  _bitsInBuffer -= count;
end;

function TInputBuffer.CopyTo(output: PByte; offset, length: Integer): Integer;
var
  avail: Integer;
  bytesFromBitBuffer: Integer = 0;
begin
  Assert(output <> nil);
  Assert(offset >= 0);
  Assert(length >= 0);
//  Assert(offset <= System.Length(output) - length);
  Assert((_bitsInBuffer mod 8) = 0);

  // Copy the bytes in bitBuffer first.
  while (_bitsInBuffer > 0) and (length > 0) do
  begin
      output[offset] := Byte(_bitBuffer);
      _bitBuffer := _bitBuffer >> 8;
      _bitsInBuffer -= 8;
      Inc(offset);
      Dec(length);
      Inc(bytesFromBitBuffer);
  end;

  if (length = 0) then
  begin
      Exit(bytesFromBitBuffer);
  end;

  avail := _end - _start;
  if (length > avail) then
  begin
      length := avail;
  end;

  Move(_buffer[_start], output[offset], length);
  _start += length;
  Result := bytesFromBitBuffer + length;
end;

function TInputBuffer.NeedsInput(): Boolean;
begin
  Result := (_start = _end);
end;

procedure TInputBuffer.SetInput(buffer: PByte; offset, length: Integer);
begin
  Assert(buffer <> nil);
  Assert(offset >= 0);
  Assert(length >= 0);
//  Assert(offset <= System.Length(buffer) - length);
  Assert(_start = _end);

  _buffer := buffer;
  _start := offset;
  _end := offset + length;
end;

procedure TInputBuffer.SkipBits(n: Integer);
begin
  Assert(
      _bitsInBuffer >= n,
      'No enough bits in the buffer, Did you call EnsureBitsAvailable?'
  );
  _bitBuffer := _bitBuffer >> n;
  _bitsInBuffer -= n;
end;

procedure TInputBuffer.SkipToByteBoundary;
begin
  _bitBuffer := _bitBuffer >> (_bitsInBuffer mod 8);
  _bitsInBuffer -= (_bitsInBuffer mod 8);
end;

end.

