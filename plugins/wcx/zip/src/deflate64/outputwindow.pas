// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE.txt file in the directory for more information.
// The Pascal translation by Alexander Koblov.

unit OutputWindow;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, InputBuffer;

type

  { TOutputWindow }

  TOutputWindow = class
  private
    // With Deflate64 we can have up to a 65536 length as well as up to a 65538 distance. This means we need a Window that is at
    // least 131074 bytes long so we have space to retrieve up to a full 64kb in lookback and place it in our buffer without
    // overwriting existing data. OutputWindow requires that the WindowSize be an exponent of 2, so we round up to 2^18.
    const WINDOW_SIZE = Integer(262144);
    const WINDOW_MASK = Integer(262143);
  private
    _window: array [0..Pred(WINDOW_SIZE)] of Byte; // The window is 2^18 bytes
    _end: Integer; // this is the position to where we should write next byte
    _bytesUsed: Integer; // The number of bytes in the output window which is not consumed.
  public
    // Add a byte to output window
    procedure Write(b: Byte);
    procedure WriteLengthDistance(length, distance: Integer);
    // Copy up to length of bytes from input directly
    function CopyFrom(input: TInputBuffer; length: Integer): Integer;
    // Free space in output window
    function FreeBytes: Integer;
    // Copy the decompressed bytes to output array
    function CopyTo(output: PByte; offset, length: Integer): Integer;
    // Bytes not consumed in output window
    property AvailableBytes: Integer read _bytesUsed;
  end;

implementation

uses
  Math;

{ TOutputWindow }

procedure TOutputWindow.Write(b: Byte);
begin
  Assert(_bytesUsed < WINDOW_SIZE, 'Can''t add byte when window is full!');
  _window[_end] := b;
  Inc(_end);
  _end := _end and WINDOW_MASK;
  Inc(_bytesUsed);
end;

procedure TOutputWindow.WriteLengthDistance(length, distance: Integer);
var
  copyStart, border: Integer;
begin
  Assert((_bytesUsed + length) <= WINDOW_SIZE, 'No Enough space');

  // move backwards distance bytes in the output stream,
  // and copy length bytes from this position to the output stream.
  _bytesUsed += length;
  copyStart := (_end - distance) and WINDOW_MASK; // start position for coping.

  border := WINDOW_SIZE - length;
  if (copyStart <= border) and (_end < border) then
  begin
      if (length <= distance) then
      begin
          Move(_window[copyStart], _window[_end], length);
          _end += length;
      end
      else
      begin
          // The referenced string may overlap the current
          // position; for example, if the last 2 bytes decoded have values
          // X and Y, a string reference with <length = 5, distance = 2>
          // adds X,Y,X,Y,X to the output stream.
          while (length > 0) do
          begin
              _window[_end] := _window[copyStart];
              Inc(_end);
              Dec(length);
              Inc(copyStart);
          end;
      end;
  end
  else
  begin
      // copy byte by byte
      while (length > 0) do
      begin
          _window[_end] := _window[copyStart];
          Inc(_end);
          Dec(length);
          Inc(copyStart);
          _end := _end and WINDOW_MASK;
          copyStart := copyStart and WINDOW_MASK;
      end;
  end;
end;

function TOutputWindow.CopyFrom(input: TInputBuffer; length: Integer): Integer;
var
  copied, tailLen: Integer;
begin
  length := Math.Min(Math.Min(length, WINDOW_SIZE - _bytesUsed), input.AvailableBytes);
  // We might need wrap around to copy all bytes.
  tailLen := WINDOW_SIZE - _end;
  if (length > tailLen) then
  begin
      // copy the first part
      copied := input.CopyTo(_window, _end, tailLen);
      if (copied = tailLen) then
      begin
          // only try to copy the second part if we have enough bytes in input
          copied += input.CopyTo(_window, 0, length - tailLen);
      end;
  end
  else
  begin
      // only one copy is needed if there is no wrap around.
      copied := input.CopyTo(_window, _end, length);
  end;

  _end := (_end + copied) and WINDOW_MASK;
  _bytesUsed += copied;
  Result := copied;
end;

function TOutputWindow.FreeBytes: Integer;
begin
  Result := WINDOW_SIZE - _bytesUsed;
end;

function TOutputWindow.CopyTo(output: PByte; offset, length: Integer): Integer;
var
  copyEnd, copied, tailLen: Integer;
begin
  if (length > _bytesUsed) then
  begin
      // we can copy all the decompressed bytes out
      copyEnd := _end;
      length := _bytesUsed;
  end
  else
  begin
      copyEnd := (_end - _bytesUsed + length) and WINDOW_MASK; // copy length of bytes
  end;

  copied := length;

  tailLen := length - copyEnd;
  if (tailLen > 0) then
  begin
      // this means we need to copy two parts separately
      // copy tailLen bytes from the end of output window
      Move(_window[WINDOW_SIZE - tailLen], output[offset], tailLen);
      offset += tailLen;
      length := copyEnd;
  end;
  if (length > 0) then
  begin
      Move(_window[copyEnd - length], output[offset], length);
  end;
  _bytesUsed -= copied;
  Assert(
      _bytesUsed >= 0,
      'check this function and find why we copied more bytes than we have'
  );
  Result := copied;
end;

end.

