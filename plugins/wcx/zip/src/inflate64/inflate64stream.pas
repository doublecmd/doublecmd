// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE.txt file in the directory for more information.
// The Pascal translation by Alexander Koblov.

unit Inflate64Stream;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, InflaterManaged;

type

  { TInflate64Stream }

  TInflate64Stream = class(TOwnerStream)
  private
    _buffer: array[Word] of Byte;
    _inflater: TInflaterManaged;
  public
    constructor Create(ASource: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

implementation

{ TInflate64Stream }

constructor TInflate64Stream.Create(ASource: TStream);
begin
  inherited Create(ASource);
  _inflater:= TInflaterManaged.Create(True);
end;

destructor TInflate64Stream.Destroy;
begin
  inherited Destroy;
  _inflater.Free;
end;

function TInflate64Stream.Read(var Buffer; Count: Longint): Longint;
var
  bytesRead, bytes: Integer;
  currentOffset, remainingCount: Integer;
begin
  currentOffset := 0;
  remainingCount := Count;

  while (true) do
  begin
      bytesRead := _inflater.Inflate(@Buffer, currentOffset, remainingCount);
      currentOffset += bytesRead;
      remainingCount -= bytesRead;

      if (remainingCount = 0) then
      begin
          break;
      end;

      if (_inflater.Finished()) then
      begin
          // if we finished decompressing, we can't have anything left in the outputwindow.
          Assert(
              _inflater.AvailableOutput = 0,
              'We should have copied all stuff out!'
          );
          break;
      end;

      bytes := FSource.Read(_buffer, Length(_buffer));
      if (bytes <= 0) then
      begin
          break;
      end
      else if (bytes > Length(_buffer)) then
      begin
          // The stream is either malicious or poorly implemented and returned a number of
          // bytes larger than the buffer supplied to it.
          raise Exception.Create('Deflate64: invalid data');
      end;

      _inflater.SetInput(_buffer, 0, bytes);
  end;

  Result := count - remainingCount;
end;

end.

