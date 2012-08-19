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
{* ABBREVIA: AbDfOutW.pas                                *}
{*********************************************************}
{* Deflate output sliding window                         *}
{*********************************************************}

unit AbDfOutW;

{$I AbDefine.inc}

interface

uses
  Classes,
  AbDfBase;

{Notes: TAbDfOutputWindow implements a sliding window on previously
        written data for the LZ77 dictionary decoding.

        AddLiteral will add a literal character at the current
        position and advance by one. AddLenDist will copy the required
        number of characters from the given position to the current
        position, and advance the stream on by the length. The class
        will periodically update the stream from the internal buffer.

        For normal Deflate, the internal buffer is 48K + 512 bytes in
        size. Once there is 48Kb worth of data, 16KB is written to
        file, and the buffer is shifted left by 16KB. We need to keep
        the last decoded 32KB in memory at all times.

        For Deflate64, the internal buffer is 96K + 512 bytes in
        size. Once there is 96Kb worth of data, 32KB is written to
        file, and the buffer is shifted left by 32KB. We need to keep
        the last decoded 64KB in memory at all times.
        }

type
  TAbDfOutputWindow = class
    private
      FBuffer     : PAnsiChar;
      FChecksum   : longint;
      FCurrent    : PAnsiChar;
      FLog        : TAbLogger;
      FPartSize   : longint;
      FSlideCount : integer;
      FStream     : TStream;
      FStreamPos  : longint;
      FTestOnly   : boolean;
      FUseCRC32   : boolean;
      FWritePoint : PAnsiChar;
    protected
      function swGetChecksum : longint;
      procedure swWriteToStream(aFlush : boolean);
    public
      constructor Create(aStream       : TStream;
                         aUseDeflate64 : boolean;
                         aUseCRC32     : boolean;
                         aPartSize     : longint;
                         aTestOnly     : boolean;
                         aLog          : TAbLogger);
      destructor Destroy; override;

      procedure AddBuffer(var aBuffer; aCount : integer);
      procedure AddLiteral(aCh : AnsiChar);
      procedure AddLenDist(aLen : integer; aDist : integer);
      function Position : longint;

      property Checksum : longint read swGetChecksum;
      property Log : TAbLogger read FLog;
  end;

implementation

uses
  SysUtils;

{Notes:
        Meaning of the internal pointers:

        |==============================+------------------------+----|
        |                              |                        |
        FBuffer                 FCurrent              FWritePoint

        Once FCurrent reaches or exceeds FWritePoint, FSlideCount
        bytes of data from FBuffer are written to the stream and the
        remaining data is moved back FSlideCount bytes, moving
        FCurrent along with it as well.
        }

{===TAbDfOutputWindow==================================================}
constructor TAbDfOutputWindow.Create(aStream       : TStream;
                                     aUseDeflate64 : boolean;
                                     aUseCRC32     : boolean;
                                     aPartSize     : longint;
                                     aTestOnly     : boolean;
                                     aLog          : TAbLogger);
var
  Size          : integer;
  LookAheadSize : integer;
begin
  {allow the ancestor to initialize}
  inherited Create;

  {save parameters}
  FLog := aLog;
  FStream := aStream;
  FTestOnly := aTestOnly;
  if (aPartSize <= 0) then
    FPartSize := 0
  else
    FPartSize := aPartSize;
  FUseCRC32 := aUseCRC32;
  if aUseCRC32 then
    FChecksum := -1  { CRC32 starts off with all bits set}
  else
    FCheckSum := 1;  { Adler32 starts off with a value of 1}

  {set capacity of sliding window}
  if aUseDeflate64 then begin
    Size := 96 * 1024;
    FSlideCount := 32 * 1024;
    LookAheadSize := 64 * 1024;
  end
  else begin
    Size := 64 * 1024;
    FSlideCount := 32 * 1024;
    LookAheadSize := 258;
  end;
  GetMem(FBuffer, Size + LookAheadSize);

  {set the other internal pointers}
  FCurrent := FBuffer;
  FWritePoint := FBuffer + Size;
  if (FPartSize > Size) then
    FPartSize := Size;
end;
{--------}
destructor TAbDfOutputWindow.Destroy;
begin
  {write remaining data and free the buffer}
  if (FBuffer <> nil) then begin
    if (FCurrent <> FBuffer) then
      swWriteToStream(true);
    FreeMem(FBuffer);
  end;

  {destroy the ancestor}
  inherited Destroy;
end;
{--------}
procedure TAbDfOutputWindow.AddBuffer(var aBuffer; aCount : integer);
var
  Buffer : PAnsiChar;
  BytesToWrite : integer;
begin
  {if we've advanced to the point when we need to write, do so}
  if (FCurrent >= FWritePoint) then
    swWriteToStream(false);

  {cast the user buffer to a PChar, it's easier to use}
  Buffer := @aBuffer;

  {calculate the number of bytes to copy}
  BytesToWrite := FWritePoint - FCurrent;
  if (BytesToWrite > aCount) then
    BytesToWrite := aCount;

  {move this block of bytes}
  Move(Buffer^, FCurrent^, BytesToWrite);

  {advance pointers and counters}
  inc(FCurrent, BytesToWrite);
  dec(aCount, BytesToWrite);

  {while there is still data to copy...}
  while (aCount > 0) do begin
    {advance the user buffer pointer}
    inc(Buffer, BytesToWrite);

    {write the sliding window chunk to the stream}
    swWriteToStream(false);

    {calculate the number of bytes to copy}
    BytesToWrite := FWritePoint - FCurrent;
    if (BytesToWrite > aCount) then
      BytesToWrite := aCount;

    {move this block of bytes}
    Move(Buffer^, FCurrent^, BytesToWrite);

    {advance pointers and counters}
    inc(FCurrent, BytesToWrite);
    dec(aCount, BytesToWrite);
  end;
end;
{--------}
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
{--------}
procedure TAbDfOutputWindow.AddLenDist(aLen : integer; aDist : integer);
var
  i : integer;
  ToChar   : PAnsiChar;
  FromChar : PAnsiChar;
begin
  {log it}
  {$IFDEF UseLogging}
  if (FLog <> nil) then
    AddLenDistToLog(FLog, Position, aLen, aDist, (aLen > aDist));
  {$ENDIF}

  {if the length to copy is less than the distance, just do a move}
  if (aLen <= aDist) then begin
    Move((FCurrent - aDist)^ , FCurrent^, aLen);
  end

  {otherwise we have to use a byte-by-byte copy}
  else begin
    FromChar := FCurrent - aDist;
    ToChar := FCurrent;
    for i := 1 to aLen do begin
      ToChar^ := FromChar^;
      inc(FromChar);
      inc(ToChar);
    end;
  end;

  {increment the current pointer}
  inc(FCurrent, aLen);

  {if we've reached the point requested, abort}
  if (FPartSize > 0) and ((FCurrent - FBuffer) >= FPartSize) then
    raise EAbPartSizedInflate.Create(''); {NOTE: This exception is expected during detection of .GZ and .TGZ files. (VerifyGZip)}

  {if we've advanced to the point when we need to write, do so}
  if (FCurrent >= FWritePoint) then
    swWriteToStream(false);
end;
{--------}
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
{--------}
procedure TAbDfOutputWindow.AddLiteral(aCh : AnsiChar);
begin
  {log it}
  {$IFDEF UseLogging}
  if (FLog <> nil) then
    AddLiteralToLog(FLog, Position, aCh);
  {$ENDIF}

  {add the literal to the buffer}
  FCurrent^ := aCh;

  {increment the current pointer}
  inc(FCurrent);

  {if we've reached the point requested, abort}
  if (FPartSize > 0) and ((FCurrent - FBuffer) >= FPartSize) then
    raise EAbPartSizedInflate.Create('');

  {if we've advanced to the point when we need to write, do so}
  if (FCurrent >= FWritePoint) then
    swWriteToStream(false);
end;
{--------}
function TAbDfOutputWindow.Position : longint;
begin
  if FTestOnly then
    Result := FStreamPos + (FCurrent - FBuffer)
  else
    Result := FStream.Position + (FCurrent - FBuffer);
end;
{--------}
function TAbDfOutputWindow.swGetChecksum : longint;
begin
  {since the checksum is calculated by the method that flushes to the
   stream, make sure any buffered data is written out first}
  if (FCurrent <> FBuffer) then
    swWriteToStream(true);

  {the CRC32 checksum algorithm requires a post-conditioning step
   after being calculated (the result is NOTted), whereas Adler32 does
   not}
  if FUseCRC32 then
    Result := not FChecksum
  else
    Result := FChecksum;
end;
{--------}
procedure TAbDfOutputWindow.swWriteToStream(aFlush : boolean);
var
  FromPtr : PAnsiChar;
begin
  {if the request was to flush, write all remaining data after
   updating the checksum}
  if aFlush then begin
    if FUseCRC32 then
      AbUpdateCRCBuffer(FChecksum, FBuffer^, FCurrent - FBuffer)
    else
      AbUpdateAdlerBuffer(FChecksum, FBuffer^, FCurrent - FBuffer);
    if FTestOnly then
      inc(FStreamPos, FCurrent - FBuffer)
    else
      FStream.WriteBuffer(FBuffer^, FCurrent - FBuffer);
    FCurrent := FBuffer;
  end

  {otherwise, update the checksum with the data in the sliding window
   chunk, write it out to the stream, and move the rest of the buffer
   back}
  else begin
    if FUseCRC32 then
      AbUpdateCRCBuffer(FChecksum, FBuffer^, FSlideCount)
    else
      AbUpdateAdlerBuffer(FChecksum, FBuffer^, FSlideCount);
    if FTestOnly then
      inc(FStreamPos, FSlideCount)
    else
      FStream.WriteBuffer(FBuffer^, FSlideCount);
    FromPtr := FBuffer + FSlideCount;
    Move(FromPtr^, FBuffer^, FCurrent - FromPtr);
    FCurrent := FCurrent - FSlideCount;
  end;
end;
{====================================================================}

end.

