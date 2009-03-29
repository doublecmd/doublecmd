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
{* ABBREVIA: AbBitBkt.pas 3.05                           *}
{*********************************************************}
{* ABBREVIA: Bit bucket memory stream class              *}
{*********************************************************}

{$I AbDefine.inc}

unit AbBitBkt;

interface

uses
  Classes;

type
  TAbBitBucketStream = class(TStream)
    private
      FBuffer  : PChar;
      FBufSize : longint;
      FBufPosn : longint;
      FPosn    : Int64;
      FSize    : Int64;
      FTail    : Int64;
    protected
    public
      constructor Create(aBufSize : cardinal);
      destructor Destroy; override;
      function Read(var Buffer; Count : Longint) : Longint; override;
      function Write(const Buffer; Count : Longint) : Longint; override;
      function Seek(const Offset : Int64; Origin : TSeekOrigin) : Int64; override;

      procedure ForceSize(aSize : Int64);
  end;

implementation

uses
  SysUtils, AbExcept;

{Notes: The buffer is a circular queue without a head pointer; FTail
        is where data is next going to be written and it wraps
        indescriminately. The buffer can never be empty--it is always
        full (initially it is full of binary zeros.
        The class is designed to act as a bit bucket for the test
        feature of Abbrevia's zip code; it is not intended as a
        complete class with many possible applications. It is designed
        to be written to in a steady progression with some reading
        back in the recently written stream (the buffer size details
        how far back the Seek method will work). Seeking outside this
        buffer will result in exceptions being generated.
        For testing deflated files, the buffer size should be 32KB,
        for imploded files, either 8KB or 4KB. The Create constructor
        limits the buffer size to these values.}

{===TAbBitBucketStream===============================================}
constructor TAbBitBucketStream.Create(aBufSize : cardinal);
begin
  inherited Create;
  if (aBufSize <> 4096) and
     (aBufSize <> 8192) and
     (aBufSize <> 32768) then
    FBufSize := 32768
  else
    FBufSize := aBufSize;
  {add a 1KB leeway}
  inc(FBufSize, 1024);
  GetMem(FBuffer, FBufSize);
end;
{--------}
destructor TAbBitBucketStream.Destroy;
begin
  if (FBuffer <> nil) then
    FreeMem(FBuffer, FBufSize);
  inherited Destroy;
end;
{--------}
procedure TAbBitBucketStream.ForceSize(aSize : Int64);
begin
  FSize := aSize;
end;
{--------}
function TAbBitBucketStream.Read(var Buffer; Count : Longint) : Longint;
var
  Chunk2Size : Int64;
  Chunk1Size : Int64;
  OutBuffer  : TByteArray absolute Buffer;
begin
  {we cannot read more bytes than there is buffer}
  if (Count > FBufSize) then
    raise EAbBBSReadTooManyBytes.Create(Count, 0);
  {calculate the size of the chunks}
  if (FBufPosn <= FTail) then begin
    Chunk1Size := FTail - FBufPosn;
    if (Chunk1Size > Count) then
      Chunk1Size := Count;
    Chunk2Size := 0;
  end
  else begin
    Chunk1Size := FBufSize - FBufPosn;
    if (Chunk1Size > Count) then begin
      Chunk1Size := Count;
      Chunk2Size := 0;
    end
    else begin
      Chunk2Size := FTail;
      if (Chunk2Size > (Count - Chunk1Size)) then
        Chunk2Size := Count - Chunk1Size;
    end
  end;
  {we cannot read more bytes than there are available}
  if (Count > (Chunk1Size + Chunk2Size)) then
    raise EAbBBSReadTooManyBytes.Create(Count, 0);
  {perform the read}
  if (Chunk1Size > 0) then begin
    Move(FBuffer[FBufPosn], OutBuffer[0], Chunk1Size);
    inc(FBufPosn, Chunk1Size);
    inc(FPosn, Chunk1Size);
  end;
  if (Chunk2Size > 0) then begin
    {we've wrapped}
    Move(FBuffer[0], OutBuffer[Chunk1Size], Chunk2Size);
    FBufPosn := Chunk2Size;
    inc(FPosn, Chunk2Size);
  end;
  Result := Count;
end;
{--------}
function TAbBitBucketStream.Write(const Buffer; Count : Longint) : Longint;
var
  Chunk2Size : longint;
  Chunk1Size : longint;
  InBuffer   : TByteArray absolute Buffer;
begin
  {we cannot write more bytes than there is buffer}
  if (Count > FBufSize) then
    raise EAbBBSWriteTooManyBytes.Create(Count, 0);
  {calculate the size of the chunks}
  Chunk1Size := FBufSize - FTail;
  if (Chunk1Size > Count) then begin
    Chunk1Size := Count;
    Chunk2Size := 0;
  end
  else begin
    Chunk2Size := Count - Chunk1Size;
  end;
  {write the first chunk}
  if (Chunk1Size > 0) then begin
    Move(InBuffer[0], FBuffer[FTail], Chunk1Size);
    inc(FTail, Chunk1Size);
  end;
  {if the second chunk size is not zero, write the second chunk; note
   that we have wrapped}
  if (Chunk2Size > 0) then begin
    Move(InBuffer[Chunk1Size], FBuffer[0], Chunk2Size);
    FTail := Chunk2Size;
  end;
  {the stream size and position have changed}
  inc(FSize, Count);
  FPosn := FSize;
  FBufPosn := FTail;
  Result := Count;
end;
{--------}
function TAbBitBucketStream.Seek(const Offset : Int64; Origin : TSeekOrigin): Int64;
var
  Posn : longint;
  BytesBack : longint;
begin
{$IFDEF LINUX}
{$IFDEF VER60}
{Bug in Kylix 1 code analysis? Complains that Posn not initialized.}
{All the other compilers complain that this assignment to Posn never used. }
  Posn := 0;                                         {!!.03}
{$ENDIF}
{$ENDIF}
  {calculate the new position}
  case Origin of
    soBeginning :
      Posn := Offset;
    soCurrent   :
      Posn := FPosn + Offset;
    soEnd       :
      if (Offset = 0) then begin
        {special case: position at end of stream}
        FBufPosn := FTail;
        FPosn := FSize;
        Result := FSize;
        Exit;
      end
      else begin
        Posn := FSize + Offset;
      end;
  else
    raise EAbBBSInvalidOrigin.Create;
  end;
  {calculate whether the new position is within the buffer; if not,
   raise exception}
  if (Posn > FSize) or
     (Posn <= (FSize - FBufSize)) then
    raise EAbBBSSeekOutsideBuffer.Create;
  {set the internal fields for the new position}
  FPosn := Posn;
  BytesBack := FSize - Posn;
  if (BytesBack <= FTail) then
    FBufPosn := FTail - BytesBack
  else
    FBufPosn := longint(FTail) + FBufSize - BytesBack;
  {return the new position}
  Result := Posn;
end;
{====================================================================}


end.
