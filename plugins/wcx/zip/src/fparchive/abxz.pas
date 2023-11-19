(* ***** BEGIN LICENSE BLOCK *****
 * Simple interface to lzma library
 *
 * Copyright (C) 2014-2023 Alexander Koblov (alexx2000@mail.ru)
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:

 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * ***** END LICENSE BLOCK ***** *)

{********************************************************************}
{* ABBREVIA: AbXz.pas                                               *}
{********************************************************************}
{* ABBREVIA: TXzDecompressionStream, TXzDecompressionStream classes *}
{********************************************************************}

unit AbXz;

{$mode delphi}
{$packrecords c}

interface

uses
  Classes, SysUtils, CTypes;

type
  TLzmaStreamRec = record
    next_in: pbyte;     (**< Pointer to the next input byte. *)
    avail_in: csize_t;  (**< Number of available input bytes in next_in. *)
    total_in: cuint64;  (**< Total number of bytes read by liblzma. *)

    next_out: pbyte;    (**< Pointer to the next output position. *)
    avail_out: csize_t; (**< Amount of free space in next_out. *)
    total_out: cuint64; (**< Total number of bytes written by liblzma. *)

    (**
     * \brief       Custom memory allocation functions
     *
     * In most cases this is NULL which makes liblzma use
     * the standard malloc() and free().
     *)
    allocator: pointer;

    (** Internal state is not visible to applications. *)
    internal: pointer;

    (*
     * Reserved space to allow possible future extensions without
     * breaking the ABI. Excluding the initialization of this structure,
     * you should not touch these, because the names of these variables
     * may change.
     *)
    reserved_ptr1: pointer;
    reserved_ptr2: pointer;
    reserved_ptr3: pointer;
    reserved_ptr4: pointer;
    reserved_int1: cuint64;
    reserved_int2: cuint64;
    reserved_int3: csize_t;
    reserved_int4: csize_t;
    reserved_enum1: cuint32;
    reserved_enum2: cuint32;
  end;

type

  { TXzCustomStream }

  TXzCustomStream = class(TOwnerStream)
  protected
    FLzmaRec: TLzmaStreamRec;
    FBuffer: array[Word] of Byte;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
  end;

  { TXzCompressionStream }

  TXzCompressionStream = class(TXzCustomStream)
  private
    procedure FlushBuffer;
    function Check(Return: cint): cint;
  public
    constructor Create(ATarget: TStream; ALevel: Integer);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  { TXzDecompressionStream }

  TXzDecompressionStream = class(TXzCustomStream)
  private
    function Check(Return: cint): cint;
  public
    constructor Create(ASource: TStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  ELzmaError = class(Exception);
  ELzmaCompressionError = class(ELzmaError);
  ELzmaDecompressionError = class(ELzmaError);

implementation

uses
  DynLibs, RtlConsts;

const
  // Lzma return codes
  LZMA_OK                 = 0;
  LZMA_STREAM_END         = 1;
  LZMA_NO_CHECK           = 2;
  LZMA_UNSUPPORTED_CHECK  = 3;
  LZMA_GET_CHECK          = 4;
  LZMA_MEM_ERROR          = 5;
  LZMA_MEMLIMIT_ERROR     = 6;
  LZMA_FORMAT_ERROR       = 7;
  LZMA_OPTIONS_ERROR      = 8;
  LZMA_DATA_ERROR         = 9;
  LZMA_BUF_ERROR          = 10;
  LZMA_PROG_ERROR         = 11;

const
  // Lzma actions
  LZMA_RUN        = 0;
  LZMA_SYNC_FLUSH = 1;
  LZMA_FULL_FLUSH = 2;
  LZMA_FINISH     = 3;

const
  // Type of the integrity check (Check ID)
  LZMA_CHECK_CRC64    = 4;

const
  // Decoding flags
  LZMA_TELL_UNSUPPORTED_CHECK = $02;
  LZMA_CONCATENATED           = $08;

const
  liblzma = {$IF DEFINED(MSWINDOWS)}
            'liblzma.dll'
            {$ELSEIF DEFINED(DARWIN)}
            'liblzma.dylib'
            {$ELSEIF DEFINED(UNIX)}
            'liblzma.so.5'
            {$IFEND};

var
  hLzma: TLibHandle = NilHandle;

var
  lzma_stream_decoder: function(var strm: TLzmaStreamRec; memlimit: cuint64; flags: cuint32): cint; cdecl;
  lzma_easy_encoder: function(var strm: TLzmaStreamRec; preset: cuint32; check: cint): cint; cdecl;
  lzma_code: function(var strm: TLzmaStreamRec; action: cint): cint; cdecl;
  lzma_end: procedure(var strm: TLzmaStreamRec); cdecl;

procedure LzmaLoadLibrary;
begin
  if hLzma <> NilHandle then Exit;

  hLzma := LoadLibrary(liblzma);
  if hLzma = NilHandle then
    raise ELzmaError.Create('Lzma shared library not found');

  @lzma_stream_decoder := GetProcAddress(hLzma, 'lzma_stream_decoder');
  @lzma_easy_encoder := GetProcAddress(hLzma, 'lzma_easy_encoder');
  @lzma_code := GetProcAddress(hLzma, 'lzma_code');
  @lzma_end := GetProcAddress(hLzma, 'lzma_end');
end;

constructor TXzCustomStream.Create(AStream: TStream);
begin
  LzmaLoadLibrary;
  inherited Create(AStream);
end;

destructor TXzCustomStream.Destroy;
begin
  if (@lzma_end <> nil) then lzma_end(FLzmaRec);
  inherited Destroy;
end;

{ TXzCompressionStream }

function TXzCompressionStream.Check(Return: cint): cint;
var
  Message: String;
begin
  Result:= Return;
  if not (Return in [LZMA_OK, LZMA_STREAM_END]) then
  begin
    case Return of
      LZMA_MEM_ERROR:
        Message:= 'Memory allocation failed';
      LZMA_OPTIONS_ERROR:
        Message:= 'Specified preset is not supported';
      LZMA_UNSUPPORTED_CHECK:
        Message:= 'Specified integrity check is not supported';
      LZMA_FORMAT_ERROR:
        Message:= 'The input is not in the .xz format';
      LZMA_DATA_ERROR:
        Message:= 'File size limits exceeded';
      else
        Message:= Format('Unknown error, possibly a bug (error code %d)', [Return]);
    end;
    raise ELzmaCompressionError.Create(Message);
  end;
end;

constructor TXzCompressionStream.Create(ATarget: TStream; ALevel: Integer);
begin
  inherited Create(ATarget);
  FLzmaRec.next_out:= FBuffer;
  FLzmaRec.avail_out:= SizeOf(FBuffer);
  Check(lzma_easy_encoder(FLzmaRec, ALevel, LZMA_CHECK_CRC64));
end;

function TXzCompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  FLzmaRec.avail_in:= Count;
  FLzmaRec.next_in:= @Buffer;
  while FLzmaRec.avail_in > 0 do
  begin
    Check(lzma_code(FLzmaRec, LZMA_RUN));
    if FLzmaRec.avail_out = 0 then FlushBuffer;
  end;
  Result:= Count;
end;

function TXzCompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Offset = 0) and (Origin = soCurrent) then
    Result:= FLzmaRec.total_in
  else if (Origin = soBeginning) and (FLzmaRec.total_in = Offset) then
    Result:= Offset
  else begin
    raise ELzmaCompressionError.CreateFmt(SStreamInvalidSeek, [ClassName]);
  end;
end;

procedure TXzCompressionStream.FlushBuffer;
begin
  FLzmaRec.next_out:= FBuffer;
  FLzmaRec.avail_out:= SizeOf(FBuffer);
  FSource.WriteBuffer(FBuffer, SizeOf(FBuffer));
end;

destructor TXzCompressionStream.Destroy;
var
  State: cint;
begin
  try
    repeat
      if FLzmaRec.avail_out = 0 then FlushBuffer;
      State:= Check(lzma_code(FLzmaRec, LZMA_FINISH));
    until State = LZMA_STREAM_END;

    if FLzmaRec.avail_out < SizeOf(FBuffer) then
    begin
      FSource.WriteBuffer(FBuffer, SizeOf(FBuffer) - FLzmaRec.avail_out);
    end;
  finally
    inherited Destroy;
  end;
end;

{ TXzDecompressionStream }

function TXzDecompressionStream.Check(Return: cint): cint;
var
  Message: String;
begin
  Result:= Return;
  if not (Return in [LZMA_OK, LZMA_STREAM_END]) then
  begin
    case Return of
      LZMA_MEM_ERROR:
        Message:= 'Memory allocation failed';
      LZMA_OPTIONS_ERROR:
        Message:= 'Unsupported decompressor flags';
      LZMA_FORMAT_ERROR:
        Message:= 'The input is not in the .xz format';
      LZMA_DATA_ERROR:
        Message:= 'Compressed file is corrupt';
      LZMA_BUF_ERROR:
        Message:= 'Compressed file is truncated or otherwise corrupt';
      else
        Message:= Format('Unknown error, possibly a bug (error code %d)', [Return]);
    end;
    raise ELzmaDecompressionError.Create(Message);
  end;
end;

constructor TXzDecompressionStream.Create(ASource: TStream);
const
  flags = LZMA_TELL_UNSUPPORTED_CHECK or LZMA_CONCATENATED;
var
  memory_limit: cuint64 = High(cuint64);
begin
  inherited Create(ASource);
  Check(lzma_stream_decoder(FLzmaRec, memory_limit, flags));
end;

function TXzDecompressionStream.Read(var Buffer; Count: Longint): Longint;
var
  State: cint;
  Action: cint = LZMA_RUN;
begin
  FLzmaRec.avail_out:= Count;
  FLzmaRec.next_out:= @Buffer;
  while FLzmaRec.avail_out > 0 do
  begin
    if FLzmaRec.avail_in = 0 then
    begin
      FLzmaRec.next_in:= FBuffer;
      FLzmaRec.avail_in:= FSource.Read(FBuffer, SizeOf(FBuffer));
      if FLzmaRec.avail_in = 0 then Action:= LZMA_FINISH;
    end;
    State:= Check(lzma_code(FLzmaRec, Action));
    if State = LZMA_STREAM_END then Break;
  end;
  Result:= Count - FLzmaRec.avail_out;
end;

function TXzDecompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Offset >= 0) and (Origin = soCurrent) then
  begin
    if (Offset > 0) then Discard(Offset);
    Result:= FLzmaRec.total_out;
  end
  else if (Origin = soBeginning) and (FLzmaRec.total_out = Offset) then
    Result:= Offset
  else begin
    raise ELzmaDecompressionError.CreateFmt(SStreamInvalidSeek, [ClassName]);
  end;
end;

end.

