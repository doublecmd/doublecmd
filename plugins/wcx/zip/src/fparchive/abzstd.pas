(* ***** BEGIN LICENSE BLOCK *****
 * Simple interface to zstd library
 *
 * Copyright (C) 2019 Alexander Koblov (alexx2000@mail.ru)
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

{**********************************************************************}
{* ABBREVIA: AbZstd.pas                                               *}
{**********************************************************************}
{* ABBREVIA: TZstdCompressionStream, TZstdDecompressionStream classes *}
{**********************************************************************}

unit AbZstd;

{$mode delphi}
{$packrecords c}

interface

uses
  Classes, SysUtils, CTypes;

const
  ZSTD_FRAMEHEADERSIZE_MAX = 18;
  ZSTD_CONTENTSIZE_UNKNOWN = UInt64(-1);
  ZSTD_CONTENTSIZE_ERROR   = UInt64(-2);

type
  PZSTD_CCtx = ^TZSTD_CCtx;
  TZSTD_CCtx = record end;

  PZSTD_DCtx = ^ZSTD_DCtx;
  ZSTD_DCtx = record end;

  PZSTD_inBuffer = ^ZSTD_inBuffer;
  ZSTD_inBuffer = record
    src: pcuint8;
    size: csize_t;
    pos: csize_t;
  end;

  PZSTD_outBuffer = ^ZSTD_outBuffer;
  ZSTD_outBuffer = record
    dst: pcuint8;
    size: csize_t;
    pos: csize_t;
  end;

  EZstdError = class(Exception);

  { TZstdCompressionStream }

  TZstdCompressionStream = class(TStream)
  private
    FStream: TStream;
    FContext: PZSTD_CCtx;
    FBufferOut: ZSTD_outBuffer;
  public
    constructor Create(OutStream: TStream; ALevel: Integer; InSize: UInt64 = ZSTD_CONTENTSIZE_UNKNOWN);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  { TZstdDecompressionStream }

  TZstdDecompressionStream = class(TStream)
  private
    FStream: TStream;
    FContext: PZSTD_DCtx;
    FBufferIn: ZSTD_inBuffer;
    FBufferInSize: UIntPtr;
    FBufferOut: ZSTD_outBuffer;
    FBufferOutPos: UIntPtr;
  public
    constructor Create(InStream: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

function ZSTD_FileSize(const InStream: TStream): UInt64;

implementation

uses
  DynLibs;

const
  libzstd = {$IF DEFINED(MSWINDOWS)}
            'libzstd.dll'
            {$ELSEIF DEFINED(DARWIN)}
            'libzstd.dylib'
            {$ELSEIF DEFINED(UNIX)}
            'libzstd.so.1'
            {$IFEND};

type
  ZSTD_cParameter = (
    ZSTD_c_compressionLevel = 100,
    ZSTD_c_checksumFlag = 201
  );

  ZSTD_EndDirective = (
    ZSTD_e_continue = 0,
    ZSTD_e_flush = 1,
    ZSTD_e_end = 2
  );

var
  ZSTD_CStreamInSize: function(): csize_t; cdecl;
  ZSTD_CStreamOutSize: function(): csize_t; cdecl;

  ZSTD_createCCtx: function(): PZSTD_CCtx; cdecl;
  ZSTD_freeCCtx: function(cctx: PZSTD_CCtx): csize_t; cdecl;

  ZSTD_CCtx_setParameter: function(cctx: PZSTD_CCtx; param: ZSTD_cParameter; value: cint): csize_t; cdecl;

  ZSTD_compressStream2: function(cctx: PZSTD_CCtx;
                                 output: PZSTD_outBuffer;
                                 input: PZSTD_inBuffer;
                                 endOp: ZSTD_EndDirective): csize_t; cdecl;

  ZSTD_DStreamInSize: function(): csize_t; cdecl;
  ZSTD_DStreamOutSize: function(): csize_t; cdecl;

  ZSTD_createDCtx: function(): PZSTD_DCtx; cdecl;
  ZSTD_freeDCtx: function(dctx: PZSTD_DCtx): csize_t; cdecl;

  ZSTD_decompressStream: function(zds: PZSTD_DCtx; output: PZSTD_outBuffer; input: PZSTD_inBuffer): csize_t; cdecl;

  ZSTD_isError: function(code: csize_t): cuint; cdecl;
  ZSTD_getErrorName: function(code: csize_t): PAnsiChar; cdecl;

  ZSTD_getFrameContentSize: function(src: pcuint8; srcSize: csize_t): cuint64; cdecl;
  ZSTD_CCtx_setPledgedSrcSize: function(cctx: PZSTD_CCtx; pledgedSrcSize: cuint64): csize_t; cdecl;

var
  hZstd: TLibHandle = NilHandle;

procedure Initialize;
begin
  if hZstd <> NilHandle then Exit;

  hZstd:= LoadLibrary(libzstd);

  if hZstd = NilHandle then
    raise EZstdError.Create('Zstd shared library not found');

  @ZSTD_CStreamInSize:= GetProcAddress(hZstd, 'ZSTD_CStreamInSize');
  @ZSTD_CStreamOutSize:= GetProcAddress(hZstd, 'ZSTD_CStreamOutSize');

  @ZSTD_createCCtx:= GetProcAddress(hZstd, 'ZSTD_createCCtx');
  @ZSTD_freeCCtx:= GetProcAddress(hZstd, 'ZSTD_freeCCtx');

  @ZSTD_CCtx_setParameter:= GetProcAddress(hZstd, 'ZSTD_CCtx_setParameter');
  @ZSTD_compressStream2:= GetProcAddress(hZstd, 'ZSTD_compressStream2');

  @ZSTD_DStreamInSize:= GetProcAddress(hZstd, 'ZSTD_DStreamInSize');
  @ZSTD_DStreamOutSize:= GetProcAddress(hZstd, 'ZSTD_DStreamOutSize');

  @ZSTD_createDCtx:= GetProcAddress(hZstd, 'ZSTD_createDCtx');
  @ZSTD_freeDCtx:= GetProcAddress(hZstd, 'ZSTD_freeDCtx');

  @ZSTD_decompressStream:= GetProcAddress(hZstd, 'ZSTD_decompressStream');

  @ZSTD_isError:= GetProcAddress(hZstd, 'ZSTD_isError');
  @ZSTD_getErrorName:= GetProcAddress(hZstd, 'ZSTD_getErrorName');

  @ZSTD_getFrameContentSize:= GetProcAddress(hZstd, 'ZSTD_getFrameContentSize');
  @ZSTD_CCtx_setPledgedSrcSize:= GetProcAddress(hZstd, 'ZSTD_CCtx_setPledgedSrcSize');
end;

function ZSTD_Check(code: csize_t): csize_t;
begin
  Result:= code;
  if (ZSTD_isError(code) <> 0) then
    raise EZstdError.Create(ZSTD_getErrorName(code))
end;

function ZSTD_FileSize(const InStream: TStream): UInt64;
var
  APosition: Int64;
  ABuffer: array[1..ZSTD_FRAMEHEADERSIZE_MAX] of Byte;
begin
  Initialize;

  APosition:= InStream.Position;
  InStream.Seek(0, soBeginning);
  InStream.Read(ABuffer[1], ZSTD_FRAMEHEADERSIZE_MAX);
  InStream.Seek(APosition, soBeginning);

  Result:= ZSTD_getFrameContentSize(@ABuffer[1], ZSTD_FRAMEHEADERSIZE_MAX);

  if (Result = ZSTD_CONTENTSIZE_UNKNOWN) or (Result = ZSTD_CONTENTSIZE_ERROR) then
    Result:= 0;
end;

{ TZstdDecompressionStream }

constructor TZstdDecompressionStream.Create(InStream: TStream);
begin
  Initialize;

  FStream:= InStream;
  FContext:= ZSTD_createDCtx();

  FBufferInSize:= ZSTD_DStreamInSize();
  FBufferIn.size := FBufferInSize;
  FBufferIn.pos:= FBufferInSize;
  FBufferIn.src:= GetMem(FBufferIn.size);

  FBufferOut.pos:= 0;
  FBufferOut.size := ZSTD_DStreamOutSize();
  FBufferOut.dst:= GetMem(FBufferOut.size);
end;

destructor TZstdDecompressionStream.Destroy;
begin
  if Assigned(FContext) then
  begin
    FreeMem(FBufferIn.src);
    FreeMem(FBufferOut.dst);
    ZSTD_freeDCtx(FContext);
  end;
  inherited Destroy;
end;

function TZstdDecompressionStream.Read(var Buffer; Count: Longint): Longint;
var
  ABuffer: PByte;
  Available: Integer;
begin
  Result:= 0;
  ABuffer:= @Buffer;

  while (Count > 0) do
  begin
    Available:= FBufferOut.pos - FBufferOutPos;

    if (Available > 0) then
    begin
      if Available > Count then
        Available:= Count;

      Move(FBufferOut.dst[FBufferOutPos], ABuffer^, Available);
      Inc(Result, Available);

      if (Available = Count) then
      begin
        Inc(FBufferOutPos, UIntPtr(Available));
        Break;
      end;

      Inc(ABuffer, Available);
      Dec(Count, Available);
    end;

    if (FBufferIn.size > 0) and (FBufferIn.pos = FBufferIn.size) then
    begin
      FBufferIn.pos:= 0;
      FBufferIn.size:= FStream.Read(FBufferIn.src^, FBufferInSize);
    end;

    FBufferOutPos:= 0;
    FBufferOut.pos:= 0;
    ZSTD_Check(ZSTD_decompressStream(FContext, @FBufferOut, @FBufferIn));

    if (FBufferOut.pos = 0) and (FBufferIn.size = 0) then Break;
  end;
end;

function TZstdDecompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result:= -1;
end;

{ TZstdCompressionStream }

constructor TZstdCompressionStream.Create(OutStream: TStream; ALevel: Integer;
  InSize: UInt64);
begin
  Initialize;

  FStream:= OutStream;
  FContext:= ZSTD_createCCtx();

  FBufferOut.size:= ZSTD_CStreamOutSize();
  FBufferOut.dst:= GetMem(FBufferOut.size);

  ZSTD_CCtx_setPledgedSrcSize(FContext, InSize);
  ZSTD_Check(ZSTD_CCtx_setParameter(FContext, ZSTD_c_checksumFlag, 1));
  ZSTD_Check(ZSTD_CCtx_setParameter(FContext, ZSTD_c_compressionLevel, ALevel));
end;

destructor TZstdCompressionStream.Destroy;
var
  ARemaining: csize_t;
  AInput: ZSTD_inBuffer;
begin
  try
    FillChar({%H-}AInput, SizeOf(ZSTD_inBuffer), 0);

    repeat
      FBufferOut.pos:= 0;

      ARemaining:= ZSTD_Check(ZSTD_compressStream2(FContext, @FBufferOut, @AInput, ZSTD_e_end));

      if (FBufferOut.pos > 0) then begin
        FStream.WriteBuffer(FBufferOut.dst^, FBufferOut.pos);
      end;
    until (ARemaining = 0);
  finally
    FreeMem(FBufferOut.dst);
    ZSTD_freeCCtx(FContext);
    inherited Destroy;
  end;
end;

function TZstdCompressionStream.Write(const Buffer; Count: Longint): Longint;
var
  AInput: ZSTD_inBuffer;
begin
  AInput.pos:= 0;
  AInput.src:= @Buffer;
  AInput.size:= Count;

  while AInput.pos < AInput.size do
  begin
    FBufferOut.pos:= 0;

    ZSTD_Check(ZSTD_compressStream2(FContext, @FBufferOut, @AInput, ZSTD_e_continue));

    if (FBufferOut.pos > 0) then
      FStream.WriteBuffer(FBufferOut.dst^, FBufferOut.pos);
  end;

  Result:= AInput.pos;
end;

function TZstdCompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result:= -1;
end;

end.

