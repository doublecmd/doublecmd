(* ***** BEGIN LICENSE BLOCK *****
 * Simple interface to brotlidec library
 *
 * Copyright (C) 2025 Alexander Koblov (alexx2000@mail.ru)
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
{* ABBREVIA: AbBrotli.pas                                           *}
{********************************************************************}
{* ABBREVIA: TBrotliDecompressionStream class                       *}
{********************************************************************}

unit AbBrotli;

{$mode delphi}
{$packenum 4}

interface

uses
  Classes, SysUtils, CTypes;

type
  TBrotliDecoderResult =  (
    BROTLI_DECODER_RESULT_ERROR = 0,
    BROTLI_DECODER_RESULT_SUCCESS = 1,
    BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT = 2,
    BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT = 3
  );
  TBrotliDecoderErrorCode = cint;

  TBrotliDecoderState = record end;
  PBrotliDecoderState = ^TBrotliDecoderState;

  { TBrotliDecompressionStream }

  TBrotliDecompressionStream = class(TOwnerStream)
  private
    Fnext_in: PByte;
    Fnext_out: PByte;
    Ftotal_out: Int64;
    Favailable_in: csize_t;
    Favailable_out: csize_t;
    FRes: TBrotliDecoderResult;
    FState: PBrotliDecoderState;
    FBuffer: array[Word] of Byte;
  private
    function Check(Res: TBrotliDecoderResult): TBrotliDecoderResult;
  public
    constructor Create(ASource: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  EBrotliError = class(Exception);
  EBrotliDecompressionError = class(EBrotliError);

implementation

uses
  RtlConsts, DCOSUtils;

const
  libbrotli = {$IF DEFINED(MSWINDOWS)}
              'libbrotlidec.dll'
              {$ELSEIF DEFINED(DARWIN)}
              'libbrotlidec.dylib'
              {$ELSEIF DEFINED(UNIX)}
              'libbrotlidec.so.1'
              {$IFEND};

var
  hBrotli: TLibHandle = NilHandle;

var
  BrotliDecoderCreateInstance: function(alloc_func, free_func, opaque: Pointer): PBrotliDecoderState; cdecl;
  BrotliDecoderDestroyInstance: procedure(state: PBrotliDecoderState); cdecl;

  BrotliDecoderDecompressStream: function(state: PBrotliDecoderState; available_in: pcsize_t; const next_in: ppbyte;
                                          available_out: pcsize_t; next_out: PPByte; total_out: pcsize_t): TBrotliDecoderResult; cdecl;

  BrotliDecoderGetErrorCode: function(const state: PBrotliDecoderState): TBrotliDecoderErrorCode; cdecl;
  BrotliDecoderErrorString: function(c: TBrotliDecoderErrorCode): PAnsiChar; cdecl;

procedure Initialize;
begin
  if hBrotli <> NilHandle then Exit;

  hBrotli:= mbLoadLibraryEx(libbrotli);

  if hBrotli = NilHandle then
    raise EBrotliError.Create('Brotli shared library not found');

  @BrotliDecoderCreateInstance:= SafeGetProcAddress(hBrotli, 'BrotliDecoderCreateInstance');
  @BrotliDecoderDestroyInstance:= SafeGetProcAddress(hBrotli, 'BrotliDecoderDestroyInstance');
  @BrotliDecoderDecompressStream:= SafeGetProcAddress(hBrotli, 'BrotliDecoderDecompressStream');
  @BrotliDecoderGetErrorCode:= SafeGetProcAddress(hBrotli, 'BrotliDecoderGetErrorCode');
  @BrotliDecoderErrorString:= SafeGetProcAddress(hBrotli, 'BrotliDecoderErrorString');
end;

{ TBrotliDecompressionStream }

function TBrotliDecompressionStream.Check(Res: TBrotliDecoderResult): TBrotliDecoderResult;
var
  AError: TBrotliDecoderErrorCode;
begin
  if (Res = BROTLI_DECODER_RESULT_ERROR) then
  begin
    AError:= BrotliDecoderGetErrorCode(FState);
    raise EBrotliDecompressionError.Create(BrotliDecoderErrorString(AError));
  end;
  Result:= Res;
end;

constructor TBrotliDecompressionStream.Create(ASource: TStream);
begin
  Initialize;
  FRes:= BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT;
  FState:= BrotliDecoderCreateInstance(nil, nil, nil);
  if (FState = nil) then raise EBrotliError.Create(EmptyStr);
  inherited Create(ASource);
end;

destructor TBrotliDecompressionStream.Destroy;
begin
  inherited Destroy;
  BrotliDecoderDestroyInstance(FState);
end;

function TBrotliDecompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  Fnext_out:= @Buffer;
  Favailable_out:= Count;
  while (Favailable_out > 0) do
  begin
    if (Favailable_in = 0) then
    begin
      Fnext_in:= FBuffer;
      Favailable_in:= FSource.Read(FBuffer, SizeOf(FBuffer));
      if (Favailable_in = 0) and (FRes = BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT) then
      begin
        raise EBrotliDecompressionError.Create('Compressed file is truncated');
      end;
    end;
    FRes:= BrotliDecoderDecompressStream(FState, @Favailable_in, @Fnext_in, @Favailable_out, @Fnext_out, nil);
    if (Check(FRes) = BROTLI_DECODER_RESULT_SUCCESS) then Break;
  end;
  Result:= Count - Favailable_out;
  Inc(Ftotal_out, Result);
end;

function TBrotliDecompressionStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  if (Offset >= 0) and (Origin = soCurrent) then
  begin
    if (Offset > 0) then Discard(Offset);
    Result:= Ftotal_out;
  end
  else if (Origin = soBeginning) and (Ftotal_out = Offset) then
    Result:= Offset
  else begin
    raise EBrotliDecompressionError.CreateFmt(SStreamInvalidSeek, [ClassName]);
  end;
end;

end.

