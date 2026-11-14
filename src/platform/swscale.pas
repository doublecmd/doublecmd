{
  Double Commander
  -------------------------------------------------------------------------
  Scale bitmap using swscale library

  Copyright (C) 2025 jdp1024 (ilford@gmail.com)
  Copyright (C) 2025-2026 Alexander Koblov (alexx2000@mail.ru)

  Permission is hereby granted, free of charge, to any person obtaining
  a copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including
  without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to
  permit persons to whom the Software is furnished to do so, subject to
  the following conditions:

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

unit swscale;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics;

function Available: Boolean;
function Scale(Bitmap, DstBitmap: TRasterImage; DstWidth, DstHeight: Cardinal): Boolean;

implementation

uses
  Math, IntfGraphics, GraphType, FPImage, DynLibs, DCOSUtils;

type
  TRawAccess = class(TRasterImage);

type
  PSwsVector = ^TSwsVector;
  TSwsVector = record
    coeff: PDouble;
    length: Integer;
  end;

  PSwsFilter = ^TSwsFilter;
  TSwsFilter = record
    lumH: PSwsVector;
    lumV: PSwsVector;
    chrH: PSwsVector;
    chrV: PSwsVector;
  end;

  PPSwsContext = ^PSwsContext;
  PSwsContext = ^TSwsContext;
  TSwsContext = record
  end;

const
  SWS_FAST_BILINEAR = $0001;
  SWS_BILINEAR      = $0002;
  SWS_BICUBIC       = $0004;
  SWS_X             = $0008;
  SWS_POINT         = $0010;
  SWS_AREA          = $0020;
  SWS_BICUBLIN      = $0040;
  SWS_GAUSS         = $0080;
  SWS_SINC          = $0100;
  SWS_LANCZOS       = $0200;
  SWS_SPLINE        = $0400;

type
  TAVPixelFormat = Integer;

const
  AV_PIX_FMT_NONE = -1;
  AV_PIX_FMT_RGB24 = 2;
  AV_PIX_FMT_BGR24 = 3;
  AV_PIX_FMT_RGBA = 26;
  AV_PIX_FMT_BGRA = 28;

var
  sws_getContext: function(srcW, srcH: Integer; srcFormat: TAVPixelFormat;
                           dstW, dstH: Integer; dstFormat: TAVPixelFormat;
                           flags: Integer;
                           srcFilter, dstFilter: PSwsFilter;
                           const param: PDouble): PSwsContext; cdecl;
  sws_scale: function(c: PSwsContext; const srcSlice: PPByte;
                      const srcStride: PInteger; srcSliceY, srcSliceH: Integer;
                      const dst: PPByte; const dstStride: PInteger): Integer; cdecl;
  sws_freeContext: procedure(swsContext: PSwsContext); cdecl;

function Align4(N: Cardinal): Cardinal; inline;
begin
  Result := (N + 3) and (-4);
end;

function Available: Boolean;
begin
  Result:= Assigned(sws_freeContext);
end;

function Scale(Bitmap, DstBitmap: TRasterImage; DstWidth, DstHeight: Cardinal): Boolean;
var
  Res: Integer;
  sws: PSwsContext;
  DPF: TAVPixelFormat;
  ARawImage: TRawImage;
  SrcSlice, DstSlice: PByte;
  SrcWidth, SrcHeight: Cardinal;
  SrcStride, DstStride: Cardinal;
  SrcImage, DstImage: TLazIntfImage;
  Description: PRawImageDescription;
  SPF: TAVPixelFormat = AV_PIX_FMT_NONE;
begin
  if Pointer(@sws_getContext) = nil then Exit(False);
  Description:= TRawAccess(Bitmap).GetRawImageDescriptionPtr;

  // validate the input, we can only process 24/32-bit RGB/BGR[A] formats
  SrcWidth := Bitmap.Width;
  case Description^.BitsPerPixel of
    24:
      begin
        SrcStride := Align4(SrcWidth * 3);
        if Description^.RedShift = 16 then
          SPF := AV_PIX_FMT_BGR24
        else
          SPF := AV_PIX_FMT_RGB24;
      end;
    32:
      begin
        SrcStride := SrcWidth * 4;
        if Description^.RedShift = 0 then
          SPF := AV_PIX_FMT_RGBA
        else
          SPF := AV_PIX_FMT_BGRA;
      end;
  end;
  if SPF = AV_PIX_FMT_NONE then Exit(False);

  SrcHeight := Bitmap.Height;

  SrcImage := TLazIntfImage.Create(Bitmap.RawImage, False);
  DstImage := TLazIntfImage.Create(DstWidth, DstHeight, [riqfRGB, riqfAlpha]);

  if DstImage.DataDescription.RedShift = 0 then
    DPF := AV_PIX_FMT_RGBA
  else begin
    DPF := AV_PIX_FMT_BGRA;
  end;

  // https://www.reddit.com/r/cemu/comments/aq2wbs/scale_filter_comparison_bilinear_vs_bicubic_vs
  // Bilinear works as well as Bicubic and Hermite
  sws := sws_getContext(SrcWidth, SrcHeight, SPF,
                        DstWidth, DstHeight, DPF, SWS_BILINEAR, nil, nil, nil);

  SrcSlice := SrcImage.PixelData;
  DstSlice := DstImage.PixelData;
  DstStride := DstWidth * 4;

  Res:= sws_scale(sws, @SrcSlice, @SrcStride, 0, SrcHeight, @DstSlice, @DstStride);
  sws_freeContext(sws);

  DstImage.GetRawImage(ARawImage, True);
  DstBitmap.LoadFromRawImage(ARawImage, True);

  DstImage.Free;
  SrcImage.Free;

  Result := (Res > 0);
end;

const
{$IF DEFINED(MSWINDOWS)}
  swslib = 'swscale-%d.dll';
{$ELSEIF DEFINED(DARWIN)}
  swslib = 'libswscale.%d.dylib';
{$ELSEIF DEFINED(UNIX)}
  swslib = 'libswscale.so.%d';
{$ENDIF}

procedure Initialize;
var
  Version: Integer;
  hModule: TLibHandle;
  LibraryName: String;
begin
  for Version:= 10 downto 7 do
  begin
    LibraryName:= Format(swslib, [Version]);
    hModule:= mbLoadLibraryEx(LibraryName);
    if (hModule <> NilHandle) then Break;
  end;

  if (hModule <> NilHandle) then
  begin
    @sws_getContext:= SafeGetProcAddress(hModule, 'sws_getContext');
    @sws_scale:= SafeGetProcAddress(hModule, 'sws_scale');
    @sws_freeContext:= SafeGetProcAddress(hModule, 'sws_freeContext');
  end;
end;

initialization
  Initialize;

end.

