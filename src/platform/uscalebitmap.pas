unit uScaleBitmap;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

function ScaleFastAvailable(Src: TRasterImage): Boolean;
function ScaleBitmapFast(Src: TRasterImage; Dst: TBitmap; DstWidth, DstHeight: Cardinal): Boolean;
function ScaleBitmapAuto(Src: TRasterImage; Dst: TBitmap; DstWidth, DstHeight: Cardinal): Boolean;

implementation

uses
  GraphType, Img32, IntfGraphics, swscale, uReSample;

type
  TRawAccess = class(TRasterImage) end;

function ScaleImage32Available(Src: TRasterImage): Boolean;
begin
  with TRawAccess(Src).GetRawImageDescriptionPtr^ do
  begin
    Result:= (Depth = 32) and (Depth = BitsPerPixel);
  end;
end;

function ScaleImage32(Bitmap, DstBitmap: TRasterImage; DstWidth, DstHeight: Cardinal): Boolean;
var
  Image32: TImage32;
  ARawImage: TRawImage;
  SrcImage, DstImage: TLazIntfImage;
begin
  SrcImage:= TLazIntfImage.Create(Bitmap.RawImage, False);
  DstImage:= TLazIntfImage.CreateCompatible(SrcImage, DstWidth, DstHeight);

  Image32:= TImage32.Create(SrcImage.Width, SrcImage.Height);
  try
    Move(TLazIntfImage(SrcImage).PixelData^, Image32.PixelBase^, SrcImage.Width * SrcImage.Height * SizeOf(TColor32));

    Image32.Resize(DstWidth, DstHeight);

    Move(Image32.PixelBase^, TLazIntfImage(DstImage).PixelData^, DstImage.Width * DstImage.Height * SizeOf(TColor32));

    DstImage.GetRawImage(ARawImage, True);
    DstBitmap.LoadFromRawImage(ARawImage, True);
  finally
    Image32.Free;
    DstImage.Free;
    SrcImage.Free;
  end;
  Result:= True;
end;

function ScaleFastAvailable(Src: TRasterImage): Boolean;
var
  Description: PRawImageDescription;
begin
  Result:= swscale.Available;
  if Result then
  begin
    Description:= TRawAccess(Src).GetRawImageDescriptionPtr;
    Result:= (Description^.Depth in [24, 32]) and
             (Description^.Depth = Description^.BitsPerPixel);
  end;
end;

function ScaleBitmapFast(Src: TRasterImage; Dst: TBitmap; DstWidth, DstHeight: Cardinal): Boolean;
begin
  if swscale.Available then
    Result:= swscale.Scale(Src, Dst, DstWidth, DstHeight)
  else begin
    Result:= False;
  end;
end;

function ScaleBitmapAuto(Src: TRasterImage; Dst: TBitmap; DstWidth, DstHeight: Cardinal): Boolean;
begin
  if ScaleFastAvailable(Src) then
  begin
    Result:= ScaleBitmapFast(Src, Dst, DstWidth, DstHeight);
  end
  else if ScaleImage32Available(Src) then
  begin
    Result:= ScaleImage32(Src, Dst, DstWidth, DstHeight);
  end
  else begin
    Result:= True;
    Dst.SetSize(DstWidth, DstHeight);
    Stretch(Src, Dst, ResampleFilters[2].Filter, ResampleFilters[2].Width);
  end;
end;

end.

