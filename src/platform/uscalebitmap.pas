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
  GraphType, swscale, uReSample;

type
  TRawAccess = class(TRasterImage) end;

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
  else begin
    Result:= True;
    Dst.SetSize(DstWidth, DstHeight);
    Stretch(Src, Dst, ResampleFilters[2].Filter, ResampleFilters[2].Width);
  end;
end;

end.

