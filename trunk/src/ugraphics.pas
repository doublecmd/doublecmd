{
   Double Commander
   -------------------------------------------------------------------------
   Graphic functions

   Copyright (C) 2013-2019 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uGraphics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, IntfGraphics;

procedure BitmapConvert(Bitmap: TRasterImage);
procedure BitmapAssign(Bitmap, Image: TRasterImage);
procedure BitmapAlpha(var ABitmap: TBitmap; APercent: Single);
procedure BitmapAssign(Bitmap: TRasterImage; Image: TLazIntfImage);
procedure BitmapCenter(var Bitmap: TBitmap; Width, Height: Integer);

implementation

uses
  GraphType, FPimage;

type
  TRawAccess = class(TRasterImage) end;

procedure BitmapConvert(Bitmap: TRasterImage);
var
  Source, Target: TLazIntfImage;
begin
  Source:= TLazIntfImage.Create(Bitmap.RawImage, False);
  try
    Target:= TLazIntfImage.Create(Bitmap.Width, Bitmap.Height, [riqfRGB, riqfAlpha]);
    try
      Target.CreateData;
      Target.CopyPixels(Source);
      BitmapAssign(Bitmap, Target);
    finally
      Target.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure BitmapAssign(Bitmap, Image: TRasterImage);
var
  RawImage: PRawImage;
begin
  RawImage:= TRawAccess(Image).GetRawImagePtr;
  // Simply change raw image owner without data copy
  Bitmap.LoadFromRawImage(RawImage^, True);
  // Set image data pointer to nil, so it will not free double
  RawImage^.ReleaseData;
end;

procedure BitmapAssign(Bitmap: TRasterImage; Image: TLazIntfImage);
var
  ARawImage: TRawImage;
begin
  Image.GetRawImage(ARawImage, True);
  // Simply change raw image owner without data copy
  Bitmap.LoadFromRawImage(ARawImage, True);
end;

procedure BitmapAlpha(var ABitmap: TBitmap; APercent: Single);
var
  X, Y: Integer;
  Color: TFPColor;
  Masked: Boolean;
  AImage: TLazIntfImage;
  SrcIntfImage: TLazIntfImage;
begin
  if ABitmap.RawImage.Description.AlphaPrec <> 0 then
  begin
    ABitmap.BeginUpdate;
    try
      AImage:= TLazIntfImage.Create(ABitmap.RawImage, False);
      for X:= 0 to AImage.Width - 1 do
      begin
        for Y:= 0 to AImage.Height - 1 do
        begin
          Color:= AImage.Colors[X, Y];
          Color.Alpha:= Round(Color.Alpha * APercent);
          AImage.Colors[X, Y]:= Color;
        end;
      end;
      AImage.Free;
    finally
      ABitmap.EndUpdate;
    end;
  end
  else begin
    Masked:= ABitmap.RawImage.Description.MaskBitsPerPixel > 0;
    SrcIntfImage:= TLazIntfImage.Create(ABitmap.RawImage, False);
    AImage:= TLazIntfImage.Create(ABitmap.Width, ABitmap.Height, [riqfRGB, riqfAlpha]);
    AImage.CreateData;
    for X:= 0 to AImage.Width - 1 do
    begin
      for Y:= 0 to AImage.Height - 1 do
      begin
        Color := SrcIntfImage.Colors[X, Y];
        if Masked and SrcIntfImage.Masked[X, Y] then
          Color.Alpha:= Low(Color.Alpha)
        else begin
          Color.Alpha:= Round(High(Color.Alpha) * APercent);
        end;
        AImage.Colors[X, Y]:= Color;
      end
    end;
    SrcIntfImage.Free;
    BitmapAssign(ABitmap, AImage);
    AImage.Free;
  end;
end;

procedure BitmapCenter(var Bitmap: TBitmap; Width, Height: Integer);
var
  X, Y: Integer;
  Source, Target: TLazIntfImage;
begin
  if (Bitmap.Width <> Width) or (Bitmap.Height <> Height) then
  begin
    Source:= TLazIntfImage.Create(Bitmap.RawImage, False);
    try
      Target:= TLazIntfImage.Create(Width, Height, [riqfRGB, riqfAlpha]);
      try
        Target.CreateData;
        Target.FillPixels(colTransparent);
        X:= (Width - Bitmap.Width) div 2;
        Y:= (Height - Bitmap.Height) div 2;
        Target.CopyPixels(Source, X, Y);
        BitmapAssign(Bitmap, Target);
      finally
        Target.Free;
      end;
    finally
      Source.Free;
    end;
  end;
end;

end.

