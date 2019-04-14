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
  Classes, SysUtils, Graphics, Controls;

type

  { TImageListHelper }

  TImageListHelper = class helper for TImageList
  public
    procedure LoadThemeIcon(Index: Integer; const AIconName: String);
  end;

procedure BitmapAssign(Bitmap: TBitmap; Image: TRasterImage);
procedure BitmapAlpha(var ABitmap: TBitmap; APercent: Single);
procedure BitmapCenter(var Bitmap: TBitmap; Width, Height: Integer);

implementation

uses
  GraphType, FPimage, FPImgCanv, IntfGraphics, uPixMapManager;

type
  TRawAccess = class(TRasterImage) end;

procedure BitmapAssign(Bitmap: TBitmap; Image: TRasterImage);
var
  RawImage: PRawImage;
begin
  RawImage:= TRawAccess(Image).GetRawImagePtr;
  // Simply change raw image owner without data copy
  Bitmap.LoadFromRawImage(RawImage^, True);
  // Set image data pointer to nil, so it will not free double
  RawImage^.ReleaseData;
end;

procedure BitmapAlpha(var ABitmap: TBitmap; APercent: Single);
var
  X, Y: Integer;
  Color: TFPColor;
  AImage: TLazIntfImage;
begin
  if ABitmap.RawImage.Description.AlphaPrec <> 0 then
  begin
    AImage:= ABitmap.CreateIntfImage();
    for X:= 0 to AImage.Width - 1 do
    begin
      for Y:= 0 to AImage.Height - 1 do
      begin
        Color:= AImage.Colors[X, Y];
        Color.Alpha:= Round(Color.Alpha * APercent);
        AImage.Colors[X, Y]:= Color;
      end;
    end;
    ABitmap.LoadFromIntfImage(AImage);
    AImage.Free;
  end;
end;

procedure BitmapCenter(var Bitmap: TBitmap; Width, Height: Integer);
var
  X, Y: Integer;
  Canvas: TFPImageCanvas;
  Source, Target: TLazIntfImage;
begin
  if (Bitmap.Width <> Width) or (Bitmap.Height <> Height) then
  begin
    Source:= Bitmap.CreateIntfImage;
    Target:= TLazIntfImage.Create(Width, Height, [riqfRGB, riqfAlpha]);
    Target.CreateData;
    Canvas:= TFPImageCanvas.Create(Target);
    X:= (Width - Bitmap.Width) div 2;
    Y:= (Height - Bitmap.Height) div 2;
    Canvas.Erase;
    Canvas.Draw(X, Y, Source);
    Bitmap.LoadFromIntfImage(Target);
    Target.Free;
    Source.Free;
  end;
end;

{ TImageListHelper }

procedure TImageListHelper.LoadThemeIcon(Index: Integer; const AIconName: String);
var
  ABitmap: TBitmap;
begin
  ABitmap:= PixMapManager.GetThemeIcon(AIconName, Self.Width);
  if (ABitmap = nil) then ABitmap:= TBitmap.Create;
  if (Index < Count) then
    Self.Replace(Index, ABitmap , nil)
  else begin
    Self.Insert(Index, ABitmap , nil)
  end;
  ABitmap.Free;
end;

end.

