{
   Double Commander
   -------------------------------------------------------------------------
   Graphic functions

   Copyright (C) 2013-2017 Alexander Koblov (alexx2000@mail.ru)

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

implementation

uses
  GraphType, uPixMapManager;

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

{ TImageListHelper }

procedure TImageListHelper.LoadThemeIcon(Index: Integer; const AIconName: String);
var
  ABitmap: TBitmap;
begin
  ABitmap:= PixMapManager.GetThemeIcon(AIconName, Self.Width);
  if Assigned(ABitmap) then
  begin
    Self.Replace(Index, ABitmap , nil);
    ABitmap.Free;
  end;
end;

end.

