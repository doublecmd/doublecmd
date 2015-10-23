{
   Double Commander
   -------------------------------------------------------------------------
   Fast JPEG thumbnail provider

   Copyright (C) 2013 Alexander Koblov (alexx2000@mail.ru)

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

unit uJpegThumb;

{$mode objfpc}{$H+}

interface

implementation

uses
  Classes, SysUtils, Types, Graphics, FPReadJPEG, IntfGraphics, GraphType,
  DCClassesUtf8, uReSample, uThumbnails;

function GetThumbnail(const aFileName: String; aSize: TSize): Graphics.TBitmap;
var
  Bitmap: TBitmap;
  RawImage: TRawImage;
  FileStream: TFileStreamEx;
  FPReaderJPEG: TFPReaderJPEG;
  LazIntfImage: TLazIntfImage;
begin
  Result:= nil;

  if TJPEGImage.IsFileExtensionSupported(ExtractFileExt(aFileName)) then
  begin
    Result:= TBitmap.Create;
    FPReaderJPEG:= TFPReaderJPEG.Create;
    FPReaderJPEG.MinWidth:= aSize.cx;
    FPReaderJPEG.MinHeight:= aSize.cy;
    try
      FileStream:= TFileStreamEx.Create(aFileName, fmOpenRead or fmShareDenyNone);
      LazIntfImage:= TLazIntfImage.Create(aSize.cx, aSize.cy, [riqfRGB]);
      try
        FPReaderJPEG.ImageRead(FileStream, LazIntfImage);
        LazIntfImage.GetRawImage(RawImage, True);
        if not ((LazIntfImage.Width > aSize.cx) or (LazIntfImage.Height > aSize.cy)) then
          Result.LoadFromRawImage(RawImage, True)
        else
          begin
            Bitmap:= TBitmap.Create;
            try
              Bitmap.LoadFromRawImage(RawImage, True);
              aSize:= TThumbnailManager.GetPreviewScaleSize(Bitmap.Width, Bitmap.Height);
              Result.SetSize(aSize.cx, aSize.cy);
              Stretch(Bitmap, Result, ResampleFilters[2].Filter, ResampleFilters[2].Width);
            finally
              Bitmap.Free;
            end;
          end;
      finally
        FPReaderJPEG.Free;
        LazIntfImage.Free;
        FileStream.Free;
      end;
    except
      FreeAndNil(Result);
    end;
  end;
end;

initialization
  TThumbnailManager.RegisterProvider(@GetThumbnail);

end.

