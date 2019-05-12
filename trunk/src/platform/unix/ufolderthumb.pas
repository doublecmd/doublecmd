{
   Double Commander
   -------------------------------------------------------------------------
   Simple folder thumbnail provider

   Copyright (C) 2019 Alexander Koblov (alexx2000@mail.ru)

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

unit uFolderThumb;

{$mode objfpc}{$H+}

interface

implementation

uses
  Math, Classes, SysUtils, Graphics, FPimage, FPImgCanv, IntfGraphics, GraphType,
  BaseUnix, Unix, Types, DCClassesUtf8, DCOSUtils, DCStrUtils, DCConvertEncoding,
  uThumbnails, uPixMapManager;

var
  ProviderIndex: Integer;

function GetPreviewScaleSize(aSize: Types.TSize; aWidth, aHeight: Integer): Types.TSize;
begin
  if aWidth > aHeight then
    begin
      Result.cx:= aSize.cx;
      Result.cy:= Result.cx * aHeight div aWidth;
      if Result.cy > aSize.cy then
      begin
        Result.cy:= aSize.cy;
        Result.cx:= Result.cy * aWidth div aHeight;
      end;
    end
  else
    begin
      Result.cy:= aSize.cy;
      Result.cx:= Result.cy * aWidth div aHeight;
    end;
end;

function GetThumbnail(const aFileName: String; aSize: Types.TSize): Graphics.TBitmap;
var
  AExt: String;
  DirPtr: pDir;
  X, Y: Integer;
  InnerSize: TSize;
  sFileName: String;
  PtrDirEnt: pDirent;
  ABitmap: TBitmap = nil;
  Canvas: TFPImageCanvas;
  Picture: TPicture = nil;
  FileStream: TFileStreamEx;
  Source, Target: TLazIntfImage;
begin
  Result:= nil;

  if FPS_ISDIR(mbFileGetAttr(aFileName)) then;
  begin
    // Create half size inner icon
    InnerSize.cx:= aSize.cx * 50 div 100;
    InnerSize.cy:= aSize.cy * 50 div 100;

    DirPtr:= fpOpenDir(PAnsiChar(CeUtf8ToSys(aFileName)));
    if Assigned(DirPtr) then
    try
      Picture:= TPicture.Create;
      PtrDirEnt:= fpReadDir(DirPtr^);
      while PtrDirEnt <> nil do
      begin
        if (PtrDirEnt^.d_name <> '..') and (PtrDirEnt^.d_name <> '.') then
        begin
          sFileName:= IncludeTrailingBackslash(aFileName);
          sFileName+= CeSysToUtf8(PtrDirEnt^.d_name);

          // Try to create thumnail using providers
          ABitmap:= TThumbnailManager.GetPreviewFromProvider(sFileName, InnerSize, ProviderIndex);
          if Assigned(ABitmap) then Break;

          // Create thumnail for image files
          AExt:= ExtractOnlyFileExt(sFileName);
          if GetGraphicClassForFileExtension(AExt) <> nil then
          try
            FileStream:= TFileStreamEx.Create(sFileName, fmOpenRead or fmShareDenyNone or fmOpenNoATime);
            try
              Picture.LoadFromStreamWithFileExt(FileStream, AExt);
              ABitmap:= TBitmap.Create;
              ABitmap.Assign(Picture.Graphic);
              Break;
            finally
              FreeAndNil(FileStream);
            end;
          except
            // Ignore
          end;
        end;
        PtrDirEnt:= fpReadDir(DirPtr^);
      end;
    finally
      fpCloseDir(DirPtr^);
      Picture.Free;
    end;
    if Assigned(ABitmap) then
    begin
      Target:= TLazIntfImage.Create(aSize.cx, aSize.cy, [riqfRGB, riqfAlpha]);
      try
        Target.CreateData;
        Canvas:= TFPImageCanvas.Create(Target);
        try
          Canvas.Erase;

          // Draw default folder icon
          Result:= PixMapManager.GetThemeIcon('folder', Min(aSize.cx, aSize.cy));
          X:= (aSize.cx - Result.Width) div 2;
          Y:= (aSize.cy - Result.Height) div 2;
          Source:= Result.CreateIntfImage;
          try
            Canvas.Draw(X, Y, Source);
          finally
            Source.Free;
          end;

          // Draw folder inner icon
          Source:= ABitmap.CreateIntfImage;
          try
            if (ABitmap.Width > InnerSize.cx) or (ABitmap.Height > InnerSize.cy) then
            begin
              InnerSize:= GetPreviewScaleSize(InnerSize, ABitmap.Width, ABitmap.Height);
              X:= (aSize.cx - InnerSize.cx) div 2;
              Y:= (aSize.cy - InnerSize.cy) div 2;
              Canvas.StretchDraw(X, Y, InnerSize.cx, InnerSize.cy, Source);
            end
            else begin
              X:= (aSize.cx - ABitmap.Width) div 2;
              Y:= (aSize.cy - ABitmap.Height) div 2;
              Canvas.Draw(X, Y, Source);
            end;
          finally
            Source.Free;
          end;
          Result.LoadFromIntfImage(Target);
        finally
          Canvas.Free;
        end;
      finally
        Target.Free;
      end;
    end;
  end;
end;

initialization
  ProviderIndex:= TThumbnailManager.RegisterProvider(@GetThumbnail);

end.

