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
  Math, Classes, SysUtils, Graphics, IntfGraphics, GraphType, BaseUnix, Unix,
  Types, DCClassesUtf8, DCOSUtils, DCStrUtils, DCConvertEncoding,
  uThumbnails, uPixMapManager, uReSample, uGraphics;

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
  OBitmap: TBitmap;
  InnerSize: TSize;
  sFileName: String;
  PtrDirEnt: pDirent;
  ABitmap: TBitmap = nil;
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

        // Draw default folder icon
        Result:= PixMapManager.GetThemeIcon('folder', Min(aSize.cx, aSize.cy));
        Source:= Result.CreateIntfImage;
        try
          X:= (aSize.cx - Result.Width) div 2;
          Y:= (aSize.cy - Result.Height) div 2;
          Target.CopyPixels(Source, X, Y);
        finally
          Source.Free;
        end;

        // Scale folder inner icon
        if (ABitmap.Width > InnerSize.cx) or (ABitmap.Height > InnerSize.cy) then
        begin
          InnerSize:= GetPreviewScaleSize(InnerSize, ABitmap.Width, ABitmap.Height);

          OBitmap:= TBitmap.Create;
          try
            OBitmap.SetSize(InnerSize.cx, InnerSize.cy);
            Stretch(ABitmap, OBitmap, ResampleFilters[2].Filter, ResampleFilters[2].Width);
          finally
            ABitmap.Free;
            ABitmap:= OBitmap;
          end;
        end;

        // Draw folder inner icon
        Source:= ABitmap.CreateIntfImage;
        try
          X:= (aSize.cx - ABitmap.Width) div 2;
          Y:= (aSize.cy - ABitmap.Height) div 2;
          Target.AlphaBlend(Source, nil, X, Y);
        finally
          Source.Free;
        end;

        BitmapAssign(Result, Target);
      finally
        Target.Free;
      end;
      ABitmap.Free;
    end;
  end;
end;

initialization
  ProviderIndex:= TThumbnailManager.RegisterProvider(@GetThumbnail);

end.

