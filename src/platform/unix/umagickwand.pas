{
   Double Commander
   -------------------------------------------------------------------------
   ImageMagick thumbnail provider

   Copyright (C) 2013-2023 Alexander Koblov (alexx2000@mail.ru)

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}

unit uMagickWand;

{$mode delphi}

interface

implementation

uses
  LCLIntf, Classes, SysUtils, DynLibs, FileUtil, Types, Graphics, CTypes,
  DCOSUtils, DCConvertEncoding, uThumbnails, uDebug, uClassesEx, uGraphics,
  uMasks;

const
  MagickFalse = 0;
  MagickTrue = 1;

const
  libMagickWand: array[0..7] of String =
  (
      'libMagickWand-7.Q16.so.10',
      'libMagickWand-7.Q16HDRI.so.10',
      'libMagickWand-6.Q16.so.7',
      'libMagickWand-6.Q16HDRI.so.7',
      'libMagickWand-6.Q16.so.6',
      'libMagickWand-6.Q16HDRI.so.6',
      'libMagickWand-6.Q16.so.3',
      'libMagickWand-6.Q16HDRI.so.3'
  );

type
  PMagickWand = Pointer;
  MagickBooleanType = culong;
  ExceptionType = Word;
  PExceptionType = ^ExceptionType;

{$PACKENUM 4}

type
  FilterTypes = (
   UndefinedFilter,
   PointFilter,
   BoxFilter,
   TriangleFilter,
   HermiteFilter,
   HannFilter,
   HammingFilter,
   BlackmanFilter,
   GaussianFilter,
   QuadraticFilter,
   CubicFilter,
   CatromFilter,
   MitchellFilter,
   JincFilter,
   SincFilter,
   SincFastFilter,
   KaiserFilter,
   WelchFilter,
   ParzenFilter,
   BohmanFilter,
   BartlettFilter,
   LagrangeFilter,
   LanczosFilter
  );

var
  MagickWand: TLibHandle;
  MaskList: TMaskList = nil;

var
  MagickWandGenesis: procedure(); cdecl;
  MagickWandTerminus: procedure(); cdecl;
  NewMagickWand: function(): PMagickWand; cdecl;
  DestroyMagickWand: function(wand: PMagickWand): PMagickWand; cdecl;
  MagickGetException: function(wand: PMagickWand; severity: PExceptionType): PAnsiChar; cdecl;
  MagickRelinquishMemory: function(resource: Pointer): Pointer; cdecl;
  MagickReadImage: function(wand: PMagickWand; const filename: PAnsiChar): MagickBooleanType; cdecl;
  MagickGetImageWidth: function(wand: PMagickWand): csize_t; cdecl;
  MagickGetImageHeight: function(wand: PMagickWand): csize_t; cdecl;
  MagickResizeImageOld: function(wand: PMagickWand; const columns, rows: csize_t; const filter: FilterTypes; const blur: double): MagickBooleanType; cdecl;
  MagickResizeImageNew: function(wand: PMagickWand; const columns, rows: csize_t; const filter: FilterTypes): MagickBooleanType; cdecl;
  MagickSetImageFormat: function(wand: PMagickWand; const format: PAnsiChar): MagickBooleanType; cdecl;
  MagickGetImageBlob: function(wand: PMagickWand; length: Pcsize_t): PByte; cdecl;

procedure RaiseWandException(Wand: PMagickWand);
var
  Description: PAnsiChar;
  Severity: ExceptionType;
  ExceptionMessage: AnsiString;
begin
  Description:= MagickGetException(Wand, @Severity);
  ExceptionMessage:= AnsiString(Description);
  Description:= MagickRelinquishMemory(Description);
  Raise Exception.Create(ExceptionMessage);
end;

function GetThumbnail(const aFileName: String; aSize: TSize): Graphics.TBitmap;
var
  Memory: PByte;
  Wand: PMagickWand;
  MemorySize: csize_t;
  Width, Height: csize_t;
  BlobStream: TBlobStream;
  Status: MagickBooleanType;
  Bitmap: TPortableNetworkGraphic;
begin
  Result:= nil;

  if MaskList.Matches(aFileName) then
  begin
    // DCDebug('GetThumbnail start: ' + IntToStr(GetTickCount));

    Wand:= NewMagickWand;

    try
      Status:= MagickReadImage(Wand, PAnsiChar(CeUtf8ToSys(aFileName)));
      try
        if (Status = MagickFalse) then RaiseWandException(Wand);

        // Get image width and height
        Width:= MagickGetImageWidth(Wand);
        Height:= MagickGetImageHeight(Wand);

        if (Width > aSize.cx) or (Height > aSize.cy) then
        begin
          // Calculate aspect width and height of thumb
          aSize:= TThumbnailManager.GetPreviewScaleSize(Width, Height);
          // Create image thumbnail
          if Assigned(MagickResizeImageNew) then
            Status:= MagickResizeImageNew(Wand, aSize.cx, aSize.cy, LanczosFilter)
          else begin
            Status:= MagickResizeImageOld(Wand, aSize.cx, aSize.cy, LanczosFilter, 1.0);
          end;
          if (Status = MagickFalse) then RaiseWandException(Wand);
        end;

        Status:= MagickSetImageFormat(Wand, 'PNG32');
        if (Status = MagickFalse) then RaiseWandException(Wand);

        Memory:= MagickGetImageBlob(Wand, @MemorySize);
        if Assigned(Memory) then
        try
          BlobStream:= TBlobStream.Create(Memory, MemorySize);
          Bitmap:= TPortableNetworkGraphic.Create;
          try
            Bitmap.LoadFromStream(BlobStream);
            Result:= Graphics.TBitmap.Create;
            BitmapAssign(Result, Bitmap);
          except
            FreeAndNil(Result);
          end;
          Bitmap.Free;
          BlobStream.Free;
        finally
          MagickRelinquishMemory(Memory);
        end;
      except
        on E: Exception do
        DCDebug('ImageMagick: ' + E.Message);
      end;

    finally
      Wand:= DestroyMagickWand(Wand);

      // DCDebug('GetThumbnail finish: ' + IntToStr(GetTickCount));
    end;
  end;
end;

procedure Initialize;
var
  Version: Integer;
  LibraryName: AnsiString;
begin
  for Version:= 0 to High(libMagickWand) do
  begin
    LibraryName:= libMagickWand[Version];
    MagickWand:= LoadLibrary(LibraryName);
    if (MagickWand <> NilHandle) then Break;
  end;

  if (MagickWand <> NilHandle) then
  try
    @MagickWandGenesis:= SafeGetProcAddress(MagickWand, 'MagickWandGenesis');
    @MagickWandTerminus:= SafeGetProcAddress(MagickWand, 'MagickWandTerminus');

    @NewMagickWand:= SafeGetProcAddress(MagickWand, 'NewMagickWand');
    @DestroyMagickWand:= SafeGetProcAddress(MagickWand, 'DestroyMagickWand');
    @MagickGetException:= SafeGetProcAddress(MagickWand, 'MagickGetException');

    @MagickRelinquishMemory:= SafeGetProcAddress(MagickWand, 'MagickRelinquishMemory');
    @MagickReadImage:= SafeGetProcAddress(MagickWand, 'MagickReadImage');
    @MagickGetImageWidth:= SafeGetProcAddress(MagickWand, 'MagickGetImageWidth');
    @MagickGetImageHeight:= SafeGetProcAddress(MagickWand, 'MagickGetImageHeight');

    if (LibraryName[15] = '6') then
      @MagickResizeImageOld:= SafeGetProcAddress(MagickWand, 'MagickResizeImage')
    else begin
      @MagickResizeImageNew:= SafeGetProcAddress(MagickWand, 'MagickResizeImage');
    end;

    @MagickSetImageFormat:= SafeGetProcAddress(MagickWand, 'MagickSetImageFormat');
    @MagickGetImageBlob:= SafeGetProcAddress(MagickWand, 'MagickGetImageBlob');

    MagickWandGenesis;

    // Register thumbnail provider
    TThumbnailManager.RegisterProvider(@GetThumbnail);
    MaskList:= TMaskList.Create('*.xcf');
    DCDebug('ImageMagick: ' + LibraryName);
  except
    FreeLibrary(MagickWand);
    MagickWand:= NilHandle;
  end;
end;

procedure Finalize;
begin
  if (MagickWand <> NilHandle) then
  begin
    MaskList.Free;
    MagickWandTerminus;
    FreeLibrary(MagickWand);
  end;
end;

initialization
  Initialize;

finalization
  Finalize;

end.
