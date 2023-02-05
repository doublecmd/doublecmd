{
   Double Commander
   -------------------------------------------------------------------------
   WebP reader implementation (via libwebp library)

   Copyright (C) 2017-2023 Alexander Koblov (alexx2000@mail.ru)

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

unit uDCReadWebP;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, FPImage;

type

  { TDCReaderWebP }

  TDCReaderWebP = class (TFPCustomImageReader)
  protected
    function  InternalCheck(Stream: TStream): Boolean; override;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
  end;

  { TWeppyImage }

  TWeppyImage = class(TFPImageBitmap)
  protected
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    class function GetFileExtensions: String; override;
  end;

implementation

uses
  InitC, DynLibs, IntfGraphics, GraphType, CTypes, DCOSUtils;

procedure CFree(P: Pointer); cdecl; external clib name 'free';

var
  WebPFree: procedure(ptr: pointer); cdecl;
  WebPGetInfo: function(const data: pcuint8; data_size: csize_t;
                        width: pcint; height: pcint): cint; cdecl;
  WebPDecodeRGBA: function(const data: pcuint8; data_size: csize_t;
                           width: pcint; height: pcint): pcuint8; cdecl;

type
  PRGBA = ^TRGBA;
  TRGBA = packed record
    Red, Green,
    Blue, Alpha: Byte;
  end;

{ TDCReaderWebP }

function TDCReaderWebP.InternalCheck(Stream: TStream): Boolean;
var
  MemoryStream: TMemoryStream;
begin
  MemoryStream:= Stream as TMemoryStream;
  Result:= WebPGetInfo(MemoryStream.Memory, MemoryStream.Size, nil, nil) <> 0;
end;

procedure TDCReaderWebP.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  Data: Pointer;
  ImageData: PRGBA;
  AWidth, AHeight: cint;
  Desc: TRawImageDescription;
  MemoryStream: TMemoryStream;
begin
  MemoryStream:= Stream as TMemoryStream;
  Data:= WebPDecodeRGBA(MemoryStream.Memory, MemoryStream.Size, @AWidth, @AHeight);
  if Assigned(Data) then
  begin
    ImageData:= PRGBA(Data);
    // Set output image size
    Img.SetSize(AWidth, AHeight);
    // Initialize image description
    Desc.Init_BPP32_R8G8B8A8_BIO_TTB(Img.Width, Img.Height);
    TLazIntfImage(Img).DataDescription:= Desc;
    // Copy image data
    Move(ImageData^, TLazIntfImage(Img).PixelData^, Img.Width * Img.Height * SizeOf(TRGBA));
    if Assigned(WebPFree) then
      WebPFree(Data)
    else begin
      CFree(Data);
    end;
  end;
end;

{ TWeppyImage }

class function TWeppyImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result:= TDCReaderWebP;
end;

class function TWeppyImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:= TSharedBitmap;
end;

class function TWeppyImage.GetFileExtensions: String;
begin
  Result:= 'webp';
end;

const
  webplib = 'libwebp.so.%d';

var
  libwebp: TLibHandle;

procedure Initialize;
var
  Version: Integer;
  LibraryName: AnsiString;
begin
  for Version:= 7 downto 5 do
  begin
    LibraryName:= Format(webplib, [Version]);
    libwebp:= LoadLibrary(LibraryName);
    if (libwebp <> NilHandle) then Break;
  end;

  if (libwebp <> NilHandle) then
  try
    @WebPFree:= GetProcAddress(libwebp, 'WebPFree');
    @WebPGetInfo:= SafeGetProcAddress(libwebp, 'WebPGetInfo');
    @WebPDecodeRGBA:= SafeGetProcAddress(libwebp, 'WebPDecodeRGBA');

    // Register image handler and format
    ImageHandlers.RegisterImageReader('Weppy Image', 'WEBP', TDCReaderWebP);
    TPicture.RegisterFileFormat('webp', 'Weppy Image', TWeppyImage);
  except
    // Skip
  end;
end;

initialization
  Initialize;

finalization
  if (libwebp <> NilHandle) then FreeLibrary(libwebp);

end.

