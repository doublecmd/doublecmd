{
   Double Commander
   -------------------------------------------------------------------------
   Scalable Vector Graphics reader implementation (via rsvg and cairo)

   Copyright (C) 2012-2019 Alexander Koblov (alexx2000@mail.ru)

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

unit uDCReadSVG;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, FPImage;

type

  { TDCReaderSVG }

  TDCReaderSVG = class (TFPCustomImageReader)
  private
    FRsvgHandle: Pointer;
  protected
    function  InternalCheck (Stream:TStream):boolean;override;
    procedure InternalRead(Stream:TStream;Img:TFPCustomImage);override;
  end;

  { TScalableVectorGraphics }

  TScalableVectorGraphics = class(TFPImageBitmap)
  protected
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    class function GetFileExtensions: string; override;
  end;


function BitmapLoadFromScalable(const FileName: String; AWidth, AHeight: Integer): TBitmap;

implementation

uses
  DynLibs, IntfGraphics, GraphType, Types, CTypes, LazUTF8, DCOSUtils,
  uThumbnails, uIconTheme, uGraphics;

type
  cairo_format_t = (
    CAIRO_FORMAT_ARGB32,
    CAIRO_FORMAT_RGB24,
    CAIRO_FORMAT_A8,
    CAIRO_FORMAT_A1
  );

type
  Pcairo_surface_t = Pointer;
  Pcairo_t = Pointer;
  PRsvgHandle = Pointer;
  PPGError = Pointer;

type
  PRsvgDimensionData = ^TRsvgDimensionData;
  TRsvgDimensionData = record
    width: cint;
    height: cint;
    em: cdouble;
    ex: cdouble;
  end;

var
  cairo_image_surface_create: function(format: cairo_format_t; width, height: LongInt): Pcairo_surface_t; cdecl;
  cairo_surface_destroy: procedure(surface: Pcairo_surface_t); cdecl;
  cairo_image_surface_get_data: function(surface: Pcairo_surface_t): PByte; cdecl;
  cairo_create: function(target: Pcairo_surface_t): Pcairo_t; cdecl;
  cairo_destroy: procedure (cr: Pcairo_t); cdecl;
  cairo_scale: procedure(cr: Pcairo_t; sx, sy: cdouble); cdecl;

  rsvg_handle_new_from_file: function(const file_name: PAnsiChar; error: PPGError): PRsvgHandle; cdecl;
  rsvg_handle_new_from_data: function(data: PByte; data_len: SizeUInt; error: PPGError): PRsvgHandle; cdecl;
  rsvg_handle_get_dimensions: procedure(handle: PRsvgHandle; dimension_data: PRsvgDimensionData); cdecl;
  rsvg_handle_render_cairo: function(handle: PRsvgHandle; cr: Pcairo_t): LongBool; cdecl;

  g_type_init: procedure; cdecl;
  g_object_unref: procedure(anObject: Pointer); cdecl;

type
  PBGRA = ^TBGRA;
  TBGRA = packed record
    Blue, Green,
    Red, Alpha: Byte;
  end;

procedure RsvgHandleRender(RsvgHandle: Pointer; CairoSurface: Pcairo_surface_t;
                           Cairo: Pcairo_t; Img: TFPCustomImage);
var
  X, Y: Integer;
  ImageData: PBGRA;
  PixelColor: TFPColor;
  Desc: TRawImageDescription;
begin
  try
    // Draws a SVG to a Cairo surface
    if rsvg_handle_render_cairo(RsvgHandle, Cairo) then
    begin
      // Get a pointer to the data of the image surface, for direct access
      ImageData:= PBGRA(cairo_image_surface_get_data(CairoSurface));
      // Initialize image description
      Desc.Init_BPP32_B8G8R8A8_BIO_TTB(Img.Width, Img.Height);
      TLazIntfImage(Img).DataDescription:= Desc;
      // Read image data
      for Y:= 0 to Img.Height - 1 do
      for X:= 0 to Img.Width - 1 do
      with ImageData^ do
      begin
        PixelColor.alpha:= Alpha + Alpha shl 8;
        PixelColor.red:= Red + Red shl 8;
        PixelColor.green:= Green + Green shl 8;
        PixelColor.blue:= Blue + Blue shl 8;

        Img.Colors[X, Y]:= PixelColor;
        Inc(ImageData);
      end;
    end;
  finally
    g_object_unref(RsvgHandle);
    cairo_destroy(Cairo);
    cairo_surface_destroy(CairoSurface);
  end;
end;

function BitmapLoadFromScalable(const FileName: String; AWidth, AHeight: Integer): TBitmap;
var
  Cairo: Pcairo_t;
  RsvgHandle: Pointer;
  Image: TLazIntfImage;
  CairoSurface: Pcairo_surface_t;
  RsvgDimensionData: TRsvgDimensionData;
begin
  Result:= nil;
  RsvgHandle:= rsvg_handle_new_from_file(PAnsiChar(UTF8ToSys(FileName)), nil);
  if Assigned(RsvgHandle) then
  begin
    Image:= TLazIntfImage.Create(AWidth, AHeight);
    try
      // Get the SVG's size
      rsvg_handle_get_dimensions(RsvgHandle, @RsvgDimensionData);
      // Creates an image surface of the specified format and dimensions
      CairoSurface:= cairo_image_surface_create(CAIRO_FORMAT_ARGB32, Image.Width, Image.Height);
      Cairo:= cairo_create(CairoSurface);
      // Scale image if needed
      if (Image.Width <> RsvgDimensionData.width) or (Image.Height <> RsvgDimensionData.height) then
      begin
        cairo_scale(Cairo, Image.Width / RsvgDimensionData.width, Image.Height / RsvgDimensionData.height);
      end;
      RsvgHandleRender(RsvgHandle, CairoSurface, Cairo, Image);
      Result:= TBitmap.Create;
      BitmapAssign(Result, Image);
    finally
      Image.Free;
    end;
  end;
end;

function GetThumbnail(const aFileName: String; aSize: TSize): Graphics.TBitmap;
var
  Scale: Boolean;
  Cairo: Pcairo_t;
  RsvgHandle: Pointer;
  Image: TLazIntfImage;
  CairoSurface: Pcairo_surface_t;
  RsvgDimensionData: TRsvgDimensionData;
begin
  Result:= nil;

  if TScalableVectorGraphics.IsFileExtensionSupported(ExtractFileExt(aFileName)) then
  begin
    RsvgHandle:= rsvg_handle_new_from_file(PAnsiChar(UTF8ToSys(aFileName)), nil);
    if Assigned(RsvgHandle) then
    begin
      Result:= TBitmap.Create;
      // Get the SVG's size
      rsvg_handle_get_dimensions(RsvgHandle, @RsvgDimensionData);
      Scale:= (RsvgDimensionData.width > aSize.cx) or (RsvgDimensionData.height > aSize.cy);
      if Scale then
      begin
        // Calculate aspect width and height of thumb
        aSize:= TThumbnailManager.GetPreviewScaleSize(RsvgDimensionData.width, RsvgDimensionData.height);
      end
      else begin
        aSize.cx:= RsvgDimensionData.width;
        aSize.cy:= RsvgDimensionData.height;
      end;
      // Creates an image surface of the specified format and dimensions
      CairoSurface:= cairo_image_surface_create(CAIRO_FORMAT_ARGB32, aSize.cx, aSize.cy);
      Cairo:= cairo_create(CairoSurface);
      // Scale image if needed
      if Scale then
      begin
        cairo_scale(Cairo, aSize.cx / RsvgDimensionData.width, aSize.cy / RsvgDimensionData.height);
      end;
      Image:= TLazIntfImage.Create(aSize.cx, aSize.cy);
      try
        RsvgHandleRender(RsvgHandle, CairoSurface, Cairo, Image);
        BitmapAssign(Result, Image);
      finally
        Image.Free;
      end;
    end;
  end;
end;

{ TDCReaderSVG }

function TDCReaderSVG.InternalCheck(Stream: TStream): boolean;
var
  MemoryStream: TMemoryStream;
begin
  MemoryStream:= Stream as TMemoryStream;
  FRsvgHandle:= rsvg_handle_new_from_data(MemoryStream.Memory, MemoryStream.Size, nil);
  Result:= Assigned(FRsvgHandle);
end;

procedure TDCReaderSVG.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  Cairo: Pcairo_t;
  CairoSurface: Pcairo_surface_t;
  RsvgDimensionData: TRsvgDimensionData;
begin
  // Get the SVG's size
  rsvg_handle_get_dimensions(FRsvgHandle, @RsvgDimensionData);
  // Set output image size
  Img.SetSize(RsvgDimensionData.width, RsvgDimensionData.height);
  // Creates an image surface of the specified format and dimensions
  CairoSurface:= cairo_image_surface_create(CAIRO_FORMAT_ARGB32, RsvgDimensionData.width, RsvgDimensionData.height);
  Cairo:= cairo_create(CairoSurface);
  // Render vector graphics to raster image
  RsvgHandleRender(FRsvgHandle, CairoSurface, Cairo, Img);
end;

{ TScalableVectorGraphics }

class function TScalableVectorGraphics.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result:= TDCReaderSVG;
end;

class function TScalableVectorGraphics.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:= TSharedBitmap;
end;

class function TScalableVectorGraphics.GetFileExtensions: string;
begin
  Result:= 'svg;svgz';
end;

const
{$IF DEFINED(UNIX)}
  cairolib   = 'libcairo.so.2';
  rsvglib    = 'librsvg-2.so.2';
  gobjectlib = 'libgobject-2.0.so.0';
{$ELSEIF DEFINED(MSWINDOWS)}
  cairolib   = 'libcairo-2.dll';
  rsvglib    = 'librsvg-2-2.dll';
  gobjectlib = 'libgobject-2.0-0.dll';
{$ENDIF}

var
  libcairo, librsvg, libgobject: TLibHandle;

procedure Initialize;
begin
  libcairo:= LoadLibrary(cairolib);
  librsvg:= LoadLibrary(rsvglib);
  libgobject:= LoadLibrary(gobjectlib);

  if (libcairo <> NilHandle) and (librsvg <> NilHandle) and (libgobject <> NilHandle) then
  try
    @cairo_image_surface_create:= SafeGetProcAddress(libcairo, 'cairo_image_surface_create');
    @cairo_surface_destroy:= SafeGetProcAddress(libcairo, 'cairo_surface_destroy');
    @cairo_image_surface_get_data:= SafeGetProcAddress(libcairo, 'cairo_image_surface_get_data');
    @cairo_create:= SafeGetProcAddress(libcairo, 'cairo_create');
    @cairo_destroy:= SafeGetProcAddress(libcairo, 'cairo_destroy');
    @cairo_scale:= SafeGetProcAddress(libcairo, 'cairo_scale');

    @rsvg_handle_new_from_file:= SafeGetProcAddress(librsvg, 'rsvg_handle_new_from_file');
    @rsvg_handle_new_from_data:= SafeGetProcAddress(librsvg, 'rsvg_handle_new_from_data');
    @rsvg_handle_get_dimensions:= SafeGetProcAddress(librsvg, 'rsvg_handle_get_dimensions');
    @rsvg_handle_render_cairo:=  SafeGetProcAddress(librsvg, 'rsvg_handle_render_cairo');

    @g_type_init:= SafeGetProcAddress(libgobject, 'g_type_init');
    @g_object_unref:= SafeGetProcAddress(libgobject, 'g_object_unref');

    g_type_init();
    // Register image handler and format
    TIconTheme.RegisterExtension('svg;svgz');
    TThumbnailManager.RegisterProvider(@GetThumbnail);
    ImageHandlers.RegisterImageReader ('Scalable Vector Graphics', 'SVG;SVGZ', TDCReaderSVG);
    TPicture.RegisterFileFormat('svg;svgz', 'Scalable Vector Graphics', TScalableVectorGraphics);
  except
  end;
end;

procedure Finalize;
begin
  if (libcairo <> NilHandle) then FreeLibrary(libcairo);
  if (librsvg <> NilHandle) then FreeLibrary(librsvg);
  if (libgobject <> NilHandle) then FreeLibrary(libgobject);
end;

initialization
  Initialize;

finalization
  Finalize;

end.

