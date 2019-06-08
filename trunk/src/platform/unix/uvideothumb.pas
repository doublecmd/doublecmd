{
  Double Commander
  -------------------------------------------------------------------------
  FFmpeg thumbnail provider

  Copyright (C) 2015 Alexander Koblov (alexx2000@mail.ru)

  FFmpegthumbnailer - lightweight video thumbnailer
  Copyright (C) 2010 Dirk Vanden Boer <dirk.vdb@gmail.com>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write to the Free Software Foundation, Inc.,
  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
}

unit uVideoThumb;

{$mode delphi}
{$packrecords c}

interface

uses
  Classes, SysUtils;

implementation

uses
  CTypes, DynLibs, Graphics, Types, LazUTF8, DCOSUtils, DCConvertEncoding,
  uThumbnails, uClassesEx, uMasks, uGraphics;

type
  ThumbnailerImageType =
  (
      Png,
      Jpeg,
      Unknown
  );

  Pvideo_thumbnailer = ^Tvideo_thumbnailer;
  Tvideo_thumbnailer = record
      thumbnail_size:          cint;                 //* default = 128 */
      seek_percentage:         cint;                 //* default = 10 */
      seek_time:               pchar;                //* default = NULL (format hh:mm:ss, overrides seek_percentage if set) */
      overlay_film_strip:      cint;                 //* default = 0 */
      workaround_bugs:         cint;                 //* default = 0 */
      thumbnail_image_quality: cint;                 //* default = 8 (0 is bad, 10 is best)*/
      thumbnail_image_type:    ThumbnailerImageType; //* default = Png */
      av_format_context:       pointer;              //* default = NULL */
      maintain_aspect_ratio:   cint;                 //* default = 1 */

      thumbnailer:             pointer;              //* for internal use only */
      filter:                  pointer;              //* for internal use only */
  end;

  Pimage_data = ^Timage_data;
  Timage_data = record
    image_data_ptr:  pcuint8; //* points to the image data after call to generate_thumbnail_to_buffer */
    image_data_size: cint;    //* contains the size of the image data after call to generate_thumbnail_to_buffer */

    internal_data:   pointer; //* for internal use only */
  end;

var
  { create video_thumbnailer structure  }
  video_thumbnailer_create: function(): Pvideo_thumbnailer; cdecl;

  { destroy video_thumbnailer structure  }
  video_thumbnailer_destroy: procedure(thumbnailer: Pvideo_thumbnailer); cdecl;

  { create image_data structure  }
  video_thumbnailer_create_image_data: function(): Pimage_data; cdecl;

  { destroy image_data structure  }
  video_thumbnailer_destroy_image_data: procedure(data: Pimage_data); cdecl;

  { generate thumbnail from video file (movie_filename), image data is stored in generated_image_data struct  }
  video_thumbnailer_generate_thumbnail_to_buffer: function(thumbnailer: Pvideo_thumbnailer; movie_filename: Pchar; generated_image_data: Pimage_data): cint; cdecl;

var
  MaskList: TMaskList = nil;
  libffmpeg: TLibHandle = NilHandle;

function GetThumbnail(const aFileName: String; aSize: TSize): Graphics.TBitmap;
var
  Data: Pimage_data;
  BlobStream: TBlobStream;
  Thumb: Pvideo_thumbnailer;
  Bitmap: TPortableNetworkGraphic;
begin
  Result:= nil;

  if MaskList.Matches(aFileName) then
  begin
    Thumb:= video_thumbnailer_create();
    if Assigned(Thumb) then
    try
      Thumb.thumbnail_size:= aSize.cx;
      Data:= video_thumbnailer_create_image_data();
      if Assigned(Data) then
      try
        if video_thumbnailer_generate_thumbnail_to_buffer(Thumb, PAnsiChar(CeUtf8ToSys(aFileName)), Data) = 0 then
        begin
          Bitmap:= TPortableNetworkGraphic.Create;
          BlobStream:= TBlobStream.Create(Data^.image_data_ptr, Data^.image_data_size);
          try
            Bitmap.LoadFromStream(BlobStream);
            Result:= Graphics.TBitmap.Create;
            BitmapAssign(Result, Bitmap);
          except
            FreeAndNil(Result);
          end;
          Bitmap.Free;
          BlobStream.Free;
        end;
      finally
        video_thumbnailer_destroy_image_data(Data);
      end;
    finally
      video_thumbnailer_destroy(Thumb);
    end;
  end;
end;

procedure Initialize;
begin
  libffmpeg:= LoadLibrary('libffmpegthumbnailer.so.4');

  if (libffmpeg <> NilHandle) then
  try
    @video_thumbnailer_create:= SafeGetProcAddress(libffmpeg, 'video_thumbnailer_create');
    @video_thumbnailer_destroy:= SafeGetProcAddress(libffmpeg, 'video_thumbnailer_destroy');

    @video_thumbnailer_create_image_data:= SafeGetProcAddress(libffmpeg, 'video_thumbnailer_create_image_data');
    @video_thumbnailer_destroy_image_data:= SafeGetProcAddress(libffmpeg, 'video_thumbnailer_destroy_image_data');
    @video_thumbnailer_generate_thumbnail_to_buffer:= SafeGetProcAddress(libffmpeg, 'video_thumbnailer_generate_thumbnail_to_buffer');

    // Register thumbnail provider
    TThumbnailManager.RegisterProvider(@GetThumbnail);
    MaskList:= TMaskList.Create('*.avi;*.flv;*.mkv;*.mp4;*.mpg;*.mov;*.wmv;*.vob;*.mpeg;*.webm');
  except
    // Skip
  end;
end;

procedure Finalize;
begin
  MaskList.Free;
  if (libffmpeg <> NilHandle) then FreeLibrary(libffmpeg);
end;

initialization
  Initialize;

finalization
  Finalize;

end.

