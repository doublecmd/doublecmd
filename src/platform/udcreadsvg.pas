{
   Double Commander
   -------------------------------------------------------------------------
   Scalable Vector Graphics reader implementation (via Image32 library)

   Copyright (C) 2022 Alexander Koblov (alexx2000@mail.ru)

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
  Classes, SysUtils, Graphics, FPImage, ZStream, Img32.SVG.Reader, uVectorImage;

type

  { TSvgReaderEx }

  TSvgReaderEx = class(TSvgReader)
  public
    function LoadFromStream(Stream: TStream): Boolean;
    function LoadFromFile(const FileName: String): Boolean;
  end;

  { TDCReaderSVG }

  TDCReaderSVG = class(TVectorReader)
  private
    FSvgReader: TSvgReaderEx;
  protected
    function  InternalCheck(Stream: TStream): Boolean; override;
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    class function CreateBitmap(const FileName: String; AWidth, AHeight: Integer): TBitmap; override;
  end;

implementation

uses
  IntfGraphics, GraphType, Types, LazUTF8, DCClassesUtf8,
  Img32, Img32.Text, Img32.Vector, Img32.Fmt.SVG, uThumbnails,
  uGraphics;

const
  HEAD_CRC    = $02; { bit 1 set: header CRC present }
  EXTRA_FIELD = $04; { bit 2 set: extra field present }
  ORIG_NAME   = $08; { bit 3 set: original file name present }
  COMMENT     = $10; { bit 4 set: file comment present }

type
  TGzHeader = packed record
    ID1        : Byte;
    ID2        : Byte;
    Method     : Byte;
    Flags      : Byte;
    ModTime    : UInt32;
    XtraFlags  : Byte;
    OS         : Byte;
  end;

function CheckGzipHeader(ASource: TStream): Boolean;
var
  ALength: Integer;
  AHeader: TGzHeader;
begin
  ASource.ReadBuffer(AHeader, SizeOf(TGzHeader));
  Result:= (AHeader.ID1 = $1F) and (AHeader.ID2 = $8B) and (AHeader.Method = 8);
  if Result then
  begin
    // Skip the extra field
    if (AHeader.Flags and EXTRA_FIELD <> 0) then
    begin
      ALength:= ASource.ReadWord;
      while ALength > 0 do
      begin
        ASource.ReadByte;
        Dec(ALength);
      end;
    end;
    // Skip the original file name
    if (AHeader.Flags and ORIG_NAME <> 0) then
    begin
      while (ASource.ReadByte > 0) do;
    end;
    // Skip the .gz file comment
    if (AHeader.Flags and COMMENT <> 0) then
    begin
      while (ASource.ReadByte > 0) do;
    end;
    // Skip the header crc
    if (AHeader.Flags and HEAD_CRC <> 0) then
    begin
      ASource.ReadWord;
    end;
  end;
end;

function BitmapLoadFromScalable(const FileName: String; AWidth, AHeight: Integer): TBitmap;
var
  Image32: TImage32;
  Image: TLazIntfImage;
  SvgReader: TSvgReaderEx;
  Description: TRawImageDescription;
begin
  Result:= nil;

  SvgReader:= TSvgReaderEx.Create;
  try
    if SvgReader.LoadFromFile(FileName) then
    begin
      Image32:= TImage32.Create(AWidth, AHeight);
      try
        SvgReader.DrawImage(Image32, True);

        Image:= TLazIntfImage.Create(AWidth, AHeight);
        try
          Description.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);
          Image.DataDescription:= Description;

          Move(Image32.PixelBase^, Image.PixelData^, AWidth * AHeight * SizeOf(TColor32));

          Result:= TBitmap.Create;
          BitmapAssign(Result, Image);
        finally
          Image.Free;
        end;
      finally
        Image32.Free;
      end;
    end;
  finally
    SvgReader.Free;
  end;
end;

function GetThumbnail(const aFileName: String; aSize: TSize): Graphics.TBitmap;
begin
  Result:= nil;

  if TScalableVectorGraphics.IsFileExtensionSupported(ExtractFileExt(aFileName)) then
  begin
    Result:= BitmapLoadFromScalable(aFileName, aSize.cx, aSize.cy);
  end;
end;

{ TSvgReaderEx }

function TSvgReaderEx.LoadFromStream(Stream: TStream): Boolean;
var
  MemoryStream: TMemoryStream;
  GzipStream: TDecompressionStream;
begin
  if not CheckGzipHeader(Stream) then
    Result:= inherited LoadFromStream(Stream)
  else begin
    MemoryStream:= TMemoryStream.Create;
    try
      GzipStream:= TDecompressionStream.Create(Stream, True);
      try
        MemoryStream.CopyFrom(GzipStream, 0);
        Result:= inherited LoadFromStream(MemoryStream);
      finally
        GzipStream.Free;
      end;
    finally
      MemoryStream.Free;
    end;
  end;
end;

function TSvgReaderEx.LoadFromFile(const FileName: String): Boolean;
var
  AStream: TFileStreamEx;
begin
  try
    AStream:= TFileStreamEx.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      Result:= LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
  except
    Result:= False;
  end;
end;

{ TDCReaderSVG }

function TDCReaderSVG.InternalCheck(Stream: TStream): Boolean;
begin
  Result:= FSvgReader.LoadFromStream(Stream)
end;

procedure TDCReaderSVG.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  ARect: TRect;
  Image32: TImage32;
  Description: TRawImageDescription;
begin
  ARect:= FSvgReader.GetViewbox(128, 128).Rect;

  Image32:= TImage32.Create(ARect.Width, ARect.Height);
  try
    FSvgReader.DrawImage(Image32, True);
    Description.Init_BPP32_B8G8R8A8_BIO_TTB(ARect.Width, ARect.Height);
    TLazIntfImage(Img).DataDescription:= Description;
    Move(Image32.PixelBase^, TLazIntfImage(Img).PixelData^, Img.Width * Img.Height * SizeOf(TColor32));
  finally
    Image32.Free;
  end;
end;

constructor TDCReaderSVG.Create;
begin
  inherited Create;
  FSvgReader:= TSvgReaderEx.Create;
end;

destructor TDCReaderSVG.Destroy;
begin
  inherited Destroy;
  FSvgReader.Free;
end;

class function TDCReaderSVG.CreateBitmap(const FileName: String; AWidth,
  AHeight: Integer): TBitmap;
begin
  Result:= BitmapLoadFromScalable(FileName, AWidth, AHeight);
end;

procedure Initialize;
begin
  if (TScalableVectorGraphics.GetReaderClass = nil) then
  begin
{$IF DEFINED(MSWINDOWS)}
    FontManager.Load('Times New Roman');
    FontManager.Load('Times New Roman Bold');
    FontManager.Load('Times New Roman Italic');
    FontManager.Load('Times New Roman Bold Italic');
{$ENDIF}

    // Register image handler and format
    TThumbnailManager.RegisterProvider(@GetThumbnail);
    TScalableVectorGraphics.RegisterReaderClass(TDCReaderSVG);
    ImageHandlers.RegisterImageReader('Scalable Vector Graphics', 'SVG;SVGZ', TDCReaderSVG);
  end;
end;

initialization
  Initialize;

end.
