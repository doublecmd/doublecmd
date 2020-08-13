unit uGdiPlusJPEG;

{$mode delphi}

interface

uses
  Classes, SysUtils, FPImage, Graphics, IntfGraphics;

type

  { TGdiPlusReaderJPEG }

  TGdiPlusReaderJPEG = class(TFPCustomImageReader)
  private
    FGrayscale: Boolean;
  protected
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function  InternalCheck(Str: TStream): boolean; override;
  end;

  { TGdiPlusJPEGImage }

  TGdiPlusJPEGImage = class(TJPEGImage)
  protected
    procedure InitializeReader({%H-}AImage: TLazIntfImage; {%H-}AReader: TFPCustomImageReader); override;
    procedure FinalizeReader({%H-}AReader: TFPCustomImageReader); override;
    class function GetReaderClass: TFPCustomImageReaderClass; override;
  end;

implementation

uses
  GraphType, LCLStrConsts, SysConst, DCOSUtils, uGdiPlus;

{ TGdiPlusJPEGImage }

procedure TGdiPlusJPEGImage.InitializeReader(AImage: TLazIntfImage;
  AReader: TFPCustomImageReader);
begin

end;

procedure TGdiPlusJPEGImage.FinalizeReader(AReader: TFPCustomImageReader);
begin
  PBoolean(@GrayScale)^ := TGdiPlusReaderJPEG(AReader).FGrayScale;
end;

class function TGdiPlusJPEGImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result:= TGdiPlusReaderJPEG;
end;

{ TGdiPlusReaderJPEG }

procedure TGdiPlusReaderJPEG.InternalRead(Str: TStream; Img: TFPCustomImage);
var
  Result: GPSTATUS;
  PixelFormat: GPPIXELFORMAT;
begin
  Result:= GdiPlusLoadFromStream(Str, Img, PixelFormat);
  if (Result <> Ok) then
    raise Exception.CreateFmt(SUnknownErrorCode, [Result]);
  FGrayScale:= (PixelFormat = PixelFormat16bppGrayScale);
end;

function TGdiPlusReaderJPEG.InternalCheck(Str: TStream): boolean;
begin
  Result:= TJpegImage.IsStreamFormatSupported(Str);
end;

procedure Initialize;
begin
  if IsGdiPlusLoaded then
  try
    // Replace image handler
    GraphicFilter(TJPEGImage);
    TPicture.UnregisterGraphicClass(TJPEGImage);
    TPicture.RegisterFileFormat(TJpegImage.GetFileExtensions, rsJpeg, TGdiPlusJPEGImage);
  except
    // Skip
  end;
end;

initialization
  Initialize;

end.

