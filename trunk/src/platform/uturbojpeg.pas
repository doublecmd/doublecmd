unit uTurboJPEG;

{$mode delphi}

interface

uses
  Classes, SysUtils, FPImage, Graphics, IntfGraphics;

type

  { TDCReaderJPEGTurbo }

  TDCReaderJPEGTurbo = class(TFPCustomImageReader)
  private
    FGrayscale: Boolean;
  protected
    procedure InternalRead(Str: TStream; Img: TFPCustomImage); override;
    function  InternalCheck(Str: TStream): boolean; override;
  end;

  { TJPEGTurboImage }

  TJPEGTurboImage = class(TJPEGImage)
  protected
    procedure InitializeReader({%H-}AImage: TLazIntfImage; {%H-}AReader: TFPCustomImageReader); override;
    procedure FinalizeReader({%H-}AReader: TFPCustomImageReader); override;
    class function GetReaderClass: TFPCustomImageReaderClass; override;
  end;

implementation

uses
  DynLibs, CTypes, GraphType, LCLStrConsts, DCOSUtils;

const
  TJCS_GRAY = 2;
  TJSAMP_GRAY = 3;
  TJFLAG_ACCURATEDCT = 4096;

type
  tjhandle = type Pointer;

  TJPF = (
    TJPF_RGB = 0, TJPF_BGR, TJPF_RGBX, TJPF_BGRX, TJPF_XBGR, TJPF_XRGB,
    TJPF_GRAY, TJPF_RGBA, TJPF_BGRA, TJPF_ABGR, TJPF_ARGB, TJPF_CMYK
  );

var
  tjInitDecompress: function(): tjhandle; cdecl;
  tjDestroy: function(handle: tjhandle): cint; cdecl;

  tjGetErrorStr2: function(handle: tjhandle): PAnsiChar; cdecl;

  tjDecompressHeader3: function(handle: tjhandle;
                                jpegBuf: pcuchar;
                                jpegSize: culong; width: pcint;
                                height: pcint; jpegSubsamp: pcint;
                                jpegColorspace: pcint): cint; cdecl;

  tjDecompress2: function(handle: tjhandle; jpegBuf: pcuchar;
                          jpegSize: culong; dstBuf: pcuchar;
                          width: cint; pitch: cint; height: cint;
                          pixelFormat: cint; flags: cint): cint; cdecl;

{ TJPEGTurboImage }

procedure TJPEGTurboImage.InitializeReader(AImage: TLazIntfImage;
  AReader: TFPCustomImageReader);
begin

end;

procedure TJPEGTurboImage.FinalizeReader(AReader: TFPCustomImageReader);
begin
  PBoolean(@GrayScale)^ := TDCReaderJPEGTurbo(AReader).FGrayScale;
end;

class function TJPEGTurboImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result:= TDCReaderJPEGTurbo;
end;

{ TDCReaderJPEGTurbo }

procedure TDCReaderJPEGTurbo.InternalRead(Str: TStream; Img: TFPCustomImage);
var
  AFormat: TJPF;
  ASize: Integer;
  jpegSubsamp: cint;
  jpegColorspace: cint;
  AWidth, AHeight: cint;
  AStream: TMemoryStream;
  jpegDecompressor: tjhandle;
begin
  ASize:= Str.Size;
  AStream:= Str as TMemoryStream;

  jpegDecompressor:= tjInitDecompress();
  if (jpegDecompressor = nil) then raise Exception.Create(EmptyStr);

  try
    if tjDecompressHeader3(jpegDecompressor, AStream.Memory, ASize, @AWidth, @AHeight, @jpegSubsamp, @jpegColorspace) < 0 then
      raise Exception.Create(tjGetErrorStr2(jpegDecompressor));

    FGrayscale:= (jpegColorspace = TJCS_GRAY) or (jpegSubsamp = TJSAMP_GRAY);

    TLazIntfImage(Img).DataDescription:= QueryDescription([riqfRGB, riqfAlpha], AWidth, AHeight);

    case TLazIntfImage(Img).DataDescription.RedShift of
       0: AFormat:= TJPF_RGBA;
       8: AFormat:= TJPF_ARGB;
      16: AFormat:= TJPF_BGRA;
      24: AFormat:= TJPF_ABGR;
    end;

    if tjDecompress2(jpegDecompressor, AStream.Memory, ASize, TLazIntfImage(Img).PixelData, AWidth, 0, AHeight, cint(AFormat), TJFLAG_ACCURATEDCT) < 0 then
      raise Exception.Create(tjGetErrorStr2(jpegDecompressor));
  finally
    tjDestroy(jpegDecompressor);
  end;
end;

function TDCReaderJPEGTurbo.InternalCheck(Str: TStream): boolean;
begin
  Result:= TJpegImage.IsStreamFormatSupported(Str);
end;

const
{$IF DEFINED(MSWINDOWS)}
  turbolib = 'libturbojpeg.dll';
{$ELSEIF DEFINED(DARWIN)}
  turbolib = 'libturbojpeg.dylib';
{$ELSEIF DEFINED(UNIX)}
  turbolib = 'libturbojpeg.so.0';
{$ENDIF}

var
  libturbo: TLibHandle;

procedure Initialize;
begin
  libturbo:= SafeLoadLibrary(turbolib);

  if (libturbo <> NilHandle) then
  try
    @tjInitDecompress:= GetProcAddress(libturbo, 'tjInitDecompress');
    @tjDestroy:= SafeGetProcAddress(libturbo, 'tjDestroy');
    @tjGetErrorStr2:= SafeGetProcAddress(libturbo, 'tjGetErrorStr2');
    @tjDecompressHeader3:= SafeGetProcAddress(libturbo, 'tjDecompressHeader3');
    @tjDecompress2:= SafeGetProcAddress(libturbo, 'tjDecompress2');

    // Replace image handler
    GraphicFilter(TJPEGImage);
    TPicture.UnregisterGraphicClass(TJPEGImage);
    TPicture.RegisterFileFormat(TJpegImage.GetFileExtensions, rsJpeg, TJPEGTurboImage);
  except
    // Skip
  end;
end;

initialization
  Initialize;

finalization
  if (libturbo <> NilHandle) then FreeLibrary(libturbo);

end.

