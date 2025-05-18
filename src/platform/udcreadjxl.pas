{
   Double Commander
   -------------------------------------------------------------------------
   JPEG XL image format reader implementation (via libjxl)

   Copyright (C) 2025 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uDCReadJXL;

{$mode delphi}
{$packrecords c}
{$packenum 4}

interface

uses
  Classes, SysUtils, Graphics, FPImage;

type

  { TDCReaderJXL }

  TDCReaderJXL = class (TFPCustomImageReader)
  private
    FDecoder: Pointer;
  protected
    function  InternalCheck (Stream: TStream): Boolean; override;
    procedure InternalRead({%H-}Stream: TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TJPEGXLImage }

  TJPEGXLImage = class(TFPImageBitmap)
  protected
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    class function GetFileExtensions: String; override;
  end;

implementation

uses
  DynLibs, IntfGraphics, GraphType, CTypes, DCOSUtils, uDebug;

const
  JXL_EXT = 'jxl';

type
  JXL_BOOL = cint;

  TJxlSignature = (
    JXL_SIG_NOT_ENOUGH_BYTES = 0,
    JXL_SIG_INVALID = 1,
    JXL_SIG_CODESTREAM = 2,
    JXL_SIG_CONTAINER = 3
  );

  TJxlOrientation = (
    JXL_ORIENT_IDENTITY = 1,
    JXL_ORIENT_FLIP_HORIZONTAL = 2,
    JXL_ORIENT_ROTATE_180 = 3,
    JXL_ORIENT_FLIP_VERTICAL = 4,
    JXL_ORIENT_TRANSPOSE = 5,
    JXL_ORIENT_ROTATE_90_CW = 6,
    JXL_ORIENT_ANTI_TRANSPOSE = 7,
    JXL_ORIENT_ROTATE_90_CCW = 8
  );

  TJxlDecoderStatus = (
    JXL_DEC_SUCCESS = 0,
    JXL_DEC_ERROR = 1,
    JXL_DEC_NEED_MORE_INPUT = 2,
    JXL_DEC_NEED_PREVIEW_OUT_BUFFER = 3,
    JXL_DEC_NEED_IMAGE_OUT_BUFFER = 5,
    JXL_DEC_JPEG_NEED_MORE_OUTPUT = 6,
    JXL_DEC_BOX_NEED_MORE_OUTPUT = 7,
    JXL_DEC_BASIC_INFO = $40,
    JXL_DEC_COLOR_ENCODING = $100,
    JXL_DEC_PREVIEW_IMAGE = $200,
    JXL_DEC_FRAME = $400,
    JXL_DEC_FULL_IMAGE = $1000,
    JXL_DEC_JPEG_RECONSTRUCTION = $2000,
    JXL_DEC_BOX = $4000,
    JXL_DEC_FRAME_PROGRESSION = $8000,
    JXL_DEC_BOX_COMPLETE = $10000
  );

  TJxlDecoder = record end;
  PJxlDecoder = ^TJxlDecoder;

  TJxlPreviewHeader = record
    xsize: cuint32;
    ysize: cuint32;
  end;

  TJxlAnimationHeader = record
    tps_numerator: cuint32;
    tps_denominator: cuint32;
    num_loops: cuint32;
    have_timecodes: JXL_BOOL;
  end;

  TJxlBasicInfo = record
    have_container: JXL_BOOL;
    xsize: cuint32;
    ysize: cuint32;
    bits_per_sample: cuint32;
    exponent_bits_per_sample: cuint32;
    intensity_target: cfloat;
    min_nits: cfloat;
    relative_to_max_display: JXL_BOOL;
    linear_below: cfloat;
    uses_original_profile: JXL_BOOL;
    have_preview: JXL_BOOL;
    have_animation: JXL_BOOL;
    orientation: TJxlOrientation;
    num_color_channels: cuint32;
    num_extra_channels: cuint32;
    alpha_bits: cuint32;
    alpha_exponent_bits: cuint32;
    alpha_premultiplied: JXL_BOOL;
    preview: TJxlPreviewHeader;
    animation: TJxlAnimationHeader;
    intrinsic_xsize: cuint32;
    intrinsic_ysize: cuint32;
    padding: array[0..99] of cuint8;
  end;
  PJxlBasicInfo = ^TJxlBasicInfo;

  TJxlDataType = (
    JXL_TYPE_FLOAT = 0,
    JXL_TYPE_UINT8 = 2,
    JXL_TYPE_UINT16 = 3,
    JXL_TYPE_FLOAT16 = 5
  );

  TJxlEndianness = (
    JXL_NATIVE_ENDIAN = 0,
    JXL_LITTLE_ENDIAN = 1,
    JXL_BIG_ENDIAN = 2
  );

  TJxlPixelFormat = record
    num_channels: cuint32;
    data_type: TJxlDataType;
    endianness: TJxlEndianness;
    align: csize_t;
  end;
  PJxlPixelFormat = ^TJxlPixelFormat;

  TJxlColorEncoding = record
    stub: array[Byte] of Byte;
  end;
  PJxlColorEncoding = ^TJxlColorEncoding;

var
  { libjxl }
  JxlDecoderCreate: function(const memory_manager: Pointer): PJxlDecoder; cdecl;
  JxlDecoderDestroy: procedure(dec: PJxlDecoder); cdecl;
  JxlSignatureCheck: function(const buf: pcuint8; len: csize_t): TJxlSignature; cdecl;
  JxlDecoderSetInput: function(dec: PJxlDecoder; const data: pcuint8; size: csize_t): TJxlDecoderStatus; cdecl;
  JxlDecoderCloseInput: procedure(dec: PJxlDecoder); cdecl;
  JxlDecoderSetImageOutBuffer: function(dec: PJxlDecoder; const format: PJxlPixelFormat; buffer: Pointer; size: csize_t): TJxlDecoderStatus; cdecl;
  JxlDecoderGetBasicInfo: function(dec: PJxlDecoder; info: PJxlBasicInfo): TJxlDecoderStatus; cdecl;
  JxlDecoderProcessInput: function(dec: PJxlDecoder): TJxlDecoderStatus; cdecl;
  JxlDecoderSubscribeEvents: function(dec: PJxlDecoder; events_wanted: cint): TJxlDecoderStatus; cdecl;
  JxlColorEncodingSetToSRGB: procedure(color_encoding: PJxlColorEncoding; is_gray: JXL_BOOL); cdecl;
  JxlDecoderSetPreferredColorProfile: function(dec: PJxlDecoder; const color_encoding: PJxlColorEncoding): TJxlDecoderStatus; cdecl;
  { libjxl_threads }
  JxlResizableParallelRunner: function(): Pointer; cdecl;
  JxlResizableParallelRunnerCreate: function(memory_manager: Pointer): Pointer; cdecl;
  JxlResizableParallelRunnerDestroy: procedure(runner_opaque: Pointer); cdecl;
  JxlDecoderSetParallelRunner: function(dec: PJxlDecoder; parallel_runner: Pointer; parallel_runner_opaque: Pointer): TJxlDecoderStatus; cdecl;
  JxlResizableParallelRunnerSuggestThreads: function(xsize: cuint64; ysize: cuint64): cuint32; cdecl;
  JxlResizableParallelRunnerSetThreads: procedure(runner_opaque: Pointer; num_threads: csize_t); cdecl;

{ TJPEGXLImage }

class function TJPEGXLImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result:= TDCReaderJXL;
end;

class function TJPEGXLImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:= TSharedBitmap;
end;

class function TJPEGXLImage.GetFileExtensions: String;
begin
  Result:= JXL_EXT;
end;

{ TDCReaderJXL }

function TDCReaderJXL.InternalCheck(Stream: TStream): Boolean;
var
  Signature: TJxlSignature;
  MemoryStream: TMemoryStream;
begin
  Result:= Stream is TMemoryStream;
  if Result then
  begin
    MemoryStream:= TMemoryStream(Stream);
    Signature:= JxlSignatureCheck(MemoryStream.Memory, Stream.Size);
    Result:= Signature in [JXL_SIG_CODESTREAM, JXL_SIG_CONTAINER];
  end;
end;

procedure TDCReaderJXL.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  ASize: csize_t;
  ACount: csize_t;
  ARunner: Pointer;
  Info: TJxlBasicInfo;
  AFormat: TJxlPixelFormat;
  Status: TJxlDecoderStatus;
  MemoryStream: TMemoryStream;
  AColorEncoding: TJxlColorEncoding;
  Description: TRawImageDescription;
begin
  if Assigned(JxlResizableParallelRunnerCreate) then
    ARunner:= JxlResizableParallelRunnerCreate(nil)
  else begin
    ARunner:= nil;
  end;
  try
    Status:= JxlDecoderSubscribeEvents(FDecoder, cint(JXL_DEC_BASIC_INFO) or
                                                 cint(JXL_DEC_COLOR_ENCODING) or
                                                 cint(JXL_DEC_FULL_IMAGE));
    if (Status <> JXL_DEC_SUCCESS) then
      raise Exception.Create('JxlDecoderSubscribeEvents');

    if Assigned(ARunner) then
    begin
      Status:= JxlDecoderSetParallelRunner(FDecoder, @JxlResizableParallelRunner, ARunner);
      if (Status <> JXL_DEC_SUCCESS) then
        raise Exception.Create('JxlDecoderSetParallelRunner');
    end;

    MemoryStream:= TMemoryStream(Stream);

    if (JxlDecoderSetInput(FDecoder, MemoryStream.Memory, MemoryStream.Size) <> JXL_DEC_SUCCESS) then
      raise Exception.Create('JxlDecoderSetInput');

    JxlDecoderCloseInput(FDecoder);

    AFormat.align:= 0;
    AFormat.num_channels:= 4;
    AFormat.data_type:= JXL_TYPE_UINT8;
    AFormat.endianness:= JXL_NATIVE_ENDIAN;

    JxlColorEncodingSetToSRGB(@AColorEncoding, 0);

    while True do
    begin
      Status:= JxlDecoderProcessInput(FDecoder);

      case Status of
        JXL_DEC_ERROR:
          raise Exception.Create('JxlDecoderProcessInput(JXL_DEC_ERROR)');
        JXL_DEC_NEED_MORE_INPUT:
          raise Exception.Create('JxlDecoderProcessInput(JXL_DEC_NEED_MORE_INPUT)');
        JXL_DEC_BASIC_INFO:
        begin
          if (JxlDecoderGetBasicInfo(FDecoder, @Info)) <> JXL_DEC_SUCCESS then
            raise Exception.Create('JxlDecoderGetBasicInfo');

          Description.Init_BPP32_R8G8B8A8_BIO_TTB(Info.xsize, Info.ysize);
          TLazIntfImage(Img).DataDescription:= Description;

          if Assigned(ARunner) then
          begin
            ACount:= JxlResizableParallelRunnerSuggestThreads(Info.xsize, Info.ysize);
            JxlResizableParallelRunnerSetThreads(ARunner, ACount);
          end;
        end;
        JXL_DEC_COLOR_ENCODING:
        begin
          Status:= JxlDecoderSetPreferredColorProfile(FDecoder, @AColorEncoding);
        end;
        JXL_DEC_NEED_IMAGE_OUT_BUFFER:
        begin
          ASize:= Info.xsize * Info.ysize * 4;
          if (JxlDecoderSetImageOutBuffer(FDecoder, @AFormat, TLazIntfImage(Img).PixelData,
                                          ASize) <> JXL_DEC_SUCCESS) then
             raise Exception.Create('JxlDecoderSetImageOutBuffer');
        end;
        JXL_DEC_SUCCESS,
        JXL_DEC_FULL_IMAGE:
        begin
          Break;
        end
        else begin
          raise Exception.Create(EmptyStr);
        end;
      end;
    end;

  finally
    if Assigned(ARunner) then
    begin
      JxlResizableParallelRunnerDestroy(ARunner);
    end;
  end;
end;

constructor TDCReaderJXL.Create;
begin
  inherited Create;
  FDecoder:= JxlDecoderCreate(nil);
end;

destructor TDCReaderJXL.Destroy;
begin
  inherited Destroy;
  if Assigned(FDecoder) then JxlDecoderDestroy(FDecoder);
end;

const
{$IF DEFINED(UNIX)}
  jxllib = 'libjxl.so.0.%d';
  jxl_threadslib = 'libjxl_threads.so.0.%d';
{$ELSEIF DEFINED(MSWINDOWS)}
  jxllib = 'libjxl.dll';
  jxl_threadslib = 'libjxl_threads.dll';
{$ENDIF}

var
  libjxl,
  libjxl_threads: TLibHandle;

procedure LoadLibrary;
{$IF DEFINED(MSWINDOWS)}
begin
  libjxl:= mbLoadLibraryEx(jxllib);
  if libjxl <> NilHandle then
  begin
    libjxl_threads:= mbLoadLibraryEx(jxl_threadslib);
  end;
end;
{$ELSE}
var
  Version: Integer;
  LibraryName: String;
begin
  for Version:= 11 downto 7 do
  begin
    LibraryName:= Format(jxllib, [Version]);
    libjxl:= mbLoadLibraryEx(LibraryName);
    if (libjxl <> NilHandle) then
    begin
      libjxl_threads:= mbLoadLibraryEx(Format(jxl_threadslib, [Version]));
      Break;
    end;
  end;
end;
{$ENDIF}

procedure Initialize;
begin
  LoadLibrary;

  if (libjxl <> NilHandle) then
  try
    @JxlDecoderCreate:= SafeGetProcAddress(libjxl, 'JxlDecoderCreate');
    @JxlDecoderDestroy:= SafeGetProcAddress(libjxl, 'JxlDecoderDestroy');
    @JxlSignatureCheck:= SafeGetProcAddress(libjxl, 'JxlSignatureCheck');
    @JxlDecoderSetInput:= SafeGetProcAddress(libjxl, 'JxlDecoderSetInput');
    @JxlDecoderCloseInput:= SafeGetProcAddress(libjxl, 'JxlDecoderCloseInput');
    @JxlDecoderProcessInput:= SafeGetProcAddress(libjxl, 'JxlDecoderProcessInput');
    @JxlDecoderGetBasicInfo:= SafeGetProcAddress(libjxl, 'JxlDecoderGetBasicInfo');
    @JxlDecoderSubscribeEvents:= SafeGetProcAddress(libjxl, 'JxlDecoderSubscribeEvents');
    @JxlColorEncodingSetToSRGB:= SafeGetProcAddress(libjxl, 'JxlColorEncodingSetToSRGB');
    @JxlDecoderSetImageOutBuffer:= SafeGetProcAddress(libjxl, 'JxlDecoderSetImageOutBuffer');
    @JxlDecoderSetParallelRunner:= SafeGetProcAddress(libjxl, 'JxlDecoderSetParallelRunner');
    @JxlDecoderSetPreferredColorProfile:= SafeGetProcAddress(libjxl, 'JxlDecoderSetPreferredColorProfile');

    if (libjxl_threads <> NilHandle) then
    try
      @JxlResizableParallelRunner:= SafeGetProcAddress(libjxl_threads, 'JxlResizableParallelRunner');
      @JxlResizableParallelRunnerCreate:= SafeGetProcAddress(libjxl_threads, 'JxlResizableParallelRunnerCreate');
      @JxlResizableParallelRunnerDestroy:= SafeGetProcAddress(libjxl_threads, 'JxlResizableParallelRunnerDestroy');
      @JxlResizableParallelRunnerSetThreads:= SafeGetProcAddress(libjxl_threads, 'JxlResizableParallelRunnerSetThreads');
      @JxlResizableParallelRunnerSuggestThreads:= SafeGetProcAddress(libjxl_threads, 'JxlResizableParallelRunnerSuggestThreads');
    except
      FreeLibrary(libjxl_threads);
      libjxl_threads:= NilHandle;
    end;

    // Register image handler and format
    ImageHandlers.RegisterImageReader ('JPEG XL Image', JXL_EXT, TDCReaderJXL);
    TPicture.RegisterFileFormat(JXL_EXT, 'JPEG XL Image', TJPEGXLImage);
  except
    on E: Exception do
    begin
      DCDebug(E.Message);
      FreeLibrary(libjxl);
      libjxl:= NilHandle;
    end;
  end;
end;

procedure Finalize;
begin
  if (libjxl <> NilHandle) then FreeLibrary(libjxl);
  if (libjxl_threads <> NilHandle) then FreeLibrary(libjxl_threads);
end;

initialization
  Initialize;

finalization
  Finalize;

end.

