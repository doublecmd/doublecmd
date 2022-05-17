unit uDCReadWIC;

{$mode delphi}

interface

uses
  Windows, Classes, SysUtils, FPImage, Graphics, IntfGraphics, ComObj, ActiveX;

const
  WICDecoder = $01;
  WICBitmapCacheOnLoad = $2;
  WICDecodeMetadataCacheOnDemand = 0;

  CLSID_WICImagingFactory: TGUID = '{CACAF262-9370-4615-A13B-9F5539DA4C0A}';
  GUID_WICPixelFormat32bppBGRA: TGUID = '{6FDDC324-4E03-4BFE-B185-3D77768DC90F}';

type
  PWICColor = ^TWicColor;
  TWICColor = Cardinal;
  PWICRect = ^TWICRect;
  TWICRect = record
    X: Integer;
    Y: Integer;
    Width: Integer;
    Height: Integer;
  end;
  PIWICColorContext = ^IWICColorContext;
  PWICBitmapPattern = ^TWICBitmapPattern;
  TWICBitmapPattern = record
    Position: ULARGE_INTEGER;
    Length: ULONG;
    Pattern: PByte;
    Mask: PByte;
    EndOfStream: BOOL;
  end;
  PPropBag2 = ^TPropBag2;
  TPropBag2 = record
    dwType: DWORD;
    vt: TVarType;
    cfType: TClipFormat;
    dwHint: DWORD;
    pstrName: POleStr;
    clsid: TCLSID;
  end;
  TWICInProcPointer = PByte;
  TWICPixelFormatGUID = TGUID;
  TREFWICPixelFormatGUID = PGUID;
  TWICComponentType = type Integer;
  TWICDecodeOptions = type Integer;
  TWICColorContextType = type Integer;
  TWICBitmapDitherType = type Integer;
  TWICBitmapPaletteType = type Integer;
  TWICBitmapInterpolationMode = type Integer;
  TWICBitmapEncoderCacheOption = type Integer;
  TWICBitmapTransformOptions = type Integer;
  TWICBitmapCreateCacheOption = type Integer;
  TWICBitmapAlphaChannelOption = type Integer;

  IWICPalette = interface;
  IWICBitmapLock = interface;
  IWICBitmapEncoderInfo = interface;
  IWICBitmapDecoderInfo = interface;
  IWICBitmapFrameEncode = interface;
  IWICBitmapFrameDecode = interface;
  IWICMetadataQueryReader = interface;
  IWICMetadataQueryWriter = interface;

  IPropertyBag2 = interface(IUnknown)
    ['{22F55882-280B-11d0-A8A9-00A0C90C2004}']
    function Read(pPropBag: PPropBag2; pErrLog: IErrorLog; pvarValue: PVariant; phrError: PHResult): HRESULT; stdcall;
    function Write(cProperties: ULONG; pPropBag: PPropBag2; pvarValue: PVariant): HRESULT; stdcall;
    function CountProperties(var pcProperties: ULONG): HRESULT; stdcall;
    function GetPropertyInfo(iProperty, cProperties: ULONG; pPropBag: PPropBag2; var pcProperties: ULONG): HRESULT; stdcall;
    function LoadObject(pstrName:POleStr; dwHint: DWORD; pUnkObject: IUnknown; pErrLog: IErrorLog): HRESULT; stdcall;
  end;
  IWICComponentInfo = interface(IUnknown)
    ['{23BC3F0A-698B-4357-886B-F24D50671334}']
    function GetComponentType(var pType: TWICComponentType): HRESULT; stdcall;
    function GetCLSID(var pclsid: TGUID): HRESULT; stdcall;
    function GetSigningStatus(var pStatus: DWORD): HRESULT; stdcall;
    function GetAuthor(cchAuthor: UINT; wzAuthor: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;
    function GetVendorGUID(var pguidVendor: TGUID): HRESULT; stdcall;
    function GetVersion(cchVersion: UINT; wzVersion: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;
    function GetSpecVersion(cchSpecVersion: UINT; wzSpecVersion: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;
    function GetFriendlyName(cchFriendlyName: UINT; wzFriendlyName: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;
  end;
  IWICBitmapSource = interface(IUnknown)
    ['{00000120-a8f2-4877-ba0a-fd2b6645fb94}']
    function GetSize(var puiWidth: UINT; var puiHeight: UINT): HRESULT; stdcall;
    function GetPixelFormat(var pPixelFormat: TWICPixelFormatGUID): HRESULT; stdcall;
    function GetResolution(var pDpiX: Double; var pDpiY: Double): HRESULT; stdcall;
    function CopyPalette(pIPalette: IWICPalette): HRESULT; stdcall;
    function CopyPixels(prc: PWICRect; cbStride: UINT; cbBufferSize: UINT; pbBuffer: PByte): HRESULT; stdcall;
  end;
  IWICBitmap = interface(IWICBitmapSource)
    ['{00000121-a8f2-4877-ba0a-fd2b6645fb94}']
    function Lock(const prcLock: TWICRect; flags: DWORD; out ppILock: IWICBitmapLock): HRESULT; stdcall;
    function SetPalette(pIPalette: IWICPalette): HRESULT; stdcall;
    function SetResolution(dpiX: Double; dpiY: Double): HRESULT; stdcall;
  end;
  IWICBitmapLock = interface(IUnknown)
    ['{00000123-a8f2-4877-ba0a-fd2b6645fb94}']
    function GetSize(var puiWidth: UINT; var puiHeight: UINT): HRESULT; stdcall;
    function GetStride(var pcbStride: UINT): HRESULT; stdcall;
    function GetDataPointer(var pcbBufferSize: UINT; var ppbData: TWICInProcPointer): HRESULT; stdcall;
    function GetPixelFormat(var pPixelFormat: TWICPixelFormatGUID): HRESULT; stdcall;
  end;
  IWICBitmapCodecInfo = interface(IWICComponentInfo)
    ['{E87A44C4-B76E-4c47-8B09-298EB12A2714}']
    function GetContainerFormat(var pguidContainerFormat: TGUID): HRESULT; stdcall;
    function GetPixelFormats(cFormats: UINT; var guidPixelFormats: PGUID; var pcActual: UINT): HRESULT; stdcall;
    function GetColorManagementVersion(cchColorManagementVersion: UINT; wzColorManagementVersion: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;
    function GetDeviceManufacturer(cchDeviceManufacturer: UINT; wzDeviceManufacturer: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;
    function GetDeviceModels(cchDeviceModels: UINT; wzDeviceModels: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;
    function GetMimeTypes(cchMimeTypes: UINT; wzMimeTypes: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;
    function GetFileExtensions(cchFileExtensions: UINT; wzFileExtensions: PWCHAR; var pcchActual: UINT): HRESULT; stdcall;
    function DoesSupportAnimation(var pfSupportAnimation: BOOL): HRESULT; stdcall;
    function DoesSupportChromakey(var pfSupportChromakey: BOOL): HRESULT; stdcall;
    function DoesSupportLossless(var pfSupportLossless: BOOL): HRESULT; stdcall;
    function DoesSupportMultiframe(var pfSupportMultiframe: BOOL): HRESULT; stdcall;
    function MatchesMimeType(wzMimeType: LPCWSTR; var pfMatches: BOOL): HRESULT; stdcall;
  end;
  IWICBitmapEncoder = interface(IUnknown)
    ['{00000103-a8f2-4877-ba0a-fd2b6645fb94}']
    function Initialize(pIStream: IStream; cacheOption: TWICBitmapEncoderCacheOption): HRESULT; stdcall;
    function GetContainerFormat(var pguidContainerFormat: TGUID): HRESULT; stdcall;
    function GetEncoderInfo(out ppIEncoderInfo: IWICBitmapEncoderInfo): HRESULT; stdcall;
    function SetColorContexts(cCount: UINT; ppIColorContext: PIWICColorContext): HRESULT; stdcall;
    function SetPalette(pIPalette: IWICPalette): HRESULT; stdcall;
    function SetThumbnail(pIThumbnail: IWICBitmapSource): HRESULT; stdcall;
    function SetPreview(pIPreview: IWICBitmapSource): HRESULT; stdcall;
    function CreateNewFrame(out ppIFrameEncode: IWICBitmapFrameEncode; var ppIEncoderOptions: IPropertyBag2): HRESULT; stdcall;
    function Commit: HRESULT; stdcall;
    function GetMetadataQueryWriter(out ppIMetadataQueryWriter: IWICMetadataQueryWriter): HRESULT; stdcall;
  end;
  IWICBitmapDecoder = interface(IUnknown)
    ['{9EDDE9E7-8DEE-47ea-99DF-E6FAF2ED44BF}']
    function QueryCapability(pIStream: IStream; var pdwCapability: DWORD): HRESULT; stdcall;
    function Initialize(pIStream: IStream; cacheOptions: TWICDecodeOptions): HRESULT; stdcall;
    function GetContainerFormat(var pguidContainerFormat: TGUID): HRESULT; stdcall;
    function GetDecoderInfo(out ppIDecoderInfo: IWICBitmapDecoderInfo): HRESULT; stdcall;
    function CopyPalette(pIPalette: IWICPalette): HRESULT; stdcall;
    function GetMetadataQueryReader(out ppIMetadataQueryReader: IWICMetadataQueryReader): HRESULT; stdcall;
    function GetPreview(out ppIBitmapSource: IWICBitmapSource): HRESULT; stdcall;
    function GetColorContexts(cCount: UINT; ppIColorContexts: PIWICColorContext; var pcActualCount : UINT): HRESULT; stdcall;
    function GetThumbnail(out ppIThumbnail: IWICBitmapSource): HRESULT; stdcall;
    function GetFrameCount(var pCount: UINT): HRESULT; stdcall;
    function GetFrame(index: UINT; out ppIBitmapFrame: IWICBitmapFrameDecode): HRESULT; stdcall;
  end;
  IWICBitmapEncoderInfo = interface(IWICBitmapCodecInfo)
    ['{94C9B4EE-A09F-4f92-8A1E-4A9BCE7E76FB}']
    function CreateInstance(out ppIBitmapEncoder: IWICBitmapEncoder): HRESULT; stdcall;
  end;
  IWICBitmapDecoderInfo = interface(IWICBitmapCodecInfo)
    ['{D8CD007F-D08F-4191-9BFC-236EA7F0E4B5}']
    function GetPatterns(cbSizePatterns: UINT; pPatterns: PWICBitmapPattern; var pcPatterns: UINT; var pcbPatternsActual: UINT): HRESULT; stdcall;
    function MatchesPattern(pIStream: IStream; var pfMatches: BOOL): HRESULT; stdcall;
    function CreateInstance(out ppIBitmapDecoder: IWICBitmapDecoder): HRESULT; stdcall;
  end;
  IWICBitmapFrameEncode = interface(IUnknown)
    ['{00000105-a8f2-4877-ba0a-fd2b6645fb94}']
    function Initialize(pIEncoderOptions: IPropertyBag2): HRESULT; stdcall;
    function SetSize(uiWidth: UINT; uiHeight: UINT): HRESULT; stdcall;
    function SetResolution(dpiX: Double; dpiY: Double): HRESULT; stdcall;
    function SetPixelFormat(var pPixelFormat: TWICPixelFormatGUID): HRESULT; stdcall;
    function SetColorContexts(cCount: UINT; ppIColorContext: PIWICColorContext): HRESULT; stdcall;
    function SetPalette(pIPalette: IWICPalette): HRESULT; stdcall;
    function SetThumbnail(pIThumbnail: IWICBitmapSource): HRESULT; stdcall;
    function WritePixels(lineCount: UINT; cbStride: UINT; cbBufferSize: UINT; pbPixels: PByte): HRESULT; stdcall;
    function WriteSource(pIBitmapSource: IWICBitmapSource; prc: PWICRect): HRESULT; stdcall;
    function Commit: HRESULT; stdcall;
    function GetMetadataQueryWriter(out ppIMetadataQueryWriter: IWICMetadataQueryWriter): HRESULT; stdcall;
  end;
  IWICBitmapFrameDecode = interface(IWICBitmapSource)
    ['{3B16811B-6A43-4ec9-A813-3D930C13B940}']
    function GetMetadataQueryReader(out ppIMetadataQueryReader: IWICMetadataQueryReader): HRESULT; stdcall;
    function GetColorContexts(cCount: UINT; ppIColorContexts: PIWICColorContext; var pcActualCount : UINT): HRESULT; stdcall;
    function GetThumbnail(out ppIThumbnail: IWICBitmapSource): HRESULT; stdcall;
  end;
  IWICBitmapScaler = interface(IWICBitmapSource)
    ['{00000302-a8f2-4877-ba0a-fd2b6645fb94}']
    function Initialize(pISource: IWICBitmapSource; uiWidth: UINT; uiHeight: UINT; mode: TWICBitmapInterpolationMode): HRESULT; stdcall;
  end;
  IWICBitmapClipper = interface(IWICBitmapSource)
    ['{E4FBCF03-223D-4e81-9333-D635556DD1B5}']
    function Initialize(pISource: IWICBitmapSource; var prc: TWICRect): HRESULT; stdcall;
  end;
  IWICBitmapFlipRotator = interface(IWICBitmapSource)
    ['{5009834F-2D6A-41ce-9E1B-17C5AFF7A782}']
    function Initialize(pISource: IWICBitmapSource; options: TWICBitmapTransformOptions): HRESULT; stdcall;
  end;
  IWICPalette = interface(IUnknown)
    ['{00000040-a8f2-4877-ba0a-fd2b6645fb94}']
    function InitializePredefined(ePaletteType: TWICBitmapPaletteType; fAddTransparentColor: BOOL): HRESULT; stdcall;
    function InitializeCustom(pColors: PWICColor; cCount: UINT): HRESULT; stdcall;
    function InitializeFromBitmap(pISurface: IWICBitmapSource; cCount: UINT; fAddTransparentColor: BOOL): HRESULT; stdcall;
    function InitializeFromPalette(pIPalette: IWICPalette): HRESULT; stdcall;
    function GetType(var pePaletteType: TWICBitmapPaletteType): HRESULT; stdcall;
    function GetColorCount(var pcCount: UINT): HRESULT; stdcall;
    function GetColors(cCount: UINT; pColors: PWICColor; var pcActualColors: UINT): HRESULT; stdcall;
    function IsBlackWhite(var pfIsBlackWhite: BOOL): HRESULT; stdcall;
    function IsGrayscale(var pfIsGrayscale: BOOL): HRESULT; stdcall;
    function HasAlpha(var pfHasAlpha: BOOL): HRESULT; stdcall;
  end;
  IWICColorContext = interface(IUnknown)
    ['{3C613A02-34B2-44ea-9A7C-45AEA9C6FD6D}']
    function InitializeFromFilename(wzFilename: LPCWSTR): HRESULT; stdcall;
    function InitializeFromMemory(const pbBuffer: PByte; cbBufferSize: UINT): HRESULT; stdcall;
    function InitializeFromExifColorSpace(value: UINT): HRESULT; stdcall;
    function GetType(var pType: TWICColorContextType): HRESULT; stdcall;
    function GetProfileBytes(cbBuffer: UINT; pbBuffer: PByte; var pcbActual: UINT): HRESULT; stdcall;
    function GetExifColorSpace(var pValue: UINT): HRESULT; stdcall;
  end;
  IWICColorTransform = interface(IWICBitmapSource)
    ['{B66F034F-D0E2-40ab-B436-6DE39E321A94}']
    function Initialize(pIBitmapSource: IWICBitmapSource; pIContextSource: IWICColorContext; pIContextDest: IWICColorContext; pixelFmtDest: TREFWICPixelFormatGUID): HRESULT; stdcall;
  end;
  IWICMetadataQueryReader = interface(IUnknown)
    ['{30989668-E1C9-4597-B395-458EEDB808DF}']
    function GetContainerFormat(var pguidContainerFormat: TGUID): HRESULT; stdcall;
    function GetLocation(cchMaxLength: UINT; wzNamespace: PWCHAR; var pcchActualLength: UINT): HRESULT; stdcall;
    function GetMetadataByName(wzName: LPCWSTR; var pvarValue: PROPVARIANT): HRESULT; stdcall;
    function GetEnumerator(out ppIEnumString: IEnumString): HRESULT; stdcall;
  end;
  IWICMetadataQueryWriter = interface(IWICMetadataQueryReader)
    ['{A721791A-0DEF-4d06-BD91-2118BF1DB10B}']
    function SetMetadataByName(wzName: LPCWSTR; const pvarValue: TPropVariant): HRESULT; stdcall;
    function RemoveMetadataByName(wzName: LPCWSTR): HRESULT; stdcall;
  end;
  IWICFastMetadataEncoder = interface(IUnknown)
    ['{B84E2C09-78C9-4AC4-8BD3-524AE1663A2F}']
    function Commit: HRESULT; stdcall;
    function GetMetadataQueryWriter(out ppIMetadataQueryWriter: IWICMetadataQueryWriter): HRESULT; stdcall;
  end;
  IWICStream = interface(IStream)
    ['{135FF860-22B7-4ddf-B0F6-218F4F299A43}']
    function InitializeFromIStream(pIStream: IStream): HRESULT; stdcall;
    function InitializeFromFilename(wzFileName: LPCWSTR; dwDesiredAccess: DWORD): HRESULT; stdcall;
    function InitializeFromMemory(pbBuffer: TWICInProcPointer; cbBufferSize: DWORD): HRESULT; stdcall;
    function InitializeFromIStreamRegion(pIStream: IStream; ulOffset: ULARGE_INTEGER; ulMaxSize: ULARGE_INTEGER): HRESULT; stdcall;
  end;
  IWICFormatConverter = interface(IWICBitmapSource)
    ['{00000301-a8f2-4877-ba0a-fd2b6645fb94}']
    function Initialize(pISource: IWICBitmapSource; const dstFormat: TWICPixelFormatGUID; dither: TWICBitmapDitherType; const pIPalette: IWICPalette; alphaThresholdPercent: Double; paletteTranslate: TWICBitmapPaletteType): HRESULT; stdcall;
    function CanConvert(srcPixelFormat: TREFWICPixelFormatGUID; dstPixelFormat: TREFWICPixelFormatGUID; var pfCanConvert: BOOL): HRESULT; stdcall;
  end;
  IWICImagingFactory = interface(IUnknown)
    ['{ec5ec8a9-c395-4314-9c77-54d7a935ff70}']
    function CreateDecoderFromFilename(wzFilename: LPCWSTR; const pguidVendor: TGUID; dwDesiredAccess: DWORD; metadataOptions: TWICDecodeOptions; out ppIDecoder: IWICBitmapDecoder): HRESULT; stdcall;
    function CreateDecoderFromStream(pIStream: IStream; const pguidVendor: TGUID; metadataOptions: TWICDecodeOptions; out ppIDecoder: IWICBitmapDecoder): HRESULT; stdcall;
    function CreateDecoderFromFileHandle(hFile: ULONG_PTR; const pguidVendor: TGUID; metadataOptions: TWICDecodeOptions; out ppIDecoder: IWICBitmapDecoder): HRESULT; stdcall;
    function CreateComponentInfo(const clsidComponent: TGUID; out ppIInfo: IWICComponentInfo): HRESULT; stdcall;
    function CreateDecoder(const guidContainerFormat: TGUID; const pguidVendor: TGUID; out ppIDecoder: IWICBitmapDecoder): HRESULT; stdcall;
    function CreateEncoder(const guidContainerFormat: TGUID; const pguidVendor: TGUID; out ppIEncoder: IWICBitmapEncoder): HRESULT; stdcall;
    function CreatePalette(out ppIPalette: IWICPalette): HRESULT; stdcall;
    function CreateFormatConverter(out ppIFormatConverter: IWICFormatConverter): HRESULT; stdcall;
    function CreateBitmapScaler(out ppIBitmapScaler: IWICBitmapScaler): HRESULT; stdcall;
    function CreateBitmapClipper(out ppIBitmapClipper: IWICBitmapClipper): HRESULT; stdcall;
    function CreateBitmapFlipRotator(out ppIBitmapFlipRotator: IWICBitmapFlipRotator): HRESULT; stdcall;
    function CreateStream(out ppIWICStream: IWICStream): HRESULT; stdcall;
    function CreateColorContext(out ppIWICColorContext: IWICColorContext): HRESULT; stdcall;
    function CreateColorTransformer(out ppIWICColorTransform: IWICColorTransform): HRESULT; stdcall;
    function CreateBitmap(uiWidth: UINT; uiHeight: UINT; pixelFormat: TREFWICPixelFormatGUID; option: TWICBitmapCreateCacheOption; out ppIBitmap: IWICBitmap): HRESULT; stdcall;
    function CreateBitmapFromSource(pIBitmapSource: IWICBitmapSource; option: TWICBitmapCreateCacheOption; out ppIBitmap: IWICBitmap): HRESULT; stdcall;
    function CreateBitmapFromSourceRect(pIBitmapSource: IWICBitmapSource; x: UINT; y: UINT; width: UINT; height: UINT; out ppIBitmap: IWICBitmap): HRESULT; stdcall;
    function CreateBitmapFromMemory(uiWidth: UINT; uiHeight: UINT; const pixelFormat: TWICPixelFormatGUID; cbStride: UINT; cbBufferSize: UINT; pbBuffer: PByte; out ppIBitmap: IWICBitmap): HRESULT; stdcall;
    function CreateBitmapFromHBITMAP(hBitmap: HBITMAP; hPalette: HPALETTE; options: TWICBitmapAlphaChannelOption; out ppIBitmap: IWICBitmap): HRESULT; stdcall;
    function CreateBitmapFromHICON(hIcon: HICON; out ppIBitmap: IWICBitmap): HRESULT; stdcall;
    function CreateComponentEnumerator(componentTypes: DWORD; options: DWORD; out ppIEnumUnknown: IEnumUnknown): HRESULT; stdcall;
    function CreateFastMetadataEncoderFromDecoder(pIDecoder: IWICBitmapDecoder; out ppIFastEncoder: IWICFastMetadataEncoder): HRESULT; stdcall;
    function CreateFastMetadataEncoderFromFrameDecode(pIFrameDecoder: IWICBitmapFrameDecode; out ppIFastEncoder: IWICFastMetadataEncoder): HRESULT; stdcall;
    function CreateQueryWriter(const guidMetadataFormat: TGUID; const pguidVendor: TGUID; out ppIQueryWriter: IWICMetadataQueryWriter): HRESULT; stdcall;
    function CreateQueryWriterFromReader(pIQueryReader: IWICMetadataQueryReader; const pguidVendor: TGUID; out ppIQueryWriter: IWICMetadataQueryWriter): HRESULT; stdcall;
  end;

type

  { TImageReaderWIC }

  TImageReaderWIC = class(TFPCustomImageReader)
  private
    FBitmapDecoder: IWICBitmapDecoder;
  protected
    procedure InternalRead({%H-}Str: TStream; Img: TFPCustomImage); override;
    function  InternalCheck(Str: TStream): Boolean; override;
  end;

  { TImageWIC }

  TImageWIC = class(TFPImageBitmap)
  protected
    class function GetReaderClass: TFPCustomImageReaderClass; override;
  public
    class var Extensions: String;
    class function GetFileExtensions: String; override;
  end;

implementation

uses
  GraphType, DCOSUtils;

var
  ImagingFactory: IWICImagingFactory;
  WICConvertBitmapSource: function(const dstFormat: TWICPixelFormatGUID; pISrc: IWICBitmapSource; out ppIDst: IWICBitmapSource): HRESULT; stdcall;

const
  CLSID_WICPngDecoder:  TGUID = '{389ea17b-5078-4cde-b6ef-25c15175c751}';
  CLSID_WICPngDecoder1: TGUID = '{389ea17b-5078-4cde-b6ef-25c15175c751}';
  CLSID_WICPngDecoder2: TGUID = '{e018945b-aa86-4008-9bd4-6777a1e40c11}';
  CLSID_WICBmpDecoder:  TGUID = '{6b462062-7cbf-400d-9fdb-813dd10f2778}';
  CLSID_WICIcoDecoder:  TGUID = '{c61bfcdf-2e0f-4aad-a8d7-e06bafebcdfe}';
  CLSID_WICJpegDecoder: TGUID = '{9456a480-e88b-43ea-9e73-0b2d9b71b1ca}';
  CLSID_WICGifDecoder:  TGUID = '{381dda3c-9ce9-4834-a23e-1f98f8fc52be}';
  CLSID_WICTiffDecoder: TGUID = '{b54e85d9-fe23-499f-8b88-6acea713752b}';
  CLSID_WICCurDecoder:  TGUID = '{22696B76-881B-48D7-88F0-DC6111FF9F0B}';
  CLSID_WICHeicDecoder: TGUID = '{E9A4A80A-44FE-4DE4-8971-7150B10A5199}';

  CLSID_WICIgnoreDecoders: array[0..9] of PGUID = (
    @CLSID_WICPngDecoder,
    @CLSID_WICPngDecoder1,
    @CLSID_WICPngDecoder2,
    @CLSID_WICBmpDecoder,
    @CLSID_WICIcoDecoder,
    @CLSID_WICJpegDecoder,
    @CLSID_WICGifDecoder,
    @CLSID_WICTiffDecoder,
    @CLSID_WICCurDecoder,
    @CLSID_WICHeicDecoder
  );

{ TImageWIC }

class function TImageWIC.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result:= TImageReaderWIC;
end;

class function TImageWIC.GetFileExtensions: String;
begin
  Result:= Extensions;
end;

{ TImageReaderWIC }

procedure TImageReaderWIC.InternalRead(Str: TStream; Img: TFPCustomImage);
var
  AWidth: Cardinal = 0;
  AHeight: Cardinal = 0;
  BitmapObject: IWICBitmap;
  BitmapSource: IWICBitmapSource;
  Description: TRawImageDescription;
  BitmapFrame: IWICBitmapFrameDecode;
begin
  OleCheck(FBitmapDecoder.GetFrame(0, BitmapFrame));

  OleCheck(ImagingFactory.CreateBitmapFromSource(BitmapFrame,
           WICBitmapCacheOnLoad, BitmapObject));

  OleCheck(BitmapObject.GetSize(AWidth, AHeight));

  OleCheck(WICConvertBitmapSource(GUID_WICPixelFormat32bppBGRA, BitmapObject,
           BitmapSource));

  Description.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);
  TLazIntfImage(Img).DataDescription:= Description;

  OleCheck(BitmapSource.CopyPixels(nil, AWidth * 4, AWidth * AHeight * 4,
                                   TLazIntfImage(Img).PixelData));
end;

function TImageReaderWIC.InternalCheck(Str: TStream): Boolean;
var
  AStream: IStream;
begin
  AStream:= TStreamAdapter.Create(Str);
  try
    Result:= (ImagingFactory.CreateDecoderFromStream(AStream, GUID_NULL,
                                                     WICDecodeMetadataCacheOnDemand, FBitmapDecoder) = S_OK);
  finally
    AStream:= nil;
  end;
end;

function IsStandardDecoder(const pclsid: TGUID): Boolean;
var
  Index: Integer;
begin
  for Index:= 0 to High(CLSID_WICIgnoreDecoders) do
  begin
    if IsEqualGUID(pclsid, CLSID_WICIgnoreDecoders[Index]^) then
      Exit(True);
  end;
  Result:= False;
end;

procedure Initialize;
var
  AClass: TGUID;
  ATemp: String;
  hModule: TLibHandle;
  cbActual: ULONG = 0;
  cchActual: UINT = 0;
  dwOptions: DWORD = 0;
  ACodec: IUnknown = nil;
  AInfo: IWICBitmapCodecInfo;
  wzFileExtensions: UnicodeString;
  ppIEnumUnknown: IEnumUnknown = nil;
begin
  if (Win32MajorVersion > 5) then
  try
    OleInitialize(nil);
    OleCheck(CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER or
                              CLSCTX_LOCAL_SERVER, IUnknown, ImagingFactory));
    OleCheck(ImagingFactory.CreateComponentEnumerator(WICDecoder, dwOptions, ppIEnumUnknown));
    SetLength(wzFileExtensions, MaxSmallint + 1);

    while(ppIEnumUnknown.Next(1, ACodec, @cbActual) = S_OK) do
    begin
      if (ACodec.QueryInterface(IWICBitmapCodecInfo, AInfo) = S_OK) then
      begin
        // Skip standard decoders
        if (AInfo.GetCLSID(AClass) = S_OK) then
        begin
          if IsStandardDecoder(AClass) then Continue;
        end;
        if (AInfo.GetFileExtensions(MaxSmallint, PWideChar(wzFileExtensions), cchActual) = S_OK) then
        begin
          ATemp:= UTF8Encode(Copy(wzFileExtensions, 1, cchActual - 1));
          TImageWIC.Extensions+= StringReplace(StringReplace(ATemp, ',', ';', [rfReplaceAll]), '.', '', [rfReplaceAll]) + ';';
        end;
      end;
    end;
    if (Length(TImageWIC.Extensions) > 0) then
    begin
      hModule:= LoadLibraryExW('WindowsCodecs.dll', 0, LOAD_LIBRARY_SEARCH_SYSTEM32);
      if (hModule <> NilHandle) then
      begin
        @WICConvertBitmapSource:= SafeGetProcAddress(hModule, 'WICConvertBitmapSource');

        ImageHandlers.RegisterImageReader('Windows Imaging Component', TImageWIC.Extensions, TImageReaderWIC);
        TPicture.RegisterFileFormat(TImageWIC.Extensions, 'Windows Imaging Component', TImageWIC);
      end;
    end;
  except
    // Skip
  end;
end;

initialization
  Initialize;

end.

