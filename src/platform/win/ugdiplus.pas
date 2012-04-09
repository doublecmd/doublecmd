{
    Double Commander
    -------------------------------------------------------------------------
    This unit contains some GDI+ API functions

    Copyright (C) 2008  Koblov Alexander (Alexx2000@mail.ru)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit uGdiPlus;

{$mode objfpc}{$H+}

interface

uses
  Windows;

type
  GPSTATUS = (
    Ok,
    GenericError,
    InvalidParameter,
    OutOfMemory,
    ObjectBusy,
    InsufficientBuffer,
    NotImplemented,
    Win32Error,
    WrongState,
    Aborted,
    FileNotFound,
    ValueOverflow,
    AccessDenied,
    UnknownImageFormat,
    FontFamilyNotFound,
    FontStyleNotFound,
    NotTrueTypeFont,
    UnsupportedGdiplusVersion,
    GdiplusNotInitialized,
    PropertyNotFound,
    PropertyNotSupported
  );

  GpColorAdjustType = (
    ColorAdjustTypeDefault = 0,
    ColorAdjustTypeBitmap = 1,
    ColorAdjustTypeBrush = 2,
    ColorAdjustTypePen = 3,
    ColorAdjustTypeText = 4,
    ColorAdjustTypeCount = 5,
    ColorAdjustTypeAny = 6
  );

  GpUnit = (
    UnitWorld = 0,
    UnitDisplay = 1,
    UnitPixel = 2,
    UnitPoint = 3,
    UnitInch = 4,
    UnitDocument = 5,
    UnitMillimeter = 6
  );

const
  GdipPixelFormatIndexed   =   $00010000; // Indexes into a palette
  GdipPixelFormatGDI       =   $00020000; // Is a GDI-supported format
  GdipPixelFormatAlpha     =   $00040000; // Has an alpha component
  GdipPixelFormatPAlpha    =   $00080000; // Pre-multiplied alpha
  GdipPixelFormatExtended  =   $00100000; // Extended color 16 bits/channel
  GdipPixelFormatCanonical =   $00200000;

type
  GPPIXELFORMAT = (
    // ...
    PixelFormat32bppRGB     =  ( 9 or (32 shl 8) or GdipPixelFormatGDI),
    PixelFormat32bppARGB    =  (10 or (32 shl 8) or GdipPixelFormatAlpha or
                                                    GdipPixelFormatGDI or
                                                    GdipPixelFormatCanonical),
    PixelFormat32bppPARGB   =  (11 or (32 shl 8) or GdipPixelFormatAlpha or
                                                    GdipPixelFormatPAlpha or
                                                    GdipPixelFormatGDI)
    // ...
  );

  GpGraphics = Pointer;
  GpImage = Pointer;
  GpBitmap = Pointer;
  GpImageAttributes = Pointer;

type

  TDebugEventLevel = (DebugEventLevelFatal, DebugEventLevelWarning);

  // Callback function that GDI+ can call, on debug builds, for assertions
  // and warnings.

  TDebugEventProc = procedure(level: TDebugEventLevel; message: PChar); stdcall;

  // Notification functions which the user must call appropriately if
  // "SuppressBackgroundThread" (below) is set.

  TNotificationHookProc = function(out token: ULONG): GPSTATUS; stdcall;

  TNotificationUnhookProc = procedure(token: ULONG); stdcall;

  // Input structure for GdiplusStartup

  GdiplusStartupInput = packed record
    GdiplusVersion          : Cardinal;        // Must be 1
    DebugEventCallback      : TDebugEventProc; // Ignored on free builds
    SuppressBackgroundThread: BOOL;            // FALSE unless you're prepared to call
                                               // the hook/unhook functions properly
    SuppressExternalCodecs  : BOOL;            // FALSE unless you want GDI+ only to use
  end;                                         // its internal image codecs.
  TGdiplusStartupInput = GdiplusStartupInput;
  PGdiplusStartupInput = ^TGdiplusStartupInput;

  // Output structure for GdiplusStartup()

  GdiplusStartupOutput = packed record
    NotificationHook  : TNotificationHookProc;
    NotificationUnhook: TNotificationUnhookProc;
  end;
  TGdiplusStartupOutput = GdiplusStartupOutput;
  PGdiplusStartupOutput = ^TGdiplusStartupOutput;

  PGdiPlusBitmapData = ^GdiPlusBitmapData;
  GdiPlusBitmapData = packed record
    Width: UINT;
    Height: UINT;
    Stride: UINT;
    PixelFormat: GPPIXELFORMAT;
    Scan0: LPBYTE;
    Reserved: UINT_PTR;
  end;

  PARGBQUAD = ^ARGBQUAD;
  ARGBQUAD = record
        rgbBlue : BYTE;
        rgbGreen : BYTE;
        rgbRed : BYTE;
        rgbAlpha : BYTE;
     end;

const
  GdipImageLockModeRead         = 1;
  GdipImageLockModeWrite        = 2;
  GdipImageLockModeUserInputBuf = 4;

type
  // functions prototypes
  TGdiplusStartup = function (out token: ULONG; input: PGdiplusStartupInput;
                              output: PGdiplusStartupOutput): GPSTATUS; stdcall;
  TGdiplusShutdown = procedure (token: ULONG); stdcall;
  TGdipCreateBitmapFromHICON = function (hicon: HICON;
                                         out bitmap: GPBITMAP): GPSTATUS; stdcall;
  TGdipCreateBitmapFromHBITMAP = function (hbitmap: HBITMAP; hpalette: HPALETTE;
                                           out bitmap: GPBITMAP): GPSTATUS; stdcall;
  TGdipCreateBitmapFromScan0 = function (Width, Height: Integer; Stride: Integer; PixelFormat: GPPIXELFORMAT;
                                         Scan0: LPBYTE; out bitmap: GPBITMAP): GPSTATUS; stdcall;
  TGdipCreateBitmapFromGraphics = function (Width, Height: Integer;
                                            graphics: GPGRAPHICS;
                                            out bitmap: GPBITMAP): GPSTATUS; stdcall;
  TGdipCreateFromHDC = function (hdc: HDC; out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  TGdipDrawImageRectI = function (graphics: GPGRAPHICS; image: GPIMAGE; x: Integer;
                                  y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  TGdipDrawImageRectRectI = function (graphics: GPGRAPHICS; image: GPIMAGE;
                                      dstx, dsty, dstwidth, dstheight: Integer;
                                      srcx, srcy, srcwidth, srcheight: Integer;
                                      srcUnit: GpUnit; imageattr: GPIMAGEATTRIBUTES;
                                      abortCallback: Pointer = nil;
                                      callbackData: Pointer = nil): GPSTATUS; stdcall;
  TGdipDisposeImage = function (image: GPIMAGE): GPSTATUS; stdcall;
  TGdipDeleteGraphics = function (graphics: GPGRAPHICS): GPSTATUS; stdcall;
  TGdipGraphicsClear = function (graphics: GPGRAPHICS; color: Integer): GPSTATUS; stdcall;
  TGdipSetInterpolationMode = function (graphics: GPGRAPHICS; interpolation: Integer): GPSTATUS; stdcall;
  TGdipCreateImageAttributes = function (out imageattr: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  TGdipDisposeImageAttributes = function (imageattr: GPIMAGEATTRIBUTES): GPSTATUS; stdcall;
  TGdipSetImageAttributesColorKeys = function (imageattr: GPIMAGEATTRIBUTES; ColorAdjustType: GpColorAdjustType;
                                               Enable: BOOL; ColorLow: LONG; ColorHigh: LONG): GPSTATUS; stdcall;
  TGdipBitmapLockBits = function (bitmap: GPBITMAP; rect: LPRECT; flags: UINT;
                                  PixelFormat: GPPIXELFORMAT;
                                  lockedData: PGdiPlusBitmapData): GPSTATUS; stdcall;
  TGdipBitmapUnlockBits = function (bitmap: GPBITMAP; lockedData: PGdiPlusBitmapData): GPSTATUS; stdcall;
  TGdipGetImagePixelFormat = function (image: GPIMAGE; out pixelFormat: GPPIXELFORMAT): GPSTATUS; stdcall;

var
  IsGdiPlusLoaded: Boolean = False;
  GdiplusStartup: TGdiplusStartup;
  GdiplusShutdown: TGdiplusShutdown;
  GdipCreateBitmapFromHICON: TGdipCreateBitmapFromHICON;
  GdipCreateBitmapFromHBITMAP: TGdipCreateBitmapFromHBITMAP;
  GdipCreateBitmapFromScan0: TGdipCreateBitmapFromScan0;
  GdipCreateBitmapFromGraphics: TGdipCreateBitmapFromGraphics;
  GdipCreateFromHDC: TGdipCreateFromHDC;
  GdipDrawImageRectI: TGdipDrawImageRectI;
  GdipDrawImageRectRectI: TGdipDrawImageRectRectI;
  GdipDisposeImage: TGdipDisposeImage;
  GdipDeleteGraphics: TGdipDeleteGraphics;
  GdipGraphicsClear: TGdipGraphicsClear;
  GdipSetInterpolationMode: TGdipSetInterpolationMode;
  GdipCreateImageAttributes: TGdipCreateImageAttributes;
  GdipDisposeImageAttributes: TGdipDisposeImageAttributes;
  GdipSetImageAttributesColorKeys: TGdipSetImageAttributesColorKeys;
  GdipBitmapLockBits: TGdipBitmapLockBits;
  GdipBitmapUnlockBits: TGdipBitmapUnlockBits;
  GdipGetImagePixelFormat: TGdipGetImagePixelFormat;

function GdiPlusStretchDraw(hicn: hIcon; hCanvas: HDC; X, Y, cxWidth, cyHeight: Integer): Boolean;
function GdiPlusStretchDraw(himl: hImageList; ImageIndex: Integer; hCanvas: HDC; X, Y, cxWidth, cyHeight: Integer): Boolean;

implementation

uses
  CommCtrl;

var
  StartupInput: TGDIPlusStartupInput;
  gdiplusToken: ULONG;

function GetBitmapPixels(hDC: HDC; BitmapInfo: LPBITMAPINFO; hBitmap: HBITMAP): PBYTE;
begin;
  // Buffer must be aligned to DWORD (it should automatically be on a 32-bit machine).
  Result := GetMem(BitmapInfo^.bmiHeader.biWidth *
                   BitmapInfo^.bmiHeader.biHeight *
                   BitmapInfo^.bmiHeader.biBitCount shr 3);

  if GetDIBits(hDC, hBitmap, 0, BitmapInfo^.bmiHeader.biHeight,
               Result, BitmapInfo, DIB_RGB_COLORS) = 0 then
  begin
    Freemem(Result);
    Result := nil;
  end;
end;

function GetBitmapFromARGBPixels(graphics: GPGRAPHICS; pixels: LPBYTE; Width, Height: Integer): GPBITMAP;
var
  x, y: Integer;
  pSrc, pDst: LPDWORD;
  bmBounds: TRECT;
  bmData: GdiPlusBitmapData;
begin
  if GdipCreateBitmapFromGraphics(Width, Height, graphics, Result) <> ok then
    Exit(nil);

  Windows.SetRect(@bmBounds, 0, 0, Width, Height);

  if GdipBitmapLockBits(Result, @bmBounds, GdipImageLockModeWrite,
                        PixelFormat32bppARGB, @bmData) <> ok then
  begin
    GdipDisposeImage(Result);
    Exit(nil);
  end;

  pSrc := LPDWORD(pixels);
  pDst := LPDWORD(bmData.Scan0);

  // Pixels retrieved by GetDIBits are bottom-up, left-right.
  for x := 0 to Width - 1 do
    for y := 0 to Height - 1 do
      pDst[(Height - 1 - y) * Width + x] := pSrc[y * Width + x];

  GdipBitmapUnlockBits(Result, @bmData);
end;

function HasAlphaChannel(pixels: LPBYTE; Width, Height: Integer): Boolean;
var
  i: Integer;
begin
  for i := 0 to Width * Height - 1 do
  begin
    if PARGBQUAD(pixels)[i].rgbAlpha <> 0 then
      Exit(True);
  end;
  Result := False;
end;

function GdiPlusStretchDraw(hicn: hIcon; hCanvas: HDC; X, Y, cxWidth, cyHeight: Integer): Boolean;
var
  pIcon: GPIMAGE;
  pCanvas: GPGRAPHICS;
  IconInfo: TICONINFO;
  BitmapInfo: TBITMAPINFO;
  pixels: LPBYTE = nil;
begin
  Result:= False;

  if GetIconInfo(hicn, IconInfo) = False then
    Exit;

  try
    GdipCreateFromHDC(hCanvas, pCanvas);

    // Prepare bitmap info structure.
    FillMemory(@BitmapInfo, sizeof(BitmapInfo), 0);
    BitmapInfo.bmiHeader.biSize := Sizeof(BitmapInfo.bmiHeader);
    GetDIBits(hCanvas, IconInfo.hbmColor, 0, 0, nil, @BitmapInfo, 0);

    if (BitmapInfo.bmiHeader.biBitCount = 32) then { only 32bpp }
    begin
      // Get pixels data.
      pixels := GetBitmapPixels(hCanvas, @BitmapInfo, IconInfo.hbmColor);

      // Check if the bitmap has alpha channel (have to be 32bpp to have ARGB format).
      if HasAlphaChannel(pixels, BitmapInfo.bmiHeader.biWidth,
                                 BitmapInfo.bmiHeader.biHeight) then
      begin
        // GdipCreateBitmapFromHICON and GdipCreateBitmapFromHBITMAP functions
        // destroy alpha channel (they write alpha=255 for each pixel).
        // Copy the ARGB values manually.
        pIcon := GetBitmapFromARGBPixels(pCanvas, pixels,
                                         BitmapInfo.bmiHeader.biWidth,
                                         BitmapInfo.bmiHeader.biHeight);
      end
      else
        // This is OK for bitmaps without alpha channel or < 32bpp.
        GdipCreateBitmapFromHICON(hicn, pIcon);
    end
    else
      // This is OK for bitmaps without alpha channel or < 32bpp.
      GdipCreateBitmapFromHICON(hicn, pIcon);

    Result:= GdipDrawImageRectI(pCanvas, pIcon, X, Y, cxWidth, cyHeight) = Ok;

  finally
    GdipDisposeImage(pIcon);
    GdipDeleteGraphics(pCanvas);
    DeleteObject(IconInfo.hbmColor);
    DeleteObject(IconInfo.hbmMask);
    if Assigned(pixels) then
      Freemem(pixels);
  end;
end;

function GdiPlusStretchDraw(himl: hImageList; ImageIndex: Integer; hCanvas: HDC; X, Y, cxWidth, cyHeight: Integer): Boolean;
var
  hicn: HICON;
begin
  Result:= False;
  try
    hicn:= ImageList_ExtractIcon(0, himl, ImageIndex);
    Result:= GdiPlusStretchDraw(hicn, hCanvas, X, Y, cxWidth, cyHeight);
  finally
    DestroyIcon(hicn);
  end;
end;

var
  hLib: HMODULE;

initialization
  hLib:= LoadLibrary('gdiplus.dll');
  IsGdiPlusLoaded:= (hLib <> 0);
  if IsGdiPlusLoaded then
    begin
      GdiplusStartup:= TGdiplusStartup(GetProcAddress(hLib, 'GdiplusStartup'));
      GdiplusShutdown:= TGdiplusShutdown(GetProcAddress(hLib, 'GdiplusShutdown'));
      GdipCreateBitmapFromHICON:= TGdipCreateBitmapFromHICON(GetProcAddress(hLib, 'GdipCreateBitmapFromHICON'));
      GdipCreateBitmapFromHBITMAP:= TGdipCreateBitmapFromHBITMAP(GetProcAddress(hLib, 'GdipCreateBitmapFromHBITMAP'));
      GdipCreateBitmapFromScan0:= TGdipCreateBitmapFromScan0(GetProcAddress(hLib, 'GdipCreateBitmapFromScan0'));
      GdipCreateBitmapFromGraphics:= TGdipCreateBitmapFromGraphics(GetProcAddress(hLib, 'GdipCreateBitmapFromGraphics'));
      GdipCreateFromHDC:= TGdipCreateFromHDC(GetProcAddress(hLib, 'GdipCreateFromHDC'));
      GdipDrawImageRectI:= TGdipDrawImageRectI(GetProcAddress(hLib, 'GdipDrawImageRectI'));
      GdipDrawImageRectRectI:= TGdipDrawImageRectRectI(GetProcAddress(hLib, 'GdipDrawImageRectRectI'));
      GdipDisposeImage:= TGdipDisposeImage(GetProcAddress(hLib, 'GdipDisposeImage'));
      GdipDeleteGraphics:= TGdipDeleteGraphics(GetProcAddress(hLib, 'GdipDeleteGraphics'));
      GdipGraphicsClear:= TGdipGraphicsClear(GetProcAddress(hLib, 'GdipGraphicsClear'));
      GdipSetInterpolationMode:= TGdipSetInterpolationMode(GetProcAddress(hLib, 'GdipSetInterpolationMode'));
      GdipCreateImageAttributes:= TGdipCreateImageAttributes(GetProcAddress(hLib, 'GdipCreateImageAttributes'));
      GdipDisposeImageAttributes:= TGdipDisposeImageAttributes(GetProcAddress(hLib, 'GdipDisposeImageAttributes'));
      GdipSetImageAttributesColorKeys:= TGdipSetImageAttributesColorKeys(GetProcAddress(hLib, 'GdipSetImageAttributesColorKeys'));
      GdipBitmapLockBits:= TGdipBitmapLockBits(GetProcAddress(hLib, 'GdipBitmapLockBits'));
      GdipBitmapUnlockBits:= TGdipBitmapUnlockBits(GetProcAddress(hLib, 'GdipBitmapUnlockBits'));
      GdipGetImagePixelFormat:= TGdipGetImagePixelFormat(GetProcAddress(hLib, 'GdipGetImagePixelFormat'));
      // Initialize GDI+ StartupInput structure
      StartupInput.DebugEventCallback:= nil;
      StartupInput.SuppressBackgroundThread:= False;
      StartupInput.SuppressExternalCodecs:= False;
      StartupInput.GdiplusVersion:= 1;
      // Initialize GDI+
      GdiplusStartup(gdiplusToken, @StartupInput, nil);
  end;

finalization
  if IsGdiPlusLoaded then
    begin
      // Close GDI +
      GdiplusShutdown(gdiplusToken);
      GdiplusStartup:= nil;
      GdiplusShutdown:= nil;
      GdipCreateBitmapFromHICON:= nil;
      GdipCreateBitmapFromHBITMAP:= nil;
      GdipCreateBitmapFromScan0:= nil;
      GdipCreateBitmapFromGraphics:= nil;
      GdipCreateFromHDC:= nil;
      GdipDrawImageRectI:= nil;
      GdipDrawImageRectRectI:= nil;
      GdipDisposeImage:= nil;
      GdipDeleteGraphics:= nil;
      GdipGraphicsClear:= nil;
      GdipSetInterpolationMode:= nil;
      GdipCreateImageAttributes:= nil;
      GdipDisposeImageAttributes:= nil;
      GdipSetImageAttributesColorKeys:= nil;
      GdipBitmapLockBits:= nil;
      GdipBitmapUnlockBits:= nil;
      GdipGetImagePixelFormat:= nil;
      FreeLibrary(hLib);
    end;

end.

