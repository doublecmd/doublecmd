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
  GpGraphics = Pointer;
  GpImage = Pointer;
  GpBitmap = Pointer;

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

type
  // functions prototypes
  TGdiplusStartup = function (out token: ULONG; input: PGdiplusStartupInput;
                              output: PGdiplusStartupOutput): GPSTATUS; stdcall;
  TGdiplusShutdown = procedure (token: ULONG); stdcall;
  TGdipCreateBitmapFromHICON = function (hicon: HICON;
                                         out bitmap: GPBITMAP): GPSTATUS; stdcall;
  TGdipCreateFromHDC = function (hdc: HDC; out graphics: GPGRAPHICS): GPSTATUS; stdcall;
  TGdipDrawImageRectI = function (graphics: GPGRAPHICS; image: GPIMAGE; x: Integer;
                                  y: Integer; width: Integer; height: Integer): GPSTATUS; stdcall;
  TGdipDisposeImage = function (image: GPIMAGE): GPSTATUS; stdcall;
  TGdipDeleteGraphics = function (graphics: GPGRAPHICS): GPSTATUS; stdcall;

var
  IsGdiPlusLoaded: Boolean = False;
  GdiplusStartup: TGdiplusStartup;
  GdiplusShutdown: TGdiplusShutdown;
  GdipCreateBitmapFromHICON: TGdipCreateBitmapFromHICON;
  GdipCreateFromHDC: TGdipCreateFromHDC;
  GdipDrawImageRectI: TGdipDrawImageRectI;
  GdipDisposeImage: TGdipDisposeImage;
  GdipDeleteGraphics: TGdipDeleteGraphics;

function GdiPlusStretchDraw(hicn: hIcon; hCanvas: HDC; X, Y, cxWidth, cyHeight: Integer): Boolean;
function GdiPlusStretchDraw(himl: hImageList; ImageIndex: Integer; hCanvas: HDC; X, Y, cxWidth, cyHeight: Integer): Boolean;

implementation
uses
  CommCtrl;

var
  StartupInput: TGDIPlusStartupInput;
  gdiplusToken: ULONG;

function GdiPlusStretchDraw(hicn: hIcon; hCanvas: HDC; X, Y, cxWidth, cyHeight: Integer): Boolean;
var
  pIcon: GPIMAGE;
  pCanvas: GPGRAPHICS;
begin
  Result:= False;
  try
    GdipCreateBitmapFromHICON(hicn, pIcon);
    GdipCreateFromHDC(hCanvas, pCanvas);
    Result:= GdipDrawImageRectI(pCanvas, pIcon, X, Y, cxWidth, cyHeight) = Ok;
  finally
    GdipDisposeImage(pIcon);
    GdipDeleteGraphics(pCanvas);
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
      GdipCreateFromHDC:= TGdipCreateFromHDC(GetProcAddress(hLib, 'GdipCreateFromHDC'));
      GdipDrawImageRectI:= TGdipDrawImageRectI(GetProcAddress(hLib, 'GdipDrawImageRectI'));
      GdipDisposeImage:= TGdipDisposeImage(GetProcAddress(hLib, 'GdipDisposeImage'));
      GdipDeleteGraphics:= TGdipDeleteGraphics(GetProcAddress(hLib, 'GdipDeleteGraphics'));
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
      GdipCreateFromHDC:= nil;
      GdipDrawImageRectI:= nil;
      GdipDisposeImage:= nil;
      GdipDeleteGraphics:= nil;
      FreeLibrary(hLib);
    end;

end.

