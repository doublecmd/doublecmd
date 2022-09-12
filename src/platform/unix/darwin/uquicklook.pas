{
   Double Commander
   -------------------------------------------------------------------------
   Quick Look thumbnail provider

   Copyright (C) 2015-2019 Alexander Koblov (alexx2000@mail.ru)

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

unit uQuickLook;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, ExtCtrls,
  MacOSAll, CocoaAll, CocoaInt,  QuickLookUI;

type
  TQLPItem = objcclass(NSObject, QLPreviewItemProtocol)
    url: NSURL;
    function previewItemURL: NSURL;
  end;

type
  TDarwinQLControl = class
  private
    superpanel: TPanel;
    superview: NSView;
    view: QLPreviewView;
  private
    procedure setFilepath( filepath: String );
    procedure OnResize( Sender: TObject );
    function isOpened(): Boolean;
  public
    constructor Create( panel: TPanel );
    destructor Destroy; override;
    procedure open;
    procedure close;
    procedure resize;
  public
    property filepath: String write setFilepath;
    property opened: Boolean read isOpened;
end;


implementation

uses
  DynLibs, FileUtil, Types, Graphics,
  uThumbnails, uDebug, uClassesEx, uGraphics, uMyDarwin;

const
  libQuickLook = '/System/Library/Frameworks/QuickLook.framework/Versions/Current/QuickLook';

var
  QuickLook: TLibHandle = NilHandle;

var
  QLThumbnailImageCreate: function(allocator: CFAllocatorRef; url: CFURLRef; maxThumbnailSize: CGSize; options: CFDictionaryRef): CGImageRef; cdecl;

function GetThumbnail(const aFileName: String; aSize: TSize): Graphics.TBitmap;
var
  ImageRef: CGImageRef;
  WorkStream: TBlobStream;
  maxThumbnailSize: CGSize;
  ImageData: CFMutableDataRef;
  theFileNameUrlRef: CFURLRef;
  theFileNameCFRef: CFStringRef;
  Bitmap: TPortableNetworkGraphic;
  ImageDest: CGImageDestinationRef;
begin
  theFileNameCFRef:= CFStringCreateWithFileSystemRepresentation(nil, PAnsiChar(aFileName));
  theFileNameUrlRef:= CFURLCreateWithFileSystemPath(nil, theFileNameCFRef, kCFURLPOSIXPathStyle, False);
  try
    maxThumbnailSize.width:= aSize.cx; maxThumbnailSize.height:= aSize.cy;
    ImageRef:= QLThumbnailImageCreate(kCFAllocatorDefault, theFileNameUrlRef, maxThumbnailSize, nil);
    if ImageRef = nil then Exit(nil);

    ImageData:= CFDataCreateMutable(nil, 0);
    // Get image data in PNG format
    ImageDest:= CGImageDestinationCreateWithData(ImageData, kUTTypePNG, 1, nil);
    CGImageDestinationAddImage(ImageDest, ImageRef, nil);

    if (CGImageDestinationFinalize(ImageDest) = 0) then
      Result:= nil
    else begin
      Bitmap:= TPortableNetworkGraphic.Create;
      WorkStream:= TBlobStream.Create(CFDataGetBytePtr(ImageData), CFDataGetLength(ImageData));
      try
        Result:= TBitmap.Create;
        try
          Bitmap.LoadFromStream(WorkStream);
          BitmapAssign(Result, Bitmap);
        except
          FreeAndNil(Result);
        end;
      finally
        Bitmap.Free;
        WorkStream.Free;
      end;
    end;
    CFRelease(ImageRef);
    CFRelease(ImageData);
    CFRelease(ImageDest);
  finally
    CFRelease(theFileNameCFRef);
    CFRelease(theFileNameUrlRef);
  end;
end;

constructor TDarwinQLControl.Create( panel: TPanel );
begin
  inherited Create;
  superpanel:= panel;
  superview:= NSView(panel.Handle);
end;

destructor TDarwinQLControl.Destroy;
begin
  close();
  superview:= nil;
  superpanel:= nil;
  inherited Destroy;
end;

procedure TDarwinQLControl.open;
begin
  if opened then Exit;

  view:= QLPreviewView.alloc.init;
  view.setShouldCloseWithWindow(false);
  superview.addSubview( view );
  superpanel.OnResize:= @OnResize;
  resize;
end;

procedure TDarwinQLControl.close;
begin
  if not opened then Exit;

  superpanel.OnResize:= nil;
  view.close;
  view.removeFromSuperview;
  view.release;
  view:= nil;
end;

procedure TDarwinQLControl.resize;
var
  rect: NSRect;
begin
  if not opened then Exit;

  rect:= NSMakeRect( 0, 0, superpanel.Width, superpanel.Height );
  view.setFrame( rect );
end;

procedure TDarwinQLControl.OnResize( Sender: TObject );
begin
  resize;
end;

procedure TDarwinQLControl.setFilepath( filepath: String );
var
  item: TQLPItem;
begin
  if not opened then Exit;
  if filepath=EmptyStr then begin
    item:= nil;
  end else begin
    item:= TQLPItem.alloc.init;
    item.url:= NSURL.fileURLWithPath( StringToNSString(filepath) );
  end;
  view.setPreviewItem( item );
end;

function TDarwinQLControl.isOpened: Boolean;
begin
  Result:= Assigned(view);
end;

function TQLPItem.previewItemURL: NSURL;
begin
  Result:= url;
end;


procedure Initialize;
begin
  QuickLook:= LoadLibrary(libQuickLook);

  if (QuickLook <> NilHandle) then
  begin
    Pointer(QLThumbnailImageCreate):= GetProcAddress(QuickLook, 'QLThumbnailImageCreate');

    if Assigned(QLThumbnailImageCreate) then
    begin
      // Register thumbnail provider
      TThumbnailManager.RegisterProvider(@GetThumbnail);
      DCDebug('Initialize QuickLook: Success');
    end;
  end;
end;

procedure Finalize;
begin
  if (QuickLook <> NilHandle) then FreeLibrary(QuickLook);
end;

initialization
  Initialize;

finalization
  Finalize;

end.

