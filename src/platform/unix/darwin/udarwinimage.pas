unit uDarwinImage;

{$mode delphi}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  Graphics,
  uClassesEx,
  uDarwinUtil,
  CocoaAll;

type
  
  { TDarwinImageUtil }

  TDarwinImageUtil = class
  public
    class function filt( const filterName: NSString; const sourceImage: NSImage ): NSImage;
    class function invertColor( const sourceImage: NSImage ): NSImage;
    class function toPNGData( const data: NSData ): NSData;
    class function toBitmap( const image: NSImage ): TBitmap;
    class function getBestWithSize(
      const srcImage: NSImage;
      const size:Integer ): NSImage;
    class function getBestFromFileWithSize(
      const path: String;
      const size:Integer ): NSImage;
  end;

implementation

class function TDarwinImageUtil.filt(
  const filterName: NSString;
  const sourceImage: NSImage ): NSImage;
var
  tiffData: NSData;
  inputImage: CIImage;
  outputImage: CIImage;
  filter: CIFilter;
  rep: NSCIImageRep;
begin
  tiffData:= sourceImage.TIFFRepresentation;
  inputImage:= CIImage.imageWithData( tiffData );
  filter:= CIFilter.filterWithName( filterName );
  filter.setValue_forKey( inputImage, kCIInputImageKey );
  outputImage:= filter.valueForKey( kCIOutputImageKey );
  rep:= NSCIImageRep.imageRepWithCIImage( outputImage );
  Result:= NSImage.alloc.initWithSize( sourceImage.size );
  Result.addRepresentation( rep );
  Result.autorelease;
end;

class function TDarwinImageUtil.invertColor( const sourceImage: NSImage ): NSImage;
begin
  Result:= filt( NSSTR('CIColorInvert'), sourceImage );
end;

class function TDarwinImageUtil.toPNGData(const data: NSData): NSData;
var
  bitmapRep: NSBitmapImageRep;
begin
  bitmapRep:= NSBitmapImageRep.imageRepWithData( data );
  Result:= bitmapRep.representationUsingType_properties( NSPNGFileType, nil );
end;

class function TDarwinImageUtil.toBitmap( const image: NSImage ): TBitmap;
var
  nsbitmap: NSBitmapImageRep;
  tempData: NSData;
  tempStream: TBlobStream = nil;
  tempBitmap: TPortableNetworkGraphic = nil;
  bitmap: TBitmap;
begin
  Result:= nil;
  if image=nil then exit;

  try
    nsbitmap:= NSBitmapImageRep.imageRepWithData( image.TIFFRepresentation );
    tempData:= nsbitmap.representationUsingType_properties( NSPNGFileType, nil );
    tempStream:= TBlobStream.Create( tempData.Bytes, tempData.Length );
    tempBitmap:= TPortableNetworkGraphic.Create;
    tempBitmap.LoadFromStream( tempStream );
    bitmap:= TBitmap.Create;
    bitmap.Assign( tempBitmap );
    Result:= bitmap;
  finally
    FreeAndNil(tempBitmap);
    FreeAndNil(tempStream);
  end;
end;

class function TDarwinImageUtil.getBestWithSize(
  const srcImage: NSImage;
  const size:Integer ): NSImage;
var
  bestRect: NSRect;
  bestImageRep: NSImageRep;
  bestImage: NSImage;
begin
  Result := nil;
  if srcImage = nil then
    exit;

  bestRect.origin.x := 0;
  bestRect.origin.y := 0;
  bestRect.size.width := size;
  bestRect.size.height := size;
  bestImageRep:= srcImage.bestRepresentationForRect_context_hints( bestRect, nil, nil );

  bestImage:= NSImage.Alloc.InitWithSize( bestImageRep.size );
  bestImage.autorelease;
  bestImage.AddRepresentation( bestImageRep );

  Result := bestImage;
end;

class function TDarwinImageUtil.getBestFromFileWithSize(
  const path: String;
  const size: Integer ): NSImage;
var
  srcImage: NSImage;
begin
  srcImage:= NSImage.Alloc.initByReferencingFile( StringToNSString(path) );
  Result:= TDarwinImageUtil.getBestWithSize( srcImage, size );
  srcImage.release;
end;

end.

