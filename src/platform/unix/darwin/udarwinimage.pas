unit uDarwinImage;

{$mode delphi}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll;

type
  
  { TDarwinImageUtil }

  TDarwinImageUtil = class
  public
    class function filt( const filterName: NSString; const sourceImage: NSImage ): NSImage;
    class function invertColor( const sourceImage: NSImage ): NSImage;
    class function toPNGData( const data: NSData ): NSData;
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

end.

