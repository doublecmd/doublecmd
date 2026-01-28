unit uDarwinImage;

{$mode delphi}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils, Contnrs, syncobjs,
  Graphics,
  uClassesEx, DCStrUtils,
  uDarwinUtil, uDarwinFile,
  CocoaAll, CocoaThemes;

type
  
  { TDarwinImageUtil }

  TDarwinImageUtil = class
  public
    class function filt( const filterName: NSString; const sourceImage: NSImage ): NSImage;
    class function invertColor( const sourceImage: NSImage ): NSImage;
    class function toPNGData( const data: NSData ): NSData;
    class function toBitmap( const image: NSImage ): TBitmap;
    class function toNSImage( const bitmap: TBitmap ): NSImage;
    class function getBestWithSize(
      const srcImage: NSImage;
      const size:Integer ): NSImage;
    class function getBestFromFileContentWithSize(
      const path: String;
      const size: Integer;
      const autoDark: Boolean = False ): NSImage;
    class function getBitmapForExt(
      const ext: String;
      const size: Integer ): TBitmap;
    class function getFileIconWithSize(
      const path: String;
      const size: Integer ): NSImage;
  end;

  // a pure image cache is needed, but TPixMapManager in DC contains too many
  // features and is too complex.

  { TDarwinImageCacheManager }

  TDarwinImageCacheManager = class( ICocoaThemeObserver )
  private
    _lockObject: TCriticalSection;
    _images: TFPObjectHashTable;
  public
    constructor Create;
    destructor Destroy; override;

    procedure onThemeChanged;

    function copyBitmapForFileExt(
      const path: String;
      const size: Integer ): TBitmap;

    function copyBitmapForFileContent(
      const path: String;
      const size: Integer;
      const autoDark: Boolean = False ): TBitmap;

    function copyBitmapForNSImage(
      const key: String;
      const image: NSImage ): TBitmap;

    function getNSImageForFileContent(
      const path: String;
      const size: Integer;
      const autoDark: Boolean = False ): NSImage;
  end;

var
  darwinImageCacheForPath: TDarwinImageCacheManager;
  darwinImageCacheForExt: TDarwinImageCacheManager;

implementation

type
  
  { TNSImageCacheItem }

  TNSImageCacheItem = class
  private
    _image: NSImage;
  public
    constructor Create( const image: NSImage );
    destructor Destroy; override;
  end;

{ TNSImageCacheItem }

constructor TNSImageCacheItem.Create( const image: NSImage );
begin
  _image:= image;
  _image.retain;
end;

destructor TNSImageCacheItem.Destroy;
begin
  _image.release;
end;

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
  if image = nil then
    Exit;

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

class function TDarwinImageUtil.toNSImage(const bitmap: TBitmap): NSImage;
var
  tempBitmap: TPortableNetworkGraphic;
  tempStream: TMemoryStream;
  tempData: NSData;
begin
  Result:= nil;
  if bitmap = nil then
    Exit;

  try
    tempBitmap:= TPortableNetworkGraphic.Create;
    tempBitmap.Assign( bitmap );
    tempStream:= TMemoryStream.Create;
    tempBitmap.SaveToStream( tempStream );
    tempData:= NSData.dataWithBytes_length( tempStream.Memory, tempStream.Size );
    Result:= NSImage.alloc.initWithData( tempData );
    Result.autorelease;
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

  bestImage:= NSImage.Alloc.InitWithSize( bestRect.size );
  bestImage.autorelease;
  bestImage.AddRepresentation( bestImageRep );

  Result := bestImage;
end;

class function TDarwinImageUtil.getBestFromFileContentWithSize(
  const path: String;
  const size: Integer;
  const autoDark: Boolean = False ): NSImage;
var
  image: NSImage;
begin
  image:= NSImage.alloc.initWithContentsOfFile( StringToNSString(path) );
  image.autorelease;
  if autoDark then begin
    if TCocoaThemeServices.isDark then
      image:= TDarwinImageUtil.invertColor( image );
  end;
  Result:= TDarwinImageUtil.getBestWithSize( image, size );
end;

class function TDarwinImageUtil.getBitmapForExt(
  const ext: String;
  const size: Integer ): TBitmap;
var
  image: NSImage;
begin
  image:= TDarwinFileUtil.getIconForExt( ext );
  image:= TDarwinImageUtil.getBestWithSize( image, size );
  Result:= TDarwinImageUtil.toBitmap( image );
end;

class function TDarwinImageUtil.getFileIconWithSize(
  const path: String;
  const size: Integer ): NSImage;
begin
  Result:= TDarwinFileUtil.getIconForFile( path );
  Result.setSize( NSMakeSize(size,size) );
end;

{ TDarwinImageCacheManager }

constructor TDarwinImageCacheManager.Create;
begin
  _lockObject:= TCriticalSection.Create;;
  _images:= TFPObjectHashTable.Create;
end;

destructor TDarwinImageCacheManager.Destroy;
begin
  FreeAndNil( _images );
  FreeAndNil( _lockObject );
end;

procedure TDarwinImageCacheManager.onThemeChanged;
begin
  _lockObject.Acquire;
  try
    _images.Clear;
  finally
    _lockObject.Release;
  end;
end;

function TDarwinImageCacheManager.copyBitmapForFileExt(
  const path: String;
  const size: Integer ): TBitmap;
var
  ext: String;
  bitmap: TBitmap;
begin
  Result:= nil;
  if path.LastIndexOf('.') > 0 then
    ext:= ExtractOnlyFileExt( path )
  else
    ext:= path;
  if ext.IsEmpty then
    Exit;

  _lockObject.Acquire;
  try
    bitmap:= TBitmap(_images[ext]);
    if _images[ext] = nil then begin
      bitmap:= TDarwinImageUtil.getBitmapForExt( ext, size );
      _images[ext]:= bitmap;
    end;
  finally
    _lockObject.Release;
  end;

  if Assigned(bitmap) then begin
    Result:= TBitmap.Create;
    Result.Assign( bitmap );
  end;
end;

function TDarwinImageCacheManager.copyBitmapForFileContent(
  const path: String;
  const size: Integer;
  const autoDark: Boolean = False ): TBitmap;
var
  image: NSImage;
  bitmap: TBitmap;
begin
  Result:= nil;

  _lockObject.Acquire;
  try
    bitmap:= TBitmap(_images[path]);
    if _images[path] = nil then begin
      image:= TDarwinImageUtil.getBestFromFileContentWithSize( path, size, autoDark );
      bitmap:= TDarwinImageUtil.toBitmap( image );
      _images[path]:= bitmap;
    end;
  finally
    _lockObject.Release;
  end;

  if Assigned(bitmap) then begin
    Result:= TBitmap.Create;
    Result.Assign( bitmap );
  end;
end;

function TDarwinImageCacheManager.copyBitmapForNSImage(
  const key: String;
  const image: NSImage ): TBitmap;
var
  bitmap: TBitmap;
begin
  Result:= nil;

  _lockObject.Acquire;
  try
    bitmap:= TBitmap(_images[key]);
    if _images[key] = nil then begin
      bitmap:= TDarwinImageUtil.toBitmap( image );
      _images[key]:= bitmap;
    end;
  finally
    _lockObject.Release;
  end;

  if Assigned(bitmap) then begin
    Result:= TBitmap.Create;
    Result.Assign( bitmap );
  end;
end;

function TDarwinImageCacheManager.getNSImageForFileContent(
  const path: String;
  const size: Integer;
  const autoDark: Boolean = False ): NSImage;
var
  item: TNSImageCacheItem;
  image: NSImage;
begin
  Result:= nil;

  _lockObject.Acquire;
  try
    item:= TNSImageCacheItem(_images[path]);
    if _images[path] = nil then begin
      image:= TDarwinImageUtil.getBestFromFileContentWithSize( path, size, autoDark );
      item:= TNSImageCacheItem.Create( image );
      _images[path]:= item;
    end;
  finally
    _lockObject.Release;
  end;

  if Assigned( item ) then
    Result:= item._image;
end;

initialization
  darwinImageCacheForPath:= TDarwinImageCacheManager.Create;
  TCocoaThemeServices.addObserver( darwinImageCacheForPath );
  darwinImageCacheForExt:= TDarwinImageCacheManager.Create;
  TCocoaThemeServices.addObserver( darwinImageCacheForExt );

finalization
  TCocoaThemeServices.removeObserver( darwinImageCacheForPath );
  FreeAndNil( darwinImageCacheForPath );
  TCocoaThemeServices.removeObserver( darwinImageCacheForExt );
  FreeAndNil( darwinImageCacheForExt );

end.

