unit uDarwinFileProperty;

{$mode delphi}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  CocoaAll,
  uFileProperty, uDarwinFinderModel, uDarwinUtil;

type

  { TDarwinFilePropertyUtil }

  TDarwinFilePropertyUtil = class
    class function getSpecificProperty( const path: String ): TFileMacOSSpecificProperty;
  end;

implementation

class function TDarwinFilePropertyUtil.getSpecificProperty(const path: String
  ): TFileMacOSSpecificProperty;
var
  url: NSURL;

  function toPrimaryColors(const tagNames: NSArray): TFileFinderTagPrimaryColors;
  var
    visualTagNames: NSMutableArray;
    tagName: NSString;
    tag: TFinderTag;
    iSource: NSUInteger;
    iDest: Integer;
    colorIndex: Integer;
  begin
    visualTagNames:= NSMutableArray.new;
    for iSource:= 0 to tagNames.count-1 do begin
      tagName:= NSString( tagNames.objectAtIndex(iSource) );
      tag:= TFinderTags.getTagOfName( tagName );
      if tag.colorIndex <= 0 then
        continue;
      visualTagNames.addObject( tagName );
    end;

    iSource:= 0;
    if visualTagNames.count > 3 then
      iSource:= visualTagNames.count - 3;
    for iDest:=0 to 2 do begin
      colorIndex:= -1;
      if iSource < visualTagNames.count then begin
        tagName:= NSString( visualTagNames.objectAtIndex(iSource) );
        tag:= TFinderTags.getTagOfName( tagName );
        colorIndex:= tag.colorIndex;
      end;
      Result.indexes[iDest]:= colorIndex;
      inc( iSource );
    end;

    visualTagNames.release;
  end;

  function getTagPrimaryColors: TFileFinderTagPrimaryColors;
  var
    tagNames: NSArray;
  begin
    Result.intValue:= -1;
    tagNames:= TDarwinFinderModelUtil.getTagNamesOfFile( url );
    if tagNames = nil then
      Exit;
    Result:= toPrimaryColors( tagNames );
  end;

  function isSeedFile: Boolean;
  var
    name: NSString;
    status: NSString;
    error: NSError = nil;
    ok: Boolean;
  begin
    name:= url.lastPathComponent;
    if name.isEqualToString(NSSTR('..')) then
      Exit( False );
    if name.hasPrefix(NSSTR('.')) and name.hasSuffix(NSSTR('.icloud')) then
      Exit( True );

    ok:= url.getResourceValue_forKey_error( @status, NSURLUbiquitousItemDownloadingStatusKey, @error );
    if NOT ok then
      logDarwinError( 'TDarwinFileUtil.getSpecificProperty.isSeedFile()', error );
    if status = nil then
      Exit( False );

    Result:= NOT status.isEqualToString( NSURLUbiquitousItemDownloadingStatusCurrent );
  end;

begin
  Result:= TFileMacOSSpecificProperty.Create;
  url:= NSURL.fileURLWithPath( StringToNSString(path) );
  Result.FinderTagPrimaryColors:= getTagPrimaryColors;
  Result.IsiCloudSeedFile:= isSeedFile;
end;

end.

