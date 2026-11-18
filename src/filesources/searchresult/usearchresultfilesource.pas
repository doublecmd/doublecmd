unit uSearchResultFileSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics,
  uFileSource, uFileSourceManager, uFlatViewFileSource,
  DCStrUtils
  {$IFDEF DARWIN}
  , uDarwinFile, uDarwinImage, uDCUtils
  {$ENDIF}
  ;

type

  ISearchResultFileSource = interface(IFlatViewFileSource)
    ['{5076D4C2-3AB8-4029-9318-0AF115F7FDDD}']
  end;

  {en
     File source for search results.
  }

  { TSearchResultFileSource }

  TSearchResultFileSource = class(TFlatViewFileSource, ISearchResultFileSource)
  protected
    _searchKeyword: String;
    _searchPath: String;
    _searchMask: String;
  public
    constructor Create( const keyword: String; const path: String; const mask: String );

    function GetDescriber: TFileSourceDescriber; override;

    function GetCustomIcon(const path: String; const iconSize: Integer): TBitmap; override; overload;
  public
    property searchKeyword: String read _searchKeyword;
    property searchPath: String read _searchPath;
    property searchMask: String read _searchMask;
  end;

implementation

uses
  uFileSystemFileSource, uLng;

const
  SEARCH_SCHEME = 'search://';

type

  { TSearchResultFileSourceDescriber }

  TSearchResultFileSourceDescriber = class( TFileSourceDescriber )
    function tabCaption(const fs: IFileSource; const path: String): String; override;
    function tabHint(const fs: IFileSource; const path: String): String; override;
  end;

var
  searchResultFileSourceDescriber: TFileSourceDescriber;

function TSearchResultFileSourceDescriber.tabCaption(
  const fs: IFileSource;
  const path: String): String;
var
  keyword: String;
begin
  keyword:= (fs as TSearchResultFileSource).searchKeyword;
  if keyword <> EmptyStr then
    Result:= rsSearchResult + ': ' + keyword
  else
    Result:= rsSearchResult;
end;

function TSearchResultFileSourceDescriber.tabHint(
  const fs: IFileSource;
  const path: String): String;
var
  searchFS: TSearchResultFileSource;
  searchKeyword: String;
  searchPath: String;
  searchMask: String;
begin
  searchFS:= fs as TSearchResultFileSource;
  searchKeyword:= searchFS.searchKeyword;
  searchPath:= searchFS.searchPath;
  searchMask:= searchFS.searchMask;

  if (searchKeyword<>EmptyStr) and (searchPath<>EmptyStr) then
    Result:= rsSearchResult + ': ' + searchKeyword + ' @ ' + searchPath
  else if (searchKeyword<>EmptyStr) or (searchPath<>EmptyStr) then
    Result:= rsSearchResult + ': ' + searchKeyword + searchPath
  else
    Result:= rsSearchResult;
  if searchMask <> EmptyStr then
    Result:= Result + ' > ' + searchMask;
end;

constructor TSearchResultFileSource.Create(
  const keyword: String;
  const path: String;
  const mask: String );
begin
  inherited Create( path );
  FCurrentAddress:= SEARCH_SCHEME;
  _searchKeyword:= keyword;
  _searchPath:= ExcludeBackPathDelimiter( path );
  _searchMask:= mask;
end;

function TSearchResultFileSource.GetDescriber: TFileSourceDescriber;
begin
  Result:= searchResultFileSourceDescriber;
end;

function TSearchResultFileSource.GetCustomIcon(
  const path: String;
  const iconSize: Integer ): TBitmap;
{$IFDEF DARWIN}
const
  ICON_PATH = '$COMMANDER_PATH/pixmaps/macOS/magnifyingglass.png';
var
  iconPath: String;
begin
  iconPath:= mbExpandFileName( ICON_PATH );
  Result:= darwinImageCacheForPath.copyBitmapForFileContent( iconPath, iconSize, True );
end;
{$ELSE}
begin
  Result:= nil;
end;
{$ENDIF}

initialization
  searchResultFileSourceDescriber:= TSearchResultFileSourceDescriber.Create;

finalization
  FreeAndNil( searchResultFileSourceDescriber );

end.

