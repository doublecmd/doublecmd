unit uDarwinFileView;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  SysUtils, Classes, Graphics,
  uiCloudDrive, uSearchResultFileSource, uFileSystemFileSource, uFileSource,
  uFile, uDisplayFile, uFileProperty,
  uFileView, uFileViewNotebook,
  uDarwinFinderModel, uDarwinFinder, uDarwinImage, uDarwinFile,
  ulng, uGlobs,
  MacOSAll, CocoaAll;

type

  TActvieNoteBookFunc = function (): TFileViewNotebook of object;
  TActiveFrameFunc = function (): TFileView of object;

  { TDarwinFileViewUtil }

  TDarwinFileViewUtil = class
  private
    class var _activeNoteBookFunc: TActvieNoteBookFunc;
    class var _activeFrameFunc: TActiveFrameFunc;
  public
    class procedure init(
      const activeNoteBookFunc: TActvieNoteBookFunc;
      const activeFrameFunc: TActiveFrameFunc );
    class procedure addFinderSearchResult(
      const fs: ISearchResultFileSource;
      const files: TStringArray;
      const newPage: Boolean );
    class procedure addiCloudDrivePage;
  end;

  { TDarwinSearchResultHandler }

  TDarwinSearchResultHandler = class
    procedure onSearchFinderTagComplete( const info: NSObject; const files: TStringArray );
    procedure onSearchSavedSearchComplete( const info: NSObject; const files: TStringArray );
  end;

  TDarwinFileViewDrawHandler = class
    procedure onDrawCell( var params: TFileSourceUIParams );
    procedure drawTagsAsDecoration(
      const colors: TFileFinderTagPrimaryColors; const drawRect: TRect; const focused: Boolean );
  end;

var
  darwinSearchResultHandler: TDarwinSearchResultHandler;
  darwinFileViewDrawHandler: TDarwinFileViewDrawHandler;

implementation

type
  
  { TFinderTagSearchResultFileSource }

  TFinderTagSearchResultFileSource = class( TSearchResultFileSource )
  private
    _searchName: String;
    _icon: TBitmap;
  public
    constructor Create( tagNames: NSArray );
    destructor Destroy; override;
    function GetRootDir(sPath: String): String; override;
    function GetCustomIcon(const path: String; const iconSize: Integer
      ): TBitmap; override; overload;
  end;

  { TSmartFolderSearchResultFileSource }

  TSmartFolderSearchResultFileSource = class( TSearchResultFileSource )
  public
    function GetRootDir(sPath: String): String; override;
    function GetCustomIcon(const path: String; const iconSize: Integer
      ): TBitmap; override; overload;
  end;

{ TFinderTagSearchResultFileSource }

constructor TFinderTagSearchResultFileSource.Create(tagNames: NSArray);

  function toString: NSString;
  var
    tagName: NSString;
    name: NSMutableString;
  begin
    name:= NSMutableString.new;
    for tagName in tagNames do begin
      name.appendString( tagName );
      name.appendString( NSSTR('|') );
    end;
    Result:= name.substringToIndex( name.length-1 );
    name.release;
  end;

var
  tag: TFinderTag;
  image: NSImage;
begin
  _searchName:= toString.UTF8String;
  Inherited Create( _searchName );
  tag:= TFinderTags.getTagOfName( NSString(tagNames.firstObject) );
  if NOT Assigned(tag) then
    Exit;

  image:= TDarwinFinderUtil.createMenuRoundImage(
    tag.colorIndex,
    gIconsInMenusSize,
    gIconsInMenusSize div 2 + 4 );

  _icon:= TDarwinImageUtil.toBitmap(image);

  image.release;
end;

destructor TFinderTagSearchResultFileSource.Destroy;
begin
  FreeAndNil( _icon );
  inherited Destroy;
end;

function TFinderTagSearchResultFileSource.GetRootDir(sPath: String): String;
begin
  Result:= PathDelim + PathDelim + PathDelim + rsSearchResult + ': ' + _searchName + PathDelim;
end;

function TFinderTagSearchResultFileSource.GetCustomIcon(
  const path: String;
  const iconSize: Integer ): TBitmap;
begin
  if Assigned(_icon) then begin
    Result:= TBitmap.Create;
    Result.Assign( _icon );
  end else begin
    Result:= Inherited;
  end;
end;

{ TSmartFolderSearchResultFileSource }

function TSmartFolderSearchResultFileSource.GetRootDir(sPath: String): String;
begin
  Result:= PathDelim + PathDelim + PathDelim + rsSearchResult + ': ' + _displayName + PathDelim;
end;

function TSmartFolderSearchResultFileSource.GetCustomIcon(
  const path: String;
  const iconSize: Integer ): TBitmap;
begin
  Result:= darwinImageCacheForExt.copyIconForFileExt( 'savedSearch', iconSize );
end;

{ TDarwinSearchResultHandler }

procedure TDarwinSearchResultHandler.onSearchFinderTagComplete(
  const info: NSObject;
  const files: TStringArray );
var
  fs: ISearchResultFileSource;
begin
  fs:= TFinderTagSearchResultFileSource.Create( NSArray(info) );
  TDarwinFileViewUtil.addFinderSearchResult( fs, files, True );
end;

procedure TDarwinSearchResultHandler.onSearchSavedSearchComplete(
  const info: NSObject;
  const files: TStringArray );
var
  fs: ISearchResultFileSource;
begin
  fs:= TSmartFolderSearchResultFileSource.Create( NSString(info).UTF8String );
  TDarwinFileViewUtil.addFinderSearchResult( fs, files, False );
end;

{ TDarwinFileViewDrawHandler }

procedure TDarwinFileViewDrawHandler.onDrawCell( var params: TFileSourceUIParams );
var
  macOSProperty: TFileMacOSSpecificProperty;
begin
  if params.multiColumns AND (params.col<>0) then
    Exit;

  macOSProperty:= params.displayFile.FSFile.MacOSSpecificProperty;
  if macOSProperty = nil then
    Exit;

  drawTagsAsDecoration( macOSProperty.FinderTagPrimaryColors, params.decorationRect, params.focused );
end;

procedure TDarwinFileViewDrawHandler.drawTagsAsDecoration(
  const colors: TFileFinderTagPrimaryColors; const drawRect: TRect;
  const focused: Boolean);
var
  i: Integer;
  colorIndex: Integer;
  color: NSColor;
  tagRect: NSRect;
  path: NSBezierPath;
begin
  tagRect.size.width:= 11;
  tagRect.size.height:= 11;
  tagRect.origin.x:= drawRect.Right - 17;
  tagRect.origin.y:= drawRect.Top + (drawRect.Height-tagRect.size.height)/2;

  for i:=0 to 2 do begin
    colorIndex:= colors.indexes[i];
    if colorIndex < 0 then
      break;
    color:= TDarwinFinderModelUtil.decorationFinderTagNSColors[colorIndex];
    color.set_;
    path:= NSBezierPath.bezierPathWithOvalInRect( tagRect );
    path.fill;
    NSColor.alternateSelectedControlTextColor.set_;
    path.stroke;
    tagRect.origin.x:= tagRect.origin.x - 5;
  end;
end;

{ TDarwinFileViewUtil }

class procedure TDarwinFileViewUtil.init(
  const activeNoteBookFunc: TActvieNoteBookFunc;
  const activeFrameFunc: TActiveFrameFunc);
begin
  _activeNoteBookFunc:= activeNoteBookFunc;
  _activeFrameFunc:= activeFrameFunc;
end;

class procedure TDarwinFileViewUtil.addFinderSearchResult(
  const fs: ISearchResultFileSource;
  const files: TStringArray;
  const newPage: Boolean );
var
  i: integer;
  count: Integer;
  sFileName: string;
  FileList: TFileTree;
  aFile: TFile;
  Notebook: TFileViewNotebook;
  fileView: TFileView;
  page: TFileViewPage;
begin
  count:= Length(files);
  FileList := TFileTree.Create;
  for i:=0 to count-1 do begin
    sFileName := files[i];
    aFile := TFileSystemFileSource.CreateFileFromFile(sFileName);
    FileList.AddSubNode(aFile);
  end;

  Notebook := _activeNoteBookFunc();
  fileView:= Notebook.ActiveView;

  fs.AddList(FileList, fileView.FileSource );

  if newPage then begin
    page:= Notebook.NewPage(fileView);
    page.MakeActive;
    fileView:= page.FileView;
  end;

  fileView.AddFileSource(fs, fs.GetRootDir);
  fileView.FlatView := True;
end;

class procedure TDarwinFileViewUtil.addiCloudDrivePage;
var
  iCloudFS: TiCloudDriveFileSource;
begin
  iCloudFS := TiCloudDriveFileSource.GetFileSource;
  _activeFrameFunc().AddFileSource(iCloudFS, iCloudFS.GetRootDir);
  _activeFrameFunc().SetFocus;
end;

initialization
  darwinSearchResultHandler:= TDarwinSearchResultHandler.Create;
  darwinFileViewDrawHandler:= TDarwinFileViewDrawHandler.Create;

finalization
  FreeAndNil( darwinSearchResultHandler );

end.

