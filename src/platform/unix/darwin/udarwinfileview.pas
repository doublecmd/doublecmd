unit uDarwinFileView;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  SysUtils, Classes, Graphics,
  uiCloudDrive, uSearchResultFileSource, uFileSystemFileSource, uFileSource,
  uFile, uDisplayFile, uFileProperty,
  uFileView, uFileViewNotebook,
  uDarwinFinderModel, uDarwinFinder, uDarwinImage, uDarwinUtil,
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
      const searchName: String;
      const files: TStringArray;
      const newPage: Boolean );
    class procedure addiCloudDrivePage;
  end;

  { TDarwinSearchResultHandler }

  TDarwinSearchResultHandler = class
    procedure onSearchFinderTagComplete( const searchName: String; const files: TStringArray );
    procedure onSearchSavedSearchComplete( const searchName: String; const files: TStringArray );
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
  
  { TFinderSearchResultFileSource }

  TFinderSearchResultFileSource = class( TSearchResultFileSource )
  private
    _searchName: String;
    _icon: TBitmap;
  public
    constructor Create( searchName: String );
    destructor Destroy; override;
    function GetRootDir(sPath: String): String; override;
    function GetCustomIcon(const path: String; const iconSize: Integer
      ): TBitmap; override; overload;
  end;

{ TFinderTagSearchResultFileSource }

constructor TFinderSearchResultFileSource.Create(searchName: String);
var
  tag: TFinderTag;
  image: NSImage;
begin
  Inherited Create;
  _searchName:= searchName;
  tag:= TFinderTags.getTagOfName( StringToNSString(_searchName) );
  if NOT Assigned(tag) then
    Exit;

  image:= TDarwinFinderUtil.createMenuRoundImage(
    tag.colorIndex,
    gIconsInMenusSize,
    gIconsInMenusSize div 2 + 4 );

  _icon:= TDarwinImageUtil.toBitmap(image);

  image.release;
end;

destructor TFinderSearchResultFileSource.Destroy;
begin
  FreeAndNil( _icon );
  inherited Destroy;
end;

function TFinderSearchResultFileSource.GetRootDir(sPath: String): String;
begin
  Result:= PathDelim + PathDelim + PathDelim + rsSearchResult + ': ' + _searchName + PathDelim;
end;

function TFinderSearchResultFileSource.GetCustomIcon(
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

{ TDarwinSearchResultHandler }

procedure TDarwinSearchResultHandler.onSearchFinderTagComplete(const searchName: String;
  const files: TStringArray);
begin
  TDarwinFileViewUtil.addFinderSearchResult( searchName, files, True );
end;

procedure TDarwinSearchResultHandler.onSearchSavedSearchComplete(
  const searchName: String; const files: TStringArray);
begin
  TDarwinFileViewUtil.addFinderSearchResult( searchName, files, False );
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
  const searchName: String;
  const files: TStringArray;
  const newPage: Boolean );
var
  i: integer;
  count: Integer;
  sFileName: string;
  SearchResultFS: ISearchResultFileSource;
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

  // Create search result file source.
  // Currently only searching FileSystem is supported.
  SearchResultFS := TFinderSearchResultFileSource.Create( searchName );
  SearchResultFS.AddList(FileList, fileView.FileSource );

  if newPage then begin
    page:= Notebook.NewPage(fileView);
    page.MakeActive;
    fileView:= page.FileView;
  end;

  fileView.AddFileSource(SearchResultFS, SearchResultFS.GetRootDir);
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

