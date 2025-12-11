unit uDarwinFileView;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  SysUtils, Classes,
  uiCloudDrive, uSearchResultFileSource, uFileSystemFileSource, uFileSource,
  uFile, uDisplayFile, uFileProperty,
  uFileView, uColumnsFileView, uFileViewNotebook,
  uDarwinFinderModel,
  ulng,
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
    class procedure addFinderSearchResultPage( const searchName: String; const files: TStringArray );
    class procedure addiCloudDrivePage;
  end;

  TDarwinSearchResultHandler = class
    procedure onSearchFinderTagComplete( const searchName: String; const files: TStringArray );
  end;

  TDarwinFileViewDrawHandler = class
    procedure onDrawCell(Sender: TFileView; aCol, aRow: Integer;
      aRect: TRect; focused: Boolean; aFile: TDisplayFile);
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
  public
    constructor Create( searchName: String );
    function GetRootDir(sPath: String): String; override;
  end;

{ TFinderTagSearchResultFileSource }

constructor TFinderSearchResultFileSource.Create(searchName: String);
begin
  Inherited Create;
  _searchName:= searchName;
end;

function TFinderSearchResultFileSource.GetRootDir(sPath: String): String;
begin
  Result:= PathDelim + PathDelim + PathDelim + rsSearchResult + ': ' + _searchName + PathDelim;
end;

{ TDarwinSearchResultHandler }

procedure TDarwinSearchResultHandler.onSearchFinderTagComplete(const searchName: String;
  const files: TStringArray);
begin
  TDarwinFileViewUtil.addFinderSearchResultPage( searchName, files );
end;

{ TDarwinFileViewDrawHandler }

procedure TDarwinFileViewDrawHandler.onDrawCell(Sender: TFileView; aCol, aRow: Integer;
  aRect: TRect; focused: Boolean; aFile: TDisplayFile);
var
  macOSProperty: TFileMacOSSpecificProperty;
begin
  if (Sender is TColumnsFileView) and (aCol<>0) then
    Exit;

  macOSProperty:= aFile.FSFile.MacOSSpecificProperty;
  if macOSProperty = nil then
    Exit;

  drawTagsAsDecoration( macOSProperty.FinderTagPrimaryColors, aRect, focused );
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
    if focused then
      NSColor.alternateSelectedControlTextColor.set_
    else
      NSColor.textBackgroundColor.set_;
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

class procedure TDarwinFileViewUtil.addFinderSearchResultPage( const searchName: String; const files: TStringArray);
var
  i: integer;
  count: Integer;
  sFileName: string;
  SearchResultFS: ISearchResultFileSource;
  FileList: TFileTree;
  aFile: TFile;
  Notebook: TFileViewNotebook;
  NewPage: TFileViewPage;
begin
  count:= Length(files);
  FileList := TFileTree.Create;
  for i:=0 to count-1 do begin
    sFileName := files[i];
    aFile := TFileSystemFileSource.CreateFileFromFile(sFileName);
    FileList.AddSubNode(aFile);
  end;

  // Add new tab for search results.
  Notebook := _activeNoteBookFunc();
  NewPage := Notebook.NewPage(Notebook.ActiveView);

  // Create search result file source.
  // Currently only searching FileSystem is supported.
  SearchResultFS := TFinderSearchResultFileSource.Create( searchName );
  SearchResultFS.AddList(FileList, Notebook.ActiveView.FileSource);

  NewPage.FileView.AddFileSource(SearchResultFS, SearchResultFS.GetRootDir);
  NewPage.FileView.FlatView := True;
  NewPage.MakeActive;
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

