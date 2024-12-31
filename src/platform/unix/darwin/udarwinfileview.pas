unit uDarwinFileView;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  uiCloudDriver, uSearchResultFileSource, uFile, uFileSystemFileSource, uFileSource,
  fMain, uFileViewNotebook, ulng;

type
  uDarwinFileViewUtil = class
    class procedure addSearchTagResultPage( const searchName: String; const files: TStringArray );
    class procedure addiCloudDriverPage;
  end;

implementation

type
  
  { TFinderTagSearchResultFileSource }

  TFinderTagSearchResultFileSource = class( TSearchResultFileSource )
  private
    _tagName: String;
  public
    constructor Create( tagName: String );
    function GetRootDir(sPath: String): String; override;
  end;

{ TFinderTagSearchResultFileSource }

constructor TFinderTagSearchResultFileSource.Create(tagName: String);
begin
  Inherited Create;
  _tagName:= tagName;
end;

function TFinderTagSearchResultFileSource.GetRootDir(sPath: String): String;
begin
  Result:= PathDelim + PathDelim + PathDelim + rsSearchResult + ': ' + _tagName + PathDelim;
end;

{ uDarwinFileViewUtil }

class procedure uDarwinFileViewUtil.addSearchTagResultPage( const searchName: String; const files: TStringArray);
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
  Notebook := frmMain.ActiveNotebook;
  NewPage := Notebook.NewPage(Notebook.ActiveView);

  // Create search result file source.
  // Currently only searching FileSystem is supported.
  SearchResultFS := TFinderTagSearchResultFileSource.Create( searchName );
  SearchResultFS.AddList(FileList, Notebook.ActiveView.FileSource);

  NewPage.FileView.AddFileSource(SearchResultFS, SearchResultFS.GetRootDir);
  NewPage.FileView.FlatView := True;
  NewPage.MakeActive;
end;

class procedure uDarwinFileViewUtil.addiCloudDriverPage;
var
  iCloudFS: TiCloudDriverFileSource;
  Notebook: TFileViewNotebook;
  NewPage: TFileViewPage;
begin
  Notebook := frmMain.ActiveNotebook;
  NewPage := Notebook.NewPage(Notebook.ActiveView);

  iCloudFS := TiCloudDriverFileSource.Create;
  iCloudFS.mountAppPoint( 'com~apple~Pages' );
  iCloudFS.mountAppPoint( 'com~apple~Numbers' );
  iCloudFS.mountAppPoint( 'com~apple~Keynote' );
  iCloudFS.mountAppPoint( 'iCloud~com~apple~Playgrounds' );
  iCloudFS.mountAppPoint( 'com~apple~ScriptEditor2' );
  iCloudFS.mount( '~/Library/Mobile Documents/com~apple~CloudDocs', '/' );

  NewPage.FileView.AddFileSource(iCloudFS, iCloudFS.GetRootDir);
  NewPage.MakeActive;
end;

end.

