unit uSearchResultUtil;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  uSearchResultFileSource, uFile, uFileSystemFileSource, uFileSource,
  fMain, uFileViewNotebook, ulng;

type
  TSearchResultUtil = class
    class procedure addResultPage( const searchName: String; const files: TStringArray);
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

class procedure TSearchResultUtil.addResultPage( const searchName: String; const files: TStringArray);
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

end.

