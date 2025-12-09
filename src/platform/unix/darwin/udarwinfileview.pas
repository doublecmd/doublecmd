unit uDarwinFileView;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  uiCloudDrive, uSearchResultFileSource, uFile, uFileSystemFileSource, uFileSource,
  fMain, uFileViewNotebook, ulng;

type
  uDarwinFileViewUtil = class
    class procedure addFinderSearchResultPage( const searchName: String; const files: TStringArray );
    class procedure addiCloudDrivePage;
  end;

  TDarwinSearchResultHandler = class
    procedure onSearchFinderTagComplete( const searchName: String; const files: TStringArray );
  end;

var
  darwinSearchResultHandler: TDarwinSearchResultHandler;

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
  uDarwinFileViewUtil.addFinderSearchResultPage( searchName, files );
end;

{ uDarwinFileViewUtil }

class procedure uDarwinFileViewUtil.addFinderSearchResultPage( const searchName: String; const files: TStringArray);
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
  SearchResultFS := TFinderSearchResultFileSource.Create( searchName );
  SearchResultFS.AddList(FileList, Notebook.ActiveView.FileSource);

  NewPage.FileView.AddFileSource(SearchResultFS, SearchResultFS.GetRootDir);
  NewPage.FileView.FlatView := True;
  NewPage.MakeActive;
end;

class procedure uDarwinFileViewUtil.addiCloudDrivePage;
var
  iCloudFS: TiCloudDriveFileSource;
begin
  iCloudFS := TiCloudDriveFileSource.GetFileSource;
  frmMain.ActiveFrame.AddFileSource(iCloudFS, iCloudFS.GetRootDir);
  frmMain.ActiveFrame.SetFocus;
end;

initialization
  darwinSearchResultHandler:= TDarwinSearchResultHandler.Create;

finalization
  FreeAndNil( darwinSearchResultHandler );

end.

