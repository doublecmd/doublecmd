unit uSearchResultUtil;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes,
  uSearchResultFileSource, uFile, uFileSystemFileSource, uFileSource,
  fMain, uFileViewNotebook;

type
  TSearchResultUtil = class
    class procedure addResultPage( const files: TStringArray);
  end;

implementation

class procedure TSearchResultUtil.addResultPage( const files: TStringArray);
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
  if files = nil then
    Exit;

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
  SearchResultFS := TSearchResultFileSource.Create;
  SearchResultFS.AddList(FileList, Notebook.ActiveView.FileSource);

  NewPage.FileView.AddFileSource(SearchResultFS, SearchResultFS.GetRootDir);
  NewPage.FileView.FlatView := True;
  NewPage.MakeActive;
end;

end.

