unit uSearchResultListOperation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFileSourceListOperation,
  uFileSource,
  uSearchResultFileSource;

type

  TSearchResultListOperation = class(TFileSourceListOperation)
  private
    FFileSource: ISearchResultFileSource;
  public
    constructor Create(aFileSource: IFileSource; aPath: String); override;
    procedure MainExecute; override;
  end;

implementation

uses
  uFile;

constructor TSearchResultListOperation.Create(aFileSource: IFileSource; aPath: String);
begin
  FFiles := TFiles.Create(aPath);
  FFileSource := aFileSource as ISearchResultFileSource;
  inherited Create(aFileSource, aPath);
end;

procedure TSearchResultListOperation.MainExecute;
  procedure AddNode(aNode: TFileTreeNode);
  var
    i: Integer;
  begin
    if Assigned(aNode) then
    begin
      for i := 0 to aNode.SubNodesCount - 1 do
      begin
        CheckOperationState;
        FFiles.Add(aNode.SubNodes[i].TheFile.Clone);
        AddNode(aNode.SubNodes[i]);
      end;
    end;
  end;
begin
  FFiles.Clear;

  // For now "flat mode" always enabled (add all files from the tree).
  if FileSource.IsPathAtRoot(Path) then
    AddNode(FFileSource.FileList);
end;

end.


