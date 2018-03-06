unit uFileSourceTreeBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uFile,
  uFileSourceOperation,
  uFileSourceOperationOptions,
  uFileSourceOperationUI,
  uSearchTemplate,
  uFindFiles;

type
  // Additional data for the filesystem tree node.
  TFileTreeNodeData = class
  public
    // True if any of the subnodes (recursively) are links.
    SubnodesHaveLinks: Boolean;
    // Whether directory or subdirectories have any elements that will not be copied/moved.
    SubnodesHaveExclusions: Boolean;
  end;

  { TFileSourceTreeBuilder }

  TFileSourceTreeBuilder = class
  protected
    FFilesTree: TFileTree;
    FFilesCount: Int64;
    FCurrentDepth: Integer;
    FDirectoriesCount: Int64;
    FFilesSize: Int64;
    FExcludeRootDir: Boolean;
    FFileTemplate: TSearchTemplate;
    FExcludeEmptyTemplateDirectories: Boolean;
    FSymlinkOption: TFileSourceOperationOptionSymLink;
    FRecursive: Boolean;
    FFileChecks: TFindFileChecks;
    FRootDir: String;

    AskQuestion: TAskQuestionFunction;
    CheckOperationState: TCheckOperationStateFunction;

    procedure AddItem(aFile: TFile; CurrentNode: TFileTreeNode);
    procedure AddFilesInDirectory(srcPath: String; CurrentNode: TFileTreeNode); virtual; abstract;
    procedure AddFile(aFile: TFile; CurrentNode: TFileTreeNode);
    procedure AddLink(aFile: TFile; CurrentNode: TFileTreeNode); virtual;
    procedure AddLinkTarget(aFile: TFile; CurrentNode: TFileTreeNode); virtual; abstract;
    procedure AddDirectory(aFile: TFile; CurrentNode: TFileTreeNode);
    procedure DecideOnLink(aFile: TFile; CurrentNode: TFileTreeNode);

    function GetItemsCount: Int64;

  public
    constructor Create(AskQuestionFunction: TAskQuestionFunction;
                       CheckOperationStateFunction: TCheckOperationStateFunction);
    destructor Destroy; override;

    procedure BuildFromFiles(Files: TFiles);
    function ReleaseTree: TFileTree;

    property ExcludeRootDir: Boolean read FExcludeRootDir write FExcludeRootDir;
    property Recursive: Boolean read FRecursive write FRecursive;
    property SymLinkOption: TFileSourceOperationOptionSymLink read FSymlinkOption write FSymlinkOption;

    property FilesTree: TFileTree read FFilesTree;
    property FilesSize: Int64 read FFilesSize;
    property FilesCount: Int64 read FFilesCount;
    property DirectoriesCount: Int64 read FDirectoriesCount;
    property ItemsCount: Int64 read GetItemsCount;
    property ExcludeEmptyTemplateDirectories: Boolean read FExcludeEmptyTemplateDirectories write FExcludeEmptyTemplateDirectories;
    {en
       Does not take ownership of SearchTemplate and does not free it.
    }
    property SearchTemplate: TSearchTemplate read FFileTemplate write FFileTemplate;
  end;

implementation

uses
  uGlobs, uLng;

constructor TFileSourceTreeBuilder.Create(AskQuestionFunction: TAskQuestionFunction;
                                          CheckOperationStateFunction: TCheckOperationStateFunction);
begin
  AskQuestion := AskQuestionFunction;
  CheckOperationState := CheckOperationStateFunction;

  FRecursive := True;
  FSymlinkOption := fsooslNone;
end;

destructor TFileSourceTreeBuilder.Destroy;
begin
  inherited Destroy;
  FFilesTree.Free;
end;

procedure TFileSourceTreeBuilder.BuildFromFiles(Files: TFiles);
var
  i: Integer;
begin
  FreeAndNil(FFilesTree);

  FFilesTree := TFileTreeNode.Create;
  FFilesTree.Data := TFileTreeNodeData.Create;
  FFilesSize := 0;
  FFilesCount := 0;
  FDirectoriesCount := 0;
  FCurrentDepth := 0;
  FRootDir := Files.Path;

  if Assigned(FFileTemplate) then
    SearchTemplateToFindFileChecks(FFileTemplate.SearchRecord, FFileChecks);

  if ExcludeRootDir then
  begin
    for i := 0 to Files.Count - 1 do
      if Files[i].IsDirectory then
        AddFilesInDirectory(Files[i].FullPath + DirectorySeparator, FFilesTree);
  end
  else
  begin
    for i := 0 to Files.Count - 1 do
      AddItem(Files[i].Clone, FFilesTree);
  end;
end;

procedure TFileSourceTreeBuilder.AddFile(aFile: TFile; CurrentNode: TFileTreeNode);
var
  AddedNode: TFileTreeNode;
  AddedIndex: Integer;
begin
  AddedIndex := CurrentNode.AddSubNode(aFile);
  AddedNode := CurrentNode.SubNodes[AddedIndex];
  AddedNode.Data := TFileTreeNodeData.Create;

  Inc(FFilesCount);
  FFilesSize:= FFilesSize + aFile.Size;
  CheckOperationState;
end;

procedure TFileSourceTreeBuilder.AddLink(aFile: TFile; CurrentNode: TFileTreeNode);
var
  AddedNode: TFileTreeNode;
  AddedIndex: Integer;
begin
  AddedIndex := CurrentNode.AddSubNode(aFile);
  AddedNode := CurrentNode.SubNodes[AddedIndex];
  AddedNode.Data := TFileTreeNodeData.Create;

  (CurrentNode.Data as TFileTreeNodeData).SubnodesHaveLinks := True;

  Inc(FFilesCount);
end;

procedure TFileSourceTreeBuilder.AddDirectory(aFile: TFile; CurrentNode: TFileTreeNode);
var
  AddedNode: TFileTreeNode;
  AddedIndex: Integer;
  NodeData: TFileTreeNodeData;
begin
  AddedIndex := CurrentNode.AddSubNode(aFile);
  AddedNode := CurrentNode.SubNodes[AddedIndex];
  NodeData := TFileTreeNodeData.Create;
  AddedNode.Data := NodeData;

  Inc(FDirectoriesCount);

  if FRecursive then
  begin
    if not Assigned(FFileTemplate) or
       (FFileTemplate.SearchRecord.SearchDepth < 0) or
       (FCurrentDepth <= FFileTemplate.SearchRecord.SearchDepth) then
    begin
      Inc(FCurrentDepth);
      AddFilesInDirectory(aFile.FullPath + DirectorySeparator, AddedNode);
      Dec(FCurrentDepth);
    end;

    if Assigned(FFileTemplate) and FExcludeEmptyTemplateDirectories and
       (AddedNode.SubNodesCount = 0) then
    begin
      CurrentNode.RemoveSubNode(AddedIndex);
      (CurrentNode.Data as TFileTreeNodeData).SubnodesHaveExclusions := True;
    end
    else
    begin
      // Propagate flags to parent.
      if NodeData.SubnodesHaveLinks then
        (CurrentNode.Data as TFileTreeNodeData).SubnodesHaveLinks := True;
      if NodeData.SubnodesHaveExclusions then
        (CurrentNode.Data as TFileTreeNodeData).SubnodesHaveExclusions := True;
    end;
  end;
end;

procedure TFileSourceTreeBuilder.DecideOnLink(aFile: TFile; CurrentNode: TFileTreeNode);
begin
  case FSymLinkOption of
    fsooslFollow:
      AddLinkTarget(aFile, CurrentNode);
    fsooslDontFollow:
      AddLink(aFile, CurrentNode);
    fsooslNone:
      begin
        case AskQuestion('', Format(rsMsgFollowSymlink, [aFile.Name]),
                       [fsourYes, fsourAll, fsourNo, fsourSkipAll],
                       fsourYes, fsourNo)
        of
          fsourYes:
            AddLinkTarget(aFile, CurrentNode);
          fsourAll:
            begin
              FSymLinkOption := fsooslFollow;
              AddLinkTarget(aFile, CurrentNode);
            end;
          fsourNo:
            AddLink(aFile, CurrentNode);
          fsourSkipAll:
            begin
              FSymLinkOption := fsooslDontFollow;
              AddLink(aFile, CurrentNode);
            end;
          else
            raise Exception.Create('Invalid user response');
        end;
      end;
    else
      raise Exception.Create('Invalid symlink option');
  end;
end;

procedure TFileSourceTreeBuilder.AddItem(aFile: TFile; CurrentNode: TFileTreeNode);
var
  Matches: Boolean;
begin
  if Assigned(FFileTemplate) then
  begin
    if AFile.IsDirectory or AFile.IsLinkToDirectory then
    begin
      Matches := CheckDirectoryName(FFileChecks, aFile.Name) and
                 CheckDirectoryNameRelative(FFileChecks, aFile.FullPath, FRootDir);
    end
    else begin
      Matches := CheckFile(FFileTemplate.SearchRecord, FFileChecks, aFile);
    end;
    if not Matches then
    begin
      (CurrentNode.Data as TFileTreeNodeData).SubnodesHaveExclusions := True;
      Exit;
    end;
  end;

  if aFile.IsLink then
    DecideOnLink(aFile, CurrentNode)
  else if aFile.IsDirectory then
    AddDirectory(aFile, CurrentNode)
  else
    AddFile(aFile, CurrentNode);
end;

function TFileSourceTreeBuilder.ReleaseTree: TFileTree;
begin
  Result := FFilesTree;
  FFilesTree := nil;
end;

function TFileSourceTreeBuilder.GetItemsCount: Int64;
begin
  Result := FilesCount + DirectoriesCount;
end;

end.

