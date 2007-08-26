(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Abbrevia
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ABBREVIA: AbCompnd.inc 3.04                           *}
{*********************************************************}
{* ABBREVIA: Compound File classes and component (Source)*}
{*   See AbCompnd.pas for the VCL header                 *}
{*   See AbQCmpnd.pas for the CLX header                 *}
{*********************************************************}

{$I AbDefine.inc}

{$IFDEF Version6}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
  Classes, SysUtils,
{$IFDEF UsingClx}
  QComCtrls,
{$ELSE}
  ComCtrls,
{$ENDIF}
  AbBase, AbConst, AbDfDec, AbDfEnc, AbDfBase;

const
  AbCompoundFileVersion = '3.1';                                {!!.03}

const
  {SystemBlock constants}
  sbSignatureSize       = 40;   {byte size of Signature field}
  sbVolumeLabelSize     = 40;   {byte size of Volume Label field}
  sbAllocationSizeSize  = 4;    {byte size of Allocation Size field}
  sbVersionSize         = 4;    {byte size of Version field}
  sbUpdateSize          = 1;    {byte size of Updating Flag field}

  {Total size of System Block}
  SizeOfSystemBlock = sbSignatureSize + sbVolumeLabelSize +
                      sbAllocationSizeSize + sbVersionSize + sbUpdateSize;

  {RootDir constants}
  rdEntryNameSize       = 28;  {byte size of Name field}
  rdEntryIDSize         = 4;   {byte size of EntryID field}
  rdParentFolderSize    = 4;   {byte size of ParentFolder field}
  rdEntryTypeSize       = 4;   {byte size of EntryType field}
  rdAttributesSize      = 4;   {byte size of Attributes field}
  rdStartBlockSize      = 4;   {byte size of StartBlock field}
  rdLastModifiedSize    = 8;   {byte size of LastModified field}
  rdSizeSize            = 4;   {byte size of UncompressedSize field}
  rdCompressedSizeSize  = 4;   {byte size of CompressedSize field}

  {Total size of a single Root Directory Entry}
  rdSizeOfDirEntry = rdEntryNameSize + rdEntryIDSize + rdParentFolderSize +
                     rdEntryTypeSize + rdAttributesSize + rdStartBlockSize +
                     rdLastModifiedSize + rdSizeSize + rdCompressedSizeSize;

  rdUnUsed              = -2;  {Constant used to flag an RD entry as unused}

  {Total size of a Root Directory entry}
  SizeOfRootDirBlock = rdEntryNameSize + rdEntryIDSize + rdParentFolderSize +
                       rdEntryTypeSize + rdAttributesSize + rdStartBlockSize +
                       rdLastModifiedSize + rdSizeSize + rdCompressedSizeSize;

  {FAT table constants}
  ftEndOfBlock          = -1;   {End of Block}
  ftUnusedBlock         = -2;   {Unused Block}

  {General constants}
  cfAllocationSize      = 512;  {Default AllocationSize (bytes)}

type
  ECompoundFileError = class(Exception);
  TrdEntryType = (etFolder, etFile);

  {dynamic array parameter for returning FAT chain sequences}
  type TFATChainArray = array of Integer;

  {forwards}
  TAbCompoundFile = class;
  TBeforeDirDeleteEvent = procedure(Sender : TAbCompoundFile; Dir : string;
                                    var AllowDelete : Boolean) of object;
  TBeforeDirModifiedEvent = procedure(Sender : TAbCompoundFile; Dir : string;
                                      var AllowModify : Boolean) of object;
  TBeforeFileDeleteEvent = procedure(Sender : TAbCompoundFile;FileName : string;
                                     var AllowDelete : Boolean) of object;
  TBeforeFileModifiedEvent = procedure(Sender : TAbCompoundFile;
                                       FileName : string; var AllowModify :
                                       Boolean) of object;

  TMultiNode = class(TObject)
  protected  {private}
    FParent     : Pointer;       {pointer to the parent node}
    FKey        : string;        {node identifier}
    FChildren   : TStringList;   {list for child keys & nodes}
    FData       : TObject;       {contained object}

    function GetChildren(Index : Integer) : TMultiNode;
    function GetChildCount : Integer;
  public
    constructor Create(const Key : string);
    destructor Destroy; override;
    function AddChild(const Key : string) : TMultiNode;
    procedure DeleteChild(Index : Integer);
    function DeleteChildByName(const ChildKey : string) : Boolean;
    function DeleteChildren : Boolean;
    function GetChild(Index : integer) : TMultiNode;
    function GetChildByName(const Key : string) : TMultiNode;
    function HasParent : Boolean;
    function HasChildren : Boolean;
    function Contains(const Key : string) : Boolean;
    property Parent : Pointer read FParent write FParent;
    property ChildCount : Integer read GetChildCount;
    property Children[Index : Integer] : TMultiNode read GetChildren;
    property Data : TObject read FData write FData;
    property Key : string read FKey write FKey;
  end;


  TMultiTree = class(TObject)
  protected  {private}
    FRoot        : TMultiNode;     {reference to root node}
    FCount       : Integer;        {count of nodes in the tree}
    FCurrentNode : TMultiNode;     {analogous to current directory}
    FSepChar     : Char;           {directory separator character}
    FIDCount     : Integer;        {counter incremented during preorder trav.}
                                   {(used to assign unique ID to each node)}

    procedure VisitSubNodesPost(Node : TMultiNode; ID : Integer);
    procedure VisitSubNodesPre(Node : TMultiNode; Strm : TStream);
    procedure VisitNode(Node : TMultiNode; Strm : TStream);
    procedure ParseDirStr(const Key : string; Lst : TStringList);      {!!.01}
    procedure PopulateSubNodes(ParentNode : TMultiNode;
                               TreeView : TTreeView; TreeNode : TTreeNode);
    procedure TraversePost(ID : Integer);
    procedure TraversePre(Strm : TStream);

  public
    constructor Create;
    destructor Destroy; override;
    function Insert(ParentNode : TMultiNode; const Key : string) : TMultiNode;
    function GetNode(const Key : string) : TMultiNode;
    function DeleteNode(const Key : string) : Boolean;
    procedure ChangeDir(const Key : string);
    function PopulateTreeView(TreeView : TTreeView) : Integer;
    property Root : TMultiNode read FRoot;
    property Count : Integer read FCount;
    property CurrentNode : TMultiNode read FCurrentNode;
    property SepChar : Char read FSepChar write FSepChar;
  end;


  TAbSystemBlock = class(TObject)
  protected {private}
    FSignature      : string;      {identifies the compound file structure}
    FVolumeLabel    : string;      {file identification in addition to filename}
    FAllocationSize : Integer;     {size of allocation block}
    FVersion        : string;      {version string identifier}
    FUpdating       : Boolean;     {internal processing indicator}

  {protected methods}
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure WriteToStream(Strm : TMemoryStream);

  {properties}
    property Signature : string read FSignature write FSignature;
    property VolumeLabel : string read FVolumeLabel write FVolumeLabel;
    property Updating : Boolean read FUpdating;
    property AllocationSize : Integer
      read FAllocationSize write FAllocationSize;
    property Version : string read FVersion;
  public
    constructor Create(VolLabel : string; AllocationSz : Integer);
  end;

  TAbDirectoryEntry = class(TObject)
  protected {private}
    FName           : string;           {name of file or folder}
    FEntryID        : Integer;          {unique ID for this dir. entry}
    FParentFolder   : LongInt;          {unique ID of parent folder}
    FEntryType      : TrdEntryType;     {folder or file}
    FAttributes     : LongInt;          {file system attributes}
    FStartBlock     : LongInt;          {starting allocation block}
    FLastModified   : TDateTime;        {last modification date/time}
    FSize           : LongInt;          {uncompressed file size}
    FCompressedSize : LongInt;          {compressed file size}

    procedure WriteToStream(Strm : TMemoryStream);
    function IsReadOnly : Boolean;
    function IsHidden : Boolean;
    function IsSysFile : Boolean;
    function IsVolumeID : Boolean;
    function IsDirectory : Boolean;
    function IsArchive : Boolean;
    function GetIsFree : Boolean;

  public
    constructor Create(AsFile : Boolean);

    property EntryName : string read FName write FName;
    property ParentFolder : LongInt read FParentFolder write FParentFolder;
    property Attributes : LongInt read FAttributes write FAttributes;
    property StartBlock : LongInt read FStartBlock write FStartBlock;
    property LastModified : TDateTime read FLastModified write FLastModified;
    property Size : LongInt read FSize write FSize;
    property CompressedSize : LongInt
      read FCompressedSize write FCompressedSize;
    property IsFree : Boolean read GetIsFree;
    property EntryType : TrdEntryType read FEntryType write FEntryType;
  end;


  TAbRootDir = class(TMultiTree)
    fAllocSize : Integer;
  protected {private}
    function AddFolder(FolderName : string) : TAbDirectoryEntry;
    function AddFile(FileName : string) : TAbDirectoryEntry;
    procedure DeleteFolder(FolderName : string);
    procedure DeleteFile(FileName : string);
    procedure WriteToStream(Strm : TMemoryStream);
    procedure GoToEntryID(ID : Integer);
  public
    constructor Create(VolLabel : string; AllocSize : Integer);
    destructor Destroy; override;
  end;


  TAbFATTable = class(TObject)
  protected {private}
    fFATArray  : Array of Integer;       {dynamic array for the FAT}
    fAllocSize : Integer;

    procedure WriteToStream(Strm : TMemoryStream);

  public
    constructor Create(AllocSize : Integer);
    destructor Destroy; override;
    function IsEndOfFile(Ndx : Integer) : Boolean;
    function IsUnUsed(Ndx : Integer) : Boolean;
    function GetNextUnusedBlock : Integer;
    procedure GetNewChain(NumBytes : Integer;
                          var ChainArray : TFATChainArray);
    procedure GetExistingChain(StartNdx : Integer;
                               var ChainArray : TFATChainArray);
    procedure ClearExistingChain(StartNdx : Integer);
    procedure GetRootDirChain(var ChainArray : TFATChainArray);
    procedure GetFATChain(var ChainArray : TFATChainArray);
    procedure GetNewRootDirChain(NumBytes : Integer;
                                 var ChainArray : TFATChainArray);
    procedure GetNewFATChain(NumBytes : Integer;
                             var ChainArray : TFATChainArray);
    procedure ClearRootDirChain;
    procedure ClearFATChain;
  end;


  TAbCompoundFile = class(TObject)
  protected {private}
    FSystemBlock : TAbSystemBlock;      {system block}
    FFATTable    : TAbFATTable;         {FAT table}
    FRootDir     : TAbRootDir;          {root directory}
    FDiskFile    : string;              {compound file name}
    FSizeOnDisk  : Integer;             {sum total of compressed sizes +
                                         uncompressed Sys, RootDir, & FAT blks}
    FStream      : TFileStream;         {Compound file stream (*.cf)}

    FOnAfterOpen          : TNotifyEvent;
    FOnBeforeClose        : TNotifyEvent;
    FOnBeforeDirDelete    : TBeforeDirDeleteEvent;
    FOnBeforeDirModified  : TBeforeDirModifiedEvent;
    FOnBeforeFileDelete   : TBeforeFileDeleteEvent;
    FOnBeforeFileModified : TBeforeFileModifiedEvent;

    function GetVolumeLabel : string;
    procedure SetVolumeLabel(Val : string);
    function GetDirectoryEntries : Integer;
    function GetSizeOnDisk : Integer;
    procedure PersistFileData(FileData : TStream;
                              var ChainArray : TFATChainArray);
    procedure PersistSystemBlock;
    procedure PersistRootDirBlock;
    procedure PersistFATBlock;
    procedure BuildSysBlock;
    procedure BuildFat;
    procedure BuildRootDir;
    procedure AddDirEntriesFromList(Lst : TStringList);

    procedure Defrag;                   {!!.03} {not implemented}
  public
    constructor Create(FileName : string; VolLabel : string;
                       AllocSize : Integer);
    destructor Destroy; override;
    procedure EnumerateFiles(Lst : TStringList);                       {!!.01}
    procedure EnumerateFolders(Lst : TStringList);                     {!!.01}
    procedure AddFile(FName : string; FileData : TStream; FileSize : Integer);
    function AddFolder(FName : string) : Boolean;
    procedure UpdateFile(FName : string; FData : TStream);
    procedure DeleteFile(FName : string);
    procedure DeleteFolder(FName : string);
    procedure Open(FName : string);
    function OpenFile(FileName : string; var Strm : TStream) : Integer;
    function PopulateTreeView(TreeView : TTreeView) : Integer;
    procedure PopulateSubNodes(ParentNode : TMultiNode;
                     TreeView : TTreeView; TreeNode : TTreeNode);
    procedure RenameFile(OrigName, NewName : string);
    procedure RenameFolder(OrigName, NewName : string);
    procedure SetCurrentDirectory(val : string);
    function GetCurrentDirectory : string;
    function GetAllocationSize : Integer;
    property CurrentDirectory : string
      read GetCurrentDirectory write SetCurrentDirectory;
    property DirectoryEntries : Integer read GetDirectoryEntries;
    property SizeOnDisk : Integer read GetSizeOnDisk;
    property Stream : TFileStream read FStream write FStream;


  published
    property VolumeLabel : string read GetVolumeLabel write SetVolumeLabel;
    property FileName : string read FDiskFile;
    property AllocationSize : Integer read GetAllocationSize;

    property OnAfterOpen : TNotifyEvent
      read FOnAfterOpen write FOnAfterOpen;
    property OnBeforeClose : TNotifyEvent
      read FOnBeforeClose write FOnBeforeClose;
    property OnBeforeDirDelete : TBeforeDirDeleteEvent
      read FOnBeforeDirDelete write FOnBeforeDirDelete;
    property OnBeforeDirModified : TBeforeDirModifiedEvent
      read FOnBeforeDirModified write FOnBeforeDirModified;
    property OnBeforeFileDelete : TBeforeFileDeleteEvent
      read FOnBeforeFileDelete write FOnBeforeFileDelete;
    property OnBeforeFileModified : TBeforeFileModifiedEvent
      read FOnBeforeFileModified write FOnBeforeFileModified;
  end;

implementation

{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
             {TMultiNode}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
constructor TMultiNode.Create(const Key : string);
  {- Creates and initializes a new node}
begin
  inherited Create;
  FKey := Key;
  FChildren := TStringList.Create;
  FChildren.Sorted := True;
  FChildren.Duplicates := dupError;
end;
{-----------------------------------------------------------------------------}
destructor TMultiNode.Destroy;
  {- Destroys the node and all of the children}
var
  i : integer;
begin
  {free children}
  for i := FChildren.Count - 1 downto 0 do
    FChildren.Objects[i].Free;
  FChildren.Free;
  if Assigned(FData) then
    TAbDirectoryEntry(FData).Free;
  inherited Destroy;
end;
{-----------------------------------------------------------------------------}
function TMultiNode.AddChild(const Key : string) : TMultiNode;
  {- Creates and adds a new node - returns the newly added node}
begin
  if Contains(Key) then
    Result := nil
  else begin
    Result := TMultiNode.Create(Key);
    Result.Parent := self;
    FChildren.AddObject(Key, Result);
  end;
end;
{-----------------------------------------------------------------------------}
function TMultiNode.Contains(const Key : string) : Boolean;
  {- Returns true if the node contains a child of the name specified by 'Key'}
begin
  Result := (FChildren.IndexOf(Key) >= 0);
end;
{-----------------------------------------------------------------------------}
procedure TMultiNode.DeleteChild(Index : Integer);
  {- Deletes the child node specified by 'Index'}
begin
  if ((Index < 0) or (Index > FChildren.Count - 1)) then
    raise ECompoundFileError.Create(AbCmpndIndexOutOfBounds);
  FChildren.Objects[Index].Free;
  FChildren.Delete(Index);
end;
{-----------------------------------------------------------------------------}
function TMultiNode.DeleteChildByName(const ChildKey : string) : Boolean;
  {- If node found, node is deleted and true is returned, else returns false}
begin
  Result := Contains(ChildKey);
  if Result then begin
    FChildren.Objects[FChildren.IndexOf(ChildKey)].Free;
    FChildren.Delete(FChildren.IndexOf(ChildKey));
  end;
end;
{-----------------------------------------------------------------------------}
function TMultiNode.DeleteChildren : Boolean;
  {- Deletes all child nodes}
var
  i : Integer;
begin
  Result := FChildren.Count > 0;
  for i := FChildren.Count - 1 downto 0 do begin
    FChildren.Objects[i].Free;
    FChildren.Delete(i);
  end;
end;
{-----------------------------------------------------------------------------}
function TMultiNode.GetChild(Index : integer) : TMultiNode;
  {- Returns the node specified by Index}
begin
  if ((Index < 0) or (Index > FChildren.Count - 1)) then
    raise ECompoundFileError.Create(AbCmpndIndexOutOfBounds);
  Result := (FChildren.Objects[Index] as TMultiNode);
end;
{-----------------------------------------------------------------------------}
function TMultiNode.GetChildByName(const Key : string) : TMultiNode;
  {- Returns the child node specified by 'Key'.  If not found, result = nil}
begin
  Result := nil;
  if Contains(Key) then
    Result := (FChildren.Objects[FChildren.IndexOf(Key)] as TMultiNode);
end;
{-----------------------------------------------------------------------------}
function TMultiNode.GetChildCount : Integer;
  {- Returns the node's children count}
begin
  Result := FChildren.Count;
end;
{-----------------------------------------------------------------------------}
function TMultiNode.GetChildren(Index : Integer) : TMultiNode;
  {- Returns the node at 'Index'}
begin
  if ((Index < 0) or (Index > FChildren.Count - 1)) then
    raise ECompoundFileError.Create(AbCmpndIndexOutOfBounds);
  Result := (FChildren.Objects[Index] as TMultiNode);
end;
{-----------------------------------------------------------------------------}
function TMultiNode.HasParent : Boolean;
  {- Returns true if parent is assigned, else returns false}
begin
  Result := (FParent <> nil);
end;
{-----------------------------------------------------------------------------}
function TMultiNode.HasChildren : Boolean;
  {- Returns true if the node contains 1 or more child nodes.}
begin
  Result := (FChildren.Count > 0);
end;
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
          {TMultiTree}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
constructor TMultiTree.Create;
  {- creates an empty tree}
begin
  inherited Create;
  FSepChar := '\';
end;
{-----------------------------------------------------------------------------}
destructor TMultiTree.Destroy;
  {- destroys all nodes (post-order)}
var
  Curr : TMultiNode;
begin
  Curr := Root;
  while Curr <> nil do begin
    if Curr.HasChildren then
      Curr := Curr.GetChildren(0)
    else
      begin
        if Curr = Root then begin
          Curr.Free;
          exit;
        end else begin
          Curr := Curr.Parent;
          Curr.DeleteChild(0);
        end;
      end;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TMultiTree.ChangeDir(const Key : string);
  {- Sets current directory of tree if path('Key') is valid}
var
  Node     : TMultiNode;
  Lst      : TStringList;
  i, Ndx   : integer;
  NotFound : Boolean;
begin
  if Root = nil then exit;
  NotFound := False;
  Lst := TStringList.Create;
  try
    ParseDirStr(Key, Lst);
    Node := CurrentNode;
    for i := 0 to Lst.Count - 1 do begin

      if Lst.Strings[i] = '\' then begin                               {!!.01}
        Node := Root;                                                  {!!.01}
        Continue;                                                      {!!.01}
      end                                                              {!!.01}

      else if Lst.Strings[i] = '.' then                                {!!.01}
        Continue                                                       {!!.01}

      else if Lst.Strings[i] = '..' then begin                         {!!.01}
        if Node <> Root then                                           {!!.01}
          Node := TMultiNode(Node.Parent);                             {!!.01}
      end else begin                                                   {!!.01}
        Ndx := Node.FChildren.IndexOf(Lst.Strings[i]);
        if Ndx >= 0 then
          Node := Node.GetChild(Ndx)
        else begin
          NotFound := True;
          Break;
        end;
      end;
    end;
  finally
    Lst.Free;
  end;
  if NotFound = false then
    FCurrentNode := Node;
end;
{-----------------------------------------------------------------------------}
function TMultiTree.DeleteNode(const Key : string) : Boolean;
  {- If node found, deletes the node & returns true, else returns false}
begin
  Result := False;
  if CurrentNode <> nil then
    if CurrentNode.Contains(Key) then begin
      Result := CurrentNode.DeleteChildByName(Key);
      Dec(FCount);
    end;
end;
{-----------------------------------------------------------------------------}
function TMultiTree.GetNode(const Key : string) : TMultiNode;
  {- Returns the node if found, else returns nil}
begin
  Result := nil;
  if CurrentNode <> nil then
    if CurrentNode.Contains(Key) then
      Result := CurrentNode.GetChildByName(Key);
end;
{-----------------------------------------------------------------------------}
function TMultiTree.Insert(ParentNode : TMultiNode;
                        const Key : string) : TMultiNode;
  {- Adds child node to specified ParentNode}
var
  NewNode : TMultiNode;
begin
  Result := nil;
  if CurrentNode = nil then begin
    {adding root node}
    NewNode := TMultiNode.Create(Key);
    FRoot := NewNode;
    FCurrentNode := NewNode;
    Result := NewNode;
  end else begin
    if not CurrentNode.Contains(Key) then begin
      Result := CurrentNode.AddChild(Key);
      Result.Parent := CurrentNode;
    end;
  end;
  Inc(FCount);
end;
{-----------------------------------------------------------------------------}
{!!.01 - Complete rewrite}
procedure TMultiTree.ParseDirStr(const Key : string; Lst : TStringList);
  {- parses Key into individual dir commands adding each to Lst}
var
  LocKey : string;
  Counter : integer;
begin
  LocKey := Key;
  Lst.Clear;

  {- are we to start from the root folder}
  Counter := 0;
  while LocKey[Counter+1] = '\' do
    inc(Counter);

  if Counter = 1 then
    Lst.Add('\');

  {- begin parsing}
  while Length(LocKey) > 0 do begin
    while LocKey[1] = '\' do
      Delete(LocKey, 1, 1);

    if pos(SepChar,LocKey) > 0 then begin
      Lst.Add(copy(LocKey, 1, Pos(SepChar, LocKey) - 1));
      Delete(LocKey, 1, Pos(SepChar, LocKey));
    end else
      if Length(LocKey) > 0 then begin
        Lst.Add(LocKey);
        LocKey := '';
      end;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TMultiTree.PopulateSubNodes(ParentNode : TMultiNode;
                     TreeView : TTreeView; TreeNode : TTreeNode);
  {- Visits sub-nodes recursively - pre order}
var
  Curr : TMultiNode;
  i    : Integer;
  Node : TTreeNode;
begin
  Node := TreeView.Items.AddChild(TreeNode, ParentNode.Key);
  Curr := ParentNode;
  if Curr <> nil then begin
    if Curr.HasChildren then begin
      for i := 0 to Curr.ChildCount -1 do
        PopulateSubNodes(Curr.Children[i], TreeView, Node);
    end;
  end;
end;
{-----------------------------------------------------------------------------}
function TMultiTree.PopulateTreeView(TreeView : TTreeView) : Integer;
{- Populates a user-supplied TTreeView with multiway tree nodes}
var
  i        : Integer;
  TreeNode : TTreeNode;
begin
  TreeView.Items.Clear;
  if Root <> nil then begin
    TreeNode := TreeView.Items.Add(nil, Root.Key);
    if Root.HasChildren then begin
      for i := 0 to Root.ChildCount - 1 do
        PopulateSubNodes(Root.Children[i], TreeView, TreeNode);
    end;
  end;
  Result := TreeView.Items.Count
end;
{-----------------------------------------------------------------------------}
procedure TMultiTree.TraversePost(ID : Integer);
  {- Traverses tree post-order - CurrentNode after traversal will be the node
     whose EntryID = ID}
var
  i : Integer;
begin
  if Root <> nil then begin
    if Root.HasChildren then begin
      for i := 0 to Root.ChildCount - 1 do
        VisitSubNodesPost(Root.Children[i], ID);
    end;
    if (TAbDirectoryEntry(Root.FData).FEntryID = ID) then
      FCurrentNode := Root;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TMultiTree.TraversePre(Strm : TStream);
  {- Traverses tree pre-order}
var
  i : Integer;
begin
  if Root <> nil then begin
    FIDCount := 1;
    TAbDirectoryEntry(Root.Data).FEntryID := FIDCount;

    VisitNode(Root, Strm);
    if Root.HasChildren then begin
      for i := 0 to Root.ChildCount - 1 do
        VisitSubNodesPre(Root.Children[i], Strm);
    end;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TMultiTree.VisitNode(Node : TMultiNode; Strm : TStream);
  {- Called recursively from VisitSubNodesPre. Assigns unique entry ID's for
     each directory entry to maintain hierarchy}
begin
  if Node.Parent = nil then
    TAbDirectoryEntry(Node.Data).ParentFolder := -1
  else
    TAbDirectoryEntry(Node.Data).ParentFolder :=
      TAbDirectoryEntry(TMultiNode(Node.Parent).Data).FEntryID;

  TAbDirectoryEntry(Node.Data).WriteToStream(TMemoryStream(Strm));
end;
{-----------------------------------------------------------------------------}
procedure TMultiTree.VisitSubNodesPost(Node : TMultiNode; ID : Integer);
  {- Visits sub-nodes recursively - post order}
var
  Curr : TMultiNode;
  i    : Integer;
begin
  Curr := Node;
  if Curr <> nil then begin
    if Curr.HasChildren then begin
      for i := 0 to Curr.ChildCount -1 do
        VisitSubNodesPost(Curr.Children[i], ID);
    end;
    if (TAbDirectoryEntry(Curr.FData).FEntryID = ID) then
      FCurrentNode := Curr;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TMultiTree.VisitSubNodesPre(Node : TMultiNode; Strm : TStream);
  {- Visits sub-nodes recursively - pre order}
var
  Curr : TMultiNode;
  i    : Integer;
begin
  Curr := Node;
  if Curr <> nil then begin
    Inc(FIDCount);
    TAbDirectoryEntry(Curr.Data).FEntryID := FIDCount;

    VisitNode(Curr, Strm);
    if Curr.HasChildren then begin
      for i := 0 to Curr.ChildCount -1 do
        VisitSubNodesPre(Curr.Children[i], Strm);
    end;
  end;
end;
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
              {TAbSystemBlock}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
constructor TAbSystemBlock.Create(VolLabel : string; AllocationSz : Integer);
{- Creates the System block structure of the compound file}
begin
  inherited Create;
  FSignature      := 'AbCompoundFile';
  FVolumeLabel    := VolLabel;
  FAllocationSize := AllocationSz;
  FVersion        := AbCompoundFileVersion;                            {!!.03}
  FUpdating       := False;
end;
{-----------------------------------------------------------------------------}
procedure TAbSystemBlock.BeginUpdate;
  {- Sets updating to true - temporarily blocking other actions}
begin
  FUpdating := True;
end;
{-----------------------------------------------------------------------------}
procedure TAbSystemBlock.EndUpdate;
  {- Clears updating flag & allows for other actions}
begin
  FUpdating := False;
end;
{-----------------------------------------------------------------------------}
procedure TAbSystemBlock.WriteToStream(Strm : TMemoryStream);
  {- writes the contents to the stream parameter}
var
  Sig       : Array[0..sbSignatureSize - 1] of char;
  VolLabel  : Array[0..sbVolumeLabelSize - 1] of char;
  AllocSize : Integer;
  Version   : Array[0..sbVersionSize - 1] of char;
  Updt      : Byte;
begin
  FillChar(Sig, sbSignatureSize, #0);
  strpcopy(Sig, FSignature);

  FillChar(VolLabel[0], sbVolumeLabelSize, #0);
  StrPCopy(VolLabel, FVolumeLabel);

  AllocSize := FAllocationSize;

  FillChar(Version[0], sbVersionSize, #0);
  StrPCopy(Version, FVersion);

  if FUpdating then
    Updt := $01
  else
    Updt := $00;

  Strm.Write(Sig[0], sbSignatureSize);
  Strm.Write(VolLabel[0], sbVolumeLabelSize);
  Strm.Write(AllocSize, SizeOf(Integer));
  Strm.Write(Version[0], sbVersionSize);
  Strm.Write(Updt, sbUpdateSize);
end;
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
           {TAbDirectoryEntry}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
constructor TAbDirectoryEntry.Create(AsFile : Boolean);
  {- Creates & initializes a new TAbDirectoryEntry}
begin
  inherited Create;
  FName           := '';
  FParentFolder   := rdUnused;
  if AsFile then begin
    FEntryType     := etFile;
{$IFNDEF Linux}
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
    FAttributes     := faArchive;
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM ON} {$ENDIF}
{$ELSE}
{$WARN SYMBOL_PLATFORM OFF}
    FAttributes     := faArchive;
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF LINUX}
  end else begin
    FEntryType     := etFolder;
    FAttributes     := faDirectory;
  end;
  FStartBlock     := rdUnused;
  FLastModified   := 0;
  FSize           := rdUnused;
  FCompressedSize := rdUnused;
end;
{-----------------------------------------------------------------------------}
function TAbDirectoryEntry.GetIsFree : Boolean;
  {- returns true if the entry has been marked for deletion}
begin
  Result := (FName = '');
end;
{-----------------------------------------------------------------------------}
function TAbDirectoryEntry.IsArchive : Boolean;
  {- returns true if the entry is an archive}
begin
{$IFNDEF Linux}
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
  Result := ((FAttributes and faArchive) > 0);
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM ON} {$ENDIF}
{$ELSE}
{$WARN SYMBOL_PLATFORM OFF}
  Result := ((FAttributes and faArchive) > 0);
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF LINUX}
end;
{-----------------------------------------------------------------------------}
function TAbDirectoryEntry.IsDirectory : Boolean;
  {- returns true if the entry is a directory}
begin
  Result := ((FAttributes and faDirectory) > 0);
end;
{-----------------------------------------------------------------------------}
function TAbDirectoryEntry.IsHidden : Boolean;
  {- returns true if the entry is hidden}
begin
{$IFNDEF Linux}
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
  Result := ((FAttributes and faHidden) > 0);
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM ON} {$ENDIF}
{$ELSE}
{$WARN SYMBOL_PLATFORM OFF}
  Result := ((FAttributes and faHidden) > 0);
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF LINUX}
end;
{-----------------------------------------------------------------------------}
function TAbDirectoryEntry.IsReadOnly : Boolean;
  {- returns true if the entry is read-only}
begin
{$IFNDEF Linux}
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
  Result := ((FAttributes and faReadOnly) > 0);
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM ON} {$ENDIF}
{$ELSE}
{$WARN SYMBOL_PLATFORM OFF}
  Result := ((FAttributes and faReadOnly) > 0);
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF LINUX}
end;
{-----------------------------------------------------------------------------}
function TAbDirectoryEntry.IsSysFile : Boolean;
  {- returns true if the entry is a system file}
begin
{$IFNDEF Linux}
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
  Result := ((FAttributes and faSysFile) > 0);
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM ON} {$ENDIF}
{$ELSE}
{$WARN SYMBOL_PLATFORM OFF}
  Result := ((FAttributes and faSysFile) > 0);
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF LINUX}
end;
{-----------------------------------------------------------------------------}
function TAbDirectoryEntry.IsVolumeID : Boolean;
  {- returns true if the entry is a volume ID}
begin
{$IFNDEF Linux}
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
  Result := ((FAttributes and faVolumeID) > 0);
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM ON} {$ENDIF}
{$ELSE}
{$WARN SYMBOL_PLATFORM OFF}
  Result := ((FAttributes and faVolumeID) > 0);
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF LINUX}
end;
{-----------------------------------------------------------------------------}
procedure TAbDirectoryEntry.WriteToStream(Strm : TMemoryStream);
  {- writes properties to stream}
var
  EntryName : Array[0..rdEntryNameSize] of char;
  FType     : Integer;
begin
  FillChar(EntryName, rdEntryNameSize - 1, #0);
  StrPCopy(EntryName, FName);

  Strm.Write(EntryName[0], rdEntryNameSize);

  Strm.Write(FEntryID, rdEntryIDSize);
  Strm.Write(FParentFolder, rdParentFolderSize);

  if EntryType = etFolder then
    FType := $00000000
  else
    FType := $00000001;
  Strm.Write(FType, rdEntryTypeSize);
  Strm.Write(FAttributes, rdAttributesSize);
  Strm.Write(FStartBlock, rdStartBlockSize);
  Strm.Write(FLastModified, rdLastModifiedSize);
  Strm.Write(FSize, rdSizeSize);
  Strm.Write(FCompressedSize, rdCompressedSizeSize);
end;
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
            {TAbRootDir}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
constructor TAbRootDir.Create(VolLabel : string; AllocSize : Integer);
  {- Creates a single-entry (vol-label) root directory structure}
begin
  inherited Create;
  fAllocSize := AllocSize;
  if VolLabel <> '' then
    AddFolder(VolLabel);
end;
{-----------------------------------------------------------------------------}
destructor TAbRootDir.Destroy;
  {- Destroys the root dir.}
begin
  inherited Destroy;
end;
{-----------------------------------------------------------------------------}
function TAbRootDir.AddFile(FileName : string) : TAbDirectoryEntry;
  {- Adds a file to the current directory of the compound file}
var
  NewNode : TMultiNode;
  NewData : TAbDirectoryEntry;
begin
  NewData := nil;
  NewNode := Insert(CurrentNode, FileName);
  if NewNode <> nil then begin
    NewData := TAbDirectoryEntry.Create(True);
    NewData.FName := FileName;
    NewData.ParentFolder := 1;
{$IFNDEF Linux}
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
    NewData.Attributes := faArchive;
{$IFDEF Version6} {$WARN SYMBOL_PLATFORM ON} {$ENDIF}
{$ELSE}
{$WARN SYMBOL_PLATFORM OFF}
    NewData.Attributes := faArchive;
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF LINUX}
    NewData.StartBlock := 3;
    NewData.LastModified := Now;
    NewData.Size := 4;
    NewData.CompressedSize := 5;
    NewData.EntryType := etFile;
    NewNode.Data := NewData;
  end;
  Result := NewData;
end;
{-----------------------------------------------------------------------------}
function TAbRootDir.AddFolder(FolderName : string) : TAbDirectoryEntry;
  {- Adds a folder to the current directory of the compound file}
var
  NewNode : TMultiNode;
  NewData : TAbDirectoryEntry;
begin
  Result := nil;
  NewNode := Insert(CurrentNode, FolderName);
  if NewNode <> nil then begin
    NewData := TAbDirectoryEntry.Create(False);
    NewData.FName := FolderName;
    NewData.ParentFolder := 1;
    NewData.Attributes := faDirectory;
    NewData.StartBlock := rdUnUsed;
    NewData.LastModified := Now;
    NewData.Size := 0;
    NewData.CompressedSize := 0;
    NewData.EntryType := etFolder;
    NewNode.Data := NewData;
    Result :=NewData;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TAbRootDir.DeleteFile(FileName : string);
  {- Deletes the specified file if found}
begin
  DeleteNode(FileName);
end;
{-----------------------------------------------------------------------------}
procedure TAbRootDir.DeleteFolder(FolderName : string);
  {- Deletes the specifed folder if found & empty}
begin
  if not CurrentNode.Contains(FolderName) then
    raise ECompoundFileError.Create(AbCmpndFileNotFound);
  if CurrentNode.ChildCount > 0 then
    raise ECompoundFileError.Create(AbCmpndFolderNotEmpty);
  DeleteFolder(FolderName);
end;
{-----------------------------------------------------------------------------}
procedure TAbRootDir.WriteToStream(Strm : TMemoryStream);
  {- Streams and writes the root directory entries to the stream parameter}
begin
  TraversePre(Strm);
end;
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
            {TAbFATTable}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
constructor TAbFATTable.Create(AllocSize : Integer);
  {- Creates the FAT table structure}
var
  i : Integer;
begin
  {Sets FAT length equal to one allocation block}
  fAllocSize := AllocSize;
  SetLength(fFATArray, AllocSize div SizeOf(Integer));
  for i := 0 to High(fFATArray) do
    fFATArray[i] := ftUnusedBlock;
  for i := 0 to 2 do
    fFATArray[i] := ftEndOfBlock;
end;
{-----------------------------------------------------------------------------}
destructor TAbFATTable.Destroy;
  {- Destroys the FAT table}
begin
  Finalize(fFATArray);
end;
{-----------------------------------------------------------------------------}
procedure TAbFATTable.ClearExistingChain(StartNdx : Integer);
  {- Sets all of the FAT entries pertaining to the sequence starting at StartNds
     to ftUnUsedBlock}
var
  ChainArray : TFATChainArray;
  i          : Integer;
begin
  SetLength(ChainArray, 0);
  GetExistingChain(StartNdx, ChainArray);
  for i := 0 to High(ChainArray) do
    fFATArray[ChainArray[i]] := ftUnUsedBlock;
end;
{-----------------------------------------------------------------------------}
procedure TAbFATTable.ClearFATChain;
  {- Sets the FAT entries pertaining to the FAT table to unused}
begin
  ClearExistingChain(2);
end;
{-----------------------------------------------------------------------------}
procedure TAbFATTable.ClearRootDirChain;
  {- Sets the FAT entries pertaining the the RootDir to unused}
begin
  ClearExistingChain(1);
end;
{-----------------------------------------------------------------------------}
procedure TAbFATTable.GetExistingChain(StartNdx : Integer;
                                       var ChainArray : TFATChainArray);
  {- Walks the FAT table starting at the index specified, and populates the
     chain array parameter with the results}
var
  BlkCount, i, ChainNdx : Integer;
begin
  if fFATArray[StartNdx] = ftUnUsedBlock then begin
    SetLength(ChainArray, 0);
    exit;
  end;

  {determine count}
  if StartNdx < 1 then
    SetLength(ChainArray, 0)
  else begin
    BlkCount := 1;
    i := StartNdx;
    while fFATArray[i] <> ftEndOfBlock do begin
      i := fFATArray[i];
      Inc(BlkCount);
    end;

    {set up array}
    SetLength(ChainArray, BlkCount);
    for i := 0 to High(ChainArray) do
      ChainArray[i] := ftUnusedBlock;

    {walk FAT & populate array}
    ChainNdx := 0;
    ChainArray[ChainNdx] := StartNdx;
    i := StartNdx;
    while fFATArray[i] <> ftEndOfBlock do begin
      Inc(ChainNdx);
      ChainArray[ChainNdx] := fFATArray[i];
      i := fFATArray[i];
    end;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TAbFATTable.GetFATChain(var ChainArray : TFATChainArray);
  {- Returns the sequence of FAT blocks used by the FAT table in the
     ChainArray parameter}
begin
  GetExistingChain(2, ChainArray);
end;
{-----------------------------------------------------------------------------}
procedure TAbFATTable.GetNewChain(NumBytes : Integer;
                                  var ChainArray : TFATChainArray);
  {- Finds sequence of free blocks required of a file of size NumBytes
     The new FAT chain is commited and passed back in the ChainArray parameter}
var
  FirstBlock          : Integer;
  TotalBlocksRequired : Integer;
  i, j, BlocksFound   : Integer;
begin
  if ((NumBytes mod fAllocSize) <> 0) then
    TotalBlocksRequired := (NumBytes div fAllocSize) + 1
  else
    TotalBlocksRequired := (NumBytes div fAllocSize);

  if TotalBlocksRequired = 0 then
    exit;

  FirstBlock := GetNextUnusedBlock;

  {set up array}
  SetLength(ChainArray, TotalBlocksRequired);
  for i := 0 to High(ChainArray) do
    ChainArray[i] := ftUnusedBlock;

  ChainArray[0] := FirstBlock;
  BlocksFound := 1;
  i := FirstBlock + 1;

  while BlocksFound < TotalBlocksRequired do begin
    if ((fFATArray[i] = ftUnusedBlock) and (i > 2)) then begin
      ChainArray[BlocksFound] := i;
      inc(BlocksFound);
    end;
    Inc(i);

    if i > High(fFATArray) then begin
      {grow FAT (allocate another block)}
      SetLength(fFATArray, Length(fFATArray) + (fAllocSize div SizeOf(Integer)));
      for j := High(fFATArray) downto (Length(fFATArray) -
                                      (fAllocSize div SizeOf(Integer))) do
        fFATArray[j] := ftUnUsedBlock;
      end;
  end;

  {Update FAT}
  for i := 0 to High(ChainArray) do begin
    if i = High(ChainArray) then
      fFATArray[ChainArray[i]] := -1
    else
      fFATArray[ChainArray[i]] := ChainArray[i+1];
  end;
end;
{-----------------------------------------------------------------------------}
procedure TAbFATTable.GetNewFATChain(NumBytes : Integer;
                                     var ChainArray : TFATChainArray);
  {- Finds and commits a new chain starting at the 3rd block. The new chain is
     returned in the ChainArray parameter}
var
  FirstBlock          : Integer;
  TotalBlocksRequired : Integer;
  i, j, BlocksFound   : Integer;
begin
  if ((NumBytes mod fAllocSize) <> 0) then
    TotalBlocksRequired := (NumBytes div fAllocSize) + 1
  else
    TotalBlocksRequired := (NumBytes div fAllocSize);

  if TotalBlocksRequired = 0 then
    exit;

  FirstBlock := 2;

  {set up array}
  SetLength(ChainArray, TotalBlocksRequired);
  for i := 0 to High(ChainArray) do
    ChainArray[i] := ftUnusedBlock;

  ChainArray[0] := FirstBlock;
  BlocksFound := 1;
  i := FirstBlock + 1;

  while BlocksFound < TotalBlocksRequired do begin
    if ((fFATArray[i] = ftUnusedBlock) and (i > 2)) then begin
      ChainArray[BlocksFound] := i;
      inc(BlocksFound);
    end;
    Inc(i);

    if i > High(fFATArray) then begin
      {grow FAT (allocate another block)}
      SetLength(fFATArray, Length(fFATArray) + (fAllocSize div SizeOf(Integer)));
      for j := High(fFATArray) downto (Length(fFATArray) -
                                      (fAllocSize div SizeOf(Integer))) do
        fFATArray[j] := ftUnUsedBlock;
      end;
  end;

  {Update FAT}
  for i := 0 to High(ChainArray) do begin
    if i = High(ChainArray) then
      fFATArray[ChainArray[i]] := -1
    else
      fFATArray[ChainArray[i]] := ChainArray[i+1];
  end;
end;
{-----------------------------------------------------------------------------}
procedure TAbFATTable.GetNewRootDirChain(NumBytes : Integer;
                                         var ChainArray : TFATChainArray);
  {- Finds and commits a new chain starting at the 2nd block. The new chain is
     returned in the ChainArray parameter}
var
  FirstBlock          : Integer;
  TotalBlocksRequired : Integer;
  i, j, BlocksFound   : Integer;
begin
  if ((NumBytes mod fAllocSize) <> 0) then
    TotalBlocksRequired := (NumBytes div fAllocSize) + 1
  else
    TotalBlocksRequired := (NumBytes div fAllocSize);

  if TotalBlocksRequired = 0 then
    exit;

  FirstBlock := 1;

  {set up array}
  SetLength(ChainArray, TotalBlocksRequired);
  for i := 0 to High(ChainArray) do
    ChainArray[i] := ftUnusedBlock;

  ChainArray[0] := FirstBlock;
  BlocksFound := 1;
  i := FirstBlock + 1;

  while BlocksFound < TotalBlocksRequired do begin
    if ((fFATArray[i] = ftUnusedBlock) and (i > 2)) then begin
      ChainArray[BlocksFound] := i;
      inc(BlocksFound);
    end;
    Inc(i);

    if i > High(fFATArray) then begin
      {grow FAT (allocate another block)}
      SetLength(fFATArray, Length(fFATArray) + (fAllocSize div SizeOf(Integer)));
      for j := High(fFATArray) downto (Length(fFATArray) -
                                      (fAllocSize div SizeOf(Integer))) do
        fFATArray[j] := ftUnUsedBlock;
      end;
  end;

  {Update FAT}
  for i := 0 to High(ChainArray) do begin
    if i = High(ChainArray) then
      fFATArray[ChainArray[i]] := -1
    else
      fFATArray[ChainArray[i]] := ChainArray[i+1];
  end;
end;
{-----------------------------------------------------------------------------}
function TAbFATTable.GetNextUnusedBlock : Integer;
  {- Returns the index into the FAT table of the next block marked as unused}
var
  i, j : Integer;
begin
  if Length(fFATArray) = 0 then
    Result := -1
  else begin
    Result := -1;
    i := 3;
    while i <= High(fFATArray) do begin
      if fFATArray[i] = ftUnusedBlock then begin
        Result := i;
        exit;
      end;
      inc(i);

      if i > High(fFATArray) then begin
        {grow FAT (allocate another block)}
        SetLength(fFATArray, Length(fFATArray) +
                  (fAllocSize div SizeOf(Integer)));
        for j := High(fFATArray) downto (Length(fFATArray) -
                      (fAllocSize div SizeOf(Integer))) do
          fFATArray[j] := ftUnUsedBlock;
        end;
    end;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TAbFATTable.GetRootDirChain(var ChainArray : TFATChainArray);
  {- Returns the sequence of FAT blocks used by the RootDir in the
     ChainArray parameter}
begin
  GetExistingChain(1, ChainArray);
end;
{-----------------------------------------------------------------------------}
function TAbFATTable.IsEndOfFile(Ndx : Integer) : Boolean;
  {- Returns true if Ndx into FAT signifies end of file}
begin
  if ((Ndx < 0) or (Ndx > High(fFATArray)) or
      (Length(fFATArray) = 0)) then
    raise ECompoundFileError.Create(AbCmpndIndexOutOfBounds)
  else
    Result := (fFATArray[Ndx] = ftEndOfBlock);
end;
{-----------------------------------------------------------------------------}
function TAbFATTable.IsUnUsed(Ndx : Integer) : Boolean;
  {- Returns true if Ndx into FAT signifies an unused block}
begin
  if ((Ndx < 0) or (Ndx > High(fFATArray)) or
      (Length(fFATArray) = 0)) then
    raise ECompoundFileError.Create(AbCmpndIndexOutOfBounds)
  else
    Result := (fFATArray[Ndx] = ftUnUsedBlock);
end;
{-----------------------------------------------------------------------------}
procedure TAbFATTable.WriteToStream(Strm : TMemoryStream);
  {- Streams and writes the FAT entries to the stream parameter}
begin
  Strm.Write(fFATArray[0], Length(fFATArray) * SizeOf(Integer));
end;
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
           {TAbCompoundFile}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
constructor TAbCompoundFile.Create(FileName : string; VolLabel : string;
                                   AllocSize : Integer);
  {- Creates a new instance}
var
  Buff : Array of Byte;
begin
  inherited Create;
  FSystemBlock := TAbSystemBlock.Create(VolLabel, AllocSize);
  FFATTable    := TAbFATTable.Create(AllocSize);
  FRootDir     := TAbRootDir.Create(VolLabel, AllocSize);
  {create file}

  if FileName <> '' then begin
    FDiskFile := FileName;
    FStream := TFileStream.Create(FileName, fmOpenReadWrite or
                                  fmCreate or fmShareDenyNone);

    {fill first 3 blocks of file}
    SetLength(Buff, 3 * AllocSize);
    FStream.Write(Buff, 3 * AllocSize);

    {write System, RootDir, and FAT blocks}
    PersistSystemBlock;
    PersistRootDirBlock;
    PersistFATBlock;

    if Assigned(FOnAfterOpen) then
      FOnAfterOpen(self);
  end;
end;
{-----------------------------------------------------------------------------}
destructor TAbCompoundFile.Destroy;
  {- Persists and then destroys the instance of the compound file}
begin
  PersistSystemBlock;
  PersistRootDirBlock;
  PersistFATBlock;

  if Assigned(FOnBeforeClose) then
    FOnBeforeClose(self);

  FSystemBlock.Free;
  FFATTable.Free;
  FRootDir.Free;
  FStream.Free;
  inherited;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.AddFile(FName : string; FileData : TStream;
                                  FileSize : Integer);
   function JustFilename(const PathName : String) : String;            {!!.01}
     {-Return just the filename and extension of a pathname.}          {!!.01}
   var                                                                 {!!.01}
     I : Cardinal;                                                     {!!.01}
   begin                                                               {!!.01}
     Result := '';                                                     {!!.01}
     if PathName = '' then Exit;                                       {!!.01}
     I := Succ(Word(Length(PathName)));                                {!!.01}
     repeat                                                            {!!.01}
       Dec(I);                                                         {!!.01}
     until (PathName[I] in ['\',':']) or (I = 0);                      {!!.01}
     Result := System.Copy(PathName, Succ(I), rdEntryNameSize);        {!!.01}
   end;                                                                {!!.01}

  {- Compresses, adds & persists the data (FileData)}
var
  DirEntry   : TAbDirectoryEntry;
  CompStream : TStream;
  CompHelper : TAbDeflateHelper;
  ChainArray : TFATChainArray;
begin
  FName := JustFileName(FName);                                        {!!.01}
  if ((FStream.Size + FileData.Size +
      (4 * FSystemBlock.AllocationSize)) >= MaxLongInt) then
    raise ECompoundFileError.Create(AbCmpndExceedsMaxFileSize);

  if FSystemBlock.Updating then
    raise ECompoundFileError.Create(AbCmpndBusyUpdating);
  FSystemBlock.BeginUpdate;
  CompStream := TMemoryStream.Create;
  CompHelper := TAbDeflateHelper.Create;
  try
    DirEntry := FRootDir.AddFile(FName);
    if DirEntry <> nil then begin
      DirEntry.FSize := FileSize;

      {compress & update dir entry's compressed size}
      FileData.Seek(0, soFromBeginning);
      Deflate(FileData, CompStream, CompHelper);
      DirEntry.FCompressedSize := CompStream.Size;

      {Get new FAT chain & persist the data}
      SetLength(ChainArray, 0);
      FFATTable.GetNewChain(CompStream.Size, ChainArray);
      DirEntry.FStartBlock := ChainArray[0];
      PersistFileData(CompStream, ChainArray);
      PersistRootDirBlock;
      PersistFATBlock;
    end;
  finally
    CompStream.Free;
    CompHelper.Free;
    FSystemBlock.EndUpdate;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.AddDirEntriesFromList(Lst : TStringList);
  {- Add individual root directory entries to RootDir structure maintaining seq.}
var
  i        : Integer;
  LstEntry : TAbDirectoryEntry;
  Entry    : TAbDirectoryEntry;
begin
  for i := 0 to Lst.Count - 1 do begin
    LstEntry := (Lst.Objects[i] as TAbDirectoryEntry);

    {locate parent folder}
    FRootDir.GoToEntryID(LstEntry.FParentFolder);

    {Add file or folder}
    if LstEntry.EntryType = etFolder then
      Entry := FRootDir.AddFolder(LstEntry.FName)
    else
      Entry := FRootDir.AddFile(LstEntry.FName);

    {assign values}
    Entry.FName := LstEntry.FName;
    Entry.FEntryID := LstEntry.FEntryID;
    Entry.FParentFolder := LstEntry.FParentFolder;
    Entry.FEntryType := LstEntry.FEntryType;
    Entry.FAttributes := LstEntry.FAttributes;
    Entry.FStartBlock := LstEntry.FStartBlock;
    Entry.FLastModified := LstEntry.FLastModified;
    Entry.FSize := LstEntry.FSize;
    Entry.FCompressedSize := LstEntry.FCompressedSize;
  end;
end;
{-----------------------------------------------------------------------------}
function TAbCompoundFile.AddFolder(FName : string) : Boolean;
  {- Adds a new folder (directory) to the compound file}
var
  EntryCount : Integer;
begin
  if ((FStream.Size + FSystemBlock.AllocationSize) >= MaxLongInt) then
    raise ECompoundFileError.Create(AbCmpndExceedsMaxFileSize);

  EntryCount := FRootDir.Count;
  FSystemBlock.BeginUpdate;
    try
      FRootDir.AddFolder(FName);
      PersistRootDirBlock;
      PersistFATBlock;
    finally
      FSystemBlock.EndUpdate;
    end;
  Result := ((FRootDir.Count - EntryCount) = 1);
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.BuildFat;
  {- Extracts FAT from this string, writes it to DestStrm(TMemoryStream) and
     ultimately updates/persists the FAT table}
var
  Buff       : Array of Integer;
  IntBuff    : Array[0..0] of Integer;
  DestStrm   : TMemoryStream;
  i, CurrPos : Integer;
  NextBlock  : Integer;
begin
  DestStrm := TMemoryStream.Create;
  try
    {Dim Buff to allocation block size}
    SetLength(Buff, FSystemBlock.AllocationSize div SizeOf(Integer));

    {Clear Buff}
    for i := Low(Buff) to High(Buff) do
      Buff[i] := ftUnusedBlock;

    {read 1st FAT block into Buff -> Write Buff to DestStrm}
    FStream.Seek(2 * FSystemBlock.AllocationSize, soFromBeginning);
    FStream.Read(Buff[0], FSystemBlock.AllocationSize);
    DestStrm.Write(Buff[0], FSystemBlock.AllocationSize);

    {Determine next block of FAT chain}
    NextBlock := Buff[2];

    {read remaining FAT blocks if they exist}
    While NextBlock <> ftEndOfBlock do begin
      FStream.Seek((NextBlock) * FSystemBlock.AllocationSize, soFromBeginning);

      {Clear buff}
      for i := Low(Buff) to High(Buff) do
        Buff[i] := ftUnusedBlock;

      FStream.Read(Buff[0], FSystemBlock.AllocationSize);
      DestStrm.Write(Buff[0], FSystemBlock.AllocationSize);

      {Determine the next FAT block - we'll return to this position in stream}
      CurrPos := DestStrm.Position;
      DestStrm.Seek((NextBlock - 1) * SizeOf(Integer), soFromBeginning);
      DestStrm.Read(IntBuff[0], SizeOf(Integer));
      NextBlock := IntBuff[0];
      DestStrm.Seek(CurrPos, soFromBeginning);
    end;

    {Set length of and populate the FFATTable.fFATArray in mem structure}
    DestStrm.Seek(0, soFromBeginning);
    SetLength(FFATTable.fFATArray, DestStrm.Size div SizeOf(Integer));
    for i := 1 to DestStrm.Size div SizeOf(Integer) do begin
      DestStrm.Read(IntBuff[0], SizeOf(Integer));
      FFATTable.fFATArray[i-1] := IntBuff[0];
    end;
  finally
    DestStrm.Free;
  end;
  PersistFATBlock;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.BuildRootDir;
  {- Builds list of root directory entries & passes list to AddDirEntriesFromList}
var
  ChainArray : TFATChainArray;
  DestStrm   : TMemoryStream;
  Buff       : Array of Byte;
  i          : Integer;
  Entry      : TAbDirectoryEntry;
  Lst        : TStringList;

  {RootDirEntry buffers}
  EName     : Array[0..rdEntryNameSize - 1] of Char;
  EID       : Array[0..0] of Integer;
  EPF       : Array[0..0] of Integer;
  EType     : Array[0..0] of Integer;
  EAttrib   : Array[0..0] of Integer;
  EStartBlk : Array[0..0] of Integer;
  EMod      : Array[0..0] of TDateTime;
  ESz       : Array[0..0] of Integer;
  ECompSz   : Array[0..0] of Integer;
begin
  {Get RootDir FAT chain}
  FFATTable.GetRootDirChain(ChainArray);
  SetLength(Buff, FSystemBlock.AllocationSize);
  DestStrm := TMemoryStream.Create;
  Lst := TStringList.Create;
  Lst.Duplicates := dupAccept;
  Lst.Sorted := False;
  try
    {Read entire RotDir block to DestStrm}
    for i := 0 to High(ChainArray) do begin
      FStream.Seek(FSystemBlock.AllocationSize * ChainArray[i], soFromBeginning);
      FStream.Read(Buff[0], FSystemBlock.AllocationSize);
      DestStrm.Write(Buff[0], FSystemBlock.AllocationSize);
    end;

    {Reset DestStrm}
    DestStrm.Seek(0, soFromBeginning);

    {For all directory entries, read entry, create object, & add to Lst}
    for i := 0 to (DestStrm.Size div rdSizeOfDirEntry) - 1 do begin
      {read a single directory entry}
      DestStrm.Read(EName[0], rdEntryNameSize);
      if EName = '' then
        continue;
      DestStrm.Read(EID[0], SizeOf(Integer));
      DestStrm.Read(EPF[0], SizeOf(Integer));
      DestStrm.Read(EType[0], SizeOf(Integer));
      DestStrm.Read(EAttrib[0], SizeOf(Integer));
      DestStrm.Read(EStartBlk[0], SizeOf(Integer));
      DestStrm.Read(EMod[0], SizeOf(TDateTime));
      DestStrm.Read(ESz[0], SizeOf(Integer));
      DestStrm.Read(ECompSz[0], SizeOf(Integer));

      if EType[0] = 0 then
        Entry := TAbDirectoryEntry.Create(False)
      else
        Entry := TAbDirectoryEntry.Create(True);

      Entry.FName := EName;
      Entry.FEntryID := EID[0];
      Entry.FParentFolder := EPF[0];
      if EType[0] = 0 then
        Entry.FEntryType := etFolder
      else
        Entry.FEntryType := etFile;
      Entry.FAttributes := EAttrib[0];
      Entry.FStartBlock := EStartBlk[0];
      Entry.FLastModified := EMod[0];
      Entry.FSize := ESz[0];
      Entry.FCompressedSize := ECompSz[0];

      {Don't add an empty dir entry}
      if Entry.FName <> '' then
        Lst.AddObject(IntToStr(i), TObject(Entry));
    end;

  {Add individual root directory entries to RootDir structure maintaining seq.}
  AddDirEntriesFromList(Lst);
  finally
    DestStrm.Free;
    for i := 0 to Lst.Count - 1 do
      if Lst.Objects[i] <> nil then
        TAbDirectoryEntry(Lst.Objects[i]).Free;
    Lst.Free;
  end;

  {Save updates}
  PersistRootDirBlock;
  PersistFATBlock;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.BuildSysBlock;
  {- Constructs System block from the contents of FStream
     (used when opening an existing compound file)}
var
  Sig          : Array[0..sbSignatureSize - 1] of Char;
  VolLabel     : Array[0..sbVolumeLabelSize - 1] of Char;
  Version      : Array[0..sbVersionSize - 1] of Char;
  AllocationSz : Array[0..0] of Integer;
begin
  FStream.Seek(0, soFromBeginning);
  FStream.Read(Sig[0], sbSignatureSize);
  FStream.Read(VolLabel[0], sbVolumeLabelSize);
  FStream.Read(AllocationSz[0], sbAllocationSizeSize);
  FStream.Read(Version[0], sbVersionSize);

  FSystemBlock.Signature := Sig;
  FSystemBlock.VolumeLabel := VolLabel;
  FSystemBlock.AllocationSize := AllocationSz[0];
  FSystemBlock.FVersion := Version;
  PersistSystemBlock;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.Defrag;
  {- Optimizes disk storage}
begin
{ not implemeneted }
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.DeleteFile(FName : string);
  {- Deletes the file from the RootDirectory and FAT blocks (data remains)}
var
  StartBlock  : Integer;
  Allow       : Boolean;
  AllowDirMod : Boolean;
begin
  Allow := True;
  AllowDirMod := True;

  if not FRootDir.CurrentNode.Contains(FName) then
    raise ECompoundFileError.Create(AbCmpndFileNotFound);

  if Assigned(FOnBeforeFileDelete) then
    FOnBeforeFileDelete(self, FName, Allow);

  if Assigned(FOnBeforeDirModified) then
    FOnBeforeDirModified(self, TMultiNode(FRootDir.CurrentNode.Parent).Key,
                         AllowDirMod);

  if (Allow and AllowDirMod) then begin
    StartBlock := TAbDirectoryEntry(FRootDir.GetNode(FName).FData).StartBlock;
    FFATTable.ClearExistingChain(StartBlock);

    FRootDir.DeleteFile(FName);
    PersistRootDirBlock;
    PersistFATBlock;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.DeleteFolder(FName : string);
  {- Deletes the folder from the RootDirectory block}
var
  Allow       : Boolean;
  AllowDirMod : Boolean;
begin
  Allow := True;
  AllowDirMod := True;

  if not FRootDir.CurrentNode.Contains(FName) then
    raise ECompoundFileError.Create(AbCmpndFileNotFound);

  if Assigned(FOnBeforeDirDelete) then
    FOnBeforeDirDelete(self, FName, Allow);

  if Assigned(FOnBeforeDirModified) then
    FOnBeforeDirModified(self, TMultiNode(FRootDir.CurrentNode.Parent).Key,
                         AllowDirMod);

  if (Allow and AllowDirMod) then begin
    FRootDir.DeleteFolder(FName);
    PersistRootDirBlock;
    PersistFATBlock;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.EnumerateFiles(Lst : TStringList);           {!!.01}
var
  i : Integer;
begin
  Lst.Clear;
  for i := 0 to FRootDir.CurrentNode.ChildCount - 1 do begin
    if (FRootDir.CurrentNode.GetChildren(i).Data as TAbDirectoryEntry).EntryType = etFile then
      Lst.Add((FRootDir.CurrentNode.GetChildren(i).Data as TAbDirectoryEntry).EntryName);
  end;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.EnumerateFolders(Lst : TStringList);         {!!.01}
var
  i : Integer;
begin
  Lst.Clear;
  for i := 0 to FRootDir.CurrentNode.ChildCount - 1 do begin
    if (FRootDir.CurrentNode.GetChildren(i).Data as TAbDirectoryEntry).EntryType = etFolder then
      Lst.Add((FRootDir.CurrentNode.GetChildren(i).Data as TAbDirectoryEntry).EntryName);
  end;
end;
{-----------------------------------------------------------------------------}
function TAbCompoundFile.GetAllocationSize : Integer;
  {- Returns the block allocation size used by the compound file}
begin
  result := FSystemBlock.AllocationSize;
end;
{-----------------------------------------------------------------------------}
function TAbCompoundFile.GetCurrentDirectory : string;
  {- Returns the current directory}
begin
  Result := FRootDir.CurrentNode.Key;
end;
{-----------------------------------------------------------------------------}
function TAbCompoundFile.GetDirectoryEntries : Integer;
  {- Returns the total number of directory entries (files and folders)}
begin
  Result := FRootDir.Count;
end;
{-----------------------------------------------------------------------------}
function TAbCompoundFile.GetSizeOnDisk : Integer;
  {- Returns the compound file size (FStream.Size)}
begin
  Result := FStream.Size;
end;
{-----------------------------------------------------------------------------}
function TAbCompoundFile.GetVolumeLabel : string;
  {- Returns the volume label of the compound file}
begin
  Result := FSystemBlock.VolumeLabel;
end;
{-----------------------------------------------------------------------------}
procedure TAbRootDir.GoToEntryID(ID : Integer);
  {- Traverses tree and sets the current node to the node whose EntryID = ID}
begin
  TraversePost(ID);
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.Open(FName : string);
{- Opens an existing compound file and builds Sys, Root Dir, and FAT blocks}
var
  Sig : Array[0..sbSignatureSize - 1] of Char;
begin
  if FStream <> nil then
    FStream.Free;
  FStream := TFileStream.Create(FName, fmOpenReadWrite or fmShareDenyNone);

  {Ensure valid signature}
  FStream.Read(Sig[0], sbSignatureSize);
  if Sig <> 'AbCompoundFile' then begin
    raise ECompoundFileError.Create(AbCmpndInvalidFile);
    exit;
  end;

  FDiskFile := FName;
  {populate Compound File structure}
  BuildSysBlock;
  BuildFat;
  BuildRootDir;

  if Assigned(FOnAfterOpen) then
    FOnAfterOpen(self);
end;
{-----------------------------------------------------------------------------}
function TAbCompoundFile.OpenFile(FileName : string; var Strm : TStream)
                                  : Integer;
  {- Opens the file and writes the file contents to Strm}
var
  ChainArray     : TFatChainArray;
  i, j           : Integer;
  Buff           : Array of Byte;
  RemainingBytes : Integer;
  CompStream     : TStream;
  CompHelper     : TAbDeflateHelper;
begin
  if not FRootDir.CurrentNode.Contains(FileName) then
    raise ECompoundFileError.Create(AbCmpndFileNotFound);

  CompStream := TMemoryStream.Create;
  CompHelper := TAbDeflateHelper.Create;

  try
    {Read the existing (compressed) file into CompStream}
    FFATTable.GetExistingChain((FRootDir.GetNode(FileName).FData
                                as TAbDirectoryEntry).StartBlock, ChainArray);
    SetLength(Buff, FSystemBlock.AllocationSize);
    for i := 0 to high(ChainArray) do begin
      for j := 0 to Pred(FSystemBlock.AllocationSize) do                {!!.02}
        Buff[j] := Byte(chr(0));
      FStream.Seek((ChainArray[i]) * FSystemBlock.AllocationSize, soFromBeginning);
      if i <> High(ChainArray) then begin
        FStream.Read(buff[0], FSystemBlock.AllocationSize);
        CompStream.Write(Buff[0], FSystemBlock.AllocationSize);
      end else begin
        {read less than entire block}
        RemainingBytes := (FRootDir.GetNode(FileName).FData as TAbDirectoryEntry).
                           CompressedSize mod FSystemBlock.AllocationSize;
        FStream.Read(Buff[0], RemainingBytes);
        CompStream.Write(Buff[0], RemainingBytes);
      end;
    end;

    {CompStream now contains the entire compressed file stream}
    CompStream.Seek(0, soFromBeginning);
    Inflate(CompStream, Strm, CompHelper);
  finally
    CompStream.Free;
    CompHelper.Free;
  end;
  Result := Strm.Size;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.PersistFATBlock;
  {- Saves the FAT table to disk}
var
  FATStrm    : TMemoryStream;
  Buff       : Array of Byte;
  i          : Integer;
  ChainArray : TFATChainArray;
begin
  {Init Buffer}
  SetLength(Buff, FSystemBlock.AllocationSize);

  {Init & fill RootDir stream}
  FATStrm := TMemoryStream.Create;

  try
    FFATTable.WriteToStream(FATStrm);

    {prep FAT Table}
    fFATTable.ClearFATChain;
    fFATTable.GetNewFATChain(FATStrm.Size, ChainArray);

    FATStrm.Seek(0, soFromBeginning);
    for i := 0 to High(ChainArray) do begin

      {Clear block contents}
      FillChar(Buff[0], FSystemBlock.AllocationSize, #0);
      FStream.Seek(FSystemBlock.FAllocationSize * ChainArray[i], soFromBeginning);
      FStream.Write(Buff[0], FSystemBlock.AllocationSize);

      {write new contents}
      FATStrm.Read(Buff[0], FSystemBlock.AllocationSize);
      FStream.Seek(FSystemBlock.FAllocationSize * ChainArray[i], soFromBeginning);
      FStream.Write(Buff[0], FSystemBlock.AllocationSize);
    end;
  finally
    FATStrm.Free;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.PersistFileData(FileData : TStream;
                                          var ChainArray : TFATChainArray);
  {- Walks FAT chain and persists data (FileData) to the corresponding blocks}
var
  Buff : Array of Byte;
  i    : Integer;
  j    : Integer;
begin
  if FileData <> nil then begin
    FileData.Seek(0, soFromBeginning);
    SetLength(Buff, FSystemBlock.AllocationSize);
    for i := 0 to High(ChainArray) do begin
      for j := 0 to FSystemBlock.AllocationSize - 1 do
        Buff[j] := Byte(chr(0));
      FileData.Read(Buff[0], FSystemBlock.AllocationSize);
      FStream.Seek(FSystemBlock.AllocationSize * ChainArray[i], soFromBeginning);

      FStream.Write(Buff[0],FSystemBlock.AllocationSize);
    end;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.PersistRootDirBlock;
  {- Saves the RootDirectory block to disk}
var
  RdStrm     : TMemoryStream;
  Buff       : Array of Byte;
  i          : Integer;
  ChainArray : TFATChainArray;
begin
  {Init Buffer}
  SetLength(Buff, FSystemBlock.AllocationSize);

  {Init & fill RootDir stream}
  RdStrm := TMemoryStream.Create;
  try
    FRootDir.WriteToStream(RdStrm);

    {prep FAT Table}
    fFATTable.ClearRootDirChain;
    fFATTable.GetNewRootDirChain(RdStrm.Size, ChainArray);

    RdStrm.Seek(0, soFromBeginning);
    for i := 0 to High(ChainArray) do begin
      {Clear block contents}
      FillChar(Buff[0], FSystemBlock.AllocationSize, #0);
      FStream.Seek(FSystemBlock.FAllocationSize * ChainArray[i], soFromBeginning);
      FStream.Write(Buff[0], FSystemBlock.AllocationSize);

      {write new contents}
      RdStrm.Read(Buff[0], FSystemBlock.AllocationSize);
      FStream.Seek(FSystemBlock.FAllocationSize * ChainArray[i], soFromBeginning);
      FStream.Write(Buff[0], FSystemBlock.AllocationSize);
    end;
  finally
    RdStrm.Free;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.PersistSystemBlock;
  {- Saves the System block to disk}
var
  Strm : TMemoryStream;
  Buff : Array of Byte;
begin
  SetLength(Buff, FSystemBlock.AllocationSize);
  Strm := TMemoryStream.Create;
  try
    FSystemBlock.WriteToStream(Strm);
    Strm.Seek(0, soFromBeginning);
    Strm.Read(Buff[0], Strm.Size);
    FStream.Seek(0, soFromBeginning);
    FStream.Write(Buff[0], FSystemBlock.AllocationSize);
  finally
    Strm.Free;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.SetCurrentDirectory(val : string);
  {- Changes the current directory to the val parameter}
begin
  FRootDir.ChangeDir(Val);
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.SetVolumeLabel(Val : string);
  {- Sets the volume label of the compound file}
begin
  FSystemBlock.VolumeLabel := Val;
  PersistSystemBlock;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.UpdateFile(FName : string; FData : TStream);
var
  StartBlk    : Integer;
  ChainArray  : TFATChainArray;
  DirEntry    : TAbDirectoryEntry;
  CompStream  : TStream;
  CompHelper  : TAbDeflateHelper;
  Allow       : Boolean;
  AllowDirMod : Boolean;
begin
  Allow := True;
  AllowDirMod := True;
  if not FRootDir.CurrentNode.Contains(FName) then
    raise ECompoundFileError.Create(AbCmpndFileNotFound);

  if ((FStream.Size + FData.Size +
      (4 * FSystemBlock.AllocationSize)) >= MaxLongInt) then
    raise ECompoundFileError.Create(AbCmpndExceedsMaxFileSize);

  if Assigned(FOnBeforeFileModified) then
    FOnBeforeFileModified(self, FName, Allow);

  if Assigned(FOnBeforeDirModified) then
    FOnBeforeDirModified(self, TMultiNode(FRootDir.CurrentNode.Parent).Key,
                         AllowDirMod);

  if (Allow and AllowDirMod) then begin
    {get dir entry & start block}
    DirEntry := TAbDirectoryEntry(FRootDir.CurrentNode.GetChildByName(FName).Data);
    StartBlk := DirEntry.StartBlock;
    CompStream := TMemoryStream.Create;
    CompHelper := TAbDeflateHelper.Create;

    try
      {clear existing FAT chain}
      FFATTable.ClearExistingChain(StartBlk);
      SetLength(ChainArray, 0);

      {Deflate data}
      FData.Seek(0, soFromBeginning);
      Deflate(FData, CompStream, CompHelper);

      {Commit new FAT chain}
      FFATTable.GetNewChain(CompStream.Size, ChainArray);

      {update start block, size, compressed size}
      DirEntry.FStartBlock := ChainArray[0];
      DirEntry.Size := FData.Size;
      DirEntry.CompressedSize := CompStream.Size;

      {persist changes}
      PersistFileData(CompStream, ChainArray);
      PersistRootDirBlock;
      PersistFATBlock;
    finally
      CompStream.Free;
      CompHelper.Free;
    end;
  end;
end;
{-----------------------------------------------------------------------------}



function TAbCompoundFile.PopulateTreeView(TreeView : TTreeView) : Integer;
  {- Populates the tree view parameter with all root directory entries}
var
  i        : Integer;
  TreeNode : TTreeNode;
begin
  TreeView.Items.Clear;
  if FRootDir.Root <> nil then begin
    TreeNode := TreeView.Items.Add(nil, FRootDir.Root.Key);
    TreeNode.ImageIndex := 0;
    TreeNode.SelectedIndex := 0;
    if FRootDir.Root.HasChildren then begin
      for i := 0 to FRootDir.Root.ChildCount - 1 do
        PopulateSubNodes(FRootDir.Root.Children[i], TreeView, TreeNode);
    end;
  end;
  Result := TreeView.Items.Count;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.PopulateSubNodes(ParentNode : TMultiNode;
                     TreeView : TTreeView; TreeNode : TTreeNode);
  {- Visits sub-nodes recursively - pre order}
var
  Curr : TMultiNode;
  i    : Integer;
  Node : TTreeNode;
begin
  Node := TreeView.Items.AddChild(TreeNode, ParentNode.Key);
  if TAbDirectoryEntry(ParentNode.Data).EntryType = etFolder then begin
    Node.ImageIndex := 0;
    Node.SelectedIndex := 0;
  end else begin
    Node.ImageIndex := 1;
    Node.SelectedIndex := 1;
  end;
  Curr := ParentNode;
  if Curr <> nil then begin
    if Curr.HasChildren then begin
      for i := 0 to Curr.ChildCount -1 do
        PopulateSubNodes(Curr.Children[i], TreeView, Node);
    end;
  end;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.RenameFile(OrigName, NewName : string);
  {- Renames the file if file is found}
var
  MultNode    : TMultiNode;
  Allow       : Boolean;
  AllowDirMod : Boolean;
begin
  Allow := True;
  AllowDirMod := True;

  {confirm valid names}
  if ((OrigName = '') or (NewName = '')) then exit;

  {prevent duplicate names}
  if ((FRootDir.FCurrentNode.Contains(NewName)) or
     (FRootDir.FCurrentNode.Key = NewName)) then exit;

  if Assigned(FOnBeforeFileModified) then
    FOnBeforeFileModified(self, OrigName, Allow);

  if Assigned(FOnBeforeDirModified) then
    FOnBeforeDirModified(self, TMultiNode(FRootDir.CurrentNode.Parent).Key,
                         AllowDirMod);

  if (Allow and AllowDirMod) then begin
    if FRootDir.FCurrentNode.Contains(OrigName) then begin
      MultNode := FRootDir.FCurrentNode.GetChildByName(OrigName);
      MultNode.Key := NewName;
      TAbDirectoryEntry(MultNode.Data).FName := NewName;
      PersistRootDirBlock;
    end else if FRootDir.FCurrentNode.Key = OrigName then begin
      MultNode := FRootDir.FCurrentNode;
      MultNode.Key := NewName;
      TAbDirectoryEntry(MultNode.Data).FName := NewName;
      PersistRootDirBlock;
    end else
      raise ECompoundFileError.Create(AbCmpndFileNotFound);
  end;
end;
{-----------------------------------------------------------------------------}
procedure TAbCompoundFile.RenameFolder(OrigName, NewName : string);
  {- Renames the folder if the folder is found}
var
  MultNode    : TMultiNode;
  Allow       : Boolean;
  AllowDirMod : Boolean;
begin
  Allow := True;
  AllowDirMod := True;

  {confirm valid names}
  if ((OrigName = '') or (NewName = '')) then exit;

  {prevent duplicate names}
  if ((FRootDir.FCurrentNode.Contains(NewName)) or
     (FRootDir.FCurrentNode.Key = NewName)) then exit;


  if Assigned(FOnBeforeFileModified) then
    FOnBeforeFileModified(self, OrigName, Allow);

  if Assigned(FOnBeforeDirModified) then
    FOnBeforeDirModified(self, TMultiNode(FRootDir.CurrentNode.Parent).Key,
                         AllowDirMod);

  if (Allow and AllowDirMod) then begin
    if FRootDir.FCurrentNode.Contains(OrigName) then begin
      MultNode := FRootDir.FCurrentNode.GetChildByName(OrigName);
      if (TAbDirectoryEntry(MultNode.Data).EntryType <> etFolder) then
        exit;
      MultNode.Key := NewName;
      TAbDirectoryEntry(MultNode.Data).FName := NewName;
      PersistRootDirBlock;
    end else if FRootDir.FCurrentNode.Key = OrigName then begin
      MultNode := FRootDir.FCurrentNode;
      if (TAbDirectoryEntry(MultNode.Data).EntryType <> etFolder) then
        exit;
      MultNode.Key := NewName;
      TAbDirectoryEntry(MultNode.Data).FName := NewName;
      PersistRootDirBlock;
    end else
      raise ECompoundFileError.Create(AbCmpndFileNotFound);
  end;
end;
{-----------------------------------------------------------------------------}

end.
