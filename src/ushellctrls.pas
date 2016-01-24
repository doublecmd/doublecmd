{
 /***************************************************************************
                                   ShellCtrls.pas
                                   ------------


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit uShellCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, LCLType, AvgLvlTree,
  ComCtrls, FileUtil, LazFileUtils, LazUtf8, LCLStrConsts;

{$if defined(Windows) or defined(darwin)}
{$define CaseInsensitiveFilenames}
{$endif}
{$IF defined(CaseInsensitiveFilenames) or defined(darwin)}
{$DEFINE NotLiteralFilenames}
{$ENDIF}

type

  { TObjectTypes }

  TObjectType = (otFolders, otNonFolders, otHidden);

  TObjectTypes = set of TObjectType;

  TFileSortType = (fstNone, fstAlphabet, fstFoldersFirst);

  { Forward declaration of the classes }

  TCustomShellTreeView = class;
  TCustomShellListView = class;

  { TCustomShellTreeView }

  TCustomShellTreeView = class(TCustomTreeView)
  private
    FObjectTypes: TObjectTypes;
    FRoot: string;
    FShellListView: TCustomShellListView;
    FFileSortType: TFileSortType;
    FInitialRoot: String;
    { Setters and getters }
    function GetPath: string;
    procedure SetFileSortType(const AValue: TFileSortType);
    procedure SetObjectTypes(AValue: TObjectTypes);
    procedure SetPath(AValue: string);
    procedure SetRoot(const AValue: string);
    procedure SetShellListView(const Value: TCustomShellListView);
  protected
    procedure Loaded; override;
    { Other methods specific to Lazarus }
    function  PopulateTreeNodeWithFiles(
      ANode: TTreeNode; ANodePath: string): Boolean;
    procedure PopulateWithBaseFiles;
    procedure DoSelectionChanged; override;
    function CanExpand(Node: TTreeNode): Boolean; override;
  public
    { Basic methods }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Methods specific to Lazarus - useful for other classes }
    class function  GetBasePath: string;
    function  GetRootPath: string;
    class procedure GetFilesInDir(const ABaseDir: string;
      AMask: string; AObjectTypes: TObjectTypes; AResult: TStrings; AFileSortType: TFileSortType = fstNone);
    { Other methods specific to Lazarus }
    function  GetPathFromNode(ANode: TTreeNode): string;
    function  GetSelectedNodePath: string; deprecated 'Use property Path instead';
    procedure Refresh(ANode: TTreeNode); overload;

    { Properties }
    property ObjectTypes: TObjectTypes read FObjectTypes write SetObjectTypes;
    property ShellListView: TCustomShellListView read FShellListView write SetShellListView;
    property FileSortType: TFileSortType read FFileSortType write SetFileSortType;
    property Root: string read FRoot write SetRoot;
    property Path: string read GetPath write SetPath;

    { Protected properties which users may want to access, see bug 15374 }
    property Items;
  end;

  { TShellTreeView }

  TShellTreeView = class(TCustomShellTreeView)
  published
    { TCustomTreeView properties }
    property Align;
    property Anchors;
    property AutoExpand;
    property BorderSpacing;
    //property BiDiMode;
    property BackgroundColor;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property Enabled;
    property ExpandSignType;
    property Font;
    property FileSortType;
    property HideSelection;
    property HotTrack;
    property Images;
    property Indent;
    //property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property Root;
    property RowSelect;
    property ScrollBars;
    property SelectionColor;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property ToolTips;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnEdited;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnSelectionChanged;
    property OnShowHint;
    property OnUTF8KeyPress;
    property Options;
    property TreeLineColor;
    property TreeLinePenStyle;
    property ExpandSignColor;
    { TCustomShellTreeView properties }
    property ObjectTypes;
    property ShellListView;
  end;

  { TCustomShellListView }

  TCustomShellListView = class(TCustomListView)
  private
    FMask: string;
    FObjectTypes: TObjectTypes;
    FRoot: string;
    FShellTreeView: TCustomShellTreeView;
    { Setters and getters }
    procedure SetMask(const AValue: string);
    procedure SetShellTreeView(const Value: TCustomShellTreeView);
    procedure SetRoot(const Value: string);
  protected
    { Methods specific to Lazarus }
    procedure PopulateWithRoot();
    procedure Resize; override;
  public
    { Basic methods }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Methods specific to Lazarus }
    function GetPathFromItem(ANode: TListItem): string;
    { Properties }
    property Mask: string read FMask write SetMask; // Can be used to conect to other controls
    property ObjectTypes: TObjectTypes read FObjectTypes write FObjectTypes;
    property Root: string read FRoot write SetRoot;
    property ShellTreeView: TCustomShellTreeView read FShellTreeView write SetShellTreeView;
    { Protected properties which users may want to access, see bug 15374 }
    property Items;
  end;

  { TShellListView }

  TShellListView = class(TCustomShellListView)
  public
    property Columns;
  published
    { TCustomListView properties
      The same as TListView excluding data properties }
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property BorderWidth;
//    property Checkboxes;
    property Color default clWindow;
//    property ColumnClick;
    property Constraints;
    property DragCursor;
    property DragMode;
//    property DefaultItemHeight;
//    property DropTarget;
    property Enabled;
//    property FlatScrollBars;
    property Font;
//    property FullDrag;
//    property GridLines;
    property HideSelection;
//    property HotTrack;
//    property HotTrackStyles;
//    property HoverTime;
    property LargeImages;
    property MultiSelect;
//    property OwnerData;
//    property OwnerDraw;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RowSelect;
    property ScrollBars;
    property ShowColumnHeaders;
    property ShowHint;
//    property ShowWorkAreas;
    property SmallImages;
    property SortColumn;
    property SortType;
    property StateImages;
    property TabStop;
    property TabOrder;
    property ToolTips;
    property Visible;
    property ViewStyle default vsReport;
//    property OnAdvancedCustomDraw;
//    property OnAdvancedCustomDrawItem;
//    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnClick;
    property OnColumnClick;
    property OnCompare;
    property OnContextPopup;
//    property OnCustomDraw;
//    property OnCustomDrawItem;
//    property OnCustomDrawSubItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDrag;
    property OnUTF8KeyPress;
    { TCustomShellListView properties }
    property ObjectTypes;
    property Root;
    property ShellTreeView;
  end;

  EShellCtrl = class(Exception);
  EInvalidPath = class(EShellCtrl);

function DbgS(OT: TObjectTypes): String; overload;

procedure Register;

implementation

{$ifdef windows}
uses Windows;
{$endif}



function DbgS(OT: TObjectTypes): String; overload;
begin
  Result := '[';
  if (otFolders in OT) then Result := Result + 'otFolders,';
  if (otNonFolders in OT) then Result := Result + 'otNonFolders,';
  if (otHidden in OT) then Result := Result + 'otHidden';
  if Result[Length(Result)] = ',' then System.Delete(Result, Length(Result), 1);
  Result := Result + ']';
end;


{
uses ShlObj;

//  $I shellctrlswin32.inc

procedure PopulateTreeViewWithShell(ATreeView: TCustomShellTreeView);
var
  ShellFolder: IShellFolder = nil;
  Win32ObjectTypes: Integer;
//  pidl: LPITEMIDLIST;
  pidlParent: LPITEMIDLIST;
begin
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, @pidl);

  SHGetDesktopFolder(ShellFolder);

  if ShellFolder = nil then Exit;

  // Converts the control data into Windows constants

  Win32ObjectTypes := 0;

  if otFolders in ATreeView.ObjectTypes then
    Win32ObjectTypes := Win32ObjectTypes or SHCONTF_FOLDERS;

  if otNonFolders in ATreeView.ObjectTypes then
    Win32ObjectTypes := Win32ObjectTypes or SHCONTF_NONFOLDERS;

  if otHidden in ATreeView.ObjectTypes then
    Win32ObjectTypes := Win32ObjectTypes or SHCONTF_INCLUDEHIDDEN;

  // Now gets the name of the desktop folder
}

{ TCustomShellTreeView }

procedure TCustomShellTreeView.SetShellListView(
  const Value: TCustomShellListView);
var
  Tmp: TCustomShellListView;
begin
  if FShellListView = Value then Exit;

  if Assigned(FShellListView) then
  begin
    Tmp := FShellListView;
    FShellListView := nil;
    Tmp.ShellTreeView := nil;
  end;

  FShellListView := Value;

  // Update the pair, it will then update itself
  // in the setter of this property
  // Updates only if necessary to avoid circular calls of the setters
  if Assigned(Value) and (Value.ShellTreeView <> Self) then
    Value.ShellTreeView := Self;
end;

procedure TCustomShellTreeView.Loaded;
begin
  inherited Loaded;
  if (FInitialRoot = '') then
    PopulateWithBaseFiles()
  else
    SetRoot(FInitialRoot);
end;

procedure TCustomShellTreeView.SetRoot(const AValue: string);
var
  RootNode: TTreeNode;
begin
  if FRoot=AValue then exit;
  if (csLoading in ComponentState) then
  begin
    FInitialRoot := AValue;
    Exit;
  end;
  //Delphi raises an unspecified exception in this case, but don't crash the IDE at designtime
  if not (csDesigning in ComponentState)
     and (AValue <> '')
     and not DirectoryExistsUtf8(ExpandFilenameUtf8(AValue)) then
     Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidRoot,[ExpandFileNameUtf8(AValue)]);
  if (AValue = '') then
    FRoot := GetBasePath
  else
    FRoot:=AValue;
  Items.Clear;
  if FRoot = '' then
  begin
    PopulateWithBaseFiles()
  end
  else
  begin
    //Add a node for Root and expand it (issue #0024230)
    //Make FRoot contain fully qualified pathname, we need it later in GetPathFromNode()
    FRoot := ExpandFileNameUtf8(FRoot);
    //Set RootNode.Text to AValue so user can choose if text is fully qualified path or not
    RootNode := Items.AddChild(nil, AValue);
    RootNode.HasChildren := True;
    RootNode.Expand(False);
  end;
  if Assigned(ShellListView) then
    ShellListView.Root := FRoot;
end;

// ToDo: Optimize, now the tree is populated in constructor, SetRoot and SetFileSortType.
// For some reason it does not show in performance really.
procedure TCustomShellTreeView.SetFileSortType(const AValue: TFileSortType);
var
  RootNode: TTreeNode;
  CurrPath: String;
begin
  if FFileSortType=AValue then exit;
  FFileSortType:=AValue;
  if (csLoading in ComponentState) then Exit;
  CurrPath := GetPath;
  try
    BeginUpdate;
    Items.Clear;
    if FRoot = '' then
      PopulateWithBaseFiles()
    else
    begin
      RootNode := Items.AddChild(nil, FRoot);
      RootNode.HasChildren := True;
      RootNode.Expand(False);
      try
       SetPath(CurrPath);
      except
        // CurrPath may have been removed in the mean time by another process, just ignore
        on E: EInvalidPath do ;//
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomShellTreeView.SetObjectTypes(AValue: TObjectTypes);
var
  CurrPath: String;
begin
  if FObjectTypes = AValue then Exit;
  FObjectTypes := AValue;
  if (csLoading in ComponentState) then Exit;
  CurrPath := GetPath;
  try
    BeginUpdate;
    Refresh(nil);
    try
       SetPath(CurrPath);
    except
      // CurrPath may have been removed in the mean time by another process, just ignore
      on E: EInvalidPath do ;//
    end;
  finally
    EndUpdate;
  end;
end;

function TCustomShellTreeView.CanExpand(Node: TTreeNode): Boolean;
var
  OldAutoExpand: Boolean;
begin
  Result:=inherited CanExpand(Node);
  if not Result then exit;
  OldAutoExpand:=AutoExpand;
  AutoExpand:=False;
  Node.DeleteChildren;
  Result := PopulateTreeNodeWithFiles(Node, GetPathFromNode(Node));
  AutoExpand:=OldAutoExpand;
end;

constructor TCustomShellTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInitialRoot := '';

  // Initial property values
  FObjectTypes:= [otFolders];

  // Populating the base dirs is done in Loaded
end;

destructor TCustomShellTreeView.Destroy;
begin
  ShellListView := nil;
  inherited Destroy;
end;

type
  { TFileItem }
  TFileItem = class(TObject)
    Name: string;
    isFolder: Boolean;
    //more data to sort by size, date... etc
    constructor Create(const DirInfo: TSearchRec);
  end;

{ TFileItem }

constructor TFileItem.Create(const DirInfo:TSearchRec);
begin
  Name:=DirInfo.Name;
  isFolder:=DirInfo.Attr and FaDirectory > 0;
end;

function FilesSortAlphabet(p1, p2: Pointer): Integer;
var
  f1, f2: TFileItem;
begin
  f1:=TFileItem(p1);
  f2:=TFileItem(p2);
  Result:=CompareText(f1.Name, f2.Name);
end;

function FilesSortFoldersFirst(p1,p2: Pointer): Integer;
var
  f1, f2: TFileItem;
begin
  f1:=TFileItem(p1);
  f2:=TFileItem(p2);
  if f1.isFolder=f2.isFolder then
    Result:=FilesSortAlphabet(p1,p2)
  else begin
    if f1.isFolder then Result:=-1
    else Result:=1;
  end;

end;

function STVCompareFiles(f1, f2: Pointer): integer;
begin
  Result:=CompareFilenames(AnsiString(f1),AnsiString(f2));
end;

{ Helper routine.
  Finds all files/directories directly inside a directory.
  Does not recurse inside subdirectories.

  AMask may contain multiple file masks separated by ;
  Don't add a final ; after the last mask.
}
class procedure TCustomShellTreeView.GetFilesInDir(const ABaseDir: string;
  AMask: string; AObjectTypes: TObjectTypes; AResult: TStrings; AFileSortType: TFileSortType);
var
  DirInfo: TSearchRec;
  FindResult: Integer;
  IsDirectory, IsValidDirectory, IsHidden, AddFile: Boolean;
  ObjectData: TObject;
  SearchStr: string;
  MaskStr: string;
  Files: TList;
  FileItem: TFileItem;
  i: Integer;
  MaskStrings: TStringList;
  FileTree: TAvgLvlTree;
  ShortFilename: AnsiString;
  j: Integer;
  {$if defined(windows) and not defined(wince)}
  ErrMode : LongWord;
  {$endif}
begin
  {$if defined(windows) and not defined(wince)}
  // disables the error dialog, while enumerating not-available drives
  // for example listing A: path, without diskette present.
  // WARNING: Since Application.ProcessMessages is called, it might effect some operations!
  ErrMode:=SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOALIGNMENTFAULTEXCEPT or SEM_NOGPFAULTERRORBOX or SEM_NOOPENFILEERRORBOX);
  try
  {$endif}

  if Trim(AMask) = '' then MaskStr := AllFilesMask
  else MaskStr := AMask;

  // The string list implements support for multiple masks separated
  // by semi-comma ";"
  MaskStrings := TStringList.Create;
  FileTree:=TAvgLvlTree.Create(@STVCompareFiles);
  try
    MaskStrings.Delimiter := ';';
    MaskStrings.DelimitedText := MaskStr;

    if AFileSortType=fstNone then Files:=nil
    else Files:=TList.Create;

    j:=0;
    for i := 0 to MaskStrings.Count - 1 do
    begin
      if MaskStrings.IndexOf(MaskStrings[i]) < i then Continue; // From patch from bug 17761: TShellListView Mask: duplicated items if mask is " *.ext;*.ext "
      SearchStr := IncludeTrailingPathDelimiter(ABaseDir) + MaskStrings.Strings[i];

      FindResult := FindFirstUTF8(SearchStr, faAnyFile, DirInfo);

      while FindResult = 0 do
      begin
        inc(j);
        if j=100 then
        begin
          Application.ProcessMessages;
          j:=0;
        end;

        ShortFilename := DirInfo.Name;

        IsDirectory := (DirInfo.Attr and FaDirectory = FaDirectory);

        IsValidDirectory := (ShortFilename <> '.') and (ShortFilename <> '..');

        IsHidden := (DirInfo.Attr and faHidden = faHidden);
        //LinuxToWinAttr already does this in FF/FN
        //{$IFDEF Unix}
        //if (DirInfo.Name<>'') and (DirInfo.Name[1]='.') then
        //  IsHidden:=true;
        //{$ENDIF}

        // First check if we show hidden files
        if IsHidden then AddFile := (otHidden in AObjectTypes)
        else AddFile := True;

        // If it is a directory, check if it is a valid one
        if IsDirectory then
          AddFile := AddFile and ((otFolders in AObjectTypes) and IsValidDirectory)
        else
          AddFile := AddFile and (otNonFolders in AObjectTypes);

        // AddFile identifies if the file is valid or not
        if AddFile then
        begin
          if not Assigned(Files) then begin
            // Mark if it is a directory (ObjectData <> nil)
            if IsDirectory then ObjectData := AResult
            else ObjectData := nil;
            if FileTree.Find(Pointer(ShortFilename))=nil then
            begin
              // From patch from bug 17761: TShellListView Mask: duplicated items if mask is " *.ext;*.ext "
              FileTree.Add(Pointer(ShortFilename));
              AResult.AddObject(ShortFilename, ObjectData);
            end;
          end else
            Files.Add ( TFileItem.Create(DirInfo));
        end;

        FindResult := FindNextUTF8(DirInfo);
      end;

      FindCloseUTF8(DirInfo);
    end;
  finally
    FileTree.Free;
    MaskStrings.Free;
  end;

  if Assigned(Files) then begin
    Objectdata:=AResult;

    case AFileSortType of
      fstAlphabet:     Files.Sort(@FilesSortAlphabet);
      fstFoldersFirst: Files.Sort(@FilesSortFoldersFirst);
    end;

    for i:=0 to Files.Count-1 do
    begin
      FileItem:=TFileItem(Files[i]);
      if (i < Files.Count - 1) and (TFileItem(Files[i]).Name = TFileItem(Files[i + 1]).Name) then
        Continue; // cause Files is sorted // From patch from bug 17761: TShellListView Mask: duplicated items if mask is " *.ext;*.ext "
      if FileItem.isFolder then
        AResult.AddObject(FileItem.Name, ObjectData)
      else
        AResult.AddObject(FileItem.Name, nil);
    end;
    for i:=0 to Files.Count-1 do
      TFileItem(Files[i]).Free;
    Files.Free;
  end;

  {$if defined(windows) and not defined(wince)}
  finally
     SetErrorMode(ErrMode);
  end;
  {$endif}
end;

class function TCustomShellTreeView.GetBasePath: string;
begin
  {$if defined(windows) and not defined(wince)}
  Result := '';
  {$endif}
  {$ifdef wince}
  Result := '\';
  {$endif}
  {$ifdef unix}
  Result := '/';
  {$endif}
end;

function TCustomShellTreeView.GetRootPath: string;
begin
  if FRoot <> '' then
    Result := FRoot
  else
    Result := GetBasePath();
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

{ Returns true if at least one item was added, false otherwise }
function TCustomShellTreeView.PopulateTreeNodeWithFiles(
  ANode: TTreeNode; ANodePath: string): Boolean;
var
  i: Integer;
  Files: TStringList;
  NewNode: TTreeNode;

   function HasSubDir(Const ADir: String): Boolean;
   var
     SR: TSearchRec;
     FindRes: LongInt;
     Attr: Longint;
     IsHidden: Boolean;
   begin
     Result:=False;
     try
       Attr := faDirectory;
       if (otHidden in fObjectTypes) then Attr := Attr or faHidden;
       FindRes := FindFirstUTF8(AppendPathDelim(ADir) + AllFilesMask, Attr , SR);
       while (FindRes = 0) do
       begin
         if ((SR.Attr and faDirectory <> 0) and (SR.Name <> '.') and
            (SR.Name <> '..')) then
         begin
           IsHidden := ((Attr and faHidden) > 0);
           //LinuxToWinAttr already does this in FF/FN
           //{$IFDEF Unix}
           //if (SR.Name<>'') and (SR.Name[1]='.') then
           //  IsHidden := True;
           //{$ENDIF}
           if not (IsHidden and (not ((otHidden in fObjectTypes)))) then
           begin
             Result := True;
             Break;
           end;
         end;
         FindRes := FindNextUtf8(SR);
       end;
     finally
       FindCloseUTF8(SR);
     end; //try
   end;

begin
  Result := False;
  // avoids crashes in the IDE by not populating during design
  if (csDesigning in ComponentState) then Exit;

  Files := TStringList.Create;
  try
    GetFilesInDir(ANodePath, AllFilesMask, FObjectTypes, Files, FFileSortType);
    Result := Files.Count > 0;

    for i := 0 to Files.Count - 1 do
    begin
      NewNode := Items.AddChildObject(ANode, Files.Strings[i], nil); //@Files.Strings[i]);
      // This marks if the node is a directory (not wether or not there are files in the folder!)
      // We need this info (is it a folder?) elsewhere.
      if (fObjectTypes * [otNonFolders] = []) then
        NewNode.HasChildren := ((Files.Objects[i] <> nil) and
                               HasSubDir(AppendpathDelim(ANodePath)+Files[i]))
      else
        NewNode.HasChildren := (Files.Objects[i] <> nil);
    end;
  finally
    Files.Free;
  end;
end;

procedure TCustomShellTreeView.PopulateWithBaseFiles;
{$if defined(windows) and not defined(wince)}
const
  DRIVE_UNKNOWN = 0;
  DRIVE_NO_ROOT_DIR = 1;
  DRIVE_REMOVABLE = 2;
  DRIVE_FIXED = 3;
  DRIVE_REMOTE = 4;
  DRIVE_CDROM = 5;
  DRIVE_RAMDISK = 6;
var
  r: LongWord;
  Drives: array[0..128] of char;
  pDrive: PChar;
  NewNode: TTreeNode;
begin
  // avoids crashes in the IDE by not populating during design
  if (csDesigning in ComponentState) then Exit;

  r := GetLogicalDriveStrings(SizeOf(Drives), Drives);
  if r = 0 then Exit;
  if r > SizeOf(Drives) then Exit;
//    raise Exception.Create(SysErrorMessage(ERROR_OUTOFMEMORY));

  pDrive := Drives;
  while pDrive^ <> #0 do
  begin
//    r := GetDriveType(pDrive);

    NewNode := Items.AddChildObject(nil, ExcludeTrailingBackslash(pDrive), pDrive);
    NewNode.HasChildren := True;

    Inc(pDrive, 4);
  end;
end;
{$else}
var
  NewNode: TTreeNode;
begin
  // avoids crashes in the IDE by not populating during design
  // also do not populate before loading is done
  if ([csDesigning, csLoading] * ComponentState <> []) then Exit;

  // This allows showing "/" in Linux, but in Windows it makes no sense to show the base
  if GetBasePath() <> '' then
  begin
    NewNode := Items.AddChild(nil, GetBasePath());
    NewNode.HasChildren := True;
    PopulateTreeNodeWithFiles(NewNode, GetBasePath());
    NewNode.Expand(False);
  end
  else
    PopulateTreeNodeWithFiles(nil, GetBasePath());
end;
{$endif}

procedure TCustomShellTreeView.DoSelectionChanged;
var
  ANode: TTreeNode;
  IsDirectory, MustBeDirectory: Boolean;
  CurrentNodePath: String;
begin
  inherited DoSelectionChanged;
  ANode := Selected;
  if Assigned(FShellListView) and Assigned(ANode) then
  begin
    //You cannot rely on HasChildren here, because it can become FALSE when user
    //clicks the expand sign and folder is empty
    //Issue 0027571
    MustBeDirectory := not (otNonFolders in FObjectTypes);
    CurrentNodePath := ChompPathDelim(GetPathFromNode(ANode));
    IsDirectory := MustBeDirectory or DirectoryExistsUtf8(CurrentNodePath);
    if IsDirectory then
    begin
      //Note: the folder may have been deleted in the mean time
      //an exception will be raised by the next line in that case
      FShellListView.Root := GetPathFromNode(ANode)
    end
    else
    begin
      //At this point we cannot tell if item used to be a folder or a file
      if not FileExistsUtf8(CurrentNodePath) then
        Raise EShellCtrl.CreateFmt(sShellCtrlsSelectedItemDoesNotExists,[CurrentNodePath]);
      if Assigned(Anode.Parent) then
        FShellListView.Root := GetPathFromNode(ANode.Parent)
      else
        FShellListView.Root := '';
    end;
  end;
end;

function TCustomShellTreeView.GetPathFromNode(ANode: TTreeNode): string;
begin
  Result := '';
  if Assigned(ANode) then
  begin
    // Build the path. In the future use ANode.Data instead of ANode.Text
    if (ANode.Parent = nil) and (GetRootPath <> '') then
      //This node is RootNode and GetRooPath contains fully qualified root path
      Result := ''
    else
      Result := ANode.Text;
    while (ANode.Parent <> nil) do
    begin
      ANode := ANode.Parent;
      if (ANode.Parent = nil) and (GetRootPath <> '') then
        //Node.Text of rootnode may not be fully qualified
        Result := GetRootPath + Result
      else
        Result := IncludeTrailingPathDelimiter(ANode.Text) + Result;
    end;
    if not FilenameIsAbsolute(Result) then
      Result := GetRootPath() + Result;    // Include root directory
  end;
end;

function TCustomShellTreeView.GetSelectedNodePath: string;
begin
  Result := GetPathFromNode(Selected);
end;

procedure TCustomShellTreeView.Refresh(ANode: TTreeNode);
//nil will refresh root
var
  RootNodeText: String;
  IsRoot: Boolean;
begin
  if (Items.Count = 0) then Exit;
  //writeln('GetFirstVisibleNode.Text = "',Items.GetFirstVisibleNode.Text,'"');
  IsRoot := (ANode = nil) or ((ANode = Items.GetFirstVisibleNode) and (GetRootPath <> ''));
  //writeln('IsRoot = ',IsRoot);
  if (ANode = nil) and (GetRootPath <> '') then ANode := Items.GetFirstVisibleNode;
  if IsRoot then
  begin
    if Assigned(ANode) then
      RootNodeText := ANode.Text  //this may differ from FRoot, so don't use FRoot here
    else
      RootNodeText := GetRootPath;
    //writeln('IsRoot = TRUE, RootNodeText = "',RootNodeText,'"');
    FRoot := #0; //invalidate FRoot
    SetRoot(RootNodeText); //re-initialize the entire tree
  end
  else
  begin
    ANode.Expand(False);
  end;
end;

function TCustomShellTreeView.GetPath: string;
begin
  Result := GetPathFromNode(Selected);
end;

{
SetPath: Path can be
- Absolute like '/usr/lib'
- Relative like 'foo/bar'
  This can be relative to:
  - Self.Root (which takes precedence over)
  - Current directory
}
procedure TCustomShellTreeView.SetPath(AValue: string);
var
  sl: TStringList;
  Node: TTreeNode;
  i: integer;
  FQRootPath, RelPath: String;
  RootIsAbsolute: Boolean;
  IsRelPath: Boolean;

  function GetAdjustedNodeText(ANode: TTreeNode): String;
  begin
    if (ANode = Items.GetFirstVisibleNode) and (FQRootPath <> '') then
    begin
      if not RootIsAbsolute then
        Result := ''
      else
        Result := FQRootPath;
    end
    else Result := ANode.Text;
  end;

  function Exists(Fn: String): Boolean;
  //Fn should be fully qualified
  var
    Attr: LongInt;
    Dirs: TStringList;
    i: Integer;
  begin
    Result := False;
    Attr := FileGetAttrUtf8(Fn);
    //writeln('TCustomShellTreeView.SetPath.Exists: Attr = ', Attr);
    if (Attr = -1) then Exit;
    if not (otNonFolders in FObjectTypes) then
      Result := ((Attr and faDirectory) > 0)
    else
      Result := True;
    //writeln('TCustomShellTreeView.SetPath.Exists: Result = ',Result);
  end;

  function PathIsDriveRoot({%H-}Path: String): Boolean;  {$if not (defined(windows) and not defined(wince))}inline;{$endif}
  //At least Win7 reports faHidden on all physical drive-roots (e.g. C:\)
  begin
    {$if defined(windows) and not defined(wince)}
    Result := (Length(Path) = 3) and
              (Upcase(Path[1]) in ['A'..'Z']) and
              (Path[2] = DriveSeparator) and
              (Path[3] in AllowDirectorySeparators);
    {$else}
    Result := False;
    {$endif windows}
  end;

  function ContainsHiddenDir(Fn: String): Boolean;
  var
    i: Integer;
    Attr: LongInt;
    Dirs: TStringList;
    RelPath: String;
  begin
    //if fn=root then always return false
    if (CompareFileNames(Fn, FQRootPath) = 0) then
      Result := False
    else
    begin
      Attr := FileGetAttrUtf8(Fn);
      Result := ((Attr and faHidden) = faHidden) and not PathIsDriveRoot(Fn);
      if not Result then
      begin
        //it also is not allowed that any folder above is hidden
        Fn := ChompPathDelim(Fn);
        Fn := ExtractFileDir(Fn);
        Dirs := TStringList.Create;
        try
          Dirs.StrictDelimiter := True;
          Dirs.Delimiter := PathDelim;
          Dirs.DelimitedText := Fn;
          Fn := '';
          for i := 0 to Dirs.Count - 1 do
          begin
            if (i = 0) then
              Fn := Dirs.Strings[i]
            else
              Fn := Fn + PathDelim + Dirs.Strings[i];
            if (Fn = '') then Continue;
            RelPath := CreateRelativePath(Fn, FQRootPath, False, True);
            //don't check if Fn now is "higher up the tree" than the current root
            if (RelPath = '') or ((Length(RelPath) > 1) and (RelPath[1] = '.') and (RelPath[2] = '.')) then
            begin
              //writeln('Fn is higher: ',Fn);
              Continue;
            end;
            {$if defined(windows) and not defined(wince)}
            if (Length(Fn) = 2) and (Fn[2] = ':') then Continue;
            {$endif}
            Attr := FileGetAttrUtf8(Fn);
            if (Attr <> -1) and ((Attr and faHidden) > 0) and not PathIsDriveRoot(Fn) then
            begin
              Result := True;
              //writeln('TCustomShellTreeView.SetPath.Exists: a subdir is hidden: Result := False');
              Break;
            end;
          end;
        finally
          Dirs.Free;
        end;
      end;
    end;
  end;

begin
  RelPath := '';

  //writeln('SetPath: GetRootPath = "',getrootpath,'"',' AValue=',AValue);

  if (GetRootPath <> '') then
    //FRoot is already Expanded in SetRoot, just add PathDelim if needed
    FQRootPath := AppendPathDelim(GetRootPath)
  else
    FQRootPath := '';
  RootIsAbsolute := (FQRootPath = '') or (FQRootPath = PathDelim)
                    or ((Length(FQRootPath) = 3) and (FQRootPath[2] = ':') and (FQRootPath[3] = PathDelim));

  //writeln('SetPath: FQRootPath = ',fqrootpath);
  //writeln('SetPath: RootIsAbsolute = ',RootIsAbsolute);

  //IsRelPath := not FileNameIsAbsolute(AValue);

  //writeln('SetPath: IsRelPath = ',not FileNameIsAbsolute(AValue));

  if not FileNameIsAbsolute(AValue) then
  begin
    if Exists(FQRootPath + AValue) then
    begin
      //Expand it, since it may be in the form of ../../foo
      AValue := ExpandFileNameUtf8(FQRootPath + AValue);
    end
    else
    begin
      //don't expand Avalue yet, we may need it in error message
      if not Exists(ExpandFileNameUtf8(AValue)) then
        Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPath,[ExpandFileNameUtf8(FQRootPath + AValue)]);
      //Directory (or file) exists
      //Make it fully qualified
      AValue := ExpandFileNameUtf8(AValue);
    end;
  end
  else
  begin
    //AValue is an absoulte path to begin with
    //if not DirectoryExistsUtf8(AValue) then
    if not Exists(AValue) then
      Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPath,[AValue]);
  end;

  //AValue now is a fully qualified path and it exists
  //Now check if it is a subdirectory of FQRootPath
  //RelPath := CreateRelativePath(AValue, FQRootPath, False);
  IsRelPath := (FQRootPath = '') or TryCreateRelativePath(AValue, FQRootPath, False, True, RelPath);

  //writeln('TCustomShellTreeView.SetPath: ');
  //writeln('  IsRelPath = ',IsRelPath);
  //writeln('  RelPath = "',RelPath,'"');
  //writeln('  FQRootPath = "',FQRootPath,'"');


  if (not IsRelpath) or ((RelPath <> '') and ((Length(RelPath) > 1) and (RelPath[1] = '.') and (RelPath[2] = '.'))) then
  begin
    // CreateRelativePath retruns a string beginning with ..
    // so AValue is not a subdirectory of FRoot
    Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPathRelative,[AValue, FQRootPath]);
  end;

  if (RelPath = '') and (FQRootPath = '') then
    RelPath := AValue;
  //writeln('RelPath = ',RelPath);
  if (RelPath = '') then
  begin
    //writeln('Root selected');
    Node := Items.GetFirstVisibleNode;
    if Assigned(Node) then
    begin
      Node.Expanded := True;
      Node.Selected := True;
    end;
    Exit;
  end;

  if not (otHidden in FObjectTypes) and ContainsHiddenDir(AValue) then
    Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPath,[AValue, FQRootPath]);

  sl := TStringList.Create;
  sl.Delimiter := PathDelim;
  sl.StrictDelimiter := True;
  sl.DelimitedText := RelPath;
  if (sl.Count > 0) and (sl[0] = '') then  // This happens when root dir is empty
    sl[0] := PathDelim;                    //  and PathDelim was the first char
  if (sl.Count > 0) and (sl[sl.Count-1] = '') then sl.Delete(sl.Count-1); //remove last empty string
  if (sl.Count = 0) then
  begin
    sl.Free;
    Exit;
  end;

  //for i := 0 to sl.Count - 1 do writeln('sl[',i:2,']="',sl[i],'"');


  BeginUpdate;
  try
    Node := Items.GetFirstVisibleNode;
    //if assigned(node) then writeln('GetFirstVisibleNode = ',GetAdjustedNodeText(Node));
    //Root node doesn't have Siblings in this case, we need one level deeper
    if (GetRootPath <> '') and Assigned(Node) then
    begin
      //writeln('Root node doesn''t have Siblings');
      Node := Node.GetFirstVisibleChild;
      //writeln('Node = ',GetAdjustedNodeText(Node));
      //if RootIsAbsolute then sl.Delete(0);
    end;

    for i := 0 to sl.Count-1 do
    begin
      {
      write('i=',i,' sl[',i,']=',sl[i],' ');
      if Node <> nil then write('GetAdjustedNodeText = ',GetAdjustedNodeText(Node))
      else  write('Node = NIL');
      writeln;
      }
      while (Node <> Nil) and
            {$IF defined(CaseInsensitiveFilenames) or defined(NotLiteralFilenames)}
            (Utf8LowerCase(GetAdjustedNodeText(Node)) <> Utf8LowerCase(sl[i]))
            {$ELSE}
            (GetAdjustedNodeText(Node) <> sl[i])
            {$ENDIF}
            do
            begin
              //write('  i=',i,' "',GetAdjustedNodeText(Node),' <> ',sl[i],' -> GetNextVisibleSibling -> ');
              Node := Node.GetNextVisibleSibling;
              {
              if Node <> nil then write('GetAdjustedNodeText = ',GetAdjustedNodeText(Node))
              else  write('Node = NIL');
              writeln;
              }
            end;
      if Node <> Nil then
      begin
        Node.Expanded := True;
        Node.Selected := True;
        Node := Node.GetFirstVisibleChild;
      end
      else
        Break;
    end;
  finally
    sl.free;
    EndUpdate;
  end;
end;


{ TCustomShellListView }

procedure TCustomShellListView.SetShellTreeView(
  const Value: TCustomShellTreeView);
var
  Tmp: TCustomShellTreeView;
begin
  if FShellTreeView = Value then Exit;
  if FShellTreeView <> nil then
  begin
    Tmp := FShellTreeView;
    FShellTreeView := nil;
    Tmp.ShellListView := nil;
  end;

  FShellTreeView := Value;

  if not (csDestroying in ComponentState) then
    Clear;

  if Value <> nil then
  begin
    FRoot := Value.GetPathFromNode(Value.Selected);
    PopulateWithRoot();

    // Also update the pair, but only if necessary to avoid circular calls of the setters
    if Value.ShellListView <> Self then Value.ShellListView := Self;
  end;

end;

procedure TCustomShellListView.SetMask(const AValue: string);
begin
  if AValue <> FMask then
  begin
    FMask := AValue;
    Clear;
    Items.Clear;
    PopulateWithRoot();
  end;
end;

procedure TCustomShellListView.SetRoot(const Value: string);
begin
  if FRoot <> Value then
  begin
    //Delphi raises an unspecified exception in this case, but don't crash the IDE at designtime
    if not (csDesigning in ComponentState)
       and (Value <> '')
       and not DirectoryExistsUtf8(ExpandFilenameUtf8(Value)) then
       Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidRoot,[Value]);
    FRoot := Value;
    Clear;
    Items.Clear;
    PopulateWithRoot();
  end;
end;

constructor TCustomShellListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Initial property values
  ViewStyle := vsReport;
  ObjectTypes := [otNonFolders];

  Self.Columns.Add;
  Self.Columns.Add;
  Self.Columns.Add;
  Self.Column[0].Caption := sShellCtrlsName;
  Self.Column[1].Caption := sShellCtrlsSize;
  Self.Column[2].Caption := sShellCtrlsType;
  // Initial sizes, necessary under Windows CE
  Resize;
end;

destructor TCustomShellListView.Destroy;
begin
  ShellTreeView := nil;
  inherited Destroy;
end;

procedure TCustomShellListView.PopulateWithRoot();
var
  i: Integer;
  Files: TStringList;
  NewItem: TListItem;
  CurFileName, CurFilePath: string;
  CurFileSize: Int64;
begin
  // avoids crashes in the IDE by not populating during design
  if (csDesigning in ComponentState) then Exit;

  // Check inputs
  if Trim(FRoot) = '' then Exit;

  Files := TStringList.Create;
  try
    TCustomShellTreeView.GetFilesInDir(FRoot, FMask, FObjectTypes, Files);

    for i := 0 to Files.Count - 1 do
    begin
      NewItem := Items.Add;
      CurFileName := Files.Strings[i];
      CurFilePath := IncludeTrailingPathDelimiter(FRoot) + CurFileName;
      // First column - Name
      NewItem.Caption := CurFileName;
      // Second column - Size
      // The raw size in bytes is stored in the data part of the item
      CurFileSize := FileSize(CurFilePath); // in Bytes
      NewItem.Data := Pointer(PtrInt(CurFileSize));
      if CurFileSize < 1024 then
        NewItem.SubItems.Add(Format(sShellCtrlsBytes, [IntToStr(CurFileSize)]))
      else if CurFileSize < 1024 * 1024 then
        NewItem.SubItems.Add(Format(sShellCtrlsKB, [IntToStr(CurFileSize div 1024)]))
      else
        NewItem.SubItems.Add(Format(sShellCtrlsMB, [IntToStr(CurFileSize div (1024 * 1024))]));
      // Third column - Type
      NewItem.SubItems.Add(ExtractFileExt(CurFileName));
    end;
    Sort;
  finally
    Files.Free;
  end;
end;

procedure TCustomShellListView.Resize;
begin
  inherited Resize;
  {$ifdef DEBUG_SHELLCTRLS}
    WriteLn(':>TCustomShellListView.HandleResize');
  {$endif}

  // The correct check is with count,
  // if Column[0] <> nil then
  // will raise an exception
  if Self.Columns.Count < 3 then Exit;

  // If the space available is small,
  // alloc a larger percentage to the secondary
  // fields
  if Width < 400 then
  begin
    Column[0].Width := (50 * Width) div 100;
    Column[1].Width := (25 * Width) div 100;
    Column[2].Width := (25 * Width) div 100;
  end
  else
  begin
    Column[0].Width := (70 * Width) div 100;
    Column[1].Width := (15 * Width) div 100;
    Column[2].Width := (15 * Width) div 100;
  end;

  {$ifdef DEBUG_SHELLCTRLS}
    WriteLn(':<TCustomShellListView.HandleResize C0.Width=',
     Column[0].Width, ' C1.Width=', Column[1].Width,
     ' C2.Width=', Column[2].Width);
  {$endif}
end;

function TCustomShellListView.GetPathFromItem(ANode: TListItem): string;
begin
  Result := IncludeTrailingPathDelimiter(FRoot) + ANode.Caption;
end;

procedure Register;
begin
  RegisterComponents('Misc',[TShellTreeView, TShellListView]);
end;

end.
