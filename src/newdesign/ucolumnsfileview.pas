unit uColumnsFileView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, StdCtrls, ExtCtrls, Grids,
  LMessages, LCLIntf, LCLType, Menus, syncobjs,
  uDragDropEx,
  uFile,
  uFileView,
  uFileSource,
  uFileSourceListOperation,
  uColumnsFileViewFiles,
  uColumns,
  uFileSorting,
  uFunctionThread,
  uXmlConfig,
  uClassesEx
  ;

//{$DEFINE timeFileView}

type

  { Sorting }

  TFileListSortingColumn = record
    iField : Integer;
    SortDirection : TSortDirection;
  end;

  PFileListSortingColumn = ^TFileListSortingColumn;

  TFileListSorting = class(TList)
  public
    Destructor Destroy; override;
    function Clone: TFileListSorting;
    procedure AddSorting(iField : Integer; SortDirection : TSortDirection);
    procedure Clear; override;
    function GetSortingDirection(iField : Integer) : TSortDirection;
  end;

  PFileListSorting = ^TFileListSorting;

  TFileListBuilderState = (rfsNone, rfsLoadingFiles, rfsLoadingIcons);

  TColumnsFileView = class;
  TColumnsFileListBuilder = class;

  { TDrawGridEx }

  TDrawGridEx = class(TDrawGrid)
  private
    // Used to register as a drag and drop source and target.
    DragDropSource: uDragDropEx.TDragDropSource;
    DragDropTarget: uDragDropEx.TDragDropTarget;

    StartDrag: Boolean;
    DragStartPoint: TPoint;
    DragRowIndex,
    DropRowIndex,
    HintRowIndex: Integer;
    LastMouseButton: TMouseButton; // Mouse button that initiated dragging
    FMouseDown: Boolean; // Used to check if button-up was received after button-down
                         // or after dropping something after dragging with right mouse button

    ColumnsView: TColumnsFileView;

    // Updates the drop row index, which is used to draw a rectangle
    // on directories during drag&drop operations.
    procedure ChangeDropRowIndex(NewIndex: Integer);

    // Simulates releasing mouse button that started a dragging operation,
    // but was released in another window or another application.
    procedure ClearMouseButtonAfterDrag;

    // If internal dragging is currently in effect, this function
    // stops internal dragging and starts external.
    procedure TransformDraggingToExternal(ScreenPoint: TPoint);

    { Events for drag&drop from external applications }
    function OnExDragBegin: Boolean;
    function OnExDragEnd  : Boolean;
    function OnExDragEnter(var DropEffect: TDropEffect; ScreenPoint: TPoint):Boolean;
    function OnExDragOver(var DropEffect: TDropEffect; ScreenPoint: TPoint):Boolean;
    function OnExDrop(const FileNamesList: TStringList; DropEffect: TDropEffect; ScreenPoint: TPoint):Boolean;
    function OnExDragLeave:Boolean;

  protected

    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;

    procedure InitializeWnd; override;
    procedure FinalizeWnd; override;

    procedure DrawCell(aCol, aRow: Integer; aRect: TRect;
              aState: TGridDrawState); override;

  public
{$IFDEF LCLGTK2}
    fLastDoubleClickTime : TDateTime;

    function TooManyDoubleClicks: Boolean;
{$ENDIF}
    constructor Create(AOwner: TComponent; AParent: TWinControl); reintroduce;
    destructor Destroy; override;

    procedure UpdateView;

    // Returns height of all the header rows.
    function GetHeaderHeight: Integer;
  end;

  { TPathLabel }

  TPathLabel = class(TLabel)
  private
    HighlightStartPos: Integer;
    HighlightText: String;
    {en
       How much space to leave between the text and left border.
    }
    FLeftSpacing: Integer;

    {en
       If a user clicks on a parent directory of the path,
       this stores the full path of that parent directory.
    }
    SelectedDir: String;

    {en
       If a mouse if over some parent directory of the currently displayed path,
       it is highlighted, so that user can click on it.
    }
    procedure Highlight(MousePosX, MousePosY: Integer);

    procedure MouseEnterEvent(Sender: TObject);
    procedure MouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseLeaveEvent(Sender: TObject);

  public

    constructor Create(AOwner: TComponent; AllowHighlight: Boolean = False); reintroduce;

    procedure Paint; override;

    {en
       Changes drawing colors depending active/inactive state.
    }
    procedure SetActive(Active: Boolean);

    function GetSelectedDir: String;

    property LeftSpacing: Integer read FLeftSpacing write FLeftSpacing;
  end;

  { TColumnsFileView }

  TColumnsFileView = class(TFileView)

  private
    FFiles: TColumnsViewFiles;  //<en List of displayed files
    FFileSourceFiles: TFiles;   //<en List of files from file source
    FListFilesThread: TFunctionThread;
    FReloading: Boolean;        //<en If currently reloading file list

    FFileListBuilders: TFPList; // of TColumnsFileListBuilder
    FFileListBuilderLock: TCriticalSection;
    {en
       Points to currently used builder while the file list is being loaded.
    }
    FCurrentFileListBuilder: TColumnsFileListBuilder;
    FSelection: TStringListEx;

    FActive: Boolean;           //<en Is this view active
    FLastActive: String;        //<en Last active file
    FLastActiveRow: Integer;    //<en Last active row
    FLastMark: String;
    FLastSelectionStartRow: Integer;
    FLastSelectionState: Boolean;
    fSearchDirect,
    fNext,
    fPrevious : Boolean;
    FFileFilter: String;

    FSorting: TFileListSorting;
    FSortColumn: Integer;
    FSortDirection: TSortDirection;

    pnlFooter: TPanel;
    lblInfo: TLabel;
    pnlHeader: TPanel;
    pmColumnsMenu: TPopupMenu;
    pnAltSearch: TPanel;
    pnlFilter: TPanel;
    edtSearch: TEdit;
    edtFilter: TEdit;
    btnCloseFilter: TButton;
    edtPath: TEdit;
    edtRename: TEdit;
    lblPath: TPathLabel;
    lblAddress: TPathLabel;
    dgPanel: TDrawGridEx;
    tmContextMenu: TTimer;

    function GetGridHorzLine: Boolean;
    function GetGridVertLine: Boolean;
    procedure SetGridHorzLine(const AValue: Boolean);
    procedure SetGridVertLine(const AValue: Boolean);
    function GetColumnsClass: TPanelColumnsClass;
    function GetActiveItem: TColumnsViewFile;
    {en
       Returns True if item is not nil and not '..'.
       May be extended to include other conditions.
    }
    function IsItemValid(AFile: TColumnsViewFile): Boolean;
    function IsActiveItemValid: Boolean;
    {en
       Returns True if there are no files shown in the panel.
    }
    function IsEmpty: Boolean;

    procedure SetFileFilter(NewFilter: String);
    {en
       Changes drawing colors depending on if this panel is active.
    }
    procedure SetActive(bActive: Boolean);

    function StartDragEx(MouseButton: TMouseButton; ScreenStartPoint: TPoint): Boolean;
    procedure ChooseFile(AFile: TColumnsViewFile; FolderMode: Boolean = False);

    procedure UpdateAddressLabel;
    procedure UpdatePathLabel;
    procedure UpdateInfoPanel;
    procedure UpdateColCount(NewColCount: Integer);
    procedure SetColumnsWidths;
    procedure RedrawGrid;

    procedure MakeVisible(iRow: Integer);
    procedure MakeSelectedVisible;
    procedure SelectFile(AFile: TColumnsViewFile);
    procedure SelectRange(iRow: PtrInt);
    procedure MarkFile(AFile: TColumnsViewFile; bMarked: Boolean);
    procedure MarkAllFiles(bMarked: Boolean);
    procedure InvertFileSelection(AFile: TColumnsViewFile);
    procedure MarkGroup(const sMask: String; bSelect: Boolean);
    procedure InvertAll;
    procedure MarkAll;
    procedure UnMarkAll;
    procedure MarkMinus;
    procedure MarkPlus;
    procedure MarkShiftPlus;
    procedure MarkShiftMinus;
    procedure SaveSelection;
    procedure RestoreSelection;

    {en
       Retrieves file list from file source into FFileSourceFiles.
       Either runs directly or starts a new thread.
    }
    procedure MakeFileSourceFileList;
    {en
       Executes those parts of making a new file list
       that must be run from GUI thread.
    }
    procedure AfterMakeFileList;
    {en
       Updates GUI after the display file list has changed.
    }
    procedure DisplayFileListHasChanged;
    {en
       Makes a new display file list and redisplays the changed list.
    }
    procedure ReDisplayFileList;

    {en
       Sorts files in FilesToSort using current sorting.
       It must be called from main thread.
    }
    procedure Sort(FilesToSort: TFiles);
    {en
       Sorts files in FilesToSort using a previously prepared sorting.
       This function may be called from a worker thread.
    }
    class procedure Sort(FilesToSort: TFiles; ASortings: TFileSortings); overload;
    {en
       Prepares sortings for later use in Sort function.
       This function must be called from main thread.
    }
    function PrepareSortings: TFileSortings;
    procedure SortByColumn(iColumn: Integer);

    procedure ShowRenameFileEdit(const sFileName:String);
    procedure ShowPathEdit;
    procedure ShowSearchPanel(Char : TUTF8Char = #0);
    procedure CloseSearchPanel;
    procedure ShowFilterPanel(Char : TUTF8Char = #0);
    procedure FilterPanelVisible;
    procedure CloseFilterPanel;

    procedure CalculateSpaceOfAllDirectories;
    procedure CalculateSpace(theFile: TColumnsViewFile);

    function DimColor(AColor: TColor): TColor;

    procedure DoOnReload;

    // -- Events --------------------------------------------------------------

    procedure edtPathExit(Sender: TObject);
    procedure edtSearchExit(Sender: TObject);
    procedure edtRenameExit(Sender: TObject);
    procedure edtFilterEnter(Sender: TObject);
    procedure edtFilterExit(Sender: TObject);

    procedure edtSearchChange(Sender: TObject);
    procedure edtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtPathKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtRenameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtFilterChange(Sender: TObject);
    procedure edtFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure btnCloseFilterClick(Sender: TObject);

    procedure dgPanelEnter(Sender: TObject);
    procedure dgPanelExit(Sender: TObject);
    procedure dgPanelDblClick(Sender: TObject);
    procedure dgPanelKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dgPanelKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dgPanelMouseLeave(Sender: TObject);
    procedure dgPanelMouseDown(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
    procedure dgPanelMouseMove(Sender: TObject; Shift: TShiftState;
                               X, Y: Integer);
    procedure dgPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgPanelStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure dgPanelDragOver(Sender, Source: TObject; X, Y: Integer;
                                               State: TDragState; var Accept: Boolean);
    procedure dgPanelDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure dgPanelEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure dgPanelHeaderClick(Sender: TObject;IsColumn: Boolean; index: Integer);
    procedure dgPanelMouseWheelUp(Sender: TObject; Shift: TShiftState;
                                  MousePos: TPoint; var Handled: Boolean);
    procedure dgPanelMouseWheelDown(Sender: TObject; Shift: TShiftState;
                                  MousePos: TPoint; var Handled: Boolean);
    procedure dgPanelSelection(Sender: TObject; aCol, aRow: Integer);
    procedure dgPanelShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure tmContextMenuTimer(Sender: TObject);
    procedure lblPathClick(Sender: TObject);
    procedure lblPathMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlHeaderResize(Sender: TObject);
    procedure ColumnsMenuClick(Sender: TObject);

    procedure UTF8KeyPressEvent(Sender: TObject; var UTF8Key: TUTF8Char);

    property FileFilter: String read FFileFilter write SetFileFilter;

  protected
    procedure CreateDefault(AOwner: TWinControl); override;

    procedure SetCurrentPath(NewPath: String); override;
    function GetActiveFile: TFile; override;
    function GetDisplayedFiles: TFiles; override;
    function GetSelectedFiles: TFiles; override;

    property LastActive: String read FLastActive write FLastActive;

  public
    ActiveColm: String;
    ActiveColmSlave: TPanelColumnsClass;
    isSlave:boolean;
//---------------------

    constructor Create(AOwner: TWinControl; AFileSource: IFileSource; APath: String); override;
    constructor Create(AOwner: TWinControl; AFileView: TFileView); override;
    constructor Create(AOwner: TWinControl; AConfig: TIniFileEx; ASectionName: String; ATabIndex: Integer); override;
    constructor Create(AOwner: TWinControl; AConfig: TXmlConfig; ANode: TXmlNode); override;

    destructor Destroy; override;

    function Clone(NewParent: TWinControl): TColumnsFileView; override;
    procedure CloneTo(FileView: TFileView); override;

    procedure AddFileSource(aFileSource: IFileSource; aPath: String); override;
    procedure RemoveLastFileSource; override;

    procedure Reload(const PathsToReload: TPathsArray = nil); override;
    procedure StopBackgroundWork; override;

    procedure LoadConfiguration(Section: String; TabIndex: Integer); override;
    procedure SaveConfiguration(Section: String; TabIndex: Integer); override;
    procedure LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); override;
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); override;

    function Focused: Boolean; override;
    procedure SetFocus; override;

    {en
       Moves the selection focus to the file specified by FileName.
    }
    procedure SetActiveFile(const aFileName: String); override;

    procedure UpdateColumnsView;
    procedure UpdateView; override;

    function HasSelectedFiles: Boolean; override;

    procedure DoDragDropOperation(Operation: TDragDropOperation;
                                  var DropParams: TDropParams); override;

    property CurrentPath: String read GetCurrentPath write SetCurrentPath;
    property GridVertLine: Boolean read GetGridVertLine write SetGridVertLine;
    property GridHorzLine: Boolean read GetGridHorzLine write SetGridHorzLine;

  published  // commands
    procedure cm_MarkInvert(param: string='');
    procedure cm_MarkMarkAll(param: string='');
    procedure cm_MarkUnmarkAll(param: string='');
    procedure cm_MarkPlus(param: string='');
    procedure cm_MarkMinus(param: string='');
    procedure cm_MarkCurrentExtension(param: string='');
    procedure cm_UnmarkCurrentExtension(param: string='');
    procedure cm_SaveSelection(param: string='');
    procedure cm_RestoreSelection(param: string='');
    procedure cm_SaveSelectionToFile(param: string='');
    procedure cm_LoadSelectionFromFile(param: string='');
    procedure cm_LoadSelectionFromClip(param: string='');
    procedure cm_QuickSearch(param: string='');
    procedure cm_QuickFilter(param: string='');
    procedure cm_Open(param: string='');
    procedure cm_ReverseOrder(param:string);
    procedure cm_SortByColumn(param: string='');
    procedure cm_CountDirContent(param: string='');
    procedure cm_RenameOnly(param: string='');
    procedure cm_ContextMenu(param: string='');
    procedure cm_EditPath(param: string='');
  end;

  { TColumnsFileListBuilder }

  TColumnsFileListBuilder = class
  private
    FTmpFileSourceFiles: TFiles;
    FTmpDisplayFiles: TColumnsViewFiles;
    FAborted: Boolean;
    FFileIndexToUpdate: Integer;
    FState: TFileListBuilderState;
    FIconsToLoad: Integer;

    // Data captured from the columns view before start.
    FColumnsView: TColumnsFileView;
    FFileSource: IFileSource;
    FFileSourcesCount: Integer;
    FFileFilter: String;
    FCurrentPath: String;
    FThread: TThread;
    FSortings: TFileSortings;

    {en
       Fills aFiles with files from aFileSourceFiles.
       Filters out any files that shouldn't be shown using aFileFilter.
    }
    class procedure MakeDisplayFileList(aFileSource: IFileSource;
                                        aFileSourceFiles: TFiles;
                                        aFiles: TColumnsViewFiles;
                                        aFileFilter: String);

    procedure AfterMakeDisplayFileList;

    {en
       Assigns the built lists to the columns view and displays new the file list.
       Must be run from GUI thread.
    }
    procedure SetColumnsFilelist;

    {en
       Load icon of each file from display file list.
    }
    procedure LoadFilesIcons;
    {en
       Redraws a column with a new icon. Must be run from GUI thread.
    }
    procedure UpdateIcon;

    procedure ClearBuilder;

    function IsWorking: Boolean; inline;

  public
    constructor Create;
    procedure Abort;

    {en
       Before starting work the builder must be initialized.
       This function must be called from the main thread just before
       the builder is scheduled to work.
    }
    procedure InitializeBeforeWork(AColumnsView: TColumnsFileView);

    {en
       Retrieves file list from file source, sorts and creates a display file list.
       It may be run from a worker thread so it cannot access GUI directly.
    }
    procedure MakeFileSourceFileList(Params: Pointer);

    property Working: Boolean read IsWorking;
    property State: TFileListBuilderState read FState;
  end;

implementation

uses
  LCLProc, Masks, Dialogs, Clipbrd, uLng, uShowMsg, uGlobs, uPixmapManager,
  uDCUtils, uOSUtils, math, fMain, fMaskInputDlg, uSearchTemplate,
  uInfoToolTip, dmCommonData,
  uFileProperty,
  uFileSourceProperty,
  uFileSourceOperation,
  uFileSourceOperationTypes,
  uFileSourceOperationOptions,
  uFileSourceCalcStatisticsOperation,
  uFileSystemFile,
  fColumnsSetConf,
  uKeyboard,
  uFileSourceUtil
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  , GtkProc  // for ReleaseMouseCapture
  , GTKGlobals  // for DblClickTime
{$ENDIF}
  ;

{$IFDEF timeFileView}
var
  startTime: TDateTime;
{$ENDIF}

function TColumnsFileView.Focused: Boolean;
begin
  Result := Assigned(dgPanel) and dgPanel.Focused;
end;

procedure TColumnsFileView.SetFocus;
begin
  if CanFocus and dgPanel.CanFocus then
    dgPanel.SetFocus;
end;

function TColumnsFileView.GetActiveFile: TFile;
var
  aFile: TColumnsViewFile;
begin
  aFile := GetActiveItem;

  if Assigned(aFile) then
    Result := aFile.TheFile
  else
    Result := nil;
end;

function TColumnsFileView.GetDisplayedFiles: TFiles;
var
  i: Integer;
begin
  Result := FFileSourceFiles.CreateObjectOfSameType(CurrentPath);

  for i := 0 to FFiles.Count - 1 do
  begin
    Result.Add(FFiles[i].TheFile.Clone);
  end;
end;

function TColumnsFileView.GetSelectedFiles: TFiles;
var
  i: Integer;
  aFile: TColumnsViewFile;
begin
  Result := FFileSourceFiles.CreateObjectOfSameType(CurrentPath);

  for i := 0 to FFiles.Count - 1 do
  begin
    if FFiles[i].Selected then
      Result.Add(FFiles[i].TheFile.Clone);
  end;

  // If no files are selected, add currently active file if it is valid.
  if (Result.Count = 0) then
  begin
    aFile := GetActiveItem;
    if IsItemValid(aFile) then
      Result.Add(aFile.TheFile.Clone);
  end;
end;

procedure TColumnsFileView.LoadConfiguration(Section: String; TabIndex: Integer);
var
  ColumnsClass: TPanelColumnsClass;
  SortCount: Integer;
  SortColumn: Integer;
  SortDirection: TSortDirection;
  i: Integer;
  sIndex: String;
begin
  sIndex := IntToStr(TabIndex);

  ActiveColm := gIni.ReadString(Section, sIndex + '_columnsset', 'Default');

  // Load sorting options.
  FSorting.Clear;
  ColumnsClass := GetColumnsClass;
  SortCount := gIni.ReadInteger(Section, sIndex + '_sortcount', 0);
  for i := 0 to SortCount - 1 do
  begin
    SortColumn := gIni.ReadInteger(Section, sIndex + '_sortcolumn' + IntToStr(i), -1);
    if (SortColumn >= 0) and (SortColumn < ColumnsClass.ColumnsCount) then
    begin
      SortDirection := TSortDirection(gIni.ReadInteger(Section, sIndex + '_sortdirection' + IntToStr(i), Integer(sdNone)));
      FSorting.AddSorting(SortColumn, SortDirection);
    end;
  end;
end;

procedure TColumnsFileView.SaveConfiguration(Section: String; TabIndex: Integer);
var
  SortingColumn: PFileListSortingColumn;
  sIndex: String;
  i: Integer;
begin
  sIndex := IntToStr(TabIndex);

  gIni.WriteString(Section, sIndex + '_columnsset', ActiveColm);

  // Save sorting options.
  gIni.WriteInteger(Section, sIndex + '_sortcount', FSorting.Count);
  for i := 0 to FSorting.Count - 1 do
  begin
    SortingColumn := PFileListSortingColumn(FSorting.Items[i]);

    gIni.WriteInteger(Section, sIndex + '_sortcolumn' + IntToStr(i),
                      SortingColumn^.iField);
    gIni.WriteInteger(Section, sIndex + '_sortdirection' + IntToStr(i),
                      Integer(SortingColumn^.SortDirection));
  end;
end;

procedure TColumnsFileView.LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
var
  ColumnsClass: TPanelColumnsClass;
  SortColumn: Integer;
  SortDirection: TSortDirection;
begin
  ActiveColm := AConfig.GetValue(ANode, 'ColumnsSet', 'Default');

  // Load sorting options.
  FSorting.Clear;
  ColumnsClass := GetColumnsClass;
  ANode := ANode.FindNode('Sorting');
  if Assigned(ANode) then
  begin
    ANode := ANode.FirstChild;
    while Assigned(ANode) do
    begin
      if ANode.CompareName('Sort') = 0 then
      begin
        if AConfig.TryGetValue(ANode, 'Column', SortColumn) and
           (SortColumn >= 0) and (SortColumn < ColumnsClass.ColumnsCount) then
        begin
          SortDirection := TSortDirection(AConfig.GetValue(ANode, 'Direction', Integer(sdNone)));
          FSorting.AddSorting(SortColumn, SortDirection);
        end
        else
          DebugLn('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
      end;
      ANode := ANode.NextSibling;
    end;
  end;
end;

procedure TColumnsFileView.SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
var
  SortingColumn: PFileListSortingColumn;
  i: Integer;
  SubNode: TXmlNode;
begin
  AConfig.SetValue(ANode, 'ColumnsSet', ActiveColm);
  ANode := AConfig.FindNode(ANode, 'Sorting', True);
  AConfig.ClearNode(ANode);

  // Save sorting options.
  for i := 0 to FSorting.Count - 1 do
  begin
    SortingColumn := PFileListSortingColumn(FSorting.Items[i]);
    SubNode := AConfig.AddNode(ANode, 'Sort');
    AConfig.AddValue(SubNode, 'Column', SortingColumn^.iField);
    AConfig.AddValue(SubNode, 'Direction', Integer(SortingColumn^.SortDirection));
  end;
end;

procedure TColumnsFileView.SelectFile(AFile: TColumnsViewFile);
begin
  InvertFileSelection(AFile);
  UpdateInfoPanel;
end;

procedure TColumnsFileView.MarkFile(AFile: TColumnsViewFile; bMarked: Boolean);
begin
  if IsItemValid(AFile) then
    AFile.Selected := bMarked;
end;

procedure TColumnsFileView.MarkAllFiles(bMarked: Boolean);
var
  i: Integer;
begin
  for i := 0 to FFiles.Count - 1 do
    MarkFile(FFiles[i], bMarked);
end;

procedure TColumnsFileView.InvertFileSelection(AFile: TColumnsViewFile);
begin
  if Assigned(AFile) then
    MarkFile(AFile, not AFile.Selected);
end;

procedure TColumnsFileView.InvertAll;
var
  i:Integer;
begin
  for i := 0 to FFiles.Count-1 do
    InvertFileSelection(FFiles[i]);

  UpdateInfoPanel;
  dgPanel.Invalidate;
end;

function TColumnsFileView.StartDragEx(MouseButton: TMouseButton; ScreenStartPoint: TPoint): Boolean;
var
  fileNamesList: TStringList;
  draggedFileItem: TColumnsViewFile;
  i: Integer;
begin
  Result := False;

  if Assigned(dgPanel.DragDropSource) and (dgPanel.DragRowIndex >= dgPanel.FixedRows) then
  begin
    draggedFileItem := FFiles[dgPanel.DragRowIndex - dgPanel.FixedRows]; // substract fixed rows (header)

    fileNamesList := TStringList.Create;
    try
      if IsItemValid(draggedFileItem) = True then
      begin
        for i := 0 to FFiles.Count-1 do
        begin
          if FFiles[i].Selected then
            fileNamesList.Add(FFiles[i].TheFile.FullPath);
        end;

        // If there were no files selected add the dragged file.
        if fileNamesList.Count = 0 then
          fileNamesList.Add(draggedFileItem.TheFile.FullPath);

        // Initiate external drag&drop operation.
        Result := dgPanel.DragDropSource.DoDragDrop(fileNamesList, MouseButton, ScreenStartPoint);

        // Refresh source file panel after drop to (possibly) another application
        // (files could have been moved for example).
        // 'draggedFileItem' is invalid after this.
        Reload;
      end;

    finally
      FreeAndNil(fileNamesList);
    end;
  end;
end;

procedure TColumnsFileView.SelectRange(iRow: PtrInt);
var
  ARow, AFromRow, AToRow: Integer;
  AFile: TColumnsViewFile;
begin
  if iRow < 0 then
    iRow:= dgPanel.Row;

  if(FLastSelectionStartRow < 0) then
    begin
      AFromRow := Min(dgPanel.Row, iRow) - dgPanel.FixedRows;
      AToRow := Max(dgPanel.Row, iRow) - dgPanel.FixedRows;
      FLastSelectionStartRow := dgPanel.Row;
    end
  else
    begin
      AFromRow := Min(FLastSelectionStartRow, iRow) - dgPanel.FixedRows; // substract fixed rows (header)
      AToRow := Max(FLastSelectionStartRow, iRow) - dgPanel.FixedRows;
    end;

  MarkAllFiles(False);
  for ARow := AFromRow to AToRow do
  begin
    AFile := FFiles[ARow];
    MarkFile(AFile, True);
  end;
  UpdateInfoPanel;
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.dgPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  iRow, iCol : Integer;
  AFile: TColumnsViewFile;
begin
  if (Y < dgPanel.GetHeaderHeight) then Exit; // if is header

  SetFocus;

  if IsEmpty then Exit;

  dgPanel.MouseToCell(X, Y, iCol, iRow);

  if iRow < dgPanel.FixedRows then  // clicked on header
    Exit;

  dgPanel.LastMouseButton:= Button;

  case Button of
    mbRight: begin
      dgPanel.Row := iRow;

      if (gMouseSelectionEnabled) and (gMouseSelectionButton = 1) then
      begin
        AFile := FFiles[iRow - dgPanel.FixedRows]; // substract fixed rows (header)

        if Assigned(AFile) then
        begin
          tmContextMenu.Enabled:= True; // start context menu timer
          FLastSelectionState:= not AFile.Selected;
          MarkFile(AFile, FLastSelectionState);
          UpdateInfoPanel;
          dgPanel.Invalidate;
          Exit;
        end;
      end;
    end;

    mbLeft: begin
      if (dgPanel.Row < 0) or (dgPanel.Row >= dgPanel.RowCount) then
        begin
          dgPanel.Row := iRow;
        end
      else if gMouseSelectionEnabled then
      begin
        if ssCtrl in Shift then
          begin
            AFile := FFiles[iRow - dgPanel.FixedRows]; // substract fixed rows (header)
            if Assigned(AFile) then
              begin
                InvertFileSelection(AFile);
                UpdateInfoPanel;
                dgPanel.Invalidate;
              end;
          end
        else if ssShift in Shift then
          begin
            SelectRange(iRow);
          end
        else if (gMouseSelectionButton = 0) then
          begin
            AFile := FFiles[iRow - dgPanel.FixedRows]; // substract fixed rows (header)
            if Assigned(AFile) and not AFile.Selected then
              begin
                MarkAllFiles(False);
                UpdateInfoPanel;
                dgPanel.Invalidate;
              end;
          end;
      end;//of mouse selection handler
    end;
  else
    dgPanel.Row := iRow;
    Exit;
  end;

  { Dragging }

  if (not dgPanel.Dragging)   and  // we could be in dragging mode already (started by a different button)
     (Y < dgPanel.GridHeight) then // check if there is an item under the mouse cursor
  begin
    // indicate that drag start at next mouse move event
    dgPanel.StartDrag:= True;
    dgPanel.DragStartPoint.X := X;
    dgPanel.DragStartPoint.Y := Y;
    dgPanel.DragRowIndex := iRow;
    uDragDropEx.TransformDragging := False;
    uDragDropEx.AllowTransformToInternal := True;
  end;
end;

procedure TColumnsFileView.dgPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  AFile: TColumnsViewFile;
  iCol, iRow: Integer;
begin
  // if right mouse button selection enabled
  if dgPanel.FMouseDown and (dgPanel.LastMouseButton = mbRight) and
     gMouseSelectionEnabled and (gMouseSelectionButton = 1) then
    begin
      dgPanel.MouseToCell(X, Y, iCol, iRow);
      if iRow < dgPanel.FixedRows then Exit; // move on header
      if dgPanel.Row <> iRow then // if new row index
        begin
          tmContextMenu.Enabled:= False; // stop context menu timer
          dgPanel.Row:= iRow;
          AFile := FFiles[iRow - dgPanel.FixedRows]; // substract fixed rows (header)
          if Assigned(AFile) then
            begin
              MarkFile(AFile, FLastSelectionState);
              UpdateInfoPanel;
              dgPanel.InvalidateRow(iRow);
            end;
        end;
    end;
end;

{ Show context or columns menu on right click }
{ Is called manually from TDrawGridEx.MouseUp }
procedure TColumnsFileView.dgPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var  I : Integer; Point:TPoint; MI:TMenuItem;
begin
  if Button = mbRight then
    begin
      { If right click on header }
      if (Y < dgPanel.GetHeaderHeight) then
        begin
          //Load Columns into menu
          pmColumnsMenu.Items.Clear;
          if ColSet.Items.Count>0 then
            begin
              For I:=0 to ColSet.Items.Count-1 do
                begin
                  MI:=TMenuItem.Create(pmColumnsMenu);
                  MI.Tag:=I;
                  MI.Caption:=ColSet.Items[I];
                  MI.OnClick:=@ColumnsMenuClick;
                  pmColumnsMenu.Items.Add(MI);
                end;
            end;

         //-
    	    MI:=TMenuItem.Create(pmColumnsMenu);
    	    MI.Caption:='-';
    	    pmColumnsMenu.Items.Add(MI);
    	   //Configure this custom columns
    	    MI:=TMenuItem.Create(pmColumnsMenu);
    	    MI.Tag:=1000;
    	    MI.Caption:=rsMenuConfigureThisCustomColumn;
    	    MI.OnClick:=@ColumnsMenuClick;
    	    pmColumnsMenu.Items.Add(MI);
    	   //Configure custom columns
    	    MI:=TMenuItem.Create(pmColumnsMenu);
    	    MI.Tag:=1001;
    	    MI.Caption:=rsMenuConfigureCustomColumns;
    	    MI.OnClick:=@ColumnsMenuClick;
    	    pmColumnsMenu.Items.Add(MI);

          Point:=(Sender as TDrawGrid).ClientToScreen(Classes.Point(0,0));
          Point.Y:=Point.Y+(Sender as TDrawGridEx).GetHeaderHeight;
          Point.X:=Point.X+X-50;
          pmColumnsMenu.PopUp(Point.X,Point.Y);
        end

      { If right click on file/directory }
      else if (Y < (Sender as TDrawGridEx).GridHeight)
           and ((gMouseSelectionButton<>1) or not gMouseSelectionEnabled) then
        begin
          Actions.DoContextMenu(Self, Mouse.CursorPos.x, Mouse.CursorPos.y);
        end
      else if (gMouseSelectionEnabled and (gMouseSelectionButton = 1)) then
        begin
          tmContextMenu.Enabled:= False; // stop context menu timer
        end;
    end
  { Open folder in new tab on middle click }
  else if (Button = mbMiddle) and (Y > dgPanel.GetHeaderHeight) then
    begin
      Actions.cm_OpenDirInNewTab();
    end;
end;

procedure TColumnsFileView.dgPanelStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
end;

procedure TColumnsFileView.dgPanelDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  iRow, Dummy: Integer;
  AFile: TColumnsViewFile = nil;
  SourcePanel: TColumnsFileView = nil;
  TargetPanel: TColumnsFileView = nil;
  SourceDir, TargetDir: String;
begin
  Accept := False;

  if (not (Source is TDrawGridEx)) or (not (Sender is TDrawGridEx)) then
    Exit;

  // Always allow dropping into an empty panel.
  // And it is also allowed to drop onto header in case all visible items
  // are directories and the user wants to drop into panel's current directory.
  if IsEmpty or (Y < dgPanel.GetHeaderHeight) then
  begin
    dgPanel.ChangeDropRowIndex(-1);
    Accept:= True;
    Exit;
  end;

  SourcePanel := ((Source as TDrawGridEx).Parent) as TColumnsFileView;
  TargetPanel := ((Sender as TDrawGridEx).Parent) as TColumnsFileView;

  SourceDir := SourcePanel.CurrentPath;
  TargetDir := TargetPanel.CurrentPath;

  dgPanel.MouseToCell(X, Y, Dummy, iRow);

  if iRow >= dgPanel.FixedRows then
    AFile := FFiles[iRow - dgPanel.FixedRows]; // substract fixed rows (header)

  if Assigned(AFile) and
     (AFile.TheFile.IsDirectory or AFile.TheFile.IsLinkToDirectory) and
     (Y < dgPanel.GridHeight)
  then
    begin
      if State = dsDragLeave then
        // Mouse is leaving the control or drop will occur immediately.
        // Don't draw DropRow rectangle.
        dgPanel.ChangeDropRowIndex(-1)
      else
        dgPanel.ChangeDropRowIndex(iRow);

      if Sender = Source then
      begin
        if not ((iRow = dgPanel.DragRowIndex) or (AFile.Selected = True)) then
          Accept := True;
      end
      else
      begin
        if Assigned(SourcePanel) and Assigned(TargetPanel) then
        begin
          if AFile.TheFile.Name = '..' then
            TargetDir := GetParentDir(TargetDir)
          else
            TargetDir := TargetDir + AFile.TheFile.Name + DirectorySeparator;

          if SourceDir <> TargetDir then Accept := True;
        end
        else
          Accept := True;
      end;
    end
  else if (Sender <> Source) then
    begin
      dgPanel.ChangeDropRowIndex(-1);

      if Assigned(SourcePanel) and Assigned(TargetPanel) then
      begin
        if SourcePanel.CurrentPath <> TargetPanel.CurrentPath then
          Accept := True;
      end
      else
        Accept := True;
    end
  else
    begin
      dgPanel.ChangeDropRowIndex(-1);
    end;
end;

procedure TColumnsFileView.dgPanelDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  SourcePanel: TColumnsFileView;
  SourceFiles: TFiles;
  DropParams: TDropParams;
begin
  if (Sender is TDrawGridEx) and (Source is TDrawGridEx) then
  begin
    SourcePanel := ((Source as TDrawGridEx).Parent) as TColumnsFileView;

    // Get file names from source panel.
    SourceFiles := SourcePanel.SelectedFiles;
    try
      // Drop onto target panel.
      with Sender as TDrawGridEx do
      begin
        DropParams := TDropParams.Create(
          SourceFiles, // Will be freed automatically.
          GetDropEffectByKeyAndMouse(GetKeyShiftState,
                                    (Source as TDrawGridEx).LastMouseButton),
          ClientToScreen(Classes.Point(X, Y)),
          True,
          SourcePanel,
          Self, Self.CurrentPath);

        frmMain.DropFiles(DropParams);
        ChangeDropRowIndex(-1);
      end;
    except
      FreeAndNil(SourceFiles);
      raise;
    end;
  end;
end;

procedure TColumnsFileView.dgPanelEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  // If cancelled by the user, DragManager does not send drag-leave event
  // to the target, so we must clear the DropRow in both panels.
{
  frmMain.FrameLeft.dgPanel.ChangeDropRowIndex(-1);
  frmMain.FrameRight.dgPanel.ChangeDropRowIndex(-1);
}
  if uDragDropEx.TransformDragging = False then
    dgPanel.ClearMouseButtonAfterDrag;
end;

procedure TColumnsFileView.dgPanelHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
var
  ShiftState : TShiftState;
  SortingDirection : TSortDirection = sdAscending;
begin
  if not IsColumn then Exit;

  ShiftState := GetKeyShiftState;
  if not ((ssShift in ShiftState) or (ssCtrl in ShiftState)) then
  begin
    SortingDirection := FSorting.GetSortingDirection(Index);
    if SortingDirection = sdNone then
      SortingDirection := sdAscending
    else
      SortingDirection := ReverseSortDirection(SortingDirection);
    FSorting.Clear;
  end;

  FSorting.AddSorting(Index, SortingDirection);
  Sort(FFileSourceFiles);

  ReDisplayFileList;
end;

procedure TColumnsFileView.dgPanelMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:= True;
  case gScrollMode of
  1:
    dgPanel.Perform(LM_VSCROLL, SB_LINEUP, 0);
  2:
    dgPanel.Perform(LM_VSCROLL, SB_PAGEUP, 0);
  else
    Handled:= False;
  end;
end;

procedure TColumnsFileView.dgPanelMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Handled:= True;
  case gScrollMode of
  1:
    dgPanel.Perform(LM_VSCROLL, SB_LINEDOWN, 0);
  2:
    dgPanel.Perform(LM_VSCROLL, SB_PAGEDOWN, 0);
  else
    Handled:= False;
  end;
end;

procedure TColumnsFileView.dgPanelSelection(Sender: TObject; aCol, aRow: Integer);
begin
  if Assigned(OnChangeActiveFile) and (FLastActiveRow <> aRow) then
    begin
      OnChangeActiveFile(Self, ActiveFile);
      FLastActiveRow:= aRow;
    end;
end;

procedure TColumnsFileView.dgPanelShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  AFile: TColumnsViewFile;
  sHint: UTF8String;
begin
  if HintInfo^.HintStr = EmptyStr then Exit; // don't show
  with dgPanel do
  AFile := FFiles[HintRowIndex - FixedRows];
  if not AFile.TheFile.IsDirectory then
    begin
      sHint:= GetFileInfoToolTip(FileSource, AFile.TheFile);
      with HintInfo^ do
      if (sHint = EmptyStr) and (HintStr = #32) then  // no tooltip
        HintStr:= EmptyStr
      else if (sHint <> EmptyStr) then // has tooltip
        begin
          if HintStr = #32 then // without name
            HintStr:= sHint
          else
            HintStr:= HintStr + LineEnding + sHint;
        end;
    end;
end;

procedure TColumnsFileView.tmContextMenuTimer(Sender: TObject);
var
  AFile: TColumnsViewFile;
  iRow, iCol: Integer;
begin
  dgPanel.FMouseDown:= False;
  tmContextMenu.Enabled:= False; // stop context menu timer
  // show context menu
  Actions.DoContextMenu(Self, Mouse.CursorPos.x, Mouse.CursorPos.y);
  // get current row
  dgPanel.MouseToCell(Mouse.CursorPos.x, Mouse.CursorPos.y, iRow, iCol);
  if iRow < dgPanel.FixedRows then Exit;
  AFile := FFiles[iRow - dgPanel.FixedRows]; // get current file
  MarkFile(AFile, False); // unselect file
  dgPanel.InvalidateRow(iRow); // invalidate current row
end;

procedure TColumnsFileView.edtSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_DOWN:
      begin
        fSearchDirect := True;
        fNext := True;
        Key := 0;
        edtSearchChange(Sender);
      end;

    VK_UP:
      begin
        fSearchDirect := False;
        fPrevious := True;
        Key := 0;
        edtSearchChange(Sender);
      end;

    VK_TAB:
      begin
        CloseSearchPanel;
        SetFocus;
        Key := 0;
      end;

    VK_ESCAPE:
      begin
        Key := 0;
        CloseSearchPanel;
        SetFocus;
      end;

    VK_RETURN,
    VK_SELECT:
      begin
        Key := 0;
        CloseSearchPanel;
        SetFocus;

        {LaBero begin}
        {en
            Execute/open selected file/directory
            if the user press ENTER during QuickSearch
        }
        ChooseFile(GetActiveItem);
        {LaBero end}
      end;
  end;
end;

procedure TColumnsFileView.SetCurrentPath(NewPath: String);
begin
  if NewPath <> '' then
  begin
    if Assigned(OnBeforeChangeDirectory) then
      if not OnBeforeChangeDirectory(Self, NewPath) then
        Exit;

    if not FileSource.SetCurrentWorkingDirectory(NewPath) then
    begin
      msgError(Format(rsMsgChDirFailed, [NewPath]));
      Exit;   // chdir failed
    end;

{
    AddDirToHistory(fActiveDir);
}
    inherited SetCurrentPath(NewPath);

    LastActive := '';
    dgPanel.Row := 0;

    if (fspDirectAccess in FileSource.GetProperties) then
      begin
        if gTermWindow and Assigned(Cons) then
          Cons.Terminal.SetCurrentDir(NewPath);
      end;

    MakeFileSourceFileList;

    UpdatePathLabel;

    if Assigned(OnAfterChangeDirectory) then
      OnAfterChangeDirectory(Self, CurrentPath);
  end;
end;

{
History should include FileSource type as well as address and path.
procedure TColumnsFileView.AddDirToHistory(const Directory: String);
begin
  if glsDirHistory.IndexOf(fActiveDir)=-1 then
    glsDirHistory.Insert(0,fActiveDir);
end;
}

procedure TColumnsFileView.ChooseFile(AFile: TColumnsViewFile; FolderMode: Boolean = False);
begin
  with AFile do
  begin
    if TheFile.Name = '..' then
    begin
      ChangePathToParent(True);
      Exit;
    end;

    if TheFile.IsDirectory or TheFile.IsLinkToDirectory then // deeper and deeper
    begin
      ChangePathToChild(TheFile);
      Exit;
    end;

    if FolderMode then exit;

    LastActive := TheFile.Name;

    try
      uFileSourceUtil.ChooseFile(Self, AFile.TheFile);

    except
      on e: Exception do
        MessageDlg('Error', e.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TColumnsFileView.ShowRenameFileEdit(const sFileName:String);
begin
  frmMain.EnableHotkeys(False);

  edtRename.Width := dgPanel.ColWidths[0]+dgPanel.ColWidths[1]-16;
  edtRename.Top := (dgPanel.CellRect(0,dgPanel.Row).Top-2);
  if gShowIcons <> sim_none then
    edtRename.Left:= gIconsSize + 3
  else
    edtRename.Left:= 2;
  edtRename.Height:=dgpanel.DefaultRowHeight+4;
  edtRename.Hint:=sFileName;
  edtRename.Text:=ExtractFileName(sFileName);
  edtRename.Visible:=True;
  edtRename.SetFocus;
  if gRenameSelOnlyName then
    begin
      {$IFDEF LCLGTK2}
      edtRename.SelStart:=1;
      {$ENDIF}
      edtRename.SelStart:=0;
      edtRename.SelLength:= UTF8Length(edtRename.Text)-UTF8Length(ExtractFileExt(edtRename.Text));
    end
  else
    edtRename.SelectAll;
end;

procedure TColumnsFileView.ShowPathEdit;
begin
  frmMain.EnableHotkeys(False);

  with lblPath do
  begin
    edtPath.SetBounds(Left, Top, Width, Height);
    edtPath.Text := CurrentPath;
    edtPath.Visible := True;
    edtPath.SetFocus;
  end;
end;

procedure TColumnsFileView.UpdateAddressLabel;
begin
  if CurrentAddress = '' then
  begin
    lblAddress.Visible := False;
  end
  else
  begin
    lblAddress.Visible := True;
    lblAddress.Caption := CurrentAddress;
  end;
end;

procedure TColumnsFileView.UpdatePathLabel;
begin
  lblPath.Caption := MinimizeFilePath(CurrentPath, lblPath.Canvas, lblPath.Width);
end;

procedure TColumnsFileView.SortByColumn(iColumn: Integer);
var
  ColumnsClass: TPanelColumnsClass;
begin
  ColumnsClass := GetColumnsClass;

  if (iColumn >= 0) and (iColumn < ColumnsClass.ColumnsCount) then
  begin
    FSorting.Clear;
    FSorting.AddSorting(iColumn, FSortDirection);
    FSortColumn := iColumn;
    Sort(FFileSourceFiles);
    ReDisplayFileList;
  end;
end;

procedure TColumnsFileView.Sort(FilesToSort: TFiles);
begin
  if (not Assigned(FilesToSort)) or
     (FilesToSort.Count = 0) then Exit;

  Sort(FilesToSort, PrepareSortings);
end;

class procedure TColumnsFileView.Sort(FilesToSort: TFiles; ASortings: TFileSortings);
var
  FileListSorter: TListSorter;
begin
  if (not Assigned(FilesToSort)) or
     (FilesToSort.Count = 0) or
     (not Assigned(ASortings)) then Exit;

  FileListSorter := TListSorter.Create(FilesToSort.List, ASortings);
  try
    FileListSorter.Sort;
  finally
    FreeAndNil(FileListSorter);
  end;
end;

function TColumnsFileView.PrepareSortings: TFileSortings;
var
  ColumnsClass: TPanelColumnsClass;
  i, j, sortingIndex : Integer;
  pSortingColumn : PFileListSortingColumn;
  Column: TPanelColumn;
  bSortedByName: Boolean;
  bSortedByExtension: Boolean;
  FileSortings: TFileSortings;
  TempSorting: TFileListSorting;
  SortFunctions: TFileFunctions;
begin
  Result := nil;

  ColumnsClass := GetColumnsClass;
  if ColumnsClass.ColumnsCount = 0 then
    Exit;

  TempSorting := TFileListSorting.Create;
  try
    for i := 0 to FSorting.Count - 1 do
    begin
      pSortingColumn := PFileListSortingColumn(FSorting[i]);
      TempSorting.AddSorting(pSortingColumn^.iField, pSortingColumn^.SortDirection);
    end;

    bSortedByName := False;
    bSortedByExtension := False;

    SetLength(FileSortings, TempSorting.Count);

    sortingIndex := 0;
    for i := 0 to TempSorting.Count - 1 do
    begin
      pSortingColumn := PFileListSortingColumn(TempSorting[i]);

      if (pSortingColumn^.iField >= 0) and
         (pSortingColumn^.iField < ColumnsClass.ColumnsCount) then
      begin
        Column := ColumnsClass.GetColumnItem(pSortingColumn^.iField);
        SortFunctions := Column.GetColumnFunctions;

        // Check if each sort function is supported.
        for j := 0 to Length(SortFunctions) - 1 do
          if TFileFunctionToProperty[SortFunctions[j]] <= FileSource.GetSupportedFileProperties then
            AddSortFunction(FileSortings[sortingIndex].SortFunctions, SortFunctions[j]);

        if Length(FileSortings[sortingIndex].SortFunctions) > 0 then
        begin
          FileSortings[sortingIndex].SortDirection := pSortingColumn^.SortDirection;

          if HasSortFunction(FileSortings[sortingIndex].SortFunctions, fsfName) then
          begin
            bSortedByName := True;
            bSortedByExtension := True;
          end
          else if HasSortFunction(FileSortings[sortingIndex].SortFunctions, fsfNameNoExtension)
          then
          begin
            bSortedByName := True;
          end
          else if HasSortFunction(FileSortings[sortingIndex].SortFunctions, fsfExtension)
          then
          begin
            bSortedByExtension := True;
          end;

          Inc(sortingIndex);
        end;
      end
      else
        Raise Exception.Create('Invalid column number in sorting - fix me');
    end;

    SetLength(FileSortings, sortingIndex);

    // Add automatic sorting by name and/or extension if there wasn't any.

    if not bSortedByName then
    begin
      if not bSortedByExtension then
        AddSorting(FileSortings, fsfName, sdAscending)
      else
        AddSorting(FileSortings, fsfNameNoExtension, sdAscending);
    end
    else
    begin
      if not bSortedByExtension then
        AddSorting(FileSortings, fsfExtension, sdAscending);
      // else
      //   There is already a sorting by filename and extension.
    end;

    Result := FileSortings;

  finally
    FreeAndNil(TempSorting);
  end;
end;

procedure TColumnsFileView.UpdateColCount(NewColCount: Integer);
begin
  while dgPanel.Columns.Count < NewColCount do
    dgPanel.Columns.Add;
  while dgPanel.Columns.Count > NewColCount do
    dgPanel.Columns.Delete(0);
end;

procedure TColumnsFileView.SetColumnsWidths;
var
  x: Integer;
  ColumnsClass: TPanelColumnsClass;
begin
  //  setup column widths
  ColumnsClass := GetColumnsClass;

  UpdateColCount(ColumnsClass.ColumnsCount);
  if ColumnsClass.ColumnsCount > 0 then
    for x:= 0 to ColumnsClass.ColumnsCount - 1 do
      begin
        if not ((x = 0) and gAutoFillColumns and (gAutoSizeColumn = 0)) then
          dgPanel.Columns.Items[x].SizePriority:= 0;
        dgPanel.ColWidths[x]:= ColumnsClass.GetColumnWidth(x);
        dgPanel.Columns.Items[x].Title.Caption:= ColumnsClass.GetColumnTitle(x);
      end;
end;

procedure TColumnsFileView.SetActive(bActive: Boolean);
begin
  FActive := bActive;

  lblAddress.SetActive(bActive);
  lblPath.SetActive(bActive);

  dgPanel.Color := DimColor(gBackColor);
end;

procedure TColumnsFileView.SetFileFilter(NewFilter: String);
begin
  FFileFilter := NewFilter;
  ReDisplayFileList;
end;

function TColumnsFileView.DimColor(AColor: TColor): TColor;
begin
  if (not FActive) and (gInactivePanelBrightness < 100) then
    Result := ModColor(AColor, gInactivePanelBrightness)
  else
    Result := AColor;
end;

procedure TColumnsFileView.DoOnReload;
begin
  if FReloading then
  begin
    FReloading := False;
    if Assigned(OnReload) then
      OnReload(Self);
  end;
end;

procedure TColumnsFileView.edtPathExit(Sender: TObject);
begin
  edtPath.Visible := False;
end;

procedure TColumnsFileView.edtSearchExit(Sender: TObject);
begin
  // sometimes must be search panel closed this way
  CloseSearchPanel;
  RedrawGrid;
end;

procedure TColumnsFileView.edtRenameExit(Sender: TObject);
begin
  edtRename.Visible := False;
  UnMarkAll;
end;

procedure TColumnsFileView.edtFilterEnter(Sender: TObject);
begin
  SetActive(True);
end;

procedure TColumnsFileView.edtFilterExit(Sender: TObject);
begin
  if edtFilter.Text = '' then
    pnlFilter.Visible := False;

  SetActive(False);
end;

procedure TColumnsFileView.edtSearchChange(Sender: TObject);
var
  I, iPos, iEnd : Integer;
  Result : Boolean;
  sSearchName,
  sSearchNameNoExt,
  sSearchExt : UTF8String;
begin
  if (edtSearch.Text='') or IsEmpty then Exit;
  //DebugLn('edtSearchChange: '+ edtSearch.Text);

  sSearchName := UTF8LowerCase(edtSearch.Text);

  if Pos('.', sSearchName) <> 0 then
    begin
      sSearchNameNoExt := ExtractOnlyFileName(sSearchName);
      sSearchExt := ExtractFileExt(sSearchName);
      if not gQuickSearchMatchBeginning then
        sSearchNameNoExt := '*' + sSearchNameNoExt;
      if not gQuickSearchMatchEnding then
        sSearchNameNoExt := sSearchNameNoExt + '*';
      sSearchName := sSearchNameNoExt + sSearchExt + '*';
    end
  else
    begin
      if not gQuickSearchMatchBeginning then
        sSearchName := '*' + sSearchName;
      sSearchName := sSearchName + '*';
    end;

  DebugLn('sSearchName = ', sSearchName);

  I := dgPanel.Row; // start search from current cursor position
  iPos := I;        // save cursor position
  if not (fNext or fPrevious) then fSearchDirect := True;
  if fSearchDirect then
    begin
      if fNext then
        I := I + 1; // begin search from next file
      iEnd := dgPanel.RowCount;
    end
  else
    begin
      if fPrevious then
        I := I - 1; // begin search from previous file
      iEnd := dgPanel.FixedRows - 1;
    end;

  try
    while I <> iEnd do
      begin
        Result := MatchesMask(UTF8LowerCase(FFiles[I - dgPanel.FixedRows].TheFile.Name), sSearchName);

        if Result then
          begin
            dgPanel.Row := I;
            MakeVisible(I);
            Exit;
          end;

        if fSearchDirect then
          Inc(I)
        else
          Dec(I);

        // if not Next or Previous then search from beginning of list
        // to cursor position
        if (not(fNext or fPrevious)) and (I = iEnd) then
          begin
            I := dgPanel.FixedRows;
            iEnd := iPos;
            iPos := I;
          end;
      end; // while
  except
    on EConvertError do; // bypass
    else
      raise;
  end;

  fNext := False;
  fPrevious := False;
end;

procedure TColumnsFileView.CloseSearchPanel;
begin
  pnAltSearch.Visible:=False;
  edtSearch.Text:='';
  SetActive(False);
end;

procedure TColumnsFileView.FilterPanelVisible;
begin
  pnlFilter.Visible := True;
  edtFilter.Width := pnlFilter.Width div 2;
end;

procedure TColumnsFileView.CloseFilterPanel;
begin
  edtFilter.Text := '';      // Automatically triggers edtFilterChange.
  pnlFilter.Visible := False;
  SetFocus;
end;

procedure TColumnsFileView.ShowSearchPanel(Char : TUTF8Char);
begin
  frmMain.EnableHotkeys(False);

  edtSearch.Height   := pnAltSearch.Canvas.TextHeight('Wg') + 1
                      + GetSystemMetrics(SM_CYEDGE) * 2;
  pnAltSearch.Height := edtSearch.Height + GetSystemMetrics(SM_CYEDGE);
  pnAltSearch.Width  := dgPanel.Width div 2;
  edtSearch.Width    := pnAltSearch.Width - edtSearch.Left
                      - GetSystemMetrics(SM_CXEDGE);

  pnAltSearch.Top  := pnlFooter.Top + pnlFooter.Height - pnAltSearch.Height;
  pnAltSearch.Left := dgPanel.Left;

  pnAltSearch.Visible := True;
  edtSearch.SetFocus;
  edtSearch.Tag := 0; // save current search position
  fSearchDirect := True; // set search direction
  fNext := False;
  fPrevious := False;
  edtSearch.Text := Char;
  edtSearch.SelStart := UTF8Length(edtSearch.Text) + 1;
  SetActive(True);
end;

procedure TColumnsFileView.ShowFilterPanel(Char : TUTF8Char = #0);
begin
  frmMain.EnableHotkeys(False);

  FilterPanelVisible;
  edtFilter.SetFocus;

  if Char <> #0 then
  begin
    edtFilter.Text := Char;
    edtFilter.SelStart := UTF8Length(edtFilter.Text) + 1;
    edtFilter.SelLength := 0;
  end
  else
  begin
    edtFilter.SelectAll;
  end;
end;

procedure TColumnsFileView.UpdateInfoPanel;
var
  i: Integer;
  FilesInDir, FilesSelected: Integer;
  SizeInDir, SizeSelected: Int64;
  SizeProperty: TFileSizeProperty;
  SizeSupported: Boolean;
begin
  if Assigned(FCurrentFileListBuilder) and
     (FCurrentFileListBuilder.State = rfsLoadingFiles) then
  begin
    lblInfo.Caption := rsMsgLoadingFileList;
  end
  else
  begin
    FilesInDir := 0;
    FilesSelected := 0;
    SizeInDir := 0;
    SizeSelected := 0;

    SizeSupported := fpSize in FileSource.SupportedFileProperties;

    for i := 0 to FFiles.Count - 1 do
    begin
      with FFiles[i] do
      begin
        if TheFile.Name = '..' then Continue;

        inc(FilesInDir);
        if Selected then
          inc(FilesSelected);

        // Count size if Size property is supported.
        if SizeSupported then
        begin
          SizeProperty := TheFile.Properties[fpSize] as TFileSizeProperty;

          if Selected then
            SizeSelected := SizeSelected + SizeProperty.Value;

          SizeInDir := SizeInDir + SizeProperty.Value;
        end;
      end;
    end;

    lblInfo.Caption := Format(rsMsgSelected,
                              [cnvFormatFileSize(SizeSelected),
                               cnvFormatFileSize(SizeInDir),
                               FilesSelected,
                               FilesInDir]);
  end;
end;

procedure TColumnsFileView.MarkAll;
begin
  MarkAllFiles(True);
  UpdateInfoPanel;
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.MarkPlus;
var
  s: String;
begin
  if IsEmpty then Exit;
  s := FLastMark;
  if not ShowMaskInputDlg(rsMarkPlus, rsMaskInput, glsMaskHistory, s) then Exit;
  FLastMark := s;
  MarkGroup(s, True);
  UpdateInfoPanel;
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.MarkShiftPlus;
var
  sGroup: String;
begin
  if IsActiveItemValid then
  begin
    sGroup := GetActiveItem.TheFile.Extension;
    if sGroup <> '' then
      sGroup := '.' + sGroup;
    MarkGroup('*' + sGroup, True);
    UpdateInfoPanel;
    dgPanel.Invalidate;
  end;
end;

procedure TColumnsFileView.MarkShiftMinus;
var
  sGroup: String;
begin
  if IsActiveItemValid then
  begin
    sGroup := GetActiveItem.TheFile.Extension;
    if sGroup <> '' then
      sGroup := '.' + sGroup;
    MarkGroup('*' + sGroup, False);
    UpdateInfoPanel;
    dgPanel.Invalidate;
  end;
end;

procedure TColumnsFileView.SaveSelection;
var
  I: Integer;
begin
  FSelection.Clear;
  for I := 0 to FFiles.Count - 1 do
    with FFiles[I] do
    begin
      if Selected then
        FSelection.Add(TheFile.Name);
    end;
end;

procedure TColumnsFileView.RestoreSelection;
var
  I: Integer;
begin
  for I := 0 to FFiles.Count - 1 do
    with FFiles[I] do
    Selected:= (FSelection.IndexOf(TheFile.Name) >= 0);
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.MarkMinus;
var
  s: String;
begin
  if IsEmpty then Exit;
  s := FLastMark;
  if not ShowMaskInputDlg(rsMarkMinus, rsMaskInput, glsMaskHistory, s) then Exit;
  FLastMark := s;
  MarkGroup(s, False);
  UpdateInfoPanel;
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.UnMarkAll;
begin
  MarkAllFiles(False);
  UpdateInfoPanel;
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.MarkGroup(const sMask: String; bSelect: Boolean);
var
  I: Integer;
  SearchTemplate: TSearchTemplate = nil;
begin
  if IsMaskSearchTemplate(sMask) then
    begin
      SearchTemplate:= gSearchTemplateList.TemplateByName[sMask];
      if Assigned(SearchTemplate) then
        for I := 0 to FFiles.Count - 1 do
          begin
            if FFiles[I].TheFile.Name = '..' then Continue;
            if SearchTemplate.CheckFile(FFiles[I].TheFile) then
              FFiles[I].Selected := bSelect;
          end;
    end
  else
    for I := 0 to FFiles.Count - 1 do
      begin
        if FFiles[I].TheFile.Name = '..' then Continue;
        if MatchesMaskList(FFiles[I].TheFile.Name, sMask) then
          FFiles[I].Selected := bSelect;
      end;
end;

procedure TColumnsFileView.edtPathKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        Key := 0;
        edtPath.Visible:=False;
        SetFocus;
      end;

    VK_RETURN,
    VK_SELECT:
      begin
        Key := 0; // catch the enter
        CurrentPath := edtPath.Text;
        edtPath.Visible := False;
        SetFocus;
      end;

{$IFDEF LCLGTK2}
    // Workaround for GTK2 - up and down arrows moving through controls.
    VK_UP,
    VK_DOWN:
      Key := 0;
{$ENDIF}
  end;
end;

procedure TColumnsFileView.edtRenameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
var
  NewFileName: String;
  OldFileNameAbsolute: String;
begin
  case Key of
    VK_ESCAPE:
      begin
        Key := 0;
        edtRename.Visible:=False;
        SetFocus;
      end;

    VK_RETURN,
    VK_SELECT:
      begin
        Key := 0; // catch the enter

        NewFileName         := edtRename.Text;
        OldFileNameAbsolute := edtRename.Hint;

        try
          if RenameFile(FileSource, ActiveFile, NewFileName, True) = True then
          begin
            edtRename.Visible:=False;
            LastActive := NewFileName;
            SetFocus;
          end
          else
            msgError(Format(rsMsgErrRename, [ExtractFileName(OldFileNameAbsolute), NewFileName]));

        except
          on e: EInvalidFileProperty do
            msgError(Format(rsMsgErrRename + ':' + LineEnding + '%s (%s)', [ExtractFileName(OldFileNameAbsolute), NewFileName, rsMsgInvalidFileName, e.Message]));
        end;
      end;

{$IFDEF LCLGTK2}
    // Workaround for GTK2 - up and down arrows moving through controls.
    VK_UP,
    VK_DOWN:
      Key := 0;
{$ENDIF}
  end;
end;

procedure TColumnsFileView.edtFilterChange(Sender: TObject);
begin
  FileFilter := edtFilter.Text;
end;

procedure TColumnsFileView.edtFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_TAB, VK_RETURN, VK_SELECT:
      begin
        SetFocus;
        Key := 0;
      end;

    VK_ESCAPE:  // Close panel and remove filter with Escape.
      begin
        CloseFilterPanel;
        Key := 0;
      end;
  end;
end;

procedure TColumnsFileView.btnCloseFilterClick(Sender: TObject);
begin
  CloseFilterPanel;
end;

procedure TColumnsFileView.MakeVisible(iRow:Integer);
begin
  with dgPanel do
  begin
    if iRow<TopRow then
      TopRow:=iRow;
    if iRow>TopRow+VisibleRowCount then
      TopRow:=iRow-VisibleRowCount;
  end;
end;

procedure TColumnsFileView.dgPanelExit(Sender: TObject);
begin
  SetActive(False);
end;

procedure TColumnsFileView.MakeSelectedVisible;
begin
  if dgPanel.Row>=0 then
    MakeVisible(dgPanel.Row);
end;

procedure TColumnsFileView.SetActiveFile(const aFileName: String);
var
  i: Integer;
begin
  LastActive := aFileName;// '';
  if aFileName <> '' then // find correct cursor position in Panel (drawgrid)
  begin
    for i := 0 to FFiles.Count - 1 do
      if FFiles[i].TheFile.Name = aFileName then
      begin
        dgPanel.Row := i + dgPanel.FixedRows;
        LastActive := aFileName;
        Break;
      end;
  end;
end;

procedure TColumnsFileView.dgPanelDblClick(Sender: TObject);
var
  Point : TPoint;
begin
{$IFDEF LCLGTK2}
  // Workaround for two doubleclicks being sent on GTK.
  if dgPanel.TooManyDoubleClicks then Exit;
{$ENDIF}

  dgPanel.StartDrag:= False; // don't start drag on double click
  Point:= dgPanel.ScreenToClient(Mouse.CursorPos);

  // If on a file/directory then choose it.
  if (Point.Y >=  dgPanel.GetHeaderHeight) and
     (Point.Y <   dgPanel.GridHeight) and
     (not IsEmpty) then
  begin
    ChooseFile(GetActiveItem);
  end;

{$IFDEF LCLGTK2}
  dgPanel.fLastDoubleClickTime := Now;
{$ENDIF}
end;

procedure TColumnsFileView.dgPanelEnter(Sender: TObject);
begin
  SetActive(True);

  UpdateInfoPanel;
  frmMain.EnableHotkeys(True);

  if Assigned(OnActivate) then
    OnActivate(Self);
end;

procedure TColumnsFileView.RedrawGrid;
begin
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.UpdateColumnsView;
var
  ColumnsClass: TPanelColumnsClass;
begin
  if ActiveColm <> '' then
  begin
    // If the ActiveColm set doesn't exist this will retrieve either
    // the first set or the default set.
    ColumnsClass := GetColumnsClass;
    // Set name in case a different set was loaded.
    ActiveColm := ColumnsClass.Name;

    SetColumnsWidths;

    dgPanel.FocusRectVisible := ColumnsClass.GetCursorBorder;
    dgPanel.FocusColor := ColumnsClass.GetCursorBorderColor;
  end;
  // else No columns set yet.
end;

procedure TColumnsFileView.dgPanelKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_SHIFT: begin
      FLastSelectionStartRow := -1;
    end;
  end;
end;

procedure TColumnsFileView.dgPanelMouseLeave(Sender: TObject);
begin
  if (gMouseSelectionEnabled) and (gMouseSelectionButton = 1) then
    dgPanel.FMouseDown:= False;
end;

procedure TColumnsFileView.dgPanelKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

  function CheckSearchOrFilter(ModifierKeys: TShiftState;
                               SearchOrFilterEnabled: Boolean;
                               SearchOrFilterMode: TShiftState;
                               out UTF8Char: TUTF8Char): Boolean;
  begin
    Result := False;

    // used for quick search/filter by Ctrl+Alt+Letter and Alt+Letter
    if SearchOrFilterEnabled then
    begin
      if ((SearchOrFilterMode <> []) and
          // Check only Ctrl and Alt.
         (ModifierKeys * [ssCtrl, ssAlt] = SearchOrFilterMode))
  {$IFDEF MSWINDOWS}
      // Entering international characters with Ctrl+Alt on Windows.
      or ((SearchOrFilterMode = []) and
         (ModifierKeys * [ssCtrl, ssAlt] = [ssCtrl, ssAlt]) and
         (ModifierKeys - [ssCtrl, ssAlt, ssShift, ssCaps] = []))
  {$ENDIF}
      then
      begin
        UTF8Char := VirtualKeyToUTF8Char(Key, ModifierKeys - SearchOrFilterMode);
        Result := UTF8Char <> '';
      end;
    end;
  end;

var
  ModifierKeys: TShiftState;
  ScreenPoint: TPoint;
  UTF8Char: TUTF8Char;
begin
  ModifierKeys := GetKeyShiftStateEx;

  if CheckSearchOrFilter(ModifierKeys, gQuickSearch, gQuickSearchMode, UTF8Char) then
  begin
    ShowSearchPanel(UTF8Char);
    Key := 0;
    Exit;
  end;

  if CheckSearchOrFilter(ModifierKeys, gQuickFilter, gQuickFilterMode, UTF8Char) then
  begin
    ShowFilterPanel(UTF8Char);
    Key := 0;
    Exit;
  end;

  case Key of
    VK_APPS:
      begin
        cm_ContextMenu('');
        Key := 0;
      end;

    VK_INSERT:
      begin
        if not IsEmpty then
        begin
          if IsActiveItemValid then
            SelectFile(GetActiveItem);
          dgPanel.InvalidateRow(dgPanel.Row);
          if dgPanel.Row < dgPanel.RowCount-1 then
            dgPanel.Row := dgPanel.Row+1;
          MakeSelectedVisible;
        end;
        Key := 0;
      end;

    VK_MULTIPLY:
      begin
        InvertAll;
        Key := 0;
      end;

    VK_ADD:
      begin
        if Shift = [ssCtrl] then
          MarkAll
        else if Shift = [] then
          MarkPlus
        else if Shift = [ssShift] then
          MarkShiftPlus;
        Key := 0;
      end;

    VK_SUBTRACT:
      begin
        if Shift = [ssCtrl] then
          UnMarkAll
        else if Shift = [] then
          MarkMinus
        else if Shift = [ssShift] then
          MarkShiftMinus;
        Key := 0;
      end;

    VK_SHIFT:
      begin
        FLastSelectionStartRow:= dgPanel.Row;
        Key := 0;
      end;

    VK_HOME, VK_END, VK_PRIOR, VK_NEXT:
      if (ssShift in Shift) then
      begin
        Application.QueueAsyncCall(@SelectRange, -1);
        //Key := 0; // not needed!
      end;

    // cursors keys in Lynx like mode
    VK_LEFT:
      if (Shift = []) and gLynxLike then
      begin
        ChangePathToParent(True);
        Key := 0;
      end;

    VK_RIGHT:
      if (Shift = []) and gLynxLike then
      begin
        if Assigned(GetActiveItem) then
          ChooseFile(GetActiveItem, True);
        Key := 0;
      end;

    VK_UP, VK_DOWN:
      begin
        if ssShift in Shift then
        begin
          if IsActiveItemValid then
          begin
            SelectFile(GetActiveItem);
            if (dgPanel.Row = dgPanel.RowCount-1) or (dgPanel.Row = dgPanel.FixedRows) then
              dgPanel.Invalidate;
            //Key := 0; // not needed!
          end;
        end
{$IFDEF LCLGTK2}
        else
        begin
          if ((dgPanel.Row = dgPanel.RowCount-1) and (Key = VK_DOWN))
          or ((dgPanel.Row = dgPanel.FixedRows) and (Key = VK_UP)) then
            Key := 0;
        end;
{$ENDIF}
      end;

    VK_SPACE:
      if (Shift = []) and
         ((not frmMain.IsCommandLineVisible) or (frmMain.edtCommand.Text = '')) then
      begin
        if not IsEmpty then
        begin
          if IsActiveItemValid then
          begin
            if GetActiveItem.TheFile.IsDirectory or
               GetActiveItem.TheFile.IsLinkToDirectory then
            begin
              Screen.Cursor := crHourGlass;
              CalculateSpace(GetActiveItem);
              Screen.Cursor := crDefault;
            end;

            SelectFile(GetActiveItem);
          end;

          if gSpaceMovesDown then
            dgPanel.Row := dgPanel.Row + 1;

          dgPanel.Invalidate;
          MakeSelectedVisible;
        end;
        Key := 0;
      end;

    VK_BACK:
      if (Shift = []) and
         ((not frmMain.IsCommandLineVisible) or (frmMain.edtCommand.Text = '')) then
      begin
        if (frmMain.edtCommand.Tag = 0) then
        begin
          ChangePathToParent(True);
          RedrawGrid;
        end;
        Key := 0;
      end;

    VK_RETURN, VK_SELECT:
      begin
        if (Shift=[]) or (Shift=[ssCaps]) then // 21.05.2009 - не учитываем CapsLock при перемещении по панелям
        begin
          // Only if there are items in the panel.
          if not IsEmpty then
          begin
            ChooseFile(GetActiveItem);
            Key := 0;
          end;
        end
        // execute active file in terminal (Shift+Enter)
        else if Shift=[ssShift] then
        begin
          if IsActiveItemValid then
          begin
            mbSetCurrentDir(CurrentPath);
            ExecCmdFork(CurrentPath + GetActiveItem.TheFile.Name, True, gRunInTerm);
            Key := 0;
          end;
        end;
      end;

    VK_MENU:  // Alt key
      if dgPanel.Dragging then
      begin
        // Force transform to external dragging in anticipation of user
        // pressing Alt+Tab to change active application window.

        // Disable flag, so that dragging isn't immediately transformed
        // back to internal before the other application window is shown.
        uDragDropEx.AllowTransformToInternal := False;

        GetCursorPos(ScreenPoint);
        dgPanel.TransformDraggingToExternal(ScreenPoint);
      end;
  end;
end;

function TColumnsFileView.IsEmpty: Boolean;
begin
  Result := (FFiles.Count = 0);
end;

function TColumnsFileView.IsItemValid(AFile: TColumnsViewFile): Boolean;
begin
  if Assigned(AFile) and (AFile.TheFile.Name <> '..') then
    Result := True
  else
    Result := False;
end;

function TColumnsFileView.IsActiveItemValid:Boolean;
begin
  Result := IsItemValid(GetActiveItem);
end;

function TColumnsFileView.HasSelectedFiles: Boolean;
var
  i: Integer;
begin
  for i := 0 to FFiles.Count - 1 do
  begin
    if FFiles[i].Selected then
      Exit(True);
  end;
  Result := False;
end;

procedure TColumnsFileView.lblPathClick(Sender: TObject);
begin
  SetFocus;

  if lblPath.GetSelectedDir <> '' then
  begin
    // User clicked on a subdirectory of the path.
    CurrentPath := lblPath.SelectedDir;
  end
  else
    Actions.cm_DirHistory('');
end;

procedure TColumnsFileView.lblPathMouseUp(Sender: TObject; Button: TMouseButton;
                                          Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbMiddle:
      begin
        Actions.cm_DirHotList('');
      end;

    mbRight:
      begin
        ShowPathEdit;
      end;
  end;
end;

procedure TColumnsFileView.pnlHeaderResize(Sender: TObject);
begin
  UpdateAddressLabel;
  UpdatePathLabel;
end;

procedure TColumnsFileView.ColumnsMenuClick(Sender: TObject);
var
  frmColumnsSetConf: TfColumnsSetConf;
  Index: Integer;
begin
  Case (Sender as TMenuItem).Tag of
    1000: //This
          begin
            Application.CreateForm(TfColumnsSetConf, frmColumnsSetConf);
            {EDIT Set}
            frmColumnsSetConf.edtNameofColumnsSet.Text:=ColSet.GetColumnSet(ActiveColm).CurrentColumnsSetName;
            Index:=ColSet.Items.IndexOf(ActiveColm);
            frmColumnsSetConf.lbNrOfColumnsSet.Caption:=IntToStr(1 + Index);
            frmColumnsSetConf.Tag:=Index;
            frmColumnsSetConf.SetColumnsClass(GetColumnsClass);
            {EDIT Set}
            if frmColumnsSetConf.ShowModal = mrOK then
            begin
              // Force saving changes to config file.
              SaveGlobs;
            end;

            FreeAndNil(frmColumnsSetConf);

            frmMain.ReLoadTabs(frmMain.LeftTabs);
            frmMain.ReLoadTabs(frmMain.RightTabs);
          end;
    1001: //All columns
          begin
            Actions.cm_Options('15');
            frmMain.ReLoadTabs(frmMain.LeftTabs);
            frmMain.ReLoadTabs(frmMain.RightTabs);
          end;

  else
    begin
      ActiveColm:=ColSet.Items[(Sender as TMenuItem).Tag];
      UpdateColumnsView;
    end;
  end;
end;

function TColumnsFileView.GetGridHorzLine: Boolean;
begin
  Result := goHorzLine in dgPanel.Options;
end;

function TColumnsFileView.GetGridVertLine: Boolean;
begin
  Result := goVertLine in dgPanel.Options;
end;

procedure TColumnsFileView.SetGridHorzLine(const AValue: Boolean);
begin
  if AValue then
    dgPanel.Options := dgPanel.Options + [goHorzLine]
  else
    dgPanel.Options := dgPanel.Options - [goHorzLine];
end;

procedure TColumnsFileView.SetGridVertLine(const AValue: Boolean);
begin
  if AValue then
    dgPanel.Options := dgPanel.Options + [goVertLine]
  else
    dgPanel.Options := dgPanel.Options - [goVertLine]
end;

constructor TColumnsFileView.Create(AOwner: TWinControl; AFileSource: IFileSource; APath: String);
begin
  inherited Create(AOwner, AFileSource, APath);

  FFiles := TColumnsViewFiles.Create;
  FSorting := TFileListSorting.Create;
  ActiveColm := 'Default';

  // Update view before making file source file list,
  // so that file list isn't unnecessarily displayed twice.
  UpdateView;
  MakeFileSourceFileList;
end;

constructor TColumnsFileView.Create(AOwner: TWinControl; AFileView: TFileView);
begin
  inherited Create(AOwner, AFileView);
  UpdateView;
end;

constructor TColumnsFileView.Create(AOwner: TWinControl; AConfig: TIniFileEx; ASectionName: String; ATabIndex: Integer);
begin
  inherited Create(AOwner, AConfig, ASectionName, ATabIndex);

  FFiles := TColumnsViewFiles.Create;
  FSorting := TFileListSorting.Create;

  LoadConfiguration(ASectionName, ATabIndex);
end;

constructor TColumnsFileView.Create(AOwner: TWinControl; AConfig: TXmlConfig; ANode: TXmlNode);
begin
  inherited Create(AOwner, AConfig, ANode);

  FFiles := TColumnsViewFiles.Create;
  FSorting := TFileListSorting.Create;

  LoadConfiguration(AConfig, ANode);
end;

procedure TColumnsFileView.CreateDefault(AOwner: TWinControl);
begin
  DebugLn('TColumnsFileView.Create components');

  dgPanel := nil;

  BorderStyle := bsNone; // Before Create or the window handle may be recreated
  inherited CreateDefault(AOwner);
  Align := alClient;

  FFiles := nil;
  FFileSourceFiles := nil;
  FListFilesThread := nil;
  FReloading := False;
  FFileListBuilderLock := TCriticalSection.Create;
  FFileListBuilders := TFPList.Create;
  FSelection:= TStringListEx.Create;

  ActiveColm := '';
  ActiveColmSlave := nil;
  isSlave := False;
  FLastSelectionStartRow := -1;
  FLastMark := '*';
  FLastActive := '';
  FActive := False;
  FFileFilter := '';

  FSorting := nil;
  // default to sorting by 0-th column
  FSortColumn := 0;
  FSortDirection := sdAscending;

  // -- other components

  dgPanel:=TDrawGridEx.Create(Self, Self);

  pnlHeader:=TPanel.Create(Self);
  pnlHeader.Parent:=Self;
  pnlHeader.Align:=alTop;
  pnlHeader.BevelInner:=bvNone;
  pnlHeader.BevelOuter:=bvNone;
  pnlHeader.AutoSize := True;

  lblAddress := TPathLabel.Create(pnlHeader, False);
  lblAddress.Parent := pnlHeader;
  lblAddress.AutoSize := False;
  lblAddress.Height := lblAddress.Canvas.TextHeight('Wg');
  lblAddress.BorderSpacing.Bottom := 1;

  lblPath := TPathLabel.Create(pnlHeader, True);
  lblPath.Parent := pnlHeader;
  lblPath.AutoSize := False;
  lblPath.Height := lblPath.Canvas.TextHeight('Wg');

  // Display path below address.
  // For correct alignment, first put path at the top, then address at the top.
  lblPath.Align := alTop;
  lblAddress.Align := alTop;

  edtPath:=TEdit.Create(lblPath);
  edtPath.Parent:=pnlHeader;
  edtPath.Visible:=False;
  edtPath.TabStop:=False;

  pnlFooter:=TPanel.Create(Self);
  pnlFooter.Parent:=Self;
  pnlFooter.Align:=alBottom;
  pnlFooter.BevelInner:=bvNone;
  pnlFooter.BevelOuter:=bvNone;
  pnlFooter.AutoSize := True;

  lblInfo:=TLabel.Create(pnlFooter);
  lblInfo.Parent:=pnlFooter;
  lblInfo.AutoSize:=False;
  lblInfo.Height := lblInfo.Canvas.TextHeight('Wg');
  lblInfo.Align := alClient;

  edtRename:=TEdit.Create(dgPanel);
  edtRename.Parent:=dgPanel;
  edtRename.Visible:=False;
  edtRename.TabStop:=False;

  // now create search panel
  pnAltSearch:=TPanel.Create(Self);
  pnAltSearch.Parent:=Self;
  pnAltSearch.Caption:=rsQuickSearchPanel;
  pnAltSearch.Alignment:=taLeftJustify;
  pnAltSearch.Visible := False;

  edtSearch:=TEdit.Create(pnAltSearch);
  edtSearch.Parent:=pnAltSearch;
  edtSearch.TabStop:=False;
  edtSearch.Left:=64;
  edtSearch.Top:=1;

  // Create filter panel.
  pnlFilter := TPanel.Create(Self);
  pnlFilter.Parent := Self;
  pnlFilter.Visible := False;
  pnlFilter.Align := alBottom;
  pnlFilter.AutoSize := True;
  pnlFilter.Caption := rsQuickFilterPanel;
  pnlFilter.Alignment := taLeftJustify;
  pnlFilter.BevelWidth := 1;
  pnlFilter.BevelInner := bvSpace;

  edtFilter := TEdit.Create(pnlFilter);
  edtFilter.Parent := pnlFilter;
  edtFilter.TabStop := False;
  edtfilter.BorderSpacing.Left := 64;
  edtFilter.Align := alLeft;

  btnCloseFilter := TButton.Create(pnlFilter);
  btnCloseFilter.Parent := pnlFilter;
  btnCloseFilter.Align := alRight;
  btnCloseFilter.Caption := 'x';
  btnCloseFilter.AutoSize := True;

  tmContextMenu:= TTimer.Create(Self);
  tmContextMenu.Enabled:= False;
  tmContextMenu.Interval:= 500;

  // ---
  dgPanel.OnUTF8KeyPress := @UTF8KeyPressEvent;
  dgPanel.OnMouseLeave:= @dgPanelMouseLeave;
  dgPanel.OnMouseDown := @dgPanelMouseDown;
  dgPanel.OnStartDrag := @dgPanelStartDrag;
  dgPanel.OnMouseMove:= @dgPanelMouseMove;
  dgPanel.OnDragOver := @dgPanelDragOver;
  dgPanel.OnDragDrop:= @dgPanelDragDrop;
  dgPanel.OnEndDrag:= @dgPanelEndDrag;
  dgPanel.OnDblClick:=@dgPanelDblClick;
  dgPanel.OnEnter:=@dgPanelEnter;
  dgPanel.OnExit:=@dgPanelExit;
  dgPanel.OnKeyUp:=@dgPanelKeyUp;
  dgPanel.OnKeyDown:=@dgPanelKeyDown;
  dgPanel.OnHeaderClick:=@dgPanelHeaderClick;
  dgPanel.OnMouseWheelUp := @dgPanelMouseWheelUp;
  dgPanel.OnMouseWheelDown := @dgPanelMouseWheelDown;
  dgPanel.OnSelection:= @dgPanelSelection;
  dgPanel.OnShowHint:= @dgPanelShowHint;

  edtSearch.OnChange := @edtSearchChange;
  edtSearch.OnKeyDown := @edtSearchKeyDown;
  edtSearch.OnExit := @edtSearchExit;

  edtFilter.OnChange := @edtFilterChange;
  edtFilter.OnKeyDown := @edtFilterKeyDown;
  edtFilter.OnEnter := @edtFilterEnter;
  edtFilter.OnExit := @edtFilterExit;

  edtPath.OnKeyDown := @edtPathKeyDown;
  edtPath.OnExit := @edtPathExit;

  edtRename.OnKeyDown := @edtRenameKeyDown;
  edtRename.OnExit := @edtRenameExit;

  btnCloseFilter.OnClick := @btnCloseFilterClick;

  pnlHeader.OnResize := @pnlHeaderResize;

  lblPath.OnClick := @lblPathClick;
  lblPath.OnMouseUp := @lblPathMouseUp;

  tmContextMenu.OnTimer:= @tmContextMenuTimer;

  pmColumnsMenu := TPopupMenu.Create(Self);
  pmColumnsMenu.Parent := Self;
end;

destructor TColumnsFileView.Destroy;
var
  i: Integer;
begin
  if Assigned(FListFilesThread) then
  begin
    StopBackgroundWork;

    // Wait until all the builders finish.
    FListFilesThread.Finish;
    TFunctionThread.WaitForWithSynchronize(FListFilesThread);
    FListFilesThread := nil;
  end;

  if Assigned(FFileListBuilders) then
  begin
    // Now all the builders can be safely freed.
    for i := 0 to FFileListBuilders.Count - 1 do
    begin
      with TColumnsFileListBuilder(FFileListBuilders[i]) do
      begin
        if Working then
          DebugLn('Error: Builder still working!');
        Free;
      end;
    end;
    FreeAndNil(FFileListBuilders);
  end;

  FreeThenNil(FFileListBuilderLock);
  FreeThenNil(FSelection);

  if Assigned(FFiles) then
    FreeAndNil(FFiles);
  if Assigned(FFileSourceFiles) then
    FreeAndNil(FFileSourceFiles);
  if Assigned(FSorting) then
    FreeAndNil(FSorting);
  inherited Destroy;
end;

function TColumnsFileView.Clone(NewParent: TWinControl): TColumnsFileView;
begin
  Result := TColumnsFileView.Create(NewParent, Self);
end;

procedure TColumnsFileView.CloneTo(FileView: TFileView);
begin
  if Assigned(FileView) then
  begin
    inherited CloneTo(FileView);

    with FileView as TColumnsFileView do
    begin
      // Clone file source files before display files because they are the reference files.
      if Assigned(Self.FFileSourceFiles) then
        FFileSourceFiles := Self.FFileSourceFiles.Clone;
      FFiles := Self.FFiles.Clone(Self.FFileSourceFiles, FFileSourceFiles);

      FLastActive := Self.FLastActive;
      FLastMark := Self.FLastMark;
      FLastSelectionStartRow := Self.FLastSelectionStartRow;

      {
      // Those are only used temporarily, so probably don't need to be copied.
      fSearchDirect,
      fNext,
      fPrevious : Boolean;
      }

      if Self.FileFilter <> '' then
      begin
        edtFilter.Text := Self.FileFilter; // will trigger assiging to FileFilter
        FilterPanelVisible;
      end;

      FSorting := Self.FSorting.Clone;
      FSortColumn := Self.FSortColumn;
      FSortDirection := Self.FSortDirection;

      ActiveColm := Self.ActiveColm;
      ActiveColmSlave := nil;    // set to nil because only used in preview?
      isSlave := Self.isSlave;
    end;
  end;
end;

procedure TColumnsFileView.AddFileSource(aFileSource: IFileSource; aPath: String);
begin
  LastActive := '';

  inherited AddFileSource(aFileSource, aPath);

  if Assigned(OnChangeFileSource) then
    OnChangeFileSource(Self);

  dgPanel.Row := 0;

  UpdateAddressLabel;
end;

procedure TColumnsFileView.RemoveLastFileSource;
var
  FocusedFile: String;
begin
  // Temporary. Do this by remembering the file name in a list?
  FocusedFile := ExtractFileName(FileSource.CurrentAddress);

  inherited RemoveLastFileSource;

  if Assigned(OnChangeFileSource) then
    OnChangeFileSource(Self);

  SetActiveFile(FocusedFile);

  UpdateAddressLabel;
end;

procedure TColumnsFileView.MakeFileSourceFileList;

  function GetBuilder: TColumnsFileListBuilder;
  var
    i: Integer;
  begin
    Result := nil;

    // Search for an existing builder that has finished working.
    // Free all other unused builders.
    i := 0;
    while i < FFileListBuilders.Count do
    begin
      if not TColumnsFileListBuilder(FFileListBuilders[i]).Working then
      begin
        // Found unused builder.
        if not Assigned(Result) then
        begin
          Result := TColumnsFileListBuilder(FFileListBuilders[i]);
          Inc(i);
        end
        else
        begin
          // Delete unneeded builders that finished working.
          TColumnsFileListBuilder(FFileListBuilders[i]).Free;
          FFileListBuilders.Delete(i);
        end;
      end
      else
        Inc(i);
    end;

    // A free builder was not found (they may still be finishing), so create a new one.
    if not Assigned(Result) then
    begin
      Result := TColumnsFileListBuilder.Create;
      FFileListBuilders.Add(Result);
    end;
  end;

begin
  if csDestroying in ComponentState then
    Exit;

  {$IFDEF timeFileView}
  startTime := Now;
  DebugLn('---- Start ----');
  {$ENDIF}

  StopBackgroundWork;

  // Pass parameters to the builder
  // (it is unsafe to access them directly from the worker thread).
  FCurrentFileListBuilder := GetBuilder;
  FCurrentFileListBuilder.InitializeBeforeWork(Self);

  if gListFilesInThread then
  begin
    // Clear grid.
    if Assigned(FFileSourceFiles) then
    begin
      FFiles.Clear; // Clear references to files from the source.
      FreeAndNil(FFileSourceFiles);
    end;
    dgPanel.RowCount := dgPanel.FixedRows;

    // Display info that file list is being loaded (after assigning builder).
    UpdateInfoPanel;

    dgPanel.Cursor := crHourGlass;

    if not Assigned(FListFilesThread) then
      FListFilesThread := TFunctionThread.Create(False);
    FListFilesThread.QueueFunction(@FCurrentFileListBuilder.MakeFileSourceFileList);
  end
  else
  begin
    FCurrentFileListBuilder.MakeFileSourceFileList(nil);
  end;
end;

procedure TColumnsFileView.AfterMakeFileList;
begin
  DisplayFileListHasChanged;
  DoOnReload;
  dgPanel.Cursor := crDefault;
end;

procedure TColumnsFileView.DisplayFileListHasChanged;
begin
  // Update grid row count.
  dgPanel.RowCount := FFiles.Count
                    + dgPanel.FixedRows; // header rows
  RedrawGrid;

  SetActiveFile(LastActive);
  UpdateInfoPanel;
end;

procedure TColumnsFileView.ReDisplayFileList;
begin
  if Assigned(FCurrentFileListBuilder) then
  begin
    case FCurrentFileListBuilder.State of
      rfsNone: ; // already finished working so it's ok to continue
      rfsLoadingIcons:
        // Stop loading icons because they will be retrieved again below.
        StopBackgroundWork;
      else
        // File list is being loaded from file source - cannot display yet.
        Exit;
    end;
  end;

  // Redisplaying file list is done in the main thread because it takes
  // relatively short time, so the user usually won't notice it and it is
  // a bit faster this way.
  TColumnsFileListBuilder.MakeDisplayFileList(
    FileSource, FFileSourceFiles, FFiles, FileFilter);
  AfterMakeFileList;
end;

procedure TColumnsFileView.Reload(const PathsToReload: TPathsArray);
var
  i: Integer;
  bReload: Boolean;
begin
  if Assigned(PathsToReload) then
  begin
    bReload := False;

    for i := Low(PathsToReload) to High(PathsToReload) do
      if IsInPath(PathsToReload[i], CurrentPath, True) then
      begin
        bReload := True;
        break;
      end;

    if not bReload then
      Exit;
  end;

  FReloading := True;
  MakeFileSourceFileList;
end;

procedure TColumnsFileView.StopBackgroundWork;
begin
  if Assigned(FCurrentFileListBuilder) then
  begin
    // This lock may block temporarily until all access
    // to Files by the builder thread is finished.
    FFileListBuilderLock.Acquire;
    try
      FCurrentFileListBuilder.Abort;
    finally
      FFileListBuilderLock.Release;
    end;

    FCurrentFileListBuilder := nil;
  end;
end;

procedure TColumnsFileView.UpdateView;
var
  bLoadingFilelist: Boolean;
begin
  bLoadingFilelist := Assigned(FCurrentFileListBuilder) and
                      FCurrentFileListBuilder.IsWorking;

  if bLoadingFilelist then
    StopBackgroundWork;

  pnlHeader.Visible := gCurDir;  // Current directory
  pnlFooter.Visible := gStatusBar;  // Status bar
  GridVertLine:= gGridVertLine;
  GridHorzLine:= gGridHorzLine;

  UpdateAddressLabel;
  UpdatePathLabel;
  dgPanel.UpdateView;
  UpdateColumnsView;

  if bLoadingFilelist then
    MakeFileSourceFileList
  else if Assigned(FFiles) then  // This condition is needed when cloning.
    ReDisplayFileList;
end;

function TColumnsFileView.GetActiveItem: TColumnsViewFile;
var
  CurrentRow: Integer;
begin
  if not IsEmpty then
  begin
    CurrentRow := dgPanel.Row;
    if CurrentRow < dgPanel.FixedRows then
      CurrentRow := dgPanel.FixedRows
    else if CurrentRow > FFiles.Count then
       CurrentRow := dgPanel.FixedRows;

    Result := FFiles[CurrentRow - dgPanel.FixedRows]; // minus fixed header
  end
  else
    Result := nil;  // No files in the panel.
end;

function TColumnsFileView.GetColumnsClass: TPanelColumnsClass;
begin
  if isSlave then
    Result := ActiveColmSlave
  else
    Result := ColSet.GetColumnSet(ActiveColm);
end;

procedure TColumnsFileView.CalculateSpaceOfAllDirectories;
var
  i: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    for i := 0 to FFiles.Count - 1 do
      if IsItemValid(FFiles[i]) and FFiles[i].TheFile.IsDirectory then
        CalculateSpace(FFiles[i]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TColumnsFileView.CalculateSpace(theFile: TColumnsViewFile);
var
  Operation: TFileSourceOperation = nil;
  CalcStatisticsOperation: TFileSourceCalcStatisticsOperation;
  CalcStatisticsOperationStatistics: TFileSourceCalcStatisticsOperationStatistics;
  TargetFiles: TFiles = nil;
begin
  if (fsoCalcStatistics in FileSource.GetOperationsTypes) and
     (fpSize in theFile.TheFile.SupportedProperties) and
     theFile.TheFile.IsDirectory then
  begin
    TargetFiles := FileSource.CreateFiles(CurrentPath);
    try
      TargetFiles.Add(theFile.TheFile.Clone);

      Operation := FileSource.CreateCalcStatisticsOperation(TargetFiles);
      CalcStatisticsOperation := Operation as TFileSourceCalcStatisticsOperation;
      CalcStatisticsOperation.SkipErrors := True;
      CalcStatisticsOperation.SymLinkOption := fsooslDontFollow;

      Operation.Execute; // blocks until finished

      CalcStatisticsOperationStatistics := CalcStatisticsOperation.RetrieveStatistics;

      (theFile.TheFile.Properties[fpSize] as TFileSizeProperty).Value := CalcStatisticsOperationStatistics.Size;

      RedrawGrid;

      // Needed to not block GUI as we're not executing operation in a thread.
      Application.ProcessMessages;

    finally
      if Assigned(TargetFiles) then
        FreeAndNil(TargetFiles);
      if Assigned(Operation) then
        FreeAndNil(Operation);
    end;
  end;
end;

procedure TColumnsFileView.UTF8KeyPressEvent(Sender: TObject; var UTF8Key: TUTF8Char);


  function CheckSearchOrFilter(ModifierKeys: TShiftState;
                               SearchOrFilterEnabled: Boolean;
                               SearchOrFilterMode: TShiftState): Boolean;
  begin
    if SearchOrFilterEnabled and (SearchOrFilterMode = []) and
       // Check only ssCtrl and ssAlt.
       (ModifierKeys * [ssCtrl, ssAlt] = SearchOrFilterMode) then
      begin
        // Make upper case if either caps-lock is toggled or shift pressed.
        if (ssCaps in ModifierKeys) xor (ssShift in ModifierKeys) then
          UTF8Key := UTF8UpperCase(UTF8Key)
        else
          UTF8Key := UTF8LowerCase(UTF8Key);

        Result := True;
      end
    else
      Result := False;
  end;

var
  ModifierKeys: TShiftState;
begin
  // quick search by Letter only

  // Check for certain Ascii keys.
  if (Length(UTF8Key) = 1) and ((Ord(UTF8Key[1]) <= 32) or
     (UTF8Key[1] in ['+','-','*','/','\'])) then Exit;

  ModifierKeys := GetKeyShiftStateEx;

  if CheckSearchOrFilter(ModifierKeys, gQuickSearch, gQuickSearchMode) then
  begin
    ShowSearchPanel(UTF8Key);
    UTF8Key := '';
  end
  else if CheckSearchOrFilter(ModifierKeys, gQuickFilter, gQuickFilterMode) then
  begin
    ShowFilterPanel(UTF8Key);
    UTF8Key := '';
  end
end;

procedure TColumnsFileView.DoDragDropOperation(Operation: TDragDropOperation;
                                               var DropParams: TDropParams);
var
  AFile: TColumnsViewFile;
  iCol, iRow: Integer;
  ClientDropPoint: TPoint;
begin
  try
    with DropParams do
    begin
      if Files.Count > 0 then
      begin
        ClientDropPoint := dgPanel.ScreenToClient(ScreenDropPoint);
        dgPanel.MouseToCell(ClientDropPoint.X, ClientDropPoint.Y, iCol, iRow);

        // default to current active directory in the destination panel
        TargetPath := Self.CurrentPath;

        if (DropIntoDirectories = True) and
           (iRow >= dgPanel.FixedRows) and
           (ClientDropPoint.Y < dgPanel.GridHeight) then
        begin
          AFile := FFiles[iRow - dgPanel.FixedRows];

          // If dropped into a directory modify destination path accordingly.
          if Assigned(AFile) and
             (AFile.TheFile.IsDirectory or AFile.TheFile.IsLinkToDirectory) then
          begin
            if AFile.TheFile.Name = '..' then
              // remove the last subdirectory in the path
              TargetPath := GetParentDir(TargetPath)
            else
              TargetPath := TargetPath + AFile.TheFile.Name + DirectorySeparator;
          end;
        end;
      end;
    end;

    // Execute the operation.
    frmMain.DoDragDropOperation(Operation, DropParams);

  finally
    if Assigned(DropParams) then
      FreeAndNil(DropParams);
  end;
end;

procedure TColumnsFileView.cm_MarkInvert(param: string='');
begin
  InvertAll;
end;

procedure TColumnsFileView.cm_MarkMarkAll(param: string='');
begin
  MarkAll;
end;

procedure TColumnsFileView.cm_MarkUnmarkAll(param: string='');
begin
  UnMarkAll;
end;

procedure TColumnsFileView.cm_MarkPlus(param: string='');
begin
  MarkPlus;
end;

procedure TColumnsFileView.cm_MarkMinus(param: string='');
begin
  MarkMinus;
end;

procedure TColumnsFileView.cm_MarkCurrentExtension(param: string='');
begin
  MarkShiftPlus;
end;

procedure TColumnsFileView.cm_UnmarkCurrentExtension(param: string='');
begin
  MarkShiftMinus;
end;

procedure TColumnsFileView.cm_SaveSelection(param: string);
begin
  SaveSelection;
end;

procedure TColumnsFileView.cm_RestoreSelection(param: string);
begin
  RestoreSelection;
end;

procedure TColumnsFileView.cm_SaveSelectionToFile(param: string);
begin
  with dmComData do
  begin
    SaveDialog.DefaultExt:= '.txt';
    SaveDialog.Filter:= '*.txt|*.txt';
    SaveDialog.FileName:= param;
    if (param <> EmptyStr) or SaveDialog.Execute then
      try
        SaveSelection;
        FSelection.SaveToFile(SaveDialog.FileName);
      except
        on E: Exception do
          msgError(rsMsgErrSaveFile + '-' + E.Message);
      end;
  end;
end;

procedure TColumnsFileView.cm_LoadSelectionFromFile(param: string);
begin
  with dmComData do
  begin
    OpenDialog.DefaultExt:= '.txt';
    OpenDialog.Filter:= '*.txt|*.txt';
    OpenDialog.FileName:= param;
    if ((param <> EmptyStr) and mbFileExists(param)) or OpenDialog.Execute then
      try
        FSelection.LoadFromFile(OpenDialog.FileName);
        RestoreSelection;
      except
        on E: Exception do
          msgError(rsMsgErrEOpen + '-' + E.Message);
      end;
  end;
end;

procedure TColumnsFileView.cm_LoadSelectionFromClip(param: string);
begin
  FSelection.Text:= Clipboard.AsText;
  RestoreSelection;
end;

procedure TColumnsFileView.cm_QuickSearch(param: string='');
begin
  ShowSearchPanel;
end;

procedure TColumnsFileView.cm_QuickFilter(param: string='');
begin
  ShowFilterPanel;
end;

procedure TColumnsFileView.cm_Open(param: string='');
begin
  if Assigned(GetActiveItem) then
    ChooseFile(GetActiveItem);
end;

procedure TColumnsFileView.cm_ReverseOrder(param:string);
begin
  FSortDirection := ReverseSortDirection(FSortDirection);
  SortByColumn(FSortColumn);
end;

procedure TColumnsFileView.cm_SortByColumn(param: string='');
var
  ColumnNumber: Integer;
begin
  if TryStrToInt(param, ColumnNumber) then
  begin
    if FSortColumn = ColumnNumber then
      FSortDirection := ReverseSortDirection(FSortDirection)
    else
      FSortDirection := sdAscending;

    SortByColumn(ColumnNumber);
  end;
end;

procedure TColumnsFileView.cm_CountDirContent(param: string='');
begin
  CalculateSpaceOfAllDirectories;
end;

procedure TColumnsFileView.cm_RenameOnly(param: string='');
var
  aFile: TFile;
begin
  if (fsoSetFileProperty in FileSource.GetOperationsTypes) then
    begin
      aFile:= ActiveFile;
      if Assigned(aFile) and aFile.IsNameValid then
        begin
          ShowRenameFileEdit(CurrentPath + aFile.Name);
        end;
    end;
end;

procedure TColumnsFileView.cm_ContextMenu(param: string='');
var
  Rect: TRect;
  Point: TPoint;
begin
  Rect := dgPanel.CellRect(0, dgPanel.Row);
  Point.X := Rect.Left + ((Rect.Right - Rect.Left) div 2);
  Point.Y := Rect.Top + ((Rect.Bottom - Rect.Top) div 2);
  Point := dgPanel.ClientToScreen(Point);
  Actions.DoContextMenu(Self, Point.X, Point.Y);
end;

procedure TColumnsFileView.cm_EditPath(param: string);
begin
  ShowPathEdit;
end;

{ TDrawGridEx }

constructor TDrawGridEx.Create(AOwner: TComponent; AParent: TWinControl);
begin
  // Initialize D&D before calling inherited create,
  // because it will create the control and call InitializeWnd.
  DragDropSource := nil;
  DragDropTarget := nil;
  TransformDragging := False;
  FMouseDown := False;

{$IFDEF LCLGTK2}
  FLastDoubleClickTime := Now;
{$ENDIF}

  inherited Create(AOwner);

  Self.Parent := AParent;
  ColumnsView := AParent as TColumnsFileView;

  StartDrag := False;
  DropRowIndex := -1;
  HintRowIndex := -1;

  DoubleBuffered := True;
  Align := alClient;
  Options := [goFixedVertLine, goFixedHorzLine, goTabs, goRowSelect,
              goColSizing, goThumbTracking, goSmoothScroll];

  TitleStyle := tsStandard;
  TabStop := False;

  UpdateView;
end;

destructor TDrawGridEx.Destroy;
begin
  inherited;
end;

procedure TDrawGridEx.UpdateView;

  function CalculateDefaultRowHeight: Integer;
  var
    OldFont, NewFont: TFont;
    i: Integer;
    MaxFontHeight: Integer = 0;
    CurrentHeight: Integer;
    ColumnsSet: TPanelColumnsClass;
  begin
    // Start with height of the icons.
    if gShowIcons <> sim_none then
      MaxFontHeight := gIconsSize;

    // Get columns settings.
    with (Parent as TColumnsFileView) do
    begin
      if not isSlave then
        ColumnsSet := ColSet.GetColumnSet(ActiveColm)
      else
        ColumnsSet := ActiveColmSlave;
    end;

    // Assign temporary font.
    OldFont     := Canvas.Font;
    NewFont     := TFont.Create;
    Canvas.Font := NewFont;

    // Search columns settings for the biggest font (in height).
    for i := 0 to ColumnsSet.Count - 1 do
    begin
      Canvas.Font.Name  := ColumnsSet.GetColumnFontName(i);
      Canvas.Font.Style := ColumnsSet.GetColumnFontStyle(i);
      Canvas.Font.Size  := ColumnsSet.GetColumnFontSize(i);

      CurrentHeight := Canvas.GetTextHeight('Wg');
      MaxFontHeight := Max(MaxFontHeight, CurrentHeight);
    end;

    // Restore old font.
    Canvas.Font := OldFont;
    FreeAndNil(NewFont);

    Result := MaxFontHeight;
  end;

var
  TabHeaderHeight: Integer;
  TempRowHeight: Integer;
begin
  Flat := gInterfaceFlat;
  Color := ColumnsView.DimColor(gBackColor);
  AutoFillColumns:= gAutoFillColumns;
  ShowHint:= (gShowToolTipMode <> []);

  // Calculate row height.
  TempRowHeight := CalculateDefaultRowHeight;
  if TempRowHeight > 0 then
    DefaultRowHeight := TempRowHeight;

  // Set rows of header.
  if gTabHeader then
  begin
    if RowCount < 1 then
      RowCount := 1;

    FixedRows := 1;

    TabHeaderHeight := Max(gIconsSize, Canvas.TextHeight('Wg'));
    if not gInterfaceFlat then
    begin
      TabHeaderHeight := TabHeaderHeight + 2;
    end;
    RowHeights[0] := TabHeaderHeight;
  end
  else
    FixedRows := 0;

  FixedCols := 0;
end;

procedure TDrawGridEx.InitializeWnd;
begin
  inherited;

  // Register as drag&drop source and target.
  DragDropSource := uDragDropEx.CreateDragDropSource(Self);
  if Assigned(DragDropSource) then
    DragDropSource.RegisterEvents(nil, nil, @OnExDragEnd);

  DragDropTarget := uDragDropEx.CreateDragDropTarget(Self);
  if Assigned(DragDropTarget) then
    DragDropTarget.RegisterEvents(@OnExDragEnter,@OnExDragOver,
                                  @OnExDrop,@OnExDragLeave);
end;

procedure TDrawGridEx.FinalizeWnd;
begin
  if Assigned(DragDropSource) then
    FreeAndNil(DragDropSource);
  if Assigned(DragDropTarget) then
    FreeAndNil(DragDropTarget);

  inherited;
end;

procedure TDrawGridEx.DrawCell(aCol, aRow: Integer; aRect: TRect;
              aState: TGridDrawState);
var
  //shared variables
  s:   string;
  iTextTop: Integer;
  AFile: TColumnsViewFile;
  FileSourceDirectAccess: Boolean;
  ColumnsSet: TPanelColumnsClass;

  //------------------------------------------------------
  //begin subprocedures
  //------------------------------------------------------

  procedure DrawFixed;
  //------------------------------------------------------
  var
    SortingDirection: TSortDirection;
    TitleX: Integer;
  begin
    // Draw background.
    Canvas.Brush.Color := GetColumnColor(ACol, True);
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(aRect);

    Canvas.Brush.Style := bsClear;
    SetCanvasFont(GetColumnFont(aCol, True));

    iTextTop := aRect.Top + (RowHeights[aRow] - Canvas.TextHeight('Wg')) div 2;

    TitleX := 0;
    s      := ColumnsSet.GetColumnTitle(ACol);

    SortingDirection := ColumnsView.FSorting.GetSortingDirection(ACol);
    if SortingDirection <> sdNone then
    begin
      TitleX := TitleX + gIconsSize;
      PixMapManager.DrawBitmap(
          PixMapManager.GetIconBySortingDirection(SortingDirection),
          Canvas,
          aRect.Left, aRect.Top);
    end;

    TitleX := max(TitleX, 4);

    if gCutTextToColWidth then
    begin
      if (aRect.Right - aRect.Left) < TitleX then
        // Column too small to display text.
        Exit
      else
        while Canvas.TextWidth(s) - ((aRect.Right - aRect.Left) - TitleX) > 0 do
          UTF8Delete(s, UTF8Length(s), 1);
    end;

    Canvas.TextOut(aRect.Left + TitleX, iTextTop, s);
  end; // of DrawHeader
  //------------------------------------------------------


  procedure DrawIconCell;
  //------------------------------------------------------
  begin
    Canvas.Brush.Style := bsClear;

    if (AFile.IconID >= 0) and (gShowIcons <> sim_none) then
    begin
      PixMapManager.DrawBitmap(AFile.IconID,
                               AFile.TheFile,
                               FileSourceDirectAccess,
                               Canvas,
                               aRect.Left + 1,
                               // center icon vertically
                               aRect.Top + (RowHeights[ARow] - gIconsSize) div 2);
    end;

    s := ColumnsSet.GetColumnItemResultString(ACol, AFile.TheFile);

    if gCutTextToColWidth then
    begin
      while Canvas.TextWidth(s) - (aRect.Right - aRect.Left) - 4 > 0 do
        Delete(s, Length(s), 1);
    end;

    if (gShowIcons <> sim_none) then
      Canvas.TextOut(aRect.Left + gIconsSize + 4, iTextTop, s)
    else
      Canvas.TextOut(aRect.Left + 2, iTextTop, s);
  end; //of DrawIconCell
  //------------------------------------------------------

  procedure DrawOtherCell;
  //------------------------------------------------------
  var
    tw, cw: Integer;
  begin
    s := ColumnsSet.GetColumnItemResultString(ACol, AFile.TheFile);

    if gCutTextToColWidth then
    begin
      while Canvas.TextWidth(s) - (aRect.Right - aRect.Left) - 4 > 0 do
        Delete(s, Length(s), 1);
    end;

    Canvas.Brush.Style := bsClear;

    case ColumnsSet.GetColumnAlign(ACol) of

      taRightJustify:
        begin
          cw := ColWidths[ACol];
          tw := Canvas.TextWidth(s);
          Canvas.TextOut(aRect.Left + cw - tw - 3, iTextTop, s);
        end;

      taLeftJustify:
        begin
          Canvas.TextOut(aRect.Left + 3, iTextTop, s);
        end;

      taCenter:
        begin
          cw := ColWidths[ACol];
          tw := Canvas.TextWidth(s);
          Canvas.TextOut(aRect.Left + ((cw - tw - 3) div 2), iTextTop, s);
        end;

    end; //of case
  end; //of DrawOtherCell
  //------------------------------------------------------

  procedure NewPrepareColors;
  //------------------------------------------------------
  var
    newColor, BackgroundColor: TColor;

    procedure TextSelect;
    //---------------------
    var
      tmp: TColor;
    begin
      tmp := ColumnsSet.GetColumnTextColor(ACol);
      if (tmp <> newColor) and (newColor <> -1) and
         (ColumnsSet.GetColumnOvercolor(ACol))
      then
        Canvas.Font.Color := newColor
      else
        Canvas.Font.Color := tmp;
    end;
    //---------------------
  begin
    Canvas.Font.Name   := ColumnsSet.GetColumnFontName(ACol);
    Canvas.Font.Size   := ColumnsSet.GetColumnFontSize(ACol);
    Canvas.Font.Style  := ColumnsSet.GetColumnFontStyle(ACol);

    // Set up default brush color first
    if (gdSelected in aState) and ColumnsView.FActive then
      BackgroundColor := ColumnsSet.GetColumnCursorColor(ACol)
    else
      begin
        // Alternate rows background color.
        if odd(ARow) then
          BackgroundColor := ColumnsSet.GetColumnBackground(ACol)
        else
          BackgroundColor := ColumnsSet.GetColumnBackground2(ACol);
      end;

    newColor := gColorExt.GetColorBy(AFile.TheFile);

    if AFile.Selected then
    begin
      if gUseInvertedSelection then
        begin
          //------------------------------------------------------
          if (gdSelected in aState) and ColumnsView.FActive then
            begin
              BackgroundColor := ColumnsSet.GetColumnCursorColor(ACol);
              Canvas.Font.Color := InvertColor(ColumnsSet.GetColumnCursorText(ACol));
            end
          else
            begin
              BackgroundColor := ColumnsSet.GetColumnMarkColor(ACol);
              TextSelect;
            end;
          //------------------------------------------------------
        end
      else
        begin
          Canvas.Font.Color := ColumnsSet.GetColumnMarkColor(ACol);
        end;
    end
    else if (gdSelected in aState) and ColumnsView.FActive then
      begin
        Canvas.Font.Color := ColumnsSet.GetColumnCursorText(ACol);
      end
    else
      begin
        TextSelect;
      end;

    // Draw background.
    Canvas.Brush.Color := ColumnsView.DimColor(BackgroundColor);
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(aRect);

    // Draw drop selection.
    if ARow = DropRowIndex then
    begin
      Canvas.Pen.Color := ColumnsSet.GetColumnTextColor(ACol);
      Canvas.Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
      Canvas.Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
    end;
  end;// of NewPrepareColors;

  //------------------------------------------------------
  //end of subprocedures
  //------------------------------------------------------

begin
  with ColumnsView do
  begin
    if not isSlave then
      ColumnsSet := ColSet.GetColumnSet(ActiveColm)
    else
      ColumnsSet := ActiveColmSlave;
  end;

  if gdFixed in aState then
  begin
    DrawFixed  // Draw column headers
  end
  else if ColumnsView.FFiles.Count > 0 then
  begin
    AFile := ColumnsView.FFiles[ARow - FixedRows]; // substract fixed rows (header)
    FileSourceDirectAccess := fspDirectAccess in ColumnsView.FileSource.Properties;

    NewPrepareColors;

    iTextTop := aRect.Top + (RowHeights[aRow] - Canvas.TextHeight('Wg')) div 2;

    if ACol = 0 then
      DrawIconCell  // Draw icon in the first column
    else
      DrawOtherCell;
  end;

  Canvas.Brush.Style := bsSolid;
  DrawCellGrid(aCol,aRow,aRect,aState);
end;

procedure TDrawGridEx.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Point: TPoint;
  AFile: TColumnsViewFile;
  ExpectedButton: TShiftStateEnum;
  iCol, iRow: Integer;
  aRect: TRect;
begin
  inherited MouseMove(Shift, X, Y);

  if FMouseDown and Self.Dragging then
  begin
    // If dragging has started then clear MouseDown flag.
    if (Abs(DragStartPoint.X - X) > DragManager.DragThreshold) or
       (Abs(DragStartPoint.Y - Y) > DragManager.DragThreshold) then
    begin
      FMouseDown := False;
    end;
  end;

  // If dragging is currently in effect, the window has mouse capture and
  // we can retrieve the window over which the mouse cursor currently is.
  if Self.Dragging and uDragDropEx.IsExternalDraggingSupported then
  begin
    Point := Self.ClientToScreen(Classes.Point(X, Y));

    // use specifically LCLIntf.WindowFromPoint to avoid confusion with Windows.WindowFromPoint
    if LCLIntf.WindowFromPoint(Point) = 0 then
    begin
      // If result is 0 then the window belongs to another process
      // and we transform intra-process dragging into inter-process dragging.

      TransformDraggingToExternal(Point);
    end;
  end

  else

  // if we are about to start dragging
  if StartDrag then
    begin
      StartDrag := False;

      case LastMouseButton of
        mbLeft   : ExpectedButton := ssLeft;
        mbMiddle : ExpectedButton := ssMiddle;
        mbRight  : ExpectedButton := ssRight;
        else       Exit;
      end;

      // Make sure the same mouse button is still pressed.
      if not (ExpectedButton in Shift) then
      begin
        ClearMouseButtonAfterDrag;
      end
      else if DragRowIndex >= FixedRows then
      begin
        AFile := (Parent as TColumnsFileView).FFiles[DragRowIndex - FixedRows]; // substract fixed rows (header)
        // Check if valid item is being dragged.
        if (Parent as TColumnsFileView).IsItemValid(AFile) then
        begin
          BeginDrag(False);
        end;
      end;
    end;

  // Show file info tooltip
  if ShowHint then
    begin
      if Y < GridHeight then
        begin
          MouseToCell(X, Y, iCol, iRow);
          if (iRow <> HintRowIndex) and (iRow >= FixedRows) then
            begin
              aRect:= CellRect(0, iRow);
              HintRowIndex:= iRow;
              Application.CancelHint;
              Self.Hint:= EmptyStr; // don't show by default
              with (Parent as TColumnsFileView) do
              begin
                AFile := FFiles[HintRowIndex - FixedRows];
                iCol:= aRect.Right - aRect.Left - 8;
                if gShowIcons <> sim_none then
                  Dec(iCol, gIconsSize);
                if iCol < Self.Canvas.TextWidth(AFile.TheFile.Name) then // with file name
                    Self.Hint:= AFile.TheFile.Name
                else if (stm_only_large_name in gShowToolTipMode) then // don't show
                  Exit
                else if not AFile.TheFile.IsDirectory then // without name
                  Self.Hint:= #32;
              end;
            end;
        end
      else
        begin
          HintRowIndex:= -1;
          Application.CancelHint;
          Self.Hint:= EmptyStr;
        end;
    end;
end;

procedure TDrawGridEx.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
{$IFDEF LCLGTK2}
  // Workaround for two doubleclicks being sent on GTK.
  // MouseUp event is sent just after doubleclick, so if we drop
  // doubleclick events we have to also drop MouseUp events that follow them.
  if TooManyDoubleClicks then Exit;
{$ENDIF}

  StartDrag := False;

  inherited MouseUp(Button, Shift, X, Y);

  // Call handler only if button-up was not lifted to finish drag&drop operation.
  if FMouseDown then
  begin
    (Parent as TColumnsFileView).dgPanelMouseUp(Self, Button, Shift, X, Y);
    FMouseDown := False;
  end;
end;

procedure TDrawGridEx.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
{$IFDEF LCLGTK2}
  // Workaround for two doubleclicks being sent on GTK.
  // MouseDown event is sent just before doubleclick, so if we drop
  // doubleclick events we have to also drop MouseDown events that precede them.
  if TooManyDoubleClicks then Exit;
{$ENDIF}

  FMouseDown := True;

  inherited;
end;

function TDrawGridEx.GetHeaderHeight: Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := 0 to FixedRows-1 do
    Result := Result + RowHeights[i];
  if Flat and (BorderStyle = bsSingle) then // TCustomGrid.GetBorderWidth
    Result := Result + 1;
end;

procedure TDrawGridEx.ChangeDropRowIndex(NewIndex: Integer);
var
  OldDropRowIndex: Integer;
begin
  if DropRowIndex <> NewIndex then
  begin
    OldDropRowIndex := DropRowIndex;

    // Set new index before redrawing.
    DropRowIndex := NewIndex;

    if OldDropRowIndex >= 0 then // invalidate old row if need
      InvalidateRow(OldDropRowIndex);
    if NewIndex >= 0 then
      InvalidateRow(NewIndex);
  end;
end;

procedure TDrawGridEx.TransformDraggingToExternal(ScreenPoint: TPoint);
var
  SourcePanel: TColumnsFileView;
begin
  // Set flag temporarily before stopping internal dragging,
  // so that triggered events will know that dragging is transforming.
  TransformDragging := True;

  // Stop internal dragging
  DragManager.DragStop(False);

{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  // Under GTK, DragManager does not release it's mouse capture on
  // DragStop(). We must release it here manually or LCL will get confused
  // with who "owns" the capture after the GTK drag&drop finishes.
  ReleaseMouseCapture;
{$ENDIF}

  // Clear flag before starting external dragging.
  TransformDragging := False;

  SourcePanel := (Parent as TColumnsFileView);

  // Start external dragging.
  // On Windows it does not return until dragging is finished.

  SourcePanel.StartDragEx(LastMouseButton, ScreenPoint);
end;

function TDrawGridEx.OnExDragEnter(var DropEffect: TDropEffect; ScreenPoint: TPoint):Boolean;
begin
  Result := True;
end;

function TDrawGridEx.OnExDragOver(var DropEffect: TDropEffect; ScreenPoint: TPoint):Boolean;
var
  ClientPoint: TPoint;
  Dummy, iRow: Integer;
  AFile: TColumnsViewFile = nil;
  TargetPanel: TColumnsFileView = nil;
begin
  Result := False;

  ClientPoint := Self.ScreenToClient(ScreenPoint);

  TargetPanel := (Self.Parent as TColumnsFileView);

  // Allow dropping into empty panel or on the header.
  if TargetPanel.IsEmpty or (ClientPoint.Y < GetHeaderHeight) then
  begin
    ChangeDropRowIndex(-1);
    Result := True;
    Exit;
  end;

  MouseToCell(ClientPoint.X, ClientPoint.Y, Dummy, iRow);

  if iRow >= FixedRows then
    // Get the item over which there is something dragged.
    AFile := TargetPanel.FFiles[iRow - FixedRows]; // substract fixed rows (header)

  if Assigned(AFile) and
     (AFile.TheFile.IsDirectory or AFile.TheFile.IsLinkToDirectory) and
     (ClientPoint.Y < GridHeight) then
    // It is a directory or link.
    begin
      ChangeDropRowIndex(iRow);
      Result := True;
    end
  else
    begin
      ChangeDropRowIndex(-1);
      Result := True;
    end;
end;

function TDrawGridEx.OnExDrop(const FileNamesList: TStringList; DropEffect: TDropEffect;
                              ScreenPoint: TPoint):Boolean;
var
  Files: TFiles;
  DropParams: TDropParams;
  TargetFileView: TFileView;
begin
  if FileNamesList.Count > 0 then
  begin
    Files := TFileSystemFiles.CreateFromFiles(
        ExtractFilePath(FileNamesList[0]), FileNamesList);
    try
      TargetFileView := Self.Parent as TFileView;

      DropParams := TDropParams.Create(
        Files, DropEffect, ScreenPoint, True,
        nil, TargetFileView, TargetFileView.CurrentPath);

      frmMain.DropFiles(DropParams);
    except
      FreeAndNil(Files);
      raise;
    end;
  end;

  ChangeDropRowIndex(-1);
  Result := True;
end;

function TDrawGridEx.OnExDragLeave: Boolean;
begin
  ChangeDropRowIndex(-1);
  Result := True;
end;

function TDrawGridEx.OnExDragBegin: Boolean;
begin
  Result := True;
end;

function TDrawGridEx.OnExDragEnd: Boolean;
{$IF DEFINED(MSWINDOWS)}
var
  startPoint: TPoint;
  currentPoint: TPoint;
{$ENDIF}
begin
{$IF DEFINED(MSWINDOWS)}
  // On windows dragging can be transformed back into internal.
  // Check if drag was aborted due to mouse moving back into
  // the application window or the user just cancelled it.
  if (DragDropSource.GetLastStatus = DragDropAborted) and
     TransformDragging then
  begin
    // Transform to internal dragging again.

    // Save current mouse position.
    GetCursorPos(currentPoint);

    // Temporarily set cursor position to the point where the drag was started
    // so that DragManager can properly read the control being dragged.
    startPoint := ClientToScreen(Self.DragStartPoint);
    SetCursorPos(startPoint.X,startPoint.Y);

    // Begin internal dragging.
    BeginDrag(True);

    // Move cursor back.
    SetCursorPos(currentPoint.X, currentPoint.Y);

    // Clear flag.
    TransformDragging := False;

    Exit;
  end;
{$ENDIF}

  ClearMouseButtonAfterDrag;

  Result := True;
end;

procedure TDrawGridEx.ClearMouseButtonAfterDrag;
begin
  // Clear some control specific flags.
  ControlState := ControlState - [csClicked, csLButtonDown];

  // reset TCustomGrid state
  FGridState := gsNormal;
end;

{$IFDEF LCLGTK2}
function TDrawGridEx.TooManyDoubleClicks: Boolean;
begin
  Result := ((Now - fLastDoubleClickTime) <= ((1/86400)*(DblClickTime/1000)));
end;
{$ENDIF}

{ TPathLabel }

constructor TPathLabel.Create(AOwner: TComponent; AllowHighlight: Boolean);
begin
  FLeftSpacing := 3; // set before painting

  inherited Create(AOwner);

  SelectedDir := '';

  HighlightStartPos := -1;
  HighlightText := '';

  SetActive(False);

  if AllowHighlight then
  begin
    OnMouseEnter:=@MouseEnterEvent;
    OnMouseMove :=@MouseMoveEvent;
    OnMouseLeave:=@MouseLeaveEvent;
  end;
end;

procedure TPathLabel.Paint;
begin
  Canvas.Brush.Color := Color;
  Canvas.Font.Color  := Font.Color;

  Canvas.FillRect(0, 0, Width, Height); // background
  Canvas.TextOut(LeftSpacing, 0, Text); // path

  // Highlight part of the path if mouse is over it.
  if HighlightStartPos <> -1 then
  begin
    Canvas.Brush.Color := Font.Color;  // reverse colors
    Canvas.Font.Color  := Color;
    Canvas.TextOut(HighlightStartPos, 0, HighlightText);
  end;
end;

procedure TPathLabel.SetActive(Active: Boolean);
begin
  case Active of
    False:
      begin
        Color      := clBtnFace;
        Font.Color := clBtnText;
      end;
    True:
      begin
        Color      := clHighlight;
        Font.Color := clHighlightText;
      end;
  end;
end;

procedure TPathLabel.Highlight(MousePosX, MousePosY: Integer);
var
  PartText: String;
  StartPos, CurPos: Integer;
  PartWidth: Integer;
  CurrentHighlightPos, NewHighlightPos: Integer;
  TextLen: Integer;
  PathDelimWidth: Integer;
begin
  CurrentHighlightPos := LeftSpacing; // start at the beginning of the path
  NewHighlightPos := -1;

  Canvas.Font := Self.Font;
  PathDelimWidth := Canvas.TextWidth(PathDelim);
  TextLen := Length(Text);

  // Start from the first character, but omit any path delimiters at the beginning.
  StartPos := 1;
  while (StartPos <= TextLen) and (Text[StartPos] = PathDelim) do
    Inc(StartPos);

  // Move the canvas position after the skipped text (if any).
  CurrentHighlightPos := CurrentHighlightPos + (StartPos - 1) * PathDelimWidth;

  for CurPos := StartPos + 1 to TextLen - 1 do
  begin
    if Text[CurPos] = PathDelim then
    begin
      PartText := Copy(Text, StartPos, CurPos - StartPos);
      PartWidth := Canvas.TextWidth(PartText);

      // If mouse is over this part of the path - highlight it.
      if InRange(MousePosX, CurrentHighlightPos, CurrentHighlightPos + PartWidth) then
      begin
        NewHighlightPos := CurrentHighlightPos;
        Break;
      end;

      CurrentHighlightPos := CurrentHighlightPos + PartWidth + PathDelimWidth;
      StartPos := CurPos + 1;
    end;
  end;

  // Repaint if highlighted part has changed.
  if NewHighlightPos <> HighlightStartPos then
  begin
    // Omit minimized part of the displayed path.
    if PartText = '..' then
      HighlightStartPos := -1
    else
      HighlightStartPos := NewHighlightPos;

    if HighlightStartPos <> -1 then
    begin
      Cursor := crHandPoint;

      HighlightText := PartText;
      // If clicked, this will be the new directory.
      SelectedDir := Copy(Text, 1, CurPos - 1);
    end
    else
    begin
      Cursor := crDefault;

      SelectedDir := '';
      HighlightText := '';
    end;

    Self.Invalidate;
  end;
end;

procedure TPathLabel.MouseEnterEvent(Sender: TObject);
begin
  Cursor := crDefault;
end;

procedure TPathLabel.MouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Highlight(X, Y);
end;

procedure TPathLabel.MouseLeaveEvent(Sender: TObject);
begin
  SelectedDir := '';
  HighlightStartPos := -1;
  HighlightText := '';
  Cursor := crDefault;
  Invalidate;
end;

function TPathLabel.GetSelectedDir: String;
begin
  Result := SelectedDir;
end;

// -- TFileListSorting --------------------------------------------------------

procedure TFileListSorting.AddSorting(iField : Integer; SortDirection : TSortDirection);
var
  i : Integer;
  pSortingColumn : PFileListSortingColumn;
begin
  i := Count - 1;
  while i >= 0 do
  begin
    pSortingColumn := PFileListSortingColumn(Self[i]);
    if pSortingColumn^.iField = iField then
    begin
      pSortingColumn^.SortDirection := ReverseSortDirection(pSortingColumn^.SortDirection);
      Exit;
    end;
    dec(i);
  end;

  new(pSortingColumn);
  pSortingColumn^.iField := iField;
  pSortingColumn^.SortDirection := SortDirection;
  Add(pSortingColumn);
end;

Destructor TFileListSorting.Destroy;
begin
  Clear;
  inherited;
end;

function TFileListSorting.Clone: TFileListSorting;
var
  i: Integer;
  pSortingColumn : PFileListSortingColumn;
begin
  Result := TFileListSorting.Create;

  for i := 0 to Count - 1 do
  begin
    pSortingColumn := PFileListSortingColumn(Self[i]);
    Result.AddSorting(pSortingColumn^.iField, pSortingColumn^.SortDirection);
  end;
end;

procedure TFileListSorting.Clear;
var
  i : Integer;
  pSortingColumn : PFileListSortingColumn;
begin
  i := Count - 1;
  while i >= 0 do
  begin
    pSortingColumn := PFileListSortingColumn(Self[i]);
    dispose(pSortingColumn);
    dec(i);
  end;

  Inherited Clear;
end;

function TFileListSorting.GetSortingDirection(iField : Integer) : TSortDirection;
var
  i : Integer;
  pSortingColumn : PFileListSortingColumn;
begin
  Result := sdNone;

  i := Count - 1;
  while i >= 0 do
  begin
    pSortingColumn := PFileListSortingColumn(Self[i]);
    if pSortingColumn^.iField = iField then
    begin
      Result := pSortingColumn^.SortDirection;
      break;
    end;
    dec(i);
  end;
end;

{ TColumnsFileListBuilder }

constructor TColumnsFileListBuilder.Create;
begin
  FTmpFileSourceFiles := nil;
  FTmpDisplayFiles := nil;
  FAborted := False;
  FState := rfsNone;
end;

procedure TColumnsFileListBuilder.Abort;
begin
  FAborted := True;
end;

procedure TColumnsFileListBuilder.InitializeBeforeWork(AColumnsView: TColumnsFileView);
begin
  FAborted := False;
  FState := rfsLoadingFiles;
  FIconsToLoad := 0;

  // Copy these parameters while it's still safe to access them from the main thread.
  FColumnsView      := AColumnsView;
  FFileSource       := AColumnsView.FileSource;
  FFileSourcesCount := AColumnsView.FileSourcesCount;
  FFileFilter       := AColumnsView.FileFilter;
  FCurrentPath      := AColumnsView.CurrentPath;
  FThread           := AColumnsView.FListFilesThread;
  FSortings         := AColumnsView.PrepareSortings;
end;

procedure TColumnsFileListBuilder.MakeFileSourceFileList(Params: Pointer);
var
  AFile: TFile;
  ListOperation: TFileSourceListOperation;
begin
  try
    if FAborted then
      Exit;

    if fsoList in FFileSource.GetOperationsTypes then
    begin
      ListOperation := FFileSource.CreateListOperation(FCurrentPath) as TFileSourceListOperation;
      if Assigned(ListOperation) then
        try
          ListOperation.AssignThread(FThread);
          ListOperation.Execute;
          FTmpFileSourceFiles := ListOperation.ReleaseFiles;
        finally
          FreeAndNil(ListOperation);
        end;
    end;

    {$IFDEF timeFileView}
    DebugLn('Loaded files   : ' + IntToStr(DateTimeToTimeStamp(Now - startTime).Time));
    {$ENDIF}

    if FAborted then
      Exit;

    if Assigned(FTmpFileSourceFiles) then
    begin
      TColumnsFileView.Sort(FTmpFileSourceFiles, FSortings);

      // Add '..' to go to higher level file source, if there is more than one.
      if (FFileSourcesCount > 1) and (FFileSource.IsPathAtRoot(FCurrentPath)) then
      begin
        AFile := FTmpFileSourceFiles.CreateFileObject(FCurrentPath);
        AFile.Name := '..';
        if fpAttributes in AFile.SupportedProperties then
          (AFile.Properties[fpAttributes] as TFileAttributesProperty).Value := faFolder;
        FTmpFileSourceFiles.Insert(AFile, 0);
      end;
    end;

    if FAborted then
      Exit;

    {$IFDEF timeFileView}
    DebugLn('Sorted files   : ' + IntToStr(DateTimeToTimeStamp(Now - startTime).Time));
    {$ENDIF}

    // Make display file list from file source file list.
    FTmpDisplayFiles := TColumnsViewFiles.Create;
    MakeDisplayFileList(FFileSource, FTmpFileSourceFiles, FTmpDisplayFiles, FFileFilter);

    {$IFDEF timeFileView}
    DebugLn('Made disp. list: ' + IntToStr(DateTimeToTimeStamp(Now - startTime).Time));
    {$ENDIF}

    AfterMakeDisplayFileList;

  finally
    {$IFDEF timeFileView}
    DebugLn('Finished       : ' + IntToStr(DateTimeToTimeStamp(Now - startTime).Time));
    {$ENDIF}

    FState := rfsNone;

    FreeThenNil(FTmpDisplayFiles);
    FreeThenNil(FTmpFileSourceFiles);
  end;
end;

class procedure TColumnsFileListBuilder.MakeDisplayFileList(
                  aFileSource: IFileSource;
                  aFileSourceFiles: TFiles;
                  aFiles: TColumnsViewFiles;
                  aFileFilter: String);
var
  AFile: TColumnsViewFile;
  i: Integer;
  invalidFilter: Boolean = False;
  localFilter: String;
  bLoadIcons: Boolean;
begin
  aFiles.Clear;

  if Assigned(aFileSourceFiles) then
  begin
    // Prepare filter string based on options.
    if aFileFilter <> EmptyStr then
    begin
      localFilter := aFileFilter;
      if not gQuickSearchMatchBeginning then
        localFilter := '*' + localFilter;
      if not gQuickSearchMatchEnding then
        localFilter := localFilter + '*';
    end;

    bLoadIcons := (not (gListFilesInThread and (GetCurrentThreadId <> MainThreadID))) or
                  ((not gLoadIconsSeparately) and (gShowIcons <> sim_none));

    for i := 0 to aFileSourceFiles.Count - 1 do
    begin
      if gShowSystemFiles = False then
      begin
        if aFileSourceFiles[i].IsSysFile then Continue;
      end;

      // Ignore list
      if gIgnoreListFileEnabled then
      begin
        if MatchesMaskListEx(aFileSourceFiles[i], glsIgnoreList) then Continue;
      end;

      // Filter files.
      if (aFileFilter <> EmptyStr) and (invalidFilter = False) then
      begin
        try
          if (aFileSourceFiles[i].Name <> '..') and
             (aFileSourceFiles[i].Name <> '.') and

             // Don't filter directories.
             not (aFileSourceFiles[i].IsDirectory or
                  aFileSourceFiles[i].IsLinkToDirectory) and

             not MatchesMask(UTF8LowerCase(aFileSourceFiles[i].Name),
                             UTF8LowerCase(localFilter))
          then
            Continue;

        except
          on EConvertError do
            invalidFilter := True;
        end;
      end;

      AFile := TColumnsViewFile.Create(aFileSourceFiles[i]);
      if bLoadIcons then
        AFile.IconID := PixMapManager.GetIconByFile(AFile.TheFile,
                                                    fspDirectAccess in aFileSource.Properties);
      aFiles.Add(AFile);
    end;
  end;
end;

procedure TColumnsFileListBuilder.AfterMakeDisplayFileList;
begin
  if gListFilesInThread and (GetCurrentThreadId <> MainThreadID) then
  begin
    if FAborted then
      Exit;

    if gLoadIconsSeparately and (gShowIcons <> sim_none) then
      FIconsToLoad := FTmpDisplayFiles.Count;

    // Loading file list is complete. Update grid with the new file list.
    TThread.Synchronize(FThread, @SetColumnsFilelist);

    {$IFDEF timeFileView}
    DebugLn('Grid updated   : ' + IntToStr(DateTimeToTimeStamp(Now - startTime).Time));
    {$ENDIF}

    // Now will load icons if not yet loaded.
    if FIconsToLoad > 0 then
    begin
      if FAborted then
        Exit;

      LoadFilesIcons;

      {$IFDEF timeFileView}
      DebugLn('Loaded icons   : ' + IntToStr(DateTimeToTimeStamp(Now - startTime).Time));
      {$ENDIF}

      if FAborted then
        Exit;

      // Loading file list is complete.
      TThread.Synchronize(FThread, @ClearBuilder);
    end;
  end
  else
  begin
    SetColumnsFilelist;
  end;
end;

procedure TColumnsFileListBuilder.SetColumnsFilelist;
begin
  if not FAborted then
  begin
    // The following swap must occur in the main thread because
    // access to FFileSourceFiles and FFiles is not protected.
    with FColumnsView do
    begin
      if Assigned(FFiles) then
        FFiles.Free;
      FFiles := FTmpDisplayFiles;
      FTmpDisplayFiles := nil;

      if Assigned(FFileSourceFiles) then
        FFileSourceFiles.Free;
      FFileSourceFiles := FTmpFileSourceFiles;
      FTmpFileSourceFiles := nil;
    end;

    // Now that files are assigned change state before displaying file list.
    if FIconsToLoad > 0 then
      FState := rfsLoadingIcons
    else
      // Loading file list is complete.
      ClearBuilder;

    // Display new file list.
    FColumnsView.AfterMakeFileList;
  end;
end;

procedure TColumnsFileListBuilder.LoadFilesIcons;
var
  i: Integer;
  bRedrawIcon: Boolean;
begin
  // This is the only place where we must access ColumnsView.FFiles from the worker thread.
  // We can only update basic datatypes here (variables which are not allocated on heap),
  // so that concurrent access from GUI thread is safe. Otherwise access to FFiles
  // would need to be fully synchronized.
  // It has to be done under FFileListBuilderLock lock (the same as in
  // StopBackgroundWork) and only after checking FAborted under the same lock.
  // That way it is synchronized with StopBackgroundWork which blocks
  // until any access to FFiles below is complete.

  for i := 0 to FIconsToLoad - 1 do
  begin
    FColumnsView.FFileListBuilderLock.Acquire;
    try
      if FAborted then
        Exit;

      with FColumnsView.FFiles[i] do
        IconID := PixMapManager.GetIconByFile(TheFile, fspDirectAccess in FFileSource.Properties);

      // This access to dgPanel should be safe from worker thread.
      with FColumnsView.dgPanel do
        bRedrawIcon := IscellVisible(0, i + FixedRows);

    finally
      FColumnsView.FFileListBuilderLock.Release;
    end;

    // Redraw column with the icon. This must be done from the GUI thread.
    if bRedrawIcon then
    begin
      FFileIndexToUpdate := i;
      TThread.Synchronize(FThread, @UpdateIcon);
    end;
  end;
end;

procedure TColumnsFileListBuilder.UpdateIcon;
begin
  if not FAborted then
  begin
    with FColumnsView.dgPanel do
    begin
      if IscellVisible(0, FFileIndexToUpdate + FixedRows) then
        InvalidateCell(0, FFileIndexToUpdate + FixedRows);
    end;
  end;
end;

procedure TColumnsFileListBuilder.ClearBuilder;
begin
  FColumnsView.FCurrentFileListBuilder := nil;
end;

function TColumnsFileListBuilder.IsWorking: Boolean;
begin
  Result := (State <> rfsNone);
end;

end.

