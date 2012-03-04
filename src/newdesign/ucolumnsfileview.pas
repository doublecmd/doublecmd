unit uColumnsFileView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, StdCtrls, ExtCtrls, Grids,
  LMessages, LCLIntf, LCLType, Menus, LCLVersion,
  uDragDropEx,
  uFile,
  uFileProperty,
  uFileView,
  uFileSource,
  uDisplayFile,
  uColumns,
  uFileSorting,
  uXmlConfig,
  uClassesEx,
  uTypes,
  uFileViewWorker,
  fQuickSearch,
  uFileViewHeader
  ;

type

  { Columns sorting }

  PColumnsSorting = ^TColumnsSorting;
  TColumnsSorting = record
    Column : Integer;
    SortDirection : TSortDirection;
  end;

  PFileListSorting = ^TColumnsSortings;
  TColumnsSortings = class(TList)
  public
    Destructor Destroy; override;
    function Clone: TColumnsSortings;
    procedure AddSorting(iColumn : Integer; SortDirection : TSortDirection);
    procedure Clear; override;
    function GetSortingDirection(iColumn : Integer) : TSortDirection;
  end;

  TColumnsFileView = class;

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
    SelectionStartIndex: Cardinal;
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

    procedure UpdateView;

    function MouseOnGrid(X, Y: LongInt): Boolean;

    // Returns height of all the header rows.
    function GetHeaderHeight: Integer;

    // Adapted from TCustomGrid.GetVisibleGrid only for visible rows.
    function GetVisibleRows: TRange;

    function IsRowVisible(aRow: Integer): Boolean;
  end;

  { TColumnsFileView }

  TColumnsFileView = class(TFileView)

  private
    FColumnsSorting: TColumnsSortings;
    FFileNameColumn: Integer;
    FExtensionColumn: Integer;
    FLastActiveRow: Integer;    //<en Last active row
    FUpdatingGrid: Boolean;
    FLastSelectionStartRow: Integer;
    FLastSelectionState: Boolean;

    pnlFooter: TPanel;
    lblInfo: TLabel;
    lblFilter: TLabel;
    pnlHeader: TFileViewHeader;
    pmColumnsMenu: TPopupMenu;
    pmOperationsCancel: TPopupMenu;
    quickSearch: TfrmQuickSearch;
    edtRename: TEdit;
    dgPanel: TDrawGridEx;
    tmContextMenu: TTimer;
    tmClearGrid: TTimer;

    function GetGridHorzLine: Boolean;
    function GetGridVertLine: Boolean;
    procedure SetGridHorzLine(const AValue: Boolean);
    procedure SetGridVertLine(const AValue: Boolean);
    function GetColumnsClass: TPanelColumnsClass;
    function GetVisibleFilesIndexes: TRange;

    {en
       Sets last active file by row nr in the grid.
    }
    procedure SetLastActiveFile(RowNr: Integer);
    {en
       Sets a file as active if the file currently exists in the grid.
       @returns(@true if the file was found and selected.)
    }
    function SetActiveFileNow(aFilePath: String): Boolean;
    function StartDragEx(MouseButton: TMouseButton; ScreenStartPoint: TPoint): Boolean;

    procedure UpdateInfoPanel;
    procedure SetRowCount(Count: Integer);
    procedure SetColumns;
    procedure RedrawGrid;
    {en
       Redraw row containing DisplayFile if it is visible.
    }
    procedure RedrawFile(DisplayFile: TDisplayFile);

    procedure MakeVisible(iRow: Integer);
    procedure MakeSelectedVisible;
    procedure SelectFile(AFile: TDisplayFile); override;
    procedure SelectRange(iRow: PtrInt);
    procedure InvertAll; override;
    procedure MarkAll;
    procedure UnMarkAll;
    function MarkMinus: Boolean; override;
    function MarkPlus: Boolean; override;
    function MarkShiftPlus: Boolean; override;
    function MarkShiftMinus: Boolean; override;
    procedure RestoreSelection; override;

    {en
       Updates GUI after the display file list has changed.
    }
    procedure DisplayFileListHasChanged;
    {en
       Format and cache all columns strings.
    }
    procedure MakeColumnsStrings(AFile: TDisplayFile);
    procedure MakeColumnsStrings(AFile: TDisplayFile; ColumnsClass: TPanelColumnsClass);
    procedure EnsureDisplayProperties;
    procedure UpdateFile(const UpdatedFile: TDisplayFile;
                         const UserData: Pointer);
    procedure CalcSpaceUpdateFile(const UpdatedFile: TDisplayFile;
                                  const UserData: Pointer);

    {en
       Prepares sortings for later use in Sort function.
       This function must be called from main thread.
    }
    function PrepareSortings: TFileSortings;
    {en
       Translates file sorting by functions to sorting by columns.
    }
    procedure SetColumnsSorting(const ASortings: TFileSortings);

    {en
       Checks which file properties are needed for displaying.
    }
    function GetFilePropertiesNeeded: TFilePropertiesTypes;

    procedure ShowRenameFileEdit(const sFileName:String);
    {en
       Search and position in a file that matches name taking into account
       passed options
    }
    procedure SearchFile(SearchTerm: UTF8String; SearchOptions: TQuickSearchOptions; SearchDirection: TQuickSearchDirection = qsdNone);

    procedure CalculateSpaceOfAllDirectories;
    procedure CalculateSpace(AFile: TDisplayFile);
    procedure CalculateSpace(var AFileList: TFVWorkerFileList);

    function DimColor(AColor: TColor): TColor;

    // -- Events --------------------------------------------------------------

    procedure edtRenameExit(Sender: TObject);

    procedure edtRenameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure quickSearchChangeSearch(Sender: TObject; ASearchText: UTF8String; const ASearchOptions: TQuickSearchOptions; ASearchDirection: TQuickSearchDirection);
    procedure quickSearchChangeFilter(Sender: TObject; AFilterText: UTF8String; const AFilterOptions: TQuickSearchOptions);
    procedure quickSearchExecute(Sender: TObject);
    procedure quickSearchHide(Sender: TObject);

    procedure dgPanelEnter(Sender: TObject);
    procedure dgPanelExit(Sender: TObject);
{$IF lcl_fullversion >= 093100}
    procedure dgPanelBeforeSelection(Sender: TObject; aCol, aRow: Integer);
{$ENDIF}
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
    procedure dgPanelTopLeftChanged(Sender: TObject);
    procedure dgPanelResize(Sender: TObject);
    procedure tmContextMenuTimer(Sender: TObject);
    procedure tmClearGridTimer(Sender: TObject);
    procedure lblFilterClick(Sender: TObject);
    procedure ColumnsMenuClick(Sender: TObject);
    procedure OperationsCancelClick(Sender: TObject);

    procedure UTF8KeyPressEvent(Sender: TObject; var UTF8Key: TUTF8Char);

  protected
    procedure CreateDefault(AOwner: TWinControl); override;

    procedure BeforeMakeFileList; override;
    procedure AfterMakeFileList; override;
    {en
       Changes drawing colors depending on if this panel is active.
    }
    procedure SetActive(bActive: Boolean); override;
    procedure SetSorting(const NewSortings: TFileSortings); override;

    procedure AfterChangePath; override;
    function GetActiveDisplayFile: TDisplayFile; override;

    procedure WorkerStarting(const Worker: TFileViewWorker); override;
    procedure WorkerFinished(const Worker: TFileViewWorker); override;

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
    procedure RemoveCurrentFileSource; override;

    procedure LoadConfiguration(Section: String; TabIndex: Integer); override;
    procedure SaveConfiguration(Section: String; TabIndex: Integer); override;
    procedure LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); override;
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); override;

    function Focused: Boolean; override;
    procedure SetFocus; override;

    procedure SetActiveFile(aFilePath: String); override;
    procedure UnselectAllFiles; override;

    procedure UpdateColumnsView;
    procedure UpdateView; override;

    procedure DoDragDropOperation(Operation: TDragDropOperation;
                                  var DropParams: TDropParams); override;

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
    procedure cm_CountDirContent(param: string='');
    procedure cm_RenameOnly(param: string='');
    procedure cm_ContextMenu(param: string='');
    procedure cm_EditPath(param: string='');
    procedure cm_GoToFirstFile(param: string='');
    procedure cm_GoToLastFile(param: string='');
  end;

implementation

uses
  LCLProc, uMasks, Clipbrd, uLng, uShowMsg, uGlobs, uPixmapManager, uDebug,
  uDCUtils, uOSUtils, math, fMain, fOptions,
  uInfoToolTip, dmCommonData,
  uFileSourceProperty,
  uFileSourceOperationTypes,
  uFileSystemFileSource,
  fColumnsSetConf,
  uKeyboard,
  uFileSourceUtil,
  uFileFunctions
{$IF DEFINED(LCLGTK)}
  , GtkProc  // for ReleaseMouseCapture
  , GTKGlobals  // for DblClickTime
{$ENDIF}
{$IF DEFINED(LCLGTK2)}
  , Gtk2Proc  // for ReleaseMouseCapture
  , GTK2Globals  // for DblClickTime
{$ENDIF}
  ;

const
  CANCEL_FILTER = 0;
  CANCEL_OPERATION = 1;

function TColumnsFileView.Focused: Boolean;
begin
  Result := Assigned(dgPanel) and dgPanel.Focused;
end;

procedure TColumnsFileView.SetFocus;
begin
  // CanFocus checks parent controls, but not parent form.
  if GetParentForm(Self).CanFocus and dgPanel.CanFocus then
    dgPanel.SetFocus;
end;

procedure TColumnsFileView.SetActive(bActive: Boolean);
begin
  inherited SetActive(bActive);

  pnlHeader.SetActive(bActive);

  dgPanel.Color := DimColor(gBackColor);
end;

procedure TColumnsFileView.SetSorting(const NewSortings: TFileSortings);
begin
  SetColumnsSorting(NewSortings);
  inherited SetSorting(PrepareSortings); // NewSortings
  TFileSorter.Sort(FAllDisplayFiles, Sorting);
  ReDisplayFileList;
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
  FColumnsSorting.Clear;
  ColumnsClass := GetColumnsClass;
  SortCount := gIni.ReadInteger(Section, sIndex + '_sortcount', 0);
  for i := 0 to SortCount - 1 do
  begin
    SortColumn := gIni.ReadInteger(Section, sIndex + '_sortcolumn' + IntToStr(i), -1);
    if (SortColumn >= 0) and (SortColumn < ColumnsClass.ColumnsCount) then
    begin
      SortDirection := TSortDirection(gIni.ReadInteger(Section, sIndex + '_sortdirection' + IntToStr(i), Integer(sdNone)));
      FColumnsSorting.AddSorting(SortColumn, SortDirection);
    end;
  end;
  inherited SetSorting(PrepareSortings);
end;

procedure TColumnsFileView.SaveConfiguration(Section: String; TabIndex: Integer);
var
  SortingColumn: PColumnsSorting;
  sIndex: String;
  i: Integer;
begin
  sIndex := IntToStr(TabIndex);

  gIni.WriteString(Section, sIndex + '_columnsset', ActiveColm);

  // Save sorting options.
  gIni.WriteInteger(Section, sIndex + '_sortcount', FColumnsSorting.Count);
  for i := 0 to FColumnsSorting.Count - 1 do
  begin
    SortingColumn := PColumnsSorting(FColumnsSorting.Items[i]);

    gIni.WriteInteger(Section, sIndex + '_sortcolumn' + IntToStr(i),
                      SortingColumn^.Column);
    gIni.WriteInteger(Section, sIndex + '_sortdirection' + IntToStr(i),
                      Integer(SortingColumn^.SortDirection));
  end;
end;

procedure TColumnsFileView.LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
var
  ColumnsClass: TPanelColumnsClass;
  SortColumn: Integer;
  SortDirection: TSortDirection;
  ColumnsViewNode: TXmlNode;
begin
  inherited LoadConfiguration(AConfig, ANode);

  // Try to read new view-specific node.
  ColumnsViewNode := AConfig.FindNode(ANode, 'ColumnsView');
  if Assigned(ColumnsViewNode) then
    ANode := ColumnsViewNode;

  ActiveColm := AConfig.GetValue(ANode, 'ColumnsSet', 'Default');

  // Load sorting options.
  FColumnsSorting.Clear;
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
          FColumnsSorting.AddSorting(SortColumn, SortDirection);
        end
        else
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
      end;
      ANode := ANode.NextSibling;
    end;
    inherited SetSorting(PrepareSortings);
  end;
end;

procedure TColumnsFileView.SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
var
  SortingColumn: PColumnsSorting;
  i: Integer;
  SubNode: TXmlNode;
begin
  inherited SaveConfiguration(AConfig, ANode);

  AConfig.SetAttr(ANode, 'Type', 'columns');
  ANode := AConfig.FindNode(ANode, 'ColumnsView', True);
  AConfig.ClearNode(ANode);

  AConfig.SetValue(ANode, 'ColumnsSet', ActiveColm);
  ANode := AConfig.FindNode(ANode, 'Sorting', True);

  // Save sorting options.
  for i := 0 to FColumnsSorting.Count - 1 do
  begin
    SortingColumn := PColumnsSorting(FColumnsSorting.Items[i]);
    SubNode := AConfig.AddNode(ANode, 'Sort');
    AConfig.AddValue(SubNode, 'Column', SortingColumn^.Column);
    AConfig.AddValue(SubNode, 'Direction', Integer(SortingColumn^.SortDirection));
  end;
end;

procedure TColumnsFileView.SelectFile(AFile: TDisplayFile);
begin
  inherited SelectFile(AFile);
  UpdateInfoPanel;
end;

procedure TColumnsFileView.InvertAll;
begin
  inherited InvertAll;

  UpdateInfoPanel;
  dgPanel.Invalidate;
end;

function TColumnsFileView.StartDragEx(MouseButton: TMouseButton; ScreenStartPoint: TPoint): Boolean;
var
  fileNamesList: TStringList;
  draggedFileItem: TDisplayFile;
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
            fileNamesList.Add(FFiles[i].FSFile.FullPath);
        end;

        // If there were no files selected add the dragged file.
        if fileNamesList.Count = 0 then
          fileNamesList.Add(draggedFileItem.FSFile.FullPath);

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
  AFile: TDisplayFile;
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
  AFile: TDisplayFile;
begin
  if (Y < dgPanel.GetHeaderHeight) then Exit; // if is header

  SetFocus;

  // history navigation for mice with extra buttons
  case Button of
    mbExtra1:
      begin
        GoToPrevHistory;
        Exit;
      end;
    mbExtra2:
      begin
        GoToNextHistory;
        Exit;
      end;
  end;

  if IsEmpty then Exit;

  if dgPanel.MouseOnGrid(X, Y) then
  begin
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
            dgPanel.SelectionStartIndex:=iRow;
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
              // if there is no selected files then select also previous file
              if not HasSelectedFiles then
              begin
                AFile := FFiles[dgPanel.Row - dgPanel.FixedRows]; // substract fixed rows (header)
                if Assigned(AFile) then
                  begin
                    MarkFile(AFile, True);
                    UpdateInfoPanel;
                    dgPanel.Invalidate;
                  end;
              end;
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
  end
  else // if mouse on empty space
    begin
      if (Button = mbRight) and (gMouseSelectionEnabled) and (gMouseSelectionButton = 1) then
        tmContextMenu.Enabled:= True; // start context menu timer
    end;

  { Dragging }

  if (not dgPanel.Dragging)   and  // we could be in dragging mode already (started by a different button)
     (dgPanel.MouseOnGrid(X, Y)) then // check if there is an item under the mouse cursor
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
  AFile: TDisplayFile;
  iCol, iRow: Integer;
  i, SelStartIndex, SelEndIndex: Cardinal;
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
          if dgPanel.SelectionStartIndex < iRow then begin
            SelStartIndex := dgPanel.SelectionStartIndex;
            SelEndIndex := iRow;
          end else begin
            SelStartIndex := iRow;
            SelEndIndex := dgPanel.SelectionStartIndex;
          end;
          dgPanel.Row:= iRow;
          for i := SelStartIndex to SelEndIndex do begin
            AFile := FFiles[i - dgPanel.FixedRows]; // substract fixed rows (header)
            if Assigned(AFile) then
              begin
                MarkFile(AFile, FLastSelectionState);
                dgPanel.InvalidateRow(i);
              end;
          end;
          UpdateInfoPanel;
        end;
    end;
end;

{ Show context or columns menu on right click }
{ Is called manually from TDrawGridEx.MouseUp }
procedure TColumnsFileView.dgPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I : Integer;
  Point:TPoint;
  MI:TMenuItem;
  Background: Boolean;
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
      else if ((gMouseSelectionButton<>1) or not gMouseSelectionEnabled) then
        begin
          Background:= not (Sender as TDrawGridEx).MouseOnGrid(X, Y);
          frmMain.Commands.DoContextMenu(Self, Mouse.CursorPos.x, Mouse.CursorPos.y, Background);
        end
      else if (gMouseSelectionEnabled and (gMouseSelectionButton = 1)) then
        begin
          tmContextMenu.Enabled:= False; // stop context menu timer
        end;
    end
  { Open folder in new tab on middle click }
  else if (Button = mbMiddle) and (Y > dgPanel.GetHeaderHeight) then
    begin
      frmMain.Commands.cm_OpenDirInNewTab();
    end;
end;

procedure TColumnsFileView.dgPanelStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
end;

procedure TColumnsFileView.dgPanelDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  iRow, Dummy: Integer;
  AFile: TDisplayFile = nil;
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
     (AFile.FSFile.IsDirectory or AFile.FSFile.IsLinkToDirectory) and
     (dgPanel.MouseOnGrid(X, Y))
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
          if AFile.FSFile.Name = '..' then
            TargetDir := GetParentDir(TargetDir)
          else
            TargetDir := TargetDir + AFile.FSFile.Name + DirectorySeparator;

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
    SourceFiles := SourcePanel.CloneSelectedFiles;
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
  procedure ClearDropNode(aFileView: TFileView);
  begin
    if aFileView is TColumnsFileView then
      TColumnsFileView(aFileView).dgPanel.ChangeDropRowIndex(-1);
  end;
begin
  // If cancelled by the user, DragManager does not send drag-leave event
  // to the target, so we must clear the DropRow in both panels.

  ClearDropNode(frmMain.FrameLeft);
  ClearDropNode(frmMain.FrameRight);

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
    SortingDirection := FColumnsSorting.GetSortingDirection(Index);
    if SortingDirection = sdNone then
      SortingDirection := sdAscending
    else
      SortingDirection := ReverseSortDirection(SortingDirection);
    FColumnsSorting.Clear;
  end;

  FColumnsSorting.AddSorting(Index, SortingDirection);
  inherited SetSorting(PrepareSortings);
  TFileSorter.Sort(FAllDisplayFiles, Sorting);
  ReDisplayFileList;
end;

procedure TColumnsFileView.dgPanelMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  I: Integer;
begin
  Handled:= True;
  case gScrollMode of
  smLineByLine:
    for I:= 1 to gWheelScrollLines do
    dgPanel.Perform(LM_VSCROLL, SB_LINEUP, 0);
  smPageByPage:
    dgPanel.Perform(LM_VSCROLL, SB_PAGEUP, 0);
  else
    Handled:= False;
  end;
end;

procedure TColumnsFileView.dgPanelMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  I: Integer;
begin
  Handled:= True;
  case gScrollMode of
  smLineByLine:
    for I:= 1 to gWheelScrollLines do
    dgPanel.Perform(LM_VSCROLL, SB_LINEDOWN, 0);
  smPageByPage:
    dgPanel.Perform(LM_VSCROLL, SB_PAGEDOWN, 0);
  else
    Handled:= False;
  end;
end;

procedure TColumnsFileView.dgPanelSelection(Sender: TObject; aCol, aRow: Integer);
var
  aFile: TFile = nil;
begin
{$IF lcl_fullversion >= 093100}
  dgPanel.Options := dgPanel.Options - [goDontScrollPartCell];
{$ENDIF}

  if (FLastActiveRow <> aRow) and (not FUpdatingGrid) then
    begin
      SetLastActiveFile(aRow);
      FLastActiveRow:= aRow;

      if Assigned(OnChangeActiveFile) then
      begin
        aFile := CloneActiveFile;
        try
          OnChangeActiveFile(Self, aFile);
        finally
          FreeAndNil(aFile);
        end;
      end;
    end;
end;

procedure TColumnsFileView.dgPanelShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  AFile: TDisplayFile;
  sHint: UTF8String;
begin
  if HintInfo^.HintStr = EmptyStr then Exit; // don't show

  with dgPanel do
  begin
    if not InRange(HintRowIndex - FixedRows, 0, FFiles.Count - 1) then
      Exit;
    AFile := FFiles[HintRowIndex - FixedRows];
  end;

  if not AFile.FSFile.IsDirectory then
    begin
      sHint:= GetFileInfoToolTip(FileSource, AFile.FSFile);
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

procedure TColumnsFileView.dgPanelTopLeftChanged(Sender: TObject);
begin
  EnsureDisplayProperties;
end;

procedure TColumnsFileView.dgPanelResize(Sender: TObject);
begin
  EnsureDisplayProperties;
end;

procedure TColumnsFileView.tmContextMenuTimer(Sender: TObject);
var
  AFile: TDisplayFile;
  iRow, iCol: Integer;
  MousePoint: TPoint;
  Background: Boolean;
begin
  dgPanel.FMouseDown:= False;
  tmContextMenu.Enabled:= False; // stop context menu timer
  // show context menu
  MousePoint:= dgPanel.ScreenToClient(Mouse.CursorPos);
  Background:= not dgPanel.MouseOnGrid(MousePoint.x, MousePoint.y);
  frmMain.Commands.DoContextMenu(Self, Mouse.CursorPos.x, Mouse.CursorPos.y, Background);
  if not Background then
  begin
    // get current row
    dgPanel.MouseToCell(MousePoint.x, MousePoint.y, iCol, iRow);
    if iRow < dgPanel.FixedRows then Exit;
    AFile := FFiles[iRow - dgPanel.FixedRows]; // get current file
    MarkFile(AFile, False); // unselect file
    dgPanel.InvalidateRow(iRow); // invalidate current row
  end;
end;

procedure TColumnsFileView.tmClearGridTimer(Sender: TObject);
begin
  tmClearGrid.Enabled := False;

  if not Assigned(FFiles) or (FFiles.Count = 0) then
  begin
    SetRowCount(0);
    RedrawGrid;
  end;
end;

procedure TColumnsFileView.AfterChangePath;
begin
  inherited AfterChangePath;

  FUpdatingGrid := True;
  dgPanel.Row := 0;
  FUpdatingGrid := False;

  MakeFileSourceFileList;
  pnlHeader.UpdatePathLabel;
end;

procedure TColumnsFileView.ShowRenameFileEdit(const sFileName:String);
var
  ALeft, ATop, AWidth, AHeight: Integer;
begin
  if FFileNameColumn <> -1 then
  begin
    edtRename.Font.Name  := GetColumnsClass.GetColumnFontName(FFileNameColumn);
    edtRename.Font.Size  := GetColumnsClass.GetColumnFontSize(FFileNameColumn);
    edtRename.Font.Style := GetColumnsClass.GetColumnFontStyle(FFileNameColumn);

    ATop := dgPanel.CellRect(FFileNameColumn, dgPanel.Row).Top - 2;
    ALeft := dgPanel.CellRect(FFileNameColumn, dgPanel.Row).Left;
    if gShowIcons <> sim_none then
      Inc(ALeft, gIconsSize + 2);
    AWidth := dgPanel.ColWidths[FFileNameColumn] - ALeft;
    if Succ(FFileNameColumn) = FExtensionColumn then
      Inc(AWidth, dgPanel.ColWidths[FExtensionColumn]);
    AHeight := dgPanel.RowHeights[dgPanel.Row] + 4;

    edtRename.SetBounds(ALeft, ATop, AWidth, AHeight);

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
end;

function TColumnsFileView.PrepareSortings: TFileSortings;
var
  ColumnsClass: TPanelColumnsClass;
  i: Integer;
  pSortingColumn : PColumnsSorting;
  Column: TPanelColumn;
begin
  Result := nil;

  ColumnsClass := GetColumnsClass;
  if ColumnsClass.ColumnsCount = 0 then
    Exit;

  for i := 0 to FColumnsSorting.Count - 1 do
  begin
    pSortingColumn := PColumnsSorting(FColumnsSorting[i]);

    if (pSortingColumn^.Column >= 0) and
       (pSortingColumn^.Column < ColumnsClass.ColumnsCount) then
    begin
      Column := ColumnsClass.GetColumnItem(pSortingColumn^.Column);
      AddSorting(Result, Column.GetColumnFunctions, pSortingColumn^.SortDirection);
    end;
  end;
end;

procedure TColumnsFileView.SetColumnsSorting(const ASortings: TFileSortings);

var
  Columns: TPanelColumnsClass;

  function AddColumnsSorting(ASortFunction: TFileFunction; ASortDirection: TSortDirection): Boolean;
  var
    k, l: Integer;
    ColumnFunctions: TFileFunctions;
  begin
    for k := 0 to Columns.Count - 1 do
    begin
      ColumnFunctions := Columns.GetColumnItem(k).GetColumnFunctions;
      for l := 0 to Length(ColumnFunctions) - 1 do
        if ColumnFunctions[l] = ASortFunction then
        begin
          FColumnsSorting.AddSorting(k, ASortDirection);
          Exit(True);
        end;
    end;
    Result := False;
  end;

var
  i, j: Integer;
begin
  FColumnsSorting.Clear;
  Columns := GetColumnsClass;
  for i := 0 to Length(ASortings) - 1 do
  begin
    for j := 0 to Length(ASortings[i].SortFunctions) - 1 do
    begin
      // Search for the column containing the sort function and add sorting
      // by that column. If function is Name and it is not found try searching
      // for NameNoExtension + Extension.
      if (not AddColumnsSorting(ASortings[i].SortFunctions[j], ASortings[i].SortDirection)) and
         (ASortings[i].SortFunctions[j] = fsfName) then
      begin
        if AddColumnsSorting(fsfNameNoExtension, ASortings[i].SortDirection) then
          AddColumnsSorting(fsfExtension, ASortings[i].SortDirection);
      end;
    end;
  end;
end;

function TColumnsFileView.GetFilePropertiesNeeded: TFilePropertiesTypes;
var
  i, j: Integer;
  ColumnsClass: TPanelColumnsClass;
  Column: TPanelColumn;
  FileFunctionsUsed: TFileFunctions;
begin
  // By default always use some properties.
  Result := [fpName,
             fpSize,              // For info panel (total size, selected size)
             fpAttributes,        // For distinguishing directories
             fpLink,              // For distinguishing directories (link to dir) and link icons
             fpModificationTime   // For selecting/coloring files (by SearchTemplate)
            ];

  ColumnsClass := GetColumnsClass;
  FFileNameColumn := -1;
  FExtensionColumn := -1;

  // Scan through all columns.
  for i := 0 to ColumnsClass.Count - 1 do
  begin
    Column := ColumnsClass.GetColumnItem(i);
    FileFunctionsUsed := Column.GetColumnFunctions;
    if Length(FileFunctionsUsed) > 0 then
    begin
      // Scan through all functions in the column.
      for j := Low(FileFunctionsUsed) to High(FileFunctionsUsed) do
      begin
        // Add file properties needed to display the function.
        Result := Result + TFileFunctionToProperty[FileFunctionsUsed[j]];
        if (FFileNameColumn = -1) and (FileFunctionsUsed[j] in [fsfName, fsfNameNoExtension]) then
          FFileNameColumn := i;
        if (FExtensionColumn = -1) and (FileFunctionsUsed[j] in [fsfExtension]) then
          FExtensionColumn := i;
      end;
    end;
  end;
end;

procedure TColumnsFileView.SetRowCount(Count: Integer);
begin
  FUpdatingGrid := True;
  dgPanel.RowCount := dgPanel.FixedRows + Count;
  FUpdatingGrid := False;
end;

procedure TColumnsFileView.SetColumns;
var
  x: Integer;
  ColumnsClass: TPanelColumnsClass;
begin
  ColumnsClass := GetColumnsClass;

  dgPanel.Columns.BeginUpdate;
  try
    dgPanel.Columns.Clear;
    for x:= 0 to ColumnsClass.ColumnsCount - 1 do
    begin
      with dgPanel.Columns.Add do
      begin
        // SizePriority = 0 means don't modify Width with AutoFill.
        // Last column is always modified if all columns have SizePriority = 0.
        if (x = 0) and (gAutoSizeColumn = 0) then
          SizePriority := 1
        else
          SizePriority := 0;
        Width:= ColumnsClass.GetColumnWidth(x);
        Title.Caption:= ColumnsClass.GetColumnTitle(x);
      end;
    end;
  finally
    dgPanel.Columns.EndUpdate;
  end;
end;

function TColumnsFileView.DimColor(AColor: TColor): TColor;
begin
  if (not Active) and (gInactivePanelBrightness < 100) then
    Result := ModColor(AColor, gInactivePanelBrightness)
  else
    Result := AColor;
end;

procedure TColumnsFileView.edtRenameExit(Sender: TObject);
begin
  edtRename.Visible := False;
  UnMarkAll;

  // dgPanelEnter don't called automatically (bug?)
  dgPanelEnter(dgPanel);
end;

procedure TColumnsFileView.SearchFile(SearchTerm: UTF8String; SearchOptions: TQuickSearchOptions; SearchDirection: TQuickSearchDirection);
var
  I, StartPos : Integer;
  Result : Boolean;
  sFileName,
  sSearchName,
  sSearchNameNoExt,
  sSearchExt : UTF8String;

  procedure CheckOutOfBounds(var RowIndex: Integer);
  begin
    if RowIndex < 0 then
      RowIndex := Max(FFiles.Count - 1, 0);

    if RowIndex >= FFiles.Count then
      RowIndex := 0;
  end;
begin
  if FFiles.Count = 0 then
    Exit;

  if SearchOptions.SearchCase = qscInsensitive then
    sSearchName := UTF8LowerCase(SearchTerm)
  else
    sSearchName := SearchTerm;

  if Pos('.', sSearchName) <> 0 then
  begin
    sSearchNameNoExt := ExtractOnlyFileName(sSearchName);
    sSearchExt := ExtractFileExt(sSearchName);
    if not (qsmBeginning in SearchOptions.Match) then
      sSearchNameNoExt := '*' + sSearchNameNoExt;
    if not (qsmEnding in SearchOptions.Match) then
      sSearchNameNoExt := sSearchNameNoExt + '*';
    sSearchName := sSearchNameNoExt + sSearchExt + '*';
  end
  else
  begin
    if not (qsmBeginning in SearchOptions.Match) then
      sSearchName := '*' + sSearchName;
    sSearchName := sSearchName + '*';
  end;

  StartPos := dgPanel.Row - dgPanel.FixedRows; // start search from current cursor position
  case SearchDirection of
    qsdFirst:
      StartPos := 0; // begin search from first file
    qsdLast:
      StartPos := FFiles.Count - 1; // begin search from last file
    qsdNext:
      StartPos := StartPos + 1; // begin search from next file
    qsdPrevious:
      StartPos := StartPos - 1; // begin search from previous file
  end;

  CheckOutOfBounds(StartPos);
  I := StartPos;

  try
    repeat
      Result := True;

      sFileName := FFiles[I].FSFile.Name;

      if SearchOptions.SearchCase = qscInsensitive then
        sFileName := UTF8LowerCase(sFileName);

      if (SearchOptions.Items = qsiFiles) and
         (FFiles[I].FSFile.IsDirectory or
          FFiles[I].FSFile.IsLinkToDirectory) then
        Result := False;

      if (SearchOptions.Items = qsiDirectories) and
         not FFiles[I].FSFile.IsDirectory and
         not FFiles[I].FSFile.IsLinkToDirectory then
        Result := False;

      if not MatchesMask(sFileName, sSearchName, SearchOptions.SearchCase = qscSensitive) then
        Result := False;

      if Result then
      begin
        dgPanel.Row := I + dgPanel.FixedRows;
        MakeVisible(dgPanel.Row);
        Exit;
      end;

      // check next file depending on search direction
      if SearchDirection in [qsdNone, qsdFirst, qsdNext] then
        Inc(I)
      else
        Dec(I);

      CheckOutOfBounds(I);
    until I = StartPos;

  except
    on EConvertError do; // bypass
    else
      raise;
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
  if GetCurrentWorkType = fvwtCreate then
  begin
    lblInfo.Caption := rsMsgLoadingFileList;
  end
  else if Assigned(FileSource) then
  begin
    FilesInDir := 0;
    FilesSelected := 0;
    SizeInDir := 0;
    SizeSelected := 0;

    SizeSupported := fpSize in FileSource.SupportedFileProperties;

    if Assigned(FFiles) then
    begin
      for i := 0 to FFiles.Count - 1 do
      begin
        with FFiles[i] do
        begin
          if FSFile.Name = '..' then Continue;

          inc(FilesInDir);
          if Selected then
            inc(FilesSelected);

          // Count size if Size property is supported.
          if SizeSupported then
          begin
            SizeProperty := FSFile.SizeProperty;

            if Selected then
              SizeSelected := SizeSelected + SizeProperty.Value;

            SizeInDir := SizeInDir + SizeProperty.Value;
          end;
        end;
      end;
    end;

    lblInfo.Caption := Format(rsMsgSelected,
                              [cnvFormatFileSize(SizeSelected),
                               cnvFormatFileSize(SizeInDir),
                               FilesSelected,
                               FilesInDir]);
  end
  else if not (csDestroying in ComponentState) then
    lblInfo.Caption := '';
end;

procedure TColumnsFileView.MarkAll;
begin
  MarkAllFiles(True);
  UpdateInfoPanel;
  dgPanel.Invalidate;
end;

function TColumnsFileView.MarkMinus: Boolean;
begin
  Result:= inherited MarkMinus;
  if Result then
  begin
    UpdateInfoPanel;
    dgPanel.Invalidate;
  end;
end;

function TColumnsFileView.MarkPlus: Boolean;
begin
  Result:= inherited MarkPlus;
  if Result then
  begin
    UpdateInfoPanel;
    dgPanel.Invalidate;
  end;
end;

function TColumnsFileView.MarkShiftPlus: Boolean;
begin
  Result:= inherited MarkShiftPlus;
  if Result then
  begin
    UpdateInfoPanel;
    dgPanel.Invalidate;
  end;
end;

function TColumnsFileView.MarkShiftMinus: Boolean;
begin
  Result:= inherited MarkShiftMinus;
  if Result then
  begin
    UpdateInfoPanel;
    dgPanel.Invalidate;
  end;
end;

procedure TColumnsFileView.RestoreSelection;
begin
  inherited RestoreSelection;
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.UnMarkAll;
begin
  MarkAllFiles(False);
  UpdateInfoPanel;
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.edtRenameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
var
  NewFileName: String;
  OldFileNameAbsolute: String;
  lenEdtText, lenEdtTextExt, i: Integer;
  seperatorSet: set of AnsiChar;
  aFile: TFile = nil;
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

        aFile := CloneActiveFile;
        try
          try
            if RenameFile(FileSource, aFile, NewFileName, True) = True then
            begin
              edtRename.Visible:=False;
              SetActiveFile(CurrentPath + NewFileName);
              SetFocus;
            end
            else
              msgError(Format(rsMsgErrRename, [ExtractFileName(OldFileNameAbsolute), NewFileName]));

          except
            on e: EInvalidFileProperty do
              msgError(Format(rsMsgErrRename + ':' + LineEnding + '%s (%s)', [ExtractFileName(OldFileNameAbsolute), NewFileName, rsMsgInvalidFileName, e.Message]));
          end;
        finally
          FreeAndNil(aFile);
        end;
      end;

    VK_F2, VK_F6:
        begin
          Key := 0;
          lenEdtText := UTF8Length(edtRename.Text);
          lenEdtTextExt := UTF8Length(ExtractFileExt(edtRename.Text));
          if (edtRename.SelLength = lenEdtText) then
          begin
            // Now all selected, change it to name-only.
            edtRename.SelStart:= 0;
            edtRename.SelLength:= lenEdtText - lenEdtTextExt;
          end
          else if (edtRename.SelStart = 0) and (edtRename.SelLength = lenEdtText - lenEdtTextExt) then
          begin
            // Now name-only selected, change it to ext-only.
            edtRename.SelStart:= edtRename.SelLength + 1;
            edtRename.SelLength:= lenEdtText - edtRename.SelStart;
          end
          else begin
            // Partial selection cycle.
            seperatorSet:= [' ', '-', '_', '.'];
            i:= edtRename.SelStart + edtRename.SelLength;
            while true do
            begin
              if (edtRename.Text[UTF8CharToByteIndex(PChar(edtRename.Text), length(edtRename.Text), i)] in seperatorSet)
                  and not(edtRename.Text[UTF8CharToByteIndex(PChar(edtRename.Text), length(edtRename.Text), i+1)] in seperatorSet) then
              begin
                edtRename.SelStart:= i;
                Break;
              end;
              inc(i);
              if i >= lenEdtText then
              begin
                edtRename.SelStart:= 0;
                Break;
              end;
            end;
            i:= edtRename.SelStart + 1;
            while true do
            begin
              if (i >= lenEdtText)
                  or (edtRename.Text[UTF8CharToByteIndex(PChar(edtRename.Text), length(edtRename.Text), i+1)] in seperatorSet) then
              begin
                edtRename.SelLength:= i - edtRename.SelStart;
                Break;
              end;
              inc(i);
            end;
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

procedure TColumnsFileView.quickSearchChangeSearch(Sender: TObject; ASearchText: UTF8String; const ASearchOptions: TQuickSearchOptions; ASearchDirection: TQuickSearchDirection);
begin
  SetActive(True);

  SearchFile(ASearchText, ASearchOptions, ASearchDirection);
end;

procedure TColumnsFileView.quickSearchChangeFilter(Sender: TObject; AFilterText: UTF8String; const AFilterOptions: TQuickSearchOptions);
begin
  SetActive(True);

  // position in file before filtering, otherwise position could be lost if
  // current file is filtered out causing jumps
  SearchFile(AFilterText, AFilterOptions);

  SetFileFilter(AFilterText, AFilterOptions);

  lblFilter.Caption := Format('(%s: %s)', [rsFilterStatus, AFilterText]);
  lblFilter.Visible := Filtered;
end;

procedure TColumnsFileView.quickSearchExecute(Sender: TObject);
begin
  SetActive(True);

  ChooseFile(GetActiveDisplayFile);
end;

procedure TColumnsFileView.quickSearchHide(Sender: TObject);
begin
  if Self.CanFocus then
    dgPanel.SetFocus;
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

procedure TColumnsFileView.SetLastActiveFile(RowNr: Integer);
begin
  if (RowNr >= dgPanel.FixedRows) and (RowNr < dgPanel.RowCount) and
     (RowNr - dgPanel.FixedRows < FFiles.Count) then
  begin
    LastActiveFile := FFiles[RowNr - dgPanel.FixedRows].FSFile.FullPath;
  end;
end;

function TColumnsFileView.SetActiveFileNow(aFilePath: String): Boolean;
var
  i: Integer;
begin
  if aFilePath <> '' then // find correct cursor position in Panel (drawgrid)
  begin
    if FileSource.GetPathType(aFilePath) = ptAbsolute then
    begin
      for i := 0 to FFiles.Count - 1 do
        if FFiles[i].FSFile.FullPath = aFilePath then
        begin
          FUpdatingGrid := True;
          dgPanel.Row := i + dgPanel.FixedRows;
          FUpdatingGrid := False;
          SetLastActiveFile(dgPanel.Row);
          Exit(True);
        end;
    end
    else
    begin
      for i := 0 to FFiles.Count - 1 do
        if FFiles[i].FSFile.Name = aFilePath then
        begin
          FUpdatingGrid := True;
          dgPanel.Row := i + dgPanel.FixedRows;
          FUpdatingGrid := False;
          SetLastActiveFile(dgPanel.Row);
          Exit(True);
        end;
    end;
  end;
  Result := False;
end;

procedure TColumnsFileView.SetActiveFile(aFilePath: String);
begin
  if GetCurrentWorkType = fvwtCreate then
  begin
    // File list is currently loading - remember requested file for later.
    RequestedActiveFile := aFilePath;
  end
  else
  begin
    // First try to select the file in the current file list.
    // If not found save it for later selection (possibly after reload).
    if SetActiveFileNow(aFilePath) then
      RequestedActiveFile := ''
    else
      RequestedActiveFile := aFilePath;
  end;
end;

procedure TColumnsFileView.UnselectAllFiles;
begin
  UnMarkAll;
end;

{$IF lcl_fullversion >= 093100}
procedure TColumnsFileView.dgPanelBeforeSelection(Sender: TObject; aCol, aRow: Integer);
begin
  if dgPanel.IsRowVisible(aRow) then
    dgPanel.Options := dgPanel.Options + [goDontScrollPartCell];
end;
{$ENDIF}

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
     (Point.Y <   dgPanel.GridHeight) then
  begin
    ChooseFile(GetActiveDisplayFile);
  end;

{$IFDEF LCLGTK2}
  dgPanel.fLastDoubleClickTime := Now;
{$ENDIF}
end;

procedure TColumnsFileView.dgPanelEnter(Sender: TObject);
begin
  SetActive(True);

  UpdateInfoPanel;

  if Assigned(OnActivate) then
    OnActivate(Self);
end;

procedure TColumnsFileView.RedrawGrid;
begin
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.RedrawFile(DisplayFile: TDisplayFile);
var
  VisibleFiles: TRange;
  i: Integer;
begin
  VisibleFiles := GetVisibleFilesIndexes;
  for i := VisibleFiles.First to VisibleFiles.Last do
  begin
    if FFiles[i] = DisplayFile then
    begin
      dgPanel.InvalidateRow(i + dgPanel.FixedRows);
      Break;
    end;
  end;
end;

procedure TColumnsFileView.UpdateColumnsView;
var
  ColumnsClass: TPanelColumnsClass;
  OldFilePropertiesNeeded: TFilePropertiesTypes;
begin
  // If the ActiveColm set doesn't exist this will retrieve either
  // the first set or the default set.
  ColumnsClass := GetColumnsClass;
  // Set name in case a different set was loaded.
  ActiveColm := ColumnsClass.Name;

  SetColumns;

  dgPanel.FocusRectVisible := ColumnsClass.GetCursorBorder and not gUseFrameCursor;
  dgPanel.FocusColor := ColumnsClass.GetCursorBorderColor;

  OldFilePropertiesNeeded := FilePropertiesNeeded;
  FilePropertiesNeeded := GetFilePropertiesNeeded;
  if FilePropertiesNeeded >= OldFilePropertiesNeeded then
  begin
    EnsureDisplayProperties;
  end;
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
var
  ScreenPoint: TPoint;
  aFile: TDisplayFile;
  mi: TMenuItem;
begin
  // check if ShiftState is equal to quick search / filter modes
  if quickSearch.CheckSearchOrFilter(Key) then
    Exit;

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
            SelectFile(GetActiveDisplayFile);
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
        ChooseFile(GetActiveDisplayFile, True);
        Key := 0;
      end;

    VK_UP, VK_DOWN:
      begin
        if ssShift in Shift then
        begin
          if IsActiveItemValid then
          begin
            SelectFile(GetActiveDisplayFile);
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
      if Shift * KeyModifiersShortcut = [] then
      begin
        aFile := GetActiveDisplayFile;
        if Assigned(aFile) then
        begin
          if IsItemValid(aFile) then
          begin
            if (aFile.FSFile.IsDirectory or
               aFile.FSFile.IsLinkToDirectory) and
               not aFile.Selected then
            begin
              CalculateSpace(aFile);
            end;

            SelectFile(aFile);
          end;

          if gSpaceMovesDown then
            dgPanel.Row := dgPanel.Row + 1;

          dgPanel.Invalidate;
          MakeSelectedVisible;
          Key := 0;
        end;
      end;

    VK_BACK:
      begin
        ChangePathToParent(True);
        Key := 0;
      end;

    VK_RETURN, VK_SELECT:
      begin
        if (Shift * KeyModifiersShortcut = []) then
        begin
          // Only if there are items in the panel.
          if not IsEmpty then
          begin
            ChooseFile(GetActiveDisplayFile);
            Key := 0;
          end;
        end
        // execute active file in terminal (Shift+Enter)
        else if (Shift * KeyModifiersShortcut = [ssShift]) then
        begin
          if IsActiveItemValid then
          begin
            mbSetCurrentDir(CurrentPath);
            ExecCmdFork(CurrentPath + GetActiveDisplayFile.FSFile.Name, True, gRunInTerm);
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

    VK_ESCAPE:
      begin
        if Filtered and (GetCurrentWorkType <> fvwtNone) then
        begin
          pmOperationsCancel.Items.Clear;

          mi := TMenuItem.Create(pmOperationsCancel);
          mi.Tag := CANCEL_FILTER;
          mi.Caption := rsCancelFilter;
          mi.OnClick := @OperationsCancelClick;
          pmOperationsCancel.Items.Add(mi);

          mi := TMenuItem.Create(pmOperationsCancel);
          mi.Tag := CANCEL_OPERATION;
          mi.Caption := rsCancelOperation;
          mi.OnClick := @OperationsCancelClick;
          pmOperationsCancel.Items.Add(mi);

          pmOperationsCancel.PopUp;

          Key := 0;
        end
        else if Filtered then
        begin
          quickSearch.Finalize;
          Key := 0;
        end
        else if GetCurrentWorkType <> fvwtNone then
        begin
          StopWorkers;
          Key := 0;
        end;
      end;
  end;
end;

procedure TColumnsFileView.lblFilterClick(Sender: TObject);
begin
  quickSearch.Initialize(qsFilter);
end;

procedure TColumnsFileView.ColumnsMenuClick(Sender: TObject);
var
  frmColumnsSetConf: TfColumnsSetConf;
  frmOptions: TfrmOptions = nil;
  Index: Integer;
begin
  Case (Sender as TMenuItem).Tag of
    1000: //This
          begin
            frmColumnsSetConf := TfColumnsSetConf.Create(nil);
            try
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
                UpdateView;
              end;
            finally
              FreeAndNil(frmColumnsSetConf);
            end;
          end;
    1001: //All columns
          begin
            frmOptions := TfrmOptions.Create(Application, 'TfrmOptionsCustomColumns');
            try
              if frmOptions.ShowModal = mrOK then
                UpdateView;
            finally
              frmOptions.Free;
            end;
          end;
  else
    begin
      ActiveColm:=ColSet.Items[(Sender as TMenuItem).Tag];
      UpdateView;
    end;
  end;
end;

procedure TColumnsFileView.OperationsCancelClick(Sender: TObject);
begin
  if not Assigned(Sender) or not (Sender is TMenuItem) then
    Exit;

  case (Sender as TMenuItem).Tag of
    CANCEL_FILTER:
      quickSearch.Finalize;
    CANCEL_OPERATION:
      StopWorkers;
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

  FFiles := TDisplayFiles.Create;
  FColumnsSorting := TColumnsSortings.Create;
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

  FFiles := TDisplayFiles.Create;
  FColumnsSorting := TColumnsSortings.Create;

  LoadConfiguration(ASectionName, ATabIndex);
end;

constructor TColumnsFileView.Create(AOwner: TWinControl; AConfig: TXmlConfig; ANode: TXmlNode);
begin
  inherited Create(AOwner, AConfig, ANode);

  FFiles := TDisplayFiles.Create;
  FColumnsSorting := TColumnsSortings.Create;

  LoadConfiguration(AConfig, ANode);

  if FileSourcesCount > 0 then
  begin
    // Update view before making file source file list,
    // so that file list isn't unnecessarily displayed twice.
    UpdateView;
    MakeFileSourceFileList;
  end;
end;

procedure TColumnsFileView.CreateDefault(AOwner: TWinControl);
begin
  DCDebug('TColumnsFileView.Create components');

  dgPanel := nil;

  BorderStyle := bsNone; // Before Create or the window handle may be recreated
  inherited CreateDefault(AOwner);
  Align := alClient;

  ActiveColm := '';
  ActiveColmSlave := nil;
  isSlave := False;
  FColumnsSorting := nil;
  FLastSelectionStartRow := -1;
  FUpdatingGrid := False;
  FFileNameColumn := -1;
  FExtensionColumn := -1;

  // -- other components

  dgPanel:=TDrawGridEx.Create(Self, Self);

  HotMan.Register(dgPanel, 'Files Panel');

  pnlHeader:=TFileViewHeader.Create(Self, Self);

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

  lblFilter := TLabel.Create(pnlFooter);
  lblFilter.Parent := pnlFooter;
  lblFilter.Align := alRight;
  lblFilter.Caption := EmptyStr;
  lblFilter.Visible := False;

  edtRename:=TEdit.Create(dgPanel);
  edtRename.Parent:=dgPanel;
  edtRename.Visible:=False;
  edtRename.TabStop:=False;
  edtRename.AutoSize:=False;

  // now create search frame
  quickSearch := TfrmQuickSearch.Create(Self);
  quickSearch.Parent := Self;
  quickSearch.Visible := False;
  quickSearch.Align := alBottom;

  tmContextMenu:= TTimer.Create(Self);
  tmContextMenu.Enabled:= False;
  tmContextMenu.Interval:= 500;
  tmContextMenu.OnTimer:= @tmContextMenuTimer;

  tmClearGrid := TTimer.Create(Self);
  tmClearGrid.Enabled := False;
  tmClearGrid.Interval := 500;
  tmClearGrid.OnTimer := @tmClearGridTimer;

  {$IFDEF LCLCARBON}
  // Under Carbon AutoSize don't work without it
  pnlHeader.ClientHeight:= 0;
  pnlFooter.ClientHeight:= 0;
  {$ENDIF}

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
{$IF lcl_fullversion >= 093100}
  dgPanel.OnBeforeSelection:= @dgPanelBeforeSelection;
{$ENDIF}
  dgPanel.OnShowHint:= @dgPanelShowHint;
  dgPanel.OnTopLeftChanged:= @dgPanelTopLeftChanged;
  dgpanel.OnResize:= @dgPanelResize;

  quickSearch.OnChangeSearch := @quickSearchChangeSearch;
  quickSearch.OnChangeFilter := @quickSearchChangeFilter;
  quickSearch.OnExecute := @quickSearchExecute;
  quickSearch.OnHide := @quickSearchHide;

  edtRename.OnKeyDown := @edtRenameKeyDown;
  edtRename.OnExit := @edtRenameExit;

  lblFilter.OnClick := @lblFilterClick;

  pmColumnsMenu := TPopupMenu.Create(Self);
  pmColumnsMenu.Parent := Self;

  pmOperationsCancel := TPopupMenu.Create(Self);
  pmOperationsCancel.Parent := Self;
end;

destructor TColumnsFileView.Destroy;
begin
  if Assigned(HotMan) then
    HotMan.UnRegister(dgPanel);

  FreeThenNil(FColumnsSorting);
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
      FLastSelectionStartRow := Self.FLastSelectionStartRow;

      FColumnsSorting := Self.FColumnsSorting.Clone;

      ActiveColm := Self.ActiveColm;
      ActiveColmSlave := nil;    // set to nil because only used in preview?
      isSlave := Self.isSlave;
    end;
  end;
end;

procedure TColumnsFileView.AddFileSource(aFileSource: IFileSource; aPath: String);
begin
  inherited AddFileSource(aFileSource, aPath);

  FUpdatingGrid := True;
  dgPanel.Row := 0;
  FUpdatingGrid := False;

  pnlHeader.UpdateAddressLabel;
end;

procedure TColumnsFileView.RemoveCurrentFileSource;
var
  FocusedFile: String;
begin
  // Temporary. Do this by remembering the file name in a list?
  FocusedFile := ExtractFileName(FileSource.CurrentAddress);

  inherited;

  SetActiveFile(FocusedFile);

  pnlHeader.UpdateAddressLabel;
end;

procedure TColumnsFileView.BeforeMakeFileList;
begin
  inherited;
  if gListFilesInThread then
  begin
    // Display info that file list is being loaded.
    UpdateInfoPanel;

    // If we cleared grid here there would be flickering if list operation is quickly completed.
    // So, only clear the grid after the file list has been loading for some time.
    tmClearGrid.Enabled := True;
  end;
end;

procedure TColumnsFileView.AfterMakeFileList;
begin
  inherited;

  tmClearGrid.Enabled := False;
  DisplayFileListHasChanged;
  EnsureDisplayProperties; // After displaying.
end;

procedure TColumnsFileView.DisplayFileListHasChanged;
begin
  // Update grid row count.
  SetRowCount(FFiles.Count);
  RedrawGrid;

  if SetActiveFileNow(RequestedActiveFile) then
    RequestedActiveFile := ''
  else
    // Requested file was not found, restore position to last active file.
    SetActiveFileNow(LastActiveFile);

  UpdateInfoPanel;
end;

procedure TColumnsFileView.MakeColumnsStrings(AFile: TDisplayFile);
begin
  MakeColumnsStrings(AFile, GetColumnsClass);
end;

procedure TColumnsFileView.MakeColumnsStrings(AFile: TDisplayFile; ColumnsClass: TPanelColumnsClass);
var
  ACol: Integer;
begin
  AFile.DisplayStrings.Clear;
  for ACol := 0 to ColumnsClass.Count - 1 do
  begin
    AFile.DisplayStrings.Add(ColumnsClass.GetColumnItemResultString(
      ACol, AFile.FSFile, FileSource));
  end;
end;

procedure TColumnsFileView.EnsureDisplayProperties;
var
  VisibleFiles: TRange;
  i: Integer;
  AFileList: TFVWorkerFileList;
  Worker: TFileViewWorker;
  AFile: TDisplayFile;
begin
  if (csDestroying in ComponentState) or
     (not Assigned(FFiles)) or
     (GetCurrentWorkType = fvwtCreate) then
    Exit;

  VisibleFiles := GetVisibleFilesIndexes;

  if not gListFilesInThread then
  begin
    for i := VisibleFiles.First to VisibleFiles.Last do
    begin
      AFile := FFiles[i];
      if AFile.FSFile.Name <> '..' then
      begin
        if AFile.IconID = -1 then
          AFile.IconID := PixMapManager.GetIconByFile(AFile.FSFile, fspDirectAccess in FileSource.Properties, True);
        {$IF DEFINED(MSWINDOWS)}
        if gIconOverlays and (AFile.IconOverlayID < 0) then
        begin
          AFile.IconOverlayID := PixMapManager.GetIconOverlayByFile(AFile.FSFile,
                                                                    fspDirectAccess in FileSource.Properties);
        end;
        {$ENDIF}
        if FileSource.CanRetrieveProperties(AFile.FSFile, FilePropertiesNeeded) then
        begin
          FileSource.RetrieveProperties(AFile.FSFile, FilePropertiesNeeded);
          MakeColumnsStrings(AFile);
        end;
      end;
    end;
  end
  else
  begin
    AFileList := TFVWorkerFileList.Create;
    try
      for i := VisibleFiles.First to VisibleFiles.Last do
      begin
        AFile := FFiles[i];
        if (AFile.FSFile.Name <> '..') and
           (FileSource.CanRetrieveProperties(AFile.FSFile, FilePropertiesNeeded) or
           (AFile.IconID = -1) or (AFile.IconOverlayID = -1)) then
        begin
          AFileList.AddClone(AFile, AFile);
        end;
      end;

      if AFileList.Count > 0 then
      begin
        Worker := TFilePropertiesRetriever.Create(
          FileSource,
          WorkersThread,
          FilePropertiesNeeded,
          @UpdateFile,
          AFileList);

        AddWorker(Worker, False);
        WorkersThread.QueueFunction(@Worker.StartParam);
      end;

    finally
      if Assigned(AFileList) then
        FreeAndNil(AFileList);
    end;
  end;
end;

procedure TColumnsFileView.UpdateFile(const UpdatedFile: TDisplayFile;
                                      const UserData: Pointer);
var
  propType: TFilePropertyType;
  aFile: TFile;
  OrigDisplayFile: TDisplayFile;
begin
  OrigDisplayFile := TDisplayFile(UserData);

  if not IsReferenceValid(OrigDisplayFile) then
    Exit; // File does not exist anymore (reference is invalid).

  aFile := OrigDisplayFile.FSFile;

{$IF (fpc_version>2) or ((fpc_version=2) and (fpc_release>4))}
  // This is a bit faster.
  for propType in UpdatedFile.FSFile.AssignedProperties - aFile.AssignedProperties do
{$ELSE}
  for propType := Low(TFilePropertyType) to High(TFilePropertyType) do
    if (propType in UpdatedFile.FSFile.AssignedProperties) and
       (not (propType in aFile.AssignedProperties)) then
{$ENDIF}
    begin
      aFile.Properties[propType] := UpdatedFile.FSFile.ReleaseProperty(propType);
    end;

  if UpdatedFile.IconID <> -1 then
    OrigDisplayFile.IconID := UpdatedFile.IconID;

  if UpdatedFile.IconOverlayID <> -1 then
    OrigDisplayFile.IconOverlayID := UpdatedFile.IconOverlayID;

  MakeColumnsStrings(OrigDisplayFile);
  RedrawFile(OrigDisplayFile);
end;

procedure TColumnsFileView.CalcSpaceUpdateFile(const UpdatedFile: TDisplayFile;
                                               const UserData: Pointer);
var
  OrigDisplayFile: TDisplayFile;
begin
  OrigDisplayFile := TDisplayFile(UserData);

  if not IsReferenceValid(OrigDisplayFile) then
    Exit; // File does not exist anymore (reference is invalid).

  OrigDisplayFile.FSFile.Size := UpdatedFile.FSFile.Size;
  MakeColumnsStrings(OrigDisplayFile);
  RedrawFile(OrigDisplayFile);
end;

procedure TColumnsFileView.WorkerStarting(const Worker: TFileViewWorker);
begin
  inherited;
  dgPanel.Cursor := crHourGlass;
  UpdateInfoPanel;
end;

procedure TColumnsFileView.WorkerFinished(const Worker: TFileViewWorker);
begin
  inherited;
  if Worker is TCalculateSpaceWorker then
  begin
    TFileSorter.Sort(FAllDisplayFiles, Sorting);
    ReDisplayFileList;
  end;
  dgPanel.Cursor := crDefault;
  UpdateInfoPanel;
end;

procedure TColumnsFileView.UpdateView;
var
  bLoadingFilelist: Boolean;
  i: Integer;
begin
  inherited;

  bLoadingFilelist := GetCurrentWorkType = fvwtCreate;
  StopWorkers;

  pnlHeader.Visible := gCurDir;  // Current directory
  pnlFooter.Visible := gStatusBar;  // Status bar
  GridVertLine:= gGridVertLine;
  GridHorzLine:= gGridHorzLine;

  pnlHeader.UpdateAddressLabel;
  pnlHeader.UpdatePathLabel;
  dgPanel.UpdateView;
  UpdateColumnsView;

  if bLoadingFilelist then
    MakeFileSourceFileList
  else
  begin
    if Assigned(FAllDisplayFiles) then
    begin
      // Clear display strings in case columns have changed.
      for i := 0 to FAllDisplayFiles.Count - 1 do
        FAllDisplayFiles[i].DisplayStrings.Clear;

      // This condition is needed when cloning to recreate filtered files.
      if not Assigned(FFiles) then
        ReDisplayFileList;
    end;
  end;
end;

function TColumnsFileView.GetActiveDisplayFile: TDisplayFile;
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

function TColumnsFileView.GetVisibleFilesIndexes: TRange;
begin
  Result := dgPanel.GetVisibleRows;
  Dec(Result.First, dgPanel.FixedRows);
  Dec(Result.Last, dgPanel.FixedRows);

  if Result.First < 0 then
    Result.First := 0;
  if Result.Last >= FFiles.Count then
    Result.Last := FFiles.Count - 1;
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
  AFileList: TFVWorkerFileList;
  AFile: TDisplayFile;
begin
  AFileList := TFVWorkerFileList.Create;
  try
    for i := 0 to FFiles.Count - 1 do
    begin
      AFile := FFiles[i];
      if IsItemValid(AFile) and AFile.FSFile.IsDirectory then
      begin
        AFileList.AddClone(AFile, AFile);
      end;
    end;

    CalculateSpace(AFileList);

  finally
    if Assigned(AFileList) then
      FreeAndNil(AFileList);
  end;
end;

procedure TColumnsFileView.CalculateSpace(AFile: TDisplayFile);
var
  AFileList: TFVWorkerFileList;
begin
  if GetCurrentWorkType = fvwtCreate then
    Exit;

  AFileList := TFVWorkerFileList.Create;
  try
    if IsItemValid(AFile) and AFile.FSFile.IsDirectory then
    begin
      AFileList.AddClone(AFile, AFile);
    end;

    CalculateSpace(AFileList);

  finally
    if Assigned(AFileList) then
      FreeAndNil(AFileList);
  end;
end;

procedure TColumnsFileView.CalculateSpace(var AFileList: TFVWorkerFileList);
var
  Worker: TFileViewWorker;
begin
  if GetCurrentWorkType = fvwtCreate then
    Exit;

  if AFileList.Count > 0 then
  begin
    Worker := TCalculateSpaceWorker.Create(
      FileSource,
      WorkersThread,
      @CalcSpaceUpdateFile,
      AFileList);

    AddWorker(Worker);
    WorkersThread.QueueFunction(@Worker.StartParam);
  end
  else
    FreeAndNil(AFileList);
end;

procedure TColumnsFileView.UTF8KeyPressEvent(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  // check if ShiftState is equal to quick search / filter modes
  if quickSearch.CheckSearchOrFilter(UTF8Key) then
    Exit;
end;

procedure TColumnsFileView.DoDragDropOperation(Operation: TDragDropOperation;
                                               var DropParams: TDropParams);
var
  AFile: TDisplayFile;
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
           (dgPanel.MouseOnGrid(ClientDropPoint.X, ClientDropPoint.Y)) then
        begin
          AFile := FFiles[iRow - dgPanel.FixedRows];

          // If dropped into a directory modify destination path accordingly.
          if Assigned(AFile) and
             (AFile.FSFile.IsDirectory or AFile.FSFile.IsLinkToDirectory) then
          begin
            if AFile.FSFile.Name = '..' then
              // remove the last subdirectory in the path
              TargetPath := GetParentDir(TargetPath)
            else
              TargetPath := TargetPath + AFile.FSFile.Name + DirectorySeparator;
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
        FSavedSelection.SaveToFile(SaveDialog.FileName);
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
        FSavedSelection.LoadFromFile(OpenDialog.FileName);
        RestoreSelection;
      except
        on E: Exception do
          msgError(rsMsgErrEOpen + '-' + E.Message);
      end;
  end;
end;

procedure TColumnsFileView.cm_LoadSelectionFromClip(param: string);
begin
  FSavedSelection.Text:= Clipboard.AsText;
  RestoreSelection;
end;

{
  Parameters:
  "togglefilter"     - toggle between quick search and quick filter
  "matchbeginning"   - toggle match beginning option
  "matchending"      - toggle match ending option
  "casesensitivity"  - toggle case sensitive searching
  "filesdirectories" - toggle betwen files, directories and both
}
procedure TColumnsFileView.cm_QuickSearch(param: string='');
begin
  if param = EmptyStr then
    quickSearch.Initialize(qsSearch)
  else
    quickSearch.ToggleOption(param);
end;

{
  Parameters:
  "togglefilter"     - toggle between quick search and quick filter
  "matchbeginning"   - toggle match beginning option
  "matchending"      - toggle match ending option
  "casesensitivity"  - toggle case sensitive searching
  "filesdirectories" - toggle betwen files, directories and both
}
procedure TColumnsFileView.cm_QuickFilter(param: string='');
begin
  if param = EmptyStr then
    quickSearch.Initialize(qsFilter)
  else
    quickSearch.ToggleOption(param);
end;

procedure TColumnsFileView.cm_Open(param: string='');
begin
  ChooseFile(GetActiveDisplayFile);
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
      aFile:= CloneActiveFile;
      if Assigned(aFile) then
      try
        if aFile.IsNameValid then
          ShowRenameFileEdit(CurrentPath + aFile.Name)
        else
          pnlHeader.ShowPathEdit;
      finally
        FreeAndNil(aFile);
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
  frmMain.Commands.DoContextMenu(Self, Point.X, Point.Y, False);
end;

procedure TColumnsFileView.cm_EditPath(param: string);
begin
  pnlHeader.ShowPathEdit;
end;

procedure TColumnsFileView.cm_GoToFirstFile(param: string);
begin
  dgPanel.Row:= dgPanel.FixedRows;
end;

procedure TColumnsFileView.cm_GoToLastFile(param: string);
begin
  dgPanel.Row:= dgPanel.RowCount - 1;
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

  // Workaround for Lazarus issue 18832.
  // Set Fixed... before setting ...Count.
  FixedRows := 0;
  FixedCols := 0;

  // Override default values to start with no columns and no rows.
  RowCount := 0;
  ColCount := 0;

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

  function CalculateTabHeaderHeight: Integer;
  var
    OldFont: TFont;
  begin
    OldFont     := Canvas.Font;
    Canvas.Font := Font;
    SetCanvasFont(GetColumnFont(0, True));
    Result      := Canvas.TextHeight('Wg');
    Canvas.Font := OldFont;
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

    TabHeaderHeight := Max(gIconsSize, CalculateTabHeaderHeight);
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
  AFile: TDisplayFile;
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
    Canvas.FillRect(aRect);

    SetCanvasFont(GetColumnFont(aCol, True));

    iTextTop := aRect.Top + (RowHeights[aRow] - Canvas.TextHeight('Wg')) div 2;

    TitleX := 0;
    s      := ColumnsSet.GetColumnTitle(ACol);

    SortingDirection := ColumnsView.FColumnsSorting.GetSortingDirection(ACol);
    if SortingDirection <> sdNone then
    begin
      TitleX := TitleX + gIconsSize;
      PixMapManager.DrawBitmap(
          PixMapManager.GetIconBySortingDirection(SortingDirection),
          Canvas,
          aRect.Left, aRect.Top + (RowHeights[aRow] - gIconsSize) div 2);
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
  var
    Y: Integer;
    IconID: PtrInt;
  begin
    if (gShowIcons <> sim_none) then
    begin
      IconID := AFile.IconID;
      // Draw default icon if there is no icon for the file.
      if IconID = -1 then
        IconID := PixMapManager.GetDefaultIcon(AFile.FSFile);

      // center icon vertically
      Y:= aRect.Top + (RowHeights[ARow] - gIconsSize) div 2;

      // Draw icon for a file
      PixMapManager.DrawBitmap(IconID,
                               Canvas,
                               aRect.Left + 1,
                               Y
                               );

      // Draw overlay icon for a file if needed
      if gIconOverlays then
      begin
        PixMapManager.DrawBitmapOverlay(AFile,
                                        FileSourceDirectAccess,
                                        Canvas,
                                        aRect.Left + 1,
                                        Y
                                        );
      end;

    end;

    if AFile.DisplayStrings.Count = 0 then
      ColumnsView.MakeColumnsStrings(AFile, ColumnsSet);
    s := AFile.DisplayStrings.Strings[ACol];

    if gCutTextToColWidth then
    begin
      Y:= ((aRect.Right - aRect.Left) - 4 - Canvas.TextWidth('W'));
      if (gShowIcons <> sim_none) then Y:= Y - gIconsSize;
      if Canvas.TextWidth(s) - Y > 0 then
      begin
        repeat
          IconID:= UTF8Length(s);
          UTF8Delete(s, IconID, 1);
        until (Canvas.TextWidth(s) - Y < 1) or (IconID = 0);
        if (IconID > 0) then
        begin
          s:= UTF8Copy(s, 1, IconID - 3);
          if gDirBrackets and (AFile.FSFile.IsDirectory or AFile.FSFile.IsLinkToDirectory) then
            s:= s + '..]'
          else
            s:= s + '...';
        end;
      end;
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
    if AFile.DisplayStrings.Count = 0 then
      ColumnsView.MakeColumnsStrings(AFile, ColumnsSet);
    s := AFile.DisplayStrings.Strings[ACol];

    if gCutTextToColWidth then
    begin
      while Canvas.TextWidth(s) - (aRect.Right - aRect.Left) - 4 > 0 do
        Delete(s, Length(s), 1);
    end;

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

  procedure PrepareColors;
  //------------------------------------------------------
  var
    TextColor: TColor = -1;
    BackgroundColor: TColor;
  //---------------------
  begin
    Canvas.Font.Name   := ColumnsSet.GetColumnFontName(ACol);
    Canvas.Font.Size   := ColumnsSet.GetColumnFontSize(ACol);
    Canvas.Font.Style  := ColumnsSet.GetColumnFontStyle(ACol);

    // Set up default background color first.
    if (gdSelected in aState) and ColumnsView.Active and (not gUseFrameCursor) then
      BackgroundColor := ColumnsSet.GetColumnCursorColor(ACol)
    else
      begin
        // Alternate rows background color.
        if odd(ARow) then
          BackgroundColor := ColumnsSet.GetColumnBackground(ACol)
        else
          BackgroundColor := ColumnsSet.GetColumnBackground2(ACol);
      end;

    // Set text color.
    if ColumnsSet.GetColumnOvercolor(ACol) then
      TextColor := gColorExt.GetColorBy(AFile.FSFile);
    if TextColor = -1 then
      TextColor := ColumnsSet.GetColumnTextColor(ACol);

    if AFile.Selected then
    begin
      if gUseInvertedSelection then
        begin
          //------------------------------------------------------
          if (gdSelected in aState) and ColumnsView.Active and (not gUseFrameCursor) then
            begin
              Canvas.Font.Color := InvertColor(ColumnsSet.GetColumnCursorText(ACol));
            end
          else
            begin
              BackgroundColor := ColumnsSet.GetColumnMarkColor(ACol);
              Canvas.Font.Color := ColumnsSet.GetColumnBackground(ACol);
            end;
          //------------------------------------------------------
        end
      else
        begin
          Canvas.Font.Color := ColumnsSet.GetColumnMarkColor(ACol);
        end;
    end
    else if (gdSelected in aState) and ColumnsView.Active and (not gUseFrameCursor) then
      begin
        Canvas.Font.Color := ColumnsSet.GetColumnCursorText(ACol);
      end
    else
      begin
        Canvas.Font.Color := TextColor;
      end;

    // Draw background.
    Canvas.Brush.Color := ColumnsView.DimColor(BackgroundColor);
    Canvas.FillRect(aRect);
  end;// of PrepareColors;

  procedure DrawLines;
  begin
    // Draw frame cursor.
    if gUseFrameCursor and (gdSelected in aState) and ColumnsView.Active then
    begin
      Canvas.Pen.Color := ColumnsSet.GetColumnCursorColor(ACol);
      Canvas.Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
      Canvas.Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
    end;

    // Draw drop selection.
    if ARow = DropRowIndex then
    begin
      Canvas.Pen.Color := ColumnsSet.GetColumnTextColor(ACol);
      Canvas.Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
      Canvas.Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
    end;
  end;
  //------------------------------------------------------
  //end of subprocedures
  //------------------------------------------------------

begin
  ColumnsSet := ColumnsView.GetColumnsClass;

  if gdFixed in aState then
  begin
    DrawFixed  // Draw column headers
  end
  else if ColumnsView.FFiles.Count > 0 then
  begin
    AFile := ColumnsView.FFiles[ARow - FixedRows]; // substract fixed rows (header)
    FileSourceDirectAccess := fspDirectAccess in ColumnsView.FileSource.Properties;

    PrepareColors;

    iTextTop := aRect.Top + (RowHeights[aRow] - Canvas.TextHeight('Wg')) div 2;

    if ACol = 0 then
      DrawIconCell  // Draw icon in the first column
    else
      DrawOtherCell;
  end;

  DrawCellGrid(aCol,aRow,aRect,aState);
  DrawLines;
end;

procedure TDrawGridEx.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Point: TPoint;
  AFile: TDisplayFile;
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
      if MouseOnGrid(X, Y) then
        begin
          MouseToCell(X, Y, iCol, iRow);
          if (iRow <> HintRowIndex) and (iRow >= FixedRows) then
            begin
              HintRowIndex:= iRow;
              Application.CancelHint;
              Self.Hint:= EmptyStr; // don't show by default
              with (Parent as TColumnsFileView) do
                if InRange(HintRowIndex - FixedRows, 0, FFiles.Count - 1) then
                begin
                  AFile := FFiles[HintRowIndex - FixedRows];
                  aRect:= CellRect(0, HintRowIndex);
                  iCol:= aRect.Right - aRect.Left - 8;
                  if gShowIcons <> sim_none then
                    Dec(iCol, gIconsSize);
                  if iCol < Self.Canvas.TextWidth(AFile.FSFile.Name) then // with file name
                      Self.Hint:= AFile.FSFile.Name
                  else if (stm_only_large_name in gShowToolTipMode) then // don't show
                    Exit
                  else if not AFile.FSFile.IsDirectory then // without name
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

  if MouseOnGrid(X, Y) then
    inherited MouseDown(Button, Shift, X, Y)
  else
    begin
      if Assigned(OnMouseDown) then
        OnMouseDown(Self, Button, Shift, X, Y);
    end;
end;

function TDrawGridEx.MouseOnGrid(X, Y: LongInt): Boolean;
var
  bTemp: Boolean;
  iRow, iCol: LongInt;
begin
  bTemp:= AllowOutboundEvents;
  AllowOutboundEvents:= False;
  MouseToCell(X, Y, iCol, iRow);
  AllowOutboundEvents:= bTemp;
  Result:= not ((iCol < 0) and (iRow < 0));
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
  AFile: TDisplayFile = nil;
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
     (AFile.FSFile.IsDirectory or AFile.FSFile.IsLinkToDirectory) and
     (MouseOnGrid(ClientPoint.X, ClientPoint.Y)) then
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
    Files := TFileSystemFileSource.CreateFilesFromFileList(
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

function TDrawGridEx.GetVisibleRows: TRange;
var
  w: Integer;
  rc: Integer;
begin
  if (TopRow<0)or(csLoading in ComponentState) then begin
    Result.First := 0;
    Result.Last := -1;
    Exit;
  end;
  // visible TopLeft Cell
  Result.First:=TopRow;
  Result.Last:=Result.First;
  rc := RowCount;

  // Top Margin of next visible Row and Bottom most visible cell
  if rc>FixedRows then begin
    w:=RowHeights[Result.First] + GCache.FixedHeight - GCache.TLRowOff;
    while (Result.Last<rc-1)and(W<GCache.ClientHeight) do begin
      Inc(Result.Last);
      W:=W+RowHeights[Result.Last];
    end;
  end else begin
    Result.Last := Result.First - 1; // no visible cells here
  end;
end;

function TDrawGridEx.IsRowVisible(aRow: Integer): Boolean;
begin
  with GCache.FullVisibleGrid do
    Result:= (Top<=aRow)and(aRow<=Bottom);
end;

// -- TColumnsSortings --------------------------------------------------------

procedure TColumnsSortings.AddSorting(iColumn : Integer; SortDirection : TSortDirection);
var
  i : Integer;
  pSortingColumn : PColumnsSorting;
begin
  i := Count - 1;
  while i >= 0 do
  begin
    pSortingColumn := PColumnsSorting(Self[i]);
    if pSortingColumn^.Column = iColumn then
    begin
      pSortingColumn^.SortDirection := ReverseSortDirection(pSortingColumn^.SortDirection);
      Exit;
    end;
    dec(i);
  end;

  new(pSortingColumn);
  pSortingColumn^.Column := iColumn;
  pSortingColumn^.SortDirection := SortDirection;
  Add(pSortingColumn);
end;

Destructor TColumnsSortings.Destroy;
begin
  Clear;
  inherited;
end;

function TColumnsSortings.Clone: TColumnsSortings;
var
  i: Integer;
  pSortingColumn : PColumnsSorting;
begin
  Result := TColumnsSortings.Create;

  for i := 0 to Count - 1 do
  begin
    pSortingColumn := PColumnsSorting(Self[i]);
    Result.AddSorting(pSortingColumn^.Column, pSortingColumn^.SortDirection);
  end;
end;

procedure TColumnsSortings.Clear;
var
  i : Integer;
  pSortingColumn : PColumnsSorting;
begin
  i := Count - 1;
  while i >= 0 do
  begin
    pSortingColumn := PColumnsSorting(Self[i]);
    dispose(pSortingColumn);
    dec(i);
  end;

  Inherited Clear;
end;

function TColumnsSortings.GetSortingDirection(iColumn : Integer) : TSortDirection;
var
  i : Integer;
  pSortingColumn : PColumnsSorting;
begin
  Result := sdNone;

  i := Count - 1;
  while i >= 0 do
  begin
    pSortingColumn := PColumnsSorting(Self[i]);
    if pSortingColumn^.Column = iColumn then
    begin
      Result := pSortingColumn^.SortDirection;
      break;
    end;
    dec(i);
  end;
end;

end.

