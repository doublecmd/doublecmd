{
   Seksi Commander
   ----------------------------
   Implementing of File Panel Components, created dynamically (replacing TFrame)

   Licence  : GNU GPL v 2.0
   Author   : radek.cervinka@centrum.cz

   contributors:

   Copyright (C) 2006-2009  Koblov Alexander (Alexx2000@mail.ru)
   
   Copyright (C) 2008 Vitaly Zotov (vitalyzotov@mail.ru)
}

unit framePanel;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, LMessages, LCLIntf,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, uFilePanel, Grids, uTypes,
  Buttons, uColumns,lcltype,Menus, uFileList, uDragDropEx;

type
  TFilePanelSelect=(fpLeft, fpRight);

  TDragDropType = (ddtInternal, ddtExternal);

  // Lists all operations supported by dragging and dropping items
  // in the panel (external, internal and via menu).
  TDragDropOperation = (ddoCopy, ddoMove, ddoSymLink, ddoHardLink);

  TFrameFilePanel = class;
  TDropParams = class;

  { TDrawGridEx }

  TDrawGridEx = class(TDrawGrid)
  private
    // Used to register as a drag and drop source and target.
    DragDropSource: uDragDropEx.TDragDropSource;
    DragDropTarget: uDragDropEx.TDragDropTarget;

    StartDrag: Boolean;
    DragStartPoint: TPoint;
    DragRowIndex,
    DropRowIndex: Integer;
    LastMouseButton: TMouseButton; // Mouse button that initiated dragging

    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;

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

    procedure InitializeWnd; override;
    procedure FinalizeWnd; override;

  public
{$IFDEF LCLGTK2}
    fLastDoubleClickTime : TDateTime;

    function TooManyDoubleClicks: Boolean;
{$ENDIF}

    constructor Create(AOwner: TComponent; AParent: TWinControl);
    destructor Destroy; override;
    procedure UpdateView;

    // Returns height of all the header rows.
    function GetHeaderHeight: Integer;

    {  This function is called from various points to handle dropping files
       into the panel. It converts drop effects available on the system
       into TDragDropOperation operations.
       Handles freeing DropParams. }
    procedure DropFiles(DropParams: TDropParams);

    {  Executes operations with dropped files, can handle any TDragDropOperation.
       Handles freeing DropParams. }
    procedure DoDragDropOperation(Operation: TDragDropOperation;
                                  DropParams: TDropParams);
  end;

  { TDropParams }

  {  Parameters passed to functions handling drag&drop.

     FileList
        List of files dropped (the class handles freeing it).
     DropEffect
        Desired action to take with regard to the files.
     ScreenDropPoint
        Point where the drop occurred.
     DropIntoDirectories
        If true it is/was allowed to drop into specific directories
        (directories may have been tracked while dragging).
        Target path will be modified accordingly if ScreenDropPoint points
        to a directory in the target panel.
     SourcePanel
        If drag drop type is internal, this field points to the source panel.
     TargetPanel
        Panel, where the drop occurred. }
  TDropParams = class
  public
    FileList: TFileList;
    DropEffect: TDropEffect;
    ScreenDropPoint: TPoint;
    DropIntoDirectories: Boolean;
    SourcePanel: TFrameFilePanel;
    TargetPanel: TFrameFilePanel;

    constructor Create(aFileList: TFileList; aDropEffect: TDropEffect;
                       aScreenDropPoint: TPoint; aDropIntoDirectories: Boolean;
                       aSourcePanel: TFrameFilePanel;
                       aTargetPanel: TFrameFilePanel);
    destructor Destroy;

    // States, whether the drag&drop operation was internal or external.
    // If SourcePanel is not nil, then it's assumed it was internal.
    function GetDragDropType: TDragDropType;
  end;
  PDropParams = ^TDropParams;

  { TPathLabel }

  TPathLabel = class(TLabel)
  private
    HighlightStartPos: Integer;
    HighlightText: String;
    {en
       How much space to leave between the text and left border.
    }
    LeftSpacing: Integer;

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

    procedure MouseEnter(Sender: TObject);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseLeave(Sender: TObject);

  protected
    procedure Paint; override;

  public

    constructor Create(AOwner: TComponent); override;

    {en
       Changes drawing colors depending active/inactive state.
    }
    procedure SetActive(Active: Boolean);

    function GetSelectedDir: String;
  end;

  { TFrameFilePanel }

  TFrameFilePanel = class (TWinControl)
  private
    fGridVertLine,
    fGridHorzLine,
    fSearchDirect,
    fNext,
    fPrevious : Boolean;
    procedure edSearchKeyPress(Sender: TObject; var Key: Char);
    procedure edSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    pnlFooter: TPanel;
    pnPanel: TPanel;
    lblLInfo: TLabel;
    pnlHeader: TPanel;
    lblLPath: TPathLabel;
    edtPath,
    edtRename: TEdit;
//---------------------
    dgPanel: TDrawGridEx;
    ActiveColm:String;
    ActiveColmSlave:TPanelColumnsClass;
    isSlave:boolean;
//---------------------
    pnAltSearch: TPanel;
    edtSearch: TEdit;
    procedure UpdateColCount(NewColCount: Integer);
    procedure SetColWidths;
    procedure edSearchChange(Sender: TObject);
    procedure edtPathKeyPress(Sender: TObject; var Key: Char);
    procedure edtRenameKeyPress(Sender: TObject; var Key: Char);
    procedure dgPanelDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure dgPanelExit(Sender: TObject);
    procedure dgPanelDblClick(Sender: TObject);
    procedure dgPanelEnter(Sender: TObject);
    procedure dgPanelKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dgPanelKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure dgPanelMouseDown(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);

    procedure dgPanelStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure dgPanelDragOver(Sender, Source: TObject; X, Y: Integer;
                                               State: TDragState; var Accept: Boolean);
    procedure dgPanelDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure dgPanelEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure dgPanelHeaderClick(Sender: TObject;IsColumn: Boolean; index: Integer);
    procedure dgPanelKeyPress(Sender: TObject; var Key: Char);
    procedure dgPanelPrepareCanvas(sender: TObject; Col, Row: Integer; aState: TGridDrawState);
    procedure dgPanelMouseWheelUp(Sender: TObject; Shift: TShiftState;
                                  MousePos: TPoint; var Handled: Boolean);
    procedure dgPanelMouseWheelDown(Sender: TObject; Shift: TShiftState;
                                  MousePos: TPoint; var Handled: Boolean);
    procedure lblLPathClick(Sender: TObject);
    procedure pnlHeaderResize(Sender: TObject);

  private
    { Private declarations }
    FActive: Boolean;
    FLastMark:String;
    FLastSelect:TGridRect;
    FLastAutoSelect: Boolean;
    FLastSelectionStartRow: Integer;
    procedure SetGridHorzLine(const AValue: Boolean);
    procedure SetGridVertLine(const AValue: Boolean);
  protected
    function StartDragEx(MouseButton: TMouseButton; ScreenStartPoint: TPoint): Boolean;
    procedure SelectRange(iRow: PtrInt);
  public
    { Public declarations }
    pnlFile:TFilePanel;
    edtCmdLine:TComboBox;
    PanelSelect:TFilePanelSelect;
    constructor Create(AOwner :TWinControl; lblDriveInfo : TLabel; lblCommandPath:TLabel; cmbCommand:TComboBox);
    destructor Destroy; override;
    procedure SetFocus;
    procedure SelectFile(frp:PFileRecItem);
    { Returns True if at least one file is/was selected. }
    function  SelectFileIfNoSelected(frp:PFileRecItem):Boolean;
    procedure UnSelectFileIfSelected(frp:PFileRecItem);
    procedure MakeVisible(iRow:Integer);
    procedure MakeSelectedVisible;
    procedure InvertAllFiles;
    procedure MarkAll;
    procedure RefreshPanel(bUpdateFileCount: Boolean = True; bUpdateDiskFreeSpace: Boolean = True);
    procedure ClearCmdLine;
    procedure CloseAltPanel;
    procedure ShowAltPanel(Char : TUTF8Char = #0);
    procedure UnMarkAll;
    procedure UpDatelblInfo;
    Function GetActiveDir:String;
    procedure MarkMinus;
    procedure MarkPlus;
    procedure MarkShiftPlus;
    procedure MarkShiftMinus;
    function AnySelected:Boolean;
    procedure ClearGridSelection;
    procedure RedrawGrid;
    procedure UpdateColumnsView;
    procedure UpdateView;
    function GetActiveItem:PFileRecItem;
    { Returns True if there are no files shown in the panel. }
    function IsEmpty:Boolean;
    function IsActiveItemValid:Boolean;
    function GetColumnsClass: TPanelColumnsClass;

    property ActiveDir:String read GetActiveDir;
    property GridVertLine: Boolean read fGridVertLine write SetGridVertLine;
    property GridHorzLine: Boolean read fGridHorzLine write SetGridHorzLine;
  end;

implementation

uses
  LCLProc, Masks, uLng, uShowMsg, uGlobs, GraphType, uPixmapManager, uVFSUtil,
  uDCUtils, uOSUtils, math, fMain, fSymLink, fHardLink, uFileSorting
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  , GtkProc  // for ReleaseMouseCapture
  , GTKGlobals  // for DblClickTime
{$ENDIF}
  ;


procedure TFrameFilePanel.SetFocus;
begin
  with FLastSelect do
  begin
    if top<0 then Top:=0;
    if Left<0 then Left:=0;
    if Right<0 then Right:=0;
    if Bottom<0 then Bottom:=0;
  end;
  if dgPanel.Row<0 then
    dgPanel.Selection:=FLastSelect;
  if frmMain.Visible and dgPanel.CanFocus then
    dgPanel.SetFocus;
  lblLPath.SetActive(True);
  pnlFile.UpdatePrompt;
  if Parent is TPage then
    frmMain.UpdateSelectedDrive(Parent.Parent as TNoteBook);
//  dgPanel.Invalidate;
end;

procedure TFrameFilePanel.SelectFile(frp:PFileRecItem);
begin
  pnlFile.InvertFileSection(frp);
  UpDatelblInfo;
end;

function TFrameFilePanel.SelectFileIfNoSelected(frp:PFileRecItem):Boolean;
var
  i:Integer;
begin
  FLastAutoSelect:= False;
  for i:=0 to pnlFile.FileList.Count-1 do
  begin
    if pnlFile.FileList.GetItem(i)^.bSelected then
    begin
      Result := True;
      Exit;
    end;
  end;
  if pnlFile.IsItemValid(frp) then
  begin
    pnlFile.InvertFileSection(frp);
    UpDatelblInfo;
    FLastAutoSelect:= True;
    Result := True;
  end
  else
    Result := False;
end;

procedure TFrameFilePanel.UnSelectFileIfSelected(frp:PFileRecItem);
begin
  if FLastAutoSelect and Assigned(frp) and (frp^.bSelected) then
    begin
      pnlFile.InvertFileSection(frp);
      UpDatelblInfo;
    end;
  FLastAutoSelect:= False;
end;

procedure TFrameFilePanel.InvertAllFiles;
begin
  pnlFile.InvertAllFiles;
  UpDatelblInfo;
  dgPanel.Invalidate;
end;

procedure TFrameFilePanel.RefreshPanel(bUpdateFileCount: Boolean = True; bUpdateDiskFreeSpace: Boolean = True);
var
  LastSelection: String;
begin
  if pnAltSearch.Visible then
    CloseAltPanel;

  if dgPanel.Row >= 0 then
  begin
    if Assigned(pnlFile.GetActiveItem) then
      LastSelection := pnlFile.GetActiveItem^.sName
    else
      LastSelection := '';
  end;

  pnlFile.Refresh(bUpdateFileCount, bUpdateDiskFreeSpace);
  pnlFile.Select(LastSelection);

  UpDatelblInfo;
end;


procedure TFrameFilePanel.ClearCmdLine;
begin
  edtCmdLine.Text:='';
//  dgPanel.SetFocus;
end;

function TFrameFilePanel.StartDragEx(MouseButton: TMouseButton; ScreenStartPoint: TPoint): Boolean;
var
  fileNamesList: TStringList;
  draggedFileItem, frp: PFileRecItem;
  i: Integer;
begin
  Result := False;

  if dgPanel.DragRowIndex >= dgPanel.FixedRows then
  begin
    draggedFileItem := pnlFile.GetReferenceItemPtr(dgPanel.DragRowIndex - dgPanel.FixedRows); // substract fixed rows (header)

    fileNamesList := TStringList.Create;
    try
      if SelectFileIfNoSelected(draggedFileItem) = True then
      begin
        for i := 0 to pnlFile.FileList.Count-1 do
        begin
          frp := pnlFile.FileList.GetItem(i);
          if frp^.bSelected then
            fileNamesList.Add(ActiveDir + frp^.sName);
        end;

        // Initiate external drag&drop operation.
        Result := dgPanel.DragDropSource.DoDragDrop(fileNamesList, MouseButton, ScreenStartPoint);

        // Refresh source file panel after drop to (possibly) another application
        // (files could have been moved for example).
        // 'draggedFileItem' is invalid after this.
        RefreshPanel;
      end;

    finally
      FreeAndNil(fileNamesList);
    end;
  end;
end;

procedure TFrameFilePanel.SelectRange(iRow: PtrInt);
var
  ARow, AFromRow, AToRow: Integer;
  frp: PFileRecItem;
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

  pnlFile.MarkAllFiles(False);
  for ARow := AFromRow to AToRow do
  begin
    frp := pnlFile.GetReferenceItemPtr(ARow);
    if not Assigned(frp) then Continue;
    pnlFile.MarkFile(frp, True);
  end;
  UpDatelblInfo;
  dgPanel.Invalidate;
end;

procedure TFrameFilePanel.dgPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  iRow, iCol : Integer;
  frp: PFileRecItem;
begin
  if (Y < dgPanel.GetHeaderHeight) then Exit; // if is header

  SetFocus;

  if IsEmpty then Exit;

  dgPanel.MouseToCell(X, Y, iCol, iRow);
  case Button of
    mbRight: begin
      dgPanel.Row := iRow;

      if (gMouseSelectionEnabled) and (gMouseSelectionButton = 1) then
      begin
        frp := pnlFile.GetReferenceItemPtr(iRow - dgPanel.FixedRows); // substract fixed rows (header)
        if Assigned(frp) then
        begin
          SelectFile(frp);
          dgPanel.Invalidate;
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
            frp := pnlFile.GetReferenceItemPtr(iRow - dgPanel.FixedRows); // substract fixed rows (header)
            if Assigned(frp) then
              begin
                SelectFile(frp);
                dgPanel.Invalidate;
              end;
          end
        else if ssShift in Shift then
          begin
            SelectRange(iRow);
          end
        else if (gMouseSelectionButton = 0) then
          begin
            frp := pnlFile.GetReferenceItemPtr(iRow - dgPanel.FixedRows); // substract fixed rows (header)
            if Assigned(frp) and not frp^.bSelected then
              begin
                pnlFile.MarkAllFiles(False);
                UpDatelblInfo;
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
    dgPanel.LastMouseButton:= Button;
    dgPanel.DragStartPoint.X := X;
    dgPanel.DragStartPoint.Y := Y;
    dgPanel.DragRowIndex := iRow;
    uDragDropEx.TransformDragging := False;
    uDragDropEx.AllowTransformToInternal := True;
  end;
end;

procedure TFrameFilePanel.dgPanelStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
end;

procedure TFrameFilePanel.dgPanelDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  iRow, Dummy: Integer;
  fri: PFileRecItem = nil;
  SourcePanel: TFrameFilePanel = nil;
  TargetPanel: TFrameFilePanel = nil;
  SourceDir, TargetDir: String;
begin
  Accept := False;

  // Always allow dropping into an empty panel.
  // And it is also allowed to drop onto header in case all visible items
  // are directories and the user wants to drop into panel's current directory.
  if IsEmpty or (Y < dgPanel.GetHeaderHeight) then
  begin
    dgPanel.ChangeDropRowIndex(-1);
    Accept:= True;
    Exit;
  end;

  if (Source is TDrawGridEx) and (Sender is TDrawGridEx) then
  begin
    SourcePanel := ((Source as TDrawGridEx).Parent) as TFrameFilePanel;
    TargetPanel := ((Sender as TDrawGridEx).Parent) as TFrameFilePanel;

    SourceDir := SourcePanel.ActiveDir;
    TargetDir := TargetPanel.ActiveDir;
  end;

  dgPanel.MouseToCell(X, Y, Dummy, iRow);

  if iRow >= dgPanel.FixedRows then
    fri:= pnlFile.GetReferenceItemPtr(iRow - dgPanel.FixedRows); // substract fixed rows (header)

  if Assigned(fri) and (FPS_ISDIR(fri^.iMode) or fri^.bLinkIsDir) and (Y < dgPanel.GridHeight) then
    begin
      if State = dsDragLeave then
        // Mouse is leaving the control or drop will occur immediately.
        // Don't draw DropRow rectangle.
        dgPanel.ChangeDropRowIndex(-1)
      else
        dgPanel.ChangeDropRowIndex(iRow);

      if Sender = Source then
      begin
        if not ((iRow = dgPanel.DragRowIndex) or (fri^.bSelected = True)) then
          Accept := True;
      end
      else
      begin
        if Assigned(SourcePanel) and Assigned(TargetPanel) then
        begin
          if fri^.sName = '..' then
            TargetDir := GetParentDir(TargetDir)
          else
            TargetDir := TargetDir + fri^.sName + DirectorySeparator;

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
        if SourcePanel.ActiveDir <> TargetPanel.ActiveDir then
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

procedure TFrameFilePanel.dgPanelDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  SourcePanel: TFrameFilePanel;
  FileList: TFileList;
begin
  if (Sender is TDrawGridEx) and (Source is TDrawGridEx) then
  begin
    SourcePanel := ((Source as TDrawGridEx).Parent) as TFrameFilePanel;

    // Get file names from source panel.
    with SourcePanel do
    begin
      if SelectFileIfNoSelected(GetActiveItem) = False then Exit;

      FileList := TFileList.Create;
      try
        CopyListSelectedExpandNames(pnlFile.FileList, FileList, ActiveDir);
        UnSelectFileIfSelected(GetActiveItem);
      except
        FreeAndNil(FileList);
        UnSelectFileIfSelected(GetActiveItem);
        Exit;
      end;
    end;

    // Drop onto target panel.
    with Sender as TDrawGridEx do
    begin
      DropFiles(TDropParams.Create(
        FileList, // Will be freed automatically.
        GetDropEffectByKeyAndMouse(GetKeyShiftState,
                                  (Source as TDrawGridEx).LastMouseButton),
        ClientToScreen(Classes.Point(X, Y)),
        True,
        SourcePanel,
        Self));

      ChangeDropRowIndex(-1);
    end;
  end;
end;

procedure TFrameFilePanel.dgPanelEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  // If cancelled by the user, DragManager does not send drag-leave event
  // to the target, so we must clear the DropRow in both panels.
  frmMain.FrameLeft.dgPanel.ChangeDropRowIndex(-1);
  frmMain.FrameRight.dgPanel.ChangeDropRowIndex(-1);

  if uDragDropEx.TransformDragging = False then
    dgPanel.ClearMouseButtonAfterDrag;
end;

procedure TFrameFilePanel.dgPanelHeaderClick(Sender: TObject;
  IsColumn: Boolean; Index: Integer);
var
  ShiftState : TShiftState;
  SortingDirection : TSortDirection = sdAscending;
begin
  if not IsColumn then Exit;

  ShiftState := GetKeyShiftState;
  if not ((ssShift in ShiftState) or (ssCtrl in ShiftState)) then
  begin
    SortingDirection := pnlFile.Sorting.GetSortingDirection(Index);
    if SortingDirection = sdNone then
      SortingDirection := sdAscending
    else
      SortingDirection := ReverseSortDirection(SortingDirection);
    pnlFile.Sorting.Clear;
  end;

  pnlFile.Sorting.AddSorting(Index, SortingDirection);
  pnlFile.Sort;
  dgPanel.Invalidate;
end;

procedure TFrameFilePanel.dgPanelKeyPress(Sender: TObject; var Key: Char);
begin
  DebugLn('dgpanel:' + Key)
end;

procedure TFrameFilePanel.dgPanelPrepareCanvas(sender: TObject; Col,
  Row: Integer; aState: TGridDrawState);
begin
  if (Row = 0) and gTabHeader then Exit;
  with dgPanel do
  begin
    if Color <> gBackColor then
      Color:= gBackColor;
  end;
end;

procedure TFrameFilePanel.dgPanelMouseWheelUp(Sender: TObject;
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

procedure TFrameFilePanel.dgPanelMouseWheelDown(Sender: TObject;
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

procedure TFrameFilePanel.edSearchKeyPress(Sender: TObject; var Key: Char);
begin
  if (key=#13) or (key=#27) then
  begin
    CloseAltPanel;
    SetFocus;
  end;

  {LaBero begin}
  {en
      Execute/open selected file/directory
      if the user press ENTER during QuickSearch
  }
  if (key=#13) then
  begin
    try
       pnlFile.ChooseFile(pnlFile.GetActiveItem);
       UpDatelblInfo;
    finally
       dgPanel.Invalidate;
       Screen.Cursor:=crDefault;
    end;
  end;
  {LaBero end}
end;

procedure TFrameFilePanel.edSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_DOWN:
      begin
        fSearchDirect := True;
        fNext := True;
        Key := 0;
        edSearchChange(Sender);
      end;

    VK_UP:
      begin
        fSearchDirect := False;
        fPrevious := True;
        Key := 0;
        edSearchChange(Sender);
      end;

    VK_TAB:
      begin
        CloseAltPanel;
        SetFocus;
        Key := 0;
      end;
  end;
end;

procedure TFrameFilePanel.UpdateColCount(NewColCount: Integer);
begin
  while dgPanel.Columns.Count < NewColCount do
    dgPanel.Columns.Add;
  while dgPanel.Columns.Count > NewColCount do
    dgPanel.Columns.Delete(0);
end;

procedure TFrameFilePanel.SetColWidths;
var
  x: Integer;
  ColumnsClass: TPanelColumnsClass;
begin
  //  setup column widths
  ColumnsClass := GetColumnsClass;

  UpdateColCount(ColumnsClass.ColumnsCount);
  if ColumnsClass.ColumnsCount>0 then
    for x:=0 to ColumnsClass.ColumnsCount-1 do
      begin
        dgPanel.Columns.Items[x].SizePriority:= 0;
        dgPanel.ColWidths[x]:= ColumnsClass.GetColumnWidth(x);
        dgPanel.Columns.Items[x].Title.Caption:= ColumnsClass.GetColumnTitle(x);
      end;
end;

procedure TFrameFilePanel.edSearchChange(Sender: TObject);
var
  I, iPos, iEnd : Integer;
  Result : Boolean;
  sSearchName,
  sSearchNameNoExt,
  sSearchExt : String;
begin
  if edtSearch.Text='' then Exit;
  //DebugLn('edSearchChange: '+ edSearch.Text);

  sSearchName := AnsiLowerCase(edtSearch.Text);

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
        I := edtSearch.Tag + 1; // begin search from next file
      iEnd := dgPanel.RowCount;
    end
  else
    begin
      if fPrevious then
        I := edtSearch.Tag - 1; // begin search from previous file
      iEnd := dgPanel.FixedRows;
    end;
  if I < 1 then I := 1;
  
  try
    while I <> iEnd do
      begin
        Result := MatchesMask(AnsiLowerCase(pnlFile.GetReferenceItemPtr(I-1)^.sName), sSearchName);

        if Result then
          begin
            dgPanel.Row := I;
            MakeVisible(I);
            edtSearch.Tag := I;
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
            I := 1;
            iEnd := iPos;
  		  iPos := 1;
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

procedure TFrameFilePanel.CloseAltPanel;
begin
  pnAltSearch.Visible:=False;
  edtSearch.Text:='';
  FActive:= False;
end;

procedure TFrameFilePanel.ShowAltPanel(Char : TUTF8Char);
begin
  frmMain.EnableHotkeys(False);

  edtSearch.Height   := pnAltSearch.Canvas.TextHeight('Pp') + 1
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
  FActive:= True;
end;

procedure TFrameFilePanel.UnMarkAll;
begin
  pnlFile.MarkAllFiles(False);
  UpDatelblInfo;
  dgPanel.Invalidate;
end;

procedure TFrameFilePanel.UpDatelblInfo;
begin
  with pnlFile do
  begin
    UpdateCountStatus;
    lblLInfo.Caption:=Format(rsMsgSelected,
      [cnvFormatFileSize(SizeSelected), cnvFormatFileSize(SizeInDir) ,FilesSelected, FilesInDir ]);
  end;
end;


procedure TFrameFilePanel.MarkAll;
begin
  pnlFile.MarkAllFiles(True);
  UpDatelblInfo;
  dgPanel.Invalidate;
end;

Function TFrameFilePanel.GetActiveDir:String;
begin
  Result:=pnlFile.ActiveDir;
end;


procedure TFrameFilePanel.MarkPlus;
var
  s:String;
begin
  if IsEmpty then Exit;
  s:=FLastMark;
  if not ShowInputComboBox(rsMarkPlus, rsMaskInput, glsMaskHistory, s) then Exit;
  FLastMark:=s;
  pnlFile.MarkGroup(s,True);
  UpDatelblInfo;
  dgPanel.Invalidate;
end;

procedure TFrameFilePanel.MarkShiftPlus;
begin
  if IsActiveItemValid then
  with GetActiveItem^ do
  begin
    pnlFile.MarkGroup('*'+sExt, True);
    UpDatelblInfo;
    dgPanel.Invalidate;
  end;
end;

procedure TFrameFilePanel.MarkShiftMinus;
begin
  if IsActiveItemValid then
  with GetActiveItem^ do
  begin
    pnlFile.MarkGroup('*'+sExt ,False);
    UpDatelblInfo;
    dgPanel.Invalidate;
  end;
end;

procedure TFrameFilePanel.MarkMinus;
var
  s:String;
begin
  if IsEmpty then Exit;
  s:=FLastMark;
  if not ShowInputComboBox(rsMarkMinus, rsMaskInput, glsMaskHistory, s) then Exit;
  FLastMark:=s;
  pnlFile.MarkGroup(s,False);
  UpDatelblInfo;
  dgPanel.Invalidate;
end;

procedure TFrameFilePanel.edtPathKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key=#27 then
  begin
    edtPath.Visible:=False;
    SetFocus;
  end;
  if Key=#13 then
  begin
    Key:=#0; // catch the enter

    pnlFile.ActiveDir:=edtPath.Text;
    edtPath.Visible:=False;

    SetFocus;
  end;
end;

procedure TFrameFilePanel.edtRenameKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key=#27 then
  begin
    edtRename.Visible:=False;
    UnMarkAll;
    SetFocus;
  end;
  if Key=#13 then
  begin
    Key:=#0; // catch the enter
    mbRenameFile(edtRename.Hint, ExtractFilePath(edtRename.Hint)+edtRename.Text);
    edtRename.Visible:=False;
    pnlFile.LastActive:=edtRename.Text;
    RefreshPanel;
    SetFocus;
  end;
end;

procedure TFrameFilePanel.dgPanelDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
 var
  //shared variables
  s:String;
  frp:PFileRecItem;
  iTextTop : Integer;

 //------------------------------------------------------
 //begin subprocedures
 //------------------------------------------------------

 function DrawFixed:boolean;
 //------------------------------------------------------
   var
      SortingDirection : TSortDirection;
      TitleX: Integer;
   begin
     Result:= False;

      if (ARow = 0) and gTabHeader then
        begin
          // Draw fixed header
          if not (ACol in [0..ActiveColmSlave.ColumnsCount-1]) then Exit;

          iTextTop := Rect.Top + (dgPanel.RowHeights[0] div 2) - (dgPanel.Canvas.TextHeight('Pp') div 2);
          with dgPanel do
          begin
            TitleX := 0;
            s := ActiveColmSlave.GetColumnTitle(ACol);

            SortingDirection := pnlFile.Sorting.GetSortingDirection(ACol);
            if SortingDirection <> sdNone then
            begin
              TitleX := TitleX + gIconsSize;
              PixMapManager.DrawBitmap(PixMapManager.GetIconBySortingDirection(SortingDirection), Canvas, Rect);
            end;

            TitleX := max(TitleX, 4);

            if gCutTextToColWidth then
              begin
                while Canvas.TextWidth(s) - ((Rect.Right - Rect.Left) - TitleX) > 0 do
                  UTF8Delete(s, UTF8Length(s), 1);
              end;

            Canvas.TextOut(Rect.Left + TitleX, iTextTop, s);
          end;
          Result := True;
        end;
   end; // of DrawHeader
  //------------------------------------------------------


  procedure DrawIconRaw;
  //------------------------------------------------------
   var
      Tr: TRect;
  begin
   with frp^, dgPanel do
     begin
       if (iIconID >= 0) and gShowIcons then
       begin
         Tr:=Rect;
         Tr.Left:=Tr.Left+1;
         PixMapManager.DrawBitmap(iIconID, Canvas, Tr);
       end;

       s:=ActiveColmSlave.GetColumnItemResultString(ACol,frp);
       if gCutTextToColWidth then
         begin
           while Canvas.TextWidth(s)-(Rect.Right-Rect.Left)-4>0 do
             Delete(s,Length(s),1);
         end;
       Canvas.Brush.Style:= bsClear;
       if gShowIcons then
         Canvas.TextOut(Rect.Left + gIconsSize + 3 ,iTextTop,s)
       else
         Canvas.TextOut(Rect.Left + 2 ,iTextTop,s);
       Canvas.Brush.Style:= bsSolid;
     end;
  end; //of DrawIconRaw
  //------------------------------------------------------
  
  Procedure DrawOtherRow;
  //------------------------------------------------------
    var
       tw, cw:Integer;
  begin
     with frp^, dgPanel do
       begin
        s:=ActiveColmSlave.GetColumnItemResultString(ACol,frp);
        if gCutTextToColWidth then
          begin
            while Canvas.TextWidth(s)-(Rect.Right-Rect.Left)-4>0 do
              Delete(s,Length(s),1);
          end;
         Canvas.Brush.Style:= bsClear;
         case ActiveColmSlave.GetColumnAlign(ACol) of
           taRightJustify:  begin
                              cw:=ColWidths[ACol];
                              tw:=Canvas.TextWidth(s);
                              Canvas.TextOut(Rect.Left+cw-tw-3,iTextTop,s);
                            end;
           taLeftJustify:   Canvas.TextOut(Rect.Left+3,iTextTop,s);
           taCenter:        begin
                              cw:=ColWidths[ACol];
                              tw:=Canvas.TextWidth(s);
                              Canvas.TextOut(Rect.Left+((cw-tw-3) div 2),iTextTop,s);
                            end;
         end; //of case
         Canvas.Brush.Style:= bsSolid;
       end;//of with
  end; //of DrawOtherRow;
  //------------------------------------------------------
  

  Procedure NewPrepareColors;
  //------------------------------------------------------
    var
       newColor,tmp:TColor;
       procedure TextSelect;
        //---------------------
         begin
           with frp^, dgPanel do
             begin
              tmp:=ActiveColmSlave.GetColumnTextColor(ACol);
              if (tmp<>newColor) and (newColor<>-1) and (ActiveColmSlave.GetColumnOvercolor(ACol)) then
                  Canvas.Font.Color:=newColor
               else  Canvas.Font.Color:= tmp;

             end;
         end;
        //---------------------
   begin
      with frp^, dgPanel do
        begin
          Canvas.Font.Name:=ActiveColmSlave.GetColumnFontName(ACol);
          Canvas.Font.Size:=ActiveColmSlave.GetColumnFontSize(ACol);
          Canvas.Font.Style:=ActiveColmSlave.GetColumnFontStyle(ACol);
          Canvas.Brush.Style:=bsSolid;

          if (gdSelected in State) and FActive then
{*}         Canvas.Brush.Color:= ActiveColmSlave.GetColumnCursorColor(ACol)
          else
            begin
              if (ARow mod 2) = 0 then
{*}                Canvas.Brush.Color := ActiveColmSlave.GetColumnBackground(ACol)
              else
{*}                Canvas.Brush.Color := ActiveColmSlave.GetColumnBackground2(ACol);
            end;

          Canvas.FillRect(Rect);
          //Canvas.Font.Style:=[];
          newColor:=gColorExt.GetColorBy(frp);
{*}       if bSelected then
            begin
              if gUseInvertedSelection then
                begin
                //------------------------------------------------------
                  if (gdSelected in State) and FActive then
                    begin
                       Canvas.Brush.Color :=ActiveColmSlave.GetColumnCursorColor(ACol);
                       Canvas.FillRect(Rect);
                       Canvas.Font.Color:=InvertColor(ActiveColmSlave.GetColumnCursorText(ACol));
                    end else
                     begin
                       Canvas.Brush.Color := ActiveColmSlave.GetColumnMarkColor(ACol);
                       Canvas.FillRect(Rect);
                       TextSelect;
                     end;
                //------------------------------------------------------
                end else
              Canvas.Font.Color:= ActiveColmSlave.GetColumnMarkColor(ACol)
            end
          else
           if (gdSelected in State) and FActive then
{*}             Canvas.Font.Color:=ActiveColmSlave.GetColumnCursorText(ACol)
          else
             begin
{*}            TextSelect;
             end;
          // draw drop selection
          if ARow = DropRowIndex then
            begin
              Canvas.Pen.Color:= ActiveColmSlave.GetColumnTextColor(ACol);
              Canvas.Line(Rect.Left,Rect.Top, Rect.Right, Rect.Top);
              Canvas.Line(Rect.Left,Rect.Bottom-1, Rect.Right, Rect.Bottom-1);
            end;
        end;//of with
   end;// of NewPrepareColors;
//------------------------------------------------------

//------------------------------------------------------
//end of subprocedures
//------------------------------------------------------

begin
  if not isSlave then  ActiveColmSlave:=ColSet.GetColumnSet(ActiveColm);

  if DrawFixed then exit;

  if (ARow>=dgPanel.RowCount)or (ARow<0) then Exit;
  if (ACol>=dgPanel.ColCount)or (ACol<0) then Exit;
  frp:=pnlFile.GetReferenceItemPtr(ARow - dgPanel.FixedRows); // substract fixed rows (header)
  if not Assigned(frp) then Exit;

  NewPrepareColors;

  iTextTop := Rect.Top + (gIconsSize div 2) - (dgPanel.Canvas.TextHeight('Pp') div 2);

  if ACol=0 then
    DrawIconRaw
  else
    DrawOtherRow;
end;

procedure TFrameFilePanel.MakeVisible(iRow:Integer);
{var
  iNewTopRow:Integer;}
begin
  with dgPanel do
  begin
    if iRow<TopRow then
      TopRow:=iRow;
    if iRow>TopRow+VisibleRowCount then
      TopRow:=iRow-VisibleRowCount;
  end;
end;

procedure TFrameFilePanel.dgPanelExit(Sender: TObject);
begin
//  DebugLn(Self.Name+'.dgPanelExit');
//  edtRename.OnExit(Sender);        // this is hack, because onExit is NOT called
{  if pnAltSearch.Visible then
    CloseAltPanel;}
  FActive:= False;
  lblLPath.SetActive(False);
  ClearGridSelection;
end;

procedure TFrameFilePanel.MakeSelectedVisible;
begin
  if dgPanel.Row>=0 then
    MakeVisible(dgPanel.Row);
end;

function TFrameFilePanel.AnySelected:Boolean;
begin
  Result:=dgPanel.Row>=0;
end;

procedure TFrameFilePanel.ClearGridSelection;
var
  nilRect:TGridRect;
begin
  FLastSelect:=dgPanel.Selection;
  nilRect.Left:=-1;
  nilRect.Top:=-1;
  nilRect.Bottom:=-1;
  nilRect.Right:=-1;
  dgPanel.Selection:=nilRect;
end;

procedure TFrameFilePanel.dgPanelDblClick(Sender: TObject);
var
  Point : TPoint;
begin
{$IFDEF LCLGTK2}
  // Workaround for two doubleclicks being sent on GTK.
  if dgPanel.TooManyDoubleClicks then Exit;
{$ENDIF}

  dgPanel.StartDrag:= False; // don't start drag on double click
  Point:= dgPanel.ScreenToClient(Mouse.CursorPos);

  // If not on a file/directory then exit.
  if (Point.Y <  dgPanel.GetHeaderHeight) or
     (Point.Y >= dgPanel.GridHeight) or
     IsEmpty then Exit;

  if pnlFile.PanelMode = pmDirectory then
    Screen.Cursor:=crHourGlass;
  try
    pnlFile.ChooseFile(pnlFile.GetActiveItem);
    UpDatelblInfo;
  finally
    dgPanel.Invalidate;
    Screen.Cursor:=crDefault;
  end;

{$IFDEF LCLGTK2}
  dgPanel.fLastDoubleClickTime := Now;
{$ENDIF}
end;

procedure TFrameFilePanel.dgPanelEnter(Sender: TObject);
begin
//  DebugLn(Self.Name+'.OnEnter');
  CloseAltPanel;
//  edtRename.OnExit(Sender);        // this is hack, bacause onExit is NOT called
  FActive:= True;
  SetFocus;
  UpDatelblInfo;
  frmMain.EnableHotkeys(True);
end;

procedure TFrameFilePanel.RedrawGrid;
begin
  dgPanel.Invalidate;
end;

procedure TFrameFilePanel.UpdateColumnsView;
var
  ColumnsClass: TPanelColumnsClass;
begin
  ColumnsClass := GetColumnsClass;

  dgPanel.FocusRectVisible := ColumnsClass.GetCursorBorder;
  dgPanel.FocusColor := ColumnsClass.GetCursorBorderColor;
end;

procedure TFrameFilePanel.dgPanelKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_SHIFT: begin
      FLastSelectionStartRow := -1;
    end;
  end;
end;

procedure TFrameFilePanel.dgPanelKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ScreenPoint:TPoint;
begin
  if Key=VK_INSERT then
  begin
    if not IsEmpty then
    begin
      if IsActiveItemValid then
        SelectFile(GetActiveItem);
      dgPanel.InvalidateRow(dgPanel.Row);
      if dgPanel.Row<dgPanel.RowCount-1 then
        dgPanel.Row:=dgPanel.Row+1;
      MakeSelectedVisible;
    end;
    Exit;
  end;

  if Key=VK_MULTIPLY then
  begin
    InvertAllFiles;
    Exit;
  end;

  if Key=VK_ADD then
  begin
    if shift=[ssCtrl] then
      MarkAll;
    if shift=[] then
      MarkPlus;
    if shift=[ssShift] then
      MarkShiftPlus;
    Exit;
  end;

  if Key=VK_SUBTRACT then
  begin
    if shift=[ssCtrl] then
      UnMarkAll;

    if shift=[] then
      MarkMinus;
    if shift=[ssShift] then
      MarkShiftMinus;
    Exit;
  end;

  if Key = VK_SHIFT then
    begin
      FLastSelectionStartRow:= dgPanel.Row;
      Exit;
    end;

  if ((Key=VK_END) or (Key=VK_HOME) or (Key=VK_NEXT) or (Key=VK_PRIOR)) and (ssShift in Shift) then
    Application.QueueAsyncCall(@SelectRange, -1);

  if ((Key=VK_DOWN) or (Key=VK_UP)) and (ssShift in Shift) then
    begin
      if IsActiveItemValid then
      begin
        SelectFile(GetActiveItem);
        if (dgPanel.Row=dgPanel.RowCount-1) or (dgPanel.Row=dgPanel.FixedRows) then
          dgPanel.Invalidate;
      end;
    end;

  {$IFDEF LCLGTK2}
   if ((dgPanel.Row=dgPanel.RowCount-1) and (key=VK_DOWN))
   or ((dgPanel.Row=dgPanel.FixedRows) and (key=VK_UP)) then
    key:=0;
  {$ENDIF}

  if dgPanel.Dragging and (Key = VK_MENU) then // Alt key
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

function TFrameFilePanel.GetActiveItem:PFileRecItem;
begin
  Result:=pnlFile.GetActiveItem;
end;

function TFrameFilePanel.IsEmpty:Boolean;
begin
  Result := pnlFile.IsEmpty;
end;

function TFrameFilePanel.IsActiveItemValid:Boolean;
begin
  Result := pnlFile.IsItemValid(GetActiveItem);
end;

procedure TFrameFilePanel.lblLPathClick(Sender: TObject);
begin
  SetFocus;

  if lblLPath.GetSelectedDir <> '' then
  begin
    // User clicked on a subdirectory of the path.
    pnlFile.ActiveDir := lblLPath.SelectedDir;
  end
  else
    Actions.cm_DirHistory('');
end;

procedure TFrameFilePanel.pnlHeaderResize(Sender: TObject);
begin
  lblLPath.Width:=pnlHeader.Width - 4;
end;

procedure TFrameFilePanel.SetGridHorzLine(const AValue: Boolean);
begin
  if AValue then
    dgPanel.Options := dgPanel.Options + [goHorzLine]
  else
    dgPanel.Options := dgPanel.Options - [goHorzLine];
end;

procedure TFrameFilePanel.SetGridVertLine(const AValue: Boolean);
begin
  if AValue then
    dgPanel.Options := dgPanel.Options + [goVertLine]
  else
    dgPanel.Options := dgPanel.Options - [goVertLine]
end;

constructor TFrameFilePanel.Create(AOwner : TWinControl; lblDriveInfo : TLabel;
                                   lblCommandPath:TLabel; cmbCommand:TComboBox);
begin
  DebugLn('TFrameFilePanel.Create components');
  inherited Create(AOwner);
  Parent:=AOwner;
  Align:=alClient;
  ActiveColmSlave:=nil;
  isSlave:=false;
  FLastSelectionStartRow:=-1;
  FLastMark:= '*';
  FLastAutoSelect:= False;

  dgPanel:=TDrawGridEx.Create(Self, Self);

  pnlHeader:=TPanel.Create(Self);
  pnlHeader.Parent:=Self;
  pnlHeader.Height:=24;
  pnlHeader.Align:=alTop;

  pnlHeader.BevelInner:=bvNone;
  pnlHeader.BevelOuter:=bvNone;

  lblLPath:=TPathLabel.Create(pnlHeader);
  lblLPath.Parent:=pnlHeader;
  lblLPath.Top := 2;
  lblLPath.AutoSize:=False;
  lblLPath.Width:=pnlHeader.Width - 4;

  edtPath:=TEdit.Create(lblLPath);
  edtPath.Parent:=pnlHeader;
  edtPath.Visible:=False;
  edtPath.TabStop:=False;

  pnlFooter:=TPanel.Create(Self);
  pnlFooter.Parent:=Self;
  pnlFooter.Align:=alBottom;

  pnlFooter.Width:=AOwner.Width;
  pnlFooter.Anchors:=[akLeft, akRight, akBottom];
  pnlFooter.Height:=20;
  pnlFooter.Top:=Height-20;

  pnlFooter.BevelInner:=bvNone;
  pnlFooter.BevelOuter:=bvNone;

  lblLInfo:=TLabel.Create(pnlFooter);
  lblLInfo.Parent:=pnlFooter;
  lblLInfo.Width:=250;//  pnlFooter.Width;
  lblLInfo.AutoSize:=True;

  edtRename:=TEdit.Create(dgPanel);
  edtRename.Parent:=dgPanel;
  edtRename.Visible:=False;
  edtRename.TabStop:=False;

  // now create search panel
  pnAltSearch:=TPanel.Create(Self);
  pnAltSearch.Parent:=Self;
  pnAltSearch.Caption:=rsQuickSearchPanel;
  pnAltSearch.Alignment:=taLeftJustify;
  
  edtSearch:=TEdit.Create(pnAltSearch);
  edtSearch.Parent:=pnAltSearch;
  edtSearch.TabStop:=False;
  edtSearch.Left:=64;
  edtSearch.Top:=1;

  pnAltSearch.Visible := False;

  // ---
  OnKeyPress:=@dgPanelKeyPress;
  dgPanel.OnMouseDown := @dgPanelMouseDown;
  dgPanel.OnStartDrag := @dgPanelStartDrag;
  dgPanel.OnDragOver := @dgPanelDragOver;
  dgPanel.OnDragDrop:= @dgPanelDragDrop;
  dgPanel.OnEndDrag:= @dgPanelEndDrag;
  dgPanel.OnDblClick:=@dgPanelDblClick;
  dgPanel.OnDrawCell:=@dgPanelDrawCell;
  dgPanel.OnEnter:=@dgPanelEnter;
  dgPanel.OnExit:=@dgPanelExit;
  dgPanel.OnKeyUp:=@dgPanelKeyUp;
  dgPanel.OnKeyDown:=@dgPanelKeyDown;
  dgPanel.OnKeyPress:=@dgPanelKeyPress;
  dgPanel.OnHeaderClick:=@dgPanelHeaderClick;
  dgPanel.OnPrepareCanvas:=@dgPanelPrepareCanvas;
  {Alexx2000}
  dgPanel.OnMouseWheelUp := @dgPanelMouseWheelUp;
  dgPanel.OnMouseWheelDown := @dgPanelMouseWheelDown;
  {/Alexx2000}
  edtSearch.OnChange:=@edSearchChange;
  edtSearch.OnKeyPress:=@edSearchKeyPress;
  edtSearch.OnKeyDown:=@edSearchKeyDown;
  edtPath.OnKeyPress:=@edtPathKeyPress;
  edtRename.OnKeyPress:=@edtRenameKeyPress;

  pnlHeader.OnResize := @pnlHeaderResize;

  lblLPath.OnClick := @lblLPathClick;

  pnlFile:=TFilePanel.Create(AOwner, Self, TDrawGrid(dgPanel),
                             lblLPath, lblCommandPath, lblDriveInfo, cmbCommand);
  
  edtCmdLine := cmbCommand;
  ClearCmdLine;

  UpdateView;

//  setup column widths
  SetColWidths;
  UpdateColumnsView;
end;

destructor TFrameFilePanel.Destroy;
begin
  if assigned(pnlFile) then
    FreeAndNil(pnlFile);
  inherited Destroy;
end;

procedure TFrameFilePanel.UpdateView;
begin
  pnlHeader.Visible := gCurDir;  // Current directory
  pnlFooter.Visible := gStatusBar;  // Status bar
  GridVertLine:= gGridVertLine;
  GridHorzLine:= gGridHorzLine;

  dgPanel.UpdateView;

  with FLastSelect do
  begin
    Left:= 0;
    Top:= 0;
    Bottom:= 0;
    Right:= dgPanel.ColCount-1;
  end;

  if gShowIcons then
    pnlFile.FileList.UpdateFileInformation(pnlFile.PanelMode);

  pnlFile.UpdatePanel;
  UpDatelblInfo;
end;

function TFrameFilePanel.GetColumnsClass: TPanelColumnsClass;
begin
  if isSlave then
    Result := ActiveColmSlave
  else
    Result := ColSet.GetColumnSet(ActiveColm);
end;


{ TDrawGridEx }

constructor TDrawGridEx.Create(AOwner: TComponent; AParent: TWinControl);
begin
  // Initialize D&D before calling inherited create,
  // because it will create the control and call InitializeWnd.
  DragDropSource := nil;
  DragDropTarget := nil;
  TransformDragging := False;

{$IFDEF LCLGTK2}
  FLastDoubleClickTime := Now;
{$ENDIF}

  inherited Create(AOwner);

  Self.Parent := AParent;

  StartDrag := False;
  DropRowIndex := -1;

  DoubleBuffered := True;
  AutoFillColumns := True;
  Align := alClient;
  ScrollBars := ssAutoVertical;
  Options := [goFixedVertLine, goFixedHorzLine, goTabs, goRowSelect,
              goColSizing, goThumbTracking];
  TitleStyle := tsStandard;
  TabStop := False;

  UpdateView;
end;

destructor TDrawGridEx.Destroy;
begin
  inherited;
end;

procedure TDrawGridEx.UpdateView;
var
  TabHeaderHeight: Integer;
begin
  Flat := gInterfaceFlat;
  Color := gBackColor;

  // Set height of each row.
  DefaultRowHeight := gIconsSize;

  // Set rows of header.
  RowCount := Integer(gTabHeader);
  if gTabHeader then
  begin
    TabHeaderHeight := gIconsSize + 1;
    if not gInterfaceFlat then
    begin
      TabHeaderHeight := TabHeaderHeight + 2;
    end;
    RowHeights[0] := TabHeaderHeight;
  end;

  FixedRows := Integer(gTabHeader);
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

procedure TDrawGridEx.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Point: TPoint;
  frp: PFileRecItem;
  ExpectedButton: TShiftStateEnum;
begin
  inherited MouseMove(Shift, X, Y);

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
        frp := (Parent as TFrameFilePanel).pnlFile.GetReferenceItemPtr(DragRowIndex - FixedRows); // substract fixed rows (header)
        // Check if valid item is being dragged.
        if (Parent as TFrameFilePanel).pnlFile.IsItemValid(frp) then
        begin
          BeginDrag(False);
        end;
      end;
    end;
end;

procedure TDrawGridEx.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  WasDragging: Boolean;
begin
{$IFDEF LCLGTK2}
  // Workaround for two doubleclicks being sent on GTK.
  // MouseUp event is sent just after doubleclick, so if we drop
  // doubleclick events we have to also drop MouseUp events that follow them.
  if TooManyDoubleClicks then Exit;
{$ENDIF}

  StartDrag := False;

  WasDragging := Self.Dragging;

  inherited MouseUp(Button, Shift, X, Y);  // will stop any dragging

  // Call handler only if button-up was not lifted to finish drag&drop operation.
  if (WasDragging = False) then
    frmMain.framedgPanelMouseUp(Self, Button, Shift, X, Y);
end;

procedure TDrawGridEx.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
{$IFDEF LCLGTK2}
  // Workaround for two doubleclicks being sent on GTK.
  // MouseDown event is sent just before doubleclick, so if we drop
  // doubleclick events we have to also drop MouseDown events that precede them.
  if TooManyDoubleClicks then Exit;
{$ENDIF}

  inherited;
end;

function TDrawGridEx.GetHeaderHeight: Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := 0 to FixedRows-1 do
    Result := Result + RowHeights[i];
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
  SourcePanel: TFrameFilePanel;
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

  SourcePanel := (Parent as TFrameFilePanel);

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
  fri: PFileRecItem;
  TargetPanel: TFrameFilePanel = nil;
begin
  Result := False;

  ClientPoint := Self.ScreenToClient(ScreenPoint);

  TargetPanel := (Self.Parent as TFrameFilePanel);

  // Allow dropping into empty panel or on the header.
  if TargetPanel.IsEmpty or (ClientPoint.Y < GetHeaderHeight) then
  begin
    ChangeDropRowIndex(-1);
    Result := True;
    Exit;
  end;

  MouseToCell(ClientPoint.X, ClientPoint.Y, Dummy, iRow);

  // Get the item over which there is something dragged.
  fri := TargetPanel.pnlFile.GetReferenceItemPtr(iRow - FixedRows); // substract fixed rows (header)

  if Assigned(fri) and (FPS_ISDIR(fri^.iMode) or fri^.bLinkIsDir) and (ClientPoint.Y < GridHeight) then
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
  FileList: TFileList;
begin
  if FileNamesList.Count > 0 then
  begin
    FileList := TFileList.Create;
    FileList.LoadFromFileNames(FileNamesList);
    DropFiles(TDropParams.Create(
      FileList, DropEffect, ScreenPoint, True,
      nil, Self.Parent as TFrameFilePanel));
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

procedure TDrawGridEx.DropFiles(DropParams: TDropParams);
begin
  if Assigned(DropParams) then
  begin
    if DropParams.FileList.Count > 0 then
    begin
      case DropParams.DropEffect of

        DropMoveEffect:
          DoDragDropOperation(ddoMove, DropParams);

        DropCopyEffect:
          DoDragDropOperation(ddoCopy, DropParams);

        DropLinkEffect:
          DoDragDropOperation(ddoSymLink, DropParams);

        DropAskEffect:
          begin
            // Ask the user what he would like to do by displaying a menu.
            // Returns immediately after showing menu.
            frmMain.pmDropMenu.PopUp(DropParams);
          end;

        else
          FreeAndNil(DropParams);

      end;
    end
    else
      FreeAndNil(DropParams);
  end;
end;

procedure TDrawGridEx.DoDragDropOperation(Operation: TDragDropOperation;
                                          DropParams: TDropParams);
var
  pfr: PFileRecItem;
  TargetDir: string;
  iCol, iRow: Integer;
  ClientDropPoint: TPoint;
  SourceFileName, TargetFileName: string;
begin
  with DropParams do
  begin
    if FileList.Count > 0 then
    begin
      ClientDropPoint := TargetPanel.dgPanel.ScreenToClient(ScreenDropPoint);
      TargetPanel.dgPanel.MouseToCell(ClientDropPoint.X, ClientDropPoint.Y, iCol, iRow);

      // default to current active directory in the destination panel
      TargetDir := TargetPanel.ActiveDir;

      if (DropIntoDirectories = True) and
         (iRow >= Self.FixedRows) and
         (ClientDropPoint.Y < TargetPanel.dgPanel.GridHeight) then
      begin
        pfr := TargetPanel.pnlFile.GetReferenceItemPtr(iRow - FixedRows);

        // If dropped into a directory modify destination path accordingly.
        if Assigned(pfr) and (FPS_ISDIR(pfr^.iMode) or (pfr^.bLinkIsDir)) then
        begin
          if pfr^.sName = '..' then
            // remove the last subdirectory in the path
            TargetDir := GetParentDir(TargetDir)
          else
            TargetDir := TargetDir + pfr^.sName + DirectorySeparator;
        end;
      end;

      case Operation of

        ddoMove:
          if GetDragDropType = ddtInternal then
            frmMain.RenameFile(TargetDir)
          else
          begin
            frmMain.RenameFile(FileList, TargetPanel, TargetDir); // will free FileList
            FileList := nil;
          end;

        ddoCopy:
          if GetDragDropType = ddtInternal then
            frmMain.CopyFile(TargetDir)
          else
          begin
            frmMain.CopyFile(FileList, TargetPanel, TargetDir);   // will free FileList
            FileList := nil;
          end;

        ddoSymLink, ddoHardLink:
          begin
            if ((GetDragDropType = ddtInternal) and
               (SourcePanel.pnlFile.PanelMode in [pmArchive, pmVFS]))
            or (TargetPanel.pnlFile.PanelMode in [pmArchive, pmVFS]) then
            begin
              msgWarning(rsMsgErrNotSupported);
            end
            else
            begin
              // TODO: process multiple files

              SourceFileName := FileList.GetFileName(0);
              TargetFileName := TargetDir + ExtractFileName(SourceFileName);

              if ((Operation = ddoSymLink) and
                 ShowSymLinkForm(SourceFileName, TargetFileName))
              or ((Operation = ddoHardLink) and
                 ShowHardLinkForm(SourceFileName, TargetFileName))
              then
                TargetPanel.RefreshPanel;
            end;
          end;
      end;
    end;
  end;

  FreeAndNil(DropParams);
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

{ TDropParams }

constructor TDropParams.Create(
                  aFileList: TFileList; aDropEffect: TDropEffect;
                  aScreenDropPoint: TPoint; aDropIntoDirectories: Boolean;
                  aSourcePanel: TFrameFilePanel;
                  aTargetPanel: TFrameFilePanel);
begin
  FileList := aFileList;
  DropEffect := aDropEffect;
  ScreenDropPoint := aScreenDropPoint;
  DropIntoDirectories := aDropIntoDirectories;
  SourcePanel := aSourcePanel;
  TargetPanel := aTargetPanel;
end;

destructor TDropParams.Destroy;
begin
  if Assigned(FileList) then
    FreeAndNil(FileList);
end;

function TDropParams.GetDragDropType: TDragDropType;
begin
  if Assigned(SourcePanel) then
    Result := ddtInternal
  else
    Result := ddtExternal;
end;

{ TPathLabel }

constructor TPathLabel.Create(AOwner: TComponent);
begin
  LeftSpacing := 3; // set before painting

  inherited;

  SelectedDir := '';

  HighlightStartPos := -1;
  HighlightText := '';

  SetActive(False);

  OnMouseEnter:=@MouseEnter;
  OnMouseMove :=@MouseMove;
  OnMouseLeave:=@MouseLeave;
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

procedure TPathLabel.MouseEnter(Sender: TObject);
begin
  Cursor := crDefault;
end;

procedure TPathLabel.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Highlight(X, Y);
end;

procedure TPathLabel.MouseLeave(Sender: TObject);
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

end.

