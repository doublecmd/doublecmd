unit uColumnsFileView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  Graphics, Controls, Forms, LMessages, LCLIntf,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Grids,
  Buttons, LCLType, Menus,
  uDragDropEx,

  uFile,
  uFileView,
  uFileSource,
  uColumnsFileViewFiles,
  uColumns,
  uFileSorting
  ;

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
    procedure AddSorting(iField : Integer; SortDirection : TSortDirection);
    procedure Clear; override;
    function GetSortingDirection(iField : Integer) : TSortDirection;
  end;

  PFileListSorting = ^TFileListSorting;

  // DragDrop

  TDragDropType = (ddtInternal, ddtExternal);

  // Lists all operations supported by dragging and dropping items
  // in the panel (external, internal and via menu).
  TDragDropOperation = (ddoCopy, ddoMove, ddoSymLink, ddoHardLink);

  TColumnsFileView = class;
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

    {  This function is called from various points to handle dropping files
       into the panel. It converts drop effects available on the system
       into TDragDropOperation operations.
       Handles freeing DropParams. }
    procedure DropFiles(DropParams: TDropParams);
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
    Files: TFiles;
    DropEffect: TDropEffect;
    ScreenDropPoint: TPoint;
    DropIntoDirectories: Boolean;
    SourcePanel: TColumnsFileView;
    TargetPanel: TColumnsFileView;

    constructor Create(aFiles: TFiles; aDropEffect: TDropEffect;
                       aScreenDropPoint: TPoint; aDropIntoDirectories: Boolean;
                       aSourcePanel: TColumnsFileView;
                       aTargetPanel: TColumnsFileView);
    destructor Destroy; override;

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

    procedure MouseEnterEvent(Sender: TObject);
    procedure MouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseLeaveEvent(Sender: TObject);

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

  { TColumnsFileView }

  TColumnsFileView = class(TFileView)

  private
    FFiles: TColumnsViewFiles;  //<en List of displayed files
    FFileSourceFiles: TFiles;   //<en List of files from file source

    FActive: Boolean;           //<en Is this view active
    FLastActive: String;        //<en Last active file
    FLastMark: String;
    FLastAutoSelect: Boolean;
    FLastSelectionStartRow: Integer;
    fSearchDirect,
    fNext,
    fPrevious : Boolean;

    fUpdateFileCount,
    fUpdateDiskFreeSpace: Boolean;

    FSorting: TFileListSorting;
    FSortCol:Integer;
    fSortDirect:TSortDirection;

    pnlFooter: TPanel;
    lblInfo: TLabel;
    pnlHeader: TPanel;
    pmColumnsMenu: TPopupMenu;
    pnAltSearch: TPanel;
    edtSearch: TEdit;
    edtPath: TEdit;
    edtRename: TEdit;
    lblPath: TPathLabel;
    dgPanel: TDrawGridEx;

    function GetGridHorzLine: Boolean;
    function GetGridVertLine: Boolean;
    procedure SetGridHorzLine(const AValue: Boolean);
    procedure SetGridVertLine(const AValue: Boolean);
    function StartDragEx(MouseButton: TMouseButton; ScreenStartPoint: TPoint): Boolean;
    procedure UpdateColCount(NewColCount: Integer);
    procedure ChooseFile(AFile: TColumnsViewFile; FolderMode: Boolean = False);
    procedure SortByColumn(iColumn: Integer);
    procedure UpdatePathLabel;
    procedure UpdateCountStatus;

    procedure SelectRange(iRow: PtrInt);
    procedure MarkFile(AFile: TColumnsViewFile; bMarked: Boolean);
    procedure MarkAllFiles(bMarked: Boolean);
    procedure InvertFileSelection(AFile: TColumnsViewFile);
    procedure MarkGroup(const sMask: String; bSelect: Boolean);

    {en
       Sorts files in file source file list (FFileSourceFiles).
       Maybe change to sort display file list instead?
    }
    procedure Sort;

    procedure ShowRenameFileEdit(const sFileName:String);
    procedure ShowPathEdit;
    procedure ShowAltPanel(Char : TUTF8Char = #0);
    procedure CloseAltPanel;

    function GetActiveItem: TColumnsViewFile;

    // -- Events --------------------------------------------------------------

    procedure edtPathExit(Sender: TObject);
    procedure edtSearchExit(Sender: TObject);
    procedure edtRenameExit(Sender: TObject);

    procedure edtSearchChange(Sender: TObject);
    procedure edtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtPathKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtRenameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    procedure dgPanelEnter(Sender: TObject);
    procedure dgPanelDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure dgPanelExit(Sender: TObject);
    procedure dgPanelDblClick(Sender: TObject);
    procedure dgPanelKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dgPanelKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dgPanelMouseDown(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
    procedure dgPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgPanelStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure dgPanelDragOver(Sender, Source: TObject; X, Y: Integer;
                                               State: TDragState; var Accept: Boolean);
    procedure dgPanelDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure dgPanelEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure dgPanelHeaderClick(Sender: TObject;IsColumn: Boolean; index: Integer);
    procedure dgPanelPrepareCanvas(sender: TObject; Col, Row: Integer; aState: TGridDrawState);
    procedure dgPanelMouseWheelUp(Sender: TObject; Shift: TShiftState;
                                  MousePos: TPoint; var Handled: Boolean);
    procedure dgPanelMouseWheelDown(Sender: TObject; Shift: TShiftState;
                                  MousePos: TPoint; var Handled: Boolean);
    procedure lblPathClick(Sender: TObject);
    procedure lblPathMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlHeaderResize(Sender: TObject);
    procedure ColumnsMenuClick(Sender: TObject);

    procedure UTF8KeyPressEvent(Sender: TObject; var UTF8Key: TUTF8Char);

  protected
    procedure SetCurrentPath(NewPath: String); override;
    function GetActiveFile: TFile; override;

  public
    ActiveColm: String;
    ActiveColmSlave: TPanelColumnsClass;
    isSlave:boolean;
//---------------------

    constructor Create(AOwner: TWinControl; AFileSource: TFileSource); override;

    destructor Destroy; override;

    {en
       Retrieves file list from file source into FFileSourceFiles.
    }
    procedure MakeFileSourceFileList;
    {en
       Fills FFiles with files from FFileSourceFiles
       (filtered and sorted already?).
    }
    procedure MakeDisplayFileList;

    procedure Refresh;

    procedure Reload; override;

    function Focused: Boolean; override;
    procedure SetFocus; override;

    procedure LoadConfiguration(Section: String; TabIndex: Integer); override;
    procedure SaveConfiguration(Section: String; TabIndex: Integer); override;

    procedure cdUpLevel;
    procedure cdDownLevel(AFile: TFile);

    { Returns True if at least one file is/was selected. }
    function  SelectFileIfNoSelected(AFile: TColumnsViewFile):Boolean;
    procedure UnSelectFileIfSelected(AFile: TColumnsViewFile);

    procedure SelectFile(AFile: TColumnsViewFile);
    procedure MakeVisible(iRow:Integer);
    procedure MakeSelectedVisible;
    {en
       Moves the selection focus to the file specified by FileName.
    }
    procedure Select(const FileName: String);

    procedure InvertAll;
    procedure MarkAll;
    procedure UnMarkAll;
    procedure MarkMinus;
    procedure MarkPlus;
    procedure MarkShiftPlus;
    procedure MarkShiftMinus;

    procedure SetColWidths;

    procedure RedrawGrid;
    procedure RefreshPanel(bUpdateFileCount: Boolean = True; bUpdateDiskFreeSpace: Boolean = True);
    procedure RefreshCount(bUpdateFileCount: Boolean = True;
                           bUpdateDiskFreeSpace: Boolean = True);

    procedure UpDatelblInfo;
    procedure UpdateColumnsView;
    procedure UpdateView; override;

    {en
       Returns True if there are no files shown in the panel.
    }
    function IsEmpty: Boolean;
    {en
       Returns True if item is not nil and not '..'.
       May be extended to include other conditions.
    }
    function IsItemValid(AFile: TColumnsViewFile): Boolean;
    function IsActiveItemValid: Boolean;

    function GetColumnsClass: TPanelColumnsClass;

    {  Executes operations with dropped files, can handle any TDragDropOperation.
       Handles freeing DropParams. }
    procedure DoDragDropOperation(Operation: TDragDropOperation;
                                  DropParams: TDropParams);

    procedure CalculateSpace(bDisplayMessage:Boolean);

    property CurrentPath: String read GetCurrentPath write SetCurrentPath;
    property LastActive: String read FLastActive write FLastActive;
    property GridVertLine: Boolean read GetGridVertLine write SetGridVertLine;
    property GridHorzLine: Boolean read GetGridHorzLine write SetGridHorzLine;

  published  // commands
    procedure cm_MarkInvert(param: string='');
    procedure cm_QuickSearch(param: string='');

  end;

implementation

uses
  LCLProc, Masks, uLng, uShowMsg, uGlobs, GraphType, uPixmapManager,
  uDCUtils, uOSUtils, math, fMain, fSymLink, fHardLink,
  uFileSourceListOperation,
  uFileProperty, uDefaultFilePropertyFormatter,
  uFileSourceProperty,
  uFileSourceOperationTypes,
  uFileSystemFile,
  fColumnsSetConf,
  uKeyboard
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  , GtkProc  // for ReleaseMouseCapture
  , GTKGlobals  // for DblClickTime
{$ENDIF}
  ;

function TColumnsFileView.Focused: Boolean;
begin
  Result := dgPanel.Focused;
end;

procedure TColumnsFileView.SetFocus;
begin
  if frmMain.Visible and dgPanel.CanFocus then
    dgPanel.SetFocus;
  lblPath.SetActive(True);
  if Parent is TPage then
    frmMain.UpdateSelectedDrive(Parent.Parent as TNoteBook);

  // Create a Panel-Changed-Event for this?
  frmMain.UpdatePrompt;
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

  SetColWidths;
  UpdateColumnsView;
  RefreshPanel;
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

procedure TColumnsFileView.cdUpLevel;
var
  PreviousSubDirectory,
  sUpLevel: String;
begin
  PreviousSubDirectory := ExtractFileName(ExcludeTrailingPathDelimiter(CurrentPath));

  sUpLevel:= GetParentDir(CurrentPath);
  if sUpLevel = EmptyStr then Exit;
  CurrentPath := sUpLevel;

  Select(PreviousSubDirectory);
end;

procedure TColumnsFileView.cdDownLevel(AFile: TFile);
begin
  CurrentPath := CurrentPath + AFile.Name + DirectorySeparator;
end;

procedure TColumnsFileView.SelectFile(AFile: TColumnsViewFile);
begin
  InvertFileSelection(AFile);
  UpDatelblInfo;
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

function TColumnsFileView.SelectFileIfNoSelected(AFile: TColumnsViewFile):Boolean;
var
  i: Integer;
begin
  FLastAutoSelect := False;
  for i := 0 to FFiles.Count-1 do
  begin
    if FFiles[i].Selected then
    begin
      Result := True;
      Exit;
    end;
  end;

  if IsItemValid(AFile) then
  begin
    InvertFileSelection(AFile);
    UpDatelblInfo;
    FLastAutoSelect:= True;
    Result := True;
  end
  else
    Result := False;
end;

procedure TColumnsFileView.UnSelectFileIfSelected(AFile: TColumnsViewFile);
begin
  if FLastAutoSelect and Assigned(AFile) and (AFile.Selected) then
    begin
      InvertFileSelection(AFile);
      UpDatelblInfo;
    end;
  FLastAutoSelect:= False;
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

  UpDatelblInfo;
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.RefreshPanel(bUpdateFileCount: Boolean = True; bUpdateDiskFreeSpace: Boolean = True);
var
  LastSelection: String;
begin
  if pnAltSearch.Visible then
    CloseAltPanel;

  if Assigned(GetActiveItem) then
    LastSelection := GetActiveItem.TheFile.Name
  else
    LastSelection := '';

  RefreshCount(bUpdateFileCount, bUpdateDiskFreeSpace);
  Select(LastSelection);

  UpDatelblInfo;
end;

procedure TColumnsFileView.RefreshCount(bUpdateFileCount: Boolean = True;
                                        bUpdateDiskFreeSpace: Boolean = True);
begin
  // set up refresh parameters
  FUpdateFileCount:= bUpdateFileCount;
  FUpdateDiskFreeSpace:= bUpdateDiskFreeSpace;

  Reload;

  // restore default value
  FUpdateFileCount:= True;
  FUpdateDiskFreeSpace:= True;
end;

function TColumnsFileView.StartDragEx(MouseButton: TMouseButton; ScreenStartPoint: TPoint): Boolean;
var
  fileNamesList: TStringList;
  draggedFileItem, AFile: TColumnsViewFile;
  i: Integer;
begin
  Result := False;

  if dgPanel.DragRowIndex >= dgPanel.FixedRows then
  begin
    draggedFileItem := FFiles[dgPanel.DragRowIndex - dgPanel.FixedRows]; // substract fixed rows (header)

    fileNamesList := TStringList.Create;
    try
      if SelectFileIfNoSelected(draggedFileItem) = True then
      begin
        for i := 0 to FFiles.Count-1 do
        begin
          if FFiles[i].Selected then
            fileNamesList.Add(CurrentPath + FFiles[i].TheFile.Name);
        end;

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
    if not Assigned(AFile) then Continue;
    MarkFile(AFile, True);
  end;
  UpDatelblInfo;
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
  case Button of
    mbRight: begin
      dgPanel.Row := iRow;

      if (gMouseSelectionEnabled) and (gMouseSelectionButton = 1) then
      begin
        AFile := FFiles[iRow - dgPanel.FixedRows]; // substract fixed rows (header)

        if Assigned(AFile) then
        begin
          InvertFileSelection(AFile);
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
            AFile := FFiles[iRow - dgPanel.FixedRows]; // substract fixed rows (header)
            if Assigned(AFile) then
              begin
                InvertFileSelection(AFile);
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
          Actions.cm_ContextMenu('OnMouseClick');
          //actContextMenu.Execute;
        end;
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
    SourcePanel := ((Source as TDrawGridEx).Parent) as TColumnsFileView;
    TargetPanel := ((Sender as TDrawGridEx).Parent) as TColumnsFileView;

    SourceDir := SourcePanel.CurrentPath;
    TargetDir := TargetPanel.CurrentPath;
  end;

  dgPanel.MouseToCell(X, Y, Dummy, iRow);

  if iRow >= dgPanel.FixedRows then
    AFile := FFiles[iRow - dgPanel.FixedRows]; // substract fixed rows (header)

  if Assigned(AFile) and AFile.TheFile.IsDirectory and (Y < dgPanel.GridHeight) then
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
  Files: TFiles;
begin
  if (Sender is TDrawGridEx) and (Source is TDrawGridEx) then
  begin
    SourcePanel := ((Source as TDrawGridEx).Parent) as TColumnsFileView;

    // Get file names from source panel.
    with SourcePanel do
    begin
      if SelectFileIfNoSelected(GetActiveItem) = False then Exit;

      Files := TFiles.Create;
      try
{
        CopyListSelectedExpandNames(pnlFile.FileList, FileList, ActiveDir);
}
        UnSelectFileIfSelected(GetActiveItem);
      except
        FreeAndNil(Files);
        UnSelectFileIfSelected(GetActiveItem);
        Exit;
      end;
    end;

    // Drop onto target panel.
    with Sender as TDrawGridEx do
    begin
      DropFiles(TDropParams.Create(
        Files, // Will be freed automatically.
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

  MakeDisplayFileList;
  RedrawGrid;
end;

procedure TColumnsFileView.dgPanelPrepareCanvas(sender: TObject; Col,
  Row: Integer; aState: TGridDrawState);
begin
  if (gdFixed in aState) and gTabHeader then
    Exit;

  with dgPanel do
  begin
    if Color <> gBackColor then
      Color:= gBackColor;
  end;
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
        CloseAltPanel;
        SetFocus;
        Key := 0;
      end;

    VK_ESCAPE:
      begin
        Key := 0;
        CloseAltPanel;
        SetFocus;
      end;

    VK_RETURN,
    VK_SELECT:
      begin
        Key := 0;
        CloseAltPanel;
        SetFocus;

        {LaBero begin}
        {en
            Execute/open selected file/directory
            if the user press ENTER during QuickSearch
        }
        try
           ChooseFile(GetActiveItem);
           UpDatelblInfo;
        finally
           dgPanel.Invalidate;
           Screen.Cursor:=crDefault;
        end;
        {LaBero end}
      end;
  end;
end;

procedure TColumnsFileView.SetCurrentPath(NewPath: String);
begin
  if NewPath <> '' then
  begin
{
    if Assigned(FOnBeforeChangeDirectory) then
      if not FOnBeforeChangeDirectory(fOwner, NewDirectory) then
        Exit;
}
{
    if not FileSource.ChangePath(NewPath) then
      begin
        msgError(Format(rsMsgChDirFailed, [NewPath]));
        Exit;   // chdir failed
      end;

    AddDirToHistory(fActiveDir);
}
    FileSource.CurrentPath := NewPath; // TODO: error handling (exception?)

    LastActive := '';
    dgPanel.Row := 0;

    {$IF NOT DEFINED(DARWIN)}
    if gTermWindow and Assigned(Cons) then
      Cons.Terminal.Write_pty('cd "' + NewPath + '"'+#13#10);
    {$ENDIF}

    MakeFileSourceFileList;

    UpdatePathLabel;
{
    if Assigned(FOnAfterChangeDirectory) then
      FOnAfterChangeDirectory(fOwner, fActiveDir);
}
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
    if AFile.TheFile.Name = '..' then
    begin
      cdUpLevel;
      Exit;
    end;

{
    if (fPanelMode=pmVFS) or ((sModeStr = 'wfx') and fVFS.FindModule(sPath + sName)) then
    begin
      LastActive:= '';
      LoadPanelVFS(pfri);
      fPanel.Row := 0;
      Exit;
    end;
    if (fPanelMode=pmArchive) or (not FPS_ISDIR(iMode) and fVFS.FindModule(sPath + sName)) then
    begin
      LastActive:= '';
      LoadPanelVFS(pfri);
      fPanel.Row := 0;
      Exit;
    end;
}
    if TheFile.IsDirectory then // deeper and deeper
    begin
      cdDownLevel(TheFile);
      Exit;
    end;

    if FolderMode then exit;
{
    //now test if exists Open command in doublecmd.ext :)
    sOpenCmd:= gExts.GetExtActionCmd(pfri^, 'open');
    if (sOpenCmd<>'') then
    begin
      if Pos('{!VFS}',sOpenCmd)>0 then
      begin
        if fVFS.FindModule(sName) then
        begin
          LoadPanelVFS(pfri);
          Exit;
        end;
      end;
      LastActive:=sName;

      ReplaceExtCommand(sOpenCmd, pfri, ActiveDir);
      if ProcessExtCommand(sOpenCmd, ActiveDir) then
        Exit;
    end;
    // and at the end try to open by system
    mbSetCurrentDir(ActiveDir);
    LastActive:= sName;
    ShellExecute(sName);
    LoadPanel;
}
  end;
end;

procedure TColumnsFileView.ShowRenameFileEdit(const sFileName:String);
begin
  frmMain.EnableHotkeys(False);

  edtRename.Width := dgPanel.ColWidths[0]+dgPanel.ColWidths[1]-16;
  edtRename.Top := (dgPanel.CellRect(0,dgPanel.Row).Top-2);
  if gShowIcons then
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

procedure TColumnsFileView.UpdatePathLabel;
begin
  lblPath.Caption := MinimizeFilePath(CurrentPath, lblPath.Canvas, lblPath.Width);
end;

procedure TColumnsFileView.UpdateCountStatus;
var
  i: Integer;
  FilesInDir, FilesSelected: Integer;
  SizeInDir, SizeSelected: Int64;
  SizeProperty: TFileSizeProperty;
  SizeSupported: Boolean;
begin
  if not fUpdateFileCount then Exit;

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
end;

procedure TColumnsFileView.SortByColumn(iColumn: Integer);
var
  ColumnsClass: TPanelColumnsClass;
begin
  ColumnsClass := GetColumnsClass;

  if (iColumn >= 0) and (iColumn < ColumnsClass.ColumnsCount) then
  begin
    FSorting.Clear;
    FSorting.AddSorting(iColumn, fSortDirect);
    FSortCol := iColumn;
    MakeDisplayFileList; // sorted here
    RedrawGrid;
  end;
end;

{
Sort files by multicolumn sorting.
}
procedure TColumnsFileView.Sort;
var
  ColumnsClass: TPanelColumnsClass;
  i : Integer;
  pSortingColumn : PFileListSortingColumn;
  Column: TPanelColumn;
  bSortedByName: Boolean;
  bSortedByExtension: Boolean;
  FileSortings: TFileSortings;
  FileListSorter: TListSorter = nil;
  TempSorting: TFileListSorting;
begin
  ColumnsClass := GetColumnsClass;

  if (FFileSourceFiles.Count = 0) or (ColumnsClass.ColumnsCount = 0) then Exit;

  TempSorting := TFileListSorting.Create;

  for i := 0 to FSorting.Count - 1 do
  begin
    pSortingColumn := PFileListSortingColumn(FSorting[i]);
    TempSorting.AddSorting(pSortingColumn^.iField, pSortingColumn^.SortDirection);
  end;

  bSortedByName := False;
  bSortedByExtension := False;

  SetLength(FileSortings, TempSorting.Count);

  for i := 0 to TempSorting.Count - 1 do
  begin
    pSortingColumn := PFileListSortingColumn(TempSorting[i]);

    if (pSortingColumn^.iField >= 0) and
       (pSortingColumn^.iField < ColumnsClass.ColumnsCount) then
    begin
      Column := ColumnsClass.GetColumnItem(pSortingColumn^.iField);
      FileSortings[i].SortFunctions := Column.GetColumnFunctions;
      FileSortings[i].SortDirection := pSortingColumn^.SortDirection;

      if HasSortFunction(FileSortings[i].SortFunctions, fsfName) then
      begin
        bSortedByName := True;
        bSortedByExtension := True;
      end
      else if HasSortFunction(FileSortings[i].SortFunctions, fsfNameNoExtension)
      then
      begin
        bSortedByName := True;
      end
      else if HasSortFunction(FileSortings[i].SortFunctions, fsfExtension)
      then
      begin
        bSortedByExtension := True;
      end;
    end
    else
      Raise Exception.Create('Invalid column number in sorting - fix me');
  end;

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

  // Sort.
  FileListSorter := TListSorter.Create(FFileSourceFiles.List, FileSortings);
  try
    FileListSorter.Sort;
  finally
    FreeAndNil(FileListSorter);
  end;

  FreeAndNil(TempSorting);
end;

procedure TColumnsFileView.UpdateColCount(NewColCount: Integer);
begin
  while dgPanel.Columns.Count < NewColCount do
    dgPanel.Columns.Add;
  while dgPanel.Columns.Count > NewColCount do
    dgPanel.Columns.Delete(0);
end;

procedure TColumnsFileView.SetColWidths;
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

procedure TColumnsFileView.edtPathExit(Sender: TObject);
begin
  edtPath.Visible := False;
end;

procedure TColumnsFileView.edtSearchExit(Sender: TObject);
begin
  // sometimes must be search panel closed this way
  CloseAltPanel;
  RedrawGrid;
end;

procedure TColumnsFileView.edtRenameExit(Sender: TObject);
begin
  edtRename.Visible := False;
  UnMarkAll;
end;

procedure TColumnsFileView.edtSearchChange(Sender: TObject);
var
  I, iPos, iEnd : Integer;
  Result : Boolean;
  sSearchName,
  sSearchNameNoExt,
  sSearchExt : String;
begin
  if edtSearch.Text='' then Exit;
  //DebugLn('edtSearchChange: '+ edtSearch.Text);

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
        Result := MatchesMask(AnsiLowerCase(FFiles[I-1].TheFile.Name), sSearchName);

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

procedure TColumnsFileView.CloseAltPanel;
begin
  pnAltSearch.Visible:=False;
  edtSearch.Text:='';
  FActive:= False;
end;

procedure TColumnsFileView.ShowAltPanel(Char : TUTF8Char);
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

procedure TColumnsFileView.UpDatelblInfo;
var
  i: Integer;
  FilesInDir, FilesSelected: Integer;
  SizeInDir, SizeSelected: Int64;
  SizeProperty: TFileSizeProperty;
  SizeSupported: Boolean;
begin
  if not fUpdateFileCount then Exit;

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

procedure TColumnsFileView.MarkAll;
begin
  MarkAllFiles(True);
  UpDatelblInfo;
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.MarkPlus;
var
  s: String;
begin
  if IsEmpty then Exit;
  s := FLastMark;
  if not ShowInputComboBox(rsMarkPlus, rsMaskInput, glsMaskHistory, s) then Exit;
  FLastMark := s;
  MarkGroup(s, True);
  UpDatelblInfo;
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.MarkShiftPlus;
begin
  if IsActiveItemValid then
  begin
    MarkGroup('*' + GetActiveItem.TheFile.Extension, True);
    UpDatelblInfo;
    dgPanel.Invalidate;
  end;
end;

procedure TColumnsFileView.MarkShiftMinus;
begin
  if IsActiveItemValid then
  begin
    MarkGroup('*' + GetActiveItem.TheFile.Extension, False);
    UpDatelblInfo;
    dgPanel.Invalidate;
  end;
end;

procedure TColumnsFileView.MarkMinus;
var
  s: String;
begin
  if IsEmpty then Exit;
  s := FLastMark;
  if not ShowInputComboBox(rsMarkMinus, rsMaskInput, glsMaskHistory, s) then Exit;
  FLastMark := s;
  MarkGroup(s, False);
  UpDatelblInfo;
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.UnMarkAll;
begin
  MarkAllFiles(False);
  UpDatelblInfo;
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.MarkGroup(const sMask: String; bSelect: Boolean);
var
  i: Integer;
begin
  for i := 0 to FFiles.Count - 1 do
  begin
    if FFiles[i].TheFile.Name = '..' then Continue;
    if MatchesMaskList(FFiles[i].TheFile.Name, sMask) then
      FFiles[i].Selected := bSelect;
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
  NewFileNameAbsolute: String;
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
        NewFileNameAbsolute := ExtractFilePath(OldFileNameAbsolute) + NewFileName;

        if mbFileExists(NewFileNameAbsolute) then
        begin
          if MsgBox(Format(rsMsgFileExistsRwrt, [NewFileName]),
                    [msmbYes, msmbNo], msmbYes, msmbNo) = mmrNo then
          begin
            Exit;
          end;
        end;

        if mbRenameFile(OldFileNameAbsolute, NewFileNameAbsolute) = True then
        begin
          edtRename.Visible:=False;
          LastActive := NewFileName;
          Reload;
          SetFocus;
        end
        else
          msgError(Format(rsMsgErrRename, [ExtractFileName(OldFileNameAbsolute), NewFileName]));
      end;

{$IFDEF LCLGTK2}
    // Workaround for GTK2 - up and down arrows moving through controls.
    VK_UP,
    VK_DOWN:
      Key := 0;
{$ENDIF}
  end;
end;

procedure TColumnsFileView.dgPanelDrawCell(Sender: TObject; ACol, ARow: integer;
  Rect: TRect; State: TGridDrawState);

var
  //shared variables
  s:   string;
  AFile: TColumnsViewFile;
  iTextTop: Integer;

  //------------------------------------------------------
  //begin subprocedures
  //------------------------------------------------------

  procedure DrawFixed;
  //------------------------------------------------------
  var
    SortingDirection: TSortDirection;
    TitleX: Integer;
  begin
    if not (ACol in [0..ActiveColmSlave.ColumnsCount - 1]) then
      Exit;

    iTextTop := Rect.Top + (dgPanel.RowHeights[0] div 2) -
                           (dgPanel.Canvas.TextHeight('Pp') div 2);
    with dgPanel do
    begin
      TitleX := 0;
      s      := ActiveColmSlave.GetColumnTitle(ACol);

      SortingDirection := FSorting.GetSortingDirection(ACol);
      if SortingDirection <> sdNone then
      begin
        TitleX := TitleX + gIconsSize;
        PixMapManager.DrawBitmap(
          PixMapManager.GetIconBySortingDirection(SortingDirection), Canvas, Rect);
      end;

      TitleX := max(TitleX, 4);

      if gCutTextToColWidth then
      begin
        while Canvas.TextWidth(s) - ((Rect.Right - Rect.Left) - TitleX) > 0 do
          UTF8Delete(s, UTF8Length(s), 1);
      end;

      Canvas.TextOut(Rect.Left + TitleX, iTextTop, s);
    end;
  end; // of DrawHeader
  //------------------------------------------------------


  procedure DrawIconCell;
  //------------------------------------------------------
  var
    Tr: TRect;
  begin
    with dgPanel do
    begin
      if (AFile.IconID >= 0) and gShowIcons then
      begin
        Tr      := Rect;
        Tr.Left := Tr.Left + 1;
        PixMapManager.DrawBitmap(AFile.IconID, Canvas, Tr);
      end;

      s := ActiveColmSlave.GetColumnItemResultString(ACol, AFile.TheFile);

      if gCutTextToColWidth then
      begin
        while Canvas.TextWidth(s) - (Rect.Right - Rect.Left) - 4 > 0 do
          Delete(s, Length(s), 1);
      end;
      Canvas.Brush.Style := bsClear;
      if gShowIcons then
        Canvas.TextOut(Rect.Left + gIconsSize + 3, iTextTop, s)
      else
        Canvas.TextOut(Rect.Left + 2, iTextTop, s);
      Canvas.Brush.Style := bsSolid;
    end;
  end; //of DrawIconCell
  //------------------------------------------------------

  procedure DrawOtherCell;
  //------------------------------------------------------
  var
    tw, cw: Integer;
  begin
    with dgPanel do
    begin
      s := ActiveColmSlave.GetColumnItemResultString(ACol, AFile.TheFile);

      if gCutTextToColWidth then
      begin
        while Canvas.TextWidth(s) - (Rect.Right - Rect.Left) - 4 > 0 do
          Delete(s, Length(s), 1);
      end;
      Canvas.Brush.Style := bsClear;

      case ActiveColmSlave.GetColumnAlign(ACol) of

        taRightJustify:
          begin
            cw := ColWidths[ACol];
            tw := Canvas.TextWidth(s);
            Canvas.TextOut(Rect.Left + cw - tw - 3, iTextTop, s);
          end;

        taLeftJustify:
          begin
            Canvas.TextOut(Rect.Left + 3, iTextTop, s);
          end;

        taCenter:
          begin
            cw := ColWidths[ACol];
            tw := Canvas.TextWidth(s);
            Canvas.TextOut(Rect.Left + ((cw - tw - 3) div 2), iTextTop, s);
          end;

      end; //of case

      Canvas.Brush.Style := bsSolid;
    end;//of with
  end; //of DrawOtherCell
  //------------------------------------------------------


  procedure NewPrepareColors;
  //------------------------------------------------------
  var
    newColor, tmp: TColor;

    procedure TextSelect;
    //---------------------
    begin
      with dgPanel do
      begin
        tmp := ActiveColmSlave.GetColumnTextColor(ACol);
        if (tmp <> newColor) and (newColor <> -1) and
           (ActiveColmSlave.GetColumnOvercolor(ACol))
        then
          Canvas.Font.Color := newColor
        else
          Canvas.Font.Color := tmp;
      end;
    end;
    //---------------------
  begin
    with dgPanel do
    begin
      Canvas.Font.Name   := ActiveColmSlave.GetColumnFontName(ACol);
      Canvas.Font.Size   := ActiveColmSlave.GetColumnFontSize(ACol);
      Canvas.Font.Style  := ActiveColmSlave.GetColumnFontStyle(ACol);

      Canvas.Brush.Style := bsSolid;

      if (gdSelected in State) and FActive then
        Canvas.Brush.Color := ActiveColmSlave.GetColumnCursorColor(ACol)
      else
      begin
        if (ARow mod 2) = 0 then
          Canvas.Brush.Color := ActiveColmSlave.GetColumnBackground(ACol)
        else
          Canvas.Brush.Color := ActiveColmSlave.GetColumnBackground2(ACol);
      end;

      Canvas.FillRect(Rect);

{
      newColor := gColorExt.GetColorBy(frp);
}

      if AFile.Selected then
      begin
        if gUseInvertedSelection then
        begin
          //------------------------------------------------------
          if (gdSelected in State) and FActive then
          begin
            Canvas.Brush.Color := ActiveColmSlave.GetColumnCursorColor(ACol);
            Canvas.FillRect(Rect);
            Canvas.Font.Color :=
              InvertColor(ActiveColmSlave.GetColumnCursorText(ACol));
          end
          else
          begin
            Canvas.Brush.Color := ActiveColmSlave.GetColumnMarkColor(ACol);
            Canvas.FillRect(Rect);
            TextSelect;
          end;
          //------------------------------------------------------
        end
        else
          Canvas.Font.Color := ActiveColmSlave.GetColumnMarkColor(ACol);
      end
      else if (gdSelected in State) and FActive then
        Canvas.Font.Color := ActiveColmSlave.GetColumnCursorText(ACol)
      else
      begin
        TextSelect;
      end;

      // draw drop selection
      if ARow = DropRowIndex then
      begin
        Canvas.Pen.Color := ActiveColmSlave.GetColumnTextColor(ACol);
        Canvas.Line(Rect.Left, Rect.Top, Rect.Right, Rect.Top);
        Canvas.Line(Rect.Left, Rect.Bottom - 1, Rect.Right, Rect.Bottom - 1);
      end;
    end;//of with
  end;// of NewPrepareColors;
  //------------------------------------------------------

  //------------------------------------------------------
  //end of subprocedures
  //------------------------------------------------------

begin
  if (ARow >= dgPanel.RowCount) or (ARow < 0) or
     (ACol >= dgPanel.ColCount) or (ACol < 0)
  then
    Exit;

  if not isSlave then
    ActiveColmSlave := ColSet.GetColumnSet(ActiveColm);

  if (gdFixed in State) and gTabHeader then
    DrawFixed  // Draw column headers
  else if FFiles.Count > 0 then
  begin
    AFile := FFiles[ARow - dgPanel.FixedRows]; // substract fixed rows (header)

    NewPrepareColors;

    iTextTop := Rect.Top + (gIconsSize div 2) - (dgPanel.Canvas.TextHeight('Pp') div 2);

    if ACol = 0 then
      DrawIconCell  // Draw icon in the first column
    else
      DrawOtherCell;
  end;
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
//  DebugLn(Self.Name+'.dgPanelExit');
//  edtRename.OnExit(Sender);        // this is hack, because onExit is NOT called
{  if pnAltSearch.Visible then
    CloseAltPanel;}
  FActive:= False;
  lblPath.SetActive(False);
end;

procedure TColumnsFileView.MakeSelectedVisible;
begin
  if dgPanel.Row>=0 then
    MakeVisible(dgPanel.Row);
end;

procedure TColumnsFileView.Select(const FileName: String);
var
  i: Integer;
begin
  LastActive := '';
  if FileName <> '' then // find correct cursor position in Panel (drawgrid)
  begin
    for i := 0 to FFiles.Count - 1 do
      if FFiles[i].TheFile.Name = FileName then
      begin
        dgPanel.Row := i + dgPanel.FixedRows;
        LastActive := FileName;
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

  // If not on a file/directory then exit.
  if (Point.Y <  dgPanel.GetHeaderHeight) or
     (Point.Y >= dgPanel.GridHeight) or
     IsEmpty then Exit;

//  if pnlFile.PanelMode = pmDirectory then
    Screen.Cursor:=crHourGlass;
  try
    ChooseFile(GetActiveItem);
    UpDatelblInfo;
  finally
    dgPanel.Invalidate;
    Screen.Cursor:=crDefault;
  end;

{$IFDEF LCLGTK2}
  dgPanel.fLastDoubleClickTime := Now;
{$ENDIF}
end;

procedure TColumnsFileView.dgPanelEnter(Sender: TObject);
begin
//  DebugLn(Self.Name+'.OnEnter');
  CloseAltPanel;
//  edtRename.OnExit(Sender);        // this is hack, bacause onExit is NOT called
  FActive:= True;
  SetFocus;
  UpDatelblInfo;
  frmMain.EnableHotkeys(True);
  frmMain.SelectedPanel := PanelSelect;
end;

procedure TColumnsFileView.RedrawGrid;
begin
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.UpdateColumnsView;
var
  ColumnsClass: TPanelColumnsClass;
begin
  ColumnsClass := GetColumnsClass;

  dgPanel.FocusRectVisible := ColumnsClass.GetCursorBorder;
  dgPanel.FocusColor := ColumnsClass.GetCursorBorderColor;
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

procedure TColumnsFileView.dgPanelKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ScreenPoint: TPoint;
  ModifierKeys: TShiftState;
  UTF8Char: TUTF8Char;
begin
debugln('panelkeydown');
  // used for quick search by Ctrl+Alt+Letter and Alt+Letter
  if gQuickSearch {and (edtCommand.Tag = 0)} then
  begin
    ModifierKeys := GetKeyShiftStateEx;

    if ((gQuickSearchMode <> []) and
        // Check only Ctrl and Alt as quicksearch keys.
       (ModifierKeys * [ssCtrl, ssAlt] = gQuickSearchMode))
{$IFDEF MSWINDOWS}
    // Entering international characters with Ctrl+Alt on Windows.
    or ((gQuickSearchMode = []) and
       (ModifierKeys * [ssCtrl, ssAlt] = [ssCtrl, ssAlt]) and
       (ModifierKeys - [ssCtrl, ssAlt, ssShift, ssCaps] = []))
{$ENDIF}
    then
    begin
      UTF8Char := VirtualKeyToUTF8Char(Key, ModifierKeys - gQuickSearchMode);
      if UTF8Char <> '' then
      begin
        ShowAltPanel(UTF8Char);
        Key := 0;
        Exit;
      end;
    end;
  end;

  // cursors keys in Lynx like mode
  if (Shift=[]) and (Key=VK_LEFT) and gLynxLike then
  begin
    cdUpLevel;
    Key:=0;
    Exit;
  end;

  if (Shift=[]) and (Key=VK_RIGHT) and gLynxLike then
  begin
    if Assigned(GetActiveItem) then
      ChooseFile(GetActiveItem, True);
    Key:=0;
    Exit;
  end;

  // handle Space key
  if (Shift=[]) and (Key=VK_Space) and
     ((not frmMain.IsCommandLineVisible) or (frmMain.edtCommand.Text='')) then
  begin
    if not IsEmpty then
    begin
      if IsActiveItemValid then
      begin
        if GetActiveItem.TheFile.IsDirectory then
          CalculateSpace(False);

        SelectFile(GetActiveItem);
      end;

      if gSpaceMovesDown then
        dgPanel.Row := dgPanel.Row + 1;

      dgPanel.Invalidate;
      MakeSelectedVisible;
    end;
    Key := 0;
    Exit;
  end;

  if (Shift=[]) and (Key=VK_BACK) and
     ((not frmMain.IsCommandLineVisible) or (frmMain.edtCommand.Text='')) then
  begin
    if (frmMain.edtCommand.Tag = 0) then
    begin
      cdUpLevel;
      RedrawGrid;
    end;
    Key := 0;
    Exit;
  end;

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
    Key := 0;
    Exit;
  end;

  if Key=VK_MULTIPLY then
  begin
    InvertAll;
    Key := 0;
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
    Key := 0;
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
    Key := 0;
    Exit;
  end;

  if Key = VK_SHIFT then
    begin
      FLastSelectionStartRow:= dgPanel.Row;
      Key := 0;
      Exit;
    end;

  if ((Key=VK_END) or (Key=VK_HOME) or (Key=VK_NEXT) or (Key=VK_PRIOR)) and (ssShift in Shift) then
  begin
    //Application.QueueAsyncCall(@SelectRange, -1); // needed?
    SelectRange(-1);
    Key := 0;
    Exit;
  end;

  if ((Key=VK_DOWN) or (Key=VK_UP)) and (ssShift in Shift) then
    begin
      if IsActiveItemValid then
      begin
        SelectFile(GetActiveItem);
        if (dgPanel.Row=dgPanel.RowCount-1) or (dgPanel.Row=dgPanel.FixedRows) then
          dgPanel.Invalidate;
        Key := 0;
        Exit;
      end;
    end;

  {$IFDEF LCLGTK2}
   if ((dgPanel.Row=dgPanel.RowCount-1) and (key=VK_DOWN))
   or ((dgPanel.Row=dgPanel.FixedRows) and (key=VK_UP)) then
    key:=0;
  {$ENDIF}

  if (Key = VK_RETURN) or (Key = VK_SELECT) then
  begin
    if (Shift=[]) or (Shift=[ssCaps]) then // 21.05.2009 - не учитываем CapsLock при перемещении по панелям
    begin
      // Only if there are items in the panel.
      if not IsEmpty then
      begin
        Screen.Cursor := crHourGlass;
        try
          ChooseFile(GetActiveItem);
          UpDatelblInfo;
        finally
          dgPanel.Invalidate;
          Screen.Cursor := crDefault;
        end;
        Key := 0;
        Exit;
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
        Exit;
      end;
    end;
  end;

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

function TColumnsFileView.IsEmpty:Boolean;
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
  lblPath.Width:=pnlHeader.Width - 4;
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
            frmColumnsSetConf.ColumnClass.Clear;
            frmColumnsSetConf.ColumnClass.Load(gIni,ActiveColm);
            {EDIT Set}
            frmColumnsSetConf.ShowModal;

            FreeAndNil(frmColumnsSetConf);
            //TODO: Reload current columns in panels
            frmMain.ReLoadTabs(frmMain.nbLeft);
            frmMain.ReLoadTabs(frmMain.nbRight);
          end;
    1001: //All columns
          begin
            Actions.cm_Options('15');
            frmMain.ReLoadTabs(frmMain.nbLeft);
            frmMain.ReLoadTabs(frmMain.nbRight);
          end;

  else
    begin
      ActiveColm:=ColSet.Items[(Sender as TMenuItem).Tag];
      SetColWidths;
      UpdateColumnsView;
//      ActiveFrame.dgPanel.ColCount:=ColSet.GetColumnSet(ActiveFrame.ActiveColm).ColumnsCount;

//      if ColSet.GetColumnSet(ActiveFrame.ActiveColm).ColumnsCount>0 then
 //      for x:=0 to ColSet.GetColumnSet(ActiveFrame.ActiveColm).ColumnsCount-1 do
   //     ActiveFrame.dgPanel.ColWidths[x]:=ColSet.GetColumnSet(ActiveFrame.ActiveColm).GetColumnWidth(x);
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

constructor TColumnsFileView.Create(AOwner: TWinControl; AFileSource: TFileSource);
begin
  DebugLn('TColumnsFileView.Create components');
  inherited Create(AOwner, AFileSource);
  Parent := AOwner;
  Align := alClient;

  FFiles := TColumnsViewFiles.Create;
  FFileSourceFiles := nil;

  ActiveColmSlave := nil;
  isSlave := False;
  FLastSelectionStartRow := -1;
  FLastMark := '*';
  FLastAutoSelect := False;
  FLastActive := '';

  FUpdateFileCount := True;
  FUpdateDiskFreeSpace := True;

  FSorting := TFileListSorting.Create;
  // default to sorting by 0-th column
  FSortCol := 0;
  FSortDirect := sdAscending;
  FSorting.AddSorting(FSortCol, FSortDirect);

  // -- other components

  dgPanel:=TDrawGridEx.Create(Self, Self);

  pnlHeader:=TPanel.Create(Self);
  pnlHeader.Parent:=Self;
  pnlHeader.Height:=24;
  pnlHeader.Align:=alTop;

  pnlHeader.BevelInner:=bvNone;
  pnlHeader.BevelOuter:=bvNone;

  lblPath:=TPathLabel.Create(pnlHeader);
  lblPath.Parent:=pnlHeader;
  lblPath.Top := 2;
  lblPath.AutoSize:=False;
  lblPath.Width:=pnlHeader.Width - 4;

  edtPath:=TEdit.Create(lblPath);
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

  lblInfo:=TLabel.Create(pnlFooter);
  lblInfo.Parent:=pnlFooter;
  lblInfo.Width:=250;//  pnlFooter.Width;
  lblInfo.AutoSize:=True;

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
  dgPanel.OnUTF8KeyPress := @UTF8KeyPressEvent;
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
  dgPanel.OnHeaderClick:=@dgPanelHeaderClick;
  dgPanel.OnPrepareCanvas:=@dgPanelPrepareCanvas;
  {Alexx2000}
  dgPanel.OnMouseWheelUp := @dgPanelMouseWheelUp;
  dgPanel.OnMouseWheelDown := @dgPanelMouseWheelDown;
  {/Alexx2000}

  edtSearch.OnChange := @edtSearchChange;
  edtSearch.OnKeyDown := @edtSearchKeyDown;
  edtSearch.OnExit := @edtSearchExit;

  edtPath.OnKeyDown := @edtPathKeyDown;
  edtPath.OnExit := @edtPathExit;

  edtRename.OnKeyDown := @edtRenameKeyDown;
  edtRename.OnExit := @edtRenameExit;

  pnlHeader.OnResize := @pnlHeaderResize;

  lblPath.OnClick := @lblPathClick;

  pmColumnsMenu := TPopupMenu.Create(Self);
  pmColumnsMenu.Parent := Self;

  MakeFileSourceFileList;
  MakeDisplayFileList; //

//  setup column widths
  SetColWidths;
  UpdateColumnsView;
  UpdateView;
  UpdatePathLabel;
end;

destructor TColumnsFileView.Destroy;
begin
  if Assigned(FFiles) then
    FreeAndNil(FFiles);
  if Assigned(FFileSourceFiles) then
    FreeAndNil(FFileSourceFiles);
  if Assigned(FSorting) then
    FreeAndNil(FSorting);
  inherited Destroy;
end;

procedure TColumnsFileView.MakeFileSourceFileList;
var
  ListOperation: TFileSourceListOperation;
  AFile: TColumnsViewFile;
  i: Integer;
begin
  //ListOperation := TFactory.GetListOperationObject(FileSource);
  ListOperation := FileSource.GetOperation(fsoList) as TFileSourceListOperation;

  ListOperation.Execute;

  if Assigned(FFileSourceFiles) then
    FreeAndNil(FFileSourceFiles);

  FFileSourceFiles := FileSource.GetFiles; //ListOperation.ReleaseFiles;

  FreeAndNil(ListOperation);

  // Make display file list from file source file list.
  MakeDisplayFileList;

  RedrawGrid;
end;

procedure TColumnsFileView.MakeDisplayFileList;
var
  AFile: TColumnsViewFile;
  i: Integer;
begin
  FFiles.Clear;

  Sort;

  for i := 0 to FFileSourceFiles.Count - 1 do
  begin
    if gShowSystemFiles = False then
    begin
      if FFileSourceFiles[i].IsSysFile then Continue;
    end;

    AFile := TColumnsViewFile.Create(FFileSourceFiles[i]);
    if gShowIcons then
      AFile.IconID := PixMapManager.GetIconByFile(AFile.TheFile, fspDirectAccess in FileSource.Properties);
    FFiles.Add(AFile);
  end;

  // Update row count.
  dgPanel.RowCount := FFiles.Count
                    + dgPanel.FixedRows; // header rows
end;

procedure TColumnsFileView.Reload;
begin
  MakeFileSourceFileList;
  Refresh;
end;

procedure TColumnsFileView.Refresh;
begin
  MakeDisplayFileList;

  if LastActive <> '' then
    Select(LastActive);

  RedrawGrid;
end;

procedure TColumnsFileView.UpdateView;
begin
  pnlHeader.Visible := gCurDir;  // Current directory
  pnlFooter.Visible := gStatusBar;  // Status bar
  GridVertLine:= gGridVertLine;
  GridHorzLine:= gGridHorzLine;

  dgPanel.UpdateView;

{
  if gShowIcons then
    pnlFile.FileList.UpdateFileInformation(pnlFile.PanelMode);
}

  MakeDisplayFileList;
  UpDatelblInfo;
end;

function TColumnsFileView.GetActiveItem: TColumnsViewFile;
var
  CurrentRow: Integer;
begin
  Result := nil;
  if IsEmpty then Exit; // No files in the panel.

  CurrentRow := dgPanel.Row;
  if CurrentRow < dgPanel.FixedRows then
    CurrentRow := dgPanel.FixedRows
  else if CurrentRow > FFiles.Count then
     CurrentRow := dgPanel.FixedRows;

  Result := FFiles[CurrentRow - dgPanel.FixedRows]; // minus fixed header
end;

function TColumnsFileView.GetColumnsClass: TPanelColumnsClass;
begin
  if isSlave then
    Result := ActiveColmSlave
  else
    Result := ColSet.GetColumnSet(ActiveColm);
end;

procedure TColumnsFileView.CalculateSpace(bDisplayMessage:Boolean);
{
var
  fl:TFileList;
  p:TFileRecItem;
}
begin
(*
  fl:= TFileList.Create; // free at Thread end by thread
  with ActiveFrame do
  begin
    if not bDisplayMessage then  // Calculate by <Space> key
      begin
{
        if not IsActiveItemValid then
        begin
          FreeAndNil(fl);
          Exit;
        end;
}

{
        p:= GetActiveItem^;
        p.sNameNoExt:= p.sName; //dstname
        p.sName:= ActiveDir+p.sName;
        p.sPath:= '';

        fl.AddItem(@p);
}
      end
    else
      begin
{
        if SelectFileIfNoSelected(GetActiveItem) = False then
        begin
          FreeAndNil(fl);
          Exit;
        end;

        CopyListSelectedExpandNames(pnlFile.FileList,fl,ActiveDir);
}
      end;
  end;

  try
  with TSpaceThread.Create(fl, bDisplayMessage) do
    begin
      if not bDisplayMessage then
        Screen.Cursor:= crHourGlass;

      // start thread
      Resume;

      if not bDisplayMessage then
        begin
          WaitFor;
          Screen.Cursor:= crDefault;
        end;

{
      with ActiveFrame.GetActiveItem^ do
      begin
        if (bDisplayMessage = False) then
          iDirSize:= FilesSize;
        ActiveFrame.pnlFile.LastActive:= sName;
      end;
}

      if not bDisplayMessage then
        Free;
    end;
  finally
    with ActiveFrame do
    begin
      Screen.Cursor:= crDefault;
//      UnMarkAll;
{
      pnlFile.UpdatePanel;
}
    end;
  end;
*)
end;

procedure TColumnsFileView.UTF8KeyPressEvent(Sender: TObject; var UTF8Key: TUTF8Char);
var
  ModifierKeys: TShiftState;
begin
debugln('panelutf8keypress');
//  if (edtCommand.Tag = 0) then
    begin
      // quick search by Letter only

      // Check for certain Ascii keys.
      if (Length(UTF8Key) = 1) and ((Ord(UTF8Key[1]) <= 32) or
         (UTF8Key[1] in ['+','-','*','/','\'])) then Exit;

      ModifierKeys := GetKeyShiftStateEx;

      if gQuickSearch and (gQuickSearchMode = []) and
         // Check only ssCtrl and ssAlt.
         (ModifierKeys * [ssCtrl, ssAlt] = gQuickSearchMode) then
        begin
          // Make upper case if either caps-lock is toggled or shift pressed.
          if (ssCaps in ModifierKeys) xor (ssShift in ModifierKeys) then
            UTF8Key := UTF8UpperCase(UTF8Key)
          else
            UTF8Key := UTF8LowerCase(UTF8Key);

          ShowAltPanel(UTF8Key);
          UTF8Key:= '';
        end;
    end
end;

procedure TColumnsFileView.DoDragDropOperation(Operation: TDragDropOperation;
                                               DropParams: TDropParams);
var
  AFile: TColumnsViewFile;
  TargetDir: string;
  iCol, iRow: Integer;
  ClientDropPoint: TPoint;
  SourceFileName, TargetFileName: string;
begin
  with DropParams do
  begin
    if Files.Count > 0 then
    begin
      ClientDropPoint := dgPanel.ScreenToClient(ScreenDropPoint);
      dgPanel.MouseToCell(ClientDropPoint.X, ClientDropPoint.Y, iCol, iRow);

      // default to current active directory in the destination panel
      TargetDir := Self.CurrentPath;

      if (DropIntoDirectories = True) and
         (iRow >= dgPanel.FixedRows) and
         (ClientDropPoint.Y < dgPanel.GridHeight) then
      begin
        AFile := FFiles[iRow - dgPanel.FixedRows];

        // If dropped into a directory modify destination path accordingly.
        if Assigned(AFile) and AFile.TheFile.IsDirectory then
        begin
          if AFile.TheFile.Name = '..' then
            // remove the last subdirectory in the path
            TargetDir := GetParentDir(TargetDir)
          else
            TargetDir := TargetDir + AFile.TheFile.Name + DirectorySeparator;
        end;
      end;

      case Operation of

        ddoMove:
          if GetDragDropType = ddtInternal then
            frmMain.RenameFile(TargetDir)
          else
          begin
//            frmMain.RenameFile(FileList, TargetPanel, TargetDir); // will free FileList
            Files := nil;
          end;

        ddoCopy:
          if GetDragDropType = ddtInternal then
            frmMain.CopyFile(TargetDir)
          else
          begin
//            frmMain.CopyFile(FileList, TargetPanel, TargetDir);   // will free FileList
            Files := nil;
          end;

        ddoSymLink, ddoHardLink:
          begin
{
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
                RefreshPanel;
            end;
}
          end;
      end;
    end;
  end;

  FreeAndNil(DropParams);
end;

procedure TColumnsFileView.cm_MarkInvert(param: string='');
begin
  InvertAll;
end;

procedure TColumnsFileView.cm_QuickSearch(param: string='');
begin
  ShowAltPanel;
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
  if gTabHeader then
  begin
    if RowCount < 1 then
      RowCount := 1;

    FixedRows := 1;

    TabHeaderHeight := gIconsSize + 1;
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

procedure TDrawGridEx.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Point: TPoint;
  AFile: TColumnsViewFile;
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
        AFile := (Parent as TColumnsFileView).FFiles[DragRowIndex - FixedRows]; // substract fixed rows (header)
        // Check if valid item is being dragged.
        if (Parent as TColumnsFileView).IsItemValid(AFile) then
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
    (Parent as TColumnsFileView).dgPanelMouseUp(Self, Button, Shift, X, Y);
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
  AFile: TColumnsViewFile;
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

  // Get the item over which there is something dragged.
  AFile := TargetPanel.FFiles[iRow - FixedRows]; // substract fixed rows (header)

  if Assigned(AFile) and
     (AFile.TheFile.IsDirectory{ or fri^.bLinkIsDir}) and
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
  Files: TFileSystemFiles;
begin
  if FileNamesList.Count > 0 then
  begin
    Files := TFileSystemFiles.Create;
    Files.LoadFromFileNames(FileNamesList);
    DropFiles(TDropParams.Create(
      Files, DropEffect, ScreenPoint, True,
      nil, Self.Parent as TColumnsFileView));
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
    if DropParams.Files.Count > 0 then
    begin
      case DropParams.DropEffect of

        DropMoveEffect:
          DropParams.TargetPanel.DoDragDropOperation(ddoMove, DropParams);

        DropCopyEffect:
          DropParams.TargetPanel.DoDragDropOperation(ddoCopy, DropParams);

        DropLinkEffect:
          DropParams.TargetPanel.DoDragDropOperation(ddoSymLink, DropParams);

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
                  aFiles: TFiles; aDropEffect: TDropEffect;
                  aScreenDropPoint: TPoint; aDropIntoDirectories: Boolean;
                  aSourcePanel: TColumnsFileView;
                  aTargetPanel: TColumnsFileView);
begin
  Files := aFiles;
  DropEffect := aDropEffect;
  ScreenDropPoint := aScreenDropPoint;
  DropIntoDirectories := aDropIntoDirectories;
  SourcePanel := aSourcePanel;
  TargetPanel := aTargetPanel;
end;

destructor TDropParams.Destroy;
begin
  if Assigned(Files) then
    FreeAndNil(Files);
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

  OnMouseEnter:=@MouseEnterEvent;
  OnMouseMove :=@MouseMoveEvent;
  OnMouseLeave:=@MouseLeaveEvent;
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

end.

