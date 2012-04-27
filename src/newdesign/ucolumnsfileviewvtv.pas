unit uColumnsFileViewVtv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, StdCtrls, ExtCtrls, Grids,
  LMessages, LCLIntf, LCLType, Menus, uTypes,
  uFile,
  uFileProperty,
  uFileView,
  uFileViewWithMainCtrl,
  uFileSource,
  uDisplayFile,
  uColumns,
  uFileSorting,
  DCXmlConfig,
  DCClassesUtf8,
  VirtualTrees;

type
  TColumnsSortDirections = array of uFileSorting.TSortDirection;
  TColumnsFileViewVTV = class;

  TNodeRange = record
    First: PVirtualNode;
    Last: PVirtualNode;
  end;

  TColumnsDrawTreeRecord = record end; // We don't need anything in it, just Node^.Index.
  PColumnsDrawTreeRecord = ^TColumnsDrawTreeRecord;

  { TColumnsDrawTree }

  TColumnsDrawTree = class(TVirtualDrawTree)
  private
    ColumnsView: TColumnsFileViewVTV;

    function GetGridHorzLine: Boolean;
    function GetGridVertLine: Boolean;
    procedure SetGridHorzLine(const AValue: Boolean);
    procedure SetGridVertLine(const AValue: Boolean);

    function GetNodeFile(Node: PVirtualNode): TDisplayFile;
    function GetVisibleNodes: TNodeRange;
    procedure SetAllRowsHeights(ARowHeight: Cardinal);

  protected

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;

    procedure InitializeWnd; override;
    procedure FinalizeWnd; override;

    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;

  public
    constructor Create(AOwner: TComponent; AParent: TWinControl); reintroduce;
    procedure AfterConstruction; override;

    procedure UpdateView;

    function MouseOnGrid(X, Y: LongInt): Boolean;

    // Returns height of all the header rows.
    function GetHeaderHeight: Integer;
    function GetVisibleIndexes: TRange;

    property GridVertLine: Boolean read GetGridVertLine write SetGridVertLine;
    property GridHorzLine: Boolean read GetGridHorzLine write SetGridHorzLine;
  end;

  { TColumnsFileViewVTV }

  TColumnsFileViewVTV = class(TFileViewWithMainCtrl)
  private
    FColumnsSortDirections: TColumnsSortDirections;
    FFileNameColumn: Integer;
    FExtensionColumn: Integer;

    pmColumnsMenu: TPopupMenu;
    edtRename: TEdit;
    dgPanel: TColumnsDrawTree;
    tmClearGrid: TTimer;

    function GetColumnsClass: TPanelColumnsClass;

    procedure SetRowCount(Count: Integer);
    procedure SetFilesDisplayItems;
    procedure SetColumns;

    procedure MakeVisible(Node: PVirtualNode);
    procedure DoSelectionChanged(Node: PVirtualNode); overload;

    {en
       Updates GUI after the display file list has changed.
    }
    procedure DisplayFileListHasChanged;
    {en
       Format and cache all columns strings for the file.
    }
    procedure MakeColumnsStrings(AFile: TDisplayFile);
    procedure MakeColumnsStrings(AFile: TDisplayFile; ColumnsClass: TPanelColumnsClass);
    procedure ClearAllColumnsStrings;
    procedure EachViewUpdateColumns(AFileView: TFileView; UserData: Pointer);

    {en
       Translates file sorting by functions to sorting directions of columns.
    }
    procedure SetColumnsSortDirections;

    {en
       Checks which file properties are needed for displaying.
    }
    function GetFilePropertiesNeeded: TFilePropertiesTypes;

    procedure ShowRenameFileEdit(AFile: TFile);

    // -- Events --------------------------------------------------------------

    procedure edtRenameExit(Sender: TObject);
    procedure edtRenameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure dgPanelAdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
      const Elements: THeaderPaintElements);
    procedure dgPanelAfterItemPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
    procedure dgPanelBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure dgPanelHeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo;
      var Elements: THeaderPaintElements);
    procedure dgPanelDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState;
      const Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure dgPanelDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      Formats: TFormatArray; Shift: TShiftState; const Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure dgPanelFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure dgPanelFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn,
      NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure dgPanelHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure dgPanelMouseWheelUp(Sender: TObject; Shift: TShiftState;
                                  MousePos: TPoint; var Handled: Boolean);
    procedure dgPanelMouseWheelDown(Sender: TObject; Shift: TShiftState;
                                  MousePos: TPoint; var Handled: Boolean);
    procedure dgPanelScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
    procedure dgPanelResize(Sender: TObject);
    procedure tmClearGridTimer(Sender: TObject);
    procedure ColumnsMenuClick(Sender: TObject);

  protected
    procedure CreateDefault(AOwner: TWinControl); override;

    procedure BeforeMakeFileList; override;
    procedure AfterMakeFileList; override;
    procedure ClearAfterDragDrop; override;
    procedure DoFileUpdated(AFile: TDisplayFile; UpdatedProperties: TFilePropertiesTypes = []); override;
    procedure DoHandleKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoMainControlShowHint(FileIndex: PtrInt; X, Y: Integer); override;
    procedure DoUpdateView; override;
    function GetActiveFileIndex: PtrInt; override;
    function GetFileIndexFromCursor(X, Y: Integer; out AtFileList: Boolean): PtrInt; override;
    function GetFileRect(FileIndex: PtrInt): TRect; override;
    function GetVisibleFilesIndexes: TRange; override;
    procedure RedrawFile(FileIndex: PtrInt); override;
    procedure RedrawFile(DisplayFile: TDisplayFile); override;
    procedure RedrawFiles; override;
    procedure SetActiveFile(FileIndex: PtrInt); override;
    procedure SetSorting(const NewSortings: TFileSortings); override;

  public
    ActiveColm: String;
    ActiveColmSlave: TPanelColumnsClass;
    isSlave:boolean;
//---------------------

    constructor Create(AOwner: TWinControl; AFileSource: IFileSource; APath: String; AFlags: TFileViewFlags = []); override;
    constructor Create(AOwner: TWinControl; AFileView: TFileView; AFlags: TFileViewFlags = []); override;
    constructor Create(AOwner: TWinControl; AConfig: TIniFileEx; ASectionName: String; ATabIndex: Integer; AFlags: TFileViewFlags = []); override;
    constructor Create(AOwner: TWinControl; AConfig: TXmlConfig; ANode: TXmlNode; AFlags: TFileViewFlags = []); override;

    destructor Destroy; override;

    function Clone(NewParent: TWinControl): TColumnsFileViewVTV; override;
    procedure CloneTo(FileView: TFileView); override;

    procedure AddFileSource(aFileSource: IFileSource; aPath: String); override;

    procedure LoadConfiguration(Section: String; TabIndex: Integer); override;
    procedure LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); override;
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); override;

    procedure UpdateColumnsView;

  published  // commands
    procedure cm_RenameOnly(const Params: array of string);
  end;

implementation

uses
  LCLProc, Clipbrd, uLng, uShowMsg, uGlobs, uPixmapManager, uDebug,
  uDCUtils, math, fMain, fOptions,
  uOrderedFileView,
  uFileSourceProperty,
  uFileSourceOperationTypes,
  fColumnsSetConf,
  uKeyboard,
  uFileSourceUtil,
  uFileFunctions,
  uFormCommands,
  fOptionsCustomColumns;

type
  TEachViewCallbackReason = (evcrUpdateColumns);
  TEachViewCallbackMsg = record
    Reason: TEachViewCallbackReason;
    UpdatedColumnsSetName: String;
    NewColumnsSetName: String; // If columns name renamed
  end;
  PEachViewCallbackMsg = ^TEachViewCallbackMsg;

procedure TColumnsFileViewVTV.SetSorting(const NewSortings: TFileSortings);
begin
  inherited SetSorting(NewSortings);
  SetColumnsSortDirections;
  SortAllDisplayFiles;
  ReDisplayFileList;
end;

procedure TColumnsFileViewVTV.LoadConfiguration(Section: String; TabIndex: Integer);
var
  ColumnsClass: TPanelColumnsClass;
  SortCount: Integer;
  SortColumn: Integer;
  SortDirection: uFileSorting.TSortDirection;
  i: Integer;
  sIndex: String;
  NewSorting: TFileSortings = nil;
  Column: TPanelColumn;
  SortFunctions: TFileFunctions;
begin
  sIndex := IntToStr(TabIndex);

  ActiveColm := gIni.ReadString(Section, sIndex + '_columnsset', 'Default');

  // Load sorting options.
  ColumnsClass := GetColumnsClass;
  SortCount := gIni.ReadInteger(Section, sIndex + '_sortcount', 0);
  for i := 0 to SortCount - 1 do
  begin
    SortColumn := gIni.ReadInteger(Section, sIndex + '_sortcolumn' + IntToStr(i), -1);
    if (SortColumn >= 0) and (SortColumn < ColumnsClass.ColumnsCount) then
    begin
      Column := ColumnsClass.GetColumnItem(SortColumn);
      if Assigned(Column) then
      begin
        SortFunctions := Column.GetColumnFunctions;
        SortDirection := uFileSorting.TSortDirection(gIni.ReadInteger(Section, sIndex + '_sortdirection' + IntToStr(i), Integer(sdNone)));
        AddSorting(NewSorting, SortFunctions, SortDirection);
      end;
    end;
  end;
  inherited SetSorting(NewSorting);
end;

procedure TColumnsFileViewVTV.LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
var
  ColumnsClass: TPanelColumnsClass;
  SortColumn: Integer;
  SortDirection: uFileSorting.TSortDirection;
  ColumnsViewNode: TXmlNode;
  NewSorting: TFileSortings = nil;
  Column: TPanelColumn;
  SortFunctions: TFileFunctions;
begin
  inherited LoadConfiguration(AConfig, ANode);

  // Try to read new view-specific node.
  ColumnsViewNode := AConfig.FindNode(ANode, 'ColumnsView');
  if Assigned(ColumnsViewNode) then
    ANode := ColumnsViewNode;

  ActiveColm := AConfig.GetValue(ANode, 'ColumnsSet', 'Default');

  // Load sorting options.
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
          Column := ColumnsClass.GetColumnItem(SortColumn);
          if Assigned(Column) then
          begin
            SortFunctions := Column.GetColumnFunctions;
            SortDirection := uFileSorting.TSortDirection(AConfig.GetValue(ANode, 'Direction', Integer(sdNone)));
            AddSorting(NewSorting, SortFunctions, SortDirection);
          end;
        end
        else
          DCDebug('Invalid entry in configuration: ' + AConfig.GetPathFromNode(ANode) + '.');
      end;
      ANode := ANode.NextSibling;
    end;
    inherited SetSorting(NewSorting);
  end;
end;

procedure TColumnsFileViewVTV.SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
begin
  inherited SaveConfiguration(AConfig, ANode);

  AConfig.SetAttr(ANode, 'Type', 'columns');

  ANode := AConfig.FindNode(ANode, 'ColumnsView', True);
  AConfig.ClearNode(ANode);

  AConfig.SetValue(ANode, 'ColumnsSet', ActiveColm);
end;

procedure TColumnsFileViewVTV.dgPanelDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState;
  const Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  MainControlDragOver(Sender, Source, Pt.x, Pt.y, State, Accept);
end;

procedure TColumnsFileViewVTV.dgPanelDragDrop(Sender: TBaseVirtualTree;
  Source: TObject;
  Formats: TFormatArray; Shift: TShiftState;
  const Pt: TPoint; var Effect: Integer; Mode: TDropMode);
begin
  MainControlDragDrop(Sender, Source, Pt.x, Pt.y);
end;

procedure TColumnsFileViewVTV.dgPanelFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  dgPanel.TreeOptions.AutoOptions := dgPanel.TreeOptions.AutoOptions - [toDisableAutoscrollOnFocus];

  if Assigned(Node) then
    DoFileIndexChanged(Node^.Index)
  else
    LastActiveFile := '';
end;

procedure TColumnsFileViewVTV.dgPanelFocusChanging(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
begin
  if (OldColumn <> NewColumn) and (OldNode = NewNode) then
    dgPanel.TreeOptions.AutoOptions := dgPanel.TreeOptions.AutoOptions + [toDisableAutoscrollOnFocus]
  else
    dgPanel.TreeOptions.AutoOptions := dgPanel.TreeOptions.AutoOptions - [toDisableAutoscrollOnFocus];
end;

procedure TColumnsFileViewVTV.dgPanelHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ShiftState : TShiftState;
  SortingDirection : uFileSorting.TSortDirection;
  ColumnsClass: TPanelColumnsClass;
  PanelColumn: TPanelColumn;
  NewSorting: TFileSortings;
  SortFunctions: TFileFunctions;
  I : Integer;
  Point: TPoint;
  MI: TMenuItem;
begin
  case Button of
    mbLeft:
      begin
        ColumnsClass := GetColumnsClass;
        PanelColumn := ColumnsClass.GetColumnItem(Column);
        if Assigned(PanelColumn) then
        begin
          NewSorting := Sorting;
          SortFunctions := PanelColumn.GetColumnFunctions;
          ShiftState := GetKeyShiftStateEx;
          if [ssShift, ssCtrl] * ShiftState = [] then
          begin
            SortingDirection := GetSortDirection(NewSorting, SortFunctions);
            if SortingDirection = sdNone then
              SortingDirection := uFileSorting.sdAscending
            else
              SortingDirection := ReverseSortDirection(SortingDirection);
            NewSorting := nil;
          end
          else
          begin
            SortingDirection := uFileSorting.sdAscending;
          end;

          AddOrUpdateSorting(NewSorting, SortFunctions, SortingDirection);
          SetSorting(NewSorting);
        end;
      end;

    mbRight:
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

        Point   := dgPanel.ClientToScreen(Classes.Point(0,0));
        Point.X := Point.X + X - 50;
        Point.Y := Point.Y + dgPanel.GetHeaderHeight;
        pmColumnsMenu.PopUp(Point.X, Point.Y);
      end;
  end;
end;

procedure TColumnsFileViewVTV.dgPanelAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
var
  SortingDirection: uFileSorting.TSortDirection;
  TitleX: Integer;
  ColumnsSet: TPanelColumnsClass;
  aCol: Integer;
  iTextTop: Integer;
  s: String;
  aRect: TRect;
begin
  aCol  := PaintInfo.Column.Index;
  aRect := PaintInfo.PaintRectangle;
  // PaintRectangle is reduced by 2 pixels on each side even with owner draw,
  // so revert this change.
  InflateRect(aRect, 2, 2);

  ColumnsSet := GetColumnsClass;

  iTextTop := aRect.Top + (PaintInfo.Column.Owner.Header.Height - PaintInfo.TargetCanvas.TextHeight('Wg')) div 2;
  TitleX   := 0;
  s        := ColumnsSet.GetColumnTitle(ACol);

  SortingDirection := FColumnsSortDirections[ACol];
  if SortingDirection <> sdNone then
  begin
    TitleX := TitleX + gIconsSize;
    PixMapManager.DrawBitmap(
        PixMapManager.GetIconBySortingDirection(SortingDirection),
        PaintInfo.TargetCanvas,
        aRect.Left, aRect.Top + (PaintInfo.Column.Owner.Header.Height - gIconsSize) div 2);
  end;

  TitleX := max(TitleX, 4);

  if gCutTextToColWidth then
  begin
    if (aRect.Right - aRect.Left) < TitleX then
      // Column too small to display text.
      Exit
    else
      while PaintInfo.TargetCanvas.TextWidth(s) - ((aRect.Right - aRect.Left) - TitleX) > 0 do
        UTF8Delete(s, UTF8Length(s), 1);
  end;

  PaintInfo.TargetCanvas.Brush.Style := bsClear;
  PaintInfo.TargetCanvas.TextOut(aRect.Left + TitleX, iTextTop, s);
end;

procedure TColumnsFileViewVTV.dgPanelAfterItemPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect);
var
  ColumnsSet: TPanelColumnsClass;
  IsFocused: Boolean;

  procedure DrawLines;
  begin
    // Draw focus rect.
    if not gUseFrameCursor and IsFocused and Active then
    begin
      TargetCanvas.Pen.Color := ColumnsSet.GetCursorBorderColor;
      TargetCanvas.Brush.Color := ColumnsSet.GetCursorBorderColor;
      TargetCanvas.FrameRect(ItemRect);
    end;
  end;
begin
  IsFocused := Node = dgPanel.FocusedNode;
  ColumnsSet := GetColumnsClass;
  DrawLines;
end;

procedure TColumnsFileViewVTV.dgPanelBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; const ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
begin
  EraseAction := eaNone;
end;

procedure TColumnsFileViewVTV.dgPanelHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  Elements := [hpeSortGlyph, hpeText];
end;

procedure TColumnsFileViewVTV.dgPanelMouseWheelUp(Sender: TObject;
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

procedure TColumnsFileViewVTV.dgPanelMouseWheelDown(Sender: TObject;
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

procedure TColumnsFileViewVTV.dgPanelScroll(Sender: TBaseVirtualTree; DeltaX, DeltaY: Integer);
begin
  if DeltaY <> 0 then
    EnsureDisplayProperties;
end;

procedure TColumnsFileViewVTV.dgPanelResize(Sender: TObject);
begin
  EnsureDisplayProperties;
end;

procedure TColumnsFileViewVTV.tmClearGridTimer(Sender: TObject);
begin
  tmClearGrid.Enabled := False;

  if IsEmpty then
  begin
    SetRowCount(0);
    RedrawFiles;
  end;
end;

procedure TColumnsFileViewVTV.ShowRenameFileEdit(AFile: TFile);
var
  ALeft, ATop, AWidth, AHeight: Integer;
  aRect: TRect;
begin
  if FFileNameColumn <> -1 then
  begin
    edtRename.Font.Name  := GetColumnsClass.GetColumnFontName(FFileNameColumn);
    edtRename.Font.Size  := GetColumnsClass.GetColumnFontSize(FFileNameColumn);
    edtRename.Font.Style := GetColumnsClass.GetColumnFontStyle(FFileNameColumn);

    aRect := dgPanel.GetDisplayRect(dgPanel.FocusedNode, FFileNameColumn, False);
    ATop := aRect.Top - 2;
    ALeft := aRect.Left;
    if gShowIcons <> sim_none then
      Inc(ALeft, gIconsSize + 2);
    AWidth := aRect.Right - aRect.Left;
    if Succ(FFileNameColumn) = FExtensionColumn then
      Inc(AWidth, dgPanel.Header.Columns[FExtensionColumn].Width);
    AHeight := dgPanel.FocusedNode^.NodeHeight + 4;

    edtRename.SetBounds(ALeft, ATop, AWidth, AHeight);

    edtRename.Hint := aFile.FullPath;
    edtRename.Text := aFile.Name;
    edtRename.Visible := True;
    edtRename.SetFocus;
    if gRenameSelOnlyName and (aFile.Extension <> EmptyStr) and (aFile.Name <> EmptyStr) then
      begin
        {$IFDEF LCLGTK2}
        edtRename.SelStart:=1;
        {$ENDIF}
        edtRename.SelStart:=0;
        edtRename.SelLength:= UTF8Length(aFile.Name) - UTF8Length(aFile.Extension) - 1;
      end
    else
      edtRename.SelectAll;
  end;
end;

procedure TColumnsFileViewVTV.RedrawFile(FileIndex: PtrInt);
begin
  dgPanel.InvalidateNode(PVirtualNode(FFiles[FileIndex].DisplayItem));
end;

procedure TColumnsFileViewVTV.RedrawFile(DisplayFile: TDisplayFile);
begin
  dgPanel.InvalidateNode(PVirtualNode(DisplayFile.DisplayItem));
end;

procedure TColumnsFileViewVTV.SetColumnsSortDirections;
var
  Columns: TPanelColumnsClass;

  function SetSortDirection(ASortFunction: TFileFunction; ASortDirection: uFileSorting.TSortDirection; Overwrite: Boolean): Boolean;
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
          if Overwrite or (FColumnsSortDirections[k] = sdNone) then
          begin
            FColumnsSortDirections[k] := ASortDirection;
            Exit(True);
          end;
        end;
    end;
    Result := False;
  end;

var
  i, j: Integer;
  ASortings: TFileSortings;
begin
  Columns := GetColumnsClass;
  ASortings := Sorting;

  SetLength(FColumnsSortDirections, Columns.Count);
  for i := 0 to Length(FColumnsSortDirections) - 1 do
    FColumnsSortDirections[i] := sdNone;

  for i := 0 to Length(ASortings) - 1 do
  begin
    for j := 0 to Length(ASortings[i].SortFunctions) - 1 do
    begin
      // Search for the column containing the sort function and add sorting
      // by that column. If function is Name and it is not found try searching
      // for NameNoExtension + Extension and vice-versa.
      if not SetSortDirection(ASortings[i].SortFunctions[j], ASortings[i].SortDirection, True) then
      begin
        if ASortings[i].SortFunctions[j] = fsfName then
        begin
          SetSortDirection(fsfNameNoExtension, ASortings[i].SortDirection, False);
          SetSortDirection(fsfExtension, ASortings[i].SortDirection, False);
        end
        else if ASortings[i].SortFunctions[j] in [fsfNameNoExtension, fsfExtension] then
        begin
          SetSortDirection(fsfName, ASortings[i].SortDirection, False);
        end;
      end;
    end;
  end;
end;

procedure TColumnsFileViewVTV.SetFilesDisplayItems;
var
  Node: PVirtualNode;
  Index: Integer = 0;
begin
  Node := dgPanel.GetFirstNoInit;
  while Assigned(Node) do
  begin
    FFiles[Index].DisplayItem := Node;
    Inc(Index);
    Node := Node^.NextSibling;
  end;
end;

function TColumnsFileViewVTV.GetFilePropertiesNeeded: TFilePropertiesTypes;
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

function TColumnsFileViewVTV.GetFileRect(FileIndex: PtrInt): TRect;
begin
  Result := dgPanel.GetDisplayRect(PVirtualNode(FFiles[FileIndex].DisplayItem), 0, False);
end;

function TColumnsFileViewVTV.GetVisibleFilesIndexes: TRange;
begin
  Result := dgPanel.GetVisibleIndexes;
end;

procedure TColumnsFileViewVTV.SetRowCount(Count: Integer);
begin
  FUpdatingActiveFile := True;
  dgPanel.RootNodeCount := Count;
  FUpdatingActiveFile := False;
end;

procedure TColumnsFileViewVTV.SetColumns;
var
  x: Integer;
  ColumnsClass: TPanelColumnsClass;
  col: TVirtualTreeColumn;
begin
  //  setup column widths
  ColumnsClass := GetColumnsClass;

  dgPanel.Header.Columns.Clear;
  dgPanel.Header.Columns.BeginUpdate;
  try
    for x:= 0 to ColumnsClass.ColumnsCount - 1 do
    begin
      col := dgPanel.Header.Columns.Add;
      if not ((x = 0) and gAutoFillColumns and (gAutoSizeColumn = 0)) then
        col.Options := col.Options + [coAutoSpring];
      if gAutoFillColumns then
        dgPanel.Header.AutoSizeIndex := gAutoSizeColumn;

      col.Width   := ColumnsClass.GetColumnWidth(x);
      //col.Text    := ColumnsClass.GetColumnTitle(x); // I think not needed, as we draw text ourselves.
      col.Margin  := 0;
      col.Spacing := 0;
    end;
  finally
    dgPanel.Header.Columns.EndUpdate;
  end;
end;

procedure TColumnsFileViewVTV.edtRenameExit(Sender: TObject);
begin
  edtRename.Visible := False;

  // OnEnter don't called automatically (bug?)
  // TODO: Check on which widgetset/OS this is needed.
  dgPanel.OnEnter(Self);
end;

procedure TColumnsFileViewVTV.edtRenameKeyDown(Sender: TObject; var Key: Word;
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

procedure TColumnsFileViewVTV.MakeVisible(Node: PVirtualNode);
begin
  dgPanel.ScrollIntoView(Node, False, False);
end;

procedure TColumnsFileViewVTV.SetActiveFile(FileIndex: PtrInt);
begin
  dgPanel.FocusedNode := PVirtualNode(FFiles[FileIndex].DisplayItem);
end;

procedure TColumnsFileViewVTV.RedrawFiles;
begin
  dgPanel.Invalidate;
end;

procedure TColumnsFileViewVTV.UpdateColumnsView;
var
  ColumnsClass: TPanelColumnsClass;
  OldFilePropertiesNeeded: TFilePropertiesTypes;
begin
  ClearAllColumnsStrings;

  // If the ActiveColm set doesn't exist this will retrieve either
  // the first set or the default set.
  ColumnsClass := GetColumnsClass;
  // Set name in case a different set was loaded.
  ActiveColm := ColumnsClass.Name;

  SetColumns;
  SetColumnsSortDirections;

  dgPanel.UpdateView;

  OldFilePropertiesNeeded := FilePropertiesNeeded;
  FilePropertiesNeeded := GetFilePropertiesNeeded;
  if FilePropertiesNeeded >= OldFilePropertiesNeeded then
  begin
    EnsureDisplayProperties;
  end;
end;

procedure TColumnsFileViewVTV.ColumnsMenuClick(Sender: TObject);
var
  frmColumnsSetConf: TfColumnsSetConf;
  Index: Integer;
  Msg: TEachViewCallbackMsg;
begin
  Case (Sender as TMenuItem).Tag of
    1000: //This
          begin
            frmColumnsSetConf := TfColumnsSetConf.Create(nil);
            try
              Msg.Reason := evcrUpdateColumns;
              Msg.UpdatedColumnsSetName := ActiveColm;
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
                Msg.NewColumnsSetName := frmColumnsSetConf.GetColumnsClass.Name;
                frmMain.ForEachView(@EachViewUpdateColumns, @Msg);
              end;
            finally
              FreeAndNil(frmColumnsSetConf);
            end;
          end;
    1001: //All columns
          begin
            ShowOptions(TfrmOptionsCustomColumns);
          end;
  else
    begin
      ActiveColm:=ColSet.Items[(Sender as TMenuItem).Tag];
      UpdateColumnsView;
      RedrawFiles;
    end;
  end;
end;

constructor TColumnsFileViewVTV.Create(AOwner: TWinControl; AFileSource: IFileSource; APath: String; AFlags: TFileViewFlags = []);
begin
  ActiveColm := 'Default';
  inherited Create(AOwner, AFileSource, APath, AFlags);
end;

constructor TColumnsFileViewVTV.Create(AOwner: TWinControl; AFileView: TFileView; AFlags: TFileViewFlags = []);
begin
  inherited Create(AOwner, AFileView, AFlags);
end;

constructor TColumnsFileViewVTV.Create(AOwner: TWinControl; AConfig: TIniFileEx; ASectionName: String; ATabIndex: Integer; AFlags: TFileViewFlags = []);
begin
  inherited Create(AOwner, AConfig, ASectionName, ATabIndex, AFlags);
end;

constructor TColumnsFileViewVTV.Create(AOwner: TWinControl; AConfig: TXmlConfig; ANode: TXmlNode; AFlags: TFileViewFlags = []);
begin
  inherited Create(AOwner, AConfig, ANode, AFlags);
end;

procedure TColumnsFileViewVTV.CreateDefault(AOwner: TWinControl);
begin
  DCDebug('TColumnsFileViewVTV.Create components');

  inherited CreateDefault(AOwner);

  FFileNameColumn := -1;
  FExtensionColumn := -1;

  // -- other components

  dgPanel:=TColumnsDrawTree.Create(Self, Self);
  MainControl := dgPanel;

  edtRename:=TEdit.Create(dgPanel);
  edtRename.Parent:=dgPanel;
  edtRename.Visible:=False;
  edtRename.TabStop:=False;
  edtRename.AutoSize:=False;

  tmClearGrid := TTimer.Create(Self);
  tmClearGrid.Enabled := False;
  tmClearGrid.Interval := 500;
  tmClearGrid.OnTimer := @tmClearGridTimer;

  // ---
  dgPanel.OnDragOver := @dgPanelDragOver;
  dgPanel.OnDragDrop:= @dgPanelDragDrop;
  dgPanel.OnAdvancedHeaderDraw:=@dgPanelAdvancedHeaderDraw;
  dgPanel.OnAfterItemPaint := @dgPanelAfterItemPaint;
  dgPanel.OnBeforeItemErase := @dgPanelBeforeItemErase;
  dgPanel.OnFocusChanged:=@dgPanelFocusChanged;
  dgPanel.OnFocusChanging:=@dgPanelFocusChanging;
  dgPanel.OnHeaderDrawQueryElements:=@dgPanelHeaderDrawQueryElements;
  dgPanel.OnHeaderClick:=@dgPanelHeaderClick;
  dgPanel.OnMouseWheelUp := @dgPanelMouseWheelUp;
  dgPanel.OnMouseWheelDown := @dgPanelMouseWheelDown;
  dgPanel.OnScroll:= @dgPanelScroll;
  dgpanel.OnResize:= @dgPanelResize;

  edtRename.OnKeyDown := @edtRenameKeyDown;
  edtRename.OnExit := @edtRenameExit;

  pmColumnsMenu := TPopupMenu.Create(Self);
  pmColumnsMenu.Parent := Self;
end;

destructor TColumnsFileViewVTV.Destroy;
begin
  inherited Destroy;
end;

function TColumnsFileViewVTV.Clone(NewParent: TWinControl): TColumnsFileViewVTV;
begin
  Result := TColumnsFileViewVTV.Create(NewParent, Self);
end;

procedure TColumnsFileViewVTV.CloneTo(FileView: TFileView);
begin
  if Assigned(FileView) then
  begin
    inherited CloneTo(FileView);

    with FileView as TColumnsFileViewVTV do
    begin
      FColumnsSortDirections := Self.FColumnsSortDirections;

      ActiveColm := Self.ActiveColm;
      ActiveColmSlave := nil;    // set to nil because only used in preview?
      isSlave := Self.isSlave;
    end;
  end;
end;

procedure TColumnsFileViewVTV.AddFileSource(aFileSource: IFileSource; aPath: String);
begin
  inherited AddFileSource(aFileSource, aPath);
end;

procedure TColumnsFileViewVTV.BeforeMakeFileList;
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

procedure TColumnsFileViewVTV.ClearAfterDragDrop;
begin
  inherited ClearAfterDragDrop;

  // Reset state. TODO: Check if this is needed on any widgetset.
  dgPanel.TreeStates := dgPanel.TreeStates -
    [tsLeftButtonDown, tsMiddleButtonDown, tsRightButtonDown, tsVCLDragging];
end;

procedure TColumnsFileViewVTV.AfterMakeFileList;
begin
  inherited;

  tmClearGrid.Enabled := False;
  DisplayFileListHasChanged;
  EnsureDisplayProperties; // After displaying.
end;

procedure TColumnsFileViewVTV.DisplayFileListHasChanged;
var
  AFocused: Boolean = False;
  Node: PVirtualNode;
begin
  // Update grid row count.
  SetRowCount(FFiles.Count);
  SetFilesDisplayItems;
  RedrawFiles;

  if SetActiveFileNow(RequestedActiveFile) then
    RequestedActiveFile := ''
  else
    // Requested file was not found, restore position to last active file.
    if not SetActiveFileNow(LastActiveFile) then
    begin
      if FLastActiveFileIndex >= dgPanel.RootNodeCount then
      begin
        FUpdatingActiveFile := True;
        dgPanel.FocusedNode := dgPanel.GetLastNoInit;
        FUpdatingActiveFile := False;
        SetLastActiveFile(FLastActiveFileIndex);
        AFocused := True;
      end
      else if FLastActiveFileIndex >= 0 then
      begin
        Node := dgPanel.GetFirstNoInit;
        while Assigned(Node) do
        begin
          if Node^.Index = FLastActiveFileIndex then
          begin
            FUpdatingActiveFile := True;
            dgPanel.FocusedNode := Node;
            FUpdatingActiveFile := False;
            SetLastActiveFile(Node^.Index);
            AFocused := True;
            Break;
          end;
          Node := Node^.NextSibling;
        end;
      end;
      if not AFocused then
        dgPanel.FocusedNode := dgPanel.GetFirstNoInit;
      // At creation the control has default size (100, 200).
      // If the first column is wider than ClientWidth then VTV scrolls
      // to the right edge of the column. So, we scroll back here.
      // dgPanel.OffsetX := 0;
    end;

  UpdateInfoPanel;
end;

procedure TColumnsFileViewVTV.MakeColumnsStrings(AFile: TDisplayFile);
begin
  MakeColumnsStrings(AFile, GetColumnsClass);
end;

procedure TColumnsFileViewVTV.MakeColumnsStrings(AFile: TDisplayFile; ColumnsClass: TPanelColumnsClass);
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

procedure TColumnsFileViewVTV.ClearAllColumnsStrings;
var
  i: Integer;
begin
  if Assigned(FAllDisplayFiles) then
  begin
    // Clear display strings in case columns have changed.
    for i := 0 to FAllDisplayFiles.Count - 1 do
      FAllDisplayFiles[i].DisplayStrings.Clear;
  end;
end;

procedure TColumnsFileViewVTV.EachViewUpdateColumns(AFileView: TFileView; UserData: Pointer);
var
  ColumnsView: TColumnsFileViewVTV;
  PMsg: PEachViewCallbackMsg;
begin
  if AFileView is TColumnsFileViewVTV then
  begin
    ColumnsView := TColumnsFileViewVTV(AFileView);
    PMsg := UserData;
    if ColumnsView.ActiveColm = PMsg^.UpdatedColumnsSetName then
    begin
      ColumnsView.ActiveColm := PMsg^.NewColumnsSetName;
      ColumnsView.UpdateColumnsView;
      ColumnsView.RedrawFiles;
    end;
  end;
end;

procedure TColumnsFileViewVTV.DoUpdateView;
begin
  inherited DoUpdateView;
  UpdateColumnsView;
end;

function TColumnsFileViewVTV.GetActiveFileIndex: PtrInt;
begin
  if Assigned(dgPanel.FocusedNode) then
    Result := dgPanel.FocusedNode^.Index
  else
    Result := InvalidFileIndex;
end;

function TColumnsFileViewVTV.GetColumnsClass: TPanelColumnsClass;
begin
  if isSlave then
    Result := ActiveColmSlave
  else
    Result := ColSet.GetColumnSet(ActiveColm);
end;

function TColumnsFileViewVTV.GetFileIndexFromCursor(X, Y: Integer; out AtFileList: Boolean): PtrInt;
var
  Node: PVirtualNode;
begin
  Node := dgPanel.GetNodeAt(X, Y);
  if Assigned(Node) then
    Result := Node^.Index
  else
    Result := InvalidFileIndex;
  AtFileList := Y >= dgPanel.GetHeaderHeight;
end;

procedure TColumnsFileViewVTV.DoFileUpdated(AFile: TDisplayFile; UpdatedProperties: TFilePropertiesTypes);
begin
  MakeColumnsStrings(AFile);
  inherited DoFileUpdated(AFile, UpdatedProperties);
end;

procedure TColumnsFileViewVTV.DoHandleKeyDown(var Key: Word; Shift: TShiftState);
var
  Node, NextNode: PVirtualNode;
  aFile: TDisplayFile;
begin
  case Key of
    VK_INSERT:
      begin
        if not IsEmpty then
        begin
          Node := dgPanel.FocusedNode;
          if IsActiveItemValid then
          begin
            InvertFileSelection(GetActiveDisplayFile, False);
            DoSelectionChanged(nil);
          end;
          NextNode := dgPanel.GetNextSiblingNoInit(Node);
          if (Node <> NextNode) and Assigned(NextNode) then
            dgPanel.FocusedNode := NextNode
          else
            dgPanel.InvalidateNode(Node);
        end;
        Key := 0;
      end;

    VK_LEFT:
      if (Shift = []) then
      begin
        if gLynxLike then
          ChangePathToParent(True)
        else
          dgPanel.OffsetX := dgPanel.OffsetX + 20;
        Key := 0;
      end;

    VK_RIGHT:
      if (Shift = []) then
      begin
        if gLynxLike then
          ChooseFile(GetActiveDisplayFile, True)
        else
          dgPanel.OffsetX := dgPanel.OffsetX - 20;
        Key := 0;
      end;

    VK_UP, VK_DOWN:
      begin
        if ssShift in Shift then
        begin
          Node := dgPanel.FocusedNode;
          aFile := dgPanel.GetNodeFile(Node);
          if IsItemValid(aFile) then
          begin
            InvertFileSelection(aFile, False);
            DoSelectionChanged(nil);
            if (Node = dgPanel.GetFirstNoInit) or
               (Node = dgPanel.GetLastNoInit) then
            begin
              dgPanel.InvalidateNode(Node);
            end;
            //Key := 0; // not needed!
          end;
        end
{$IFDEF LCLGTK2}
        else
        begin
          Node := dgPanel.FocusedNode;
          if ((Node = dgPanel.GetLastNoInit) and (Key = VK_DOWN))
          or ((Node = dgPanel.GetFirstNoInit) and (Key = VK_UP)) then
            Key := 0;
        end;
{$ENDIF}
      end;

    VK_SPACE:
      if Shift * KeyModifiersShortcut = [] then
      begin
        Node := dgPanel.FocusedNode;
        if Assigned(Node) then
        begin
          aFile := dgPanel.GetNodeFile(Node);
          if IsItemValid(aFile) then
          begin
            if (aFile.FSFile.IsDirectory or
               aFile.FSFile.IsLinkToDirectory) and
               not aFile.Selected then
            begin
              CalculateSpace(aFile);
            end;

            InvertFileSelection(aFile, False);
            DoSelectionChanged(nil);
          end;

          if gSpaceMovesDown then
          begin
            NextNode := dgPanel.GetNextSiblingNoInit(Node);
            if (Node <> NextNode) and Assigned(NextNode) then
              dgPanel.FocusedNode := NextNode
            else
              dgPanel.InvalidateNode(Node);
          end
          else
            dgPanel.InvalidateNode(Node);
          Key := 0;
        end;
      end;
  end;

  inherited DoHandleKeyDown(Key, Shift);
end;

procedure TColumnsFileViewVTV.DoMainControlShowHint(FileIndex: PtrInt; X, Y: Integer);
var
  aRect: TRect;
  iCol: Integer;
  AFile: TDisplayFile;
begin
  AFile := FFiles[FileIndex];
  aRect:= dgPanel.GetDisplayRect(PVirtualNode(AFile.DisplayItem), 0, False);
  iCol:= aRect.Right - aRect.Left - 8;
  if gShowIcons <> sim_none then
    Dec(iCol, gIconsSize);
  if iCol < dgPanel.Canvas.TextWidth(AFile.FSFile.Name) then // with file name
    dgPanel.Hint:= AFile.FSFile.Name
  else if (stm_only_large_name in gShowToolTipMode) then // don't show
    Exit
  else if not AFile.FSFile.IsDirectory then // without name
    dgPanel.Hint:= #32;
end;

procedure TColumnsFileViewVTV.DoSelectionChanged(Node: PVirtualNode);
begin
  if Assigned(Node) then
    DoSelectionChanged(Node^.Index)
  else
    DoSelectionChanged(-1);
end;

procedure TColumnsFileViewVTV.cm_RenameOnly(const Params: array of string);
var
  aFile: TFile;
begin
  if (fsoSetFileProperty in FileSource.GetOperationsTypes) then
    begin
      aFile:= CloneActiveFile;
      if Assigned(aFile) then
      try
        if aFile.IsNameValid then
          ShowRenameFileEdit(aFile)
        else
          ShowPathEdit;
      finally
        FreeAndNil(aFile);
      end;
    end;
end;

{ TColumnsDrawTree }

constructor TColumnsDrawTree.Create(AOwner: TComponent; AParent: TWinControl);
begin
  inherited Create(AOwner);

  Self.Parent := AParent;
  ColumnsView := AParent as TColumnsFileViewVTV;

  DragType := dtVCL;
  HintMode := hmHint;
end;

procedure TColumnsDrawTree.AfterConstruction;
begin
  inherited;

  RootNodeCount := 0;
  NodeDataSize := SizeOf(TColumnsDrawTreeRecord);

  Align := alClient;

  TreeOptions.AutoOptions := [toAutoScroll, toDisableAutoscrollHorizontal];
  TreeOptions.MiscOptions := [toFullRowDrag];
  // TODO: if no mouse action for middle button: include toWheelPanning
  TreeOptions.PaintOptions := [toShowBackground,
    toThemeAware, toUseBlendedImages, toGhostedIfUnfocused, toStaticBackground,
    toAlwaysHideSelection, toHideFocusRect];
  TreeOptions.SelectionOptions := [toDisableDrawSelection, toExtendedFocus,
    toFullRowSelect];

  TabStop := False;
  Margin := 0;
  TextMargin := 0;
  Indent := 0;
  AnimationDuration := 0;

  UpdateView;
end;

procedure TColumnsDrawTree.UpdateView;

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
    with (Parent as TColumnsFileViewVTV) do
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
    Result      := Canvas.TextHeight('Wg');
    Canvas.Font := OldFont;
  end;

var
  TabHeaderHeight: Integer;
  TempRowHeight: Integer;
  Node: PVirtualNode;
begin
  BeginUpdate;

  try
    if gInterfaceFlat then
    begin
      Header.Style := hsPlates;
      BorderStyle := bsNone;
      BorderWidth := 0;
    end
    else
      Header.Style := hsFlatButtons;

    GridVertLine:= gGridVertLine;
    GridHorzLine:= gGridHorzLine;

    // Calculate row height.
    TempRowHeight := CalculateDefaultRowHeight;
    if TempRowHeight > 0 then
    begin
      DefaultNodeHeight := TempRowHeight;

      // Set each node's height if changed.
      Node := GetFirstNoInit;
      if Assigned(Node) and (NodeHeight[Node] <> TempRowHeight) then
        SetAllRowsHeights(TempRowHeight);
    end;

    // Add additional space at the bottom so that the filelist doesn't jump at the end.
    // It happens when ClientHeight is not an exact multiplication of DefaultNodeHeight.
    BottomSpace := ClientHeight mod DefaultNodeHeight;

    Header.Options := [hoColumnResize, hoDblClickResize, hoDisableAnimatedResize,
      hoOwnerDraw];

    // Set rows of header.
    if gTabHeader then
    begin
      Header.Options := Header.Options + [hoVisible];

      TabHeaderHeight := Max(gIconsSize, CalculateTabHeaderHeight);
      TabHeaderHeight := TabHeaderHeight + 2; // for borders
      if not gInterfaceFlat then
      begin
        TabHeaderHeight := TabHeaderHeight + 2; // additional borders if not flat
      end;
      Header.DefaultHeight := TabHeaderHeight;
    end;

    if gAutoFillColumns then
      Header.Options := Header.Options + [hoAutoResize, hoAutoSpring];

  finally
    EndUpdate;
  end;
end;

procedure TColumnsDrawTree.InitializeWnd;
begin
  inherited InitializeWnd;
  ColumnsView.InitializeDragDropEx(Self);
end;

procedure TColumnsDrawTree.FinalizeWnd;
begin
  ColumnsView.FinalizeDragDropEx(Self);
  inherited FinalizeWnd;
end;

procedure TColumnsDrawTree.DoPaintNode(var PaintInfo: TVTPaintInfo);
var
  //shared variables
  s:   string;
  iTextTop: Integer;
  AFile: TDisplayFile;
  FileSourceDirectAccess: Boolean;
  ColumnsSet: TPanelColumnsClass;
  IsFocused: Boolean;
  aRect: TRect;
  aCol: TColumnIndex;

  //------------------------------------------------------
  //begin subprocedures
  //------------------------------------------------------

  procedure DrawIconCell;
  //------------------------------------------------------
  var
    Y: Integer;
    IconID: PtrInt;
    oldClipping: Boolean;
  begin
    if (gShowIcons <> sim_none) then
    begin
      IconID := AFile.IconID;
      // Draw default icon if there is no icon for the file.
      if IconID = -1 then
        IconID := PixMapManager.GetDefaultIcon(AFile.FSFile);

      // center icon vertically
      Y:= aRect.Top + (PaintInfo.Node^.NodeHeight - gIconsSize) div 2;

      // Draw icon for a file
      PixMapManager.DrawBitmap(IconID,
                               PaintInfo.Canvas,
                               aRect.Left + 1,
                               Y
                               );

      // Draw overlay icon for a file if needed
      if gIconOverlays then
      begin
        PixMapManager.DrawBitmapOverlay(AFile,
                                        FileSourceDirectAccess,
                                        PaintInfo.Canvas,
                                        aRect.Left + 1,
                                        Y
                                        );
      end;

    end;

    s := AFile.DisplayStrings.Strings[ACol];

    if gCutTextToColWidth then
    begin
      Y:= ((aRect.Right - aRect.Left) - 4 - PaintInfo.Canvas.TextWidth('W'));
      if (gShowIcons <> sim_none) then Y:= Y - gIconsSize;
      if PaintInfo.Canvas.TextWidth(s) - Y > 0 then
      begin
        repeat
          IconID:= UTF8Length(s);
          UTF8Delete(s, IconID, 1);
        until (PaintInfo.Canvas.TextWidth(s) - Y < 1) or (IconID = 0);
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

    oldClipping := PaintInfo.Canvas.Clipping;
    //PaintInfo.Canvas.Clipping := False;

    if (gShowIcons <> sim_none) then
      PaintInfo.Canvas.TextOut(aRect.Left + gIconsSize + 4, iTextTop, s)
    else
      PaintInfo.Canvas.TextOut(aRect.Left + 2, iTextTop, s);

//    PaintInfo.Canvas.Clipping := oldClipping;
  end; //of DrawIconCell
  //------------------------------------------------------

  procedure DrawOtherCell;
  //------------------------------------------------------
  var
    tw, cw: Integer;
    oldClipping: Boolean;
  begin
    s := AFile.DisplayStrings.Strings[ACol];

    if gCutTextToColWidth then
    begin
      while PaintInfo.Canvas.TextWidth(s) - (aRect.Right - aRect.Left) - 4 > 0 do
        Delete(s, Length(s), 1);
    end;

    oldClipping := PaintInfo.Canvas.Clipping;
    //PaintInfo.Canvas.Clipping := False;

    case ColumnsSet.GetColumnAlign(ACol) of

      taRightJustify:
        begin
          cw := Header.Columns.Items[ACol].Width;
          tw := PaintInfo.Canvas.TextWidth(s);
          PaintInfo.Canvas.TextOut(aRect.Right - tw - 3, iTextTop, s);
        end;

      taLeftJustify:
        begin
          PaintInfo.Canvas.TextOut(aRect.Left + 3, iTextTop, s);
        end;

      taCenter:
        begin
          cw := Header.Columns.Items[ACol].Width;
          tw := PaintInfo.Canvas.TextWidth(s);
          PaintInfo.Canvas.TextOut(aRect.Left + ((cw - tw - 3) div 2), iTextTop, s);
        end;

    end; //of case

//    PaintInfo.Canvas.Clipping := oldClipping;
  end; //of DrawOtherCell
  //------------------------------------------------------

  procedure PrepareColors;
  //------------------------------------------------------
  var
    TextColor: TColor = -1;
    BackgroundColor: TColor;
    IsCursor: Boolean;
  //---------------------
  begin
    PaintInfo.Canvas.Font.Name   := ColumnsSet.GetColumnFontName(ACol);
    PaintInfo.Canvas.Font.Size   := ColumnsSet.GetColumnFontSize(ACol);
    PaintInfo.Canvas.Font.Style  := ColumnsSet.GetColumnFontStyle(ACol);

    IsCursor := IsFocused and ColumnsView.Active and (not gUseFrameCursor);
    // Set up default background color first.
    if IsCursor then
      begin
        BackgroundColor := ColumnsSet.GetColumnCursorColor(ACol);
      end
    else
      begin
        // Alternate rows background color.
        if odd(PaintInfo.Node^.Index) then
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
          if IsCursor then
            begin
              TextColor := InvertColor(ColumnsSet.GetColumnCursorText(ACol));
            end
          else
            begin
              BackgroundColor := ColumnsSet.GetColumnMarkColor(ACol);
              TextColor := ColumnsSet.GetColumnBackground(ACol);
            end;
          //------------------------------------------------------
        end
      else
        begin
          TextColor := ColumnsSet.GetColumnMarkColor(ACol);
        end;
    end
    else if IsCursor then
      begin
        TextColor := ColumnsSet.GetColumnCursorText(ACol);
      end;

    BackgroundColor := ColumnsView.DimColor(BackgroundColor);

    if AFile.RecentlyUpdatedPct <> 0 then
    begin
      TextColor := LightColor(TextColor, AFile.RecentlyUpdatedPct);
      BackgroundColor := LightColor(BackgroundColor, AFile.RecentlyUpdatedPct);
    end;

    // Draw background.
    PaintInfo.Canvas.Brush.Color := BackgroundColor;
    PaintInfo.Canvas.FillRect(aRect);
    PaintInfo.Canvas.Font.Color := TextColor;
  end;// of PrepareColors;

  procedure DrawLines;
  begin
    // Draw frame cursor.
    if gUseFrameCursor and IsFocused and ColumnsView.Active then
    begin
      PaintInfo.Canvas.Pen.Color := ColumnsSet.GetColumnCursorColor(ACol);
      PaintInfo.Canvas.Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
      PaintInfo.Canvas.Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
    end;

    // Draw drop selection.
    if PaintInfo.Node^.Index = ColumnsView.FDropFileIndex then
    begin
      PaintInfo.Canvas.Pen.Color := ColumnsSet.GetColumnTextColor(ACol);
      PaintInfo.Canvas.Line(aRect.Left, aRect.Top + 1, aRect.Right, aRect.Top + 1);
      PaintInfo.Canvas.Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
    end;
  end;
  //------------------------------------------------------
  //end of subprocedures
  //------------------------------------------------------

begin
  aRect := PaintInfo.ContentRect;
  aCol := PaintInfo.Column;

  AFile := GetNodeFile(PaintInfo.Node);
  if not Assigned(AFile) then
  begin
    PaintInfo.Canvas.Brush.Color := Self.Color;
    PaintInfo.Canvas.FillRect(aRect);
    Exit;
  end;

  ColumnsSet := ColumnsView.GetColumnsClass;
  FileSourceDirectAccess := fspDirectAccess in ColumnsView.FileSource.Properties;
  if AFile.DisplayStrings.Count = 0 then
    ColumnsView.MakeColumnsStrings(AFile, ColumnsSet);

  IsFocused := PaintInfo.Node = FocusedNode;

  PrepareColors;

  // Paint on next column if it is empty.
  {if (ColumnsSet.GetColumnAlign(ACol) = taLeftJustify) and
     (ACol + 1 < ColumnsSet.ColumnsCount) and
     (AFile.DisplayStrings[ACol + 1] = EmptyStr) then
    aRect.Right := aRect.Right + Header.Columns.Items[ACol + 1].Width;

  // Paint on previous column if it is empty.
  if (ColumnsSet.GetColumnAlign(ACol) = taRightJustify) and
     (ACol - 1 >= 0) and
     (AFile.DisplayStrings[ACol - 1] = EmptyStr) then
    aRect.Left := aRect.Left - Header.Columns.Items[ACol - 1].Width;
  }
  iTextTop := aRect.Top + (PaintInfo.Node^.NodeHeight - PaintInfo.Canvas.TextHeight('Wg')) div 2;

  if PaintInfo.Column = 0 then
    DrawIconCell  // Draw icon in the first column
  else
    DrawOtherCell;

  DrawLines;
end;

function TColumnsDrawTree.GetNodeFile(Node: PVirtualNode): TDisplayFile;
begin
  if InRange(Node^.Index, 0, ColumnsView.FFiles.Count-1) then
    Result := ColumnsView.FFiles[Node^.Index]
  else
    Result := nil;
end;

procedure TColumnsDrawTree.SetAllRowsHeights(ARowHeight: Cardinal);
var
  Node: PVirtualNode;
begin
  BeginUpdate;
  try
    Node := GetFirstNoInit;
    while Assigned(Node) do
    begin
      NodeHeight[Node] := ARowHeight;
      Node := Node^.NextSibling;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TColumnsDrawTree.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  BackgroundClick: Boolean;
  Point: TPoint;
begin
{$IFDEF LCLGTK2}
  // Workaround for two doubleclicks being sent on GTK.
  // MouseUp event is sent just after doubleclick, so if we drop
  // doubleclick events we have to also drop MouseUp events that follow them.
  if TooManyDoubleClicks then Exit;
{$ENDIF}

  // Handle only if button-up was not lifted to finish drag&drop operation.
  if not ColumnsView.FMainControlMouseDown then
    Exit;

  inherited MouseUp(Button, Shift, X, Y);

  ColumnsView.FMainControlMouseDown := False;

  if Button = mbRight then
    begin
      { If right click on file/directory }
      if ((gMouseSelectionButton<>1) or not gMouseSelectionEnabled) then
        begin
          BackgroundClick:= not MouseOnGrid(X, Y);
          Point := ClientToScreen(Classes.Point(X, Y));
          frmMain.Commands.DoContextMenu(ColumnsView, Point.x, Point.y, BackgroundClick);
        end
      else if (gMouseSelectionEnabled and (gMouseSelectionButton = 1)) then
        begin
          ColumnsView.tmContextMenu.Enabled:= False; // stop context menu timer
        end;
    end
  { Open folder in new tab on middle click }
  else if (Button = mbMiddle) and (Y > GetHeaderHeight) then
    begin
      frmMain.Commands.cm_OpenDirInNewTab([]);
    end;
end;

procedure TColumnsDrawTree.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
{$IFDEF LCLGTK2}
  // Workaround for two doubleclicks being sent on GTK.
  // MouseDown event is sent just before doubleclick, so if we drop
  // doubleclick events we have to also drop MouseDown events that precede them.
  if TooManyDoubleClicks then Exit;
{$ENDIF}

  ColumnsView.FMainControlMouseDown := True;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TColumnsDrawTree.KeyDown(var Key: Word; Shift: TShiftState);
var
  Node, Temp: PVirtualNode;
  Offset: Integer;
begin
  // Override scrolling with PageUp, PageDown because VirtualTreeView scrolls too much.
  case Key of
    VK_PRIOR:
      if Shift = [] then
      begin
        Offset := 0;
        // If there's no focused node then just take the very first one.
        if FocusedNode = nil then
          Node := GetFirstNoInit
        else
        begin
          // Go up as many nodes as comprise together a size of ClientHeight.
          Node := FocusedNode;
          Temp := Node;
          while Assigned(Temp) do
          begin
            Inc(Offset, NodeHeight[Temp]);
            if Offset >= ClientHeight then
              Break;
            Node := Temp;
            Temp := GetPreviousSiblingNoInit(Temp);
          end;
        end;
        FocusedNode := Node;
        Key := 0;
      end;

    VK_NEXT:
      if Shift = [] then
      begin
        Offset := 0;
        // If there's no focused node then just take the very last one.
        if FocusedNode = nil then
          Node := GetLastNoInit
        else
        begin
          // Go down as many nodes as comprise together a size of ClientHeight.
          Node := FocusedNode;
          Temp := Node;
          while Assigned(Temp) do
          begin
            Inc(Offset, NodeHeight[Temp]);
            if Offset >= ClientHeight then
              Break;
            Node := Temp;
            Temp := GetNextSiblingNoInit(Temp);
          end;
        end;
        FocusedNode := Node;
        //if OffsetY mod DefaultNodeHeight <> 0 then
        //  OffsetY := OffsetY - ClientHeight mod DefaultNodeHeight;
        Key := 0;
      end;
  end;

  inherited KeyDown(Key, Shift);
end;

function TColumnsDrawTree.MouseOnGrid(X, Y: LongInt): Boolean;
begin
  Result := Assigned(GetNodeAt(X, Y));
end;

function TColumnsDrawTree.GetHeaderHeight: Integer;
begin
  Result := Header.Height;
end;

function TColumnsDrawTree.GetGridHorzLine: Boolean;
begin
  Result := toShowHorzGridLines in TreeOptions.PaintOptions;
end;

function TColumnsDrawTree.GetGridVertLine: Boolean;
begin
  Result := toShowVertGridLines in TreeOptions.PaintOptions;
end;

procedure TColumnsDrawTree.SetGridHorzLine(const AValue: Boolean);
begin
  if AValue then
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowHorzGridLines]
  else
    TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowHorzGridLines];
end;

procedure TColumnsDrawTree.SetGridVertLine(const AValue: Boolean);
begin
  if AValue then
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowVertGridLines]
  else
    TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowVertGridLines];
end;

function TColumnsDrawTree.GetVisibleNodes: TNodeRange;
var
  Offset: Integer = 0;
  Node: PVirtualNode;
  CH: Integer;
begin
  Result.First := GetNodeAt(0, 0, True, Offset);
  Result.Last := Result.First;
  CH := ClientHeight;

  // Go down as many nodes as comprise together a size of ClientHeight.
  if Assigned(Result.Last) then
  begin
    while True do
    begin
      if Offset >= CH then
        Break;
      Node := GetNextSiblingNoInit(Result.Last);
      if not Assigned(Node) then
        Break;
      Result.Last := Node;
      Inc(Offset, NodeHeight[Node]);
    end;
  end;
end;

function TColumnsDrawTree.GetVisibleIndexes: TRange;
begin
  if csLoading in ComponentState then
  begin
    Result.First := 0;
    Result.Last  := -1;
  end
  else
  begin
    // This assumes each row has the same height = DefaultNodeHeight.
    Result.First := -OffsetY div DefaultNodeHeight;
    Result.Last  := (-OffsetY + ClientHeight) div DefaultNodeHeight;
    // Account for the fact the BottomSpace might be > 0.
    if Result.Last >= RootNodeCount then
      Result.Last := RootNodeCount - 1;
  end;
end;

end.

