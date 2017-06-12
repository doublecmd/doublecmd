unit uColumnsFileView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Forms, ExtCtrls, Grids,
  LMessages, LCLIntf, LCLType, Menus, LCLVersion,
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
  uTypes,
  uFileViewWithGrid;

type
  TFunctionDime = function (AColor: TColor): TColor of Object;

  TColumnsSortDirections = array of TSortDirection;
  TColumnsFileView = class;

  { TDrawGridEx }

  TDrawGridEx = class(TDrawGrid)
  private
    ColumnsView: TColumnsFileView;

    function GetGridHorzLine: Boolean;
    function GetGridVertLine: Boolean;
    procedure SetGridHorzLine(const AValue: Boolean);
    procedure SetGridVertLine(const AValue: Boolean);

  protected
    {$IF lcl_fullversion < 1090000}
    function SelectCell(aCol, aRow: Integer): Boolean; override;
    {$ENDIF}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;

    procedure InitializeWnd; override;
    procedure FinalizeWnd; override;

    procedure DrawColumnText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState); override;

    procedure DrawCell(aCol, aRow: Integer; aRect: TRect;
              aState: TGridDrawState); override;

    {$if lcl_fullversion >= 1070000}
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
                const AXProportion, AYProportion: Double); override;
    {$endif}
  public
    ColumnsOwnDim: TFunctionDime;

    constructor Create(AOwner: TComponent; AParent: TWinControl); reintroduce;

    procedure UpdateView;

    function MouseOnGrid(X, Y: LongInt): Boolean;

    // Returns height of all the header rows.
    function GetHeaderHeight: Integer;

    // Adapted from TCustomGrid.GetVisibleGrid only for visible rows.
    function GetVisibleRows: TRange;
    {en
       Retrieves first and last fully visible row number.
    }
    function GetFullVisibleRows: TRange;

    function IsRowVisible(aRow: Integer): Boolean;
    procedure ScrollHorizontally(ForwardDirection: Boolean);

    property GridVertLine: Boolean read GetGridVertLine write SetGridVertLine;
    property GridHorzLine: Boolean read GetGridHorzLine write SetGridHorzLine;

  end;

  TColumnResized = procedure (Sender: TObject; ColumnIndex: Integer; ColumnNewsize: integer) of object;

  { TColumnsFileView }

  TColumnsFileView = class(TFileViewWithMainCtrl)

  private
    FColumnsSortDirections: TColumnsSortDirections;
    FFileNameColumn: Integer;
    FExtensionColumn: Integer;

    pmColumnsMenu: TPopupMenu;
    dgPanel: TDrawGridEx;
    FOnColumnResized: TColumnResized;

    function GetColumnsClass: TPanelColumnsClass;

    procedure SetRowCount(Count: Integer);
    procedure SetFilesDisplayItems;
    procedure SetColumns;

    procedure MakeVisible(iRow: Integer);
    procedure MakeActiveVisible;

    {en
       Format and cache all columns strings.
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

    // -- Events --------------------------------------------------------------

{$IF lcl_fullversion >= 093100}
    procedure dgPanelBeforeSelection(Sender: TObject; aCol, aRow: Integer);
{$ENDIF}
    procedure dgPanelHeaderClick(Sender: TObject;IsColumn: Boolean; index: Integer);
    procedure dgPanelMouseWheelUp(Sender: TObject; Shift: TShiftState;
                                  MousePos: TPoint; var Handled: Boolean);
    procedure dgPanelMouseWheelDown(Sender: TObject; Shift: TShiftState;
                                  MousePos: TPoint; var Handled: Boolean);
    procedure dgPanelSelection(Sender: TObject; aCol, aRow: Integer);
    procedure dgPanelTopLeftChanged(Sender: TObject);
    procedure dgPanelResize(Sender: TObject);
    procedure dgPanelHeaderSized(Sender: TObject; IsColumn: Boolean; index: Integer);
    procedure ColumnsMenuClick(Sender: TObject);

  protected
    procedure CreateDefault(AOwner: TWinControl); override;

    procedure BeforeMakeFileList; override;
    procedure ClearAfterDragDrop; override;
    procedure DisplayFileListChanged; override;
    procedure DoColumnResized(Sender: TObject; ColumnIndex: Integer; ColumnNewSize: Integer);
    procedure DoFileUpdated(AFile: TDisplayFile; UpdatedProperties: TFilePropertiesTypes = []); override;
    procedure DoHandleKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoUpdateView; override;
    procedure FileSourceFileListLoaded; override;
    function GetActiveFileIndex: PtrInt; override;
    function GetFileIndexFromCursor(X, Y: Integer; out AtFileList: Boolean): PtrInt; override;
    function GetFileRect(FileIndex: PtrInt): TRect; override;
    function GetVisibleFilesIndexes: TRange; override;
    procedure RedrawFile(FileIndex: PtrInt); override;
    procedure RedrawFile(DisplayFile: TDisplayFile); override;
    procedure RedrawFiles; override;
    procedure SetActiveFile(FileIndex: PtrInt); override;
    procedure SetSorting(const NewSortings: TFileSortings); override;
    procedure ShowRenameFileEdit(aFile: TFile); override;
    procedure UpdateRenameFileEditPosition; override;

    procedure AfterChangePath; override;

  public
    ActiveColm: String;
    ActiveColmSlave: TPanelColumnsClass;
    isSlave:boolean;
//---------------------

    constructor Create(AOwner: TWinControl; AFileSource: IFileSource; APath: String; AFlags: TFileViewFlags = []); override;
    constructor Create(AOwner: TWinControl; AFileView: TFileView; AFlags: TFileViewFlags = []); override;
    constructor Create(AOwner: TWinControl; AConfig: TXmlConfig; ANode: TXmlNode; AFlags: TFileViewFlags = []); override;

    destructor Destroy; override;

    function Clone(NewParent: TWinControl): TColumnsFileView; override;
    procedure CloneTo(FileView: TFileView); override;

    procedure AddFileSource(aFileSource: IFileSource; aPath: String); override;

    procedure LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); override;
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode; ASaveHistory:boolean); override;

    procedure UpdateColumnsView;
    procedure SetGridFunctionDim(ExternalDimFunction:TFunctionDime);

    property OnColumnResized: TColumnResized read FOnColumnResized write FOnColumnResized;
  published
    procedure cm_CopyFileDetailsToClip(const Params: array of string);

  end;

implementation

uses
  LCLProc, Buttons, Clipbrd, DCStrUtils, uLng, uGlobs, uPixmapManager, uDebug,
  uDCUtils, math, fMain, fOptions, uClipboard,
  uOrderedFileView,
  uFileSourceProperty,
  uKeyboard,
  uFileFunctions,
  uFormCommands,
  uFileViewNotebook,
  fOptionsCustomColumns;

type
  TEachViewCallbackReason = (evcrUpdateColumns);
  TEachViewCallbackMsg = record
    Reason: TEachViewCallbackReason;
    UpdatedColumnsSetName: String;
    NewColumnsSetName: String; // If columns name renamed
  end;
  PEachViewCallbackMsg = ^TEachViewCallbackMsg;

procedure TColumnsFileView.SetSorting(const NewSortings: TFileSortings);
begin
  inherited SetSorting(NewSortings);
  SetColumnsSortDirections;
end;

procedure TColumnsFileView.LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
var
  ColumnsClass: TPanelColumnsClass;
  SortColumn: Integer;
  SortDirection: TSortDirection;
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
            SortDirection := TSortDirection(AConfig.GetValue(ANode, 'Direction', Integer(sdNone)));
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

procedure TColumnsFileView.SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode; ASaveHistory:boolean);
begin
  inherited SaveConfiguration(AConfig, ANode, ASaveHistory);

  AConfig.SetAttr(ANode, 'Type', 'columns');

  ANode := AConfig.FindNode(ANode, 'ColumnsView', True);
  AConfig.ClearNode(ANode);

  AConfig.SetValue(ANode, 'ColumnsSet', ActiveColm);
end;

procedure TColumnsFileView.dgPanelHeaderClick(Sender: TObject;
  IsColumn: Boolean; index: Integer);
var
  ShiftState : TShiftState;
  SortingDirection : TSortDirection;
  ColumnsClass: TPanelColumnsClass;
  Column: TPanelColumn;
  NewSorting: TFileSortings;
  SortFunctions: TFileFunctions;
begin
  if (not IsColumn) or (not gTabHeader) then Exit;

  ColumnsClass := GetColumnsClass;
  Column := ColumnsClass.GetColumnItem(Index);
  if Assigned(Column) then
  begin
    NewSorting := Sorting;
    SortFunctions := Column.GetColumnFunctions;
    if Length(SortFunctions) = 0 then Exit;
    ShiftState := GetKeyShiftStateEx;
    if [ssShift, ssCtrl] * ShiftState = [] then
    begin
      SortingDirection := GetSortDirection(NewSorting, SortFunctions);
      if SortingDirection = sdNone then
        begin
          // If there is no direction currently, sort "sdDescending" for size and date.
          // Commonly, we search seek more often for most recent files then older any others.
          // When sorting by size, often it is to find larger file to make room.
          // Anyway, it makes DC like TC, and also, Windows Explorer do the same.
          case SortFunctions[0] of
            fsfSize, fsfModificationTime, fsfCreationTime, fsfLastAccessTime: SortingDirection:=sdDescending;
            else SortingDirection:=sdAscending;
          end;
        end
      else
        begin
          SortingDirection := ReverseSortDirection(SortingDirection);
        end;
      NewSorting := nil;
    end
    else
    begin
      // If there is no direction currently, sort "sdDescending" for size and date (see previous comment).
      case SortFunctions[0] of
        fsfSize, fsfModificationTime, fsfCreationTime, fsfLastAccessTime: SortingDirection:=sdDescending;
        else SortingDirection:=sdAscending;
      end;
    end;

    AddOrUpdateSorting(NewSorting, SortFunctions, SortingDirection);
    SetSorting(NewSorting);
  end;
end;

procedure TColumnsFileView.dgPanelMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  I: Integer;
begin
  Handled:= True;
  if not IsLoadingFileList then
  begin

    if (Shift=[ssCtrl])and(gFonts[dcfMain].Size<MAX_FONT_SIZE_MAIN) then
    begin
      gFonts[dcfMain].Size:=gFonts[dcfMain].Size+1;
      frmMain.FrameLeft.UpdateView;
      frmMain.FrameRight.UpdateView;
      Handled:=True;
      Exit;
    end;


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
end;

procedure TColumnsFileView.dgPanelMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
var
  I: Integer;
begin
  Handled:= True;
  if not IsLoadingFileList then
  begin

    if (Shift=[ssCtrl])and(gFonts[dcfMain].Size>MIN_FONT_SIZE_MAIN) then
    begin
      gFonts[dcfMain].Size:=gFonts[dcfMain].Size-1;
      frmMain.FrameLeft.UpdateView;
      frmMain.FrameRight.UpdateView;
      Handled:=True;
      Exit;
    end;

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
end;

procedure TColumnsFileView.dgPanelSelection(Sender: TObject; aCol, aRow: Integer);
begin
{$IF lcl_fullversion >= 093100}
  dgPanel.Options := dgPanel.Options - [goDontScrollPartCell];
{$ENDIF}
  DoFileIndexChanged(aRow - dgPanel.FixedRows);
end;

procedure TColumnsFileView.dgPanelTopLeftChanged(Sender: TObject);
begin
  Notify([fvnVisibleFilePropertiesChanged]);
end;

procedure TColumnsFileView.dgPanelResize(Sender: TObject);
begin
  Notify([fvnVisibleFilePropertiesChanged]);
end;

procedure TColumnsFileView.AfterChangePath;
begin
  inherited AfterChangePath;

  if not IsLoadingFileList then
  begin
    FUpdatingActiveFile := True;
    dgPanel.Row := 0;
    FUpdatingActiveFile := False;
  end;
end;

procedure TColumnsfileView.SetGridFunctionDim(ExternalDimFunction:TFunctionDime);
begin
  dgPanel.ColumnsOwnDim:=ExternalDimFunction;
end;

procedure TColumnsFileView.ShowRenameFileEdit(aFile: TFile);
begin
  if FFileNameColumn <> -1 then
  begin
    if not edtRename.Visible then
    begin
      edtRename.Font.Name  := GetColumnsClass.GetColumnFontName(FFileNameColumn);
      edtRename.Font.Size  := GetColumnsClass.GetColumnFontSize(FFileNameColumn);
      edtRename.Font.Style := GetColumnsClass.GetColumnFontStyle(FFileNameColumn);

      UpdateRenameFileEditPosition;
    end;

    inherited ShowRenameFileEdit(AFile);
  end;
end;

procedure TColumnsFileView.UpdateRenameFileEditPosition;
var
  ARect: TRect;
begin
  ARect := dgPanel.CellRect(FFileNameColumn, dgPanel.Row);
  Dec(ARect.Top, 2);
  Inc(ARect.Bottom, 2);

  if (gShowIcons <> sim_none) and (FFileNameColumn = 0) then
    Inc(ARect.Left, gIconsSize + 2);

  if Succ(FFileNameColumn) = FExtensionColumn then
    Inc(ARect.Right, dgPanel.ColWidths[FExtensionColumn]);

  edtRename.SetBounds(ARect.Left, ARect.Top, ARect.Width, ARect.Height);
end;

procedure TColumnsFileView.RedrawFile(FileIndex: PtrInt);
begin
  dgPanel.InvalidateRow(FileIndex + dgPanel.FixedRows);
end;

procedure TColumnsFileView.SetColumnsSortDirections;
var
  Columns: TPanelColumnsClass;

  function SetSortDirection(ASortFunction: TFileFunction; ASortDirection: TSortDirection; Overwrite: Boolean): Boolean;
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

procedure TColumnsFileView.SetFilesDisplayItems;
var
  i: Integer;
begin
  for i := 0 to FFiles.Count - 1 do
    FFiles[i].DisplayItem := Pointer(i + dgPanel.FixedRows);
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

function TColumnsFileView.GetFileRect(FileIndex: PtrInt): TRect;
begin
  Result := dgPanel.CellRect(0, FileIndex + dgPanel.FixedRows);
end;

procedure TColumnsFileView.SetRowCount(Count: Integer);
begin
  FUpdatingActiveFile := True;
  dgPanel.RowCount := dgPanel.FixedRows + Count;
  FUpdatingActiveFile := False;
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

procedure TColumnsFileView.MakeVisible(iRow:Integer);
var
  AVisibleRows: TRange;
begin
  with dgPanel do
  begin
    AVisibleRows := GetFullVisibleRows;
    if iRow < AVisibleRows.First then
      TopRow := AVisibleRows.First;
    if iRow > AVisibleRows.Last then
      TopRow := iRow - (AVisibleRows.Last - AVisibleRows.First);
  end;
end;

procedure TColumnsFileView.MakeActiveVisible;
begin
  if dgPanel.Row>=0 then
    MakeVisible(dgPanel.Row);
end;

procedure TColumnsFileView.SetActiveFile(FileIndex: PtrInt);
begin
  dgPanel.Row := FileIndex + dgPanel.FixedRows;
  MakeVisible(dgPanel.Row);
end;

{$IF lcl_fullversion >= 093100}
procedure TColumnsFileView.dgPanelBeforeSelection(Sender: TObject; aCol, aRow: Integer);
begin
  if dgPanel.IsRowVisible(aRow) then
    dgPanel.Options := dgPanel.Options + [goDontScrollPartCell];
end;
{$ENDIF}

procedure TColumnsFileView.RedrawFile(DisplayFile: TDisplayFile);
begin
  dgPanel.InvalidateRow(PtrInt(DisplayFile.DisplayItem));
end;

procedure TColumnsFileView.RedrawFiles;
begin
  dgPanel.Invalidate;
end;

procedure TColumnsFileView.UpdateColumnsView;
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

  dgPanel.FocusRectVisible := ColumnsClass.UseCursorBorder and not ColumnsClass.UseFrameCursor;
  dgPanel.FocusColor := ColumnsClass.CursorBorderColor;
  dgPanel.UpdateView;

  OldFilePropertiesNeeded := FilePropertiesNeeded;
  FilePropertiesNeeded := GetFilePropertiesNeeded;
  if FilePropertiesNeeded >= OldFilePropertiesNeeded then
  begin
    Notify([fvnVisibleFilePropertiesChanged]);
  end;
end;

procedure TColumnsFileView.ColumnsMenuClick(Sender: TObject);
begin
  Case (Sender as TMenuItem).Tag of
    1001: //All columns, but current one will be selected.
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

constructor TColumnsFileView.Create(AOwner: TWinControl; AFileSource: IFileSource; APath: String; AFlags: TFileViewFlags = []);
begin
  ActiveColm := 'Default';
  FOnColumnResized := nil;
  inherited Create(AOwner, AFileSource, APath, AFlags);
end;

constructor TColumnsFileView.Create(AOwner: TWinControl; AFileView: TFileView; AFlags: TFileViewFlags = []);
begin
  inherited Create(AOwner, AFileView, AFlags);
end;

constructor TColumnsFileView.Create(AOwner: TWinControl; AConfig: TXmlConfig; ANode: TXmlNode; AFlags: TFileViewFlags = []);
begin
  inherited Create(AOwner, AConfig, ANode, AFlags);
end;

procedure TColumnsFileView.CreateDefault(AOwner: TWinControl);
begin
  DCDebug('TColumnsFileView.Create components');

  inherited CreateDefault(AOwner);

  FFileNameColumn := -1;
  FExtensionColumn := -1;

  // -- other components

  dgPanel:=TDrawGridEx.Create(Self, Self);
  MainControl := dgPanel;

  // ---
  dgPanel.OnHeaderClick:=@dgPanelHeaderClick;
  dgPanel.OnMouseWheelUp := @dgPanelMouseWheelUp;
  dgPanel.OnMouseWheelDown := @dgPanelMouseWheelDown;
  dgPanel.OnSelection:= @dgPanelSelection;
{$IF lcl_fullversion >= 093100}
  dgPanel.OnBeforeSelection:= @dgPanelBeforeSelection;
{$ENDIF}
  dgPanel.OnTopLeftChanged:= @dgPanelTopLeftChanged;
  dgpanel.OnResize:= @dgPanelResize;
  dgPanel.OnHeaderSized:= @dgPanelHeaderSized;

  pmColumnsMenu := TPopupMenu.Create(Self);
  pmColumnsMenu.Parent := Self;

  if Assigned(NotebookPage) then
  begin
    FOnColumnResized:= @DoColumnResized;
  end;
end;

destructor TColumnsFileView.Destroy;
begin
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

    if FileView is TColumnsFileView then
    with FileView as TColumnsFileView do
    begin
      FColumnsSortDirections := Self.FColumnsSortDirections;

      ActiveColm := Self.ActiveColm;
      ActiveColmSlave := nil;    // set to nil because only used in preview?
      isSlave := Self.isSlave;
    end;
  end;
end;

procedure TColumnsFileView.AddFileSource(aFileSource: IFileSource; aPath: String);
begin
  inherited AddFileSource(aFileSource, aPath);

  if not IsLoadingFileList then
  begin
    FUpdatingActiveFile := True;
    dgPanel.Row := 0;
    FUpdatingActiveFile := False;
  end;
end;

procedure TColumnsFileView.BeforeMakeFileList;
begin
  inherited;
  if gListFilesInThread then
  begin
    // Display info that file list is being loaded.
    UpdateInfoPanel;
  end;
end;

procedure TColumnsFileView.ClearAfterDragDrop;
begin
  inherited ClearAfterDragDrop;

  // reset TCustomGrid state
  dgPanel.FGridState := gsNormal;
end;

procedure TColumnsFileView.FileSourceFileListLoaded;
begin
  inherited;

  FUpdatingActiveFile := True;
  dgPanel.Row := 0;
  FUpdatingActiveFile := False;
end;

procedure TColumnsFileView.DisplayFileListChanged;
begin
  // Update grid row count.
  SetRowCount(FFiles.Count);
  SetFilesDisplayItems;
  RedrawFiles;

  if SetActiveFileNow(RequestedActiveFile) then
    RequestedActiveFile := ''
  // Requested file was not found, restore position to last active file.
  else if not SetActiveFileNow(LastActiveFile) then
  // Make sure at least that the previously active file is still visible after displaying file list.
    MakeActiveVisible;

  Notify([fvnVisibleFilePropertiesChanged]);

  inherited;
end;

procedure TColumnsFileView.DoColumnResized(Sender: TObject;
  ColumnIndex: Integer; ColumnNewSize: Integer);

  procedure UpdateWidth(Notebook: TFileViewNotebook);
  var
    I: Integer;
    ColumnsView: TColumnsFileView;
  begin
    for I:= 0 to Notebook.PageCount - 1 do
    begin
      if Notebook.View[I] is TColumnsFileView then
      begin
        ColumnsView:= TColumnsFileView(Notebook.View[I]);
        if ColumnsView.ActiveColm = ActiveColm then
        begin
          ColumnsView.dgPanel.ColWidths[ColumnIndex]:= ColumnNewSize;
        end;
      end;
    end;
  end;

begin
  if gColumnsAutoSaveWidth then
  begin
    GetColumnsClass.SetColumnWidth(ColumnIndex, ColumnNewSize);
    UpdateWidth(frmMain.LeftTabs);
    UpdateWidth(frmMain.RightTabs);
  end;
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

procedure TColumnsFileView.ClearAllColumnsStrings;
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

procedure TColumnsFileView.EachViewUpdateColumns(AFileView: TFileView; UserData: Pointer);
var
  ColumnsView: TColumnsFileView;
  PMsg: PEachViewCallbackMsg;
begin
  if AFileView is TColumnsFileView then
  begin
    ColumnsView := TColumnsFileView(AFileView);
    PMsg := UserData;
    if ColumnsView.ActiveColm = PMsg^.UpdatedColumnsSetName then
    begin
      ColumnsView.ActiveColm := PMsg^.NewColumnsSetName;
      ColumnsView.UpdateColumnsView;
      ColumnsView.RedrawFiles;
    end;
  end;
end;

procedure TColumnsFileView.DoUpdateView;
begin
  inherited DoUpdateView;
  UpdateColumnsView;
end;

function TColumnsFileView.GetActiveFileIndex: PtrInt;
begin
  Result := dgPanel.Row - dgPanel.FixedRows;
end;

function TColumnsFileView.GetVisibleFilesIndexes: TRange;
begin
  Result := dgPanel.GetVisibleRows;
  Dec(Result.First, dgPanel.FixedRows);
  Dec(Result.Last, dgPanel.FixedRows);
end;

function TColumnsFileView.GetColumnsClass: TPanelColumnsClass;
begin
  if isSlave then
    Result := ActiveColmSlave
  else
    Result := ColSet.GetColumnSet(ActiveColm);
end;

function TColumnsFileView.GetFileIndexFromCursor(X, Y: Integer; out AtFileList: Boolean): PtrInt;
var
  bTemp: Boolean;
  iRow, iCol: LongInt;
begin
  with dgPanel do
  begin
    bTemp:= AllowOutboundEvents;
    AllowOutboundEvents:= False;
    MouseToCell(X, Y, iCol, iRow);
    AllowOutboundEvents:= bTemp;
    Result:= IfThen(iRow < 0, InvalidFileIndex, iRow - FixedRows);
    AtFileList := Y >= GetHeaderHeight;
  end;
end;

procedure TColumnsFileView.DoFileUpdated(AFile: TDisplayFile; UpdatedProperties: TFilePropertiesTypes);
begin
  MakeColumnsStrings(AFile);
  inherited DoFileUpdated(AFile, UpdatedProperties);
end;

procedure TColumnsFileView.DoHandleKeyDown(var Key: Word; Shift: TShiftState);
var
  AFile: TDisplayFile;
begin
  case Key of
    VK_INSERT:
      begin
        if not IsEmpty then
        begin
          if IsActiveItemValid then
          begin
            InvertFileSelection(GetActiveDisplayFile, False);
            DoSelectionChanged(dgPanel.Row - dgPanel.FixedRows);
          end;
          if dgPanel.Row < dgPanel.RowCount-1 then
            dgPanel.Row := dgPanel.Row + 1;
          MakeActiveVisible;
        end;
        Key := 0;
      end;

    // cursors keys in Lynx like mode
    VK_LEFT:
      if (Shift = []) then
      begin
        if gLynxLike then
          ChangePathToParent(True)
        else
          dgPanel.ScrollHorizontally(False);
        Key := 0;
      end;

    VK_RIGHT:
      if (Shift = []) then
      begin
        if gLynxLike then
          ChooseFile(GetActiveDisplayFile, True)
        else
          dgPanel.ScrollHorizontally(True);
        Key := 0;
      end;

    VK_SPACE:
      if Shift * KeyModifiersShortcut = [] then
      begin
        aFile := GetActiveDisplayFile;
        if IsItemValid(aFile) then
        begin
          if (aFile.FSFile.IsDirectory or
             aFile.FSFile.IsLinkToDirectory) and
             not aFile.Selected then
          begin
            CalculateSpace(aFile);
          end;

          InvertFileSelection(aFile, False);
        end;

        if gSpaceMovesDown then
          dgPanel.Row := dgPanel.Row + 1;

        MakeActiveVisible;
        DoSelectionChanged(dgPanel.Row - dgPanel.FixedRows);
        Key := 0;
      end;
  end;

  inherited DoHandleKeyDown(Key, Shift);
end;

procedure TColumnsFileView.dgPanelHeaderSized(Sender: TObject; IsColumn: Boolean; index: Integer);
begin
  if IsColumn then
    if Assigned(FOnColumnResized) then
      begin
        FOnColumnResized(Self, index, dgPanel.ColWidths[index]);
      end;
end;

procedure TColumnsFileView.cm_CopyFileDetailsToClip(const Params: array of string);
var
  I: Integer;
  AFile: TDisplayFile;
  sl: TStringList = nil;

  procedure AddFile;
  var
    J: Integer;
    S: String;
  begin
    if AFile.FSFile.IsNameValid then
    begin
      S:= EmptyStr;
      for J:= 0 to AFile.DisplayStrings.Count - 1 do
      begin
        S:= S + AFile.DisplayStrings[J] + #09;
      end;
      J:= Length(S);
      if J > 0 then sl.Add(Copy(S, 1, J - 1));
    end;
  end;

begin
  if DisplayFiles.Count > 0 then
  begin
    sl:= TStringList.Create;
    try
      for I:= 0 to FFiles.Count - 1 do
      begin
        AFile:= FFiles[I];
        if AFile.Selected then AddFile;
      end;

      if sl.Count = 0 then
      begin
        AFile:= GetActiveDisplayFile;
        AddFile;
      end;

      Clipboard.Clear;   // prevent multiple formats in Clipboard
      ClipboardSetText(TrimRightLineEnding(sl.Text, sl.TextLineBreakStyle));
    finally
      FreeAndNil(sl);
    end;
  end;
end;

{ TDrawGridEx }

constructor TDrawGridEx.Create(AOwner: TComponent; AParent: TWinControl);
begin
  inherited Create(AOwner);

  ColumnsView := AParent as TColumnsFileView;
  ColumnsOwnDim := @ColumnsView.DimColor;

  // Workaround for Lazarus issue 18832.
  // Set Fixed... before setting ...Count.
  FixedRows := 0;
  FixedCols := 0;

  // Override default values to start with no columns and no rows.
  RowCount := 0;
  ColCount := 0;

  DoubleBuffered := True;
  Align := alClient;
  Options := [goFixedVertLine, goFixedHorzLine, goTabs, goRowSelect, goColSizing,
              goThumbTracking, goSmoothScroll, goHeaderHotTracking, goHeaderPushedLook];

  TitleStyle := gColumnsTitleStyle;
  TabStop := False;

  Self.Parent := AParent;
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
  AutoFillColumns:= gAutoFillColumns;
  GridVertLine:= gGridVertLine;
  GridHorzLine:= gGridHorzLine;

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
    TabHeaderHeight := TabHeaderHeight + 2; // for borders
    if not gInterfaceFlat then
    begin
      TabHeaderHeight := TabHeaderHeight + 2; // additional borders if not flat
    end;
    RowHeights[0] := TabHeaderHeight;
  end
  else
  begin
    if FixedRows > 0 then
    begin
      // First reduce number of rows so that the 0'th row, which will be changed
      // to not-fixed, won't be counted as a row having a file.
      if RowCount > 0 then
        RowCount := RowCount - 1;
      FixedRows := 0;
    end;
  end;

  FixedCols := 0;
  // Set column number to zero, must be called after fixed columns change
  MoveExtend(False, 0, Row);
end;

procedure TDrawGridEx.InitializeWnd;
begin
  inherited InitializeWnd;
  ColumnsView.InitializeDragDropEx(Self);
end;

procedure TDrawGridEx.FinalizeWnd;
begin
  ColumnsView.FinalizeDragDropEx(Self);
  inherited FinalizeWnd;
end;

procedure TDrawGridEx.DrawColumnText(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  SortingDirection: TSortDirection;
begin
  SortingDirection := ColumnsView.FColumnsSortDirections[ACol];

  if SortingDirection <> sdNone then
  begin
    PixMapManager.DrawBitmap(
        PixMapManager.GetIconBySortingDirection(SortingDirection),
        Canvas,
        aRect.Left, aRect.Top + (RowHeights[aRow] - gIconsSize) div 2);
    aRect.Left += gIconsSize;
  end;

  DrawCellText(aCol, aRow, aRect, aState, GetColumnTitle(aCol));
end;

function TDrawGridEx.GetFullVisibleRows: TRange;
begin
  Result.First := GCache.FullVisibleGrid.Top;
  Result.Last  := GCache.FullVisibleGrid.Bottom;
end;

procedure TDrawGridEx.DrawCell(aCol, aRow: Integer; aRect: TRect;
              aState: TGridDrawState);
const
  CELL_PADDING = 2;

var
  //shared variables
  s:   string;
  iTextTop: Integer;
  AFile: TDisplayFile;
  FileSourceDirectAccess: Boolean;
  ColumnsSet: TPanelColumnsClass;

  //------------------------------------------------------
  // begin subprocedures
  //------------------------------------------------------

  procedure DrawFixed;
  //------------------------------------------------------
  var
    TextStyle: TTextStyle;
  begin
    SetCanvasFont(GetColumnFont(aCol, True));
    Canvas.Brush.Color := GetColumnColor(ACol, True);

    TextStyle := Canvas.TextStyle;
    TextStyle.Layout := tlCenter;
    Canvas.TextStyle := TextStyle;

    DefaultDrawCell(aCol, aRow, aRect, aState);
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
                               aRect.Left + CELL_PADDING,
                               Y
                               );

      // Draw overlay icon for a file if needed
      if gIconOverlays then
      begin
        PixMapManager.DrawBitmapOverlay(AFile,
                                        FileSourceDirectAccess,
                                        Canvas,
                                        aRect.Left + CELL_PADDING,
                                        Y
                                        );
      end;

    end;

    s := AFile.DisplayStrings.Strings[ACol];

    if gCutTextToColWidth then
    begin
      Y:= (aRect.Right - aRect.Left) - 2*CELL_PADDING;
      if (gShowIcons <> sim_none) then Y:= Y - gIconsSize - 2;
      s:= FitFileName(s, Canvas, AFile.FSFile, Y);
    end;

    if (gShowIcons <> sim_none) then
      Canvas.TextOut(aRect.Left + CELL_PADDING + gIconsSize + 2, iTextTop, s)
    else
      Canvas.TextOut(aRect.Left + CELL_PADDING, iTextTop, s);
  end; //of DrawIconCell
  //------------------------------------------------------

  procedure DrawOtherCell;
  //------------------------------------------------------
  var
    tw: Integer;
  begin
    s := AFile.DisplayStrings.Strings[ACol];

    if gCutTextToColWidth then
      s := FitOtherCellText(s, Canvas, ARect.Right - ARect.Left - 2*CELL_PADDING);

    case ColumnsSet.GetColumnAlign(ACol) of

      taRightJustify:
        begin
          tw := Canvas.TextWidth(s);
          Canvas.TextOut(aRect.Right - tw - CELL_PADDING, iTextTop, s);
        end;

      taLeftJustify:
        begin
          Canvas.TextOut(aRect.Left + CELL_PADDING, iTextTop, s);
        end;

      taCenter:
        begin
          tw := Canvas.TextWidth(s);
          Canvas.TextOut((aRect.Left + aRect.Right - tw) div 2, iTextTop, s);
        end;

    end; //of case
  end; //of DrawOtherCell
  //------------------------------------------------------

  procedure PrepareColors;
  //------------------------------------------------------
  var
    TextColor: TColor = clDefault;
    BackgroundColor: TColor;
    IsCursor: Boolean;
    IsCursorInactive: Boolean;
  //---------------------
  begin
    Canvas.Font.Name    := ColumnsSet.GetColumnFontName(ACol);
    Canvas.Font.Size    := ColumnsSet.GetColumnFontSize(ACol);
    Canvas.Font.Style   := ColumnsSet.GetColumnFontStyle(ACol);
    Canvas.Font.Quality := ColumnsSet.GetColumnFontQuality(ACol);

    IsCursor := (gdSelected in aState) and ColumnsView.Active and (not ColumnsSet.UseFrameCursor);
    IsCursorInactive := (gdSelected in aState) and (not ColumnsView.Active) and (not ColumnsSet.UseFrameCursor);
    // Set up default background color first.
    if IsCursor then
      BackgroundColor := ColumnsSet.GetColumnCursorColor(ACol)
    else
      begin
        if IsCursorInactive AND ColumnsSet.GetColumnUseInactiveSelColor(ACol) then
          BackgroundColor := ColumnsSet.GetColumnInactiveCursorColor(ACol)
        else
          // Alternate rows background color.
          if odd(ARow) then
            BackgroundColor := ColumnsSet.GetColumnBackground(ACol)
          else
            BackgroundColor := ColumnsSet.GetColumnBackground2(ACol);
      end;

    // Set text color.
    if ColumnsSet.GetColumnOvercolor(ACol) then
      TextColor := AFile.TextColor;
    if (TextColor = clDefault) or (TextColor = clNone) then
      TextColor := ColumnsSet.GetColumnTextColor(ACol);

    if AFile.Selected then
    begin
      if ColumnsSet.GetColumnUseInvertedSelection(ACol) then
        begin
          //------------------------------------------------------
          if IsCursor OR (IsCursorInactive AND ColumnsSet.GetColumnUseInactiveSelColor(ACol)) then
            begin
              TextColor := InvertColor(ColumnsSet.GetColumnCursorText(ACol));
            end
          else
            begin
              if ColumnsView.Active OR (not ColumnsSet.GetColumnUseInactiveSelColor(ACol)) then
                BackgroundColor := ColumnsSet.GetColumnMarkColor(ACol)
              else
                BackgroundColor := ColumnsSet.GetColumnInactiveMarkColor(ACol);
              TextColor := ColumnsSet.GetColumnBackground(ACol);
            end;
          //------------------------------------------------------
        end
      else
        begin
          if ColumnsView.Active OR (not ColumnsSet.GetColumnUseInactiveSelColor(ACol)) then
            TextColor := ColumnsSet.GetColumnMarkColor(ACol)
          else
            TextColor := ColumnsSet.GetColumnInactiveMarkColor(ACol);
        end;
    end
    else if IsCursor then
      begin
        TextColor := ColumnsSet.GetColumnCursorText(ACol);
      end;

    BackgroundColor := ColumnsOwnDim(BackgroundColor);

    if AFile.RecentlyUpdatedPct <> 0 then
    begin
      TextColor := LightColor(TextColor, AFile.RecentlyUpdatedPct);
      BackgroundColor := LightColor(BackgroundColor, AFile.RecentlyUpdatedPct);
    end;

    // Draw background.
    Canvas.Brush.Color := BackgroundColor;
    Canvas.FillRect(aRect);
    Canvas.Font.Color := TextColor;
    Canvas.Brush.Style := bsClear;
  end;// of PrepareColors;

  procedure DrawLines;
  var
    delta:integer;
  begin
    // Draw frame cursor.
    Canvas.Pen.Width := ColumnsSet.GetColumnBorderFrameWidth(ACol);

    if Canvas.Pen.Width<=1 then
    begin
      delta:=0;
    end else
    begin
      if odd(Canvas.Pen.Width) then
        delta:=Canvas.Pen.Width shr 1
      else
        delta:=(Canvas.Pen.Width shr 1)+1;
    end;


    if ColumnsSet.UseFrameCursor and (gdSelected in aState) and (ColumnsView.Active OR ColumnsSet.GetColumnUseInactiveSelColor(Acol)) then
    begin
      if ColumnsView.Active then
        Canvas.Pen.Color := ColumnsSet.GetColumnCursorColor(ACol)
      else
        Canvas.Pen.Color := ColumnsSet.GetColumnInactiveCursorColor(ACol);



      if ACol=0 then
      begin
        Canvas.Line(aRect.Left + 1, aRect.Top + delta , aRect.Right , aRect.Top + delta );
        Canvas.Line(aRect.Left + 1, aRect.Bottom - 1 - delta, aRect.Right, aRect.Bottom - 1 - delta);

        Canvas.Line(aRect.Left + delta, aRect.Top + delta , aRect.Left + delta, aRect.Bottom - delta - 1);
      end else
      if ACol<ColCount-1 then
      begin
        Canvas.Line(aRect.Left, aRect.Top + delta , aRect.Right , aRect.Top + delta );
        Canvas.Line(aRect.Left, aRect.Bottom - 1 - delta, aRect.Right, aRect.Bottom - 1 - delta);
      end else
      begin
        Canvas.Line(aRect.Left, aRect.Top + delta , aRect.Right - delta - 1, aRect.Top + delta );
        Canvas.Line(aRect.Left, aRect.Bottom - 1 - delta, aRect.Right - delta -1, aRect.Bottom - 1 - delta);

        Canvas.Line(aRect.Right - delta - 1, aRect.Top + delta , aRect.Right - delta - 1, aRect.Bottom - delta - 1);
      end;


      {
      Canvas.Pen.Color:=clred;
      Canvas.Brush.Style:=bsClear;
//      Canvas.Rectangle(Rect(aRect.Left + delta , aRect.Top + delta , aRect.Right - delta,aRect.Bottom - delta));
      }
    end;

    // Draw drop selection.
    if ARow - FixedRows = ColumnsView.FDropFileIndex then
    begin
      Canvas.Pen.Color := ColumnsSet.GetColumnTextColor(ACol);


    if ACol=0 then
    begin
      Canvas.Line(aRect.Left + 1, aRect.Top + delta , aRect.Right , aRect.Top + delta );
      Canvas.Line(aRect.Left + 1, aRect.Bottom - 1 - delta, aRect.Right, aRect.Bottom - 1 - delta);

      Canvas.Line(aRect.Left + delta, aRect.Top + delta , aRect.Left + delta, aRect.Bottom - delta - 1);
    end else
    if ACol<ColCount-1 then
    begin
      Canvas.Line(aRect.Left, aRect.Top + delta , aRect.Right , aRect.Top + delta );
      Canvas.Line(aRect.Left, aRect.Bottom - 1 - delta, aRect.Right, aRect.Bottom - 1 - delta);
    end else
    begin
      Canvas.Line(aRect.Left, aRect.Top + delta , aRect.Right - delta - 1, aRect.Top + delta );
      Canvas.Line(aRect.Left, aRect.Bottom - 1 - delta, aRect.Right - delta -1, aRect.Bottom - 1 - delta);

      Canvas.Line(aRect.Right - delta - 1, aRect.Top + delta , aRect.Right - delta - 1, aRect.Bottom - delta - 1);
    end;

      {
//      Canvas.Rectangle(aRect);
      Canvas.Line(aRect.Left, aRect.Top + delta , aRect.Right - delta, aRect.Top + delta );
      Canvas.Line(aRect.Left, aRect.Bottom - 1 - delta, aRect.Right - delta, aRect.Bottom - 1 - delta);

      if ACol=0 then
         Canvas.Line(aRect.Left + delta, aRect.Top + delta , aRect.Left + delta, aRect.Bottom - delta - 1);


      if ACol=ColCount-1 then
      Canvas.Line(aRect.Right - delta - 1, aRect.Top + delta , aRect.Right - delta - 1, aRect.Bottom - delta - 1);
        }
        {
        Canvas.Pen.Color:=clred;
        Canvas.Brush.Style:=bsClear;
        Canvas.Rectangle(Rect(aRect.Left {+ delta} , aRect.Top {+ delta} , aRect.Right - delta,aRect.Bottom - delta));
        }
    end;
  end;

  procedure DrawExtendedCells;
  type
    TCell = record
      Col: Integer;         // column index
      Rect: TRect;          // initial rect
      LeftBound,            // new left bound
      RightBound: Integer;  // new right bound
    end;

    procedure GetCellBounds(var ACell: TCell);
    var
      CellText: string;
      CellWidth: Integer;
      ColAlign: TAlignment;
    begin
      CellText := AFile.DisplayStrings[ACell.Col];
      CellWidth := Canvas.TextWidth(CellText) + 2*CELL_PADDING;
      if (ACell.Col = 0) and (gShowIcons <> sim_none) then
        CellWidth := CellWidth + gIconsSize + 2;

      ColAlign := ColumnsSet.GetColumnAlign(ACell.Col);
      if (ColAlign = taLeftJustify) or (ACell.Col = 0) then
      begin
        ACell.LeftBound := ACell.Rect.Left;
        ACell.RightBound := ACell.LeftBound + CellWidth;
      end
      else if ColAlign = taRightJustify then
      begin
        ACell.RightBound := ACell.Rect.Right;
        ACell.LeftBound := ACell.RightBound - CellWidth;
      end
      else
      begin
        ACell.LeftBound := (ACell.Rect.Left + ACell.Rect.Right - CellWidth) div 2;
        if (ACell.Rect.Left <= ACell.LeftBound) or (not gCutTextToColWidth) then
          ACell.RightBound := ACell.LeftBound + CellWidth
        else begin
          ACell.LeftBound := ACell.Rect.Left;
          ACell.RightBound := ACell.Rect.Right;
        end;
      end;
    end;

    procedure FindNextCell(ACurrentCol, ADirection: Integer; out ACell: TCell);
    var
      C: Integer;
    begin
      C := ACurrentCol + ADirection;
      while (C >= 0) and (C < ColCount) do
      begin
        if (AFile.DisplayStrings[C] <> '') and (ColWidths[C] <> 0) then
        begin
          ACell.Col := C;
          ACell.Rect := CellRect(C, aRow);
          GetCellBounds(ACell);
          Exit;
        end;
        C := C + ADirection;
      end;
      ACell.Col := -1;
    end;

    procedure ReconcileBounds(var LCell, RCell: TCell);
    var
      LeftEdge: Integer absolute LCell.RightBound;
      RightEdge: Integer absolute RCell.LeftBound;
      LeftColEdge: Integer absolute LCell.Rect.Right;
    begin
      if (LeftEdge <= RightEdge) or (not gCutTextToColWidth) then
        Exit;

      if (RightEdge < LeftColEdge) and (LeftColEdge < LeftEdge) then
      begin
        LeftEdge := LeftColEdge;
        RightEdge := LeftColEdge;
      end
      else if LeftEdge <= LeftColEdge then
        RightEdge := LeftEdge
      else
        LeftEdge := RightEdge;
    end;

    procedure DrawCell(const ACell: TCell);
    begin
      aCol := ACell.Col;
      aRect.Left := ACell.LeftBound;
      aRect.Right := ACell.RightBound;
      if aCol = 0 then
        DrawIconCell
      else
        DrawOtherCell;
    end;

  var
    CCell, LCell, RCell: TCell;
  begin
    CCell.Col := aCol;
    CCell.Rect := aRect;

    FindNextCell(CCell.Col, -1, LCell);
    FindNextCell(CCell.Col, +1, RCell);

    if AFile.DisplayStrings[CCell.Col] = '' then
    begin
      if (LCell.Col <> -1) and (RCell.Col <> -1) then
        ReconcileBounds(LCell, RCell);

      if (LCell.Col <> -1) and (CCell.Rect.Left < LCell.RightBound) then
        DrawCell(LCell);

      if (RCell.Col <> -1) and (RCell.LeftBound < CCell.Rect.Right) then
        DrawCell(RCell);
    end
    else
    begin
      GetCellBounds(CCell);

      if LCell.Col <> -1 then
      begin
        ReconcileBounds(LCell, CCell);
        if CCell.Rect.Left < LCell.RightBound then
          DrawCell(LCell);
      end;

      if RCell.Col <> -1 then
      begin
        ReconcileBounds(CCell, RCell);
        if RCell.LeftBound < CCell.Rect.Right then
          DrawCell(RCell);
      end;

      DrawCell(CCell);
    end;

    aCol := CCell.Col;
    aRect := CCell.Rect;
  end;
  //------------------------------------------------------
  //end of subprocedures
  //------------------------------------------------------

begin
  ColumnsSet := ColumnsView.GetColumnsClass;

  if gdFixed in aState then
  begin
    DrawFixed; // Draw column headers
    if TitleStyle <> tsNative then DrawCellGrid(aCol, aRow, aRect, aState);
  end
  else if ColumnsView.IsFileIndexInRange(ARow - FixedRows) then
  begin
    AFile := ColumnsView.FFiles[ARow - FixedRows]; // substract fixed rows (header)
    FileSourceDirectAccess := fspDirectAccess in ColumnsView.FileSource.Properties;

    if AFile.DisplayStrings.Count = 0 then
      ColumnsView.MakeColumnsStrings(AFile, ColumnsSet);

    PrepareColors;

    iTextTop := aRect.Top + (RowHeights[aRow] - Canvas.TextHeight('Wg')) div 2;

    if gExtendCellWidth then
      DrawExtendedCells
    else
    begin
      if ACol = 0 then
        DrawIconCell  // Draw icon in the first column
      else
        DrawOtherCell;
    end;

    DrawCellGrid(aCol,aRow,aRect,aState);
    DrawLines;
  end
  else
  begin
    Canvas.Brush.Color := Self.Color;
    Canvas.FillRect(aRect);
  end;
end;

{$if lcl_fullversion >= 1070000}
procedure TDrawGridEx.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  // Don't auto adjust layout
end;
{$endif}

procedure TDrawGridEx.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  I : Integer;
  Point: TPoint;
  MI: TMenuItem;
  Background: Boolean;
begin
  if ColumnsView.IsLoadingFileList then Exit;
{$IFDEF LCLGTK2}
  // Workaround for two doubleclicks being sent on GTK.
  // MouseUp event is sent just after doubleclick, so if we drop
  // doubleclick events we have to also drop MouseUp events that follow them.
  if ColumnsView.TooManyDoubleClicks then Exit;
{$ENDIF}

  // Handle only if button-up was not lifted to finish drag&drop operation.
  if not ColumnsView.FMainControlMouseDown then
    Exit;

  inherited MouseUp(Button, Shift, X, Y);

  ColumnsView.FMainControlMouseDown := False;

  if Button = mbRight then
    begin
      { If right click on header }
      if (Y >= 0) and (Y < GetHeaderHeight) then
        begin
          //Load Columns into menu
          ColumnsView.pmColumnsMenu.Items.Clear;
          if ColSet.Items.Count>0 then
            begin
              For I:=0 to ColSet.Items.Count-1 do
                begin
                  MI:=TMenuItem.Create(ColumnsView.pmColumnsMenu);
                  MI.Tag:=I;
                  MI.Caption:=ColSet.Items[I];
                  MI.Checked:=(ColSet.Items[I] = ColumnsView.ActiveColm);
                  MI.OnClick:=@ColumnsView.ColumnsMenuClick;
                  ColumnsView.pmColumnsMenu.Items.Add(MI);
                end;
            end;
          //-
          MI:=TMenuItem.Create(ColumnsView.pmColumnsMenu);
          MI.Caption:='-';
          ColumnsView.pmColumnsMenu.Items.Add(MI);
          //Configure custom columns
          MI:=TMenuItem.Create(ColumnsView.pmColumnsMenu);
          MI.Tag:=1001;
          MI.Caption:=rsMenuConfigureCustomColumns;
          MI.OnClick:=@ColumnsView.ColumnsMenuClick;
          ColumnsView.pmColumnsMenu.Items.Add(MI);

          Point:=ClientToScreen(Classes.Point(0,0));
          Point.Y:=Point.Y+GetHeaderHeight;
          Point.X:=Point.X+X-50;
          ColumnsView.pmColumnsMenu.PopUp(Point.X,Point.Y);
        end

      { If right click on file/directory }
      else if ((gMouseSelectionButton<>1) or not gMouseSelectionEnabled) then
        begin
          Background:= not MouseOnGrid(X, Y);
          Point := ClientToScreen(Classes.Point(X, Y));
          frmMain.Commands.DoContextMenu(ColumnsView, Point.x, Point.y, Background);
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

procedure TDrawGridEx.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  if ColumnsView.IsLoadingFileList then Exit;
{$IFDEF LCLGTK2}
  // Workaround for two doubleclicks being sent on GTK.
  // MouseDown event is sent just before doubleclick, so if we drop
  // doubleclick events we have to also drop MouseDown events that precede them.
  if ColumnsView.TooManyDoubleClicks then Exit;
{$ENDIF}

  ColumnsView.FMainControlMouseDown := True;

  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TDrawGridEx.MouseMove(Shift: TShiftState; X, Y: Integer);
  procedure Scroll(ScrollCode: SmallInt);
  var
    Msg: TLMVScroll;
  begin
    Msg.Msg := LM_VSCROLL;
    Msg.ScrollCode := ScrollCode;
    Msg.SmallPos := 1; // How many lines scroll
    Msg.ScrollBar := Handle;
    Dispatch(Msg);
  end;
begin
  inherited MouseMove(Shift, X, Y);
  if DragManager.IsDragging or ColumnsView.IsMouseSelecting then
  begin
    if Y < DefaultRowHeight then
      Scroll(SB_LINEUP)
    else if Y > ClientHeight - DefaultRowHeight then
      Scroll(SB_LINEDOWN);
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

function TDrawGridEx.GetGridHorzLine: Boolean;
begin
  Result := goHorzLine in Options;
end;

function TDrawGridEx.GetGridVertLine: Boolean;
begin
  Result := goVertLine in Options;
end;

procedure TDrawGridEx.SetGridHorzLine(const AValue: Boolean);
begin
  if AValue then
    Options := Options + [goHorzLine]
  else
    Options := Options - [goHorzLine];
end;

procedure TDrawGridEx.SetGridVertLine(const AValue: Boolean);
begin
  if AValue then
    Options := Options + [goVertLine]
  else
    Options := Options - [goVertLine];
end;

{$IF lcl_fullversion < 1090000}
// Workaround for Lazarus issue 31942.
function TDrawGridEx.SelectCell(aCol, aRow: Integer): Boolean;
begin
  Result:= inherited SelectCell(aCol, aRow);
  // ScrollToCell hangs when Width = 0
  if Width = 0 then
  begin
    Result:= False;
    SetColRow(aCol, aRow);
  end;
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

procedure TDrawGridEx.KeyDown(var Key: Word; Shift: TShiftState);
var
  SavedKey: Word;
begin
  if ColumnsView.IsLoadingFileList then
  begin
    ColumnsView.HandleKeyDownWhenLoading(Key, Shift);
    Exit;
  end;

  SavedKey := Key;
  // Set RangeSelecting before cursor is moved.
  ColumnsView.FRangeSelecting :=
    (ssShift in Shift) and
    (SavedKey in [VK_HOME, VK_END, VK_PRIOR, VK_NEXT]);
  // Special case for selection with shift key (works like VK_INSERT)
  if (SavedKey in [VK_UP, VK_DOWN]) and (ssShift in Shift) then
    ColumnsView.InvertActiveFile;

  {$IFDEF LCLGTK2}
  // Workaround for GTK2 - up and down arrows moving through controls.
  if Key in [VK_UP, VK_DOWN] then
  begin
    if ((Row = RowCount-1) and (Key = VK_DOWN))
    or ((Row = FixedRows) and (Key = VK_UP)) then
      Key := 0;
  end;
  {$ENDIF}

  inherited KeyDown(Key, Shift);

  if (ColumnsView.FRangeSelecting) and (Row >= FixedRows) then
    ColumnsView.Selection(SavedKey, Row - FixedRows);
end;

procedure TDrawGridEx.ScrollHorizontally(ForwardDirection: Boolean);
  function TryMove(ACol: Integer): Boolean;
  begin
    Result := not IscellVisible(ACol, Row);
    if Result then
      MoveExtend(False, ACol, Row);
  end;
var
  i: Integer;
begin
  if ForwardDirection then
  begin
    for i := Col + 1 to ColCount - 1 do
      if TryMove(i) then
        Break;
  end
  else
  begin
    for i := Col - 1 downto 0 do
      if TryMove(i) then
        Break;
  end;
end;

end.

