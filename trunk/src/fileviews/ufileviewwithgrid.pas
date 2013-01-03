unit uFileViewWithGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LMessages, Grids, Graphics, StdCtrls,
  uDisplayFile, DCXmlConfig, uFileSorting, uFileProperty, uTypes,
  uFileViewWithMainCtrl, uFile, uFileViewHeader, uFileView, uFileSource;

type

  TFileViewWithGrid = class;

  { TFileViewGrid }

  TFileViewGrid = class(TDrawGrid)
  protected
    FFileView: TFileViewWithGrid;
  protected
    procedure RowHeightsChanged; override;
    procedure ColWidthsChanged;  override;
    procedure FinalizeWnd; override;
    procedure InitializeWnd; override;
    function MouseOnGrid(X, Y: LongInt): Boolean;
    procedure DoOnResize; override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure TopLeftChanged; override;
  protected
    procedure DrawLines(aIdx, aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure PrepareColors(aFile: TDisplayFile; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure UpdateView; virtual; abstract;
    procedure CalculateColRowCount; virtual; abstract;
    procedure CalculateColumnWidth; virtual; abstract;
    function  CellToIndex(ACol, ARow: Integer): Integer; virtual; abstract;
    procedure IndexToCell(Index: Integer; out ACol, ARow: Integer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent; AParent: TWinControl); reintroduce; virtual;
  end;

  { TFileViewGridClass }

  TFileViewGridClass = class of TFileViewGrid;

  { TFileViewWithGrid }

  TFileViewWithGrid = class (TFileViewWithMainCtrl)
  protected
    TabHeader: TFileViewFixedHeader;
    dgPanel: TFileViewGrid;
    lblDetails: TLabel;
  private
    procedure SetFilesDisplayItems;
    procedure UpdateFooterDetails;
    procedure dgPanelSelection(Sender: TObject; aCol, aRow: Integer);
  protected
    procedure MakeColumnsStrings(AFile: TDisplayFile);
    function GetFileViewGridClass: TFileViewGridClass; virtual; abstract;
  protected
    procedure CreateDefault(AOwner: TWinControl); override;
    procedure BeforeMakeFileList; override;
    procedure ClearAfterDragDrop; override;
    procedure AfterChangePath; override;
    procedure DisplayFileListChanged; override;
    procedure DoMainControlShowHint(FileIndex: PtrInt; X, Y: Integer); override;
    procedure DoOnResize; override;
    procedure FileSourceFileListLoaded; override;
    function GetActiveFileIndex: PtrInt; override;
    function GetFileIndexFromCursor(X, Y: Integer; out AtFileList: Boolean): PtrInt; override;
    function GetFileRect(FileIndex: PtrInt): TRect; override;
    procedure RedrawFile(FileIndex: PtrInt); override;
    procedure RedrawFile(DisplayFile: TDisplayFile); override;
    procedure RedrawFiles; override;
    procedure SetActiveFile(FileIndex: PtrInt); override;
    procedure DoFileUpdated(AFile: TDisplayFile; UpdatedProperties: TFilePropertiesTypes = []); override;
    procedure DoHandleKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure UpdateInfoPanel; override;
    procedure DoUpdateView; override;
    procedure SetSorting(const NewSortings: TFileSortings); override;
  public
    constructor Create(AOwner: TWinControl; AFileSource: IFileSource; APath: String; AFlags: TFileViewFlags = []); override;
    constructor Create(AOwner: TWinControl; AConfig: TXmlConfig; ANode: TXmlNode; AFlags: TFileViewFlags = []); override;
    constructor Create(AOwner: TWinControl; AFileView: TFileView; AFlags: TFileViewFlags = []); override;
    destructor Destroy; override;

    procedure CloneTo(FileView: TFileView); override;

    procedure AddFileSource(aFileSource: IFileSource; aPath: String); override;

    procedure LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); override;
  end;

  function FitFileName(const AFileName: UTF8String; ACanvas: TCanvas; AFile: TFile; ATargetWidth: Integer): UTF8String;

implementation

uses
  LCLIntf, LCLType, LCLVersion, LCLProc, math,
  uGlobs, uPixmapManager, uKeyboard,
  uDCUtils, fMain,
  uFileSourceProperty,
  uFileFunctions,
  uOrderedFileView;

function FitFileName(const AFileName: UTF8String; ACanvas: TCanvas; AFile: TFile; ATargetWidth: Integer): UTF8String;
var
  Index: Integer;
begin
  Result:= AFileName;
  if ACanvas.TextWidth(AFileName) - ATargetWidth > 0 then
  begin
    repeat
      Index:= UTF8Length(Result);
      UTF8Delete(Result, Index, 1);
    until (ACanvas.TextWidth(Result) - ATargetWidth < 1) or (Index = 0);
    if (Index > 0) then
    begin
      Result:= UTF8Copy(Result, 1, Index - 3);
      if gDirBrackets and (AFile.IsDirectory or AFile.IsLinkToDirectory) then
        Result:= Result + '..]'
      else
        Result:= Result + '...';
    end;
  end;
end;

{ TFileViewGrid }

procedure TFileViewGrid.InitializeWnd;
begin
  inherited InitializeWnd;
  FFileView.InitializeDragDropEx(Self);
end;

procedure TFileViewGrid.DoOnResize;
begin
  inherited DoOnResize;
  CalculateColRowCount;
  CalculateColumnWidth;
end;

procedure TFileViewGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
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
end;

procedure TFileViewGrid.RowHeightsChanged;
begin
  inherited RowHeightsChanged;
  CalculateColRowCount;
end;

procedure TFileViewGrid.ColWidthsChanged;
begin
  inherited ColWidthsChanged;
  CalculateColRowCount;
end;

function TFileViewGrid.MouseOnGrid(X, Y: LongInt): Boolean;
var
  bTemp: Boolean;
  iRow, iCol: LongInt;
begin
  bTemp:= AllowOutboundEvents;
  AllowOutboundEvents:= False;
  MouseToCell(X, Y, iCol, iRow);
  AllowOutboundEvents:= bTemp;
  Result:= not (CellToIndex(iCol, iRow) < 0);
end;

procedure TFileViewGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if FFileView.IsLoadingFileList then Exit;

{$IF DECLARED(lcl_fullversion) and (lcl_fullversion >= 093100)}
  // Don't scroll partially visible cells on mouse click
  Options:= Options + [goDontScrollPartCell];
{$ENDIF}

{$IFDEF LCLGTK2}
  // Workaround for two doubleclicks being sent on GTK.
  // MouseDown event is sent just before doubleclick, so if we drop
  // doubleclick events we have to also drop MouseDown events that precede them.
  if FFileView.TooManyDoubleClicks then Exit;
{$ENDIF}

  FFileView.FMainControlMouseDown := True;

  if MouseOnGrid(X, Y) then
    inherited MouseDown(Button, Shift, X, Y)
  else
    begin
      if Assigned(OnMouseDown) then
        OnMouseDown(Self, Button, Shift, X, Y);
    end;
end;

procedure TFileViewGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  BackgroundClick: Boolean;
  Point: TPoint;
begin
  if FFileView.IsLoadingFileList then Exit;

{$IF DECLARED(lcl_fullversion) and (lcl_fullversion >= 093100)}
  // Don't scroll partially visible cells on mouse click
  Options:= Options - [goDontScrollPartCell];
{$ENDIF}

{$IFDEF LCLGTK2}
  // Workaround for two doubleclicks being sent on GTK.
  // MouseUp event is sent just after doubleclick, so if we drop
  // doubleclick events we have to also drop MouseUp events that follow them.
  if FFileView.TooManyDoubleClicks then Exit;
{$ENDIF}

  // Handle only if button-up was not lifted to finish drag&drop operation.
  if not FFileView.FMainControlMouseDown then
    Exit;

  inherited MouseUp(Button, Shift, X, Y);

  FFileView.FMainControlMouseDown := False;

  if Button = mbRight then
    begin
      { If right click on file/directory }
      if ((gMouseSelectionButton <> 1) or not gMouseSelectionEnabled) then
        begin
          BackgroundClick:= not MouseOnGrid(X, Y);
          Point := ClientToScreen(Classes.Point(X, Y));
          frmMain.Commands.DoContextMenu(FFileView, Point.x, Point.y, BackgroundClick);
        end
      else if (gMouseSelectionEnabled and (gMouseSelectionButton = 1)) then
        begin
          FFileView.tmContextMenu.Enabled:= False; // stop context menu timer
        end;
    end
  { Open folder in new tab on middle click }
  else if (Button = mbMiddle) then
    begin
      frmMain.Commands.cm_OpenDirInNewTab([]);
    end;
end;

procedure TFileViewGrid.TopLeftChanged;
begin
  inherited TopLeftChanged;
  FFileView.Notify([fvnVisibleFilePropertiesChanged]);
end;

procedure TFileViewGrid.DrawLines(aIdx, aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
begin
  // Draw frame cursor.
  if gUseFrameCursor and (gdSelected in aState) and FFileView.Active then
  begin
    Canvas.Pen.Color := gCursorColor;
    Canvas.Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
    Canvas.Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
  end;

  // Draw drop selection.
  if (FFileView.FDropFileIndex >= 0) and (aIdx = FFileView.FDropFileIndex) then
  begin
    Canvas.Pen.Color := gForeColor;
    Canvas.Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
    Canvas.Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
  end;
end;

procedure TFileViewGrid.PrepareColors(AFile: TDisplayFile; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  TextColor: TColor = clDefault;
  BackgroundColor: TColor;
  IsCursor: Boolean;
begin
  Canvas.Font.Name   := gFonts[dcfMain].Name;
  Canvas.Font.Size   := gFonts[dcfMain].Size;
  Canvas.Font.Style  := gFonts[dcfMain].Style;

  IsCursor := (gdSelected in aState) and FFileView.Active and (not gUseFrameCursor);
  // Set up default background color first.
  if IsCursor then
    BackgroundColor := gCursorColor
  else
    begin
      // Alternate rows background color.
      if odd(ARow) then
        BackgroundColor := gBackColor
      else
        BackgroundColor := gBackColor2;
    end;

  // Set text color.
  TextColor := AFile.TextColor;
  if (TextColor = clDefault) or (TextColor = clNone) then
    TextColor := gForeColor;

  if AFile.Selected then
  begin
    if gUseInvertedSelection then
      begin
        //------------------------------------------------------
        if IsCursor then
          begin
            TextColor := InvertColor(gCursorText);
          end
        else
          begin
            BackgroundColor := gMarkColor;
            TextColor := TextColor;
          end;
        //------------------------------------------------------
      end
    else
      begin
        TextColor := gMarkColor;
      end;
  end
  else if IsCursor then
    begin
      TextColor := gCursorText;
    end;

  BackgroundColor := FFileView.DimColor(BackgroundColor);

  if AFile.RecentlyUpdatedPct <> 0 then
  begin
    TextColor := LightColor(TextColor, AFile.RecentlyUpdatedPct);
    BackgroundColor := LightColor(BackgroundColor, AFile.RecentlyUpdatedPct);
  end;

  // Draw background.
  Canvas.Brush.Color := BackgroundColor;
  Canvas.FillRect(aRect);
  Canvas.Font.Color := TextColor;
end;

constructor TFileViewGrid.Create(AOwner: TComponent; AParent: TWinControl);
begin
  FFileView := AParent as TFileViewWithGrid;

  inherited Create(AOwner);

  // Workaround for Lazarus issue 18832.
  // Set Fixed... before setting ...Count.
  FixedRows := 0;
  FixedCols := 0;

  // Override default values to start with one column and one rows.
  RowCount := 1;
  ColCount := 1;

  DefaultColWidth:= 200;

  Self.Parent := AParent;

  DoubleBuffered := True;
  Align := alClient;
  MouseWheelOption:= mwGrid;
  Options := [goTabs, goThumbTracking, goSmoothScroll];
  TabStop := False;

  UpdateView;
end;

procedure TFileViewGrid.FinalizeWnd;
begin
  FFileView.FinalizeDragDropEx(Self);
  inherited FinalizeWnd;
end;

{ TFileViewWithGrid }

procedure TFileViewWithGrid.RedrawFile(DisplayFile: TDisplayFile);
var
  ACol, ARow: Integer;
begin
  dgPanel.IndexToCell(PtrInt(DisplayFile.DisplayItem), ACol, ARow);
  dgPanel.InvalidateCell(ACol, ARow);
end;

procedure TFileViewWithGrid.RedrawFiles;
begin
  dgPanel.Invalidate;
end;

procedure TFileViewWithGrid.MakeColumnsStrings(AFile: TDisplayFile);
begin
  AFile.DisplayStrings.Text:= FormatFileFunction('DC().GETFILENAME{}', AFile.FSFile, FileSource);
end;

procedure TFileViewWithGrid.RedrawFile(FileIndex: PtrInt);
var
  ACol, ARow: Integer;
begin
  dgPanel.IndexToCell(FileIndex, ACol, ARow);
  dgPanel.InvalidateCell(ACol, ARow);
end;

procedure TFileViewWithGrid.DisplayFileListChanged;
begin
  dgPanel.CalculateColRowCount;
  dgPanel.CalculateColumnWidth;
  SetFilesDisplayItems;

  if SetActiveFileNow(RequestedActiveFile) then
    RequestedActiveFile := ''
  else
    // Requested file was not found, restore position to last active file.
    SetActiveFileNow(LastActiveFile);

  Notify([fvnVisibleFilePropertiesChanged]);

  inherited DisplayFileListChanged;
end;

procedure TFileViewWithGrid.CreateDefault(AOwner: TWinControl);
begin
  inherited CreateDefault(AOwner);
  dgPanel:= GetFileViewGridClass.Create(Self, Self);
  MainControl := dgPanel;

  TabHeader:= TFileViewFixedHeader.Create(Self, Self);
  TabHeader.Top:= pnlHeader.Height;

  lblDetails:= TLabel.Create(pnlFooter);
  lblDetails.Align:= alRight;
  lblDetails.Alignment:= taRightJustify;
  lblDetails.Parent:= pnlFooter;

  dgPanel.OnSelection:= @dgPanelSelection;

  // By default always use some properties.
  FilePropertiesNeeded := [fpName,
                           fpSize,            // For info panel (total size, selected size)
                           fpAttributes,      // For distinguishing directories
                           fpLink,            // For distinguishing directories (link to dir) and link icons
                           fpModificationTime // For selecting/coloring files (by SearchTemplate)
                          ];
end;

procedure TFileViewWithGrid.BeforeMakeFileList;
begin
  inherited BeforeMakeFileList;
end;

procedure TFileViewWithGrid.FileSourceFileListLoaded;
begin
  inherited;

  FUpdatingActiveFile := True;
  dgPanel.MoveExtend(False, 0, 0);
  FUpdatingActiveFile := False;
end;

procedure TFileViewWithGrid.ClearAfterDragDrop;
begin
  inherited ClearAfterDragDrop;

  // reset TCustomGrid state
  dgPanel.FGridState := gsNormal;
end;

procedure TFileViewWithGrid.AfterChangePath;
begin
  inherited AfterChangePath;

  if not IsLoadingFileList then
  begin
    FUpdatingActiveFile := True;
    dgPanel.MoveExtend(False, 0, 0);
    FUpdatingActiveFile := False;
  end;
end;

function TFileViewWithGrid.GetActiveFileIndex: PtrInt;
begin
  Result := dgPanel.CellToIndex(dgPanel.Col, dgPanel.Row);
end;

function TFileViewWithGrid.GetFileIndexFromCursor(X, Y: Integer; out AtFileList: Boolean): PtrInt;
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
    Result:= CellToIndex(iCol, iRow);
    AtFileList := True; // Always at file list because header in dgPanel not used
  end;
end;

function TFileViewWithGrid.GetFileRect(FileIndex: PtrInt): TRect;
var
  ACol, ARow: Integer;
begin
  dgPanel.IndexToCell(FileIndex, ACol, ARow);
  Result := dgPanel.CellRect(ACol, ARow);
end;

procedure TFileViewWithGrid.DoOnResize;
var
  I: Integer;
  AWidth: Integer;
begin
  inherited DoOnResize;

  if Assigned(TabHeader) then
  begin
    AWidth:= Width div TabHeader.Sections.Count;
    for I:= 0 to TabHeader.Sections.Count - 1 do
      TabHeader.Sections[I].Width:= AWidth;
  end;

  UpdateFooterDetails;
  Notify([fvnVisibleFilePropertiesChanged]);
end;

constructor TFileViewWithGrid.Create(AOwner: TWinControl; AConfig: TXmlConfig;
  ANode: TXmlNode; AFlags: TFileViewFlags = []);
begin
  inherited Create(AOwner, AConfig, ANode, AFlags);
end;

constructor TFileViewWithGrid.Create(AOwner: TWinControl; AFileView: TFileView;
  AFlags: TFileViewFlags);
var
  I: Integer;
begin
  inherited Create(AOwner, AFileView, AFlags);

  if (not (AFileView is TFileViewWithGrid)) and Assigned(FAllDisplayFiles) then
  begin
    // Update display strings in case FileView type have changed.
    for I := 0 to FAllDisplayFiles.Count - 1 do
      MakeColumnsStrings(FAllDisplayFiles[I]);
  end;
end;

destructor TFileViewWithGrid.Destroy;
begin
  inherited Destroy;
end;

procedure TFileViewWithGrid.CloneTo(FileView: TFileView);
begin
  if Assigned(FileView) then
  begin
    inherited CloneTo(FileView);

    if FileView is TFileViewWithGrid then
    with FileView as TFileViewWithGrid do
    begin
      TabHeader.UpdateSorting(Self.Sorting);
    end;
  end;
end;

procedure TFileViewWithGrid.AddFileSource(aFileSource: IFileSource; aPath: String);
begin
  inherited AddFileSource(aFileSource, aPath);

  if not IsLoadingFileList then
  begin
    FUpdatingActiveFile := True;
    dgPanel.MoveExtend(False, 0, 0);
    FUpdatingActiveFile := False;
  end;
end;

procedure TFileViewWithGrid.LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
begin
  inherited LoadConfiguration(AConfig, ANode);
  TabHeader.UpdateSorting(Sorting);
end;

procedure TFileViewWithGrid.SetActiveFile(FileIndex: PtrInt);
var
  ACol, ARow: Integer;
begin
  dgPanel.IndexToCell(FileIndex, ACol, ARow);
  dgPanel.Col := ACol;
  dgPanel.Row := ARow;
end;

procedure TFileViewWithGrid.SetFilesDisplayItems;
var
  i: Integer;
begin
  for i := 0 to FFiles.Count - 1 do
    FFiles[i].DisplayItem := Pointer(i);
end;

procedure TFileViewWithGrid.UpdateFooterDetails;
var
  AFile: TFile;
  AFileName: UTF8String;
begin
  if FSelectedCount > 0 then
    lblDetails.Caption:= EmptyStr
  else
    begin
      AFile:= CloneActiveFile;
      if Assigned(AFile) then
      try
        // Get details info about file
        AFileName:= #32#32 +FormatFileFunction('DC().GETFILEEXT{}', AFile, FileSource);
        AFileName:= AFileName + #32#32 + FormatFileFunction('DC().GETFILESIZE{}', AFile, FileSource);
        AFileName:= AFileName + #32#32 + FormatFileFunction('DC().GETFILETIME{}', AFile, FileSource);
        AFileName:= AFileName + #32#32 + FormatFileFunction('DC().GETFILEATTR{}', AFile, FileSource);
        lblDetails.Caption:= AFileName;
        // Get file name
        AFileName:= FormatFileFunction('DC().GETFILENAMENOEXT{}', AFile, FileSource);
        lblInfo.Caption:= FitFileName(AFileName, lblInfo.Canvas, AFile, lblInfo.ClientWidth);
      finally
        AFile.Free;
      end;
    end;
end;

procedure TFileViewWithGrid.dgPanelSelection(Sender: TObject; aCol, aRow: Integer);
begin
  DoFileIndexChanged(dgPanel.CellToIndex(aCol, aRow));
  UpdateFooterDetails;
end;

procedure TFileViewWithGrid.UpdateInfoPanel;

begin
  inherited UpdateInfoPanel;
  UpdateFooterDetails;
end;

procedure TFileViewWithGrid.DoUpdateView;

  function CalculateTabHeaderHeight: Integer;
  var
    OldFont: TFont;
  begin
    with TabHeader do
    begin
      OldFont     := Canvas.Font;
      Canvas.Font := Font;
      Result      := Canvas.TextHeight('Wg');
      Canvas.Font := OldFont;
    end;
  end;

var
  TabHeaderHeight: Integer;
begin
  inherited DoUpdateView;
  dgPanel.UpdateView;
  TabHeader.Visible := gTabHeader;
  // Set rows of header.
  if gTabHeader then
  begin
    TabHeader.UpdateHeader;

    TabHeaderHeight := Max(gIconsSize, CalculateTabHeaderHeight);
    TabHeaderHeight := TabHeaderHeight + 2; // for borders
    if not gInterfaceFlat then
    begin
      TabHeaderHeight := TabHeaderHeight + 2; // additional borders if not flat
    end;
    TabHeader.Height := TabHeaderHeight;
  end;
  Notify([fvnVisibleFilePropertiesChanged]);
end;

procedure TFileViewWithGrid.SetSorting(const NewSortings: TFileSortings);
begin
  inherited SetSorting(NewSortings);
  TabHeader.UpdateSorting(NewSortings);
end;

constructor TFileViewWithGrid.Create(AOwner: TWinControl;
  AFileSource: IFileSource; APath: String; AFlags: TFileViewFlags);
begin
  inherited Create(AOwner, AFileSource, APath, AFlags);
end;

procedure TFileViewWithGrid.DoFileUpdated(AFile: TDisplayFile; UpdatedProperties: TFilePropertiesTypes);
begin
  MakeColumnsStrings(AFile);
  inherited DoFileUpdated(AFile, UpdatedProperties);
end;

procedure TFileViewWithGrid.DoHandleKeyDown(var Key: Word; Shift: TShiftState);
var
  Index,
  aCol, aRow: Integer;
  AFile: TDisplayFile;
begin
  case Key of
    VK_INSERT:
      begin
        if not IsEmpty then
        begin
          Index:= GetActiveFileIndex;
          if IsFileIndexInRange(Index) then
          begin
            AFile := FFiles[Index];
            if IsItemValid(AFile) then
            begin
              InvertFileSelection(AFile, False);
              DoSelectionChanged(Index);
            end;
            dgPanel.IndexToCell(Index + 1, aCol, aRow);
            if not ((aCol < 0) and (aRow < 0)) then
            begin
              dgPanel.Col:= aCol;
              dgPanel.Row:= aRow;
            end;
          end;
        end;
        Key := 0;
      end;

    VK_SPACE:
      if Shift * KeyModifiersShortcut = [] then
      begin
        Index:= GetActiveFileIndex;
        if IsFileIndexInRange(Index) then
        begin
          AFile := FFiles[Index];
          if IsItemValid(aFile) then
          begin
            if (aFile.FSFile.IsDirectory or
               aFile.FSFile.IsLinkToDirectory) and
               not aFile.Selected then
            begin
              CalculateSpace(aFile);
            end;

            InvertFileSelection(aFile, False);
            DoSelectionChanged(Index);

            if gSpaceMovesDown then
            begin
              dgPanel.IndexToCell(Index + 1, aCol, aRow);
              if not ((aCol < 0) and (aRow < 0)) then
              begin
                dgPanel.Col:= aCol;
                dgPanel.Row:= aRow;
              end;
            end;
          end;
        end;
        Key := 0;
      end;
  end;

  inherited DoHandleKeyDown(Key, Shift);
end;

procedure TFileViewWithGrid.DoMainControlShowHint(FileIndex: PtrInt; X, Y: Integer);
var
  aRect: TRect;
  ACol, ARow, iCol: Integer;
  AFile: TDisplayFile;
begin
  AFile := FFiles[FileIndex];
  dgPanel.IndexToCell(FileIndex, ACol, ARow);
  aRect:= dgPanel.CellRect(ACol, ARow);
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

end.

