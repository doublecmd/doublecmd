unit uBriefFileView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LMessages, Grids, Graphics, StdCtrls,
  uDisplayFile, DCXmlConfig, uFileSorting, uFileProperty, uTypes,
  uFileViewWithMainCtrl, uFileViewHeader, uFileView, uFileSource;

type

  TBriefFileView = class;

  { TBriefDrawGrid }

  TBriefDrawGrid = class(TDrawGrid)
  private
    BriefView: TBriefFileView;
    procedure CalculateColRowCount;
    procedure CalculateColumnWidth;
    function  CellToIndex(ACol, ARow: Integer): Integer;
    procedure IndexToCell(Index: Integer; out ACol, ARow: Integer);
  protected
    procedure UpdateView;
    procedure RowHeightsChanged; override;
    procedure ColWidthsChanged;  override;
    procedure FinalizeWnd; override;
    procedure InitializeWnd; override;
    function MouseOnGrid(X, Y: LongInt): Boolean;
    function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DoOnResize; override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MoveSelection; override;
  public
    constructor Create(AOwner: TComponent; AParent: TWinControl); reintroduce;

    procedure DrawCell(aCol, aRow: Integer; aRect: TRect;
              aState: TGridDrawState); override;
  end;

  { TBriefFileView }

  TBriefFileView = class (TFileViewWithMainCtrl)
    private
      TabHeader: TFileViewFixedHeader;
      dgPanel: TBriefDrawGrid;
      lblDetails: TLabel;

      procedure MakeColumnsStrings(AFile: TDisplayFile);
      procedure SetFilesDisplayItems;
      procedure UpdateFooterDetails;

      procedure dgPanelTopLeftChanged(Sender: TObject);
      procedure dgPanelSelection(Sender: TObject; aCol, aRow: Integer);
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
      function GetVisibleFilesIndexes: TRange; override;
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

    function Clone(NewParent: TWinControl): TBriefFileView; override;
    procedure CloneTo(FileView: TFileView); override;

    procedure AddFileSource(aFileSource: IFileSource; aPath: String); override;

    procedure LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); override;
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); override;
  end;

implementation

uses
  LCLIntf, LCLType, LCLVersion, LCLProc, math,
  uGlobs, uPixmapManager,
  uDCUtils, fMain,
  uFile,
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

{ TBriefDrawGrid }

procedure TBriefDrawGrid.CalculateColRowCount;
var
  glw, bw: Integer;
  AIndex, ACol, ARow: Integer;
begin
  if (csDesigning in ComponentState) then Exit;

  if not Assigned(BriefView.FFiles) then Exit;

  glw := Max(GridLineWidth, 1);
  bw  := Max(BorderWidth, 1);

  if DefaultRowHeight > 0 then
  begin
    // Save active file index
    AIndex:= CellToIndex(Col, Row);

    RowCount := (Height - GetSystemMetrics(SM_CYHSCROLL) -
                 glw - (2 * bw)) div (DefaultRowHeight + glw);
    if RowCount > 0 then
    ColCount := (BriefView.FFiles.Count + RowCount - 1) div RowCount;

    // Restore active file index
    IndexToCell(AIndex, ACol, ARow);
    MoveExtend(False, ACol, ARow);
  end;
  Invalidate;
end;

procedure TBriefDrawGrid.CalculateColumnWidth;
var
  I, J, L, M: Integer;
begin
  if not Assigned(BriefView.FFiles) then Exit;
  if BriefView.FFiles.Count < 2 then
    DefaultColWidth:= ClientWidth div 3
  else
    begin
      J:= 0;
      M:= 0;
      for I:= 0 to BriefView.FFiles.Count - 1 do
      begin
        L:= Length(BriefView.FFiles[I].FSFile.Name);
        if L > M then
        begin
          M:= L;
          J:= I;
        end;
      end;
      Canvas.Font.Name   := gFonts[dcfMain].Name;
      Canvas.Font.Size   := gFonts[dcfMain].Size;
      Canvas.Font.Style  := gFonts[dcfMain].Style;
      M:= Canvas.TextWidth(BriefView.FFiles[J].FSFile.Name + 'WWW');
      if (gShowIcons = sim_none) then
        M:= M + 2
      else
        M:= M + gIconsSize + 4;
      if M > ClientWidth then M:= ClientWidth - 4;
      DefaultColWidth:= M;
    end;
end;

function TBriefDrawGrid.CellToIndex(ACol, ARow: Integer): Integer;
begin
  if (ARow < 0) or (ARow >= RowCount) or (ACol <  0) or (ACol >= ColCount) then Exit(-1);
  Result:= ACol * RowCount + ARow;
  if (Result < 0) or (Result >= BriefView.FFiles.Count) then
    Result:= -1;
end;

procedure TBriefDrawGrid.IndexToCell(Index: Integer; out ACol, ARow: integer);
begin
  if (Index < 0) or (Index >= BriefView.FFiles.Count) then
    begin
      ACol:= -1;
      ARow:= -1;
    end
  else
    begin
      ACol:= Index div RowCount;
      ARow:= Index mod RowCount;
    end;
end;

procedure TBriefDrawGrid.InitializeWnd;
begin
  inherited InitializeWnd;
  BriefView.InitializeDragDropEx(Self);
end;

procedure TBriefDrawGrid.UpdateView;

  function CalculateDefaultRowHeight: Integer;
  var
    OldFont, NewFont: TFont;
    MaxFontHeight: Integer = 0;
    CurrentHeight: Integer;
  begin
    // Start with height of the icons.
    if gShowIcons <> sim_none then
      MaxFontHeight := gIconsSize;

    // Assign temporary font.
    OldFont     := Canvas.Font;
    NewFont     := TFont.Create;
    Canvas.Font := NewFont;

    // Search columns settings for the biggest font (in height).
    Canvas.Font.Name  := gFonts[dcfMain].Name;
    Canvas.Font.Style := gFonts[dcfMain].Style;
    Canvas.Font.Size  := gFonts[dcfMain].Size;

    CurrentHeight := Canvas.GetTextHeight('Wg');
    MaxFontHeight := Max(MaxFontHeight, CurrentHeight);

    // Restore old font.
    Canvas.Font := OldFont;
    FreeAndNil(NewFont);

    Result := MaxFontHeight;
  end;

var
  TempRowHeight: Integer;
begin
  Flat := gInterfaceFlat;

  // Calculate row height.
  TempRowHeight := CalculateDefaultRowHeight;
  if TempRowHeight > 0 then
    DefaultRowHeight := TempRowHeight;

  // Calculate column width
  CalculateColumnWidth;
end;

procedure TBriefDrawGrid.DoOnResize;
begin
  inherited DoOnResize;
  CalculateColRowCount;
  CalculateColumnWidth;
end;

procedure TBriefDrawGrid.RowHeightsChanged;
begin
  inherited RowHeightsChanged;
  CalculateColRowCount;
end;

procedure TBriefDrawGrid.ColWidthsChanged;
begin
  inherited ColWidthsChanged;
  CalculateColRowCount;
end;

function TBriefDrawGrid.MouseOnGrid(X, Y: LongInt): Boolean;
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

function TBriefDrawGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if not BriefView.IsLoadingFileList then
  begin
    Result:= inherited DoMouseWheelDown(Shift, MousePos);
    Result:= Perform(LM_HSCROLL, SB_LINERIGHT, 0) = 0;
  end
  else
    Result := True; // Handled
end;

function TBriefDrawGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if not BriefView.IsLoadingFileList then
  begin
    Result:= inherited DoMouseWheelUp(Shift, MousePos);
    Result:= Perform(LM_HSCROLL, SB_LINELEFT, 0) = 0;
  end
  else
    Result := True; // Handled
end;

procedure TBriefDrawGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if BriefView.IsLoadingFileList then Exit;

{$IF DECLARED(lcl_fullversion) and (lcl_fullversion >= 093100)}
  // Don't scroll partially visible cells on mouse click
  Options:= Options + [goDontScrollPartCell];
{$ENDIF}

{$IFDEF LCLGTK2}
  // Workaround for two doubleclicks being sent on GTK.
  // MouseDown event is sent just before doubleclick, so if we drop
  // doubleclick events we have to also drop MouseDown events that precede them.
  if BriefView.TooManyDoubleClicks then Exit;
{$ENDIF}

  BriefView.FMainControlMouseDown := True;

  if MouseOnGrid(X, Y) then
    inherited MouseDown(Button, Shift, X, Y)
  else
    begin
      if Assigned(OnMouseDown) then
        OnMouseDown(Self, Button, Shift, X, Y);
    end;
end;

procedure TBriefDrawGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
  procedure Scroll(ScrollCode: SmallInt);
  var
    Msg: TLMHScroll;
  begin
    Msg.Msg := LM_HSCROLL;
    Msg.ScrollCode := ScrollCode;
    Msg.SmallPos := 1; // How many lines scroll
    Msg.ScrollBar := Handle;
    Dispatch(Msg);
  end;
begin
  inherited MouseMove(Shift, X, Y);
  if DragManager.IsDragging or BriefView.IsMouseSelecting then
  begin
    if X < 25 then
      Scroll(SB_LINEUP)
    else if X > ClientWidth - 25 then
      Scroll(SB_LINEDOWN);
  end;
end;

procedure TBriefDrawGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  BackgroundClick: Boolean;
  Point: TPoint;
begin
  if BriefView.IsLoadingFileList then Exit;

{$IF DECLARED(lcl_fullversion) and (lcl_fullversion >= 093100)}
  // Don't scroll partially visible cells on mouse click
  Options:= Options - [goDontScrollPartCell];
{$ENDIF}

{$IFDEF LCLGTK2}
  // Workaround for two doubleclicks being sent on GTK.
  // MouseUp event is sent just after doubleclick, so if we drop
  // doubleclick events we have to also drop MouseUp events that follow them.
  if BriefView.TooManyDoubleClicks then Exit;
{$ENDIF}

  // Handle only if button-up was not lifted to finish drag&drop operation.
  if not BriefView.FMainControlMouseDown then
    Exit;

  inherited MouseUp(Button, Shift, X, Y);

  BriefView.FMainControlMouseDown := False;

  if Button = mbRight then
    begin
      { If right click on file/directory }
      if ((gMouseSelectionButton <> 1) or not gMouseSelectionEnabled) then
        begin
          BackgroundClick:= not MouseOnGrid(X, Y);
          Point := ClientToScreen(Classes.Point(X, Y));
          frmMain.Commands.DoContextMenu(BriefView, Point.x, Point.y, BackgroundClick);
        end
      else if (gMouseSelectionEnabled and (gMouseSelectionButton = 1)) then
        begin
          BriefView.tmContextMenu.Enabled:= False; // stop context menu timer
        end;
    end
  { Open folder in new tab on middle click }
  else if (Button = mbMiddle) then
    begin
      frmMain.Commands.cm_OpenDirInNewTab([]);
    end;
end;

procedure TBriefDrawGrid.MoveSelection;
begin
  inherited MoveSelection;
  BriefView.DoFileIndexChanged(CellToIndex(Col, Row));
end;

procedure TBriefDrawGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  SavedKey: Word;
  FileIndex: Integer;
  ACol, ARow: Integer;
begin
  if BriefView.IsLoadingFileList then
  begin
    BriefView.HandleKeyDownWhenLoading(Key, Shift);
    Exit;
  end;

  SavedKey := Key;
  // Set RangeSelecting before cursor is moved.
  BriefView.FRangeSelecting :=
    (ssShift in Shift) and
    (SavedKey in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END, VK_PRIOR, VK_NEXT]);

  case Key of
    VK_LEFT:
      begin
        if (Col - 1 < 0) then
        begin
          MoveExtend(False, 0, 0);
          Key:= 0;
        end;
      end;
    VK_RIGHT:
      begin
        if (CellToIndex(Col + 1, Row) < 0) then
        begin
          IndexToCell(BriefView.FFiles.Count - 1, ACol, ARow);
          MoveExtend(False, ACol, ARow);
          Key:= 0;
        end;
      end;
    VK_PRIOR:
      begin
        FileIndex:= CellToIndex(Col, Row) - (VisibleRowCount - 1);
        if FileIndex < 0 then
          FileIndex:= 0;
        IndexToCell(FileIndex, ACol, ARow);
        MoveExtend(False, ACol, ARow);
        Key:= 0;
      end;
    VK_NEXT:
      begin
        FileIndex:= CellToIndex(Col, Row) + (VisibleRowCount - 1);
        if FileIndex >= BriefView.FFiles.Count then
          FileIndex:= BriefView.FFiles.Count - 1;
        IndexToCell(FileIndex, ACol, ARow);
        MoveExtend(False, ACol, ARow);
        Key:= 0;
      end;
    VK_HOME:
      begin
        MoveExtend(False, 0, 0);
        Key:= 0;
      end;
    VK_END:
      begin
        IndexToCell(BriefView.FFiles.Count - 1, ACol, ARow);
        MoveExtend(False, ACol, ARow);
        Key:= 0;
      end;
    VK_UP, VK_DOWN:
      begin
        if (CellToIndex(Col, Row) >= BriefView.FFiles.Count - 1) and
           (Key = VK_DOWN) then
          begin
            Key:= 0;
          end
        else if ((Row = RowCount-1) and (Key = VK_DOWN)) then
          begin
            if (Col < ColCount - 1) then
            begin
              Row:= 0;
              Col:= Col + 1;
            end;
            Key:= 0;
          end
        else if (Row = FixedRows) and (Key = VK_UP) then
          begin
            if (Col > 0) then
            begin
              Row:= RowCount - 1;
              Col:= Col - 1;
            end;
            Key:= 0;
          end;
      end;
  end;
  inherited KeyDown(Key, Shift);

  if ssShift in Shift then
  begin
    FileIndex := CellToIndex(Col, Row);
    if FileIndex <> InvalidFileIndex then
      BriefView.Selection(SavedKey, FileIndex);
  end;
end;

constructor TBriefDrawGrid.Create(AOwner: TComponent; AParent: TWinControl);
begin
  BriefView := AParent as TBriefFileView;

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

procedure TBriefDrawGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  Idx: Integer;
  //shared variables
  s:   string;
  iTextTop: Integer;
  AFile: TDisplayFile;
  FileSourceDirectAccess: Boolean;

  //------------------------------------------------------
  //begin subprocedures
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

      s := AFile.DisplayStrings[0];
      Y:= (DefaultColWidth - 4 - Canvas.TextWidth('W'));
      if (gShowIcons <> sim_none) then Y:= Y - gIconsSize;
      s:= FitFileName(s, Canvas, AFile.FSFile, Y);

      if (gShowIcons <> sim_none) then
        Canvas.TextOut(aRect.Left + gIconsSize + 4, iTextTop, s)
      else
        Canvas.TextOut(aRect.Left + 2, iTextTop, s);
    end; //of DrawIconCell
    //------------------------------------------------------

  procedure PrepareColors;
  //------------------------------------------------------
  var
    TextColor: TColor = -1;
    BackgroundColor: TColor;
    IsCursor: Boolean;
  //---------------------
  begin
    Canvas.Font.Name   := gFonts[dcfMain].Name;
    Canvas.Font.Size   := gFonts[dcfMain].Size;
    Canvas.Font.Style  := gFonts[dcfMain].Style;

    IsCursor := (gdSelected in aState) and BriefView.Active and (not gUseFrameCursor);
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
    TextColor := gColorExt.GetColorBy(AFile.FSFile);
    if TextColor = -1 then TextColor := gForeColor;

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

    BackgroundColor := BriefView.DimColor(BackgroundColor);

    if AFile.RecentlyUpdatedPct <> 0 then
    begin
      TextColor := LightColor(TextColor, AFile.RecentlyUpdatedPct);
      BackgroundColor := LightColor(BackgroundColor, AFile.RecentlyUpdatedPct);
    end;

    // Draw background.
    Canvas.Brush.Color := BackgroundColor;
    Canvas.FillRect(aRect);
    Canvas.Font.Color := TextColor;
  end;// of PrepareColors;

  procedure DrawLines;
  begin
    // Draw frame cursor.
    if gUseFrameCursor and (gdSelected in aState) and BriefView.Active then
    begin
      Canvas.Pen.Color := gCursorColor;
      Canvas.Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
      Canvas.Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
    end;

    // Draw drop selection.
    if (BriefView.FDropFileIndex >= 0) and (Idx = BriefView.FDropFileIndex) then
    begin
      Canvas.Pen.Color := gForeColor;
      Canvas.Line(aRect.Left, aRect.Top, aRect.Right, aRect.Top);
      Canvas.Line(aRect.Left, aRect.Bottom - 1, aRect.Right, aRect.Bottom - 1);
    end;
  end;
  //------------------------------------------------------
  //end of subprocedures
  //------------------------------------------------------

begin
  Idx:= CellToIndex(aCol, aRow);
  if (Idx >= 0) and (BriefView.FFiles.Count > 0) then
    begin
      AFile:= BriefView.FFiles[Idx];
      FileSourceDirectAccess:= fspDirectAccess in BriefView.FileSource.Properties;
      if AFile.DisplayStrings.Count = 0 then
        BriefView.MakeColumnsStrings(AFile);

      PrepareColors;

      iTextTop := aRect.Top + (RowHeights[aRow] - Canvas.TextHeight('Wg')) div 2;

      DrawIconCell;
    end
  else
    begin
      // Draw background.
      Canvas.Brush.Color := BriefView.DimColor(gBackColor);
      Canvas.FillRect(aRect);
    end;

  DrawCellGrid(aCol,aRow,aRect,aState);
  DrawLines;
end;

procedure TBriefDrawGrid.FinalizeWnd;
begin
  BriefView.FinalizeDragDropEx(Self);
  inherited FinalizeWnd;
end;

{ TBriefFileView }

function TBriefFileView.GetVisibleFilesIndexes: TRange; {Done}
begin
  with dgPanel do
  begin
    if (TopRow < 0) or (csLoading in ComponentState) then
      begin
        Result.First:= 0;
        Result.Last:= -1;
      end
    else
      begin
        Result.First:= (LeftCol * VisibleRowCount - 1);
        Result.Last:=  (LeftCol + VisibleColCount + 1) * VisibleRowCount - 1;
        if Result.First < 0 then Result.First:= 0;
        if Result.Last >= FFiles.Count then Result.Last:= FFiles.Count - 1;
      end;
  end;
end;

procedure TBriefFileView.RedrawFile(DisplayFile: TDisplayFile);
var
  ACol, ARow: Integer;
begin
  dgPanel.IndexToCell(PtrInt(DisplayFile.DisplayItem), ACol, ARow);
  dgPanel.InvalidateCell(ACol, ARow);
end;

procedure TBriefFileView.RedrawFiles;
begin
  dgPanel.Invalidate;
end;

procedure TBriefFileView.MakeColumnsStrings(AFile: TDisplayFile);
begin
  AFile.DisplayStrings.Text:= FormatFileFunction('DC().GETFILENAME{}', AFile.FSFile, FileSource);
end;

procedure TBriefFileView.RedrawFile(FileIndex: PtrInt);
var
  ACol, ARow: Integer;
begin
  dgPanel.IndexToCell(FileIndex, ACol, ARow);
  dgPanel.InvalidateCell(ACol, ARow);
end;

procedure TBriefFileView.dgPanelTopLeftChanged(Sender: TObject);
begin
  Notify([fvnVisibleFilePropertiesChanged]);
end;

procedure TBriefFileView.dgPanelSelection(Sender: TObject; aCol, aRow: Integer);
begin
  DoFileIndexChanged(dgPanel.CellToIndex(aCol, aRow));
  UpdateFooterDetails;
end;

procedure TBriefFileView.DisplayFileListChanged;
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

procedure TBriefFileView.CreateDefault(AOwner: TWinControl);
begin
  inherited CreateDefault(AOwner);
  dgPanel:= TBriefDrawGrid.Create(Self, Self);
  MainControl := dgPanel;

  TabHeader:= TFileViewFixedHeader.Create(Self, Self);
  TabHeader.Top:= pnlHeader.Height;

  lblDetails:= TLabel.Create(pnlFooter);
  lblDetails.Align:= alRight;
  lblDetails.Alignment:= taRightJustify;
  lblDetails.Parent:= pnlFooter;

  dgPanel.OnSelection:= @dgPanelSelection;
  dgPanel.OnTopLeftChanged:= @dgPanelTopLeftChanged;

  // By default always use some properties.
  FilePropertiesNeeded := [fpName,
                           fpSize,            // For info panel (total size, selected size)
                           fpAttributes,      // For distinguishing directories
                           fpLink,            // For distinguishing directories (link to dir) and link icons
                           fpModificationTime // For selecting/coloring files (by SearchTemplate)
                          ];
end;

procedure TBriefFileView.BeforeMakeFileList;
begin
  inherited BeforeMakeFileList;
end;

procedure TBriefFileView.FileSourceFileListLoaded;
begin
  inherited;

  FUpdatingActiveFile := True;
  dgPanel.MoveExtend(False, 0, 0);
  FUpdatingActiveFile := False;
end;

procedure TBriefFileView.ClearAfterDragDrop;
begin
  inherited ClearAfterDragDrop;

  // reset TCustomGrid state
  dgPanel.FGridState := gsNormal;
end;

procedure TBriefFileView.AfterChangePath;
begin
  inherited AfterChangePath;

  if not IsLoadingFileList then
  begin
    FUpdatingActiveFile := True;
    dgPanel.MoveExtend(False, 0, 0);
    FUpdatingActiveFile := False;
  end;
end;

function TBriefFileView.GetActiveFileIndex: PtrInt;
begin
  Result := dgPanel.CellToIndex(dgPanel.Col, dgPanel.Row);
end;

function TBriefFileView.GetFileIndexFromCursor(X, Y: Integer; out AtFileList: Boolean): PtrInt;
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

function TBriefFileView.GetFileRect(FileIndex: PtrInt): TRect;
var
  ACol, ARow: Integer;
begin
  dgPanel.IndexToCell(FileIndex, ACol, ARow);
  Result := dgPanel.CellRect(ACol, ARow);
end;

procedure TBriefFileView.DoOnResize;
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

constructor TBriefFileView.Create(AOwner: TWinControl; AConfig: TXmlConfig;
  ANode: TXmlNode; AFlags: TFileViewFlags = []);
begin
  inherited Create(AOwner, AConfig, ANode, AFlags);
end;

constructor TBriefFileView.Create(AOwner: TWinControl; AFileView: TFileView;
  AFlags: TFileViewFlags);
var
  I: Integer;
begin
  inherited Create(AOwner, AFileView, AFlags);

  if (not (AFileView is TBriefFileView)) and Assigned(FAllDisplayFiles) then
  begin
    // Update display strings in case FileView type have changed.
    for I := 0 to FAllDisplayFiles.Count - 1 do
      MakeColumnsStrings(FAllDisplayFiles[I]);
  end;
end;

destructor TBriefFileView.Destroy;
begin
  inherited Destroy;
end;

function TBriefFileView.Clone(NewParent: TWinControl): TBriefFileView;
begin
  Result := TBriefFileView.Create(NewParent, Self);
end;

procedure TBriefFileView.CloneTo(FileView: TFileView);
begin
  if Assigned(FileView) then
  begin
    inherited CloneTo(FileView);

    if FileView is TBriefFileView then
    with FileView as TBriefFileView do
    begin
      TabHeader.UpdateSorting(Self.Sorting);
    end;
  end;
end;

procedure TBriefFileView.AddFileSource(aFileSource: IFileSource; aPath: String);
begin
  inherited AddFileSource(aFileSource, aPath);

  if not IsLoadingFileList then
  begin
    FUpdatingActiveFile := True;
    dgPanel.MoveExtend(False, 0, 0);
    FUpdatingActiveFile := False;
  end;
end;

procedure TBriefFileView.LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
begin
  inherited LoadConfiguration(AConfig, ANode);
  TabHeader.UpdateSorting(Sorting);
end;

procedure TBriefFileView.SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
begin
  inherited SaveConfiguration(AConfig, ANode);

  AConfig.SetAttr(ANode, 'Type', 'brief');
end;

procedure TBriefFileView.SetActiveFile(FileIndex: PtrInt);
var
  ACol, ARow: Integer;
begin
  dgPanel.IndexToCell(FileIndex, ACol, ARow);
  dgPanel.Col := ACol;
  dgPanel.Row := ARow;
end;

procedure TBriefFileView.SetFilesDisplayItems;
var
  i: Integer;
begin
  for i := 0 to FFiles.Count - 1 do
    FFiles[i].DisplayItem := Pointer(i);
end;

procedure TBriefFileView.UpdateFooterDetails;
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

procedure TBriefFileView.UpdateInfoPanel;

begin
  inherited UpdateInfoPanel;
  UpdateFooterDetails;
end;

procedure TBriefFileView.DoUpdateView;
begin
  inherited DoUpdateView;
  TabHeader.UpdateHeader;
  dgPanel.UpdateView;
  Notify([fvnVisibleFilePropertiesChanged]);
end;

procedure TBriefFileView.SetSorting(const NewSortings: TFileSortings);
begin
  inherited SetSorting(NewSortings);
  TabHeader.UpdateSorting(NewSortings);
end;

constructor TBriefFileView.Create(AOwner: TWinControl;
  AFileSource: IFileSource; APath: String; AFlags: TFileViewFlags);
begin
  inherited Create(AOwner, AFileSource, APath, AFlags);
end;

procedure TBriefFileView.DoFileUpdated(AFile: TDisplayFile; UpdatedProperties: TFilePropertiesTypes);
begin
  MakeColumnsStrings(AFile);
  inherited DoFileUpdated(AFile, UpdatedProperties);
end;

procedure TBriefFileView.DoHandleKeyDown(var Key: Word; Shift: TShiftState);
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
  end;

  inherited DoHandleKeyDown(Key, Shift);
end;

procedure TBriefFileView.DoMainControlShowHint(FileIndex: PtrInt; X, Y: Integer);
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

