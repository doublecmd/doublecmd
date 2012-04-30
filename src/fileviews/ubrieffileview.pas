unit uBriefFileView;

{$mode objfpc}{$H+}

interface

uses
  LMessages, Grids, uFileView, uFileSource, Graphics,
  Classes, SysUtils, Controls, ExtCtrls, ComCtrls, contnrs, fgl,
  uFile, uDisplayFile, uFormCommands, DCXmlConfig,
  DCClassesUtf8, uFileSorting, uFileViewHistory, uFileProperty, uFileViewWorker,
  uFunctionThread, uFileSystemWatcher, uTypes, uFileViewWithMainCtrl,
  uFileViewHeader;

type

  TBriefFileView = class;

  { TBriefDrawGrid }

  TBriefDrawGrid = class(TDrawGrid)
  private
    BriefView: TBriefFileView;
    procedure CalculateColRowCount(Data: PtrInt);
    function  CellToIndex(ACol, ARow: Integer): Integer;
    procedure IndexToCell(Index: Integer; out ACol, ARow: Integer);
  protected
    procedure UpdateView;
    procedure Resize; override;
    procedure RowHeightsChanged; override;
    procedure ColWidthsChanged;  override;
    procedure FinalizeWnd; override;
    procedure InitializeWnd; override;
    function MouseOnGrid(X, Y: LongInt): Boolean;
    function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
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

      procedure MakeColumnsStrings(AFile: TDisplayFile);
      procedure SetFilesDisplayItems;

      procedure dgPanelTopLeftChanged(Sender: TObject);
   protected
      procedure CreateDefault(AOwner: TWinControl); override;
      procedure BeforeMakeFileList; override;
      procedure AfterMakeFileList; override;
      procedure ClearAfterDragDrop; override;
      procedure AfterChangePath; override;
      procedure DoMainControlShowHint(FileIndex: PtrInt; X, Y: Integer); override;
      function GetActiveFileIndex: PtrInt; override;
      function GetFileIndexFromCursor(X, Y: Integer; out AtFileList: Boolean): PtrInt; override;
      function GetFileRect(FileIndex: PtrInt): TRect; override;
      function GetVisibleFilesIndexes: TRange; override;
      procedure RedrawFile(FileIndex: PtrInt); override;
      procedure RedrawFile(DisplayFile: TDisplayFile); override;
      procedure RedrawFiles; override;
      procedure Resize; override;
      procedure SetActiveFile(FileIndex: PtrInt); override;
      procedure DoFileUpdated(AFile: TDisplayFile; UpdatedProperties: TFilePropertiesTypes = []); override;
      procedure DoUpdateView; override;
      procedure SetSorting(const NewSortings: TFileSortings); override;
  public
    constructor Create(AOwner: TWinControl; AConfig: TXmlConfig; ANode: TXmlNode; AFlags: TFileViewFlags = []); override;
    destructor Destroy; override;

    procedure AddFileSource(aFileSource: IFileSource; aPath: String); override;

    procedure LoadConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); override;
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); override;
  end;

implementation

uses
  LCLIntf, LCLType, Forms,
  LCLProc, Clipbrd, uLng, uShowMsg, uGlobs, uPixmapManager, uDebug,
  uDCUtils, uOSUtils, math, fMain, fMaskInputDlg, uSearchTemplate,
  dmCommonData,
  uFileSourceProperty,
  uFileSourceOperationTypes,
  fColumnsSetConf,
  uKeyboard,
  uFileSourceUtil,
  uFileFunctions;

{ TBriefDrawGrid }

procedure TBriefDrawGrid.CalculateColRowCount(Data: PtrInt);
var
  glw, bw: Integer;
begin
  if (csDesigning in ComponentState) then Exit;

  if not Assigned(BriefView.FFiles) then Exit;

  glw := Max(GridLineWidth, 1);
  bw  := Max(BorderWidth, 1);

  if DefaultRowHeight > 0 then
  begin
    RowCount := (Height - GetSystemMetrics(SM_CYHSCROLL) -
                 glw - (2 * bw)) div (DefaultRowHeight + glw);
    if RowCount > 0 then
    ColCount := (BriefView.FFiles.Count + RowCount - 1) div RowCount;
  end;
  Invalidate;
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
end;

procedure TBriefDrawGrid.Resize;
begin
  inherited Resize;
  Application.QueueAsyncCall(@CalculateColRowCount, 0);
end;

procedure TBriefDrawGrid.RowHeightsChanged;
begin
  inherited RowHeightsChanged;
  Application.QueueAsyncCall(@CalculateColRowCount, 0);
end;

procedure TBriefDrawGrid.ColWidthsChanged;
begin
  inherited ColWidthsChanged;
  Application.QueueAsyncCall(@CalculateColRowCount, 0);
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
  Result:= inherited DoMouseWheelDown(Shift, MousePos);
  Result:= Perform(LM_HSCROLL, SB_PAGERIGHT, 0) = 0;
end;

function TBriefDrawGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result:= inherited DoMouseWheelUp(Shift, MousePos);
  Result:= Perform(LM_HSCROLL, SB_PAGELEFT, 0) = 0;
end;

procedure TBriefDrawGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
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

procedure TBriefDrawGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  BackgroundClick: Boolean;
  Point: TPoint;
begin
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

procedure TBriefDrawGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RIGHT:
      begin
        if (CellToIndex(Col + 1, Row) < 0) then Key:= 0;
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
end;

constructor TBriefDrawGrid.Create(AOwner: TComponent; AParent: TWinControl);
begin
  BriefView := AParent as TBriefFileView;

  inherited Create(AOwner);

  // Workaround for Lazarus issue 18832.
  // Set Fixed... before setting ...Count.
  FixedRows := 0;
  FixedCols := 0;

  // Override default values to start with no columns and no rows.
  RowCount := 0;
  ColCount := 0;

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

      s := AFile.FSFile.Name;

      while Canvas.TextWidth(s) - (aRect.Right - aRect.Left) - 4 > 0 do
        Delete(s, Length(s), 1);

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
  //---------------------
  begin
    Canvas.Font.Name   := gFonts[dcfMain].Name;
    Canvas.Font.Size   := gFonts[dcfMain].Size;
    Canvas.Font.Style  := gFonts[dcfMain].Style;

    // Set up default background color first.
    if (gdSelected in aState) and BriefView.Active and (not gUseFrameCursor) then
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
          if (gdSelected in aState) and BriefView.Active and (not gUseFrameCursor) then
            begin
              Canvas.Font.Color := InvertColor(gCursorText);
            end
          else
            begin
              BackgroundColor := gMarkColor;
              Canvas.Font.Color := TextColor;
            end;
          //------------------------------------------------------
        end
      else
        begin
          Canvas.Font.Color := gMarkColor;
        end;
    end
    else if (gdSelected in aState) and BriefView.Active and (not gUseFrameCursor) then
      begin
        Canvas.Font.Color := gCursorText;
      end
    else
      begin
        Canvas.Font.Color := TextColor;
      end;

    // Draw background.
    Canvas.Brush.Color := BriefView.DimColor(BackgroundColor);
    Canvas.FillRect(aRect);
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
        Result.Last:=  (LeftCol + VisibleColCount) * VisibleRowCount - 1;
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
  AFile.DisplayStrings.Add(FormatFileFunction('GETFILENAME', AFile.FSFile, FileSource));
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

procedure TBriefFileView.CreateDefault(AOwner: TWinControl);
begin
  inherited CreateDefault(AOwner);
  dgPanel:= TBriefDrawGrid.Create(Self, Self);
  MainControl := dgPanel;

  TabHeader:= TFileViewFixedHeader.Create(Self, Self);
  TabHeader.Top:= pnlHeader.Height;

  dgPanel.OnTopLeftChanged:= @dgPanelTopLeftChanged;
end;

procedure TBriefFileView.BeforeMakeFileList;
begin
  inherited BeforeMakeFileList;
end;

procedure TBriefFileView.AfterMakeFileList;
begin
  inherited AfterMakeFileList;
  dgPanel.CalculateColRowCount(0);
  SetFilesDisplayItems;
  Notify([fvnVisibleFilePropertiesChanged]);
end;

procedure TBriefFileView.ClearAfterDragDrop;
begin
  inherited ClearAfterDragDrop;

  // reset TCustomGrid state
  dgPanel.FGridState := gsNormal;
end;

procedure TBriefFileView.AfterChangePath;
begin
//  FUpdatingActiveFile := True;
  dgPanel.Col := 0;
  dgPanel.Row := 0;
//  FUpdatingActiveFile := False;

  inherited AfterChangePath;
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

procedure TBriefFileView.Resize;
var
  I: Integer;
  AWidth: Integer;
begin
  inherited Resize;

  if Assigned(TabHeader) then
  begin
    AWidth:= Width div TabHeader.Sections.Count;
    for I:= 0 to TabHeader.Sections.Count - 1 do
      TabHeader.Sections[I].Width:= AWidth;
  end;

  Notify([fvnVisibleFilePropertiesChanged]);
end;

constructor TBriefFileView.Create(AOwner: TWinControl; AConfig: TXmlConfig;
  ANode: TXmlNode; AFlags: TFileViewFlags = []);
begin
  inherited Create(AOwner, AConfig, ANode, AFlags);
end;

destructor TBriefFileView.Destroy;
begin
  inherited Destroy;
end;

procedure TBriefFileView.AddFileSource(aFileSource: IFileSource; aPath: String);
begin
  inherited AddFileSource(aFileSource, aPath);
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

procedure TBriefFileView.DoUpdateView;
begin
  inherited DoUpdateView;
  TabHeader.UpdateHeader;
  dgPanel.UpdateView;
end;

procedure TBriefFileView.SetSorting(const NewSortings: TFileSortings);
begin
  inherited SetSorting(NewSortings);
  TabHeader.UpdateSorting(NewSortings);
  SortAllDisplayFiles;
  ReDisplayFileList;
end;

procedure TBriefFileView.DoFileUpdated(AFile: TDisplayFile; UpdatedProperties: TFilePropertiesTypes);
begin
  MakeColumnsStrings(AFile);
  inherited DoFileUpdated(AFile, UpdatedProperties);
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

