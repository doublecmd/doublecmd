unit uBriefFileView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LMessages, Grids, Graphics,
  uDisplayFile, DCXmlConfig, uTypes, uFileViewWithGrid, uFile,
  uFileSource;

type

  TBriefFileView = class;

  { TBriefDrawGrid }

  TBriefDrawGrid = class(TFileViewGrid)
  protected
    FBriefView: TBriefFileView;
  protected
    procedure UpdateView; override;
    procedure CalculateColRowCount; override;
    procedure CalculateColumnWidth; override;
    procedure DoMouseMoveScroll(X, Y: Integer);
  protected
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean); override;
  public
    constructor Create(AOwner: TComponent; AParent: TWinControl); override;
    function  CellToIndex(ACol, ARow: Integer): Integer; override;
    procedure IndexToCell(Index: Integer; out ACol, ARow: Integer); override;
    procedure DrawCell(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState); override;
  end;

  { TBriefFileView }

  TBriefFileView = class (TFileViewWithGrid)
  protected
    procedure CreateDefault(AOwner: TWinControl); override;
    function GetFileViewGridClass: TFileViewGridClass; override;
    procedure ShowRenameFileEdit(var aFile: TFile); override;
    procedure UpdateRenameFileEditPosition; override;
    function GetVisibleFilesIndexes: TRange; override;
    function GetIconRect(FileIndex: PtrInt): TRect; override;
    procedure MouseScrollTimer(Sender: TObject); override;
  public
    function Clone(NewParent: TWinControl): TBriefFileView; override;
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode; ASaveHistory:boolean); override;
  end;

implementation

uses
  LCLIntf, LCLType, LCLVersion, LCLProc, Math, StdCtrls,
  uGlobs, uPixmapManager, uKeyboard, fMain,
  uFileSourceProperty,
  uOrderedFileView;

const
  CELL_PADDING = 1;

{ TBriefDrawGrid }

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

    Canvas.Font.PixelsPerInch := NewFont.PixelsPerInch;

    // Search columns settings for the biggest font (in height).
    Canvas.Font.Name  := gFonts[dcfMain].Name;
    Canvas.Font.Style := gFonts[dcfMain].Style;
    Canvas.Font.Size  := gFonts[dcfMain].Size;

    CurrentHeight := Canvas.GetTextHeight('Wg');
    MaxFontHeight := Max(MaxFontHeight, CurrentHeight);

    // Restore old font.
    Canvas.Font := OldFont;
    FreeAndNil(NewFont);

    Result := MaxFontHeight + gExtraLineSpan;
  end;

var
  TempRowHeight: Integer;
begin
  // Fix border blinking while scroll window
  Flat := True; // gInterfaceFlat;

  // Calculate row height.
  TempRowHeight := CalculateDefaultRowHeight;
  if TempRowHeight > 0 then
    DefaultRowHeight := TempRowHeight;

  // Calculate column width
  CalculateColumnWidth;
end;

procedure TBriefDrawGrid.CalculateColRowCount;
var
  ARowCount: Integer;
  AIndex, ACol, ARow: Integer;
begin
  if (csDesigning in ComponentState) then Exit;

  if not Assigned(FBriefView.FFiles) then Exit;

  if (ClientHeight > 0) and (DefaultRowHeight > 0) then
  begin
    // Save active file index
    AIndex:= CellToIndex(Col, Row);

    ARowCount := (ClientHeight - BorderWidth * 2) div DefaultRowHeight;
    if ARowCount > 0 then
    begin
      RowCount := ARowCount;
      ColCount := (FBriefView.FFiles.Count + ARowCount - 1) div ARowCount;
      // Restore active file index
      if AIndex >= 0 then
      begin
        IndexToCell(AIndex, ACol, ARow);
        MoveExtend(False, ACol, ARow);
      end;
    end;
  end;
  Invalidate;
end;

procedure TBriefDrawGrid.CalculateColumnWidth;
var
  I, J, L, M: Integer;
begin
  if not Assigned(FBriefView.FFiles) or (FBriefView.FFiles.Count = 0) then Exit;
  if gBriefViewMode = bvmFixedWidth then
    DefaultColWidth:= Min(ClientWidth, gBriefViewFixedWidth)
  else if gBriefViewMode = bvmFixedCount then
    DefaultColWidth:= ClientWidth div Max(1, gBriefViewFixedCount)
  else if (FBriefView.FFiles.Count = 1) and (FBriefView.FFiles[0].FSFile.Name = '..') then
    DefaultColWidth:= ClientWidth div 3
  else
    begin
      J:= 0;
      M:= 0;
      for I:= 0 to FBriefView.FFiles.Count - 1 do
      begin
        L:= Length(FBriefView.FFiles[I].FSFile.Name);
        if L > M then
        begin
          M:= L;
          J:= I;
        end;
      end;
      Canvas.Font.Name          := gFonts[dcfMain].Name;
      Canvas.Font.Size          := gFonts[dcfMain].Size;
      Canvas.Font.Style         := gFonts[dcfMain].Style;
      M:= Canvas.TextWidth(FBriefView.FFiles[J].FSFile.Name + 'WWW');
      if (gShowIcons = sim_none) then
        M:= M + 2
      else
        M:= M + gIconsSize + 4;
      if M > ClientWidth then M:= ClientWidth - 4;
      DefaultColWidth:= M;
    end;
end;

procedure TBriefDrawGrid.DoMouseMoveScroll(X, Y: Integer);
var
  TickCount: QWord;
  AEvent: SmallInt;
begin
  TickCount := GetTickCount64;

  if X < 25 then
    AEvent := SB_LINEUP
  else if X > ClientWidth - 25 then
    AEvent := SB_LINEDOWN
  else begin
    FBriefView.tmMouseScroll.Enabled := False;
    Exit;
  end;

  if (FLastMouseMoveTime = 0) then
    FLastMouseMoveTime := TickCount
  else if (FLastMouseScrollTime = 0) then
    FLastMouseScrollTime := TickCount
  else if (TickCount - FLastMouseMoveTime > 200) and (TickCount - FLastMouseScrollTime > 50) then
  begin
    Scroll(LM_HSCROLL, AEvent);
    FLastMouseScrollTime := GetTickCount64;
    FBriefView.tmMouseScroll.Enabled := True;
  end;
end;

function TBriefDrawGrid.CellToIndex(ACol, ARow: Integer): Integer;
begin
  if (ARow < 0) or (ARow >= RowCount) or (ACol <  0) or (ACol >= ColCount) then Exit(-1);
  Result:= ACol * RowCount + ARow;
  if (Result < 0) or (Result >= FBriefView.FFiles.Count) then
    Result:= -1;
end;

procedure TBriefDrawGrid.IndexToCell(Index: Integer; out ACol, ARow: Integer);
begin
  if (Index < 0) or (Index >= FBriefView.FFiles.Count) or (RowCount = 0) then
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

procedure TBriefDrawGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  SavedKey: Word;
  FileIndex: Integer;
  ACol, ARow: Integer;
begin
  if FBriefView.IsLoadingFileList then
  begin
    FBriefView.HandleKeyDownWhenLoading(Key, Shift);
    Exit;
  end;

  SavedKey := Key;
  // Set RangeSelecting before cursor is moved.
  FBriefView.FRangeSelecting :=
    (ssShift in Shift) and
    (SavedKey in [VK_LEFT, VK_RIGHT, VK_HOME, VK_END, VK_PRIOR, VK_NEXT]);
  // Special case for selection with shift key (works like VK_INSERT)
  if (SavedKey in [VK_UP, VK_DOWN]) and (ssShift in Shift) then
    FBriefView.InvertActiveFile;

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
          IndexToCell(FBriefView.FFiles.Count - 1, ACol, ARow);
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
        if FileIndex >= FBriefView.FFiles.Count then
          FileIndex:= FBriefView.FFiles.Count - 1;
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
        IndexToCell(FBriefView.FFiles.Count - 1, ACol, ARow);
        MoveExtend(False, ACol, ARow);
        Key:= 0;
      end;
    VK_UP, VK_DOWN:
      begin
        if (CellToIndex(Col, Row) >= FBriefView.FFiles.Count - 1) and
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

  if FBriefView.FRangeSelecting then
  begin
    FileIndex := CellToIndex(Col, Row);
    if FileIndex <> InvalidFileIndex then
      FBriefView.Selection(SavedKey, FileIndex);
  end;
end;

procedure TBriefDrawGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FBriefView.IsMouseSelecting then DoMouseMoveScroll(X, Y);
end;

function TBriefDrawGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin

  if not FBriefView.IsLoadingFileList then
  begin

    if (Shift=[ssCtrl])and(gFonts[dcfMain].Size > gFonts[dcfMain].MinValue) then
    begin
      gFonts[dcfMain].Size:=gFonts[dcfMain].Size-1;
      frmMain.FrameLeft.UpdateView;
      frmMain.FrameRight.UpdateView;
      Result:=True;
      Exit;
    end;

    Result:= inherited DoMouseWheelDown(Shift, MousePos);
    Result:= Perform(LM_HSCROLL, SB_LINERIGHT, 0) = 0;
  end
  else
    Result := True; // Handled
end;

function TBriefDrawGrid.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin

  if not FBriefView.IsLoadingFileList then
  begin

    if (Shift=[ssCtrl])and(gFonts[dcfMain].Size > gFonts[dcfMain].MinValue) then
    begin
      gFonts[dcfMain].Size:=gFonts[dcfMain].Size+1;
      frmMain.FrameLeft.UpdateView;
      frmMain.FrameRight.UpdateView;
      Result:=True;
      Exit;
    end;


    Result:= inherited DoMouseWheelUp(Shift, MousePos);
    Result:= Perform(LM_HSCROLL, SB_LINELEFT, 0) = 0;
  end
  else
    Result := True; // Handled
end;

procedure TBriefDrawGrid.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);
  DoMouseMoveScroll(X, Y);
end;

constructor TBriefDrawGrid.Create(AOwner: TComponent; AParent: TWinControl);
begin
  FBriefView:= AParent as TBriefFileView;
  inherited Create(AOwner, AParent);
  // Fix vertical bar flash
  ScrollBars := ssAutoHorizontal;
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

        if gShowHiddenDimmed and AFile.FSFile.IsHidden then
          PixMapManager.DrawBitmapAlpha(IconID,
                                        Canvas,
                                        aRect.Left + CELL_PADDING,
                                        Y
                                       )
        else
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
                                          aRect.Left + 1,
                                          Y
                                          );
        end;

      end;
      // Print filename with align
      Y:= (DefaultColWidth - 2 - Canvas.TextWidth('I'));
      if (gShowIcons <> sim_none) then Y:= Y - gIconsSize - 2;
      if (not gBriefViewFileExtAligned) or (AFile.FSFile.Extension = '') then
        begin
          s:= AFile.DisplayStrings[0];
          s:= FitFileName(s, Canvas, AFile.FSFile, Y);
        end
      else
        begin
          // Right align extention print
          s:= AFile.FSFile.Extension;
          Canvas.TextOut(aRect.Left + DefaultColWidth - Canvas.TextWidth(s + 'I'), iTextTop, s);
          s:= AFile.FSFile.NameNoExt;
          s:= FitFileName(s, Canvas, AFile.FSFile, Y - Canvas.TextWidth(AFile.FSFile.Extension + 'I'));
        end;
      if (gShowIcons <> sim_none) then
        Canvas.TextOut(aRect.Left + gIconsSize + 4, iTextTop, s)
      else
        Canvas.TextOut(aRect.Left + 2, iTextTop, s);
    end; //of DrawIconCell

  //------------------------------------------------------
  //end of subprocedures
  //------------------------------------------------------

begin
  Idx:= CellToIndex(aCol, aRow);
  if (Idx >= 0) and (FBriefView.FFiles.Count > 0) then
    begin
      AFile:= FBriefView.FFiles[Idx];
      FileSourceDirectAccess:= fspDirectAccess in FBriefView.FileSource.Properties;
      if AFile.DisplayStrings.Count = 0 then
        FBriefView.MakeColumnsStrings(AFile);

      PrepareColors(aFile, aCol, aRow, aRect, aState);

      iTextTop := aRect.Top + (RowHeights[aRow] - Canvas.TextHeight('Wg')) div 2;

      DrawIconCell;
    end
  else
    begin
      // Draw background.
      Canvas.Brush.Color := FBriefView.DimColor(gBackColor);
      Canvas.FillRect(aRect);
    end;

  DrawCellGrid(aCol, aRow, aRect, aState);
  DrawLines(Idx, aCol, aRow, aRect, aState);
end;

{ TBriefFileView }

procedure TBriefFileView.CreateDefault(AOwner: TWinControl);
begin
  inherited CreateDefault(AOwner);
  tmMouseScroll.Interval := 350;

  // Changing height of a FileView with horizontal scrolling when hiding quick search causes file jumps under mouse
  quickSearch.LimitedAutoHide := True;
end;

function TBriefFileView.GetFileViewGridClass: TFileViewGridClass;
begin
  Result:= TBriefDrawGrid;
end;

procedure TBriefFileView.ShowRenameFileEdit(var aFile: TFile);
begin
  if not edtRename.Visible then
  begin
    edtRename.Font.Name  := gFonts[dcfMain].Name;
    edtRename.Font.Size  := gFonts[dcfMain].Size;
    edtRename.Font.Style := gFonts[dcfMain].Style;

    dgPanel.LeftCol:= dgPanel.Col;

    UpdateRenameFileEditPosition;
  end;

  inherited ShowRenameFileEdit(AFile);
end;

procedure TBriefFileView.UpdateRenameFileEditPosition;
var
  ARect: TRect;
begin
  inherited UpdateRenameFileEditPosition;

  ARect := dgPanel.CellRect(dgPanel.Col, dgPanel.Row);
  Dec(ARect.Top, 2);
  Inc(ARect.Bottom, 2);

  if gShowIcons <> sim_none then
    Inc(ARect.Left, gIconsSize + 2);

  if gInplaceRenameButton and (ARect.Right + edtRename.ButtonWidth < dgPanel.ClientWidth) then
    Inc(ARect.Right, edtRename.ButtonWidth);

  edtRename.SetBounds(ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
end;

function TBriefFileView.GetVisibleFilesIndexes: TRange;
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

function TBriefFileView.GetIconRect(FileIndex: PtrInt): TRect;
var
  ACol, ARow: Integer;
begin
  dgPanel.IndexToCell(FileIndex, ACol, ARow);
  Result := dgPanel.CellRect(ACol, ARow);

  Result.Top:= Result.Top + (dgPanel.RowHeights[ARow] - gIconsSize) div 2;
  Result.Left:= Result.Left + CELL_PADDING;
  Result.Right:= Result.Left + gIconsSize;
  Result.Bottom:= Result.Bottom + gIconsSize;
end;

procedure TBriefFileView.MouseScrollTimer(Sender: TObject);
var
  APoint: TPoint;
begin
  if DragManager.IsDragging or IsMouseSelecting then
  begin
    APoint := dgPanel.ScreenToClient(Mouse.CursorPos);
    TBriefDrawGrid(dgPanel).DoMouseMoveScroll(APoint.X, APoint.Y);
  end;
end;

function TBriefFileView.Clone(NewParent: TWinControl): TBriefFileView;
begin
  Result := TBriefFileView.Create(NewParent, Self);
end;

procedure TBriefFileView.SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode; ASaveHistory:boolean);
begin
  inherited SaveConfiguration(AConfig, ANode, ASaveHistory);

  AConfig.SetAttr(ANode, 'Type', 'brief');
end;

end.

