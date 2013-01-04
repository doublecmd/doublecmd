unit uBriefFileView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LMessages, Grids, Graphics, StdCtrls,
  uDisplayFile, DCXmlConfig, uFileSorting, uFileProperty, uTypes,
  uFileViewWithGrid, uFile, uFileViewHeader, uFileView, uFileSource;

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
    function  CellToIndex(ACol, ARow: Integer): Integer; override;
    procedure IndexToCell(Index: Integer; out ACol, ARow: Integer); override;
  protected
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function  DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function  DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent; AParent: TWinControl); override;
    procedure DrawCell(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState); override;
  end;

  { TBriefFileView }

  TBriefFileView = class (TFileViewWithGrid)
  protected
    function GetFileViewGridClass: TFileViewGridClass; override;
    procedure ShowRenameFileEdit(aFile: TFile); override;
    function GetVisibleFilesIndexes: TRange; override;
  public
    function Clone(NewParent: TWinControl): TBriefFileView; override;
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); override;
  end;

implementation

uses
  LCLIntf, LCLType, LCLVersion, LCLProc, math,
  uGlobs, uPixmapManager, uKeyboard,
  uDCUtils, fMain,
  uFileSourceProperty,
  uFileFunctions,
  uOrderedFileView;

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

procedure TBriefDrawGrid.CalculateColRowCount;
var
  glw, bw: Integer;
  AIndex, ACol, ARow: Integer;
begin
  if (csDesigning in ComponentState) then Exit;

  if not Assigned(FBriefView.FFiles) then Exit;

  glw := Max(GridLineWidth, 1);
  bw  := Max(BorderWidth, 1);

  if DefaultRowHeight > 0 then
  begin
    // Save active file index
    AIndex:= CellToIndex(Col, Row);

    RowCount := (Height - GetSystemMetrics(SM_CYHSCROLL) -
                 glw - (2 * bw)) div (DefaultRowHeight + glw);
    if RowCount > 0 then
    ColCount := (FBriefView.FFiles.Count + RowCount - 1) div RowCount;

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
  if not Assigned(FBriefView.FFiles) then Exit;
  if FBriefView.FFiles.Count < 2 then
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
      Canvas.Font.Name   := gFonts[dcfMain].Name;
      Canvas.Font.Size   := gFonts[dcfMain].Size;
      Canvas.Font.Style  := gFonts[dcfMain].Style;
      M:= Canvas.TextWidth(FBriefView.FFiles[J].FSFile.Name + 'WWW');
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
  if (Result < 0) or (Result >= FBriefView.FFiles.Count) then
    Result:= -1;
end;

procedure TBriefDrawGrid.IndexToCell(Index: Integer; out ACol, ARow: Integer);
begin
  if (Index < 0) or (Index >= FBriefView.FFiles.Count) then
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
  if DragManager.IsDragging or FBriefView.IsMouseSelecting then
  begin
    if X < 25 then
      Scroll(SB_LINEUP)
    else if X > ClientWidth - 25 then
      Scroll(SB_LINEDOWN);
  end;
end;

function TBriefDrawGrid.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  if not FBriefView.IsLoadingFileList then
  begin
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
    Result:= inherited DoMouseWheelUp(Shift, MousePos);
    Result:= Perform(LM_HSCROLL, SB_LINELEFT, 0) = 0;
  end
  else
    Result := True; // Handled
end;

constructor TBriefDrawGrid.Create(AOwner: TComponent; AParent: TWinControl);
begin
  FBriefView:= AParent as TBriefFileView;
  inherited Create(AOwner, AParent);
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

function TBriefFileView.GetFileViewGridClass: TFileViewGridClass;
begin
  Result:= TBriefDrawGrid;
end;

procedure TBriefFileView.ShowRenameFileEdit(aFile: TFile);
var
  ALeft, ATop, AWidth, AHeight: Integer;
begin
  if not edtRename.Visible then
  begin
    edtRename.Font.Name  := gFonts[dcfMain].Name;
    edtRename.Font.Size  := gFonts[dcfMain].Size;;
    edtRename.Font.Style := gFonts[dcfMain].Style;

    dgPanel.LeftCol:= dgPanel.Col;
    ATop := dgPanel.CellRect(dgPanel.Col, dgPanel.Row).Top - 2;
    ALeft := dgPanel.CellRect(dgPanel.Col, dgPanel.Row).Left;
    if gShowIcons <> sim_none then
      Inc(ALeft, gIconsSize + 2);
    AWidth := dgPanel.ColWidths[dgPanel.Col] - ALeft;
    AHeight := dgPanel.RowHeights[dgPanel.Row] + 4;

    edtRename.SetBounds(ALeft, ATop, AWidth, AHeight);
  end;

  inherited ShowRenameFileEdit(AFile);
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

function TBriefFileView.Clone(NewParent: TWinControl): TBriefFileView;
begin
  Result := TBriefFileView.Create(NewParent, Self);
end;

procedure TBriefFileView.SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
begin
  inherited SaveConfiguration(AConfig, ANode);

  AConfig.SetAttr(ANode, 'Type', 'brief');
end;

end.

