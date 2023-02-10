unit uThumbFileView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Grids, Types, DCXmlConfig, uFileSource, uOrderedFileView,
  uDisplayFile, uFileViewWorker, uThumbnails, uFileView, uTypes, uFileViewWithGrid,
  uFile;

type

  { TFileThumbnailsRetriever }

  TFileThumbnailsRetriever = class(TFileViewWorker)
  private
    FWorkingFile: TDisplayFile;
    FWorkingUserData: Pointer;
    FFileList: TFVWorkerFileList;
    FThumbnailManager: TThumbnailManager;
    FUpdateFileMethod: TUpdateFileMethod;
    FFileSource: IFileSource;
    FBitmapList: TBitmapList;

    {en
       Updates file in the file view with new data from FWorkerData.
       It is called from GUI thread.
    }
    procedure DoUpdateFile;

  protected
    procedure Execute; override;

  public
    constructor Create(AFileSource: IFileSource;
                       AThread: TThread;
                       ABitmapList: TBitmapList;
                       AUpdateFileMethod: TUpdateFileMethod;
                       AThumbnailManager: TThumbnailManager;
                       var AFileList: TFVWorkerFileList); reintroduce;
    destructor Destroy; override;
  end;

  TThumbFileView = class;

  { TThumbDrawGrid }

  TThumbDrawGrid = class(TFileViewGrid)
  private
    FThumbSize: TSize;
    FMouseDownY: Integer;
    FThumbView: TThumbFileView;
    FUpdateColCount: Integer;
  protected
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean); override;
  protected
    procedure UpdateView; override;
    procedure CalculateColRowCount; override;
    procedure CalculateColumnWidth; override;
    procedure DoMouseMoveScroll(Sender: TObject; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent; AParent: TWinControl); override;
    function  CellToIndex(ACol, ARow: Integer): Integer; override;
    procedure IndexToCell(Index: Integer; out ACol, ARow: Integer); override;
    procedure DrawCell(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState); override;
  end;


  { TThumbFileView }

  TThumbFileView = class(TFileViewWithGrid)
  private
    FBitmapList: TBitmapList;
    FThumbnailManager: TThumbnailManager;
    procedure ThumbnailsRetrieverOnUpdate(const UpdatedFile: TDisplayFile; const UserData: Pointer);
  protected
    procedure CreateDefault(AOwner: TWinControl); override;
    procedure AfterChangePath; override;
    procedure EnsureDisplayProperties; override;
    function GetFileViewGridClass: TFileViewGridClass; override;
    function GetVisibleFilesIndexes: TRange; override;
    procedure ShowRenameFileEdit(var aFile: TFile); override;
    procedure UpdateRenameFileEditPosition(); override;
    function GetIconRect(FileIndex: PtrInt): TRect; override;
    procedure MouseScrollTimer(Sender: TObject); override;
  public
    constructor Create(AOwner: TWinControl; AConfig: TXmlConfig; ANode: TXmlNode; AFlags: TFileViewFlags = []); override;
    constructor Create(AOwner: TWinControl; AFileView: TFileView; AFlags: TFileViewFlags = []); override;
    destructor Destroy; override;
    function Clone(NewParent: TWinControl): TThumbFileView; override;
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode; ASaveHistory:boolean); override;
  end;

implementation

uses
  LCLIntf, LCLType, LMessages, Graphics, Math, StdCtrls, uFileSourceProperty,
  uGlobs, uPixMapManager;

{ TFileThumbnailsRetriever }

procedure TFileThumbnailsRetriever.DoUpdateFile;
begin
  if not Aborted and Assigned(FUpdateFileMethod) then
    FUpdateFileMethod(FWorkingFile, FWorkingUserData);
end;

procedure TFileThumbnailsRetriever.Execute;
var
  I: Integer;
  Bitmap: TBitmap;
begin
  for I := 0 to FFileList.Count - 1 do
  begin
    if Aborted then
      Exit;

    FWorkingFile := FFileList.Files[I];
    FWorkingUserData := FFileList.Data[I];

    try
        if FWorkingFile.Tag < 0 then
        begin
          Bitmap:= FThumbnailManager.CreatePreview(FWorkingFile.FSFile);
          if Assigned(Bitmap) then
          begin
            FWorkingFile.Tag := FBitmapList.Add(Bitmap);
          end;
        end;

      if Aborted then
        Exit;

      TThread.Synchronize(Thread, @DoUpdateFile);

    except
      on EFileNotFound do;
    end;
  end;
end;

constructor TFileThumbnailsRetriever.Create(AFileSource: IFileSource;
  AThread: TThread; ABitmapList: TBitmapList;
  AUpdateFileMethod: TUpdateFileMethod; AThumbnailManager: TThumbnailManager;
  var AFileList: TFVWorkerFileList);
begin
  inherited Create(AThread);

  FWorkType             := fvwtUpdate;
  FFileList             := AFileList;
  AFileList             := nil;
  FFileSource           := AFileSource;
  FBitmapList           := ABitmapList;
  FThumbnailManager     := AThumbnailManager;
  FUpdateFileMethod     := AUpdateFileMethod;
end;

destructor TFileThumbnailsRetriever.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

{ TThumbDrawGrid }

procedure TThumbDrawGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  SavedKey: Word;
  FileIndex: Integer;
  ACol, ARow: Integer;
begin
  if FThumbView.IsLoadingFileList then
  begin
    FThumbView.HandleKeyDownWhenLoading(Key, Shift);
    Exit;
  end;

  SavedKey := Key;
  // Set RangeSelecting before cursor is moved.
  FThumbView.FRangeSelecting :=
    (ssShift in Shift) and
    (SavedKey in [VK_UP, VK_DOWN, VK_HOME, VK_END, VK_PRIOR, VK_NEXT]);
  // Special case for selection with shift key (works like VK_INSERT)
  if (SavedKey in [VK_LEFT, VK_RIGHT]) and (ssShift in Shift) then
    FThumbView.InvertActiveFile;

  case Key of
    VK_LEFT:
      begin
        if (Col - 1 < 0) and (Row > 0) then
        begin
          MoveExtend(False, ColCount - 1, Row - 1);
          Key:= 0;
        end;
      end;
    VK_RIGHT:
      begin
        if (CellToIndex(Col + 1, Row) < 0) then
        begin
          if (Row + 1 < RowCount) then
            MoveExtend(False, 0, Row + 1)
          else
            begin
              IndexToCell(FThumbView.FFiles.Count - 1, ACol, ARow);
              MoveExtend(False, ACol, ARow);
            end;
          Key:= 0;
        end;
      end;
    VK_HOME:
      begin
        MoveExtend(False, 0, 0);
        Key:= 0;
      end;
    VK_END:
      begin
        IndexToCell(FThumbView.FFiles.Count - 1, ACol, ARow);
        MoveExtend(False, ACol, ARow);
        Key:= 0;
      end;
    VK_DOWN:
      begin
        if (CellToIndex(Col, Row + 1) < 0) then
          begin
            IndexToCell(FThumbView.FFiles.Count - 1, ACol, ARow);
            MoveExtend(False, ACol, ARow);
            Key:= 0;
          end
      end;
  end;

  inherited KeyDown(Key, Shift);

  if FThumbView.FRangeSelecting then
  begin
    FileIndex := CellToIndex(Col, Row);
    if FileIndex <> InvalidFileIndex then
      FThumbView.Selection(SavedKey, FileIndex);
  end;
end;

procedure TThumbDrawGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FThumbView.IsMouseSelecting then DoMouseMoveScroll(nil, X, Y);
end;

procedure TThumbDrawGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FMouseDownY := Y;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TThumbDrawGrid.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);
  DoMouseMoveScroll(nil, X, Y);
end;

procedure TThumbDrawGrid.UpdateView;
var
  I: Integer;

  function CalculateDefaultRowHeight: Integer;
  var
    OldFont, NewFont: TFont;
  begin
    // Assign temporary font.
    OldFont     := Canvas.Font;
    NewFont     := TFont.Create;
    Canvas.Font := NewFont;

    Canvas.Font.PixelsPerInch := NewFont.PixelsPerInch;

    // Search columns settings for the biggest font (in height).
    Canvas.Font.Name  := gFonts[dcfMain].Name;
    Canvas.Font.Style := gFonts[dcfMain].Style;
    Canvas.Font.Size  := gFonts[dcfMain].Size;

    if gUseFrameCursor then
      Result := gThumbSize.cy + Canvas.GetTextHeight('Wg') + gBorderFrameWidth*2 + 4
    else
      Result := gThumbSize.cy + Canvas.GetTextHeight('Wg') + 6;

    // Restore old font.
    Canvas.Font := OldFont;
    FreeAndNil(NewFont);
  end;

begin
  // Fix border blinking while scroll window
  Flat := True; // gInterfaceFlat;

  // Calculate row height.
  DefaultRowHeight := CalculateDefaultRowHeight;

  // Calculate column width
  CalculateColumnWidth;

  // Refresh thumbnails
  if (FThumbSize.cx <> gThumbSize.cx) or (FThumbSize.cy <> gThumbSize.cy) then
  begin
    FThumbSize:= gThumbSize;
    FThumbView.FBitmapList.Clear;
    if Assigned(FThumbView.FAllDisplayFiles) then
    begin
      // Clear thumbnail image index
      for I := 0 to FThumbView.FAllDisplayFiles.Count - 1 do
        FThumbView.FAllDisplayFiles[I].Tag:= -1;
    end;
    FThumbView.Notify([fvnVisibleFilePropertiesChanged]);
  end;
end;

procedure TThumbDrawGrid.CalculateColRowCount;
var
  AIndex, ACol, ARow: Integer;
  AColCount, ABorderWidth: Integer;
begin
  if (csDesigning in ComponentState) or (FUpdateColCount > 0) then Exit;

  if not Assigned(FFileView.DisplayFiles) then Exit;

  BeginUpdate;
  Inc(FUpdateColCount);
  try
    if (ClientWidth > 0) and (DefaultColWidth > 0) then
    begin
      // Save active file index
      AIndex:= CellToIndex(Col, Row);
      ABorderWidth:= BorderWidth * 2;

      AColCount := (ClientWidth - ABorderWidth) div DefaultColWidth;
      if AColCount > 0 then
      begin
        ColCount := AColCount;
        RowCount := (FFileView.DisplayFiles.Count + AColCount - 1) div AColCount;
        if ColCount > 0 then
        begin
          ARow := (ClientWidth - ABorderWidth) div ColCount;
          // Update columns widths
          for ACol := 0 to ColCount - 1 do
            ColWidths[ACol]:= ARow;
        end;
        // Restore active file index
        if AIndex >= 0 then
        begin
          IndexToCell(AIndex, ACol, ARow);
          MoveExtend(False, ACol, ARow);
        end;
      end;
    end;
  finally
    EndUpdate(True);
    Dec(FUpdateColCount);
  end;
end;

procedure TThumbDrawGrid.CalculateColumnWidth;
begin
  if gUseFrameCursor then
    DefaultColWidth := gThumbSize.cx + gBorderFrameWidth*2 + 2
  else
    DefaultColWidth := gThumbSize.cx + 4;
end;

procedure TThumbDrawGrid.DoMouseMoveScroll(Sender: TObject; X, Y: Integer);
const
  LastPos: Integer = 0;
var
  Delta: Integer;
  TickCount: QWord;
  AEvent: SmallInt = -1;
begin
  TickCount := GetTickCount64;

  Delta := DefaultRowHeight div 3;
  if Y < Delta then
    AEvent := SB_LINEUP
  else if (Y > ClientHeight - Delta) and (Y - FMouseDownY > 8) then
  begin
    AEvent := SB_LINEDOWN;
  end;

  // Scroll at each 8 pixel mouse move
  if (Abs(LastPos - Y) < 8) and (Sender <> FThumbView.tmMouseScroll) then
    Exit;

  if (AEvent = -1) then
  begin
    FThumbView.tmMouseScroll.Enabled := False;
    Exit;
  end;

  LastPos := Y;

  if (FLastMouseMoveTime = 0) then
    FLastMouseMoveTime := TickCount
  else if (FLastMouseScrollTime = 0) then
    FLastMouseScrollTime := TickCount
  else if (TickCount - FLastMouseMoveTime > 200) and (TickCount - FLastMouseScrollTime > 50) then
  begin
    Scroll(LM_VSCROLL, AEvent);
    FLastMouseScrollTime := GetTickCount64;
    FThumbView.tmMouseScroll.Enabled := True;
    if (AEvent = SB_LINEDOWN) then FMouseDownY := -1;
  end;
end;

function TThumbDrawGrid.CellToIndex(ACol, ARow: Integer): Integer;
begin
  if (ARow < 0) or (ARow >= RowCount) or (ACol <  0) or (ACol >= ColCount) then Exit(-1);
  Result:= ARow * ColCount + ACol;
  if (Result < 0) or (Result >= FFileView.DisplayFiles.Count) then
    Result:= -1;
end;

procedure TThumbDrawGrid.IndexToCell(Index: Integer; out ACol, ARow: Integer);
begin
  if (Index < 0) or (Index >= FFileView.DisplayFiles.Count) or (ColCount = 0) then
    begin
      ACol:= -1;
      ARow:= -1;
    end
  else
    begin
      ARow:= Index div ColCount;
      ACol:= Index mod ColCount;
    end;
end;

constructor TThumbDrawGrid.Create(AOwner: TComponent; AParent: TWinControl);
begin
  FThumbSize:= gThumbSize;
  FThumbView:= AParent as TThumbFileView;
  inherited Create(AOwner, AParent);
  // Fix horizontal bar flash
  ScrollBars := ssAutoVertical;
  Options := Options + [goDontScrollPartCell];
end;

procedure TThumbDrawGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  Idx: Integer;
  //shared variables
  AFile: TDisplayFile;
  FileSourceDirectAccess: Boolean;

  //------------------------------------------------------
  //begin subprocedures
  //------------------------------------------------------

  procedure DrawIconCell(aRect: TRect);
  var
    iTextTop: Integer;
    X, Y: Integer;
    s: string;
    IconID: PtrInt;
    Bitmap: TBitmap;
  begin
    iTextTop := aRect.Bottom - Canvas.TextHeight('Wg');

    IconID := AFile.Tag;

    if (AFile.FSFile.IsNameValid) and (IconID >= 0) and
       (IconID < FThumbView.FBitmapList.Count) then
      begin
        Bitmap:= FThumbView.FBitmapList[IconID];
        X:= aRect.Left + (aRect.Right - aRect.Left - Bitmap.Width) div 2;
        Y:= aRect.Top + (iTextTop - aRect.Top - Bitmap.Height) div 2;
        Canvas.Draw(X, Y, Bitmap);
      end
    else
      begin
        IconID := AFile.IconID;
        // Draw default icon if there is no icon for the file.
        if IconID = -1 then
          IconID := PixMapManager.GetDefaultIcon(AFile.FSFile);

        // Center icon
        X:= aRect.Left + (aRect.Right - aRect.Left - gIconsSize) div 2;
        Y:= aRect.Top + (iTextTop - aRect.Top - gIconsSize) div 2;

        // Draw icon for a file
        PixMapManager.DrawBitmap(IconID, Canvas, X, Y);
      end;

    // Draw overlay icon for a file if needed
    if gIconOverlays then
    begin
      PixMapManager.DrawBitmapOverlay(AFile,
                                      FileSourceDirectAccess,
                                      Canvas,
                                      aRect.Left + 2,
                                      iTextTop - gIconsSize - 2
                                      );
    end;

    s:= AFile.DisplayStrings[0];
    s:= FitFileName(s, Canvas, AFile.FSFile, aRect.Width - 4);

    Canvas.TextOut(aRect.Left + 2, iTextTop - 1, s);
    Canvas.Pen.Color:= InvertColor(ColorToRGB(gBackColor));
    Canvas.Pen.Width := 1;
    Canvas.Frame(aRect.Left + 1, aRect.Top + 1, aRect.Right - 1, aRect.Bottom - Canvas.TextHeight('Pp') - 1);
  end; //of DrawIconCell

  //------------------------------------------------------
  //end of subprocedures
  //------------------------------------------------------

begin
  Idx:= CellToIndex(aCol, aRow);
  if (Idx >= 0) and (FThumbView.FFiles.Count > 0) then
    begin
      AFile:= FThumbView.FFiles[Idx];
      FileSourceDirectAccess:= fspDirectAccess in FFileView.FileSource.Properties;
      if AFile.DisplayStrings.Count = 0 then
        FThumbView.MakeColumnsStrings(AFile);

      PrepareColors(AFile, aCol, aRow, aRect, aState);

      if gUseFrameCursor then
        DrawIconCell(Rect(aRect.Left + gBorderFrameWidth - 1, aRect.Top + gBorderFrameWidth - 1,
                          aRect.Right - gBorderFrameWidth + 1, aRect.Bottom - gBorderFrameWidth + 1))
      else
        DrawIconCell(aRect);
    end
  else
    begin
      // Draw background.
      Canvas.Brush.Color := FThumbView.DimColor(gBackColor);
      Canvas.FillRect(aRect);
    end;

  DrawCellGrid(aCol, aRow, aRect, aState);
  DrawLines(Idx, aCol, aRow, aRect, aState);
end;

{ TThumbFileView }

procedure TThumbFileView.ThumbnailsRetrieverOnUpdate(
  const UpdatedFile: TDisplayFile; const UserData: Pointer);
var
  OrigDisplayFile: TDisplayFile absolute UserData;
begin
  if not IsReferenceValid(OrigDisplayFile) then
    Exit; // File does not exist anymore (reference is invalid).

  if UpdatedFile.Tag <> -1 then
    OrigDisplayFile.Tag := UpdatedFile.Tag;

  DoFileUpdated(OrigDisplayFile);
end;

procedure TThumbFileView.CreateDefault(AOwner: TWinControl);
begin
  inherited CreateDefault(AOwner);

  tmMouseScroll.Interval := 200;
  FBitmapList:= TBitmapList.Create(True);
  FThumbnailManager:= TThumbnailManager.Create(gBackColor);
end;

procedure TThumbFileView.AfterChangePath;
begin
  FBitmapList.Clear;
  inherited AfterChangePath;
end;

procedure TThumbFileView.EnsureDisplayProperties;
var
  VisibleFiles: TRange;
  i: Integer;
  Bitmap: TBitmap;
  AFileList: TFVWorkerFileList = nil;
  Worker: TFileViewWorker;
  AFile: TDisplayFile;
begin
  if (csDestroying in ComponentState) or
     (GetCurrentWorkType = fvwtCreate) or
     IsEmpty then
    Exit;

  if fspDirectAccess in FileSource.Properties then
  begin
    VisibleFiles := GetVisibleFilesIndexes;

    if not gListFilesInThread then
    begin
      for i := VisibleFiles.First to VisibleFiles.Last do
      begin
        AFile := FFiles[i];

        if (AFile.Tag < 0) and AFile.FSFile.IsNameValid then
        begin
          Bitmap:= FThumbnailManager.CreatePreview(AFile.FSFile);
          if Assigned(Bitmap) then
          begin
            AFile.Tag := FBitmapList.Add(Bitmap);
          end;
        end;
      end;
    end
    else
    begin
      try
        for i := VisibleFiles.First to VisibleFiles.Last do
        begin
          AFile := FFiles[i];
          if (AFile.Tag < 0) and AFile.FSFile.IsNameValid then
          begin
            if not Assigned(AFileList) then
              AFileList := TFVWorkerFileList.Create;
            AFileList.AddClone(AFile, AFile);
          end;
        end;

        if Assigned(AFileList) and (AFileList.Count > 0) then
        begin
          Worker := TFileThumbnailsRetriever.Create(
            FileSource,
            WorkersThread,
            FBitmapList,
            @ThumbnailsRetrieverOnUpdate,
            FThumbnailManager,
            AFileList);

          AddWorker(Worker, True);
          WorkersThread.QueueFunction(@Worker.StartParam);
        end;

      finally
        AFileList.Free;
      end;
    end;
  end;
  inherited EnsureDisplayProperties;
end;

function TThumbFileView.GetFileViewGridClass: TFileViewGridClass;
begin
  Result:= TThumbDrawGrid;
end;

function TThumbFileView.GetVisibleFilesIndexes: TRange;
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
        Result.First:= (TopRow * VisibleColCount - 1);
        Result.Last:=  (TopRow + VisibleRowCount + 1) * VisibleColCount - 1;
        if Result.First < 0 then Result.First:= 0;
        if Result.Last >= FFiles.Count then Result.Last:= FFiles.Count - 1;
      end;
  end;
end;

procedure TThumbFileView.ShowRenameFileEdit(var aFile: TFile);
begin
  if not edtRename.Visible then
  begin
    edtRename.Font.Name  := gFonts[dcfMain].Name;
    edtRename.Font.Size  := gFonts[dcfMain].Size;
    edtRename.Font.Style := gFonts[dcfMain].Style;

    UpdateRenameFileEditPosition;
  end;

  inherited ShowRenameFileEdit(AFile);
end;

procedure TThumbFileView.UpdateRenameFileEditPosition();
var
  ARect: TRect;
begin
  inherited UpdateRenameFileEditPosition;

  ARect := dgPanel.CellRect(dgPanel.Col, dgPanel.Row);
  ARect.Top := ARect.Bottom - dgPanel.Canvas.TextHeight('Wg') - 4;

  if gInplaceRenameButton and (ARect.Right + edtRename.ButtonWidth < dgPanel.ClientWidth) then
    Inc(ARect.Right, edtRename.ButtonWidth);

  edtRename.SetBounds(ARect.Left, ARect.Top, ARect.Right - ARect.Left, ARect.Bottom - ARect.Top);
end;

function TThumbFileView.GetIconRect(FileIndex: PtrInt): TRect;
begin
  Result:= GetFileRect(FileIndex);
  Result.Right:= Result.Left + (Result.Right - Result.Left) div 4;
end;

procedure TThumbFileView.MouseScrollTimer(Sender: TObject);
var
  APoint: TPoint;
begin
  if DragManager.IsDragging or IsMouseSelecting then
  begin
    APoint := dgPanel.ScreenToClient(Mouse.CursorPos);
    TThumbDrawGrid(dgPanel).DoMouseMoveScroll(tmMouseScroll, APoint.X, APoint.Y);
  end;
end;

constructor TThumbFileView.Create(AOwner: TWinControl; AConfig: TXmlConfig;
  ANode: TXmlNode; AFlags: TFileViewFlags);
begin
  inherited Create(AOwner, AConfig, ANode, AFlags);
end;

constructor TThumbFileView.Create(AOwner: TWinControl; AFileView: TFileView;
  AFlags: TFileViewFlags);
var
  I: Integer;
begin
  inherited Create(AOwner, AFileView, AFlags);

  if Assigned(FAllDisplayFiles) then
  begin
    // Clear thumbnail image index
    for I := 0 to FAllDisplayFiles.Count - 1 do
      FAllDisplayFiles[I].Tag:= -1;
    // Load thumbnails
    Notify([fvnVisibleFilePropertiesChanged]);
  end;
end;

destructor TThumbFileView.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FBitmapList);
  FreeAndNil(FThumbnailManager);
end;

function TThumbFileView.Clone(NewParent: TWinControl): TThumbFileView;
begin
  Result := TThumbFileView.Create(NewParent, Self);
end;

procedure TThumbFileView.SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode; ASaveHistory:boolean);
begin
  inherited SaveConfiguration(AConfig, ANode, ASaveHistory);

  AConfig.SetAttr(ANode, 'Type', 'thumbnails');
end;

end.

