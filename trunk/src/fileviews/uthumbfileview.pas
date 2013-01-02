unit uThumbFileView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Grids, DCXmlConfig, uFileSource, uOrderedFileView,
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
    FThumbView: TThumbFileView;
    FUpdateColCount: Integer;
  protected
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
  protected
    procedure UpdateView; override;
    procedure CalculateColRowCount; override;
    procedure CalculateColumnWidth; override;
    function  CellToIndex(ACol, ARow: Integer): Integer; override;
    procedure IndexToCell(Index: Integer; out ACol, ARow: Integer); override;
  public
    constructor Create(AOwner: TComponent; AParent: TWinControl); override;
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
    procedure ShowRenameFileEdit(aFile: TFile); override;
  public
    constructor Create(AOwner: TWinControl; AConfig: TXmlConfig; ANode: TXmlNode; AFlags: TFileViewFlags = []); override;
    constructor Create(AOwner: TWinControl; AFileView: TFileView; AFlags: TFileViewFlags = []); override;
    destructor Destroy; override;
    function Clone(NewParent: TWinControl): TThumbFileView; override;
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); override;
  end;

implementation

uses
  LCLIntf, LCLType, Graphics, Math, uFileSourceProperty, uGlobs,
  uPixMapManager;

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
    (SavedKey in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN, VK_HOME, VK_END, VK_PRIOR, VK_NEXT]);

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

  if ssShift in Shift then
  begin
    FileIndex := CellToIndex(Col, Row);
    if FileIndex <> InvalidFileIndex then
      FThumbView.Selection(SavedKey, FileIndex);
  end;
end;


procedure TThumbDrawGrid.UpdateView;

  function CalculateDefaultRowHeight: Integer;
  var
    OldFont, NewFont: TFont;
  begin
    // Assign temporary font.
    OldFont     := Canvas.Font;
    NewFont     := TFont.Create;
    Canvas.Font := NewFont;

    // Search columns settings for the biggest font (in height).
    Canvas.Font.Name  := gFonts[dcfMain].Name;
    Canvas.Font.Style := gFonts[dcfMain].Style;
    Canvas.Font.Size  := gFonts[dcfMain].Size;

    Result := gThumbHeight + Canvas.GetTextHeight('Wg') + 6;

    // Restore old font.
    Canvas.Font := OldFont;
    FreeAndNil(NewFont);
  end;

begin
  Flat := gInterfaceFlat;

  // Calculate row height.
  DefaultRowHeight := CalculateDefaultRowHeight;

  // Calculate column width
  CalculateColumnWidth;
end;

procedure TThumbDrawGrid.CalculateColRowCount;
var
  glw, bw: Integer;
  AIndex, ACol, ARow: Integer;
begin
  if (csDesigning in ComponentState) or (FUpdateColCount > 0) then Exit;

  if not Assigned(FFileView.DisplayFiles) then Exit;

  glw := Max(GridLineWidth, 1);
  bw  := Max(BorderWidth, 1);

  BeginUpdate;
  Inc(FUpdateColCount);
  try
    if DefaultColWidth > 0 then
    begin
      // Save active file index
      AIndex:= CellToIndex(Col, Row);

      ColCount := (Width - GetSystemMetrics(SM_CXVSCROLL) -
                   glw - (2 * bw)) div (DefaultColWidth + glw);
      if ColCount > 0 then
      begin
        RowCount := (FFileView.DisplayFiles.Count + ColCount - 1) div ColCount;
        if ColCount > 0 then
        begin
          ARow := (Width - GetSystemMetrics(SM_CXVSCROLL) -
                   glw - (2 * bw)) div ColCount - glw;
          // Update columns widths
          for ACol := 0 to ColCount - 1 do
            ColWidths[ACol]:= ARow;
        end;
      end;

      // Restore active file index
      IndexToCell(AIndex, ACol, ARow);
      MoveExtend(False, ACol, ARow);
    end;
  finally
    EndUpdate(True);
    Dec(FUpdateColCount);
  end;
end;

procedure TThumbDrawGrid.CalculateColumnWidth;
begin
  DefaultColWidth:= gThumbWidth + 4;
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
  if (Index < 0) or (Index >= FFileView.DisplayFiles.Count) then
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
  FThumbView:= AParent as TThumbFileView;
  inherited Create(AOwner, AParent);
end;

procedure TThumbDrawGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
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
  var
    X, Y: Integer;
    IconID: PtrInt;
    Bitmap: TBitmap;
  begin
    IconID := AFile.Tag;

    if (AFile.FSFile.IsDirectory = False) and (IconID >= 0) and
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

    s := AFile.DisplayStrings[0];
    Y:= (ColWidths[ACol] - 4 - Canvas.TextWidth('W'));
    s:= FitFileName(s, Canvas, AFile.FSFile, Y);

    Canvas.TextOut(aRect.Left + 2, iTextTop, s);
    Canvas.Pen.Color:= InvertColor(gBackColor);
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

      iTextTop := aRect.Top + (RowHeights[aRow] - Canvas.TextHeight('Wg'));

      DrawIconCell;
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

  FBitmapList:= TBitmapList.Create(True);
  FThumbnailManager:= TThumbnailManager.Create(gThumbWidth, gThumbHeight, clWhite);

  Notify([fvnVisibleFilePropertiesChanged]);
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

        if (AFile.Tag < 0) and (AFile.FSFile.IsDirectory = False) then
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
          if (AFile.Tag < 0) and (AFile.FSFile.IsDirectory = False) then
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

procedure TThumbFileView.ShowRenameFileEdit(aFile: TFile);
var
  ARect: TRect;
  ALeft, ATop, AWidth, AHeight: Integer;
begin
  if not edtRename.Visible then
  begin
    edtRename.Font.Name  := gFonts[dcfMain].Name;
    edtRename.Font.Size  := gFonts[dcfMain].Size;;
    edtRename.Font.Style := gFonts[dcfMain].Style;

    with dgPanel do
    begin
      ARect := CellRect(Col, Row);
      ATop := ARect.Bottom - Canvas.TextHeight('Wg') - 4;
      ALeft := ARect.Left;
      AWidth := ARect.Right - ALeft;
      AHeight := ARect.Bottom - ATop;
    end;

    edtRename.SetBounds(ALeft, ATop, AWidth, AHeight);
  end;

  inherited ShowRenameFileEdit(AFile);
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

procedure TThumbFileView.SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode);
begin
  inherited SaveConfiguration(AConfig, ANode);

  AConfig.SetAttr(ANode, 'Type', 'thumbnails');
end;

end.

