unit uThumbFileView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Grids, DCXmlConfig, uFileSource, uBriefFileView,
  uDisplayFile, uFileViewWorker, uThumbnails, uFileView, uTypes, uFileViewWithGrid;

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
  public
    destructor Destroy; override;
    function Clone(NewParent: TWinControl): TThumbFileView; override;
    procedure SaveConfiguration(AConfig: TXmlConfig; ANode: TXmlNode); override;
  end;

implementation

uses
  LCLIntf, LCLType, Graphics, Math, uFileSourceProperty, uGlobs,
  uPixMapManager, uDCUtils;

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
  DirectAccess: Boolean;
begin
  DirectAccess := fspDirectAccess in FFileSource.Properties;

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

    Result := gThumbHeight + Canvas.GetTextHeight('Wg') + 4;

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
  if (csDesigning in ComponentState) then Exit;

  if not Assigned(FFileView.DisplayFiles) then Exit;

  glw := Max(GridLineWidth, 1);
  bw  := Max(BorderWidth, 1);

  if DefaultRowHeight > 0 then
  begin
    // Save active file index
    AIndex:= CellToIndex(Col, Row);

    ColCount := (Width - GetSystemMetrics(SM_CXVSCROLL) -
                 glw - (2 * bw)) div (DefaultColWidth + glw);
    if ColCount > 0 then
    RowCount := (FFileView.DisplayFiles.Count + ColCount - 1) div ColCount;

    // Restore active file index
    IndexToCell(AIndex, ACol, ARow);
    MoveExtend(False, ACol, ARow);
  end;
  Invalidate;
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
  Bitmap: TBitmap;
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
    bmp: TBitmap;
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
                                      aRect.Left + 1,
                                      iTextTop - gIconsSize
                                      );
    end;

    s := AFile.DisplayStrings[0];
    Y:= (DefaultColWidth - 4 - Canvas.TextWidth('W'));
    s:= FitFileName(s, Canvas, AFile.FSFile, Y);

    Canvas.TextOut(aRect.Left + 2, iTextTop, s);
    Canvas.Pen.Color:= InvertColor(gBackColor);
    Canvas.Brush.Style:= bsClear;
    Canvas.Rectangle(aRect.Left + 2, aRect.Top + 2, aRect.Right - 2, aRect.Bottom - Canvas.TextHeight('Pp'));
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
  FThumbnailManager:= TThumbnailManager.Create(128, 128, clWhite);
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
  DirectAccess: Boolean;
begin
  if (csDestroying in ComponentState) or
     (GetCurrentWorkType = fvwtCreate) or
     IsEmpty then
    Exit;

  VisibleFiles := GetVisibleFilesIndexes;
  DirectAccess := fspDirectAccess in FileSource.Properties;

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

