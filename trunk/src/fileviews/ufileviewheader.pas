unit uFileViewHeader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, ComCtrls, LCLVersion,
  uPathLabel, uFileView, KASPathEdit, uFileSorting;

type

  { TFileViewHeader }

  TFileViewHeader = class(TPanel)
  private
    FFileView: TFileView;
    FAddressLabel: TPathLabel;
    FPathLabel: TPathLabel;
    FPathEdit: TKASPathEdit;
    procedure HeaderResize(Sender: TObject);
    procedure PathEditExit(Sender: TObject);
    procedure PathEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PathLabelClick(Sender: TObject);
    procedure PathLabelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AddressLabelClick(Sender: TObject);
    procedure AddressLabelMouseEnter(Sender: TObject);
    procedure PathLabelDblClick(Sender: TObject);
    procedure tmViewHistoryMenuTimer(Sender: TObject);

    procedure PathLabelMouseWheelUp(Sender: TObject;Shift: TShiftState; MousePos: TPoint;var Handled:Boolean);
    procedure PathLabelMouseWheelDown(Sender: TObject;Shift: TShiftState; MousePos: TPoint;var Handled:Boolean);

    procedure EachViewUpdateHeader(AFileView: TFileView; {%H-}UserData: Pointer);

  protected
    tmViewHistoryMenu: TTimer;
    procedure PathLabelSetColor(APathLabel: TPathLabel);
  public
    constructor Create(AOwner: TFileView; AParent: TWinControl); reintroduce;

    procedure UpdateAddressLabel;
    procedure UpdatePathLabel;
    procedure UpdateFont;

    procedure ShowPathEdit;
    procedure SetActive(bActive: Boolean);
  end;

  { TFileViewFixedHeader }

  TFileViewFixedHeader = class(THeaderControl)
  private
    FFileView: TFileView;
    FDown: Boolean;
    FMouseInControl: Boolean;
    FSelectedSection: Integer;
    FSorting: TFileSortings;
    procedure UpdateState;
  protected
    procedure SectionClick(Section: THeaderSection); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown({%H-}Button: TMouseButton; {%H-}Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseMove({%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer); override;
    procedure MouseUp({%H-}Button: TMouseButton; {%H-}Shift: TShiftState;
                      {%H-}X, {%H-}Y: Integer); override;
    {$if lcl_fullversion >= 1070000}
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
                const AXProportion, AYProportion: Double); override;
    {$endif}
  public
    constructor Create(AOwner: TFileView; AParent: TWinControl); reintroduce;
    destructor Destroy; override;

    procedure Click; override;
    procedure DblClick; override;
    procedure UpdateHeader;
    procedure UpdateSorting(Sorting: TFileSortings);
  end;

implementation

uses
  LCLType, ShellCtrls, uDCUtils, DCOSUtils, DCStrUtils, uKeyboard,
  fMain, uFileSourceUtil, uGlobs, uPixMapManager, uLng, uFileFunctions,
  uArchiveFileSource, uFileViewWithPanels;

const
  SortingImageIndex: array[TSortDirection] of Integer = (-1, 0, 1);

{ TFileViewHeader }

procedure TFileViewHeader.PathEditExit(Sender: TObject);
begin
  FPathEdit.Visible := False;
end;

procedure TFileViewHeader.PathEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  NewPath: String;
begin
  case Key of
    VK_ESCAPE:
      begin
        Key := 0;
        FPathEdit.Visible:=False;
        FFileView.SetFocus;
      end;

    VK_RETURN,
    VK_SELECT:
      begin
        Key := 0; // catch the enter
        NewPath:= NormalizePathDelimiters(FPathEdit.Text);
        NewPath:= ReplaceEnvVars(ReplaceTilde(NewPath));
        if not mbFileExists(NewPath) then
          begin
            if not ChooseFileSource(FFileView, NewPath, True) then
              Exit;
          end
        else
          begin
            if not ChooseFileSource(FFileView, ExtractFileDir(NewPath)) then
              Exit;
            FFileView.SetActiveFile(ExtractFileName(NewPath));
          end;
        FPathEdit.Visible := False;
        FFileView.SetFocus;
      end;

{$IFDEF LCLGTK2}
    // Workaround for GTK2 - up and down arrows moving through controls.
    VK_UP,
    VK_DOWN:
      Key := 0;
{$ENDIF}
  end;
end;

procedure TFileViewHeader.PathLabelClick(Sender: TObject);
var
  walkPath, selectedDir, dirNameToSelect: String;
begin
  FFileView.SetFocus;

  if FPathLabel.SelectedDir <> '' then
  begin
    // User clicked on a subdirectory of the path.
    walkPath := FFileView.CurrentPath;
    selectedDir := FPathLabel.SelectedDir;
    FFileView.CurrentPath := selectedDir;

    while (Length(walkPath) > Length(selectedDir) + 1) do
    begin
      dirNameToSelect := ExtractFileName(ExcludeTrailingPathDelimiter(walkPath));
      walkPath := FFileView.FileSource.GetParentDir(walkPath);
    end;
    FFileView.SetActiveFile(dirNameToSelect);
  end
  else
    tmViewHistoryMenu.Enabled:=TRUE; //Let's start timer. If it's a double-click, we'll abort timer otherwise we'll show history as before but 250ms later.
end;

procedure TFileViewHeader.tmViewHistoryMenuTimer(Sender: TObject);
begin
  tmViewHistoryMenu.Enabled:=FALSE;
  frmMain.Commands.cm_ViewHistory([]);
end;

procedure TFileViewHeader.PathLabelMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint;var Handled:Boolean);
begin
  if (ssCtrl in Shift) and (gFonts[dcfPathEdit].Size < gFonts[dcfPathEdit].MaxValue) then
  begin
    gFonts[dcfPathEdit].Size:= gFonts[dcfPathEdit].Size + 1;
    frmMain.ForEachView(@EachViewUpdateHeader, nil);
  end;
end;

procedure TFileViewHeader.PathLabelMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint;var Handled:Boolean);
begin
  if (ssCtrl in Shift) and (gFonts[dcfPathEdit].Size > gFonts[dcfPathEdit].MinValue) then
  begin
     gFonts[dcfPathEdit].Size:= gFonts[dcfPathEdit].Size - 1;
     frmMain.ForEachView(@EachViewUpdateHeader, nil);
  end;
end;

{ TFileViewHeader.PathLabelDblClick }
{ -If we double-click on the the path label, it shows the Hot Dir popup menu at the cursor position.
  -If we click just once, after the 250ms of the timer, it shows the history.
  This will make both kind of people happy AND will make DC like TC}
procedure TFileViewHeader.PathLabelDblClick(Sender: TObject);
begin
  tmViewHistoryMenu.Enabled:=FALSE; //Cancel the possibility of a left click
  FFileView.SetFocus;
  frmMain.Commands.cm_DirHotList(['position=cursor']);
end;

procedure TFileViewHeader.PathLabelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbMiddle:
      begin
        FFileView.SetFocus;
        frmMain.Commands.cm_DirHotList(['position=cursor']);
      end;

    mbRight:
      begin
        ShowPathEdit;
      end;
  end;
end;

procedure TFileViewHeader.AddressLabelClick(Sender: TObject);
var
  walkPath, dirNameToSelect: String;
begin
  FFileView.SetFocus;

  if (FAddressLabel.AllowHighlight) and
     (Length(FAddressLabel.SelectedDir) > 0) then
  begin
    // User clicked on a subdirectory of the address.
    walkPath := FFileView.CurrentAddress;
    SetFileSystemPath(FFileView, FAddressLabel.SelectedDir);

    while (Length(walkPath) > Length(FAddressLabel.SelectedDir) + 1) do
    begin
      dirNameToSelect := ExtractFileName(ExcludeTrailingPathDelimiter(walkPath));
      walkPath := FFileView.FileSource.GetParentDir(walkPath);
    end;
    FFileView.SetActiveFile(dirNameToSelect);
  end;
end;

procedure TFileViewHeader.AddressLabelMouseEnter(Sender: TObject);
begin
  FAddressLabel.AllowHighlight:= FFileView.FileSource is TArchiveFileSource;
end;

procedure TFileViewHeader.EachViewUpdateHeader(AFileView: TFileView; UserData: Pointer);
begin
  TFileViewWithPanels(AFileView).Header.UpdateFont;
end;

procedure TFileViewHeader.PathLabelSetColor(APathLabel: TPathLabel);
begin
  APathLabel.ActiveColor:= gPathActiveColor;
  APathLabel.ActiveFontColor:= gPathActiveFontColor;
  APathLabel.InactiveColor:= gPathInactiveColor;
  APathLabel.InactiveFontColor:= gPathInactiveFontColor;
end;

constructor TFileViewHeader.Create(AOwner: TFileView; AParent: TWinControl);
begin
  inherited Create(AOwner);

  FFileView:= AOwner;
  Parent:= AParent;
  Align:= alTop;
  BevelInner:= bvNone;
  BevelOuter:= bvNone;
  AutoSize:= True;
  DoubleBuffered:= True;

  FAddressLabel := TPathLabel.Create(Self, False);
  FAddressLabel.Parent := Self;
  FAddressLabel.BorderSpacing.Bottom := 1;

  FPathLabel := TPathLabel.Create(Self, True);
  FPathLabel.Parent := Self;

  PathLabelSetColor(FPathLabel);
  PathLabelSetColor(FAddressLabel);

  // Display path below address.
  // For correct alignment, first put path at the top, then address at the top.
  FPathLabel.Align := alTop;
  FAddressLabel.Align := alTop;

  FPathEdit:= TKASPathEdit.Create(FPathLabel);
  FPathEdit.Parent:= Self;
  FPathEdit.Visible:= False;
  FPathEdit.TabStop:= False;
  FPathEdit.ObjectTypes:= [otFolders, otHidden];

  OnResize:= @HeaderResize;

  FPathEdit.OnExit:= @PathEditExit;
  FPathEdit.OnKeyDown:= @PathEditKeyDown;

  FPathLabel.OnClick := @PathLabelClick;
  FPathLabel.OnDblClick := @PathLabelDblClick;
  FPathLabel.OnMouseUp := @PathLabelMouseUp;

  FPathLabel.OnMouseWheelDown := @PathLabelMouseWheelDown;
  FPathLabel.OnMouseWheelUp := @PathLabelMouseWheelUp;


  FAddressLabel.OnClick := @AddressLabelClick;
  FAddressLabel.OnMouseEnter:= @AddressLabelMouseEnter;
  
  tmViewHistoryMenu := TTimer.Create(Self); //Timer used to show history after a while in case it was not a double click to show Hot dir
  tmViewHistoryMenu.Enabled  := False;
  tmViewHistoryMenu.Interval := 250;
  tmViewHistoryMenu.OnTimer  := @tmViewHistoryMenuTimer;

  UpdateFont;
end;

procedure TFileViewHeader.HeaderResize(Sender: TObject);
begin
  UpdateAddressLabel;
  UpdatePathLabel;
end;

procedure TFileViewHeader.UpdateAddressLabel;
begin
  if FFileView.CurrentAddress = '' then
  begin
    FAddressLabel.Visible := False;
  end
  else
  begin
    FAddressLabel.Top:= 0;
    FAddressLabel.Caption := FFileView.CurrentAddress;
    FAddressLabel.Visible := True;
  end;
end;

procedure TFileViewHeader.UpdatePathLabel;
begin
  FPathLabel.Caption := MinimizeFilePath(FFileView.CurrentPath, FPathLabel.Canvas, FPathLabel.Width);
end;

procedure TFileViewHeader.UpdateFont;
begin
  FontOptionsToFont(gFonts[dcfPathEdit], FAddressLabel.Font);
  FontOptionsToFont(gFonts[dcfPathEdit], FPathLabel.Font);
  FontOptionsToFont(gFonts[dcfPathEdit], FPathEdit.Font);
end;

procedure TFileViewHeader.ShowPathEdit;
begin
  with FPathLabel do
  begin
    FPathEdit.SetBounds(Left, Top, Width, Height);
    FPathEdit.Text := FFileView.CurrentPath;
    FPathEdit.Visible := True;
    FPathEdit.SetFocus;
  end;
end;

procedure TFileViewHeader.SetActive(bActive: Boolean);
begin
  FAddressLabel.SetActive(bActive);
  FPathLabel.SetActive(bActive);
end;

{ TFileViewFixedHeader }

procedure TFileViewFixedHeader.UpdateState;
var
  i, Index: Integer;
  MaxState: THeaderSectionState;
  P: TPoint;
begin
  MaxState := hsNormal;
  if Enabled then
    if FDown then
    begin
      MaxState := hsPressed;
      Index := FSelectedSection;
    end else if FMouseInControl then
    begin
      MaxState := hsHot;
      P := ScreenToClient(Mouse.CursorPos);
      Index := GetSectionAt(P);
    end;

  for i := 0 to Sections.Count - 1 do
    if (i <> Index) then
      Sections[i].State := hsNormal
    else
      Sections[i].State := MaxState;
end;

procedure TFileViewFixedHeader.SectionClick(Section: THeaderSection);
var
  SortingDirection : TSortDirection;
  NewSorting: TFileSortings;
  SortFunctions: TFileFunctions;
begin
  with FFileView do
  begin
    NewSorting := Sorting;
    SortFunctions := FSorting[Section.Index].SortFunctions;
    if [ssShift, ssCtrl] * GetKeyShiftStateEx = [] then
      begin
        SortingDirection := GetSortDirection(NewSorting, SortFunctions);
        if SortingDirection = sdNone then
          begin
            // If there is no direction currently, sort "sdDescending" for size and date.
            // Commonly, we search seek more often for most recent files then older any others.
            // When sorting by size, often it is to find larger file to make room.
            // Anyway, it makes DC like TC, and also, Windows Explorer do the same.
            case SortFunctions[0] of
              fsfSize, fsfModificationTime, fsfCreationTime, fsfLastAccessTime: SortingDirection:= sdDescending;
              else SortingDirection:= sdAscending;
            end;
          end
        else begin
          SortingDirection := ReverseSortDirection(SortingDirection);
        end;
        NewSorting := nil;
      end
    else
      begin
        // If there is no direction currently, sort "sdDescending" for size and date (see previous comment).
        case SortFunctions[0] of
          fsfSize, fsfModificationTime, fsfCreationTime, fsfLastAccessTime: SortingDirection:= sdDescending;
          else SortingDirection:= sdAscending;
        end;
      end;

    AddOrUpdateSorting(NewSorting, SortFunctions, SortingDirection);
    FFileView.Sorting:= NewSorting;
  end;
  inherited SectionClick(Section);
end;

procedure TFileViewFixedHeader.Click;
var
  Index: Integer;
begin
  if FDown then
  begin
    inherited Click;
    Index := GetSectionAt(ScreenToClient(Mouse.CursorPos));
    if Index <> -1 then
      SectionClick(Sections[Index]);
  end;
end;

procedure TFileViewFixedHeader.DblClick;
begin
  Click;
end;

procedure TFileViewFixedHeader.UpdateHeader;
var
  I: Integer;
begin
  for I:= 0 to Sections.Count - 1 do
  begin
    Sections[I].ImageIndex:= SortingImageIndex[FSorting[I].SortDirection];
  end;
end;

procedure TFileViewFixedHeader.UpdateSorting(Sorting: TFileSortings);
var
  I, J: Integer;
begin
  for I:= Low(FSorting) to High(FSorting) do
  begin
    FSorting[I].SortDirection:= sdNone;
    for J:= Low(Sorting) to High(Sorting) do
    begin
      if (FSorting[I].SortFunctions[0] = Sorting[J].SortFunctions[0]) or
         ((Sorting[J].SortFunctions[0] = fsfName) and (FSorting[I].SortFunctions[0] = fsfNameNoExtension))then
      begin
        FSorting[I].SortDirection:= Sorting[J].SortDirection;
        Break;
      end;
    end;
  end;
  UpdateHeader;
end;

procedure TFileViewFixedHeader.MouseEnter;
begin
  inherited MouseEnter;
  if not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    UpdateState;
  end;
end;

procedure TFileViewFixedHeader.MouseLeave;
begin
  inherited MouseLeave;
  if not (csDesigning in ComponentState) then
  begin
    FMouseInControl := False;
    FDown := False;
    UpdateState;
  end;
end;

procedure TFileViewFixedHeader.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not (csDesigning in ComponentState) then
  begin
    FDown:= True;
    FSelectedSection:=GetSectionAt(Point(X, Y));
    UpdateState;
  end;
end;

procedure TFileViewFixedHeader.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not (csDesigning in ComponentState) then
  begin
    UpdateState;
  end;
end;

procedure TFileViewFixedHeader.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if not (csDesigning in ComponentState) then
  begin
    FDown:= False;
    UpdateState;
  end;
end;

{$if lcl_fullversion >= 1070000}
procedure TFileViewFixedHeader.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
begin
  // Don't auto adjust vertical layout
  inherited DoAutoAdjustLayout(AMode, AXProportion, 1.0);
end;
{$endif}

constructor TFileViewFixedHeader.Create(AOwner: TFileView; AParent: TWinControl);
var
  I: Integer;
begin
  inherited Create(AOwner);

  FFileView:= AOwner;
  Parent:= AParent;
  Align:= alTop;
  DoubleBuffered:= True;

  Sections.Add.Text:= rsColName;
  Sections.Add.Text:= rsColExt;
  Sections.Add.Text:= rsColSize;
  Sections.Add.Text:= rsColDate;
  Sections.Add.Text:= rsColAttr;

  Images:= TImageList.CreateSize(gIconsSize, gIconsSize);
  Images.Add(PixMapManager.GetBitmap(PixMapManager.GetIconBySortingDirection(sdAscending)), nil);
  Images.Add(PixMapManager.GetBitmap(PixMapManager.GetIconBySortingDirection(sdDescending)), nil);

  SetLength(FSorting, 5);
  for I:= Low(FSorting) to High(FSorting) do
  SetLength(FSorting[I].SortFunctions, 1);
  FSorting[0].SortDirection:= sdNone;
  FSorting[0].SortFunctions[0]:= fsfNameNoExtension;
  FSorting[1].SortDirection:= sdNone;
  FSorting[1].SortFunctions[0]:= fsfExtension;
  FSorting[2].SortDirection:= sdNone;
  FSorting[2].SortFunctions[0]:= fsfSize;
  FSorting[3].SortDirection:= sdNone;
  FSorting[3].SortFunctions[0]:= fsfModificationTime;
  FSorting[4].SortDirection:= sdNone;
  FSorting[4].SortFunctions[0]:= fsfAttr;
end;

destructor TFileViewFixedHeader.Destroy;
begin
  Images.Free;
  inherited Destroy;
end;

end.

