unit uFileViewHeader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, ComCtrls,
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
    procedure PathLabelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(AOwner: TFileView; AParent: TWinControl); reintroduce;

    procedure UpdateAddressLabel;
    procedure UpdatePathLabel;

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
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                      X, Y: Integer); override;
  public
    constructor Create(AOwner: TFileView; AParent: TWinControl); reintroduce;
    destructor Destroy; override;

    procedure Click; override;
    procedure UpdateHeader;
    procedure UpdateSorting(Sorting: TFileSortings);
  end;

implementation

uses
  LCLType, ShellCtrls, uDCUtils, DCOSUtils, DCStrUtils, uKeyboard,
  fMain, uFileSourceUtil, uGlobs, uPixMapManager, uLng, uFileFunctions;

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
  NewPath: UTF8String;
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
          ChooseFileSource(FFileView, NewPath)
        else
          begin
            ChooseFileSource(FFileView, ExtractFileDir(NewPath));
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
  walkPath, dirNameToSelect: UTF8String;
begin
  FFileView.SetFocus;

  if FPathLabel.SelectedDir <> '' then
  begin
    // User clicked on a subdirectory of the path.
    walkPath := FFileView.CurrentPath;
    FFileView.CurrentPath := FPathLabel.SelectedDir;

    while (Length(walkPath) > Length(FPathLabel.SelectedDir) + 1) do
    begin
      dirNameToSelect := ExtractFileName(ExcludeTrailingPathDelimiter(walkPath));
      walkPath := FFileView.FileSource.GetParentDir(walkPath);
    end;
    FFileView.SetActiveFile(dirNameToSelect);
  end
  else
    frmMain.Commands.cm_ViewHistory([]);
end;

procedure TFileViewHeader.PathLabelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbMiddle:
      begin
        FFileView.SetFocus;
        frmMain.Commands.cm_DirHotList([]);
      end;

    mbRight:
      begin
        ShowPathEdit;
      end;
  end;
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

  FAddressLabel := TPathLabel.Create(Self, False);
  FAddressLabel.Parent := Self;
  FAddressLabel.BorderSpacing.Bottom := 1;

  FPathLabel := TPathLabel.Create(Self, True);
  FPathLabel.Parent := Self;

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
  FPathLabel.OnMouseUp := @PathLabelMouseUp;
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
    FAddressLabel.Visible := True;
    FAddressLabel.Caption := FFileView.CurrentAddress;
  end;
end;

procedure TFileViewHeader.UpdatePathLabel;
begin
  FPathLabel.Caption := MinimizeFilePath(FFileView.CurrentPath, FPathLabel.Canvas, FPathLabel.Width);
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
          SortingDirection := sdAscending
        else
          SortingDirection := ReverseSortDirection(SortingDirection);
        NewSorting := nil;
      end
    else
      begin
        SortingDirection := sdAscending;
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

constructor TFileViewFixedHeader.Create(AOwner: TFileView; AParent: TWinControl);
var
  I: Integer;
begin
  inherited Create(AOwner);

  FFileView:= AOwner;
  Parent:= AParent;
  Align:= alTop;

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

