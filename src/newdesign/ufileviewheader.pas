unit uFileViewHeader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, ComCtrls,
  uPathLabel, uFileView, KASPathEdit;

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

  { TBriefHeaderControl }

  TBriefHeaderControl = class(THeaderControl)
  private
    FDown: Boolean;
    FMouseInControl: Boolean;
    FSelectedSection: Integer;
    procedure UpdateState;
  protected
    procedure Click; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                      X, Y: Integer); override;
  end;

implementation

uses
  LCLType, ShellCtrls, uDCUtils, DCOSUtils, DCStrUtils,
  fMain, uFileSourceUtil;

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

{ TBriefHeaderControl }

procedure TBriefHeaderControl.UpdateState;
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

procedure TBriefHeaderControl.Click;
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

procedure TBriefHeaderControl.MouseEnter;
begin
  inherited MouseEnter;
  if not (csDesigning in ComponentState) then
  begin
    FMouseInControl := True;
    UpdateState;
  end;
end;

procedure TBriefHeaderControl.MouseLeave;
begin
  inherited MouseLeave;
  if not (csDesigning in ComponentState) then
  begin
    FMouseInControl := False;
    FDown := False;
    UpdateState;
  end;
end;

procedure TBriefHeaderControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not (csDesigning in ComponentState) then
  begin
    FDown:= True;
    FSelectedSection:=GetSectionAt(Point(X, Y));
    UpdateState;
  end;
end;

procedure TBriefHeaderControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not (csDesigning in ComponentState) then
  begin
    UpdateState;
  end;
end;

procedure TBriefHeaderControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if not (csDesigning in ComponentState) then
  begin
    FDown:= False;
    UpdateState;
  end;
end;

end.
