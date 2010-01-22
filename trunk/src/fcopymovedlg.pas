unit fCopyMoveDlg;

{$mode objfpc}{$H+}

interface

uses
  LResources, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Buttons, ExtCtrls, lclproc, EditBtn, ComCtrls, Menus,
  uFileSystemCopyOperation, uFileSystemMoveOperation, uFileViewNotebook,
  uOperationsManager;

type

  TCopyMoveDlgType = (cmdtCopy, cmdtMove);

  { TfrmCopyDlg }

  TfrmCopyDlg = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnOptions: TButton;
    btnSaveOptions: TButton;
    cbDropReadOnlyFlag: TCheckBox;
    cbFollowLinks: TCheckBox;
    cbCorrectLinks: TCheckBox;
    cbCheckFreeSpace: TCheckBox;
    cmbFileType: TComboBox;
    cmbFileExists: TComboBox;
    cmbDirectoryExists: TComboBox;
    edtDst: TEdit;
    grpOptions: TGroupBox;
    lblFileExists: TLabel;
    lblDirectoryExists: TLabel;
    lblCopySrc: TLabel;
    lblFileType: TLabel;
    miAutoStart: TMenuItem;
    miQueueFirst: TMenuItem;
    miQueueLast: TMenuItem;
    miManualStart: TMenuItem;
    pnlCheckboxes: TPanel;
    pnlSelector: TPanel;
    btnStartMode: TSpeedButton;
    pmOperationStartMode: TPopupMenu;
    procedure btnCancelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOKMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOptionsClick(Sender: TObject);
    procedure btnSaveOptionsClick(Sender: TObject);
    procedure btnStartModeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure frmCopyDlgKeyPress(Sender: TObject; var Key: Char);
    procedure frmCopyDlgShow(Sender: TObject);
    procedure miAutoStartClick(Sender: TObject);
    procedure miManualStartClick(Sender: TObject);
    procedure miQueueFirstClick(Sender: TObject);
    procedure miQueueLastClick(Sender: TObject);

  private
    FDialogType: TCopyMoveDlgType;
    noteb: TFileViewNotebook;
    FOperationStartingState: TOperationStartingState;

    function ShowTabsSelector: integer;
    procedure TabsSelector(Sender: TObject);
    procedure TabsSelectorMouseDown(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
    procedure ShowOptions(bShow: Boolean);

    procedure SetStartModeMenuText;
    procedure UnCheckStartModeMenuItems;

  public
    constructor Create(TheOwner: TComponent; DialogType: TCopyMoveDlgType); reintroduce;
    procedure SetOperationOptions(CopyOperation: TFileSystemCopyOperation); overload;
    procedure SetOperationOptions(MoveOperation: TFileSystemMoveOperation); overload;

    property OperationStartingState: TOperationStartingState read FOperationStartingState;
  end;


implementation

uses
  fMain, LCLType, uGlobs, uFileSourceOperationOptions, uLng;

constructor TfrmCopyDlg.Create(TheOwner: TComponent; DialogType: TCopyMoveDlgType);
begin
  noteb := nil;
  FDialogType := DialogType;
  FOperationStartingState := ossAutoStart;
  pmOperationStartMode := nil;
  inherited Create(TheOwner);
end;

procedure TfrmCopyDlg.TabsSelector(Sender: TObject);
begin
  edtDst.Text := noteb[(Sender as TButton).tag].CurrentPath;
end;

procedure TfrmCopyDlg.TabsSelectorMouseDown(Sender: TObject; Button: TMouseButton;
                                            Shift: TShiftState; X, Y: Integer);
begin
  edtDst.Text := noteb[(Sender as TButton).tag].CurrentPath;
end;

function TfrmCopyDlg.ShowTabsSelector: integer;
var
  btnS, btnL: TButton;
  i, tc: PtrInt;
  st: TStringList;
  s: String;
begin
  noteb := frmMain.NotActiveNotebook;

  if noteb.PageCount = 1 then
    begin
      Result:=0;
      exit;
    end;

  tc := noteb.PageCount;
  st := TStringList.Create;
  try
    for i:=0 to tc-1 do
    if noteb.View[i].Visible then
      begin
        s:=noteb[i].CurrentPath;
        if st.IndexOf(s)=-1 then
          begin
            st.Add(s);
            st.Objects[st.Count-1]:=TObject(i);
          end;
      end;

    tc := st.Count;
    btnL := nil;
    if tc>10 then tc:=10;
    for i:=0 to tc-1 do
      begin
        btnS:= TButton.Create(Self);
        btns.Parent:=pnlSelector;
        btns.Tag:=PtrInt(st.Objects[i]);
        if i<9 then
          btns.Caption := '&' + IntToStr(i+1) + ' - ' + noteb.Page[PtrInt(st.Objects[i])].Caption
        else
          btns.Caption := '&0 - ' + noteb.Page[PtrInt(st.Objects[i])].Caption;

        btnS.OnClick := @TabsSelector;
        btnS.OnMouseDown := @TabsSelectorMouseDown;

        btns.AutoSize:=True;
        btns.Left := 2;
        btns.Top := 0;
        btns.Anchors :=[akLeft,akTop,akBottom];
        btns.Visible := True;

        if btnL <> nil then
        begin
          btns.AnchorSideLeft.Control := btnL;
          btns.AnchorSideLeft.Side := asrRight;
        end;

        btnL := btnS;
        if (Self.Width < (btnL.Left+btnL.Width+200)) then // 200 = Ok + Cancel
          Self.Width := (btnL.Left+btnL.Width+200);
      end;

  finally
    st.Free;
  end;
end;


procedure TfrmCopyDlg.frmCopyDlgShow(Sender: TObject);
begin
  case FDialogType of
    cmdtCopy:
      begin
        Caption := rsDlgCp;
      end;

    cmdtMove:
      begin
        Caption := rsDlgMv;
        cbDropReadOnlyFlag.Visible := False;
        cbFollowLinks.Visible := False;
      end;
  end;

  if gShowCopyTabSelectPanel then
    ShowTabsSelector;

  edtDst.SelectAll;
  edtDst.SetFocus;
end;

procedure TfrmCopyDlg.miAutoStartClick(Sender: TObject);
begin
  btnOK.Caption := rsOperStartStateAutoStart;
  UnCheckStartModeMenuItems;
  miAutoStart.Checked := True;
  FOperationStartingState := ossAutoStart;
end;

procedure TfrmCopyDlg.miManualStartClick(Sender: TObject);
begin
  btnOK.Caption := rsOperStartStateManualStart;
  UnCheckStartModeMenuItems;
  miManualStart.Checked := True;
  FOperationStartingState := ossManualStart;
end;

procedure TfrmCopyDlg.miQueueFirstClick(Sender: TObject);
begin
  btnOK.Caption := rsOperStartStateQueueFirst;
  UnCheckStartModeMenuItems;
  miQueueFirst.Checked := True;
  FOperationStartingState := ossQueueFirst;
end;

procedure TfrmCopyDlg.miQueueLastClick(Sender: TObject);
begin
  btnOK.Caption := rsOperStartStateQueueLast;
  UnCheckStartModeMenuItems;
  miQueueLast.Checked := True;
  FOperationStartingState := ossQueueLast;
end;

procedure TfrmCopyDlg.frmCopyDlgKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#27 then
  begin
    ModalResult:=mrCancel;
    Key := #0;
  end
  else if Key=#13 then
  begin
    ModalResult:=mrOK;
    Key:=#0;
  end;
end;

procedure TfrmCopyDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if gShowCopyTabSelectPanel and (edtDst.Focused=false) and (key-49<pnlSelector.ControlCount) then
    begin
      if (key>=VK_1) and (Key<=VK_9) then
         TButton(pnlSelector.Controls[key-49]).Click;

      if key=vk_0 then
        TButton(pnlSelector.Controls[9]).Click;
    end;
end;

procedure TfrmCopyDlg.btnCancelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    ModalResult := btnCancel.ModalResult;
end;

procedure TfrmCopyDlg.btnOKMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    ModalResult := btnOk.ModalResult;
end;

procedure TfrmCopyDlg.btnOptionsClick(Sender: TObject);
begin
  ShowOptions(not grpOptions.Visible);
end;

procedure TfrmCopyDlg.btnSaveOptionsClick(Sender: TObject);
begin
  case cmbFileExists.ItemIndex of
    0: gOperationOptionFileExists := fsoofeNone;
    1: gOperationOptionFileExists := fsoofeOverwrite;
    2: gOperationOptionFileExists := fsoofeSkip;
  end;
  case cmbDirectoryExists.ItemIndex of
    0: gOperationOptionDirectoryExists := fsoodeNone;
    1: gOperationOptionDirectoryExists := fsoodeDelete;
    2: gOperationOptionDirectoryExists := fsoodeCopyInto;
    3: gOperationOptionDirectoryExists := fsoodeSkip;
  end;
  gDropReadOnlyFlag := (cbDropReadOnlyFlag.State = cbChecked);
  case cbFollowLinks.State of
    cbChecked   : gOperationOptionSymLinks := fsooslFollow;
    cbUnchecked : gOperationOptionSymLinks := fsooslDontFollow;
    cbGrayed    : gOperationOptionSymLinks := fsooslNone;
  end;
  gOperationOptionCorrectLinks := cbCorrectLinks.Checked;
  gOperationOptionCheckFreeSpace := cbCheckFreeSpace.Checked;
end;

procedure TfrmCopyDlg.btnStartModeClick(Sender: TObject);
begin
  btnOK.PopupMenu.PopUp;
end;

procedure TfrmCopyDlg.FormCreate(Sender: TObject);
begin
  pnlSelector.Visible := gShowCopyTabSelectPanel;

  // Fix align of options box and dialog size at start.
  if not pnlSelector.Visible then
    grpOptions.Top := grpOptions.Top -
                      (pnlSelector.Height +
                       pnlSelector.BorderSpacing.Top +
                       pnlSelector.BorderSpacing.Bottom);

  ShowOptions(False);

  // Load default options.
  case gOperationOptionFileExists of
    fsoofeNone     : cmbFileExists.ItemIndex := 0;
    fsoofeOverwrite: cmbFileExists.ItemIndex := 1;
    fsoofeSkip     : cmbFileExists.ItemIndex := 2;
  end;
  case gOperationOptionDirectoryExists of
    fsoodeNone     : cmbDirectoryExists.ItemIndex := 0;
    fsoodeDelete   : cmbDirectoryExists.ItemIndex := 1;
    fsoodeCopyInto : cmbDirectoryExists.ItemIndex := 2;
    fsoodeSkip     : cmbDirectoryExists.ItemIndex := 3;
  end;
  cbDropReadOnlyFlag.Checked := gDropReadOnlyFlag;
  case gOperationOptionSymLinks of
    fsooslFollow     : cbFollowLinks.State := cbChecked;
    fsooslDontFollow : cbFollowLinks.State := cbUnchecked;
    fsooslNone       : cbFollowLinks.State := cbGrayed;
  end;
  cbCorrectLinks.Checked := gOperationOptionCorrectLinks;
  cbCheckFreeSpace.Checked := gOperationOptionCheckFreeSpace;

  // Start mode menu.
  SetStartModeMenuText;
  miAutoStartClick(nil);
end;

procedure TfrmCopyDlg.ShowOptions(bShow: Boolean);
begin
  if bShow then
  begin
    Self.Height := grpOptions.Top + grpOptions.Height + grpOptions.BorderSpacing.Bottom;
    grpOptions.Visible := True;
  end
  else
  begin
    grpOptions.Visible := False;
    Self.Height := grpOptions.Top;
  end;
end;

procedure TfrmCopyDlg.SetStartModeMenuText;
begin
  miAutoStart.Caption   := rsOperStartStateAutoStart;
  miManualStart.Caption := rsOperStartStateManualStart;
  miQueueFirst.Caption  := rsOperStartStateQueueFirst;
  miQueueLast.Caption   := rsOperStartStateQueueLast;
end;

procedure TfrmCopyDlg.UnCheckStartModeMenuItems;
var
  i: Integer;
begin
  for i := 0 to pmOperationStartMode.Items.Count - 1 do
    pmOperationStartMode.Items[i].Checked := False;
end;

procedure TfrmCopyDlg.SetOperationOptions(CopyOperation: TFileSystemCopyOperation);
begin
  with CopyOperation do
  begin
    case cmbFileExists.ItemIndex of
      0: FileExistsOption := fsoofeNone;
      1: FileExistsOption := fsoofeOverwrite;
      2: FileExistsOption := fsoofeSkip;
    end;
    case cmbDirectoryExists.ItemIndex of
      0: DirExistsOption := fsoodeNone;
      1: DirExistsOption := fsoodeDelete;
      2: DirExistsOption := fsoodeCopyInto;
      3: DirExistsOption := fsoodeSkip;
    end;
    case cbFollowLinks.State of
      cbChecked  : SymLinkOption := fsooslFollow;
      cbUnchecked: SymLinkOption := fsooslDontFollow;
      cbGrayed   : SymLinkOption := fsooslNone;
    end;
    DropReadOnlyAttribute := (cbDropReadOnlyFlag.State = cbChecked);
    CorrectSymLinks := cbCorrectLinks.Checked;
    CheckFreeSpace := cbCheckFreeSpace.Checked;
  end;
end;

procedure TfrmCopyDlg.SetOperationOptions(MoveOperation: TFileSystemMoveOperation);
begin
  with MoveOperation do
  begin
    case cmbFileExists.ItemIndex of
      0: FileExistsOption := fsoofeNone;
      1: FileExistsOption := fsoofeOverwrite;
      2: FileExistsOption := fsoofeSkip;
    end;
    case cmbDirectoryExists.ItemIndex of
      0: DirExistsOption := fsoodeNone;
      1: DirExistsOption := fsoodeDelete;
      2: DirExistsOption := fsoodeCopyInto;
      3: DirExistsOption := fsoodeSkip;
    end;
    //DropReadOnlyAttribute := (cbDropReadOnlyFlag.State = cbChecked);
    CorrectSymLinks := cbCorrectLinks.Checked;
    CheckFreeSpace := cbCheckFreeSpace.Checked;
  end;
end;

initialization
 {$I fcopymovedlg.lrs}
end.
