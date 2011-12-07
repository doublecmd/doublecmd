unit fCopyMoveDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, ExtCtrls, Menus,
  uFileViewNotebook,
  uFileSourceOperation,
  uFileSourceOperationOptionsUI,
  uOperationsManager;

type

  TCopyMoveDlgType = (cmdtCopy, cmdtMove);

  { TfrmCopyDlg }

  TfrmCopyDlg = class(TForm)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnOptions: TButton;
    btnSaveOptions: TButton;
    edtDst: TEdit;
    grpOptions: TGroupBox;
    lblCopySrc: TLabel;
    miAutoStart: TMenuItem;
    miQueueFirst: TMenuItem;
    miQueueLast: TMenuItem;
    miManualStart: TMenuItem;
    pnlButtons: TPanel;
    pnlOptions: TPanel;
    pnlSelector: TPanel;
    btnStartMode: TSpeedButton;
    pmOperationStartMode: TPopupMenu;
    procedure btnCancelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOKMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOptionsClick(Sender: TObject);
    procedure btnOptionsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnSaveOptionsClick(Sender: TObject);
    procedure btnStartModeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure frmCopyDlgShow(Sender: TObject);
    procedure miAutoStartClick(Sender: TObject);
    procedure miManualStartClick(Sender: TObject);
    procedure miQueueFirstClick(Sender: TObject);
    procedure miQueueLastClick(Sender: TObject);

  private
    FDialogType: TCopyMoveDlgType;
    noteb: TFileViewNotebook;
    FOperationStartingState: TOperationStartingState;
    FOperationOptionsUIClass: TFileSourceOperationOptionsUIClass;
    FOperationOptionsUI: TFileSourceOperationOptionsUI;

    function ShowTabsSelector: integer;
    procedure TabsSelector(Sender: TObject);
    procedure TabsSelectorMouseDown(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
    procedure ShowOptions(bShow: Boolean);

    procedure SetStartModeMenuText;
    procedure UnCheckStartModeMenuItems;

  public
    constructor Create(TheOwner: TComponent; DialogType: TCopyMoveDlgType;
                       AOperationOptionsUIClass: TFileSourceOperationOptionsUIClass); reintroduce;
    procedure SetOperationOptions(Operation: TFileSourceOperation);

    property OperationStartingState: TOperationStartingState read FOperationStartingState;
  end;


implementation

{$R *.lfm}

uses
  fMain, LCLType, uGlobs, uLng;

constructor TfrmCopyDlg.Create(TheOwner: TComponent; DialogType: TCopyMoveDlgType;
                               AOperationOptionsUIClass: TFileSourceOperationOptionsUIClass);
begin
  noteb := nil;
  FDialogType := DialogType;
  FOperationOptionsUIClass := AOperationOptionsUIClass;
  FOperationOptionsUI := nil;
  FOperationStartingState := ossAutoStart;
  pmOperationStartMode := nil;
  inherited Create(TheOwner);
end;

procedure TfrmCopyDlg.SetOperationOptions(Operation: TFileSourceOperation);
begin
  if Assigned(FOperationOptionsUI) then
    FOperationOptionsUI.SetOperationOptions(Operation);
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

  case Key of
    VK_ESCAPE: // Must handle before drag manager. Lazarus bug 0020676.
      begin
        ModalResult := mrCancel;
        Key := 0;
      end;
  end;
end;

procedure TfrmCopyDlg.btnCancelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  if (Button = mbLeft) and (Sender = FindLCLControl(Mouse.CursorPos)) then
    ModalResult := btnCancel.ModalResult;
{$ENDIF}
end;

procedure TfrmCopyDlg.btnOKMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  if (Button = mbLeft) and (Sender = FindLCLControl(Mouse.CursorPos)) then
    ModalResult := btnOk.ModalResult;
{$ENDIF}
end;

procedure TfrmCopyDlg.btnOptionsClick(Sender: TObject);
begin
{$IF NOT (DEFINED(LCLGTK) or DEFINED(LCLGTK2))}
  ShowOptions(not pnlOptions.Visible);
{$ENDIF}
end;

procedure TfrmCopyDlg.btnOptionsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  if (Button = mbLeft) and (Sender = FindLCLControl(Mouse.CursorPos)) then
    ShowOptions(not pnlOptions.Visible);
{$ENDIF}
end;

procedure TfrmCopyDlg.btnSaveOptionsClick(Sender: TObject);
begin
  if Assigned(FOperationOptionsUI) then
    FOperationOptionsUI.SaveOptions;
end;

procedure TfrmCopyDlg.btnStartModeClick(Sender: TObject);
begin
  btnOK.PopupMenu.PopUp;
end;

procedure TfrmCopyDlg.FormCreate(Sender: TObject);
begin
  pnlSelector.Visible := gShowCopyTabSelectPanel;

  // Fix align of options panel and dialog size at start.
  if not pnlSelector.Visible then
    pnlOptions.Top := pnlOptions.Top -
                      (pnlSelector.Height +
                       pnlSelector.BorderSpacing.Top +
                       pnlSelector.BorderSpacing.Bottom);

  // Operation options.
  if Assigned(FOperationOptionsUIClass) then
  begin
    FOperationOptionsUI := FOperationOptionsUIClass.Create(Self);
    FOperationOptionsUI.Parent := grpOptions;
  end
  else
    btnOptions.Visible := False;
  ShowOptions(False);

  // Start mode menu.
  SetStartModeMenuText;
  miAutoStartClick(nil);
end;

procedure TfrmCopyDlg.ShowOptions(bShow: Boolean);
begin
  if bShow then
  begin
    pnlOptions.Visible := True;
    Self.Height := pnlOptions.Top + pnlOptions.Height +
                   pnlOptions.BorderSpacing.Top + pnlOptions.BorderSpacing.Bottom;
  end
  else
  begin
    pnlOptions.Visible := False;
    Self.Height := pnlOptions.Top;
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

end.
