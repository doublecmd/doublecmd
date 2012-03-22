unit fCopyMoveDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, Buttons, ExtCtrls, Menus, KASPathEdit,
  uFileSource,
  uFileViewNotebook,
  uFileSourceOperation,
  uFileSourceOperationOptionsUI,
  uOperationsManager,
  uFormCommands;

type

  TCopyMoveDlgType = (cmdtCopy, cmdtMove);

  { TfrmCopyDlg }

  TfrmCopyDlg = class(TForm, IFormCommands)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnAddToQueue: TBitBtn;
    btnOptions: TButton;
    btnSaveOptions: TButton;
    edtDst: TKASPathEdit;
    grpOptions: TGroupBox;
    lblCopySrc: TLabel;
    pnlButtons: TPanel;
    pnlOptions: TPanel;
    pnlSelector: TPanel;
    btnCreateSpecialQueue: TSpeedButton;
    procedure btnAddToQueueClick(Sender: TObject);
    procedure btnCancelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnAddToQueueMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOKClick(Sender: TObject);
    procedure btnOKMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOptionsClick(Sender: TObject);
    procedure btnOptionsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnSaveOptionsClick(Sender: TObject);
    procedure btnStartModeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure frmCopyDlgShow(Sender: TObject);

  private
    FCommands: TFormCommands;
    FDialogType: TCopyMoveDlgType;
    FQueueIdentifier: TOperationsManagerQueueIdentifier;
    noteb: TFileViewNotebook;
    FFileSource: IFileSource;
    FOperationOptionsUIClass: TFileSourceOperationOptionsUIClass;
    FOperationOptionsUI: TFileSourceOperationOptionsUI;

    function ShowTabsSelector: integer;
    procedure TabsSelector(Sender: TObject);
    procedure TabsSelectorMouseDown(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: Integer);
    procedure ShowOptions(bShow: Boolean);

    property Commands: TFormCommands read FCommands{$IF FPC_FULLVERSION >= 020501} implements IFormCommands{$ENDIF};

  public
    constructor Create(TheOwner: TComponent; DialogType: TCopyMoveDlgType;
                       AFileSource: IFileSource;
                       AOperationOptionsUIClass: TFileSourceOperationOptionsUIClass); reintroduce;
    constructor Create(TheOwner: TComponent); override;
    procedure SetOperationOptions(Operation: TFileSourceOperation);

    property QueueIdentifier: TOperationsManagerQueueIdentifier read FQueueIdentifier;

    {$IF FPC_FULLVERSION < 020501}
    // "implements" does not work in FPC < 2.5.1
    function ExecuteCommand(Command: string; Param: String=''): TCommandFuncResult;
    function GetCommandCaption(Command: String; CaptionType: TCommandCaptionType): String;
    procedure GetCommandsList(List: TStrings);
    {$ENDIF}

  published
    procedure cm_AddToQueue(Param: String = '');
  end;


implementation

{$R *.lfm}

uses
  fMain, LCLType, LCLVersion, uGlobs, uLng, uHotkeyManager;

const
  HotkeysCategory = 'Copy/Move Dialog';

constructor TfrmCopyDlg.Create(TheOwner: TComponent; DialogType: TCopyMoveDlgType;
                               AFileSource: IFileSource;
                               AOperationOptionsUIClass: TFileSourceOperationOptionsUIClass);
begin
  FDialogType := DialogType;
  FFileSource := AFileSource;
  FOperationOptionsUIClass := AOperationOptionsUIClass;
  FQueueIdentifier := FreeOperationsQueueId;
  FCommands := TFormCommands.Create(Self);
  inherited Create(TheOwner);
end;

constructor TfrmCopyDlg.Create(TheOwner: TComponent);
begin
  Create(TheOwner, cmdtCopy, nil, nil);
end;

procedure TfrmCopyDlg.SetOperationOptions(Operation: TFileSourceOperation);
begin
  if Assigned(FOperationOptionsUI) then
    FOperationOptionsUI.SetOperationOptions(Operation);
end;

procedure TfrmCopyDlg.cm_AddToQueue(Param: String);
begin
  FQueueIdentifier := SingleQueueId;
  ModalResult := btnAddToQueue.ModalResult;
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

  {$IF lcl_fullversion < 093100}
  case Key of
    VK_ESCAPE: // Must handle before drag manager. Lazarus bug 0020676.
      begin
        ModalResult := mrCancel;
        Key := 0;
      end;
  end;
  {$ENDIF}
end;

procedure TfrmCopyDlg.btnCancelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{$IF (DEFINED(LCLGTK) or DEFINED(LCLGTK2)) and (lcl_fullversion < 093100)}
  if (Button = mbLeft) and (Sender = FindLCLControl(Mouse.CursorPos)) then
    ModalResult := btnCancel.ModalResult;
{$ENDIF}
end;

procedure TfrmCopyDlg.btnAddToQueueClick(Sender: TObject);
begin
{$IF NOT ((DEFINED(LCLGTK) or DEFINED(LCLGTK2)) and (lcl_fullversion < 093100))}
  FQueueIdentifier := SingleQueueId;
{$ENDIF}
end;

procedure TfrmCopyDlg.btnAddToQueueMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{$IF (DEFINED(LCLGTK) or DEFINED(LCLGTK2)) and (lcl_fullversion < 093100)}
  if (Button = mbLeft) and (Sender = FindLCLControl(Mouse.CursorPos)) then
  begin
    cm_AddToQueue('');
  end;
{$ENDIF}
end;

procedure TfrmCopyDlg.btnOKClick(Sender: TObject);
begin
{$IF NOT ((DEFINED(LCLGTK) or DEFINED(LCLGTK2)) and (lcl_fullversion < 093100))}
  FQueueIdentifier := FreeOperationsQueueId;
{$ENDIF}
end;

procedure TfrmCopyDlg.btnOkMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{$IF (DEFINED(LCLGTK) or DEFINED(LCLGTK2)) and (lcl_fullversion < 093100)}
  if (Button = mbLeft) and (Sender = FindLCLControl(Mouse.CursorPos)) then
  begin
     FQueueIdentifier := FreeOperationsQueueId;
     ModalResult := btnOk.ModalResult;
  end;
{$ENDIF}
end;

procedure TfrmCopyDlg.btnOptionsClick(Sender: TObject);
begin
{$IF NOT ((DEFINED(LCLGTK) or DEFINED(LCLGTK2)) and (lcl_fullversion < 093100))}
  ShowOptions(not pnlOptions.Visible);
{$ENDIF}
end;

procedure TfrmCopyDlg.btnOptionsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{$IF (DEFINED(LCLGTK) or DEFINED(LCLGTK2)) and (lcl_fullversion < 093100)}
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
var
  HMForm: THMForm;
  Hotkey: THotkey;
begin
  pnlSelector.Visible := gShowCopyTabSelectPanel;

  // Fix align of options panel and dialog size at start.
  if not pnlSelector.Visible then
    pnlOptions.Top := pnlOptions.Top -
                      (pnlSelector.Height +
                       pnlSelector.BorderSpacing.Top +
                       pnlSelector.BorderSpacing.Bottom);

  // Set initial size.
  Self.Height := pnlOptions.Top;

  // Operation options.
  if Assigned(FOperationOptionsUIClass) then
  begin
    FOperationOptionsUI := FOperationOptionsUIClass.Create(Self, FFileSource);
    FOperationOptionsUI.Parent := grpOptions;
  end
  else
    btnOptions.Visible := False;
  ShowOptions(False);

  btnOK.Caption := rsDlgOpStart;
  FQueueIdentifier := FreeOperationsQueueId;

  HMForm := HotMan.Register(Self, HotkeysCategory);
  Hotkey := HMForm.Hotkeys.FindByCommand('cm_AddToQueue');

  if Assigned(Hotkey) then
    btnAddToQueue.Caption := btnAddToQueue.Caption + ' (' + Hotkey.Shortcut + ')';
end;

procedure TfrmCopyDlg.FormDestroy(Sender: TObject);
begin
  HotMan.UnRegister(Self);
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

{$IF FPC_FULLVERSION < 020501}
function TfrmCopyDlg.ExecuteCommand(Command: string; Param: String): TCommandFuncResult;
begin
  Result := FCommands.ExecuteCommand(Command, Param);
end;

function TfrmCopyDlg.GetCommandCaption(Command: String; CaptionType: TCommandCaptionType): String;
begin
  Result := FCommands.GetCommandCaption(Command, CaptionType);
end;

procedure TfrmCopyDlg.GetCommandsList(List: TStrings);
begin
  FCommands.GetCommandsList(List);
end;
{$ENDIF}

initialization
  TFormCommands.RegisterCommandsForm(TfrmCopyDlg, HotkeysCategory, @rsHotkeyCategoryCopyMoveDialog);

end.
