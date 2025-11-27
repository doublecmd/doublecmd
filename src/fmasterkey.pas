unit fMasterKey;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, ButtonPanel, Argon2;

type

  { TfrmMasterKey }

  TfrmMasterKey = class(TForm)
    btnTest: TButton;
    pnlButtons: TButtonPanel;
    seIterations: TSpinEdit;
    seMemory: TSpinEdit;
    seParallelism: TSpinEdit;
    cmbFunction: TComboBox;
    edtPassword: TEdit;
    edtRepeat: TEdit;
    gbMasterKey: TGroupBox;
    gbKeyTransform: TGroupBox;
    lblHeader: TLabel;
    lblIterations: TLabel;
    lblMemory: TLabel;
    lblUnit: TLabel;
    lblFunction: TLabel;
    lblParallelism: TLabel;
    lblFooter: TLabel;
    lblPassword: TLabel;
    lblRepeat: TLabel;
    lblText: TLabel;
    pnlKeyTransform: TPanel;
    pnlMemory: TPanel;
    procedure btnTestClick(Sender: TObject);
    procedure edtPasswordChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FTest: Boolean;
    function Test(Finish: Boolean): Boolean;
  end;

function CreateMasterKey(Short: Boolean; out Password: String; out ArgonType: Targon2_type; out M: UInt32; out T, P: UInt16): Boolean;

implementation

{$R *.lfm}

uses
  uLng;

function CreateMasterKey(Short: Boolean; out Password: String; out ArgonType: Targon2_type; out M: UInt32; out T, P: UInt16): Boolean;
begin
  with TfrmMasterKey.Create(Application) do
  try
    seMemory.Value:= 256;
    seIterations.Value:= 2;
    seParallelism.Value:= 4;
    cmbFunction.ItemIndex:= 0;
    gbMasterKey.Visible:= not Short;
    pnlButtons.OKButton.Enabled:= Short;
    pnlButtons.OKButton.ModalResult:= mrNone;

    Result:= (ShowModal = mrOK);

    if Result then
    begin
      if cmbFunction.ItemIndex = 0 then
        ArgonType:= Argon2_d
      else begin
        ArgonType:= Argon2_id;
      end;
      T:= seIterations.Value;
      P:= seParallelism.Value;
      M:= seMemory.Value * 1024;
      Password:= edtPassword.Text;
    end;
  finally
    Free;
  end;
end;

{ TfrmMasterKey }

function TfrmMasterKey.Test(Finish: Boolean): Boolean;
const
  HASH_LEN = 88;
  SALT = 'a5abbcdd86a5d7f9c8a76ea7d0197ed1';
  Buttons: array[Boolean] of TMsgDlgButtons = ([mbOK], [mbOK, mbCancel]);
var
  Ret: Integer;
  Delay: Double;
  StartTime: QWord;
  Res: TModalResult;
  ArgonType: Targon2_type;
  t_cost, m_cost, parallelism: UInt32;
  Hash: array[0..Pred(HASH_LEN)] of Byte;
begin
  t_cost:= seIterations.Value;
  m_cost:= seMemory.Value * 1024;
  parallelism:= seParallelism.Value;

  if cmbFunction.ItemIndex = 0 then
    ArgonType:= Argon2_d
  else begin
    ArgonType:= Argon2_id;
  end;
  StartTime:= GetTickCount64;

  Ret:= argon2_kdf(t_cost, m_cost, parallelism, 'password', 8,
                   SALT, Length(SALT), @Hash[0], HASH_LEN, ArgonType);

  FTest:= (Ret = ARGON2_OK);

  if (not FTest) then
    MessageDlg(Format(rsMsgKeyTransformError, [Ret]), mtError, [mbOK], 0)
  else begin
    Delay:= Double(GetTickCount64 - StartTime) / 1000;
    Res:= MessageDlg(Format(rsMsgKeyTransformTime, [Delay]), mtInformation, Buttons[Finish], 0);
    if Finish and (Res <> mrOK) then FTest:= False;
  end;

  Result:= FTest;
end;

procedure TfrmMasterKey.btnTestClick(Sender: TObject);
begin
  Test(False);
end;

procedure TfrmMasterKey.edtPasswordChange(Sender: TObject);
begin
  pnlButtons.OKButton.Enabled:= (Length(edtPassword.Text) > 0) and (edtPassword.Text = edtRepeat.Text);
end;

procedure TfrmMasterKey.OKButtonClick(Sender: TObject);
begin
  if FTest or Test(True) then
  begin
    ModalResult:= mrOK;
  end;
end;

end.

