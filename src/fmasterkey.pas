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
  end;

function CreateMasterKey(Short: Boolean; out Password: String; out ArgonType: Targon2_type; var M: UInt32; var T, P: UInt16): Boolean;

implementation

{$R *.lfm}

function CreateMasterKey(Short: Boolean; out Password: String; out ArgonType: Targon2_type; var M: UInt32; var T, P: UInt16): Boolean;
begin
  with TfrmMasterKey.Create(Application) do
  try
    seIterations.Value:= T;
    seParallelism.Value:= P;
    seMemory.Value:= M div 1024;
    lblText.Visible:= not Short;
    gbMasterKey.Visible:= not Short;
    pnlButtons.OKButton.Enabled:= Short;

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

procedure TfrmMasterKey.btnTestClick(Sender: TObject);
const
  HASH_LEN = 88;
  SALT = 'a5abbcdd86a5d7f9c8a76ea7d0197ed1';
var
  Res: Integer;
  StartTime: QWord;
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

  Res:= argon2_kdf(t_cost, m_cost, parallelism, 'password', 8,
                   SALT, Length(SALT), @Hash[0], HASH_LEN, ArgonType);

  if (Res <> ARGON2_OK) then
    ShowMessage('Error ' + IntToStr(Res))
  else begin
    ShowMessage(Format('The key transformation took %f seconds.', [Double(GetTickCount64 - StartTime) / 1000]));
  end;
end;

procedure TfrmMasterKey.edtPasswordChange(Sender: TObject);
begin
  pnlButtons.OKButton.Enabled:= (Length(edtPassword.Text) > 0) and (edtPassword.Text = edtRepeat.Text);
end;

end.

