unit fGioAuthDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ButtonPanel, uGio2;

type

  { TfrmGioAuthDialog }

  TfrmGioAuthDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    edtDomain: TEdit;
    edtPassword: TEdit;
    edtUserName: TEdit;
    imgAuth: TImage;
    lblDomain: TLabel;
    lblMessage: TLabel;
    lblPassword: TLabel;
    lblUserName: TLabel;
    pnlUser: TPanel;
    pnlConnect: TPanel;
    rbAnonymous: TRadioButton;
    rbConnetAs: TRadioButton;
    procedure rbAnonymousChange(Sender: TObject);
  private
    procedure ShowThread;
  public
    { public declarations }
  end;

function ShowAuthDlg(const Message: String; var Flags: TGAskPasswordFlags;
                     var DefaultUser, DefaultDomain: String; out Password: String): Boolean;


implementation

function ShowAuthDlg(const Message: String; var Flags: TGAskPasswordFlags;
  var DefaultUser, DefaultDomain: String; out Password: String): Boolean;
begin
  with TfrmGioAuthDialog.Create(Application) do
  try
    Caption:= Application.Title;
    lblMessage.Caption:= Message;
    rbAnonymous.Checked:= (Flags and G_ASK_PASSWORD_ANONYMOUS_SUPPORTED <> 0);
    pnlConnect.Visible:= rbAnonymous.Checked;
    pnlUser.Enabled:= not pnlConnect.Visible;
    edtUserName.Text:= DefaultUser;
    edtDomain.Text:= DefaultDomain;
    lblDomain.Visible:= (Flags and G_ASK_PASSWORD_NEED_DOMAIN <> 0);
    edtDomain.Visible:= lblDomain.Visible;

    TThread.Synchronize(nil, @ShowThread);

    Result:= ModalResult = mrOK;

    if Result then
    begin
      if not rbAnonymous.Checked then
      begin
        Password:= edtPassword.Text;
        DefaultUser:= edtUserName.Text;
        DefaultDomain:= edtDomain.Text;
        if pnlConnect.Visible then Flags -= G_ASK_PASSWORD_ANONYMOUS_SUPPORTED;
      end;
    end;
  finally
    Free;
  end;
end;

{$R *.lfm}

{ TfrmGioAuthDialog }

procedure TfrmGioAuthDialog.rbAnonymousChange(Sender: TObject);
begin
  pnlUser.Enabled:= not rbAnonymous.Checked;
end;

procedure TfrmGioAuthDialog.ShowThread;
begin
  ShowModal;
end;

end.

