unit fCopyDlg;

interface

uses
  LResources,
  SysUtils, Types, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type

  { TfrmCopyDlg }

  TfrmCopyDlg = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbDropReadOnlyFlag: TCheckBox;
    edtDst: TEdit;
    lblCopySrc: TLabel;
    cmbFileType: TComboBox;
    lblFileType: TLabel;
    procedure frmCopyDlgKeyPress(Sender: TObject; var Key: Char);
    procedure frmCopyDlgShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCopyDlg: TfrmCopyDlg;

implementation
uses
  uLng;

procedure TfrmCopyDlg.frmCopyDlgShow(Sender: TObject);
begin
  edtDst.SelectAll;
  edtDst.SetFocus;
end;

procedure TfrmCopyDlg.frmCopyDlgKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#27 then
    ModalResult:=mrCancel;
  if Key=#13 then
  begin
    ModalResult:=mrOK;
    Key:=#0;
  end;
end;

initialization
 {$I fCopyDlg.lrs}
end.
