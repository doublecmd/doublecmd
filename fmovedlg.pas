unit fMoveDlg;
{$mode objfpc}{$H+}
interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmMoveDlg = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    edtDst: TEdit;
    lblMoveSrc: TLabel;
    lblFileType: TLabel;
    cmbFileType: TComboBox;
    procedure frmMoveDlgKeyPress(Sender: TObject; var Key: Char);
    procedure frmMoveDlgShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMoveDlg: TfrmMoveDlg;

implementation
uses
  uLng;

procedure TfrmMoveDlg.frmMoveDlgShow(Sender: TObject);
begin
  edtDst.SelectAll;
  edtDst.SetFocus;
end;

procedure TfrmMoveDlg.frmMoveDlgKeyPress(Sender: TObject; var Key: Char);
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
 {$I fmovedlg.lrs}
end.
