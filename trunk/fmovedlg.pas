unit fMoveDlg;
{$mode objfpc}{$H+}
interface

uses
  LResources,
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, fLngForm, Buttons;

type
  TfrmMoveDlg = class(TfrmLng)
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

    procedure LoadLng; override;
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

procedure TfrmMoveDlg.LoadLng;
begin
  Caption:= lngGetString(clngDlgMv);
//  lblCopySrc.Caption:= lngGetString(clngDlgMvSrc); //handle by MainForm
  lblFileType.Caption:= lngGetString(clngDlgMvType);
end;

initialization
 {$I fMoveDlg.lrs}
end.
